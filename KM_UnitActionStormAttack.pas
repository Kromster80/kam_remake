unit KM_UnitActionStormAttack;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Utils, KM_CommonClasses, KM_Defaults, KM_Units, Math, KM_Points;


{Charge forwards until we are tired or hit an obstacle}
type
  TUnitActionStormAttack = class(TUnitAction)
  private
    fDelay: integer; //Delay before action starts
    fTileSteps: integer; //The number of tiles we have walked onto so far
    fStamina: integer; //How much stamina to run do we have
    fNextPos: TKMPoint; //The tile we are currently walking to
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying
    procedure IncVertex(aFrom, aTo: TKMPoint);
    procedure DecVertex;
    function CheckForObstacle(NextPos: TKMPoint):boolean;
  public
    constructor Create(aUnit: TKMUnit; aActionType: TUnitActionType; aRow: Integer);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TUnitActionName; override;
    function GetExplanation:string; override;
    function GetSpeed: Single;
    function Execute: TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

implementation
uses KM_Terrain, KM_Units_Warrior, KM_ResourceGFX;


{ TUnitActionStormAttack }
constructor TUnitActionStormAttack.Create(aUnit: TKMUnit; aActionType: TUnitActionType; aRow: Integer);
const MIN_STAMINA=8; MAX_STAMINA=6; //8..13
begin
  Inherited Create(aUnit, aActionType, True);
  fTileSteps      := -1; //-1 so the first initializing step makes it 0
  fDelay          := (aRow-1)*5; //No delay for the first row
  fStamina        := MIN_STAMINA + KaMRandom(MAX_STAMINA);
  fNextPos        := KMPoint(0,0);
  fVertexOccupied := KMPoint(0,0);
end;


destructor TUnitActionStormAttack.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
    DecVertex;
  Inherited;
end;


constructor TUnitActionStormAttack.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fDelay);
  LoadStream.Read(fTileSteps);
  LoadStream.Read(fStamina);
  LoadStream.Read(fNextPos);
  LoadStream.Read(fVertexOccupied);
end;


function TUnitActionStormAttack.ActName: TUnitActionName;
begin
  Result := uan_StormAttack;
end;


function TUnitActionStormAttack.GetExplanation: string;
begin
  Result := 'Storming';
end;


procedure TUnitActionStormAttack.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell fTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPoint(0,0)), 'Storm vertex in use');
  Assert(not fTerrain.HasVertexUnit(KMGetDiagVertex(aFrom,aTo)), 'Storm vertex blocked');

  fTerrain.UnitVertexAdd(aFrom,aTo); //Running counts as walking
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionStormAttack.DecVertex;
begin
  //Tell fTerrain that this vertex is not being used anymore
  Assert(not KMSamePoint(fVertexOccupied, KMPoint(0,0)), 'DecVertex 0:0 Storm');

  fTerrain.UnitVertexRem(fVertexOccupied);
  fVertexOccupied := KMPoint(0,0);
end;


function TUnitActionStormAttack.GetSpeed: Single;
begin
  if (fTileSteps <= 0) or (fTileSteps >= fStamina-1) then
    Result := fResource.UnitDat[fUnit.UnitType].Speed
  else
    Result := fResource.UnitDat[fUnit.UnitType].Speed * STORM_SPEEDUP;
end;


function TUnitActionStormAttack.CheckForObstacle(NextPos: TKMPoint):boolean;
begin
  Result := (not fTerrain.CheckPassability(NextPos, fUnit.GetDesiredPassability)) or
            (not fTerrain.CanWalkDiagonaly(fUnit.GetPosition, NextPos)) or
            (fTerrain.HasVertexUnit(KMGetDiagVertex(fUnit.GetPosition, NextPos))) or
            (fTerrain.Land[NextPos.Y,NextPos.X].IsUnit <> nil);
end;


function TUnitActionStormAttack.Execute: TActionResult;
var
  DX,DY:shortint;
  WalkX,WalkY,Distance:single;
  FoundEnemy: TKMUnit;
begin
  if KMSamePoint(fNextPos, KMPoint(0,0)) then
    fNextPos := fUnit.GetPosition; //Set fNextPos to current pos so it initializes on the first run

  //Walk for the first step before running
  if fDelay>0 then begin
    dec(fDelay);
    fUnit.AnimStep := UnitStillFrames[fUnit.Direction];
    Result := ActContinues;
    exit;
  end;

  //First and last steps are walking, inbetween are running
  if (fTileSteps<=0) or (fTileSteps>=fStamina-1) then begin
    Distance := fResource.UnitDat[fUnit.UnitType].Speed;
    fActionType := ua_Walk;
  end else begin
    Distance := fResource.UnitDat[fUnit.UnitType].Speed * STORM_SPEEDUP;
    fActionType := ua_Spec;
  end;

  if KMSamePointF(fUnit.PositionF, KMPointF(fNextPos), Distance/2) then
  begin
    inc(fTileSteps); //We have stepped on a new tile
    //Set precise position to avoid rounding errors
    fUnit.PositionF := KMPointF(fNextPos);

    //No longer using previous vertex
    if KMStepIsDiag(fUnit.PrevPosition, fUnit.NextPosition) and (fTileSteps > 0) then
      DecVertex;

    //Begin the next step
    fNextPos := KMGetPointInDir(fUnit.GetPosition, fUnit.Direction).Loc;

    Locked := false; //So find enemy works
    FoundEnemy := TKMUnitWarrior(fUnit).FindEnemy;
    //Action ends if: 1: Used up stamina. 2: There is an enemy to fight. 3: NextPos is an obsticle
    if (fTileSteps >= fStamina) or (FoundEnemy <> nil) or CheckForObstacle(fNextPos) then
    begin
      Result := ActDone; //Finished run
      //Make it so that when we halt we stay at this new location if we have not been given different order
      if TKMUnitWarrior(fUnit).GetOrder = wo_None then
        TKMUnitWarrior(fUnit).OrderLocDir := KMPointDir(fUnit.GetPosition, TKMUnitWarrior(fUnit).OrderLocDir.Dir);
      //Begin the fight right now
      if FoundEnemy <> nil then
      begin
        TKMUnitWarrior(fUnit).FightEnemy(FoundEnemy);
        Result := ActContinues; //Set result to ActContinues so the new fight action isn't destroyed
      end;
      exit; //Must exit right away as we might have changed this action to fight
    end;
    Locked := true; //Finished using FindEnemy
    //Do some house keeping because we have now stepped on a new tile
    fUnit.UpdateNextPosition(fNextPos);
    fTerrain.UnitWalk(fUnit.PrevPosition,fUnit.NextPosition,fUnit); //Pre-occupy next tile
    if KMStepIsDiag(fUnit.PrevPosition,fUnit.NextPosition) then
      IncVertex(fUnit.PrevPosition,fUnit.NextPosition);
  end;

  WalkX := fNextPos.X - fUnit.PositionF.X;
  WalkY := fNextPos.Y - fUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fUnit.PositionF := KMPointF(fUnit.PositionF.X + DX*Math.min(Distance,abs(WalkX)),
                              fUnit.PositionF.Y + DY*Math.min(Distance,abs(WalkY)));

  inc(fUnit.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
  Result := ActContinues;
end;


procedure TUnitActionStormAttack.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fDelay);
  SaveStream.Write(fTileSteps);
  SaveStream.Write(fStamina);
  SaveStream.Write(fNextPos);
  SaveStream.Write(fVertexOccupied);
end;


end.
