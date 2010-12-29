unit KM_UnitActionStormAttack;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Utils, KM_CommonTypes, KM_Defaults, KM_Units, Math;


{Charge forwards until we are tired or hit an obstacle}
type
TUnitActionStormAttack = class(TUnitAction)
  private
    fDelay: integer; //Delay before action starts
    fTileSteps: integer; //The number of tiles we have walked onto so far
    fStamina: integer; //How much stamina to run do we have
    fNextPos: TKMPoint; //The tile we are currently walking to
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying
  public
    constructor Create(aActionType:TUnitActionType; aRow:integer);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    procedure IncVertex(aFrom, aTo: TKMPoint);
    procedure DecVertex;
    function CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

implementation
uses KM_Terrain, KM_Units_Warrior;


{ TUnitActionStormAttack }
constructor TUnitActionStormAttack.Create(aActionType:TUnitActionType; aRow:integer);
const MIN_STAMINA=8; MAX_STAMINA=6; //8..13
begin
  Inherited Create(aActionType);
  fActionName     := uan_StormAttack;
  Locked          := true;
  fTileSteps      := -1; //-1 so the first initializing step makes it 0
  fDelay          := (aRow-1)*5; //No delay for the first row
  fStamina        := MIN_STAMINA + random(MAX_STAMINA);
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


procedure TUnitActionStormAttack.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell fTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPoint(0,0)), 'Storm vertex in use');
  Assert(not fTerrain.HasVertexUnit(KMGetDiagVertex(aFrom,aTo)), 'Storm vertex blocked');

  fTerrain.UnitVertexAdd(KMGetDiagVertex(aFrom,aTo));
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionStormAttack.DecVertex;
begin
  //Tell fTerrain that this vertex is not being used anymore
  Assert(not KMSamePoint(fVertexOccupied, KMPoint(0,0)), 'DecVertex 0:0 Storm');

  fTerrain.UnitVertexRem(fVertexOccupied);
  fVertexOccupied := KMPoint(0,0);
end;


function TUnitActionStormAttack.CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
begin
  Result := (not fTerrain.CheckPassability(NextPos,KMUnit.GetDesiredPassability)) or
            (not fTerrain.CanWalkDiagonaly(KMUnit.GetPosition,NextPos)) or
            (fTerrain.HasVertexUnit(KMGetDiagVertex(KMUnit.GetPosition,NextPos))) or
            (fTerrain.Land[NextPos.Y,NextPos.X].IsUnit <> nil);
end;


function TUnitActionStormAttack.Execute(KMUnit: TKMUnit):TActionResult;
const STORM_SPEEDUP=1.5;
var
  DX,DY:shortint;
  WalkX,WalkY,Distance:single;
begin
  if KMSamePoint(fNextPos,KMPoint(0,0)) then
    fNextPos := KMUnit.GetPosition; //Set fNextPos to current pos so it initializes on the first run

  //Walk for the first step before running
  if fDelay>0 then begin
    dec(fDelay);
    KMUnit.AnimStep := UnitStillFrames[KMUnit.Direction];
    Result := ActContinues;
    exit;
  end;

  //First and last steps are walking, inbetween are running
  if (fTileSteps<=0) or (fTileSteps>=fStamina-1) then begin
    Distance := ACTION_TIME_DELTA * KMUnit.GetSpeed;
    fActionType := ua_Walk;
  end else begin
    Distance := ACTION_TIME_DELTA * KMUnit.GetSpeed * STORM_SPEEDUP;
    fActionType := ua_Spec;
  end;

  if KMSamePointF(KMUnit.PositionF, KMPointF(fNextPos), Distance/2) then
  begin
    inc(fTileSteps); //We have stepped on a new tile
    //Set precise position to avoid rounding errors
    KMUnit.PositionF := KMPointF(fNextPos);

    //No longer using previous vertex
    if KMStepIsDiag(KMUnit.PrevPosition,KMUnit.NextPosition) and (fTileSteps > 0) then
      DecVertex;

    if fTileSteps >= fStamina then
    begin
      Result := ActDone; //Finished run
      //Make it so that when we halt we stay at this new location if we have not been given different order
      if TKMUnitWarrior(KMUnit).GetOrder = wo_None then
        TKMUnitWarrior(KMUnit).OrderLocDir := KMPointDir(KMUnit.GetPosition,TKMUnitWarrior(KMUnit).OrderLocDir.Dir);
      exit;
    end;

    //Begin the next step
    fNextPos := KMGetPointInDir(KMUnit.GetPosition, KMUnit.Direction).Loc;
    if CheckForObstacle(KMUnit, fNextPos) then
    begin
      Result := ActDone; //Hit an obstacle
      //Make it so that when we halt we stay at this new location if we have not been given different order
      if TKMUnitWarrior(KMUnit).GetOrder = wo_None then
        TKMUnitWarrior(KMUnit).OrderLocDir := KMPointDir(KMUnit.GetPosition,TKMUnitWarrior(KMUnit).OrderLocDir.Dir);
      exit;
    end;
    //Do some house keeping because we have stepped on a new tile
    KMUnit.UpdateNextPosition(fNextPos);
    fTerrain.UnitWalk(KMUnit.PrevPosition,KMUnit.NextPosition,KMUnit); //Pre-occupy next tile
    if KMStepIsDiag(KMUnit.PrevPosition,KMUnit.NextPosition) then
      IncVertex(KMUnit.PrevPosition,KMUnit.NextPosition);
  end;

  WalkX := fNextPos.X - KMUnit.PositionF.X;
  WalkY := fNextPos.Y - KMUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  KMUnit.PositionF := KMPointF(KMUnit.PositionF.X + DX*Math.min(Distance,abs(WalkX)),
                               KMUnit.PositionF.Y + DY*Math.min(Distance,abs(WalkY)));

  inc(KMUnit.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
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
