unit KM_UnitActionStormAttack;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_CommonUtils,
  KM_Units;


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
  public
    constructor Create(aUnit: TKMUnit; aActionType: TUnitActionType; aRow: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    function GetSpeed: Single;
    function Execute: TActionResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

implementation
uses
  KM_Resource, KM_ResUnits, KM_Units_Warrior;


const
  STORM_SPEEDUP = 1.5;


{ TUnitActionStormAttack }
constructor TUnitActionStormAttack.Create(aUnit: TKMUnit; aActionType: TUnitActionType; aRow: Integer);
const
  //Tiles traveled measured in KaM TPR: Min 8, maximum 13
  //We reduced the variation in order to make storm attack more useful
  MIN_STAMINA = 12;
  MAX_STAMINA = 13;
begin
  inherited Create(aUnit, aActionType, True);
  fTileSteps      := -1; //-1 so the first initializing step makes it 0
  fDelay          := aRow * 5; //No delay for the first row
  fStamina        := MIN_STAMINA + KaMRandom(MAX_STAMINA-MIN_STAMINA+1);
  fNextPos        := KMPOINT_ZERO;
  fVertexOccupied := KMPOINT_ZERO;
end;


destructor TUnitActionStormAttack.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
    DecVertex;
  inherited;
end;


constructor TUnitActionStormAttack.Load(LoadStream: TKMemoryStream);
begin
  inherited;
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


function TUnitActionStormAttack.GetExplanation: UnicodeString;
begin
  Result := 'Storming';
end;


procedure TUnitActionStormAttack.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell gTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'Storm vertex in use');
  //Assert(not gTerrain.HasVertexUnit(KMGetDiagVertex(aFrom,aTo)), 'Storm vertex blocked');

  fUnit.VertexAdd(aFrom,aTo); //Running counts as walking
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionStormAttack.DecVertex;
begin
  //Tell gTerrain that this vertex is not being used anymore
  Assert(not KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'DecVertex 0:0 Storm');

  fUnit.VertexRem(fVertexOccupied);
  fVertexOccupied := KMPOINT_ZERO;
end;


function TUnitActionStormAttack.GetSpeed: Single;
begin
  if (fTileSteps <= 0) or (fTileSteps >= fStamina-1) then
    Result := gRes.Units[fUnit.UnitType].Speed
  else
    Result := gRes.Units[fUnit.UnitType].Speed * STORM_SPEEDUP;
end;


function TUnitActionStormAttack.Execute: TActionResult;
var
  DX, DY: ShortInt;
  WalkX, WalkY, Distance: Single;
begin
  if KMSamePoint(fNextPos, KMPOINT_ZERO) then
    fNextPos := fUnit.GetPosition; //Set fNextPos to current pos so it initializes on the first run

  //Walk for the first step before running
  if fDelay > 0 then
  begin
    Dec(fDelay);
    fUnit.AnimStep := UnitStillFrames[fUnit.Direction];
    Result := ar_ActContinues;
    Exit;
  end;

  //Last step is walking, others are running (unit gets tired and slows at the end)
  //In KaM the first step was also walking, but this makes it less useful/surprising
  if (fTileSteps >= fStamina - 1) then
  begin
    Distance := gRes.Units[fUnit.UnitType].Speed;
    fActionType := ua_Walk;
  end else begin
    Distance := gRes.Units[fUnit.UnitType].Speed * STORM_SPEEDUP;
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

    //Check for units nearby to fight
    Locked := False; //Unlock during this check only so CheckForEnemy can abandon our action
    if (fUnit is TKMUnitWarrior) then
      if TKMUnitWarrior(fUnit).CheckForEnemy then
      begin
        //If we've picked a fight it means this action no longer exists,
        //so we must exit out (don't set ActDone as that will now apply to fight action)
        Result := ar_ActContinues;
        Exit;
      end;
    Locked := True; //Finished CheckForEnemy, so lock again

    //Begin the next step
    fNextPos := KMGetPointInDir(fUnit.GetPosition, fUnit.Direction);

    //Action ends if: 1: Used up stamina. 2: There is an enemy to fight. 3: NextPos is an obsticle
    if (fTileSteps >= fStamina) or not fUnit.CanStepTo(fNextPos.X, fNextPos.Y, fUnit.DesiredPassability) then
    begin
      Result := ar_ActDone; //Finished run
      Exit; //Must exit right away as we might have changed this action to fight
    end;

    //Do some house keeping because we have now stepped on a new tile
    fUnit.NextPosition := fNextPos;
    fUnit.Walk(fUnit.PrevPosition, fUnit.NextPosition); //Pre-occupy next tile
    if KMStepIsDiag(fUnit.PrevPosition,fUnit.NextPosition) then
      IncVertex(fUnit.PrevPosition,fUnit.NextPosition);
  end;

  WalkX := fNextPos.X - fUnit.PositionF.X;
  WalkY := fNextPos.Y - fUnit.PositionF.Y;
  DX := Sign(WalkX); //-1,0,1
  DY := Sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fUnit.PositionF := KMPointF(fUnit.PositionF.X + DX*Math.min(Distance, Abs(WalkX)),
                              fUnit.PositionF.Y + DY*Math.min(Distance, Abs(WalkY)));

  inc(fUnit.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
  Result := ar_ActContinues;
end;


procedure TUnitActionStormAttack.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fDelay);
  SaveStream.Write(fTileSteps);
  SaveStream.Write(fStamina);
  SaveStream.Write(fNextPos);
  SaveStream.Write(fVertexOccupied);
end;


end.
