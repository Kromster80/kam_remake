unit KM_UnitActionStormAttack;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Utils, KM_CommonTypes, KM_Defaults, KM_Units, Math;


{Charge forwards until we are tired or hit an obstacle}
type
TUnitActionStormAttack = class(TUnitAction)
  private
    fDelay: integer; //Delay before action starts
    fStep: integer; //
    fStamina: integer; //How much stamina to run do we have
    fNextPos: TKMPoint; //Next tile
  public
    constructor Create(aActionType:TUnitActionType; aRow:integer);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

implementation
uses KM_Terrain;


{ TUnitActionStormAttack }
constructor TUnitActionStormAttack.Create(aActionType:TUnitActionType; aRow:integer);
const MIN_STAMINA=8; MAX_STAMINA=6; //8..13
begin
  Inherited Create(aActionType);
  fActionName     := uan_StormAttack;
  Locked          := true;
  fStep           := 0;
  fDelay          := aRow*5;
  fStamina        := (MIN_STAMINA + random(MAX_STAMINA))*10;
  fNextPos        := KMPoint(0,0);
end;


destructor TUnitActionStormAttack.Destroy;
begin
  Inherited;
end;


constructor TUnitActionStormAttack.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  //LoadStream.Read(fStamina);
end;


function TUnitActionStormAttack.CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
begin
  Result := (not fTerrain.CheckPassability(NextPos,KMUnit.GetDesiredPassability)) or
            (not fTerrain.CanWalkDiagonaly(KMUnit.GetPosition,NextPos)) or
            (fTerrain.UnitsHitTest(NextPos.X,NextPos.Y) <> nil);
end;


function TUnitActionStormAttack.Execute(KMUnit: TKMUnit):TActionResult;
const STORM_SPEEDUP=3;
var
  DX,DY:shortint;
  WalkX,WalkY,Distance:single;
begin

  //Pause before running
  if fDelay>0 then begin
    dec(fDelay);
    Result := ActContinues;
    exit;
  end;

  inc(fStep);

  if (fStep<10) or (fStep>fStamina-10) then begin
    Distance := ACTION_TIME_DELTA * KMUnit.GetSpeed;
    fActionType := ua_Walk;
  end else begin
    Distance := ACTION_TIME_DELTA * KMUnit.GetSpeed * STORM_SPEEDUP;
    fActionType := ua_Spec;
  end;

  if KMSamePointF(KMUnit.PositionF, KMPointF(fNextPos), Distance/2) then
  begin
    //Set precise position to avoid rounding errors
    KMUnit.PositionF := KMPointF(fNextPos);

    fNextPos := KMGetPointInDir(KMUnit.GetPosition, KMUnit.Direction).Loc;
    if CheckForObstacle(KMUnit, fNextPos) then
    begin
      Result := ActDone; //Hit an obstacle
      exit;
    end;
  end;

  WalkX := fNextPos.X - KMUnit.PositionF.X;
  WalkY := fNextPos.Y - KMUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  KMUnit.PositionF := KMPointF(KMUnit.PositionF.X + DX*min(Distance,abs(WalkX)),
                               KMUnit.PositionF.Y + DY*min(Distance,abs(WalkY)));

  inc(KMUnit.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
end;


procedure TUnitActionStormAttack.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  //SaveStream.Write(fStamina);
end;


end.
