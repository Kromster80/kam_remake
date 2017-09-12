unit KM_UnitActionSteer;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_Defaults, KM_CommonClasses, KM_Units, KM_Points;

{Steer in place for set time}
type
  TUnitActionSteer = class(TUnitAction)
  private
    fDesireToSteer, fStuckFor: Byte; //Likelihood of changing direction
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying
    fNextPos: TKMPoint; //The tile we are currently walking to
    procedure IncVertex(aFrom, aTo: TKMPoint);
    procedure DecVertex;
    function ChooseNextStep(out Point: TKMPoint):Boolean;
  public
    constructor Create(aUnit: TKMUnit; aActionType:TUnitActionType; aLocked:boolean);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    function Execute: TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses
  KM_CommonUtils, KM_Resource, KM_ResUnits;


{ TUnitActionSteer }
constructor TUnitActionSteer.Create(aUnit: TKMUnit; aActionType:TUnitActionType; aLocked:boolean);
begin
  inherited Create(aUnit, aActionType, aLocked);
  Assert(aUnit is TKMUnitAnimal); //Only animals do steering
  fVertexOccupied := KMPOINT_ZERO;
  fNextPos        := KMPOINT_ZERO;
end;


destructor TUnitActionSteer.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
    DecVertex;
  Inherited;
end;


constructor TUnitActionSteer.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fDesireToSteer);
  LoadStream.Read(fStuckFor);
  LoadStream.Read(fVertexOccupied);
  LoadStream.Read(fNextPos);
end;


function TUnitActionSteer.ActName: TUnitActionName;
begin
  Result := uan_Steer;
end;


function TUnitActionSteer.GetExplanation: UnicodeString;
begin
  Result := 'Steering';
end;


procedure TUnitActionSteer.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell gTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'Steer vertex in use');

  fUnit.VertexAdd(aFrom,aTo);
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionSteer.DecVertex;
begin
  //Tell gTerrain that this vertex is not being used anymore
  Assert(not KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'DecVertex 0:0 Steer');

  fUnit.VertexRem(fVertexOccupied);
  fVertexOccupied := KMPOINT_ZERO;
end;


function TUnitActionSteer.ChooseNextStep(out Point: TKMPoint): Boolean;
var
  I,K,J: Integer;
  Loc:TKMPoint;
  List: TKMPointList;
  GoodSpot: Boolean;
begin
  Inc(fDesireToSteer);
  //Default is the next tile in the direction we're going
  Loc := KMGetPointInDir(fUnit.GetPosition, fUnit.Direction);
  //Decide whether we should change direction or not
  if (KaMRandom(10) < fDesireToSteer)
  or not fUnit.CanStepTo(Loc.X, Loc.Y, fUnit.DesiredPassability) then
  begin
    fDesireToSteer := 0; //Reset it
    List := TKMPointList.Create;
    Loc := fUnit.GetPosition;
    for I:=-1 to 1 do
      for K:=-1 to 1 do
        if ((I<>0)or(K<>0)) and fUnit.CanStepTo(Loc.X+I, Loc.Y+K, fUnit.DesiredPassability) then
        begin
          //Directions next to our current one are preferable (looks nicer if animals don't make jarring direction changes often)
          GoodSpot := KMGetDirection(I, K) in [KMNextDirection(fUnit.Direction), KMPrevDirection(fUnit.Direction)];
          for J:=0 to 5*Byte(GoodSpot) do
            List.Add(KMPoint(Loc.X+I, Loc.Y+K));
        end;
    Result := List.GetRandom(Point);
    List.Free;
  end
  else
  begin
    Point := Loc;
    Result := True;
  end;
end;


function TUnitActionSteer.Execute: TActionResult;
var
  DX,DY:shortint;
  WalkX,WalkY,Distance:single;
  FirstStep: Boolean;
begin
  if KMSamePoint(fNextPos, KMPOINT_ZERO) then
  begin
    fNextPos := fUnit.GetPosition; //Set fNextPos to current pos so it initializes on the first run
    FirstStep := True;
  end
  else
    FirstStep := False;
      
  Distance := gRes.Units[fUnit.UnitType].Speed;
  if KMSamePointF(fUnit.PositionF, KMPointF(fNextPos), Distance/2) then
  begin
    //Set precise position to avoid rounding errors
    fUnit.PositionF := KMPointF(fNextPos);

    //No longer using previous vertex
    if KMStepIsDiag(fUnit.PrevPosition, fUnit.NextPosition) and not FirstStep and (fStuckFor = 0) then
      DecVertex;

    //Decide on next step
    if not ChooseNextStep(fNextPos) then
    begin
      inc(fStuckFor);
      if fStuckFor > 200 then
        Result := ar_ActAborted //We have been stuck for a while so abort and TKMUnitAnimal.UpdateState will kill us
      else
        Result := ar_ActContinues;
      Exit;
    end;
    fStuckFor := 0;
    
    //Do some house keeping because we have now stepped on a new tile
    fUnit.NextPosition := fNextPos;
    fUnit.Walk(fUnit.PrevPosition, fUnit.NextPosition); //Pre-occupy next tile
    if KMStepIsDiag(fUnit.PrevPosition,fUnit.NextPosition) then
      IncVertex(fUnit.PrevPosition,fUnit.NextPosition);
    //Update unit direction so we are facing the way we are going
    fUnit.Direction := KMGetDirection(fUnit.PrevPosition, fUnit.NextPosition);
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
  Result := ar_ActContinues;
end;


procedure TUnitActionSteer.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fDesireToSteer);
  SaveStream.Write(fStuckFor);
  SaveStream.Write(fVertexOccupied);
  SaveStream.Write(fNextPos);
end;


end.
