unit KM_UnitActionAbandonWalk;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Points;


{Abandon the current walk, move onto next tile}
type
  TUnitActionAbandonWalk = class(TUnitAction)
  private
    fWalkTo: TKMPoint;
    fVertexOccupied: TKMPoint;
  public
    constructor Create(aUnit: TKMUnit; LocB, aVertexOccupied: TKMPoint; aActionType: TUnitActionType);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    function Execute: TActionResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Resource, KM_ResUnits;


{ TUnitActionAbandonWalk }
constructor TUnitActionAbandonWalk.Create(aUnit: TKMUnit; LocB, aVertexOccupied: TKMPoint; aActionType: TUnitActionType);
begin
  Assert(LocB.X*LocB.Y <> 0, 'Illegal WalkTo 0;0');
  inherited Create(aUnit, aActionType, False);

  fWalkTo         := LocB;
  fVertexOccupied := aVertexOccupied;
end;


destructor TUnitActionAbandonWalk.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
  begin
    fUnit.VertexRem(fVertexOccupied); //Unoccupy vertex
    fVertexOccupied := KMPOINT_ZERO;
  end;
  inherited;
end;


constructor TUnitActionAbandonWalk.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fWalkTo);
  LoadStream.Read(fVertexOccupied);
end;


function TUnitActionAbandonWalk.ActName: TUnitActionName;
begin
  Result := uan_AbandonWalk;
end;


function TUnitActionAbandonWalk.GetExplanation: UnicodeString;
begin
  Result := 'Abandoning walk';
end;


function TUnitActionAbandonWalk.Execute: TActionResult;
var
  DX, DY: ShortInt;
  WalkX, WalkY, Distance: Single;
begin
  Result := ar_ActContinues;

  //Execute the route in series of moves
  Distance := gRes.Units[fUnit.UnitType].Speed;

  //Check if unit has arrived on tile
  if KMSamePointF(fUnit.PositionF, KMPointF(fWalkTo), Distance/2) then
  begin
    fUnit.PositionF := KMPointF(fWalkTo); //Set precise position to avoid rounding errors
    fUnit.IsExchanging := False; //Disable sliding (in case it was set in previous step)
    if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
    begin
      fUnit.VertexRem(fVertexOccupied); //Unoccupy vertex
      fVertexOccupied := KMPOINT_ZERO;
    end;
    StepDone := True;
    Result := ar_ActDone;
    exit;
  end;

  WalkX := fWalkTo.X - fUnit.PositionF.X;
  WalkY := fWalkTo.Y - fUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fUnit.PositionF := KMPointF(fUnit.PositionF.X + DX*Math.min(Distance,abs(WalkX)),
                              fUnit.PositionF.Y + DY*Math.min(Distance,abs(WalkY)));
  Inc(fUnit.AnimStep);
end;


procedure TUnitActionAbandonWalk.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fWalkTo);
  SaveStream.Write(fVertexOccupied);
end;


end.
