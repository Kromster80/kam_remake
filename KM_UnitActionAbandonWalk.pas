unit KM_UnitActionAbandonWalk;
{$I KaM_Remake.inc}
interface
uses Math, 
  KM_CommonTypes, KM_Defaults, KM_Units, KM_Points;


{Abandon the current walk, move onto next tile}
type
  TUnitActionAbandonWalk = class(TUnitAction)
  private
    fWalkTo:TKMPoint;
    fVertexOccupied:TKMPoint;
  public
    constructor Create(LocB,aVertexOccupied :TKMPoint; const aActionType:TUnitActionType=ua_Walk);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    class function ActName: TUnitActionName; override;
    function GetExplanation:string; override;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_Terrain, KM_ResourceGFX;


{ TUnitActionAbandonWalk }
constructor TUnitActionAbandonWalk.Create(LocB,aVertexOccupied:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  Assert(LocB.X*LocB.Y<>0, 'Illegal WalkTo 0;0');
  Inherited Create(aActionType);

  Locked          := false;
  fWalkTo         := LocB;
  fVertexOccupied := aVertexOccupied;
end;


destructor TUnitActionAbandonWalk.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
  begin
    fTerrain.UnitVertexRem(fVertexOccupied); //Unoccupy vertex
    fVertexOccupied := KMPoint(0,0);
  end;
  Inherited;
end;


constructor TUnitActionAbandonWalk.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fWalkTo);
  LoadStream.Read(fVertexOccupied);
end;


class function TUnitActionAbandonWalk.ActName: TUnitActionName;
begin
  Result := uan_AbandonWalk;
end;


function TUnitActionAbandonWalk.GetExplanation: string;
begin
  Result := 'Abandoning walk';
end;


function TUnitActionAbandonWalk.Execute(KMUnit: TKMUnit):TActionResult;
var
  DX,DY:shortint; WalkX,WalkY,Distance:single;
begin
  Result := ActContinues;

  //Execute the route in series of moves
  Distance := fResource.UnitDat[KMUnit.UnitType].Speed;

  //Check if unit has arrived on tile
  if KMSamePointF(KMUnit.PositionF, KMPointF(fWalkTo), Distance/2) then
  begin
    KMUnit.PositionF := KMPointF(fWalkTo); //Set precise position to avoid rounding errors
    KMUnit.IsExchanging := false; //Disable sliding (in case it was set in previous step)
    if not KMSamePoint(fVertexOccupied,KMPoint(0,0)) then
    begin
      fTerrain.UnitVertexRem(fVertexOccupied); //Unoccupy vertex
      fVertexOccupied := KMPoint(0,0);
    end;
    StepDone := true;
    Result := ActDone;
    exit;
  end;

  WalkX := fWalkTo.X - KMUnit.PositionF.X;
  WalkY := fWalkTo.Y - KMUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  KMUnit.PositionF := KMPointF(KMUnit.PositionF.X + DX*Math.min(Distance,abs(WalkX)),
                               KMUnit.PositionF.Y + DY*Math.min(Distance,abs(WalkY)));
  inc(KMUnit.AnimStep);
end;


procedure TUnitActionAbandonWalk.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fWalkTo);
  SaveStream.Write(fVertexOccupied);
end;


end.
