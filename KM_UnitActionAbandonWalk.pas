unit KM_UnitActionAbandonWalk;
{$I KaM_Remake.inc}
interface
uses Math, KromUtils,
  KM_CommonTypes, KM_Defaults, KM_Utils, KM_Units;


{Abandon the current walk, move onto next tile}
type
  TUnitActionAbandonWalk = class(TUnitAction)
  private
    fWalkTo, fVertexOccupied:TKMPoint;
  public
    constructor Create(LocB,aVertexOccupied :TKMPoint; const aActionType:TUnitActionType=ua_Walk);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad(); override;
    destructor Destroy; override;
    procedure Execute(KMUnit: TKMUnit; out DoEnd: Boolean); override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_Terrain;


{ TUnitActionAbandonWalk }
constructor TUnitActionAbandonWalk.Create(LocB,aVertexOccupied:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  fLog.AssertToLog(LocB.X*LocB.Y<>0, 'Illegal WalkTo 0;0');
  Inherited Create(aActionType);
  fActionName     := uan_AbandonWalk;
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


procedure TUnitActionAbandonWalk.SyncLoad();
begin
  Inherited;
  //nothing, FPC doesn't likes it missing for some reason?
end;


procedure TUnitActionAbandonWalk.Execute(KMUnit: TKMUnit; out DoEnd: Boolean);
var
  DX,DY:shortint; WalkX,WalkY,Distance:single;
begin
  DoEnd := false;

  //Execute the route in series of moves
  Distance := ACTION_TIME_DELTA * KMUnit.GetSpeed;

  //Check if unit has arrived on tile
  if Equals(KMUnit.PositionF.X,fWalkTo.X,Distance/2) and Equals(KMUnit.PositionF.Y,fWalkTo.Y,Distance/2) then
  begin
    KMUnit.IsExchanging := false; //Disable sliding (in case it was set in previous step)
    KMUnit.PositionF := KMPointF(fWalkTo.X,fWalkTo.Y); //Set precise position to avoid rounding errors
    DoEnd := true; //We are finished
    if not KMSamePoint(fVertexOccupied,KMPoint(0,0)) then
    begin
      fTerrain.UnitVertexRem(fVertexOccupied); //Unoccupy vertex
      fVertexOccupied := KMPoint(0,0);
    end;
    GetIsStepDone := true;
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
  inherited;
  SaveStream.Write(fWalkTo);
  SaveStream.Write(fVertexOccupied);
end;


end.
