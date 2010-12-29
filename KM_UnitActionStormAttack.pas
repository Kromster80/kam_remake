unit KM_UnitActionStormAttack;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Utils, KM_CommonTypes, KM_Defaults, KM_Units;


{Charge forwards until we are tired or hit an obstacle}
type
TUnitActionStormAttack = class(TUnitAction)
  private
    fRow: integer; //The row we are in tells us the delay before we start running (1..n)

  public
    constructor Create(aActionType:TUnitActionType);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

implementation
uses KM_Terrain;

{ TUnitActionFight }
constructor TUnitActionStormAttack.Create(aActionType:TUnitActionType);
begin
  Inherited Create(aActionType);
  fActionName     := uan_StormAttack;
  Locked          := true;
end;


destructor TUnitActionStormAttack.Destroy;
begin
  Inherited;
end;


constructor TUnitActionStormAttack.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
end;


function TUnitActionStormAttack.CheckForObstacle(KMUnit: TKMUnit; NextPos: TKMPoint):boolean;
begin
  Result := (not fTerrain.CheckPassability(NextPos,KMUnit.GetDesiredPassability)) or
            (not fTerrain.CanWalkDiagonaly(KMUnit.GetPosition,NextPos)) or
            (fTerrain.UnitsHitTest(NextPos.X,NextPos.Y) <> nil);
end;


function TUnitActionStormAttack.Execute(KMUnit: TKMUnit):TActionResult;
//var NextPos: TKMPoint;
begin
  {if UnitSteppedOnNewTile then
  begin
    NextPos := KMGetPointInDir(KMUnit.GetPosition, KMUnit.Direction);
    if CheckForObstacle(KMUnit, NextPos) then
    begin
      Result := ActDone; //Hit an obstacle
      exit;
    end;
  end
  else
  begin
    //Show charge animation and move unit
  end;}
end;


procedure TUnitActionStormAttack.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
end;


end.
