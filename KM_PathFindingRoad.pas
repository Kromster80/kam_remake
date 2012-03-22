unit KM_PathFindingRoad;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_PathFinding, KM_Terrain, KM_Points;


type
  //Pathfinding with regard to players plans
  TPathFindingRoad = class(TPathFinding)
  private
    fOwner: TPlayerIndex;
  protected
    function CanWalkTo(aFrom, aTo: TKMPoint): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
  public
    constructor Create(aOwner: TPlayerIndex);
  end;


implementation
uses KM_PlayersCollection;


{ TPathFindingRoad }
constructor TPathFindingRoad.Create(aOwner: TPlayerIndex);
begin
  fOwner := aOwner;
end;


function TPathFindingRoad.CanWalkTo(aFrom, aTo: TKMPoint): Boolean;
begin
  Result := not KMStepIsDiag(aFrom, aTo);
end;


function TPathFindingRoad.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := inherited and not fPlayers[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


end.
