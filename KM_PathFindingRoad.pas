unit KM_PathFindingRoad;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_PathFinding, KM_Points;


type
  //Pathfinding with regard to players plans
  TPathFindingRoad = class(TPathFinding)
  private
    fOwner: TPlayerIndex;
  protected
    function CanWalkTo(const aFrom, aTo: TKMPoint): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
  public
    constructor Create(aOwner: TPlayerIndex);

    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    function Route_Make(aLocA, aLocB: TKMPoint; aPass: TPassabilitySet; NodeList: TKMPointList): Boolean; reintroduce;//load;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_PlayersCollection, KM_Terrain;


{ TPathFindingRoad }
constructor TPathFindingRoad.Create(aOwner: TPlayerIndex);
begin
  inherited Create;
  fOwner := aOwner;
end;


procedure TPathFindingRoad.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
end;


procedure TPathFindingRoad.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
end;


procedure TPathFindingRoad.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
end;


function TPathFindingRoad.CanWalkTo(const aFrom, aTo: TKMPoint): Boolean;
begin
  Result := not KMStepIsDiag(aFrom, aTo);
end;


function TPathFindingRoad.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  //Since we don't allow roads to be built diagonally we can assume
  //path is always 1 tile (10 points)
  Result := 10;

  //Building roads over fields is discouraged unless unavoidable
  if fTerrain.TileIsCornField(KMPoint(aToX, aToY))
  or fTerrain.TileIsWineField(KMPoint(aToX, aToY)) then
    Inc(Result, 60); //60 points equals to 6 tiles penalty
end;


function TPathFindingRoad.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := inherited IsWalkableTile(aX,aY)
            and (fPlayers[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) in [ft_None, ft_Road])
            and not fPlayers[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


function TPathFindingRoad.Route_Make(aLocA, aLocB: TKMPoint; aPass: TPassabilitySet; NodeList: TKMPointList): Boolean;
begin
  Result := inherited Route_Make(aLocA, aLocB, aPass, 0, nil, NodeList, False);
end;


end.
