unit KM_PathFindingRoad;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_PathFinding, KM_PathFindingAStarNew, KM_Points;


type
  //Pathfinding that finds a route for a road to be built
  //todo: Maybe it is worth trying to make Roadfinder a house-aware algo,
  //to prefer connecting to supply/demand houses
  TPathFindingRoad = class(TPathFindingAStarNew)
  private
    fOwner: TKMHandIndex;
    fRoadConnectID: Byte;
  protected
    function CanWalkTo(const aFrom: TKMPoint; aToX, aToY: SmallInt): Boolean; override;
    function DestinationReached(aX, aY: Word): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
    function EstimateToFinish(aX, aY: Word): Word; override;
  public
    constructor Create(aOwner: TKMHandIndex);

    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    function Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean; reintroduce;
    function Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aRoadConnectID: Byte; NodeList: TKMPointList): Boolean; reintroduce;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
  end;

  //Minor variation on class above for creating shortcuts in AI road network
  TPathFindingRoadShortcuts = class(TPathFindingRoad)
  private
  protected
    function DestinationReached(aX, aY: Word): Boolean; override;
    function IsWalkableTile(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
  public
  end;


implementation
uses
  KM_HandsCollection, KM_Terrain, KM_Units, KM_Hand;


{ TPathFindingRoad }
constructor TPathFindingRoad.Create(aOwner: TKMHandIndex);
begin
  inherited Create;
  fOwner := aOwner;
end;


procedure TPathFindingRoad.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TPathFindingRoad.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fOwner);
  SaveStream.Write(fRoadConnectID);
end;


procedure TPathFindingRoad.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fOwner);
  LoadStream.Read(fRoadConnectID);
end;


function TPathFindingRoad.CanWalkTo(const aFrom: TKMPoint; aToX, aToY: SmallInt): Boolean;
begin
  //Roads can't go diagonally, only in 90 turns
  Result := (aToX = aFrom.X) or (aToY = aFrom.Y);
end;


function TPathFindingRoad.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
var IsRoad: Boolean;
begin
  IsRoad := (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)
            or (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ft_Road)
            or (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);

  //Since we don't allow roads to be built diagonally we can assume
  //path is always 1 tile (10 points)
  if IsRoad then
    Result := 0
  else
    Result := 10;

  //Building roads over fields is discouraged unless unavoidable
  if gTerrain.TileIsCornField(KMPoint(aToX, aToY))
  or gTerrain.TileIsWineField(KMPoint(aToX, aToY)) then
    Inc(Result, 60); //60 points equals to 6 tiles penalty
end;


function TPathFindingRoad.EstimateToFinish(aX, aY: Word): Word;
begin
  case fDestination of
    pdLocation:    //Rough estimation
                    Result := (Abs(aX - fLocB.X) + Abs(aY - fLocB.Y)) * 10;

    pdPassability: //Every direction is equaly good
                    Result := 0;
    else            Result := 0;
  end;
end;


function TPathFindingRoad.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := ([tpMakeRoads, tpWalkRoad] * gTerrain.Land[aY,aX].Passability <> [])
            and (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) in [ft_None, ft_Road])
            and not gHands[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


function TPathFindingRoad.DestinationReached(aX, aY: Word): Boolean;
begin
  Result := ((aX = fLocB.X) and (aY = fLocB.Y)) //We reached destination point
            or ((gTerrain.Land[aY, aX].TileOverlay = to_Road) //We reached destination road network
               and (fRoadConnectID <> 0) //No network
               and (gTerrain.GetRoadConnectID(KMPoint(aX, aY)) = fRoadConnectID));
end;


function TPathFindingRoad.Route_Make(aLocA, aLocB: TKMPoint; NodeList: TKMPointList): Boolean;
begin
  Result := inherited Route_Make(aLocA, aLocB, [tpMakeRoads, tpWalkRoad], 0, nil, NodeList);
end;


//Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
function TPathFindingRoad.Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aRoadConnectID: Byte; NodeList: TKMPointList): Boolean;
begin
  fRoadConnectID := aRoadConnectID;
  Result := inherited Route_ReturnToWalkable(aLocA, aLocB, wcRoad, 0, [tpMakeRoads, tpWalkRoad], NodeList);
end;


{ TPathFindingRoadShortcuts }
function TPathFindingRoadShortcuts.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
var IsRoad: Boolean;
begin
  //Since we don't allow roads to be built diagonally we can assume
  //path is always 1 tile (10 points)
  Result := 10;

  //Off road costs extra
  IsRoad := (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)
            or (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ft_Road)
            or (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);
  if not IsRoad then
    Inc(Result, 30);

  //Building roads over fields is discouraged unless unavoidable
  if gTerrain.TileIsCornField(KMPoint(aToX, aToY))
  or gTerrain.TileIsWineField(KMPoint(aToX, aToY)) then
    Inc(Result, 40);
end;


function TPathFindingRoadShortcuts.IsWalkableTile(aX, aY: Word): Boolean;
begin
  Result := ([tpMakeRoads, tpWalkRoad] * gTerrain.Land[aY,aX].Passability <> [])
            or (gTerrain.Land[aY, aX].TileLock = tlRoadWork);
  Result := Result and (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aX, aY)) in [ft_None, ft_Road])
                   and not gHands[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aX, aY));
end;


function TPathFindingRoadShortcuts.DestinationReached(aX, aY: Word): Boolean;
begin
  Result := ((aX = fLocB.X) and (aY = fLocB.Y)); //We reached destination point
end;

end.
