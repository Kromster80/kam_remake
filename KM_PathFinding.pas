unit KM_PathFinding;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_Terrain, KM_Points;


const
  PATH_CACHE_MAX = 12; //How many paths to cache
  PATH_CACHE_INIT_WEIGHT = 5; //New path weight

type
  TKMPathDestination = (
    pdLocation, //Walk to location
    pdPassability, //Walk to desired passability
    pdHouse //Approach house from any side (workers and warriors)
    );

  //This is a helper class for TTerrain
  //Here should be pathfinding and all associated stuff
  //I think we should refactor this unit and move some TTerrain methods here
  TPathFinding = class
  private
    fCache: array [0 .. PATH_CACHE_MAX - 1] of record
      Weight: Word;
      Pass: TPassabilitySet;
      Route: TKMPointList;
    end;
  protected
    fPass: TPassabilitySet;
    fTargetWalkConnect: TWalkConnect;
    fTargetNetwork: Byte;
    fDistance: Single;
    fIsInteractionAvoid: Boolean;
    fTargetHouse: TKMHouse;
    procedure AddToCache(NodeList: TKMPointList);
    function TryRouteFromCache(NodeList: TKMPointList): Boolean;
  protected
    fLocA: TKMPoint;
    fLocB: TKMPoint;
    fDestination: TKMPathDestination;
    function CanWalkTo(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean; virtual;
    function DestinationReached(aX, aY: Word): Boolean; virtual;
    function IsWalkableTile(aX, aY: Word): Boolean; virtual;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; virtual;
    function EstimateToFinish(aX, aY: Word): Word; virtual;
    function MakeRoute: Boolean; virtual; abstract;
    procedure ReturnRoute(NodeList: TKMPointList); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function Route_Make(aLocA, aLocB: TKMPoint; aPass: TPassabilitySet; aDistance: Single; aTargetHouse: TKMHouse; NodeList: TKMPointList): Boolean;
    function Route_MakeAvoid(aLocA, aLocB: TKMPoint; aPass: TPassabilitySet; aDistance: Single; aTargetHouse: TKMHouse; NodeList: TKMPointList): Boolean;
    function Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aTargetWalkConnect: TWalkConnect; aTargetNetwork: Byte; aPass: TPassabilitySet; NodeList: TKMPointList): Boolean;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
    procedure UpdateState;
  end;


implementation


{ TPathFinding }
constructor TPathFinding.Create;
var
  I: Integer;
begin
  inherited;

  if CACHE_PATHFINDING then
  for I := 0 to PATH_CACHE_MAX - 1 do
    fCache[I].Route := TKMPointList.Create;
end;


destructor TPathFinding.Destroy;
var
  I: Integer;
begin
  if CACHE_PATHFINDING then
  for I := 0 to PATH_CACHE_MAX - 1 do
    FreeAndNil(fCache[I].Route);

  inherited;
end;


//Find a route from A to B which meets aPass Passability
//Results should be written as NodeCount of waypoint nodes to Nodes
function TPathFinding.Route_Make(aLocA, aLocB: TKMPoint; aPass:TPassabilitySet; aDistance:single; aTargetHouse:TKMHouse; NodeList:TKMPointList): Boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass;
  fTargetNetwork := 0;
  fTargetWalkConnect := wcWalk;
  fDistance := aDistance;
  fIsInteractionAvoid := False;
  fTargetHouse := aTargetHouse;
  if fTargetHouse = nil then
    fDestination := pdLocation
  else
    fDestination := pdHouse;

  //Try to find similar route in cache and reuse it
  if CACHE_PATHFINDING and TryRouteFromCache(NodeList) then
    Result := True
  else
  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end else
    NodeList.Clear;
end;


//We are using Interaction Avoid mode (go around busy units)
function TPathFinding.Route_MakeAvoid(aLocA, aLocB: TKMPoint; aPass:TPassabilitySet; aDistance:single; aTargetHouse:TKMHouse; NodeList:TKMPointList):boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass;
  fTargetNetwork := 0;
  fTargetWalkConnect := wcWalk;
  fDistance := aDistance;
  fIsInteractionAvoid := True;
  fTargetHouse := aTargetHouse;
  if fTargetHouse = nil then
    fDestination := pdLocation
  else
    fDestination := pdHouse;

  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end;
end;


//Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
function TPathFinding.Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aTargetWalkConnect:TWalkConnect; aTargetNetwork:byte; aPass:TPassabilitySet; NodeList:TKMPointList): Boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass; //Should be unused here
  fTargetNetwork := aTargetNetwork;
  fTargetWalkConnect := aTargetWalkConnect;
  fDistance := 0;
  fIsInteractionAvoid := False;
  fTargetHouse := nil;
  fDestination := pdPassability;

  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end else
    NodeList.Clear;
end;


function TPathFinding.CanWalkTo(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean;
begin
  Result := fTerrain.CanWalkDiagonaly(aFrom, bX, bY);
end;


function TPathFinding.IsWalkableTile(aX, aY: Word): Boolean;
begin
  //If cell meets Passability then estimate it
  Result := (fPass * fTerrain.Land[aY,aX].Passability) <> [];
end;


//How much it costs to move From -> To
function TPathFinding.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  if Abs(aFromX-aToX) > Abs(aFromY-aToY) then
    Result := Abs(aFromX-aToX) * 10 + Abs(aFromY-aToY) * 4
  else
    Result := Abs(aFromY-aToY) * 10 + Abs(aFromX-aToX) * 4;

  //Do not add extra cost if the tile is the target, as it can cause a longer route to be chosen
  if (aToX <> fLocB.X) or (aToY <> fLocB.Y) then
  begin
    if DO_WEIGHT_ROUTES and (fTerrain.Land[aToY,aToX].IsUnit <> nil) then
      Inc(Result, 10); //Unit = 1 extra tile
    if fIsInteractionAvoid and fTerrain.TileIsLocked(KMPoint(aToX,aToY)) then
      Inc(Result, 500); //In interaction avoid mode, working unit = 50 tiles
  end;
end;


function TPathFinding.EstimateToFinish(aX, aY: Word): Word;
begin
  //Use Estim even if destination is Passability, as it will make it faster.
  //Target should be in the right direction even though it's not our destination.
  Result := (Abs(aX - fLocB.X) + Abs(aY - fLocB.Y)) * 10;
end;


function TPathFinding.DestinationReached(aX, aY: Word): Boolean;
begin
  case fDestination of
    pdLocation:    Result := KMLengthDiag(KMPoint(aX, aY), fLocB) <= fDistance;
    pdPassability: Result := fTerrain.GetConnectID(fTargetWalkConnect, KMPoint(aX, aY)) = fTargetNetwork;
    pdHouse:       Result := fTargetHouse.InReach(KMPoint(aX, aY), fDistance);
    else            Result := true;
  end;
end;


//Cache the route incase it is needed soon
procedure TPathFinding.AddToCache(NodeList: TKMPointList);
var
  I: Integer;
  Best: Integer;
begin
  //Find cached route with least weight and replace it
  Best := 0;
  for I := 1 to PATH_CACHE_MAX - 1 do
  if fCache[I].Weight < fCache[Best].Weight then
    Best := I;

  fCache[Best].Weight := PATH_CACHE_INIT_WEIGHT;
  fCache[Best].Pass := fPass;
  fCache[Best].Route.Copy(NodeList);
end;


function TPathFinding.TryRouteFromCache(NodeList: TKMPointList): Boolean;
  //Check if we can straightly walk to Route from our loc
  function NearStart(const aRoute: TKMPointList): Boolean;
  begin
    Result := (KMLengthDiag(aRoute[0], fLocB) < 2)
           or (KMLengthDiag(aRoute[1], fLocB) < 2)
           or (KMLengthDiag(aRoute[2], fLocB) < 2);
  end;
  //Check if we can straightly walk to target from any of last Routes points
  function NearEnd(const aRoute: TKMPointList): Boolean;
  begin
    Result := (KMLengthDiag(aRoute[aRoute.Count-1], fLocB) < 2)
           or (KMLengthDiag(aRoute[aRoute.Count-2], fLocB) < 2)
           or (KMLengthDiag(aRoute[aRoute.Count-3], fLocB) < 2);
  end;
var
  I,K: Integer;
  BestStart, BestEnd: Word;
  NewL, BestL: Single;
begin
  Result := False;

  for I := 0 to PATH_CACHE_MAX - 1 do
  if (fCache[I].Route.Count > 0)
  and (fCache[I].Pass = fPass) then
  begin

    //Check if route starts within reach
    BestL := MaxSingle;
    for K := 0 to 5 do
    begin
      NewL := KMLengthDiag(fLocA, fCache[I].Route[K]);
      if NewL < 2 then
      begin
        BestStart := K;
        BestL := NewL;
      end;
    end;

    if BestL >= 2 then Continue;

    //Check if route ends within reach
    BestL := MaxSingle;
    for K := fCache[I].Route.Count - 1 downto fCache[I].Route.Count - 5 do
    begin
      NewL := KMLengthDiag(fLocB, fCache[I].Route[K]);
      if NewL < 2 then
      begin
        BestEnd := K;
        BestL := NewL;
      end;
    end;

    if BestL >= 2 then Continue;

    //Assemble the route
    NodeList.Clear;
    NodeList.AddEntry(fLocA);
    for K := BestStart to BestEnd do
      NodeList.AddEntry(fCache[I].Route[K]);
    NodeList.AddEntry(fLocB);

    //Mark the cached route as more useful
    Inc(fCache[I].Weight);

    Result := True;
    Exit;
  end;
end;


procedure TPathFinding.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write('PathFinding');

  if CACHE_PATHFINDING then
  for I := 0 to PATH_CACHE_MAX - 1 do
  begin
    SaveStream.Write(fCache[I].Weight);
    SaveStream.Write(fCache[I].Pass, SizeOf(fCache[I].Pass));
    fCache[I].Route.SaveToStream(SaveStream);
  end;
end;


procedure TPathFinding.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.ReadAssert('PathFinding');

  if CACHE_PATHFINDING then
  for I := 0 to PATH_CACHE_MAX - 1 do
  begin
    LoadStream.Read(fCache[I].Weight);
    LoadStream.Read(fCache[I].Pass, SizeOf(fCache[I].Pass));
    fCache[I].Route.LoadFromStream(LoadStream);
  end;
end;


procedure TPathFinding.UpdateState;
var
  I: Integer;
begin
  if CACHE_PATHFINDING then
  for I := 0 to PATH_CACHE_MAX - 1 do
    fCache[I].Weight := Max(fCache[I].Weight - 1, 0);
end;


end.
