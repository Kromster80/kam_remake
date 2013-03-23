unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils, TypInfo,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_TerrainFinder, KM_PerfLog;


type
  TFindNearest = (fnHouse, fnStone, fnTrees, fnSoil, fnWater, fnCoal, fnIron, fnGold);

  TKMTerrainFinderCity = class(TKMTerrainFinderCommon)
  protected
    fOwner: TPlayerIndex;
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  public
    FindType: TFindNearest;
    HouseType: THouseType;
    constructor Create(aOwner: TPlayerIndex);
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
  end;

  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;
    fListGold: TKMPointList; //List of possible goldmine locations
    fFinder: TKMTerrainFinderCity;

    function GetSourceLocation(aHouseType: array of THouseType; out Loc: TKMPoint): Boolean;

    function NextToOre(aHouse: THouseType; aOreType: TResourceType; out aLoc: TKMPoint): Boolean;
    function NextToHouse(aHouse: THouseType; aTarget, aAvoid: array of THouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(out aLoc: TKMPoint): Boolean;
    function NextToTrees(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToGrass(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    procedure AfterMissionInit;

    function FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean; overload;
    procedure FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    procedure FindNearest(const aStart: TKMPoint; aRadius: Byte; aHouse: THouseType; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


const
  AI_FIELD_HEIGHT = 3;
  AI_FIELD_WIDTH = 3;
  AI_FIELD_MAX_AREA = (AI_FIELD_WIDTH * 2 + 1) * AI_FIELD_HEIGHT;


implementation
uses KM_Houses, KM_Terrain, KM_Player, KM_PlayersCollection, KM_Utils, KM_AIFields;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
  fFinder := TKMTerrainFinderCity.Create(fOwner);

  fListGold := TKMPointList.Create;
end;


destructor TKMCityPlanner.Destroy;
begin
  fListGold.Free;
  fFinder.Free;

  inherited;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  Result := False;

  case aHouse of
    ht_Store:           Result := NextToHouse(aHouse, [ht_Any], [ht_Store], aLoc);
    ht_ArmorSmithy:     Result := NextToHouse(aHouse, [ht_IronSmithy, ht_CoalMine, ht_Barracks], [], aLoc);
    ht_ArmorWorkshop:   Result := NextToHouse(aHouse, [ht_Tannery, ht_Barracks], [], aLoc);
    ht_Bakery:          Result := NextToHouse(aHouse, [ht_Mill], [], aLoc);
    ht_Barracks:        Result := NextToHouse(aHouse, [ht_Any], [], aLoc);
    ht_Butchers:        Result := NextToHouse(aHouse, [ht_Swine], [], aLoc);
    ht_Inn:             Result := NextToHouse(aHouse, [ht_Any], [ht_Inn], aLoc);
    ht_IronSmithy:      Result := NextToHouse(aHouse, [ht_IronMine, ht_CoalMine], [], aLoc);
    ht_Metallurgists:   Result := NextToHouse(aHouse, [ht_GoldMine], [], aLoc);
    ht_Mill:            Result := NextToHouse(aHouse, [ht_Farm], [], aLoc);
    ht_Sawmill:         Result := NextToHouse(aHouse, [ht_Woodcutters], [], aLoc);
    ht_School:          Result := NextToHouse(aHouse, [ht_Store, ht_Barracks], [], aLoc);
    ht_Stables:         Result := NextToHouse(aHouse, [ht_Farm], [], aLoc);
    ht_Swine:           Result := NextToHouse(aHouse, [ht_Farm], [], aLoc);
    ht_Tannery:         Result := NextToHouse(aHouse, [ht_Swine], [], aLoc);
    ht_WeaponSmithy:    Result := NextToHouse(aHouse, [ht_IronSmithy, ht_CoalMine, ht_Barracks], [], aLoc);
    ht_WeaponWorkshop:  Result := NextToHouse(aHouse, [ht_Sawmill, ht_Barracks], [], aLoc);

    ht_CoalMine:      Result := NextToOre(aHouse, rt_Coal, aLoc);
    ht_GoldMine:      Result := NextToOre(aHouse, rt_GoldOre, aLoc);
    ht_IronMine:      Result := NextToOre(aHouse, rt_IronOre, aLoc);

    ht_Quary:         Result := NextToStone(aLoc);
    ht_Woodcutters:   Result := NextToTrees([ht_Store, ht_Woodcutters, ht_Sawmill], aHouse, aLoc);
    ht_Farm:          Result := NextToGrass(ht_Any, aHouse, aLoc);
    ht_Wineyard:      Result := NextToGrass(ht_Any, aHouse, aLoc);
    ht_FisherHut:     {Result := NextToWater(aHouse, aLoc)};

    //ht_Marketplace:;
    //ht_SiegeWorkshop:;
    //ht_TownHall:;
    //ht_WatchTower:;
  end;
end;


function TKMCityPlanner.GetSourceLocation(aHouseType: array of THouseType; out Loc: TKMPoint): Boolean;
var
  HT: THouseType;
  House: TKMHouse;
begin
  Result := False;
  HT := aHouseType[KaMRandom(Length(aHouseType))];

  House := fPlayers[fOwner].Houses.FindHouse(HT, 0, 0, KaMRandom(fPlayers[fOwner].Stats.GetHouseQty(HT)) + 1);
  if House <> nil then
  begin
    Loc := House.GetPosition;
    Result := True;
  end;
end;


procedure TKMCityPlanner.AfterMissionInit;
var
  I,K: Integer;
begin
  //Mark all spots where we could possibly place a goldmine
  //some smarter logic can clip left/right edges later on?
  for I := 1 to fTerrain.MapY - 2 do
  for K := 1 to fTerrain.MapX - 2 do
  if fTerrain.TileGoodForGoldmine(K,I) then
    fListGold.AddEntry(KMPoint(K,I));
end;


function TKMCityPlanner.NextToGrass(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  function CanPlaceHouse(aHouse: THouseType; aX, aY: Word): Boolean;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := False;
    if fPlayers[fOwner].CanAddHousePlanAI(aX, aY, aHouse, True) then
    begin
      FieldCount := 0;
      for I := Min(aY + 2, fTerrain.MapY - 1) to Max(aY + 2 + AI_FIELD_HEIGHT - 1, 1) do
      for K := Max(aX - AI_FIELD_WIDTH, 1) to Min(aX + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
      if fPlayers[fOwner].CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
      begin
        Inc(FieldCount);
        //Request slightly more than we need to have a good choice
        if FieldCount >= Min(AI_FIELD_MAX_AREA, IfThen(aHouse = ht_Farm, 16, 10)) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
var
  I, K: Integer;
  Bid, BestBid: Single;
  TargetLoc: TKMPoint;
begin
  Result := False;
  Assert(aHouse in [ht_Farm, ht_Wineyard]);

  if not GetSourceLocation([aTarget], TargetLoc) then Exit;

  BestBid := MaxSingle;
  for I := Max(TargetLoc.Y - 7, 1) to Min(TargetLoc.Y + 6, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - 7, 1) to Min(TargetLoc.X + 7, fTerrain.MapX - 1) do
  if CanPlaceHouse(aHouse, K, I) then
  begin
    Bid := KMLength(KMPoint(K,I), TargetLoc)
           - fAIFields.Influences.Ownership[fOwner, I, K] / 5
           + KaMRandom * 4;
    if Bid < BestBid then
    begin
      aLoc := KMPoint(K,I);
      BestBid := Bid;
      Result := True;
    end;
  end;
end;


function TKMCityPlanner.NextToHouse(aHouse: THouseType; aTarget, aAvoid: array of THouseType; out aLoc: TKMPoint): Boolean;
var
  I: Integer;
  Bid, BestBid: Single;
  TargetLoc: TKMPoint;
  Locs: TKMPointTagList;
begin
  Result := False;

  if not GetSourceLocation(aTarget, TargetLoc) then Exit;

  Locs := TKMPointTagList.Create;
  try
    FindNearest(KMPointBelow(TargetLoc), 32, aHouse, 12, Locs);

    BestBid := MaxSingle;
    for I := 0 to Locs.Count - 1 do
    begin
      Bid := Locs.Tag[I]
             - fAIFields.Influences.Ownership[fOwner,Locs[I].Y,Locs[I].X] / 5;
      if (Bid < BestBid) then
      begin
        aLoc := Locs[I];
        BestBid := Bid;
        Result := True;
      end;
    end;
  finally
    Locs.Free;
  end;
end;


//Called when AI needs to find a good spot for a new Quary
function TKMCityPlanner.NextToStone(out aLoc: TKMPoint): Boolean;
var
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPoint;
  Locs: TKMPointTagList;
  TargetLoc: TKMPoint;
begin
  Result := False;

  if not GetSourceLocation([ht_Store], TargetLoc) then Exit;

  Locs := TKMPointTagList.Create;
  try
    //1. Find all tiles from which stone can be mined
    FindNearest(KMPointBelow(TargetLoc), 32, fnStone, 15, Locs);

    //2. Pick random loc
    if not Locs.GetRandom(StoneLoc) then Exit;

    //3. Find place for Quarry near that loc
    BestBid := MaxSingle;
    for I := StoneLoc.Y to Min(StoneLoc.Y + 6, fTerrain.MapY - 1) do
    for K := Max(StoneLoc.X - 6, 1) to Min(StoneLoc.X + 6, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlanAI(K, I, ht_Quary, True) then
    begin
      Bid := KMLength(KMPoint(K,I), TargetLoc)
             - fAIFields.Influences.Ownership[fOwner,I,K] / 10
             + KaMRandom * 5;
      if (Bid < BestBid) then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
  finally
    Locs.Free;
  end;
end;


function TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean;
begin
  fFinder.FindType := aType;
  fFinder.HouseType := ht_None;
  Result := fFinder.FindNearest(aStart, aRadius, [CanWalkRoad, CanMakeRoads], aResultLoc);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := aType;
  fFinder.HouseType := ht_None;
  fFinder.FindNearest(aStart, aRadius, [CanWalkRoad, CanMakeRoads], aMaxCount, aLocs);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aHouse: THouseType; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := fnHouse;
  fFinder.HouseType := aHouse;
  fFinder.FindNearest(aStart, aRadius, [CanWalkRoad, CanMakeRoads], aMaxCount, aLocs);
end;


function TKMCityPlanner.NextToOre(aHouse: THouseType; aOreType: TResourceType; out aLoc: TKMPoint): Boolean;
var
  P: TKMPoint;
  TargetLoc: TKMPoint;
begin
  Result := False;

  if not GetSourceLocation([ht_Store], TargetLoc) then Exit;

  //Look for nearest Ore
  case aOreType of
    rt_Coal:    if not FindNearest(KMPointBelow(TargetLoc), 45, fnCoal, P) then Exit;
    rt_IronOre: if not FindNearest(KMPointBelow(TargetLoc), 45, fnIron, P) then Exit;
    rt_GoldOre: if not FindNearest(KMPointBelow(TargetLoc), 45, fnGold, P) then Exit;
  end;

  //todo: If there's no ore AI should not keep calling this over and over again

  aLoc := P;
  Result := True;
end;


function TKMCityPlanner.NextToTrees(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RES = 7;
  SEARCH_RAD = 20; //Search for forests within this radius
  SEARCH_DIV = (SEARCH_RAD * 2) div SEARCH_RES + 1;
  HUT_RAD = 4; //Search for the best place for a hut in this radius
var
  I, K: Integer;
  Bid, BestBid: Single;
  TargetLoc: TKMPoint;
  TreeLoc: TKMPoint;
  Mx, My: SmallInt;
  MyForest: array [0..SEARCH_RES-1, 0..SEARCH_RES-1] of ShortInt;
begin
  Result := False;

  if not GetSourceLocation(aTarget, TargetLoc) then Exit;

    //todo: Rework through FindNearest to avoid roundabouts
  //Fill in MyForest map
  FillChar(MyForest[0,0], SizeOf(MyForest), #0);
  for I := Max(TargetLoc.Y - SEARCH_RAD, 1) to Min(TargetLoc.Y + SEARCH_RAD, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - SEARCH_RAD, 1) to Min(TargetLoc.X + SEARCH_RAD, fTerrain.MapX - 1) do
  if fTerrain.ObjectIsChopableTree(K, I) then
  begin
    Mx := (K - TargetLoc.X + SEARCH_RAD) div SEARCH_DIV;
    My := (I - TargetLoc.Y + SEARCH_RAD) div SEARCH_DIV;

    Inc(MyForest[My, Mx]);
  end;

  //Find cell with most trees
  BestBid := -MaxSingle;
  TreeLoc := TargetLoc; //Init incase we cant find a spot at all
  for I := Low(MyForest) to High(MyForest) do
  for K := Low(MyForest[I]) to High(MyForest[I]) do
  begin
    Mx := Round(TargetLoc.X - SEARCH_RAD + (K + 0.5) * SEARCH_DIV);
    My := Round(TargetLoc.Y - SEARCH_RAD + (I + 0.5) * SEARCH_DIV);
    if InRange(Mx, 1, fTerrain.MapX - 1) and InRange(My, 1, fTerrain.MapY - 1)
    and (fAIFields.Influences.AvoidBuilding[My, Mx] = 0) then
    begin
      Bid := MyForest[I, K] + KaMRandom * 2; //Add some noise for varied results
      if Bid > BestBid then
      begin
        TreeLoc := KMPoint(Mx, My);
        BestBid := Bid;
      end;
    end;
  end;

  BestBid := MaxSingle;
  for I := Max(TreeLoc.Y - HUT_RAD, 1) to Min(TreeLoc.Y + HUT_RAD, fTerrain.MapY - 1) do
  for K := Max(TreeLoc.X - HUT_RAD, 1) to Min(TreeLoc.X + HUT_RAD, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
    begin
      Bid := KMLength(KMPoint(K,I), TargetLoc) + KaMRandom * 5;
      if (Bid < BestBid) then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


procedure TKMCityPlanner.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fFinder.OwnerUpdate(fOwner);
end;


procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  fFinder.Save(SaveStream);
  fListGold.SaveToStream(SaveStream);
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  fFinder.Load(LoadStream);
  fListGold.LoadFromStream(LoadStream);
end;


{ TKMTerrainFinderCity }
constructor TKMTerrainFinderCity.Create(aOwner: TPlayerIndex);
begin
  inherited Create;

  fOwner := aOwner;
end;


procedure TKMTerrainFinderCity.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
end;


function TKMTerrainFinderCity.CanUse(const X, Y: Word): Boolean;
begin
  case FindType of
    fnHouse:  Result := fPlayers[fOwner].CanAddHousePlanAI(X, Y, HouseType, True);

    fnStone:  Result := (fTerrain.TileIsStone(X, Max(Y-2, 1)) > 1);

    fnCoal:   Result := (fTerrain.TileIsCoal(X, Y) > 1)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_CoalMine, False);

    fnIron:   Result := (fTerrain.TileIsIron(X, Max(Y-1, 1)) > 0)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_IronMine, False);

    fnGold:   Result := (fTerrain.TileIsGold(X, Max(Y-1, 1)) > 0)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_GoldMine, False);

    else      Result := False;
  end;
end;


function TKMTerrainFinderCity.CanWalkHere(const X,Y: Word): Boolean;
var
  TerOwner: TPlayerIndex;
begin
  //Check for specific passabilities
  case FindType of
    fnIron:   Result := (fPassability * fTerrain.Land[Y,X].Passability <> [])
                        or fTerrain.TileGoodForIron(X, Y);

    fnGold:   Result := (fPassability * fTerrain.Land[Y,X].Passability <> [])
                        or fTerrain.TileGoodForGoldmine(X, Y);

    else      Result := (fPassability * fTerrain.Land[Y,X].Passability <> []);
  end;

  if not Result then Exit;

  //Don't build on allies and/or enemies territory
  TerOwner := fAIFields.Influences.GetBestOwner(X,Y);
  Result := ((TerOwner = fOwner) or (TerOwner = PLAYER_NONE));
end;


procedure TKMTerrainFinderCity.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fOwner);
end;


procedure TKMTerrainFinderCity.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fOwner);
end;


end.
