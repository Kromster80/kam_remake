unit KM_AICityPlanner;
{$I KaM_Remake.inc}
interface
uses
  KM_TerrainFinder,
  KM_ResHouses, KM_ResWares,
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  TFindNearest = (fnHouse, fnStone, fnTrees, fnSoil, fnWater, fnCoal, fnIron, fnGold);

  //Terrain finder optimized for CityPlanner demands of finding resources and houses
  TKMTerrainFinderCity = class(TKMTerrainFinderCommon)
  protected
    fOwner: TKMHandIndex;
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  public
    FindType: TFindNearest;
    HouseType: THouseType;
    constructor Create(aOwner: TKMHandIndex);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
  end;

  TKMCityPlanner = class
  private
    fOwner: TKMHandIndex;
    fListGold: TKMPointList; //List of possible goldmine locations
    fFinder: TKMTerrainFinderCity;

    function GetSeeds(aHouseType: array of THouseType): TKMPointArray;

    function NextToOre(aHouse: THouseType; aOreType: TWareType; out aLoc: TKMPoint; aNearAnyHouse: Boolean = False): Boolean;
    function NextToHouse(aHouse: THouseType; aSeed, aAvoid: array of THouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToTrees(aHouse: THouseType; aSeed: array of THouseType; out aLoc: TKMPoint): Boolean;
    function NextToGrass(aHouse: THouseType; aSeed: array of THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy; override;

    procedure AfterMissionInit;

    function FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean; overload;
    procedure FindNearest(const aStart: TKMPointArray; aRadius: Byte; aType: TFindNearest; aPass: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    procedure FindNearest(const aStart: TKMPointArray; aRadius: Byte; aHouse: THouseType; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


const
  AI_FIELD_HEIGHT = 3;
  AI_FIELD_WIDTH = 4;
  AI_FIELD_MAX_AREA = (AI_FIELD_WIDTH * 2 + 1) * AI_FIELD_HEIGHT;


implementation
uses
  Math,
  KM_Hand, KM_AIFields, KM_AIInfluences,
  KM_Terrain, KM_HandsCollection,
  KM_Resource, KM_ResUnits,
  KM_Houses, KM_CommonUtils;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TKMHandIndex);
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
    ht_WatchTower:      Result := NextToHouse(aHouse, [ht_Barracks], [], aLoc);
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

    ht_CoalMine:      Result := NextToOre(aHouse, wt_Coal, aLoc);
    ht_GoldMine:      Result := NextToOre(aHouse, wt_GoldOre, aLoc);
    ht_IronMine:      Result := NextToOre(aHouse, wt_IronOre, aLoc);

    ht_Quary:         Result := NextToStone(aHouse, aLoc);
    ht_Woodcutters:   Result := NextToTrees(aHouse, [ht_Store, ht_Woodcutters, ht_Sawmill], aLoc);
    ht_Farm:          Result := NextToGrass(aHouse, [ht_Any], aLoc);
    ht_Wineyard:      Result := NextToGrass(aHouse, [ht_Any], aLoc);
    ht_FisherHut:     {Result := NextToWater(aHouse, aLoc)};

    //ht_Marketplace:;
    //ht_SiegeWorkshop:;
    //ht_TownHall:;
    //ht_WatchTower:;
  end;

  //If we failed to find something, try to place the house anywhere (better than ignoring it)
  if not Result and not (aHouse in [ht_CoalMine, ht_GoldMine, ht_IronMine, ht_Quary, ht_Farm, ht_Wineyard, ht_FisherHut]) then
    Result := NextToHouse(aHouse, [ht_Any], [], aLoc);
end;


//Receive list of desired house types
//Output list of locations below these houses
function TKMCityPlanner.GetSeeds(aHouseType: array of THouseType): TKMPointArray;
var
  I, K: Integer;
  H: THouseType;
  Count, HQty: Integer;
  House: TKMHouse;
begin
  Count := 0;
  SetLength(Result, Count);

  for I := Low(aHouseType) to High(aHouseType) do
  begin
    H := aHouseType[I];
    HQty := gHands[fOwner].Stats.GetHouseQty(H);
    //ht_Any picks three random houses for greater variety
    for K := 0 to 1 + Byte(H = ht_Any) * 2 do
    begin
      House := gHands[fOwner].Houses.FindHouse(H, 0, 0, KaMRandom(HQty) + 1);
      if House <> nil then
      begin
        SetLength(Result, Count + 1);
        //Position is as good as Entrance for city planning
        Result[Count] := KMPointBelow(House.GetPosition);
        Inc(Count);
      end;
    end;
  end;
end;


procedure TKMCityPlanner.AfterMissionInit;
var
  I,K: Integer;
begin
  //Mark all spots where we could possibly place a goldmine
  //some smarter logic can clip left/right edges later on?
  for I := 1 to gTerrain.MapY - 2 do
  for K := 1 to gTerrain.MapX - 2 do
  if gTerrain.TileGoodForGoldmine(K,I) then
    fListGold.Add(KMPoint(K,I));
end;


function TKMCityPlanner.NextToGrass(aHouse: THouseType; aSeed: array of THouseType; out aLoc: TKMPoint): Boolean;
  function CanPlaceHouse(aHouse: THouseType; aX, aY: Word): Boolean;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := False;
    if gHands[fOwner].CanAddHousePlanAI(aX, aY, aHouse, True) then
    begin
      FieldCount := 0;
      for I := Min(aY - 2, gTerrain.MapY - 1) to Max(aY + 2 + AI_FIELD_HEIGHT - 1, 1) do
      for K := Max(aX - AI_FIELD_WIDTH, 1) to Min(aX + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddFieldPlan(KMPoint(K,I), ft_Corn)
      //Skip fields within actual house areas
      and ((aHouse <> ht_Farm)     or not InRange(I, aY-2, aY) or not InRange(K, aX-1, aX+2))
      and ((aHouse <> ht_Wineyard) or not InRange(I, aY-1, aY) or not InRange(K, aX-2, aX)) then
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
  I, K, J: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  S: TKMPoint;
begin
  Result := False;
  Assert(aHouse in [ht_Farm, ht_Wineyard]);

  SeedLocs := GetSeeds(aSeed);

  BestBid := MaxSingle;
  for J := Low(SeedLocs) to High(SeedLocs) do
  begin
    S := SeedLocs[J];
    for I := Max(S.Y - 7, 1) to Min(S.Y + 6, gTerrain.MapY - 1) do
    for K := Max(S.X - 7, 1) to Min(S.X + 7, gTerrain.MapX - 1) do
    if CanPlaceHouse(aHouse, K, I) then
    begin
      Bid := KMLength(KMPoint(K,I), S)
             - gAIFields.Influences.Ownership[fOwner, I, K] / 5
             + KaMRandom * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
  end;
end;


function TKMCityPlanner.NextToHouse(aHouse: THouseType; aSeed, aAvoid: array of THouseType; out aLoc: TKMPoint): Boolean;
var
  I: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  Locs: TKMPointTagList;
begin
  Result := False;

  SeedLocs := GetSeeds(aSeed);

  Locs := TKMPointTagList.Create;
  try
    FindNearest(SeedLocs, 32, aHouse, 12, Locs);

    BestBid := MaxSingle;
    for I := 0 to Locs.Count - 1 do
    begin
      Bid := Locs.Tag[I]
             - gAIFields.Influences.Ownership[fOwner,Locs[I].Y,Locs[I].X] / 5;
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
function TKMCityPlanner.NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RAD = 8;
var
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPoint;
  Locs: TKMPointTagList;
  SeedLocs: TKMPointArray;
  J, M: Integer;
  tmp: TKMPointDir;
begin
  Result := False;

  SeedLocs := GetSeeds([ht_Any]);

  Locs := TKMPointTagList.Create;
  try
    //Find all tiles from which stone can be mined, by walking to them
    FindNearest(SeedLocs, 32, fnStone, [tpWalk], 12, Locs);
    if Locs.Count = 0 then Exit;

    //Check few random tiles if we can build Quary nearby
    BestBid := MaxSingle;
    for J := 0 to 2 do
    begin
      M := KaMRandom(Locs.Count);
      StoneLoc := Locs[M];
      for I := Max(StoneLoc.Y - SEARCH_RAD, 1) to Min(StoneLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
      for K := Max(StoneLoc.X - SEARCH_RAD, 1) to Min(StoneLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
      begin
        Bid := Locs.Tag[M]
               - gAIFields.Influences.Ownership[fOwner,I,K] / 10
               + KaMRandom * 3
               + KMLengthDiag(K, I, StoneLoc); //Distance to stone is important
        if (Bid < BestBid) then
        begin
          aLoc := KMPoint(K,I);
          BestBid := Bid;
          Result := True;
        end;
      end;
    end;
  finally
    Locs.Free;
  end;

  //Make sure stonemason actually can reach some stone (avoid build-destroy loop)
  if Result then
    if not gTerrain.FindStone(aLoc, gRes.Units[ut_StoneCutter].MiningRange, KMPOINT_ZERO, True, tmp) then
      Result := False;
end;


function TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean;
begin
  fFinder.FindType := aType;
  fFinder.HouseType := ht_None;
  Result := fFinder.FindNearest(aStart, aRadius, [tpWalkRoad, tpMakeRoads], aResultLoc);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPointArray; aRadius: Byte; aType: TFindNearest; aPass: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := aType;
  fFinder.HouseType := ht_None;
  fFinder.FindNearest(aStart, aRadius, aPass, aMaxCount, aLocs);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPointArray; aRadius: Byte; aHouse: THouseType; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := fnHouse;
  fFinder.HouseType := aHouse;
  fFinder.FindNearest(aStart, aRadius, [tpWalkRoad, tpMakeRoads], aMaxCount, aLocs);
end;


function TKMCityPlanner.NextToOre(aHouse: THouseType; aOreType: TWareType; out aLoc: TKMPoint; aNearAnyHouse: Boolean = False): Boolean;
var
  P: TKMPoint;
  SeedLocs: TKMPointArray;
begin
  Result := False;


  //Look for nearest Ore
  case aOreType of
    wt_Coal:    begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([ht_Any])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(ht_CoalMine) > 0 then
                      SeedLocs := GetSeeds([ht_CoalMine])
                    else
                      SeedLocs := GetSeeds([ht_Store]);
                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs))], 45, fnCoal, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
    wt_IronOre: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([ht_Any])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(ht_IronMine) > 0 then
                      SeedLocs := GetSeeds([ht_IronMine, ht_CoalMine])
                    else
                      SeedLocs := GetSeeds([ht_CoalMine, ht_Store]);
                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs))], 45, fnIron, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
    wt_GoldOre: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([ht_Any])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(ht_GoldMine) > 0 then
                      SeedLocs := GetSeeds([ht_GoldMine, ht_CoalMine])
                    else
                      SeedLocs := GetSeeds([ht_CoalMine, ht_Store]);
                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs))], 45, fnGold, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
  end;

  //todo: If there's no ore AI should not keep calling this over and over again
  // Maybe AI can cache search results for such non-replenishing resources

  aLoc := P;
  Result := True;
end;


function TKMCityPlanner.NextToTrees(aHouse: THouseType; aSeed: array of THouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RES = 7;
  SEARCH_RAD = 20; //Search for forests within this radius
  SEARCH_DIV = (SEARCH_RAD * 2) div SEARCH_RES + 1;
  HUT_RAD = 6; //Search for the best place for a hut in this radius
var
  I, K: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  seedLoc, TreeLoc: TKMPoint;
  Mx, My: SmallInt;
  MyForest: array [0..SEARCH_RES-1, 0..SEARCH_RES-1] of ShortInt;
begin
  Result := False;

  SeedLocs := GetSeeds(aSeed);
  if Length(SeedLocs) = 0 then Exit;

  // Pick one random seed loc from given
  seedLoc := SeedLocs[KaMRandom(Length(SeedLocs))];

    //todo: Rework through FindNearest to avoid roundabouts
  //Fill in MyForest map
  FillChar(MyForest[0,0], SizeOf(MyForest), #0);
  for I := Max(seedLoc.Y - SEARCH_RAD, 1) to Min(seedLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
  for K := Max(seedLoc.X - SEARCH_RAD, 1) to Min(seedLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
  if gTerrain.ObjectIsChopableTree(K, I) then
  begin
    Mx := (K - seedLoc.X + SEARCH_RAD) div SEARCH_DIV;
    My := (I - seedLoc.Y + SEARCH_RAD) div SEARCH_DIV;

    Inc(MyForest[My, Mx]);
  end;

  //Find cell with most trees
  BestBid := -MaxSingle;
  TreeLoc := seedLoc; //Init incase we cant find a spot at all
  for I := Low(MyForest) to High(MyForest) do
  for K := Low(MyForest[I]) to High(MyForest[I]) do
  begin
    Mx := Round(seedLoc.X - SEARCH_RAD + (K + 0.5) * SEARCH_DIV);
    My := Round(seedLoc.Y - SEARCH_RAD + (I + 0.5) * SEARCH_DIV);
    if InRange(Mx, 1, gTerrain.MapX - 1) and InRange(My, 1, gTerrain.MapY - 1)
    and (gAIFields.Influences.AvoidBuilding[My, Mx] = 0) then
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
  for I := Max(TreeLoc.Y - HUT_RAD, 1) to Min(TreeLoc.Y + HUT_RAD, gTerrain.MapY - 1) do
  for K := Max(TreeLoc.X - HUT_RAD, 1) to Min(TreeLoc.X + HUT_RAD, gTerrain.MapX - 1) do
    if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
    begin
      Bid := KMLength(KMPoint(K,I), seedLoc) + KaMRandom * 5;
      if (Bid < BestBid) then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


procedure TKMCityPlanner.OwnerUpdate(aPlayer: TKMHandIndex);
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
constructor TKMTerrainFinderCity.Create(aOwner: TKMHandIndex);
begin
  inherited Create;

  fOwner := aOwner;
end;


procedure TKMTerrainFinderCity.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


function TKMTerrainFinderCity.CanUse(const X, Y: Word): Boolean;
var
  I, K: Integer;
begin
  case FindType of
    fnHouse:  Result := gHands[fOwner].CanAddHousePlanAI(X, Y, HouseType, True);

    fnStone:  Result := (gTerrain.TileIsStone(X, Max(Y-1, 1)) > 1);

    fnCoal:   Result := (gTerrain.TileIsCoal(X, Y) > 1)
                         and gHands[fOwner].CanAddHousePlanAI(X, Y, ht_CoalMine, False);

    fnIron:   begin
                Result := gHands[fOwner].CanAddHousePlanAI(X, Y, ht_IronMine, False);
                //If we can build a mine here then search for ore
                if Result then
                  for I:=Max(X-4, 1) to Min(X+3, gTerrain.MapX) do
                    for K:=Max(Y-8, 1) to Y do
                      if gTerrain.TileIsIron(I, K) > 0 then
                        Exit;
                Result := False; //Didn't find any ore
              end;

    fnGold:   begin
                Result := gHands[fOwner].CanAddHousePlanAI(X, Y, ht_GoldMine, False);
                //If we can build a mine here then search for ore
                if Result then
                  for I:=Max(X-4, 1) to Min(X+4, gTerrain.MapX) do
                    for K:=Max(Y-8, 1) to Y do
                      if gTerrain.TileIsGold(I, K) > 0 then
                        Exit;
                Result := False; //Didn't find any ore
              end;

    else      Result := False;
  end;
end;


function TKMTerrainFinderCity.CanWalkHere(const X,Y: Word): Boolean;
var
  TerOwner: TKMHandIndex;
begin
  //Check for specific passabilities
  case FindType of
    fnIron:   Result := (fPassability * gTerrain.Land[Y,X].Passability <> [])
                        or gTerrain.TileGoodForIron(X, Y);

    fnGold:   Result := (fPassability * gTerrain.Land[Y,X].Passability <> [])
                        or gTerrain.TileGoodForGoldmine(X, Y);

    else      Result := (fPassability * gTerrain.Land[Y,X].Passability <> []);
  end;

  if not Result then Exit;

  //Don't build on allies and/or enemies territory
  TerOwner := gAIFields.Influences.GetBestOwner(X,Y);
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
