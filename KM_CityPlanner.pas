unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils, TypInfo,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_TerrainFinder, KM_PerfLog;


type
  TFindNearest = (fnStone, fnTrees, fnSoil, fnWater, fnCoal, fnIron, fnGold);

  TKMTerrainFinderCity = class(TKMTerrainFinderCommon)
  protected
    fOwner: TPlayerIndex;
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  public
    FindType: TFindNearest;
    constructor Create(aOwner: TPlayerIndex);
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
  end;

  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;
    fFinder: TKMTerrainFinderCity;

    function GetSourceLocation(aHouseType: array of THouseType; out Loc: TKMPoint): Boolean;

    function NextToOre(aHouse: THouseType; aOreType: TResourceType; out aLoc: TKMPoint): Boolean;
    function NextToHouse(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function FindPlaceForQuary(out aLoc: TKMPoint): Boolean;
    function NextToTrees(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToGrass(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    function FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean;
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
end;


destructor TKMCityPlanner.Destroy;
begin
  fFinder.Free;

  inherited;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  Result := False;

  case aHouse of
    ht_Store:           Result := NextToHouse([ht_Any], aHouse, aLoc);
    ht_ArmorSmithy:     Result := NextToHouse([ht_IronSmithy, ht_CoalMine, ht_Barracks], aHouse, aLoc);
    ht_ArmorWorkshop:   Result := NextToHouse([ht_Tannery, ht_Barracks], aHouse, aLoc);
    ht_Bakery:          Result := NextToHouse([ht_Mill], aHouse, aLoc);
    ht_Barracks:        Result := NextToHouse([ht_Any], aHouse, aLoc);
    ht_Butchers:        Result := NextToHouse([ht_Swine], aHouse, aLoc);
    ht_Inn:             Result := NextToHouse([ht_Store], aHouse, aLoc);
    ht_IronSmithy:      Result := NextToHouse([ht_IronMine, ht_CoalMine], aHouse, aLoc);
    ht_Metallurgists:   Result := NextToHouse([ht_GoldMine], aHouse, aLoc);
    ht_Mill:            Result := NextToHouse([ht_Farm], aHouse, aLoc);
    ht_Sawmill:         Result := NextToHouse([ht_Woodcutters], aHouse, aLoc);
    ht_School:          Result := NextToHouse([ht_Store, ht_Barracks], aHouse, aLoc);
    ht_Stables:         Result := NextToHouse([ht_Farm], aHouse, aLoc);
    ht_Swine:           Result := NextToHouse([ht_Farm], aHouse, aLoc);
    ht_Tannery:         Result := NextToHouse([ht_Swine], aHouse, aLoc);
    ht_WeaponSmithy:    Result := NextToHouse([ht_IronSmithy, ht_CoalMine, ht_Barracks], aHouse, aLoc);
    ht_WeaponWorkshop:  Result := NextToHouse([ht_Sawmill, ht_Barracks], aHouse, aLoc);

    ht_CoalMine:      Result := NextToOre(aHouse, rt_Coal, aLoc);
    ht_GoldMine:      Result := NextToOre(aHouse, rt_GoldOre, aLoc);
    ht_IronMine:      Result := NextToOre(aHouse, rt_IronOre, aLoc);

    ht_Quary:         Result := FindPlaceForQuary(aLoc);
    ht_Woodcutters:   Result := NextToTrees([ht_Store, ht_Woodcutters], aHouse, aLoc);
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


function TKMCityPlanner.NextToGrass(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  function CanPlaceHouse(aHouse: THouseType; aX, aY: Word): Boolean;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := False;
    if fPlayers[fOwner].CanAddHousePlanAI(aX, aY, aHouse, False)
    and (aHouse in [ht_Farm, ht_Wineyard]) then
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

  if not GetSourceLocation([aTarget], TargetLoc) then Exit;

  BestBid := MaxSingle;
  for I := Max(TargetLoc.Y - 5, 1) to Min(TargetLoc.Y + 6, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - 7, 1) to Min(TargetLoc.X + 7, fTerrain.MapX - 1) do
  if fAIFields.Influences.AvoidBuilding[I,K] = 0 then
  if CanPlaceHouse(aHouse, K, I) then
    begin
      Bid := KMLength(KMPoint(K,I), TargetLoc) + KaMRandom * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


function TKMCityPlanner.NextToHouse(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
const RAD = 15;
var
  I, K: Integer;
  Bid, BestBid: Single;
  TerOwner: TPlayerIndex;
  TargetLoc: TKMPoint;
  P: TKMPlayer;
begin
  Result := False;

  if not GetSourceLocation(aTarget, TargetLoc) then Exit;

  P := fPlayers[fOwner];

  BestBid := MaxSingle;
  for I := Max(TargetLoc.Y - RAD, 1) to Min(TargetLoc.Y + RAD, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - RAD, 1) to Min(TargetLoc.X + RAD, fTerrain.MapX - 1) do
    if P.CanAddHousePlanAI(K, I, aHouse, False) then
    begin
      Bid := KMLengthDiag(KMPoint(K,I), TargetLoc) - fAIFields.Influences.Ownership[fOwner,I,K] + KaMRandom * 5;
      TerOwner := fAIFields.Influences.GetBestOwner(K,I);
      if (Bid < BestBid) and ((TerOwner = fOwner) or (TerOwner = PLAYER_NONE)) then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


//Called when AI needs to find a good spot for new Quary
function TKMCityPlanner.FindPlaceForQuary(out aLoc: TKMPoint): Boolean;
const RAD = 32;
var
  I, K: Integer;
  Bid, BestBid: Single;
  TerOwner: TPlayerIndex;
  StoneLoc: TKMPointDir;
  TargetLoc: TKMPoint;
begin
  Result := False;

  if not GetSourceLocation([ht_Store], TargetLoc) then Exit;

  //todo: Using FindStone with such a large radius is very slow due to GetTilesWithinDistance (10ms average)
  //      Maybe instead check 60x60 area, but only each 5th cell or so
  if not fTerrain.FindStone(KMPointBelow(TargetLoc), RAD, KMPoint(0,0), True, StoneLoc) then Exit;
  //if not FindNearest(KMPointBelow(TargetLoc), RAD, fnStone, StoneLoc.Loc) then Exit;

  BestBid := MaxSingle;
  for I := StoneLoc.Loc.Y to Min(StoneLoc.Loc.Y + 5, fTerrain.MapY - 1) do
  for K := Max(StoneLoc.Loc.X - 5, 1) to Min(StoneLoc.Loc.X + 5, fTerrain.MapX - 1) do
  if (fAIFields.Influences.AvoidBuilding[I,K] = 0)
  and fPlayers[fOwner].CanAddHousePlanAI(K, I, ht_Quary, False) then
  begin
    Bid := KMLength(KMPoint(K,I), TargetLoc) - fAIFields.Influences.Ownership[fOwner,I,K] + KaMRandom * 4;
    TerOwner := fAIFields.Influences.GetBestOwner(K,I);
    if (Bid < BestBid) and ((TerOwner = fOwner) or (TerOwner = PLAYER_NONE)) then
    begin
      aLoc := KMPoint(K,I);
      BestBid := Bid;
      Result := True;
    end;
  end;
end;


function TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean;
begin
  fFinder.FindType := aType;
  Result := fFinder.FindNearest(aStart, aRadius, CanOwn, aResultLoc);
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
    rt_Coal:    if not FindNearest(TargetLoc, 40, fnCoal, P) then Exit;
    rt_IronOre: if not FindNearest(TargetLoc, 40, fnIron, P) then Exit;
    rt_GoldOre: if not FindNearest(TargetLoc, 40, fnGold, P) then Exit;
  end;

  aLoc := P;
  Result := True;
end;


function TKMCityPlanner.NextToTrees(aTarget: array of THouseType; aHouse: THouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RAD = 20; //Search for forests within this radius
  HUT_RAD = 5; //Search for the best place for a hut in this radius
var
  I, K: Integer;
  Bid, BestBid: Single;
  TerOwner: TPlayerIndex;
  TargetLoc: TKMPoint;
  TreeLoc: TKMPoint;
  Mx, My: SmallInt;
  MyForest: array [0..7, 0..7] of ShortInt;
begin
  Result := False;

  if not GetSourceLocation(aTarget, TargetLoc) then Exit;

  //Fill in MyForest map
  FillChar(MyForest[0,0], SizeOf(MyForest), #0);
  for I := Max(TargetLoc.Y - SEARCH_RAD, 1) to Min(TargetLoc.Y + SEARCH_RAD, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - SEARCH_RAD, 1) to Min(TargetLoc.X + SEARCH_RAD, fTerrain.MapX - 1) do
  if fTerrain.ObjectIsChopableTree(K, I) then
  begin
    Mx := Round((K - TargetLoc.X + SEARCH_RAD) / (SEARCH_RAD * 2 + 1) * 7);
    My := Round((I - TargetLoc.Y + SEARCH_RAD) / (SEARCH_RAD * 2 + 1) * 7);

    Inc(MyForest[My, Mx]);
  end;

  //Find cell with most trees
  BestBid := -MaxSingle;
  TreeLoc := TargetLoc; //Init incase we cant find a spot at all
  for I := Low(MyForest) to High(MyForest) do
  for K := Low(MyForest[I]) to High(MyForest[I]) do
  begin
    Mx := Round(K / 7 * (SEARCH_RAD * 2 + 1) + TargetLoc.X - SEARCH_RAD);
    My := Round(I / 7 * (SEARCH_RAD * 2 + 1) + TargetLoc.Y - SEARCH_RAD);
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
    if (fAIFields.Influences.AvoidBuilding[I,K] = 0)
    and fPlayers[fOwner].CanAddHousePlanAI(K, I, aHouse, False) then
    begin
      Bid := KMLength(KMPoint(K,I), TargetLoc) + KaMRandom * 5;
      TerOwner := fAIFields.Influences.GetBestOwner(K,I);
      if (Bid < BestBid) and ((TerOwner = fOwner) or (TerOwner = PLAYER_NONE)) then
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
  //
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
begin
  //
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
    fnStone:  Result := (fTerrain.TileIsStone(X, Max(Y-1, 1)) > 1);
    fnCoal:   Result := (fTerrain.TileIsCoal(X, Y) > 1)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_CoalMine, True);
    fnIron:   Result := (fTerrain.TileIsIron(X, Max(Y-1, 1)) > 0)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_IronMine, True);
    fnGold:   Result := (fTerrain.TileIsGold(X, Max(Y-1, 1)) > 0)
                         and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_GoldMine, True);
    else      Result := False;
  end;
end;


function TKMTerrainFinderCity.CanWalkHere(const X,Y: Word): Boolean;
var
  TerOwner: TPlayerIndex;
begin
  //Don't build on allies and/or enemies territory
  TerOwner := fAIFields.Influences.GetBestOwner(X,Y);
  Result := (TerOwner = fOwner) or (TerOwner = PLAYER_NONE);
end;


end.
