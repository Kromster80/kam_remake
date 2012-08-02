unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils, TypInfo,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_PerfLog;


type
  //TCityInfluence = (ciAvoid, ciTrees);

  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;
    fPerfLog: TKMPerfLog;

    function FindNearestOre(aStart: TKMPoint; aRadius: Byte; aOreType: TResourceType; out aEnd: TKMPoint): Boolean;
    function NextToOre(aHouse: THouseType; aOreType: TResourceType; out aLoc: TKMPoint): Boolean;
    function NextToHouse(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToTrees(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    //function NextToGrass(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    //Stone, Wood, Farm, Wine, Coal,
    //InfluenceMap: array of array of array [TCityInfluence] of Byte;

    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    procedure OwnerUpdate(aPlayer:TPlayerIndex);
    procedure UpdateInfluence;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Houses, KM_Terrain, KM_Player, KM_PlayersCollection, KM_Utils;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;

  if DO_PERF_LOGGING then fPerfLog := TKMPerfLog.Create;
  //UpdateInfluence;
end;


destructor TKMCityPlanner.Destroy;
begin
  if DO_PERF_LOGGING then fPerfLog.SaveToFile(ExeDir + 'Logs\PerfLogCity'+IntToStr(fOwner)+'.txt');
  if DO_PERF_LOGGING then fPerfLog.Free;

  inherited;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  Result := False;

  case aHouse of
    //ht_Store:       Result := NextToHouse(ht_Store, aHouse, aLoc);
    ht_ArmorSmithy:     Result := NextToHouse(ht_IronSmithy, aHouse, aLoc);
    ht_ArmorWorkshop:   Result := NextToHouse(ht_Tannery, aHouse, aLoc);
    ht_Bakery:          Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Barracks:        Result := NextToHouse(ht_GoldMine, aHouse, aLoc);
    ht_Butchers:        Result := NextToHouse(ht_Swine, aHouse, aLoc);
    ht_Inn:             Result := NextToHouse(ht_Store, aHouse, aLoc);
    ht_IronSmithy:      Result := NextToHouse(ht_IronMine, aHouse, aLoc);
    ht_Metallurgists:   Result := NextToHouse(ht_GoldMine, aHouse, aLoc);
    ht_Mill:            Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Sawmill:         Result := NextToHouse(ht_Woodcutters, aHouse, aLoc);
    ht_School:          Result := NextToHouse(ht_Store, aHouse, aLoc);
    ht_Swine:           Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Tannery:         Result := NextToHouse(ht_Swine, aHouse, aLoc);
    ht_WeaponSmithy:    Result := NextToHouse(ht_IronSmithy, aHouse, aLoc);
    ht_WeaponWorkshop:  Result := NextToHouse(ht_Sawmill, aHouse, aLoc);

    ht_CoalMine:      Result := NextToOre(ht_CoalMine, rt_Coal, aLoc);
    ht_GoldMine:      Result := NextToOre(ht_GoldMine, rt_GoldOre, aLoc);
    ht_IronMine:      Result := NextToOre(ht_IronMine, rt_IronOre, aLoc);

    ht_Quary:         Result := NextToStone(aHouse, aLoc);
    ht_Woodcutters:   Result := NextToTrees(aHouse, aLoc);
    ht_Farm:          Result := NextToHouse(ht_Store, aHouse, aLoc);//NextToGrass(aHouse, aLoc);
    ht_Wineyard:      Result := NextToHouse(ht_Store, aHouse, aLoc);//NextToGrass(aHouse, aLoc);
  end;
end;


{function TKMCityPlanner.NextToGrass(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  StoreLoc: TKMPoint;
  TreeLoc: TKMPointDir;
  TA: TPlantAct;
begin
  Result := False;

  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);
  if S = nil then Exit;

  StoreLoc := S.GetPosition;

  if not fTerrain.FindTree(KMPointBelow(S.GetPosition), 20, KMPoint(0,0), taAny, TreeLoc, TA) then Exit;

  BestBid := MaxSingle;

  for I := Max(TreeLoc.Loc.Y - 5, 1) to Min(TreeLoc.Loc.Y + 6, fTerrain.MapY - 1) do
  for K := Max(TreeLoc.Loc.X - 7, 1) to Min(TreeLoc.Loc.X + 7, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlan(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + KaMRandom * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;}


function TKMCityPlanner.NextToHouse(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  function CanPlaceHouse(aHouse: THouseType; aX, aY: Word): Boolean;
  const RAD = 4;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := fPlayers[fOwner].CanAddHousePlanAI(aX, aY, aHouse, False);
    if Result and (aHouse in [ht_Farm, ht_Wineyard]) then
    begin
      FieldCount := 0;
      for I := Min(aY + 2, fTerrain.MapY - 1) to Min(aY + RAD, fTerrain.MapY - 1) do
      for K := Max(aX - RAD, 1) to Min(aX + RAD, fTerrain.MapX - 1) do
        if CanMakeFields in fTerrain.Land[I,K].Passability then
          Inc(FieldCount);
      Result := FieldCount >= 15;
    end;
  end;
var
  TargetH: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  TargetLoc: TKMPoint;
  P: TKMPlayer;
begin
  Result := False;

  P := fPlayers[fOwner];
  TargetH := P.Houses.FindHouse(aTarget, 0, 0, KaMRandom(P.Stats.GetHouseQty(aHouse)) + 1);
  if TargetH = nil then Exit;

  BestBid := MaxSingle;
  TargetLoc := TargetH.GetEntrance;

  for I := Max(TargetLoc.Y - 10, 1) to Min(TargetLoc.Y + 10, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - 10, 1) to Min(TargetLoc.X + 10, fTerrain.MapX - 1) do
    if CanPlaceHouse(aHouse, K, I) then
    begin
      Bid := GetLength(KMPoint(K,I), TargetLoc) + KaMRandom * 3;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


function TKMCityPlanner.NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPointDir;
  StoreLoc: TKMPoint;
begin
  Result := False;

  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);
  if S = nil then Exit;

  StoreLoc := S.GetPosition;

  if not fTerrain.FindStone(KMPointBelow(S.GetPosition), 20, KMPoint(0,0), StoneLoc) then Exit;

  BestBid := MaxSingle;

  for I := Min(StoneLoc.Loc.Y + 2, fTerrain.MapY - 1) to Min(StoneLoc.Loc.Y + 4, fTerrain.MapY - 1) do
  for K := Max(StoneLoc.Loc.X - 5, 1) to Min(StoneLoc.Loc.X + 5, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlanAI(K, I, aHouse, False) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + KaMRandom * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


function TKMCityPlanner.FindNearestOre(aStart: TKMPoint; aRadius: Byte; aOreType: TResourceType; out aEnd: TKMPoint): Boolean;
var
  MapX, MapY: Word;
  Visited: array of array of Byte;
  BestDist: Byte;
  BestLoc: TKMPoint;

  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(X,Y: Word; aWalkDistance: Byte);
  var Xt, Yt: Word;
  begin
    Xt := aStart.X - X + aRadius;
    Yt := aStart.Y - Y + aRadius;

    //If new path is longer than old we don't care about it
    if (aWalkDistance >= Visited[Xt,Yt]) then Exit;

    //New path is new or better than old
    if ((aOreType = rt_Coal) and (fTerrain.TileIsCoal(X, Y) > 1)
      and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_CoalMine, True))
    or ((aOreType = rt_IronOre) and (fTerrain.TileIsIron(X, Max(Y-1, 1)) > 0)
      and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_IronMine, True))
    or ((aOreType = rt_GoldOre) and (fTerrain.TileIsGold(X, Max(Y-1, 1)) > 0)
      and fPlayers[fOwner].CanAddHousePlanAI(X, Y, ht_GoldMine, True))
    then
    begin
      BestDist := aWalkDistance;
      BestLoc := KMPoint(X,Y);
    end;

    //Mark this tile as visited
    Visited[Xt,Yt] := aWalkDistance;

    //Run again on surrounding tiles
    //We check only 4 neighbors, because that x6 times faster than 8 neighbors
    //and we don't really care for perfect circle test
    if (aWalkDistance + 2 <= BestDist) then
    begin
      if X-1 >= 1 then     Visit(X-1, Y, aWalkDistance+2);
      if Y-1 >= 1 then     Visit(X, Y-1, aWalkDistance+2);
      if Y+1 <= MapY then  Visit(X, Y+1, aWalkDistance+2);
      if X+1 <= MapX then  Visit(X+1, Y, aWalkDistance+2);
    end;
  end;

var I,K: Integer;
begin
  Assert(aRadius <= 120, 'GetTilesWithinDistance can''t handle radii > 80');

  //Assign Rad to local variable,
  //when we find better Loc we reduce the Rad to skip any farther Locs
  BestDist := aRadius * 2;
  BestLoc := KMPoint(0,0);
  MapX := fTerrain.MapX;
  MapY := fTerrain.MapY;

  SetLength(Visited, 2*aRadius+1, 2*aRadius+1);
  for I := 0 to 2 * aRadius do
  for K := 0 to 2 * aRadius do
    Visited[I,K] := 255; //Initially all tiles are unexplored

  Visit(aStart.X, aStart.Y, 0); //Starting tile is at walking distance zero

  aEnd := BestLoc;
  Result := BestLoc.X <> 0;
end;


function TKMCityPlanner.NextToOre(aHouse: THouseType; aOreType: TResourceType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  P: TKMPoint;
  StoreLoc: TKMPoint;
begin
  Result := False;

  //Store is the center of our town
  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);
  if S = nil then Exit;
  StoreLoc := S.GetPosition;

  //Look for nearest Ore
  if not FindNearestOre(StoreLoc, 40, aOreType, P) then Exit;

  aLoc := P;
  Result := True;
end;


function TKMCityPlanner.NextToTrees(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  Tmp, StoreLoc: TKMPoint;
  TreeLoc: TKMPointDir;
  Trees: TKMPointDirList;
  Stumps,Empty: TKMPointList;
begin
  Result := False;

  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);
  if S = nil then Exit;

  StoreLoc := S.GetPosition;

  Trees := TKMPointDirList.Create;
  Stumps := TKMPointList.Create;
  Empty := TKMPointList.Create;
  fTerrain.FindTree(KMPointBelow(S.GetPosition), 20, KMPoint(0,0), taAny, Trees, Stumps, Empty);
  if not Trees.GetRandom(TreeLoc) then
  begin
    if not Stumps.GetRandom(Tmp) then
      if not Empty.GetRandom(Tmp) then Exit;
    TreeLoc := KMPointDir(Tmp,Dir_NA);
  end;
  Trees.Free;
  Stumps.Free;
  Empty.Free;

  BestBid := MaxSingle;

  for I := Max(TreeLoc.Loc.Y - 5, 1) to Min(TreeLoc.Loc.Y + 6, fTerrain.MapY - 1) do
  for K := Max(TreeLoc.Loc.X - 7, 1) to Min(TreeLoc.Loc.X + 7, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlanAI(K, I, aHouse, False) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + KaMRandom * 4;
      if Bid < BestBid then
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
end;


//todo: Move terrain parts to Terrain
procedure TKMCityPlanner.UpdateInfluence;
const
  BSize = 3;
  BLen = Sqr(BSize * 2 + 1);
var
  //CI: TCityInfluence;
  S: TKMHouse;
  I, K: Integer;
  Tmp: array of array of Byte;
  Bmp: TBitmap;
begin
  SetLength(Tmp, fTerrain.MapY, fTerrain.MapX);

  //Avoid Gold/Iron
  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
    Tmp[I, K] := Byte((fTerrain.TileIsIron(K, I) > 1)
                                     or (fTerrain.TileIsGold(K, I) > 1)) * $FF;

  //Bleed Gold/Iron to book space around mines for roads
  for I := 3 to fTerrain.MapY - 1 do
  for K := 2 to fTerrain.MapX - 2 do
    fTerrain.Land[I,K].Influence := fTerrain.Land[I,K].Influence
                                    or Tmp[I, K]
                                    or Tmp[I - 1, K - 1]
                                    or Tmp[I - 1, K]
                                    or Tmp[I - 1, K + 1]
                                    or Tmp[I - 2, K - 1]
                                    or Tmp[I - 2, K]
                                    or Tmp[I - 2, K + 1];

  //Avoid Coal
  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
    fTerrain.Land[I,K].Influence := fTerrain.Land[I,K].Influence or (Byte(fTerrain.TileIsCoal(K, I) > 1) * $FF);

  //Leave some free space below Store
  S := fPlayers[fOwner].FindHouse(ht_Store);
  if S <> nil then
  for I := S.GetEntrance.Y + 1 to Min(S.GetEntrance.Y + 2, fTerrain.MapY - 1) do
  for K := Max(S.GetEntrance.X - 2, 1) to Min(S.GetEntrance.X + 2, fTerrain.MapX - 1) do
    fTerrain.Land[I,K].Influence := fTerrain.Land[I,K].Influence or $FF;


  if EXPORT_INFLUENCE then
  //for CI := Low(TCityInfluence) to High(TCityInfluence) do
  begin
    Bmp := TBitmap.Create;
    Bmp.PixelFormat := pf32bit;
    Bmp.Width := fTerrain.MapX;
    Bmp.Height := fTerrain.MapY;
    for I := 1 to fTerrain.MapY - 1 do
      for K := 1 to fTerrain.MapX - 1 do
        Bmp.Canvas.Pixels[K,I] := fTerrain.Land[I,K].Influence * 65793;// or $FF000000;

    Bmp.SaveToFile(ExeDir + 'Influence.bmp');
    Bmp.Free;
  end;
end;


procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
end;

end.
