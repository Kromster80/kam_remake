unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points;


type
  TCityInfluence = (ciGold);

  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;

    //Stone, Wood, Farm, Wine, Coal,
    fInfluenceMap: array of array of array [TCityInfluence] of Byte;

    function NextToHouse(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToTrees(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToGrass(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;

    procedure UpdateInfluence;
  end;


implementation
uses KM_Houses, KM_Terrain, KM_PlayersCollection;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  case aHouse of
    ht_School:    Result := NextToHouse(ht_Store, aHouse, aLoc);
    ht_Inn:       Result := NextToHouse(ht_Store, aHouse, aLoc);
    ht_Sawmill:   Result := NextToHouse(ht_Woodcutters, aHouse, aLoc);
    ht_Mill:      Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Bakery:    Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Swine:     Result := NextToHouse(ht_Farm, aHouse, aLoc);
    ht_Butchers:  Result := NextToHouse(ht_Swine, aHouse, aLoc);
    ht_Tannery:   Result := NextToHouse(ht_Swine, aHouse, aLoc);

    ht_Quary:         Result := NextToStone(aHouse, aLoc);
    ht_Woodcutters:   Result := NextToTrees(aHouse, aLoc);
    ht_Farm:          Result := NextToHouse(ht_Store, aHouse, aLoc);//NextToGrass(aHouse, aLoc);
    ht_Wineyard:      Result := NextToHouse(ht_Store, aHouse, aLoc);//NextToGrass(aHouse, aLoc);
  end;

  //Result := True;
end;


function TKMCityPlanner.NextToGrass(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  StoreLoc: TKMPoint;
  TreeLoc: TKMPointDir;
  TA: TPlantAct;
begin
  {Result := False;

  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);
  if S = nil then Exit;

  StoreLoc := S.GetPosition;

  if not fTerrain.FindTree(KMPointBelow(S.GetPosition), 20, KMPoint(0,0), taAny, TreeLoc, TA) then Exit;

  BestBid := MaxSingle;

  for I := Max(TreeLoc.Loc.Y - 5, 1) to Min(TreeLoc.Loc.Y + 6, fTerrain.MapY - 1) do
  for K := Max(TreeLoc.Loc.X - 7, 1) to Min(TreeLoc.Loc.X + 7, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlan(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + Random * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;}
end;


function TKMCityPlanner.NextToHouse(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  TargetLoc: TKMPoint;
begin
  Result := False;
  S := fPlayers[fOwner].Houses.FindHouse(aTarget, 0, 0, 1, True);
  if S = nil then
    S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);

  BestBid := MaxSingle;
  TargetLoc := S.GetEntrance;

  for I := Max(TargetLoc.Y - 10, 1) to Min(TargetLoc.Y + 10, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - 10, 1) to Min(TargetLoc.X + 10, fTerrain.MapX - 1) do
    if fPlayers[fOwner].CanAddHousePlanAI(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), TargetLoc) + Random * 3;
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
    if fPlayers[fOwner].CanAddHousePlanAI(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + Random * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


function TKMCityPlanner.NextToTrees(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
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
    if fPlayers[fOwner].CanAddHousePlanAI(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + Random * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


procedure TKMCityPlanner.UpdateInfluence;
var I, K: Integer;
begin
  SetLength(fInfluenceMap, fTerrain.MapX, fTerrain.MapY);

  //Fill influence
  for I := 1 to fTerrain.MapY - 2 do
    for K := 1 to fTerrain.MapX - 1 do
    begin
      fInfluenceMap[I,K,ciGold] := Byte(CanBuildGold in fTerrain.Land[I,K].Passability) * 255;
      fInfluenceMap[I+1,K,ciGold] := Byte(CanBuildGold in fTerrain.Land[I,K].Passability) * 255;
    end;

  //Blur
  for I := 1 to fTerrain.MapY - 1 do
    for K := 1 to fTerrain.MapX - 1 do
      fInfluenceMap[I,K,ciGold] := Byte(CanBuildGold in fTerrain.Land[I,K].Passability) * 255;



end;

end.
