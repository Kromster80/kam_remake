unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils, TypInfo,
  KM_Defaults, KM_Points, KM_CommonClasses;


type
  TCityInfluence = (ciCoal, ciGold, ciIron);

  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;

    //Stone, Wood, Farm, Wine, Coal,
    fInfluenceMap: array of array of array [TCityInfluence] of Byte;

    function NextToHouse(aTarget, aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    function NextToTrees(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
    //function NextToGrass(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;

    procedure OwnerUpdate(aPlayer:TPlayerIndex);

    procedure UpdateInfluence;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Houses, KM_Terrain, KM_PlayersCollection, KM_Utils;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;

  UpdateInfluence;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  Result := False;

  case aHouse of
    //ht_Store:     Result := NextToHouse(ht_Store, aHouse, aLoc);
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
  function CanPlaceHouse(aHouse: THouseType; aPos: TKMPoint): Boolean;
  const RAD = 4;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := fPlayers[fOwner].CanAddHousePlanAI(aPos, aHouse);
    if Result and (aHouse in [ht_Farm, ht_Wineyard]) then
    begin
      FieldCount := 0;
      for I := Min(aPos.Y + 2, fTerrain.MapY - 1) to Min(aPos.Y + RAD, fTerrain.MapY - 1) do
      for K := Max(aPos.X - RAD, 1) to Min(aPos.X + RAD, fTerrain.MapX - 1) do
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
begin
  Result := False;
  TargetH := fPlayers[fOwner].Houses.FindHouse(aTarget, 0, 0, 1, True);

  if TargetH = nil then
    Exit;

  BestBid := MaxSingle;
  TargetLoc := TargetH.GetEntrance;

  for I := Max(TargetLoc.Y - 10, 1) to Min(TargetLoc.Y + 10, fTerrain.MapY - 1) do
  for K := Max(TargetLoc.X - 10, 1) to Min(TargetLoc.X + 10, fTerrain.MapX - 1) do
    if CanPlaceHouse(aHouse, KMPoint(K,I)) then
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
    if fPlayers[fOwner].CanAddHousePlanAI(KMPoint(K,I), aHouse) then
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


procedure TKMCityPlanner.UpdateInfluence;
const
  BSize = 4;
  BLen = Sqr(BSize * 2 + 1);
var
  CI: TCityInfluence;
  I, K, M, N: Integer;
  T: Integer;
  Tmp: array of array of byte;
  //Bmp: TBitmap;
begin
  SetLength(fInfluenceMap, fTerrain.MapY, fTerrain.MapX);
  SetLength(Tmp, fTerrain.MapY, fTerrain.MapX);

  //Fill influence
  for I := 1 to fTerrain.MapY - 1 do
    for K := 1 to fTerrain.MapX - 1 do
    begin
      fInfluenceMap[I, K, ciCoal] := (Byte(fTerrain.Land[I, K].Terrain in [152, 153, 154, 155]) * 255);
      fInfluenceMap[I, K, ciGold] := (Byte(CanBuildGold in fTerrain.Land[I, K].Passability) * 255);
      fInfluenceMap[I, K, ciIron] := (Byte(CanBuildIron in fTerrain.Land[I, K].Passability) * 255);
    end;

  for CI := Low(TCityInfluence) to High(TCityInfluence) do
  begin
    for I := 2 to fTerrain.MapY - 1 do
      for K := 2 to fTerrain.MapX - 2 do
        Tmp[I, K] := fInfluenceMap[I, K, CI] or fInfluenceMap[I - 1, K, CI] or fInfluenceMap[I - 1, K - 1, CI] or
          fInfluenceMap[I - 1, K + 1, CI];

    //Blur (naive unweightened implementation)
    for I := 1 to fTerrain.MapY - 1 do
      for K := 1 to fTerrain.MapX - 1 do
      begin
        T := 0;
        for M := Max(I - BSize, 1) to Min(I + BSize, fTerrain.MapY - 1) do
          for N := Max(K - BSize, 1) to Min(K + BSize, fTerrain.MapX - 1) do
            T := T + Tmp[M, N];
        fInfluenceMap[I, K, CI] := fInfluenceMap[I, K, CI] or Tmp[I, K] or Min(T shl 2 div BLen, 255);
      end;

    {Bmp := TBitmap.Create;
    Bmp.PixelFormat := pf32bit;
    Bmp.Width := fTerrain.MapX;
    Bmp.Height := fTerrain.MapY;
    for I := 0 to fTerrain.MapY - 1 do
      for K := 0 to fTerrain.MapX - 1 do
        Bmp.Canvas.Pixels[K,I] := fInfluenceMap[I,K,CI] * 65793;// or $FF000000;

    Bmp.SaveToFile(ExeDir + GetEnumName(TypeInfo(TCityInfluence), Integer(CI)) + '.bmp');
    Bmp.Free;}
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
