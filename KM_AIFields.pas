unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_Terrain, KM_Defaults,
  KM_Player, KM_Points, KM_NavMesh;

type
  //Strcucture to describe NavMesh layout
  TKMInfluences = class
  private
    fMapX: Word;
    fMapY: Word;
    fUpdatePlayerId: TPlayerIndex;
    Influence: array of array of array of Byte;
    procedure InitInfluenceAvoid;
    procedure InitInfluenceForest;
    procedure UpdateDirectInfluence;
    procedure UpdateOwnershipInfluence;
  public
    //Common map of areas where building is undesired (around Store, Mines, Woodcutters)
    AvoidBuilding: array of array of Byte;
    //Areas of forests, needed only for initial Woodcutters placement and gets calculated once on mission start
    Forest: array of array of Byte; //0..255 is enough

    Ownership: array of array of array of SmallInt;
    //Tension: array of array of array of SmallInt;

    procedure AddAvoidBuilding(X,Y: Word; aRad: Byte);
    function GetBestOwner(X, Y: Word): TPlayerIndex;
    procedure Init;
    procedure ExportInfluenceMaps;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


  //Influence maps, navmeshes, etc
  TKMAIFields = class
  private
    fNavMesh: TKMNavMesh;
    fInfluences: TKMInfluences;
  public
    constructor Create;
    destructor Destroy; override;

    property NavMesh: TKMNavMesh read fNavMesh;
    property Influences: TKMInfluences read fInfluences;

    procedure AfterMissionInit;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  fAIFields: TKMAIFields;


implementation
uses KM_PlayersCollection, KM_RenderAux, KM_Houses;


const
  //Decay affects how much space AI needs to be able to expand
  //Values smaller than 3 start to block green AI on AcrossDesert too fast
  INFLUENCE_DECAY = 5;
  INFLUENCE_ENEMY_DIV = 2;


{ TKMInfluenceMaps }
//Make the area around to be avoided by common houses
procedure TKMInfluences.AddAvoidBuilding(X,Y: Word; aRad: Byte);
var I,K: Integer;
begin
  for I := Max(Y - aRad, 1) to Min(Y + aRad, fMapY - 1) do
  for K := Max(X - aRad, 1) to Min(X + aRad, fMapX - 1) do
    if Sqr(X-K) + Sqr(Y-I) <= Sqr(aRAD) then
      AvoidBuilding[I,K] := 255;
end;


//Return index of player who has most influence on this tile, or none
function TKMInfluences.GetBestOwner(X, Y: Word): TPlayerIndex;
var
  I: Integer;
  Best: Integer;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to fPlayers.Count - 1 do
  if Ownership[I,Y,X] > Best then
  begin
    Best := Ownership[I,Y,X];
    Result := I;
  end;
end;


procedure TKMInfluences.Init;
begin
  fMapX := fTerrain.MapX;
  fMapY := fTerrain.MapY;
  SetLength(AvoidBuilding, fMapY, fMapX);
  SetLength(Forest, fMapY, fMapX);
  SetLength(Influence, fPlayers.Count, fMapY, fMapX);
  SetLength(Ownership, fPlayers.Count, fMapY, fMapX);
  InitInfluenceAvoid;
  InitInfluenceForest;
  UpdateDirectInfluence;
  UpdateOwnershipInfluence;
end;


//AI should avoid certain areas, keeping them for special houses
procedure TKMInfluences.InitInfluenceAvoid;
var
  S: TKMHouse;
  I, K, J: Integer;
  Bmp: TBitmap;
  M: Integer;
  N: Integer;
begin
  //Avoid areas where Gold/Iron mines should be
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  if (fTerrain.TileIsIron(K, I) > 1) or (fTerrain.TileIsGold(K, I) > 1) then
    for M := I to Min(I + 2, fMapY - 1) do
    for N := Max(K - 1, 1) to Min(K + 1, fMapX - 1) do
      AvoidBuilding[M, N] := 255;

  //Avoid Coal fields
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
   AvoidBuilding[I,K] := AvoidBuilding[I,K] or (Byte(fTerrain.TileIsCoal(K, I) > 1) * $FF);

  //Leave free space around all players Stores
  for J := 0 to fPlayers.Count - 1 do
  begin
    S := fPlayers[J].FindHouse(ht_Store);
    if S <> nil then
    for I := Max(S.GetEntrance.Y - 4, 1) to Min(S.GetEntrance.Y + 3, fMapY - 1) do
    for K := Max(S.GetEntrance.X - 3, 1) to Min(S.GetEntrance.X + 3, fMapX - 1) do
      AvoidBuilding[I,K] := AvoidBuilding[I,K] or $FF;
  end;
end;


procedure TKMInfluences.InitInfluenceForest;
  procedure DoFill(X,Y: SmallInt);
  const
    //Each tree adds this weight lineary fading to forest map
    TREE_WEIGHT = 8;
  var
    I,K: Integer;
  begin
    //Distribute tree weight lineary in TREE_WEIGHT radius
    //loops are TREE_WEIGHT-1 because we skip 0 weight edges
    for I := Max(Y - TREE_WEIGHT+1, 1) to Min(Y + TREE_WEIGHT-1, fMapY - 1) do
    for K := Max(X - TREE_WEIGHT+1, 1) to Min(X + TREE_WEIGHT-1, fMapX - 1) do
    if Abs(I-Y) + Abs(K-X) < TREE_WEIGHT then
      Forest[I,K] := Min(Forest[I,K] + TREE_WEIGHT - (Abs(I-Y) + Abs(K-X)), 255);
  end;
var
  I, K: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(fTerrain <> nil);

  for I := 0 to fMapY - 1 do
  for K := 0 to fMapX - 1 do
    Forest[I,K] := 0;

  //Update forest influence map
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  if fTerrain.ObjectIsChopableTree(K,I) then
    DoFill(K,I);
end;


procedure TKMInfluences.UpdateDirectInfluence;
var
  L: TPlayerIndex;

  procedure DoFill(X,Y: Integer; V: Byte);
  begin
    if  InRange(Y, 1, fMapY - 1)
    and InRange(X, 1, fMapX - 1)
    and (V > Influence[L, Y, X])
    and (CanOwn in fTerrain.Land[Y,X].Passability) then
    begin
      Influence[L, Y, X] := V;
      DoFill(X, Y-1, Max(V - INFLUENCE_DECAY, 0));
      DoFill(X-1, Y, Max(V - INFLUENCE_DECAY, 0));
      DoFill(X+1, Y, Max(V - INFLUENCE_DECAY, 0));
      DoFill(X, Y+1, Max(V - INFLUENCE_DECAY, 0));
      //We can get away with 4-tap fill, it looks fine already
    end;
  end;
var
  I, K, H: Integer;
  P: TKMPoint;
  PlayerHouses: TKMHousesCollection;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(fTerrain <> nil);

  L := fUpdatePlayerId; //Shortcut

  //Update direct influence maps
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
    Influence[L, I, K] := 0;

  //Sync tile ownership
  PlayerHouses := fPlayers[L].Houses;
  for I := 0 to PlayerHouses.Count - 1 do
  if PlayerHouses[I].HouseType <> ht_WatchTower then
  begin
    P := PlayerHouses[I].GetPosition;
    Influence[L, P.Y, P.X] := 254;
  end;

  //Expand influence with faloff
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  if Influence[L, I, K] = 254 then
    DoFill(K,I,Influence[L, I, K]+1);
end;


procedure TKMInfluences.UpdateOwnershipInfluence;
var
  I, K, H: Integer;
  L: TPlayerIndex;
  T: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(fTerrain <> nil);

  L := fUpdatePlayerId; //Shortcut

  //Fill Ownership map
  //Ownerhip = influence of player - influences of his enemies
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  begin
    if Influence[L, I, K] <> 0 then
    begin
      T := Influence[L, I, K];

      for H := 0 to fPlayers.Count - 1 do
      if (H <> L) and (fPlayers[L].Alliances[H] = at_Enemy) then
        T := T - Influence[H, I, K] div INFLUENCE_ENEMY_DIV;

      Ownership[L, I, K] := Max(T, 0);
    end
    else
      Ownership[L, I, K] := 0;
  end;

  //Normalization is not working as intended,
  //it gives unfair advantage to one using it,
  //muting neighbours areas beyond reason
end;


procedure TKMInfluences.ExportInfluenceMaps;
var
  I, J, K: Integer;
begin
  for J := 0 to fPlayers.Count - 1 do
  with TBitmap.Create do
  begin
    Width := fMapX;
    Height:= fMapY;
    PixelFormat := pf32bit;
    for I := 0 to Height-1 do
      for K := 0 to Width-1 do
        Canvas.Pixels[K,I] := Influence[J, I, K];
    SaveToFile(ExeDir + 'Export\Influence map Player'+IntToStr(J) + '.bmp');
  end;

  for J := 0 to fPlayers.Count - 1 do
  with TBitmap.Create do
  begin
    Width := fMapX;
    Height:= fMapY;
    PixelFormat := pf32bit;
    for I := 0 to Height-1 do
      for K := 0 to Width-1 do
        Canvas.Pixels[K,I] := Influence[J, I, K];
    SaveToFile(ExeDir + 'Export\Influence map Player'+IntToStr(J) + '.bmp');
  end;
end;


procedure TKMInfluences.Save(SaveStream: TKMemoryStream);
var
  PCount, SizeY, SizeX: Word;
  I: Integer;
  K: Integer;
  H: Integer;
begin
  PCount := Length(Influence);
  SizeY := Length(Influence[0]);
  SizeX := Length(Influence[0][0]);

  SaveStream.Write('Influences');

  SaveStream.Write(PCount);
  SaveStream.Write(SizeY);
  SaveStream.Write(SizeX);

  for I := 0 to PCount - 1 do
    for K := 0 to SizeY - 1 do
      SaveStream.Write(Influence[I,K,0], SizeX * SizeOf(Influence[0,0,0]));

  for I := 0 to PCount - 1 do
    for K := 0 to SizeY - 1 do
      SaveStream.Write(Ownership[I,K,0], SizeX * SizeOf(Ownership[0,0,0]));

  for K := 0 to SizeY - 1 do
    SaveStream.Write(AvoidBuilding[K,0], SizeX * SizeOf(AvoidBuilding[0,0]));

  for K := 0 to SizeY - 1 do
    SaveStream.Write(Forest[K,0], SizeX * SizeOf(Forest[0,0]));
end;


procedure TKMInfluences.Load(LoadStream: TKMemoryStream);
var
  PCount, SizeY, SizeX: Word;
  I: Integer;
  K: Integer;
  H: Integer;
begin
  LoadStream.ReadAssert('Influences');

  LoadStream.Read(PCount);
  LoadStream.Read(SizeY);
  LoadStream.Read(SizeX);

  SetLength(Influence, PCount, SizeY, SizeX);
  SetLength(Ownership, PCount, SizeY, SizeX);
  SetLength(AvoidBuilding, SizeY, SizeX);
  SetLength(Forest, SizeY, SizeX);

  for I := 0 to PCount - 1 do
    for K := 0 to SizeY - 1 do
      LoadStream.Read(Influence[I,K,0], SizeX * SizeOf(Influence[0,0,0]));

  for I := 0 to PCount - 1 do
    for K := 0 to SizeY - 1 do
      LoadStream.Read(Ownership[I,K,0], SizeX * SizeOf(Ownership[0,0,0]));

  for K := 0 to SizeY - 1 do
    LoadStream.Read(AvoidBuilding[K,0], SizeX * SizeOf(AvoidBuilding[0,0]));

  for K := 0 to SizeY - 1 do
    LoadStream.Read(Forest[K,0], SizeX * SizeOf(Forest[0,0]));
end;


procedure TKMInfluences.UpdateState(aTick: Cardinal);
begin
  if aTick mod 50 = 15 then
  begin
    fUpdatePlayerId := (fUpdatePlayerId + 1) mod fPlayers.Count;

    UpdateDirectInfluence;
    UpdateOwnershipInfluence;
  end;
end;


//Render debug symbols
procedure TKMInfluences.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  Col: Cardinal;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  if OVERLAY_INFLUENCES then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := $80000000;
      J := GetBestOwner(K,I);
      if J <> PLAYER_NONE then
        Col := (fPlayers[J].FlagColor and $FFFFFF) or (Byte(Max(Ownership[J,I,K],0)) shl 24);
      fRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_AVOID then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := AvoidBuilding[I,K] * 65793 or $80000000;
      fRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_FOREST then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := Min(Forest[I,K] * 6, 255) * 65793 or $80000000;
      fRenderAux.Quad(K, I, Col);
    end;
end;


{ TKMAIFields }
constructor TKMAIFields.Create;
begin
  inherited;

  fNavMesh := TKMNavMesh.Create;
  fInfluences := TKMInfluences.Create;
end;


destructor TKMAIFields.Destroy;
begin
  FreeAndNil(fNavMesh);
  FreeAndNil(fInfluences);
  inherited;
end;


procedure TKMAIFields.AfterMissionInit;
begin
  if AI_GEN_INFLUENCE_MAPS then
    fInfluences.Init;

  if AI_GEN_NAVMESH then
    fNavMesh.Init;
end;


procedure TKMAIFields.Save(SaveStream: TKMemoryStream);
begin
  fNavMesh.Save(SaveStream);
  fInfluences.Save(SaveStream);
end;


procedure TKMAIFields.Load(LoadStream: TKMemoryStream);
begin
  fNavMesh.Load(LoadStream);
  fInfluences.Load(LoadStream);
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  fInfluences.UpdateState(aTick);
  fNavMesh.UpdateState(aTick);
end;


//Render debug symbols
procedure TKMAIFields.Paint(aRect: TKMRect);
begin
  if AI_GEN_INFLUENCE_MAPS then
    fInfluences.Paint(aRect);

  if AI_GEN_NAVMESH then
    fNavMesh.Paint(aRect);
end;


end.
