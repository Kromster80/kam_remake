unit KM_AIInfluences;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  //Collection of influence maps
  TKMInfluences = class
  private
    fMapX: Word;
    fMapY: Word;
    fUpdatePlayerId: TKMHandIndex; //Player we will be updating next
    //This is cache for CanOwn in gTerrain.Land
    Ownable: array of array of Boolean;

    //Stored in 1D arrays for optimisation, accessed through property
    fInfluence: array of Byte; //Each players area of influence
    fOwnership: array of Byte;

    function GetInfluence(aPlayer: Byte; Y,X: Word): Byte; inline;
    procedure SetInfluence(aPlayer: Byte; Y,X: Word; aInfluence: Byte); inline;
    function GetOwnership(aPlayer: Byte; Y,X: Word): Byte; inline;
    procedure SetOwnership(aPlayer: Byte; Y,X: Word; aOwnership: Byte); inline;

    property Influence[aPlayer: Byte; Y,X: Word]: Byte read GetInfluence write SetInfluence;

    procedure InitInfluenceAvoid;
    procedure InitInfluenceOwnable;
    procedure UpdateDirectInfluence(aIndex: TKMHandIndex);
    procedure UpdateOwnershipInfluence(aIndex: TKMHandIndex);
  public
    //Common map of areas where building is undesired (around Store, Mines, Woodcutters)
    AvoidBuilding: array of array of Byte;

    //Tiles best owner and his influence
    property Ownership[aPlayer:Byte; Y,X: Word]: Byte read GetOwnership write SetOwnership;
    //Tension: array of array of array of SmallInt;

    procedure AddAvoidBuilding(X,Y: Word; aRad: Single);
    procedure RemAvoidBuilding(aArea: TKMRect);
    function GetBestOwner(X, Y: Word): TKMHandIndex;
    procedure Init;
    procedure ExportInfluenceMaps;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


implementation
uses
  Classes, Graphics, SysUtils, Math,
  KM_RenderAux,
  KM_Terrain, KM_Houses, KM_HouseCollection,
  KM_Hand, KM_HandsCollection, KM_ResHouses;


const
  //Decay affects how much space AI needs to be able to expand
  //Values smaller than 3 start to block green AI on AcrossDesert too fast
  INFLUENCE_DECAY = 5;
  INFLUENCE_DECAY_D = 7;
  INFLUENCE_ENEMY_DIV = 2;


{ TKMInfluenceMaps }
//Make the area around to be avoided by common houses
procedure TKMInfluences.AddAvoidBuilding(X,Y: Word; aRad: Single);
var I,K: Integer;
begin
  for I := Max(Y - Ceil(aRad), 1) to Min(Y + Ceil(aRad), fMapY - 1) do
  for K := Max(X - Ceil(aRad), 1) to Min(X + Ceil(aRad), fMapX - 1) do
    if Sqr(X-K) + Sqr(Y-I) <= Sqr(aRAD) then
      AvoidBuilding[I,K] := 255;
end;


procedure TKMInfluences.RemAvoidBuilding(aArea: TKMRect);
var I,K: Integer;
begin
  for I := Max(aArea.Top , 1) to Min(aArea.Bottom, fMapY - 1) do
  for K := Max(aArea.Left, 1) to Min(aArea.Right , fMapX - 1) do
    AvoidBuilding[I,K] := 0;
end;


function TKMInfluences.GetInfluence(aPlayer: Byte; Y,X: Word): Byte;
begin
  Result := fInfluence[aPlayer*fMapX*fMapY + Y*fMapX + X];
end;


procedure TKMInfluences.SetInfluence(aPlayer: Byte; Y,X: Word; aInfluence: Byte);
begin
  fInfluence[aPlayer*fMapX*fMapY + Y*fMapX + X] := aInfluence;
end;


function TKMInfluences.GetOwnership(aPlayer: Byte; Y,X: Word): Byte;
begin
  Result := fOwnership[aPlayer*fMapX*fMapY + Y*fMapX + X];
end;


procedure TKMInfluences.SetOwnership(aPlayer: Byte; Y,X: Word; aOwnership: Byte);
begin
  fOwnership[aPlayer*fMapX*fMapY + Y*fMapX + X] := aOwnership;
end;


//Return index of player who has most influence on this tile, or none
function TKMInfluences.GetBestOwner(X, Y: Word): TKMHandIndex;
var
  I: Integer;
  Best: Integer;
begin
  Result := PLAYER_NONE;
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  Best := 0;
  for I := 0 to gHands.Count - 1 do
  if Ownership[I,Y,X] > Best then
  begin
    Best := Ownership[I,Y,X];
    Result := I;
  end;
end;


procedure TKMInfluences.Init;
var
  I: Integer;
begin
  fMapX := gTerrain.MapX;
  fMapY := gTerrain.MapY;
  SetLength(AvoidBuilding, fMapY, fMapX);
  SetLength(Ownable, fMapY, fMapX);
  SetLength(fInfluence, gHands.Count * fMapY * fMapX);
  SetLength(fOwnership, gHands.Count * fMapY * fMapX);
  InitInfluenceAvoid;
  InitInfluenceOwnable;

  if AI_GEN_INFLUENCE_MAPS then
  for I := 0 to gHands.Count - 1 do
  begin
    UpdateDirectInfluence(I);
    UpdateOwnershipInfluence(I);
  end;
end;


//AI should avoid certain areas, keeping them for special houses
procedure TKMInfluences.InitInfluenceAvoid;
var
  S: TKMHouse;
  I, K, J: Integer;
  M: Integer;
  N: Integer;
begin
  //Avoid areas where Gold/Iron mines should be
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  if (gTerrain.TileIsIron(K, I) > 1) or (gTerrain.TileIsGold(K, I) > 1) then
    for M := I to Min(I + 2, fMapY - 1) do
    for N := Max(K - 1, 1) to Min(K + 1, fMapX - 1) do
      AvoidBuilding[M, N] := 255;

  //Avoid Coal fields
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
   AvoidBuilding[I,K] := AvoidBuilding[I,K] or (Byte(gTerrain.TileIsCoal(K, I) > 1) * $FF);

  //Leave free space below all players Stores
  for J := 0 to gHands.Count - 1 do
  begin
    S := gHands[J].FindHouse(ht_Store);
    if S <> nil then
    for I := Max(S.Entrance.Y - 3, 1) to Min(S.Entrance.Y + 2, fMapY - 1) do
    for K := Max(S.Entrance.X - 2, 1) to Min(S.Entrance.X + 2, fMapX - 1) do
      AvoidBuilding[I,K] := AvoidBuilding[I,K] or $FF;
  end;
end;


procedure TKMInfluences.InitInfluenceOwnable;
var
  I, K: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
    Ownable[I,K] := (tpOwn in gTerrain.Land[I,K].Passability);
end;


procedure TKMInfluences.UpdateDirectInfluence(aIndex: TKMHandIndex);
  procedure DoFill(X,Y: Word; V: Byte);
  begin
    if  (V > Influence[aIndex, Y, X])
    and (Ownable[Y,X]) then
    begin
      Influence[aIndex, Y, X] := V;
      if V - INFLUENCE_DECAY <= 0 then Exit;

      if Y-1 >= 1       then DoFill(X, Y-1, V - INFLUENCE_DECAY);
      if X-1 >= 1       then DoFill(X-1, Y, V - INFLUENCE_DECAY);
      if X+1 <= fMapX-1 then DoFill(X+1, Y, V - INFLUENCE_DECAY);
      if Y+1 <= fMapY-1 then DoFill(X, Y+1, V - INFLUENCE_DECAY);

      //We can get away with 4-tap fill, it looks fine already
      //Diagonals passages are uncommon and don't matter that much for influence
      {if V - INFLUENCE_DECAY_D <= 0 then Exit;
      DoFill(X-1, Y-1, V - INFLUENCE_DECAY_D);
      DoFill(X+1, Y-1, V - INFLUENCE_DECAY_D);
      DoFill(X-1, Y+1, V - INFLUENCE_DECAY_D);
      DoFill(X+1, Y+1, V - INFLUENCE_DECAY_D);}
    end;
  end;
var
  I, K: Integer;
  P: TKMPoint;
  PlayerHouses: TKMHousesCollection;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(gTerrain <> nil);

  //Clear
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
    Influence[aIndex, I, K] := 0;

  //Sync tile ownership
  PlayerHouses := gHands[aIndex].Houses;
  for I := 0 to PlayerHouses.Count - 1 do
  if not PlayerHouses[I].IsDestroyed and (PlayerHouses[I].HouseType <> ht_WatchTower) then
  begin
    P := PlayerHouses[I].GetPosition;
    //Expand influence with faloff
    DoFill(P.X, P.Y, 255);
  end;
end;


procedure TKMInfluences.UpdateOwnershipInfluence(aIndex: TKMHandIndex);
var
  I, K, H: Integer;
  T: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(gTerrain <> nil);

  //Fill Ownership map
  //Ownerhip = influence of player - influences of his enemies
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  begin
    T := Influence[aIndex, I, K];
    if T <> 0 then
    begin
      for H := 0 to gHands.Count - 1 do
      if gHands[aIndex].Alliances[H] = at_Enemy then
        T := T - Influence[H, I, K] div INFLUENCE_ENEMY_DIV;

      Ownership[aIndex, I, K] := Max(T, 0);
    end
    else
      Ownership[aIndex, I, K] := 0;
  end;

  //Normalization is not working as intended,
  //it gives unfair advantage to one using it,
  //muting neighbours areas beyond reason
end;


procedure TKMInfluences.ExportInfluenceMaps;
var
  I, J, K: Integer;
begin
  for J := 0 to gHands.Count - 1 do
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

  for J := 0 to gHands.Count - 1 do
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
  PCount: Word;
  K: Integer;
begin
  PCount := gHands.Count;

  SaveStream.WriteA('Influences');

  SaveStream.Write(PCount);
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fUpdatePlayerId);

  SaveStream.Write(fInfluence[0], fMapX * fMapY * PCount * SizeOf(fInfluence[0]));
  SaveStream.Write(fOwnership[0], fMapX * fMapY * PCount * SizeOf(fOwnership[0]));

  for K := 0 to fMapY - 1 do
    SaveStream.Write(AvoidBuilding[K,0], fMapX * SizeOf(AvoidBuilding[0,0]));

  for K := 0 to fMapY - 1 do
    SaveStream.Write(Ownable[K,0], fMapX * SizeOf(Ownable[0,0]));
end;


procedure TKMInfluences.Load(LoadStream: TKMemoryStream);
var
  PCount: Word;
  K: Integer;
begin
  LoadStream.ReadAssert('Influences');

  LoadStream.Read(PCount);
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fUpdatePlayerId);

  SetLength(fInfluence, PCount * fMapY * fMapX);
  SetLength(fOwnership, PCount * fMapY * fMapX);
  SetLength(AvoidBuilding, fMapY, fMapX);
  SetLength(Ownable, fMapY, fMapX);

  LoadStream.Read(fInfluence[0], fMapX * fMapY * PCount * SizeOf(fInfluence[0]));
  LoadStream.Read(fOwnership[0], fMapX * fMapY * PCount * SizeOf(fOwnership[0]));

  for K := 0 to fMapY - 1 do
    LoadStream.Read(AvoidBuilding[K,0], fMapX * SizeOf(AvoidBuilding[0,0]));

  for K := 0 to fMapY - 1 do
    LoadStream.Read(Ownable[K,0], fMapX * SizeOf(Ownable[0,0]));
end;


procedure TKMInfluences.UpdateState(aTick: Cardinal);
begin
  //Update one player every 15 sec
  if aTick mod 150 = 15 then
  begin
    fUpdatePlayerId := (fUpdatePlayerId + 1) mod gHands.Count;

    UpdateDirectInfluence(fUpdatePlayerId);
    UpdateOwnershipInfluence(fUpdatePlayerId);
  end;
end;


//Render debug symbols
procedure TKMInfluences.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  Col: Cardinal;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  if OVERLAY_INFLUENCE then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := $80000000;
      J := GetBestOwner(K,I);
      if J <> PLAYER_NONE then
        Col := (gHands[J].FlagColor and $FFFFFF) or (Influence[J,I,K] shl 24);
      gRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_OWNERSHIP then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := $80000000;
      J := GetBestOwner(K,I);
      if J <> PLAYER_NONE then
        Col := (gHands[J].FlagColor and $FFFFFF)
                or (Ownership[J,I,K] shl 24)
                or ((Byte(InRange(Ownership[J,I,K], OWN_THRESHOLD, OWN_MARGIN)) * 255) shl 24);
      gRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_AVOID then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := AvoidBuilding[I,K] * 65793 or $80000000;
      gRenderAux.Quad(K, I, Col);
    end;
end;


end.