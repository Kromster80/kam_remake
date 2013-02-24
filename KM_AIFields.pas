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
    fUpdatePlayerId: TPlayerIndex; //Player we will be updating next
    //This is cache for CanOwn in fTerrain.Land
    Ownable: array of array of Boolean;

    //Stored in 1D arrays for optimisation, accessed through property
    fInfluence: array of Byte; //Each players area of influence
    fOwnership: array of Byte;

    function GetInfluence(aPlayer: Byte; Y,X: Word): Byte;
    procedure SetInfluence(aPlayer: Byte; Y,X: Word; aInfluence: Byte);
    function GetOwnership(aPlayer: Byte; Y,X: Word): Byte;
    procedure SetOwnership(aPlayer: Byte; Y,X: Word; aOwnership: Byte);

    property Influence[aPlayer:Byte; Y,X: Word]: Byte read GetInfluence write SetInfluence;

    procedure InitInfluenceAvoid;
    procedure InitInfluenceOwnable;
    procedure UpdateDirectInfluence(aIndex: TPlayerIndex);
    procedure UpdateOwnershipInfluence(aIndex: TPlayerIndex);
  public
    //Common map of areas where building is undesired (around Store, Mines, Woodcutters)
    AvoidBuilding: array of array of Byte;

    //Tiles best owner and his influence
    property Ownership[aPlayer:Byte; Y,X: Word]: Byte read GetOwnership write SetOwnership;
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
  INFLUENCE_DECAY_D = 7;
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
  Result := PLAYER_NONE;
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  Best := 0;
  for I := 0 to fPlayers.Count - 1 do
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
  fMapX := fTerrain.MapX;
  fMapY := fTerrain.MapY;
  SetLength(AvoidBuilding, fMapY, fMapX);
  SetLength(Ownable, fMapY, fMapX);
  SetLength(fInfluence, fPlayers.Count*fMapY*fMapX);
  SetLength(fOwnership, fPlayers.Count*fMapY*fMapX);
  InitInfluenceAvoid;
  InitInfluenceOwnable;

  if AI_GEN_INFLUENCE_MAPS then
  for I := 0 to fPlayers.Count - 1 do
  begin
    UpdateDirectInfluence(I);
    UpdateOwnershipInfluence(I);
  end;
end;


function TKMInfluences.GetInfluence(aPlayer: Byte; Y,X: Word): Byte;
begin
  Result := fInfluence[aPlayer*fMapX*fMapY + Y*fMapX +X];
end;


procedure TKMInfluences.SetInfluence(aPlayer: Byte; Y,X: Word; aInfluence: Byte);
begin
  fInfluence[aPlayer*fMapX*fMapY + Y*fMapX +X] := aInfluence;
end;


function TKMInfluences.GetOwnership(aPlayer: Byte; Y,X: Word): Byte;
begin
  Result := fOwnership[aPlayer*fMapX*fMapY + Y*fMapX +X];
end;


procedure TKMInfluences.SetOwnership(aPlayer: Byte; Y,X: Word; aOwnership: Byte);
begin
  fOwnership[aPlayer*fMapX*fMapY + Y*fMapX +X] := aOwnership;
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


procedure TKMInfluences.InitInfluenceOwnable;
var
  I, K: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;

  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
    Ownable[I,K] := (CanOwn in fTerrain.Land[I,K].Passability);
end;


procedure TKMInfluences.UpdateDirectInfluence(aIndex: TPlayerIndex);
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
  Assert(fTerrain <> nil);

  //Clear
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
    Influence[aIndex, I, K] := 0;

  //Sync tile ownership
  PlayerHouses := fPlayers[aIndex].Houses;
  for I := 0 to PlayerHouses.Count - 1 do
  if not PlayerHouses[I].IsDestroyed and (PlayerHouses[I].HouseType <> ht_WatchTower) then
  begin
    P := PlayerHouses[I].GetPosition;
    //Expand influence with faloff
    DoFill(P.X, P.Y, 255);
  end;
end;


procedure TKMInfluences.UpdateOwnershipInfluence(aIndex: TPlayerIndex);
var
  I, K, H: Integer;
  T: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(fTerrain <> nil);

  //Fill Ownership map
  //Ownerhip = influence of player - influences of his enemies
  for I := 1 to fMapY - 1 do
  for K := 1 to fMapX - 1 do
  begin
    T := Influence[aIndex, I, K];
    if T <> 0 then
    begin
      for H := 0 to fPlayers.Count - 1 do
      if (H <> aIndex) and (fPlayers[aIndex].Alliances[H] = at_Enemy) then
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
  PCount: Word;
  K: Integer;
begin
  PCount := fPlayers.Count;

  SaveStream.Write('Influences');

  SaveStream.Write(PCount);
  SaveStream.Write(fMapY);
  SaveStream.Write(fMapX);
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
  I,K: Integer;
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
    fUpdatePlayerId := (fUpdatePlayerId + 1) mod fPlayers.Count;

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
        Col := (fPlayers[J].FlagColor and $FFFFFF) or (Influence[J,I,K] shl 24);
      fRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_OWNERSHIP then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := $80000000;
      J := GetBestOwner(K,I);
      if J <> PLAYER_NONE then
        Col := (fPlayers[J].FlagColor and $FFFFFF)
                or (Ownership[J,I,K] shl 24)
                or ((Byte(InRange(Ownership[J,I,K], OWN_THRESHOLD, OWN_MARGIN)) * 255) shl 24);
      fRenderAux.Quad(K, I, Col);
    end;

  if OVERLAY_AVOID then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := AvoidBuilding[I,K] * 65793 or $80000000;
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
