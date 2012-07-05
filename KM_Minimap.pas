unit KM_Minimap;
{$I KaM_Remake.inc}
interface
uses Classes, dglOpenGL, KromUtils, KromOGLUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Utils,
  KM_MissionScript, KM_Render, KM_Terrain, KM_Alerts;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMinimap = class
  private
    fIsMapEditor: Boolean; //Paint missing army memmbers
    fSepia: Boolean; //Less saturated display for menu
    fParser: TMissionParserPreview;
    fMyTerrain: TTerrain;
    fAlerts: TKMAlerts;

    //We need to store map properties locally since Minimaps come from various
    //sources which do not have Terrain in them (TMissionParserPreview, Stream)
    fMapY: Word;
    fMapX: Word;
    fBase: TCardinalArray; //Base terrain layer
    fMapTex: TTexture;
    fWidthPOT: Word;
    fHeightPOT: Word;
    procedure ApplySepia;
    procedure UpdateMinimapFromGame;
    procedure UpdateMinimapFromParser(aRevealAll:Boolean);
    procedure UpdateTexture;
  public
    PlayerColors: array [1..MAX_PLAYERS] of Cardinal;
    PlayerLocations: array [1..MAX_PLAYERS] of TKMPoint;
    constructor Create(aFromParser: Boolean; aIsMapEditor: Boolean; aSepia: Boolean);
    destructor Destroy; override;

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property MapTex: TTexture read fMapTex;

    function AlertsCount: Integer;
    function Alert(aIndex: Integer): TKMAlert;
    function ShowAlerts: Boolean;

    procedure LoadFromMission(aMissionPath: string);
    procedure LoadFromTerrain(aAlerts: TKMAlerts);
    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);

    procedure Update(aRevealAll: Boolean);
  end;


implementation
uses KM_PlayersCollection, KM_Resource, KM_Units, KM_Units_Warrior;


{ TKMMinimap }
constructor TKMMinimap.Create(aFromParser: Boolean; aIsMapEditor: Boolean; aSepia: Boolean);
begin
  inherited Create;

  fIsMapEditor := aIsMapEditor;
  fSepia := aSepia;
  fMapTex.Tex := TRender.GenerateTextureCommon;

  //We don't need terrain on main menu, just a parser
  //Otherwise access synced Game terrain
  if aFromParser then
    fParser := TMissionParserPreview.Create(False);
end;


destructor TKMMinimap.Destroy;
begin
  FreeAndNil(fParser);
  inherited;
end;


//Load map in a direct way, should be used only when in Menu
procedure TKMMinimap.LoadFromMission(aMissionPath: string);
var I: Integer;
begin
  fParser.LoadMission(aMissionPath);

  fMapX := fParser.MapX - 1;
  fMapY := fParser.MapY - 1;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;

  for I := 1 to MAX_PLAYERS do
  begin
    PlayerColors[I] := fParser.PlayerPreview[I].Color;
    PlayerLocations[I] := fParser.PlayerPreview[I].StartingLoc;
  end;
end;


procedure TKMMinimap.LoadFromTerrain(aAlerts: TKMAlerts);
var I: Integer;
begin
  fMyTerrain := fTerrain;
  fMapX := fMyTerrain.MapX - 1;
  fMapY := fMyTerrain.MapY - 1;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;

  for I := 1 to MAX_PLAYERS do
  begin
    PlayerColors[I] := $00000000;
    PlayerLocations[I] := KMPoint(0,0);
  end;

  fAlerts := aAlerts;
end;


procedure TKMMinimap.UpdateMinimapFromParser(aRevealAll: Boolean);
var
  I, K, N: Integer;
  Light: SmallInt;
  x0,y2: Word;
begin
  for I := 1 to fMapY do
  for K := 1 to fMapX do
    with fParser.MapPreview[K,I] do
    begin
      N := (I-1) * fMapX + (K-1);
      if not aRevealAll and not Revealed then
        fBase[N] := $E0000000
      else
        if TileOwner <> 0 then
          fBase[N] := PlayerColors[TileOwner]
        else
        begin
          //Formula for lighting is the same as in TTerrain.RebuildLighting
          x0 := Max(K-1, 1);
          y2 := Min(I+1, fMapY);
          Light := Round(EnsureRange((TileHeight - (fParser.MapPreview[K,y2].TileHeight + fParser.MapPreview[x0,I].TileHeight)/2)/22, -1, 1)*64);
          fBase[N] := Byte(EnsureRange(fResource.Tileset.TileColor[TileID].R+Light, 0, 255)) +
                      Byte(EnsureRange(fResource.Tileset.TileColor[TileID].G+Light, 0, 255)) shl 8 +
                      Byte(EnsureRange(fResource.Tileset.TileColor[TileID].B+Light, 0, 255)) shl 16 or $FF000000;
        end;
    end;
end;


function TKMMinimap.Alert(aIndex: Integer): TKMAlert;
begin
  if (fPlayers.CheckAlliance(fAlerts[aIndex].Owner, MyPlayer.PlayerIndex) = at_Ally) then
    Result := fAlerts[aIndex]
  else
    Result := nil;
end;


function TKMMinimap.AlertsCount: Integer;
begin
  Result := fAlerts.Count;
end;


//Sepia method taken from:
//http://www.techrepublic.com/blog/howdoi/how-do-i-convert-images-to-grayscale-and-sepia-tone-using-c/120
procedure TKMMinimap.ApplySepia;
const SEPIA_VAL = 0.4;
var
  I: Integer;
  R, G, B, R2, G2, B2: Byte;
begin
  for I := 0 to fMapX * fMapY - 1 do
  begin
    //We split color to RGB values
    R := fBase[I] and $FF;
    G := fBase[I] shr 8 and $FF;
    B := fBase[I] shr 16 and $FF;

    //Apply sepia coefficients and merge back with SEPIA_VAL factor
    R2 := Min(Round(0.393 * R + 0.769 * G + 0.189 * B), 255);
    R2 := Mix(R2, R, SEPIA_VAL);

    G2 := Min(Round(0.349 * R + 0.686 * G + 0.168 * B), 255);
    G2 := Mix(G2, G, SEPIA_VAL);

    B2 := Min(Round(0.272 * R + 0.534 * G + 0.131 * B), 255);
    B2 := Mix(B2, B, SEPIA_VAL);

    fBase[I] := (R2 + G2 shl 8 + B2 shl 16) or $FF000000;
  end;
end;


//MapEditor stores only commanders instead of all groups members
procedure TKMMinimap.UpdateMinimapFromGame;
var
  FOW,ID: Byte;
  I,J,K: Integer;
  U: TKMUnit;
  W: TKMUnitWarrior;
  P: TKMPoint;
  DoesFit: Boolean;
  Light: Smallint;
begin
  for I := 0 to fMapY - 1 do
  for K := 0 to fMapX - 1 do
  begin
    if MyPlayer <> nil then
      FOW := MyPlayer.FogOfWar.CheckTileRevelation(K+1,I+1,true)
    else
      FOW := 255;
    if FOW = 0 then
      fBase[I*fMapX + K] := $E0000000
    else
      if fMyTerrain.Land[I+1,K+1].TileOwner <> -1 then
        fBase[I*fMapX + K] := fPlayers.Player[fMyTerrain.Land[I+1,K+1].TileOwner].FlagColor
      else
      begin
        U := fMyTerrain.Land[I+1,K+1].IsUnit;
        if U <> nil then
          if U.GetOwner <> PLAYER_ANIMAL then
            fBase[I*fMapX + K] := fPlayers.Player[U.GetOwner].FlagColor
          else
            fBase[I*fMapX + K] := fResource.UnitDat[U.UnitType].MinimapColor
        else
        begin
          ID := fMyTerrain.Land[I+1,K+1].Terrain;
          Light := Round(fMyTerrain.Land[I+1,K+1].Light*64)-(255-FOW); //it's -255..255 range now
          fBase[I*fMapX + K] := Byte(EnsureRange(fResource.Tileset.TileColor[ID].R+Light,0,255)) +
                                Byte(EnsureRange(fResource.Tileset.TileColor[ID].G+Light,0,255)) shl 8 +
                                Byte(EnsureRange(fResource.Tileset.TileColor[ID].B+Light,0,255)) shl 16 or $FF000000;
        end;
      end;
  end;

  //Scan all players units and paint all virtual group members in MapEd
  if fIsMapEditor then
    for I:=0 to fPlayers.Count-1 do
      for K:=0 to fPlayers[I].Units.Count-1 do
        if fPlayers[I].Units[K] is TKMUnitWarrior then
        begin
          W := TKMUnitWarrior(fPlayers[I].Units[K]);
          for J:=1 to W.fMapEdMembersCount do
          begin
            P := GetPositionInGroup2(W.GetPosition.X, W.GetPosition.Y, W.Direction, J+1, W.UnitsPerRow, fMapX, fMapY, DoesFit);
            if not DoesFit then Continue; //Don't render units that are off the map in the map editor
            fBase[P.Y * fMapX + P.X] := fPlayers[I].FlagColor;
          end;
        end;
end;


procedure TKMMinimap.Update(aRevealAll: Boolean);
begin
  if SKIP_RENDER then Exit;

  if fParser <> nil then
    UpdateMinimapFromParser(aRevealAll)
  else
    UpdateMinimapFromGame;

  if fSepia then ApplySepia;

  UpdateTexture;
end;


procedure TKMMinimap.UpdateTexture;
var
  wData: Pointer;
  I: Word;
begin
  GetMem(wData, fWidthPOT * fHeightPOT * 4);

  if fMapY > 0 then //if MapY = 0 then loop will overflow to MaxWord
  for I := 0 to fMapY - 1 do
    Move(Pointer(Cardinal(fBase) + I * fMapX * 4)^,
         Pointer(Cardinal(wData) + I * fWidthPOT * 4)^, fMapX * 4);

  TRender.UpdateTexture(fMapTex.Tex, fWidthPOT, fHeightPOT, tf_RGBA8, wData);
  FreeMem(wData);
end;


procedure TKMMinimap.SaveToStream(SaveStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  SaveStream.Write('Minimap');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
  for I := 1 to MAX_PLAYERS do
  begin
    SaveStream.Write(PlayerColors[I]);
    SaveStream.Write(PlayerLocations[I]);
  end;
end;


function TKMMinimap.ShowAlerts: Boolean;
begin
  Result := fAlerts <> nil;
end;


procedure TKMMinimap.LoadFromStream(LoadStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  LoadStream.ReadAssert('Minimap');

  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(L);
  SetLength(fBase, L);
  if L > 0 then
    LoadStream.Read(fBase[0], L * SizeOf(Cardinal));
  for I := 1 to MAX_PLAYERS do
  begin
    LoadStream.Read(PlayerColors[I]);
    LoadStream.Read(PlayerLocations[I]);
  end;

  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;

  if fMapX * fMapY = 0 then Exit;

  if fSepia then ApplySepia;
  UpdateTexture;
end;


end.
