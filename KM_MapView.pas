unit KM_MapView;
{$I KaM_Remake.inc}
interface
uses Classes, dglOpenGL, KromUtils, KromOGLUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Utils,
  KM_MissionScript, KM_Render, KM_Terrain;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMapView = class
  private
    fFromParser: Boolean;
    fIsMapEditor: Boolean;
    fSepia: Boolean;
    fUseCustomColors: Boolean;
    fParser: TMissionParserPreview;
    fMyTerrain: TTerrain;
    fMapY: Word;
    fMapX: Word;
    fBase: TCardinalArray; //Base terrain layer
    fMapTex: TTexture;
    fWidthPOT: Word;
    fHeightPOT: Word;
    procedure UpdateMinimapFromGame;
    procedure UpdateMinimapFromParser(aRevealAll:Boolean);
    procedure SepiaFilter;
    procedure UpdateTexture;
  public
    PlayerColors:array[1..MAX_PLAYERS] of Cardinal;
    constructor Create(aTerrain: TTerrain; aIsMapEditor: Boolean; aSepia: Boolean);
    destructor Destroy; override;

    procedure LoadTerrain(aMissionPath: string);

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property MapTex: TTexture read fMapTex;
    property UseCustomColors: Boolean read fUseCustomColors write fUseCustomColors;
    function GetPlayerLoc(aIndex: Byte): TKMPoint;
    procedure Update(aRevealAll: Boolean);
    procedure UpdateMapSize(aX, aY: Word);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_PlayersCollection, KM_Resource, KM_Units, KM_Units_Warrior;


{ TKMMinimap }
constructor TKMMapView.Create(aTerrain: TTerrain; aIsMapEditor: Boolean; aSepia: Boolean);
begin
  inherited Create;

  fIsMapEditor := aIsMapEditor;
  fSepia := aSepia;
  fMapTex.Tex := TRender.GenerateTextureCommon;

  //We don't need terrain on main menu, just a parser
  //Otherwise access synced Game terrain
  fFromParser := (aTerrain = nil);

  if fFromParser then
  begin
    fMyTerrain := nil;
    fParser := TMissionParserPreview.Create(False);
  end
  else
  begin
    fMyTerrain := aTerrain;
    fParser := nil;
  end;
end;


destructor TKMMapView.Destroy;
begin
  if fFromParser then fParser.Free;
  inherited;
end;


//Load map in a direct way, should be used only when in Menu
procedure TKMMapView.LoadTerrain(aMissionPath: string);
begin
  fParser.LoadMission(aMissionPath);

  fMapX := fParser.MapX - 1;
  fMapY := fParser.MapY - 1;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;
end;


procedure TKMMapView.UpdateMapSize(aX, aY: Word);
begin
  fMapX := aX - 1;
  fMapY := aY - 1;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;
end;


procedure TKMMapView.UpdateMinimapFromParser(aRevealAll: Boolean);
var
  I, K, N: Integer;
  Light: SmallInt;
  x0,y2: Word;
begin
  for I := 1 to fMapY do
  for K := 1 to fMapX do
    with fParser.MapPreview[K,I] do
    begin
      N := (I-1)*fMapX + (K-1);
      if not aRevealAll and not Revealed then
        fBase[N] := $E0000000
      else
        if TileOwner <> 0 then
        begin
          if fUseCustomColors then
            fBase[N] := PlayerColors[TileOwner]
          else
            fBase[N] := fParser.PlayerPreview[TileOwner].Color;
        end
        else
        begin
          //Formula for lighting is the same as in TTerrain.RebuildLighting
          x0 := Max(K-1, 1);
          y2 := Min(I+1, fMapY);
          Light := Round(EnsureRange((TileHeight - (fParser.MapPreview[K,y2].TileHeight + fParser.MapPreview[x0,I].TileHeight)/2)/22, -1, 1)*64);
          fBase[N] := EnsureRange(fResource.Tileset.TileColor[TileID].R+Light, 0, 255) +
                      EnsureRange(fResource.Tileset.TileColor[TileID].G+Light, 0, 255) shl 8 +
                      EnsureRange(fResource.Tileset.TileColor[TileID].B+Light, 0, 255) shl 16 or $FF000000;
        end;
    end;
end;


//Sepia method taken from:
//http://www.techrepublic.com/blog/howdoi/how-do-i-convert-images-to-grayscale-and-sepia-tone-using-c/120
procedure TKMMapView.SepiaFilter;
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


function TKMMapView.GetPlayerLoc(aIndex:byte): TKMPoint;
begin
  Assert(fFromParser); //Should only be used in parser mode
  Result := fParser.PlayerPreview[aIndex].StartingLoc;
end;


//MapEditor stores only commanders instead of all groups members
procedure TKMMapView.UpdateMinimapFromGame;
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
          fBase[I*fMapX + K] := EnsureRange(fResource.Tileset.TileColor[ID].R+Light,0,255) +
                                EnsureRange(fResource.Tileset.TileColor[ID].G+Light,0,255) shl 8 +
                                EnsureRange(fResource.Tileset.TileColor[ID].B+Light,0,255) shl 16 or $FF000000;
        end;
      end;
  end;

  //Scan all players units and paint all virtual group members
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


procedure TKMMapView.Update(aRevealAll: Boolean);
begin
  if SKIP_RENDER then Exit;

  if fFromParser then
    UpdateMinimapFromParser(aRevealAll)
  else
    UpdateMinimapFromGame;

  if fSepia then SepiaFilter;

  UpdateTexture;
end;


procedure TKMMapView.UpdateTexture;
var
  wData: Pointer;
  I: Word;
begin
  GetMem(wData, fWidthPOT * fHeightPOT * 4);

  for I := 0 to fMapY - 1 do
    Move(Pointer(Cardinal(fBase) + I * fMapX * 4)^,
         Pointer(Cardinal(wData) + I * fWidthPOT * 4)^, fMapX * 4);

  TRender.UpdateTexture(fMapTex.Tex, fWidthPOT, fHeightPOT, tf_RGBA8, wData);
  FreeMem(wData);
end;


procedure TKMMapView.Save(SaveStream: TKMemoryStream);
var L: Cardinal;
begin
  SaveStream.Write('Minimap');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
end;


procedure TKMMapView.Load(LoadStream:TKMemoryStream);
var L: Cardinal;
begin
  LoadStream.ReadAssert('Minimap');

  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(L);
  SetLength(fBase, L);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;

  if L > 0 then
    LoadStream.Read(fBase[0], L * SizeOf(Cardinal));

  if fSepia then SepiaFilter;
  UpdateTexture;
end;


end.
