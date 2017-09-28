unit KM_Minimap;
{$I KaM_Remake.inc}
interface
uses
  KromOGLUtils,
  KM_Terrain, KM_Alerts,
  KM_MissionScript_Preview,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMinimap = class
  private
    fPaintVirtualGroups: Boolean; //Paint missing army memmbers
    fSepia: Boolean; //Less saturated display for menu
    fParser: TMissionParserPreview;
    fMyTerrain: TKMTerrain;
    fAlerts: TKMAlerts;

    //We need to store map properties locally since Minimaps come from various
    //sources which do not have Terrain in them (TMissionParserPreview, Stream)
    fMapY: Word;
    fMapX: Word;
    fBase: TKMCardinalArray; //Base terrain layer
    fMapTex: TTexture;
    fWidthPOT: Word;
    fHeightPOT: Word;
    procedure ApplySepia;
    procedure Resize(aX, aY: Word);
    procedure UpdateMinimapFromGame;
    procedure UpdateMinimapFromParser(aRevealAll:Boolean);
    procedure UpdateTexture;
  public
    HandColors: array [0..MAX_HANDS-1] of Cardinal;
    HandLocs: array [0..MAX_HANDS-1] of TKMPoint;
    HandShow: array [0..MAX_HANDS-1] of Boolean;
    HandTeam: array [0..MAX_HANDS-1] of ShortInt;
    constructor Create(aFromParser: Boolean; aSepia: Boolean);
    destructor Destroy; override;

    property Alerts: TKMAlerts read fAlerts write fAlerts;
    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property MapTex: TTexture read fMapTex;
    property PaintVirtualGroups: Boolean read fPaintVirtualGroups write fPaintVirtualGroups;

    procedure LoadFromMission(const aMissionPath: string; const aRevealFor: array of TKMHandIndex);
    procedure LoadFromTerrain;
    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);

    procedure Update(aRevealAll: Boolean);
  end;


implementation
uses
  SysUtils, KromUtils, Math,
  KM_Game, KM_Render, KM_AIFields, KM_AIInfluences,
  KM_Units, KM_UnitGroups, KM_Hand, KM_HandsCollection,
  KM_Resource, KM_ResUnits, KM_CommonUtils, KM_Utils;


{ TKMMinimap }
constructor TKMMinimap.Create(aFromParser: Boolean; aSepia: Boolean);
begin
  inherited Create;

  fSepia := aSepia;
  fMapTex.Tex := TRender.GenerateTextureCommon;

  //We don't need terrain on main menu, just a parser
  //Otherwise access synced Game terrain
  if aFromParser then
    fParser := TMissionParserPreview.Create;
end;


destructor TKMMinimap.Destroy;
begin
  FreeAndNil(fParser);
  inherited;
end;


//Load map in a direct way, should be used only when in Menu
procedure TKMMinimap.LoadFromMission(const aMissionPath: string; const aRevealFor: array of TKMHandIndex);
var
  I: Integer;
begin
  fParser.LoadMission(aMissionPath, aRevealFor);

  Resize(fParser.MapX - 1, fParser.MapY - 1);

  for I := 0 to MAX_HANDS - 1 do
  begin
    HandColors[I] := fParser.PlayerPreview[I].Color;
    HandLocs[I] := fParser.PlayerPreview[I].StartingLoc;
    HandShow[I] := fParser.PlayerPreview[I].CanHuman;
  end;
end;


procedure TKMMinimap.LoadFromTerrain;
var
  I: Integer;
begin
  fMyTerrain := gTerrain;

  Resize(fMyTerrain.MapX - 1, fMyTerrain.MapY - 1);

  for I := 0 to MAX_HANDS - 1 do
  begin
    HandColors[I] := $00000000;
    HandLocs[I] := KMPOINT_ZERO;
    HandShow[I] := False;
  end;
end;


procedure TKMMinimap.Resize(aX, aY: Word);
begin
  fMapX := aX;
  fMapY := aY;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;
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
        if TileOwner <> PLAYER_NONE then
          fBase[N] := HandColors[TileOwner]
        else
        begin
          //Formula for lighting is the same as in TTerrain.RebuildLighting
          x0 := Max(K-1, 1);
          y2 := Min(I+1, fMapY);
          Light := Round(EnsureRange((TileHeight - (fParser.MapPreview[K,y2].TileHeight + fParser.MapPreview[x0,I].TileHeight)/2)/22, -1, 1)*64);
          fBase[N] := Byte(EnsureRange(gRes.Tileset.TileColor[TileID].R+Light, 0, 255)) +
                      Byte(EnsureRange(gRes.Tileset.TileColor[TileID].G+Light, 0, 255)) shl 8 +
                      Byte(EnsureRange(gRes.Tileset.TileColor[TileID].B+Light, 0, 255)) shl 16 or $FF000000;
        end;
    end;
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
  P: TKMPoint;
  DoesFit: Boolean;
  Light: Smallint;
  Owner: TKMHandIndex;
  Group: TKMUnitGroup;
begin
  if OVERLAY_OWNERSHIP then
  begin
    for I := 0 to fMapY - 1 do
      for K := 0 to fMapX - 1 do
      begin
        Owner := gAIFields.Influences.GetBestOwner(K,I);
        if Owner <> PLAYER_NONE then
          fBase[I*fMapX + K] := ReduceBrightness(gHands[Owner].FlagColor, Byte(Max(gAIFields.Influences.Ownership[Owner,I,K],0)))
        else
          fBase[I*fMapX + K] := $FF000000;
      end;
    Exit;
  end;

  for I := 0 to fMapY - 1 do
    for K := 0 to fMapX - 1 do
    begin
      FOW := gMySpectator.FogOfWar.CheckTileRevelation(K+1,I+1);

      if FOW = 0 then
        fBase[I*fMapX + K] := $FF000000
      else
        if (fMyTerrain.Land[I+1,K+1].TileOwner <> -1)
          and not fMyTerrain.TileIsCornField(KMPoint(K+1, I+1)) //Do not show corn and wine on minimap
          and not fMyTerrain.TileIsWineField(KMPoint(K+1, I+1)) then
          fBase[I*fMapX + K] := gHands[fMyTerrain.Land[I+1,K+1].TileOwner].FlagColor
        else
        begin
          U := fMyTerrain.Land[I+1,K+1].IsUnit;
          if U <> nil then
            if U.Owner <> PLAYER_ANIMAL then
              fBase[I*fMapX + K] := gHands[U.Owner].FlagColor
            else
              fBase[I*fMapX + K] := gRes.Units[U.UnitType].MinimapColor
          else
          begin
            ID := fMyTerrain.Land[I+1,K+1].Terrain;
            // Do not use fMyTerrain.Land[].Light for borders of the map, because it is set to -1 for fading effect
            // So assume fMyTerrain.Land[].Light as 0 in this case
            if (I = 0) or (I = fMapY - 1) or (K = 0) or (K = fMapX - 1) then
              Light := 255-FOW
            else
              Light := Round(fMyTerrain.Land[I+1,K+1].Light*64)-(255-FOW); //it's -255..255 range now
            fBase[I*fMapX + K] := Byte(EnsureRange(gRes.Tileset.TileColor[ID].R+Light,0,255)) +
                                  Byte(EnsureRange(gRes.Tileset.TileColor[ID].G+Light,0,255)) shl 8 +
                                  Byte(EnsureRange(gRes.Tileset.TileColor[ID].B+Light,0,255)) shl 16 or $FF000000;
          end;
        end;
    end;

  //Scan all players units and paint all virtual group members in MapEd
  if fPaintVirtualGroups then
    for I := 0 to gHands.Count - 1 do
      for K := 0 to gHands[I].UnitGroups.Count - 1 do
      begin
        Group := gHands[I].UnitGroups[K];
        for J := 1 to Group.MapEdCount - 1 do
        begin
          //GetPositionInGroup2 operates with 1..N terrain, while Minimap uses 0..N-1, hence the +1 -1 fixes
          P := GetPositionInGroup2(Group.Position.X, Group.Position.Y, Group.Direction, J, Group.UnitsPerRow, fMapX+1, fMapY+1, DoesFit);
          if not DoesFit then Continue; //Don't render units that are off the map in the map editor
          fBase[(P.Y - 1) * fMapX + P.X - 1] := gHands[I].FlagColor;
        end;
      end;

  //Draw
  if (gGame <> nil) and (gGame.GameMode = gmMapEd)
    and (mlMapResize in gGame.MapEditor.VisibleLayers)
    and not KMSameRect(gGame.MapEditor.ResizeMapRect, KMRECT_ZERO) then
    for I := 0 to fMapY - 1 do
      for K := 0 to fMapX - 1 do
      begin
        if not KMInRect(KMPoint(K+1,I+1), gGame.MapEditor.ResizeMapRect) then
          fBase[I*fMapX + K] := ApplyColorCoef(fBase[I*fMapX + K], 2, 1, 1);
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
    Move(Pointer(NativeUint(fBase) + I * fMapX * 4)^,
         Pointer(NativeUint(wData) + I * fWidthPOT * 4)^, fMapX * 4);

  TRender.UpdateTexture(fMapTex.Tex, fWidthPOT, fHeightPOT, tf_RGBA8, wData);
  FreeMem(wData);
end;


procedure TKMMinimap.SaveToStream(SaveStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  SaveStream.WriteA('Minimap');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
  for I := 0 to MAX_HANDS - 1 do
  begin
    SaveStream.Write(HandColors[I]);
    SaveStream.Write(HandLocs[I]);
    SaveStream.Write(HandShow[I]);
  end;
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
  for I := 0 to MAX_HANDS - 1 do
  begin
    LoadStream.Read(HandColors[I]);
    LoadStream.Read(HandLocs[I]);
    LoadStream.Read(HandShow[I]);
  end;

  //Resize will update UV bounds. Resizing fBase is ok since the size does not changes
  Resize(fMapX, fMapY);

  if fMapX * fMapY = 0 then Exit;

  if fSepia then ApplySepia;
  UpdateTexture;
end;


end.
