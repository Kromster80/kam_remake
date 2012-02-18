unit KM_MapView;
{$I KaM_Remake.inc}
interface
uses Classes, dglOpenGL, KromUtils, KromOGLUtils, Math, SysUtils,
  KM_MissionScript, KM_Render, KM_Terrain, KM_Points, KM_Utils;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMapView = class
  private
    fRender: TRender; //Should be used to Gen and Update texture
    fFromParser: Boolean;
    fIsMapEditor: Boolean;
    fSepia: Boolean;
    fParser: TMissionParserPreview;
    fMyTerrain: TTerrain;
    fMapY: Word;
    fMapX: Word;
    fBase: TCardinalArray; //Base terrain layer
    fMapTex: TTexture;
    procedure UpdateMinimapFromGame;
    procedure UpdateMinimapFromParser(aRevealAll:Boolean);
    procedure SepiaFilter;
  public
    constructor Create(aRender: TRender; aTerrain: TTerrain; aIsMapEditor:Boolean; aSepia:Boolean);
    destructor Destroy; override;

    procedure LoadTerrain(aMissionPath: string);
    procedure Clear;

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property MapTex: TTexture read fMapTex;
    function GetPlayerLoc(aIndex:byte):TKMPoint;
    procedure Update(aRevealAll:Boolean);
  end;

  //todo: Add Starting positions (Position, PlayerID, FlagColor, Alliances?)
  //todo: For tactic missions add (Armies, FlagColors)


implementation
uses KM_TGATexture, KM_Defaults, KM_Resource, KM_PlayersCollection, KM_Units, KM_Units_Warrior;


{ TKMMinimap }
constructor TKMMapView.Create(aRender: TRender; aTerrain: TTerrain; aIsMapEditor:Boolean; aSepia:Boolean);
begin
  inherited Create;

  fIsMapEditor := aIsMapEditor;
  fRender := aRender;
  fSepia := aSepia;
  fMapTex.Tex := GenerateTextureCommon;

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
end;


procedure TKMMapView.Clear;
begin
  LoadTerrain('');
end;


procedure TKMMapView.UpdateMinimapFromParser(aRevealAll:Boolean);
var i,k:Integer; Light:SmallInt; x0,y2:Word;
begin
  fMapX := fParser.MapX-1;
  fMapY := fParser.MapY-1;
  SetLength(fBase, fMapX * fMapY);

  for i:=1 to fMapY do
  for k:=1 to fMapX do
    with fParser.MapPreview[k,i] do
    begin
      if not aRevealAll and not Revealed then
        fBase[(i-1)*fMapX + (k-1)] := $FF000000
      else
        if TileOwner <> 0 then
          fBase[(i-1)*fMapX + (k-1)] := fParser.PlayerPreview[TileOwner].Color
        else
        begin
          //Forumlua for lighting same as in TTerrain.RebuildLighting
          x0:=EnsureRange(k-1,1,fMapX);
          y2:=EnsureRange(i+1,1,fMapY);
          Light := Round(EnsureRange((TileHeight - (fParser.MapPreview[k,y2].TileHeight + fParser.MapPreview[x0,i].TileHeight)/2)/22, -1, 1)*64);
          fBase[(i-1)*fMapX + (k-1)] := EnsureRange(fResource.Tileset.TileColor[TileID].R+Light,0,255) +
                                        EnsureRange(fResource.Tileset.TileColor[TileID].G+Light,0,255) shl 8 +
                                        EnsureRange(fResource.Tileset.TileColor[TileID].B+Light,0,255) shl 16;
        end;
    end;
end;


//Sepia method taken from: http://www.techrepublic.com/blog/howdoi/how-do-i-convert-images-to-grayscale-and-sepia-tone-using-c/120
procedure TKMMapView.SepiaFilter;
const SEPIA_VAL = 0.4;
var i:integer; R,G,B,R2,G2,B2:byte;
begin
  for i:=0 to fMapX*fMapY - 1 do
  begin
    R :=  fBase[i] AND $000000FF;
    G := (fBase[i] AND $0000FF00) shr 8;
    B := (fBase[i] AND $00FF0000) shr 16;

    R2 := Min(Round(0.393*R + 0.769*G + 0.189*B),255);
    R2 := Round(SEPIA_VAL*R2 + (1-SEPIA_VAL)*R);

    G2 := Min(Round(0.349*R + 0.686*G + 0.168*B),255);
    G2 := Round(SEPIA_VAL*G2 + (1-SEPIA_VAL)*G);

    B2 := Min(Round(0.272*R + 0.534*G + 0.131*B),255);
    B2 := Round(SEPIA_VAL*B2 + (1-SEPIA_VAL)*B);

    fBase[i] := (R2 + (G2 shl 8) + (B2 shl 16)) AND $FFFFFFFF;
  end;
end;


function TKMMapView.GetPlayerLoc(aIndex:byte):TKMPoint;
begin
  Assert(fFromParser); //Should only be used in parser mode
  Result := fParser.PlayerPreview[aIndex].StartingLoc;
end;


//MapEditor stores only commanders instead of all groups members
procedure TKMMapView.UpdateMinimapFromGame;
var
  FOW,ID:byte;
  i,j,k:integer;
  U: TKMUnit;
  W: TKMUnitWarrior;
  P: TKMPoint;
  DoesFit: Boolean;
  Light:smallint;
begin
  fMapX := fMyTerrain.MapX-1;
  fMapY := fMyTerrain.MapY-1;
  SetLength(fBase, fMapX * fMapY);

  for i:=0 to fMapY-1 do
  for k:=0 to fMapX-1 do
  begin
    if MyPlayer <> nil then
      FOW := MyPlayer.FogOfWar.CheckTileRevelation(k+1,i+1,true)
    else
      FOW := 255;
    if FOW = 0 then
      fBase[i*fMapX + k] := 0
    else
      if fMyTerrain.Land[i+1,k+1].TileOwner <> -1 then
        fBase[i*fMapX + k] := fPlayers.Player[fMyTerrain.Land[i+1,k+1].TileOwner].FlagColor
      else
      begin
        U := fMyTerrain.Land[i+1,k+1].IsUnit;
        if U <> nil then
          if U.GetOwner <> PLAYER_ANIMAL then
            fBase[i*fMapX + k] := fPlayers.Player[U.GetOwner].FlagColor
          else
            fBase[i*fMapX + k] := fResource.UnitDat[U.UnitType].MinimapColor
        else
        begin
          ID := fMyTerrain.Land[i+1,k+1].Terrain;
          Light := Round(fMyTerrain.Land[i+1,k+1].Light*64)-(255-FOW); //it's -255..255 range now
          fBase[i*fMapX + k] := EnsureRange(fResource.Tileset.TileColor[ID].R+Light,0,255) +
                                EnsureRange(fResource.Tileset.TileColor[ID].G+Light,0,255) shl 8 +
                                EnsureRange(fResource.Tileset.TileColor[ID].B+Light,0,255) shl 16;
        end;
      end;
  end;

  //Scan all players units and paint all virtual group members
  if fIsMapEditor then
    for i:=0 to fPlayers.Count-1 do
      for k:=0 to fPlayers[i].Units.Count-1 do
        if fPlayers[i].Units[k] is TKMUnitWarrior then
        begin
          W := TKMUnitWarrior(fPlayers[i].Units[k]);
          for j:=1 to W.fMapEdMembersCount do
          begin
            P := GetPositionInGroup2(W.GetPosition.X, W.GetPosition.Y, W.Direction, j+1, W.UnitsPerRow, fMapX, fMapY, DoesFit);
            if not DoesFit then Continue; //Don't render units that are off the map in the map editor
            fBase[P.Y * fMapX + P.X] := fPlayers[i].FlagColor;
          end;
        end;
end;


procedure TKMMapView.Update(aRevealAll:Boolean);
var
  wData: Pointer;
  I: Word;
  WidthPOT, HeightPOT: Word;
begin
  if fFromParser then
    UpdateMinimapFromParser(aRevealAll)
  else
    UpdateMinimapFromGame;

  if fSepia then SepiaFilter;

  WidthPOT := MakePOT(fMapX);
  HeightPOT := MakePOT(fMapY);

  GetMem(wData, WidthPOT * HeightPOT * 4);

  for I := 0 to fMapY - 1 do
    Move(Pointer(Cardinal(fBase) + I * fMapX * 4)^,
         Pointer(Cardinal(wData) + I * WidthPOT * 4)^, fMapX * 4);

  glBindTexture(GL_TEXTURE_2D, fMapTex.Tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, WidthPOT, HeightPOT, 0, GL_RGBA, GL_UNSIGNED_BYTE, wData);

  fMapTex.U := fMapX / WidthPOT;
  fMapTex.V := fMapY / HeightPOT;

  FreeMem(wData);

  glBindTexture(GL_TEXTURE_2D, 0);
end;


end.
