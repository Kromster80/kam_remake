unit KM_MapView;
{$I KaM_Remake.inc}
interface
uses Classes, dglOpenGL, KromUtils, KromOGLUtils, KM_Render, Math, SysUtils,
  KM_Terrain, KM_Points, KM_Utils;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMapView = class
  private
    fRender: TRender;
    fMyTerrain: TTerrain;
    fMapY: Word;
    fMapX: Word;
    fPixels: TCardinalArray;
    fMapTex: TTexture;
    procedure UpdateMinimap(aMapEditor: Boolean);
  public
    constructor Create(aRender: TRender; aTerrain: TTerrain = nil);

    property Terrain: TTerrain read fMyTerrain;

    property MapTex: TTexture read fMapTex;
    procedure Update(aMapEditor: Boolean);
  end;


implementation
uses KM_Defaults, KM_Resource, KM_PlayersCollection, KM_Units_Warrior;


{ TKMMinimap }
constructor TKMMapView.Create(aRender: TRender; aTerrain: TTerrain = nil);
begin
  inherited Create;

  //Create our own local terrain to access when in menu
  if aTerrain = nil then
    fMyTerrain := TTerrain.Create
  else //Otherwise access sunced Game terrain
    fMyTerrain := aTerrain;

  //Create texture handle
  glGenTextures(1, @fMapTex.Tex);
  glBindTexture(GL_TEXTURE_2D, fMapTex.Tex);

  {Enable color blending into texture}
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  {Keep original KaM grainy look}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

  {Clamping UVs solves edge artifacts}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  glBindTexture(GL_TEXTURE_2D, 0);

  fRender := aRender;
end;


//MapEditor stores only commanders instead of all groups members
procedure TKMMapView.UpdateMinimap(aMapEditor: Boolean);
var
  FOW,ID:byte;
  i,j,k:integer;
  W: TKMUnitWarrior;
  P: TKMPoint;
  DoesFit: Boolean;
  Light:smallint;
begin
  for i:=0 to fMapY-1 do
  for k:=0 to fMapX-1 do
  begin
    if MyPlayer <> nil then
      FOW := MyPlayer.FogOfWar.CheckTileRevelation(k+1,i+1,true)
    else
      FOW := 255;
    if FOW = 0 then
      fPixels[i*fMapX + k] := 0
    else
      if fMyTerrain.Land[i+1,k+1].TileOwner <> -1 then
        fPixels[i*fMapX + k] := fPlayers.Player[fMyTerrain.Land[i+1,k+1].TileOwner].FlagColor
      else
        if fMyTerrain.Land[i+1,k+1].IsUnit <> nil then
          if fMyTerrain.Land[i+1,k+1].IsUnit.GetOwner <> PLAYER_ANIMAL then
            fPixels[i*fMapX + k] := fPlayers.Player[fMyTerrain.Land[i+1,k+1].IsUnit.GetOwner].FlagColor
          else
            fPixels[i*fMapX + k] := fResource.UnitDat[fMyTerrain.Land[i+1,k+1].IsUnit.UnitType].MinimapColor
        else
        begin
          ID := fMyTerrain.Land[i+1,k+1].Terrain;
          Light := round(fMyTerrain.Land[i+1,k+1].Light*64)-(255-FOW); //it's -255..255 range now
          fPixels[i*fMapX + k] := EnsureRange(fResource.Tileset.TileColor[ID].R+Light,0,255) +
                                  EnsureRange(fResource.Tileset.TileColor[ID].G+Light,0,255) shl 8 +
                                  EnsureRange(fResource.Tileset.TileColor[ID].B+Light,0,255) shl 16;
        end;
  end;

  //Scan all players units and paint all virtual group members
  if aMapEditor then
    for i:=0 to fPlayers.Count-1 do
      for k:=0 to fPlayers[i].Units.Count-1 do
        if fPlayers[i].Units[k] is TKMUnitWarrior then
        begin
          W := TKMUnitWarrior(fPlayers[i].Units[k]);
          for j:=1 to W.fMapEdMembersCount do
          begin
            P := GetPositionInGroup2(W.GetPosition.X, W.GetPosition.Y, W.Direction, j+1, W.UnitsPerRow, fMapX, fMapY, DoesFit);
            if not DoesFit then Continue; //Don't render units that are off the map in the map editor
            fPixels[P.Y * fMapX + P.X] := fPlayers[i].FlagColor;
          end;
        end;
end;


procedure TKMMapView.Update(aMapEditor: Boolean);
var
  wData: Pointer;
  I: Integer;
  WidthPOT, HeightPOT: Word;
begin
  fMapX := fMyTerrain.MapX;
  fMapY := fMyTerrain.MapY;
  SetLength(fPixels, fMapX * fMapY);

  UpdateMinimap(aMapEditor);

  WidthPOT := MakePOT(fMapX);
  HeightPOT := MakePOT(fMapY);

  GetMem(wData, WidthPOT * HeightPOT * 4);

  for I := 0 to fMapY - 1 do
    Move(Pointer(Cardinal(fPixels) + I * fMapX * 4)^,
         Pointer(Cardinal(wData) + I * WidthPOT * 4)^, fMapX * 4);

  glBindTexture(GL_TEXTURE_2D, fMapTex.Tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, WidthPOT, HeightPOT, 0, GL_RGBA, GL_UNSIGNED_BYTE, wData);

  fMapTex.U := fMapX / WidthPOT;
  fMapTex.V := fMapY / HeightPOT;

  FreeMem(wData);

  glBindTexture(GL_TEXTURE_2D, 0);
end;


end.
