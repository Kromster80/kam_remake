unit KM_GUIMapEdTerrainTiles;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;


type
  TKMMapEdTerrainTiles = class
  private
    fLastTile: Byte;

    procedure TilesChange(Sender: TObject);
    procedure TilesSet(aIndex: Integer);
    procedure TilesRefresh(Sender: TObject);
  protected
    Panel_Tiles: TKMPanel;
    TilesTable: array [0 .. MAPED_TILES_X * MAPED_TILES_Y - 1] of TKMButtonFlat; //how many are visible?
    TilesScroll: TKMScrollBar;
    TilesRandom: TKMCheckBox;
    TilesMagicWater, TilesEyedropper: TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    procedure UpdateState;
    function Visible: Boolean;
  end;


implementation
uses
  KM_ResFonts, KM_ResTexts, KM_GameCursor, KM_RenderUI, KM_InterfaceGame;


const
  //Tiles table made by JBSnorro, thanks to him :)
  MapEdTileRemap: array [1..256] of Integer = (
     1,73,74,75,37,21,22, 38, 33, 34, 32,181,173,177,129,130,131,132,133, 49,193,197,217,225,  0,  0, 45, 24, 13, 23,208,224,
    27,76,77,78,36,39,40,198,100,101,102,189,169,185,134,135,136,137,138,124,125,126,229,218,219,220, 46, 11,  5,  0, 26,216,
    28,79,80,81,35,88,89, 90, 70, 71, 72,182,174,178,196,139,140,141,142,127,128,  0,230,226,227,228, 47,204,205,206,203,207,
    29,82,83,84,85,86,87,  0,112,113,114,190,170,186,161,162,163,164,165,106,107,108,233,234,231,  0, 48,221,213,214,199,200,
    30,94,95,96,57,58,59,  0,103,104,105,183,175,179,157,202,158,159,160,117,118,119,209,210,241,245,194,248, 65, 66,195, 25,
    31, 9,19,20,41,42,43, 44,  6,  7, 10,191,171,187,149,150,151,152, 16,242,243,244,235,238,239,240,  0, 50,172, 52,222,223,
    18,67,68,69,91,92,93,  0,  3,  4,  2,184,176,180,145,146,147,148,  8,115,116,120,236,237,143,144,  0, 53,167, 55,215,232,
    17,97,98,99, 0, 0, 0,  0, 12, 14, 15,192,168,188,153,154,155,156,  0,121,122,123,211,212,201,  0,246,166, 51, 54,  0,  0);
    // 247 - doesn't work in game, replaced with random road


constructor TKMMapEdTerrainTiles.Create(aParent: TKMPanel);
var
  J,K: Integer;
begin
  inherited Create;

  Panel_Tiles := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Tiles, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_HINTS_TILES], fnt_Outline, taCenter);

  TilesMagicWater := TKMButtonFlat.Create(Panel_Tiles, 2, 24, TB_WIDTH - 4, 20, 0);
  TilesMagicWater.Caption := gResTexts[TX_MAPED_TERRAIN_MAGIC_WATER];
  TilesMagicWater.CapOffsetY := -12;
  TilesMagicWater.Hint := gResTexts[TX_MAPED_TERRAIN_MAGIC_WATER_HINT];
  TilesMagicWater.OnClick := TilesChange;

  TilesEyedropper := TKMButtonFlat.Create(Panel_Tiles, 2, 50, TB_WIDTH - 4, 20, 0);
  TilesEyedropper.Caption := gResTexts[TX_MAPED_TERRAIN_EYEDROPPER];
  TilesEyedropper.CapOffsetY := -12;
  TilesEyedropper.Hint := gResTexts[TX_MAPED_TERRAIN_EYEDROPPER_HINT];
  TilesEyedropper.OnClick := TilesChange;

  TilesRandom := TKMCheckBox.Create(Panel_Tiles, 0, 86, TB_WIDTH, 20, gResTexts[TX_MAPED_TERRAIN_TILES_RANDOM], fnt_Metal);
  TilesRandom.Checked := True;
  TilesRandom.OnClick := TilesChange;
  TilesRandom.Hint := gResTexts[TX_MAPED_TERRAIN_TILES_RANDOM_HINT];

  //Create scroll first to link to its MouseWheel event
  TilesScroll := TKMScrollBar.Create(Panel_Tiles, 2, 106 + 4 + MAPED_TILES_Y * 32, 194, 20, sa_Horizontal, bsGame);
  TilesScroll.MaxValue := 256 div MAPED_TILES_Y - MAPED_TILES_X; // 32 - 6
  TilesScroll.Position := 0;
  TilesScroll.OnChange := TilesRefresh;
  for J := 0 to MAPED_TILES_Y - 1 do
  for K := 0 to MAPED_TILES_X - 1 do
  begin
    TilesTable[J * MAPED_TILES_X + K] := TKMButtonFlat.Create(Panel_Tiles, K * 32, 106 + J * 32, 32, 32, 1, rxTiles);
    TilesTable[J * MAPED_TILES_X + K].Tag :=  J * MAPED_TILES_X + K; //Store ID
    TilesTable[J * MAPED_TILES_X + K].OnClick := TilesChange;
    TilesTable[J * MAPED_TILES_X + K].OnMouseWheel := TilesScroll.MouseWheel;
  end;
end;


procedure TKMMapEdTerrainTiles.TilesChange(Sender: TObject);
begin
  TilesMagicWater.Down := (Sender = TilesMagicWater);
  TilesEyedropper.Down := (Sender = TilesEyedropper);

  if Sender = TilesMagicWater then
    gGameCursor.Mode := cmMagicWater;

  if Sender = TilesEyedropper then
    gGameCursor.Mode := cmEyedropper;

  if Sender = TilesRandom then
    gGameCursor.MapEdDir := 4 * Byte(TilesRandom.Checked); //Defined=0..3 or Random=4

  if (Sender is TKMButtonFlat)
  and not (Sender = TilesMagicWater)
  and not (Sender = TilesEyedropper) then
    TilesSet(TKMButtonFlat(Sender).TexID);

  TilesRefresh(nil);
end;


procedure TKMMapEdTerrainTiles.TilesSet(aIndex: Integer);
begin
  TilesMagicWater.Down := False;
  TilesEyedropper.Down := False;
  if aIndex <> 0 then
  begin
    gGameCursor.Mode := cmTiles;
    gGameCursor.Tag1 := aIndex - 1; //MapEdTileRemap is 1 based, tag is 0 based
    if TilesRandom.Checked then
      gGameCursor.MapEdDir := 4;

    //Remember last selected Tile
    fLastTile := aIndex;
  end;

  TilesRefresh(nil);
end;


procedure TKMMapEdTerrainTiles.TilesRefresh;
  function GetTileIDFromTag(aTag: Byte): Byte;
  var X,Y,Tile: Byte;
  begin
    X := aTag mod MAPED_TILES_X + TilesScroll.Position;
    Y := (aTag div MAPED_TILES_X);
    Tile := (256 div MAPED_TILES_Y) * Y + X;
    Result := MapEdTileRemap[Tile + 1];
  end;
var
  I,K,L: Integer;
  TileID: Integer;
begin
  TilesRandom.Checked := (gGameCursor.MapEdDir = 4);

  for I := 0 to MAPED_TILES_Y - 1 do
  for K := 0 to MAPED_TILES_X - 1 do
  begin
    L := I * MAPED_TILES_X + K;
    TileID := GetTileIDFromTag(L);
    TilesTable[L].TexID := TileID;
    //Don't disable it because then scrollwheel doesn't work
    TilesTable[L].HideHighlight := TileID = 0;
    TilesTable[L].Clickable := TileID <> 0;
    if TileID = 0 then
      TilesTable[L].Hint := ''
    else
      //Show 0..N-1 to be consistent with objects and script commands like States.MapTileObject
      TilesTable[L].Hint := IntToStr(TileID - 1);
    //If cursor has a tile then make sure its properly selected in table as well
    TilesTable[L].Down := (gGameCursor.Mode = cmTiles) and (gGameCursor.Tag1 = TileID - 1);
    TilesEyedropper.Down := gGameCursor.Mode = cmEyedropper;
  end;
end;


procedure TKMMapEdTerrainTiles.Show;
begin
  TilesSet(fLastTile);
  gGameCursor.MapEdDir := 0;
  Panel_Tiles.Show;
end;


function TKMMapEdTerrainTiles.Visible: Boolean;
begin
  Result := Panel_Tiles.Visible;
end;


procedure TKMMapEdTerrainTiles.Hide;
begin
  Panel_Tiles.Hide;
end;


procedure TKMMapEdTerrainTiles.UpdateState;
begin
  TilesRandom.Checked := (gGameCursor.MapEdDir = 4);
  TilesRefresh(nil);
end;


end.
