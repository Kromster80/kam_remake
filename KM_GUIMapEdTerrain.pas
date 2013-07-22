unit KM_GUIMapEdTerrain;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;

type
  TKMTerrainTab = (ttBrush, ttHeights, ttTile, ttObject, ttSelection);

  //Collection of terrain editing controls
  TKMMapEdTerrain = class
  private
    //Objects in MapElem are placed sparsely, so we need to compact them
    //to use in MapEd palette
    fTileDirection: Byte;
    fLastObject: Byte;
    fLastTile: Byte;
    fCountCompact: Integer;
    fCompactToMapElem: array [Byte] of Byte; //Pointers to valid MapElem's
    fMapElemToCompact: array [Byte] of Byte; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.

    procedure CompactMapElements;
    procedure BrushChange(Sender: TObject);
    procedure BrushRefresh;
    procedure HeightChange(Sender: TObject);
    procedure HeightRefresh;
    procedure TilesChange(Sender: TObject);
    procedure TilesSet(aIndex: Integer);
    procedure TilesRefresh(Sender: TObject);
    procedure ObjectsChange(Sender: TObject);
    procedure ObjectsRefresh(Sender: TObject);
    procedure SelectionClick(Sender: TObject);
    procedure UnRedoClick(Sender: TObject);
    procedure PageChange(Sender: TObject);
  protected
    Panel_Terrain: TKMPanel;
      Button_Terrain: array [TKMTerrainTab] of TKMButton;
      Button_TerrainUndo: TKMButton;
      Button_TerrainRedo: TKMButton;
      Panel_Brushes: TKMPanel;
        BrushSize: TKMTrackBar;
        BrushCircle,BrushSquare: TKMButtonFlat;
        BrushTable: array [0..6, 0..4] of TKMButtonFlat;
        BrushRandom: TKMCheckBox;
      Panel_Heights: TKMPanel;
        HeightSize, HeightSlope, HeightSpeed: TKMTrackBar;
        HeightShapeLabel: TKMLabel;
        HeightCircle,HeightSquare: TKMButtonFlat;
        HeightElevate, HeightUnequalize: TKMButtonFlat;
      Panel_Tiles: TKMPanel;
        TilesTable: array [0 .. MAPED_TILES_X * MAPED_TILES_Y - 1] of TKMButtonFlat; //how many are visible?
        TilesScroll: TKMScrollBar;
        TilesRandom: TKMCheckBox;
        TilesMagicWater: TKMButtonFlat;
      Panel_Objects: TKMPanel;
        ObjectErase: TKMButtonFlat;
        ObjectBlock: TKMButtonFlat;
        ObjectsTable: array [0..8] of TKMButtonFlat;
        ObjectsScroll: TKMScrollBar;
      Panel_Selection: TKMPanel;
        Button_SelectCopy: TKMButton;
        Button_SelectPaste: TKMButton;
        Button_SelectPasteApply: TKMButton;
        Button_SelectPasteCancel: TKMButton;
        Button_SelectFlipH, Button_SelectFlipV: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible(aPage: TKMTerrainTab): Boolean; overload;
    function Visible: Boolean; overload;
    procedure UpdateState;
  end;


implementation
uses
  KM_Resource, KM_ResFonts, KM_ResMapElements, KM_ResTexts,
  KM_Game, KM_GameCursor, KM_RenderUI,
  KM_TerrainPainter, KM_TerrainSelection,
  KM_InterfaceDefaults, KM_InterfaceMapEditor;


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


{Switch between pages}
procedure TKMMapEdTerrain.PageChange(Sender: TObject);
var I: Integer;
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Hide existing pages
  for I := 1 to Panel_Terrain.ChildCount do
  if Panel_Terrain.Childs[I] is TKMPanel then
    Panel_Terrain.Childs[I].Hide;

  if (Sender = Button_Terrain[ttBrush]) then
  begin
    BrushChange(BrushTable[0,0]);
    Panel_Brushes.Show;
  end else
  if (Sender = Button_Terrain[ttHeights]) then
  begin
    HeightChange(HeightCircle);
    HeightChange(HeightElevate);
    Panel_Heights.Show;
  end else
  if (Sender = Button_Terrain[ttTile]) then
  begin
    TilesSet(fLastTile);
    GameCursor.MapEdDir := 0;
    Panel_Tiles.Show;
  end else
  if (Sender = Button_Terrain[ttObject]) then
  begin
    case fLastObject of
      61:   ObjectsChange(ObjectBlock);
      255:  ObjectsChange(ObjectErase);
      else  ObjectsChange(ObjectsTable[fLastObject]);
    end;
    Panel_Objects.Show;
  end else
  if (Sender = Button_Terrain[ttSelection]) then
  begin
    Button_SelectPaste.Enabled := fGame.MapEditor.Selection.Selection_DataInBuffer;
    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
    Button_SelectFlipH.Disable;
    Button_SelectFlipV.Disable;
    SelectionClick(Button_SelectCopy);
    Panel_Selection.Show;
  end;
end;


procedure TKMMapEdTerrain.CompactMapElements;
var
  I: Integer;
begin
  fCountCompact := 0;
  for I := 0 to fResource.MapElements.Count - 1 do
  if (MapElem[I].Anim.Count > 0) and (MapElem[I].Anim.Step[1] > 0)
  and (MapElem[I].Stump = -1) and (I <> 61) then //Hide falling trees and invisible wall (61)
  begin
    fCompactToMapElem[fCountCompact] := I; //pointer
    fMapElemToCompact[I] := fCountCompact; //Reverse lookup
    Inc(fCountCompact);
  end;
end;


constructor TKMMapEdTerrain.Create(aParent: TKMPanel);
const
  BtnGlyph: array [TKMTerrainTab] of Word = (383, 388, 382, 385, 384);
  BtnHint: array [TKMTerrainTab] of Word = (
    TX_MAPED_TERRAIN_HINTS_BRUSHES,
    TX_MAPED_TERRAIN_HINTS_HEIGHTS,
    TX_MAPED_TERRAIN_HINTS_TILES,
    TX_MAPED_TERRAIN_HINTS_OBJECTS,
    TX_MAPED_COPY_TITLE);
  Surfaces: array [0 .. 6, 0 .. 4] of TTerrainKind = (
    (tkGrass,       tkMoss,         tkRustyGrass1,  tkRustyGrass2,  tkCustom),
    (tkDirtGrass,   tkDirt,         tkGravel,       tkCobbleStone,  tkCustom),
    (tkGrassSand2,  tkGrassSand1,   tkSand,         tkRichSand,     tkCustom),
    (tkSwamp,       tkGrassyWater,  tkWater,        tkFastWater,    tkCustom),
    (tkShallowSnow, tkSnow,         tkDeepSnow,     tkIce,          tkCustom),
    (tkStoneMount,  tkGoldMount,    tkIronMount,    tkAbyss,        tkCustom),
    (tkCoal,        tkGold,         tkIron,         tkLava,         tkCustom));
var
  I: TKMTerrainTab;
  J,K: Integer;
begin
  inherited Create;

  fTileDirection := 0;
  CompactMapElements;

  Panel_Terrain := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 28);
    for I := Low(TKMTerrainTab) to High(TKMTerrainTab) do
    begin
      Button_Terrain[I] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * Byte(I), 0, SMALL_TAB_W, SMALL_TAB_H, BtnGlyph[I], rxGui, bsGame);
      Button_Terrain[I].Hint := gResTexts[BtnHint[I]];
      Button_Terrain[I].OnClick := PageChange;
    end;

    Button_TerrainUndo := TKMButton.Create(Panel_Terrain, 180, 0, 20, 20, '<', bsGame);
    Button_TerrainUndo.OnClick := UnRedoClick;
    Button_TerrainRedo := TKMButton.Create(Panel_Terrain, 200, 0, 20, 20, '>', bsGame);
    Button_TerrainRedo.OnClick := UnRedoClick;

    Panel_Brushes := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Brushes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_BRUSH], fnt_Outline, taCenter);
      BrushSize   := TKMTrackBar.Create(Panel_Brushes, 0, 30, 100, 0, 12);
      BrushSize.OnChange := BrushChange;
      BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 106, 28, 24, 24, 592);
      BrushCircle.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      BrushCircle.OnClick := BrushChange;
      BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 134, 28, 24, 24, 593);
      BrushSquare.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
      BrushSquare.OnClick := BrushChange;

      for J := Low(Surfaces) to High(Surfaces) do
      for K := Low(Surfaces[J]) to High(Surfaces[J]) do
      if Surfaces[J,K] <> tkCustom then
      begin
        BrushTable[J,K] := TKMButtonFlat.Create(Panel_Brushes, K * 36, 60 + J * 40, 34, 34, Combo[Surfaces[J,K], Surfaces[J,K], 1] + 1, rxTiles);  // grass
        BrushTable[J,K].Tag := Byte(Surfaces[J,K]);
        BrushTable[J,K].OnClick := BrushChange;
      end;

      BrushRandom := TKMCheckBox.Create(Panel_Brushes, 0, 350, TB_WIDTH, 20, gResTexts[TX_MAPED_TERRAIN_BRUSH_RANDOM], fnt_Metal);
      BrushRandom.OnClick := BrushChange;

    Panel_Heights := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Heights, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_HEIGHTS], fnt_Outline, taCenter);
      HeightShapeLabel := TKMLabel.Create(Panel_Heights, 0, 34, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SHAPE], fnt_Metal, taLeft);
      HeightCircle := TKMButtonFlat.Create(Panel_Heights, 120, 30, 24, 24, 592);
      HeightCircle.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      HeightCircle.OnClick  := HeightChange;
      HeightSquare := TKMButtonFlat.Create(Panel_Heights, 150, 30, 24, 24, 593);
      HeightSquare.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
      HeightSquare.OnClick  := HeightChange;

      HeightSize          := TKMTrackBar.Create(Panel_Heights, 0, 60, TB_WIDTH, 1, 15); //1..15(4bit) for size
      HeightSize.Caption  := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SIZE];
      HeightSize.Hint     := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT];
      HeightSize.OnChange := HeightChange;
      HeightSlope           := TKMTrackBar.Create(Panel_Heights, 0, 115, TB_WIDTH, 1, 15); //1..15(4bit) for slope shape
      HeightSlope.Caption   := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SLOPE];
      HeightSlope.Hint      := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SLOPE_HINT];
      HeightSlope.OnChange  := HeightChange;
      HeightSpeed           := TKMTrackBar.Create(Panel_Heights, 0, 170, TB_WIDTH, 1, 15); //1..15(4bit) for speed
      HeightSpeed.Caption   := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SPEED];
      HeightSpeed.Hint      := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SPEED_HINT];
      HeightSpeed.OnChange  := HeightChange;

      HeightElevate             := TKMButtonFlat.Create(Panel_Heights, 0, 225, TB_WIDTH, 20, 0);
      HeightElevate.OnClick     := HeightChange;
      HeightElevate.Down        := True;
      HeightElevate.Caption     := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE];
      HeightElevate.CapOffsetY  := -12;
      HeightElevate.Hint        := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE_HINT];
      HeightUnequalize          := TKMButtonFlat.Create(Panel_Heights, 0, 255, TB_WIDTH, 20, 0);
      HeightUnequalize.OnClick  := HeightChange;
      HeightUnequalize.Caption  := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE];
      HeightUnequalize.CapOffsetY  := -12;
      HeightUnequalize.Hint      := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE_HINT];

    Panel_Tiles := TKMPanel.Create(Panel_Terrain, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Tiles, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_HINTS_TILES], fnt_Outline, taCenter);

      TilesMagicWater := TKMButtonFlat.Create(Panel_Tiles, 2, 24, TB_WIDTH - 4, 20, 0);
      TilesMagicWater.Caption := gResTexts[TX_MAPED_TERRAIN_MAGIC_WATER];
      TilesMagicWater.CapOffsetY := -12;
      TilesMagicWater.Hint := gResTexts[TX_MAPED_TERRAIN_MAGIC_WATER_HINT];
      TilesMagicWater.OnClick := TilesChange;

      TilesRandom := TKMCheckBox.Create(Panel_Tiles, 0, 60, TB_WIDTH, 20, gResTexts[TX_MAPED_TERRAIN_TILES_RANDOM], fnt_Metal);
      TilesRandom.Checked := True;
      TilesRandom.OnClick := TilesChange;
      TilesRandom.Hint := gResTexts[TX_MAPED_TERRAIN_TILES_RANDOM_HINT];

      //Create scroll first to link to its MouseWheel event
      TilesScroll := TKMScrollBar.Create(Panel_Tiles, 2, 80 + 4 + MAPED_TILES_Y * 32, 194, 20, sa_Horizontal, bsGame);
      TilesScroll.MaxValue := 256 div MAPED_TILES_Y - MAPED_TILES_X; // 32 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := TilesRefresh;
      for J := 0 to MAPED_TILES_Y - 1 do
      for K := 0 to MAPED_TILES_X - 1 do
      begin
        TilesTable[J * MAPED_TILES_X + K] := TKMButtonFlat.Create(Panel_Tiles, K * 32, 80 + J * 32, 32, 32, 1, rxTiles);
        TilesTable[J * MAPED_TILES_X + K].Tag :=  J * MAPED_TILES_X + K; //Store ID
        TilesTable[J * MAPED_TILES_X + K].OnClick := TilesChange;
        TilesTable[J * MAPED_TILES_X + K].OnMouseWheel := TilesScroll.MouseWheel;
      end;

    Panel_Objects := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Objects, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_OBJECTS], fnt_Outline, taCenter);
      ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 0, 295, TB_WIDTH, 20, sa_Horizontal, bsGame);
      ObjectsScroll.MinValue := 0;
      ObjectsScroll.MaxValue := fCountCompact div 3 - 3;
      ObjectsScroll.Position := 0;
      ObjectsScroll.OnChange := ObjectsRefresh;
      for J := 0 to 2 do for K := 0 to 2 do
      begin
        ObjectsTable[J*3+K] := TKMButtonFlat.Create(Panel_Objects, J*65, 40+K*85,64,84,1,rxTrees); //RXid=1  // 1 2
        ObjectsTable[J*3+K].Tag := J*3+K; //Store ID
        ObjectsTable[J*3+K].OnClick := ObjectsChange;
        ObjectsTable[J*3+K].OnMouseWheel := ObjectsScroll.MouseWheel;
      end;
      ObjectErase := TKMButtonFlat.Create(Panel_Objects, 0, 8,32,32,340);
      ObjectErase.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_REMOVE];
      ObjectErase.Tag := 255; //no object
      ObjectErase.OnClick := ObjectsChange;

      ObjectBlock := TKMButtonFlat.Create(Panel_Objects, TB_WIDTH-32, 8,32,32,254,rxTrees);
      ObjectBlock.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_BLOCK];
      ObjectBlock.Tag := 61; //no object
      ObjectBlock.OnClick := ObjectsChange;

    Panel_Selection := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Selection, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_COPY_TITLE], fnt_Outline, taCenter);
        Button_SelectCopy := TKMButton.Create(Panel_Selection, 20, 30, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_COPY], bsGame);
        Button_SelectCopy.Hint := gResTexts[TX_MAPED_COPY_COPY_HINT];
        Button_SelectCopy.OnClick := SelectionClick;
        Button_SelectPaste := TKMButton.Create(Panel_Selection, 20, 60, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_PASTE], bsGame);
        Button_SelectPaste.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
        Button_SelectPaste.OnClick := SelectionClick;
        Button_SelectPasteApply := TKMButton.Create(Panel_Selection, 20, 90, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_PASTE_APPLY], bsGame);
        Button_SelectPasteApply.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
        Button_SelectPasteApply.OnClick := SelectionClick;
        Button_SelectPasteCancel := TKMButton.Create(Panel_Selection, 20, 120, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_PASTE_CANCEL], bsGame);
        Button_SelectPasteCancel.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
        Button_SelectPasteCancel.OnClick := SelectionClick;
        Button_SelectFlipH := TKMButton.Create(Panel_Selection, 20, 150, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_COPY_PASTE_HFLIP], bsGame);
        Button_SelectFlipH.Hint := gResTexts[TX_MAPED_COPY_PASTE_HFLIP_HINT];
        Button_SelectFlipH.OnClick := SelectionClick;
        Button_SelectFlipV := TKMButton.Create(Panel_Selection, 20, 180, TB_WIDTH - 40, 20, gResTexts[TX_MAPED_COPY_PASTE_VFLIP], bsGame);
        Button_SelectFlipV.Hint := gResTexts[TX_MAPED_COPY_PASTE_VFLIP_HINT];
        Button_SelectFlipV.OnClick := SelectionClick;
        with TKMLabel.Create(Panel_Selection, 8, 230, TB_WIDTH-16, 80, gResTexts[TX_MAPED_COPY_SELECT_HINT], fnt_Grey, taLeft) do
          AutoWrap := True;
end;


procedure TKMMapEdTerrain.BrushChange(Sender: TObject);
begin
  GameCursor.Mode := cmBrush;
  GameCursor.MapEdSize := BrushSize.Position;
  fGame.MapEditor.TerrainPainter.RandomizeTiling := BrushRandom.Checked;

  if Sender = BrushCircle then
    GameCursor.MapEdShape := hsCircle
  else
  if Sender = BrushSquare then
    GameCursor.MapEdShape := hsSquare
  else
  if Sender is TKMButtonFlat then
    GameCursor.Tag1 := TKMButtonFlat(Sender).Tag;

  BrushRefresh;
end;


procedure TKMMapEdTerrain.BrushRefresh;
var
  I,K: Integer;
begin
  BrushCircle.Down := (GameCursor.MapEdShape = hsCircle);
  BrushSquare.Down := (GameCursor.MapEdShape = hsSquare);

  for I := Low(BrushTable) to High(BrushTable) do
  for K := Low(BrushTable[I]) to High(BrushTable[I]) do
  if BrushTable[I,K] <> nil then
    BrushTable[I,K].Down := (BrushTable[I,K].Tag = GameCursor.Tag1);
end;


procedure TKMMapEdTerrain.HeightChange(Sender: TObject);
begin
  GameCursor.MapEdSize := HeightSize.Position;
  GameCursor.MapEdSlope := HeightSlope.Position;
  GameCursor.MapEdSpeed := HeightSpeed.Position;

  //Shape
  if Sender = HeightCircle then
    GameCursor.MapEdShape := hsCircle
  else
  if Sender = HeightSquare then
    GameCursor.MapEdShape := hsSquare;

  //Kind
  if Sender = HeightElevate then
    GameCursor.Mode := cmElevate
  else
  if Sender = HeightUnequalize then
    GameCursor.Mode := cmEqualize;

  HeightRefresh;
end;


procedure TKMMapEdTerrain.HeightRefresh;
begin
  HeightCircle.Down := (GameCursor.MapEdShape = hsCircle);
  HeightSquare.Down := (GameCursor.MapEdShape = hsSquare);

  HeightElevate.Down := (GameCursor.Mode = cmElevate);
  HeightUnequalize.Down := (GameCursor.Mode = cmEqualize);
end;


procedure TKMMapEdTerrain.TilesChange(Sender: TObject);
begin
  TilesMagicWater.Down := (Sender = TilesMagicWater);
  if Sender = TilesMagicWater then
    GameCursor.Mode := cmMagicWater;

  if Sender = TilesRandom then
    GameCursor.MapEdDir := 4 * Byte(TilesRandom.Checked); //Defined=0..3 or Random=4

  if (Sender is TKMButtonFlat) and not (Sender = TilesMagicWater) then
    TilesSet(TKMButtonFlat(Sender).TexID);

  TilesRefresh(nil);
end;


procedure TKMMapEdTerrain.TilesSet(aIndex: Integer);
begin
  TilesMagicWater.Down := False;
  if aIndex <> 0 then
  begin
    GameCursor.Mode := cmTiles;
    GameCursor.Tag1 := aIndex - 1; //MapEdTileRemap is 1 based, tag is 0 based
    if TilesRandom.Checked then
      GameCursor.MapEdDir := 4;

    fLastTile := aIndex;
  end;

  TilesRefresh(nil);
end;


procedure TKMMapEdTerrain.TilesRefresh;
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
  TilesRandom.Checked := (GameCursor.MapEdDir = 4);

  for I := 0 to MAPED_TILES_Y - 1 do
  for K := 0 to MAPED_TILES_X - 1 do
  begin
    L := I * MAPED_TILES_X + K;
    TileID := GetTileIDFromTag(L);
    TilesTable[L].TexID := TileID;
    TilesTable[L].Enabled := TileID <> 0;
    TilesTable[L].Hint := IntToStr(TileID);
    //If cursor has a tile then make sure its properly selected in table as well
    TilesTable[L].Down := (GameCursor.Mode = cmTiles) and (GameCursor.Tag1 = TileID - 1);
  end;
end;


procedure TKMMapEdTerrain.ObjectsChange(Sender: TObject);
var
  ObjID: Integer;
begin
  ObjID := ObjectsScroll.Position * 3 + TKMButtonFlat(Sender).Tag; //0..n-1

  //Skip indexes out of range
  if not InRange(ObjID, 0, fCountCompact - 1)
  and not (TKMButtonFlat(Sender).Tag = 255)
  and not (TKMButtonFlat(Sender).Tag = 61) then
    Exit;

  GameCursor.Mode := cmObjects;
  if TKMButtonFlat(Sender).Tag = 255 then
    //Erase
    GameCursor.Tag1 := 255
  else
  if TKMButtonFlat(Sender).Tag = 61 then
    //Block
    GameCursor.Tag1 := 61
  else
    //Object
    GameCursor.Tag1 := fCompactToMapElem[ObjID]; //0..n-1

  fLastObject := TKMButtonFlat(Sender).Tag;

  ObjectsRefresh(nil);
end;


procedure TKMMapEdTerrain.ObjectsRefresh(Sender: TObject);
var
  I: Integer;
  ObjID: Integer;
begin
  for I := 0 to 8 do
  begin
    ObjID := ObjectsScroll.Position * 3 + I;
    if ObjID < fCountCompact then
    begin
      ObjectsTable[I].TexID := MapElem[fCompactToMapElem[ObjID]].Anim.Step[1] + 1;
      ObjectsTable[I].Caption := IntToStr(ObjID);
      ObjectsTable[I].Enable;
    end
    else
    begin
      ObjectsTable[I].TexID := 0;
      ObjectsTable[I].Caption := '';
      ObjectsTable[I].Disable;
    end;
    //Mark the selected one using reverse lookup
    ObjectsTable[I].Down := (GameCursor.Mode = cmObjects) and (ObjID = fMapElemToCompact[GameCursor.Tag1]);
  end;

  ObjectErase.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 255); //or delete button
  ObjectBlock.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 61); //or block button
end;


procedure TKMMapEdTerrain.SelectionClick(Sender: TObject);
begin
  GameCursor.Mode := cmSelection;
  GameCursor.Tag1 := 0;

  if Sender = Button_SelectCopy then
  begin
    //Copy selection into cursor
    fGame.MapEditor.Selection.Selection_Copy;
    Button_SelectPaste.Enabled := fGame.MapEditor.Selection.Selection_DataInBuffer;
  end
  else
  if Sender = Button_SelectPaste then
  begin
    //Paste selection
    fGame.MapEditor.Selection.Selection_PasteBegin;

    Button_SelectPasteApply.Enable;
    Button_SelectPasteCancel.Enable;
    Button_SelectFlipH.Enable;
    Button_SelectFlipV.Enable;
    Button_SelectCopy.Disable;
    Button_SelectPaste.Disable;
  end
  else
  if Sender = Button_SelectPasteApply then
  begin
    //Apply paste
    fGame.MapEditor.Selection.Selection_PasteApply;
    fGame.MapEditor.TerrainPainter.MakeCheckpoint;

    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
    Button_SelectFlipH.Disable;
    Button_SelectFlipV.Disable;
    Button_SelectCopy.Enable;
    Button_SelectPaste.Enable;
  end
  else
  if Sender = Button_SelectPasteCancel then
  begin
    //Cancel pasting
    fGame.MapEditor.Selection.Selection_PasteCancel;
    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
    Button_SelectFlipH.Disable;
    Button_SelectFlipV.Disable;
    Button_SelectCopy.Enable;
    Button_SelectPaste.Enable;
  end
  else
  if Sender = Button_SelectFlipH then
  begin
    //Flip selected
    fGame.MapEditor.Selection.Selection_Flip(fa_Horizontal);
  end
  else
  if Sender = Button_SelectFlipV then
  begin
    //Flip selected
    fGame.MapEditor.Selection.Selection_Flip(fa_Vertical);
  end;
end;


procedure TKMMapEdTerrain.UnRedoClick(Sender: TObject);
begin
  if Sender = Button_TerrainUndo then
    fGame.MapEditor.TerrainPainter.Undo;

  if Sender = Button_TerrainRedo then
    fGame.MapEditor.TerrainPainter.Redo;

  Button_TerrainUndo.Enabled := fGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := fGame.MapEditor.TerrainPainter.CanRedo;
end;


procedure TKMMapEdTerrain.Show;
begin
  Panel_Terrain.Show;
  PageChange(Button_Terrain[ttBrush]);
end;


function TKMMapEdTerrain.Visible: Boolean;
begin
  Result := Panel_Terrain.Visible;
end;


//Check if specific page is visble
function TKMMapEdTerrain.Visible(aPage: TKMTerrainTab): Boolean;
begin
  Result := False;
  case aPage of
    ttBrush:      Result := Panel_Brushes.Visible;
    ttHeights:    Result := Panel_Heights.Visible;
    ttTile:       Result := Panel_Tiles.Visible;
    ttObject:     Result := Panel_Objects.Visible;
    ttSelection:  Result := Panel_Selection.Visible;
  end;
end;


procedure TKMMapEdTerrain.UpdateState;
begin
  TilesRandom.Checked := (GameCursor.MapEdDir = 4);

  Button_SelectPaste.Enabled := fGame.MapEditor.Selection.Selection_DataInBuffer;

  Button_TerrainUndo.Enabled := fGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := fGame.MapEditor.TerrainPainter.CanRedo;

  //TODO: hack for selection visibility
  if Panel_Selection.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlSelection];
end;


end.
