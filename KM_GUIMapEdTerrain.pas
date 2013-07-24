unit KM_GUIMapEdTerrain;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_GUIMapEdTerrainBrushes,
   KM_GUIMapEdTerrainHeights,
   KM_GUIMapEdTerrainTiles;

type
  TKMTerrainTab = (ttBrush, ttHeights, ttTile, ttObject, ttSelection);

  //Collection of terrain editing controls
  TKMMapEdTerrain = class
  private
    fGuiBrushes: TKMMapEdTerrainBrushes;
    fGuiHeights: TKMMapEdTerrainHeights;
    fGuiTiles: TKMMapEdTerrainTiles;

    //Objects in MapElem are placed sparsely, so we need to compact them
    //to use in MapEd palette
    fLastObject: Byte;
    fCountCompact: Integer;
    fCompactToMapElem: array [Byte] of Byte; //Pointers to valid MapElem's
    fMapElemToCompact: array [Byte] of Byte; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.

    procedure CompactMapElements;
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
    destructor Destroy; override;

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
    fGuiBrushes.Show
  else
  if (Sender = Button_Terrain[ttHeights]) then
    fGuiHeights.Show
  else
  if (Sender = Button_Terrain[ttTile]) then
    fGuiTiles.Show
  else
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
    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
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
  Surfaces: array [0 .. 6, 0 .. 4] of TKMTerrainKind = (
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

    fGuiBrushes := TKMMapEdTerrainBrushes.Create(Panel_Terrain);
    fGuiHeights := TKMMapEdTerrainHeights.Create(Panel_Terrain);
    fGuiTiles := TKMMapEdTerrainTiles.Create(Panel_Terrain);

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


destructor TKMMapEdTerrain.Destroy;
begin
  fGuiBrushes.Free;
  fGuiHeights.Free;
  fGuiTiles.Free;

  inherited;
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
    ttBrush:      Result := fGuiBrushes.Visible;
    ttHeights:    Result := fGuiHeights.Visible;
    ttTile:       Result := fGuiTiles.Visible;
    ttObject:     Result := Panel_Objects.Visible;
    ttSelection:  Result := Panel_Selection.Visible;
  end;
end;


procedure TKMMapEdTerrain.UpdateState;
begin
  fGuiTiles.UpdateState;

  Button_SelectPaste.Enabled := fGame.MapEditor.Selection.Selection_DataInBuffer;

  Button_TerrainUndo.Enabled := fGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := fGame.MapEditor.TerrainPainter.CanRedo;

  //TODO: hack for selection visibility
  if Panel_Selection.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlSelection];
end;


end.
