unit KM_GUIMapEdTerrain;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_GUIMapEdTerrainBrushes,
   KM_GUIMapEdTerrainHeights,
   KM_GUIMapEdTerrainTiles,
   KM_GUIMapEdTerrainObjects,
   KM_GUIMapEdTerrainSelection;


type
  TKMTerrainTab = (ttBrush, ttHeights, ttTile, ttObject, ttSelection);

  //Collection of terrain editing controls
  TKMMapEdTerrain = class
  private
    fGuiBrushes: TKMMapEdTerrainBrushes;
    fGuiHeights: TKMMapEdTerrainHeights;
    fGuiTiles: TKMMapEdTerrainTiles;
    fGuiObjects: TKMMapEdTerrainObjects;
    fGuiSelection: TKMMapEdTerrainSelection;

    procedure PageChange(Sender: TObject);
    procedure UnRedoClick(Sender: TObject);
  protected
    Panel_Terrain: TKMPanel;
    Button_Terrain: array [TKMTerrainTab] of TKMButton;
    Button_TerrainUndo: TKMButton;
    Button_TerrainRedo: TKMButton;
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
  KM_ResFonts, KM_ResTexts,
  KM_Game, KM_GameCursor, KM_RenderUI,
  KM_InterfaceDefaults, KM_InterfaceMapEditor;


{ TKMMapEdTerrain }
constructor TKMMapEdTerrain.Create(aParent: TKMPanel);
const
  BtnGlyph: array [TKMTerrainTab] of Word = (383, 388, 382, 385, 384);
  BtnHint: array [TKMTerrainTab] of Word = (
    TX_MAPED_TERRAIN_HINTS_BRUSHES,
    TX_MAPED_TERRAIN_HINTS_HEIGHTS,
    TX_MAPED_TERRAIN_HINTS_TILES,
    TX_MAPED_TERRAIN_HINTS_OBJECTS,
    TX_MAPED_COPY_TITLE);
var
  I: TKMTerrainTab;
begin
  inherited Create;

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
    fGuiObjects := TKMMapEdTerrainObjects.Create(Panel_Terrain);
    fGuiSelection := TKMMapEdTerrainSelection.Create(Panel_Terrain);
end;


destructor TKMMapEdTerrain.Destroy;
begin
  fGuiBrushes.Free;
  fGuiHeights.Free;
  fGuiTiles.Free;
  fGuiObjects.Free;
  fGuiSelection.Free;

  inherited;
end;


procedure TKMMapEdTerrain.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Hide existing pages
  fGuiBrushes.Hide;
  fGuiHeights.Hide;
  fGuiTiles.Hide;
  fGuiObjects.Hide;
  fGuiSelection.Hide;

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
    fGuiObjects.Show
  else
  if (Sender = Button_Terrain[ttSelection]) then
    fGuiSelection.Show;

  fOnPageChanged(Slef);
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
    ttObject:     Result := fGuiObjects.Visible;
    ttSelection:  Result := fGuiSelection.Visible;
  end;
end;


procedure TKMMapEdTerrain.UpdateState;
begin
  fGuiTiles.UpdateState;
  fGuiSelection.UpdateState;

  Button_TerrainUndo.Enabled := fGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := fGame.MapEditor.TerrainPainter.CanRedo;
end;


end.
