unit KM_GUIMapEdTerrain;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
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
    fOnPageChange: TNotifyEvent;

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
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;
    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
    procedure KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var aHandled: Boolean);

    property GuiTiles: TKMMapEdTerrainTiles read fGuiTiles;

    procedure Show(aTab: TKMTerrainTab);
    procedure ShowIndex(aIndex: Byte);
    function Visible(aPage: TKMTerrainTab): Boolean; overload;
    function Visible: Boolean; overload;
    procedure Resize;
    procedure UpdateState;
    procedure RightClickCancel;
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_GameCursor, KM_RenderUI, KM_InterfaceGame;


{ TKMMapEdTerrain }
constructor TKMMapEdTerrain.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
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

  fOnPageChange := aOnPageChange;

  Panel_Terrain := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 50);
    for I := Low(TKMTerrainTab) to High(TKMTerrainTab) do
    begin
      Button_Terrain[I] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * Byte(I), 0, SMALL_TAB_W, SMALL_TAB_H, BtnGlyph[I], rxGui, bsGame);
      Button_Terrain[I].Hint := gResTexts[BtnHint[I]];
      Button_Terrain[I].OnClick := PageChange;
    end;

    Button_TerrainUndo := TKMButton.Create(Panel_Terrain, 155, 0, 15, 26, '<', bsGame);
    Button_TerrainUndo.Hint := gResTexts[TX_MAPED_UNDO_HINT];
    Button_TerrainUndo.OnClick := UnRedoClick;
    Button_TerrainRedo := TKMButton.Create(Panel_Terrain, 170, 0, 15, 26, '>', bsGame);
    Button_TerrainRedo.Hint := gResTexts[TX_MAPED_REDO_HINT];
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


procedure TKMMapEdTerrain.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  fGuiObjects.KeyDown(Key, Shift, aHandled);
end;


procedure TKMMapEdTerrain.KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if Visible then
  begin
    if (ssCtrl in Shift) and (Key = Ord('Y')) then
    begin
      Button_TerrainRedo.Click; //Ctrl+Y = Redo
      aHandled := True;
    end;
    if (ssCtrl in Shift) and (Key = Ord('Z')) then
    begin
      if ssShift in Shift then
        Button_TerrainUndo.Click //Ctrl+Shift+Z = Redo
      else
        Button_TerrainUndo.Click; //Ctrl+Z = Undo
      aHandled := True;
    end;
  end;
  fGuiObjects.KeyUp(Key, Shift, aHandled);
end;


procedure TKMMapEdTerrain.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer; var aHandled: Boolean);
begin
  fGuiBrushes.MouseWheel(Shift, WheelDelta, X, Y, aHandled);
end;


procedure TKMMapEdTerrain.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gGameCursor.Mode := cmNone;

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

  //Signla that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdTerrain.UnRedoClick(Sender: TObject);
begin
  if Sender = Button_TerrainUndo then
    gGame.MapEditor.TerrainPainter.Undo;

  if Sender = Button_TerrainRedo then
    gGame.MapEditor.TerrainPainter.Redo;

  Button_TerrainUndo.Enabled := gGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := gGame.MapEditor.TerrainPainter.CanRedo;
end;


procedure TKMMapEdTerrain.Show(aTab: TKMTerrainTab);
begin
  Panel_Terrain.Show;
  PageChange(Button_Terrain[aTab]);
end;


procedure TKMMapEdTerrain.ShowIndex(aIndex: Byte);
begin
  if aIndex in [Byte(Low(TKMTerrainTab))..Byte(High(TKMTerrainTab))] then
    Show(TKMTerrainTab(aIndex));
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


procedure TKMMapEdTerrain.Resize;
begin
  fGuiObjects.Resize;
end;


procedure TKMMapEdTerrain.RightClickCancel;
begin
  fGuiObjects.RightClickCancel;
end;


procedure TKMMapEdTerrain.UpdateState;
begin
  fGuiBrushes.UpdateState;
  fGuiTiles.UpdateState;
  fGuiObjects.UpdateState;
  fGuiSelection.UpdateState;

  Button_TerrainUndo.Enabled := gGame.MapEditor.TerrainPainter.CanUndo;
  Button_TerrainRedo.Enabled := gGame.MapEditor.TerrainPainter.CanRedo;
end;


end.
