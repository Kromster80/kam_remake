unit KM_GUIMapEdTerrainBrushes;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;


type
  //Painting on terrain with terrain brushes
  TKMMapEdTerrainBrushes = class
  private
    procedure BrushChange(Sender: TObject);
    procedure BrushRefresh;
  protected
    Panel_Brushes: TKMPanel;
    BrushSize: TKMTrackBar;
    BrushCircle: TKMButtonFlat;
    BrushSquare: TKMButtonFlat;
    BrushTable: array [0..6, 0..4] of TKMButtonFlat;
    BrushRandom: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_ResFonts, KM_ResTexts, KM_Game, KM_GameCursor, KM_RenderUI,
  KM_TerrainPainter, KM_InterfaceDefaults;


{ TKMMapEdTerrainBrushes }
constructor TKMMapEdTerrainBrushes.Create(aParent: TKMPanel);
const
  Surfaces: array [0 .. 6, 0 .. 4] of TKMTerrainKind = (
    (tkGrass,       tkMoss,         tkRustyGrass1,  tkRustyGrass2,  tkCustom),
    (tkDirtGrass,   tkDirt,         tkGravel,       tkCobbleStone,  tkCustom),
    (tkGrassSand2,  tkGrassSand1,   tkSand,         tkRichSand,     tkCustom),
    (tkSwamp,       tkGrassyWater,  tkWater,        tkFastWater,    tkCustom),
    (tkShallowSnow, tkSnow,         tkDeepSnow,     tkIce,          tkCustom),
    (tkStoneMount,  tkGoldMount,    tkIronMount,    tkAbyss,        tkCustom),
    (tkCoal,        tkGold,         tkIron,         tkLava,         tkCustom));
var
  I,K: Integer;
begin
  inherited Create;

  Panel_Brushes := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);

  TKMLabel.Create(Panel_Brushes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_BRUSH], fnt_Outline, taCenter);
  BrushSize   := TKMTrackBar.Create(Panel_Brushes, 0, 30, 100, 0, 12);
  BrushSize.OnChange := BrushChange;
  BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 106, 28, 24, 24, 592);
  BrushCircle.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
  BrushCircle.OnClick := BrushChange;
  BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 134, 28, 24, 24, 593);
  BrushSquare.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
  BrushSquare.OnClick := BrushChange;

  for I := Low(Surfaces) to High(Surfaces) do
  for K := Low(Surfaces[I]) to High(Surfaces[I]) do
  if Surfaces[I,K] <> tkCustom then
  begin
    BrushTable[I,K] := TKMButtonFlat.Create(Panel_Brushes, K * 36, 60 + I * 40, 34, 34, Combo[Surfaces[I,K], Surfaces[I,K], 1] + 1, rxTiles);  // grass
    BrushTable[I,K].Tag := Byte(Surfaces[I,K]);
    BrushTable[I,K].OnClick := BrushChange;
  end;

  BrushRandom := TKMCheckBox.Create(Panel_Brushes, 0, 350, TB_WIDTH, 20, gResTexts[TX_MAPED_TERRAIN_BRUSH_RANDOM], fnt_Metal);
  BrushRandom.OnClick := BrushChange;
end;


procedure TKMMapEdTerrainBrushes.BrushChange(Sender: TObject);
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


procedure TKMMapEdTerrainBrushes.BrushRefresh;
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


procedure TKMMapEdTerrainBrushes.Hide;
begin
  Panel_Brushes.Hide;
end;


procedure TKMMapEdTerrainBrushes.Show;
begin
  BrushChange(BrushTable[0,0]);

  Panel_Brushes.Show;
end;


function TKMMapEdTerrainBrushes.Visible: Boolean;
begin
  Result := Panel_Brushes.Visible;
end;


end.
