unit KM_GUIMapEdTerrainHeights;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults;


type
  //Terrain height editing
  TKMMapEdTerrainHeights = class
  private
    procedure HeightChange(Sender: TObject);
    procedure HeightRefresh;
  protected
    Panel_Heights: TKMPanel;
    HeightSize: TKMTrackBar;
    HeightSlope: TKMTrackBar;
    HeightSpeed: TKMTrackBar;
    HeightShapeLabel: TKMLabel;
    HeightCircle: TKMButtonFlat;
    HeightSquare: TKMButtonFlat;
    HeightElevate: TKMButtonFlat;
    HeightUnequalize: TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_ResFonts, KM_ResTexts, KM_GameCursor, KM_RenderUI,
  KM_InterfaceGame;


{ TKMMapEdTerrainHeights }
constructor TKMMapEdTerrainHeights.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Heights := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
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
end;


procedure TKMMapEdTerrainHeights.HeightChange(Sender: TObject);
begin
  gGameCursor.MapEdSize := HeightSize.Position;
  gGameCursor.MapEdSlope := HeightSlope.Position;
  gGameCursor.MapEdSpeed := HeightSpeed.Position;

  //Shape
  if Sender = HeightCircle then
    gGameCursor.MapEdShape := hsCircle
  else
  if Sender = HeightSquare then
    gGameCursor.MapEdShape := hsSquare;

  //Kind
  if Sender = HeightElevate then
    gGameCursor.Mode := cmElevate
  else
  if Sender = HeightUnequalize then
    gGameCursor.Mode := cmEqualize;

  HeightRefresh;
end;


procedure TKMMapEdTerrainHeights.HeightRefresh;
begin
  HeightCircle.Down := (gGameCursor.MapEdShape = hsCircle);
  HeightSquare.Down := (gGameCursor.MapEdShape = hsSquare);

  HeightElevate.Down := (gGameCursor.Mode = cmElevate);
  HeightUnequalize.Down := (gGameCursor.Mode = cmEqualize);
end;


procedure TKMMapEdTerrainHeights.Show;
begin
  HeightChange(HeightCircle);
  HeightChange(HeightElevate);
  Panel_Heights.Show;
end;


function TKMMapEdTerrainHeights.Visible: Boolean;
begin
  Result := Panel_Heights.Visible;
end;


procedure TKMMapEdTerrainHeights.Hide;
begin
  Panel_Heights.Hide;
end;


end.
