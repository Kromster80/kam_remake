unit KM_GUIMapEdPlayerColors;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls;

type
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptMarkers);

  TKMMapEdPlayerColors = class
  private
    procedure Player_ColorClick(Sender: TObject);
  protected
    Panel_Color: TKMPanel;
    ColorSwatch_Color: TKMColorSwatch;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_Game, KM_ResTexts, KM_RenderUI, KM_Resource, KM_ResFonts, KM_InterfaceGame;


{ TKMMapEdPlayerColors }
constructor TKMMapEdPlayerColors.Create(aParent: TKMPanel);
var
  I: Integer;
  Col: array [0..255] of Cardinal;
begin
  inherited Create;

  Panel_Color := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Color, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYER_COLORS], fnt_Outline, taCenter);
  TKMBevel.Create(Panel_Color, 0, 30, TB_WIDTH, 180);
  ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 2, 32, 16, 16, 11);
  for I := 0 to 255 do
    Col[I] := fResource.Palettes.DefDal.Color32(I);
  ColorSwatch_Color.SetColors(Col);
  ColorSwatch_Color.OnClick := Player_ColorClick;
end;


procedure TKMMapEdPlayerColors.Player_ColorClick(Sender: TObject);
begin
  gHands[MySpectator.HandIndex].FlagColor := ColorSwatch_Color.GetColor;

  gGame.ActiveInterface.SyncUI;
end;


procedure TKMMapEdPlayerColors.Hide;
begin
  Panel_Color.Hide;
end;


procedure TKMMapEdPlayerColors.Show;
begin
  Panel_Color.Show;
end;


function TKMMapEdPlayerColors.Visible: Boolean;
begin
  Result := Panel_Color.Visible;
end;


end.
