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
    function GetCodeBGRColor(aColor: Cardinal): String;
  protected
    Panel_Color: TKMPanel;
    ColorSwatch_Color: TKMColorSwatch;
    //Components for Show Code BGR Color
    Shape_BGRColor: TKMShape;
    Edit_BGRCode: TKMEdit;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_Game, KM_ResTexts, KM_RenderUI, KM_Resource, KM_ResFonts,
  KM_InterfaceGame, KM_Hand, KM_CommonUtils;


{ TKMMapEdPlayerColors }
constructor TKMMapEdPlayerColors.Create(aParent: TKMPanel);
const MAX_COL = 288;
var
  Hue, Sat, Bri, I, K: Integer;
  R, G, B: Byte;
  Col: array [0..MAX_COL-1] of Cardinal;
begin
  inherited Create;

  Panel_Color := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Color, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYER_COLORS], fnt_Outline, taCenter);
  TKMBevel.Create(Panel_Color, 0, 30, TB_WIDTH, 202);
  ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 2, 32, 16, 18, 11);

  //Show Code BGR Color
  TKMBevel.Create(Panel_Color, 0, 235, 20, 20);
  Shape_BGRColor := TKMShape.Create(Panel_Color, 2, 237, 17, 17);
  Edit_BGRCode := TKMEdit.Create(Panel_Color, 20, 235, TB_WIDTH - 20, 20, fnt_Metal, true);
  Edit_BGRCode.BlockInput := True;


  //Generate a palette using HSB so the layout is more intuitive
  I := 0;
  for Hue := 0 to 16 do //Less than 17 hues doesn't give a good solid yellow hue
    for Bri := 1 to 4 do
      for Sat := 4 downto 1 do //Reversed saturation looks more natural
      begin
        ConvertHSB2RGB(Hue/17, Sat/4, Bri/5, R, G, B);
        Col[I] := (B shl 16) or (G shl 8) or R or $FF000000;
        Inc(I);
      end;
  //Add greyscale at the bottom
  for I := 0 to 15 do
  begin
    K := I*16;
    Col[MAX_COL-16+I] := (K shl 16) or (K shl 8) or K or $FF000000;
  end;

  ColorSwatch_Color.SetColors(Col);

  ColorSwatch_Color.OnClick := Player_ColorClick;
end;


function TKMMapEdPlayerColors.GetCodeBGRColor(aColor: Cardinal): String;
begin
  Result := Format('%.6x', [FlagColorToTextColor(aColor) and $FFFFFF]);
end;

procedure TKMMapEdPlayerColors.Player_ColorClick(Sender: TObject);
begin
  gMySpectator.Hand.FlagColor := ColorSwatch_Color.GetColor;
  Shape_BGRColor.FillColor := ColorSwatch_Color.GetColor;
  Edit_BGRCode.Text := GetCodeBGRColor(ColorSwatch_Color.GetColor);
  gGame.ActiveInterface.SyncUI(False);
end;

procedure TKMMapEdPlayerColors.Hide;
begin
  Panel_Color.Hide;
end;

procedure TKMMapEdPlayerColors.Show;
begin
  Panel_Color.Show;
  ColorSwatch_Color.SelectByColor(gMySpectator.Hand.FlagColor);
  Shape_BGRColor.FillColor := gMySpectator.Hand.FlagColor;
  Edit_BGRCode.Text := GetCodeBGRColor(gMySpectator.Hand.FlagColor);
end;


function TKMMapEdPlayerColors.Visible: Boolean;
begin
  Result := Panel_Color.Visible;
end;


end.
