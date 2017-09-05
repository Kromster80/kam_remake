unit KM_GUIMapEdTownFormationsPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_Defaults, KM_Pics;


type
  TKMMapEdTownFormations = class
  private
    fOwner: TKMHandIndex;
    procedure Formations_Close(Sender: TObject);
    function GetVisible: Boolean;
  protected
    Panel_Formations: TKMPanel;
    Image_FormationsFlag: TKMImage;
    NumEdit_FormationsCount,
    NumEdit_FormationsColumns: array [TGroupType] of TKMNumericEdit;
    Button_Formations_Ok: TKMButton;
    Button_Formations_Cancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    procedure Show(aPlayer: TKMHandIndex);
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand;


{ TKMMapEdFormations }
constructor TKMMapEdTownFormations.Create(aParent: TKMPanel);
const
  T: array [TGroupType] of Integer = (TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE, TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);  SIZE_X = 570;
  SIZE_Y = 200;
var
  GT: TGroupType;
  Img: TKMImage;
begin
  inherited Create;

  Panel_Formations := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Formations.AnchorsCenter;
  Panel_Formations.Hide;

  TKMBevel.Create(Panel_Formations, -1000,  -1000, 4000, 4000);
  Img := TKMImage.Create(Panel_Formations, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
  Img.ImageStretch;
  TKMBevel.Create(Panel_Formations,   0,  0, SIZE_X, SIZE_Y);
  TKMLabel.Create(Panel_Formations, SIZE_X div 2, 10, gResTexts[TX_MAPED_AI_FORMATIONS_TITLE], fnt_Outline, taCenter);

  Image_FormationsFlag := TKMImage.Create(Panel_Formations, 10, 10, 0, 0, 30, rxGuiMain);

  TKMLabel.Create(Panel_Formations, 20, 70, 80, 0, gResTexts[TX_MAPED_AI_FORMATIONS_COUNT], fnt_Metal, taLeft);
  TKMLabel.Create(Panel_Formations, 20, 95, 80, 0, gResTexts[TX_MAPED_AI_FORMATIONS_COLUMNS], fnt_Metal, taLeft);

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    TKMLabel.Create(Panel_Formations, 130 + Byte(GT) * 110 + 32, 50, 0, 0, gResTexts[T[GT]], fnt_Metal, taCenter);
    NumEdit_FormationsCount[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 70, 1, 255);
    NumEdit_FormationsColumns[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 95, 1, 255);
  end;

  Button_Formations_Ok := TKMButton.Create(Panel_Formations, SIZE_X-20-320-10, 150, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
  Button_Formations_Ok.OnClick := Formations_Close;
  Button_Formations_Cancel := TKMButton.Create(Panel_Formations, SIZE_X-20-160, 150, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
  Button_Formations_Cancel.OnClick := Formations_Close;
end;


function TKMMapEdTownFormations.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_Formations_Cancel.IsClickable then
                begin
                  Formations_Close(Button_Formations_Cancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_Formations_Ok.IsClickable then
                begin
                  Formations_Close(Button_Formations_Ok);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdTownFormations.Show(aPlayer: TKMHandIndex);
var
  GT: TGroupType;
begin
  fOwner := aPlayer;

  //Fill UI
  Image_FormationsFlag.FlagColor := gHands[fOwner].FlagColor;

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    NumEdit_FormationsCount[GT].Value := gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].NumUnits;
    NumEdit_FormationsColumns[GT].Value := gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow;
  end;

  Panel_Formations.Show;
end;


function TKMMapEdTownFormations.GetVisible: Boolean;
begin
  Result := Panel_Formations.Visible;
end;


procedure TKMMapEdTownFormations.Formations_Close(Sender: TObject);
var
  GT: TGroupType;
begin
  Assert(Image_FormationsFlag.FlagColor = gHands[fOwner].FlagColor, 'Cheap test to see if active player didn''t changed');

  if Sender = Button_Formations_Ok then
    //Save settings
    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].NumUnits := NumEdit_FormationsCount[GT].Value;
      gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow := NumEdit_FormationsColumns[GT].Value;
    end;

  Panel_Formations.Hide;
end;


end.
