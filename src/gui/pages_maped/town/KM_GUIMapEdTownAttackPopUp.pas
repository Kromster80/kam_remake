unit KM_GUIMapEdTownAttackPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, SysUtils,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_Points, KM_AIAttacks;

type
  TKMMapEdTownAttack = class
  private
    fOwner: TKMHandIndex;
    fIndex: Integer;
    procedure Attack_Change(Sender: TObject);
    procedure Attack_Close(Sender: TObject);
    procedure Attack_Refresh(aAttack: TAIAttack);
    procedure Attack_Save;
    procedure Attack_Switch(Sender: TObject);
    function GetVisible: Boolean;
  protected
    Panel_Attack: TKMPanel;
    Label_AttackHeader: TKMLabel;
    Button_Next: TKMButton;
    Button_Prev: TKMButton;
    Radio_AttackType: TKMRadioGroup;
    NumEdit_AttackDelay: TKMNumericEdit;
    NumEdit_AttackMen: TKMNumericEdit;
    NumEdit_AttackAmount: array [TGroupType] of TKMNumericEdit;
    CheckBox_AttackTakeAll: TKMCheckBox;
    Radio_AttackTarget: TKMRadioGroup;
    TrackBar_AttackRange: TKMTrackBar;
    NumEdit_AttackLocX: TKMNumericEdit;
    NumEdit_AttackLocY: TKMNumericEdit;
    Button_AttackOk: TKMButton;
    Button_AttackCancel: TKMButton;
  public
    fOnDone: TNotifyEvent;
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure Show(aPlayer: TKMHandIndex; aIndex: Integer);
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand;


const
  GROUP_TEXT: array [TGroupType] of Integer = (
    TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE,
    TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);


{ TKMMapEdAttack }
constructor TKMMapEdTownAttack.Create(aParent: TKMPanel);
const
  SIZE_X = 570;
  SIZE_Y = 360;
var
  GT: TGroupType;
begin
  inherited Create;

  Panel_Attack := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Attack.AnchorsCenter;
  Panel_Attack.Hide;

    TKMBevel.Create(Panel_Attack, -1000,  -1000, 4000, 4000);
    with TKMImage.Create(Panel_Attack, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain) do ImageStretch;
    TKMBevel.Create(Panel_Attack,   0,  0, SIZE_X, SIZE_Y);
    Label_AttackHeader := TKMLabel.Create(Panel_Attack, SIZE_X div 2, 10, gResTexts[TX_MAPED_AI_ATTACK_INFO], fnt_Outline, taCenter);

    Button_Prev := TKMButton.Create(Panel_Attack, 20, 10, 60, 20, '<<', bsMenu);
    Button_Prev.OnClick := Attack_Switch;
    Button_Next := TKMButton.Create(Panel_Attack, SIZE_X-20-60, 10, 60, 20, '>>', bsMenu);
    Button_Next.OnClick := Attack_Switch;

    TKMLabel.Create(Panel_Attack, 20, 40, gResTexts[TX_MAPED_AI_ATTACK_COL_TYPE], fnt_Metal, taLeft);
    Radio_AttackType := TKMRadioGroup.Create(Panel_Attack, 20, 60, 160, 40, fnt_Grey);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_ONCE]);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_REP]);
    Radio_AttackType.OnChange := Attack_Change;

    with TKMLabel.Create(Panel_Attack, 180, 40, 150, 40, gResTexts[TX_MAPED_AI_ATTACK_DELAY], fnt_Metal, taLeft) do
      AutoWrap := True;
    NumEdit_AttackDelay := TKMNumericEdit.Create(Panel_Attack, 180, 80, 0, High(SmallInt));
    NumEdit_AttackDelay.OnChange := Attack_Change;

    with TKMLabel.Create(Panel_Attack, 340, 40, 200, 40, gResTexts[TX_MAPED_AI_ATTACK_SOLDIERS], fnt_Metal, taLeft) do
      AutoWrap := True;
    NumEdit_AttackMen := TKMNumericEdit.Create(Panel_Attack, 340, 80, 0, 1000);
    NumEdit_AttackMen.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 340, 160, gResTexts[TX_MAPED_AI_ATTACK_COUNT], fnt_Metal, taLeft);
    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      TKMLabel.Create(Panel_Attack, 425, 180 + Byte(GT) * 20, 0, 0, gResTexts[GROUP_TEXT[GT]], fnt_Grey, taLeft);
      NumEdit_AttackAmount[GT] := TKMNumericEdit.Create(Panel_Attack, 340, 180 + Byte(GT) * 20, 0, 255);
      NumEdit_AttackAmount[GT].OnChange := Attack_Change;
    end;

    CheckBox_AttackTakeAll := TKMCheckBox.Create(Panel_Attack, 340, 265, 210, 20, gResTexts[TX_MAPED_AI_ATTACK_TAKE_RANDOMLY], fnt_Metal);
    CheckBox_AttackTakeAll.OnClick := Attack_Change;

    //Second row

    TKMLabel.Create(Panel_Attack, 20, 160, gResTexts[TX_MAPED_AI_ATTACK_COL_TARGET], fnt_Metal, taLeft);
    Radio_AttackTarget := TKMRadioGroup.Create(Panel_Attack, 20, 180, 310, 80, fnt_Grey);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CLOSEST]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE_ARMY]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE_START]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CUSTOM]);
    Radio_AttackTarget.OnChange := Attack_Change;

    NumEdit_AttackLocX := TKMNumericEdit.Create(Panel_Attack, 20, 260, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocX.OnChange := Attack_Change;
    NumEdit_AttackLocY := TKMNumericEdit.Create(Panel_Attack, 20, 280, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocY.OnChange := Attack_Change;

    //Range is not implemented yet (unused feature in KaM?)
    with TKMLabel.Create(Panel_Attack, 200, 240, 'Range (untested)', fnt_Metal, taLeft) do Hide;
    TrackBar_AttackRange := TKMTrackBar.Create(Panel_Attack, 200, 260, 100, 0, 255);
    TrackBar_AttackRange.Disable;
    TrackBar_AttackRange.Hide;
    TrackBar_AttackRange.OnChange := Attack_Change;

    Button_AttackOk := TKMButton.Create(Panel_Attack, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
    Button_AttackOk.OnClick := Attack_Close;
    Button_AttackCancel := TKMButton.Create(Panel_Attack, SIZE_X-20-160, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
    Button_AttackCancel.OnClick := Attack_Close;
end;


procedure TKMMapEdTownAttack.Attack_Change(Sender: TObject);
var
  GT: TGroupType;
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist

  for GT := Low(TGroupType) to High(TGroupType) do
    NumEdit_AttackAmount[GT].Enabled := not CheckBox_AttackTakeAll.Checked;

  NumEdit_AttackLocX.Enabled := (TAIAttackTarget(Radio_AttackTarget.ItemIndex) = att_CustomPosition);
  NumEdit_AttackLocY.Enabled := (TAIAttackTarget(Radio_AttackTarget.ItemIndex) = att_CustomPosition);
end;


procedure TKMMapEdTownAttack.Attack_Close(Sender: TObject);
begin
  if Sender = Button_AttackOk then
    Attack_Save;

  Panel_Attack.Hide;
  fOnDone(Self);
end;


procedure TKMMapEdTownAttack.Attack_Refresh(aAttack: TAIAttack);
var
  GT: TGroupType;
begin
  Label_AttackHeader.Caption := gResTexts[TX_MAPED_AI_ATTACK_INFO] + ' (' + IntToStr(fIndex) + ')';

  //Set attack properties to UI
  Radio_AttackType.ItemIndex := Byte(aAttack.AttackType);
  NumEdit_AttackDelay.Value := aAttack.Delay div 10;
  NumEdit_AttackMen.Value := aAttack.TotalMen;
  for GT := Low(TGroupType) to High(TGroupType) do
    NumEdit_AttackAmount[GT].Value := aAttack.GroupAmounts[GT];
  CheckBox_AttackTakeAll.Checked := aAttack.TakeAll;
  Radio_AttackTarget.ItemIndex := Byte(aAttack.Target);
  TrackBar_AttackRange.Position := aAttack.Range;
  NumEdit_AttackLocX.Value := aAttack.CustomPosition.X;
  NumEdit_AttackLocY.Value := aAttack.CustomPosition.Y;

  //Certain values disable certain controls
  Attack_Change(nil);
end;


procedure TKMMapEdTownAttack.Attack_Save;
var
  AA: TAIAttack;
  GT: TGroupType;
begin
  //Copy attack info from controls to Attacks
  AA.AttackType := TAIAttackType(Radio_AttackType.ItemIndex);
  AA.Delay := NumEdit_AttackDelay.Value * 10;
  AA.TotalMen := NumEdit_AttackMen.Value;
  for GT := Low(TGroupType) to High(TGroupType) do
    AA.GroupAmounts[GT] := NumEdit_AttackAmount[GT].Value;
  AA.TakeAll := CheckBox_AttackTakeAll.Checked;
  AA.Target := TAIAttackTarget(Radio_AttackTarget.ItemIndex);
  AA.Range := TrackBar_AttackRange.Position;
  AA.CustomPosition := KMPoint(NumEdit_AttackLocX.Value, NumEdit_AttackLocY.Value);

  gHands[fOwner].AI.General.Attacks[fIndex] := AA;
end;


//Show previous or next attack
//We save changes before switching
procedure TKMMapEdTownAttack.Attack_Switch(Sender: TObject);
var
  atCount: Integer;
begin
  Attack_Save;

  atCount := gHands[fOwner].AI.General.Attacks.Count;

  if Sender = Button_Prev then
    fIndex := (fIndex + atCount - 1) mod atCount
  else
    fIndex := (fIndex + 1) mod atCount;

  Attack_Refresh(gHands[fOwner].AI.General.Attacks[fIndex]);
end;


function TKMMapEdTownAttack.GetVisible: Boolean;
begin
  Result := Panel_Attack.Visible;
end;


function TKMMapEdTownAttack.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_AttackCancel.IsClickable then
                begin
                  Attack_Close(Button_AttackCancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_AttackOk.IsClickable then
                begin
                  Attack_Close(Button_AttackOk);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdTownAttack.Show(aPlayer: TKMHandIndex; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Attack_Refresh(gHands[fOwner].AI.General.Attacks[fIndex]);
  Panel_Attack.Show;
end;


end.
