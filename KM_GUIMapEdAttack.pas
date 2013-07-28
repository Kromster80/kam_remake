unit KM_GUIMapEdAttack;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_Points, KM_AIAttacks;

type
  TKMMapEdAttack = class
  private
    fOwner: TPlayerIndex;
    fIndex: Integer;
    procedure Attack_Change(Sender: TObject);
    procedure Attack_Close(Sender: TObject);
    procedure Attack_Refresh(aAttack: TAIAttack);
  protected
    Panel_Attack: TKMPanel;
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

    procedure Show(aPlayer: TPlayerIndex; aIndex: Integer);
  end;


implementation
uses
  KM_PlayersCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts;


const
  GROUP_TEXT: array [TGroupType] of Integer = (
    TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE,
    TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);

  GROUP_IMG: array [TGroupType] of Word = (
    371, 374,
    376, 377);


{ TKMMapEdAttack }
constructor TKMMapEdAttack.Create(aParent: TKMPanel);
const
  SIZE_X = 570;
  SIZE_Y = 360;
var
  GT: TGroupType;
begin
  inherited Create;

  Panel_Attack := TKMPanel.Create(aParent, 362, 250, SIZE_X, SIZE_Y);
  Panel_Attack.Anchors := [];
  Panel_Attack.Hide;

    TKMBevel.Create(Panel_Attack, -1000,  -1000, 4000, 4000);
    with TKMImage.Create(Panel_Attack, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain) do ImageStretch;
    TKMBevel.Create(Panel_Attack,   0,  0, SIZE_X, SIZE_Y);
    TKMLabel.Create(Panel_Attack, SIZE_X div 2, 10, gResTexts[TX_MAPED_AI_ATTACK_INFO], fnt_Outline, taCenter);

    TKMLabel.Create(Panel_Attack, 20, 40, gResTexts[TX_MAPED_AI_ATTACK_COL_TYPE], fnt_Metal, taLeft);
    Radio_AttackType := TKMRadioGroup.Create(Panel_Attack, 20, 60, 80, 40, fnt_Metal);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_ONCE]);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_REP]);
    Radio_AttackType.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 130, 40, gResTexts[TX_MAPED_AI_ATTACK_DELAY], fnt_Metal, taLeft);
    NumEdit_AttackDelay := TKMNumericEdit.Create(Panel_Attack, 130, 60, 0, High(SmallInt));
    NumEdit_AttackDelay.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 240, 40, gResTexts[TX_MAPED_AI_ATTACK_COL_MEN], fnt_Metal, taLeft);
    NumEdit_AttackMen := TKMNumericEdit.Create(Panel_Attack, 240, 60, 0, 1000);
    NumEdit_AttackMen.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 340, 40, gResTexts[TX_MAPED_AI_ATTACK_COUNT], fnt_Metal, taLeft);
    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      TKMLabel.Create(Panel_Attack, 425, 60 + Byte(GT) * 20, 0, 0, gResTexts[GROUP_TEXT[GT]], fnt_Metal, taLeft);
      NumEdit_AttackAmount[GT] := TKMNumericEdit.Create(Panel_Attack, 340, 60 + Byte(GT) * 20, 0, 255);
      NumEdit_AttackAmount[GT].OnChange := Attack_Change;
    end;

    CheckBox_AttackTakeAll := TKMCheckBox.Create(Panel_Attack, 340, 145, 160, 20, gResTexts[TX_MAPED_AI_ATTACK_TAKE_ALL], fnt_Metal);
    CheckBox_AttackTakeAll.OnClick := Attack_Change;

    //Second row

    TKMLabel.Create(Panel_Attack, 20, 170, gResTexts[TX_MAPED_AI_ATTACK_COL_TARGET], fnt_Metal, taLeft);
    Radio_AttackTarget := TKMRadioGroup.Create(Panel_Attack, 20, 190, 160, 80, fnt_Metal);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CLOSEST]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE1]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE2]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CUSTOM]);
    Radio_AttackTarget.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 200, 170, gResTexts[TX_MAPED_AI_TARGET_POS], fnt_Metal, taLeft);
    NumEdit_AttackLocX := TKMNumericEdit.Create(Panel_Attack, 200, 190, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocX.OnChange := Attack_Change;
    NumEdit_AttackLocY := TKMNumericEdit.Create(Panel_Attack, 200, 210, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocY.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 200, 240, 'Range (untested)', fnt_Metal, taLeft);
    TrackBar_AttackRange := TKMTrackBar.Create(Panel_Attack, 200, 260, 100, 0, 255);
    TrackBar_AttackRange.Disable;
    TrackBar_AttackRange.OnChange := Attack_Change;

    Button_AttackOk := TKMButton.Create(Panel_Attack, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
    Button_AttackOk.OnClick := Attack_Close;
    Button_AttackCancel := TKMButton.Create(Panel_Attack, SIZE_X-20-160, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
    Button_AttackCancel.OnClick := Attack_Close;
end;


procedure TKMMapEdAttack.Attack_Change(Sender: TObject);
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


procedure TKMMapEdAttack.Attack_Close(Sender: TObject);
var
  AA: TAIAttack;
  GT: TGroupType;
begin
  if Sender = Button_AttackOk then
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

    gPlayers[fOwner].AI.General.Attacks[fIndex] := AA;
  end;

  Panel_Attack.Hide;

  fOnDone(Self);
end;


procedure TKMMapEdAttack.Attack_Refresh(aAttack: TAIAttack);
var
  GT: TGroupType;
begin
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


procedure TKMMapEdAttack.Show(aPlayer: TPlayerIndex; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Attack_Refresh(gPlayers[aPlayer].AI.General.Attacks[aIndex]);
  Panel_Attack.Show;
end;


end.
