unit KM_GUIMapEdTownOffence;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, SysUtils,
   KM_Controls,
   KM_Points, KM_AIAttacks, KM_GUIMapEdTownAttackPopUp;

type
  TKMMapEdTownOffence = class
  private
    fAttackPopUp: TKMMapEdTownAttack;
    procedure Attacks_Add(Sender: TObject);
    procedure Attacks_Del(Sender: TObject);
    procedure Attacks_Edit(aIndex: Integer);
    procedure Attacks_ListClick(Sender: TObject);
    procedure Attacks_ListDoubleClick(Sender: TObject);
    procedure Attacks_OnDone(Sender: TObject);
    procedure Attacks_Refresh;
    procedure AutoAttackClick(Sender: TObject);
    procedure SetAttackPopUp(aValue: TKMMapEdTownAttack);
  protected
    Panel_Offence: TKMPanel;
    CheckBox_AutoAttack: TKMCheckBox;
    ColumnBox_Attacks: TKMColumnBox;
    Button_AttacksAdd: TKMButton;
    Button_AttacksDel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    property AttackPopUp: TKMMapEdTownAttack read fAttackPopUp write SetAttackPopUp;

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_InterfaceGame, KM_Hand;


{ TKMMapEdTownOffence }
constructor TKMMapEdTownOffence.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Offence := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Offence, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_ATTACK], fnt_Outline, taCenter);

  CheckBox_AutoAttack := TKMCheckBox.Create(Panel_Offence, 0, 24, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_ATTACK_AUTO], fnt_Metal);
  CheckBox_AutoAttack.Hint := gResTexts[TX_MAPED_AI_ATTACK_AUTO_HINT];
  CheckBox_AutoAttack.OnClick := AutoAttackClick;

  ColumnBox_Attacks := TKMColumnBox.Create(Panel_Offence, 0, 50, TB_WIDTH, 210, fnt_Game, bsGame);
  ColumnBox_Attacks.SetColumns(fnt_Outline,
    [gResTexts[TX_MAPED_AI_ATTACK_COL_TYPE],
     gResTexts[TX_MAPED_AI_ATTACK_COL_DELAY],
     gResTexts[TX_MAPED_AI_ATTACK_COL_MEN],
     gResTexts[TX_MAPED_AI_ATTACK_COL_TARGET],
     gResTexts[TX_MAPED_AI_ATTACK_COL_LOC]], [0, 20, 60, 100, 130]);
  ColumnBox_Attacks.OnClick := Attacks_ListClick;
  ColumnBox_Attacks.OnDoubleClick := Attacks_ListDoubleClick;

  Button_AttacksAdd := TKMButton.Create(Panel_Offence, 0, 270, 25, 25, '+', bsGame);
  Button_AttacksAdd.OnClick := Attacks_Add;
  Button_AttacksDel := TKMButton.Create(Panel_Offence, 30, 270, 25, 25, 'X', bsGame);
  Button_AttacksDel.OnClick := Attacks_Del;
end;


//Add a dummy attack and let mapmaker edit it
procedure TKMMapEdTownOffence.Attacks_Add(Sender: TObject);
var
  AA: TAIAttack;
begin
  FillChar(AA, SizeOf(AA), #0);
  gMySpectator.Hand.AI.General.Attacks.AddAttack(AA);

  Attacks_Refresh;
  ColumnBox_Attacks.ItemIndex := gMySpectator.Hand.AI.General.Attacks.Count - 1;

  //Edit the attack we have just appended
  Attacks_Edit(ColumnBox_Attacks.ItemIndex);
end;


procedure TKMMapEdTownOffence.Attacks_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  if InRange(I, 0, gMySpectator.Hand.AI.General.Attacks.Count - 1) then
    gMySpectator.Hand.AI.General.Attacks.Delete(I);

  Attacks_Refresh;
end;


procedure TKMMapEdTownOffence.Attacks_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, gMySpectator.Hand.AI.General.Attacks.Count - 1));
  AttackPopUp.Show(gMySpectator.HandIndex, aIndex);
end;


procedure TKMMapEdTownOffence.Attacks_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  Button_AttacksDel.Enabled := InRange(I, 0, gMySpectator.Hand.AI.General.Attacks.Count - 1);
end;


procedure TKMMapEdTownOffence.Attacks_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, gMySpectator.Hand.AI.General.Attacks.Count - 1) then
    Attacks_Edit(I);
end;


procedure TKMMapEdTownOffence.Attacks_OnDone(Sender: TObject);
begin
  Attacks_Refresh;
end;


procedure TKMMapEdTownOffence.Attacks_Refresh;
const
  Typ: array [TAIAttackType] of string = ('O', 'R');
  Tgt: array [TAIAttackTarget] of string = ('U', 'HA', 'HS', 'Pos');
var
  I: Integer;
  A: TAIAttack;
begin
  ColumnBox_Attacks.Clear;

  for I := 0 to gMySpectator.Hand.AI.General.Attacks.Count - 1 do
  begin
    A := gMySpectator.Hand.AI.General.Attacks[I];
    ColumnBox_Attacks.AddItem(MakeListRow([Typ[A.AttackType], IntToStr(A.Delay div 10), IntToStr(A.TotalMen), Tgt[A.Target], TypeToString(A.CustomPosition)]));
  end;

  Attacks_ListClick(nil);

  CheckBox_AutoAttack.Checked := gMySpectator.Hand.AI.Setup.AutoAttack;
end;


procedure TKMMapEdTownOffence.AutoAttackClick(Sender: TObject);
begin
  gMySpectator.Hand.AI.Setup.AutoAttack := CheckBox_AutoAttack.Checked;
end;


procedure TKMMapEdTownOffence.Hide;
begin
  Panel_Offence.Hide;
end;


procedure TKMMapEdTownOffence.SetAttackPopUp(aValue: TKMMapEdTownAttack);
begin
  fAttackPopUp := aValue;
  fAttackPopUp.fOnDone := Attacks_OnDone;
end;


procedure TKMMapEdTownOffence.Show;
begin
  Attacks_Refresh;
  Panel_Offence.Show;
end;


function TKMMapEdTownOffence.Visible: Boolean;
begin
  Result := Panel_Offence.Visible;
end;


end.
