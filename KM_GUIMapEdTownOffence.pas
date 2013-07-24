unit KM_GUIMapEdTownOffence;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
   KM_Controls, KM_Defaults, KM_Pics, KM_Maps, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_Points, KM_InterfaceDefaults, KM_AIAttacks, KM_AIGoals, KM_Terrain,
   KM_GUIMapEdAttack;

type
  TKMMapEdTownOffence = class
  private
    fAttackPopUp: TKMMapEdAttack;
    procedure Attacks_Add(Sender: TObject);
    procedure Attacks_Del(Sender: TObject);
    procedure Attacks_Edit(aIndex: Integer);
    procedure Attacks_ListClick(Sender: TObject);
    procedure Attacks_ListDoubleClick(Sender: TObject);
    procedure Attacks_OnDone(Sender: TObject);
    procedure Attacks_Refresh;
    procedure SetAttackPopUp(aValue: TKMMapEdAttack);
  protected
    Panel_Offence: TKMPanel;
    CheckBox_AutoAttack: TKMCheckBox;
    ColumnBox_Attacks: TKMColumnBox;
    Button_AttacksAdd: TKMButton;
    Button_AttacksDel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    property AttackPopUp: TKMMapEdAttack read fAttackPopUp write SetAttackPopUp;

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_CommonClasses, KM_PlayersCollection, KM_ResTexts, KM_Game, KM_Main, KM_GameCursor,
  KM_GameApp, KM_Resource, KM_TerrainDeposits, KM_ResCursors, KM_Utils,
  KM_AIDefensePos, KM_ResHouses, KM_RenderUI, KM_Sound, KM_ResSound,
  KM_ResWares, KM_ResFonts;


{ TKMMapEdTownOffence }
constructor TKMMapEdTownOffence.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Offence := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Offence, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_ATTACK], fnt_Outline, taCenter);

  CheckBox_AutoAttack := TKMCheckBox.Create(Panel_Offence, 0, 30, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_ATTACK_AUTO], fnt_Metal);
  CheckBox_AutoAttack.Disable;

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
  gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.AddAttack(AA);

  Attacks_Refresh;
  ColumnBox_Attacks.ItemIndex := gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1;

  //Edit the attack we have just appended
  Attacks_Edit(ColumnBox_Attacks.ItemIndex);
end;


procedure TKMMapEdTownOffence.Attacks_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  if InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1) then
    gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Delete(I);

  Attacks_Refresh;
end;


procedure TKMMapEdTownOffence.Attacks_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1));
  AttackPopUp.Show(MySpectator.PlayerIndex, aIndex);
end;


procedure TKMMapEdTownOffence.Attacks_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  Button_AttacksDel.Enabled := InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1);
end;


procedure TKMMapEdTownOffence.Attacks_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1) then
    Attacks_Edit(I);
end;


procedure TKMMapEdTownOffence.Attacks_OnDone(Sender: TObject);
begin
  Attacks_Refresh;
end;


procedure TKMMapEdTownOffence.Attacks_Refresh;
const
  Typ: array [TAIAttackType] of string = ('O', 'R');
  Tgt: array [TAIAttackTarget] of string = ('U', 'H1', 'H2', 'Pos');
var
  I: Integer;
  A: TAIAttack;
begin
  ColumnBox_Attacks.Clear;

  for I := 0 to gPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1 do
  begin
    A := gPlayers[MySpectator.PlayerIndex].AI.General.Attacks[I];
    ColumnBox_Attacks.AddItem(MakeListRow([Typ[A.AttackType], IntToStr(A.Delay div 10), IntToStr(A.TotalMen), Tgt[A.Target], TypeToString(A.CustomPosition)]));
  end;

  Attacks_ListClick(nil);
end;


procedure TKMMapEdTownOffence.Hide;
begin
  Panel_Offence.Hide;
end;


procedure TKMMapEdTownOffence.SetAttackPopUp(aValue: TKMMapEdAttack);
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
