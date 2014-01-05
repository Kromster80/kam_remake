unit KM_GUIMapEdTownDefence;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_GUIMapEdFormations;


type
  TKMMapEdTownDefence = class
  private
    procedure Town_DefenceFormations(Sender: TObject);
    procedure Town_DefenceAddClick(Sender: TObject);
    procedure Town_DefenceRefresh;
    procedure Town_DefenceChange(Sender: TObject);
  protected
    Panel_Defence: TKMPanel;
    Button_DefencePosAdd: TKMButtonFlat;
    CheckBox_AutoDefence: TKMCheckBox;
    CheckBox_DefendAllies: TKMCheckBox;
    TrackBar_AutoAttackRange: TKMTrackBar;
    TrackBar_RecruitCount: TKMTrackBar;
    TrackBar_MaxSoldiers: TKMTrackBar;
    CheckBox_MaxSoldiers: TKMCheckBox;
    TrackBar_RecruitDelay: TKMTrackBar;
    Button_EditFormations: TKMButton;
  public
    FormationsPopUp: TKMMapEdFormations;

    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_GameCursor, KM_RenderUI, KM_ResFonts, KM_InterfaceGame;


{ TKMMapEdTownDefence }
constructor TKMMapEdTownDefence.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Defence := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Defence, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_DEFENSE], fnt_Outline, taCenter);
  Button_DefencePosAdd := TKMButtonFlat.Create(Panel_Defence, 0, 30, 33, 33, 338);
  Button_DefencePosAdd.OnClick := Town_DefenceAddClick;
  Button_DefencePosAdd.Hint    := gResTexts[TX_MAPED_AI_DEFENSE_HINT];

  TKMLabel.Create(Panel_Defence, 0, 65, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_DEFENSE_OPTIONS], fnt_Outline, taCenter);
  CheckBox_AutoDefence := TKMCheckBox.Create(Panel_Defence, 0, 90, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_DEFENSE_AUTO], fnt_Metal);
  CheckBox_AutoDefence.Hint := gResTexts[TX_MAPED_AI_DEFENSE_AUTO_HINT];
  CheckBox_AutoDefence.OnClick := Town_DefenceChange;

  CheckBox_DefendAllies := TKMCheckBox.Create(Panel_Defence, 0, 110, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_DEFEND_ALLIES], fnt_Metal);
  CheckBox_DefendAllies.Hint := gResTexts[TX_MAPED_AI_DEFEND_ALLIES_HINT];
  CheckBox_DefendAllies.OnClick := Town_DefenceChange;

  TrackBar_AutoAttackRange := TKMTrackBar.Create(Panel_Defence, 0, 136, TB_WIDTH, 1, 20);
  TrackBar_AutoAttackRange.Caption := gResTexts[TX_MAPED_AI_AUTO_ATTACK];
  TrackBar_AutoAttackRange.Hint := gResTexts[TX_MAPED_AI_AUTO_ATTACK_HINT];
  TrackBar_AutoAttackRange.OnChange := Town_DefenceChange;

  TrackBar_RecruitCount := TKMTrackBar.Create(Panel_Defence, 0, 186, TB_WIDTH, 1, 20);
  TrackBar_RecruitCount.Caption := gResTexts[TX_MAPED_AI_RECRUITS];
  TrackBar_RecruitCount.Hint := gResTexts[TX_MAPED_AI_RECRUITS_HINT];
  TrackBar_RecruitCount.OnChange := Town_DefenceChange;

  TrackBar_RecruitDelay := TKMTrackBar.Create(Panel_Defence, 0, 230, TB_WIDTH, 0, 500);
  TrackBar_RecruitDelay.Caption := gResTexts[TX_MAPED_AI_RECRUIT_DELAY];
  TrackBar_RecruitDelay.Hint := gResTexts[TX_MAPED_AI_RECRUIT_DELAY_HINT];
  TrackBar_RecruitDelay.Step := 5;
  TrackBar_RecruitDelay.OnChange := Town_DefenceChange;

  CheckBox_MaxSoldiers := TKMCheckBox.Create(Panel_Defence, 0, 274, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_MAX_SOLDIERS], fnt_Metal);
  CheckBox_MaxSoldiers.Hint := gResTexts[TX_MAPED_AI_MAX_SOLDIERS_ENABLE_HINT];
  CheckBox_MaxSoldiers.OnClick := Town_DefenceChange;
  TrackBar_MaxSoldiers := TKMTrackBar.Create(Panel_Defence, 20, 292, TB_WIDTH - 20, 0, 500);
  TrackBar_MaxSoldiers.Caption := '';
  TrackBar_MaxSoldiers.Hint := gResTexts[TX_MAPED_AI_MAX_SOLDIERS_HINT];
  TrackBar_MaxSoldiers.Step := 5;
  TrackBar_MaxSoldiers.OnChange := Town_DefenceChange;

  Button_EditFormations := TKMButton.Create(Panel_Defence, 0, 322, TB_WIDTH, 25, gResTexts[TX_MAPED_AI_FORMATIONS], bsGame);
  Button_EditFormations.OnClick := Town_DefenceFormations;
end;


procedure TKMMapEdTownDefence.Town_DefenceAddClick(Sender: TObject);
begin
  //Press the button
  Button_DefencePosAdd.Down := not Button_DefencePosAdd.Down and (Sender = Button_DefencePosAdd);

  if Button_DefencePosAdd.Down then
  begin
    GameCursor.Mode := cmMarkers;
    GameCursor.Tag1 := MARKER_DEFENCE;
  end
  else
  begin
    GameCursor.Mode := cmNone;
    GameCursor.Tag1 := 0;
  end;
end;


procedure TKMMapEdTownDefence.Town_DefenceChange(Sender: TObject);
begin
  gHands[MySpectator.HandIndex].AI.Setup.AutoDefend := CheckBox_AutoDefence.Checked;
  gHands[MySpectator.HandIndex].AI.Setup.DefendAllies := CheckBox_DefendAllies.Checked;
  gHands[MySpectator.HandIndex].AI.Setup.AutoAttackRange := TrackBar_AutoAttackRange.Position;
  gHands[MySpectator.HandIndex].AI.Setup.RecruitCount := TrackBar_RecruitCount.Position;
  gHands[MySpectator.HandIndex].AI.Setup.RecruitDelay := TrackBar_RecruitDelay.Position * 600;

  if not CheckBox_MaxSoldiers.Checked then
    gHands[MySpectator.HandIndex].AI.Setup.MaxSoldiers := -1
  else
    gHands[MySpectator.HandIndex].AI.Setup.MaxSoldiers := TrackBar_MaxSoldiers.Position;

  Town_DefenceRefresh;
end;


procedure TKMMapEdTownDefence.Town_DefenceFormations(Sender: TObject);
begin
  FormationsPopUp.Show(MySpectator.HandIndex);
end;


procedure TKMMapEdTownDefence.Town_DefenceRefresh;
begin
  CheckBox_AutoDefence.Checked := gHands[MySpectator.HandIndex].AI.Setup.AutoDefend;
  CheckBox_DefendAllies.Checked := gHands[MySpectator.HandIndex].AI.Setup.DefendAllies;
  TrackBar_AutoAttackRange.Position := gHands[MySpectator.HandIndex].AI.Setup.AutoAttackRange;
  TrackBar_RecruitCount.Position := gHands[MySpectator.HandIndex].AI.Setup.RecruitCount;
  TrackBar_RecruitDelay.Position := Round(gHands[MySpectator.HandIndex].AI.Setup.RecruitDelay / 600);

  CheckBox_MaxSoldiers.Checked := (gHands[MySpectator.HandIndex].AI.Setup.MaxSoldiers >= 0);
  TrackBar_MaxSoldiers.Enabled := CheckBox_MaxSoldiers.Checked;
  TrackBar_MaxSoldiers.Position := Max(gHands[MySpectator.HandIndex].AI.Setup.MaxSoldiers, 0);
end;


procedure TKMMapEdTownDefence.Hide;
begin
  Panel_Defence.Hide;
end;


procedure TKMMapEdTownDefence.Show;
begin
  Town_DefenceAddClick(nil);
  Town_DefenceRefresh;
  Panel_Defence.Show;
end;


function TKMMapEdTownDefence.Visible: Boolean;
begin
  Result := Panel_Defence.Visible;
end;


end.
