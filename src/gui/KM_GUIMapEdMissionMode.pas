unit KM_GUIMapEdMissionMode;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_Defaults;

type
  TKMMapEdMissionMode = class
  private
    procedure Mission_ModeChange(Sender: TObject);
    procedure Mission_ModeUpdate;
    procedure AIBuilderChange(Sender: TObject);
  protected
    Panel_Mode: TKMPanel;
    Radio_MissionMode: TKMRadioGroup;
    Button_AIBuilderSetup: TKMButton;
    Button_AIBuilderWarn: TKMLabel;
    Button_AIBuilderOK, Button_AIBuilderCancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_RenderUI, KM_ResFonts, KM_InterfaceGame, KM_HandsCollection, KM_Hand;


{ TKMMapEdMissionMode }
constructor TKMMapEdMissionMode.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Mode := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Mode, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_MISSION_MODE], fnt_Outline, taCenter);
  Radio_MissionMode := TKMRadioGroup.Create(Panel_Mode, 0, 30, TB_WIDTH, 40, fnt_Metal);
  Radio_MissionMode.Add(gResTexts[TX_MAPED_MISSION_NORMAL]);
  Radio_MissionMode.Add(gResTexts[TX_MAPED_MISSION_TACTIC]);
  Radio_MissionMode.OnChange := Mission_ModeChange;

  TKMLabel.Create(Panel_Mode, 0, 90, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_DEFAULTS_HEADING], fnt_Outline, taCenter);
  Button_AIBuilderSetup := TKMButton.Create(Panel_Mode, 0, 120, TB_WIDTH, 30, gResTexts[TX_MAPED_AI_DEFAULTS_MP_BUILDER], bsGame);
  Button_AIBuilderSetup.Hint := gResTexts[TX_MAPED_AI_DEFAULTS_MP_BUILDER_HINT];
  Button_AIBuilderSetup.OnClick := AIBuilderChange;

  Button_AIBuilderWarn := TKMLabel.Create(Panel_Mode, 0, 110, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_DEFAULTS_CONFIRM], fnt_Grey, taLeft);
  Button_AIBuilderWarn.AutoWrap := True;
  Button_AIBuilderWarn.Hide;
  Button_AIBuilderOK := TKMButton.Create(Panel_Mode, 0, 200, 88, 20, gResTexts[TX_MAPED_OK], bsGame);
  Button_AIBuilderOK.OnClick := AIBuilderChange;
  Button_AIBuilderOK.Hide;
  Button_AIBuilderCancel := TKMButton.Create(Panel_Mode, 92, 200, 88, 20, gResTexts[TX_MAPED_CANCEL], bsGame);
  Button_AIBuilderCancel.OnClick := AIBuilderChange;
  Button_AIBuilderCancel.Hide;
end;


procedure TKMMapEdMissionMode.Mission_ModeChange(Sender: TObject);
begin
  gGame.MissionMode := TKMissionMode(Radio_MissionMode.ItemIndex);
end;


procedure TKMMapEdMissionMode.AIBuilderChange(Sender: TObject);
var I: Integer;
begin
  if Sender = Button_AIBuilderSetup then
  begin
    Button_AIBuilderOK.Show;
    Button_AIBuilderCancel.Show;
    Button_AIBuilderWarn.Show;
    Button_AIBuilderSetup.Hide;
  end;

  if Sender = Button_AIBuilderOK then
    for I := 0 to gHands.Count-1 do
    begin
      gGame.MapEditor.PlayerAI[I] := True;
      gHands[I].AI.General.DefencePositions.Clear;
      gHands[I].AI.General.Attacks.Clear;
      gHands[I].AI.Setup.ApplyAgressiveBuilderSetup;
    end;

  if (Sender = Button_AIBuilderOK) or (Sender = Button_AIBuilderCancel) then
  begin
    Button_AIBuilderOK.Hide;
    Button_AIBuilderCancel.Hide;
    Button_AIBuilderWarn.Hide;
    Button_AIBuilderSetup.Show;
  end;
end;


procedure TKMMapEdMissionMode.Mission_ModeUpdate;
begin
  Radio_MissionMode.ItemIndex := Byte(gGame.MissionMode);
end;


procedure TKMMapEdMissionMode.Hide;
begin
  Panel_Mode.Hide;
end;


procedure TKMMapEdMissionMode.Show;
begin
  Mission_ModeUpdate;
  Panel_Mode.Show;
  AIBuilderChange(Button_AIBuilderCancel); //Hide confirmation
end;


function TKMMapEdMissionMode.Visible: Boolean;
begin
  Result := Panel_Mode.Visible;
end;


end.
