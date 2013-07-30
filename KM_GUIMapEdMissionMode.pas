unit KM_GUIMapEdMissionMode;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_Defaults, KM_Pics, KM_InterfaceDefaults;

type
  TKMMapEdMissionMode = class
  private
    procedure Mission_ModeChange(Sender: TObject);
    procedure Mission_ModeUpdate;
  protected
    Panel_Mode: TKMPanel;
    Radio_MissionMode: TKMRadioGroup;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_Game,
  KM_RenderUI, KM_ResFonts;


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
end;


procedure TKMMapEdMissionMode.Mission_ModeChange(Sender: TObject);
begin
  fGame.MissionMode := TKMissionMode(Radio_MissionMode.ItemIndex);
end;


procedure TKMMapEdMissionMode.Mission_ModeUpdate;
begin
  Radio_MissionMode.ItemIndex := Byte(fGame.MissionMode);
end;


procedure TKMMapEdMissionMode.Hide;
begin
  Panel_Mode.Hide;
end;


procedure TKMMapEdMissionMode.Show;
begin
  Mission_ModeUpdate;
  Panel_Mode.Show;
end;


function TKMMapEdMissionMode.Visible: Boolean;
begin
  Result := Panel_Mode.Visible;
end;


end.
