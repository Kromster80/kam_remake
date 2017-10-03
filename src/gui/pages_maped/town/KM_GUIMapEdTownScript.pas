unit KM_GUIMapEdTownScript;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils,
   KM_Controls;

type
  TKMMapEdTownScript = class
  private
    procedure Town_ScriptRefresh;
    procedure Town_ScriptChange(Sender: TObject);
  protected
    Panel_Script: TKMPanel;
    CheckBox_AutoBuild: TKMCheckBox;
    CheckBox_AutoRepair: TKMCheckBox;
    TrackBar_SerfsPer10Houses: TKMTrackBar;
    TrackBar_WorkerCount: TKMTrackBar;
    CheckBox_UnlimitedEquip: TKMCheckBox;
    DropBox_ArmyType: TKMDropList;
    TrackBar_EquipRateLeather: TKMTrackBar;
    TrackBar_EquipRateIron: TKMTrackBar;
    Button_AIStart: TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_InterfaceGame, KM_GameCursor,
  KM_Defaults, KM_Pics, KM_Hand, KM_ResHouses;


{ TKMMapEdTownScript }
constructor TKMMapEdTownScript.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Script := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Script, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_TITLE], fnt_Outline, taCenter);
  CheckBox_AutoBuild := TKMCheckBox.Create(Panel_Script, 0, 30, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_AUTOBUILD], fnt_Metal);
  CheckBox_AutoBuild.OnClick := Town_ScriptChange;
  CheckBox_AutoRepair := TKMCheckBox.Create(Panel_Script, 0, 50, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_AUTOREPAIR], fnt_Metal);
  CheckBox_AutoRepair.OnClick := Town_ScriptChange;
  TrackBar_SerfsPer10Houses := TKMTrackBar.Create(Panel_Script, 0, 70, TB_WIDTH, 1, 50);
  TrackBar_SerfsPer10Houses.Caption := gResTexts[TX_MAPED_AI_SERFS_PER_10_HOUSES];
  TrackBar_SerfsPer10Houses.OnChange := Town_ScriptChange;
  TrackBar_WorkerCount := TKMTrackBar.Create(Panel_Script, 0, 110, TB_WIDTH, 0, 50);
  TrackBar_WorkerCount.Caption := gResTexts[TX_MAPED_AI_WORKERS];
  TrackBar_WorkerCount.OnChange := Town_ScriptChange;

  TKMLabel.Create(Panel_Script, 0, 156, TB_WIDTH, 0, gResTexts[TX_MAPED_AI_ARMY_TYPE], fnt_Metal, taLeft);
  DropBox_ArmyType := TKMDropList.Create(Panel_Script, 0, 176, TB_WIDTH, 20, fnt_Metal, '', bsGame);
  DropBox_ArmyType.OnChange := Town_ScriptChange;
  DropBox_ArmyType.Add(gResTexts[TX_MAPED_AI_ARMY_TYPE_IRON_THEN_LEATHER], Byte(atIronThenLeather));
  DropBox_ArmyType.Add(gResTexts[TX_MAPED_AI_ARMY_TYPE_IRON],              Byte(atIron));
  DropBox_ArmyType.Add(gResTexts[TX_MAPED_AI_ARMY_TYPE_LEATHER],           Byte(atLeather));
  DropBox_ArmyType.Add(gResTexts[TX_MAPED_AI_ARMY_TYPE_MIXED],             Byte(atIronAndLeather));

  CheckBox_UnlimitedEquip := TKMCheckBox.Create(Panel_Script, 0, 200, TB_WIDTH, 20, gResTexts[TX_MAPED_AI_FASTEQUIP], fnt_Metal);
  CheckBox_UnlimitedEquip.OnClick := Town_ScriptChange;
  CheckBox_UnlimitedEquip.Hint := gResTexts[TX_MAPED_AI_FASTEQUIP_HINT];

  TrackBar_EquipRateLeather := TKMTrackBar.Create(Panel_Script, 0, 216, TB_WIDTH, 10, 300);
  TrackBar_EquipRateLeather.Caption := gResTexts[TX_MAPED_AI_DEFENSE_EQUIP_LEATHER];
  TrackBar_EquipRateLeather.Step := 5;
  TrackBar_EquipRateLeather.OnChange := Town_ScriptChange;

  TrackBar_EquipRateIron := TKMTrackBar.Create(Panel_Script, 0, 260, TB_WIDTH, 10, 300);
  TrackBar_EquipRateIron.Caption := gResTexts[TX_MAPED_AI_DEFENSE_EQUIP_IRON];
  TrackBar_EquipRateIron.Step := 5;
  TrackBar_EquipRateIron.OnChange := Town_ScriptChange;

  TKMLabel.Create(Panel_Script, 0, 310, gResTexts[TX_MAPED_AI_START], fnt_Metal, taLeft);
  Button_AIStart         := TKMButtonFlat.Create(Panel_Script, 0, 330, 33, 33, 62, rxGuiMain);
  Button_AIStart.Hint    := gResTexts[TX_MAPED_AI_START_HINT];
  Button_AIStart.OnClick := Town_ScriptChange;
end;


procedure TKMMapEdTownScript.Town_ScriptRefresh;
begin
  CheckBox_AutoBuild.Checked := gMySpectator.Hand.AI.Setup.AutoBuild;
  CheckBox_AutoRepair.Checked := gMySpectator.Hand.AI.Setup.AutoRepair;
  TrackBar_SerfsPer10Houses.Position := Round(10*gMySpectator.Hand.AI.Setup.SerfsPerHouse);
  if gMySpectator.HandIndex <> -1 then TrackBar_SerfsPer10Houses.Hint := Format(gResTexts[TX_MAPED_AI_SERFS_PER_10_HOUSES_HINT], [gMySpectator.Hand.Stats.GetHouseQty(ht_Any)]);
  TrackBar_WorkerCount.Position := gMySpectator.Hand.AI.Setup.WorkerCount;
  CheckBox_UnlimitedEquip.Checked := gMySpectator.Hand.AI.Setup.UnlimitedEquip;
  TrackBar_EquipRateLeather.Position := gMySpectator.Hand.AI.Setup.EquipRateLeather div 10;
  TrackBar_EquipRateIron.Position := gMySpectator.Hand.AI.Setup.EquipRateIron div 10;
  DropBox_ArmyType.SelectByTag(Byte(gMySpectator.Hand.AI.Setup.ArmyType));

  TrackBar_EquipRateLeather.Enable;
  TrackBar_EquipRateIron.Enable;
  case gMySpectator.Hand.AI.Setup.ArmyType of
    atLeather: TrackBar_EquipRateIron.Disable;
    atIron:    TrackBar_EquipRateLeather.Disable;
  end;
end;


procedure TKMMapEdTownScript.Town_ScriptChange(Sender: TObject);
begin
  gMySpectator.Hand.AI.Setup.AutoBuild := CheckBox_AutoBuild.Checked;
  gMySpectator.Hand.AI.Setup.AutoRepair := CheckBox_AutoRepair.Checked;
  gMySpectator.Hand.AI.Setup.SerfsPerHouse := TrackBar_SerfsPer10Houses.Position / 10;
  gMySpectator.Hand.AI.Setup.WorkerCount := TrackBar_WorkerCount.Position;
  gMySpectator.Hand.AI.Setup.UnlimitedEquip := CheckBox_UnlimitedEquip.Checked;
  gMySpectator.Hand.AI.Setup.EquipRateLeather := TrackBar_EquipRateLeather.Position * 10;
  gMySpectator.Hand.AI.Setup.EquipRateIron := TrackBar_EquipRateIron.Position * 10;
  gMySpectator.Hand.AI.Setup.ArmyType := TArmyType(DropBox_ArmyType.GetSelectedTag);

  TrackBar_EquipRateLeather.Enable;
  TrackBar_EquipRateIron.Enable;
  case gMySpectator.Hand.AI.Setup.ArmyType of
    atLeather: TrackBar_EquipRateIron.Disable;
    atIron:    TrackBar_EquipRateLeather.Disable;
  end;

  if Sender = Button_AIStart then
    Button_AIStart.Down := not Button_AIStart.Down;

  if Button_AIStart.Down then
  begin
    gGameCursor.Mode := cmMarkers;
    gGameCursor.Tag1 := MARKER_AISTART;
  end
  else
    gGameCursor.Mode := cmNone;
end;


procedure TKMMapEdTownScript.UpdateState;
begin
  Button_AIStart.Down := (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_AISTART);
end;


procedure TKMMapEdTownScript.Hide;
begin
  Panel_Script.Hide;
end;


procedure TKMMapEdTownScript.Show;
begin
  Button_AIStart.Down := False;
  Town_ScriptRefresh;
  Panel_Script.Show;
end;


function TKMMapEdTownScript.Visible: Boolean;
begin
  Result := Panel_Script.Visible;
end;


end.
