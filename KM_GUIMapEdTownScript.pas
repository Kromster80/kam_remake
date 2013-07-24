unit KM_GUIMapEdTownScript;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
   KM_Controls, KM_Defaults, KM_Pics, KM_Maps, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_Points, KM_InterfaceDefaults, KM_AIAttacks, KM_AIGoals, KM_Terrain;

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
  public
    constructor Create(aParent: TKMPanel);

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
  TrackBar_WorkerCount := TKMTrackBar.Create(Panel_Script, 0, 110, TB_WIDTH, 0, 30);
  TrackBar_WorkerCount.Caption := gResTexts[TX_MAPED_AI_WORKERS];
  TrackBar_WorkerCount.OnChange := Town_ScriptChange;
end;


procedure TKMMapEdTownScript.Town_ScriptRefresh;
begin
  CheckBox_AutoBuild.Checked := gPlayers[MySpectator.PlayerIndex].AI.Setup.AutoBuild;
  CheckBox_AutoRepair.Checked := gPlayers[MySpectator.PlayerIndex].AI.Mayor.AutoRepair;
  TrackBar_SerfsPer10Houses.Position := Round(10*gPlayers[MySpectator.PlayerIndex].AI.Setup.SerfsPerHouse);
  TrackBar_WorkerCount.Position := gPlayers[MySpectator.PlayerIndex].AI.Setup.WorkerCount;
end;


procedure TKMMapEdTownScript.Town_ScriptChange(Sender: TObject);
begin
  gPlayers[MySpectator.PlayerIndex].AI.Setup.AutoBuild := CheckBox_AutoBuild.Checked;
  gPlayers[MySpectator.PlayerIndex].AI.Mayor.AutoRepair := CheckBox_AutoRepair.Checked;
  gPlayers[MySpectator.PlayerIndex].AI.Setup.SerfsPerHouse := TrackBar_SerfsPer10Houses.Position / 10;
  gPlayers[MySpectator.PlayerIndex].AI.Setup.WorkerCount := TrackBar_WorkerCount.Position;
end;


procedure TKMMapEdTownScript.Hide;
begin
  Panel_Script.Hide;
end;


procedure TKMMapEdTownScript.Show;
begin
  Town_ScriptRefresh;
  Panel_Script.Show;
end;


function TKMMapEdTownScript.Visible: Boolean;
begin
  Result := Panel_Script.Visible;
end;


end.
