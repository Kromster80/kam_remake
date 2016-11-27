unit KM_GUIGameMenuSettings;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Defaults;

type
  TKMGameMenuSettings = class
  private
    procedure Menu_Settings_Change(Sender: TObject);
  protected
    Panel_Settings: TKMPanel;
      CheckBox_Settings_Autosave: TKMCheckBox;
      CheckBox_Settings_ReplayAutopause: TKMCheckBox;
      TrackBar_Settings_Brightness: TKMTrackBar;
      TrackBar_Settings_SFX: TKMTrackBar;
      TrackBar_Settings_Music: TKMTrackBar;
      TrackBar_Settings_ScrollSpeed: TKMTrackBar;
      CheckBox_Settings_MusicOff: TKMCheckBox;
      CheckBox_Settings_ShuffleOn: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure Menu_Settings_Fill;
    procedure SetAutosaveEnabled(aEnabled: Boolean);
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_GameApp, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_InterfaceGame, KM_Sound, KM_Game;


{ TKMMapEdMenuQuit }
constructor TKMGameMenuSettings.Create(aParent: TKMPanel);
const
  PAD = 10;
  WID = TB_WIDTH - PAD * 2;
begin
  inherited Create;

  Panel_Settings := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 332);
    CheckBox_Settings_Autosave := TKMCheckBox.Create(Panel_Settings,PAD,15,WID,20,gResTexts[TX_MENU_OPTIONS_AUTOSAVE],fnt_Metal);
    CheckBox_Settings_Autosave.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ReplayAutoPause := TKMCheckBox.Create(Panel_Settings,PAD,40,WID,20,'Replay autopause',fnt_Metal); //TODO translate
    CheckBox_Settings_ReplayAutoPause.OnClick := Menu_Settings_Change;
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,PAD,65,WID,0,20);
    TrackBar_Settings_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,PAD,120,WID,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,PAD,175,WID,0,20);
    TrackBar_Settings_SFX.Caption := gResTexts[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := gResTexts[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,PAD,230,WID,0,20);
    TrackBar_Settings_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := gResTexts[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,PAD,285,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,PAD,310,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


procedure TKMGameMenuSettings.Menu_Settings_Fill;
begin
  TrackBar_Settings_Brightness.Position     := gGameApp.GameSettings.Brightness;
  CheckBox_Settings_Autosave.Checked        := gGameApp.GameSettings.Autosave;
  CheckBox_Settings_ReplayAutoPause.Checked := gGameApp.GameSettings.ReplayAutopause;
  TrackBar_Settings_ScrollSpeed.Position    := gGameApp.GameSettings.ScrollSpeed;
  TrackBar_Settings_SFX.Position            := Round(gGameApp.GameSettings.SoundFXVolume * TrackBar_Settings_SFX.MaxValue);
  TrackBar_Settings_Music.Position          := Round(gGameApp.GameSettings.MusicVolume * TrackBar_Settings_Music.MaxValue);
  CheckBox_Settings_MusicOff.Checked        := gGameApp.GameSettings.MusicOff;
  CheckBox_Settings_ShuffleOn.Checked       := gGameApp.GameSettings.ShuffleOn;

  TrackBar_Settings_Music.Enabled           := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled       := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ReplayAutoPause.Enabled := gGame.GameMode = gmReplayMulti;

end;


procedure TKMGameMenuSettings.Menu_Settings_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled   := (gGameApp.GameSettings.MusicOff <> CheckBox_Settings_MusicOff.Checked);
  ShuffleToggled := (gGameApp.GameSettings.ShuffleOn <> CheckBox_Settings_ShuffleOn.Checked);

  gGameApp.GameSettings.Brightness      := TrackBar_Settings_Brightness.Position;
  gGameApp.GameSettings.Autosave        := CheckBox_Settings_ReplayAutopause.Checked;
  gGameApp.GameSettings.ReplayAutopause := CheckBox_Settings_Autosave.Checked;
  gGameApp.GameSettings.ScrollSpeed     := TrackBar_Settings_ScrollSpeed.Position;
  gGameApp.GameSettings.SoundFXVolume   := TrackBar_Settings_SFX.Position / TrackBar_Settings_SFX.MaxValue;
  gGameApp.GameSettings.MusicVolume     := TrackBar_Settings_Music.Position / TrackBar_Settings_Music.MaxValue;
  gGameApp.GameSettings.MusicOff        := CheckBox_Settings_MusicOff.Checked;
  gGameApp.GameSettings.ShuffleOn       := CheckBox_Settings_ShuffleOn.Checked;

  gSoundPlayer.UpdateSoundVolume(gGameApp.GameSettings.SoundFXVolume);
  gGameApp.MusicLib.UpdateMusicVolume(gGameApp.GameSettings.MusicVolume);
  if MusicToggled then
  begin
    gGameApp.MusicLib.ToggleMusic(not gGameApp.GameSettings.MusicOff);
    if not gGameApp.GameSettings.MusicOff then
      ShuffleToggled := True; //Re-shuffle songs if music has been enabled
  end;
  if ShuffleToggled then
    gGameApp.MusicLib.ToggleShuffle(gGameApp.GameSettings.ShuffleOn);

  TrackBar_Settings_Music.Enabled := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


procedure TKMGameMenuSettings.Hide;
begin
  Panel_Settings.Hide;
end;


procedure TKMGameMenuSettings.SetAutosaveEnabled(aEnabled: Boolean);
begin
  CheckBox_Settings_Autosave.Enabled := aEnabled;
end;


procedure TKMGameMenuSettings.Show;
begin
  Panel_Settings.Show;
end;


function TKMGameMenuSettings.Visible: Boolean;
begin
  Result := Panel_Settings.Visible;
end;


end.
