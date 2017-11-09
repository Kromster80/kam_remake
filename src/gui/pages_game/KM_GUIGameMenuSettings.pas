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
    procedure UpdateControlsPosition;
  protected
    Panel_Settings: TKMPanel;
      CheckBox_Settings_Autosave: TKMCheckBox;
      CheckBox_Settings_ReplayAutopause: TKMCheckBox;
      CheckBox_Settings_ReplaySpecShowBeacons: TKMCheckBox;
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
    procedure UpdateView;
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
var
  TopPos: Integer;
begin
  inherited Create;

  Panel_Settings := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 412);
    TopPos := 15;
    CheckBox_Settings_Autosave := TKMCheckBox.Create(Panel_Settings,PAD,TopPos,WID,20,gResTexts[TX_MENU_OPTIONS_AUTOSAVE],fnt_Metal);
    CheckBox_Settings_Autosave.OnClick := Menu_Settings_Change;
    Inc(TopPos, 25);
    CheckBox_Settings_ReplayAutoPause := TKMCheckBox.Create(Panel_Settings,PAD,TopPos,WID,20,'Replay|autopause',fnt_Metal); //Todo: translate
    CheckBox_Settings_ReplayAutoPause.Hint := 'Automatically pause replay when peacetime ends'; //Todo: translate
    CheckBox_Settings_ReplayAutoPause.OnClick := Menu_Settings_Change;
    Inc(TopPos, 40);
    CheckBox_Settings_ReplaySpecShowBeacons := TKMCheckBox.Create(Panel_Settings,PAD,TopPos,WID,20,'Show beacons',fnt_Metal); //Todo: translate
    CheckBox_Settings_ReplaySpecShowBeacons.Hint := 'Show players beacons'; //Todo: translate
    CheckBox_Settings_ReplaySpecShowBeacons.OnClick := Menu_Settings_Change;
    Inc(TopPos, 25);
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,PAD,TopPos,WID,0,20);
    TrackBar_Settings_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    Inc(TopPos, 55);
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,PAD,TopPos,WID,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    Inc(TopPos, 55);
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,PAD,TopPos,WID,0,20);
    TrackBar_Settings_SFX.Caption := gResTexts[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := gResTexts[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    Inc(TopPos, 55);
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,PAD,TopPos,WID,0,20);
    TrackBar_Settings_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := gResTexts[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    Inc(TopPos, 55);
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,PAD,TopPos,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    Inc(TopPos, 25);
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,PAD,TopPos,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


procedure TKMGameMenuSettings.UpdateView;
begin
  CheckBox_Settings_ReplayAutoPause.Enabled := (gGame.GameMode = gmReplayMulti) and gGame.IsPeaceTime;
end;


procedure TKMGameMenuSettings.UpdateControlsPosition;
var Top: Integer;
begin
  Top := 15;
  if gGame.IsReplay then
    CheckBox_Settings_Autosave.Hide
  else begin
    CheckBox_Settings_Autosave.Show;
    Inc(Top, 25);
  end;

  if gGame.GameMode = gmReplayMulti then
  begin
    CheckBox_Settings_ReplayAutoPause.Top := Top;
    CheckBox_Settings_ReplayAutoPause.Show;
    Inc(Top, 40);
  end else
    CheckBox_Settings_ReplayAutoPause.Hide;

  if gGame.GameMode in [gmReplaySingle, gmReplayMulti, gmMultiSpectate] then
  begin
    CheckBox_Settings_ReplaySpecShowBeacons.Top := Top;
    CheckBox_Settings_ReplaySpecShowBeacons.Show;
    Inc(Top, 25);
  end else
    CheckBox_Settings_ReplaySpecShowBeacons.Hide;

  TrackBar_Settings_Brightness.Top := Top;
  Inc(Top, 55);
  TrackBar_Settings_ScrollSpeed.Top := Top;
  Inc(Top, 55);
  TrackBar_Settings_SFX.Top := Top;
  Inc(Top, 55);
  TrackBar_Settings_Music.Top := Top;
  Inc(Top, 55);
  CheckBox_Settings_MusicOff.Top := Top;
  Inc(Top, 25);
  CheckBox_Settings_ShuffleOn.Top := Top;

  Panel_Settings.Height := CheckBox_Settings_ShuffleOn.Top + CheckBox_Settings_ShuffleOn.Height + 2;
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

  if gGame.IsReplay then
    CheckBox_Settings_ReplaySpecShowBeacons.Checked := gGameApp.GameSettings.ReplayShowBeacons
  else if gGame.GameMode = gmMultiSpectate then
    CheckBox_Settings_ReplaySpecShowBeacons.Checked := gGameApp.GameSettings.SpecShowBeacons;

  TrackBar_Settings_Music.Enabled           := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled       := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ReplayAutoPause.Enabled := (gGame.GameMode = gmReplayMulti) and gGame.IsPeaceTime;
  UpdateControlsPosition;
end;


procedure TKMGameMenuSettings.Menu_Settings_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled   := (gGameApp.GameSettings.MusicOff <> CheckBox_Settings_MusicOff.Checked);
  ShuffleToggled := (gGameApp.GameSettings.ShuffleOn <> CheckBox_Settings_ShuffleOn.Checked);

  gGameApp.GameSettings.Brightness            := TrackBar_Settings_Brightness.Position;
  gGameApp.GameSettings.Autosave              := CheckBox_Settings_Autosave.Checked;
  gGameApp.GameSettings.ReplayAutopause       := CheckBox_Settings_ReplayAutopause.Checked;
  gGameApp.GameSettings.ScrollSpeed           := TrackBar_Settings_ScrollSpeed.Position;
  gGameApp.GameSettings.SoundFXVolume         := TrackBar_Settings_SFX.Position / TrackBar_Settings_SFX.MaxValue;
  gGameApp.GameSettings.MusicVolume           := TrackBar_Settings_Music.Position / TrackBar_Settings_Music.MaxValue;
  gGameApp.GameSettings.MusicOff              := CheckBox_Settings_MusicOff.Checked;
  gGameApp.GameSettings.ShuffleOn             := CheckBox_Settings_ShuffleOn.Checked;

  if gGame.IsReplay then
    gGameApp.GameSettings.ReplayShowBeacons   := CheckBox_Settings_ReplaySpecShowBeacons.Checked
  else if gGame.GameMode = gmMultiSpectate then
    gGameApp.GameSettings.SpecShowBeacons   := CheckBox_Settings_ReplaySpecShowBeacons.Checked;

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
