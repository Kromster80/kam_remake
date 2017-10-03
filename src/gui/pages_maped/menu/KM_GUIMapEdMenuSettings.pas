unit KM_GUIMapEdMenuSettings;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Defaults;

type
  TKMMapEdMenuSettings = class
  private
    procedure Menu_Settings_Change(Sender: TObject);
  protected
    Panel_Settings: TKMPanel;
      TrackBar_Settings_Brightness: TKMTrackBar;
      TrackBar_Settings_SFX: TKMTrackBar;
      TrackBar_Settings_Music: TKMTrackBar;
      TrackBar_Settings_ScrollSpeed: TKMTrackBar;
      CheckBox_Settings_MusicOff: TKMCheckBox;
      CheckBox_Settings_ShuffleOn: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure Menu_Settings_Fill;
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_GameApp, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_InterfaceGame, KM_Sound;


{ TKMMapEdMenuQuit }
constructor TKMMapEdMenuSettings.Create(aParent: TKMPanel);
const
  PAD = 10;
  WID = TB_WIDTH - PAD * 2;
begin
  inherited Create;

  Panel_Settings := TKMPanel.Create(aParent, 0, 44, TB_WIDTH, 332);
    TKMLabel.Create(Panel_Settings, 0, PAGE_TITLE_Y, TB_WIDTH, 30, gResTexts[TX_MENU_SETTINGS], fnt_Outline, taLeft);
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,PAD,40,WID,0,20);
    TrackBar_Settings_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,PAD,95,WID,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,PAD,150,WID,0,20);
    TrackBar_Settings_SFX.Caption := gResTexts[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := gResTexts[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,PAD,205,WID,0,20);
    TrackBar_Settings_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := gResTexts[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,PAD,260,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,PAD,285,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


procedure TKMMapEdMenuSettings.Menu_Settings_Fill;
begin
  TrackBar_Settings_Brightness.Position   := gGameApp.GameSettings.Brightness;
  TrackBar_Settings_ScrollSpeed.Position  := gGameApp.GameSettings.ScrollSpeed;
  TrackBar_Settings_SFX.Position          := Round(gGameApp.GameSettings.SoundFXVolume * TrackBar_Settings_SFX.MaxValue);
  TrackBar_Settings_Music.Position        := Round(gGameApp.GameSettings.MusicVolume * TrackBar_Settings_Music.MaxValue);
  CheckBox_Settings_MusicOff.Checked      := gGameApp.GameSettings.MusicOff;
  CheckBox_Settings_ShuffleOn.Checked     := gGameApp.GameSettings.ShuffleOn;

  TrackBar_Settings_Music.Enabled     := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


procedure TKMMapEdMenuSettings.Menu_Settings_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled   := (gGameApp.GameSettings.MusicOff <> CheckBox_Settings_MusicOff.Checked);
  ShuffleToggled := (gGameApp.GameSettings.ShuffleOn <> CheckBox_Settings_ShuffleOn.Checked);

  gGameApp.GameSettings.Brightness    := TrackBar_Settings_Brightness.Position;
  gGameApp.GameSettings.ScrollSpeed   := TrackBar_Settings_ScrollSpeed.Position;
  gGameApp.GameSettings.SoundFXVolume := TrackBar_Settings_SFX.Position / TrackBar_Settings_SFX.MaxValue;
  gGameApp.GameSettings.MusicVolume   := TrackBar_Settings_Music.Position / TrackBar_Settings_Music.MaxValue;
  gGameApp.GameSettings.MusicOff      := CheckBox_Settings_MusicOff.Checked;
  gGameApp.GameSettings.ShuffleOn     := CheckBox_Settings_ShuffleOn.Checked;

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


procedure TKMMapEdMenuSettings.Hide;
begin
  Panel_Settings.Hide;
end;


procedure TKMMapEdMenuSettings.Show;
begin
  Panel_Settings.Show;
end;


function TKMMapEdMenuSettings.Visible: Boolean;
begin
  Result := Panel_Settings.Visible;
end;


end.
