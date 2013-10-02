unit KM_GUIMenuOptions;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, SysUtils, KromOGLUtils,
  KM_Controls, KM_Defaults, KM_Settings, KM_Pics, KM_Resolutions,
  KM_InterfaceDefaults;


type
  TKMMenuOptions = class {(TKMGUIPage)}
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fMainSettings: TMainSettings;
    fGameSettings: TGameSettings;
    fResolutions: TKMResolutions;

    //We remember old values to enable/disable "Apply" button dynamicaly
    PrevResolutionId: TResIndex;
    //Try to pick the same refresh rate on resolution change
    DesiredRefRate: Integer;

    procedure ApplyResolution(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure ChangeResolution(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure FlagClick(Sender: TObject);
    procedure Refresh;
    procedure RefreshResolutions;
  protected
    Panel_Options: TKMPanel;
      Panel_Options_GFX: TKMPanel;
        TrackBar_Options_Brightness: TKMTrackBar;
        CheckBox_Options_VSync: TKMCheckBox;
        RadioGroup_Options_Shadows: TKMRadioGroup;
      Panel_Options_Ctrl: TKMPanel;
        TrackBar_Options_ScrollSpeed: TKMTrackBar;
      Panel_Options_Game: TKMPanel;
        CheckBox_Options_Autosave: TKMCheckBox;
      Panel_Options_Sound: TKMPanel;
        Label_Options_MusicOff: TKMLabel;
        TrackBar_Options_SFX,TrackBar_Options_Music: TKMTrackBar;
        CheckBox_Options_MusicOff: TKMCheckBox;
        CheckBox_Options_ShuffleOn: TKMCheckBox;
      Panel_Options_Lang: TKMPanel;
        Radio_Options_Lang: TKMRadioGroup;
        Image_Options_Lang_Flags: array of TKMImage;
      Panel_Options_Res: TKMPanel;
        CheckBox_Options_FullScreen: TKMCheckBox;
        DropBox_Options_Resolution: TKMDropList;
        DropBox_Options_RefreshRate: TKMDropList;
        Button_Options_ResApply: TKMButton;
      Button_Options_Back: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show;
  end;


implementation
uses KM_Main, KM_ResTexts, KM_GameApp, KM_ResLocales, KM_Sound, KM_RenderUI, KM_ResFonts;


{ TKMGUIMainOptions }
constructor TKMMenuOptions.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  I: Integer;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  //We cant pass pointers to Settings in here cos on GUI creation fMain/fGameApp are not initialized yet

  Panel_Options := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Options.AnchorsStretch;
    with TKMImage.Create(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,rxGuiMain) do
    begin
      ImageStretch;
      Anchors := [akLeft];
    end;

    //Controls section
    Panel_Options_Ctrl:=TKMPanel.Create(Panel_Options,120,130,220,80);
    Panel_Options_Ctrl.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Ctrl,6,0,288,20,gResTexts[TX_MENU_OPTIONS_CONTROLS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Ctrl,0,20,220,60);

      TrackBar_Options_ScrollSpeed := TKMTrackBar.Create(Panel_Options_Ctrl,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
      TrackBar_Options_ScrollSpeed.OnChange := Change;

    //Gameplay section
    Panel_Options_Game:=TKMPanel.Create(Panel_Options,120,230,220,50);
    Panel_Options_Game.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Game,6,0,188,20,gResTexts[TX_MENU_OPTIONS_GAMEPLAY],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Game,0,20,220,30);

      CheckBox_Options_Autosave := TKMCheckBox.Create(Panel_Options_Game,12,27,196,20,gResTexts[TX_MENU_OPTIONS_AUTOSAVE], fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Change;

    //Graphics section
    Panel_Options_GFX:=TKMPanel.Create(Panel_Options,360,300,220,178);
    Panel_Options_GFX.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_GFX,6,0,188,20,gResTexts[TX_MENU_OPTIONS_GRAPHICS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_GFX,0,20,220,158);
      TrackBar_Options_Brightness:=TKMTrackBar.Create(Panel_Options_GFX,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
      TrackBar_Options_Brightness.OnChange:=Change;
      CheckBox_Options_VSync := TKMCheckBox.Create(Panel_Options_GFX, 10, 90, 200, 20, gResTexts[TX_MENU_OPTIONS_VSYNC], fnt_Metal);
      CheckBox_Options_VSync.OnClick := Change;
      TKMLabel.Create(Panel_Options_GFX,10,120,200,20,gResTexts[TX_MENU_OPTIONS_SHADOW_QUALITY],fnt_Metal,taLeft);
      RadioGroup_Options_Shadows := TKMRadioGroup.Create(Panel_Options_GFX,10,138,200,32, fnt_Metal);
      RadioGroup_Options_Shadows.Add(gResTexts[TX_MENU_OPTIONS_SHADOW_QUALITY_LOW]);
      RadioGroup_Options_Shadows.Add(gResTexts[TX_MENU_OPTIONS_SHADOW_QUALITY_HIGH]);
      RadioGroup_Options_Shadows.OnChange := Change;

    //SFX section
    Panel_Options_Sound:=TKMPanel.Create(Panel_Options,120,300,220,167);
    Panel_Options_Sound.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Sound,6,0,188,20,gResTexts[TX_MENU_OPTIONS_SOUND],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Sound,0,20,220,147);

      TrackBar_Options_SFX       := TKMTrackBar.Create(Panel_Options_Sound, 10, 27, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      TrackBar_Options_Music     := TKMTrackBar.Create(Panel_Options_Sound, 10, 77, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      CheckBox_Options_MusicOff  := TKMCheckBox.Create(Panel_Options_Sound, 12, 127, 196, 20, gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE], fnt_Metal);
      CheckBox_Options_ShuffleOn := TKMCheckBox.Create(Panel_Options_Sound, 12, 147, 196, 20, gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE], fnt_Metal);
      TrackBar_Options_SFX.Caption   := gResTexts[TX_MENU_SFX_VOLUME];
      TrackBar_Options_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
      TrackBar_Options_SFX.OnChange      := Change;
      TrackBar_Options_Music.OnChange    := Change;
      CheckBox_Options_MusicOff.OnClick  := Change;
      CheckBox_Options_ShuffleOn.OnClick := Change;

    //Resolutions section
    Panel_Options_Res := TKMPanel.Create(Panel_Options, 360, 130, 210, 160);
    Panel_Options_Res.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Res, 6, 0, 188, 20, gResTexts[TX_MENU_OPTIONS_RESOLUTION], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_Options_Res, 0, 20, 220, 140);

      CheckBox_Options_FullScreen := TKMCheckBox.Create(Panel_Options_Res, 12, 30, 176, 20, gResTexts[TX_MENU_OPTIONS_FULLSCREEN], fnt_Metal);
      CheckBox_Options_FullScreen.OnClick := ChangeResolution;

      DropBox_Options_Resolution := TKMDropList.Create(Panel_Options_Res, 10, 50, 180, 20, fnt_Metal, '', bsMenu);
      DropBox_Options_Resolution.OnChange := ChangeResolution;

      DropBox_Options_RefreshRate := TKMDropList.Create(Panel_Options_Res, 10, 85, 180, 20, fnt_Metal, '', bsMenu);
      DropBox_Options_RefreshRate.OnChange := ChangeResolution;

      Button_Options_ResApply := TKMButton.Create(Panel_Options_Res, 10, 120, 180, 30, gResTexts[TX_MENU_OPTIONS_APPLY], bsMenu);
      Button_Options_ResApply.OnClick := ApplyResolution;

    //Language section
    Panel_Options_Lang:=TKMPanel.Create(Panel_Options,600,130,240,30+gResLocales.Count*20);
    Panel_Options_Lang.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Lang,6,0,242,20,gResTexts[TX_MENU_OPTIONS_LANGUAGE],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Lang,0,20,260,10+gResLocales.Count*20);

      Radio_Options_Lang := TKMRadioGroup.Create(Panel_Options_Lang, 28, 27, 220, 20*gResLocales.Count, fnt_Metal);
      SetLength(Image_Options_Lang_Flags,gResLocales.Count);
      for I := 0 to gResLocales.Count - 1 do
      begin
        Radio_Options_Lang.Add(gResLocales[I].Title);
        Image_Options_Lang_Flags[I] := TKMImage.Create(Panel_Options_Lang,6,28+(I*20),16,11, gResLocales[I].FlagSpriteID, rxGuiMain);
        Image_Options_Lang_Flags[I].Tag := I;
        Image_Options_Lang_Flags[I].OnClick := FlagClick;
      end;
      Radio_Options_Lang.OnChange := Change;

    //Back button
    Button_Options_Back:=TKMButton.Create(Panel_Options,120,630,220,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_Options_Back.Anchors := [akLeft];
    Button_Options_Back.OnClick := BackClick;
end;


destructor TKMMenuOptions.Destroy;
begin

  inherited;
end;


//This is called when the options page is shown, so update all the values
//Note: Options can be required to fill before fGameApp is completely initialized,
//hence we need to pass either fGameApp.Settings or a direct Settings link
procedure TKMMenuOptions.Refresh;
begin
  CheckBox_Options_Autosave.Checked     := fGameSettings.Autosave;
  TrackBar_Options_Brightness.Position  := fGameSettings.Brightness;
  CheckBox_Options_VSync.Checked        := fMainSettings.VSync;
  RadioGroup_Options_Shadows.ItemIndex  := Byte(fGameSettings.AlphaShadows);
  TrackBar_Options_ScrollSpeed.Position := fGameSettings.ScrollSpeed;
  TrackBar_Options_SFX.Position         := Round(fGameSettings.SoundFXVolume * TrackBar_Options_SFX.MaxValue);
  TrackBar_Options_Music.Position       := Round(fGameSettings.MusicVolume * TrackBar_Options_Music.MaxValue);
  CheckBox_Options_MusicOff.Checked     := fGameSettings.MusicOff;
  TrackBar_Options_Music.Enabled        := not CheckBox_Options_MusicOff.Checked;
  CheckBox_Options_ShuffleOn.Checked    := fGameSettings.ShuffleOn;
  CheckBox_Options_ShuffleOn.Enabled    := not CheckBox_Options_MusicOff.Checked;

  Radio_Options_Lang.ItemIndex := gResLocales.IndexByCode(fGameSettings.Locale);

  //We need to reset dropboxes every time we enter Options page
  RefreshResolutions;
end;


//Changed options are saved immediately (cos they are easy to restore/rollback)
procedure TKMMenuOptions.Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled := (fGameSettings.MusicOff <> CheckBox_Options_MusicOff.Checked);
  ShuffleToggled := (fGameSettings.ShuffleOn <> CheckBox_Options_ShuffleOn.Checked);

  fGameSettings.Autosave      := CheckBox_Options_Autosave.Checked;
  fGameSettings.Brightness    := TrackBar_Options_Brightness.Position;
  fMainSettings.VSync         := CheckBox_Options_VSync.Checked;
  fGameSettings.AlphaShadows  := RadioGroup_Options_Shadows.ItemIndex = 1;
  fGameSettings.ScrollSpeed   := TrackBar_Options_ScrollSpeed.Position;
  fGameSettings.SoundFXVolume := TrackBar_Options_SFX.Position / TrackBar_Options_SFX.MaxValue;
  fGameSettings.MusicVolume   := TrackBar_Options_Music.Position / TrackBar_Options_Music.MaxValue;
  fGameSettings.MusicOff      := CheckBox_Options_MusicOff.Checked;
  fGameSettings.ShuffleOn     := CheckBox_Options_ShuffleOn.Checked;
  TrackBar_Options_Music.Enabled      := not CheckBox_Options_MusicOff.Checked;
  CheckBox_Options_ShuffleOn.Enabled  := not CheckBox_Options_MusicOff.Checked;

  gSoundPlayer.UpdateSoundVolume(fGameSettings.SoundFXVolume);
  fGameApp.MusicLib.UpdateMusicVolume(fGameSettings.MusicVolume);
  SetupVSync(fMainSettings.VSync);
  if MusicToggled then
  begin
    fGameApp.MusicLib.ToggleMusic(not fGameSettings.MusicOff);
    if not fGameSettings.MusicOff then
      ShuffleToggled := True; //Re-shuffle songs if music has been enabled
  end;
  if ShuffleToggled then
    fGameApp.MusicLib.ToggleShuffle(fGameSettings.ShuffleOn);

  if Sender = Radio_Options_Lang then
  begin
    fGameApp.ToggleLocale(gResLocales[Radio_Options_Lang.ItemIndex].Code);
    Exit; //Exit ASAP because whole interface will be recreated
  end;
end;


//Apply resolution changes
procedure TKMMenuOptions.ChangeResolution(Sender: TObject);
var
  I: Integer;
  ResID, RefID: Integer;
begin
  if fResolutions.Count = 0 then Exit;

  DropBox_Options_Resolution.Enabled := CheckBox_Options_FullScreen.Checked;
  DropBox_Options_RefreshRate.Enabled := CheckBox_Options_FullScreen.Checked;

  //Repopulate RefreshRates list
  if Sender = DropBox_Options_Resolution then
  begin
    ResID := DropBox_Options_Resolution.ItemIndex;

    //Reset refresh rates, because they are different for each resolution
    DropBox_Options_RefreshRate.Clear;
    for I := 0 to fResolutions.Items[ResID].RefRateCount - 1 do
    begin
      DropBox_Options_RefreshRate.Add(Format('%d Hz', [fResolutions.Items[ResID].RefRate[I]]));
      //Make sure to select something. SelectedRefRate is prefered, otherwise select first
      if (I = 0) or (fResolutions.Items[ResID].RefRate[I] = DesiredRefRate) then
        DropBox_Options_RefreshRate.ItemIndex := I;
    end;
  end;

  //Make button enabled only if new resolution/mode differs from old
  ResID := DropBox_Options_Resolution.ItemIndex;
  RefID := DropBox_Options_RefreshRate.ItemIndex;
  Button_Options_ResApply.Enabled :=
      (fMainSettings.FullScreen <> CheckBox_Options_FullScreen.Checked) or
      (CheckBox_Options_FullScreen.Checked and ((PrevResolutionId.ResID <> ResID) or
                                                (PrevResolutionId.RefID <> RefID)));
  //Remember which one we have selected so we can reselect it if the user changes resolution
  DesiredRefRate := fResolutions.Items[ResID].RefRate[RefID];
end;


procedure TKMMenuOptions.ApplyResolution(Sender: TObject);
var
  ResID, RefID: Integer;
  NewResolution: TScreenRes;
begin
  if fResolutions.Count = 0 then Exit;

  fMainSettings.FullScreen := CheckBox_Options_FullScreen.Checked;

  ResID := DropBox_Options_Resolution.ItemIndex;
  RefID := DropBox_Options_RefreshRate.ItemIndex;
  NewResolution.Width := fResolutions.Items[ResID].Width;
  NewResolution.Height := fResolutions.Items[ResID].Height;
  NewResolution.RefRate := fResolutions.Items[ResID].RefRate[RefID];

  fMainSettings.Resolution := NewResolution;
  fMain.ReinitRender(True);
end;


procedure TKMMenuOptions.FlagClick(Sender: TObject);
begin
  Assert(Sender is TKMImage);
  Radio_Options_Lang.ItemIndex := TKMImage(Sender).Tag;
  Change(Radio_Options_Lang);
end;


//Resets dropboxes, they will have correct values
procedure TKMMenuOptions.RefreshResolutions;
var I: Integer; R: TResIndex;
begin
  DropBox_Options_Resolution.Clear;
  DropBox_Options_RefreshRate.Clear;

  R := fResolutions.GetResolutionIDs(fMainSettings.Resolution);

  if fResolutions.Count > 0 then
  begin
    for I := 0 to fResolutions.Count - 1 do
    begin
      DropBox_Options_Resolution.Add(Format('%dx%d', [fResolutions.Items[I].Width, fResolutions.Items[I].Height]));
      if (I = 0) or (I = R.ResID) then
        DropBox_Options_Resolution.ItemIndex := I;
    end;

    for I := 0 to fResolutions.Items[R.ResID].RefRateCount - 1 do
    begin
      DropBox_Options_RefreshRate.Add(Format('%d Hz', [fResolutions.Items[R.ResID].RefRate[I]]));
      if (I = 0) or (I = R.RefID) then
      begin
        DropBox_Options_RefreshRate.ItemIndex := I;
        DesiredRefRate := fResolutions.Items[R.ResID].RefRate[I];
      end;
    end;
  end
  else
  begin
    //no supported resolutions
    DropBox_Options_Resolution.Add(gResTexts[TX_MENU_OPTIONS_RESOLUTION_NOT_SUPPORTED]);
    DropBox_Options_RefreshRate.Add(gResTexts[TX_MENU_OPTIONS_REFRESH_RATE_NOT_SUPPORTED]);
    DropBox_Options_Resolution.ItemIndex := 0;
    DropBox_Options_RefreshRate.ItemIndex := 0;
  end;

  CheckBox_Options_FullScreen.Checked := fMainSettings.FullScreen;
  //Controls should be disabled, when there is no resolution to choose
  CheckBox_Options_FullScreen.Enabled := fResolutions.Count > 0;
  DropBox_Options_Resolution.Enabled  := (fMainSettings.FullScreen) and (fResolutions.Count > 0);
  DropBox_Options_RefreshRate.Enabled := (fMainSettings.FullScreen) and (fResolutions.Count > 0);

  PrevResolutionId := R;
  Button_Options_ResApply.Disable;
end;


procedure TKMMenuOptions.Show;
begin
  //Remember what we are working with
  //(we do that on Show because Create gets called from Main/Game constructor and fMain/fGameApp are not yet assigned)
  //Ideally we could pass them as parameters here
  fMainSettings := fMain.Settings;
  fGameSettings := fGameApp.GameSettings;
  fResolutions := fMain.Resolutions;

  Refresh;
  Panel_Options.Show;
end;


procedure TKMMenuOptions.BackClick(Sender: TObject);
begin
  //Return to MainMenu and restore resolution changes
  fMainSettings.SaveSettings;

  fOnPageChange(gpMainMenu);
end;


end.
