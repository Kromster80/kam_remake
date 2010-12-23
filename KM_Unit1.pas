unit KM_Unit1;
{$I KaM_Remake.inc}
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, Buttons, Messages,
  Math, SysUtils, KromUtils,
  {$IFDEF WDC} OpenGL, MPlayer, {$ENDIF}
  {$IFDEF FPC} GL, LResources, {$ENDIF}
  dglOpenGL, MMSystem,
  KM_Render, KM_ResourceGFX, KM_Defaults, KM_Form_Loading,
  KM_Game, KM_PlayersCollection, 
  KM_TextLibrary, KM_Sound;

type
  TForm1 = class(TForm)
    MenuItem1: TMenuItem;
    N2: TMenuItem;
    Debug_ShowUnits: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Debug1: TMenuItem;
    Debug_ShowWires: TMenuItem;
    Panel5: TPanel;
    Timer100ms: TTimer;
    Debug_PrintScreen: TMenuItem;
    Export1: TMenuItem;
    Export_GUIRX: TMenuItem;
    Export_TreesRX: TMenuItem;
    Export_HousesRX: TMenuItem;
    Export_UnitsRX: TMenuItem;
    Export_GUIMainRX: TMenuItem;
    Export_Fonts1: TMenuItem;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    Export_Text: TMenuItem;
    Export_Deliverlists1: TMenuItem;
    Export_Sounds1: TMenuItem;
    Debug_PassabilityTrack: TTrackBar;
    Label2: TLabel;
    Export_HouseAnim1: TMenuItem;
    Export_UnitAnim1: TMenuItem;
    RGPlayer: TRadioGroup;
    Button_Stop: TButton;
    OpenMissionMenu: TMenuItem;
    Debug_ShowOverlay: TMenuItem;
    AnimData1: TMenuItem;
    Other1: TMenuItem;
    Debug_ShowPanel1: TMenuItem;
    Export_TreeAnim1: TMenuItem;
    Export_GUIMainHRX: TMenuItem;
    TB_Angle: TTrackBar;
    Label3: TLabel;
    {$IFDEF WDC} MediaPlayer1: TMediaPlayer; {$ENDIF}
    ExportMainMenu1: TMenuItem; 
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure TB_Angle_Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExportMainMenu1Click(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
  published
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure Debug_ShowWiresClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Debug_PrintScreenClick(Sender: TObject);
    procedure Export_TreesRXClick(Sender: TObject);
    procedure Export_HousesRXClick(Sender: TObject);
    procedure Export_UnitsRXClick(Sender: TObject);
    procedure Export_GUIRXClick(Sender: TObject);
    procedure Export_GUIMainRXClick(Sender: TObject);
    procedure Export_GUIMainHRXClick(Sender: TObject);
    procedure Export_Sounds1Click(Sender: TObject);
    procedure Export_HouseAnim1Click(Sender: TObject);
    procedure Export_UnitAnim1Click(Sender: TObject);
    procedure Export_TextClick(Sender: TObject);
    procedure Export_Fonts1Click(Sender: TObject);
    procedure Export_DeliverLists1Click(Sender: TObject);
    procedure Debug_PassabilityTrackChange(Sender: TObject);
    procedure Debug_ShowOverlayClick(Sender: TObject);
    procedure Button_StopClick(Sender: TObject);
    procedure RGPlayerClick(Sender: TObject);
    procedure Open_MissionMenuClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Debug_ShowUnitClick(Sender: TObject);
    procedure Debug_ShowPanel1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SetScreenResolution(Width, Height, RefreshRate: word);
    procedure ResetResolution;
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WMSysCommand(var Msg : TWMSysCommand); message WM_SYSCOMMAND;
    procedure ReadAvailableResolutions;
  public
    procedure ApplyCursorRestriction;
    procedure ToggleControlsVisibility(ShowCtrls:boolean);
    procedure ToggleFullScreen(Toggle:boolean; ResolutionID:word; aVSync:boolean; ReturnToOptions:boolean);
  end;

var
  Form1: TForm1;
  FormLoading:TFormLoading;
  TextT:GLuint; //Tiles

implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}

uses KM_Settings, KM_CommonTypes, KM_TGATexture;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var FrameTime:cardinal; s:string;
begin
  //if not Form1.Active then exit;

  //Counting FPS
  begin
    FrameTime  := TimeGetTime - OldTimeFPS;
    OldTimeFPS := TimeGetTime;

    if CAP_MAX_FPS and (FPS_LAG<>1)and(FrameTime<FPS_LAG) then begin
      sleep(FPS_LAG-FrameTime);
      FrameTime:=FPS_LAG;
    end;

    inc(OldFrameTimes,FrameTime);
    inc(FrameCount);
    if OldFrameTimes>=FPS_INTERVAL then begin
      s := Format('%.1f fps',[1000/(OldFrameTimes/FrameCount)]);
      if CAP_MAX_FPS then s := s + ' (' + inttostr(FPS_LAG) + ')';
      StatusBar1.Panels[3].Text:=s;
      OldFrameTimes:=0;
      FrameCount:=0;
    end;
  end;
  //FPS calculation complete

  fGame.UpdateStateIdle(FrameTime);
  fRender.Render;
  Done := false; //repeats OnIdle event
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  TempSettings:TGlobalSettings;
begin
  if Sender<>nil then exit;

  FormLoading.Label5.Caption := GAME_VERSION;
  FormLoading.Show; //This is our splash screen
  FormLoading.Refresh;
  Panel5.Color := clBlack;
  ToggleControlsVisibility(SHOW_DEBUG_CONTROLS);

  //Randomize; //Randomize the random seed to ensure that we don't get repeditive patterns,
  //but we need this to be Off to reproduce bugs
  TimeBeginPeriod(1); //initialize timer precision
  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log'); //First thing - create a log

  ReadAvailableResolutions;    //Undecided as to how this will fit in with the game, see discussion

  TempSettings := TGlobalSettings.Create; //Read settings (fullscreen property and resolutions)
  ToggleFullScreen(TempSettings.IsFullScreen, TempSettings.GetResolutionID, TempSettings.IsVSync, false); //Now we can decide whether we should make it full screen or not
  TempSettings.Free;

  //We don't need to re-init fGame since it's already handled in ToggleFullScreen (sic!)
  //fGame:=TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height, true);

  Application.OnIdle:=Form1.OnIdle;
  fLog.AppendLog('Form1 create is done');

  //Show the message if user has old OpenGL drivers (pre-1.4)
  if not GL_VERSION_1_4 then
  begin
    Application.MessageBox(PChar(fTextLibrary.GetRemakeString(53)),
        'Warning', MB_OK or MB_ICONWARNING);
  end;

  Timer100ms.Interval := fGame.fGlobalSettings.GetPace; //FormLoading gets hidden OnTimer event
  Form1.Caption := 'KaM Remake - ' + GAME_VERSION;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  //Reset the resolution
  ResetResolution;
  fGame.GameStop(gr_Silent);
  FreeThenNil(fGame);
  FreeThenNil(fLog);
  TimeEndPeriod(1);
  ClipCursor(nil); //Release the cursor restriction
end;


procedure TForm1.FormResize(Sender:TObject);
begin
  if fGame<>nil then //Occurs on exit
    fGame.ResizeGameArea(Panel5.Width, Panel5.Height);
  if fLog<>nil then
    fLog.AppendLog('FormResize - '+inttostr(Panel5.Top)+':'+inttostr(Panel5.Height));
  ApplyCursorRestriction;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLog.AssertToLog(Form1.KeyPreview, 'Form1 should recieve all keys to pass them to fGame');
  fGame.KeyUp(Key, Shift, true);
end;


procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLog.AssertToLog(Form1.KeyPreview, 'Form1 should recieve all keys to pass them to fGame');
  fGame.KeyUp(Key, Shift);
end;


procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseDown(Button, Shift, X, Y); end;


procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin fGame.MouseMove(Shift, X, Y); end;


procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseUp(Button, Shift, X, Y); end;


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin fGame.MouseWheel(Shift, WheelDelta, Panel5.ScreenToClient(MousePos).X, Panel5.ScreenToClient(MousePos).Y); end;


procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  //if not (Form1.Active or FormLoading.Active) then exit;
  if FormLoading.Visible then begin
    FormLoading.Hide;
    Form1.Show;
    Form1.SetFocus;
  end;
  if fGame<>nil then fGame.UpdateState;
end;


//Open
procedure TForm1.Open_MissionMenuClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then exit;
  fGame.GameStop(gr_Silent);
  fGame.GameStart(OpenDialog1.FileName, 'OpenDialog1 game');
end;


procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then exit;
  fGame.GameStop(gr_Silent);
  fGame.MapEditorStart(OpenDialog1.FileName, 0, 0);
end;


//Exit
procedure TForm1.ExitClick(Sender: TObject); begin Form1.Close; end;


//About
procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Label1.Caption:='';
  FormLoading.Show;
end;


//Debug Options
procedure TForm1.Debug_ShowWiresClick(Sender: TObject);
begin
  Debug_ShowWires.Checked := not Debug_ShowWires.Checked;
  SHOW_TERRAIN_WIRES := Debug_ShowWires.Checked;
end;

procedure TForm1.Debug_ShowOverlayClick(Sender: TObject);
begin
  Debug_ShowOverlay.Checked := not Debug_ShowOverlay.Checked;
  SHOW_CONTROLS_OVERLAY := Debug_ShowOverlay.Checked;
end;

procedure TForm1.Debug_ShowUnitClick(Sender: TObject);
begin
  Debug_ShowUnits.Checked := not Debug_ShowUnits.Checked;
  SHOW_UNIT_MOVEMENT := Debug_ShowUnits.Checked;
  SHOW_UNIT_ROUTES   := Debug_ShowUnits.Checked;
end;


procedure TForm1.Debug_PrintScreenClick(Sender: TObject);
{$IFDEF WDC} var s:string; {$ENDIF}
begin
  {$IFDEF WDC}
  DateTimeToString(s,'yyyy-mm-dd hh-nn-ss',Now); //2007-12-23 15-24-33
  if fRender<>nil then fRender.DoPrintScreen(ExeDir+'KaM '+s+'.jpg');
  {$ENDIF}
end;


procedure TForm1.Debug_ShowPanel1Click(Sender: TObject);
begin GroupBox1.Visible := not GroupBox1.Visible; end;


procedure TForm1.Debug_PassabilityTrackChange(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES:=Debug_PassabilityTrack.Position<>0;
  Debug_PassabilityTrack.Max:=length(PassabilityStr);
  if Debug_PassabilityTrack.Position <> 0 then
    Label2.Caption := PassabilityStr[TPassability(Debug_PassabilityTrack.Position)]
  else
    Label2.Caption := '';
end;


//Exports
procedure TForm1.Export_TreesRXClick(Sender: TObject);   begin ExportRX2BMP(1); end;
procedure TForm1.Export_HousesRXClick(Sender: TObject);  begin ExportRX2BMP(2); end;
procedure TForm1.Export_UnitsRXClick(Sender: TObject);   begin ExportRX2BMP(3); end;
procedure TForm1.Export_GUIRXClick(Sender: TObject);     begin ExportRX2BMP(4); end;
procedure TForm1.Export_GUIMainRXClick(Sender: TObject); begin ExportRX2BMP(5); end;
procedure TForm1.Export_GUIMainHRXClick(Sender: TObject);begin ExportRX2BMP(6); end;
procedure TForm1.Export_Sounds1Click(Sender: TObject);   begin fSoundLib.ExportSounds; end;
procedure TForm1.Export_TreeAnim1Click(Sender: TObject); begin ExportTreeAnim2BMP(); end;
procedure TForm1.Export_HouseAnim1Click(Sender: TObject);begin ExportHouseAnim2BMP(); end;
procedure TForm1.Export_UnitAnim1Click(Sender: TObject); begin ExportUnitAnim2BMP();  end;
procedure TForm1.Export_TextClick(Sender: TObject);      begin fTextLibrary.ExportTextLibraries; end;

procedure TForm1.Export_Fonts1Click(Sender: TObject);
begin
  fLog.AssertToLog(fResource<>nil,'Can''t export Fonts cos they aren''t loaded yet');
  fResource.LoadFonts(true, fGame.fGlobalSettings.GetLocale);
end;


procedure TForm1.Export_DeliverLists1Click(Sender: TObject);
var i:integer;
begin
  if fPlayers=nil then exit;
  for i:=1 to fPlayers.PlayerCount do
    fPlayers.Player[i].DeliverList.SaveToFile(ExeDir+'Player_'+inttostr(i)+'_Deliver_List.txt');
end;


procedure TForm1.RGPlayerClick(Sender: TObject);
begin
  if (fPlayers<>nil) and (fPlayers.Player[RGPlayer.ItemIndex+1]<>nil) then
    MyPlayer := fPlayers.Player[RGPlayer.ItemIndex+1];
end;


procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then fGame.SetGameSpeed(120) else fGame.SetGameSpeed(1);
end;      


procedure TForm1.Button_StopClick(Sender: TObject);
begin
  fGame.GameStop(gr_Cancel);
end;


procedure TForm1.TB_Angle_Change(Sender: TObject);
begin
  RENDER_3D:=TB_Angle.Position<>0;
  Label3.Caption:=inttostr(TB_Angle.Position)+' 3D';
  fRender.SetRotation(-TB_Angle.Position,0,0);
  fRender.Render;
end;


procedure TForm1.ToggleControlsVisibility(ShowCtrls:boolean);
var i:integer;
begin
  Form1.Refresh;

  {if ShowCtrls then Form1.Menu := MainMenu1
                else Form1.Menu := nil; //Not working as intended yet}

  {$IFDEF WDC} //Lazarus can't operate with ClientSize for it's not multi-platform property
  if MainMenu1.Items[0].Visible and not ShowCtrls then //Hiding controls
    Form1.ClientHeight := Form1.ClientHeight - 20
  else
  if not MainMenu1.Items[0].Visible and ShowCtrls then //Showing controls
    Form1.ClientHeight := Form1.ClientHeight + 20;
  {$ENDIF}

  GroupBox1.Visible  := ShowCtrls;
  StatusBar1.Visible := ShowCtrls;
  for i:=1 to MainMenu1.Items.Count do
    MainMenu1.Items[i-1].Visible := ShowCtrls;

  GroupBox1.Enabled  := ShowCtrls;
  StatusBar1.Enabled := ShowCtrls;
  for i:=1 to MainMenu1.Items.Count do
    MainMenu1.Items[i-1].Enabled := ShowCtrls;

  Form1.Refresh;

  Panel5.Top    := 0;
  Panel5.Height := Form1.ClientHeight;
  Panel5.Width  := Form1.ClientWidth;

  if fGame<>nil then //Could happen on game start when Form gets resized and fGame is nil
    fGame.ResizeGameArea(Panel5.Width,Panel5.Height);
end;


procedure TForm1.ToggleFullScreen(Toggle:boolean; ResolutionID:word; aVSync:boolean; ReturnToOptions:boolean);
begin
  if Toggle then begin
    SetScreenResolution(SupportedResolutions[ResolutionID,1],SupportedResolutions[ResolutionID,2],SupportedRefreshRates[ResolutionID]);
    Form1.Refresh;
    Form1.BorderStyle  := bsSizeable; //if we don't set Form1 sizeable it won't expand to fullscreen
    Form1.WindowState  := wsMaximized;
    Form1.BorderStyle  := bsNone;     //and now we can make it borderless again
    Form1.FormStyle    := fsStayOnTop;//Should overlay TaskBar
    Form1.Refresh;
  end else begin
    ResetResolution;
    Form1.Refresh;
    Form1.WindowState  := wsNormal;
    Form1.BorderStyle  := bsSizeable;
    Form1.FormStyle    := fsNormal;
    Form1.ClientWidth  := MENU_DESIGN_X;
    Form1.ClientHeight := MENU_DESIGN_Y;
    Form1.Refresh;
    Form1.Left := Math.max((Screen.Width  - MENU_DESIGN_X)div 2,0);
    Form1.Top  := Math.max((Screen.Height - MENU_DESIGN_Y)div 2,0); //Center on screen and make sure titlebar is visible
  end;

  Panel5.Top    := 0;
  Panel5.Height := Form1.ClientHeight;
  Panel5.Width  := Form1.ClientWidth;

  //It's required to re-init whole OpenGL related things when RC gets toggled fullscreen
  //Don't know how lame it is, but it works well
  //It wastes a bit of RAM (1.5mb) and takes few seconds to re-init
  FreeThenNil(fGame); //Saves all settings into ini file in midst
  //Now re-init fGame
  fGame := TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height,aVSync {$IFDEF WDC}, MediaPlayer1 {$ENDIF});
  fGame.ResizeGameArea(Panel5.Width,Panel5.Height);
  fLog.AppendLog('ToggleFullscreen - '+inttostr(Panel5.Top)+':'+inttostr(Panel5.Height));

  if ReturnToOptions then fGame.fMainMenuInterface.ShowScreen_Options; //Return to the options screen
  ApplyCursorRestriction;
end;


procedure TForm1.SetScreenResolution(Width, Height, RefreshRate: word);
var
  DeviceMode: DEVMODE;
begin
  if not FORCE_RESOLUTION then exit;
  ZeroMemory(@DeviceMode,sizeof(DeviceMode));
  with DeviceMode do begin
    dmSize := SizeOf(TDeviceMode);
    dmPelsWidth := Width;
    dmPelsHeight := Height;
    dmBitsPerPel := 32;
    dmDisplayFrequency := RefreshRate;
    dmFields := DM_DISPLAYFREQUENCY or DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  end;
  ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN);
  ApplyCursorRestriction;
end;


procedure TForm1.ResetResolution;
begin
  if FORCE_RESOLUTION then ChangeDisplaySettings(DEVMODE(nil^),0);
end;


procedure TForm1.WMSysCommand(var Msg : TWMSysCommand);
begin
  //If the system message is screensaver or monitor power off then trap the message and set its result to -1
  if (Msg.CmdType = SC_SCREENSAVE) or (Msg.CmdType = SC_MONITORPOWER) then
    Msg.Result := -1
  else
    Inherited;
end;


procedure TForm1.ReadAvailableResolutions;
var
  i,k : integer;
  DevMode : TDevMode;
begin
  i := 0;
  FillChar(SupportedRefreshRates, SizeOf(SupportedRefreshRates), 0); //Thats a nice trick to fill it with zeroes ;)
  while EnumDisplaySettings(nil, i, DevMode) do
  with DevMode do
  begin
    inc(i);
    if dmBitsPerPel=32 then //List only 32bpp modes
    for k:=1 to RESOLUTION_COUNT do
    if (SupportedResolutions[k,1] = dmPelsWidth) and (SupportedResolutions[k,2] = dmPelsHeight)then
      SupportedRefreshRates[k] := Math.max(SupportedRefreshRates[k], dmDisplayFrequency);
  end;
end;


procedure TForm1.ApplyCursorRestriction;
var Rect: TRect;
begin
  if (fGame <> nil) and (fGame.fGlobalSettings <> nil) and fGame.fGlobalSettings.IsFullScreen then
  begin
    Rect := BoundsRect;
    ClipCursor(@Rect); //Restrict the cursor movement to inside our form
  end
  else
    ClipCursor(nil); //Otherwise have no restriction
end;


procedure TForm1.ExportMainMenu1Click(Sender: TObject);
begin
  //fGame.fMainMenuInterface.MyControls.SaveToFile(ExeDir+'MainMenu.txt');
end;


//Restrict minimum Form ClientArea size to MENU_DESIGN_X/Y
procedure TForm1.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var Margin:TPoint;
begin
  Margin.X := Width - ClientWidth;
  Margin.Y := Height - ClientHeight;

  NewWidth := math.max(NewWidth, MENU_DESIGN_X + Margin.X);
  NewHeight:= math.max(NewHeight, MENU_DESIGN_Y + Margin.Y);

  Resize := true;
end;


{$IFDEF FPC}
initialization
{$I KM_Unit1.lrs}
{$ENDIF}

end.
