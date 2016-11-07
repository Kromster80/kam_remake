unit KM_Main;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Forms, Math, SysUtils, StrUtils, Dialogs,
  {$IFDEF MSWindows} Windows, MMSystem, {$ENDIF}
  KromUtils, KM_FormLoading, KM_FormMain, KM_Settings, KM_Resolutions
  {$IFDEF USE_MAD_EXCEPT}, KM_Exceptions{$ENDIF};

type
  TKMMain = class
  private
    fFormMain: TFormMain;
    fFormLoading: TFormLoading;

    fOldTimeFPS, fOldFrameTimes, fFrameCount: Cardinal;
    fFlashing: Boolean;
    fMutex: THandle;

    fMainSettings: TMainSettings;
    fResolutions: TKMResolutions;

    procedure DoRestore(Sender: TObject);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoIdle(Sender: TObject; var Done: Boolean);

    procedure MapCacheUpdate;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure CloseQuery(var CanClose: Boolean);
    procedure Stop(Sender: TObject);

    procedure UpdateWindowParams(aWindowParams: TKMWindowParamsRecord);
    procedure Move(aWindowParams: TKMWindowParamsRecord);
    procedure Resize(aWidth, aHeight: Integer); overload;
    procedure Resize(aWidth, aHeight: Integer; aWindowParams: TKMWindowParamsRecord); overload;
    procedure Render;
    procedure ShowAbout;
    property FormMain: TFormMain read fFormMain;

    procedure ApplyCursorRestriction;
    function GetScreenBounds(out Bounds: TRect): Boolean;
    function IsFormActive: Boolean;
    function ClientRect: TRect;
    function ClientToScreen(aPoint: TPoint): TPoint;
    procedure ReinitRender(aReturnToOptions: Boolean);
    procedure FlashingStart;
    procedure FlashingStop;

    function LockMutex: Boolean;
    procedure UnlockMutex;

    procedure StatusBarText(aPanelIndex: Integer; const aText: UnicodeString); overload;

    property Resolutions: TKMResolutions read fResolutions;
    property Settings: TMainSettings read fMainSettings;
  end;


var
  fMain: TKMMain;


implementation
uses
  KM_Defaults, KM_GameApp, KM_Utils, KM_Log, KM_Maps;

const
  //Random GUID generated in Delphi by Ctrl+G
  KAM_MUTEX = '07BB7CC6-33F2-44ED-AD04-1E255E0EDF0D';

{ TKMMain }
constructor TKMMain.Create;
begin
  inherited;
  //Create exception handler as soon as possible in case it crashes early on
  {$IFDEF USE_MAD_EXCEPT}fExceptions := TKMExceptions.Create;{$ENDIF}

  //Form created first will be on taskbar
  Application.CreateForm(TFormMain, fFormMain);
  Application.CreateForm(TFormLoading, fFormLoading);
end;


destructor TKMMain.Destroy;
begin
  {$IFDEF USE_MAD_EXCEPT}fExceptions.Free;{$ENDIF}
  inherited;
end;


procedure TKMMain.Start;
begin
  //Random is only used for cases where order does not matter, e.g. shuffle tracks
  Randomize;

  fFormLoading.Label5.Caption := GAME_VERSION;
  fFormLoading.Show; //This is our splash screen
  fFormLoading.Refresh;

  {$IFDEF MSWindows}
  TimeBeginPeriod(1); //initialize timer precision
  {$ENDIF}
  ExeDir := ExtractFilePath(Application.ExeName);

  CreateDir(ExeDir + 'Logs' + PathDelim);
  gLog := TKMLog.Create(ExeDir + 'Logs' + PathDelim + 'KaM_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log'); //First thing - create a log
  gLog.DeleteOldLogs;

  //Resolutions are created first so that we could check Settings against them
  fResolutions := TKMResolutions.Create;

  //Only after we read settings (fullscreen property and resolutions)
  //we can decide whenever we want to create Game fullscreen or not (OpenGL init depends on that)
  fMainSettings := TMainSettings.Create;
  //We need to verify INI values, as they can be from another display
  if not fResolutions.IsValid(fMainSettings.Resolution) then
  begin
    fMainSettings.Resolution := fResolutions.FindCorrect(fMainSettings.Resolution);
    if not fResolutions.IsValid(fMainSettings.Resolution) then
      fMainSettings.FullScreen := False;
  end;

  fFormMain.Caption := 'KaM Remake - ' + GAME_VERSION;
  //Will make the form slightly higher, so do it before ReinitRender so it is reset
  fFormMain.ControlsSetVisibile(SHOW_DEBUG_CONTROLS);

  // Check INI window params, if not valid - set NeedResetToDefaults flag for future update
  if not fMainSettings.WindowParams.IsValid(Screen) then
     fMainSettings.WindowParams.NeedResetToDefaults := True;

  ReinitRender(False);

  Application.OnIdle := DoIdle;
  Application.OnActivate := DoActivate;
  Application.OnDeactivate := DoDeactivate;
  Application.OnRestore := DoRestore; //OnActivate seems to happen at the wrong times, OnRestore happens when alt-tabbing back in full screen mode

  //Update map cache files (*.mi) in the background so map lists load faster
  MapCacheUpdate;

  //Process messages in queue before hiding Loading, so that they all land on Loading form, not main one
  Application.ProcessMessages;
  fFormLoading.Hide;
end;


procedure TKMMain.StatusBarText(aPanelIndex: Integer; const aText: UnicodeString);
begin
  fFormMain.StatusBar1.Panels[aPanelIndex].Text := aText;
end;


procedure TKMMain.CloseQuery(var CanClose: Boolean);
var
  WasRunning: Boolean;
begin
  //MessageDlg works better than Application.MessageBox or others, it stays on top and
  //pauses here until the user clicks ok. However for some reason we chose MessageBox
  //thus we need to pause the game manually

  CanClose := (gGameApp = nil) or (gGameApp.Game = nil) or gGameApp.Game.IsReplay;

  if not CanClose then
  begin
    //We want to pause the game for the time user verifies he really wants to close
    WasRunning := not gGameApp.Game.IsMultiplayer
                  and not gGameApp.Game.IsMapEditor
                  and not gGameApp.Game.IsPaused;

    //Pause the game
    if WasRunning then
      gGameApp.Game.IsPaused := True;

    //Ask the Player
    {$IFDEF MSWindows}
    //MessageBox works best in Windows (gets stuck under main form less)
    CanClose := MessageBox( fFormMain.Handle,
                            PChar('Any unsaved changes will be lost. Exit?'),
                            PChar('Warning'),
                            MB_YESNO or MB_ICONWARNING or MB_SETFOREGROUND or MB_TASKMODAL
                           ) = IDYES;
    {$ENDIF}
    {$IFDEF Unix}
    CanClose := MessageDlg('Any unsaved changes will be lost. Exit?', mtWarning, [mbYes, mbNo], 0) = mrYes;
    {$ENDIF}

    //Resume the game
    if not CanClose and WasRunning then
      gGameApp.Game.IsPaused := False;
  end;
end;


procedure TKMMain.Stop(Sender: TObject);
begin
  //Reset the resolution
  FreeThenNil(fResolutions);
  FreeThenNil(fMainSettings);
  FreeThenNil(gGameApp);
  FreeThenNil(gLog);

  {$IFDEF MSWindows}
  TimeEndPeriod(1);
  ClipCursor(nil); //Release the cursor restriction
  {$ENDIF}

  // We could have been asked to close by MainForm or from other place (e.g. MainMenu Exit button)
  // In first case Form will take care about closing itself

  // Do not call gMain.Stop from FormClose handler again
  fFormMain.OnClose := nil;

  if Sender <> fFormMain then
    fFormMain.Close;
end;


//Apply the cursor restriction when alt-tabbing back
procedure TKMMain.DoRestore(Sender: TObject);
begin
  if Application.Active and (fMainSettings <> nil) then
    ApplyCursorRestriction; //Cursor restriction is lost when alt-tabbing out, so we need to apply it again
end;


procedure TKMMain.DoActivate(Sender: TObject);
begin
  if Application.Active then
    FlashingStop;
end;


procedure TKMMain.DoDeactivate(Sender: TObject);
begin
  //Occurs during Toggle to fullscreen, should be ignored
  if Application.Active then Exit;

  //Prevent the game window from being in the way by minimizing when alt-tabbing
  if (fMainSettings <> nil) and fMainSettings.FullScreen then
  begin
    {$IFDEF MSWindows}
      ClipCursor(nil); //Remove all cursor clipping just in case Windows doesn't automatically
    {$ENDIF}
    Application.Minimize;
  end;
end;


procedure TKMMain.DoIdle(Sender: TObject; var Done: Boolean);
var
  FrameTime: Cardinal;
begin
  if CHECK_8087CW then
    //$1F3F is used to mask out reserved/undefined bits
    Assert((Get8087CW and $1F3F = $133F), '8087CW is wrong');

  //if not Form1.Active then exit;

  //Counting FPS
  begin
    FrameTime  := GetTimeSince(fOldTimeFPS);
    fOldTimeFPS := TimeGet;

    if CAP_MAX_FPS and (FPS_LAG <> 1) and (FrameTime < FPS_LAG) then
    begin
      Sleep(FPS_LAG - FrameTime);
      FrameTime := FPS_LAG;
    end;

    inc(fOldFrameTimes, FrameTime);
    inc(fFrameCount);
    if fOldFrameTimes >= FPS_INTERVAL then
    begin
      if gGameApp <> nil then gGameApp.FPSMeasurement(Round(1000 / (fOldFrameTimes / fFrameCount)));
      StatusBarText(3, Format('%.1f fps', [1000 / (fOldFrameTimes / fFrameCount)]) +
                       IfThen(CAP_MAX_FPS, ' (' + inttostr(FPS_LAG) + ')'));
      fOldFrameTimes := 0;
      fFrameCount := 0;
    end;
  end;
  //FPS calculation complete

  //Some PCs seem to change 8087CW randomly between events like Timers and OnMouse*,
  //so we need to set it right before we do game logic processing
  Set8087CW($133F);
  if gGameApp <> nil then
  begin
    gGameApp.UpdateStateIdle(FrameTime);
    gGameApp.Render(False);
  end;

  Done := False; //Repeats OnIdle asap without performing Form-specific idle code
end;


procedure TKMMain.ReinitRender(aReturnToOptions: Boolean);
begin
  if fMainSettings.FullScreen then
  begin
    // Lock window params while we are in FullScreen mode
    fMainSettings.WindowParams.LockParams;
    if fResolutions.IsValid(fMainSettings.Resolution) then
      fResolutions.SetResolution(fMainSettings.Resolution)
    else
      fMainSettings.FullScreen := False;
  end else
    fResolutions.Restore;

  fFormLoading.Position := poScreenCenter;
  fFormMain.ToggleFullscreen(fMainSettings.FullScreen, fMainSettings.WindowParams.NeedResetToDefaults);

  //It's required to re-init whole OpenGL related things when RC gets toggled fullscreen
  FreeThenNil(gGameApp); //Saves all settings into ini file in midst
  gGameApp := TKMGameApp.Create(fFormMain.RenderArea,
                                fFormMain.RenderArea.Width,
                                fFormMain.RenderArea.Height,
                                fMainSettings.VSync,
                                fFormLoading.LoadingStep,
                                fFormLoading.LoadingText,
                                StatusBarText);
  gGameApp.AfterConstruction(aReturnToOptions);

  gLog.AddTime('ToggleFullscreen');
  gLog.AddTime('Form Width/Height: '+inttostr(fFormMain.Width)+':'+inttostr(fFormMain.Height));
  gLog.AddTime('Panel Width/Height: '+inttostr(fFormMain.RenderArea.Width)+':'+inttostr(fFormMain.RenderArea.Height));

  //Hide'n'show will make form go ontop of taskbar
  fFormMain.Hide;
  fFormMain.Show;

  Resize(fFormMain.RenderArea.Width, fFormMain.RenderArea.Height); //Force everything to resize
  // Unlock window params if are no longer in FullScreen mode
  if (not fMainSettings.FullScreen) then
    fMainSettings.WindowParams.UnlockParams;

  ApplyCursorRestriction;
end;


function TKMMain.LockMutex: Boolean;
begin
  Result := True;
  {$IFDEF MSWindows}
    if not BLOCK_DUPLICATE_APP then Exit;
    fMutex := CreateMutex(nil, True, PChar(KAM_MUTEX));
    if fMutex = 0 then
      RaiseLastOSError;
    Result := (GetLastError <> ERROR_ALREADY_EXISTS);
  if not Result then UnlockMutex; //Close our own handle on the mutex because someone else already made the mutex
  {$ENDIF}
  {$IFDEF Unix}
    Result := True;
  {$ENDIF}
end;


procedure TKMMain.MapCacheUpdate;
begin
  //Thread frees itself automatically
  TTMapsCacheUpdater.Create([mfSP, mfMP, mfDL]);
end;


procedure TKMMain.UnlockMutex;
begin
  {$IFDEF MSWindows}
    if not BLOCK_DUPLICATE_APP then Exit;
    if fMutex = 0 then Exit; //Didn't have a mutex lock
    CloseHandle(fMutex);
    fMutex := 0;
  {$ENDIF}
end;


procedure TKMMain.FlashingStart;
{$IFNDEF FPC}
var
  flashInfo: TFlashWInfo;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if (GetForeGroundWindow <> fMain.FormMain.Handle) then
  begin
    flashInfo.cbSize := 20;
    flashInfo.hwnd := Application.Handle;
    flashInfo.dwflags := FLASHW_ALL;
    flashInfo.ucount := 5;
    flashInfo.dwtimeout := 0;
    fFlashing := True;
    FlashWindowEx(flashInfo);
  end
  {$ENDIF}
end;


procedure TKMMain.FlashingStop;
{$IFNDEF FPC}
var
  flashInfo: TFlashWInfo;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if fFlashing then
  begin
    flashInfo.cbSize := 20;
    flashInfo.hwnd := Application.Handle;
    flashInfo.dwflags := FLASHW_STOP;
    flashInfo.ucount := 0;
    flashInfo.dwtimeout := 0;
    fFlashing := False;
    FlashWindowEx(flashInfo);
  end
  {$ENDIF}
end;


function TKMMain.ClientRect: TRect;
begin
  Result := fFormMain.RenderArea.ClientRect;
  Result.TopLeft := ClientToScreen(Result.TopLeft);
  Result.BottomRight := ClientToScreen(Result.BottomRight);
end;


function TKMMain.ClientToScreen(aPoint: TPoint): TPoint;
begin
  Result := fFormMain.RenderArea.ClientToScreen(aPoint);
end;


//Can be invalid very breifly if you change resolutions (this is possible in Windowed mode)
function TKMMain.GetScreenBounds(out Bounds: TRect): Boolean;
var I: Integer;
begin
  Result := False;
  Bounds := Classes.Rect(-1,-1,-1,-1);
  fFormMain.Monitor; //This forces Delphi to reload Screen.Monitors (only if necessary) and so fixes crashes when using multiple monitors
  //Maximized is a special case, it can only be on one monitor. This is required because when maximized form.left = -9 (on Windows 7 anyway)
  if fFormMain.WindowState = wsMaximized then
  begin
    for I:=0 to Screen.MonitorCount-1 do
      //Find the monitor with the left closest to the left of the form
      if (I = 0) or
         ((abs(fFormMain.Left - Screen.Monitors[I].Left) <= abs(fFormMain.Left - Bounds.Left)) and
          (abs(fFormMain.Top  - Screen.Monitors[I].Top ) <= abs(fFormMain.Top  - Bounds.Top))) then
      begin
        Result := True;
        Bounds.Left  := Screen.Monitors[I].Left;
        Bounds.Right := Screen.Monitors[I].Width+Screen.Monitors[I].Left;
        Bounds.Top   := Screen.Monitors[I].Top;
        Bounds.Bottom:= Screen.Monitors[I].Height+Screen.Monitors[I].Top;
      end;
  end
  else
    for I:=0 to Screen.MonitorCount-1 do
      //See if our form is within the boundaries of this monitor (I.e. when it is not outside the boundaries)
      if not ((fFormMain.Left               >= Screen.Monitors[I].Width + Screen.Monitors[I].Left) or
              (fFormMain.Width + fFormMain.Left <= Screen.Monitors[I].Left) or
              (fFormMain.Top                >= Screen.Monitors[I].Height + Screen.Monitors[I].Top) or
              (fFormMain.Height + fFormMain.Top <= Screen.Monitors[I].Top)) then
      begin
        if not Result then
        begin
          //First time we have to initialise the result
          Result := True;
          Bounds.Left  := Screen.Monitors[I].Left;
          Bounds.Right := Screen.Monitors[I].Width+Screen.Monitors[I].Left;
          Bounds.Top   := Screen.Monitors[I].Top;
          Bounds.Bottom:= Screen.Monitors[I].Height+Screen.Monitors[I].Top;
        end
        else
        begin
          //After the first time we compare it with the previous result and take the largest possible area
          Bounds.Left  := Math.Min(Bounds.Left,  Screen.Monitors[I].Left);
          Bounds.Right := Math.Max(Bounds.Right, Screen.Monitors[I].Width+Screen.Monitors[I].Left);
          Bounds.Top   := Math.Min(Bounds.Top,   Screen.Monitors[I].Top);
          Bounds.Bottom:= Math.Max(Bounds.Bottom,Screen.Monitors[I].Height+Screen.Monitors[I].Top);
        end;
      end;
end;


function TKMMain.IsFormActive: Boolean;
begin
  Result := fFormMain.Active;
end;


procedure TKMMain.Render;
begin
  if gGameApp <> nil then
    gGameApp.Render(False);
end;


procedure TKMMain.Resize(aWidth, aHeight: Integer);
begin
  if gGameApp <> nil then
    gGameApp.Resize(aWidth, aHeight);
end;



procedure TKMMain.Resize(aWidth, aHeight: Integer; aWindowParams: TKMWindowParamsRecord);
begin
  if gGameApp <> nil then
  begin
    gGameApp.Resize(aWidth, aHeight);
    UpdateWindowParams(aWindowParams);
  end;
end;


procedure TKMMain.Move(aWindowParams: TKMWindowParamsRecord);
begin
  UpdateWindowParams(aWindowParams);
end;


procedure TKMMain.UpdateWindowParams(aWindowParams: TKMWindowParamsRecord);
begin
  if gGameApp <> nil then
    fMainSettings.WindowParams.ApplyWindowParams(aWindowParams);
end;


procedure TKMMain.ShowAbout;
begin
  fFormLoading.Bar1.Position := 0;
  fFormLoading.Label1.Caption := '';
  fFormLoading.Show;
end;


//Restrict cursor movement in fullscreen mode
//For multiple monitors, it's very annoying if you play a fullscreen game and your cursor slides
//onto second monitor instead of stopping at the edge as expected.
procedure TKMMain.ApplyCursorRestriction;
var Rect: TRect;
begin
  //This restriction is removed when alt-tabbing out, and added again when alt-tabbing back
  {$IFDEF MSWindows}
  if fMainSettings.FullScreen then
  begin
    Rect := fFormMain.BoundsRect;
    ClipCursor(@Rect);
  end
  else
    ClipCursor(nil); //Otherwise have no restriction
  {$ENDIF}
end;


end.
