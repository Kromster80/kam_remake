unit KM_Main;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Forms, Math, SysUtils, StrUtils, Dialogs,
  {$IFDEF MSWindows} Windows, MMSystem, {$ENDIF}
  KromUtils, KM_FormLoading, KM_FormMain, KM_Settings, KM_Resolutions;

type
  TKMMain = class
  private
    fMainSettings: TMainSettings;
    fResolutions: TKMResolutions;

    procedure DoRestore(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create;

    procedure Start;
    procedure Stop(Sender: TObject);

    procedure Resize(X,Y: Integer);
    procedure Render;
    procedure ShowAbout;

    procedure ApplyCursorRestriction;
    function GetScreenBounds: TRect;
    function IsFormActive: Boolean;
    function ClientRect: TRect;
    function ClientToScreen(aPoint: TPoint): TPoint;
    procedure ReinitRender(aReturnToOptions: Boolean);

    procedure StatusBarText(aPanelIndex: Integer; const aText: string); overload;

    property Resolutions: TKMResolutions read fResolutions;
    property Settings: TMainSettings read fMainSettings;
  end;


var
  fMain: TKMMain;


implementation
uses KM_Defaults, KM_Game, KM_Utils, KM_Log;


{ TKMRemake }
constructor TKMMain.Create;
begin
  inherited;

end;


procedure TKMMain.Start;
begin
  //Random GUID generated in Delphi by Ctrl+G
  if BLOCK_DUPLICATE_APP
  and CheckDuplicateApplication('07BB7CC6-33F2-44ED-AD04-1E255E0EDF0D') then
  begin
    ShowMessage('Another copy of the Application is already running');
    Free; //Release fMain memory
    Halt; //Immmediately close the application
  end;

  SetKaMSeed(4); //Used for gameplay events so the order is important
  Randomize; //Random is only used for cases where order does not matter, e.g. shuffle tracks

  FormLoading.Label5.Caption := GAME_VERSION;
  FormLoading.Show; //This is our splash screen
  FormLoading.Refresh;

  {$IFDEF MSWindows}
  TimeBeginPeriod(1); //initialize timer precision
  {$ENDIF}
  ExeDir := ExtractFilePath(Application.ExeName);

  CreateDir(ExeDir + 'Logs\');
  fLog := TKMLog.Create(ExeDir + 'Logs\KaM_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log'); //First thing - create a log
  fLog.DeleteOldLogs;

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

  FormMain.Caption := 'KaM Remake - ' + GAME_VERSION;
  //Will make the form slightly higher, so do it before ReinitRender so it is reset
  FormMain.ToggleControlsVisibility(SHOW_DEBUG_CONTROLS);

  ReinitRender(False);

  Application.OnIdle := DoIdle;
  Application.OnDeactivate := DoDeactivate;
  Application.OnRestore := DoRestore; //OnActivate seems to happen at the wrong times, OnRestore happens when alt-tabbing back in full screen mode

  FormLoading.Hide;
end;


procedure TKMMain.StatusBarText(aPanelIndex: Integer; const aText: string);
begin
  FormMain.StatusBar1.Panels[aPanelIndex].Text := aText;
end;


procedure TKMMain.Stop(Sender: TObject);
begin
  //Reset the resolution
  if fResolutions<>nil then FreeThenNil(fResolutions);
  if fMainSettings<>nil then FreeThenNil(fMainSettings);
  if fGame<>nil then fGame.Stop(gr_Silent);
  if fGame<>nil then FreeThenNil(fGame);
  if fLog<>nil then FreeThenNil(fLog);
  {$IFDEF MSWindows}
  TimeEndPeriod(1);
  ClipCursor(nil); //Release the cursor restriction
  {$ENDIF}

  //We could be asked to close from MainForm or from other place
  //In first case Form will take care about closing itself
  if Sender <> FormMain then
    FormMain.Close;
end;


//Apply the cursor restriction when alt-tabbing back
procedure TKMMain.DoRestore(Sender: TObject);
begin
  if Application.Active and (fMainSettings <> nil) then
    ApplyCursorRestriction; //Cursor restriction is lost when alt-tabbing out, so we need to apply it again
end;


procedure TKMMain.DoDeactivate(Sender: TObject);
begin
  //Occurs during Toggle to fullscreen, should be ignored
  if Application.Active then Exit;

  //Prevent the game window from being in the way by minimizing when alt-tabbing
  if (fMainSettings <> nil) and fMainSettings.FullScreen then
  begin
    ClipCursor(nil); //Remove all cursor clipping just in case Windows doesn't automatically
    Application.Minimize;
  end;
end;


procedure TKMMain.DoIdle(Sender: TObject; var Done: Boolean);
var
  FrameTime: Cardinal;
begin
  //if not Form1.Active then exit;

  //Counting FPS
  begin
    FrameTime  := TimeGet - OldTimeFPS;
    OldTimeFPS := TimeGet;

    if CAP_MAX_FPS and (FPS_LAG <> 1) and (FrameTime < FPS_LAG) then
    begin
      Sleep(FPS_LAG - FrameTime);
      FrameTime := FPS_LAG;
    end;

    inc(OldFrameTimes, FrameTime);
    inc(FrameCount);
    if OldFrameTimes >= FPS_INTERVAL then
    begin
      StatusBarText(3, Format('%.1f fps', [1000 / (OldFrameTimes / FrameCount)]) +
                       IfThen(CAP_MAX_FPS, ' (' + inttostr(FPS_LAG) + ')'));
      OldFrameTimes := 0;
      FrameCount := 0;
    end;
  end;
  //FPS calculation complete

  if fGame <> nil then
  begin
    fGame.UpdateStateIdle(FrameTime);
    fGame.Render;
  end;

  Done := False; //Repeats OnIdle asap without performing Form-specific idle code
end;


procedure TKMMain.ReinitRender(aReturnToOptions: Boolean);
begin
  if fMainSettings.FullScreen then
    if fResolutions.IsValid(fMainSettings.Resolution) then
      fResolutions.SetResolution(fMainSettings.Resolution)
    else
      fMainSettings.FullScreen := False
  else
    fResolutions.Restore;

  FormLoading.Position := poScreenCenter;
  FormMain.ToggleFullscreen(fMainSettings.FullScreen);

  //It's required to re-init whole OpenGL related things when RC gets toggled fullscreen
  FreeThenNil(fGame); //Saves all settings into ini file in midst
  fGame := TKMGame.Create(
                          FormMain.Panel5.Handle,
                          FormMain.Panel5.Width,
                          FormMain.Panel5.Height,
                          fMainSettings.VSync,
                          FormLoading.LoadingStep,
                          FormLoading.LoadingText
                          );
  fGame.AfterConstruction(aReturnToOptions);
  fGame.OnCursorUpdate := StatusBarText;

  fLog.AppendLog('ToggleFullscreen');
  fLog.AppendLog('Form Width/Height: '+inttostr(FormMain.Width)+':'+inttostr(FormMain.Height));
  fLog.AppendLog('Panel Width/Height: '+inttostr(FormMain.Panel5.Width)+':'+inttostr(FormMain.Panel5.Height));

  Resize(FormMain.Panel5.Width, FormMain.Panel5.Height); //Force everything to resize
  ApplyCursorRestriction;
end;


function TKMMain.ClientRect: TRect;
begin
  Result := FormMain.Panel5.ClientRect;
  Result.TopLeft := ClientToScreen(Result.TopLeft);
  Result.BottomRight := ClientToScreen(Result.BottomRight);
end;


function TKMMain.ClientToScreen(aPoint: TPoint): TPoint;
begin
  Result := FormMain.Panel5.ClientToScreen(aPoint);
end;


function TKMMain.GetScreenBounds: TRect;
var i: integer;
    FirstTime: boolean;
begin
  Result := Classes.Rect(-1,-1,-1,-1);
  FormMain.Monitor; //This forces Delphi to reload Screen.Monitors (only if necessary) and so fixes crashes when using multiple monitors
  FirstTime := true;
  //Maximized is a special case, it can only be on one monitor. This is required because when maximized form.left = -9 (on Windows 7 anyway)
  if FormMain.WindowState = wsMaximized then
  begin
    for i:=0 to Screen.MonitorCount-1 do
      //Find the monitor with the left closest to the left of the form
      if (i = 0) or
         ((abs(FormMain.Left - Screen.Monitors[i].Left) <= abs(FormMain.Left - Result.Left)) and
          (abs(FormMain.Top  - Screen.Monitors[i].Top ) <= abs(FormMain.Top  - Result.Top))) then
      begin
        Result.Left  := Screen.Monitors[i].Left;
        Result.Right := Screen.Monitors[i].Width+Screen.Monitors[i].Left;
        Result.Top   := Screen.Monitors[i].Top;
        Result.Bottom:= Screen.Monitors[i].Height+Screen.Monitors[i].Top;
      end;
  end
  else
    for i:=0 to Screen.MonitorCount-1 do
      //See if our form is within the boundaries of this monitor (i.e. when it is not outside the boundaries)
      if not ((FormMain.Left               >= Screen.Monitors[i].Width + Screen.Monitors[i].Left) or
              (FormMain.Width + FormMain.Left <= Screen.Monitors[i].Left) or
              (FormMain.Top                >= Screen.Monitors[i].Height + Screen.Monitors[i].Top) or
              (FormMain.Height + FormMain.Top <= Screen.Monitors[i].Top)) then
      begin
        if FirstTime then
        begin
          //First time we have to initialise the result
          FirstTime := false;
          Result.Left  := Screen.Monitors[i].Left;
          Result.Right := Screen.Monitors[i].Width+Screen.Monitors[i].Left;
          Result.Top   := Screen.Monitors[i].Top;
          Result.Bottom:= Screen.Monitors[i].Height+Screen.Monitors[i].Top;
        end
        else
        begin
          //After the first time we compare it with the previous result and take the largest possible area
          Result.Left  := Math.Min(Result.Left,  Screen.Monitors[i].Left);
          Result.Right := Math.Max(Result.Right, Screen.Monitors[i].Width+Screen.Monitors[i].Left);
          Result.Top   := Math.Min(Result.Top,   Screen.Monitors[i].Top);
          Result.Bottom:= Math.Max(Result.Bottom,Screen.Monitors[i].Height+Screen.Monitors[i].Top);
        end;
      end;
end;


function TKMMain.IsFormActive: Boolean;
begin
  Result := FormMain.Active;
end;


procedure TKMMain.Render;
begin
  if fGame <> nil then
    fGame.Render;
end;


procedure TKMMain.Resize(X, Y: Integer);
begin
  if fLog <> nil then
    fLog.AppendLog('FormResize X/Y: '+inttostr(X)+':'+inttostr(Y));

  if fGame <> nil then
    fGame.Resize(X, Y);
end;


procedure TKMMain.ShowAbout;
begin
  FormLoading.Bar1.Position := 0;
  FormLoading.Label1.Caption := '';
  FormLoading.Show;
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
    Rect := FormMain.BoundsRect;
    ClipCursor(@Rect);
  end
  else
    ClipCursor(nil); //Otherwise have no restriction
  {$ENDIF}
end;


end.
