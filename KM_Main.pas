unit KM_Main;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, ExtCtrls, Forms, Math, SysUtils, StrUtils,
  {$IFDEF MSWindows} Windows, MMSystem, {$ENDIF}
  KromUtils, KM_FormLoading, KM_FormMain, KM_Settings;

type
  TKMMain = class
  private
    fTimer: TTimer;

    procedure DoDeactivate(Sender: TObject);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure DoTimer(Sender: TObject);

    procedure CheckResolution(aGameSettings: TGlobalSettings);
    procedure ReadAvailableResolutions;
    procedure SortScreenResData;
    procedure ResetResolution;
    procedure SetScreenResolution(Width, Height, RefreshRate: word);
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
    procedure ToggleFullScreen(aSettings: TGlobalSettings; aReturnToOptions: Boolean);

    procedure StatusBarText(const aData: string); overload;
    procedure StatusBarText(aPanelIndex: Integer; aText: string); overload;
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
var
  TempSettings: TGlobalSettings;
begin
  SetKaMSeed(4); //Used for gameplay events so the order is important
  Randomize; //Random is only used for cases where order does not matter, e.g. shuffle tracks

  FormLoading.Label5.Caption := GAME_VERSION;
  FormLoading.Show; //This is our splash screen
  FormLoading.Refresh;

  {$IFDEF MSWindows}
  TimeBeginPeriod(1); //initialize timer precision
  {$ENDIF}
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

  CreateDir(ExeDir + 'Logs\');
  fLog := TKMLog.Create(ExeDir+'Logs\KaM_'+FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz',Now)+'.log'); //First thing - create a log

  ReadAvailableResolutions;

  //Only after we read settings (fullscreen property and resolutions)
  //we can decide whenever we want to create Game fullscreen or not (OpenGL init depends on that)
  TempSettings := TGlobalSettings.Create;
  CheckResolution(TempSettings);
  ToggleFullScreen(TempSettings, False);
  TempSettings.Free;

  Application.OnIdle := DoIdle;
  Application.OnDeactivate := DoDeactivate;

  //todo: Move timer to fGame
  fTimer := TTimer.Create(nil);
  fTimer.OnTimer := DoTimer;
  fTimer.Interval := fGame.GlobalSettings.SpeedPace;
  fTimer.Enabled := True;

  FormMain.Caption := 'KaM Remake - ' + GAME_VERSION;
  FormMain.ToggleControlsVisibility(SHOW_DEBUG_CONTROLS);

  FormLoading.Hide;
end;


procedure TKMMain.StatusBarText(const aData: string);
begin
  FormMain.StatusBar1.Panels[1].Text := aData;
end;


procedure TKMMain.StatusBarText(aPanelIndex: Integer; aText: string);
begin
  FormMain.StatusBar1.Panels[aPanelIndex].Text := aText;
end;


procedure TKMMain.Stop(Sender: TObject);
begin
  //Reset the resolution
  ResetResolution;
  if fGame<>nil then fGame.Stop(gr_Silent);
  if fGame<>nil then FreeThenNil(fGame);
  if fLog<>nil then FreeThenNil(fLog);
  {$IFDEF MSWindows}
  TimeEndPeriod(1);
  ClipCursor(nil); //Release the cursor restriction
  {$ENDIF}

  FormMain.Close;
end;


procedure TKMMain.DoDeactivate(Sender: TObject);
begin
  //Occurs during Toggle to fullscreen, should be ignored
  if Application.Active then Exit;

  //Prevent the game window from being in the way by minimizing when alt-tabbing
  if (fGame <> nil) and (fGame.GlobalSettings <> nil) and fGame.GlobalSettings.FullScreen then
    Application.Minimize;
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


procedure TKMMain.DoTimer(Sender: TObject);
begin
  if fGame <> nil then
    fGame.UpdateState;
end;


procedure TKMMain.ToggleFullScreen(aSettings: TGlobalSettings; aReturnToOptions: boolean);
var VSync: Boolean;
begin
  //Remember as it gets wiped on game recreate
  VSync := aSettings.VSync;

  if aSettings.FullScreen then
    SetScreenResolution(ScreenRes[aSettings.ResolutionID].Width, ScreenRes[aSettings.ResolutionID].Height, aSettings.RefreshRate)
  else
    ResetResolution;

  FormLoading.Position := poScreenCenter;
  FormMain.ToggleFullscreen(aSettings.FullScreen);

  //It's required to re-init whole OpenGL related things when RC gets toggled fullscreen
  FreeThenNil(fGame); //Saves all settings into ini file in midst
  fGame := TKMGame.Create(
                          ExeDir,
                          FormMain.Panel5.Handle,
                          FormMain.Panel5.Width,
                          FormMain.Panel5.Height,
                          VSync,
                          FormLoading.LoadingStep,
                          FormLoading.LoadingText
                          );
  fGame.AfterConstruction(aReturnToOptions);
  fGame.OnCursorUpdate := StatusBarText;

  fLog.AppendLog('ToggleFullscreen');
  fLog.AppendLog('Form Width/Height: '+inttostr(FormMain.Width)+':'+inttostr(FormMain.Height));
  fLog.AppendLog('Panel Width/Height: '+inttostr(FormMain.Panel5.Width)+':'+inttostr(FormMain.Panel5.Height));

  ApplyCursorRestriction;
end;


procedure TKMMain.ReadAvailableResolutions;
var
  I,M,N: integer;
  {$IFDEF MSWindows}DevMode: TDevMode;{$ENDIF}
begin
  {$IFDEF MSWindows}
  //Clear
  FillChar(ScreenRes, SizeOf(ScreenRes), #0);

  I := 0;
  while EnumDisplaySettings(nil, I, DevMode) do
  with DevMode do
  begin
    Inc(I);
    //Take only 32bpp modes
    //Exclude rotated modes, as Win reports them too
    if (dmBitsPerPel = 32) and (dmPelsWidth > dmPelsHeight)
    and (dmPelsWidth >= 1024) and (dmPelsHeight >= 768)
    and (dmDisplayFrequency > 0) then
    begin
      //todo: There's a problem with VMware which reports ~20 resolutions which
      //A. do not fit
      //B. are not sorted
      //C. cause range-check-error since A is not checked properly (fixed, see comment below)
      //We should sort resolutions manually anyway,
      //and decide which resolutions to keep (highest 15 ?) or raise the limit or use a ComboBox

      //@Jimmy: Conditions are checked from left to right, so the following would lead to range error
      //        (ScreenRes[N].Width <> 0) and (N < RESOLUTION_COUNT)
      //since it first checks ScreenRes[N], where N could be already out of range
      //@Jimmy: You can do range checks manually setting _Count limit to very low, e.g.1 or 2 instead of 15

      //Find next empty place and avoid duplicating
      N := 1;
      while (N <= RESOLUTION_COUNT) and (ScreenRes[N].Width <> 0)
            and ((ScreenRes[N].Width <> dmPelsWidth) or (ScreenRes[N].Height <> dmPelsHeight)) do
        Inc(N);

      if (N <= RESOLUTION_COUNT) and (ScreenRes[N].Width = 0) then
      begin
        ScreenRes[N].Width := dmPelsWidth;
        ScreenRes[N].Height := dmPelsHeight;
      end;

      //Find next empty place and avoid duplicating
      M := 1;
      while (N <= RESOLUTION_COUNT) and (M <= REFRESH_RATE_COUNT)
            and (ScreenRes[N].RefRate[M] <> 0)
            and (ScreenRes[N].RefRate[M] <> dmDisplayFrequency) do
        Inc(M);

      if (M <= REFRESH_RATE_COUNT) and (N <= RESOLUTION_COUNT) and (ScreenRes[N].RefRate[M] = 0) then
        ScreenRes[N].RefRate[M] := dmDisplayFrequency;
    end;
  end;
  {$ENDIF}
  //sorting retrieved data
  SortScreenResData;
end;


{ //Jimmy: I suggest you take this scheme and move it to separate unit KM_Resolutions.pas

type
  TKMResolutions = class
  private
    fCount: Integer;
    fItems: array of TScreenResData;

    function GetItem(aIndex: Integer): TScreenResData;
    procedure ReadAvailable;
    procedure Sort;
    procedure Restore
  public
    constructor Create; //runs SaveCurrent, ReadAvailable, Sort
    destructor Destroy; //runs RestoreCurrent

    property Count read fCount; //Used by UI
    property Items[aIndex: Integer]: TScreenResData read GetItem; //Used by UI

    function FindBestMatch(aRes: TScreenResData): Integer; //Get best matching resolutions or use current
    procedure SetResolution(aIndex: Integer); //Apply the resolution
  end;

//For UserInterface please use 2 DropBoxes one below another
[Resolution  [\/]]
[RefreshRate [\/]]
That would be much less custom code.
}


procedure TKMMain.SortScreenResData;
var I,J,K:integer;
    TempScreenResData:TScreenResData;
    TempRefRate:Word;
begin
  for I:=1 to RESOLUTION_COUNT do
  begin
    for J:=1 to REFRESH_RATE_COUNT do
    begin
      //firstly, refresh rates for each resolution are being sorted
      K:=J;  //iterator will be modified, but we don't want to lose it
      while ((K>1) and (ScreenRes[I].RefRate[K] < ScreenRes[I].RefRate[K-1]) and
             //excluding zero values from sorting, so they are kept at the end of array
             (ScreenRes[I].RefRate[K] > 0)) do
      begin
        //simple replacement of data
        TempRefRate := ScreenRes[I].RefRate[K];
        ScreenRes[I].RefRate[K] := ScreenRes[I].RefRate[K-1];
        ScreenRes[I].RefRate[K-1] := TempRefRate;
        dec(K);
      end;
    end;
    if I=1 then continue;
    J:=I;  //iterator will be modified, but we don't want to lose it
    //moving resolution to its final position
    while ((J>1) and (((ScreenRes[J].Width < ScreenRes[J-1].Width) and
           //excluding zero values from sorting, so they are kept at the end of array
           (ScreenRes[J].Width > 0) and (ScreenRes[J].Height > 0)) or
           ((ScreenRes[J].Width = ScreenRes[J-1].Width) and
           (ScreenRes[J].Height < ScreenRes[J-1].Height)))) do
    begin
      //simple replacement of data
      TempScreenResData := ScreenRes[J];
      ScreenRes[J] := ScreenRes[J-1];
      ScreenRes[J-1] := TempScreenResData;
      dec(J);
    end;
  end;
end;


//Checks, whether resolution from INI file is correct
//and if not - reuse current resolution
procedure TKMMain.CheckResolution(aGameSettings: TGlobalSettings);
var I, J: Integer;
    ResolutionFound: Boolean;
    {$IFDEF MSWindows}DevMode: TDevMode;{$ENDIF}
begin
  {$IFDEF MSWindows}
  //Try to find matching Resolution
  ResolutionFound := False;
  for I := 1 to RESOLUTION_COUNT do
    if (ScreenRes[I].Width = aGameSettings.ResolutionWidth)
    and(ScreenRes[I].Height = aGameSettings.ResolutionHeight) then
      for J := 1 to REFRESH_RATE_COUNT do
        if (aGameSettings.RefreshRate = ScreenRes[I].RefRate[J]) then
          ResolutionFound := True;

  //Otherwise take current Resolution/RefreshRate
  if not ResolutionFound then
  begin
    EnumDisplaySettings(nil, Cardinal(-1){ENUM_CURRENT_SETTINGS}, DevMode);
    with DevMode do
    begin
      aGameSettings.ResolutionWidth := dmPelsWidth;
      aGameSettings.ResolutionHeight := dmPelsHeight;
      aGameSettings.RefreshRate := dmDisplayFrequency;
    end;
    //correct values must be saved immediately
    aGameSettings.SaveSettings(True);
  end;
  {$ENDIF}
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


procedure TKMMain.SetScreenResolution(Width, Height, RefreshRate: Word);
{$IFDEF MSWindows} var DeviceMode: DEVMODE; {$ENDIF}
begin
  {$IFDEF MSWindows}
  ZeroMemory(@DeviceMode, SizeOf(DeviceMode));

  with DeviceMode do
  begin
    dmSize := SizeOf(TDeviceMode);
    dmPelsWidth := Width;
    dmPelsHeight := Height;
    dmBitsPerPel := 32;
    dmDisplayFrequency := RefreshRate;
    dmFields := DM_DISPLAYFREQUENCY or DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  end;

  ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN);
  {$ENDIF}
end;


//Restore initial Windows resolution
procedure TKMMain.ResetResolution;
begin
  {$IFDEF MSWindows}
  //if (fGame = nil) or (fGame.GlobalSettings = nil) or fGame.GlobalSettings.FullScreen then
    ChangeDisplaySettings(DEVMODE(nil^), 0);
  {$ENDIF}
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
//@Lewin: Why?
//@Krom: I think it's for multiple monitors, it's very annoying if you play a fullscreen game and your cursor slides onto second monitor
//       instead of stopping at the edge as expected. But I think this restriction will still apply when you alt-tab
//       out, so we should disable it then so the player can use both screens. Needs testing.
//@Lewin: Thats the sideeffect of undocumented code - neither of us remembers why exactly it was brought in the first place
procedure TKMMain.ApplyCursorRestriction;
var Rect: TRect;
begin
  {$IFDEF MSWindows}
  if (fGame <> nil) and (fGame.GlobalSettings <> nil) and fGame.GlobalSettings.FullScreen then
  begin
    Rect := FormMain.BoundsRect;
    ClipCursor(@Rect);
  end
  else
    ClipCursor(nil); //Otherwise have no restriction
  {$ENDIF}
end;


end.
