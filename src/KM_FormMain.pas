unit KM_FormMain;
{$I KaM_Remake.inc}
interface
uses
  Classes, ComCtrls, Controls, Buttons, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus, StdCtrls, SysUtils, StrUtils, ShellAPI,
  KM_RenderControl, KM_Settings,
  {$IFDEF FPC} LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, Messages; {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType; {$ENDIF}


type
  TFormMain = class(TForm)
    MenuItem1: TMenuItem;
    N2: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Debug1: TMenuItem;
    Debug_PrintScreen: TMenuItem;
    Export1: TMenuItem;
    Export_GUIRX: TMenuItem;
    Export_TreesRX: TMenuItem;
    Export_HousesRX: TMenuItem;
    Export_UnitsRX: TMenuItem;
    Export_GUIMainRX: TMenuItem;
    Export_Custom: TMenuItem;
    Export_Tileset: TMenuItem;
    Export_Fonts1: TMenuItem;
    GroupBox1: TGroupBox;
    chkSuperSpeed: TCheckBox;
    Export_Deliverlists1: TMenuItem;
    Export_Sounds1: TMenuItem;
    Export_HouseAnim1: TMenuItem;
    Export_UnitAnim1: TMenuItem;
    RGPlayer: TRadioGroup;
    Button_Stop: TButton;
    OpenMissionMenu: TMenuItem;
    AnimData1: TMenuItem;
    Other1: TMenuItem;
    Debug_ShowPanel: TMenuItem;
    Export_TreeAnim1: TMenuItem;
    ExportMainMenu: TMenuItem;
    Debug_EnableCheats: TMenuItem;
    ExportUIPages: TMenuItem;
    Resources1: TMenuItem;
    HousesDat1: TMenuItem;
    GroupBox2: TGroupBox;
    chkShowOwnership: TCheckBox;
    chkShowNavMesh: TCheckBox;
    chkShowAvoid: TCheckBox;
    chkShowBalance: TCheckBox;
    tbOwnMargin: TTrackBar;
    tbOwnThresh: TTrackBar;
    Label5: TLabel;
    Label6: TLabel;
    chkShowDefences: TCheckBox;
    ResourceValues1: TMenuItem;
    GroupBox3: TGroupBox;
    chkUIControlsBounds: TCheckBox;
    chkUITextBounds: TCheckBox;
    GroupBox4: TGroupBox;
    tbAngleX: TTrackBar;
    tbAngleY: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    tbBuildingStep: TTrackBar;
    Label1: TLabel;
    GroupBox5: TGroupBox;
    tbPassability: TTrackBar;
    Label2: TLabel;
    chkShowRoutes: TCheckBox;
    chkShowWires: TCheckBox;
    tbAngleZ: TTrackBar;
    Label7: TLabel;
    chkSelectionBuffer: TCheckBox;
    GroupBoxLogs: TGroupBox;
    chkLogDelivery: TCheckBox;
    chkLogNetConnection: TCheckBox;
    RGLogNetPackets: TRadioGroup;
    chkLogsShowInChat: TCheckBox;
    chkUIControlsID: TCheckBox;
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Debug_ExportMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Debug_EnableCheatsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AboutClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure Debug_PrintScreenClick(Sender: TObject);
    procedure Export_TreesRXClick(Sender: TObject);
    procedure Export_HousesRXClick(Sender: TObject);
    procedure Export_UnitsRXClick(Sender: TObject);
    procedure Export_GUIClick(Sender: TObject);
    procedure Export_GUIMainRXClick(Sender: TObject);
    procedure Export_CustomClick(Sender: TObject);
    procedure Export_TilesetClick(Sender: TObject);
    procedure Export_Sounds1Click(Sender: TObject);
    procedure Export_HouseAnim1Click(Sender: TObject);
    procedure Export_UnitAnim1Click(Sender: TObject);
    procedure Export_Fonts1Click(Sender: TObject);
    procedure Export_DeliverLists1Click(Sender: TObject);
    procedure Button_StopClick(Sender: TObject);
    procedure RGPlayerClick(Sender: TObject);
    procedure Open_MissionMenuClick(Sender: TObject);
    procedure chkSuperSpeedClick(Sender: TObject);
    procedure Debug_ShowPanelClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Debug_ExportUIPagesClick(Sender: TObject);
    procedure HousesDat1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ResourceValues1Click(Sender: TObject);
    procedure ControlsUpdate(Sender: TObject);

    procedure RenderAreaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderAreaMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure RenderAreaMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderAreaMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RenderAreaResize(aWidth, aHeight: Integer);
    procedure RenderAreaRender(aSender: TObject);
  private
    fUpdating: Boolean;
    procedure FormKeyUpProc(aKey: Word; aShift: TShiftState);
    {$IFDEF MSWindows}
    function GetWindowParams: TKMWindowParamsRecord;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMExitSizeMove(var Msg: TMessage) ; message WM_EXITSIZEMOVE;
    procedure WMAppCommand(var Msg: TMessage); message WM_APPCOMMAND;
    {$ENDIF}
  public
    RenderArea: TKMRenderControl;
    procedure ControlsSetVisibile(aShowCtrls: Boolean);
    procedure ControlsReset;
    procedure ToggleFullscreen(aFullscreen, aWindowDefaultParams: Boolean);
  end;


implementation
//{$IFDEF WDC}
  {$R *.dfm}
//{$ENDIF}

uses
  KromUtils,
  KM_Defaults,
  KM_Main,
  //Use these units directly to avoid pass-through methods in fMain
  KM_Resource,
  KM_ResSprites,
  KM_GameApp,
  KM_HandsCollection,
  KM_ResSound,
  KM_Pics,
  KM_RenderPool,
  KM_Hand,
  KM_ResKeys,
  KM_Log;


//Remove VCL panel and use flicker-free TMyPanel instead
procedure TFormMain.FormCreate(Sender: TObject);
begin
  RenderArea := TKMRenderControl.Create(Self);
  RenderArea.Parent := Self;
  RenderArea.Align := alClient;
  RenderArea.Color := clMaroon;
  RenderArea.OnMouseDown := RenderAreaMouseDown;
  RenderArea.OnMouseMove := RenderAreaMouseMove;
  RenderArea.OnMouseUp := RenderAreaMouseUp;
  RenderArea.OnResize := RenderAreaResize;
  RenderArea.OnRender := RenderAreaRender;

  //Lazarus needs OnMouseWheel event to be for the panel, not the entire form
  {$IFDEF FPC} RenderArea.OnMouseWheel := RenderAreaMouseWheel; {$ENDIF}

  {$IFDEF MSWindows}
    //Means it will receive WM_SIZE WM_PAINT always in pair (if False - WM_PAINT is not called if size becames smaller)
    RenderArea.FullRepaint := True;
    RenderArea.BevelOuter := bvNone;
  {$ENDIF}

  //Put debug panel on top
  RenderArea.SendToBack;
  GroupBox1.BringToFront;
end;

procedure TFormMain.FormShow(Sender: TObject);
var BordersWidth, BordersHeight: Integer;
begin
  //We do this in OnShow rather than OnCreate as the window borders aren't
  //counted properly in OnCreate
  BordersWidth := Width - ClientWidth;
  BordersHeight := Height - ClientHeight;
  //Constraints includes window borders, so we add them on as Margin
  Constraints.MinWidth := MIN_RESOLUTION_WIDTH + BordersWidth;
  Constraints.MinHeight := MIN_RESOLUTION_HEIGHT + BordersHeight;

  // We have to put it here, to proper window positioning for multimonitor systems
  if not fMain.Settings.FullScreen then
  begin
    Left := fMain.Settings.WindowParams.Left;
    Top := fMain.Settings.WindowParams.Top;
  end;

end;


//Restrict minimum Form ClientArea size to MENU_DESIGN_X/Y
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if gGameApp <> nil then gGameApp.KeyDown(Key, Shift);
end;


procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if gGameApp <> nil then gGameApp.KeyPress(Key);
end;


procedure TFormMain.FormKeyUpProc(aKey: Word; aShift: TShiftState);
begin
  if aKey = gResKeys[SC_DEBUG_WINDOW].Key then begin
    SHOW_DEBUG_CONTROLS := not SHOW_DEBUG_CONTROLS;
    ControlsSetVisibile(SHOW_DEBUG_CONTROLS);
  end;

  if gGameApp <> nil then gGameApp.KeyUp(aKey, aShift);
end;


procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');

  FormKeyUpProc(Key, Shift);
end;


procedure TFormMain.RenderAreaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if gGameApp <> nil then gGameApp.MouseDown(Button, Shift, X, Y); end;


procedure TFormMain.RenderAreaMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin if gGameApp <> nil then gGameApp.MouseMove(Shift, X, Y); end;


procedure TFormMain.RenderAreaMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if gGameApp <> nil then
  begin
    //Somehow Shift state does not contain mouse buttons ssLeft/ssRight/ssMiddle
    if Button = mbLeft then
      Include(Shift, ssLeft)
    else if Button = mbRight then
      Include(Shift, ssRight)
    else if Button = mbMiddle then
      Include(Shift, ssMiddle);

    gGameApp.MouseUp(Button, Shift, X, Y);
  end;
end;


procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if gGameApp <> nil then gGameApp.MouseWheel(Shift, WheelDelta, RenderArea.ScreenToClient(MousePos).X, RenderArea.ScreenToClient(MousePos).Y); end;


procedure TFormMain.RenderAreaMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if gGameApp <> nil then gGameApp.MouseWheel(Shift, WheelDelta, MousePos.X, MousePos.Y); end;


procedure TFormMain.RenderAreaResize(aWidth, aHeight: Integer);
begin
  fMain.Resize(aWidth, aHeight, GetWindowParams);
end;


procedure TFormMain.RenderAreaRender(aSender: TObject);
begin
  fMain.Render;
end;


//Open
procedure TFormMain.Open_MissionMenuClick(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1, '', ExeDir, 'Knights & Merchants Mission (*.dat)|*.dat') then
    gGameApp.NewSingleMap(OpenDialog1.FileName, TruncateExt(ExtractFileName(OpenDialog1.FileName)));
end;


procedure TFormMain.MenuItem1Click(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1, '', ExeDir, 'Knights & Merchants Mission (*.dat)|*.dat') then
    gGameApp.NewMapEditor(OpenDialog1.FileName, 0, 0);
end;


//Exit
procedure TFormMain.ExitClick(Sender: TObject);
begin
  Close;
end;


//About
procedure TFormMain.AboutClick(Sender: TObject);
begin
  fMain.ShowAbout;
end;


//Debug Options
procedure TFormMain.Debug_EnableCheatsClick(Sender: TObject);
begin
  Debug_EnableCheats.Checked := not Debug_EnableCheats.Checked;
  DEBUG_CHEATS := Debug_EnableCheats.Checked;
end;


procedure TFormMain.Debug_PrintScreenClick(Sender: TObject);
begin
  if gGameApp <> nil then
    gGameApp.PrintScreen;
end;


procedure TFormMain.Debug_ShowPanelClick(Sender: TObject);
begin
  GroupBox1.Visible := not GroupBox1.Visible;
end;


//Exports
procedure TFormMain.Export_TreesRXClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxTrees);
end;

procedure TFormMain.Export_HousesRXClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxHouses);
end;

procedure TFormMain.Export_UnitsRXClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxUnits);
end;

procedure TFormMain.Export_GUIClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxGUI);
end;

procedure TFormMain.Export_GUIMainRXClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxGUIMain);
end;

procedure TFormMain.Export_CustomClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxCustom);
end;

procedure TFormMain.Export_TilesetClick(Sender: TObject);
begin
  gRes.Sprites.ExportToPNG(rxTiles);
end;

procedure TFormMain.Export_Sounds1Click(Sender: TObject);
begin
  gRes.Sounds.ExportSounds;
end;

procedure TFormMain.Export_TreeAnim1Click(Sender: TObject);
begin
  gRes.ExportTreeAnim;
end;

procedure TFormMain.Export_HouseAnim1Click(Sender: TObject);
begin
  gRes.ExportHouseAnim;
end;

procedure TFormMain.Export_UnitAnim1Click(Sender: TObject);
begin
  gRes.ExportUnitAnim;
end;

procedure TFormMain.HousesDat1Click(Sender: TObject);
begin
  gRes.Houses.ExportCSV(ExeDir + 'Export' + PathDelim + 'houses.dat.csv')
end;


procedure TFormMain.Export_Fonts1Click(Sender: TObject);
begin
  Assert(gRes <> nil, 'Can''t export Fonts cos they aren''t loaded yet');
  gRes.Fonts.ExportFonts;
end;


procedure TFormMain.Export_DeliverLists1Click(Sender: TObject);
var I: Integer;
begin
  if gHands = nil then Exit;
  //You could possibly cheat in multiplayer by seeing what supplies your enemy has
  if (gGameApp.Game <> nil) and (not gGameApp.Game.IsMultiplayer or MULTIPLAYER_CHEATS) then
  for I := 0 to gHands.Count - 1 do
    gHands[I].Deliveries.Queue.ExportToFile(ExeDir + 'Player_' + IntToStr(I) + '_Deliver_List.txt');
end;


procedure TFormMain.RGPlayerClick(Sender: TObject);
begin
  if (gGameApp.Game = nil)
  or gGameApp.Game.IsMapEditor
  or gGameApp.Game.IsMultiplayer then
    Exit;

  if (gHands <> nil) and (RGPlayer.ItemIndex < gHands.Count) then
    gMySpectator.HandIndex := RGPlayer.ItemIndex;
end;


procedure TFormMain.chkSuperSpeedClick(Sender: TObject);
begin
  if (gGameApp.Game = nil)
  or (gGameApp.Game.IsMultiplayer
    and not gGameApp.Game.IsMPGameSpeedUpAllowed
    and not MULTIPLAYER_SPEEDUP
    and not gGameApp.Game.IsReplay) then
    Exit;

  gGameApp.Game.SetGameSpeed(IfThen(chkSuperSpeed.Checked, 300, gGameApp.Game.GetNormalGameSpeed), False);
end;


procedure TFormMain.Button_StopClick(Sender: TObject);
begin
  if gGameApp.Game <> nil then
    if gGameApp.Game.IsMapEditor then
      gGameApp.Stop(gr_MapEdEnd)
    else
      gGameApp.Stop(gr_Cancel);
end;


//Revert all controls to defaults (e.g. before MP session)
procedure TFormMain.ControlsReset;
  procedure ResetGroupBox(aBox: TGroupBox);
  var
    I: Integer;
  begin
    for I := 0 to aBox.ControlCount - 1 do
      if aBox.Controls[I] is TCheckBox then
        TCheckBox(aBox.Controls[I]).Checked := aBox.Controls[I] = chkLogNetConnection
      else
      if aBox.Controls[I] is TTrackBar then
        TTrackBar(aBox.Controls[I]).Position := 0
      else
      if aBox.Controls[I] is TRadioGroup then
        TRadioGroup(aBox.Controls[I]).ItemIndex := 0
      else
      if (aBox.Controls[I] is TGroupBox) then
        ResetGroupBox(TGroupBox(aBox.Controls[I]));
  end;
begin
  fUpdating := True;
  ResetGroupBox(GroupBox1);

  tbOwnMargin.Position := OWN_MARGIN_DEF;
  tbOwnThresh.Position := OWN_THRESHOLD_DEF;

  fUpdating := False;
  ControlsUpdate(nil);
end;


procedure TFormMain.ControlsSetVisibile(aShowCtrls: Boolean);
var I: Integer;
begin
  Refresh;

  GroupBox1.Visible  := aShowCtrls;
  StatusBar1.Visible := aShowCtrls;

  //For some reason cycling Form.Menu fixes the black bar appearing under the menu upon making it visible.
  //This is a better workaround than ClientHeight = +20 because it works on Lazarus and high DPI where Menu.Height <> 20.
  Menu := nil;
  if aShowCtrls then Menu := MainMenu1;

  GroupBox1.Enabled  := aShowCtrls;
  StatusBar1.Enabled := aShowCtrls;
  for I := 0 to MainMenu1.Items.Count - 1 do
    MainMenu1.Items[I].Enabled := aShowCtrls;

  Refresh;

  RenderArea.Top    := 0;
  RenderArea.Height := ClientHeight;
  RenderArea.Width  := ClientWidth;
  fMain.Resize(RenderArea.Width, RenderArea.Height, GetWindowParams);
end;


procedure TFormMain.ControlsUpdate(Sender: TObject);
var
  I: Integer;
  AllowDebugChange: Boolean;
begin
  if fUpdating then Exit;

  //You could possibly cheat in multiplayer by seeing debug render info
  AllowDebugChange := (gGameApp.Game = nil)
                   or (not gGameApp.Game.IsMultiplayer or MULTIPLAYER_CHEATS)
                   or (Sender = nil); //Happens in ControlsReset only (using this anywhere else could allow MP cheating)

  //Debug render
  if AllowDebugChange then
  begin
    I := tbPassability.Position;
    tbPassability.Max := Byte(High(TKMTerrainPassability));
    Label2.Caption := IfThen(I <> 0, PassabilityGuiText[TKMTerrainPassability(I)], '');
    SHOW_TERRAIN_PASS := I;
    SHOW_TERRAIN_WIRES := chkShowWires.Checked;
    SHOW_UNIT_ROUTES := chkShowRoutes.Checked;
    SHOW_SEL_BUFFER := chkSelectionBuffer.Checked;
  end;

  //AI
  if AllowDebugChange then
  begin
    SHOW_AI_WARE_BALANCE := chkShowBalance.Checked;
    OVERLAY_DEFENCES := chkShowDefences.Checked;
    OVERLAY_AVOID := chkShowAvoid.Checked;
    OVERLAY_OWNERSHIP := chkShowOwnership.Checked;
    OVERLAY_NAVMESH := chkShowNavMesh.Checked;

    OWN_MARGIN := tbOwnMargin.Position;
    tbOwnThresh.Max := OWN_MARGIN;
    OWN_THRESHOLD := tbOwnThresh.Position;
  end;

  //UI
  SHOW_CONTROLS_OVERLAY := chkUIControlsBounds.Checked;
  SHOW_TEXT_OUTLINES := chkUITextBounds.Checked;
  SHOW_CONTROLS_ID := chkUIControlsID.Checked;

  //Graphics
  if AllowDebugChange then
  begin
    //Otherwise it could crash on the main menu
    if gRenderPool <> nil then
    begin
      RENDER_3D := False;//tbAngleX.Position + tbAngleY.Position <> 0;
      Label3.Caption := 'AngleX ' + IntToStr(tbAngleX.Position);
      Label4.Caption := 'AngleY ' + IntToStr(tbAngleY.Position);
      Label7.Caption := 'AngleZ ' + IntToStr(tbAngleZ.Position);
      gRenderPool.SetRotation(-tbAngleX.Position, -tbAngleZ.Position, -tbAngleY.Position);
      fMain.Render;
    end;
    HOUSE_BUILDING_STEP := tbBuildingStep.Position / tbBuildingStep.Max;
  end;

  //Logs
  SHOW_LOGS_IN_CHAT := chkLogsShowInChat.Checked;

  if AllowDebugChange then
  begin
    if chkLogDelivery.Checked then
      Include(gLog.MessageTypes, lmt_Delivery)
    else
      Exclude(gLog.MessageTypes, lmt_Delivery);

    if chkLogNetConnection.Checked then
      Include(gLog.MessageTypes, lmt_NetConnection)
    else
      Exclude(gLog.MessageTypes, lmt_NetConnection);

    case RGLogNetPackets.ItemIndex of
      0:    begin
              Exclude(gLog.MessageTypes, lmt_NetPacketOther);
              Exclude(gLog.MessageTypes, lmt_NetPacketCommand);
              Exclude(gLog.MessageTypes, lmt_NetPacketPingFps);
            end;
      1:    begin
              Include(gLog.MessageTypes, lmt_NetPacketOther);
              Exclude(gLog.MessageTypes, lmt_NetPacketCommand);
              Exclude(gLog.MessageTypes, lmt_NetPacketPingFps);
            end;
      2:    begin
              Include(gLog.MessageTypes, lmt_NetPacketOther);
              Include(gLog.MessageTypes, lmt_NetPacketCommand);
              Exclude(gLog.MessageTypes, lmt_NetPacketPingFps);
            end;
      3:    begin
              Include(gLog.MessageTypes, lmt_NetPacketOther);
              Include(gLog.MessageTypes, lmt_NetPacketCommand);
              Include(gLog.MessageTypes, lmt_NetPacketPingFps);
            end;
      else  raise Exception.Create('Unexpected RGLogNetPackets.ItemIndex = ' + IntToStr(RGLogNetPackets.ItemIndex));
    end;
  end;

end;


procedure TFormMain.ToggleFullscreen(aFullscreen, aWindowDefaultParams: Boolean);
begin
  if aFullScreen then begin
    Show; //Make sure the form is shown (e.g. on game creation), otherwise it won't wsMaximize
    BorderStyle  := bsSizeable; //if we don't set Form1 sizeable it won't maximize
    WindowState  := wsNormal;
    WindowState  := wsMaximized;
    BorderStyle  := bsNone;     //and now we can make it borderless again
  end else begin
    BorderStyle  := bsSizeable;
    WindowState  := wsNormal;
    if (aWindowDefaultParams) then
    begin
      Position := poScreenCenter;
      ClientWidth  := MENU_DESIGN_X;
      ClientHeight := MENU_DESIGN_Y;
      // We've set default window params, so update them
      fMain.UpdateWindowParams(GetWindowParams);
      // Unset NeedResetToDefaults flag
      fMain.Settings.WindowParams.NeedResetToDefaults := False;
    end else begin
      // Here we set window Width/Height and State
      // Left and Top will set on FormShow, so omit setting them here
      Position := poDesigned;
      ClientWidth  := fMain.Settings.WindowParams.Width;
      ClientHeight := fMain.Settings.WindowParams.Height;
      Left := fMain.Settings.WindowParams.Left;
      Top := fMain.Settings.WindowParams.Top;
      WindowState  := fMain.Settings.WindowParams.State;
    end;
  end;

  //Make sure Panel is properly aligned
  RenderArea.Align := alClient;
end;


// Return current window params
function TFormMain.GetWindowParams: TKMWindowParamsRecord;
  // FindTaskBar returns the Task Bar's position, and fills in
  // ARect with the current bounding rectangle.
  function FindTaskBar(var aRect: TRect): Integer;
  var	AppData: TAppBarData;
  begin
    Result := -1;
    // 'Shell_TrayWnd' is the name of the task bar's window
    AppData.Hwnd := FindWindow('Shell_TrayWnd', nil);
    if AppData.Hwnd <> 0 then
    begin
      AppData.cbSize := SizeOf(TAppBarData);
      // SHAppBarMessage will return False (0) when an error happens.
      if SHAppBarMessage(ABM_GETTASKBARPOS,
        {$IFDEF FPC}@AppData{$ENDIF}
        {$IFDEF WDC}AppData{$ENDIF}
        ) <> 0 then
      begin
        Result := AppData.uEdge;
        aRect := AppData.rc;
      end;
    end;
  end;
var
  Wp: TWindowPlacement;
  BordersWidth, BordersHeight: SmallInt;
  Rect: TRect;
begin
  Result.State := WindowState;
  case WindowState of
    wsMinimized:  ;
    wsNormal:     begin
                    Result.Width := ClientWidth;
                    Result.Height := ClientHeight;
                    Result.Left := Left;
                    Result.Top := Top;
                  end;
    wsMaximized:  begin
                    Wp.length := SizeOf(TWindowPlacement);
                    GetWindowPlacement(Handle, @Wp);

                    // Get current borders width/height
                    BordersWidth := Width - ClientWidth;
                    BordersHeight := Height - ClientHeight;

                    // rcNormalPosition do not have ClientWidth/ClientHeight
                    // so we have to calc it manually via substracting borders width/height
                    Result.Width := Wp.rcNormalPosition.Right - Wp.rcNormalPosition.Left - BordersWidth;
                    Result.Height := Wp.rcNormalPosition.Bottom - Wp.rcNormalPosition.Top - BordersHeight;

                    // Adjustment of window position due to TaskBar position/size
                    case FindTaskBar(Rect) of
                      ABE_LEFT: begin
                                  Result.Left := Wp.rcNormalPosition.Left + Rect.Right;
                                  Result.Top := Wp.rcNormalPosition.Top;
                                end;
                      ABE_TOP:  begin
                                  Result.Left := Wp.rcNormalPosition.Left;
                                  Result.Top := Wp.rcNormalPosition.Top + Rect.Bottom;
                                end
                      else      begin
                                  Result.Left := Wp.rcNormalPosition.Left;
                                  Result.Top := Wp.rcNormalPosition.Top;
                                end;
                    end;
                  end;
  end;
end;


{$IFDEF MSWindows}
procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  //If the system message is screensaver or monitor power off then trap the message and set its result to -1
  if (Msg.CmdType = SC_SCREENSAVE) or (Msg.CmdType = SC_MONITORPOWER) then
    Msg.Result := -1
  else
    inherited;
end;


// Handle extra mouse buttons (forward/backward)
procedure TFormMain.WMAppCommand(var Msg: TMessage);
  // Parse DwKeys flags to get ShiftState
  function GetShiftState(aDwKeys: Word): TShiftState;
  begin
    Result := [];
    if (aDwKeys and MK_LBUTTON) <> 0 then
      Include(Result, ssLeft)
    else if (aDwKeys and MK_RBUTTON) <> 0 then
      Include(Result, ssRight)
    else if (aDwKeys and MK_MBUTTON) <> 0 then
      Include(Result, ssMiddle)
    else if (aDwKeys and MK_CONTROL) <> 0 then
      Include(Result, ssCtrl)
    else if (aDwKeys and MK_SHIFT) <> 0 then
      Include(Result, ssShift);
  end;

var dwKeys,uDevice,cmd: Word;
  ShiftState: TShiftState;
begin
  ShiftState := [];
  uDevice := GET_DEVICE_LPARAM(Msg.lParam);
  if uDevice = FAPPCOMMAND_MOUSE then
  begin
    dwKeys := GET_KEYSTATE_LPARAM(Msg.lParam);
    ShiftState := GetShiftState(dwKeys);
    cmd := GET_APPCOMMAND_LPARAM(Msg.lParam);
    case cmd of
       APPCOMMAND_BROWSER_FORWARD:  FormKeyUpProc(VK_XBUTTON1, ShiftState);
       APPCOMMAND_BROWSER_BACKWARD: FormKeyUpProc(VK_XBUTTON2, ShiftState);
       else
         inherited;
     end;
  end;
end;


procedure TFormMain.WMExitSizeMove(var Msg: TMessage) ;
begin
  fMain.Move(GetWindowParams);
end;
{$ENDIF}


procedure TFormMain.Debug_ExportMenuClick(Sender: TObject);
begin
  ForceDirectories(ExeDir + 'Export' + PathDelim);
  gGameApp.MainMenuInterface.MyControls.SaveToFile(ExeDir + 'Export' + PathDelim + 'MainMenu.txt');
end;


procedure TFormMain.Debug_ExportUIPagesClick(Sender: TObject);
begin
  if (gGameApp.Game <> nil) and (gGameApp.Game.ActiveInterface <> nil) then
    gGameApp.Game.ActiveInterface.ExportPages(ExeDir + 'Export' + PathDelim)
  else
  if gGameApp.MainMenuInterface <> nil then
    gGameApp.MainMenuInterface.ExportPages(ExeDir + 'Export' + PathDelim);
end;


//Tell fMain if we want to shut down the program
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var MenuHidden: Boolean;
begin
  //Hacky solution to MessageBox getting stuck under main form: In full screen we must show
  //the menu while displaying a MessageBox otherwise it goes under the main form on some systems
  MenuHidden := (BorderStyle = bsNone) and (Menu = nil);
  if MenuHidden then Menu := MainMenu1;
  fMain.CloseQuery(CanClose);
  if MenuHidden then Menu := nil;
end;


procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMain.Stop(Self);
end;


procedure TFormMain.ResourceValues1Click(Sender: TObject);
begin
  gRes.Wares.ExportCostsTable('ResourceValues.txt');
end;


{$IFDEF FPC}
initialization
{$I KM_FormMain.lrs}
{$ENDIF}


end.
