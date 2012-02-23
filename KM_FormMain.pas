unit KM_FormMain;
{$I KaM_Remake.inc}
interface
uses
  Classes, ComCtrls, Controls, Buttons, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus, StdCtrls, SysUtils,
  {$IFDEF FPC} LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, Messages; {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType; {$ENDIF}


type
  //Custom flicker-free Panel
  TMyPanel = class(TPanel)
  private
    LastScreenPos: TPoint;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  published
    property OnMouseWheel; //Required for Lazarus
  end;


type
  TFormMain = class(TForm)
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
    ExportMainMenu1: TMenuItem;
    Button_CalcArmy: TButton;
    Debug_EnableCheats: TMenuItem;
    ShowAIAttacks1: TMenuItem;
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure TB_Angle_Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExportMainMenu1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button_CalcArmyClick(Sender: TObject);
    procedure Debug_EnableCheatsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure Debug_ShowWiresClick(Sender: TObject);
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
    procedure Panel5MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ShowAIAttacks1Click(Sender: TObject);
  private
    {$IFDEF MSWindows}
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    {$ENDIF}
  public
    procedure ToggleControlsVisibility(aShowCtrls: Boolean);
    procedure ToggleFullscreen(aFullscreen: Boolean);
  end;


var
  FormMain: TFormMain;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}

uses
  KromUtils,
  KM_Defaults,
  KM_Log,
  KM_Main,
  KM_Points,
  //Use these units directly to avoid pass-through methods in fMain
  KM_Resource,
  KM_ResourceSprites,
  KM_Game, KM_PlayersCollection,
  KM_Sound,
  KM_RenderPool,
  KM_TextLibrary,
  KM_GameInputProcess;


{ TMyPanel }
procedure TMyPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  //Do not erase background, thats causing BG color flickering on repaint
  //Just tell it's done
  Message.Result := 1;
end;


procedure TMyPanel.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  P: TPoint;
begin
  P := ClientToScreen(Classes.Point(0, 0));
  //It is said to help with black borders bug in Windows
  if (LastScreenPos.X <> P.X) or (LastScreenPos.Y <> P.Y) then
  begin
    PostMessage(Handle, WM_SIZE, SIZE_RESTORED, Width + (Height shl 16));
    LastScreenPos := P;
  end;

  BeginPaint(Handle, PS);
  try
    fMain.Render;
  finally
    EndPaint(Handle, PS);
  end;

  inherited;
end;


//Let the renderer know that client area size has changed ASAP
//Handling it through VCL functions takes too long and Panel is already filled with bg color
procedure TMyPanel.WMSize(var Message: TWMSize);
begin
  //inherited;
  fMain.Resize(Message.Width, Message.Height);
end;


//Remove VCL panel and use flicker-free TMyPanel instead
procedure TFormMain.FormCreate(Sender: TObject);
begin
  RemoveControl(Panel5);

  Panel5 := TMyPanel.Create(Self);
  Panel5.Parent := Self;
  Panel5.BevelOuter := bvNone;
  Panel5.Align := alClient;
  Panel5.Color := clMaroon;
  Panel5.OnMouseDown := Panel1MouseDown;
  Panel5.OnMouseMove := Panel1MouseMove;
  Panel5.OnMouseUp := Panel1MouseUp;
  //Lazarus needs OnMouseWheel event to be for the panel, not the entire form
  {$IFDEF FPC} TMyPanel(Panel5).OnMouseWheel := Panel5MouseWheel; {$ENDIF}

  //Means it will receive WM_SIZE WM_PAINT always in pair (if False - WM_PAINT is not called if size becames smaller)
  Panel5.FullRepaint := True;

  //Put debug panel on top
  Panel5.SendToBack;
  GroupBox1.BringToFront;
end;


//Restrict minimum Form ClientArea size to MENU_DESIGN_X/Y
procedure TFormMain.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
  Margin: TPoint;
begin
  Margin.X := Width - ClientWidth;
  Margin.Y := Height - ClientHeight;

  NewWidth := Math.max(NewWidth, MENU_DESIGN_X + Margin.X);
  NewHeight := Math.max(NewHeight, MENU_DESIGN_Y + Margin.Y);

  Resize := True;
end;


procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if fGame<>nil then
    fGame.KeyDown(Key, Shift);
end;


procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if fGame<>nil then fGame.KeyPress(Key);
end;


procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');

  if Key = VK_F11  then begin
    SHOW_DEBUG_CONTROLS := not SHOW_DEBUG_CONTROLS;
    ToggleControlsVisibility(SHOW_DEBUG_CONTROLS);
  end;

  if fGame<>nil then fGame.KeyUp(Key, Shift);
end;


procedure TFormMain.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if fGame<>nil then fGame.MouseDown(Button, Shift, X, Y); end;


procedure TFormMain.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin if fGame<>nil then fGame.MouseMove(Shift, X, Y); end;


procedure TFormMain.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if fGame<>nil then fGame.MouseUp(Button, Shift, X, Y); end;


procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if fGame<>nil then fGame.MouseWheel(Shift, WheelDelta, Panel5.ScreenToClient(MousePos).X, Panel5.ScreenToClient(MousePos).Y); end;

procedure TFormMain.Panel5MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if fGame<>nil then fGame.MouseWheel(Shift, WheelDelta, MousePos.X, MousePos.Y); end;


//Open
procedure TFormMain.Open_MissionMenuClick(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then
    fGame.StartSingleMap(OpenDialog1.FileName, TruncateExt(ExtractFileName(OpenDialog1.FileName)));
end;


procedure TFormMain.MenuItem1Click(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then
    fGame.StartMapEditor(OpenDialog1.FileName, false, 0, 0);
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
procedure TFormMain.Debug_ShowWiresClick(Sender: TObject);
begin
  Debug_ShowWires.Checked := not Debug_ShowWires.Checked;
  SHOW_TERRAIN_WIRES := Debug_ShowWires.Checked;
end;


procedure TFormMain.Debug_ShowOverlayClick(Sender: TObject);
begin
  Debug_ShowOverlay.Checked := not Debug_ShowOverlay.Checked;
  SHOW_CONTROLS_OVERLAY := Debug_ShowOverlay.Checked;
end;


procedure TFormMain.Debug_ShowUnitClick(Sender: TObject);
begin
  Debug_ShowUnits.Checked := not Debug_ShowUnits.Checked;
  SHOW_UNIT_MOVEMENT := Debug_ShowUnits.Checked;
  SHOW_UNIT_ROUTES   := Debug_ShowUnits.Checked;
end;


procedure TFormMain.Debug_EnableCheatsClick(Sender: TObject);
begin
  Debug_EnableCheats.Checked := not Debug_EnableCheats.Checked;
  DEBUG_CHEATS := Debug_EnableCheats.Checked;
end;


procedure TFormMain.Debug_PrintScreenClick(Sender: TObject);
begin
  if fGame <> nil then
    fGame.PrintScreen;
end;


procedure TFormMain.Debug_ShowPanel1Click(Sender: TObject);
begin GroupBox1.Visible := not GroupBox1.Visible; end;


procedure TFormMain.Debug_PassabilityTrackChange(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES:=Debug_PassabilityTrack.Position<>0;
  Debug_PassabilityTrack.Max:=length(PassabilityStr);
  if Debug_PassabilityTrack.Position <> 0 then
    Label2.Caption := PassabilityStr[TPassability(Debug_PassabilityTrack.Position)]
  else
    Label2.Caption := '';
  if fGame<> nil then
    fGame.FormPassability := Debug_PassabilityTrack.Position;
end;


//Exports
procedure TFormMain.Export_TreesRXClick(Sender: TObject);   begin fResource.Sprites.ExportToBMP(rxTrees); end;
procedure TFormMain.Export_HousesRXClick(Sender: TObject);  begin fResource.Sprites.ExportToBMP(rxHouses); end;
procedure TFormMain.Export_UnitsRXClick(Sender: TObject);   begin fResource.Sprites.ExportToBMP(rxUnits); end;
procedure TFormMain.Export_GUIRXClick(Sender: TObject);     begin fResource.Sprites.ExportToBMP(rxGUI); end;
procedure TFormMain.Export_GUIMainRXClick(Sender: TObject); begin fResource.Sprites.ExportToBMP(rxGUIMain); end;
procedure TFormMain.Export_GUIMainHRXClick(Sender: TObject);begin fResource.Sprites.ExportToBMP(rxGUIMainH); end;
procedure TFormMain.Export_Sounds1Click(Sender: TObject);   begin fSoundLib.ExportSounds; end;
procedure TFormMain.Export_TreeAnim1Click(Sender: TObject); begin fResource.ExportTreeAnim; end;
procedure TFormMain.Export_HouseAnim1Click(Sender: TObject);begin fResource.ExportHouseAnim; end;
procedure TFormMain.Export_UnitAnim1Click(Sender: TObject); begin fResource.ExportUnitAnim;  end;
procedure TFormMain.Export_TextClick(Sender: TObject);      begin fTextLibrary.ExportTextLibraries; end;

procedure TFormMain.Export_Fonts1Click(Sender: TObject);
begin
  fLog.AssertToLog(fResource<>nil, 'Can''t export Fonts cos they aren''t loaded yet');
  fResource.ResourceFont.ExportFonts(fGame.GlobalSettings.Locale);
end;


procedure TFormMain.Export_DeliverLists1Click(Sender: TObject);
var i:integer;
begin
  if fPlayers=nil then exit;
  for i:=0 to fPlayers.Count-1 do
    fPlayers[i].DeliverList.ExportToFile(ExeDir+'Player_'+inttostr(i)+'_Deliver_List.txt');
end;


procedure TFormMain.RGPlayerClick(Sender: TObject);
begin
  if (fGame.GameState in [gsNoGame, gsEditor]) or fGame.MultiplayerMode then exit;
  if (fPlayers<>nil) and (RGPlayer.ItemIndex < fPlayers.Count) then
    fGame.GameInputProcess.CmdTemp(gic_TempChangeMyPlayer, RGPlayer.ItemIndex);
end;


procedure TFormMain.CheckBox2Click(Sender: TObject);
begin
  if (fGame.GameState in [gsNoGame, gsEditor]) or (fGame.MultiplayerMode and not fGame.ReplayMode) then exit;
  if CheckBox2.Checked then fGame.SetGameSpeed(300) else fGame.SetGameSpeed(1);
end;


procedure TFormMain.Button_StopClick(Sender: TObject);
begin
  fGame.Stop(gr_Cancel);
end;


procedure TFormMain.Button_CalcArmyClick(Sender: TObject);
begin // For test Army evaluation
  fGame.StartSingleMap('', 'TestCalcArmy');
  Assert(MyPlayer<>nil);
  {Point.X := 10; Point.Y := 10;
  MyPlayer.AddUnitGroup(ut_Bowman, Point, dir_E, 3, 50);}
  MyPlayer.AddUnitGroup(ut_Militia, KMPoint(12,31), dir_E, 2, 20);
  fPlayers[1].AddUnitGroup(ut_AxeFighter, KMPoint(30,31), dir_W, 2, 10);
  {Point.X := 32; Point.Y := 10;
  fPlayers[1].AddUnitGroup(ut_Bowman, Point, dir_W, 1, 1);}
end;


procedure TFormMain.TB_Angle_Change(Sender: TObject);
begin
  RENDER_3D := TB_Angle.Position <> 0;
  Label3.Caption := IntToStr(TB_Angle.Position) + ' 3D';
  fRenderPool.SetRotation(-TB_Angle.Position, 0, 0);
  fMain.Render;
end;


procedure TFormMain.ToggleControlsVisibility(aShowCtrls: Boolean);
var i:integer;
begin
  Refresh;

  GroupBox1.Visible  := aShowCtrls;
  StatusBar1.Visible := aShowCtrls;
  for i:=1 to MainMenu1.Items.Count do
    MainMenu1.Items[i-1].Visible := aShowCtrls;

  //For some reason cycling Form.Menu fixes the black bar appearing under the menu upon making it visible.
  //This is a better workaround than ClientHeight = +20 because it works on Lazarus and high DPI where Menu.Height <> 20.
  Menu := nil;
  Menu := MainMenu1;

  GroupBox1.Enabled  := aShowCtrls;
  StatusBar1.Enabled := aShowCtrls;
  for i:=1 to MainMenu1.Items.Count do
    MainMenu1.Items[i-1].Enabled := aShowCtrls;

  Refresh;

  Panel5.Top    := 0;
  Panel5.Height := ClientHeight;
  Panel5.Width  := ClientWidth;

  fMain.Resize(Panel5.Width, Panel5.Height);
end;


procedure TFormMain.ToggleFullscreen(aFullscreen: Boolean);
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
    ClientWidth  := MENU_DESIGN_X;
    ClientHeight := MENU_DESIGN_Y;
    Position     := poScreenCenter;
  end;

  //Make sure Panel is properly aligned
  Panel5.Align := alClient;
end;


{$IFDEF MSWindows}
procedure TFormMain.WMSysCommand(var Msg: TWMSysCommand);
begin
  //If the system message is screensaver or monitor power off then trap the message and set its result to -1
  if (Msg.CmdType = SC_SCREENSAVE) or (Msg.CmdType = SC_MONITORPOWER) then
    Msg.Result := -1
  else
    Inherited;
end;
{$ENDIF}


procedure TFormMain.ExportMainMenu1Click(Sender: TObject);
begin
  //fGame.fMainMenuInterface.MyControls.SaveToFile(ExeDir+'MainMenu.txt');
end;


//Consult with the fGame if we can shut down the program
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (fGame <> nil) and not fGame.CanClose then
    CanClose := MessageDlg('Any unsaved changes will be lost. Exit?',mtWarning,[mbYes, mbNo],0) = mrYes
  else
    CanClose := True;
end;


procedure TFormMain.ShowAIAttacks1Click(Sender: TObject);
var i: Integer; s: string;
begin
  s := '';

  for i:=0 to fPlayers.Count-1 do
    s := s + IntToStr(i) + eol + fPlayers[i].AI.Attacks.GetAsText + eol;

  ShowMessage(s);
end;


{$IFDEF FPC}
initialization
{$I KM_FormMain.lrs}
{$ENDIF}


end.
