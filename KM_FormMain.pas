unit KM_FormMain;
{$I KaM_Remake.inc}
interface
uses
  Classes, ComCtrls, Controls, Buttons, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus, StdCtrls, SysUtils, StrUtils, TypInfo,
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
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Debug1: TMenuItem;
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
    chkSuperSpeed: TCheckBox;
    Export_Text: TMenuItem;
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
    Export_GUIMainHRX: TMenuItem;
    ExportMainMenu: TMenuItem;
    Debug_EnableCheats: TMenuItem;
    ExportGamePages: TMenuItem;
    ExportMenuPages: TMenuItem;
    Resources1: TMenuItem;
    HousesDat1: TMenuItem;
    GroupBox2: TGroupBox;
    chkShowOwnership: TCheckBox;
    chkShowNavMesh: TCheckBox;
    chkShowForest: TCheckBox;
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
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Debug_ExportMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Debug_EnableCheatsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
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
    procedure Button_StopClick(Sender: TObject);
    procedure RGPlayerClick(Sender: TObject);
    procedure Open_MissionMenuClick(Sender: TObject);
    procedure chkSuperSpeedClick(Sender: TObject);
    procedure Debug_ShowPanelClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel5MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Debug_ExportGamePagesClick(Sender: TObject);
    procedure Debug_ExportMenuPagesClick(Sender: TObject);
    procedure HousesDat1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ResourceValues1Click(Sender: TObject);
    procedure ControlsUpdate(Sender: TObject);
  private
    fUpdating: Boolean;
    {$IFDEF MSWindows}
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    {$ENDIF}
  public
    procedure ControlsSetVisibile(aShowCtrls: Boolean);
    procedure ControlsReset;
    procedure ToggleFullscreen(aFullscreen: Boolean);
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
  KM_ResourceSprites,
  KM_Controls,
  KM_GameApp,
  KM_PlayersCollection,
  KM_Sound,
  KM_Pics,
  KM_RenderPool,
  KM_TextLibrary,
  KM_Locales;


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
var
  Margin: TPoint;
begin
  Margin.X := Width - ClientWidth;
  Margin.Y := Height - ClientHeight;
  Constraints.MinWidth := MIN_RESOLUTION_WIDTH + Margin.X;
  Constraints.MinHeight := MIN_RESOLUTION_HEIGHT + Margin.Y;

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
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if fGameApp <> nil then fGameApp.KeyDown(Key, Shift);
end;


procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');
  if fGameApp <> nil then fGameApp.KeyPress(Key);
end;


procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Assert(KeyPreview, 'MainForm should recieve all keys to pass them to fGame');

  if Key = VK_F11  then begin
    SHOW_DEBUG_CONTROLS := not SHOW_DEBUG_CONTROLS;
    ControlsSetVisibile(SHOW_DEBUG_CONTROLS);
  end;

  if fGameApp <> nil then fGameApp.KeyUp(Key, Shift);
end;


procedure TFormMain.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if fGameApp <> nil then fGameApp.MouseDown(Button, Shift, X, Y); end;


procedure TFormMain.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin if fGameApp <> nil then fGameApp.MouseMove(Shift, X, Y); end;


procedure TFormMain.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if fGameApp <> nil then fGameApp.MouseUp(Button, Shift, X, Y); end;


procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if fGameApp <> nil then fGameApp.MouseWheel(Shift, WheelDelta, Panel5.ScreenToClient(MousePos).X, Panel5.ScreenToClient(MousePos).Y); end;

procedure TFormMain.Panel5MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin if fGameApp <> nil then fGameApp.MouseWheel(Shift, WheelDelta, MousePos.X, MousePos.Y); end;


//Open
procedure TFormMain.Open_MissionMenuClick(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1, '', ExeDir, 'Knights & Merchants Mission (*.dat)|*.dat') then
    fGameApp.NewSingleMap(OpenDialog1.FileName, TruncateExt(ExtractFileName(OpenDialog1.FileName)));
end;


procedure TFormMain.MenuItem1Click(Sender: TObject);
begin
  if RunOpenDialog(OpenDialog1, '', ExeDir, 'Knights & Merchants Mission (*.dat)|*.dat') then
    fGameApp.NewMapEditor(OpenDialog1.FileName, 0, 0);
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
  if fGameApp <> nil then
    fGameApp.PrintScreen;
end;


procedure TFormMain.Debug_ShowPanelClick(Sender: TObject);
begin GroupBox1.Visible := not GroupBox1.Visible; end;


//Exports
procedure TFormMain.Export_TreesRXClick(Sender: TObject);   begin fResource.Sprites.ExportToPNG(rxTrees); end;
procedure TFormMain.Export_HousesRXClick(Sender: TObject);  begin fResource.Sprites.ExportToPNG(rxHouses); end;
procedure TFormMain.Export_UnitsRXClick(Sender: TObject);   begin fResource.Sprites.ExportToPNG(rxUnits); end;
procedure TFormMain.Export_GUIRXClick(Sender: TObject);     begin fResource.Sprites.ExportToPNG(rxGUI); end;
procedure TFormMain.Export_GUIMainRXClick(Sender: TObject); begin fResource.Sprites.ExportToPNG(rxGUIMain); end;
procedure TFormMain.Export_GUIMainHRXClick(Sender: TObject);begin {fResource.Sprites.ExportToPNG(rxGUIMainH);} end;
procedure TFormMain.Export_Sounds1Click(Sender: TObject);   begin fSoundLib.ExportSounds; end;
procedure TFormMain.Export_TreeAnim1Click(Sender: TObject); begin fResource.ExportTreeAnim; end;
procedure TFormMain.Export_HouseAnim1Click(Sender: TObject);begin fResource.ExportHouseAnim; end;
procedure TFormMain.Export_UnitAnim1Click(Sender: TObject); begin fResource.ExportUnitAnim;  end;
procedure TFormMain.HousesDat1Click(Sender: TObject);       begin fResource.HouseDat.ExportCSV(ExeDir+'Export\houses.dat.csv') end;


procedure TFormMain.Export_TextClick(Sender: TObject);
var I: Integer; MyTextLibrary: TTextLibrary;
begin
  for I := 0 to fLocales.Count-1 do
  begin
    //Don't mess up the actual text library by loading other locales
    MyTextLibrary := TTextLibrary.Create(ExeDir+'data\text\', fLocales[i].Code);
    MyTextLibrary.ExportTextLibraries;
    MyTextLibrary.Free;
  end;
end;

procedure TFormMain.Export_Fonts1Click(Sender: TObject);
begin
  Assert(fResource <> nil, 'Can''t export Fonts cos they aren''t loaded yet');
  fResource.ResourceFont.ExportFonts(fGameApp.GameSettings.Locale);
end;


procedure TFormMain.Export_DeliverLists1Click(Sender: TObject);
var I: Integer;
begin
  if fPlayers = nil then Exit;
  //You could possibly cheat in multiplayer by seeing what supplies your enemy has
  if (fGameApp.Game <> nil) and (not fGameApp.Game.IsMultiplayer or MULTIPLAYER_CHEATS) then
  for I := 0 to fPlayers.Count - 1 do
    fPlayers[I].Deliveries.Queue.ExportToFile(ExeDir + 'Player_' + IntToStr(I) + '_Deliver_List.txt');
end;


procedure TFormMain.RGPlayerClick(Sender: TObject);
begin
  if (fGameApp.Game = nil)
  or fGameApp.Game.IsMapEditor
  or fGameApp.Game.IsMultiplayer then
    Exit;

  if (fPlayers <> nil) and (RGPlayer.ItemIndex < fPlayers.Count) then
    MyPlayer := fPlayers[RGPlayer.ItemIndex];
end;


procedure TFormMain.chkSuperSpeedClick(Sender: TObject);
begin
  if (fGameApp.Game = nil)
  or (fGameApp.Game.IsMultiplayer and not MULTIPLAYER_SPEEDUP and not fGameApp.Game.IsReplay) then
    Exit;

  fGameApp.Game.SetGameSpeed(IfThen(chkSuperSpeed.Checked, 300, 1));
end;


procedure TFormMain.Button_StopClick(Sender: TObject);
begin
  if fGameApp.Game <> nil then
    if fGameApp.Game.IsMapEditor then
      fGameApp.Stop(gr_MapEdEnd)
    else
      fGameApp.Stop(gr_Cancel);
end;


//Revert all controls to defaults (e.g. before MP session)
procedure TFormMain.ControlsReset;
  procedure ResetGroupBox(aBox: TGroupBox);
  var
    I: Integer;
  begin
    for I := 0 to aBox.ControlCount - 1 do
    if aBox.Controls[I] is TCheckBox then
      TCheckBox(aBox.Controls[I]).Checked := False
    else
    if aBox.Controls[I] is TTrackBar then
      TTrackBar(aBox.Controls[I]).Position := 0
    else
    if aBox.Controls[I] is TGroupBox then
      ResetGroupBox(TGroupBox(aBox.Controls[I]));
  end;
begin
  fUpdating := True;
  ResetGroupBox(GroupBox1);

  tbOwnMargin.Position := OWN_MARGIN;
  tbOwnThresh.Position := OWN_THRESHOLD;

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

  Panel5.Top    := 0;
  Panel5.Height := ClientHeight;
  Panel5.Width  := ClientWidth;

  fMain.Resize(Panel5.Width, Panel5.Height);
end;


procedure TFormMain.ControlsUpdate(Sender: TObject);
var
  I: Integer;
  AllowDebugChange: Boolean;
begin
  if fUpdating then Exit;

  //You could possibly cheat in multiplayer by seeing debug render info
  AllowDebugChange := (fGameApp.Game = nil)
                   or (not fGameApp.Game.IsMultiplayer or MULTIPLAYER_CHEATS)
                   or ((Sender is TCheckBox) and not TCheckBox(Sender).Checked)
                   or ((Sender is TTrackBar) and (TTrackBar(Sender).Position = 0))
                   or (Sender = nil);

  //Debug render
  if AllowDebugChange then
  begin
    I := tbPassability.Position;
    tbPassability.Max := Byte(High(TPassability));
    Label2.Caption := IfThen(I <> 0, GetEnumName(TypeInfo(TPassability), I), '');
    SHOW_TERRAIN_PASS := I;
    SHOW_TERRAIN_WIRES := chkShowWires.Checked;
    SHOW_UNIT_ROUTES := chkShowRoutes.Checked;
  end;

  //AI
  if AllowDebugChange then
  begin
    SHOW_AI_WARE_BALANCE := chkShowBalance.Checked;
    OVERLAY_DEFENCES := chkShowDefences.Checked;
    OVERLAY_AVOID := chkShowAvoid.Checked;
    OVERLAY_FOREST := chkShowForest.Checked;
    OVERLAY_OWNERSHIP := chkShowOwnership.Checked;
    OVERLAY_NAVMESH := chkShowNavMesh.Checked;

    OWN_MARGIN := tbOwnMargin.Position;
    tbOwnThresh.Max := OWN_MARGIN;
    OWN_THRESHOLD := tbOwnThresh.Position;
  end;

  //UI
  SHOW_CONTROLS_OVERLAY := chkUIControlsBounds.Checked;
  SHOW_TEXT_OUTLINES := chkUITextBounds.Checked;

  //Graphics
  if AllowDebugChange then
  begin
    //Otherwise it could crash on the main menu
    if fRenderPool <> nil then
    begin
      RENDER_3D := tbAngleX.Position + tbAngleY.Position <> 0;
      Label3.Caption := 'AngleX ' + IntToStr(tbAngleX.Position);
      Label4.Caption := 'AngleY ' + IntToStr(tbAngleY.Position);
      fRenderPool.SetRotation(-tbAngleX.Position, 0, -tbAngleY.Position);
      fMain.Render;
    end;
    HOUSE_BUILDING_STEP := tbBuildingStep.Position / tbBuildingStep.Max;
  end;
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
    inherited;
end;
{$ENDIF}


procedure TFormMain.Debug_ExportMenuClick(Sender: TObject);
begin
  ForceDirectories(ExeDir + 'Export\');
  fGameApp.MainMenuInterface.MyControls.SaveToFile(ExeDir + 'Export\MainMenu.txt');
end;


procedure TFormMain.Debug_ExportMenuPagesClick(Sender: TObject);
var
  I, K: Integer;
  MC: TKMMasterControl;
begin
  if fGameApp.MainMenuInterface = nil then Exit;

  MC := fGameApp.MainMenuInterface.MyControls;
  ForceDirectories(ExeDir + 'Export\MainMenu\');

  for I := 1 to MC.MainPanel.ChildCount do
    if MC.MainPanel.Childs[I] is TKMPanel then
    begin
      //Hide all other panels
      for K := 1 to MC.MainPanel.ChildCount do
        if MC.MainPanel.Childs[K] is TKMPanel then
          MC.MainPanel.Childs[K].Hide;

      MC.MainPanel.Childs[I].Show;

      fGameApp.Render;
      fGameApp.PrintScreen(ExeDir + 'Export\MainMenu\Panel' + int2fix(I, 3) + '.jpg');
    end;
end;

procedure TFormMain.Debug_ExportGamePagesClick(Sender: TObject);
var
  I, K: Integer;
  MC: TKMMasterControl;
begin
  if (fGameApp.Game = nil) or (fGameApp.Game.GamePlayInterface = nil) then Exit;

  MC := fGameApp.Game.GamePlayInterface.MyControls;
  ForceDirectories(ExeDir + 'Export\GamePlay\');

  for I := 1 to MC.MainPanel.ChildCount do
    if MC.MainPanel.Childs[I] is TKMPanel then
    begin
      //Hide all other panels
      for K := 1 to MC.MainPanel.ChildCount do
        if MC.MainPanel.Childs[K] is TKMPanel then
          MC.MainPanel.Childs[K].Hide;

      MC.MainPanel.Childs[I].Show;

      fGameApp.Render;
      fGameApp.PrintScreen(ExeDir + 'Export\GamePlay\Panel' + int2fix(I, 3) + '.jpg');
    end;
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
  fResource.Resources.ExportCostsTable('ResourceValues.txt');
end;


{$IFDEF FPC}
initialization
{$I KM_FormMain.lrs}
{$ENDIF}


end.
