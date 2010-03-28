unit KM_Unit1;
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, Buttons, Messages,
  Math, SysUtils, KromUtils,
  {$IFDEF VER140} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, LResources, {$ENDIF}
  dglOpenGL, MMSystem,
  KM_Render, KM_ResourceGFX, KM_Defaults, KM_Form_Loading, KM_Terrain,
  KM_Game, KM_Units, KM_Houses, KM_Viewport, KM_PlayersCollection, ColorPicker,
  KM_LoadLib, KM_SoundFX,
  {$IFDEF VER140} MPlayer, {$ENDIF}
  KM_Utils;

type

  { TForm1 }

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
    TeamColorPicker: TShape;
    CheckBox2: TCheckBox;
    Export_Text: TMenuItem;
    Export_Deliverlists1: TMenuItem;
    Export_Sounds1: TMenuItem;
    Debug_PassabilityTrack: TTrackBar;
    Label2: TLabel;
    Export_HouseAnim1: TMenuItem;
    Export_UnitAnim1: TMenuItem;
    RGPlayer: TRadioGroup;
    Button_V: TButton;
    Button_6: TButton;
    Button_Stop: TButton;
    OpenMissionMenu: TMenuItem;
    Button_1: TButton;
    Debug_ShowOverlay: TMenuItem;
    AnimData1: TMenuItem;
    Other1: TMenuItem;
    Debug_ShowPanel1: TMenuItem;
    Button_W: TButton;
    Export_TreeAnim1: TMenuItem;
    Export_GUIMainHRX: TMenuItem;
    TB_Angle: TTrackBar;
    Label3: TLabel;
    Label1: TLabel;
    {$IFDEF VER140} MediaPlayer1: TMediaPlayer; {$ENDIF}
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure TB_Angle_Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  published
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure Debug_ShowWiresClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button_VClick(Sender: TObject);
    procedure Button_6Click(Sender: TObject);
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
    procedure TeamColorPickerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TeamColorPickerDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Debug_PassabilityTrackChange(Sender: TObject);
    procedure Debug_ShowOverlayClick(Sender: TObject);
    procedure Button_StopClick(Sender: TObject);
    procedure RGPlayerClick(Sender: TObject);
    procedure Open_MissionMenuClick(Sender: TObject);
    procedure Button_1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Debug_ShowUnitClick(Sender: TObject);
    procedure Debug_ShowPanel1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Button_WClick(Sender: TObject);
    procedure SetScreenResolution(Width, Height, RefreshRate: word);
    procedure ResetResolution;
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WMSysCommand(var Msg : TWMSysCommand); message WM_SYSCOMMAND;
    procedure ReadAvailableResolutions;
  public
    procedure ApplyCursorRestriction;
    procedure ToggleControlsVisibility(ShowCtrls:boolean);
    procedure ToggleFullScreen(Toggle:boolean; ResolutionID:word; ReturnToOptions:boolean);
  end;

var
  Form1: TForm1;
  FormLoading:TFormLoading;
  TextT:GLuint; //Tiles

implementation
{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

uses KM_Settings, KM_CommonTypes, KM_TGATexture;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var FrameTime:cardinal;
begin
  if not Form1.Active then exit;

  //Counting FPS
  begin
    FrameTime:=TimeGetTime-OldTimeFPS;
    OldTimeFPS:=TimeGetTime;

    if (FPS_LAG<>1)and(FrameTime<FPS_LAG) then begin
      sleep(FPS_LAG-FrameTime);
      FrameTime:=FPS_LAG;
    end;

    inc(OldFrameTimes,FrameTime);
    inc(FrameCount);
    if OldFrameTimes>=FPS_INTERVAL then begin
      StatusBar1.Panels[2].Text:=floattostr(round((1000/(OldFrameTimes/FrameCount))*10)/10)+' fps ('+inttostr(1000 div FPS_LAG)+')';
      OldFrameTimes:=0;
      FrameCount:=0;
    end;
  end; //FPS calculation complete

  fGame.UpdateStateIdle(FrameTime);
  fRender.Render;
  done := false; //repeats OnIdle event
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  TempSettings:TGameSettings;
  s:string;
begin
  if Sender<>nil then exit;

  FormLoading.Label5.Caption := GAME_VERSION;
  FormLoading.Show; //This is our splash screen
  FormLoading.Refresh;

  //Randomize; //Randomize the random seed to ensure that we don't get repeditive patterns,
  //but we need this to be Off to reproduce bugs
  TimeBeginPeriod(1); //initialize timer precision
  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log'); //First thing - create a log

  ReadAvailableResolutions;    //Undecided as to how this will fit in with the game, see discussion

  Panel5.Color := clBlack;

  TempSettings:=TGameSettings.Create; //Read settings (fullscreen property and resolutions)
                                       //Don't need to free it here, it's FreeAndNil'ed at fGame re-init
  ToggleFullScreen(TempSettings.IsFullScreen, TempSettings.GetResolutionID, false); //Now we can decide whether we should make it full screen or not

  TempSettings.Free; //does not required any more

  //We don't need to re-init fGame since it's already handled in ToggleFullScreen (sic!)
  //fGame:=TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height, true);

  Application.OnIdle:=Form1.OnIdle;

  ToggleControlsVisibility(ShowDebugControls);

  fLog.AppendLog('Form1 create is done');

  {$IFDEF FPC}
  LoadTexture(ExeDir+'Resource\Tiles1.tga', TextT, 0); //todo: fix this workaround for Lazarus
  {$ENDIF}

  //Show the message if user has old OpenGL drivers (pre-1.4)
  if not GL_VERSION_1_4 then
  begin
    s := 'Old OpenGL version detected, game may run slowly and/or with graphic flaws'+eol+
         'Please update your graphic drivers to get better performance';
    Application.MessageBox(@(s)[1],'Warning',MB_OK + MB_ICONEXCLAMATION);
  end;
  Timer100ms.Interval := fGame.fGameSettings.GetPace; //FormLoading gets hidden OnTimer event
  Form1.Caption := 'KaM Remake - ' + GAME_VERSION;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  //Reset the resolution
  ResetResolution;
  fGame.StopGame(gr_Silent);
  FreeAndNil(fGame);
  FreeAndNil(fLog);
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


procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLog.AssertToLog(Form1.KeyPreview, 'Form1 should recieve all keys to pass them to fGame');
  fGame.KeyUp(Key, Shift);
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 fLog.AssertToLog(Form1.KeyPreview, 'Form1 should recieve all keys to pass them fo fGame');
 fGame.KeyUp(Key, Shift, true);
end;


procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseDown(Button, Shift, X, Y); end;


procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin fGame.MouseMove(Shift, X, Y); end;


procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseUp(Button, Shift, X, Y); end;


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (MOUSEWHEEL_ZOOM_ENABLE) and (fGame.GameState in [gsRunning,gsEditor]) then
  if fViewport<>nil then
  fViewport.SetZoom(fViewport.Zoom+WheelDelta/2000);
end;


procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  if not (Form1.Active or FormLoading.Active) then exit;
  if FormLoading.Visible then begin
    FormLoading.Hide;
    Form1.SetFocus;
  end;
  fGame.UpdateState;
  Form1.Caption := inttostr(fGame.ScreenY);
end;


//Open
procedure TForm1.Open_MissionMenuClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then exit;
  fGame.StopGame(gr_Silent);
  fGame.StartGame(OpenDialog1.FileName, 'OpenDialog1 game');
end;


procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'',ExeDir,'Knights & Merchants Mission (*.dat)|*.dat') then exit;
  fGame.StopGame(gr_Silent);
  OpenDialog1.FileName := ExtractFileName(OpenDialog1.FileName);
  fGame.StartMapEditor(Copy(OpenDialog1.FileName,1,length(OpenDialog1.FileName)-4));
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
  ShowTerrainWires := Debug_ShowWires.Checked;
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
var s:string;
begin
  DateTimeToString(s,'yyyy-mm-dd hh-nn-ss',Now); //2007-12-23 15-24-33
  if fRender<>nil then fRender.DoPrintScreen(ExeDir+'KaM '+s+'.jpg');
end;

procedure TForm1.Debug_ShowPanel1Click(Sender: TObject);
begin GroupBox1.Visible := not GroupBox1.Visible; end;

procedure TForm1.Debug_PassabilityTrackChange(Sender: TObject);
begin
  ShowTerrainWires:=Debug_PassabilityTrack.Position<>0;
  Debug_PassabilityTrack.Max:=length(PassabilityStr)-1; //Reserve 0 as Off
  Label2.Caption:= PassabilityStr[Debug_PassabilityTrack.Position];
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
  fResource.LoadFonts(true, fGame.fGameSettings.GetLocale);
end;


procedure TForm1.Export_DeliverLists1Click(Sender: TObject);
var f:textfile; i:integer;
begin
  if fPlayers=nil then exit;

  assignfile(f,ExeDir+'DeliverLists.txt'); Rewrite(f);
  for i:=1 to fPlayers.PlayerCount do
    writeln(f,'Player_'+inttostr(i)+eol+fPlayers.Player[i].DeliverList.WriteToText+eol+eol);
  closefile(f);
end;


procedure TForm1.TeamColorPickerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DefineInputColor(TeamColors[1] and $FFFFFF,Sender);
end;


procedure TForm1.TeamColorPickerDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TeamColors[1]:=cardinal(TeamColorPicker.Brush.Color) or $FF000000;
  fRender.Render;
end;


procedure TForm1.RGPlayerClick(Sender: TObject);
begin
  if fPlayers=nil then exit;
  if fPlayers.Player[RGPlayer.ItemIndex+1] <> nil then
    MyPlayer:=fPlayers.Player[RGPlayer.ItemIndex+1];
end;


procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then fGame.GameSpeed:=50 else fGame.GameSpeed:=1;
end;      


{Walk tests}
procedure TForm1.Button_WClick(Sender: TObject);
var U:TKMUnit;
begin
  fGame.StopGame(gr_Silent);
  fGame.StartGame('', 'W');
  DO_SERFS_WALK_ROADS := false;
  MyPlayer:=fPlayers.Player[1];

  //Diagonal exchange
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,5));
  U.SetActionWalk(U,KMPoint(9,9));
  //U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,5));
  //U.SetActionWalk(U,KMPoint(9,9));
  //U:=MyPlayer.AddUnit(ut_Miner, KMPoint(9,9));
  //U.SetActionWalk(U,KMPoint(5,5));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(9,9));
  U.SetActionWalk(U,KMPoint(5,5)); //}

  //Walk in row
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,8));
  U.SetActionWalk(U,KMPoint(5,14));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,8));
  U.SetActionWalk(U,KMPoint(5,14));//}

  //Work around busy unit
  {MyPlayer.AutoRoadConnect(KMPoint(5,10),KMPoint(15,10));
  U:=MyPlayer.AddUnit(ut_Serf, KMPoint(5,10));
  U.SetActionWalk(U,KMPoint(15,10));
  U:=MyPlayer.AddUnit(ut_Worker, KMPoint(10,10));
  U.SetActionStay(1000,ua_Work2);}

  //Solve big diamond
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(4,10));
  U.SetActionWalk(U,KMPoint(6,8));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,9));
  U.SetActionWalk(U,KMPoint(7,11));
  U:=MyPlayer.AddUnit(ut_Baker, KMPoint(6,10));
  U.SetActionWalk(U,KMPoint(4,12));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,11));
  U.SetActionWalk(U,KMPoint(3,9));//}

  //Unsolved situations:
  //--------------------

  //Solve diamond with destination being blocked
  //Idea: If unit can't move then it should be no problem to GetOutOfTheWay and recompute WalkRoute from new spot
  //but how to maintain TTask integrity?
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(4,10));
  U.SetActionWalk(U,KMPoint(5,9));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,9));
  U.SetActionWalk(U,KMPoint(6,10));
  U:=MyPlayer.AddUnit(ut_Baker, KMPoint(6,10));
  U.SetActionWalk(U,KMPoint(5,11));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,11));
  U.SetActionWalk(U,KMPoint(4,10));//}
                                                      

  //Walk through 7x50 group
  MyPlayer.AddGroup(ut_Baker, KMPoint(8,8), dir_W, 7, 350);
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(7,8));
  U.SetActionWalk(U,KMPoint(58,8));//}
  

  MyPlayer.AddUnit(ut_HorseScout, KMPoint(12,12)); //don't let mission to be immediately lost
  fTerrain.RevealWholeMap(play_1);
  fViewPort.SetCenter(10,10);
end;


{Village tests}
procedure TForm1.Button_VClick(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
  fGame.StopGame(gr_Silent);
  fGame.StartGame('', 'V');

fViewPort.SetCenter(11,9);

for k:=-6 to 5 do for i:=-5 to 6 do
fTerrain.SetResourceDeposit(KMPoint(8+i,14+k),rt_Coal);

for k:=-4 to 0 do for i:=-3 to 3 do
fTerrain.SetResourceDeposit(KMPoint(21+i,6+k),rt_IronOre);

for k:=0 to 1 do for i:=0 to 1 do
fTerrain.SetResourceDeposit(KMPoint(15+i,8+k),rt_Stone);

MyPlayer.AddRoadPlan(KMPoint(2,6),mu_RoadPlan,true);

MyPlayer.AddRoadPlan(KMPoint(2,7),mu_FieldPlan,true);
MyPlayer.AddRoadPlan(KMPoint(3,7),mu_FieldPlan,true);
MyPlayer.AddRoadPlan(KMPoint(4,7),mu_FieldPlan,true);
MyPlayer.AddRoadPlan(KMPoint(5,7),mu_FieldPlan,true);

MyPlayer.AddHouse(ht_Farm, KMPoint(3,5));
MyPlayer.AddHouse(ht_Mill, KMPoint(8,5));
MyPlayer.AddHouse(ht_Bakery, KMPoint(13,5));
MyPlayer.AddUnit(ut_Farmer, KMPoint(3,7));
MyPlayer.AddUnit(ut_Baker, KMPoint(4,7));
MyPlayer.AddUnit(ut_Baker, KMPoint(5,7));

MyPlayer.AddHouse(ht_Store, KMPoint(17,5));

MyPlayer.AddHouse(ht_WoodCutters, KMPoint(24,9));
MyPlayer.AddHouse(ht_SawMill, KMPoint(7,9));
MyPlayer.AddHouse(ht_Quary, KMPoint(12,9));
MyPlayer.AddUnit(ut_WoodCutter, KMPoint(7,11));                                                 
MyPlayer.AddUnit(ut_StoneCutter, KMPoint(6,9));

MyPlayer.AddRoadPlan(KMPoint(2,14),mu_WinePlan,true);
MyPlayer.AddRoadPlan(KMPoint(3,14),mu_WinePlan,true);
MyPlayer.AddRoadPlan(KMPoint(4,14),mu_WinePlan,true);
MyPlayer.AddRoadPlan(KMPoint(5,14),mu_WinePlan,true);
MyPlayer.AddHouse(ht_WineYard, KMPoint(4,13));
MyPlayer.AddUnit(ut_Farmer, KMPoint(15,9));
MyPlayer.AddHouse(ht_CoalMine, KMPoint(8,13));
MyPlayer.AddUnit(ut_Miner, KMPoint(10,9));
MyPlayer.AddHouse(ht_FisherHut, KMPoint(12,13)); //Added to demonstrate a house without an occupant in the building page

MyPlayer.AddHouse(ht_WeaponSmithy, KMPoint(16,13));
MyPlayer.AddHouse(ht_WeaponWorkshop, KMPoint(16,16));

MyPlayer.AddHouse(ht_ArmorSmithy, KMPoint(20,13));
MyPlayer.AddHouse(ht_ArmorWorkshop, KMPoint(20,17));

MyPlayer.AddHouse(ht_IronMine, KMPoint(21,6));
MyPlayer.AddHouse(ht_IronSmithy, KMPoint(21,9));

MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));

for i:=1 to 16 do
MyPlayer.AddUnit(ut_Serf, KMPoint(2,11));

for i:=1 to 3 do
MyPlayer.AddUnit(ut_Worker, KMPoint(3,11));

MyPlayer.AddUnit(ut_Recruit, KMPoint(12,11));
MyPlayer.AddUnit(ut_Metallurgist, KMPoint(13,11));
MyPlayer.AddUnit(ut_Miner, KMPoint(13,11));
MyPlayer.AddUnit(ut_Smith, KMPoint(13,11));
MyPlayer.AddUnit(ut_Smith, KMPoint(13,11));

H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store));
if H<>nil then H.AddMultiResource(rt_All,250); //It had had lack of stones
if H<>nil then H.AddMultiResource(rt_Sausages,500);

MyPlayer.AddRoadPlan(KMPoint(3,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(4,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(5,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(6,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(7,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(8,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(9,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(10,6),mu_RoadPlan,true);

MyPlayer.AddHouse(ht_School, KMPoint(4,17));
MyPlayer.AddHouse(ht_Inn, KMPoint(9,18));
end;


{Maxplayer tests}
procedure TForm1.Button_6Click(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
  fGame.StopGame(gr_Silent);
  fGame.StartGame('', '6');

  for k:=1 to 6 do begin
    MyPlayer:=fPlayers.Player[k];

    MyPlayer.AddHouse(ht_Store, KMPoint(10+k*4,25));
    H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store));
    if H<>nil then H.AddMultiResource(rt_All,30);

    for i:=1 to 5 do MyPlayer.AddUnit(ut_Serf, KMPoint(10+k*4,28));
    for i:=1 to 3 do MyPlayer.AddUnit(ut_Worker, KMPoint(10+k*4+1,28));

    MyPlayer.AddGroup(ut_HorseScout, KMPoint(10+k*4+1,32),dir_N,3,6);
    MyPlayer.AddGroup(ut_Arbaletman, KMPoint(10+k*4+1,36),dir_N,3,6);

  end;

  RGPlayer.ItemIndex:=2;
  RGPlayerClick(nil); //Update

  fViewPort.SetCenter(22,30);
end;


{Single house tests}
procedure TForm1.Button_1Click(Sender: TObject);
var H:TKMHouse; i:integer;
begin
  fGame.StopGame(gr_Silent);
  fGame.StartGame('', '1',1);
  MyPlayer := fPlayers.Player[1];

  MyPlayer.AddHouse(ht_Store, KMPoint(4,5));
  H := TKMHouseStore(MyPlayer.FindHouse(ht_Store));
  if H<>nil then TKMHouseStore(H).AddMultiResource(rt_All, 1300);

  MyPlayer.AddHouse(ht_Sawmill, KMPoint(9,8));
  MyPlayer.AddUnit(ut_Lamberjack, KMPoint(9,13));
  MyPlayer.AutoRoadConnect(KMPointY1(KMPoint(4,5)), KMPointY1(KMPoint(9,8)));

  for i:=1 to 5 do MyPlayer.AddUnit(ut_Serf, KMPoint(6,6));

//  MyPlayer.AddGroup(ut_Pikeman, KMPoint(6,15), dir_N, 4, 12);
//  for i:=1 to 5 do
//    MyPlayer.AddUnit(ut_Serf, KMPoint(4,8));
//  MyPlayer.AddUnit(ut_Worker, KMPoint(5,8));

  //MyPlayer.AddHouse(ht_Inn,KMPoint(18,8));
//  MyPlayer.AutoRoadConnect(KMPointY1(KMPoint(4,5)),KMPointY1(KMPoint(18,8)));
  //MyPlayer.AddHousePlan(ht_Mill,KMPoint(6,12),true);
  //MyPlayer.AddHouse(ht_Stables,KMPoint(9,8));
  //MyPlayer.AddHouse(ht_Swine,KMPoint(15,8));
  //MyPlayer.AddUnit(ut_AnimalBreeder, KMPoint(9,12));
  //MyPlayer.AddUnit(ut_AnimalBreeder, KMPoint(10,12));

  {MyPlayer.AddGroup(ut_Militia,KMPoint(5,14),dir_N,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(10,14),dir_NE,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(15,14),dir_E,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(20,14),dir_SE,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(25,14),dir_S,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(30,14),dir_SW,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(35,14),dir_W,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(40,14),dir_NW,3,6);}

  //H.AddDamage(255);

  //for i:=1 to 25 do MyPlayer.AddUnit(ut_Serf, KMPoint(20,20));

  fViewPort.SetCenter(10,9);

//  MyPlayer.AddUnit(ut_Wolf,KMPoint(5,12));
{  MyPlayer.AddUnit(ut_Fish,KMPoint(6,12));
  MyPlayer.AddUnit(ut_Watersnake,KMPoint(7,12));
  MyPlayer.AddUnit(ut_Seastar,KMPoint(8,12));
  MyPlayer.AddUnit(ut_Crab,KMPoint(9,12));
  MyPlayer.AddUnit(ut_Waterflower,KMPoint(10,12));
  MyPlayer.AddUnit(ut_Waterleaf,KMPoint(11,12));
  MyPlayer.AddUnit(ut_Duck,KMPoint(12,12)); }
end;


procedure TForm1.Button_StopClick(Sender: TObject);
begin
  fGame.StopGame(gr_Cancel);
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

  {$IFDEF VER140} //Lazarus can't operate with ClientSize for it's not multi-platform property
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


procedure TForm1.ToggleFullScreen(Toggle:boolean; ResolutionID:word; ReturnToOptions:boolean);
begin
  if Toggle then begin
    SetScreenResolution(SupportedResolutions[ResolutionID,1],SupportedResolutions[ResolutionID,2],SupportedRefreshRates[ResolutionID]);
    Form1.Refresh;
    Form1.BorderStyle  := bsSizeable; //if we don't set Form1 sizeable it won't expand to fullscreen
    Form1.WindowState  := wsMaximized;
    Form1.BorderStyle  := bsNone;     //and now we can make it borderless again
    Form1.Refresh;
  end else begin
    ResetResolution;
    Form1.Refresh;
    Form1.WindowState  := wsNormal;
    Form1.BorderStyle  := bsSizeable;
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
  FreeAndNil(fGame); //Saves all settings into ini file in midst
  //Now re-init fGame
  fGame := TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height);
  fGame.ResizeGameArea(Panel5.Width,Panel5.Height);
  fLog.AppendLog('ToggleFullscreen - '+inttostr(Panel5.Top)+':'+inttostr(Panel5.Height));

  if ReturnToOptions then fGame.fMainMenuInterface.ShowScreen_Options; //Return to the options screen
  ApplyCursorRestriction
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
    inherited;
end;


procedure TForm1.ReadAvailableResolutions;
var
  i,k : integer;
  DevMode : TDevMode;
begin
  i:=0;
  FillChar(SupportedRefreshRates, SizeOf(SupportedRefreshRates), 0); //Thats a nice trick to fill it with zeroes ;)
  while EnumDisplaySettings(nil, i, DevMode) do
  with DevMode do
  begin
    inc(i);
    if dmBitsPerPel=32 then
    for k:=1 to RESOLUTION_COUNT do
    if (SupportedResolutions[k,1] = dmPelsWidth) and (SupportedResolutions[k,2] = dmPelsHeight)then
      SupportedRefreshRates[k] := Math.max(SupportedRefreshRates[k], dmDisplayFrequency);
  end;
end;


procedure TForm1.ApplyCursorRestriction;
var
  Rect: TRect;
begin
  if (fGame <> nil) and (fGame.fGameSettings <> nil) and fGame.fGameSettings.IsFullScreen then
  begin
    Rect := BoundsRect;
    ClipCursor(@Rect); //Restrict the cursor movement to inside our form
  end
  else ClipCursor(nil); //Otherwise have no restriction
end;


{$IFDEF FPC}
initialization
{$I KM_Unit1.lrs}
{$ENDIF}

end.
