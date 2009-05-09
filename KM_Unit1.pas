unit KM_Unit1;
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, FileCtrl, ExtCtrls, ComCtrls,
  Menus, Buttons, Math, SysUtils, KromUtils, OpenGL, dglOpenGL, MMSystem,
  KM_Render, KM_RenderUI, KM_ReadGFX1, KM_Defaults,
  KM_Form_Loading, KM_Terrain, KM_Game,
  KM_Units, KM_Houses, KM_Viewport, KM_Users, ColorPicker, KM_LoadLib, KM_LoadSFX, KM_LoadDAT,
  MPlayer;

type                           
  TForm1 = class(TForm)
    N2: TMenuItem;
    Debug_ShowUnits: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenMapMenu: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Debug1: TMenuItem;
    Debug_ShowWires: TMenuItem;
    Debug_ShowObjects: TMenuItem;
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
    Image4: TImage;
    TBZoomControl: TTrackBar;
    Image3: TImage;
    Label1: TLabel;
    TeamColorPicker: TShape;
    CheckBox2: TCheckBox;
    Debug_Pause: TCheckBox;
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
    Step1Frame: TButton;
    Button_1: TButton;
    Debug_ShowOverlay: TMenuItem;
    AnimData1: TMenuItem;
    Other1: TMenuItem;
    Debug_ShowPanel1: TMenuItem;
    Button_W: TButton;
    Export_TreeAnim1: TMenuItem;
    Export_GUIMainHRX: TMenuItem;
    MediaPlayer1: TMediaPlayer;
    TB_Angle: TTrackBar;
    Label3: TLabel;
    procedure Export_TreeAnim1Click(Sender: TObject);
    procedure TB_Angle_Change(Sender: TObject);
  published
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure Open_MapClick(Sender: TObject);
    procedure Debug_ZoomChange(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Debug_ResetZoomClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure Debug_ShowWiresClick(Sender: TObject);
    procedure Debug_ShowObjectsClick(Sender: TObject);
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
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;
  FormLoading:TFormLoading;

implementation  {$R *.DFM}
uses KM_Settings;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var FrameTime:cardinal;
begin //Counting FPS
  if not Form1.Active then exit;

  FrameTime:=TimeGetTime-OldTimeFPS;
  OldTimeFPS:=TimeGetTime;

  if (FPSLag<>1)and(FrameTime<FPSLag) then begin
    sleep(FPSLag-FrameTime);
    FrameTime:=FPSLag;
  end;

  inc(OldFrameTimes,FrameTime);
  inc(FrameCount);
  if OldFrameTimes>=FPS_INTERVAL then begin
    StatusBar1.Panels[2].Text:=floattostr(round((1000/(OldFrameTimes/FrameCount))*10)/10)+' fps ('+inttostr(1000 div FPSLag)+')';
    OldFrameTimes:=0;
    FrameCount:=0;
    fLog.AppendLog('First sec frame done');
  end; //FPS calculation complete

fRender.Render;
done:=false; //repeats OnIdle event
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if Sender<>nil then exit;

  FormLoading.Show; //This is our splash screen
  FormLoading.Refresh;

  //Randomize; //Randomize the random seed to ensure that we don't get repeditive patterns, but we need this to be Off to reproduce bugs
  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log'); //First thing - create a log

  //Form1.BorderStyle:=bsSizeable;

  Form1.WindowState:=wsMaximized;
  //Form1.ClientWidth:=1024;
  //Form1.ClientHeight:=768;
  //To get fullscreen - change this in ObjectInspector, otherwise it doesn't work right
  //!Form1.BorderStyle:=bsNone;

  Form1.Refresh;
  fGame:=TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height, MediaPlayer1);

  TimeBeginPeriod(1);
  Application.OnIdle:=Form1.OnIdle;

  fLog.AppendLog('Form1 create is done');

  FormLoading.Hide;

  Timer100ms.Interval:=GAME_LOGIC_PACE; //100ms
  Form1.Caption:='KaM Remake - '+'New.map';
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fGame);
  TimeEndPeriod(1);
end;


procedure TForm1.FormResize(Sender:TObject);
begin
  if fGame<>nil then //Occurs on exit
    fGame.ResizeGameArea(Panel5.Width,Panel5.Height);
end;


procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseDown(Button, Shift, X, Y); end;


procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin fGame.MouseMove(Shift, X, Y); end;


procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseUp(Button, Shift, X, Y); end;


procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if MOUSEWHEEL_ZOOM_ENABLE then
  if fViewport<>nil then
  fViewport.SetZoom(fViewport.Zoom+WheelDelta/2000); //4Debug only
end;


procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  if not Form1.Active then exit;

  if (Debug_Pause.Checked)and(Sender<>Step1Frame) then exit; //Pause

  fGame.UpdateState;
end;


//Open
procedure TForm1.Open_MapClick(Sender: TObject);
begin
  //Assert(false,'Should be re-rigged');
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;
  fTerrain.OpenMapFromFile(OpenDialog1.FileName);
  fTerrain.RevealWholeMap(play_1);
  Form1.Caption:='KaM Remake - '+OpenDialog1.FileName;
end;


procedure TForm1.Open_MissionMenuClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants Mission (*.dat)|*.dat') then exit;
  fGame.StopGame;
  fGame.StartGame(OpenDialog1.FileName);
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
  Debug_ShowWires.Checked:=not Debug_ShowWires.Checked;
  ShowTerrainWires:=Debug_ShowWires.Checked;
end;

procedure TForm1.Debug_ShowObjectsClick(Sender: TObject);
begin Debug_ShowObjects.Checked:=not Debug_ShowObjects.Checked; end;

procedure TForm1.Debug_ShowOverlayClick(Sender: TObject);
begin
  Debug_ShowOverlay.Checked:= not Debug_ShowOverlay.Checked;
  MakeDrawPagesOverlay:=Debug_ShowOverlay.Checked;
end;

procedure TForm1.Debug_ShowUnitClick(Sender: TObject);
begin
  Debug_ShowUnits.Checked:= not Debug_ShowUnits.Checked;
  MakeShowUnitMove:=Debug_ShowUnits.Checked;
  MakeShowUnitRoutes:=Debug_ShowUnits.Checked;
end;

procedure TForm1.Debug_PrintScreenClick(Sender: TObject);
var s:string;
begin
  DateTimeToString(s,'yyyy-mm-dd hh-nn-ss',Now); //2007-12-23 15-24-33
  if fRender<>nil then fRender.DoPrintScreen(ExeDir+'KaM '+s+'.jpg');
end;

procedure TForm1.Debug_ShowPanel1Click(Sender: TObject);
begin GroupBox1.Visible:=not GroupBox1.Visible; end;

procedure TForm1.Debug_PassabilityTrackChange(Sender: TObject);
begin
  ShowTerrainWires:=true;
  Debug_PassabilityTrack.Max:=length(PassabilityStr)-1;
  Label2.Caption:= PassabilityStr[Debug_PassabilityTrack.Position+1];
end;

procedure TForm1.Debug_ZoomChange(Sender: TObject);
begin fGame.ZoomInGameArea(ZoomLevels[TBZoomControl.Position]); end;

procedure TForm1.Debug_ResetZoomClick(Sender: TObject);
begin TBZoomControl.Position:=4; end;



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
var i:integer;
begin
  for i:=1 to length(FontFiles) do
    ReadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.fnt',TKMFont(i),true);
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
  DefineInputColor((Sender as TShape).Brush.Color,Sender);
end;


procedure TForm1.TeamColorPickerDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TeamColors[1]:=TeamColorPicker.Brush.Color;
  fRender.Render;
end;


procedure TForm1.RGPlayerClick(Sender: TObject);
begin
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
  fGame.StopGame;
  fGame.StartGame('');
  MyPlayer:=fPlayers.Player[1];

  //Diagonal exchange
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,5));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(9,9)));
  U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,5));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(9,9)));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(9,9));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,5)));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(9,9));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,5))); //}

  //Walk in row
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(5,8));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,14)));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,8));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,14)));//}

  //Solve diamond
  //Idea: If unit can't move then it should be no problem to GetOutOfTheWay and recompute WalkRoute from new spot
  {U:=MyPlayer.AddUnit(ut_Baker, KMPoint(4,10));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,9)));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,9));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(6,10)));
  U:=MyPlayer.AddUnit(ut_Baker, KMPoint(6,10));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(5,11)));
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,11));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(4,10)));//}

  //Walk through group
  MyPlayer.AddGroup(ut_Baker, KMPoint(8,8),dir_W,7,49);
  U:=MyPlayer.AddUnit(ut_Miner, KMPoint(5,8));
  U.SetAction(TUnitActionWalkTo.Create(U.GetPosition,KMPoint(16,8)));//}

  fTerrain.RevealWholeMap(play_1);
  fViewPort.SetCenter(10,10);
end;


{Village tests}
procedure TForm1.Button_VClick(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
  fGame.StopGame();
  fGame.StartGame('');

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
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
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

for i:=1 to 16 do
MyPlayer.AddUnit(ut_Serf, KMPoint(2,11));

for i:=1 to 3 do
MyPlayer.AddUnit(ut_Worker, KMPoint(3,11));

MyPlayer.AddUnit(ut_Recruit, KMPoint(12,11));
MyPlayer.AddUnit(ut_Metallurgist, KMPoint(13,11));
MyPlayer.AddUnit(ut_Miner, KMPoint(13,11));
MyPlayer.AddUnit(ut_Smith, KMPoint(13,11));
MyPlayer.AddUnit(ut_Smith, KMPoint(13,11));

H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store,0,0));
if H<>nil then H.AddMultiResource(rt_All,25);
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
  fGame.StopGame;
  fGame.StartGame('');

  for k:=1 to 6 do begin
    MyPlayer:=fPlayers.Player[k];

    MyPlayer.AddHouse(ht_Store, KMPoint(k*4,5));
    H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store,0,0));
    if H<>nil then H.AddMultiResource(rt_All,30);

    for i:=1 to 5 do MyPlayer.AddUnit(ut_Serf, KMPoint(k*4,8));
    for i:=1 to 3 do MyPlayer.AddUnit(ut_Worker, KMPoint(k*4+1,8));

  end;

  RGPlayer.ItemIndex:=2;
  RGPlayerClick(nil); //Update

  fViewPort.SetCenter(13,11);
end;


{Single house tests}
procedure TForm1.Button_1Click(Sender: TObject);
var H:TKMHouse; i:integer; U:TKMUnit;
begin
  fGame.StopGame;
  fGame.StartGame('');
  MyPlayer:=fPlayers.Player[1];

  MyPlayer.AddHouse(ht_Store, KMPoint(4,5));
  H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store,0,0));
  if H<>nil then TKMHouseStore(H).AddMultiResource(rt_All,1300);

  for i:=1 to 5 do MyPlayer.AddUnit(ut_Serf, KMPoint(4,8));
  MyPlayer.AddUnit(ut_Worker, KMPoint(5,8));

  H:=MyPlayer.AddHouse(ht_Inn,KMPoint(18,8));
  MyPlayer.AddHouse(ht_Stables,KMPoint(9,8));
  MyPlayer.AddHouse(ht_Swine,KMPoint(15,8));
  MyPlayer.AddUnit(ut_AnimalBreeder, KMPoint(9,12));
  MyPlayer.AddUnit(ut_AnimalBreeder, KMPoint(9,12));

  {MyPlayer.AddGroup(ut_Militia,KMPoint(5,14),dir_N,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(10,14),dir_NE,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(15,14),dir_E,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(20,14),dir_SE,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(25,14),dir_S,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(30,14),dir_SW,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(35,14),dir_W,3,6);
  MyPlayer.AddGroup(ut_Militia,KMPoint(40,14),dir_NW,3,6);}

  //H.AddDamage(255);
  //MyPlayer.AddHouse(ht_Inn,KMPoint(9,8));
  //MyPlayer.AddUnit(ut_Baker, KMPoint(9,9));

  //for i:=1 to 25 do MyPlayer.AddUnit(ut_Serf, KMPoint(20,20));

  fViewPort.SetCenter(10,9);

  MyPlayer.AddUnit(ut_Wolf,KMPoint(5,12));
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
  fGame.StopGame;
end;

procedure TForm1.TB_Angle_Change(Sender: TObject);
begin
RENDER_3D:=TB_Angle.Position<>0;
Label3.Caption:=inttostr(TB_Angle.Position);
fRender.SetRotation(-TB_Angle.Position,0,0);
fRender.Render;
end;

{procedure TForm1.Button1Click(Sender: TObject);
var
  Count,h:word;
  A:array[1..TEST_MAX_WALK_PATH]of TKMPoint;
  T:cardinal;
begin
  T:=TimeGetTime;
  for h:=1 to 1 do
  fTerrain.Route_Make(KMPoint(1,14),KMPoint(71,2),canWalk,Count,A);
  Button1.Caption:=inttostr(TimeGetTime-T)+'ms';
end;}

end.
