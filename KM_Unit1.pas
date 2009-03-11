unit KM_Unit1;
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, FileCtrl, ExtCtrls, ComCtrls,
  Menus, Buttons, Math, SysUtils, KromUtils, OpenGL, dglOpenGL,
  KM_Render, KM_RenderUI, KM_ReadGFX1, KM_Defaults,
  KM_Form_Loading, KM_Terrain, KM_Game,
  KM_Units, KM_Houses, KM_Viewport, KM_Users, KM_Controls, ColorPicker, KM_LoadLib, KM_LoadSFX, KM_LoadDAT;

type                           
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenMapMenu: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Advanced1: TMenuItem;
    ShowWires: TMenuItem;
    ShowObjects: TMenuItem;
    ShowFlatTerrain: TMenuItem;
    Panel5: TPanel;
    Timer100ms: TTimer;
    PrintScreen1: TMenuItem;
    Export1: TMenuItem;
    ExportGUIRX: TMenuItem;
    ExportTreesRX: TMenuItem;
    ExportHousesRX: TMenuItem;
    ExportUnitsRX: TMenuItem;
    ExportGUIMainRX: TMenuItem;
    Exportfonts1: TMenuItem;
    GroupBox1: TGroupBox;
    Image4: TImage;
    TBZoomControl: TTrackBar;
    Image3: TImage;
    Label1: TLabel;
    TeamColorPicker: TShape;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    ExportText: TMenuItem;
    ExportStatus1: TMenuItem;
    ExportDeliverlists1: TMenuItem;
    ExportSounds1: TMenuItem;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    CheckBox4: TCheckBox;
    HouseAnim1: TMenuItem;
    UnitAnim1: TMenuItem;
    CheckBox5: TCheckBox;
    RGPlayer: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenMissionMenu: TMenuItem;
    Step1Frame: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure OpenMapClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetZoomClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ShowWiresClick(Sender: TObject);
    procedure ShowObjectsClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PrintScreen1Click(Sender: TObject);
    procedure ExportGUIRXClick(Sender: TObject);
    procedure ExportTreesRXClick(Sender: TObject);
    procedure ExportHousesRXClick(Sender: TObject);
    procedure ExportUnitsRXClick(Sender: TObject);
    procedure ExportGUIMainRXClick(Sender: TObject);
    procedure TeamColorPickerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TeamColorPickerDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Exportfonts1Click(Sender: TObject);
    procedure DoScrolling;
    procedure ExportTextClick(Sender: TObject);
    procedure ExportDeliverlists1Click(Sender: TObject);
    procedure ExportSounds1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure HouseAnim1Click(Sender: TObject);
    procedure UnitAnim1Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RGPlayerClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure OpenMissionMenuClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
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

  FrameTime:=GetTickCount-OldTimeFPS;
  OldTimeFPS:=GetTickCount;

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

  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log'); //First thing - create a log

  Form1.WindowState:=wsMaximized;
  //Form1.BorderStyle:=bsSizeable;
//  Form1.FormResize(nil);
  Form1.Refresh;
  fGame:=TKMGame.Create(ExeDir,Panel5.Handle,Panel5.Width,Panel5.Height);

  Application.OnIdle:=Form1.OnIdle;

  fLog.AppendLog('Form1 create is done');

  FormLoading.Hide;

  Timer100ms.Interval:=GAME_LOGIC_PACE; //100ms
  Form1.Caption:='KaM Remake - '+'New.map';
end;

procedure TForm1.OpenMapClick(Sender: TObject);
begin
  //Assert(false,'Should be re-rigged');
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;
  fTerrain.OpenMapFromFile(OpenDialog1.FileName);
  Form1.Caption:='KaM Remake - '+OpenDialog1.FileName;
end;

procedure TForm1.FormResize(Sender:TObject);
begin
  if fGame<>nil then //Occurs on exit
    fGame.ResizeGameArea(Panel5.Width,Panel5.Height);
end;

procedure TForm1.ZoomChange(Sender: TObject);
begin
  fGame.ZoomInGameArea(ZoomLevels[TBZoomControl.Position]);
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseDown(Button, Shift, X, Y); end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin fGame.MouseMove(Shift, X, Y); end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin fGame.MouseUp(Button, Shift, X, Y); end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Label1.Caption:='';
  FormLoading.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fGame);
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  if not Form1.Active then exit;

  if (CheckBox1.Checked)and(Sender<>Step1Frame) then exit;

  if CheckBox4.Checked then
  if GlobalTickCount mod 2 <> 0 then exit;

  fGame.UpdateState;

  if fGame.GameIsRunning then //Need to re-rig it to fViewport later
    DoScrolling; //Now check to see if we need to scroll
end;

procedure TForm1.ResetZoomClick(Sender: TObject);
begin
  TBZoomControl.Position:=4;
end;

procedure TForm1.ExitClick(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.ShowWiresClick(Sender: TObject);
begin
  ShowWires.Checked:=not ShowWires.Checked;
end;

procedure TForm1.ShowObjectsClick(Sender: TObject);
begin
  ShowObjects.Checked:=not ShowObjects.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
  fGame.StopGame;
  fGame.StartGame('');
TKMControl(Sender).Enabled:=false;
fViewPort.SetCenter(11,9);

for k:=-5 to 5 do for i:=-4 to 6 do
fTerrain.SetCoalReserve(KMPoint(8+i,14+k));

for k:=-5 to 5 do for i:=-4 to 6 do
fTerrain.SetOreReserve(KMPoint(21+i,6+k),rt_IronOre);

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
if H<>nil then H.AddMultiResource(rt_All,15);
if H<>nil then H.AddMultiResource(rt_Sousages,500);

MyPlayer.AddRoadPlan(KMPoint(3,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(4,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(5,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(6,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(7,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(8,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(9,6),mu_RoadPlan,true);
MyPlayer.AddRoadPlan(KMPoint(10,6),mu_RoadPlan,true);

MyPlayer.AddHousePlan(ht_School, KMPoint(4,17),true);
MyPlayer.AddHouse(ht_Inn, KMPoint(9,18));
end;

procedure TForm1.Button2Click(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
  fGame.StopGame;
  fGame.StartGame('');
TKMControl(Sender).Enabled:=false;

for k:=1 to 4 do begin
  MyPlayer:=fPlayers.Player[k];

  MyPlayer.AddHouse(ht_Store, KMPoint(k*4,5));
  H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store,0,0));
  if H<>nil then H.AddMultiResource(rt_All,30);

  for i:=1 to 5 do MyPlayer.AddUnit(ut_Serf, KMPoint(k*4,8));
  for i:=1 to 3 do MyPlayer.AddUnit(ut_Worker, KMPoint(k*4+1,8));

end;

fViewPort.SetCenter(10,9);

end;

procedure TForm1.PrintScreen1Click(Sender: TObject);
var s:string;
begin
  DateTimeToString(s,'yyyy-mm-dd hh-nn-ss',Now); //2007-12-23 15-24-33
  if fRender<>nil then fRender.DoPrintScreen(ExeDir+'KaM '+s+'.jpg');
end;

procedure TForm1.ExportTreesRXClick(Sender: TObject);  begin ExportRX2BMP(1); end;
procedure TForm1.ExportHousesRXClick(Sender: TObject); begin ExportRX2BMP(2); end;
procedure TForm1.ExportUnitsRXClick(Sender: TObject);  begin ExportRX2BMP(3); end;
procedure TForm1.ExportGUIRXClick(Sender: TObject);    begin ExportRX2BMP(4); end;
procedure TForm1.ExportGUIMainRXClick(Sender: TObject);begin ExportRX2BMP(5); end;
procedure TForm1.ExportSounds1Click(Sender: TObject);  begin fSoundLib.ExportSounds; end;
procedure TForm1.HouseAnim1Click(Sender: TObject);     begin ExportHouseAnim2BMP(); end;
procedure TForm1.UnitAnim1Click(Sender: TObject);      begin ExportUnitAnim2BMP();  end;
procedure TForm1.ExportTextClick(Sender: TObject);     begin fTextLibrary.ExportTextLibraries; end;

procedure TForm1.ExportFonts1Click(Sender: TObject);
var i:integer;
begin
  for i:=1 to length(FontFiles) do
    ReadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.fnt',TKMFont(i),true);
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


//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Global_Data
procedure TForm1.DoScrolling;
const DirectionsBitfield:array[0..12]of byte = (0,c_Scroll6,c_Scroll0,c_Scroll7,c_Scroll2,0,c_Scroll1,0,c_Scroll4,c_Scroll5,0,0,c_Scroll3);
var XCoord, YCoord, ScrollAdv: integer; Temp:byte;
begin
  XCoord := fViewport.GetCenter.X; //First set X and Y to be the current values
  YCoord := fViewport.GetCenter.Y;
  Temp:=0; //That is our bitfield variable for directions, 0..12 range
  //    3 2 6  These are directions
  //    1 * 4  They are converted from bitfield to actual cursor constants, see Arr array
  //    9 8 12

  ScrollAdv := SCROLLSPEED + byte(fGameSettings.IsFastScroll)*3; //4 times faster

  //Left, Top, Right, Bottom
  if Mouse.CursorPos.X < SCROLLFLEX then begin inc(Temp,1); dec(XCoord,ScrollAdv); end;
  if Mouse.CursorPos.Y < SCROLLFLEX then begin inc(Temp,2); dec(YCoord,ScrollAdv); end;
  if Mouse.CursorPos.X > Screen.Width -1-SCROLLFLEX then begin inc(Temp,4); inc(XCoord,ScrollAdv); end;
  if Mouse.CursorPos.Y > Screen.Height-1-SCROLLFLEX then begin inc(Temp,8); inc(YCoord,ScrollAdv); end;
  if Temp<>0 then Screen.Cursor :=DirectionsBitfield[Temp]; //Sample cursor type from bitfield value

  //Now do actual the scrolling, if needed
  if (XCoord<>fViewport.GetCenter.X)or(YCoord<>fViewport.GetCenter.Y) then
  begin
    fViewport.SetCenter(XCoord,YCoord);
    Scrolling := true; //Stop OnMouseOver from overriding my cursor changes
  end else begin
    Scrolling := false; //Allow cursor changes to be overriden and reset if still on a scrolling cursor
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;
  end;
end;



procedure TForm1.ExportDeliverlists1Click(Sender: TObject);
var f:textfile; i:integer;
begin
  if fPlayers=nil then exit;

  assignfile(f,ExeDir+'DeliverLists.txt'); Rewrite(f);
  for i:=1 to fPlayers.PlayerCount do
    writeln(f,'Player_'+inttostr(i)+eol+fPlayers.Player[i].DeliverList.WriteToText+eol+eol);
  closefile(f);
end;


procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  CheckBox3.Checked:=true;
  TrackBar1.Max:=length(PassabilityStr)-1;
  Label2.Caption:= PassabilityStr[TrackBar1.Position+1];
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  MakeDrawPagesOverlay:=CheckBox5.Checked;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  fGame.StopGame;
end;

procedure TForm1.RGPlayerClick(Sender: TObject);
begin
  MyPlayer:=fPlayers.Player[RGPlayer.ItemIndex+1];
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fSoundLib.Play(sfx_dig,KMPoint(48,48),false);
end;

procedure TForm1.OpenMissionMenuClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants Mission (*.dat)|*.dat') then exit;    
  fGame.StopGame;
  fGame.StartGame(OpenDialog1.FileName);
end;

procedure TForm1.Button5Click(Sender: TObject);
var H:TKMHouseStore; i:integer;
begin
  fGame.StopGame;
  fGame.StartGame('');
  MyPlayer:=fPlayers.Player[1];

  MyPlayer.AddHouse(ht_Store, KMPoint(4,5));
  H:=TKMHouseStore(MyPlayer.FindHouse(ht_Store,0,0));
  if H<>nil then H.AddMultiResource(rt_All,300);

  for i:=1 to 2 do MyPlayer.AddUnit(ut_Serf, KMPoint(4,8));
  MyPlayer.AddUnit(ut_Worker, KMPoint(5,8));

  MyPlayer.AddHouse(ht_Inn,KMPoint(9,8));
  //MyPlayer.AddUnit(ut_Baker, KMPoint(9,9));
  //TKMHouse(MyPlayer.FindHouse(ht_Mill,0,0)).ResAddToIn(rt_Corn,5);

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

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then fGame.GameSpeed:=50 else fGame.GameSpeed:=1;
end;

end.
