unit KM_Unit1;
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, FileCtrl, ExtCtrls, ComCtrls,
  Menus, Buttons, math, SysUtils, KromUtils, OpenGL, KromOGLUtils, dglOpenGL, JPEG,
  KM_Render, KM_RenderUI, KM_ReadGFX1, KM_Defaults, KM_GamePlayInterface,
  KM_Form_Loading, KM_Terrain,
  KM_Units, KM_Houses, KM_Viewport, KM_Log, KM_Users, KM_Controls, ColorPicker, KM_LoadLib, KM_LoadSFX;

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
    Script1: TMenuItem;
    OpenDAT: TMenuItem;
    ExportDAT: TMenuItem;
    DecodeDAT: TMenuItem;
    ExportGUIMainRX: TMenuItem;
    Exportfonts1: TMenuItem;
    GroupBox1: TGroupBox;
    Image4: TImage;
    TBZoomControl: TTrackBar;
    Image3: TImage;
    Label1: TLabel;
    Shape267: TShape;
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
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure DecodeDATClick(Sender: TObject);
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
    procedure ShowFlatTerrainClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PrintScreen1Click(Sender: TObject);
    procedure ExportGUIRXClick(Sender: TObject);
    procedure ExportTreesRXClick(Sender: TObject);
    procedure ExportHousesRXClick(Sender: TObject);
    procedure ExportUnitsRXClick(Sender: TObject);
    procedure ExportGUIMainRXClick(Sender: TObject);
    procedure Shape267MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape267DragDrop(Sender, Source: TObject; X, Y: Integer);
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

  FormLoading.Show;
  FormLoading.Refresh;

  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log'); //First thing - create a log

  FormLoading.Label1.Caption:='Initializing 3D ...';
  fRender:= TRender.Create(Form1.Panel5.Handle);

  //Must be done early on so that GamePlayInterface can use it
  FormLoading.Label1.Caption:='Reading KaM data ...';
  fTextLibrary:= TTextLibrary.Create(ExeDir+'data\misc\');
  fSoundLibrary:= TSoundLibrary.Create;
  ReadGFX(ExeDir);
  fLog.AppendLog('Resources are loaded',true);

  FormLoading.Label1.Caption:='Initializing FrontEnd ...';
  fViewport:= TViewport.Create;
  fGameSettings:= TGameSettings.Create;
  fControls:= TKMControlsCollection.Create;
  fGamePlayInterface:= TKMGamePlayInterface.Create;
  fLog.AppendLog('FrontEnd initialized',true);

  FormLoading.Label1.Caption:='Initializing Gameplay ...';
  fTerrain:= TTerrain.Create;
  fTerrain.MakeNewMap(96,96);

  fPlayers:=TKMAllPlayers.Create(6); //Create 6 players
  MyPlayer:=fPlayers.Player[1];

  Application.OnIdle:=Form1.OnIdle;
  Form1.Caption:='KaM Remake - '+'New.map';

  fLog.AppendLog('Form1 create is done');

  FormLoading.Hide;

  Timer100ms.Interval:=GAME_LOGIC_PACE; //100ms
  Form1.WindowState:=wsMaximized;
  Form1.FormResize(nil);

end;

procedure TForm1.OpenMapClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;
  fTerrain.OpenMapFromFile(OpenDialog1.FileName);
  fViewport.SetZoom:=1;
  Form1.Caption:='KaM Remake - '+OpenDialog1.FileName;
end;

procedure TForm1.FormResize(Sender:TObject);
begin
  fRender.RenderResize(Panel5.Width,Panel5.Height);
  fViewport.SetArea(Panel5.Width,Panel5.Height);
  fViewport.SetZoom:=ZoomLevels[TBZoomControl.Position];
end;

procedure TForm1.DecodeDATClick(Sender: TObject);
var ii,fsize:integer; f:file; c:array[1..65536] of char;
begin
if not OpenDialog1.Execute then exit;
fsize:=GetFileSize(OpenDialog1.FileName);
assignfile(f,OpenDialog1.FileName); reset(f,1);
blockread(f,c,fsize);
for ii:=1 to fsize do c[ii]:=chr(ord(c[ii]) xor 239);
closefile(f);
assignfile(f,OpenDialog1.FileName+'.txt'); rewrite(f,1);
blockwrite(f,c,fsize);
closefile(f);
end;

procedure TForm1.ZoomChange(Sender: TObject);
begin
fViewport.SetZoom:=ZoomLevels[TBZoomControl.Position];
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if X<=ToolBarWidth then begin
    fControls.OnMouseDown(X,Y,Button);
    exit;
  end;

  Panel1MouseMove(Panel5,Shift,X,Y);

  //example for units need change
  //Removed right since it interfers with the school buttons
  if Button = mbMiddle then
    fPlayers.Player[1].AddUnit(ut_HorseScout, KMPoint(CursorXc,CursorYc));
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if InRange(X,1,Panel5.Width-1) and InRange(Y,1,Panel5.Height-1) then
  else exit;

if X<=ToolBarWidth then begin
  fControls.OnMouseOver(X,Y,Shift);
  Screen.Cursor:=c_Default;
  exit;
end;

CursorX:=fViewport.GetCenter.X+(X-fViewport.ViewRect.Right/2-ToolBarWidth/2)/CELL_SIZE_PX/fViewport.Zoom;
CursorY:=fViewport.GetCenter.Y+(Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;

CursorY:=fTerrain.ConvertCursorToMapCoord(CursorX,CursorY);

CursorXc:=EnsureRange(round(CursorX+0.5),1,fTerrain.MapX); //Cell below cursor
CursorYc:=EnsureRange(round(CursorY+0.5),1,fTerrain.MapY);
CursorXn:=EnsureRange(round(CursorX+1),1,fTerrain.MapX); //Node below cursor
CursorYn:=EnsureRange(round(CursorY+1),1,fTerrain.MapY);

StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(CursorX*10)/10)+' '+floattostr(round(CursorY*10)/10)
+' | '+inttostr(CursorXc)+' '+inttostr(CursorYc);

if CursorMode.Mode=cm_None then
  if (fPlayers.HousesHitTest(CursorXc, CursorYc)<>nil)or
     (fPlayers.UnitsHitTest(CursorXc, CursorYc)<>nil) then
    Screen.Cursor:=c_Info
  else if not Scrolling then
    Screen.Cursor:=c_Default;

fTerrain.UpdateCursor(CursorMode.Mode,KMPoint(CursorXc,CursorYc));

end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var P:TKMPoint;
begin
  P.X:=CursorXc;
  P.Y:=CursorYc;

  if X<=ToolBarWidth then begin
    fControls.OnMouseUp(X,Y,Button);
    exit;
  end;
  
  if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button

  case CursorMode.Mode of
    cm_None:
      begin
        if fPlayers.UnitsHitTest(CursorXc, CursorYc)<>nil then begin
          fGamePlayInterface.ShowUnitInfo(fPlayers.UnitsHitTest(CursorXc, CursorYc));
          fPlayers.SelectedUnit:=fPlayers.UnitsHitTest(CursorXc, CursorYc);
        end; //Houses have priority over units, so you can't select an occupant
        if fPlayers.HousesHitTest(CursorXc, CursorYc)<>nil then begin
          fPlayers.SelectedHouse:=fPlayers.HousesHitTest(CursorXc, CursorYc);
          fGamePlayInterface.ShowHouseInfo(fPlayers.HousesHitTest(CursorXc, CursorYc));
        end;
      end;
    cm_Road: MyPlayer.AddRoadPlan(P,mu_RoadPlan);
    cm_Field: MyPlayer.AddRoadPlan(P,mu_FieldPlan);
    cm_Wine: MyPlayer.AddRoadPlan(P,mu_WinePlan);

    cm_Erase:
      begin
        MyPlayer.RemPlan(P);
        MyPlayer.RemHouse(P);
      end;
    cm_Houses:
      begin
        if MyPlayer.AddHousePlan(THouseType(CursorMode.Param),P) then
          fGamePlayInterface.SelectRoad;
      end;
    end;

  //These are only for testing purposes, Later on it should be changed a lot
  if fPlayers.SelectedUnit<>nil then
  if fPlayers.SelectedUnit.GetUnitType=ut_HorseScout then
  fPlayers.SelectedUnit.SetAction(TUnitActionWalkTo.Create(fPlayers.SelectedUnit.GetPosition,P));
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Label1.Caption:='';
  FormLoading.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fRender.Destroy;
  fGameSettings.Destroy;
  fPlayers.Destroy;
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
var i:integer;
begin
if not Form1.Active then exit;
inc(GlobalTickCount);

//if GlobalTickCount=30 then fGamePlayInterface.DropDownMessageButton(ButtonLogoID,MessageID);

if CheckBox1.Checked then exit;

if CheckBox4.Checked then
if GlobalTickCount mod 2 <> 0 then exit;

fTerrain.UpdateState;
fPlayers.UpdateState;
fGamePlayInterface.UpdateState;

if CheckBox2.Checked then
  for i:=1 to 50 do begin
    fTerrain.UpdateState;
    fPlayers.UpdateState;
   // fGamePlayInterface.UpdateState;
  end;
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

procedure TForm1.ShowFlatTerrainClick(Sender: TObject);
begin
  ShowFlatTerrain.Checked:= not ShowFlatTerrain.Checked;
  xh:=36+byte(ShowFlatTerrain.Checked)*164; // 1/36 .. 1/200
end;


procedure TForm1.Button1Click(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
TKMControl(Sender).Enabled:=false;
fViewPort.SetCenter(11,9);

for k:=-5 to 5 do for i:=-4 to 6 do
fTerrain.SetCoalReserve(KMPoint(8+i,14+k));

for k:=-5 to 5 do for i:=-4 to 6 do
fTerrain.SetOreReserve(KMPoint(21+i,6+k),rt_IronOre);

MyPlayer.AddRoadPlan(KMPoint(2,6),mu_RoadPlan);

MyPlayer.AddRoadPlan(KMPoint(2,7),mu_FieldPlan);
MyPlayer.AddRoadPlan(KMPoint(3,7),mu_FieldPlan);
MyPlayer.AddRoadPlan(KMPoint(4,7),mu_FieldPlan);
MyPlayer.AddRoadPlan(KMPoint(5,7),mu_FieldPlan);

MyPlayer.AddHouse(ht_Farm, KMPoint(3,5));
MyPlayer.AddHouse(ht_Mill, KMPoint(8,5));
MyPlayer.AddHouse(ht_Bakery, KMPoint(13,5));
MyPlayer.AddUnit(ut_Farmer, KMPoint(3,7));
MyPlayer.AddUnit(ut_Baker, KMPoint(4,7));
MyPlayer.AddUnit(ut_Baker, KMPoint(5,7));

MyPlayer.AddHouse(ht_Store, KMPoint(17,5));

MyPlayer.AddHouse(ht_WoodCutters, KMPoint(4,9));
MyPlayer.AddHouse(ht_SawMill, KMPoint(7,9));
MyPlayer.AddHouse(ht_Quary, KMPoint(12,9));
MyPlayer.AddUnit(ut_WoodCutter, KMPoint(7,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_Lamberjack, KMPoint(8,11));
MyPlayer.AddUnit(ut_StoneCutter, KMPoint(6,9));

MyPlayer.AddRoadPlan(KMPoint(2,14),mu_WinePlan);
MyPlayer.AddRoadPlan(KMPoint(3,14),mu_WinePlan);
MyPlayer.AddRoadPlan(KMPoint(4,14),mu_WinePlan);
MyPlayer.AddRoadPlan(KMPoint(5,14),mu_WinePlan);
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
if H<>nil then H.AddMultiResource(rt_All,5);

MyPlayer.AddRoadPlan(KMPoint(3,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(4,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(5,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(6,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(7,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(8,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(9,6),mu_RoadPlan);
MyPlayer.AddRoadPlan(KMPoint(10,6),mu_RoadPlan);

MyPlayer.AddHousePlan(ht_School, KMPoint(4,17));
MyPlayer.AddHousePlan(ht_Inn, KMPoint(9,18));
end;

procedure TForm1.Button2Click(Sender: TObject);
var H:TKMHouseStore; i,k:integer;
begin
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
var sh,sw,i,k:integer; jpg: TJpegImage; mkbmp:TBitmap; bmp:array of cardinal; s:string;
begin
sh:=Panel5.Height;
sw:=Panel5.Width;

setlength(bmp,sw*sh+1);
glReadPixels(0,0,sw,sh,GL_BGRA,GL_UNSIGNED_BYTE,@bmp[0]);

//Mirror verticaly
for i:=0 to (sh div 2)-1 do for k:=0 to sw-1 do
SwapInt(bmp[i*sw+k],bmp[((sh-1)-i)*sw+k]);

mkbmp:=TBitmap.Create;
mkbmp.Handle:=CreateBitmap(sw,sh,1,32,@bmp[0]);

jpg:=TJpegImage.Create;
jpg.assign(mkbmp);
jpg.ProgressiveEncoding:=true;
jpg.ProgressiveDisplay:=true;
jpg.Performance:=jpBestQuality;
jpg.CompressionQuality:=90;
jpg.Compress;
DateTimeToString(s,'yyyy-mm-dd hh-nn-ss',Now); //2007-12-23 15-24-33
jpg.SaveToFile(ExeDir+'KaM '+s+'.jpg');

jpg.Free;
mkbmp.Free;
end;

procedure TForm1.ExportTreesRXClick(Sender: TObject);  begin ExportRX2BMP(1); end;
procedure TForm1.ExportHousesRXClick(Sender: TObject); begin ExportRX2BMP(2); end;
procedure TForm1.ExportUnitsRXClick(Sender: TObject);  begin ExportRX2BMP(3); end;
procedure TForm1.ExportGUIRXClick(Sender: TObject);    begin ExportRX2BMP(4); end;
procedure TForm1.ExportGUIMainRXClick(Sender: TObject);begin ExportRX2BMP(5); end;

procedure TForm1.ExportFonts1Click(Sender: TObject);
var i:integer;
begin
  for i:=1 to length(FontFiles) do
    ReadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.fnt',TKMFont(i),true);
end;  


procedure TForm1.Shape267MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DefineInputColor((Sender as TShape).Brush.Color,Sender);
end;

procedure TForm1.Shape267DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TeamColors[3]:=Shape267.Brush.Color;
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


procedure TForm1.ExportTextClick(Sender: TObject);
begin
  fTextLibrary.ExportTextLibraries;
end;


procedure TForm1.ExportDeliverlists1Click(Sender: TObject);
var f:textfile; i:integer;
begin
assignfile(f,ExeDir+'DeliverLists.txt'); Rewrite(f);
for i:=1 to fPlayers.PlayerCount do
  writeln(f,'Player_'+inttostr(i)+eol+fPlayers.Player[i].DeliverList.WriteToText+eol+eol);
closefile(f);
end;

procedure TForm1.ExportSounds1Click(Sender: TObject);
begin
fSoundLibrary.ExportSounds;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
CheckBox3.Checked:=true;
TrackBar1.Max:=length(PassabilityStr)-1;
Label2.Caption:= PassabilityStr[TrackBar1.Position+1];
end;


procedure TForm1.HouseAnim1Click(Sender: TObject);
begin
  ExportHouseAnim2BMP();
end;

procedure TForm1.UnitAnim1Click(Sender: TObject);
begin
  ExportUnitAnim2BMP();
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  MakeDrawPagesOverlay:=CheckBox5.Checked;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
//Reset map
  fPlayers.Destroy;

  fTerrain.MakeNewMap(96,96);

  fPlayers:=TKMAllPlayers.Create(6); //Create 6 players
  MyPlayer:=fPlayers.Player[1];

end;

procedure TForm1.RGPlayerClick(Sender: TObject);
begin
  MyPlayer:=fPlayers.Player[RGPlayer.ItemIndex+1];
end;

end.
