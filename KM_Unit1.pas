unit KM_Unit1;
interface
uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, FileCtrl, ExtCtrls, ComCtrls,
  Menus, Buttons, math, SysUtils, KromUtils, OpenGL, KromOGLUtils, dglOpenGL, JPEG,
  KM_Render, KM_RenderUI, KM_ReadGFX1, KM_Defaults, KM_GamePlayInterface,
  KM_Form_Loading, KM_Tplayer, KM_Terrain, KM_Global_Data,
  KM_Units, KM_Houses, KM_Viewport, KM_Log, KM_Users, KM_Controls, ColorPicker;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    TBZoomControl: TTrackBar;
    SaveDialog1: TSaveDialog;
    Pallete: TPageControl;
    TabSheet3: TTabSheet;
    BB01: TSpeedButton;
    BB02: TSpeedButton;
    BB03: TSpeedButton;
    BB04: TSpeedButton;
    BB05: TSpeedButton;
    BB06: TSpeedButton;
    BB07: TSpeedButton;
    BB08: TSpeedButton;
    BB09: TSpeedButton;
    BB10: TSpeedButton;
    BB97: TSpeedButton;
    BB99: TSpeedButton;
    BB98: TSpeedButton;
    BB00: TSpeedButton;
    BB11: TSpeedButton;
    BB12: TSpeedButton;
    BB13: TSpeedButton;
    BB14: TSpeedButton;
    BB15: TSpeedButton;
    BB16: TSpeedButton;
    BB17: TSpeedButton;
    BB18: TSpeedButton;
    BB21: TSpeedButton;
    BB19: TSpeedButton;
    BB22: TSpeedButton;
    BB20: TSpeedButton;
    BB23: TSpeedButton;
    BB24: TSpeedButton;
    BB25: TSpeedButton;
    BB26: TSpeedButton;
    BB27: TSpeedButton;
    BB28: TSpeedButton;
    BB29: TSpeedButton;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    MiniMap: TImage;
    ShapeFOV: TShape;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenMapMenu: TMenuItem;
    NewMapMenu: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Advanced1: TMenuItem;
    ShowWires: TMenuItem;
    ShowObjects: TMenuItem;
    ShowFlatTerrain: TMenuItem;
    Panel5: TPanel;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
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
    Pl1: TSpeedButton;
    Pl4: TSpeedButton;
    Pl5: TSpeedButton;
    Pl2: TSpeedButton;
    Pl3: TSpeedButton;
    Pl6: TSpeedButton;
    Timer1sec: TTimer;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ExportGUIMainRX: TMenuItem;
    Shape267: TShape;
    Exportfonts1: TMenuItem;
    procedure OpenDATClick(Sender: TObject);
    procedure OpenMap(filename:string);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure DecodeDATClick(Sender: TObject);
    procedure OpenMapClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Pl1Click(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetZoomClick(Sender: TObject);
    procedure BBClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ShowWiresClick(Sender: TObject);
    procedure ShowObjectsClick(Sender: TObject);
    procedure ShowFlatTerrainClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PrintScreen1Click(Sender: TObject);
    procedure ExportGUIRXClick(Sender: TObject);
    procedure ExportTreesRXClick(Sender: TObject);
    procedure ExportHousesRXClick(Sender: TObject);
    procedure ExportUnitsRXClick(Sender: TObject);
    procedure Timer1secTimer(Sender: TObject);
    procedure ExportGUIMainRXClick(Sender: TObject);
    procedure Shape267MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape267DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Exportfonts1Click(Sender: TObject);  
    procedure DoScrolling;

  private     { Private declarations }
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public      { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;
  ControlList: TKMUserControlList;

implementation  {$R *.DFM}

uses KM_LoadDAT;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var
  FrameTime:integer;
begin //Counting FPS
if not Form1.Active then exit;
FrameTime:=GetTickCount-OldTimeFPS;
OldTimeFPS:=GetTickCount;
if (FPSLag<>1)and(FrameTime<FPSLag) then
begin
  sleep(FPSLag-FrameTime);
  FrameTime:=FPSLag;
end;
inc(OldFrameTimes,FrameTime);
inc(FrameCount);
if OldFrameTimes>=FPS_INTERVAL then
begin
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
  fControls:= TKMControlsCollection.Create;
  fGamePlayInterface:= TKMGamePlayInterface.Create;
  fRender:= TRender.Create;
  fViewport:= TViewport.Create;
  fTerrain:= TTerrain.Create;
  fMiniMap:= TMiniMap.Create(ShapeFOV,MiniMap,Label1);
  Application.OnIdle:=Form1.OnIdle;
end;

procedure TForm1.OpenMapClick(Sender: TObject);
begin
if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;
OpenMap(OpenDialog1.FileName);
end;

procedure TForm1.OpenMap(filename:string);
begin
fTerrain.OpenMapFromFile(filename);
fViewport.SetZoom(1);
Form1.FormResize(nil);
Form1.Caption:='KaM Remake - '+filename;
end;

procedure TForm1.FormResize(Sender:TObject);
begin
  fRender.RenderResize(Panel5.Width,Panel5.Height);
  fViewport.SetArea(Panel5.Width,Panel5.Height);
  fViewport.SetZoom(ZoomLevels[TBZoomControl.Position]);
  fMiniMap.SetRect(fViewport);
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
fViewport.SetZoom(ZoomLevels[TBZoomControl.Position]);
fMiniMap.SetRect(fViewport);
end;

procedure TForm1.MiniMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin MiniMapSpy:=true; MiniMapMouseMove(nil,Shift,X,Y); end;

procedure TForm1.MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
if not MiniMapSpy then exit;
fViewport.SetCenter(X,Y);
fMiniMap.SetRect(fViewport);
end;

procedure TForm1.MiniMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin MiniMapMouseMove(nil,Shift,X,Y); MiniMapSpy:=false; end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TKMPoint;
begin
  MousePressed:=true;
  case Button of
    mbLeft:
      MouseButton:= mb2Left;
    mbRight:
      MouseButton:= mb2Right;
  else
    MouseButton:=mb2None;
  end;
  Panel1MouseMove(Panel5,Shift,X,Y);

  P.X:= CursorXc;
  P.Y:= CursorYc;

  fControls.OnMouseDown(X,Y,Button);

  //example for units need change
  if Button = mbRight then
    ControlList.AddUnit(play_1, ut_Serf, P)
  else if Button = mbMiddle then
    ControlList.AddUnit(play_1, ut_HorseScout, P);

end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if (X>0)and(X<Panel5.Width)and(Y>0)and(Y<Panel5.Height) then
  else exit;
CursorX:=fViewport.XCoord+(X-fViewport.ViewRect.Right/2+ToolBarWidth/2)/CellSize/fViewport.Zoom;
CursorY:=fViewport.YCoord+(Y-fViewport.ViewRect.Bottom/2)/CellSize/fViewport.Zoom;

CursorY:=fTerrain.ConvertCursorToMapCoord(CursorX,CursorY);

CursorXc:=EnsureRange(round(CursorX+0.5),1,fTerrain.MapX); //Cell below cursor
CursorYc:=EnsureRange(round(CursorY+0.5),1,fTerrain.MapY);
CursorXn:=EnsureRange(round(CursorX+1),1,fTerrain.MapX); //Node below cursor
CursorYn:=EnsureRange(round(CursorY+1),1,fTerrain.MapY);

StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(CursorX*10)/10)+' '+floattostr(round(CursorY*10)/10)
+' | '+inttostr(CursorXc)+' '+inttostr(CursorYc);

if CursorMode=cm_None then
  if (ControlList.HousesHitTest(CursorXc, CursorYc)<>nil)or
     (ControlList.UnitsHitTest(CursorXc, CursorYc)<>nil) then
    Screen.Cursor:=c_Info
  else
    Screen.Cursor:=c_Default;

fTerrain.UpdateCursor(CursorMode,KMPoint(CursorXc,CursorYc));
fControls.OnMouseOver(X,Y,Shift);

if not MousePressed then exit;

CursorXn2:=CursorXn; CursorYn2:=CursorYn;
CursorXc2:=CursorXc; CursorYc2:=CursorYc;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    P:TKMPoint;
begin
  MousePressed:=false;
  P.X:=CursorXc;
  P.Y:=CursorYc;

  if X<=ToolBarWidth then
    fControls.OnMouseUp(X,Y,Button)
  else

  case CursorMode of
    cm_None:
      begin
        if ControlList.HousesHitTest(CursorXc, CursorYc)<>nil then
          fGamePlayInterface.ShowHouseInfo(ControlList.HousesHitTest(CursorXc, CursorYc).GetHouseType);
      end;
    cm_Roads:
      begin
        if LandBrush=1 then ControlList.AddRoadPlan(P,mu_RoadPlan);
        if LandBrush=2 then ControlList.AddRoadPlan(P,mu_FieldPlan);
        if LandBrush=3 then ControlList.AddRoadPlan(P,mu_WinePlan);
      end;
    cm_Erase:
      begin
        ControlList.RemPlan(P);
        ControlList.RemHouse(P);
      end;
    cm_Houses:
      if LandBrush in [1..29] then
        ControlList.AddHousePlan(P,THouseType(LandBrush),play_1)
    end;
MouseButton:=mb2None;
end;

procedure TForm1.Pl1Click(Sender: TObject);
begin
//Obsolete
s:=(TSpeedButton(Sender)).Name;
Mission.ActivePlayer:=strtoint(s[3]);
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
fRender.Destroy;
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
var i:integer;
begin
if not Form1.Active then exit;
if CheckBox1.Checked then exit;
ControlList.UpdateState;

inc(GlobalTickCount);
if GlobalTickCount mod 2 = 0 then fTerrain.UpdateState; //Update every third tick

if CheckBox2.Checked then
  for i:=1 to 50 do
    ControlList.UpdateState;

  //Now check to see if we need to scroll
  DoScrolling;
end;

procedure TForm1.ResetZoomClick(Sender: TObject);
begin
  TBZoomControl.Position:=4;
end;

procedure TForm1.BBClick(Sender: TObject);
begin
if TSpeedButton(Sender).Down=false then begin
  CursorMode:=cm_None;
  ActiveTileName:=nil;
  exit;
end;
ActiveTileName:=Sender;
s:=(TSpeedButton(Sender)).Name;
LandBrush:=strtoint(s[3]+s[4]);
case LandBrush of
  0:      CursorMode:=cm_Erase;
  1..29:  CursorMode:=cm_Houses;
  97..99: begin CursorMode:=cm_Roads; dec(LandBrush,96); end; //Brush in 1..3
  else    CursorMode:=cm_None;
end;
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
  xh:=36+integer(ShowFlatTerrain.Checked)*164; // 1/36 .. 1/200
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  ControlList:= TKMUserControlList.Create();
  ControlList.Add(play_1, uct_User);
end;

destructor TForm1.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TForm1.OpenDATClick(Sender: TObject);
begin
if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants dat (*.dat)|*.dat') then exit;
LoadDAT(OpenDialog1.FileName);
ExportDAT.Enabled:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
TKMControl(Sender).Enabled:=false;
fViewPort.XCoord:=6;
fViewPort.YCoord:=11;
ControlList.AddHouse(play_1, ht_Farm,KMPoint(4,5));
ControlList.AddHouse(play_1, ht_Mill,KMPoint(8,5));
ControlList.AddHouse(play_1, ht_Bakery,KMPoint(12,5));
ControlList.AddHouse(play_1, ht_Store,KMPoint(16,5));
ControlList.AddHouse(play_1, ht_Quary,KMPoint(12,8));
ControlList.AddHouse(play_1, ht_WoodCutters,KMPoint(12,11));
ControlList.AddHouse(play_1, ht_SawMill,KMPoint(12,14));

ControlList.AddUnit(play_1, ut_Farmer, KMPoint(15,9));
ControlList.AddUnit(play_1, ut_StoneCutter, KMPoint(6,9));
ControlList.AddUnit(play_1, ut_WoodCutter, KMPoint(7,9));
ControlList.AddUnit(play_1, ut_Lamberjack, KMPoint(8,9));
ControlList.AddUnit(play_1, ut_Baker, KMPoint(9,9));
ControlList.AddUnit(play_1, ut_Baker, KMPoint(10,9));
ControlList.AddUnit(play_1, ut_Serf, KMPoint(4,11));
ControlList.AddUnit(play_1, ut_Serf, KMPoint(5,11));
ControlList.AddUnit(play_1, ut_Serf, KMPoint(6,11));
ControlList.AddUnit(play_1, ut_Serf, KMPoint(7,11));
ControlList.AddUnit(play_1, ut_Worker, KMPoint(8,11));
ControlList.AddUnit(play_1, ut_Worker, KMPoint(9,11));

ControlList.AddRoadPlan(KMPoint(5,13),mu_FieldPlan);
ControlList.AddRoadPlan(KMPoint(6,13),mu_FieldPlan);
ControlList.AddRoadPlan(KMPoint(7,13),mu_FieldPlan);
ControlList.AddRoadPlan(KMPoint(5,14),mu_WinePlan);
ControlList.AddRoadPlan(KMPoint(6,14),mu_WinePlan);
ControlList.AddRoadPlan(KMPoint(7,14),mu_WinePlan);

ControlList.AddRoadPlan(KMPoint(5,12),mu_RoadPlan);
ControlList.AddRoadPlan(KMPoint(6,12),mu_RoadPlan);
ControlList.AddRoadPlan(KMPoint(7,12),mu_RoadPlan);
ControlList.AddRoadPlan(KMPoint(8,12),mu_RoadPlan);
ControlList.AddRoadPlan(KMPoint(8,13),mu_RoadPlan);
ControlList.AddRoadPlan(KMPoint(8,14),mu_RoadPlan);

ControlList.AddHousePlan(KMPoint(9,18), ht_Inn, play_1);
end;

procedure TForm1.PrintScreen1Click(Sender: TObject);
var sh,sw,i,k:integer; jpg: TJpegImage; mkbmp:TBitmap; bmp:array of cardinal;
begin
sh:=Panel5.Width;
sw:=Panel5.Height;

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

procedure TForm1.Exportfonts1Click(Sender: TObject);
var i:integer;
begin
  for i:=1 to length(FontFiles) do
    ReadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.fnt',TKMFont(i),true);
end;

procedure TForm1.Timer1secTimer(Sender: TObject);
begin
  if not Form1.Active then exit;
  fMiniMap.Repaint; //No need to repaint it more often
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

procedure TForm1.DoScrolling;
var
  XCoord, YCoord: integer;
  HaveChanged: boolean;
begin
  //Here we must test each edge to see if we need to scroll in that direction
  //We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Defaults

  //First set X and Y to be the current values
  XCoord := fViewport.XCoord;
  YCoord := fViewport.YCoord;
  HaveChanged := false;

  // -----------------      LEFT       ------------------
  if Mouse.CursorPos.X < SCROLLFLEX then
  begin
    //We must scroll left
    XCoord := XCoord-SCROLLSPEED;
    HaveChanged := true;
  end;
  // -----------------      TOP       ------------------
  if Mouse.CursorPos.Y < SCROLLFLEX then
  begin
    //We must scroll up
    YCoord := YCoord-SCROLLSPEED;
    HaveChanged := true;
  end;
  // -----------------      RIGHT       ------------------
  if Mouse.CursorPos.X > Screen.Width-1-SCROLLFLEX then
  begin
    //We must scroll right
    XCoord := XCoord+SCROLLSPEED;
    HaveChanged := true;
  end;         
  // -----------------      BOTTOM       ------------------
  if Mouse.CursorPos.Y > Screen.Height-1-SCROLLFLEX then
  begin
    //We must scroll down
    YCoord := YCoord+SCROLLSPEED;
    HaveChanged := true;
  end;

  //Now do actual the scrolling, if needed
  if HaveChanged then
  begin
    fViewport.SetCenter(XCoord,YCoord);
    fMiniMap.SetRect(fViewport); //Update mini-map
  end;
end;

{//I want to suggest. Please don't think of me as a moroon, but I feel important to keep code tight rather than spread
//I squeezed 48lines into 19. Since they do all fit into one screen it's much easier to read now.
//Cursors will probably add another 12lines to it
//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Defaults
procedure TForm1.DoScrolling;
var XCoord, YCoord: integer;
begin
  //First set X and Y to be the current values
  XCoord := fViewport.XCoord;
  YCoord := fViewport.YCoord;

  //Left, Top, Right, Bottom
  if Mouse.CursorPos.X < SCROLLFLEX then XCoord := XCoord-SCROLLSPEED;
  if Mouse.CursorPos.Y < SCROLLFLEX then YCoord := YCoord-SCROLLSPEED;
  if Mouse.CursorPos.X > Screen.Width -1-SCROLLFLEX then XCoord := XCoord+SCROLLSPEED;
  if Mouse.CursorPos.Y > Screen.Height-1-SCROLLFLEX then YCoord := YCoord+SCROLLSPEED;

  //Now do actual the scrolling, if needed
  if (XCoord<>fViewport.XCoord)or(YCoord<>fViewport.YCoord) then
  begin
    fViewport.SetCenter(XCoord,YCoord);
    fMiniMap.SetRect(fViewport); //Update mini-map
  end;
end;}

end.
