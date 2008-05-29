unit KM_Unit1;
interface
uses
  KM_Defaults, Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, KromUtils, OpenGL,
  dglOpenGL, Menus, ComCtrls, Buttons, KM_Render, KM_ReadGFX1,
  ImgList, KM_Form_Loading, math, Grids, KM_Tplayer, KM_Terrain, KM_Global_Data,
  KM_Units, KM_Houses, KM_Viewport, KM_Log, KM_Users, JPEG;

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
    BB99: TSpeedButton;
    BB98: TSpeedButton;
    BB97: TSpeedButton;
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
    Shape1: TShape;
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
    Image1: TImage;
    Button1: TButton;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Timer1sec: TTimer;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ExportGUIMainRX: TMenuItem;
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
    procedure NewMapClick(Sender: TObject);
    procedure PalletePageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetZoomClick(Sender: TObject);
    procedure BBClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ShowWiresClick(Sender: TObject);
    procedure ShowObjectsClick(Sender: TObject);
    procedure ShowFlatTerrainClick(Sender: TObject);
    procedure ExportDATClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PrintScreen1Click(Sender: TObject);
    procedure ExportGUIRXClick(Sender: TObject);
    procedure ExportTreesRXClick(Sender: TObject);
    procedure ExportHousesRXClick(Sender: TObject);
    procedure ExportUnitsRXClick(Sender: TObject);
    procedure Timer1secTimer(Sender: TObject);
    procedure ExportGUIMainRXClick(Sender: TObject);

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

uses KM_Form_NewMap, KM_LoadDAT, KM_MapSettings;

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
end; //FPS calculation complete

fRender.Render;
done:=false; //repeats OnIdle event
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fRender:= TRender.Create;
  fViewport:= TViewport.Create;
  fTerrain:= TTerrain.Create;
  fMiniMap:= TMiniMap.Create(Shape1,MiniMap,Label1);
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
fMinimap.ReSize(Map.X,Map.Y);
fViewport.SetZoom(1);
Form1.FormResize(nil);
Form1.Caption:='KaM Editor - '+filename;
end;

procedure TForm1.FormResize(Sender:TObject);
begin
  fRender.RenderResize(Panel5.Width,Panel5.Height);
  fViewport.SetZoom(ZoomLevels[TBZoomControl.Position]);
  fViewport.SetArea(Panel5.Width,Panel5.Height);
  fMiniMap.SetRect(fViewport);
end;

procedure TForm1.DecodeDATClick(Sender: TObject);
var ii,fsize:integer;
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

  P.X:= MapXc;
  P.Y:= MapYc;
  //example for units need change
  if Button = mbRight then
    ControlList.AddUnit('User', ut_Serf, P)
  else if Button = mbMiddle then
    ControlList.AddUnit('User', ut_HorseScout, P)
  else if Button = mbLeft then
  begin
    if ControlList.UnitsSelectedUnit <> nil then
      ControlList.UnitsSelectedUnit.SetAction(TMoveUnitAction.Create(P));
    ControlList.UnitsHitTest(P.X, P.Y);
  end;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if (X>0)and(X<Panel5.Width)and(Y>0)and(Y<Panel5.Height) then
  else
    exit;
MapX:=fViewport.XCoord+(X-fViewport.ViewWidth/2)/CellSize/fViewport.Zoom;
MapY:=fViewport.YCoord+(Y-fViewport.ViewHeight/2)/CellSize/fViewport.Zoom;

MapY:=fTerrain.ConvertCursorToMapCoord(MapX,MapY);

MapXc:=EnsureRange(round(MapX+0.5),1,Map.X); //Cell below cursor
MapYc:=EnsureRange(round(MapY+0.5),1,Map.Y);
MapXn:=EnsureRange(round(MapX+1),1,Map.X); //Node below cursor
MapYn:=EnsureRange(round(MapY+1),1,Map.Y);

StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(MapX*10)/10)+' '+floattostr(round(MapY*10)/10);

if not MousePressed then exit;

MapXn2:=MapXn; MapYn2:=MapYn;
MapXc2:=MapXc; MapYc2:=MapYc;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TKMPoint;
begin
MousePressed:=false;
P.X:=MapXc;
P.Y:=MapYc;

  case BrushMode of
    bm_Houses:
      begin
        if LandBrush = 0 then
          begin
//            Mission.RemRoad(MapXc,MapYc);
            ControlList.RemHouse(P);
          end
        else
        if LandBrush in [1..29] then
        ControlList.AddHouse(THouseType(LandBrush),P)
        else
//          if LandBrush = 99 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer);
      end;
  end;
MouseButton:=mb2None;
end;

procedure TForm1.Pl1Click(Sender: TObject);
begin
s:=(TSpeedButton(Sender)).Name;
Mission.ActivePlayer:=strtoint(s[3]);
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Show;
end;

procedure TForm1.NewMapClick(Sender: TObject);
begin
  FormNewMap.Show;
end;

procedure TForm1.PalletePageChange(Sender: TObject);
begin //
if ActiveTileName<>nil then
TSpeedButton(ActiveTileName).Down:=false; //Relese last pressed button
LandBrush:=0;
BrushMode:=bm_None;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
fRender.Destroy;
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
var i:integer;
begin
if CheckBox1.Checked then exit;
ControlList.UpdateState;
fTerrain.UpdateState;
if CheckBox2.Checked then
for i:=1 to 9 do
  ControlList.UpdateState;
end;

procedure TForm1.ResetZoomClick(Sender: TObject);
const crHand = 30;
var
bm:TBitmap;
IconInfo:TIconInfo;
begin
  TBZoomControl.Position:=4;

  bm:=TBitmap.Create;
  bm.LoadFromFile(ExeDir+'Image 00033.bmp');

  Pallete.Canvas.StretchDraw(Pallete.ClientRect,bm);

  IconInfo.fIcon:=false;
  IconInfo.xHotspot:=1;
  IconInfo.yHotspot:=1;
  IconInfo.hbmMask:=bm.Handle;
  IconInfo.hbmColor:=bm.Handle;

  Screen.Cursors[crHand]:=CreateIconIndirect(iconInfo);
  Screen.Cursor:=crHand;
end;

procedure TForm1.BBClick(Sender: TObject);
begin
ActiveTileName:=Sender;
s:=(TSpeedButton(Sender)).Name;
LandBrush:=strtoint(s[3]+s[4]);
BrushMode:=bm_Houses;
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
  ControlList.Add('User', uct_User);
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

procedure TForm1.ExportDATClick(Sender: TObject);
begin
  if Mission=nil then
  begin
ExportDAT.Enabled:=false;
    exit;
  end;
  if not RunSaveDialog(SaveDialog1,'Mission.txt','','Mission text (*.txt)|*.txt') then
    exit;
ExportText(SaveDialog1.FileName);
end; 

procedure TForm1.Button1Click(Sender: TObject);
begin
fViewPort.XCoord:=11;
fViewPort.YCoord:=11;
ControlList.AddHouse(ht_Farm,KMPoint(4,5));
ControlList.AddHouse(ht_Mill,KMPoint(8,5));
ControlList.AddHouse(ht_Bakery,KMPoint(12,5));
ControlList.AddHouse(ht_Store,KMPoint(16,5));
ControlList.AddHouse(ht_Quary,KMPoint(12,8));
ControlList.AddHouse(ht_WoodCutters,KMPoint(12,11));
ControlList.AddHouse(ht_SawMill,KMPoint(12,14));

ControlList.AddUnit('User', ut_Farmer, KMPoint(5,9));
ControlList.AddUnit('User', ut_StoneCutter, KMPoint(6,9));
ControlList.AddUnit('User', ut_WoodCutter, KMPoint(7,9));
ControlList.AddUnit('User', ut_Lamberjack, KMPoint(8,9));
ControlList.AddUnit('User', ut_Baker, KMPoint(9,9));
ControlList.AddUnit('User', ut_Baker, KMPoint(10,9));
ControlList.AddUnit('User', ut_Serf, KMPoint(4,11));
ControlList.AddUnit('User', ut_Serf, KMPoint(5,11));
ControlList.AddUnit('User', ut_Serf, KMPoint(6,11));
ControlList.AddUnit('User', ut_Serf, KMPoint(7,11));
ControlList.AddUnit('User', ut_Worker, KMPoint(8,11));
ControlList.AddUnit('User', ut_Worker, KMPoint(9,11));

ControlList.AddRoadPlan(KMPoint(5,12),rdt_Road);
ControlList.AddRoadPlan(KMPoint(6,12),rdt_Road);
ControlList.AddRoadPlan(KMPoint(7,13),rdt_Road);
ControlList.AddRoadPlan(KMPoint(7,14),rdt_Road);
ControlList.AddRoadPlan(KMPoint(7,15),rdt_Road);
ControlList.AddRoadPlan(KMPoint(7,12),rdt_Road);

ControlList.AddHousePlan(KMPoint(7,16), ht_Inn);
end;

procedure TForm1.PrintScreen1Click(Sender: TObject);
var sh,sw,i,k,h:integer; t:byte; jpg: TJpegImage; mkbmp:TBitmap; bmp:array of array[1..4]of byte;
begin
sh:=fViewPort.ViewHeight;
sw:=fViewPort.ViewWidth;

setlength(bmp,sw*sh+1);
glReadPixels(0,0,sw,sh,GL_BGRA,GL_UNSIGNED_BYTE,@bmp[0]);

//Mirror verticaly
for i:=0 to (sh div 2)-1 do for k:=0 to sw-1 do for h:=1 to 4 do begin
t:=bmp[i*sw+k,h]; bmp[i*sw+k,h]:=bmp[((sh-1)-i)*sw+k,h]; bmp[((sh-1)-i)*sw+k,h]:=t; end;

mkbmp:=TBitmap.Create;
mkbmp.Handle:=CreateBitmap(sw,sh,1,32,@bmp[0,1]);

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

procedure TForm1.ExportGUIRXClick(Sender: TObject);    begin ExportRX2BMP('GUI');    end;
procedure TForm1.ExportGUIMainRXClick(Sender: TObject);begin ExportRX2BMP('GUIMain');end;
procedure TForm1.ExportTreesRXClick(Sender: TObject);  begin ExportRX2BMP('Trees');  end;
procedure TForm1.ExportHousesRXClick(Sender: TObject); begin ExportRX2BMP('Houses'); end;
procedure TForm1.ExportUnitsRXClick(Sender: TObject);  begin ExportRX2BMP('Units');  end;

procedure TForm1.Timer1secTimer(Sender: TObject);
begin
fMiniMap.Repaint;
end;

end.
