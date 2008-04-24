unit KM_Unit1;
interface
uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, KromUtils, OpenGL,
  dglOpenGL, Menus, ComCtrls, Buttons, KM_Defaults, KM_Render, KM_ReadGFX1,
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
    ImageList1: TImageList;
    Pl1: TSpeedButton;
    Pl2: TSpeedButton;
    Pl3: TSpeedButton;
    Pl4: TSpeedButton;
    Pl5: TSpeedButton;
    Pl6: TSpeedButton;
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
    Button3: TButton;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    ExportDAT: TButton;
    Timer1: TTimer;
    Image1: TImage;
    TabSheet1: TTabSheet;
    Memo2: TMemo;
    TabSheet2: TTabSheet;
    HousePallete: TDrawGrid;
    ImageList2: TImageList;
    ImageList3: TImageList;
    UnitPallete: TDrawGrid;
    UnitPalleteScroll: TScrollBar;
    HousePalleteScroll: TScrollBar;
    Button1: TButton;
    PrintScreen1: TMenuItem;
    procedure OpenDATClick(Sender: TObject);
    procedure OpenMap(filename:string);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure ConvertDATClick(Sender: TObject);
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
    procedure HousePalleteScrollChange(Sender: TObject);
    procedure HousePalleteDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Mapsettings1Click(Sender: TObject);
    procedure ExportDATClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UnitPalleteScrollChange(Sender: TObject);
    procedure UnitPalleteDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Button1Click(Sender: TObject);
    procedure PrintScreen1Click(Sender: TObject);

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
fMinimap.Repaint();
fViewport.SetZoom(10);
Form1.FormResize(nil);
Form1.Caption:='KaM Editor - '+filename;
end;

procedure TForm1.FormResize(Sender:TObject);
begin
  fRender.RenderResize(Panel5.Width,Panel5.Height);
  fViewport.SetZoom(sqr(TBZoomControl.Position/20)*10);
  fViewport.SetArea(Panel5.Width,Panel5.Height);
  fMiniMap.SetRect(fViewport);
end;

procedure TForm1.ConvertDATClick(Sender: TObject);
var ii:integer;
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
fViewport.SetZoom(sqr(TBZoomControl.Position/20)*10);
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
  P: TPoint;
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
    ControlList.AddUnit('User', ut_WHorseScout, P)
  else if Button = mbLeft then
  begin
    if ControlList.UnitsSelectedUnit <> nil then
      ControlList.UnitsSelectedUnit.SetAction(TMoveUnitAction.Create(P.X, P.Y));
    ControlList.UnitsHitTest(P.X, P.Y);
  end;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
  ii,Tmp:integer;
    Ycoef:array[-2..4]of single;
begin
  if (X>0)and(X<Panel5.Width)and(Y>0)and(Y<Panel5.Height) then
  else
    exit;
MapX:=(fViewport.XCoord+(X-fViewport.ViewWidth/2)/CellSize/fViewport.Zoom*10);
MapY:=(fViewport.YCoord+(Y-fViewport.ViewHeight/2)/CellSize/fViewport.Zoom*10);

MapXc:=EnsureRange(round(MapX+0.5),1,Map.X); //Cell below cursor
MapYc:=EnsureRange(round(MapY+0.5),1,Map.Y);

  for ii:=-2 to 4 do
  begin//make an array of tile heights above and below cursor (-2..4)
Tmp:=EnsureRange(MapYc+ii,1,Map.Y);
Ycoef[ii]:=(MapYc-1)+ii-(fTerrain.Land[Tmp,MapXc].Height*(1-frac(MapX))+fTerrain.Land[Tmp,MapXc+1].Height*frac(MapX))/xh;
end;

for ii:=-2 to 3 do //check if cursor in a tile and adjust it there
    if (MapY>=Ycoef[ii])and(MapY<=Ycoef[ii+1]) then
    begin
      MapY:=MapYc+ii-0.5;
      break;
    end;

MapXc:=EnsureRange(round(MapX+0.5),1,Map.X); //Cell below cursor
MapYc:=EnsureRange(round(MapY+0.5),1,Map.Y);
MapXn:=EnsureRange(round(MapX+1),1,Map.X); //Node below cursor
MapYn:=EnsureRange(round(MapY+1),1,Map.Y);

StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(MapX*10)/10)+' '+floattostr(round(MapY*10)/10);

  if not MousePressed then
    exit;
                  {
if BrushMode=bmHouses then
    if (MapXc in [2..Map.X-1])and(MapYc in [2..Map.Y-1]) then
    begin
      if LandBrush=99 then
        Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer);
      if LandBrush=00 then
        Mission.RemRoad(MapXc,MapYc);
end;                       }

MapXn2:=MapXn; MapYn2:=MapYn;
MapXc2:=MapXc; MapYc2:=MapYc;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TPoint;
begin
MousePressed:=false;
P.X:=MapXc;
P.Y:=MapYc;

  case BrushMode of
    bmHouses:
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
fMiniMap.Repaint;
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
BrushMode:=bmNone;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
fRender.Destroy;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ControlList.UpdateState;
  //ControlList.Paint();
//  Units.Paint();
end;

procedure TForm1.ResetZoomClick(Sender: TObject);
begin TBZoomControl.Position:=20; end;

procedure TForm1.BBClick(Sender: TObject);
begin
ActiveTileName:=Sender;
s:=(TSpeedButton(Sender)).Name;
LandBrush:=strtoint(s[3]+s[4]);
BrushMode:=bmHouses;
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

procedure TForm1.HousePalleteScrollChange(Sender: TObject);
begin
  HousePallete.TopRow:=HousePalleteScroll.Position;
  Form1.Caption:=inttostr(HouseID[HousePalleteScroll.Position+1]);
end;

procedure TForm1.HousePalleteDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
ImageList2.Draw(HousePallete.Canvas,Rect.Left,Rect.Top,ARow);
end;

procedure TForm1.UnitPalleteScrollChange(Sender: TObject);
begin UnitPallete.TopRow:=UnitPalleteScroll.Position; Form1.Caption:=inttostr(UnitID[UnitPalleteScroll.Position+1]); end;

procedure TForm1.UnitPalleteDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
ImageList3.Draw(UnitPallete.Canvas,Rect.Left,Rect.Top,ARow);
end;

procedure TForm1.Mapsettings1Click(Sender: TObject);
begin
Form_MapSettings.Show;
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
var
  P: TPoint;
begin
fViewPort.XCoord:=10;
fViewPort.YCoord:=10;
ControlList.AddHouse(ht_Farm,Point(4,5));
ControlList.AddHouse(ht_Mill,Point(8,5));
//ControlList.HousesHitTest(4,5);
//Houses.SelectedHouse.AddResource(rt_Corn);
ControlList.AddHouse(ht_Bakery,Point(12,5));
ControlList.AddHouse(ht_Store,Point(16,5));

P.x:=random(2)+5;
P.y:=random(8)+5;
//  ControlList.AddUnit('User', ut_Serf, P);
  ControlList.AddUnit('User', ut_VFarmer, Point(8,8));
//  Units.HitTest(x, y);
//  Units.SelectedUnit.GiveResource(rt_Corn);
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

end.
