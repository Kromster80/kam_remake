unit KM_Unit1;
{$I ..\KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, glut, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, KromUtils,
  {$IFDEF FPC} GL, LResources, {$ENDIF}
  dglOpenGL, Menus, ComCtrls, Buttons, KM_Defaults, KM_Render,
  KM_Form_Loading, Math, Grids, Spin, ImgList;

{$IFDEF Unix}
type HGLRC = integer;
//type HDC = integer;
{$ENDIF}

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    TBZoomControl: TTrackBar;
    SaveDialog1: TSaveDialog;
    Pallete: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    LB01: TSpeedButton;
    LB10: TSpeedButton;
    LB08: TSpeedButton;
    LB06: TSpeedButton;
    LB11: TSpeedButton;
    LB05: TSpeedButton;
    LB18: TSpeedButton;
    LB19: TSpeedButton;
    LB20: TSpeedButton;
    LB02: TSpeedButton;
    LB07: TSpeedButton;
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
    BB97: TSpeedButton;
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
    Shape1: TShape;
    ObjPallete: TDrawGrid;
    ImageList1: TImageList;
    ObjPalleteScroll: TScrollBar;
    LB03: TSpeedButton;
    LB04: TSpeedButton;
    LB12: TSpeedButton;
    LB21: TSpeedButton;
    LB13: TSpeedButton;
    LB14: TSpeedButton;
    LB15: TSpeedButton;
    LB16: TSpeedButton;
    LB17: TSpeedButton;
    LB09: TSpeedButton;
    LB22: TSpeedButton;
    LB23: TSpeedButton;
    TabSheet4: TTabSheet;
    TabSheet6: TTabSheet;
    LB24: TSpeedButton;
    LB25: TSpeedButton;
    LB26: TSpeedButton;
    Label3: TLabel;
    TileRotateButton: TBitBtn;
    ObjBlock: TSpeedButton;
    ObjErase: TSpeedButton;
    CBRandomizeTiling: TCheckBox;
    GB01: TSpeedButton;
    GB03: TSpeedButton;
    GB02: TSpeedButton;
    SaveMapButton: TBitBtn;
    OpenMapButton: TBitBtn;
    NewMapButton: TBitBtn;
    Undo: TBitBtn;
    Redo: TBitBtn;
    ScrollBox2: TScrollBox;
    Image5: TImage;
    Shape2: TShape;
    LoadReliefFromBMP: TButton;
    OpenProButton: TBitBtn;
    Pl1: TSpeedButton;
    Pl2: TSpeedButton;
    Pl3: TSpeedButton;
    Pl4: TSpeedButton;
    Pl5: TSpeedButton;
    Pl6: TSpeedButton;
    BrushSize: TTrackBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenMapMenu: TMenuItem;
    NewMapMenu: TMenuItem;
    Exit1: TMenuItem;
    SaveMapMenu: TMenuItem;
    N1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Advanced1: TMenuItem;
    ShowWires: TMenuItem;
    ShowObjects: TMenuItem;
    ShowFlatTerrain: TMenuItem;
    Panel5: TPanel;
    Panel1: TPanel;
    ScrollBar2: TScrollBar;
    ScrollBar1: TScrollBar;
    Button1: TButton;
    Edit1: TMenuItem;
    CopyAreaMenu: TMenuItem;
    PasteAreaMenu: TMenuItem;
    Button3: TButton;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Memo2: TMemo;
    ExportDAT: TButton;
    HousePallete: TDrawGrid;
    ImageList2: TImageList;
    HousePalleteScroll: TScrollBar;
    ConvertDAT: TButton;
    Label4: TLabel;
    Label5: TLabel;
    ShowFlow: TMenuItem;
    Pl7: TSpeedButton;
    Pl8: TSpeedButton;
    GroupBox1: TGroupBox;
    SpinEdit1: TSpinEdit;
    Button4: TButton;
    Label6: TLabel;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    RG_Terrain: TRadioGroup;
    RG_Relief: TRadioGroup;
    GroupBox2: TGroupBox;
    ElevSize: TTrackBar;
    GroupBox3: TGroupBox;
    ElevSpeed: TTrackBar;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    RG_Angle: TRadioGroup;
    MagicWater: TBitBtn;
    procedure OpenDATClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RenderResize(Sender:TObject);
    procedure RenderFrame(Sender:TObject);
    procedure ConvertDATClick(Sender: TObject);
    procedure OpenMapClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure TerrainTileSelect(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Pl1Click(Sender: TObject);
    procedure ObjPalleteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure ObjPalleteScrollChange(Sender: TObject);
    procedure ObjPalleteClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure NewMapClick(Sender: TObject);
    procedure PalletePageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ObjBlockClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TileRotateButtonClick(Sender: TObject);
    procedure ResetZoomClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure ObjEraseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LoadReliefFromBMPClick(Sender: TObject);
    procedure MakeUndoPoint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BBClick(Sender: TObject);
    procedure SaveMapClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure OpenProClick(Sender: TObject);
    procedure ShowWiresClick(Sender: TObject);
    procedure ShowObjectsClick(Sender: TObject);
    procedure ShowFlatTerrainClick(Sender: TObject);
    procedure CopyAreaMenuClick(Sender: TObject);
    procedure PasteAreaMenuClick(Sender: TObject);
    procedure HousePalleteScrollChange(Sender: TObject);
    procedure HousePalleteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure ExportDATClick(Sender: TObject);
    procedure ShowFlowClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure MagicWaterClick(Sender: TObject);
  public
    procedure RenderInit;
    procedure DoMagicWater(X,Y:integer);
    procedure RebuildMap(X,Y,Rad:integer);
    procedure BrushTerrainTile(Y,X,Index:integer);
    procedure PrepareMapToSave;
    procedure PrepareLoadedMap(filename:string);
    procedure OpenMap(filename:string);
    procedure OpenPro(filename:string);
    procedure CopyTerrain;
    procedure PasteTerrain;
    procedure UpdateLight(X,Y:integer);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

  procedure BuildMiniMap;

  const
    MaxMapSize = 1024;      //Max map dimension
    CellSize = 40;          //Single cell size in pixels
    Overlap = 0.25 / 256;   //UV position overlap
    MaxUndoSteps = 32 + 1;  //Undo steps + 1 for last redo
    FPSLag = 1; //33;       //lag between frames, 1000/FPSLag = max allowed FPS
    FPS_INTERVAL = 1000;    //time between FPS measurements, more=accurate

    MaxPlayers = 6;   //Maximum players per map
    MaxHouses = 255;  //Maximum houses one player can own

var
  h_DC: HDC;
  h_RC: HGLRC;
  Text1,TextG,TextA:GLuint;
  Tree:array[1..256]of GLuint;     //Object textures
  House:array[1..32]of GLuint;     //Object textures
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations

  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  Form1: TForm1;
  f,f2:file;
  ft:textfile;
  fsize,NumRead:integer;
  c:array[1..131072] of AnsiChar;
  ExeDir:string;
  XH:single=33.333;        //Height divider
  BrushMode:bmBrushMode;
  LandBrush:integer=0;  //Active brush
  Zoom:single=10;
  s:string;
  Map: record X,Y:integer; end;
  ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
  Res:array[1..MaxMapSize*2]of packed record X1,Y1,X2,Y2:integer; Typ:byte; end;
  MouseButton:shortint;

  MapX,MapY:single; //Precise cursor position on map
  MapXn,MapYn:integer; //Cursor position node
  MapXc,MapYc:integer; //Cursor position cell
  MapXn2,MapYn2:integer; //keeps previous node position
  MapXc2,MapYc2:integer; //keeps previous cell position



  CopyArea:array[1..2,1..2]of integer; //CopyRectangle

  Land:array[1..MaxMapSize+1,1..MaxMapSize+1]of packed record
    Terrain,Light,Height1,Rot,x3,Obj,Pass:byte;
    c1,c2,c3,c4:byte;
    y1,y2,y3,Border,y5,y6,y7,y8,y9,y10,y11,y12:byte;
  end;
  Land2:array[1..MaxMapSize+1,1..MaxMapSize+1]of record
    TerType:shortint; //Stores terrain type per node
    Tiles:smallint;  //Stores kind of transition tile used, no need to save into MAP footer
    Height:single;   //Floating-point height for smoother elevation, temp value only for editing
    Light:byte;
  end;
  LandCopy:array[1..MaxMapSize+1,1..MaxMapSize+1]of packed record
    Terrain,Height1,Rot,Obj:byte;
    TerType:shortint;
  end;

  UndoStep:integer=1;
  RedoStep:integer=0;
  UndoCounter:integer=0;
  UndoLand:array[1..MaxUndoSteps,1..MaxMapSize,1..MaxMapSize]of record
  Terrain,Height,Rot,Obj:byte;
  TerType:shortint;
  Tiles:smallint;
  end;

TreeQty:integer;
TreePal:array[1..512] of byte;
TreeSize:array[1..512,1..2] of word;
TreePivot:array[1..512] of record x,y:integer; end;
TreeData:array[1..512] of array of byte;
Pal0:array[1..256,1..3]of byte;
ObjPalleteTable:array[0..90,0..12]of integer; //Pallete rows info

HouseQty:integer;
HousePal:array[1..2000] of byte;
HouseSize:array[1..2000,1..2] of word;
HousePivot:array[1..2000] of record x,y:integer; end;
HouseData:array[1..2000] of array of byte;
HouseID:array[1..2000] of word;

MapElemQty:integer=254; //Default qty
MapElem:array[1..512]of packed record
Tree:array[1..30]of smallint; //60
animqty:word;                 //62
u1:array[1..16]of word;       //94
u2:shortint;                  //95
u3,u4:word;                   //99
end;

  ActiveTileName:TObject; //Object (Brush) that was pressed last, should be released on tab change
  MiniMapSpy:boolean=false;
  MousePressed:boolean=false;
  AnyChangesMade:boolean=false;




implementation
{$R *.dfm}


uses KM_Form_NewMap, KM_LoadDAT, KM_TPlayer, KM_TGATexture;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var FrameTime:cardinal;
    i,k,Rad:integer;
    x1,x2,y1,y2:integer;
    Tmp:single;
begin
  if not Form1.Active then exit;

  //Counting FPS
  FrameTime:=GetTickCount-OldTimeFPS;
  OldTimeFPS:=GetTickCount;

  if (FPSLag<>1)and(FrameTime<FPSLag) then
  begin
    sleep(FPSLag-FrameTime);
    FrameTime := FPSLag;
  end;

  inc(OldFrameTimes, FrameTime);
  inc(FrameCount);

  if OldFrameTimes >= FPS_INTERVAL then
  begin
    StatusBar1.Panels[2].Text:=floattostr(round((1000/(OldFrameTimes/FrameCount))*10)/10)+' fps ('+IntToStr(1000 div FPSLag)+')';
    OldFrameTimes := 0;
    FrameCount := 0;
  end;
  //FPS calculation complete

  if MousePressed and (BrushMode=bmRelief) and (LandBrush<>0) then begin
    Rad:=ElevSize.Position;
    x1:=max(MapXn-Rad,1);
    x2:=min(MapXn+Rad,Map.X);
    y1:=max(MapYn-Rad,1);
    y2:=min(MapYn+Rad,Map.Y);

    for i:=y1 to y2 do
    for k:=x1 to x2 do
    begin
      case RG_Relief.ItemIndex of
        0:    Tmp := 1 - sqrt(sqr(i-MapYn)+sqr(k-MapXn))/Rad;//circular falloff
        1:    Tmp := 1 - max(abs(i-MapYn), abs(k-MapXn))/Rad; //pyramid falloff
        else  Tmp := 0;
      end;
      if Tmp<0 then Tmp:=0;

      if (Tmp>0) then
        case LandBrush of
          1:  Tmp := MouseButton*Tmp*(ElevSpeed.Position/5); //Elevate smooth
          2:  Tmp := MouseButton*(ElevSpeed.Position/5); //Elevate plateu
          3:  begin
                if Land2[i,k].Height < Land2[MapYc,MapXc].Height then
                  Tmp:=-min(Land2[MapYc,MapXc].Height-Land2[i,k].Height,abs(MouseButton))
                else //Flatten
                if Land2[i,k].Height > Land2[MapYc,MapXc].Height then
                  Tmp:=min(Land2[i,k].Height-Land2[MapYc,MapXc].Height,abs(MouseButton))
                else
                  Tmp:=0;

                  Tmp:=MouseButton*Tmp;
              end;
        end;

      Tmp := Tmp*(FrameTime/10)/2; //say it takes 2sec to raise from 0 to 100

      Land2[i,k].Height := EnsureRange(Land2[i,k].Height+Tmp, 0, 100);

      UpdateLight(k,i); //Update all 3 concerned nodes
      UpdateLight(k+1,i);
      UpdateLight(k,i+1);
    end;
  end;

  RenderFrame(nil);
  Done := False; //repeats OnIdle event
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Pallete.ActivePageIndex:=0;
  Application.OnIdle := Form1.OnIdle;
end;


procedure TForm1.OpenMapClick(Sender: TObject);
begin
  if AnyChangesMade then
  if MessageBox(Form1.Handle,'Any unsaved changes will be lost. Proceed?', 'Warning', MB_ICONWARNING or MB_OKCANCEL{$IFDEF MSWindows} or MB_APPLMODAL{$ENDIF}) <> IDOK then exit;

  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;
  OpenMap(OpenDialog1.FileName);
end;

procedure TForm1.OpenMap(filename:string);
var
  i,k:integer;
  Head:record X,Y:integer; end;
begin

  assignfile(f,filename);
  reset(f,1);
  blockread(f, Head, 8);

  if (Head.X > MaxMapSize) or (Head.Y > MaxMapSize) then
  begin
    closefile(f);
    MessageBox(Form1.Handle,'Too big map or not a KaM map.', 'Error', MB_OK);
    exit;
  end;

  reset(f,1);
  blockread(f,Map,8);
  for i:=1 to Map.Y do blockread(f,Land[i],Map.X*23);
  blockread(f,ResHead,22);
  for i:=1 to ResHead.Allocated do
  blockread(f,Res[i],17);

  blockread(f,c,4,NumRead);
  if NumRead=4 then begin
    blockread(f,c,4,NumRead); c[5] := #0; //ADDN
    if StrPas(PAnsiChar(@c[1]))='TILE' then
    begin
      blockread(f,i,4,NumRead); //Chunk size
      blockread(f,i,4,NumRead); //Cypher - ommited
      for i:=1 to Map.Y do
      for k:=1 to Map.X do
        blockread(f,Land2[i,k].TerType,1);
    end;
  end;
  closefile(f);

  PrepareLoadedMap(filename);
end;

procedure TForm1.RenderInit;
begin
  glClearColor(0.77, 0.77, 0.77, 0); 	   //Background
  glClear (GL_COLOR_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);                 //Enables Smooth Color Shading
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
  LoadTextureTGA(ExeDir+'Resource\Tiles1.tga', Text1);    // Load the Textures
  LoadTextureTGA(ExeDir+'Resource\gradient.tga', TextG); // Load the Textures
  LoadTextureTGA(ExeDir+'Resource\arrow.tga', TextA);    // Load the Textures
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glPolygonMode(GL_FRONT,GL_FILL);
end;

procedure TForm1.RenderResize(Sender:TObject);
begin
  if Panel1.Height=0 then Panel1.Height:=1;
  if Panel1.Width=0  then Panel1.Width :=1;        // Panel1.Height/Panel1.Width

  glViewport(0, 0, Panel1.Width, Panel1.Height);
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                   // Reset View
  //Half a map into each direction
  gluOrtho2D(-Panel1.Width/CellSize/2,Panel1.Width/CellSize/2,Panel1.Height/CellSize/2,-Panel1.Height/CellSize/2);
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity;                   // Reset View
  ZoomChange(nil);
end;

procedure TForm1.RenderFrame(Sender:TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View
  glScalef(Zoom/10,Zoom/10,Zoom/10);
  glTranslatef(-ScrollBar1.Position,-ScrollBar2.Position,0);

  glLineWidth(Zoom/4);
  glPointSize(Zoom/2);

  RenderTerrainAndRoads;

  if ShowWires.Checked then RenderWires;

  RenderObjects;
  RenderBuildings;
  RenderCursorPosition(Form1.Pallete.ActivePage.Caption);

  RenderArrows;
  {$IFDEF MSWindows}
  SwapBuffers(h_DC);
  {$ENDIF}
  {$IFDEF Unix}
  //Hopefully it swaps correct thing or something at all;
  glutSwapBuffers;
  //MessageBox(Form1.Handle,'SwapBuffers not working', 'Error', MB_OK);
  {$ENDIF}
end;


procedure TForm1.ConvertDATClick(Sender: TObject);
var ii:integer;
begin
  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants dat (*.dat)|*.dat') then exit;
  fsize:=GetFileSize(OpenDialog1.FileName);
  assignfile(f,OpenDialog1.FileName); reset(f,1);
  blockread(f,c,fsize);
  for ii:=1 to fsize do
    c[ii] := AnsiChar(ord(c[ii]) xor 239);
  closefile(f);
  assignfile(f,OpenDialog1.FileName+'.txt'); rewrite(f,1);
  blockwrite(f,c,fsize);
  closefile(f);
end;


procedure TForm1.PrepareMapToSave;
var
  i,k:integer;
begin
  for i:=1 to Map.Y do
  begin
    Land[i,Map.X].x3    := 20;
    Land[i,Map.X].Pass  := 0;
    Land[i,1].Border    := 0;
    Land[i,Map.X].Border:= 205;
  end;

  for k:=1 to Map.X do
  begin
    Land[Map.Y,k].x3    := 20;
    Land[Map.Y,k].Pass  := 0;
    Land[1,k].Border    := 0;
    Land[Map.Y,k].Border:= 205;
  end;

  for i:=1 to Map.Y do
  for k:=1 to Map.X do
  begin
    Land[i,k].Height1 := EnsureRange(round(Land2[i,k].Height),0,100);
    Land[i,k].Pass    := TilePassability[Land[i,k].Terrain+1];
  end;

  UpdateLight(0,0);

  for i:=1 to Map.Y do for k:=1 to Map.X do
    if (i=1) or (i=Map.Y) or (k=1) or (k=Map.X) then
      Land[i,k].Light := 0 //Edges
    else
      Land[i,k].Light := Land2[i,k].Light;
end;


procedure TForm1.ZoomChange(Sender: TObject);
var RatioX,RatioY:single;
begin
  Zoom:=sqr(TBZoomControl.Position/20)*10;
  Label1.Caption:=IntToStr(round(Zoom*10))+'%';
  RatioX:=Form1.MiniMap.Width  / max(Map.X,1);
  RatioY:=Form1.MiniMap.Height / max(Map.Y,1);
  Shape1.Width :=round((Panel1.Width /CellSize/Zoom*10)*RatioX);
  Shape1.Height:=round((Panel1.Height/CellSize/Zoom*10)*RatioY);
  Shape1.Left:=Form1.MiniMap.Left+round(ScrollBar1.Position*RatioX - Shape1.Width /2);
  Shape1.Top :=Form1.MiniMap.Top+ round(ScrollBar2.Position*RatioY - Shape1.Height/2);
end;

procedure BuildMiniMap;
var
  i,k,j:integer;
  MyBitmap:TBitmap;
  {$IFDEF WDC}P:PByteArray;{$ENDIF}
begin
  MyBitmap:=TBitmap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  MyBitmap.Width:=Map.X;
  MyBitmap.Height:=Map.Y;

  for i:=1 to Map.Y-1 do begin
    {$IFDEF WDC} P:=MyBitmap.ScanLine[i-1]; {$ENDIF}
    for k:=1 to Map.X-1 do begin
      {$IFDEF WDC}
      P[k*3-1]:=EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF + Land2[i,k].Light*4-64,0,255);
      P[k*3-2]:=EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF00 SHR 8 + Land2[i,k].Light*4-64,0,255);
      P[k*3-3]:=EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF0000 SHR 16 + Land2[i,k].Light*4-64,0,255);
      {$ENDIF}
      {$IFDEF FPC}
      MyBitmap.Canvas.Pixels[k-1,i-1] := EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF + Land2[i,k].Light*4-64,0,255) +
                                         EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF00 SHR 8 + Land2[i,k].Light*4-64,0,255) shl 8 +
                                         EnsureRange(MMap[Land[i,k].Terrain+1] AND $FF0000 SHR 16 + Land2[i,k].Light*4-64,0,255) shl 16;
      {$ENDIF}
    end;
  end;

  if Mission<>nil then
  for i:=1 to Map.Y-1 do for k:=1 to Map.X-1 do
  if Mission.Roads[k,i]<>gpN then
  MyBitmap.Canvas.Pixels[k-1,i-1]:=
    PlayerColors[Mission.Owner[k,i],1]+
    PlayerColors[Mission.Owner[k,i],2]*256+
    PlayerColors[Mission.Owner[k,i],3]*65536 else
  for j:=1 to 8 do
  if Mission.Player[j].HitTest(k,i)<>0 then
  MyBitmap.Canvas.Pixels[k-1,i-1]:=
    PlayerColors[j,1]+
    PlayerColors[j,2]*256+
    PlayerColors[j,3]*65536;

  //Change aspect
  //(Form1.MiniMap.Height/Form1.MiniMap.Width)=0.92
  if Map.X>=Map.Y then begin
    Form1.MiniMap.Width:=208;
    Form1.MiniMap.Height:=round(192*(Map.Y/Map.X));
    Form1.MiniMap.Top:=1+(192-Form1.MiniMap.Height) div 2;
    Form1.MiniMap.Left:=1;
    {Form1.MiniMap.Picture.Width
    Form1.MiniMap.Picture.Width
    Form1.MiniMap.ClientWidth:=Form1.MiniMap.Width;
    Form1.MiniMap.ClientHeight:=Form1.MiniMap.Height;}
  end else begin
    Form1.MiniMap.Width:=round(208*(Map.X/Map.Y));
    Form1.MiniMap.Height:=192;
    Form1.MiniMap.Top:=1;
    Form1.MiniMap.Left:=1+(208-Form1.MiniMap.Width) div 2;
    {Form1.MiniMap.Picture.Width
    Form1.MiniMap.Picture.Width
    Form1.MiniMap.ClientWidth:=Form1.MiniMap.Width;
    Form1.MiniMap.ClientHeight:=Form1.MiniMap.Height;}
  end;

  Form1.MiniMap.Canvas.StretchDraw(Form1.MiniMap.ClientRect,MyBitmap);

  MyBitmap.Free;
end;

procedure TForm1.MiniMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin MiniMapSpy:=true; MiniMapMouseMove(nil,Shift,X,Y); end;

procedure TForm1.MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if not MiniMapSpy then exit;
  ScrollBar1.Position:=round(X/(Form1.MiniMap.Width  / Map.X));
  ScrollBar2.Position:=round(Y/(Form1.MiniMap.Height  / Map.Y));
  ZoomChange(nil);
end;

procedure TForm1.MiniMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin MiniMapMouseMove(nil,Shift,X,Y); MiniMapSpy:=false; end;

procedure TForm1.TerrainTileSelect(Sender: TObject);
begin
  LandBrush := 0;
  BrushMode := bmNone;
  CopyAreaMenu.Checked := false;
  PasteAreaMenu.Checked := false;
  ActiveTileName := Sender;
  s := (TSpeedButton(Sender)).Name;
  if not TSpeedButton(Sender).Down then
    exit; // button released
  if s[1] + s[2] = 'LB' then
  begin
    LandBrush := strtoint(s[3] + s[4]); // Get brush ID 1..100
    BrushMode := bmTerrain;
  end;
  if s[1] + s[2] = 'GB' then
  begin
    LandBrush := strtoint(s[3] + s[4]); // Get brush ID
    BrushMode := bmRelief;
  end;
  StatusBar1.Panels[3].Text := 'Brush: ' + IntToStr(LandBrush);
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MakeUndoPoint(nil); //Make whenever user uses any brushes
  if Button=mbLeft  then MouseButton:=1;
  if Button=mbRight then MouseButton:=-1;
  //MapXn2:=MapXn; MapYn2:=MapYn;
  //MapXc2:=MapXc; MapYc2:=MapYc;
  Panel1MouseMove(Panel1,Shift,X,Y);
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var ii,kk,Rad,Tmp:integer;
    Ycoef:array[-2..5]of single;
begin
MousePressed:=(ssLeft in Shift)or(ssRight in Shift);
if (X>0)and(X<Panel1.Width)and(Y>0)and(Y<Panel1.Height) then else exit;
MapX:=(ScrollBar1.Position+(X-Panel1.Width/2)/CellSize/Zoom*10);
MapY:=(ScrollBar2.Position+(Y-Panel1.Height/2)/CellSize/Zoom*10);

MapXc:=EnsureRange(round(MapX+0.5),1,Map.X); //Cell below cursor
MapYc:=EnsureRange(round(MapY+0.5),1,Map.Y);

for ii:=-2 to 5 do begin//make an array of tile heights above and below cursor (-2..5)
Tmp:=EnsureRange(MapYc+ii,1,Map.Y);
Ycoef[ii]:=(MapYc-1)+ii-(Land2[Tmp,MapXc].Height*(1-frac(MapX))+Land2[Tmp,MapXc+1].Height*frac(MapX))/xh;
end;

for ii:=4 downto -2 do //check if cursor in a tile and adjust it there
if (MapY>=Ycoef[ii])and(MapY<=Ycoef[ii+1]) then begin MapY:=MapYc+ii-
(Ycoef[ii+1]-MapY) / (Ycoef[ii+1]-Ycoef[ii]); break; end;

MapXc:=EnsureRange(round(MapX+0.5),1,Map.X); //Cell below cursor
MapYc:=EnsureRange(round(MapY+0.5),1,Map.Y);
MapXn:=EnsureRange(round(MapX+1),1,Map.X); //Node below cursor
MapYn:=EnsureRange(round(MapY+1),1,Map.Y);

StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(MapX*10)/10)+' '+floattostr(round(MapY*10)/10);

if not MousePressed then exit;

if (BrushMode=bmTerrain)and(LandBrush<>0) then
  begin
  Rad:=BrushSize.Position;
  if (Rad=0)and((MapXn2<>MapXn)or(MapYn2<>MapYn)) then
  Land2[MapYn,MapXn].TerType:=LandBrush
  else
  if (MapXc2<>MapXc)or(MapYc2<>MapYc) then
  if Rad mod 2 = 1 then
    begin                                               //There are two brush types here, even and odd size
    Rad:=Rad div 2;                                     //first comes odd sizes 1,3,5..
    for ii:=-Rad to Rad do for kk:=-Rad to Rad do       //
    if (RG_Terrain.ItemIndex=1)or(sqrt(sqr(ii)+sqr(kk))<Rad+0.5) then               //Rounding corners in a nice way
    BrushTerrainTile(MapYc+ii,MapXc+kk,LandBrush);
    end
  else
    begin
    Rad:=Rad div 2;                                     //even sizes 2,4,6..
    for ii:=-Rad to Rad-1 do for kk:=-Rad to Rad-1 do   //
    if (RG_Terrain.ItemIndex=1)or(sqrt(sqr(ii+0.5)+sqr(kk+0.5))<Rad) then           //Rounding corners in a nice way
    BrushTerrainTile(MapYc+ii,MapXc+kk,LandBrush);
    end;
  RebuildMap(MapXc,MapYc,Rad+5);
  end;

if BrushMode=bmTiles then
if ((MapXc2<>MapXc)or(MapYc2<>MapYc))and(LandBrush in [1..255]) then begin
Land[MapYc,MapXc].Terrain:=LandBrush-1;
Land2[MapYc,MapXc].TerType:=-abs(Land2[MapYc,MapXc].TerType);
if RG_Angle.ItemIndex=4 then Land[MapYc,MapXc].Rot:=random(4)
                        else Land[MapYc,MapXc].Rot:=RG_Angle.ItemIndex;
//special fix for tiles, which are placed upside down
if LandBrush in [118,125,126] then Land[MapYc,MapXc].Rot:=(Land[MapYc,MapXc].Rot+2) mod 4;
end;

if BrushMode=bmTileRotate then begin
  if MapXc2*MapYc2=0 then begin
    MapXc2:=MapXc;
    MapYc2:=MapYc;
  end;
  Land[MapYc,MapXc].Rot:=Land[MapYc2,MapXc2].Rot; //Repeat previous tile Rotation
end;

if BrushMode=bmObjects then
if LandBrush=255 then Land[MapYc,MapXc].Obj:=255; //remove object

if BrushMode=bmHouses then
if InRange(MapXc,1,Map.X-1)and InRange(MapYc,1,Map.Y-1) then begin
if LandBrush in [00,97..99] then Mission.RemRoad(MapXc,MapYc);
if LandBrush=99 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpR);
if LandBrush=98 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpF);
if LandBrush=97 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpW);
end;

if BrushMode=bmCopy then begin
if MouseButton= 1 then begin if CopyArea[2,1]=0 then CopyArea[1,1]:=MapXc else
                             CopyArea[1,1]:=min(MapXc,CopyArea[2,1]); CopyArea[1,2]:=MapYc; end;
if MouseButton=-1 then begin CopyArea[2,1]:=max(MapXc,CopyArea[1,1]); CopyArea[2,2]:=MapYc; end;
end;

MapXn2:=MapXn; MapYn2:=MapYn;
MapXc2:=MapXc; MapYc2:=MapYc;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i,k,ID:integer;
begin
if LandBrush<>0 then begin
Undo.Enabled:=true;
RedoStep:=0;
Redo.Enabled:=false;

ID:=(UndoCounter+1-1) mod MaxUndoSteps + 1; //1..Max //Looks like we are making last Redo step here

  for i:=1 to Map.Y do for k:=1 to Map.X do begin
  UndoLand[ID,i,k].Terrain:=Land[i,k].Terrain;
  UndoLand[ID,i,k].Height:=round(EnsureRange(Land2[i,k].Height,0,100));
  UndoLand[ID,i,k].Rot:=Land[i,k].Rot;
  UndoLand[ID,i,k].Obj:=Land[i,k].Obj;
  UndoLand[ID,i,k].TerType:=Land2[i,k].TerType;
  UndoLand[ID,i,k].Tiles:=Land2[i,k].Tiles;
  end;
  UpdateLight(0,0);
end;

if BrushMode=bmObjects then
if LandBrush=255 then Land[MapYc,MapXc].Obj:=255 else
if LandBrush<>0  then Land[MapYc,MapXc].Obj:=ObjIndex[LandBrush];

if (BrushMode=bmTileRotate) then begin
  if MouseButton= 1 then Land[MapYc,MapXc].Rot:=(Land[MapYc,MapXc].Rot + 1) mod 4;
  if MouseButton=-1 then if Land[MapYc,MapXc].Rot<1 then Land[MapYc,MapXc].Rot:=3 else dec(Land[MapYc,MapXc].Rot);
end;

if (BrushMode=bmMagicWater) then begin
  DoMagicWater(MapXc,MapYc);
end;

if BrushMode=bmCopy then
CopyTerrain;

if BrushMode=bmPaste then
PasteTerrain;

if BrushMode=bmHouses then begin
if LandBrush = 0 then Mission.RemHouse(MapXc,MapYc);
if LandBrush in [00,97..99] then Mission.RemRoad(MapXc,MapYc);
if LandBrush=99 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpR);
if LandBrush=98 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpF);
if LandBrush=97 then Mission.AddRoad(MapXc,MapYc,Mission.ActivePlayer,gpW);
if LandBrush in [1..29] then begin
  Mission.Player[Mission.ActivePlayer].AddHouse(LandBrush,MapXc,MapYc);
  LandBrush:=99;
end;
end;

MouseButton:=0;
BuildMiniMap;

MapXc2:=0; MapYc2:=0; MapXn2:=0; MapYn2:=0; //Reset
end;

procedure TForm1.Pl1Click(Sender: TObject);
begin
  s := (TSpeedButton(Sender)).Name;
  Mission.ActivePlayer := strtoint(s[3]);
  Label4.Caption := 'Player: ' + s[3];
end;

procedure TForm1.ObjPalleteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
//ObjPallete.RowHeights[ARow]:=ObjPalleteTable[ARow+1,0]+4; done in ReadGFX
//ObjPallete.Canvas.FillRect(Rect);
if (ACol=0)or(ObjPalleteTable[ARow+1,ACol+1]<>ObjPalleteTable[ARow+1,ACol+1-1]) then
ImageList1.Draw(ObjPallete.Canvas,
                Rect.Left,
                Rect.Top+(ObjPalleteTable[ARow+1,0]+2-TreeSize[ObjIndexGFX[ObjPalleteTable[ARow+1,ACol+1]],2]),
                ObjPalleteTable[ARow+1,ACol+1]-1);
end;


procedure TForm1.ObjPalleteScrollChange(Sender: TObject);
begin
  ObjPallete.TopRow := ObjPalleteScroll.Position;
end;


procedure TForm1.ObjPalleteClick(Sender: TObject);
begin
  ObjBlock.Down := false;
  ObjErase.Down := false;
  LandBrush := ObjPalleteTable[ObjPallete.Row + 1, ObjPallete.Col + 1];
  BrushMode := bmObjects;
  StatusBar1.Panels[3].Text := 'Object: ' + IntToStr(LandBrush) + ' ' + IntToStr(ObjIndexGFX[LandBrush]) + ' (' +
    IntToStr(ObjIndex[LandBrush]) + ')';
end;


procedure TForm1.AboutClick(Sender: TObject);
begin
  FormLoading.Bar1.Position:=0;
  FormLoading.Show;
end;

procedure TForm1.ScrollBarChange(Sender: TObject);
begin
  ZoomChange(nil);
  RenderFrame(nil);
end;

procedure TForm1.NewMapClick(Sender: TObject);
begin
  if AnyChangesMade then
  if MessageBox(Form1.Handle,'Any unsaved changes will be lost. Proceed?', 'Warning', MB_ICONWARNING or MB_OKCANCEL{$IFDEF MSWindows} or MB_APPLMODAL{$ENDIF}) <> IDOK then exit;
  FormNewMap.Show;
end;

procedure TForm1.BrushTerrainTile(Y, X, Index: integer);
var
  xx, yy, T: integer;
begin
  if (Y < 1) or (X < 1) then
    exit;
  if (Y > Map.Y) or (X > Map.X) then
    exit;
  Land[Y, X].Terrain := Index;
  xx := EnsureRange(X, 1, Map.X - 1);
  yy := EnsureRange(Y, 1, Map.Y - 1);
  Land2[yy, xx].TerType := Index;
  Land2[yy, xx + 1].TerType := Index;
  Land2[yy + 1, xx + 1].TerType := Index;
  Land2[yy + 1, xx].TerType := Index;

  T := abs(Combo[Index, Index, 1]); //Pick a tile ID from table
  if (CBRandomizeTiling.Checked) then
    case LandBrush of
      1 .. 17, 21 .. 23:
        if random(6) = 1 then
          T := RandomTiling[Index, random(RandomTiling[Index, 0]) + 1]; // chance of 1/6
      18 .. 20, 24 .. 26:
        T := RandomTiling[Index, random(RandomTiling[Index, 0]) + 1]; //equal chance
    end;
  Land[yy, xx].Terrain := T;
  Land[yy, xx].Rot := random(4); //random direction for all plain tiles
end;

procedure TForm1.RebuildMap(X,Y,Rad:integer);
var i,k,pY,pX,Nodes,Rot,Ter1,Ter2,T,A,B,C,D:integer;
begin
for i:=-Rad to Rad do for k:=-Rad to Rad do if sqrt(sqr(i)+sqr(k))<Rad then
  begin
  pX:=EnsureRange(X+k,1,Map.X);
  pY:=EnsureRange(Y+i,1,Map.Y);
  if (Land2[pY  ,pX].TerType>0)and(Land2[pY  ,pX+1].TerType>0)
  and(Land2[pY+1,pX].TerType>0)and(Land2[pY+1,pX+1].TerType>0) then //don't touch custom placed tiles (with negative values)
    begin
    A:=abs(Land2[pY  ,pX].TerType); B:=abs(Land2[pY  ,pX+1].TerType);
    C:=abs(Land2[pY+1,pX].TerType); D:=abs(Land2[pY+1,pX+1].TerType);
    Rot:=0; Nodes:=1;// Ter1:=1; Ter2:=1;

    if (A=B)or(C=D)  then begin Ter1:=A; Ter2:=C; Nodes:=2; if A<C then Rot:=2 else Rot:=0; end;
    if (A=C)or(B=D)  then begin Ter1:=A; Ter2:=B; Nodes:=2; if A<B then Rot:=1 else Rot:=3; end;

    if A=D then begin Ter1:=A; Ter2:=B; Nodes:=4+1; Rot:=1; end; //special case \
    if B=C then begin Ter1:=A; Ter2:=B; Nodes:=4+2; Rot:=0; end; //special case /

    if (A=B)and(C=D) then begin Ter1:=A; Ter2:=C; Nodes:=2; if A<C then Rot:=2 else Rot:=0; end;
    if (A=C)and(B=D) then begin Ter1:=A; Ter2:=B; Nodes:=2; if A<B then Rot:=1 else Rot:=3; end;

    if (B=C)and(C=D) then begin Ter1:=C; Ter2:=A; Nodes:=3; if C<A then Rot:=3 else Rot:=1; end;
    if (A=C)and(C=D) then begin Ter1:=A; Ter2:=B; Nodes:=3; if A<B then Rot:=0 else Rot:=2; end;
    if (A=B)and(B=D) then begin Ter1:=A; Ter2:=C; Nodes:=3; if A<C then Rot:=2 else Rot:=0; end;
    if (A=B)and(B=C) then begin Ter1:=A; Ter2:=D; Nodes:=3; if A<D then Rot:=1 else Rot:=3; end;

    if (A=B)and(B=C)and(C=D) then begin Ter1:=A; Ter2:=A; Nodes:=4; Rot:=0; end;

      //Terrain table has only half filled, so make sure first comes bigger ID
      if Ter1<Ter2 then
        begin T:=Ter1; Ter1:=Ter2; Ter2:=T;
          case Nodes of
          1..3: Nodes:=4-Nodes;  //hence invert nodes count
          5..6: Rot:=1;
          end;
        end;

      //Some tiles placed upside and should be rotated 180`
      if Nodes<4 then
      if Combo[Ter1,Ter2,Nodes]<0 then case Rot of //Flip direction
      0: Rot:=2; 1: Rot:=3; 2: Rot:=0; 3: Rot:=1; end;

      if Nodes<4 then T:=abs(Combo[Ter1,Ter2,Nodes]);     //transition tiles
      if Nodes=4 then T:=abs(Combo[Ter1,Ter2,1]);         //no transition
      if Nodes>4 then T:=abs(Combo[Ter1,Ter2,3]);         //transition use 1 or 3

      if (CBRandomizeTiling.Checked)and(Ter1=Ter2) then //for plain tiles only
        case Ter1 of
        1..17,21..23: if random(6)=1 then T:=RandomTiling[Ter1,random(RandomTiling[Ter1,0])+1]; // chance of 1/6
        18..20,24..26: T:=RandomTiling[Ter1,random(RandomTiling[Ter1,0])+1]; //equal chance
        end;

      if Ter1=Ter2 then Rot:=random(4); //random direction for all plain tiles

      //Need to check if this tile was already smart-painted, "4-Nodes" hence default value is 0
      if Land2[pY,pX].Tiles<>Ter1*Ter2*(4-Nodes) then
        begin
        Land2[pY,pX].Tiles:=Ter1*Ter2*(4-Nodes);//store not only nodes info, but also terrain type used
        Land[pY,pX].Terrain:=T;
        Land[pY,pX].Rot:=Rot mod 4;
        end;
    end;
  end;
end;

procedure TForm1.PalletePageChange(Sender: TObject);
begin //
  if ActiveTileName <> nil then
    TSpeedButton(ActiveTileName).Down := false; //Relese last pressed button
  LandBrush := 0;
  BrushMode := bmNone;
  CopyAreaMenu.Checked := false;
  PasteAreaMenu.Checked := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
{$IFDEF MSWindows}
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
{$ENDIF}
{$IFDEF Unix}
  //do not know how to fix them :(
  //glXMakeCurrent(display, wid, util_glctx);
  //glXDestroyContext(h_RC);
  MessageBox(Form1.Handle, 'glXMakeCurrent and glXDestroyContext not working', 'Error', MB_OK);
{$ENDIF}
end;

procedure TForm1.ObjBlockClick(Sender: TObject);
begin
  if not TSpeedButton(Sender).Down then
  begin
    LandBrush := 0;
    exit;
  end;
  ActiveTileName := Sender;
  LandBrush := 30; //aka 42 block passage
  BrushMode := bmObjects;
  StatusBar1.Panels[3].Text := 'Object: Block Passage';
end;

procedure TForm1.ObjEraseClick(Sender: TObject);
begin
  if not TSpeedButton(Sender).Down then
  begin
    LandBrush := 0;
    exit;
  end;
  ActiveTileName := Sender;
  LandBrush := 255;
  BrushMode := bmObjects;
  StatusBar1.Panels[3].Text := 'Object: Erase';
end;

procedure TForm1.HelpClick(Sender: TObject);
begin
  //Application.HelpJump('HelpContents');
end;

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  Shape2.Left := TImage(Sender).Left + X div 32 * 32;
  Shape2.Top := TImage(Sender).Top + Y div 32 * 32;
  LandBrush := TileRemap[(Y div 32) * 32 + (X div 32) + 1];
  BrushMode := bmTiles;
  StatusBar1.Panels[3].Text := 'Brush: ' + IntToStr(LandBrush);
end;


procedure TForm1.TileRotateButtonClick(Sender: TObject);
begin
  LandBrush := 1;
  BrushMode := bmTileRotate;
  StatusBar1.Panels[3].Text := 'Brush: Rotate Tile';
end;


procedure TForm1.MagicWaterClick(Sender: TObject);
begin
  LandBrush := 1;
  BrushMode := bmMagicWater;
  StatusBar1.Panels[3].Text := 'Brush: Magic Water';
end;


procedure TForm1.ResetZoomClick(Sender: TObject);
begin TBZoomControl.Position:=20; end;

procedure TForm1.UndoClick(Sender: TObject);
var i,k,ID:integer;
begin
  ID := (UndoCounter - 1) mod MaxUndoSteps + 1; //1..Max
  for i := 1 to Map.Y do
    for k := 1 to Map.X do
    begin
      Land[i, k].Terrain := UndoLand[ID, i, k].Terrain;
      Land2[i, k].Height := UndoLand[ID, i, k].Height;
      Land[i, k].Rot := UndoLand[ID, i, k].Rot;
      Land[i, k].Obj := UndoLand[ID, i, k].Obj;
      Land2[i, k].TerType := UndoLand[ID, i, k].TerType;
      Land2[i, k].Tiles := UndoLand[ID, i, k].Tiles;
    end;
  UpdateLight(0, 0);
  dec(UndoStep);
  dec(UndoCounter);
  Redo.Enabled := true;
  inc(RedoStep);
  if UndoStep = 1 then
    Undo.Enabled := false; //last one is occupied by actual map. Needed for Redo functional
  BuildMiniMap;
end;

procedure TForm1.RedoClick(Sender: TObject);
var i,k,ID:integer;
begin
  inc(UndoCounter);
  ID := (UndoCounter + 1 - 1) mod MaxUndoSteps + 1; //1..Max
  for i := 1 to Map.Y do
    for k := 1 to Map.X do
    begin
      Land[i, k].Terrain := UndoLand[ID, i, k].Terrain;
      Land2[i, k].Height := UndoLand[ID, i, k].Height;
      Land[i, k].Rot := UndoLand[ID, i, k].Rot;
      Land[i, k].Obj := UndoLand[ID, i, k].Obj;
      Land2[i, k].TerType := UndoLand[ID, i, k].TerType;
      Land2[i, k].Tiles := UndoLand[ID, i, k].Tiles;
    end;
  UpdateLight(0, 0);
  inc(UndoStep);
  dec(RedoStep);
  if RedoStep = 0 then
    Redo.Enabled := false;
  Undo.Enabled := true;
  BuildMiniMap;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AnyChangesMade then
    CanClose := MessageBox(Form1.Handle,'Any unsaved changes will be lost. Proceed?', 'Warning', MB_ICONWARNING or MB_OKCANCEL{$IFDEF MSWindows} or MB_APPLMODAL{$ENDIF}) = IDOK;
end;

procedure TForm1.LoadReliefFromBMPClick(Sender: TObject);
var bm:TBitmap; i,k:integer;
begin
  if not RunOpenDialog(OpenDialog1, '', '', 'Windows Bitmap files (*.bmp)|*.bmp') then
    exit;
  bm := TBitmap.Create;
  bm.LoadFromFile(OpenDialog1.filename);
  MakeUndoPoint(nil);
  if (Map.Y <> bm.Height) or (Map.X <> bm.Width) then
    ShowMessage('Bitmap size doesn''t match scenery size');
  for i := 1 to min(Map.Y - 1, bm.Height) do
    for k := 1 to min(Map.X - 1, bm.Width) do
      Land2[i, k].Height := round(((bm.Canvas.Pixels[k, i] mod 256) + (bm.Canvas.Pixels[k, i] div 256 mod 256) +
        (bm.Canvas.Pixels[k, i] div 65536)) / 3 / 2.55);
  UpdateLight(0, 0);
  BuildMiniMap;
end;

procedure TForm1.MakeUndoPoint(Sender: TObject);
var i,k,ID:integer;
begin
  AnyChangesMade := true;
  Undo.Enabled := true;
  RedoStep := 0;
  Redo.Enabled := false;
  inc(UndoCounter);
  if UndoStep < MaxUndoSteps then
    inc(UndoStep)
  else
    UndoStep := MaxUndoSteps;

  ID:=(UndoCounter-1) mod MaxUndoSteps + 1; //1..Max

  for i:=1 to Map.Y do for k:=1 to Map.X do begin
  UndoLand[ID,i,k].Terrain:=Land[i,k].Terrain;
  UndoLand[ID,i,k].Height:=round(EnsureRange(Land2[i,k].Height,0,100));
  UndoLand[ID,i,k].Rot:=Land[i,k].Rot;
  UndoLand[ID,i,k].Obj:=Land[i,k].Obj;
  UndoLand[ID,i,k].TerType:=Land2[i,k].TerType;
  UndoLand[ID,i,k].Tiles:=Land2[i,k].Tiles;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
//var i,k,ci:integer;
begin
  {SaveMapButton.Enabled:=true;
  SaveMapMenu.Enabled:=true;
  AllowToSaveMap:=true;
  ci:=0;
  //Renders all objects into two lines
  //for k:=1 to 45 do begin Land[75,k*2].Obj:=ObjIndex[ci+1]; inc(ci); end;
  //for k:=1 to 45 do begin Land[79,k*2].Obj:=ObjIndex[ci+1]; inc(ci); end;
  //Render all tiles into block
  for i:=75 to 90 do for k:=75 to 90 do begin
  Land[i,k].Terrain:=ci; Land2[i,k].Height:=0; Land[i,k].Rot:=0; Land[i,k].Obj:=255; inc(ci); end;}
end;

procedure TForm1.OpenProClick(Sender: TObject);
begin
  if AnyChangesMade then
  if MessageBox(Form1.Handle,'Any unsaved changes will be lost. Proceed?', 'Warning', MB_ICONWARNING or MB_OKCANCEL{$IFDEF MSWindows} or MB_APPLMODAL{$ENDIF}) <> IDOK then exit;

  if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants map project (*.pro)|*.pro') then exit;
  OpenPro(OpenDialog1.FileName);
end;

procedure TForm1.OpenPro(filename:string);
var i,k:integer; Head:record X,Y:integer; end;
begin
  assignfile(f,filename); reset(f,1);
  blockread(f,Head,8);
  if (Head.X>MaxMapSize)or(Head.Y>MaxMapSize) then begin
    closefile(f);
    MessageBox(Form1.Handle,'Too big map or not KaM map.', 'Error', MB_OK);
    exit;
  end;
  reset(f,1);
  blockread(f,Map,8);
  for i:=1 to Map.Y do for k:=1 to Map.X do begin
  blockread(f,Land[i,k].Terrain,1);
  Land[i,k].Light:=16;
  blockread(f,Land[i,k].Height1,1);
  blockread(f,Land[i,k].Rot,1);
  Land[i,k].x3:=0;
  blockread(f,Land[i,k].Obj,1);
  blockread(f,Land[i,k].Pass,1);
  blockread(f,c,3);
  Land[i,k].Pass:=255; //allow anything
  Land[i,k].c1:=255;
  Land[i,k].c2:=255;
  Land[i,k].c3:=255;
  Land[i,k].c4:=255;
  Land[i,k].y1:=0;
  Land[i,k].y2:=0;
  Land[i,k].y3:=0;
  Land[i,k].Border:=255;//
  Land[i,k].y5:=255;
  Land[i,k].y6:=205;
  Land[i,k].y7:=0;
  Land[i,k].y8:=0;
  Land[i,k].y9:=0;
  Land[i,k].y10:=0;
  Land[i,k].y11:=0;
  Land[i,k].y12:=0;
  blockread(f,Land2[i,k].TerType,1);
  blockread(f,c,7);
  end;
  closefile(f);

  ResHead.x1:=0;
  ResHead.Allocated:=Map.X+Map.Y;
  ResHead.Qty1:=0;
  ResHead.Qty2:=ResHead.Qty1;
  if ResHead.Qty1>0 then
  ResHead.x5:=ResHead.Qty1-1
  else
  ResHead.x5:=0;
  ResHead.Len17:=17;

  for i := 1 to ResHead.Allocated do
  begin
    Res[i].x1 := -842150451;
    Res[i].Y1 := -842150451;
    Res[i].X2 := -842150451;
    Res[i].Y2 := -842150451;
    Res[i].Typ := 255;
  end;

  PrepareLoadedMap(filename);
end;

procedure TForm1.PrepareLoadedMap(filename:string);
var i,k:integer; RemWrong:boolean; ErrS:string;
begin
  UndoStep:=1;
  RedoStep:=0;
  UndoCounter:=0;
  Form1.Redo.Enabled:=false;
  Form1.Undo.Enabled:=false;
  BrushMode:=bmNone;
  CopyAreaMenu.Checked:=false;
  PasteAreaMenu.Checked:=false;

  //Reset all mission data
  Mission:=TMission.Create;

  RemWrong:=false;
  for i:=1 to Map.Y do for k:=1 to Map.X do begin
    Land2[i,k].Height:=Land[i,k].Height1;

    if Land[i,k].Obj<>255 then
    if ObjIndexInv[Land[i,k].Obj]=0 then begin
      if not RemWrong then
      begin
        ErrS := 'Wrong object used at '+IntToStr(k)+':'+IntToStr(i)+' it will be removed'+EolW+'Remove all wrong objects silently?';
        if MessageBox(Form1.Handle,@(ErrS[1]),'Warning', mb_yesno)=IDYES then RemWrong:=true;
      end;
      Land[i,k].Obj:=255
    end;
  end;

  UpdateLight(0,0);

  ScrollBar1.Max:=Map.X;
  ScrollBar2.Max:=Map.Y;
  ScrollBar1.Position:=Map.X div 2; //Set view to center of map
  ScrollBar2.Position:=Map.Y div 2; //Set view to center of map
  StatusBar1.Panels.Items[0].Text:='Map size: '+IntToStr(Map.X)+' x '+IntToStr(Map.Y);

  BuildMiniMap;
  ZoomChange(nil);
  RenderResize(nil);
  Form1.Caption:='KaM Editor - '+filename;
end;

procedure TForm1.BBClick(Sender: TObject);
begin
  LandBrush := 0;
  BrushMode := bmNone;
  if not TSpeedButton(Sender).Down then
    exit;
  ActiveTileName := Sender;
  s := (TSpeedButton(Sender)).Name;
  LandBrush := strtoint(s[3] + s[4]);
  BrushMode := bmHouses;
end;

procedure TForm1.SaveMapClick(Sender: TObject);
var i,k:integer;
begin
  if not RunSaveDialog(SaveDialog1,'','','Knights & Merchants map (*.map)|*.map') then exit;

  assignfile(f,SaveDialog1.FileName);
  rewrite(f,1);

  PrepareMapToSave;

  blockwrite(f,Map,8);
  //KaM seems to recompute passability, but there's a bug making some troops disappear on save00.map
  for i:=1 to Map.Y do blockwrite(f,Land[i],Map.X*23);
  blockwrite(f,ResHead,22);
  for i:=1 to ResHead.Allocated do blockwrite(f,Res[i],17);

  blockwrite(f,'ADDN',4);
  blockwrite(f,'TILE',4); //Chunk name
  i:=4+Map.Y*Map.X;
  blockwrite(f,i,4); //Chunk size
  i:=0;
  blockwrite(f,i,4);
  for i:=1 to Map.Y do
  for k:=1 to Map.X do
  blockwrite(f,Land2[i,k].TerType,1);

  closefile(f);

  Form1.Caption:='KaM Editor - '+SaveDialog1.FileName;
end;

procedure TForm1.ExitClick(Sender: TObject);
begin Form1.Close; end;

procedure TForm1.ShowWiresClick(Sender: TObject);
begin ShowWires.Checked:=not ShowWires.Checked; end;

procedure TForm1.ShowObjectsClick(Sender: TObject);
begin ShowObjects.Checked:=not ShowObjects.Checked; end;

procedure TForm1.ShowFlatTerrainClick(Sender: TObject);
begin
ShowFlatTerrain.Checked:= not ShowFlatTerrain.Checked;
xh:=36+integer(ShowFlatTerrain.Checked)*164; // 1/36 .. 1/200
end;

procedure TForm1.CopyAreaMenuClick(Sender: TObject);
begin
LandBrush:=0;
PasteAreaMenu.Checked:=false;
PasteAreaMenu.Enabled:=true;
CopyAreaMenu.Checked:= not CopyAreaMenu.Checked;
if CopyAreaMenu.Checked then BrushMode:=bmCopy else BrushMode:=bmNone;
end;

procedure TForm1.CopyTerrain;
var ii,kk,x1,x2,y1,y2:integer;
begin
if (CopyArea[2,1]=0)or(CopyArea[2,1]=0) then exit;
x1:=min(CopyArea[1,1],CopyArea[2,1]); x2:=max(CopyArea[1,1],CopyArea[2,1]);
y1:=min(CopyArea[1,2],CopyArea[2,2]); y2:=max(CopyArea[1,2],CopyArea[2,2]);
for ii:=y1 to y2+1 do for kk:=x1 to x2+1 do
    begin
    LandCopy[ii-y1+1,kk-x1+1].Terrain:=Land[ii,kk].Terrain;
    LandCopy[ii-y1+1,kk-x1+1].Height1:=round(Land2[ii,kk].Height);
    LandCopy[ii-y1+1,kk-x1+1].Rot:=Land[ii,kk].Rot;
    LandCopy[ii-y1+1,kk-x1+1].Obj:=Land[ii,kk].Obj;
    LandCopy[ii-y1+1,kk-x1+1].TerType:=Land2[ii,kk].TerType;
    end;
end;

procedure TForm1.PasteTerrain;
var ii,kk:integer;
begin
for ii:=MapYc to min(MapYc+abs(CopyArea[2,2]-CopyArea[1,2]),Map.Y-1) do //y..Y
for kk:=MapXc to min(MapXc+abs(CopyArea[2,1]-CopyArea[1,1]),Map.X-1) do //x..X
    begin
    Land [ii,kk].Terrain:=LandCopy[ii-MapYc+1,kk-MapXc+1].Terrain;
    Land2[ii,kk].Height :=LandCopy[ii-MapYc+1,kk-MapXc+1].Height1;
    Land [ii,kk].Rot    :=LandCopy[ii-MapYc+1,kk-MapXc+1].Rot;
    Land [ii,kk].Obj    :=LandCopy[ii-MapYc+1,kk-MapXc+1].Obj;
    Land2[ii,kk].TerType:=LandCopy[ii-MapYc+1,kk-MapXc+1].TerType;
    end;
end;

procedure TForm1.PasteAreaMenuClick(Sender: TObject);
begin
  LandBrush:=0;
  CopyAreaMenu.Checked:=false;
  PasteAreaMenu.Checked:= not PasteAreaMenu.Checked;
  if PasteAreaMenu.Checked then BrushMode:=bmPaste else BrushMode:=bmNone;
end;

procedure TForm1.HousePalleteScrollChange(Sender: TObject);
begin
  HousePallete.TopRow:=HousePalleteScroll.Position;
  Form1.Caption:=IntToStr(HouseID[HousePalleteScroll.Position+1]);
end;

procedure TForm1.HousePalleteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  ImageList2.Draw(HousePallete.Canvas,Rect.Left,Rect.Top,ARow);
end;

procedure TForm1.OpenDATClick(Sender: TObject);
begin
if not RunOpenDialog(OpenDialog1,'','','Knights & Merchants dat (*.dat)|*.dat') then exit;
LoadDAT(OpenDialog1.FileName);
ExportDAT.Enabled:=true;
end;

procedure TForm1.ExportDATClick(Sender: TObject);
begin
  if Mission=nil then begin
    ExportDAT.Enabled:=false;
    exit;
  end;
  if not RunSaveDialog(SaveDialog1,'Mission.txt','','Mission text (*.txt)|*.txt') then exit;
  ExportText(SaveDialog1.FileName);
end;


procedure TForm1.ShowFlowClick(Sender: TObject);
begin
  ShowFlow.Checked:= not ShowFlow.Checked;
end;

{procedure TForm1.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  NewWidth:=max(NewWidth,910);
  NewHeight:=max(NewHeight,718);
end;}

procedure TForm1.Button4Click(Sender: TObject);
var i,k:integer; T:^Byte;
begin

  if SpinEdit2.Value>SpinEdit3.Value then SpinEdit3.Value:=SpinEdit2.Value;

  for i:=1 to Map.Y do for k:=1 to Map.X do begin
    T  := Pointer(cardinal(@Land[i,k].Terrain)+SpinEdit1.Value-1);
    T^ :=SpinEdit2.Value+Random(SpinEdit3.Value-SpinEdit2.Value+1);
  end;
end;

procedure TForm1.UpdateLight(X,Y:integer);
var i,k,y2,x1:integer;
begin
if X*Y=0 then //Update whole map
for i:=1 to Map.Y do for k:=1 to Map.X do begin
  y2:=EnsureRange(i+1,1,Map.Y);
  x1:=EnsureRange(k-1,1,Map.X);
  Land2[i,k].Light:=round(EnsureRange((Land2[i,k].Height-(Land2[y2,k].Height+Land2[i,x1].Height)/2)/22*16+16,0,32));
  if (i=1)or(i=Map.Y)or(k=1)or(k=Map.X) then
    Land2[i,k].Light:=0;
end else begin
  X:=EnsureRange(X,1,Map.X);
  Y:=EnsureRange(Y,1,Map.Y);
  y2:=EnsureRange(Y+1,1,Map.Y);
  x1:=EnsureRange(X-1,1,Map.X);
  Land2[Y,X].Light:=round(EnsureRange((Land2[Y,X].Height-(Land2[y2,X].Height+Land2[Y,x1].Height)/2)/22*16+16,0,32));
end;
end;


procedure TForm1.DoMagicWater(X,Y:integer);
var tiles:array of array of byte; i,k:integer;

  procedure FillArea(x,y:word; ID:byte); //Mode = 1canWalk or 2canWalkRoad
  begin
    if (Land[y,x].Terrain in [126, 127])and(tiles[y,x]=0) then //Shores
      tiles[y,x] := ID+1; //Filled

    if (Land[y,x].Terrain in [192, 196])and(tiles[y,x]=0) then //Water, weeds
    begin
      tiles[y,x] := ID; //Filled

      if x-1>=1 then begin
        if y-1>=1 then     FillArea(x-1,y-1,ID);
                           FillArea(x-1,y  ,ID);
        if y+1<=Map.Y then FillArea(x-1,y+1,ID);
      end;

      if y-1>=1 then     FillArea(x,y-1,ID);
      if y+1<=Map.Y then FillArea(x,y+1,ID);

      if x+1<=Map.X then begin
        if y-1>=1 then     FillArea(x+1,y-1,ID);
                           FillArea(x+1,y  ,ID);
        if y+1<=Map.Y then FillArea(x+1,y+1,ID);
      end;
    end;
  end;
begin
  if MouseButton= 1 then Land[Y,X].Rot:=(Land[Y,X].Rot + 1) mod 4;
  if MouseButton=-1 then if Land[Y,X].Rot<1 then Land[Y,X].Rot:=3 else dec(Land[Y,X].Rot);


  setlength(tiles, map.Y+1);
  for i:=1 to Map.Y do
    setlength(tiles[i], map.X+1);


  if Land[Y,X].Terrain in [192, 196] then begin
    FillArea(x,y,1);

    for i:=1 to Map.Y do for k:=1 to Map.X do begin
      if tiles[i,k] = 1 then
        Land[i,k].Rot := Land[Y,X].Rot;

      //Match direction, else ignore
      if tiles[i,k] = 1+1 then //Shores
        case Land[i,k].Rot of
          0: if Land[Y,X].Rot = 3 then Land[i,k].Terrain := 126 else
             if Land[Y,X].Rot = 1 then Land[i,k].Terrain := 127;

          1: if Land[Y,X].Rot = 0 then Land[i,k].Terrain := 126 else
             if Land[Y,X].Rot = 2 then Land[i,k].Terrain := 127;

          2: if Land[Y,X].Rot = 1 then Land[i,k].Terrain := 126 else
             if Land[Y,X].Rot = 3 then Land[i,k].Terrain := 127;

          3: if Land[Y,X].Rot = 2 then Land[i,k].Terrain := 126 else
             if Land[Y,X].Rot = 0 then Land[i,k].Terrain := 127;
        end;

    end;
  end;

end;


{$IFDEF FPC}
initialization
  {$I KM_Unit1.lrs}
{$ENDIF}


end.
