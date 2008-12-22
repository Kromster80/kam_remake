unit KM_Render;
interface
uses windows, sysutils, Forms, OpenGL, dglOpenGL, KromOGLUtils, KromUtils, math, ExtCtrls, TGATexture, KM_Defaults;

type
TRender = class
private
  h_DC: HDC;
  h_RC: HGLRC;
  TextG,Text512:GLuint;
  procedure RenderQuad(pX,pY:integer);
  procedure RenderWireQuad(pX,pY:integer);
  procedure RenderTile(Index,pX,pY,Rot:integer);
  procedure RenderDot(pX,pY:single);
  procedure RenderSprite(RX,ID:integer; pX,pY:single);
  procedure RenderSprite2(RX,ID:integer; pX,pY:single);
protected
public
  constructor Create;
  destructor Destroy;
  procedure SetRender(RenderFrame:HWND);
  procedure SetRenderDefaults();
  procedure RenderResize(Width,Height:integer);
  procedure Render();
  procedure RenderTerrainAndFields(x1,x2,y1,y2:integer);
  procedure RenderWires();
  procedure RenderObject(Index,AnimStep,pX,pY:integer);
  procedure RenderObjectSpecial(Fs:TFieldSpecial; AnimStep,pX,pY:integer);
  procedure RenderMarkup(Index:integer; pX,pY:integer);
  procedure RenderBorder(Border:TBorderType; Dir:integer; pX,pY:integer);
  procedure RenderCursorPosition(ActivePage:string);
  procedure RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single);
  procedure RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
  procedure RenderHouse(Index,pX,pY:integer);
  procedure RenderHouseBuild(Index,Mode,Step,pX,pY:integer);
  procedure RenderHouseSupply(Index:integer; R1,R2:array of byte; pX,pY:integer);
  procedure RenderHouseWork(Index,AnimType,AnimStep,Owner,pX,pY:integer);
published
end;

implementation

uses KM_Unit1, KM_Terrain, KM_Global_Data, KM_Units, KM_Houses;

procedure TRender.SetRender(RenderFrame:HWND);
begin
  InitOpenGL;
  h_DC := GetDC(RenderFrame);
  if h_DC=0 then
  begin
    MessageBox(Form1.Handle, 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  if not SetDCPixelFormat(h_DC) then
    exit;
  h_RC := wglCreateContext(h_DC);
  if h_RC=0 then
  begin
    MessageBox(Form1.Handle, 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  if not wglMakeCurrent(h_DC, h_RC) then
  begin
    MessageBox(Form1.Handle, 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  ReadExtensions;
  ReadImplementationProperties;
end;

procedure TRender.SetRenderDefaults();
begin
  glClearColor(0, 0, 0, 0); 	   //Background
  glClear (GL_COLOR_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);                 //Enables Smooth Color Shading
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
  LoadTexture(ExeDir+'Resource\gradient.tga', TextG);    // Load the Textures
  LoadTexture(ExeDir+'Resource\Tiles512.tga', Text512);    // Load the Textures
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glPolygonMode(GL_FRONT,GL_FILL);
  BuildFont(h_DC,16);
end;

procedure TRender.RenderResize(Width,Height:integer);
begin
  if Height=0 then Height:=1;
  if Width=0  then Width :=1;        // Panel1.Height/Panel1.Width
  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity();                   // Reset View
  //Half a map into each direction
  gluOrtho2D(0,Width,Height,0);
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View
end;

constructor TRender.Create;
begin
{  LightPos[1]:=-20;
  LightPos[2]:= 40;
  LightPos[3]:= 40;
  LightPos[4]:=  0;
  LightDiff[1]:=   1;
  LightDiff[2]:=   1;
  LightDiff[3]:=0.97;
  LightDiff[4]:=   1;}
end;

destructor TRender.Destroy;
begin
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
end;

procedure TRender.Render();
begin
  glClear(GL_COLOR_BUFFER_BIT);    // Clear The Screen
  glLoadIdentity();                // Reset The View
  glTranslate(fViewport.ViewWidth/2,fViewport.ViewHeight/2,0);
  glScale(fViewport.Zoom*CellSize,fViewport.Zoom*CellSize,fViewport.Zoom*CellSize);
  glTranslate(-fViewport.XCoord,-fViewport.YCoord,0);

  glLineWidth(fViewport.Zoom*2);
  glPointSize(fViewport.Zoom*5);

  fTerrain.Paint;
  if Form1.ShowWires.Checked then fRender.RenderWires();
  ControlList.Paint;

  glLoadIdentity();                // Reset The View

  fControls.Paint;

  SwapBuffers(h_DC);
end;

procedure TRender.RenderTerrainAndFields(x1,x2,y1,y2:integer);
var
  i,k:integer; ID,Rot,rd:integer;
  ax,ay:single; xt,a:integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
glColor4f(1,1,1,1);
glBindTexture(GL_TEXTURE_2D, Text512);
glbegin (GL_QUADS);
  with fTerrain do
    for i:=y1 to y2 do
      for k:=x1 to x2 do begin
        xt:=fTerrain.Land[i,k].Terrain;
        ax:=((xt div 64) mod 2) /2;
        ay:=(xt div 128) /2;

        TexC[1,1]:=(xt mod 8  )/16+ax+Overlap; TexC[1,2]:=1-(xt mod 64 div 8  )/16+ay-Overlap;
        TexC[2,1]:=(xt mod 8  )/16+ax+Overlap; TexC[2,2]:=1-(xt mod 64 div 8+1)/16+ay+Overlap;
        TexC[3,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[3,2]:=1-(xt mod 64 div 8+1)/16+ay+Overlap;
        TexC[4,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[4,2]:=1-(xt mod 64 div 8  )/16+ay-Overlap;
        TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

        if fTerrain.Land[i,k].Rotation and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
        if fTerrain.Land[i,k].Rotation and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

        glTexCoord2fv(@TexC[TexO[1]]);
        glvertex2f(k-1,i-1-Land[i,k].Height/xh);

        glTexCoord2fv(@TexC[TexO[2]]);
        glvertex2f(k-1,i-Land[i+1,k].Height/xh);

        glTexCoord2fv(@TexC[TexO[3]]);
        glvertex2f(k,i-Land[i+1,k+1].Height/xh);

        glTexCoord2fv(@TexC[TexO[4]]);
        glvertex2f(k,i-1-Land[i,k+1].Height/xh);
      end;
      glEnd;

      for i:=y1 to y2 do for k:=x1 to x2 do
      begin
        case fTerrain.Land[i,k].FieldSpecial of
          fs_Dig1: RenderTile(250,k,i,0);
          fs_Dig2: RenderTile(252,k,i,0);
          fs_Dig3: RenderTile(254,k,i,0);
          fs_Dig4: RenderTile(256,k,i,0);
        end;

        if fTerrain.Land[i,k].FieldType = fdt_Road then
          begin
            rd:=byte(fTerrain.Land[max(i-1,1),k                  ].FieldType = fdt_Road)*1 +
                byte(fTerrain.Land[i         ,min(k+1,MaxMapSize)].FieldType = fdt_Road)*2 +
                byte(fTerrain.Land[max(i+1,1),k                  ].FieldType = fdt_Road)*4 +
                byte(fTerrain.Land[i         ,min(k-1,MaxMapSize)].FieldType = fdt_Road)*8;
            ID:=RoadsConnectivity[rd,1];
            Rot:=RoadsConnectivity[rd,2];
            RenderTile(ID,k,i,Rot);
          end;
      end;

  glColor4f(1,1,1,1);
  glBlendFunc(GL_DST_COLOR,GL_ONE);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glbegin (GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    begin
      glTexCoord2f(0,max(0,Land[i  ,k  ].Light)); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/xh);
      glTexCoord2f(0,max(0,Land[i+1,k  ].Light)); glvertex2f(k-1,i  -Land[i+1,k  ].Height/xh);
      glTexCoord2f(0,max(0,Land[i+1,k+1].Light)); glvertex2f(k  ,i  -Land[i+1,k+1].Height/xh);
      glTexCoord2f(0,max(0,Land[i  ,k+1].Light)); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/xh);
    end;
  glEnd;

  glBlendFunc(GL_ZERO,GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glbegin (GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    begin
    glTexCoord2f(0,max(0,-Land[i  ,k  ].Light)); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/xh);
    glTexCoord2f(0,max(0,-Land[i+1,k  ].Light)); glvertex2f(k-1,i  -Land[i+1,k  ].Height/xh);
    glTexCoord2f(0,max(0,-Land[i+1,k+1].Light)); glvertex2f(k  ,i  -Land[i+1,k+1].Height/xh);
    glTexCoord2f(0,max(0,-Land[i  ,k+1].Light)); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/xh);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glBindTexture(GL_TEXTURE_2D,0);
end;

procedure TRender.RenderCursorPosition(ActivePage:string);
begin
//==============================================
//-Houses-
//Render house shape
//==============================================
if CursorMode=cm_Houses then
if (LandBrush in [0])and(not MousePressed) then
  begin
  glColor4f(1,0,0,0.2);    //Object eraser
  RenderQuad(MapXc,MapYc);
  end else
if LandBrush in [1..29] then
  begin
//  RenderHouse(LandBrush,1,0,MapXc,MapYc);
  end else
if LandBrush in [97..99] then
  begin
  RenderTile(255,MapXc,MapYc,0);
  glBindTexture(GL_TEXTURE_2D, 0);
  end;

glPointSize(1);
glLineWidth(1);
end;

procedure TRender.RenderWires();
var i,k:integer; x1,x2,y1,y2:integer;
begin
x1:=max(MapXc-11,1); x2:=min(MapXc+11,fTerrain.MapX);
y1:=max(MapYc-10,1); y2:=min(MapYc+10,fTerrain.MapY);

glLineWidth(1);
for i:=y1 to y2 do begin
  glbegin (GL_LINE_STRIP);
  for k:=x1 to x2 do begin
    glColor4f(0.8,1,0.6,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10); //Smooth circle gradient blending
    glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/xh);
  end;
  glEnd;
end;

glPointSize(3);
glbegin (GL_POINTS);
for i:=y1 to y2 do
for k:=x1 to x2 do begin
//  glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
  glColor4f(byte(fTerrain.Land[i,k].BorderX=bt_HousePlan),byte(fTerrain.Land[i,k].BorderY=bt_HousePlan),0,1);
  glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/xh);
end;
glEnd;

(*glRasterPos2f(k-1+0.1,i-1-0.1-Land[i,k].Height/xh);
glColor4f(0.6,1,0.45,0.75);
glPrint(inttostr(Land[i,k].Height));
glPrint(inttostr(Land[i,k].Border));*)

glLineWidth(fViewPort.Zoom/4);
end;


procedure TRender.RenderQuad(pX,pY:integer);
begin
glbegin (GL_QUADS);
with fTerrain do begin
  glvertex2f(pX-1,pY-1-Land[pY  ,pX  ].Height/xh);
  glvertex2f(pX  ,pY-1-Land[pY  ,pX+1].Height/xh);
  glvertex2f(pX  ,pY-  Land[pY+1,pX+1].Height/xh);
  glvertex2f(pX-1,pY-  Land[pY+1,pX  ].Height/xh);
end;
glEnd;
end;


procedure TRender.RenderWireQuad(pX,pY:integer);
begin
  glbegin (GL_LINE_LOOP);
  with fTerrain do begin
    glvertex2f(pX-1,pY-1-Land[pY  ,pX  ].Height/xh);
    glvertex2f(pX  ,pY-1-Land[pY  ,pX+1].Height/xh);
    glvertex2f(pX  ,pY-  Land[pY+1,pX+1].Height/xh);
    glvertex2f(pX-1,pY-  Land[pY+1,pX  ].Height/xh);
  end;
  glEnd;
end;

procedure TRender.RenderDot(pX,pY:single);
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  glBegin (GL_QUADS);
    glvertex2f(pX-1    ,pY-1    );
    glvertex2f(pX-1+0.1,pY-1    );
    glvertex2f(pX-1+0.1,pY-1-0.1);
    glvertex2f(pX-1    ,pY-1-0.1);
  glEnd;
end;

procedure TRender.RenderSprite(RX,ID:integer; pX,pY:single);
begin
  with GFXData[RX,ID] do begin
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(u1,v2); glvertex2f(pX-1                 ,pY-1         );
    glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CellSize,pY-1         );
    glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CellSize,pY-1-pxHeight/CellSize);
    glTexCoord2f(u1,v1); glvertex2f(pX-1                 ,pY-1-pxHeight/CellSize);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
    {glLineWidth(1);
    glBegin (GL_LINE_LOOP);
    glvertex2f(pX-1                 ,pY-1         );
    glvertex2f(pX-1+pxWidth/CellSize,pY-1         );
    glvertex2f(pX-1+pxWidth/CellSize,pY-1-pxHeight/CellSize);
    glvertex2f(pX-1                 ,pY-1-pxHeight/CellSize);
    glEnd;}
  end;
end;

procedure TRender.RenderSprite2(RX,ID:integer; pX,pY:single);
begin
  with GFXData[RX,ID] do begin
    glBindTexture(GL_TEXTURE_2D, AltID);
    glBegin (GL_QUADS);
    glTexCoord2f(u1,v2); glvertex2f(pX-1                 ,pY-1         );
    glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CellSize,pY-1         );
    glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CellSize,pY-1-pxHeight/CellSize);
    glTexCoord2f(u1,v1); glvertex2f(pX-1                 ,pY-1-pxHeight/CellSize);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

procedure TRender.RenderTile(Index,pX,pY,Rot:integer);
var xt,k,i,a:integer; ax,ay:single;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
if (pX<1)or(pX>fTerrain.MapX) then exit;
if (pY<1)or(pY>fTerrain.MapY) then exit;

if not InRange(Index,1,256) then Assert(false,'Wrong tile index');

glColor4f(1,1,1,1);
glBindTexture(GL_TEXTURE_2D, Text512);
xt:=Index-1;

ax:=((xt div 64) mod 2) /2;
ay:=(xt div 128) /2;

TexC[1,1]:=(xt mod 8  )/16+ax+Overlap; TexC[1,2]:=1-(xt mod 64 div 8  )/16+ay-Overlap;
TexC[2,1]:=(xt mod 8  )/16+ax+Overlap; TexC[2,2]:=1-(xt mod 64 div 8+1)/16+ay+Overlap;
TexC[3,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[3,2]:=1-(xt mod 64 div 8+1)/16+ay+Overlap;
TexC[4,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[4,2]:=1-(xt mod 64 div 8  )/16+ay-Overlap;
TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

k:=pX; i:=pY;
glbegin (GL_QUADS);
with fTerrain do begin
  glTexCoord2fv(@TexC[TexO[1]]);
  glvertex2f(k-1,i-1-Land[i,k].Height/xh);

  glTexCoord2fv(@TexC[TexO[2]]);
  glvertex2f(k-1,i-Land[i+1,k].Height/xh);

  glTexCoord2fv(@TexC[TexO[3]]);
  glvertex2f(k,i-Land[i+1,k+1].Height/xh);

  glTexCoord2fv(@TexC[TexO[4]]);
  glvertex2f(k,i-1-Land[i,k+1].Height/xh);
end;
glEnd;
glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderObject(Index,AnimStep,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
if MapElem[Index].Count=0 then exit;
ID:=MapElem[Index].Step[AnimStep mod MapElem[Index].Count +1]+1;
if ID<=0 then exit;
if Index=61 then begin //Object 42 is an invisible wall
  glBindTexture(GL_TEXTURE_2D,0);
  glColor4f(1,0,0,1);
  RenderDot(pX,pY);
end else begin
  ShiftX:=RXData[1].Pivot[ID].x/CellSize;
  ShiftY:=(RXData[1].Pivot[ID].y+RXData[1].Size[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh;
  RenderSprite(1,ID,pX+ShiftX,pY+ShiftY);
  {RenderDot(pX,pY);
  glRasterPos2f(pX-1+0.1,pY-1+0.1);
  glPrint(inttostr(Index)+':'+inttostr(ID));}
end;
end;

procedure TRender.RenderObjectSpecial(Fs:TFieldSpecial; AnimStep,pX,pY:integer);
var ShiftX,ShiftY:single; ID,Index:integer;
begin
  glColor4f(1,1,1,1);
  case Fs of
  fs_Corn1: Index:=59;
  fs_Corn2: Index:=60;
  else exit;
  end;
  ID:=MapElem[Index].Step[AnimStep mod MapElem[Index].Count +1]+1;
  ShiftX:=0;
  ShiftY:=(0+RXData[1].Size[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh-0.4;
  RenderSprite(1,ID,pX+ShiftX,pY+ShiftY);
  ID:=MapElem[Index].Step[(AnimStep+1) mod MapElem[Index].Count +1]+1;
  ShiftX:=0.5;
  ShiftY:=(0+RXData[1].Size[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh-0.4;
  RenderSprite(1,ID,pX+ShiftX,pY+ShiftY);
  ID:=MapElem[Index].Step[(AnimStep+1) mod MapElem[Index].Count +1]+1;
  ShiftX:=0;
  ShiftY:=(0+RXData[1].Size[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh+0.1;
  RenderSprite(1,ID,pX+ShiftX,pY+ShiftY);
  ID:=MapElem[Index].Step[(AnimStep) mod MapElem[Index].Count +1]+1;
  ShiftX:=0.5;
  ShiftY:=(0+RXData[1].Size[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh+0.1;
  RenderSprite(1,ID,pX+ShiftX,pY+ShiftY);
end;

procedure TRender.RenderMarkup(Index:integer; pX,pY:integer);
var a,b:TKMPointF; ID:integer;
begin
case Index of
1: ID:=105; //Road
2: ID:=107; //Field
3: ID:=108; //Wine
else ID:=0;
end;
  glColor4f(1,1,1,1);
  glBindTexture(GL_TEXTURE_2D,GFXData[4,ID].TexID);

  a.x:=GFXData[4,ID].u1; a.y:=GFXData[4,ID].v1;
  b.x:=GFXData[4,ID].u2; b.y:=GFXData[4,ID].v2;

  glBegin(GL_QUADS);
    glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1 - fTerrain.Land[pY,pX].Height/xh);
    glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1 - fTerrain.Land[pY,pX].Height/xh-0.25);
    glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY   - fTerrain.Land[pY+1,pX+1].Height/xh-0.25);
    glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY   - fTerrain.Land[pY+1,pX+1].Height/xh);

    glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY   - fTerrain.Land[pY+1,pX].Height/xh);
    glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY   - fTerrain.Land[pY+1,pX].Height/xh-0.25);
    glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1 - fTerrain.Land[pY,pX+1].Height/xh-0.25);
    glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1 - fTerrain.Land[pY,pX+1].Height/xh);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderBorder(Border:TBorderType; Dir:integer; pX,pY:integer);
var a,b:TKMPointF; ID1,ID2:integer; t:single; HeightInPx:integer;
begin
  ID1:=0; ID2:=0;
  if bt_HouseBuilding = Border then
    if Dir=1 then ID1:=463 else ID2:=467; //WIP (Wood planks)
  if bt_HousePlan = Border then
    if Dir=1 then ID1:=105 else ID2:=117; //Plan (Ropes)
  if bt_Wine = Border then
    if Dir=1 then ID1:=462 else ID2:=466; //Fence (Wood)
  if bt_Field = Border then
    if Dir=1 then ID1:=461 else ID2:=465; //Fence (Stones)

  glColor4f(1,1,1,1);
    if Dir = 1 then begin //Horizontal border
      glBindTexture(GL_TEXTURE_2D,GFXData[4,ID1].TexID);
      a.x:=GFXData[4,ID1].u1; a.y:=GFXData[4,ID1].v1;
      b.x:=GFXData[4,ID1].u2; b.y:=GFXData[4,ID1].v2;
      t:=GFXData[4,ID1].PxWidth/CellSize; //Height of border
      glBegin(GL_QUADS);
        glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1+t/2 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1-t/2 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1-t/2 - fTerrain.Land[pY,pX+1].Height/xh);
        glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1+t/2 - fTerrain.Land[pY,pX+1].Height/xh);
      glEnd;
    end;
    if Dir = 2 then begin //Vertical border
      glBindTexture(GL_TEXTURE_2D,GFXData[4,ID2].TexID);
      HeightInPx:= Round ( CellSize * (1 + fTerrain.Land[pY,pX].Height/xh - fTerrain.Land[pY+1,pX].Height/xh) );
      a.x:=GFXData[4,ID2].u1; a.y:=GFXData[4,ID2].v1;
      b.x:=GFXData[4,ID2].u2; b.y:=GFXData[4,ID2].v2 * (HeightInPx / GFXData[4,ID2].PxHeight);
      t:=GFXData[4,ID2].PxWidth/CellSize; //Width of border
      glBegin(GL_QUADS);
        glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
        glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
      glEnd;
{      glBindTexture(GL_TEXTURE_2D, 0);
      glBegin(GL_LINE_LOOP);
        glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
        glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
        glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
      glEnd;}
    end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderHouseWork(Index,AnimType,AnimStep,Owner,pX,pY:integer);
var ShiftX,ShiftY:single; ID,AnimCount:integer; i:integer; Arr:array[0..24]of integer;
begin
  if AnimType<>0 then
  begin
  ConvertSetToArray(AnimType, @Arr);
  for i:=1 to Arr[0] do
    begin
      AnimType:=Arr[i];
      AnimCount:=HouseDAT[Index].Anim[AnimType].Count;
      if AnimCount<>0 then
        begin
          ID:=HouseDAT[Index].Anim[AnimType].Step[AnimStep mod AnimCount + 1]+1;
          ShiftX:=RXData[2].Pivot[ID].x/CellSize;
          ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
          ShiftX:=ShiftX+HouseDAT[Index].Anim[AnimType].MoveX/40;
          ShiftY:=ShiftY+HouseDAT[Index].Anim[AnimType].MoveY/40;
          RenderSprite(2,ID,pX+ShiftX,pY+ShiftY);
          glColor4ubv(@TeamColors[Owner]);
          if GFXData[2,ID].AltID<>0 then //Mill rotor, etc
          RenderSprite2(2,ID,pX+ShiftX,pY+ShiftY);
        end;
    end;
  end;
end;

procedure TRender.RenderHouse(Index,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
    //Render base
    ID:=HouseDAT[Index].Stone+1;
    ShiftX:=RXData[2].Pivot[ID].x/CellSize;
    ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(2,ID,pX+ShiftX,pY+ShiftY);
end;

procedure TRender.RenderHouseBuild(Index,Mode,Step,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
if Mode=1 then begin
    ID:=Index+250;
    ShiftX:=RXData[4].Pivot[ID].x/CellSize-0.5;
    ShiftY:=(RXData[4].Pivot[ID].y+RXData[4].Size[ID,2])/CellSize+0.25-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(4,ID,pX+ShiftX,pY+ShiftY);
end;
end;

procedure TRender.RenderHouseSupply(Index:integer; R1,R2:array of byte; pX,pY:integer);
var ShiftX,ShiftY:single; ID,i:integer;
begin
for i:=1 to 4 do if (R1[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyIn[i,min(R1[i-1],5)]+1;
    if ID>0 then begin
    ShiftX:=RXData[2].Pivot[ID].x/CellSize;
    ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(2,ID,pX+ShiftX,pY+ShiftY);
    end;
    end;
for i:=1 to 4 do if (R2[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyOut[i,min(R2[i-1],5)]+1;
    if ID>0 then begin
    ShiftX:=RXData[2].Pivot[ID].x/CellSize;
    ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(2,ID,pX+ShiftX,pY+ShiftY);
    end;
    end;
end;

procedure TRender.RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Count;
ID:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=RXData[3].Pivot[ID].x/CellSize;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID,2])/CellSize;

  ShiftY:=ShiftY-fTerrain.InterpolateMapCoord(pX,pY)/xh-0.4;

  RenderSprite(3,ID,pX+ShiftX,pY+ShiftY);
  glColor4ubv(@TeamColors[Owner]);
  if GFXData[3,ID].AltID<>0 then
  RenderSprite2(3,ID,pX+ShiftX,pY+ShiftY);

  glColor3ubv(@TeamColors[Owner]);  //Render dot where unit is
  RenderDot(pX,pY-fTerrain.InterpolateMapCoord(pX,pY)/xh);
end;

procedure TRender.RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitCarry[CarryID].Dir[DirID].Count;
ID:=UnitCarry[CarryID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=RXData[3].Pivot[ID].x/CellSize;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID,2])/CellSize;
  ShiftY:=ShiftY-fTerrain.InterpolateMapCoord(pX,pY)/xh-0.4;
  ShiftX:=ShiftX+UnitCarry[CarryID].Dir[DirID].MoveX/40;
  ShiftY:=ShiftY+UnitCarry[CarryID].Dir[DirID].MoveY/40;
  RenderSprite(3,ID,pX+ShiftX,pY+ShiftY);
  glColor4ubv(@TeamColors[Owner]);
  if GFXData[3,ID].AltID<>0 then
  RenderSprite2(3,ID,pX+ShiftX,pY+ShiftY);
end;



end.
