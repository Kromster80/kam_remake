unit KM_Render;
interface

uses windows, sysutils, Forms, OpenGL, dglOpenGL, KromOGLUtils, KromUtils, math, ExtCtrls, TGATexture;

type
TRender = class
private
  h_DC: HDC;
  h_RC: HGLRC;
  LightPos,LightDiff:array[1..4] of GLfloat;
  Text0,Text1,Text2,Text3,TextG,Text512:GLuint;
  procedure RenderPoint(pX,pY:integer);
  procedure RenderQuad(pX,pY:integer);
  procedure RenderWireQuad(pX,pY:integer);
  procedure RenderTile(Index,pX,pY,Rot:integer);
  procedure RenderSprite(TexID:integer; pX,pY,SizeX,SizeY:single);
  procedure RenderSprite2(TexID:integer; pX,pY,SizeX,SizeY:single);
  procedure RenderRectangle(ElementID:integer; X,Y,SizeX,SizeY:integer);
  procedure Render3DButton(GUITexID:integer; X,Y,SizeX,SizeY:integer);
protected
public
  constructor Create;
  destructor Destroy;
  procedure SetRender(RenderFrame:HWND);
  procedure SetRenderDefaults();
  procedure RenderResize(Width,Height:integer);
  procedure Render();
  procedure RenderToolBar();
  procedure RenderTerrainAndRoads(x1,x2,y1,y2:integer);
  procedure RenderWires();
  procedure RenderObject(Index,AnimStep,pX,pY:integer);
  procedure RenderMarkup(Index:integer; pX,pY:integer);
  procedure RenderBorder(Index:integer; Surround:integer; pX,pY:integer);
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

uses KM_Unit1, KM_Defaults, KM_Terrain, KM_Global_Data, KM_Units, KM_Houses;

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
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
  LoadTexture(ExeDir+'Resource\text0.tga', Text0);    // Load the Textures
  LoadTexture(ExeDir+'Resource\text1.tga', Text1);    // Load the Textures
  LoadTexture(ExeDir+'Resource\text2.tga', Text2);    // Load the Textures
  LoadTexture(ExeDir+'Resource\text3.tga', Text3);    // Load the Textures
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
  LightPos[1]:=-20;
  LightPos[2]:= 40;
  LightPos[3]:= 40;
  LightPos[4]:=  0;
  LightDiff[1]:=   1;
  LightDiff[2]:=   1;
  LightDiff[3]:=0.97;
  LightDiff[4]:=   1;
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
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glTranslate(fViewport.ViewWidth/2,fViewport.ViewHeight/2,0);
  glScale(fViewport.Zoom*CellSize,fViewport.Zoom*CellSize,fViewport.Zoom*CellSize);
  glTranslate(-fViewport.XCoord,-fViewport.YCoord,0);

  glLineWidth(fViewport.Zoom*2);
  glPointSize(fViewport.Zoom*5);

  fTerrain.Paint;
  if Form1.ShowWires.Checked then fRender.RenderWires();
  ControlList.Paint;

//  glLoadIdentity();                // Reset The View
//  RenderToolBar();

  SwapBuffers(h_DC);
end;

procedure TRender.RenderToolBar();
begin
RenderRectangle(407,0,0,GUISize[407,1],GUISize[407,2]);
RenderRectangle(554,0,200,GUISize[554,1],GUISize[554,2]);
RenderRectangle(404,0,200+168,GUISize[404,1],GUISize[404,2]);
RenderRectangle(404,0,200+168+400,GUISize[404,1],GUISize[404,2]);
RenderRectangle(0,8,12,176,176);

Render3DButton(439,12,376,36,36);
Render3DButton(440,58,376,36,36);
Render3DButton(441,104,376,36,36);
Render3DButton(442,150,376,36,36);

Render3DButton(42,68,430,64,40);
end;

procedure TRender.RenderTerrainAndRoads(x1,x2,y1,y2:integer);
var
  i,k:integer; ID,Rot,rd:integer;
  ax,ay:single; xt,a:integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
glEnable(GL_LIGHTING);
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

        glNormal3fv(@Land[i,k].Normal);
        glTexCoord2fv(@TexC[TexO[1]]);
        glvertex2f(k-1,i-1-Land[i,k].Height/xh);

        glNormal3fv(@Land[i+1,k].Normal);
        glTexCoord2fv(@TexC[TexO[2]]);
        glvertex2f(k-1,i-Land[i+1,k].Height/xh);

        glNormal3fv(@Land[i+1,k+1].Normal);
        glTexCoord2fv(@TexC[TexO[3]]);
        glvertex2f(k,i-Land[i+1,k+1].Height/xh);

        glNormal3fv(@Land[i,k+1].Normal);
        glTexCoord2fv(@TexC[TexO[4]]);
        glvertex2f(k,i-1-Land[i,k+1].Height/xh);
      end;
glEnd;

for i:=y1 to y2 do for k:=x1 to x2 do
begin
  if (fTerrain.Land[i,k].TileOwner = 0)
    and(fTerrain.Land[i,k].RoadState in [1..4]) then
      RenderTile(248+fTerrain.Land[i,k].RoadState*2,k,i,0);
  if (fTerrain.Land[i,k].RoadType in [rdt_Road..rdt_Wine])
    and(fTerrain.Land[i,k].TileOwner <> 0)  then
    begin
      rd:=fTerrain.Land[i,k].RoadSurr;
      ID:=RoadsConnectivity[rd,1];
      Rot:=RoadsConnectivity[rd,2];
      RenderTile(ID,k,i,Rot);
    end;
end;

glDisable(GL_LIGHTING);

glColor4f(1,1,1,1);
glBlendFunc(GL_DST_COLOR,GL_ONE);
glBindTexture(GL_TEXTURE_2D, TextG);
glbegin (GL_QUADS);
glNormal3f(0,1,0);
with fTerrain do
for i:=y1 to y2 do for k:=x1 to x2 do
  begin
  glTexCoord2f(0,Land[i  ,k  ].Light); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/xh);
  glTexCoord2f(0,Land[i+1,k  ].Light); glvertex2f(k-1,i  -Land[i+1,k  ].Height/xh);
  glTexCoord2f(0,Land[i+1,k+1].Light); glvertex2f(k  ,i  -Land[i+1,k+1].Height/xh);
  glTexCoord2f(0,Land[i  ,k+1].Light); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/xh);
  end;
glEnd;
glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//Need to add shadow overdraw later

glBindTexture(GL_TEXTURE_2D,0);
end;

procedure TRender.RenderCursorPosition(ActivePage:string);
begin
//==============================================
//-Houses-
//Render house shape
//==============================================
if BrushMode=bm_Houses then
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
var i,k:integer;
begin
glLineWidth(1);
for i:=max(MapYc-10,1) to min(MapYc+10,Map.Y) do begin
glbegin (GL_LINE_STRIP);
for k:=max(MapXc-11,1) to min(MapXc+11,Map.X) do begin
glColor4f(0.8,1,0.6,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/xh);
end;
glEnd;
end;

glPointSize(3);
glbegin (GL_POINTS);
for i:=max(MapYc-10,1) to min(MapYc+10,Map.Y) do
for k:=max(MapXc-10,1) to min(MapXc+10,Map.X) do begin
glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/xh);
end;
glEnd;
//glRasterPos2f(k-1+0.1,i-1-0.1-Land[i,k].Height/xh);
//glColor4f(0.6,1,0.45,0.75);
//glPrint(inttostr(Land[i,k].Height));
//if Land[i,k].Rot and 4=4 then glPrint('X');
//if Land[i,k].Rot and 8=8 then glPrint('Y');
//glPrint(inttostr(Land[i,k].Border));
glLineWidth(fViewPort.Zoom/4);
end;

//
//This piece of code should render all units on screen


procedure TRender.RenderPoint(pX,pY:integer);
begin
glColor4f(0.4,0.3,0,1);
glbegin (GL_POINTS);
glvertex2f(pX-1,pY-1-fTerrain.Land[pY,pX].Height/xh);
glEnd;
end;

procedure TRender.RenderQuad(pX,pY:integer);
begin
glbegin (GL_QUADS);
glNormal3f(0,1,0);
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
glNormal3f(0,1,0);
with fTerrain do begin
  glvertex2f(pX-1,pY-1-Land[pY  ,pX  ].Height/xh);
  glvertex2f(pX  ,pY-1-Land[pY  ,pX+1].Height/xh);
  glvertex2f(pX  ,pY-  Land[pY+1,pX+1].Height/xh);
  glvertex2f(pX-1,pY-  Land[pY+1,pX  ].Height/xh);
end;
glEnd;
end;

procedure TRender.RenderSprite(TexID:integer; pX,pY,SizeX,SizeY:single);
begin
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(0,1); glvertex2f(pX-1      ,pY-1      );
    glTexCoord2f(1,1); glvertex2f(pX-1+SizeX,pY-1      );
    glTexCoord2f(1,0); glvertex2f(pX-1+SizeX,pY-1-SizeY);
    glTexCoord2f(0,0); glvertex2f(pX-1      ,pY-1-SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderSprite2(TexID:integer; pX,pY,SizeX,SizeY:single);
begin
if TexID=0 then exit;
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(0,1); glvertex2f(pX-1      ,pY-1      );
    glTexCoord2f(1,1); glvertex2f(pX-1+SizeX,pY-1      );
    glTexCoord2f(1,0); glvertex2f(pX-1+SizeX,pY-1-SizeY);
    glTexCoord2f(0,0); glvertex2f(pX-1      ,pY-1-SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderTile(Index,pX,pY,Rot:integer);
var xt,k,i,a:integer; ax,ay:single;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
if (pX<1)or(pX>Map.X) then exit;
if (pY<1)or(pY>Map.Y) then exit;

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
  glNormal3fv(@Land[i,k].Normal);
  glTexCoord2fv(@TexC[TexO[1]]);
  glvertex2f(k-1,i-1-Land[i,k].Height/xh);

  glNormal3fv(@Land[i+1,k].Normal);
  glTexCoord2fv(@TexC[TexO[2]]);
  glvertex2f(k-1,i-Land[i+1,k].Height/xh);

  glNormal3fv(@Land[i+1,k+1].Normal);
  glTexCoord2fv(@TexC[TexO[3]]);
  glvertex2f(k,i-Land[i+1,k+1].Height/xh);

  glNormal3fv(@Land[i,k+1].Normal);
  glTexCoord2fv(@TexC[TexO[4]]);
  glvertex2f(k,i-1-Land[i,k+1].Height/xh);
end;
glEnd;
glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.RenderRectangle(ElementID:integer; X,Y,SizeX,SizeY:integer);
var a,b:TKMPointF;
begin
if ElementID=0 then exit;
    a.x:=GUITexUV[ElementID].Left/GUITex[ElementID].TexW;
    a.y:=GUITexUV[ElementID].Top/GUITex[ElementID].TexH;   //texture is flipped upside-down
    b.x:=GUITexUV[ElementID].Right/GUITex[ElementID].TexW;
    b.y:=GUITexUV[ElementID].Bottom/GUITex[ElementID].TexH;

    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, GUITex[ElementID].TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(a.x,a.y); glvertex2f(X      ,Y      );
    glTexCoord2f(b.x,a.y); glvertex2f(X+SizeX,Y      );
    glTexCoord2f(b.x,b.y); glvertex2f(X+SizeX,Y+SizeY);
    glTexCoord2f(a.x,b.y); glvertex2f(X      ,Y+SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.Render3DButton(GUITexID:integer; X,Y,SizeX,SizeY:integer);
var a,b:TKMPointF; Inset,InsetU,InsetV:single;
begin
//402 is a stone background
    a.x:=GUITexUV[402].Left/GUITex[402].TexW;
    a.y:=GUITexUV[402].Top/GUITex[402].TexH;   //texture is flipped upside-down
    b.x:=GUITexUV[402].Right/GUITex[402].TexW;
    b.y:=GUITexUV[402].Bottom/GUITex[402].TexH;

    Inset:=8;
    InsetU:=8/GUITex[402].TexW;
    InsetV:=8/GUITex[402].TexH;

    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, GUITex[402].TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(a.x,a.y); glvertex2f(X      ,Y      );
    glTexCoord2f(b.x,a.y); glvertex2f(X+SizeX,Y      );
    glTexCoord2f(b.x,b.y); glvertex2f(X+SizeX,Y+SizeY);
    glTexCoord2f(a.x,b.y); glvertex2f(X      ,Y+SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);

    RenderRectangle(GUITexID,round(X+(SizeX-GUISize[GUITexID,1])/2),round(Y+(SizeY-GUISize[GUITexID,2])/2),GUISize[GUITexID,1],GUISize[GUITexID,2]);
end;


procedure TRender.RenderObject(Index,AnimStep,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
ID:=MapElem[Index].Step[AnimStep mod MapElem[Index].Count +1]+1;
if Index=61 then begin //Object 42 is an invisible wall
  glBindTexture(GL_TEXTURE_2D,0);
  glColor4f(1,0,0,0.5);
  RenderTile(0,pX,pY,0);
end else begin
  glColor4f(1,1,1,1);
  ShiftX:=TreePivot[ID].x/CellSize;
  ShiftY:=(TreePivot[ID].y+TreeSize[ID,2])/CellSize-fTerrain.Land[pY,pX].Height/xh;
  RenderSprite(TreeTex[ID,1],pX+ShiftX,pY+ShiftY,TreeTex[ID,2]/40,TreeTex[ID,3]/40);
end;
end;

procedure TRender.RenderMarkup(Index:integer; pX,pY:integer);
var a,b:TKMPointF; ID:integer;
begin
case Index of
1: ID:=105; //Road
2: ID:=107; //Field
3: ID:=108; //Wine
end;
  glColor4f(1,1,1,1);
  glBindTexture(GL_TEXTURE_2D,GUITex[ID].TexID);
    a.x:=GUITexUV[ID].Left/GUITex[ID].TexW;
    a.y:=GUITexUV[ID].Bottom/GUITex[ID].TexH;
    b.x:=GUITexUV[ID].Right/GUITex[ID].TexW;
    b.y:=GUITexUV[ID].Top/GUITex[ID].TexH;
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

procedure TRender.RenderBorder(Index:integer; Surround:integer; pX,pY:integer);
var a,b:TKMPointF; ID1,ID2:integer; t:single;
        procedure BindTexID(ID:integer);
        begin
          glBindTexture(GL_TEXTURE_2D,GUITex[ID].TexID);
          a.x:=GUITexUV[ID].Left/GUITex[ID].TexW;
          a.y:=GUITexUV[ID].Bottom/GUITex[ID].TexH;
          b.x:=GUITexUV[ID].Right/GUITex[ID].TexW;
          b.y:=GUITexUV[ID].Top/GUITex[ID].TexH;
          t:=GUITexUV[ID].Right/40; //Width and Height of border
        end;
begin
  case Index of
    7: begin ID1:=105; ID2:=106; end; //Plan (Ropes)
    8: begin ID1:=463; ID2:=467; end; //Build (Fence)
  end;

  glColor4f(1,1,1,1);
    if Surround and 1 = 0 then begin
      BindTexID(ID1);
      glBegin(GL_QUADS);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1 - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1-t - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1-t - fTerrain.Land[pY,pX+1].Height/xh);
      glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1 - fTerrain.Land[pY,pX+1].Height/xh);
      glEnd;
    end;
    if Surround and 2 = 0 then begin
      BindTexID(ID2);
      glBegin(GL_QUADS);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-t/2, pY-1 - fTerrain.Land[pY,pX+1].Height/xh);
      glTexCoord2f(b.x,a.y); glvertex2f(pX+t/2, pY-1 - fTerrain.Land[pY,pX+1].Height/xh);
      glTexCoord2f(b.x,b.y); glvertex2f(pX+t/2, pY   - fTerrain.Land[pY+1,pX+1].Height/xh);
      glTexCoord2f(a.x,b.y); glvertex2f(pX-t/2, pY   - fTerrain.Land[pY+1,pX+1].Height/xh);
      glEnd;
    end;
    if Surround and 8 = 0 then begin
      BindTexID(ID2);
      glBegin(GL_QUADS);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
      glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
      glEnd;
    end;
    if Surround and 4 = 0 then begin
      BindTexID(ID1);
      glBegin(GL_QUADS);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY - fTerrain.Land[pY+1,pX].Height/xh);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-t - fTerrain.Land[pY+1,pX].Height/xh);
      glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-t - fTerrain.Land[pY+1,pX+1].Height/xh);
      glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY - fTerrain.Land[pY+1,pX+1].Height/xh);
      glEnd;
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
          ShiftX:=HousePivot[ID].x/CellSize;
          ShiftY:=(HousePivot[ID].y+HouseSize[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
          ShiftX:=ShiftX+HouseDAT[Index].Anim[AnimType].MoveX/40;
          ShiftY:=ShiftY+HouseDAT[Index].Anim[AnimType].MoveY/40;
          RenderSprite(HouseTex[ID,1],pX+ShiftX,pY+ShiftY,HouseTex[ID,2]/40,HouseTex[ID,3]/40);
          glColor4ubv(@TeamColors[Owner]);
          RenderSprite2(HouseTex[ID,4],pX+ShiftX,pY+ShiftY,HouseTex[ID,2]/40,HouseTex[ID,3]/40);
        end;
    end;
  end;
end;

procedure TRender.RenderHouse(Index,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
    //Render base
    ID:=HouseDAT[Index].Stone+1;
    ShiftX:=HousePivot[ID].x/CellSize;
    ShiftY:=(HousePivot[ID].y+HouseSize[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(HouseTex[ID,1],pX+ShiftX,pY+ShiftY,HouseTex[ID,2]/40,HouseTex[ID,3]/40);
end;

procedure TRender.RenderHouseBuild(Index,Mode,Step,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
if Mode=1 then begin
    ID:=Index+250;
    ShiftX:=GUIPivot[ID].x/CellSize;
    ShiftY:=(GUIPivot[ID].y+GUISize[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(GUITex[ID].TexID,pX+ShiftX,pY+ShiftY,GUITex[ID].TexW/40,GUITex[ID].TexH/40);
end;
end;

procedure TRender.RenderHouseSupply(Index:integer; R1,R2:array of byte; pX,pY:integer);
var ShiftX,ShiftY:single; ID,i:integer;
begin
for i:=1 to 4 do if (R1[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyIn[i,min(R1[i-1],5)]+1;
    if ID>0 then begin    
    ShiftX:=HousePivot[ID].x/CellSize;
    ShiftY:=(HousePivot[ID].y+HouseSize[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(HouseTex[ID,1],pX+ShiftX,pY+ShiftY,HouseTex[ID,2]/40,HouseTex[ID,3]/40);
    end;
    end;
for i:=1 to 4 do if (R2[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyOut[i,min(R2[i-1],5)]+1;
    if ID>0 then begin
    ShiftX:=HousePivot[ID].x/CellSize;
    ShiftY:=(HousePivot[ID].y+HouseSize[ID,2])/CellSize-fTerrain.Land[pY+1,pX].Height/xh;
    RenderSprite(HouseTex[ID,1],pX+ShiftX,pY+ShiftY,HouseTex[ID,2]/40,HouseTex[ID,3]/40);
    end;
    end;
end;

procedure TRender.RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Count;
ID:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=UnitPivot[ID].x/CellSize;
  ShiftY:=(UnitPivot[ID].y+UnitSize[ID,2])/CellSize-fTerrain.Land[round(pY)+1,round(pX)].Height/xh-0.25;
  RenderSprite(UnitTex[ID,1],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
  glColor4ubv(@TeamColors[Owner]);
  RenderSprite2(UnitTex[ID,4],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
end;

procedure TRender.RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitCarry[CarryID].Dir[DirID].Count;
ID:=UnitCarry[CarryID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=UnitPivot[ID].x/CellSize;
  ShiftY:=(UnitPivot[ID].y+UnitSize[ID,2])/CellSize-fTerrain.Land[round(pY)+1,round(pX)].Height/xh-0.25;
  ShiftX:=ShiftX+UnitCarry[CarryID].Dir[DirID].MoveX/40;
  ShiftY:=ShiftY+UnitCarry[CarryID].Dir[DirID].MoveY/40;
  RenderSprite(UnitTex[ID,1],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
  glColor4ubv(@TeamColors[Owner]);
  RenderSprite2(UnitTex[ID,4],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
end;



end.
