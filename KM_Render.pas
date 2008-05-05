unit KM_Render;
interface

uses windows, sysutils, Forms, OpenGL, dglOpenGL, KromOGLUtils, KromUtils, math, ExtCtrls, TGATexture;

type
TRender = class
private
  h_DC: HDC;
  h_RC: HGLRC;
  LightPos,LightDiff:array[1..4] of GLfloat;
  Text0,Text1,Text2,Text3,TextG:GLuint;
  procedure RenderPoint(pX,pY:integer);
  procedure RenderQuad(pX,pY:integer);
  procedure RenderWireQuad(pX,pY:integer);
  procedure RenderTile(Index,pX,pY,Rot:integer);
  procedure RenderSprite(TexID:integer; pX,pY,SizeX,SizeY:single);
  procedure RenderRectangle(TexID:integer; X,Y,SizeX,SizeY:integer);
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
  procedure RenderTerrainAndRoads();
  procedure RenderWires();
  procedure RenderObject(Index,AnimStep,pX,pY:integer);
  procedure RenderCursorPosition(ActivePage:string);
  procedure RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single);
  procedure RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
  procedure RenderHouse(Index,pX,pY:integer);
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

  glLoadIdentity();                // Reset The View
  RenderToolBar();

  SwapBuffers(h_DC);
end;

procedure TRender.RenderToolBar();
begin
RenderRectangle(GUITex[407,1],0,0,GUITex[407,2],GUITex[407,3]);
RenderRectangle(GUITex[554,1],0,200,GUITex[554,2],GUITex[554,3]);
RenderRectangle(GUITex[404,1],0,200+168,GUITex[404,2],GUITex[404,3]);
RenderRectangle(GUITex[404,1],0,200+168+400,GUITex[404,2],GUITex[404,3]);
RenderRectangle(0,8,12,176,176);

Render3DButton(439,12,376,40,40);
end;

procedure TRender.RenderTerrainAndRoads();
var
  i,k,x0,x1,x2,y1,y2,y3:integer; ID,Rot,rd:integer;
begin
  glEnable(GL_LIGHTING);

x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

//i,k get clipped into map space inside RenderTile()
  for i:=y1 to y2 do
    for k:=x1 to x2 do
      RenderTile(fTerrain.Land[i,k].Terrain+1,k,i,fTerrain.Land[i,k].Rotation);

if Mission<>nil then
for i:=y1 to y2 do for k:=x1 to x2 do
with Mission do
if Roads[k,i] then
  begin
    rd:=0;
    if (Roads[k                  ,max(i-1,1)         ]) then inc(rd,1);  //   1
    if (Roads[min(k+1,MaxMapSize),i                  ]) then inc(rd,2);  //  8*2
    if (Roads[k                  ,min(i+1,MaxMapSize)]) then inc(rd,4);  //   4
    if (Roads[max(k-1,1)         ,i                  ]) then inc(rd,8);  //Take preset from table
    ID:=RoadsConnectivity[rd,1];
    Rot:=RoadsConnectivity[rd,2];
  RenderTile(ID,k,i,Rot);
  end;

glColor4f(1,1,1,1);
glBlendFunc(GL_DST_COLOR,GL_ONE);
glBindTexture(GL_TEXTURE_2D, TextG);
glbegin (GL_QUADS);
for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain do
  begin
  x0:=max(k-1,1); y3:=min(i+2,Map.Y);
  glNormal3f(0,1,0); //height difference / 35 seems to be about perfect
  glTexCoord2f(0,min(max(Land[i  ,k  ].Height-Land[i+1,x0 ].Height,0)/35,0.98)); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/xh);
  glTexCoord2f(0,min(max(Land[i+1,k  ].Height-Land[y3 ,x0 ].Height,0)/35,0.98)); glvertex2f(k-1,i  -Land[i+1,k  ].Height/xh);
  glTexCoord2f(0,min(max(Land[i+1,k+1].Height-Land[y3 ,k  ].Height,0)/35,0.98)); glvertex2f(k  ,i  -Land[i+1,k+1].Height/xh);
  glTexCoord2f(0,min(max(Land[i  ,k+1].Height-Land[i+1,k  ].Height,0)/35,0.98)); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/xh);
  end;
glEnd;
glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//Need to add shadow overdraw yet

{if Form1.CB1.Checked then
for i:=y1 to y2 do for k:=x1 to x2 do begin
glColor4f(1,1,1,(Land[i,k].y1)/255);
RenderQuad(k,i);
end;  }
glBindTexture(GL_TEXTURE_2D,0);
glDisable(GL_LIGHTING);
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

procedure TRender.RenderTile(Index,pX,pY,Rot:integer);
var xt,k,i,a:integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
if (pX<1)or(pX>Map.X) then exit;
if (pY<1)or(pY>Map.Y) then exit;

glColor4f(1,1,1,1);
case Index-1 of
   0..63: glBindTexture(GL_TEXTURE_2D, Text0);
 64..127: glBindTexture(GL_TEXTURE_2D, Text1);
128..191: glBindTexture(GL_TEXTURE_2D, Text2);
192..255: glBindTexture(GL_TEXTURE_2D, Text3); end;
xt:=Index-1;
k:=pX; i:=pY;
TexC[1,1]:=(xt mod 8  )/8+Overlap; TexC[1,2]:=1-(xt div 8  )/8-Overlap;
TexC[2,1]:=(xt mod 8  )/8+Overlap; TexC[2,2]:=1-(xt div 8+1)/8+Overlap;
TexC[3,1]:=(xt mod 8+1)/8-Overlap; TexC[3,2]:=1-(xt div 8+1)/8+Overlap;
TexC[4,1]:=(xt mod 8+1)/8-Overlap; TexC[4,2]:=1-(xt div 8  )/8-Overlap;
TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

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
end;

procedure TRender.RenderRectangle(TexID:integer; X,Y,SizeX,SizeY:integer);
begin
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(0,0); glvertex2f(X      ,Y      );
    glTexCoord2f(1,0); glvertex2f(X+SizeX,Y      );
    glTexCoord2f(1,1); glvertex2f(X+SizeX,Y+SizeY);
    glTexCoord2f(0,1); glvertex2f(X      ,Y+SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TRender.Render3DButton(GUITexID:integer; X,Y,SizeX,SizeY:integer);
begin
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, GUITex[402,1]);
    glBegin (GL_QUADS);
    glTexCoord2f(0,0); glvertex2f(X      ,Y      );
    glTexCoord2f(1,0); glvertex2f(X+SizeX,Y      );
    glTexCoord2f(1,1); glvertex2f(X+SizeX,Y+SizeY);
    glTexCoord2f(0,1); glvertex2f(X      ,Y+SizeY);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);

    RenderRectangle(GUITex[GUITexID,1],X,Y,GUITex[GUITexID,2],GUITex[GUITexID,3]);
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
  glColor4f(Owner/2,1,1,1);
  ShiftX:=UnitPivot[ID].x/CellSize;
  ShiftY:=(UnitPivot[ID].y+UnitSize[ID,2])/CellSize-fTerrain.Land[round(pY)+1,round(pX)].Height/xh;
  RenderSprite(UnitTex[ID,1],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
end;

procedure TRender.RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitCarry[CarryID].Dir[DirID].Count;
ID:=UnitCarry[CarryID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  glColor4f(Owner/2,1,1,1);
  ShiftX:=UnitPivot[ID].x/CellSize;
  ShiftY:=(UnitPivot[ID].y+UnitSize[ID,2])/CellSize-fTerrain.Land[round(pY)+1,round(pX)].Height/xh;
  ShiftX:=ShiftX+UnitCarry[CarryID].Dir[DirID].MoveX/40;
  ShiftY:=ShiftY+UnitCarry[CarryID].Dir[DirID].MoveY/40;
  RenderSprite(UnitTex[ID,1],pX+ShiftX,pY+ShiftY,UnitTex[ID,2]/40,UnitTex[ID,3]/40);
end;



end.
