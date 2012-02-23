unit KM_RenderAux;
{$I KaM_Remake.inc}
interface
uses
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_Points;

type
  TRenderAux = class
  private
    procedure RenderDot(pX,pY:single; Size:single = 0.05);
    procedure RenderDotOnTile(pX,pY:single);
    procedure RenderLine(x1,y1,x2,y2:single);
    procedure RenderQuad(pX,pY:integer);
  public
    procedure Circle(x,y,rad:single; Fill,Line:TColor4);
    procedure Dot(X,Y:single; aCol:TColor4);
    procedure Passability(aRect: TKMRect; aPass: Integer);
    procedure Projectile(x1,y1,x2,y2:single);
    procedure Quad(pX,pY:integer; aCol:TColor4);
    procedure Text(pX,pY:integer; aText:string; aCol:TColor4);
    procedure UnitMoves(aRect: TKMRect);
    procedure UnitPointers(pX,pY:single; Count:integer);
    procedure UnitRoute(NodeList:TKMPointList; Pos:integer; aUnitType:byte);
    procedure Wires(aRect: TKMRect);
  end;


var
  fRenderAux: TRenderAux;


implementation
uses KM_Terrain;


{Simple dot to know where it actualy is}
procedure TRenderAux.RenderDot(pX,pY:single; Size:single = 0.05);
begin
  glBegin(GL_QUADS);
    glkRect(pX-1-Size,pY-1+Size,pX-1+Size,pY-1-Size);
  glEnd;
end;


procedure TRenderAux.RenderDotOnTile(pX,pY:single);
begin
  pY := pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV;
  glBegin(GL_QUADS);
    glkRect(pX-1,pY-1,pX-1+0.1,pY-1-0.1);
  glEnd;
end;


procedure TRenderAux.RenderLine(x1,y1,x2,y2:single);
begin
  glBegin(GL_LINES);
    glVertex2f(x1-1, y1-1 - fTerrain.InterpolateLandHeight(x1,y1)/CELL_HEIGHT_DIV);
    glVertex2f(x2-1, y2-1 - fTerrain.InterpolateLandHeight(x2,y2)/CELL_HEIGHT_DIV);
  glEnd;
end;


{Used for internal things like overlays, etc..}
procedure TRenderAux.RenderQuad(pX,pY:integer);
begin
  if not fTerrain.TileInMapCoords(pX,pY) then exit;

  glBegin(GL_QUADS);
    with fTerrain do
    glkQuad(pX-1,pY-1-Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV,
            pX  ,pY-1-Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV,
            pX  ,pY-  Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV,
            pX-1,pY-  Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV);
  glEnd;
end;


procedure TRenderAux.Circle(x,y,rad:single; Fill,Line:TColor4);
const SEC_COUNT=20;
var i:integer;
begin
  glPushMatrix;
    glTranslatef(x,y,0);
    glColor4ubv(@Fill);
    glBegin(GL_POLYGON);
      for i:=-SEC_COUNT to SEC_COUNT do
        glvertex3f(cos(i/SEC_COUNT*pi)*rad,sin(i/SEC_COUNT*pi)*rad,0);//-1..1
    glEnd;
    glBegin(GL_POLYGON);
      for i:=-SEC_COUNT to SEC_COUNT do
        glvertex3f(cos(i/SEC_COUNT*pi)*rad/3,sin(i/SEC_COUNT*pi)*rad/3,0);//-1..1
    glEnd;
    glColor4ubv(@Line);
    glBegin(GL_LINE_STRIP);
      for i:=-SEC_COUNT to SEC_COUNT do
        glvertex3f(cos(i/SEC_COUNT*pi)*rad,sin(i/SEC_COUNT*pi)*rad,0);//-1..1
    glEnd;
  glPopMatrix;
end;


procedure TRenderAux.Dot(X,Y:single; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  RenderDot(X,Y);
end;


procedure TRenderAux.Passability(aRect: TKMRect; aPass: Integer);
var I,K: Integer;
begin
  if aPass <> 0 then
  begin
    glColor4f(0,1,0,0.25);
    for I := aRect.Y1 to aRect.Y2 do
    for K := aRect.X1 to aRect.X2 do
      if TPassability(Byte(Low(TPassability)) + aPass) in fTerrain.Land[I,K].Passability then
        RenderQuad(K,I);
  end;
end;


procedure TRenderAux.Projectile(x1,y1,x2,y2:single);
begin
  glColor4f(1, 1, 0, 1);
  RenderDot(x1,y1);
  glColor4f(1, 0, 0, 1);
  RenderDot(x2,y2,0.1);
  RenderLine(x1,y1,x2,y2);
end;


procedure TRenderAux.Quad(pX,pY:integer; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  RenderQuad(pX,pY);
end;


procedure TRenderAux.Text(pX,pY:integer; aText:string; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  glRasterPos2f(pX - 0.5,pY - 1 - fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
  glPrint(aText);
end;


procedure TRenderAux.UnitMoves(aRect: TKMRect);
var I,K: Integer; VertexUsage: Byte;
begin
  for I := aRect.Y1 to aRect.Y2 do
  for K := aRect.X1 to aRect.X2 do
  begin
    if fTerrain.Land[I,K].IsVertexUnit <> vu_None then
    begin
      VertexUsage := byte(fTerrain.Land[I,K].IsVertexUnit);
      glColor4f(1-VertexUsage/3, VertexUsage/3, 0.6, 0.8);
      RenderDot(K, I-fTerrain.InterpolateLandHeight(K,I)/CELL_HEIGHT_DIV, 0.3);
    end;
    if fTerrain.Land[I,K].IsUnit <> nil then
    begin
      glColor4f(0.17, 0.83, 0, 0.8);
      RenderQuad(K,I);
    end;
  end;
end;


procedure TRenderAux.UnitPointers(pX,pY:single; Count:integer);
var i:integer;
begin
  for i:=1 to Count do
    RenderDot(pX+i/5,pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
end;


procedure TRenderAux.UnitRoute(NodeList:TKMPointList; Pos:integer; aUnitType:byte);
var i,k:integer; x,y:single;
begin
  if NodeList.Count = 0 then exit;

  case aUnitType of
    1: glColor3f(1,0,0); //Serf
    10: glColor3f(1,0,1); //Worker
    15..30: glColor3f(0,1,0); //Army
    31..38: glColor3f(0,0.5,0); //Animals
    else glColor3f(1,1,0); //Citizens
  end;

  for i:=1 to NodeList.Count do
    RenderDotOnTile(NodeList.List[i].X+0.5,NodeList.List[i].Y+0.5);

  glBegin(GL_LINE_STRIP);
  for i:=1 to NodeList.Count do
    glVertex2f(NodeList.List[i].X-0.5,NodeList.List[i].Y-0.5-fTerrain.InterpolateLandHeight(NodeList.List[i].X+0.5,NodeList.List[i].Y+0.5)/CELL_HEIGHT_DIV);
  glEnd;

  glColor4f(1,1,1,1); //Vector where unit is going to
  i:=Pos;
  k:=min(Pos+1,NodeList.Count);
  x:=mix(NodeList.List[i].X-0.5,NodeList.List[k].X-0.5,0.4);
  y:=mix(NodeList.List[i].Y-0.5,NodeList.List[k].Y-0.5,0.4)+0.2; //0.2 to render vector a bit lower so it won't gets overdrawned by another route
  RenderDotOnTile(NodeList.List[i].X+0.5,NodeList.List[i].Y+0.5+0.2);
  glBegin(GL_LINES);
    glVertex2f(NodeList.List[i].X-0.5,NodeList.List[i].Y-0.5+0.2-fTerrain.InterpolateLandHeight(NodeList.List[i].X+0.5,NodeList.List[i].Y+0.5)/CELL_HEIGHT_DIV);
    glVertex2f(x,y-fTerrain.InterpolateLandHeight(x+1,y+1)/CELL_HEIGHT_DIV);
  glEnd;
end;


procedure TRenderAux.Wires(aRect: TKMRect);
var i,k:integer;
begin
  for I := aRect.Y1 to aRect.Y2 do
  begin
    glBegin(GL_LINE_STRIP);
    for K := aRect.X1 to aRect.X2 do
    begin
      glColor4f(0.8,1,0.6,1);
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  end;

  glPushAttrib(GL_POINT_BIT);
    glPointSize(3);
    glBegin(GL_POINTS);
    for I := aRect.Y1 to aRect.Y2 do
    for K := aRect.X1 to aRect.X2 do
    begin
      //glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
      glColor4f(byte(fTerrain.Land[i,k].Border=bt_HousePlan),byte(fTerrain.Land[i,k].Border=bt_HousePlan),0,1);
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  glPopAttrib;
end;


end.
