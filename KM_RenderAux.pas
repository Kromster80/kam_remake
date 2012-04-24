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
    procedure CircleOnTerrain(X, Y, Rad: Single; Fill, Line: TColor4);
    procedure Dot(X,Y:single; aCol:TColor4);
    procedure DotOnTerrain(X,Y:single; aCol:TColor4);
    procedure Passability(aRect: TKMRect; aPass: Integer);
    procedure Projectile(x1,y1,x2,y2:single);
    procedure Quad(pX,pY:integer; aCol:TColor4);
    procedure SquareOnTerrain(X1, Y1, X2, Y2: Single; Fill, Line: TColor4);
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
    glkRect(pX-Size,pY+Size,pX+Size,pY-Size);
  glEnd;
end;


procedure TRenderAux.RenderDotOnTile(pX,pY:single);
begin
  pY := pY-fTerrain.HeightAt(pX,pY)/CELL_HEIGHT_DIV;
  glBegin(GL_QUADS);
    glkRect(pX,pY,pX+0.1,pY-0.1);
  glEnd;
end;


procedure TRenderAux.RenderLine(x1,y1,x2,y2:single);
begin
  glBegin(GL_LINES);
    glVertex2f(x1, y1 - fTerrain.HeightAt(x1,y1)/CELL_HEIGHT_DIV);
    glVertex2f(x2, y2 - fTerrain.HeightAt(x2,y2)/CELL_HEIGHT_DIV);
  glEnd;
end;


{Used for internal things like overlays, etc..}
procedure TRenderAux.RenderQuad(pX,pY:Integer);
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


procedure TRenderAux.Circle(X,Y,Rad: Single; Fill,Line: TColor4);
const SEC_COUNT = 20;
var I: Integer;
begin
  glPushMatrix;
    glTranslatef(X,Y,0);
    glColor4ubv(@Fill);
    glBegin(GL_POLYGON);
      for I := -SEC_COUNT to SEC_COUNT do
        glVertex2f(Cos(I/SEC_COUNT*pi)*Rad, Sin(I/SEC_COUNT*pi)*Rad);//-1..1
    glEnd;
    glBegin(GL_POLYGON);
      for I := -SEC_COUNT to SEC_COUNT do
        glVertex2f(Cos(I/SEC_COUNT*pi)*Rad/3, Sin(I/SEC_COUNT*pi)*Rad/3);//-1..1
    glEnd;
    glColor4ubv(@Line);
    glBegin(GL_LINE_STRIP);
      for I := -SEC_COUNT to SEC_COUNT do
        glVertex2f(Cos(I/SEC_COUNT*pi)*Rad, Sin(I/SEC_COUNT*pi)*Rad);//-1..1
    glEnd;
  glPopMatrix;
end;


procedure TRenderAux.CircleOnTerrain(X, Y, Rad: Single; Fill, Line: TColor4);
const SEC_COUNT = 24;
var I: Integer; C,S: Single;
begin
  glColor4ubv(@Line);
  glBegin(GL_LINE_LOOP);
    for I := -SEC_COUNT to SEC_COUNT - 1 do
    begin
      C := Cos(I / SEC_COUNT * Pi) * Rad;
      S := Sin(I / SEC_COUNT * Pi) * Rad;
      glVertex2f(X + C, Y + S - fTerrain.HeightAt(X + C, Y + S) / CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


procedure TRenderAux.SquareOnTerrain(X1, Y1, X2, Y2: Single; Fill, Line: TColor4);
var I: Integer;
begin
  glColor4ubv(@Line);
  glBegin(GL_LINE_LOOP);
    glVertex2f(X1, Y1 - fTerrain.HeightAt(X1, Y1) / CELL_HEIGHT_DIV);
    for I := Ceil(X1) to Trunc(X2) do
      glVertex2f(I, Y1 - fTerrain.HeightAt(I, Y1) / CELL_HEIGHT_DIV);
    glVertex2f(X2, Y1 - fTerrain.HeightAt(X2, Y1) / CELL_HEIGHT_DIV);

    glVertex2f(X2, Y2 - fTerrain.HeightAt(X2, Y2) / CELL_HEIGHT_DIV);
    for I := Trunc(X2) downto Ceil(X1) do
      glVertex2f(I, Y2 - fTerrain.HeightAt(I, Y2) / CELL_HEIGHT_DIV);
    glVertex2f(X1, Y2 - fTerrain.HeightAt(X1, Y2) / CELL_HEIGHT_DIV);
  glEnd;
end;


procedure TRenderAux.Dot(X,Y:single; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  RenderDot(X,Y);
end;


procedure TRenderAux.DotOnTerrain(X,Y:single; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  RenderDot(X,Y - fTerrain.HeightAt(X, Y) / CELL_HEIGHT_DIV);
end;


procedure TRenderAux.Passability(aRect: TKMRect; aPass: Integer);
var I,K: Integer;
begin
  if aPass <> 0 then
  begin
    glColor4f(0,1,0,0.25);
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      if TPassability(Byte(Low(TPassability)) + aPass - 1) in fTerrain.Land[I,K].Passability then
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
  glRasterPos2f(pX - 0.5,pY - 1 - fTerrain.HeightAt(pX,pY)/CELL_HEIGHT_DIV);
  glPrint(aText);
end;


procedure TRenderAux.UnitMoves(aRect: TKMRect);
var I,K: Integer; VertexUsage: Byte;
begin
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
  begin
    if fTerrain.Land[I,K].IsVertexUnit <> vu_None then
    begin
      VertexUsage := byte(fTerrain.Land[I,K].IsVertexUnit);
      glColor4f(1-VertexUsage/3, VertexUsage/3, 0.6, 0.8);
      RenderDot(K, I-fTerrain.HeightAt(K,I)/CELL_HEIGHT_DIV, 0.3);
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
    RenderDot(pX+i/5,pY-fTerrain.HeightAt(pX,pY)/CELL_HEIGHT_DIV);
end;


procedure TRenderAux.UnitRoute(NodeList: TKMPointList; Pos: Integer; aUnitType: Byte);
var
  I, K: Integer;
  FaceX, FaceY: Single;
begin
  if NodeList.Count = 0 then Exit;

  case aUnitType of
    1: glColor3f(1,0,0); //Serf
    10: glColor3f(1,0,1); //Worker
    15..30: glColor3f(0,1,0); //Army
    31..38: glColor3f(0,0.5,0); //Animals
    else glColor3f(1,1,0); //Citizens
  end;

  for I := 0 to NodeList.Count - 1 do
    RenderDotOnTile(NodeList[I].X + 0.5, NodeList[I].Y + 0.5);

  glBegin(GL_LINE_STRIP);
    for I := 0 to NodeList.Count - 1 do
      glVertex2f(NodeList[I].X-0.5, NodeList[I].Y-0.5-fTerrain.HeightAt(NodeList[I].X+0.5, NodeList[I].Y+0.5)/CELL_HEIGHT_DIV);
  glEnd;

  glColor4f(1,1,1,1); //Vector where unit is going to
  I := Pos;
  K := Min(Pos + 1, NodeList.Count - 1);
  FaceX := Mix(NodeList[I].X - 0.5, NodeList[K].X - 0.5, 0.4);
  FaceY := Mix(NodeList[I].Y - 0.5, NodeList[K].Y - 0.5, 0.4) + 0.2; //0.2 to render vector a bit lower so it won't gets overdrawned by another route
  RenderDotOnTile(NodeList[I].X + 0.5, NodeList[I].Y + 0.5 + 0.2);
  glBegin(GL_LINES);
    glVertex2f(NodeList[I].X-0.5, NodeList[I].Y-0.5+0.2-fTerrain.HeightAt(NodeList[I].X+0.5,NodeList[I].Y+0.5)/CELL_HEIGHT_DIV);
    glVertex2f(FaceX, FaceY - fTerrain.HeightAt(FaceX+1, FaceY+1)/CELL_HEIGHT_DIV);
  glEnd;
end;


procedure TRenderAux.Wires(aRect: TKMRect);
var i,k:integer;
begin
  for I := aRect.Top to aRect.Bottom do
  begin
    glBegin(GL_LINE_STRIP);
    for K := aRect.Left to aRect.Right do
    begin
      glColor4f(0.8,1,0.6,1);
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  end;

  glPushAttrib(GL_POINT_BIT);
    glPointSize(3);
    glBegin(GL_POINTS);
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      //glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
      glColor4f(byte(fTerrain.Land[i,k].Border=bt_HousePlan),byte(fTerrain.Land[i,k].Border=bt_HousePlan),0,1);
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  glPopAttrib;
end;


end.
