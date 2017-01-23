unit KM_Render;
{$I ..\KaM_Remake.inc}
interface

uses SysUtils, Forms,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} FileUtil, LCLIntf, LCLType,
  Classes, Graphics, Controls,
  {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  dglOpenGL, KromOGLUtils, Math, ExtCtrls;

procedure RenderTerrainAndRoads;
procedure RenderWires;
procedure RenderObjects;
procedure RenderBuildings;
procedure RenderCursorPosition(ActivePage:string);
procedure RenderArrows;

procedure RenderPoint(pX,pY:integer);
procedure RenderQuad(pX,pY:integer);
procedure RenderWireQuad(pX,pY:integer);
procedure RenderTile(Index,pX,pY,Rot:integer);
procedure RenderObject(Index,pX,pY:integer; Func:string);
procedure RenderHouse(Index,pX,pY,Owner,Stage:integer);
procedure RenderSprite(TexID:integer; pX,pY,SizeX,SizeY:single);
procedure RenderArrow(pX,pY:integer);

implementation

uses KM_Unit1, KM_Defaults, KM_TPlayer;

procedure RenderTerrainAndRoads;
var i,k:integer; ID,Rot:integer; rd:integer; col:integer;
ViewArea:TRect;
begin
  with Form1 do begin
  ViewArea.Left :=max( round(ScrollBar1.Position-Panel1.Width/CellSize/2/Zoom*10)   , 1      );
  ViewArea.Right:=min( round(ScrollBar1.Position+Panel1.Width/CellSize/2/Zoom*10)+1 , Map.X-1);

  ViewArea.Top   :=max( round(ScrollBar2.Position-Panel1.Height/CellSize/2/Zoom*10)   , 1      );
  ViewArea.Bottom:=min( round(ScrollBar2.Position+Panel1.Height/CellSize/2/Zoom*10)+4 , Map.Y-1);
  end;

  //i,k get clipped into map space inside RenderTile
  glColor4f(1,1,1,1);
  for i:=ViewArea.Top to ViewArea.Bottom do
  for k:=ViewArea.Left to ViewArea.Right do
    RenderTile(Land[i,k].Terrain+1,k,i,Land[i,k].Rot);

  if Form1.ShowFlow.Checked then
  for i:=ViewArea.Top to ViewArea.Bottom do
  for k:=ViewArea.Left to ViewArea.Right do
  if TileDirection[Land[i,k].Terrain+1]<>0 then begin
    col:=(TileDirection[Land[i,k].Terrain+1] - 1 + Land[i,k].Rot*2) mod 8;
    glColor4f(col/7,0.5,1-col/7,1);
    RenderArrow(k,i);
  end;

  if Mission<>nil then
  for i:=ViewArea.Top to ViewArea.Bottom do
  for k:=ViewArea.Left to ViewArea.Right do
  with Mission do begin
  if Roads[k,i]=gpR then
    begin
      rd:=0;
      if (Roads[k                  ,max(i-1,1)         ]=gpR) then inc(rd,1);  //   1
      if (Roads[min(k+1,MaxMapSize),i                  ]=gpR) then inc(rd,2);  //  8*2
      if (Roads[k                  ,min(i+1,MaxMapSize)]=gpR) then inc(rd,4);  //   4
      if (Roads[max(k-1,1)         ,i                  ]=gpR) then inc(rd,8);  //Take preset from table
      ID:=RoadsConnectivity[rd,1];
      Rot:=RoadsConnectivity[rd,2];

      glColor4f(1,1,1,1);
      RenderTile(ID,k,i,Rot);
      glColor4ub(PlayerColors[Owner[k,i],1],PlayerColors[Owner[k,i],2],PlayerColors[Owner[k,i],3],80);
      if Form1.Pallete.ActivePage.Caption='Houses' then RenderTile(ID,k,i,Rot);
    end;
  if Roads[k,i]=gpF then begin
      glColor4f(1,1,1,1);
      RenderTile(64,k,i,0);
      glColor4ub(PlayerColors[Owner[k,i],1],PlayerColors[Owner[k,i],2],PlayerColors[Owner[k,i],3],80);
      if Form1.Pallete.ActivePage.Caption='Houses' then RenderTile(64,k,i,0);
  end;
  if Roads[k,i]=gpW then begin
      glColor4f(1,1,1,1);
      RenderTile(56,k,i,0);
      glColor4ub(PlayerColors[Owner[k,i],1],PlayerColors[Owner[k,i],2],PlayerColors[Owner[k,i],3],80);
      if Form1.Pallete.ActivePage.Caption='Houses' then RenderTile(56,k,i,0);
  end;
  end;

  glColor4f(1,1,1,1);
  glBlendFunc(GL_DST_COLOR,GL_ONE);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glBegin(GL_QUADS);
  for i:=ViewArea.Top to ViewArea.Bottom do
  for k:=ViewArea.Left to ViewArea.Right do
    begin
    glNormal3f(0,1,0); //height difference / 35 seems to be about perfect

    glTexCoord2f(0,1-max(Land2[i  ,k  ].Light-16,0)/16); glvertex2f(k-1,i-1-Land2[i  ,k  ].Height/xh);
    glTexCoord2f(0,1-max(Land2[i+1,k  ].Light-16,0)/16); glvertex2f(k-1,i  -Land2[i+1,k  ].Height/xh);
    glTexCoord2f(0,1-max(Land2[i+1,k+1].Light-16,0)/16); glvertex2f(k  ,i  -Land2[i+1,k+1].Height/xh);
    glTexCoord2f(0,1-max(Land2[i  ,k+1].Light-16,0)/16); glvertex2f(k  ,i-1-Land2[i  ,k+1].Height/xh);
    end;
  glEnd;

  glBlendFunc(GL_ZERO,GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glBegin(GL_QUADS);
  for i:=ViewArea.Top to ViewArea.Bottom do
  for k:=ViewArea.Left to ViewArea.Right do
    begin
    glNormal3f(0,1,0); //height difference / 35 seems to be about perfect
    glTexCoord2f(0,1-max(16-Land2[i  ,k  ].Light,0)/16); glvertex2f(k-1,i-1-Land2[i  ,k  ].Height/xh);
    glTexCoord2f(0,1-max(16-Land2[i+1,k  ].Light,0)/16); glvertex2f(k-1,i  -Land2[i+1,k  ].Height/xh);
    glTexCoord2f(0,1-max(16-Land2[i+1,k+1].Light,0)/16); glvertex2f(k  ,i  -Land2[i+1,k+1].Height/xh);
    glTexCoord2f(0,1-max(16-Land2[i  ,k+1].Light,0)/16); glvertex2f(k  ,i-1-Land2[i  ,k+1].Height/xh);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  {if Form1.CB1.Checked then
  for i:=y1 to y2 do for k:=x1 to x2 do begin
  glColor4f(1,1,1,(Land[i,k].y1)/255);
  RenderQuad(k,i);
  end;  }
  glBindTexture(GL_TEXTURE_2D,0);
end;

procedure RenderObjects;
var i,k:integer;
begin
for i:=1 to Map.Y do for k:=1 to Map.X do
if Land[i,k].Obj<>255 then
  begin
  RenderObject(Land[i,k].Obj,k,i,'Normal');
  if Form1.ShowObjects.Checked then
    if ObjIndexInv[Land[i,k].Obj]<>0 then
    begin
    glPointSize(Zoom/5);
    glBegin(GL_POINTS);
    glvertex2f(k-1, i-1-Land2[i,k].Height/xh);
    glend; glPointSize(1);
    glRasterPos2f(k-1+0.05,i-1-0.05-Land2[i,k].Height/xh);
    glPrint(IntToStr(Land[i,k].Obj));
    glRasterPos2f(k-1+0.05,i-1+0.4-0.05-Land2[i,k].Height/xh);
    glPrint('#'+IntToStr(ObjIndexGFX[ObjIndexInv[Land[i,k].Obj]]));
    end;
  end;
end;

procedure RenderCursorPosition(ActivePage:string);
var ii,kk,Rad,x1,x2,y1,y2:integer;
Tmp:single;
begin
  glColor4f(1,1,1,1);
//==============================================
//-Terrain-
//Render a brush if mouse button is not pressed
//==============================================
  if BrushMode=bmTerrain then
    if (LandBrush<>0)and(not MousePressed) then
    begin
      Rad:=Form1.BrushSize.Position;
      if Rad = 0 then
        RenderPoint(MapXn,MapYn)                              //brush size smaller than one cell
      else
      if Rad mod 2 = 1 then
      begin                                               //There are two brush types here, even and odd size
        Rad:=Rad div 2;                                     //first comes odd sizes 1,3,5..
        for ii:=-Rad to Rad do for kk:=-Rad to Rad do       //
        if (Form1.RG_Terrain.ItemIndex=1)or(sqrt(sqr(ii)+sqr(kk))<Rad+0.5) then               //Rounding corners in a nice way
          RenderTile(Combo[LandBrush,LandBrush,1]+1,MapXc+kk,MapYc+ii,0);
      end
      else
      begin
        Rad:=Rad div 2;                                     //even sizes 2,4,6..
        for ii:=-Rad to Rad-1 do for kk:=-Rad to Rad-1 do   //
        if (Form1.RG_Terrain.ItemIndex=1)or(sqrt(sqr(ii+0.5)+sqr(kk+0.5))<Rad) then           //Rounding corners in a nice way
          RenderTile(Combo[LandBrush,LandBrush,1]+1,MapXc+kk,MapYc+ii,0);
      end;
    end;

//==============================================
//-Tiles-
//Render a tile if mouse button is not pressed
//==============================================
if BrushMode=bmTiles then
if (LandBrush<>0)and(not MousePressed) then
if LandBrush in [118,125,126] then RenderTile(LandBrush,MapXc,MapYc,Form1.RG_Angle.ItemIndex+2 mod 4)
                              else RenderTile(LandBrush,MapXc,MapYc,Form1.RG_Angle.ItemIndex);

//==============================================
//-TileRotate-
//Render a tile with rotate icon if mouse button is not pressed
//==============================================
if BrushMode=bmTileRotate then
if LandBrush<>0 then
  begin
  glColor4f(1,1,0,1);
  RenderWireQuad(MapXc,MapYc);
  end;

//==============================================
//-Relief-
//Render area to be modified
//==============================================
  if BrushMode=bmRelief then
  begin
    Rad := Form1.ElevSize.Position;
    x1:=max(MapXn-Rad,1); x2:=min(MapXn+Rad,Map.X);
    y1:=max(MapYn-Rad,1); y2:=min(MapYn+Rad,Map.Y-1);

    glBegin(GL_POINTS);
      for ii:=y1 to y2 do for kk:=x1 to x2 do
      begin
        case Form1.RG_Relief.ItemIndex of
          1:    Tmp := 1-max(abs(ii-MapYn),abs(kk-MapXn))/Rad; //pyramid falloff
          0:    Tmp := 1-sqrt(sqr(ii-MapYn)+sqr(kk-MapXn))/Rad;//circular falloff
          else  Tmp := 0;
        end;

        case LandBrush of
          1: glColor4f(0,1,0,max(Tmp,0));
          2: glColor4f(0,1,0,integer(Tmp>0));
          3: glColor4f(0,1,0,abs(Land2[ii,kk].Height-Land2[MapYn,MapXn].Height)/8);
        end;

        if Tmp > 0 then
          glVertex2f(kk-1,ii-1-Land2[ii,kk].Height/xh);
      end;
    glEnd;
  end;

//==============================================
//-Objects-
//Render object. If there's an object to be replaced/removed render it in red
//==============================================
if BrushMode=bmObjects then if LandBrush<>0 then
  begin
  if Land[MapYc,MapXc].Obj<>255 then
  RenderObject(Land[MapYc,MapXc].Obj,MapXc,MapYc,'ToDel');
  if LandBrush=255 then
    begin
    glColor4f(1,0,0,0.2);    //Object eraser
    RenderQuad(MapXc,MapYc);
    end
  else
  RenderObject(ObjIndex[LandBrush],MapXc,MapYc,'Normal');
  end;

//==============================================
//-Houses-
//Render house shape
//==============================================
if BrushMode=bmHouses then
if (LandBrush in [0])and(not MousePressed) then
  begin
  glColor4f(1,0,0,0.2);    //Object eraser
  RenderQuad(MapXc,MapYc);
  end else
if LandBrush in [1..29] then
  begin
  RenderHouse(LandBrush,MapXc,MapYc,Mission.ActivePlayer,0);
  end else
if LandBrush = 97 then RenderTile(56,MapXc,MapYc,0)
  else
if LandBrush = 98 then RenderTile(64,MapXc,MapYc,0)
  else
if LandBrush = 99 then RenderTile(255,MapXc,MapYc,0)
  else
  glBindTexture(GL_TEXTURE_2D, 0);

//==============================================
//-Copy area-
//Render two anchors and a rect between them
//==============================================
if BrushMode=bmCopy then
  begin
  glColor4f(1,0,0,0.5); RenderQuad(CopyArea[1,1],CopyArea[1,2]);
  glColor4f(0,0,1,0.5); RenderQuad(CopyArea[2,1],CopyArea[2,2]);
  x1:=CopyArea[1,1]; y1:=min(CopyArea[1,2],CopyArea[2,2]);
  x2:=CopyArea[2,1]; y2:=max(CopyArea[1,2],CopyArea[2,2]);
  if (x1<1)or(x2>Map.X) then exit;
  if (y1<1)or(y2>Map.Y) then exit;
  glColor4f(1,1,1,0.33);  
  glBegin(GL_QUADS);
  glNormal3f(0,1,0);
  glvertex2f(x1-1,y1-1-Land2[y1,x1].Height/xh);
  glvertex2f(x2  ,y1-1-Land2[y1,x2].Height/xh);
  glvertex2f(x2  ,y2  -Land2[y2,x2].Height/xh);
  glvertex2f(x1-1,y2  -Land2[y2,x1].Height/xh);
  glEnd;
  end;

//==============================================
//-Paste area-
//Render two anchors and paste area preview
//==============================================
  if BrushMode=bmPaste then
  begin
    for ii:=1 to abs(CopyArea[2,2]-CopyArea[1,2])+1 do
    for kk:=1 to abs(CopyArea[2,1]-CopyArea[1,1])+1 do
    RenderTile(LandCopy[ii,kk].Terrain+1,MapXc+kk-1,MapYc+ii-1,LandCopy[ii,kk].Rot);
    glColor4f(1,0,0,0.5); RenderQuad(MapXc,MapYc);
    glColor4f(0,0,1,0.5); RenderQuad(MapXc+abs(CopyArea[2,1]-CopyArea[1,1]),MapYc+abs(CopyArea[2,2]-CopyArea[1,2]));
  end;
  
  glPointSize(1);
  glLineWidth(1);
end;

procedure RenderBuildings;
var i,k:integer;
begin
if Mission<>nil then
for i:=1 to 8 do
  for k:=1 to Mission.Player[i].HouseCount do
  RenderHouse(Mission.Player[i].House[k].Kind,
              Mission.Player[i].House[k].PosX,
              Mission.Player[i].House[k].PosY,i,1);
glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure RenderWires;
var i,k:integer; //T:^Byte;
begin
glLineWidth(1);
for i:=max(MapYc-10,1) to min(MapYc+10,Map.Y) do begin
glBegin(GL_LINE_STRIP);
for k:=max(MapXc-11,1) to min(MapXc+11,Map.X) do begin
glColor4f(0.8,1,0.6,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
glvertex2f(k-1,i-1-Land2[i,k].Height/xh);
end;
glEnd;
end;

glBegin(GL_POINTS);
for i:=max(MapYc-10,1) to min(MapYc+10,Map.Y) do
for k:=max(MapXc-10,1) to min(MapXc+10,Map.X) do begin
glColor4f(Land2[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
glvertex2f(k-1,i-1-Land2[i,k].Height/xh);
end;
glEnd;
{
for i:=max(MapYc-10,1) to min(MapYc+10,Map.Y) do
for k:=max(MapXc-10,1) to min(MapXc+10,Map.X) do begin
glRasterPos2f(k-1+0.1,i-1-0.1-Land[i,k].Height1/xh);
glColor4f(0.6,1,0.45,0.75);

T:=Pointer(Integer(@Land[i,k].Terrain)+Form1.SpinEdit1.Value-1);
glPrint(IntToStr(T^));

//if Land[i,k].Rot and 4=4 then glPrint('X');
//if Land[i,k].Rot and 8=8 then glPrint('Y');
//glPrint(IntToStr(Land[i,k].Border));
end;  }
glLineWidth(Zoom/4);
end;

procedure RenderArrows;
begin
  glBegin(GL_LINES);
  glColor4f(1,0,0,1);
  glvertex2f(10,0  ); glvertex2f(0,0);
  glvertex2f(9, 0.5); glvertex2f(10,0);
  glvertex2f(9,-0.5); glvertex2f(10,0);
  glColor4f(0,1,0,1);
  glvertex2f(0,10  ); glvertex2f(0,0);
  glvertex2f( 0.5,9); glvertex2f(0,10);
  glvertex2f(-0.5,9); glvertex2f(0,10);
  glEnd;
end;

procedure RenderPoint(pX,pY:integer);
begin
if pX<1 then
s:='7';
glColor4f(0.4,0.3,0,1);
glBegin(GL_POINTS);
glvertex2f(pX-1,pY-1-Land2[pY,pX].Height/xh);
glEnd;
end;

procedure RenderQuad(pX,pY:integer);
begin
if (pX<=0)or(pX>=Map.X) then exit;
if (pY<=0)or(pY>=Map.Y) then exit;
glBegin(GL_QUADS);
glNormal3f(0,1,0);
glvertex2f(pX-1,pY-1-Land2[pY  ,pX  ].Height/xh);
glvertex2f(pX  ,pY-1-Land2[pY  ,pX+1].Height/xh);
glvertex2f(pX  ,pY-  Land2[pY+1,pX+1].Height/xh);
glvertex2f(pX-1,pY-  Land2[pY+1,pX  ].Height/xh);
glEnd;
end;

procedure RenderWireQuad(pX,pY:integer);
begin
if (pX<=0)or(pX>=Map.X) then exit;
if (pY<=0)or(pY>=Map.Y) then exit;
glBegin(GL_LINE_LOOP);
glNormal3f(0,1,0);
glvertex2f(pX-1,pY-1-Land2[pY  ,pX  ].Height/xh);
glvertex2f(pX  ,pY-1-Land2[pY  ,pX+1].Height/xh);
glvertex2f(pX  ,pY-  Land2[pY+1,pX+1].Height/xh);
glvertex2f(pX-1,pY-  Land2[pY+1,pX  ].Height/xh);
glEnd;
end;

procedure RenderTile(Index,pX,pY,Rot:integer);
var xt,k,i,a:integer;
begin
  if (pX<1)or(pX>Map.X) then exit;
  if (pY<1)or(pY>Map.Y) then exit;

  glBindTexture(GL_TEXTURE_2D, Text1);

  xt:=Index-1;
  k:=pX; i:=pY;
  TexC[1,1]:=(xt mod 16  )/16+Overlap; TexC[1,2]:=(xt div 16    )/16+Overlap;
  TexC[2,1]:=(xt mod 16  )/16+Overlap; TexC[2,2]:=(xt div 16 + 1)/16-Overlap;
  TexC[3,1]:=(xt mod 16+1)/16-Overlap; TexC[3,2]:=(xt div 16 + 1)/16-Overlap;
  TexC[4,1]:=(xt mod 16+1)/16-Overlap; TexC[4,2]:=(xt div 16    )/16+Overlap;
  TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

  if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
  if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

  glbegin(GL_QUADS);
  glTexCoord2fv(@TexC[TexO[1]]); glvertex2f(k-1,i-1-Land2[i  ,k  ].Height/xh);
  glTexCoord2fv(@TexC[TexO[2]]); glvertex2f(k-1,i  -Land2[i+1,k  ].Height/xh);
  glTexCoord2fv(@TexC[TexO[3]]); glvertex2f(k  ,i  -Land2[i+1,k+1].Height/xh);
  glTexCoord2fv(@TexC[TexO[4]]); glvertex2f(k  ,i-1-Land2[i  ,k+1].Height/xh);
  glEnd;
end;

procedure RenderObject(Index,pX,pY:integer; Func:string);
var ShiftX,ShiftY:single; ID:integer;
begin
if ObjIndexInv[Index]=0 then
exit;
ID:=ObjIndexGFX[ObjIndexInv[Index]];
if ID=0 then exit;
if Index=61 then begin //Object 42 is an invisible wall
  glLineWidth(Zoom/2);
  glColor4f(1,0,0,0.5); glBindTexture(GL_TEXTURE_2D,0);
  glBegin(GL_LINES);
  glNormal3f(0,1,0);
  glvertex2f(pX-1,pY-1-Land2[pY  ,pX  ].Height/xh);
  glvertex2f(pX  ,pY-  Land2[pY+1,pX+1].Height/xh);
  glvertex2f(pX  ,pY-1-Land2[pY  ,pX+1].Height/xh);
  glvertex2f(pX-1,pY-  Land2[pY+1,pX  ].Height/xh);
  glEnd;
  glLineWidth(1);
end else begin
  if Func='Normal' then glColor4f(1,1,1,1);
  if Func='ToDel' then glColor4f(1,0,0,1);
  ShiftX:=TreePivot[ID].x/CellSize;
  ShiftY:=(TreePivot[ID].y+TreeSize[ID,2])/CellSize-Land2[pY,pX].Height/xh;
  RenderSprite(Tree[ID], pX+ShiftX, pY+ShiftY, 128/CellSize, 128/CellSize);
  if Index=60 then begin
    RenderSprite(Tree[ID], pX+ShiftX+0.65, pY+ShiftY, 128/CellSize, 128/CellSize);
    RenderSprite(Tree[ID], pX+ShiftX, pY+ShiftY+0.65, 128/CellSize, 128/CellSize);
    RenderSprite(Tree[ID], pX+ShiftX+0.65, pY+ShiftY+0.65, 128/CellSize, 128/CellSize);
  end;
  if (Index=249)or(Index=250) then begin
    glColor4f(1,0,0,0.5); glBindTexture(GL_TEXTURE_2D,0);
    glBegin(GL_LINES);
    glNormal3f(0,1,0);
    glvertex2f(pX-1,pY-1-Land2[pY  ,pX  ].Height/xh);
    glvertex2f(pX  ,pY-  Land2[pY+1,pX+1].Height/xh);
    glvertex2f(pX  ,pY-1-Land2[pY  ,pX+1].Height/xh);
    glvertex2f(pX-1,pY-  Land2[pY+1,pX  ].Height/xh);
    glEnd;
  end;
end;
end;

procedure RenderHouse(Index,pX,pY,Owner,Stage:integer);
var ShiftX,ShiftY:single; ID:integer; i,k:integer;
begin
  ID:=HouseIndexGFX[Index];
  if ID=0 then exit;
  if Stage=1 then
  begin
    glColor4f(1,1,1,1);
    ShiftX:=HousePivot[ID].x/CellSize;
    ShiftY:=(HousePivot[ID].y+HouseSize[ID,2])/CellSize-Land2[pY+1,pX].Height/xh;
    RenderSprite(House[Index], pX+ShiftX, pY+ShiftY, 256/CellSize, 256/CellSize);
    glColor4ubv(@PlayerColors[Owner]);
    if Form1.Pallete.ActivePage.Caption='Houses' then
    RenderSprite(0, pX+ShiftX, pY+ShiftY, 2, 0.1);
  end;

  if Stage=0 then
  begin
    glColor4f(0,1,1,1);
    for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[Index,i,k]=1 then RenderWireQuad(pX+k-3,pY+i-4)
    else if HousePlanYX[Index,i,k]=2 then RenderQuad(pX+k-3,pY+i-4);
  //  glColor3ub(R[Owner],G[Owner],B[Owner]); //Owner color
  //  RenderQuad(pX,pY);
  end;
end;


procedure RenderSprite(TexID:integer; pX,pY,SizeX,SizeY:single);
begin
  glBindTexture(GL_TEXTURE_2D, TexID);
  glBegin(GL_QUADS);
    glTexCoord2f(0,1); glvertex2f(pX-1      ,pY-1      );
    glTexCoord2f(1,1); glvertex2f(pX-1+SizeX,pY-1      );
    glTexCoord2f(1,0); glvertex2f(pX-1+SizeX,pY-1-SizeY);
    glTexCoord2f(0,0); glvertex2f(pX-1      ,pY-1-SizeY);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure RenderArrow(pX,pY:integer);
var A,cx:single;
begin                                           //cos(0)=1
                                                //sin(0)=0
  A:=(-90+TileDirection[Land[pY,pX].Terrain+1]*45+Land[pY,pX].Rot*90)/180*pi;
  cx:=(Land2[pY,pX].Height+Land2[pY+1,pX].Height+Land2[pY+1,pX+1].Height+Land2[pY,pX+1].Height)/xh/4;

  glBindTexture(GL_TEXTURE_2D, textA);
  glBegin(GL_QUADS);
    glNormal3f(0,1,0);
    A:=A+pi/2; glTexCoord2f(sin(A)/2+0.5,1-cos(A)/2-0.5); glvertex2f(pX-1,pY-1-cx);
    A:=A+pi/2; glTexCoord2f(sin(A)/2+0.5,1-cos(A)/2-0.5); glvertex2f(pX-1,pY  -cx);
    A:=A+pi/2; glTexCoord2f(sin(A)/2+0.5,1-cos(A)/2-0.5); glvertex2f(pX  ,pY  -cx);
    A:=A+pi/2; glTexCoord2f(sin(A)/2+0.5,1-cos(A)/2-0.5); glvertex2f(pX  ,pY-1-cx);
  glEnd;
end;

end.
