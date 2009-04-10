unit KM_Render;
interface
uses OpenGL, dglOpenGL, windows, sysutils, Forms, KromOGLUtils, KromUtils, math, ExtCtrls, JPEG, Graphics,
  KM_TGATexture, KM_Defaults;

type
TRender = class
private
  h_DC: HDC;
  h_RC: HGLRC;
  TextG,Text512:GLuint;
  RenderCount:integer;
  RO:array of integer; //RenderOrder
  RenderList:array of record
    Loc:TKMPointF;
    RX:byte;
    ID:word;
    NewInst:boolean;
    Team:byte;
    AlphaStep:single; //Only appliable to HouseBuild
    FOWvalue:byte; // Fog of War thickness
  end;
  RenderAreaSize:TKMPoint;
  procedure RenderDot(pX,pY:single);
  procedure RenderDotOnTile(pX,pY:single);
  procedure RenderQuad(pX,pY:integer);
  procedure RenderTile(Index,pX,pY,Rot:integer);
  procedure RenderSprite(RX:byte; ID:word; pX,pY:single; const Col:TColor4=$FF; const aFOW:byte=$FF);
  procedure RenderSpriteAlphaTest(RX:byte; ID:word; Param:single; pX,pY:single; const Col:TColor4=$FF);
  procedure AddSpriteToList(aRX:byte; aID:word; pX,pY:single; aNew:boolean; const aTeam:byte=0; const Step:single=-1; const aFOW:byte=$FF);
  procedure SortRenderList;
  procedure RenderRenderList;
  procedure RenderCursorHighlights;
  procedure RenderBrightness(Value:byte);
protected
public
  constructor Create(RenderFrame:HWND);
  destructor Destroy; override;
  procedure RenderResize(Width,Height:integer);
  procedure Render();
  procedure DoPrintScreen(filename:string);
  procedure RenderTerrainAndFields(x1,x2,y1,y2:integer);
  procedure RenderFieldBorders(x1,x2,y1,y2:integer);
  procedure RenderTerrainObjects(x1,x2,y1,y2,AnimStep:integer);
  procedure RenderWires();
  procedure RenderUnitMoves();
  procedure RenderRoute(Count:integer; Nodes:array of TKMPoint; Col:TColor4);
  procedure RenderBuildIcon(P:TKMPoint; id:integer=479);
  procedure RenderWireQuad(P:TKMPoint; Col:TColor4);
  procedure RenderWireHousePlan(P:TKMPoint; aHouseType:THouseType);
  procedure RenderObject(Index,AnimStep,pX,pY:integer);
  procedure RenderObjectSpecial(Fs:TFieldSpecial; AnimStep,pX,pY:integer);
  procedure RenderMarkup(Index:integer; pX,pY:integer);
  procedure RenderBorder(Border:TBorderType; Dir:integer; pX,pY:integer);
  procedure RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single; NewInst:boolean);
  procedure RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
  procedure RenderHouseBuild(Index,pX,pY:integer);
  procedure RenderHouseBuildSupply(Index:integer; Wood,Stone:byte; pX,pY:integer);
  procedure RenderHouseWood(Index:integer; Step:single; pX,pY:integer);
  procedure RenderHouseStone(Index:integer; Step:single; pX,pY:integer);
  procedure RenderHouseSupply(Index:integer; R1,R2:array of byte; pX,pY:integer);
  procedure RenderHouseWork(Index,AnimType,AnimStep,Owner,pX,pY:integer);
  property GetRenderAreaSize:TKMPoint read RenderAreaSize;
published
end;

var
  fRender: TRender;

implementation 
uses KM_Unit1, KM_Terrain, KM_Units, KM_Houses, KM_Viewport, KM_Controls, KM_Users,
KM_Settings, KM_GamePlayInterface, KM_Game;


constructor TRender.Create(RenderFrame:HWND);
begin
  Inherited Create;
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults();

  glDisable(GL_LIGHTING);
  fLog.AppendLog('Pre-texture done');
  LoadTexture(ExeDir+'Resource\gradient.tga', TextG,0);    // Load the Textures
  LoadTexture(ExeDir+'Resource\Tiles512.tga', Text512,0);  // Load the Textures
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
  Inherited;
end;


procedure TRender.RenderResize(Width,Height:integer);
begin
  if Height=0 then Height:=1;
  if Width=0  then Width :=1;
  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity();                   // Reset View
  gluOrtho2D(0,Width,Height,0);
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View
  RenderAreaSize.X:=Width;
  RenderAreaSize.Y:=Height;
end;

procedure TRender.Render();
begin
  glClear(GL_COLOR_BUFFER_BIT);    // Clear The Screen, can save some FPS on this one

  if fGame.GameIsRunning then begin //If game is running
    glLoadIdentity();                // Reset The View
    glTranslate(fViewport.ViewWidth/2,fViewport.ViewHeight/2,0);
    glkScale(fViewport.Zoom*CELL_SIZE_PX);
    glTranslate(-fViewport.GetCenter.X+ToolBarWidth/CELL_SIZE_PX/fViewport.Zoom,-fViewport.GetCenter.Y,0);

    glLineWidth(fViewport.Zoom*2);
    glPointSize(fViewport.Zoom*5);

    RenderCount:=0; //Init RenderList

    fTerrain.Paint;
    glLineWidth(1);
    glPointSize(1);
    if ShowTerrainWires then RenderWires();
    if MakeShowUnitMove then RenderUnitMoves();

    fPlayers.Paint;            //Units and houses

    SortRenderList;
    RenderRenderList;

    glLineWidth(fViewport.Zoom*2);
    glPointSize(fViewport.Zoom*5);
    RenderCursorHighlights();
    glLineWidth(1);
    glPointSize(1);

    glLoadIdentity();             // Reset The View
    glLineWidth(1);
    glPointSize(1);
    glkMoveAALines(true); //Required for outlines and points when there's AA turned on on user machine
    fGameplayInterface.Paint;

    glLoadIdentity();
    RenderBrightness(fGameSettings.GetBrightness);

  end else begin

    glLoadIdentity();             // Reset The View
    glLineWidth(1);
    glPointSize(1);
    glkMoveAALines(true); //Required for outlines and points when there's AA turned on on user machine
    fMainMenuInterface.Paint;
    
  end;

  SwapBuffers(h_DC);
end;


procedure TRender.DoPrintScreen(filename:string);
var sh,sw,i,k:integer; jpg: TJpegImage; mkbmp:TBitmap; bmp:array of cardinal;
begin
  sw:=RenderAreaSize.X;
  sh:=RenderAreaSize.Y;

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
  jpg.SaveToFile(filename);

  jpg.Free;
  mkbmp.Free;
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
  for i:=y1 to y2 do for k:=x1 to x2 do begin
    xt:=fTerrain.Land[i,k].Terrain;
    ax:=((xt div 64) mod 2) /2;
    ay:=(xt div 128) /2;

    TexC[1,1]:=(xt mod 8  )/16+ax+Overlap; TexC[1,2]:=(xt mod 64 div 8  )/16+ay+Overlap;
    TexC[2,1]:=(xt mod 8  )/16+ax+Overlap; TexC[2,2]:=(xt mod 64 div 8+1)/16+ay-Overlap;
    TexC[3,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[3,2]:=(xt mod 64 div 8+1)/16+ay-Overlap;
    TexC[4,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[4,2]:=(xt mod 64 div 8  )/16+ay+Overlap;
    TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

    if fTerrain.Land[i,k].Rotation and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
    if fTerrain.Land[i,k].Rotation and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

    glTexCoord2fv(@TexC[TexO[1]]); glvertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
    glTexCoord2fv(@TexC[TexO[2]]); glvertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
    glTexCoord2fv(@TexC[TexO[3]]); glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
    glTexCoord2fv(@TexC[TexO[4]]); glvertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
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

  if fTerrain.Land[i,k].FieldType in [fdt_Road,fdt_HouseRoad] then
    begin
      rd:=byte(fTerrain.Land[max(i-1,1)         ,k                  ].FieldType in [fdt_Road,fdt_HouseRoad])*1 +
          byte(fTerrain.Land[i                  ,min(k+1,MaxMapSize)].FieldType in [fdt_Road,fdt_HouseRoad])*2 +
          byte(fTerrain.Land[min(i+1,MaxMapSize),k                  ].FieldType in [fdt_Road,fdt_HouseRoad])*4 +
          byte(fTerrain.Land[i                  ,max(k-1,1)         ].FieldType in [fdt_Road,fdt_HouseRoad])*8;
      ID:=RoadsConnectivity[rd,1];
      Rot:=RoadsConnectivity[rd,2];
      RenderTile(ID,k,i,Rot);
    end;
end;

  glColor4f(1,1,1,1);
  //Render highlights
  glBlendFunc(GL_DST_COLOR,GL_ONE);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glbegin (GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    begin
      glTexCoord1f(max(0,Land[i  ,k  ].Light)); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k  ].Light)); glvertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k+1].Light)); glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i  ,k+1].Light)); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;                  

  //Render shadows
  glBlendFunc(GL_ZERO,GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glbegin (GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    begin
    glTexCoord1f(max(max(0,-Land[i  ,k  ].Light),1-CheckRevelation(k,i,MyPlayer.PlayerID)));
    glvertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
    glTexCoord1f(max(max(0,-Land[i+1,k  ].Light),1-CheckRevelation(k,i+1,MyPlayer.PlayerID)));
    glvertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
    glTexCoord1f(max(max(0,-Land[i+1,k+1].Light),1-CheckRevelation(k+1,i+1,MyPlayer.PlayerID)));
    glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
    glTexCoord1f(max(max(0,-Land[i  ,k+1].Light),1-CheckRevelation(k+1,i,MyPlayer.PlayerID)));
    glvertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D,0);
end;


procedure TRender.RenderFieldBorders(x1,x2,y1,y2:integer);
var i,k:integer;
begin
for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain do begin
    if Land[i,k].BorderX <> bt_None then
      RenderBorder(Land[i,k].BorderX,1,k,i); //Horizontal

    if Land[i,k].BorderY <> bt_None then
      RenderBorder(Land[i,k].BorderY,2,k,i); //Vertical

    if Land[i,k].Markup in [mu_RoadPlan..mu_WinePlan] then
      RenderMarkup(byte(Land[i,k].Markup),k,i); //Input in range 1..3

  end;
end;


procedure TRender.RenderTerrainObjects(x1,x2,y1,y2,AnimStep:integer);
var i,k:integer;
begin
for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain do begin
    if Land[i,k].FallingTreeAnimStep <> -1 then
    begin
      RenderObject(Land[i,k].Obj+1,Land[i,k].FallingTreeAnimStep,k,i);
    end
    else
    begin
      if Land[i,k].Obj<>255 then RenderObject(Land[i,k].Obj+1,AnimStep,k,i);
      if Land[i,k].FieldSpecial<>fs_None then RenderObjectSpecial(Land[i,k].FieldSpecial,AnimStep,k,i);
    end;
  end;
end;


procedure TRender.RenderWires();
var i,k,t:integer; x1,x2,y1,y2:integer;
begin
  x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
  y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

  for i:=y1 to y2 do begin
    glbegin (GL_LINE_STRIP);
    for k:=x1 to x2 do begin
      glColor4f(0.8,1,0.6,1.2-sqrt(sqr(i-CursorYc)+sqr(k-CursorXc))/10); //Smooth circle gradient blending
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  end;

  glColor4f(0,1,0,0.5);
  t:=Form1.TrackBar1.Position;
  for i:=y1 to y2 do for k:=x1 to x2 do
    if word(fTerrain.Land[i,k].Passability) AND Pow(2,t) = Pow(2,t) then
      RenderQuad(k,i);

  glPointSize(3);
  glbegin (GL_POINTS);
  for i:=y1 to y2 do for k:=x1 to x2 do begin
    //glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
    glColor4f(byte(fTerrain.Land[i,k].BorderX=bt_HousePlan),byte(fTerrain.Land[i,k].BorderY=bt_HousePlan),0,1);
    glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
  end;
  glEnd;
end;


procedure TRender.RenderUnitMoves();
var i,k:integer; x1,x2,y1,y2:integer;
begin
  x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
  y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

  glColor4f(0,1,1,0.5);

  for i:=y1 to y2 do for k:=x1 to x2 do
    if fTerrain.Land[i,k].IsUnit>0 then begin
      glColor4f(fTerrain.Land[i,k].IsUnit/6,1-fTerrain.Land[i,k].IsUnit/6,0,0.8);
      RenderQuad(k,i);
    end;

end;


procedure TRender.RenderRoute(Count:integer; Nodes:array of TKMPoint; Col:TColor4);
var i:integer;
begin
glColor4ubv(@Col);
for i:=1 to Count do
  RenderDotOnTile(Nodes[i-1].X+0.5,Nodes[i-1].Y+0.5);
glBegin(GL_LINE_STRIP);
for i:=1 to Count do
  glVertex2f(Nodes[i-1].X-0.5,Nodes[i-1].Y-0.5-fTerrain.InterpolateLandHeight(Nodes[i-1].X+0.5,Nodes[i-1].Y+0.5)/CELL_HEIGHT_DIV);
glEnd;
end;


procedure TRender.RenderBuildIcon(P:TKMPoint; id:integer=479);
begin
  if fTerrain.TileInMapCoords(P.X,P.Y) then
    RenderSprite(4,id,P.X+0.2,P.Y+1-0.2-fTerrain.InterpolateLandHeight(P.X+0.5,P.Y+0.5)/CELL_HEIGHT_DIV);
end;


procedure TRender.RenderWireQuad(P:TKMPoint; Col:TColor4);
begin
  glColor4ubv(@Col);
  glbegin (GL_LINE_LOOP);
  if fTerrain.TileInMapCoords(P.X,P.Y) then
  with fTerrain do begin
    glvertex2f(p.X-1,p.Y-1-Land[p.Y  ,p.X  ].Height/CELL_HEIGHT_DIV);
    glvertex2f(p.X  ,p.Y-1-Land[p.Y  ,p.X+1].Height/CELL_HEIGHT_DIV);
    glvertex2f(p.X  ,p.Y-  Land[p.Y+1,p.X+1].Height/CELL_HEIGHT_DIV);
    glvertex2f(p.X-1,p.Y-  Land[p.Y+1,p.X  ].Height/CELL_HEIGHT_DIV);
  end;
  glEnd;
end;


procedure TRender.RenderWireHousePlan(P:TKMPoint; aHouseType:THouseType);
var i,k,s,t:integer; P2:TKMPoint; AllowBuild:boolean;
  MarkedLocations:array[1..64] of TKMPoint; //List of locations with special marks on them
  MarkCount:integer;

  procedure MarkPoint(APoint:TKMPoint; AID:integer);
  var v: integer;
  begin
    for v:=1 to MarkCount do if KMSamePoint(MarkedLocations[v],APoint) then exit;
    RenderBuildIcon(APoint,AID);
    inc(MarkCount);
    MarkedLocations[MarkCount] := APoint;
  end;
begin
  MarkCount := 0;
  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
    if fTerrain.TileInMapCoords(P.X+k-3-HouseDAT[byte(aHouseType)].EntranceOffsetX,P.Y+i-4,1) then begin
      P2:=KMPoint(P.X+k-3-HouseDAT[byte(aHouseType)].EntranceOffsetX,P.Y+i-4); //This can't be done earlier since values can be off-map 

        //Check house-specific conditions, e.g. allow shipyards only near water and etc..
        case aHouseType of
          ht_IronMine: AllowBuild := (CanBuildIron in fTerrain.Land[P2.Y,P2.X].Passability);
          ht_GoldMine: AllowBuild := (CanBuildGold in fTerrain.Land[P2.Y,P2.X].Passability);
          ht_Wall:     AllowBuild := (CanWalk      in fTerrain.Land[P2.Y,P2.X].Passability);
          else         AllowBuild := (CanBuild     in fTerrain.Land[P2.Y,P2.X].Passability);
        end;

        //Forbid planning on unrevealed areas
        AllowBuild := AllowBuild and (fTerrain.CheckRevelation(P2.X,P2.Y,MyPlayer.PlayerID)<>0);

        //Check surrounding tiles in +/- 1 range for other houses pressence                                
        for s:=-1 to 1 do for t:=-1 to 1 do
        if (s<>0)or(t<>0) then  //This is a surrounding tile, not the actual tile
        if fTerrain.Land[P2.Y+t,P2.X+s].FieldType in [fdt_HousePlan,fdt_HouseWIP,fdt_House,fdt_HouseRoad] then
        begin
          MarkPoint(KMPoint(P2.X+s,P2.Y+t),479);
          AllowBuild := false;
        end;

        //Mark the tile according to previous check results
        if AllowBuild then begin
          RenderWireQuad(P2,$FFFFFF00); //Cyan
          if HousePlanYX[byte(aHouseType),i,k]=2 then
            MarkPoint(P2,481);
        end else begin
          if HousePlanYX[byte(aHouseType),i,k]=2 then
            MarkPoint(P2,482)
          else
            if aHouseType in [ht_GoldMine,ht_IronMine] then
              MarkPoint(P2,480)
            else
              MarkPoint(P2,479);
        end;

    end else
    if fTerrain.TileInMapCoords(P.X+k-3-HouseDAT[byte(aHouseType)].EntranceOffsetX,P.Y+i-4,0) then
      MarkPoint(KMPoint(P.X+k-3-HouseDAT[byte(aHouseType)].EntranceOffsetX,P.Y+i-4),479);
  end;
end;
             
//@Lewin: I always wanted to ask you 2 things - why??
//
//procedure MarkPoint(APoint:TKMPoint; AID:integer); //Why first letter is 'A', not 'a'
//
//and the 2nd one
//
//if Foo<>Bar then
//begin
//  Foo:=Bar;
//  ...
//end;
//
//Why do you move 'begin' to the next line instead of
//
//if Foo<>Bar then begin
//  Foo:=Bar;
//  ...
//end;
//
//No offence, just curiosity

{
@Krom: First question:  A/a, that was a kind of mistake. I forgot that it should be lower case.
                        I'll use 'a' in the future. Would you like me to change it? (e.g. in LoadDAT)
Second question:
  That's just how I was taught. It still looks odd to me to have the begin right after the then.
  I have always done it like that and so I have to remember not to here. (I am trying to follow your
  coding/formatting standards, which differ greatly from mine) Please let me know if there is anything
  else that I do which you do not like. I am trying to do it your way, but sometimes I forget/make mistakes. ;)
}

procedure TRender.RenderObject(Index,AnimStep,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer; FOW:byte;
begin
  if MapElem[Index].Count=0 then exit;

  FOW:=EnsureRange(round(fTerrain.CheckRevelation(pX,pY,MyPlayer.PlayerID)*255),0,255);
  if FOW = 0 then exit; //Don't render objects which are unexplored
  if FOW <=128 then AnimStep:=0; //Stop animation
  ID:=MapElem[Index].Step[AnimStep mod MapElem[Index].Count +1]+1;
  if ID<=0 then exit;

  if Index=61 then begin //Object 42 is an invisible wall
    glBindTexture(GL_TEXTURE_2D,0);
    glColor4f(1,0,0,1);
    RenderDot(pX,pY);
  end else begin
    ShiftX:=RXData[1].Pivot[ID].x/CELL_SIZE_PX;
    ShiftY:=(RXData[1].Pivot[ID].y+RXData[1].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(1,ID,pX+ShiftX,pY+ShiftY,true,0,-1,FOW);
    {RenderDot(pX,pY);
    glRasterPos2f(pX-1+0.1,pY-1+0.1);
    glPrint(inttostr(Index)+':'+inttostr(ID));}
  end;
end;


{ 4 objects packed on 1 tile for Corn and Grapes }
procedure TRender.RenderObjectSpecial(Fs:TFieldSpecial; AnimStep,pX,pY:integer);
var Index:integer; FOW:byte;
  procedure AddSpriteToListBy(ID:integer; AnimStep:integer; pX,pY:integer; ShiftX,ShiftY:single);
  begin
    ID := MapElem[ID].Step[ AnimStep mod MapElem[ID].Count +1 ] +1;
    ShiftY := ShiftY + (RXData[1].Size[ID,2]) / CELL_SIZE_PX-fTerrain.Land[pY,pX].Height / CELL_HEIGHT_DIV;
    AddSpriteToList(1,ID,pX+ShiftX,pY+ShiftY,true,0,-1,FOW);
  end;
begin
  FOW:=EnsureRange(round(fTerrain.CheckRevelation(pX,pY,MyPlayer.PlayerID)*255),0,255);
  if FOW <=128 then AnimStep:=0; //Stop animation

  case Fs of
    fs_Corn1: Index:=59;
    fs_Corn2: Index:=60;
    fs_Wine1: Index:=55;
    fs_Wine2: Index:=56;
    fs_Wine3: Index:=57;
    fs_Wine4: Index:=58;
    else exit;
  end;
  AddSpriteToListBy(Index, AnimStep  , pX, pY, 0  ,-0.4);
  AddSpriteToListBy(Index, AnimStep+1, pX, pY, 0.5,-0.4);
  if Index in [55..58]then exit;
  AddSpriteToListBy(Index, AnimStep+1, pX, pY, 0  , 0.1);
  AddSpriteToListBy(Index, AnimStep  , pX, pY, 0.5, 0.1);
end;


procedure TRender.RenderMarkup(Index:integer; pX,pY:integer);
var a,b:TKMPointF; ID:integer; FOW, DrawX, DrawY:single;
begin
  case Index of
    1: ID:=105; //Road
    2: ID:=107; //Field
    3: ID:=108; //Wine
    else ID:=0;
  end;
  FOW:=fTerrain.CheckRevelation(pX,pY,MyPlayer.PlayerID);

  glColor4f(FOW,FOW,FOW,1);
  glBindTexture(GL_TEXTURE_2D,GFXData[4,ID].TexID);

  a.x:=GFXData[4,ID].u1; a.y:=GFXData[4,ID].v1;
  b.x:=GFXData[4,ID].u2; b.y:=GFXData[4,ID].v2;

  //Shift the X and Y so that the markup is centred correctly on the tile
  DrawX:=pX; //This one is unchanged, unless you have a screenshot to show
  DrawY:=pY+0.1;
  //@Krom: I guess it's ok like that. I was simply trying to make the red X in placement
  //       centre on the markup.
  //       At the moment it's a little high and to the right IMO, but it's your choice.
  //       It's subtle, but if you compare it, it is different to KaM.
  //       But I'm happy to leave it like this, few people will notice. To be deleted.
  glBegin(GL_QUADS);
    glTexCoord2f(b.x,a.y); glvertex2f(DrawX-1, DrawY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
    glTexCoord2f(a.x,a.y); glvertex2f(DrawX-1, DrawY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(a.x,b.y); glvertex2f(DrawX  , DrawY   - fTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(b.x,b.y); glvertex2f(DrawX  , DrawY   - fTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV);

    glTexCoord2f(b.x,a.y); glvertex2f(DrawX-1, DrawY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
    glTexCoord2f(a.x,a.y); glvertex2f(DrawX-1, DrawY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(a.x,b.y); glvertex2f(DrawX  , DrawY-1 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(b.x,b.y); glvertex2f(DrawX  , DrawY-1 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderBorder(Border:TBorderType; Dir:integer; pX,pY:integer);
var a,b:TKMPointF; ID1,ID2:integer; t:single; HeightInPx:integer; FOW:single;
begin
  ID1:=0; ID2:=0;
  if bt_HouseBuilding = Border then if Dir=1 then ID1:=463 else ID2:=467; //WIP (Wood planks)
  if bt_HousePlan = Border then     if Dir=1 then ID1:=105 else ID2:=117; //Plan (Ropes)
  if bt_Wine = Border then          if Dir=1 then ID1:=462 else ID2:=466; //Fence (Wood)
  if bt_Field = Border then         if Dir=1 then ID1:=461 else ID2:=465; //Fence (Stones)

  FOW:=fTerrain.CheckRevelation(pX,pY,MyPlayer.PlayerID);

  glColor4f(FOW,FOW,FOW,1);
  if Dir = 1 then begin //Horizontal border
    glBindTexture(GL_TEXTURE_2D,GFXData[4,ID1].TexID);
    a.x:=GFXData[4,ID1].u1; a.y:=GFXData[4,ID1].v1;
    b.x:=GFXData[4,ID1].u2; b.y:=GFXData[4,ID1].v2;
    t:=GFXData[4,ID1].PxWidth/CELL_SIZE_PX; //Height of border
    glBegin(GL_QUADS);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1+t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1-t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1-t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1+t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
    glEnd;
  end;

  if Dir = 2 then begin //Vertical border
    glBindTexture(GL_TEXTURE_2D,GFXData[4,ID2].TexID);
    HeightInPx := Round ( CELL_SIZE_PX * (1 + (fTerrain.Land[pY,pX].Height - fTerrain.Land[pY+1,pX].Height)/CELL_HEIGHT_DIV) );
    a.x:=GFXData[4,ID2].u1; a.y:=GFXData[4,ID2].v1;
    b.x:=GFXData[4,ID2].u2; b.y:=GFXData[4,ID2].v2 * (HeightInPx / GFXData[4,ID2].PxHeight);
    t:=GFXData[4,ID2].PxWidth/CELL_SIZE_PX; //Width of border
    glBegin(GL_QUADS);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
    glEnd;
{    glBindTexture(GL_TEXTURE_2D, 0);
    glBegin(GL_LINE_LOOP);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/xh);
      glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
      glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/xh);
    glEnd;}
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


{Render house WIP tablet}
procedure TRender.RenderHouseBuild(Index,pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
  pX:=pX+HouseDAT[Index].EntranceOffsetX;
  ID:=Index+250;
  ShiftX:=RXData[4].Pivot[ID].x/CELL_SIZE_PX+0.5;
  ShiftY:=(RXData[4].Pivot[ID].y+RXData[4].Size[ID,2])/CELL_SIZE_PX+0.5-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(4,ID,pX+ShiftX,pY+ShiftY,true);
end;


{Render house build supply}
procedure TRender.RenderHouseBuildSupply(Index:integer; Wood,Stone:byte; pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
  if Wood<>0 then begin
    ID:=260+Wood-1;
    ShiftX:=HouseDAT[Index].BuildSupply[Wood].MoveX/CELL_SIZE_PX;
    ShiftY:=(HouseDAT[Index].BuildSupply[Wood].MoveY+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false);
  end;
  if Stone<>0 then begin
    ID:=267+Stone-1;
    ShiftX:=HouseDAT[Index].BuildSupply[6+Stone].MoveX/CELL_SIZE_PX;
    ShiftY:=(HouseDAT[Index].BuildSupply[6+Stone].MoveY+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false);
  end;
end;


{Render house in wood}
procedure TRender.RenderHouseWood(Index:integer; Step:single; pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
  ID:=HouseDAT[Index].WoodPic+1;
  ShiftX:=RXData[2].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,true,0,Step);
end;


{Render house in stone}
procedure TRender.RenderHouseStone(Index:integer; Step:single; pX,pY:integer);
var ShiftX,ShiftY:single; ID:integer;
begin
  RenderHouseWood(Index,1,pX,pY); //Render Wood part of it, opaque
  ID:=HouseDAT[Index].StonePic+1;
  ShiftX:=RXData[2].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false,0,Step);
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
          ShiftX:=RXData[2].Pivot[ID].x/CELL_SIZE_PX;
          ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
          ShiftX:=ShiftX+HouseDAT[Index].Anim[AnimType].MoveX/CELL_SIZE_PX;
          ShiftY:=ShiftY+HouseDAT[Index].Anim[AnimType].MoveY/CELL_SIZE_PX;
          AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false,Owner);
        end;
    end;
  end;
end;


procedure TRender.RenderHouseSupply(Index:integer; R1,R2:array of byte; pX,pY:integer);
var ShiftX,ShiftY:single; ID,i:integer;
begin
for i:=1 to 4 do if (R1[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyIn[i,min(R1[i-1],5)]+1;
    if ID>0 then begin
    ShiftX:=RXData[2].Pivot[ID].x/CELL_SIZE_PX;
    ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false);
    end;
    end;
for i:=1 to 4 do if (R2[i-1])>0 then begin
    ID:=HouseDAT[Index].SupplyOut[i,min(R2[i-1],5)]+1;
    if ID>0 then begin
    ShiftX:=RXData[2].Pivot[ID].x/CELL_SIZE_PX;
    ShiftY:=(RXData[2].Pivot[ID].y+RXData[2].Size[ID,2])/CELL_SIZE_PX-fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,pX+ShiftX,pY+ShiftY,false);
    end;
    end;
end;

procedure TRender.RenderUnit(UnitID,ActID,DirID,StepID,Owner:integer; pX,pY:single; NewInst:boolean);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Count;
ID:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID,2])/CELL_SIZE_PX;

  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,NewInst,Owner);

  if not MakeShowUnitMove then exit;
  glColor3ubv(@TeamColors[Owner]);  //Render dot where unit is
  RenderDot(pX,pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
end;

procedure TRender.RenderUnitCarry(CarryID,DirID,StepID,Owner:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
AnimSteps:=SerfCarry[CarryID].Dir[DirID].Count;
ID:=SerfCarry[CarryID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
if ID<=0 then exit;
  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID,2])/CELL_SIZE_PX;
  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4;
  ShiftX:=ShiftX+SerfCarry[CarryID].Dir[DirID].MoveX/CELL_SIZE_PX;
  ShiftY:=ShiftY+SerfCarry[CarryID].Dir[DirID].MoveY/CELL_SIZE_PX;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,false,Owner);
end;


{Simple dot to know where it actualy is}
procedure TRender.RenderDot(pX,pY:single);
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  glBegin (GL_QUADS);
    glkRect(pX-1,pY-1,pX-1+0.1,pY-1-0.1);
  glEnd;
end;

procedure TRender.RenderDotOnTile(pX,pY:single);
begin
  pY:=pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV;
  glBindTexture(GL_TEXTURE_2D, 0);
  glBegin (GL_QUADS);
    glkRect(pX-1,pY-1,pX-1+0.1,pY-1-0.1);
  glEnd;
end;


{Used for internal things like overlays, etc..}
procedure TRender.RenderQuad(pX,pY:integer);
begin
glbegin (GL_QUADS);
if fTerrain.TileInMapCoords(pX,pY) then
with fTerrain do begin
  glkQuad(pX-1,pY-1-Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV,
          pX  ,pY-1-Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV,
          pX  ,pY-  Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV,
          pX-1,pY-  Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV);
end;
glEnd;
end;

{Render one terrian cell}
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

TexC[1,1]:=(xt mod 8  )/16+ax+Overlap; TexC[1,2]:=(xt mod 64 div 8  )/16+ay+Overlap;
TexC[2,1]:=(xt mod 8  )/16+ax+Overlap; TexC[2,2]:=(xt mod 64 div 8+1)/16+ay-Overlap;
TexC[3,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[3,2]:=(xt mod 64 div 8+1)/16+ay-Overlap;
TexC[4,1]:=(xt mod 8+1)/16+ax-Overlap; TexC[4,2]:=(xt mod 64 div 8  )/16+ay+Overlap;
TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

k:=pX; i:=pY;
glbegin (GL_QUADS);
with fTerrain do begin
  glTexCoord2fv(@TexC[TexO[1]]); glvertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
  glTexCoord2fv(@TexC[TexO[2]]); glvertex2f(k-1,i-Land[i+1,k].Height/CELL_HEIGHT_DIV);
  glTexCoord2fv(@TexC[TexO[3]]); glvertex2f(k,i-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
  glTexCoord2fv(@TexC[TexO[4]]); glvertex2f(k,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
end;
glEnd;
glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderSprite(RX:byte; ID:word; pX,pY:single; const Col:TColor4 = $FF; const aFOW:byte=$FF);
var h:integer;
begin
for h:=1 to 2 do
  with GFXData[RX,ID] do begin
    if h=1 then begin
      glColor4f(1,1,1,1);
      glColor4ub(aFOW,aFOW,aFOW,255);
      glBindTexture(GL_TEXTURE_2D, TexID);
    end else
      if (h=2) and (RXData[RX].NeedTeamColors) and (AltID<>0) then begin
        glColor3ubv(@Col);
        glBindTexture(GL_TEXTURE_2D, AltID);
        //glBlendFunc(GL_DST_COLOR,GL_SRC_COLOR);
      end else
        exit;

    glBegin (GL_QUADS);
    glTexCoord2f(u1,v2); glvertex2f(pX-1                     ,pY-1         );
    glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
    glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
    glTexCoord2f(u1,v1); glvertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    {glBegin (GL_LINE_LOOP);
    glRect(pX-1,pY-1,pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;}
  end;
end;


procedure TRender.RenderSpriteAlphaTest(RX:byte; ID:word; Param:single; pX,pY:single; const Col:TColor4=$FF);
begin
//if Param<1 then begin
  //NOTION: This function does not work on some GPUs will need to replace it with simplier more complicated way
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER,1-Param);
  glBlendFunc(GL_ONE,GL_ZERO);

{end else begin
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_ALWAYS,0);
  glBlendFunc(GL_ONE,GL_ZERO);}
//end;

  with GFXData[RX,ID] do begin
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
    glTexCoord2f(u1,v2); glvertex2f(pX-1                     ,pY-1         );
    glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
    glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
    glTexCoord2f(u1,v1); glvertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
    //glBegin (GL_LINE_LOOP);
    //glRect(pX-1,pY-1,pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
    //glEnd;
  end;
glDisable(GL_ALPHA_TEST);
glAlphaFunc(GL_ALWAYS,0);
glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
end;


{Collect all sprites into list}
procedure TRender.AddSpriteToList(aRX:byte; aID:word; pX,pY:single; aNew:boolean; const aTeam:byte=0; const Step:single=-1; const aFOW:byte=$FF);
begin
inc(RenderCount);
if length(RenderList)-1<RenderCount then setlength(RenderList,length(RenderList)+32); //Book some space

RenderList[RenderCount].Loc:=KMPointF(pX,pY); //Position of sprite, floating-point
RenderList[RenderCount].RX:=aRX;              //RX library
RenderList[RenderCount].ID:=aID;              //Texture ID
RenderList[RenderCount].NewInst:=aNew;        //Is this a new item (can be occluded), or a child one (always on top)
RenderList[RenderCount].Team:=aTeam;          //Team ID (determines color)
RenderList[RenderCount].AlphaStep:=Step;      //Alpha step for wip buildings
RenderList[RenderCount].FOWvalue:=aFOW;       //Visibility
end;


{Need to sort all items in list from top-right to bottom-left}
procedure TRender.SortRenderList;
var i,k,t:integer;
begin
setlength(RO,RenderCount+1);

for i:=1 to RenderCount do //Mark child sprites with 0
if RenderList[i].NewInst then RO[i]:=i else RO[i]:=0;

for i:=1 to RenderCount do if i<>0 then //Exclude child sprites from comparision
  for k:=i+1 to RenderCount do if k<>0 then

    if RenderList[RO[k]].Loc.Y < RenderList[RO[i]].Loc.Y then begin //TopMost
      t:=RO[k]; RO[k]:=RO[i]; RO[i]:=t;
    end else

    if RenderList[RO[k]].Loc.Y = RenderList[RO[i]].Loc.Y then
    if RenderList[RO[k]].Loc.X > RenderList[RO[i]].Loc.X then begin //Rightmost
      t:=RO[k]; RO[k]:=RO[i]; RO[i]:=t;
    end;

end;


{Now render all these items from list}
procedure TRender.RenderRenderList;
var i,h:integer;
begin
for i:=1 to RenderCount do
if RO[i]<>0 then begin
  h:=RO[i];
  if not MakeGameSprites then
    RenderDot(RenderList[h].Loc.X,RenderList[h].Loc.Y)
  else
  repeat //Render child sprites only after their parent
    with RenderList[h] do begin
      if AlphaStep=-1 then
        if Team<>0 then
          RenderSprite(RX,ID,Loc.X,Loc.Y,TeamColors[Team])
        else
          RenderSprite(RX,ID,Loc.X,Loc.Y,$FF0000FF,FOWvalue)
      else
        RenderSpriteAlphaTest(RX,ID,AlphaStep,Loc.X,Loc.Y,$FF)
    end;
    inc(h);
  until((h>RenderCount)or(RenderList[h].NewInst));
end;
setlength(RenderList,0);
setlength(RO,0);
end;


procedure TRender.RenderCursorHighlights;
begin
with fTerrain do
case CursorMode.Mode of
  cm_None:;
  cm_Erase:if ((CanRemovePlan(CursorPos,MyPlayer.PlayerID)) or CanRemoveHouse(CursorPos,MyPlayer.PlayerID))
               and (CheckRevelation(CursorPos.X,CursorPos.Y,MyPlayer.PlayerID)<>0) then
             fRender.RenderWireQuad(CursorPos, $FFFFFF00) //Cyan quad
           else fRender.RenderBuildIcon(CursorPos);       //Red X
  cm_Road: if (CanPlaceRoad(CursorPos,mu_RoadPlan)) and (CheckRevelation(CursorPos.X,CursorPos.Y,MyPlayer.PlayerID)<>0) then
             fRender.RenderWireQuad(CursorPos, $FFFFFF00) //Cyan quad
           else fRender.RenderBuildIcon(CursorPos);       //Red X
  cm_Field: if (CanPlaceRoad(CursorPos,mu_FieldPlan)) and (CheckRevelation(CursorPos.X,CursorPos.Y,MyPlayer.PlayerID)<>0) then
             fRender.RenderWireQuad(CursorPos, $FFFFFF00) //Cyan quad
           else fRender.RenderBuildIcon(CursorPos);       //Red X
  cm_Wine: if (CanPlaceRoad(CursorPos,mu_WinePlan)) and (CheckRevelation(CursorPos.X,CursorPos.Y,MyPlayer.PlayerID)<>0) then
             fRender.RenderWireQuad(CursorPos, $FFFFFF00) //Cyan quad
           else fRender.RenderBuildIcon(CursorPos);       //Red X
  cm_Houses: fRender.RenderWireHousePlan(CursorPos, THouseType(CursorMode.Param)); //Cyan quad
end;
end;


//Render highlight overlay to make whole picture look brighter
procedure TRender.RenderBrightness(Value:byte);
begin
  Value:=Value-1;
  glBlendFunc(GL_DST_ALPHA,GL_DST_ALPHA);
  glColor4f(Value/20,Value/20,Value/20,Value/20);
  glBegin(GL_QUADS);
    glkRect(0,0,RenderAreaSize.X,RenderAreaSize.Y);
  glEnd;
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

end.
