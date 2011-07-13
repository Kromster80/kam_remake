unit KM_Render;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, glut, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Buttons,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  {$IFDEF WDC} JPEG, {$ENDIF} //Lazarus doesn't have JPEG library yet
  KM_TGATexture, KM_Defaults, KM_Utils, KM_CommonTypes;

{$IFDEF Unix}
type HDC = integer;
type HGLRC = integer;
type HWND = integer;
type FW_BOLD = integer;
{$ENDIF}

type
  TRender = class
    private
      h_DC: HDC;
      h_RC: HGLRC;
      fOpenGL_Vendor, fOpenGL_Renderer, fOpenGL_Version:string;
      TextG:GLuint; //Shading gradient
      TextT:GLuint; //Tiles
      TextW:array[1..8]of GLuint; //Water
      TextS:array[1..3]of GLuint; //Swamps
      TextF:array[1..5]of GLuint; //WaterFalls
      fRenderAreaSize:TKMPoint;

      rPitch,rHeading,rBank:integer;

      RenderCount:word;
      RO:array of word; //RenderOrder
      RenderList:array of record
        Loc,Obj:TKMPointF;
        RX:byte;
        ID:word;
        NewInst,IsUnit:boolean;
        Team:cardinal;
        AlphaStep:single; //Only appliable to HouseBuild
        FOWvalue:byte; // Fog of War thickness
      end;

      procedure RenderDot(pX,pY:single; Size:single = 0.05);
      procedure RenderDotOnTile(pX,pY:single);
      procedure RenderLine(x1,y1,x2,y2:single);
      procedure RenderQuad(pX,pY:integer);
      procedure RenderTile(Index:byte; pX,pY,Rot:integer);
      procedure RenderSprite(RX:byte; ID:word; pX,pY:single; Col:TColor4; aFOW:byte; HighlightRed: boolean=false);
      procedure RenderSpriteAlphaTest(RX:byte; ID:word; Param:single; pX,pY:single; aFOW:byte);
      procedure AddSpriteToList(aRX:byte; aID:word; pX,pY,oX,oY:single; aNew:boolean; const aTeam:cardinal=$00000000; const Step:single=-1; aIsUnit:boolean=false);
      procedure ClipRenderList;
      procedure SortRenderList;
      procedure RenderRenderList;
      procedure RenderTerrainMarkup(Index:integer; pX,pY:integer);
      procedure RenderTerrainBorder(Border:TBorderType; Pos:TKMDirection; pX,pY:integer);
      procedure RenderCursorWireQuad(P:TKMPoint; Col:TColor4);
      procedure RenderCursorBuildIcon(P:TKMPoint; id:integer=479);
      procedure RenderCursorWireHousePlan(P:TKMPoint; aHouseType:THouseType);
      procedure RenderCursorHighlights;
      procedure RenderBrightness(Value:byte);
    public
      Stat_Sprites:integer; //Total sprites in queue
      Stat_Sprites2:integer;//Rendered sprites
      constructor Create(RenderFrame:HWND; aVSync:boolean);
      destructor Destroy; override;
      procedure LoadTileSet;
      procedure SetRotation(aH,aP,aB:integer);
      procedure ResizeGameArea(Width,Height:integer; aRenderMode:TRenderMode);
      procedure Render;
      {$IFDEF WDC}
      procedure DoPrintScreen(FileName:string);
      {$ENDIF}
      procedure RenderTerrain(x1,x2,y1,y2,AnimStep:integer);
      procedure RenderTerrainFieldBorders(x1,x2,y1,y2:integer);
      procedure RenderTerrainObjects(x1,x2,y1,y2,AnimStep:integer);
      procedure RenderDebugCircle(x,y,rad:single; Fill,Line:TColor4);
      procedure RenderDebugLine(x1,y1,x2,y2:single);
      procedure RenderDebugProjectile(x1,y1,x2,y2:single);
      procedure RenderDebugWires(x1,x2,y1,y2:integer);
      procedure RenderDebugPassability(x1,x2,y1,y2:integer);
      procedure RenderDebugUnitPointers(pX,pY:single; Count:integer);
      procedure RenderDebugUnitMoves(x1,x2,y1,y2:integer);
      procedure RenderDebugUnitRoute(NodeList:TKMPointList; Pos:integer; aUnitType:byte);
      procedure RenderDebugQuad(pX,pY:integer);
      procedure RenderDebugText(pX,pY:integer; aText:string; aCol:TColor4);
      procedure RenderProjectile(aProj:TProjectileType; pX,pY:single; Flight:single; Dir:TKMDirection=dir_NA);
      procedure RenderObjectOrQuad(Index,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
      procedure RenderObject(Index,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
      procedure RenderObjectQuad(Index:integer; AnimStep,pX,pY:integer; IsDouble:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
      procedure RenderHouseBuild(Index:THouseType; Loc:TKMPoint);
      procedure RenderHouseBuildSupply(Index:THouseType; Wood,Stone:byte; Loc:TKMPoint);
      procedure RenderHouseWood(Index:THouseType; Step:single; Loc:TKMPoint);
      procedure RenderHouseStone(Index:THouseType; Step:single; Loc:TKMPoint);
      procedure RenderHouseWork(Index:THouseType; AnimType,AnimStep:cardinal; Loc:TKMPoint; FlagColor:TColor4);
      procedure RenderHouseSupply(Index:THouseType; const R1,R2:array of byte; Loc:TKMPoint);
      procedure RenderHouseStableBeasts(Index:THouseType; BeastID,BeastAge,AnimStep:integer; Loc:TKMPoint);
      procedure RenderUnit(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
      procedure RenderUnitCarry(CarryID,DirID,StepID:integer; pX,pY:single);
      procedure RenderUnitThought(Thought:TUnitThought; pX,pY:single);
      procedure RenderUnitFlag(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; UnitX,UnitY:single; NewInst:boolean);
      procedure RenderUnitWithDefaultArm(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
      property RenderAreaSize:TKMPoint read fRenderAreaSize;
      property RendererVersion:string read fOpenGL_Version;
    end;

    
var
  fRender: TRender;


implementation
uses KM_Terrain, KM_Viewport, KM_PlayersCollection, KM_Game, KM_Sound, KM_ResourceGFX, KM_Units;


constructor TRender.Create(RenderFrame:HWND; aVSync:boolean);
begin
  Inherited Create;
  {$IFDEF MSWindows}
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  glDisable(GL_LIGHTING); //We don't need it

  fOpenGL_Vendor   := glGetString(GL_VENDOR);   fLog.AddToLog('OpenGL Vendor:  '  +fOpenGL_Vendor);
  fOpenGL_Renderer := glGetString(GL_RENDERER); fLog.AddToLog('OpenGL Renderer:  '+fOpenGL_Renderer);
  fOpenGL_Version  := glGetString(GL_VERSION);  fLog.AddToLog('OpenGL Version:  ' +fOpenGL_Version);

  SetupVSync(aVSync);

  BuildFont(h_DC, 16, FW_BOLD);

  setlength(RenderList,512);
  {$ENDIF}
  {$IFDEF Unix}
    MessageBox(Form1.Handle,'Trender.Create not working', 'Error', MB_OK);
  {$ENDIF}
end;


destructor TRender.Destroy;
begin
  setlength(RenderList,0);
  {$IFDEF MSWindows}
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
  {$ENDIF}
  {$IFDEF Unix}
  //do not know how to fix them :(
  //just error for now
  MessageBox(Form1.Handle,'glXMakeCurrent and glXDestroyContext not working', 'Error', MB_OK);
  //glXMakeCurrent(display, wid, util_glctx);
  //glXDestroyContext(h_RC);
  {$ENDIF}
  Inherited;
end;


// Load the Textures
procedure TRender.LoadTileSet;
var i:integer;
begin
  LoadTexture(ExeDir+'Resource\gradient.tga', TextG);
  LoadTexture(ExeDir+'Resource\Tiles1.tga', TextT);
  fResource.MakeTileGFXFromTexture(TextT);
  if MAKE_ANIM_TERRAIN then begin
    for i:=1 to 8 do LoadTexture(ExeDir+'Resource\Water'+inttostr(i)+'.tga', TextW[i]);
    for i:=1 to 3 do LoadTexture(ExeDir+'Resource\Swamp'+inttostr(i)+'.tga', TextS[i]);
    for i:=1 to 5 do LoadTexture(ExeDir+'Resource\Falls'+inttostr(i)+'.tga', TextF[i]);
  end;
end;


procedure TRender.SetRotation(aH,aP,aB:integer);
begin
  rHeading := aH;
  rPitch   := aP;
  rBank    := aB;
end;


procedure TRender.ResizeGameArea(Width,Height:integer; aRenderMode:TRenderMode);
begin
  if Height=0 then Height:=1;
  if Width=0  then Width :=1;
  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                   // Reset View
  if aRenderMode=rm2D then
    gluOrtho2D(0,Width,Height,0)
  else
    gluPerspective(80, -Width/Height, 0.1, 5000.0);
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity;                   // Reset View
  fRenderAreaSize.X := Width;
  fRenderAreaSize.Y := Height;
end;


procedure TRender.Render;
begin
  glClear(GL_COLOR_BUFFER_BIT);    // Clear The Screen, can save some FPS on this one

  if fGame.GameState in [gsPaused, gsOnHold, gsRunning, gsReplay, gsEditor] then
  begin //If game is running
    glLoadIdentity; // Reset The View
    //glRotate(-15,0,0,1); //Funny thing
    glTranslatef(fViewport.ViewWidth/2, fViewport.ViewHeight/2, 0);
    glkScale(fViewport.Zoom*CELL_SIZE_PX);
    glTranslatef(-fViewport.GetCenter.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fViewport.Zoom, -fViewport.GetCenter.Y, 0);
    if RENDER_3D then
    begin
      glLoadIdentity;
      ResizeGameArea(fRenderAreaSize.X,fRenderAreaSize.Y,rm3D);

      glkScale(-CELL_SIZE_PX/14);
      glRotatef(rHeading,1,0,0);
      glRotatef(rPitch  ,0,1,0);
      glRotatef(rBank   ,0,0,1);
      glTranslatef(-fViewport.GetCenter.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fViewport.Zoom, -fViewport.GetCenter.Y-8, 10);
      glkScale(fViewport.Zoom);
      ResizeGameArea(fRenderAreaSize.X, fRenderAreaSize.Y, rm2D);
    end;

    glLineWidth(fViewport.Zoom*2);
    glPointSize(fViewport.Zoom*5);

    RenderCount := 0; //Init RenderList

    fTerrain.Paint;
    fPlayers.Paint; //Quite slow           //Units and houses
    if fGame.GameState in [gsPaused, gsOnHold, gsRunning, gsReplay] then
      fGame.Projectiles.Paint; //Render all arrows and etc..

    ClipRenderList; //drop items that are outside of viewport
    SortRenderList; //sort items overlaying
    RenderRenderList;

    RenderCursorHighlights; //Will be on-top of everything

    if DISPLAY_SOUNDS then fSoundLib.Paint;
  end;

  glLoadIdentity;             // Reset The View
  glLineWidth(1);
  glPointSize(1);
  glkMoveAALines(true); //Required for outlines and points when there's AA turned on on user machine
  fGame.PaintInterface;

  glLoadIdentity;
  RenderBrightness(fGame.GlobalSettings.Brightness);

  glFinish;
  {$IFDEF MSWindows}
  SwapBuffers(h_DC);
  {$ENDIF}
  {$IFDEF Unix}
  glutswapbuffers;
  {$ENDIF}
end;


{$IFDEF WDC}
procedure TRender.DoPrintScreen(FileName:string);
var sh,sw,i,k:integer; jpg: TJpegImage; mkbmp:TBitmap; bmp:array of cardinal;
begin
  sw := fRenderAreaSize.X;
  sh := fRenderAreaSize.Y;

  setlength(bmp,sw*sh+1);
  glReadPixels(0,0,sw,sh,GL_BGRA,GL_UNSIGNED_BYTE,@bmp[0]);

  //Mirror verticaly
  for i:=0 to (sh div 2)-1 do for k:=0 to sw-1 do
  SwapInt(bmp[i*sw+k],bmp[((sh-1)-i)*sw+k]);

  mkbmp := TBitmap.Create;
  mkbmp.Handle:=CreateBitmap(sw,sh,1,32,@bmp[0]);

  jpg := TJpegImage.Create;
  jpg.assign(mkbmp);
  jpg.ProgressiveEncoding := true;
  jpg.ProgressiveDisplay  := true;
  jpg.Performance         := jpBestQuality;
  jpg.CompressionQuality  := 90;
  jpg.Compress;
  jpg.SaveToFile(FileName);

  jpg.Free;
  mkbmp.Free;
end;
{$ENDIF}


procedure TRender.RenderTerrain(x1,x2,y1,y2,AnimStep:integer);
var
  Lay2:boolean;
  i,k,iW:integer; ID,Rot,rd:integer;
  xt,a:integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
  glColor4f(1,1,1,1);

  for iW:=1 to 1+3*byte(MAKE_ANIM_TERRAIN) do begin //Each new layer inflicts 10% fps drop
    case iW of
      1: glBindTexture(GL_TEXTURE_2D, TextT);
      2: glBindTexture(GL_TEXTURE_2D, TextW[AnimStep mod 8 + 1]); 
      3: glBindTexture(GL_TEXTURE_2D, TextS[AnimStep mod 24 div 8 + 1]); //These should be unsynced later on
      4: glBindTexture(GL_TEXTURE_2D, TextF[AnimStep mod 5 + 1]);
    end;
    glBegin (GL_QUADS);
      with fTerrain do
      for i:=y1 to y2 do for k:=x1 to x2 do
      if (iW=1) or (MyPlayer.FogOfWar.CheckTileRevelation(k,i) > FOG_OF_WAR_ACT) then //No animation in FOW
      begin
        xt:=fTerrain.Land[i,k].Terrain; 

        if KAM_WATER_DRAW and (iW=1) and (xt in [192,193,196]) then begin
          Lay2:=true;
          xt:=32;
        end else
          Lay2:=false;

        TexC[1,1]:=(xt mod 16  )/16; TexC[1,2]:=(xt div 16  )/16;
        TexC[2,1]:=(xt mod 16  )/16; TexC[2,2]:=(xt div 16+1)/16;
        TexC[3,1]:=(xt mod 16+1)/16; TexC[3,2]:=(xt div 16+1)/16;
        TexC[4,1]:=(xt mod 16+1)/16; TexC[4,2]:=(xt div 16  )/16;

        TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

        if fTerrain.Land[i,k].Rotation and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
        if fTerrain.Land[i,k].Rotation and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

        if RENDER_3D then begin
          glTexCoord2fv(@TexC[TexO[1]]); glvertex3f(k-1,i-1,-Land[i,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[2]]); glvertex3f(k-1,i  ,-Land[i+1,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[3]]); glvertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[4]]); glvertex3f(k  ,i-1,-Land[i,k+1].Height/CELL_HEIGHT_DIV);
        end else begin
        if Lay2 then glColor4f(1,1,1,Land[i,k].Height/CELL_HEIGHT_DIV+0.5)
        else glColor4f(1,1,1,1);
          glTexCoord2fv(@TexC[TexO[1]]); glvertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
        if Lay2 then glColor4f(1,1,1,Land[i+1,k].Height/CELL_HEIGHT_DIV+0.5)
        else glColor4f(1,1,1,1);
          glTexCoord2fv(@TexC[TexO[2]]); glvertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
        if Lay2 then glColor4f(1,1,1,Land[i+1,k+1].Height/CELL_HEIGHT_DIV+0.5)
        else glColor4f(1,1,1,1);
          glTexCoord2fv(@TexC[TexO[3]]); glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
        if Lay2 then glColor4f(1,1,1,Land[i,k+1].Height/CELL_HEIGHT_DIV+0.5)
        else glColor4f(1,1,1,1);
          glTexCoord2fv(@TexC[TexO[4]]); glvertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
        end;
      end;
    glEnd;
  end;

  for i:=y1 to y2 do for k:=x1 to x2 do
  begin
    case fTerrain.Land[i,k].TileOverlay of
      to_Dig1: RenderTile(249,k,i,0);
      to_Dig2: RenderTile(251,k,i,0);
      to_Dig3: RenderTile(253,k,i,0);
      to_Dig4: RenderTile(255,k,i,0);
      to_Wall: begin
                 glColor4f(0.5,0,0,0.5);
                 RenderQuad(k,i);
               end;
    end;

    if fTerrain.Land[i,k].TileOverlay=to_Road then
      begin
        rd:=byte(fTerrain.Land[max(i-1,1)           ,k                    ].TileOverlay=to_Road) shl 0 +
            byte(fTerrain.Land[i                    ,min(k+1,MAX_MAP_SIZE)].TileOverlay=to_Road) shl 1 +
            byte(fTerrain.Land[min(i+1,MAX_MAP_SIZE),k                    ].TileOverlay=to_Road) shl 2 +
            byte(fTerrain.Land[i                    ,max(k-1,1)           ].TileOverlay=to_Road) shl 3;
        ID  := RoadsConnectivity[rd,1];
        Rot := RoadsConnectivity[rd,2];
        RenderTile(ID,k,i,Rot);
      end;
  end;

  glColor4f(1,1,1,1);
  //Render highlights
  glBlendFunc(GL_DST_COLOR,GL_ONE);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glBegin (GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    if RENDER_3D then begin
      glTexCoord1f(max(0,Land[i  ,k  ].Light)); glvertex3f(k-1,i-1,-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k  ].Light)); glvertex3f(k-1,i  ,-Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k+1].Light)); glvertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i  ,k+1].Light)); glvertex3f(k  ,i-1,-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord1f(max(0,Land[i  ,k  ].Light)); glvertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k  ].Light)); glvertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k+1].Light)); glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i  ,k+1].Light)); glvertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;

  //Render shadows and FOW at once
  glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, TextG);
  glBegin (GL_QUADS);
    with fTerrain do
    for i:=y1 to y2 do for k:=x1 to x2 do
    if RENDER_3D then begin
      glTexCoord1f(kromutils.max(0,-Land[i  ,k  ].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i)/255));
      glvertex3f(k-1,i-1,-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i+1,k  ].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i+1)/255));
      glvertex3f(k-1,i  ,-Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i+1,k+1].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i+1)/255));
      glvertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i  ,k+1].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i)/255));
      glvertex3f(k  ,i-1,-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord1f(kromutils.max(0, -Land[i  ,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i)/255));
      glvertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i+1,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i+1)/255));
      glvertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i+1,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i+1)/255));
      glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i  ,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i)/255));
      glvertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D,0);

  if SHOW_WALK_CONNECT then
  for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain do
  begin
    glColor4f(Land[i,k].WalkConnect[wcWalk]/8,Land[i,k].WalkConnect[wcRoad]/8,0,0.5);
    RenderQuad(k,i)
  end;

  //if SHOW_MAP_AREAS then
  {for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain do
  begin
    glColor4f(byte(PatternDAT[Land[i,k].Terrain+1].u2 and 1 = 1),
              byte(PatternDAT[Land[i,k].Terrain+1].u2 and 2 = 2),
              byte(PatternDAT[Land[i,k].Terrain+1].u2 and 4 = 4),0.5);
    RenderQuad(k,i)
  end;}
end;


procedure TRender.RenderTerrainFieldBorders(x1,x2,y1,y2:integer);
var i,k:integer;
begin
  for i:=y1 to y2 do
  for k:=x1 to x2 do
  with fTerrain do
  begin
    if Land[i,k].BorderTop then RenderTerrainBorder(Land[i,k].Border,dir_N,k,i);
    if Land[i,k].BorderLeft then RenderTerrainBorder(Land[i,k].Border,dir_E,k,i);
    if Land[i,k].BorderRight then RenderTerrainBorder(Land[i,k].Border,dir_W,k,i);
    if Land[i,k].BorderBottom then RenderTerrainBorder(Land[i,k].Border,dir_S,k,i);

    if Land[i,k].Markup in [mu_RoadPlan..mu_WallPlan] then
      RenderTerrainMarkup(byte(Land[i,k].Markup),k,i); //Input in range 1..3
  end;
end;


procedure TRender.RenderTerrainObjects(x1,x2,y1,y2,AnimStep:integer);
var i,k:integer;
begin
  for i:=y1 to y2 do for k:=x1 to x2 do
  if fTerrain.Land[i,k].Obj<>255 then
    RenderObjectOrQuad(fTerrain.Land[i,k].Obj+1,AnimStep,k,i);

  with fTerrain.FallingTrees do
  for i:=1 to Count do begin
    RenderObject(Tag[i]+1,AnimStep-Tag2[i],List[i].X,List[i].Y);
    fLog.AssertToLog(AnimStep-Tag2[i] <= 100,'Falling tree overrun?');
  end;
end;


procedure TRender.RenderDebugCircle(x,y,rad:single; Fill,Line:TColor4);
const SEC_COUNT=20;
var i:integer;
begin
  X := X - 0.5;
  Y := Y - 1 - fTerrain.InterpolateLandHeight(X,Y)/CELL_HEIGHT_DIV;
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


procedure TRender.RenderDebugLine(x1,y1,x2,y2:single);
begin
  glColor4f(1.0, 0.75, 1.0, 1.0);
  RenderDot(x1,y1);
  RenderDot(x2,y2);
  RenderLine(x1,y1,x2,y2);
end;


procedure TRender.RenderDebugProjectile(x1,y1,x2,y2:single);
begin
  glColor4f(1, 1, 0, 1);
  RenderDot(x1,y1);
  glColor4f(1, 0, 0, 1);
  RenderDot(x2,y2,0.1);
  RenderLine(x1,y1,x2,y2);
end;


procedure TRender.RenderDebugWires(x1,x2,y1,y2:integer);
var i,k:integer;
begin
  for i:=y1 to y2 do begin
    glBegin (GL_LINE_STRIP);
    for k:=x1 to x2 do begin
      glColor4f(0.8,1,0.6,1.2-sqrt(sqr(i-GameCursor.Cell.Y)+sqr(k-GameCursor.Cell.X))/10); //Smooth circle gradient blending
      glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
    end;
    glEnd;
  end;

  glPointSize(3);
  glBegin (GL_POINTS);
  for i:=y1 to y2 do for k:=x1 to x2 do begin
    //glColor4f(fTerrain.Land[i,k].Height/100,0,0,1.2-sqrt(sqr(i-MapYc)+sqr(k-MapXc))/10);
    glColor4f(byte(fTerrain.Land[i,k].Border=bt_HousePlan),byte(fTerrain.Land[i,k].Border=bt_HousePlan),0,1);
    glvertex2f(k-1,i-1-fTerrain.Land[i,k].Height/CELL_HEIGHT_DIV);
  end;
  glEnd;
end;


procedure TRender.RenderDebugPassability(x1,x2,y1,y2:integer);
var i,k,Passability:integer;
begin
  Passability := fGame.FormPassability;
  if fGame.fMapEditorInterface <> nil then
    Passability := max(Passability, fGame.fMapEditorInterface.ShowPassability);

  if Passability <> 0 then
  begin
    glColor4f(0,1,0,0.25);
    for i:=y1 to y2 do for k:=x1 to x2 do
      {$IFDEF WDC}
      if word(fTerrain.Land[i,k].Passability) AND (1 shl Passability) = (1 shl Passability) then
      {$ENDIF}
      {$IFDEF FPC} //Can't accept word
      if integer(fTerrain.Land[i,k].Passability) AND (1 shl Passability) = (1 shl Passability) then
      {$ENDIF}
        RenderQuad(k,i);
  end;
end;


procedure TRender.RenderDebugQuad(pX,pY:integer);
begin
  glColor4f(1,1,1,0.15);
  RenderQuad(pX,pY);
end;


procedure TRender.RenderDebugText(pX,pY:integer; aText:string; aCol:TColor4);
begin
  glColor4ubv(@aCol);
  glRasterPos2f(pX - 0.5,pY - 1 - fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
  glPrint(aText);
end;


procedure TRender.RenderDebugUnitPointers(pX,pY:single; Count:integer);
var i:integer;
begin
  for i:=1 to Count do
    RenderDot(pX+i/5,pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
end;


procedure TRender.RenderDebugUnitMoves(x1,x2,y1,y2:integer);
var i,k:integer; VertexUsage: byte;
begin
  for i:=y1 to y2 do for k:=x1 to x2 do begin
    if fTerrain.Land[i,k].IsVertexUnit<>vu_None then begin
      VertexUsage := byte(fTerrain.Land[i,k].IsVertexUnit);
      glColor4f(1-VertexUsage/3,VertexUsage/3,0.6,0.8);
      RenderDot(k,i-fTerrain.InterpolateLandHeight(k,i)/CELL_HEIGHT_DIV,0.3);
    end;
    if fTerrain.Land[i,k].IsUnit<>nil then begin
      glColor4f(0.17,0.83,0,0.8);
      RenderQuad(k,i);
    end;
  end;
end;


procedure TRender.RenderDebugUnitRoute(NodeList:TKMPointList; Pos:integer; aUnitType:byte);
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


procedure TRender.RenderProjectile(aProj:TProjectileType; pX,pY:single; Flight:single; Dir:TKMDirection=dir_NA);
var
  FOW:byte;
  ID:integer;
  ShiftX,ShiftY:single;
begin
  FOW := MyPlayer.FogOfWar.CheckTileRevelation(round(pX),round(pY));
  if FOW <= 128 then exit; //Don't render objects which are behind FOW

  case aProj of
    pt_Arrow: with UnitSprite[byte(ut_Bowman)].Act[byte(ua_Spec)].Dir[byte(Dir)] do
                ID := Step[round(Flight*Count)+1]+1;
    pt_Bolt:  with UnitSprite[byte(ut_Arbaletman)].Act[byte(ua_Spec)].Dir[byte(Dir)] do
                ID := Step[round(Flight*Count)+1]+1;
    pt_TowerRock: ID := ProjectileBounds[aProj,1]+1;
    else ID := 1; //Nothing?
  end;

  ShiftX := RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := (RXData[3].Pivot[ID].y+RXData[3].Size[ID].Y)/CELL_SIZE_PX;

  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,pX,pY,true);
end;


procedure TRender.RenderObjectOrQuad(Index,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
begin
  //Render either normal object or quad depending on what it is
  if MapElem[Index].WineOrCorn then
    RenderObjectQuad(Index,AnimStep,pX,pY,(Index-1 in [54..57]),DoImmediateRender,Deleting) //54..57 are grapes, all others are doubles
  else
    RenderObject(Index,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRender.RenderObject(Index,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
var ShiftX,ShiftY:single; ID:integer; FOW:byte;
begin
  if MapElem[Index].Count=0 then exit;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX,pY);
  if FOW = 0 then exit; //Don't render objects which are unexplored
  if FOW <=128 then AnimStep:=0; //Stop animation
  ID:=MapElem[Index].Step[AnimStep mod MapElem[Index].Count +1]+1;
  if ID<=0 then exit;

  if Index=61 then begin //Invisible wall
    ShiftX := 0; //Required if DoImmediateRender = true
    ShiftY := 0;
    glColor4f(1,0,0,0.33);
    RenderQuad(pX,pY);
    RenderCursorWireQuad(KMPoint(pX,pY),$FF0000FF);
  end else begin
    ShiftX:=RXData[1].Pivot[ID].x/CELL_SIZE_PX;
    ShiftY:=(RXData[1].Pivot[ID].y+RXData[1].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(1,ID,pX+ShiftX,pY+ShiftY,pX,pY,true);
    {RenderDot(pX,pY);
    glRasterPos2f(pX-1+0.1,pY-1+0.1);
    glPrint(inttostr(Index)+':'+inttostr(ID));}
  end;

  if DoImmediateRender then RenderSprite(1,ID,pX+ShiftX,pY+ShiftY,$FFFFFFFF,255,Deleting);

end;


{ 4 objects packed on 1 tile for Corn and Grapes }
procedure TRender.RenderObjectQuad(Index:integer; AnimStep,pX,pY:integer; IsDouble:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
var FOW:byte;
  procedure AddSpriteToListBy(aID:integer; aAnimStep:integer; pX,pY:integer; ShiftX,ShiftY:single);
  var ID:integer;
  begin
    ID := MapElem[aID].Step[aAnimStep mod MapElem[aID].Count +1 ] +1;
    ShiftY := ShiftY + (RXData[1].Size[ID].Y) / CELL_SIZE_PX;
    ShiftY := ShiftY - fTerrain.InterpolateLandHeight(pX+ShiftX, pY+ShiftY)/CELL_HEIGHT_DIV;
    AddSpriteToList(1, ID, pX+ShiftX, pY+ShiftY, pX, pY, true);
    if DoImmediateRender then RenderSprite(1,ID,pX+ShiftX,pY+ShiftY,$FFFFFFFF,255,Deleting);
  end;
begin
  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX,pY);
  if FOW <=128 then AnimStep:=0; //Stop animation

  AddSpriteToListBy(Index, AnimStep  , pX, pY, 0  , -0.4);
  AddSpriteToListBy(Index, AnimStep+1, pX, pY, 0.5, -0.4);
  if IsDouble then exit;
  AddSpriteToListBy(Index, AnimStep+1, pX, pY, 0  , 0.1);
  AddSpriteToListBy(Index, AnimStep  , pX, pY, 0.5, 0.1);
end;


{Render house WIP tablet}
procedure TRender.RenderHouseBuild(Index:THouseType; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  Loc.X := Loc.X + fResource.HouseDat[Index].EntranceOffsetX;
  ID := byte(Index) + 250;
  ShiftX := Loc.X + RXData[4].Pivot[ID].x/CELL_SIZE_PX + 0.5;
  ShiftY := Loc.Y + (RXData[4].Pivot[ID].y + RXData[4].Size[ID].Y)/CELL_SIZE_PX + 0.5 - fTerrain.Land[Loc.Y+1, Loc.X].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(4,ID,ShiftX,ShiftY,Loc.X,Loc.Y,true);
end;


{Render house build supply}
procedure TRender.RenderHouseBuildSupply(Index:THouseType; Wood,Stone:byte; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  if Wood<>0 then begin
    ID := 260+Wood-1;
    ShiftX := Loc.X + fResource.HouseDat[Index].BuildSupply[Wood].MoveX/CELL_SIZE_PX;
    ShiftY := Loc.Y + (fResource.HouseDat[Index].BuildSupply[Wood].MoveY+RXData[2].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,ShiftX,ShiftY,Loc.X,Loc.Y,false);
  end;
  if Stone<>0 then begin
    ID := 267+Stone-1;
    ShiftX := Loc.X + fResource.HouseDat[Index].BuildSupply[6+Stone].MoveX/CELL_SIZE_PX;
    ShiftY := Loc.Y + (fResource.HouseDat[Index].BuildSupply[6+Stone].MoveY+RXData[2].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
    AddSpriteToList(2,ID,ShiftX,ShiftY,Loc.X,Loc.Y,false);
  end;
end;


{Render house in wood}
procedure TRender.RenderHouseWood(Index:THouseType; Step:single; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  ID := fResource.HouseDat[Index].WoodPic+1;
  ShiftX := Loc.X + RXData[2].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := Loc.Y + (RXData[2].Pivot[ID].y+RXData[2].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(2,ID,ShiftX,ShiftY,Loc.X,Loc.Y,true,0,Step);
end;


{Render house in stone}
procedure TRender.RenderHouseStone(Index:THouseType; Step:single; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  RenderHouseWood(Index,1,Loc); //Render Wood part of it, opaque
  ID := fResource.HouseDat[Index].StonePic+1;
  ShiftX := Loc.X + RXData[2].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := Loc.Y + (RXData[2].Pivot[ID].y+RXData[2].Size[ID].Y)/CELL_SIZE_PX - fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(2,ID,ShiftX,ShiftY,Loc.X,Loc.Y,false,0,Step);
end;


procedure TRender.RenderHouseWork(Index:THouseType; AnimType,AnimStep:cardinal; Loc:TKMPoint; FlagColor:TColor4);
var AnimCount,ID:cardinal; i:byte; ShiftX,ShiftY:single;
begin
  if AnimType = 0 then exit;

  for i:=1 to Length(fResource.HouseDat[Index].Anim) do
  if AnimType and (1 shl i) = (1 shl i) then
  begin
    AnimCount := fResource.HouseDat[Index].Anim[i].Count;
    if AnimCount<>0 then
    begin
      ID := fResource.HouseDat[Index].Anim[i].Step[AnimStep mod AnimCount + 1]+1;
      ShiftX := RXData[2].Pivot[ID].x/CELL_SIZE_PX;
      ShiftY := (RXData[2].Pivot[ID].y+RXData[2].Size[ID].Y)/CELL_SIZE_PX - fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
      ShiftX := ShiftX+fResource.HouseDat[Index].Anim[i].MoveX/CELL_SIZE_PX;
      ShiftY := ShiftY+fResource.HouseDat[Index].Anim[i].MoveY/CELL_SIZE_PX;
      AddSpriteToList(2,ID,Loc.X+ShiftX,Loc.Y+ShiftY,Loc.X,Loc.Y,false,FlagColor);
    end;
  end;
end;


procedure TRender.RenderHouseSupply(Index:THouseType; const R1,R2:array of byte; Loc:TKMPoint);
var ID,i,k:integer;

  procedure AddHouseSupplySprite(aID:integer);
  var ShiftX,ShiftY:single;
  begin
    if aID>0 then
    begin
      ShiftX := Loc.X + RXData[2].Pivot[aID].x/CELL_SIZE_PX;
      ShiftY := Loc.Y + (RXData[2].Pivot[aID].y+RXData[2].Size[aID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
      AddSpriteToList(2,aID,ShiftX,ShiftY,Loc.X,Loc.Y,false);
    end;
  end;

begin
  for i:=1 to 4 do if (R1[i-1])>0 then
    begin
      ID:=fResource.HouseDat[Index].SupplyIn[i,min(R1[i-1],5)]+1;
      AddHouseSupplySprite(ID);
    end;
  for i:=1 to 4 do if (R2[i-1])>0 then
  begin
    //Exception for some houses that render layered
    if THouseType(Index) in [ht_WeaponSmithy,ht_ArmorSmithy,ht_WeaponWorkshop,ht_ArmorWorkshop] then
    begin
      for k := 1 to min(R2[i-1],5) do
      begin
        ID:=fResource.HouseDat[Index].SupplyOut[i,k]+1;
        AddHouseSupplySprite(ID);
      end
    end
    else
    begin
      ID:=fResource.HouseDat[Index].SupplyOut[i,min(R2[i-1],5)]+1;
      AddHouseSupplySprite(ID);
    end;
  end
end;


procedure TRender.RenderHouseStableBeasts(Index:THouseType; BeastID,BeastAge,AnimStep:integer; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  with fResource.HouseDat.BeastAnim[Index,BeastID,BeastAge] do begin
    ID := Step[AnimStep mod Count + 1]+1;
    ShiftX := MoveX/CELL_SIZE_PX;
    ShiftY := MoveY/CELL_SIZE_PX;
  end;

  ShiftX:=ShiftX+RXData[2].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=ShiftY+(RXData[2].Pivot[ID].y+RXData[2].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
  AddSpriteToList(2,ID,Loc.X+ShiftX,Loc.Y+ShiftY,Loc.X,Loc.Y,false);
end;


procedure TRender.RenderUnit(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
  AnimSteps:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Count;
  ID:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
  if ID<=0 then exit;

  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID].Y)/CELL_SIZE_PX;

  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,pX,pY,NewInst,FlagColor,-1,true);
  if DoImmediateRender then RenderSprite(3,ID,pX+ShiftX,pY+ShiftY,FlagColor,255,Deleting);

  if SHOW_UNIT_MOVEMENT then begin
    glColor3ubv(@FlagColor);  //Render dot where unit is
    RenderDot(pX-0.5,pY-1-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV);
  end;
end;


procedure TRender.RenderUnitCarry(CarryID,DirID,StepID:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
  AnimSteps:=SerfCarry[CarryID].Dir[DirID].Count;
  ID:=SerfCarry[CarryID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
  if ID<=0 then exit;
  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID].Y)/CELL_SIZE_PX;
  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4;
  ShiftX:=ShiftX+SerfCarry[CarryID].Dir[DirID].MoveX/CELL_SIZE_PX;
  ShiftY:=ShiftY+SerfCarry[CarryID].Dir[DirID].MoveY/CELL_SIZE_PX;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,pX,pY,false,0);
end;


procedure TRender.RenderUnitThought(Thought:TUnitThought; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer;
begin
  if byte(Thought) = 0 then exit;
  ID:=ThoughtBounds[byte(Thought),2]+1 -
     (fGame.GameTickCount mod word(ThoughtBounds[byte(Thought),2]-ThoughtBounds[byte(Thought),1]));
  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID].Y)/CELL_SIZE_PX;
  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4 - 1.5;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,pX,pY,false);
end;


procedure TRender.RenderUnitFlag(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; UnitX,UnitY:single; NewInst:boolean);
var ShiftX,ShiftY:single; ID:integer; AnimSteps:integer;
begin
  AnimSteps:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Count;
  ID:=UnitSprite[UnitID].Act[ActID].Dir[DirID].Step[StepID mod AnimSteps + 1]+1;
  if ID<=0 then exit;

  ShiftX:=RXData[3].Pivot[ID].x/CELL_SIZE_PX -0.5;
  ShiftY:=(RXData[3].Pivot[ID].y+RXData[3].Size[ID].Y)/CELL_SIZE_PX;

  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(UnitX,UnitY)/CELL_HEIGHT_DIV-0.4 -2.25;
  AddSpriteToList(3,ID,pX+ShiftX,pY+ShiftY,pX,pY,NewInst,FlagColor);

  if SHOW_UNIT_MOVEMENT then begin
    glColor3ubv(@FlagColor);
    RenderDot(pX,pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV); //Render dot where unit is
  end;
end;


procedure TRender.RenderUnitWithDefaultArm(UnitID,ActID,DirID,StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
begin
  RenderUnit(UnitID,ActID,DirID,StepID,pX,pY,FlagColor,NewInst,DoImmediateRender,Deleting);
  if (ua_WalkArm in UnitSupportedActions[TUnitType(UnitID)]) or (TUnitType(UnitID) = ut_Serf) then
    RenderUnit(UnitID,byte(ua_WalkArm),DirID,StepID,pX,pY,FlagColor,NewInst,DoImmediateRender,Deleting);
end;


{Simple dot to know where it actualy is}
procedure TRender.RenderDot(pX,pY:single; Size:single = 0.05);
begin
  glBegin (GL_QUADS);
    glkRect(pX-1-Size,pY-1+Size,pX-1+Size,pY-1-Size);
  glEnd;
end;


procedure TRender.RenderDotOnTile(pX,pY:single);
begin
  pY:=pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV;
  glBegin (GL_QUADS);
    glkRect(pX-1,pY-1,pX-1+0.1,pY-1-0.1);
  glEnd;
end;


procedure TRender.RenderLine(x1,y1,x2,y2:single);
begin
  glBegin (GL_LINES);
    glVertex2f(x1-1, y1-1 - fTerrain.InterpolateLandHeight(x1,y1)/CELL_HEIGHT_DIV);
    glVertex2f(x2-1, y2-1 - fTerrain.InterpolateLandHeight(x2,y2)/CELL_HEIGHT_DIV);
  glEnd;
end;


{Used for internal things like overlays, etc..}
procedure TRender.RenderQuad(pX,pY:integer);
begin
  if not fTerrain.TileInMapCoords(pX,pY) then exit;

  glBegin (GL_QUADS);
    with fTerrain do
    glkQuad(pX-1,pY-1-Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV,
            pX  ,pY-1-Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV,
            pX  ,pY-  Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV,
            pX-1,pY-  Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV);
  glEnd;
end;

{Render one terrian cell}
procedure TRender.RenderTile(Index:byte; pX,pY,Rot:integer);
var k,i,a:integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
  if (pX<1)or(pX>fTerrain.MapX) then exit;
  if (pY<1)or(pY>fTerrain.MapY) then exit;

  if not InRange(Index,0,255) then fLog.AssertToLog(false,'Wrong tile index, should be 0..255');

  glColor4f(1,1,1,1);
  glBindTexture(GL_TEXTURE_2D, TextT);

  TexC[1,1] := (Index mod 16  )/16; TexC[1,2]:=(Index div 16  )/16;
  TexC[2,1] := (Index mod 16  )/16; TexC[2,2]:=(Index div 16+1)/16;
  TexC[3,1] := (Index mod 16+1)/16; TexC[3,2]:=(Index div 16+1)/16;
  TexC[4,1] := (Index mod 16+1)/16; TexC[4,2]:=(Index div 16  )/16;
  TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

  if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
  if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

  k:=pX; i:=pY;
  glBegin (GL_QUADS);
  with fTerrain do
    if RENDER_3D then begin
      glTexCoord2fv(@TexC[TexO[1]]); glvertex3f(k-1,i-1,-Land[i,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[2]]); glvertex3f(k-1,i  ,-Land[i+1,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[3]]); glvertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[4]]); glvertex3f(k  ,i-1,-Land[i,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord2fv(@TexC[TexO[1]]); glvertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[2]]); glvertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[3]]); glvertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[4]]); glvertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderSprite(RX:byte; ID:word; pX,pY:single; Col:TColor4; aFOW:byte; HighlightRed: boolean=false);
var h:integer;
begin
  for h:=1 to 2 do
  with GFXData[RX,ID] do begin
    if h=1 then begin
      glColor3ub(aFOW,aFOW,aFOW);
      glBindTexture(GL_TEXTURE_2D, TexID);
    end else
    if (h=2) and (aFOW<>0) then begin //Don't render colorflags if they aren't visible cos of FOW
      glColor4ubv(@Col);
      glBindTexture(GL_TEXTURE_2D, AltID);
    end;

    if HighlightRed then glColor4f(1,0,0,1);

    if (h=1)or( (h=2)and(RXData[RX].NeedTeamColors)and(AltID<>0)) then begin
      glBegin(GL_QUADS);
        glTexCoord2f(u1,v2); glvertex2f(pX-1                     ,pY-1                      );
        glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1                      );
        glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(u1,v1); glvertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
  end;

  glBindTexture(GL_TEXTURE_2D, 0);

  if not SHOW_SPRITES_RECT then exit;
  glColor4f(1,1,1,0.5);
  glBegin(GL_LINE_LOOP);
    with GFXData[RX,ID] do
    glkRect(pX-1,pY-1,pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
  glEnd;
end;


procedure TRender.RenderSpriteAlphaTest(RX:byte; ID:word; Param:single; pX,pY:single; aFOW:byte);
begin
  //NOTION: This function does not work on some GPUs will need to replace it with simplier more complicated way
  //glDisable(GL_BLEND);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER,1-Param);
  glBlendFunc(GL_ONE,GL_ZERO);

  with GFXData[RX,ID] do begin
    glColor3ub(aFOW,aFOW,aFOW);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin (GL_QUADS);
      glTexCoord2f(u1,v2); glvertex2f(pX-1                     ,pY-1         );
      glTexCoord2f(u2,v2); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
      glTexCoord2f(u2,v1); glvertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(u1,v1); glvertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
  glDisable(GL_ALPHA_TEST);
  glAlphaFunc(GL_ALWAYS,0);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  //glEnable(GL_BLEND);

  if not SHOW_SPRITES_RECT then exit;
  glColor3f(1,1,1);
  glBegin (GL_LINE_LOOP);
    with GFXData[RX,ID] do
    glkRect(pX-1,pY-1,pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
  glEnd;
end;


{Collect all sprites into list}
procedure TRender.AddSpriteToList(aRX:byte; aID:word; pX,pY,oX,oY:single; aNew:boolean; const aTeam:cardinal=$00000000; const Step:single=-1; aIsUnit:boolean=false);
begin
  inc(RenderCount);
  if length(RenderList)-1<RenderCount then setlength(RenderList,length(RenderList)+256); //Book some space

  RenderList[RenderCount].Loc:=KMPointF(pX,pY); //Position of sprite, floating-point
  RenderList[RenderCount].Obj:=KMPointF(oX,oY); //Position of object in tile-space, floating-point
  RenderList[RenderCount].RX:=aRX;              //RX library
  RenderList[RenderCount].ID:=aID;              //Texture ID
  RenderList[RenderCount].NewInst:=aNew;        //Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[RenderCount].Team:=aTeam;          //Team ID (determines color)
  RenderList[RenderCount].AlphaStep:=Step;      //Alpha step for wip buildings
  RenderList[RenderCount].IsUnit:=aIsUnit;      //Because units use different FOW offsets

  RenderList[RenderCount].FOWvalue:=255;        //Visibility recomputed in ClipRender anyway
end;


procedure TRender.ClipRenderList;
var i:integer; P:TKMPoint;
begin
  setlength(RO,RenderCount+1);

  for i:=1 to RenderCount do
  if RenderList[i].NewInst then begin
    RO[i]:=i;
    if RenderList[i].IsUnit then
    begin
      P.X:=round(RenderList[i].Obj.X-0.5);
      P.Y:=round(RenderList[i].Obj.Y-1);
    end else
    begin
      P.X:=round(RenderList[i].Obj.X);
      P.Y:=round(RenderList[i].Obj.Y);
    end;
    //RenderQuad(P.X,P.Y);
    RenderList[i].FOWvalue := MyPlayer.FogOfWar.CheckTileRevelation(P.X,P.Y);
    if (RenderList[i].FOWvalue <= 128) and RenderList[i].IsUnit then
      RO[i] := 0;
  end else begin
    RO[i]:=0;
    RenderList[i].FOWvalue:=RenderList[i-1].FOWvalue; //Take from previous
  end;

end;


{Need to sort all items in list from top-right to bottom-left}
procedure TRender.SortRenderList;
var i,k:integer;
begin
  for i:=1 to RenderCount do if RO[i]<>0 then //Exclude child sprites from comparison
  for k:=i+1 to RenderCount do if RO[k]<>0 then
    if (RenderList[RO[k]].Loc.Y < RenderList[RO[i]].Loc.Y)
    or((RenderList[RO[k]].Loc.Y = RenderList[RO[i]].Loc.Y)
    and(RenderList[RO[k]].Loc.X > RenderList[RO[i]].Loc.X))
    then //TopMost Rightmost
      SwapInt(RO[k],RO[i])
end;


{Now render all these items from list}
procedure TRender.RenderRenderList;
var i,h:integer;
begin
  Stat_Sprites:=RenderCount;
  Stat_Sprites2:=0;

  for i:=1 to RenderCount do
  if RO[i]<>0 then begin

    h:=RO[i];
    //Incase no sprites were made
    if (RenderList[h].RX=2) and not MAKE_HOUSE_SPRITES then
      RenderDot(RenderList[h].Loc.X,RenderList[h].Loc.Y)
    else
    if (RenderList[h].RX=3) and not MAKE_UNIT_SPRITES then
      RenderDot(RenderList[h].Loc.X,RenderList[h].Loc.Y)
    else
    begin

      glPushMatrix;
        glTranslatef(RenderList[h].Obj.X,RenderList[h].Obj.Y,0);
        glRotatef(rHeading,-1,0,0);
        glTranslatef(-RenderList[h].Obj.X,-RenderList[h].Obj.Y,0);

        repeat //Render child sprites only after their parent
          with RenderList[h] do begin
            if AlphaStep=-1 then
              RenderSprite(RX,ID,Loc.X,Loc.Y,Team,FOWvalue)
            else
              RenderSpriteAlphaTest(RX,ID,AlphaStep,Loc.X,Loc.Y,FOWvalue)
          end;
          inc(h);
          inc(Stat_Sprites2);
        until((h>RenderCount)or(RenderList[h].NewInst));

      glPopMatrix;
    end;

  end;

end;


procedure TRender.RenderTerrainMarkup(Index:integer; pX,pY:integer);
var a,b:TKMPointF; ID:integer; FOW:byte;
begin
  case Index of
    1: ID:=105; //Road
    2: ID:=107; //Field
    3: ID:=108; //Wine
    4: ID:=111; //Wall
    else exit;  //WTF?
  end;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX,pY);

  glColor3ub(FOW,FOW,FOW);
  glBindTexture(GL_TEXTURE_2D,GFXData[4,ID].TexID);

  a.x:=GFXData[4,ID].u1; a.y:=GFXData[4,ID].v1;
  b.x:=GFXData[4,ID].u2; b.y:=GFXData[4,ID].v2;

  glBegin(GL_QUADS);
    glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1 - fTerrain.Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV+0.10);
    glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1 - fTerrain.Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY   - fTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY   - fTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV);

    glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY   - fTerrain.Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV);
    glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY   - fTerrain.Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1 - fTerrain.Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1 - fTerrain.Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV+0.10);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderTerrainBorder(Border:TBorderType; Pos:TKMDirection; pX,pY:integer);
var a,b:TKMPointF; ID:integer; t:single; HeightInPx:integer; FOW:byte;
begin
  ID:=1;
  case Border of
    bt_HouseBuilding: if Pos in [dir_N,dir_S] then ID:=463 else ID:=467; //WIP (Wood planks)
    bt_HousePlan:     if Pos in [dir_N,dir_S] then ID:=105 else ID:=117; //Plan (Ropes)
    bt_Wine:          if Pos in [dir_N,dir_S] then ID:=462 else ID:=466; //Fence (Wood)
    bt_Field:         if Pos in [dir_N,dir_S] then ID:=461 else ID:=465; //Fence (Stones)
  end;

  if Pos=dir_S then pY:=pY+1;
  if Pos=dir_W then pX:=pX+1;
  if Pos in [dir_N,dir_S] then
  begin //Horizontal border
    glBindTexture(GL_TEXTURE_2D,GFXData[4,ID].TexID);
    a.x:=GFXData[4,ID].u1; a.y:=GFXData[4,ID].v1;
    b.x:=GFXData[4,ID].u2; b.y:=GFXData[4,ID].v2;
    t:=GFXData[4,ID].PxWidth/CELL_SIZE_PX; //Height of border
    glBegin(GL_QUADS);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1, pY-1+t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1, pY-1-t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX+1,pY);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(a.x,b.y); glvertex2f(pX  , pY-1-t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,b.y); glvertex2f(pX  , pY-1+t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
    glEnd;
  end
  else begin //Vertical border
    glBindTexture(GL_TEXTURE_2D,GFXData[4,ID].TexID);
    HeightInPx := Round ( CELL_SIZE_PX * (1 + (fTerrain.Land[pY,pX].Height - fTerrain.Land[pY+1,pX].Height)/CELL_HEIGHT_DIV) );
    a.x:=GFXData[4,ID].u1; a.y:=GFXData[4,ID].v1;
    b.x:=GFXData[4,ID].u2; b.y:=GFXData[4,ID].v2 * (HeightInPx / GFXData[4,ID].PxHeight);
    t:=GFXData[4,ID].PxWidth/CELL_SIZE_PX; //Width of border
    glBegin(GL_QUADS);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(a.x,a.y); glvertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,a.y); glvertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY+1);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(b.x,b.y); glvertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,b.y); glvertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
    glEnd;
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderCursorWireQuad(P:TKMPoint; Col:TColor4);
begin
  if not fTerrain.TileInMapCoords(P.X, P.Y) then exit;
  glColor4ubv(@Col);
  glBegin(GL_LINE_LOOP);
    with fTerrain do begin
      glvertex2f(P.X-1,P.Y-1-Land[P.Y  ,P.X  ].Height/CELL_HEIGHT_DIV);
      glvertex2f(P.X  ,P.Y-1-Land[P.Y  ,P.X+1].Height/CELL_HEIGHT_DIV);
      glvertex2f(P.X  ,P.Y-  Land[P.Y+1,P.X+1].Height/CELL_HEIGHT_DIV);
      glvertex2f(P.X-1,P.Y-  Land[P.Y+1,P.X  ].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


procedure TRender.RenderCursorBuildIcon(P:TKMPoint; id:integer=479);
begin
  if fTerrain.TileInMapCoords(P.X,P.Y) then
    RenderSprite(4,id,P.X+0.2,P.Y+1-0.2-fTerrain.InterpolateLandHeight(P.X+0.5,P.Y+0.5)/CELL_HEIGHT_DIV,$FFFFFFFF,255);
end;


procedure TRender.RenderCursorWireHousePlan(P:TKMPoint; aHouseType:THouseType);
var i,k,s,t:integer; P2:TKMPoint; AllowBuild:boolean;
  MarkedLocations:array[1..64] of TKMPoint; //List of locations with special marks on them
  MarkCount:integer;

  procedure MarkPoint(aPoint:TKMPoint; aID:integer);
  var v: integer;
  begin
    for v:=1 to MarkCount do
      if KMSamePoint(MarkedLocations[v],aPoint) then
        exit;
    RenderCursorBuildIcon(aPoint,aID);
    inc(MarkCount);
    MarkedLocations[MarkCount] := aPoint;
  end;
begin
  MarkCount := 0;
  FillChar(MarkedLocations, SizeOf(MarkedLocations), #0); //It's filled with garbage if not initialized

  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[byte(aHouseType),i,k]<>0 then
  begin

    if fTerrain.TileInMapCoords(P.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,P.Y+i-4,1) then
    begin
      //This can't be done earlier since values can be off-map
      P2 := KMPoint(P.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,P.Y+i-4);

      //Check house-specific conditions, e.g. allow shipyards only near water and etc..
      case aHouseType of
        ht_IronMine: AllowBuild := (CanBuildIron in fTerrain.Land[P2.Y,P2.X].Passability);
        ht_GoldMine: AllowBuild := (CanBuildGold in fTerrain.Land[P2.Y,P2.X].Passability);
        else         AllowBuild := (CanBuild     in fTerrain.Land[P2.Y,P2.X].Passability);
      end;

      //Forbid planning on unrevealed areas
      AllowBuild := AllowBuild and (MyPlayer.FogOfWar.CheckTileRevelation(P2.X,P2.Y) > 0);

      //Check surrounding tiles in +/- 1 range for other houses pressence
      if not (CanBuild in fTerrain.Land[P2.Y,P2.X].Passability) then
      for s:=-1 to 1 do for t:=-1 to 1 do
      if (s<>0)or(t<>0) then  //This is a surrounding tile, not the actual tile
      if fTerrain.Land[P2.Y+t,P2.X+s].Markup in [mu_HousePlan, mu_HouseFenceCanWalk, mu_HouseFenceNoWalk, mu_House] then
      begin
        MarkPoint(KMPoint(P2.X+s,P2.Y+t),479);
        AllowBuild := false;
      end;

      //Mark the tile according to previous check results
      if AllowBuild then
      begin
        RenderCursorWireQuad(P2,$FFFFFF00); //Cyan
        if HousePlanYX[byte(aHouseType),i,k]=2 then
          MarkPoint(P2,481);
      end else
      begin
        if HousePlanYX[byte(aHouseType),i,k]=2 then
          MarkPoint(P2,482)
        else
          if aHouseType in [ht_GoldMine,ht_IronMine] then
            MarkPoint(P2,480)
          else
            MarkPoint(P2,479);
      end;

    end else
    if fTerrain.TileInMapCoords(P.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,P.Y+i-4,0) then
      MarkPoint(KMPoint(P.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,P.Y+i-4),479);
  end;
end;


procedure TRender.RenderCursorHighlights;
  function TileVisible:boolean;
  begin //Shortcut function
    Result := MyPlayer.FogOfWar.CheckTileRevelation(GameCursor.Cell.X,GameCursor.Cell.Y) > 0;
  end;
var
  UnitDelete: TKMUnit;
begin
  if GameCursor.Cell.Y*GameCursor.Cell.X = 0 then exit; //Caused a rare crash
  with fTerrain do
  case GameCursor.Mode of
    cm_None:   ;
    cm_Erase:  begin
                 if fGame.GameState = gsEditor then
                 begin
                   if (fGame.fMapEditorInterface.GetShownPage = esp_Units) //With Units tab see if there's a unit below cursor
                   and fPlayers.RemAnyUnit(GameCursor.Cell,true) and TileVisible then
                   begin
                     UnitDelete := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                     if UnitDelete <> nil then
                       RenderUnitWithDefaultArm(byte(UnitDelete.UnitType),byte(ua_Walk),byte(UnitDelete.Direction),UnitDelete.AnimStep,GameCursor.Cell.X+0.5,GameCursor.Cell.Y+1,fPlayers[UnitDelete.GetOwner].FlagColor,true,true,true);
                   end
                   else
                   if (
                         //With Buildings tab see if we can remove Fields or Houses
                          (fGame.fMapEditorInterface.GetShownPage = esp_Buildings)
                          and (    TileIsCornField(GameCursor.Cell)
                                or TileIsWineField(GameCursor.Cell)
                                or (Land[GameCursor.Cell.Y,GameCursor.Cell.X].TileOverlay=to_Road)
                                or fPlayers.RemAnyHouse(GameCursor.Cell, true, true))
                      )
                   //And of course it is visible
                   and TileVisible then
                     RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                   else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
                 end;

                 if fGame.GameState in [gsPaused, gsOnHold, gsRunning] then
                 begin
                   if (MyPlayer.RemPlan(GameCursor.Cell, true, true) or MyPlayer.RemHouse(GameCursor.Cell, true, true))
                   and TileVisible then
                     RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                   else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
                 end;
               end;
    cm_Road:   if CanPlaceRoad(GameCursor.Cell, mu_RoadPlan, MyPlayer) and TileVisible then
                 RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
               else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Field:  if CanPlaceRoad(GameCursor.Cell, mu_FieldPlan, MyPlayer) and TileVisible then
                 RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
               else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Wine:   if CanPlaceRoad(GameCursor.Cell, mu_WinePlan, MyPlayer) and TileVisible then
                 RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
               else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Wall:   if CanPlaceRoad(GameCursor.Cell, mu_WallPlan, MyPlayer) and TileVisible then
                 RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
               else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Houses: RenderCursorWireHousePlan(GameCursor.Cell, THouseType(GameCursor.Tag1)); //Cyan quad
    cm_Tiles:  if GameCursor.Tag2 in [0..3] then
                 RenderTile(GameCursor.Tag1, GameCursor.Cell.X, GameCursor.Cell.Y, GameCursor.Tag2)
               else
                 RenderTile(GameCursor.Tag1, GameCursor.Cell.X, GameCursor.Cell.Y, (fTerrain.AnimStep div 5) mod 4); //Spin it slowly so player remembers it is on randomized
    cm_Objects:begin
                 RenderObjectOrQuad(fTerrain.Land[GameCursor.Cell.Y,GameCursor.Cell.X].Obj+1, fTerrain.AnimStep, GameCursor.Cell.X, GameCursor.Cell.Y, true, true); //Make entire object red
                 RenderObjectOrQuad(GameCursor.Tag1+1, fTerrain.AnimStep, GameCursor.Cell.X, GameCursor.Cell.Y, true);
               end;
    cm_Height: begin
                 //todo: Render dots on tiles with brightness showing how much they will be elevated
                 RenderDotOnTile(GameCursor.Float.X+1,GameCursor.Float.Y+1);
               end;
    cm_Units:  if CanPlaceUnit(GameCursor.Cell, TUnitType(GameCursor.Tag1)) then
                 RenderUnitWithDefaultArm(GameCursor.Tag1,byte(ua_Walk),byte(dir_S),UnitStillFrames[dir_S],GameCursor.Cell.X+0.5,GameCursor.Cell.Y+1,MyPlayer.FlagColor,true,true)
               else RenderCursorBuildIcon(GameCursor.Cell);       //Red X
  end;
end;


//Render highlight overlay to make whole picture look brighter
procedure TRender.RenderBrightness(Value:byte);
begin
  if Value=0 then exit;
  glBlendFunc(GL_DST_COLOR,GL_ONE);
  glColor4f(Value/20,Value/20,Value/20,Value/20);
  glBegin(GL_QUADS);
    glkRect(0,0,fRenderAreaSize.X,fRenderAreaSize.Y);
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;


end.
