unit KM_RenderPool;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  {$IFDEF WDC} JPEG, {$ENDIF} //Lazarus doesn't have JPEG library yet -> FPReadJPEG?
  KM_Defaults, KM_CommonClasses, KM_Pics, KM_Render, KM_ResourceSprites, KM_Points, KM_Terrain;

type
  TRenderList = class
  private
    fCount: Word;
    RenderOrder: array of smallint; //Order in which sprites will be drawn ()
    RenderList: array of record
      Loc: TKMPointF; //Where sprite corner is located
      Feet: TKMPointF; //Feet of the sprite for FOW calculation (X;Y) and Z ordering (Y only)
      RX: TRXType;
      ID: Word;
      NewInst: Boolean;
      TeamColor: Cardinal;
      AlphaStep: Single; //Only appliable to HouseBuild
      FOWvalue: Byte; // Fog of War thickness
    end;

    fStat_Sprites: Integer; //Total sprites in queue
    fStat_Sprites2: Integer;//Rendered sprites
    procedure ClipRenderList;
    procedure SortRenderList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1); overload;
    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1); overload;
    property Stat_Sprites: Integer read fStat_Sprites;
    property Stat_Sprites2: Integer read fStat_Sprites2;
    procedure Render;
  end;

  //Game Renderer
  TRenderPool = class
  private
    fRXData: array [TRXType] of TRXData; //Shortcuts
    fRender: TRender;
    rPitch,rHeading,rBank: Integer;
    fRenderList: TRenderList;
    procedure RenderTile(Index: Byte; pX,pY,Rot: Integer);
    procedure RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param: Single; pX,pY: Single; aFOW: Byte);
    procedure RenderTerrainMarkup(aLocX, aLocY: Word; aFieldType: TFieldType);
    procedure RenderTerrainBorder(Border: TBorderType; Pos: TKMDirection; pX,pY: Integer);
    procedure RenderObjectOrQuad(aIndex,AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderObject(aIndex: Integer; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderObjectQuad(aIndex: Integer; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);

    //Terrain rendering sub-class
    procedure RenderTerrain;
    procedure RenderTerrainTiles(aRect: TKMRect; AnimStep: Integer);
    procedure RenderTerrainFieldBorders(aRect: TKMRect);
    procedure RenderTerrainObjects(aRect: TKMRect; AnimStep: Cardinal);

    //Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderCursorHighlights;
    procedure RenderCursorWireQuad(P: TKMPoint; Col: TColor4);
    procedure RenderCursorBuildIcon(aLoc: TKMPoint; aID: Integer=479);
    procedure RenderCursorWireHousePlan(P: TKMPoint; aHouseType: THouseType);

  public
    constructor Create(aRender: TRender);
    destructor Destroy; override;

    property RenderList: TRenderList read fRenderList;

    procedure SetRotation(aH,aP,aB: Integer);
    procedure DoPrintScreen(FileName: string);

    procedure Render;

    procedure AddProjectile(aProj: TProjectileType; aPos: TKMPointF; aDir: TKMDirection; aFlight: Single);
    procedure AddHouseTablet(aHouse: THouseType; Loc: TKMPoint);
    procedure AddHouseBuildSupply(aHouse: THouseType; Loc: TKMPoint; Wood,Stone: Byte);
    procedure AddHouseWood(aHouse: THouseType; Loc: TKMPoint; Step: Single);
    procedure AddHouseStone(aHouse: THouseType; Loc: TKMPoint; Step: Single);
    procedure AddHouseWork(aHouse: THouseType; Loc: TKMPoint; aActSet: THouseActionSet; AnimStep: Cardinal; FlagColor: TColor4);
    procedure AddHouseSupply(aHouse: THouseType; Loc: TKMPoint; const R1,R2:array of byte);
    procedure AddHouseMarketSupply(Loc: TKMPoint; ResType: TResourceType; ResCount:word; AnimStep: Integer);
    procedure AddHouseStableBeasts(aHouse: THouseType; Loc: TKMPoint; BeastID,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
    procedure AddHouseEater(Loc: TKMPoint; aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; OffX,OffY: Single; FlagColor: TColor4);
    procedure AddUnit(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure AddUnitCarry(aCarry: TResourceType; aDir: TKMDirection; StepID: Integer; pX,pY: Single);
    procedure AddUnitThought(Thought: TUnitThought; pX,pY: Single);
    procedure AddUnitFlag(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; UnitAnim, FlagAnim: Integer; pX,pY: Single; FlagColor, TeamColor: TColor4);
    procedure AddUnitWithDefaultArm(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
  end;


var
  fRenderPool: TRenderPool;


implementation
uses KM_RenderAux, KM_PlayersCollection, KM_Game, KM_Sound, KM_Resource, KM_ResourceUnit, KM_ResourceHouse, KM_Units;


constructor TRenderPool.Create(aRender: TRender);
var RT: TRXType;
begin
  Inherited Create;

  for RT := Low(TRXType) to High(TRXType) do
    fRXData[RT] := fResource.Sprites[RT].RXData;

  fRender := aRender;
  fRenderList := TRenderList.Create;
end;


destructor TRenderPool.Destroy;
begin
  fRenderList.Free;
  inherited;
end;


procedure TRenderPool.SetRotation(aH,aP,aB: Integer);
begin
  rHeading := aH;
  rPitch   := aP;
  rBank    := aB;
end;


//Render:
// 1. Sets viewport
// 2. Renders terrain
// 3. Polls Game objects to add themselves to RenderList through Add** methods
// 4. Renders cursor highlights
procedure TRenderPool.Render;
begin
  if fRender.Blind then Exit;

  glLoadIdentity; // Reset The View
  //glRotate(-15,0,0,1); //Funny thing
  glTranslatef(fGame.Viewport.ViewportClip.X/2, fGame.Viewport.ViewportClip.Y/2, 0);
  glScalef(fGame.Viewport.Zoom*CELL_SIZE_PX, fGame.Viewport.Zoom*CELL_SIZE_PX, 1);
  glTranslatef(-fGame.Viewport.Position.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fGame.Viewport.Zoom, -fGame.Viewport.Position.Y, 0);
  if RENDER_3D then
  begin
    fRender.SetRenderMode(rm3D);

    glkScale(-CELL_SIZE_PX/14);
    glRotatef(rHeading,1,0,0);
    glRotatef(rPitch  ,0,1,0);
    glRotatef(rBank   ,0,0,1);
    glTranslatef(-fGame.Viewport.Position.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fGame.Viewport.Zoom, -fGame.Viewport.Position.Y-8, 10);
    glScalef(fGame.Viewport.Zoom, fGame.Viewport.Zoom, 1);
  end;

  glPushAttrib(GL_LINE_BIT or GL_POINT_BIT);
    glLineWidth(fGame.Viewport.Zoom * 2);
    glPointSize(fGame.Viewport.Zoom * 5);

    RenderTerrain;
    fPlayers.Paint; //Quite slow           //Units and houses
    if fGame.GameState in [gsPaused, gsOnHold, gsRunning, gsReplay] then
      fGame.Projectiles.Paint; //Render all arrows and etc..

    fRenderList.Render;

    RenderCursorHighlights; //Will be on-top of everything

    if DISPLAY_SOUNDS then fSoundLib.Paint;
  glPopAttrib;
end;


procedure TRenderPool.DoPrintScreen(FileName: String);
{$IFDEF WDC}
var
  i, k, W, H: integer;
  jpg: TJpegImage;
  mkbmp: TBitMap;
  bmp: array of Cardinal;
{$ENDIF}
begin
{$IFDEF WDC}
  W := fRender.ScreenX;
  H := fRender.ScreenY;

  SetLength(bmp, W * H + 1);
  glReadPixels(0, 0, W, H, GL_BGRA, GL_UNSIGNED_BYTE, @bmp[0]);

  //Mirror verticaly
  for i := 0 to (H div 2) - 1 do
    for k := 0 to W - 1 do
      SwapInt(bmp[i * W + k], bmp[((H - 1) - i) * W + k]);

  mkbmp := TBitmap.Create;
  mkbmp.Handle := CreateBitmap(W, H, 1, 32, @bmp[0]);

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
{$ENDIF}
end;


procedure TRenderPool.RenderTerrainTiles(aRect: TKMRect; AnimStep: Integer);
  procedure LandLight(A: Single);
  begin
    glColor4f(A / 1.5 + 0.5, A / 1.5 + 0.5, A / 1.5 + 0.5, Abs(A * 2)); // Balanced looks
    //glColor4f(a*2,a*2,a*2,Abs(a*2)); //Deeper shadows
  end;
var
  i,k,iW: Integer;
  ID,rd,Rot: Byte;
  xt,A: Integer;
  TexC: array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO: array[1..4]of byte;         //order of UV coordinates, for rotations
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT);

  //With depth test we can render all terrain tiles and then apply light/shadow without worrying about
  //foothills shadows going over mountain tops. Each tile strip is rendered an next Z plane.
  //Means that Z-test on gpu will take care of clipping the foothill shadows
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  for iW:=1 to 1+3*byte(MAKE_ANIM_TERRAIN) do begin //Each new layer inflicts 10% fps drop
    case iW of
      1: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextT);
      2: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextW[AnimStep mod 8 + 1]);
      3: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextS[AnimStep mod 24 div 8 + 1]); //These should be unsynced later on
      4: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextF[AnimStep mod 5 + 1]);
    end;
    glBegin(GL_QUADS);
      with fTerrain do
      for i := aRect.Top to aRect.Bottom do
      for k := aRect.Left to aRect.Right do
      if (iW=1) or (MyPlayer.FogOfWar.CheckTileRevelation(k,i,true) > FOG_OF_WAR_ACT) then //No animation in FOW
      begin
        xt := fTerrain.Land[i,k].Terrain;

        TexC[1,1]:=(xt mod 16  )/16; TexC[1,2]:=(xt div 16  )/16;
        TexC[2,1]:=(xt mod 16  )/16; TexC[2,2]:=(xt div 16+1)/16;
        TexC[3,1]:=(xt mod 16+1)/16; TexC[3,2]:=(xt div 16+1)/16;
        TexC[4,1]:=(xt mod 16+1)/16; TexC[4,2]:=(xt div 16  )/16;

        TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

        if fTerrain.Land[i,k].Rotation and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
        if fTerrain.Land[i,k].Rotation and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

        glColor4f(1,1,1,1);
        if RENDER_3D then begin
          glTexCoord2fv(@TexC[TexO[1]]); glVertex3f(k-1,i-1,-Land[i,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[2]]); glVertex3f(k-1,i  ,-Land[i+1,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[3]]); glVertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[4]]); glVertex3f(k  ,i-1,-Land[i,k+1].Height/CELL_HEIGHT_DIV);
        end else begin

          glTexCoord2fv(@TexC[TexO[1]]); glVertex3f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV, -i);
          glTexCoord2fv(@TexC[TexO[2]]); glVertex3f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV, -i);
          glTexCoord2fv(@TexC[TexO[3]]); glVertex3f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV, -i);
          glTexCoord2fv(@TexC[TexO[4]]); glVertex3f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV, -i);

          if KAM_WATER_DRAW and (iW=1) and fTerrain.TileIsWater(KMPoint(k,i)) then
          begin
            xt := 32;
            TexC[1,1] := (xt mod 16  )/16; TexC[1,2] := (xt div 16  )/16;
            TexC[2,1] := (xt mod 16  )/16; TexC[2,2] := (xt div 16+1)/16;
            TexC[3,1] := (xt mod 16+1)/16; TexC[3,2] := (xt div 16+1)/16;
            TexC[4,1] := (xt mod 16+1)/16; TexC[4,2] := (xt div 16  )/16;
            TexO[1] := 1; TexO[2] := 2; TexO[3] := 3; TexO[4] := 4;

            LandLight(Land[i  ,k  ].Light);
            glTexCoord2fv(@TexC[TexO[1]]); glVertex3f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV, -i);
            LandLight(Land[i+1,k  ].Light);
            glTexCoord2fv(@TexC[TexO[2]]); glVertex3f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV, -i);
            LandLight(Land[i+1,k+1].Light);
            glTexCoord2fv(@TexC[TexO[3]]); glVertex3f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV, -i);
            LandLight(Land[i  ,k+1].Light);
            glTexCoord2fv(@TexC[TexO[4]]); glVertex3f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV, -i);
          end;
        end;
      end;
    glEnd;
  end;

  glColor4f(1,1,1,1);

  for i := aRect.Top to aRect.Bottom do
  for k := aRect.Left to aRect.Right do
  begin
    case fTerrain.Land[i,k].TileOverlay of
      to_Dig1: RenderTile(249,k,i,0);
      to_Dig2: RenderTile(251,k,i,0);
      to_Dig3: RenderTile(253,k,i,0);
      to_Dig4: RenderTile(255,k,i,0);
      to_Wall: fRenderAux.Quad(k,i, $80000080); //We don't have graphics yet
    end;

    if fTerrain.Land[i,k].TileOverlay=to_Road then
    begin
      rd := 0;
      if fTerrain.TileInMapCoords(k  ,i-1) then rd:=rd+byte(fTerrain.Land[i-1,k  ].TileOverlay=to_Road) shl 0;
      if fTerrain.TileInMapCoords(k+1,i  ) then rd:=rd+byte(fTerrain.Land[i  ,k+1].TileOverlay=to_Road) shl 1;
      if fTerrain.TileInMapCoords(k  ,i+1) then rd:=rd+byte(fTerrain.Land[i+1,k  ].TileOverlay=to_Road) shl 2;
      if fTerrain.TileInMapCoords(k-1,i  ) then rd:=rd+byte(fTerrain.Land[i  ,k-1].TileOverlay=to_Road) shl 3;
      ID  := RoadsConnectivity[rd,1];
      Rot := RoadsConnectivity[rd,2];
      RenderTile(ID,k,i,Rot);
    end;
  end;

  glColor4f(1,1,1,1);
  //Render highlights
  glBlendFunc(GL_DST_COLOR, GL_ONE);
  glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextL);
  glBegin(GL_QUADS);
  with fTerrain do
  for i := aRect.Top to aRect.Bottom do
  for k := aRect.Left to aRect.Right do
    if RENDER_3D then begin
      glTexCoord1f(Land[i  ,k  ].Light); glVertex3f(k-1,i-1,-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(Land[i+1,k  ].Light); glVertex3f(k-1,i  ,-Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(Land[i+1,k+1].Light); glVertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(Land[i  ,k+1].Light); glVertex3f(k  ,i-1,-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord1f(Land[i  ,k  ].Light); glVertex3f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(Land[i+1,k  ].Light); glVertex3f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(Land[i+1,k+1].Light); glVertex3f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(Land[i  ,k+1].Light); glVertex3f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV, -i);
    end;
  glEnd;

  //Render shadows and FOW at once
  glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextD);
  glBegin(GL_QUADS);
    with fTerrain do
    for i := aRect.Top to aRect.Bottom do
    for k := aRect.Left to aRect.Right do
    if RENDER_3D then begin
      glTexCoord1f(kromutils.max(0,-Land[i  ,k  ].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i,true)/255));
      glVertex3f(k-1,i-1,-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i+1,k  ].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i+1,true)/255));
      glVertex3f(k-1,i  ,-Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i+1,k+1].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i+1,true)/255));
      glVertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0,-Land[i  ,k+1].Light,1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i,true)/255));
      glVertex3f(k  ,i-1,-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord1f(max(-Land[i  ,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i,true)/255));
      glVertex3f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(max(-Land[i+1,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i+1,true)/255));
      glVertex3f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(max(-Land[i+1,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i+1,true)/255));
      glVertex3f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord1f(max(-Land[i  ,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i,true)/255));
      glVertex3f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV, -i);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);

  if SHOW_WALK_CONNECT then
  for i := aRect.Top to aRect.Bottom do
  for k := aRect.Left to aRect.Right do
  with fTerrain.Land[i,k] do
    fRenderAux.Quad(k, i, WalkConnect[wcWalk] * 32 + (WalkConnect[wcRoad] * 32) shl 8 or $80000000);

  glPopAttrib;
end;


procedure TRenderPool.RenderTerrainFieldBorders(aRect: TKMRect);
var
  I,K: Integer;
  BordersList: TKMPointDirList;
  FieldsList, TabletsList: TKMPointTagList;
begin
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
  with fTerrain do
  begin
    if Land[I,K].BorderTop then RenderTerrainBorder(Land[I,K].Border, dir_N, K, I);
    if Land[I,K].BorderLeft then RenderTerrainBorder(Land[I,K].Border, dir_E, K, I);
    if Land[I,K].BorderRight then RenderTerrainBorder(Land[I,K].Border, dir_W, K, I);
    if Land[I,K].BorderBottom then RenderTerrainBorder(Land[I,K].Border, dir_S, K, I);
  end;

  //@Lewin: Since plans are per-player now, what do we do about allies that:
  // - have partially overlapping plans
  // - have plans/tablets on exact same spot
  //@Krom: Allies should see each other's plans and not allow to place over existing ones.
  //For enemies, if two plans overlap, the other should be removed when one is started.
  //This is currently working for roads/fields but not houses

  //Fieldplans
  FieldsList := TKMPointTagList.Create;
  MyPlayer.GetFieldPlans(FieldsList, aRect, True); //Include fake field plans for painting
  for i := 0 to FieldsList.Count - 1 do
    RenderTerrainMarkup(FieldsList[i].X, FieldsList[i].Y, TFieldType(FieldsList.Tag[i]));
  FreeAndNil(FieldsList);

  //Borders
  BordersList := TKMPointDirList.Create;
  MyPlayer.GetPlansBorders(BordersList, aRect);
  for i := 0 to BordersList.Count - 1 do
    RenderTerrainBorder(bt_HousePlan, BordersList[i].Dir, BordersList[i].Loc.X, BordersList[i].Loc.Y);
  FreeAndNil(BordersList);

  //Tablets
  TabletsList := TKMPointTagList.Create;
  MyPlayer.GetPlansTablets(TabletsList, aRect);
  for i := 0 to TabletsList.Count - 1 do
    AddHouseTablet(THouseType(TabletsList.Tag[i]), TabletsList[i]);
  FreeAndNil(TabletsList);
end;


procedure TRenderPool.RenderTerrainObjects(aRect: TKMRect; AnimStep: Cardinal);
var
  I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
    if fTerrain.Land[I, K].Obj <> 255 then
      RenderObjectOrQuad(fTerrain.Land[I, K].Obj + 1, AnimStep, K, I);

  with fTerrain do
    for I := 0 to FallingTrees.Count - 1 do
    begin
      RenderObject(FallingTrees.Tag[I] + 1, AnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y);
      Assert(AnimStep - FallingTrees.Tag2[I] <= 100, 'Falling tree overrun?');
    end;
end;


procedure TRenderPool.AddProjectile(aProj: TProjectileType; aPos: TKMPointF; aDir: TKMDirection; aFlight: Single);
var
  FOW: Byte;
  ID: Integer;
  R: TRXData;
  CornerX,CornerY: Single;
  Ground: Single;
begin
  //We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not fTerrain.TileInMapCoords(Round(aPos.X), Round(aPos.Y)) then Exit;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(Round(aPos.X), Round(aPos.Y), True);
  if FOW <= 128 then Exit; //Don't render objects which are behind FOW

  case aProj of
    pt_Arrow:     with fResource.UnitDat[ut_Bowman].UnitAnim[ua_Spec, aDir] do
                    ID := Step[Round(Min(aFlight, 1) * Count) + 1] + 1;
    pt_Bolt:      with fResource.UnitDat[ut_Arbaletman].UnitAnim[ua_Spec, aDir] do
                    ID := Step[Round(Min(aFlight, 1) * Count) + 1] + 1;
    pt_SlingRock: with fResource.UnitDat[ut_Slingshot].UnitAnim[ua_Spec, aDir] do
                    ID := Step[Round(Min(aFlight, 1) * Count) + 1] + 1;
    pt_TowerRock: ID := ProjectileBounds[aProj, 1] + 1;
    else          ID := 1; //Nothing?
  end;

  R := fRXData[rxUnits];

  CornerX := R.Pivot[ID].X / CELL_SIZE_PX - 1;
  CornerY := (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1;

  case aProj of
    pt_Arrow, pt_Bolt, pt_SlingRock:  Ground := aPos.Y + (0.5 - Abs(Min(aFlight, 1) - 0.5)) - 0.5;
    pt_TowerRock:                     Ground := aPos.Y + (1 - Min(aFlight, 1)) - 0.5;
    else                              Ground := aPos.Y - 1; //Nothing?
  end;

  fRenderList.AddSprite(rxUnits, ID, aPos.X + CornerX, aPos.Y + CornerY, aPos.X - 1, Ground);
end;


procedure TRenderPool.RenderObjectOrQuad(aIndex,AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  //Render either normal object or quad depending on what it is
  if MapElem[aIndex].WineOrCorn then
    RenderObjectQuad(aIndex,AnimStep,pX,pY,(aIndex-1 in [54..57]),DoImmediateRender,Deleting) //54..57 are grapes, all others are doubles
  else
    RenderObject(aIndex,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderObject(aIndex: Integer; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;
  pX,pY: Integer;
  CornerX, CornerY: Single;
  gX, gY: Single;
  ID, ID0: Integer;
  FOW: Byte;
begin
  if MapElem[aIndex].Count = 0 then exit;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(LocX,LocY,true);
  if FOW = 0 then exit; //Don't render objects which are unexplored
  if FOW <=128 then AnimStep:=0; //Stop animation
  ID := MapElem[aIndex].Step[AnimStep mod MapElem[aIndex].Count +1]+1;
  ID0 := MapElem[aIndex].Step[1] + 1;
  if ID <= 0 then exit;

  pX := LocX - 1;
  pY := LocY - 1;

  if aIndex = 61 then begin //Invisible wall
    CornerX := pX; //Required if DoImmediateRender = true
    CornerY := pY;
    fRenderAux.Quad(pX, pY, $800000FF);
    RenderCursorWireQuad(KMPoint(pX, pY), $FF0000FF);
  end else begin
    R := fRXData[rxTrees];
    gX := pX + (R.Pivot[ID0].X + R.Size[ID0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
    CornerY := pY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                  - fTerrain.HeightAt(gX, gY) / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxTrees, ID, CornerX, CornerY, gX, gY);

    fRenderAux.Dot(pX, pY - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV, $FFFF0000);
    fRenderAux.Dot(pX + R.Pivot[ID].X / CELL_SIZE_PX,
                   pY + R.Pivot[ID].Y / CELL_SIZE_PX - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV, $FFFFFF00);
    //fRenderAux.Dot(CornerX, CornerY, $FFFF00FF);
    //glRasterPos2f(pX - 1 + 0.1, pY - 1 + 0.1);
    //glPrint(inttostr(aIndex) + ':' + inttostr(ID));
  end;

  if DoImmediateRender then
    RenderSprite(rxTrees, ID, CornerX, CornerY, $FFFFFFFF, 255, Deleting);
end;


//4 objects packed on 1 tile for Corn and Grapes
procedure TRenderPool.RenderObjectQuad(aIndex: Integer; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;

  procedure AddSpriteBy(aAnimStep: Integer; pX,pY: Single);
  var
    ID, ID0: Integer;
    CornerX, CornerY, gX, gY: Single;
  begin
    ID := MapElem[aIndex].Step[aAnimStep mod MapElem[aIndex].Count + 1] + 1;
    ID0 := MapElem[aIndex].Step[1] + 1;

    gX := pX + (R.Pivot[ID0].X + R.Size[ID0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
    CornerY := pY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
             - fTerrain.HeightAt(gX, gY) / CELL_HEIGHT_DIV;

    fRenderList.AddSprite(rxTrees, ID, CornerX, CornerY, gX, gY);

    if DoImmediateRender then
      RenderSprite(rxTrees, ID, CornerX, CornerY, $FFFFFFFF, 255, Deleting);
  end;

var
  FOW: Byte;
begin
  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX, pY, True);
  if FOW <= 128 then AnimStep := 0; //Stop animation

  R := fRXData[rxTrees];
  AddSpriteBy(AnimStep  , pX - 0.75, pY - 0.75);
  AddSpriteBy(AnimStep+1, pX - 0.25, pY - 0.75);
  if IsDouble then Exit;
  AddSpriteBy(AnimStep+1, pX - 0.75, pY - 0.25);
  AddSpriteBy(AnimStep  , pX - 0.25, pY - 0.25);
end;


//Render house WIP tablet
procedure TRenderPool.AddHouseTablet(aHouse: THouseType; Loc: TKMPoint);
var
  ID: Integer;
  CornerX, CornerY, gX, gY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];
  ID := fResource.HouseDat[aHouse].TabletIcon;

  gX := Loc.X + (R.Pivot[ID].X + R.Size[ID].X / 2) / CELL_SIZE_PX - 0.5;
  gY := Loc.Y + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 0.45;
  CornerX := Loc.X + R.Pivot[ID].X / CELL_SIZE_PX - 0.25;
  CornerY := Loc.Y + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 0.45
                   - fTerrain.HeightAt(gX, gY) / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxGui, ID, CornerX, CornerY, gX, gY);
end;


//Render house build supply
procedure TRenderPool.AddHouseBuildSupply(aHouse: THouseType; Loc: TKMPoint; Wood,Stone: Byte);
var
  R: TRXData;
  ID: Integer;
  BS: THouseBuildSupply;
  CornerX, CornerY: Single;
begin
  R := fRXData[rxHouses];
  BS := fResource.HouseDat[aHouse].BuildSupply;
  if Wood <> 0 then
  begin
    ID := 260 + Wood - 1;
    CornerX := Loc.X + BS[1, Wood].MoveX / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (BS[1, Wood].MoveY + R.Size[ID].Y) / CELL_SIZE_PX - 1
                     - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY);
  end;
  if Stone <> 0 then
  begin
    ID := 267 + Stone - 1;
    CornerX := Loc.X + BS[2, Stone].MoveX / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (BS[2, Stone].MoveY + R.Size[ID].Y) / CELL_SIZE_PX - 1
                     - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY);
  end;
end;


//Render house in wood
procedure TRenderPool.AddHouseWood(aHouse: THouseType; Loc: TKMPoint; Step: Single);
var
  R: TRXData;
  ID,ID2: Integer;
  CornerX, CornerY, gW, gS, gX, gY: Single;
begin
  R := fRXData[rxHouses];
  ID := fResource.HouseDat[aHouse].WoodPic + 1;
  ID2 := fResource.HouseDat[aHouse].StonePic + 1;
  gW := R.Pivot[ID].Y + R.Size[ID].Y;
  gS := R.Pivot[ID2].Y + R.Size[ID2].Y;
  gX := Loc.X + (R.Pivot[ID].X + R.Size[ID].X / 2) / CELL_SIZE_PX - 1;
  gY := Loc.Y + Max(gW, gS) / CELL_SIZE_PX - 1;
  CornerX := Loc.X + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := Loc.Y + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY, gX, gY, $0, Step);
end;


//Render house in stone
procedure TRenderPool.AddHouseStone(aHouse: THouseType; Loc: TKMPoint; Step: Single);
var
  ID: Integer;
  CornerX, CornerY: Single;
  R: TRXData;
begin
  //We need to render Wood part of the house because Stone part may have some of
  //detail clipped where there's no stone used(e.g. Sawmills left side)
  AddHouseWood(aHouse, Loc, 1);

  R := fRXData[rxHouses];
  ID := fResource.HouseDat[aHouse].StonePic + 1;
  CornerX := Loc.X + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := Loc.Y + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY, $0, Step);
end;


procedure TRenderPool.AddHouseWork(aHouse: THouseType; Loc: TKMPoint; aActSet: THouseActionSet; AnimStep: Cardinal; FlagColor: TColor4);
var
  ID: Cardinal;
  AT: THouseActionType;
  A: TKMHouseAnim;
  R: TRXData;
  CornerX, CornerY: Single;
begin
  if aActSet = [] then Exit;

  R := fRXData[rxHouses];

  //See if action is in set and render it
  for AT := Low(THouseActionType) to High(THouseActionType) do
  if AT in aActSet then
  begin
    A := fResource.HouseDat[aHouse].Anim[AT];
    if A.Count <> 0 then
    begin
      ID := A.Step[AnimStep mod A.Count + 1] + 1;
      CornerX := Loc.X + (R.Pivot[ID].X + A.MoveX) / CELL_SIZE_PX - 1;
      CornerY := Loc.Y + (R.Pivot[ID].Y + A.MoveY + R.Size[ID].Y) / CELL_SIZE_PX - 1
                       - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
      fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY, FlagColor);
    end;
  end;
end;


procedure TRenderPool.AddHouseSupply(aHouse: THouseType; Loc: TKMPoint; const R1,R2:array of byte);
var ID,i,k: Integer;
  R: TRXData;

  procedure AddHouseSupplySprite(aID: Integer);
  var CornerX,CornerY: Single;
  begin
    if aID > 0 then
    begin
      CornerX := Loc.X + R.Pivot[aID].X / CELL_SIZE_PX - 1;
      CornerY := Loc.Y + (R.Pivot[aID].Y + R.Size[aID].Y) / CELL_SIZE_PX - 1
                       - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
      fRenderList.AddSprite(rxHouses, aID, CornerX, CornerY);
    end;
  end;

begin
  R := fRXData[rxHouses];

  for i := 1 to 4 do
  if (R1[i - 1]) > 0 then
  begin
    ID := fResource.HouseDat[aHouse].SupplyIn[i, Min(R1[i - 1], 5)] + 1;
    AddHouseSupplySprite(ID);
  end;

  for i := 1 to 4 do
  if (R2[i - 1]) > 0 then
  begin
    //Exception for some houses that render layered
    if aHouse in [ht_WeaponSmithy, ht_ArmorSmithy, ht_WeaponWorkshop, ht_ArmorWorkshop] then
      for k := 1 to Min(R2[i - 1], 5) do
      begin
        ID := fResource.HouseDat[aHouse].SupplyOut[i, k] + 1;
        AddHouseSupplySprite(ID);
      end
    else
    begin
      ID := fResource.HouseDat[aHouse].SupplyOut[i, Min(R2[i - 1], 5)] + 1;
      AddHouseSupplySprite(ID);
    end;
  end;
end;


procedure TRenderPool.AddHouseMarketSupply(Loc: TKMPoint; ResType: TResourceType; ResCount:word; AnimStep: Integer);
var i,ID: Integer;
  CornerX,CornerY: Single; R: TRXData;
begin
  if ResType = rt_Horse then //Horses are a beast, BeastID is the count, age is 1
    for i:=1 to Min(ResCount, MarketWares[ResType].Count) do //Render each beast
      AddHouseStableBeasts(ht_Marketplace, Loc, i, 1, AnimStep, rxGame) //Use RXGame
  else
  begin
    if MarketWares[ResType].Count = 0 then exit;
    ID := (MarketWares[ResType].TexStart-1) + Min(ResCount, MarketWares[ResType].Count);
    if ID = 0 then Exit;

    R := fRXData[rxGame];
    CornerX := Loc.X + (R.Pivot[ID].X + MarketWaresOffsetX) / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (R.Pivot[ID].Y + MarketWaresOffsetY + R.Size[ID].Y) / CELL_SIZE_PX - 1
                     - fTerrain.Land[Loc.Y+1,Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxGame, ID, CornerX, CornerY);
  end;
end;


procedure TRenderPool.AddHouseStableBeasts(aHouse: THouseType; Loc: TKMPoint; BeastID,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
var
  CornerX, CornerY: Single;
  ID: Integer;
  R: TRXData;
  BA: TKMHouseBeastAnim;
begin
  R := fRXData[aRX];

  BA := fResource.HouseDat.BeastAnim[aHouse,BeastID,BeastAge];

  ID := BA.Step[AnimStep mod BA.Count + 1] + 1;
  CornerX := Loc.X + (BA.MoveX + R.Pivot[ID].X) / CELL_SIZE_PX - 1;
  CornerY := Loc.Y + (BA.MoveY + R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(aRX, ID, CornerX, CornerY);
end;


procedure TRenderPool.AddUnit(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  CornerX, CornerY, Ground: Single;
  ID, ID0: Integer;
  A: TKMUnitsAnim;
  R: TRXData;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  ID0 := A.Step[UnitStillFrames[aDir] mod A.Count + 1] + 1;
  if ID <= 0 then exit;
  R := fRXData[rxUnits];

  CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := pY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV;
  Ground := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;

  if NewInst then
    fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY, pX, Ground, FlagColor)
  else
    fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY, FlagColor);

  if DoImmediateRender then
    RenderSprite(rxUnits, ID, CornerX, CornerY, FlagColor, 255, Deleting);

  //if SHOW_UNIT_MOVEMENT and fGame.AllowDebugRendering then
  if NewInst then
  begin
    fRenderAux.Dot(pX, pY - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV, FlagColor);
    fRenderAux.Dot(CornerX, CornerY, $FF000080);
  end;
end;


procedure TRenderPool.AddHouseEater(Loc: TKMPoint; aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; OffX,OffY: Single; FlagColor: TColor4);
var
  CornerX, CornerY: Single;
  ID: Integer;
  A: TKMUnitsAnim;
  R: TRXData;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then exit;
  R := fRXData[rxUnits];

  //Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  CornerX := Loc.X + OffX + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := Loc.Y + OffY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY, FlagColor);
end;


procedure TRenderPool.AddUnitCarry(aCarry: TResourceType; aDir: TKMDirection; StepID: Integer; pX,pY: Single);
var
  CornerX, CornerY: Single;
  ID: Integer;
  A: TKMUnitsAnim;
  R: TRXData;
begin
  A := fResource.UnitDat.SerfCarry[aCarry, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then Exit;
  R := fRXData[rxUnits];

  CornerX := pX + (R.Pivot[ID].X + a.MoveX) / CELL_SIZE_PX;
  CornerY := pY + (R.Pivot[ID].Y + R.Size[ID].Y + a.MoveY) / CELL_SIZE_PX
                - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY);
end;


procedure TRenderPool.AddUnitThought(Thought: TUnitThought; pX,pY: Single);
var
  ID: Integer;
  CornerX, CornerY: Single;
  R: TRXData;
begin
  if Thought = th_None then Exit;
  R := fRXData[rxUnits];

  //Thought bubbles are animated in reverse
  ID := ThoughtBounds[Thought, 2] + 1 -
       (fGame.GameTickCount mod word(ThoughtBounds[Thought, 2] - ThoughtBounds[Thought, 1]));

  CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := pY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1.5
                - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY);
end;


procedure TRenderPool.AddUnitFlag(
  aUnit: TUnitType; aAct: TUnitActionType;
  aDir: TKMDirection; UnitAnim, FlagAnim: Integer;
  pX, pY: Single; FlagColor, TeamColor: TColor4);
var
  R: TRXData;
  A: TKMUnitsAnim;
  ID0, IDUnit, IDFlag: Integer;
  FlagX, FlagY, CornerX, CornerY, Ground: Single;
begin
  R := fRXData[rxUnits];

  //Unit position
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  IDUnit := A.Step[UnitAnim mod A.Count + 1] + 1;
  ID0 := A.Step[UnitStillFrames[aDir] mod A.Count+1] + 1;
  if IDUnit <= 0 then Exit;

  CornerX := pX + R.Pivot[IDUnit].X / CELL_SIZE_PX;
  CornerY := pY + (R.Pivot[IDUnit].Y + R.Size[IDUnit].Y) / CELL_SIZE_PX
                - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV;
  Ground := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;

  //Flag position
  A := fResource.UnitDat[aUnit].UnitAnim[ua_WalkArm, aDir];
  IDFlag := A.Step[FlagAnim mod A.Count + 1] + 1;
  if IDFlag <= 0 then Exit;

  FlagX := pX + (R.Pivot[IDFlag].X + FlagXOffset[UnitGroups[aUnit], aDir]) / CELL_SIZE_PX - 0.5;
  FlagY := pY + (R.Pivot[IDFlag].Y + FlagYOffset[UnitGroups[aUnit], aDir] + R.Size[IDFlag].Y) / CELL_SIZE_PX
              - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV - 2.25;

  if aDir in [dir_SE, dir_S, dir_SW, dir_W] then
  begin
    fRenderList.AddSprite(rxUnits, IDFlag, FlagX, FlagY, pX, Ground, FlagColor);
    fRenderList.AddSprite(rxUnits, IDUnit, CornerX, CornerY, TeamColor);
  end
  else
  begin
    fRenderList.AddSprite(rxUnits, IDUnit, CornerX, CornerY, pX, Ground, TeamColor);
    fRenderList.AddSprite(rxUnits, IDFlag, FlagX, FlagY, FlagColor);
  end;

  if SHOW_UNIT_MOVEMENT and fGame.AllowDebugRendering then
    fRenderAux.Dot(pX, pY - fTerrain.HeightAt(pX, pY) / CELL_HEIGHT_DIV, FlagColor); // Render dot where unit is
end;


procedure TRenderPool.AddUnitWithDefaultArm(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  if aUnit = ut_Fish then aAct := FishCountAct[5]; //In map editor always render 5 fish
  AddUnit(aUnit,aAct,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
  if fResource.UnitDat[aUnit].SupportsAction(ua_WalkArm) then
    AddUnit(aUnit,ua_WalkArm,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
end;


{Render one terrian cell}
procedure TRenderPool.RenderTile(Index: Byte; pX,pY,Rot: Integer);
var k,i,a: Integer;
  TexC:array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO:array[1..4]of byte;         //order of UV coordinates, for rotations
begin
  if not fTerrain.TileInMapCoords(pX,pY) then exit;

  glColor4f(1,1,1,1);
  glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextT);

  TexC[1,1] := (Index mod 16  )/16; TexC[1,2]:=(Index div 16  )/16;
  TexC[2,1] := (Index mod 16  )/16; TexC[2,2]:=(Index div 16+1)/16;
  TexC[3,1] := (Index mod 16+1)/16; TexC[3,2]:=(Index div 16+1)/16;
  TexC[4,1] := (Index mod 16+1)/16; TexC[4,2]:=(Index div 16  )/16;
  TexO[1]:=1; TexO[2]:=2; TexO[3]:=3; TexO[4]:=4;

  if Rot and 1 = 1 then begin a:=TexO[1]; TexO[1]:=TexO[2]; TexO[2]:=TexO[3]; TexO[3]:=TexO[4]; TexO[4]:=a; end; // 90 2-3-4-1
  if Rot and 2 = 2 then begin a:=TexO[1]; TexO[1]:=TexO[3]; TexO[3]:=a; a:=TexO[2]; TexO[2]:=TexO[4]; TexO[4]:=a; end; // 180 3-4-1-2

  k:=pX; i:=pY;
  glBegin(GL_QUADS);
  with fTerrain do
    if RENDER_3D then begin
      glTexCoord2fv(@TexC[TexO[1]]); glVertex3f(k-1,i-1,-Land[i,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[2]]); glVertex3f(k-1,i  ,-Land[i+1,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[3]]); glVertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[4]]); glVertex3f(k  ,i-1,-Land[i,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord2fv(@TexC[TexO[1]]); glVertex3f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord2fv(@TexC[TexO[2]]); glVertex3f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord2fv(@TexC[TexO[3]]); glVertex3f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV, -i);
      glTexCoord2fv(@TexC[TexO[4]]); glVertex3f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV, -i);
    end;
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRenderPool.RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
var
  Lay, TopLay: Byte;
  F: TColor4;
begin
  //If there's AltID - render 2 layers instead of ordinary 1
  TopLay := 1 + Byte(GFXData[aRX,aID].AltID <> 0);

  for Lay := 1 to TopLay do
  with GFXData[aRX, aID] do
  begin
    if Lay = 1 then
    begin
      glColor3ub(aFOW, aFOW, aFOW);
      glBindTexture(GL_TEXTURE_2D, TexID);
    end else
    if (Lay = 2) and (aFOW <> 0) then  //Don't render colorflags if they aren't visible cos of FOW
    begin
      //Multiply RGB component of flag color by FOW
      F := ((Col and $FF) * aFOW shr 8) or
           ((((Col shr 8) and $FF) * aFOW shr 8) shl 8) or
           ((((Col shr 16) and $FF) * aFOW shr 8) shl 16) or
           Col and $FF000000;
      glColor4ubv(@F);
      glBindTexture(GL_TEXTURE_2D, AltID);
    end;

    if HighlightRed then glColor4f(1,0,0,1);

    glBegin(GL_QUADS);
      glTexCoord2f(u1, v2); glVertex2f(pX                     , pY                      );
      glTexCoord2f(u2, v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
      glTexCoord2f(u2, v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(u1, v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  glBindTexture(GL_TEXTURE_2D, 0);

  if SHOW_SPRITES_RECT then
  begin
    glPushAttrib(GL_LINE_BIT);
      glLineWidth(1);
      glColor4f(1,1,1,0.5);
      glBegin(GL_LINE_LOOP);
        with GFXData[aRX, aID] do
          glkRect(pX, pY, pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
      glEnd;
    glPopAttrib;
  end;
end;


procedure TRenderPool.RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param: Single; pX,pY: Single; aFOW: Byte);
begin
  //NOTION: This function does not work on some GPUs will need to replace it with simplier more complicated way
  //glDisable(GL_BLEND);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 1-Param);
  glBlendFunc(GL_ONE, GL_ZERO);

  with GFXData[aRX,aID] do
  begin
    glColor3ub(aFOW, aFOW, aFOW);
    glBindTexture(GL_TEXTURE_2D, TexID);
    glBegin(GL_QUADS);
      glTexCoord2f(u1,v2); glVertex2f(pX-1                     ,pY-1         );
      glTexCoord2f(u2,v2); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
      glTexCoord2f(u2,v1); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(u1,v1); glVertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  glDisable(GL_ALPHA_TEST);
  glAlphaFunc(GL_ALWAYS,0);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  //glEnable(GL_BLEND);

  if SHOW_SPRITES_RECT then
  begin
    glPushAttrib(GL_LINE_BIT);
    glLineWidth(1);
    glColor3f(1,1,1);
    glBegin(GL_LINE_LOOP);
      with GFXData[aRX,aID] do
        glkRect(pX-1,pY-1,pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glPopAttrib;
  end;
end;


procedure TRenderPool.RenderTerrain;
var
  Rect: TKMRect;
  Passability: integer;
begin
  Rect := fGame.Viewport.GetClip;

  RenderTerrainTiles(Rect, fTerrain.AnimStep);
  RenderTerrainFieldBorders(Rect);
  RenderTerrainObjects(Rect, fTerrain.AnimStep);

  if not fGame.AllowDebugRendering then
    exit;

  if SHOW_TERRAIN_WIRES then
    fRenderAux.Wires(Rect);

  if SHOW_TERRAIN_WIRES then
  begin
    Passability := fGame.FormPassability;
    if fGame.fMapEditorInterface <> nil then
      Passability := max(Passability, fGame.fMapEditorInterface.ShowPassability);
    fRenderAux.Passability(Rect, Passability);
  end;

  if SHOW_UNIT_MOVEMENT then
    fRenderAux.UnitMoves(Rect);
end;


procedure TRenderPool.RenderTerrainMarkup(aLocX, aLocY: Word; aFieldType: TFieldType);
var
  a,b: TKMPointF;
  ID: Integer;
  FOW: Byte;
begin
  case aFieldType of
    ft_Road:  ID := 105; // Road
    ft_Corn:  ID := 107; // Field
    ft_Wine:  ID := 108; // Wine
    ft_Wall:  ID := 111; // Wall
  else
    Exit; // WTF?
  end;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(aLocX, aLocY, true);

  glColor3ub(FOW, FOW, FOW);
  glBindTexture(GL_TEXTURE_2D, GFXData[rxGui, ID].TexID);

  a.X := GFXData[rxGui, ID].u1;
  a.Y := GFXData[rxGui, ID].v1;
  b.X := GFXData[rxGui, ID].u2;
  b.Y := GFXData[rxGui, ID].v2;

  glBegin(GL_QUADS);
    glTexCoord2f(b.x,a.y); glVertex2f(aLocX-1, aLocY-1 - fTerrain.Land[aLocY  ,aLocX  ].Height/CELL_HEIGHT_DIV+0.10);
    glTexCoord2f(a.x,a.y); glVertex2f(aLocX-1, aLocY-1 - fTerrain.Land[aLocY  ,aLocX  ].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(a.x,b.y); glVertex2f(aLocX  , aLocY   - fTerrain.Land[aLocY+1,aLocX+1].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(b.x,b.y); glVertex2f(aLocX  , aLocY   - fTerrain.Land[aLocY+1,aLocX+1].Height/CELL_HEIGHT_DIV);

    glTexCoord2f(b.x,a.y); glVertex2f(aLocX-1, aLocY   - fTerrain.Land[aLocY+1,aLocX  ].Height/CELL_HEIGHT_DIV);
    glTexCoord2f(a.x,a.y); glVertex2f(aLocX-1, aLocY   - fTerrain.Land[aLocY+1,aLocX  ].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(a.x,b.y); glVertex2f(aLocX  , aLocY-1 - fTerrain.Land[aLocY  ,aLocX+1].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(b.x,b.y); glVertex2f(aLocX  , aLocY-1 - fTerrain.Land[aLocY  ,aLocX+1].Height/CELL_HEIGHT_DIV+0.10);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRenderPool.RenderTerrainBorder(Border: TBorderType; Pos: TKMDirection; pX,pY: Integer);
var a,b: TKMPointF; ID: Integer; t: Single; HeightInPx: Integer; FOW: Byte;
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
    glBindTexture(GL_TEXTURE_2D,GFXData[rxGui,ID].TexID);
    a.x:=GFXData[rxGui,ID].u1; a.y:=GFXData[rxGui,ID].v1;
    b.x:=GFXData[rxGui,ID].u2; b.y:=GFXData[rxGui,ID].v2;
    t:=GFXData[rxGui,ID].PxWidth/CELL_SIZE_PX; //Height of border
    glBegin(GL_QUADS);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY,true);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(b.x,a.y); glVertex2f(pX-1, pY-1+t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,a.y); glVertex2f(pX-1, pY-1-t/2 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX+1,pY,true);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(a.x,b.y); glVertex2f(pX  , pY-1-t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,b.y); glVertex2f(pX  , pY-1+t/2 - fTerrain.Land[pY,pX+1].Height/CELL_HEIGHT_DIV);
    glEnd;
  end
  else begin //Vertical border
    glBindTexture(GL_TEXTURE_2D,GFXData[rxGui,ID].TexID);
    HeightInPx := Round ( CELL_SIZE_PX * (1 + (fTerrain.Land[pY,pX].Height - fTerrain.Land[pY+1,pX].Height)/CELL_HEIGHT_DIV) );
    a.x:=GFXData[rxGui,ID].u1; a.y:=GFXData[rxGui,ID].v1;
    b.x:=GFXData[rxGui,ID].u2; b.y:=GFXData[rxGui,ID].v2 * (HeightInPx / GFXData[rxGui,ID].PxHeight);
    t:=GFXData[rxGui,ID].PxWidth/CELL_SIZE_PX; //Width of border
    glBegin(GL_QUADS);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY,true);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(a.x,a.y); glVertex2f(pX-1-t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(b.x,a.y); glVertex2f(pX-1+t/2, pY-1 - fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV);
      FOW:=MyPlayer.FogOfWar.CheckVerticeRevelation(pX,pY+1,true);
      glColor3ub(FOW,FOW,FOW);
      glTexCoord2f(b.x,b.y); glVertex2f(pX-1+t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
      glTexCoord2f(a.x,b.y); glVertex2f(pX-1-t/2, pY   - fTerrain.Land[pY+1,pX].Height/CELL_HEIGHT_DIV);
    glEnd;
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRenderPool.RenderCursorWireQuad(P: TKMPoint; Col: TColor4);
begin
  if not fTerrain.TileInMapCoords(P.X, P.Y) then exit;
  glColor4ubv(@Col);
  glBegin(GL_LINE_LOOP);
    with fTerrain do begin
      glVertex2f(P.X-1,P.Y-1-Land[P.Y  ,P.X  ].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X  ,P.Y-1-Land[P.Y  ,P.X+1].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X  ,P.Y-  Land[P.Y+1,P.X+1].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X-1,P.Y-  Land[P.Y+1,P.X  ].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


procedure TRenderPool.RenderCursorBuildIcon(aLoc: TKMPoint; aID: Integer=479);
begin
  if fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
    RenderSprite(rxGui, aID, aLoc.X - 0.8, aLoc.Y - 0.2 -
      fTerrain.HeightAt(aLoc.X - 0.5, aLoc.Y - 0.5) / CELL_HEIGHT_DIV,
      $FFFFFFFF, 255);
end;


procedure TRenderPool.RenderCursorWireHousePlan(P: TKMPoint; aHouseType: THouseType);
var
  I: Integer;
  MarksList: TKMPointTagList;
begin
  MarksList := TKMPointTagList.Create;
  MarksList.AllowDuplicates := True; //Enterance can have duplicates: cyan quad + door icon
  MyPlayer.GetHouseMarks(P, aHouseType, MarksList);

  for I := 0 to MarksList.Count - 1 do
  if MarksList.Tag[I] = 0 then
    RenderCursorWireQuad(MarksList[I], $FFFFFF00) //Cyan rect
  else
    RenderCursorBuildIcon(MarksList[I], MarksList.Tag[I]); //icon

  MarksList.Free;
end;


procedure TRenderPool.RenderCursorHighlights;
var
  P: TKMPoint;
  U: TKMUnit;
begin
  if GameCursor.Cell.Y*GameCursor.Cell.X = 0 then exit; //Caused a rare crash

  P := GameCursor.Cell;

  if (GameCursor.Mode <> cm_None) and (GameCursor.Mode <> cm_Houses) and
     (MyPlayer.FogOfWar.CheckTileRevelation(P.X, P.Y, False) = 0) then
    RenderCursorBuildIcon(P)       //Red X
  else

  with fTerrain do
  case GameCursor.Mode of
    cm_None:   ;
    cm_Erase:   case fGame.GameState of
                  gsEditor:
                    begin
                      //With Units tab see if there's a unit below cursor
                      if (fGame.fMapEditorInterface.GetShownPage = esp_Units) then
                      begin
                        U := fTerrain.UnitsHitTest(P.X, P.Y);
                        if U <> nil then
                          AddUnitWithDefaultArm(U.UnitType,ua_Walk,U.Direction,U.AnimStep,P.X+0.5,P.Y+1,MyPlayer.FlagColor,true,true);
                      end
                      else
                      if (
                            //With Buildings tab see if we can remove Fields or Houses
                             (fGame.fMapEditorInterface.GetShownPage = esp_Buildings)
                             and (    TileIsCornField(P)
                                   or TileIsWineField(P)
                                   or (Land[P.Y,P.X].TileOverlay=to_Road)
                                   or (fPlayers.HousesHitTest(P.X, P.Y) <> nil))
                         )
                      //And of course it is visible
                      then
                        RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                      else
                        RenderCursorBuildIcon(P); //Red X
                    end;

                  gsPaused, gsOnHold, gsRunning:
                    begin
                      if ((MyPlayer.BuildList.FieldworksList.HasFakeField(P) <> ft_None)
                          or MyPlayer.BuildList.HousePlanList.HasPlan(P)
                          or (MyPlayer.HousesHitTest(P.X, P.Y) <> nil))
                      then
                        RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                      else
                        RenderCursorBuildIcon(P); //Red X
                    end;
                end;
    cm_Road:    if MyPlayer.CanAddFakeFieldPlan(P, ft_Road) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cm_Field:   if MyPlayer.CanAddFakeFieldPlan(P, ft_Corn) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cm_Wine:    if MyPlayer.CanAddFakeFieldPlan(P, ft_Wine) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cm_Wall:    if MyPlayer.CanAddFakeFieldPlan(P, ft_Wall) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cm_Houses:  RenderCursorWireHousePlan(P, THouseType(GameCursor.Tag1)); //Cyan quads and red Xs
    cm_Tiles:   if GameCursor.Tag2 in [0..3] then
                  RenderTile(GameCursor.Tag1, P.X, P.Y, GameCursor.Tag2)
                else
                  RenderTile(GameCursor.Tag1, P.X, P.Y, (fTerrain.AnimStep div 5) mod 4); //Spin it slowly so player remembers it is on randomized
    cm_Objects: begin
                  //If there's object below - paint it in Red
                  RenderObjectOrQuad(fTerrain.Land[P.Y,P.X].Obj+1, fTerrain.AnimStep, P.X, P.Y, true, true);
                  RenderObjectOrQuad(GameCursor.Tag1+1, fTerrain.AnimStep, P.X, P.Y, true);
                end;
    cm_Height:  fRenderAux.Circle(GameCursor.Float.X,GameCursor.Float.Y - fTerrain.HeightAt(GameCursor.Float)/CELL_HEIGHT_DIV, (GameCursor.Tag1 and $F) div 2, $00000000,  $FFFFFFFF);
    cm_Units:   if CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                  AddUnitWithDefaultArm(TUnitType(GameCursor.Tag1),ua_Walk,dir_S,UnitStillFrames[dir_S],P.X-0.5,P.Y,MyPlayer.FlagColor,true)
                else
                  RenderCursorBuildIcon(P); //Red X
  end;
end;


{ TRenderList }
constructor TRenderList.Create;
begin
  inherited;
  fCount := 0;
  SetLength(RenderList, 512); //Allocate some space
end;


destructor TRenderList.Destroy;
begin
  SetLength(RenderList, 0);
  inherited;
end;


procedure TRenderList.ClipRenderList;
var I: Integer;
begin
  SetLength(RenderOrder, fCount);

  for I := 0 to fCount - 1 do
  if RenderList[I].NewInst then
  begin
    RenderOrder[I] := I;
    RenderList[I].FOWvalue := MyPlayer.FogOfWar.CheckRevelation(RenderList[I].Feet, True);

    //We rendered only houses under FOW to see their rooftops
    //But we might as well render everything for consistency
    //if (RenderList[I].FOWvalue <= 128) and RenderList[I].IsUnit then
    //  RenderOrder[I] := -1;}
  end else begin
    RenderOrder[I] := -1;
    RenderList[I].FOWvalue := RenderList[I-1].FOWvalue; //Take from previous
  end;
end;


{Need to sort all items in list from top-right to bottom-left}
procedure TRenderList.SortRenderList;
var I,K: Integer;
begin
  for I := 0 to fCount - 1 do
    if RenderOrder[I] <> -1 then //Exclude child sprites from comparison
      for K := I + 1 to fCount - 1 do
        if RenderOrder[K] <> -1 then
          if (RenderList[RenderOrder[K]].Feet.Y < RenderList[RenderOrder[I]].Feet.Y)
          or((RenderList[RenderOrder[K]].Feet.Y = RenderList[RenderOrder[I]].Feet.Y)
          and(RenderList[RenderOrder[K]].Loc.X > RenderList[RenderOrder[I]].Loc.X))
          then //TopMost Rightmost
            SwapInt(RenderOrder[K], RenderOrder[I])
end;


//New items must provide their ground level
procedure TRenderList.AddSprite(aRX: TRXType; aID: Word; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then SetLength(RenderList, fCount + 256); //Book some space

  RenderList[fCount].Loc        := KMPointF(pX, pY); //Position of sprite, floating-point
  RenderList[fCount].Feet       := KMPointF(gX, gY); //Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             //RX library
  RenderList[fCount].ID         := aID;             //Texture ID
  RenderList[fCount].NewInst    := True;            //Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           //Team ID (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      //Alpha step for wip buildings
  RenderList[fCount].FOWvalue   := 255;             //Visibility recomputed in ClipRender anyway

  inc(fCount); //New item added
end;


//Child items don't need ground level
procedure TRenderList.AddSprite(aRX: TRXType; aID: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then SetLength(RenderList, fCount + 256); //Book some space

  RenderList[fCount].Loc        := KMPointF(pX,pY); //Position of sprite, floating-point
  RenderList[fCount].Feet       := KMPointF(0, 0);  //Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             //RX library
  RenderList[fCount].ID         := aID;             //Texture ID
  RenderList[fCount].NewInst    := False;            //Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           //Team ID (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      //Alpha step for wip buildings
  RenderList[fCount].FOWvalue   := 255;             //Visibility recomputed in ClipRender anyway

  inc(fCount); //New item added
end;


{Now render all these items from list}
procedure TRenderList.Render;
var i,h: Integer;
begin
  ClipRenderList; //Clip invisible items, Mark child items (RenderOrder[i] := -1), Apply FOW
  SortRenderList; //Sort items overlaying

  fStat_Sprites := fCount;
  fStat_Sprites2 := 0;

  for i := 0 to fCount - 1 do
  if RenderOrder[i] <> -1 then
  begin
    h := RenderOrder[i];
    glPushMatrix;

      if RENDER_3D then
      begin
        glTranslatef(RenderList[h].Loc.X, RenderList[h].Loc.Y, 0);
        glRotatef(fRenderPool.rHeading, -1, 0, 0);
        glTranslatef(-RenderList[h].Loc.X, -RenderList[h].Loc.Y, 0);
      end;

      repeat //Render child sprites only after their parent
        with RenderList[h] do
        begin
          if AlphaStep = -1 then
            fRenderPool.RenderSprite(RX, ID, Loc.X, Loc.Y, TeamColor, FOWvalue)
          else
            fRenderPool.RenderSpriteAlphaTest(RX, ID, AlphaStep, Loc.X, Loc.Y, FOWvalue);

          if SHOW_GROUND_LINES and NewInst then //Don't render child (not NewInst) ground lines, since they are unused
          begin
            glBegin(GL_LINES);
              glColor3f(1,1,0.5);
              glVertex2f(Feet.X + 0.15, Feet.Y - fTerrain.HeightAt(Feet) / CELL_HEIGHT_DIV);
              glVertex2f(Feet.X - 0.15, Feet.Y - fTerrain.HeightAt(Feet) / CELL_HEIGHT_DIV);
            glEnd;
          end;
        end;
        inc(h);
        inc(fStat_Sprites2);
      until ((h = fCount) or RenderList[h].NewInst);
    glPopMatrix;
  end;
  fCount := 0;
end;


end.
