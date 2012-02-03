unit KM_Render;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  {$IFDEF WDC} JPEG, {$ENDIF} //Lazarus doesn't have JPEG library ye-> FPReadJPEG?t
  KM_Defaults, KM_CommonClasses, KM_RenderSetup, KM_ResourceSprites, KM_Points;

type
  TRenderList = class
  private
    fCount: Word;
    RenderOrder: array of smallint; //Order in which sprites will be drawn ()
    RenderList: array of record
      Loc: TKMPointF;
      Ground: Single;
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
    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY,aGround: Single; aNew: Boolean; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    property Stat_Sprites: Integer read fStat_Sprites;
    property Stat_Sprites2: Integer read fStat_Sprites2;
    procedure Render;
  end;

  //Game Renderer
  TRender = class
  private
    fSetup: TRenderSetup;
    rPitch,rHeading,rBank:integer;
    fRenderList: TRenderList;
    procedure RenderTile(Index: Byte; pX,pY,Rot: Integer);
    procedure RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param: Single; pX,pY: Single; aFOW: Byte);
    procedure RenderTerrainMarkup(aLocX, aLocY: Word; aFieldType: TFieldType);
    procedure RenderTerrainBorder(Border: TBorderType; Pos: TKMDirection; pX,pY: Integer);
    procedure RenderObjectOrQuad(aIndex,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
    procedure RenderObject(aIndex:Integer; AnimStep:Cardinal; pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
    procedure RenderObjectQuad(aIndex:Integer; AnimStep:Cardinal; pX,pY:integer; IsDouble:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);

    //Terrain rendering sub-class
    procedure RenderTerrain;
    procedure RenderTerrainTiles(x1,x2,y1,y2,AnimStep:integer);
    procedure RenderTerrainFieldBorders(x1,x2,y1,y2:integer);
    procedure RenderTerrainObjects(x1,x2,y1,y2:Integer; AnimStep: Cardinal);

    //Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderCursorHighlights;
    procedure RenderCursorWireQuad(P:TKMPoint; Col:TColor4);
    procedure RenderCursorBuildIcon(P:TKMPoint; id:integer=479);
    procedure RenderCursorWireHousePlan(P:TKMPoint; aHouseType:THouseType);

    procedure RenderBrightness(Value: Byte);
  public
    constructor Create(aSetup: TRenderSetup);
    destructor Destroy; override;

    property RenderList: TRenderList read fRenderList;

    procedure SetRotation(aH,aP,aB:integer);
    procedure DoPrintScreen(FileName: string);

    procedure Render;

    procedure AddProjectile(aProj: TProjectileType; pX,pY: Single; Flight: Single; Dir: TKMDirection);
    procedure AddHouseTablet(Index:THouseType; Loc:TKMPoint);
    procedure AddHouseBuildSupply(Index:THouseType; Wood,Stone:byte; Loc:TKMPoint);
    procedure AddHouseWood(Index:THouseType; Step:single; Loc:TKMPoint);
    procedure AddHouseStone(Index:THouseType; Step:single; Loc:TKMPoint);
    procedure AddHouseWork(aHouse:THouseType; aActSet:THouseActionSet; AnimStep:cardinal; Loc:TKMPoint; FlagColor:TColor4);
    procedure AddHouseSupply(Index:THouseType; const R1,R2:array of byte; Loc:TKMPoint);
    procedure AddMarketSupply(ResType:TResourceType; ResCount:word; Loc:TKMPoint; AnimStep:integer);
    procedure AddHouseStableBeasts(Index:THouseType; BeastID,BeastAge,AnimStep:integer; Loc:TKMPoint; aRX: TRXType = rxHouses);
    procedure AddEater(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; Loc:TKMPoint; OffX,OffY:single; FlagColor:TColor4);
    procedure AddUnit(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
    procedure AddUnitCarry(aCarry:TResourceType; aDir:TKMDirection; StepID:integer; pX,pY:single);
    procedure AddUnitThought(Thought:TUnitThought; pX,pY:single);
    procedure AddUnitFlag(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; UnitX,UnitY:single; NewInst:boolean);
    procedure AddUnitWithDefaultArm(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; DoImmediateRender:boolean=false; Deleting:boolean=false);
  end;


var
  fRender: TRender;


implementation
uses KM_RenderAux, KM_Terrain, KM_PlayersCollection, KM_Game, KM_Sound, KM_Resource, KM_ResourceUnit, KM_ResourceHouse, KM_Units;


constructor TRender.Create(aSetup: TRenderSetup);
begin
  Inherited Create;

  fSetup := aSetup;
  fRenderList := TRenderList.Create;
end;


destructor TRender.Destroy;
begin
  fRenderList.Free;
  inherited;
end;


procedure TRender.SetRotation(aH,aP,aB:integer);
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
procedure TRender.Render;
begin
  fSetup.BeginFrame;

  if fGame = nil then exit; //Happens sometimes during ToggleFullScreen

  if fGame.GameState in [gsPaused, gsOnHold, gsRunning, gsReplay, gsEditor] then
  begin //If game is running
    glLoadIdentity; // Reset The View
    //glRotate(-15,0,0,1); //Funny thing
    glTranslatef(fGame.Viewport.ViewportClip.X/2, fGame.Viewport.ViewportClip.Y/2, 0);
    glkScale(fGame.Viewport.Zoom*CELL_SIZE_PX);
    glTranslatef(-fGame.Viewport.Position.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fGame.Viewport.Zoom, -fGame.Viewport.Position.Y, 0);
    if RENDER_3D then
    begin
      fSetup.SetRenderMode(rm3D);

      glkScale(-CELL_SIZE_PX/14);
      glRotatef(rHeading,1,0,0);
      glRotatef(rPitch  ,0,1,0);
      glRotatef(rBank   ,0,0,1);
      glTranslatef(-fGame.Viewport.Position.X+TOOLBAR_WIDTH/CELL_SIZE_PX/fGame.Viewport.Zoom, -fGame.Viewport.Position.Y-8, 10);
      glkScale(fGame.Viewport.Zoom);
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

  fSetup.SetRenderMode(rm2D);
  fGame.PaintInterface;

  glLoadIdentity;
  RenderBrightness(fGame.GlobalSettings.Brightness);

  fSetup.EndFrame;
end;


procedure TRender.DoPrintScreen(FileName:string);
{$IFDEF WDC}
var
  i, k, W, H: integer;
  jpg: TJpegImage;
  mkbmp: TBitMap;
  bmp: array of Cardinal;
{$ENDIF}
begin
{$IFDEF WDC}
  W := fSetup.ScreenX;
  H := fSetup.ScreenY;

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


procedure TRender.RenderTerrainTiles(x1,x2,y1,y2,AnimStep:integer);
  procedure LandLight(a:single);
  begin
    glColor4f(a/1.5+0.5,a/1.5+0.5,a/1.5+0.5,Abs(a*2)); //Balanced looks
    //glColor4f(a*2,a*2,a*2,Abs(a*2)); //Deeper shadows
  end;
var
  i,k,iW: Integer;
  ID,rd,Rot: Byte;
  xt,a: Integer;
  TexC: array[1..4,1..2]of GLfloat; //Texture UV coordinates
  TexO: array[1..4]of byte;         //order of UV coordinates, for rotations
begin

  for iW:=1 to 1+3*byte(MAKE_ANIM_TERRAIN) do begin //Each new layer inflicts 10% fps drop
    case iW of
      1: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextT);
      2: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextW[AnimStep mod 8 + 1]);
      3: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextS[AnimStep mod 24 div 8 + 1]); //These should be unsynced later on
      4: glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextF[AnimStep mod 5 + 1]);
    end;
    glBegin(GL_QUADS);
      with fTerrain do
      for i:=y1 to y2 do for k:=x1 to x2 do
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

          glTexCoord2fv(@TexC[TexO[1]]); glVertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[2]]); glVertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[3]]); glVertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
          glTexCoord2fv(@TexC[TexO[4]]); glVertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);

          if KAM_WATER_DRAW and (iW=1) and fTerrain.TileIsWater(KMPoint(k,i)) then
          begin
            xt := 32;
            TexC[1,1] := (xt mod 16  )/16; TexC[1,2] := (xt div 16  )/16;
            TexC[2,1] := (xt mod 16  )/16; TexC[2,2] := (xt div 16+1)/16;
            TexC[3,1] := (xt mod 16+1)/16; TexC[3,2] := (xt div 16+1)/16;
            TexC[4,1] := (xt mod 16+1)/16; TexC[4,2] := (xt div 16  )/16;
            TexO[1] := 1; TexO[2] := 2; TexO[3] := 3; TexO[4] := 4;

            LandLight(Land[i  ,k  ].Light);
            glTexCoord2fv(@TexC[TexO[1]]); glVertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
            LandLight(Land[i+1,k  ].Light);
            glTexCoord2fv(@TexC[TexO[2]]); glVertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
            LandLight(Land[i+1,k+1].Light);
            glTexCoord2fv(@TexC[TexO[3]]); glVertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
            LandLight(Land[i  ,k+1].Light);
            glTexCoord2fv(@TexC[TexO[4]]); glVertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
          end;
        end;
      end;
    glEnd;
  end;

  glColor4f(1,1,1,1);

  for i:=y1 to y2 do for k:=x1 to x2 do
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
  glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextG);
  glBegin(GL_QUADS);
  with fTerrain do
  for i:=y1 to y2 do for k:=x1 to x2 do
    if RENDER_3D then begin
      glTexCoord1f(max(0,Land[i  ,k  ].Light)); glVertex3f(k-1,i-1,-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k  ].Light)); glVertex3f(k-1,i  ,-Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k+1].Light)); glVertex3f(k  ,i  ,-Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i  ,k+1].Light)); glVertex3f(k  ,i-1,-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end else begin
      glTexCoord1f(max(0,Land[i  ,k  ].Light)); glVertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k  ].Light)); glVertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i+1,k+1].Light)); glVertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(max(0,Land[i  ,k+1].Light)); glVertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;

  //Render shadows and FOW at once
  glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
  glBindTexture(GL_TEXTURE_2D, fResource.Tileset.TextG);
  glBegin(GL_QUADS);
    with fTerrain do
    for i:=y1 to y2 do for k:=x1 to x2 do
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
      glTexCoord1f(kromutils.max(0, -Land[i  ,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i,true)/255));
      glVertex2f(k-1,i-1-Land[i  ,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i+1,k  ].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k,i+1,true)/255));
      glVertex2f(k-1,i  -Land[i+1,k  ].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i+1,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i+1,true)/255));
      glVertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord1f(kromutils.max(0, -Land[i  ,k+1].Light, 1-MyPlayer.FogOfWar.CheckVerticeRevelation(k+1,i,true)/255));
      glVertex2f(k  ,i-1-Land[i  ,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);

  if SHOW_WALK_CONNECT then
  for i:=y1 to y2 do for k:=x1 to x2 do
  with fTerrain.Land[i,k] do
    fRenderAux.Quad(k, i, WalkConnect[wcWalk] * 32 + (WalkConnect[wcRoad] * 32) shl 8 or $80000000);
end;


procedure TRender.RenderTerrainFieldBorders(x1,x2,y1,y2:integer);
var
  i,k:integer;
  F: TFieldType;
  BordersList: TKMPointDirList;
  TabletsList: TKMPointTagList;
begin
  for i:=y1 to y2 do
  for k:=x1 to x2 do
  with fTerrain do
  begin
    if Land[i,k].BorderTop then RenderTerrainBorder(Land[i,k].Border,dir_N,k,i);
    if Land[i,k].BorderLeft then RenderTerrainBorder(Land[i,k].Border,dir_E,k,i);
    if Land[i,k].BorderRight then RenderTerrainBorder(Land[i,k].Border,dir_W,k,i);
    if Land[i,k].BorderBottom then RenderTerrainBorder(Land[i,k].Border,dir_S,k,i);

    F := MyPlayer.BuildList.FieldworksList.HasField(KMPoint(k,i));
    if F <> ft_None then
      RenderTerrainMarkup(k, i, F);
  end;

  //@Lewin: Since plans are per-player now, what do we do about allies that:
  // - have partially overlapping plans
  // - have plans/tablets on exact same spot

  BordersList := TKMPointDirList.Create;
  MyPlayer.BuildList.HousePlanList.GetBorders(BordersList, x1,x2,y1,y2);
  for i := 0 to BordersList.Count - 1 do
    RenderTerrainBorder(bt_HousePlan, BordersList[i].Dir, BordersList[i].Loc.X, BordersList[i].Loc.Y);
  BordersList.Free;

  TabletsList := TKMPointTagList.Create;
  MyPlayer.BuildList.HousePlanList.GetTablets(TabletsList, x1,x2,y1,y2);
  for i := 1 to TabletsList.Count do
    fRender.AddHouseTablet(THouseType(TabletsList.Tag[i]), TabletsList.List[i]);
  TabletsList.Free;
end;


procedure TRender.RenderTerrainObjects(x1,x2,y1,y2:Integer; AnimStep: Cardinal);
var I,K: Integer;
begin
  for i := y1 to y2 do
    for k := x1 to x2 do
      if fTerrain.Land[I,K].Obj <> 255 then
        RenderObjectOrQuad(fTerrain.Land[i, k].Obj + 1, AnimStep, k, i);

  with fTerrain.FallingTrees do
    for i := 1 to Count do
    begin
      RenderObject(Tag[i] + 1, AnimStep - Tag2[i], List[i].X, List[i].Y);
      Assert(AnimStep - Tag2[i] <= 100, 'Falling tree overrun?');
    end;
end;


procedure TRender.AddProjectile(aProj: TProjectileType; pX,pY: Single; Flight: Single; Dir: TKMDirection);
var
  FOW: Byte;
  ID: Integer;
  ShiftX,ShiftY: Single;
  Ground: Single;
begin
  if not fTerrain.TileInMapCoords(Round(pX), Round(pY)) then Exit; //Arrows may fly off map

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(Round(pX), Round(pY), true);
  if FOW <= 128 then exit; //Don't render objects which are behind FOW

  case aProj of
    pt_Arrow:     with fResource.UnitDat[ut_Bowman].UnitAnim[ua_Spec, Dir] do
                    ID := Step[round(Flight*Count)+1]+1;
    pt_Bolt:      with fResource.UnitDat[ut_Arbaletman].UnitAnim[ua_Spec, Dir] do
                    ID := Step[round(Flight*Count)+1]+1;
    pt_SlingRock: with fResource.UnitDat[ut_Slingshot].UnitAnim[ua_Spec, Dir] do
                    ID := Step[round(Flight*Count)+1]+1;
    pt_TowerRock: ID := ProjectileBounds[aProj,1]+1;
    else          ID := 1; //Nothing?
  end;

  ShiftX := RXData[rxUnits].Pivot[ID].x / CELL_SIZE_PX;
  ShiftY := (RXData[rxUnits].Pivot[ID].y + RXData[rxUnits].Size[ID].Y) / CELL_SIZE_PX;

  case aProj of
    pt_Arrow, pt_Bolt, pt_SlingRock:  Ground := pY + ShiftY + (0.5 - Abs(Flight-0.5)) + 0.5;
    pt_TowerRock:                     Ground := pY + ShiftY + (1 - Flight) + 0.5;
    else                              Ground := pY + ShiftY; //Nothing?
  end;

  fRenderList.AddSprite(rxUnits, ID, pX + ShiftX, pY + ShiftY, Ground, True);
end;


procedure TRender.RenderObjectOrQuad(aIndex,AnimStep,pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
begin
  //Render either normal object or quad depending on what it is
  if MapElem[aIndex].WineOrCorn then
    RenderObjectQuad(aIndex,AnimStep,pX,pY,(aIndex-1 in [54..57]),DoImmediateRender,Deleting) //54..57 are grapes, all others are doubles
  else
    RenderObject(aIndex,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRender.RenderObject(aIndex:Integer; AnimStep:Cardinal; pX,pY:integer; DoImmediateRender:boolean=false; Deleting:boolean=false);
var ShiftX,ShiftY:single; ID:integer; FOW:byte;
begin
  if MapElem[aIndex].Count=0 then exit;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX,pY,true);
  if FOW = 0 then exit; //Don't render objects which are unexplored
  if FOW <=128 then AnimStep:=0; //Stop animation
  ID:=MapElem[aIndex].Step[AnimStep mod MapElem[aIndex].Count +1]+1;
  if ID<=0 then exit;

  if aIndex=61 then begin //Invisible wall
    ShiftX := 0; //Required if DoImmediateRender = true
    ShiftY := 0;
    fRenderAux.Quad(pX,pY,$800000FF);
    RenderCursorWireQuad(KMPoint(pX,pY),$FF0000FF);
  end else begin
    ShiftX:=RXData[rxTrees].Pivot[ID].x/CELL_SIZE_PX;
    ShiftY:=(RXData[rxTrees].Pivot[ID].y+RXData[rxTrees].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[pY,pX].Height/CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxTrees, ID, pX+ShiftX, pY+ShiftY, pY+ShiftY, True);
    {RenderDot(pX,pY);
    glRasterPos2f(pX-1+0.1,pY-1+0.1);
    glPrint(inttostr(aIndex)+':'+inttostr(ID));}
  end;

  if DoImmediateRender then RenderSprite(rxTrees,ID,pX+ShiftX,pY+ShiftY,$FFFFFFFF,255,Deleting);
end;


{ 4 objects packed on 1 tile for Corn and Grapes }
procedure TRender.RenderObjectQuad(aIndex:Integer; AnimStep:Cardinal; pX,pY:integer; IsDouble:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
var FOW:byte;
  procedure AddSpriteBy(aID:integer; aAnimStep:integer; pX,pY:integer; ShiftX,ShiftY:single);
  var ID:integer;
  begin
    ID := MapElem[aID].Step[aAnimStep mod MapElem[aID].Count +1 ] +1;
    ShiftY := ShiftY + (RXData[rxTrees].Size[ID].Y) / CELL_SIZE_PX;
    ShiftY := ShiftY - fTerrain.InterpolateLandHeight(pX+ShiftX, pY+ShiftY)/CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxTrees, ID, pX+ShiftX, pY+ShiftY, pY+ShiftY, True);
    if DoImmediateRender then RenderSprite(rxTrees,ID,pX+ShiftX,pY+ShiftY,$FFFFFFFF,255,Deleting);
  end;
begin
  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX,pY,true);
  if FOW <=128 then AnimStep := 0; //Stop animation

  AddSpriteBy(aIndex, AnimStep  , pX, pY, 0  , -0.4);
  AddSpriteBy(aIndex, AnimStep+1, pX, pY, 0.5, -0.4);
  if IsDouble then exit;
  AddSpriteBy(aIndex, AnimStep+1, pX, pY, 0  , 0.1);
  AddSpriteBy(aIndex, AnimStep  , pX, pY, 0.5, 0.1);
end;


{Render house WIP tablet}
procedure TRender.AddHouseTablet(Index:THouseType; Loc:TKMPoint);
var ShiftX,ShiftY: Single; ID: Integer;
begin
  ID := fResource.HouseDat[Index].TabletIcon;
  ShiftX := Loc.X +  RXData[rxGui].Pivot[ID].x/CELL_SIZE_PX + 0.6;
  ShiftY := Loc.Y + (RXData[rxGui].Pivot[ID].y + RXData[rxGui].Size[ID].Y)/CELL_SIZE_PX + 0.5 -
                    ((fTerrain.Land[Loc.Y+1, Loc.X].Height+fTerrain.Land[Loc.Y+1, Loc.X+1].Height) div 2)/CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxGui, ID, ShiftX, ShiftY, ShiftY, true);
end;


{Render house build supply}
procedure TRender.AddHouseBuildSupply(Index:THouseType; Wood,Stone:byte; Loc:TKMPoint);
var ShiftX,ShiftY:single; ID:integer;
begin
  if Wood<>0 then begin
    ID := 260+Wood-1;
    ShiftX := Loc.X + fResource.HouseDat[Index].BuildSupply[1, Wood].MoveX/CELL_SIZE_PX;
    ShiftY := Loc.Y + (fResource.HouseDat[Index].BuildSupply[1, Wood].MoveY+RXData[rxHouses].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses,ID,ShiftX,ShiftY,ShiftY,false);
  end;
  if Stone<>0 then begin
    ID := 267+Stone-1;
    ShiftX := Loc.X + fResource.HouseDat[Index].BuildSupply[2, Stone].MoveX/CELL_SIZE_PX;
    ShiftY := Loc.Y + (fResource.HouseDat[Index].BuildSupply[2, Stone].MoveY+RXData[rxHouses].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses,ID,ShiftX,ShiftY,ShiftY,false);
  end;
end;


{Render house in wood}
procedure TRender.AddHouseWood(Index: THouseType; Step: Single; Loc: TKMPoint);
var ShiftX,ShiftY: Single; ID: Integer;
begin
  ID := fResource.HouseDat[Index].WoodPic + 1;
  ShiftX := Loc.X + RXData[rxHouses].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := Loc.Y + (RXData[rxHouses].Pivot[ID].y+RXData[rxHouses].Size[ID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxHouses,ID,ShiftX,ShiftY,ShiftY,true,0,Step);
end;


{Render house in stone}
procedure TRender.AddHouseStone(Index: THouseType; Step: Single; Loc: TKMPoint);
var ShiftX,ShiftY: Single; ID: Integer;
begin
  AddHouseWood(Index, 1, Loc); //Render Wood part of it, opaque
  ID := fResource.HouseDat[Index].StonePic + 1;
  ShiftX := Loc.X + RXData[rxHouses].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := Loc.Y + (RXData[rxHouses].Pivot[ID].y+RXData[rxHouses].Size[ID].Y)/CELL_SIZE_PX - fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
  fRenderList.AddSprite(rxHouses,ID,ShiftX,ShiftY,ShiftY,false,0,Step);
end;


procedure TRender.AddHouseWork(aHouse:THouseType; aActSet:THouseActionSet; AnimStep:cardinal; Loc:TKMPoint; FlagColor:TColor4);
var AnimCount,ID:cardinal; AT:THouseActionType; ShiftX,ShiftY:single;
begin
  if aActSet = [] then Exit;

  //See if action is in set and render it
  for AT:=Low(THouseActionType) to High(THouseActionType) do
  if AT in aActSet then
  begin
    AnimCount := fResource.HouseDat[aHouse].Anim[AT].Count;
    if AnimCount<>0 then
    begin
      ID := fResource.HouseDat[aHouse].Anim[AT].Step[AnimStep mod AnimCount + 1]+1;
      ShiftX := RXData[rxHouses].Pivot[ID].x/CELL_SIZE_PX;
      ShiftY := (RXData[rxHouses].Pivot[ID].y+RXData[rxHouses].Size[ID].Y)/CELL_SIZE_PX - fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
      ShiftX := ShiftX+fResource.HouseDat[aHouse].Anim[AT].MoveX/CELL_SIZE_PX;
      ShiftY := ShiftY+fResource.HouseDat[aHouse].Anim[AT].MoveY/CELL_SIZE_PX;
      fRenderList.AddSprite(rxHouses,ID,Loc.X+ShiftX,Loc.Y+ShiftY,Loc.Y+ShiftY,false,FlagColor);
    end;
  end;
end;


procedure TRender.AddHouseSupply(Index:THouseType; const R1,R2:array of byte; Loc:TKMPoint);
var ID,i,k:integer;

  procedure AddHouseSupplySprite(aID:integer);
  var ShiftX,ShiftY:single;
  begin
    if aID>0 then
    begin
      ShiftX := Loc.X + RXData[rxHouses].Pivot[aID].x/CELL_SIZE_PX;
      ShiftY := Loc.Y + (RXData[rxHouses].Pivot[aID].y+RXData[rxHouses].Size[aID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
      fRenderList.AddSprite(rxHouses,aID,ShiftX,ShiftY,ShiftY,false);
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
    if Index in [ht_WeaponSmithy, ht_ArmorSmithy, ht_WeaponWorkshop, ht_ArmorWorkshop] then
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


procedure TRender.AddMarketSupply(ResType:TResourceType; ResCount:word; Loc:TKMPoint; AnimStep:integer);
var i,ID: Integer;

  procedure AddHouseSupplySprite(aID:integer);
  var ShiftX,ShiftY:single;
  begin
    if aID <> 0 then
    begin
      ShiftX := Loc.X + (RXData[rxGame].Pivot[aID].x + MarketWaresOffsetX)/CELL_SIZE_PX;
      ShiftY := Loc.Y + (RXData[rxGame].Pivot[aID].y + MarketWaresOffsetY+RXData[rxGame].Size[aID].Y)/CELL_SIZE_PX-fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;
      fRenderList.AddSprite(rxGame,aID,ShiftX,ShiftY,ShiftY,false);
    end;
  end;

begin
  if ResType = rt_Horse then //Horses are a beast, BeastID is the count, age is 1
    for i:=1 to Min(ResCount, MarketWares[ResType].Count) do //Render each beast
      AddHouseStableBeasts(ht_Marketplace, i, 1, AnimStep, Loc, rxGame) //Use RXGame
  else
  begin
    if MarketWares[ResType].Count = 0 then exit;
    ID := (MarketWares[ResType].TexStart-1) + Min(ResCount, MarketWares[ResType].Count);
    AddHouseSupplySprite(ID);
  end;
end;


procedure TRender.AddHouseStableBeasts(Index:THouseType; BeastID,BeastAge,AnimStep:integer; Loc:TKMPoint; aRX: TRXType = rxHouses);
var ShiftX,ShiftY:single; ID:integer;
begin
  with fResource.HouseDat.BeastAnim[Index,BeastID,BeastAge] do begin
    ID := Step[AnimStep mod Count + 1]+1;
    ShiftX := MoveX/CELL_SIZE_PX;
    ShiftY := MoveY/CELL_SIZE_PX;
  end;

  ShiftX := ShiftX + RXData[aRX].Pivot[ID].X / CELL_SIZE_PX;
  ShiftY := ShiftY + (RXData[aRX].Pivot[ID].Y + RXData[aRX].Size[ID].Y) / CELL_SIZE_PX - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(aRX, ID, Loc.X + ShiftX, Loc.Y + ShiftY, Loc.Y + ShiftY, False);
end;


procedure TRender.AddUnit(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; NewInst:boolean; DoImmediateRender:boolean=false; Deleting:boolean=false);
var ShiftX,ShiftY:single; ID:integer; A:TKMUnitsAnim;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then exit;

  ShiftX := RXData[rxUnits].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := (RXData[rxUnits].Pivot[ID].y+RXData[rxUnits].Size[ID].Y)/CELL_SIZE_PX;

  ShiftY := ShiftY - fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV - 0.4;
  fRenderList.AddSprite(rxUnits,ID,pX+ShiftX,pY+ShiftY,pY+ShiftY,NewInst,FlagColor);
  if DoImmediateRender then RenderSprite(rxUnits,ID,pX+ShiftX,pY+ShiftY,FlagColor,255,Deleting);

  if SHOW_UNIT_MOVEMENT and fGame.AllowDebugRendering then
    fRenderAux.Dot(pX-0.5,pY-1-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV, FlagColor);
end;


procedure TRender.AddEater(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; Loc:TKMPoint; OffX,OffY:single; FlagColor:TColor4);
var ShiftX,ShiftY:single; ID:integer; A:TKMUnitsAnim;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then exit;

  //Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  ShiftX:=OffX+RXData[rxUnits].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=OffY+(RXData[rxUnits].Pivot[ID].y+RXData[rxUnits].Size[ID].Y)/CELL_SIZE_PX - fTerrain.Land[Loc.Y+1,Loc.X].Height/CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits,ID,Loc.X+ShiftX,Loc.Y+ShiftY,Loc.Y+ShiftY,False,FlagColor);
end;


procedure TRender.AddUnitCarry(aCarry:TResourceType; aDir:TKMDirection; StepID:integer; pX,pY:single);
var ShiftX,ShiftY:single; ID:integer; A:TKMUnitsAnim;
begin
  A := fResource.UnitDat.SerfCarry[aCarry, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then Exit;

  ShiftX := RXData[rxUnits].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY := (RXData[rxUnits].Pivot[ID].y + RXData[rxUnits].Size[ID].Y)/CELL_SIZE_PX;
  ShiftY := ShiftY - fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4;
  ShiftX := ShiftX + A.MoveX/CELL_SIZE_PX;
  ShiftY := ShiftY + A.MoveY/CELL_SIZE_PX;
  fRenderList.AddSprite(rxUnits, ID, pX + ShiftX, pY + ShiftY, pY + ShiftY, false);
end;


procedure TRender.AddUnitThought(Thought: TUnitThought; pX,pY: Single);
var
  ID: Integer;
  ShiftX, ShiftY: Single;
begin
  if Thought = th_None then Exit;

  //Thought bubbles are animated in reverse
  ID := ThoughtBounds[Thought, 2] + 1 -
       (fGame.GameTickCount mod word(ThoughtBounds[Thought, 2] - ThoughtBounds[Thought, 1]));

  ShiftX:=RXData[rxUnits].Pivot[ID].x/CELL_SIZE_PX;
  ShiftY:=(RXData[rxUnits].Pivot[ID].y+RXData[rxUnits].Size[ID].Y)/CELL_SIZE_PX;
  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV-0.4 - 1.5;
  fRenderList.AddSprite(rxUnits,ID,pX+ShiftX,pY+ShiftY,pY+ShiftY,false);
end;


procedure TRender.AddUnitFlag(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; UnitX,UnitY:single; NewInst:boolean);
var ShiftX,ShiftY:single; ID:integer; A:TKMUnitsAnim;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod A.Count + 1] + 1;
  if ID <= 0 then exit;

  ShiftX:=RXData[rxUnits].Pivot[ID].x/CELL_SIZE_PX -0.5;
  ShiftY:=(RXData[rxUnits].Pivot[ID].y+RXData[rxUnits].Size[ID].Y)/CELL_SIZE_PX;

  ShiftY:=ShiftY-fTerrain.InterpolateLandHeight(UnitX,UnitY)/CELL_HEIGHT_DIV-0.4 -2.25;
  fRenderList.AddSprite(rxUnits,ID,pX+ShiftX,pY+ShiftY,pY+ShiftY,NewInst,FlagColor);

  if SHOW_UNIT_MOVEMENT and fGame.AllowDebugRendering then
    fRenderAux.Dot(pX,pY-fTerrain.InterpolateLandHeight(pX,pY)/CELL_HEIGHT_DIV, FlagColor); //Render dot where unit is
end;


procedure TRender.AddUnitWithDefaultArm(aUnit:TUnitType; aAct:TUnitActionType; aDir:TKMDirection; StepID:integer; pX,pY:single; FlagColor:TColor4; DoImmediateRender:boolean=false; Deleting:boolean=false);
begin
  if aUnit = ut_Fish then aAct := FishCountAct[5]; //In map editor always render 5 fish
  AddUnit(aUnit,aAct,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
  if fResource.UnitDat[aUnit].SupportsAction(ua_WalkArm) then
    AddUnit(aUnit,ua_WalkArm,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
end;


{Render one terrian cell}
procedure TRender.RenderTile(Index:byte; pX,pY,Rot:integer);
var k,i,a:integer;
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
      glTexCoord2fv(@TexC[TexO[1]]); glVertex2f(k-1,i-1-Land[i,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[2]]); glVertex2f(k-1,i  -Land[i+1,k].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[3]]); glVertex2f(k  ,i  -Land[i+1,k+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[TexO[4]]); glVertex2f(k  ,i-1-Land[i,k+1].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRender.RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
var
  Lay, TopLay: Byte;
begin
  if (GFXData[aRX,aID].AltID <> 0) then
    TopLay := 2
  else
    TopLay := 1;

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
      glColor4ubv(@Col);
      glBindTexture(GL_TEXTURE_2D, AltID);
    end;

    if HighlightRed then glColor4f(1,0,0,1);

    glBegin(GL_QUADS);
      glTexCoord2f(u1, v2); glVertex2f(pX-1                     , pY-1                      );
      glTexCoord2f(u2, v2); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX, pY-1                      );
      glTexCoord2f(u2, v1); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX, pY-1-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(u1, v1); glVertex2f(pX-1                     , pY-1-pxHeight/CELL_SIZE_PX);
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
          glkRect(pX-1, pY-1, pX-1+pxWidth/CELL_SIZE_PX, pY-1-pxHeight/CELL_SIZE_PX);
      glEnd;
    glPopAttrib;
  end;
end;


procedure TRender.RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param:single; pX,pY:single; aFOW:byte);
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


procedure TRender.RenderTerrain;
var
  x1, x2, y1, y2: integer;
  Passability: integer;
begin
  x1 := fGame.Viewport.GetClip.Left;
  x2 := fGame.Viewport.GetClip.Right;
  y1 := fGame.Viewport.GetClip.Top;
  y2 := fGame.Viewport.GetClip.Bottom;

  fRender.RenderTerrainTiles(x1, x2, y1, y2, fTerrain.AnimStep);
  fRender.RenderTerrainFieldBorders(x1, x2, y1, y2);
  fRender.RenderTerrainObjects(x1, x2, y1, y2, fTerrain.AnimStep);

  if not fGame.AllowDebugRendering then
    exit;

  if SHOW_TERRAIN_WIRES then
    fRenderAux.Wires(x1, x2, y1, y2);

  if SHOW_TERRAIN_WIRES then
  begin
    Passability := fGame.FormPassability;
    if fGame.fMapEditorInterface <> nil then
      Passability := max(Passability, fGame.fMapEditorInterface.ShowPassability);
    fRenderAux.Passability(x1, x2, y1, y2, Passability);
  end;

  if SHOW_UNIT_MOVEMENT then
    fRenderAux.UnitMoves(x1, x2, y1, y2);
end;


procedure TRender.RenderTerrainMarkup(aLocX, aLocY: Word; aFieldType: TFieldType);
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


procedure TRender.RenderCursorWireQuad(P:TKMPoint; Col:TColor4);
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


procedure TRender.RenderCursorBuildIcon(P:TKMPoint; id:integer=479);
begin
  if fTerrain.TileInMapCoords(P.X,P.Y) then
    RenderSprite(rxGui,id,P.X+0.2,P.Y+1-0.2-fTerrain.InterpolateLandHeight(P.X+0.5,P.Y+0.5)/CELL_HEIGHT_DIV,$FFFFFFFF,255);
end;


procedure TRender.RenderCursorWireHousePlan(P:TKMPoint; aHouseType:THouseType);
var i:integer; MarksList: TKMPointTagList;
begin
  MarksList := TKMPointTagList.Create;
  MyPlayer.GetHouseMarks(P, aHouseType, MarksList);

  for i:=1 to MarksList.Count do
  if MarksList.Tag[i] = 0 then
    RenderCursorWireQuad(MarksList.List[i], $FFFFFF00) //Cyan rect
  else
    RenderCursorBuildIcon(MarksList.List[i], MarksList.Tag[i]); //icon

  MarksList.Free;
end;


procedure TRender.RenderCursorHighlights;
  //Shortcut function
  function TileVisible: Boolean;
  begin
    Result := MyPlayer.FogOfWar.CheckTileRevelation(GameCursor.Cell.X, GameCursor.Cell.Y, True) > 0;
  end;
var
  U: TKMUnit;
begin
  if GameCursor.Cell.Y*GameCursor.Cell.X = 0 then exit; //Caused a rare crash

  with fTerrain do
  case GameCursor.Mode of
    cm_None:   ;
    cm_Erase:   case fGame.GameState of
                  gsEditor:
                    begin
                      //With Units tab see if there's a unit below cursor
                      if (fGame.fMapEditorInterface.GetShownPage = esp_Units) then
                      begin
                        U := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                        if U <> nil then
                          AddUnitWithDefaultArm(U.UnitType,ua_Walk,U.Direction,U.AnimStep,GameCursor.Cell.X+0.5,GameCursor.Cell.Y+1,MyPlayer.FlagColor,true,true);
                      end
                      else
                      if (
                            //With Buildings tab see if we can remove Fields or Houses
                             (fGame.fMapEditorInterface.GetShownPage = esp_Buildings)
                             and (    TileIsCornField(GameCursor.Cell)
                                   or TileIsWineField(GameCursor.Cell)
                                   or (Land[GameCursor.Cell.Y,GameCursor.Cell.X].TileOverlay=to_Road)
                                   or (fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y) <> nil))
                         )
                      //And of course it is visible
                      and TileVisible then
                        RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                      else
                        RenderCursorBuildIcon(GameCursor.Cell);       //Red X
                    end;

                  gsPaused, gsOnHold, gsRunning:
                    begin
                      if ((MyPlayer.BuildList.FieldworksList.HasField(GameCursor.Cell) <> ft_None)
                          or MyPlayer.BuildList.HousePlanList.HasPlan(GameCursor.Cell)
                          or (MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y) <> nil))
                      and TileVisible then
                        RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                      else
                        RenderCursorBuildIcon(GameCursor.Cell);       //Red X
                    end;
                end;
    cm_Road:    if MyPlayer.CanAddFieldPlan(GameCursor.Cell, ft_Road) then
                  RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Field:   if MyPlayer.CanAddFieldPlan(GameCursor.Cell, ft_Corn) then
                  RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Wine:    if MyPlayer.CanAddFieldPlan(GameCursor.Cell, ft_Wine) then
                  RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Wall:    if MyPlayer.CanAddFieldPlan(GameCursor.Cell, ft_Wall) then
                  RenderCursorWireQuad(GameCursor.Cell, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(GameCursor.Cell);       //Red X
    cm_Houses:  RenderCursorWireHousePlan(GameCursor.Cell, THouseType(GameCursor.Tag1)); //Cyan quad
    cm_Tiles:   if GameCursor.Tag2 in [0..3] then
                  RenderTile(GameCursor.Tag1, GameCursor.Cell.X, GameCursor.Cell.Y, GameCursor.Tag2)
                else
                  RenderTile(GameCursor.Tag1, GameCursor.Cell.X, GameCursor.Cell.Y, (fTerrain.AnimStep div 5) mod 4); //Spin it slowly so player remembers it is on randomized
    cm_Objects: begin
                  RenderObjectOrQuad(fTerrain.Land[GameCursor.Cell.Y,GameCursor.Cell.X].Obj+1, fTerrain.AnimStep, GameCursor.Cell.X, GameCursor.Cell.Y, true, true); //Make entire object red
                  RenderObjectOrQuad(GameCursor.Tag1+1, fTerrain.AnimStep, GameCursor.Cell.X, GameCursor.Cell.Y, true);
                end;
    cm_Height:  fRenderAux.Circle(GameCursor.Float.X,GameCursor.Float.Y - fTerrain.InterpolateLandHeight(GameCursor.Float)/CELL_HEIGHT_DIV, (GameCursor.Tag1 and $F) div 2, $00000000,  $FFFFFFFF);
    cm_Units:   if CanPlaceUnit(GameCursor.Cell, TUnitType(GameCursor.Tag1)) then
                  AddUnitWithDefaultArm(TUnitType(GameCursor.Tag1),ua_Walk,dir_S,UnitStillFrames[dir_S],GameCursor.Cell.X+0.5,GameCursor.Cell.Y+1,MyPlayer.FlagColor,true)
                else
                  RenderCursorBuildIcon(GameCursor.Cell);       //Red X
  end;
end;


//Render highlight overlay to make whole picture look brighter (more saturated)
procedure TRender.RenderBrightness(Value: Byte);
begin
  //There will be no change to image anyway
  if Value = 0 then Exit;

  glBlendFunc(GL_DST_COLOR, GL_ONE);
  glColor4f(Value/20, Value/20, Value/20, Value/20);
  glBegin(GL_QUADS);
    glkRect(0, 0, fSetup.ScreenX, fSetup.ScreenY);
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
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
var I: Integer; PX,PY: Word;
begin
  SetLength(RenderOrder, fCount);

  for I := 0 to fCount - 1 do
  if RenderList[I].NewInst then
  begin
    RenderOrder[I] := I;
    PX := Round(RenderList[I].Loc.X);
    PY := Round(RenderList[I].Loc.Y);
    //RenderQuad(P.X,P.Y);
    RenderList[I].FOWvalue := MyPlayer.FogOfWar.CheckTileRevelation(PX, PY, True);

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
          if (RenderList[RenderOrder[K]].Ground < RenderList[RenderOrder[I]].Ground)
          or((RenderList[RenderOrder[K]].Ground = RenderList[RenderOrder[I]].Ground)
          and(RenderList[RenderOrder[K]].Loc.X > RenderList[RenderOrder[I]].Loc.X))
          then //TopMost Rightmost
            SwapInt(RenderOrder[K], RenderOrder[I])
end;


procedure TRenderList.AddSprite(aRX: TRXType; aID: Word; pX,pY,aGround: Single; aNew: Boolean; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then SetLength(RenderList, fCount + 256); //Book some space

  RenderList[fCount].Loc        := KMPointF(pX,pY); //Position of sprite, floating-point
  RenderList[fCount].Ground     := aGround;         //Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             //RX library
  RenderList[fCount].ID         := aID;             //Texture ID
  RenderList[fCount].NewInst    := aNew;            //Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           //Team ID (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      //Alpha step for wip buildings
  RenderList[fCount].FOWvalue   := 255;             //Visibility recomputed in ClipRender anyway

  inc(fCount); //New item added
end;


{Now render all these items from list}
procedure TRenderList.Render;
var i,h:integer;
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
        glRotatef(fRender.rHeading, -1, 0, 0);
        glTranslatef(-RenderList[h].Loc.X, -RenderList[h].Loc.Y, 0);
      end;

      repeat //Render child sprites only after their parent
        with RenderList[h] do
        begin
          if AlphaStep = -1 then
            fRender.RenderSprite(RX, ID, Loc.X, Loc.Y, TeamColor,FOWvalue)
          else
            fRender.RenderSpriteAlphaTest(RX, ID, AlphaStep, Loc.X, Loc.Y, FOWvalue);

          if SHOW_GROUND_LINES then
          begin
            glBegin(GL_LINES);
              glColor3f(1,1,0.5);
              glVertex2f(Loc.X - 1, Ground - 1);
              glVertex2f(Loc.X - 1 + GFXData[RX, ID].PxWidth/CELL_SIZE_PX, Ground - 1);
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
