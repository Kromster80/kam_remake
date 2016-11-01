unit KM_RenderPool;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_Pics, KM_Points, KM_Render, KM_Viewport,
  KM_RenderTerrain, KM_ResHouses, KM_ResSprites, KM_ResWares,
  KM_Houses, KM_Terrain, KM_Projectiles, OBJLoader;

type
  TKMPaintLayer = (plTerrain, plObjects, plCursors);

  TKMRenderSprite = record
    Loc: TKMPointF; // Where sprite lower-left corner is located
    Feet: TKMPointF; // Feet of the sprite for FOW calculation (X;Y) and Z ordering (Y only)
    RX: TRXType;
    ID: Word;
    UID: Integer;
    NewInst: Boolean;
    TeamColor: Cardinal;
    AlphaStep: Single; // Only apply-able to HouseBuild
    SelectionRect: TKMRectF; // Used for selecting units by sprite
  end;

  TSmallIntArray = array of smallint;

  // List of sprites prepared to be rendered
  TRenderList = class
  private
    fCount: Word;
    RenderOrder: TSmallIntArray; // Order in which sprites will be drawn ()
    RenderList: array of TKMRenderSprite;

    fStat_Sprites: Integer; // Total sprites in queue
    fStat_Sprites2: Integer;// Rendered sprites
    procedure ClipRenderList;
    procedure SendToRender(aId: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteG(aRX: TRXType; aID: Word; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);

    property Stat_Sprites: Integer read fStat_Sprites;
    property Stat_Sprites2: Integer read fStat_Sprites2;
    function GetSelectionUID(CurPos: TKMPointF): Integer;
    procedure Clear;
    procedure SortRenderList;
    procedure Render;
  end;

  // Collect everything that need to be rendered and put it in a list
  TRenderPool = class
  private
    fRXData: array [TRXType] of TRXData; // Shortcuts
    fViewport: TKMViewport;
    fRender: TRender;
    // fSampleHouse: TOBJModel;
    rPitch,rHeading,rBank: Integer;
    fRenderList: TRenderList;
    fRenderTerrain: TRenderTerrain;

    fFieldsList: TKMPointTagList;
    fHousePlansList: TKMPointDirList;
    fTabletsList: TKMPointTagList;
    fMarksList: TKMPointTagList;
    fHouseOutline: TKMPointList;

    procedure ApplyTransform;
    procedure RenderBackgroundUI(aRect: TKMRect);
    // Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderForegroundUI;

    procedure RenderSprite(aRX: TRXType; aId: Word; pX, pY: Single; Col: TColor4; HighlightRed: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aId: Word; aWoodProgress: Single; pX, pY: Single; aId2: Word = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
    procedure RenderMapElement1(aIndex: Byte; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderMapElement4(aIndex: Byte; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderHouseOutline(aHouse: TKMHouse);

    // Terrain rendering sub-class
    procedure CollectPlans(aRect: TKMRect);
    procedure CollectTerrainObjects(aRect: TKMRect; aAnimStep: Cardinal);
    procedure PaintRallyPoints(aPass: Byte);

    procedure RenderWireHousePlan(P: TKMPoint; aHouseType: THouseType);
  public
    constructor Create(aViewport: TKMViewport; aRender: TRender);
    destructor Destroy; override;

    procedure AddAlert(aLoc: TKMPointF; aId: Word; aFlagColor: TColor4);
    procedure AddProjectile(aProj: TProjectileType; aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
    procedure AddHouse(aHouse: THouseType; aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single);

    procedure AddHouseTablet(aHouse: THouseType; Loc: TKMPoint);
    procedure AddHouseBuildSupply(aHouse: THouseType; Loc: TKMPoint; Wood,Stone: Byte);
    procedure AddHouseWork(aHouse: THouseType; Loc: TKMPoint; aActSet: THouseActionSet; AnimStep: Cardinal; FlagColor: TColor4);
    procedure AddHouseSupply(aHouse: THouseType; Loc: TKMPoint; const R1,R2:array of byte);
    procedure AddHouseMarketSupply(Loc: TKMPoint; ResType: TWareType; ResCount:word; AnimStep: Integer);
    procedure AddHouseStableBeasts(aHouse: THouseType; Loc: TKMPoint; BeastId,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
    procedure AddHouseEater(Loc: TKMPoint; aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
    procedure AddUnit(aUnit: TUnitType; aUID: Integer; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure AddUnitCarry(aCarry: TWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; pX,pY: Single);
    procedure AddUnitThought(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; Thought: TKMUnitThought; pX,pY: Single);
    procedure AddUnitFlag(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; FlagAnim: Integer; pX,pY: Single; FlagColor: TColor4);
    procedure AddUnitWithDefaultArm(aUnit: TUnitType; aUID: Integer; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);

    procedure RenderMapElement(aIndex: Byte; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderSpriteOnTile(aLoc: TKMPoint; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
    procedure RenderSpriteOnTerrain(aLoc: TKMPointF; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
    procedure RenderTile(Index: Byte; pX,pY,Rot: Integer);
    procedure RenderWireTile(P: TKMPoint; Col: TColor4);

    property RenderList: TRenderList read fRenderList;
    property RenderTerrain: TRenderTerrain read fRenderTerrain;
    procedure SetRotation(aH,aP,aB: Integer);

    procedure Render;
  end;


var
  fRenderPool: TRenderPool;


implementation
uses
  KM_CommonTypes, KM_RenderAux, KM_HandsCollection, KM_Game, KM_Sound, KM_Resource, KM_ResUnits,
  KM_ResMapElements, KM_Units, KM_AIFields, KM_TerrainPainter, KM_GameCursor, KM_HouseBarracks,
  KM_FogOfWar, KM_Hand;


constructor TRenderPool.Create(aViewport: TKMViewport; aRender: TRender);
var
  RT: TRXType;
begin
  inherited Create;

  for RT := Low(TRXType) to High(TRXType) do
    fRXData[RT] := gRes.Sprites[RT].RXData;

  fRender := aRender;
  fViewport := aViewport;

  fRenderList     := TRenderList.Create;
  fRenderTerrain  := TRenderTerrain.Create;
  gRenderAux      := TRenderAux.Create;

  fFieldsList     := TKMPointTagList.Create;
  fHousePlansList := TKMPointDirList.Create;
  fTabletsList    := TKMPointTagList.Create;
  fMarksList      := TKMPointTagList.Create;
  fHouseOutline   := TKMPointList.Create;
  // fSampleHouse := TOBJModel.Create;
  // fSampleHouse.LoadFromFile(ExeDir + 'Store.obj');
end;


destructor TRenderPool.Destroy;
begin
  fFieldsList.Free;
  fHousePlansList.Free;
  fTabletsList.Free;
  fMarksList.Free;
  fHouseOutline.Free;
  // fSampleHouse.Free;
  fRenderList.Free;
  fRenderTerrain.Free;
  gRenderAux.Free;

  inherited;
end;


procedure TRenderPool.SetRotation(aH,aP,aB: Integer);
begin
  rHeading := aH;
  rPitch   := aP;
  rBank    := aB;
end;


procedure TRenderPool.ApplyTransform;
begin
  glLoadIdentity; // Reset The View
  glTranslatef(fViewport.ViewportClip.X/2, fViewport.ViewportClip.Y/2, 0);
  glScalef(fViewport.Zoom*CELL_SIZE_PX, fViewport.Zoom*CELL_SIZE_PX, 1 / 256);
  glTranslatef(-fViewport.Position.X+TOOLBAR_WIdTH/CELL_SIZE_PX/fViewport.Zoom, -fViewport.Position.Y, 0);
  if RENDER_3D then
  begin
    fRender.SetRenderMode(rm3D);

    glkScale(-CELL_SIZE_PX/14);
    glRotatef(rHeading,1,0,0);
    glRotatef(rPitch  ,0,1,0);
    glRotatef(rBank   ,0,0,1);
    glTranslatef(-fViewport.Position.X+TOOLBAR_WIdTH/CELL_SIZE_PX/fViewport.Zoom, -fViewport.Position.Y-8, 10);
    glScalef(fViewport.Zoom, fViewport.Zoom, 1);
  end;

  glRotatef(rHeading,1,0,0);
  glRotatef(rPitch  ,0,1,0);
  glRotatef(rBank   ,0,0,1);
  glTranslatef(0, 0, -fViewport.Position.Y);
end;


// Render:
// 1. Sets viewport
// 2. Renders terrain
// 3. Polls Game objects to add themselves to RenderList through Add** methods
// 4. Renders cursor highlights
procedure TRenderPool.Render;
var
  ClipRect: TKMRect;
begin
  if fRender.Blind then Exit;

  ApplyTransform;

  glPushAttrib(GL_LINE_BIT or GL_POINT_BIT);
    glLineWidth(fViewport.Zoom * 2);
    glPointSize(fViewport.Zoom * 5);

    // Render only within visible area
    ClipRect := fViewport.GetClip;

    // Collect players plans for terrain layer
    CollectPlans(ClipRect);

    // With depth test we can render all terrain tiles and then apply light/shadow without worrying about
    // foothills shadows going over mountain tops. Each tile strip is rendered an next Z plane.
    // Means that Z-test on gpu will take care of clipping the foothill shadows
    glEnable(GL_DEPTH_TEST);

    // Everything flat of terrain
    fRenderTerrain.ClipRect := ClipRect;
    fRenderTerrain.RenderBase(gTerrain.AnimStep, gMySpectator.FogOfWar);

    // Disable depth test //and write to depth buffer,
    // so that terrain shadows could be applied seamlessly ontop
    glDisable(GL_DEPTH_TEST);

    fRenderTerrain.RenderFences;

    fRenderTerrain.RenderPlayerPlans(fFieldsList, fHousePlansList);

    // House highlight, debug display
    RenderBackgroundUI(ClipRect);

    // Sprites are added by Terrain/Players/Projectiles, then sorted by position
    fRenderList.Clear;
    CollectTerrainObjects(ClipRect, gTerrain.AnimStep);
    PaintRallyPoints(0);

    gHands.Paint(ClipRect); // Units and houses
    gProjectiles.Paint;

    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(0);

    fRenderList.SortRenderList;
    fRenderList.Render;

    fRenderTerrain.RenderFOW(gMySpectator.FogOfWar, False);

    // Alerts/rally second pass is rendered after FOW
    PaintRallyPoints(1);
    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(1);

    // Cursor overlays (including blue-wire plans), go on top of everything
    RenderForegroundUI;

  glPopAttrib;
end;


procedure TRenderPool.RenderBackgroundUI(aRect: TKMRect);
var
  I, K: Integer;
begin
  if gMySpectator.Highlight is TKMHouse then
    RenderHouseOutline(TKMHouse(gMySpectator.Highlight));

  if gGame.IsMapEditor then
    gGame.MapEditor.Paint(plTerrain, aRect);

  if gAIFields <> nil then
    gAIFields.Paint(aRect);

  if SHOW_WALK_CONNECT then
  begin
    glPushAttrib(GL_DEPTH_BUFFER_BIT);
      glDisable(GL_DEPTH_TEST);

      for I := aRect.Top to aRect.Bottom do
      for K := aRect.Left to aRect.Right do
        gRenderAux.Text(K, I, IntToStr(gTerrain.Land[I,K].WalkConnect[wcWalk]), $FFFFFFFF);

    glPopAttrib;
  end;

  if SHOW_TERRAIN_WIRES then
    gRenderAux.Wires(aRect);

  if SHOW_TERRAIN_PASS <> 0 then
    gRenderAux.Passability(aRect, SHOW_TERRAIN_PASS);

  if SHOW_UNIT_MOVEMENT then
    gRenderAux.UnitMoves(aRect);
end;


procedure TRenderPool.CollectTerrainObjects(aRect: TKMRect; aAnimStep: Cardinal);
var
  I, K: Integer;
begin
  if gGame.IsMapEditor and not (mlObjects in gGame.MapEditor.VisibleLayers) then
    Exit;

  if gGame.IsMapEditor then
    gGame.MapEditor.Paint(plObjects, aRect);

  with gTerrain do
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
  begin
    if (Land[I, K].Obj <> 255)
    // In the map editor we shouldn't render terrain objects within the paste preview
    and (not gGame.IsMapEditor or not (mlSelection in gGame.MapEditor.VisibleLayers)
         or not gGame.MapEditor.Selection.TileWithinPastePreview(K, I)) then
      RenderMapElement(Land[I, K].Obj, AnimStep, K, I);

    // Fake wine objects for MapEd
    case Land[I,K].CornOrWine of
      1: ;
      2: RenderMapElement(54, AnimStep, K, I);
    end;
  end;

  // Falling trees are in a separate list
  with gTerrain do
  for I := 0 to FallingTrees.Count - 1 do
  begin
    RenderMapElement1(FallingTrees.Tag[I], aAnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y);
    Assert(AnimStep - FallingTrees.Tag2[I] <= 100, 'Falling tree overrun?');
  end;

  // Tablets on house plans, for self and allies
  fTabletsList.Clear;
  if gGame.GameMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti] then
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        gHands[I].GetPlansTablets(fTabletsList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetPlansTablets(fTabletsList, aRect)
  else
    gMySpectator.Hand.GetPlansTablets(fTabletsList, aRect);

  for I := 0 to fTabletsList.Count - 1 do
    AddHouseTablet(THouseType(fTabletsList.Tag[I]), fTabletsList[I]);
end;


procedure TRenderPool.PaintRallyPoints(aPass: Byte);
var
  B: TKMHouseBarracks;
  C: TKMHouseWoodcutters;
  P: TKMPointF;
begin
  if gGame.IsMapEditor then Exit; // Don't render rally point in map editor
  if not (gMySpectator.Selected is TKMHouseBarracks) and not (gMySpectator.Selected is TKMHouseWoodcutters) then
    Exit;

  if gMySpectator.Selected is TKMHouseBarracks then
  begin
    B := TKMHouseBarracks(gMySpectator.Selected);
    P := KMPointF(B.RallyPoint.X-0.5, B.RallyPoint.Y-0.5);
    if B.IsRallyPointSet then
    begin
      case aPass of
        0: begin
             AddAlert(P, 249, gHands[B.Owner].FlagColor);
             gRenderAux.LineOnTerrain(B.GetEntrance.X-0.5, B.GetEntrance.Y-0.5, P.X, P.Y, gHands[B.Owner].FlagColor, $F0F0, False);
           end;
        1: if gMySpectator.FogOfWar.CheckRevelation(P) < FOG_OF_WAR_MAX then
             fRenderPool.RenderSpriteOnTerrain(P, 249, gHands[B.Owner].FlagColor);
      end;
    end;
  end
  else
    if gMySpectator.Selected is TKMHouseWoodcutters then
    begin
      C := TKMHouseWoodcutters(gMySpectator.Selected);
      if C.IsCuttingPointSet then
      begin
        P := KMPointF(C.CuttingPoint.X - 0.5, C.CuttingPoint.Y - 0.5);
        case aPass of
          0: begin
               AddAlert(P, 660, gHands[C.Owner].FlagColor);
               gRenderAux.LineOnTerrain(C.GetEntrance.X - 0.5, C.GetEntrance.Y - 0.5, P.X, P.Y, gHands[C.Owner].FlagColor, $F0F0, False);
             end;
          1: if gMySpectator.FogOfWar.CheckRevelation(P) < FOG_OF_WAR_MAX then
             fRenderPool.RenderSpriteOnTerrain(P, 660, gHands[C.Owner].FlagColor);
        end;
      end;
    end;
end;


procedure TRenderPool.RenderTile(Index: Byte; pX, pY, Rot: Integer);
begin
  fRenderTerrain.RenderTile(Index, pX, pY, Rot);
end;


procedure TRenderPool.RenderMapElement(aIndex: Byte; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  // Render either normal object or quad depending on what it is
  if MapElem[aIndex].WineOrCorn then
    RenderMapElement4(aIndex,AnimStep,pX,pY,(aIndex in [54..57]),DoImmediateRender,Deleting) // 54..57 are grapes, all others are doubles
  else
    RenderMapElement1(aIndex,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderMapElement1(aIndex: Byte; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;
  pX, pY: Integer;
  CornerX, CornerY: Single;
  gX, gY: Single;
  Id, Id0: Integer;
  FOW: Byte;
  A: TKMAnimLoop;
begin
  if aIndex = 61 then
  begin
    // Invisible wall
    // Render as a red outline in map editor mode
    if gGame.IsMapEditor then
    begin
      gRenderAux.Quad(LocX, LocY, $600000FF);
      RenderWireTile(KMPoint(LocX, LocY), $800000FF);
    end;
  end
  else
  begin
    if MapElem[aIndex].Anim.Count = 0 then Exit;

    if DYNAMIC_FOG_OF_WAR then
    begin
      FOW := gMySpectator.FogOfWar.CheckTileRevelation(LocX,LocY);
      if FOW <= 128 then AnimStep := 0; // Stop animation
    end;
    A := MapElem[aIndex].Anim;
    Id := A.Step[AnimStep mod Byte(A.Count) +1]+1;
    Id0 := A.Step[1] + 1;
    if Id <= 0 then exit;

    R := fRXData[rxTrees];
    pX := LocX - 1;
    pY := LocY - 1;
    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    CornerY := pY - gTerrain.HeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;
    if not DoImmediateRender then
      fRenderList.AddSpriteG(rxTrees, Id, 0, CornerX, CornerY, gX, gY)
    else
      RenderSprite(rxTrees, Id, CornerX, CornerY, $FFFFFFFF, Deleting);
  end;
end;


// 4 objects packed on 1 tile for Corn and Grapes
procedure TRenderPool.RenderMapElement4(aIndex: Byte; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;

  procedure AddSpriteBy(aAnimStep: Integer; pX,pY: Single);
  var
    Id, Id0: Integer;
    CornerX, CornerY, gX, gY: Single;
    A: TKMAnimLoop;
  begin
    A := MapElem[aIndex].Anim;
    Id := A.Step[aAnimStep mod Byte(A.Count) + 1] + 1;
    Id0 := A.Step[1] + 1;

    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    CornerY := pY - gTerrain.HeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

    if not DoImmediateRender then
      fRenderList.AddSpriteG(rxTrees, Id, 0, CornerX, CornerY, gX, gY)
    else
      RenderSprite(rxTrees, Id, CornerX, CornerY, $FFFFFFFF, Deleting);
  end;

var
  FOW: Byte;
begin
  if DYNAMIC_FOG_OF_WAR then
  begin
    FOW := gMySpectator.FogOfWar.CheckTileRevelation(pX, pY);
    if FOW <= 128 then AnimStep := 0; // Stop animation
  end;

  R := fRXData[rxTrees];
  if IsDouble then
  begin
    AddSpriteBy(AnimStep  , pX - 0.75, pY - 0.6);
    AddSpriteBy(AnimStep+1, pX - 0.25, pY - 0.6);
  end
  else
  begin
    AddSpriteBy(AnimStep  , pX - 0.75, pY - 0.75);
    AddSpriteBy(AnimStep+1, pX - 0.25, pY - 0.75);
    AddSpriteBy(AnimStep+1, pX - 0.75, pY - 0.25);
    AddSpriteBy(AnimStep  , pX - 0.25, pY - 0.25);
  end;
end;


// Render alert
procedure TRenderPool.AddAlert(aLoc: TKMPointF; aId: Word; aFlagColor: TColor4);
var
  CornerX, CornerY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];

  CornerX := aLoc.X + R.Pivot[aId].X / CELL_SIZE_PX;
  CornerY := gTerrain.FlatToHeight(aLoc).Y + R.Pivot[aId].Y / CELL_SIZE_PX;

  fRenderList.AddSpriteG(rxGui, aId, 0, CornerX, CornerY, aLoc.X, aLoc.Y, aFlagColor);
end;


// Render house WIP tablet
procedure TRenderPool.AddHouseTablet(aHouse: THouseType; Loc: TKMPoint);
var
  Id: Integer;
  CornerX, CornerY, gX, gY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];
  Id := gRes.HouseDat[aHouse].TabletIcon;

  gX := Loc.X + (R.Pivot[Id].X + R.Size[Id].X / 2) / CELL_SIZE_PX - 0.5;
  gY := Loc.Y + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.45;
  CornerX := Loc.X + R.Pivot[Id].X / CELL_SIZE_PX - 0.25;
  CornerY := Loc.Y - gTerrain.HeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.55;
  fRenderList.AddSpriteG(rxGui, Id, 0, CornerX, CornerY, gX, gY);
end;


// Render house build supply
procedure TRenderPool.AddHouseBuildSupply(aHouse: THouseType; Loc: TKMPoint; Wood, Stone: Byte);
var
  rx: TRXData;
  id: Integer;
  supply: THouseBuildSupply;
  cornerX, cornerY: Single;
begin
  rx := fRXData[rxHouses];
  supply := gRes.HouseDat[aHouse].BuildSupply;

  if Wood <> 0 then
  begin
    id := 260 + Wood - 1;
    cornerX := Loc.X + supply[1, Wood].MoveX / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[1, Wood].MoveY + rx.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY);
  end;

  if Stone <> 0 then
  begin
    id := 267 + Stone - 1;
    cornerX := Loc.X + supply[2, Stone].MoveX / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[2, Stone].MoveY + rx.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY);
  end;
end;


// Render house in wood
procedure TRenderPool.AddHouse(aHouse: THouseType; aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single);
var
  R: TRXData;
  PicWood, PicStone, PicSnow: Integer;
  GroundWood, GroundStone, gX, gY: Single;

  function CornerX(aPic: Integer): Single;
  begin
    Result := aLoc.X + R.Pivot[aPic].X / CELL_SIZE_PX - 1;
  end;

  function CornerY(aPic: Integer): Single;
  begin
    Result := aLoc.Y + (R.Pivot[aPic].Y + R.Size[aPic].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land[aLoc.Y + 1, aLoc.X].Height / CELL_HEIGHT_DIV;
  end;

begin
  // We cannot skip when WoodStep = 0 because building supply is rendered as a child.
  // Instead RenderSpriteAlphaTest will skip rendering when WoodStep = 0

  R := fRXData[rxHouses];

  PicWood := gRes.HouseDat[aHouse].WoodPic + 1;
  PicStone := gRes.HouseDat[aHouse].StonePic + 1;
  PicSnow := gRes.HouseDat[aHouse].SnowPic + 1;

  GroundWood := R.Pivot[PicWood].Y + R.Size[PicWood].Y;
  GroundStone := R.Pivot[PicStone].Y + R.Size[PicStone].Y;

  gX := aLoc.X + (R.Pivot[PicWood].X + R.Size[PicWood].X / 2) / CELL_SIZE_PX - 1;
  gY := aLoc.Y + Max(GroundWood, GroundStone) / CELL_SIZE_PX - 1.5;

  // If it's fully built we can render without alpha
  if (aWoodStep = 1) and (aStoneStep = 1) then
  begin
    // Snow only happens on fully built houses
    if SNOW_HOUSES
    and (aSnowStep > 0)
    and (PicSnow <> 0) then
    begin
      // If snow is 100% we only need to render snow sprite
      if aSnowStep = 1 then
        fRenderList.AddSpriteG(rxHouses, PicSnow, 0, CornerX(PicSnow), CornerY(PicSnow), gX, gY, $0)
      else
      begin
        // Render stone with snow blended on top using AlphaTest
        //todo: Shadow shouldn't get rendered twice
        fRenderList.AddSpriteG(rxHouses, PicStone, 0, CornerX(PicStone), CornerY(PicStone), gX, gY, $0);
        fRenderList.AddSpriteG(rxHouses, PicSnow, 0, CornerX(PicSnow), CornerY(PicSnow), gX, gY, $0, aSnowStep);
      end;
    end
    else
      fRenderList.AddSpriteG(rxHouses, PicStone, 0, CornerX(PicStone), CornerY(PicStone), gX, gY, $0);
  end
  else
  begin
    // Wood part of the house (may be seen below Stone part before construction is complete, e.g. Sawmill)
    fRenderList.AddSpriteG(rxHouses, PicWood, 0, CornerX(PicWood), CornerY(PicWood), gX, gY, $0, aWoodStep);
    if aStoneStep > 0 then
      fRenderList.AddSprite(rxHouses, PicStone, CornerX(PicStone), CornerY(PicStone), $0, aStoneStep);
  end;
end;


procedure TRenderPool.AddHouseWork(aHouse: THouseType; Loc: TKMPoint; aActSet: THouseActionSet; AnimStep: Cardinal; FlagColor: TColor4);
var
  Id: Cardinal;
  AT: THouseActionType;
  A: TKMAnimLoop;
  R: TRXData;
  CornerX, CornerY: Single;
begin
  if aActSet = [] then Exit;

  R := fRXData[rxHouses];

  //See if action is in set and render it
  for AT := Low(THouseActionType) to High(THouseActionType) do
  if AT in aActSet then
  begin
    A := gRes.HouseDat[aHouse].Anim[AT];
    if A.Count > 0 then
    begin
      Id := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
      CornerX := Loc.X + (R.Pivot[Id].X + A.MoveX) / CELL_SIZE_PX - 1;
      CornerY := Loc.Y + (R.Pivot[Id].Y + A.MoveY + R.Size[Id].Y) / CELL_SIZE_PX - 1
                       - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;

      fRenderList.AddSprite(rxHouses, Id, CornerX, CornerY, FlagColor);
    end;
  end;
end;


procedure TRenderPool.AddHouseSupply(aHouse: THouseType; Loc: TKMPoint; const R1, R2: array of Byte);
var
  Id, I, K: Integer;
  R: TRXData;

  procedure AddHouseSupplySprite(aId: Integer);
  var
    CornerX, CornerY: Single;
  begin
    if aId = 0 then Exit;

    CornerX := Loc.X + R.Pivot[aId].X / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (R.Pivot[aId].Y + R.Size[aId].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, aId, CornerX, CornerY);
  end;

begin
  R := fRXData[rxHouses];

  for I := 1 to 4 do
  if (R1[I - 1]) > 0 then
  begin
    Id := gRes.HouseDat[aHouse].SupplyIn[I, Min(R1[I - 1], 5)] + 1;

    // Need to swap Coal and Steel for the ArmorSmithy
    // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
    if (aHouse = ht_ArmorSmithy) and (I in [1,2]) then
      Id := gRes.HouseDat[aHouse].SupplyIn[3-I, Min(R1[I - 1], 5)] + 1;

    AddHouseSupplySprite(Id);
  end;

  for I := 1 to 4 do
  if (R2[I - 1]) > 0 then
  begin
    // Makes compiler happy
    Id := 0;

    // Exception for houses whose wares are layered
    if aHouse in [ht_WeaponSmithy, ht_ArmorSmithy, ht_WeaponWorkshop, ht_ArmorWorkshop] then
    begin
      for K := 1 to Min(R2[I - 1], 5) do
      begin
        Id := gRes.HouseDat[aHouse].SupplyOut[I, K] + 1;
        // Need to swap Shields and Armor for the ArmorSmithy
        // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
        if (aHouse = ht_ArmorSmithy) and (I in [1,2]) then
          Id := gRes.HouseDat[aHouse].SupplyOut[3-I, K] + 1;
      end;
    end else
      Id := gRes.HouseDat[aHouse].SupplyOut[I, Min(R2[I - 1], 5)] + 1;

    AddHouseSupplySprite(Id);
  end;
end;


procedure TRenderPool.AddHouseMarketSupply(Loc: TKMPoint; ResType: TWareType; ResCount:word; AnimStep: Integer);
var
  i, Id: Integer;
  CornerX, CornerY: Single;
  R: TRXData;
begin
  if ResType = wt_Horse then // Horses are a beast, BeastId is the count, age is 1
    for i:=1 to Min(ResCount, MarketWares[ResType].Count) do // Render each beast
      AddHouseStableBeasts(ht_Marketplace, Loc, i, 1, AnimStep, rxHouses)
  else
  begin
    if MarketWares[ResType].Count = 0 then exit;
    Id := (MarketWares[ResType].TexStart-1) + Min(ResCount, MarketWares[ResType].Count);
    if Id = 0 then Exit;

    R := fRXData[rxHouses];
    CornerX := Loc.X + (R.Pivot[Id].X + MarketWaresOffsetX) / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (R.Pivot[Id].Y + MarketWaresOffsetY + R.Size[Id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land[Loc.Y+1,Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, Id, CornerX, CornerY);
  end;
end;


procedure TRenderPool.AddHouseStableBeasts(aHouse: THouseType; Loc: TKMPoint; BeastId,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
var
  CornerX, CornerY: Single;
  Id: Integer;
  R: TRXData;
  A: TKMAnimLoop;
begin
  R := fRXData[aRX];

  A := gRes.HouseDat.BeastAnim[aHouse,BeastId,BeastAge];

  Id := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
  CornerX := Loc.X + (A.MoveX + R.Pivot[Id].X) / CELL_SIZE_PX - 1;
  CornerY := Loc.Y + (A.MoveY + R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(aRX, Id, CornerX, CornerY);
end;


// aRenderPos has gTerrain.HeightAt factored in already, aTilePos is on tile coordinates for Z ordering
procedure TRenderPool.AddProjectile(aProj: TProjectileType; aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
var
  FOW: Byte;
  Id: Integer;
  R: TRXData;
  CornerX, CornerY: Single;
  Ground: Single;
begin
  // We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not gTerrain.TileInMapCoords(Round(aRenderPos.X), Round(aRenderPos.Y)) then Exit;

  if DYNAMIC_FOG_OF_WAR then
  begin
    FOW := gMySpectator.FogOfWar.CheckRevelation(aRenderPos);
    if FOW <= 128 then Exit; // Don't render objects which are behind FOW
  end;

  case aProj of
    pt_Arrow:     with gRes.UnitDat[ut_Bowman].UnitAnim[ua_Spec, aDir] do
                    Id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    pt_Bolt:      with gRes.UnitDat[ut_Arbaletman].UnitAnim[ua_Spec, aDir] do
                    Id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    pt_SlingRock: with gRes.UnitDat[ut_Slingshot].UnitAnim[ua_Spec, aDir] do
                    Id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    pt_TowerRock: Id := ProjectileBounds[aProj, 1] + 1;
    else          Id := 1; // Nothing?
  end;

  R := fRXData[rxUnits];

  CornerX := R.Pivot[Id].X / CELL_SIZE_PX - 1;
  CornerY := (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1;

  case aProj of
    pt_Arrow, pt_Bolt, pt_SlingRock:  Ground := aTilePos.Y + (0.5 - Abs(Min(aFlight, 1) - 0.5)) - 0.5;
    pt_TowerRock:                     Ground := aTilePos.Y + Min(aFlight, 1)/5 - 0.4;
    else                              Ground := aTilePos.Y - 1; // Nothing?
  end;

  fRenderList.AddSpriteG(rxUnits, Id, 0, aRenderPos.X + CornerX, aRenderPos.Y + CornerY, aTilePos.X - 1, Ground);
end;


procedure TRenderPool.AddUnit(aUnit: TUnitType; aUID: Integer; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  CornerX, CornerY, Ground: Single;
  Id, Id0: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := gRes.UnitDat[aUnit].UnitAnim[aAct, aDir];
  Id := A.Step[StepId mod Byte(A.Count) + 1] + 1;
  Id0 := A.Step[UnitStillFrames[aDir] mod Byte(A.Count) + 1] + 1;
  if Id <= 0 then exit;
  R := fRXData[rxUnits];

  CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
  CornerY := gTerrain.FlatToHeight(pX, pY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;
  Ground := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;

  if DoImmediateRender then
    RenderSprite(rxUnits, Id, CornerX, CornerY, FlagColor, Deleting)
  else
    if NewInst then
      fRenderList.AddSpriteG(rxUnits, Id, aUID, CornerX, CornerY, pX, Ground, FlagColor)
    else
      fRenderList.AddSprite(rxUnits, Id, CornerX, CornerY, FlagColor);

  if SHOW_UNIT_MOVEMENT then
  if NewInst then
  begin
    gRenderAux.DotOnTerrain(pX, pY, FlagColor);
    gRenderAux.Dot(CornerX, CornerY, $FF000080);
  end;
end;


procedure TRenderPool.AddHouseEater(Loc: TKMPoint; aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
var
  CornerX, CornerY: Single;
  Id: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := gRes.UnitDat[aUnit].UnitAnim[aAct, aDir];
  Id := A.Step[StepId mod Byte(A.Count) + 1] + 1;
  if Id <= 0 then exit;
  R := fRXData[rxUnits];

  // Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  CornerX := Loc.X + OffX + R.Pivot[Id].X / CELL_SIZE_PX - 1;
  CornerY := Loc.Y + OffY + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits, Id, CornerX, CornerY, FlagColor);
end;


procedure TRenderPool.AddUnitCarry(aCarry: TWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; pX,pY: Single);
var
  CornerX, CornerY: Single;
  Id: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := gRes.UnitDat.SerfCarry[aCarry, aDir];
  Id := A.Step[StepId mod Byte(A.Count) + 1] + 1;
  if Id <= 0 then Exit;
  R := fRXData[rxUnits];

  CornerX := pX + (R.Pivot[Id].X + a.MoveX) / CELL_SIZE_PX;
  CornerY := gTerrain.FlatToHeight(pX, pY) + (R.Pivot[Id].Y + R.Size[Id].Y + a.MoveY) / CELL_SIZE_PX;
  fRenderList.AddSprite(rxUnits, Id, CornerX, CornerY);
end;


procedure TRenderPool.AddUnitThought(aUnit: TUnitType; aAct: TUnitActionType;
                                     aDir: TKMDirection;
                                     Thought: TKMUnitThought; pX,pY: Single);
var
  Id: Integer;
  CornerX, CornerY, Ground: Single;
  R: TRXData;
  A: TKMAnimLoop;
  Id0: Integer;
begin
  if Thought = th_None then Exit;
  R := fRXData[rxUnits];

  // Unit position
  A := gRes.UnitDat[aUnit].UnitAnim[aAct, aDir];
  Id0 := A.Step[UnitStillFrames[aDir] mod Byte(A.Count) + 1] + 1;

  // Units feet
  Ground := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
  // The thought should be slightly lower than the unit so it goes OVER warrior flags
  Ground := Ground + THOUGHT_X_OFFSET;

  // Thought bubbles are animated in reverse
  Id := ThoughtBounds[Thought, 2] + 1 -
       (gGame.GameTickCount mod Word(ThoughtBounds[Thought, 2] - ThoughtBounds[Thought, 1]));

  CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
  CornerY := gTerrain.FlatToHeight(pX, pY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1.5;
  fRenderList.AddSpriteG(rxUnits, Id, 0, CornerX, CornerY, pX, Ground);
end;


procedure TRenderPool.AddUnitFlag(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection;
                                  FlagAnim: Integer; pX, pY: Single; FlagColor: TColor4);
const
  // Offsets for flags rendering in pixels
  FlagXOffset: array [TGroupType, TKMDirection] of shortint = (
    ( 0, 10, -1,  2,  1, -6,-10,  4, 13),  // gt_Melee
    ( 0,  6,  5,  7, -3,-10, -4, 10,  9),  // gt_AntiHorse
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gt_Ranged
    ( 0,  6,  2,  3, -5,-10, -8,  5,  6)); // gt_Mounted

  FlagYOffset: array [TGroupType, TKMDirection] of shortint = (
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gt_Melee
    ( 0, 23, 25, 25, 21, 20, 19, 20, 22),  // gt_AntiHorse
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gt_Ranged
    ( 0,  4, 16, 16,  4,  5,  2,  3,  4)); // gt_Mounted
var
  R: TRXData;
  A: TKMAnimLoop;
  Id0, IdFlag: Integer;
  FlagX, FlagY, Ground: Single;
begin
  R := fRXData[rxUnits];

  // Unit position
  A := gRes.UnitDat[aUnit].UnitAnim[aAct, aDir];
  Id0 := A.Step[UnitStillFrames[aDir] mod Byte(A.Count) + 1] + 1;

  // Units feet
  Ground := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;

  // Flag position
  A := gRes.UnitDat[aUnit].UnitAnim[ua_WalkArm, aDir];
  IdFlag := A.Step[FlagAnim mod Byte(A.Count) + 1] + 1;
  if IdFlag <= 0 then Exit;

  FlagX := pX + (R.Pivot[IdFlag].X + FlagXOffset[UnitGroups[aUnit], aDir]) / CELL_SIZE_PX - 0.5;
  FlagY := gTerrain.FlatToHeight(pX, pY) + (R.Pivot[IdFlag].Y + FlagYOffset[UnitGroups[aUnit], aDir] + R.Size[IdFlag].Y) / CELL_SIZE_PX - 2.25;

  fRenderList.AddSpriteG(rxUnits, IdFlag, 0, FlagX, FlagY, pX, Ground, FlagColor);
end;


procedure TRenderPool.AddUnitWithDefaultArm(aUnit: TUnitType; aUID: Integer; aAct: TUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  if aUnit = ut_Fish then aAct := FishCountAct[5]; // In map editor always render 5 fish
  AddUnit(aUnit, aUID, aAct, aDir, StepId, pX, pY, FlagColor, True, DoImmediateRender, Deleting);
  if gRes.UnitDat[aUnit].SupportsAction(ua_WalkArm) then
    AddUnit(aUnit, aUID, ua_WalkArm, aDir, StepId, pX, pY, FlagColor, True, DoImmediateRender, Deleting);
end;


{procedure TRenderPool.RenderObject(aRX: TRXType; aId: Word; pX,pY: Single);
type
    TVector4f = record X,Y,Z,W: Single; end;
    TColor4f = record R,G,B,A: Single; end;
const
    LightPos: TVector4f = (X:-1; Y:0; Z:-2; W:0);
    LightAmb: TColor4f = (R:0.1; G:0.1; B:0.1; A:0);
    LightDiff: TColor4f = (R:0.9; G:0.9; B:0.9; A:0);
    LightSpec: TColor4f = (R:1.0; G:1.0; B:1.0; A:0);
begin
  glPushMatrix;
  glPushAttrib(GL_LIGHTING_BIT or GL_DEPTH_BUFFER_BIT);
    glScalef(1, 1, CELL_SIZE_PX);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmb);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
    glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpec);
    glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);

    glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 0.0); //Directional lighting
    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR); //Specular does not depend on material color

    glTranslatef(pX, pY, -1);
    glRotatef(32.5, 1, 0, 0);
    glColor4f(0.8, 0.8, 0.8, 1);

    fSampleHouse.DrawModel;
  glPopAttrib;
  glPopMatrix;
end;}


procedure TRenderPool.RenderSprite(aRX: TRXType; aId: Word; pX,pY: Single;
  Col: TColor4; HighlightRed: Boolean = False);
begin
  with GFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    glBindTexture(GL_TEXTURE_2D, Tex.Id);
    if HighlightRed then glColor3f(1,0,0);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(pX                     , pY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if GFXData[aRX, aId].Alt.Id <> 0 then
  with GFXData[aRX, aId] do
  begin
    glColor4ubv(@Col);
    glBindTexture(GL_TEXTURE_2D, Alt.Id);
    glBegin(GL_QUADS);
      glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(pX                     , pY                      );
      glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
      glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  glBindTexture(GL_TEXTURE_2D, 0);
end;


// Param - defines at which level alpha-test will be set (acts like a threshhold)
// Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
// white there will have sprite rendered
// If there are two masks then we need to render sprite only there
// where its mask is white AND where second mask is black
procedure TRenderPool.RenderSpriteAlphaTest(
  aRX: TRXType; aId: Word; aWoodProgress: Single; pX, pY: Single;
  aId2: Word = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
begin
  // Skip rendering if alphas are zero (occurs so non-started houses can still have child sprites)
  if (aWoodProgress = 0) and (aStoneProgress = 0) then Exit;
  
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // Prepare stencil mask. Sprite will be rendered only where are white pixels
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    // Wood progress
    glAlphaFunc(GL_GREATER, 1 - aWoodProgress);
    with GFXData[aRX,aId] do
    begin
      glColor3f(1, 1, 1);
      glBindTexture(GL_TEXTURE_2D, Alt.Id);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(pX                     ,pY         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(pX                     ,pY-pxHeight/CELL_SIZE_PX);
      glEnd;
      glBindTexture(GL_TEXTURE_2D, 0);
    end;

    // Stone progress
    if aId2 <> 0 then
    begin
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);

      glAlphaFunc(GL_GREATER, 1 - aStoneProgress);
        with GFXData[aRX,aId2] do
        begin
          glColor3f(1, 1, 1);
          glBindTexture(GL_TEXTURE_2D, Alt.Id);
          glBegin(GL_QUADS);
            glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2                     ,Y2         );
            glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2         );
            glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2-pxHeight/CELL_SIZE_PX);
            glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2                     ,Y2-pxHeight/CELL_SIZE_PX);
          glEnd;
          glBindTexture(GL_TEXTURE_2D, 0);
        end;
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  // Render sprite
  with GFXData[aRX,aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    glBindTexture(GL_TEXTURE_2D, Tex.Id);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(pX                     ,pY         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(pX                     ,pY-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


procedure TRenderPool.CollectPlans(aRect: TKMRect);
var
  I: Integer;
begin
  fFieldsList.Clear;
  fHousePlansList.Clear;

  // Collect field plans (road, corn, wine)
  if gGame.GameMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti] then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetFieldPlans as it will give us plans multiple times for allies
        gHands[I].BuildList.FieldworksList.GetFields(fFieldsList, aRect, False)
    else
      gHands[gMySpectator.FOWIndex].GetFieldPlans(fFieldsList, aRect, False)
  end
  else
  begin
    // Field plans for self and allies
    // Include fake field plans for painting
    gMySpectator.Hand.GetFieldPlans(fFieldsList, aRect, True);
  end;

  // House plans for self and allies
  if gGame.GameMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti] then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetHousePlans as it will give us plans multiple times for allies
        gHands[I].BuildList.HousePlanList.GetOutlines(fHousePlansList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetHousePlans(fHousePlansList, aRect)
  end
  else
    gMySpectator.Hand.GetHousePlans(fHousePlansList, aRect);
end;


procedure TRenderPool.RenderWireTile(P: TKMPoint; Col: TColor4);
begin
  if not gTerrain.TileInMapCoords(P.X, P.Y) then exit;
  glColor4ubv(@Col);
  glBegin(GL_LINE_LOOP);
    with gTerrain do begin
      glVertex2f(P.X-1,P.Y-1-Land[P.Y  ,P.X  ].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X  ,P.Y-1-Land[P.Y  ,P.X+1].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X  ,P.Y-  Land[P.Y+1,P.X+1].Height/CELL_HEIGHT_DIV);
      glVertex2f(P.X-1,P.Y-  Land[P.Y+1,P.X  ].Height/CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


// Until profiling we use straightforward approach of recreating outline each frame
// Optimize later if needed
procedure TRenderPool.RenderHouseOutline(aHouse: TKMHouse);
var
  Loc: TKMPoint;
  I: Integer;
  X, Y: Word;
begin
  if aHouse = nil then
    Exit;

  // Get an outline of build area
  fHouseOutline.Clear;

  Loc := aHouse.GetPosition;
  gRes.HouseDat[aHouse.HouseType].Outline(fHouseOutline);

  glColor3f(0, 1, 1);
  glBegin(GL_LINE_LOOP);
    with gTerrain do
    for I := 0 to fHouseOutline.Count - 1 do
    begin
      X := Loc.X + fHouseOutline[I].X - 3;
      Y := Loc.Y + fHouseOutline[I].Y - 4;
      glVertex2f(X, Y - Land[Y+1, X+1].Height / CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


procedure TRenderPool.RenderSpriteOnTile(aLoc: TKMPoint; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
var
  pX, pY: Single;
begin
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  pX := aLoc.X - 0.5 + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.FlatToHeight(aLoc.X - 0.5, aLoc.Y - 0.5) -
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(rxGui, aId, pX, pY, aFlagColor);
end;


procedure TRenderPool.RenderSpriteOnTerrain(aLoc: TKMPointF; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
var
  pX, pY: Single;
begin
  // if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;
  pX := aLoc.X + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.FlatToHeight(aLoc.X, aLoc.Y) +
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(rxGui, aId, pX, pY, aFlagColor);
end;


procedure TRenderPool.RenderWireHousePlan(P: TKMPoint; aHouseType: THouseType);
var
  I: Integer;
begin
  fMarksList.Clear;
  gMySpectator.Hand.GetHouseMarks(P, aHouseType, fMarksList);

  for I := 0 to fMarksList.Count - 1 do
  if fMarksList.Tag[I] = TC_OUTLINE then
    RenderWireTile(fMarksList[I], $FFFFFF00) // Cyan rect
  else
    RenderSpriteOnTile(fMarksList[I], fMarksList.Tag[I]); // Icon
end;


procedure TRenderPool.RenderForegroundUI;
var
  P: TKMPoint;
  F: TKMPointF;
  U: TKMUnit;
  I, K: Integer;
  Tmp: Single;
  Rad, Slope: Byte;
begin
  if gGameCursor.Cell.Y * gGameCursor.Cell.X = 0 then Exit; // Caused a rare crash

  if gGame.IsMapEditor then
    gGame.MapEditor.Paint(plCursors, KMRect(0,0,0,0));

  P := gGameCursor.Cell;
  F := gGameCursor.Float;

  if (gGameCursor.Mode <> cmNone) and (gGameCursor.Mode <> cmHouses) and
     (gMySpectator.FogOfWar.CheckTileRevelation(P.X, P.Y) = 0) then
    RenderSpriteOnTile(P, TC_BLOCK)       // Red X
  else

  with gTerrain do
  case gGameCursor.Mode of
    cmNone:       ;
    cmErase:      if not gGame.IsMapEditor then
                  begin
                    if ((gMySpectator.Hand.BuildList.FieldworksList.HasFakeField(P) <> ft_None)
                        or gMySpectator.Hand.BuildList.HousePlanList.HasPlan(P)
                        or (gMySpectator.Hand.HousesHitTest(P.X, P.Y) <> nil))
                    then
                      RenderWireTile(P, $FFFFFF00) // Cyan quad
                    else
                      RenderSpriteOnTile(P, TC_BLOCK); // Red X
                  end;
    cmRoad:       if gMySpectator.Hand.CanAddFakeFieldPlan(P, ft_Road) then
                    RenderWireTile(P, $FFFFFF00) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmField:      if gMySpectator.Hand.CanAddFakeFieldPlan(P, ft_Corn) then
                    RenderWireTile(P, $FFFFFF00) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmWine:       if gMySpectator.Hand.CanAddFakeFieldPlan(P, ft_Wine) then
                    RenderWireTile(P, $FFFFFF00) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmHouses:     RenderWireHousePlan(P, THouseType(gGameCursor.Tag1)); // Cyan quads and red Xs
    cmBrush:      if gGameCursor.Tag1 <> 0 then
                  begin
                    Rad := gGameCursor.MapEdSize;
                    if Rad = 0 then
                      // Brush size smaller than one cell
                      gRenderAux.DotOnTerrain(Round(F.X), Round(F.Y), $FF80FF80)
                    else
                    // There are two brush types here, even and odd size
                    if Rad mod 2 = 1 then
                    begin
                      // First comes odd sizes 1,3,5..
                      Rad := Rad div 2;
                      for I := -Rad to Rad do
                      for K := -Rad to Rad do
                      // Rounding corners in a nice way
                      if (gGameCursor.MapEdShape = hsSquare)
                      or (Sqr(I) + Sqr(K) < Sqr(Rad+0.5)) then
                        RenderTile(Combo[TKMTerrainKind(gGameCursor.Tag1), TKMTerrainKind(gGameCursor.Tag1),1],P.X+K,P.Y+I,0);
                    end
                    else
                    begin
                      // Even sizes 2,4,6..
                      Rad := Rad div 2;
                      for I := -Rad to Rad - 1 do
                      for K := -Rad to Rad - 1 do
                      // Rounding corners in a nice way
                      if (gGameCursor.MapEdShape = hsSquare)
                      or (Sqr(I+0.5)+Sqr(K+0.5) < Sqr(Rad)) then
                        RenderTile(Combo[TKMTerrainKind(gGameCursor.Tag1), TKMTerrainKind(gGameCursor.Tag1),1],P.X+K,P.Y+I,0);
                    end;
                  end;
    cmTiles:      if gGameCursor.MapEdDir in [0..3] then
                    fRenderTerrain.RenderTile(gGameCursor.Tag1, P.X, P.Y, gGameCursor.MapEdDir)
                  else
                    fRenderTerrain.RenderTile(gGameCursor.Tag1, P.X, P.Y, (gTerrain.AnimStep div 5) mod 4); // Spin it slowly so player remembers it is on randomized
    cmObjects:    begin
                    // If there's object below - paint it in Red
                    RenderMapElement(gTerrain.Land[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, true, true);
                    RenderMapElement(gGameCursor.Tag1, gTerrain.AnimStep, P.X, P.Y, true);
                  end;
    cmMagicWater: ; //TODO: Render some effect to show magic water is selected
    cmElevate,
    cmEqualize:   begin
                    Rad := gGameCursor.MapEdSize;
                    Slope := gGameCursor.MapEdSlope;
                    for I := Max((Round(F.Y) - Rad), 1) to Min((Round(F.Y) + Rad), gTerrain.MapY -1) do
                    for K := Max((Round(F.X) - Rad), 1) to Min((Round(F.X) + Rad), gTerrain.MapX - 1) do
                    begin
                      case gGameCursor.MapEdShape of
                        hsCircle: Tmp := 1 - GetLength(I-Round(F.Y), K-Round(F.X)) / Rad;
                        hsSquare: Tmp := 1 - Math.max(abs(I-Round(F.Y)), abs(K-Round(F.X))) / Rad;
                        else                 Tmp := 0;
                      end;
                      Tmp := Power(Abs(Tmp), (Slope + 1) / 6) * Sign(Tmp); // Modify slopes curve
                      Tmp := EnsureRange(Tmp * 2.5, 0, 1); // *2.5 makes dots more visible
                      gRenderAux.DotOnTerrain(K, I, $FF or (Round(Tmp*255) shl 24));
                    end;
                    case gGameCursor.MapEdShape of
                      hsCircle: gRenderAux.CircleOnTerrain(round(F.X), round(F.Y), Rad, $00000000,  $FFFFFFFF);
                      hsSquare: gRenderAux.SquareOnTerrain(round(F.X) - Rad, round(F.Y) - Rad, round(F.X + Rad), round(F.Y) + Rad, $FFFFFFFF);
                    end;
                  end;
    cmUnits:      if (gGameCursor.Mode = cmUnits) and (gGameCursor.Tag1 = 255) then
                  begin
                    U := gTerrain.UnitsHitTest(P.X, P.Y);
                    if U <> nil then
                      AddUnitWithDefaultArm(U.UnitType, 0, ua_Walk,U.Direction,U.AnimStep,P.X+UNIT_OFF_X,P.Y+UNIT_OFF_Y,gMySpectator.Hand.FlagColor,true,true);
                  end
                  else
                    if CanPlaceUnit(P, TUnitType(gGameCursor.Tag1)) then
                      AddUnitWithDefaultArm(TUnitType(gGameCursor.Tag1), 0, ua_Walk, dir_S, UnitStillFrames[dir_S], P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, gMySpectator.Hand.FlagColor, True)
                    else
                      RenderSpriteOnTile(P, TC_BLOCK); // Red X
    cmMarkers:    case gGameCursor.Tag1 of
                    MARKER_REVEAL:        begin
                                            RenderSpriteOnTile(P, 394, gMySpectator.Hand.FlagColor);
                                            gRenderAux.CircleOnTerrain(P.X, P.Y,
                                             gGameCursor.MapEdSize,
                                             gMySpectator.Hand.FlagColor AND $10FFFFFF,
                                             gMySpectator.Hand.FlagColor);
                                          end;
                    MARKER_DEFENCE:       RenderSpriteOnTile(P, 519, gMySpectator.Hand.FlagColor);
                    MARKER_CENTERSCREEN:  RenderSpriteOnTile(P, 391, gMySpectator.Hand.FlagColor);
                    MARKER_AISTART:       RenderSpriteOnTile(P, 390, gMySpectator.Hand.FlagColor);
                  end;
  end;

  if DISPLAY_SOUNDS then gSoundPlayer.Paint;
end;


{ TRenderList }
constructor TRenderList.Create;
begin
  inherited;
  fCount := 0;
  SetLength(RenderList, 512); // Allocate some space
end;


destructor TRenderList.Destroy;
begin
  SetLength(RenderList, 0);
  inherited;
end;


function TRenderList.GetSelectionUID(CurPos: TKMPointF): Integer;
var
  I, K: Integer;
begin
  Result := -1; // Didn't hit anything
  // Skip if cursor is over FOW
  if gMySpectator.FogOfWar.CheckRevelation(CurPos) <= FOG_OF_WAR_MIN then Exit;
  // Select closest (higher Z) units first (list is in low..high Z-order)
  for I := Length(RenderOrder) - 1 downto 0 do
    if RenderOrder[I] <> -1 then
    begin
      K := RenderOrder[I];
      // Don't check child sprites, we don't want to select serfs by the long pike they are carrying
      if (RenderList[K].UID > 0) and KMInRect(CurPos, RenderList[K].SelectionRect) then
      begin
        Result := RenderList[K].UID;
        Exit;
      end;
    end;
end;


procedure TRenderList.Clear;
begin
  fCount := 0;
end;


procedure TRenderList.ClipRenderList;
var
  I, J: Integer;
begin
  SetLength(RenderOrder, fCount);
  J := 0;
  for I := 0 to fCount - 1 do
    if RenderList[I].NewInst then
    begin
      RenderOrder[J] := I;
      inc(J);
    end;
  SetLength(RenderOrder, J);
end;


// Sort all items in list from top-right to bottom-left
procedure TRenderList.SortRenderList;
var
  RenderOrderAux: TSmallIntArray;

  procedure DoQuickSort(aLo, aHi: Integer);
  var
    Lo, Hi: Integer;
    Mid: Single;
  begin
    Lo := aLo;
    Hi := aHi;
    Mid := RenderList[RenderOrder[(Lo + Hi) div 2]].Feet.Y;
    repeat
      while RenderList[RenderOrder[Lo]].Feet.Y < Mid do Inc(Lo);
      while RenderList[RenderOrder[Hi]].Feet.Y > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        SwapInt(RenderOrder[Lo], RenderOrder[Hi]);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > aLo then DoQuickSort(aLo, Hi);
    if Lo < aHi then DoQuickSort(Lo, aHi);
  end;

  procedure Merge(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd - 1 do
      if (A < aMid) and ((B >= aEnd)
      or (RenderList[RenderOrderAux[A]].Feet.Y <= RenderList[RenderOrderAux[B]].Feet.Y)) then
      begin
        RenderOrder[I] := RenderOrderAux[A];
        Inc(A);
      end else begin
        RenderOrder[I] := RenderOrderAux[B];
        Inc(B);
      end;
  end;

  // The same as Merge, but RenderOrder and RenderOrderAux are switched
  procedure MergeAux(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd-1 do
      if (A < aMid) and ((B >= aEnd)
      or (RenderList[RenderOrder[A]].Feet.Y <= RenderList[RenderOrder[B]].Feet.Y)) then
      begin
        RenderOrderAux[I] := RenderOrder[A];
        Inc(A);
      end else begin
        RenderOrderAux[I] := RenderOrder[B];
        Inc(B);
      end;
  end;

  // aUseAux tells us which array to store results in, it should flip each recurse
  procedure DoMergeSort(aStart, aEnd: Integer; aUseAux: Boolean);
  var
    Mid: Integer;
  begin
    if aEnd - aStart < 2 then Exit;
    Mid := (aStart + aEnd) div 2;
    DoMergeSort(aStart, Mid, not aUseAux);
    DoMergeSort(Mid, aEnd, not aUseAux);
    if aUseAux then
      MergeAux(aStart, Mid, aEnd)
    else
      Merge(aStart, Mid, aEnd);
  end;

begin
  ClipRenderList;
  if fCount > 0 then
  begin
    SetLength(RenderOrderAux, Length(RenderOrder));
    Move(RenderOrder[0], RenderOrderAux[0], Length(RenderOrder)*SizeOf(RenderOrder[0]));
    // Quicksort is unstable which causes Z fighting, so we use mergesort
    DoMergeSort(0, Length(RenderOrder), False);
    // DoQuickSort(0, Length(RenderOrder) - 1);
  end;
end;


// New items must provide their ground level
procedure TRenderList.AddSpriteG(aRX: TRXType; aId: Word; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then
    SetLength(RenderList, fCount + 256); // Book some space

  RenderList[fCount].Loc        := KMPointF(pX, pY); // Position of sprite, floating-point
  RenderList[fCount].Feet       := KMPointF(gX, gY); // Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             // RX library
  RenderList[fCount].Id         := aId;             // Texture Id
  RenderList[fCount].UID        := aUID;            // Object Id
  RenderList[fCount].NewInst    := True;            // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings

  if aUID > 0 then
    with RenderList[fCount].SelectionRect do
    begin
      Left := RenderList[fCount].Loc.X;
      Bottom := gY;
      Right := Left + GFXData[aRX, aId].PxWidth / CELL_SIZE_PX;
      Top := Bottom - GFXData[aRX, aId].PxHeight / CELL_SIZE_PX;
    end;

  Inc(fCount); // New item added
end;


// Child items don't need ground level
procedure TRenderList.AddSprite(aRX: TRXType; aId: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then
    SetLength(RenderList, fCount + 256); // Book some space

  RenderList[fCount].Loc        := KMPointF(pX,pY); // Position of sprite, floating-point
  RenderList[fCount].Feet       := RenderList[fCount-1].Feet;  // Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             // RX library
  RenderList[fCount].Id         := aId;             // Texture Id
  RenderList[fCount].UID        := 0;               // Child sprites aren't used for selecting units
  RenderList[fCount].NewInst    := False;           // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings

  Inc(fCount); // New item added
end;


procedure TRenderList.SendToRender(aId: Integer);
var
  Sp1, Sp2: TKMRenderSprite;
  Sp2Exists: Boolean;
begin
  // Shortcuts to Sprites info
  Sp1 := RenderList[aId];
  Sp2Exists := (aId + 1 < fCount);
  if Sp2Exists then
    Sp2 := RenderList[aId + 1];

  if Sp1.AlphaStep = -1 then
  begin
    fRenderPool.RenderSprite(Sp1.RX, Sp1.Id, Sp1.Loc.X, Sp1.Loc.Y, Sp1.TeamColor);
  end
  else
  begin
    // Houses are rendered as Wood+Stone part. For Stone we want to skip
    // Wooden part where it is occluded (so that smooth shadows dont overlay)

    // Check if next comes our child, Stone layer
    if Sp2Exists and not Sp2.NewInst and (Sp2.AlphaStep > 0) then
      fRenderPool.RenderSpriteAlphaTest(Sp1.RX, Sp1.Id, Sp1.AlphaStep, Sp1.Loc.X, Sp1.Loc.Y,
                                                Sp2.Id, Sp2.AlphaStep, Sp2.Loc.X, Sp2.Loc.Y)
    else
      fRenderPool.RenderSpriteAlphaTest(Sp1.RX, Sp1.Id, Sp1.AlphaStep, Sp1.Loc.X, Sp1.Loc.Y);
  end;

  if SHOW_GROUND_LINES and Sp1.NewInst then
  begin
    // Child ground lines are useless
    glBegin(GL_LINES);
      glColor3f(1,1,0.5);
      glVertex2f(Sp1.Feet.X + 0.15, gTerrain.FlatToHeight(Sp1.Feet).Y);
      glVertex2f(Sp1.Feet.X - 0.15, gTerrain.FlatToHeight(Sp1.Feet).Y);
    glEnd;
  end;
end;


// Now render all these items from list
procedure TRenderList.Render;
var
  I, K, objectCount: Integer;
begin
  fStat_Sprites := fCount;
  fStat_Sprites2 := 0;
  objectCount := Length(RenderOrder);

  for I := 0 to objectCount - 1 do
  if RenderOrder[I] <> -1 then
  begin
    K := RenderOrder[I];
    glPushMatrix;

      if RENDER_3D then
      begin
        glTranslatef(RenderList[K].Loc.X, RenderList[K].Loc.Y, 0);
        glRotatef(fRenderPool.rHeading, -1, 0, 0);
        glTranslatef(-RenderList[K].Loc.X, -RenderList[K].Loc.Y, 0);
      end;

      repeat // Render child sprites after their parent
        SendToRender(K);
        if SHOW_SEL_BUFFER and RenderList[K].NewInst and (RenderList[K].UID > 0) then
          gRenderAux.SquareOnTerrain(RenderList[K].SelectionRect.Left , RenderList[K].SelectionRect.Top,
                                     RenderList[K].SelectionRect.Right, RenderList[K].SelectionRect.Bottom, RenderList[K].UID or $FF000000);
        Inc(K);
        Inc(fStat_Sprites2);
      until ((K = fCount) or RenderList[K].NewInst);
    glPopMatrix;
  end;
end;


end.
