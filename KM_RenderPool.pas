unit KM_RenderPool;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  KM_Defaults, KM_CommonClasses, KM_Pics, KM_Render,
  KM_RenderTerrain, KM_ResourceSprites, KM_Points, KM_Houses, KM_Terrain;

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

    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteG(aRX: TRXType; aID: Word; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);

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
    fRenderTerrain: TRenderTerrain;
    procedure RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param: Single; pX, pY: Single; aFOW: Byte; aID2: Word = 0; Param2: Single = 0; X2: Single = 0; Y2: Single = 0);
    procedure RenderObject(aIndex: Byte; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderObjectQuad(aIndex: Byte; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderHouseOutline(aHouse: TKMHouse);

    //Terrain rendering sub-class
    procedure RenderTerrain;
    procedure RenderTerrainObjects(aRect: TKMRect; AnimStep: Cardinal);

    procedure RenderSprites;

    //Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderCursors;
    procedure RenderCursorWireQuad(P: TKMPoint; Col: TColor4);
    procedure RenderCursorWireHousePlan(P: TKMPoint; aHouseType: THouseType);
  public
    constructor Create(aRender: TRender);
    destructor Destroy; override;

    procedure AddAlert(aLoc: TKMPointF; aID: Word; aFlagColor: TColor4);
    procedure AddProjectile(aProj: TProjectileType; aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
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
    procedure AddUnitFlag(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; UnitAnim, FlagAnim: Integer; pX,pY: Single; FlagColor: TColor4);
    procedure AddUnitWithDefaultArm(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);

    procedure RenderCursorBuildIcon(aLoc: TKMPoint; aID: Integer = TC_BLOCK; aFlagColor: TColor4 = $FFFFFFFF);
    procedure RenderTile(Index: Byte; pX,pY,Rot: Integer);
    procedure RenderObjectOrQuad(aIndex: Byte; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);

    property RenderList: TRenderList read fRenderList;
    procedure SetRotation(aH,aP,aB: Integer);

    procedure Render;
  end;


var
  fRenderPool: TRenderPool;


implementation
uses KM_CommonTypes, KM_RenderAux, KM_PlayersCollection, KM_Projectiles, KM_Game, KM_Sound, KM_Resource,
  KM_ResourceHouse, KM_ResourceMapElements, KM_Units, KM_AIFields, KM_TerrainPainter;


constructor TRenderPool.Create(aRender: TRender);
var RT: TRXType;
begin
  inherited Create;

  for RT := Low(TRXType) to High(TRXType) do
    fRXData[RT] := fResource.Sprites[RT].RXData;

  fRender := aRender;
  fRenderList     := TRenderList.Create;
  fRenderTerrain  := TRenderTerrain.Create;
  fRenderAux      := TRenderAux.Create;
end;


destructor TRenderPool.Destroy;
begin
  fRenderList.Free;
  fRenderTerrain.Free;
  FreeThenNil(fRenderAux);
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

    //Background
    RenderTerrain;

    //Sprites are added by Terrain/Players/Projectiles, then sorted by position
    RenderSprites;

    //Cursor overlays (including blue-wire plans), go on top of everything
    RenderCursors;

    if DISPLAY_SOUNDS then fSoundLib.Paint;
  glPopAttrib;
end;


procedure TRenderPool.RenderTerrainObjects(aRect: TKMRect; AnimStep: Cardinal);
var
  I, K: Integer;
  TabletsList: TKMPointTagList;
begin
  if fGame.IsMapEditor and not (mlObjects in fGame.MapEditor.VisibleLayers) then
    Exit;

  with fTerrain do
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
    if Land[I, K].Obj <> 255 then
      RenderObjectOrQuad(Land[I, K].Obj, AnimStep, K, I);

  //Falling trees are in a separate list
  with fTerrain do
  for I := 0 to FallingTrees.Count - 1 do
  begin
    RenderObject(FallingTrees.Tag[I], AnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y);
    Assert(AnimStep - FallingTrees.Tag2[I] <= 100, 'Falling tree overrun?');
  end;

  //Tablets on house plans, for self and allies
  TabletsList := TKMPointTagList.Create;
  MyPlayer.GetPlansTablets(TabletsList, aRect, fGame.IsReplay);
  for I := 0 to TabletsList.Count - 1 do
    AddHouseTablet(THouseType(TabletsList.Tag[I]), TabletsList[I]);
  TabletsList.Free;
end;


procedure TRenderPool.RenderTile(Index: Byte; pX, pY, Rot: Integer);
begin
  fRenderTerrain.RenderTile(Index, pX, pY, Rot);
end;


procedure TRenderPool.RenderObjectOrQuad(aIndex: Byte; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  //Render either normal object or quad depending on what it is
  if MapElem[aIndex].WineOrCorn then
    RenderObjectQuad(aIndex,AnimStep,pX,pY,(aIndex in [54..57]),DoImmediateRender,Deleting) //54..57 are grapes, all others are doubles
  else
    RenderObject(aIndex,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderObject(aIndex: Byte; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;
  pX,pY: Integer;
  CornerX, CornerY: Single;
  gX, gY: Single;
  ID, ID0: Integer;
  FOW: Byte;
  A: TKMAnimLoop;
begin
  if MapElem[aIndex].Anim.Count = 0 then Exit;

  A := MapElem[aIndex].Anim;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(LocX,LocY,true);
  if FOW = 0 then exit; //Don't render objects which are unexplored
  if FOW <= 128 then AnimStep := 0; //Stop animation
  ID := A.Step[AnimStep mod Byte(A.Count) +1]+1;
  ID0 := A.Step[1] + 1;
  if ID <= 0 then exit;

  pX := LocX - 1;
  pY := LocY - 1;

  if aIndex = 61 then begin //Invisible wall
    CornerX := pX; //Required if DoImmediateRender = true
    CornerY := pY;
    //Render as a red outline in map editor mode
    if fGame.IsMapEditor then
    begin
      fRenderAux.Quad(pX+1, pY+1, $600000FF);
      RenderCursorWireQuad(KMPoint(pX+1, pY+1), $800000FF);
    end;
  end else begin
    R := fRXData[rxTrees];
    gX := pX + (R.Pivot[ID0].X + R.Size[ID0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
    CornerY := pY - fTerrain.HeightAt(gX, gY) + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX;
    if not DoImmediateRender then
      fRenderList.AddSpriteG(rxTrees, ID, CornerX, CornerY, gX, gY);

    //fRenderAux.DotOnTerrain(pX, pY, $FFFF0000);
    //fRenderAux.Dot(pX + R.Pivot[ID].X / CELL_SIZE_PX,
    //               fTerrain.FlatToHeight(pX, pY) + R.Pivot[ID].Y / CELL_SIZE_PX, $FFFFFF00);
    //fRenderAux.Dot(CornerX, CornerY, $FFFF00FF);
    //glRasterPos2f(pX - 1 + 0.1, pY - 1 + 0.1);
    //glPrint(inttostr(aIndex) + ':' + inttostr(ID));
  end;

  if DoImmediateRender then
    RenderSprite(rxTrees, ID, CornerX, CornerY, $FFFFFFFF, 255, Deleting);
end;


//4 objects packed on 1 tile for Corn and Grapes
procedure TRenderPool.RenderObjectQuad(aIndex: Byte; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;

  procedure AddSpriteBy(aAnimStep: Integer; pX,pY: Single);
  var
    ID, ID0: Integer;
    CornerX, CornerY, gX, gY: Single;
    A: TKMAnimLoop;
  begin
    A := MapElem[aIndex].Anim;
    ID := A.Step[aAnimStep mod Byte(A.Count) + 1] + 1;
    ID0 := A.Step[1] + 1;

    gX := pX + (R.Pivot[ID0].X + R.Size[ID0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
    CornerY := pY - fTerrain.HeightAt(gX, gY) + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX;

    if not DoImmediateRender then
      fRenderList.AddSpriteG(rxTrees, ID, CornerX, CornerY, gX, gY)
    else
      RenderSprite(rxTrees, ID, CornerX, CornerY, $FFFFFFFF, 255, Deleting);
  end;

var
  FOW: Byte;
begin
  FOW := MyPlayer.FogOfWar.CheckTileRevelation(pX, pY, True);
  if FOW <= 128 then AnimStep := 0; //Stop animation

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


//Render alert
procedure TRenderPool.AddAlert(aLoc: TKMPointF; aID: Word; aFlagColor: TColor4);
var
  CornerX, CornerY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];

  CornerX := aLoc.X + R.Pivot[aID].X / CELL_SIZE_PX;
  CornerY := fTerrain.FlatToHeight(aLoc).Y + R.Pivot[aID].Y / CELL_SIZE_PX;

  //todo: Beacons should ignore FOW
  fRenderList.AddSpriteG(rxGui, aID, CornerX, CornerY, aLoc.X, aLoc.Y, aFlagColor);
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
  CornerY := Loc.Y - fTerrain.HeightAt(gX, gY) + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 0.55;
  fRenderList.AddSpriteG(rxGui, ID, CornerX, CornerY, gX, gY);
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
  CornerX, CornerY, GroundWood, GroundStone, gX, gY: Single;
begin

  R := fRXData[rxHouses];
  ID := fResource.HouseDat[aHouse].WoodPic + 1;
  ID2 := fResource.HouseDat[aHouse].StonePic + 1;
  GroundWood := R.Pivot[ID].Y + R.Size[ID].Y;
  GroundStone := R.Pivot[ID2].Y + R.Size[ID2].Y;
  gX := Loc.X + (R.Pivot[ID].X + R.Size[ID].X / 2) / CELL_SIZE_PX - 1;
  gY := Loc.Y + Max(GroundWood, GroundStone) / CELL_SIZE_PX - 1.5;
  CornerX := Loc.X + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := Loc.Y + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSpriteG(rxHouses, ID, CornerX, CornerY, gX, gY, $0, Step);
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
    A := fResource.HouseDat[aHouse].Anim[AT];
    if A.Count > 0 then
    begin
      ID := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
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
      AddHouseStableBeasts(ht_Marketplace, Loc, i, 1, AnimStep, rxHouses)
  else
  begin
    if MarketWares[ResType].Count = 0 then exit;
    ID := (MarketWares[ResType].TexStart-1) + Min(ResCount, MarketWares[ResType].Count);
    if ID = 0 then Exit;

    R := fRXData[rxHouses];
    CornerX := Loc.X + (R.Pivot[ID].X + MarketWaresOffsetX) / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (R.Pivot[ID].Y + MarketWaresOffsetY + R.Size[ID].Y) / CELL_SIZE_PX - 1
                     - fTerrain.Land[Loc.Y+1,Loc.X].Height / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, ID, CornerX, CornerY);
  end;
end;


procedure TRenderPool.AddHouseStableBeasts(aHouse: THouseType; Loc: TKMPoint; BeastID,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
var
  CornerX, CornerY: Single;
  ID: Integer;
  R: TRXData;
  A: TKMAnimLoop;
begin
  R := fRXData[aRX];

  A := fResource.HouseDat.BeastAnim[aHouse,BeastID,BeastAge];

  ID := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
  CornerX := Loc.X + (A.MoveX + R.Pivot[ID].X) / CELL_SIZE_PX - 1;
  CornerY := Loc.Y + (A.MoveY + R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(aRX, ID, CornerX, CornerY);
end;


//aRenderPos has fTerrain.HeightAt factored in already, aTilePos is on tile coordinates for Z ordering
procedure TRenderPool.AddProjectile(aProj: TProjectileType; aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
var
  FOW: Byte;
  ID: Integer;
  R: TRXData;
  CornerX,CornerY: Single;
  Ground: Single;
begin
  //We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not fTerrain.TileInMapCoords(Round(aRenderPos.X), Round(aRenderPos.Y)) then Exit;

  FOW := MyPlayer.FogOfWar.CheckTileRevelation(Round(aRenderPos.X), Round(aRenderPos.Y), True);
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
    pt_Arrow, pt_Bolt, pt_SlingRock:  Ground := aTilePos.Y + (0.5 - Abs(Min(aFlight, 1) - 0.5)) - 0.5;
    pt_TowerRock:                     Ground := aTilePos.Y + Min(aFlight, 1)/5 - 0.4;
    else                              Ground := aTilePos.Y - 1; //Nothing?
  end;

  fRenderList.AddSpriteG(rxUnits, ID, aRenderPos.X + CornerX, aRenderPos.Y + CornerY, aTilePos.X - 1, Ground);
end;


procedure TRenderPool.AddUnit(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  CornerX, CornerY, Ground: Single;
  ID, ID0: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod Byte(A.Count) + 1] + 1;
  ID0 := A.Step[UnitStillFrames[aDir] mod Byte(A.Count) + 1] + 1;
  if ID <= 0 then exit;
  R := fRXData[rxUnits];

  CornerX := pX + R.Pivot[ID].X / CELL_SIZE_PX;
  CornerY := fTerrain.FlatToHeight(pX, pY) + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX;
  Ground := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;

  if NewInst then
    fRenderList.AddSpriteG(rxUnits, ID, CornerX, CornerY, pX, Ground, FlagColor)
  else
    fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY, FlagColor);

  if DoImmediateRender then
    RenderSprite(rxUnits, ID, CornerX, CornerY, FlagColor, 255, Deleting);

  if SHOW_UNIT_MOVEMENT then
  if NewInst then
  begin
    fRenderAux.DotOnTerrain(pX, pY, FlagColor);
    fRenderAux.Dot(CornerX, CornerY, $FF000080);
  end;
end;


procedure TRenderPool.AddHouseEater(Loc: TKMPoint; aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; OffX,OffY: Single; FlagColor: TColor4);
var
  CornerX, CornerY: Single;
  ID: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID := A.Step[StepID mod Byte(A.Count) + 1] + 1;
  if ID <= 0 then exit;
  R := fRXData[rxUnits];

  //Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  CornerX := Loc.X + OffX + R.Pivot[ID].X / CELL_SIZE_PX - 1;
  CornerY := Loc.Y + OffY + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1
                   - fTerrain.Land[Loc.Y + 1, Loc.X].Height / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY, FlagColor);
end;


procedure TRenderPool.AddUnitCarry(aCarry: TResourceType; aDir: TKMDirection; StepID: Integer; pX,pY: Single);
var
  CornerX, CornerY: Single;
  ID: Integer;
  A: TKMAnimLoop;
  R: TRXData;
begin
  A := fResource.UnitDat.SerfCarry[aCarry, aDir];
  ID := A.Step[StepID mod Byte(A.Count) + 1] + 1;
  if ID <= 0 then Exit;
  R := fRXData[rxUnits];

  CornerX := pX + (R.Pivot[ID].X + a.MoveX) / CELL_SIZE_PX;
  CornerY := fTerrain.FlatToHeight(pX, pY) + (R.Pivot[ID].Y + R.Size[ID].Y + a.MoveY) / CELL_SIZE_PX;
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
  CornerY := fTerrain.FlatToHeight(pX, pY) + (R.Pivot[ID].Y + R.Size[ID].Y) / CELL_SIZE_PX - 1.5;
  fRenderList.AddSprite(rxUnits, ID, CornerX, CornerY);
end;


procedure TRenderPool.AddUnitFlag(
  aUnit: TUnitType; aAct: TUnitActionType;
  aDir: TKMDirection; UnitAnim, FlagAnim: Integer;
  pX, pY: Single; FlagColor: TColor4);
var
  R: TRXData;
  A: TKMAnimLoop;
  ID0, IDFlag: Integer;
  FlagX, FlagY, Ground: Single;
begin
  R := fRXData[rxUnits];

  //Unit position
  A := fResource.UnitDat[aUnit].UnitAnim[aAct, aDir];
  ID0 := A.Step[UnitStillFrames[aDir] mod Byte(A.Count) + 1] + 1;

  //Units feet
  Ground := pY + (R.Pivot[ID0].Y + R.Size[ID0].Y) / CELL_SIZE_PX;

  //Flag position
  A := fResource.UnitDat[aUnit].UnitAnim[ua_WalkArm, aDir];
  IDFlag := A.Step[FlagAnim mod Byte(A.Count) + 1] + 1;
  if IDFlag <= 0 then Exit;

  FlagX := pX + (R.Pivot[IDFlag].X + FlagXOffset[UnitGroups[aUnit], aDir]) / CELL_SIZE_PX - 0.5;
  FlagY := fTerrain.FlatToHeight(pX, pY) + (R.Pivot[IDFlag].Y + FlagYOffset[UnitGroups[aUnit], aDir] + R.Size[IDFlag].Y) / CELL_SIZE_PX - 2.25;

  fRenderList.AddSpriteG(rxUnits, IDFlag, FlagX, FlagY, pX, Ground, FlagColor);
end;


procedure TRenderPool.AddUnitWithDefaultArm(aUnit: TUnitType; aAct: TUnitActionType; aDir: TKMDirection; StepID: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  if aUnit = ut_Fish then aAct := FishCountAct[5]; //In map editor always render 5 fish
  AddUnit(aUnit,aAct,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
  if fResource.UnitDat[aUnit].SupportsAction(ua_WalkArm) then
    AddUnit(aUnit,ua_WalkArm,aDir,StepID,pX,pY,FlagColor,True,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderSprite(aRX: TRXType; aID: Word; pX,pY: Single; Col: TColor4; aFOW: Byte; HighlightRed: Boolean = False);
var
  Lay, TopLay: Byte;
  F: TColor4;
begin
  //If there's AltID - render 2 layers instead of ordinary 1
  TopLay := 1 + Byte(GFXData[aRX, aID].Alt.ID <> 0);

  for Lay := 1 to TopLay do
  with GFXData[aRX, aID] do
  begin
    if Lay = 1 then
    begin
      glColor3ub(aFOW, aFOW, aFOW);
      glBindTexture(GL_TEXTURE_2D, Tex.ID);
      if HighlightRed then glColor3f(1,0,0);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(pX                     , pY                      );
        glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
        glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end else
    if (Lay = 2) and (aFOW <> 0) then  //Don't render colorflags if they aren't visible cos of FOW
    begin
      //Multiply RGB component of flag color by FOW
      F := ((Col and $FF) * aFOW shr 8) or
           ((((Col shr 8) and $FF) * aFOW shr 8) shl 8) or
           ((((Col shr 16) and $FF) * aFOW shr 8) shl 16) or
           Col and $FF000000;
      glColor4ubv(@F);
      glBindTexture(GL_TEXTURE_2D, Alt.ID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(pX                     , pY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
  end;

  glBindTexture(GL_TEXTURE_2D, 0);
end;


//  Param - defines at which level alpha-test will be set (acts like a threshhold)
//Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
//white there will have sprite rendered
//  If there are two masks then we need to render sprite only there
//where its mask is white AND where second mask is black
procedure TRenderPool.RenderSpriteAlphaTest(aRX: TRXType; aID: Word; Param: Single; pX, pY: Single;
  aFOW: Byte; aID2: Word = 0; Param2: Single = 0; X2: Single = 0; Y2: Single = 0);
begin
  glClear(GL_STENCIL_BUFFER_BIT);

  //Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  //Do not render anything on screen while setting up stencil mask
  glColorMask(False, False, False, False);

  //Prepare stencil mask. Sprite will be rendered only where are white pixels
  glEnable(GL_ALPHA_TEST);
  glBlendFunc(GL_ONE, GL_ZERO);

  glAlphaFunc(GL_GREATER, 1 - Param);
    with GFXData[aRX,aID] do
    begin
      glColor3f(1, 1, 1);
      glBindTexture(GL_TEXTURE_2D, Alt.ID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(pX-1                     ,pY-1         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
      glEnd;
      glBindTexture(GL_TEXTURE_2D, 0);
    end;

  if aID2 <> 0 then
  begin
    glStencilOp(GL_DECR, GL_DECR, GL_DECR);

    glAlphaFunc(GL_GREATER, 1 - Param2);
      with GFXData[aRX,aID2] do
      begin
        glColor3f(1, 1, 1);
        glBindTexture(GL_TEXTURE_2D, Alt.ID);
        glBegin(GL_QUADS);
          glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2-1                     ,Y2-1         );
          glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2-1+pxWidth/CELL_SIZE_PX,Y2-1         );
          glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2-1+pxWidth/CELL_SIZE_PX,Y2-1-pxHeight/CELL_SIZE_PX);
          glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2-1                     ,Y2-1-pxHeight/CELL_SIZE_PX);
        glEnd;
        glBindTexture(GL_TEXTURE_2D, 0);
      end;
  end;

  glDisable(GL_ALPHA_TEST);
  glAlphaFunc(GL_ALWAYS, 0);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); //Revert alpha mode

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  //Render sprite
  with GFXData[aRX,aID] do
  begin
    glColor3ub(aFOW, aFOW, aFOW);
    glBindTexture(GL_TEXTURE_2D, Tex.ID);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(pX-1                     ,pY-1         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(pX-1+pxWidth/CELL_SIZE_PX,pY-1-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(pX-1                     ,pY-1-pxHeight/CELL_SIZE_PX);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


procedure TRenderPool.RenderTerrain;
var
  Rect: TKMRect;
  FieldsList: TKMPointTagList;
  HousePlansList: TKMPointDirList;
begin
  Rect := fGame.Viewport.GetClip;

  //Field plans (road, corn, wine) for self and allies
  FieldsList := TKMPointTagList.Create;
  MyPlayer.GetFieldPlans(FieldsList, Rect, True, fGame.IsReplay); //Include fake field plans for painting

  //House plans for self and allies
  HousePlansList := TKMPointDirList.Create;
  MyPlayer.GetHousePlans(HousePlansList, Rect, fGame.IsReplay);


  fRenderTerrain.Render(Rect, fTerrain.AnimStep, MyPlayer.FogOfWar, FieldsList, HousePlansList);


  FreeAndNil(FieldsList);
  FreeAndNil(HousePlansList);

  if fPlayers.Highlight is TKMHouse then
    RenderHouseOutline(TKMHouse(fPlayers.Highlight));

  if fGame.IsMapEditor then
    fGame.MapEditor.Paint(plTerrain);

  if fAIFields <> nil then
    fAIFields.Paint(Rect);

  if SHOW_TERRAIN_WIRES then
    fRenderAux.Wires(Rect);

  if SHOW_TERRAIN_PASS <> 0 then
    fRenderAux.Passability(Rect, SHOW_TERRAIN_PASS);

  if SHOW_UNIT_MOVEMENT then
    fRenderAux.UnitMoves(Rect);
end;


//Collect all the sprites in the pool, sort them and render
procedure TRenderPool.RenderSprites;
var
  Rect: TKMRect;
begin
  Rect := fGame.Viewport.GetClip;

  RenderTerrainObjects(Rect, fTerrain.AnimStep);

  fPlayers.Paint; //Quite slow           //Units and houses
  fProjectiles.Paint;
  fGame.Alerts.Paint;

  fRenderList.Render;
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


//Until profiling we use straightforward approach of recreating outline each frame
//Optimize later if needed
procedure TRenderPool.RenderHouseOutline(aHouse: TKMHouse);
var
  Outline: TKMPointList;
  Loc: TKMPoint;
  I: Integer;
  X,Y: Word;
begin
  if aHouse = nil then
    Exit;

  //Get an outline of build area
  Outline := TKMPointList.Create;

  Loc := aHouse.GetPosition;
  fResource.HouseDat[aHouse.HouseType].GetOutline(Outline);

  glColor3f(0, 1, 1);
  glBegin(GL_LINE_LOOP);
    with fTerrain do
    for I := 0 to Outline.Count - 1 do
    begin
      X := Loc.X + Outline[I].X - 3;
      Y := Loc.Y + Outline[I].Y - 4;
      glVertex2f(X, Y - Land[Y+1, X+1].Height / CELL_HEIGHT_DIV);
    end;
  glEnd;

  Outline.Free;
end;


procedure TRenderPool.RenderCursorBuildIcon(aLoc: TKMPoint; aID: Integer = TC_BLOCK; aFlagColor: TColor4 = $FFFFFFFF);
var
  pX, pY: Single;
begin
  if not fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  pX := aLoc.X - 0.5 - GFXData[rxGui, aID].PxWidth/CELL_SIZE_PX/2;
  pY := fTerrain.FlatToHeight(aLoc.X - 0.5, aLoc.Y - 0.5) + GFXData[rxGui, aID].PxHeight/CELL_SIZE_PX/2;
  RenderSprite(rxGui, aID, pX, pY, aFlagColor, 255);
end;


procedure TRenderPool.RenderCursorWireHousePlan(P: TKMPoint; aHouseType: THouseType);
var
  I: Integer;
  MarksList: TKMPointTagList;
begin
  MarksList := TKMPointTagList.Create;
  MyPlayer.GetHouseMarks(P, aHouseType, MarksList);

  for I := 0 to MarksList.Count - 1 do
  if MarksList.Tag[I] = 0 then
    RenderCursorWireQuad(MarksList[I], $FFFFFF00) //Cyan rect
  else
    RenderCursorBuildIcon(MarksList[I], MarksList.Tag[I]); //icon

  MarksList.Free;
end;


procedure TRenderPool.RenderCursors;
var
  P: TKMPoint;
  F: TKMPointF;
  U: TKMUnit;
  I,K: Integer;
  Tmp: Single;
  Rad, Slope: Byte;
begin
  if GameCursor.Cell.Y*GameCursor.Cell.X = 0 then Exit; //Caused a rare crash

  if fGame.IsMapEditor then
    fGame.MapEditor.Paint(plCursors);

  P := GameCursor.Cell;
  F := GameCursor.Float;

  if (GameCursor.Mode <> cmNone) and (GameCursor.Mode <> cmHouses) and
     (MyPlayer.FogOfWar.CheckTileRevelation(P.X, P.Y, False) = 0) then
    RenderCursorBuildIcon(P)       //Red X
  else

  with fTerrain do
  case GameCursor.Mode of
    cmNone:     ;
    cmErase:    case fGame.GameMode of
                  gmMapEd:
                    begin
                      //With Buildings tab see if we can remove Fields or Houses
                      if (fGame.MapEditorInterface.GetShownPage = esp_Buildings)
                         and (    TileIsCornField(P)
                               or TileIsWineField(P)
                               or (Land[P.Y,P.X].TileOverlay=to_Road)
                               or (fPlayers.HousesHitTest(P.X, P.Y) <> nil))
                      then
                        RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                      else
                        RenderCursorBuildIcon(P); //Red X
                    end;

                  gmSingle, gmMulti, gmReplaySingle, gmReplayMulti:
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
    cmRoad:     if MyPlayer.CanAddFakeFieldPlan(P, ft_Road) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cmField:    if MyPlayer.CanAddFakeFieldPlan(P, ft_Corn) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cmWine:     if MyPlayer.CanAddFakeFieldPlan(P, ft_Wine) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cmWall:     if MyPlayer.CanAddFakeFieldPlan(P, ft_Wall) then
                  RenderCursorWireQuad(P, $FFFFFF00) //Cyan quad
                else
                  RenderCursorBuildIcon(P);       //Red X
    cmHouses:   RenderCursorWireHousePlan(P, THouseType(GameCursor.Tag1)); //Cyan quads and red Xs
    cmBrush:    if GameCursor.Tag1 <> 0 then
                begin
                  Rad := GameCursor.MapEdSize;
                  if Rad = 0 then
                    //brush size smaller than one cell
                    fRenderAux.DotOnTerrain(Round(F.X), Round(F.Y), $FF80FF80)
                  else
                  //There are two brush types here, even and odd size
                  if Rad mod 2 = 1 then
                  begin
                    //first comes odd sizes 1,3,5..
                    Rad := Rad div 2;
                    for I := -Rad to Rad do
                    for K := -Rad to Rad do
                    //Rounding corners in a nice way
                    if (GameCursor.MapEdShape = hsSquare)
                    or (Sqr(I) + Sqr(K) < Sqr(Rad+0.5)) then
                      RenderTile(Combo[TTerrainKind(GameCursor.Tag1), TTerrainKind(GameCursor.Tag1),1],P.X+K,P.Y+I,0);
                  end
                  else
                  begin
                    //even sizes 2,4,6..
                    Rad := Rad div 2;
                    for I := -Rad to Rad - 1 do
                    for K := -Rad to Rad - 1 do
                    //Rounding corners in a nice way
                    if (GameCursor.MapEdShape = hsSquare)
                    or (Sqr(I+0.5)+Sqr(K+0.5) < Sqr(Rad)) then
                      RenderTile(Combo[TTerrainKind(GameCursor.Tag1), TTerrainKind(GameCursor.Tag1),1],P.X+K,P.Y+I,0);
                  end;
                end;
    cmTiles:    if GameCursor.MapEdDir in [0..3] then
                  fRenderTerrain.RenderTile(GameCursor.Tag1, P.X, P.Y, GameCursor.MapEdDir)
                else
                  fRenderTerrain.RenderTile(GameCursor.Tag1, P.X, P.Y, (fTerrain.AnimStep div 5) mod 4); //Spin it slowly so player remembers it is on randomized
    cmObjects:  begin
                  //If there's object below - paint it in Red
                  RenderObjectOrQuad(fTerrain.Land[P.Y,P.X].Obj, fTerrain.AnimStep, P.X, P.Y, true, true);
                  RenderObjectOrQuad(GameCursor.Tag1, fTerrain.AnimStep, P.X, P.Y, true);
                end;
    cmElevate,
    cmEqualize: begin
                  Rad := GameCursor.MapEdSize;
                  Slope := GameCursor.MapEdSlope;
                  for I := Max((Round(F.Y) - Rad), 1) to Min((Round(F.Y) + Rad), fTerrain.MapY -1) do
                  for K := Max((Round(F.X) - Rad), 1) to Min((Round(F.X) + Rad), fTerrain.MapX - 1) do
                  begin
                    case GameCursor.MapEdShape of
                      hsCircle: Tmp := 1 - GetLength(I-Round(F.Y), K-Round(F.X)) / Rad;
                      hsSquare: Tmp := 1 - Math.max(abs(I-Round(F.Y)), abs(K-Round(F.X))) / Rad;
                      else                 Tmp := 0;
                    end;
                    Tmp := Power(Abs(Tmp), (Slope + 1) / 6) * Sign(Tmp); //Modify slopes curve
                    Tmp := EnsureRange(Tmp * 2.5, 0, 1); //*2.5 makes dots more visible
                    fRenderAux.DotOnTerrain(K, I, $FF or (Round(Tmp*255) shl 24));
                  end;
                  case GameCursor.MapEdShape of
                    hsCircle: fRenderAux.CircleOnTerrain(round(F.X), round(F.Y), Rad, $00000000,  $FFFFFFFF);
                    hsSquare: fRenderAux.SquareOnTerrain(round(F.X) - Rad, round(F.Y) - Rad, round(F.X + Rad), round(F.Y) + Rad, $00000000,  $FFFFFFFF);
                  end;
                end;
    cmUnits:    if (GameCursor.Mode = cmUnits) and (GameCursor.Tag1 = 255) then
                begin
                  U := fTerrain.UnitsHitTest(P.X, P.Y);
                  if U <> nil then
                    AddUnitWithDefaultArm(U.UnitType,ua_Walk,U.Direction,U.AnimStep,P.X+UNIT_OFF_X,P.Y+UNIT_OFF_Y,MyPlayer.FlagColor,true,true);
                end else
                if CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                  AddUnitWithDefaultArm(TUnitType(GameCursor.Tag1), ua_Walk, dir_S, UnitStillFrames[dir_S], P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, MyPlayer.FlagColor, True)
                else
                  RenderCursorBuildIcon(P); //Red X
    cmMarkers:  case GameCursor.Tag1 of
                  MARKER_REVEAL:        RenderCursorBuildIcon(P, 394, MyPlayer.FlagColor);
                  MARKER_DEFENCE:       RenderCursorBuildIcon(P, 519, MyPlayer.FlagColor);
                  MARKER_CENTERSCREEN:  RenderCursorBuildIcon(P, 391, MyPlayer.FlagColor);
                end;
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
procedure TRenderList.AddSpriteG(aRX: TRXType; aID: Word; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
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
var
  I, K: Integer;
  SecondID: Word;
  SecondAlpha: Single;
  X2, Y2: Single;
begin
  ClipRenderList; //Clip invisible items, Mark child items (RenderOrder[I] := -1), Apply FOW
  SortRenderList; //Sort items overlaying

  fStat_Sprites := fCount;
  fStat_Sprites2 := 0;

  for I := 0 to fCount - 1 do
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

      repeat //Render child sprites only after their parent
        with RenderList[K] do
        begin
          if AlphaStep = -1 then
            fRenderPool.RenderSprite(RX, ID, Loc.X, Loc.Y, TeamColor, FOWvalue)
          else
          begin
            //Houses are rendered as Wood+Stone part. For Stone we want to skip
            //Wooden part where it is occluded (so that smooth shadows dont overlay)

            //Check if next comes our child, Stone layer
            if (K+1 < fCount)
            and not RenderList[K+1].NewInst
            and (RenderList[K+1].AlphaStep > 0) then
            begin
              SecondID := RenderList[K+1].ID;
              SecondAlpha := RenderList[K+1].AlphaStep;
              X2 := RenderList[K+1].Loc.X;
              Y2 := RenderList[K+1].Loc.Y;
              fRenderPool.RenderSpriteAlphaTest(RX, ID, AlphaStep, Loc.X, Loc.Y, FOWvalue, SecondID, SecondAlpha, X2, Y2);
            end
            else
              fRenderPool.RenderSpriteAlphaTest(RX, ID, AlphaStep, Loc.X, Loc.Y, FOWvalue);

          end;

          if SHOW_GROUND_LINES and NewInst then //Don't render child (not NewInst) ground lines, since they are unused
          begin
            glBegin(GL_LINES);
              glColor3f(1,1,0.5);
              glVertex2f(Feet.X + 0.15, fTerrain.FlatToHeight(Feet).Y);
              glVertex2f(Feet.X - 0.15, fTerrain.FlatToHeight(Feet).Y);
            glEnd;
          end;
        end;
        inc(K);
        inc(fStat_Sprites2);
      until ((K = fCount) or RenderList[K].NewInst);
    glPopMatrix;
  end;
  fCount := 0;
end;


end.
