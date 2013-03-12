unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Utils, KM_ResourceTileset, KM_TerrainFinder;


type
  //Farmers/Woodcutters preferred activity
  TPlantAct = (taCut, taPlant, taAny);
  TTileOverlay = (to_None, to_Dig1, to_Dig2, to_Dig3, to_Dig4, to_Road, to_Wall);


  {Class to store all terrain data, aswell terrain routines}
  TKMTerrain = class
  private
    fAnimStep: Cardinal;
    fMapEditor: Boolean; //In MapEd mode some features behave differently
    fMapX: Word; //Terrain width and height
    fMapY: Word; //Terrain width and height

    fTileset: TKMTileset;
    fFinder: TKMTerrainFinder;

    fBoundsWC: TKMRect; //WC rebuild bounds used in FlattenTerrain (put outside to fight with recursion SO error in FlattenTerrain EnsureWalkable)

    function TileIsSand(Loc:TKMPoint): Boolean;
    function TileIsSoil(X,Y: Word): Boolean;
    function TileIsWalkable(Loc:TKMPoint): Boolean;
    function TileIsRoadable(Loc:TKMPoint): Boolean;
    function TileIsFactorable(Loc:TKMPoint): Boolean;

    function ChooseCuttingDirection(aLoc, aTree: TKMPoint; out CuttingPoint: TKMPointDir): Boolean;

    procedure UpdateFences(Loc: TKMPoint; CheckSurrounding: Boolean = True);

    procedure UpdateWalkConnect(const aSet: array of TWalkConnect; aRect: TKMRect; aDiagObjectsEffected:Boolean);

    procedure CCLFind(aWC: TWalkConnect; aPass: TPassability; aAllowDiag: Boolean);
  public
    Land: array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of record
      Terrain: Byte;
      Height: Byte;
      Rotation: Byte;
      Obj: Byte;

      //Age of tree, another independent variable since trees can grow on fields
      TreeAge: Byte; //Not init=0 .. Full=TreeAgeFull Depending on this tree gets older and thus could be chopped

      //Age of field/wine, another independent variable
      FieldAge: Byte; //Empty=0, 1, 2, 3, 4, Full=CORN_AGE_MAX  Depending on this special object maybe rendered (straw, grapes)

      //Tells us the stage of house construction or workers making a road
      TileLock: TTileLock;

      //Used to display half-dug road
      TileOverlay: TTileOverlay; //fs_None fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4 +Roads

      TileOwner: TPlayerIndex; //Who owns the tile by having a house/road/field on it
      IsUnit: Pointer; //Whenever there's a unit on that tile mark the tile as occupied and count the number
      IsVertexUnit: TKMVertexUsage; //Whether there are units blocking the vertex. (walking diagonally or fighting)

      //MAPEDITOR
      OldTerrain, OldRotation: Byte; //Only used for map editor

      //DEDUCTED
      Light: Single; //KaM stores node lighting in 0..32 range (-16..16), but I want to use -1..1 range
      Passability: TPassabilitySet; //Meant to be set of allowed actions on the tile

      WalkConnect: array [TWalkConnect] of Word; //Whole map is painted into interconnected areas

      Fence: TFenceType; //Fences (ropes, planks, stones)
      FenceSide: Byte; //Bitfield whether the fences are enabled
    end;

    FallingTrees: TKMPointTagList;

    constructor Create;
    destructor Destroy; override;
    procedure MakeNewMap(aWidth, aHeight: Integer; aMapEditor: Boolean);
    procedure LoadFromFile(FileName: string; aMapEditor: Boolean);
    procedure SaveToFile(aFile:string);

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;

    procedure SetTileLock(aLoc: TKMPoint; aTileLock: TTileLock);
    procedure UnlockTile(aLoc: TKMPoint);
    procedure SetRoads(aList: TKMPointList; aOwner: TPlayerIndex; aUpdateWalkConnects: Boolean = True);
    procedure SetField(Loc: TKMPoint; aOwner: TPlayerIndex; aFieldType: TFieldType);
    procedure SetHouse(Loc: TKMPoint; aHouseType: THouseType; aHouseStage: THouseStage; aOwner: TPlayerIndex; const aFlattenTerrain: Boolean = False);
    procedure SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);

    procedure RemovePlayer(aPlayer:TPlayerIndex);
    procedure RemRoad(Loc:TKMPoint);
    procedure RemField(Loc:TKMPoint);
    procedure SetWall(Loc:TKMPoint; aOwner:TPlayerIndex);
    procedure IncDigState(Loc:TKMPoint);
    procedure ResetDigState(Loc:TKMPoint);

    function CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType): Boolean;
    function CanPlaceGoldmine(X,Y: Word): Boolean;
    function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType): Boolean;
    function CanPlaceHouseFromScript(aHouseType: THouseType; Loc:TKMPoint): Boolean;
    function CanAddField(aX, aY: Word; aFieldType: TFieldType): Boolean;
    function CheckHeightPass(aLoc:TKMPoint; aPass:THeightPass): Boolean;
    procedure AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);

    function FindWineField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir): Boolean;
    function FindCornField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; aPlantAct:TPlantAct; out PlantAct:TPlantAct; out FieldPoint:TKMPointDir): Boolean;
    function FindStone(aLoc: TKMPoint; aRadius: Byte; aAvoidLoc: TKMPoint; aIgnoreWorkingUnits:Boolean; out StonePoint: TKMPointDir): Boolean;
    function FindOre(aLoc: TKMPoint; aRes: TResourceType; out OrePoint: TKMPoint): Boolean;
    function CanFindTree(aLoc: TKMPoint; aRadius: Word):Boolean;
    procedure FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aPlantAct: TPlantAct; Trees:TKMPointDirList; BestToPlant,SecondBestToPlant: TKMPointList);
    function FindFishWater(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; aIgnoreWorkingUnits:Boolean; out FishPoint: TKMPointDir): Boolean;
    function CanFindFishingWater(aLoc:TKMPoint; aRadius:integer): Boolean;
    function ChooseTreeToPlant(aLoc:TKMPoint):integer;
    procedure GetHouseMarks(aLoc:TKMPoint; aHouseType:THouseType; aList:TKMPointTagList);

    function WaterHasFish(aLoc:TKMPoint): Boolean;
    function CatchFish(aLoc:TKMPointDir; TestOnly: Boolean=false): Boolean;

    procedure SetTree(Loc:TKMPoint; ID:integer);
    procedure FallTree(Loc:TKMPoint);
    procedure ChopTree(Loc:TKMPoint);
    procedure RemoveObject(Loc:TKMPoint);

    procedure SowCorn(Loc:TKMPoint);
    procedure CutCorn(Loc:TKMPoint);
    procedure CutGrapes(Loc:TKMPoint);

    procedure DecStoneDeposit(Loc:TKMPoint);
    function DecOreDeposit(Loc:TKMPoint; rt:TResourceType): Boolean;

    function CheckPassability(Loc:TKMPoint; aPass:TPassability): Boolean;
    function HasUnit(Loc:TKMPoint): Boolean;
    function HasVertexUnit(Loc:TKMPoint): Boolean;
    function GetRoadConnectID(Loc:TKMPoint): Byte;
    function GetWalkConnectID(Loc:TKMPoint): Byte;
    function GetConnectID(aWalkConnect: TWalkConnect; Loc:TKMPoint): Byte;

    function CheckAnimalIsStuck(Loc: TKMPoint; aPass: TPassability; aCheckUnits: Boolean = True): Boolean;
    function GetOutOfTheWay(Loc, PusherLoc: TKMPoint; aPass: TPassability): TKMPoint;
    function FindSideStepPosition(Loc, Loc2, Loc3: TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: Boolean = False): Boolean;
    function Route_CanBeMade(LocA, LocB: TKMPoint; aPass: TPassability; aDistance: Single): Boolean;
    function Route_CanBeMadeToVertex(LocA, LocB: TKMPoint; aPass: TPassability): Boolean;
    function GetClosestTile(TargetLoc, OriginLoc: TKMPoint; aPass: TPassability; aAcceptTargetLoc: Boolean): TKMPoint;

    procedure UnitAdd(LocTo: TKMPoint; aUnit: Pointer);
    procedure UnitRem(LocFrom: TKMPoint);
    procedure UnitWalk(LocFrom,LocTo: TKMPoint; aUnit: Pointer);
    procedure UnitSwap(LocFrom,LocTo: TKMPoint; UnitFrom: Pointer);
    procedure UnitVertexAdd(LocTo: TKMPoint; Usage: TKMVertexUsage); overload;
    procedure UnitVertexAdd(LocFrom, LocTo: TKMPoint); overload;
    procedure UnitVertexRem(LocFrom: TKMPoint);
    function VertexUsageCompatible(LocFrom, LocTo: TKMPoint): Boolean;
    function GetVertexUsageType(LocFrom, LocTo: TKMPoint): TKMVertexUsage;

    function TileInMapCoords(X, Y: Integer; Inset: Byte = 0): Boolean;
    function VerticeInMapCoords(X, Y: Integer; Inset: Byte = 0): Boolean;
    function EnsureTileInMapCoords(X, Y: Integer; Inset: Byte = 0): TKMPoint;

    function TileGoodForIron(X, Y: Word): Boolean;
    function TileGoodForGoldmine(X, Y: Word): Boolean;
    function TileGoodForField(X, Y: Word): Boolean;
    function TileGoodForTree(X, Y: Word): Boolean;
    function TileIsWater(Loc: TKMPoint): Boolean; overload;
    function TileIsWater(X, Y: Word): Boolean; overload;
    function TileIsStone(X, Y: Word): Byte;
    function TileIsCoal(X, Y: Word): Byte;
    function TileIsIron(X, Y: Word): Byte;
    function TileIsGold(X, Y: Word): Byte;
    function TileIsCornField(Loc: TKMPoint): Boolean;
    function TileIsWineField(Loc: TKMPoint): Boolean;
    function TileIsLocked(aLoc: TKMPoint): Boolean;
    function UnitsHitTest(X, Y: Word): Pointer;
    function UnitsHitTestF(aLoc: TKMPointF): Pointer;
    function UnitsHitTestWithinRad(aLoc: TKMPoint; MinRad, MaxRad: Single; aPlayer: TPlayerIndex; aAlliance: TAllianceType; Dir: TKMDirection; const aClosest: Boolean): Pointer;

    function ObjectIsChopableTree(X,Y: Word): Boolean; overload;
    function ObjectIsChopableTree(Loc: TKMPoint; aStage: TChopableAge): Boolean; overload;
    function CanWalkDiagonaly(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean;

    function TopHill: Byte;
    procedure FlattenTerrain(Loc: TKMPoint; aUpdateWalkConnects: Boolean=true); overload;
    procedure FlattenTerrain(LocList: TKMPointList); overload;

    function ConvertCursorToMapCoord(inX,inY:single): Single;
    function FlatToHeight(inX, inY: Single): Single; overload;
    function FlatToHeight(aPoint: TKMPointF): TKMPointF; overload;
    function HeightAt(inX, inY: Single): Single;

    procedure UpdateLighting(aRect: TKMRect);
    procedure UpdatePassability(aRect: TKMRect); overload;
    procedure UpdatePassability(Loc: TKMPoint); overload;

    procedure IncAnimStep; //Lite-weight UpdateState for MapEd
    property AnimStep: Cardinal read fAnimStep;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;

    procedure UpdateState;
  end;


var
  //Terrain is a globally accessible resource by so many objects
  //In rare cases local terrain is used (e.g. main menu minimap)
  fTerrain: TKMTerrain;


implementation
uses KM_Log, KM_PlayersCollection,
  KM_Resource, KM_Units, KM_ResourceHouse, KM_ResourceMapElements, KM_Sound, KM_UnitActionStay, KM_Units_Warrior;


{ TKMTerrain }
constructor TKMTerrain.Create;
begin
  inherited;
  fAnimStep := 0;
  FallingTrees := TKMPointTagList.Create;
  fTileset := fResource.Tileset; //Local shortcut
end;


destructor TKMTerrain.Destroy;
begin
  FreeAndNil(FallingTrees);
  FreeAndNil(fFinder);
  inherited;
end;


//Reset whole map with default values
procedure TKMTerrain.MakeNewMap(aWidth, aHeight: Integer; aMapEditor: Boolean);
var I, K: Integer;
begin
  fMapEditor := aMapEditor;
  fMapX := Min(aWidth,  MAX_MAP_SIZE);
  fMapY := Min(aHeight, MAX_MAP_SIZE);

  for I := 1 to fMapY do
  for K := 1 to fMapX do
  with Land[I,K] do
  begin
    Terrain      := 0;
    Height       := 30 + KaMRandom(7);  //variation in Height
    Rotation     := KaMRandom(4);  //Make it random
    OldTerrain   := 0;
    OldRotation  := 0;
    Obj          := 255;             //none
    //Uncomment to enable random trees, but we don't want that for the map editor by default
    //if KaMRandom(16)=0 then Obj := ChopableTrees[KaMRandom(13)+1,4];
    TileOverlay  := to_None;
    TileLock     := tlNone;
    Passability  := []; //Gets recalculated later
    TileOwner    := -1;
    IsUnit       := nil;
    IsVertexUnit := vu_None;
    FieldAge     := 0;
    TreeAge      := IfThen(ObjectIsChopableTree(KMPoint(K,I), caAgeFull), TREE_AGE_FULL, 0);
    Fence        := fncNone;
    FenceSide    := 0;
  end;

  fFinder := TKMTerrainFinder.Create;
  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWork], KMRect(1, 1, fMapX, fMapY), True);
end;


procedure TKMTerrain.LoadFromFile(FileName: string; aMapEditor: Boolean);
var
  i,k:integer;
  S:TKMemoryStream;
  NewX,NewY: Integer;
begin
  fMapX := 0;
  fMapY := 0;

  if not CheckFileExists(FileName) then Exit;

  fMapEditor := aMapEditor;

  fLog.AddTime('Loading map file: ' + FileName);

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX <= MAX_MAP_SIZE) and (NewY <= MAX_MAP_SIZE), 'Can''t open the map cos it has too big dimensions');
    fMapX := NewX;
    fMapY := NewY;

    for i:=1 to fMapY do for k:=1 to fMapX do
    begin
      Land[i,k].OldTerrain   := 0;
      Land[i,k].OldRotation  := 0;
      Land[i,k].TileOverlay  := to_None;
      Land[i,k].TileLock     := tlNone;
      Land[i,k].Passability  := []; //Gets recalculated later
      Land[i,k].TileOwner    := -1;
      Land[i,k].IsUnit       := nil;
      Land[i,k].IsVertexUnit := vu_None;
      Land[i,k].FieldAge     := 0;
      Land[i,k].TreeAge      := 0;
      Land[i,k].Fence       := fncNone;
      Land[i,k].FenceSide   := 0;

      S.Read(Land[i,k].Terrain); //1
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Height); //3
      S.Read(Land[i,k].Rotation); //4
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Obj); //6
      S.Seek(17, soFromCurrent);
      if ObjectIsChopableTree(KMPoint(k,i), caAge1) then Land[i,k].TreeAge := 1;
      if ObjectIsChopableTree(KMPoint(k,i), caAge2) then Land[i,k].TreeAge := TREE_AGE_1;
      if ObjectIsChopableTree(KMPoint(k,i), caAge3) then Land[i,k].TreeAge := TREE_AGE_2;
      if ObjectIsChopableTree(KMPoint(k,i), caAgeFull) then Land[i,k].TreeAge := TREE_AGE_FULL;
      //Everything else is default
    end;
  finally
    S.Free;
  end;

  fFinder := TKMTerrainFinder.Create;
  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWork], KMRect(1, 1, fMapX, fMapY), True);
  fLog.AddTime('Map file loaded');
end;


//Save (export) map in KaM .map format with additional tile information on the end?
procedure TKMTerrain.SaveToFile(aFile: string);
var f:file; i,k:integer; c0,cF:cardinal; light,b205: Byte; SizeX,SizeY:Integer;
    ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
    Res:array[1..MAX_MAP_SIZE*2]of packed record X1,Y1,X2,Y2:integer; Typ: Byte; end;
begin
  ForceDirectories(ExtractFilePath(aFile));

  AssignFile(f,aFile); rewrite(f,1);

  //Dimensions must be stored as 4 byte integers
  SizeX := fMapX;
  SizeY := fMapY;
  blockwrite(f,SizeX,4);
  blockwrite(f,SizeY,4);

  c0 := 0;
  cF := $FFFFFFFF;
  b205 := 205;
  for i:=1 to fMapY do for k:=1 to fMapX do
  begin
    if TileIsCornField(KMPoint(k,i)) or TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,Land[i,k].OldTerrain,1) //Map file stores terrain, not the fields placed over it, so save OldTerrain rather than Terrain
    else
      blockwrite(f,Land[i,k].Terrain,1);

    light := round((Land[i,k].Light+1)*16);
    blockwrite(f,light,1); //Light
    blockwrite(f,Land[i,k].Height,1);

    if TileIsCornField(KMPoint(k,i)) or TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,Land[i,k].OldRotation,1) //Map file stores terrain, not the fields placed over it, so save OldRotation rather than Rotation
    else
      blockwrite(f,Land[i,k].Rotation,1);

    blockwrite(f,c0,1); //unknown

    //Don't save winefield objects as they are part of the DAT not map
    if TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,cF,1)
    else
      blockwrite(f,Land[i,k].Obj,1);

    blockwrite(f,cF,1); //Passability?

    blockwrite(f,cF,4); //unknown
    blockwrite(f,c0,3); //unknown
    //Border
    if (i=fMapY) or (k=fMapX) then
      blockwrite(f,b205,1) //Bottom/right = 205
    else
      if (i=1) or (k=1) then
        blockwrite(f,c0,1) //Top/left = 0
      else
        blockwrite(f,cF,1); //Rest of the screen = 255
    blockwrite(f,cF,1); //unknown - always 255
    blockwrite(f,b205,1); //unknown - always 205
    blockwrite(f,c0,2); //unknown - always 0
    blockwrite(f,c0,4); //unknown - always 0
  end;

  //Resource footer: Temporary hack to make the maps compatible with KaM. If we learn how resource footers
  //are formatted we can implement it, but for now it appears to work fine like this.
  ResHead.x1:=0;
  ResHead.Allocated := fMapX+fMapY;
  ResHead.Qty1:=0;
  ResHead.Qty2:=ResHead.Qty1;
  if ResHead.Qty1>0 then
    ResHead.x5:=ResHead.Qty1-1
  else
    ResHead.x5:=0;
  ResHead.Len17:=17;

  for i:=1 to ResHead.Allocated do
  begin
    Res[i].X1:=-842150451; Res[i].Y1:=-842150451;
    Res[i].X2:=-842150451; Res[i].Y2:=-842150451;
    Res[i].Typ:=255;
  end;

  blockwrite(f,ResHead,22);
  for i:=1 to ResHead.Allocated do blockwrite(f,Res[i],17);

  closefile(f);
end;


{Check if requested tile (X,Y) is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TKMTerrain.TileInMapCoords(X,Y:integer; Inset: Byte=0): Boolean;
begin
  Result := InRange(X,1+Inset,fMapX-1-Inset) and InRange(Y,1+Inset,fMapY-1-Inset);
end;


{Check if requested vertice is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TKMTerrain.VerticeInMapCoords(X,Y:integer; Inset: Byte=0): Boolean;
begin
  Result := InRange(X,1+Inset,fMapX-Inset) and InRange(Y,1+Inset,fMapY-Inset);
end;


{Ensure that requested tile is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TKMTerrain.EnsureTileInMapCoords(X,Y: Integer; Inset: Byte = 0):TKMPoint;
begin
  Result.X := EnsureRange(X,1+Inset,fMapX-1-Inset);
  Result.Y := EnsureRange(Y,1+Inset,fMapY-1-Inset);
end;


function TKMTerrain.TileGoodForIron(X,Y: Word): Boolean;
  function HousesNearTile: Boolean;
  var I,K: Integer;
  begin
    Result := False;
    for I := -1 to 1 do
    for K := -1 to 1 do
      if (Land[Y+I,X+K].TileLock in [tlFenced,tlDigged,tlHouse]) then
        Result := True;
  end;
begin
  Result := (Land[Y,X].Terrain in [109,166..170])
    and (Land[Y,X].Rotation mod 4 = 0) //only horizontal mountain edges allowed
    and ((Land[Y,X].Obj = 255) or (MapElem[Land[Y,X].Obj].CanBeRemoved))
    and TileInMapCoords(X,Y, 1)
    and not HousesNearTile
    and (Land[Y,X].TileLock = tlNone)
    and CheckHeightPass(KMPoint(X,Y), hpBuildingMines);
end;


function TKMTerrain.TileGoodForGoldmine(X,Y: Word): Boolean;
begin
  Result := TileInMapCoords(X,Y, 1)
    and (Land[Y,X].Terrain in [171..175])
    and (Land[Y,X].Rotation mod 4 = 0); //only horizontal mountain edges allowed
end;


function TKMTerrain.TileGoodForField(X,Y: Word): Boolean;
begin
  Result := TileIsSoil(X,Y)
    and not MapElem[Land[Y,X].Obj].AllBlocked
    and (Land[Y,X].TileLock = tlNone)
    and (Land[Y,X].TileOverlay <> to_Road)
    and not TileIsWineField(KMPoint(X,Y))
    and not TileIsCornField(KMPoint(X,Y))
    and CheckHeightPass(KMPoint(X,Y), hpWalking);
end;


function TKMTerrain.TileGoodForTree(X,Y: Word): Boolean;
  function IsObjectsNearby: Boolean;
  var I,K: Integer; P: TKMPoint;
  begin
    Result := False;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if ((I<>0) or (K<>0)) and TileInMapCoords(X+I, Y+K) then
        begin
          P := KMPoint(X+I, Y+K);

          //Tiles next to it can't be trees/stumps
          if MapElem[Land[P.Y,P.X].Obj].DontPlantNear then
            Result := True;

          //Tiles above or to the left can't be road/field/locked
          if (I <= 0) and (K <= 0) then
            if (Land[P.Y,P.X].TileLock <> tlNone)
            or (Land[P.Y,P.X].TileOverlay = to_Road)
            or TileIsCornField(P)
            or TileIsWineField(P) then
              Result := True;

          if Result then Exit;
        end;
  end;

  function HousesNearVertex: Boolean;
  var I,K: Integer;
  begin
    Result := False;
    for I := -1 to 1 do
    for K := -1 to 1 do
      if TileInMapCoords(X+K, Y+I)
      and (Land[Y+I,X+K].TileLock in [tlFenced,tlDigged,tlHouse]) then
      begin
        if (I+1 in [0,1]) and (K+1 in [0,1]) then //Only houses above/left of the tile
          Result := True;
      end;
  end;

begin
  //todo: Optimize above functions. Recheck UpdatePass and WC if the check Rects can be made smaller

  Result := TileIsSoil(X,Y)
    and not IsObjectsNearby //This function checks surrounding tiles
    and (Land[Y,X].TileLock = tlNone)
    and (X > 1) and (Y > 1) //Not top/left of map, but bottom/right is ok
    and (Land[Y,X].TileOverlay <> to_Road)
    and not HousesNearVertex
    //Woodcutter will dig out other object in favour of his tree
    and ((Land[Y,X].Obj = 255) or (MapElem[Land[Y,X].Obj].CanBeRemoved))
    and CheckHeightPass(KMPoint(X,Y), hpWalking);
end;


//Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed
function TKMTerrain.TileIsWater(Loc: TKMPoint): Boolean;
begin
  Result := fTileset.TileIsWater(Land[Loc.Y, Loc.X].Terrain);
end;


function TKMTerrain.TileIsWater(X,Y : Word): Boolean;
begin
  Result := fTileset.TileIsWater(Land[Y, X].Terrain);
end;


//Check if requested tile is sand suitable for crabs
function TKMTerrain.TileIsSand(Loc: TKMPoint): Boolean;
begin
  Result := fTileset.TileIsSand(Land[Loc.Y, Loc.X].Terrain);
end;


//Check if requested tile is Stone and returns Stone deposit
function TKMTerrain.TileIsStone(X,Y: Word): Byte;
begin
  Result := fTileset.TileIsStone(Land[Y, X].Terrain);
end;


function TKMTerrain.TileIsCoal(X,Y: Word): Byte;
begin
  Result := fTileset.TileIsCoal(Land[Y, X].Terrain);
end;


function TKMTerrain.TileIsIron(X,Y: Word): Byte;
begin
  Result := fTileset.TileIsIron(Land[Y, X].Terrain);
end;


function TKMTerrain.TileIsGold(X,Y: Word): Byte;
begin
  Result := fTileset.TileIsGold(Land[Y, X].Terrain);
end;


//Check if requested tile is soil suitable for fields and trees
function TKMTerrain.TileIsSoil(X,Y: Word): Boolean;
begin
  Result := fTileset.TileIsSoil(Land[Y, X].Terrain);
end;


//Check if requested tile is generally walkable
function TKMTerrain.TileIsWalkable(Loc: TKMPoint): Boolean;
begin
  Result := fTileset.TileIsWalkable(Land[Loc.Y, Loc.X].Terrain);
end;


//Check if requested tile is generally suitable for road building
function TKMTerrain.TileIsRoadable(Loc: TKMPoint): Boolean;
begin
  Result := fTileset.TileIsRoadable(Land[Loc.Y, Loc.X].Terrain);
end;


//Check if the tile is a corn field
function TKMTerrain.TileIsCornField(Loc: TKMPoint): Boolean;
begin
 //Tile can't be used as a field if there is road or any other overlay
  Result := fTileset.TileIsCornField(Land[Loc.Y, Loc.X].Terrain)
            and (Land[Loc.Y,Loc.X].TileOverlay = to_None);
end;


//Check if the tile is a wine field
function TKMTerrain.TileIsWineField(Loc: TKMPoint): Boolean;
begin
 //Tile can't be used as a winefield if there is road or any other overlay
 //It also must have right object on it
  Result := fTileset.TileIsWineField(Land[Loc.Y, Loc.X].Terrain)
            and (Land[Loc.Y,Loc.X].TileOverlay = to_None)
            and (Land[Loc.Y,Loc.X].Obj in [54..57]);
end;


//Check if this tile can be factored
function TKMTerrain.TileIsFactorable(Loc: TKMPoint): Boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and fTileset.TileIsFactorable(Land[Loc.Y, Loc.X].Terrain);
end;


function TKMTerrain.TileIsLocked(aLoc: TKMPoint): Boolean;
var
  U: TKMUnit;
begin
  U := Land[aLoc.Y,aLoc.X].IsUnit;
  //Action=nil can happen due to calling TileIsLocked during Unit.UpdateState.
  //Checks for Action=nil happen elsewhere, this is not the right place.
  if (U <> nil) and (U.GetUnitAction = nil) then
    Result := False
  else
    Result := (U <> nil) and (U.GetUnitAction.Locked);
end;


//Check if there's unit on the tile
//Note that IsUnit refers to where unit started walking to, not the actual unit position
//(which is what we used in unit interaction), so check all 9 tiles to get accurate result
function TKMTerrain.UnitsHitTest(X,Y: Word): Pointer;
var
  I, K: Integer;
  U: TKMUnit;
begin
  Result := nil;
  for I := max(Y - 1, 1) to Min(Y + 1, fMapY) do
  for K := max(X - 1, 1) to Min(X + 1, fMapX) do
  begin
    U := Land[I,K].IsUnit;
    if (U <> nil) and U.HitTest(X,Y) then
      Result := Land[I,K].IsUnit;
  end;
end;


//Test up to 4x4 related tiles around and pick unit whos no farther than 1 tile
function TKMTerrain.UnitsHitTestF(aLoc: TKMPointF): Pointer;
var
  I, K: Integer;
  U: TKMUnit;
  T: Single;
begin
  Result := nil;
  for I := Max(Trunc(aLoc.Y) - 1, 1) to Min(Trunc(aLoc.Y) + 2, fMapY) do
  for K := Max(Trunc(aLoc.X) - 1, 1) to Min(Trunc(aLoc.X) + 2, fMapX) do
  begin
    U := Land[I,K].IsUnit;
    if U <> nil then
    begin
      T := KMLengthSqr(U.PositionF, aLoc);
      if (T <= 1) and ((Result = nil) or (T < KMLengthSqr(TKMUnit(Result).PositionF, aLoc))) then
        Result := U;
    end;
  end;
end;


//Function to use with WatchTowers/Archers/Warriors
{ Should scan withing given radius and return closest unit with given Alliance status
  Should be optimized versus usual UnitsHitTest
  Prefer Warriors over Citizens}
function TKMTerrain.UnitsHitTestWithinRad(aLoc: TKMPoint; MinRad, MaxRad: Single; aPlayer: TPlayerIndex; aAlliance: TAllianceType; Dir: TKMDirection; const aClosest: Boolean): Pointer;
var
  I,K: Integer; //Counters
  LowX,LowY,HighX,HighY: Integer; //Ranges
  dX,dY: Integer;
  RequiredMaxRad: Single;
  U,C,W: TKMUnit; //CurrentUnit, BestWarrior, BestCitizen
  Warriors, Citizens: TList;
  P: TKMPoint;
begin
  W := nil;
  C := nil;

  if not aClosest then
  begin
    Warriors := TList.Create;
    Citizens := TList.Create;
  end;

  //Scan one tile further than the maximum radius due to rounding
  LowX := Max(Round(aLoc.X-(MaxRad+1)), 1); //1.42 gets rounded to 1
  LowY := Max(Round(aLoc.Y-(MaxRad+1)), 1); //1.42 gets rounded to 1
  HighX := Min(Round(aLoc.X+(MaxRad+1)), fMapX); //1.42 gets rounded to 1
  HighY := Min(Round(aLoc.Y+(MaxRad+1)), fMapY); //1.42 gets rounded to 1

  for I := LowY to HighY do
  for K := LowX to HighX do
  if (Land[I,K].IsUnit <> nil) then
  begin
    //Check archer sector. If it's not within the 90 degree sector for this direction, then don't use this tile (continue)
    dX := K - aLoc.X;
    dY := I - aLoc.Y;
    case Dir of
      dir_N : if not ((Abs(dX) <= -dY) and (dY < 0)) then Continue;
      dir_NE: if not ((dX > 0)         and (dY < 0)) then Continue;
      dir_E:  if not ((dX > 0) and (Abs(dY) <= dX))  then Continue;
      dir_SE: if not ((dX > 0)         and (dY > 0)) then Continue;
      dir_S : if not ((Abs(dX) <= dY)  and (dY > 0)) then Continue;
      dir_SW: if not ((dX < 0)         and (dY > 0)) then Continue;
      dir_W:  if not ((dX < 0) and (Abs(dY) <= -dX)) then Continue;
      dir_NW: if not ((dX < 0)         and (dY < 0)) then Continue;
    end;

    U := Land[I,K].IsUnit;

    //Alliance is the check that will invalidate most candidates, so do it early on
    if (U = nil)
    or (fPlayers.CheckAlliance(aPlayer, U.Owner) <> aAlliance) //How do WE feel about enemy, not how they feel about us
    or U.IsDeadOrDying
    or not U.Visible then //Inside of house
      Continue;

    //Don't check tiles farther than closest Warrior
    if aClosest and (W <> nil)
    and (KMLengthSqr(aLoc, KMPoint(K,I)) >= KMLengthSqr(aLoc, W.GetPosition)) then
      Continue; //Since we check left-to-right we can't exit just yet (there are possible better enemies below)

    //In KaM archers can shoot further than sight radius (shoot further into explored areas)
    //so CheckTileRevelation is required, we can't remove it to optimise.
    //But because it will not invalidate many candidates, check it late so other checks can do their work first
    if (fPlayers[aPlayer].FogOfWar.CheckTileRevelation(K,I,false) <> 255) then Continue;

    //This unit could be on a different tile next to KMPoint(k,i), so we cannot use that anymore.
    //There was a crash caused by VertexUsageCompatible checking (k,i) instead of U.GetPosition.
    //In that case aLoc = (37,54) and k,i = (39;52) but U.GetPosition = (38;53).
    //This shows why you can't use (k,i) in checks because it is distance >2 from aLoc! (in melee fight)
    P := U.GetPosition;

    RequiredMaxRad := MaxRad;
    if (MaxRad = 1) and KMStepIsDiag(aLoc, P) then
      RequiredMaxRad := 1.42; //Use diagonal radius sqrt(2) instead

    if CanWalkDiagonaly(aLoc, P.X, P.Y)
    and ((Abs(aLoc.X - P.X) <> 1)
          or (Abs(aLoc.Y - P.Y) <> 1)
          or VertexUsageCompatible(aLoc, P)
        )
    and InRange(KMLength(KMPointF(aLoc), U.PositionF), MinRad, RequiredMaxRad) //Unit's exact position must be close enough
    then
      if aClosest then
      begin
        if U is TKMUnitWarrior then
          W := U
        else
          C := U;
      end
      else
      begin
        if U is TKMUnitWarrior then
          Warriors.Add(U)
        else
          Citizens.Add(U);
      end;
  end;

  if aClosest then
  begin
    if W <> nil then
      Result := W
    else
      Result := C;
  end
  else
  begin
    if Warriors.Count > 0 then
      Result := Warriors[KaMRandom(Warriors.Count)]
    else
      if Citizens.Count > 0 then
        Result := Citizens[KaMRandom(Citizens.Count)]
      else
        Result := nil;

    Warriors.Free;
    Citizens.Free;
  end;
end;


function TKMTerrain.ObjectIsChopableTree(X,Y: Word): Boolean;
var I: Byte; K: TChopableAge;
begin
  Result := True;

  for I := 1 to Length(ChopableTrees) do
    for K := Low(TChopableAge) to High(TChopableAge) do
      if (Land[Y,X].Obj = ChopableTrees[I,K]) then Exit;

  Result := False;
end;


function TKMTerrain.ObjectIsChopableTree(Loc: TKMPoint; aStage: TChopableAge): Boolean;
var I: Byte;
begin
  Result := True;

  for I := 1 to Length(ChopableTrees) do
    if (Land[Loc.Y, Loc.X].Obj = ChopableTrees[I, aStage]) then Exit;

  Result := False;
end;


{Check wherever unit can walk from A to B diagonaly}
{Return true if direction is either walkable or not diagonal}
{Maybe this can also be used later for inter-tile passability}
function TKMTerrain.CanWalkDiagonaly(const aFrom: TKMPoint; bX, bY: SmallInt): Boolean;
begin
  Result := True;

  //Tiles are not diagonal to each other
  if (Abs(aFrom.X - bX) <> 1) or (Abs(aFrom.Y - bY) <> 1) then
    Exit; 
                                                                 //Relative tiles locations
  if (aFrom.X < bX) and (aFrom.Y < bY) then                               //   A
    Result := not MapElem[Land[bY, bX].Obj].DiagonalBlocked               //     B
  else
  if (aFrom.X < bX) and (aFrom.Y > bY) then                               //     B
    Result := not MapElem[Land[bY+1, bX].Obj].DiagonalBlocked             //   A
  else
  if (aFrom.X > bX) and (aFrom.Y > bY) then                               //   B
    Result := not MapElem[Land[aFrom.Y, aFrom.X].Obj].DiagonalBlocked     //     A
  else
  if (aFrom.X > bX) and (aFrom.Y < bY) then                               //     A
    Result := not MapElem[Land[aFrom.Y+1, aFrom.X].Obj].DiagonalBlocked;  //   B
end;


//Place lock on tile, any new TileLock replaces old one, thats okay
procedure TKMTerrain.SetTileLock(aLoc: TKMPoint; aTileLock: TTileLock);
var
  R: TKMRect;
begin
  Assert(aTileLock in [tlDigged, tlRoadWork, tlFieldWork], 'We expect only these 3 locks, that affect only 1 tile an don''t change neighbours Passability');

  Land[aLoc.Y, aLoc.X].TileLock := aTileLock;
  R := KMRect(aLoc);

  //Placing a lock on tile blocks tiles CanPlantTree
  UpdatePassability(KMRectGrow(R, 1));

  //Allowed TileLocks affect passability on this single tile
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], R, False);
end;


//Remove lock from tile
procedure TKMTerrain.UnlockTile(aLoc: TKMPoint);
var
  R: TKMRect;
begin
  Assert(Land[aLoc.Y, aLoc.X].TileLock in [tlDigged, tlRoadWork, tlFieldWork], 'We expect only these 3 locks, that affect only 1 tile an don''t change neighbours Passability');

  Land[aLoc.Y, aLoc.X].TileLock := tlNone;
  R := KMRect(aLoc);

  //Removing a lock from tile unblock BR tiles CanPlantTree
  UpdatePassability(KMRectGrow(R, 1));

  //Allowed TileLocks affect passability on this single tile
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], R, False);
end;


procedure TKMTerrain.SetRoads(aList: TKMPointList; aOwner: TPlayerIndex; aUpdateWalkConnects: Boolean = True);
var
  I: Integer;
  Bounds: TKMRect;
  HasBounds: Boolean;
begin
  if aList.Count = 0 then Exit; //Nothing to be done

  for I := 0 to aList.Count - 1 do
  begin
    Land[aList[I].Y, aList[I].X].TileOwner   := aOwner;
    Land[aList[I].Y, aList[I].X].TileOverlay := to_Road;
    Land[aList[I].Y, aList[I].X].FieldAge    := 0;
    UpdateFences(aList[I]);
  end;

  HasBounds := aList.GetBounds(Bounds);
  Assert(HasBounds);

  //Grow the bounds by extra tile because some passabilities
  //depend on road nearby (e.g. CanPlantTree)
  UpdatePassability(KMRectGrowBottomRight(Bounds));

  //Roads don't affect wcWalk or wcFish
  if aUpdateWalkConnects then
    UpdateWalkConnect([wcRoad], Bounds, False);
end;


procedure TKMTerrain.RemRoad(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOwner := -1;
  Land[Loc.Y,Loc.X].TileOverlay := to_None;
  Land[Loc.Y,Loc.X].FieldAge  := 0;
  UpdateFences(Loc);
  UpdatePassability(KMRectGrowBottomRight(KMRect(Loc)));

  //Roads don't affect wcWalk or wcFish
  UpdateWalkConnect([wcRoad], KMRect(Loc), False);
end;


procedure TKMTerrain.RemField(Loc: TKMPoint);
var ObjectChanged: Boolean;
begin
  Land[Loc.Y,Loc.X].TileOwner := -1;
  Land[Loc.Y,Loc.X].TileOverlay := to_None;
  Land[Loc.Y,Loc.X].Terrain := Land[Loc.Y,Loc.X].OldTerrain; //Reset terrain
  Land[Loc.Y,Loc.X].Rotation := Land[Loc.Y,Loc.X].OldRotation; //Reset terrain
  if Land[Loc.Y,Loc.X].Obj in [54..59] then
  begin
    Land[Loc.Y,Loc.X].Obj := 255; //Remove corn/wine
    ObjectChanged := True;
  end
  else
    ObjectChanged := False;
  Land[Loc.Y,Loc.X].FieldAge := 0;
  UpdateFences(Loc);
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));

  //Update affected WalkConnect's
  UpdateWalkConnect([wcWalk,wcRoad,wcWork], KMRectGrow(KMRect(Loc),1), ObjectChanged); //Winefields object block diagonals
end;


procedure TKMTerrain.RemovePlayer(aPlayer: TPlayerIndex);
var
  I, K: Word;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      if Land[I, K].TileOwner > aPlayer then
        Land[I, K].TileOwner := Pred(Land[I, K].TileOwner)
      else if Land[I, K].TileOwner = aPlayer then
        Land[I, K].TileOwner := -1;
end;


procedure TKMTerrain.SetWall(Loc: TKMPoint; aOwner: TPlayerIndex);
begin
  Land[Loc.Y,Loc.X].TileOwner := aOwner;
  Land[Loc.Y,Loc.X].TileOverlay := to_Wall;
  Land[Loc.Y,Loc.X].FieldAge := 0;
  UpdateFences(Loc);
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRect(Loc), False);
end;


{Set field on tile - corn/wine}
procedure TKMTerrain.SetField(Loc: TKMPoint; aOwner: TPlayerIndex; aFieldType: TFieldType);
begin
  Land[Loc.Y,Loc.X].TileOwner   := aOwner;
  Land[Loc.Y,Loc.X].TileOverlay := to_None;
  Land[Loc.Y,Loc.X].FieldAge    := 0;

  //Remember old terrain if we need to revert it in MapEd
  Land[Loc.Y,Loc.X].OldTerrain  := Land[Loc.Y, Loc.X].Terrain;
  Land[Loc.Y,Loc.X].OldRotation := Land[Loc.Y, Loc.X].Rotation;

  case aFieldType of
    ft_Road:      Land[Loc.Y,Loc.X].TileOverlay := to_Road;
    ft_Corn:      begin
                    Land[Loc.Y,Loc.X].Terrain  := 62;
                    Land[Loc.Y,Loc.X].Rotation := 0;
                    //If object is already corn then set the field age (some maps start with corn placed)
                    if not fMapEditor then //Don't do this in editor mode
                    case Land[Loc.Y,Loc.X].Obj of
                      58: begin  //Smaller greeninsh Corn
                            Land[Loc.Y,Loc.X].FieldAge := CORN_AGE_2;
                            Land[Loc.Y,Loc.X].Terrain  := 60;
                          end;
                      59: begin  //Full-grown Corn 1
                            //-1 because it is increased in update state, otherwise it wouldn't be noticed
                            Land[Loc.Y,Loc.X].FieldAge := CORN_AGE_FULL-1;
                            Land[Loc.Y,Loc.X].Terrain  := 60;
                          end;
                    end;
                  end;
    ft_Wine:      begin
                    Land[Loc.Y,Loc.X].Terrain  := 55;
                    Land[Loc.Y,Loc.X].Rotation := 0;
                    CutGrapes(Loc); //Set object and age
                  end;
    ft_InitWine:  begin
                    Land[Loc.Y,Loc.X].Terrain  := 55;
                    Land[Loc.Y,Loc.X].Rotation := 0;
                  end;
  end;

  UpdateFences(Loc);
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));
  //Walk and Road because Grapes are blocking diagonal moves
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(Loc)), (aFieldType = ft_Wine)); //Grape object blocks diagonal, others don't
end;


procedure TKMTerrain.IncDigState(Loc: TKMPoint);
begin
  case Land[Loc.Y,Loc.X].TileOverlay of
    to_Dig3: Land[Loc.Y,Loc.X].TileOverlay := to_Dig4;
    to_Dig2: Land[Loc.Y,Loc.X].TileOverlay := to_Dig3;
    to_Dig1: Land[Loc.Y,Loc.X].TileOverlay := to_Dig2;
    else     Land[Loc.Y,Loc.X].TileOverlay := to_Dig1;
  end;
end;


procedure TKMTerrain.ResetDigState(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOverlay:=to_None;
end;


{ Finds a winefield ready to be picked }
function TKMTerrain.FindWineField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir): Boolean;
var
  I: Integer;
  ValidTiles: TKMPointList;
  NearTiles, FarTiles: TKMPointDirList;
  P: TKMPoint;
begin
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);

  NearTiles := TKMPointDirList.Create;
  FarTiles := TKMPointDirList.Create;
  for I := 0 to ValidTiles.Count - 1 do
  begin
    P := ValidTiles[I];
    if not KMSamePoint(aAvoidLoc,P) then
      if TileIsWineField(P) then
        if Land[P.Y,P.X].FieldAge = CORN_AGE_MAX then
          if not TileIsLocked(P) then //Taken by another farmer
            if Route_CanBeMade(aLoc, P, CanWalk, 0) then
            begin
              if KMLengthSqr(aLoc, P) <= Sqr(aRadius div 2) then
                NearTiles.AddItem(KMPointDir(P, dir_NA))
              else
                FarTiles.AddItem(KMPointDir(P, dir_NA));
            end;
  end;

  //Prefer close tiles to reduce inefficiency with shared fields
  Result := NearTiles.GetRandom(FieldPoint);
  if not Result then
    Result := FarTiles.GetRandom(FieldPoint);

  NearTiles.Free;
  FarTiles.Free;
  ValidTiles.Free;
end;


{ Finds a corn field }
function TKMTerrain.FindCornField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; aPlantAct:TPlantAct; out PlantAct:TPlantAct; out FieldPoint:TKMPointDir): Boolean;
var
  I: Integer;
  ValidTiles, NearTiles, FarTiles: TKMPointList;
  P: TKMPoint;
begin
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);

  NearTiles := TKMPointList.Create;
  FarTiles := TKMPointList.Create;
  for I := 0 to ValidTiles.Count - 1 do
  begin
    P := ValidTiles[i];
    if not KMSamePoint(aAvoidLoc,P) then
      if TileIsCornField(P) then
        if((aPlantAct in [taAny, taPlant]) and (Land[P.Y,P.X].FieldAge = 0)) or
          ((aPlantAct in [taAny, taCut])   and (Land[P.Y,P.X].FieldAge = CORN_AGE_MAX)) then
          if not TileIsLocked(P) then //Taken by another farmer
            if Route_CanBeMade(aLoc, P, CanWalk, 0) then
            begin
              if KMLengthSqr(aLoc, P) <= Sqr(aRadius div 2) then
                NearTiles.AddEntry(P)
              else
                FarTiles.AddEntry(P);
            end;
  end;

  //Prefer close tiles to reduce inefficiency with shared fields
  Result := NearTiles.GetRandom(P);
  if not Result then
    Result := FarTiles.GetRandom(P);

  FieldPoint := KMPointDir(P, dir_NA);
  NearTiles.Free;
  FarTiles.Free;
  ValidTiles.Free;
  if not Result then
    PlantAct := taAny
  else
    if Land[FieldPoint.Loc.Y,FieldPoint.Loc.X].FieldAge = CORN_AGE_MAX then
      PlantAct := taCut
    else
      PlantAct := taPlant;
end;


{Find closest harvestable deposit of Stone}
{Return walkable tile below Stone deposit}
function TKMTerrain.FindStone(aLoc: TKMPoint; aRadius: Byte; aAvoidLoc: TKMPoint; aIgnoreWorkingUnits:Boolean; out StonePoint: TKMPointDir): Boolean;
var
  I: Integer;
  ValidTiles: TKMPointList;
  ChosenTiles: TKMPointList;
  P: TKMPoint;
begin
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);

  ChosenTiles := TKMPointList.Create;
  for I := 0 to ValidTiles.Count - 1 do
  begin
    P := ValidTiles[I];
    if (P.Y >= 2) //Can't mine stone from top row of the map (don't call TileIsStone with Y=0)
    and not KMSamePoint(aAvoidLoc, P)
    and (TileIsStone(P.X, P.Y - 1) > 0)
    and (aIgnoreWorkingUnits or not TileIsLocked(P)) //Already taken by another stonemason
    and Route_CanBeMade(aLoc, P, CanWalk, 0) then
      ChosenTiles.AddEntry(P);
  end;

  Result := ChosenTiles.GetRandom(P);
  StonePoint := KMPointDir(P, dir_N);
  ChosenTiles.Free;
  ValidTiles.Free;
end;


//Given aLoc the function return location of richest ore within predefined bounds
function TKMTerrain.FindOre(aLoc: TKMPoint; aRes: TResourceType; out OrePoint: TKMPoint): Boolean;
var
  I,K: Integer;
  RadLeft, RadRight, RadTop, RadBottom: Integer;
  R1,R2,R3,R4: Byte; //Ore densities
  L: array [1..4] of TKMPointList;
begin
  if not (aRes in [rt_IronOre, rt_GoldOre, rt_Coal]) then
    raise ELocError.Create('Wrong resource as Ore', aLoc);

  //Create separate list for each density, to be able to pick best one
  for I := 1 to 4 do
    L[I] := TKMPointList.Create;

  //These values have been measured from KaM
  case aRes of
    rt_GoldOre: begin RadLeft:=7; RadRight:=6; RadTop:=11; RadBottom:=2; R1:=144; R2:=145; R3:=146; R4:=147; end;
    rt_IronOre: begin RadLeft:=7; RadRight:=5; RadTop:=11; RadBottom:=2; R1:=148; R2:=149; R3:=150; R4:=151; end;
    rt_Coal:    begin RadLeft:=4; RadRight:=5; RadTop:= 5; RadBottom:=2; R1:=152; R2:=153; R3:=154; R4:=155; end;
    else        begin RadLeft:=0; RadRight:=0; RadTop:= 0; RadBottom:=0; R1:=  0; R2:=  0; R3:=  0; R4:=  0; end;
  end;

  for I := Max(aLoc.Y - RadTop, 1) to Min(aLoc.Y + RadBottom, fMapY - 1) do
  for K := Max(aLoc.X - RadLeft, 1) to Min(aLoc.X + RadRight, fMapX - 1) do
  begin
    if Land[I, K].Terrain = R1 then
    begin
      //Poorest ore gets mined in range - 2
      if InRange(I - aLoc.Y, - RadTop + 2, RadBottom - 2) then
        if InRange(K - aLoc.X, - RadLeft + 2, RadRight - 2) then
          L[1].AddEntry(KMPoint(K, I))
    end
    else if Land[I, K].Terrain = R2 then
    begin
      //Second poorest ore gets mined in range - 1
      if InRange(I - aLoc.Y, - RadTop + 1, RadBottom - 1) then
        if InRange(K - aLoc.X, - RadLeft + 1, RadRight - 1) then
          L[2].AddEntry(KMPoint(K, I))
    end
    else if Land[I, K].Terrain = R3 then
      //Always mine second richest ore
      L[3].AddEntry(KMPoint(K, I))
    else
      if Land[I, K].Terrain = R4 then
        // Always mine richest ore
        L[4].AddEntry(KMPoint(K, I));
  end;

  //Equation elements will be evalueated one by one until True is found
  Result := L[4].GetRandom(OrePoint) or
            L[3].GetRandom(OrePoint) or
            L[2].GetRandom(OrePoint) or
            L[1].GetRandom(OrePoint);

  for I := 1 to 4 do L[I].Free;
end;


function TKMTerrain.ChooseCuttingDirection(aLoc, aTree: TKMPoint; out CuttingPoint: TKMPointDir): Boolean;
var I, K, BestSlope, Slope: Integer;
begin
  BestSlope := MaxInt;
  Result := False; //It is already tested that we can walk to the tree, but double-check

  for I:=-1 to 0 do for K:=-1 to 0 do
  if Route_CanBeMade(aLoc, KMPoint(aTree.X+K, aTree.Y+I), CanWalk, 0) then
  begin
    Slope := Round(HeightAt(aTree.X+K-0.5, aTree.Y+I-0.5) * CELL_HEIGHT_DIV) - Land[aTree.Y, aTree.X].Height;
    //Cutting trees which are higher than us from the front looks visually poor, (axe hits ground) so avoid it where possible
    if (I = 0) and (Slope < 0) then Slope := Slope - 100; //Make it worse but not worse than initial BestSlope
    if Abs(Slope) < BestSlope then
    begin
      CuttingPoint := KMPointDir(aTree.X+K, aTree.Y+I, KMGetVertexDir(K, I));
      Result := True;
      BestSlope := Abs(Slope);
    end;
  end;
end;


function TKMTerrain.CanFindTree(aLoc: TKMPoint; aRadius: Word):Boolean;
var
  ValidTiles: TKMPointList;
  I: Integer;
  T: TKMPoint;
  CuttingPoint: TKMPointDir;
begin
  Result := False;
  //Scan terrain and add all trees/spots into lists
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);
  for I := 0 to ValidTiles.Count - 1 do
  begin
     //Store in temp variable for speed
    T := ValidTiles[I];

    if (KMLengthDiag(aLoc, T) <= aRadius)
    //Any age tree will do
    and (ObjectIsChopableTree(T, caAge1) or ObjectIsChopableTree(T, caAge2) or
         ObjectIsChopableTree(T, caAge3) or ObjectIsChopableTree(T, caAgeFull))
    and Route_CanBeMadeToVertex(aLoc, T, CanWalk)
    and ChooseCuttingDirection(aLoc, T, CuttingPoint) then
    begin
      Result := True;
      Break;
    end;
  end;
  ValidTiles.Free;
end;


//Return location of a Tree or a place to plant a tree depending on TreeAct
//taChop - Woodcutter wants to get a Tree because he went from home with an axe
//        (maybe his first target was already chopped down, so he either needs a tree or will go home)
//taPlant - Woodcutter specifically wants to get an empty place to plant a Tree
//taAny - Anything will do since Woodcutter is querying from home
//Result indicates if desired TreeAct place was found successfully
procedure TKMTerrain.FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aPlantAct: TPlantAct; Trees: TKMPointDirList; BestToPlant, SecondBestToPlant: TKMPointList);
var
  ValidTiles: TKMPointList;
  I: Integer;
  T: TKMPoint;
  CuttingPoint: TKMPointDir;
begin
  //Why do we use 3 lists instead of one like Corn does?
  //Because we should always prefer stumps over empty places
  //even if there's only 1 stump - we choose it

  //Scan terrain and add all trees/spots into lists
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);
  for I := 0 to ValidTiles.Count - 1 do
  begin
     //Store in temp variable for speed
    T := ValidTiles[I];

    if (KMLengthDiag(aLoc, T) <= aRadius)
    and not KMSamePoint(aAvoidLoc, T) then
    begin

      //Grownup tree
      if (aPlantAct in [taCut, taAny])
      and ObjectIsChopableTree(T, caAgeFull)
      and (Land[T.Y,T.X].TreeAge >= TREE_AGE_FULL)
      //Woodcutter could be standing on any tile surrounding this tree
      and not TileIsLocked(T)
      and ((T.X = 1) or not TileIsLocked(KMPoint(T.X-1, T.Y))) //if K=1, K-1 will be off map
      and ((T.Y = 1) or not TileIsLocked(KMPoint(T.X, T.Y-1)))
      and ((T.X = 1) or (T.Y = 1) or not TileIsLocked(KMPoint(T.X-1, T.Y-1)))
      and Route_CanBeMadeToVertex(aLoc, T, CanWalk) then
        if ChooseCuttingDirection(aLoc, T, CuttingPoint) then
          Trees.AddItem(CuttingPoint); //Tree

      if (aPlantAct in [taPlant, taAny])
      and TileGoodForTree(T.X, T.Y)
      and Route_CanBeMade(aLoc, T, CanWalk, 0)
      and not TileIsLocked(T) then //Taken by another woodcutter
        if ObjectIsChopableTree(T, caAgeStump) then
          BestToPlant.AddEntry(T) //Prefer to dig out and plant on stumps to avoid cluttering whole area with em
        else
          SecondBestToPlant.AddEntry(T); //Empty space and other objects that can be dug out (e.g. mushrooms) if no other options available
    end;
  end;
  ValidTiles.Free;
end;


{Find seaside}
{Return walkable tile nearby}
function TKMTerrain.FindFishWater(aLoc: TKMPoint; aRadius: Integer; aAvoidLoc: TKMPoint; aIgnoreWorkingUnits:Boolean; out FishPoint: TKMPointDir): Boolean;
var I,J,K: Integer;
    P: TKMPoint;
    ValidTiles: TKMPointList;
    ChosenTiles: TKMPointDirList;
begin
  ValidTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, canWalk, ValidTiles);

  ChosenTiles := TKMPointDirList.Create;
  for I := 0 to ValidTiles.Count - 1 do
  begin
    P := ValidTiles[I];
    //Check that this tile is valid
    if (aIgnoreWorkingUnits or not TileIsLocked(P)) //Taken by another fisherman
    and Route_CanBeMade(aLoc, P, CanWalk, 0)
    and not KMSamePoint(aAvoidLoc, P) then
      //Now find a tile around this one that is water
      for J := -1 to 1 do
        for K := -1 to 1 do
          if ((K <> 0) or (J <> 0))
          and TileInMapCoords(P.X+J, P.Y+K)
          and TileIsWater(P.X+J, P.Y+K)
          and WaterHasFish(KMPoint(P.X+J, P.Y+K)) then //Limit to only tiles which are water and have fish
            ChosenTiles.AddItem(KMPointDir(P, KMGetDirection(J, K)));
  end;

  Result := ChosenTiles.GetRandom(FishPoint);
  ChosenTiles.Free;
  ValidTiles.Free;
end;


function TKMTerrain.CanFindFishingWater(aLoc: TKMPoint; aRadius: Integer): Boolean;
var I,K:integer;
begin
  Result := False;
  for I := max(aLoc.Y - aRadius, 1) to Min(aLoc.Y + aRadius, fMapY-1) do
  for K := max(aLoc.X - aRadius, 1) to Min(aLoc.X + aRadius, fMapX-1) do
    if (KMLengthDiag(aLoc, KMPoint(K,I)) <= aRadius)
    and TileIsWater(K,I) then
    begin
      Result := True;
      Exit;
    end;
end;


function TKMTerrain.ChooseTreeToPlant(aLoc:TKMPoint):integer;
begin
  //This function randomly chooses a tree object based on the terrain type. Values matched to KaM, using all soil tiles.
  case Land[aLoc.Y,aLoc.X].Terrain of
    0..3,5,6,8,9,11,13,14,18,19,56,57,66..69,72..74,84..86,93..98,180,188: Result := ChopableTrees[1+KaMRandom(7), caAge1]; //Grass (oaks, etc.)
    26..28,75..80,182,190:                                                 Result := ChopableTrees[7+KaMRandom(2), caAge1]; //Yellow dirt
    16,17,20,21,34..39,47,49,58,64,65,87..89,183,191,220,247:              Result := ChopableTrees[9+KaMRandom(5), caAge1]; //Brown dirt (pine trees)
    else Result := ChopableTrees[1+KaMRandom(Length(ChopableTrees)), caAge1]; //If it isn't one of those soil types then choose a random tree
  end;
end;


procedure TKMTerrain.GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);
  procedure MarkPoint(aPoint: TKMPoint; aID: Integer);
  var I: Integer;
  begin
    for I := 0 to aList.Count - 1 do //Skip wires from comparison
      if (aList.Tag[I] <> 0) and KMSamePoint(aList[I], aPoint) then
        Exit;
    aList.AddEntry(aPoint, aID);
  end;

var
  i,k,s,t:integer;
  P2:TKMPoint;
  AllowBuild: Boolean;
  HA: THouseArea;
begin
  Assert(aList.Count = 0);
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
  if HA[i,k] <> 0 then
  begin

    if TileInMapCoords(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4,1) then
    begin
      //This can't be done earlier since values can be off-map
      P2 := KMPoint(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4);

      //Check house-specific conditions, e.g. allow shipyards only near water and etc..
      case aHouseType of
        ht_IronMine: AllowBuild := TileGoodForIron(P2.X, P2.Y);
        ht_GoldMine: AllowBuild := CanPlaceGoldmine(P2.X, P2.Y);
        else         AllowBuild := (CanBuild     in Land[P2.Y,P2.X].Passability);
      end;

      //Check surrounding tiles in +/- 1 range for other houses pressence
      if not AllowBuild then
      for s:=-1 to 1 do for t:=-1 to 1 do
      if (s<>0) or (t<>0) then  //This is a surrounding tile, not the actual tile
      if Land[P2.Y+t,P2.X+s].TileLock in [tlFenced,tlDigged,tlHouse] then
      begin
        MarkPoint(KMPoint(P2.X+s,P2.Y+t), TC_BLOCK);
        AllowBuild := false;
      end;

      //Mark the tile according to previous check results
      if AllowBuild then
      begin
        aList.AddEntry(P2, 0);
        if HA[i,k] = 2 then
          MarkPoint(P2, TC_ENTRANCE);
      end else
      begin
        if HA[i,k]=2 then
          MarkPoint(P2, TC_BLOCK_ENTRANCE)
        else
          if aHouseType in [ht_GoldMine,ht_IronMine] then
            MarkPoint(P2, TC_BLOCK_MINE)
          else
            MarkPoint(P2, TC_BLOCK);
      end;

    end
    else
      if TileInMapCoords(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4, 0) then
        MarkPoint(KMPoint(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4), 479);
  end;
end;


function TKMTerrain.WaterHasFish(aLoc: TKMPoint): Boolean;
begin
  Result := (fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aLoc.Y,aLoc.X].WalkConnect[wcFish],false) <> nil);
end;


function TKMTerrain.CatchFish(aLoc: TKMPointDir; TestOnly: Boolean = False): Boolean;
var MyFish: TKMUnitAnimal;
begin
  //Here we are catching fish in the tile 1 in the direction
  aLoc.Loc := KMPoint(KMGetPointInDir(aLoc.Loc, aLoc.Dir));
  MyFish := fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aLoc.Loc.Y, aLoc.Loc.X].WalkConnect[wcFish], not TestOnly);
  Result := (MyFish <> nil);
  if (not TestOnly) and (MyFish <> nil) then MyFish.ReduceFish; //This will reduce the count or kill it (if they're all gone)
end;


procedure TKMTerrain.SetTree(Loc: TKMPoint; ID: Integer);
begin
  Land[Loc.Y,Loc.X].Obj := ID;
  Land[Loc.Y,Loc.X].TreeAge := 1;

  //Add 1 tile on sides because surrounding tiles will be affected (CanPlantTrees)
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));

  //Tree could have blocked the only diagonal passage
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(Loc)), True); //Trees block diagonal
end;


{Remove the tree and place a falling tree instead}
procedure TKMTerrain.FallTree(Loc: TKMPoint);
var I: Integer;
begin
  for I := 1 to Length(ChopableTrees) do
    if ChopableTrees[I, caAgeFull] = Land[Loc.Y,Loc.X].Obj then
    begin
      Land[Loc.Y,Loc.X].Obj := ChopableTrees[I, caAgeStump];             //Set stump object
      FallingTrees.AddEntry(Loc,ChopableTrees[I, caAgeFall], fAnimStep); //along with falling tree
      fSoundLib.Play(sfx_TreeDown, Loc, True);
      Exit;
    end;
end;


{Remove the tree and place stump instead}
procedure TKMTerrain.ChopTree(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].TreeAge := 0;
  FallingTrees.RemoveEntry(Loc);
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));

  //WalkConnect takes diagonal passability into account
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(Loc)), True); //Trees block diagonals
end;


procedure TKMTerrain.RemoveObject(Loc:TKMPoint);
var BlockedDiagonal: Boolean;
begin
  if Land[Loc.Y,Loc.X].Obj <> 255 then
  begin
    BlockedDiagonal := MapElem[Land[Loc.Y,Loc.X].Obj].DiagonalBlocked;
    Land[Loc.Y,Loc.X].Obj := 255;
    if BlockedDiagonal then
      UpdateWalkConnect([wcWalk,wcRoad,wcWork], KMRectGrowTopLeft(KMRect(Loc)), True);
  end;
end;


procedure TKMTerrain.SowCorn(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 1;
  Land[Loc.Y,Loc.X].Terrain  := 61; //Plant it right away, don't wait for update state
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));
end;


procedure TKMTerrain.CutCorn(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 0;
  Land[Loc.Y,Loc.X].Terrain  := 63;
  Land[Loc.Y,Loc.X].Obj := 255;
end;


procedure TKMTerrain.CutGrapes(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 1;
  Land[Loc.Y,Loc.X].Obj := 54; //Reset the grapes
end;


{Extract one unit of stone}
procedure TKMTerrain.DecStoneDeposit(Loc:TKMPoint);

  procedure UpdateTransition(X,Y:integer);
  const TileID:array[0..15]of byte = (0,139,139,138,139,140,138,141,139,138,140,141,138,141,141,128);
         RotID:array[0..15]of byte = (0,  0,  1,  0,  2,  0,  1,  3,  3,  3,  1,  2,  2,  1,  0,  0);
  var Bits: Byte;
  begin
    if not TileInMapCoords(X,Y) or (TileIsStone(X,Y) > 0) then Exit;

    Bits := Byte(TileInMapCoords(  X,Y-1) and (TileIsStone(  X,Y-1)>0))*1 +
            Byte(TileInMapCoords(X+1,  Y) and (TileIsStone(X+1,  Y)>0))*2 +
            Byte(TileInMapCoords(  X,Y+1) and (TileIsStone(  X,Y+1)>0))*4 +
            Byte(TileInMapCoords(X-1,  Y) and (TileIsStone(X-1,  Y)>0))*8;

    //We UpdateTransition when the stone becomes grass, Bits can never = 15
    //The tile in center is fully mined and one below has Stoncutter on it,
    //hence there cant be any tile surrounded by stones from all sides
    Assert(Bits < 15);
    Land[Y,X].Terrain  := TileID[Bits];
    Land[Y,X].Rotation := RotID[Bits];
    if Land[Y,X].Terrain = 0 then Land[Y,X].Rotation := KaMRandom(4); //Randomise the direction of grass tiles
    UpdatePassability(Loc);
  end;

begin
  //Replace with smaller ore deposit tile (there are 2 sets of tiles, we can choose random)
  case Land[Loc.Y,Loc.X].Terrain of
    132, 137: Land[Loc.Y,Loc.X].Terrain := 131 + KaMRandom(2)*5;
    131, 136: Land[Loc.Y,Loc.X].Terrain := 130 + KaMRandom(2)*5;
    130, 135: Land[Loc.Y,Loc.X].Terrain := 129 + KaMRandom(2)*5;
    129, 134: Land[Loc.Y,Loc.X].Terrain := 128 + KaMRandom(2)*5;
    128, 133: begin
                Land[Loc.Y,Loc.X].Terrain  := 0;
                Land[Loc.Y,Loc.X].Rotation := KaMRandom(4);

                //Tile type has changed and we need to update these 5 tiles transitions:
                UpdateTransition(Loc.X,Loc.Y);
                UpdateTransition(Loc.X,Loc.Y-1); //    x
                UpdateTransition(Loc.X+1,Loc.Y); //  x X x
                UpdateTransition(Loc.X,Loc.Y+1); //    x
                UpdateTransition(Loc.X-1,Loc.Y);
              end;
    else      Exit;
  end;

  FlattenTerrain(Loc);
end;


{ Try to extract one unit of ore
  It may fail cos of two miners mining the same last piece of ore }
function TKMTerrain.DecOreDeposit(Loc:TKMPoint; rt:TResourceType): Boolean;
begin
  if not (rt in [rt_IronOre,rt_GoldOre,rt_Coal]) then
    raise ELocError.Create('Wrong ore decrease',Loc);

  Result := true;
  case Land[Loc.Y,Loc.X].Terrain of
    144: Land[Loc.Y,Loc.X].Terrain:=157+KaMRandom(3); //Gold
    145: Land[Loc.Y,Loc.X].Terrain:=144;
    146: Land[Loc.Y,Loc.X].Terrain:=145;
    147: Land[Loc.Y,Loc.X].Terrain:=146;
    148: Land[Loc.Y,Loc.X].Terrain:=160+KaMRandom(4); //Iron
    149: Land[Loc.Y,Loc.X].Terrain:=148;
    150: Land[Loc.Y,Loc.X].Terrain:=149;
    151: Land[Loc.Y,Loc.X].Terrain:=150;
    152: Land[Loc.Y,Loc.X].Terrain:=35 +KaMRandom(2); //Coal
    153: Land[Loc.Y,Loc.X].Terrain:=152;
    154: Land[Loc.Y,Loc.X].Terrain:=153;
    155: Land[Loc.Y,Loc.X].Terrain:=154;
    else Result := false;
  end;
  Land[Loc.Y,Loc.X].Rotation:=KaMRandom(4);
  UpdatePassability(Loc);
end;


procedure TKMTerrain.UpdatePassability(Loc: TKMPoint);
  procedure AddPassability(aPass: TPassability);
  begin
    Land[Loc.Y,Loc.X].Passability := Land[Loc.Y,Loc.X].Passability + [aPass];
  end;
var
  I, K: Integer;
  HousesNearTile, HousesNearVertex: Boolean;
begin
  Assert(TileInMapCoords(Loc.X, Loc.Y)); //First of all exclude all tiles outside of actual map

  Land[Loc.Y,Loc.X].Passability := [];

  if TileIsWalkable(Loc)
  and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
  and CheckHeightPass(Loc, hpWalking) then
    AddPassability(CanOwn);

  //For all passability types other than CanAll, houses and fenced houses are excluded
  if Land[Loc.Y,Loc.X].TileLock in [tlNone, tlFenced, tlFieldWork, tlRoadWork] then
  begin

    if TileIsWalkable(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
    and CheckHeightPass(Loc, hpWalking) then
      AddPassability(CanWalk);

    if (Land[Loc.Y,Loc.X].TileOverlay = to_Road)
    and (CanWalk in Land[Loc.Y,Loc.X].Passability) then //Not all roads are walkable, they must also have CanWalk passability
      AddPassability(CanWalkRoad);

    //Check for houses around this tile/vertex
    HousesNearTile := False;
    for i := -1 to 1 do
    for k := -1 to 1 do
      if TileInMapCoords(Loc.X+k, Loc.Y+i)
      and (Land[Loc.Y+i,Loc.X+k].TileLock in [tlFenced,tlDigged,tlHouse]) then
        HousesNearTile := True;

    if TileIsRoadable(Loc)
    and ((Land[Loc.Y,Loc.X].Obj = 255) or (MapElem[Land[Loc.Y,Loc.X].Obj].CanBeRemoved)) //Only certain objects are excluded
    and not HousesNearTile
    and not TileIsCornField(Loc) //Can't build houses on fields
    and not TileIsWineField(Loc)
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and TileInMapCoords(Loc.X, Loc.Y, 1)
    and CheckHeightPass(Loc, hpBuilding) then
      AddPassability(CanBuild);

    if TileIsRoadable(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and CheckHeightPass(Loc, hpWalking) then
      AddPassability(CanMakeRoads);

    if TileIsWater(Loc) then
      AddPassability(CanFish);

    if TileIsSand(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
    //TileLock checked in outer begin/end
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and not TileIsCornField(Loc)
    and not TileIsWineField(Loc)
    and CheckHeightPass(Loc, hpWalking) then //Can't crab on houses, fields and roads (can walk on fenced house so you can't kill them by placing a house on top of them)
      AddPassability(CanCrab);

    if TileIsSoil(Loc.X,Loc.Y)
    and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
    //TileLock checked in outer begin/end
    //Wolf are big enough to run over roads, right?
    and not TileIsCornField(Loc)
    and not TileIsWineField(Loc)
    and CheckHeightPass(Loc, hpWalking) then
      AddPassability(CanWolf);
  end;

  if TileIsWalkable(Loc)
  and not MapElem[Land[Loc.Y,Loc.X].Obj].AllBlocked
  and CheckHeightPass(Loc, hpWalking)
  and (Land[Loc.Y,Loc.X].TileLock <> tlHouse) then
    AddPassability(CanWorker);

  //Check all 4 tiles that border with this vertex
  if TileIsFactorable(KMPoint(Loc.X  ,Loc.Y))
  and TileIsFactorable(KMPoint(Loc.X-1,Loc.Y))
  and TileIsFactorable(KMPoint(Loc.X  ,Loc.Y-1))
  and TileIsFactorable(KMPoint(Loc.X-1,Loc.Y-1)) then
    AddPassability(canFactor);

  //Check for houses around this vertice(!)
  //Use only with CanElevate since it's vertice-based!
  HousesNearVertex := False;
  for i := -1 to 0 do
  for k := -1 to 0 do
    if TileInMapCoords(Loc.X+k, Loc.Y+i) then
    //Can't elevate built houses, can elevate fenced and dug houses though
    if (Land[Loc.Y+i,Loc.X+k].TileLock = tlHouse) then
      HousesNearVertex := True;

  if VerticeInMapCoords(Loc.X,Loc.Y)
  and not HousesNearVertex then
    AddPassability(CanElevate);
end;


function TKMTerrain.CheckPassability(Loc:TKMPoint; aPass:TPassability): Boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and (aPass in Land[Loc.Y,Loc.X].Passability);
end;


function TKMTerrain.HasUnit(Loc: TKMPoint): Boolean;
begin
  Assert(TileInMapCoords(Loc.X,Loc.Y));
  Result := Land[Loc.Y,Loc.X].IsUnit <> nil;
end;


function TKMTerrain.HasVertexUnit(Loc:TKMPoint): Boolean;
begin
  Assert(TileInMapCoords(Loc.X,Loc.Y));
  Result := Land[Loc.Y,Loc.X].IsVertexUnit <> vu_None;
end;


//Check which road connect ID the tile has (to which road network does it belongs to)
function TKMTerrain.GetRoadConnectID(Loc: TKMPoint): Byte;
begin
  Result := GetConnectID(wcRoad, Loc);
end;


//Check which walk connect ID the tile has (to which walk network does it belongs to)
function TKMTerrain.GetWalkConnectID(Loc: TKMPoint): Byte;
begin
  Result := GetConnectID(wcWalk, Loc);
end;


function TKMTerrain.GetConnectID(aWalkConnect: TWalkConnect; Loc: TKMPoint): Byte;
begin
  if TileInMapCoords(Loc.X,Loc.Y) then
    Result := Land[Loc.Y,Loc.X].WalkConnect[aWalkConnect]
  else
    Result := 0; //No network
end;


function TKMTerrain.CheckAnimalIsStuck(Loc:TKMPoint; aPass:TPassability; aCheckUnits: Boolean=true): Boolean;
var I,K: integer;
begin
  Result := true; //Assume we are stuck
  for I := -1 to 1 do for K := -1 to 1 do
    if (I <> 0) or (K <> 0) then
      if TileInMapCoords(Loc.X+K,Loc.Y+I) then
        if CanWalkDiagonaly(Loc, Loc.X+K, Loc.Y+I) then
          if (Land[Loc.Y+I,Loc.X+K].IsUnit = nil) or (not aCheckUnits) then
            if aPass in Land[Loc.Y+I,Loc.X+K].Passability then
            begin
              Result := false; //at least one tile is empty, so unit is not stuck
              exit;
            end;
end;


{Return random tile surrounding Loc with aPass property. PusherLoc is the unit that pushed us which is}
{preferable to other units (otherwise we can get two units swapping places forever)}
function TKMTerrain.GetOutOfTheWay(Loc, PusherLoc:TKMPoint; aPass:TPassability):TKMPoint;
var
  I, K: Integer;
  L1, L2, L3: TKMPointList;
  TempUnit: TKMUnit;
begin
  //List 1 holds all available walkable positions except self
  L1 := TKMPointList.Create;
  for I:=-1 to 1 do for K:=-1 to 1 do
    if ((I<>0) or (K<>0))
    and TileInMapCoords(Loc.X+K, Loc.Y+I)
    and CanWalkDiagonaly(Loc, Loc.X + K, Loc.Y + I) //Check for trees that stop us walking on the diagonals!
    and (Land[Loc.Y+I,Loc.X+K].TileLock in [tlNone, tlFenced])
    and (aPass in Land[Loc.Y+I,Loc.X+K].Passability) then
      L1.AddEntry(KMPoint(Loc.X+K, Loc.Y+I));

  //List 2 holds the best positions, ones which are not occupied
  L2 := TKMPointList.Create;
  for I := 0 to L1.Count - 1 do
    if Land[L1[I].Y, L1[I].X].IsUnit = nil then
      L2.AddEntry(L1[I]);

  //List 3 holds the second best positions, ones which are occupied with an idle unit
  L3 := TKMPointList.Create;
  for I := 0 to L1.Count - 1 do
    if Land[L1[I].Y, L1[I].X].IsUnit <> nil then
    begin
      TempUnit := UnitsHitTest(L1[I].X, L1[I].Y);
      //Always include the pushers loc in the possibilities, otherwise we can get two units swapping places forever
      if KMSamePoint(L1[I],PusherLoc)
      or ((TempUnit <> nil) and (TempUnit.GetUnitAction is TUnitActionStay)
          and (not TUnitActionStay(TempUnit.GetUnitAction).Locked)) then
        L3.AddEntry(L1[I]);
    end;

  if not(L2.GetRandom(Result)) then
    if not(L3.GetRandom(Result)) then
      if not(L1.GetRandom(Result)) then
        Result := Loc;

  L1.Free;
  L2.Free;
  L3.Free;
end;


function TKMTerrain.FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: boolean=false): Boolean;
var
  I, K: Integer;
  L1, L2: TKMPointList;
begin
  //List 1 holds all positions next to both Loc and Loc2
  L1 := TKMPointList.Create;
  for I := -1 to 1 do
  for K := -1 to 1 do
    if ((I <> 0) or (K <> 0))
    and TileInMapCoords(Loc.X+K,Loc.Y+I)
    and not KMSamePoint(KMPoint(Loc.X+K,Loc.Y+I), Loc2)
    and (aPass in Land[Loc.Y+I,Loc.X+K].Passability)
    and CanWalkDiagonaly(Loc, Loc.X+K, Loc.Y+I) //Check for trees that stop us walking on the diagonals!
    and (Land[Loc.Y+I,Loc.X+K].TileLock in [tlNone, tlFenced])
    and (KMLengthDiag(KMPoint(Loc.X+K,Loc.Y+I),Loc2) <= 1) //Right next to Loc2 (not diagonal)
    and not HasUnit(KMPoint(Loc.X+K,Loc.Y+I)) then //Doesn't have a unit
      L1.AddEntry(KMPoint(Loc.X+K,Loc.Y+I));

  //List 2 holds the best positions, ones which are also next to Loc3 (next position)
  L2 := TKMPointList.Create;
  if not KMSamePoint(Loc3, KMPoint(0,0)) then //No Loc3 was given
  for I := 0 to L1.Count - 1 do
    if KMLengthDiag(L1[I], Loc3) < 1.5 then //Next to Loc3 (diagonal is ok)
      L2.AddEntry(L1[I]);

  Result := True;
  if not(L2.GetRandom(SidePoint)) then
  if (OnlyTakeBest) or (not(L1.GetRandom(SidePoint))) then
    Result := False; //No side step positions available

  L1.Free;
  L2.Free;
end;


//Test wherever it is possible to make the route without actually making it to save performance
function TKMTerrain.Route_CanBeMade(LocA, LocB: TKMPoint; aPass: TPassability; aDistance: Single): Boolean;
var i,k:integer; TestRadius: Boolean; WC: TWalkConnect;
begin
  Result := True;

  //target has to be different point than source
  //Result:=not (KMSamePoint(LocA,LocB)); //Or maybe we don't care

  //Source point has to be walkable
  Result := Result and CheckPassability(LocA, aPass);

  //Target has to be walkable within Distance
  TestRadius := False;
  for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
  for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
  if KMLength(LocB,KMPoint(k,i)) <= aDistance then
    TestRadius := TestRadius or CheckPassability(KMPoint(k,i),aPass);
  Result := Result and TestRadius;

  case aPass of
    CanWalk:      WC := wcWalk;
    CanWalkRoad:  WC := wcRoad;
    CanFish:      WC := wcFish;
    CanWorker:    WC := wcWork;
    else Exit;
  end;

  {if WC = wcWork then
  with TBitmap.Create do
  begin
    Width := fMapX;
    Height:= fMapY;
    PixelFormat := pf32bit;
    for I := 0 to Height-1 do
      for K := 0 to Width-1 do
        Canvas.Pixels[K,I] := Land[I+1,K+1].WalkConnect[wcWork] * 32;
    SaveToFile(ExeDir + 'wcWork.bmp');
    Free;
  end;}

  //Walkable way between A and B is proved by FloodFill
  TestRadius := False;
  for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
  for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
  if KMLength(LocB,KMPoint(k,i)) <= aDistance then
    TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[WC] = Land[i,k].WalkConnect[WC]);
  Result := Result and TestRadius;
end;


//Check if a route can be made to this vertex, from any direction (used for woodcutter cutting trees)
function TKMTerrain.Route_CanBeMadeToVertex(LocA, LocB: TKMPoint; aPass: TPassability): Boolean;
var i,k:integer;
begin
  Result := false;
  //Check from top-left of vertex to vertex tile itself
  for i := Max(LocB.Y-1,1) to LocB.Y do
    for k := Max(LocB.X-1,1) to LocB.X do
      Result := Result or Route_CanBeMade(LocA,KMPoint(k,i),aPass,0);
end;


//Returns the closest tile to TargetLoc with aPass and walk connect to OriginLoc
//If no tile found - return Origin location
function TKMTerrain.GetClosestTile(TargetLoc, OriginLoc: TKMPoint; aPass: TPassability; aAcceptTargetLoc: Boolean):TKMPoint;
const TestDepth = 255;
var
  i:integer;
  P:TKMPointI;
  T:TKMPoint;
  WalkConnectID: integer;
  wcType: TWalkConnect;
begin
  case aPass of
    CanWalkRoad: wcType := wcRoad;
    CanFish:     wcType := wcFish;
    else         wcType := wcWalk; //CanWalk is default
  end;

  WalkConnectID := Land[OriginLoc.Y,OriginLoc.X].WalkConnect[wcType]; //Store WalkConnect ID of origin

  //If target is accessable then use it
  if aAcceptTargetLoc and CheckPassability(TargetLoc, aPass) and (WalkConnectID = Land[TargetLoc.Y,TargetLoc.X].WalkConnect[wcType]) then
  begin
    Result := TargetLoc;
    exit;
  end;

  //If target is not accessable then choose a tile near to the target that is accessable
  //As we Cannot reach our destination we are "low priority" so do not choose a tile with another unit on it (don't bump important units)
  for i:=0 to TestDepth do begin
    P := GetPositionFromIndex(TargetLoc, i);
    if not TileInMapCoords(P.X,P.Y) then continue;
    T := KMPoint(P.X,P.Y);
    if CheckPassability(T, aPass)
      and (WalkConnectID = Land[T.Y,T.X].WalkConnect[wcType])
      and (not HasUnit(T) or KMSamePoint(T,OriginLoc)) //Allow position we are currently on, but not ones with other units
    then
    begin
      Result := T; //Assign if all test are passed
      exit;
    end;
  end;

  Result := OriginLoc; //If we don't find one, return existing Loc
end;


{Mark tile as occupied}
procedure TKMTerrain.UnitAdd(LocTo:TKMPoint; aUnit: Pointer);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Land[LocTo.Y,LocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(LocTo));
  Land[LocTo.Y,LocTo.X].IsUnit := aUnit
end;


{ Mark tile as empty }
// We have no way of knowing whether a unit is inside a house, or several units exit a house at once
// when exiting the game and destroying all units this will cause asserts.
procedure TKMTerrain.UnitRem(LocFrom:TKMPoint);
begin
  if not DO_UNIT_INTERACTION then exit;
  Land[LocFrom.Y,LocFrom.X].IsUnit := nil;
end;


{Mark previous tile as empty and next one as occupied}
//We need to check both tiles since UnitWalk is called only by WalkTo where both tiles aren't houses
procedure TKMTerrain.UnitWalk(LocFrom,LocTo:TKMPoint; aUnit: Pointer);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Land[LocFrom.Y, LocFrom.X].IsUnit = aUnit, 'Trying to remove wrong unit at '+TypeToString(LocFrom));
  Land[LocFrom.Y, LocFrom.X].IsUnit := nil;
  Assert(Land[LocTo.Y, LocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(LocTo));
  Land[LocTo.Y, LocTo.X].IsUnit := aUnit
end;


procedure TKMTerrain.UnitSwap(LocFrom,LocTo:TKMPoint; UnitFrom:Pointer);
begin
  Assert(Land[LocFrom.Y,LocFrom.X].IsUnit = UnitFrom, 'Trying to swap wrong unit at '+TypeToString(LocFrom));
  Land[LocFrom.Y,LocFrom.X].IsUnit := Land[LocTo.Y,LocTo.X].IsUnit;
  Land[LocTo.Y,LocTo.X].IsUnit := UnitFrom;
end;


{Mark vertex as occupied}
procedure TKMTerrain.UnitVertexAdd(LocTo:TKMPoint; Usage: TKMVertexUsage);
begin
  if not DO_UNIT_INTERACTION then exit;
  assert(Usage <> vu_None, 'Invalid add vu_None at '+TypeToString(LocTo));
  assert((Land[LocTo.Y,LocTo.X].IsVertexUnit = vu_None) or (Land[LocTo.Y,LocTo.X].IsVertexUnit = Usage),'Opposite vertex in use at '+TypeToString(LocTo));

  Land[LocTo.Y,LocTo.X].IsVertexUnit := Usage;
end;


procedure TKMTerrain.UnitVertexAdd(LocFrom, LocTo:TKMPoint);
begin
  assert(KMStepIsDiag(LocFrom, LocTo), 'Add non-diagonal vertex?');
  UnitVertexAdd(KMGetDiagVertex(LocFrom, LocTo), GetVertexUsageType(LocFrom, LocTo));
end;


{Mark vertex as empty}
procedure TKMTerrain.UnitVertexRem(LocFrom:TKMPoint);
begin
  if not DO_UNIT_INTERACTION then exit;
  Land[LocFrom.Y,LocFrom.X].IsVertexUnit := vu_None;
end;


//This function tells whether the diagonal is "in use". (a bit like IsUnit) So if there is a unit walking on
//the oppsoite diagonal you cannot use the vertex (same diagonal is allowed for passing and fighting)
//It stops units walking diagonally through each other or walking through a diagonal that has weapons swinging through it
function TKMTerrain.VertexUsageCompatible(LocFrom, LocTo:TKMPoint): Boolean;
var
  Vert: TKMPoint;
  VertUsage: TKMVertexUsage;
begin
  Assert(KMStepIsDiag(LocFrom, LocTo));
  Vert := KMGetDiagVertex(LocFrom, LocTo);
  VertUsage := GetVertexUsageType(LocFrom, LocTo);
  Result := (Land[Vert.Y, Vert.X].IsVertexUnit in [vu_None, VertUsage]);
end;


function TKMTerrain.GetVertexUsageType(LocFrom, LocTo:TKMPoint): TKMVertexUsage;
var dx, dy: integer;
begin
  dx := LocFrom.X - LocTo.X;
  dy := LocFrom.Y - LocTo.Y;
  Assert((abs(dx) = 1) and (abs(dy) = 1));
  if (dx*dy = 1) then Result := vu_NWSE
                 else Result := vu_NESW;
end;


//Interpolate between 12 vertices surrounding this tile (X and Y, no diagonals)
//Also it is FlattenTerrain duty to preserve walkability if there are units standing
procedure TKMTerrain.FlattenTerrain(Loc: TKMPoint; aUpdateWalkConnects: Boolean = True);
var TilesFactored: Integer;

  //If tiles with units standing on them become unwalkable we should try to fix them
  procedure EnsureWalkable(aX,aY: Word);
  begin
    //We did not recalculated passability yet, hence tile has CanWalk but CheckHeightPass=False already
    if (CanWalk in Land[aY,aX].Passability)
    //Yield in TestStone is much better if we comment this out, also general result is flatter/"friendlier"
    //and (Land[aY,aX].IsUnit <> nil)
    and not CheckHeightPass(KMPoint(aX,aY), hpWalking)
    and not fMapEditor //Allow units to become "stuck" in MapEd, as height changing is allowed anywhere
    then
      //This recursive call should be garanteed to exit, as eventually the terrain will be flat enough
      FlattenTerrain(KMPoint(aX,aY), False); //WalkConnect should be done at the end
  end;

  function GetHeight(aX,aY: Word; Neighbour: Boolean): Byte;
  begin
    if TileInMapCoords(aX,aY) and (not Neighbour or (canFactor in Land[aY,aX].Passability)) then
    begin
      Result := Land[aY,aX].Height;
      Inc(TilesFactored);
    end
    else
      Result := 0;
  end;

var
  I, K: Word;
  Avg: Word;
begin
  Assert(TileInMapCoords(Loc.X, Loc.Y), 'Can''t flatten tile outside map coordinates');

  if aUpdateWalkConnects then
    fBoundsWC := KMRect(Loc.X, Loc.Y, Loc.X, Loc.Y);

  //Expand fBoundsWC in case we were called by EnsureWalkable, and fBoundsWC won't know about this tile
  if fBoundsWC.Left > Loc.X - 1 then fBoundsWC.Left := Loc.X - 1;
  if fBoundsWC.Top > Loc.Y - 1 then fBoundsWC.Top := Loc.Y - 1;
  if fBoundsWC.Right < Loc.X + 1 then fBoundsWC.Right := Loc.X + 1;
  if fBoundsWC.Bottom < Loc.Y + 1 then fBoundsWC.Bottom := Loc.Y + 1;

  TilesFactored := 0; //GetHeight will add to this
  Avg :=                                   GetHeight(Loc.X,Loc.Y-1,True ) + GetHeight(Loc.X+1,Loc.Y-1,True ) +
         GetHeight(Loc.X-1,Loc.Y  ,True) + GetHeight(Loc.X,Loc.Y  ,False) + GetHeight(Loc.X+1,Loc.Y  ,False) + GetHeight(Loc.X+2,Loc.Y  ,True) +
         GetHeight(Loc.X-1,Loc.Y+1,True) + GetHeight(Loc.X,Loc.Y+1,False) + GetHeight(Loc.X+1,Loc.Y+1,False) + GetHeight(Loc.X+2,Loc.Y+1,True) +
                                           GetHeight(Loc.X,Loc.Y+2,True ) + GetHeight(Loc.X+1,Loc.Y+2,True );
  Assert(TilesFactored <> 0); //Non-neighbour tiles will always be factored
  Avg := Round(Avg / TilesFactored);

  if CanElevate in Land[Loc.Y  ,Loc.X  ].Passability then Land[Loc.Y  ,Loc.X  ].Height := Mix(Avg, Land[Loc.Y  ,Loc.X  ].Height, 0.5);
  if CanElevate in Land[Loc.Y  ,Loc.X+1].Passability then Land[Loc.Y  ,Loc.X+1].Height := Mix(Avg, Land[Loc.Y  ,Loc.X+1].Height, 0.5);
  if CanElevate in Land[Loc.Y+1,Loc.X  ].Passability then Land[Loc.Y+1,Loc.X  ].Height := Mix(Avg, Land[Loc.Y+1,Loc.X  ].Height, 0.5);
  if CanElevate in Land[Loc.Y+1,Loc.X+1].Passability then Land[Loc.Y+1,Loc.X+1].Height := Mix(Avg, Land[Loc.Y+1,Loc.X+1].Height, 0.5);

  //All 9 tiles around and including this one could have become unwalkable and made a unit stuck, so check them all
  for I := Max(Loc.Y-1, 1) to Min(Loc.Y+1, fMapY-1) do
    for K := Max(Loc.X-1, 1) to Min(Loc.X+1, fMapX-1) do
      EnsureWalkable(K, I);

  UpdateLighting(KMRect(Loc.X-2, Loc.Y-2, Loc.X+3, Loc.Y+3));
  //Changing height will affect the cells around this one
  UpdatePassability(KMRectGrow(KMRect(Loc), 1));

  if aUpdateWalkConnects then
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], fBoundsWC, False);
end;


//Flatten a list of points on mission init
procedure TKMTerrain.FlattenTerrain(LocList: TKMPointList);
var
  I: Integer;
begin
  //Flatten terrain will extend fBoundsWC as necessary, which cannot be predicted due to EnsureWalkable effecting a larger area
  if not LocList.GetBounds(fBoundsWC) then
    Exit;

  for I := 0 to LocList.Count - 1 do
    FlattenTerrain(LocList[I], False); //Rebuild the Walk Connect at the end, rather than every time

  //wcFish not affected by height
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], fBoundsWC, False);
end;


//Rebuilds lighting values for given bounds.
//These values are used to draw highlights/shadows on terrain
//Note that input values may be off-map
procedure TKMTerrain.UpdateLighting(aRect: TKMRect);
var
  I, K: Integer;
  x0, y2: Integer;
begin
  //Valid vertices are within 1..Map
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY) do
  for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX) do
  begin
    x0 := Max(K-1, 1);
    y2 := Min(I+1, fMapY);
    Land[I,K].Light := EnsureRange((Land[I,K].Height-(Land[y2,K].Height+Land[I,x0].Height)/2)/22,-1,1); //  1.33*16 ~=22

    //Use more contrast lighting for Waterbeds
    if fTileset.TileIsWater(Land[I, K].Terrain) then
      Land[I,K].Light := EnsureRange(Land[I,K].Light * 1.3 + 0.1, -1, 1);

    //Map borders always fade to black
    if (I = 1) or (I = fMapY) or (K = 1) or (K = fMapX) then
      Land[I,K].Light := -1;
  end;
end;


//Rebuilds passability for given bounds
procedure TKMTerrain.UpdatePassability(aRect: TKMRect);
var I, K: Integer;
begin
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY - 1) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX - 1) do
      UpdatePassability(KMPoint(K, I));
end;


//Rebuilds connected areas using flood fill algorithm
procedure TKMTerrain.UpdateWalkConnect(const aSet: array of TWalkConnect; aRect: TKMRect; aDiagObjectsEffected:Boolean);
  procedure FloodFill(aWC: TWalkConnect; aPass: TPassability; aAllowDiag: Boolean);
  var
    AreaID: Byte;
    Count: Integer;

    procedure FillArea(X,Y: Word);
    begin
      if (Land[Y,X].WalkConnect[aWC] = 0) //Untested area
      and (aPass in Land[Y,X].Passability) then //Matches passability
      begin
        Land[Y,X].WalkConnect[aWC] := AreaID;
        Inc(Count);
        //Using custom TileInMapCoords replacement gives ~40% speed improvement
        //Using custom CanWalkDiagonally is also much faster
        if X-1 >= 1 then
        begin
          if aAllowDiag and (Y-1 >= 1) and not MapElem[Land[Y,X].Obj].DiagonalBlocked then
            FillArea(X-1, Y-1);
          FillArea(X-1, Y);
          if aAllowDiag and (Y+1 <= fMapY) and not MapElem[Land[Y+1,X].Obj].DiagonalBlocked then
            FillArea(X-1,Y+1);
        end;

        if Y-1 >= 1 then     FillArea(X, Y-1);
        if Y+1 <= fMapY then FillArea(X, Y+1);

        if X+1 <= fMapX then
        begin
          if aAllowDiag and (Y-1 >= 1) and not MapElem[Land[Y,X+1].Obj].DiagonalBlocked then
            FillArea(X+1, Y-1);
          FillArea(X+1, Y);
          if aAllowDiag and (Y+1 <= fMapY) and not MapElem[Land[Y+1,X+1].Obj].DiagonalBlocked then
            FillArea(X+1, Y+1);
        end;
      end;
    end;
  //const MinSize = 1; //Minimum size that is treated as new area
  var I,K: Integer;
  begin
    //Reset everything
    for I := 1 to fMapY do for K := 1 to fMapX do
      Land[I,K].WalkConnect[aWC] := 0;

    AreaID := 0;
    for I := 1 to fMapY do for K := 1 to fMapX do
    if (Land[I,K].WalkConnect[aWC] = 0)
    and (aPass in Land[I,K].Passability) then
    begin
      Inc(AreaID);
      Count := 0;
      FillArea(K,I);

      if Count <= 1 then //Revert
      begin
        Dec(AreaID);
        Count := 0;
        Land[I,K].WalkConnect[aWC] := 0;
      end;

      Assert(AreaID < 255, 'UpdateWalkConnect failed due too many unconnected areas');
    end;
  end;

  function CheckCanSkip(aWorkRect:TKMRect; aWC:TWalkConnect; aPass:TPassability):Boolean;
  var I,K: Integer; AllPass, AllFail: Boolean;
  begin
    //If objects were effected we must reprocess because a tree could block the connection
    //between two areas. Also skip this check if the area is too large because it takes too long
    if (KMRectArea(aWorkRect) > 100) then
    begin
      Result := False;
      Exit;
    end;
    Result := True;
    AllPass := True;
    AllFail := True;
    for I := aWorkRect.Top to aWorkRect.Bottom do
      for K := aWorkRect.Left to aWorkRect.Right do
      begin
        if aDiagObjectsEffected then
        begin
          AllPass := AllPass and ((Land[I,K].WalkConnect[aWC] <> 0) and (aPass in Land[I,K].Passability));
          AllFail := AllFail and ((Land[I,K].WalkConnect[aWC] = 0) and not (aPass in Land[I,K].Passability));
          //If all tiles that changed are walkable or not walkable currently and in our last UpdateWalkConnect, it's safe to skip
          Result := AllPass or AllFail;
        end else begin
          Result := Result and
                    //First case: Last time we did WalkConnect the tile WASN'T walkable,
                    //and Passability confirms this has not changed (tile still not walkable)
                   (((Land[I,K].WalkConnect[aWC] = 0) and not (aPass in Land[I,K].Passability)) or
                    //Second case: Last time we did WalkConnect the tile WAS walkable,
                    //and Passability confirms this has not changed (tile still walkable)
                    ((Land[I,K].WalkConnect[aWC] <> 0) and (aPass in Land[I,K].Passability)));
        end;
        if not Result then Exit; //If one tile has changed, we need to do the whole thing
      end;
  end;

const
  WCSet: array [TWalkConnect] of TPassability = (
    CanWalk, CanWalkRoad, CanFish, CanWorker);
var
  J: Integer;
  WC: TWalkConnect;
  AllowDiag: Boolean;
  Pass: TPassability;
  WorkRect: TKMRect;
begin
  WorkRect := KMClipRect(aRect, 1, 1, fMapX-1, fMapY-1);

  //Process all items from set
  for J := Low(aSet) to High(aSet) do
  begin
    WC := aSet[J];
    Pass := WCSet[WC];
    AllowDiag := (WC <> wcRoad); //Do not consider diagonals "connected" for roads

    if not CheckCanSkip(WorkRect, WC, Pass) then
      if USE_CCL_WALKCONNECT then
        CCLFind(WC, Pass, AllowDiag)
      else
        FloodFill(WC, Pass, AllowDiag);
  end;
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TKMTerrain.SetHouse(Loc: TKMPoint; aHouseType: THouseType; aHouseStage: THouseStage; aOwner: TPlayerIndex; const aFlattenTerrain: Boolean = False);
var
  I, K, X, Y: Word;
  ToFlatten: TKMPointList;
  HA: THouseArea;
  ObjectsEffected: Boolean; //UpdateWalkConnect cares about this for optimisation purposes
begin
  ObjectsEffected := False;
  if aFlattenTerrain then //We will check aFlattenTerrain only once, otherwise there are compiler warnings
    ToFlatten := TKMPointList.Create
  else
    ToFlatten := nil;

  if aHouseStage = hsNone then
    SetHouseAreaOwner(Loc, aHouseType, -1)
  else
    SetHouseAreaOwner(Loc, aHouseType, aOwner);

  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do
  for k:=1 to 4 do
    if HA[i,k] <> 0 then
    begin
      x := Loc.X + k - 3;
      y := Loc.Y + i - 4;
      if TileInMapCoords(x,y) then
      begin
        case aHouseStage of
          hsNone:         Land[y,x].TileLock := tlNone;
          hsFence:        Land[y,x].TileLock := tlFenced; //Initial state, Laborer should assign NoWalk to each tile he digs
          hsBuilt:        begin
                            //Script houses are placed as built, add TileLock for them too
                            Land[y,x].TileLock := tlHouse;

                            //Add road for scipted houses
                            if HA[i,k] = 2 then
                              Land[y,x].TileOverlay := to_Road;

                            if ToFlatten <> nil then
                            begin
                              //In map editor don't remove objects (remove on mission load instead)
                              if Land[y,x].Obj <> 255 then
                              begin
                                ObjectsEffected := ObjectsEffected or MapElem[Land[y,x].Obj].DiagonalBlocked;
                                Land[y,x].Obj := 255;
                              end;
                              //If house was set e.g. in mission file we must flatten the terrain as no one else has
                              ToFlatten.AddEntry(KMPoint(x,y));
                            end;
                          end;
        end;
        UpdateFences(KMPoint(x,y));
      end;
    end;

  if ToFlatten <> nil then
  begin
    FlattenTerrain(ToFlatten);
    ToFlatten.Free;
  end;

  //Recalculate Passability for tiles around the house so that they can't be built on too
  UpdatePassability(KMRect(Loc.X - 3, Loc.Y - 4, Loc.X + 2, Loc.Y + 1));
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRect(Loc.X - 3, Loc.Y - 4, Loc.X + 2, Loc.Y + 1), ObjectsEffected);
end;


{That is mainly used for minimap now}
procedure TKMTerrain.SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);
var i,k:integer; HA:THouseArea;
begin
  HA := fResource.HouseDat[aHouseType].BuildArea;
  case aHouseType of
    ht_None:    Land[Loc.Y,Loc.X].TileOwner := aOwner;
    ht_Any:     ; //Do nothing
    else        for i:=1 to 4 do for k:=1 to 4 do //If this is a house make change for whole place
                  if HA[i,k]<>0 then
                    if TileInMapCoords(Loc.X+k-3,Loc.Y+i-4) then
                      Land[Loc.Y+i-4,Loc.X+k-3].TileOwner := aOwner;
  end;
end;


{Check if Unit can be placed here}
//Used by MapEd, so we use AllowedTerrain which lets us place citizens off-road
function TKMTerrain.CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType): Boolean;
begin
  Result := TileInMapCoords(Loc.X, Loc.Y)
            and (Land[Loc.Y, Loc.X].IsUnit = nil) //Check for no unit below
            and (fResource.UnitDat[aUnitType].AllowedPassability in Land[Loc.Y, Loc.X].Passability);
end;


function TKMTerrain.CanPlaceGoldmine(X,Y: Word): Boolean;
  function HousesNearTile: Boolean;
  var I,K: Integer;
  begin
    Result := False;
    for I := -1 to 1 do
    for K := -1 to 1 do
      if (Land[Y+I,X+K].TileLock in [tlFenced,tlDigged,tlHouse]) then
        Result := True;
  end;
begin
  Result := TileGoodForGoldmine(X,Y)
    and ((Land[Y,X].Obj = 255) or (MapElem[Land[Y,X].Obj].CanBeRemoved))
    and not HousesNearTile
    and (Land[Y,X].TileLock = tlNone)
    and CheckHeightPass(KMPoint(X,Y), hpBuildingMines);
end;


{Check if house can be placed in that place}
function TKMTerrain.CanPlaceHouse(Loc: TKMPoint; aHouseType: THouseType): Boolean;
var I,K,X,Y: Integer; HA: THouseArea;
begin
  Result := True;
  HA := fResource.HouseDat[aHouseType].BuildArea;
  Loc.X := Loc.X - fResource.HouseDat[aHouseType].EntranceOffsetX; //update offset
  for I := 1 to 4 do
  for K := 1 to 4 do
    if Result and (HA[I,K] <> 0) then
    begin
      X := Loc.X + k - 3;
      Y := Loc.Y + i - 4;
      //Inset one tile from map edges
      Result := Result and TileInMapCoords(X, Y, 1);

      case aHouseType of
        ht_IronMine: Result := Result and TileGoodForIron(X, Y);
        ht_GoldMine: Result := Result and CanPlaceGoldmine(X, Y);
        else         Result := Result and (CanBuild in Land[Y,X].Passability);
      end;
    end;
end;


//Simple checks when placing houses from the script:
function TKMTerrain.CanPlaceHouseFromScript(aHouseType: THouseType; Loc:TKMPoint): Boolean;
var
  I, K, L, M: Integer;
  HA: THouseArea;
  TX, TY: Integer;
begin
  Result := True;
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for I := 1 to 4 do
  for K := 1 to 4 do
  if (HA[I,K] <> 0) then
  begin
    TX := Loc.X + K - 3;
    TY := Loc.Y + I - 4;
    Result := Result and TileInMapCoords(TX, TY, 1); //Inset one tile from map edges
    Result := Result and TileIsWalkable(KMPoint(TX, TY)); //Tile must be walkable

    //Mines must be on a mountain edge
    if aHouseType = ht_IronMine then
      Result := Result and (Land[TY,TX].Terrain in [109, 166..170]) and (Land[TY,TX].Rotation mod 4 = 0);
    if aHouseType = ht_GoldMine then
      Result := Result and (Land[TY,TX].Terrain in [171..175     ]) and (Land[TY,TX].Rotation mod 4 = 0);

    //Check surrounding tiles for another house that overlaps
    for L := -1 to 1 do
    for M := -1 to 1 do
    if TileInMapCoords(TX+M, TY+L) and (Land[TY+L, TX+M].TileLock <> tlNone) then
      Result := False;

    //Check if there are units below placed BEFORE the house is added
    //Units added AFTER the house will be autoplaced around it
    Result := Result and (Land[TY, TX].IsUnit = nil);

    if not Result then Exit;
  end;
end;


function TKMTerrain.CanAddField(aX, aY: Word; aFieldType: TFieldType): Boolean;
begin
  //Make sure it is within map, roads can be built on edge
  Result := TileInMapCoords(aX, aY);
  case aFieldType of
    ft_Road:  Result := Result and (CanMakeRoads  in Land[aY, aX].Passability);
    ft_Corn,
    ft_Wine:  Result := Result and TileGoodForField(aX, aY);
    ft_Wall:  Result := Result and (CanMakeRoads  in Land[aY, aX].Passability);
    else      Result := False;
  end;
end;


function TKMTerrain.CheckHeightPass(aLoc:TKMPoint; aPass:THeightPass): Boolean;
  function TestHeight(aHeight: Byte): Boolean;
  var Points: array[1..4] of byte;
  begin
    //Put points into an array like this so it's easy to understand:
    // 1 2
    // 3 4
    //Local map boundaries test is faster
    Points[1] := Land[aLoc.Y,                 aLoc.X].Height;
    Points[2] := Land[aLoc.Y,                 Min(aLoc.X+1, fMapX-1)].Height;
    Points[3] := Land[Min(aLoc.Y+1, fMapY-1), aLoc.X].Height;
    Points[4] := Land[Min(aLoc.Y+1, fMapY-1), Min(aLoc.X+1, fMapX-1)].Height;

    {KaM method checks the differences between the 4 verticies around the tile.
    There is a special case that means it is more (twice) as tolerant to bottom-left to top right (2-3) and
    bottom-right to top-right (4-2) slopes. This sounds very odd, but if you don't believe me then do the tests yourself. ;)
    The reason for this probably has something to do with the fact that shaddows and stuff flow from
    the bottom-left to the top-right in KaM.
    This formula could be revised later, but for now it matches KaM perfectly.
    The biggest problem with it is backwards sloping tiles which are shown as walkable.
    But it doesn't matter that much because this system is really just a backup (it's more important for
    building than walking) and map creators should block tiles themselves with the special invisible block object.}

    //Sides of tile
    Result :=            (abs(Points[1]-Points[2]) < aHeight);
    Result := Result AND (abs(Points[3]-Points[4]) < aHeight);
    Result := Result AND (abs(Points[3]-Points[1]) < aHeight);
    Result := Result AND (abs(Points[4]-Points[2]) < aHeight*2); //Bottom-right to top-right is twice as tolerant

    //Diagonals of tile
    Result := Result AND (abs(Points[1]-Points[4]) < aHeight);
    Result := Result AND (abs(Points[3]-Points[2]) < aHeight*2); //Bottom-left to top-right is twice as tolerant
  end;
begin
  //Three types measured in KaM: >=25 - unwalkable/unroadable; >=25 - iron/gold mines unbuildable;
  //>=18 - other houses unbuildable.
  Result := true;
  if not TileInMapCoords(aLoc.X,aLoc.Y) then exit;
  case aPass of
    hpWalking: Result := TestHeight(25);
    hpBuilding: Result := TestHeight(18);
    hpBuildingMines: Result := TestHeight(25);
  end;
end;


procedure TKMTerrain.AddHouseRemainder(Loc: TKMPoint; aHouseType: THouseType; aBuildState: THouseBuildState);
var I, K: Integer; HA: THouseArea;
begin
  HA := fResource.HouseDat[aHouseType].BuildArea;

  if aBuildState in [hbs_Stone, hbs_Done] then //only leave rubble if the construction was well underway (stone and above)
  begin
    //Leave rubble
    for I:=2 to 4 do for K:=2 to 4 do
      if (HA[I-1,K] <> 0) and (HA[I,K-1] <> 0)
      and (HA[I-1,K-1] <> 0) and (HA[I,K] <> 0) then
        Land[Loc.Y+I-4,Loc.X+K-3].Obj := 68 + KaMRandom(6);
    //Leave dug terrain
    for I:=1 to 4 do for K:=1 to 4 do
      if HA[I,K] <> 0 then
      begin
        Land[Loc.Y+I-4, Loc.X+K-3].TileOverlay := to_Dig3;
        Land[Loc.Y+I-4, Loc.X+K-3].TileLock := tlNone;
      end;
  end
  else
  begin
    //For glyphs leave nothing
    for I:=1 to 4 do for K:=1 to 4 do
      if HA[I,K] <> 0 then
        Land[Loc.Y+I-4, Loc.X+K-3].TileLock := tlNone;
  end;

  UpdatePassability(KMRect(Loc.X - 3, Loc.Y - 4, Loc.X + 2, Loc.Y + 1));
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRect(Loc.X - 3, Loc.Y - 4, Loc.X + 2, Loc.Y + 1), (aBuildState in [hbs_Stone, hbs_Done])); //Rubble objects block diagonals
end;


{Check 4 surrounding tiles, and if they are different place a fence}
procedure TKMTerrain.UpdateFences(Loc: TKMPoint; CheckSurrounding: Boolean = True);
  function GetFenceType: TFenceType;
  begin
    if TileIsCornField(Loc) then
      Result := fncCorn
    else
    if TileIsWineField(Loc) then
      Result := fncWine
    else
    if Land[Loc.Y,Loc.X].TileLock in [tlFenced, tlDigged] then
      Result := fncHouseFence
    else
      Result := fncNone;
  end;
  function GetFenceEnabled(X, Y: SmallInt): Boolean;
  begin
    Result := True;
    if not TileInMapCoords(X,Y) then exit;
    if (TileIsCornField(Loc) and TileIsCornField(KMPoint(X,Y)))or //Both are Corn
       (TileIsWineField(Loc) and TileIsWineField(KMPoint(X,Y)))or //Both are Wine
      ((Land[Loc.Y,Loc.X].TileLock in [tlFenced, tlDigged]) and (Land[Y,X].TileLock in [tlFenced, tlDigged])) then //Both are either house fence
      Result := False;
  end;
begin
 if not TileInMapCoords(Loc.X, Loc.Y) then exit;

  Land[Loc.Y,Loc.X].Fence := GetFenceType;
  if Land[Loc.Y,Loc.X].Fence = fncNone then
    Land[Loc.Y,Loc.X].FenceSide := 0
  else
  begin
    Land[Loc.Y,Loc.X].FenceSide := Byte(GetFenceEnabled(Loc.X,Loc.Y-1)) + //N
                                   Byte(GetFenceEnabled(Loc.X-1,Loc.Y)) * 2 + //E
                                   Byte(GetFenceEnabled(Loc.X+1,Loc.Y)) * 4 + //W
                                   Byte(GetFenceEnabled(Loc.X,Loc.Y+1)) * 8; //S
  end;

  if CheckSurrounding then
  begin
    UpdateFences(KMPoint(Loc.X-1, Loc.Y), False);
    UpdateFences(KMPoint(Loc.X+1, Loc.Y), False);
    UpdateFences(KMPoint(Loc.X, Loc.Y-1), False);
    UpdateFences(KMPoint(Loc.X, Loc.Y+1), False);
  end;
end;


{Cursor position should be converted to tile-coords respecting tile heights}
function TKMTerrain.ConvertCursorToMapCoord(inX,inY:single):single;
var ii:integer; Xc,Yc:integer; Tmp:integer; Ycoef:array[-2..4]of single;
begin
  Xc := EnsureRange(round(inX+0.5),1,fMapX-1); //Cell below cursor without height check
  Yc := EnsureRange(round(inY+0.5),1,fMapY-1);

  for ii:=-2 to 4 do //make an array of tile heights above and below cursor (-2..4)
  begin
    Tmp := EnsureRange(Yc+ii,1,fMapY);
    Ycoef[ii] := (Yc-1)+ii-(Land[Tmp,Xc].Height*(1-frac(inX))
                           +Land[Tmp,Xc+1].Height*frac(inX))/CELL_HEIGHT_DIV;
  end;

  Result := Yc; //Assign something incase following code returns nothing

  for ii:=-2 to 3 do //check if cursor in a tile and adjust it there
    if InRange(inY, Ycoef[ii], Ycoef[ii+1]) then
    begin
      Result := Yc+ii-(Ycoef[ii+1]-inY) / (Ycoef[ii+1]-Ycoef[ii]);
      break;
    end;

  //fLog.AssertToLog(false,'TTerrain.ConvertCursorToMapCoord - couldn''t convert')
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.FlatToHeight(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  Tmp1, Tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-2);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-2);

  Tmp1 := Mix(Land[Yc+1, Xc+2].Height, Land[Yc+1, Xc+1].Height, Frac(inX));
  Tmp2 := Mix(Land[Yc+2, Xc+2].Height, Land[Yc+2, Xc+1].Height, Frac(inX));
  Result := inY - Mix(Tmp2, Tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.FlatToHeight(aPoint: TKMPointF): TKMPointF;
begin
  Result.X := aPoint.X;
  Result.Y := FlatToHeight(aPoint.X, aPoint.Y);
end;


//Return height within cell interpolating node heights
//Note that input parameters are 0 based
function TKMTerrain.HeightAt(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  Tmp1, Tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-2);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-2);

  Tmp1 := Mix(Land[Yc+1, Xc+2].Height, Land[Yc+1, Xc+1].Height, Frac(inX));
  Tmp2 := Mix(Land[Yc+2, Xc+2].Height, Land[Yc+2, Xc+1].Height, Frac(inX));
  Result := Mix(Tmp2, Tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


//Get highest walkable hill on a maps top row to use for viewport top bound
function TKMTerrain.TopHill: Byte;
var
  I,K: Integer;
begin
  Result := 0;
  //Check last 2 strips in case 2nd has a taller hill
  for I := 1 to 2 do
  for K := 2 to fMapX do
  if ((canWalk in Land[I, K-1].Passability) or (canWalk in Land[I, K].Passability)) then
    Result := Max(Result, Land[I, K].Height - (I-1) * CELL_SIZE_PX);
end;


procedure TKMTerrain.IncAnimStep;
begin
  Inc(fAnimStep);
end;


procedure TKMTerrain.Save(SaveStream: TKMemoryStream);
var I,K: Integer;
begin
  Assert(not fMapEditor, 'MapEd mode is not intended to be saved into savegame');

  SaveStream.Write('Terrain');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fAnimStep);

  FallingTrees.SaveToStream(SaveStream);

  for I:=1 to fMapY do for K:=1 to fMapX do
  begin
    //Only save fields that cannot be recalculated after loading
    SaveStream.Write(Land[I,K].Terrain);
    SaveStream.Write(Land[I,K].Height);
    SaveStream.Write(Land[I,K].Rotation);
    SaveStream.Write(Land[I,K].Obj);
    SaveStream.Write(Land[I,K].TreeAge);
    SaveStream.Write(Land[I,K].FieldAge);
    SaveStream.Write(Land[I,K].TileLock, SizeOf(Land[I,K].TileLock));
    SaveStream.Write(Land[I,K].TileOverlay, SizeOf(Land[I,K].TileOverlay));
    SaveStream.Write(Land[I,K].TileOwner, SizeOf(Land[I,K].TileOwner));
    if Land[I,K].IsUnit <> nil then
      SaveStream.Write(TKMUnit(Land[I,K].IsUnit).ID) //Store ID, then substitute it with reference on SyncLoad
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(Land[I,K].IsVertexUnit, SizeOf(Land[I,K].IsVertexUnit));
  end;
end;


procedure TKMTerrain.Load(LoadStream: TKMemoryStream);
var i,k:integer;
begin
  LoadStream.ReadAssert('Terrain');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fAnimStep);

  FallingTrees.LoadFromStream(LoadStream);

  for i:=1 to fMapY do for k:=1 to fMapX do
  begin
    LoadStream.Read(Land[i,k].Terrain);
    LoadStream.Read(Land[i,k].Height);
    LoadStream.Read(Land[i,k].Rotation);
    LoadStream.Read(Land[i,k].Obj);
    LoadStream.Read(Land[i,k].TreeAge);
    LoadStream.Read(Land[i,k].FieldAge);
    LoadStream.Read(Land[i,k].TileLock,SizeOf(Land[i,k].TileLock));
    LoadStream.Read(Land[i,k].TileOverlay,SizeOf(Land[i,k].TileOverlay));
    LoadStream.Read(Land[i,k].TileOwner,SizeOf(Land[i,k].TileOwner));
    LoadStream.Read(Land[i,k].IsUnit, 4);
    LoadStream.Read(Land[i,k].IsVertexUnit,SizeOf(Land[i,k].IsVertexUnit));
  end;

  for i:=1 to fMapY do for k:=1 to fMapX do
    UpdateFences(KMPoint(k,i), False);

  fFinder := TKMTerrainFinder.Create;

  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWork], KMRect(1, 1, fMapX, fMapY), True);

  fLog.AddTime('Terrain loaded');
end;


procedure TKMTerrain.SyncLoad;
var
  I, K: Integer;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      Land[I,K].IsUnit := fPlayers.GetUnitByID(Cardinal(Land[I,K].IsUnit));
end;


{ This whole thing is very CPU intesive, think of it - to update whole (192*192) tiles map }
//Don't use any advanced math here, only simpliest operations - + div *
procedure TKMTerrain.UpdateState;
  procedure SetLand(X, Y, aTile, aObj: Byte);
  var FloodfillNeeded: Boolean;
  begin
    Land[Y,X].Terrain := aTile;
    FloodfillNeeded   := MapElem[Land[Y,X].Obj].DiagonalBlocked <> MapElem[aObj].DiagonalBlocked;
    Land[Y,X].Obj     := aObj;
    if FloodfillNeeded then //When trees are removed by corn growing we need to update floodfill
      UpdateWalkConnect([wcWalk,wcRoad,wcWork], KMRectGrowTopLeft(KMRect(X,Y,X,Y)), True);
  end;
var
  H, I, K, A: Word;
  J: TChopableAge;
  T: Integer;
begin
  if not DYNAMIC_TERRAIN then Exit;

  inc(fAnimStep);

  //Update falling trees animation
  for T := FallingTrees.Count - 1 downto 0 do
  if fAnimStep >= FallingTrees.Tag2[T] + Cardinal(MapElem[FallingTrees.Tag[T]].Anim.Count - 1) then
    ChopTree(FallingTrees[T]); //Make the tree turn into a stump

  //Process odd then even rows each tick to save time (less loops)
  for A := 1 to (fMapY div 2) do
  begin
    I := 2*A - (fAnimStep mod 2); //1..fMapY
    for K := 1 to fMapX do
    begin
      //All those global things can be performed once a sec, or even less frequent
      if ((I*fMapX div 2)+K+(fAnimStep div 2)) mod (TERRAIN_PACE div 2) = 0 then
      begin

        if InRange(Land[I,K].FieldAge, 1, CORN_AGE_MAX-1) then
        begin
          Inc(Land[I,K].FieldAge);
          if TileIsCornField(KMPoint(K,I)) then
            case Land[I,K].FieldAge of
              CORN_AGE_1:     SetLand(K,I,59,255);
              CORN_AGE_2:     SetLand(K,I,60,255);
              CORN_AGE_3:     SetLand(K,I,60,58);
              CORN_AGE_FULL:  begin
                                //Skip to the end
                                SetLand(K,I,60,59);
                                Land[I,K].FieldAge := CORN_AGE_MAX;
                              end;
            end
          else
          if TileIsWineField(KMPoint(K,I)) then
            case Land[I,K].FieldAge of
              WINE_AGE_1:     SetLand(K,I,55,55);
              WINE_AGE_2:     SetLand(K,I,55,56);
              WINE_AGE_FULL:  begin
                                //Skip to the end
                                SetLand(K,I,55,57);
                                Land[I,K].FieldAge := CORN_AGE_MAX;
                              end;
            end;
        end;

        if InRange(Land[I,K].TreeAge, 1, TREE_AGE_FULL) then
        begin
          Inc(Land[I,K].TreeAge);
          if (Land[I,K].TreeAge = TREE_AGE_1)
          or (Land[I,K].TreeAge = TREE_AGE_2)
          or (Land[I,K].TreeAge = TREE_AGE_FULL) then //Speedup
            for H := Low(ChopableTrees) to High(ChopableTrees) do
              for J := caAge1 to caAge3 do
                if Land[I,K].Obj = ChopableTrees[H,J] then
                  case Land[I,K].TreeAge of
                    TREE_AGE_1:    Land[I,K].Obj := ChopableTrees[H, caAge2];
                    TREE_AGE_2:    Land[I,K].Obj := ChopableTrees[H, caAge3];
                    TREE_AGE_FULL: Land[I,K].Obj := ChopableTrees[H, caAgeFull];
                  end;
        end;
      end;
    end;
  end;
end;


procedure TKMTerrain.CCLFind(aWC: TWalkConnect; aPass: TPassability; aAllowDiag: Boolean);
var
  Parent: array [0..512] of Word;

  function TopParent(const Area: Word): Word;
  begin
    Result := Area;
    while Parent[Result] <> Result do
      Result := Parent[Result];
  end;

  procedure AddAlias(const Area1, Area2: Word);
  begin
    //See if there are common parents
    if Area2 <> Area1 then
      Parent[Area2] := Area1;
  end;
const Samples: array [0..3, 0..1] of ShortInt = ((-1,-1),(0,-1),(1,-1),(-1,0));
var
  I,K,H: Word;
  X,Y: Smallint;
  AreaID: Word;
  NCount: Byte;
begin
  //Reset everything
  for I := 1 to fMapY do for K := 1 to fMapX do
    Land[I,K].WalkConnect[aWC] := 0;

  FillChar(Parent, SizeOf(Parent), #0);

  AreaID := 1;
  for I := 1 to fMapY do
  for K := 1 to fMapX do
  if (aPass in Land[I,K].Passability) then
  begin

    //Check 4 preceeding neighbors, if there is ID we will take it
    NCount := 0;
    for H := 0 to 3 do
    begin
      X := K + Samples[H,0];
      Y := I + Samples[H,1];

      if (Y >= 1) and InRange(X, 1, fMapX) and (aPass in Land[Y,X].Passability) then
      if (H = 1) or (H = 3) or (aAllowDiag and (
                                 ((H = 0) and not MapElem[Land[I,K].Obj].DiagonalBlocked) or
                                 ((H = 2) and not MapElem[Land[I,K+1].Obj].DiagonalBlocked)))
      then
      begin
        if (NCount = 0) then
          Land[I,K].WalkConnect[aWC] := Land[Y,X].WalkConnect[aWC]
        else
          //Remember alias
          if (Parent[Land[Y,X].WalkConnect[aWC]] <> Parent[Land[I,K].WalkConnect[aWC]]) then
            AddAlias(TopParent(Land[Y,X].WalkConnect[aWC]), TopParent(Land[I,K].WalkConnect[aWC]));

        Inc(NCount);
      end;
    end;

    //If there's no Area we create new one
    if NCount = 0 then
    begin
      Land[I,K].WalkConnect[aWC] := AreaID;
      Parent[AreaID] := AreaID;
      Inc(AreaID);
      Assert(AreaID < 32767, 'UpdateWalkConnect failed due too many unconnected areas');
    end;
  end;

  //1 -> 2    1 -> 2
  //2 -> 2    2 -> 2
  //3 -> 4    3 -> 5
  //4 -> 5    4 -> 5
  //5 -> 5    5 -> 5
  //Merge parents
  for I := 1 to AreaID - 1 do
    while Parent[I] <> Parent[Parent[I]] do
      Parent[I] := Parent[Parent[I]];

  //Merge areas
  for I := 1 to fMapY do
  for K := 1 to fMapX do
  if (Land[I,K].WalkConnect[aWC] <> 0) then
    Land[I,K].WalkConnect[aWC] := Parent[Land[I,K].WalkConnect[aWC]];
end;


end.
