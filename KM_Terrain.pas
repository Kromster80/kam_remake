unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Utils;


type
  //Farmers/Woodcutters preferred activity
  TPlantAct = (taCut, taPlant, taAny);


  {Class to store all terrain data, aswell terrain routines}
  TTerrain = class
  private
    fAnimStep: Cardinal;
    fMapEditor: Boolean; //In MapEd mode some features behave differently
    fMapX: Word; //Terrain width and height
    fMapY: Word; //Terrain width and height

    procedure UpdateBorders(Loc: TKMPoint; CheckSurrounding: Boolean = True);
    procedure UpdateLighting(aRect: TKMRect);
    procedure UpdatePassability(aRect: TKMRect); overload;
    procedure UpdatePassability(Loc: TKMPoint); overload;
    procedure UpdatePassabilityAround(Loc: TKMPoint);
    procedure UpdateWalkConnect(const aSet: array of TWalkConnect);
  public
    Land: array[1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE]of record
      Terrain:byte;
      Height:byte;
      Rotation:byte;
      Obj:byte;

      //Age of tree, another independent variable since trees can grow on fields
      TreeAge:word; //Not init=0 .. Full=TreeAgeFull Depending on this tree gets older and thus could be chopped

      //Age of field/wine, another independent variable
      FieldAge:word; //Empty=0, 1, 2, 3, 4, Full=65535  Depending on this special object maybe rendered (straw, grapes)

      //Tells us the stage of house construction or workers making a road
      TileLock: TTileLock;

      //Used to display half-dug road
      TileOverlay:TTileOverlay; //fs_None fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4 +Roads

      TileOwner:TPlayerIndex; //Who owns the tile by having a house/road/field on it
      IsUnit: Pointer; //Whenever there's a unit on that tile mark the tile as occupied and count the number
      IsVertexUnit:TKMVertexUsage; //Whether there are units blocking the vertex. (walking diagonally or fighting)


      //MAPEDITOR
      OldTerrain, OldRotation:byte; //Only used for map editor
      HeightAdd:byte; //Fraction part of height, for smooth height editing


      //DEDUCTED
      Light:single; //KaM stores node lighting in 0..32 range (-16..16), but I want to use -1..1 range
      Passability:TPassabilitySet; //Meant to be set of allowed actions on the tile

      WalkConnect: array [TWalkConnect] of byte; //Whole map is painted into interconnected areas

      Border: TBorderType; //Borders (ropes, planks, stones)
      BorderTop, BorderLeft, BorderBottom, BorderRight:boolean; //Whether the borders are enabled
    end;

    FallingTrees: TKMPointTagList;

    constructor Create;
    destructor Destroy; override;
    procedure MakeNewMap(Width, Height: Integer; aMapEditor: Boolean);
    procedure LoadFromFile(FileName: string; aMapEditor: Boolean);
    procedure SaveToFile(aFile:string);

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;

    procedure SetTileLock(aLoc: TKMPoint; aTileLock: TTileLock);
    procedure UnlockTile(Loc:TKMPoint);
    procedure SetRoads(aList: TKMPointList; aOwner: TPlayerIndex);
    procedure SetField(Loc: TKMPoint; aOwner: TPlayerIndex; aFieldType: TFieldType);
    procedure SetHouse(Loc: TKMPoint; aHouseType: THouseType; aHouseStage: THouseStage; aOwner: TPlayerIndex; const aFlattenTerrain: Boolean = False);
    procedure SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);

    procedure RemovePlayer(aPlayer:TPlayerIndex);
    procedure RemRoad(Loc:TKMPoint);
    procedure RemField(Loc:TKMPoint);
    procedure SetWall(Loc:TKMPoint; aOwner:TPlayerIndex);
    procedure IncDigState(Loc:TKMPoint);
    procedure ResetDigState(Loc:TKMPoint);

    function CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType):boolean;
    function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType):boolean;
    function CanPlaceHouseFromScript(aHouseType: THouseType; Loc:TKMPoint):boolean;
    function CanAddField(Loc: TKMPoint; aFieldType: TFieldType): Boolean;
    function CheckHeightPass(aLoc:TKMPoint; aPass:TPassability):boolean;
    procedure AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);

    function FindWineField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir):Boolean;
    function FindCornField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; aPlantAct:TPlantAct; out PlantAct:TPlantAct; out FieldPoint:TKMPointDir):Boolean;
    function FindStone(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out StonePoint: TKMPointDir):Boolean;
    function FindOre(aLoc:TKMPoint; Rt:TResourceType; out OrePoint: TKMPoint):Boolean;
    function FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aPlantAct: TPlantAct; out Tree: TKMPointDir; out PlantAct:TPlantAct): Boolean;
    function FindFishWater(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FishPoint: TKMPointDir):Boolean;
    function CanFindFishingWater(aLoc:TKMPoint; aRadius:integer):boolean;
    function ChooseTreeToPlant(aLoc:TKMPoint):integer;
    procedure GetHouseMarks(aLoc:TKMPoint; aHouseType:THouseType; aList:TKMPointTagList);

    function WaterHasFish(aLoc:TKMPoint):boolean;
    function CatchFish(aLoc:TKMPointDir; TestOnly:boolean=false):boolean;

    procedure SetTree(Loc:TKMPoint; ID:integer);
    procedure FallTree(Loc:TKMPoint);
    procedure ChopTree(Loc:TKMPoint);

    procedure SowCorn(Loc:TKMPoint);
    procedure CutCorn(Loc:TKMPoint);
    procedure CutGrapes(Loc:TKMPoint);

    procedure SetResourceDeposit(Loc:TKMPoint; rt:TResourceType);
    procedure DecStoneDeposit(Loc:TKMPoint);
    function DecOreDeposit(Loc:TKMPoint; rt:TResourceType):boolean;

    function CheckPassability(Loc:TKMPoint; aPass:TPassability):boolean;
    function HasUnit(Loc:TKMPoint):boolean;
    function HasVertexUnit(Loc:TKMPoint):boolean;
    function GetRoadConnectID(Loc:TKMPoint):byte;
    function GetWalkConnectID(Loc:TKMPoint):byte;
    function GetConnectID(aWalkConnect: TWalkConnect; Loc:TKMPoint):byte;

    function CheckAnimalIsStuck(Loc:TKMPoint; aPass:TPassability; aCheckUnits:boolean=true):boolean;
    function GetOutOfTheWay(Loc, PusherLoc:TKMPoint; aPass:TPassability):TKMPoint;
    function FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: boolean=false):Boolean;
    function Route_CanBeMade(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single):boolean;
    function Route_CanBeMadeToVertex(LocA, LocB:TKMPoint; aPass:TPassability):boolean;
    function GetClosestTile(TargetLoc, OriginLoc: TKMPoint; aPass: TPassability; aAcceptTargetLoc: Boolean):TKMPoint;

    procedure UnitAdd(LocTo:TKMPoint; aUnit: Pointer);
    procedure UnitRem(LocFrom:TKMPoint);
    procedure UnitWalk(LocFrom,LocTo:TKMPoint; aUnit: Pointer);
    procedure UnitSwap(LocFrom,LocTo:TKMPoint; UnitFrom: Pointer);
    procedure UnitVertexAdd(LocTo:TKMPoint; Usage: TKMVertexUsage); overload;
    procedure UnitVertexAdd(LocFrom, LocTo:TKMPoint); overload;
    procedure UnitVertexRem(LocFrom:TKMPoint);
    function VertexUsageCompatible(LocFrom, LocTo:TKMPoint): Boolean;
    function GetVertexUsageType(LocFrom, LocTo:TKMPoint): TKMVertexUsage;

    function TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
    function VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
    function EnsureTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;

    function TileIsWater(Loc:TKMPoint):boolean;
    function TileHasWater(Loc:TKMPoint):boolean;
    function TileIsSand(Loc:TKMPoint):boolean;
    function TileIsStone(X,Y:Word):byte;
    function TileIsSoil(Loc:TKMPoint):boolean;
    function TileIsWalkable(Loc:TKMPoint):boolean;
    function TileIsRoadable(Loc:TKMPoint):boolean;
    function TileIsCornField(Loc:TKMPoint):boolean;
    function TileIsWineField(Loc:TKMPoint):boolean;
    function TileIsFactorable(Loc:TKMPoint):boolean;
    function TileIsLocked(aLoc:TKMPoint):boolean;
    function UnitsHitTest(X,Y:word): Pointer;
    function UnitsHitTestF(aLoc: TKMPointF): Pointer;
    function UnitsHitTestWithinRad(aLoc:TKMPoint; MinRad, MaxRad:single; aPlayer:TPlayerIndex; aAlliance:TAllianceType; Dir:TKMDirection): Pointer;

    function ObjectIsChopableTree(Loc:TKMPoint; Stage:byte):boolean;
    function CanWalkDiagonaly(A,B:TKMPoint):boolean;

    procedure FlattenTerrain(Loc:TKMPoint; aUpdateWalkConnects:boolean=true); overload;
    procedure FlattenTerrain(LocList:TKMPointList); overload;

    function GetVertexCursorPosition: TKMPoint;
    function ConvertCursorToMapCoord(inX,inY:single): Single;
    function HeightAt(inX, inY: Single): Single; overload;
    function HeightAt(aPoint: TKMPointF): Single; overload;

    procedure MapEdHeight;
    procedure MapEdTile(aLoc:TKMPoint; aTile,aRotation:byte);

    procedure IncAnimStep; //Lite-weight UpdateState for MapEd
    property AnimStep: Cardinal read fAnimStep;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;

    procedure UpdateState;
    procedure UpdateStateIdle;
  end;


var
  //Terrain is a globally accessible resource by so many objects
  //In rare cases local terrain is used (e.g. main menu minimap)
  fTerrain: TTerrain;


implementation
uses KM_Log, KM_PlayersCollection,
  KM_Resource, KM_Units, KM_ResourceHouse, KM_Sound, KM_UnitActionStay, KM_Units_Warrior;


{ TTerrain }
constructor TTerrain.Create;
begin
  inherited;
  fAnimStep := 0;
  FallingTrees := TKMPointTagList.Create;
end;


destructor TTerrain.Destroy;
begin
  FreeAndNil(FallingTrees);
  inherited;
end;


//Reset whole map with default values
procedure TTerrain.MakeNewMap(Width, Height: Integer; aMapEditor: Boolean);
var i,k:integer;
begin
  fMapEditor := aMapEditor;
  fMapX := Min(Width, MAX_MAP_SIZE);
  fMapY := Min(Height,MAX_MAP_SIZE);

  for i:=1 to fMapY do for k:=1 to fMapX do with Land[i,k] do begin
    Terrain      := 0;
    Height       := KaMRandom(7);    //variation in height
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
    TreeAge      := 0;
    if ObjectIsChopableTree(KMPoint(k,i),4) then TreeAge := TREE_AGE_FULL;
    Border       := bt_None;
    BorderTop    := false;
    BorderLeft   := false;
    BorderBottom := false;
    BorderRight  := false;
  end;

  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWolf, wcCrab, wcWork]);
end;


procedure TTerrain.LoadFromFile(FileName: string; aMapEditor: Boolean);
var
  i,k:integer;
  S:TKMemoryStream;
  NewX,NewY:integer;
begin
  if not CheckFileExists(FileName) then Exit;

  fMapEditor := aMapEditor;

  fLog.AppendLog('Loading map file: ' + FileName);

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX <= MAX_MAP_SIZE) and (NewY <= MAX_MAP_SIZE), 'Can''t open the map cos it has too big dimensions');
    fMapX := NewX;
    fMapY := NewY;
    MakeNewMap(fMapX, fMapY, aMapEditor); //Reset whole map to default
    for i:=1 to fMapY do for k:=1 to fMapX do
    begin
      S.Read(Land[i,k].Terrain); //1
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Height); //3
      S.Read(Land[i,k].Rotation); //4
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Obj); //6
      S.Seek(17, soFromCurrent);
      if ObjectIsChopableTree(KMPoint(k,i),1) then Land[i,k].TreeAge := 1;
      if ObjectIsChopableTree(KMPoint(k,i),2) then Land[i,k].TreeAge := TREE_AGE_1;
      if ObjectIsChopableTree(KMPoint(k,i),3) then Land[i,k].TreeAge := TREE_AGE_2;
      if ObjectIsChopableTree(KMPoint(k,i),4) then Land[i,k].TreeAge := TREE_AGE_FULL;
      //Everything else is default
    end;
  finally
    S.Free;
  end;

  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWolf, wcCrab, wcWork]);
  fLog.AppendLog('Map file loaded');
end;


//Save (export) map in KaM .map format with additional tile information on the end?
procedure TTerrain.SaveToFile(aFile: string);
var f:file; i,k:integer; c0,cF:cardinal; light,b205:byte; SizeX,SizeY:Integer;
    ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
    Res:array[1..MAX_MAP_SIZE*2]of packed record X1,Y1,X2,Y2:integer; Typ:byte; end;
begin

  ForceDirectories(ExtractFilePath(aFile));

  assignfile(f,aFile); rewrite(f,1);

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

  for i:=1 to ResHead.Allocated do begin
    Res[i].X1:=-842150451; Res[i].Y1:=-842150451;
    Res[i].X2:=-842150451; Res[i].Y2:=-842150451;
    Res[i].Typ:=255;
  end;

  blockwrite(f,ResHead,22);
  for i:=1 to ResHead.Allocated do blockwrite(f,Res[i],17);

  {blockwrite(f,'ADDN',4);
  blockwrite(f,'TILE',4); //Chunk name
  i := 4 + fMapY*fMapX;
  blockwrite(f,i,4); //Chunk size
  i := 0; //contained lock in older version
  blockwrite(f,i,4);
  for i:=1 to Map.Y do for k:=1 to Map.X do blockwrite(f,Land2[i,k].TPoint,1);}

  closefile(f);
end;


{Check if requested tile (X,Y) is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,fMapX-1-Inset) and InRange(Y,1+Inset,fMapY-1-Inset);
end;


{Check if requested vertice is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,fMapX-Inset) and InRange(Y,1+Inset,fMapY-Inset);
end;


{Ensure that requested tile is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.EnsureTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;
begin
  Result.X := EnsureRange(X,1+Inset,fMapX-1-Inset);
  Result.Y := EnsureRange(Y,1+Inset,fMapY-1-Inset);
end;


{Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed}
function TTerrain.TileIsWater(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [48,114,115,119,192,193,194,196, 200, 208..211, 235,236, 240,244];
end;


{Check if requested tile has any water, including ground-water transitions}
function TTerrain.TileHasWater(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := (Land[Loc.Y,Loc.X].Terrain in [4,10,12,22,23,44,48,105..107,114..127,142,143,192..194,196,198..200,208..211,230,232..244]);
end;


{Check if requested tile is sand suitable for crabs}
function TTerrain.TileIsSand(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [31..33, 70,71, 99,100,102,103, 108,109, 112,113, 116,117, 169, 173, 181, 189];
end;


{Check if requested tile is Stone and returns Stone deposit}
function TTerrain.TileIsStone(X,Y:Word):byte;
begin
  //Should be Tileset property, especially if we allow different tilesets
  case Land[Y, X].Terrain of
    132,137: Result := 5;
    131,136: Result := 4;
    130,135: Result := 3;
    129,134: Result := 2;
    128,133: Result := 1;
    else     Result := 0;
  end;
end;


{Check if requested tile is soil suitable for fields and trees}
function TTerrain.TileIsSoil(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..28, 34..39, 47, 49, 55..58, 64..69, 72..80, 84..87, 88,89, 93..98,180,182..183,188,190..191,220,247];
end;


{Check if requested tile is generally walkable}
function TTerrain.TileIsWalkable(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  //Include 1/2 and 3/4 walkable as walkable
  //Result := Land[Loc.Y,Loc.X].Terrain in [0..6, 8..11,13,14, 16..22, 25..31, 32..39, 44..47, 49,52,55, 56..63,
  //                                        64..71, 72..79, 80..87, 88..95, 96..103, 104,106..109,111, 112,113,116,117, 123..125,
  //                                        138..139, 152..155, 166,167, 168..175, 180..183, 188..191,
  //                                        197, 203..205,207, 212..215, 220..223, 242,243,247];
  //+1 converts terrain type from 0..255 to 1..256
  //Values can be 1 or 2, What 2 does is unknown
  Result := PatternDAT[Land[Loc.Y,Loc.X].Terrain+1].Walkable <> 0;
end;


{Check if requested tile is generally suitable for road building}
function TTerrain.TileIsRoadable(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  //Do not include 1/2 and 1/4 walkable as roadable
  //Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..31, 32..39, 45..47, 49, 52, 55, 56..63,
  //                                        64..71, 72..79, 80..87, 88..95, 96..103, 104,108,111, 112,113,
  //                                        152..155,180..183,188..191,
  //                                        203..205, 212,213,215, 220, 247];
  Result := PatternDAT[Land[Loc.Y,Loc.X].Terrain+1].Buildable <> 0;
end;


{Check if the tile is a corn field}
function TTerrain.TileIsCornField(Loc: TKMPoint):boolean;
begin
  Result := (Land[Loc.Y,Loc.X].Terrain in [59..63])
            and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road); //Can't be if there is road here
end;


{Check if the tile is a wine field}
function TTerrain.TileIsWineField(Loc: TKMPoint):boolean;
begin
  Result := (Land[Loc.Y,Loc.X].Terrain in [55])
            and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road) //Can't be if there is road here
            and (Land[Loc.Y,Loc.X].Obj in [54..57]); //Must have object (e.g. for init when labourer is building it)
end;


function TTerrain.TileIsFactorable(Loc: TKMPoint): Boolean;
begin
  //List of tiles that cannot be factored (coordinates outside the map return true)
  Result := not TileInMapCoords(Loc.X,Loc.Y) or
            not (Land[Loc.Y,Loc.X].Terrain in [7,15,24,50,53,144..151,156..165,198,199,202,206]);
end;


function TTerrain.TileIsLocked(aLoc: TKMPoint): Boolean;
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
function TTerrain.UnitsHitTest(X,Y: Word): Pointer;
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
function TTerrain.UnitsHitTestF(aLoc: TKMPointF): Pointer;
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
      T := GetLength(U.PositionF, aLoc);
      if (T <= 1) and ((Result = nil) or (T < GetLength(TKMUnit(Result).PositionF, aLoc))) then
        Result := U;
    end;
  end;
end;


//Function to use with WatchTowers/Archers/AutoLinking/
{ Should scan withing given radius and return closest unit with given Alliance status
  Should be optimized versus usual UnitsHitTest
  Prefer Warriors over Citizens}
function TTerrain.UnitsHitTestWithinRad(aLoc: TKMPoint; MinRad, MaxRad: Single; aPlayer: TPlayerIndex; aAlliance: TAllianceType; Dir: TKMDirection): Pointer;
var
  i,k: integer; //Counters
  lx,ly,hx,hy: integer; //Ranges
  dX,dY: integer;
  RequiredMaxRad: single;
  U,C,W: TKMUnit; //CurrentUnit, BestWarrior, BestCitizen
  P: TKMPoint;
begin
  W := nil;
  C := nil;

  //Scan one tile further than the maximum radius due to rounding
  lx := max(round(aLoc.X-(MaxRad+1)),1); //1.42 gets rounded to 1
  ly := max(round(aLoc.Y-(MaxRad+1)),1); //1.42 gets rounded to 1
  hx := min(round(aLoc.X+(MaxRad+1)),fMapX); //1.42 gets rounded to 1
  hy := min(round(aLoc.Y+(MaxRad+1)),fMapY); //1.42 gets rounded to 1

  //In KaM archers can shoot further than sight radius (shoot further into explored areas)
  //so CheckTileRevelation is required, we can't remove it to optimise.
  for i:=ly to hy do for k:=lx to hx do
  if (Land[i,k].IsUnit <> nil)
  and (fPlayers.Player[aPlayer].FogOfWar.CheckTileRevelation(k,i,false) = 255) then
  begin
    //Check archer sector. If it's not within the 90 degree sector for this direction, then don't use this tile (continue)
    dX := k-aLoc.X;
    dY := i-aLoc.Y;
    case Dir of
      dir_N : if not((abs(dX)<=-dY)and(dY<0)) then continue;
      dir_NE: if not((dX>0)        and(dY<0)) then continue;
      dir_E:  if not((dX>0)and (abs(dY)<=dX)) then continue;
      dir_SE: if not((dX>0)        and(dY>0)) then continue;
      dir_S : if not((abs(dX)<=dY) and(dY>0)) then continue;
      dir_SW: if not((dX<0)        and(dY>0)) then continue;
      dir_W:  if not((dX<0)and(abs(dY)<=-dX)) then continue;
      dir_NW: if not((dX<0)        and(dY<0)) then continue;
    end;

    //Don't check tiles farther than closest Warrior
    if (W<>nil) and (GetLength(KMPoint(aLoc.X,aLoc.Y),KMPoint(k,i)) >= GetLength(KMPoint(aLoc.X,aLoc.Y),W.GetPosition)) then
      Continue; //Since we check left-to-right we can't exit just yet (there are possible better enemies below)

    U := Land[i,k].IsUnit;

    if (U = nil)
    or U.IsDeadOrDying
    or not U.Visible then //Inside of house
      Continue;

    //This unit could be on a different tile next to KMPoint(k,i), so we cannot use that anymore.
    //There was a crash caused by VertexUsageCompatible checking (k,i) instead of U.GetPosition.
    //In that case aLoc = (37,54) and k,i = (39;52) but U.GetPosition = (38;53).
    //This shows why you can't use (k,i) in checks because it is distance >2 from aLoc! (in melee fight)
    P := U.GetPosition;

    RequiredMaxRad := MaxRad;
    if (MaxRad = 1) and KMStepIsDiag(aLoc, P) then
      RequiredMaxRad := 1.42; //Use diagonal radius sqrt(2) instead

    if CanWalkDiagonaly(aLoc, P) and
       (fPlayers.CheckAlliance(aPlayer, U.GetOwner) = aAlliance) and //How do WE feel about enemy, not how they feel about us
       ((abs(aLoc.X - P.X) <> 1) or (abs(aLoc.Y - P.Y) <> 1) or VertexUsageCompatible(aLoc,P)) and
       (InRange(GetLength(KMPointF(aLoc), U.PositionF), MinRad, RequiredMaxRad)) //Unit's exact position must be close enough
    then
      if U is TKMUnitWarrior then
        W := U
      else
        C := U;
  end;

  if W <> nil then
    Result := W
  else
    Result := C;
end;


function TTerrain.ObjectIsChopableTree(Loc:TKMPoint; Stage:byte):boolean;
var h,i:byte;
begin
  //If Stage is not in 1..6 then assume they mean any type of tree
  Result:=false;
  for h:=1 to length(ChopableTrees) do
    if Stage in [1..6] then
      Result := Result or (Land[Loc.Y,Loc.X].Obj = ChopableTrees[h,Stage])
    else for i:=1 to 6 do
      Result := Result or (Land[Loc.Y,Loc.X].Obj = ChopableTrees[h,i])
end;


{Check wherever unit can walk from A to B diagonaly}
{Return true if direction is either walkable or not diagonal}
{Maybe this can also be used later for inter-tile passability}
function TTerrain.CanWalkDiagonaly(A,B:TKMPoint):boolean;
begin
  Result := true;

  if (abs(A.X-B.X)<>1) or (abs(A.Y-B.Y)<>1) then exit; //Tiles are not diagonal to each other

                                                                 //Relative tiles locations
  if (A.X<B.X)and(A.Y<B.Y) then                                  //   A
    Result := not MapElem[Land[B.Y,B.X].Obj+1].DiagonalBlocked   //     B
  else
  if (A.X<B.X)and(A.Y>B.Y) then                                  //     B
    Result := not MapElem[Land[B.Y+1,B.X].Obj+1].DiagonalBlocked //   A
  else
  if (A.X>B.X)and(A.Y>B.Y) then                                  //   B
    Result := not MapElem[Land[A.Y,A.X].Obj+1].DiagonalBlocked   //     A
  else
  if (A.X>B.X)and(A.Y<B.Y) then                                  //     A
    Result := not MapElem[Land[A.Y+1,A.X].Obj+1].DiagonalBlocked;//   B
end;


{Place lock on tile, any new TileLock replaces old one, thats okay}
procedure TTerrain.SetTileLock(aLoc: TKMPoint; aTileLock: TTileLock);
begin
  Land[aLoc.Y, aLoc.X].TileLock := aTileLock;
  UpdatePassabilityAround(aLoc);

  //TileLocks affect passability so therefore also floodfill
  UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWolf, wcCrab, wcWork]);
end;


{Remove lock from tile}
procedure TTerrain.UnlockTile(Loc: TKMPoint);
begin
  Land[Loc.Y, Loc.X].TileLock := tlNone;
  UpdatePassabilityAround(Loc);

  //TileLocks affect passability so therefore also floodfill
  UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWolf, wcCrab, wcWork]);
end;


procedure TTerrain.SetRoads(aList: TKMPointList; aOwner: TPlayerIndex);
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
    UpdateBorders(aList[I]);
  end;

  HasBounds := aList.GetBounds(Bounds);
  Assert(HasBounds);

  //Grow the bounds by extra tile because some passabilities
  //depend on road nearby (e.g. CanPlantTree)
  UpdatePassability(KMRectGrow(Bounds, 1));

  //Roads don't affect wcWalk or wcFish
  UpdateWalkConnect([wcRoad, wcWolf, wcCrab]);
end;


procedure TTerrain.RemRoad(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOwner := -1;
  Land[Loc.Y,Loc.X].TileOverlay := to_None;
  Land[Loc.Y,Loc.X].FieldAge  := 0;
  UpdateBorders(Loc);
  UpdatePassabilityAround(Loc);

  //Roads don't affect wcWalk or wcFish
  UpdateWalkConnect([wcRoad, wcWolf, wcCrab]);
end;


procedure TTerrain.RemField(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOwner := -1;
  Land[Loc.Y,Loc.X].TileOverlay := to_None;
  Land[Loc.Y,Loc.X].Terrain := Land[Loc.Y,Loc.X].OldTerrain; //Reset terrain
  Land[Loc.Y,Loc.X].Rotation := Land[Loc.Y,Loc.X].OldRotation; //Reset terrain
  if Land[Loc.Y,Loc.X].Obj in [54..59] then
    Land[Loc.Y,Loc.X].Obj := 255; //Remove corn/wine
  Land[Loc.Y,Loc.X].FieldAge := 0;
  UpdateBorders(Loc);
  UpdatePassabilityAround(Loc);

  //Update affected WalkConnect's
  UpdateWalkConnect([wcWalk, wcWolf, wcCrab]);
end;


procedure TTerrain.RemovePlayer(aPlayer:TPlayerIndex);
var i,k:word;
begin
  for i:=1 to fMapY do for k:=1 to fMapX do
    if Land[i,k].TileOwner > aPlayer then
      Land[i,k].TileOwner := pred(Land[i,k].TileOwner)
    else
    if Land[i,k].TileOwner = aPlayer then
      Land[i,k].TileOwner := -1;
end;


procedure TTerrain.SetWall(Loc:TKMPoint; aOwner:TPlayerIndex);
begin
  Land[Loc.Y,Loc.X].TileOwner :=aOwner;
  Land[Loc.Y,Loc.X].TileOverlay := to_Wall;
  Land[Loc.Y,Loc.X].FieldAge := 0;
  UpdateBorders(Loc);
  UpdatePassabilityAround(Loc);
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


{Set field on tile - corn/wine}
procedure TTerrain.SetField(Loc: TKMPoint; aOwner: TPlayerIndex; aFieldType: TFieldType);
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

  UpdateBorders(Loc);
  UpdatePassabilityAround(Loc);
  //Walk and Road because Grapes are blocking diagonal moves
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab]);
end;


procedure TTerrain.IncDigState(Loc:TKMPoint);
begin
  case Land[Loc.Y,Loc.X].TileOverlay of
    to_Dig3: Land[Loc.Y,Loc.X].TileOverlay:=to_Dig4;
    to_Dig2: Land[Loc.Y,Loc.X].TileOverlay:=to_Dig3;
    to_Dig1: Land[Loc.Y,Loc.X].TileOverlay:=to_Dig2;
    else     Land[Loc.Y,Loc.X].TileOverlay:=to_Dig1;
  end;
end;


procedure TTerrain.ResetDigState(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOverlay:=to_None;
end;


{ Finds a winefield ready to be picked }
function TTerrain.FindWineField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir):Boolean;
var i,k:integer; List:TKMPointDirList;
begin
  List := TKMPointDirList.Create;
  for i:=max(aLoc.Y-aRadius,1) to min(aLoc.Y+aRadius,fMapY-1) do
  for k:=max(aLoc.X-aRadius,1) to min(aLoc.X+aRadius,fMapX-1) do
    if (KMLength(aLoc,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if TileIsWineField(KMPoint(k,i)) then
        if Land[i,k].FieldAge=65535 then
          if not TileIsLocked(KMPoint(k,i)) then //Taken by another farmer
            if Route_CanBeMade(aLoc,KMPoint(k,i),CanWalk,0) then
              List.AddItem(KMPointDir(k, i, dir_NA));

  Result := List.GetRandom(FieldPoint);
  List.Free;
end;


{ Finds a corn field }
function TTerrain.FindCornField(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; aPlantAct:TPlantAct; out PlantAct:TPlantAct; out FieldPoint:TKMPointDir):Boolean;
var i,k:integer; List:TKMPointDirList;
begin
  List := TKMPointDirList.Create;
  for i:=max(aLoc.Y-aRadius,1) to min(aLoc.Y+aRadius,fMapY-1) do
  for k:=max(aLoc.X-aRadius,1) to min(aLoc.X+aRadius,fMapX-1) do
    if (KMLength(aLoc,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if TileIsCornField(KMPoint(k,i)) then
        if((aPlantAct in [taAny, taPlant]) and (Land[i,k].FieldAge = 0)) or
          ((aPlantAct in [taAny, taCut]) and (Land[i,k].FieldAge = 65535)) then
          if not TileIsLocked(KMPoint(k,i)) then //Taken by another farmer
            if Route_CanBeMade(aLoc,KMPoint(k,i),CanWalk,0) then
              List.AddItem(KMPointDir(k, i, dir_NA));

  Result := List.GetRandom(FieldPoint);
  List.Free;
  if not Result then
    PlantAct := taAny
  else
    if Land[FieldPoint.Loc.Y,FieldPoint.Loc.X].FieldAge = 65535 then
      PlantAct := taCut
    else
      PlantAct := taPlant;
end;


{Find closest harvestable deposit of Stone}
{Return walkable tile below Stone deposit}
function TTerrain.FindStone(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out StonePoint: TKMPointDir):Boolean;
const Ins=1; //1..Map-1
var i,k:integer; List:TKMPointDirList;
begin
  List := TKMPointDirList.Create;
  for i:=max(aLoc.Y-aRadius,Ins) to min(aLoc.Y+aRadius,fMapY-Ins-1) do //Leave one more tile below, where Stoncutter will stand
  for k:=max(aLoc.X-aRadius,Ins) to min(aLoc.X+aRadius,fMapX-Ins) do
    if (KMLength(aLoc,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i+1)) then
      if (TileIsStone(k,i)>0) then
        if (CanWalk in Land[i+1,k].Passability) //Now check the tile right below
        and not TileIsLocked(KMPoint(k, i+1)) then //Taken by another stonemason
          if Route_CanBeMade(aLoc,KMPoint(k,i+1),CanWalk,0) then
            List.AddItem(KMPointDir(k, i+1, dir_N));

  Result := List.GetRandom(StonePoint);
  List.Free;
end;


function TTerrain.FindOre(aLoc:TKMPoint; Rt:TResourceType; out OrePoint: TKMPoint):Boolean;
var i,k: Integer;
  RadLeft, RadRight, RadTop, RadBottom: Integer;
  R1,R2,R3,R4: Byte;
  L: array [1..4] of TKMPointList;
begin
  if not (Rt in [rt_IronOre, rt_GoldOre, rt_Coal]) then
    raise ELocError.Create('Wrong resource as Ore',aLoc);

  for i:=1 to 4 do L[i]:=TKMPointList.Create; //4 densities

  case Rt of
    rt_GoldOre: begin RadLeft:=7; RadRight:=6; RadTop:=11; RadBottom:=2; R1:=144; R2:=145; R3:=146; R4:=147; end;
    rt_IronOre: begin RadLeft:=7; RadRight:=5; RadTop:=11; RadBottom:=2; R1:=148; R2:=149; R3:=150; R4:=151; end;
    rt_Coal:    begin RadLeft:=4; RadRight:=5; RadTop:= 5; RadBottom:=2; R1:=152; R2:=153; R3:=154; R4:=155; end;
    else        begin RadLeft:=0; RadRight:=0; RadTop:= 0; RadBottom:=0; R1:=  0; R2:=  0; R3:=  0; R4:=  0; end;
  end;

  for i:=max(aLoc.Y-RadTop,1) to min(aLoc.Y+RadBottom,fMapY-1) do
  for k:=max(aLoc.X-RadLeft,1) to min(aLoc.X+RadRight,fMapX-1) do
  begin
    if Land[i,k].Terrain = R1 then begin if InRange(i,aLoc.Y-RadTop +2,aLoc.Y+RadBottom-2) then
                                         if InRange(k,aLoc.X-RadLeft+2,aLoc.X+RadRight -2) then
                                         L[1].AddEntry(KMPoint(k,i)) end else
    if Land[i,k].Terrain = R2 then begin if InRange(i,aLoc.Y-RadTop +1,aLoc.Y+RadBottom-1) then
                                         if InRange(k,aLoc.X-RadLeft+1,aLoc.X+RadRight -1) then
                                         L[2].AddEntry(KMPoint(k,i)) end else
    if Land[i,k].Terrain = R3 then L[3].AddEntry(KMPoint(k,i)) else //Always mine second richest ore, it is never left in KaM
    if Land[i,k].Terrain = R4 then L[4].AddEntry(KMPoint(k,i));     //Always mine richest ore
  end;

  Result := True;
  if not L[4].GetRandom(OrePoint) then
  if not L[3].GetRandom(OrePoint) then
  if not L[2].GetRandom(OrePoint) then
  if not L[1].GetRandom(OrePoint) then
    Result := False;

  for i:=1 to 4 do L[i].Free;
end;


//Return location of a Tree or a place to plant a tree depending on TreeAct
//taChop - Woodcutter wants to get a Tree because he went from home with an axe
//        (maybe his first target was already chopped down, so he either needs a tree or will go home)
//taPlant - Woodcutter specifically wants to get an empty place to plant a Tree
//taAny - Anything will do since Woodcutter is querying from home
//Result indicates if desired TreeAct place was found successfully
function TTerrain.FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aPlantAct: TPlantAct; out Tree: TKMPointDir; out PlantAct: TPlantAct): Boolean;
var
  List1, List2, List3: TKMPointList;
  I, K: Integer;
  T: TKMPoint;
  BestSlope, Slope: Integer;
begin
  //Why do we use 3 lists instead of one like Corn does?
  //Because we should always prefer stumps over empty places
  //even if there's only 1 stump - we choose it
  List1 := TKMPointList.Create; //Trees
  List2 := TKMPointList.Create; //Stumps (preferred when planting a new tree)
  List3 := TKMPointList.Create; //Clear places

  //Scan terrain and add all trees/spots into lists
  for I := max(aLoc.Y-aRadius, 1) to min(aLoc.Y+aRadius, fMapY-1) do
  for K := max(aLoc.X-aRadius, 1) to min(aLoc.X+aRadius, fMapX-1) do
  begin
     //Store in temp variable for speed
    T := KMPoint(K, I);

    if (KMLength(aLoc, T) <= aRadius)
    and not KMSamePoint(aAvoidLoc, T) then
    begin

      //Grownup tree
      if (aPlantAct in [taCut, taAny])
      and ObjectIsChopableTree(T, 4)
      and (Land[I,K].TreeAge >= TREE_AGE_FULL)
      //Woodcutter could be standing on any tile surrounding this tree
      and not TileIsLocked(T)
      and ((K = 1) or not TileIsLocked(KMPoint(K-1, I))) //if K=1, K-1 will be off map
      and ((I = 1) or not TileIsLocked(KMPoint(K, I-1)))
      and ((K = 1) or (I = 1) or not TileIsLocked(KMPoint(K-1, I-1)))
      and Route_CanBeMadeToVertex(aLoc, T, CanWalk) then
        List1.AddEntry(T);

      if (aPlantAct in [taPlant, taAny])
      and (CanPlantTrees in Land[I,K].Passability)
      and Route_CanBeMade(aLoc, T, CanWalk, 0)
      and not TileIsLocked(T) then //Taken by another woodcutter
        if ObjectIsChopableTree(T, 6) then
          List2.AddEntry(T) //Stump
        else
          List3.AddEntry(T); //Empty place
    end;
  end;

  //Convert taAny to either a Tree or a Spot
  //Average tree uses ~9 tiles, hence we correct Places weight by some amount (15 fits good) to make choice between Plant and Chop more even. Feel free to fine-tune it
  if (aPlantAct in [taCut, taAny])
  and ((List1.Count > 8) //Always chop the tree if there are many
       or (List2.Count + List3.Count = 0)
       or ((List1.Count > 0) and (KaMRandom < List1.Count / (List1.Count + (List2.Count + List3.Count)/15)))
      ) then
  begin
    PlantAct := taCut;
    Result := List1.GetRandom(T);
  end else begin
    PlantAct := taPlant;
    Result := List2.GetRandom(T);
    if not Result then
      Result := List3.GetRandom(T);
  end;

  //Note: Result can still be False
  List1.Free;
  List2.Free;
  List3.Free;

  //Choose direction of approach based on which one is the flatest (animation looks odd if not flat)
  if (PlantAct = taCut) and Result then
  begin
    BestSlope := 255;
    Result := False; //It is already tested that we can walk to the tree, but double-check

    for i:=-1 to 0 do for k:=-1 to 0 do
    if Route_CanBeMade(aLoc, KMPoint(T.X+k, T.Y+i), CanWalk, 0) then
    begin
      Slope := Round(HeightAt(T.X+k-0.5, T.Y+i-0.5)) - Land[T.Y, T.X].Height;
      //Cutting trees which are higher than us from the front looks visually poor, (axe hits ground) so avoid it where possible
      if (i = 0) and (Slope < 0) then Slope := Slope - 100; //Make it worse but not worse than initial BestSlope
      if Abs(Slope) < BestSlope then
      begin
        Tree := KMPointDir(T.X+k, T.Y+i, KMGetVertexDir(k, i));
        Result := True;
        BestSlope := Abs(Slope);
      end;
    end;
  end;

  if (PlantAct = taPlant) and Result then
    Tree := KMPointDir(T, dir_N); //Trees must always be planted facing north as that is the unit DAT animation that is used
end;


{Find seaside}
{Return walkable tile nearby}
function TTerrain.FindFishWater(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out FishPoint: TKMPointDir):Boolean;
const Ins=2; //2..Map-2
var i,k,j,l:integer; List:TKMPointDirList;
begin
  //Check is in stages:
  // A) Inital checks, inside map and radius, etc.
  // B) Limit to only tiles which are water
  // C) Limit to tiles which have fish in the water body
  // D) Final checks, route can be made, etc.

  List:=TKMPointDirList.Create;
  for i:=max(aLoc.Y-aRadius,Ins) to min(aLoc.Y+aRadius,fMapY-Ins) do
  for k:=max(aLoc.X-aRadius,Ins) to min(aLoc.X+aRadius,fMapX-Ins) do
    // A) Inital checks, inside map and radius, etc.
    if (KMLength(aLoc,KMPoint(k,i)) <= aRadius) then
      if TileIsWater(KMPoint(k,i)) and WaterHasFish(KMPoint(k,i)) then //Limit to only tiles which are water and have fish
        //Now find a tile around this one that can be fished from
        for j:=-1 to 1 do
          for l:=-1 to 1 do
            if TileInMapCoords(k+j,i+l) and ((l <> 0) or (j <> 0)) and
            not TileIsLocked(KMPoint(k+j, i+l)) then //Taken by another fisherman
              // D) Final check: route can be made and isn't avoid loc
              if Route_CanBeMade(aLoc, KMPoint(k+j, i+l), CanWalk, 0)
              and not KMSamePoint(aAvoidLoc,KMPoint(k+j,i+l)) then
                List.AddItem(KMPointDir(k+j, i+l, KMGetDirection(j,l)));

  Result := List.GetRandom(FishPoint);
  List.Free;
end;


function TTerrain.CanFindFishingWater(aLoc: TKMPoint; aRadius: Integer): Boolean;
const Ins=2; //2..Map-2
var i,k:integer;
begin
  Result := false;
  for i:=max(aLoc.Y-aRadius,Ins) to min(aLoc.Y+aRadius,fMapY-Ins) do
  for k:=max(aLoc.X-aRadius,Ins) to min(aLoc.X+aRadius,fMapX-Ins) do
    if (KMLength(aLoc,KMPoint(k,i)) <= aRadius) then
      if TileIsWater(KMPoint(k,i)) then
      begin
        Result := true;
        exit;
      end;
end;


function TTerrain.ChooseTreeToPlant(aLoc:TKMPoint):integer;
begin
  //This function randomly chooses a tree object based on the terrain type. Values matched to KaM, using all soil tiles.
  case Land[aLoc.Y,aLoc.X].Terrain of
    0..3,5,6,8,9,11,13,14,18,19,56,57,66..69,72..74,84..86,93..98,180,188: Result := ChopableTrees[1+KaMRandom(7),1]; //Grass (oaks, etc.)
    26..28,75..80,182,190:                                                 Result := ChopableTrees[7+KaMRandom(2),1]; //Yellow dirt
    16,17,20,21,34..39,47,49,58,64,65,87..89,183,191,220,247:              Result := ChopableTrees[9+KaMRandom(5),1]; //Brown dirt (pine trees)
    else Result := ChopableTrees[1+KaMRandom(length(ChopableTrees)),1]; //If it isn't one of those soil types then choose a random tree
  end;
end;


procedure TTerrain.GetHouseMarks(aLoc:TKMPoint; aHouseType:THouseType; aList:TKMPointTagList);
  procedure MarkPoint(aPoint: TKMPoint; aID: Integer);
  var I: Integer;
  begin
    for I := 0 to aList.Count - 1 do //Skip wires from comparison
      if (aList.Tag[I] <> 0) and KMSamePoint(aList[I], aPoint) then
        Exit;
    aList.AddEntry(aPoint, aID, 0);
  end;

var
  i,k,s,t:integer;
  P2:TKMPoint;
  AllowBuild:boolean;
  HA: THouseArea;
begin
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
  if HA[i,k]<>0 then
  begin

    if TileInMapCoords(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4,1) then
    begin
      //This can't be done earlier since values can be off-map
      P2 := KMPoint(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4);

      //Check house-specific conditions, e.g. allow shipyards only near water and etc..
      case aHouseType of
        ht_IronMine: AllowBuild := (CanBuildIron in Land[P2.Y,P2.X].Passability);
        ht_GoldMine: AllowBuild := (CanBuildGold in Land[P2.Y,P2.X].Passability);
        else         AllowBuild := (CanBuild     in Land[P2.Y,P2.X].Passability);
      end;

      //Check surrounding tiles in +/- 1 range for other houses pressence
      if not AllowBuild then
      for s:=-1 to 1 do for t:=-1 to 1 do
      if (s<>0) or (t<>0) then  //This is a surrounding tile, not the actual tile
      if Land[P2.Y+t,P2.X+s].TileLock in [tlFenced,tlDigged,tlHouse] then
      begin
        MarkPoint(KMPoint(P2.X+s,P2.Y+t),479);
        AllowBuild := false;
      end;

      //Mark the tile according to previous check results
      if AllowBuild then
      begin
        aList.AddEntry(P2, 0, 0);
        if HA[i,k]=2 then
          MarkPoint(P2,481);
      end else
      begin
        if HA[i,k]=2 then
          MarkPoint(P2,482)
        else
          if aHouseType in [ht_GoldMine,ht_IronMine] then
            MarkPoint(P2,480)
          else
            MarkPoint(P2,479);
      end;

    end else
    if TileInMapCoords(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4,0) then
      MarkPoint(KMPoint(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4),479);
  end;
end;


function TTerrain.WaterHasFish(aLoc:TKMPoint):boolean;
begin
  Result := (fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aLoc.Y,aLoc.X].WalkConnect[wcFish],false) <> nil);
end;


function TTerrain.CatchFish(aLoc:TKMPointDir; TestOnly:boolean=false):boolean;
var MyFish: TKMUnitAnimal;
begin
  //Here we are catching fish in the tile 1 in the direction
  aLoc := KMGetPointInDir(aLoc.Loc, aLoc.Dir);
  MyFish := fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aLoc.Loc.Y,aLoc.Loc.X].WalkConnect[wcFish],not TestOnly);
  Result := (MyFish <> nil);
  if (not TestOnly) and (MyFish <> nil) then MyFish.ReduceFish; //This will reduce the count or kill it (if they're all gone)
end;


procedure TTerrain.SetTree(Loc: TKMPoint; ID: Integer);
begin
  Land[Loc.Y,Loc.X].Obj :=  ID;
  Land[Loc.Y,Loc.X].TreeAge :=  1;

  //Add 1 tile on sides because surrounding tiles will be affected (CanPlantTrees)
  UpdatePassabilityAround(Loc);
end;


{Remove the tree and place a falling tree instead}
procedure TTerrain.FallTree(Loc:TKMPoint);
var h:integer;
begin
  for h:=1 to length(ChopableTrees) do
    if ChopableTrees[h,4]=Land[Loc.Y,Loc.X].Obj then
    begin
      Land[Loc.Y,Loc.X].Obj:=ChopableTrees[h,6];                        //Set stump object
      FallingTrees.AddEntry(Loc,ChopableTrees[h,5],fAnimStep);  //along with falling tree
      fSoundLib.Play(sfx_TreeDown,Loc,true);
      exit;
    end;
end;


{Remove the tree and place stump instead}
procedure TTerrain.ChopTree(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].TreeAge := 0;
  FallingTrees.RemoveEntry(Loc);
  UpdatePassabilityAround(Loc); //Because surrounding tiles will be affected (CanPlantTrees)

  //WalkConnect takes diagonal passability into account
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


procedure TTerrain.SowCorn(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 1;
  Land[Loc.Y,Loc.X].Terrain  := 61; //Plant it right away, don't wait for update state
  UpdatePassability(Loc);
end;


procedure TTerrain.CutCorn(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 0;
  Land[Loc.Y,Loc.X].Terrain  := 63;
  Land[Loc.Y,Loc.X].Obj := 255;
end;


procedure TTerrain.CutGrapes(Loc: TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge := 1;
  Land[Loc.Y,Loc.X].Obj := 54; //Reset the grapes
end;


{Used only in debug - places coal on map}
procedure TTerrain.SetResourceDeposit(Loc:TKMPoint; rt:TResourceType);
begin
  if not TileInMapCoords(Loc.X, Loc.Y) then exit;
  case rt of
    rt_Stone:   Land[Loc.Y,Loc.X].Terrain:=132;
    rt_Coal:    Land[Loc.Y,Loc.X].Terrain:=155;
    rt_IronOre: Land[Loc.Y,Loc.X].Terrain:=151;
    rt_GoldOre: Land[Loc.Y,Loc.X].Terrain:=147;
    else        raise ELocError.Create('Wrong resource deposit',Loc);
  end;
  UpdatePassability(Loc);
end;


{Extract one unit of stone}
procedure TTerrain.DecStoneDeposit(Loc:TKMPoint);

  procedure UpdateTransition(X,Y:integer);
  const TileID:array[0..15]of byte = (0,139,139,138,139,140,138,141,139,138,140,141,138,141,141,128);
         RotID:array[0..15]of byte = (0,  0,  1,  0,  2,  0,  1,  3,  3,  3,  1,  2,  2,  1,  0,  0);
  var Bits:byte;
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
function TTerrain.DecOreDeposit(Loc:TKMPoint; rt:TResourceType):boolean;
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


procedure TTerrain.UpdatePassabilityAround(Loc:TKMPoint);
begin
  UpdatePassability(KMRect(Loc.X - 1, Loc.Y - 1, Loc.X + 1, Loc.Y + 1));
end;


procedure TTerrain.UpdatePassability(Loc:TKMPoint);
var
  i,k:integer;
  HousesNearBy:boolean;

  procedure AddPassability(aLoc:TKMPoint; aPass:TPassabilitySet);
  begin
    Land[aLoc.Y,aLoc.X].Passability:=Land[aLoc.Y,aLoc.X].Passability + aPass;
  end;

  function IsObjectsNearby(X,Y:integer): Boolean;
  var I,K: Integer; P: TKMPoint;
  begin
    Result := False;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if ((I<>0) or (K<>0)) and TileInMapCoords(X+I, Y+K) then
        begin
          P := KMPoint(X+I, Y+K);

          //Tiles next to it can't be trees/stumps
          if MapElem[Land[P.Y,P.X].Obj+1].DontPlantNear then
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
begin
  Assert(TileInMapCoords(Loc.X, Loc.Y)); //First of all exclude all tiles outside of actual map

  Land[Loc.Y,Loc.X].Passability := [];

  //For all passability types other than CanAll, houses and fenced houses are excluded
  if Land[Loc.Y,Loc.X].TileLock in [tlNone, tlFenced, tlRoadWork] then
  begin

    if TileIsWalkable(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
    and CheckHeightPass(Loc, CanWalk) then
      AddPassability(Loc, [CanWalk]);

    if (Land[Loc.Y,Loc.X].TileOverlay = to_Road)
    and (CanWalk in Land[Loc.Y,Loc.X].Passability) then //Not all roads are walkable, they must also have CanWalk passability
      AddPassability(Loc, [CanWalkRoad]);

    //Check for houses around this tile
    HousesNearBy := False;
    for i := -1 to 1 do
    for k := -1 to 1 do
      if TileInMapCoords(Loc.X+k, Loc.Y+i)
      and (Land[Loc.Y+i,Loc.X+k].TileLock in [tlFenced,tlDigged,tlHouse]) then
        HousesNearBy := True;

    if TileIsRoadable(Loc)
    and ((Land[Loc.Y,Loc.X].Obj = 255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved)) //Only certain objects are excluded
    and not HousesNearBy
    and not TileIsCornField(Loc) //Can't build houses on fields
    and not TileIsWineField(Loc)
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and TileInMapCoords(Loc.X, Loc.Y, 1)
    and CheckHeightPass(Loc, CanBuild) then //No houses nearby
      AddPassability(Loc, [CanBuild]);

    if (Land[Loc.Y,Loc.X].Terrain in [109,166..170])
    and (Land[Loc.Y,Loc.X].Rotation mod 4 = 0) //only horizontal mountain edges allowed
    and ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved))
    and not HousesNearBy
    and not TileIsCornField(Loc) //Can't build houses on fields
    and not TileIsWineField(Loc)
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and TileInMapCoords(Loc.X,Loc.Y, 1)
    and CheckHeightPass(Loc, CanBuildIron) then
      AddPassability(Loc, [CanBuildIron]);

    if (Land[Loc.Y,Loc.X].Terrain in [171..175])
    and (Land[Loc.Y,Loc.X].Rotation mod 4 = 0)
    and ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved))
    and not HousesNearBy
    and not TileIsCornField(Loc) //Can't build houses on fields
    and not TileIsWineField(Loc)
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and TileInMapCoords(Loc.X,Loc.Y, 1)
    and CheckHeightPass(Loc,CanBuildGold) then
      AddPassability(Loc, [CanBuildGold]);

    if TileIsRoadable(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and CheckHeightPass(Loc,CanMakeRoads) then
      AddPassability(Loc, [CanMakeRoads]);

    if TileIsSoil(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and not TileIsWineField(Loc)
    and not TileIsCornField(Loc)
    and CheckHeightPass(Loc,CanMakeFields) then
      AddPassability(Loc, [CanMakeFields]);

    if TileIsSoil(Loc)
    and not IsObjectsNearby(Loc.X,Loc.Y) //This function checks surrounding tiles
    and (Land[Loc.Y,Loc.X].TileLock = tlNone)
    and (Loc.X > 1) and (Loc.Y > 1) //Not top/left of map, but bottom/right is ok
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and not HousesNearBy
    and ((Land[Loc.Y,Loc.X].Obj=255) or ObjectIsChopableTree(KMPoint(Loc.X,Loc.Y),6))
    and CheckHeightPass(Loc, CanPlantTrees) then
      AddPassability(Loc, [CanPlantTrees]);

    if TileIsWater(Loc) then
      AddPassability(Loc, [CanFish]);

    if TileIsSand(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
    //TileLock checked in outer begin/end
    and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)
    and not TileIsCornField(Loc)
    and not TileIsWineField(Loc)
    and CheckHeightPass(Loc, CanCrab) then //Can't crab on houses, fields and roads (can walk on fenced house so you can't kill them by placing a house on top of them)
      AddPassability(Loc, [CanCrab]);

    if TileIsSoil(Loc)
    and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
    //TileLock checked in outer begin/end
    //Wolf are big enough to run over roads, right?
    and not TileIsCornField(Loc)
    and not TileIsWineField(Loc)
    and CheckHeightPass(Loc,CanWolf) then
      AddPassability(Loc, [CanWolf]);
  end;

  if TileIsWalkable(Loc)
  and not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked
  and CheckHeightPass(Loc, CanWalk)
  and (Land[Loc.Y,Loc.X].TileLock <> tlHouse) then
    AddPassability(Loc, [CanWorker]);

  //Check all 4 tiles that border with this vertex
  if TileIsFactorable(KMPoint(Loc.X  ,Loc.Y))
  and TileIsFactorable(KMPoint(Loc.X-1,Loc.Y))
  and TileIsFactorable(KMPoint(Loc.X  ,Loc.Y-1))
  and TileIsFactorable(KMPoint(Loc.X-1,Loc.Y-1)) then
    AddPassability(Loc, [canFactor]);

  //Check for houses around this vertice(!)
  //Use only with CanElevate since it's vertice-based!
  HousesNearBy := False;
  for i := -1 to 0 do
  for k := -1 to 0 do
    if TileInMapCoords(Loc.X+k, Loc.Y+i) then
    //Can't elevate built houses, can elevate fenced and dug houses though
    if (Land[Loc.Y+i,Loc.X+k].TileLock = tlHouse) then
      HousesNearBy := True;

  if VerticeInMapCoords(Loc.X,Loc.Y)
  and not HousesNearBy then
    AddPassability(Loc, [CanElevate]);
end;


function TTerrain.CheckPassability(Loc:TKMPoint; aPass:TPassability):boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and (aPass in Land[Loc.Y,Loc.X].Passability);
end;


function TTerrain.HasUnit(Loc:TKMPoint):boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and (Land[Loc.Y,Loc.X].IsUnit <> nil); //Second condition won't get checked if first is false
end;


function TTerrain.HasVertexUnit(Loc:TKMPoint):boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and (Land[Loc.Y,Loc.X].IsVertexUnit <> vu_None); //Second condition won't get checked if first is false
end;


//Check which road connect ID the tile has (to which road network does it belongs to)
function TTerrain.GetRoadConnectID(Loc:TKMPoint):byte;
begin
  Result := GetConnectID(wcRoad, Loc);
end;


//Check which walk connect ID the tile has (to which walk network does it belongs to)
function TTerrain.GetWalkConnectID(Loc:TKMPoint):byte;
begin
  Result := GetConnectID(wcWalk, Loc);
end;


function TTerrain.GetConnectID(aWalkConnect: TWalkConnect; Loc:TKMPoint):byte;
begin
  if TileInMapCoords(Loc.X,Loc.Y) then
    Result := Land[Loc.Y,Loc.X].WalkConnect[aWalkConnect]
  else
    Result:=0; //No network
end;


function TTerrain.CheckAnimalIsStuck(Loc:TKMPoint; aPass:TPassability; aCheckUnits:boolean=true):boolean;
var i,k: integer;
begin
  Result := true; //Assume we are stuck
  for i:=-1 to 1 do for k:=-1 to 1 do
    if TileInMapCoords(Loc.X+k,Loc.Y+i) then
      if (i<>0)or(k<>0) then
        if CanWalkDiagonaly(Loc,KMPoint(Loc.X+k,Loc.Y+i)) then
          if (Land[Loc.Y+i,Loc.X+k].IsUnit = nil) or (not aCheckUnits) then
            if aPass in Land[Loc.Y+i,Loc.X+k].Passability then
            begin
              Result := false; //at least one tile is empty, so unit is not stuck
              exit;
            end;
end;


{Return random tile surrounding Loc with aPass property. PusherLoc is the unit that pushed us which is}
{preferable to other units (otherwise we can get two units swapping places forever)}
function TTerrain.GetOutOfTheWay(Loc, PusherLoc:TKMPoint; aPass:TPassability):TKMPoint;
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
    and CanWalkDiagonaly(Loc, KMPoint(Loc.X+K, Loc.Y+I)) //Check for trees that stop us walking on the diagonals!
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


function TTerrain.FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: boolean=false):Boolean;
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
    and CanWalkDiagonaly(Loc, KMPoint(Loc.X+K,Loc.Y+I)) //Check for trees that stop us walking on the diagonals!
    and (Land[Loc.Y+I,Loc.X+K].TileLock in [tlNone, tlFenced])
    and (KMLength(KMPoint(Loc.X+K,Loc.Y+I),Loc2) <= 1) //Right next to Loc2 (not diagonal)
    and not HasUnit(KMPoint(Loc.X+K,Loc.Y+I)) then //Doesn't have a unit
      L1.AddEntry(KMPoint(Loc.X+K,Loc.Y+I));

  //List 2 holds the best positions, ones which are also next to Loc3 (next position)
  L2 := TKMPointList.Create;
  if not KMSamePoint(Loc3, KMPoint(0,0)) then //No Loc3 was given
  for I := 0 to L1.Count - 1 do
    if KMLength(L1[I], Loc3) < 1.5 then //Next to Loc3 (diagonal is ok)
      L2.AddEntry(L1[I]);

  Result := True;
  if not(L2.GetRandom(SidePoint)) then
  if (OnlyTakeBest) or (not(L1.GetRandom(SidePoint))) then
    Result := False; //No side step positions available

  L1.Free;
  L2.Free;
end;


//Test wherever it is possible to make the route without actually making it to save performance
function TTerrain.Route_CanBeMade(LocA, LocB: TKMPoint; aPass: TPassability; aDistance: Single): Boolean;
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
  if GetLength(LocB,KMPoint(k,i)) <= aDistance then
    TestRadius := TestRadius or CheckPassability(KMPoint(k,i),aPass);
  Result := Result and TestRadius;

  case aPass of
    CanWalk:      WC := wcWalk;
    CanWalkRoad:  WC := wcRoad;
    CanFish:      WC := wcFish;
    CanWolf:      WC := wcWolf;
    CanCrab:      WC := wcCrab;
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
  if GetLength(LocB,KMPoint(k,i)) <= aDistance then
    TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[WC] = Land[i,k].WalkConnect[WC]);
  Result := Result and TestRadius;
end;


//Check if a route can be made to this vertex, from any direction (used for woodcutter cutting trees)
function TTerrain.Route_CanBeMadeToVertex(LocA, LocB: TKMPoint; aPass: TPassability):boolean;
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
function TTerrain.GetClosestTile(TargetLoc, OriginLoc: TKMPoint; aPass: TPassability; aAcceptTargetLoc: Boolean):TKMPoint;
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
    CanWolf:     wcType := wcWolf;
    CanCrab:     wcType := wcCrab;
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
    then begin
      Result := T; //Assign if all test are passed
      exit;
    end;
  end;

  Result := OriginLoc; //If we don't find one, return existing Loc
end;


{Mark tile as occupied}
procedure TTerrain.UnitAdd(LocTo:TKMPoint; aUnit: Pointer);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Land[LocTo.Y,LocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(LocTo));
  Land[LocTo.Y,LocTo.X].IsUnit := aUnit
end;


{ Mark tile as empty }
// We have no way of knowing whether a unit is inside a house, or several units exit a house at once
// when exiting the game and destroying all units this will cause asserts.
procedure TTerrain.UnitRem(LocFrom:TKMPoint);
begin
  if not DO_UNIT_INTERACTION then exit;
  Land[LocFrom.Y,LocFrom.X].IsUnit := nil;
end;


{Mark previous tile as empty and next one as occupied}
//We need to check both tiles since UnitWalk is called only by WalkTo where both tiles aren't houses
procedure TTerrain.UnitWalk(LocFrom,LocTo:TKMPoint; aUnit: Pointer);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Land[LocFrom.Y, LocFrom.X].IsUnit = aUnit, 'Trying to remove wrong unit at '+TypeToString(LocFrom));
  Land[LocFrom.Y, LocFrom.X].IsUnit := nil;
  Assert(Land[LocTo.Y, LocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(LocTo));
  Land[LocTo.Y, LocTo.X].IsUnit := aUnit
end;


procedure TTerrain.UnitSwap(LocFrom,LocTo:TKMPoint; UnitFrom:Pointer);
begin
  Assert(Land[LocFrom.Y,LocFrom.X].IsUnit = UnitFrom, 'Trying to swap wrong unit at '+TypeToString(LocFrom));
  Land[LocFrom.Y,LocFrom.X].IsUnit := Land[LocTo.Y,LocTo.X].IsUnit;
  Land[LocTo.Y,LocTo.X].IsUnit := UnitFrom;
end;


{Mark vertex as occupied}
procedure TTerrain.UnitVertexAdd(LocTo:TKMPoint; Usage: TKMVertexUsage);
begin
  if not DO_UNIT_INTERACTION then exit;
  assert(Usage <> vu_None, 'Invalid add vu_None at '+TypeToString(LocTo));
  assert((Land[LocTo.Y,LocTo.X].IsVertexUnit = vu_None) or (Land[LocTo.Y,LocTo.X].IsVertexUnit = Usage),'Opposite vertex in use at '+TypeToString(LocTo));

  Land[LocTo.Y,LocTo.X].IsVertexUnit := Usage;
end;


procedure TTerrain.UnitVertexAdd(LocFrom, LocTo:TKMPoint);
begin
  assert(KMStepIsDiag(LocFrom, LocTo), 'Add non-diagonal vertex?');
  UnitVertexAdd(KMGetDiagVertex(LocFrom, LocTo), GetVertexUsageType(LocFrom, LocTo));
end;


{Mark vertex as empty}
procedure TTerrain.UnitVertexRem(LocFrom:TKMPoint);
begin
  if not DO_UNIT_INTERACTION then exit;
  Land[LocFrom.Y,LocFrom.X].IsVertexUnit := vu_None;
end;


//This function tells whether the diagonal is "in use". (a bit like IsUnit) So if there is a unit walking on
//the oppsoite diagonal you cannot use the vertex (same diagonal is allowed for passing and fighting)
//It stops units walking diagonally through each other or walking through a diagonal that has weapons swinging through it
function TTerrain.VertexUsageCompatible(LocFrom, LocTo:TKMPoint): Boolean;
var
  Vert: TKMPoint;
  VertUsage: TKMVertexUsage;
begin
  Vert := KMGetDiagVertex(LocFrom, LocTo);
  VertUsage := GetVertexUsageType(LocFrom, LocTo);
  Result := (Land[Vert.Y, Vert.X].IsVertexUnit in [vu_None, VertUsage]);
end;


function TTerrain.GetVertexUsageType(LocFrom, LocTo:TKMPoint): TKMVertexUsage;
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
procedure TTerrain.FlattenTerrain(Loc: TKMPoint; aUpdateWalkConnects: Boolean = True);
var TilesFactored: Integer;

  //If tiles with units standing on them become unwalkable we should try to fix them
  procedure EnsureWalkable(aX,aY: Word);
  begin
    //We did not recalculated passability yet, hence tile has CanWalk but CheckHeightPass=False already
    if (Land[aY,aX].IsUnit <> nil)
    and CheckPassability(KMPoint(aX,aY), CanWalk)
    and not CheckHeightPass(KMPoint(aX,aY), CanWalk)
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
      inc(TilesFactored);
    end
    else
      Result := 0;
  end;

var
  I, K: Word;
  Avg: Word;
begin
  Assert(TileInMapCoords(Loc.X, Loc.Y), 'Can''t flatten tile outside map coordinates');

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
  UpdatePassabilityAround(Loc); //Changing height will affect the cells around this one

  if aUpdateWalkConnects then
    UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


{Flatten a list of points in the mission init}
procedure TTerrain.FlattenTerrain(LocList:TKMPointList);
var
  I: Integer;
begin
  for I := 0 to LocList.Count - 1 do
    FlattenTerrain(LocList[I], False); //Rebuild the Walk Connect at the end, rather than every time

  //All 5 are affected by height
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


//Rebuilds lighting values for given bounds.
//These values are used to draw highlights/shadows on terrain
//Note that input values may be off-map
procedure TTerrain.UpdateLighting(aRect: TKMRect);
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

    //Map borders always fade to black
    if (I = 1) or (I = fMapY) or (K = 1) or (K = fMapX) then
      Land[I,K].Light := -1;
  end;
end;


//Rebuilds passability for given bounds
procedure TTerrain.UpdatePassability(aRect: TKMRect);
var I, K: Integer;
begin
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY - 1) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX - 1) do
      UpdatePassability(KMPoint(K, I));
end;


//Rebuilds connected areas using flood fill algorithm
procedure TTerrain.UpdateWalkConnect(const aSet: array of TWalkConnect);
var
  WC: TWalkConnect;
  AllowDiag: Boolean;
  Count: Integer;
  Pass: TPassability;
  FillID:Byte; //The ID to be filled, don't pass it into FillArea each time when it doesn't change

  procedure FillArea(X,Y: Word);
  begin
    if (Land[Y,X].WalkConnect[WC] = 0) //Untested area
    and (Pass in Land[Y,X].Passability) then //Matches passability
    begin
      Land[Y,X].WalkConnect[WC] := FillID;
      Inc(Count);
      //Using custom TileInMapCoords replacement gives ~40% speed improvement
      //Using custom CanWalkDiagonally is also much faster
      if X-1 >= 1 then
      begin
        if AllowDiag and (Y-1 >= 1) and not MapElem[Land[Y,X].Obj+1].DiagonalBlocked then
          FillArea(X-1, Y-1);
        FillArea(X-1, Y);
        if AllowDiag and (Y+1 <= fMapY) and not MapElem[Land[Y+1,X].Obj+1].DiagonalBlocked then
          FillArea(X-1,Y+1);
      end;

      if Y-1 >= 1 then     FillArea(X, Y-1);
      if Y+1 <= fMapY then FillArea(X, Y+1);

      if X+1 <= fMapX then
      begin
        if AllowDiag and (Y-1 >= 1) and not MapElem[Land[Y,X+1].Obj+1].DiagonalBlocked then
          FillArea(X+1, Y-1);
        FillArea(X+1, Y);
        if AllowDiag and (Y+1 <= fMapY) and not MapElem[Land[Y+1,X+1].Obj+1].DiagonalBlocked then
          FillArea(X+1, Y+1);
      end;
    end;
  end;

  //Split into a seperate function to save CPU time pushing and popping ID off the stack
  //when it never changes in subsequent runs
  procedure StartFillArea(aX,aY: Word; aID: Byte);
  begin
    FillID := aID;
    FillArea(aX,aY);
  end;

//const MinSize=9; //Minimum size that is treated as new area
const
  WCSet: array [TWalkConnect] of TPassability = (
    CanWalk, CanWalkRoad, CanFish, CanWolf, CanCrab, CanWorker);
var
  I,J,K: Integer;
  AreaID: Byte;
begin
  //Process all items from set
  for J := Low(aSet) to High(aSet) do
  begin
    WC := aSet[J];
    Pass := WCSet[WC];
    AllowDiag := (WC <> wcRoad); //Do not consider diagonals "connected" for roads

    //todo: Can be optimized if we know from which Tile to rebuild, and if that tile makes no difference - skip the thing

    //Reset everything
    for I := 1 to fMapY do for K := 1 to fMapX do
      Land[I,K].WalkConnect[WC] := 0;

    AreaID := 0;
    for I := 1 to fMapY do for K := 1 to fMapX do
    if (Land[I,K].WalkConnect[WC] = 0) and (Pass in Land[I,K].Passability) then
    begin
      Inc(AreaID);
      Count := 0;
      StartFillArea(K,I,AreaID);

      if Count = 1 {<MinSize} then //Revert
      begin
        Dec(AreaID);
        Count := 0;
        Land[I,K].WalkConnect[WC] := 0;
      end;

      Assert(AreaID < 255, 'UpdateWalkConnect failed due too many unconnected areas');
    end;
  end;
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TTerrain.SetHouse(Loc: TKMPoint; aHouseType: THouseType; aHouseStage: THouseStage; aOwner: TPlayerIndex; const aFlattenTerrain: Boolean = False);
var i,k,x,y:word; ToFlatten:TKMPointList; HA:THouseArea;
begin
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
                              Land[y,x].Obj := 255;
                              //If house was set e.g. in mission file we must flatten the terrain as no one else has
                              ToFlatten.AddEntry(KMPoint(x,y));
                            end;
                          end;
        end;
        UpdateBorders(KMPoint(x,y));
      end;
    end;

  if ToFlatten <> nil then
  begin
    FlattenTerrain(ToFlatten);
    ToFlatten.Free;
  end;

  //Recalculate Passability for tiles around the house so that they can't be built on too
  UpdatePassability(KMRect(Loc.X - 3, Loc.Y - 4, Loc.X + 2, Loc.Y + 1));
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


{That is mainly used for minimap now}
procedure TTerrain.SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);
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
function TTerrain.CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType):boolean;
begin
  Result := TileInMapCoords(Loc.X, Loc.Y)
            and (Land[Loc.Y, Loc.X].IsUnit = nil) //Check for no unit below
            and (fResource.UnitDat[aUnitType].AllowedPassability in Land[Loc.Y, Loc.X].Passability);
end;


{Check if house can be placed in that place}
function TTerrain.CanPlaceHouse(Loc: TKMPoint; aHouseType: THouseType): Boolean;
var I,K: Integer; HA: THouseArea;
begin
  Result := True;
  HA := fResource.HouseDat[aHouseType].BuildArea;
  Loc.X := Loc.X - fResource.HouseDat[aHouseType].EntranceOffsetX; //update offset
  for I := 1 to 4 do
  for K := 1 to 4 do
    if Result and (HA[I,K] <> 0) then
    begin
      //Inset one tile from map edges
      Result := Result and TileInMapCoords(Loc.X + k - 3, Loc.Y + i - 4, 1);

      case aHouseType of
        ht_IronMine: Result := Result and (CanBuildIron in Land[Loc.Y+I-4,Loc.X+K-3].Passability);
        ht_GoldMine: Result := Result and (CanBuildGold in Land[Loc.Y+I-4,Loc.X+K-3].Passability);
        else         Result := Result and (CanBuild in Land[Loc.Y+I-4,Loc.X+K-3].Passability);
      end;
    end;
end;


//Simple checks when placing houses from the script:
function TTerrain.CanPlaceHouseFromScript(aHouseType: THouseType; Loc:TKMPoint):boolean;
var i,k,j,l:integer; HA:THouseArea; TestLoc: TKMPoint;
begin
  Result := True;
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
    if Result and (HA[i,k] <> 0) then
    begin
      TestLoc := KMPoint(Loc.X+k-3, Loc.Y+i-4);
      Result := Result and TileInMapCoords(TestLoc.X, TestLoc.Y, 1); //Inset one tile from map edges
      Result := Result and TileIsWalkable(TestLoc); //Tile must be walkable

      //Mines must be on a mountain edge
      if aHouseType = ht_IronMine then
        Result := Result and (Land[TestLoc.Y,TestLoc.X].Terrain in [109, 166..170]) and (Land[TestLoc.Y,TestLoc.X].Rotation mod 4 = 0);
      if aHouseType = ht_GoldMine then
        Result := Result and (Land[TestLoc.Y,TestLoc.X].Terrain in [171..175     ]) and (Land[TestLoc.Y,TestLoc.X].Rotation mod 4 = 0);

      //Check surrounding tiles for another house that overlaps
      for j:=-1 to 1 do
      for l:=-1 to 1 do
        if TileInMapCoords(TestLoc.X+l, TestLoc.Y+j)
        and (Land[TestLoc.Y+j,TestLoc.X+l].TileLock <> tlNone) then
          Result := False;
    end;
end;


function TTerrain.CanAddField(Loc: TKMPoint; aFieldType: TFieldType): Boolean;
begin
  //Make sure it is within map, roads can be built on edge
  Result := TileInMapCoords(Loc.X, Loc.Y);
  case aFieldType of
    ft_Road:  Result := Result AND (CanMakeRoads  in Land[Loc.Y, Loc.X].Passability);
    ft_Corn:  Result := Result AND (CanMakeFields in Land[Loc.Y, Loc.X].Passability);
    ft_Wine:  Result := Result AND (CanMakeFields in Land[Loc.Y, Loc.X].Passability);
    ft_Wall:  Result := Result AND (CanMakeRoads  in Land[Loc.Y, Loc.X].Passability);
    else      Result := False;
  end;
end;


function TTerrain.CheckHeightPass(aLoc:TKMPoint; aPass:TPassability):boolean;
  function GetHgtSafe(X,Y:word):byte;
  begin
    if TileInMapCoords(X,Y) then
      Result := Land[Y,X].Height //Use requested tile
    else
      Result := Land[aLoc.Y,aLoc.X].Height; //Otherwise return height of original tile which will have no effect
  end;
  function TestHeight(aHeight:byte):boolean;
  var Points: array[1..4] of byte;
  begin
    //Put points into an array like this so it's easy to understand:
    // 1 2
    // 3 4
    Points[1] := GetHgtSafe(aLoc.X,   aLoc.Y);
    Points[2] := GetHgtSafe(aLoc.X+1, aLoc.Y);
    Points[3] := GetHgtSafe(aLoc.X,   aLoc.Y+1);
    Points[4] := GetHgtSafe(aLoc.X+1, aLoc.Y+1);

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
    Result :=            (abs(Points[1]-Points[2]) <= aHeight);
    Result := Result AND (abs(Points[3]-Points[4]) <= aHeight);
    Result := Result AND (abs(Points[3]-Points[1]) <= aHeight);
    Result := Result AND (abs(Points[4]-Points[2]) <= aHeight*2); //Bottom-right to top-right is twice as tolerant

    //Diagonals of tile
    Result := Result AND (abs(Points[1]-Points[4]) <= aHeight);
    Result := Result AND (abs(Points[3]-Points[2]) <= aHeight*2); //Bottom-left to top-right is twice as tolerant
  end;
begin
  //Three types tested in KaM: >=25 - unwalkable/roadable; >=18 - unbuildable.
  Result := true;
  if not TileInMapCoords(aLoc.X,aLoc.Y) then exit;
  case aPass of
    CanWalk,CanWalkRoad,CanMakeRoads,CanMakeFields,CanPlantTrees,CanCrab,CanWolf: Result := TestHeight(25);
    CanBuild,CanBuildGold,CanBuildIron: Result := TestHeight(20);
  end; //For other passabilities we ignore height (return default true)
end;


procedure TTerrain.AddHouseRemainder(Loc: TKMPoint; aHouseType: THouseType; aBuildState: THouseBuildState);
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
  UpdateWalkConnect([wcWalk, wcRoad, wcWolf, wcCrab, wcWork]);
end;


{Check 4 surrounding tiles, and if they are different place a border}
procedure TTerrain.UpdateBorders(Loc: TKMPoint; CheckSurrounding: Boolean = True);
  function GetBorderType: TBorderType;
  begin
    if TileIsCornField(Loc) then
      Result := bt_Field
    else
    if TileIsWineField(Loc) then
      Result := bt_Wine
    else
    if Land[Loc.Y,Loc.X].TileLock in [tlFenced, tlDigged] then
      Result := bt_HouseBuilding
    else
      Result := bt_None;
  end;
  function GetBorderEnabled(Loc2: TKMPoint): Boolean;
  begin
    Result := True;
    if not TileInMapCoords(Loc2.X,Loc2.Y) then exit;
    if (TileIsCornField(Loc) and TileIsCornField(Loc2))or //Both are Corn
       (TileIsWineField(Loc) and TileIsWineField(Loc2))or //Both are Wine
      ((Land[Loc.Y,Loc.X].TileLock in [tlFenced, tlDigged]) and (Land[Loc2.Y,Loc2.X].TileLock in [tlFenced, tlDigged])) then //Both are either house fence
      Result := False;
  end;
begin
 if not TileInMapCoords(Loc.X,Loc.Y) then exit;

  Land[Loc.Y,Loc.X].Border:=GetBorderType;
  if Land[Loc.Y,Loc.X].Border = bt_None then begin
    Land[Loc.Y,Loc.X].BorderTop    := false;
    Land[Loc.Y,Loc.X].BorderLeft   := false;
    Land[Loc.Y,Loc.X].BorderBottom := false;
    Land[Loc.Y,Loc.X].BorderRight  := false;
  end
  else begin
    Land[Loc.Y,Loc.X].BorderTop    := GetBorderEnabled(KMPoint(Loc.X,Loc.Y-1));
    Land[Loc.Y,Loc.X].BorderLeft   := GetBorderEnabled(KMPoint(Loc.X-1,Loc.Y));
    Land[Loc.Y,Loc.X].BorderBottom := GetBorderEnabled(KMPoint(Loc.X,Loc.Y+1));
    Land[Loc.Y,Loc.X].BorderRight  := GetBorderEnabled(KMPoint(Loc.X+1,Loc.Y));
  end;
  if CheckSurrounding then
  begin
    UpdateBorders(KMPoint(Loc.X-1,Loc.Y),false);
    UpdateBorders(KMPoint(Loc.X+1,Loc.Y),false);
    UpdateBorders(KMPoint(Loc.X,Loc.Y-1),false);
    UpdateBorders(KMPoint(Loc.X,Loc.Y+1),false);
  end;
end;


{ Returns a rounded vertex based cursor position, maybe we'll need it later. }
function TTerrain.GetVertexCursorPosition:TKMPoint;
begin
  Result.X := EnsureRange(round(GameCursor.Float.X + 1), 1, fMapX);
  Result.Y := EnsureRange(round(GameCursor.Float.Y + 1), 1, fMapY);
end;


{Cursor position should be converted to tile-coords respecting tile heights}
function TTerrain.ConvertCursorToMapCoord(inX,inY:single):single;
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


//Return height within cell interpolating node heights
//Note that input parameters are 0 based
function TTerrain.HeightAt(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  Tmp1, Tmp2: single;
begin
  //todo: Make this match KaM by creating some comparision screenshots of slopes, hills, etc.
  Xc := Trunc(inX);
  Yc := Trunc(inY);
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Xc, 0, fMapX-2);
  Yc := EnsureRange(Yc, 0, fMapY-2);

  Tmp1 := mix(Land[Yc+1, Xc+2].Height, Land[Yc+1, Xc+1].Height, frac(inX));
  Tmp2 := mix(Land[Yc+2, Xc+2].Height, Land[Yc+2, Xc+1].Height, frac(inX));
  Result := mix(Tmp2, Tmp1, frac(inY));
end;


function TTerrain.HeightAt(aPoint: TKMPointF): Single;
begin
  Result := HeightAt(aPoint.X, aPoint.Y);
end;


procedure TTerrain.MapEdHeight();
var
  I, K: Integer;
  Rad, Slope: Byte;
  Tmp: Single;
  R: TKMRect;
  aLoc : TKMPointF;
  aRaise: Boolean;
begin
  aLoc    := KMPointF(GameCursor.Float.X+1, GameCursor.Float.Y+1); // Mouse point
  aRaise  := ssLeft in GameCursor.SState;         // Raise or Lowered
  Rad     := GameCursor.MapEdSize;
  Slope   := GameCursor.MapEdSlope;

  for I := Max((round(aLoc.Y) - Rad), 1) to Min((round(aLoc.Y) + Rad), fMapY) do
  for K := Max((round(aLoc.X) - Rad), 1) to Min((round(aLoc.X) + Rad), fMapX) do
  begin
    case GameCursor.MapEdShape of
      hsCircle: Tmp := 1 - GetLength(I-aLoc.Y, K-aLoc.X) / Rad;
      hsSquare: Tmp := 1 - Math.max(abs(I-aLoc.Y), abs(K-aLoc.X)) / Rad;
      else      Tmp := 0;
    end;
    if GameCursor.Mode = cm_Equalize then
    begin
      if aRaise then   // Unequalize
      begin
        if (i > 1) and (k >1) and (i < fMapY-1) and (k < fMapX-1) then
        begin
          if (Land[I,K].Height < Land[I-1,K+1].Height) then
            Tmp := -min(Land[I-1,K+1].Height-Land[i,k].Height,tmp)
          else 
            if (Land[I,K].Height > Land[I-1,K+1].Height) then
              Tmp := min(Land[i,k].Height-Land[I-1,K+1].Height,tmp) 
            else
              Tmp := 0;
        end;
      end else         // Flatten
      begin
        if (Land[I,K].Height < Land[trunc(aLoc.Y),trunc(aLoc.X)].Height) then
          Tmp := -min(Land[trunc(aLoc.Y),trunc(aLoc.X)].Height-Land[i,k].Height,tmp)
        else 
          if (Land[I,K].Height > Land[trunc(aLoc.Y),trunc(aLoc.X)].Height) then
            Tmp := min(Land[i,k].Height-Land[trunc(aLoc.Y),trunc(aLoc.X)].Height,tmp) 
          else
            Tmp := 0;
      end;
    end;

    //Compute resulting floating-point height
    Tmp := power(abs(Tmp),(Slope+1)/6)*sign(Tmp); //Modify slopes curve
    Tmp := EnsureRange(Land[I,K].Height + Land[I,K].HeightAdd/255 + Math.max(0,Tmp) * (Byte(aRaise)*2-1), 0, 100);
    Land[I,K].Height := trunc(Tmp);
    Land[I,K].HeightAdd := round(frac(Tmp)*255); //write fractional part in 0..255 range (1Byte) to save us mem
  end;

  R := KMRectGrow(KMRect(aLoc), Rad);
  UpdateLighting(R);
  UpdatePassability(R);
end;


procedure TTerrain.MapEdTile(aLoc:TKMPoint; aTile, aRotation:byte);
begin
  if TileInMapCoords(aLoc.X, aLoc.Y) then
  begin
    Land[aLoc.Y, aLoc.X].Terrain := aTile;
    Land[aLoc.Y, aLoc.X].Rotation := aRotation;
    UpdatePassability(aLoc);
  end;
end;


procedure TTerrain.IncAnimStep;
begin
  inc(fAnimStep);
end;


procedure TTerrain.Save(SaveStream: TKMemoryStream);
var i,k:integer;
begin
  Assert(not fMapEditor, 'MapEd mode is not intended to be saved into savegame');

  SaveStream.Write('Terrain');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fAnimStep);

  FallingTrees.SaveToStream(SaveStream);

  for i:=1 to fMapY do for k:=1 to fMapX do
  begin
    //Only save fields that cannot be recalculated after loading
    SaveStream.Write(Land[i,k].Terrain);
    SaveStream.Write(Land[i,k].Height);
    SaveStream.Write(Land[i,k].Rotation);
    SaveStream.Write(Land[i,k].Obj);
    SaveStream.Write(Land[i,k].TreeAge);
    SaveStream.Write(Land[i,k].FieldAge);
    SaveStream.Write(Land[i,k].TileLock,SizeOf(Land[i,k].TileLock));
    SaveStream.Write(Land[i,k].TileOverlay,SizeOf(Land[i,k].TileOverlay));
    SaveStream.Write(Land[i,k].TileOwner,SizeOf(Land[i,k].TileOwner));
    if Land[i,k].IsUnit <> nil then
      SaveStream.Write(TKMUnit(Land[i,k].IsUnit).ID) //Store ID, then substitute it with reference on SyncLoad
    else
      SaveStream.Write(Integer(0));
    SaveStream.Write(Land[i,k].IsVertexUnit, SizeOf(Land[i,k].IsVertexUnit));
  end;
end;


procedure TTerrain.Load(LoadStream: TKMemoryStream);
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
    UpdateBorders(KMPoint(k,i), False);

  UpdateLighting(KMRect(1, 1, fMapX, fMapY));
  UpdatePassability(KMRect(1, 1, fMapX, fMapY));

  UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWolf, wcCrab, wcWork]);

  fLog.AppendLog('Terrain loaded');
end;


procedure TTerrain.SyncLoad;
var
  I, K: Integer;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      Land[I,K].IsUnit := fPlayers.GetUnitByID(Cardinal(Land[I,K].IsUnit));
end;


{ This whole thing is very CPU intesive, think of it - to update whole (192*192) tiles map }
//Don't use any advanced math here, only simpliest operations - + div *
procedure TTerrain.UpdateState;
  procedure SetLand(X, Y, aTile, aObj: Byte);
  begin
    Land[Y,X].Terrain := aTile;
    Land[Y,X].Obj     := aObj;
  end;
var
  H, I, J, K: Word;
  T: Integer;
begin
  inc(fAnimStep);

  for T := FallingTrees.Count - 1 downto 0 do
  if fAnimStep - FallingTrees.Tag2[T]+1 >= MapElem[FallingTrees.Tag[T]+1].Count then
    ChopTree(FallingTrees[T]); //Make the tree turn into a stump

  for I := 1 to fMapY do
  for K := 1 to fMapX do
  //All those global things can be performed once a sec, or even less frequent
  if (I*fMapX+K+fAnimStep) mod TERRAIN_PACE = 0 then
  begin

    if InRange(Land[I,K].FieldAge, 1, 65534) then
    begin
      Inc(Land[I,K].FieldAge);
      if TileIsCornField(KMPoint(K,I)) then
        case Land[I,K].FieldAge of
          CORN_AGE_1:     SetLand(K,I,59,255);
          CORN_AGE_2:     SetLand(K,I,60,58);
          CORN_AGE_FULL:  begin
                            //Skip to the end
                            SetLand(K,I,60,59);
                            Land[I,K].FieldAge := 65535;
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
                            Land[I,K].FieldAge := 65535;
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
          for J := 1 to 3 do
            if Land[I,K].Obj = ChopableTrees[H,J] then
              case Land[I,K].TreeAge of
                TREE_AGE_1:    Land[I,K].Obj := ChopableTrees[H,2];
                TREE_AGE_2:    Land[I,K].Obj := ChopableTrees[H,3];
                TREE_AGE_FULL: Land[I,K].Obj := ChopableTrees[H,4];
              end;
    end;
  end;
end;


//Only MapEd accesses it
procedure TTerrain.UpdateStateIdle;
begin
  case GameCursor.Mode of
    cm_Elevate,
    cm_Equalize:  if (ssLeft in GameCursor.SState) or (ssRight in GameCursor.SState) then
                    MapEdHeight;
    cm_Tiles:     if (ssLeft in GameCursor.SState) then
                    if GameCursor.MapEdDir in [0..3] then //Defined direction
                      MapEdTile(GameCursor.Cell, GameCursor.Tag1, GameCursor.MapEdDir)
                    else //Random direction
                      MapEdTile(GameCursor.Cell, GameCursor.Tag1, KaMRandom(4));
  end;
end;


end.
