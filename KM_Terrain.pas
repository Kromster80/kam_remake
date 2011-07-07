unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
     KM_Defaults, KM_CommonTypes, KM_Player, KM_Units, KM_Units_Warrior, KM_Utils, KM_Houses;


const MAX_MAP_SIZE = 192;


type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
  private
    fAnimStep:integer;
  public
    MapX,MapY:integer; //Terrain width and height

    Land:array[1..MAX_MAP_SIZE,1..MAX_MAP_SIZE]of record
      Terrain:byte;
      Height:byte;
      Rotation:byte;
      Obj:byte;

      //Age of tree, another independent variable since trees can grow on fields
      TreeAge:word; //Not init=0 .. Full=TreeAgeFull Depending on this tree gets older and thus could be chopped

      //Age of field/wine, another independent variable
      FieldAge:word; //Empty=0, 1, 2, 3, 4, Full=65535  Depending on this special object maybe rendered (straw, grapes)

      //Visible for all players, HouseWIP is not a markup in fact, but it fits well in here, so let it be here
      Markup:TMarkup; //Markup (ropes) used on-top of tiles for roads/fields/houseplan/housearea

      //Used to display half-dug road
      TileOverlay:TTileOverlay; //fs_None fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4 +Roads

      TileOwner:TPlayerIndex; //Who owns the tile by having a house/road/field on it
      IsUnit:TKMUnit; //Whenever there's a unit on that tile mark the tile as occupied and count the number
      IsVertexUnit:TKMVertexUsage; //Whether there are units blocking the vertex. (walking diagonally or fighting)


      //MAPEDITOR
      OldTerrain, OldRotation:byte; //Only used for map editor
      HeightAdd:byte; //Fraction part of height, for smooth height editing


      //DEDUCTED 
      Light:single; //KaM stores node lighting in 0..32 range (-16..16), but I want to use -1..1 range
      Passability:TPassabilitySet; //Meant to be set of allowed actions on the tile

      WalkConnect:array[TWalkConnect]of byte; //Whole map is painted into interconnected areas 1=CanWalk, 2=CanWalkRoad, 3=CanFish, 4=CanWalkAvoid: walk avoiding tiles under construction, only recalculated when needed

      Border: TBorderType; //Borders (ropes, planks, stones)
      BorderTop, BorderLeft, BorderBottom, BorderRight:boolean; //Whether the borders are enabled
    end;

    FallingTrees: TKMPointTagList;
    MiniMapRGB:array[1..MAX_MAP_SIZE,1..MAX_MAP_SIZE]of cardinal;

    constructor Create;
    destructor Destroy; override;
    procedure MakeNewMap(Width,Height:integer);
    function LoadFromFile(FileName:string):boolean;

    procedure SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
    procedure SetRoad(Loc:TKMPoint; aOwner:TPlayerIndex);
    procedure SetRoads(aList:TKMPointList; aOwner:TPlayerIndex);
    procedure SetField(Loc:TKMPoint; aOwner:TPlayerIndex; aFieldType:TFieldType);
    procedure SetHouse(Loc:TKMPoint; aHouseType: THouseType; aHouseStage:THouseStage; aOwner:TPlayerIndex; const aFlattenTerrain:boolean=false);
    procedure SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);

    procedure RemovePlayer(aPlayer:TPlayerIndex);
    procedure RemMarkup(Loc:TKMPoint);
    procedure RemRoad(Loc:TKMPoint);
    procedure RemField(Loc:TKMPoint);
    procedure SetWall(Loc:TKMPoint; aOwner:TPlayerIndex);
    procedure IncDigState(Loc:TKMPoint);
    procedure ResetDigState(Loc:TKMPoint);

    function CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType; aAllowCitizensOffRoad:boolean=true):boolean;
    function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; aPlayer:TKMPlayer):boolean;
    function CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup; aPlayer:TKMPlayer):boolean;
    function CheckHeightPass(aLoc:TKMPoint; aPass:TPassability):boolean;
    procedure AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);

    function FindField(aPosition:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean; aAvoidLoc:TKMPoint):TKMPoint;
    function FindTree(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPointDir;
    function FindStone(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPoint;
    function FindOre(aPosition:TKMPoint; Rt:TResourceType):TKMPoint;
    function FindPlaceForTree(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPoint;
    function FindFishWater(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPointDir;
    function CanFindFishingWater(aPosition:TKMPoint; aRadius:integer):boolean;
    function ChooseTreeToPlant(aPosition:TKMPoint):integer;

    function WaterHasFish(aPosition:TKMPoint):boolean;
    function CatchFish(aPosition:TKMPointDir; TestOnly:boolean=false):boolean;

    procedure SetTree(Loc:TKMPoint; ID:integer);
    procedure FallTree(Loc:TKMPoint);
    procedure ChopTree(Loc:TKMPoint);

    procedure SowCorn(Loc:TKMPoint);
    procedure CutCorn(Loc:TKMPoint);
    procedure CutGrapes(Loc:TKMPoint);

    procedure SetResourceDeposit(Loc:TKMPoint; rt:TResourceType);
    procedure DecStoneDeposit(Loc:TKMPoint);
    function DecOreDeposit(Loc:TKMPoint; rt:TResourceType):boolean;

    procedure RecalculatePassability(Loc:TKMPoint);
    procedure RecalculatePassabilityAround(Loc:TKMPoint);
    function CheckPassability(Loc:TKMPoint; aPass:TPassability):boolean;
    function HasUnit(Loc:TKMPoint):boolean;
    function HasVertexUnit(Loc:TKMPoint):boolean;
    function GetRoadConnectID(Loc:TKMPoint):byte;
    function GetWalkConnectID(Loc:TKMPoint):byte;
    function GetConnectID(aWalkConnect: TWalkConnect; Loc:TKMPoint):byte;

    function CheckAnimalIsStuck(Loc:TKMPoint; aPass:TPassability; aCheckUnits:boolean=true):boolean;
    function GetOutOfTheWay(Loc, Loc2:TKMPoint; aPass:TPassability):TKMPoint;
    function FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; OnlyTakeBest: boolean=false):TKMPoint;
    function Route_CanBeMade(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
    function Route_CanBeMadeToVertex(LocA, LocB:TKMPoint; aPass:TPassability):boolean;
    function Route_CanBeMadeToHouse(LocA:TKMPoint; aHouse:TKMHouse; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
    function Route_MakeAvoid(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aHouse:TKMHouse; var NodeList:TKMPointList):boolean;
    procedure Route_Make(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aHouse:TKMHouse; var NodeList:TKMPointList);
    procedure Route_ReturnToRoad(LocA, LocB:TKMPoint; TargetRoadNetworkID:byte; var NodeList:TKMPointList);
    procedure Route_ReturnToWalkable(LocA, LocB:TKMPoint; TargetWalkNetworkID:byte; var NodeList:TKMPointList);
    function GetClosestTile(TargetLoc, OriginLoc:TKMPoint; aPass:TPassability):TKMPoint;

    procedure UnitAdd(LocTo:TKMPoint; aUnit:TKMUnit);
    procedure UnitRem(LocFrom:TKMPoint);
    procedure UnitWalk(LocFrom,LocTo:TKMPoint; aUnit:TKMUnit);
    procedure UnitSwap(LocFrom,LocTo:TKMPoint; UnitFrom:TKMUnit);
    procedure UnitVertexAdd(LocTo:TKMPoint; Usage: TKMVertexUsage); overload;
    procedure UnitVertexAdd(LocFrom, LocTo:TKMPoint); overload;
    procedure UnitVertexRem(LocFrom:TKMPoint);
    function VertexUsageCompatible(LocFrom, LocTo:TKMPoint): boolean;
    function GetVertexUsageType(LocFrom, LocTo:TKMPoint): TKMVertexUsage;

    function TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
    function VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
    function EnsureTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;

    function TileIsWater(Loc:TKMPoint):boolean;
    function TileIsSand(Loc:TKMPoint):boolean;
    function TileIsStone(Loc:TKMPoint):byte;
    function TileIsSoil(Loc:TKMPoint):boolean;
    function TileIsWalkable(Loc:TKMPoint):boolean;
    function TileIsRoadable(Loc:TKMPoint):boolean;
    function TileIsCornField(Loc:TKMPoint):boolean;
    function TileIsWineField(Loc:TKMPoint):boolean;
    function TileIsLocked(aLoc:TKMPoint):boolean;
    function UnitsHitTest(X,Y:word):TKMUnit;
    function UnitsHitTestWithinRad(aLoc:TKMPoint; MinRad, MaxRad:single; aPlayer:TPlayerIndex; aAlliance:TAllianceType; Dir:TKMDirection): TKMUnit;

    function ObjectIsChopableTree(Loc:TKMPoint; Stage:byte):boolean;
    function CanWalkDiagonaly(A,B:TKMPoint):boolean;
    function FindNewNode(A,B:TKMPoint; aPass:TPassability):TKMPoint;

    procedure UpdateBorders(Loc:TKMPoint; CheckSurrounding:boolean=true);
    procedure FlattenTerrain(Loc:TKMPoint); overload;
    procedure FlattenTerrain(LocList:TKMPointList); overload;
    procedure RebuildLighting(LowX,HighX,LowY,HighY:integer);
    procedure RebuildPassability(LowX,HighX,LowY,HighY:integer);
    procedure RebuildWalkConnect(wcType:TWalkConnect);

    procedure ComputeCursorPosition(X,Y:word; Shift: TShiftState);
    function GetVertexCursorPosition:TKMPoint;
    function ConvertCursorToMapCoord(inX,inY:single):single;
    function InterpolateLandHeight(inX,inY:single):single; overload;
    function InterpolateLandHeight(aPoint:TKMPointF):single; overload;
    function MixLandHeight(inX,inY:byte):byte;

    procedure MapEdHeight(aLoc:TKMPointF; aSize, aShape:byte; aRaise:boolean);
    procedure MapEdTile(aLoc:TKMPoint; aTile,aRotation:byte);

    procedure RefreshMinimapData;

    procedure IncAnimStep; //Lite-weight UpdaState for MapEd
    property AnimStep: integer read fAnimStep;
    procedure SaveToMapFile(aFile:string);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure UpdateStateIdle;
    procedure Paint;
  end;

var
  fTerrain: TTerrain;

implementation

uses KM_Viewport, KM_Render, KM_PlayersCollection, KM_Sound, KM_PathFinding, KM_UnitActionStay, KM_Game;

constructor TTerrain.Create;
begin
  Inherited;
  fAnimStep:=0;
  FallingTrees := TKMPointTagList.Create;
end;


destructor TTerrain.Destroy;
begin
  FreeAndNil(FallingTrees);
  Inherited;
end;


//Reset whole map with default values
procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k:integer;
begin
  MapX := min(Width, MAX_MAP_SIZE);
  MapY := min(Height,MAX_MAP_SIZE);

  for i:=1 to MapY do for k:=1 to MapX do with Land[i,k] do begin
    Terrain      := 0;
    Height       := random(7);    //variation in height
    Rotation     := random(4);  //Make it random
    OldTerrain   := 0;
    OldRotation  := 0;
    Obj          := 255;             //none
    if Random(16)=0 then Obj := ChopableTrees[Random(13)+1,4];
    TileOverlay  := to_None;
    Markup       := mu_None;
    Passability  := []; //Gets recalculated later
    TileOwner    := -1;
    IsUnit       := nil;
    IsVertexUnit := vu_None;
    FieldAge     := 0;
    TreeAge      := 0;
    if ObjectIsChopableTree(KMPoint(k,i),4) then TreeAge := TreeAgeFull;
    Border       := bt_None;
    BorderTop    := false;
    BorderLeft   := false;
    BorderBottom := false;
    BorderRight  := false;
  end;

  RebuildLighting(1,MapX,1,MapY);
  RebuildPassability(1,MapX,1,MapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcFish);
end;


function TTerrain.LoadFromFile(FileName:string):boolean;
var
  i,k:integer;
  S:TKMemoryStream;
  NewX,NewY:integer;
begin
  Result := false;
  if not CheckFileExists(FileName) then exit;
  fLog.AppendLog('Loading map file: '+FileName);

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX <= MAX_MAP_SIZE) and (NewY <= MAX_MAP_SIZE), 'Can''t open the map cos it has too big dimensions');
    MapX := NewX;
    MapY := NewY;
    MakeNewMap(MapX, MapY); //Reset whole map to default
    for i:=1 to MapY do for k:=1 to MapX do
    begin
      S.Read(Land[i,k].Terrain); //1
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Height); //3
      S.Read(Land[i,k].Rotation); //4
      S.Seek(1, soFromCurrent);
      S.Read(Land[i,k].Obj); //6
      S.Seek(17, soFromCurrent);
      if ObjectIsChopableTree(KMPoint(k,i),1) then Land[i,k].TreeAge := 1;
      if ObjectIsChopableTree(KMPoint(k,i),2) then Land[i,k].TreeAge := TreeAge1;
      if ObjectIsChopableTree(KMPoint(k,i),3) then Land[i,k].TreeAge := TreeAge2;
      if ObjectIsChopableTree(KMPoint(k,i),4) then Land[i,k].TreeAge := TreeAgeFull;
      //Everything else is default
    end;
  finally
    S.Free;
  end;

  RebuildLighting(1,MapX,1,MapY);
  RebuildPassability(1,MapX,1,MapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcFish);
  fLog.AppendLog('Map file loaded');

  Result := true;
end;


//Save (export) map in KaM .map format with additional tile information on the end?
procedure TTerrain.SaveToMapFile(aFile:string);
var f:file; i,k,t,u:integer;
    ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
    Res:array[1..MAX_MAP_SIZE*2]of packed record X1,Y1,X2,Y2:integer; Typ:byte; end;
begin

  if not DirectoryExists(ExtractFilePath(aFile)) then
  CreateDir(ExtractFilePath(aFile));

  assignfile(f,aFile); rewrite(f,1);

  blockwrite(f,MapX,4);
  blockwrite(f,MapY,4);

  t := 0;
  u := 255;
  for i:=1 to MapY do for k:=1 to MapX do
  begin
    if TileIsCornField(KMPoint(k,i)) or TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,Land[i,k].OldTerrain,1) //Map file stores terrain, not the fields placed over it, so save OldTerrain rather than Terrain
    else
      blockwrite(f,Land[i,k].Terrain,1);

    blockwrite(f,t,1); //Light
    blockwrite(f,Land[i,k].Height,1);

    if TileIsCornField(KMPoint(k,i)) or TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,Land[i,k].OldRotation,1) //Map file stores terrain, not the fields placed over it, so save OldRotation rather than Rotation
    else
      blockwrite(f,Land[i,k].Rotation,1);

    blockwrite(f,t,1); //unknown

    //Don't save winefield objects as they are part of the DAT not map
    if TileIsWineField(KMPoint(k,i)) then
      blockwrite(f,u,1)
    else
      blockwrite(f,Land[i,k].Obj,1);

    blockwrite(f,t,1); //Passability?

    blockwrite(f,t,4); //unknown
    blockwrite(f,t,4); //unknown
    blockwrite(f,t,4); //unknown
    blockwrite(f,t,4); //unknown
  end;

  
  //@Krom: Temporary fix to make the maps compatible with KaM. I do not understand the resource footer
  //       If you would like to write it properly please feel free to do so

  ResHead.x1:=0;
  ResHead.Allocated:=MapX+MapY;
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
  i := 4 + MapY*MapX;
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
  Result := InRange(X,1+Inset,MapX-1-Inset) and InRange(Y,1+Inset,MapY-1-Inset);
end;


{Check if requested vertice is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,MapX-Inset) and InRange(Y,1+Inset,MapY-Inset);
end;


{Ensure that requested tile is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.EnsureTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;
begin
  Result.X := EnsureRange(X,1+Inset,MapX-1-Inset);
  Result.Y := EnsureRange(Y,1+Inset,MapY-1-Inset);
end;


{Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed}
function TTerrain.TileIsWater(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [48,114,115,119,192,193,194,196, 200, 208..211, 235,236, 240,244];
end;


{Check if requested tile is sand suitable for crabs}
function TTerrain.TileIsSand(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [31..33, 70,71, 99,100,102,103, 108,109, 112,113, 116,117, 169, 173, 181, 189];
end;


{Check if requested tile is Stone and returns Stone deposit}
function TTerrain.TileIsStone(Loc:TKMPoint):byte;
begin
  //Should be Tileset property, especially if we allow different tilesets
  case Land[Loc.Y,Loc.X].Terrain of
    132,137: Result:=5;
    131,136: Result:=4;
    130,135: Result:=3;
    129,134: Result:=2;
    128,133: Result:=1;
    else     Result:=0;
  end;
end;


{Check if requested tile is soil suitable for fields and trees}
function TTerrain.TileIsSoil(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..28, 34..39, 47, 49, 56,57,58, 64..69, 72..80, 84..87, 88,89, 93..98,180,182..183,188,190..191,220,247];
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
  Result:=PatternDAT[Land[Loc.Y,Loc.X].Terrain+1].Walkable <> 0;
end;

{Check if requested tile is generally suitable for road building}
function TTerrain.TileIsRoadable(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  //Do not include 1/2 and 1/4 walkable as roadable
  Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..31, 32..39, 45..47, 49, 52, 55, 56..63,
                                          64..71, 72..79, 80..87, 88..95, 96..103, 104,108,111, 112,113,
                                          152..155,180..183,188..191,
                                          203..205, 212,213,215, 220, 247];
end;


{Check if the tile is a corn field}
function TTerrain.TileIsCornField(Loc:TKMPoint):boolean;
begin
  Result := Land[Loc.Y,Loc.X].Terrain in [59..63];
  Result := Result and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road); //Can't be if there is road here
end;


{Check if the tile is a wine field}
function TTerrain.TileIsWineField(Loc:TKMPoint):boolean;
begin
  Result := Land[Loc.Y,Loc.X].Terrain in [55];
  Result := Result and (Land[Loc.Y,Loc.X].TileOverlay <> to_Road); //Can't be if there is road here
  Result := Result and (Land[Loc.Y,Loc.X].Obj in [54..57]); //Must have object (e.g. for init when labourer is building it)
end;


function TTerrain.TileIsLocked(aLoc:TKMPoint):boolean;
begin
  Result := false;
  if (Land[aLoc.Y,aLoc.X].IsUnit <> nil) then begin
    if Land[aLoc.Y,aLoc.X].IsUnit.GetUnitAction=nil then
    Assert(Land[aLoc.Y,aLoc.X].IsUnit.GetUnitAction<>nil);
    Result := (Land[aLoc.Y,aLoc.X].IsUnit.GetUnitAction.Locked);
  end;
end;


//Check if there's unit on the tile
//Note that IsUnit refers to where unit started walking to, not the actual unit position
//(which is what we used in unit interaction), so check all 9 tiles to get accurate result
function TTerrain.UnitsHitTest(X,Y:word):TKMUnit;
var i,k:integer;
begin
  Result := nil;
  for i:=max(Y-1,1) to min(Y+1,MapY) do for k:=max(X-1,1) to min(X+1,MapX) do
  if (Land[i,k].IsUnit <> nil) and (Land[i,k].IsUnit.HitTest(X,Y)) then
    Result := Land[i,k].IsUnit;
end;


//Function to use with WatchTowers/Archers/AutoLinking/
{ Should scan withing given radius and return closest unit with given Alliance status
  Should be optimized versus usual UnitsHitTest
  Prefer Warriors over Citizens}
function TTerrain.UnitsHitTestWithinRad(aLoc:TKMPoint; MinRad, MaxRad:single; aPlayer:TPlayerIndex; aAlliance:TAllianceType; Dir:TKMDirection): TKMUnit;
var
  i,k:integer; //Counters
  lx,ly,hx,hy:integer; //Ranges
  dX,dY:integer;
  U,C,W:TKMUnit; //CurrentUnit, BestWarrior, BestCitizen
begin
  W := nil;
  C := nil;

  //Scan one tile further than the maximum radius due to rounding
  lx := max(round(aLoc.X-(MaxRad+1)),1); //1.42 gets rounded to 1
  ly := max(round(aLoc.Y-(MaxRad+1)),1); //1.42 gets rounded to 1
  hx := min(round(aLoc.X+(MaxRad+1)),MapX); //1.42 gets rounded to 1
  hy := min(round(aLoc.Y+(MaxRad+1)),MapY); //1.42 gets rounded to 1

  for i:=ly to hy do for k:=lx to hx do
  if InRange(GetLength(aLoc, KMPoint(k,i)), MinRad, MaxRad)
    and (fPlayers.Player[aPlayer].FogOfWar.CheckTileRevelation(k,i) = 255)
    and (Land[i,k].IsUnit <> nil)
    and (Land[i,k].IsUnit.HitTest(k,i)) then //Unit is actually on the tile
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

    if (U <> nil) and
       U.Visible and //Inside of house
       CanWalkDiagonaly(aLoc,KMPoint(k,i)) and
       (fPlayers.CheckAlliance(aPlayer, U.GetOwner) = aAlliance) and //How do WE feel about enemy, not how they feel about us
       ((abs(aLoc.X - k) <> 1) or (abs(aLoc.Y - i) <> 1) or VertexUsageCompatible(aLoc,KMPoint(k,i))) and
       (not U.IsDeadOrDying)
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


{Find new node inbetween A and B}
function TTerrain.FindNewNode(A,B:TKMPoint; aPass:TPassability):TKMPoint;
var Options:TKMPointList;
  function CheckTile(aA,aB:smallint):TKMPoint;
  begin
    if CheckPassability(KMPoint(aA,aB),aPass) then
      Result := KMPoint(aA,aB)
    else
      Result := KMPoint(0,0);
  end;
begin
  Options := TKMPointList.Create;

  if (abs(A.X-B.X)=1) and (abs(A.Y-B.Y)=1) then //Diagonal
  begin
    if CheckPassability(KMPoint(A.X,B.Y),aPass) then Options.AddEntry(KMPoint(A.X,B.Y));
    if CheckPassability(KMPoint(B.X,A.Y),aPass) then Options.AddEntry(KMPoint(B.X,A.Y));
  end;

  if (A.X=B.X) and (abs(A.Y-B.Y)=1) then //Vertical
  begin
    if CheckPassability(KMPoint(A.X-1,A.Y),aPass) then Options.AddEntry(KMPoint(A.X-1,A.Y));
    if CheckPassability(KMPoint(A.X-1,B.Y),aPass) then Options.AddEntry(KMPoint(A.X-1,B.Y));
    if CheckPassability(KMPoint(A.X+1,A.Y),aPass) then Options.AddEntry(KMPoint(A.X+1,A.Y));
    if CheckPassability(KMPoint(A.X+1,B.Y),aPass) then Options.AddEntry(KMPoint(A.X+1,B.Y));
  end;

  if (abs(A.X-B.X)=1) and (A.Y=B.Y) then //Horizontal
  begin
    if CheckPassability(KMPoint(A.X,A.Y-1),aPass) then Options.AddEntry(KMPoint(A.X,A.Y-1));
    if CheckPassability(KMPoint(B.X,A.Y-1),aPass) then Options.AddEntry(KMPoint(B.X,A.Y-1));
    if CheckPassability(KMPoint(A.X,A.Y+1),aPass) then Options.AddEntry(KMPoint(A.X,A.Y+1));
    if CheckPassability(KMPoint(B.X,A.Y+1),aPass) then Options.AddEntry(KMPoint(B.X,A.Y+1));
  end;

  Result := Options.GetRandom;
  Options.Free;
end;


{Place markup on tile, any new markup replaces old one, thats okay}
procedure TTerrain.SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
begin
  Land[Loc.Y,Loc.X].Markup:=aMarkup;
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcWalk); //Markups affect passability so therefore also floodfill
  RebuildWalkConnect(wcRoad);
end;


{Remove markup from tile}
procedure TTerrain.RemMarkup(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].Markup:=mu_None;
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcWalk); //Markups affect passability so therefore also floodfill
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.SetRoad(Loc:TKMPoint; aOwner:TPlayerIndex);
begin
  Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  Land[Loc.Y,Loc.X].TileOverlay:=to_Road;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  UpdateBorders(Loc);
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.SetRoads(aList:TKMPointList; aOwner:TPlayerIndex);
var i:integer; TL,BR:TKMPoint;
begin
  if aList.Count = 0 then exit; //Nothing to be done

  for i:=1 to aList.Count do begin
    Land[aList.List[i].Y,aList.List[i].X].TileOwner:=aOwner;
    Land[aList.List[i].Y,aList.List[i].X].TileOverlay:=to_Road;
    Land[aList.List[i].Y,aList.List[i].X].FieldAge:=0;
    UpdateBorders(aList.List[i]);
  end;
  
  TL := aList.GetTopLeft;
  BR := aList.GetBottomRight;
  RebuildPassability(TL.X-1, BR.X+1, TL.Y-1, BR.Y+1);
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.RemRoad(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOwner:=-1;
  Land[Loc.Y,Loc.X].TileOverlay:=to_None;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  UpdateBorders(Loc);
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.RemField(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TileOwner:=-1;
  Land[Loc.Y,Loc.X].TileOverlay:=to_None;
  Land[Loc.Y,Loc.X].Terrain := Land[Loc.Y,Loc.X].OldTerrain; //Reset terrain
  Land[Loc.Y,Loc.X].Rotation := Land[Loc.Y,Loc.X].OldRotation; //Reset terrain
  if Land[Loc.Y,Loc.X].Obj in [54..59] then Land[Loc.Y,Loc.X].Obj := 255; //Remove corn/wine
  Land[Loc.Y,Loc.X].FieldAge:=0;
  UpdateBorders(Loc);
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.RemovePlayer(aPlayer:TPlayerIndex);
var i,k:word;
begin
  for i:=1 to MapY do for k:=1 to MapX do
    if Land[i,k].TileOwner > aPlayer then
      Land[i,k].TileOwner := pred(Land[i,k].TileOwner)
    else
    if Land[i,k].TileOwner = aPlayer then
      Land[i,k].TileOwner := -1;
end;


procedure TTerrain.SetWall(Loc:TKMPoint; aOwner:TPlayerIndex);
begin
  Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  Land[Loc.Y,Loc.X].TileOverlay:=to_Wall;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  UpdateBorders(Loc);
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcRoad);
end;


{Set field on tile - corn/wine}
procedure TTerrain.SetField(Loc:TKMPoint; aOwner:TPlayerIndex; aFieldType:TFieldType);
begin
  Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  Land[Loc.Y,Loc.X].TileOverlay:=to_None;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  Land[Loc.Y,Loc.X].OldTerrain:=Land[Loc.Y,Loc.X].Terrain;
  Land[Loc.Y,Loc.X].OldRotation:=Land[Loc.Y,Loc.X].Rotation;

  if aFieldType=ft_Corn then begin
    Land[Loc.Y,Loc.X].Terrain:=62;
    Land[Loc.Y,Loc.X].Rotation:=0;
    //If object is already corn then set the field age (some maps start with corn placed)
    if fGame.GameState <> gsEditor then //Don't do this in editor mode
    case Land[Loc.Y,Loc.X].Obj of
      58: begin  //Corn 1
            Land[Loc.Y,Loc.X].FieldAge := 435;
            Land[Loc.Y,Loc.X].Terrain := 60;
          end;
      59: begin  //Corn 1
            Land[Loc.Y,Loc.X].FieldAge := 630;
            Land[Loc.Y,Loc.X].Terrain := 60;
          end;
    end;
  end else
  if (aFieldType=ft_Wine) or (aFieldType=ft_InitWine) then begin
    Land[Loc.Y,Loc.X].Terrain  := 55;
    Land[Loc.Y,Loc.X].Rotation := 0;
  end;
  if aFieldType=ft_Wine then
    CutGrapes(Loc);

  UpdateBorders(Loc);
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
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


{ Should find closest field around}
{aAgeFull is used for ft_Corn. Incase Farmer is looking for empty or full field of corn}
function TTerrain.FindField(aPosition:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean; aAvoidLoc:TKMPoint):TKMPoint;
var i,k:integer; List:TKMPointList;
begin
  List := TKMPointList.Create;
  for i:=max(aPosition.Y-aRadius,1) to min(aPosition.Y+aRadius,MapY-1) do
  for k:=max(aPosition.X-aRadius,1) to min(aPosition.X+aRadius,MapX-1) do
    if (KMLength(aPosition,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if ((aFieldType=ft_Corn) and TileIsCornField(KMPoint(k,i)))or
         ((aFieldType=ft_Wine) and TileIsWineField(KMPoint(k,i))) then
        if ((aAgeFull)and(Land[i,k].FieldAge=65535))or
           ((not aAgeFull)and(Land[i,k].FieldAge=0)) then
          if Route_CanBeMade(aPosition,KMPoint(k,i),CanWalk,0,false) then
            List.AddEntry(KMPoint(k,i));

  Result := List.GetRandom;
  List.Free;
end;


{Find closest chopable Tree around}
function TTerrain.FindTree(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPointDir;
const Ins=2; //2..Map-2
var i,k,Best:integer; List:TKMPointList; TreeLoc: TKMPoint;
begin
  //List1 is all trees within radius
  List:=TKMPointList.Create;
  for i:=max(aPosition.Y-aRadius,Ins) to min(aPosition.Y+aRadius,MapY-Ins) do
  for k:=max(aPosition.X-aRadius,Ins) to min(aPosition.X+aRadius,MapX-Ins) do
    if (KMLength(aPosition,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if ObjectIsChopableTree(KMPoint(k,i),4)and(Land[i,k].TreeAge>=TreeAgeFull) then //Grownup tree
        if Route_CanBeMadeToVertex(aPosition,KMPoint(k,i),CanWalk) then
          List.AddEntry(KMPoint(k,i));

  TreeLoc := List.GetRandom; //Choose our tree
  List.Free;

  //Now choose our direction of approch based on which one is the flatest (animation looks odd if not flat)
  Best := 255;
  Result := KMPointDir(0,0,0);

  //Only bother choosing direction if tree is valid, otherwise just exit with invalid
  if not KMSamePoint(TreeLoc,KMPoint(0,0)) then
  for i:=-1 to 0 do for k:=-1 to 0 do
    if ((i=0)and(k=0)) or Route_CanBeMade(aPosition,KMPoint(TreeLoc.X+k,TreeLoc.Y+i),CanWalk,0,false) then
      if (abs(MixLandHeight(TreeLoc.X+k,TreeLoc.Y+i)-Land[TreeLoc.Y,TreeLoc.X].Height) < Best) and
        ((i<>0)or(MixLandHeight(TreeLoc.X+k,TreeLoc.Y+i)-Land[TreeLoc.Y,TreeLoc.X].Height >= 0)) then
      begin
        Result := KMPointDir(TreeLoc.X+k,TreeLoc.Y+i,byte(KMGetVertexDir(k,i))-1);
        Best := abs(Round(MixLandHeight(TreeLoc.X+k,TreeLoc.Y+i))-Land[TreeLoc.Y,TreeLoc.X].Height);
      end;
end;


{Find closest harvestable deposit of Stone}
{Return walkable tile below Stone deposit}
function TTerrain.FindStone(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPoint;
const Ins=2; //2..Map-2
var i,k:integer; List:TKMPointList;
begin
  List := TKMPointList.Create;
  for i:=max(aPosition.Y-aRadius,Ins) to min(aPosition.Y+aRadius,MapY-Ins-1) do //Leave one more tile below, where Stoncutter will stand
  for k:=max(aPosition.X-aRadius,Ins) to min(aPosition.X+aRadius,MapX-Ins) do
    if (KMLength(aPosition,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i+1)) then
      if (TileIsStone(KMPoint(k,i))>0) then
        if (CanWalk in Land[i+1,k].Passability) then //Now check the tile right below
          if Route_CanBeMade(aPosition,KMPoint(k,i+1),CanWalk,0,false) then
            List.AddEntry(KMPoint(k,i+1));

  Result := List.GetRandom;
  List.Free;
end;


function TTerrain.FindOre(aPosition:TKMPoint; Rt:TResourceType):TKMPoint;
var i,k,RadLeft,RadRight,RadTop,RadBottom,R1,R2,R3,R4:integer; L:array[1..4]of TKMPointList;
begin
  if not (Rt in [rt_IronOre, rt_GoldOre, rt_Coal]) then
    fGame.GameError(aPosition, 'Wrong resource as Ore');

  for i:=1 to 4 do L[i]:=TKMPointList.Create; //4 densities

  case Rt of
    rt_GoldOre: begin RadLeft:=7; RadRight:=6; RadTop:=11; RadBottom:=2; R1:=144; R2:=145; R3:=146; R4:=147; end;
    rt_IronOre: begin RadLeft:=7; RadRight:=5; RadTop:=11; RadBottom:=2; R1:=148; R2:=149; R3:=150; R4:=151; end;
    rt_Coal:    begin RadLeft:=4; RadRight:=5; RadTop:= 5; RadBottom:=2; R1:=152; R2:=153; R3:=154; R4:=155; end;
    else        begin RadLeft:=0; RadRight:=0; RadTop:= 0; RadBottom:=0; R1:=  0; R2:=  0; R3:=  0; R4:=  0; end;
  end;

  for i:=max(aPosition.Y-RadTop,1) to min(aPosition.Y+RadBottom,MapY-1) do
  for k:=max(aPosition.X-RadLeft,1) to min(aPosition.X+RadRight,MapX-1) do
  begin
    if Land[i,k].Terrain = R1 then begin if InRange(i,aPosition.Y-RadTop +2,aPosition.Y+RadBottom-2) then
                                         if InRange(k,aPosition.X-RadLeft+2,aPosition.X+RadRight -2) then
                                         L[1].AddEntry(KMPoint(k,i)) end else
    if Land[i,k].Terrain = R2 then begin if InRange(i,aPosition.Y-RadTop +1,aPosition.Y+RadBottom-1) then
                                         if InRange(k,aPosition.X-RadLeft+1,aPosition.X+RadRight -1) then
                                         L[2].AddEntry(KMPoint(k,i)) end else
    if Land[i,k].Terrain = R3 then L[3].AddEntry(KMPoint(k,i)) else //Always mine second richest ore, it is never left in KaM
    if Land[i,k].Terrain = R4 then L[4].AddEntry(KMPoint(k,i));     //Always mine richest ore
  end;

  if L[4].Count<>0 then Result:=L[4].GetRandom else
  if L[3].Count<>0 then Result:=L[3].GetRandom else
  if L[2].Count<>0 then Result:=L[2].GetRandom else
  if L[1].Count<>0 then Result:=L[1].GetRandom else
  Result:=KMPoint(0,0);

  for i:=1 to 4 do L[i].Free;
end;


{Find suitable place to plant a tree.
Prefer ex-trees locations}
function TTerrain.FindPlaceForTree(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPoint;
var i,k:integer; List1,List2:TKMPointList;
begin
  List1:=TKMPointList.Create;
  List2:=TKMPointList.Create;
  for i:=max(aPosition.Y-aRadius,1) to min(aPosition.Y+aRadius,MapY-1) do
  for k:=max(aPosition.X-aRadius,1) to min(aPosition.X+aRadius,MapX-1) do
    if (KMLength(aPosition,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if (CanPlantTrees in Land[i,k].Passability) and Route_CanBeMade(aPosition,KMPoint(k,i),CanWalk,0,false) then begin

        if ObjectIsChopableTree(KMPoint(k,i),6) then //Stump
            List1.AddEntry(KMPoint(k,i))
          else
            List2.AddEntry(KMPoint(k,i));

      end;
if List1.Count>0 then
  Result:=List1.GetRandom
else
  Result:=List2.GetRandom;
List1.Free;
List2.Free;
end;


{Find seaside}
{Return walkable tile nearby}
function TTerrain.FindFishWater(aPosition:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint):TKMPointDir;
const Ins=2; //2..Map-2
var i,k,j,l:integer; List:TKMPointDirList;
begin
  //Check is in stages:
  // A) Inital checks, inside map and radius, etc.
  // B) Limit to only tiles which are water
  // C) Limit to tiles which have fish in the water body
  // D) Final checks, route can be made, etc.

  List:=TKMPointDirList.Create;
  for i:=max(aPosition.Y-aRadius,Ins) to min(aPosition.Y+aRadius,MapY-Ins) do
  for k:=max(aPosition.X-aRadius,Ins) to min(aPosition.X+aRadius,MapX-Ins) do
    // A) Inital checks, inside map and radius, etc.
    if (KMLength(aPosition,KMPoint(k,i)) <= aRadius) then
      if TileIsWater(KMPoint(k,i)) and WaterHasFish(KMPoint(k,i)) then //Limit to only tiles which are water and have fish
        //Now find a tile around this one that can be fished from
        for j:=-1 to 1 do
          for l:=-1 to 1 do
            if TileInMapCoords(k+j,i+l) and ((l <> 0) or (j <> 0)) then
              // D) Final check: route can be made and isn't avoid loc
              if Route_CanBeMade(aPosition, KMPoint(k+j, i+l), CanWalk, 0,false)
              and not KMSamePoint(aAvoidLoc,KMPoint(k+j,i+l)) then
                List.AddEntry(KMPointDir(k+j, i+l, byte(KMGetDirection(j,l))-1));

  Result:=List.GetRandom;
  List.Free;
end;


function TTerrain.CanFindFishingWater(aPosition:TKMPoint; aRadius:integer):boolean;
const Ins=2; //2..Map-2
var i,k:integer;
begin
  Result := false;
  for i:=max(aPosition.Y-aRadius,Ins) to min(aPosition.Y+aRadius,MapY-Ins) do
  for k:=max(aPosition.X-aRadius,Ins) to min(aPosition.X+aRadius,MapX-Ins) do
    if (KMLength(aPosition,KMPoint(k,i)) <= aRadius) then
      if TileIsWater(KMPoint(k,i)) then
      begin
        Result := true;
        exit;
      end;
end;


function TTerrain.ChooseTreeToPlant(aPosition:TKMPoint):integer;
begin
  //This function randomly chooses a tree object based on the terrain type. Values matched to KaM, using all soil tiles.
  case Land[aPosition.Y,aPosition.X].Terrain of
    0..3,5,6,8,9,11,13,14,18,19,56,57,66..69,72..74,84..86,93..98,180,188: Result := ChopableTrees[1+Random(7),1]; //Grass (oaks, etc.)
    26..28,75..80,182,190:                                                 Result := ChopableTrees[7+Random(2),1]; //Yellow dirt
    16,17,20,21,34..39,47,49,58,64,65,87..89,183,191,220,247:              Result := ChopableTrees[9+Random(5),1]; //Brown dirt (pine trees)
    else Result := ChopableTrees[1+Random(length(ChopableTrees)),1]; //If it isn't one of those soil types then choose a random tree
  end;
end;


function TTerrain.WaterHasFish(aPosition:TKMPoint):boolean;
begin
  Result := (fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aPosition.Y,aPosition.X].WalkConnect[wcFish],false) <> nil);
end;


function TTerrain.CatchFish(aPosition:TKMPointDir; TestOnly:boolean=false):boolean;
var MyFish: TKMUnitAnimal;
begin
  //Here we are catching fish in the tile 1 in the direction
  aPosition := KMGetPointInDir(aPosition.Loc, TKMDirection(aPosition.Dir+1));
  MyFish := fPlayers.PlayerAnimals.GetFishInWaterBody(Land[aPosition.Loc.Y,aPosition.Loc.X].WalkConnect[wcFish],not TestOnly);
  Result := (MyFish <> nil);
  if (not TestOnly) and (MyFish <> nil) then MyFish.ReduceFish; //This will reduce the count or kill it (if they're all gone)
end;


procedure TTerrain.SetTree(Loc:TKMPoint; ID:integer);
begin
  Land[Loc.Y,Loc.X].Obj:=ID;
  Land[Loc.Y,Loc.X].TreeAge:=1;
  RebuildPassability(Loc.X-1,Loc.X+1,Loc.Y-1,Loc.Y+1); //Because surrounding tiles will be affected (CanPlantTrees)
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
procedure TTerrain.ChopTree(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].TreeAge:=0;
  FallingTrees.RemoveEntry(Loc);
  RecalculatePassabilityAround(Loc); //Because surrounding tiles will be affected (CanPlantTrees)
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
end;


procedure TTerrain.SowCorn(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=1;
  Land[Loc.Y,Loc.X].Terrain := 61; //Plant it right away, don't wait for update state
  RecalculatePassability(Loc);
end;


procedure TTerrain.CutCorn(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=0;
  Land[Loc.Y,Loc.X].Terrain:=63;
  Land[Loc.Y,Loc.X].Obj:=255;
end;


procedure TTerrain.CutGrapes(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=1;
  Land[Loc.Y,Loc.X].Obj:=54; //Reset the grapes
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
    else        fGame.GameError(Loc, 'Wrong resource deposit');
  end;
  RecalculatePassability(Loc);
end;


{Extract one unit of stone}
procedure TTerrain.DecStoneDeposit(Loc:TKMPoint);

  procedure UpdateTransition(X,Y:integer);
  const TileID:array[0..15]of byte = (0,139,139,138,139,140,138,141,139,138,140,141,138,141,141,128);
         RotID:array[0..15]of byte = (0,  0,  1,  0,  2,  0,  1,  3,  3,  3,  1,  2,  2,  1,  0,  0);
  var Bits:byte;
  begin
    if TileInMapCoords(X,Y) then
    if TileIsStone(KMPoint(X,Y))=0 then
    begin
      Bits:=0;
      if TileInMapCoords(X,Y+1) and (TileIsStone(KMPoint(X,Y-1))>0) then inc(Bits,1);    //     1
      if TileInMapCoords(X+1,Y) and (TileIsStone(KMPoint(X+1,Y))>0) then inc(Bits,2);    //   8 . 2
      if TileInMapCoords(X,Y-1) and (TileIsStone(KMPoint(X,Y+1))>0) then inc(Bits,4);    //     4
      if TileInMapCoords(X-1,Y) and (TileIsStone(KMPoint(X-1,Y))>0) then inc(Bits,8);    //
      Land[Y,X].Terrain:=TileID[Bits];
      Land[Y,X].Rotation:=RotID[Bits];
      if Land[Y,X].Terrain = 0 then Land[Y,X].Rotation:=Random(4); //Randomise the direction of grass tiles
      RecalculatePassability(Loc);
    end;
  end;

begin
  case Land[Loc.Y,Loc.X].Terrain of
    132,137: Land[Loc.Y,Loc.X].Terrain:=131+Random(2)*5;
    131,136: Land[Loc.Y,Loc.X].Terrain:=130+Random(2)*5;
    130,135: Land[Loc.Y,Loc.X].Terrain:=129+Random(2)*5;
    129,134: Land[Loc.Y,Loc.X].Terrain:=128+Random(2)*5;
    128,133: Land[Loc.Y,Loc.X].Terrain:=0;
    else exit;
  end;
  Land[Loc.Y,Loc.X].Rotation:=Random(4);
  UpdateTransition(Loc.X,Loc.Y);   //Update these 5 transitions
  UpdateTransition(Loc.X,Loc.Y-1); //    x
  UpdateTransition(Loc.X+1,Loc.Y); //  x X x
  UpdateTransition(Loc.X,Loc.Y+1); //    x
  UpdateTransition(Loc.X-1,Loc.Y);
  //@Krom: We have a crash caused by this. Start the town tutorial, then leave it running without touching anything.
  //       After ~30 minutes the game crashes with a "serf on unwalkable tile" error. It is caused by a serf
  //       walking past the stone hill at the exact moment the stonemason finishes mining and flatterns the hill.
  //       The tile the serf is stepping to becomes unwalkable, (too steep) triggering this error.
  //       Same thing would happen if you had any unit standing near a stone hill. We need to discuss a solution.
  //       I think this is the same bug being reported on the forums. I can't see any easy solutions.
  //       We need to flatten this tile without making any other tiles become unwalkable. (at least ones with units)
  FlattenTerrain(Loc);
  //If tile stonemason is standing on becomes unwalkable, flatten it too so he doesn't get stuck all the time
  if not CheckHeightPass(KMPointY1(Loc),CanWalk) then
    FlattenTerrain(KMPointY1(Loc));
  RecalculatePassabilityAround(Loc);
  RebuildWalkConnect(wcWalk);
end;


{ Try to extract one unit of ore
  It may fail cos of two miners mining the same last piece of ore }
function TTerrain.DecOreDeposit(Loc:TKMPoint; rt:TResourceType):boolean;
begin
  if not (rt in [rt_IronOre,rt_GoldOre,rt_Coal]) then
    fGame.GameError(Loc, 'Wrong ore decrease');

  Result := true;
  case Land[Loc.Y,Loc.X].Terrain of
    144: Land[Loc.Y,Loc.X].Terrain:=157+random(3); //Gold
    145: Land[Loc.Y,Loc.X].Terrain:=144;
    146: Land[Loc.Y,Loc.X].Terrain:=145;
    147: Land[Loc.Y,Loc.X].Terrain:=146;
    148: Land[Loc.Y,Loc.X].Terrain:=160+random(4); //Iron
    149: Land[Loc.Y,Loc.X].Terrain:=148;
    150: Land[Loc.Y,Loc.X].Terrain:=149;
    151: Land[Loc.Y,Loc.X].Terrain:=150;
    152: Land[Loc.Y,Loc.X].Terrain:=35 +random(2); //Coal
    153: Land[Loc.Y,Loc.X].Terrain:=152;
    154: Land[Loc.Y,Loc.X].Terrain:=153;
    155: Land[Loc.Y,Loc.X].Terrain:=154;
    else Result := false;
  end;
  Land[Loc.Y,Loc.X].Rotation:=random(3);
  RecalculatePassability(Loc);
end;


procedure TTerrain.RecalculatePassabilityAround(Loc:TKMPoint);
begin
  RebuildPassability(Loc.X-1,Loc.X+1,Loc.Y-1,Loc.Y+1);
end;


procedure TTerrain.RecalculatePassability(Loc:TKMPoint);
var i,k:integer;
  HousesNearBy:boolean;
  procedure AddPassability(aLoc:TKMPoint; aPass:TPassabilitySet);
  begin Land[aLoc.Y,aLoc.X].Passability:=Land[aLoc.Y,aLoc.X].Passability + aPass; end;

  function IsObjectsNearby(X,Y:integer):boolean;
  var i,k:integer;
  begin
    Result := false;
    for i:=-1 to 1 do
      for k:=-1 to 1 do
        if TileInMapCoords(X+i,Y+k)and((i<>0)or(k<>0)) then
        begin
          //Tiles next to it can't be trees/stumps
          if MapElem[Land[Y+k,X+i].Obj+1].DontPlantNear then Result:=true;
          //Tiles above or to the left can't be road/field/markup
          if (i<=0)and(k<=0) then
            if (Land[Y+k,X+i].Markup<>mu_None)or(Land[Y+k,X+i].TileOverlay = to_Road)or
              (TileIsCornField(KMPoint(X+i,Y+k)))or(TileIsWineField(KMPoint(X+i,Y+k))) then
              Result := true;
        end;
  end;
begin
  //First of all exclude all tiles outside of actual map
  if not TileInMapCoords(Loc.X,Loc.Y) then begin
    fGame.GameError(Loc, 'Failed to recalculate passability');
    exit;
  end;

  Land[Loc.Y,Loc.X].Passability := [];

  //For all passability types other than CanAll, houses and fenced houses are excluded
  if not(Land[Loc.Y,Loc.X].Markup in [mu_House, mu_HouseFenceNoWalk]) then begin

   if (TileIsWalkable(Loc))and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      CheckHeightPass(Loc,CanWalk)then
     AddPassability(Loc, [CanWalk]);

   if (Land[Loc.Y,Loc.X].TileOverlay=to_Road)and
      CheckPassability(Loc,CanWalk) then //Not all roads are walkable, they must also have CanWalk passability
     AddPassability(Loc, [CanWalkRoad]);

   //Check for houses around this tile
   HousesNearBy := false;
   for i:=-1 to 1 do
     for k:=-1 to 1 do
       if TileInMapCoords(Loc.X+k,Loc.Y+i) then
         if (Land[Loc.Y+i,Loc.X+k].Markup in [mu_HousePlan,mu_HouseFenceCanWalk,mu_HouseFenceNoWalk,mu_House]) then
           HousesNearBy := true;

   if (TileIsRoadable(Loc))and
      ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved))and //Only certain objects are excluded
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not TileIsCornField(Loc))and
      (not TileIsWineField(Loc))and //Can't build houses on fields
      (TileInMapCoords(Loc.X,Loc.Y,1))and
      (not HousesNearBy)and
      CheckHeightPass(Loc,CanBuild) then //No houses nearby
      AddPassability(Loc, [CanBuild]);

   if (Land[Loc.Y,Loc.X].Terrain in [109,166..170])and
      (Land[Loc.Y,Loc.X].Rotation = 0)and //only horizontal mountain edges allowed
      ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved))and
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not TileIsCornField(Loc))and
      (not TileIsWineField(Loc))and //Can't build houses on fields
      (TileInMapCoords(Loc.X,Loc.Y,1))and
      (not HousesNearBy)and //No houses nearby
      CheckHeightPass(Loc,CanBuildIron) then
     AddPassability(Loc, [CanBuildIron]);

   if (Land[Loc.Y,Loc.X].Terrain in [171..175])and
      (Land[Loc.Y,Loc.X].Rotation = 0)and
      ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved))and
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not TileIsCornField(Loc))and
      (not TileIsWineField(Loc))and //Can't build houses on fields
      (TileInMapCoords(Loc.X,Loc.Y,1))and
      (not HousesNearBy)and //No houses nearby
      CheckHeightPass(Loc,CanBuildGold) then
     AddPassability(Loc, [CanBuildGold]);

   if (TileIsRoadable(Loc))and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Road)and
      CheckHeightPass(Loc,CanMakeRoads) then
     AddPassability(Loc, [CanMakeRoads]);

   if (TileIsSoil(Loc))and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)and
      CheckHeightPass(Loc,CanMakeFields) then
     AddPassability(Loc, [CanMakeFields]);

   if (TileIsSoil(Loc))and
      (not IsObjectsNearby(Loc.X,Loc.Y))and //This function checks surrounding tiles
      (Land[Loc.Y,Loc.X].Markup=mu_None)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (Loc.X > 1)and(Loc.Y > 1)and //Not top/left of map, but bottom/right is ok
      (Land[Loc.Y,Loc.X].TileOverlay <> to_Road)and
      (not HousesNearBy)and
      ((Land[Loc.Y,Loc.X].Obj=255) or ObjectIsChopableTree(KMPoint(Loc.X,Loc.Y),6))and
      CheckHeightPass(Loc,CanPlantTrees) then
     AddPassability(Loc, [CanPlantTrees]);

   if TileIsWater(Loc) then
     AddPassability(Loc, [CanFish]);

   if (TileIsSand(Loc))and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      (Land[Loc.Y,Loc.X].Markup<>mu_HouseFenceNoWalk)and
      (Land[Loc.Y,Loc.X].Markup<>mu_House)and
      (Land[Loc.Y,Loc.X].Markup<>mu_UnderConstruction)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Road)and
      (not TileIsCornField(Loc))and
      (not TileIsWineField(Loc))and
      CheckHeightPass(Loc,CanCrab) then //Can't crab on houses, fields and roads (can walk on markups so you can't kill them by placing a house on top of them)
     AddPassability(Loc, [CanCrab]);

   if (TileIsSoil(Loc))and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      (not TileIsCornField(Loc))and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not TileIsWineField(Loc))and
      CheckHeightPass(Loc,CanWolf) then
     AddPassability(Loc, [CanWolf]);

  end;
  if (TileIsWalkable(Loc))and
    (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
    (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
    CheckHeightPass(Loc,CanWalk)and
    not(Land[Loc.Y,Loc.X].Markup = mu_House) then
    AddPassability(Loc, [CanWorker]);

 //Check for houses around this vertice(!) Use only with CanElevate since it's vertice-based!
 HousesNearBy := false;
 for i:=-1 to 0 do
   for k:=-1 to 0 do
     if TileInMapCoords(Loc.X+k,Loc.Y+i) then
       if (Land[Loc.Y+i,Loc.X+k].Markup in [mu_House])or
          (Land[Loc.Y,Loc.X].TileOverlay=to_Wall) then
         HousesNearBy := true;

 if (VerticeInMapCoords(Loc.X,Loc.Y))and
    (not HousesNearBy) then
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


{Return random tile surrounding Loc with aPass property except Loc2}
{The command is used for unit interaction}
function TTerrain.GetOutOfTheWay(Loc, Loc2:TKMPoint; aPass:TPassability):TKMPoint;
var i,k:integer; L1,L2,L3:TKMPointList; TempUnit: TKMUnit; Loc2IsOk: boolean;
begin
  //List 1 holds all available walkable positions except self
  L1:=TKMPointList.Create;
  for i:=-1 to 1 do for k:=-1 to 1 do
    if (i<>0)or(k<>0) then
      if TileInMapCoords(Loc.X+k,Loc.Y+i) then
        if CanWalkDiagonaly(Loc,KMPoint(Loc.X+k,Loc.Y+i)) then //Check for trees that stop us walking on the diagonals!
          if Land[Loc.Y+i,Loc.X+k].Markup <> mu_UnderConstruction then
            if aPass in Land[Loc.Y+i,Loc.X+k].Passability then
              L1.AddEntry(KMPoint(Loc.X+k,Loc.Y+i));

  //List 2 holds the best positions, ones which are not occupied
  L2:=TKMPointList.Create;
  for i:=1 to L1.Count do
    if Land[L1.List[i].Y,L1.List[i].X].IsUnit = nil then
      L2.AddEntry(L1.List[i]);

  Loc2IsOk := false;
  //List 3 holds the second best positions, ones which are occupied with an idle unit
  L3:=TKMPointList.Create;
  for i:=1 to L1.Count do
    if Land[L1.List[i].Y,L1.List[i].X].IsUnit <> nil then
    begin
      if KMSamePoint(L1.List[i],Loc2) then Loc2IsOk := true; //Make sure unit that pushed us is a valid tile
      TempUnit := fTerrain.UnitsHitTest(L1.List[i].X, L1.List[i].Y);
      if TempUnit <> nil then
        if (TempUnit.GetUnitAction is TUnitActionStay) and (not TUnitActionStay(TempUnit.GetUnitAction).Locked) then
          L3.AddEntry(L1.List[i]);
    end;

  if L2.Count<>0 then
    Result:=L2.GetRandom
  else
  if Loc2IsOk then //If there are no free tiles then the unit that pushed us is a good option (exchange?)
    Result:=Loc2
  else
  if L3.Count<>0 then
    Result:=L3.GetRandom
  else
  if L1.Count<>0 then
    Result:=L1.GetRandom
  else
    Result:=Loc;

  L1.Free;
  L2.Free;
  L3.Free;
end;


function TTerrain.FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; OnlyTakeBest: boolean=false):TKMPoint;
var i,k:integer; L1,L2:TKMPointList;
begin
  //List 1 holds all positions next to both Loc and Loc2
  L1 := TKMPointList.Create;
  for i:=-1 to 1 do for k:=-1 to 1 do
  if ((i<>0)or(k<>0)) and TileInMapCoords(Loc.X+k,Loc.Y+i) then //Valid tile for test
  if not KMSamePoint(KMPoint(Loc.X+k,Loc.Y+i),Loc2) then
  if aPass in Land[Loc.Y+i,Loc.X+k].Passability then
  if CanWalkDiagonaly(Loc,KMPoint(Loc.X+k,Loc.Y+i)) then //Check for trees that stop us walking on the diagonals!
  if Land[Loc.Y+i,Loc.X+k].Markup <> mu_UnderConstruction then
  if KMLength(KMPoint(Loc.X+k,Loc.Y+i),Loc2) <= 1 then //Right next to Loc2 (not diagonal)
  if not fTerrain.HasUnit(KMPoint(Loc.X+k,Loc.Y+i)) then //Doesn't have a unit
    L1.AddEntry(KMPoint(Loc.X+k,Loc.Y+i));

  //List 2 holds the best positions, ones which are also next to Loc3 (next position)
  L2 := TKMPointList.Create;
  if not KMSamePoint(Loc3, KMPoint(0,0)) then //No Loc3 was given
  for i:=1 to L1.Count do
    if KMLength(L1.List[i],Loc3) < 1.5 then //Next to Loc3 (diagonal is ok)
      L2.AddEntry(L1.List[i]);

  if L2.Count<>0 then
    Result:=L2.GetRandom
  else if (not OnlyTakeBest) and (L1.Count<>0) then
    Result:=L1.GetRandom
  else
    Result:=KMPoint(0,0); //No side step positions available

  L1.Free;
  L2.Free;
end;


//Test wherever it is possible to make the route without actually making it to save performance
function TTerrain.Route_CanBeMade(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
var i,k:integer; aHouse:TKMHouse; TestRadius:boolean;
begin
  Result := true;

  //target has to be different point than source
  //Result:=not (KMSamePoint(LocA,LocB)); //Or maybe we don't care

  //If we are in worker mode then use the house entrance passability if we are still standing in a house
  if aPass=CanWorker then
  begin
    aHouse := fPlayers.HousesHitTest(LocA.X,LocA.Y);
    if aHouse <> nil then
    begin
      LocA := KMPointY1(aHouse.GetEntrance);
      aPass := CanWalk;
    end;
  end;

  //target point has to be walkable
  Result := Result and CheckPassability(LocA,aPass);
  TestRadius := false;
  for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),MapY-1) do
  for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),MapX-1) do
  if GetLength(LocB,KMPoint(k,i))<=aDistance then
    TestRadius := TestRadius or CheckPassability(KMPoint(k,i),aPass);
  Result := Result and TestRadius;

  //There's a walkable way between A and B (which is proved by FloodFill test on map init)
  if aPass=CanWalk then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),MapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),MapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcWalk] = Land[i,k].WalkConnect[wcWalk]);
    Result := Result and TestRadius;
  end;

  if aPass=CanWalkRoad then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),MapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),MapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcRoad] = Land[i,k].WalkConnect[wcRoad]);
    Result := Result and TestRadius;
  end;

  if aPass=CanFish then
    Result := Result and (Land[LocA.Y,LocA.X].WalkConnect[wcFish] = Land[LocB.Y,LocB.X].WalkConnect[wcFish]);

  if aInteractionAvoid then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),MapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),MapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcAvoid] = Land[i,k].WalkConnect[wcAvoid]);
    Result := Result and TestRadius;
  end;
end;


function TTerrain.Route_CanBeMadeToVertex(LocA, LocB:TKMPoint; aPass:TPassability):boolean;
var i,k:integer;
begin
  //Check if a route can be made to this vertex, from any direction (used for woodcutter cutting trees)
  Result := false;
  //Check from top-left of vertex to vertex tile itself
  for i := -1 to 0 do
    for k := -1 to 0 do
      Result := Result or Route_CanBeMade(LocA,KMPoint(LocB.X+i,LocB.Y+k),aPass,0,false);
end;


function TTerrain.Route_CanBeMadeToHouse(LocA:TKMPoint; aHouse:TKMHouse; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
var i:integer; Cells: TKMPointList;
begin
  //Check if a route can be made to any tile around this house
  Result := false;
  Cells := TKMPointList.Create;
  try
    aHouse.GetListOfCellsWithin(Cells);
    for i:=1 to Cells.Count do
      Result := Result or Route_CanBeMade(LocA,Cells.List[i],aPass,aDistance,aInteractionAvoid);
  finally
    Cells.Free;
  end;
end;


//Tests weather route can be made
function TTerrain.Route_MakeAvoid(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aHouse:TKMHouse; var NodeList:TKMPointList):boolean;
var fPath:TPathFinding;
begin
  fPath := TPathFinding.Create(LocA, LocB, aPass, aDistance, aHouse, true); //True means we are using Interaction Avoid mode (go around busy units)
  try
    Result := fPath.RouteSuccessfullyBuilt;
    if not Result then exit;
    if NodeList <> nil then NodeList.Clearup;
    fPath.ReturnRoute(NodeList);
  finally
    fPath.Free;
  end;
end;


{Find a route from A to B which meets aPass Passability}
{Results should be written as NodeCount of waypoint nodes to Nodes}
procedure TTerrain.Route_Make(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aHouse:TKMHouse; var NodeList:TKMPointList);
var fPath:TPathFinding;
begin
  fPath := TPathFinding.Create(LocA, LocB, aPass, aDistance, aHouse);
  fPath.ReturnRoute(NodeList);
  FreeAndNil(fPath);
end;


procedure TTerrain.Route_ReturnToRoad(LocA, LocB:TKMPoint; TargetRoadNetworkID:byte; var NodeList:TKMPointList);
var fPath:TPathFinding;
begin
  fPath := TPathFinding.Create(LocA, wcRoad, TargetRoadNetworkID, CanWalk, LocB);
  fPath.ReturnRoute(NodeList);
  FreeAndNil(fPath);
end;


procedure TTerrain.Route_ReturnToWalkable(LocA, LocB:TKMPoint; TargetWalkNetworkID:byte; var NodeList:TKMPointList);
var fPath:TPathFinding;
begin
  fPath := TPathFinding.Create(LocA, wcWalk, TargetWalkNetworkID, CanWorker, LocB);
  fPath.ReturnRoute(NodeList);
  FreeAndNil(fPath);
end;


//Returns the closest tile to TargetLoc with aPass and walk connect to OriginLoc
//If no tile found - return Origin location
function TTerrain.GetClosestTile(TargetLoc, OriginLoc:TKMPoint; aPass:TPassability):TKMPoint;
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
  if fTerrain.CheckPassability(TargetLoc, aPass) and (WalkConnectID = Land[TargetLoc.Y,TargetLoc.X].WalkConnect[wcType]) then
  begin
    Result := TargetLoc;
    exit;
  end;

  //If target is not accessable then choose a tile near to the target that is accessable
  //As we Cannot reach our destination we are "low priority" so do not choose a tile with another unit on it (don't bump important units)
  for i:=0 to TestDepth do begin
    P := GetPositionFromIndex(TargetLoc, i);
    if not fTerrain.TileInMapCoords(P.X,P.Y) then continue;
    T := KMPoint(P.X,P.Y);
    if fTerrain.CheckPassability(T, aPass)
      and (WalkConnectID = Land[T.Y,T.X].WalkConnect[wcType])
      and (not fTerrain.HasUnit(T) or KMSamePoint(T,OriginLoc)) //Allow position we are currently on, but not ones with other units
    then begin
      Result := T; //Assign if all test are passed
      exit;
    end;
  end;

  Result := OriginLoc; //If we don't find one, return existing Loc
end;


{Mark tile as occupied}
procedure TTerrain.UnitAdd(LocTo:TKMPoint; aUnit:TKMUnit);
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
procedure TTerrain.UnitWalk(LocFrom,LocTo:TKMPoint; aUnit:TKMUnit);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Land[LocFrom.Y,LocFrom.X].IsUnit = aUnit, 'Trying to remove wrong unit at '+TypeToString(LocFrom));
  Land[LocFrom.Y,LocFrom.X].IsUnit := nil;
  Assert(Land[LocTo.Y,LocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(LocTo));
  Land[LocTo.Y,LocTo.X].IsUnit := aUnit
end;


procedure TTerrain.UnitSwap(LocFrom,LocTo:TKMPoint; UnitFrom:TKMUnit);
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


function TTerrain.VertexUsageCompatible(LocFrom, LocTo:TKMPoint): boolean;
var Vert: TKMPoint; VertUsage: TKMVertexUsage;
begin
  Vert := KMGetDiagVertex(LocFrom, LocTo);
  VertUsage := GetVertexUsageType(LocFrom, LocTo);
  Result := (Land[Vert.Y,Vert.X].IsVertexUnit = vu_None) or (Land[Vert.Y,Vert.X].IsVertexUnit = VertUsage);
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


{Take 4 neighbour heights and approach it}
procedure TTerrain.FlattenTerrain(Loc:TKMPoint);
var TempH:byte;
begin
  TempH:=(
    Land[Loc.Y,Loc.X].Height+
    Land[Loc.Y+1,Loc.X].Height+
    Land[Loc.Y,Loc.X+1].Height+
    Land[Loc.Y+1,Loc.X+1].Height
  )div 4;
  if CheckPassability(KMPoint(Loc.X,Loc.Y),CanElevate) then
  Land[Loc.Y,Loc.X].Height:=mix(Land[Loc.Y,Loc.X].Height,TempH,0.5);
  if CheckPassability(KMPoint(Loc.X,Loc.Y+1),CanElevate) then
  Land[Loc.Y+1,Loc.X].Height:=mix(Land[Loc.Y+1,Loc.X].Height,TempH,0.5);
  if CheckPassability(KMPoint(Loc.X+1,Loc.Y),CanElevate) then
  Land[Loc.Y,Loc.X+1].Height:=mix(Land[Loc.Y,Loc.X+1].Height,TempH,0.5);
  if CheckPassability(KMPoint(Loc.X+1,Loc.Y+1),CanElevate) then
  Land[Loc.Y+1,Loc.X+1].Height:=mix(Land[Loc.Y+1,Loc.X+1].Height,TempH,0.5);

  RebuildLighting(Loc.X-2,Loc.X+3,Loc.Y-2,Loc.Y+3);
  RecalculatePassabilityAround(Loc); //Changing height will affect the cells around this one
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
end;


{Flatten a list of points in the mission init}
procedure TTerrain.FlattenTerrain(LocList:TKMPointList);
var TempH:byte; i:integer; Loc:TKMPoint;
begin
  for i:=1 to LocList.Count do begin
    Loc := LocList.List[i];
    TempH:=(
      Land[Loc.Y,Loc.X].Height+
      Land[Loc.Y+1,Loc.X].Height+
      Land[Loc.Y,Loc.X+1].Height+
      Land[Loc.Y+1,Loc.X+1].Height
    )div 4;
    if CheckPassability(KMPoint(Loc.X,Loc.Y),CanElevate) then
    Land[Loc.Y,Loc.X].Height:=mix(Land[Loc.Y,Loc.X].Height,TempH,0.5);
    if CheckPassability(KMPoint(Loc.X,Loc.Y+1),CanElevate) then
    Land[Loc.Y+1,Loc.X].Height:=mix(Land[Loc.Y+1,Loc.X].Height,TempH,0.5);
    if CheckPassability(KMPoint(Loc.X+1,Loc.Y),CanElevate) then
    Land[Loc.Y,Loc.X+1].Height:=mix(Land[Loc.Y,Loc.X+1].Height,TempH,0.5);
    if CheckPassability(KMPoint(Loc.X+1,Loc.Y+1),CanElevate) then
    Land[Loc.Y+1,Loc.X+1].Height:=mix(Land[Loc.Y+1,Loc.X+1].Height,TempH,0.5);
    RebuildLighting(Loc.X-2,Loc.X+3,Loc.Y-2,Loc.Y+3);
    RecalculatePassabilityAround(Loc); //Changing height will affect the cells around this one
  end;

  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
end;


{ Rebuilds lighting values for given bounds.
These values are used to draw highlights/shadows on terrain.}
procedure TTerrain.RebuildLighting(LowX,HighX,LowY,HighY:integer);
var i,k:integer; x0,y2:integer;
begin
  for i:=LowY to HighY do for k:=LowX to HighX do
    if VerticeInMapCoords(k,i) then begin
      x0:=EnsureRange(k-1,1,MapX);
      y2:=EnsureRange(i+1,1,MapY);
      if VerticeInMapCoords(x0,y2) then
        Land[i,k].Light:=EnsureRange((Land[i,k].Height-(Land[y2,k].Height+Land[i,x0].Height)/2)/22,-1,1); //  1.33*16 ~=22
    if (i=1)or(i=MapY)or(k=1)or(k=MapX) then //Map borders fade to black
      Land[i,k].Light:=-1;
    end;
end;


{ Rebuilds passability for given bounds }
procedure TTerrain.RebuildPassability(LowX,HighX,LowY,HighY:integer);
var i,k:integer;
begin
  for i:=max(LowY,1) to min(HighY,MapY-1) do for k:=max(LowX,1) to min(HighX,MapX-1) do
    RecalculatePassability(KMPoint(k,i));
end;


{ Rebuilds connected areas using flood fill algorithm }
procedure TTerrain.RebuildWalkConnect(wcType:TWalkConnect);
//const MinSize=9; //Minimum size that is treated as new area
var i,k{,h}:integer; AreaID:byte; Count:integer; Pass:TPassability;

  procedure FillArea(x,y:word; ID:byte; var Count:integer); //Mode = 1CanWalk or 2CanWalkRoad
  begin
    if (Land[y,x].WalkConnect[wcType]=0)and(Pass in Land[y,x].Passability)and //Untested area
     ((wcType <> wcAvoid)or
     ( (wcType=wcAvoid) and not fTerrain.TileIsLocked(KMPoint(x,y)) )) then //Matches passability
    begin
      Land[y,x].WalkConnect[wcType]:=ID;
      inc(Count);
      //Using custom TileInMapCoords replacement gives ~40% speed improvement
      if x-1>=1 then begin
        if y-1>=1 then    FillArea(x-1,y-1,ID,Count);
                          FillArea(x-1,y  ,ID,Count);
        if y+1<=MapY then FillArea(x-1,y+1,ID,Count);
      end;

      if y-1>=1 then    FillArea(x,y-1,ID,Count);
      if y+1<=MapY then FillArea(x,y+1,ID,Count);

      if x+1<=MapX then begin
        if y-1>=1 then    FillArea(x+1,y-1,ID,Count);
                          FillArea(x+1,y  ,ID,Count);
        if y+1<=MapY then FillArea(x+1,y+1,ID,Count);
      end;
    end;
  end;

begin
    case wcType of
      wcWalk:  Pass := CanWalk;
      wcRoad:  Pass := CanWalkRoad;
      wcFish:  Pass := CanFish;
      wcAvoid: Pass := CanWalk; //Special case for unit interaction avoiding
      else     fLog.AssertToLog(false, 'Unexpected aPass in RebuildWalkConnect function');
    end;

    //Reset everything
    for i:=1 to MapY do for k:=1 to MapX do
      Land[i,k].WalkConnect[wcType] := 0;

    AreaID := 0;
    for i:=1 to MapY do for k:=1 to MapX do
    if (Land[i,k].WalkConnect[wcType]=0) and (Pass in Land[i,k].Passability) and
     ((wcType <> wcAvoid)or
     ( (wcType=wcAvoid) and not fTerrain.TileIsLocked(KMPoint(k,i)) )) then
    begin
      inc(AreaID);
      Count := 0;
      FillArea(k,i,AreaID,Count);

      if Count=1 {<MinSize} then //Revert
      begin
        dec(AreaID);
        Count := 0;
        Land[i,k].WalkConnect[wcType] := 0;
      end;

      fLog.AssertToLog(AreaID<255,'RebuildWalkConnect failed due too many unconnected areas');
    end;
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TTerrain.SetHouse(Loc:TKMPoint; aHouseType: THouseType; aHouseStage:THouseStage; aOwner:TPlayerIndex; const aFlattenTerrain:boolean=false);
var i,k,x,y:word; L,H:TKMPoint; ToFlatten:TKMPointList;
begin
  if aFlattenTerrain then //We will check aFlattenTerrain only once, otherwise there are compiler warnings
    ToFlatten := TKMPointList.Create
  else
    ToFlatten := nil;

  if aHouseStage in [hs_None, hs_Plan] then
    SetHouseAreaOwner(Loc, aHouseType, -1)
  else
    SetHouseAreaOwner(Loc, aHouseType, aOwner);

  for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then
    begin
      x:=Loc.X+k-3; y:=Loc.Y+i-4;
      if TileInMapCoords(x,y) then
      begin

        if (HousePlanYX[byte(aHouseType),i,k]=2)and(aHouseStage=hs_Built) then
          Land[y,x].TileOverlay := to_Road;

        case aHouseStage of
          hs_None:  Land[y,x].Markup:=mu_None;
          hs_Plan:  Land[y,x].Markup:=mu_HousePlan;
          hs_Fence: Land[y,x].Markup:=mu_HouseFenceCanWalk; //Initial state, Laborer should assign NoWalk to each tile he digs
          hs_Built: begin
                      Land[y,x].Markup:=mu_House;
                      Land[y,x].Obj:=255;
                      //If house was set e.g. in mission file we must flatten the terrain as no one else has
                      if ToFlatten<>nil then ToFlatten.AddEntry(KMPoint(x,y));
                    end;
        end;

        UpdateBorders(KMPoint(x,y));
      end;
    end;

  if ToFlatten<>nil then FlattenTerrain(ToFlatten);
  if ToFlatten<>nil then FreeAndNil(ToFlatten);
  //Recalculate Passability for tiles around the house so that they can't be built on too
  L:=EnsureTileInMapCoords(Loc.X-3,Loc.Y-4);
  H:=EnsureTileInMapCoords(Loc.X+2,Loc.Y+1);
  RebuildPassability(L.X,H.X,L.Y,H.Y);
  RebuildWalkConnect(wcRoad);
end;


{That is mainly used for minimap now}
procedure TTerrain.SetHouseAreaOwner(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerIndex);
var i,k:integer;
begin
  case aHouseType of
    ht_None:    Land[Loc.Y,Loc.X].TileOwner := aOwner;
    ht_Any:     ; //Do nothing
    else        for i:=1 to 4 do for k:=1 to 4 do //If this is a house make change for whole place
                  if HousePlanYX[byte(aHouseType),i,k]<>0 then
                    if TileInMapCoords(Loc.X+k-3,Loc.Y+i-4) then
                      Land[Loc.Y+i-4,Loc.X+k-3].TileOwner := aOwner;
  end;
end;


{Check if Unit can be placed here}
//Serfs and Workers can be placed off-road since player might want to have a lot of them.
function TTerrain.CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType; aAllowCitizensOffRoad:boolean=true):boolean;
var DesiredPass:TPassability;
begin
  Result := TileInMapCoords(Loc.X, Loc.Y); //Only within map coords
  Result := Result and (Land[Loc.Y, Loc.X].IsUnit = nil); //Check for no unit below

  case aUnitType of
    ut_Woodcutter..ut_Fisher,ut_StoneCutter..ut_Recruit: DesiredPass := CanWalkRoad; //Citizens except Worker
    ut_Wolf..ut_Duck:                              DesiredPass := AnimalTerrain[byte(aUnitType)] //Animals
    else                                           DesiredPass := CanWalk; //Serf, Worker, Warriors
  end;

  //In e.g. map editor mode we are allowed to place citizens off roads
  if aAllowCitizensOffRoad and (DesiredPass = CanWalkRoad) then
    DesiredPass := CanWalk;

  Result := Result and (DesiredPass in Land[Loc.Y, Loc.X].Passability);
end;


{Check if house can be placed in that place}
function TTerrain.CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; aPlayer:TKMPlayer):boolean;
var i,k:integer;
begin
Result:=true;
  Loc.X:=Loc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX; //update offset
  for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
      Result := Result AND TileInMapCoords(Loc.X+k-3,Loc.Y+i-4,1); //Inset one tile from map edges
      Result := Result AND (Land[Loc.Y+i-4,Loc.X+k-3].Markup<>mu_UnderConstruction);

      case aHouseType of
        ht_IronMine: Result := Result AND (CanBuildIron in Land[Loc.Y+i-4,Loc.X+k-3].Passability);
        ht_GoldMine: Result := Result AND (CanBuildGold in Land[Loc.Y+i-4,Loc.X+k-3].Passability);
        //ht_Wall:     Result := Result AND (CanWalk in Land[Loc.Y+i-4,Loc.X+k-3].Passability);
      else
        Result := Result AND (CanBuild in Land[Loc.Y+i-4,Loc.X+k-3].Passability);
      end;

      Result := Result AND (aPlayer.FogOfWar.CheckTileRevelation(Loc.X+k-3,Loc.Y+i-4) > 0);
    end;
end;


function TTerrain.CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup; aPlayer:TKMPlayer):boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y); //Make sure it is inside map, roads can be built on edge
  case aMarkup of
    mu_RoadPlan:  Result := Result AND (CanMakeRoads  in Land[Loc.Y,Loc.X].Passability);
    mu_FieldPlan: Result := Result AND (CanMakeFields in Land[Loc.Y,Loc.X].Passability);
    mu_WinePlan:  Result := Result AND (CanMakeFields in Land[Loc.Y,Loc.X].Passability);
    mu_WallPlan:  Result := Result AND (CanMakeRoads  in Land[Loc.Y,Loc.X].Passability);
    else          Result := false;
  end;
  Result := Result AND (Land[Loc.Y,Loc.X].Markup<>mu_UnderConstruction);
  Result := Result AND (aPlayer.FogOfWar.CheckTileRevelation(Loc.X,Loc.Y) > 0); //We check tile revelation to place a tile-based markup, right?
end;


function TTerrain.CheckHeightPass(aLoc:TKMPoint; aPass:TPassability):boolean;
  function GetHgtSafe(MyLoc:TKMPoint):byte;
  begin
    if TileInMapCoords(MyLoc.X,MyLoc.Y) then
      Result := Land[MyLoc.Y,MyLoc.X].Height //Use requested tile
    else
      Result := Land[aLoc.Y,aLoc.X].Height; //Otherwise return height of original tile which will have no effect
  end;
  function TestHeight(aHeight:byte):boolean;
  var Points: array[1..4] of byte;
  begin
    //Put points into an array like this so it's easy to understand:
    // 1 2
    // 3 4
    Points[1] := GetHgtSafe(aLoc);
    Points[2] := GetHgtSafe(KMPointX1(aLoc));
    Points[3] := GetHgtSafe(KMPointY1(aLoc));
    Points[4] := GetHgtSafe(KMPointX1Y1(aLoc));

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
    CanBuild,CanBuildGold,CanBuildIron: Result := TestHeight(18);
  end; //For other passabilities we ignore height (return default true)
end;


procedure TTerrain.AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);
var i,k:integer;
begin
  if aBuildState in [hbs_Stone, hbs_Done] then //only leave rubble if the construction was well underway (stone and above)
  begin
    //For houses that are at least partually built (leaves rubble)
    for i:=2 to 4 do for k:=2 to 4 do
      if HousePlanYX[byte(aHouseType),i-1,k]<>0 then
      if HousePlanYX[byte(aHouseType),i,k-1]<>0 then
      if HousePlanYX[byte(aHouseType),i-1,k-1]<>0 then
      if HousePlanYX[byte(aHouseType),i,k]<>0 then
        Land[Loc.Y+i-4,Loc.X+k-3].Obj:=68+Random(6);
    for i:=1 to 4 do for k:=1 to 4 do
      if (HousePlanYX[byte(aHouseType),i,k]=1) or (HousePlanYX[byte(aHouseType),i,k]=2) then
      begin
        Land[Loc.Y+i-4,Loc.X+k-3].TileOverlay:=to_Dig3;
        Land[Loc.Y+i-4,Loc.X+k-3].Markup:=mu_None;
      end;
  end
  else begin
    //For glyphs
    for i:=1 to 4 do for k:=1 to 4 do
      if (HousePlanYX[byte(aHouseType),i,k]=1) or (HousePlanYX[byte(aHouseType),i,k]=2) then begin
        Land[Loc.Y+i-4,Loc.X+k-3].Markup:=mu_None;
      end;
  end;
  RebuildPassability(Loc.X-3,Loc.X+2,Loc.Y-4,Loc.Y+1);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
end;


{Check 4 surrounding tiles, and if they are different place a border}
procedure TTerrain.UpdateBorders(Loc:TKMPoint; CheckSurrounding:boolean=true);
  function GetBorderType:TBorderType;
  begin
    if TileIsCornField(Loc) then Result:=bt_Field
    else
    if TileIsWineField(Loc) then Result:=bt_Wine
    else
    if Land[Loc.Y,Loc.X].Markup=mu_HousePlan then Result:=bt_HousePlan
    else
    if Land[Loc.Y,Loc.X].Markup in [mu_HouseFenceCanWalk,mu_HouseFenceNoWalk] then Result:=bt_HouseBuilding
    else
    Result:=bt_None;
  end;
  function GetBorderEnabled(Loc2:TKMPoint):boolean;
  begin
    Result:=true;
    if not TileInMapCoords(Loc2.X,Loc2.Y) then exit;
    if (TileIsCornField(Loc) and TileIsCornField(Loc2))or //Both are Corn
       (TileIsWineField(Loc) and TileIsWineField(Loc2))or //Both are Wine
      ((Land[Loc.Y,Loc.X].Markup in [mu_HousePlan, mu_HouseFenceCanWalk, mu_HouseFenceNoWalk]) and (Land[Loc.Y,Loc.X].Markup=Land[Loc2.Y,Loc2.X].Markup)) or //Both are same mu_House****
      ((Land[Loc.Y,Loc.X].Markup in [mu_HouseFenceCanWalk, mu_HouseFenceNoWalk]) and (Land[Loc2.Y,Loc2.X].Markup in [mu_HouseFenceCanWalk, mu_HouseFenceNoWalk])) then //Both are either house fence
      Result:=false;
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
  Result.X := EnsureRange(round(GameCursor.Float.X+1),1,MapX);
  Result.Y := EnsureRange(round(GameCursor.Float.Y+1),1,MapY);
end;


{Compute cursor position and store it in global variables}
procedure TTerrain.ComputeCursorPosition(X,Y:word; Shift: TShiftState);
var ViewCenter: TKMPointF;
begin
  with GameCursor do begin
    ViewCenter := fViewport.GetCenter; //Required for Linux compatibility
    Float.X := ViewCenter.X + (X-fViewport.ViewRect.Right/2-TOOLBAR_WIDTH/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := ViewCenter.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := fTerrain.ConvertCursorToMapCoord(Float.X,Float.Y);

    Cell.X := EnsureRange(round(Float.X+0.5), 1, fTerrain.MapX); //Cell below cursor in map bounds
    Cell.Y := EnsureRange(round(Float.Y+0.5), 1, fTerrain.MapY);

    SState := Shift;
  end;
end;


{Cursor position should be converted to tile-coords respecting tile heights}
function TTerrain.ConvertCursorToMapCoord(inX,inY:single):single;
var ii:integer; Xc,Yc:integer; Tmp:integer; Ycoef:array[-2..4]of single;
begin
  Xc := EnsureRange(round(inX+0.5),1,MapX-1); //Cell below cursor without height check
  Yc := EnsureRange(round(inY+0.5),1,MapY-1);

  for ii:=-2 to 4 do //make an array of tile heights above and below cursor (-2..4)
  begin
    Tmp := EnsureRange(Yc+ii,1,MapY);
    Ycoef[ii] := (Yc-1)+ii-(fTerrain.Land[Tmp,Xc].Height*(1-frac(inX))
                           +fTerrain.Land[Tmp,Xc+1].Height*frac(inX))/CELL_HEIGHT_DIV;
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


{Return height within cell interpolating node heights}
function TTerrain.InterpolateLandHeight(inX,inY:single):single;
var Xc,Yc:integer; Tmp1,Tmp2:single;
begin
  //todo: Make this match KaM by creating some comparision screenshots of slopes, hills, etc.
  Xc := trunc(inX);
  Yc := trunc(inY);
  Result := 0;
  if not VerticeInMapCoords(Xc,Yc) then exit;

  Tmp1 := mix(fTerrain.Land[Yc  ,Xc+1].Height, fTerrain.Land[Yc  ,Xc].Height, frac(inX));
  if Yc >= MAX_MAP_SIZE then
    Tmp2 := 0
  else
    Tmp2 := mix(fTerrain.Land[Yc+1,Xc+1].Height, fTerrain.Land[Yc+1,Xc].Height, frac(inX));
  Result := mix(Tmp2, Tmp1, frac(inY));
end;


function TTerrain.InterpolateLandHeight(aPoint:TKMPointF):single;
begin
  Result := InterpolateLandHeight(aPoint.X,aPoint.Y);
end;


{ Like above but just a mix of the 4 cells, no factional parts }
function TTerrain.MixLandHeight(inX,inY:byte):byte;
begin
  Result := (Land[inY,inX].Height+Land[inY,inX+1].Height+Land[inY+1,inX].Height+Land[inY+1,inX+1].Height) div 4;
end;


procedure TTerrain.MapEdHeight(aLoc:TKMPointF; aSize, aShape:byte; aRaise:boolean);
var
  i,k:integer;
  Size,Slope:byte;
  Tmp:single;
begin
  Size := aSize AND $F; //Low bits
  Slope := aSize SHR 4; //High bits

  for i := (round(aLoc.Y) - Size div 2) to (round(aLoc.Y) + Size div 2) do
  for k := (round(aLoc.X) - Size div 2) to (round(aLoc.X) + Size div 2) do
  if VerticeInMapCoords(k,i) then
  begin
    case aShape of
      MAPED_HEIGHT_CIRCLE: Tmp := 1 - GetLength(i-aLoc.Y, k-aLoc.X) / (Size div 2);
      MAPED_HEIGHT_SQUARE: Tmp := 1 - Math.max(abs(i-aLoc.Y), abs(k-aLoc.X)) / (Size div 2);
      else Tmp := 0;
    end;
    Tmp := power(abs(Tmp),(Slope+1)/6)*sign(Tmp); //Modify slopes curve
    //Compute resulting floating-point height
    Tmp := EnsureRange(Land[i,k].Height + Land[i,k].HeightAdd/255 + Math.max(0,Tmp) * (byte(aRaise)*2-1), 0, 100);
    Land[i,k].Height := trunc(Tmp);
    Land[i,k].HeightAdd := round(frac(Tmp)*255); //write fractional part in 0..255 range (1byte) to save us mem
  end;

  RebuildLighting(round(aLoc.X) - Size div 2,round(aLoc.X) + Size div 2,round(aLoc.Y) - Size div 2,round(aLoc.Y) + Size div 2);
  RebuildPassability(round(aLoc.X) - Size div 2,round(aLoc.X) + Size div 2,round(aLoc.Y) - Size div 2,round(aLoc.Y) + Size div 2);
end;


procedure TTerrain.MapEdTile(aLoc:TKMPoint; aTile, aRotation:byte);
begin
  if TileInMapCoords(aLoc.X, aLoc.Y) then
  begin
    Land[aLoc.Y, aLoc.X].Terrain := aTile;
    Land[aLoc.Y, aLoc.X].Rotation := aRotation;
    RecalculatePassability(aLoc);
  end;
end;


procedure TTerrain.RefreshMinimapData;
var i,k,ID:integer; Light:smallint; Loc:TKMPointList; FOW:byte;
begin
  for i:=1 to fTerrain.MapY do for k:=1 to fTerrain.MapX do begin
    FOW := MyPlayer.FogOfWar.CheckTileRevelation(k,i);
    if FOW = 0 then
      MiniMapRGB[i,k] := 0
    else
      if fTerrain.Land[i,k].TileOwner = -1 then begin
        ID := fTerrain.Land[i,k].Terrain+1;
        Light := round(fTerrain.Land[i,k].Light*64)-(255-FOW); //it's -255..255 range now
        MiniMapRGB[i,k] :=  EnsureRange(TileMMColor[ID].R+Light,0,255) +
                            EnsureRange(TileMMColor[ID].G+Light,0,255) shl 8 +
                            EnsureRange(TileMMColor[ID].B+Light,0,255) shl 16;
      end else
        MiniMapRGB[i,k] :=  fPlayers.Player[fTerrain.Land[i,k].TileOwner].FlagColor;
  end;

  Loc := TKMPointList.Create;
  for i:=0 to fPlayers.Count-1 do
  if fPlayers.Player[i]<>nil then begin
    fPlayers.Player[i].Units.GetLocations(Loc);
    for k:=1 to Loc.Count do
    if (MyPlayer.FogOfWar.CheckTileRevelation(Loc.List[k].X, Loc.List[k].Y)=255) then
      MiniMapRGB[Loc.List[k].Y,Loc.List[k].X] := fPlayers.Player[i].FlagColor;
  end;

  fPlayers.PlayerAnimals.Units.GetLocations(Loc, ut_Fish);
  for k:=1 to Loc.Count do
  if (MyPlayer.FogOfWar.CheckTileRevelation(Loc.List[k].X, Loc.List[k].Y)=255) then
    MiniMapRGB[Loc.List[k].Y,Loc.List[k].X] := $FF4444;

  Loc.Free;
end;


procedure TTerrain.IncAnimStep;
begin
  inc(fAnimStep);
end;


procedure TTerrain.Save(SaveStream:TKMemoryStream);
var i,k:integer;
begin
  SaveStream.Write('Terrain');
  SaveStream.Write(MapX);
  SaveStream.Write(MapY);

  SaveStream.Write(fAnimStep);
  FallingTrees.Save(SaveStream);

  for i:=1 to MapY do for k:=1 to MapX do
  begin
    //Only save fields that cannot be recalculated after loading
    SaveStream.Write(Land[i,k].Terrain);
    SaveStream.Write(Land[i,k].Height);
    SaveStream.Write(Land[i,k].Rotation);
    SaveStream.Write(Land[i,k].Obj);
    SaveStream.Write(Land[i,k].TreeAge);
    SaveStream.Write(Land[i,k].FieldAge);
    SaveStream.Write(Land[i,k].Markup,SizeOf(Land[i,k].Markup));
    SaveStream.Write(Land[i,k].TileOverlay,SizeOf(Land[i,k].TileOverlay));
    SaveStream.Write(Land[i,k].TileOwner,SizeOf(Land[i,k].TileOwner));
    if Land[i,k].IsUnit <> nil then
      SaveStream.Write(Land[i,k].IsUnit.ID) //Store ID, then substitute it with reference on SyncLoad
    else
      SaveStream.Write(Zero);
    SaveStream.Write(Land[i,k].IsVertexUnit,SizeOf(Land[i,k].IsVertexUnit));
  end;
end;


procedure TTerrain.Load(LoadStream:TKMemoryStream);
var i,k:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Terrain', 'Terrain not found');
  LoadStream.Read(MapX);
  LoadStream.Read(MapY);

  LoadStream.Read(fAnimStep);
  FallingTrees.Load(LoadStream);

  for i:=1 to MapY do for k:=1 to MapX do
  begin
    LoadStream.Read(Land[i,k].Terrain);
    LoadStream.Read(Land[i,k].Height);
    LoadStream.Read(Land[i,k].Rotation);
    LoadStream.Read(Land[i,k].Obj);
    LoadStream.Read(Land[i,k].TreeAge);
    LoadStream.Read(Land[i,k].FieldAge);
    LoadStream.Read(Land[i,k].Markup,SizeOf(Land[i,k].Markup));
    LoadStream.Read(Land[i,k].TileOverlay,SizeOf(Land[i,k].TileOverlay));
    LoadStream.Read(Land[i,k].TileOwner,SizeOf(Land[i,k].TileOwner));
    LoadStream.Read(Land[i,k].IsUnit, 4);
    LoadStream.Read(Land[i,k].IsVertexUnit,SizeOf(Land[i,k].IsVertexUnit));
  end;

  for i:=1 to MapY do for k:=1 to MapX do
    UpdateBorders(KMPoint(k,i),false);

  RebuildLighting(1, MapX, 1, MapY);
  RebuildPassability(1, MapX, 1, MapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
  RebuildWalkConnect(wcFish);
  fLog.AppendLog('Terrain loaded');
end;


procedure TTerrain.SyncLoad;
var i,k:integer;
begin
  for i:=1 to MapY do for k:=1 to MapX do
    Land[i,k].IsUnit := fPlayers.GetUnitByID(cardinal(Land[i,k].IsUnit));
end;


{ This whole thing is very CPU intesive, think of it - to update whole (192*192) tiles map }
//Don't use any advanced math here, only simpliest operations - + div *
procedure TTerrain.UpdateState;
var i,k,h,j:integer;
  procedure SetLand(X,Y,aTile,aObj:byte);
  begin
    Land[Y,X].Terrain := aTile;
    Land[Y,X].Obj     := aObj;
  end;
begin
  inc(fAnimStep);

  for i:=FallingTrees.Count downto 1 do
  if fAnimStep - FallingTrees.Tag2[i] > MapElem[FallingTrees.Tag[i]+1].Count-1 then
    fTerrain.ChopTree(FallingTrees.List[i]); //Make the tree turn into a stump

  for i:=1 to MapY do
  for k:=1 to MapX do
  //All those global things can be performed once a sec, or even less frequent
  if (i*MapX+k+fAnimStep) mod TERRAIN_PACE = 0 then
  begin

    if InRange(Land[i,k].FieldAge,1,65534) then
    begin
      inc(Land[i,k].FieldAge);
      if TileIsCornField(KMPoint(k,i)) then
        case Land[i,k].FieldAge of
          CORN_AGE1: SetLand(k,i,61,255);
          CORN_AGE2: SetLand(k,i,59,255);
          CORN_AGE3: SetLand(k,i,60,58);
          CORN_AGEFULL: begin SetLand(k,i,60,59); Land[i,k].FieldAge:=65535; end; //Skip to the end
        end
      else
      if TileIsWineField(KMPoint(k,i)) then
        case Land[i,k].FieldAge of
          WINE_AGE1: SetLand(k,i,55,54); //54=naked weeds
          WINE_AGE2: SetLand(k,i,55,55);
          WINE_AGE3: SetLand(k,i,55,56);
          WINE_AGEFULL: begin SetLand(k,i,55,57); Land[i,k].FieldAge:=65535; end;//Skip to the end
        end;
    end;

    if InRange(Land[i,k].TreeAge,1,TreeAgeFull) then
    begin
      inc(Land[i,k].TreeAge);
      if (Land[i,k].TreeAge=TreeAge1)or(Land[i,k].TreeAge=TreeAge2)or
         (Land[i,k].TreeAge=TreeAgeFull) then //Speedup
      for h:=1 to length(ChopableTrees) do
        for j:=1 to 3 do
          if Land[i,k].Obj=ChopableTrees[h,j] then
            case Land[i,k].TreeAge of
              TreeAge1:    Land[i,k].Obj:=ChopableTrees[h,2];
              TreeAge2:    Land[i,k].Obj:=ChopableTrees[h,3];
              TreeAgeFull: Land[i,k].Obj:=ChopableTrees[h,4];
            end;
    end;

  end;
end;


//Only MapEd accesses it
procedure TTerrain.UpdateStateIdle;
begin
  case GameCursor.Mode of
    cm_Height:
              if (ssLeft in GameCursor.SState) or (ssRight in GameCursor.SState) then
              MapEdHeight(KMPointF(GameCursor.Float.X+1,GameCursor.Float.Y+1), GameCursor.Tag1, GameCursor.Tag2, ssLeft in GameCursor.SState);
    cm_Tiles:
              if (ssLeft in GameCursor.SState) then
                if GameCursor.Tag2 in [0..3] then //Defined direction
                  MapEdTile(GameCursor.Cell, GameCursor.Tag1, GameCursor.Tag2)
                else //Random direction
                  MapEdTile(GameCursor.Cell, GameCursor.Tag1, Random(4));
  end;
end;


procedure TTerrain.Paint;
var x1,x2,y1,y2:integer;
begin
  x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
  y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

  fRender.RenderTerrain(x1,x2,y1,y2,fAnimStep);
  fRender.RenderTerrainFieldBorders(x1,x2,y1,y2);
  fRender.RenderTerrainObjects(x1,x2,y1,y2,fAnimStep);

  if SHOW_TERRAIN_WIRES then fRender.RenderDebugWires(x1,x2,y1,y2);
  if SHOW_TERRAIN_WIRES then fRender.RenderDebugPassability(x1,x2,y1,y2);
  if SHOW_UNIT_MOVEMENT then fRender.RenderDebugUnitMoves(x1,x2,y1,y2);
end;


end.
