unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
     KM_Defaults, KM_CommonTypes, KM_Player, KM_Units, KM_Units_Warrior, KM_Utils, KM_Houses,
     KM_PathFinding, KM_Points;


const MAX_MAP_SIZE = 192;


type
  TTreeAct = (taChop, taPlant, taAny);

  {Class to store all terrain data, aswell terrain routines}
  TTerrain = class
  private
    fAnimStep:integer;
    fMapX,fMapY:integer; //Terrain width and height
  public

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

    Pathfinding: TPathFinding;
    FallingTrees: TKMPointTagList;
    MiniMapRGB:array[1..MAX_MAP_SIZE,1..MAX_MAP_SIZE]of cardinal;

    constructor Create;
    destructor Destroy; override;
    procedure MakeNewMap(Width,Height:integer);

    property MapX: integer read fMapX;
    property MapY: integer read fMapY;

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

    function CanPlaceUnit(Loc:TKMPoint; aUnitType: TUnitType):boolean;
    function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; aPlayer:TKMPlayer):boolean;
    function CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup; aPlayer:TKMPlayer):boolean;
    function CheckHeightPass(aLoc:TKMPoint; aPass:TPassability):boolean;
    procedure AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);

    function FindField(aLoc:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir):Boolean;
    function FindStone(aLoc:TKMPoint; aRadius:integer; aAvoidLoc:TKMPoint; out StonePoint: TKMPointDir):Boolean;
    function FindOre(aLoc:TKMPoint; Rt:TResourceType; out OrePoint: TKMPoint):Boolean;
    function FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aTreeAct: TTreeAct; out Tree: TKMPointDir; out TreeAct: TTreeAct): Boolean;
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
    function FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: boolean=false):Boolean;
    function Route_CanBeMade(LocA, LocB:TKMPoint; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
    function Route_CanBeMadeToVertex(LocA, LocB:TKMPoint; aPass:TPassability):boolean;
    function Route_CanBeMadeToHouse(LocA:TKMPoint; aHouse:TKMHouse; aPass:TPassability; aDistance:single; aInteractionAvoid:boolean):boolean;
    function GetClosestTile(TargetLoc, OriginLoc:TKMPoint; aPass:TPassability; aAcceptTargetLoc:boolean=true):TKMPoint;

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
    function TileIsStone(X,Y:Word):byte;
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

    procedure UpdateBorders(Loc:TKMPoint; CheckSurrounding:boolean=true);
    procedure FlattenTerrain(Loc:TKMPoint; aRebuildWalkConnects:boolean=true); overload;
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

    procedure IncAnimStep; //Lite-weight UpdateState for MapEd
    property AnimStep: integer read fAnimStep;

    procedure LoadFromFile(FileName:string);
    procedure SaveToFile(aFile:string);

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure UpdateStateIdle;
    procedure UpdateMinimapData(aMapEditor:Boolean);
    procedure Paint;
  end;


var
  fTerrain: TTerrain;


implementation
uses KM_Viewport, KM_Render, KM_RenderAux, KM_PlayersCollection, KM_Sound, KM_UnitActionStay, KM_Game, KM_ResourceGFX, KM_ResourceHouse, KM_Log;


{ TTerrain }
constructor TTerrain.Create;
begin
  Inherited;
  fAnimStep := 0;
  FallingTrees := TKMPointTagList.Create;
  Pathfinding := TPathFinding.Create;
end;


destructor TTerrain.Destroy;
begin
  FreeAndNil(Pathfinding);
  FreeAndNil(FallingTrees);
  Inherited;
end;


//Reset whole map with default values
procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k:integer;
begin
  fMapX := Min(Width, MAX_MAP_SIZE);
  fMapY := Min(Height,MAX_MAP_SIZE);

  for i:=1 to fMapY do for k:=1 to fMapX do with Land[i,k] do begin
    Terrain      := 0;
    Height       := KaMRandom(7);    //variation in height
    Rotation     := KaMRandom(4);  //Make it random
    OldTerrain   := 0;
    OldRotation  := 0;
    Obj          := 255;             //none
    if KaMRandom(16)=0 then Obj := ChopableTrees[KaMRandom(13)+1,4];
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

  PathFinding.UpdateMapSize(fMapX, fMapY);

  RebuildLighting(1,fMapX,1,fMapY);
  RebuildPassability(1,fMapX,1,fMapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcFish);
end;


procedure TTerrain.LoadFromFile(FileName:string);
var
  i,k:integer;
  S:TKMemoryStream;
  NewX,NewY:integer;
begin
  if not CheckFileExists(FileName) then Exit;

  fLog.AppendLog('Loading map file: '+FileName);

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX <= MAX_MAP_SIZE) and (NewY <= MAX_MAP_SIZE), 'Can''t open the map cos it has too big dimensions');
    fMapX := NewX;
    fMapY := NewY;
    MakeNewMap(fMapX, fMapY); //Reset whole map to default
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
      if ObjectIsChopableTree(KMPoint(k,i),2) then Land[i,k].TreeAge := TreeAge1;
      if ObjectIsChopableTree(KMPoint(k,i),3) then Land[i,k].TreeAge := TreeAge2;
      if ObjectIsChopableTree(KMPoint(k,i),4) then Land[i,k].TreeAge := TreeAgeFull;
      //Everything else is default
    end;
  finally
    S.Free;
  end;

  PathFinding.UpdateMapSize(fMapX, fMapY);

  RebuildLighting(1,fMapX,1,fMapY);
  RebuildPassability(1,fMapX,1,fMapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcFish);
  fLog.AppendLog('Map file loaded');
end;


//Save (export) map in KaM .map format with additional tile information on the end?
procedure TTerrain.SaveToFile(aFile:string);
var f:file; i,k:integer; c0,cF:cardinal; light,b205:byte;
    ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
    Res:array[1..MAX_MAP_SIZE*2]of packed record X1,Y1,X2,Y2:integer; Typ:byte; end;
begin

  if not DirectoryExists(ExtractFilePath(aFile)) then
  CreateDir(ExtractFilePath(aFile));

  assignfile(f,aFile); rewrite(f,1);

  blockwrite(f,fMapX,4);
  blockwrite(f,fMapY,4);

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
  for i:=max(Y-1,1) to min(Y+1,fMapY) do for k:=max(X-1,1) to min(X+1,fMapX) do
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
  RequiredMaxRad: single;
  U,C,W:TKMUnit; //CurrentUnit, BestWarrior, BestCitizen
begin
  W := nil;
  C := nil;

  //Scan one tile further than the maximum radius due to rounding
  lx := max(round(aLoc.X-(MaxRad+1)),1); //1.42 gets rounded to 1
  ly := max(round(aLoc.Y-(MaxRad+1)),1); //1.42 gets rounded to 1
  hx := min(round(aLoc.X+(MaxRad+1)),fMapX); //1.42 gets rounded to 1
  hy := min(round(aLoc.Y+(MaxRad+1)),fMapY); //1.42 gets rounded to 1

  for i:=ly to hy do for k:=lx to hx do
  if (fPlayers.Player[aPlayer].FogOfWar.CheckTileRevelation(k,i) = 255)
    and (Land[i,k].IsUnit <> nil) then
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

    //@Krom: Please let me know if this makes any sense. I want to comment it so we remember why
    //       we use U.GetPosition instead of KMPoint(k,i) in checks. Rewrite my comments if you like.

    //This unit could be on a different tile next to KMPoint(k,i), so we cannot use that anymore.
    //There was a crash caused by VertexUsageCompatible checking (k,i) instead of U.GetPosition.
    //In that case aLoc = (37,54) and k,i = (39;52) but U.GetPosition = (38;53).
    //This shows why you can't use (k,i) in checks because it is distance >2 from aLoc! (in melee fight)
    if U = nil then Continue;
    P := U.GetPosition;

    RequiredMaxRad := MaxRad;
    if (MaxRad = 1) and KMStepIsDiag(aLoc, P) then
      RequiredMaxRad := 1.42; //Use diagonal radius sqrt(2) instead

    if (U <> nil) and
       U.Visible and //Inside of house
       CanWalkDiagonaly(aLoc,P) and
       (fPlayers.CheckAlliance(aPlayer, U.GetOwner) = aAlliance) and //How do WE feel about enemy, not how they feel about us
       ((abs(aLoc.X - P.X) <> 1) or (abs(aLoc.Y - P.Y) <> 1) or VertexUsageCompatible(aLoc,P)) and
       (InRange(GetLength(KMPointF(aLoc), U.PositionF), MinRad, RequiredMaxRad)) and //Unit's exact position must be close enough
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
  
  aList.GetTopLeft(TL);
  aList.GetBottomRight(BR);
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
  for i:=1 to fMapY do for k:=1 to fMapX do
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
function TTerrain.FindField(aLoc:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean; aAvoidLoc:TKMPoint; out FieldPoint:TKMPointDir):Boolean;
var i,k:integer; List:TKMPointDirList;
begin
  List := TKMPointDirList.Create;
  for i:=max(aLoc.Y-aRadius,1) to min(aLoc.Y+aRadius,fMapY-1) do
  for k:=max(aLoc.X-aRadius,1) to min(aLoc.X+aRadius,fMapX-1) do
    if (KMLength(aLoc,KMPoint(k,i))<=aRadius) and not KMSamePoint(aAvoidLoc,KMPoint(k,i)) then
      if ((aFieldType=ft_Corn) and TileIsCornField(KMPoint(k,i)))or
         ((aFieldType=ft_Wine) and TileIsWineField(KMPoint(k,i))) then
        if ((aAgeFull)and(Land[i,k].FieldAge=65535))or
           ((not aAgeFull)and(Land[i,k].FieldAge=0)) then
          if Route_CanBeMade(aLoc,KMPoint(k,i),CanWalk,0,false) then
            List.AddEntry(KMPointDir(k, i, dir_NA));

  Result := List.GetRandom(FieldPoint);
  List.Free;
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
        if (CanWalk in Land[i+1,k].Passability) then //Now check the tile right below
          if Route_CanBeMade(aLoc,KMPoint(k,i+1),CanWalk,0,false) then
            List.AddEntry(KMPointDir(k, i+1, dir_N));

  Result := List.GetRandom(StonePoint);
  List.Free;
end;


function TTerrain.FindOre(aLoc:TKMPoint; Rt:TResourceType; out OrePoint: TKMPoint):Boolean;
var i,k,RadLeft,RadRight,RadTop,RadBottom,R1,R2,R3,R4:integer; L:array[1..4]of TKMPointList;
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
function TTerrain.FindTree(aLoc: TKMPoint; aRadius: Word; aAvoidLoc: TKMPoint; aTreeAct: TTreeAct; out Tree: TKMPointDir; out TreeAct: TTreeAct): Boolean;
var
  List1, List2, List3: TKMPointList;
  i,k: Integer;
  T: TKMPoint;
  BestSlope, Slope: Integer;
begin
  List1 := TKMPointList.Create; //Trees
  List2 := TKMPointList.Create; //Stumps (preferred when planting a new tree)
  List3 := TKMPointList.Create; //Clear places

  //Scan terrain and add all trees/spots into lists
  for i:=max(aLoc.Y-aRadius,1) to min(aLoc.Y+aRadius,fMapY-1) do
  for k:=max(aLoc.X-aRadius,1) to min(aLoc.X+aRadius,fMapX-1) do
  begin
    T := KMPoint(k,i);

    if (KMLength(aLoc, T) <= aRadius)
    and not KMSamePoint(aAvoidLoc, T) then
    begin

      //Grownup tree
      if (aTreeAct in [taChop, taAny])
      and ObjectIsChopableTree(T, 4)
      and (Land[i,k].TreeAge >= TreeAgeFull)
      and Route_CanBeMadeToVertex(aLoc, T, CanWalk) then
        List1.AddEntry(T);

      if (aTreeAct in [taPlant, taAny])
      and (CanPlantTrees in Land[i,k].Passability)
      and Route_CanBeMade(aLoc, T, CanWalk, 0, False) then
        if ObjectIsChopableTree(T, 6) then
          List2.AddEntry(T) //Stump
        else
          List3.AddEntry(T); //Empty place
    end;
  end;

  //Convert taAny to either a Tree or a Spot
  //Average tree uses ~9 tiles, hence we correct Places weight by some amount (15 fits good) to make choice between Plant and Chop more even. Feel free to fine-tune it
  if (List1.Count > 8) //Always chop the tree if there are many
  or (List2.Count + List3.Count = 0)
  or ((List1.Count > 0) and (KaMRandom < List1.Count / (List1.Count + (List2.Count + List3.Count)/15))) then
  begin
    Result := List1.GetRandom(T);
    TreeAct := taChop;
  end else begin
    Result := List2.GetRandom(T);
    if not Result then
      Result := List3.GetRandom(T);
    TreeAct := taPlant;
  end;

  //Note: Result can still be False
  List1.Free;
  List2.Free;
  List3.Free;

  //Choose direction of approach based on which one is the flatest (animation looks odd if not flat)
  if (TreeAct = taChop) and Result then
  begin
    BestSlope := 255;
    Result := False; //It is already tested that we can walk to the tree, but double-check

    for i:=-1 to 0 do for k:=-1 to 0 do
    if Route_CanBeMade(aLoc, KMPoint(T.X+k, T.Y+i), CanWalk, 0, False) then
    begin
      Slope := MixLandHeight(T.X+k, T.Y+i) - Land[T.Y, T.X].Height;
      if (Abs(Slope) < BestSlope) and ((i <> 0) or (Slope >= 0)) then
      begin
        Tree := KMPointDir(T.X+k, T.Y+i, KMGetVertexDir(k, i));
        Result := True;
        BestSlope := Abs(Slope);
      end;
    end;  
  end;

  if (TreeAct = taPlant) and Result then
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
            if TileInMapCoords(k+j,i+l) and ((l <> 0) or (j <> 0)) then
              // D) Final check: route can be made and isn't avoid loc
              if Route_CanBeMade(aLoc, KMPoint(k+j, i+l), CanWalk, 0,false)
              and not KMSamePoint(aAvoidLoc,KMPoint(k+j,i+l)) then
                List.AddEntry(KMPointDir(k+j, i+l, KMGetDirection(j,l)));

  Result := List.GetRandom(FishPoint);
  List.Free;
end;


function TTerrain.CanFindFishingWater(aLoc:TKMPoint; aRadius:integer):boolean;
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
var
  i,k,s,t:integer;
  P2:TKMPoint;
  AllowBuild:boolean;
  HA: THouseArea;

  procedure MarkPoint(aPoint:TKMPoint; aID:integer);
  var v: integer;
  begin
    for v:=1 to aList.Count do //Skip wires from comparison
      if (aList.Tag[v] <> 0) and KMSamePoint(aList.List[v],aPoint) then
        Exit;
    aList.AddEntry(aPoint, aID, 0);
  end;
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

      //Forbid planning on unrevealed areas
      AllowBuild := AllowBuild and (MyPlayer.FogOfWar.CheckTileRevelation(P2.X,P2.Y) > 0);

      //Check surrounding tiles in +/- 1 range for other houses pressence
      if not (CanBuild in Land[P2.Y,P2.X].Passability) then
      for s:=-1 to 1 do for t:=-1 to 1 do
      if (s<>0)or(t<>0) then  //This is a surrounding tile, not the actual tile
      if Land[P2.Y+t,P2.X+s].Markup in [mu_HousePlan, mu_HouseFenceCanWalk, mu_HouseFenceNoWalk, mu_House] then
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
    else        raise ELocError.Create('Wrong resource deposit',Loc);
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
    RecalculatePassability(Loc);
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
  RecalculatePassability(Loc);
end;


procedure TTerrain.RecalculatePassabilityAround(Loc:TKMPoint);
begin
  RebuildPassability(Loc.X-1,Loc.X+1,Loc.Y-1,Loc.Y+1);
end;


procedure TTerrain.RecalculatePassability(Loc:TKMPoint);
var
  i,k:integer;
  HousesNearBy:boolean;

  procedure AddPassability(aLoc:TKMPoint; aPass:TPassabilitySet);
  begin
    Land[aLoc.Y,aLoc.X].Passability:=Land[aLoc.Y,aLoc.X].Passability + aPass;
  end;

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
  Assert(TileInMapCoords(Loc.X,Loc.Y)); //First of all exclude all tiles outside of actual map

  Land[Loc.Y,Loc.X].Passability := [];

  //For all passability types other than CanAll, houses and fenced houses are excluded
  if not(Land[Loc.Y,Loc.X].Markup in [mu_House, mu_HouseFenceNoWalk]) then begin

   if (TileIsWalkable(Loc))and
      (Land[Loc.Y,Loc.X].TileOverlay<>to_Wall)and
      (not MapElem[Land[Loc.Y,Loc.X].Obj+1].AllBlocked)and
      CheckHeightPass(Loc,CanWalk) then
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
      (not TileIsWineField(Loc))and
      (not TileIsCornField(Loc))and
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
      TempUnit := UnitsHitTest(L1.List[i].X, L1.List[i].Y);
      if TempUnit <> nil then
        if (TempUnit.GetUnitAction is TUnitActionStay) and (not TUnitActionStay(TempUnit.GetUnitAction).Locked) then
          L3.AddEntry(L1.List[i]);
    end;

  if not(L2.GetRandom(Result)) then
  if not(L3.GetRandom(Result)) then
  if Loc2IsOk then //If there are no free or idle tiles then the unit that pushed us is a good option (exchange)
    Result := Loc2
  else
  if not(L1.GetRandom(Result)) then
    Result := Loc;

  L1.Free;
  L2.Free;
  L3.Free;
end;


function TTerrain.FindSideStepPosition(Loc,Loc2,Loc3:TKMPoint; aPass: TPassability; out SidePoint: TKMPoint; OnlyTakeBest: boolean=false):Boolean;
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
  if not HasUnit(KMPoint(Loc.X+k,Loc.Y+i)) then //Doesn't have a unit
    L1.AddEntry(KMPoint(Loc.X+k,Loc.Y+i));

  //List 2 holds the best positions, ones which are also next to Loc3 (next position)
  L2 := TKMPointList.Create;
  if not KMSamePoint(Loc3, KMPoint(0,0)) then //No Loc3 was given
  for i:=1 to L1.Count do
    if KMLength(L1.List[i],Loc3) < 1.5 then //Next to Loc3 (diagonal is ok)
      L2.AddEntry(L1.List[i]);

  Result := True;
  if not(L2.GetRandom(SidePoint)) then
  if (OnlyTakeBest) or (not(L1.GetRandom(SidePoint))) then
    Result := false; //No side step positions available

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
      LocA := KMPointBelow(aHouse.GetEntrance);
      aPass := CanWalk;
    end;
  end;

  //target point has to be walkable
  Result := Result and CheckPassability(LocA,aPass);
  TestRadius := false;
  for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
  for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
  if GetLength(LocB,KMPoint(k,i))<=aDistance then
    TestRadius := TestRadius or CheckPassability(KMPoint(k,i),aPass);
  Result := Result and TestRadius;

  //There's a walkable way between A and B (which is proved by FloodFill test on map init)
  if aPass=CanWalk then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcWalk] = Land[i,k].WalkConnect[wcWalk]);
    Result := Result and TestRadius;
  end;

  if aPass=CanWalkRoad then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcRoad] = Land[i,k].WalkConnect[wcRoad]);
    Result := Result and TestRadius;
  end;

  if aPass=CanFish then
    Result := Result and (Land[LocA.Y,LocA.X].WalkConnect[wcFish] = Land[LocB.Y,LocB.X].WalkConnect[wcFish]);

  if aInteractionAvoid then
  begin
    TestRadius := false;
    for i:=max(round(LocB.Y-aDistance),1) to min(round(LocB.Y+aDistance),fMapY-1) do
    for k:=max(round(LocB.X-aDistance),1) to min(round(LocB.X+aDistance),fMapX-1) do
    if GetLength(LocB,KMPoint(k,i))<=aDistance then
      TestRadius := TestRadius or (Land[LocA.Y,LocA.X].WalkConnect[wcAvoid] = Land[i,k].WalkConnect[wcAvoid]);
    Result := Result and TestRadius;
  end;
end;


//Check if a route can be made to this vertex, from any direction (used for woodcutter cutting trees)
function TTerrain.Route_CanBeMadeToVertex(LocA, LocB:TKMPoint; aPass:TPassability):boolean;
var i,k:integer;
begin
  Result := false;
  //Check from top-left of vertex to vertex tile itself
  for i := Max(LocB.Y-1,1) to LocB.Y do
    for k := Max(LocB.X-1,1) to LocB.X do
      Result := Result or Route_CanBeMade(LocA,KMPoint(k,i),aPass,0,false);
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


//Returns the closest tile to TargetLoc with aPass and walk connect to OriginLoc
//If no tile found - return Origin location
function TTerrain.GetClosestTile(TargetLoc, OriginLoc:TKMPoint; aPass:TPassability; aAcceptTargetLoc:boolean=true):TKMPoint;
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


//Interpolate between 12 vertices surrounding this tile (X and Y, no diagonals)
//Also it is FlattenTerrain duty to preserve walkability if there are units standing
procedure TTerrain.FlattenTerrain(Loc:TKMPoint; aRebuildWalkConnects:boolean=true);

  //If tiles with units standing on them become unwalkable we should try to fix them
  procedure EnsureWalkable(aX,aY:word);
  begin
    //We did not recalculated passability yet, hence tile has CanWalk but CheckHeightPass=False already
    if (Land[aY,aX].IsUnit <> nil)
    and CheckPassability(KMPoint(aX,aY),CanWalk)
    and not CheckHeightPass(KMPoint(aX,aY),CanWalk)
    and (fGame.GameState <> gsEditor) //Allow units to become "stuck" in MapEd, as height changing is allowed anywhere
    then
      //This recursive call should be garanteed to exit, as eventually the terrain will be flat enough
      FlattenTerrain(KMPoint(aX,aY), False); //WalkConnect should be done at the end
  end;

  function GetHeight(X,Y:word):byte;
  begin
    Result := Land[EnsureRange(Y,1,fMapY), EnsureRange(X,1,fMapX)].Height;
  end;

var i,k: word; Avg:byte;
begin
  Assert(TileInMapCoords(Loc.X, Loc.Y));

  Avg := Round((                      GetHeight(Loc.X,Loc.Y-1) + GetHeight(Loc.X+1,Loc.Y-1) +
         GetHeight(Loc.X-1,Loc.Y  ) + GetHeight(Loc.X,Loc.Y  ) + GetHeight(Loc.X+1,Loc.Y  ) + GetHeight(Loc.X+2,Loc.Y  ) +
         GetHeight(Loc.X-1,Loc.Y+1) + GetHeight(Loc.X,Loc.Y+1) + GetHeight(Loc.X+1,Loc.Y+1) + GetHeight(Loc.X+2,Loc.Y+1) +
                                      GetHeight(Loc.X,Loc.Y+2) + GetHeight(Loc.X+1,Loc.Y+2) ) / 12);

  if CanElevate in Land[Loc.Y  ,Loc.X  ].Passability then Land[Loc.Y  ,Loc.X  ].Height := Mix(Avg, Land[Loc.Y  ,Loc.X  ].Height, 0.5);
  if CanElevate in Land[Loc.Y  ,Loc.X+1].Passability then Land[Loc.Y  ,Loc.X+1].Height := Mix(Avg, Land[Loc.Y  ,Loc.X+1].Height, 0.5);
  if CanElevate in Land[Loc.Y+1,Loc.X  ].Passability then Land[Loc.Y+1,Loc.X  ].Height := Mix(Avg, Land[Loc.Y+1,Loc.X  ].Height, 0.5);
  if CanElevate in Land[Loc.Y+1,Loc.X+1].Passability then Land[Loc.Y+1,Loc.X+1].Height := Mix(Avg, Land[Loc.Y+1,Loc.X+1].Height, 0.5);

  //All 9 tiles around and including this one could have become unwalkable and made a unit stuck, so check them all
  for i:=Max(Loc.Y-1,1) to Min(Loc.Y+1,fMapY-1) do
    for k:=Max(Loc.X-1,1) to Min(Loc.X+1,fMapX-1) do
      EnsureWalkable(k,i);

  RebuildLighting(Loc.X-2,Loc.X+3,Loc.Y-2,Loc.Y+3);
  RecalculatePassabilityAround(Loc); //Changing height will affect the cells around this one

  if aRebuildWalkConnects then
  begin
    RebuildWalkConnect(wcWalk);
    RebuildWalkConnect(wcRoad);
  end;
end;


{Flatten a list of points in the mission init}
procedure TTerrain.FlattenTerrain(LocList:TKMPointList);
var i:integer;
begin
  for i:=1 to LocList.Count do
    FlattenTerrain(LocList.List[i], false); //Rebuild the Walk Connect at the end, rather than every time

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
      x0:=EnsureRange(k-1,1,fMapX);
      y2:=EnsureRange(i+1,1,fMapY);
      if VerticeInMapCoords(x0,y2) then
        Land[i,k].Light:=EnsureRange((Land[i,k].Height-(Land[y2,k].Height+Land[i,x0].Height)/2)/22,-1,1); //  1.33*16 ~=22
    if (i=1) or (i=fMapY) or (k=1) or (k=fMapX) then //Map borders fade to black
      Land[i,k].Light := -1;
    end;
end;


{ Rebuilds passability for given bounds }
procedure TTerrain.RebuildPassability(LowX,HighX,LowY,HighY:integer);
var i,k:integer;
begin
  for i:=max(LowY,1) to min(HighY,fMapY-1) do for k:=max(LowX,1) to min(HighX,fMapX-1) do
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
     ( (wcType=wcAvoid) and not TileIsLocked(KMPoint(x,y)) )) then //Matches passability
    begin
      Land[y,x].WalkConnect[wcType]:=ID;
      inc(Count);
      //Using custom TileInMapCoords replacement gives ~40% speed improvement
      if x-1>=1 then begin
        if y-1>=1 then    FillArea(x-1,y-1,ID,Count);
                          FillArea(x-1,y  ,ID,Count);
        if y+1<=fMapY then FillArea(x-1,y+1,ID,Count);
      end;

      if y-1>=1 then    FillArea(x,y-1,ID,Count);
      if y+1<=fMapY then FillArea(x,y+1,ID,Count);

      if x+1<=fMapX then begin
        if y-1>=1 then    FillArea(x+1,y-1,ID,Count);
                          FillArea(x+1,y  ,ID,Count);
        if y+1<=fMapY then FillArea(x+1,y+1,ID,Count);
      end;
    end;
  end;

begin
    case wcType of
      wcWalk:  Pass := CanWalk;
      wcRoad:  Pass := CanWalkRoad;
      wcFish:  Pass := CanFish;
      wcAvoid: Pass := CanWalk; //Special case for unit interaction avoiding
      else     Assert(false, 'Unexpected aPass in RebuildWalkConnect function');
    end;

    //Reset everything
    for i:=1 to fMapY do for k:=1 to fMapX do
      Land[i,k].WalkConnect[wcType] := 0;

    AreaID := 0;
    for i:=1 to fMapY do for k:=1 to fMapX do
    if (Land[i,k].WalkConnect[wcType]=0) and (Pass in Land[i,k].Passability) and
     ((wcType <> wcAvoid)or
     ( (wcType=wcAvoid) and not TileIsLocked(KMPoint(k,i)) )) then
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

      Assert(AreaID<255,'RebuildWalkConnect failed due too many unconnected areas');
    end;
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TTerrain.SetHouse(Loc:TKMPoint; aHouseType: THouseType; aHouseStage:THouseStage; aOwner:TPlayerIndex; const aFlattenTerrain:boolean=false);
var i,k,x,y:word; ToFlatten:TKMPointList; HA:THouseArea;
begin
  if aFlattenTerrain then //We will check aFlattenTerrain only once, otherwise there are compiler warnings
    ToFlatten := TKMPointList.Create
  else
    ToFlatten := nil;

  if aHouseStage in [hs_None, hs_Plan] then
    SetHouseAreaOwner(Loc, aHouseType, -1)
  else
    SetHouseAreaOwner(Loc, aHouseType, aOwner);

  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
    if HA[i,k]<>0 then
    begin
      x:=Loc.X+k-3; y:=Loc.Y+i-4;
      if TileInMapCoords(x,y) then
      begin

        if (HA[i,k]=2)and(aHouseStage=hs_Built) then
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
  RebuildPassability(Loc.X-3,Loc.X+2,Loc.Y-4,Loc.Y+1);
  RebuildWalkConnect(wcRoad);
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
function TTerrain.CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; aPlayer:TKMPlayer):boolean;
var i,k:integer; HA:THouseArea;
begin
  Result:=true;
  HA := fResource.HouseDat[aHouseType].BuildArea;
  Loc.X:=Loc.X-fResource.HouseDat[aHouseType].EntranceOffsetX; //update offset
  for i:=1 to 4 do for k:=1 to 4 do
    if HA[i,k]<>0 then begin
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
    CanBuild,CanBuildGold,CanBuildIron: Result := TestHeight(18);
  end; //For other passabilities we ignore height (return default true)
end;


procedure TTerrain.AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);
var i,k:integer; HA:THouseArea;
begin
  HA := fResource.HouseDat[aHouseType].BuildArea;

  if aBuildState in [hbs_Stone, hbs_Done] then //only leave rubble if the construction was well underway (stone and above)
  begin
    //For houses that are at least partually built (leaves rubble)
    for i:=2 to 4 do for k:=2 to 4 do
      if HA[i-1,k]<>0 then
      if HA[i,k-1]<>0 then
      if HA[i-1,k-1]<>0 then
      if HA[i,k]<>0 then
        Land[Loc.Y+i-4,Loc.X+k-3].Obj:=68+KaMRandom(6);
    for i:=1 to 4 do for k:=1 to 4 do
      if (HA[i,k] in [1,2]) then
      begin
        Land[Loc.Y+i-4,Loc.X+k-3].TileOverlay:=to_Dig3;
        Land[Loc.Y+i-4,Loc.X+k-3].Markup:=mu_None;
      end;
  end
  else begin
    //For glyphs
    for i:=1 to 4 do for k:=1 to 4 do
      if (HA[i,k] in [1,2]) then
        Land[Loc.Y+i-4,Loc.X+k-3].Markup:=mu_None;

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
  Result.X := EnsureRange(round(GameCursor.Float.X+1),1,fMapX);
  Result.Y := EnsureRange(round(GameCursor.Float.Y+1),1,fMapY);
end;


{Compute cursor position and store it in global variables}
procedure TTerrain.ComputeCursorPosition(X,Y:word; Shift: TShiftState);
begin
  with GameCursor do
  begin
    Float.X := fViewport.Position.X + (X-fViewport.ViewRect.Right/2-TOOLBAR_WIDTH/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := fViewport.Position.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
    Float.Y := ConvertCursorToMapCoord(Float.X,Float.Y);

    Cell.X := EnsureRange(round(Float.X+0.5), 1, fMapX); //Cell below cursor in map bounds
    Cell.Y := EnsureRange(round(Float.Y+0.5), 1, fMapY);

    SState := Shift;
  end;
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


{Return height within cell interpolating node heights}
function TTerrain.InterpolateLandHeight(inX,inY:single):single;
var Xc,Yc:integer; Tmp1,Tmp2:single;
begin
  //todo: Make this match KaM by creating some comparision screenshots of slopes, hills, etc.
  Xc := trunc(inX);
  Yc := trunc(inY);
  Result := 0;
  if not VerticeInMapCoords(Xc,Yc) then exit;

  Tmp1 := mix(Land[Yc  ,Xc+1].Height, Land[Yc  ,Xc].Height, frac(inX));
  if Yc >= MAX_MAP_SIZE then
    Tmp2 := 0
  else
    Tmp2 := mix(Land[Yc+1,Xc+1].Height, Land[Yc+1,Xc].Height, frac(inX));
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


//MapEditor stores only commanders instead of all groups members
procedure TTerrain.UpdateMinimapData(aMapEditor:Boolean);
var
  FOW,ID:byte;
  i,j,k:integer;
  W: TKMUnitWarrior;
  P: TKMPoint;
  DoesFit: Boolean;
  Light:smallint;
begin
  for i:=1 to fMapY do
  for k:=1 to fMapX do
  begin
    FOW := MyPlayer.FogOfWar.CheckTileRevelation(k,i);
    if FOW = 0 then
      MiniMapRGB[i,k] := 0
    else
      if Land[i,k].TileOwner <> -1 then
        MiniMapRGB[i,k] := fPlayers.Player[Land[i,k].TileOwner].FlagColor
      else
        if Land[i,k].IsUnit <> nil then
          if Land[i,k].IsUnit.GetOwner <> PLAYER_ANIMAL then
            MiniMapRGB[i,k] := fPlayers.Player[Land[i,k].IsUnit.GetOwner].FlagColor
          else
            MiniMapRGB[i,k] := fResource.UnitDat[Land[i,k].IsUnit.UnitType].MinimapColor
        else
        begin
          ID := Land[i,k].Terrain;
          Light := round(Land[i,k].Light*64)-(255-FOW); //it's -255..255 range now
          MiniMapRGB[i,k] := EnsureRange(fResource.Tileset.TileColor[ID].R+Light,0,255) +
                             EnsureRange(fResource.Tileset.TileColor[ID].G+Light,0,255) shl 8 +
                             EnsureRange(fResource.Tileset.TileColor[ID].B+Light,0,255) shl 16;
        end;
  end;

  //Scan all players units and paint all virtual group members
  if aMapEditor then
    for i:=0 to fPlayers.Count-1 do
      for k:=0 to fPlayers[i].Units.Count-1 do
        if fPlayers[i].Units[k] is TKMUnitWarrior then
        begin
          W := TKMUnitWarrior(fPlayers[i].Units[k]);
          for j:=1 to W.fMapEdMembersCount do
          begin
            P := GetPositionInGroup2(W.GetPosition.X, W.GetPosition.Y, W.Direction, j+1, W.UnitsPerRow, MapX, MapY, DoesFit);
            if not DoesFit then Continue; //Don't render units that are off the map in the map editor
            MiniMapRGB[P.Y,P.X] := fPlayers[i].FlagColor;
          end;
        end;
end;


procedure TTerrain.IncAnimStep;
begin
  inc(fAnimStep);
end;


procedure TTerrain.Save(SaveStream:TKMemoryStream);
var i,k:integer;
begin
  SaveStream.Write('Terrain');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);

  SaveStream.Write(fAnimStep);
  FallingTrees.Save(SaveStream);

  for i:=1 to fMapY do for k:=1 to fMapX do
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
      SaveStream.Write(Integer(0));
    SaveStream.Write(Land[i,k].IsVertexUnit,SizeOf(Land[i,k].IsVertexUnit));
  end;
end;


procedure TTerrain.Load(LoadStream:TKMemoryStream);
var i,k:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Terrain', 'Terrain not found');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);

  LoadStream.Read(fAnimStep);
  FallingTrees.Load(LoadStream);

  for i:=1 to fMapY do for k:=1 to fMapX do
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

  for i:=1 to fMapY do for k:=1 to fMapX do
    UpdateBorders(KMPoint(k,i),false);

  PathFinding.UpdateMapSize(fMapX, fMapY);

  RebuildLighting(1, fMapX, 1, fMapY);
  RebuildPassability(1, fMapX, 1, fMapY);
  RebuildWalkConnect(wcWalk);
  RebuildWalkConnect(wcRoad);
  RebuildWalkConnect(wcFish);
  fLog.AppendLog('Terrain loaded');
end;


procedure TTerrain.SyncLoad;
var i,k:integer;
begin
  for i:=1 to fMapY do for k:=1 to fMapX do
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
    ChopTree(FallingTrees.List[i]); //Make the tree turn into a stump

  for i:=1 to fMapY do
  for k:=1 to fMapX do
  //All those global things can be performed once a sec, or even less frequent
  if (i*fMapX+k+fAnimStep) mod TERRAIN_PACE = 0 then
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
                  MapEdTile(GameCursor.Cell, GameCursor.Tag1, KaMRandom(4));
  end;
end;


procedure TTerrain.Paint;
var x1,x2,y1,y2:integer; Passability:integer;
begin  
  x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
  y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

  fRender.RenderTerrain(x1,x2,y1,y2,fAnimStep);
  fRender.RenderTerrainFieldBorders(x1,x2,y1,y2);
  fRender.RenderTerrainObjects(x1,x2,y1,y2,fAnimStep);

  if SHOW_TERRAIN_WIRES then fRenderAux.Wires(x1,x2,y1,y2);
  if SHOW_TERRAIN_WIRES then
  begin
    Passability := fGame.FormPassability;
    if fGame.fMapEditorInterface <> nil then
      Passability := max(Passability, fGame.fMapEditorInterface.ShowPassability);
    fRenderAux.Passability(x1,x2,y1,y2, Passability);
  end;
  if SHOW_UNIT_MOVEMENT then fRenderAux.UnitMoves(x1,x2,y1,y2);
end;


end.
