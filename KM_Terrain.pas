unit KM_Terrain;
interface
uses Controls, StdCtrls, Math, KM_Defaults, KromUtils, SysUtils;

const
MaxMapSize=256; //I have a request, keep it 176 for now, as it will help to solve compatibility issues (just like those you've mentioned).

type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
private
  AnimStep:integer;
  FallingTrees: TKMPointList;

public
  MapX,MapY:integer; //Terrain width and height
  MM:array[1..MaxMapSize,1..MaxMapSize]of record R,G,B:byte; end;

  CursorPos:TKMPoint;

  Land:array[1..MaxMapSize,1..MaxMapSize]of record
    Terrain,Height,Rotation,Obj:byte;

    //KaM stores node lighting in 0..32 range (-16..16), but I want to use -1..1 range
    Light:single;

    //Meant to be set of allowed actions on the tile
    Passability:TPassabilitySet; //pass_CanWalk, pass_CanBuild, pass_CanPlantTrees, pass_CanField

    //Name says it all, should simplify player related issues
    TileOwner:TPlayerID; //Players 1..8

    //Markup (ropes) used on-top of tiles for roads/fields/wine
    //It can be placed independent from everything else
    Markup:TMarkup; //None, Road, Field, Wine

    //These are tiles layed on top of the terrain
    //with some degree of transparecy in them
    //hence they use own variable
    FieldType:TFieldType; //None, WIP, Road, Field, Wine

    //Age of field/wine, another independent variable
    //Depending on this special object maybe rendered (straw, grapes)
    FieldAge:word;  //Empty=0, 1, 2, 3, 4, Full=65535

    //Age of tree, another independent variable since trees can grow on fields
    //Depending on this tree gets older and thus could be chopped
    TreeAge:word;  //Empty=0, 1, 2, 3, 4, Full=65535

    //The animation step for a falling tree, because they must fall at the right time.
    FallingTreeAnimStep:integer;

    //SpecialObjects on field/wine, depends on FieldAge (x4 straw, x2 grapes)
    FieldSpecial:TFieldSpecial;  //fs_None, fs_Corn1, fs_Corn2, fs_Wine1, fs_Wine2, fs_Wine3, fs_Wine4, fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4

    //Another var for borders (ropes, planks, stones) Top and Left
    BorderX,BorderY:TBorderType; //Should be also bottom and right

    //Lies within range 0, TERRAIN_FOG_OF_WAR_MIN..TERRAIN_FOG_OF_WAR_MAX.
    FogOfWar:array[1..8]of byte;

    //Whenever there's a unit on that tile mark the tile as occupied
    //Counter for number of units on tile
    IsUnit:byte;

  end;

  constructor Create;
  destructor Destroy; override;
  procedure MakeNewMap(Width,Height:integer);
  function OpenMapFromFile(filename:string):boolean;

  procedure SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
  procedure RemMarkup(Loc:TKMPoint); 
  procedure RemFieldSpecial(Loc:TKMPoint);
  procedure SetField(Loc:TKMPoint; aOwner:TPlayerID; aFieldType:TFieldType);
  procedure IncFieldState(Loc:TKMPoint);

  procedure SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; fdt:TFieldType);
  procedure SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
  function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; PlayerRevealID:TPlayerID=play_none):boolean;
  function CanRemovePlan(Loc:TKMPoint; PlayerID:TPlayerID):boolean;
  function CanRemoveHouse(Loc:TKMPoint; PlayerID:TPlayerID):boolean;
  function CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup; PlayerRevealID:TPlayerID=play_none):boolean;
  procedure AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);

  function FindField(aPosition:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean):TKMPoint;
  function FindTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindStone(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindCoal(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindOre(aPosition:TKMPoint; aRadius:integer; Rt:TResourceType):TKMPoint;
  function FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;

  procedure AddTree(Loc:TKMPoint; ID:integer);
  procedure ChopTree(Loc:TKMPoint);
  procedure FallTree(Loc:TKMPoint);
  procedure InitGrowth(Loc:TKMPoint);
  procedure CutCorn(Loc:TKMPoint);
  procedure CutGrapes(Loc:TKMPoint);
  procedure SetResourceDeposit(Loc:TKMPoint; rt:TResourceType);
  procedure DecStoneDeposit(Loc:TKMPoint);
  procedure DecCoalDeposit(Loc:TKMPoint);
  procedure DecOreDeposit(Loc:TKMPoint; rt:TResourceType);

  procedure RecalculatePassability(Loc:TKMPoint);
  function CheckPassability(Loc:TKMPoint; aPass:TPassability):boolean;

  function GetOutOfTheWay(Loc:TKMPoint; aPass:TPassability):TKMPoint;
  procedure MakeRoute(LocA, LocB:TKMPoint; aPass:TPassability; out NodeCount:word; out Nodes:array of TKMPoint);
  procedure UnitAdd(LocTo:TKMPoint);
  procedure UnitRem(LocFrom:TKMPoint);
  procedure UnitWalk(LocFrom,LocTo:TKMPoint);

  function TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
  function SetTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;
  function VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
  function TileIsWater(Loc:TKMPoint):boolean;
  function TileIsSand(Loc:TKMPoint):boolean;
  function TileIsStone(Loc:TKMPoint):byte;
  function TileIsSoil(Loc:TKMPoint):boolean;
  function TileIsWalkable(Loc:TKMPoint):boolean;
  function TileIsRoadable(Loc:TKMPoint):boolean;
  function TileIsExTree(Loc:TKMPoint):boolean;
  procedure RevealCircle(Pos:TKMPoint; Radius,Amount:word; PlayerID:TPlayerID);
  procedure RevealWholeMap(PlayerID:TPlayerID);
  function CheckRevelation(X,Y:word; PlayerID:TPlayerID):byte;
  procedure UpdateBorders(Loc:TKMPoint);
  procedure FlattenTerrain(Loc:TKMPoint);
  procedure RebuildLighting(LowX,HighX,LowY,HighY:integer);
  procedure RebuildPassability(LowX,HighX,LowY,HighY:integer);
  function ConvertCursorToMapCoord(inX,inY:single):single;
  function InterpolateLandHeight(inX,inY:single):single;

  procedure RefreshMinimapData();

  procedure UpdateState;
  procedure UpdateCursor(aCursor:cmCursorMode; Loc:TKMPoint);
  procedure Paint;
end;

var
  fTerrain: TTerrain;

implementation

uses KM_Unit1, KM_Viewport, KM_Render, KM_Users, KM_Houses, KM_LoadSFX;

constructor TTerrain.Create;
begin
  //Don't know what to put here yet
  FallingTrees := TKMPointList.Create;
end;

destructor TTerrain.Destroy;
begin
  FreeAndNil(FallingTrees);
  inherited;
end;

//Reset whole map with default values
procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k,h:integer;
begin
  MapX:=min(Width,MaxMapSize);
  MapY:=min(Height,MaxMapSize);

  for i:=1 to MapY do for k:=1 to MapX do with Land[i,k] do begin
    Terrain:=0;
    Height:=random(7);    //variation in height
    Rotation:=random(4);  //Make it random
    Obj:=255;             //none
    if Random(16)=0 then Obj:=ChopableTrees[Random(13)+1,2];
    FieldSpecial:=fs_None;
    Markup:=mu_None;
    Passability:=[]; //Gets recalculated later
    TileOwner:=play_none;
    FieldType:=fdt_None;
    FieldAge:=0;
    TreeAge:=0;
    FallingTreeAnimStep:=-1;
    BorderX:=bt_None;
    BorderY:=bt_None;
    for h:=1 to 8 do FogOfWar[h]:=0;
    IsUnit:=0;
  end;

  //i:=SizeOf(Land[1,1]); //Shall align it manually later on for better performance and less size

  RebuildLighting(1,MapX,1,MapY);
  RebuildPassability(1,MapX,1,MapY);
end;


function TTerrain.OpenMapFromFile(filename:string):boolean;
var
  i,k:integer;
  c:array[1..23]of byte;
  f:file;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
  fLog.AppendLog('Loading map file');
  assignfile(f,filename); reset(f,1);
  blockread(f,k,4);
  blockread(f,i,4);
  Assert((k<=MaxMapSize)and(i<=MaxMapSize),'TTerrain.OpenMapFromFile - Can''t open the map cos it''s too big.');
  MapX:=k;
  MapY:=i;
  MakeNewMap(MapX,MapY); //Reset whole map to default
  for i:=1 to MapY do for k:=1 to MapX do
    begin
      blockread(f,c,23);
      Land[i,k].Terrain:=c[1];
      Land[i,k].Height:=c[3];
      Land[i,k].Rotation:=c[4];
      Land[i,k].Obj:=c[6];
      //Everything else is default
      //Land[i,k].Passability:=[];
      //Land[i,k].TileOwner:=play_none; //no roads
    end;
closefile(f);
RebuildLighting(1,MapX,1,MapY);
RebuildPassability(1,MapX,1,MapY);
fLog.AppendLog('Map file loaded');
Result:=true;
end;


{Check if requested tile (X,Y) is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,MapX-1-Inset) and InRange(Y,1+Inset,MapY-1-Inset);
end;


{Check if requested tile is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.SetTileInMapCoords(X,Y:integer; Inset:byte=0):TKMPoint;
begin
  Result.X := EnsureRange(X,1+Inset,MapX-1-Inset);
  Result.Y := EnsureRange(Y,1+Inset,MapY-1-Inset);
end;


{Check if requested vertice is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TTerrain.VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,MapX-Inset) and InRange(Y,1+Inset,MapY-Inset);
end;


//@Lewin: Feel free to tweak these flags if you think they are wrong, I could have been mistaken with Soil
//@Krom:  I've tweaked them all, and I think they are correct. There could still be mistakes, but we can fix them
//        when we find them. To be deleted...
{Check if requested tile is water suitable for fish and/or sail. No waterfalls}
function TTerrain.TileIsWater(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  Result := Land[Loc.Y,Loc.X].Terrain in [192,193,194,196, 200, 208..211, 235,236, 240,244];
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
  Result := Land[Loc.Y,Loc.X].Terrain in [0..6, 8..11,13,14, 16..22, 25..31, 32..39, 44..47, 49,52,55, 56..63,
                                          64..71, 72..79, 80..87, 88..95, 96..103, 104,106..109,111, 112,113,116,117, 123..125,
                                          138..139, 152..155, 166,167, 168..175, 180..183, 188..191,
                                          197, 203..205,207, 212..215, 220..223, 242,243,247];
end;

{Check if requested tile is generally suitable for road building}
function TTerrain.TileIsRoadable(Loc:TKMPoint):boolean;
begin
  //Should be Tileset property, especially if we allow different tilesets
  //Do not include 1/2 and 1/4 walkable as roadable
  Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..31, 32..39, 45..47, 49, 52, 55, 56..63,
                                          64..71, 72..80, 80..87, 88..95, 96..103, 104,108,111, 112,113,
                                          152..155,180..183,188..191,
                                          203..205, 212,213,215, 220, 247];
end;


{Check if the tile has stomp of chopped tree}
function TTerrain.TileIsExTree(Loc:TKMPoint):boolean;
var i:integer;
begin
  Result:=false;
  if Land[Loc.Y,Loc.X].Obj<>255 then
    for i:=1 to length(ChopableTrees) do
      Result:=Result or (Land[Loc.Y,Loc.X].Obj=ChopableTrees[i,6]);
end;

//Also need to make such table for objects with 2 options
// - CanBuildOnTop(means everything allowed), CanWalkOnTop(can only walk and build roads on top), CanNothing


{Reveal circle on map}
{Amount controls how "strong" terrain is revealed, almost instantly or slowly frame-by-frame in multiple calls}
procedure TTerrain.RevealCircle(Pos:TKMPoint; Radius,Amount:word; PlayerID:TPlayerID);
var i,k:integer;
begin
  if not InRange(byte(PlayerID),1,8) then exit;
  for i:=Pos.Y-Radius to Pos.Y+Radius do for k:=Pos.X-Radius to Pos.X+Radius do
  if (VerticeInMapCoords(k,i,1))and(GetLength(Pos,KMPoint(k,i))<=Radius) then
    Land[i,k].FogOfWar[byte(PlayerID)] := min(Land[i,k].FogOfWar[byte(PlayerID)] + Amount,TERRAIN_FOG_OF_WAR_MAX);
end;


{Reveal whole map to max value}
procedure TTerrain.RevealWholeMap(PlayerID:TPlayerID);
var i,k:integer;
begin
  if not InRange(byte(PlayerID),1,8) then exit;
  for i:=1 to MapY do for k:=1 to MapX do
    Land[i,k].FogOfWar[byte(PlayerID)] := TERRAIN_FOG_OF_WAR_MAX;
end;


{Check if requested vertice is revealed for given player}
{Return value revelation is x100 in percent}
function TTerrain.CheckRevelation(X,Y:word; PlayerID:TPlayerID):byte;
begin
  //I like how "alive" fog looks with some tweaks
  //pulsating around units and slowly thickening when they leave :)
  if Land[Y,X].FogOfWar[byte(PlayerID)] >= TERRAIN_FOG_OF_WAR_ACT then
    Result:=255
  else
    Result:=EnsureRange(round(Land[Y,X].FogOfWar[byte(PlayerID)] / TERRAIN_FOG_OF_WAR_ACT * 255),0,255);
end;


{Place markup on tile, any new markup replaces old one, thats okay}
procedure TTerrain.SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
begin
  Land[Loc.Y,Loc.X].Markup:=aMarkup;
  RecalculatePassability(Loc);
end;


{Remove markup from tile}
procedure TTerrain.RemMarkup(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].Markup:=mu_None;
  RecalculatePassability(Loc);
end;


{Remove FieldSpecial from tile}
procedure TTerrain.RemFieldSpecial(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldSpecial:=fs_None;
  RecalculatePassability(Loc);
end;


{Set field on tile - road/field/wine}
procedure TTerrain.SetField(Loc:TKMPoint; aOwner:TPlayerID; aFieldType:TFieldType);
begin
  Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  Land[Loc.Y,Loc.X].Markup:=mu_None;
  Land[Loc.Y,Loc.X].FieldType:=aFieldType;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  Land[Loc.Y,Loc.X].FieldSpecial:=fs_None;

  UpdateBorders(Loc);
  if aFieldType=fdt_Field then begin
    Land[Loc.Y,Loc.X].Terrain:=62;
    Land[Loc.Y,Loc.X].Rotation:=0;
  end else
  if aFieldType=fdt_Wine  then begin
    Land[Loc.Y,Loc.X].Terrain:=55;
    Land[Loc.Y,Loc.X].Rotation:=0;
  end;
  RecalculatePassability(Loc);
end;


procedure TTerrain.IncFieldState(Loc:TKMPoint);
begin
  case Land[Loc.Y,Loc.X].FieldSpecial of
    fs_Dig3: Land[Loc.Y,Loc.X].FieldSpecial:=fs_Dig4;
    fs_Dig2: Land[Loc.Y,Loc.X].FieldSpecial:=fs_Dig3;
    fs_Dig1: Land[Loc.Y,Loc.X].FieldSpecial:=fs_Dig2;
    else     Land[Loc.Y,Loc.X].FieldSpecial:=fs_Dig1;
  end;
end;


{ Should find closest field around}
{aAgeFull is used for fdt_Field mostly. Incase Farmer is looking for empty or full field of corn}
function TTerrain.FindField(aPosition:TKMPoint; aRadius:integer; aFieldType:TFieldType; aAgeFull:boolean):TKMPoint;
var i,k:integer; List:TKMPointList;
begin
  List:=TKMPointList.Create;
  for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
    for k:=aPosition.X-aRadius to aPosition.X+aRadius do
      if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
        if Land[i,k].FieldType=aFieldType then
          if ((aAgeFull)and(Land[i,k].FieldAge=65535))or((not aAgeFull)and(Land[i,k].FieldAge=0)) then
            List.AddEntry(KMPoint(k,i));

  Result:=List.GetRandom;
  List.Free;
end;


{Find closest chopable Tree around}
function TTerrain.FindTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer; List:TKMPointList;
begin
  List:=TKMPointList.Create;
  for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
    for k:=aPosition.X-aRadius to aPosition.X+aRadius do
      if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
        if MapElem[Land[i,k].Obj+1].Properties[mep_CuttableTree]=1 then
          List.AddEntry(KMPoint(k,i));
  Result:=List.GetRandom;
  List.Free;
end;


{Find closest harvestable deposit of Stone}
{Return walkable tile below Stone deposit}
function TTerrain.FindStone(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer; List:TKMPointList;
begin
  List:=TKMPointList.Create;
//  Result:=KMPoint(0,0);
  for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
    for k:=aPosition.X-aRadius to aPosition.X+aRadius do
      if (TileInMapCoords(k,i,1))and(TileInMapCoords(k,i+1,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
        if (TileIsStone(KMPoint(k,i))>0)and(TileIsWalkable(KMPoint(k,i+1))) then
          List.AddEntry(KMPoint(k,i+1));
//            Result:=KMPoint(k,i+1);

  Result:=List.GetRandom;
  List.Free;
end;


function TTerrain.FindCoal(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k,RadX,RadY:integer; L:array[1..4]of TKMPointList;
begin
  for i:=1 to 4 do L[i]:=TKMPointList.Create; //4 densities

  RadX:=aRadius+3; //Should add some gradient to it later on
  RadY:=aRadius+2; //Should add some gradient to it later on
  for i:=aPosition.Y-RadY to aPosition.Y+RadY do
    for k:=aPosition.X-RadX to aPosition.X+RadX do
      if TileInMapCoords(k,i) then
        case Land[i,k].Terrain of
        152: if abs(i-aPosition.Y)<=(RadY-3) then if abs(k-aPosition.X)<=(RadX-3) then L[1].AddEntry(KMPoint(k,i));
        153: if abs(i-aPosition.Y)<=(RadY-2) then if abs(k-aPosition.X)<=(RadX-2) then L[2].AddEntry(KMPoint(k,i));
        154: if abs(i-aPosition.Y)<=(RadY-1) then if abs(k-aPosition.X)<=(RadX-1) then L[3].AddEntry(KMPoint(k,i));
        155: L[4].AddEntry(KMPoint(k,i));
        end;

  if L[4].Count<>0 then Result:=L[4].GetRandom else
  if L[3].Count<>0 then Result:=L[3].GetRandom else
  if L[2].Count<>0 then Result:=L[2].GetRandom else
  if L[1].Count<>0 then Result:=L[1].GetRandom else
  Result:=KMPoint(0,0);
  
  for i:=1 to 4 do L[i].Free;
end;


function TTerrain.FindOre(aPosition:TKMPoint; aRadius:integer; Rt:TResourceType):TKMPoint; //Gold or Iron
var i,k:integer; L:array[1..4]of TKMPointList;
begin
  Assert(Rt in [rt_IronOre,rt_GoldOre],'Wrong resource');
  for i:=1 to 4 do L[i]:=TKMPointList.Create; //4 densities

  //aRadius:=aRadius+2; //Should add some gradient to it later on or not?
  //Ore radius is not circular, hence -2 on top and -1 on bottom
  for i:=aPosition.Y-aRadius-2 to aPosition.Y+aRadius-1 do
    for k:=aPosition.X-aRadius to aPosition.X+aRadius do
      if TileInMapCoords(k,i) then begin
        if Rt=rt_GoldOre then
        case Land[i,k].Terrain of
        144: L[1].AddEntry(KMPoint(k,i));
        145: L[2].AddEntry(KMPoint(k,i));
        146: L[3].AddEntry(KMPoint(k,i));
        147: L[4].AddEntry(KMPoint(k,i));
        end;
        if Rt=rt_IronOre then
        case Land[i,k].Terrain of
        148: L[1].AddEntry(KMPoint(k,i));
        149: L[2].AddEntry(KMPoint(k,i));
        150: L[3].AddEntry(KMPoint(k,i));
        151: L[4].AddEntry(KMPoint(k,i));
        end;
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
function TTerrain.FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer; List1,List2:TKMPointList;
begin
List1:=TKMPointList.Create;
List2:=TKMPointList.Create;
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      if CanPlantTrees in Land[i,k].Passability then begin

        if TileIsExTree(KMPoint(k,i)) then
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


procedure TTerrain.AddTree(Loc:TKMPoint; ID:integer);
begin
  Land[Loc.Y,Loc.X].Obj:=ID;
  Land[Loc.Y,Loc.X].TreeAge:=1;
  RecalculatePassability(Loc);
end;

{Remove the tree and place a falling tree instead}
procedure TTerrain.FallTree(Loc:TKMPoint);
var h:integer;
begin
  for h:=1 to length(ChopableTrees) do
    if ChopableTrees[h,4]=Land[Loc.Y,Loc.X].Obj then
      Land[Loc.Y,Loc.X].Obj:=ChopableTrees[h,5];
  Land[Loc.Y,Loc.X].TreeAge:=0;
  Land[Loc.Y,Loc.X].FallingTreeAnimStep:=0;
  fSoundLib.Play(sfx_TreeDown,Loc,true);
  FallingTrees.AddEntry(Loc);
  RecalculatePassability(Loc);
end;

{Remove the tree and place stump instead}
procedure TTerrain.ChopTree(Loc:TKMPoint);
var h:integer;
begin
  for h:=1 to length(ChopableTrees) do
    if ChopableTrees[h,5]=Land[Loc.Y,Loc.X].Obj then
      Land[Loc.Y,Loc.X].Obj:=ChopableTrees[h,6];
  Land[Loc.Y,Loc.X].TreeAge:=0;
  Land[Loc.Y,Loc.X].FallingTreeAnimStep:=-1;
  FallingTrees.RemoveEntry(Loc);
  RecalculatePassability(Loc);
end;


procedure TTerrain.InitGrowth(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=1;
  RecalculatePassability(Loc);
end;


procedure TTerrain.CutCorn(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=0;
  Land[Loc.Y,Loc.X].Terrain:=63;
  Land[Loc.Y,Loc.X].FieldSpecial:=fs_None;
end;


procedure TTerrain.CutGrapes(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=1;
  Land[Loc.Y,Loc.X].FieldSpecial:=fs_Wine1;
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
    else Assert(false,'Wrong resource');
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
    if TileIsStone(KMPoint(X,Y))=0 then begin
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

  function DecHeight(CurHeight,BaseHeight:byte; ReductionFactor:single):byte;
  var MyMin:byte;
  begin
    if CurHeight < BaseHeight then
      MyMin := 100 //This tile is low, so we should allow it to be raised
    else
      MyMin := CurHeight; //This tile is higher than the base, so don't allow it to be raised
    Result := min(round(max(0,CurHeight-BaseHeight) * ReductionFactor)+BaseHeight,MyMin);
    Result := EnsureRange(Result + RandomS(2),0,100);
  end;

  procedure ReduceHeights(LowX,HighX,LowY,HighY:word);
  var ix, iy: word;
  const StoneHeightReduction = 0.95; //The amount that surounding tiles height will be reduced by for the surounding tiles
        GrassHeightReduction = 0.8; //Grass is reduced more than stone to ensure that no peaks in the grass occur
  begin
    for iy:=HighY downto LowY do
      for ix:=LowX to HighX do
      begin
        if TileIsStone(KMPoint(ix,iy))>0 then
          Land[iy,ix].Height:=DecHeight(Land[iy,ix].Height,Land[iy+1,ix].Height,StoneHeightReduction)
        else Land[iy,ix].Height:=DecHeight(Land[iy,ix].Height,Land[iy+1,ix].Height,GrassHeightReduction);   
        RecalculatePassability(KMPoint(ix,iy));
      end;
    RebuildLighting(LowX,HighX,LowY,HighY); //Update the lighting
  end;
  const HeightReduction = 0.75; //The amount that height will be reduced by for the decreased tile
begin
  case Land[Loc.Y,Loc.X].Terrain of
    132,137: Land[Loc.Y,Loc.X].Terrain:=131+Random(2)*5;
    131,136: Land[Loc.Y,Loc.X].Terrain:=130+Random(2)*5;
    130,135: Land[Loc.Y,Loc.X].Terrain:=129+Random(2)*5;
    129,134: Land[Loc.Y,Loc.X].Terrain:=128+Random(2)*5;
    128,133: Land[Loc.Y,Loc.X].Terrain:=0;
    else exit; //Assert(false,'Unable to DecStoneReserve at '+TypeToString(Loc)+' for it isn''t there');
  end;
  Land[Loc.Y,Loc.X].Rotation:=Random(4);
  Land[Loc.Y,Loc.X].Height:=DecHeight(Land[Loc.Y,Loc.X].Height,Land[Loc.Y+1,Loc.X].Height,HeightReduction); //Reduce height
  RecalculatePassability(Loc);
  UpdateTransition(Loc.X,Loc.Y);
  UpdateTransition(Loc.X,Loc.Y-1);
  UpdateTransition(Loc.X+1,Loc.Y);
  UpdateTransition(Loc.X,Loc.Y+1);
  UpdateTransition(Loc.X-1,Loc.Y);
  ReduceHeights(Loc.X-1,Loc.X+1,Loc.Y-1,Loc.Y+1); //Required for height reduction
end;


{Extract one unit of coal}
procedure TTerrain.DecCoalDeposit(Loc:TKMPoint);
begin
  case Land[Loc.Y,Loc.X].Terrain of
  152: Land[Loc.Y,Loc.X].Terrain:=36;
  153: Land[Loc.Y,Loc.X].Terrain:=152;
  154: Land[Loc.Y,Loc.X].Terrain:=153;
  155: Land[Loc.Y,Loc.X].Terrain:=154;
  //This check is removed incase worker builds wine field ontop of coal tile
  //else Assert(false,'Can''t DecCoalReserve');
  end;
  RecalculatePassability(Loc);
end;


{Extract one unit of ore}
procedure TTerrain.DecOreDeposit(Loc:TKMPoint; rt:TResourceType);
begin
  Assert(Rt in [rt_IronOre,rt_GoldOre],'Wrong resource');
  case Land[Loc.Y,Loc.X].Terrain of
  144: Land[Loc.Y,Loc.X].Terrain:=157; //Gold
  145: Land[Loc.Y,Loc.X].Terrain:=144;
  146: Land[Loc.Y,Loc.X].Terrain:=145;
  147: Land[Loc.Y,Loc.X].Terrain:=146;
  148: Land[Loc.Y,Loc.X].Terrain:=160; //Iron
  149: Land[Loc.Y,Loc.X].Terrain:=148;
  150: Land[Loc.Y,Loc.X].Terrain:=149;
  151: Land[Loc.Y,Loc.X].Terrain:=150;
  else Assert(false,'Can''t DecOreReserve');
  end;
  RecalculatePassability(Loc);
end;


procedure TTerrain.RecalculatePassability(Loc:TKMPoint);
var i,k:integer;
  HousesNearBy:boolean;
  procedure AddPassability(Loc:TKMPoint; aPass:TPassabilitySet);
  begin Land[Loc.Y,Loc.X].Passability:=Land[Loc.Y,Loc.X].Passability + aPass; end;
begin
  if not TileInMapCoords(loc.X,loc.y) then
    Assert(false, 'Fail: '+TypeToString(loc));
  Land[Loc.Y,Loc.X].Passability:=[];

  //First of all exclude all tiles outside of actual map and all houses
  if (TileInMapCoords(Loc.X,Loc.Y))and(Land[Loc.Y,Loc.X].FieldType<>fdt_House) then begin

       AddPassability(Loc, [canAll]);

     if (TileIsWalkable(Loc))and
        (MapElem[Land[Loc.Y,Loc.X].Obj+1].Properties[mep_AllBlocked] = 0)and
        (not (Land[Loc.Y,Loc.X].FieldType in [fdt_HouseWIP,fdt_HouseRoad]))then
       AddPassability(Loc, [canWalk]);

     if (true)and
        ((Land[Loc.Y,Loc.X].FieldType in [fdt_Road]))then
       AddPassability(Loc, [canWalkRoad]);

     //Check for houses around this tile
     HousesNearBy := false;
     for i:=-1 to 1 do
       for k:=-1 to 1 do
         if TileInMapCoords(Loc.X+k,Loc.Y+i) then
           if (Land[Loc.Y+i,Loc.X+k].FieldType in [fdt_HousePlan,fdt_HouseWIP,fdt_House,fdt_HouseRoad]) then
             HousesNearBy := true;

     if (TileIsRoadable(Loc))and
        ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved = 1))and
        //Only certain objects are excluded
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (TileInMapCoords(Loc.X,Loc.Y,1))and
        //No houses nearby
        (not HousesNearBy)and
        (Land[Loc.Y,Loc.X].FieldType in [fdt_None,fdt_Road])then
        AddPassability(Loc, [canBuild]);

     if (Land[Loc.Y,Loc.X].Terrain in [109,166..170])and
        (Land[Loc.Y,Loc.X].Rotation = 0)and     
        ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved = 1))and
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (TileInMapCoords(Loc.X,Loc.Y,1))and
        //No houses nearby
        (not HousesNearBy)and
        (not (Land[Loc.Y,Loc.X].FieldType in [fdt_HousePlan,fdt_HouseWIP,fdt_House,fdt_HouseRoad]))then
       AddPassability(Loc, [canBuildIron]);

     if (Land[Loc.Y,Loc.X].Terrain in [171..175])and
        (Land[Loc.Y,Loc.X].Rotation = 0)and
        ((Land[Loc.Y,Loc.X].Obj=255) or (MapElem[Land[Loc.Y,Loc.X].Obj+1].CanBeRemoved = 1))and
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (TileInMapCoords(Loc.X,Loc.Y,1))and
        //No houses nearby
        (not HousesNearBy)and
        (not (Land[Loc.Y,Loc.X].FieldType in [fdt_HousePlan,fdt_HouseWIP,fdt_House,fdt_HouseRoad]))then
       AddPassability(Loc, [canBuildGold]);

     if (TileIsRoadable(Loc))and
        (MapElem[Land[Loc.Y,Loc.X].Obj+1].Properties[mep_AllBlocked] = 0)and
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (Land[Loc.Y,Loc.X].FieldType in [fdt_None,fdt_Field,fdt_Wine])then
       AddPassability(Loc, [canMakeRoads]);

     if (TileIsSoil(Loc))and
        (MapElem[Land[Loc.Y,Loc.X].Obj+1].Properties[mep_AllBlocked] = 0)and
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (Land[Loc.Y,Loc.X].FieldType in [fdt_None])then
       AddPassability(Loc, [canMakeFields]);

     if (TileIsSoil(Loc))and
        (Land[Loc.Y,Loc.X].Obj=255)or(TileIsExTree(Loc))and //No object or ExTree
        (TileInMapCoords(Loc.X,Loc.Y,1))and
        (Land[Loc.Y,Loc.X].Markup=mu_None)and
        (Land[Loc.Y,Loc.X].FieldType in [fdt_None,fdt_RoadWIP,fdt_FieldWIP,fdt_WineWIP])then
       AddPassability(Loc, [canPlantTrees]);

     if TileIsWater(Loc) then
       AddPassability(Loc, [canFish]);

     if TileIsSand(Loc) then
       AddPassability(Loc, [canCrab]);

  end else
    Land[Loc.Y,Loc.X].Passability:=[]; //Allow nothing
end;


function TTerrain.CheckPassability(Loc:TKMPoint; aPass:TPassability):boolean;
begin
  Result := aPass in Land[Loc.Y,Loc.X].Passability;
end;


{Return random tile surrounding given one with aPass property}
function TTerrain.GetOutOfTheWay(Loc:TKMPoint; aPass:TPassability):TKMPoint;
var i,k:integer; L:TKMPointList;
begin
  L:=TKMPointList.Create;
  for i:=-1 to 1 do for k:=-1 to 1 do
    if TileInMapCoords(Loc.X+k,Loc.Y+i) then
      if(i<>0)and(k<>0) then
        if aPass in Land[Loc.Y+i,Loc.X+k].Passability then
          L.AddEntry(KMPoint(Loc.X+k,Loc.Y+i));
  Result:=L.GetRandom;
end;


{Find a route from A to B which meets aPass Passability}
{Results should be written as NodeCount of waypoint nodes to Nodes}
{Simplification1 - ajoin nodes that don't require direction change}
procedure TTerrain.MakeRoute(LocA, LocB:TKMPoint; aPass:TPassability; out NodeCount:word; out Nodes:array of TKMPoint);
const c_closed=65535;
var
  i,k,y,x:integer;
  NewCost:integer;
  MinCost:record
    Cost:integer;
    ID:word;
    Pos:TKMPoint;
  end;
  ORef:array[1..MaxMapSize,1..MaxMapSize] of word; //Ref to OpenList
  OCount:word;
  OList:array[1..1024]of record //List of checked cells
    Pos:TKMPoint;
    CostTo:word;
    Estim:word;
    Parent:word;//Ref to parent
  end;
begin

  //Don't try to make a route if it's obviously impossible
  if (KMSamePoint(LocA,LocB))or(not (aPass in Land[LocB.Y,LocB.X].Passability)) then begin
    NodeCount:=0;
    Nodes[0]:=LocA;
    exit;
  end;

  OCount:=0;
  FillChar(ORef,SizeOf(ORef),#0);
  FillChar(OList,SizeOf(OList),#0);

  //Init start point
  inc(OCount);
  ORef[LocA.Y,LocA.X]:=OCount;
  OList[OCount].Pos:=LocA;
  OList[OCount].CostTo:=0; //
  OList[OCount].Estim:=(abs(LocB.X-LocA.X) + abs(LocB.Y-LocA.Y))*10;
  OList[OCount].Parent:=0;

  k:=0;
  repeat
  inc(k);

    //Find cell with least (Estim+CostTo)
    MinCost.Cost:=65535;
    for i:=1 to OCount do
    if OList[i].Estim<>c_closed then
    if (OList[i].Estim+OList[i].CostTo) < MinCost.Cost then begin
      MinCost.Cost:=OList[i].Estim+OList[i].CostTo;
      MinCost.ID:=i;
      MinCost.Pos:=OList[i].Pos;
    end;

    //Keep looking if we haven't reached destination
    if not KMSamePoint(MinCost.Pos,LocB) then begin

      OList[MinCost.ID].Estim:=c_closed;

      //Check all surrounds and issue costs to them
      for y:=MinCost.Pos.Y-1 to MinCost.Pos.Y+1 do for x:=MinCost.Pos.X-1 to MinCost.Pos.X+1 do
      if TileInMapCoords(x,y) then //Ignore those outside of MapCoords
        if ORef[y,x]=0 then begin //Cell is new
          if aPass in Land[y,x].Passability then begin //If cell meets Passability then estimate it
            inc(OCount);
            ORef[y,x]:=OCount;
            OList[OCount].Pos:=KMPoint(x,y);
            OList[OCount].Parent:=ORef[MinCost.Pos.Y,MinCost.Pos.X];
            OList[OCount].CostTo:=OList[OList[OCount].Parent].CostTo+round(GetLength(y-MinCost.Pos.Y,x-MinCost.Pos.X)*10); //
            OList[OCount].Estim:=(abs(x-LocB.X) + abs(y-LocB.Y))*10;
          end else begin //If cell doen't meets Passability then mark it as Closed
            inc(OCount);
            OList[OCount].Estim:=c_closed;
            OList[OCount].Pos:=KMPoint(x,y);
          end;
        end else begin
          //If route through new cell is shorter than ORef[y,x] then
          if OList[ORef[y,x]].Estim<>c_closed then begin
            NewCost:=round(GetLength(y-MinCost.Pos.Y,x-MinCost.Pos.X)*10);
            if OList[MinCost.ID].CostTo + NewCost < OList[ORef[y,x]].CostTo then begin
              OList[ORef[y,x]].Parent:=ORef[MinCost.Pos.Y,MinCost.Pos.X];
              OList[ORef[y,x]].CostTo:=OList[MinCost.ID].CostTo + NewCost;
              //OList[ORef[y,x]].Estim:=(abs(x-LocB.X) + abs(y-LocB.Y))*10;
            end;
          end;
        end;
    end;
  until((k=500)or(OCount+8>=length(OList))or(KMSamePoint(MinCost.Pos,LocB)));

  if (not KMSamePoint(MinCost.Pos,LocB))or(k=500) then begin
    NodeCount:=0; //Something went wrong
    Nodes[0]:=LocA;
    exit;
  end;

  //Calculate NodeCount
  k:=MinCost.ID; NodeCount:=0;
  repeat
    inc(NodeCount);
    k:=OList[k].Parent;
  until(k=0);

  if NodeCount > length(Nodes) then begin
    NodeCount:=0; //Something went wrong
    Nodes[0]:=LocA;
    exit;
  end;

  k:=MinCost.ID;
  for i:=1 to NodeCount do begin
    Nodes[NodeCount-i]:=OList[k].Pos; //invert, since path is assembled LocB>LocA in fact
    k:=OList[k].Parent;
  end;
    //Nodes[0]:=LocA;

  //Should ajoin straight pieces to reduce mem usage
  //Important rule:
  // - First node is LocA,
  // - next to last node is neighbour of LocB (important for delivery to workers),
  // - last node is LocB

  {if NodeCount>3 then begin
  k:=2;
  for i:=3 to NodeCount-1 do begin //simplify within LocA+1 .. LocB-2 range
  // i/k are always -1 since array is [0..Count-1] range
    if (sign(Nodes[k-1].X-Nodes[k-2].X) = sign(Nodes[i-1].X-Nodes[i-2].X))and //Direction matches
       (sign(Nodes[k-1].Y-Nodes[k-2].Y) = sign(Nodes[i-1].Y-Nodes[i-2].Y)) then begin
      Nodes[k-1]:=Nodes[i-1];
    end else begin
      inc(k);
      Nodes[k-1]:=Nodes[i-1];
    end;
  end;
  inc(k);
  Nodes[k-1]:=Nodes[NodeCount-1];
  NodeCount:=k;
  end;}

  //for i:=1 to NodeCount do
  //  Assert(TileInMapCoords(Nodes[i-1].X,Nodes[i-1].Y));
end;


{Mark previous tile as empty and next one as occupied}
procedure TTerrain.UnitAdd(LocTo:TKMPoint);
begin
//  if not TileInMapCoords(LocTo.Y,LocTo.X) then
//    break;
//  inc(Land[LocTo.Y,LocTo.X].IsUnit);
end;

{Mark previous tile as empty and next one as occupied}
procedure TTerrain.UnitRem(LocFrom:TKMPoint);
begin
//  dec(Land[LocFrom.Y,LocFrom.X].IsUnit);
end;

{Mark previous tile as empty and next one as occupied}
procedure TTerrain.UnitWalk(LocFrom,LocTo:TKMPoint);
begin
//  dec(Land[LocFrom.Y,LocFrom.X].IsUnit);
//  inc(Land[LocTo.Y,LocTo.X].IsUnit);
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
  Land[Loc.Y,Loc.X].Height:=mix(Land[Loc.Y,Loc.X].Height,TempH,0.5);
  Land[Loc.Y+1,Loc.X].Height:=mix(Land[Loc.Y+1,Loc.X].Height,TempH,0.5);
  Land[Loc.Y,Loc.X+1].Height:=mix(Land[Loc.Y,Loc.X+1].Height,TempH,0.5);
  Land[Loc.Y+1,Loc.X+1].Height:=mix(Land[Loc.Y+1,Loc.X+1].Height,TempH,0.5);

  RebuildLighting(Loc.X-2,Loc.X+3,Loc.Y-2,Loc.Y+3);
  RecalculatePassability(Loc);
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
        Land[i,k].Light:=EnsureRange((Land[i,k].Height-(Land[y2,k].Height+Land[i,x0].Height)/2)/22,-1,1)*(1-Overlap); //  1.33*16 ~=22
    if (i=1)or(i=MapY)or(k=1)or(k=MapX) then //Map borders fade to black
      Land[i,k].Light:=-1+Overlap;
    end;
end;


{ Rebuilds passability for given bounds }
procedure TTerrain.RebuildPassability(LowX,HighX,LowY,HighY:integer);
var i,k:integer;
begin
  for i:=LowY to HighY do for k:=LowX to HighX do
    if TileInMapCoords(k,i) then
      RecalculatePassability(KMPoint(k,i));
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TTerrain.SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; fdt:TFieldType);
var i,k,x,y:word; L,H:TKMPoint;
begin
  for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
      x:=Loc.X+k-3; y:=Loc.Y+i-4;
      if TileInMapCoords(x,y) then begin

        if (fdt=fdt_House)and(HousePlanYX[byte(aHouseType),i,k]=2) then
          Land[y,x].FieldType:=fdt_HouseRoad
        else
          Land[y,x].FieldType:=fdt;

        UpdateBorders(KMPoint(x,y));
        RecalculatePassability(KMPoint(x,y));
      end;
    end;
  //Recalculate Passability for tiles around the house so that they can't be built on too
  L:=SetTileInMapCoords(Loc.X-3,Loc.Y-4);
  H:=SetTileInMapCoords(Loc.X+2,Loc.Y+1);
  RebuildPassability(L.X,L.Y,H.X,H.Y);
end;


{That is mainly used for minimap now}
procedure TTerrain.SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
var i,k:integer;
begin
  if aHouseType<>ht_None then //If this is a house make a change for whole place
    for i:=1 to 4 do for k:=1 to 4 do
      if TileInMapCoords(Loc.X+k-3,Loc.Y+i-4) then
        if HousePlanYX[byte(aHouseType),i,k]<>0 then
          Land[Loc.Y+i-4,Loc.X+k-3].TileOwner:=aOwner;

  if aHouseType=ht_None then
    Land[Loc.Y,Loc.X].TileOwner:=aOwner;
end;


{Check if house can be placed in that place}
function TTerrain.CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType; PlayerRevealID:TPlayerID=play_none):boolean;
var i,k,l,m:integer;
begin
Result:=true;
  for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
      Result := Result AND TileInMapCoords(Loc.X+k-3,Loc.Y+i-4,1); //Inset one tile from map edges
      //Now check that surrounding tiles don't have a house
      for l:=-1 to 1 do
        for m:=-1 to 1 do  
          if fTerrain.TileInMapCoords(Loc.X+k-3+l,Loc.Y+i-4+m) then
            Result := Result AND (not (Land[Loc.Y+i-4+m,Loc.X+k-3+l].FieldType in [fdt_HousePlan,fdt_HouseWIP,fdt_House,fdt_HouseRoad]));

      if aHouseType=ht_IronMine then
        Result := Result AND (CanBuildIron in Land[Loc.Y+i-4,Loc.X+k-3].Passability) else
      if aHouseType=ht_GoldMine then
        Result := Result AND (CanBuildGold in Land[Loc.Y+i-4,Loc.X+k-3].Passability) else
      if aHouseType=ht_Wall then
        Result := Result AND (CanWalk in Land[Loc.Y+i-4,Loc.X+k-3].Passability)
      else
        Result := Result AND (CanBuild in Land[Loc.Y+i-4,Loc.X+k-3].Passability);

      if PlayerRevealID <> play_none then
        Result := Result AND (CheckRevelation(Loc.X+k-3,Loc.Y+i-4,PlayerRevealID) > 0);
    end;
end;


function TTerrain.CanRemovePlan(Loc:TKMPoint; PlayerID:TPlayerID):boolean;
begin
   Result := fPlayers.Player[integer(PlayerID)].RemPlan(Loc,true);
end;


function TTerrain.CanRemoveHouse(Loc:TKMPoint; PlayerID:TPlayerID):boolean;
begin
   Result := fPlayers.Player[integer(PlayerID)].RemHouse(Loc,true,true);
end;


function TTerrain.CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup; PlayerRevealID:TPlayerID=play_none):boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y,1); //Do inset one tile from map edges
  case aMarkup of
  mu_RoadPlan: Result := Result AND (canMakeRoads in Land[Loc.Y,Loc.X].Passability);
  mu_FieldPlan: Result := Result AND (canMakeFields in Land[Loc.Y,Loc.X].Passability);
  mu_WinePlan: Result := Result AND (canMakeFields in Land[Loc.Y,Loc.X].Passability);
  else Result:=false;
  end;
  if PlayerRevealID <> play_none then
    Result := Result AND (CheckRevelation(Loc.X,Loc.Y,PlayerRevealID) > 0);
end;


procedure TTerrain.AddHouseRemainder(Loc:TKMPoint; aHouseType:THouseType; aBuildState:THouseBuildState);
var i,k:integer;
begin
  if aBuildState in [hbs_Wood, hbs_Stone, hbs_Done] then
  begin
    //For houses that are at least partually built (leaves rubble)
    for i:=2 to 4 do for k:=2 to 4 do
      if HousePlanYX[byte(aHouseType),i-1,k]<>0 then
      if HousePlanYX[byte(aHouseType),i,k-1]<>0 then
      if HousePlanYX[byte(aHouseType),i-1,k-1]<>0 then
      if HousePlanYX[byte(aHouseType),i,k]<>0 then
        Land[Loc.Y+i-4,Loc.X+k-3].Obj:=68+Random(6);
    for i:=1 to 4 do for k:=1 to 4 do
      if (HousePlanYX[byte(aHouseType),i,k]=1) or (HousePlanYX[byte(aHouseType),i,k]=2) then begin
        Land[Loc.Y+i-4,Loc.X+k-3].FieldSpecial:=fs_Dig3;
        Land[Loc.Y+i-4,Loc.X+k-3].FieldType:=fdt_None;
      end;
  end
  else begin
    //For glyphs
    for i:=1 to 4 do for k:=1 to 4 do
      if (HousePlanYX[byte(aHouseType),i,k]=1) or (HousePlanYX[byte(aHouseType),i,k]=2) then begin
        ///@Krom: Unfinished, due to issues with design of Terrain system. To be discussed... (somewhere else)
        Land[Loc.Y+i-4,Loc.X+k-3].FieldType:=fdt_None;
        Land[Loc.Y+i-4,Loc.X+k-3].BorderX:=bt_None;
        Land[Loc.Y+i-4,Loc.X+k-3].BorderY:=bt_None;
      end;
  end;
  RebuildPassability(Loc.X-3,Loc.X+2,Loc.Y-4,Loc.Y+1);
end;


{Check 4 surrounding tiles, and if they are different place a border}
procedure TTerrain.UpdateBorders(Loc:TKMPoint);
  function GetBorder(a,b:TFieldType):TBorderType;
  begin
    Result:=bt_None;
    if (a=fdt_Field)or(b=fdt_Field) then Result:=bt_Field;
    if (a=fdt_Wine)or(b=fdt_Wine) then Result:=bt_Wine;
    if (a=fdt_HousePlan)or(b=fdt_HousePlan) then Result:=bt_HousePlan;
    if (a=fdt_HouseWIP)or(b=fdt_HouseWIP) then Result:=bt_HouseBuilding;
    if a=b then Result:=bt_None;
  end;
begin

  if not TileInMapCoords(Loc.X,Loc.Y) then exit;

  if TileInMapCoords(Loc.X-1,Loc.Y) then
  Land[Loc.Y,Loc.X].BorderY:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y,Loc.X-1].FieldType);

  if TileInMapCoords(Loc.X,Loc.Y-1) then
  Land[Loc.Y,Loc.X].BorderX:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y-1,Loc.X].FieldType);

  if TileInMapCoords(Loc.X+1,Loc.Y) then
  Land[Loc.Y,Loc.X+1].BorderY:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y,Loc.X+1].FieldType);

  if TileInMapCoords(Loc.X,Loc.Y+1) then
  Land[Loc.Y+1,Loc.X].BorderX:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y+1,Loc.X].FieldType);

end;


{Cursor position should be converted to tile-coords respecting tile heights}
function TTerrain.ConvertCursorToMapCoord(inX,inY:single):single;
var ii:integer; Xc,Yc:integer; Tmp:integer; Ycoef:array[-2..4]of single;
begin
  Xc:=EnsureRange(round(inX+0.5),1,MapX-1); //Cell below cursor without height check
  Yc:=EnsureRange(round(inY+0.5),1,MapY-1);

  for ii:=-2 to 4 do
  begin//make an array of tile heights above and below cursor (-2..4)
    Tmp:=EnsureRange(Yc+ii,1,MapY);
    Ycoef[ii]:=(Yc-1)+ii-(fTerrain.Land[Tmp,Xc].Height*(1-frac(InX))
                         +fTerrain.Land[Tmp,Xc+1].Height*frac(InX))/CELL_HEIGHT_DIV;
  end;

  Result:=Yc; //Assign something incase following code returns nothing 

  for ii:=-2 to 3 do //check if cursor in a tile and adjust it there
    if (InY>=Ycoef[ii])and(InY<=Ycoef[ii+1]) then
      begin
        Result:=Yc+ii-(Ycoef[ii+1]-InY) / (Ycoef[ii+1]-Ycoef[ii]);
        break;
      end;
  //Assert(false,'TTerrain.ConvertCursorToMapCoord - couldn''t convert')
end;


{Return height within cell interpolating node heights}
function TTerrain.InterpolateLandHeight(inX,inY:single):single;
var Xc,Yc:integer; Tmp1,Tmp2:single;
begin
  Xc:=trunc(inX);
  Yc:=trunc(inY);
  Assert(VerticeInMapCoords(Xc,Yc),'InterpolateLandHeight accessed wrong '+inttostr(Xc)+':'+inttostr(Yc));
  Tmp1:=mix(fTerrain.Land[Yc  ,Xc+1].Height, fTerrain.Land[Yc  ,Xc].Height, frac(InX));
  Tmp2:=mix(fTerrain.Land[Yc+1,Xc+1].Height, fTerrain.Land[Yc+1,Xc].Height, frac(InX));
  Result:=mix(Tmp2, Tmp1, frac(InY));
end;


procedure TTerrain.RefreshMinimapData();
var i,k,ID:integer; Light:single; Loc:TKMPointList; FOW:byte;
begin
  for i:=1 to fTerrain.MapY do for k:=1 to fTerrain.MapX do begin
    //MM[i,k].A:=255;
    FOW:=fTerrain.CheckRevelation(k,i,MyPlayer.PlayerID);
    if fTerrain.Land[i,k].TileOwner=play_none then begin
      if FOW=0 then begin
        MM[i,k].R:=0;
        MM[i,k].G:=0;
        MM[i,k].B:=0;
      end else begin
        ID:=fTerrain.Land[i,k].Terrain+1;
        Light:=fTerrain.Land[i,k].Light/4-(1-FOW/255); //Originally it's -1..1 range
        //Will tweak it later..
        MM[i,k].R:=round(EnsureRange(TileMMColor[ID].R+Light,0,1)*255);
        MM[i,k].G:=round(EnsureRange(TileMMColor[ID].G+Light,0,1)*255);
        MM[i,k].B:=round(EnsureRange(TileMMColor[ID].B+Light,0,1)*255);
      end;
    end else begin
      MM[i,k].R:=TeamColors[byte(fTerrain.Land[i,k].TileOwner)] and $FF;
      MM[i,k].G:=TeamColors[byte(fTerrain.Land[i,k].TileOwner)] shr 8 and $FF;
      MM[i,k].B:=TeamColors[byte(fTerrain.Land[i,k].TileOwner)] shr 16 and $FF;
    end;
  end;

  Loc:=TKMPointList.Create;
  for i:=1 to fPlayers.PlayerCount do begin
    fPlayers.Player[i].GetUnitLocations(Loc);
    for k:=1 to Loc.Count do begin
      MM[Loc.List[k].Y,Loc.List[k].X].R:=TeamColors[i] and $FF;
      MM[Loc.List[k].Y,Loc.List[k].X].G:=TeamColors[i] shr 8 and $FF;
      MM[Loc.List[k].Y,Loc.List[k].X].B:=TeamColors[i] shr 16 and $FF;
    end;
  end;
  Loc.Free;
end;


{ This whole thing is very CPU intesive think of it - to update all 40000tiles }
//Don't use any advanced math here, only simpliest operations - + div * 
procedure TTerrain.UpdateState;
var i,k,h,j:integer;
  procedure SetLand(x,y,tile:byte; Spec:TFieldSpecial);
  begin
    Land[y,x].Terrain:=tile;
    Land[y,x].FieldSpecial:=Spec;
  end;
begin
  inc(AnimStep);
  for i:=1 to FallingTrees.Count do
    inc(Land[FallingTrees.List[i].Y,FallingTrees.List[i].X].FallingTreeAnimStep);

for i:=1 to MapY do
  for k:=1 to MapX do
  //All those global things can be performed once a sec, or even less frequent
  if (i*MapX+k+AnimStep) mod round(TERRAIN_PACE div GAME_LOGIC_PACE) = 0 then begin

  if FOG_OF_WAR_ENABLE then
  for h:=1 to 8 do
    if Land[i,k].FogOfWar[h] > TERRAIN_FOG_OF_WAR_MIN then dec(Land[i,k].FogOfWar[h]);

    if InRange(Land[i,k].FieldAge,1,65534) then inc(Land[i,k].FieldAge);

    if Land[i,k].FieldType=fdt_Field then begin
      case Land[i,k].FieldAge of
         45: SetLand(k,i,61,fs_None);  //Numbers are measured from KaM, ~195sec
        240: SetLand(k,i,59,fs_None);
        435: SetLand(k,i,60,fs_Corn1);
        630: SetLand(k,i,60,fs_Corn2);
        650: Land[i,k].FieldAge:=65535; //Skip to the end
      end;
    end else
    if Land[i,k].FieldType=fdt_Wine then begin
      case Land[i,k].FieldAge of
        45: SetLand(k,i,55,fs_Wine1);
       240: SetLand(k,i,55,fs_Wine2);
       435: SetLand(k,i,55,fs_Wine3);
       630: SetLand(k,i,55,fs_Wine4);
       650: Land[i,k].FieldAge:=65535; //Skip to the end
      end;
    end;

    if InRange(Land[i,k].TreeAge,1,65534) then inc(Land[i,k].TreeAge);
    for h:=1 to length(ChopableTrees) do
      for j:=1 to 3 do
        if Land[i,k].Obj=ChopableTrees[h,j] then
          case Land[i,k].TreeAge of
            45: Land[i,k].Obj:=ChopableTrees[h,2];
           240: Land[i,k].Obj:=ChopableTrees[h,3];
           435: Land[i,k].Obj:=ChopableTrees[h,4];
           630: Land[i,k].TreeAge:=65535; //Skip to the end
          end;

  end;
end;


procedure TTerrain.UpdateCursor(aCursor:cmCursorMode; Loc:TKMPoint);
begin
  CursorMode.Mode:=aCursor;
  CursorPos:=Loc;
end;


procedure TTerrain.Paint;
var x1,x2,y1,y2:integer;
begin
  x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
  y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

  fRender.RenderTerrain(x1,x2,y1,y2,AnimStep);
  fRender.RenderTerrainFieldBorders(x1,x2,y1,y2);
  fRender.RenderTerrainObjects(x1,x2,y1,y2,AnimStep);

  if ShowTerrainWires then fRender.RenderDebugWires();
  if MakeShowUnitMove then fRender.RenderDebugUnitMoves();

end;

end.
