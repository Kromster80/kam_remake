unit KM_Terrain;
interface
uses Controls, StdCtrls, Math, KM_Defaults, KromUtils;

const
MaxMapSize=176; //I have a request, keep it 176 for now, as it will help to solve compatibility issues (just like those you've mentioned).

type TPassability = (canWalk, canWalkRoad, canBuild, canMakeRoads, canMakeFields, canPlantTrees, canFish);
     TPassabilitySet = set of TPassability;
const PassabilityStr:array[1..7] of string = ('canWalk', 'canWalkRoad', 'canBuild', 'canMakeRoads', 'canMakeFields', 'canPlantTrees', 'canFish');
{canWalk - General passability of tile for any walking units}
{canWalkRoad - Type of passability for Serfs when transporting goods, only roads have it}
{canBuild - Can we build a house on this tile?}
{canMakeRoads - Thats less strict than house building, roads can be placed almost everywhere where units can walk, except e.g. bridges}
{canMakeFields - Thats more strict than roads, cos e.g. on beaches you can't make fields}
{canPlantTrees - If Forester can plant a tree here, dunno if it's the same as fields}
{canFish - water tiles where fisherman can fish}

type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
private
  AnimStep:integer;

public
  MapX,MapY:integer; //Terrain width and height

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
    TreeAge:byte;  //Empty=0, 1, 2, 3, 4, Full=255

    //SpecialObjects on field/wine, depends on FieldAge (x4 straw, x2 grapes)
    FieldSpecial:TFieldSpecial;  //fs_None, fs_Corn1, fs_Corn2, fs_Wine1, fs_Wine2, fs_Wine3, fs_Wine4, fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4

    //Another var for borders (ropes, planks, stones) Top and Left
    BorderX,BorderY:TBorderType;
  end;
  
  constructor Create;
  procedure MakeNewMap(Width,Height:integer);
  function OpenMapFromFile(filename:string):boolean;

  procedure SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
  procedure RemMarkup(Loc:TKMPoint);
  procedure SetField(Loc:TKMPoint; aOwner:TPlayerID; aFieldType:TFieldType);
  procedure IncFieldState(Loc:TKMPoint);

  procedure SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; fdt:TFieldType);
  procedure SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
  function CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType):boolean;
  function CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup):boolean;

  function FindGrapes(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindCorn(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindCornField(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindStone(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;

  procedure AddTree(Loc:TKMPoint; ID:integer);
  procedure ChopTree(Loc:TKMPoint);
  procedure InitGrowth(Loc:TKMPoint);
  procedure CutCorn(Loc:TKMPoint);
  procedure CutGrapes(Loc:TKMPoint);

  procedure AddPassability(Loc:TKMPoint; aPass:TPassabilitySet);
  procedure RemPassability(Loc:TKMPoint; aPass:TPassabilitySet);
  procedure MakeRoute(LocA, LocB:TKMPoint; aPass:TPassability; out NodeCount:integer; out Nodes:array of TKMPoint);

  function TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
  function VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
  procedure UpdateBorders(Loc:TKMPoint);
  procedure FlattenTerrain(Loc:TKMPoint);
  procedure RebuildLighting(LowX,HighX,LowY,HighY:integer);
  function ConvertCursorToMapCoord(inX,inY:single):single;
  function InterpolateLandHeight(inX,inY:single):single;

  procedure UpdateState;
  procedure UpdateCursor(aCursor:cmCursorMode; Loc:TKMPoint);
  procedure Paint;
end;

var
  fTerrain: TTerrain;

implementation

uses KM_Unit1, KM_Viewport, KM_Render, KM_Users;

constructor TTerrain.Create;
begin
//Don't know what to put here yet
end;


//Reset whole map with default values
procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k:integer;
begin
  MapX:=min(Width,MaxMapSize);
  MapY:=min(Height,MaxMapSize);

  for i:=1 to MapY do for k:=1 to MapX do with Land[i,k] do begin
    Terrain:=0;
    Height:=random(7);    //variation in height
    Rotation:=random(4);  //Make it random
    Obj:=255;             //none
    FieldSpecial:=fs_None;
    Markup:=mu_None;
    Passability:=[canWalk, canBuild, canMakeRoads, canMakeFields, canPlantTrees]; //allow anything
    TileOwner:=play_none;
    FieldType:=fdt_None;
    FieldAge:=0;
    TreeAge:=0;
    BorderX:=bt_None;
    BorderY:=bt_None;
  end;

  RebuildLighting(1,MapX,1,MapY);
end;


function TTerrain.OpenMapFromFile(filename:string):boolean;
var
  i,k:integer;
  c:array[1..23]of byte;
  f:file;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
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
      //Land[i,k].Passability:=[CanWalk, CanBuild, CanPlantTrees, CanMakeFields];
      //Land[i,k].TileOwner:=play_none; //no roads
    end;
closefile(f);
RebuildLighting(1,MapX,1,MapY);
Result:=true;
end;


{Check if requested tile is within Map boundaries}
function TTerrain.TileInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,MapX-1-Inset) and InRange(Y,1+Inset,MapY-1-Inset);
end;


{Check if requested vertice is within Map boundaries}
function TTerrain.VerticeInMapCoords(X,Y:integer; Inset:byte=0):boolean;
begin
  Result := InRange(X,1+Inset,MapX-Inset) and InRange(Y,1+Inset,MapY-Inset);
end;


{Place markup on tile, any new markup replaces old one, thats okay}
procedure TTerrain.SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
begin
  Land[Loc.Y,Loc.X].Markup:=aMarkup;
end;

{Remove markup from tile}
procedure TTerrain.RemMarkup(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].Markup:=mu_None;
end;


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
    RemPassability(Loc,[canBuild]);
  end else
  if aFieldType=fdt_Wine  then begin
    Land[Loc.Y,Loc.X].Terrain:=55;
    Land[Loc.Y,Loc.X].Rotation:=0;
    RemPassability(Loc,[canBuild]);
  end else begin
    RemPassability(Loc,[CanPlantTrees, CanMakeFields]);
    AddPassability(Loc,[canBuild]);
  end;
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


{ Should find closest wine field around.
Perhaps this could be merged with FindCorn? }
function TTerrain.FindGrapes(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      if Land[i,k].FieldType=fdt_Wine then
        if Land[i,k].FieldAge=65535 then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindCorn(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      if Land[i,k].FieldType=fdt_Field then
        if Land[i,k].FieldAge=65535 then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindCornField(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      if Land[i,k].FieldType=fdt_Field then
        if Land[i,k].FieldAge=0 then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k,h:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      for h:=1 to length(ChopableTrees) do
        if Land[i,k].Obj=ChopableTrees[h,4] then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindStone(aPosition:TKMPoint; aRadius:integer):TKMPoint;
//var i,k,h:integer;
begin
Result:=KMPoint(1,1);
{Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      for h:=1 to length(ChopableTrees) do
        if Land[i,k].Obj=ChopableTrees[h,4] then
          Result:=KMPoint(k,i);}
end;


{Find suitable place for planting a tree.
Prefer ex-trees locations}
function TTerrain.FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var h,i,k,ci,ck:integer; List1,List2:array of TKMPoint; FoundExTree:boolean;
begin
setlength(List1,1024);
setlength(List2,1024);
ci:=0; ck:=0;
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (TileInMapCoords(k,i,1))and(KMLength(aPosition,KMPoint(k,i))<=aRadius) then
      if CanPlantTrees in Land[i,k].Passability then begin

        FoundExTree:=false;

        //If there's an object, check if it is ex-tree
        if Land[i,k].Obj<>255 then
        for h:=1 to length(ChopableTrees) do
          if Land[i,k].Obj=ChopableTrees[h,6] then
            FoundExTree:=true;

        if FoundExTree then
          begin
            List1[ci]:=KMPoint(k,i);
            inc(ci);
          end else begin
            List2[ck]:=KMPoint(k,i);
            inc(ck);
          end;

        Assert(ci<length(List1),'Can''t make a list for tree placement');
        Assert(ck<length(List2),'Can''t make a list for tree placement');

      end;
if ci<>0 then
  Result:=List1[random(ci)]
else
  Result:=List2[random(ck)];
end;


procedure TTerrain.AddTree(Loc:TKMPoint; ID:integer);
begin
  Land[Loc.Y,Loc.X].Obj:=ID;
  RemPassability(Loc,[canBuild,canPlantTrees]);
  Land[Loc.Y,Loc.X].TreeAge:=1;
end;

{}
procedure TTerrain.ChopTree(Loc:TKMPoint);
var h:integer;
begin
  for h:=1 to length(ChopableTrees) do
    if ChopableTrees[h,4]=Land[Loc.Y,Loc.X].Obj then
      Land[Loc.Y,Loc.X].Obj:=ChopableTrees[h,6];

  AddPassability(Loc,[canBuild,canPlantTrees]);
  Land[Loc.Y,Loc.X].TreeAge:=0;
end;


procedure TTerrain.InitGrowth(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].FieldAge:=1;
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

procedure TTerrain.AddPassability(Loc:TKMPoint; aPass:TPassabilitySet);
begin
Land[Loc.Y,Loc.X].Passability:=Land[Loc.Y,Loc.X].Passability + aPass;
end;


procedure TTerrain.RemPassability(Loc:TKMPoint; aPass:TPassabilitySet);
begin
Land[Loc.Y,Loc.X].Passability:=Land[Loc.Y,Loc.X].Passability - aPass;
end;

{Find a route from A to B which meets aPass Passability}
{Results should be written as NodeCount of waypoint nodes to Nodes}
{Simplification1 - ajoin nodes that don't require direction change}
procedure TTerrain.MakeRoute(LocA, LocB:TKMPoint; aPass:TPassability; out NodeCount:integer; out Nodes:array of TKMPoint);
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
    if (MinCost.Pos.X <> LocB.X)or(MinCost.Pos.Y <> LocB.Y) then begin

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
  until((k=500)or(OCount+8>=length(OList))or((MinCost.Pos.X = LocB.X)and(MinCost.Pos.Y = LocB.Y)));

  if (MinCost.Pos.X <> LocB.X)or(MinCost.Pos.Y <> LocB.Y)or(k=200) then begin
    NodeCount:=1;
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
    NodeCount:=1;
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

  if NodeCount>3 then begin
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
  end;

  for i:=1 to NodeCount do
    Assert(TileInMapCoords(Nodes[i-1].X,Nodes[i-1].Y));
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


{Place house plan on terrain and change terrain properties accordingly}
procedure TTerrain.SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; fdt:TFieldType);
var i,k:integer;
  procedure OccupyTile(X,Y:integer);
  var i,k:integer;
  begin
    for i:=-1 to 1 do for k:=-1 to 1 do
      if TileInMapCoords(X+k,Y+i) then
        RemPassability(KMPoint(X+k,Y+i),[canBuild,canPlantTrees]);

    RemPassability(KMPoint(X,Y),[CanMakeRoads,CanMakeFields]);
    if fdt=fdt_House then
      RemPassability(KMPoint(X,Y),[CanWalkRoad,CanWalk]);
  end;
begin
  for i:=1 to 4 do for k:=1 to 4 do begin

    if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
      OccupyTile(Loc.X+k-3, Loc.Y+i-4);
      Land[Loc.Y+i-4,Loc.X+k-3].FieldType:=fdt;
    end;

    if TileInMapCoords(Loc.X+k-3,Loc.Y+i-4) then
      UpdateBorders(KMPoint(Loc.X+k-3, Loc.Y+i-4));
  end;
end;

{That is mainly used for minimap now}
procedure TTerrain.SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
var i,k:integer;
begin
  if aHouseType<>ht_None then //If this is a house make a change for whole place
    for i:=1 to 4 do for k:=1 to 4 do begin
      if HousePlanYX[byte(aHouseType),i,k]<>0 then
        Land[Loc.Y+i-4,Loc.X+k-3].TileOwner:=aOwner;
    end;
  if aHouseType=ht_None then
    Land[Loc.Y,Loc.X].TileOwner:=aOwner;
end;

{Check if house can be placed in that place}
function TTerrain.CanPlaceHouse(Loc:TKMPoint; aHouseType: THouseType):boolean;
var i,k:integer;
begin
Result:=true;
  for i:=1 to 4 do for k:=1 to 4 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then begin
      Result := Result AND TileInMapCoords(Loc.X+k-3,Loc.Y+i-4,1); //Inset one tile from map edges
      if aHouseType<>ht_Wall then
        Result := Result AND (CanBuild in Land[Loc.Y+i-4,Loc.X+k-3].Passability)
      //else
        //Result := Result AND (CanWalk in Land[Loc.Y+i-4,Loc.X+k-3].Passability);
    end;
end;

function TTerrain.CanPlaceRoad(Loc:TKMPoint; aMarkup: TMarkup):boolean;
begin  
  Result:=true;
  Result := Result AND TileInMapCoords(Loc.X,Loc.Y,1); //Do inset one tile from map edges
  Result := Result AND (CanMakeFields in Land[Loc.Y,Loc.X].Passability);
  Result := Result AND (ControlList.HousesHitTest(Loc.X,Loc.Y)=nil);
  if aMarkup <> mu_RoadPlan then //Don't allow fields on fields
    Result := Result AND (Land[Loc.Y,Loc.X].FieldType=fdt_None);
  //Add other check here, e.g. trees, fields, etc..
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
                         +fTerrain.Land[Tmp,Xc+1].Height*frac(InX))/xh;
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
  Tmp1:=mix(fTerrain.Land[Yc  ,Xc+1].Height, fTerrain.Land[Yc  ,Xc].Height, frac(InX));
  Tmp2:=mix(fTerrain.Land[Yc+1,Xc+1].Height, fTerrain.Land[Yc+1,Xc].Height, frac(InX));
  Result:=mix(Tmp2, Tmp1, frac(InY));
end;


procedure TTerrain.UpdateState;
var i,k,h,j:integer;
  procedure SetLand(x,y,tile:byte; Spec:TFieldSpecial);
  begin
    Land[y,x].Terrain:=tile;
    Land[y,x].FieldSpecial:=Spec;
  end;
begin
  inc(AnimStep);

for i:=1 to MapY do
  for k:=1 to MapX do
  if (i*MapX+k+AnimStep) mod round(TERRAIN_PACE/GAME_LOGIC_PACE) = 0 then begin //All those global things can be performed once a sec, or even less frequent

    if InRange(Land[i,k].FieldAge,1,65534) then inc(Land[i,k].FieldAge);

    if Land[i,k].FieldType=fdt_Field then begin
      case Land[i,k].FieldAge of
         45: SetLand(k,i,61,fs_None);  //Numbers are measured from KaM, 195
        240: SetLand(k,i,59,fs_None);
        435: SetLand(k,i,60,fs_Corn1);
        630: SetLand(k,i,60,fs_Corn2);
        650: Land[i,k].FieldAge:=65535; //Skip to the end
      end;
    end else
    if Land[i,k].FieldType=fdt_Wine then begin
      case Land[i,k].FieldAge of
        10: SetLand(k,i,55,fs_Wine1);
        20: SetLand(k,i,55,fs_Wine2);
        30: SetLand(k,i,55,fs_Wine3);
        40: SetLand(k,i,55,fs_Wine4);
        50: Land[i,k].FieldAge:=65535; //Skip to the end
      end;
    end;

    if InRange(Land[i,k].TreeAge,1,254) then inc(Land[i,k].TreeAge);
    for h:=1 to length(ChopableTrees) do
      for j:=1 to 3 do
        if Land[i,k].Obj=ChopableTrees[h,j] then
          case Land[i,k].TreeAge of
            1: Land[i,k].Obj:=ChopableTrees[h,2];
            3: Land[i,k].Obj:=ChopableTrees[h,3];
            5: Land[i,k].Obj:=ChopableTrees[h,4];
            7: Land[i,k].TreeAge:=255; //Skip to the end
          end;

  end;
end;


procedure TTerrain.UpdateCursor(aCursor:cmCursorMode; Loc:TKMPoint);
begin
  CursorMode.Mode:=aCursor;
  CursorPos:=Loc;
end;


procedure TTerrain.Paint;
var i,k:integer; x1,x2,y1,y2:integer;
begin
x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

fRender.RenderTerrainAndFields(x1,x2,y1,y2);

for i:=y1 to y2 do for k:=x1 to x2 do
  begin
    if Land[i,k].BorderX <> bt_None then
      fRender.RenderBorder(Land[i,k].BorderX,1,k,i); //Horizontal

    if Land[i,k].BorderY <> bt_None then
      fRender.RenderBorder(Land[i,k].BorderY,2,k,i); //Vertical

    if Land[i,k].Markup in [mu_RoadPlan..mu_WinePlan] then
      fRender.RenderMarkup(byte(Land[i,k].Markup),k,i); //Input in range 1..3

    if Land[i,k].Obj<>255 then
      fRender.RenderObject(Land[i,k].Obj+1,AnimStep,k,i);

    if Land[i,k].FieldSpecial<>fs_None then
      fRender.RenderObjectSpecial(Land[i,k].FieldSpecial,AnimStep,k,i);

  end;

case CursorMode.Mode of
  cm_None:;
  cm_Erase: fRender.RenderWireQuad(CursorPos, $FF0000FF); //Red quad
  cm_Road: fRender.RenderWireQuad(CursorPos, $FFFFFF00); //Cyan quad
  cm_Field: fRender.RenderWireQuad(CursorPos, $FFFFFF00); //Cyan quad
  cm_Wine: fRender.RenderWireQuad(CursorPos, $FFFFFF00); //Cyan quad
  cm_Houses: fRender.RenderWireHousePlan(CursorPos, THouseType(CursorMode.Param)); //Cyan quad
end;
end;

end.
