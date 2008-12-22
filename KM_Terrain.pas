unit KM_Terrain;
interface
uses Controls, StdCtrls, Math, KM_Defaults, KromUtils;

const
MaxMapSize=176;

type TPassability = (CanWalk, CanBuild, CanPlantTrees, CanMakeFields, CanFish);

type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
private
  AnimStep:integer;

public
  Land:array[1..MaxMapSize,1..MaxMapSize]of record
    Terrain,Height,Rotation,Obj:byte;

    //KaM stores node lighting in 0..32 range (-16..16), but I want to use -1..1 range
    Light:single;

    //Meant to be set of allowed actions on the tile
    Passability:set of TPassability; //pass_CanWalk, pass_CanBuild, pass_CanPlantTrees, pass_CanField

    //Name says it all
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
    FieldAge:byte;  //Empty=0, 1, 2, 3, 4, Full=255

    //Age of tree, another independent variable since trees can grow on fields
    //Depending on this tree gets older and thus could be chopped
    TreeAge:byte;  //Empty=0, 1, 2, 3, 4, Full=255

    //SpecialObjects on field/wine, depends on FieldAge (x4 straw, x2 grapes)
    FieldSpecial:TFieldSpecial;  //fs_None, fs_Corn1, fs_Corn2, fs_Wine1, fs_Wine2, fs_Wine3, fs_Wine4, fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4

    //Another var for borders (ropes, planks, stones) Top and Left
    BorderX,BorderY:TBorderType;
  end;

  MapX,MapY:integer; //Terrain width and height

  constructor Create;
  procedure MakeNewMap(Width,Height:integer);
  function OpenMapFromFile(filename:string):boolean;
  procedure RebuildLighting(LowX,HighX,LowY,HighY:integer);
  function ConvertCursorToMapCoord(inX,inY:single):single;
  function InterpolateMapCoord(inX,inY:single):single;
public
  procedure IncFieldState(Loc:TKMPoint);
  procedure SetField(Loc:TKMPoint; aOwner:TPlayerID; aFieldType:TFieldType);

  procedure SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
  procedure RemMarkup(Loc:TKMPoint);

  function FindGrapes(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindCorn(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindCornField(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  function FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
  procedure InitGrowth(Loc:TKMPoint);
  procedure CutCorn(Loc:TKMPoint);

  procedure FlattenTerrain(Loc:TKMPoint);
  procedure AddTree(Loc:TKMPoint; ID:integer);
  procedure ChopTree(Loc:TKMPoint);
public
  procedure SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; bt:TBorderType);
  procedure SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
  procedure UpdateBorders(Loc:TKMPoint);
public
  procedure UpdateState;
  procedure Paint;
published

end;

implementation

uses KM_Unit1, KM_Global_Data;

constructor TTerrain.Create;
begin
//Don't know what to put here yet
end;

procedure TTerrain.UpdateState;
var i,k,h,j:integer;

  procedure SetLand(x,y,tile:byte; Spec:TFieldSpecial);
  begin
    Land[y,x].Terrain:=tile;
    Land[y,x].FieldSpecial:=Spec;
  end;
begin
//  TimeDelta:= GetTickCount - fLastUpdateTime;
//  fLastUpdateTime:= GetTickCount;
  inc(AnimStep);
for i:=1 to MapY do
  for k:=1 to MapX do begin

    if (Land[i,k].FieldAge > 0) and (Land[i,k].FieldAge < 255) then inc(Land[i,k].FieldAge);
    if (Land[i,k].TreeAge  > 0) and (Land[i,k].TreeAge  < 255) then inc(Land[i,k].TreeAge);

    if Land[i,k].FieldType=fdt_Field then
    case Land[i,k].FieldAge of
      5:  SetLand(k,i,61,fs_None);
      10: SetLand(k,i,60,fs_Corn1);
      15: SetLand(k,i,59,fs_Corn2);
      16: Land[i,k].FieldAge:=255; //Skip to the end
    end;
    if Land[i,k].FieldType=fdt_Wine then
    case Land[i,k].FieldAge of
      5:  SetLand(k,i,52,fs_None);
      10: SetLand(k,i,52,fs_Wine1);
      15: SetLand(k,i,52,fs_Wine2);
      20: SetLand(k,i,52,fs_Wine3);
      25: SetLand(k,i,52,fs_Wine4);
      26: Land[i,k].FieldAge:=255; //Skip to the end
    end;

    for h:=1 to length(ChopableTrees) do
      for j:=1 to 3 do
        if Land[i,k].Obj=ChopableTrees[h,j] then
          case Land[i,k].TreeAge of
            5:  Land[i,k].Obj:=ChopableTrees[h,2];
            10: Land[i,k].Obj:=ChopableTrees[h,3];
            15: Land[i,k].Obj:=ChopableTrees[h,4];
            16: Land[i,k].TreeAge:=255; //Skip to the end
          end;

  end;
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
end;

procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k:integer;
begin
Width:=min(Width,MaxMapSize);
Height:=min(Height,MaxMapSize);

MapX:=Width;
MapY:=Height;

for i:=1 to Height do for k:=1 to Width do with Land[i,k] do begin
Terrain:=0;
Height:=random(7); //variation in height
Rotation:=0;
Obj:=255; //none
FieldSpecial:=fs_None;
Markup:=mu_None;
Passability:=[CanWalk, CanBuild, CanPlantTrees, CanMakeFields]; //allow anything
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
begin
  Result:=false;
  assignfile(f,filename); reset(f,1);
  blockread(f,MapX,4);
  blockread(f,MapY,4);
  if (MapX>MaxMapSize)or(MapY>MaxMapSize) then
    begin
      closefile(f);
      fLog.AppendLog('TTerrain.OpenMapFromFile - Can''t open the map');
      exit;
    end;
  for i:=1 to MapY do for k:=1 to MapX do
    begin
      blockread(f,c,23);
      Land[i,k].Terrain:=c[1];
      Land[i,k].Height:=c[3];
      Land[i,k].Rotation:=c[4];
      Land[i,k].Obj:=c[6];
      Land[i,k].Passability:=[CanWalk, CanBuild, CanPlantTrees, CanMakeFields];
      Land[i,k].TileOwner:=play_none; //no roads
      Land[i,k].FieldType:=fdt_None;
      Land[i,k].FieldAge:=0;
      Land[i,k].TreeAge:=0;
    end;
closefile(f);
RebuildLighting(1,MapX,1,MapY);
fMiniMap.ReSize(MapX,MapY);
Result:=true;
end;


{ Rebuilds lighting values for given bounds.
These values are used to draw highlights/shadows on terrain.}
procedure TTerrain.RebuildLighting(LowX,HighX,LowY,HighY:integer);
var i,k:integer; x0,y2:integer;
begin
  for i:=LowY to HighY do for k:=LowX to HighX do begin
    x0:=EnsureRange(k-1,1,MapX);
    y2:=EnsureRange(i+1,1,MapY);
    Land[i,k].Light:=EnsureRange((Land[i,k].Height-(Land[y2,k].Height+Land[i,x0].Height)/2)/22,-1,1)*(1-Overlap); //  1.33*16 ~=22
    if (i=1)or(i=MapY)or(k=1)or(k=MapX) then
      Land[i,k].Light:=-1+Overlap;
  end
end;


procedure TTerrain.SetMarkup(Loc:TKMPoint; aMarkup:TMarkup);
begin
  Land[Loc.Y,Loc.X].Markup:=aMarkup;
end;


procedure TTerrain.RemMarkup(Loc:TKMPoint);
begin
  Land[Loc.Y,Loc.X].Markup:=mu_None;
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

procedure TTerrain.SetField(Loc:TKMPoint; aOwner:TPlayerID; aFieldType:TFieldType);
begin
  Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  Land[Loc.Y,Loc.X].Markup:=mu_None;
  Land[Loc.Y,Loc.X].FieldType:=aFieldType;
  Land[Loc.Y,Loc.X].FieldAge:=0;
  Land[Loc.Y,Loc.X].FieldSpecial:=fs_None;

  UpdateBorders(Loc);
  if aFieldType=fdt_Field then Land[Loc.Y,Loc.X].Terrain:=62;
  if aFieldType=fdt_Wine  then Land[Loc.Y,Loc.X].Terrain:=55;
end;

function TTerrain.FindGrapes(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (InRange(i,1,Self.MapY))and(InRange(k,1,Self.MapX))and(GetLength(aPosition.Y-i,aPosition.X-k)<=aRadius) then
      if Land[i,k].FieldType=fdt_Wine then
        if Land[i,k].FieldAge=255 then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindCorn(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (InRange(i,1,Self.MapY))and(InRange(k,1,Self.MapX))and(GetLength(aPosition.Y-i,aPosition.X-k)<=aRadius) then
      if Land[i,k].FieldType=fdt_Field then
        if Land[i,k].FieldAge=255 then
          Result:=KMPoint(k,i);
end;

function TTerrain.FindCornField(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var i,k:integer;
begin
Result:=KMPoint(0,0);
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (InRange(i,1,Self.MapY))and(InRange(k,1,Self.MapX))and(GetLength(aPosition.Y-i,aPosition.X-k)<=aRadius) then
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
    if (InRange(i,1,Self.MapY))and(InRange(k,1,Self.MapX))and(GetLength(aPosition.Y-i,aPosition.X-k)<=aRadius) then
      for h:=1 to length(ChopableTrees) do
        if Land[i,k].Obj=ChopableTrees[h,4] then
          Result:=KMPoint(k,i);
end;


function TTerrain.FindPlaceForTree(aPosition:TKMPoint; aRadius:integer):TKMPoint;
var h,i,k,ci,ck:integer; List1,List2:array of TKMPoint; FoundExTree:boolean;
begin
setlength(List1,1024);
setlength(List2,1024);
ci:=0; ck:=0;
for i:=aPosition.Y-aRadius to aPosition.Y+aRadius do
  for k:=aPosition.X-aRadius to aPosition.X+aRadius do
    if (InRange(i,1,Self.MapY))and(InRange(k,1,Self.MapX))and(GetLength(aPosition.Y-i,aPosition.X-k)<=aRadius) then
      if CanPlantTrees in Land[i,k].Passability then begin

        FoundExTree:=false;

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

        if ci>=length(List1) then Assert(false,'Can''t make a list for tree placement');
        if ck>=length(List2) then Assert(false,'Can''t make a list for tree placement');

      end;
if ci<>0 then
  Result:=List1[random(ci)]
else
  Result:=List2[random(ck)];
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

procedure TTerrain.AddTree(Loc:TKMPoint; ID:integer);
begin
Land[Loc.Y,Loc.X].Obj:=ID;
Land[Loc.Y,Loc.X].Passability:=Land[Loc.Y,Loc.X].Passability-[CanPlantTrees,CanBuild];
Land[Loc.Y,Loc.X].TreeAge:=1;
end;

procedure TTerrain.ChopTree(Loc:TKMPoint);
var h:integer;
begin
for h:=1 to length(ChopableTrees) do
  if ChopableTrees[h,4]=Land[Loc.Y,Loc.X].Obj then
    Land[Loc.Y,Loc.X].Obj:=ChopableTrees[h,6];

Land[Loc.Y,Loc.X].Passability:=Land[Loc.Y,Loc.X].Passability+[CanPlantTrees,CanBuild];
Land[Loc.Y,Loc.X].TreeAge:=0;
end;

procedure TTerrain.SetHousePlan(Loc:TKMPoint; aHouseType: THouseType; bt:TBorderType);
var i,k:integer; hp,hp1,hp2,hp3,hp4:integer;
begin                                     //    4
  for i:=1 to 4 do for k:=1 to 4 do begin // 3  HP 1
                                          //    2
      hp:=min(HousePlanYX[byte(aHouseType),i,k],1);
      if k=4 then hp1:=0 else hp1:=min(HousePlanYX[byte(aHouseType),i,k+1],1);
      if i=4 then hp2:=0 else hp2:=min(HousePlanYX[byte(aHouseType),i+1,k],1);
      if k=1 then hp3:=0 else hp3:=min(HousePlanYX[byte(aHouseType),i,k-1],1);
      if i=1 then hp4:=0 else hp4:=min(HousePlanYX[byte(aHouseType),i-1,k],1);

      if hp<>hp1 then Land[Loc.Y+i-4,Loc.X+k-2].BorderY:=bt;
      if hp<>hp4 then Land[Loc.Y+i-4,Loc.X+k-3].BorderX:=bt;
      if hp<>hp2 then Land[Loc.Y+i-3,Loc.X+k-3].BorderX:=bt;
      if hp<>hp3 then Land[Loc.Y+i-4,Loc.X+k-3].BorderY:=bt;

  end;
end;

procedure TTerrain.SetTileOwnership(Loc:TKMPoint; aHouseType: THouseType; aOwner:TPlayerID);
var i,k:integer;
begin
  if aHouseType<>ht_None then //If this is a house make a change for whole place
    for i:=1 to 4 do for k:=1 to 4 do begin
      if HousePlanYX[byte(aHouseType),i,k]<>0 then
        Land[Loc.Y+i-4,Loc.X+k-3].TileOwner:=aOwner;
    end;
  if aHouseType=ht_None then begin
    Land[Loc.Y,Loc.X].TileOwner:=aOwner;
  end;
end;

procedure TTerrain.UpdateBorders(Loc:TKMPoint);
  function GetBorder(a,b:TFieldType):TBorderType;
  begin
    Result:=bt_None;
    if (a=fdt_Field)or(b=fdt_Field) then Result:=bt_Field;
    if (a=fdt_Wine)or(b=fdt_Wine) then Result:=bt_Wine;
    if a=b then Result:=bt_None;
  end;
begin

  if Loc.X-1>=1 then
  Land[Loc.Y,Loc.X].BorderY:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y,Loc.X-1].FieldType);

  if Loc.Y-1>=1 then
  Land[Loc.Y,Loc.X].BorderX:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y-1,Loc.X].FieldType);

  if Loc.X+1<=MapX then
  Land[Loc.Y,Loc.X+1].BorderY:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y,Loc.X+1].FieldType);

  if Loc.Y+1<=MapY then
  Land[Loc.Y+1,Loc.X].BorderX:=GetBorder(Land[Loc.Y,Loc.X].FieldType,Land[Loc.Y+1,Loc.X].FieldType);

end;

function TTerrain.ConvertCursorToMapCoord(inX,inY:single):single;
var ii:integer; Xc,Yc:integer; Tmp:integer; Ycoef:array[-2..4]of single;
begin
  Xc:=EnsureRange(round(inX+0.5),1,MapX); //Cell below cursor
  Yc:=EnsureRange(round(inY+0.5),1,MapY);

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
end;

function TTerrain.InterpolateMapCoord(inX,inY:single):single;
var Xc,Yc:integer; Tmp1,Tmp2:single;
begin
Xc:=trunc(inX);
Yc:=trunc(inY);
Tmp1:=fTerrain.Land[Yc,Xc].Height*(1-frac(InX))+fTerrain.Land[Yc,Xc+1].Height*frac(InX);
Tmp2:=fTerrain.Land[Yc+1,Xc].Height*(1-frac(InX))+fTerrain.Land[Yc+1,Xc+1].Height*frac(InX);
Result:=Tmp1*(1-frac(InY)) + Tmp2*frac(InY);
end;

end.
