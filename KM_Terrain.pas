unit KM_Terrain;
interface

uses Controls, StdCtrls, Math, KM_Defaults, KromUtils;

const
MaxMapSize=176;         //Single cell size in pixels

//type TRoadBuildState = (rbs_1=1,)

type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
private
AnimStep:integer;
protected
public
  Land:array[1..MaxMapSize,1..MaxMapSize]of record
    Terrain,Height,Rotation,Obj:byte;
    Normal:record X,Y,Z:single; end;
    Light:single;
    Passability:byte;
    RoadState:byte;
  end;
  constructor Create;
  procedure MakeNewMap(Width,Height:integer);
  function OpenMapFromFile(filename:string):boolean;
  procedure RebuildNormals(LowX,HighX,LowY,HighY:integer);
  procedure RebuildLightning(LowX,HighX,LowY,HighY:integer);
  function ConvertSquareToMapCoord(inX,inY:single):single;
  procedure IncRoadState(Loc:TKMPoint);
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
begin
//  TimeDelta:= GetTickCount - fLastUpdateTime;
//  fLastUpdateTime:= GetTickCount;
  inc(AnimStep);
end;

procedure TTerrain.Paint;
var i,k:integer; x1,x2,y1,y2:integer;
begin
x1:=fViewport.GetClip.Left; x2:=fViewport.GetClip.Right;
y1:=fViewport.GetClip.Top;  y2:=fViewport.GetClip.Bottom;

fRender.RenderTerrainAndRoads(x1,x2,y1,y2);

for i:=y1 to y2 do for k:=x1 to x2 do
  if Land[i,k].Obj<>255 then
    fRender.RenderObject(Land[i,k].Obj+1,AnimStep,k,i);
end;

procedure TTerrain.MakeNewMap(Width,Height:integer);
var i,k:integer;
begin
Width:=min(Width,MaxMapSize);
Height:=min(Height,MaxMapSize);

Map.X:=Width;
Map.Y:=Height;

for i:=1 to Height do for k:=1 to Width do with Land[i,k] do begin
Terrain:=0;
Height:=random(4); //small variation in height
Rotation:=0;
Obj:=255; //none
Passability:=255; //allow anything
RoadState:=0; //no roads
end;
RebuildNormals(1,Map.X,1,Map.Y);
end;

function TTerrain.OpenMapFromFile(filename:string):boolean;
var
  i,k:integer;
  c:array[1..23]of byte;
begin
  Result:=false;
  assignfile(f,filename); reset(f,1);
  blockread(f,Map,8);
  if (Map.X>MaxMapSize)or(Map.Y>MaxMapSize) then
    begin
      closefile(f);
      fLog.AppendLog('TTerrain.OpenMapFromFile - Can''t open the map');
      exit;
    end;
  for i:=1 to Map.Y do for k:=1 to Map.X do
    begin
      blockread(f,c,23);
      Land[i,k].Terrain:=c[1];
      Land[i,k].Height:=c[3];
      Land[i,k].Rotation:=c[4];
      Land[i,k].Obj:=c[6];
      Land[i,k].Passability:=255;
      Land[i,k].RoadState:=0;
    end;

closefile(f);
RebuildNormals(1,Map.X,1,Map.Y);
RebuildLightning(1,Map.X,1,Map.Y);
fMinimap.Repaint;
Result:=true;
end;

procedure TTerrain.RebuildNormals(LowX,HighX,LowY,HighY:integer);
var i,k:integer; x0,x2,y0,y2:integer;
begin
for i:=LowY to HighY do for k:=LowX to HighX do
  with Land[i,k] do begin
    x0:=EnsureRange(k-1,0,Map.X+1);
    x2:=EnsureRange(k+1,0,Map.X+1);
    y0:=EnsureRange(i-1,0,Map.Y+1);
    y2:=EnsureRange(i+1,0,Map.Y+1);
    if (x0=0)or(y0=0)or(x2=Map.X+1)or(y2=Map.Y+1) then
    begin
      Normal.X:=0;
      Normal.Y:=0;
      Normal.Z:=-1;
    end
    else
    begin
      Normal.X:=(Land[i ,x0].Height-Land[i ,x2].Height)/32; //-100..100 / 3
      Normal.Y:=(Land[y0,k ].Height-Land[y2,k ].Height)/32; //-100..100 / 3
      Normal.Z:=1;
    end;
  end;
end;

procedure TTerrain.RebuildLightning(LowX,HighX,LowY,HighY:integer);
var i,k:integer; x0,y2:integer;
begin
for i:=LowY to HighY do for k:=LowX to HighX do
  with Land[i,k] do begin
    x0:=EnsureRange(k-1,0,Map.X+1);
    y2:=EnsureRange(i+1,0,Map.Y+1);
    if (x0=0)or(y2=Map.Y+1) then
      Light:=0
    else
      Light:=min(max(Land[i,k].Height-Land[y2,x0].Height,0)/35,0.98)
  end;
end;

procedure TTerrain.IncRoadState(Loc:TKMPoint);
begin
Land[Loc.Y,Loc.X].RoadState:=min(Land[Loc.Y,Loc.X].RoadState+1,4);
end;

function TTerrain.ConvertSquareToMapCoord(inX,inY:single):single;
var ii:integer; Xc,Yc:integer; Tmp:integer; Ycoef:array[-2..4]of single;
begin
  Xc:=EnsureRange(round(inX+0.5),1,Map.X); //Cell below cursor
  Yc:=EnsureRange(round(inY+0.5),1,Map.Y);

  for ii:=-2 to 4 do
  begin//make an array of tile heights above and below cursor (-2..4)
    Tmp:=EnsureRange(Yc+ii,1,Map.Y);
    Ycoef[ii]:=(Yc-1)+ii-(fTerrain.Land[Tmp,Xc].Height*(1-frac(InX))+fTerrain.Land[Tmp,Xc+1].Height*frac(InX))/xh;
  end;

for ii:=-2 to 3 do //check if cursor in a tile and adjust it there
  if (InY>=Ycoef[ii])and(InY<=Ycoef[ii+1]) then
    begin
      Result:=Yc+ii-(Ycoef[ii+1]-InY) / (Ycoef[ii+1]-Ycoef[ii]);
      break;
    end;
end;

end.
