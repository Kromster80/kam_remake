unit KM_Terrain;
interface

uses Controls, StdCtrls, Math, KM_Defaults, KromUtils;

type
{Class to store all terrain data, aswell terrain routines}
TTerrain = class
private
protected
public

  Land:array[1..MaxMapSize,1..MaxMapSize]of record
    Terrain,Height,Rotation,Obj,Passability:byte;
    Normal:record X,Y,Z:single; end;
  end;
  constructor Create;
  procedure MakeNewMap(Width,Height:integer);
  function OpenMapFromFile(filename:string):boolean;
  procedure RebuildNormals(LowX,HighX,LowY,HighY:integer);
published

end;

implementation

uses KM_Unit1, KM_Global_Data;

constructor TTerrain.Create;
begin
//Don't know what to put here yet
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
  //    MessageBox(Form1.Handle,'Too big map or not a KaM map.', 'Error', MB_OK);
      exit;
    end;
  for i:=1 to Map.Y do for k:=1 to Map.X do
    begin
      blockread(f,c,23);
      Land[i,k].Terrain:=c[1];
      Land[i,k].Height:=c[3];
      Land[i,k].Rotation:=c[4];
      Land[i,k].Obj:=c[6];
      Land[i,k].Passability:=c[7];
    end; 

closefile(f);
RebuildNormals(1,Map.X,1,Map.Y);
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


end.
