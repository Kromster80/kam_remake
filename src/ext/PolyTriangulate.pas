//
//Original poly_unit.pas code by David E. Dirkse
//(http://www.davdata.nl/math/index.html)
//Refactoring by Krom Stern 2012
//
unit PolyTriangulate;
interface
uses KM_Points, Math;

const
  maxpolypoint = 100;

procedure Triangulate(pts: TKMPointArray; Count: Byte; var Tris: array of Word; out TCount: Word);


implementation


type
  Tpoints = array [1 .. maxpolypoint] of TKMPoint;

  Tvector = record
    x, y, dx, dy: smallInt;
    dir: double; // direction 0..2*pi
  end;

  TVectorList = array [1 .. maxpolypoint] of Tvector;

  TTriangle = array [0 .. 2] of Tvector;

  TTriangleList = array [0 .. maxpolypoint - 1] of TTriangle;

var
  //In
  PCount: Byte;
  pp: Tpoints;
  //Temp
  VectorList: TVectorList;
  VCount: byte;
  //Out
  TriangleCount: Byte;
  TriangleList: TTriangleList;


function VectorDirection(deltaX,deltaY : double) : double;
//return direction of vector in radians
//(+,0) = 0; (0,+) = 0.5pi ; (-,0) = pi ; (0,-) = 1.5pi
begin
  if deltaX = 0 then
  begin
    if deltaY > 0 then
      result := pi / 2
    else
      result := pi / 2 * 3;
    exit;
  end;
  result := arctan((deltaY) / (deltaX));
  if deltaX < 0 then
    result := result + pi;
  if result < 0 then
    result := result + pi * 2;
end;

function vrsect(const v1,v2 : TVector; out f1, f2: Double): Boolean;
//calculate intersection of vectors v1,v2
//line1 = (v1.x1,v1.y1) +f1*(v1.dx,v1.dy)
//line2 = (v2.x1,v2.y1) +f2*(v2.dx,v2.dy)
//return f1,f2,fvalid
var d,vx,vy : double;
const
    frnd = 1e-6;                 //floating point round to zero
begin
 d := v1.dx*v2.dy - v1.dy*v2.dx;//discriminant

 Result := d <> 0;

 if not Result then
   Exit;

 vx := v2.x - v1.x;
 vy := v2.y - v1.y;
 f1 := (vx*v2.dy - vy*v2.dx)/d;
 f2 := (vx*v1.dy - vy*v1.dx)/d;

 if abs(f1) < frnd then f1 := 0;  //round to 1e-6
 if abs(f2) < frnd then f2 := 0;
 if abs(f1-1) < frnd then f1 := 1;
 if abs(f2-1) < frnd then f2 := 1;
end;

function outward(vnr : word) : boolean;
//return true if  vlist[vnr] points outward
var i : word;
    crosscount : shortInt;
    VAngle : double;
  ValidSect: Boolean;
  f1,f2: Double;
begin
 crosscount := 0;
 for i := 1 to VCount do
  begin
   ValidSect := vrsect(VectorList[vnr],VectorList[i], f1, f2);
     if ValidSect and (f1 > 1) then
      begin
       VAngle := VectorList[i].dir - VectorList[vnr].dir;
       if Vangle < 0 then Vangle := Vangle + pi*2;
       if Vangle < pi then
        begin
         if (f2 = 0) or (f2 = 1) then inc(crosscount);   //left crossing
         if (f2 > 0) and (f2 < 1) then inc(crosscount,2);//..
        end;
       if VAngle > pi then
        begin
         if (f2 = 0) or (f2 = 1) then dec(crosscount);   //right crossing
         if (f2 > 0) and (f2 < 1) then dec(crosscount,2);//..
        end;
    end;//if fvalid..
  end;//for
 result := crosscount = 0;
end;

function Empty3(i1,i2,i3 : word) : boolean;
//check for no point inside triangle i1,i2 (i3)
//no point : true
var
  k: Word;
  sv, sw: Tvector;
  ValidSect: Boolean;
  f1, f2: double;
begin
  result := False;
  with sv do
  begin
    x := VectorList[i1].x;
    y := VectorList[i1].y;
    dx := VectorList[i3].x - VectorList[i1].x;
    dy := VectorList[i3].y - VectorList[i1].y;
  end;
  for k := 1 to VCount do
    if (k <> i1) and (k <> i2) and (k <> i3) then
    begin
      with sw do
      begin
        x := VectorList[i2].x;
        y := VectorList[i2].y;
        dx := VectorList[k].x - x;
        dy := VectorList[k].y - y;
      end;
      ValidSect := vrsect(sv, sw, f1, f2);
      if ValidSect and (f2 >= 1) and (f1 > 0) and (f1 < 1) then
        exit;
    end; // for
  result := True;
end;

//---- hi level support procedures --------------

//--- build Vectorlist

procedure buildVectorlist;
var I: Word;
begin
  for I := 1 to PCount - 1 do
    with VectorList[I] do
    begin
      x := pp[I].x;
      y := pp[I].y;
      dx := pp[I + 1].x - x;
      dy := pp[I + 1].y - y;
      dir := VectorDirection(dx, dy); // direction of vector
    end;
  VCount := PCount - 1;
end;

//--- check vectorlist

procedure checkvectors;
  function Adjacent(a,b : word): boolean;
  //called by "checkvectors"  (geoVectorcount must be set before)
  //return true if vectors Vlist[a],Vlist[b] share start- endpoint
  begin
    result := ((a = 1) and (b = VCount)) or
              ((b = 1) and (a = VCount)) or
              (abs(a-b) < 2);
  end;
//check Vlist for
// - duplicate points
// - intersections
var
  I, J: Word;
  ValidSect: Boolean;
  f1, f2: double;
begin
  for i := 1 to VCount do // check duplicate points
    for J := i + 1 to VCount do
      Assert((VectorList[i].x <> VectorList[J].x) or (VectorList[i].y <> VectorList[J].y),
        'duplicate point');

  for I := 1 to VCount do // check for no intersection
    for J := 1 to VCount do
      if not Adjacent(i, J) then
      begin
        ValidSect := vrsect(VectorList[i], VectorList[J], f1, f2);
        if ValidSect and InRange(f1, 0, 1) and InRange(f2, 0, 1) then
          Assert(False, 'intersection of edges');
      end;
end;

procedure buildTriangleList;
var
  I, J, ni, nni: Word;
  OK: Boolean;
label loop1, loop2, end1;
begin
  TriangleCount := 0;
  I := 1;

loop1:

  if I > 1 then dec(I);

loop2:

  ni := I + 1;
  if ni > VCount then ni := 1; //correct overflow
  nni := ni + 1;
  if nni > VCount then nni := 1;
  OK := outward(I) and Empty3(I,ni,nni);
  if OK then
  begin
    TriangleList[TriangleCount, 0] := VectorList[I];
    TriangleList[TriangleCount, 1] := VectorList[ni];
    TriangleList[TriangleCount, 2] := VectorList[nni];
    inc(TriangleCount);

    with VectorList[I] do              //replace vlist[i] by vlist[i]+vlist[ni]
    begin
      dx := dx + VectorList[ni].dx;
      dy := dy + VectorList[ni].dy;
      dir := VectorDirection(dx,dy);
    end;
    for J := ni to VCount-1 do
      VectorList[J] := VectorList[J+1];//eliminate vlist[ni]
    dec(VCount);
    if VCount >= 3 then
      goto loop1
    else
      exit;
  end
  else         //not OK
  begin
    inc(I);
    if I <= VCount then goto loop2;
    Assert(False, 'triangulation error');
  end;
end;


procedure Triangulate(pts: TKMPointArray; Count: Byte; var Tris: array of Word; out TCount: Word);
var
  I: Integer;
  K: Integer;
  L: Integer;
begin
  PCount := Count + 1;
  for I := 0 to Count - 1 do
    pp[I+1] := pts[I];
  pp[PCount] := pp[1];

  Assert(PCount > 3, 'insufficient points');

 buildvectorlist;
 checkvectors;
 buildtrianglelist;

 Assert(Length(Tris) >= TriangleCount * 3);
 for I := 0 to TriangleCount - 1 do
 for K := 0 to 2 do
 begin
   for L := 1 to PCount-1 do
   if (pp[L].x = TriangleList[I, K].X)
   and (pp[L].y = TriangleList[I, K].Y) then
     Tris[I*3+K] := L - 1;
 end;
 TCount := TriangleCount;
end;


end.
