//Credit to Paul Bourke (pbourke@swin.edu.au) for the original Fortran 77 Program :))
//Conversion to Visual Basic by EluZioN (EluZioN@casesladder.com)
//Conversion from VB to Delphi6 by Dr Steve Evans (steve@lociuk.com)
///////////////////////////////////////////////////////////////////////////////
//June 2002 Update by Dr Steve Evans (steve@lociuk.com): Heap memory allocation
//added to prevent stack overflow when MaxVertices and MaxTriangles are very large.
//Additional Updates in June 2002:
//Bug in InCircle function fixed. Radius r := Sqrt(rsqr).
//Check for duplicate points added when inserting new point.
//For speed, all points pre-sorted in x direction using quicksort algorithm and
//triangles flagged when no longer needed. The circumcircle centre and radius of
//the triangles are now stored to improve calculation time.
///////////////////////////////////////////////////////////////////////////////
//You can use this code however you like providing the above credits remain in tact

unit Delaunay;
interface
uses KM_Points;

//Set these as applicable
const
  MAX_VERTICES = 5000;
  MAX_TRIANGLES = 10000;


//Created Triangles, vv# are the vertex pointers
type
  TDTriangle = record
    vv0: Word;
    vv1: Word;
    vv2: Word;
    PreCalc: Byte;
    xc,yc,r: Double;
  end;

  TDVertexArray = array [0..MAX_VERTICES] of TKMPoint;
  PVertexArray = ^TDVertexArray;

  TDTriangleArray = array [0..MAX_TRIANGLES] of TDTriangle;
  PTriangleArray = ^TDTriangleArray;

type
  TDelaunay = class
  private
    fTolerance: Single;
    fPolyCount: Integer;
    fVerticeCount: Integer;
    function InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
             var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;
    function WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
    function Triangulate(aVertCount: Integer): Integer;
    procedure QuickSort(var A: PVertexArray; aLow, aHigh: Integer);
  public
    Vertex: PVertexArray;
    Triangle: PTriangleArray;
    constructor Create(x1,y1,x2,y2: Integer);
    destructor Destroy; override;
    procedure AddPoint(x,y: Integer);
    procedure Mesh;
    property PolyCount: Integer read fPolyCount;
    property Tolerance: Single read fTolerance write fTolerance;
    property VerticeCount: Integer read fVerticeCount;
  end;


implementation


type
  TDComplete = array [0..MAX_TRIANGLES] of Boolean;
  PComplete = ^TDComplete;

  TDEdges = array [0..1, 0..MAX_TRIANGLES * 3] of SmallInt;
  PEdges = ^TDEdges;


//Constructor must setup bounding rectangle for all points
constructor TDelaunay.Create(x1,y1,x2,y2: Integer);
begin
  inherited Create;

  //Allocate memory for arrays
  GetMem(Vertex, SizeOf(Vertex^));
  GetMem(Triangle, SizeOf(Triangle^));

  Assert((x2>x1) and (y2>y1), 'Bounding rect must have positive area');

  fTolerance := 0.01;

  //Points added in counter-clockwise order:
  // 1 3
  // 2 4
  AddPoint(x1,y1);
  AddPoint(x1,y2);
  AddPoint(x2,y2);
  AddPoint(x2,y1);
end;


destructor TDelaunay.Destroy;
begin
  //Free memory for arrays
  FreeMem(Vertex, SizeOf(Vertex^));
  FreeMem(Triangle, SizeOf(Triangle^));

  inherited;
end;


function TDelaunay.InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
  var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;
//Return TRUE if the point (xp,yp) lies inside the circumcircle
//made up by points (x1,y1) (x2,y2) (x3,y3)
//The circumcircle centre is returned in (xc,yc) and the radius r
//NOTE: A point on the edge is inside the circumcircle
var
  eps: Double;
  m1: Double;
  m2: Double;
  mx1: Double;
  mx2: Double;
  my1: Double;
  my2: Double;
  dx: Double;
  dy: Double;
  rsqr: Double;
  drsqr: Double;
begin

  eps:= 0.000001;
  Result := False;

  //Check if xc,yc and r have already been calculated
  if  Triangle^[j].PreCalc = 1 then
  begin
    xc := Triangle^[j].xc;
    yc := Triangle^[j].yc;
    r  := Triangle^[j].r;
    rsqr := r*r;
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;
  end else
  begin
    if (Abs(y1 - y2) < eps) And (Abs(y2 - y3) < eps) then
      Assert(False, 'INCIRCUM - F - Points are coincident !!');

    if Abs(y2 - y1) < eps then
    begin
      m2 := -(x3 - x2) / (y3 - y2);
      mx2 := (x2 + x3) / 2;
      my2 := (y2 + y3) / 2;
      xc := (x2 + x1) / 2;
      yc := m2 * (xc - mx2) + my2;
    end
    else if Abs(y3 - y2) < eps then
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      mx1 := (x1 + x2) / 2;
      my1 := (y1 + y2) / 2;
      xc := (x3 + x2) / 2;
      yc := m1 * (xc - mx1) + my1;
    end
    else
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      m2 := -(x3 - x2) / (y3 - y2);
      mx1 := (x1 + x2) / 2;
      mx2 := (x2 + x3) / 2;
      my1 := (y1 + y2) / 2;
      my2 := (y2 + y3) / 2;
      if (m1-m2)<>0 then  //se
      begin
        xc := (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2);
        yc := m1 * (xc - mx1) + my1;
      end else
      begin
        xc:= (x1+x2+x3)/3;
        yc:= (y1+y2+y3)/3;
      end;

    end;

    dx := x2 - xc;
    dy := y2 - yc;
    rsqr := dx * dx + dy * dy;
    r := Sqrt(rsqr);
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;

    //store the xc,yc and r for later use
    Triangle^[j].PreCalc:=1;
    Triangle^[j].xc:=xc;
    Triangle^[j].yc:=yc;
    Triangle^[j].r:=r;
  end;

  if drsqr <= rsqr then
    Result := True;
end;


function TDelaunay.WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
//Determines which side of a line the point (xp,yp) lies.
//The line goes from (x1,y1) to (x2,y2)
//Returns -1 for a point to the left
//         0 for a point on the line
//        +1 for a point to the right
var
  equation: Double;
begin
  equation := ((yp - y1) * (x2 - x1)) - ((y2 - y1) * (xp - x1));

  If equation > 0 then
     WhichSide := -1
  else If equation = 0 then
     WhichSide := 0
  else
     WhichSide := 1;
end;


function TDelaunay.Triangulate(aVertCount: Integer): Integer;
//Takes as input NVERT vertices in arrays Vertex()
//Returned is a list of NTRI triangular faces in the array
//Triangle(). These triangles are arranged in clockwise order.
var
  Complete: PComplete;
  Edges: PEdges;
  Nedge: LongInt;

  //General Variables
  VertID : Integer;
  j : Integer;
  k : Integer;
  oPolyCount : Integer;
  xc : Double;
  yc : Double;
  r : Double;
  InCir : Boolean;
begin
  //Allocate memory
  GetMem(Complete, sizeof(Complete^));
  GetMem(Edges, sizeof(Edges^));

  Triangle^[0].vv0 := 0;
  Triangle^[0].vv1 := 1;
  Triangle^[0].vv2 := 2;
  Triangle^[0].Precalc := 0;
  Triangle^[1].vv0 := 0;
  Triangle^[1].vv1 := 2;
  Triangle^[1].vv2 := 3;
  Triangle^[1].Precalc := 0;
  Complete^[0] := False;
  Complete^[1] := False;
  oPolyCount := 2;

  //Include each point one at a time into the existing mesh
  for VertID := 4 to aVertCount - 1 do
  begin
    Nedge := 0;
    //Set up the edge buffer.
    //If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    //three edges of that triangle are added to the edge buffer.
    J := 0;
    repeat
      if not Complete^[J] then
      begin
        InCir := InCircle(Vertex^[VertID].x, Vertex^[VertID].y, Vertex^[Triangle^[J].vv0].x,
                          Vertex^[Triangle^[J].vv0].y, Vertex^[Triangle^[J].vv1].x,
                          Vertex^[Triangle^[J].vv1].y, Vertex^[Triangle^[J].vv2].x,
                          Vertex^[Triangle^[J].vv2].y, xc, yc, r, J);
        //Include this if points are sorted by X
        if (xc + r) < Vertex^[VertID].x then  //
          Complete^[J] := True          //
        else                            //
          if InCir then
          begin
            Edges^[0, Nedge + 0] := Triangle^[J].vv0;
            Edges^[1, Nedge + 0] := Triangle^[J].vv1;
            Edges^[0, Nedge + 1] := Triangle^[J].vv1;
            Edges^[1, Nedge + 1] := Triangle^[J].vv2;
            Edges^[0, Nedge + 2] := Triangle^[J].vv2;
            Edges^[1, Nedge + 2] := Triangle^[J].vv0;
            Nedge := Nedge + 3;
            //Move last triangle to J
            Triangle^[J].vv0 := Triangle^[oPolyCount - 1].vv0;
            Triangle^[J].vv1 := Triangle^[oPolyCount - 1].vv1;
            Triangle^[J].vv2 := Triangle^[oPolyCount - 1].vv2;
            Triangle^[J].PreCalc:=Triangle^[oPolyCount - 1].PreCalc;
            Triangle^[J].xc:=Triangle^[oPolyCount - 1].xc;
            Triangle^[J].yc:=Triangle^[oPolyCount - 1].yc;
            Triangle^[J].r:=Triangle^[oPolyCount - 1].r;
            Triangle^[oPolyCount - 1].PreCalc:=0;
            Complete^[J] := Complete^[oPolyCount - 1];
            J := J - 1;
            oPolyCount := oPolyCount - 1;
          end;
      end;
      J := J + 1;
    until(J >= oPolyCount);

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    for J := 0 to Nedge - 1 do
    if not (Edges^[0, J] = -1) and not (Edges^[1, J] = -1) then
    for K := J + 1 to Nedge - 1 do
    if not (Edges^[0, K] = -1) and not (Edges^[1, K] = -1) then
    if (Edges^[0, J] = Edges^[1, K]) and (Edges^[1, J] = Edges^[0, K]) then
    begin
      Edges^[0, J] := -1;
      Edges^[1, J] := -1;
      Edges^[0, K] := -1;
      Edges^[1, K] := -1;
    end;

    //  Form new triangles for the current point
    //  Skipping over any tagged edges.
    //  All edges are arranged in clockwise order.
    for J := 0 to Nedge - 1 do
    if not (Edges^[0, J] = -1) and not (Edges^[1, J] = -1) then
    begin
      Triangle^[oPolyCount].vv0 := Edges^[0, J];
      Triangle^[oPolyCount].vv1 := Edges^[1, J];
      Triangle^[oPolyCount].vv2 := VertID;
      Triangle^[oPolyCount].PreCalc := 0;
      Complete^[oPolyCount] := False;
      oPolyCount := oPolyCount + 1;
    end;
  end;

  Result := oPolyCount;

  //Free memory
  FreeMem(Complete, sizeof(Complete^));
  FreeMem(Edges, sizeof(Edges^));
end;


procedure TDelaunay.AddPoint(x,y: Integer);
var
  I: Integer;
begin
  //Skip duplicate points
  for I := 0 to fVerticeCount - 1 do
  if (Abs(x-Vertex^[I].x) < fTolerance)
  and(Abs(y-Vertex^[I].y) < fTolerance) then
    Exit;

  Vertex^[fVerticeCount].x := x;
  Vertex^[fVerticeCount].y := y;
  Inc(fVerticeCount);
end;


procedure TDelaunay.Mesh;
begin
  if fVerticeCount < 4 then Exit;
  //Sort added points, skip bounding rect (first 4 points)
  QuickSort(Vertex, 4, fVerticeCount - 1);
  fPolyCount := Triangulate(fVerticeCount); //Returns number of triangles created
end;


procedure TDelaunay.QuickSort(var A: PVertexArray; aLow,aHigh: Integer);
//Sort all points by x
  procedure DoQuickSort(var A: PVertexArray; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid: Double;
    T: TKMPoint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A^[(Lo + Hi) div 2].x;
    repeat
      while A^[Lo].x < Mid do Inc(Lo);
      while A^[Hi].x > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A^[Lo];
        A^[Lo] := A^[Hi];
        A^[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then DoQuickSort(A, iLo, Hi);
    if Lo < iHi then DoQuickSort(A, Lo, iHi);
  end;
begin
  if aHigh > aLow then
    DoQuickSort(A, aLow, aHigh);
end;


end.
