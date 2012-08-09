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
uses Types, KM_Points;

//Set these as applicable
const
  MAX_VERTICES = 500000;
  MAX_TRIANGLES = 1000000;
  DUPLICATE_TOLERANCE = 0.01;


//Created Triangles, vv# are the vertex pointers
type
  TDTriangle = record
    vv0: LongInt;
    vv1: LongInt;
    vv2: LongInt;
    PreCalc: Integer;
    xc,yc,r: Double;
  end;

  TDVertexArray = array[0..MAX_VERTICES] of TKMPointF;
  PVertexArray = ^TDVertexArray;

  TDTriangleArray = array[0..MAX_TRIANGLES] of TDTriangle;
  PTriangleArray = ^TDTriangleArray;

type
  TDelaunay = class
  private
    function InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
             var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;
    function WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
    function Triangulate(aVertCount: Integer): Integer;
    procedure QuickSort(var A: PVertexArray; Low,High: Integer);
  public
    PolyCount: Integer;
    VerticeCount: Integer; //Variable for total number of points (vertices)
    Vertex: PVertexArray;
    Triangle: PTriangleArray;
    constructor Create;
    destructor Destroy;
    procedure Mesh;
    procedure AddPoint(x,y: Integer);
  end;


implementation


type
  TDComplete = array [0..MAX_TRIANGLES] of Boolean;
  PComplete = ^TDComplete;

  TDEdges = array[0..1, 0..MAX_TRIANGLES * 3] of LongInt;
  PEdges = ^TDEdges;


constructor TDelaunay.Create;
begin
  //Initiate total points to 1, using base 0 causes problems in the functions
  VerticeCount := 1;
  PolyCount:=0;

  //Allocate memory for arrays
  GetMem(Vertex, sizeof(Vertex^));
  GetMem(Triangle, sizeof(Triangle^));
end;

destructor TDelaunay.Destroy;
begin
  //Free memory for arrays
  FreeMem(Vertex, sizeof(Vertex^));
  FreeMem(Triangle, sizeof(Triangle^));
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
  InCircle := False;

  //Check if xc,yc and r have already been calculated
  if  Triangle^[j].PreCalc=1 then
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

  If (Abs(y1 - y2) < eps) And (Abs(y2 - y3) < eps) Then
    Assert(False, 'INCIRCUM - F - Points are coincident !!');

  If Abs(y2 - y1) < eps Then
  begin
  m2 := -(x3 - x2) / (y3 - y2);
  mx2 := (x2 + x3) / 2;
  my2 := (y2 + y3) / 2;
  xc := (x2 + x1) / 2;
  yc := m2 * (xc - mx2) + my2;
  end
  Else If Abs(y3 - y2) < eps Then
  begin
  m1 := -(x2 - x1) / (y2 - y1);
  mx1 := (x1 + x2) / 2;
  my1 := (y1 + y2) / 2;
  xc := (x3 + x2) / 2;
  yc := m1 * (xc - mx1) + my1;
  end
  Else
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

  If drsqr <= rsqr Then InCircle := True;

end;



Function TDelaunay.WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
//Determines which side of a line the point (xp,yp) lies.
//The line goes from (x1,y1) to (x2,y2)
//Returns -1 for a point to the left
//         0 for a point on the line
//        +1 for a point to the right
var
 equation: Double;
begin
  equation := ((yp - y1) * (x2 - x1)) - ((y2 - y1) * (xp - x1));

  If equation > 0 Then
     WhichSide := -1
  Else If equation = 0 Then
     WhichSide := 0
  Else
     WhichSide := 1;
End;



Function TDelaunay.Triangulate(aVertCount: Integer): Integer;
//Takes as input NVERT vertices in arrays Vertex()
//Returned is a list of NTRI triangular faces in the array
//Triangle(). These triangles are arranged in clockwise order.
var

  Complete: PComplete;
  Edges: PEdges;
  Nedge: LongInt;

  //For Super Triangle
  xmin: Double;
  xmax: Double;
  ymin: Double;
  ymax: Double;
  xmid: Double;
  ymid: Double;
  dx: Double;
  dy: Double;
  dmax: Double;

  //General Variables
  VertID : Integer;
  I: Integer;
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

  //Find the maximum and minimum vertex bounds.
  //This is to allow calculation of the bounding triangle
  xmin := Vertex^[0].x;
  ymin := Vertex^[0].y;
  xmax := xmin;
  ymax := ymin;
  for VertID := 1 To aVertCount - 1 do
  begin
    If Vertex^[VertID].x < xmin Then xmin := Vertex^[VertID].x;
    If Vertex^[VertID].x > xmax Then xmax := Vertex^[VertID].x;
    If Vertex^[VertID].y < ymin Then ymin := Vertex^[VertID].y;
    If Vertex^[VertID].y > ymax Then ymax := Vertex^[VertID].y;
  end;

  dx := xmax - xmin;
  dy := ymax - ymin;
  if dx > dy then
    dmax := dx
  else
    dmax := dy;

  xmid := Trunc((xmax + xmin) / 2);
  ymid := Trunc((ymax + ymin) / 2);

  //Set up the supertriangle
  //This is a triangle which encompasses all the sample points.
  //The supertriangle coordinates are added to the end of the
  //vertex list. The supertriangle is the first triangle in
  //the triangle list.

  Vertex^[aVertCount + 0].x := (xmid - 2 * dmax);
  Vertex^[aVertCount + 0].y := (ymid - dmax);
  Vertex^[aVertCount + 1].x := xmid;
  Vertex^[aVertCount + 1].y := (ymid + 2 * dmax);
  Vertex^[aVertCount + 2].x := (xmid + 2 * dmax);
  Vertex^[aVertCount + 2].y := (ymid - dmax);
  Triangle^[0].vv0 := aVertCount + 0;
  Triangle^[0].vv1 := aVertCount + 1;
  Triangle^[0].vv2 := aVertCount + 2;
  Triangle^[0].Precalc := 0;

  Complete^[0] := False;
  oPolyCount := 0;

  //Include each point one at a time into the existing mesh
  for VertID := 0 to aVertCount - 1 do
  begin
    Nedge := 0;
    //Set up the edge buffer.
    //If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    //three edges of that triangle are added to the edge buffer.
    J := -1;
    repeat
      J := J + 1;
      if not Complete^[J] then
      begin
        InCir := InCircle(Vertex^[VertID].x, Vertex^[VertID].y, Vertex^[Triangle^[J].vv0].x,
                          Vertex^[Triangle^[J].vv0].y, Vertex^[Triangle^[J].vv1].x,
                          Vertex^[Triangle^[J].vv1].y, Vertex^[Triangle^[J].vv2].x,
                          Vertex^[Triangle^[J].vv2].y, xc, yc, r, J);
        //Include this if points are sorted by X
        if (xc + r) < Vertex[VertID].x then  //
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
            Triangle^[J].vv0 := Triangle^[oPolyCount].vv0;
            Triangle^[J].vv1 := Triangle^[oPolyCount].vv1;
            Triangle^[J].vv2 := Triangle^[oPolyCount].vv2;
            Triangle^[J].PreCalc:=Triangle^[oPolyCount].PreCalc;
            Triangle^[J].xc:=Triangle^[oPolyCount].xc;
            Triangle^[J].yc:=Triangle^[oPolyCount].yc;
            Triangle^[J].r:=Triangle^[oPolyCount].r;
            Triangle^[oPolyCount].PreCalc:=0;
            Complete^[J] := Complete^[oPolyCount];
            J := J - 1;
            oPolyCount := oPolyCount - 1;
          end;
      end;
    until(J >= oPolyCount);

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    for J := 0 to Nedge - 1 do
    if not (Edges^[0, J] = 0) and not (Edges^[1, J] = 0) then
    for K := J + 1 to Nedge - 1 do
    if not (Edges^[0, K] = 0) and not (Edges^[1, K] = 0) then
    if (Edges^[0, J] = Edges^[1, K]) and (Edges^[1, J] = Edges^[0, K]) then
    begin
      Edges^[0, J] := 0;
      Edges^[1, J] := 0;
      Edges^[0, K] := 0;
      Edges^[1, K] := 0;
    end;

    //  Form new triangles for the current point
    //  Skipping over any tagged edges.
    //  All edges are arranged in clockwise order.
    for J := 0 To Nedge - 1 do
    if not (Edges^[0, J] = 0) and not (Edges^[1, J] = 0) then
    begin
      oPolyCount := oPolyCount + 1;
      Triangle^[oPolyCount].vv0 := Edges^[0, J];
      Triangle^[oPolyCount].vv1 := Edges^[1, J];
      Triangle^[oPolyCount].vv2 := VertID;
      Triangle^[oPolyCount].PreCalc := 0;
      Complete^[oPolyCount] := False;
    end;
  end;

  //Remove triangles with supertriangle vertices
  //These are triangles which have a vertex number greater than NVERT
  I:= -1;
  repeat
    I := I + 1;
    if (Triangle^[I].vv0 > aVertCount - 1)
    or (Triangle^[I].vv1 > aVertCount - 1)
    or (Triangle^[I].vv2 > aVertCount - 1) then
    begin
      Triangle^[I].vv0 := Triangle^[oPolyCount].vv0;
      Triangle^[I].vv1 := Triangle^[oPolyCount].vv1;
      Triangle^[I].vv2 := Triangle^[oPolyCount].vv2;
      I := I - 1;
      oPolyCount := oPolyCount - 1;
    end;
  until(I >= oPolyCount);

  Result := oPolyCount;

  //Free memory
  FreeMem(Complete, sizeof(Complete^));
  FreeMem(Edges, sizeof(Edges^));
End;


procedure TDelaunay.Mesh;
begin
  QuickSort(Vertex, 0, VerticeCount - 1);
  if VerticeCount > 2 then
    PolyCount := Triangulate(VerticeCount); //Returns number of triangles created
end;


procedure TDelaunay.AddPoint(x,y: Integer);
var
  I: Integer;
begin
  //Skip duplicate points
  for I := 0 to VerticeCount - 1 do
  if (Abs(x-Vertex^[i].x) < DUPLICATE_TOLERANCE) and
     (Abs(y-Vertex^[i].y) < DUPLICATE_TOLERANCE) then
    Exit;

  Vertex^[VerticeCount].x := x;
  Vertex^[VerticeCount].y := y;
  //Increment the total number of points
  Inc(VerticeCount);
end;


procedure TDelaunay.QuickSort(var A: PVertexArray; Low,High: Integer);
//Sort all points by x
  procedure DoQuickSort(var A: PVertexArray; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid: Double;
    T: TKMPointF;
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
  DoQuickSort(A, Low, High);
end;



end.
