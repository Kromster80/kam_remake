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

uses Dialogs, Graphics, Forms, Types;

//Set these as applicable
Const MaxVertices = 500000;
Const MaxTriangles = 1000000;
Const ExPtTolerance = 0.000001;

//Points (Vertices)
Type dVertex = record
    x: Double;
    y: Double;
end;

//Created Triangles, vv# are the vertex pointers
Type dTriangle = record
    vv0: LongInt;
    vv1: LongInt;
    vv2: LongInt;
    PreCalc: Integer;
    xc,yc,r: Double;
end;

type TDVertex = array[0..MaxVertices] of dVertex;
type PVertex = ^TDVertex;

type TDTriangle = array[0..MaxTriangles] of dTriangle;
type PTriangle = ^TDTriangle;

type TDComplete = array [0..MaxTriangles] of Boolean;
type PComplete = ^TDComplete;

type TDEdges = array[0..2,0..MaxTriangles * 3] of LongInt;
type PEdges = ^TDEdges;

type
  TDelaunay = class
  private
    { Private declarations }
    Vertex: PVertex;
    Triangle: PTriangle;
    function InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
             var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;
    Function WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
    Function Triangulate(nvert: Integer): Integer;
  public
    { Public declarations }
    TempBuffer: TBitmap;
    HowMany: Integer;
    tPoints: Integer; //Variable for total number of points (vertices)
    TargetForm: TForm;
    constructor Create;
    destructor Destroy;
    procedure Mesh;
    procedure Draw;
    procedure AddPoint(x,y: Integer);
    procedure ClearBackPage;
    procedure FlipBackPage;
    procedure QuickSort(var A: PVertex; Low,High: Integer);
  end;

implementation

constructor TDelaunay.Create;
begin
  //Initiate total points to 1, using base 0 causes problems in the functions
  tPoints := 1;
  HowMany:=0;
  TempBuffer:=TBitmap.Create;

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
  begin
  ShowMessage('INCIRCUM - F - Points are coincident !!');
  Exit;
  end;

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



Function TDelaunay.Triangulate(nvert: Integer): Integer;
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
  i : Integer;
  j : Integer;
  k : Integer;
  ntri : Integer;
  xc : Double;
  yc : Double;
  r : Double;
  inc : Boolean;
begin

  //Allocate memory
  GetMem(Complete, sizeof(Complete^));
  GetMem(Edges, sizeof(Edges^));


//Find the maximum and minimum vertex bounds.
//This is to allow calculation of the bounding triangle
xmin := Vertex^[1].x;
ymin := Vertex^[1].y;
xmax := xmin;
ymax := ymin;
  For i := 2 To nvert do
  begin
  If Vertex^[i].x < xmin Then xmin := Vertex^[i].x;
  If Vertex^[i].x > xmax Then xmax := Vertex^[i].x;
  If Vertex^[i].y < ymin Then ymin := Vertex^[i].y;
  If Vertex^[i].y > ymax Then ymax := Vertex^[i].y;
  end;

dx := xmax - xmin;
dy := ymax - ymin;
If dx > dy Then
    dmax := dx
Else
    dmax := dy;

xmid := Trunc((xmax + xmin) / 2);
ymid := Trunc((ymax + ymin) / 2);

//Set up the supertriangle
//This is a triangle which encompasses all the sample points.
//The supertriangle coordinates are added to the end of the
//vertex list. The supertriangle is the first triangle in
//the triangle list.

Vertex^[nvert + 1].x := (xmid - 2 * dmax);
Vertex^[nvert + 1].y := (ymid - dmax);
Vertex^[nvert + 2].x := xmid;
Vertex^[nvert + 2].y := (ymid + 2 * dmax);
Vertex^[nvert + 3].x := (xmid + 2 * dmax);
Vertex^[nvert + 3].y := (ymid - dmax);
Triangle^[1].vv0 := nvert + 1;
Triangle^[1].vv1 := nvert + 2;
Triangle^[1].vv2 := nvert + 3;
Triangle^[1].Precalc := 0;

Complete[1] := False;
ntri := 1;

//Include each point one at a time into the existing mesh
For i := 1 To nvert do
begin
  Nedge := 0;
    //Set up the edge buffer.
    //If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    //three edges of that triangle are added to the edge buffer.
    j := 0;
      repeat
        j := j + 1;
        If Complete^[j] <> True Then
        begin
            inc := InCircle(Vertex^[i].x, Vertex^[i].y, Vertex^[Triangle^[j].vv0].x,
                            Vertex^[Triangle^[j].vv0].y, Vertex^[Triangle^[j].vv1].x,
                            Vertex^[Triangle^[j].vv1].y, Vertex^[Triangle^[j].vv2].x,
                            Vertex^[Triangle^[j].vv2].y, xc, yc, r,j);
            //Include this if points are sorted by X
            If (xc + r) < Vertex[i].x Then  //
               complete[j] := True          //
            Else                            //
            If inc Then
            begin
                Edges^[1, Nedge + 1] := Triangle^[j].vv0;
                Edges^[2, Nedge + 1] := Triangle^[j].vv1;
                Edges^[1, Nedge + 2] := Triangle^[j].vv1;
                Edges^[2, Nedge + 2] := Triangle^[j].vv2;
                Edges^[1, Nedge + 3] := Triangle^[j].vv2;
                Edges^[2, Nedge + 3] := Triangle^[j].vv0;
                Nedge := Nedge + 3;
                Triangle^[j].vv0 := Triangle^[ntri].vv0;
                Triangle^[j].vv1 := Triangle^[ntri].vv1;
                Triangle^[j].vv2 := Triangle^[ntri].vv2;
                Triangle^[j].PreCalc:=Triangle^[ntri].PreCalc;
                Triangle^[j].xc:=Triangle^[ntri].xc;
                Triangle^[j].yc:=Triangle^[ntri].yc;
                Triangle^[j].r:=Triangle^[ntri].r;
                Triangle^[ntri].PreCalc:=0;
                Complete^[j] := Complete^[ntri];
                j := j - 1;
                ntri := ntri - 1;
            End;
        End;
    until j>=ntri;

// Tag multiple edges
// Note: if all triangles are specified anticlockwise then all
// interior edges are opposite pointing in direction.
    For j := 1 To Nedge - 1 do
    begin
        If Not (Edges^[1, j] = 0) And Not (Edges^[2, j] = 0) Then
        begin
            For k := j + 1 To Nedge do
            begin
                If Not (Edges^[1, k] = 0) And Not (Edges^[2, k] = 0) Then
                begin
                    If Edges^[1, j] = Edges^[2, k] Then
                    begin
                        If Edges^[2, j] = Edges^[1, k] Then
                         begin
                            Edges^[1, j] := 0;
                            Edges^[2, j] := 0;
                            Edges^[1, k] := 0;
                            Edges^[2, k] := 0;
                         End;
                     End;
                End;
            end;
        End;
    end;

//  Form new triangles for the current point
//  Skipping over any tagged edges.
//  All edges are arranged in clockwise order.
    For j := 1 To Nedge do
    begin
            If Not (Edges^[1, j] = 0) And Not (Edges^[2, j] = 0) Then
            begin
                ntri := ntri + 1;
                Triangle^[ntri].vv0 := Edges^[1, j];
                Triangle^[ntri].vv1 := Edges^[2, j];
                Triangle^[ntri].vv2 := i;
                Triangle^[ntri].PreCalc:=0;
                Complete^[ntri] := False;
            End;
    end;
end;

//Remove triangles with supertriangle vertices
//These are triangles which have a vertex number greater than NVERT
i:= 0;
repeat
  i := i + 1;
  If (Triangle^[i].vv0 > nvert) Or (Triangle^[i].vv1 > nvert) Or (Triangle^[i].vv2 > nvert) Then
  begin
     Triangle^[i].vv0 := Triangle^[ntri].vv0;
     Triangle^[i].vv1 := Triangle^[ntri].vv1;
     Triangle^[i].vv2 := Triangle^[ntri].vv2;
     i := i - 1;
     ntri := ntri - 1;
  End;
until i>=ntri;

Triangulate := ntri;

  //Free memory
  FreeMem(Complete, sizeof(Complete^));
  FreeMem(Edges, sizeof(Edges^));
End;


procedure TDelaunay.Mesh;
begin
  QuickSort(Vertex,1,tPoints-1);
  If tPoints > 3 Then
  HowMany := Triangulate(tPoints-1); //'Returns number of triangles created.
end;



procedure TDelaunay.Draw;
var
  //variable to hold how many triangles are created by the triangulate function
  i: Integer;
begin
  // Clear the form canvas
  ClearBackPage;

  TempBuffer.Canvas.Brush.Color := clTeal;
  //Draw the created triangles
  if (HowMany > 0) then
  begin
    For i:= 1 To HowMany do
    begin
    TempBuffer.Canvas.Polygon([Point(Trunc(Vertex^[Triangle^[i].vv0].x), Trunc(Vertex^[Triangle^[i].vv0].y)),
                                  Point(Trunc(Vertex^[Triangle^[i].vv1].x), Trunc(Vertex^[Triangle^[i].vv1].y)),
                                  Point(Trunc(Vertex^[Triangle^[i].vv2].x), Trunc(Vertex^[Triangle^[i].vv2].y))]);
    end;
  end;

  FlipBackPage;
end;

procedure TDelaunay.AddPoint(x,y: Integer);
var
i, AE: Integer;
begin

  //Check for duplicate points
  AE:=0;
  i:=1;
  while i<tPoints do
  begin
  If (Abs(x-Vertex^[i].x) < ExPtTolerance) and
     (Abs(y-Vertex^[i].y) < ExPtTolerance) Then AE:=1;
  Inc(i);
  end;

  if AE=0 then
  begin
  //Set Vertex coordinates where you clicked the pic box
  Vertex^[tPoints].x := x;
  Vertex^[tPoints].y := y;
  //Increment the total number of points
  tPoints := tPoints + 1;
  end;

end;




procedure TDelaunay.ClearBackPage;
begin
  TempBuffer.Height:=TargetForm.Height;
  TempBuffer.Width:=TargetForm.Width;
  TempBuffer.Canvas.Brush.Color := clSilver;
  TempBuffer.Canvas.FillRect(Rect(0,0,TargetForm.Width,TargetForm.Height));
end;

procedure TDelaunay.FlipBackPage;
var
  ARect : TRect;
begin
  ARect := Rect(0,0,TargetForm.Width,TargetForm.Height);
  TargetForm.Canvas.CopyRect(ARect, TempBuffer.Canvas, ARect);
end;


procedure TDelaunay.QuickSort(var A: PVertex; Low,High: Integer);
//Sort all points by x
  procedure DoQuickSort(var A: PVertex; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid: Double;
    T: dVertex;
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
