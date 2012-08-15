unit KM_PolySimplify;
interface
uses
  KM_Points;


type
  TKMNodesArray = record
    Count: Integer;
    Nodes: TKMPointArray;
  end;

  TKMShapesArray = record
    Count: Integer;
    Shape: array of TKMNodesArray;
  end;

  //Class to simplify shapes by removing points within aError
  TKMSimplifyShapes = class
  private
    fError: Single;
    fRect: TKMRect;

    fIn: TKMShapesArray;
    procedure Simplify(aErrorSqr: Single; const aInput: array of TKMPointI; var aKeep: array of Boolean; aFrom, aTo: Integer);
    function PolySimplify(const aInput: TKMPointArray; var aOutput: TKMPointArray): Integer;
  public
    constructor Create(aError: Single; aRect: TKMRect);
    procedure SimplifyShapes(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
  end;

//Simplify shapes by removing unnecessary points from straight lines
procedure SimplifyStraights(const aIn: TKMShapesArray; var aOut: TKMShapesArray);

procedure ForceOutlines(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);

procedure RemoveObstaclePolies(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);

//Remove anything that is outside bounds
procedure RemoveFrame(var aTriMesh: TKMTriMesh);

//Remove anything that is outside bounds
procedure RemoveDegenerates(var aTriMesh: TKMTriMesh);

implementation
uses KM_CommonTypes, KromUtils;


procedure SimplifyStraights(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
  procedure SimplifyStraights2(const aIn: TKMNodesArray; var aOut: TKMNodesArray);
  var K: Integer; P0, P1, P2: Integer;
  begin
    //Reserve space for worst case when nothing gets optimized
    SetLength(aOut.Nodes, aIn.Count);

    aOut.Count := 0;
    for K := 0 to aIn.Count - 1 do
    begin
      P0 := (K - 1 + aIn.Count) mod aIn.Count;
      P1 := K;
      P2 := (K + 1) mod aIn.Count;
      if ((aIn.Nodes[P0].X <> aIn.Nodes[P1].X) or (aIn.Nodes[P1].X <> aIn.Nodes[P2].X))
      and ((aIn.Nodes[P0].Y <> aIn.Nodes[P1].Y) or (aIn.Nodes[P1].Y <> aIn.Nodes[P2].Y)) then
      begin
        aOut.Nodes[aOut.Count] := aIn.Nodes[K];
        Inc(aOut.Count);
      end;
    end;

    //Trim to actual length
    SetLength(aOut.Nodes, aOut.Count);
  end;

var I: Integer;
begin
  SetLength(aOut.Shape, aIn.Count);

  for I := 0 to aIn.Count - 1 do
    SimplifyStraights2(aIn.Shape[I], aOut.Shape[I]);

  aOut.Count := aIn.Count;
end;


function VectorDiff(const A, B: TKMPointF): TKMPointF;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;


function DotProduct(const A, B: TKMPointF): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;


function DistanceSqr(const A, B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


constructor TKMSimplifyShapes.Create(aError: Single; aRect: TKMRect);
begin
  inherited Create;

  fError := aError;
  fRect := aRect;
end;

procedure TKMSimplifyShapes.Simplify(aErrorSqr: Single; const aInput: array of TKMPointI; var aKeep: array of Boolean; aFrom, aTo: Integer);
var
  I: Integer;
  MaxDistI: Integer;
  MaxDistSqr: Single;
  NodeDistSqr, TestDot, Tmp: Single;
  DistSqr: Single;
  Node1, Node2: TKMPointF;
  TestPos, NodeVect, TestVect: TKMPointF;
  TestP: TKMPointF;
begin
  //There is nothing to simplify
  if aTo <= aFrom + 1 then Exit;

  Node1 := KMPointF(aInput[aFrom]);
  Node2 := KMPointF(aInput[aTo mod Length(aInput)]);
  NodeVect := VectorDiff(Node2, Node1);
  NodeDistSqr := DistanceSqr(Node2, Node1);
  MaxDistI := 0;
  MaxDistSqr := 0;

  //Check all points and pick farthest away
  for I := aFrom + 1 to aTo - 1 do
  begin
    TestP := KMPointF(aInput[I]);

    TestVect := VectorDiff(TestP, Node1);
    TestDot := DotProduct(TestVect, NodeVect);

    //Calculate distance to segment
    if TestDot <= 0 then
      DistSqr := DistanceSqr(TestP, Node1)
    else
    if TestDot >= NodeDistSqr then
      DistSqr := DistanceSqr(TestP, Node2)
    else
    begin
      if NodeDistSqr <> 0 then
        Tmp := TestDot / NodeDistSqr
      else
        Tmp := 0;
      TestPos.X := Node1.X + Tmp * NodeVect.X;
      TestPos.Y := Node1.Y + Tmp * NodeVect.Y;
      DistSqr := DistanceSqr(TestP, TestPos);
    end;

    //Pick farthest point
    if DistSqr > MaxDistSqr then
    begin
      MaxDistI  := I;
      MaxDistSqr := DistSqr;
    end;
  end;

  //See if we need to split once again due to Error or too long span
  if (MaxDistSqr > aErrorSqr) or (aTo - aFrom > 12) then
  begin
    aKeep[MaxDistI] := True;

    Simplify(aErrorSqr, aInput, aKeep, aFrom, MaxDistI);
    Simplify(aErrorSqr, aInput, aKeep, MaxDistI, aTo);
  end;
end;


//Based on Douglas-Peucker algorithm for polyline simplification
//  aError - max allowed distance between resulting line and removed points
function TKMSimplifyShapes.PolySimplify(const aInput: TKMPointArray; var aOutput: TKMPointArray): Integer;
const MAX_TRIES = 5;
var
  I, K: Integer;
  KeptCount: Integer;
  KeepMiddle: Boolean;
  NCount: Integer;
  Prev: Integer;
  Keep: array of Boolean;
  Kept: array of Integer;
  Loops: Byte;
  Err: Single;
begin
  Result := 0;
  Assert(Length(aInput) > 3, 'There''s nothing to simplify?');

  NCount := Length(aInput);
  //NCount+1 because we duplicate last point to let algo work on 2 polylines
  SetLength(Keep, NCount+1);
  for I := 0 to NCount do
    Keep[I] := False;

  //We split loop in half and simplify both segments independently as two convex
  //lines. That is because algo is aimed at polyline, not polyloop
  Keep[0] := True;
  Keep[NCount] := True;
  KeepMiddle := True;

  //Keep nodes on edges
  for I := 0 to NCount - 1 do
  if (aInput[I].X = fRect.Left) or (aInput[I].Y = fRect.Top)
  or (aInput[I].X = fRect.Right) or (aInput[I].Y = fRect.Bottom) then
  begin
    Keep[I] := True;
    KeepMiddle := False;
  end;

  if KeepMiddle then
    Keep[NCount div 2] := True;

  //Try hard to keep at leaft 4 points in the outline (first and last are the same, hence 4, not 3 for convex shape)
  //With each pass we decrease allowed Error so that algo will keep more and more points to final of all points
  Loops := 0;
  repeat
      Result := 0;
      if (1 - MAX_TRIES * Loops) <> 0 then
        Err := Sqr(fError) / (1 - MAX_TRIES * Loops)
      else
        Err := 0;

      Prev := 0;
      for I := 1 to NCount do
      if Keep[I] then
      begin
        //We use Sqr values for all comparisons for speedup
        Simplify(Err, aInput, Keep, Prev, I);
        Prev := I;
      end;

      //Check if there are self-intersections by assembling aligned array and testing it
      KeptCount := 0;
      SetLength(Kept, NCount+1);
      for I := 0 to NCount do
      if Keep[I] then
      begin
        Kept[KeptCount] := I;
        Inc(KeptCount);
      end;

      for I := 0 to KeptCount - 2 do
      for K := I + 2 to KeptCount - 2 do
      if SegmentsIntersect(aInput[Kept[I]].X, aInput[Kept[I]].Y, aInput[Kept[I+1] mod NCount].X, aInput[Kept[I+1] mod NCount].Y,
                           aInput[Kept[K]].X, aInput[Kept[K]].Y, aInput[Kept[K+1] mod NCount].X, aInput[Kept[K+1] mod NCount].Y) then
      begin
        //If segments intersect we simplify them both with smaller Error
        Simplify(Err/2, aInput, Keep, Kept[I], Kept[I+1]);
        Simplify(Err/2, aInput, Keep, Kept[K], Kept[K+1]);
      end;

      //Fill resulting array with preserved points
      for I := 0 to NCount - 1 do
      if Keep[I] then
      begin
        aOutput[Result] := aInput[I];
        Inc(Result);
      end;

    Inc(Loops);
  until(Result > 3) or (Loops > MAX_TRIES);
end;


procedure TKMSimplifyShapes.SimplifyShapes(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
var I: Integer;
begin
  SetLength(aOut.Shape, aIn.Count);

  for I := 0 to aIn.Count - 1 do
  begin
    //Reserve space for worst case when all points are kept
    SetLength(aOut.Shape[I].Nodes, aIn.Shape[I].Count);
    aOut.Shape[I].Count := PolySimplify(aIn.Shape[I].Nodes, aOut.Shape[I].Nodes);
  end;
  aOut.Count := aIn.Count;
end;


procedure ForceEdge(var aTriMesh: TKMTriMesh; X1,Y1,X2,Y2: Integer);
var
  Edges: array [0..1] of array of SmallInt;
  Loop: array of Word;
  LoopCount: Integer;
  Nedge: LongInt;

  procedure AssembleLoop(aStart, aEnd: Word);
  var I, H: Integer;
  begin
    Loop[0] := aStart;
    LoopCount := 1;
    I := 0;
    repeat
      for H := 0 to Nedge - 1 do
      if (Edges[0, H] = Loop[LoopCount - 1]) then
      begin
        Loop[LoopCount] := Edges[1, H];
        Inc(LoopCount);
        Break; //We break to check = aEnd condition (otherwise we could skip it)
      end;
      Assert(LoopCount <= Nedge, 'End is missing?');
      Inc(I);
      Assert(I <= 1000, 'End is missing2?');
    until(Loop[LoopCount - 1] = aEnd);
  end;

  procedure TriangulateLoop;
  var L: Integer; V: TKMPointArray; PCount: Integer; Pols: array of Word; Res: Boolean;
  begin
    SetLength(V, LoopCount);
    SetLength(Pols, (LoopCount - 2) * 3);
    for L := 0 to LoopCount - 1 do
      V[L] := aTriMesh.Vertices[Loop[L]];

    Res := KMTriangulate(LoopCount, V, PCount, Pols);
    Assert(Res, 'Triangulation failed');

    for L := 0 to PCount - 1 do
    begin
      SetLength(aTriMesh.Polygons, Length(aTriMesh.Polygons) + 1);
      aTriMesh.Polygons[High(aTriMesh.Polygons),0] := Loop[Pols[L*3+0]];
      aTriMesh.Polygons[High(aTriMesh.Polygons),1] := Loop[Pols[L*3+1]];
      aTriMesh.Polygons[High(aTriMesh.Polygons),2] := Loop[Pols[L*3+2]];
    end;
  end;

var
  I, K: Integer;
  Vertice1, Vertice2: Integer;
  Intersect: Boolean;
begin
  with aTriMesh do
  begin
    Vertice1 := -1;
    Vertice2 := -1;
    //Find vertices
    for I := 0 to High(Vertices) do
    begin
      if (x1 = Vertices[I].x) and (y1 = Vertices[I].y) then
        Vertice1 := I;
      if (x2 = Vertices[I].x) and (y2 = Vertices[I].y) then
        Vertice2 := I;
      if (Vertice1 <> -1) and (Vertice2 <> -1) then
        Break;
    end;

    Assert((Vertice1 <> -1) and (Vertice2 <> -1), 'Vertices could not be found?');

    //Exit early if that edge exists
    for I := 0 to High(Polygons) do
    if ((Vertice1 = Polygons[I,0]) and (Vertice2 = Polygons[I,1]))
    or ((Vertice1 = Polygons[I,1]) and (Vertice2 = Polygons[I,2]))
    or ((Vertice1 = Polygons[I,2]) and (Vertice2 = Polygons[I,0])) then
      Exit;

    //How many edges we could possible need?
    SetLength(Edges[0], 1000);
    SetLength(Edges[1], 1000);

    //Find triangles we cross
    I := 0;
    Nedge := 0;
    repeat
      //Test each Polygons for intersection with the Edge

      //Eeach test checks if Edge and Polygons edge intersect
      Intersect :=
           SegmentsIntersect(x1, y1, x2, y2, aTriMesh.Vertices[aTriMesh.Polygons[I,0]].X, aTriMesh.Vertices[aTriMesh.Polygons[I,0]].Y, aTriMesh.Vertices[Polygons[I,1]].X,  Vertices[Polygons[I,1]].Y)
        or SegmentsIntersect(x1, y1, x2, y2, aTriMesh.Vertices[aTriMesh.Polygons[I,1]].X, aTriMesh.Vertices[aTriMesh.Polygons[I,1]].Y, aTriMesh.Vertices[Polygons[I,2]].X,  Vertices[Polygons[I,2]].Y)
        or SegmentsIntersect(x1, y1, x2, y2, aTriMesh.Vertices[aTriMesh.Polygons[I,2]].X, aTriMesh.Vertices[aTriMesh.Polygons[I,2]].Y, aTriMesh.Vertices[Polygons[I,0]].X,  Vertices[Polygons[I,0]].Y);

      //Cut the Polygons
      if Intersect then
      begin
        //Save triangles edges
        Edges[0, Nedge + 0] := aTriMesh.Polygons[I,0];
        Edges[1, Nedge + 0] := aTriMesh.Polygons[I,1];
        Edges[0, Nedge + 1] := aTriMesh.Polygons[I,1];
        Edges[1, Nedge + 1] := aTriMesh.Polygons[I,2];
        Edges[0, Nedge + 2] := aTriMesh.Polygons[I,2];
        Edges[1, Nedge + 2] := aTriMesh.Polygons[I,0];
        Nedge := Nedge + 3;
        //Move last Polygons to I
        Polygons[I,0] := Polygons[High(Polygons),0];
        Polygons[I,1] := Polygons[High(Polygons),1];
        Polygons[I,2] := Polygons[High(Polygons),2];
        Dec(I);
        SetLength(Polygons, Length(Polygons) - 1);
        Assert(Length(Polygons) > 0, '<0?');
      end;

      Inc(I);
    until (I >= Length(Polygons));

    //Remove duplicate edges and leave only outline
    for I := 0 to Nedge - 1 do
    if (Edges[0, I] > -1) and (Edges[1, I] > -1) then
    for K := I + 1 to Nedge - 1 do
    if (Edges[0, K] > -1) and (Edges[1, K] > -1) then
    if (Edges[0, I] = Edges[1, K]) and (Edges[1, I] = Edges[0, K]) then
    begin
      Edges[0, I] := -Edges[0, I];
      Edges[1, I] := -Edges[1, I];
      Edges[0, K] := -Edges[0, K];
      Edges[1, K] := -Edges[1, K];
    end;

    //Assemble two polygons on Edge sides
    if Nedge > 0 then
    begin
      SetLength(Loop, Nedge*2);
      AssembleLoop(Vertice1, Vertice2);
      TriangulateLoop;
      AssembleLoop(Vertice2, Vertice1);
      TriangulateLoop;
    end;
  end;
end;


procedure ForceOutlines(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);
var
  I,K: Integer;
begin
  for I := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[I] do
      for K := 0 to Count - 1 do
        ForceEdge(aTriMesh, Nodes[K].X, Nodes[K].Y, Nodes[(K + 1) mod Count].X, Nodes[(K + 1) mod Count].Y);
end;


procedure RemoveObstacle(var aTriMesh: TKMTriMesh; aNodes: TKMPointArray);
var
  I, K, L, M: Integer;
  VCount: Integer;
  Indexes: array of Integer;
  B: Boolean;
begin
  with aTriMesh do
  begin
    VCount := Length(aNodes);
    SetLength(Indexes, VCount);

    //Find Indexes
    for I := 0 to High(Vertices) do
    for K := 0 to VCount - 1 do
    if (aNodes[K].X = Vertices[I].x) and (aNodes[K].Y = Vertices[I].y) then
      Indexes[K] := I;

    //Find Indexes
    I := 0;
    repeat
      B := True;
      for K := 0 to VCount - 1 do
      if B and (Indexes[K] = Polygons[I,0]) then
        for L := K+1 to K+VCount - 2 do
        if B and (Indexes[L mod VCount] = Polygons[I,1]) then
          for M := L+1 to K+VCount - 1 do
          if B and (Indexes[M mod VCount] = Polygons[I,2]) then
          //Cut the triangle
          begin
            //Move last triangle to I
            Polygons[I,0] := Polygons[High(Polygons),0];
            Polygons[I,1] := Polygons[High(Polygons),1];
            Polygons[I,2] := Polygons[High(Polygons),2];
            Dec(I);
            SetLength(Polygons, Length(Polygons) - 1);
            B := False;
          end;
      Inc(I);
    until(I >= Length(Polygons));

    //Delete tris that lie on the outlines edge (direction is important)
    I := 0;
    repeat
      for K := 0 to VCount - 1 do
      if (Indexes[K] = Polygons[I,0]) and (Indexes[(K+1) mod VCount] = Polygons[I,1])
      or (Indexes[K] = Polygons[I,1]) and (Indexes[(K+1) mod VCount] = Polygons[I,2])
      or (Indexes[K] = Polygons[I,2]) and (Indexes[(K+1) mod VCount] = Polygons[I,0]) then
      //Cut the triangle
      begin
        //Move last triangle to I
        Polygons[I,0] := Polygons[High(Polygons),0];
        Polygons[I,1] := Polygons[High(Polygons),1];
        Polygons[I,2] := Polygons[High(Polygons),2];
        Dec(I);
        SetLength(Polygons, Length(Polygons) - 1);
        Break;
      end;
      Inc(I);
    until(I >= Length(Polygons));
  end;
end;


procedure RemoveObstaclePolies(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);
var
  I: Integer;
begin
  for I := 0 to fSimpleOutlines.Count - 1 do
  with fSimpleOutlines.Shape[I] do
    RemoveObstacle(aTriMesh, Nodes);
end;


//Remove anything that is outside bounds
procedure RemoveFrame(var aTriMesh: TKMTriMesh);
var I: Integer;
begin
  I := 0;
  with aTriMesh do
  repeat
    if (Polygons[I,0] < 4)
    or (Polygons[I,1] < 4)
    or (Polygons[I,2] < 4) then
    //Cut the triangle
    begin
      //Move last triangle to I
      Polygons[I,0] := Polygons[High(Polygons),0];
      Polygons[I,1] := Polygons[High(Polygons),1];
      Polygons[I,2] := Polygons[High(Polygons),2];
      Dec(I);
      SetLength(Polygons, Length(Polygons) - 1);
    end;
    Inc(I);
  until(I >= Length(Polygons));
end;


//Remove anything that is outside bounds
procedure RemoveDegenerates(var aTriMesh: TKMTriMesh);
var I: Integer;
begin
  I := 0;
  with aTriMesh do
  repeat
    if (Polygons[I,0] = Polygons[I,1])
    or (Polygons[I,1] = Polygons[I,2])
    or (Polygons[I,2] = Polygons[I,0]) then
    //Cut the triangle
    begin
      //Move last triangle to I
      Polygons[I,0] := Polygons[High(Polygons),0];
      Polygons[I,1] := Polygons[High(Polygons),1];
      Polygons[I,2] := Polygons[High(Polygons),2];
      Dec(I);
      SetLength(Polygons, Length(Polygons) - 1);
    end;
    Inc(I);
  until(I >= Length(Polygons));
end;


end.
