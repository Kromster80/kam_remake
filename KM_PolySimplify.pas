unit KM_PolySimplify;
interface
uses
  KM_Points, Math;


type
  TKMNodesArray = record
    Count: Integer;
    Nodes: TKMPointArray;
  end;

  TKMShapesArray = record
    Count: Integer;
    Shape: array of TKMNodesArray;
  end;

  PKMShapesArray = ^TKMShapesArray;


  //Class to simplify shapes by removing points within aError
  TKMSimplifyShapes = class
  private
    fError: Single;
    fRect: TKMRect;
    fIn: TKMShapesArray;
    fOut: PKMShapesArray;
    fKeep: array of array of Boolean;
    procedure SetupKeepArray;
    procedure Simplify(aShape, aFrom, aTo: Integer; aErrorSqr: Single; aForceSplit: Boolean = False);
    procedure PolySimplify(aShape: Integer);
    procedure WriteOutput;
    procedure SetupOutputArray;
    procedure FixDegenerateShapes;
    procedure FixIntersectingShapes;
    procedure SimplifyShapes;
  public
    constructor Create(aError: Single; aRect: TKMRect);
    procedure Execute(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
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
uses KM_CommonTypes, KromUtils, KM_Triangulate;


constructor TKMSimplifyShapes.Create(aError: Single; aRect: TKMRect);
begin
  inherited Create;

  fError := aError;
  fRect := aRect;
end;

procedure TKMSimplifyShapes.Simplify(aShape, aFrom, aTo: Integer; aErrorSqr: Single; aForceSplit: Boolean = False);
var
  InLoop: TKMPointArray;
  I: Integer;
  MaxDistI: Integer;
  MaxDistSqr: Single;
  NodeDistSqr, TestDot, Tmp: Single;
  DistSqr: Single;
  Node1, Node2: TKMPointI;
  TestPos: TKMPointF;
  NodeVect, TestVect: TKMPointI;
  TestP: TKMPointI;
begin
  InLoop := fIn.Shape[aShape].Nodes;

  //There is nothing to simplify
  if aTo <= aFrom + 1 then Exit;

  Node1 := InLoop[aFrom];
  Node2 := InLoop[aTo mod Length(InLoop)];
  NodeVect := KMVectorDiff(Node2, Node1);
  NodeDistSqr := KMDistanceSqr(Node2, Node1);
  MaxDistI := 0;
  MaxDistSqr := -1;

  //Check all points and pick farthest away
  for I := aFrom + 1 to aTo - 1 do
  begin
    TestP := InLoop[I];

    TestVect := KMVectorDiff(TestP, Node1);
    TestDot := KMDotProduct(TestVect, NodeVect);

    //Calculate distance to segment
    if TestDot <= 0 then
      DistSqr := KMDistanceSqr(TestP, Node1)
    else
    if TestDot >= NodeDistSqr then
      DistSqr := KMDistanceSqr(TestP, Node2)
    else
    begin
      if NodeDistSqr <> 0 then
        Tmp := TestDot / NodeDistSqr
      else
        Tmp := 0;
      //TestPos is projected on to the line and thus needs FloatingPoint position
      TestPos.X := Node1.X + Tmp * NodeVect.X;
      TestPos.Y := Node1.Y + Tmp * NodeVect.Y;
      DistSqr := KMDistanceSqr(KMPointF(TestP), TestPos);
    end;

    //Add slightly more weight to the middle to allow to split straight lines
    DistSqr := DistSqr + Min(aTo - I, I - aFrom) / 100;

    //Pick farthest point
    if DistSqr > MaxDistSqr then
    begin
      MaxDistI := I;
      MaxDistSqr := DistSqr;
    end;
  end;

  //See if we need to split once again due to Error, too long span or forced
  //irregardless of cause - split by farthest point
  if (MaxDistSqr > aErrorSqr) or (KMLengthSqr(Node1, Node2) > Sqr(12)) or aForceSplit then
  begin
    fKeep[aShape, MaxDistI] := True;

    Simplify(aShape, aFrom, MaxDistI, aErrorSqr);
    Simplify(aShape, MaxDistI, aTo, aErrorSqr);
  end;
end;


//Based on Douglas-Peucker algorithm for polyline simplification
//  aError - max allowed distance between resulting line and removed points
procedure TKMSimplifyShapes.PolySimplify(aShape: Integer);
var
  InLoop: TKMPointArray;
  I: Integer;
  NCount: Integer;
  Prev: Integer;
begin
  InLoop := fIn.Shape[aShape].Nodes;
  NCount := fIn.Shape[aShape].Count;

  Prev := 0;
  for I := 1 to NCount do
  if fKeep[aShape,I] then
  begin
    //We use Sqr values for all comparisons for speedup
    Simplify(aShape, Prev, I, Sqr(fError));
    Prev := I;
  end;
end;


//Simplify all the shapes
procedure TKMSimplifyShapes.SimplifyShapes;
var
  I: Integer;
begin
  for I := 0 to fIn.Count - 1 do
  begin
    Assert(fIn.Shape[I].Count > 3, 'There''s nothing to simplify?');
    PolySimplify(I);
  end;
end;


//Fill resulting array with preserved points
procedure TKMSimplifyShapes.WriteOutput;
var
  I, K: Integer;
begin
  for I := 0 to fIn.Count - 1 do
  begin
    fOut.Shape[I].Count := 0;
    for K := 0 to fIn.Shape[I].Count - 1 do
    if fKeep[I,K] then
    begin
      fOut.Shape[I].Nodes[fOut.Shape[I].Count] := fIn.Shape[I].Nodes[K];
      Inc(fOut.Shape[I].Count);
    end;
  end;
end;


//Setup boolean array that tells algo which points to keep
procedure TKMSimplifyShapes.SetupKeepArray;
var
  I,K: Integer;
  KeepMiddle: Boolean;
begin
  SetLength(fKeep, fIn.Count);
  for I := 0 to fIn.Count - 1 do
  begin
    //NCount+1 because we duplicate last point to let algo work on 2 polylines
    SetLength(fKeep[I], fIn.Shape[I].Count + 1);
    for K := 0 to fIn.Shape[I].Count do
      fKeep[I,K] := False;
    fKeep[I,0] := True;
    fKeep[I,fIn.Shape[I].Count] := True;

    //We split loop in half and simplify both segments independently as two convex
    //lines. That is because algo is aimed at polyline, not polyloop
    KeepMiddle := True;

    //Keep nodes on edges
    for K := 0 to fIn.Shape[I].Count - 1 do
    if (fIn.Shape[I].Nodes[K].X = fRect.Left) or (fIn.Shape[I].Nodes[K].Y = fRect.Top)
    or (fIn.Shape[I].Nodes[K].X = fRect.Right) or (fIn.Shape[I].Nodes[K].Y = fRect.Bottom) then
    begin
      fKeep[I,K] := True;
      KeepMiddle := False;
    end;

    if KeepMiddle then
      fKeep[I, fIn.Shape[I].Count div 2] := True;
  end;
end;


//Check that shapes have at least 3 points
procedure TKMSimplifyShapes.FixDegenerateShapes;
var
  I, K: Integer;
  Prev: Integer;
begin
  for I := 0 to fIn.Count - 1 do
    if fOut.Shape[I].Count < 3 then
    begin
      Assert(fOut.Shape[I].Count = 2, 'Two points expected');
      Prev := 0;
      for K := 1 to fIn.Shape[I].Count do
        if fKeep[I, K] then
        begin
          //We use Sqr values for all comparisons for speedup
          Simplify(I, Prev, K, Sqr(fError), True);
          Prev := K;
        end;
    end;
end;


//Check shapes for intersections
procedure TKMSimplifyShapes.FixIntersectingShapes;
var
  IntCount: Integer;
  Ints: array of array [0..2] of Word;
  KeptCount: array of Integer;
  Kept: array of array of Integer;

  //Assemble aligned array
  procedure AssembleKeptReference;
  var
    I, K: Integer;
  begin
    SetLength(Kept, fIn.Count);
    SetLength(KeptCount, fIn.Count + 1);
    for I := 0 to fIn.Count - 1 do
    begin
      KeptCount[I] := 0;
      //We need to store last point to be able to simplify last segment
      SetLength(Kept[I], fIn.Shape[I].Count + 1);
      for K := 0 to fIn.Shape[I].Count do
        if fKeep[I, K] then
        begin
          Kept[I, KeptCount[I]] := K;
          Inc(KeptCount[I]);
        end;
    end;
  end;

  //Segments should not intersect or touch except for the start/end
  procedure CheckIntersect(L1, N1, N2, L2, N3, N4: Integer);
  var A,B,C,D: TKMPointI;
  begin
    A := fIn.Shape[L1].Nodes[N1 mod fIn.Shape[L1].Count];
    B := fIn.Shape[L1].Nodes[N2 mod fIn.Shape[L1].Count];
    C := fIn.Shape[L2].Nodes[N3 mod fIn.Shape[L2].Count];
    D := fIn.Shape[L2].Nodes[N4 mod fIn.Shape[L2].Count];

    if KMSamePoint(A, C) or KMSamePoint(A, D) or KMSamePoint(B, C) or KMSamePoint(B, D) then
      Exit;

    if KMSegmentsIntersectOrTouch(A, B, C, D) then
    begin
      //If outline intersects itself we split the longest segment
      if L1 = L2 then
        if (N2 - N1) > (N4 - N3) then
        begin
          SetLength(Ints, IntCount + 1);
          Ints[IntCount, 0] := L1;
          Ints[IntCount, 1] := N1;
          Ints[IntCount, 2] := N2;
          Inc(IntCount);
        end
        else
        begin
          SetLength(Ints, IntCount + 1);
          Ints[IntCount, 0] := L2;
          Ints[IntCount, 1] := N3;
          Ints[IntCount, 2] := N4;
          Inc(IntCount);
        end
      else
      //If segments belong to different lines we cant yet decide which split is better
      begin
        SetLength(Ints, IntCount + 1);
        Ints[IntCount, 0] := L1;
        Ints[IntCount, 1] := N1;
        Ints[IntCount, 2] := N2;
        Inc(IntCount);
        SetLength(Ints, IntCount + 1);
        Ints[IntCount, 0] := L2;
        Ints[IntCount, 1] := N3;
        Ints[IntCount, 2] := N4;
        Inc(IntCount);
      end;
    end;
  end;
  procedure WriteIntersections;
  var I, K, L, M: Integer;
  begin
    IntCount := 0;
    //Test self-intersections
    for I := 0 to fOut.Count - 1 do
      for K := 0 to KeptCount[I] - 2 do
        for M := K + 2 to KeptCount[I] - 2 do
          CheckIntersect(I, Kept[I,K], Kept[I,K+1], I, Kept[I,M], Kept[I,M+1]);

    //Test intersections with other outlines
    for I := 0 to fOut.Count - 1 do for K := 0 to KeptCount[I] - 2 do
      for L := I + 1 to fOut.Count - 1 do for M := 0 to KeptCount[L] - 2 do
        CheckIntersect(I, Kept[I,K], Kept[I,K+1], L, Kept[L,M], Kept[L,M+1]);

  end;
var
  I, LoopCount: Integer;
begin
  LoopCount := 0;
  repeat
    AssembleKeptReference;
    WriteIntersections;

    for I := 0 to IntCount - 1 do
      Simplify(Ints[I, 0],
               Ints[I, 1],
               Ints[I, 2],
               Sqr(fError), True);

    Inc(LoopCount);
    Assert(LoopCount <= 20, 'Can''t resolve intersections');
  until (IntCount = 0);
end;


procedure TKMSimplifyShapes.Execute(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
begin
  fIn := aIn;
  fOut := @aOut;

  SetupOutputArray;
  SetupKeepArray;

  SimplifyShapes;
  WriteOutput;

  FixDegenerateShapes;
  WriteOutput;

  FixIntersectingShapes;

  WriteOutput;
end;


procedure TKMSimplifyShapes.SetupOutputArray;
var
  I: Integer;
begin
  fOut.Count := fIn.Count;
  SetLength(fOut.Shape, fOut.Count);
  for I := 0 to fIn.Count - 1 do
    //Reserve space for worst case when all points are kept
    SetLength(fOut.Shape[I].Nodes, fIn.Shape[I].Count);
end;


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
      if (((aIn.Nodes[P0].X <> aIn.Nodes[P1].X) or (aIn.Nodes[P1].X <> aIn.Nodes[P2].X))
      and ((aIn.Nodes[P0].Y <> aIn.Nodes[P1].Y) or (aIn.Nodes[P1].Y <> aIn.Nodes[P2].Y)))
      or (K mod 11 = 6) //Keep some points inbetween
//todo: Don't keep points on Rect bounds
      then
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


procedure ForceEdge(var aTriMesh: TKMTriMesh; A,B: TKMPointI);
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
  var L: Integer; V: TKMPointArray; PCount: Word; Pols: array of Word;
  begin
    SetLength(V, LoopCount);
    SetLength(Pols, (LoopCount - 2) * 3);
    for L := 0 to LoopCount - 1 do
      V[L] := aTriMesh.Vertices[Loop[L]];

    //KMTriangulate(LoopCount, V, PCount, Pols);
    PolyTriangulate(V, LoopCount, Pols, PCount);

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
      if (A.X = Vertices[I].X) and (A.Y = Vertices[I].Y) then
        Vertice1 := I;
      if (B.X = Vertices[I].X) and (B.Y = Vertices[I].Y) then
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
           KMSegmentsIntersect(A, B, aTriMesh.Vertices[aTriMesh.Polygons[I,0]], aTriMesh.Vertices[Polygons[I,1]])
        or KMSegmentsIntersect(A, B, aTriMesh.Vertices[aTriMesh.Polygons[I,1]], aTriMesh.Vertices[Polygons[I,2]])
        or KMSegmentsIntersect(A, B, aTriMesh.Vertices[aTriMesh.Polygons[I,2]], aTriMesh.Vertices[Polygons[I,0]]);

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
      //Discard edges (but keep their value for debug. 0 becomes -1000)
      Edges[0, I] := -1000 - Edges[0, I];
      Edges[1, I] := -1000 - Edges[1, I];
      Edges[0, K] := -1000 - Edges[0, K];
      Edges[1, K] := -1000 - Edges[1, K];
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
  procedure CheckAllPolysFaceUp;
  var I: Integer;
  begin
    with aTriMesh do
    for I := 0 to High(Polygons) do
      Assert(KMNormal2Poly(Vertices[Polygons[I,0]], Vertices[Polygons[I,1]], Vertices[Polygons[I,2]]) >= 0);
  end;
var
  I, K: Integer;
begin
  for I := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[I] do
      for K := 0 to Count - 1 do
        ForceEdge(aTriMesh, Nodes[K], Nodes[(K + 1) mod Count]);
  CheckAllPolysFaceUp;
end;


procedure RemoveObstaclePolies(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);
type TPolyFill = (pfUnknown, pfKeep, pfRemove);
var
  Mark: array of TPolyFill;
  procedure RemoveObstacle(var aTriMesh: TKMTriMesh; aNodes: TKMNodesArray);
    procedure DoFlood(N1, N2: Integer);
    var I: Integer;
    begin
      with aTriMesh do
      for I := 0 to High(Polygons) do
      if Mark[I] = pfUnknown then
      if ((Polygons[I,0] = N1) and (Polygons[I,2] = N2))
      or ((Polygons[I,1] = N1) and (Polygons[I,0] = N2))
      or ((Polygons[I,2] = N1) and (Polygons[I,1] = N2)) then
      begin
        Mark[I] := pfRemove;
        DoFlood(Polygons[I,0], Polygons[I,1]);
        DoFlood(Polygons[I,1], Polygons[I,2]);
        DoFlood(Polygons[I,2], Polygons[I,0]);
      end;
    end;
  var
    I, K, L: Integer;
    Outline: array of Integer;
  begin
    with aTriMesh do
    begin
      SetLength(Outline, aNodes.Count);

      //Find Indexes
      for I := 0 to High(Vertices) do
      for K := 0 to aNodes.Count - 1 do
      if (aNodes.Nodes[K].X = Vertices[I].x) and (aNodes.Nodes[K].Y = Vertices[I].y) then
        Outline[K] := I;

      for I := 0 to High(Polygons) do
        Mark[I] := pfUnknown;

      //Create outline for obstacle with Keep/Remove polys
      for I := 0 to High(Polygons) do
      begin
        for K := 0 to aNodes.Count - 1 do
        if (Outline[K] = Polygons[I,0]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,1])
        or (Outline[K] = Polygons[I,1]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,2])
        or (Outline[K] = Polygons[I,2]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,0]) then
          Mark[I] := pfRemove;

        for K := 0 to aNodes.Count - 1 do
        if (Outline[K] = Polygons[I,0]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,2])
        or (Outline[K] = Polygons[I,1]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,0])
        or (Outline[K] = Polygons[I,2]) and (Outline[(K+1) mod aNodes.Count] = Polygons[I,1]) then
          Mark[I] := pfKeep;
      end;

      for I := 0 to High(Polygons) do
      if Mark[I] = pfRemove then
      begin
        DoFlood(Polygons[I,0], Polygons[I,1]);
        DoFlood(Polygons[I,1], Polygons[I,2]);
        DoFlood(Polygons[I,2], Polygons[I,0]);
      end;

      //Cut the triangles
      I := 0;
      repeat
        if Mark[I] = pfRemove then
        begin
          //Move last triangle to I
          Polygons[I,0] := Polygons[High(Polygons),0];
          Polygons[I,1] := Polygons[High(Polygons),1];
          Polygons[I,2] := Polygons[High(Polygons),2];
          Mark[I] := Mark[High(Polygons)];
          Dec(I);
          SetLength(Polygons, Length(Polygons) - 1);
        end;
        Inc(I);
      until(I >= Length(Polygons));
    end;
  end;
var
  I: Integer;
begin
  SetLength(Mark, Length(aTriMesh.Polygons));
  for I := 0 to fSimpleOutlines.Count - 1 do
    RemoveObstacle(aTriMesh, fSimpleOutlines.Shape[I]);
end;


//Remove anything that is outside bounds
procedure RemoveFrame(var aTriMesh: TKMTriMesh);
var I: Integer;
begin
  I := 0;
  with aTriMesh do
  repeat
    //We take advantage of the fact that
    //first 4 points were added to make the Frame
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
