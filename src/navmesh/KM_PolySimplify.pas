unit KM_PolySimplify;
{$I KaM_Remake.inc}
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

  PKMShapesArray = ^TKMShapesArray;

  TKMEdgesArray = record
    Count: Word;
    Nodes: array of array [0..1] of SmallInt; //Allow for negative values to mark odd egdes
  end;

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
procedure SimplifyStraights(const aIn: TKMShapesArray; aRect: TKMRect; var aOut: TKMShapesArray);

procedure ForceOutlines(var aTriMesh: TKMTriMesh; aRect: TKMRect; fSimpleOutlines: TKMShapesArray);

procedure RemoveObstaclePolies(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);

//Remove anything that is outside bounds
procedure RemoveFrame(var aTriMesh: TKMTriMesh);

//Remove anything that is outside bounds
procedure CheckForDegenerates(var aTriMesh: TKMTriMesh);


implementation
uses
  SysUtils, Math,
  PolyTriangulate;


const
  MAX_STRAIGHT_SPAN = 7; //Keep just every Nth point on nonedge  straightsin raw outlines
  MAX_SIMPLIFIED_SPAN = 12; //Allow max N span on simplified outline


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
  Node1, Node2: TKMPoint;
  TestPos: TKMPointF;
  NodeVect, TestVect: TKMPoint;
  TestP: TKMPoint;
begin
  InLoop := fIn.Shape[aShape].Nodes;

  //There is nothing to simplify
  if aTo <= aFrom + 1 then Exit;

  Node1 := InLoop[aFrom];
  Node2 := InLoop[aTo mod Length(InLoop)];
  NodeVect := KMPointSubtract(Node2, Node1);
  NodeDistSqr := KMDistanceSqr(Node2, Node1);
  MaxDistI := 0;
  MaxDistSqr := -1;

  //Check all points and pick farthest away
  for I := aFrom + 1 to aTo - 1 do
  begin
    TestP := InLoop[I];

    TestVect := KMPointSubtract(TestP, Node1);
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
  if (MaxDistSqr > aErrorSqr) or (KMLengthSqr(Node1, Node2) > Sqr(MAX_SIMPLIFIED_SPAN)) or aForceSplit then
  begin
    fKeep[aShape, MaxDistI] := True;

    Simplify(aShape, aFrom, MaxDistI, aErrorSqr);
    Simplify(aShape, MaxDistI, aTo, aErrorSqr);
  end;
end;


//Simplify all the shapes
//Based on Douglas-Peucker algorithm for polyline simplification
//  aError - max allowed distance between resulting line and removed points
procedure TKMSimplifyShapes.SimplifyShapes;
var
  I, K: Integer;
  Prev: Integer;
begin
  for I := 0 to fIn.Count - 1 do
  begin
    Assert(fIn.Shape[I].Count > 3, 'There''s nothing to simplify?');
    Prev := 0;
    for K := 1 to fIn.Shape[I].Count do
    if fKeep[I,K] then
    begin
      //We use Sqr values for all comparisons for speedup
      Simplify(I, Prev, K, Sqr(fError));
      Prev := K;
    end;
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
  I,K,L: Integer;
  KeepMiddle: Boolean;
  Corners: array of ShortInt;
  N0, N1, N2: Integer;
  FirstCorner: Integer;
  TmpNode: TKMPoint;
  TmpCorn: ShortInt;
  Count1, Count2: Integer;
  Best: Single;
  BestI: Integer;
begin
  SetLength(fKeep, fIn.Count);

  for I := 0 to fIn.Count - 1 do
  with fIn.Shape[I] do
  begin
    //Find corner nodes
    SetLength(Corners, Count);
    for K := 0 to Count - 1 do
    begin
      N0 := (K + Count - 1) mod Count;
      N1 := K;
      N2 := (K + 1) mod Count;
      Corners[K] := Sign(KMNormal2Poly(Nodes[N0], Nodes[N1], Nodes[N2]));
    end;

    //Check corner count and invert signs if we are dealing with CCW instead of CW
    //(yes, that can happen if obstacle gets parsed from opposite side)
    Count1 := 0;
    Count2 := 0;
    for K := 0 to Count - 1 do
    begin
      Count1 := Count1 + Byte(Corners[K] > 0);
      Count2 := Count2 + Byte(Corners[K] < 0);
    end;
    Assert(Abs(Count1 - Count2) = 4);
    if Count2 > Count1 then
    for K := 0 to Count - 1 do
      Corners[K] := -Corners[K];

    FirstCorner := -1;
    for K := 0 to Count - 1 do
    begin
      N0 := (K + Count - 1) mod Count;
      N1 := K;
      N2 := (K + 1) mod Count;
      if (Corners[N1] = 1) and ((Corners[N0] >= 0) or (Corners[N2] >= 0)) then
      begin
        FirstCorner := K;
        Break;
      end;
    end;

    if FirstCorner = -1 then
      raise Exception.Create('Could not find corners?');

    //Shift start to Corner node
    if FirstCorner > 0 then
    begin
      //Do that many 1-item shifts
      for L := 0 to FirstCorner - 1 do
      begin
        TmpCorn := Corners[0];
        TmpNode := Nodes[0];
        for K := 0 to Count - 2 do
        begin
          Corners[K] := Corners[K+1];
          Nodes[K] := Nodes[K+1];
        end;
        Corners[Count - 1] := TmpCorn;
        Nodes[Count - 1] := TmpNode;
      end;
    end;

    //NCount+1 because we duplicate last point to let algo work on 2 polylines
    SetLength(fKeep[I], Count + 1);
    for K := 0 to Count do
      fKeep[I,K] := False;
    fKeep[I,0] := True;
    fKeep[I,Count] := True;

    //We split loop in half and simplify both segments independently as two convex
    //lines. That is because algo is aimed at polyline, not polyloop
    KeepMiddle := True;

    //Keep nodes on edges (skip 1st-last that are already marked)
    for K := 1 to Count - 1 do
    if (Nodes[K].X = fRect.Left) or (Nodes[K].Y = fRect.Top)
    or (Nodes[K].X = fRect.Right) or (Nodes[K].Y = fRect.Bottom) then
    begin
      fKeep[I,K] := True;
      KeepMiddle := False;
    end;

    //Find farthest corner in the middle
    if KeepMiddle then
    begin
      Best := 0;
      BestI := 0;
      for K := 2 to Count - 2 do
      if (Corners[K] = 1) and (Best < KMDistanceSqr(Nodes[0], Nodes[K])) then
      begin
        Best := KMDistanceSqr(Nodes[0], Nodes[K]);
        BestI := K;
      end;
      fKeep[I, BestI] := True;
    end;
  end;
end;


//Check that shapes have at least 3 points
procedure TKMSimplifyShapes.FixDegenerateShapes;
var
  I, K: Integer;
  Prev: Integer;
  Node1, Node2: TKMPoint;
  A, B: Integer;
  Best: Single;
begin
  for I := 0 to fIn.Count - 1 do
    if fOut.Shape[I].Count < 3 then
      Assert(fOut.Shape[I].Count = 2, 'Each shape must have at least 2 points');

  //Forcefully split 2 node outlines each segments
  for I := 0 to fIn.Count - 1 do
  if fOut.Shape[I].Count = 2 then
  begin
    Prev := 0;
    for K := 1 to fIn.Shape[I].Count do
    if fKeep[I, K] then
    begin
      Simplify(I, Prev, K, Sqr(fError), True);
      Prev := K;
    end;
  end;

  //Add 4th point to each 3 point shape (to make sure it is not degenerate)
  for I := 0 to fIn.Count - 1 do
    if fOut.Shape[I].Count = 3 then
    begin
      A := 0; B := 0; Best := -1;
      Prev := 0;
      for K := 1 to fIn.Shape[I].Count do
        if fKeep[I, K] then
        begin
          Node1 := fIn.Shape[I].Nodes[Prev];
          Node2 := fIn.Shape[I].Nodes[K mod fIn.Shape[I].Count];
          if KMDistanceSqr(Node1, Node2) > Best then
          begin
            A := Prev;
            B := K;
            Best := KMDistanceSqr(Node1, Node2);
          end;
          Prev := K;
        end;
      Simplify(I, A, B, Sqr(fError), True);
    end;//}
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
  var A,B,C,D: TKMPoint;
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


procedure SimplifyStraights(const aIn: TKMShapesArray; aRect: TKMRect; var aOut: TKMShapesArray);
  procedure SimplifyStraights2(const aIn: TKMNodesArray; var aOut: TKMNodesArray);
  var
    K: Integer;
    P0, P1, P2: TKMPoint;
    NodesOnEdge: Boolean;
    NodesOnLine: Boolean;
    KeepNode: Boolean;
  begin
    //Reserve space for worst case when nothing gets optimized
    SetLength(aOut.Nodes, aIn.Count);

    aOut.Count := 0;
    for K := 0 to aIn.Count - 1 do
    begin
      P0 := aIn.Nodes[(K - 1 + aIn.Count) mod aIn.Count];
      P1 := aIn.Nodes[K];
      P2 := aIn.Nodes[(K + 1) mod aIn.Count];

      NodesOnEdge := (P1.X = aRect.Left) or (P1.Y = aRect.Top) or (P1.X = aRect.Right) or (P1.Y = aRect.Bottom);
      NodesOnLine := ((P0.X = P1.X) and (P1.X = P2.X)) or ((P0.Y = P1.Y) and (P1.Y = P2.Y));

      KeepNode := not NodesOnLine or ((K mod MAX_STRAIGHT_SPAN = 0) and not NodesOnEdge);

      if KeepNode then
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


procedure ForceEdge(var aTriMesh: TKMTriMesh; A,B: TKMPoint; aSkipMissing: Boolean);
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
      Assert(I <= 100, 'End is missing2?');
    until(Loop[LoopCount - 1] = aEnd);
  end;

  procedure TriangulateLoop;
  var L: Integer; V: TKMPointArray; PCount: Word; Pols: array of Word;
  begin
    SetLength(V, LoopCount);
    SetLength(Pols, (LoopCount - 2) * 3);
    for L := 0 to LoopCount - 1 do
      V[L] := aTriMesh.Vertices[Loop[L]];

    Triangulate(V, LoopCount, Pols, PCount);

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

    Assert(aSkipMissing or ((Vertice1 <> -1) and (Vertice2 <> -1)), 'Vertices could not be found?');

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


procedure ForceOutlines(var aTriMesh: TKMTriMesh; aRect: TKMRect; fSimpleOutlines: TKMShapesArray);
  procedure CheckAllPolysFaceUp;
  var I: Integer;
  begin
    with aTriMesh do
    for I := 0 to High(Polygons) do
      Assert(KMNormal2Poly(Vertices[Polygons[I,0]], Vertices[Polygons[I,1]], Vertices[Polygons[I,2]]) >= 0);
  end;

var
  Tmp: TKMPointArray;

  procedure SortAndApply(aX: Boolean);
  var
    I, K: Integer;
  begin
  //Sort
  for I := 0 to High(Tmp) do
    for K := I + 1 to High(Tmp) do
      if aX and (Tmp[K].X < Tmp[I].X)
      or not aX and (Tmp[K].Y < Tmp[I].Y) then
        KMSwapPoints(Tmp[K], Tmp[I]);

  //Apply
  for I := 0 to High(Tmp) - 1 do
    ForceEdge(aTriMesh, Tmp[I], Tmp[I+1], True);
  end;
var
  I, K: Integer;
begin
  for I := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[I] do
      for K := 0 to Count - 1 do
        ForceEdge(aTriMesh, Nodes[K], Nodes[(K + 1) mod Count], False);

  //Collect
  for I := 0 to High(aTriMesh.Vertices) do
  if aTriMesh.Vertices[I].X = aRect.Left then
  begin
    SetLength(Tmp, Length(Tmp)+1);
    Tmp[High(Tmp)] := aTriMesh.Vertices[I];
  end;

  SortAndApply(False);
  SetLength(Tmp, 0);

  //Collect
  for I := 0 to High(aTriMesh.Vertices) do
  if aTriMesh.Vertices[I].Y = aRect.Top then
  begin
    SetLength(Tmp, Length(Tmp)+1);
    Tmp[High(Tmp)] := aTriMesh.Vertices[I];
  end;

  SortAndApply(True);
  SetLength(Tmp, 0);

  //Collect
  for I := 0 to High(aTriMesh.Vertices) do
  if aTriMesh.Vertices[I].X = aRect.Right then
  begin
    SetLength(Tmp, Length(Tmp)+1);
    Tmp[High(Tmp)] := aTriMesh.Vertices[I];
  end;

  SortAndApply(False);
  SetLength(Tmp, 0);

  //Collect
  for I := 0 to High(aTriMesh.Vertices) do
  if aTriMesh.Vertices[I].Y = aRect.Bottom then
  begin
    SetLength(Tmp, Length(Tmp)+1);
    Tmp[High(Tmp)] := aTriMesh.Vertices[I];
  end;

  SortAndApply(True);
  SetLength(Tmp, 0);

  CheckAllPolysFaceUp;
end;


procedure RemoveObstaclePolies(var aTriMesh: TKMTriMesh; fSimpleOutlines: TKMShapesArray);
type
  TPolyFill = (pfUnknown, pfKeep, pfRemove);
var
  Mark: array of TPolyFill;

  procedure MarkOutlines(var aTriMesh: TKMTriMesh; aNodes: TKMNodesArray);
  var
    I, K: Integer;
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
    end;
  end;

  procedure FloodFill;
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
    I: Integer;
  begin
    with aTriMesh do
    for I := 0 to High(Polygons) do
    if Mark[I] = pfRemove then
    begin
      DoFlood(Polygons[I,0], Polygons[I,1]);
      DoFlood(Polygons[I,1], Polygons[I,2]);
      DoFlood(Polygons[I,2], Polygons[I,0]);
    end;
  end;

  procedure CutTriangles;
  var
    I: Integer;
  begin
    with aTriMesh do
    begin
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
  for I := 0 to High(aTriMesh.Polygons) do
    Mark[I] := pfUnknown;

  for I := 0 to fSimpleOutlines.Count - 1 do
    MarkOutlines(aTriMesh, fSimpleOutlines.Shape[I]);

  FloodFill;

  CutTriangles;
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
    if (Polygons[I,0] < 4) or (Polygons[I,1] < 4) or (Polygons[I,2] < 4) then
    begin
      //Cut the triangle (Move last triangle to I)
      Polygons[I,0] := Polygons[High(Polygons),0];
      Polygons[I,1] := Polygons[High(Polygons),1];
      Polygons[I,2] := Polygons[High(Polygons),2];
      Dec(I);
      SetLength(Polygons, Length(Polygons) - 1);
    end;
    Inc(I);
  until(I >= Length(Polygons));

  for I := 0 to High(aTriMesh.Vertices) - 4 do
    aTriMesh.Vertices[I] := aTriMesh.Vertices[I+4];

  for I := 0 to High(aTriMesh.Polygons) do
  begin
    aTriMesh.Polygons[I,0] := aTriMesh.Polygons[I,0] - 4;
    aTriMesh.Polygons[I,1] := aTriMesh.Polygons[I,1] - 4;
    aTriMesh.Polygons[I,2] := aTriMesh.Polygons[I,2] - 4;
  end;
end;


procedure CheckForDegenerates(var aTriMesh: TKMTriMesh);
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
      raise Exception.Create('Degenerate poly left');
      {//Move last triangle to I
      Polygons[I,0] := Polygons[High(Polygons),0];
      Polygons[I,1] := Polygons[High(Polygons),1];
      Polygons[I,2] := Polygons[High(Polygons),2];
      Dec(I);
      SetLength(Polygons, Length(Polygons) - 1);}
    end;
    Inc(I);
  until(I >= Length(Polygons));
end;


end.
