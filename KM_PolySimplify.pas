unit KM_PolySimplify;
interface
uses
  KM_Points;


type
  TKMPointArray = array of TKMPointI;

  TKMNodesArray = record
    Count: Integer;
    Nodes: TKMPointArray;
  end;

  TKMShapesArray = record
    Count: Integer;
    Shape: array of TKMNodesArray;
  end;


//Simplify shapes by removing unnecessary points from straight lines
procedure SimplifyStraights(const aIn: TKMShapesArray; var aOut: TKMShapesArray);

//Simplify shapes by removing points within aError
procedure SimplifyShapes(const aIn: TKMShapesArray; var aOut: TKMShapesArray; aError: Single; aRect: TKMRect);


implementation


procedure SimplifyStraights(const aIn: TKMShapesArray; var aOut: TKMShapesArray);
  procedure SimplifyStraights2(const aIn: TKMNodesArray; var aOut: TKMNodesArray);
  var K: Integer; P0, P1, P2: Integer;
  begin
    //Reserve space for worst case when nothing gets optimized
    SetLength(aOut.Nodes, aIn.Count);

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


procedure Simplify(aErrorSqr: Single; const aInput: array of TKMPointI; var aKeep: array of Boolean; aFrom, aTo: Integer);
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
  Node2 := KMPointF(aInput[aTo]);
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

  //See if we need to split once again
  if MaxDistSqr > aErrorSqr then
  begin
    aKeep[MaxDistI] := True;

    Simplify(aErrorSqr, aInput, aKeep, aFrom, MaxDistI);
    Simplify(aErrorSqr, aInput, aKeep, MaxDistI, aTo);
  end;
end;


//Based on Douglas-Peucker algorithm for polyline simplification
//  aError - max allowed distance between resulting line and removed points
function PolySimplify(aError: Single; aRect: TKMRect; const aInput: TKMPointArray; var aOutput: TKMPointArray): Integer;
var
  I, N: Integer;
  Prev: Integer;
  Keep: array of Boolean;
begin
  Result := 0;

  //See if there's nothing to simplify
  if Length(aInput) < 4 then Exit;
  //If loop length is < Tolerance
  //  return 0
  //else
  //  return loop intact

  N := Length(aInput);
  Assert((aInput[0].X = aInput[N-1].X) and (aInput[0].Y = aInput[N-1].Y),
         'We need shape to be closed to properly process as a series of polylines');

  SetLength(Keep, N);
  for I := 0 to N - 1 do
    Keep[I] := False;

  //We split loop in half and simplify both segments independently as two convex
  //lines. That is because algo is aimed at polyline, not polyloop
  Keep[0] := True;
  Keep[N div 2] := True;
  Keep[N - 1] := True;

  //Keep more nodes on edges
  for I := 0 to N - 1 do
  if (aInput[I].X = aRect.Left) or (aInput[I].Y = aRect.Top)
  or (aInput[I].X = aRect.Right) or (aInput[I].Y = aRect.Bottom) then
    Keep[I] := True;

  Prev := 0;
  for I := 1 to N - 1 do
  if Keep[I] then
  begin
    //We use Sqr values for all comparisons for speedup
    Simplify(Sqr(aError), aInput, Keep, Prev, I);
    Prev := I;
  end;

  //Fill resulting array with preserved points
  for I := 0 to N - 1 do
  if Keep[I] then
  begin
    aOutput[Result] := aInput[I];
    Inc(Result);
  end;
end;


procedure SimplifyShapes(const aIn: TKMShapesArray; var aOut: TKMShapesArray; aError: Single; aRect: TKMRect);
var I: Integer;
begin
  SetLength(aOut.Shape, aIn.Count);

  for I := 0 to aIn.Count - 1 do
  begin
    //Duplicate last point so that Douglas-Peucker could work on loop as 2 polylines
    SetLength(aIn.Shape[I].Nodes, aIn.Shape[I].Count + 1);
    aIn.Shape[I].Nodes[aIn.Shape[I].Count] := aIn.Shape[I].Nodes[0];
    Inc(aIn.Shape[I].Count);

    //Reserve space for worst case when all points are kept
    SetLength(aOut.Shape[I].Nodes, aIn.Shape[I].Count);
    aOut.Shape[I].Count := PolySimplify(aError, aRect, aIn.Shape[I].Nodes, aOut.Shape[I].Nodes);

    //Cut last point since it duplicates 0
    Dec(aOut.Shape[I].Count);
    SetLength(aOut.Shape[I].Nodes, aOut.Shape[I].Count);
  end;
  aOut.Count := aIn.Count;
end;


end.
