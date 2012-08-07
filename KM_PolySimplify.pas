unit KM_PolySimplify;
interface
uses
  KM_Points;


//Based on Douglas-Peucker algorithm for polyline simplification
//  aError - max allowed distance between resulting line and removed points
function PolySimplify(aError: Single; const aInput: array of TKMPointI; var aOutput: array of TKMPointI): integer;


implementation


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


function PolySimplify(aError: Single; const aInput: array of TKMPointI; var aOutput: array of TKMPointI): Integer;
var
  I, N: integer;
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
  SetLength(Keep, N);
  for I := 0 to N - 1 do
    Keep[I] := False;

  //We split loop in half and simplify both segments independently as two convex
  //lines. That is because algo is aimed at polyline, not polyloop
  Keep[0] := True;
  Keep[N div 2] := True;

  //We use Sqr values for all comparisons for speedup
  Simplify(Sqr(aError), aInput, Keep, 0, N div 2);
  Simplify(Sqr(aError), aInput, Keep, N div 2, N - 1);

  //Fill resulting array with preserved points
  for I := 0 to N - 1 do
  if Keep[I] then
  begin
    aOutput[Result] := aInput[I];
    Inc(Result);
  end;
end;


end.
