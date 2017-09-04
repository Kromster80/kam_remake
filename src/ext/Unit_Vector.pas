unit Unit_Vector;
interface


type
  TVector2f = record
    case Integer of
      0: (X, Y: Single);
      1: (U, V: Single);
    end;

    TVector2i = record X,Y: Integer; end;
    TVector2d = record X,Y: Double; end;
    TVector3f = record X,Y,Z: Single; end;
    TVector3d = record X,Y,Z: Double; end;
    TVector3i = record X,Y,Z: Integer; end;
    TVector4f = record X,Y,Z,W: Single; end;
    TVector4d = record X,Y,Z,W: Double; end;

    PVector3f = ^TVector3f;

  TVertice = record
    X,Y,Z: Single;
    nx, ny, nz: Single;
  end;

  TPoly3 = array [0..2] of Integer;

  function Vector2i(X, Y: Integer): TVector2i;
  function Vector3(X, Y, Z: Single): TVector3f;
  function Vector4d(X, Y, Z, W: Single): TVector4d;
  function Vertice(X, Y, Z, nx, ny, nz: Single): TVertice;
  function Poly3(A,B,C: Integer): TPoly3;

  function VectorLengthSqr(const A: TVector3f): Single;
  function VectorAdd(const A,B: TVector3f): TVector3f;
  function VectorSubtract(const A,B: TVector3f): TVector3f;
  function VectorScale(const A: TVector3f; Scale: Single): TVector3f;
  function VectorDotProduct(const A,B: TVector3f): Single;
  function VectorCrossProduct(const A,B : TVector3f): TVector3f;
  function VectorCombine(const A,B: TVector3f; const Coef: Single): TVector3f;

  function RayTriangleIntersect(const aRayStart, aRayVector, A, B, C: TVector3f; out aPoint, aNormal: TVector3f): Boolean;


implementation


function Vector2i(X, Y: Integer): TVector2i;
begin
  Result.X := X;
  Result.Y := Y;
end;


function Vector3(X, Y, Z: Single): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


function Vector4d(X, Y, Z, W: Single): TVector4d;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;


function Vertice(X, Y, Z, nx, ny, nz: Single): TVertice;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.nx := nx;
  Result.ny := ny;
  Result.nz := nz;
end;


function Poly3(A,B,C: Integer): TPoly3;
begin
  Result[0] := A;
  Result[1] := B;
  Result[2] := C;
end;


function VectorLengthSqr(const A: TVector3f): Single;
begin
  Result := Sqr(A.X) + Sqr(A.Y) + Sqr(A.Z);
end;


function VectorAdd(const A,B: TVector3f): TVector3f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;


function VectorSubtract(const A,B: TVector3f): TVector3f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;


function VectorScale(const A: TVector3f; Scale: Single): TVector3f;
begin
  Result.X := A.X * Scale;
  Result.Y := A.Y * Scale;
  Result.Z := A.Z * Scale;
end;


function VectorNormalize(const A: TVector3f): TVector3f;
var
  Scale: Single;
begin
  Scale := Sqrt(VectorLengthSqr(A));

  if Scale = 0 then
    Scale := 1;

  Result.X := A.X / Scale;
  Result.Y := A.Y / Scale;
  Result.Z := A.Z / Scale;
end;


function VectorDotProduct(const A,B: TVector3f): Single;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;


function VectorCrossProduct(const A,B : TVector3f): TVector3f;
begin
  Result.X := A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;


function VectorCombine(const A,B: TVector3f; const Coef: Single): TVector3f;
begin
  Result.X := A.X + B.X * Coef;
  Result.Y := A.Y + B.Y * Coef;
  Result.Z := A.Z + B.Z * Coef;
end;


function RayTriangleIntersect(const aRayStart, aRayVector, A, B, C: TVector3f; out aPoint, aNormal: TVector3f): Boolean;
var
  V1, V2, PV, QV, TV: TVector3f;
  T, U, V, Det, InvDet: Single;
begin
  Result := False;

  V1 := VectorSubtract(B, A);
  V2 := VectorSubtract(C, A);
  PV := VectorCrossProduct(aRayVector, V2);
  Det := VectorDotProduct(V1, PV);

  //Skip if Ray is parallel to triangle
  if Abs(Det) < 0.00001 then
    Exit;

  InvDet := 1 / Det;
  TV := VectorSubtract(aRayStart, A);
  U := VectorDotProduct(TV, PV) * InvDet;

  if (U >= 0) and (U <= 1) then
  begin
    QV := VectorCrossProduct(TV, V1);
    V := VectorDotProduct(aRayVector, QV) * InvDet;
    if (V >= 0) and (U + V <= 1) then
    begin
      T := VectorDotProduct(V2, QV) * InvDet;
      if T > 0 then
      begin
        aPoint := VectorCombine(aRayStart, aRayVector, T);
        aNormal := VectorNormalize(VectorCrossProduct(V1, V2));
        Result := True;
      end;
    end;
  end;
end;


end.
