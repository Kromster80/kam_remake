unit KM_Points;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils;

type
  TKMDirection = (dir_NA, dir_N, dir_NE, dir_E, dir_SE, dir_S, dir_SW, dir_W, dir_NW);

type
  //Records must be packed so they are stored identically in MP saves (padding bytes are unknown values)
  TKMPoint = record X,Y: Integer; end;
  TKMPointDir = packed record Loc: TKMPoint; Dir: TKMDirection; end;
  TKMPointExact = packed record Loc: TKMPoint; Exact: Boolean; end;
  TKMPointF = record X,Y: Single; end;
  TKMPointW = record X,Y: Word; end; // For backwards compatibility with cmp files
  TKMPointArray = array of TKMPoint;
  TKMTrisArray = array of array [0..2] of Integer;

  TKMTriMesh = record
    Vertices: TKMPointArray;
    Polygons: TKMTrisArray;
  end;

  //We have our own TKMRect that consistently matches TKMPoint range
  //Rects are often used without range checking and include negative off-map coords
  TKMRect = packed record Left, Top, Right, Bottom: Integer end;
  TKMRectF = packed record Left, Top, Right, Bottom: Single end;

  TKMPointFunction = function(aPoint: TKMPoint): Boolean of object;

  function KMPoint(X,Y: Integer): TKMPoint;
  function KMPointF(X,Y: Single): TKMPointF; overload;
  function KMPointF(P: TKMPoint):  TKMPointF; overload;
  function KMPointDir(X,Y: Integer; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointDir(P: TKMPoint; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointX1Y1(P:TKMPoint): TKMPoint;
  function KMPointBelow(P: TKMPoint): TKMPoint;
  function KMPointAbove(P: TKMPoint): TKMPoint;
  function KMNormVector(const P: TKMPoint; R: Integer): TKMPoint;

  function KMPointRound(const P: TKMPointF): TKMPoint;
  function KMSamePoint(P1,P2: TKMPoint): Boolean; overload;
  function KMSamePointF(P1,P2: TKMPointF): Boolean; overload;
  function KMSamePointF(P1,P2: TKMPointF; Epsilon:single): boolean; overload;
  function KMSamePointDir(P1,P2: TKMPointDir): boolean;

  function KMRect(aLeft, aTop, aRight, aBottom: SmallInt): TKMRect; overload;
  function KMRect(aPoint: TKMPoint): TKMRect; overload;
  function KMRect(aPoint: TKMPointF): TKMRect; overload;
  function KMRectF(aRect: TKMRect): TKMRectF; overload;
  function KMRectF(aPoint: TKMPointF): TKMRectF; overload;
  function KMRectF(aLeft, aTop, aRight, aBottom: SmallInt): TKMRectF; overload;
  function KMRectRound(aRect: TKMRectF): TKMRect;
  function KMRectGrow(aRect: TKMRect; aInset: Integer): TKMRect;
  function KMRectGrowTopLeft(aRect: TKMRect): TKMRect;
  function KMRectShinkTopLeft(aRect: TKMRect): TKMRect;
  function KMRectGrowBottomRight(aRect: TKMRect): TKMRect;
  function KMClipRect(aRect: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect;
  function KMInRect(aPoint: TKMPoint; aRect: TKMRect): Boolean; overload;
  function KMInRect(aPoint: TKMPointF; aRect: TKMRect): Boolean; overload;
  function KMInRect(aPoint: TKMPointF; aRect: TKMRectF): Boolean; overload;
  function KMRectArea(aRect: TKMRect): Integer;
  function KMRectMove(aRect: TKMRect; X,Y: Integer): TKMRect;

  function KMGetDirection(X,Y: Integer): TKMDirection; overload;
  function KMGetDirection(X,Y: Single): TKMDirection; overload;
  function KMGetDirection(P: TKMPointF): TKMDirection; overload;
  function KMGetDirection(FromPos, ToPos: TKMPoint): TKMDirection; overload;
  function KMGetDirection(FromPos, ToPos: TKMPointF): TKMDirection; overload;
  function GetDirModifier(Dir1,Dir2:TKMDirection): Byte;
  function KMGetVertexDir(X,Y: integer): TKMDirection;
  function KMGetVertexTile(P: TKMPoint; Dir: TKMDirection): TKMPoint;
  function KMGetVertex(Dir: TKMDirection): TKMPointF;
  function KMGetPointInDir(aPoint: TKMPoint; aDir: TKMDirection; aDist: Byte = 1): TKMPoint;

  function KMAddDirection(aDir: TKMDirection; aAdd: Byte): TKMDirection;
  function KMNextDirection(aDir: TKMDirection): TKMDirection;
  function KMPrevDirection(aDir: TKMDirection): TKMDirection;

  function KMGetDiagVertex(P1,P2:TKMPoint): TKMPoint;
  function KMStepIsDiag(const P1,P2:TKMPoint): Boolean;

  function KMVectorDiff(const A, B: TKMPoint): TKMPoint;
  function KMDotProduct(const A, B: TKMPoint): Single;
  function KMDistanceSqr(const A, B: TKMPoint): Single; overload;
  function KMDistanceSqr(const A, B: TKMPointF): Single; overload;

  function KMPerpendecular(A,B: TKMPoint): TKMPointF;
  //Cross product of 2D vectors, pointed either Up or Down
  function KMNormal2Poly(const v1,v2,v3: TKMPoint): Single; overload;
  function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
  function KMSegmentsIntersect(A, B, C, D: TKMPoint): Boolean;
  function KMSegmentsIntersectOrTouch(A, B, C, D: TKMPoint): Boolean;

  function KMLength(A, B: TKMPoint): Single; overload;
  function KMLength(A, B: TKMPointF): Single; overload;
  function KMLengthDiag(A, B: TKMPoint): Single; overload;
  function KMLengthDiag(X,Y: Integer; B: TKMPoint): Single; overload;
  function KMLengthSqr(A, B: TKMPoint): Integer; overload;
  function KMLengthSqr(A, B: TKMPointF): Single; overload;

  function KMLerp(A,B: TKMPoint; MixValue: Single): TKMPointF; overload;
  function KMLerp(A,B: TKMPointF; MixValue: Single): TKMPointF; overload;

  procedure KMSwapPoints(var A,B: TKMPoint);
  procedure KMSwapPointDir(var A,B: TKMPointDir);

  function TypeToString(T: TKMPoint): string; overload;
  function TypeToString(T: TKMDirection): string; overload;


implementation


function KMPoint(X,Y: Integer): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPointF(P: TKMPoint): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;


function KMPointF(X, Y: Single): TKMPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPointDir(X,Y: Integer; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc.X := X;
  Result.Loc.Y := Y;
  Result.Dir := Dir;
end;


function KMPointDir(P:TKMPoint; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc := P;
  Result.Dir := Dir;
end;


function KMPointX1Y1(P: TKMPoint): TKMPoint;
begin
  Result.X := P.X + 1;
  Result.Y := P.Y + 1;
end;


function KMPointBelow(P: TKMPoint): TKMPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y + 1;
end;


function KMPointAbove(P: TKMPoint): TKMPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y - 1;
end;

function KMNormVector(const P: TKMPoint; R: Integer): TKMPoint;
begin
  Result.X := Round(R*P.X / sqrt(sqr(P.X) + sqr(P.Y)));
  Result.Y := Round(R*P.Y / sqrt(sqr(P.X) + sqr(P.Y)));
end;

function KMPointRound(const P: TKMPointF): TKMPoint;
begin
  Result.X := Round(P.X);
  Result.Y := Round(P.Y);
end;


function KMSamePoint(P1,P2: TKMPoint): Boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;


function KMSamePointF(P1,P2: TKMPointF): Boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;


function KMSamePointF(P1,P2:TKMPointF; Epsilon:single): boolean;
begin
  Result := (abs(P1.X - P2.X) < Epsilon) and (abs(P1.Y - P2.Y) < Epsilon);
end;


function KMSamePointDir(P1,P2: TKMPointDir): boolean;
begin
  Result := ( P1.Loc.X = P2.Loc.X ) and ( P1.Loc.Y = P2.Loc.Y ) and ( P1.Dir = P2.Dir );
end;


function KMRect(aLeft, aTop, aRight, aBottom: SmallInt): TKMRect;
begin
  Result.Left   := aLeft;
  Result.Right  := aRight;
  Result.Top    := aTop;
  Result.Bottom := aBottom;
end;


//Make rect with single point
function KMRect(aPoint: TKMPoint): TKMRect;
begin
  Result.Left   := aPoint.X;
  Result.Right  := aPoint.X;
  Result.Top    := aPoint.Y;
  Result.Bottom := aPoint.Y;
end;


//Encompass PointF into fixed-point rect (2x2)
function KMRect(aPoint: TKMPointF): TKMRect;
begin
  Result.Left   := Floor(aPoint.X) - Byte(Frac(aPoint.X) = 0);
  Result.Right  := Ceil(aPoint.X)  + Byte(Frac(aPoint.X) = 0);
  Result.Top    := Floor(aPoint.Y) - Byte(Frac(aPoint.Y) = 0);
  Result.Bottom := Ceil(aPoint.Y)  + Byte(Frac(aPoint.Y) = 0);
end;


function KMRectF(aRect: TKMRect): TKMRectF;
begin
  Result.Left   := aRect.Left;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top;
  Result.Bottom := aRect.Bottom;
end;


function KMRectF(aPoint: TKMPointF): TKMRectF;
begin
  Result.Left   := aPoint.X;
  Result.Right  := aPoint.X;
  Result.Top    := aPoint.Y;
  Result.Bottom := aPoint.Y;
end;


function KMRectF(aLeft, aTop, aRight, aBottom: SmallInt): TKMRectF;
begin
  Result.Left   := aLeft;
  Result.Right  := aRight;
  Result.Top    := aTop;
  Result.Bottom := aBottom;
end;


function KMRectRound(aRect: TKMRectF): TKMRect;
begin
  Result.Left   := Round(aRect.Left);
  Result.Right  := Round(aRect.Right);
  Result.Top    := Round(aRect.Top);
  Result.Bottom := Round(aRect.Bottom);
end;


function KMRectGrow(aRect: TKMRect; aInset: Integer): TKMRect;
begin
  Result.Left   := Math.Max(aRect.Left   - aInset, 0);
  Result.Right  := Math.Max(aRect.Right  + aInset, 0);
  Result.Top    := Math.Max(aRect.Top    - aInset, 0);
  Result.Bottom := Math.Max(aRect.Bottom + aInset, 0);
end;


function KMRectGrowTopLeft(aRect: TKMRect): TKMRect;
begin
  Result.Left   := aRect.Left - 1;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top  - 1;
  Result.Bottom := aRect.Bottom;
end;


function KMRectShinkTopLeft(aRect: TKMRect): TKMRect;
begin
  Result.Left   := aRect.Left + 1;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top  + 1;
  Result.Bottom := aRect.Bottom;
end;


function KMRectGrowBottomRight(aRect: TKMRect): TKMRect;
begin
  Result.Left   := aRect.Left;
  Result.Right  := aRect.Right + 1;
  Result.Top    := aRect.Top;
  Result.Bottom := aRect.Bottom + 1;
end;


function KMClipRect(aRect: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect;
begin
  Result.Left   := EnsureRange(aRect.Left, X1, X2);
  Result.Right  := EnsureRange(aRect.Right, X1, X2);
  Result.Top    := EnsureRange(aRect.Top, Y1, Y2);
  Result.Bottom := EnsureRange(aRect.Bottom, Y1, Y2);
end;


function KMInRect(aPoint: TKMPoint; aRect: TKMRect): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMInRect(aPoint: TKMPointF; aRect: TKMRect): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMInRect(aPoint: TKMPointF; aRect: TKMRectF): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMRectArea(aRect: TKMRect):Integer;
begin
  Result := (aRect.Right - aRect.Left) * (aRect.Bottom  - aRect.Top);
end;


function KMRectMove(aRect: TKMRect; X,Y: Integer): TKMRect;
begin
  Result.Left   := aRect.Left + X;
  Result.Right  := aRect.Right + X;
  Result.Top    := aRect.Top + Y;
  Result.Bottom := aRect.Bottom + Y;
end;


function KMGetDirection(X,Y: Integer): TKMDirection;
const
  DirectionsBitfield: array [-1..1, -1..1] of TKMDirection =
    ((dir_NW, dir_W,  dir_SW),
     (dir_N,  dir_NA, dir_S),
     (dir_NE, dir_E,  dir_SE));
var
  Scale: Integer;
  A, B: ShortInt;
begin
  Scale := Max(Max(Abs(X), Abs(Y)), 1);
  A := Round(X / Scale);
  B := Round(Y / Scale);
  Result := DirectionsBitfield[A, B]; // -1, 0, 1
end;


function KMGetDirection(X,Y: Single): TKMDirection;
const
  DirectionsBitfield: array [-1..1, -1..1] of TKMDirection =
    ((dir_NW, dir_W,  dir_SW),
     (dir_N,  dir_NA, dir_S),
     (dir_NE, dir_E,  dir_SE));
var
  Scale: Single;
  A, B: ShortInt;
begin
  Scale := Max(Max(Abs(X), Abs(Y)), 1);
  A := Round(X / Scale);
  B := Round(Y / Scale);
  Result := DirectionsBitfield[A, B]; // -1, 0, 1
end;


function KMGetDirection(P: TKMPointF): TKMDirection;
begin
  Result := KMGetDirection(P.X, P.Y);
end;


function KMGetDirection(FromPos, ToPos: TKMPoint): TKMDirection;
begin
  Result := KMGetDirection(Integer(ToPos.X - FromPos.X), Integer(ToPos.Y - FromPos.Y));
end;


function KMGetDirection(FromPos, ToPos: TKMPointF): TKMDirection;
begin
  Result := KMGetDirection(ToPos.X - FromPos.X, ToPos.Y - FromPos.Y);
end;


//How big is the difference between directions (in fights hit from behind is 5 times harder)
//  1 0 1
//  2   2
//  3 4 3
function GetDirModifier(Dir1,Dir2: TKMDirection): Byte;
begin
  Result := Abs(Byte(Dir1) - ((Byte(Dir2) + 4) mod 8));

  if Result > 4 then
    Result := 8 - Result; //Mirror it, as the difference must always be 0..4
end;


function KMGetVertexDir(X,Y: Integer): TKMDirection;
const DirectionsBitfield: array [-1..0, -1..0] of TKMDirection =
        ((dir_SE, dir_NE), (dir_SW, dir_NW));
begin
  Result := DirectionsBitfield[X,Y];
end;


function KMGetVertexTile(P: TKMPoint; Dir: TKMDirection): TKMPoint;
const
  XBitField: array[TKMDirection] of smallint = (0,0,1,0,1,0,0,0,0);
  YBitField: array[TKMDirection] of smallint = (0,0,0,0,1,0,1,0,0);
begin
  Result := KMPoint(P.X+XBitField[Dir], P.Y+YBitField[Dir]);
end;


function KMGetVertex(Dir: TKMDirection): TKMPointF;
const
  XBitField: array[TKMDirection] of single = (0, 0, 0.7,1,0.7,0,-0.7,-1,-0.7);
  YBitField: array[TKMDirection] of single = (0,-1,-0.7,0,0.7,1, 0.7, 0,-0.7);
begin
  Result := KMPointF(XBitField[Dir], YBitField[Dir]);
end;


function KMGetPointInDir(aPoint: TKMPoint; aDir: TKMDirection; aDist: Byte = 1): TKMPoint;
const
  XBitField: array [TKMDirection] of SmallInt = (0, 0, 1, 1, 1, 0,-1,-1,-1);
  YBitField: array [TKMDirection] of SmallInt = (0,-1,-1, 0, 1, 1, 1, 0,-1);
begin
  Result.X := aPoint.X + XBitField[aDir] * aDist;
  Result.Y := aPoint.Y + YBitField[aDir] * aDist;
end;


function KMAddDirection(aDir: TKMDirection; aAdd: Byte): TKMDirection;
begin
  Assert(aDir <> dir_NA);
  Result := TKMDirection((Byte(aDir) + aAdd - 1) mod 8 + 1);
end;


function KMNextDirection(aDir: TKMDirection): TKMDirection;
begin
  if aDir < dir_NW then
    Result := Succ(aDir)
  else
    Result := dir_N; //Rewind to start
end;


function KMPrevDirection(aDir: TKMDirection): TKMDirection;
begin
  if aDir > dir_N then
    Result := Pred(aDir)
  else
    Result := dir_NW; //Rewind to end
end;


function KMGetDiagVertex(P1,P2: TKMPoint): TKMPoint;
begin
  //Returns the position of the vertex inbetween the two diagonal points (points must be diagonal)
  Result.X := max(P1.X,P2.X);
  Result.Y := max(P1.Y,P2.Y);
end;


function KMStepIsDiag(const P1,P2: TKMPoint): Boolean;
begin
  Result := (P2.X - P1.X <> 0) and (P2.Y - P1.Y <> 0);
end;


function KMVectorDiff(const A, B: TKMPoint): TKMPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;


function KMDotProduct(const A, B: TKMPoint): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;


function KMDistanceSqr(const A, B: TKMPoint): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMDistanceSqr(const A, B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMPerpendecular(A,B: TKMPoint): TKMPointF;
var
  Tmp: TKMPointF;
  D: Single;
begin
  Tmp.X := B.X - A.X;
  Tmp.Y := B.Y - A.Y;

  D := Sqrt(Tmp.X * Tmp.X + Tmp.Y * Tmp.Y);

  Result.X := A.X - Tmp.Y / D;
  Result.Y := A.Y + Tmp.X / D;
end;


function KMNormal2Poly(const v1,v2,v3: TKMPoint): Single;
begin
  Result := (v1.Y - v2.Y) * (v1.X - v3.X) - (v1.X - v2.X) * (v1.Y - v3.Y);
end;


function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
begin
  Result := (KMNormal2Poly(A, B, P) > 0) and (KMNormal2Poly(B, C, P) > 0) and (KMNormal2Poly(C, A, P) > 0);
end;


//Segments intersect
function KMSegmentsIntersect(A, B, C, D: TKMPoint): Boolean;
var
  ABx, ABy, CDx, CDy: Single;
  D2, S, T: Single;
begin
  ABx := B.x - A.x;     ABy := B.y - A.y;
  CDx := D.x - C.x;     CDy := D.y - C.y;

  D2 := -CDx * ABy + ABx * CDy;

  S := (-ABy * (A.x - C.x) + ABx * (A.y - C.y)) / D2;
  T := ( CDx * (A.y - C.y) - CDy * (A.x - C.x)) / D2;

  Result := (S > 0) and (S < 1) and (T > 0) and (T < 1);
end;


function KMSegmentsIntersectOrTouch(A, B, C, D: TKMPoint): Boolean;
var
  ABx, ABy, CDx, CDy: Single;
  D2, S, T: Single;
begin
  ABx := B.x - A.x;     ABy := B.y - A.y;
  CDx := D.x - C.x;     CDy := D.y - C.y;

  D2 := -CDx * ABy + ABx * CDy;

  S := (-ABy * (A.x - C.x) + ABx * (A.y - C.y)) / D2;
  T := ( CDx * (A.y - C.y) - CDy * (A.x - C.x)) / D2;

  Result := (S >= 0) and (S <= 1) and (T >= 0) and (T <= 1);
end;


//True length between 2 points
function KMLength(A,B: TKMPoint): Single;
begin
  Result := Sqrt(Sqr(A.X - B.X) + Sqr(A.Y - B.Y));
end;


function KMLength(A,B: TKMPointF): Single;
begin
  Result := Sqrt(Sqr(A.X - B.X) + Sqr(A.Y - B.Y));
end;


//Rough and faster Length as combination of straight and diagonal
function KMLengthDiag(A, B: TKMPoint): Single;
begin
  if Abs(A.X - B.X) > Abs(A.Y - B.Y) then
    Result := Abs(A.X - B.X) + Abs(A.Y - B.Y) * 0.41
  else
    Result := Abs(A.Y - B.Y) + Abs(A.X - B.X) * 0.41;
end;


function KMLengthDiag(X,Y: Integer; B: TKMPoint): Single;
begin
  if Abs(X - B.X) > Abs(Y - B.Y) then
    Result := Abs(X - B.X) + Abs(Y - B.Y) * 0.41
  else
    Result := Abs(Y - B.Y) + Abs(X - B.X) * 0.41;
end;


//Squared length for cases where we need to compare two lengths
//or pick the best one and actual value is not that important
//we can save some cycles on ommitting SQRT
function KMLengthSqr(A, B: TKMPoint): Integer;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLengthSqr(A, B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLerp(A,B: TKMPoint; MixValue: Single): TKMPointF;
begin
  Result.X := A.X + (B.X - A.X) * MixValue;
  Result.Y := A.Y + (B.Y - A.Y) * MixValue;
end;


function KMLerp(A,B: TKMPointF; MixValue: Single): TKMPointF;
begin
  Result.X := A.X + (B.X - A.X) * MixValue;
  Result.Y := A.Y + (B.Y - A.Y) * MixValue;
end;


procedure KMSwapPoints(var A,B: TKMPoint);
var T: Integer;
begin
  T:=A.X; A.X:=B.X; B.X:=T;
  T:=A.Y; A.Y:=B.Y; B.Y:=T;
end;


procedure KMSwapPointDir(var A,B: TKMPointDir);
var
  T: TKMPointDir;
begin
  T := A;
  A := B;
  B := T;
end;


function TypeToString(T: TKMPoint): string;
begin
  Result := '(' + IntToStr(T.X) + ';' + IntToStr(T.Y) + ')';
end;


function TypeToString(T: TKMDirection): string;
const
  S: array [TKMDirection] of string = ('N/A', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW');
begin
  Result := S[T];
end;


end.
