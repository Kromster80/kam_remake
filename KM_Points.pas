unit KM_Points;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils;

type
  TKMDirection = (dir_NA=0, dir_N=1, dir_NE=2, dir_E=3, dir_SE=4, dir_S=5, dir_SW=6, dir_W=7, dir_NW=8);

type
  //Records must be packed so they are stored identically in MP saves (padding bytes are unknown values)
  TKMPoint = packed record X,Y: Word; end;
  TKMPointDir = packed record Loc: TKMPoint; Dir: TKMDirection; end;
  TKMPointF = packed record X,Y: Single; end;
  TKMPointI = packed record X,Y: Integer; end; //Allows negative values
  TKMPointArray = array of TKMPointI;
  TKMTrisArray = array of array [0..2] of Integer;

  TKMTriMesh = record
    Vertices: TKMPointArray;
    Polygons: TKMTrisArray;
  end;

  //We have our own TKMRect that consistently matches TKMPoint range
  //Rects are often used without range checking and include negative off-map coords
  TKMRect = packed record Left, Top, Right, Bottom: SmallInt end;

  TKMPointFunction = function(aPoint: TKMPoint): Boolean of object;

  function KMPoint(X,Y: Word): TKMPoint; overload;
  function KMPoint(P: TKMPointI): TKMPoint; overload;
  function KMPointI(X,Y: Integer): TKMPointI;
  function KMPointF(X,Y: Single): TKMPointF; overload;
  function KMPointF(P: TKMPoint):  TKMPointF; overload;
  function KMPointF(P: TKMPointI):  TKMPointF; overload;
  function KMPointDir(X,Y: Word; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointDir(P: TKMPoint; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointX1Y1(P:TKMPoint): TKMPoint;
  function KMPointBelow(P: TKMPoint): TKMPoint;
  function KMPointAbove(P: TKMPoint): TKMPoint;

  function KMPointRound(const P: TKMPointF): TKMPoint;
  function KMSamePoint(P1,P2: TKMPoint): Boolean; overload;
  function KMSamePoint(P1,P2: TKMPointI): Boolean; overload;
  function KMSamePointF(P1,P2: TKMPointF): Boolean; overload;
  function KMSamePointF(P1,P2: TKMPointF; Epsilon:single): boolean; overload;
  function KMSamePointDir(P1,P2: TKMPointDir): boolean;

  function KMRect(aLeft, aTop, aRight, aBottom: SmallInt): TKMRect; overload;
  function KMRect(aPoint: TKMPoint): TKMRect; overload;
  function KMRect(aPoint: TKMPointF): TKMRect; overload;
  function KMRectGrow(aRect: TKMRect; aInset: Integer): TKMRect;
  function KMRectGrowTopLeft(aRect: TKMRect; aInset: Integer): TKMRect;
  function KMClipRect(aRect: TKMRect; X1,Y1,X2,Y2: Word): TKMRect;
  function KMInRect(aPoint: TKMPoint; aRect: TKMRect): Boolean; overload;
  function KMInRect(aPoint: TKMPointF; aRect: TKMRect): Boolean; overload;
  function KMInRect(aPoint: TKMPointI; aRect: TKMRect): Boolean; overload;
  function KMRectArea(aRect: TKMRect):Integer;

  function KMGetDirection(X,Y: Integer): TKMDirection; overload;
  function KMGetDirection(X,Y: Single): TKMDirection; overload;
  function KMGetDirection(P: TKMPointF): TKMDirection; overload;
  function KMGetDirection(FromPos, ToPos: TKMPoint): TKMDirection; overload;
  function KMGetDirection(FromPos, ToPos: TKMPointF): TKMDirection; overload;
  function GetDirModifier(Dir1,Dir2:TKMDirection): byte;
  function KMGetVertexDir(X,Y: integer):TKMDirection;
  function KMGetVertexTile(P: TKMPoint; Dir: TKMDirection):TKMPoint;
  function KMGetVertex(Dir: TKMDirection):TKMPointF;
  function KMGetPointInDir(aPoint: TKMPoint; aDir: TKMDirection; aDist: Byte = 1): TKMPointI;

  function KMAddDirection(aDir: TKMDirection; aAdd: Byte): TKMDirection;
  function KMNextDirection(aDir: TKMDirection): TKMDirection;
  function KMPrevDirection(aDir: TKMDirection): TKMDirection;

  function KMGetDiagVertex(P1,P2:TKMPoint): TKMPoint;
  function KMStepIsDiag(const P1,P2:TKMPoint):boolean;

  function KMVectorDiff(const A, B: TKMPointI): TKMPointI;
  function KMDotProduct(const A, B: TKMPointI): Single;
  function KMDistanceSqr(const A, B: TKMPointI): Single; overload;
  function KMDistanceSqr(const A, B: TKMPointF): Single; overload;

  function KMPerpendecular(A,B: TKMPoint): TKMPointF;
  //Cross product of 2D vectors, pointed either Up or Down
  function KMNormal2Poly(const v1,v2,v3: TKMPoint): Single; overload;
  function KMNormal2Poly(const v1,v2,v3: TKMPointI): Single; overload;
  function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
  function KMSegmentsIntersect(A, B, C, D: TKMPointI): Boolean;
  function KMSegmentsIntersectOrTouch(A, B, C, D: TKMPointI): Boolean;
  //procedure KMTriangulate(VerticeCount: Integer; Vertice: TKMPointArray; var PolyCount: Integer; var Polys: array of Word);

  function KMLength(A, B: TKMPoint): Single; overload;
  function KMLength(A, B: TKMPointF): Single; overload;
  function KMLengthDiag(A, B: TKMPoint): Single;
  function KMLengthSqr(A, B: TKMPoint): Integer; overload;
  function KMLengthSqr(A, B: TKMPointI): Integer; overload;
  function KMLengthSqr(A, B: TKMPointF): Single; overload;

  function KMLerp(A,B: TKMPoint; MixValue: Single): TKMPointF; overload;
  function KMLerp(A,B: TKMPointF; MixValue: Single): TKMPointF; overload;

  procedure KMSwapPoints(var A,B: TKMPoint); overload;
  procedure KMSwapPoints(var A,B: TKMPointI); overload;

  function TypeToString(T: TKMPoint): string; overload;
  function TypeToString(T: TKMDirection): string; overload;


implementation


function KMPoint(X,Y: Word): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPoint(P: TKMPointI): TKMPoint;
begin
  Assert((P.X >= 0) and (P.Y >= 0));
  Result.X := P.X;
  Result.Y := P.Y;
end;


function KMPointI(X,Y: Integer): TKMPointI;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPointF(P: TKMPoint): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;


function KMPointF(P: TKMPointI): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;


function KMPointF(X, Y: Single): TKMPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPointDir(X,Y: Word; Dir: TKMDirection): TKMPointDir;
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


function KMPointRound(const P: TKMPointF): TKMPoint;
begin
  Result.X := Round(P.X);
  Result.Y := Round(P.Y);
end;


function KMSamePoint(P1,P2: TKMPoint): Boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;


function KMSamePoint(P1,P2: TKMPointI): Boolean;
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


function KMRectGrow(aRect: TKMRect; aInset: Integer): TKMRect;
begin
  Result.Left   := Math.Max(aRect.Left   - aInset, 0);
  Result.Right  := Math.Max(aRect.Right  + aInset, 0);
  Result.Top    := Math.Max(aRect.Top    - aInset, 0);
  Result.Bottom := Math.Max(aRect.Bottom + aInset, 0);
end;


function KMRectGrowTopLeft(aRect: TKMRect; aInset: Integer): TKMRect;
begin
  Result.Left   := Math.Max(aRect.Left - aInset, 0);
  Result.Right  := aRect.Right;
  Result.Top    := Math.Max(aRect.Top  - aInset, 0);
  Result.Bottom := aRect.Bottom;
end;


function KMClipRect(aRect: TKMRect; X1,Y1,X2,Y2: Word): TKMRect;
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


function KMInRect(aPoint: TKMPointI; aRect: TKMRect): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMRectArea(aRect: TKMRect):Integer;
begin
  Result := (aRect.Right - aRect.Left) * (aRect.Bottom  - aRect.Top);
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
  Scale := Max(Abs(X), Abs(Y));
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
  Scale := Max(Abs(X), Abs(Y));
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
  Result := KMGetDirection(ToPos.X - FromPos.X, ToPos.Y - FromPos.Y);
end;


function KMGetDirection(FromPos, ToPos: TKMPointF): TKMDirection;
begin
  Result := KMGetDirection(ToPos.X - FromPos.X, ToPos.Y - FromPos.Y);
end;


//How big is the difference between directions (in fights hit from behind is 5 times harder)
//  1 0 1
//  2   2
//  3 4 3
function GetDirModifier(Dir1,Dir2:TKMDirection): byte;
begin
  Result := Abs(Byte(Dir1) - ((Byte(Dir2) + 4) mod 8));

  if Result > 4 then
    Result := 8 - Result; //Mirror it, as the difference must always be 0..4
end;


function KMGetVertexDir(X,Y: integer):TKMDirection;
const DirectionsBitfield:array[-1..0,-1..0]of TKMDirection =
        ((dir_SE,dir_NE),(dir_SW,dir_NW));
begin
  Result := DirectionsBitfield[X,Y];
end;


function KMGetVertexTile(P:TKMPoint; Dir: TKMDirection):TKMPoint;
const
  XBitField: array[TKMDirection] of smallint = (0,0,1,0,1,0,0,0,0);
  YBitField: array[TKMDirection] of smallint = (0,0,0,0,1,0,1,0,0);
begin
  Result := KMPoint(P.X+XBitField[Dir], P.Y+YBitField[Dir]);
end;


function KMGetVertex(Dir: TKMDirection):TKMPointF;
const
  XBitField: array[TKMDirection] of single = (0, 0, 0.7,1,0.7,0,-0.7,-1,-0.7);
  YBitField: array[TKMDirection] of single = (0,-1,-0.7,0,0.7,1, 0.7, 0,-0.7);
begin
  Result := KMPointF(XBitField[Dir], YBitField[Dir]);
end;


function KMGetPointInDir(aPoint: TKMPoint; aDir: TKMDirection; aDist: Byte = 1): TKMPointI;
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
  Result := (sign(P2.X-P1.X) <> 0) and (sign(P2.Y-P1.Y) <> 0);
end;


function KMVectorDiff(const A, B: TKMPointI): TKMPointI;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;


function KMDotProduct(const A, B: TKMPointI): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;


function KMDistanceSqr(const A, B: TKMPointI): Single;
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


function KMNormal2Poly(const v1,v2,v3: TKMPointI): Single;
begin
  Result := (v1.Y - v2.Y) * (v1.X - v3.X) - (v1.X - v2.X) * (v1.Y - v3.Y);
end;


function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
begin
  Result := (KMNormal2Poly(A, B, P) > 0) and (KMNormal2Poly(B, C, P) > 0) and (KMNormal2Poly(C, A, P) > 0);
end;


//Segments intersect
function KMSegmentsIntersect(A, B, C, D: TKMPointI): Boolean;
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


function KMSegmentsIntersectOrTouch(A, B, C, D: TKMPointI): Boolean;
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


//This function has a bug, it fails to triangulate W-shaped polygon
procedure KMTriangulate(VerticeCount: Integer; Vertice: TKMPointArray; var PolyCount: Integer; var Polys: array of Word);
var
  LoopCount: Integer;
  n0,n1,n2:integer;
  Skip: array of Byte;
  ValidPoly:boolean;
  I, K, L: Integer;
  LastVtx: Integer;
  TripletFound: Boolean;
begin
  SetLength(Skip, VerticeCount);

  LastVtx := 0;
  PolyCount := 0;
  LoopCount := 1;
  repeat

    //Find 3 aligned unused vertices starting from LastVtx
    TripletFound := False;
    n0 := -1;  n1 := -1;  n2 := -1;
    for I := LastVtx to LastVtx + VerticeCount - 1 - 2 do //Keep last 2 points for n1/n2
    if Skip[I mod VerticeCount] = 0 then
    begin
      n0 := I mod VerticeCount;
      for K := I + 1 to LastVtx + VerticeCount - 1 - 1 do //Keep last 1 point for n2
      if Skip[K mod VerticeCount] = 0 then
      begin
        n1 := K mod VerticeCount;
        for L := K + 1 to LastVtx + VerticeCount - 1 do
        if Skip[L mod VerticeCount] = 0 then
        begin
          n2 := L mod VerticeCount;
          TripletFound := True;
          Break;
        end;
        Break;
      end;
      Break;
    end;

    //There are no triplets left, our task is done
    if not TripletFound then
      Break;

    //Check which direction poly is facing, should be Down (depends on vertice order and coords system)
    ValidPoly := KMNormal2Poly(Vertice[n0], Vertice[n1], Vertice[n2]) > 0;

    //Take n0 and n2 as basis and test all remaining vertices to be on one side
    if ValidPoly then
      for I := 0 to VerticeCount - 1 do
      if (Skip[I] = 0) and (I <> n0) and (I <> n1) and (I <> n2) then
      if KMNormal2Poly(Vertice[n0], Vertice[n2], Vertice[I]) < 0 then
      begin
        ValidPoly := False;
        Break;
      end;

    if ValidPoly then
    begin
      Polys[PolyCount * 3]     := n0;
      Polys[PolyCount * 3 + 1] := n1;
      Polys[PolyCount * 3 + 2] := n2;
      Inc(PolyCount);
      Skip[n1] := 1;
      LastVtx := n2;
    end
    else
      LastVtx := n1;

    Inc(LoopCount);
  until(LoopCount=1500); //How long to keep looking for more polys

  Assert(LoopCount < 1500, 'Triangulation failed');
  Assert(PolyCount = VerticeCount - 2, 'Triangulation failed');
end;


function KMLength(A,B: TKMPoint): Single;
begin
  Result := sqrt(sqr(A.x-B.x) + sqr(A.y-B.y));
end;


function KMLength(A,B: TKMPointF): Single;
begin
  Result := sqrt(sqr(A.x-B.x) + sqr(A.y-B.y));
end;


//Length as straight and diagonal
function KMLengthDiag(A, B: TKMPoint): Single;
begin
  if Abs(A.X-B.X) > Abs(A.Y-B.Y) then
    Result := Abs(A.X-B.X) + Abs(A.Y-B.Y) * 0.41
  else
    Result := Abs(A.Y-B.Y) + Abs(A.X-B.X) * 0.41;
end;


function KMLengthSqr(A, B: TKMPoint): Integer;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLengthSqr(A, B: TKMPointI): Integer;
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
var T: Word;
begin
  T:=A.X; A.X:=B.X; B.X:=T;
  T:=A.Y; A.Y:=B.Y; B.Y:=T;
end;


procedure KMSwapPoints(var A,B: TKMPointI);
var T: Integer;
begin
  T:=A.X; A.X:=B.X; B.X:=T;
  T:=A.Y; A.Y:=B.Y; B.Y:=T;
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
