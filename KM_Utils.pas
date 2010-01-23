unit KM_Utils;
interface
uses KromUtils, SysUtils, KM_Defaults, Math;

type
  TKMPoint = record X,Y:word; end;
  TKMPointDir = record Loc:TKMPoint; Dir:word; end;
  TKMPointF = record X,Y:single; end;

  function KMPoint(X, Y: word): TKMPoint; overload;
  function KMPoint(P: TKMPointDir): TKMPoint; overload;
  function KMPointF(X, Y: single): TKMPointF; overload;
  function KMPointF(P: TKMPoint):  TKMPointF; overload;
  function KMPointDir(X, Y, Dir: word): TKMPointDir;
  function KMPointX1(P:TKMPoint): TKMPoint;
  function KMPointX1Y1(X, Y: word): TKMPoint; overload;
  function KMPointX1Y1(P:TKMPoint): TKMPoint; overload;
  function KMPointY1(P:TKMPoint): TKMPoint; overload;
  function KMPointY1(P:TKMPointF): TKMPoint; overload;

  function KMPointRound(P:TKMPointf): TKMPoint;
  function KMSamePoint(P1,P2:TKMPoint): boolean;
  function KMSamePointF(P1,P2:TKMPointF): boolean;
  function KMSamePointDir(P1,P2:TKMPointDir): boolean;

  function KMGetDirection(X,Y: integer): TKMDirection; overload;
  function KMGetDirection(FromPos,ToPos: TKMPoint):TKMDirection; overload;
  function KMGetCoord(aPos:TKMPointDir):TKMPointDir;
  function KMGetPointInDir(aPoint:TKMPoint; aDir: TKMDirection): TKMPoint;
  function KMLoopDirection(aDir: byte): TKMDirection;
  function KMGetDiagVertex(P1,P2:TKMPoint): TKMPoint;
  function KMStepIsDiag(P1,P2:TKMPoint):boolean;

  function GetLength(A,B:TKMPoint): single;
  function KMLength(A,B:TKMPoint): single;

  function Mix(A,B:TKMPointF; MixValue:single):TKMPointF; overload;

  procedure KMSwapPoints(var A,B:TKMPoint);

  function GetPositionInGroup(OriginX, OriginY:integer; aDir:TKMDirection; PlaceX,PlaceY:integer):TKMPoint;

  function TypeToString(t:THouseType):string; overload;
  function TypeToString(t:TResourceType):string; overload;
  function TypeToString(t:TUnitType):string; overload;
  function TypeToString(t:TKMPoint):string; overload;
  function TypeToString(t:TKMDirection):string; overload;

implementation
uses KM_LoadLib;


function KMPoint(X, Y: word): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPointF(P:TKMPoint): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function KMPoint(P: TKMPointDir): TKMPoint;
begin
  Result := P.Loc;
end;

function KMPointF(X, Y: single): TKMPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPointDir(X, Y, Dir: word): TKMPointDir;
begin
  Result.Loc := KMPoint(X,Y);
  Result.Dir := Dir;
end;

function KMPointX1Y1(X, Y: word): TKMPoint;
begin
  Result.X := X+1;
  Result.Y := Y+1;
end;

function KMPointX1Y1(P:TKMPoint): TKMPoint;
begin
  Result.X := P.X+1;
  Result.Y := P.Y+1;
end;

function KMPointX1(P:TKMPoint): TKMPoint;
begin
  Result.X := P.X+1;
  Result.Y := P.Y;
end;

function KMPointY1(P:TKMPoint): TKMPoint; overload;
begin
  Result.X := P.X;
  Result.Y := P.Y+1;
end;

function KMPointY1(P:TKMPointF): TKMPoint; overload;
begin
  Result.X := round(P.X);
  Result.Y := round(P.Y)+1;
end;

function KMPointRound(P:TKMPointf): TKMPoint;
begin
  //@Krom: I'm getting an occasional "Range Check Error" here... Is there something we can do to prevent that?
  Assert((P.X>=0));
  Assert((P.Y>=0));
  //@Lewin: RCE mostly happens here if input position is negative. Which it should not be. Means
  //that somewhere else is a bug. Maybe animal gets off map or something... Is that a repeatable bug?
  //Autosave will help to check that.
  //@Krom: No, it's not repeatable, I had it happen in TPR7 a few times... I'll investigate it.
  Result.X := round(P.X);
  Result.Y := round(P.Y);
end;

function KMSamePoint(P1,P2:TKMPoint): boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;

function KMSamePointF(P1,P2:TKMPointF): boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;

function KMSamePointDir(P1,P2:TKMPointDir): boolean;
begin
  Result := ( P1.Loc.X = P2.Loc.X ) and ( P1.Loc.Y = P2.Loc.Y ) and ( P1.Dir = P2.Dir );
end;


function KMGetDirection(X,Y: integer): TKMDirection;
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection =
        ((dir_SE,dir_E,dir_NE),(dir_S,dir_NA,dir_N),(dir_SW,dir_W,dir_NW));
begin
  Result := DirectionsBitfield[sign(X), sign(Y)]; //-1,0,1
end;


function KMGetDirection(FromPos,ToPos: TKMPoint): TKMDirection;
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection =
        ((dir_NW,dir_W,dir_SW),(dir_N,dir_NA,dir_S),(dir_NE,dir_E,dir_SE));
begin
  Result := DirectionsBitfield[sign(ToPos.X - FromPos.X), sign(ToPos.Y - FromPos.Y)]; //-1,0,1
end;


function KMGetCoord(aPos:TKMPointDir):TKMPointDir;
const XYBitfield: array [0..8]of array [1..2]of shortint =
        ((0,0),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)); //N/A, N, NE, E, SE, S, SW, W, NW
begin
  Result.Dir := aPos.Dir;
  Result.Loc.X := aPos.Loc.X + XYBitfield[shortint(aPos.Dir+1),1]; //+1 to dir because it is 0..7 not 0..8 like TKMDirection is
  Result.Loc.Y := aPos.Loc.Y + XYBitfield[shortint(aPos.Dir+1),2];
end;


function KMGetPointInDir(aPoint:TKMPoint; aDir: TKMDirection): TKMPoint;
const
  XBitField: array[TKMDirection] of smallint = (0, 0, 1,1,1,0,-1,-1,-1);
  YBitField: array[TKMDirection] of smallint = (0,-1,-1,0,1,1, 1, 0,-1);
begin
  Result := KMPoint(aPoint.X+XBitField[aDir],aPoint.Y+YBitField[aDir]);
end;


function KMLoopDirection(aDir: byte): TKMDirection; //Used after added or subtracting from direction so it is still 1..8
begin
  Result := TKMDirection(((aDir+7) mod 8)+1);
end;


function KMGetDiagVertex(P1,P2:TKMPoint): TKMPoint;
begin
  //Returns the position of the vertex inbetween the two diagonal points (points must be diagonal)
  Result.X := max(P1.X,P2.X);
  Result.Y := max(P1.Y,P2.Y);
end;


function KMStepIsDiag(P1,P2:TKMPoint):boolean;
begin
  Result := ((sign(P2.X-P1.X) <> 0) and
             (sign(P2.Y-P1.Y) <> 0));
end;


function GetLength(A,B:TKMPoint): single; overload;
begin
  Result := sqrt(sqr(A.x-B.x) + sqr(A.y-B.y));
end;


//Length as straight and diagonal
function KMLength(A,B:TKMPoint): single;
begin
if abs(A.X-B.X) > abs(A.Y-B.Y) then
  Result := abs(A.X-B.X) + abs(A.Y-B.Y)*0.41
else
  Result := abs(A.Y-B.Y) + abs(A.X-B.X)*0.41
end;


function Mix(A,B:TKMPointF; MixValue:single):TKMPointF;
begin
  Result.X := A.X*MixValue + B.X*(1-MixValue);
  Result.Y := A.Y*MixValue + B.Y*(1-MixValue);
end;


procedure KMSwapPoints(var A,B:TKMPoint);
var w:word;
begin
  w:=A.X; A.X:=B.X; B.X:=w;
  w:=A.Y; A.Y:=B.Y; B.Y:=w;
end;


{Returns point where unit should be placed regarding direction & offset from Commanders position}
function GetPositionInGroup(OriginX, OriginY:integer; aDir:TKMDirection; PlaceX,PlaceY:integer):TKMPoint;
const DirAngle:array[TKMDirection]of word =   (0,    0,    45,   90,   135,  180,   225,  270,   315);
const DirRatio:array[TKMDirection]of single = (0,    1,  1.41,    1,  1.41,    1,  1.41,    1,  1.41);
begin
  //If it is < 1 then set it to 1 (troops will stand on edge of map but not past it)
  Result.X := max(OriginX + round( PlaceX*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) - PlaceY*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) ),1);
  Result.Y := max(OriginY + round( PlaceX*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) + PlaceY*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) ),1);
end;


{TypeToString routines}
function TypeToString(t:TUnitType):string;
var s:string;
begin
if byte(t) in [1..29] then
  s:=fTextLibrary.GetTextString(siUnitNames+byte(t))
else
  s:='N/A';
Result:=s;
end;


function TypeToString(t:THouseType):string;
var s:string;
begin
if byte(t) in [1..HOUSE_COUNT] then
  s:=fTextLibrary.GetTextString(siHouseNames+byte(t))
else
  s:='N/A';
Result:=s;
end;


function TypeToString(t:TResourceType):string;
var s:string;
begin
if byte(t) in [1..28] then
  s:=fTextLibrary.GetTextString(siResourceNames+byte(t))
else
  s:='N/A';
Result:=s;
end;


function TypeToString(t:TKMPoint):string;
begin
  Result:='('+inttostr(t.x)+';'+inttostr(t.y)+')';
end;


function TypeToString(t:TKMDirection):string;
begin
  Result:=TKMDirectionS[byte(t)];
end;

end.
