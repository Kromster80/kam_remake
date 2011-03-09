unit KM_Utils;
{$I KaM_Remake.inc}
interface
uses KromUtils, SysUtils, KM_CommonTypes, KM_Defaults, Math;

  function KMPoint(X,Y:word): TKMPoint; overload;
  function KMPoint(P:TKMPointI): TKMPoint; overload;
  function KMPointF(X,Y:single): TKMPointF; overload;
  function KMPointF(P:TKMPoint):  TKMPointF; overload;
  function KMPointDir(X,Y,Dir:word): TKMPointDir; overload;
  function KMPointDir(X, Y:word; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointDir(P:TKMPoint; Dir: word): TKMPointDir; overload;
  function KMPointDir(P:TKMPoint; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointX1(P:TKMPoint): TKMPoint;
  function KMPointX1Y1(X,Y:word): TKMPoint; overload;
  function KMPointX1Y1(P:TKMPoint): TKMPoint; overload;
  function KMPointY1(P:TKMPoint): TKMPoint; overload;
  function KMPointY1(P:TKMPointF): TKMPoint; overload;

  function KMPointRound(const P:TKMPointF): TKMPoint;
  function KMSamePoint(P1,P2:TKMPoint): boolean;
  function KMSamePointF(P1,P2:TKMPointF): boolean; overload;
  function KMSamePointF(P1,P2:TKMPointF; Epsilon:single): boolean; overload;
  function KMSamePointDir(P1,P2:TKMPointDir): boolean;

  function KMGetDirection(X,Y: integer): TKMDirection; overload;
  function KMGetDirection(FromPos,ToPos: TKMPoint):TKMDirection; overload;
  function KMGetDirection(FromPos,ToPos: TKMPointF):TKMDirection; overload;
  function GetDirModifier(Dir1,Dir2:TKMDirection): byte;
  function KMGetCursorDirection(X,Y: integer): TKMDirection;
  function KMGetVertexDir(X,Y: integer):TKMDirection;
  function KMGetVertexTile(P:TKMPoint; Dir: TKMDirection):TKMPoint;
  function KMGetPointInDir(aPoint:TKMPoint; aDir: TKMDirection): TKMPointDir;
  function KMLoopDirection(aDir: byte): TKMDirection;
  function KMGetDiagVertex(P1,P2:TKMPoint): TKMPoint;
  function KMStepIsDiag(P1,P2:TKMPoint):boolean;

  function GetLength(A,B:TKMPoint): single; overload;
  function GetLength(A,B:TKMPointF): single; overload;
  function KMLength(A,B:TKMPoint): single;

  function Mix(A,B:TKMPointF; MixValue:single):TKMPointF; overload;

  procedure KMSwapPoints(var A,B:TKMPoint);

  function GetPositionInGroup2(OriginX, OriginY:integer; aDir:TKMDirection; aI, aUnitPerRow:integer; MapX,MapY:integer; AllowOffMap:boolean=false):TKMPoint;
  function GetPositionFromIndex(aOrigin:TKMPoint; aIndex:byte):TKMPointI;

  function KMMapNameToPath(const aMapName, aExtension:string):string;
  function KMSlotToSaveName(aSlot:integer; const aExtension:string):string;

  function MapSizeToString(X,Y:integer):string;

  function TypeToString(t:THouseType):string; overload;
  function TypeToString(t:TResourceType):string; overload;
  function TypeToString(t:TUnitType):string; overload;
  function TypeToString(t:TKMPoint):string; overload;
  function TypeToString(t:TKMDirection):string; overload;

implementation
uses KM_TextLibrary;


function KMPoint(X,Y:word): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPoint(P:TKMPointI): TKMPoint;
begin
  Assert((P.X>=0) and (P.Y>=0));
  Result.X := P.X;
  Result.Y := P.Y;
end;


function KMPointF(P:TKMPoint): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
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

function KMPointDir(X, Y:word; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc := KMPoint(X,Y);
  Result.Dir := byte(Dir)-1;
end;

function KMPointDir(P:TKMPoint; Dir: word): TKMPointDir;
begin
  Result.Loc := P;
  Result.Dir := Dir;
end;

function KMPointDir(P:TKMPoint; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc := P;
  Result.Dir := byte(Dir)-1;
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


function KMPointRound(const P:TKMPointF):TKMPoint;
begin
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

function KMSamePointF(P1,P2:TKMPointF; Epsilon:single): boolean;
begin
  Result := (abs(P1.X - P2.X) < Epsilon) and (abs(P1.Y - P2.Y) < Epsilon);
end;


function KMSamePointDir(P1,P2:TKMPointDir): boolean;
begin
  Result := ( P1.Loc.X = P2.Loc.X ) and ( P1.Loc.Y = P2.Loc.Y ) and ( P1.Dir = P2.Dir );
end;


function KMGetDirection(X,Y: integer): TKMDirection;
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection =
        ((dir_SE,dir_E,dir_NE),(dir_S,dir_NA,dir_N),(dir_SW,dir_W,dir_NW));
var Scale:integer; a,b:shortint;
begin
  Scale := max(abs(X),abs(Y));
  a := round(X/Scale);
  b := round(Y/Scale);
  Result := DirectionsBitfield[a, b]; //-1,0,1
end;


function KMGetDirection(FromPos,ToPos: TKMPoint): TKMDirection;
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection =
        ((dir_NW,dir_W,dir_SW),(dir_N,dir_NA,dir_S),(dir_NE,dir_E,dir_SE));
var Scale:integer; a,b:shortint;
begin
  Scale := max(abs(ToPos.X-FromPos.X),abs(ToPos.Y-FromPos.Y));
  a := round((ToPos.X-FromPos.X)/Scale);
  b := round((ToPos.Y-FromPos.Y)/Scale);
  Result := DirectionsBitfield[a,b]; //-1,0,1
end;


function KMGetDirection(FromPos,ToPos: TKMPointF): TKMDirection;
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection =
        ((dir_NW,dir_W,dir_SW),(dir_N,dir_NA,dir_S),(dir_NE,dir_E,dir_SE));
var Scale:single; a,b:shortint;
begin
  Scale := max(abs(ToPos.X-FromPos.X),abs(ToPos.Y-FromPos.Y));
  a := round((ToPos.X-FromPos.X)/Scale);
  b := round((ToPos.Y-FromPos.Y)/Scale);
  Result := DirectionsBitfield[a,b]; //-1,0,1
end;


//How big is the difference between directions (in fights hit from behind is 5 times harder)
//  1 0 1
//  2   2
//  3 4 3
function GetDirModifier(Dir1,Dir2:TKMDirection): byte;
begin
  Result := abs(byte(Dir1) - ((byte(Dir2)+4) mod 8));

  if Result > 4 then
    Result := 8 - Result; //Mirror it, as the difference must always be 0..4
end;


function KMGetCursorDirection(X,Y: integer): TKMDirection;
begin
  Result := dir_NA;
  if GetLength(X,Y) <= DirCursorNARadius then exit; //Use default value dir_NA for the middle

  if abs(X) > abs(Y) then
    if X > 0 then Result := dir_W
             else Result := dir_E;
  if abs(Y) > abs(X) then
    if Y > 0 then Result := dir_N
             else Result := dir_S;
  //Only way to select diagonals is by having X=Y (i.e. the corners), that natural way works best
  if X = Y then
    if X > 0 then Result := dir_NW
             else Result := dir_SE;
  if X = -Y then
    if X > 0 then Result := dir_SW
             else Result := dir_NE;
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


function KMGetPointInDir(aPoint:TKMPoint; aDir: TKMDirection): TKMPointDir;
const
  XBitField: array[TKMDirection] of smallint = (0, 0, 1,1,1,0,-1,-1,-1);
  YBitField: array[TKMDirection] of smallint = (0,-1,-1,0,1,1, 1, 0,-1);
begin
  Result.Dir := byte(aDir)-1;
  Result.Loc.X := aPoint.X+XBitField[aDir];
  Result.Loc.Y := aPoint.Y+YBitField[aDir];
end;


//Used after added or subtracting from direction so it is still 1..8
function KMLoopDirection(aDir: byte): TKMDirection;
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


function GetLength(A,B:TKMPointF): single; overload;
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
// 23145     231456
// 6789X     789xxx
function GetPositionInGroup2(OriginX, OriginY:integer; aDir:TKMDirection; aI, aUnitPerRow:integer; MapX,MapY:integer; AllowOffMap:boolean=false):TKMPoint;
const DirAngle:array[TKMDirection]of word =   (0,    0,    45,   90,   135,  180,   225,  270,   315);
const DirRatio:array[TKMDirection]of single = (0,    1,  1.41,    1,  1.41,    1,  1.41,    1,  1.41);
var PlaceX, PlaceY, ResultX, ResultY:integer;
begin
  Assert(aUnitPerRow>0);
  if aI=1 then begin
    PlaceX := 0;
    PlaceY := 0;
  end else begin
    if aI <= aUnitPerRow div 2 + 1 then
      dec(aI);
    PlaceX := (aI-1) mod aUnitPerRow - aUnitPerRow div 2;
    PlaceY := (aI-1) div aUnitPerRow;
  end;

  ResultX := OriginX + round( PlaceX*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) - PlaceY*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) );
  ResultY := OriginY + round( PlaceX*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) + PlaceY*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) );

  if AllowOffMap then
  begin
    //Fit to bounds + 1 on all sides so we know when the unit can't reach it's target (GetClosestTile
    //will correct it)
    Result.X := EnsureRange(ResultX, 0, MapX);
    Result.Y := EnsureRange(ResultY, 0, MapY);
  end else begin
    //Fit to bounds
    Result.X := EnsureRange(ResultX, 1, MapX-1);
    Result.Y := EnsureRange(ResultY, 1, MapY-1);
  end;
end;


//See Docs\GetPositionFromIndex.xls for explanation
function GetPositionFromIndex(aOrigin:TKMPoint; aIndex:byte):TKMPointI;
const Rings:array[1..10] of word = (0, 1, 9, 25, 49, 81, 121, 169, 225, 289);
var                        //Ring#  1  2  3  4   5   6   7    8    9    10
  Ring, Span, Span2, Orig:byte;
  Off1,Off2,Off3,Off4,Off5:byte;
begin
  //Quick solution
  if aIndex=0 then begin
    Result.X := aOrigin.X;
    Result.Y := aOrigin.Y;
    exit;
  end;

  if aIndex = 5 then
    sleep(1);

  //Find ring in which Index is located
  Ring := 0;
  repeat inc(Ring); until(Rings[Ring]>aIndex);
  dec(Ring);

  //Remember Ring span and half-span
  Span := Ring*2-1-1; //Span-1
  Span2 := Ring-1;    //Half a span -1

  //Find offset from Rings 1st item
  Orig := aIndex - Rings[Ring];

  //Find Offset values in each span
  Off1 := min(Orig,Span2); dec(Orig,Off1);
  Off2 := min(Orig,Span);  dec(Orig,Off2);
  Off3 := min(Orig,Span);  dec(Orig,Off3);
  Off4 := min(Orig,Span);  dec(Orig,Off4);
  Off5 := min(Orig,Span2-1); //dec(Orig,Off5);

  //Compute result
  Result.X := aOrigin.X + Off1 - Off3 + Off5;
  Result.Y := aOrigin.Y - Span2 + Off2 - Off4;
end;


function KMMapNameToPath(const aMapName, aExtension:string):string;
begin
  Result := ExeDir+'Maps\'+aMapName+'\'+aMapName+'.'+aExtension;
end;


function KMSlotToSaveName(aSlot:integer; const aExtension:string):string;
begin
  Result := ExeDir+'Saves\save'+int2fix(aSlot,2)+'.'+aExtension;
end;


function MapSizeToString(X,Y:integer):string;
begin
  case X*Y of
            1.. 48* 48: Result := 'XS';
     48* 48+1.. 72* 72: Result := 'S';
     72* 72+1..112*112: Result := 'M';
    112*112+1..176*176: Result := 'L';
    176*176+1..256*256: Result := 'XL';
    256*256+1..320*320: Result := 'XXL';
    else                Result := '???';
  end;
end;


{TypeToString routines}
function TypeToString(t:TUnitType):string;
var s:string;
begin
  case byte(t) of
    1..30: s := fTextLibrary.GetTextString(siUnitNames+byte(t));
    31:    s := 'Wolf';
    32:    s := 'Fish';
    33:    s := 'Watersnake';
    34:    s := 'Seastar';
    35:    s := 'Crab';
    36:    s := 'Waterflower';
    37:    s := 'Waterleaf';
    38:    s := 'Duck';
    else   s := 'N/A';
  end;
  Result := s;
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
