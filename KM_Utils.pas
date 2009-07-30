unit KM_Utils;
interface
uses KromUtils, sysUtils, KM_Defaults;

type
  TKMPoint = record X,Y:word; end;
  TKMPointF = record X,Y:single; end;

  function KMPoint(X, Y: word): TKMPoint;
  function KMPointF(X, Y: single): TKMPointF;
  function KMPointX1Y1(X, Y: word): TKMPoint;
  function KMPointY1(P:TKMPoint): TKMPoint; overload
  function KMPointY1(P:TKMPointF): TKMPoint; overload

  function KMPointRound(P:TKMPointf): TKMPoint;
  function KMSamePoint(P1,P2:TKMPoint): boolean;
  function KMSamePointF(P1,P2:TKMPointF): boolean;

  function GetLength(A,B:TKMPoint): single;
  function KMLength(A,B:TKMPoint): single;
  //function KMRoute(A,B:TKMPoint): single; //Unused

  procedure KMSwapPoints(var A,B:TKMPoint);

  function TypeToString(t:THouseType):string; overload
  function TypeToString(t:TResourceType):string; overload
  function TypeToString(t:TUnitType):string; overload
  function TypeToString(t:TKMPoint):string; overload
  function TypeToString(t:TKMDirection):string; overload

implementation
uses KM_LoadLib;


function KMPoint(X, Y: word): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPointF(X, Y: single): TKMPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPointX1Y1(X, Y: word): TKMPoint;
begin
  Result.X := X+1;
  Result.Y := Y+1;
end;

function KMPointY1(P:TKMPoint): TKMPoint; overload
begin
  Result.X := P.X;
  Result.Y := P.Y+1;
end;

function KMPointY1(P:TKMPointF): TKMPoint; overload
begin
  Result.X := round(P.X);
  Result.Y := round(P.Y)+1;
end;

function KMPointRound(P:TKMPointf): TKMPoint;
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


function GetLength(A,B:TKMPoint): single; overload
begin
  Result:=sqrt(sqr(A.x-B.x)+sqr(A.y-B.y));
end;


//Length as straight and diagonal
function KMLength(A,B:TKMPoint): single;
begin
if abs(A.X-B.X) > abs(A.Y-B.Y) then
  Result := abs(A.X-B.X) + abs(A.Y-B.Y)*0.41
else
  Result := abs(A.Y-B.Y) + abs(A.X-B.X)*0.41
end;


//Length as route, return 0 if unwalkable
{function KMRoute(A,B:TKMPoint): single;
begin
  Result:=0;
end;}


procedure KMSwapPoints(var A,B:TKMPoint);
var w:word;
begin
  w:=A.X; A.X:=B.X; B.X:=w;
  w:=A.Y; A.Y:=B.Y; B.Y:=w;
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
