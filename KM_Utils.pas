unit KM_Utils;
interface
uses KromUtils, sysUtils, KM_Defaults;

function TypeToString(t:THouseType):string; overload
function TypeToString(t:TResourceType):string; overload
function TypeToString(t:TUnitType):string; overload
function TypeToString(t:TKMPoint):string; overload
function TypeToString(t:TKMDirection):string; overload

implementation
uses KM_LoadLib;

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
