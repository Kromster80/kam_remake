unit KM_Utils;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  {$IFDEF Unix}
  unix, baseunix, UnixUtil,
  {$ENDIF}
  {$IFDEF FPC} FileUtil, {$ENDIF}
  {$IFDEF WDC} IOUtils, {$ENDIF}
	SysUtils, StrUtils, Classes, Controls, KM_CommonTypes;

  function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;

	function GetShiftState(aButton: TMouseButton): TShiftState;
  function GetMultiplicator(aButton: TMouseButton): Word; overload;
  function GetMultiplicator(aShift: TShiftState): Word; overload;


implementation
uses
  KM_CommonUtils, KM_ResTexts, KM_ResKeys;


function GetShiftState(aButton: TMouseButton): TShiftState;
begin
  Result := [];
  case aButton of
    mbLeft:   Include(Result, ssLeft);
    mbRight:  Include(Result, ssRight);
  end;

  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
end;


function GetMultiplicator(aButton: TMouseButton): Word;
begin
  Result := GetMultiplicator(GetShiftState(aButton));
end;


function GetMultiplicator(aShift: TShiftState): Word;
begin
  Result := Byte(aShift = [ssLeft]) + Byte(aShift = [ssRight]) * 10 + Byte(aShift = [ssShift, ssLeft]) * 100 + Byte(aShift = [ssShift, ssRight]) * 1000;
end;


function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;
var
  HotKeyStr: String;
begin
  Result := gResTexts[aTextId];
  HotKeyStr := gResKeys.GetKeyNameById(aHotkeyId);
  if HotKeyStr <> '' then
    Result := Result + Format(' (''%s'')', [HotKeyStr]);

end;


end.

