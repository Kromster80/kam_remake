unit KM_ScriptingUtils;
{$I KaM_Remake.inc}

interface
uses
  Math, SysUtils, uPSRuntime, KM_ScriptingEvents;

type
  TKMScriptUtils = class(TKMScriptEntity)
  public
    function AbsI(aValue: Integer): Integer;
    function AbsS(aValue: Single): Single;

    function ArrayElementCount(aElement: AnsiString; aArray: array of String): Integer;
    function ArrayElementCountB(aElement: Boolean; aArray: array of Boolean): Integer;
    function ArrayElementCountI(aElement: Integer; aArray: array of Integer): Integer;
    function ArrayElementCountS(aElement: Single; aArray: array of Single): Integer;

    function ArrayHasElement(aElement: AnsiString; aArray: array of String): Boolean;
    function ArrayHasElementB(aElement: Boolean; aArray: array of Boolean): Boolean;
    function ArrayHasElementI(aElement: Integer; aArray: array of Integer): Boolean;
    function ArrayHasElementS(aElement: Single; aArray: array of Single): Boolean;

    function EnsureRangeI(aValue, aMin, aMax: Integer): Integer;
    function EnsureRangeS(aValue, aMin, aMax: Single): Single;

    function Format(const aFormatting: string; aData: array of const): string;

    function IfThen(aBool: Boolean; aTrue, aFalse: AnsiString): AnsiString;
    function IfThenI(aBool: Boolean; aTrue, aFalse: Integer): Integer;
    function IfThenS(aBool: Boolean; aTrue, aFalse: Single): Single;

    function InAreaI(aX, aY, aXMin, aYMin, aXMax, aYMax: Integer): Boolean;
    function InAreaS(aX, aY, aXMin, aYMin, aXMax, aYMax: Single): Boolean;

    function InRangeI(aValue, aMin, aMax: Integer): Boolean;
    function InRangeS(aValue, aMin, aMax: Single): Boolean;

    function MaxI(A, B: Integer): Integer;
    function MaxS(A, B: Single): Single;

    function MaxInArrayI(aArray: array of Integer): Integer;
    function MaxInArrayS(aArray: array of Single): Single;

    function MinI(A, B: Integer): Integer;
    function MinS(A, B: Single): Single;

    function MinInArrayI(aArray: array of Integer): Integer;
    function MinInArrayS(aArray: array of Single): Single;

    function Power(aBase, aExp: Extended): Extended;

    function Sqr(A: Extended): Extended;

    function SumI(aArray: array of Integer): Integer;
    function SumS(aArray: array of Single): Single;

    function TimeToString(aTicks: Integer): AnsiString;

  end;


implementation

uses
  KM_CommonUtils;

{ TKMScriptingUtils }


//* Version: 7000+
//* Returns absolute value of specified integer
function TKMScriptUtils.AbsI(aValue: Integer): Integer;
begin
  try
    Result := System.Abs(aValue);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns absolute value of specified single number
function TKMScriptUtils.AbsS(aValue: Single): Single;
begin
  try
    Result := System.Abs(aValue);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks how many times specified string comes in requested array
function TKMScriptUtils.ArrayElementCount(aElement: AnsiString; aArray: array of String): Integer;
var
  I: Integer;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = String(aElement) then
          Inc(Result);
    end
    else
      LogParamWarning('Utils.ArrayElementCount: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks how many times specified boolean comes in requested array
function TKMScriptUtils.ArrayElementCountB(aElement: Boolean; aArray: array of Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Inc(Result);
    end
    else
      LogParamWarning('Utils.ArrayElementCountB: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks how many times specified integer comes in requested array
function TKMScriptUtils.ArrayElementCountI(aElement: Integer; aArray: array of Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Inc(Result);
    end
    else
      LogParamWarning('Utils.ArrayElementCountI: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks how many times specified single comes in requested array
function TKMScriptUtils.ArrayElementCountS(aElement: Single; aArray: array of Single): Integer;
var
  I: Integer;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Inc(Result);
    end
    else
      LogParamWarning('Utils.ArrayElementCountS: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether requested array has specified string
function TKMScriptUtils.ArrayHasElement(aElement: AnsiString; aArray: array of String): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = String(aElement) then
          Exit(True);
    end
    else
      LogParamWarning('Utils.ArrayHasElement: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether requested array has specified boolean
function TKMScriptUtils.ArrayHasElementB(aElement: Boolean; aArray: array of Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Exit(True);
    end
    else
      LogParamWarning('Utils.ArrayHasElementB: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether requested array has specified integer
function TKMScriptUtils.ArrayHasElementI(aElement: Integer; aArray: array of Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Exit(True);
    end
    else
      LogParamWarning('Utils.ArrayHasElementI: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether requested array has specified single number
function TKMScriptUtils.ArrayHasElementS(aElement: Single; aArray: array of Single): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] = aElement then
          Exit(True);
    end
    else
      LogParamWarning('Utils.ArrayHasElementS: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns the closest to aValue integer that is in interval [aMin..aMax]
function TKMScriptUtils.EnsureRangeI(aValue, aMin, aMax: Integer): Integer;
begin
  try
    Result := Math.EnsureRange(aValue, aMin, aMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns the closest to aValue single number that is in interval [aMin..aMax]
function TKMScriptUtils.EnsureRangeS(aValue, aMin, aMax: Single): Single;
begin
  try
    Result := Math.EnsureRange(aValue, aMin, aMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Wrapper for pascal Format function
//* Formats aFormatting string with specified aData array of parameters
function TKMScriptUtils.Format(const aFormatting: string; aData: array of const): string;
begin
  try
    Result := SysUtils.Format(aFormatting, aData);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks condition aBool and returns aTrue/aFalse string depending on check result
function TKMScriptUtils.IfThen(aBool: Boolean; aTrue, aFalse: AnsiString): AnsiString;
begin
  try
    if aBool then
      Result := aTrue
    else
      Result := aFalse;
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks condition aBool and returns aTrue/aFalse integer depending on check result
function TKMScriptUtils.IfThenI(aBool: Boolean; aTrue, aFalse: Integer): Integer;
begin
  try
    Result := Math.IfThen(aBool, aTrue, aFalse);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks condition aBool and returns aTrue/aFalse single number depending on check result
function TKMScriptUtils.IfThenS(aBool: Boolean; aTrue, aFalse: Single): Single;
begin
  try
    Result := Math.IfThen(aBool, aTrue, aFalse);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* 2D variant of InRange, can be used for unit locs checks
function TKMScriptUtils.InAreaI(aX, aY, aXMin, aYMin, aXMax, aYMax: Integer): Boolean;
begin
  try
    Result := (Math.InRange(aX, aXMin, aXMax)) and Math.InRange(aY, aYMin, aYMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* 2D variant of InRange
function TKMScriptUtils.InAreaS(aX, aY, aXMin, aYMin, aXMax, aYMax: Single): Boolean;
begin
  try
    Result := (Math.InRange(aX, aXMin, aXMax)) and Math.InRange(aY, aYMin, aYMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether specified aValue is in interval [aMin..aMax]
function TKMScriptUtils.InRangeI(aValue, aMin, aMax: Integer): Boolean;
begin
  try
    Result := Math.InRange(aValue, aMin, aMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Checks whether specified aValue is in interval [aMin..aMax]
function TKMScriptUtils.InRangeS(aValue, aMin, aMax: Single): Boolean;
begin
  try
    Result := Math.InRange(aValue, aMin, aMax);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;



//* Version: 7000+
//* Returns max number of two specified
function TKMScriptUtils.MaxI(A, B: Integer): Integer;
begin
  try
    Result := Math.Max(A, B);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns max number of two specified
function TKMScriptUtils.MaxS(A, B: Single): Single;
begin
  try
    Result := Math.Max(A, B);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns max number of array elements
function TKMScriptUtils.MaxInArrayI(aArray: array of Integer): Integer;
var
  I: Integer;
begin
  try
    Result := -MaxInt;
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] > Result then
          Result := aArray[I];
    end
    else
      LogParamWarning('Utils.MaxInArrayI: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns max number of array elements
function TKMScriptUtils.MaxInArrayS(aArray: array of Single): Single;
var
  I: Integer;
begin
  try
    Result := MinSingle;
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] > Result then
          Result := aArray[I];
    end
    else
      LogParamWarning('Utils.MaxInArrayS: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns min number of two specified
function TKMScriptUtils.MinI(A, B: Integer): Integer;
begin
  try
    Result := Math.Min(A, B);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns min number of two specified
function TKMScriptUtils.MinS(A, B: Single): Single;
begin
  try
    Result := Math.Min(A, B);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns min number of array elements
function TKMScriptUtils.MinInArrayI(aArray: array of Integer): Integer;
var
  I: Integer;
begin
  try
    Result := MaxInt;
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] < Result then
          Result := aArray[I];
    end
    else
      LogParamWarning('Utils.MinInArrayI: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns min number of array elements
function TKMScriptUtils.MinInArrayS(aArray: array of Single): Single;
var
  I: Integer;
begin
  try
    Result := MaxSingle;
    if Length(aArray) > 0 then
    begin
      for I := 0 to High(aArray) do
        if aArray[I] < Result then
          Result := aArray[I];
    end
    else
      LogParamWarning('Utils.MinInArrayS: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Exponentation, base 'Base' raised to power 'Exp'.
//* F.e. Power(3, 2) = 3^2 = 9
//* Returns base "Base" raised to power "Exp"
function TKMScriptUtils.Power(aBase, aExp: Extended): Extended;
begin
  try
    Result := Math.Power(aBase, aExp);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns A^2 = A*A
function TKMScriptUtils.Sqr(A: Extended): Extended;
begin
  try
    Result := System.Sqr(A);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns sum of the elements of requested array
function TKMScriptUtils.SumI(aArray: array of Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
      for I := 0 to High(aArray) do
        Result := Result + aArray[I]
    else
      LogParamWarning('Utils.SumI: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Returns sum of the elements of requested array
function TKMScriptUtils.SumS(aArray: array of Single): Single;
begin
  Result := 0;
  try
    if Length(aArray) > 0 then
      Result := Math.Sum(aArray)
    else
      LogParamWarning('Utils.SumS: Requested array is empty',[]);
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


//* Version: 7000+
//* Converts game ticks count into string: HH:MM:SS
//* Can be used for scripted timers
function TKMScriptUtils.TimeToString(aTicks: Integer): AnsiString;
var
  H, M, S: Integer;
begin
  try
    if aTicks >= 0 then
    begin
      H := aTicks div 36000;
      M := (aTicks div 600) mod 60;
      S := (aTicks div 10) mod 60;
      Result := AnsiString(Format('%.2d:%.2d:%.2d', [H, M, S]));
    end
    else
      Result := '';
  except
    gScriptEvents.ExceptionOutsideScript := True;
    raise;
  end;
end;


end.
