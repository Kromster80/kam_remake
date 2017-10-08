unit KM_CommonUtils;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC}Forms,{$ENDIF}   //Lazarus do not know UITypes
  {$IFDEF WDC}UITypes,{$ENDIF} //We use settings in console modules
  Classes, DateUtils, Math, SysUtils, KM_Defaults, KM_Points, KM_CommonTypes
  {$IFDEF MSWindows}
  ,Windows
  ,MMSystem //Required for TimeGet which is defined locally because this unit must NOT know about KromUtils as it is not Linux compatible (and this unit is used in Linux dedicated servers)
  {$ENDIF}
  {$IFDEF Unix}
  ,unix, baseunix, UnixUtil
  {$ENDIF}
  ;

  function KMGetCursorDirection(X,Y: integer): TKMDirection;

  function GetPositionInGroup2(OriginX, OriginY: Word; aDir: TKMDirection; aIndex, aUnitPerRow: Word; MapX, MapY: Word; out aTargetCanBeReached: Boolean): TKMPoint;
  function GetPositionFromIndex(aOrigin: TKMPoint; aIndex: Byte): TKMPoint;

  function FixDelim(const aString: UnicodeString): UnicodeString;

  procedure ConvertRGB2HSB(aR, aG, aB: Integer; out oH, oS, oB: Single);
  procedure ConvertHSB2RGB(aHue, aSat, aBri: Single; out R, G, B: Byte);
  function EnsureBrightness(aColor: Cardinal; aMinBrightness: Single; aMaxBrightness: Single = 1): Cardinal;
  function MultiplyBrightnessByFactor(aColor: Cardinal; aBrightnessFactor: Single; aMinBrightness: Single = 0; aMaxBrightness: Single = 1): Cardinal;
  function ReduceBrightness(aColor: Cardinal; aBrightness: Byte): Cardinal;
  function GetColorDistance(aColor1,aColor2: Cardinal): Single;
  function GetPingColor(aPing: Word): Cardinal;
  function GetFPSColor(aFPS: Word): Cardinal;
  function FlagColorToTextColor(aColor: Cardinal): Cardinal;
  function TimeToString(aTime: TDateTime): UnicodeString;
  function WrapColor(const aText: UnicodeString; aColor: Cardinal): UnicodeString;
  function WrapColorA(const aText: AnsiString; aColor: Cardinal): AnsiString;
  function StripColor(const aText: UnicodeString): UnicodeString;
  function FindMPColor(aColor: Cardinal): Integer;

  procedure ParseDelimited(const Value, Delimiter: UnicodeString; SL: TStringList);

  procedure SetKaMSeed(aSeed: Integer);
  function GetKaMSeed: Integer;
  function KaMRandom:extended; overload;
  function KaMRandom(aMax:integer):integer; overload;
  function KaMRandomS(Range_Both_Directions:integer):integer; overload;
  function KaMRandomS(Range_Both_Directions:single):single; overload;

  function TimeGet: Cardinal;
  function GetTimeSince(aTime: Cardinal): Cardinal;
  function UTCNow: TDateTime;
  function UTCToLocal(Input: TDateTime): TDateTime;

  function MapSizeIndex(X, Y: Word): Byte;
  function MapSizeText(X,Y: Word): UnicodeString;

  //Taken from KromUtils to reduce dependancies (required so the dedicated server compiles on Linux without using Controls)
  procedure KMSwapInt(var A,B: Byte); overload;
  procedure KMSwapInt(var A,B: Shortint); overload;
  procedure KMSwapInt(var A,B: Smallint); overload;
  procedure KMSwapInt(var A,B: Word); overload;
  procedure KMSwapInt(var A,B: Integer); overload;
  procedure KMSwapInt(var A,B: Cardinal); overload;

  procedure KMSummArr(aArr1, aArr2: PKMCardinalArray);
  procedure KMSummAndEnlargeArr(aArr1, aArr2: PKMCardinalArray);

  function Pack4ByteToInteger(aByte1, aByte2, aByte3, aByte4: Byte): Integer;
  procedure UnpackIntegerTo4Byte(aInt: Integer; out aByte1, aByte2, aByte3, aByte4: Byte);

  function GetFileDirName(const aFilePath: UnicodeString): UnicodeString;

  function GetNoColorMarkupText(const aText: UnicodeString): UnicodeString;

  function DeleteDoubleSpaces(const aString: string): string;

  function CountOccurrences(const aSubstring, aText: String): Integer;
  function IntToBool(aValue: Integer): Boolean;

  //String functions
  function StrIndexOf(const aStr, aSubStr: String): Integer;
  function StrLastIndexOf(const aStr, aSubStr: String): Integer;
  function StrSubstring(const aStr: String; aFrom, aLength: Integer): String; overload;
  function StrSubstring(const aStr: String; aFrom: Integer): String; overload;
  function StrContains(const aStr, aSubStr: String): Boolean;
  function StrTrimRight(const aStr: String; aCharsToTrim: TKMCharArray): String;
  {$IFDEF WDC}
  function StrSplit(const aStr, aDelimiters: String): TStrings;
  {$ENDIF}

implementation
uses
  StrUtils, Types;

var
  fKaMSeed: Integer;


//Taken from KromUtils to reduce dependancies (required so the dedicated server compiles on Linux without using Controls)
function GetLength(A,B: Single): Single;
begin
  Result := Sqrt(Sqr(A) + Sqr(B));
end;

procedure KMSwapInt(var A,B: Byte);
var
  S: byte;
begin
  S := A; A := B; B := S;
end;

procedure KMSwapInt(var A,B: Shortint);
var
  S: Shortint;
begin
  S := A; A := B; B := S;
end;

procedure KMSwapInt(var A,B: Smallint);
var S: Smallint;
begin
  S:=A; A:=B; B:=S;
end;

procedure KMSwapInt(var A,B: Word);
var S: Word;
begin
  S:=A; A:=B; B:=S;
end;

procedure KMSwapInt(var A,B: Integer);
var S: Integer;
begin
  S:=A; A:=B; B:=S;
end;

procedure KMSwapInt(var A,B: Cardinal);
var S: cardinal;
begin
  S:=A; A:=B; B:=S;
end;


procedure KMSummArr(aArr1, aArr2: PKMCardinalArray);
var
  I: Integer;
begin
  Assert(Length(aArr1^) = Length(aArr2^), 'Arrays should have same length');
  for I := Low(aArr1^) to High(aArr1^) do
    Inc(aArr1^[I], aArr2^[I]);
end;


procedure KMSummAndEnlargeArr(aArr1, aArr2: PKMCardinalArray);
var
  I, OldLen1: Integer;
begin
  OldLen1 := Length(aArr1^);
  if OldLen1 < Length(aArr2^) then
  begin
    SetLength(aArr1^, Length(aArr2^));
    for I := OldLen1 to Length(aArr2^) - 1 do
      aArr1^[I] := 0;                     //Init array with 0
  end;

  for I := Low(aArr2^) to High(aArr2^) do
  begin
    Inc(aArr1^[I], aArr2^[I]);
  end;
end;


function Pack4ByteToInteger(aByte1, aByte2, aByte3, aByte4: Byte): Integer;
begin
  Result := (aByte1 shl 24) or (aByte2 shl 16) or (aByte3 shl 8) or aByte4;
end;


procedure UnpackIntegerTo4Byte(aInt: Integer; out aByte1, aByte2, aByte3, aByte4: Byte);
begin
  aByte1 := (aInt and $FF000000) shr 24;
  aByte2 := (aInt and $FF0000) shr 16;
  aByte3 := (aInt and $FF00) shr 8;
  aByte4 := aInt and $FF;
end;


//This unit must not know about KromUtils because it is used by the Linux Dedicated servers
//and KromUtils is not Linux compatible. Therefore this function is copied directly from KromUtils.
//Do not remove and add KromUtils to uses, that would cause the Linux build to fail
function TimeGet: Cardinal;
begin
  {$IFDEF MSWindows}
  Result := TimeGetTime; //Return milliseconds with ~1ms precision
  {$ENDIF}
  {$IFDEF Unix}
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000) mod high(Cardinal));
  {$ENDIF}
end;


function GetTimeSince(aTime: Cardinal): Cardinal;
begin
  //TimeGet will loop back to zero after ~49 days since system start
  Result := (Int64(TimeGet) - Int64(aTime) + Int64(High(Cardinal))) mod Int64(High(Cardinal));
end;


function UTCNow: TDateTime;
{$IFDEF MSWindows}
var st: TSystemTime;
begin
  GetSystemTime(st);
  Result := SystemTimeToDateTime(st);
end;
{$ENDIF}
{$IFDEF Unix}
var
  TimeVal: TTimeVal;
  TimeZone: PTimeZone;
  a: Double;
begin
  TimeZone := nil;
  fpGetTimeOfDay(@TimeVal, TimeZone);
  // Convert to milliseconds
  a := (TimeVal.tv_sec * 1000.0) + (TimeVal.tv_usec / 1000.0);
  Result := (a / MSecsPerDay) + UnixDateDelta;
end;
{$ENDIF}


function UTCToLocal(Input: TDateTime): TDateTime;
{$IFDEF WDC}
begin
  Result := TTimeZone.Local.ToLocalTime(Input);
end;
{$ENDIF}
//There seems to be no easy way to do this in FPC without an external library
//From: http://lists.lazarus.freepascal.org/pipermail/lazarus/2010-September/055568.html
{$IFDEF FPC}
var
  TZOffset: Integer;
  {$IFDEF MSWINDOWS}
  BiasType: Byte;
  TZInfo: TTimeZoneInformation;
  {$ENDIF}
begin
  Result := Input;
  {$IFDEF MSWINDOWS}
  BiasType := GetTimeZoneInformation(TZInfo);
  if (BiasType=0) then
    Exit; //No timezone so return the input

  // Determine offset in effect for DateTime UT.
  if (BiasType=2) then
    TZOffset := TZInfo.Bias + TZInfo.DaylightBias
  else
    TZOffset := TZInfo.Bias + TZInfo.StandardBias;
  {$ENDIF}
  {$IFDEF UNIX}
    TZOffset := -Tzseconds div 60;
  {$ENDIF}

  // Apply offset.
  if (TZOffset > 0) then
    // Time zones west of Greenwich.
    Result := Input - EncodeTime(TZOffset div 60, TZOffset mod 60, 0, 0)
  else if (TZOffset = 0) then
    // Time Zone = Greenwich.
    Result := Input
  else if (TZOffset < 0) then
    // Time zones east of Greenwich.
    Result := Input + EncodeTime(Abs(TZOffset) div 60, Abs(TZOffset) mod 60, 0, 0);
end;
{$ENDIF}


function MapSizeIndex(X, Y: Word): Byte;
begin
  case X * Y of
            1.. 48* 48: Result := 0;
     48* 48+1.. 80* 80: Result := 1;
     80* 80+1..128*128: Result := 2;
    128*128+1..176*176: Result := 3;
    176*176+1..224*224: Result := 4;
    224*224+1..320*320: Result := 5;
    else                Result := 6;
  end;
end;


function MapSizeText(X, Y: Word): UnicodeString;
//Pretend these are understandable in any language
const MAP_SIZES: array [0..6] of String = ('XS', 'S', 'M', 'L', 'XL', 'XXL', '???');
begin
  Result := MAP_SIZES[MapSizeIndex(X, Y)];
end;


function KMGetCursorDirection(X,Y: Integer): TKMDirection;
var Ang, Dist: Single;
begin
  Dist := GetLength(X, Y);
  if Dist > DirCursorNARadius then
  begin
    //Convert XY to angle value
    Ang := ArcTan2(Y/Dist, X/Dist) / Pi * 180;

    //Convert angle value to direction
    Result := TKMDirection((Round(Ang + 270 + 22.5) mod 360) div 45 + 1);
  end
  else
    Result := dir_NA;
end;


{Returns point where unit should be placed regarding direction & offset from Commanders position}
// 23145     231456
// 6789X     789xxx
function GetPositionInGroup2(OriginX, OriginY: Word; aDir: TKMDirection; aIndex, aUnitPerRow: Word; MapX, MapY: Word; out aTargetCanBeReached: Boolean): TKMPoint;
const
  DirAngle: array [TKMDirection] of Word   = (0, 0, 45, 90, 135, 180, 225, 270, 315);
  DirRatio: array [TKMDirection] of Single = (0, 1, 1.41, 1, 1.41, 1, 1.41, 1, 1.41);
var
  PlaceX, PlaceY, ResultX, ResultY: integer;
begin
  Assert(aUnitPerRow > 0);
  if aIndex = 0 then
  begin
    ResultX := OriginX;
    ResultY := OriginY;
  end
  else
  begin
    if aIndex <= aUnitPerRow div 2 then
      Dec(aIndex);
    PlaceX := aIndex mod aUnitPerRow - aUnitPerRow div 2;
    PlaceY := aIndex div aUnitPerRow;

    ResultX := OriginX + Round( PlaceX*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) - PlaceY*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) );
    ResultY := OriginY + Round( PlaceX*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) + PlaceY*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) );
  end;

  aTargetCanBeReached := InRange(ResultX, 1, MapX-1) and InRange(ResultY, 1, MapY-1);
  //Fit to bounds
  Result.X := EnsureRange(ResultX, 1, MapX-1);
  Result.Y := EnsureRange(ResultY, 1, MapY-1);
end;


//See Docs\GetPositionFromIndex.xls for explanation
function GetPositionFromIndex(aOrigin: TKMPoint; aIndex: Byte): TKMPoint;
const
  Rings: array[1..10] of Word =
//Ring#  1  2  3  4   5   6   7    8    9    10
        (0, 1, 9, 25, 49, 81, 121, 169, 225, 289);
var
  Ring, Span, Span2, Orig: Byte;
  Off1,Off2,Off3,Off4,Off5: Byte;
begin
  //Quick solution
  if aIndex = 0 then
  begin
    Result.X := aOrigin.X;
    Result.Y := aOrigin.Y;
    Exit;
  end;

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


//Use this function to convert platform-specific path delimiters
function FixDelim(const aString: UnicodeString): UnicodeString;
begin
  Result := StringReplace(aString, '\', PathDelim, [rfReplaceAll, rfIgnoreCase]);
end;


function GetPingColor(aPing: Word): Cardinal;
begin
  case aPing of
    0..299  : Result := clPingLow;
    300..599: Result := clPingNormal;
    600..999: Result := clPingHigh;
    else      Result := clPingCritical;
  end;
end;


function GetFPSColor(aFPS: Word): Cardinal;
begin
  case aFPS of
    0..9  : Result := clFpsCritical;
    10..12: Result := clFpsLow;
    13..15: Result := clFpsNormal;
    else    Result := clFpsHigh;
  end;
end;


procedure ConvertRGB2HSB(aR, aG, aB: Integer; out oH, oS, oB: Single);
var
  R, G, B: Single;
  Rdlt, Gdlt, Bdlt, Vmin, Vmax, Vdlt: Single;
begin
  R := aR / 255;
  G := aG / 255;
  B := aB / 255;

  Vmin := Math.min(R, Math.min(G, B));
  Vmax := Math.max(R, Math.max(G, B));
  Vdlt := Vmax - Vmin;
  oB := (Vmax + Vmin) / 2;
  if Vdlt = 0 then
  begin
    oH := 0.5;
    oS := 0;
  end
  else
  begin // Middle of HSImage
    if oB < 0.5 then
      oS := Vdlt / (Vmax + Vmin)
    else
      oS := Vdlt / (2 - Vmax - Vmin);

    Rdlt := (R - Vmin) / Vdlt;
    Gdlt := (G - Vmin) / Vdlt;
    Bdlt := (B - Vmin) / Vdlt;

    if R = Vmax then oH := (Gdlt - Bdlt) / 6 else
    if G = Vmax then oH := 1/3 - (Rdlt - Bdlt) / 6 else
    if B = Vmax then oH := 2/3 - (Gdlt - Rdlt) / 6 else
                      oH := 0;

    if oH < 0 then oH := oH + 1;
    if oH > 1 then oH := oH - 1;
  end;
end;


procedure ConvertHSB2RGB(aHue, aSat, aBri: Single; out R, G, B: Byte);
const V = 6;
var Hue, Sat, Bri, Rt, Gt, Bt: Single;
begin
  Hue := EnsureRange(aHue, 0, 1);
  Sat := EnsureRange(aSat, 0, 1);
  Bri := EnsureRange(aBri, 0, 1);

  //Hue
  if Hue < 1/6 then
  begin
    Rt := 1;
    Gt := Hue * V;
    Bt := 0;
  end else
  if Hue < 2/6 then
  begin
    Rt := (2/6 - Hue) * V;
    Gt := 1;
    Bt := 0;
  end else
  if Hue < 3/6 then
  begin
    Rt := 0;
    Gt := 1;
    Bt := (Hue - 2/6) * V;
  end else
  if Hue < 4/6 then
  begin
    Rt := 0;
    Gt := (4/6 - Hue) * V;
    Bt := 1;
  end else
  if Hue < 5/6 then
  begin
    Rt := (Hue - 4/6) * V;
    Gt := 0;
    Bt := 1;
  end else
  //if Hue < 6/6 then
  begin
    Rt := 1;
    Gt := 0;
    Bt := (6/6 - Hue) * V;
  end;

  //Saturation
  Rt := Rt + (0.5 - Rt) * (1 - Sat);
  Gt := Gt + (0.5 - Gt) * (1 - Sat);
  Bt := Bt + (0.5 - Bt) * (1 - Sat);

  //Brightness
  if Bri > 0.5 then
  begin
    //Mix with white
    Rt := Rt + (1 - Rt) * (Bri - 0.5) * 2;
    Gt := Gt + (1 - Gt) * (Bri - 0.5) * 2;
    Bt := Bt + (1 - Bt) * (Bri - 0.5) * 2;
  end
  else if Bri < 0.5 then
  begin
    //Mix with black
    Rt := Rt * (Bri * 2);
    Gt := Gt * (Bri * 2);
    Bt := Bt * (Bri * 2);
  end;
  //if Bri = 127 then color remains the same

  R := Round(Rt * 255);
  G := Round(Gt * 255);
  B := Round(Bt * 255);
end;


//Reduce brightness
//aBrightness - from 0 to 255, where 255 is current Brightness
function ReduceBrightness(aColor: Cardinal; aBrightness: Byte): Cardinal;
begin
  Result := Round((aColor and $FF) / 255 * aBrightness)
            or
            Round((aColor shr 8 and $FF) / 255 * aBrightness) shl 8
            or
            Round((aColor shr 16 and $FF) / 255 * aBrightness) shl 16
            or
            (aColor and $FF000000);
end;


function GetColorDistance(aColor1,aColor2: Cardinal): Single;
var
  R1,G1,B1,A1,R2,G2,B2,A2: Single;
begin
  R1 := (aColor1 and $FF) / 255;
  G1 := (aColor1 shr 8 and $FF) / 255;
  B1 := (aColor1 shr 16 and $FF) / 255;
  A1 := (aColor1 shr 24 and $FF) / 255;

  R2 := (aColor2 and $FF) / 255;
  G2 := (aColor2 shr 8 and $FF) / 255;
  B2 := (aColor2 shr 16 and $FF) / 255;
  A2 := (aColor2 shr 24 and $FF) / 255;

  Result := Sqrt(Sqr(R1 - R2) + Sqr(G1 - G2) + Sqr(B1 - B2) + Sqr(A1 - A2));
end;


function EnsureBrightness(aColor: Cardinal; aMinBrightness: Single; aMaxBrightness: Single = 1): Cardinal;
begin
  Result := MultiplyBrightnessByFactor(aColor, 1, aMinBrightness, aMaxBrightness);
end;


function MultiplyBrightnessByFactor(aColor: Cardinal; aBrightnessFactor: Single; aMinBrightness: Single = 0; aMaxBrightness: Single = 1): Cardinal;
var
  R, G, B: Byte;
  Hue, Sat, Bri: Single;
begin
  ConvertRGB2HSB(aColor and $FF, aColor shr 8 and $FF, aColor shr 16 and $FF, Hue, Sat, Bri);
  Bri := EnsureRange(Bri*aBrightnessFactor, aMinBrightness, aMaxBrightness);
  ConvertHSB2RGB(Hue, Sat, Bri, R, G, B);

  //Preserve transparency value
  Result := (R + G shl 8 + B shl 16) or (aColor and $FF000000);
end;


//Desaturate and lighten the color best done in HSB colorspace
function FlagColorToTextColor(aColor: Cardinal): Cardinal;
var
  R, G, B: Byte;
  Hue, Sat, Bri: Single;
begin
  ConvertRGB2HSB(aColor and $FF, aColor shr 8 and $FF, aColor shr 16 and $FF, Hue, Sat, Bri);

  //Desaturate and lighten
  Sat := Math.Min(Sat, 0.93);
  Bri := Math.Max(Bri + 0.1, 0.2);
  ConvertHSB2RGB(Hue, Sat, Bri, R, G, B);

  //Preserve transparency value
  Result := (R + G shl 8 + B shl 16) or (aColor and $FF000000);
end;


//Convert DateTime to string xx:xx:xx where hours have at least 2 digits
//F.e. we can have 72:12:34 for 3 days long game
function TimeToString(aTime: TDateTime): UnicodeString;
begin
  //We can't use simple Trunc(aTime * 24 * 60 * 60) maths because it is prone to rounding errors
  //e.g. 3599 equals to 59:58 and 3600 equals to 59:59
  //That is why we resort to DateUtils routines which are slower but much more correct
  Result :=  Format('%.2d', [HoursBetween(aTime, 0)]) + FormatDateTime(':nn:ss', aTime);
end;


//Make a string wrapped into color code
function WrapColor(const aText: UnicodeString; aColor: Cardinal): UnicodeString;
begin
  Result := '[$' + IntToHex(aColor and $00FFFFFF, 6) + ']' + aText + '[]';
end;


function WrapColorA(const aText: AnsiString; aColor: Cardinal): AnsiString;
begin
  Result := '[$' + AnsiString(IntToHex(aColor and $00FFFFFF, 6) + ']') + aText + '[]';
end;


function StripColor(const aText: UnicodeString): UnicodeString;
var
  I: Integer;
  skippingMarkup: Boolean;
begin
  Result := '';
  skippingMarkup := False;

  for I := 1 to Length(aText) do
  begin
    if (I+1 <= Length(aText))
    and ((aText[I] + aText[I+1] = '[$') or (aText[I] + aText[I+1] = '[]')) then
      skippingMarkup := True;

    if not skippingMarkup then
      Result := Result + aText[I];

    if skippingMarkup and (aText[I] = ']') then
      skippingMarkup := False;
  end;
end;


function FindMPColor(aColor: Cardinal): Integer;
var I: Integer;
begin
  Result := 0;
  for I := Low(MP_TEAM_COLORS) to High(MP_TEAM_COLORS) do
    if MP_TEAM_COLORS[I] = aColor then
      Result := I;
end;


//Taken from: http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
procedure ParseDelimited(const Value, Delimiter: UnicodeString; SL: TStringList);
var
  dx: Integer;
  ns: UnicodeString;
  txt: UnicodeString;
  Delta: Integer;
begin
  Delta := Length(Delimiter);
  txt := Value + Delimiter;
  SL.BeginUpdate;
  SL.Clear;
  try
    while Length(txt) > 0 do
    begin
      dx := Pos(Delimiter, txt);
      ns := Copy(txt, 0, dx-1);
      SL.Add(ns);
      txt := Copy(txt, dx+Delta, MaxInt);
    end;
  finally
    SL.EndUpdate;
  end;
end;


//Quote from page 5 of 'Random Number Generators': "We recommend the construction of an initialization procedure,
//Randomize, which prompts for an initial value of seed and forces it to be an integer between 1 and 2^31 - 2."
procedure SetKaMSeed(aSeed: Integer);
begin
  Assert(InRange(aSeed, 1, 2147483646), 'KaMSeed initialised incorrectly: ' + IntToStr(aSeed));
  if CUSTOM_RANDOM then
    fKaMSeed := aSeed
  else
    RandSeed := aSeed;
end;


function GetKaMSeed: Integer;
begin
  if CUSTOM_RANDOM then
    Result := fKaMSeed
  else
    Result := RandSeed;
end;


//Taken from "Random Number Generators" by Stephen K. Park and Keith W. Miller.
(*  Integer  Version  2  *)
function KaMRandom: Extended;
const
  A = 16807;
  M = 2147483647; //Prime number 2^31 - 1
  Q = 127773; // M div A
  R = 2836; // M mod A
var
  C1, C2, NextSeed: integer;
begin
  if not CUSTOM_RANDOM then
  begin
    Result := Random;
    Exit;
  end;

  Assert(InRange(fKaMSeed,1,M-1), 'KaMSeed initialised incorrectly: '+IntToStr(fKaMSeed));
  C2 := fKaMSeed div Q;
  C1 := fKaMSeed mod Q;
  NextSeed := A*C1 - R*C2;

  if NextSeed > 0 then
    fKaMSeed := NextSeed
  else
    fKaMSeed := NextSeed + M;

  Result := fKaMSeed / M;
end;


function KaMRandom(aMax:integer):integer;
begin
  if CUSTOM_RANDOM then
    Result := trunc(KaMRandom*aMax)
  else
    Result := Random(aMax);
end;


function KaMRandomS(Range_Both_Directions:integer):integer; overload;
begin
  Result := KaMRandom(Range_Both_Directions*2+1)-Range_Both_Directions;
end;


function KaMRandomS(Range_Both_Directions:single):single; overload;
begin
  Result := KaMRandom(round(Range_Both_Directions*20000)+1)/10000-Range_Both_Directions;
end;


// Returns file directory name
// F.e. for aFilePath = 'c:/kam/remake/fore.ver' returns 'remake'
function GetFileDirName(const aFilePath: UnicodeString): UnicodeString;
var DirPath: UnicodeString;
begin
  Result := '';
  if Trim(aFilePath) = '' then Exit;

  DirPath := ExtractFileDir(aFilePath);

  if DirPath = '' then Exit;

  if StrIndexOf(DirPath, PathDelim) <> -1 then
    Result := copy(DirPath, StrLastIndexOf(DirPath, PathDelim) + 2);
end;


// Returnes text ignoring color markup [$FFFFFF][]
function GetNoColorMarkupText(const aText: UnicodeString): UnicodeString;
var I, TmpColor: Integer;
begin
  Result := '';

  if aText = '' then Exit;

  I := 1;
  while I <= Length(aText) do
  begin
    //Ignore color markups [$FFFFFF][]
    if (aText[I]='[') and (I+1 <= Length(aText)) and (aText[I+1]=']') then
      Inc(I) //Skip past this markup
    else
      if (aText[I]='[') and (I+8 <= Length(aText))
      and (aText[I+1] = '$') and (aText[I+8]=']')
      and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
        Inc(I,8) //Skip past this markup
      else
        //Not markup so count width normally
        Result := Result + aText[I];
    Inc(I);
  end;
end;


//Replace continious spaces with single space
function DeleteDoubleSpaces(const aString: string): string;
var I: Integer;
begin
  Result := '';
  if aString = '' then Exit;
  Result := aString[1];

  for I := 2 to Length(aString) do
  begin
    if aString[I] = ' ' then
    begin
      if not (aString[I-1] = ' ') then
        Result := Result + ' ';
    end else
      Result := Result + aString[I];
  end;
end;


function CountOccurrences(const aSubstring, aText: String): Integer;
var
  Offset: integer;
begin
  Result := 0;
  Offset := PosEx(aSubstring, aText, 1);
  while Offset <> 0 do
  begin
    Inc(Result);
    Offset := PosEx(aSubstring, aText, Offset + length(aSubstring));
  end;
end;


function IntToBool(aValue: Integer): Boolean;
begin
  Result := aValue <> 0;
end;


{
String functions
These function are replacements for String functions introduced after XE2 (XE5 probably)
Names are the same as in new Delphi versions, but with 'Str' prefix
}
function StrIndexOf(const aStr, aSubStr: String): Integer;
begin
  //Todo refactor:
  //@Krom: Why not just replace StrIndexOf with Pos everywhere in code?
  Result := AnsiPos(aSubStr, aStr) - 1;
end;


function StrLastIndexOf(const aStr, aSubStr: String): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 1 to Length(aStr) do
    if AnsiPos(aSubStr, StrSubstring(aStr, I-1)) <> 0 then
      Result := I - 1;
end;


function StrSubstring(const aStr: String; aFrom: Integer): String;
begin
  //Todo refactor:
  //@Krom: Why not just replace StrSubstring with RightStr everywhere in code?
  Result := Copy(aStr, aFrom + 1, Length(aStr));
end;


function StrSubstring(const aStr: String; aFrom, aLength: Integer): String;
begin
  //Todo refactor:
  //@Krom: Why not just replace StrSubstring with Copy everywhere in code?
  Result := Copy(aStr, aFrom + 1, aLength);
end;


function StrContains(const aStr, aSubStr: String): Boolean;
begin
  //Todo refactor:
  //@Krom: Why not just replace StrContains with Pos() <> 0 everywhere in code?
  Result := StrIndexOf(aStr, aSubStr) <> -1;
end;


function StrTrimRight(const aStr: String; aCharsToTrim: TKMCharArray): String;
var Found: Boolean;
    I, J: Integer;
begin
  for I := Length(aStr) downto 1 do
  begin
    Found := False;
    for J := Low(aCharsToTrim) to High(aCharsToTrim) do
    begin
      if aStr[I] = aCharsToTrim[J] then
      begin
        Found := True;
        Break;
      end;
    end;
    if not Found then
      Break;
  end;
  Result := Copy(aStr, 1, I);
end;


{$IFDEF WDC}
function StrSplit(const aStr, aDelimiters: String): TStrings;
var StrArray: TStringDynArray;
    I: Integer;
begin
  //Todo refactor:
  //@Krom: It's bad practice to create object (TStringList) inside and return it as parent class (TStrings).
  //Do we really need it this way? Better to pass TStringList from outside in a parameter.

  StrArray := SplitString(aStr, aDelimiters);
  Result := TStringList.Create;
  for I := Low(StrArray) to High(StrArray) do
    Result.Add(StrArray[I]);
end;
{$ENDIF}


end.
