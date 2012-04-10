unit KM_Utils;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils, KM_Defaults, KM_Points;

  function KMGetCursorDirection(X,Y: integer): TKMDirection;

  function GetPositionInGroup2(OriginX, OriginY:integer; aDir:TKMDirection; aI, aUnitPerRow:integer; MapX,MapY:integer; out aTargetCanBeReached:boolean):TKMPoint;
  function GetPositionFromIndex(aOrigin:TKMPoint; aIndex:byte):TKMPointI;

  function MapNameToPath(const aMapName, aExtension:string; aIsMultiplayer:boolean):string;
  function FixDelim(const aString:string):string;

  function GetPingColor(aPing:word):cardinal;
  function FlagColorToTextColor(aColor: Cardinal): Cardinal;

  procedure ParseDelimited(const SL: TStringList; const Value: string; const Delimiter: string);

  procedure SetKaMSeed(aSeed:integer);
  function GetKaMSeed:integer;
  function KaMRandom:extended; overload;
  function KaMRandom(aMax:integer):integer; overload;
  function KaMRandomS(Range_Both_Directions:integer):integer; overload;
  function KaMRandomS(Range_Both_Directions:single):single; overload;
  {$IFDEF Unix}
  function FakeGetTickCount: DWord;
  {$ENDIF}

implementation


var
  fKaMSeed: Integer;


//Taken from KromUtils to reduce dependancies (required so the dedicated server compiles on Linu without using Controls)
function GetLength(A, B: Single): Single;
begin
  Result := Sqrt(Sqr(A) + Sqr(B));
end;


function int2fix(Number,Len:integer):string;
var ss:string; x:byte;
begin
  ss := inttostr(Number);
  for x:=length(ss) to Len-1 do
    ss := '0' + ss;
  if length(ss)>Len then
    ss:='**********';//ss[99999999]:='0'; //generating an error in lame way
  setlength(ss, Len);
  Result := ss;
end;

//This is a fake GetTickCount for Linux (Linux does not have one)
{$IFDEF Unix}
function FakeGetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF}


//Look for last dot and truncate it
function TruncateExt(FileName:string): string;
var i:word; DotPlace:word;
begin

  DotPlace := length(FileName) + 1; //In case there's no Extension
  for i:=1 to length(FileName) do
    if FileName[i] = '.' then //FileExtension separator is always a .
      DotPlace := i;

  Result := Copy(FileName, 1, DotPlace - 1);
end;


function KMGetCursorDirection(X,Y: integer): TKMDirection;
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
function GetPositionInGroup2(OriginX, OriginY:integer; aDir:TKMDirection; aI, aUnitPerRow:integer; MapX,MapY:integer; out aTargetCanBeReached:boolean):TKMPoint;
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

  aTargetCanBeReached := InRange(ResultX, 1, MapX-1) and InRange(ResultY, 1, MapY-1);
  //Fit to bounds
  Result.X := EnsureRange(ResultX, 1, MapX-1);
  Result.Y := EnsureRange(ResultY, 1, MapY-1);
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


function MapNameToPath(const aMapName, aExtension:string; aIsMultiplayer:boolean):string;
begin
  if aIsMultiplayer then Result := ExeDir+'MapsMP\'
                    else Result := ExeDir+'Maps\';
  Result := Result+aMapName+'\'+aMapName+'.'+aExtension;
end;


//Use this function to convert platform-specific path delimiters
function FixDelim(const aString: string): string;
begin
  Result := StringReplace(aString, '\', PathDelim, [rfReplaceAll, rfIgnoreCase]);
end;


function GetPingColor(aPing: Word): Cardinal;
begin
  case aPing of
    0..299  : Result := icGreen;
    300..599: Result := icYellow;
    600..999: Result := icOrange;
    else      Result := icRed;
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

  Vmin := min(R, min(G, B));
  Vmax := max(R, max(G, B));
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


//Desaturate and lighten the color best done in HSB colorspace
function FlagColorToTextColor(aColor: Cardinal): Cardinal;
var
  R, G, B: Byte;
  Hue, Sat, Bri: Single;
begin
  ConvertRGB2HSB(aColor and $FF, aColor shr 8 and $FF, aColor shr 16 and $FF, Hue, Sat, Bri);

  //Desaturate and lighten
  Sat := Min(Sat, 0.93);
  Bri := Max(Bri + 0.1, 0.2);
  ConvertHSB2RGB(Hue, Sat, Bri, R, G, B);

  //Preserve transparency value
  Result := (R + G shl 8 + B shl 16) or (aColor and $FF000000);
end;


//Taken from: http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
procedure ParseDelimited(const SL: TStringList; const Value: string; const Delimiter: string);
var
  dx: integer;
  ns: string;
  txt: string;
  Delta: integer;
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
procedure SetKaMSeed(aSeed:integer);
begin
  assert(InRange(aSeed,1,2147483646),'KaMSeed initialised incorrectly: '+IntToStr(aSeed));
  if CUSTOM_RANDOM then
    fKaMSeed := aSeed
  else
    RandSeed := aSeed;
end;


function GetKaMSeed:integer;
begin
  if CUSTOM_RANDOM then
    Result := fKaMSeed
  else
    Result := RandSeed;
end;


//Taken from "Random Number Generators" by Stephen K. Park and Keith W. Miller.
(*  Integer  Version  2  *)
function KaMRandom:extended;
const
  A = 16807;
  M = 2147483647; //Prime number 2^31 - 1
  Q = 127773; // M div A
  R = 2836; // M mod A
var
  C1, C2, NextSeed: integer;
begin
  if not CUSTOM_RANDOM then begin
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


end.
