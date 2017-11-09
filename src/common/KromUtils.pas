unit KromUtils;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Controls, Dialogs, ExtCtrls, Forms, SysUtils, Classes
  {$IFDEF MSWindows} ,Windows, MMSystem {$ENDIF}
  {$IFDEF Unix} ,LCLType {$ENDIF}
  {$IFDEF FPC} , LCLIntf, UTF8Process, LazHelpHTML {$ENDIF}
  {$IFDEF WDC} , ShellApi {$ENDIF}
  ;

type
  TAnsiStringArray = array of AnsiString;
  TUnicodeStringArray = array of UnicodeString;
  TSetOfAnsiChar = set of AnsiChar;


function TimeGet: Cardinal;
function ExtractOpenedFileName(in_s: string):string;
function GetFileExt (const FileName: string): string;
function AssureFileExt(FileName,Ext:string): string;
function TruncateExt(FileName:string): string;
function GetFileSize(const FileName: string): LongInt;
function CheckSameContents(A, B: string): Boolean;
function CharInSet(C: AnsiChar; S: TSetOfAnsiChar): Boolean; overload;
function CharInSet(C: WideChar; S: TSetOfAnsiChar): Boolean; overload;

procedure FreeThenNil(var Obj);

function ReverseString(s1:string):string;

function int2fix(Number,Len:integer):string;

function Min(const A,B,C: integer):integer; overload;
function Min(const A,B,C: single):single; overload;
function Min(Values: array of Single): Single; overload;
function PickMin(Values: array of Single): Byte;

function Max(const A,B,C: integer):integer; overload;
function Max(const A,B,C: single):single; overload;

  function GetLengthSQR(ix,iy,iz:integer): integer; //Length without SQRT
  function GetLength(ix,iy,iz:single): single; overload;
  function GetLength(ix,iy:single): single; overload;

  function Mix(x1,x2,MixValue:single):single; overload;
  function Mix(x1,x2:integer; MixValue:single):integer; overload;
  function Lerp(A,B: Single; MixValue: Single): Single;

  procedure decs(var AText: string; const Len: integer = 1); overload;
  procedure decs(var AText: widestring; const Len: integer = 1); overload;
  function decs(AText: string; Len, RunAsFunction: integer): string; overload;
  function RemoveQuotes(Input: string): string;
  procedure SwapStr(var A, B: string);
  procedure SwapInt(var A, B: Byte); overload;
  procedure SwapInt(var A, B: ShortInt); overload;
  procedure SwapInt(var A, B: SmallInt); overload;
  procedure SwapInt(var A, B: Word); overload;
  procedure SwapInt(var A, B: Integer); overload;
  procedure SwapInt(var A, B: Cardinal); overload;
  {$IFDEF WDC}
  procedure SwapInt(var A, B: NativeUInt); overload;
  {$ENDIF}
  procedure SwapFloat(var A, B: Single);
  function Equals(A, B: single; const Epsilon: single = 0.001): Boolean;

function MakePOT(num:integer): integer;
function Adler32CRC(TextPointer:Pointer; TextLength:cardinal):cardinal; overload;
function Adler32CRC(const aPath:string):cardinal; overload;
function Adler32CRC(S:TMemoryStream):cardinal; overload;
function RandomS(Range_Both_Directions:integer):integer; overload;
function RandomS(Range_Both_Directions:single):single; overload;
function PseudoRandom(aMax:cardinal):cardinal;
function RunOpenDialog(Sender:TOpenDialog; Name,Path,Filter:string):boolean;
function RunSaveDialog(Sender:TSaveDialog; FileName, FilePath, Filter:string; const FileExt:string = ''):boolean;

procedure DoClientAreaResize(aForm:TForm);

function CheckDuplicateApplication(aGUID: string): Boolean;

function BrowseURL(const URL: string) : Boolean;
function OpenPDF(const URL: string): Boolean;
procedure MailTo(Address,Subject,Body:string);
procedure OpenMySite(ToolName:string; Address:string='http://krom.reveur.de');
function CountMatches(const aString, aSubString: string): Integer;

const
  EolA: AnsiString = #13#10; //EndOfLine
  EolW: UnicodeString = #0013#0010; //EndOfLine


implementation


function Min(const A,B,C: integer): integer; overload;
begin
  if A < B then if A < C then Result := A else Result := C
           else if B < C then Result := B else Result := C;
end;

function Min(const A,B,C: single): single; overload;
begin
  if A < B then if A < C then Result := A else Result := C
           else if B < C then Result := B else Result := C;
end;


function Min(Values: array of Single): Single;
var
  I: Integer;
begin
  Result := Values[0];
  for I := 1 to High(Values) do
  if Values[I] < Result then
    Result := Values[I];
end;


//Pick minimum value and return its index
//if two or more values are equal then first in list is returned
function PickMin(Values: array of Single): Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to High(Values) do
  if Values[I] < Values[Result] then
    Result := I;
end;


function Max(const A,B,C: integer): integer; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;

function Max(const A,B,C: single): single; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;


//I re-add this it is required by KM_Editor.
function ExtractOpenedFileName(in_s: string): string;
var k:word; out_s:string; QMarks:boolean;
begin
k:=0; out_s:=''; QMarks:=false;

repeat      //First of all skip exe path
inc(k);
  if in_s[k]='"' then
  repeat inc(k);
  until(in_s[k]='"');
until((k>=length(in_s))or(in_s[k]=#32));  //in_s[k]=#32 now

inc(k);     //in_s[k]=" or first char

if (length(in_s)>k)and(in_s[k]=#32) then //Skip doublespace, WinXP bug ?
    repeat
    inc(k);
    until((k>=length(in_s))or(in_s[k]<>#32));

if (length(in_s)>k) then begin

    if in_s[k]='"' then begin
    inc(k); //Getting path from "...."
    QMarks:=true;
    end;

    repeat
    out_s:=out_s+in_s[k];
    inc(k);
    until((length(in_s)=k-1)or(in_s[k]='"')or((QMarks=false)and(in_s[k]=' ')));

end else out_s:='';

Result:=out_s;
end;


//Linux wants this instead of timegettime, it should work on Windows too
function TimeGet: Cardinal;
begin
  {$IFDEF MSWindows}
  Result := TimeGetTime; //Returns milliseconds with ~1ms precision
  {$ENDIF}
  {$IFDEF Unix}
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
  {$ENDIF}
end;


//Returns file extension without dot
function GetFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if length(Result)>0 then
    Result := UpperCase(Copy(Result, 2, length(Result)-1))
  else
    Result := '';
end;


function AssureFileExt(FileName,Ext:string): string;
begin
if (Ext='')or(GetFileExt(FileName)=UpperCase(Ext)) then
  Result:=FileName
else
  Result:=FileName+'.'+Ext;
end;


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


function GetFileSize(const FileName: string): LongInt;
var
  SearchRec: TSearchRec;
begin
  try
    if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
      Result := SearchRec.Size
    else
      Result := -1;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;


function CheckSameContents(A, B: string): Boolean;
var S1, S2: TMemoryStream; I: Cardinal;
begin
  Result := FileExists(A) and FileExists(B);

  if Result then
  begin
    S1 := TMemoryStream.Create;
    S1.LoadFromFile(A);
    S2 := TMemoryStream.Create;
    S2.LoadFromFile(B);

    Result := (S1.Size = S2.Size);

    if Result then
    begin
      I := 0;
      repeat
        Result := PByte(Cardinal(S1.Memory) + I)^ = PByte(Cardinal(S2.Memory) + I)^;
        Inc(I);
      until (not Result or (I = S1.Size));
    end;

    S1.Free;
    S2.Free;
  end;
end;


function CharInSet(C: AnsiChar; S: TSetOfAnsiChar): Boolean;
begin
  Result := C in S;
end;


function CharInSet(C: WideChar; S: TSetOfAnsiChar): Boolean;
begin
  Result := (Ord(C) <= MAXBYTE) and (AnsiChar(C) in S);
end;


procedure FreeThenNil(var Obj);
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;


function ReverseString(s1: string): string;
var
  s2: string;
  i: integer;
begin
  s2 := s1; // preparing ?
  for i := 1 to length(s1) do
    s2[i] := s1[length(s1) - i + 1];
  ReverseString := s2;
end;


function int2fix(Number, Len: integer): string;
var
  ss: string;
  x: byte;
begin
  ss := inttostr(Number);
  for x := length(ss) to Len - 1 do
    ss := '0' + ss;
  if length(ss) > Len then
    ss := '**********'; // ss[99999999]:='0'; //generating an error in lame way
  setlength(ss, Len);
  Result := ss;
end;


// Return closest bigger PowerOfTwo number
function MakePOT(num: integer): integer;
begin
  num := num - 1;
  num := num OR (num SHR 1);
  num := num OR (num SHR 2);
  num := num OR (num SHR 4);
  num := num OR (num SHR 8);
  num := num OR (num SHR 16); // 32bit needs no more
  Result := num + 1;
end;


function GetLengthSQR(ix, iy, iz: integer): integer;
begin
  Result := sqr(ix) + sqr(iy) + sqr(iz);
end;


function GetLength(ix, iy, iz: single): single; overload;
begin
  Result := sqrt(sqr(ix) + sqr(iy) + sqr(iz));
end;


function GetLength(ix, iy: single): single; overload;
begin
  Result := sqrt(sqr(ix) + sqr(iy));
end;


function Mix(x1, x2, MixValue: single): single; overload;
begin
  Result := x1 * MixValue + x2 * (1 - MixValue);
end;


function Mix(x1, x2: integer; MixValue: single): integer; overload;
begin
  Result := round(x1 * MixValue + x2 * (1 - MixValue));
end;


function Lerp(A,B: Single; MixValue: Single): Single;
begin
  Result := A + (B - A) * MixValue;
end;


procedure decs(var AText: string; const Len: integer = 1);
begin
  if length(AText) <= abs(Len) then
    AText := ''
  else if Len >= 0 then
    AText := Copy(AText, 1, length(AText) - Len)
  else
    AText := Copy(AText, 1 + abs(Len), length(AText) - abs(Len));
end;


procedure decs(var AText: widestring; const Len: integer = 1);
begin
  if length(AText) <= abs(Len) then
    AText := ''
  else if Len >= 0 then
    AText := Copy(AText, 1, length(AText) - Len)
  else
    AText := Copy(AText, 1 + abs(Len), length(AText) - abs(Len));
end;


function decs(AText: string; Len, RunAsFunction: integer): string; overload;
begin
  if length(AText) <= abs(Len) then
    Result := ''
  else if Len >= 0 then
    Result := Copy(AText, 1, length(AText) - Len)
  else
    Result := Copy(AText, 1 + abs(Len), length(AText) - abs(Len));
end;


function RemoveQuotes(Input: string): string;
var
  i, k: integer;
begin
  Result := '';
  k := 1;
  while (Input[k] <> '"') and (k <= length(Input)) do
    inc(k);
  if k = length(Input) then
    exit; // No quotes found

  for i := k + 1 to length(Input) do
    if Input[i] <> '"' then
      Result := Result + Input[i]
    else
      exit; // Will exit on first encountered quotes from 2nd character
end;


procedure SwapStr(var A, B: string);
var
  S: string;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A, B: byte);
var
  S: byte;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A, B: shortint);
var
  S: shortint;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A,B:smallint);
var s:smallint;
begin
  s:=A; A:=B; B:=s;
end;


procedure SwapInt(var A,B:word);
var s:word;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B: Integer);
var s:integer;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B: Cardinal);
var s:cardinal;
begin
  s:=A; A:=B; B:=s;
end;

{$IFDEF WDC}
procedure SwapInt(var A,B: NativeUInt);
var s:NativeUInt;
begin
  s:=A; A:=B; B:=s;
end;
{$ENDIF}

procedure SwapFloat(var A,B:single);
var s:single;
begin
  s:=A; A:=B; B:=s;
end;


function Equals(A,B:single; const Epsilon:single=0.001):boolean;
begin
  Result := abs(A-B) <= Epsilon;
end;


function Adler32CRC(TextPointer: Pointer; TextLength: Cardinal): Cardinal;
var
  i, A, B: Cardinal;
begin
  A := 1;
  B := 0; // A is initialized to 1, B to 0
  for i := 1 to TextLength do
  begin
    inc(A, pbyte(Cardinal(TextPointer) + i - 1)^);
    inc(B, A);
  end;
  A := A mod 65521; // 65521 (the largest prime number smaller than 2^16)
  B := B mod 65521;
  Adler32CRC := B + A shl 16; // reverse order for smaller numbers
end;


function Adler32CRC(const aPath: string): Cardinal;
var
  S: TMemoryStream;
begin
  Result := 0;
  if not FileExists(aPath) then
    exit;

  S := TMemoryStream.Create;
  try
    S.LoadFromFile(aPath);
    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


function Adler32CRC(S: TMemoryStream): Cardinal;
var
  i, A, B: Cardinal;
begin
  A := 1;
  B := 0; // A is initialized to 1, B to 0

  // We need to MOD B within cos it may overflow in files larger than 65kb, A overflows with files larger than 16mb
  if S.Size <> 0 then
    for i := 0 to S.Size - 1 do
    begin
      inc(A, pbyte(Cardinal(S.Memory) + i)^);
      B := (B + A) mod 65521; // 65521 (the largest prime number smaller than 2^16)
    end;
  A := A mod 65521;
  Result := B + A shl 16; // reverse order for smaller numbers
end;


function RandomS(Range_Both_Directions: integer): integer; overload;
begin
  Result := Random(Range_Both_Directions * 2 + 1) - Range_Both_Directions;
end;


function RandomS(Range_Both_Directions: single): single; overload;
begin
  Result := Random(round(Range_Both_Directions * 20000) + 1) / 10000 - Range_Both_Directions;
end;


//Return Random number without disturbing RandomNumberGenerator
//we need to use it in case where Random should return repeating series of numbers
//from time to time with the same RandSeed, e.g. when AI logic depends on Randoms
//and some of player input needs Random too, but it should not affect AI
function PseudoRandom(aMax: Cardinal): Cardinal;
begin
  if aMax = 0 then
    Result := 0
  else
    Result := TimeGet mod aMax;
end;


function RunOpenDialog(Sender: TOpenDialog; Name, Path, Filter: string): boolean;
begin
  Sender.FileName := Name;
  Sender.InitialDir := Path;
  Sender.Filter := Filter;
  Result := Sender.Execute; // Returns "false" if user pressed "Cancel"
  //Result := Result and FileExists(Sender.FileName); //Already should be enabled in OpenDialog options
end;


function RunSaveDialog(Sender:TSaveDialog; FileName, FilePath, Filter:string; const FileExt:string = ''):boolean;
begin
  Sender.FileName   := FileName;
  Sender.InitialDir := FilePath;
  Sender.Filter     := Filter;
  Result            := Sender.Execute; //Returns "false" if user pressed "Cancel"
  if not Result then exit;
  Sender.FileName   := AssureFileExt(Sender.FileName, FileExt);
end;


//Each time we run the check we create an unique Mutex
//Thus we can check the Mutex and see if another copy of our application is run
//Win will automatically release the Mutex on application exit
function CheckDuplicateApplication(aGUID: string): Boolean;
begin
  {$IFDEF MSWindows}
    if CreateMutex(nil, True, PChar(aGUID)) = 0 then
      RaiseLastOSError;
    Result := (GetLastError = ERROR_ALREADY_EXISTS);
  {$ENDIF}
  {$IFDEF Unix}
    Result := False;
  {$ENDIF}
end;


procedure DoClientAreaResize(aForm:TForm);
const DesignHeight = 18;
var
  HeightDif:integer;
  i:integer;
begin
  HeightDif := GetSystemMetrics(SM_CYCAPTION) - DesignHeight;

  for i:=0 to aForm.ControlCount-1 do
    if (akBottom in aForm.Controls[i].Anchors) and
       (akTop in aForm.Controls[i].Anchors) then
      aForm.Controls[i].Height := aForm.Controls[i].Height - HeightDif
    else
    if (akBottom in aForm.Controls[i].Anchors) then
      aForm.Controls[i].Top := aForm.Controls[i].Top - HeightDif;

  aForm.ClientHeight := aForm.ClientHeight + HeightDif;
end;


function OpenPDF(const URL: string): Boolean;
begin
  {$IFDEF WDC}
  Result := ShellExecute(Application.Handle, 'open', PChar(URL),nil,nil, SW_SHOWNORMAL) > 32;
  {$ENDIF}

  {$IFDEF FPC}
  Result := OpenDocument(URL);
  {$ENDIF}
end;


function BrowseURL(const URL: string): Boolean;
{$IFDEF FPC}
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: LongInt;
  BrowserProcess: TProcessUTF8;
{$ENDIF}
begin
  //We need some result incase it's neither WDC nor FPC
  Result := False;

  {$IFDEF WDC}
    //ShellExecute returns a value greater than 32 if successful, or an error value that is less than or equal to 32 otherwise
    if ShellExecute(Application.Handle, 'open', PChar(URL),nil,nil, SW_SHOWNORMAL) > 32 then
      Result := True;
  {$ENDIF}

  {$IFDEF FPC}
  v:=THTMLBrowserHelpViewer.Create(nil);
  try
    v.FindDefaultBrowser(BrowserPath,BrowserParams);

    p:=System.Pos('%s', BrowserParams);
    System.Delete(BrowserParams,p,2);
    System.Insert(URL,BrowserParams,p);

    // start browser
    BrowserProcess:=TProcessUTF8.Create(nil);
    try
      BrowserProcess.CommandLine:=BrowserPath+' '+BrowserParams;
      BrowserProcess.Execute;
      Result := True;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
  {$ENDIF}
end;


procedure MailTo(Address,Subject,Body:string);
begin
  BrowseURL('mailto:'+Address+'?subject='+Subject+'&body='+Body);
end;


procedure OpenMySite(ToolName:string; Address:string='http://krom.reveur.de');
begin
  BrowseURL(Address+'/index_r.php?t='+ToolName); //Maybe add tool version later..
end;


// Case sensitive
function CountMatches(const aString, aSubString: string): Integer;
begin
  if (aString = '') or (aSubString = '') or (Pos(aSubString, aString) = 0) then
    Result := 0
  else
    Result := (Length(aString) - Length(StringReplace(aString, aSubString, '', [rfReplaceAll]))) div Length(aSubString);
end;

end.
