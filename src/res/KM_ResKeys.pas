unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResLocales;


const
  //Load key IDs from this include file
  {$I KM_KeyIDs.inc}

  //for missing Keys
  NO_KEY = nil;

type
  TKMKeyLibraryCommon = class
  private
    FILE_PATH: String;
    procedure LoadKeymapFile(var aArray: TIntegerArray);
  end;


  TKMKeyLibrarySingle = class(TKMKeyLibraryCommon)
  private
    fKeys: TIntegerArray;
    function GetKeys(aIndex: Word): String;
  public
    procedure LoadKeys(aPathTemplate: string); //initial locale for UI strings
    property Keys[aIndex: Word]: String read GetKeys; default;
  end;


  TKMKeyLibraryMulti = class(TKMKeyLibraryCommon)
  private
    fPref: Integer;
    fKeys: array of TIntegerArray;
    function GetKeys(aIndex: Word): Integer;
  public
    constructor Create;
    procedure LoadKeys(aPathTemplate: string); //All locales for Mission strings
    function HasKey(aIndex: Word): Boolean;
    property Keys[aIndex: Word]: Integer read GetKeys; default;
    procedure Save(aStream: TKMemoryStream);
    procedure SaveKey(aKeyID, aKeyValue: Integer);
    procedure Load(aStream: TKMemoryStream);
  end;


var
  //All games Keys accessible from everywhere
  gResKeys: TKMKeyLibraryMulti;


implementation


{ TKMKeyLibraryCommon }
//keymap files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMKeyLibraryCommon.LoadKeymapFile(var aArray: TIntegerArray);
  function KeyToArray(const Value: String): TStringArray;
  var
    P, Start: PWideChar;
    S: String;
  begin
    SetLength(Result, 0);
    P := Pointer(Value);
    if P = nil then Exit;

    // This is a lot faster than using StrPos/AnsiStrPos when
    // LineBreak is the default (#13#10)
    while P^ <> #0 do
    begin
      Start := P;
      while not KromUtils.CharInSet(P^, [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := S;

      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;
var
  Tmp: TStringArray;
  //langCode: AnsiString;
  libKey: String;
  I: Integer;
  s: String;
  firstDelimiter: Integer;
  id, topId: Integer;
  {$IFDEF FPC} tmpA: AnsiString; {$ENDIF}
begin
  FILE_PATH := (ExeDir + 'data' + PathDelim + 'keys.keymap');
  if not FileExists(FILE_PATH) then Exit;

  //Load ANSI file with codepage we say into unicode string
  //langCode := AnsiString(Copy(FILE_PATH, Length(FILE_PATH) - 7, 3));
  libKey := ReadTextA(FILE_PATH);
  Tmp := KeyToArray(libKey);

  for I := High(Tmp) downto 0 do
  begin
    firstDelimiter := Pos(':', Tmp[I]);
    if firstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(Tmp[I], firstDelimiter - 1), topId) then
      Break;
  end;

  Assert(topId <= 1024, 'Dont allow too many strings for no reason');

  //Don't shrink the array, we might be overloading base locale with a partial translation
  if Length(aArray) < topId + 1 then
    SetLength(aArray, topId + 1);

  for I := 0 to High(Tmp) do
  begin
    s := Tmp[I];

    //Get string index and skip erroneous lines
    firstDelimiter := Pos(':', s);
    if firstDelimiter = 0 then Continue;
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter - 1)), id) then Continue;

    s := RightStr(s, Length(s) - firstDelimiter);
    //Required characters that can't be stored in plain text
    //todo: Remove them in favor of | for eol (and update libx files)
    {$IFDEF WDC}
    s := StringReplace(s, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); //EOL
    s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
    {$ENDIF}
    {$IFDEF FPC}
    //In FPC StringReplace only works for UTF8/Ansi strings
    tmpA := UTF16toUTF8(s);
    tmpA := StringReplace(tmpA, '\n', EolW, [rfReplaceAll, rfIgnoreCase]); //EOL
    tmpA := StringReplace(tmpA, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := UTF8toUTF16(tmpA);
    {$ENDIF}
    aArray[id] := StrToInt(s);
  end;
end;


function TKMKeyLibrarySingle.GetKeys(aIndex: Word): String;
begin
  if aIndex < Length(fKeys) then
    Result := IntToStr(fKeys[aIndex])
  else
    Result := '~~~String ' + IntToStr(aIndex) + ' out of range!~~~';
end;


procedure TKMKeyLibrarySingle.LoadKeys;
begin
  LoadKeymapFile(fKeys);
end;


{ TKMKeyLibraryMulti }
constructor TKMKeyLibraryMulti.Create;
begin
  inherited Create;

  fPref := 0;
end;


//Check if requested string is empty
function TKMKeyLibraryMulti.HasKey(aIndex: Word): Boolean;
begin
  Result := ((fPref <> -1) and (aIndex < Length(fKeys[fPref])) and (fKeys[fPref, aIndex] <> 0));
end;


function TKMKeyLibraryMulti.GetKeys(aIndex: Word): Integer;
begin
  if (fPref <> -1) and (aIndex < Length(fKeys[fPref])) and (fKeys[fPref, aIndex] <> 0) then
    Result := fKeys[fPref, aIndex]
  //else
    //Result := '~~~Key ' + IntToStr(aIndex) + ' out of range!~~~';
end;


//Path template with %s
procedure TKMKeyLibraryMulti.LoadKeys;
begin
  LoadKeymapFile(fKeys[0]);
end;


procedure TKMKeyLibraryMulti.Save(aStream: TKMemoryStream);
var
  K: Integer;
  KeyCount: Integer;
begin
  //Only save values containing keys
  aStream.Write(1);
  if Length(fKeys[0]) > 0 then
  begin
    aStream.WriteA('keys');

    KeyCount := Length(fKeys[0]);

    aStream.Write(KeyCount);
    for K := 0 to KeyCount - 1 do
      aStream.Write(fKeys[0,K]);
  end;
end;

procedure TKMKeyLibraryMulti.SaveKey(aKeyID, aKeyValue: Integer);
var
  SL: TStringList;
  I: Integer;
  s: string;
begin
  SL := TStringList.Create;
  SL.DefaultEncoding := TEncoding.UTF8;

  for I := 0 to 27 do
  if I <= High(fKeys) then
  if fKeys[I, aKeyID] <> 0 then
  begin
    s := IntToStr(I) + ':'+ IntToStr(fKeys[I, aKeyID]);
    s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := StringReplace(s, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    SL.Add(s);
  end;

  //Don't create blank files for unused translations
  if (SL.Count > 0) or FileExists(FILE_PATH) then
    SL.SaveToFile(FILE_PATH);
  SL.Free;
end;


procedure TKMKeyLibraryMulti.Load(aStream: TKMemoryStream);
var
  I,K: Integer;
  LocCount, KeyCount: Integer;
  curLoc: AnsiString;
  Tmp: String;
begin
  //Try to match savegame locales with players locales,
  //cos some players might have non-native locales missing
  //We might add locale selection to setup.exe

  SetLength(fKeys, 1);

  aStream.Read(LocCount);
  for I := 0 to LocCount - 1 do
  begin
    aStream.ReadA(curLoc);
    aStream.Read(KeyCount);

    SetLength(fKeys[0], KeyCount);
    for K := 0 to KeyCount - 1 do
      aStream.Read(fKeys[0,K]);
  end;
end;


end.
