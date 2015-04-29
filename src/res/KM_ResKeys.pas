unit KM_ResKeys;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF FPC} lconvencoding, LazUTF8, {$ENDIF}
  Classes, SysUtils, StrUtils, KromUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_FileIO, KM_ResTexts;

const
  // Load key IDs from this include file
  {$I KM_KeyIDs.inc}

  // For missing Keys
  NO_KEY = nil;

type
  TKMKeyLibrary = class
  private
    fKeys: TIntegerArray;
    FilePath: String;
    function GetKeys(aIndex: Word): Integer;
    procedure LoadKeymapFile(var aArray: TIntegerArray);
  public
    KeyCount: Integer;
    constructor Create;
    function HasKey(aIndex: Word): Boolean;
    property Keys[aIndex: Word]: Integer read GetKeys; default;
    procedure LoadKeys; // Load all the keys
    procedure ResetKeyBind;
    procedure SaveKey(aKeyID: Integer; aKeyValue: Word);
  end;

var
  // All games Keys accessible from everywhere
  gResKeys: TKMKeyLibrary;

implementation

{ TKMKeyLibrary }
constructor TKMKeyLibrary.Create;
begin
  FilePath := (ExeDir + 'data' + PathDelim + 'keys.keymap');
  if (not FileExists(FilePath)) then // If file is missing, create one here
    ResetKeyBind;
  inherited Create;
end;


// .keymap files consist of lines. Each line has an index and a text. Lines without index are skipped
procedure TKMKeyLibrary.LoadKeymapFile(var aArray: TIntegerArray);
  function KeyToArray(const Value: UnicodeString): TUnicodeStringArray;
  var
    Point, Start: PWideChar;
    KeyString: UnicodeString;
  begin
    SetLength(Result, 0);
    KeyCount := 0;
    Point := Pointer(Value);
    if Point = nil then Exit;

    // This is a lot faster than using StrPos/AnsiStrPos when
    // LineBreak is the default (#13#10)
    while Point^ <> #0 do
    begin
      Start := Point;
      while not KromUtils.CharInSet(Point^, [#0, #10, #13]) do Inc(Point);
      SetString(KeyString, Start, Point - Start);

      SetLength(Result, KeyCount + 1);
      Result[KeyCount] := KeyString;
      Inc(KeyCount);

      if Point^ = #13 then Inc(Point);
      if Point^ = #10 then Inc(Point);
    end;
  end;
var
  KeyStringArray: TUnicodeStringArray;
  KeyString, LibKey: UnicodeString;
  I, KeyID, KeyValue, TopId, FirstDelimiter, SecondDelimiter: Integer;
begin
  if not FileExists(FilePath) then Exit;

  // Load UniCode file with codepage
  LibKey := ReadTextU(FilePath, 1252);
  KeyStringArray := KeyToArray(LibKey);

  for I := High(KeyStringArray) downto 0 do
  begin
    FirstDelimiter := Pos(':', KeyStringArray[I]);
    if FirstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(KeyStringArray[I], FirstDelimiter - 1), TopId) then
      Break;
  end;

  Assert(TopId <= 1024, 'Dont allow too many strings for no reason');

  // Don't shrink the array, we might be overloading
  if Length(aArray) < TopId + 1 then
    SetLength(aArray, TopId + 1);

  for I := 0 to High(KeyStringArray) do
  begin
    KeyString := KeyStringArray[I];
    // Get Key index and Value
    FirstDelimiter := Pos(':', KeyString);
    SecondDelimiter := Pos('//', KeyString);
    KeyID := StrToInt(Copy(KeyString, 0, FirstDelimiter - 1));
    KeyValue := StrToInt(Copy(KeyString, FirstDelimiter + 1, SecondDelimiter - FirstDelimiter -1));
    aArray[KeyID] := KeyValue;
  end;
end;


// Check if requested string is empty
function TKMKeyLibrary.HasKey(aIndex: Word): Boolean;
begin
  Result := ((aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0));
end;


function TKMKeyLibrary.GetKeys(aIndex: Word): Integer;
begin
  if (aIndex < Length(fKeys)) and (fKeys[aIndex] <> 0) then
    Result := fKeys[aIndex]
  else
    Result := -1;
end;


procedure TKMKeyLibrary.LoadKeys;
begin
  LoadKeymapFile(fKeys);
end;


procedure TKMKeyLibrary.SaveKey(aKeyID: Integer; aKeyValue: Word);
var
  Keystring: string;
  KeyStringList: TStringList;
  I: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}
  fKeys[aKeyID] := aKeyValue;

  for I := 0 to KeyCount - 1 do
  begin
    Keystring := IntToStr(I) + ':'+ IntToStr(fKeys[I]) + '//' + gResTexts.GetNameForKey(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(FilePath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
end;


procedure TKMKeyLibrary.ResetKeyBind;
var
  Keystring: string;
  KeyStringList: TStringList;
  I, KeyValue: Integer;
begin
  KeyStringList := TStringList.Create;
  {$IFDEF WDC}KeyStringList.DefaultEncoding := TEncoding.UTF8;{$ENDIF}

  for I := 0 to KeyCount - 1 do
  begin
    case I of
      0:  KeyValue := 37;
      1:  KeyValue := 39;
      2:  KeyValue := 38;
      3:  KeyValue := 40;
      4:  KeyValue := 112;
      5:  KeyValue := 113;
      6:  KeyValue := 114;
      7:  KeyValue := 115;
      8:  KeyValue := 72;
      9:  KeyValue := 83;
      10: KeyValue := 76;
      11: KeyValue := 70;
      12: KeyValue := 88;
      13: KeyValue := 187;
      14: KeyValue := 189;
      15: KeyValue := 190;
      16: KeyValue := 188;
      17: KeyValue := 116;
      18: KeyValue := 117;
      19: KeyValue := 118;
      20: KeyValue := 119;
      21: KeyValue := 66;
      22: KeyValue := 80;
      23: KeyValue := 84;
      24: KeyValue := 34;
      25: KeyValue := 33;
      26: KeyValue := 8;
      27: KeyValue := 49;
      28: KeyValue := 50;
      29: KeyValue := 51;
      30: KeyValue := 52;
      31: KeyValue := 53;
      32: KeyValue := 54;
      33: KeyValue := 55;
      34: KeyValue := 56;
      35: KeyValue := 57;
      36: KeyValue := 48;
      37: KeyValue := 32;
      38: KeyValue := 46;
      39: KeyValue := 13;
      40: KeyValue := 27;
      41: KeyValue := 77;
      42: KeyValue := 86;
      43: KeyValue := 68;
      44: KeyValue := 67;
    end;
    Keystring := IntToStr(I) + ':' + IntToStr(KeyValue) + '//' + gResTexts.GetNameForKey(I);
    KeyStringList.Add(Keystring);
  end;

  KeyStringList.SaveToFile(FilePath{$IFDEF WDC}, TEncoding.UTF8{$ENDIF});
  KeyStringList.Free;
end;

end.
