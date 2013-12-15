unit KM_ResLocales;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  KM_CommonTypes;


type
  TKMLocaleInfo = record
    Code: AnsiString; //3-letter code: 'eng', 'rus'
    Title: string; //Full name: 'English', 'Russian'
    FontCodepage: Word;
    FlagSpriteID: Integer;
    FallbackLocale: AnsiString;
    TranslatorCredit: UnicodeString;
  end;

  TKMLocales = class
  private
    fCount: Integer;
    fLocaleList: array of TKMLocaleInfo;
    fUserLocale: AnsiString;
    procedure LoadLocales(const aFile: string);
    procedure SetUserLocale(aLocale: AnsiString);
    function ParseLine(aLine: string; out aLocale: TKMLocaleInfo): Boolean;
    function GetLocaleByIndex(aIndex: Integer): TKMLocaleInfo;
  public
    FallbackLocale: AnsiString;
    DefaultLocale: AnsiString;
    constructor Create(aPath: string; aUserLocale: AnsiString);
    property Count: Integer read fCount;
    property UserLocale: AnsiString read fUserLocale write SetUserLocale;
    property Locales[aIndex: Integer]: TKMLocaleInfo read GetLocaleByIndex; default;
    function IndexByCode(const aLocaleCode: AnsiString): Integer;
    function LocaleByCode(const aCode: AnsiString): TKMLocaleInfo;
    function TranslatorCredits: string;
    function CodePagesList: TKMWordArray;
  end;


var
  gResLocales: TKMLocales;


const
  DEFAULT_LOCALE: AnsiString = 'eng';


implementation


{ TKMLocales }
//Path to locales info file, usually \data\locales.txt
constructor TKMLocales.Create(aPath: string; aUserLocale: AnsiString);
begin
  inherited Create;

  LoadLocales(aPath);

  DefaultLocale := DEFAULT_LOCALE;
  SetUserLocale(aUserLocale);
end;


procedure TKMLocales.SetUserLocale(aLocale: AnsiString);
begin
  //Make sure user locale is valid
  if IndexByCode(aLocale) <> -1 then
    fUserLocale := aLocale
  else
    fUserLocale := DefaultLocale;

  FallbackLocale := LocaleByCode(fUserLocale).FallbackLocale;
end;


function TKMLocales.ParseLine(aLine: UnicodeString; out aLocale: TKMLocaleInfo): Boolean;
const
  PARAM_COUNT = 6;
var
  I, L, R: Integer;
  Chunk: string;
begin
  Result := False;
  //Skip short lines and comments
  if (Length(aLine) <= 2) or (aLine[1] + aLine[2] = '//') then Exit;

  L := 1;
  for I := 1 to PARAM_COUNT do
  begin
    R := PosEx(',', aLine, L);
    if I = PARAM_COUNT then //Last parameter does not require delimeter
      Chunk := Copy(aLine, L, Length(aLine) - L + 1)
    else
    begin
      if (R = 0) then Exit; //Skip line if some parameter is missing
      Chunk := Copy(aLine, L, R - L);
    end;
    Chunk := Trim(Chunk);
    case I of
      1: aLocale.Code             := AnsiString(Chunk);
      2: aLocale.Title            := Chunk;
      3: aLocale.FontCodepage     := StrToIntDef(Chunk, 0);
      4: aLocale.FlagSpriteID     := StrToIntDef(Chunk, 0);
      5: aLocale.FallbackLocale   := AnsiString(Chunk);
      6: aLocale.TranslatorCredit := Chunk;
    end;

    L := R + 1;
  end;
  Result := True; //Succeeded if we didn't exit earlier
end;


procedure TKMLocales.LoadLocales(const aFile: string);
var
  SL: TStringList;
  I: Integer;
  NewLocale: TKMLocaleInfo;
begin
  Assert(FileExists(aFile), 'Locales file could not be found: ' + aFile);

  SL := TStringList.Create;
  SL.LoadFromFile(aFile);

  for I := 0 to SL.Count - 1 do
    if ParseLine(SL[I], NewLocale) then
    begin
      if fCount >= Length(fLocaleList) then
        SetLength(fLocaleList, fCount + 8);
      fLocaleList[fCount] := NewLocale;
      Inc(fCount);
    end;

  SL.Free;
end;


function TKMLocales.GetLocaleByIndex(aIndex: Integer): TKMLocaleInfo;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fLocaleList[aIndex];
end;


function TKMLocales.LocaleByCode(const aCode: AnsiString): TKMLocaleInfo;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    if fLocaleList[I].Code = aCode then
    begin
      Result := fLocaleList[I];
      Exit;
    end;
  Assert(False, aCode + ' is not a valid Locale');
end;


function TKMLocales.IndexByCode(const aLocaleCode: AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if fLocaleList[I].Code = aLocaleCode then
    begin
      Result := I;
      Exit;
    end;
end;


function TKMLocales.TranslatorCredits: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if fLocaleList[I].TranslatorCredit <> '' then //e.g. English has no credits
      Result := Result + fLocaleList[I].Title + ' - ' + fLocaleList[I].TranslatorCredit + '|';
end;


function TKMLocales.CodePagesList: TKMWordArray;
var
  I,K: Integer;
  added: Boolean;
  resCount: Word;
begin
  //Reserve maximum required space
  SetLength(Result, Count);

  resCount := 0;
  for I := 0 to Count - 1 do
  begin
    //Check if we already have that element
    added := False;
    for K := 0 to Count - 1 do
      added := added or (Locales[I].FontCodepage = Result[K]);

    //Append element
    if not added then
    begin
      Result[resCount] := Locales[I].FontCodepage;
      Inc(resCount);
    end;
  end;

  //Trim excess elements
  SetLength(Result, resCount);
end;


end.
