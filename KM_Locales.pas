unit KM_Locales;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils, KM_Defaults;

type
  TKMLocaleInfo = record
    Code: string[3]; //eng
    Title: string; //English
    FontCodepage: string;
    FlagSpriteID: Integer;
    FallbackLocale: string;
    TranslatorCredit: string;
  end;

  TKMLocales = class
  private
    fCount: Integer;
    fLocaleList: array of TKMLocaleInfo;
    procedure LoadLocales(const aFile: string);
    function ParseLine(aLine: string; out aLocale: TKMLocaleInfo): Boolean;
    function GetLocaleByIndex(aIndex: Integer): TKMLocaleInfo;
  public
    constructor Create;
    function GetIDFromCode(const aLocaleCode: string): Integer;
    function GetTranslatorCredits: string;
    property Locales[aIndex: Integer]: TKMLocaleInfo read GetLocaleByIndex; default;
    function GetLocale(const aCode: string): TKMLocaleInfo;
    property Count: Integer read fCount;
  end;


  var fLocales: TKMLocales;
  const DEFAULT_LOCALE = 'eng';


implementation


{ TKMLocales }
constructor TKMLocales.Create;
begin
  inherited;
  LoadLocales(ExeDir + 'data\locales.txt');
end;


function TKMLocales.ParseLine(aLine: string; out aLocale: TKMLocaleInfo): Boolean;
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
      1: aLocale.Code             := Chunk;
      2: aLocale.Title            := Chunk;
      3: aLocale.FontCodepage     := Chunk;
      4: aLocale.FlagSpriteID     := StrToIntDef(Chunk, 0);
      5: aLocale.FallbackLocale   := Chunk;
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
  MyLocale: TKMLocaleInfo;
begin
  Assert(FileExists(aFile), 'Locales file could not be found: ' + aFile);

  SL := TStringList.Create;
  SL.LoadFromFile(aFile);

  for I := 0 to SL.Count - 1 do
    if ParseLine(SL[I], MyLocale) then
    begin
      inc(fCount);
      if fCount > Length(fLocaleList) then SetLength(fLocaleList, fCount + 8);
      fLocaleList[fCount-1] := MyLocale;
    end;

  SL.Free;
end;


function TKMLocales.GetLocaleByIndex(aIndex: Integer): TKMLocaleInfo;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fLocaleList[aIndex];
end;


function TKMLocales.GetLocale(const aCode: string): TKMLocaleInfo;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    if fLocaleList[i].Code = aCode then
    begin
      Result := fLocaleList[i];
      Exit;
    end;
  Assert(False, aCode + ' is not a valid Locale');
end;


function TKMLocales.GetIDFromCode(const aLocaleCode: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to fCount - 1 do
    if fLocaleList[i].Code = aLocaleCode then
    begin
      Result := i;
      Exit;
    end;
end;


function TKMLocales.GetTranslatorCredits: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to fCount - 1 do
    if fLocaleList[i].TranslatorCredit <> '' then //e.g. English has no credits
      Result := Result + fLocaleList[i].Title + ' - ' + fLocaleList[i].TranslatorCredit + '|';
end;


end.
