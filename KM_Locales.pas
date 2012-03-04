unit KM_Locales;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KM_Defaults;

type
  //@Krom: Is there a reason for using Shortstring? (fast/more efficient?) I noticed we used it for
  //       the old Locales constant, but I didn't know why.
  TKMLocaleInfo = record
    Code: String; //eng
    Title: String; //English
    FontCodepage: String;
    FlagSpriteID: Integer;
    TranslatorCredit: String;
  end;

  TKMLocales = class
  private
    fCount: Integer;
    fLocaleList: array of TKMLocaleInfo;
    procedure LoadLocales(const aFile:String);
    function ParseLocale(aLine: String; out aLocale:TKMLocaleInfo):Boolean;
    function GetLocaleByID(aLocaleID:Integer):TKMLocaleInfo;
    function GetLocaleByCode(const aLocaleCode:String):TKMLocaleInfo;
  public
    constructor Create;
    function GetIDFromCode(const aLocaleCode:String):Integer;
    function GetTranslatorCredits:String;
    property Locales[Index:Integer]: TKMLocaleInfo read GetLocaleByID; default;
    property Items[const Code:String]: TKMLocaleInfo read GetLocaleByCode;
    property Count: Integer read fCount;
  end;

  var fLocales: TKMLocales;
  const DEFAULT_LOCALE = 'eng';

implementation


constructor TKMLocales.Create;
begin
  Inherited;
  LoadLocales(ExeDir+'data\locales.txt');
end;


function TKMLocales.ParseLocale(aLine: String; out aLocale:TKMLocaleInfo):Boolean;
var K,L,R: Integer; Word: String;
const PARAM_COUNT = 5;
begin
  Result := False;
  //Skip short lines and comments
  if (Length(aLine) <= 2) or (aLine[1]+aLine[2] = '//') then Exit;

  L := 1;
  for K:=1 to PARAM_COUNT do
  begin
    R := PosEx(',', aLine, L);
    if K = PARAM_COUNT then //Last parameter does not require delimeter
      Word := Copy(aLine, L, Length(aLine) - L + 1)
    else
    begin
      if (R = 0) then Exit; //Skip line if some parameter is missing
      Word := Copy(aLine, L, R - L);
    end;
    Word := Trim(Word);
    case K of
      1: aLocale.Code             := Word;
      2: aLocale.Title            := Word;
      3: aLocale.FontCodepage     := Word;
      4: aLocale.FlagSpriteID     := StrToIntDef(Word, 0);
      5: aLocale.TranslatorCredit := Word;
    end;

    L := R + 1;
  end;
  Result := True; //Succeeded if we didn't exit earlier
end;


procedure TKMLocales.LoadLocales(const aFile:String);
var
  SL: TStringList;
  I: Integer;
  MyLocale:TKMLocaleInfo;
begin
  Assert(FileExists(aFile), 'Locales file not found: '+aFile);

  SL := TStringList.Create;
  SL.LoadFromFile(aFile);

  for I := 0 to SL.Count - 1 do
    if ParseLocale(SL[I],MyLocale) then
    begin
      inc(fCount);
      if fCount > Length(fLocaleList) then SetLength(fLocaleList,fCount+8);
      fLocaleList[fCount-1] := MyLocale;
    end;

  SL.Free;
end;


function TKMLocales.GetLocaleByID(aLocaleID:Integer):TKMLocaleInfo;
begin
  Result := fLocaleList[aLocaleID];
end;


function TKMLocales.GetLocaleByCode(const aLocaleCode:String):TKMLocaleInfo;
var I:Integer;
begin
  for I:=0 to fCount-1 do
    if fLocaleList[i].Code = aLocaleCode then
    begin
      Result := fLocaleList[i];
      Exit;
    end;
end;


function TKMLocales.GetIDFromCode(const aLocaleCode:String):Integer;
var I:Integer;
begin
  Result := -1;
  for I:=0 to fCount-1 do
    if fLocaleList[i].Code = aLocaleCode then
    begin
      Result := i;
      Exit;
    end;
end;


function TKMLocales.GetTranslatorCredits:String;
var I:Integer;
begin
  Result := '';
  for I:=0 to fCount-1 do
    if fLocaleList[i].TranslatorCredit <> '' then //e.g. English has no credits
      Result := Result + fLocaleList[i].Title + ' - ' + fLocaleList[i].TranslatorCredit + '|';
end;

end.
