unit Unit_Text;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics, Math,
  StdCtrls, StrUtils, SysUtils, Windows;


type
  TTextInfo = record
               TextID: Integer;
               ConstName: string; //Name used in KM_TextLibrary.pas
             end;

  TStringArray = array of string;

type
  TTextManager = class
  private
    fLocalesCount: Integer;
    fLocales: array of string;
    fDefaultLocale: Integer;

    fTexts: array of TStringArray;
    fConsts: array of TTextInfo;

    function GetConst(aIndex: Integer): TTextInfo;
    function GetText(aIndex: Integer): TStringArray;
    function GetLocale(aIndex: Integer): string;

    procedure ScanAvailableTranslations(aTextPath: string);
    procedure LoadTextLibraryConsts(aConstPath: string);
    procedure LoadTranslation(aTextPath: string; TranslationID: integer);
    procedure AddMissingConsts;
    procedure SaveTextLibraryConsts(aFileName: string);
    procedure SaveTranslation(aTextPath: string; TranslationID: integer);
    procedure SetConst(aIndex: Integer; const Value: TTextInfo);
  public
    procedure Load(aTextPath: string; aConstPath: string);
    procedure Save(aTextPath: string; aConstPath: string);

    function ConstCount: Integer;
    property Consts[aIndex: Integer]: TTextInfo read GetConst write SetConst;
    property Texts[aIndex: Integer]: TStringArray read GetText;
    property LocalesCount: Integer read fLocalesCount;
    property Locales[aIndex: Integer]: string read GetLocale;
    property DefaultLocale: Integer read fDefaultLocale;

    procedure DeleteConst(aIndex: Integer);
    procedure Insert(aIndex: Integer);
    procedure InsertSeparator(aIndex: Integer);

    procedure MoveUp(aIndex: Integer);
    procedure MoveDown(aIndex: Integer);

    procedure SortByIndex;
    procedure SortByName;
    procedure CompactIndexes;
  end;


implementation


const
  eol: string = #13#10; //EndOfLine


procedure TTextManager.Load(aTextPath: string; aConstPath: string);
var
  I: Integer;
begin
  fLocalesCount := 0;

  SetLength(fConsts, 0);
  SetLength(fTexts, 0);
  SetLength(fLocales, 0);

  ScanAvailableTranslations(aTextPath);
  LoadTextLibraryConsts(aConstPath);
  for I := 0 to fLocalesCount - 1 do
    LoadTranslation(Format(aTextPath, [fLocales[I]]), I);

  AddMissingConsts;
end;


procedure TTextManager.Save(aTextPath: string; aConstPath: string);
var
  I: Integer;
begin
  SaveTextLibraryConsts(aConstPath);
  for I := 0 to fLocalesCount - 1 do
    SaveTranslation(Format(aTextPath, [fLocales[I]]), I);
end;


procedure TTextManager.AddMissingConsts;
  function TextEmpty(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 0 to fLocalesCount - 1 do
      Result := Result and (Trim(fTexts[aIndex, I]) = '');
  end;
  function TextUnused(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 0 to High(fConsts) do
    if fConsts[I].TextID = aIndex then
    begin
      Result := False;
      Break;
    end;
  end;
var I: Integer; s: string;
begin
  for I := 0 to High(fTexts) do
    if not TextEmpty(I) and TextUnused(I) then
    begin
      s := StringReplace(fTexts[I, fDefaultLocale], ' ', '', [rfReplaceAll]);
      s := UpperCase(LeftStr(s, 16));

      SetLength(fConsts, Length(fConsts) + 1);
      fConsts[High(fConsts)].TextID := I;

      fConsts[High(fConsts)].ConstName := 'TX_UNUSED_' + IntToStr(I) + '_' + s;
    end;
end;


procedure TTextManager.ScanAvailableTranslations(aTextPath: string);
var
  SearchRec: TSearchRec;
begin
  fDefaultLocale := 0; //Default in case there is no eng
  fLocalesCount := 0;
  FindFirst(Format(aTextPath, ['*']), faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
    and (SearchRec.Attr and faDirectory <> faDirectory) then
    begin
        SetLength(fLocales, fLocalesCount + 1);
        //text.***.libx
        fLocales[fLocalesCount] := Copy(SearchRec.Name, 6, 3);
        if fLocales[fLocalesCount] = 'eng' then
          fDefaultLocale := fLocalesCount;
        Inc(fLocalesCount);
      end;
  until (FindNext(SearchRec) <> 0);
end;


procedure TTextManager.SetConst(aIndex: Integer; const Value: TTextInfo);
begin
  fConsts[aIndex] := Value;
end;

procedure TTextManager.SaveTextLibraryConsts(aFileName: string);
var
  myFile : TextFile;
  I: integer;
begin
  AssignFile(myFile, aFileName);
  ReWrite(myFile);

  for I := 0 to High(fConsts) do
    if fConsts[I].TextID = -1 then
      WriteLn(myFile, '')
    else
      WriteLn(myFile, fConsts[I].ConstName + ' = ' + IntToStr(fConsts[I].TextID) + ';');

  CloseFile(myFile);
end;


procedure TTextManager.LoadTextLibraryConsts(aConstPath: string);
const Size_Inc = 100;
var
  SL: TStringList;
  Line: string;
  i, CenterPos: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aConstPath);

  SetLength(fConsts, SL.Count);

  for I := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[I]);

    CenterPos := Pos(' = ',Line);
    //Separator (line without ' = ')
    if CenterPos = 0 then
    begin
      fConsts[I].TextID := -1;
      fConsts[I].ConstName := '';
    end
    else
    begin
      fConsts[I].TextID := StrToInt(Copy(Line, CenterPos + 3, Length(Line) - CenterPos - 3));
      fConsts[I].ConstName := Copy(Line, 1, CenterPos - 1);
    end;
  end;

  SL.Free;
end;


procedure TTextManager.LoadTranslation(aTextPath: string; TranslationID: Integer);
var
  SL:TStringList;
  i,ID,firstDelimiter:integer;
  Line:string;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aTextPath);

  SetLength(fTexts, 3000);
  for I := 0 to 3000 - 1 do
    SetLength(fTexts[I], fLocalesCount);

  for i := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[i]);

    firstDelimiter := Pos(':', Line);
    if firstDelimiter = 0 then continue;
    if not TryStrToInt(TrimLeft(LeftStr(Line, firstDelimiter-1)), ID) then continue;

    Line := RightStr(Line, Length(Line) - firstDelimiter);
    //Required characters that can't be stored in plain text
    Line := StringReplace(Line, '\n', eol, [rfReplaceAll, rfIgnoreCase]); //EOL
    Line := StringReplace(Line, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash

    fTexts[ID, TranslationID] := Line;
  end;

  SL.Free;
end;


procedure TTextManager.SaveTranslation(aTextPath: string; TranslationID: Integer);
var
  aStringList:TStringList;
  i:integer;
  s:string;
begin
  aStringList := TStringList.Create;

  aStringList.Add('');
  aStringList.Add('MaxID:' + IntToStr(3000));
  aStringList.Add('');
  for i := 0 to Length(fTexts) - 1 do
  if fTexts[i, TranslationID] <> '' then
  begin
    s := IntToStr(I) + ':'+ fTexts[i, TranslationID];
    s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    aStringList.Add(s);
  end;

  aStringList.SaveToFile(aTextPath);
  aStringList.Free;
end;


//Sort the items by TextID
procedure TTextManager.SortByIndex;
  function Compare(A, B: Integer): Boolean;
  begin
    Result := fConsts[A].TextID > fConsts[B].TextID;
  end;

  procedure Swap(A, B: Integer);
  var Temp: TTextInfo;
  begin
    Temp := fConsts[A]; fConsts[A] := fConsts[B]; fConsts[B] := Temp;
  end;

var
  I, K: Integer;
begin
  //Delete separators
  for I := High(fConsts) downto 0 do
    if fConsts[I].TextID = -1 then
    begin
      for K := I to Length(fConsts)-2 do
        fConsts[K] := fConsts[K+1];
      SetLength(fConsts, Length(fConsts) - 1);
    end;

  //Sort
  for I := 0 to High(fConsts) do
  for K:= I + 1 to High(fConsts) do
  if Compare(I, K) then
    Swap(I, K);

  //Add separators anew between grouped areas
  for I := 1 to High(fConsts) do
    if (fConsts[I-1].TextID <> -1) and (fConsts[I-1].TextID+1 <> fConsts[I].TextID) then
    begin
      SetLength(fConsts, Length(fConsts) + 1);
      for K := Length(fConsts)-1 downto I+1 do
        fConsts[K] := fConsts[K-1];
      fConsts[I].TextID := -1;
      fConsts[I].ConstName := '';
    end;
end;


//Sort TextIDs by Index
procedure TTextManager.SortByName;
  function Compare(A, B: Integer): Boolean;
  begin
    Result := fConsts[A].TextID > fConsts[B].TextID;
  end;

  procedure Swap(A, B: Integer);
  var
    I: Integer;
    S: string;
    T: Integer;
  begin
    T := fConsts[A].TextID; fConsts[A].TextID := fConsts[B].TextID; fConsts[B].TextID := T;
    for I := 0 to fLocalesCount - 1 do
    begin
      S := fTexts[fConsts[A].TextID, I];
      fTexts[fConsts[A].TextID, I] := fTexts[fConsts[B].TextID, I];
      fTexts[fConsts[B].TextID, I] := S;
    end;
  end;

var
  I, K: Integer;
begin
  //Sort
  for I := 0 to High(fConsts) do
  if fConsts[I].TextID <> -1 then
    for K:= I + 1 to High(fConsts) do
    if fConsts[K].TextID <> -1 then
      if Compare(I,K) then
        Swap(I,K);

  //Compact Indexes
  {for I := 1 to High(fConsts) - 1 do
  if (fConsts[I].TextID = -1) and (fConsts[I-1].TextID = fConsts[I+1].TextID+1) then
    InsertConst()}
end;


procedure TTextManager.CompactIndexes;
begin
  {SetLength(Used, Length(fTexts));
  for I := 0 to High(Used) do
    Used[I] := 1;

  for I := 0 to High(fConsts) do
    if fConsts[I].TextID <> -1 then
      Used[fConsts[I].TextID] := 0;

  for I := 1 to High(fConsts) do
  if (fConsts[I].TextID = -1) and (fConsts[I-1].TextID = -1) then
    DeleteConst(I);}
end;


function TTextManager.ConstCount: Integer;
begin
  Result := Length(fConsts);
end;


procedure TTextManager.Insert(aIndex: Integer);
var I: Integer;
begin
  SetLength(fConsts, Length(fConsts) + 1);

  //Move others down
  for i := Length(fConsts)-2 downto aIndex do
    fConsts[i+1] := fConsts[i];

  SetLength(fTexts, Length(fTexts) + 1);
  SetLength(fTexts[High(fTexts)], fLocalesCount);
  for I := 0 to fLocalesCount - 1 do
    fTexts[aIndex,I] := '';

  fConsts[aIndex].TextID := High(fTexts);
  fConsts[aIndex].ConstName := 'TX_NEW'+IntToStr(High(fTexts));
end;


procedure TTextManager.DeleteConst(aIndex: Integer);
var I,K: Integer;
begin
  if fConsts[aIndex].TextID <> -1 then
  begin
    //Shift all fTexts up
    for I := fConsts[aIndex].TextID to High(fTexts)-1 do
      for K := 0 to fLocalesCount - 1 do
        fTexts[I,K] := fTexts[I+1,K];

    //Shift pointers
    for I := 0 to High(fConsts) do
      if fConsts[I].TextID > fConsts[aIndex].TextID then
        Dec(fConsts[I].TextID);
  end;

  //Shift fConsts up
  for I := aIndex to Length(fConsts)-2 do
    fConsts[I] := fConsts[I+1];

  SetLength(fConsts, Length(fConsts)-1);
end;


function TTextManager.GetConst(aIndex: Integer): TTextInfo;
begin
  Result := fConsts[aIndex];
end;

function TTextManager.GetLocale(aIndex: Integer): string;
begin
  Result := fLocales[aIndex];
end;

function TTextManager.GetText(aIndex: Integer): TStringArray;
begin
  Result := fTexts[aIndex];
end;

procedure TTextManager.InsertSeparator(aIndex: Integer);
var i: integer;
begin
  SetLength(fConsts, Length(fConsts) + 1);

  //Move others down
  for i := Length(fConsts)-2 downto aIndex do
    fConsts[i+1] := fConsts[i];

  fConsts[aIndex].TextID := -1;
  fConsts[aIndex].ConstName := '';
end;


procedure TTextManager.MoveUp(aIndex: Integer);
var Temp: TTextInfo;
begin
  if aIndex <= 0 then Exit; //Can't move the top item up

  Temp := fConsts[aIndex];
  fConsts[aIndex] := fConsts[aIndex-1];
  fConsts[aIndex-1] := Temp;
end;


procedure TTextManager.MoveDown(aIndex: Integer);
var Temp: TTextInfo;
begin
  if aIndex = High(fConsts) then Exit; //Can't move the bottom item down

  Temp := fConsts[aIndex];
  fConsts[aIndex] := fConsts[aIndex+1];
  fConsts[aIndex+1] := Temp;
end;


end.
