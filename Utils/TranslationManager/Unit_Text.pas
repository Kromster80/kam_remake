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

    fTextsCount: Integer;
    fTexts: array of TStringArray;
    fConsts: array of TTextInfo;

    function GetConst(aIndex: Integer): TTextInfo;
    function GetText(aIndex: Integer): TStringArray;
    function GetLocale(aIndex: Integer): string;

    procedure ScanTranslations(aTextPath: string);
    procedure LoadConsts(aConstPath: string);
    procedure LoadText(aTextPath: string; TranslationID: integer);
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
    procedure Slice(aFirst, aNum: Integer);

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

  ScanTranslations(aTextPath);
  if fLocalesCount = 0 then Exit;

  fTextsCount := 0;

  LoadConsts(aConstPath);
  for I := 0 to fLocalesCount - 1 do
    LoadText(Format(aTextPath, [fLocales[I]]), I);

  AddMissingConsts;
end;


procedure TTextManager.Save(aTextPath: string; aConstPath: string);
var
  I: Integer;
begin
  if aConstPath <> '' then
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
  for I := 0 to fTextsCount - 1 do
    if not TextEmpty(I) and TextUnused(I) then
    begin
      s := StringReplace(fTexts[I, fDefaultLocale], ' ', '', [rfReplaceAll]);
      s := UpperCase(LeftStr(s, 16));

      SetLength(fConsts, Length(fConsts) + 1);
      fConsts[High(fConsts)].TextID := I;

      fConsts[High(fConsts)].ConstName := 'TX_UNUSED_' + IntToStr(I) + '_' + s;
    end;
end;


//aTextPath is in format drive\path\filename.%s.libx
procedure TTextManager.ScanTranslations(aTextPath: string);
var SearchRec: TSearchRec;
begin
  fLocalesCount := 0;
  fDefaultLocale := 0; //Default in case there is no eng

  FindFirst(Format(aTextPath, ['*']), faAnyFile - faDirectory, SearchRec);
  repeat
    SetLength(fLocales, fLocalesCount + 1);
    //text.***.libx
    fLocales[fLocalesCount] := Copy(SearchRec.Name, 6, 3);
    if fLocales[fLocalesCount] = 'eng' then
      fDefaultLocale := fLocalesCount;
    Inc(fLocalesCount);
  until (FindNext(SearchRec) <> 0);
end;


procedure TTextManager.SetConst(aIndex: Integer; const Value: TTextInfo);
begin
  fConsts[aIndex] := Value;
end;


procedure TTextManager.Slice(aFirst, aNum: Integer);
var I, K: Integer;
begin
  for I := 0 to aNum - 1 do
    for K := 0 to fLocalesCount - 1 do
      fTexts[I, K] := fTexts[I + aFirst, K];

  SetLength(fTexts, aNum);
  fTextsCount := aNum;
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


procedure TTextManager.LoadConsts(aConstPath: string);
var
  SL: TStringList;
  Line: string;
  I, CenterPos: Integer;
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


procedure TTextManager.LoadText(aTextPath: string; TranslationID: Integer);
var
  SL: TStringList;
  B: Boolean;
  I, ID, firstDelimiter, T: Integer;
  Line: string;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aTextPath);

  B := False;
  for I := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[I]);
    if Pos('MaxID:', Line) = 1 then
    begin
      B := TryStrToInt(TrimLeft(Copy(Line, 7, Length(Line))), T);
      Break;
    end;
  end;

  //Make sure we've got valid TextCount
  if B then
  begin
    if (fTextsCount <> 0) and (fTextsCount <> T) then
    begin
      ShowMessage(aTextPath + ' MaxID is inconsistent with other files');
      B := False;
    end
    else
      fTextsCount := T;
  end
  else
  begin
    ShowMessage(aTextPath + ' has no MaxID entry');
    B := False;
  end;

  if B then
  begin
    SetLength(fTexts, fTextsCount, fLocalesCount);

    for I := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);

      firstDelimiter := Pos(':', Line);
      if firstDelimiter = 0 then continue;
      if not TryStrToInt(TrimLeft(LeftStr(Line, firstDelimiter-1)), ID) then continue;

      Line := RightStr(Line, Length(Line) - firstDelimiter);
      //Required characters that can't be stored in plain text
      Line := StringReplace(Line, '\n', eol, [rfReplaceAll, rfIgnoreCase]); //EOL
      Line := StringReplace(Line, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash

      fTexts[ID, TranslationID] := Line;
    end;
  end;

  SL.Free;
end;


procedure TTextManager.SaveTranslation(aTextPath: string; TranslationID: Integer);
var
  SL: TStringList;
  I: Integer;
  s: string;
begin
  SL := TStringList.Create;

  SL.Add(''); //First line may contain BOM
  SL.Add('MaxID:' + IntToStr(fTextsCount - 1));
  SL.Add('');
  for I := 0 to fTextsCount - 1 do
  if fTexts[I, TranslationID] <> '' then
  begin
    s := IntToStr(I) + ':'+ fTexts[I, TranslationID];
    s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    SL.Add(s);
  end;

  SL.SaveToFile(aTextPath);
  SL.Free;
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

  //Append new strings, they will be empty
  SetLength(fTexts, fTextsCount + 1, fLocalesCount);
  Inc(fTextsCount);

  fConsts[aIndex].TextID := fTextsCount - 1;
  fConsts[aIndex].ConstName := 'TX_NEW' + IntToStr(fTextsCount - 1);
end;


procedure TTextManager.DeleteConst(aIndex: Integer);
var I,K: Integer;
begin
  if fConsts[aIndex].TextID <> -1 then
  begin
    //Shift all fTexts up
    for I := fConsts[aIndex].TextID to fTextsCount-2 do
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
