unit Unit_Text;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics, Math, KromUtils,
  StdCtrls, StrUtils, SysUtils, Windows, KM_ResLocales, KM_FileIO;


type
  TTextInfo = record
    TextID: Integer;
    ConstName: string; // Name used in KM_TextLibrary.pas
  end;

  TStringArray = array of string;

type
  TTextManager = class
  private
    fTextPath: string;
    fConstPath: string; // We use consts only for ingame library, others don't need them

    fTextsTopId: Integer;
    fTexts: array of TStringArray;
    fConsts: array of TTextInfo;

    function GetConst(aIndex: Integer): TTextInfo;
    function GetText(aIndex: Integer): TStringArray;

    procedure LoadConsts(aConstPath: string);
    procedure LoadText(aTextPath: string; TranslationID: integer; aCodePage: Word);
    procedure AddMissingConsts;
    procedure SaveTextLibraryConsts(aFileName: string);
    procedure SaveTranslation(aTextPath: string; TranslationID: integer);
    procedure SetConst(aIndex: Integer; const Value: TTextInfo);
  public
    procedure Load(const aTextPath, aConstPath: string);
    procedure Save;

    function ConstCount: Integer;
    property Consts[aIndex: Integer]: TTextInfo read GetConst write SetConst;
    property Texts[aIndex: Integer]: TStringArray read GetText;
    function TextBlankInAll(aIndex: Integer): Boolean;

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
uses
  KM_Defaults;


const
  eol: string = #13#10; // EndOfLine


procedure TTextManager.Load(const aTextPath, aConstPath: string);
var
  I: Integer;
begin
  fTextPath := aTextPath;
  fConstPath := aConstPath;

  SetLength(fConsts, 0);
  SetLength(fTexts, 0);

  fTextsTopId := -1;

  if fConstPath <> '' then
    LoadConsts(fConstPath);

  for I := 0 to gResLocales.Count - 1 do
    LoadText(Format(fTextPath, [gResLocales[I].Code]), I, gResLocales[I].FontCodepage);

  if fConstPath <> '' then
    AddMissingConsts;
end;


procedure TTextManager.Save;
var
  I: Integer;
begin
  if fConstPath <> '' then
    SaveTextLibraryConsts(fConstPath);

  for I := 0 to gResLocales.Count - 1 do
    SaveTranslation(Format(fTextPath, [gResLocales[I].Code]), I);
end;


procedure TTextManager.AddMissingConsts;
  function TextEmpty(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 0 to gResLocales.Count - 1 do
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
var
  I: Integer;
  s: string;
begin
  for I := 0 to fTextsTopId do
    if not TextEmpty(I) and TextUnused(I) then
    begin
      s := StringReplace(fTexts[I, gResLocales.IndexByCode(DEFAULT_LOCALE)], ' ', '', [rfReplaceAll]);
      s := UpperCase(LeftStr(s, 16));

      SetLength(fConsts, Length(fConsts) + 1);
      fConsts[High(fConsts)].TextID := I;

      fConsts[High(fConsts)].ConstName := Format('TX_UNUSED_%d_%s', [I, s]);
    end;
end;


procedure TTextManager.SetConst(aIndex: Integer; const Value: TTextInfo);
begin
  fConsts[aIndex] := Value;
end;


procedure TTextManager.Slice(aFirst, aNum: Integer);
var
  I, K: Integer;
begin
  for I := 0 to aNum - 1 do
    for K := 0 to gResLocales.Count - 1 do
      fTexts[I, K] := fTexts[I + aFirst, K];

  SetLength(fTexts, aNum);
  fTextsTopId := aNum - 1;
end;


procedure TTextManager.SaveTextLibraryConsts(aFileName: string);
var
  myFile : TextFile;
  I: integer;
  s: string;
  DefLoc: Integer;
begin
  AssignFile(myFile, aFileName);
  ReWrite(myFile);

  DefLoc := gResLocales.IndexByCode(DEFAULT_LOCALE);

  for I := 0 to High(fConsts) do
  if fConsts[I].TextID = -1 then
    WriteLn(myFile, '')
  else
  begin
    s := '';

    //Append english text for easier lookup from code
    if fTexts[fConsts[I].TextID, DefLoc] <> '' then
    begin
      s := fTexts[fConsts[I].TextID, DefLoc];
      s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
      s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    end;

    s := fConsts[I].ConstName + ' = ' + IntToStr(fConsts[I].TextID) + '; //' + s;

    WriteLn(myFile, s)
  end;

  CloseFile(myFile);
end;


procedure TTextManager.LoadConsts(aConstPath: string);
var
  SL: TStringList;
  Line: string;
  I, K, CenterPos, CommentPos: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aConstPath);

  SetLength(fConsts, SL.Count);

  for I := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[I]);

    CenterPos := Pos(' = ', Line);
    //Separator (line without ' = ')
    if CenterPos = 0 then
    begin
      fConsts[I].TextID := -1;
      fConsts[I].ConstName := '';
    end
    else
    begin
      CommentPos := Pos('; //', Line);
      if CommentPos = 0 then
        fConsts[I].TextID := StrToInt(Copy(Line, CenterPos + 3, Length(Line) - CenterPos - 3))
      else
        fConsts[I].TextID := StrToInt(Copy(Line, CenterPos + 3, CommentPos - CenterPos - 3));
      fConsts[I].ConstName := Copy(Line, 1, CenterPos - 1);
    end;
  end;

  //Ensure there are no duplicates, because that's a very bad situation
  for I := 0 to SL.Count - 1 do
    for K := I+1 to SL.Count - 1 do
      if (fConsts[I].TextID <> -1) and (fConsts[I].TextID = fConsts[K].TextID) then
        ShowMessage('Error: Two constants have the same ID!: '+fConsts[I].ConstName+' & '+fConsts[K].ConstName+' = '+IntToStr(fConsts[I].TextID));

  SL.Free;
end;


procedure TTextManager.LoadText(aTextPath: string; TranslationID: Integer; aCodePage: Word);
var
  SL: TStringList;
  firstDelimiter, topId: Integer;
  I, ID: Integer;
  Line: string;
begin
  if not FileExists(aTextPath) then Exit;

  SL := TStringList.Create;
  SL.Text := ReadTextU(aTextPath, aCodePage);

  for I := SL.Count - 1 downto 0 do
  begin
    firstDelimiter := Pos(':', SL[I]);
    if firstDelimiter = 0 then Continue;

    if TryStrToInt(LeftStr(SL[I], firstDelimiter - 1), topId) then
      Break;
  end;

  Assert(topId <= 1024, 'Dont allow too many strings for no reason');

  fTextsTopId := Max(fTextsTopId, topId, ConstCount);

  SetLength(fTexts, fTextsTopId + 1, gResLocales.Count);

  for I := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[I]);

    firstDelimiter := Pos(':', Line);
    if firstDelimiter = 0 then Continue;
    if not TryStrToInt(TrimLeft(LeftStr(Line, firstDelimiter-1)), ID) then Continue;

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
  SL: TStringList;
  I: Integer;
  s: string;
begin
  SL := TStringList.Create;
  SL.DefaultEncoding := TEncoding.UTF8;

  for I := 0 to fTextsTopId do
  if I <= High(fTexts) then
  if fTexts[I, TranslationID] <> '' then
  begin
    s := IntToStr(I) + ':'+ fTexts[I, TranslationID];
    s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    SL.Add(s);
  end;

  //Don't create blank files for unused translations
  if (SL.Count > 0) or FileExists(aTextPath) then
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
    for I := 0 to gResLocales.Count - 1 do
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
var
  I, K, CurIndex: Integer;
  fOldTexts: array of TStringArray;
begin
  if fConstPath = '' then Exit;

  //Backup current texts
  SetLength(fOldTexts, Length(fTexts), gResLocales.Count);
  for I:=0 to Length(fTexts)-1 do
    for K:=0 to gResLocales.Count-1 do
      fOldTexts[I,K] := fTexts[I,K];

  CurIndex := 0;
  for I:=0 to Length(fConsts)-1 do
  begin
    if fConsts[I].TextID = -1 then
      Continue;

    for K := 0 to gResLocales.Count - 1 do
      fTexts[CurIndex,K] := fOldTexts[fConsts[I].TextID,K];

    fConsts[I].TextID := CurIndex;
    Inc(CurIndex);
  end;
  SetLength(fTexts, CurIndex);
  fTextsTopId := CurIndex - 1;
end;


function TTextManager.ConstCount: Integer;
begin
  if fConstPath <> '' then
    Result := Length(fConsts)
  else
    Result := Length(fTexts);
end;


procedure TTextManager.Insert(aIndex: Integer);
var
  I,K: Integer;
begin
  if fConstPath <> '' then
  begin
    SetLength(fConsts, Length(fConsts) + 1);

    //Move others down
    for I := Length(fConsts)-2 downto aIndex do
      fConsts[I+1] := fConsts[I];

    //Append new strings, they will be empty
    Inc(fTextsTopId);
    SetLength(fTexts, fTextsTopId + 1, gResLocales.Count);

    fConsts[aIndex].TextID := fTextsTopId;
    fConsts[aIndex].ConstName := 'TX_NEW' + IntToStr(fTextsTopId);
  end
  else
  begin
    Inc(fTextsTopId);
    SetLength(fTexts, fTextsTopId + 1, gResLocales.Count);

    //Move down
    for I := fTextsTopId downto aIndex + 1 do
      for K := 0 to gResLocales.Count - 1 do
        fTexts[I,K] := fTexts[I-1,K];

    //Clear
    for I := 0 to gResLocales.Count - 1 do
      fTexts[aIndex,I] := '';
  end;
end;


function TTextManager.TextBlankInAll(aIndex: Integer): Boolean;
var
  K: Integer;
begin
  Result := True;
  for K := 0 to gResLocales.Count - 1 do
    Result := Result and (fTexts[aIndex, K] = '');
end;


procedure TTextManager.DeleteConst(aIndex: Integer);
var I,K: Integer;
begin
  if fConstPath <> '' then
  begin
    if fConsts[aIndex].TextID <> -1 then
    begin
      //Clear all fTexts
      for K := 0 to gResLocales.Count - 1 do
        fTexts[fConsts[aIndex].TextID,K] := '';
    end;

    //Shift fConsts up
    for I := aIndex to Length(fConsts)-2 do
      fConsts[I] := fConsts[I+1];

    SetLength(fConsts, Length(fConsts)-1);
  end
  else
  begin
    //Clear all fTexts
    for K := 0 to gResLocales.Count - 1 do
      fTexts[aIndex,K] := '';
  end;
end;


function TTextManager.GetConst(aIndex: Integer): TTextInfo;
begin
  if fConstPath <> '' then
    Result := fConsts[aIndex]
  else
  begin
    Result.ConstName := StringReplace(fTexts[aIndex, gResLocales.IndexByCode(DEFAULT_LOCALE)], ' ', '', [rfReplaceAll]);
    Result.ConstName := 'TX_' + IntToStr(aIndex) + '_' + UpperCase(LeftStr(Result.ConstName, 16));
    Result.TextID := aIndex;
  end;
end;


function TTextManager.GetText(aIndex: Integer): TStringArray;
begin
  Result := fTexts[aIndex];
end;


procedure TTextManager.InsertSeparator(aIndex: Integer);
var
  i: integer;
begin
  SetLength(fConsts, Length(fConsts) + 1);

  //Move others down
  for i := Length(fConsts)-2 downto aIndex do
    fConsts[i+1] := fConsts[i];

  fConsts[aIndex].TextID := -1;
  fConsts[aIndex].ConstName := '';
end;


procedure TTextManager.MoveUp(aIndex: Integer);
var
  Temp: TTextInfo;
begin
  if aIndex <= 0 then Exit; //Can't move the top item up

  Temp := fConsts[aIndex];
  fConsts[aIndex] := fConsts[aIndex-1];
  fConsts[aIndex-1] := Temp;
end;


procedure TTextManager.MoveDown(aIndex: Integer);
var
  Temp: TTextInfo;
begin
  if aIndex = High(fConsts) then Exit; //Can't move the bottom item down

  Temp := fConsts[aIndex];
  fConsts[aIndex] := fConsts[aIndex+1];
  fConsts[aIndex+1] := Temp;
end;


end.
