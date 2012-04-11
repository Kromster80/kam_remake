unit Unit_Text;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics, Math,
  StdCtrls, StrUtils, SysUtils, Windows, KM_Locales;


type
  TTextInfo = record
               TextID: Integer;
               ConstName: string; //Name used in KM_TextLibrary.pas
             end;

  TStringArray = array of string;

type
  TTextManager = class
  private
    fUseConsts: Boolean; //We use consts only for ingame library, others don't need them
    fTextMaxID: Integer;
    fTexts: array of TStringArray;
    fConsts: array of TTextInfo;

    function GetConst(aIndex: Integer): TTextInfo;
    function GetText(aIndex: Integer): TStringArray;

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
var I: Integer;
begin
  SetLength(fConsts, 0);
  SetLength(fTexts, 0);

  fUseConsts := aConstPath <> '';
  fTextMaxID := -1;

  if fUseConsts then
    LoadConsts(aConstPath);

  for I := 0 to fLocales.Count - 1 do
    LoadText(Format(aTextPath, [fLocales[I].Code]), I);

  if fUseConsts then
    AddMissingConsts;
end;


procedure TTextManager.Save(aTextPath: string; aConstPath: string);
var I: Integer;
begin
  if fUseConsts then
    SaveTextLibraryConsts(aConstPath);

  for I := 0 to fLocales.Count - 1 do
    SaveTranslation(Format(aTextPath, [fLocales[I].Code]), I);
end;


procedure TTextManager.AddMissingConsts;
  function TextEmpty(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 0 to fLocales.Count - 1 do
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
  for I := 0 to fTextMaxID do
    if not TextEmpty(I) and TextUnused(I) then
    begin
      s := StringReplace(fTexts[I, fLocales.GetIDFromCode(DEFAULT_LOCALE)], ' ', '', [rfReplaceAll]);
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
var I, K: Integer;
begin
  for I := 0 to aNum - 1 do
    for K := 0 to fLocales.Count - 1 do
      fTexts[I, K] := fTexts[I + aFirst, K];

  SetLength(fTexts, aNum);
  fTextMaxID := aNum-1;
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
  if not FileExists(aTextPath) then Exit;

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
    if (fTextMaxID <> -1) and (fTextMaxID <> T) then
    begin
      ShowMessage(aTextPath + ' MaxID is inconsistent with other files');
      B := False;
    end
    else
      fTextMaxID := T;
  end
  else
  begin
    ShowMessage(aTextPath + ' has no MaxID entry');
    B := False;
  end;

  if B then
  begin
    SetLength(fTexts, fTextMaxID + 1, fLocales.Count);

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
  SL.Add('MaxID:' + IntToStr(fTextMaxID));
  SL.Add('');
  for I := 0 to fTextMaxID do
  if I <= High(fTexts) then
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
    for I := 0 to fLocales.Count - 1 do
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
  if fUseConsts then
    Result := Length(fConsts)
  else
    Result := Length(fTexts);
end;


procedure TTextManager.Insert(aIndex: Integer);
var I,K: Integer;
begin
  if fUseConsts then
  begin
    SetLength(fConsts, Length(fConsts) + 1);

    //Move others down
    for I := Length(fConsts)-2 downto aIndex do
      fConsts[I+1] := fConsts[I];

    //Append new strings, they will be empty
    Inc(fTextMaxID);
    SetLength(fTexts, fTextMaxID + 1, fLocales.Count);

    fConsts[aIndex].TextID := fTextMaxID;
    fConsts[aIndex].ConstName := 'TX_NEW' + IntToStr(fTextMaxID);
  end
  else
  begin
    Inc(fTextMaxID);
    SetLength(fTexts, fTextMaxID + 1, fLocales.Count);

    //Move down
    for I := fTextMaxID downto aIndex + 1 do
      for K := 0 to fLocales.Count - 1 do
        fTexts[I,K] := fTexts[I-1,K];

    //Clear
    for I := 0 to fLocales.Count - 1 do
      fTexts[aIndex,I] := '';
  end;
end;


procedure TTextManager.DeleteConst(aIndex: Integer);
var I,K: Integer;
begin
  if fUseConsts then
  begin
    if fConsts[aIndex].TextID <> -1 then
    begin
      //Clear all fTexts
      for K := 0 to fLocales.Count - 1 do
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
    for K := 0 to fLocales.Count - 1 do
      fTexts[aIndex,K] := '';
  end;
end;


function TTextManager.GetConst(aIndex: Integer): TTextInfo;
begin
  if fUseConsts then
    Result := fConsts[aIndex]
  else
  begin
    Result.ConstName := StringReplace(fTexts[aIndex, fLocales.GetIDFromCode(DEFAULT_LOCALE)], ' ', '', [rfReplaceAll]);
    Result.ConstName := 'TX_' + IntToStr(aIndex) + '_' + UpperCase(LeftStr(Result.ConstName, 16));
    Result.TextID := aIndex;
  end;
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
