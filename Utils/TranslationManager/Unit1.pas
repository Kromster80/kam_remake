unit Unit1;
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}
interface
uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, FileCtrl,
  Dialogs, StrUtils, StdCtrls, Math, ExtCtrls;


type
  TTextInfo = record
               TextID: Integer;
               ConstName: string; //Name used in KM_TextLibrary.pas
             end;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    EditConstName: TEdit;
    Label1: TLabel;
    btnSortByIndex: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    btnInsert: TButton;
    ScrollBox1: TScrollBox;
    btnDelete: TButton;
    btnInsertSeparator: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    cbShowMissing: TComboBox;
    Label2: TLabel;
    cbIncludeSameAsEnglish: TCheckBox;
    LabelIncludeSameAsEnglish: TLabel;
    btnSortByName: TButton;
    btnCompactIndexes: TButton;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnSortByIndexClick(Sender: TObject);
    procedure btnSortByNameClick(Sender: TObject);
    procedure EditConstNameChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertSeparatorClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure cbShowMissingChange(Sender: TObject);
    procedure cbIncludeSameAsEnglishClick(Sender: TObject);
    procedure LabelIncludeSameAsEnglishClick(Sender: TObject);
    procedure btnCompactIndexesClick(Sender: TObject);
  private
    TransMemos: array of TMemo;
    TransLabels: array of TLabel;

    LocalesCount: Integer;
    Locales: array of string;
    DefaultLocale: Integer;

    Texts: array of array of string;
    Consts: array of TTextInfo;

    ListboxLookup: array of integer;

    IgnoreChanges: Boolean;
    procedure MemoChange(Sender: TObject);

    procedure Load(aTextPath: string; aConstPath: string);
    procedure ScanAvailableTranslations(aTextPath: string);
    procedure LoadTextLibraryConsts(aConstPath: string);
    procedure LoadTranslation(aTextPath: string; TranslationID: integer);
    procedure AddMissingConsts;
    procedure DeleteConst(aIndex: Integer);

    procedure SaveTextLibraryConsts(aFileName: string);
    procedure SaveTranslation(aTextPath: string; TranslationID: integer);
    procedure RefreshList;
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


const
  eol: string = #13#10; //EndOfLine
  TextPath = '..\..\data\text\';
  ConstPath = '..\..\KM_TextIDs.inc';


procedure TForm1.FormCreate(Sender: TObject);
begin
  btnLoadClick(btnLoad);
end;


procedure TForm1.Load(aTextPath: string; aConstPath: string);
var
  i: integer;
begin
  LocalesCount := 0;

  SetLength(Consts, 0);
  SetLength(Texts, 0);
  SetLength(Locales, 0);

  //SetLength(IDLookup, 0);

  ScanAvailableTranslations(aTextPath);
  LoadTextLibraryConsts(aConstPath);
  for i := 1 to LocalesCount do
    LoadTranslation(aTextPath + 'text.' + Locales[i] + '.libx', i);

  AddMissingConsts;

  RefreshList;
end;


procedure TForm1.AddMissingConsts;
  function TextEmpty(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 1 to LocalesCount do
      Result := Result and (Trim(Texts[aIndex,I]) = '');
  end;
  function TextUnused(aIndex: Integer): Boolean;
  var I: Integer;
  begin
    Result := True;
    for I := 0 to High(Consts) do
    if Consts[I].TextID = aIndex then
    begin
      Result := False;
      Break;
    end;
  end;
var I: Integer; s: string;
begin
  for I := 0 to High(Texts) do
    if not TextEmpty(I) and TextUnused(I) then
    begin
      s := StringReplace(Texts[I, DefaultLocale], ' ', '', [rfReplaceAll]);
      s := UpperCase(LeftStr(s, 16));

      SetLength(Consts, Length(Consts) + 1);
      Consts[High(Consts)].TextID := I;

      Consts[High(Consts)].ConstName := 'TX_UNUSED_' + IntToStr(I) + '_' + s;
    end;
end;


procedure TForm1.RefreshList;
  function ShowConst(aIndex: Integer): Boolean;
  begin
    if cbShowMissing.ItemIndex = 0 then
      Result := True
    else
    begin
      Result := (Consts[aIndex].TextID <> -1) and
                (
                  (Texts[Consts[aIndex].TextID, cbShowMissing.ItemIndex] = '') or
                  (
                    cbIncludeSameAsEnglish.Checked and
                    (Texts[Consts[aIndex].TextID, cbShowMissing.ItemIndex] = Texts[Consts[aIndex].TextID, DefaultLocale])
                  )
                );
    end;
  end;
var
  I, TopIdx, ItemIdx: Integer;
  s: string;
begin
  ListBox1.Items.BeginUpdate;
  ItemIdx := ListBox1.ItemIndex;
  TopIdx := ListBox1.TopIndex;
  ListBox1.Clear;

  SetLength(ListboxLookup, 0);
  SetLength(ListboxLookup, Length(Consts));

  for I := 0 to High(Consts) do
  if ShowConst(I) then
  begin
    ListboxLookup[ListBox1.Items.Count] := I;
    if Consts[I].TextID = -1 then
      s := ''
    else
      s := IntToStr(Consts[I].TextID) + ': ' + Consts[I].ConstName;

    ListBox1.Items.Add(s);
  end;

  ListBox1.Items.EndUpdate;
  ListBox1.ItemIndex := EnsureRange(ItemIdx, 0, ListBox1.Count - 1);
  ListBox1.TopIndex := TopIdx;
  ListBox1Click(ListBox1);

  Label3.Caption := 'Count ' + IntToStr(ListBox1.Count);
end;


procedure TForm1.ScanAvailableTranslations(aTextPath: string);

  function GetCharset(aLang:string):TFontCharset;
  begin
    Result := DEFAULT_CHARSET;
    if aLang = 'rus' then Result := RUSSIAN_CHARSET;
    if aLang = 'bul' then Result := RUSSIAN_CHARSET;
    if aLang = 'pol' then Result := EASTEUROPE_CHARSET;
    if aLang = 'hun' then Result := EASTEUROPE_CHARSET;
    if aLang = 'cze' then Result := EASTEUROPE_CHARSET;
    if aLang = 'svk' then Result := EASTEUROPE_CHARSET;
  end;

var
  SearchRec:TSearchRec;
  i:integer;
begin
  Assert(DirectoryExists(aTextPath), 'Misc folder does not exist: '+aTextPath);

  FindFirst(aTextPath+'*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then //Exclude parent folders
      //Check files
      if (SearchRec.Attr and faDirectory <> faDirectory)
      and (LeftStr(SearchRec.Name,length('text.')) = 'text.')
      and (RightStr(SearchRec.Name,length('.libx')) = '.libx') then
      begin
        inc(LocalesCount);
        SetLength(Locales,LocalesCount+1);
        Locales[LocalesCount] := Copy(SearchRec.Name,length('text')+2,3);
      end;
  until (FindNext(SearchRec)<>0);

  for i:=1 to Length(TransLabels)-1 do
  begin
    FreeAndNil(TransLabels[i]);
    FreeAndNil(TransMemos[i]);
  end;

  SetLength(TransMemos, LocalesCount + 1);
  SetLength(TransLabels, LocalesCount + 1);

  cbShowMissing.Items.Clear;
  cbShowMissing.Items.Add('None');
  DefaultLocale := 1; //Default in case there is no eng
  for i:=1 to LocalesCount do
  begin
    if Locales[i] = 'eng' then DefaultLocale := i;
    TransLabels[i] := TLabel.Create(Form1);
    TransLabels[i].Parent := ScrollBox1;
    TransLabels[i].SetBounds(8,4+(i-1)*80,30,30);
    TransLabels[i].Caption := Locales[i];

    TransMemos[i] := TMemo.Create(Form1);
    TransMemos[i].Parent := ScrollBox1;
    TransMemos[i].SetBounds(8,22+(i-1)*80,ScrollBox1.Width-16,60);
    TransMemos[i].Anchors := [akLeft,akRight,akTop];
    TransMemos[i].Font.Charset := GetCharset(Locales[i]);
    TransMemos[i].Tag := i;
    TransMemos[i].OnChange := MemoChange;

    cbShowMissing.Items.Add(Locales[i]);
  end;
  cbShowMissing.ItemIndex := 0;
end;


procedure TForm1.SaveTextLibraryConsts(aFileName: string);
var
  myFile : TextFile;
  I: integer;
begin
  AssignFile(myFile, aFileName);
  ReWrite(myFile);

  for I := 0 to High(Consts) do
    if Consts[I].TextID = -1 then
      WriteLn(myFile, '')
    else
      WriteLn(myFile, Consts[I].ConstName + ' = ' + IntToStr(Consts[I].TextID) + ';');

  CloseFile(myFile);
end;


procedure TForm1.LoadTextLibraryConsts(aConstPath: string);
const Size_Inc = 100;
var
  SL: TStringList;
  Line: string;
  i, CenterPos: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aConstPath);

  SetLength(Consts, SL.Count);

  for I := 0 to SL.Count - 1 do
  begin
    Line := Trim(SL[I]);

    CenterPos := Pos(' = ',Line);
    //Separator (line without ' = ')
    if CenterPos = 0 then
    begin
      Consts[I].TextID := -1;
      Consts[I].ConstName := '';
    end
    else
    begin
      Consts[I].TextID := StrToInt(Copy(Line, CenterPos + 3, Length(Line) - CenterPos - 3));
      Consts[I].ConstName := Copy(Line, 1, CenterPos - 1);
    end;
  end;

  SL.Free;
end;


procedure TForm1.LoadTranslation(aTextPath: string; TranslationID: Integer);
var
  SL:TStringList;
  i,ID,firstDelimiter:integer;
  Line:string;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(aTextPath);

  SetLength(Texts, 3000);
  for I := 0 to 3000 - 1 do
    SetLength(Texts[I], LocalesCount+1);

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

    Texts[ID, TranslationID] := Line;
  end;

  SL.Free;
end;


procedure TForm1.SaveTranslation(aTextPath: string; TranslationID: Integer);
var
  aStringList:TStringList;
  i:integer;
  s:string;
begin
  aStringList := TStringList.Create;

  aStringList.Add('');
  aStringList.Add('MaxID:' + IntToStr(3000));
  aStringList.Add('');
  for i := 0 to Length(Texts) - 1 do
  if Texts[i, TranslationID] <> '' then
  begin
    s := IntToStr(I) + ':'+ Texts[i, TranslationID];
    s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
    s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    aStringList.Add(s);
  end;

  aStringList.SaveToFile(aTextPath);
  aStringList.Free;
end;


procedure TForm1.ListBox1Click(Sender: TObject);
var i,ID:integer;
begin
  if (Length(Consts) = 0) or (ListBox1.ItemIndex = -1) then exit;

  IgnoreChanges := true;
  ID := ListboxLookup[ListBox1.ItemIndex];
  EditConstName.Text := Consts[ID].ConstName;

  for i:=1 to LocalesCount do
    if Consts[ID].TextID <> -1 then
      TransMemos[i].Text := {$IFDEF FPC}AnsiToUTF8{$ENDIF}(Texts[Consts[ID].TextID, i])
    else
      TransMemos[i].Text := '';
  IgnoreChanges := false;
end;


//Sort the items by TextID
procedure TForm1.btnSortByIndexClick(Sender: TObject);
  function Compare(A, B: Integer): Boolean;
  begin
    Result := Consts[A].TextID > Consts[B].TextID;
  end;

  procedure Swap(A, B: Integer);
  var Temp: TTextInfo;
  begin
    Temp := Consts[A]; Consts[A] := Consts[B]; Consts[B] := Temp;
  end;

var
  I, K: Integer;
begin
  //Delete separators
  for I := High(Consts) downto 0 do
    if Consts[I].TextID = -1 then
    begin
      for K := I to Length(Consts)-2 do
        Consts[K] := Consts[K+1];
      SetLength(Consts, Length(Consts) - 1);
    end;

  //Sort
  for I := 0 to High(Consts) do
  for K:= I + 1 to High(Consts) do
  if Compare(I, K) then
    Swap(I, K);

  //Add separators anew between grouped areas
  for I := 1 to High(Consts) do
    if (Consts[I-1].TextID <> -1) and (Consts[I-1].TextID+1 <> Consts[I].TextID) then
    begin
      SetLength(Consts, Length(Consts) + 1);
      for K := Length(Consts)-1 downto I+1 do
        Consts[K] := Consts[K-1];
      Consts[I].TextID := -1;
      Consts[I].ConstName := '';
    end;

  RefreshList;
end;


//Sort TextIDs by Index
procedure TForm1.btnSortByNameClick(Sender: TObject);
  function Compare(A, B: Integer): Boolean;
  begin
    Result := Consts[A].TextID > Consts[B].TextID;
  end;

  procedure Swap(A, B: Integer);
  var
    I: Integer;
    S: string;
    T: Integer;
  begin
    T := Consts[A].TextID; Consts[A].TextID := Consts[B].TextID; Consts[B].TextID := T;
    for I := 1 to LocalesCount do
    begin
      S := Texts[Consts[A].TextID, I];
      Texts[Consts[A].TextID, I] := Texts[Consts[B].TextID, I];
      Texts[Consts[B].TextID, I] := S;
    end;
  end;

var
  I, K: Integer;
begin
  //Sort
  for I := 0 to High(Consts) do
  if Consts[I].TextID <> -1 then
    for K:= I + 1 to High(Consts) do
    if Consts[K].TextID <> -1 then
      if Compare(I,K) then
        Swap(I,K);

  //Compact Indexes
  {for I := 1 to High(Consts) - 1 do
  if (Consts[I].TextID = -1) and (Consts[I-1].TextID = Consts[I+1].TextID+1) then
    InsertConst()}

  RefreshList;
end;


procedure TForm1.btnCompactIndexesClick(Sender: TObject);
var
  I: Integer;
  Used: array of Word;
begin
  {SetLength(Used, Length(Texts));
  for I := 0 to High(Used) do
    Used[I] := 1;

  for I := 0 to High(Consts) do
    if Consts[I].TextID <> -1 then
      Used[Consts[I].TextID] := 0;

  for I := 1 to High(Consts) do
  if (Consts[I].TextID = -1) and (Consts[I-1].TextID = -1) then
    DeleteConst(I);

  RefreshList;}
end;


procedure TForm1.EditConstNameChange(Sender: TObject);
var ID: Integer;
begin
  if IgnoreChanges then exit;
  ID := ListboxLookup[ListBox1.ItemIndex];
  if Consts[ID].TextID = -1 then exit;
  Consts[ID].ConstName := EditConstName.Text;
  ListBox1.Items[ListBox1.ItemIndex] := IntToStr(Consts[ID].TextID)+': '+Consts[ID].ConstName;
end;


procedure TForm1.MemoChange(Sender: TObject);
var ID,t:integer;
begin
  if IgnoreChanges then exit;
  ID := ListboxLookup[ListBox1.ItemIndex];
  if Consts[ID].TextID = -1 then exit;
  t := TMemo(Sender).Tag;
  Texts[Consts[ID].TextID, t] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);
end;


procedure TForm1.btnLoadClick(Sender: TObject);
begin
  //if SelectDirectory(WorkDir, [], 0) then
  Load(TextPath, ConstPath);
end;


procedure TForm1.btnSaveClick(Sender: TObject);
var i:integer;
begin
  SaveTextLibraryConsts(ConstPath);
  for i:=1 to LocalesCount do
    SaveTranslation(TextPath+'text.'+Locales[i]+'.libx',i);
end;


procedure TForm1.btnInsertClick(Sender: TObject);
var i,ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding

  SetLength(Consts, Length(Consts) + 1);

  //Move others down
  for i := Length(Consts)-2 downto ID do
    Consts[i+1] := Consts[i];

  SetLength(Texts, Length(Texts) + 1);
  SetLength(Texts[High(Texts)], LocalesCount+1);
  for i:=1 to LocalesCount do
    Texts[ID,i] := '';

  Consts[ID].TextID := High(Texts);
  Consts[ID].ConstName := 'TX_NEW'+IntToStr(High(Texts));

  ListBox1.Items.Insert(ID, IntToStr(Consts[ID].TextID) + ': '+Consts[ID].ConstName);
  ListBox1.ItemIndex := ListBox1.ItemIndex;
  ListBox1Click(Listbox1); //Force select the new item
end;


procedure TForm1.DeleteConst(aIndex: Integer);
var i,k: integer;
begin
  if Consts[aIndex].TextID <> -1 then
  begin
    //Shift all texts up
    for i := Consts[aIndex].TextID to High(Texts)-1 do
      for k := 1 to LocalesCount do
        Texts[i,k] := Texts[i+1,k];

    //Shift pointers
    for i := 0 to High(Consts) do
      if Consts[i].TextID > Consts[aIndex].TextID then
        Dec(Consts[i].TextID);
  end;

  //Shift consts up
  for i := aIndex to Length(Consts)-2 do
    Consts[i] := Consts[i+1];

  SetLength(Consts, Length(Consts)-1);
end;


procedure TForm1.btnDeleteClick(Sender: TObject);
var i,k,ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are deleting
  if ID = -1 then Exit;

  DeleteConst(ID);

  ListBox1.Items.Delete(ID);
  RefreshList;
  ListBox1.ItemIndex := ID;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var i,ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding

  SetLength(Consts, Length(Consts) + 1);

  //Move others down
  for i := Length(Consts)-2 downto ID do
    Consts[i+1] := Consts[i];

  Consts[ID].TextID := -1;
  Consts[ID].ConstName := '';

  ListBox1.Items.Insert(ID, '');
  ListBox1.ItemIndex := ListBox1.ItemIndex;
  ListBox1Click(Listbox1); //Force select the new item
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var ID: integer; Temp: TTextInfo;
begin
  ID := ListBox1.ItemIndex;
  if ID <= 0 then Exit; //Can't move the top item up

  Temp := Consts[ID];
  Consts[ID] := Consts[ID-1];
  Consts[ID-1] := Temp;

  ListBox1.Items.Move(ID, ID-1);
  ListBox1.ItemIndex := ID-1;
end;


procedure TForm1.btnMoveDownClick(Sender: TObject);
var ID: integer; Temp: TTextInfo;
begin
  ID := ListBox1.ItemIndex;
  if ID = High(Consts) then Exit; //Can't move the bottom item down

  Temp := Consts[ID];
  Consts[ID] := Consts[ID+1];
  Consts[ID+1] := Temp;
  ListBox1.Items.Move(ID, ID+1);
  ListBox1.ItemIndex := ID;
end;


procedure TForm1.cbShowMissingChange(Sender: TObject);
var
  Filter: Boolean;
begin
  RefreshList;
  Filter := cbShowMissing.ItemIndex > 0;

  //Disable buttons
  btnSortByIndex.Enabled := not Filter;
  btnSortByName.Enabled := not Filter;
  btnInsert.Enabled := not Filter;
  btnDelete.Enabled := not Filter;
  btnInsertSeparator.Enabled := not Filter;
  btnMoveUp.Enabled := not Filter;
  btnMoveDown.Enabled := not Filter;
  cbIncludeSameAsEnglish.Enabled := Filter;
  LabelIncludeSameAsEnglish.Enabled := Filter;

  //Select the first item
  ListBox1.ItemIndex := 0;
  ListBox1Click(ListBox1);
end;


procedure TForm1.cbIncludeSameAsEnglishClick(Sender: TObject);
begin
  cbShowMissingChange(cbIncludeSameAsEnglish);
end;


procedure TForm1.LabelIncludeSameAsEnglishClick(Sender: TObject);
begin
  cbIncludeSameAsEnglish.Checked := not cbIncludeSameAsEnglish.Checked;
  cbIncludeSameAsEnglishClick(LabelIncludeSameAsEnglish);
end;


end.
