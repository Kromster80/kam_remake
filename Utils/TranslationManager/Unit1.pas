unit Unit1;
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, StdCtrls, Math, ExtCtrls;

const
  eol:string=#13+#10; //EndOfLine
  MiscFolder = '..\..\data\misc\';
  TextLibraryFile = '..\..\KM_TextIDs.inc';

type
  TTextInfo = record
               ID:integer;
               ConstName:string; //Name used in KM_TextLibrary.pas
               Translations:array of string;
             end;
type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    EditConstName: TEdit;
    Label1: TLabel;
    btnReorderList: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    btnInsert: TButton;
    ScrollBox1: TScrollBox;
    btnDelete: TButton;
    btnInsertSeparator: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnReorderListClick(Sender: TObject);
    procedure EditConstNameChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertSeparatorClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
  private
    { Private declarations }
    TransMemos: array of TMemo;
    TransLabels: array of TLabel;

    TranslationCount:integer;
    TranslationCodes:array of string;
    TextsCount:integer;
    Texts:array of TTextInfo;
    IDLookup:array of integer;
    MaxID:integer;
    LastSelected:integer;
    IgnoreChanges:boolean;
    procedure MemoChange(Sender: TObject);

    procedure Load(aMiscFolder:string; aTextLibraryFile:string);
    procedure ScanAvailableTranslations(aMiscFolder:string);
    procedure LoadTextLibraryConsts(aFileName:string);
    procedure LoadTranslation(aFileName:string; TranslationID:integer);

    procedure SaveTextLibraryConsts(aFileName:string);
    procedure SaveTranslation(aFileName:string; TranslationID:integer);
    procedure RefreshList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  btnLoadClick(btnLoad);
end;


procedure TForm1.Load(aMiscFolder:string; aTextLibraryFile:string);
var i:integer;
begin
  TextsCount := 0;
  TranslationCount := 0;
  MaxID := 0;
  SetLength(Texts,0);
  SetLength(TranslationCodes,0);
  SetLength(IDLookup,0);

  ScanAvailableTranslations(aMiscFolder);
  LoadTextLibraryConsts(aTextLibraryFile);
  for i:=1 to TranslationCount do
    LoadTranslation(aMiscFolder+'remake.'+TranslationCodes[i]+'.libx',i);
  RefreshList;
end;


procedure TForm1.RefreshList;
var i:integer; ID:string;
begin
  ListBox1.Clear;
  for i:=1 to TextsCount do
  begin
    if Texts[i].ID = -1 then
      ID := '' else ID := IntToStr(Texts[i].ID)+': ';
    ListBox1.Items.Add(ID+Texts[i].ConstName);
  end;
  if InRange(LastSelected,0,ListBox1.Count-1) then
    ListBox1.ItemIndex := LastSelected
  else
    ListBox1.ItemIndex := 0;
  ListBox1Click(ListBox1);
end;


procedure TForm1.ScanAvailableTranslations(aMiscFolder:string);

  function GetCharset(aLang:string):TFontCharset;
  begin
    Result := DEFAULT_CHARSET;
    if aLang = 'rus' then Result := RUSSIAN_CHARSET;
    if aLang = 'pol' then Result := EASTEUROPE_CHARSET;
    if aLang = 'hun' then Result := EASTEUROPE_CHARSET;
    if aLang = 'cze' then Result := EASTEUROPE_CHARSET;
    if aLang = 'svk' then Result := EASTEUROPE_CHARSET;
    if aLang = 'cze' then Result := EASTEUROPE_CHARSET;
  end;

var
  SearchRec:TSearchRec;
  i:integer;
begin
  Assert(DirectoryExists(aMiscFolder),'Misc folder does not exist: '+aMiscFolder);
  FindFirst(aMiscFolder+'*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then //Exclude parent folders
      //Check files
      if (SearchRec.Attr and faDirectory <> faDirectory)
      and (LeftStr(SearchRec.Name,length('remake.')) = 'remake.')
      and (RightStr(SearchRec.Name,length('.libx')) = '.libx') then
      begin
        inc(TranslationCount);
        SetLength(TranslationCodes,TranslationCount+1);
        TranslationCodes[TranslationCount] := Copy(SearchRec.Name,length('remake')+2,3);
      end;
  until (FindNext(SearchRec)<>0);

  for i:=1 to Length(TransLabels)-1 do
  begin
    FreeAndNil(TransLabels[i]);
    FreeAndNil(TransMemos[i]);
  end;
  SetLength(TransMemos,TranslationCount+1);
  SetLength(TransLabels,TranslationCount+1);
  for i:=1 to TranslationCount do
  begin
    TransLabels[i] := TLabel.Create(Form1);
    TransLabels[i].Parent := ScrollBox1;
    TransLabels[i].SetBounds(8,4+(i-1)*80,30,30);
    TransLabels[i].Caption := TranslationCodes[i];

    TransMemos[i] := TMemo.Create(Form1);
    TransMemos[i].Parent := ScrollBox1;
    TransMemos[i].SetBounds(8,24+(i-1)*80,ScrollBox1.Width-16,60);
    TransMemos[i].Anchors := [akLeft,akRight,akTop];
    TransMemos[i].Font.Charset := GetCharset(TranslationCodes[i]);
    TransMemos[i].Tag := i;
    TransMemos[i].OnChange := MemoChange;
  end;
end;


procedure TForm1.SaveTextLibraryConsts(aFileName:string);
var
  myFile : TextFile;
  i: integer;
begin
  AssignFile(myFile, aFileName);
  ReWrite(myFile);

  for i:=1 to TextsCount do
  begin
    if Texts[i].ID = -1 then
      WriteLn(myFile, '')
    else
      WriteLn(myFile, Texts[i].ConstName+' = '+IntToStr(Texts[i].ID+2000)+';')
  end;

  CloseFile(myFile);
end;


procedure TForm1.LoadTextLibraryConsts(aFileName:string);
const Size_Inc = 100;
var
  myFile : TextFile;
  Line: string;
  i,CenterPos:integer;
begin
  AssignFile(myFile, aFileName);
  Reset(myFile);
  while not Eof(myFile) do
  begin
    ReadLn(myFile, Line);
    Line := Trim(Line);
    inc(TextsCount);
    if Length(Texts) <= TextsCount then
      SetLength(Texts,TextsCount+Size_Inc);
    CenterPos := Pos(' = ',Line);
    //Separator (line without ' = ')
    if CenterPos = 0 then
    begin
      Texts[TextsCount].ID := -1;
      Texts[TextsCount].ConstName := '';
    end
    else
    begin
      Texts[TextsCount].ID := StrToInt(Copy(Line,CenterPos+3,Length(Line)-CenterPos-3))-2000;
      MaxID := Math.Max(MaxID, Texts[TextsCount].ID);
      Texts[TextsCount].ConstName := Copy(Line,1,CenterPos-1);
      SetLength(Texts[TextsCount].Translations,TranslationCount+1);
      for i:=1 to TranslationCount do
        Texts[TextsCount].Translations[i] := '';
      //Reverse lookup for loading translations
      if Length(IDLookup) <= Texts[TextsCount].ID then
        SetLength(IDLookup,Texts[TextsCount].ID+Size_Inc);
      IDLookup[Texts[TextsCount].ID] := TextsCount;
    end;
  end;

  CloseFile(myFile);
end;


procedure TForm1.LoadTranslation(aFileName:string; TranslationID:integer);
var
  aStringList:TStringList;
  i,ID,firstDelimiter:integer;
  s:string;
begin
  aStringList := TStringList.Create;
  aStringList.LoadFromFile(aFileName);

  for i:=0 to aStringList.Count-1 do
  begin
    s := aStringList[i];

    firstDelimiter := Pos(':', s);
    if firstDelimiter=0 then continue;
    
    if not TryStrToInt(TrimLeft(LeftStr(s, firstDelimiter-1)), ID) then continue;

    if ID <= Length(IDLookup) then
    begin
      s := RightStr(s, Length(s)-firstDelimiter);
      //Required characters that can't be stored in plain text
      s := StringReplace(s, '\n', eol, [rfReplaceAll, rfIgnoreCase]); //EOL
      s := StringReplace(s, '\\', '\', [rfReplaceAll, rfIgnoreCase]); //Slash
      Texts[IDLookup[ID]].Translations[TranslationID] := s;
    end;
  end;

  aStringList.Free;
end;


procedure TForm1.SaveTranslation(aFileName:string; TranslationID:integer);
var
  aStringList:TStringList;
  i:integer;
  s:string;
begin
  aStringList := TStringList.Create;

  aStringList.Add('');
  aStringList.Add('MaxID:'+IntToStr(MaxID));
  aStringList.Add('');
  for i:=1 to TextsCount do
  begin
    if Texts[i].ID = -1 then
      s := ''
    else
    begin
      s := IntToStr(Texts[i].ID) + ':'+ Texts[i].Translations[TranslationID];
      s := StringReplace(s, '\', '\\', [rfReplaceAll, rfIgnoreCase]); //Slash
      s := StringReplace(s, eol, '\n', [rfReplaceAll, rfIgnoreCase]); //EOL
    end;
    aStringList.Add(s);
  end;

  aStringList.SaveToFile(aFileName);
  aStringList.Free;
end;


procedure TForm1.ListBox1Click(Sender: TObject);
var i,ID:integer;
begin
  IgnoreChanges := true;
  if TextsCount = 0 then exit;
  ID := ListBox1.ItemIndex+1;
  EditConstName.Text := Texts[ID].ConstName;
  LastSelected := ID-1;
  for i:=1 to TranslationCount do
    if Texts[ID].ID <> -1 then
      TransMemos[i].Text := {$IFDEF FPC}AnsiToUTF8{$ENDIF}(Texts[ID].Translations[i])
    else
      TransMemos[i].Text := '';
  IgnoreChanges := false;
end;

procedure TForm1.btnReorderListClick(Sender: TObject);
var i,BlanksCount:integer;
begin
  BlanksCount := 0;
  for i:=1 to TextsCount do
  begin
    if Texts[i].ID = -1 then
      inc(BlanksCount)
    else
      Texts[i].ID := i-BlanksCount;
  end;
  MaxID := TextsCount-BlanksCount;
  RefreshList;
end;

procedure TForm1.EditConstNameChange(Sender: TObject);
var i:integer;
begin
  if IgnoreChanges then exit;
  i := ListBox1.ItemIndex+1;
  if Texts[i].ID = -1 then exit;
  Texts[i].ConstName := EditConstName.Text;
  ListBox1.Items[ListBox1.ItemIndex] := IntToStr(Texts[i].ID)+': '+Texts[i].ConstName;
end;


procedure TForm1.MemoChange(Sender: TObject);
var i,t:integer;
begin
  if IgnoreChanges then exit;
  i := ListBox1.ItemIndex+1;
  if Texts[i].ID = -1 then exit;
  t := TMemo(Sender).Tag;
  Texts[i].Translations[t] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);
end;


procedure TForm1.btnLoadClick(Sender: TObject);
begin
  Load(MiscFolder,TextLibraryFile);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var i:integer;
begin
  SaveTextLibraryConsts(TextLibraryFile);
  for i:=1 to TranslationCount do
    SaveTranslation(MiscFolder+'remake.'+TranslationCodes[i]+'.libx',i);
end;

procedure TForm1.btnInsertClick(Sender: TObject);
var i,ID: integer;
begin
  ID := ListBox1.ItemIndex+2; //Item place we are adding
  inc(TextsCount);
  SetLength(Texts,Length(Texts)+1);
  //Move others down
  for i:=TextsCount downto ID do
  begin
    Texts[i+1] := Texts[i];
  end;
  inc(MaxID);
  Texts[ID].ID := MaxID;
  Texts[ID].ConstName := '';
  SetLength(Texts[ID].Translations,TranslationCount+1);
  for i:=1 to TranslationCount do
    Texts[ID].Translations[i] := '';

  ListBox1.Items.Insert(ID-1, IntToStr(Texts[ID].ID)+': '+Texts[ID].ConstName);
  ListBox1.ItemIndex := ListBox1.ItemIndex+1;
  ListBox1Click(Listbox1); //Force select the new item
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var i,ID: integer;
begin
  ID := ListBox1.ItemIndex+1; //Item place we are deleting
  if Texts[ID].ID = MaxID then dec(MaxID);
  //Move items up
  for i:=ID to TextsCount do
  begin
    Texts[i] := Texts[i+1];
  end;
  dec(TextsCount);
  ListBox1.Items.Delete(ListBox1.ItemIndex);
  ListBox1.ItemIndex := max(0,ID-2);
  ListBox1Click(Listbox1); //Force select the new item
end;

procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var i,ID: integer;
begin
  ID := ListBox1.ItemIndex+2; //Item place we are adding
  SetLength(Texts,Length(Texts)+1);
  inc(TextsCount);
  //Move others down
  for i:=TextsCount downto ID do
  begin
    Texts[i+1] := Texts[i];
  end;
  Texts[ID].ID := -1;
  Texts[ID].ConstName := '';
  SetLength(Texts[ID].Translations,TranslationCount+1);
  for i:=1 to TranslationCount do
    Texts[ID].Translations[i] := '';

  ListBox1.Items.Insert(ID-1, '');
  ListBox1.ItemIndex := ListBox1.ItemIndex+1;
  ListBox1Click(Listbox1); //Force select the new item
end;

procedure TForm1.btnMoveUpClick(Sender: TObject);
var ID: integer; Temp: TTextInfo;
begin
  ID := ListBox1.ItemIndex+1;
  if ID = 1 then exit; //Can't move the top item up
  Temp := Texts[ID];
  Texts[ID] := Texts[ID-1];
  Texts[ID-1] := Temp;
  ListBox1.Items.Move(ID-1,ID-2);
  ListBox1.ItemIndex := ID-2;
end;

procedure TForm1.btnMoveDownClick(Sender: TObject);
var ID: integer; Temp: TTextInfo;
begin
  ID := ListBox1.ItemIndex+1;
  if ID = TextsCount then exit; //Can't move the bottom item down
  Temp := Texts[ID];
  Texts[ID] := Texts[ID+1];
  Texts[ID+1] := Temp;
  ListBox1.Items.Move(ID-1,ID);
  ListBox1.ItemIndex := ID;
end;

end.
