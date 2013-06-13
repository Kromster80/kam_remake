unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus,
  {$IFDEF MSWINDOWS} ComCtrls, FileCtrl, {$ENDIF}
  StdCtrls, StrUtils, Windows, SysUtils, CheckLst, INIFiles,
  KM_Defaults, KM_Locales, Unit_Text, Unit_PathManager;

const
  USER_MODE = False; //Disables insert, delete, compact, sort, etc. functions so translators don't click them by mistake

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    lblConstName: TLabel;
    btnInsert: TButton;
    ScrollBox1: TScrollBox;
    btnDelete: TButton;
    btnInsertSeparator: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    Label3: TLabel;
    Button1: TButton;
    lbFolders: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
    Label4: TLabel;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit2: TMenuItem;
    mnuSave: TMenuItem;
    mnuSortByIndex: TMenuItem;
    mnuSortByName: TMenuItem;
    mnuCompactIndexes: TMenuItem;
    mnuListUnused: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    btnRename: TButton;
    clbShowLang: TCheckListBox;
    cbShowMis: TCheckBox;
    cbShowDup: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnSortByIndexClick(Sender: TObject);
    procedure btnSortByNameClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertSeparatorClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnCompactIndexesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure lbFoldersClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbShowLang8888Change(Sender: TObject);
    procedure btnUnusedClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure clbShowLangClickCheck(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbShowMisClick(Sender: TObject);
  private
    fPathManager: TPathManager;
    fTextManager: TTextManager;
    fExeDir: string;
    fWorkDir: string;
    fBuffer: array of string;

    TransMemos: array of TMemo;
    TransLabels: array of TLabel;
    ListboxLookup: array of Integer;
    IgnoreChanges: Boolean;
    fPreviousFolder: Integer;
    procedure MemoChange(Sender: TObject);

    procedure InitLocalesList;
    procedure RefreshFolders;
    procedure RefreshFilter;
    procedure RefreshLocales;
    procedure RefreshList;
    procedure LoadSettings(aPath: string);
    procedure SaveSettings(aPath: string);
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


const
  eol: string = #13#10; //EndOfLine
  //TextPath = '..\..\data\text\';
  //ConstPath = '..\..\KM_TextIDs.inc';


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Remake Translation Manager (' + GAME_REVISION + ')';

  fExeDir := ExtractFilePath(ParamStr(0));
  fWorkDir := fExeDir + '..\..\';
  fLocales := TKMLocales.Create(fWorkDir + 'data\locales.txt');

  InitLocalesList;

  RefreshLocales;

  fPathManager := TPathManager.Create;
  RefreshFolders;

  fTextManager := TTextManager.Create;

  mnuSave.Enabled := False;

  btnInsert.Visible := not USER_MODE;
  btnRename.Enabled := not USER_MODE; //Users can't change constants names
  btnDelete.Visible := not USER_MODE;
  btnInsertSeparator.Visible := not USER_MODE;
  mnuSortByIndex.Visible := not USER_MODE;
  btnMoveUp.Visible := not USER_MODE;
  btnMoveDown.Visible := not USER_MODE;
  mnuSortByName.Visible := not USER_MODE;
  mnuCompactIndexes.Visible := not USER_MODE;
  btnCopy.Visible := not USER_MODE;
  btnPaste.Visible := not USER_MODE;
  mnuListUnused.Visible := not USER_MODE;

  LoadSettings(fExeDir + 'TranslationManager.ini');
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveSettings(fExeDir + 'TranslationManager.ini');
  fTextManager.Free;
end;


procedure TForm1.FormResize(Sender: TObject);
var
  I,K: Integer;
  SelCount, SecHeight: Word;
begin
  SelCount := 0;
  for I := 0 to fLocales.Count - 1 do
  if (I+1 < clbShowLang.Count) then
  if clbShowLang.Checked[I+1] then
    Inc(SelCount);

  if SelCount = 0 then
    Exit;

  SecHeight := ScrollBox1.ClientHeight div SelCount;

  K := 0;
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
  begin
    TransLabels[I].SetBounds(8, 4 + K * SecHeight, 100, 20);
    TransMemos[I].SetBounds(8, 22 + K * SecHeight, ScrollBox1.Width - 16, SecHeight - 20);
    Inc(K);
  end;
end;


procedure TForm1.RefreshFolders;
var
  I: Integer;
begin
  lbFolders.Clear;
  fPathManager.Clear;
  fPathManager.AddPath(fWorkDir);

  for I := 0 to fPathManager.Count - 1 do
    lbFolders.Items.Add(fPathManager[I]);
end;


procedure TForm1.lbFoldersClick(Sender: TObject);
const MSG_WARNING: string = 'You have unsaved changes that will be lost, load new libx anyway?';
var ID: Integer;
begin
  //Let the user abort and save edited translations
  if mnuSave.Enabled
  and (MessageDlg(MSG_WARNING, mtWarning, mbOKCancel, 0) <> mrOK) then
  begin
    lbFolders.ItemIndex := fPreviousFolder;
    Exit;
  end;

  fPreviousFolder := lbFolders.ItemIndex;
  ID := lbFolders.ItemIndex;
  if ID = -1 then Exit;

  //Special case for ingame text library
  if SameText(lbFolders.Items[ID], 'data\text\text.%s.libx') then
  begin
    fTextManager.Load(fWorkDir + lbFolders.Items[ID], fWorkDir + 'KM_TextIDs.inc');
    btnInsertSeparator.Enabled := True;
    btnRename.Enabled := not USER_MODE;
    btnMoveUp.Enabled := True;
    btnMoveDown.Enabled := True;
    mnuSortByName.Enabled := True;
    mnuSortByIndex.Enabled := True;
    mnuCompactIndexes.Enabled := True;
  end
  else
  begin
    fTextManager.Load(fWorkDir + lbFolders.Items[ID], '');
    btnInsertSeparator.Enabled := False;
    btnRename.Enabled := False;
    btnMoveUp.Enabled := False;
    btnMoveDown.Enabled := False;
    mnuSortByName.Enabled := False;
    mnuSortByIndex.Enabled := False;
    mnuCompactIndexes.Enabled := False;
  end;

  RefreshList;
  mnuSave.Enabled := False;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
var ID: Integer;
begin
  ID := lbFolders.ItemIndex;
  if ID = -1 then Exit;

  if SameText(lbFolders.Items[ID], 'data\text\text.%s.libx') then
    fTextManager.Save(fWorkDir + lbFolders.Items[ID], fWorkDir + 'KM_TextIDs.inc')
  else
    fTextManager.Save(fWorkDir + lbFolders.Items[ID], '');

  mnuSave.Enabled := False;
end;


procedure TForm1.RefreshList;
  function ShowConst(aIndex: Integer): Boolean;
  var
    I,K, TextID, DefLoc: Integer;
  begin
    Result := True;
    TextID := fTextManager.Consts[aIndex].TextID;
    DefLoc := fLocales.GetIDFromCode(DEFAULT_LOCALE);

    //Hide lines that have text
    if cbShowMis.Checked then
    begin
      Result := False;
      if (TextID <> -1) then
        for I := 0 to fLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
            Result := Result or (fTextManager.Texts[TextID][I] = '');
    end;

    //Show lines that are the same in selected locales
    if Result and cbShowDup.Checked then
    begin
      Result := False;
      if (TextID <> -1) then
        for I := 0 to fLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
          for K := 0 to fLocales.Count - 1 do
            if (K <> I) and clbShowLang.Checked[K+1] then
              Result := Result or (fTextManager.Texts[TextID][I] = fTextManager.Texts[TextID][K]);
    end;

    if Result and (Edit1.Text <> '') then
        Result := (TextID <> -1) and (Pos(UpperCase(Edit1.Text), UpperCase(fTextManager.Texts[TextID][DefLoc])) <> 0);
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
  SetLength(ListboxLookup, fTextManager.ConstCount);

  for I := 0 to fTextManager.ConstCount - 1 do
  if ShowConst(I) then
  begin
    ListboxLookup[ListBox1.Items.Count] := I;
    if fTextManager.Consts[I].TextID = -1 then
      s := ''
    else
      s := IntToStr(fTextManager.Consts[I].TextID) + ': ' + fTextManager.Consts[I].ConstName;

    ListBox1.Items.Add(s);
  end;

  ListBox1.Items.EndUpdate;
  ListBox1.ItemIndex := EnsureRange(ItemIdx, 0, ListBox1.Count - 1);
  ListBox1.TopIndex := TopIdx;
  ListBox1Click(ListBox1);

  Label3.Caption := 'Count ' + IntToStr(ListBox1.Count);
end;


procedure TForm1.InitLocalesList;
  function GetCharset(aLang: string): TFontCharset;
  begin
    //Using slower but more compact comparisons
    if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
      Result := RUSSIAN_CHARSET
    else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
      Result := EASTEUROPE_CHARSET
    else if Pos(aLang, 'tur') <> 0 then
      Result := TURKISH_CHARSET
    else if Pos(aLang, 'lit,lat') <> 0 then
      Result := BALTIC_CHARSET
    else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
      Result := ANSI_CHARSET
    else
      Result := DEFAULT_CHARSET;
  end;
var
  I: Integer;
begin
  clbShowLang.Clear;

  clbShowLang.Items.Add('All');

  for I := 0 to fLocales.Count - 1 do
    clbShowLang.Items.Add(fLocales[I].Code);

  SetLength(TransMemos, fLocales.Count);
  SetLength(TransLabels, fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
  begin
    TransLabels[I] := TLabel.Create(Form1);
    TransLabels[I].Parent := ScrollBox1;
    TransLabels[I].Caption := fLocales[I].Title + ' (' + fLocales[I].Code + ')';
    TransLabels[I].Hide;

    TransMemos[I] := TMemo.Create(Form1);
    TransMemos[I].Parent := ScrollBox1;
    TransMemos[I].Anchors := [akLeft, akRight, akTop];
    TransMemos[I].Font.Charset := GetCharset(fLocales[I].Code);
    TransMemos[I].OnChange := MemoChange;
    TransMemos[I].Tag := I;
    TransMemos[I].Hide;
  end;
end;


procedure TForm1.RefreshLocales;
var
  I: Integer;
begin
  for I := 0 to fLocales.Count - 1 do
  begin
    TransMemos[I].Visible := clbShowLang.Checked[I+1];
    TransLabels[I].Visible := clbShowLang.Checked[I+1];
  end;

  FormResize(Self);
end;


procedure TForm1.LoadSettings(aPath: string);
var
  I: Integer;
  F: TIniFile;
  Locs: string;
begin
  F := TIniFile.Create(aPath);
  try
    Locs := F.ReadString('Root', 'Selected_Locales', 'eng');
  finally
    F.Free;
  end;

  for I := 0 to fLocales.Count - 1 do
  if Pos(fLocales[I].Code, Locs) <> 0 then
    clbShowLang.Checked[I+1] := True;

  RefreshLocales;
end;


procedure TForm1.SaveSettings(aPath: string);
var
  I: Integer;
  F: TIniFile;
  Locs: string;
begin
  Locs := '';
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
    Locs := Locs + fLocales[I].Code + ',';

  F := TIniFile.Create(aPath);
  try
    F.WriteString('Root', 'Selected_Locales', Locs);
  finally
    F.Free;
  end;
end;


procedure TForm1.ListBox1Click(Sender: TObject);
var I,ID: Integer;
begin
  if (ListBox1.ItemIndex = -1) then exit;

  IgnoreChanges := true;
  ID := ListboxLookup[ListBox1.ItemIndex];

  btnRename.Enabled := fTextManager.Consts[ID].TextID <> -1;

  lblConstName.Caption := fTextManager.Consts[ID].ConstName;

  for I := 0 to fLocales.Count - 1 do
    if fTextManager.Consts[ID].TextID <> -1 then
      TransMemos[i].Text := {$IFDEF FPC}AnsiToUTF8{$ENDIF}(fTextManager.Texts[fTextManager.Consts[ID].TextID][i])
    else
      TransMemos[i].Text := '';
  IgnoreChanges := false;
end;


//Sort the items by TextID
procedure TForm1.btnSortByIndexClick(Sender: TObject);
begin
  fTextManager.SortByIndex;
  RefreshList;
  mnuSave.Enabled := True;
end;


//Sort TextIDs by Index
procedure TForm1.btnSortByNameClick(Sender: TObject);
begin
  fTextManager.SortByName;
  //Compact Indexes
  RefreshList;
  mnuSave.Enabled := True;
end;


//Export TSK/TPR texts into separate files
procedure TForm1.Button1Click(Sender: TObject);
const
  TSK: array [1..21] of Integer =
    (529, 530, 531, 531, 532, 534, 534, 537, 538, 539,
     540, 541, 542, 542, 543, 543, 543, 543, 546, 547, 548);
  TPR: array [1..15] of Integer =
    (549, 550, 550, 551, 553, 553, 554, 556, 556, 558,
     559, 559, 560, 561, 563);
var I: Integer;
begin
  //Export campaign in-mission texts
  {for I := 1 to 20 do
  if (TSK[I+1] - TSK[I] <> 0) then
  begin
    fTextManager.Load(fWorkDir + 'data\text\text.%s.libx', '');
    fTextManager.Slice(TSK[I], TSK[I+1] - TSK[I]);
    fTextManager.Save(fWorkDir + 'Campaigns\The Shattered Kingdom\TSK' + Format('%.2d', [I]) + '\TSK' + Format('%.2d', [I]) + '.%s.libx', '');
  end;
  for I := 1 to 14 do
  if (TPR[I+1] - TPR[I] <> 0) then
  begin
    fTextManager.Load(fWorkDir + 'data\text\text.%s.libx', '');
    fTextManager.Slice(TPR[I], TPR[I+1] - TPR[I]);
    fTextManager.Save(fWorkDir + 'Campaigns\The Peasants Rebellion\TPR' + Format('%.2d', [I]) + '\TPR' + Format('%.2d', [I]) + '.%s.libx', '');
  end;}

  //Export campaign briefings
  fTextManager.Load(fWorkDir + 'data\text\text.%s.libx', '');
  fTextManager.Slice(1249, 20);
  fTextManager.Save(fWorkDir + 'Campaigns\The Shattered Kingdom\text.%s.libx', '');
  fTextManager.Load(fWorkDir + 'data\text\text.%s.libx', '');
  fTextManager.Slice(1349, 14);
  fTextManager.Save(fWorkDir + 'Campaigns\The Peasants Rebellion\text.%s.libx', '');
end;


procedure TForm1.btnCompactIndexesClick(Sender: TObject);
begin
  fTextManager.CompactIndexes;
  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.Edit1Change(Sender: TObject);
begin
  RefreshFilter;
  RefreshList;
end;


procedure TForm1.mnuExitClick(Sender: TObject);
begin
  Close;
end;


procedure TForm1.MemoChange(Sender: TObject);
var ID,T: Integer;
begin
  if IgnoreChanges then exit;
  ID := ListboxLookup[ListBox1.ItemIndex];
  if fTextManager.Consts[ID].TextID = -1 then exit;
  T := TMemo(Sender).Tag;
  fTextManager.Texts[fTextManager.Consts[ID].TextID][T] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);
  mnuSave.Enabled := True;
end;


procedure TForm1.btnInsertClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.Insert(ID);
  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.InsertSeparator(ID);
  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.btnDeleteClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are deleting
  if ID = -1 then Exit;

  fTextManager.DeleteConst(ID);
  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;

  fTextManager.MoveUp(ID);
  RefreshList;
  ListBox1.ItemIndex := Max(ID - 1, 0);
  ListBox1Click(nil); //Reselect the item to update the translation boxes
  mnuSave.Enabled := True;
end;


procedure TForm1.btnCopyClick(Sender: TObject);
var I, ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  SetLength(fBuffer, fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fBuffer[I] := fTextManager.Texts[fTextManager.Consts[ListboxLookup[ID]].TextID][I];
  btnPaste.Enabled := True;
end;


procedure TForm1.btnPasteClick(Sender: TObject);
var I, ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  Assert(Length(fBuffer) = fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fTextManager.Texts[fTextManager.Consts[ListboxLookup[ID]].TextID][I] := fBuffer[I];
  mnuSave.Enabled := True;
  ListBox1Click(nil);
end;


procedure TForm1.btnRenameClick(Sender: TObject);
var
  OldName, NewName: string;
  ID: Integer;
  T: TTextInfo;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  ID := ListboxLookup[ListBox1.ItemIndex];
  if fTextManager.Consts[ID].TextID = -1 then Exit;

  //Copy to temp record to allow to assign to field
  T := fTextManager.Consts[ID];

  OldName := T.ConstName;
  NewName := InputBox('', 'New name:', OldName);

  if NewName = OldName then
    Exit;

  T.ConstName := NewName;
  fTextManager.Consts[ID] := T;

  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.btnMoveDownClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  fTextManager.MoveDown(ID);
  RefreshList;
  ListBox1.ItemIndex := Min(ID + 1, ListBox1.Count - 1);
  ListBox1Click(nil); //Reselect the item to update the translation boxes
  mnuSave.Enabled := True;
end;


procedure TForm1.cbShowMisClick(Sender: TObject);
begin
  RefreshFilter;
  RefreshList;

  //Select the first item
  ListBox1.ItemIndex := 0;
  ListBox1Click(ListBox1);
end;


procedure TForm1.RefreshFilter;
var
  Filter: Boolean;
begin
  Filter := cbShowMis.Checked or cbShowDup.Checked or (Edit1.Text <> '');

  //Disable buttons
  mnuSortByIndex.Enabled := not Filter;
  mnuSortByName.Enabled := not Filter;
  mnuCompactIndexes.Enabled := not Filter;
  btnInsert.Enabled := not Filter;
  btnDelete.Enabled := not Filter;
  btnInsertSeparator.Enabled := not Filter;
  btnMoveUp.Enabled := not Filter;
  btnMoveDown.Enabled := not Filter;
end;


procedure TForm1.clbShowLangClickCheck(Sender: TObject);
var
  I,K: Integer;
begin
  if clbShowLang.Selected[0] then
    if clbShowLang.State[0] = cbChecked then
      for I := 1 to clbShowLang.Count - 1 do
        clbShowLang.Checked[I] := True
    else
    if clbShowLang.State[0] = cbUnchecked then
      for I := 1 to clbShowLang.Count - 1 do
        clbShowLang.Checked[I] := False;

  K := 0;
  for I := 1 to clbShowLang.Count - 1 do
  if clbShowLang.Checked[I] then
    Inc(K);

  if K = 0 then
    clbShowLang.State[0] := cbUnchecked
  else
  if K = clbShowLang.Count - 1 then
    clbShowLang.State[0] := cbChecked
  else
    clbShowLang.State[0] := cbGrayed;

  RefreshList;
  RefreshLocales;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (not mnuSave.Enabled) or
              (MessageDlg('You have unsaved changes that will be lost, are you sure you want to exit?', mtWarning, [mbYes,mbNo], 0) = mrYes);
end;

procedure TForm1.cbShowLang8888Change(Sender: TObject);
begin
  FreeAndNil(fLocales);
  fLocales := TKMLocales.Create(fWorkDir + 'data\locales.txt');
  RefreshLocales;
end;


procedure TForm1.btnUnusedClick(Sender: TObject);
var
  I: Integer;
  SearchRec: TSearchRec;
  ConstList: TStringList;
  PasFile: TMemoryStream;
  PasString: AnsiString;
begin
  ConstList := TStringList.Create;
  PasFile := TMemoryStream.Create;
  try
    //Prepare list of all constants we will be looking for
    for I := 0 to fTextManager.ConstCount - 1 do
      ConstList.Append(fTextManager.Consts[I].ConstName);

    //Check all *.pas files
    FindFirst(fWorkDir + '*.pas', faAnyFile - faDirectory, SearchRec);
    repeat
      PasFile.LoadFromFile(fWorkDir + SearchRec.Name);
      SetString(PasString, PChar(PasFile.Memory), PasFile.Size);
      for I := ConstList.Count - 1 downto 0 do
      if Pos(ConstList[I], PasString) <> 0 then
        ConstList.Delete(I);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    //Remove duplicate EOLs (keep section separators)
    for I := ConstList.Count - 2 downto 0 do
    if (ConstList[I] = '') and (ConstList[I+1] = '') then
      ConstList.Delete(I);

    ConstList.SaveToFile(fWorkDir + 'TM_unused.txt');
    ShowMessage(ConstList.Text);
  finally
    PasFile.Free;
    ConstList.Free;
  end;
end;


end.
