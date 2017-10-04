unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus,
  {$IFDEF MSWINDOWS} ComCtrls, FileCtrl, {$ENDIF}
  StdCtrls, StrUtils, Windows, SysUtils, CheckLst, INIFiles, Zippit,
  KM_Defaults, KM_FileIO, KM_ResLocales, Unit_Text, Unit_PathManager,
  Vcl.Samples.Spin;

const
  //Disables insert, delete, compact, sort, etc. functions
  //so translators don't click them by mistake
  USER_MODE = False;

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
    lbFolders: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
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
    StatusBar1: TStatusBar;
    clbFolders: TCheckListBox;
    mmSaveAllZIP: TMenuItem;
    sdExportZIP: TSaveDialog;
    FilterGroupBox: TGroupBox;
    cbShowMis: TCheckBox;
    cbShowDup: TCheckBox;
    Label4: TLabel;
    edTextFilter: TEdit;
    Label1: TLabel;
    edLabelName: TEdit;
    Label3: TLabel;
    edLabelId: TEdit;
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
    procedure lbFoldersClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnUnusedClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure clbShowLangClickCheck(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbShowMisClick(Sender: TObject);
    procedure clbFoldersClickCheck(Sender: TObject);
    procedure mmSaveAllZIPClick(Sender: TObject);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
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
  eol: string = #13#10; // EndOfLine


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Remake Translation Manager (' + GAME_REVISION + ')';

  fExeDir := ExtractFilePath(ParamStr(0));
  fWorkDir := fExeDir + '..\..\';
  gResLocales := TKMLocales.Create(fWorkDir + 'data\locales.txt', DEFAULT_LOCALE);

  InitLocalesList;

  RefreshLocales;

  fPathManager := TPathManager.Create;
  RefreshFolders;

  fTextManager := TTextManager.Create;

  //Hide menu entries that Users should not access
  mnuSave.Enabled := False;
  MainMenu1.Items[1].Visible := not USER_MODE;
  mnuSortByIndex.Visible := not USER_MODE;
  mnuSortByName.Visible := not USER_MODE;
  mnuCompactIndexes.Visible := not USER_MODE;
  mnuListUnused.Visible := not USER_MODE;

  //Hide edit butotns that Users should not access
  btnInsert.Visible := not USER_MODE;
  btnRename.Visible := not USER_MODE;
  btnDelete.Visible := not USER_MODE;
  btnInsertSeparator.Visible := not USER_MODE;
  btnMoveUp.Visible := not USER_MODE;
  btnMoveDown.Visible := not USER_MODE;
  btnCopy.Visible := not USER_MODE;
  btnPaste.Visible := not USER_MODE;

  LoadSettings(fExeDir + 'TranslationManager.ini');

  WindowState := wsMaximized;
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
  for I := 0 to gResLocales.Count - 1 do
  if (I+1 < clbShowLang.Count) then
  if clbShowLang.Checked[I+1] then
    Inc(SelCount);

  if SelCount = 0 then
    Exit;

  SecHeight := ScrollBox1.ClientHeight div SelCount;

  K := 0;
  for I := 0 to gResLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
  begin
    TransLabels[I].SetBounds(8, 4 + K * SecHeight, 100, 20);
    TransMemos[I].SetBounds(8, 22 + K * SecHeight, ScrollBox1.Width - 20, SecHeight - 20);
    Inc(K);
  end;
end;


procedure TForm1.RefreshFolders;
var
  I: Integer;
begin
  lbFolders.Clear;
  fPathManager.Clear;

  //Add paths
  if clbFolders.Checked[0] then
    fPathManager.AddPath(fWorkDir, 'data' + PathDelim + 'text' + PathDelim);
  if clbFolders.Checked[1] then
    fPathManager.AddPath(fWorkDir, 'Tutorials' + PathDelim);
  if clbFolders.Checked[2] then
    fPathManager.AddPath(fWorkDir, 'Campaigns' + PathDelim);
  if clbFolders.Checked[3] then
    fPathManager.AddPath(fWorkDir, 'Maps' + PathDelim);
  if clbFolders.Checked[4] then
    fPathManager.AddPath(fWorkDir, 'MapsMP' + PathDelim);

  for I := 0 to fPathManager.Count - 1 do
    lbFolders.Items.Add(fPathManager[I]);
end;


procedure TForm1.lbFoldersClick(Sender: TObject);
const
  MSG_WARNING: string = 'You have unsaved changes that will be lost, load new libx anyway?';
  PATH_TEXT = 'data\text\text.%s.libx';
  PATH_CONST = 'KM_TextIDs.inc';
var
  ID: Integer;
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

  // Special case for ingame text library
  if SameText(lbFolders.Items[ID], PATH_TEXT) then
    fTextManager.Load(fWorkDir + PATH_TEXT, fWorkDir + PATH_CONST)
  else
    fTextManager.Load(fWorkDir + lbFolders.Items[ID], '');

  RefreshFilter;
  RefreshList;
  mnuSave.Enabled := False;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  fTextManager.Save;
end;


procedure TForm1.RefreshList;
  function ShowConst(aIndex: Integer): Boolean;
  var
    I,K, TextID, DefLoc, LabelId: Integer;
    TextConstName: String;
  begin
    Result := True;
    TextID := fTextManager.Consts[aIndex].TextID;
    TextConstName := fTextManager.Consts[aIndex].ConstName;
    DefLoc := gResLocales.IndexByCode(DEFAULT_LOCALE);

    //Hide lines that have text
    if cbShowMis.Checked then
      if TextID = -1 then
        Result := False
      else
      begin
        Result := False;
        for I := 0 to gResLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
            Result := Result or (fTextManager.Texts[TextID][I] = '');
      end;

    //Show lines that are the same in selected locales
    if Result and cbShowDup.Checked then
      if TextID = -1 then
        Result := False
      else
      begin
        Result := False;
        for I := 0 to gResLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
          for K := 0 to gResLocales.Count - 1 do
            if (K <> I) and clbShowLang.Checked[K+1] then
              Result := Result or (fTextManager.Texts[TextID][I] = fTextManager.Texts[TextID][K]);
      end;

    if Result and (edTextFilter.Text <> '') then
      Result := (TextID <> -1) and (Pos(UpperCase(edTextFilter.Text), UpperCase(fTextManager.Texts[TextID][DefLoc])) <> 0);

    if Result and (edLabelName.Text <> '') then
      Result := (TextID <> -1) and (Pos(UpperCase(edLabelName.Text), UpperCase(TextConstName)) <> 0);

    if Result and (edLabelId.Text <> '') then
      Result := (TextID <> -1) and TryStrToInt(edLabelId.Text, LabelId) and (TextID = LabelId);
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

  StatusBar1.Panels[0].Text := 'Count ' + IntToStr(ListBox1.Count);
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

  for I := 0 to gResLocales.Count - 1 do
    clbShowLang.Items.Add(gResLocales[I].Code);

  SetLength(TransMemos, gResLocales.Count);
  SetLength(TransLabels, gResLocales.Count);
  for I := 0 to gResLocales.Count - 1 do
  begin
    TransLabels[I] := TLabel.Create(Form1);
    TransLabels[I].Parent := ScrollBox1;
    TransLabels[I].Caption := gResLocales[I].Title + ' (' + gResLocales[I].Code + ')';
    TransLabels[I].Hide;

    TransMemos[I] := TMemo.Create(Form1);
    TransMemos[I].Parent := ScrollBox1;
    TransMemos[I].Anchors := [akLeft, akRight, akTop];
    TransMemos[I].Font.Name := 'Arial Unicode MS'; //If not found, parent font is used
    TransMemos[I].Font.Charset := GetCharset(gResLocales[I].Code);
    TransMemos[I].OnChange := MemoChange;
    TransMemos[I].Tag := I;
    TransMemos[I].Hide;
  end;
end;


procedure TForm1.RefreshLocales;
var
  I: Integer;
begin
  for I := 0 to gResLocales.Count - 1 do
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
const
  DEFAULT_FOLDER_CHECKED: array[0..4] of Boolean =
    (True, True, True,
     False, False); //Maps/MapsMP are not ticked by default
begin
  F := TIniFile.Create(aPath);
  try
    Locs := F.ReadString('Root', 'Selected_Locales', 'eng');
    for I := 0 to clbFolders.Items.Count-1 do
      if F.ReadBool('Folders', clbFolders.Items[I], DEFAULT_FOLDER_CHECKED[I]) then
        clbFolders.Checked[I] := True;
  finally
    F.Free;
  end;

  //If there are any items "All" should be greyed
  if Locs <> '' then
    clbShowLang.State[0] := cbGrayed;

  for I := 0 to gResLocales.Count - 1 do
  if Pos(gResLocales[I].Code, Locs) <> 0 then
    clbShowLang.Checked[I+1] := True;

  RefreshLocales;
  RefreshFolders;
end;


procedure TForm1.SaveSettings(aPath: string);
var
  I: Integer;
  F: TIniFile;
  Locs: string;
begin
  Locs := '';
  for I := 0 to gResLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
    Locs := Locs + gResLocales[I].Code + ',';

  F := TIniFile.Create(aPath);
  try
    F.WriteString('Root', 'Selected_Locales', Locs);
    for I := 0 to clbFolders.Items.Count-1 do
      F.WriteBool('Folders', clbFolders.Items[I], clbFolders.Checked[I]);
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

  for I := 0 to gResLocales.Count - 1 do
    if fTextManager.Consts[ID].TextID <> -1 then
      TransMemos[i].Text := {$IFDEF FPC}AnsiToUTF8{$ENDIF}(fTextManager.Texts[fTextManager.Consts[ID].TextID][i])
    else
      TransMemos[i].Text := '';
  IgnoreChanges := false;
end;


procedure TForm1.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_DELETE then
    btnDeleteClick(Self);
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


procedure TForm1.btnCompactIndexesClick(Sender: TObject);
begin
  fTextManager.CompactIndexes;
  RefreshList;
  mnuSave.Enabled := True;
end;


procedure TForm1.FilterChanged(Sender: TObject);
begin
  RefreshFilter;
  RefreshList;
end;


procedure TForm1.mmSaveAllZIPClick(Sender: TObject);
var
  ExportPathManager: TPathManager;
  I, K: Integer;
  MyZip: TZippit;
  S: string;
begin
  if not sdExportZIP.Execute(Handle) then Exit;

  ExportPathManager := TPathManager.Create;
  ExportPathManager.AddPath(fWorkDir, '');

  MyZip := TZippit.Create;
  for I:=0 to ExportPathManager.Count-1 do
    for K := 0 to gResLocales.Count - 1 do
    begin
      S := Format(ExportPathManager[I], [gResLocales[K].Code]);
      if FileExists(fWorkDir + S) then
        MyZip.AddFile(fWorkDir + S, ExtractFilePath(S));
    end;

  MyZip.SaveToFile(sdExportZIP.FileName);
  MyZip.Free;
  ExportPathManager.Free;
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

  SetLength(fBuffer, gResLocales.Count);
  for I := 0 to gResLocales.Count - 1 do
    fBuffer[I] := fTextManager.Texts[fTextManager.Consts[ListboxLookup[ID]].TextID][I];
  btnPaste.Enabled := True;
end;


procedure TForm1.btnPasteClick(Sender: TObject);
var I, ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  Assert(Length(fBuffer) = gResLocales.Count);
  for I := 0 to gResLocales.Count - 1 do
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
  NewName := UpperCase(InputBox('', 'New name:', OldName));

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
  Id: Integer;
  Filter, MainFile: Boolean;
begin
  Id := lbFolders.ItemIndex;
  if Id = -1 then Exit;

  MainFile := SameText(lbFolders.Items[ID], 'data\text\text.%s.libx');
  Filter := cbShowMis.Checked or cbShowDup.Checked
    or (edTextFilter.Text <> '') or (edLabelId.Text <> '') or (edLabelName.Text <> '');

  //Disable buttons
  mnuSortByIndex.Enabled := MainFile and not Filter;
  mnuSortByName.Enabled := MainFile and not Filter;
  mnuCompactIndexes.Enabled := MainFile and not Filter;
  btnInsert.Enabled := not Filter;
  btnRename.Enabled := MainFile;
  btnDelete.Enabled := not Filter;
  btnInsertSeparator.Enabled := MainFile and not Filter;
  btnMoveUp.Enabled := MainFile and not Filter;
  btnMoveDown.Enabled := MainFile and not Filter;
end;


procedure TForm1.clbFoldersClickCheck(Sender: TObject);
begin
  RefreshFolders;
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
  CanClose := (not mnuSave.Enabled) or (MessageDlg('Exit without saving?', mtWarning, [mbYes, mbNo], 0) = mrYes);
end;


procedure TForm1.btnUnusedClick(Sender: TObject);
var
  ConstList: TStringList;

  procedure CheckPasFiles(aDir: string);
  var
    I: Integer;
    SearchRec: TSearchRec;
    PasString: AnsiString;
  begin
    if FindFirst(aDir + '*.*', faAnyFile or faDirectory, SearchRec) = 0 then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) <> 0) then
        begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            CheckPasFiles(aDir + SearchRec.Name + '\'); //Check subfolder recursively
          Continue;
        end;
        if not EndsStr('.pas', SearchRec.Name) then Continue;

        PasString := ReadTextA(aDir + SearchRec.Name);
        for I := ConstList.Count - 1 downto 0 do
        if Pos(ConstList[I], PasString) <> 0 then
          ConstList.Delete(I);
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    end;
  end;

var
  I: Integer;
begin
  ConstList := TStringList.Create;
  try
    //Prepare list of all constants we will be looking for
    for I := 0 to fTextManager.ConstCount - 1 do
      ConstList.Append(fTextManager.Consts[I].ConstName);

    //Check all *.pas files
    CheckPasFiles(fWorkDir + 'src\');

    //Remove duplicate EOLs (keep section separators)
    for I := ConstList.Count - 2 downto 0 do
    if (ConstList[I] = '') and (ConstList[I+1] = '') then
      ConstList.Delete(I);

    ConstList.SaveToFile(fWorkDir + 'TM_unused.txt');
    ShowMessage(ConstList.Text);
  finally
    ConstList.Free;
  end;
end;


end.
