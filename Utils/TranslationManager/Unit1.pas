unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus,
  {$IFDEF MSWINDOWS} ComCtrls, FileCtrl, {$ENDIF}
  StdCtrls, StrUtils, Windows, SysUtils,
  KM_Locales, Unit_Text, Unit_PathManager;

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
    cbShowMissing: TComboBox;
    Label2: TLabel;
    cbIncludeSameAsEnglish: TCheckBox;
    LabelIncludeSameAsEnglish: TLabel;
    Label3: TLabel;
    Button1: TButton;
    lbFolders: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
    Label4: TLabel;
    Edit1: TEdit;
    Label5: TLabel;
    cbShowLang: TComboBox;
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
    procedure cbShowMissingChange(Sender: TObject);
    procedure cbIncludeSameAsEnglishClick(Sender: TObject);
    procedure LabelIncludeSameAsEnglishClick(Sender: TObject);
    procedure btnCompactIndexesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure lbFoldersClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbShowLangChange(Sender: TObject);
    procedure btnUnusedClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
  private
    fPathManager: TPathManager;
    fTextManager: TTextManager;
    fWorkDir: string;
    fBuffer: array of string;

    TransMemos: array of TMemo;
    TransLabels: array of TLabel;
    ListboxLookup: array of Integer;
    IgnoreChanges: Boolean;
    fPreviousFolder, fPreviousShowLang: Integer;
    procedure MemoChange(Sender: TObject);

    procedure RefreshFolders;
    procedure RefreshLocales;
    procedure RefreshList;
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
  fWorkDir := ExtractFilePath((ParamStr(0))) + '..\..\';
  fLocales := TKMLocales.Create(fWorkDir + 'data\locales.txt');
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
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fTextManager.Free;
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
  var TextID: Integer; DefLoc: Integer;
  begin
    Result := True;
    TextID := fTextManager.Consts[aIndex].TextID;
    DefLoc := fLocales.GetIDFromCode(DEFAULT_LOCALE);

    if Result and (cbShowMissing.ItemIndex <> 0) then
      Result := (TextID <> -1) and
                (
                  (fTextManager.Texts[TextID][cbShowMissing.ItemIndex-1] = '') or
                  (
                    cbIncludeSameAsEnglish.Checked and
                    (fTextManager.Texts[TextID][cbShowMissing.ItemIndex-1] = fTextManager.Texts[TextID][DefLoc])
                  )
                );

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


procedure TForm1.RefreshLocales;

  function GetCharset(aLang: string): TFontCharset;
  begin
    Result := DEFAULT_CHARSET;
    if aLang = 'rus' then Result := RUSSIAN_CHARSET;
    if aLang = 'bul' then Result := RUSSIAN_CHARSET;
    if aLang = 'pol' then Result := EASTEUROPE_CHARSET;
    if aLang = 'hun' then Result := EASTEUROPE_CHARSET;
    if aLang = 'cze' then Result := EASTEUROPE_CHARSET;
    if aLang = 'svk' then Result := EASTEUROPE_CHARSET;
  end;

var I, K, SelectedLang, SelectedShowMissing: Integer;
begin
  for I := 0 to High(TransLabels) do
  begin
    FreeAndNil(TransLabels[I]);
    FreeAndNil(TransMemos[I]);
  end;
  SelectedLang := cbShowLang.ItemIndex;
  if SelectedLang = -1 then
    SelectedLang := 0;

  SetLength(TransMemos, fLocales.Count);
  SetLength(TransLabels, fLocales.Count);

  SelectedShowMissing := cbShowMissing.ItemIndex;
  cbShowMissing.Items.Clear;
  cbShowMissing.Items.Add('None');

  cbShowLang.Items.Clear;
  cbShowLang.Items.Add('All');
  cbShowLang.ItemIndex := 0; //All by default
  K := 0;
  for I := 0 to fLocales.Count - 1 do
  begin
    TransMemos[I] := TMemo.Create(Form1);
    TransMemos[I].Parent := ScrollBox1;
    TransMemos[I].Tag := I;
    TransMemos[I].Hide;
    cbShowMissing.Items.Add(fLocales[I].Code);
    cbShowLang.Items.Add(fLocales[I].Code);
    if SelectedLang = I+1 then cbShowLang.ItemIndex := I+1;
    if (SelectedLang = 0) or (SelectedLang = I+1)
    or (fLocales[I].Code = 'eng') then //Always show ENG
    begin
      TransLabels[I] := TLabel.Create(Form1);
      TransLabels[I].Parent := ScrollBox1;
      TransLabels[I].SetBounds(8, 4 + K * 80, 30, 30);
      TransLabels[I].Caption := fLocales[I].Title + ' (' + fLocales[I].Code + ')';

      TransMemos[I].Parent := ScrollBox1;
      TransMemos[I].SetBounds(8, 22 + K * 80, ScrollBox1.Width - 16, 60);
      TransMemos[I].Anchors := [akLeft, akRight, akTop];
      TransMemos[I].Font.Charset := GetCharset(fLocales[I].Code);
      TransMemos[I].OnChange := MemoChange;
      TransMemos[I].Show;

      inc(K);
    end;
  end;
  if SelectedShowMissing = -1 then
    cbShowMissing.ItemIndex := 0
  else
    cbShowMissing.ItemIndex := SelectedShowMissing;
  ListBox1Click(nil);
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


procedure TForm1.cbShowMissingChange(Sender: TObject);
var
  Filter: Boolean;
begin
  RefreshList;
  Filter := cbShowMissing.ItemIndex > 0;

  //Disable buttons
  mnuSortByIndex.Enabled := not Filter;
  mnuSortByName.Enabled := not Filter;
  mnuCompactIndexes.Enabled := not Filter;
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


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (not mnuSave.Enabled) or
              (MessageDlg('You have unsaved changes that will be lost, are you sure you want to exit?', mtWarning, [mbYes,mbNo], 0) = mrYes);
end;

procedure TForm1.cbShowLangChange(Sender: TObject);
begin
  fPreviousShowLang := cbShowLang.ItemIndex;
  FreeAndNil(fLocales);
  fLocales := TKMLocales.Create(fWorkDir + 'data\locales.txt');
  RefreshLocales;
end;

{$IFDEF WDC}
function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
{$ENDIF}


(*procedure TForm1.btnUnusedClick(Sender: TObject);
var i, DefLoc:integer; res:string; sl: TStringList;
begin
  DefLoc := fLocales.GetIDFromCode(DEFAULT_LOCALE);
  sl:= TStringList.Create;
  for i:=0 to fTextManager.ConstCount-1 do
  begin
    mnuListUnused.Caption := 'Searching.... '+IntToStr(I)+' / '+IntToStr(fTextManager.ConstCount-1);
    {$IFDEF WDC}
    res := GetDosOutput('findstr "'+fTextManager.Consts[i].ConstName+'" *.pas', fWorkDir);
    {$ENDIF}
    if res = '' then sl.Add(fTextManager.Consts[i].ConstName);
  end;
  sl.SaveToFile('TM_unused.txt');
  mnuListUnused.Caption := 'List unused';
  ShowMessage('Found '+IntToStr(sl.Count)+' unused strings in files '+fWorkDir+'*.pas. Saved list to TM_unused.txt');
  sl.Free;
end;*)


//@Lewin: This seems to be much faster and readable. Can you tell why did you chose above method?
procedure TForm1.btnUnusedClick(Sender: TObject);
var
  I: Integer;
  SearchRec: TSearchRec;
  ConstList: TStringList;
  PasFile: TStringStream;
begin
  ConstList := TStringList.Create;
  PasFile := TStringStream.Create;
  try
    //Prepare list of all constants we will be looking for
    for I := 0 to fTextManager.ConstCount - 1 do
      ConstList.Append(fTextManager.Consts[I].ConstName);

    //Check all *.pas files
    FindFirst(fWorkDir + '*.pas', faAnyFile - faDirectory, SearchRec);
    repeat
      PasFile.LoadFromFile(fWorkDir + SearchRec.Name);
      for I := ConstList.Count - 1 downto 0 do
      if Pos(ConstList[I], PasFile.DataString) <> 0 then
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
