unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math,
  {$IFDEF MSWINDOWS} FileCtrl, {$ENDIF}
  StdCtrls, StrUtils, SysUtils, Windows, KM_Locales, Unit_Text, Unit_PathManager, ComCtrls;


type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    EditConstName: TEdit;
    Label1: TLabel;
    btnSortByIndex: TButton;
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
    Button1: TButton;
    lbFolders: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnSortByIndexClick(Sender: TObject);
    procedure btnSortByNameClick(Sender: TObject);
    procedure EditConstNameChange(Sender: TObject);
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
  private
    fPathManager: TPathManager;
    fTextManager: TTextManager;
    fWorkDir: string;
    fBuffer: array of string;

    TransMemos: array of TMemo;
    TransLabels: array of TLabel;
    ListboxLookup: array of Integer;
    IgnoreChanges: Boolean;
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

  btnSave.Enabled := False;
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
  if btnSave.Enabled
  and (MessageDlg(MSG_WARNING, mtWarning, mbOKCancel, 0) = mrCancel) then
    Exit;

  ID := lbFolders.ItemIndex;
  if ID = -1 then Exit;

  //Special case for ingame text library
  if SameText(lbFolders.Items[ID], 'data\text\text.%s.libx') then
  begin
    fTextManager.Load(fWorkDir + lbFolders.Items[ID], fWorkDir + 'KM_TextIDs.inc');
    EditConstName.Enabled := True;
    btnInsertSeparator.Enabled := True;
    btnMoveUp.Enabled := True;
    btnMoveDown.Enabled := True;
    btnSortByName.Enabled := True;
    btnSortByIndex.Enabled := True;
    btnCompactIndexes.Enabled := True;
  end
  else
  begin
    fTextManager.Load(fWorkDir + lbFolders.Items[ID], '');
    EditConstName.Enabled := False;
    btnInsertSeparator.Enabled := False;
    btnMoveUp.Enabled := False;
    btnMoveDown.Enabled := False;
    btnSortByName.Enabled := False;
    btnSortByIndex.Enabled := False;
    btnCompactIndexes.Enabled := False;
  end;

  RefreshList;
  btnSave.Enabled := False;
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

  btnSave.Enabled := False;
end;


procedure TForm1.RefreshList;
  function ShowConst(aIndex: Integer): Boolean;
  begin
    if cbShowMissing.ItemIndex = 0 then
      Result := True
    else
    begin
      Result := (fTextManager.Consts[aIndex].TextID <> -1) and
                (
                  (fTextManager.Texts[fTextManager.Consts[aIndex].TextID][cbShowMissing.ItemIndex] = '') or
                  (
                    cbIncludeSameAsEnglish.Checked and
                    (fTextManager.Texts[fTextManager.Consts[aIndex].TextID][cbShowMissing.ItemIndex] = fTextManager.Texts[fTextManager.Consts[aIndex].TextID][fLocales.GetIDFromCode(DEFAULT_LOCALE)])
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

var I: Integer;
begin
  for I := 0 to High(TransLabels) do
  begin
    FreeAndNil(TransLabels[I]);
    FreeAndNil(TransMemos[I]);
  end;

  SetLength(TransMemos, fLocales.Count);
  SetLength(TransLabels, fLocales.Count);

  cbShowMissing.Items.Clear;
  cbShowMissing.Items.Add('None');
  for I := 0 to fLocales.Count - 1 do
  begin
    TransLabels[I] := TLabel.Create(Form1);
    TransLabels[I].Parent := ScrollBox1;
    TransLabels[I].SetBounds(8, 4 + I * 80, 30, 30);
    TransLabels[I].Caption := fLocales[I].Title + ' (' + fLocales[I].Code + ')';

    TransMemos[I] := TMemo.Create(Form1);
    TransMemos[I].Parent := ScrollBox1;
    TransMemos[I].SetBounds(8, 22 + I * 80, ScrollBox1.Width - 16, 60);
    TransMemos[I].Anchors := [akLeft, akRight, akTop];
    TransMemos[I].Font.Charset := GetCharset(fLocales[I].Code);
    TransMemos[I].Tag := I;
    TransMemos[I].OnChange := MemoChange;

    cbShowMissing.Items.Add(fLocales[I].Code);
  end;
  cbShowMissing.ItemIndex := 0;
end;


procedure TForm1.ListBox1Click(Sender: TObject);
var I,ID: Integer;
begin
  if (ListBox1.ItemIndex = -1) then exit;

  IgnoreChanges := true;
  ID := ListboxLookup[ListBox1.ItemIndex];
  EditConstName.Text := fTextManager.Consts[ID].ConstName;

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
  btnSave.Enabled := True;
end;


//Sort TextIDs by Index
procedure TForm1.btnSortByNameClick(Sender: TObject);
begin
  fTextManager.SortByName;
  //Compact Indexes
  RefreshList;
  btnSave.Enabled := True;
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
  btnSave.Enabled := True;
end;


procedure TForm1.EditConstNameChange(Sender: TObject);
var ID: Integer; T: TTextInfo;
begin
  if IgnoreChanges then Exit;

  ID := ListboxLookup[ListBox1.ItemIndex];
  if fTextManager.Consts[ID].TextID = -1 then exit;

  T := fTextManager.Consts[ID];
  T.ConstName := EditConstName.Text;
  fTextManager.Consts[ID] := T;

  RefreshList;
  btnSave.Enabled := True;
end;


procedure TForm1.MemoChange(Sender: TObject);
var ID,T: Integer;
begin
  if IgnoreChanges then exit;
  ID := ListboxLookup[ListBox1.ItemIndex];
  if fTextManager.Consts[ID].TextID = -1 then exit;
  T := TMemo(Sender).Tag;
  fTextManager.Texts[fTextManager.Consts[ID].TextID][T] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);
  btnSave.Enabled := True;
end;


procedure TForm1.btnInsertClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.Insert(ID);
  RefreshList;
  btnSave.Enabled := True;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.InsertSeparator(ID);
  RefreshList;
  btnSave.Enabled := True;
end;


procedure TForm1.btnDeleteClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are deleting
  if ID = -1 then Exit;

  fTextManager.DeleteConst(ID);
  RefreshList;
  btnSave.Enabled := True;
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;

  fTextManager.MoveUp(ID);
  RefreshList;
  ListBox1.ItemIndex := Max(ID - 1, 0);
  btnSave.Enabled := True;
end;


procedure TForm1.btnCopyClick(Sender: TObject);
var I, ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  SetLength(fBuffer, fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fBuffer[I] := fTextManager.Texts[fTextManager.Consts[ID].TextID][I];
  btnPaste.Enabled := True;
end;


procedure TForm1.btnPasteClick(Sender: TObject);
var I, ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  Assert(Length(fBuffer) = fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fTextManager.Texts[fTextManager.Consts[ID].TextID][I] := fBuffer[I];
  btnSave.Enabled := True;
  ListBox1Click(nil);
end;


procedure TForm1.btnMoveDownClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  fTextManager.MoveDown(ID);
  RefreshList;
  ListBox1.ItemIndex := Min(ID + 1, ListBox1.Count - 1);
  btnSave.Enabled := True;
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
