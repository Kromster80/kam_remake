unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, FileCtrl,
  Dialogs, StrUtils, StdCtrls, Math, ExtCtrls, Unit_Text;


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
    procedure FormDestroy(Sender: TObject);
  private
    fTextManager: TTextManager;

    TransMemos: array of TMemo;
    TransLabels: array of TLabel;
    ListboxLookup: array of Integer;
    IgnoreChanges: Boolean;
    procedure MemoChange(Sender: TObject);

    procedure RefreshLocales;
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
  fTextManager := TTextManager.Create;

  btnLoadClick(btnLoad);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fTextManager.Free;
end;


procedure TForm1.btnLoadClick(Sender: TObject);
begin
  //if SelectDirectory(WorkDir, [], 0) then
  fTextManager.Load('..\..\data\text\text.%s.libx', '..\..\KM_TextIDs.inc');

  RefreshLocales;
  RefreshList;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  fTextManager.Save('..\..\data\text\text.%s.libx', '..\..\KM_TextIDs.inc');
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
                    (fTextManager.Texts[fTextManager.Consts[aIndex].TextID][cbShowMissing.ItemIndex] = fTextManager.Texts[fTextManager.Consts[aIndex].TextID][fTextManager.DefaultLocale])
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

  SetLength(TransMemos, fTextManager.LocalesCount);
  SetLength(TransLabels, fTextManager.LocalesCount);

  cbShowMissing.Items.Clear;
  cbShowMissing.Items.Add('None');
  for I := 0 to fTextManager.LocalesCount - 1 do
  begin
    TransLabels[I] := TLabel.Create(Form1);
    TransLabels[I].Parent := ScrollBox1;
    TransLabels[I].SetBounds(8,4+I*80,30,30);
    TransLabels[I].Caption := fTextManager.Locales[I];

    TransMemos[I] := TMemo.Create(Form1);
    TransMemos[I].Parent := ScrollBox1;
    TransMemos[I].SetBounds(8,22+I*80,ScrollBox1.Width-16,60);
    TransMemos[I].Anchors := [akLeft,akRight,akTop];
    TransMemos[I].Font.Charset := GetCharset(fTextManager.Locales[I]);
    TransMemos[I].Tag := I;
    TransMemos[I].OnChange := MemoChange;

    cbShowMissing.Items.Add(fTextManager.Locales[I]);
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

  for I := 0 to fTextManager.LocalesCount - 1 do
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
end;


//Sort TextIDs by Index
procedure TForm1.btnSortByNameClick(Sender: TObject);
begin
  fTextManager.SortByName;
  //Compact Indexes
  RefreshList;
end;


procedure TForm1.btnCompactIndexesClick(Sender: TObject);
begin
  fTextManager.CompactIndexes;
  RefreshList;
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
end;


procedure TForm1.MemoChange(Sender: TObject);
var ID,T: Integer;
begin
  if IgnoreChanges then exit;
  ID := ListboxLookup[ListBox1.ItemIndex];
  if fTextManager.Consts[ID].TextID = -1 then exit;
  T := TMemo(Sender).Tag;
  fTextManager.Texts[fTextManager.Consts[ID].TextID][T] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);
end;


procedure TForm1.btnInsertClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.Insert(ID);
  RefreshList;
end;



procedure TForm1.btnDeleteClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are deleting
  if ID = -1 then Exit;

  fTextManager.DeleteConst(ID);
  RefreshList;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex; //Item place we are adding
  if ID = -1 then Exit;

  fTextManager.InsertSeparator(ID);
  RefreshList;
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;

  fTextManager.MoveUp(ID);
  RefreshList;
  ListBox1.ItemIndex := Max(ID - 1, 0);
end;


procedure TForm1.btnMoveDownClick(Sender: TObject);
var ID: integer;
begin
  ID := ListBox1.ItemIndex;

  fTextManager.MoveDown(ID);
  RefreshList;
  ListBox1.ItemIndex := Min(ID + 1, ListBox1.Count - 1);
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
