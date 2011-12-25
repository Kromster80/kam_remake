unit umain;

{$MODE objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls,
  Spin,
  Menus,
  uGoTo,
  ufind,
  LCLType;

type
  TStringData = class(TObject)
  private
  public
    Index : integer;
    EndsInNL : Boolean;
    FakeIndex, Number: Integer;
  end;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAL: TBitBtn;
    btnCL: TBitBtn;
    btnCU: TBitBtn;
    btnNL: TBitBtn;
    btnOL: TBitBtn;
    btnNU: TBitBtn;
    btnSL: TBitBtn;
    btnOU: TBitBtn;
    btnLSlashL: TBitBtn;
    btnAU: TBitBtn;
    btnEL: TBitBtn;
    btnEU: TBitBtn;
    btnNBS : TBitBtn;
    btnAutoFormat : TBitBtn;
    btnLoad : TBitBtn;
    btnLSlash: TBitBtn;
    btnZ1L: TBitBtn;
    btnSU: TBitBtn;
    btnSave : TBitBtn;
    btnSaveAs : TBitBtn;
    btnZ2L: TBitBtn;
    btnZ1U: TBitBtn;
    btnZ2U: TBitBtn;
    lblAutoComment : TLabel;
    lblStrIndex : TLabel;
    lblNumChar : TLabel;
    lblStrNum: TLabel;
    lblStrLen: TLabel;
    MainMenu : TMainMenu;
    Memo : TMemo;
    mmiInsertString: TMenuItem;
    mmiBaltic: TMenuItem;
    mmiMac: TMenuItem;
    mmiSymbol: TMenuItem;
    mmiISO4: TMenuItem;
    mmiISO3: TMenuItem;
    mmiISO2: TMenuItem;
    mmiISO1: TMenuItem;
    mmiRussian: TMenuItem;
    mmiOEM: TMenuItem;
    mmiANSI: TMenuItem;
    mmiUnicode: TMenuItem;
    mmiEastEurope: TMenuItem;
    mmiDefault: TMenuItem;
    mmiJapanese: TMenuItem;
    mmiHangeul: TMenuItem;
    mmiJohab: TMenuItem;
    mmiGB2312: TMenuItem;
    mmiBig5: TMenuItem;
    mmiGreek: TMenuItem;
    mmiTurkish: TMenuItem;
    mmiVietnamese: TMenuItem;
    mmiHebrew: TMenuItem;
    mmiArabic: TMenuItem;
    mmiThai: TMenuItem;
    mmiCharSet: TMenuItem;
    mmiDeleteString: TMenuItem;
    mmiAddNew: TMenuItem;
    mmiFindPrevious: TMenuItem;
    mmiFindNext: TMenuItem;
    mmiFind: TMenuItem;
    mmiGoToIndex : TMenuItem;
    mmiEdit : TMenuItem;
    mmiExit : TMenuItem;
    mmiSaveAs : TMenuItem;
    mmiSave : TMenuItem;
    mmiLoad : TMenuItem;
    mmiAbout : TMenuItem;
    mmiRealHelp : TMenuItem;
    mmiHelp : TMenuItem;
    mmiFile : TMenuItem;
    OpenDialog : TOpenDialog;
    SaveDialog : TSaveDialog;
    seAutoFormatLineNum : TSpinEdit;
    TreeView : TTreeView;
    procedure mmiANSIClick(Sender: TObject);
    procedure mmiDefaultClick(Sender: TObject);
    procedure mmiDeleteStringClick(Sender: TObject);
    procedure mmiAddNewClick(Sender: TObject);
    procedure mmiEastEuropeClick(Sender: TObject);
    procedure mmiFindNextClick(Sender: TObject);
    procedure mmiFindPreviousClick(Sender: TObject);
    procedure mmiISO1Click(Sender: TObject);
    procedure mmiISO2Click(Sender: TObject);
    procedure mmiISO3Click(Sender: TObject);
    procedure mmiISO4Click(Sender: TObject);
    procedure mmiOEMClick(Sender: TObject);
    procedure mmiRussianClick(Sender: TObject);
    procedure mmiUnicodeClick(Sender: TObject);
    procedure SearchTreeView(SearchString:string;AMatchCase,AFromCursor,ABackwards,ATreatAsSpaces: boolean);
    procedure SelectTextInMemo(ASearchString:string; MatchCase:boolean);
    procedure btnAutoFormatClick(Sender : TObject);
    procedure InsertCharAtCarot(AChar: String);
    procedure btnAddCharClick(Sender: TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure mmiAboutClick(Sender : TObject);
    procedure mmiExitClick(Sender : TObject);
    procedure mmiFindClick(Sender: TObject);
    procedure mmiGoToIndexClick(Sender : TObject);
    procedure mmiLoadClick(Sender : TObject);
    procedure mmiRealHelpClick(Sender : TObject);
    procedure mmiSaveAsClick(Sender : TObject);
    procedure mmiSaveClick(Sender : TObject);
    procedure mmiShowEmptyStringsClick(Sender : TObject);
    procedure SetFileHasSaved(AHasSave : boolean);
    procedure SetCurrentFileName(ACFN : string);
    procedure btnLoadClick(Sender : TObject);
    procedure btnSaveAsClick(Sender : TObject);
    procedure btnSaveClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure LoadLIBFile(AFileName : string);
    procedure SaveLIBFile(AFileName : string);
    procedure MemoChange(Sender : TObject);
    procedure TreeViewSelectionChanged(Sender : TObject);
    procedure ReportError(AMainSection : string; AErrorTypeMsg : string; ALastThingDone : string; ANotes : string = '');
    procedure SetCharSet(CharSet : TFontCharSet);
  private
    { private declarations }
    FHasSaved : Boolean;
    FCurrentFileName : string;
  public
    { public declarations }
    StringsData : array[1..100000] of string;
    //StringIsFF : array[1..100000] of boolean;
    StrCount : integer;
    //these are unknow things so we set them as the same on write out
    //Byte3 : integer;
    //Byte4 : integer;
    Byte5 : integer;
    Byte6 : integer;
    Byte7 : integer;
    Byte8 : integer;
    property CurrentFileName : string read FCurrentFileName write SetCurrentFileName;
    property FileHasSaved : boolean read FHasSaved write SetFileHasSaved;
  end;

var
  frmMain : TfrmMain;

implementation

{ TfrmMain }

function RightStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    if RightStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := RightStr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := LeftStr(temp, length(temp) - 1);
  end;
end;


function LeftStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    //ShowMessage('temp: '+temp);
    if LeftStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := leftstr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := RightStr(temp, length(temp) - 1);
  end;
end;

function EncodeString(AString : string) : string;
var
  Str : string;
begin
  Str := AString;
  Str := StringReplace(Str, '&NBS;', #127, [rfReplaceAll]);
  Result := Str;
end;

function UnencodeString(AString : string) : string;
var
  Str : string;
begin
  Str := AString;
  Str := StringReplace(Str, #127, '&NBS;', [rfReplaceAll]);
  
  //this will remove incorrect new lines (char 13) thought to be created by TB's editor
  Str := StringReplace(Str, #13, '', [rfReplaceAll]);
  //Remove tabs, as they are invisible in the game
  Str := StringReplace(Str, '	', '', [rfReplaceAll]);
  //remove char 0, as it should not be here
  Str := StringReplace(Str, char(0), '', [rfReplaceAll]);
  Result := Str;
end;

procedure TfrmMain.SetCurrentFileName(ACFN : string);
begin
  FCurrentFileName := ACFN;
  SetFileHasSaved(FileHasSaved);
end;

procedure TfrmMain.SetFileHasSaved(AHasSave : boolean);
begin
  FHasSaved := AHasSave;

  if FileHasSaved then
  begin
    frmMain.Caption := 'LIB Decoder' + ' [' + CurrentFileName + '] ';
  end
  else
  begin
    frmMain.Caption := 'LIB Decoder' + ' [' + CurrentFileName + '] *';
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender : TObject; var CanClose : boolean);
var
  DlgResult : integer;
begin
  CanClose := false;
  if FileHasSaved then
  begin
    CanClose := true;
  end
  else
  begin
    // Not saved!
    DlgResult := MessageDlg(CurrentFileName + ' has not been saved. Do you want to save it now?', mtConfirmation, mbYesNoCancel, 1);
    if DlgResult = mrYes then //Yes (I want to save, then close)
    begin
      btnSaveClick(Sender);
      if FileHasSaved then
        CanClose := true
      else
        CanClose := false;
    end;
    if DlgResult = mrCancel then //Cancel (I don't won't to do anything at all!)
    begin
      CanClose := false;
    end;
    if DlgResult = mrNo then //No, forget about the file and close!
    begin
      CanClose := true;
    end;
  end;
end;

procedure TfrmMain.mmiAboutClick(Sender : TObject);
begin
  MessageDlg('Made by Lewin Hodgman.' + #13#10 + ' Version 0.8.2 (07/03/2009)', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.mmiExitClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmMain.SearchTreeView(SearchString:string;AMatchCase,AFromCursor,ABackwards,ATreatAsSpaces: boolean);
var
  CurrentItemText: string;
  i, StartPos, EndPos: integer;
  Finished, FoundMatch: boolean;
begin
    if AMatchCase = false then
    begin
      SearchString := UpperCase(SearchString);
    end;
    StartPos := 0;
    if ABackwards = false then
    begin
      StartPos := 0;
      EndPos := TreeView.Items.Count-1;
    end
    else
    begin
      StartPos := TreeView.Items.Count-1;
      EndPos := 0;
    end;
    if AFromCursor = true then
      if ABackwards = false then
        StartPos := TreeView.Selected.Index+1
      else
        StartPos := TreeView.Selected.Index-1;
    Finished := false;
    FoundMatch := false;
    i := StartPos;
    //ShowMessage('startpos :'+IntToStr(StartPos));
    //ShowMessage('endpos :'+IntToStr(EndPos));
    if (i >= 0) then
      Finished := false
    else
      Finished := true;
    if ABackwards = false then
      if (i <= EndPos) then
        Finished := false
      else
        Finished := true;
        
    if ABackwards = true then
      if (EndPos <= i) then
        Finished := false
      else
        Finished := true;
    
    //go through all items in the treeview
    while Finished = false do
    begin
       //ShowMessage('looping :'+IntToStr(i));
       CurrentItemText := TreeView.Items.Item[i].Text;
       //apply the options
       if ATreatAsSpaces = true then
       begin
         CurrentItemText := StringReplace(CurrentItemText,'&NBS;',' ',[rfReplaceAll]);
         CurrentItemText := StringReplace(CurrentItemText,'|',' ',[rfReplaceAll]);
       end;
       if AMatchCase = false then
       begin
         CurrentItemText := UpperCase(CurrentItemText);
       end;
       //do the test
       if Length(StringReplace(CurrentItemText,SearchString,'',[])) < Length(CurrentItemText) then
       begin
         //Found it
         TreeView.Selected := TreeView.Items.Item[i];
         //now also select the text in the memo
         SelectTextInMemo(SearchString,AMatchCase);
         FoundMatch := true;
         Finished := true;
       end;
       //manual i incrementing
       if ABackwards = true then
         i := i - 1
       else
         i := i + 1;
       if i = EndPos+1 then
         Finished := true;
    end;
    if FoundMatch = false then
      MessageDlg('Search string "'+SearchString+'" not found',mtWarning,[mbOK],0);
end;

procedure TfrmMain.SelectTextInMemo(ASearchString:string; MatchCase:boolean);
var
  Str: string;
begin
  //Replace |s with new lines, as that is what happens in the memo
  ASearchString := StringReplace(ASearchString,'|',#13,[rfReplaceAll]);
  //Now select it
  if MatchCase = false then
    Str := UpperCase(Memo.Lines.Text);
  if MatchCase = true then
    Str := Memo.Lines.Text;
  //ShowMessage('Start of string: '+IntToStr(Length(LeftStrTo(Str,ASearchString))));
  //ShowMessage('String: '+ASearchString+' Leftstr to pattern: '+LeftStrTo(Str,ASearchString)+' Memo Text: '+Str);
  Memo.SelStart := Length(LeftStrTo(Str,ASearchString));
  Memo.SelLength := Length(ASearchString);
  Memo.SetFocus;
end;

procedure TfrmMain.mmiFindNextClick(Sender: TObject);
begin
  SearchTreeView(frmFind.edtText.Text,frmFind.cbMatchCase.Checked,true,false,frmFind.cbTreatAsSpaces.Checked);
end;

procedure TfrmMain.mmiAddNewClick(Sender: TObject);
var
TempNodeData: TStringData;
NewNode: TTreeNode;
begin
  //Add a new string to inernal records and tree view
  StrCount := StrCount+1;

  StringsData[StrCount] := ' ';
  TempNodeData := TStringData.Create;
  TempNodeData.EndsInNL := false;
  //TempNodeData.FakeIndex := StrCount;
  TempNodeData.Index := StrCount;
  TempNodeData.Number := TStringData(TreeView.Items.Item[TreeView.Items.Count-1].Data).Index+1;
  NewNode := TreeView.Items.InsertBehind(TreeView.Items.Item[TreeView.Items.Count-1],' ');
  NewNode.Text := ' ';
  NewNode.Data := TempNodeData;
  TreeView.Selected := NewNode;
  //TreeView.Items.AddObject(,' ',);
end;

procedure TfrmMain.mmiEastEuropeClick(Sender: TObject);
begin
  SetCharSet(EASTEUROPE_CHARSET);
end;

procedure TfrmMain.mmiDeleteStringClick(Sender: TObject);
var
 ToSelect: TTreeNode;
begin
  if MessageDlg('Are you sure you want to delete the last string?',mtWarning,mbYesNo,0) = mrYes then
  begin
  StringsData[StrCount] := '';
  TStringData(TreeView.Items.Item[TreeView.Items.Count-1].Data).Free;

  ToSelect := TreeView.Items.Item[TreeView.Items.Count-1].GetPrev;
  
  TreeView.Items.Item[TreeView.Items.Count-1].Delete;

  StrCount := StrCount-1;
  
  TreeView.Selected := ToSelect;
  end;
end;

procedure TfrmMain.mmiDefaultClick(Sender: TObject);
begin
  SetCharSet(DEFAULT_CHARSET);
end;

procedure TfrmMain.mmiANSIClick(Sender: TObject);
begin
  if Sender = mmiANSI then
    SetCharSet(ANSI_CHARSET);
  if Sender = mmiSymbol then
    SetCharSet(SYMBOL_CHARSET);
  if Sender = mmiMac then
    SetCharSet(MAC_CHARSET);
  if Sender = mmiBaltic then
    SetCharSet(BALTIC_CHARSET);
  if Sender = mmiJapanese then
    SetCharSet(SHIFTJIS_CHARSET);
  if Sender = mmiHangeul then
    SetCharSet(HANGEUL_CHARSET);
  if Sender = mmiJohab then
    SetCharSet(JOHAB_CHARSET);
  if Sender = mmiGB2312 then
    SetCharSet(GB2312_CHARSET);
  if Sender = mmiBig5 then
    SetCharSet(CHINESEBIG5_CHARSET);
  if Sender = mmiGreek then
    SetCharSet(GREEK_CHARSET);
  if Sender = mmiTurkish then
    SetCharSet(TURKISH_CHARSET);
  if Sender = mmiVietnamese then
    SetCharSet(VIETNAMESE_CHARSET);
  if Sender = mmiHebrew then
    SetCharSet(HEBREW_CHARSET);
  if Sender = mmiArabic then
    SetCharSet(ARABIC_CHARSET);
  if Sender = mmiThai then
    SetCharSet(THAI_CHARSET);
end;

procedure TfrmMain.mmiFindPreviousClick(Sender: TObject);
begin
  SearchTreeView(frmFind.edtText.Text,frmFind.cbMatchCase.Checked,true,true,frmFind.cbTreatAsSpaces.Checked);
end;

procedure TfrmMain.mmiISO1Click(Sender: TObject);
begin
  SetCharSet(FCS_ISO_8859_1);
end;

procedure TfrmMain.mmiISO2Click(Sender: TObject);
begin
  SetCharSet(FCS_ISO_8859_2);
end;

procedure TfrmMain.mmiISO3Click(Sender: TObject);
begin
  SetCharSet(FCS_ISO_8859_3);
end;

procedure TfrmMain.mmiISO4Click(Sender: TObject);
begin
  SetCharSet(FCS_ISO_8859_4);
end;

procedure TfrmMain.mmiOEMClick(Sender: TObject);
begin
  SetCharSet(OEM_CHARSET);
end;

procedure TfrmMain.mmiRussianClick(Sender: TObject);
begin
  SetCharSet(RUSSIAN_CHARSET);
end;

procedure TfrmMain.mmiUnicodeClick(Sender: TObject);
begin
  SetCharSet(4);
end;

procedure TfrmMain.mmiFindClick(Sender: TObject);
begin
  if frmFind.ShowModal = mrOK then
  begin
    SearchTreeView(frmFind.edtText.Text,frmFind.cbMatchCase.Checked,frmFind.cbFromCursor.Checked,frmFind.cbBackwards.Checked,frmFind.cbTreatAsSpaces.Checked);
  end;
end;

procedure TfrmMain.mmiGoToIndexClick(Sender : TObject);
var
  FrmResult : integer;
  Node : TTreeNode;
  //FoundMatch: Boolean;
begin
  //FoundMatch := false;
  //first setup the goto form to look right (true = index)
  FrmResult := frmGoTo.ShowModal;
  if FrmResult = mrOK then
  begin
    //ShowMessage('got ok');
    //select the right one on the tree view
    Node := TreeView.Items[0];
    while Node <> nil do
    begin
      //ShowMessage('comparing strings value of '+IntToStr(TStringData(Node.Data).Index-1)+' with se value of '+IntToStr(frmGoTo.seIndex.Value));
      if frmGoTo.cbType.ItemIndex = 0 then
      if TStringData(Node.Data).Index - 1 = frmGoTo.seIndex.Value then
      begin
        //ShowMessage('Matched!');
          //FoundMatch := true;
        TreeView.Selected := Node;
        Break;
      end;
      
      {if frmGoTo.cbType.ItemIndex = 1 then
      if TStringData(Node.Data).FakeIndex - 1 = frmGoTo.seIndex.Value then
      begin
        //ShowMessage('Matched!');
          //FoundMatch := true;
        TreeView.Selected := Node;
        Break;
      end;}
      
      if frmGoTo.cbType.ItemIndex = 2 then
      if TStringData(Node.Data).Number = frmGoTo.seIndex.Value then
      begin
        //ShowMessage('Matched!');
          //FoundMatch := true;
        TreeView.Selected := Node;
        Break;
      end;
      Node := Node.GetNext;
    end;
    //if FoundMatch = false then
    //ShowMessage('no match found...');
  end;
end;

procedure TfrmMain.mmiLoadClick(Sender : TObject);
begin
  btnLoadClick(Sender);
end;

procedure TfrmMain.mmiRealHelpClick(Sender : TObject);
begin
  MessageDlg('Not done yet. Thanks for clicking!!', mtWarning, [mbOK], 0);
end;

procedure TfrmMain.mmiSaveAsClick(Sender : TObject);
begin
  btnSaveAsClick(Sender);
end;

procedure TfrmMain.mmiSaveClick(Sender : TObject);
begin
  btnSaveClick(Sender);
end;

procedure TfrmMain.mmiShowEmptyStringsClick(Sender : TObject);
begin

end;

procedure TfrmMain.btnAutoFormatClick(Sender : TObject);
var
  MemText, TempText : string;
  Done, Done2 : boolean;
  i, i2, i3 : integer;
begin
  if Length(Memo.Text) > 3 then
  begin
  //go through the memo text and add new line where they should be

  MemText := Memo.Text;
  //first remove all the current new lines and put spaces there
  MemText := StringReplace(MemText, #10, ' ', [rfReplaceAll]);
  MemText := StringReplace(MemText, #13, '', [rfReplaceAll]);
  MemText := StringReplace(MemText, '&NBS;', #127, [rfReplaceAll]);
  //remove double spaces
  MemText := StringReplace(MemText, '  ', ' ', [rfReplaceAll]);
  MemText := StringReplace(MemText, '  ', ' ', [rfReplaceAll]);
  MemText := StringReplace(MemText, '  ', ' ', [rfReplaceAll]);

  //now
  Done := false;
  i := 1;
  i2 := 1;
  TempText := '';
  Memo.Text := '';
  Memo.Clear;
  while Done <> true do
  begin
    TempText := TempText + MemText[i];
    if i2 = seAutoFormatLineNum.Value then
    begin
      //ShowMessage('stepping backwards');
      //step backward to the last space
      i3 := Length(TempText) - 1;
      Done2 := false;
      while Done2 <> true do
      begin
        //ShowMessage('looping test '+IntToStr(i3)+': '+TempText[i3]);
        if TempText[i3] = ' ' then
        begin
          //ShowMessage('changing i from '+IntToStr(i)+' to '+IntToStr(i-(Length(TempText)-i3)));
          i := i - (Length(TempText) - i3);
          TempText := LeftStr(TempText, i3 - 1);
          Done2 := true;
        end;
        i3 := i3 - 1;
        if i3 <= 1 then
          Done2 := true;
      end;
      //ShowMessage('loop done');
      if Memo.Text = '' then
        Memo.Lines.Text := StringReplace(TempText,#127,'&NBS;',[rfReplaceAll])
      else
        //place a new line
        Memo.Lines.Add(StringReplace(TempText,#127,'&NBS;',[rfReplaceAll]));
      //ShowMessage('Adding: "'+TempText+'"');
      TempText := '';
      //reset i2
      i2 := 0;
    end;
    //counter stuff
    i := i + 1;
    i2 := i2 + 1;
    if i = Length(MemText) + 1 then
    begin
      Memo.Lines.Add(StringReplace(TempText,#127,'&NBS;',[rfReplaceAll]));
      Done := true;
    end;
  end;
  //ShowMessage(TempText);
  //Memo.Text := TempText;
  end;
end;

procedure TfrmMain.InsertCharAtCarot(AChar: String);
var
  TempSelStart : integer;
begin
  TempSelStart := Memo.SelStart;
  Memo.Text := LeftStr(Memo.Text, Memo.SelStart) + AChar + RightStr(Memo.Text, Length(Memo.Text) - Memo.SelStart);

  Memo.SelStart := TempSelStart + Length(AChar);
end;

procedure TfrmMain.btnAddCharClick(Sender: TObject);
begin
  if (Sender as TBitBtn) = btnLSlash then
    InsertCharAtCarot('£');
  if (Sender as TBitBtn) = btnLSlashL then
    InsertCharAtCarot('³');
  if (Sender as TBitBtn) = btnNBS then
    InsertCharAtCarot('&NBS;');
  if (Sender as TBitBtn) = btnAU then
    InsertCharAtCarot('¥');
  if (Sender as TBitBtn) = btnAL then
    InsertCharAtCarot('¹');
  if (Sender as TBitBtn) = btnCU then
    InsertCharAtCarot('Æ');
  if (Sender as TBitBtn) = btnCL then
    InsertCharAtCarot('æ');
  if (Sender as TBitBtn) = btnEU then
    InsertCharAtCarot('Ê');
  if (Sender as TBitBtn) = btnEL then
    InsertCharAtCarot('ê');
  if (Sender as TBitBtn) = btnNU then
    InsertCharAtCarot('Ñ');
  if (Sender as TBitBtn) = btnNL then
    InsertCharAtCarot('ñ');
  if (Sender as TBitBtn) = btnOU then
    InsertCharAtCarot('Ó');
  if (Sender as TBitBtn) = btnOL then
    InsertCharAtCarot('ó');
  if (Sender as TBitBtn) = btnSU then
    InsertCharAtCarot('Œ');
  if (Sender as TBitBtn) = btnSL then
    InsertCharAtCarot('œ');
  if (Sender as TBitBtn) = btnZ1U then
    InsertCharAtCarot('');
  if (Sender as TBitBtn) = btnZ1L then
    InsertCharAtCarot('Ÿ');
  if (Sender as TBitBtn) = btnZ2U then
    InsertCharAtCarot('¯');
  if (Sender as TBitBtn) = btnZ2L then
    InsertCharAtCarot('¿');
end;

procedure TfrmMain.btnLoadClick(Sender : TObject);
var
 CanClose: boolean;
 DlgResult: integer;
begin
  CanClose := false;
  if FileHasSaved then
  begin
    CanClose := true;
  end
  else
  begin
    // Not saved!
    DlgResult := MessageDlg(CurrentFileName + ' has not been saved. Do you want to save it now?', mtConfirmation, mbYesNoCancel, 1);
    if DlgResult = mrYes then //Yes (I want to save, then close)
    begin
      btnSaveClick(Sender);
      if FileHasSaved then
        CanClose := true
      else
        CanClose := false;
    end;
    if DlgResult = mrCancel then //Cancel (I don't won't to do anything at all!)
    begin
      CanClose := false;
    end;
    if DlgResult = mrNo then //No, forget about the file and close!
    begin
      CanClose := true;
    end;
  end;
  
  if CanClose = true then
  if OpenDialog.Execute then
  begin
    TreeView.Items.Clear;
    Memo.Lines.Clear;
    LoadLIBFile(OpenDialog.FileName);
    CurrentFileName := OpenDialog.FileName;
    FileHasSaved := true;
  end;
end;

procedure TfrmMain.btnSaveAsClick(Sender : TObject);
begin
  SaveDialog.FileName := CurrentFileName;
  if SaveDialog.Execute then
  begin
    SaveLIBFile(SaveDialog.FileName);
    if FileExists(SaveDialog.FileName) then
    begin
      CurrentFileName := SaveDialog.FileName;
      FileHasSaved := true;
    end;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender : TObject);
begin
  if CurrentFileName <> '' then
    //save it as the same name as the one we opened
    SaveLIBFile(CurrentFileName);
  FileHasSaved := true;
end;

procedure TfrmMain.FormCreate(Sender : TObject);
begin
  //parse params
  if Application.ParamCount > 0 then
  begin
    //they have passed somthing in, assume it is a file
    if FileExists(Application.Params[1]) then
    begin
      TreeView.Items.Clear;
      Memo.Lines.Clear;
      LoadLIBFile(Application.Params[1]);
      CurrentFileName := Application.Params[1];
    end;
  end;
  FileHasSaved := true;
  SetCharSet(DEFAULT_CHARSET);
end;

procedure TfrmMain.LoadLIBFile(AFileName : string);
var
  LIBFile : file of char;
  i, Bytetwo : integer;
  c : char;
  i2, i3, ExtraCount, LastFirstFFIndex : integer;
  TempStr,TempStr2 : string;
  FileData : array[0..100000] of byte;
  NumCounter : integer;
  b : byte;
  TempNode, LastFirstFFNode : TTreeNode;
  TempStringData : TStringData;
  LastThingDone, ErrorTypeMsg : string;
  LastWasFF: boolean;
  LastStrLen: integer;
begin
  if FileExists(AFileName) then
  begin
    try
      //ShowMessage('start');
      LastThingDone := 'Opening File';
      //first load the entire file into a string
      AssignFile(LIBFile, AFileName);
      Reset(LIBFile);
      LastThingDone := 'Loading bytes into array';
      //ShowMessage('about to loop');
      i2 := 0;
      while not EOF(LIBFile) do
      begin
        Read(LIBFile, c);
        b := ord(c);
        FileData[i2] := b;
        i2 := i2 + 1;
      end;

      LastThingDone := 'Reopening File';
      CloseFile(LIBFile);
      AssignFile(LIBFile, AFileName);
      Reset(LIBFile);

      LastThingDone := 'Loading Length';
      //load string count from first two bytes
      Read(LIBFile, c);
      i := ord(c);
      Read(LIBFile, c);
      StrCount := i + (ord(c) * 256);

      LastThingDone := 'Loading unknown bytes';
      //load 6 unknown bytes to be written out at the end
      Read(LIBFile, c);
      LastStrLen := ord(c);
      Read(LIBFile, c);
      i := ord(c);
      LastStrLen := LastStrLen + (i * 256);
      Read(LIBFile, c);
      Byte5 := ord(c);
      //ShowMessage('5: '+IntToStr(Byte5));
      Read(LIBFile, c);
      Byte6 := ord(c);
      //ShowMessage('6: '+IntToStr(Byte6));
      Read(LIBFile, c);
      Byte7 := ord(c);
      //ShowMessage('7: '+IntToStr(Byte7));
      Read(LIBFile, c);
      Byte8 := ord(c);
      //ShowMessage('8: '+IntToStr(Byte8));

      LastThingDone := 'Starting proccessing loop';
      ExtraCount := 1;
      //FFCount := 0;
      LastWasFF := false;
      NumCounter := 1;
      LastFirstFFIndex := 1;
      for i3 := 1 to StrCount do
      begin
        LastThingDone := 'Loop ' + IntToStr(i3) + ': Loading index bytes';
        Read(LIBFile, c);
        i := ord(c);
        Read(LIBFile, c);
        Bytetwo := ord(c);
        //check for FF byte pars
        if (i = 255) and (Bytetwo = 255) then
        begin
          //We must convert this to a blank string

          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Creating node';
          //string is now in tempstr
          //if IsFF = true then
          //ShowMessage('Is ff');
          TempNode := TreeView.Items.Add(TreeView.Items.GetFirstNode, ' ');
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Creating string data';
          TempStringData := TStringData.Create;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Setting index';
          //TempStringData.FakeIndex := i3+1;
          TempStringData.Number := NumCounter+1;
          TempStringData.Index := i3+1;
          //TempStringData.Number := i3 - FFCount;
          TempStringData.EndsInNL := false;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Finishing';
          TempNode.Data := TempStringData;
          if LastWasFF = false then
            LastFirstFFNode := TempNode;
          //if TempNode.GetNextSibling <> nil then
          //  TempNode.MoveTo(TempNode.GetNextSibling,naInsert);
          StringsData[i3+1] := ' ';
          //selected it each time to make sure it is correct
          //if TempNode <> nil then
          //  TreeView.Selected := TempNode;
          NumCounter := NumCounter+1;
          if LastWasFF = false then
            LastFirstFFIndex := i3;
          LastWasFF := true;
        end
        else
        begin
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Finding length and location';
          i := i + (Bytetwo * 256);
          //load the text of the string
          TempStr := '';
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Loading string';
          //for the last string we must just go to the end of the file
          if i3 = StrCount then
          begin
            LastThingDone := 'Loop ' + IntToStr(i3) + ': LAST STRING';
            i := LastStrLen;
            {for i2 := ExtraCount to FileLength-1 do
            begin
              if Stop = false then
              begin
                if char(FileData[i2+(StrCount*2) + 5]) = char(0) then
                  Stop := true
                else
                  TempStr := TempStr + char(FileData[i2+(StrCount*2) + 5]);
              end;
            end;    }
            //ShowMessage('This is the last string. ExtraCount: '+IntToStr(ExtraCount)+' Location: '+IntToStr(i)+' Length '+IntToStr(i - 1 - ExtraCount));
          end;
          
          for i2 := ExtraCount to i - 1 do
          begin
            TempStr := TempStr + char(FileData[(StrCount * 2) + 5 + i2]);
          end;
          ExtraCount := i + 1;
          //fix non-breaking space
          TempStr := UnencodeString(TempStr);
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Creating node';
          //string is now in tempstr
          //if IsFF = true then
          //ShowMessage('Is ff');
          TempNode := TreeView.Items.Add(TreeView.Items.GetFirstNode, TempStr);
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Creating string data';
          TempStringData := TStringData.Create;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Is FF';
          //now we must make the index correct, a cording to weather or not the last string was an FF
          if (LastWasFF = true) then
          begin
            //TempStringData.FakeIndex := LastFirstFFIndex;
            TempStringData.Number := LastFirstFFIndex;
            TempStringData.Index := LastFirstFFIndex;
            TempStringData.EndsInNL := false;
            LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Testing NL';
            TempStr2 := Trim(TempStr);
            LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Testing NL If statment';
            if TempStr2 <> '' then
            if (TempStr2[Length(TempStr2)] = '|') then
            begin
              TempStringData.EndsInNL := true;
            end;
            LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Assigning data to node';
            TempNode.Text := TempStr;
            TempNode.Data := TempStringData;
            LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Saving internal string: '+IntToStr(LastFirstFFIndex-1);
            StringsData[LastFirstFFIndex] := TempStr;
            LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Moving Node';
            if LastFirstFFNode <> nil then
              TempNode.MoveTo(LastFirstFFNode,naInsert);
          end
          else
          begin
          //TempStringData.FakeIndex := i3;
          TempStringData.Number := NumCounter;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Setting index';
          TempStringData.Index := i3;
          //TempStringData.Number := i3 - FFCount;
          TempStringData.EndsInNL := false;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Testing NL';
          TempStr2 := Trim(TempStr);
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Setting string data - Testing NL If statment';
          if TempStr2 <> '' then
          if (TempStr2[Length(TempStr2)] = '|') then
          begin
            TempStringData.EndsInNL := true;
          end;
          LastThingDone := 'Loop ' + IntToStr(i3) + ': Adding string to tree view - Finishing';
          TempNode.Text := TempStr;
          TempNode.Data := TempStringData;
          StringsData[i3] := TempStr;
          end; //not enable FF
          LastWasFF := false;
          //selected it each time to make sure it is correct
          //if TempNode <> nil then
          //  TreeView.Selected := TempNode;
          NumCounter := NumCounter+1;
        end;
        {end
        else
          if i = 255 then
          begin
            StringIsFF[i3] := true;
            FFCount := FFCount + 1;
          end;}
      end;
      LastThingDone := 'Closing file';
      CloseFile(LIBFile);
      //try to make the scroll bar look ok
      TreeView.Selected := TreeView.Items.GetFirstNode;
      {i := 0;
      NotDone := True;
      //Now. I must move all the first strings after blank values to the begining of the FFs
      while NotDone = True do
      begin
        TreeView.Selected := TreeView.Items.Item[i];
        if TreeView.Items.Item[i].Text = ' ' then
        begin
          //this is an FF
          LastWasFF := true;
        end
        else
        begin
          if LastWasFF = true then
          begin
            //The last one was an FF, we must move it to the begining of this FF sequence
            TreeView.Items.Item[i].MoveTo(LastNonFFNode,naInsert);
          end;
          LastWasFF := false;
          LastNonFFNode := TreeView.Items.Item[i];
        end;
        i := i+1;
        //if at end, exit out
        if i = TreeView.Items.Count then
          NotDone := false;
      end;}
      
    except
      on EZeroDivide do
      begin
        ErrorTypeMsg := 'Division by 0';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EInterror do
      begin
        ErrorTypeMsg := 'Invalid integer';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on ERangeError do
      begin
        ErrorTypeMsg := 'Out of range';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EIntOverflow do
      begin
        ErrorTypeMsg := 'Integer over flow';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EMathError do
      begin
        ErrorTypeMsg := 'Mathamatical error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EInvalidOp do
      begin
        ErrorTypeMsg := 'Invalid operator';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EOverflow do
      begin
        ErrorTypeMsg := 'Over flow';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EUnderflow do
      begin
        ErrorTypeMsg := 'Under flow';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EInOutError do
      begin
        ErrorTypeMsg := 'Input output error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EHeapMemoryError do
      begin
        ErrorTypeMsg := 'Memory heap error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EHeapException do
      begin
        ErrorTypeMsg := 'Heap exception';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EExternalException do
      begin
        ErrorTypeMsg := 'External exception';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EInvalidPointer do
      begin
        ErrorTypeMsg := 'Invalid pointer';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EOutOfMemory do
      begin
        ErrorTypeMsg := 'Out of memory';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EAccessViolation do
      begin
        ErrorTypeMsg := 'Access Violation';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EConvertError do
      begin
        ErrorTypeMsg := 'Conversion error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EFormatError do
      begin
        ErrorTypeMsg := 'Format error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EOSError do
      begin
        ErrorTypeMsg := 'OS error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
      on EConvertError do
      begin
        ErrorTypeMsg := 'Conversion error';
        ReportError('Load Function', ErrorTypeMsg, LastThingDone);

      end;
    end;
  end
  else
  begin
    MessageDlg('File not found!', mtError, [mbOK], 0);
  end;
end;


procedure TfrmMain.SaveLIBFile(AFileName : string);
var
  TheFile : file;
  ToWrite, IndexToMoveDown : string;
  i, ExtraCount, FFCount, iFF, LastStrLen : integer;
  b : byte;
  LastWasFF: boolean;
begin
  if FileExists(AFileName) then
  begin
    DeleteFile(AFileName);
  end;
  ToWrite := '';
  //now start the real header

  LastWasFF := false;
  ExtraCount := 0;
  FFCount := 0;
  i := 1;
  
  //Write out the first one manually, to prevent it from becomming a FF byte par (which will break KaM)
  if Length(EncodeString(StringsData[i])) > 0 then
  begin
    ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) mod 256);
    ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) div 256);
    ExtraCount := ExtraCount + Length(EncodeString(StringsData[i])) + 1;
  end
  else
  begin
    //string is blank so write it blank
    ToWrite := ToWrite+char(0);
    ToWrite := ToWrite+char(0);
  end;

  //Start with the second string, as we did the first above
  for i := 2 to StrCount do
  begin
    if i = StrCount then
    begin
      if (StringsData[StrCount] = '') or (StringsData[StrCount] = 'FF') or (StringsData[StrCount] = ' ') then
        LastStrLen := ExtraCount
      else
        LastStrLen := Length(EncodeString(StringsData[i])) + 1 + ExtraCount;
    end
    else
    begin
      if (StringsData[i] = '') or (StringsData[i] = ' ') or (StringsData[i] = 'FF') then
      begin
        LastWasFF := true;
        FFCount := FFCount+1;
        //ToWrite := ToWrite + char(255);
        //ToWrite := ToWrite + char(255);
      end
      else
      begin
        if LastWasFF = true then
        begin
          //first we must put cut out the last index, and put it after all the FFs. This we must put this string
          IndexToMoveDown := RightStr(ToWrite,2);
          ToWrite := LeftStr(ToWrite,Length(ToWrite)-2);
          for iFF := 1 to FFCount do
          begin
            ToWrite := ToWrite + char(255);
            ToWrite := ToWrite + char(255);
          end;
          ToWrite := ToWrite + IndexToMoveDown;
          ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) mod 256);
          ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) div 256);
          ExtraCount := ExtraCount + Length(EncodeString(StringsData[i])) + 1;
        end
        else
        begin
          //as normal
          ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) mod 256);
          ToWrite := ToWrite + char((Length(EncodeString(StringsData[i])) + 1 + ExtraCount) div 256);
          ExtraCount := ExtraCount + Length(EncodeString(StringsData[i])) + 1;
        end;
        LastWasFF := false;
        FFCount := 0;
      end;
    end;
  end;
  //now we must see if the there are any FF bytes on the end of the file, and write them out
  if LastWasFF = true then
  begin
    //first we must put remove the last index, and put it an extra FF after the FFs
    //IndexToMoveDown := RightStr(ToWrite,2);
    ToWrite := LeftStr(ToWrite,Length(ToWrite)-2);
    for iFF := 1 to FFCount do
    begin
      ToWrite := ToWrite + char(255);
      ToWrite := ToWrite + char(255);
    end;
    ToWrite := ToWrite + char(255);
    ToWrite := ToWrite + char(255);
    //ToWrite := ToWrite + IndexToMoveDown;
  end;
  //now write the actual strings
  for i := 1 to StrCount do
  begin
    //don't do space strings (blank) either
    if (EncodeString(StringsData[i]) <> '') and (StringsData[i] <> 'FF') and (StringsData[i] <> ' ') and (LeftStr(StringsData[i],1) <> char(0)) then
    begin
      ToWrite := ToWrite + StringsData[i] + char(0);
    end;
  end;
  //add another 0 char as the footer
  ToWrite := ToWrite + char(0);
  //ToWrite := ToWrite + char(0) + char(0) + char(0) + char(0);
    
  ToWrite := EncodeString(ToWrite);
  
  //Now do the top header, in reverse so it is ok
  
  //8 byte header, containing the total number of strings
  ToWrite := char(Byte8) + ToWrite;
  ToWrite := char(Byte7) + ToWrite;
  ToWrite := char(Byte6) + ToWrite;
  ToWrite := char(Byte5) + ToWrite;
  ToWrite := char(LastStrLen div 256) + ToWrite;
  ToWrite := char(LastStrLen mod 256) + ToWrite;
  ToWrite := char(StrCount div 256) + ToWrite;
  ToWrite := char(StrCount mod 256) + ToWrite;

  //save the file
  AssignFile(TheFile, AFileName);
  i := 1;
  Rewrite(TheFile, 1);
  while i <= Length(ToWrite) do
  begin
    b := ord(char(ToWrite[i]));
    Blockwrite(TheFile, char(b), 1);
    inc(i);
  end;
  CloseFile(TheFile);
end;

procedure TfrmMain.MemoChange(Sender : TObject);
begin
  if TreeView.Selected <> nil then
  begin
    if TStringData(TreeView.Selected.Data).EndsInNL = true then
      StringsData[TStringData(TreeView.Selected.Data).Index] := StringReplace(StringReplace(Memo.Lines.Text, #10, '', [rfReplaceAll]), #13, '|', [rfReplaceAll])
    else
      StringsData[TStringData(TreeView.Selected.Data).Index] := StringReplace(TrimRight(StringReplace(TrimRight(Memo.Lines.Text), #10, '', [rfReplaceAll])), #13, '|', [rfReplaceAll]);
    if StringsData[TStringData(TreeView.Selected.Data).Index] = '' then
      StringsData[TStringData(TreeView.Selected.Data).Index] := ' ';
    TreeView.Selected.Text := StringsData[TStringData(TreeView.Selected.Data).Index];
    FileHasSaved := false;
    lblStrLen.Caption := 'String Length: ' + IntToStr(Length(EncodeString(StringsData[TStringData(TreeView.Selected.Data).Index])));
    if (TreeView.Selected.Text = '') or
      (TreeView.Selected.Text = ' ') or
      (TreeView.Selected.Data = nil) then
      lblStrLen.Caption := 'String Length: 0';
  end;
end;

procedure TfrmMain.TreeViewSelectionChanged(Sender : TObject);
var
Str: String;
begin
  if TreeView.Selected <> nil then
  begin
    //ShowMessage(IntToStr(TStringData(TreeView.Selected.Data).Number));
    //now update the indexes
    lblStrIndex.Caption := 'String Index: ' + IntToStr(TStringData(TreeView.Selected.Data).Index - 1) + ' (Also used for in-game messages)';
    //lblStrIGIndex.Caption := 'In-Game Index: ' + IntToStr(TStringData(TreeView.Selected.Data).FakeIndex - 1) + ' (Also used for in-game messages)';
    lblStrNum.Caption := 'String Number: ' + IntToStr(TStringData(TreeView.Selected.Data).Number);
    lblStrLen.Caption := 'String Length: ' + IntToStr(Length(EncodeString(StringsData[TStringData(TreeView.Selected.Data).Index])));
    //put the text for this string in the box on the left
    Memo.Lines.Clear;
    //use str so that it happens invisiblaly
    Str := TrimRight(StringReplace(UnencodeString(StringsData[TStringData(TreeView.Selected.Data).Index]), '|', #13, [rfReplaceAll]));
    Memo.Lines.Text := Str;
    if (TreeView.Selected.Text = '') or
      (TreeView.Selected.Text = ' ') or
      (TreeView.Selected.Data = nil) then
      lblStrLen.Caption := 'String Length: 0';
  end;
end;

procedure TfrmMain.ReportError(AMainSection : string; AErrorTypeMsg : string; ALastThingDone : string; ANotes : string = '');
var
  NL : string;
begin
  if ANotes = '' then
    ANotes := 'If you think this is a bug then please send me these details and the file you were working with at the time. My email: lewinjh@gmail.com';

  NL := #13#10;
  MessageDlg('An error has occoured. What ever was currently being processed will not have completed succesfully. Error Details:' + NL +
    NL + 'Function: ' + AMainSection + NL + 'Type: ' + AErrorTypeMsg + NL + 'Section: ' + ALastThingDone + NL +
    NL + ANotes +
    NL + 'It is highly recommended for you to close the application before you continue to use it, to aviod memory leaks', mtError, [mbOK], 0);
end;

procedure TfrmMain.SetCharSet(CharSet : TFontCharSet);
begin
  frmMain.Font.CharSet := CharSet;
  TreeView.Font.CharSet := CharSet;
  Memo.Font.CharSet := CharSet;
end;

initialization
  {$I umain.lrs}

end.
