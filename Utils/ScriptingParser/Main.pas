unit Main;

interface

uses
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs,
  SysUtils, Classes, StdCtrls, StrUtils, INIFiles;

type
  TForm1 = class(TForm)
    OpenTxtDlg: TOpenTextFileDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtParserOutput: TMemo;
    edtActionsFile: TEdit;
    edtEventsFile: TEdit;
    edtStatesFile: TEdit;
    edtOutputFileActions: TEdit;
    edtOutputFileEvents: TEdit;
    edtOutputFileStates: TEdit;
    btnParse: TButton;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnParseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure txtParserOutputKeyPress(Sender: TObject; var Key: Char);
    procedure edtOnTextChange(Sender: TObject);
  private
    SettingsFile: TINIFile;
    iniLoc: String;
    actStringList, evntStringList, stsStringList: TStringList;
    safeToWrite: Boolean;
    procedure txtParser(aFile: String; aList: TStringList);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenTxtDlg        := TOpenTextFileDialog.Create(Self);
  OpenTxtDlg.Filter := 'Pascal files (*.pas)|*.PAS|Any file (*.*)|*.*';
  iniLoc            := ExtractFilePath(Application.ExeName) + '\settings.ini';
  SettingsFile      := TINIFile.Create(iniLoc);

  if not FileExists(iniLoc) then
  begin
    SettingsFile.WriteString('INPUT',  'Actions', '');
    SettingsFile.WriteString('INPUT',  'Events',  '');
    SettingsFile.WriteString('INPUT',  'States',  '');
    SettingsFile.WriteString('OUTPUT', 'Actions', '');
    SettingsFile.WriteString('OUTPUT', 'Events',  '');
    SettingsFile.WriteString('OUTPUT', 'States',  '');
  end;

  edtActionsFile.Text       := SettingsFile.ReadString('INPUT',  'Actions', '');
  edtEventsFile.Text        := SettingsFile.ReadString('INPUT',  'Events',  '');
  edtStatesFile.Text        := SettingsFile.ReadString('INPUT',  'States',  '');
  edtOutputFileActions.Text := SettingsFile.ReadString('OUTPUT', 'Actions', '');
  edtOutputFileEvents.Text  := SettingsFile.ReadString('OUTPUT', 'Events',  '');
  edtOutputFileStates.Text  := SettingsFile.ReadString('OUTPUT', 'States',  '');
  FreeAndNil(SettingsFile);

  actStringList  := TStringList.Create;
  evntStringList := TStringList.Create;
  stsStringList  := TStringList.Create;
  safeToWrite    := True;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(actStringList);
  FreeAndNil(evntStringList);
  FreeAndNil(stsStringList);
end;


// Scans file's contents and puts it all in proper formatting for most wikis.
procedure TForm1.txtParser(aFile: String; aList: TStringList);
var
  i, iPlus: Integer;
  versionStr, descStr, restStr, finalStr: String;
  SourceTxt: TStringList;
begin
  SourceTxt := TStringList.Create;
  try
    aList.Add('| Version | Name | Description | Parameters and types | Returns |');
    aList.Add('| ------- | ---- | ----------- | -------------------- | ------- |');
    SourceTxt.LoadFromFile(aFile);

    for i := 0 to SourceTxt.Count-1 do
    begin
      versionStr := '';
      descStr    := ' |';
      restStr    := '';
      finalStr   := '';
      iPlus      := 0;

      // Before anything it should start with "//* Version:"
      if SourceTxt[i].StartsWith('//* Version:') then
      begin
        versionStr := '| ' + SourceTxt[i].Substring(SourceTxt[i].IndexOf(':') + 2);
        Inc(iPlus);
      end;

      // Descriptions are only added by lines starting with "//* "
      if SourceTxt[i+iPlus].StartsWith('//* ') and not (versionStr = '') then
      begin
        // Repeat until no description tags are found
        while SourceTxt[i+iPlus].StartsWith('//* ') do
        begin
          descStr := descStr + ' ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('*') + 2);
          Inc(iPlus);
        end;
      end;

      // Skip empty or "faulty" lines
      while not ((SourceTxt[i+iPlus].StartsWith('procedure')) or
                 (SourceTxt[i+iPlus].StartsWith('function'))) and not (versionStr = '') do
        Inc(iPlus);

      // Format procedures
      if SourceTxt[i+iPlus].StartsWith('procedure') and not (versionStr = '') then
      begin
        restStr := ' | ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('.') + 1);
        restStr := ReplaceStr(restStr, 'Proc', 'On');

        if restStr.Contains('(') then
        begin
          restStr := ReplaceStr(restStr, '(', descStr + ' | `');
          restStr := restStr.TrimRight([')', ';']) + '` | |';
        end else
          restStr := restStr.TrimRight([';']) + descStr + ' | | |';

        finalStr := versionStr + restStr;
      end;

      // Format functions
      if SourceTxt[i+iPlus].StartsWith('function') and not (versionStr = '') then
      begin
        restStr := ' | ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('.') + 1);
        Delete(restStr, restStr.LastIndexOf(':')+1, restStr.Length);
        restStr := ReplaceStr(restStr, 'Proc', 'On');

        if restStr.Contains('(') then
        begin
          restStr := ReplaceStr(restStr, '(', descStr + ' | `');
          restStr := restStr.TrimRight([')', ';']) + '` | `';
        end else
          restStr := restStr + descStr + ' | | `';

        restStr  := restStr + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].LastIndexOf(':') + 2);
        restStr  := restStr.TrimRight([';']) + '` |';
        finalStr := versionStr + restStr;
      end;

      if not (finalStr = '') then
        aList.Add(finalStr);
    end;
  finally
    aList.Add('' + sLineBreak + '');
    FreeAndNil(SourceTxt);
  end;
end;


procedure TForm1.btnParseClick(Sender: TObject);
var
  Filename: String;
begin
  txtParserOutput.Lines.Clear;
  actStringList.Clear;
  evntStringList.Clear;
  stsStringList.Clear;

  if not (edtActionsFile.Text = '') then
  begin
    if FileExists(edtActionsFile.Text) then
    begin
      actStringList.Clear;
      actStringList.Add('##Actions' + sLineBreak);
      txtParser(edtActionsFile.Text, actStringList);
      txtParserOutput.Lines.AddStrings(actStringList);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if not (edtEventsFile.Text = '') then
  begin
    if FileExists(edtEventsFile.Text) then
    begin
      evntStringList.Clear;
      evntStringList.Add('##Events' + sLineBreak);
      txtParser(edtEventsFile.Text, evntStringList);
      txtParserOutput.Lines.AddStrings(evntStringList);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if not (edtStatesFile.Text = '') then
  begin
    if FileExists(edtStatesFile.Text) then
    begin
      stsStringList.Clear;
      stsStringList.Add('##States' + sLineBreak);
      txtParser(edtStatesFile.Text, stsStringList);
      txtParserOutput.Lines.AddStrings(stsStringList);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if txtParserOutput.Lines.Count <= 0 then
    if OpenTxtDlg.Execute(Self.Handle) then
    begin
      Filename := OpenTxtDlg.FileName;

      if FileExists(Filename) then
      begin
        actStringList.Clear;
        txtParser(Filename, actStringList);
        txtParserOutput.Lines.Assign(actStringList);
      end else
        raise Exception.Create('File does not exist.');
    end;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
var
  Filename: String;
begin
  if txtParserOutput.Lines.Count > 0 then
  begin
    if not (edtOutputFileActions.Text = '') and (actStringList.Count > 0) then
    begin
      Filename := edtOutputFileActions.Text;
      actStringList.SaveToFile(Filename);
    end;

    if not (edtOutputFileEvents.Text = '') and (evntStringList.Count > 0) then
    begin
      Filename := edtOutputFileEvents.Text;
      evntStringList.SaveToFile(Filename);
    end;

    if not (edtOutputFileStates.Text = '') and (stsStringList.Count > 0) then
    begin
      Filename := edtOutputFileStates.Text;
      stsStringList.SaveToFile(Filename);
    end;
  end;
end;


procedure TForm1.txtParserOutputKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;


procedure TForm1.edtOnTextChange(Sender: TObject);
begin
  if safeToWrite then
  begin
    SettingsFile := TINIFile.Create(iniLoc);

    if Sender = edtActionsFile then
      SettingsFile.WriteString('INPUT',  'Actions', edtActionsFile.Text);

    if Sender = edtEventsFile then
      SettingsFile.WriteString('INPUT',  'Events',  edtEventsFile.Text);

    if Sender = edtStatesFile then
      SettingsFile.WriteString('INPUT',  'States',  edtStatesFile.Text);

    if Sender = edtOutputFileActions then
      SettingsFile.WriteString('OUTPUT', 'Actions',  edtOutputFileActions.Text);

    if Sender = edtOutputFileEvents then
      SettingsFile.WriteString('OUTPUT', 'Events',  edtOutputFileEvents.Text);

    if Sender = edtOutputFileStates then
      SettingsFile.WriteString('OUTPUT', 'States',  edtOutputFileStates.Text);

    FreeAndNil(SettingsFile);
  end;
end;

end.
