unit Main;
interface
uses
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, SysUtils, Classes, StdCtrls, StrUtils, INIFiles;

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
    fSettingsPath: String;
    fListActions, fListEvents, fListStates: TStringList;
    fSafeToWrite: Boolean;
    procedure txtParser(aFile: String; aList: TStringList);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Settings: TINIFile;
begin
  fSettingsPath := ExtractFilePath(Application.ExeName) + 'ScriptingParser.ini';
  Settings      := TINIFile.Create(fSettingsPath);

  if not FileExists(fSettingsPath) then
  begin
    Settings.WriteString('INPUT',  'Actions', '..\..\src\scripting\KM_ScriptingActions.pas');
    Settings.WriteString('INPUT',  'Events',  '..\..\src\scripting\KM_ScriptingEvents.pas');
    Settings.WriteString('INPUT',  'States',  '..\..\src\scripting\KM_ScriptingStates.pas');
    Settings.WriteString('OUTPUT', 'Actions', 'Actions.wiki');
    Settings.WriteString('OUTPUT', 'Events',  'Events.wiki');
    Settings.WriteString('OUTPUT', 'States',  'States.wiki');
  end;

  edtActionsFile.Text       := Settings.ReadString('INPUT',  'Actions', '');
  edtEventsFile.Text        := Settings.ReadString('INPUT',  'Events',  '');
  edtStatesFile.Text        := Settings.ReadString('INPUT',  'States',  '');
  edtOutputFileActions.Text := Settings.ReadString('OUTPUT', 'Actions', '');
  edtOutputFileEvents.Text  := Settings.ReadString('OUTPUT', 'Events',  '');
  edtOutputFileStates.Text  := Settings.ReadString('OUTPUT', 'States',  '');
  FreeAndNil(Settings);

  fListActions := TStringList.Create;
  fListEvents  := TStringList.Create;
  fListStates  := TStringList.Create;
  fSafeToWrite := True;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(fListActions);
  FreeAndNil(fListEvents);
  FreeAndNil(fListStates);
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
  fListActions.Clear;
  fListEvents.Clear;
  fListStates.Clear;

  if not (edtActionsFile.Text = '') then
  begin
    if FileExists(edtActionsFile.Text) then
    begin
      fListActions.Clear;
      fListActions.Add('##Actions' + sLineBreak);
      txtParser(edtActionsFile.Text, fListActions);
      txtParserOutput.Lines.AddStrings(fListActions);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if not (edtEventsFile.Text = '') then
  begin
    if FileExists(edtEventsFile.Text) then
    begin
      fListEvents.Clear;
      fListEvents.Add('##Events' + sLineBreak);
      txtParser(edtEventsFile.Text, fListEvents);
      txtParserOutput.Lines.AddStrings(fListEvents);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if not (edtStatesFile.Text = '') then
  begin
    if FileExists(edtStatesFile.Text) then
    begin
      fListStates.Clear;
      fListStates.Add('##States' + sLineBreak);
      txtParser(edtStatesFile.Text, fListStates);
      txtParserOutput.Lines.AddStrings(fListStates);
    end else
      raise Exception.Create('File does not exist.');
  end;

  if txtParserOutput.Lines.Count <= 0 then
    if OpenTxtDlg.Execute(Self.Handle) then
    begin
      Filename := OpenTxtDlg.FileName;

      if FileExists(Filename) then
      begin
        fListActions.Clear;
        txtParser(Filename, fListActions);
        txtParserOutput.Lines.Assign(fListActions);
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
    if not (edtOutputFileActions.Text = '') and (fListActions.Count > 0) then
    begin
      Filename := edtOutputFileActions.Text;
      fListActions.SaveToFile(Filename);
    end;

    if not (edtOutputFileEvents.Text = '') and (fListEvents.Count > 0) then
    begin
      Filename := edtOutputFileEvents.Text;
      fListEvents.SaveToFile(Filename);
    end;

    if not (edtOutputFileStates.Text = '') and (fListStates.Count > 0) then
    begin
      Filename := edtOutputFileStates.Text;
      fListStates.SaveToFile(Filename);
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
var
  Settings: TINIFile;
begin
  if fSafeToWrite then
  begin
    Settings := TINIFile.Create(fSettingsPath);

    if Sender = edtActionsFile then
      Settings.WriteString('INPUT',  'Actions', edtActionsFile.Text);

    if Sender = edtEventsFile then
      Settings.WriteString('INPUT',  'Events',  edtEventsFile.Text);

    if Sender = edtStatesFile then
      Settings.WriteString('INPUT',  'States',  edtStatesFile.Text);

    if Sender = edtOutputFileActions then
      Settings.WriteString('OUTPUT', 'Actions',  edtOutputFileActions.Text);

    if Sender = edtOutputFileEvents then
      Settings.WriteString('OUTPUT', 'Events',  edtOutputFileEvents.Text);

    if Sender = edtOutputFileStates then
      Settings.WriteString('OUTPUT', 'States',  edtOutputFileStates.Text);

    FreeAndNil(Settings);
  end;
end;


end.
