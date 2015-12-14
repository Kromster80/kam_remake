unit Main;
interface
uses
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, SysUtils, Classes, StdCtrls, StrUtils, INIFiles;

type
  TForm1 = class(TForm)
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
    btnGenerate: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnGenerateClick(Sender: TObject);
    procedure txtParserOutputKeyPress(Sender: TObject; var Key: Char);
    procedure edtOnTextChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fSettingsPath: string;
    fSafeToWrite: Boolean;
    procedure txtParser(aFile: string; aList: TStringList);
    function paramParser(aString: string): string;
    procedure Reinit;
  end;

  TParamHolder = record
    Name: string;
    varType: Integer;
  end;

const
  VAR_TYPES: array[0..42] of string = (
    'Byte', 'Shortint', 'Smallint', 'Word', 'Integer', 'Cardinal', 'Longint',
    'Longword', 'Int64', 'QWord', 'Real', 'Single', 'Double', 'Extended',
    'Currency', 'TByteSet', 'Boolean', 'ByteBool', 'WordBool', 'LongBool',
    'Char', 'WideChar', 'PChar', 'String', 'AnsiString', 'UnicodeString',
    'TKMHandIndex', 'TKMHouse', 'TKMUnit', 'TKMUnitGroup',
    'TKMHouseType', 'TKMWareType', 'TKMFieldType', 'TKMUnitType',
    'THouseType', 'TWareType', 'TFieldType', 'TUnitType',
    'array of const', 'Array of const', 'Array of Const',
    'array of Integer', 'Array of Integer'
  );

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Click;
end;


procedure TForm1.Reinit;
var
  Settings: TINIFile;
begin
  Settings := TINIFile.Create(fSettingsPath);

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

  fSafeToWrite := True;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;


{
  Parses the param string into prefered wiki-format.
  Results:
  1 - [name]: [type];
  2 - etc
}
function TForm1.paramParser(aString: string): string;
var
  i, varTypeInt, nextType: Integer;
  isType: Boolean;
  splitList, paramList, typeList: TStringList;
  resultStr, varTypeName: string;
  paramHolder: array of TParamHolder;
begin
  splitList   := TStringList.Create;
  paramList   := TStringList.Create;
  typeList    := TStringList.Create;
  try
    resultStr   := '';
    // If not set to -1 it skips the first variable
    nextType    := -1;

    splitList.AddStrings(aString.Split([' ']));

    // Re-combine type arrays
    for i := 0 to splitList.Count - 1 do
    begin
      splitList[i] := splitList[i].TrimRight([',', ':', ';']);

      if splitList[i] = 'array' then
      begin
        nextType := i + 2;
        // For some reason this kept giving 'array of Integer;' hence the trim
        paramList.Add((splitList[i] + ' ' + splitList[nextType - 1] + ' ' + splitList[nextType]).TrimRight([',', ':', ';']));
      end else
        // Skip unused stuff
        if not ((splitList[i] = 'of') or (splitList[i] = 'const') or (i = nextType)) then
          paramList.Add(splitList[i]);
    end;

    // Bind variable names to their type
    for i := 0 to paramList.Count - 1 do
    begin
      isType := False;
      for varTypeName in VAR_TYPES do
        if paramList[i] = varTypeName then
        begin
          typeList.Add(paramList[i]);
          Inc(varTypeInt);
          isType := True;
        end;
      if not isType then
      begin
        SetLength(paramHolder, Length(paramHolder) + 1);
        paramHolder[High(paramHolder)].Name := paramList[i];
        paramHolder[High(paramHolder)].varType := varTypeInt;
      end;
    end;

    // Add numbers and line-break tags
    for i := 0 to Length(paramHolder) - 1 do
    begin
      resultStr := resultStr + IntToStr(i + 1) + ' - ' + paramHolder[i].Name + ': ' + typeList[paramHolder[i].varType] + ';';
      if not (i = Length(paramHolder) - 1) then
        resultStr := resultStr + ' <br> ';
    end;
    Result := resultStr;
  finally
    FreeAndNil(splitList);
    FreeAndNil(paramList);
    FreeAndNil(typeList);
  end;
end;


// Scans file's contents and puts it all in proper formatting for most wikis.
procedure TForm1.txtParser(aFile: string; aList: TStringList);
var
  i, iPlus: Integer;
  versionStr, descStr, restStr, finalStr: string;
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
        if SourceTxt[i+iPlus].Contains('(') then
        begin
          restStr := ' | ' + Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('.') + 2,
                                  SourceTxt[i+iPlus].IndexOf('(') - (SourceTxt[i+iPlus].IndexOf('.') + 1));
          restStr := ReplaceStr(restStr, 'Proc', 'On');
          restStr := restStr + descStr + ' | ' + paramParser(Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                  SourceTxt[i+iPlus].IndexOf(')') - (
                                                                  SourceTxt[i+iPlus].IndexOf('(') + 1))) + ' | |';
        end else
        begin
          restStr := ' | ' + Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('.') + 2,
                                  SourceTxt[i+iPlus].IndexOf(';') - (SourceTxt[i+iPlus].IndexOf('.') + 1));
          restStr := ReplaceStr(restStr, 'Proc', 'On');
          restStr := restStr.TrimRight([';']) + descStr + ' | | |';
        end;

        finalStr := versionStr + restStr;
      end;

      // Format functions
      if SourceTxt[i+iPlus].StartsWith('function') and not (versionStr = '') then
      begin
        if SourceTxt[i+iPlus].Contains('(') then
        begin
          restStr := ' | ' + Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('.') + 2,
                                  SourceTxt[i+iPlus].IndexOf('(') - (SourceTxt[i+iPlus].IndexOf('.') + 1));
          restStr := ReplaceStr(restStr, 'Func', 'On');
          restStr := restStr + descStr + ' | ' + paramParser(Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                  SourceTxt[i+iPlus].IndexOf(')') - (
                                                                  SourceTxt[i+iPlus].IndexOf('(') + 1))) + ' | ';
        end else
        begin
          restStr := ' | ' + Copy(SourceTxt[i+iPlus], SourceTxt[i+iPlus].IndexOf('.') + 2,
                                  SourceTxt[i+iPlus].IndexOf(':') - (SourceTxt[i+iPlus].IndexOf('.') + 1));
          restStr := ReplaceStr(restStr, 'Func', 'On');
          restStr := restStr + descStr + ' | | ';
        end;

        restStr  := restStr + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].LastIndexOf(':') + 2);
        restStr  := restStr.TrimRight([';']) + ' |';
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


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  Filename: string;
  fListActions, fListEvents, fListStates: TStringList;
begin
  txtParserOutput.Lines.Clear;

  fListActions := TStringList.Create;
  fListEvents  := TStringList.Create;
  fListStates  := TStringList.Create;

  if FileExists(edtActionsFile.Text) then
  begin
    fListActions.Add('####Actions' + sLineBreak);
    txtParser(edtActionsFile.Text, fListActions);
    txtParserOutput.Lines.AddStrings(fListActions);
  end else
    raise Exception.Create('File does not exist.');

  if FileExists(edtEventsFile.Text) then
  begin
    fListEvents.Add('####Events' + sLineBreak);
    txtParser(edtEventsFile.Text, fListEvents);
    txtParserOutput.Lines.AddStrings(fListEvents);
  end else
    raise Exception.Create('File does not exist.');

  if FileExists(edtStatesFile.Text) then
  begin
    fListStates.Add('####States' + sLineBreak);
    txtParser(edtStatesFile.Text, fListStates);
    txtParserOutput.Lines.AddStrings(fListStates);
  end else
    raise Exception.Create('File does not exist.');

  if edtOutputFileActions.Text <> '' then
    fListActions.SaveToFile(edtOutputFileActions.Text);

  if edtOutputFileEvents.Text <> '' then
    fListEvents.SaveToFile(edtOutputFileEvents.Text);

  if edtOutputFileStates.Text <> '' then
    fListStates.SaveToFile(edtOutputFileStates.Text);

  FreeAndNil(fListActions);
  FreeAndNil(fListEvents);
  FreeAndNil(fListStates);
end;


procedure TForm1.txtParserOutputKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  fSettingsPath := ExtractFilePath(Application.ExeName) + 'ScriptingParser.ini';
  Reinit;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  fSettingsPath := ExtractFilePath(Application.ExeName) + 'ScriptingParser2.ini';
  Reinit;
end;


procedure TForm1.edtOnTextChange(Sender: TObject);
var
  Settings: TINIFile;
begin
  if not fSafeToWrite then Exit;

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


end.
