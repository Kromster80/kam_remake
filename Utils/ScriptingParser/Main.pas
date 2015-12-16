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
    procedure ParseText(aFile: string; aList: TStringList);
    function ParseParams(aString: string; aDescriptions: TStringList): string;
    procedure Reinit;
  end;

  TParamHolder = record
    Name, varType: string;
  end;

  TCommandInfo = record
    Version: string;
    Name: string;
    Description: string;
    Parameters: string;
    Return: string;
    ReturnDesc: string;
  end;

const
  VAR_TYPE_NAME: array[0..24] of string = (
    'Byte', 'Shortint', 'Smallint', 'Word', 'Integer', 'Cardinal', 'Single', 'Boolean', 'String',
    'array of const', 'array of Integer',
    'TKMHouseType', 'TKMWareType', 'TKMFieldType', 'TKMUnitType',
    'THouseType', 'TWareType', 'TFieldType', 'TUnitType',
    'TKMObjectiveStatus', 'TKMObjectiveType',
    'TKMHouse', 'TKMUnit', 'TKMUnitGroup', 'TKMHandIndex' // Werewolf types
  );

  VAR_TYPE_ALIAS: array[0..24] of string = (
    'Byte', 'Shortint', 'Smallint', 'Word', 'Integer', 'Cardinal', 'Single', 'Boolean', 'String',
    'array of const', 'array of Integer',
    'TKMHouseType', 'TKMWareType', 'TKMFieldType', 'TKMUnitType',
    'THouseType', 'TWareType', 'TFieldType', 'TUnitType',
    'TKMObjectiveStatus', 'TKMObjectiveType',
    'Integer', 'Integer', 'Integer', 'Integer' // Werewolf types
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
function TForm1.ParseParams(aString: string; aDescriptions: TStringList): string;
var
  i, j, K, nextType: Integer;
  isParam: Boolean;
  listTokens, paramList, typeList: TStringList;
  paramHolder: array of TParamHolder;
  lastType: string;
begin
  Result := '';

  listTokens := TStringList.Create;
  paramList := TStringList.Create;
  typeList  := TStringList.Create;
  try
    // If not set to -1 it skips the first variable
    nextType := -1;

    listTokens.AddStrings(aString.Split([' ']));

    // Re-combine type arrays
    for i := 0 to listTokens.Count - 1 do
    begin
      listTokens[i] := listTokens[i].TrimRight([',', ':', ';']);

      if SameText(listTokens[i], 'array') then
      begin
        nextType := i + 2;
        // For some reason this kept giving 'array of Integer;' hence the trim
        paramList.Add((listTokens[i] + ' ' + listTokens[nextType - 1] + ' ' + listTokens[nextType]).TrimRight([',', ':', ';']));
      end else
        // Skip unused stuff
        if not ((SameText(listTokens[i], 'of')) or (SameText(listTokens[i], 'const')) or (i = nextType)) then
          paramList.Add(listTokens[i]);
    end;

    // Bind variable names to their type
    // Use reverse scan, so that we can remember last met type and apply it to all preceeding parameters
    lastType := '';
    for i := paramList.Count - 1 downto 0 do
    begin
      // See if this token is a Type
      isParam := True;
      for K := 0 to High(VAR_TYPE_NAME) do
        if SameText(VAR_TYPE_NAME[K], paramList[i]) then
        begin
          lastType := VAR_TYPE_ALIAS[K];
          isParam := False;
          Break;
        end;

      if isParam then
      begin
        SetLength(paramHolder, Length(paramHolder) + 1);
        paramHolder[High(paramHolder)].Name := paramList[i];
        paramHolder[High(paramHolder)].varType := lastType;
      end;
    end;

    // Add numbers and line-breaks
    for i := High(paramHolder) downto 0 do
    begin
      Result := Result + {IntToStr(High(paramHolder) - i + 1) + ' - ' +} paramHolder[i].Name + ': ' + paramHolder[i].varType + ';';

      // Add micro descriptions to the parameters and remove them from the stringlist.
      for j := aDescriptions.Count - 1 downto 0 do
        if aDescriptions[j].StartsWith(paramHolder[i].Name) then
        begin
          Result := Result + ' <br> ' + aDescriptions[j].Substring(aDescriptions[j].IndexOf(':') + 2);
          aDescriptions.Delete(j);
          Break;
        end;

      if i <> 0 then
        Result := Result + ' <br> ';
    end;
  finally
    FreeAndNil(listTokens);
    FreeAndNil(paramList);
    FreeAndNil(typeList);
  end;
end;


// Scans file's contents and puts it all in proper formatting for most wikis.
procedure TForm1.ParseText(aFile: string; aList: TStringList);
var
  i, j, iPlus: Integer;
  restStr: string;
  sourceTxt, descrTxt: TStringList;
  res: TCommandInfo;
begin
  sourceTxt := TStringList.Create;
  descrTxt  := TStringList.Create;
  try
    sourceTxt.LoadFromFile(aFile);

    for i := 0 to SourceTxt.Count - 1 do
    begin
      // Reset old values
      res.Version     := '';
      res.Name        := '';
      res.Description := '';
      res.Parameters  := '';
      res.Return      := '';
      res.ReturnDesc  := '';
      iPlus := 0;
      descrTxt.Clear;

      // Before anything it should start with "//* Version:"
      if sourceTxt[i].StartsWith('//* Version:') then
      begin
        restStr := Trim(sourceTxt[i].Substring(sourceTxt[i].IndexOf(':') + 2));
        res.Version := IfThen(restStr = '', '-', restStr);
        Inc(iPlus);

        // Descriptions are only added by lines starting with "//* "
        if sourceTxt[i+iPlus].StartsWith('//* ') then
          // Repeat until no description tags are found
          while sourceTxt[i+iPlus].StartsWith('//* ') do
          begin
            // Handle return description separately to keep the output clean.
            if sourceTxt[i+iPlus].StartsWith('//* Return:') then
              res.ReturnDesc := sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].IndexOf(':') + 2)
            else
              descrTxt.Add(sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].IndexOf('*') + 2));
            Inc(iPlus);
          end;

        // Skip empty or "faulty" lines
        while not (sourceTxt[i+iPlus].StartsWith('procedure') or sourceTxt[i+iPlus].StartsWith('function')) do
          Inc(iPlus);

        // Format procedures
        if sourceTxt[i+iPlus].StartsWith('procedure') then
        begin
          if sourceTxt[i+iPlus].Contains('(') then
          begin
            restStr := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                            sourceTxt[i+iPlus].IndexOf('(') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Name := ReplaceStr(restStr, 'Proc', 'On');
            res.Parameters := ParseParams(Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                   sourceTxt[i+iPlus].IndexOf(')') - (
                                                                   sourceTxt[i+iPlus].IndexOf('(') + 1)), descrTxt);
          end else
          begin
            restStr := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                            sourceTxt[i+iPlus].IndexOf(';') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Name := ReplaceStr(restStr, 'Proc', 'On');
          end;
        end;

        // Format functions
        if sourceTxt[i+iPlus].StartsWith('function') then
        begin
          if sourceTxt[i+iPlus].Contains('(') then
          begin
            restStr := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                            sourceTxt[i+iPlus].IndexOf('(') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Name := ReplaceStr(restStr, 'Func', 'On');
            res.Parameters := ParseParams(Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('(') + 2,
                                                                   sourceTxt[i+iPlus].IndexOf(')') - (
                                                                   sourceTxt[i+iPlus].IndexOf('(') + 1)), descrTxt);
          end else
          begin
            restStr := Copy(sourceTxt[i+iPlus], sourceTxt[i+iPlus].IndexOf('.') + 2,
                            sourceTxt[i+iPlus].IndexOf(':') - (sourceTxt[i+iPlus].IndexOf('.') + 1));
            res.Name := ReplaceStr(restStr, 'Func', 'On');
          end;

          restStr  := sourceTxt[i+iPlus].Substring(sourceTxt[i+iPlus].LastIndexOf(':') + 2);
          res.Return  := restStr.TrimRight([';']);
        end;

        for j := 0 to descrTxt.Count - 1 do
        begin
          res.Description := res.Description + ' ' + descrTxt[j];
        end;

        // Now we have all the parts and can combine them however we like
        aList.Add('| ' + res.Version + ' | ' + res.Name + ' | ' + res.Description +
                  ' | <sub>' + res.Parameters + '</sub> | ' +
                  res.Return + IfThen(res.ReturnDesc <> '', ' //' + res.ReturnDesc) + ' |');
      end;
    end;
  finally
    aList.Add(sLineBreak);
    FreeAndNil(sourceTxt);
    FreeAndNil(descrTxt);
  end;
end;


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  listActions, listEvents, listStates: TStringList;
begin
  txtParserOutput.Lines.Clear;

  if FileExists(edtActionsFile.Text) then
  begin
    listActions := TStringList.Create;
    listActions.Add('####Actions' + sLineBreak);
    listActions.Add('| Ver<br>sion | Action | Description | Parameters<br>and types | Returns |');
    listActions.Add('| ------- | ---- | ----------- | -------------------- | ------- |');
    ParseText(edtActionsFile.Text, listActions);
    txtParserOutput.Lines.AddStrings(listActions);

    if edtOutputFileActions.Text <> '' then
      listActions.SaveToFile(edtOutputFileActions.Text);
    FreeAndNil(listActions);
  end;

  if FileExists(edtEventsFile.Text) then
  begin
    listEvents  := TStringList.Create;
    listEvents.Add('####Events' + sLineBreak);
    listEvents.Add('| Ver<br>sion | Event | Description | Parameters<br>and types |');
    listEvents.Add('| ------- | ---- | ----------- | -------------------- |');
    ParseText(edtEventsFile.Text, listEvents);
    txtParserOutput.Lines.AddStrings(listEvents);

    if edtOutputFileEvents.Text <> '' then
      listEvents.SaveToFile(edtOutputFileEvents.Text);
    FreeAndNil(listEvents);
  end;

  if FileExists(edtStatesFile.Text) then
  begin
    listStates  := TStringList.Create;
    listStates.Add('####States' + sLineBreak);
    listStates.Add('| Ver<br>sion | State | Description | Parameters<br>and types | Returns |');
    listStates.Add('| ------- | ---- | ----------- | -------------------- | ------- |');
    ParseText(edtStatesFile.Text, listStates);
    txtParserOutput.Lines.AddStrings(listStates);

    if edtOutputFileStates.Text <> '' then
      listStates.SaveToFile(edtOutputFileStates.Text);
    FreeAndNil(listStates);
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
