program ScriptValidatorCLI;
{$APPTYPE CONSOLE}
{$R *.res}

uses
  SysUtils,
  KM_Defaults,
  Main in 'src\Main.pas',
  SVCLI_Types in 'src\SVCLI_Types.pas';

var
  fMain:        TMain;
  fParamRecord: TCLIParamRecord;
  I:            Integer;
  fArgs:        string;

begin
  try
    writeln(sLineBreak + '  KaM Remake Script Validator' + sLineBreak);
    fMain := TMain.Create;

    if ParamCount = 0 then
    begin
      fMain.ShowHelp;
      Exit;
    end;

    fArgs := 'Arguments:';

    for I := 1 to ParamCount do // Skip 0, as this is the EXE-path
    begin
      fArgs := fArgs + ' ' + paramstr(I);

      if (paramstr(I) = '-h') or (paramstr(I) = '-help') then
      begin
        fParamRecord.Help := True;
        Continue;
      end else if (paramstr(I) = '-c') or (paramstr(I) = '-campaign') then
      begin
        fParamRecord.Campaign := True;
        Continue;
      end else if (paramstr(I) = '-v') or (paramstr(I) = '-verbose') then
      begin
        fParamRecord.Verbose := True;
        Continue;
      end else if (paramstr(I) = '-V') or (paramstr(I) = '-version') then
      begin
        fParamRecord.Version := True;
        Continue;
      end else if fParamRecord.ScriptFile = '' then
        fParamRecord.ScriptFile := paramstr(I);
    end;

    if fParamRecord.Verbose then
      writeln('VERBOSE: ' + fArgs + sLineBreak);

    if fParamRecord.Version then
    begin
      writeln('  Game version: ' + GAME_REVISION);
      writeln('  Validator version: ' + VALIDATOR_VERSION + sLineBreak);
    end;

    if fParamRecord.Help then
    begin
      fMain.ShowHelp;
      Exit;
    end;

    fMain.Start(fParamRecord);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  if fMain <> nil then
    FreeAndNil(fMain);
end.
