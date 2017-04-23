program ScriptValidator;

uses
  SysUtils,
  Forms,
  KM_Defaults,
  Unit1 in 'Unit1.pas' {Form1},
  ValidatorTypes in 'ValidatorTypes.pas',
  ConsoleMain in 'ConsoleMain.pas';

{$APPTYPE CONSOLE}
{$R *.res}
var
  fConsoleMain: TConsoleMain;
  fParamRecord: TCLIParamRecord;
  I:            Integer;
  fArgs:        string;

begin
  if ParamCount >= 1 then
  begin
    try
      writeln(sLineBreak + 'KaM Remake Script Validator' + sLineBreak);
      fConsoleMain := TConsoleMain.Create;

      if ParamCount = 0 then
      begin
        fConsoleMain.ShowHelp;
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
        writeln('Game version: ' + GAME_REVISION);
        writeln('Validator version: ' + VALIDATOR_VERSION + sLineBreak);
      end;

      if fParamRecord.Help then
      begin
        fConsoleMain.ShowHelp;
        Exit;
      end;

      fConsoleMain.Start(fParamRecord);
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;

    if fConsoleMain <> nil then
      FreeAndNil(fConsoleMain);
  end else
  begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
  end;
end.
