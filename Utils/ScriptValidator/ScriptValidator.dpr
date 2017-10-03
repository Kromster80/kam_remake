program ScriptValidator;
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Forms,
  KM_Defaults,
  Unit1 in 'Unit1.pas' {Form1},
  ValidatorTypes in 'ValidatorTypes.pas',
  ConsoleMain in 'ConsoleMain.pas';
{$R *.res}

var
  fConsoleMain: TConsoleMain;
  fParamRecord: TCLIParamRecord;
  fArgs:        string;


function IsConsoleMode: Boolean;
var
  SI: TStartupInfo;
begin
  SI.cb := SizeOf(StartUpInfo);
  GetStartupInfo(SI);
  Result := (SI.dwFlags and STARTF_USESHOWWINDOW) = 0;
end;


procedure ProcessParams;
var
  I: Integer;
begin
  if ParamCount = 0 then
  begin
    fParamRecord.Help := True;
    Exit;
  end;

  for I := 1 to ParamCount do // Skip 0, as this is the EXE-path
  begin
    fArgs := fArgs + ' ' + paramstr(I) + sLineBreak;

    if (paramstr(I) = '-h') or (paramstr(I) = '-help') then
    begin
      fParamRecord.Help := True;
      continue;
    end;

    if (paramstr(I) = '-a') or (paramstr(I) = '-all') then
    begin
      fParamRecord.AllMaps := True;
      continue;
    end;

    if (paramstr(I) = '-c') or (paramstr(I) = '-campaign') then
    begin
      fParamRecord.Campaign := True;
      continue;
    end;

    if (paramstr(I) = '-g') or (paramstr(I) = '-graphic') then
    begin
      fParamRecord.GraphicMode := True;
      continue;
    end;

    if (paramstr(I) = '-v') or (paramstr(I) = '-verbose') then
    begin
      fParamRecord.Verbose := True;
      continue;
    end;

    if (paramstr(I) = '-V') or (paramstr(I) = '-version') then
    begin
      fParamRecord.Version := True;
      continue;
    end;

    // Only allow one script file
    if fParamRecord.ScriptFile = '' then
      fParamRecord.ScriptFile := paramstr(I);
  end;
end;


begin
  if not IsConsoleMode then
  begin
    FreeConsole; // Used to hide the console
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
  end else
  begin
    try
      writeln(VALIDATOR_START_TEXT);
      ProcessParams;
      fConsoleMain := TConsoleMain.Create;

      if fParamRecord.Verbose then
        writeln('VERBOSE: Arguments:' + sLinebreak + fArgs);

      if fParamRecord.Version then
      begin
        writeln('Game version: ' + GAME_REVISION + sLineBreak +
                'Validator version: ' + VALIDATOR_VERSION + sLineBreak);
      end;

      // Always exit after showing help.
      if fParamRecord.Help then
      begin
        fConsoleMain.ShowHelp;
        Exit;
      end;

      if fParamRecord.GraphicMode then
      begin
        Application.Initialize;
        Application.MainFormOnTaskbar := True;
        Application.CreateForm(TForm1, Form1);
        Application.Run;
      end else
        fConsoleMain.Start(fParamRecord);
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;

    if fConsoleMain <> nil then
      FreeAndNil(fConsoleMain);
  end;

end.
