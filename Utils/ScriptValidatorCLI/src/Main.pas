unit Main;

interface
uses
  SysUtils,
  SVCLI_Types,
  KM_Defaults, KM_Scripting;

type
  TMain = class(TObject)
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Start(aParameterRecord: TCLIParamRecord);
    procedure ShowHelp;
  private
    fScripting: TKMScripting;
  end;

const
  VALIDATOR_VERSION_MAJOR = '1';
  VALIDATOR_VERSION_MINOR = '00';
  VALIDATOR_VERSION       = VALIDATOR_VERSION_MAJOR + '.' +
                            VALIDATOR_VERSION_MINOR + ' - ' +
                            GAME_REVISION;

implementation

{ TMain }
constructor TMain.Create;
begin
  fScripting := TKMScripting.Create(nil);
end;


destructor TMain.Destroy;
begin
  FreeAndNil(fScripting);
end;


procedure TMain.Start(aParameterRecord: TCLIParamRecord);
var
  CampaignFile,
  OutputText:   string;
begin
  if aParameterRecord.Verbose then
    writeln('VERBOSE: Checking if script file exists.' + sLineBreak);

  if not FileExists(aParameterRecord.ScriptFile)then
  begin
    writeln('  File not found: ' + aParameterRecord.ScriptFile);
    Exit;
  end else if aParameterRecord.Verbose then
    writeln('VERBOSE: Found script file.' + sLineBreak);

  if aParameterRecord.Campaign then
  begin
    if aParameterRecord.Verbose then
      writeln('VERBOSE: Checking if campaign script file exists.' + sLineBreak);

    CampaignFile := ExtractFilePath(aParameterRecord.ScriptFile) + '..\campaigndata.script';

    if not FileExists(CampaignFile)then
    begin
      writeln('  Campaign file not found: ' + CampaignFile);
      CampaignFile := '';
    end else if aParameterRecord.Verbose then
      writeln('VERBOSE: Found campaign script file.' + sLineBreak);
  end;

  if aParameterRecord.Verbose then
    writeln('VERBOSE: Loading script file(s) into dynamic script compiler.' + sLineBreak);

  fScripting.LoadFromFile(aParameterRecord.ScriptFile, CampaignFile, nil);

  if fScripting.ErrorString <> '' then
  begin
    OutputText := OutputText + '  Errors:' + sLineBreak;
    OutputText := OutputText + '    ' + StringReplace(fScripting.ErrorString,
                                                      '|', sLineBreak + '    ',
                                                      [rfReplaceAll]);
  end;

  if fScripting.WarningsString <> '' then
  begin
    OutputText := OutputText + '  Warnings:' + sLineBreak;
    OutputText := OutputText + '    ' + StringReplace(fScripting.WarningsString,
                                                      '|', sLineBreak + '    ',
                                                      [rfReplaceAll]);
  end;

  if OutputText <> '' then
    writeln(OutputText)
  else
    writeln('  ' + aParameterRecord.ScriptFile + ' - No errors :)' + sLineBreak);
end;


procedure TMain.ShowHelp;
begin
  writeln('||===============================================================================||');
  writeln('||                                                                               ||');
  writeln('||  Script Validator CLI has a few options.                                      ||');
  writeln('||  Below we will show these options and give a brief explanation what they do.  ||');
  writeln('||                                                                               ||');
  writeln('||===============================================================================||');
  writeln('||                                                                               ||');
  writeln('||  Usage:                                                                       ||');
  writeln('||    ScriptValidatorCLI.exe [OPTIONS] FileName.ext                              ||');
  writeln('||                                                                               ||');
  writeln('||  Options:                                                                     ||');
  writeln('||    -h / -help        - Will show this menu                                    ||');
  writeln('||    -c / -campaign    - Check a campaign type script                           ||');
  writeln('||    -v / -verbose     - Show verbose messages                                  ||');
  writeln('||    -V / -version     - Show the script validator version                      ||');
  writeln('||                                                                               ||');
  writeln('||===============================================================================||');
  writeln('||                                                                               ||');
  writeln('||  Credits:                                                                     ||');
  writeln('||    Programming > Krom                                                         ||');
  writeln('||                > Lewin                                                        ||');
  writeln('||                                                                               ||');
  writeln('||    Additional programming > Alex                                              ||');
  writeln('||                           > Andreus                                           ||');
  writeln('||                           > Rey                                               ||');
  writeln('||                           > Danjb                                             ||');
  writeln('||                           > Thimo                                             ||');
  writeln('||                                                                               ||');
  writeln('||===============================================================================||');
  writeln('');
end;

end.
