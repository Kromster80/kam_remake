unit Unit_Runner;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_TextLibrary, KM_Utils;


type
  TKMRunnerCommon = class;
  TKMRunnerClass = class of TKMRunnerCommon;

  TKMRunResult = record
    Value: Integer;
  end;

  TKMRunResults = array of TKMRunResult;

  TKMRunnerCommon = class
  protected
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    function Execute(aRun: Integer): TKMRunResult; virtual; abstract;
  public
    function Run(aCount: Integer): TKMRunResults;
  end;

  procedure RegisterRunner(aRunner: TKMRunnerClass);

var
  RunnerList: array of TKMRunnerClass;

implementation


procedure RegisterRunner(aRunner: TKMRunnerClass);
begin
  SetLength(RunnerList, Length(RunnerList) + 1);
  RunnerList[High(RunnerList)] := aRunner;
end;


{ TKMRunnerCommon }
function TKMRunnerCommon.Run(aCount: Integer): TKMRunResults;
var
  I: Integer;
begin
  SetLength(Result, aCount);

  SetUp;

  for I := 0 to aCount - 1 do
    Result[I] := Execute(I);

  TearDown;
end;


procedure TKMRunnerCommon.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\..\';
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGameApp := TKMGameApp.Create(0, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;


procedure TKMRunnerCommon.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
end;


end.
