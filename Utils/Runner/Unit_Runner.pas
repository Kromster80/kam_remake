unit Unit_Runner;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_TextLibrary, KM_Utils;


const
  MAX_VALUES = 8;

type
  TKMRunnerCommon = class;
  TKMRunnerClass = class of TKMRunnerCommon;

  TKMRunResults = record
    RunCount: Integer; //How many runs were logged
    ValCount: Integer; //How many values each run has
    ValueMin, ValueMax: array of Single;
    Value: array of array [0..MAX_VALUES - 1] of Single;
  end;

  TKMRunnerCommon = class
  protected
    fRun: Integer;
    fResults: TKMRunResults;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    procedure Execute(aRun: Integer); virtual; abstract;
    procedure SimulateGame(aTicks: Integer);
    procedure ProcessRunResults;
  public
    OnProgress: TStringEvent;
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
  SetUp;

  fResults.RunCount := aCount;
  SetLength(fResults.Value, aCount);
  SetLength(fResults.ValueMin, aCount);
  SetLength(fResults.ValueMax, aCount);

  for I := 0 to aCount - 1 do
  begin
    fRun := I;
    Execute(I);
  end;

  TearDown;

  ProcessRunResults;
  Result := fResults;
end;


procedure TKMRunnerCommon.ProcessRunResults;
var
  I, K: Integer;
begin
  //Get min max
  with fResults do
  for I := 0 to ValCount - 1 do
  begin
    ValueMin[I] := Value[0,I];
    ValueMax[I] := Value[0,I];
    for K := 1 to RunCount - 1 do
    begin
      ValueMin[I] := Min(ValueMin[I], Value[K,I]);
      ValueMax[I] := Max(ValueMax[I], Value[K,I]);
    end;
  end;
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


procedure TKMRunnerCommon.SimulateGame(aTicks: Integer);
var I: Integer;
begin
  for I := 0 to aTicks - 1 do
  begin
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
    if (I mod 60*10 = 0) and Assigned(OnProgress) then
    begin
      OnProgress(Format('%d (%d min)', [fRun, I div 600]));
    end;
  end;
end;


end.
