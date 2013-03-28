unit Unit_Runner;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_TextLibrary, KM_Utils;


type
  TKMRunnerCommon = class;
  TKMRunnerClass = class of TKMRunnerCommon;

  TKMRunResults = record
    ChartsCount: Integer; //How many charts return
    ValueCount: Integer; //How many values
    ValueMin, ValueMax: Integer;
    Value: array {Run} of array {Value} of Integer;
    TimesCount: Integer;
    TimeMin, TimeMax: Integer;
    Times: array {Run} of array {Tick} of Cardinal;
  end;

  TKMRunnerCommon = class
  protected
    fRun: Integer;
    fResults: TKMRunResults;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    procedure Execute(aRun: Integer); virtual; abstract;
    procedure SimulateGame;
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

  fResults.ChartsCount := aCount;
  SetLength(fResults.Value, fResults.ChartsCount, fResults.ValueCount);
  SetLength(fResults.Times, fResults.ChartsCount, fResults.TimesCount);

  for I := 0 to aCount - 1 do
  begin
    if Assigned(OnProgress) then
      OnProgress(Format('%d', [I]));

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
  if ValueCount > 0 then
  begin
    ValueMin := Value[0,0];
    ValueMax := Value[0,0];
    for I := 0 to ChartsCount - 1 do
    for K := 0 to ValueCount - 1 do
    begin
      ValueMin := Min(ValueMin, Value[I,K]);
      ValueMax := Max(ValueMax, Value[I,K]);
    end;
  end;
  //Get min max
  with fResults do
  if TimesCount > 0 then
  begin
    TimeMin := Times[0,0];
    TimeMax := Times[0,0];
    for I := 0 to ChartsCount - 1 do
    for K := 0 to TimesCount - 1 do
    begin
      TimeMin := Min(TimeMin, Times[I,K]);
      TimeMax := Max(TimeMax, Times[I,K]);
    end;
  end;
end;


procedure TKMRunnerCommon.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\..\';
  //fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');
  fGameApp := TKMGameApp.Create(nil, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;


procedure TKMRunnerCommon.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fLog);
end;


procedure TKMRunnerCommon.SimulateGame;
var I: Integer;
begin
  for I := 0 to fResults.TimesCount - 1 do
  begin
    fResults.Times[fRun, I] := TimeGet;

    fGameApp.Game.UpdateGame(nil);

    fResults.Times[fRun, I] := TimeGet - fResults.Times[fRun, I];

    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);

    if (I mod 60*10 = 0) and Assigned(OnProgress) then
      OnProgress(Format('%d (%d min)', [fRun, I div 600]));
  end;
end;


end.
