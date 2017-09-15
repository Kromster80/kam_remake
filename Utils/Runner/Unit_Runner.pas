unit Unit_Runner;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KromUtils,
  KM_GameApp, KM_ResLocales, KM_Log, KM_ResTexts, KM_CommonUtils, KM_RenderControl;


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
    fRenderTarget: TKMRenderControl;
    fRun: Integer;
    fResults: TKMRunResults;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    procedure Execute(aRun: Integer); virtual; abstract;
    procedure SimulateGame;
    procedure ProcessRunResults;
  public
    Duration: Integer;
    OnProgress: TUnicodeStringEvent;
    constructor Create(aRenderTarget: TKMRenderControl); reintroduce;
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
constructor TKMRunnerCommon.Create(aRenderTarget: TKMRenderControl);
begin
  inherited Create;

  fRenderTarget := aRenderTarget;
end;


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
var
  tgtWidth, tgtHeight: Word;
begin
  SKIP_RENDER := (fRenderTarget = nil);
  SKIP_SOUND := True;
  SKIP_LOADING_CURSOR := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\..\';
  //gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');

  fResults.TimesCount := Duration*60*10;

  if fRenderTarget = nil then
  begin
    tgtWidth := 1024;
    tgtHeight := 768;
  end else
  begin
    tgtWidth := fRenderTarget.Width;
    tgtHeight := fRenderTarget.Height;
  end;

  gGameApp := TKMGameApp.Create(fRenderTarget, tgtWidth, tgtHeight, False, nil, nil, nil, True);
  gGameApp.GameSettings.Autosave := False;
end;


procedure TKMRunnerCommon.TearDown;
begin
  gGameApp.Stop(gr_Silent);
  FreeAndNil(gGameApp);
  FreeAndNil(gLog);
  if Assigned(OnProgress) then
    OnProgress('Done');

end;


procedure TKMRunnerCommon.SimulateGame;
var I: Integer;
begin
  for I := 0 to fResults.TimesCount - 1 do
  begin
    fResults.Times[fRun, I] := TimeGet;

    gGameApp.Game.UpdateGame(nil);
    gGameApp.Render(False);

    fResults.Times[fRun, I] := TimeGet - fResults.Times[fRun, I];

    if gGameApp.Game.IsPaused then
      gGameApp.Game.GameHold(False, gr_Win);

    if (I mod 60*10 = 0) and Assigned(OnProgress) then
      OnProgress(Format('%d (%d min)', [fRun + 1, I div 600]));
  end;
end;


end.
