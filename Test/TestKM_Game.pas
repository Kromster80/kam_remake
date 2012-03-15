unit TestKM_Game;
interface
uses
  TestFramework,
  SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_Game, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, Math;

type
  // Test methods for class TKMCampaign
  TestTKMGame = class(TTestCase)
  strict private
    //FKMGame: TKMGame;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStone;
  end;

implementation

procedure TestTKMGame.SetUp;
begin
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\log.tmp');
  fLocales := TKMLocales.Create;
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGame := TKMGame.Create(0, 1024, 768, False, nil, nil, True);
end;

procedure TestTKMGame.TearDown;
begin
  fGame.Stop(gr_Silent);
  FreeAndNil(fGame);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
end;

procedure TestTKMGame.TestStone;
var I, K: Integer;
begin
  fGame.StartSingleMap(ExtractFilePath(ParamStr(0)) + 'StoneTest.dat', 'Stone Test');
  Check(fPlayers[0].Stats.GetGoodsProduced = 0);

  for I := 1 to 10000 do
  begin
    fGame.UpdateState;
    if fGame.GameState = gsOnHold then
      fGame.GameHold(False, gr_Win);
  end;

  Check(fPlayers[0].Stats.GetGoodsProduced > 0, 'StoneMining got broken');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest('Functional', TestTKMGame.Suite);
end.

