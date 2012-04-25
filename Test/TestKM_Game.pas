unit TestKM_Game;
interface
uses
  TestFramework, Windows,
  SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_Game, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils, Math;

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
    procedure TestFight95;
    procedure TestAIBuild;
    procedure Test3Cities;
  end;

implementation

procedure TestTKMGame.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGame := TKMGame.Create(0, 1024, 768, False, nil, nil, True);
  fGame.GlobalSettings.Autosave := False;
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
var I: Integer; T: Cardinal;
begin
  fGame.StartSingleMap(ExtractFilePath(ParamStr(0)) + 'StoneTest.dat', 'Stone Test');
  Check(fPlayers[0].Stats.GetGoodsProduced = 0);

  T := GetTickCount;

  for I := 1 to 2*60*60*10 do
  begin
    fGame.UpdateGame(nil);
    if fGame.GameState = gsOnHold then
      fGame.GameHold(False, gr_Win);
  end;
  Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  fGame.Save('StoneTest');
  Check(fPlayers[0].Stats.GetGoodsProduced >= 3800, 'StoneMining got broken? Mined '+IntToStr(fPlayers[0].Stats.GetGoodsProduced)+'/3800');
end;


procedure TestTKMGame.TestFight95;
var I, K: Integer; P1, P2: Integer;
begin
  P1 := 0;
  P2 := 0;
  for K := 1 to 50 do
  begin
    fGame.StartSingleMap(ExtractFilePath(ParamStr(0)) + 'FightTest95.dat', 'Fight Test');

    SetKaMSeed(K);

    TKMUnitWarrior(fPlayers[0].Units[0]).OrderAttackUnit(fPlayers[1].Units[0]);

    for I := 1 to 1000 do
    begin
      fGame.UpdateGame(nil);
      if fGame.GameState = gsOnHold then
        fGame.GameHold(False, gr_Win);
    end;

    if fPlayers[0].Stats.GetWarriorsKilled > fPlayers[1].Stats.GetWarriorsKilled then
      Inc(P1)
    else
      Inc(P2);
    fGame.Stop(gr_Silent);
  end;
  Check(False, IntToStr(P1)+':'+IntToStr(P2)+'Game is unfair?');
end;


procedure TestTKMGame.TestAIBuild;
var I: Integer; T: Cardinal;
begin
  fGame.StartSingleMap(ExtractFilePath(ParamStr(0)) + 'AcrossDesert.dat', 'AcrossDesert');

  T := GetTickCount;

  for I := 1 to 60*60*10 do
  begin
    fGame.UpdateGame(nil);
    if fGame.GameState = gsOnHold then
      fGame.GameHold(False, gr_Win);
  end;
  Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  fGame.Save('AcrossDesert');
end;


procedure TestTKMGame.Test3Cities;
var I: Integer;
begin
  fGame.StartSingleMap(ExtractFilePath(ParamStr(0)) + '..\Maps\Test by Rayzel\Test by Rayzel.dat', 'Test by Rayzel');

  for I := 1 to 60*60*10 do
  begin
    fGame.UpdateGame(nil);
    if fGame.GameState = gsOnHold then
      fGame.GameHold(False, gr_Win);
  end;

  fGame.Save('Test by Rayzel');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest('Functional', TestTKMGame.Suite);
end.

