unit TestKM_Game;
interface
uses
  TestFramework, Windows,
  SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils, Math;

type
  TestTKMGame = class(TTestCase)
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
  fGameApp := TKMGameApp.Create(0, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;

procedure TestTKMGame.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
end;

procedure TestTKMGame.TestStone;
var I: Integer; T: Cardinal;
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + 'StoneTest.dat', 'Stone Test');
  Check(fPlayers[0].Stats.GetGoodsProduced = 0);

  T := GetTickCount;

  for I := 1 to 2*60*60*10 do
  begin
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
  end;
  Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  fGameApp.Game.Save('StoneTest');
  Check(fPlayers[0].Stats.GetGoodsProduced >= 3800, 'StoneMining got broken? Mined '+IntToStr(fPlayers[0].Stats.GetGoodsProduced)+'/3800');
end;


procedure TestTKMGame.TestFight95;
var
  I, K: Integer;
  Victory1, Victory2: Integer;
  Best1, Best2: Integer;
begin
  Victory1 := 0;
  Victory2 := 0;
  Best1 := 0;
  Best2 := 0;
  for K := 1 to 50 do
  begin
    fGameApp.NewEmptyMap(128, 128);
    SetKaMSeed(K);
//    fPlayers[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
//    fPlayers[1].AddUnitGroup(ut_Hallebardman, KMPoint(65, 64), dir_W, 8, 24);

    //fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(63, 64), dir_E, 8, 24);
    //fPlayers[1].AddUnitGroup(ut_Cavalry, KMPoint(65, 64), dir_W, 8, 24);

    fPlayers[0].AddUnitGroup(ut_Cavalry, KMPoint(63, 64), dir_E, 8, 24);
    fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

    TKMUnitWarrior(fPlayers[0].Units[0]).OrderAttackUnit(fPlayers[1].Units[0]);

    for I := 1 to 1000 do
    begin
      fGameApp.Game.UpdateGame(nil);
      if fGameApp.Game.IsPaused then
        fGameApp.Game.GameHold(False, gr_Win);
      if fPlayers[0].Stats.GetArmyCount * fPlayers[1].Stats.GetArmyCount = 0 then
        Break;
    end;

    if fPlayers[0].Stats.GetWarriorsKilled > fPlayers[1].Stats.GetWarriorsKilled then
    begin
      Inc(Victory1);
      Best1 := Max(Best1, fPlayers[0].Stats.GetWarriorsKilled - fPlayers[1].Stats.GetWarriorsKilled);
    end
    else
    if fPlayers[0].Stats.GetWarriorsKilled < fPlayers[1].Stats.GetWarriorsKilled then
    begin
      Inc(Victory2);
      Best2 := Max(Best2, fPlayers[1].Stats.GetWarriorsKilled - fPlayers[0].Stats.GetWarriorsKilled);
    end;
    fGameApp.Stop(gr_Silent);
  end;
  Status(Format('Victories %d:%d, Best survivals %d:%d', [Victory1, Victory2, Best1, Best2]));
end;


procedure TestTKMGame.TestAIBuild;
var I: Integer; T: Cardinal;
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + 'AcrossDesert.dat', 'AcrossDesert');

  T := GetTickCount;

  for I := 1 to 60*60*10 do
  begin
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
  end;
  Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  fGameApp.Game.Save('AcrossDesert');
end;


procedure TestTKMGame.Test3Cities;
var I: Integer;
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\Maps\Test by Rayzel\Test by Rayzel.dat', 'Test by Rayzel');

  for I := 1 to 60*60*10 do
  begin
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
  end;

  fGameApp.Game.Save('Test by Rayzel');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest('Functional', TestTKMGame.Suite);
end.

