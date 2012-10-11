unit Runner_Game;
{$I KaM_Remake.inc}
interface
uses
  Forms, Unit_Runner, Windows,
  SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils, Math;


type
  TKMRunnerStone = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
  end;

  TKMRunnerFight95 = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
  end;

  TKMRunnerAIBuild = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
  end;


implementation


procedure TKMRunnerStone.SetUp;
begin
  inherited;
  fResults.ValCount := 1;

  AI_GEN_INFLUENCE_MAPS := False;
  AI_GEN_NAVMESH := False;
  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerStone.Execute(aRun: Integer);
begin
  //Total amount of stones = 4140
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\StoneMines\StoneMines.dat', 'StoneMines');

  SetKaMSeed(aRun+1);

  SimulateGame(2*60*60*10);

  fResults.Value[aRun, 0] := fPlayers[0].Stats.GetGoodsProduced;

  //fGameApp.Game.Save('StoneTest');
  fGameApp.Stop(gr_Silent);
end;


procedure TKMRunnerFight95.SetUp;
begin
  inherited;
  fResults.ValCount := 2;

  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerFight95.Execute(aRun: Integer);
begin
  fGameApp.NewEmptyMap(128, 128);
  SetKaMSeed(aRun + 1);

  //fPlayers[0].AddUnitGroup(ut_Cavalry, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

  //fPlayers[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Hallebardman, KMPoint(65, 64), dir_W, 8, 24);

  //fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Cavalry, KMPoint(65, 64), dir_W, 8, 24);

  fPlayers[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
  fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

  TKMUnitWarrior(fPlayers[1].Units[0]).OrderAttackUnit(fPlayers[0].Units[0]);

  SimulateGame(1200);

  fResults.Value[aRun, 0] := fPlayers[0].Stats.GetUnitQty(ut_Any);
  fResults.Value[aRun, 1] := fPlayers[1].Stats.GetUnitQty(ut_Any);

  fGameApp.Stop(gr_Silent);
end;


procedure TKMRunnerAIBuild.SetUp;
begin
  inherited;
  fResults.ValCount := 5;
end;


procedure TKMRunnerAIBuild.Execute(aRun: Integer);
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\MapsMP\Across the Desert\Across the Desert.dat', 'Across the Desert');

  fPlayers.RemovePlayer(0);
  MyPlayer := fPlayers[0];

  SetKaMSeed(aRun + 1);

  SimulateGame(1*60*60*10);

  fResults.Value[aRun, 0] := fPlayers[0].Stats.GetResourceQty(rt_All);
  fResults.Value[aRun, 1] := fPlayers[1].Stats.GetResourceQty(rt_All);
  fResults.Value[aRun, 2] := fPlayers[2].Stats.GetResourceQty(rt_All);
  fResults.Value[aRun, 3] := fPlayers[3].Stats.GetResourceQty(rt_All);
  fResults.Value[aRun, 4] := fPlayers[4].Stats.GetResourceQty(rt_All);

  fGameApp.Stop(gr_Silent);
end;


initialization
  RegisterRunner(TKMRunnerStone);
  RegisterRunner(TKMRunnerFight95);
  RegisterRunner(TKMRunnerAIBuild);


end.

