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
    function Execute(aRun: Integer): TKMRunResult; override;
  end;

  TKMRunnerFight95 = class(TKMRunnerCommon)
  protected
    function Execute(aRun: Integer): TKMRunResult; override;
  end;

  TKMRunnerAIBuild = class(TKMRunnerCommon)
  protected
    function Execute(aRun: Integer): TKMRunResult; override;
  end;


implementation


function TKMRunnerStone.Execute(aRun: Integer): TKMRunResult;
begin
  DYNAMIC_TERRAIN := False;
  //Total amount of stones = 4140
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\StoneMines\StoneMines.dat', 'StoneMines');

  SetKaMSeed(aRun+1);

  SimulateGame(2*60*60*10);

  //fGameApp.Game.Save('StoneTest');
//  Check(fPlayers[0].Stats.GetGoodsProduced >= 4140 * 0.9, 'StoneMining got broken? Mined '+IntToStr(fPlayers[0].Stats.GetGoodsProduced)+'/3800');
  Result.Value := fPlayers[0].Stats.GetGoodsProduced;

  fGameApp.Stop(gr_Silent);
end;


function TKMRunnerFight95.Execute(aRun: Integer): TKMRunResult;
var
  I, K: Integer;
begin
  DYNAMIC_TERRAIN := False;
  fGameApp.NewEmptyMap(128, 128);
  SetKaMSeed(aRun + 1);

  //fPlayers[0].AddUnitGroup(ut_Cavalry, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

  fPlayers[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
  fPlayers[1].AddUnitGroup(ut_Hallebardman, KMPoint(65, 64), dir_W, 8, 24);

  //fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Cavalry, KMPoint(65, 64), dir_W, 8, 24);

  TKMUnitWarrior(fPlayers[1].Units[0]).OrderAttackUnit(fPlayers[0].Units[0]);

  SimulateGame(1000);

  Result.Value := fPlayers[0].Stats.GetUnitQty(ut_Any);// - fPlayers[1].Stats.GetWarriorsKilled;

  fGameApp.Stop(gr_Silent);
end;


function TKMRunnerAIBuild.Execute(aRun: Integer): TKMRunResult;
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\MapsMP\Across the Desert\Across the Desert.dat', 'Across the Desert');

  fPlayers.RemovePlayer(5);
  fPlayers.RemovePlayer(4);
  fPlayers.RemovePlayer(3);
  fPlayers.RemovePlayer(2);
  fPlayers.RemovePlayer(0);
  MyPlayer := fPlayers[0];

  SetKaMSeed(aRun + 1);

  SimulateGame(1*60*60*10);

  Result.Value := fPlayers[0].Stats.GetResourceQty(rt_All);// - fPlayers[1].Stats.GetWarriorsKilled;

  fGameApp.Stop(gr_Silent);
end;


initialization
  RegisterRunner(TKMRunnerStone);
  RegisterRunner(TKMRunnerFight95);
  RegisterRunner(TKMRunnerAIBuild);


end.

