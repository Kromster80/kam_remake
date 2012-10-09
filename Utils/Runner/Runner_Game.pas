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
var I: Integer; T: Cardinal;
begin
  //Total amount of stones = 4140
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\StoneMines\StoneMines.dat', 'StoneMines');

  SetKaMSeed(aRun+1);

  T := GetTickCount;

  for I := 1 to 2*60*60*10 do
  begin
    Application.ProcessMessages;
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
  end;
  //Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  //fGameApp.Game.Save('StoneTest');
//  Check(fPlayers[0].Stats.GetGoodsProduced >= 4140 * 0.9, 'StoneMining got broken? Mined '+IntToStr(fPlayers[0].Stats.GetGoodsProduced)+'/3800');
  Result.Value := fPlayers[0].Stats.GetGoodsProduced;
end;


function TKMRunnerFight95.Execute(aRun: Integer): TKMRunResult;
var
  I, K: Integer;
  Victory1, Victory2: Integer;
  Best1, Best2, Worst1, Worst2: Integer;
begin
  Victory1 := 0;
  Victory2 := 0;
  Best1 := 0;
  Best2 := 0;
  Worst1 := 100;
  Worst2 := 100;
  for K := 1 to 50 do
  begin
    fGameApp.NewEmptyMap(128, 128);
    SetKaMSeed(K);
    //fPlayers[0].AddUnitGroup(ut_Cavalry, KMPoint(63, 64), dir_E, 8, 24);
    //fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

    fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
    fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(65, 64), dir_W, 8, 24);

    //fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(63, 64), dir_E, 8, 24);
    //fPlayers[1].AddUnitGroup(ut_Cavalry, KMPoint(65, 64), dir_W, 8, 24);

    TKMUnitWarrior(fPlayers[1].Units[0]).OrderAttackUnit(fPlayers[0].Units[0]);

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
      Worst1 := Min(Worst1, fPlayers[0].Stats.GetWarriorsKilled - fPlayers[1].Stats.GetWarriorsKilled);
    end
    else
    if fPlayers[0].Stats.GetWarriorsKilled < fPlayers[1].Stats.GetWarriorsKilled then
    begin
      Inc(Victory2);
      Best2 := Max(Best2, fPlayers[1].Stats.GetWarriorsKilled - fPlayers[0].Stats.GetWarriorsKilled);
      Worst2 := Min(Worst2, fPlayers[1].Stats.GetWarriorsKilled - fPlayers[0].Stats.GetWarriorsKilled);
    end;
    fGameApp.Stop(gr_Silent);
  end;
  //Status(Format('Victories %d:%d, Best survivals %d-%d:%d-%d', [Victory1, Victory2, Worst1, Best1, Worst2, Best2]));
end;


function TKMRunnerAIBuild.Execute(aRun: Integer): TKMRunResult;
var I: Integer; T: Cardinal;
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + 'AcrossDesert.dat', 'AcrossDesert');

  T := GetTickCount;

  for I := 1 to 2*60*60*10 do
  begin
    fGameApp.Game.UpdateGame(nil);
    if fGameApp.Game.IsPaused then
      fGameApp.Game.GameHold(False, gr_Win);
  end;
  //Status('Done in ' + IntToStr(GetTickCount - T) + ' ms');

  fGameApp.Game.Save('AcrossDesert');
end;


initialization
  RegisterRunner(TKMRunnerStone);
  RegisterRunner(TKMRunnerFight95);
  RegisterRunner(TKMRunnerAIBuild);


end.

