unit Runner_Game;
{$I KaM_Remake.inc}
interface
uses
  Forms, Unit_Runner, Windows,
  SysUtils, KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils, Math;


type
  //Typical usage:
  //SetUp, Execute(1), Execute(2) .. Execute(N), TearDown

  TKMRunnerStone = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMRunnerFight95 = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMRunnerAIBuild = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMVortamicPF = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;


implementation


procedure TKMRunnerStone.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
  fResults.TimesCount := 0;

  AI_GEN_INFLUENCE_MAPS := False;
  AI_GEN_NAVMESH := False;
  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerStone.TearDown;
begin
  inherited;
  AI_GEN_INFLUENCE_MAPS := True;
  AI_GEN_NAVMESH := True;
  DYNAMIC_TERRAIN := True;
end;


procedure TKMRunnerStone.Execute(aRun: Integer);
var
  I,K: Integer;
  L: TKMPointList;
  P: TKMPoint;
begin
  //Total amount of stone = 4140
  fTerrain := TKMTerrain.Create;
  fTerrain.LoadFromFile(ExeDir + 'Maps\StoneMines\StoneMines.map', False);

  SetKaMSeed(aRun+1);

  //Stonemining is done programmatically, by iterating through all stone tiles
  //and mining them if conditions are right (like Stonemasons would do)

  L := TKMPointList.Create;
  for I := 1 to fTerrain.MapY - 2 do
  for K := 1 to fTerrain.MapX - 1 do
  if fTerrain.TileIsStone(K,I) > 0 then
    L.AddEntry(KMPoint(K,I));

  I := 0;
  fResults.Value[aRun, 0] := 0;
  repeat
    L.GetRandom(P);

    if fTerrain.TileIsStone(P.X,P.Y) > 0 then
    begin
      if fTerrain.CheckPassability(KMPointBelow(P), CanWalk) then
      begin
        fTerrain.DecStoneDeposit(P);
        fResults.Value[aRun, 0] := fResults.Value[aRun, 0] + 3;
        I := 0;
      end;
    end
    else
      L.RemoveEntry(P);

    Inc(I);
    if I > 200 then
      Break;
  until (L.Count = 0);

  FreeAndNil(fTerrain);
end;


procedure TKMRunnerFight95.SetUp;
begin
  inherited;
  fResults.ValueCount := 2;
  fResults.TimesCount := 2*60*10;

  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerFight95.TearDown;
begin
  inherited;
  DYNAMIC_TERRAIN := True;
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

  fPlayers[1].UnitGroups[0].OrderAttackUnit(fPlayers[0].Units[0], True);

  SimulateGame;

  fResults.Value[aRun, 0] := fPlayers[0].Stats.GetUnitQty(ut_Any);
  fResults.Value[aRun, 1] := fPlayers[1].Stats.GetUnitQty(ut_Any);

  fGameApp.Stop(gr_Silent);
end;


{ TKMRunnerAIBuild }
procedure TKMRunnerAIBuild.SetUp;
begin
  inherited;
  fResults.ValueCount := 5;
  fResults.TimesCount := 60*60*10;
end;


procedure TKMRunnerAIBuild.TearDown;
begin
  inherited;
  //
end;


procedure TKMRunnerAIBuild.Execute(aRun: Integer);
begin
  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\AcrossDesert\AcrossDesert.dat', 'Across the Desert');

  fPlayers.RemovePlayer(0);
  MyPlayer := fPlayers[0];

  SetKaMSeed(aRun + 1);

  SimulateGame;

  fGameApp.Game.Save('AI Build #' + IntToStr(aRun));

  fResults.Value[aRun, 0] := fPlayers[0].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 1] := fPlayers[1].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 2] := fPlayers[2].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 3] := fPlayers[3].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 4] := fPlayers[4].Stats.GetWarriorsTrained;

  {fResults.Value[aRun, 0] := fPlayers[0].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 1] := fPlayers[1].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 2] := fPlayers[2].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 3] := fPlayers[3].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 4] := fPlayers[4].Stats.GetGoodsProduced(rt_Stone);}

  {fResults.Value[aRun, 0] := fPlayers[0].Stats.GetHousesBuilt;
  fResults.Value[aRun, 1] := fPlayers[0].Stats.GetHousesBuilt;
  fResults.Value[aRun, 2] := fPlayers[0].Stats.GetHousesBuilt;
  fResults.Value[aRun, 3] := fPlayers[0].Stats.GetHousesBuilt;
  fResults.Value[aRun, 4] := fPlayers[0].Stats.GetHousesBuilt;}

  fGameApp.Stop(gr_Silent);
end;


{ TKMVortamicPF }
procedure TKMVortamicPF.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
  fResults.TimesCount := 5*60*10;
end;

procedure TKMVortamicPF.TearDown;
begin
  inherited;

end;

procedure TKMVortamicPF.Execute(aRun: Integer);
var
  T: Cardinal;
begin
  inherited;

  //Intended to be run multiple of 4 times to compare different PF algorithms
//  PathFinderToUse := (aRun mod 4) div 2; //01230123 > 00110011
//  CACHE_PATHFINDING := Boolean(aRun mod 2);  //0101

  fGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\Vortamic\Vortamic.dat', 'Across the Desert');

  SetKaMSeed(aRun div 4 + 1); //11112222

  T := TimeGet;
  SimulateGame;
  fResults.Value[aRun, 0] := TimeGet - T;

  fGameApp.Stop(gr_Silent);
end;

initialization
  RegisterRunner(TKMRunnerStone);
  RegisterRunner(TKMRunnerFight95);
  RegisterRunner(TKMRunnerAIBuild);
  RegisterRunner(TKMVortamicPF);


end.

