unit TestKM_UnitActionGoInOut;
{$I KaM_Remake.inc}
interface
uses
  TestFramework, KM_Units, KM_Points, KM_CommonClasses, Classes, KromUtils, SysUtils,
  KM_Defaults, KM_UnitActionGoInOut, KM_Houses;

type
  // Test methods for class TUnitActionGoInOut
  TestTUnitActionGoInOut = class(TTestCase)
  strict private
    fUnit: TKMUnit;
    fHouse: TKMHouse;
    FUnitActionGoInOut: TUnitActionGoInOut;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecute;
  end;


implementation
uses KM_Log, KM_PlayersCollection, KM_PlayerSpectator, KM_Resource, KM_Sound, KM_Terrain, KM_Utils, KM_ResourceHouse;


procedure TestTUnitActionGoInOut.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  SetKaMSeed(4);
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'log.log');
  fResource := TResource.Create(nil, nil, nil);
  fResource.LoadMenuResources;
  fSoundLib := TSoundLib.Create(0, False);
  gTerrain := TKMTerrain.Create;
  gTerrain.MakeNewMap(32, 32, False);
  fPlayers := TKMPlayersCollection.Create;
  fPlayers.AddPlayers(1);
  MySpectator := TKMSpectator.Create(0);

  fUnit := TKMUnit.Create(0, ut_Serf, KMPoint(8, 10), 0);
  fHouse := TKMHouse.Create(0, ht_Store, 9, 9, 0, hbs_Done);
  FUnitActionGoInOut := TUnitActionGoInOut.Create(fUnit, ua_Walk, gd_GoInside, fHouse);
end;


procedure TestTUnitActionGoInOut.TearDown;
begin
  FUnitActionGoInOut.Free;
  fPlayers.Free;
  gTerrain.Free;
  fSoundLib.Free;
  fResource.Free;
  gLog.Free;
  FUnitActionGoInOut := nil;
end;


procedure TestTUnitActionGoInOut.TestExecute;
var
  ReturnValue: TActionResult;
  I: Integer;
  K: Integer;
begin
  TearDown;

  for I := 1 to 20 do
  begin
    SetUp;
    for K := 0 to I do
      FUnitActionGoInOut.Execute;

    fUnit.HitPointsDecrease(255, PLAYER_NONE);
    FUnitActionGoInOut.Execute;
    TearDown;
  end;

  SetUp;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUnitActionGoInOut.Suite);


end.

