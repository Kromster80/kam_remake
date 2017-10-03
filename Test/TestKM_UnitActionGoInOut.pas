unit TestKM_UnitActionGoInOut;
{$I KaM_Remake.inc}
interface
uses
  TestFramework, KM_Units, KM_Points, KM_CommonClasses, Classes, KromUtils, SysUtils,
  KM_Defaults, KM_UnitActionGoInOut, KM_Houses, KM_Scripting;

type
  // Test methods for class TUnitActionGoInOut
  TestTUnitActionGoInOut = class(TTestCase)
  strict private
    fScripting: TKMScripting;
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
uses
  KM_Log, KM_HandsCollection, KM_HandSpectator, KM_Resource, KM_ResSound, KM_Terrain, KM_CommonUtils, KM_ResHouses;


procedure TestTUnitActionGoInOut.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  SetKaMSeed(4);
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'log.log');
  gRes := TKMResource.Create(nil, nil);
  gRes.LoadMainResources;
  fScripting := TKMScripting.Create(nil);
  gTerrain := TKMTerrain.Create;
  gTerrain.MakeNewMap(32, 32, False);
  gHands := TKMHandsCollection.Create;
  gHands.AddPlayers(1);
  gMySpectator := TKMSpectator.Create(0);

  fUnit := TKMUnit.Create(0, ut_Serf, KMPoint(8, 10), 0);
  fHouse := TKMHouse.Create(0, ht_Store, 9, 9, 0, hbs_Done);
  FUnitActionGoInOut := TUnitActionGoInOut.Create(fUnit, ua_Walk, gd_GoInside, fHouse);
end;


procedure TestTUnitActionGoInOut.TearDown;
begin
  FUnitActionGoInOut.Free;
  gHands.Free;
  gTerrain.Free;
  fScripting.Free;
  FreeAndNil(gRes);
  gLog.Free;
  FUnitActionGoInOut := nil;
end;


procedure TestTUnitActionGoInOut.TestExecute;
var
  I: Integer;
  K: Integer;
begin
  TearDown;

  for I := 1 to 20 do
  begin
    SetUp;
    for K := 0 to I do
      FUnitActionGoInOut.Execute;

    fUnit.HitPointsDecrease(255, nil);
    FUnitActionGoInOut.Execute;
    TearDown;
  end;

  SetUp;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUnitActionGoInOut.Suite);


end.

