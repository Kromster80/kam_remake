unit TestKM_UnitActionGoInOut;
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
    procedure TestSyncLoad;
    procedure TestActName;
    procedure TestGetExplanation;
    procedure TestGetDoorwaySlide;
    procedure TestExecute;
  end;

implementation
uses KM_Log, KM_PlayersCollection, KM_Resource, KM_Sound, KM_Terrain, KM_Utils;

procedure TestTUnitActionGoInOut.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  SetKaMSeed(4);
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'log.log');
  fResource := TResource.Create(nil, nil, nil);
  fResource.LoadMenuResources('');
  fSoundLib := TSoundLib.Create('', 0, False);
  fTerrain := TTerrain.Create;
  fTerrain.MakeNewMap(32, 32, False);
  fPlayers := TKMPlayersCollection.Create;
  fPlayers.AddPlayers(1);
  MyPlayer := fPlayers[0];

  fUnit := TKMUnit.Create(0, ut_Serf, 8, 10, 0);
  fHouse := TKMHouse.Create(0, ht_Store, 9, 9, 0, hbs_Done);
  FUnitActionGoInOut := TUnitActionGoInOut.Create(fUnit, ua_Walk, gd_GoInside, fHouse);
end;

procedure TestTUnitActionGoInOut.TearDown;
begin
  FUnitActionGoInOut.Free;
  fPlayers.Free;
  fTerrain.Free;
  fSoundLib.Free;
  fResource.Free;
  fLog.Free;
  FUnitActionGoInOut := nil;
end;

procedure TestTUnitActionGoInOut.TestSyncLoad;
begin
  FUnitActionGoInOut.SyncLoad;
  // TODO: Validate method results
end;

procedure TestTUnitActionGoInOut.TestActName;
var
  ReturnValue: TUnitActionName;
begin
  ReturnValue := FUnitActionGoInOut.ActName;
  // TODO: Validate method results
end;

procedure TestTUnitActionGoInOut.TestGetExplanation;
var
  ReturnValue: string;
begin
  ReturnValue := FUnitActionGoInOut.GetExplanation;
  // TODO: Validate method results
end;

procedure TestTUnitActionGoInOut.TestGetDoorwaySlide;
var
  ReturnValue: Single;
  aCheck: TCheckAxis;
begin
  // TODO: Setup method call parameters
  ReturnValue := FUnitActionGoInOut.GetDoorwaySlide(aCheck);
  // TODO: Validate method results
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

    fUnit.HitPointsDecrease(255);
    FUnitActionGoInOut.Execute;
    TearDown;
  end;

  SetUp;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUnitActionGoInOut.Suite);
end.

