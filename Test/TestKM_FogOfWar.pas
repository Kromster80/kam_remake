unit TestKM_FogOfWar;
interface
uses
  TestFramework, Classes, KM_Points, Math, SysUtils, KM_FogOfWar, KM_CommonClasses;

type
  TestTKMFogOfWar = class(TTestCase)
  strict private
    FKMFogOfWar: TKMFogOfWar;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRevealCircle;
    procedure TestRevealEverything;
    procedure TestCheckVerticeRevelation;
    procedure TestCheckTileRevelation;
    procedure TestCheckRevelation;
    procedure TestSyncFOW;
    procedure TestSave;
    procedure TestLoad;
    procedure TestUpdateState;
  end;

implementation

procedure TestTKMFogOfWar.SetUp;
begin
  FKMFogOfWar := TKMFogOfWar.Create(8, 8);
end;

procedure TestTKMFogOfWar.TearDown;
begin
  FKMFogOfWar.Free;
  FKMFogOfWar := nil;
end;

procedure TestTKMFogOfWar.TestRevealCircle;
var
  Amount: Word;
  Radius: Word;
  Pos: TKMPoint;
begin
  // TODO: Setup method call parameters
  FKMFogOfWar.RevealCircle(Pos, Radius, Amount);
  // TODO: Validate method results
end;

procedure TestTKMFogOfWar.TestRevealEverything;
begin
  FKMFogOfWar.RevealEverything;
  // TODO: Validate method results
end;

procedure TestTKMFogOfWar.TestCheckVerticeRevelation;
var
  I,K: Word;
  ReturnValue: Byte;
begin
  FKMFogOfWar.RevealEverything;
  for I := 0 to 7 do
  for K := 0 to 7 do
  begin
    ReturnValue := FKMFogOfWar.CheckVerticeRevelation(I, K);
    Check(ReturnValue = 255, IntToStr(I)+IntToStr(K));
  end;
end;

//Valid tiles are in 1..MAX-1 range (last row of tiles is not built because of no vertices)
procedure TestTKMFogOfWar.TestCheckTileRevelation;
var
  I,K: Word;
  ReturnValue: Byte;
begin
  FKMFogOfWar.RevealEverything;

  for I := 0 to 8 do
  for K := 0 to 8 do
  begin
    ReturnValue := FKMFogOfWar.CheckTileRevelation(I, K);
    if InRange(I, 1, 7) and InRange(K, 1, 7) then
      Check(ReturnValue = 255, IntToStr(I)+IntToStr(K))
    else
      Check(ReturnValue = 0, IntToStr(I)+IntToStr(K));
  end;
end;

procedure TestTKMFogOfWar.TestCheckRevelation;
var
  I,K: Integer;
  ReturnValue: Byte;
  aPoint: TKMPointF;
begin
  FKMFogOfWar.RevealEverything;
  for I := -10 to 800 do
  for K := -10 to 800 do
  begin
    ReturnValue := FKMFogOfWar.CheckRevelation(KMPointF(I/100, K/100));
    if InRange(I/100, 0.001, 6.999) and InRange(K/100, 0.001, 6.999) then
      Check(ReturnValue = 255, IntToStr(I)+IntToStr(K))
    else
      Check(ReturnValue = 0, IntToStr(I)+IntToStr(K));
  end;
end;

procedure TestTKMFogOfWar.TestSyncFOW;
var
  aFOW: TKMFogOfWar;
begin
  // TODO: Setup method call parameters
  //FKMFogOfWar.SyncFOW(aFOW);
  // TODO: Validate method results
end;

procedure TestTKMFogOfWar.TestSave;
var
  SaveStream: TKMemoryStream;
begin
  // TODO: Setup method call parameters
  //FKMFogOfWar.Save(SaveStream);
  // TODO: Validate method results
end;

procedure TestTKMFogOfWar.TestLoad;
var
  LoadStream: TKMemoryStream;
begin
  // TODO: Setup method call parameters
  //FKMFogOfWar.Load(LoadStream);
  // TODO: Validate method results
end;

procedure TestTKMFogOfWar.TestUpdateState;
begin
  FKMFogOfWar.UpdateState;
  // TODO: Validate method results
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTKMFogOfWar.Suite);
end.

