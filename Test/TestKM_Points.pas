unit TestKM_Points;
interface
uses
  TestFramework,
  SysUtils, KM_Points;

type
  // Test methods for Points
  TestKMPoints = class(TTestCase)
  strict private

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestKMPoint;
    procedure TestKMPointBelow;
    procedure TestKMRect;
    procedure TestKMAddDirection;
  end;

implementation

procedure TestKMPoints.SetUp;
begin
  //
end;

procedure TestKMPoints.TearDown;
begin
  //
end;

procedure TestKMPoints.TestKMPoint;
var T: TKMPoint;
begin
  T := KMPoint(0,0);
  Check((T.X = 0) and (T.Y = 0));
  T := KMPoint(65535,65535);
  Check((T.X = 65535) and (T.Y = 65535));
end;

procedure TestKMPoints.TestKMPointBelow;
var T: TKMPoint;
begin
  T := KMPoint(0,0);
  T := KMPointBelow(T);
  Check((T.X = 0) and (T.Y = 1));

  T := KMPoint(0, 1024);
  T := KMPointBelow(T);
  Check((T.X = 0) and (T.Y = 1025));
end;


procedure TestKMPoints.TestKMRect;
var
  T: TKMPointF;
  R: TKMRect;
begin
  T := KMPointF(-1.5, 2.4);
  R := KMRect(T);
  Check((R.Left = -2) and (R.Top = 2) and (R.Right = -1) and (R.Bottom = 3));

  T := KMPointF(-1, 2);
  R := KMRect(T);
  Check((R.Left = -2) and (R.Top = 1) and (R.Right = 0) and (R.Bottom = 3));

  T := KMPointF(0, 0);
  R := KMRect(T);
  Check((R.Left = -1) and (R.Top = -1) and (R.Right = 1) and (R.Bottom = 1));
end;


procedure TestKMPoints.TestKMAddDirection;
begin
  Check(KMAddDirection(dir_N, 1) = dir_NE);
  Check(KMAddDirection(dir_N, 2) = dir_E);
  Check(KMAddDirection(dir_N, 3) = dir_SE);
  Check(KMAddDirection(dir_N, 4) = dir_S);
  Check(KMAddDirection(dir_N, 5) = dir_SW);
  Check(KMAddDirection(dir_N, 6) = dir_W);
  Check(KMAddDirection(dir_N, 7) = dir_NW);
  Check(KMAddDirection(dir_N, 8) = dir_N);
  Check(KMAddDirection(dir_N, 9) = dir_NE);
  Check(KMAddDirection(dir_N, 160) = dir_N);
  Check(KMAddDirection(dir_N, 161) = dir_NE);
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMPoints.Suite);
end.
