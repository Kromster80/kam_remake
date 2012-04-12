unit TestKM_Points;
interface
uses
  TestFramework,
  SysUtils, KM_Points;

type
  // Test methods for Points
  TestKMPoints = class(TTestCase)
  strict private
    //FKMGame: TKMGame;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestKMPoint;
    procedure TestKMPointBelow;
    procedure TestKMRect;
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

initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMPoints.Suite);
end.
