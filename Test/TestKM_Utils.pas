unit TestKM_Utils;
interface
uses
  TestFramework, StrUtils, Classes,
  SysUtils,
  KM_CommonUtils;

type
  TestKMUtils = class(TTestCase)
  published
    procedure TestTimeToString;
  end;

implementation

//See if time gets correctly converted to string
procedure TestKMUtils.TestTimeToString;
const ToSec = 1/24/60/60;
begin
  Check(TimeToString(0) = '00:00:00', '0');
  Check(TimeToString(0.5 * ToSec) = '00:00:00', '0.5');
  Check(TimeToString(1.5 * ToSec) = '00:00:01', '1.5');
  Check(TimeToString(59.5 * ToSec) = '00:00:59', '59.5');
  Check(TimeToString(60 * ToSec) = '00:01:00', '60');
  Check(TimeToString(60.5 * ToSec) = '00:01:00', '60.5');
  Check(TimeToString(3599 * ToSec) = '00:59:59', '3599 <> ' + TimeToString(3599 * ToSec));
  Check(TimeToString(3600 * ToSec) = '01:00:00', '3600 <> ' + TimeToString(3600 * ToSec));
  Check(TimeToString(3601 * ToSec) = '01:00:01', '3601');
  Check(TimeToString(36000 * ToSec) = '10:00:00', '36000');
  Check(TimeToString(359999.5 * ToSec) = '99:59:59', '359999.5');
  Check(TimeToString(360000 * ToSec) = '100:00:00', '360000');
  Check(TimeToString(360000.5 * ToSec) = '100:00:00', '360000.5');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest('Utils', TestKMUtils.Suite);
end.
