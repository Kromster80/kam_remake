unit TestKM_MessageStack;
interface
uses
  TestFramework, KM_MessageStack, KM_Points, KM_CommonTypes, KM_CommonClasses,
  SysUtils, Math;

type
  TestTKMMessageList = class(TTestCase)
  strict private
    fMessageList: TKMMessageList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;


implementation


procedure TestTKMMessageList.SetUp;
begin
  fMessageList := TKMMessageList.Create;
  fMessageList.Add(mkText, '', KMPoint(0,0));
  fMessageList.Add(mkText, '', KMPoint(0,0));
  fMessageList.Add(mkHouse, '', KMPoint(0,0));
  fMessageList.Add(mkHouse, '', KMPoint(0,0));
  fMessageList.Add(mkText, '', KMPoint(0,0));
  fMessageList.Add(mkUnit, '', KMPoint(0,0));
  fMessageList.Add(mkQuill, '', KMPoint(0,0));
end;

procedure TestTKMMessageList.TearDown;
begin
  fMessageList.Free;
  fMessageList := nil;
end;

initialization
  RegisterTest('MessageStack', TestTKMMessageList.Suite);


end.

