unit TestKM_Scripting;
interface
uses
  TestFramework, StrUtils, Classes,
  SysUtils,
  KM_Defaults, KM_Scripting, KM_Log, KM_Utils;


type
  TestKMScripting = class(TTestCase)
  private
    fScripting: TKMScripting;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAllMaps;
  end;


implementation
uses
  KM_Maps;


procedure TestKMScripting.SetUp;
begin
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';

  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fScripting := TKMScripting.Create(nil);
end;


procedure TestKMScripting.TearDown;
begin
  fScripting.Free;
  gLog.Free;
end;


//See if all maps load into Scripting
procedure TestKMScripting.TestLoadAllMaps;
var
  I: Integer;
  GoodMaps: Integer;
  PathToMaps: TStringList;
begin
  GoodMaps := 0;

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    for I := 0 to PathToMaps.Count - 1 do
    if FileExists(ChangeFileExt(PathToMaps[I], '.script')) then
    begin
      try
        fScripting.LoadFromFile(ChangeFileExt(PathToMaps[I], '.script'), '', nil);
        Inc(GoodMaps);
      except
        //Report and swallow asserts
        on E: EAssertionFailed do
          Status('Script did not load: ' + PathToMaps[I] + '. '+ E.Message);
      end;

      Check(fScripting.ErrorHandler.HasErrors, 'Script did not load: ' + PathToMaps[I]);
    end;

    Status(IntToStr(PathToMaps.Count - GoodMaps) + ' of ' + IntToStr(PathToMaps.Count) + ' scripts failed');
  finally
    PathToMaps.Free;
  end;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMScripting.Suite);


end.
