unit TestKM_MissionScript;
interface
uses
  TestFramework, StrUtils, Classes, SysUtils,
  KM_Defaults, KM_Log, KM_Utils,
  KM_GameApp, KM_ResLocales, KM_ResTexts,
  KM_MissionScript_Standard;


type
  TestKMMissionScript = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMissionScripts;
  end;


implementation
uses KM_Maps;


procedure TestKMMissionScript.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt', DEFAULT_LOCALE);
  fTextMain := TKMTextLibrarySingle.Create;
  fTextMain.LoadLocale(ExeDir + 'data\text\');
  fGameApp := TKMGameApp.Create(nil, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;


procedure TestKMMissionScript.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextMain);
  FreeAndNil(fLocales);
  FreeAndNil(gLog);
end;


//See if all scripts are parsable
procedure TestKMMissionScript.TestLoadMissionScripts;
var
  I: Integer;
  PathToMaps: TStringList;
begin
  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    for I := 0 to PathToMaps.Count - 1 do
    begin
      try
        fGameApp.NewSingleMap(PathToMaps[I], '');

        //Warnings and Errors are written into the Log
      except
        //Report and swallow asserts
        on E: EAssertionFailed do
          Status('Script did not load: ' + PathToMaps[I] + '. '+ E.Message);
      end;
    end;
  finally
    PathToMaps.Free;
  end;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMMissionScript.Suite);


end.
