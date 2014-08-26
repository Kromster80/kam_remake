unit TestKM_Terrain;
interface
uses
  TestFramework, StrUtils, Classes,
  SysUtils,
  KM_Defaults, KM_Terrain, KM_Resource, KM_ResTileset, KM_Log, KM_Utils;


type
  TestKMTerrain = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAllMaps;
  end;


implementation
uses KM_Maps;


procedure TestKMTerrain.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';

  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  gRes := TKMResource.Create(nil, nil);
  gRes.LoadMainResources;
  gTerrain := TKMTerrain.Create;
end;


procedure TestKMTerrain.TearDown;
begin
  gTerrain.Free;
  FreeAndNil(gRes);
  gLog.Free;
end;


//See if all maps load into Terrain
procedure TestKMTerrain.TestLoadAllMaps;
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
    begin
      try
        gTerrain.LoadFromFile(ChangeFileExt(PathToMaps[I], '.map'), False);
        Inc(GoodMaps);
      except
        //Report and swallow asserts
        on E: EAssertionFailed do
          Status('Map did not load: ' + PathToMaps[I] + '. '+ E.Message);
      end;

      Check(gTerrain.MapX * gTerrain.MapY <> 0, 'Map did not load: ' + PathToMaps[I]);
    end;

    Status(IntToStr(PathToMaps.Count - GoodMaps) + ' of ' + IntToStr(PathToMaps.Count) + ' maps failed');
  finally
    PathToMaps.Free;
  end;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMTerrain.Suite);


end.
