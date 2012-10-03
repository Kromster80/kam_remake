unit TestKM_AIFields;
interface
uses
  TestFramework, KM_CommonTypes, SysUtils, KM_CommonClasses, KM_Points,
  KM_Defaults, KromUtils, Classes, KM_Utils, Math, KM_Player, KM_PolySimplify,
  KM_AIFields, KM_Log, KM_Terrain, KM_Resource, KM_ResourceTileset;


type
  TestTKMAIFields = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUpdateNavMesh;
  end;


implementation


procedure TestTKMAIFields.SetUp;
begin
  SKIP_RENDER := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';

  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fResource := TResource.Create(nil, nil, nil);
  fResource.LoadMenuResources('');
  fTerrain := TTerrain.Create;
  fAIFields := TKMAIFields.Create;
end;

procedure TestTKMAIFields.TearDown;
begin
  fAIFields.Free;
  fTerrain.Free;
  fResource.Free;
  fLog.Free;
end;

procedure TestTKMAIFields.TestUpdateNavMesh;
var
  I: Integer;
  Count, Total: Integer;
  SearchRec: TSearchRec;
  PathToMaps: TStringList;
begin
  Count := 0;
  Total := 0;

  PathToMaps := TStringList.Create;
  try
    PathToMaps.Add(ExeDir + 'Maps\');
    PathToMaps.Add(ExeDir + 'MapsMP\');
    PathToMaps.Add(ExeDir + 'Tutorials\');

    //Include all campaigns maps
    FindFirst(ExeDir + 'Campaigns\*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        PathToMaps.Add(ExeDir + 'Campaigns\' + SearchRec.Name + '\');
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    for I := 0 to PathToMaps.Count - 1 do
      if DirectoryExists(PathToMaps[I]) then
      begin
        FindFirst(PathToMaps[I] + '*', faDirectory, SearchRec);
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
          and FileExists(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.map') then
          begin
            try
              fTerrain.LoadFromFile(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.map', False);
              fAIFields.UpdateState(0);
              Inc(Count);
            except
              //Report and swallow asserts
              on E: EAssertionFailed do
                Status('Map did not load: ' + SearchRec.Name + '. '+ E.Message);
            end;
            Inc(Total);
          end;
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;

  finally
    PathToMaps.Free;
  end;

  Check(Total = Count, IntToStr(Total - Count) + ' of ' + IntToStr(Total) + ' maps failed');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestTKMAIFields.Suite);


end.

