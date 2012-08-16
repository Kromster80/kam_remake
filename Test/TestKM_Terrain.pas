unit TestKM_Terrain;
interface
uses
  TestFramework, StrUtils, Classes,
  SysUtils,
  KM_Defaults, KM_Terrain, KM_Resource, KM_ResourceTileset, KM_Log, KM_Utils;

type
  TestKMTerrain = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAllMaps;
  end;

implementation

procedure TestKMTerrain.SetUp;
begin
  SKIP_RENDER := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';

  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fResource := TResource.Create(nil, nil, nil);
  fResource.LoadMenuResources('');
  fTerrain := TTerrain.Create;
end;

procedure TestKMTerrain.TearDown;
begin
  fTerrain.Free;
  fResource.Free;
  fLog.Free;
end;

//See if all maps load into Terrain
procedure TestKMTerrain.TestLoadAllMaps;
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
              Inc(Count);
            except
              //Report and swallow asserts
              on E: EAssertionFailed do Status('Map did not load: ' + SearchRec.Name + '. '+ E.Message);
              //Signal other errors loud
              else raise;
            end;
            Check(fTerrain.MapX * fTerrain.MapY <> 0, 'Map did not load: ' + SearchRec.Name);
            Inc(Total);
          end;
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;

  finally
    PathToMaps.Free;
  end;

  Status(IntToStr(Count) + '/' + IntToStr(Total) + ' maps checked')
end;


initialization
  // Register any test cases with the test runner
  RegisterTest('Functional', TestKMTerrain.Suite);
end.
