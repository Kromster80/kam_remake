unit TestKM_Terrain;
interface
uses
  TestFramework, StrUtils,
  SysUtils, KM_Terrain, KM_Log, KM_Utils;

type
  // Test methods for Points
  TestKMTerrain = class(TTestCase)
  strict private

  private

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAllMaps;
  end;

implementation

procedure TestKMTerrain.SetUp;
begin
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fTerrain := TTerrain.Create;
end;

procedure TestKMTerrain.TearDown;
begin
  fTerrain.Free;
  fLog.Free;
end;

procedure TestKMTerrain.TestLoadAllMaps;
var
  I: Integer;
  SearchRec: TSearchRec;
  PathToMaps: string;
begin
  for I := 0 to 3 do
  begin
    case I of
      0: PathToMaps := ExtractFilePath(ParamStr(0)) + '..\Maps\';
      1: PathToMaps := ExtractFilePath(ParamStr(0)) + '..\MapsMP\';
      2: PathToMaps := ExtractFilePath(ParamStr(0)) + '..\Campaigns\The Shattered Kingdom\';
      3: PathToMaps := ExtractFilePath(ParamStr(0)) + '..\Campaigns\The Peasants Rebellion\';
    end;

    if DirectoryExists(PathToMaps) then
    begin
      FindFirst(PathToMaps + '*', faDirectory, SearchRec);
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        and FileExists(PathToMaps + SearchRec.Name + '\' + SearchRec.Name + '.map') then
        begin
          fTerrain.LoadFromFile(PathToMaps + SearchRec.Name + '\' + SearchRec.Name + '.map', False);
          Check(fTerrain.MapX * fTerrain.MapY <> 0, 'Map did not load: ' + SearchRec.Name);
        end;
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    end;
  end;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMTerrain.Suite);
end.
