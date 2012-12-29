unit TestKM_Scripting;
interface
uses
  TestFramework, StrUtils, Classes,
  SysUtils,
  KM_Defaults, KM_Scripting, KM_Log, KM_Utils;


type
  TestKMScripting = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAllMaps;
  end;


implementation


procedure TestKMScripting.SetUp;
begin
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\';

  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'Temp\temp.log');
  fScripting := TKMScripting.Create;
end;

procedure TestKMScripting.TearDown;
begin
  fScripting.Free;
  fLog.Free;
end;

//See if all maps load into Scripting
procedure TestKMScripting.TestLoadAllMaps;
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
          and FileExists(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.script') then
          begin
            try
              fScripting.LoadFromFile(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.script');
              Inc(Count);
            except
              //Report and swallow asserts
              on E: EAssertionFailed do
                Status('Script did not load: ' + SearchRec.Name + '. '+ E.Message);
            end;
            Check(fScripting.ErrorString = '', 'Script did not load: ' + SearchRec.Name);
            Inc(Total);
          end;
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;

  finally
    PathToMaps.Free;
  end;

  Status(IntToStr(Total - Count) + ' of ' + IntToStr(Total) + ' scripts failed');
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestKMScripting.Suite);


end.
