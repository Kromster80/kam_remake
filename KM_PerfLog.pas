unit KM_PerfLog;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils;


type
  TPerfSection = (psTick, psHungarian);

  //Log how much time each section takes and write results to a log file

  TKMPerfLog = class
  private
    fCount: array [TPerfSection] of Integer;
    fTimeEnter: array [TPerfSection] of Cardinal;
    fTimes: array [TPerfSection] of array of Cardinal;
  public
    procedure Clear;
    procedure EnterSection(aSection: TPerfSection);
    procedure LeaveSection(aSection: TPerfSection);
    procedure SaveToFile(aFilename: UnicodeString);
  end;


implementation


const
  SectionName: array [TPerfSection] of AnsiString = ('Tick', 'Hungarian');


{ TKMPerfLog }
procedure TKMPerfLog.Clear;
var
  I: TPerfSection;
begin
  for I := Low(TPerfSection) to High(TPerfSection) do
    fCount[I] := 0;
end;


procedure TKMPerfLog.EnterSection(aSection: TPerfSection);
begin
  fTimeEnter[aSection] := TimeGet;
end;


procedure TKMPerfLog.LeaveSection(aSection: TPerfSection);
var T: Cardinal;
begin
  T := TimeGet - fTimeEnter[aSection]; //Measure it ASAP
  if fCount[aSection] >= Length(fTimes[aSection]) then
    SetLength(fTimes[aSection], fCount[aSection] + 1024);

  fTimes[aSection, fCount[aSection]] := T;
  Inc(fCount[aSection]);
end;


procedure TKMPerfLog.SaveToFile(aFilename: UnicodeString);
var
  K: TPerfSection;
  I: Integer;
  S: TStringList;
begin
  ForceDirectories(ExtractFilePath(aFilename));

  S := TStringList.Create;

  for K := Low(TPerfSection) to High(TPerfSection) do
  begin
    //Section name
    S.Append(SectionName[K]);
    S.Append(StringOfChar('-', 60));

    //Times
    for I := 0 to fCount[K] - 1 do
    if fTimes[K,I] > 10 then //Dont bother saving 95% of data
      S.Append(Format('%d'#9'%d', [I, fTimes[K,I]]));

    //Footer
    S.Append('');
    S.Append('');
  end;

  S.SaveToFile(aFilename);
  S.Free;
end;


end.
