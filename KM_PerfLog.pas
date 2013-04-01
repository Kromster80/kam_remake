unit KM_PerfLog;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils;


type
  TPerfSection = (psTick);

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
    procedure SaveToFile(aFilename: string);
  end;


implementation


const
  SectionName: array [TPerfSection] of string = ('Tick');


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
begin
  if fCount[aSection] >= Length(fTimes[aSection]) then
    SetLength(fTimes[aSection], fCount[aSection] + 1024);

  fTimes[aSection, fCount[aSection]] := TimeGet - fTimeEnter[aSection];
  Inc(fCount[aSection]);
end;


procedure TKMPerfLog.SaveToFile(aFilename: string);
var
  K: TPerfSection;
  I: Integer;
  S: TFileStream;
  ss: AnsiString;
begin
  ForceDirectories(ExtractFilePath(aFilename));
  S := TFileStream.Create(aFilename, fmCreate);

  for K := Low(TPerfSection) to High(TPerfSection) do
  begin
    //Section name
    ss := SectionName[K] + eol + StringOfChar('-', 60) + eol;
    S.WriteBuffer(ss[1], Length(ss));

    //Times
    for I := 0 to fCount[K] - 1 do
    if fTimes[K,I] > 10 then //Dont bother saving 95% of data
    begin
      ss := Format('%d'#9'%d' + eol, [I, fTimes[K,I]]);
      S.WriteBuffer(ss[1], Length(ss));
    end;

    //Footer
    ss := eol + eol;
    S.WriteBuffer(ss[1], Length(ss));
  end;

  S.Free;
end;


end.
