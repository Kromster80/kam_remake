unit KM_PerfLog;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils;


type
  TKMPerfLog = class
  private
    fCount: Integer;
    fTimes: array of Cardinal;
  public
    procedure Clear;
    procedure AddTime(aTime: Cardinal);
    procedure SaveToFile(aFilename: string);
  end;


implementation


{ TKMPerfLog }
procedure TKMPerfLog.Clear;
begin
  fCount := 0;
end;


procedure TKMPerfLog.AddTime(aTime: Cardinal);
begin
  if fCount >= Length(fTimes) then
    SetLength(fTimes, fCount + 1024);

  fTimes[fCount] := aTime;
  inc(fCount);
end;


procedure TKMPerfLog.SaveToFile(aFilename: string);
var I: Integer;
  S: TFileStream;
  ss: AnsiString;
begin
  S := TFileStream.Create(aFilename, fmCreate);

  for I := 0 to fCount - 1 do
  begin
    ss := Format('%d'#9'%d' + eol, [I, fTimes[I]]);
    S.WriteBuffer(ss[1], Length(ss));
  end;

  S.Free;
end;


end.
