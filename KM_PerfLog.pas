unit KM_PerfLog;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils;


type
  TKMPerfLog = class
  public
    Count: Integer;
    Times: array of Cardinal;
    procedure Clear;
    procedure AddTime(aTime: Cardinal);
    procedure SaveToFile(aFilename: string);
  end;


implementation


{ TKMPerfLog }
procedure TKMPerfLog.Clear;
begin
  Count := 0;
end;


procedure TKMPerfLog.AddTime(aTime: Cardinal);
begin
  if Count >= Length(Times) then
    SetLength(Times, Count + 1024);

  Times[Count] := aTime;
  Inc(Count);
end;


procedure TKMPerfLog.SaveToFile(aFilename: string);
var
  I: Integer;
  S: TFileStream;
  ss: AnsiString;
begin
  ForceDirectories(ExtractFilePath(aFilename));
  S := TFileStream.Create(aFilename, fmCreate);

  for I := 0 to Count - 1 do
  if Times[I] > 15 then //Dont bother saving 95% of data
  begin
    ss := Format('%d'#9'%d' + eol, [I, Times[I]]);
    S.WriteBuffer(ss[1], Length(ss));
  end;

  S.Free;
end;


end.
