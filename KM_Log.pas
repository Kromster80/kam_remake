unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, KromUtils;

{This is our custom logging system}
type
  TKMLog = class
  private
    fl: textfile;
    fLogPath: string;
    fFirstTick: cardinal;
    fPreviousTick: cardinal;
    fPreviousDate: TDateTime;
    procedure AddLine(const aText: string);
    procedure AddLineNoTime(const aText: string);
  public
    constructor Create(aPath: string);
    // AppendLog adds the line to Log along with time passed since previous line added
    procedure AppendLog(const aText: string); overload;
    procedure AppendLog(const aText: string; num: integer); overload;
    procedure AppendLog(const aText: string; num: single); overload;
    procedure AppendLog(num: integer; const aText: string); overload;
    procedure AppendLog(const aText: string; Res: boolean); overload;
    procedure AppendLog(a, b: integer); overload;
    // Add line if TestValue=false
    procedure AssertToLog(TestValue: boolean; const aMessageText: string);
    // AddToLog simply adds the text
    procedure AddToLog(const aText: string);
    property LogPath: string read fLogPath;
  end;

  var
    fLog: TKMLog;


implementation
uses KM_Defaults;


{Reset log file}
constructor TKMLog.Create(aPath:string);
begin
  Inherited Create;
  fLogPath := aPath;
  fFirstTick := TimeGet;
  fPreviousTick := TimeGet;
  AssignFile(fl, fLogPath);
  Rewrite(fl);
  CloseFile(fl);
  AddLine('Log is up and running. Game version: '+GAME_VERSION);
end;


{Lines are timestamped, each line invokes file open/close for writing,
meaning no lines will be lost if Remake crashes}
procedure TKMLog.AddLine(const aText:string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
  if abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
  begin
    WriteLn(fl,'================');
    WriteLn(fl,'Date: '+FormatDateTime('yyyy/mm/dd',Now));
    WriteLn(fl,'================');
  end;
  WriteLn(fl,FormatDateTime('hh:nn:ss:zzz',Now)+#9+
             floattostr((TimeGet - fFirstTick)/1000)+'s'+#9+
             floattostr((TimeGet - fPreviousTick)/1000)+'s'+#9+aText);
  CloseFile(fl);
  fPreviousTick := TimeGet;
  fPreviousDate := Now;
end;


{Same line but without timestamp}
procedure TKMLog.AddLineNoTime(const aText: string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  WriteLn(fl, #9#9 + aText);
  CloseFile(fl);
end;


procedure TKMLog.AppendLog(const aText: string);
begin
  AddLine(aText);
end;


procedure TKMLog.AppendLog(const aText: string; num: integer);
begin
  AddLine(aText + ' ' + inttostr(num));
end;


procedure TKMLog.AppendLog(const aText: string; num: single);
begin
  AddLine(aText + ' ' + floattostr(num));
end;


procedure TKMLog.AppendLog(num: integer; const aText: string);
begin
  AddLine(inttostr(num) + ' ' + aText);
end;


procedure TKMLog.AppendLog(const aText: string; Res: boolean);
var
  s: string;
begin
  if Res then
    s := 'done'
  else
    s := 'fail';
  AddLine(aText + ' ... ' + s);
end;


procedure TKMLog.AppendLog(A, B: integer);
begin
  AddLine(inttostr(A) + ' : ' + inttostr(B));
end;


procedure TKMLog.AssertToLog(TestValue: boolean; const aMessageText: string);
begin
  if TestValue then
    exit;
  AddLine('ASSERTION FAILED! Msg: ' + aMessageText);
  Assert(false, 'ASSERTION FAILED! Msg: ' + aMessageText);
end;


procedure TKMLog.AddToLog(const aText: string);
begin
  AddLineNoTime(aText);
end;


end.
