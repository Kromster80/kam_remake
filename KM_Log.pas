unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes
  {$IFDEF MSWindows}
  ,MMSystem //Required for TimeGet which is defined locally because this unit must NOT know about KromUtils as it is not Linux compatible (and this unit is used in Linux dedicated servers)
  {$ENDIF}
  ;


//This is our custom logging system
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
    constructor Create(const aPath: string);
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
    procedure DeleteOldLogs;
  end;

  var
    fLog: TKMLog;


implementation
uses KM_Defaults;


//New thread, in which old logs are deleted (used internally)
type
  TKMOldLogsDeleter = class(TThread)
  private
    fPathToLogs: string;
  public
    constructor Create(const aPathToLogs:string);
    procedure Execute; override;
  end;


//This unit must not know about KromUtils because it is used by the Linux Dedicated servers
//and KromUtils is not Linux compatible. Therefore this function is copied directly from KromUtils.
//Do not remove and add KromUtils to uses, that would cause the Linux build to fail
function TimeGet: Cardinal;
begin
  {$IFDEF MSWindows}
  Result := TimeGetTime; //Return milliseconds with ~1ms precision
  {$ENDIF}
  {$IFDEF Unix}
  Result := cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
  {$ENDIF}
end;


{ TKMOldLogsDeleter }
constructor TKMOldLogsDeleter.Create(const aPathToLogs: string);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  //Must set these values BEFORE starting the thread
  FreeOnTerminate := True; //object can be automatically removed after its termination
  fPathToLogs := aPathToLogs;
end;


procedure TKMOldLogsDeleter.Execute;
var SearchRec:TSearchRec;
begin
  if not DirectoryExists(fPathToLogs) then Exit;

  FindFirst(fPathToLogs+'KaM*.log', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory) //Only files
    and(SearchRec.Name<>'.')and(SearchRec.Name<>'..')
    and(abs(Now - FileDateToDateTime(FileAge(fPathToLogs+SearchRec.Name))) > DEL_LOGS_OLDER_THAN)
    then
      DeleteFile(fPathToLogs+SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


{ TKMLog }
constructor TKMLog.Create(const aPath: string);
begin
  inherited Create;
  fLogPath := aPath;
  fFirstTick := TimeGet;
  fPreviousTick := TimeGet;
  ForceDirectories(ExtractFilePath((aPath)));
  AssignFile(fl, fLogPath);
  Rewrite(fl);
  CloseFile(fl);
  AddLine('Log is up and running. Game version: ' + GAME_VERSION);
  AddLine('Timestamp'#9'Elapsed time'#9'Delta'#9'Description');
end;


//Run thread to delete old logs.
procedure TKMLog.DeleteOldLogs;
begin
  //No need to remember the instance, it's set to FreeOnTerminate
  TKMOldLogsDeleter.Create(ExtractFilePath(fLogPath));
end;


//Lines are timestamped, each line invokes file open/close for writing,
//meaning that no lines will be lost if Remake crashes
procedure TKMLog.AddLine(const aText: string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
  if Abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
  begin
    WriteLn(fl, '================');
    WriteLn(fl, 'Date: ' + FormatDateTime('yyyy/mm/dd', Now));
    WriteLn(fl, '================');
  end;
  WriteLn(fl,FormatDateTime('hh:nn:ss:zzz', Now)+#9+
             floattostr((TimeGet - fFirstTick)/1000)+'s'#9+
             floattostr((TimeGet - fPreviousTick))+'ms'#9+aText);
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
