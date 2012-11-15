unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes, KM_Utils;


//This is our custom logging system
type
  TKMLog = class
  private
    fl: textfile;
    fLogPath: string;
    fFirstTick: cardinal;
    fPreviousTick: cardinal;
    fPreviousDate: TDateTime;
    procedure AddLineTime(const aText: string);
    procedure AddLineNoTime(const aText: string);
  public
    constructor Create(const aPath: string);
    // AppendLog adds the line to Log along with time passed since previous line added
    procedure AddTime(const aText: string); overload;
    procedure AddTime(const aText: string; num: Integer); overload;
    procedure AddTime(const aText: string; num: Single); overload;
    procedure AddTime(num: Integer; const aText: string); overload;
    procedure AddTime(const aText: string; Res: boolean); overload;
    procedure AddTime(a, b: integer); overload;
    // Add line if TestValue=false
    procedure AddAssert(const aMessageText: string);
    // AddToLog simply adds the text
    procedure AddNoTime(const aText: string);
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
var SearchRec: TSearchRec;
begin
  if not DirectoryExists(fPathToLogs) then Exit;

  if FindFirst(fPathToLogs + 'KaM*.log', faAnyFile, SearchRec) = 0 then
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory) //Only files
    and(Abs(Now - FileDateToDateTime(FileAge(fPathToLogs + SearchRec.Name))) > DEL_LOGS_OLDER_THAN)
    then
      DeleteFile(fPathToLogs + SearchRec.Name);
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
  AddLineNoTime('Timestamp'#9'Elapsed'#9'Delta'#9'Description');
  AddLineTime('Log is up and running. Game version: ' + GAME_VERSION);
end;


//Run thread to delete old logs.
procedure TKMLog.DeleteOldLogs;
begin
  //No need to remember the instance, it's set to FreeOnTerminate
  TKMOldLogsDeleter.Create(ExtractFilePath(fLogPath));
end;


//Lines are timestamped, each line invokes file open/close for writing,
//meaning that no lines will be lost if Remake crashes
procedure TKMLog.AddLineTime(const aText: string);
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
             floattostr(GetTimeSince(fFirstTick)/1000)+'s'#9+
             floattostr(GetTimeSince(fPreviousTick))+'ms'#9+aText);
  CloseFile(fl);
  fPreviousTick := TimeGet;
  fPreviousDate := Now;
end;


{Same line but without timestamp}
procedure TKMLog.AddLineNoTime(const aText: string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  WriteLn(fl, #9#9#9#9 + aText);
  CloseFile(fl);
end;


procedure TKMLog.AddTime(const aText: string);
begin
  AddLineTime(aText);
end;


procedure TKMLog.AddTime(const aText: string; num: integer);
begin
  AddLineTime(aText + ' ' + inttostr(num));
end;


procedure TKMLog.AddTime(const aText: string; num: single);
begin
  AddLineTime(aText + ' ' + floattostr(num));
end;


procedure TKMLog.AddTime(num: integer; const aText: string);
begin
  AddLineTime(inttostr(num) + ' ' + aText);
end;


procedure TKMLog.AddTime(const aText: string; Res: boolean);
var
  s: string;
begin
  if Res then
    s := 'done'
  else
    s := 'fail';
  AddLineTime(aText + ' ... ' + s);
end;


procedure TKMLog.AddTime(A, B: integer);
begin
  AddLineTime(inttostr(A) + ' : ' + inttostr(B));
end;


procedure TKMLog.AddAssert(const aMessageText: string);
begin
  AddLineNoTime('ASSERTION FAILED! Msg: ' + aMessageText);
  Assert(False, 'ASSERTION FAILED! Msg: ' + aMessageText);
end;


procedure TKMLog.AddNoTime(const aText: string);
begin
  AddLineNoTime(aText);
end;


end.
