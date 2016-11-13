unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes, KM_Utils, Log4d, Forms;

const
  LOG_NET_CATEGORY = 'net';
  LOG_DELIVERY_CATEGORY = 'delivery';

  NO_TIME_LOG_LVL_NAME = 'NoTime';
  ASSERT_LOG_LVL_NAME = 'Assert';

  function GetLogger(aClass: TClass; aCategory: string = ''): TLogLogger;
  function GetNetLogger(aClass: TClass): TLogLogger;
  function GetDeliveryLogger(aClass: TClass): TLogLogger;
  function GetNoTimeLogLvl: TLogLevel;
  function GetAssertLogLvl: TLogLevel;
  procedure DeleteOldLogs;
  function GetLogPath:string;

var
  fLogPath: string;
type
  //Logging system
//  TKMLog = class
//  private
//    fl: textfile;
//    fLogPath: UnicodeString;
//    fFirstTick: cardinal;
//    fPreviousTick: cardinal;
//    fPreviousDate: TDateTime;
//    procedure AddLineTime(const aText: UnicodeString);
//    procedure AddLineNoTime(const aText: UnicodeString);
//  public
//    constructor Create(const aPath: UnicodeString);
//    // AppendLog adds the line to Log along with time passed since previous line added
//    procedure AddTime(const aText: UnicodeString); overload;
//    procedure AddTime(const aText: UnicodeString; num: Integer); overload;
//    procedure AddTime(const aText: UnicodeString; num: Single); overload;
//    procedure AddTime(num: Integer; const aText: UnicodeString); overload;
//    procedure AddTime(const aText: UnicodeString; Res: boolean); overload;
//    procedure AddTime(a, b: integer); overload;
//    // Add line if TestValue=false
//    procedure AddAssert(const aMessageText: UnicodeString);
//    // AddToLog simply adds the text
//    procedure AddNoTime(const aText: UnicodeString);
//    procedure DeleteOldLogs;
//    property LogPath: UnicodeString read fLogPath; //Used by dedicated server
//  end;

  TKMLogFileAppender = class(TLogFileAppender)
  protected
    procedure SetOption(const Name, Value: string); override;
  end;

  TKMLogLayout = class(TLogCustomLayout)
  private
    fFirstTick: Cardinal;
    fPreviousTick: Cardinal;
    fPreviousDate: TDateTime;
  protected
    function GetHeader: string; override;
    procedure SetOption(const Name, Value: string); override;
  public
    procedure Init; override;
    function Format(const Event: TLogEvent): string; override;
  end;

  TKMInfoNoTimeLogLevel = class(TLogLevel)
  public
    constructor Create;
  end;

  TKMWarnAssertLogLevel = class(TLogLevel)
  public
    constructor Create;
  end;

//var
  //gLog: TKMLog;


implementation
uses
  KM_Defaults;


//New thread, in which old logs are deleted (used internally)
type
  TKMOldLogsDeleter = class(TThread)
  private
    fPathToLogs: UnicodeString;
  public
    constructor Create(const aPathToLogs: UnicodeString);
    procedure Execute; override;
  end;


{ TKMOldLogsDeleter }
constructor TKMOldLogsDeleter.Create(const aPathToLogs: UnicodeString);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  //Must set these values BEFORE starting the thread
  FreeOnTerminate := True; //object can be automatically removed after its termination
  fPathToLogs := aPathToLogs;
end;


procedure TKMOldLogsDeleter.Execute;
var
  SearchRec: TSearchRec;
  fileDateTime: TDateTime;
begin
  if not DirectoryExists(fPathToLogs) then Exit;

  if FindFirst(fPathToLogs + 'KaM*.log', faAnyFile - faDirectory, SearchRec) = 0 then
  repeat
    Assert(FileAge(fPathToLogs + SearchRec.Name, fileDateTime), 'How is that it does not exists any more?');

    if (Abs(Now - fileDateTime) > DEL_LOGS_OLDER_THAN) then
      DeleteFile(fPathToLogs + SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


//{ TKMLog }
//constructor TKMLog.Create(const aPath: UnicodeString);
//begin
//  inherited Create;
//  fLogPath := aPath;
//  fFirstTick := TimeGet;
//  fPreviousTick := TimeGet;
//  ForceDirectories(ExtractFilePath((aPath)));
//
//  AssignFile(fl, fLogPath);
//  Rewrite(fl);
//  //           hh:nn:ss.zzz 12345.678s 1234567ms     text-text-text
//  WriteLn(fl, '   Timestamp    Elapsed     Delta     Description');
//  CloseFile(fl);
//
//  AddLineTime('Log is up and running. Game version: ' + GAME_VERSION);
//end;
//
//
////Run thread to delete old logs.
//procedure TKMLog.DeleteOldLogs;
//begin
//  if Self = nil then Exit;
//
//  //No need to remember the instance, it's set to FreeOnTerminate
//  TKMOldLogsDeleter.Create(ExtractFilePath(fLogPath));
//end;
//
//
////Lines are timestamped, each line invokes file open/close for writing,
////meaning that no lines will be lost if Remake crashes
//procedure TKMLog.AddLineTime(const aText: UnicodeString);
//begin
//  AssignFile(fl, fLogPath);
//  Append(fl);
//  //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
//  if Abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
//  begin
//    WriteLn(fl, '========================');
//    WriteLn(fl, '    Date: ' + FormatDateTime('yyyy/mm/dd', Now));
//    WriteLn(fl, '========================');
//  end;
//  WriteLn(fl, Format('%12s %9.3fs %7dms     %s', [
//                FormatDateTime('hh:nn:ss.zzz', Now),
//                GetTimeSince(fFirstTick) / 1000,
//                GetTimeSince(fPreviousTick),
//                aText]));
//  CloseFile(fl);
//  fPreviousTick := TimeGet;
//  fPreviousDate := Now;
//end;
//
//
//{Same line but without timestamp}
//procedure TKMLog.AddLineNoTime(const aText: UnicodeString);
//begin
//  AssignFile(fl, fLogPath);
//  Append(fl);
//  WriteLn(fl, '                                      ' + aText);
//  CloseFile(fl);
//end;
//
//
//procedure TKMLog.AddTime(const aText: UnicodeString);
//begin
//  if Self = nil then Exit;
//
//  AddLineTime(aText);
//end;
//
//
//procedure TKMLog.AddTime(const aText: UnicodeString; num: integer);
//begin
//  if Self = nil then Exit;
//
//  AddLineTime(aText + ' ' + inttostr(num));
//end;
//
//
//procedure TKMLog.AddTime(const aText: UnicodeString; num: single);
//begin
//  if Self = nil then Exit;
//
//  AddLineTime(aText + ' ' + floattostr(num));
//end;
//
//
//procedure TKMLog.AddTime(num: integer; const aText: UnicodeString);
//begin
//  if Self = nil then Exit;
//
//  AddLineTime(inttostr(num) + ' ' + aText);
//end;
//
//
//procedure TKMLog.AddTime(const aText: UnicodeString; Res: boolean);
//var
//  s: UnicodeString;
//begin
//  if Self = nil then Exit;
//
//  if Res then
//    s := 'done'
//  else
//    s := 'fail';
//  AddLineTime(aText + ' ... ' + s);
//end;
//
//
//procedure TKMLog.AddTime(A, B: integer);
//begin
//  if Self = nil then Exit;
//
//  AddLineTime(inttostr(A) + ' : ' + inttostr(B));
//end;
//
//
//procedure TKMLog.AddAssert(const aMessageText: UnicodeString);
//begin
//  if Self = nil then Exit;
//
//  AddLineNoTime('ASSERTION FAILED! Msg: ' + aMessageText);
//  Assert(False, 'ASSERTION FAILED! Msg: ' + aMessageText);
//end;
//
//
//procedure TKMLog.AddNoTime(const aText: UnicodeString);
//begin
//  if Self = nil then Exit;
//
//  AddLineNoTime(aText);
//end;


{TKMLogFileAppender}
procedure TKMLogFileAppender.SetOption(const Name: string; const Value: string);
begin
  if Name <> FileNameOpt then
    inherited SetOption(Name, Value)
  else
    inherited SetOption(FileNameOpt, GetLogPath);
end;


{TKMLogLayout}
procedure TKMLogLayout.Init;
begin
  fFirstTick := TimeGet;
  fPreviousTick := TimeGet;
end;


function TKMLogLayout.GetHeader: string;
begin
  Result := '   Timestamp    Elapsed     Delta    Category                    Level    Description' + sLineBreak;
end;


procedure TKMLogLayout.SetOption(const Name, Value: string);
begin
  // To prevent some NPE in Log4d
  if Self = nil then Exit;
end;


function TKMLogLayout.Format(const Event: TLogEvent): string;
begin
  Result := '';
  //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
  if Abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
  begin
    Result := Result + '========================' + sLineBreak;
    Result := Result + '    Date: ' + FormatDateTime('yyyy/mm/dd', Now) + sLineBreak;
    Result := Result + '========================' + sLineBreak;
  end;

  if (Event.Level.Name = NO_TIME_LOG_LVL_NAME) then
  begin
    Result := Result +
      '                                                                          ' +
      Event.Message + sLineBreak;
  end else if (Event.Level.Name = ASSERT_LOG_LVL_NAME) then
  begin
    Result := Result +
      '                                                                 warn    ASSERTION FAILED! Msg: ' +
      Event.Message + sLineBreak;
    Assert(False, 'ASSERTION FAILED! Msg: ' + Event.Message);
  end else begin
    Result := Result +
      SysUtils.Format('%12s %9.3fs %7dms    %-27s %-8s %s', [
      FormatDateTime('hh:nn:ss.zzz', Now),
      GetTimeSince(fFirstTick) / 1000,
      GetTimeSince(fPreviousTick),
      Event.LoggerName,
      Event.Level.Name,
      Event.Message]) + sLineBreak;
  end;

  fPreviousTick := TimeGet;
  fPreviousDate := Now;
end;


{TKMInfoNoTimeLogLevel}
constructor TKMInfoNoTimeLogLevel.Create;
begin
  inherited Create(NO_TIME_LOG_LVL_NAME, InfoValue + 100);
end;


{TKMWarnAssertLogLevel}
constructor TKMWarnAssertLogLevel.Create;
begin
  inherited Create(ASSERT_LOG_LVL_NAME, WarnValue + 100);
end;



{log static functions}
function GetLogger(aClass: TClass; aCategory: string = ''): TLogLogger;
begin
  if (aCategory <> '') then
    Result := TLogLogger.GetLogger(aCategory + '.' + aClass.ClassName)
  else
    Result := TLogLogger.GetLogger(aClass.ClassName);
end;


function GetNetLogger(aClass: TClass): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_NET_CATEGORY);
end;


function GetDeliveryLogger(aClass: TClass): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_DELIVERY_CATEGORY);
end;


function GetNoTimeLogLvl: TLogLevel;
begin
  Result := TLogLevel.GetLevel(NO_TIME_LOG_LVL_NAME);
end;


function GetAssertLogLvl: TLogLevel;
begin
  Result := TLogLevel.GetLevel(ASSERT_LOG_LVL_NAME);
end;


procedure DeleteOldLogs;
begin
  TKMOldLogsDeleter.Create(ExtractFilePath(GetLogPath));
end;


function GetLogPath:string;
begin
  if fLogPath.IsEmpty then
    fLogPath := GetExeDir + 'Logs' + PathDelim + 'KaM_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log';
  Result := fLogPath;
end;


initialization
  // Register Custom Logger classes
  RegisterLayout(TKMLogLayout);
  RegisterAppender(TKMLogFileAppender);

  TKMInfoNoTimeLogLevel.Create;
  TKMWarnAssertLogLevel.Create;

end.
