unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, StrUtils, Classes, KM_Utils, Log4d, Forms,
  {$IFDEF LINUX}
    SyncObjs
  {$ELSE}
    Windows
  {$ENDIF};

const
  LOG_NET_CATEGORY = 'net';
  LOG_DELIVERY_CATEGORY = 'delivery';

  NO_TIME_LOG_LVL_NAME = 'NoTime';
  ASSERT_LOG_LVL_NAME = 'Assert';

type

  TKMLog = class
  private
    class var fLogPath: UnicodeString;
    // Get Logger for specified class and category
    // If No Class specified Category logger will returned
    // If No Category specified Class logger will returnes
    // If neither Class nor Category is specified return RootLogger
    function GetLogger(aClass: TClass = nil; aCategory: string = ''): TLogLogger;
    // Get NET category Logger for specified class
    function GetNetLogger(aClass: TClass = nil): TLogLogger;
    // Get DELIVERY category Logger for specified class
    function GetDeliveryLogger(aClass: TClass = nil): TLogLogger;
    procedure Init(aLogPath: UnicodeString);
  public
    constructor Create(aLogPath: UnicodeString); overload;
    constructor Create(aPropsPath, aLogPath: UnicodeString); overload;
    destructor Destroy;
    // pass-through Logging to RootLogger
    procedure Trace(const aMessage: string; const aErr: Exception = nil);
    procedure Debug(const aMessage: string; const aErr: Exception = nil);
    procedure Info(const aMessage: string; const aErr: Exception = nil);
    procedure Warn(const aMessage: string; const aErr: Exception = nil);
    procedure Error(const aMessage: string; const aErr: Exception = nil);
    procedure Fatal(const aMessage: string; const aErr: Exception = nil);
    procedure NoTime(const aMessage: string; const aErr: Exception = nil);
    procedure LogAndAssert(const aMessage: string; const aErr: Exception = nil);

    class property LogPath: UnicodeString read fLogPath;
    // Custom loggers:
    // Root Logger
    function Root: TLogLogger;
    // Net logger
    function Net(aClass: TClass = nil): TLogLogger;
    // Delivery logger
    function Delivery(aClass: TClass = nil): TLogLogger;

    procedure DeleteOldLogs;
  end;


  TKMLogFileAppender = class(TLogCustomAppender)
  private
    fl: textfile;
    fLogPath: UnicodeString;
    procedure InternalInit;
  protected
    procedure DoAppend(const Message: string); override;
    procedure SetOption(const Name, Value: string); override;
  end;

  // Layot to write log messages
  TKMLogLayout = class(TLogCustomLayout)
  private
    fFirstTick: Cardinal;
    fPreviousTick: Cardinal;
    fPreviousDate: TDateTime;
  protected
    procedure SetOption(const Name, Value: string); override;
  public
    procedure Init; override;
    function Format(const Event: TLogEvent): string; override;
  end;

  // Special log level to write log without time
  TKMInfoNoTime = class(TLogLevel)
  public
    constructor Create;
  end;

  // Special log level to write Assert error messages
  TKMWarnAssert = class(TLogLevel)
  public
    constructor Create;
  end;

var
  gLog: TKMLog;


implementation
uses
  KM_Defaults;

const
  InitOpt = 'init';
var
  NoTimeLogLvl: TLogLevel;
  AssertLogLvl: TLogLevel;

//New thread, in which old logs are deleted (used internally)
type
  TKMOldLogsDeleter = class(TThread)
  private
    fPathToLogs: UnicodeString;
  public
    constructor Create(const aPathToLogs: UnicodeString);
    procedure Execute; override;
  end;


{TKMLog}
constructor TKMLog.Create(aLogPath: UnicodeString);
begin
  Init(aLogPath);
  //Load Basic Logger configuration
  TLogBasicConfigurator.Configure;
end;


constructor TKMLog.Create(aPropsPath, aLogPath: UnicodeString);
begin
  Init(aLogPath);
  //Load Logger configuration from file
  TLogPropertyConfigurator.Configure(aPropsPath);
end;

procedure TKMLog.Init(aLogPath: UnicodeString);
begin
  RegisterLayout(TKMLogLayout);
  RegisterAppender(TKMLogFileAppender);

  NoTimeLogLvl := TKMInfoNoTime.Create;
  AssertLogLvl := TKMWarnAssert.Create;
  //Save Logs Path for use while Configure process
  TKMLog.fLogPath := aLogPath;
end;


destructor TKMLog.Destroy;
begin
  FreeAndNil(NoTimeLogLvl);
  FreeAndNil(AssertLogLvl);
  TKMLog.fLogPath := '';
end;


function TKMLog.GetLogger(aClass: TClass = nil; aCategory: string = ''): TLogLogger;
var LoggerName: string;
begin
  if aClass = nil then
  begin
    if aCategory = '' then
    begin
      Result := TLogLogger.GetRootLogger;
      Exit;
    end else
      LoggerName := aCategory;
  end else
    LoggerName := IfThen(aCategory = '', aClass.ClassName, aCategory + '.' + aClass.ClassName);
  Result := TLogLogger.GetLogger(LoggerName);
end;


function TKMLog.Root: TLogLogger;
begin
  Result := TLogLogger.GetRootLogger;
end;


function TKMLog.GetNetLogger(aClass: TClass = nil): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_NET_CATEGORY);
end;


function TKMLog.GetDeliveryLogger(aClass: TClass = nil): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_DELIVERY_CATEGORY);
end;


procedure TKMLog.Trace(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Trace(aMessage, aErr);
end;


procedure TKMLog.Debug(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Debug(aMessage, aErr);
end;


procedure TKMLog.Info(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Info(aMessage, aErr);
end;


procedure TKMLog.Warn(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Warn(aMessage, aErr);
end;


procedure TKMLog.Error(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Error(aMessage, aErr);
end;


procedure TKMLog.Fatal(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Fatal(aMessage, aErr);
end;


procedure TKMLog.NoTime(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Log(NoTimeLogLvl, aMessage, aErr);
end;


procedure TKMLog.LogAndAssert(const aMessage: string; const aErr: Exception = nil);
begin
  Root.Log(AssertLogLvl, aMessage, aErr);
end;


function TKMLog.Net(aClass: TClass = nil): TLogLogger;
begin
  Result := GetNetLogger(aClass);
end;


function TKMLog.Delivery(aClass: TClass = nil): TLogLogger;
begin
  Result := GetDeliveryLogger(aClass);
end;


procedure TKMLog.DeleteOldLogs;
begin
  TKMOldLogsDeleter.Create(fLogPath);
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
  SearchPattern: string;
begin
  if not DirectoryExists(fPathToLogs) then Exit;

  if FindFirst(fPathToLogs + 'KaM*.log', faAnyFile - faDirectory, SearchRec) = 0 then
  repeat
    Assert(FileAge(fPathToLogs + SearchRec.Name, fileDateTime), 'How is that it does not exists any more?');

    if (Abs(Now - fileDateTime) > DEL_LOGS_OLDER_THAN) then
      SysUtils.DeleteFile(fPathToLogs + SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  SysUtils.FindClose(SearchRec);
end;


{TKMLogFileAppender}
// parse appender options
procedure TKMLogFileAppender.SetOption(const Name: string; const Value: string);
begin
  inherited SetOption(Name, Value);
  EnterCriticalSection(FCriticalAppender);
  try
    if (Name = InitOpt) then
    begin
      InternalInit;
    end;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;


procedure TKMLogFileAppender.InternalInit;
begin
  // Get previously initialized LogPath
  fLogPath := TKMLog.fLogPath;
  ForceDirectories(ExtractFilePath(fLogPath));
  AssignFile(fl, fLogPath);
  Rewrite(fl);
  Writeln(fl, '                Log is up and running. Game version: ' + GAME_VERSION);
  Writeln(fl, '   Timestamp    Elapsed     Delta    Category                    Level    Description');
  CloseFile(fl);
end;


procedure TKMLogFileAppender.DoAppend(const Message: string);
begin
  if fLogPath = '' then Exit;  // Appender has been not initialized yet
  AssignFile(fl, fLogPath);
  System.Append(fl);
  Write(fl, Message);
  // Close file every time we write in it to avoid any loss of log messages
  CloseFile(fl);
end;


{TKMLogLayout}
procedure TKMLogLayout.Init;
begin
  fFirstTick := TimeGet;
  fPreviousTick := TimeGet;
end;


procedure TKMLogLayout.SetOption(const Name, Value: string);
begin
  // To prevent errors in Log4d
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


{TKMInfoNoTime}
constructor TKMInfoNoTime.Create;
begin
  inherited Create(NO_TIME_LOG_LVL_NAME, InfoValue + 100);
end;


{TKMWarnAssert}
constructor TKMWarnAssert.Create;
begin
  inherited Create(ASSERT_LOG_LVL_NAME, WarnValue + 100);
end;


end.
