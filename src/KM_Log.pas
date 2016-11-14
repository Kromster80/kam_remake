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

  // Get executable file directory
  function GetExeDir: string;

type

  TKMLog = class
  private
  public
    // Logging to RootLogger
    procedure Trace(const aMessage: string; const aErr: Exception = nil);
    procedure Debug(const aMessage: string; const aErr: Exception = nil);
    procedure Info(const aMessage: string; const aErr: Exception = nil);
    procedure Warn(const aMessage: string; const aErr: Exception = nil);
    procedure Error(const aMessage: string; const aErr: Exception = nil);
    procedure Fatal(const aMessage: string; const aErr: Exception = nil);
    procedure Log(const aLogLevel: TLogLevel; const aMessage: string; const aErr: Exception = nil);

    // Custom loggers:
    // RootLogger
    function Logger: TLogLogger;
    // Net logger
    function Net(aClass: TClass = nil): TLogLogger;
    // Delivery logger
    function Delivery(aClass: TClass = nil): TLogLogger;
  end;


  TKMLogUtils = class
  public
    // Get Logger for specified class and category
    // If No Class specified Category logger will returned
    // If No Category specified Class logger will returnes
    // If neither Class nor Category is specified return RootLogger
    class function GetLogger(aClass: TClass = nil; aCategory: string = ''): TLogLogger;
    // Get NET category Logger for specified class
    class function GetNetLogger(aClass: TClass = nil): TLogLogger;
    // Get DELIVERY category Logger for specified class
    class function GetDeliveryLogger(aClass: TClass = nil): TLogLogger;
  end;


  TKMLogInitializer = class
  private
    fLogPath: UnicodeString;
    fFileName: UnicodeString;
    fFileNamePrefix: UnicodeString;
    fPathToLogsDir: UnicodeString;
    fInitialized: Boolean;
  public
    constructor Create;
    procedure InitWPrefix(const aFileNamePrefix: UnicodeString);
    procedure InitWName(const aFileName: UnicodeString);
    procedure Init(const aLogPath: UnicodeString);
    procedure DeleteOldLogs;
    property LogPath: UnicodeString read fLogPath;
    property FileNamePrefix: UnicodeString read fFileNamePrefix;
    property PathToLogsDir: UnicodeString read fPathToLogsDir;
    property FileName: UnicodeString read fFileName;
    property IsInitialized: Boolean read fInitialized;
  end;

{ Log file appender
   	possible options:
			pathToLogsDir 	- Path to logs dir. Example: C:\Temp
			fileNamePrefix 	- File name prefix for log file. Suffix for file name is date-time in format yyyy-mm-dd_hh-nn-ss-zzz and .log extension
			fileName		- File name.
			layout			- Layout to format logging messages

	Option pathToLogsDir should be set BEFORE any of the options fileName or fileNamePrefix. Otherwise it will be ignored.

	If pathToLogsDir is not set, then Logs dir will be set to ExeDir\Logs, where ExeDir is the directory of executable file

	If both options fileNamePrefix and fileName are setted, then only first will be used.}
  TKMLogFileAppender = class(TLogCustomAppender)
  private
    fl: textfile;
    fLogPath: UnicodeString;
    procedure InternalInit;
  protected
    procedure DoAppend(const Message: string); override;
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const aName, aFileName, aFileNamePrefix, aPathToLogsDir: string; const aLayout: ILogLayout = nil); reintroduce; virtual;
    procedure InitLogFileWPrefix(const aFileNamePrefix: string); virtual;
    procedure InitLogFileWName(const aFileName: string); virtual;
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
  TKMInfoNoTimeLogLevel = class(TLogLevel)
  public
    constructor Create;
  end;

  // Special log level to write Assert error messages
  TKMWarnAssertLogLevel = class(TLogLevel)
  public
    constructor Create;
  end;

var
  NoTimeLogLvl: TLogLevel;
  AssertLogLvl: TLogLevel;
  gLogInitializer: TKMLogInitializer;   // log system initializer
  gLog: TKMLog;


implementation
uses
  KM_Defaults;

const
  FileNamePrefixOpt = 'fileNamePrefix';
  PathToLogsDirOpt = 'pathToLogsDir';

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
procedure TKMLog.Trace(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Trace(aMessage, aErr);
end;


procedure TKMLog.Debug(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Debug(aMessage, aErr);
end;


procedure TKMLog.Info(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Info(aMessage, aErr);
end;


procedure TKMLog.Warn(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Warn(aMessage, aErr);
end;


procedure TKMLog.Error(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Error(aMessage, aErr);
end;


procedure TKMLog.Fatal(const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Fatal(aMessage, aErr);
end;


procedure TKMLog.Log(const aLogLevel: TLogLevel; const aMessage: string; const aErr: Exception = nil);
begin
  TKMLogUtils.GetLogger.Log(aLogLevel, aMessage, aErr);
end;


function TKMLog.Logger: TLogLogger;
begin
  Result := TKMLogUtils.GetLogger;
end;


function TKMLog.Net(aClass: TClass = nil): TLogLogger;
begin
  Result := TKMLogUtils.GetNetLogger(aClass);
end;


function TKMLog.Delivery(aClass: TClass = nil): TLogLogger;
begin
  Result := TKMLogUtils.GetDeliveryLogger(aClass);
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

  // Do not delete old logs if Log file was specified by direct fileName option
  if not gLogInitializer.FileNamePrefix.IsEmpty then
  begin
    if FindFirst(fPathToLogs + gLogInitializer.FileNamePrefix + '*.log', faAnyFile - faDirectory, SearchRec) = 0 then
    repeat
      Assert(FileAge(fPathToLogs + SearchRec.Name, fileDateTime), 'How is that it does not exists any more?');

      if (Abs(Now - fileDateTime) > DEL_LOGS_OLDER_THAN) then
        SysUtils.DeleteFile(fPathToLogs + SearchRec.Name);
    until (FindNext(SearchRec) <> 0);
    SysUtils.FindClose(SearchRec);
  end;
end;


{TKMLogInitializer}
constructor TKMLogInitializer.Create;
begin
  inherited Create;
  fInitialized := False;
end;


// Logs initialization with FileNamePrefix
procedure TKMLogInitializer.InitWPrefix(const aFileNamePrefix: UnicodeString);
var PathToLogsDir: string;
begin
  if fInitialized then Exit;  // Logs are initialized only once;
  Assert(not aFileNamePrefix.IsEmpty(), 'Error: empty parameter "' + FileNamePrefixOpt + '" of TKMLogFileAppender');
  fFileNamePrefix := aFileNamePrefix;
  InitWName(aFileNamePrefix + FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log');
end;


// Logs initialization with FileNamePrefix
procedure TKMLogInitializer.InitWName(const aFileName: UnicodeString);
begin
  if fInitialized then Exit;  // Logs are initialized only once;
  Assert(not aFileName.IsEmpty(), 'Error: fileName is empty for TKMLogFileAppender');
  // Check if fPathToLogsDir is Set. If not - use default value
  if fPathToLogsDir.IsEmpty then
    fPathToLogsDir := GetExeDir + 'Logs';   // Default dir for Logs
  Init(fPathToLogsDir + PathDelim + aFileName);
end;


//Logs initialization with full path to log file
procedure TKMLogInitializer.Init(const aLogPath: UnicodeString);
begin
  if fInitialized then Exit;  // Logs are initialized only once;
  fLogPath := aLogPath;
  fInitialized := True;
end;


procedure TKMLogInitializer.DeleteOldLogs;
begin
  if not fInitialized then Exit;
  TKMOldLogsDeleter.Create(PathToLogsDir);
end;


{TKMLogFileAppender}
constructor TKMLogFileAppender.Create(const aName, aFileName, aFileNamePrefix, aPathToLogsDir: string; const aLayout: ILogLayout = nil);
begin
  inherited Create(aName, aLayout);
  SetOption(PathToLogsDirOpt, aPathToLogsDir);
  SetOption(FileNamePrefixOpt, aFileNamePrefix);
  SetOption(FileNameOpt, aFileName);
end;

// parse appender options
procedure TKMLogFileAppender.SetOption(const Name: string; const Value: string);
begin
  inherited SetOption(Name, Value);
  EnterCriticalSection(FCriticalAppender);
  try
    if (Value <> '') then
    begin
      if (Name = PathToLogsDirOpt) then
        gLogInitializer.fPathToLogsDir := Value
      else if (Name = FileNamePrefixOpt) then
        InitLogFileWPrefix(Value)
      else if (Name = FileNameOpt) then
        InitLogFileWName(Value);
    end;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;

//Init appender with filename prefix
procedure TKMLogFileAppender.InitLogFileWPrefix(const aFileNamePrefix: UnicodeString);
var
  strPath: string;
  f : TextFile;
begin
  if gLogInitializer.IsInitialized then Exit;

  gLogInitializer.InitWPrefix(aFileNamePrefix); // Init log system with file name prefix
  InternalInit;
end;

//Init appender with filename
procedure TKMLogFileAppender.InitLogFileWName(const aFileName: UnicodeString);
var
  strPath: string;
  f : TextFile;
begin
  if gLogInitializer.IsInitialized then Exit;

  gLogInitializer.InitWName(aFileName);   // Init log system with file name
  InternalInit;
end;


procedure TKMLogFileAppender.InternalInit;
begin
  fLogPath := gLogInitializer.LogPath;
  ForceDirectories(gLogInitializer.PathToLogsDir);
  AssignFile(fl, fLogPath);
  Rewrite(fl);
  Writeln(fl, '                Log is up and running. Game version: ' + GAME_VERSION);
  Writeln(fl, '   Timestamp    Elapsed     Delta    Category                    Level    Description');
  CloseFile(fl);
end;


procedure TKMLogFileAppender.DoAppend(const Message: string);
begin
  if fLogPath.IsEmpty then Exit;  // Appender has been not initialized yet
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


{TKMLogUtils}
class function TKMLogUtils.GetLogger(aClass: TClass = nil; aCategory: string = ''): TLogLogger;
var LoggerName: string;
begin
  if aClass = nil then
  begin
    if aCategory.IsEmpty then
    begin
      Result := TLogLogger.GetRootLogger;
      Exit;
    end else
      LoggerName := aCategory;
  end else
    LoggerName := IfThen(aCategory.IsEmpty, aClass.ClassName, aCategory + '.' + aClass.ClassName);
  Result := TLogLogger.GetLogger(LoggerName);
end;


class function TKMLogUtils.GetNetLogger(aClass: TClass = nil): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_NET_CATEGORY);
end;


class function TKMLogUtils.GetDeliveryLogger(aClass: TClass = nil): TLogLogger;
begin
  Result := GetLogger(aClass, LOG_DELIVERY_CATEGORY);
end;


function GetExeDir: string;
begin
  if (ExeDir.IsEmpty) then
    ExeDir := ExtractFilePath(Application.ExeName);
  Result := ExeDir;
end;


initialization
  // Register Custom Logger classes
  // These objects will be freed after Log4d destroy, so no need for manual FreeAndNil
  RegisterLayout(TKMLogLayout);
  RegisterAppender(TKMLogFileAppender);

  NoTimeLogLvl := TKMInfoNoTimeLogLevel.Create;
  AssertLogLvl := TKMWarnAssertLogLevel.Create;

end.
