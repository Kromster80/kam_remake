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

var
  NoTimeLogLvl: TLogLevel;
  AssertLogLvl: TLogLevel;

type
  TKMLogUtils = class
    private
      class var fLogPath: string;
    public
      class procedure DeleteOldLogs;
      class function GetLogPath:string;
  end;

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


{TKMLogUtils}
class procedure TKMLogUtils.DeleteOldLogs;
begin
  TKMOldLogsDeleter.Create(ExtractFilePath(GetLogPath));
end;


class function TKMLogUtils.GetLogPath:string;
begin
  if fLogPath.IsEmpty then
    fLogPath := GetExeDir + 'Logs' + PathDelim + 'KaM_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log';
  Result := fLogPath;
end;


{TKMLogFileAppender}
procedure TKMLogFileAppender.SetOption(const Name: string; const Value: string);
begin
  // Did not find better solution, then this
  if Name <> FileNameOpt then
    inherited SetOption(Name, Value)
  else
    inherited SetOption(FileNameOpt, TKMLogUtils.GetLogPath);
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


initialization
  // Register Custom Logger classes
  RegisterLayout(TKMLogLayout);
  RegisterAppender(TKMLogFileAppender);

  NoTimeLogLvl := TKMInfoNoTimeLogLevel.Create;
  AssertLogLvl := TKMWarnAssertLogLevel.Create;

end.
