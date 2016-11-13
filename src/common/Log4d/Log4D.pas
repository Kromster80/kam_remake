unit Log4D;

{
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at https://www.mozilla.org/MPL/1.1/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  The Original Code is Log4D.pas.

  The Initial Developer of the Original Code is Keith Wood.
  All Rights Reserved.

  Contributor(s): adasen, mhoenemann, aweber, ahesse, michaelJustin, svenHarazim.
}

{  
  Logging for Delphi.
  Based on log4j Java package from Apache
  (http://jakarta.apache.org/log4j/docs/index.html).
  Currently based on log4j 1.2.12

  Written by Keith Wood (kbwood@iprimus.com.au).
  Version 1.0 - 29 April 2001.
  Version 1.2 - 9 September 2003.
  Version 1.3 - 24 July 2004.
  Version 1.2.12 - 6 October 2009

  changes by adasen:
  - added threshold option to TLogCustomAppender (as in SkeletonAppender.java)
  - use fmShareDenyWrite instead of fmShareExclusive in TLogFileAppender
  - added TLogRollingFileAppender (as in RollingFileAppender.java)

  changes by mhoenemann:
  - reopen a newly created file stream in TLogFileAppender to get
    fmShareDenyWrite on a new logfile (due to a bug in SysUtils.FileCreate())

  changes by aweber:
  - changed TLogRollingFileAppender.RollOver from protected to public
    so that it can be called on purpose (as in Java)
  - added ILogRollingFileAppender in order to be able to use RollOver
  - moved all methods of TLogCustomAppender from private to protected in order
    to be able to subclass it properly

  changes by ahesse:
  - add jedi.inc for compiler version switches
  - add trace methods like in log4j 1.2.12
  - change version back to 1.2.12 to reflect log4j version
  - add Encoding to TLogCustomAppender
  - make TLogLevel.Create() public to add User defined Log Levels
  - TLogLogger.IsEnabledFor() must use the same logic as TLogLogger.Log()

}

interface

{$DEFINE HAS_UNIT_CONTNRS}

uses
  Classes,
{$IFDEF LINUX}
  SyncObjs,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF HAS_UNIT_CONTNRS}
  Contnrs,
{$ENDIF}
  SysUtils;

const
  Log4DVersion = '1.2.12';

  { Default pattern string for log output.
    Shows the application supplied message. }
  DefaultPattern = '%m%n';
  { A conversion pattern equivalent to the TTCC layout.
    Shows runtime, thread, level, logger, NDC, and message. }
  TTCCPattern    = '%r [%t] %p %c %x - %m%n';

  { Common prefix for option names in an initialisation file.
    Note that the search for all option names is case sensitive. }
  KeyPrefix        = 'log4d';
  { Specify the additivity of a logger's appenders. }
  AdditiveKey      = KeyPrefix + '.additive.';
  { Define a named appender. }
  AppenderKey      = KeyPrefix + '.appender.';
  { Nominate a factory to use to generate loggers.
    This factory must have been registered with RegisterLoggerFactory.
    If none is specified, then the default factory is used. }
  LoggerFactoryKey = KeyPrefix + '.loggerFactory';
  { Define a new logger, and set its logging level and appenders. }
  LoggerKey        = KeyPrefix + '.logger.';
  { Defining this value as true makes log4d print internal debug
    statements to debug output. }
  DebugKey         = KeyPrefix + '.configDebug';
  { Specify the error handler to be used with an appender. }
  ErrorHandlerKey  = '.errorHandler';
  { Specify the filters to be used with an appender. }
  FilterKey        = '.filter';
  { Specify the layout to be used with an appender. }
  LayoutKey        = '.layout';
  { Associate an object renderer with the class to be rendered. }
  RendererKey      = KeyPrefix + '.renderer.';
  { Set the logging level and appenders for the root. }
  RootLoggerKey    = KeyPrefix + '.rootLogger';
  { Set the overall logging level. }
  ThresholdKey     = KeyPrefix + '.threshold';

  { Special level value signifying inherited behaviour. }
  InheritedLevel = 'inherited';

  { Threshold option for TLogCustomAppender. }
  ThresholdOpt      = 'threshold';
  { Encoding option for TLogCustomAppender. }
  EncodingOpt = 'encoding';
  { Accept option for TLog*Filter. }
  AcceptMatchOpt = 'acceptOnMatch';
  { Appending option for TLogFileAppender. }
  AppendOpt      = 'append';
  { Common date format option for layouts. }
  DateFormatOpt  = 'dateFormat';
  { File name option for TLogFileAppender. }
  FileNameOpt    = 'fileName';
  { Case-sensitivity option for TLogStringFilter. }
  IgnoreCaseOpt  = 'ignoreCase';
  { Match string option for TLogLevelMatchFilter and TLogStringFilter. }
  MatchOpt       = 'match';
  { Maximum string option for TLogLevelRangeFilter. }
  MaxOpt         = 'maximum';
  { Minimum string option for TLogLevelRangeFilter. }
  MinOpt         = 'minimum';
  { Pattern option for TLogPatternLayout. }
  PatternOpt     = 'pattern';
  { Title option for TLogHTMLLayout. }
  TitleOpt       = 'title';
  { Maximum file size option for TLogRollingFileAppender }
  MaxFileSizeOpt = 'maxFileSize';
  { Maximum number of backup files option for TLogRollingFileAppender }
  MaxBackupIndexOpt = 'maxBackupIndex';

  DEFAULT_MAX_FILE_SIZE = 10*1024*1024;
  DEFAULT_MAX_BACKUP_INDEX = 1;

type
{$IFDEF VER120}
  TClassList  = TList;
  TObjectList = TList;
{$ENDIF}

{$IFDEF LINUX}
  TRTLCriticalSection = TCriticalSection;
{$ENDIF}

  { Log-specific exceptions. }
  ELogException = class(Exception);

  { Allow for initialisation of a dynamically created object. }
  ILogDynamicCreate = interface(IUnknown)
    ['{287DAA34-3A9F-45C6-9417-1B0D4DFAC86C}']
    procedure Init;
  end;

  { Get/set arbitrary options on an object. }
  ILogOptionHandler = interface(ILogDynamicCreate)
    ['{AC1C0E30-2DBF-4C55-9C2E-9A0F1A3E4F58}']
    function GetOption(const Name: string): string;
    procedure SetOption(const Name, Value: string);
    property Options[const Name: string]: string read GetOption write SetOption;
  end;

  { Base class for handling options. }
  TLogOptionHandler = class(TInterfacedObject, ILogOptionHandler)
  private
    FOptions: TStringList;
  protected
    function GetOption(const Name: string): string; virtual;
    procedure SetOption(const Name, Value: string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Options[const Name: string]: string read GetOption write SetOption;
    procedure Init; virtual;
  end;

{ Levels ----------------------------------------------------------------------}

  { Levels of messages for logging.
    The Level property identifies increasing severity of messages.
    All those above or equal to a particular setting are logged. }
  TLogLevel = class(TObject)
  private
    FLevel: Integer;
    FName: string;
  public
    constructor Create(const Name: string; Level: Integer);
    property Level: Integer read FLevel;
    property Name: string read FName;
    function IsGreaterOrEqual(LogLevel: TLogLevel): Boolean;
    { Retrieve a level object given its level. }
    class function GetLevel(LogLevel: Integer): TLogLevel; overload;
    { Retrieve a level object given its level, or default if not valid. }
    class function GetLevel(LogLevel: Integer; DefaultLevel: TLogLevel): TLogLevel;
      overload;
    { Retrieve a level object given its name. }
    class function GetLevel(const Name: string): TLogLevel; overload;
    { Retrieve a level object given its name, or default if not valid. }
    class function GetLevel(const Name: string; DefaultLevel: TLogLevel): TLogLevel;
      overload;
  end;

const
  { Levels of logging as integer values. }
  OffValue   = High(Integer);
  FatalValue = 50000;
  ErrorValue = 40000;
  WarnValue  = 30000;
  InfoValue  = 20000;
  DebugValue = 10000;
  TraceValue =  5000;
  AllValue   = Low(Integer);

var
  { Standard levels are automatically created (in decreasing severity):
    Off, Fatal, Error, Warn, Info, Debug, All. }
  Off:   TLogLevel;
  Fatal: TLogLevel;
  Error: TLogLevel;
  Warn:  TLogLevel;
  Info:  TLogLevel;
  Debug: TLogLevel;
  Trace: TLogLevel;
  All:   TLogLevel;

{ NDC -------------------------------------------------------------------------}

type
  { Keep track of the nested diagnostic context (NDC). }
  TLogNDC = class(TObject)
  private
    class function GetNDCIndex: Integer;
    class function GetThreadId: string;
  public
    class procedure Clear;
    class procedure CloneStack(const Context: TStringList);
    class function GetDepth: Integer;
    class procedure Inherit(const Context: TStringList);
    class function Peek: string;
    class function Pop: string;
    class procedure Push(const Context: string);
    class procedure Remove;
    class procedure SetMaxDepth(const MaxDepth: Integer);
  end;

{ Events ----------------------------------------------------------------------}

  TLogLogger = class;

  { An event to be logged. }
  TLogEvent = class(TPersistent)
  private
    FError: Exception;
    FLevel: TLogLevel;
    FLogger: TLogLogger;
    FMessage: string;
    FTimeStamp: TDateTime;
    function GetElapsedTime: LongInt;
    function GetErrorClass: string;
    function GetErrorMessage: string;
    function GetLoggerName: string;
    function GetNDC: string;
    function GetThreadId: LongInt;
  public
    constructor Create(const Logger: TLogLogger;
      const Level: TLogLevel; const Message: string;
      const Err: Exception; const TimeStamp: TDateTime = 0); overload;
    constructor Create(const Logger: TLogLogger;
      const Level: TLogLevel; const Message: TObject;
      const Err: Exception; const TimeStamp: TDateTime = 0); overload;
    property ElapsedTime: LongInt read GetElapsedTime;
    property Error: Exception read FError;
    property ErrorClass: string read GetErrorClass;
    property ErrorMessage: string read GetErrorMessage;
    property Level: TLogLevel read FLevel;
    property LoggerName: string read GetLoggerName;
    property Message: string read FMessage;
    property NDC: string read GetNDC;
    property ThreadId: LongInt read GetThreadId;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

{ Logger factory --------------------------------------------------------------}

  { Factory for creating loggers. }
  ILogLoggerFactory = interface(IUnknown)
    ['{AEE5E86C-B708-45B2-BEAD-B370D71CAA2F}']
    function MakeNewLoggerInstance(const Name: string): TLogLogger;
  end;

  { Default implementation of a logger factory. }
  TLogDefaultLoggerFactory = class(TInterfacedObject, ILogLoggerFactory)
  public
    function MakeNewLoggerInstance(const Name: string): TLogLogger;
  end;

{ Loggers ---------------------------------------------------------------------}

  ILogAppender = interface;
  ILogRenderer = interface;
  TLogHierarchy = class;

  { This is the central class in the Log4D package. One of the distinctive
    features of Log4D is hierarchical loggers and their evaluation. }
  TLogLogger = class(TLogOptionHandler, ILogOptionHandler)
  private
    FAdditive: Boolean;
    FAppenders: TInterfaceList;
    FHierarchy: TLogHierarchy;
    FLevel: TLogLevel;
    FName: string;
    FParent: TLogLogger;
  protected
    FCriticalLogger: TRTLCriticalSection;
    procedure CallAppenders(const Event: TLogEvent);
    procedure CloseAllAppenders;
    function CountAppenders: Integer;
    procedure DoLog(const LogLevel: TLogLevel; const Message: string;
      const Err: Exception = nil); overload; virtual;
    procedure DoLog(const LogLevel: TLogLevel; const Message: TObject;
      const Err: Exception = nil); overload; virtual;
    function GetLevel: TLogLevel; virtual;
  public
    constructor Create(const Name: string); reintroduce;
    destructor Destroy; override;
    property Additive: Boolean read FAdditive write FAdditive;
    property Appenders: TInterfaceList read FAppenders;
    property Hierarchy: TLogHierarchy read FHierarchy write FHierarchy;
    property Level: TLogLevel read GetLevel write FLevel;
    property Name: string read FName;
    property Parent: TLogLogger read FParent;
    procedure AddAppender(const Appender: ILogAppender);
    procedure AssertLog(const Assertion: Boolean; const Message: string);
      overload;
    procedure AssertLog(const Assertion: Boolean; const Message: TObject);
      overload;
    procedure Debug(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Debug(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Debug(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
    procedure Error(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Error(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Error(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
    procedure Fatal(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Fatal(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Fatal(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
    function GetAppender(const Name: string): ILogAppender;
    class function GetLogger(const Clazz: TClass;
      const Factory: ILogLoggerFactory = nil): TLogLogger; overload;
    class function GetLogger(const Name: string;
      const Factory: ILogLoggerFactory = nil): TLogLogger; overload;
    class function GetRootLogger: TLogLogger;
    procedure Info(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Info(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Info(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
    function IsAppender(const Appender: ILogAppender): Boolean;
    function IsDebugEnabled: Boolean;
    function IsEnabledFor(const LogLevel: TLogLevel): Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsTraceEnabled: Boolean;
    procedure LockLogger;
    procedure Log(const LogLevel: TLogLevel; const Fmt: string; const Args: array of const;
      const Err: Exception = nil); overload;
    procedure Log(const LogLevel: TLogLevel; const Message: string;
      const Err: Exception = nil); overload;
    procedure Log(const LogLevel: TLogLevel; const Message: TObject;
      const Err: Exception = nil); overload;
    procedure RemoveAllAppenders;
    procedure RemoveAppender(const Appender: ILogAppender); overload;
    procedure RemoveAppender(const Name: string); overload;
    procedure Trace(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Trace(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Trace(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
    procedure UnlockLogger;
    procedure Warn(const Fmt: string; const Args: array of const; const Err: Exception = nil);
      overload; virtual;
    procedure Warn(const Message: string; const Err: Exception = nil);
      overload; virtual;
    procedure Warn(const Message: TObject; const Err: Exception = nil);
      overload; virtual;
  end;

  { The specialised root logger - cannot have a nil level. }
  TLogRoot = class(TLogLogger)
  private
    procedure SetLevel(const Val: TLogLevel);
  public
    constructor Create(const Level: TLogLevel);
    property Level: TLogLevel read GetLevel write SetLevel;
  end;

  { Specialised logger for internal logging. }
  TLogLog = class(TLogLogger)
  private
    FInternalDebugging: Boolean;
  protected
    procedure DoLog(const LogLevel: TLogLevel; const Message: string;
      const Err: Exception); override;
    procedure DoLog(const LogLevel: TLogLevel; const Message: TObject;
      const Err: Exception); override;
  public
    constructor Create;
    property InternalDebugging: Boolean read FInternalDebugging
      write FInternalDebugging;
  end;

{ Hierarchy -------------------------------------------------------------------}

  { Listen to events occuring within a hierarchy. }
  ILogHierarchyEventListener = interface
    ['{A216D50F-B9A5-4871-8EE3-CB55C41E138B}']
    procedure AddAppenderEvent(const Logger: TLogLogger;
      const Appender: ILogAppender);
    procedure RemoveAppenderEvent(const Logger: TLogLogger;
      const Appender: ILogAppender);
  end;

  { This class is specialised in retreiving loggers by name and
    also maintaining the logger hierarchy.

    The casual user should not have to deal with this class directly.

    The structure of the logger hierachy is maintained by the GetInstance
    method. The hierarchy is such that children link to their parent but
    parents do not have any pointers to their children. Moreover, loggers
    can be instantiated in any order, in particular descendant before ancestor.

    In case a descendant is created before a particular ancestor, then it creates
    an empty node for the ancestor and adds itself to it. Other descendants
    of the same ancestor add themselves to the previously created node. }
  TLogHierarchy = class(TObject)
  private
    FEmittedNoAppenderWarning: Boolean;
    FListeners: TInterfaceList;
    FLoggers: TStringList;
    FRenderedClasses: TClassList;
    FRenderers: TInterfaceList;
    FRoot: TLogLogger;
    FThreshold: TLogLevel;
    procedure SetThresholdProp(const Level: TLogLevel);
    procedure UpdateParent(const Logger: TLogLogger);
  protected
    FCriticalHierarchy: TRTLCriticalSection;
  public
    constructor Create(Root: TLogLogger);
    destructor Destroy; override;
    property Root: TLogLogger read FRoot;
    property Threshold: TLogLevel read FThreshold write SetThresholdProp;
    procedure AddHierarchyEventListener(
      const Listener: ILogHierarchyEventListener);
    procedure AddRenderer(RenderedClass: TClass; Renderer: ILogRenderer);
    procedure Clear;
    procedure EmitNoAppenderWarning(const Logger: TLogLogger);
    function Exists(const Name: string): TLogLogger;
    procedure FireAppenderEvent(const Adding: Boolean; const Logger: TLogLogger;
      const Appender: ILogAppender);
    procedure GetCurrentLoggers(const List: TStringList);
    function GetLogger(const Name: string;
      const Factory: ILogLoggerFactory = nil): TLogLogger;
    function GetRenderer(const RenderedClass: TClass): ILogRenderer;
    function IsDisabled(const LogLevel: Integer): Boolean;
    procedure RemoveHierarchyEventListener(
      const Listener: ILogHierarchyEventListener);
    procedure ResetConfiguration;
    procedure SetThreshold(const Name: string);
    procedure Shutdown;
  end;

{ Layouts ---------------------------------------------------------------------}

  { Functional requirements for a layout. }
  ILogLayout = interface(ILogOptionHandler)
    ['{87FDD680-96D7-45A0-A135-CB88ABAD5519}']
    function Format(const Event: TLogEvent): string;
    function GetContentType: string;
    function GetFooter: string;
    function GetHeader: string;
    function IgnoresException: Boolean;
    property ContentType: string read GetContentType;
    property Footer: string read GetFooter;
    property Header: string read GetHeader;
  end;

  { Abstract base for layouts.
    Subclasses must at least override Format.

    Accepts the following options:

    # Format for date and time stamps, string, optional, defaults to ShortDateFormat
    # See FormatDateTime function for more details
    log4d.appender.<name>.layout.dateFormat=yyyy/mm/dd hh:nn:ss.zzz
  }
  TLogCustomLayout = class(TLogOptionHandler, ILogDynamicCreate,
    ILogOptionHandler, ILogLayout)
  private
    FDateFormat: string;
  protected
    property DateFormat: string read FDateFormat write FDateFormat;
    function GetContentType: string; virtual;
    function GetHeader: string; virtual;
    function GetFooter: string; virtual;
    procedure SetOption(const Name, Value: string); override;
  public
    property ContentType: string read GetContentType;
    property Footer: string read GetFooter;
    property Header: string read GetHeader;
    function Format(const Event: TLogEvent): string; virtual; abstract;
    function IgnoresException: Boolean; virtual;
    procedure Init; override;
  end;

  { Basic implementation of a layout. }
  TLogSimpleLayout = class(TLogCustomLayout)
  public
    function Format(const Event: TLogEvent): string; override;
  end;

  { This layout outputs events in a HTML table.

    Accepts the following options:

    # Title for HTML page, string, optional
    log4d.appender.<name>.layout.title=Logging Messages
  }
  TLogHTMLLayout = class(TLogCustomLayout)
  private
    FTitle: string;
  protected
    function GetContentType: string; override;
    function GetFooter: string; override;
    function GetHeader: string; override;
    procedure SetOption(const Name, Value: string); override;
  public
    property Title: string read FTitle write FTitle;
    function Format(const Event: TLogEvent): string; override;
    function IgnoresException: Boolean; override;
  end;

  { Layout based on specified pattern.

    Accepts the following options:

    # Format for rendering the log event, string, optional, defaults to %m%n
    # See comments of Format method for more details
    log4d.appender.<name>.layout.pattern=%r [%t] %p %c %x - %m%n
  }
  TLogPatternLayout = class(TLogCustomLayout)
  private
    FPattern: string;
    FPatternParts: TStringList;
    procedure SetPattern(const Pattern: string);
  protected
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const Pattern: string = DefaultPattern); reintroduce;
    destructor Destroy; override;
    property Pattern: string read FPattern write SetPattern;
    function Format(const Event: TLogEvent): string; override;
    procedure Init; override;
  end;

{ Renderers -------------------------------------------------------------------}

  { Renderers transform an object into a string message for display. }
  ILogRenderer = interface(ILogOptionHandler)
    ['{169B03C6-E2C7-4F62-AD19-17408AB30681}']
    function Render(const Message: TObject): string;
  end;

  { Abstract base class for renderers - handles basic option setting.
    Subclasses must at least override Render. }
  TLogCustomRenderer = class(TLogOptionHandler, ILogDynamicCreate,
    ILogOptionHandler, ILogRenderer)
  public
    function Render(const Message: TObject): string; virtual; abstract;
  end;

{ ErrorHandler ----------------------------------------------------------------}

  { Appenders may delegate their error handling to ErrorHandlers.

    Error handling is a particularly tedious to get right because by
    definition errors are hard to predict and to reproduce. }
  ILogErrorHandler = interface(ILogOptionHandler)
    ['{B902C52A-5E4E-47A8-B291-BE8E7660F754}']
    procedure SetAppender(const Appender: ILogAppender);
    procedure SetBackupAppender(const BackupAppender: ILogAppender);
    procedure SetLogger(const Logger: TLogLogger);
    { The appender for which errors are handled. }
    property Appender: ILogAppender write SetAppender;
    { The appender to use in case of failure. }
    property BackupAppender: ILogAppender write SetBackupAppender;
    { The logger that the failing appender might be attached to. }
    property Logger: TLogLogger write SetLogger;
    { This method prints the error message passed as a parameter. }
    procedure Error(const Message: string); overload;
    { This method should handle the error. Information about the error
      condition is passed a parameter. }
    procedure Error(const Message: string; const Err: Exception;
      const ErrorCode: Integer; const Event: TLogEvent = nil); overload;
  end;

  { Abstract base class for error handlers - handles basic option setting.
    Subclasses must at least override Error. }
  TLogCustomErrorHandler = class(TLogOptionHandler, ILogDynamicCreate,
    ILogOptionHandler, ILogErrorHandler)
  private
    FAppender: ILogAppender;
    FBackupAppender: ILogAppender;
    FLogger: TLogLogger;
  protected
    procedure SetAppender(const Appender: ILogAppender); virtual;
    procedure SetBackupAppender(const BackupAppender: ILogAppender); virtual;
    procedure SetLogger(const Logger: TLogLogger); virtual;
  public
    property Appender: ILogAppender write SetAppender;
    property BackupAppender: ILogAppender write SetBackupAppender;
    property Logger: TLogLogger write SetLogger;
    procedure Error(const Message: string); overload; virtual; abstract;
    procedure Error(const Message: string; const Err: Exception;
      const ErrorCode: Integer; const Event: TLogEvent = nil); overload;
      virtual; abstract;
  end;

  { Fallback on an alternative appender if an error arises. }
  TLogFallbackErrorHandler = class(TLogCustomErrorHandler)
  private
    FLoggers: TObjectList;
  protected
    procedure SetLogger(const Logger: TLogLogger); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Error(const Message: string); overload; override;
    procedure Error(const Message: string; const Err: Exception;
      const ErrorCode: Integer; const Event: TLogEvent = nil); overload;
      override;
  end;

  { Displays only the first error sent to it to debugging output. }
  TLogOnlyOnceErrorHandler = class(TLogCustomErrorHandler)
  private
    FSeenError: Boolean;
  public
    procedure Error(const Message: string); overload; override;
    procedure Error(const Message: string; const Err: Exception;
      const ErrorCode: Integer; const Event: TLogEvent = nil); overload;
      override;
  end;

{ Filters ---------------------------------------------------------------------}

  TLogFilterDecision = (fdDeny, fdNeutral, fdAccept);

  { Filters can control to a finer degree of detail which messages get logged. }
  ILogFilter = interface(ILogOptionHandler)
    ['{B28213D7-ACE2-4C44-B820-D9437D44F8DA}']
    function Decide(const Event: TLogEvent): TLogFilterDecision;
  end;

  { Abstract base class for filters - handles basic option setting.
    Subclasses must at least override Decide.

    Accepts the following options:

    # Class identification
    log4d.appender.<name>.filter1=TLogCustomFilter
    # Accept or reject the log event when deciding, Boolean, optional, default true
    log4d.appender.<name>.filter1.acceptOnMatch=true
  }
  TLogCustomFilter = class(TLogOptionHandler, ILogDynamicCreate,
    ILogOptionHandler, ILogFilter)
  private
    FAcceptOnMatch: Boolean;
  protected
    property AcceptOnMatch: Boolean read FAcceptOnMatch write FAcceptOnMatch;
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const AcceptOnMatch: Boolean = True); reintroduce;
    function Decide(const Event: TLogEvent): TLogFilterDecision; virtual;
      abstract;
  end;

  { Deny all messages. }
  TLogDenyAllFilter = class(TLogCustomFilter)
  public
    property AcceptOnMatch;
    function Decide(const Event: TLogEvent): TLogFilterDecision; override;
  end;

  { Filter by the message's level.

    Accepts the following options (as well as the standard acceptOnMatch one):

    # Class identification
    log4d.appender.<name>.filter1=TLogLevelMatchFilter
    # Logging level to match on, Level, mandatory
    log4d.appender.<name>.filter1.match=warn
  }
  TLogLevelMatchFilter = class(TLogCustomFilter)
  private
    FMatchLevel: TLogLevel;
  protected
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const MatchLevel: TLogLevel;
      const AcceptOnMatch: Boolean = True); reintroduce;
    property AcceptOnMatch;
    property MatchLevel: TLogLevel read FMatchLevel write FMatchLevel;
    function Decide(const Event: TLogEvent): TLogFilterDecision; override;
  end;

  { Filter by the message's level being within a range.

    Accepts the following options (as well as the standard acceptOnMatch one):

    # Class identification
    log4d.appender.<name>.filter1=TLogLevelRangeFilter
    # Minimum logging level to match on, Level, mandatory
    log4d.appender.<name>.filter1.minimum=warn
    # Maximum logging level to match on, Level, mandatory
    log4d.appender.<name>.filter1.maximum=error
  }
  TLogLevelRangeFilter = class(TLogCustomFilter)
  private
    FMaxLevel: TLogLevel;
    FMinLevel: TLogLevel;
  protected
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const MaxLevel, MinLevel: TLogLevel;
      const AcceptOnMatch: Boolean = True); reintroduce;
    property AcceptOnMatch;
    property MaxLevel: TLogLevel read FMaxLevel write FMaxLevel;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    function Decide(const Event: TLogEvent): TLogFilterDecision; override;
  end;

  { Filter by text within the message.

    Accepts the following options (as well as the standard acceptOnMatch one):

    # Class identification
    log4d.appender.<name>.filter1=TLogStringFilter
    # Text to match on anywhere in message, string, mandatory
    log4d.appender.<name>.filter1.match=xyz
    # Whether match is case-sensitive, Boolean, optional, defaults to false
    log4d.appender.<name>.filter1.ignoreCase=true
  }
  TLogStringFilter = class(TLogCustomFilter)
  private
    FIgnoreCase: Boolean;
    FMatch: string;
  protected
    procedure SetOption(const Name, Value: string); override;
  public
    constructor Create(const Match: string; const IgnoreCase: Boolean = False;
      const AcceptOnMatch: Boolean = True); reintroduce;
    property AcceptOnMatch;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property Match: string read FMatch write FMatch;
    function Decide(const Event: TLogEvent): TLogFilterDecision; override;
  end;

{ Appenders -------------------------------------------------------------------}

  { Implement this interface for your own strategies
    for printing log statements. }
  ILogAppender = interface(ILogOptionHandler)
    ['{E1A06EA7-34CA-4DA4-9A8A-C76CF34257AC}']
    procedure AddFilter(const Filter: ILogFilter);
    procedure Append(const Event: TLogEvent);
    procedure Close;
    function GetErrorHandler: ILogErrorHandler;
    function GetFilters: TInterfaceList;
    function GetLayout: ILogLayout;
    function GetName: string;
    procedure RemoveAllFilters;
    procedure RemoveFilter(const Filter: ILogFilter);
    function RequiresLayout: Boolean;
    procedure SetErrorHandler(const ErrorHandler: ILogErrorHandler);
    procedure SetLayout(const Layout: ILogLayout);
    procedure SetName(const Name: string);
    property ErrorHandler: ILogErrorHandler read GetErrorHandler
      write SetErrorHandler;
    property Filters: TInterfaceList read GetFilters;
    property Layout: ILogLayout read GetLayout write SetLayout;
    property Name: string read GetName write SetName;
  end;

  { Basic implementation of an appender for printing log statements.
    Subclasses should at least override DoAppend(string). }
  TLogCustomAppender = class(TLogOptionHandler, ILogDynamicCreate,
    ILogOptionHandler, ILogAppender)
  private
    FClosed: Boolean;
    FErrorHandler: ILogErrorHandler;
    FFilters: TInterfaceList;
    FLayout: ILogLayout;
    FName: string;
    FThreshold : TLogLevel;
    {$IFDEF UNICODE}
    FEncoding: TEncoding;
    {$ENDIF UNICODE}
  protected
    FCriticalAppender: TRTLCriticalSection;
    function GetErrorHandler: ILogErrorHandler;
    function GetFilters: TInterfaceList;
    function GetLayout: ILogLayout;
    function GetName: string;
    {$IFDEF UNICODE}
    function GetEncoding: TEncoding;
    procedure SetEncoding(const Value: TEncoding);
    {$ENDIF UNICODE}
    procedure SetErrorHandler(const ErrorHandler: ILogErrorHandler);
    procedure SetLayout(const Layout: ILogLayout);
    procedure SetName(const Name: string);
    function CheckEntryConditions: Boolean; virtual;
    function CheckFilters(const Event: TLogEvent): Boolean; virtual;
    procedure DoAppend(const Event: TLogEvent); overload; virtual;
    procedure DoAppend(const Message: string); overload; virtual; abstract;
    procedure WriteFooter; virtual;
    procedure WriteHeader; virtual;
    procedure SetOption(const Name, Value: string); override;
    function isAsSevereAsThreshold(level : TLogLevel) : boolean;
  public
    constructor Create(const Name: string; const Layout: ILogLayout = nil);
      reintroduce; virtual;
    destructor Destroy; override;
    property ErrorHandler: ILogErrorHandler read GetErrorHandler write SetErrorHandler;
    property Filters: TInterfaceList read GetFilters;
    property Layout: ILogLayout read GetLayout write SetLayout;
    property Name: string read GetName write SetName;
    procedure AddFilter(const Filter: ILogFilter); virtual;
    procedure Append(const Event: TLogEvent); virtual;
    procedure Close; virtual;
    procedure Init; override;
    procedure RemoveAllFilters; virtual;
    procedure RemoveFilter(const Filter: ILogFilter); virtual;
    function RequiresLayout: Boolean; virtual;
    {$IFDEF UNICODE}
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    {$ENDIF UNICODE}
  end;

  { Discard log messages. }
  TLogNullAppender = class(TLogCustomAppender)
  protected
    procedure DoAppend(const Message: string); override;
  end;

  { Send log messages to debugging output. }
  TLogODSAppender = class(TLogCustomAppender)
  protected
    procedure DoAppend(const Message: string); override;
  end;

  { Send log messages to a stream. }
  TLogStreamAppender = class(TLogCustomAppender)
  private
    FStream: TStream;
  protected
    procedure DoAppend(const Message: string); override;
  public
    constructor Create(const Name: string; const Stream: TStream;
      const Layout: ILogLayout = nil); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { Send log messages to a file.

    Accepts the following options:

    # Class identification
    log4d.appender.<name>=TLogFileAppender
    # Name of the file to write to, string, mandatory
    log4d.appender.<name>.fileName=C:\Logs\App.log
    # Whether to append to file, Boolean, optional, defaults to true
    log4d.appender.<name>.append=false
  }
  TLogFileAppender = class(TLogStreamAppender)
  private
    FAppend: Boolean;
    FFileName: TFileName;
  protected
    procedure SetOption(const Name, Value: string); override;
    procedure SetLogFile(const Name: string); virtual;
    procedure CloseLogFile; virtual;
  public
    constructor Create(const Name, FileName: string;
      const Layout: ILogLayout = nil; const Append: Boolean = True);
      reintroduce; virtual;
    property FileName: TFileName read FFileName;
    property OpenAppend: Boolean read FAppend;
  end;

  { Implement this interface for your own strategies
    for printing log statements. }
  ILogRollingFileAppender = interface(ILogAppender)
    ['{49981B61-840B-440F-A444-BF11A91D1876}']
    procedure RollOver;
  end;

  { Send log messages to a file which uses logfile rotation

    Accepts the following options:

    # Class identification
    log4d.appender.<name>=TLogRollingFileAppender
    # Name of the file to write to, string, mandatory
    log4d.appender.<name>.fileName=C:\Logs\App.log
    # Whether to append to file, Boolean, optional, defaults to true
    log4d.appender.<name>.append=false
    # Max. file size accepts suffix "KB", "MB" and "GB", optional, default 10MB
    log4d.appender.<name>.maxFileSize=10MB
    # Max number of backup files, optional, default is 1
    log4d.appender.<name>.maxBackupIndex=3
  }
  TLogRollingFileAppender = class(TLogFileAppender, ILogRollingFileAppender)
  private
    FMaxFileSize : integer;
    FMaxBackupIndex : integer;
    FCurrentSize : integer;
  protected
    procedure SetOption(const Name, Value: string); override;
    procedure DoAppend(const msg: string); override;
  public
    procedure Init; override;
    procedure RollOver; virtual;        // just in case someone wants to override it...
    property MaxFileSize : integer read FMaxFileSize;
    property MaxBackupIndex : integer read FMaxBackupIndex;
  end;

{ Configurators ---------------------------------------------------------------}

  { Use this class to quickly configure the package. }
  TLogBasicConfigurator = class(TObject)
  private
    FRegistry: TStringList;
    FLoggerFactory: ILogLoggerFactory;
  protected
    { Used by subclasses to add a renderer
      to the hierarchy passed as parameter. }
    procedure AddRenderer(const Hierarchy: TLogHierarchy;
      const RenderedName, RendererName: string);
    function AppenderGet(const Name: string): ILogAppender;
    procedure AppenderPut(const Appender: ILogAppender);
    procedure SetGlobalProps(const Hierarchy: TLogHierarchy;
      const FactoryClassName, Debug, Threshold: string);
  public
    constructor Create;
    destructor Destroy; override;
    { Add appender to the root logger. If no appender is provided,
      add a TLogODSAppender that uses TLogPatternLayout with the
      TTCCPattern and prints to debugging output for the root logger. }
    class procedure Configure(const Appender: ILogAppender = nil);
    { Reset the default hierarchy to its default. It is equivalent to calling
      Logger.GetDefaultHierarchy.ResetConfiguration.
      See TLogHierarchy.ResetConfiguration for more details. }
    class procedure ResetConfiguration;
  end;

  { Extends BasicConfigurator to provide configuration from an external file.
    See DoConfigure for the expected format.

    It is sometimes useful to see how Log4D is reading configuration files.
    You can enable Log4D internal logging by defining the log4d.debug variable. }
  TLogPropertyConfigurator = class(TLogBasicConfigurator)
  protected
    procedure ConfigureRootLogger(const Props: TStringList;
      const Hierarchy: TLogHierarchy);
    procedure ParseAdditivityForLogger(const Props: TStringList;
      const Logger: TLogLogger);
    function ParseAppender(const Props: TStringList;
      const AppenderName: string): ILogAppender;
    procedure ParseLoggersAndRenderers(const Props: TStringList;
      const Hierarchy: TLogHierarchy);
    procedure ParseLogger(const Props: TStringList; const Logger: TLogLogger;
      const Value: string);
  public
    class procedure Configure(const Filename: string); overload;
    class procedure Configure(const Props: TStringList); overload;
    procedure DoConfigure(const FileName: string;
      const Hierarchy: TLogHierarchy); overload;
    procedure DoConfigure(const Props: TStringList;
      const Hierarchy: TLogHierarchy); overload;
  end;

{ Register a new appender class. }
procedure RegisterAppender(const Appender: TClass);
{ Find an appender based on its class name and create a new instance of it. }
function FindAppender(const ClassName: string): ILogAppender;

{ Register a new logger factory class. }
procedure RegisterLoggerFactory(const LoggerFactory: TClass);
{ Find a logger factory based on its class name
  and create a new instance of it. }
function FindLoggerFactory(const ClassName: string): ILogLoggerFactory;

{ Register a new error handler class. }
procedure RegisterErrorHandler(const ErrorHandler: TClass);
{ Find an error handler based on its class name
  and create a new instance of it. }
function FindErrorHandler(const ClassName: string): ILogErrorHandler;

{ Register a new filter class. }
procedure RegisterFilter(const Filter: TClass);
{ Find a filter based on its class name and create a new instance of it. }
function FindFilter(const ClassName: string): ILogFilter;

{ Register a new layout class. }
procedure RegisterLayout(const Layout: TClass);
{ Find a layout based on its class name and create a new instance of it. }
function FindLayout(const ClassName: string): ILogLayout;

{ Register a new class that can be rendered. }
procedure RegisterRendered(const Rendered: TClass);
{ Find a rendered class based on its class name and return its class. }
function FindRendered(const ClassName: string): TClass;

{ Register a new object renderer class. }
procedure RegisterRenderer(const Renderer: TClass);
{ Find an object renderer based on its class name
  and create a new instance of it. }
function FindRenderer(const ClassName: string): ILogRenderer;

{ Convert string value to a Boolean, with default. }
function StrToBool(Value: string; const Default: Boolean): Boolean;

{$IFDEF UNICODE}
{ Convert string encoding value to a default TEncoding. }
function FindEncodingFromName(const Name: string): TEncoding;
{$ENDIF UNICODE}

{$IFDEF LINUX}
procedure EnterCriticalSection(var CS: TCriticalSection);
procedure LeaveCriticalSection(var CS: TCriticalSection);
procedure InitializeCriticalSection(var CS: TCriticalSection);
procedure DeleteCriticalSection(var CS: TCriticalSection);
function GetCurrentThreadID: Integer;
procedure OutputDebugString(const S: PChar);
{$ENDIF}

var
  { Default implementation of ILogLoggerFactory }
  DefaultLoggerFactory: TLogDefaultLoggerFactory;
  { The logging hierarchy }
  DefaultHierarchy: TLogHierarchy;
  { Internal package logging. }
  LogLog: TLogLog;

implementation

{$IFDEF UNICODE}
uses
  Consts;
{$ENDIF UNICODE}

const
  CRLF = #13#10;
  MilliSecsPerDay = 24 * 60 * 60 * 1000;

resourcestring
  AddingLoggerMsg         = 'Adding logger "%s" in error handler';
  AppenderDefinedMsg      = 'Appender "%s" was already parsed';
  BadConfigFileMsg        = 'Couldn''t read configuration file "%s" - %s';
  ClosedAppenderMsg       = 'Not allowed to write to a closed appender';
  ConvertErrorMsg         = 'Could not convert "%s" to level';
  EndAppenderMsg          = 'Parsed "%s" options';
  EndErrorHandlerMsg      = 'End of parsing for "%s" error handler';
  EndFiltersMsg           = 'End of parsing for "%s" filter';
  EndLayoutMsg            = 'End of parsing for "%s" layout';
  FallbackMsg             = 'Fallback on error %s';
  FallbackReplaceMsg      = 'Fallback replacing "%s" with "%s" in logger "%s"';
  FinishedConfigMsg       = 'Finished configuring with %s';
  HandlingAdditivityMsg   = 'Handling %s="%s"';
  IgnoreConfigMsg         = 'Ignoring configuration file "%s"';
  InterfaceNotImplMsg     = '%s doesn''t implement %s';
  LayoutRequiredMsg       = 'Appender "%s" requires a layout';
  LevelHdr                = 'Level';
  LevelTokenMsg           = 'Level token is "%s"';
  LoggerFactoryMsg        = 'Setting logger factory to "%s"';
  LoggerHdr               = 'Logger';
  MessageHdr              = 'Message';
  NDCHdr                  = 'NDC';
  NilErrorHandlerMsg      = 'An appender cannot have a nil error handler';
  NilLevelMsg             = 'The root can''t have a nil level';
  NoAppendersMsg          = 'No appenders could be found for logger "%s"';
  NoAppenderCreatedMsg    = 'Couldn''t create appender named "%s"';
  NoClassMsg              = 'Couldn''t find class %s';
  NoLayoutMsg             = 'No layout set for appender named "%s"';
  NoRenderedCreatedMsg    = 'Couldn''t find rendered class "%s"';
  NoRendererMsg           = 'No renderer found for class %s';
  NoRendererCreatedMsg    = 'Couldn''t create renderer "%s"';
  NoRootLoggerMsg         = 'Couldn''t find root logger information. Is this OK?';
  ParsingAppenderMsg      = 'Parsing appender named "%s"';
  ParsingLoggerMsg        = 'Parsing for logger "%s" with value="%s"';
  ParsingErrorHandlerMsg  = 'Parsing error handler options for "%s"';
  ParsingFiltersMsg       = 'Parsing filter options for "%s"';
  ParsingLayoutMsg        = 'Parsing layout options for "%s"';
  PleaseInitMsg           = 'Please initialise the Log4D system properly';
  RendererMsg             = 'Rendering class: "%s", Rendered class: "%s"';
  SessionStartMsg         = 'Log session start time';
  SettingAdditivityMsg    = 'Setting additivity for "%s" to "%s"';
  SettingAppenderMsg      = 'Setting appender "%s" in error handler';
  SettingBackupMsg        = 'Setting backup appender "%s" in error handler';
  SettingLevelMsg         = 'Logger "%s" set to level "%s"';
  SettingLoggerMsg        = 'Setting logger "%s" in error handler';
  ThreadHdr               = 'Thread';
  TimeHdr                 = 'Time';
  ValueUnknownMsg         = 'Unknown';

var
  { Start time for the logging process - to compute elapsed time. }
  StartTime: TDateTime;

{ TLogOptionHandler -----------------------------------------------------------}

constructor TLogOptionHandler.Create;
begin
  inherited Create;
  Init;
end;

destructor TLogOptionHandler.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

{ Just return the saved option value. }
function TLogOptionHandler.GetOption(const Name: string): string;
begin
  Result := FOptions.Values[Name];
end;

procedure TLogOptionHandler.Init;
begin
  FOptions := TStringList.Create;
end;

{ Just save the option value. }
procedure TLogOptionHandler.SetOption(const Name, Value: string);
begin
  FOptions.Values[Name] := Value;
end;

{ TLogLevel -------------------------------------------------------------------}

var
  Levels: TObjectList;

{ Accumulate a list (in descending order) of TLogLevel objects defined. }
procedure RegisterLevel(Level: TLogLevel);
var
  Index: Integer;
begin
  if Levels.IndexOf(Level) > -1 then
    Exit;
  for Index := 0 to Levels.Count - 1 do
    if TLogLevel(Levels[Index]).Level < Level.Level then
    begin
      Levels.Insert(Index, Level);
      Exit;
    end
    else if TLogLevel(Levels[Index]).Level = Level.Level then
    begin
{$IFDEF VER120}
      TObject(Levels[Index]).Free;
{$ELSE}
      Levels[Index].Free;
{$ENDIF}
      Levels[Index] := Level;
      Exit;
    end;
  Levels.Add(Level);
end;

constructor TLogLevel.Create(const Name: string; Level: Integer);
begin
  inherited Create;
  FName  := Name;
  FLevel := Level;
  RegisterLevel(Self);
end;

{ Retrieve a level object given its level. }
class function TLogLevel.GetLevel(LogLevel: Integer): TLogLevel;
begin
  Result := GetLevel(LogLevel, Debug);
end;

{ Retrieve a level object given its level, or default if not valid. }
class function TLogLevel.GetLevel(LogLevel: Integer; DefaultLevel: TLogLevel):
  TLogLevel;
var
  Index: Integer;
begin
  Result := DefaultLevel;
  for Index := 0 to Levels.Count - 1 do
    if TLogLevel(Levels[Index]).Level = LogLevel then
    begin
      Result := TLogLevel(Levels[Index]);
      Break;
    end
    else if TLogLevel(Levels[Index]).Level < LogLevel then
      Break;
end;

{ Retrieve a level object given its name. }
class function TLogLevel.GetLevel(const Name: string): TLogLevel;
begin
  Result := GetLevel(Name, Debug);
end;

{ Retrieve a level object given its name, or default if not valid. }
class function TLogLevel.GetLevel(const Name: string; DefaultLevel: TLogLevel):
  TLogLevel;
var
  Index: Integer;
begin
  Result := DefaultLevel;
  for Index := 0 to Levels.Count - 1 do
    if TLogLevel(Levels[Index]).Name = Name then
    begin
      Result := TLogLevel(Levels[Index]);
      Exit;
    end;
end;

{ Returns True if this level has a higher or equal value
  than the level passed as argument, False otherwise. }
function TLogLevel.IsGreaterOrEqual(LogLevel: TLogLevel): Boolean;
begin
  Result := (Self.Level >= LogLevel.Level);
end;

{ TLogNDC ---------------------------------------------------------------------}

var
  { The nested diagnostic contexts (NDCs).
    This list has one entry for each thread.
    For each entry, the attached object is another string list
    containing the actual context strings. }
  NDC: TStringList;
  { The controller for synchronisation }
  CriticalNDC: TRTLCriticalSection;

{ Empty out the current NDC stack. }
class procedure TLogNDC.Clear;
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index > -1 then
      TStringList(NDC.Objects[Index]).Clear;
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Return a copy of the current NDC. }
class procedure TLogNDC.CloneStack(const Context: TStringList);
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index > -1 then
      Context.Assign(TStringList(NDC.Objects[Index]))
    else
      Context.Clear;
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Retrieve the depth for the current NDC. }
class function TLogNDC.GetDepth: Integer;
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Result := 0;
    Index  := GetNDCIndex;
    if Index > -1 then
      Result := TStringList(NDC.Objects[Index]).Count;
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Find the index in the NDCs for the current thread. }
class function TLogNDC.GetNDCIndex: Integer;
begin
  Result := NDC.IndexOf(GetThreadId);
end;

{ Return the current thread id as a string. }
class function TLogNDC.GetThreadId: string;
begin
  Result := IntToStr(GetCurrentThreadId);
end;

{ Use the provided context for this NDC. }
class procedure TLogNDC.Inherit(const Context: TStringList);
var
  Index: Integer;
begin
  if Context = nil then
    Exit;
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index = -1 then
      Index := NDC.AddObject(GetThreadId, TStringList.Create);
    TStringList(NDC.Objects[Index]).Assign(Context)
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Retrieve the current NDC for display. }
class function TLogNDC.Peek: string;
var
  Index, Index2: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Result := '';
    Index  := GetNDCIndex;
    if Index = -1 then
      Exit;
    with TStringList(NDC.Objects[Index]) do
      for Index2 := 0 to Count - 1 do
        Result := Result + '|' + Strings[Index2];
    Delete(Result, 1, 1);
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Remove the last context string added to the stack and return its value. }
class function TLogNDC.Pop: string;
var
  Index, Index2: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Result := '';
    Index  := GetNDCIndex;
    if Index = -1 then
      Exit;
    with TStringList(NDC.Objects[Index]) do
    begin
      for Index2 := 0 to Count - 1 do
        Result := Result + '|' + Strings[Index2];
      if Count <= 1 then
        TLogNDC.Clear
      else if Count > 0 then
        Delete(Count - 1);
    end;
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Add a new context string to the stack. }
class procedure TLogNDC.Push(const Context: string);
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index = -1 then
      Index := NDC.AddObject(GetThreadId, TStringList.Create);
    with TStringList(NDC.Objects[Index]) do
      Add(Context);
  finally
    LeaveCriticalSection(CriticalNDC)
  end;
end;

{ Remove the current NDC. }
class procedure TLogNDC.Remove;
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index > -1 then
    begin
      NDC.Objects[Index].Free;
      NDC.Delete(Index);
    end;
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ Trim the current NDC back to a certain depth. }
class procedure TLogNDC.SetMaxDepth(const MaxDepth: Integer);
var
  Index: Integer;
begin
  EnterCriticalSection(CriticalNDC);
  try
    Index := GetNDCIndex;
    if Index > -1 then
      with TStringList(NDC.Objects[Index]) do
        while Count > MaxDepth do
          Delete(Count - 1);
  finally
    LeaveCriticalSection(CriticalNDC);
  end;
end;

{ TLogEvent -------------------------------------------------------------------}

constructor TLogEvent.Create(const Logger: TLogLogger;
  const Level: TLogLevel; const Message: string; const Err: Exception;
  const TimeStamp: TDateTime);
begin
  inherited Create;
  FLogger  := Logger;
  FLevel   := Level;
  FMessage := Message;
  FError   := Err;
  if TimeStamp = 0 then
    FTimeStamp := Now
  else
    FTimeStamp := TimeStamp;
end;

{ Immediately render an object into a text message. }
constructor TLogEvent.Create(const Logger: TLogLogger;
  const Level: TLogLevel; const Message: TObject; const Err: Exception;
  const TimeStamp: TDateTime);
var
  Renderer: ILogRenderer;
begin
  inherited Create;
  Renderer := Logger.Hierarchy.GetRenderer(Message.ClassType);
  if Renderer = nil then
  begin
    LogLog.Error(NoRendererMsg, [Message.ClassName]);
    Abort;
  end
  else
    Create(Logger, Level, Renderer.Render(Message), Err, Timestamp);
end;

{ The elapsed time since package start up (in milliseconds). }
function TLogEvent.GetElapsedTime: LongInt;
begin
  Result := Round((TimeStamp - StartTime) * MilliSecsPerDay);
end;

{ Return the embedded exception's class name (if there is one). }
function TLogEvent.GetErrorClass: string;
begin
  if Error <> nil then
    Result := Error.ClassName
  else
    Result := '';
end;

{ Return the embedded exception's message and classname (if there is one). }
function TLogEvent.GetErrorMessage: string;
begin
  if Error <> nil then
    Result := Error.Message + ' (' + Error.ClassName + ')'
  else
    Result := '';
end;

{ Return the name of the logger. }
function TLogEvent.GetLoggerName: string;
begin
  Result := FLogger.Name;
end;

{ Return the nested diagnostic context. }
function TLogEvent.GetNDC: string;
begin
  Result := TLogNDC.Peek;
end;

{ Return the current thread ID. }
function TLogEvent.GetThreadId: LongInt;
begin
  Result := GetCurrentThreadId;
end;

{ TLogDefaultLoggerFactory ----------------------------------------------------}

function TLogDefaultLoggerFactory.MakeNewLoggerInstance(const Name: string):
  TLogLogger;
begin
  Result := TLogLogger.Create(Name);
end;

{ TLogLogger ------------------------------------------------------------------}

constructor TLogLogger.Create(const Name: string);
begin
  inherited Create;
  InitializeCriticalSection(FCriticalLogger);
  FAdditive  := True;
  FAppenders := TInterfaceList.Create;
  FName      := Name;
end;

destructor TLogLogger.Destroy;
begin
  FAppenders.Free;
  DeleteCriticalSection(FCriticalLogger);
  inherited Destroy;
end;

procedure TLogLogger.AddAppender(const Appender: ILogAppender);
begin
  LockLogger;
  try
    if FAppenders.IndexOf(Appender) = -1 then
    begin
      FAppenders.Add(Appender);
      if FHierarchy <> nil then
        FHierarchy.FireAppenderEvent(True, Self, Appender);
    end;
  finally
    UnlockLogger;
  end;
end;

{ Log a message if the assertion is false. }
procedure TLogLogger.AssertLog(const Assertion: Boolean;
  const Message: string);
begin
  if not Assertion then
    DoLog(Log4D.Error, Message);
end;

{ Log a message if the assertion is false. }
procedure TLogLogger.AssertLog(const Assertion: Boolean;
  const Message: TObject);
begin
  if not Assertion then
    DoLog(Log4D.Error, Message);
end;

{ Send event to each appender to be logged.
  If additive, also send to parent's appenders. }
procedure TLogLogger.CallAppenders(const Event: TLogEvent);
var
  Index: Integer;
begin
  LockLogger;
  try
    if CountAppenders = 0 then
    begin
      LogLog.Error(NoAppendersMsg, [Name]);
      LogLog.Error(PleaseInitMsg);
      Exit;
    end;
    for Index := 0 to FAppenders.Count - 1 do
      ILogAppender(FAppenders[Index]).Append(Event);
    if Additive and (Parent <> nil) then
      Parent.CallAppenders(Event);
  finally
    UnlockLogger;
  end;
end;

procedure TLogLogger.CloseAllAppenders;
var
  Index: Integer;
begin
  LockLogger;
  try
    for Index := 0 to FAppenders.Count - 1 do
      ILogAppender(FAppenders[Index]).Close;
  finally
    UnlockLogger;
  end;
end;

{ Include parent's appenders in the count (if additive). }
function TLogLogger.CountAppenders: Integer;
begin
  Result := FAppenders.Count;
  if Additive and (Parent <> nil) then
    Result := Result + Parent.CountAppenders;
end;

procedure TLogLogger.Debug(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Debug, Fmt, Args, Err);
end;

procedure TLogLogger.Debug(const Message: string; const Err: Exception);
begin
  Log(Log4D.Debug, Message, Err);
end;

procedure TLogLogger.Debug(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Debug, Message, Err);
end;

{ Create the logging event object and send it to the appenders. }
procedure TLogLogger.DoLog(const LogLevel: TLogLevel; const Message: string;
  const Err: Exception);
var
  Event: TLogEvent;
begin
  Event := TLogEvent.Create(Self, LogLevel, Message, Err);
  try
    CallAppenders(Event);
  finally
    Event.Free;
  end;
end;

procedure TLogLogger.DoLog(const LogLevel: TLogLevel; const Message: TObject;
  const Err: Exception);
var
  Event: TLogEvent;
begin
  Event := TLogEvent.Create(Self, LogLevel, Message, Err);
  try
    CallAppenders(Event);
  finally
    Event.Free;
  end;
end;

procedure TLogLogger.Error(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Error, Fmt, Args, Err);
end;

procedure TLogLogger.Error(const Message: string; const Err: Exception);
begin
  Log(Log4D.Error, Message, Err);
end;

procedure TLogLogger.Error(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Error, Message, Err);
end;

procedure TLogLogger.Fatal(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Fatal, Fmt, Args, Err);
end;

procedure TLogLogger.Fatal(const Message: string; const Err: Exception);
begin
  Log(Log4D.Fatal, Message, Err);
end;

procedure TLogLogger.Fatal(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Fatal, Message, Err);
end;

{ Find an appender by name. }
function TLogLogger.GetAppender(const Name: string): ILogAppender;
var
  Index: Integer;
begin
  Result := nil;
  LockLogger;
  try
    for Index := 0 to FAppenders.Count - 1 do
      if ILogAppender(FAppenders[Index]).Name = Name then
      begin
        Result := ILogAppender(FAppenders[Index]);
        Exit;
      end;
  finally
    UnlockLogger;
  end;
end;

{ Create new loggers via the logger class. }
class function TLogLogger.GetLogger(const Clazz: TClass;
  const Factory: ILogLoggerFactory): TLogLogger;
begin
  Result := GetLogger(Clazz.ClassName, Factory);
end;

{ Create new loggers via the logger class. }
class function TLogLogger.GetLogger(const Name: string;
  const Factory: ILogLoggerFactory): TLogLogger;
begin
  Result := DefaultHierarchy.GetLogger(Name, Factory);
end;

{ Retrieve the root logger. }
class function TLogLogger.GetRootLogger: TLogLogger;
begin
  Result := DefaultHierarchy.Root;
end;

{ Return parent's level if not set in this logger. }
function TLogLogger.GetLevel: TLogLevel;
begin
  if FLevel <> nil then
    Result := FLevel
  else
    Result := Parent.Level;
end;

procedure TLogLogger.Info(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Info, Fmt, Args, Err);
end;

procedure TLogLogger.Info(const Message: string; const Err: Exception);
begin
  Log(Log4D.Info, Message, Err);
end;

procedure TLogLogger.Info(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Info, Message, Err);
end;

{ Is a given appender in use. }
function TLogLogger.IsAppender(const Appender: ILogAppender): Boolean;
begin
  Result := (FAppenders.IndexOf(Appender) > -1);
end;

function TLogLogger.IsDebugEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Debug);
end;

{ Hierarchy can disable logging at a global level. }
function TLogLogger.IsEnabledFor(const LogLevel: TLogLevel): Boolean;
begin
  Result := not Hierarchy.IsDisabled(LogLevel.Level) and LogLevel.IsGreaterOrEqual(Level);
end;

function TLogLogger.IsErrorEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Error);
end;

function TLogLogger.IsFatalEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Fatal);
end;

function TLogLogger.IsInfoEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Info);
end;

function TLogLogger.IsTraceEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Trace);
end;

function TLogLogger.IsWarnEnabled: Boolean;
begin
  Result := IsEnabledFor(Log4D.Warn);
end;

{ Synchronise access to the logger. }
procedure TLogLogger.LockLogger;
begin
  EnterCriticalSection(FCriticalLogger);
end;

{ Hierarchy can disable logging at a global level. }
procedure TLogLogger.Log(const LogLevel: TLogLevel; const Fmt: string; const Args: array of const;
  const Err: Exception);
begin
  if IsEnabledFor(LogLevel) then
    DoLog(LogLevel, Format(Fmt, Args), Err);
end;

procedure TLogLogger.Log(const LogLevel: TLogLevel; const Message: string; const Err: Exception);
begin
  if IsEnabledFor(LogLevel) then
    DoLog(LogLevel, Message, Err);
end;

procedure TLogLogger.Log(const LogLevel: TLogLevel; const Message: TObject; const Err: Exception);
begin
  if IsEnabledFor(LogLevel) then
    DoLog(LogLevel, Message, Err);
end;

procedure TLogLogger.RemoveAllAppenders;
begin
  LockLogger;
  try
    FAppenders.Clear;
  finally
    UnlockLogger;
  end;
end;

procedure TLogLogger.RemoveAppender(const Appender: ILogAppender);
begin
  LockLogger;
  try
    FAppenders.Remove(Appender);
    if FHierarchy <> nil then
      FHierarchy.FireAppenderEvent(False, Self, Appender);
  finally
    UnlockLogger;
  end;
end;

procedure TLogLogger.RemoveAppender(const Name: string);
var
  Appender: ILogAppender;
begin
  LockLogger;
  try
    Appender := GetAppender(Name);
    if Appender <> nil then
      FAppenders.Remove(Appender);
  finally
    UnlockLogger;
  end;
end;

procedure TLogLogger.Trace(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Trace, Fmt, Args, Err);
end;

procedure TLogLogger.Trace(const Message: string; const Err: Exception);
begin
  Log(Log4D.Trace, Message, Err);
end;

procedure TLogLogger.Trace(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Trace, Message, Err);
end;

{ Release synchronised access to the logger. }
procedure TLogLogger.UnlockLogger;
begin
  LeaveCriticalSection(FCriticalLogger);
end;

procedure TLogLogger.Warn(const Fmt: string; const Args: array of const; const Err: Exception);
begin
  Log(Log4D.Warn, Fmt, Args, Err);
end;

procedure TLogLogger.Warn(const Message: string; const Err: Exception);
begin
  Log(Log4D.Warn, Message, Err);
end;

procedure TLogLogger.Warn(const Message: TObject; const Err: Exception);
begin
  Log(Log4D.Warn, Message, Err);
end;

{ TLogRoot --------------------------------------------------------------------}

const
  InternalRootName = 'root';

constructor TLogRoot.Create(const Level: TLogLevel);
begin
  inherited Create(InternalRootName);
  Self.Level := Level;
end;

{ Root logger cannot have a nil level. }
procedure TLogRoot.SetLevel(const Val: TLogLevel);
begin
  if Val = nil then
  begin
    LogLog.Error(NilLevelMsg);
    inherited Level := Log4D.Debug;
  end
  else
    inherited Level := Val;
end;

{ TLogLog ---------------------------------------------------------------------}

{ Initialise internal logging - send it to debugging output. }
constructor TLogLog.Create;
// fix for Free Pascal by M Justin: use a variable
var
  TmpAppender: ILogAppender;
begin
  inherited Create('');

  TmpAppender := TLogODSAppender.Create('');
  AddAppender(TmpAppender);
  InternalDebugging := False;
  Level             := Log4D.Debug;
end;

{ Only log internal debugging messages when requested. }
procedure TLogLog.DoLog(const LogLevel: TLogLevel; const Message: string;
  const Err: Exception);
begin
  if (LogLevel <> Log4D.Debug) or InternalDebugging then
    inherited DoLog(LogLevel, Message, Err);
end;

procedure TLogLog.DoLog(const LogLevel: TLogLevel; const Message: TObject;
  const Err: Exception);
begin
  if (LogLevel <> Log4D.Debug) or InternalDebugging then
    inherited DoLog(LogLevel, Message, Err);
end;

{ TLogHierarchy ---------------------------------------------------------------}

constructor TLogHierarchy.Create(Root: TLogLogger);
begin
  inherited Create;
  InitializeCriticalSection(FCriticalHierarchy);
  FEmittedNoAppenderWarning := False;
  FListeners                := TInterfaceList.Create;
  FLoggers                  := TStringList.Create;
  FRenderedClasses          := TClassList.Create;
  FRenderers                := TInterfaceList.Create;
  FRoot                     := Root;
  FRoot.Hierarchy           := Self;
  FThreshold                := All;
end;

destructor TLogHierarchy.Destroy;
begin
  Shutdown;
  Clear;
  FListeners.Free;
  FLoggers.Free;
  if TLogLogger(FRoot).RefCount > 0 then
    TLogLogger(FRoot)._Release
  else
    FRoot.Free;
  FRenderedClasses.Free;
  FRenderers.Free;
  DeleteCriticalSection(FCriticalHierarchy);
  inherited Destroy;
end;

{ Add a new listener for hierarchy events. }
procedure TLogHierarchy.AddHierarchyEventListener(
  const Listener: ILogHierarchyEventListener);
begin
  if FListeners.IndexOf(Listener) = -1 then
    FListeners.Add(Listener);
end;

{ Add an object renderer for a specific class. }
procedure TLogHierarchy.AddRenderer(RenderedClass: TClass;
  Renderer: ILogRenderer);
var
  Index: Integer;
begin
  Index := FRenderedClasses.IndexOf(RenderedClass);
  if Index = -1 then
  begin
    FRenderedClasses.Add(RenderedClass);
    FRenderers.Add(Renderer);
  end
  else
    FRenderers[Index] := Renderer;
end;

{ This call will clear all logger definitions from the internal hashtable.
  Invoking this method will irrevocably mess up the logger hierarchy.
  You should really know what you are doing before invoking this method. }
procedure TLogHierarchy.Clear;
var
  Index: Integer;
begin
  for Index := 0 to FLoggers.Count - 1 do
    if TLogLogger(FLoggers.Objects[Index]).RefCount > 0 then
      TLogLogger(FLoggers.Objects[Index])._Release
    else
      FLoggers.Objects[Index].Free;
  FLoggers.Clear;
end;

{ Warn the user about no appenders for a logger, but only once. }
procedure TLogHierarchy.EmitNoAppenderWarning(const Logger: TLogLogger);
begin
  if not FEmittedNoAppenderWarning then
  begin
    LogLog.Warn(NoAppendersMsg, [Logger.Name]);
    LogLog.Warn(PleaseInitMsg);
    FEmittedNoAppenderWarning := True;
  end;
end;

{ Check if the named logger exists in the hirarchy.
  If so return its reference, otherwise return nil. }
function TLogHierarchy.Exists(const Name: string): TLogLogger;
var
  Index: Integer;
begin
  Index := FLoggers.IndexOf(Name);
  if Index > -1 then
    Result := TLogLogger(FLoggers.Objects[Index])
  else
    Result := nil;
end;

{ Notify registered listeners of an event. }
procedure TLogHierarchy.FireAppenderEvent(const Adding: Boolean;
  const Logger: TLogLogger; const Appender: ILogAppender);
var
  Index: Integer;
begin
  for Index := 0 to FListeners.Count - 1 do
    with ILogHierarchyEventListener(FListeners[Index]) do
      if Adding then
        AddAppenderEvent(Logger, Appender)
      else
        RemoveAppenderEvent(Logger, Appender);
end;

{ Returns all the currently defined loggers in this hierarchy as
  a string list (excluding the root logger). }
procedure TLogHierarchy.GetCurrentLoggers(const List: TStringList);
var
  Index: Integer;
begin
  for Index := 0 to FLoggers.Count - 1 do
    List.Add(FLoggers[Index]);
end;

{ Return a new logger instance named as the first parameter using the
  specified factory. If no factory is provided, use the DefaultLoggerFactory.
  If a logger of that name already exists, then it will be returned.
  Otherwise, a new logger is instantiated by the factory parameter
  and linked with its existing ancestors as well as children. }
function TLogHierarchy.GetLogger(const Name: string;
  const Factory: ILogLoggerFactory): TLogLogger;
var
  LoggerFactory: ILogLoggerFactory;
begin
  EnterCriticalSection(FCriticalHierarchy);
  try
    Result := Exists(Name);
    if Result = nil then
    begin
      if Factory <> nil then
        LoggerFactory := Factory
      else
        LoggerFactory := DefaultLoggerFactory;
      Result           := LoggerFactory.MakeNewLoggerInstance(Name);
      Result.Hierarchy := Self;
      FLoggers.AddObject(Name, Result);
      UpdateParent(Result);
    end;
  finally
    LeaveCriticalSection(FCriticalHierarchy);
  end;
end;

{ Return a renderer for the named class. }
function TLogHierarchy.GetRenderer(const RenderedClass: TClass): ILogRenderer;
var
  Index: Integer;
  Rendered: TClass;
begin
  Result   := nil;
  Rendered := RenderedClass;
  repeat
    Index := FRenderedClasses.IndexOf(Rendered);
    if Index > -1 then
      Result   := ILogRenderer(FRenderers[Index])
    else
      Rendered := Rendered.ClassParent;
  until (Result <> nil) or (Rendered.ClassName = 'TObject');
end;

{ Check for global disabling of levels. }
function TLogHierarchy.IsDisabled(const LogLevel: Integer): Boolean;
begin
  Result := (FThreshold.Level > LogLevel);
end;

{ Remove a previously register event listener. }
procedure TLogHierarchy.RemoveHierarchyEventListener(
  const Listener: ILogHierarchyEventListener);
begin
  FListeners.Remove(Listener);
end;

{ Reset all values contained in this hierarchy instance to their default.
  This removes all appenders from all loggers, sets the level of
  all non-root loggers to nil, sets their additivity flag to true and
  sets the level of the root logger to Debug.
  Moreover, message disabling is set its default 'off' value.
  Existing loggers are not removed. They are just reset. }
procedure TLogHierarchy.ResetConfiguration;
var
  Index: Integer;
begin
  EnterCriticalSection(FCriticalHierarchy);
  try
    Root.Level := Debug;
    Threshold  := All;

    Shutdown;  { Nested locks are OK }

    for Index := 0 to FLoggers.Count - 1 do
      with TLogLogger(FLoggers.Objects[Index]) do
      begin
        Level    := nil;
        Additive := True;
      end;
    FRenderedClasses.Clear;
    FRenderers.Clear;
  finally
    LeaveCriticalSection(FCriticalHierarchy);
  end;
end;

{ Set the overall logging level. Cannot set a nil threshold. }
procedure TLogHierarchy.SetThresholdProp(const Level: TLogLevel);
begin
  if Level <> nil then
    FThreshold := Level;
end;

{ Set the overall hierarchy logging level by name. }
procedure TLogHierarchy.SetThreshold(const Name: string);
var
  Level: TLogLevel;
begin
  Level := TLogLevel.GetLevel(LowerCase(Name), nil);
  if Level = nil then
    LogLog.Warn(ConvertErrorMsg, [Name])
  else
    Threshold := Level;
end;

{ Shutting down a hierarchy will safely close and remove
  all appenders in all loggers including the root logger.
  Some appenders need to be closed before the application exists,
  otherwise, pending logging events might be lost. }
procedure TLogHierarchy.Shutdown;
var
  Index: Integer;
begin
  EnterCriticalSection(FCriticalHierarchy);
  try
    Root.CloseAllAppenders;
    Root.RemoveAllAppenders;
    for Index := 0 to FLoggers.Count - 1 do
      with TLogLogger(FLoggers.Objects[Index]) do
      begin
        CloseAllAppenders;
        RemoveAllAppenders;
      end;
  finally
    LeaveCriticalSection(FCriticalHierarchy);
  end;
end;

{ Set the parent for the specified logger.
  The logger hierarchy is based on names separated by a dot (.).
  The root is the ultimate parent. Otherwise, we use GetLogger to
  return a reference to the immediate parent (and create it if necessary). }
procedure TLogHierarchy.UpdateParent(const Logger: TLogLogger);
var
  Index: Integer;

  { Return the index of the last dot (.) in the text, or zero if none. }
  function LastDot(Text: string): Integer;
  begin
    for Result := Length(Text) downto 1 do
      if Text[Result] = '.' then
        Exit;
    Result := 0;
  end;

begin
  Index := LastDot(Logger.Name);
  if Index = 0 then
    Logger.FParent := Root
  else
    Logger.FParent := GetLogger(Copy(Logger.Name, 1, Index - 1));
end;

{ TLogCustomLayout ------------------------------------------------------------}

{ Returns the content type output by this layout.
  The base class returns 'text/plain'. }
function TLogCustomLayout.GetContentType: string;
begin
  Result := 'text/plain';
end;

{ Returns the footer for the layout format. The base class returns ''. }
function TLogCustomLayout.GetFooter: string;
begin
  Result := '';
end;

{ Returns the header for the layout format. The base class returns ''. }
function TLogCustomLayout.GetHeader: string;
begin
  Result := '';
end;

{ If the layout handles the Exception object contained within, then the
  layout should return False. Otherwise, if the layout ignores Exception
  object, then the layout should return True. The base class returns True. }
function TLogCustomLayout.IgnoresException: Boolean;
begin
  Result := True;
end;

{ Initialisation - date format is a standard option. }
procedure TLogCustomLayout.Init;
begin
  inherited Init;
  SetOption(DateFormatOpt,
    {$IF CompilerVersion >= 22}FormatSettings.{$ENDIF}
    {$IFDEF FPC}FormatSettings.{$ENDIF}
    ShortDateFormat);
end;

{ Set a list of options for this layout. }
procedure TLogCustomLayout.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if (Name = DateFormatOpt) and (Value <> '') then
    DateFormat := Value;
end;

{ TLogSimpleLayout ------------------------------------------------------------}

{ Show event level and message. }
function TLogSimpleLayout.Format(const Event: TLogEvent): string;
begin
  Result := Event.Level.Name + ' - ' + Event.Message + CRLF;
end;

{ TLogHTMLLayout --------------------------------------------------------------}

{ Write a HTML table row for each event. }
function TLogHTMLLayout.Format(const Event: TLogEvent): string;
var
  ErrorMessage: string;
begin
  Result := '<tr><td>' + IntToStr(Event.ElapsedTime) +
    '</td><td>' + IntToStr(Event.ThreadId) + '</td><td>';
  if Event.Level = Debug then
    Result := Result + '<font color="#339933">' + Event.Level.Name +
      '</font>'
  else if Event.Level.IsGreaterOrEqual(Warn) then
    Result := Result + '<font color="#993300"><strong>' + Event.Level.Name +
      '</strong></font>'
  else
    Result := Result + Event.Level.Name;
  Result := Result + '</td><td>' + Event.LoggerName + '</td>' +
    '<td>' + Event.NDC + '</td><td>' + Event.Message + '</td></tr>' + CRLF;
  ErrorMessage := Event.ErrorMessage;
  if ErrorMessage <> '' then
    Result := Result + '<tr><td bgcolor="#993300" ' +
      'style="color: White; font-size: xx-small;" colspan="6">' +
      ErrorMessage + '</td></tr>' + CRLF;
end;

{ Returns the content type output by this layout, i.e 'text/html'. }
function TLogHTMLLayout.GetContentType: string;
begin
  Result := 'text/html';
end;

{ Returns appropriate HTML footers. }
function TLogHTMLLayout.GetFooter: string;
begin
  Result := '</table>' + CRLF + '</body>' + CRLF + '</html>' + CRLF;
end;

{ Returns appropriate HTML headers. }
function TLogHTMLLayout.GetHeader: string;
begin
  Result := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" ' +
    '"http://www.w3.org/TR/html4/loose.dtd">' + CRLF +
    '<html>' + CRLF + '<head>' + CRLF +
    '<title>' + Title + '</title>' + CRLF +
    '<style type="text/css">' + CRLF +
    '<!--' + CRLF +
    'body, table {font-family: arial,sans-serif; font-size: x-small;}' + CRLF +
    'th {background: #336699; color: #FFFFFF; text-align: left;}' + CRLF +
    '-->' + CRLF +
    '</style>' + CRLF +
    '</head>' + CRLF +
    '<body bgcolor="#FFFFFF" topmargin="6" leftmargin="6">' + CRLF +
    '<hr size="1" noshade>' + CRLF +
    SessionStartMsg + ' ' + DateTimeToStr(Now) + '<br>' + CRLF +
    '<br>' + CRLF +
    '<table cellspacing="0" cellpadding="4" border="1" ' +
    'bordercolor="#224466" width="100%">' + CRLF +
    '<tr><th>' + TimeHdr + '</th><th>' + ThreadHdr + '</th>' +
    '<th>' + LevelHdr + '</th><th>' + LoggerHdr + '</th>' +
    '<th>' + NDCHdr + '</th><th>' + MessageHdr + '</th></tr>' + CRLF;
end;

{ The HTML layout handles the exception contained in logging events.
  Hence, this method return False. }
function TLogHTMLLayout.IgnoresException: Boolean;
begin
  Result := False;
end;

{ Set the title for the HTML page. }
procedure TLogHTMLLayout.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if (Name = TitleOpt) and (Value <> '') then
    Title := Value;
end;

{ TLogPatternLayout -----------------------------------------------------------}

type
  TPatternPart = (ppText, ppLogger, ppClassName, ppDate, ppException,
    ppFileName, ppLocation, ppLine, ppMessage, ppMethod, ppNewLine,
    ppLevel, ppRuntime, ppThread, ppNDC, ppPercent);

const
  { These characters identify the types above. }
  PatternChars        = ' cCdeFlLmMnprtx%';
  { These characters substitute for those above in the processed format. }
  PatternReplacements = ' ssssssdssssddss';

constructor TLogPatternLayout.Create(const Pattern: string);
begin
  inherited Create;
  Self.Pattern := Pattern;
end;

destructor TLogPatternLayout.Destroy;
begin
  FPatternParts.Free;
  inherited Destroy;
end;

{ Compile the formatted string from the specified pattern and its parts.
  Pattern characters are as follows:
  c - Logger name, e.g. myapp.more
  C - Class name of caller - not implemented
  e - Message and class name from the exception associated with the event
  d - Current date and time, using date format set as option
  F - File name of calling class - not implemented
  l - Name and location within calling method - not implemented
  L - Line number within calling method - not implemented
  m - Message associated with event
  M - Method name within calling class - not implemented
  n - New line
  p - Level name
  r - Runtime in milliseconds since start
  t - Thread id
  x - Nested diagnostic context (NDC)
  % - The percent character
  Pattern characters are preceded by a percent sign (%) and may contain
  field formatting characters per Delphi's Format function, e.g. %-7p
  displays the event's level, left justified in a 7 character field.
  Other text is displayed as is. }
function TLogPatternLayout.Format(const Event: TLogEvent): string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to FPatternParts.Count - 1 do
    case TPatternPart(FPatternParts.Objects[Index]) of
      ppText:      Result := Result + FPatternParts[Index];
      ppLogger:  Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.LoggerName]);
      ppClassName: Result := Result +
        SysUtils.Format(FPatternParts[Index], [ValueUnknownMsg]);
      ppDate:      Result := Result + FormatDateTime(DateFormat, Now);
      ppException: Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.ErrorMessage]);
      ppFileName:  Result := Result +
        SysUtils.Format(FPatternParts[Index], [ValueUnknownMsg]);
      ppLocation:  Result := Result +
        SysUtils.Format(FPatternParts[Index], [ValueUnknownMsg]);
      ppLine:      Result := Result +
        SysUtils.Format(FPatternParts[Index], [ValueUnknownMsg]);
      ppMessage:   Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.Message]);
      ppMethod:    Result := Result +
        SysUtils.Format(FPatternParts[Index], [ValueUnknownMsg]);
      ppNewLine:   Result := Result + CRLF;
      ppLevel:  Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.Level.Name]);
      ppRuntime:   Result := Result + SysUtils.Format(FPatternParts[Index],
        [Event.ElapsedTime]);
      ppThread:    Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.ThreadId]);
      ppNDC:       Result := Result +
        SysUtils.Format(FPatternParts[Index], [Event.NDC]);
      ppPercent:   Result := Result + '%';
    end;
end;

procedure TLogPatternLayout.Init;
begin
  inherited Init;
  FPatternParts := TStringList.Create;
end;

procedure TLogPatternLayout.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if (Name = PatternOpt) and (Value <> '') then
    Pattern := Value;
end;

{ Extract the portions of the pattern for easier processing later. }
procedure TLogPatternLayout.SetPattern(const Pattern: string);
var
  Index: Integer;
  Part: string;
  PartType: TPatternPart;
begin
  FPattern := Pattern;
  FPatternParts.Clear;
  Part     := '';
  Index    := 1;
  while Index <= Length(FPattern) do
  begin
    if FPattern[Index] = '%' then
    begin
      { Patterns are delimited by percents (%) and continue up to
        one of the special characters noted earlier. }
      repeat
        Part := Part + FPattern[Index];
        Inc(Index);
      until (Index > Length(FPattern)) or
        (Pos(FPattern[Index], PatternChars) > 1);
      if Index > Length(FPattern) then
        Part := Part + 'm'
      else
        Part := Part + FPattern[Index];
      PartType := TPatternPart(Pos(Part[Length(Part)], PatternChars) - 1);
      Part[Length(Part)] :=
        PatternReplacements[Pos(FPattern[Index], PatternChars)];
      FPatternParts.AddObject(Part, Pointer(Integer(PartType)));
      Part := '';
      Inc(Index);
    end
    else
    begin
      { The rest is straight text - up to the next percent (%). }
      repeat
        Part := Part + FPattern[Index];
        Inc(Index);
      until (Index > Length(FPattern)) or (FPattern[Index] = '%');
      FPatternParts.AddObject(Part, Pointer(Integer(ppText)));
      Part := '';
    end;
  end;
end;

{ TLogCustomErrorHandler ------------------------------------------------------}

procedure TLogCustomErrorHandler.SetAppender(const Appender: ILogAppender);
begin
  FAppender := Appender;
  LogLog.Debug(SettingAppenderMsg, [Appender.Name]);
end;

procedure TLogCustomErrorHandler.SetBackupAppender(
  const BackupAppender: ILogAppender);
begin
  FBackupAppender := BackupAppender;
  LogLog.Debug(SettingBackupMsg, [BackupAppender.Name]);
end;

procedure TLogCustomErrorHandler.SetLogger(const Logger: TLogLogger);
begin
  FLogger := Logger;
  LogLog.Debug(SettingLoggerMsg, [Logger.Name]);
end;

{ TLogOnlyOnceErrorHandler ----------------------------------------------------}

{ Only first error sent here is reported. }
procedure TLogOnlyOnceErrorHandler.Error(const Message: string);
begin
  if not FSeenError then
  begin
    LogLog.Error(Message);
    FSeenError := True;
  end;
end;

procedure TLogOnlyOnceErrorHandler.Error(const Message: string;
  const Err: Exception; const ErrorCode: Integer; const Event: TLogEvent);
begin
  if not FSeenError then
    Error(Format('%s - (%d) %s', [Message, Err.Message, ErrorCode]));
end;

{ TLogFallbackErrorHandler ----------------------------------------------------}

constructor TLogFallbackErrorHandler.Create;
begin
  inherited Create;
  FLoggers             := TObjectList.Create;
{$IFDEF VER130}
  FLoggers.OwnsObjects := False;
{$ENDIF}
{$IF CompilerVersion >= 14}
  FLoggers.OwnsObjects := False;
{$ENDIF}
end;

destructor TLogFallbackErrorHandler.Destroy;
begin
  FLoggers.Free;
  inherited Destroy;
end;

{ Fallback on an alternative appender if an error arises. }
procedure TLogFallbackErrorHandler.Error(const Message: string);
var
  Index: Integer;
begin
  LogLog.Debug(FallbackMsg, [Message]);
  for Index := 0 to FLoggers.Count - 1 do
    with TLogLogger(FLoggers[Index]) do
    begin
      LogLog.Debug(FallbackReplaceMsg,
        [FAppender.Name, FBackupAppender.Name, Name]);
      RemoveAppender(FAppender);
      AddAppender(FBackupAppender);
    end;
end;

procedure TLogFallbackErrorHandler.Error(const Message: string;
  const Err: Exception; const ErrorCode: Integer; const Event: TLogEvent);
begin
  Error(Format('%s - (%d) %s', [Message, Err.Message, ErrorCode]));
end;

{ Add to the list of loggers to search for on failure. }
procedure TLogFallbackErrorHandler.SetLogger(const Logger: TLogLogger);
begin
  if FLoggers.IndexOf(Logger) = -1 then
  begin
    FLoggers.Add(Logger);
    LogLog.Debug(AddingLoggerMsg, [Logger.Name]);
  end;
end;

{ TLogCustomFilter ------------------------------------------------------------}

const
  Acceptance: array [Boolean] of TLogFilterDecision = (fdDeny, fdAccept);

{ Initialisation. }
constructor TLogCustomFilter.Create(const AcceptOnMatch: Boolean);
begin
  inherited Create;
  Self.AcceptOnMatch := AcceptOnMatch;
end;

{ Set common option. }
procedure TLogCustomFilter.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = AcceptMatchOpt then
    FAcceptOnMatch := StrToBool(Value, True);
end;

{ TLogDenyAllFilter -----------------------------------------------------------}

{ Deny all messages. }
function TLogDenyAllFilter.Decide(const Event: TLogEvent): TLogFilterDecision;
begin
  Result := fdDeny;
end;

{ TLogLevelMatchFilter --------------------------------------------------------}

{ Initialisation. }
constructor TLogLevelMatchFilter.Create(const MatchLevel: TLogLevel;
  const AcceptOnMatch: Boolean);
begin
  inherited Create(AcceptOnMatch);
  Self.MatchLevel := MatchLevel;
end;

{ Check for the matching level, then accept or deny based on the flag. }
function TLogLevelMatchFilter.Decide(const Event: TLogEvent):
  TLogFilterDecision;
begin
  if (MatchLevel <> nil) and (MatchLevel.Level = Event.Level.Level) then
    Result := Acceptance[AcceptOnMatch]
  else
    Result := fdNeutral;
end;

{ Set matching level from options. }
procedure TLogLevelMatchFilter.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = MatchOpt then
    FMatchLevel := TLogLevel.GetLevel(Value);
end;

{ TLogLevelRangeFilter --------------------------------------------------------}

{ Initialisation. }
constructor TLogLevelRangeFilter.Create(const MaxLevel, MinLevel: TLogLevel;
  const AcceptOnMatch: Boolean);
begin
  inherited Create(AcceptOnMatch);
  Self.MaxLevel := MaxLevel;
  Self.MinLevel := MinLevel;
end;

{ Check for the matching levels, then accept or deny based on the flag. }
function TLogLevelRangeFilter.Decide(const Event: TLogEvent):
  TLogFilterDecision;
begin
  if (MaxLevel <> nil) and (MaxLevel.Level >= Event.Level.Level) and
      (MinLevel <> nil) and (MinLevel.Level <= Event.Level.Level) then
    Result := Acceptance[AcceptOnMatch]
  else
    Result := fdNeutral;
end;

{ Set matching levels from options. }
procedure TLogLevelRangeFilter.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = MaxOpt then
    FMaxLevel := TLogLevel.GetLevel(Value)
  else if Name = MinOpt then
    FMinLevel := TLogLevel.GetLevel(Value);
end;

{ TLogStringFilter ------------------------------------------------------------}

{ Initialisation. }
constructor TLogStringFilter.Create(const Match: string;
  const IgnoreCase: Boolean; const AcceptOnMatch: Boolean);
begin
  inherited Create(AcceptOnMatch);
  Self.Match      := Match;
  Self.IgnoreCase := IgnoreCase;
end;

{ Check for the matching string, then accept or deny based on the flag. }
function TLogStringFilter.Decide(const Event: TLogEvent): TLogFilterDecision;
var
  MatchOn, MatchWith: string;
begin
  if IgnoreCase then
  begin
    MatchOn   := LowerCase(Match);
    MatchWith := LowerCase(Event.Message);
  end
  else
  begin
    MatchOn   := Match;
    MatchWith := Event.Message;
  end;
  if Pos(MatchOn, MatchWith) > 0 then
    Result := Acceptance[AcceptOnMatch]
  else
    Result := fdNeutral;
end;

{ Set string value to match and case-sensitivity from options. }
procedure TLogStringFilter.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if Name = MatchOpt then
    FMatch := Value
  else if Name = IgnoreCaseOpt then
    FIgnoreCase := StrToBool(Value, FIgnoreCase);
end;

{ TLogCustomAppender ----------------------------------------------------------}

constructor TLogCustomAppender.Create(const Name: string;
  const Layout: ILogLayout);
begin
  inherited Create;
  FName := Name;
  if Layout <> nil then
    FLayout := Layout
  else
    FLayout := TLogSimpleLayout.Create;
end;

destructor TLogCustomAppender.Destroy;
begin
  Close;
  FFilters.Free;
  DeleteCriticalSection(FCriticalAppender);
  inherited Destroy;
end;

{ Add a filter to the end of the filter list. }
procedure TLogCustomAppender.AddFilter(const Filter: ILogFilter);
begin
  if FFilters.IndexOf(Filter) = -1 then
    FFilters.Add(Filter);
end;

{ Log in appender-specific way. }
procedure TLogCustomAppender.Append(const Event: TLogEvent);
begin
  EnterCriticalSection(FCriticalAppender);
  try
    if isAsSevereAsThreshold(Event.Level) then
      if CheckEntryConditions then
        if CheckFilters(Event) then
          DoAppend(Event);
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;

{ Only log if not closed and a layout is available. }
function TLogCustomAppender.CheckEntryConditions: Boolean;
begin
  Result := False;
  if FClosed then
  begin
    LogLog.Warn(ClosedAppenderMsg);
    Exit;
  end;
  if (Layout = nil) and RequiresLayout then
  begin
    ErrorHandler.Error(Format(NoLayoutMsg, [Name]));
    Exit;
  end;
  Result := True;
end;

{ Only log if any/all filters allow it. }
function TLogCustomAppender.CheckFilters(const Event: TLogEvent): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to FFilters.Count - 1 do
    case ILogFilter(FFilters[Index]).Decide(Event) of
      fdAccept:  begin
                   Result := True;
                   Exit;
                 end;
      fdDeny:    begin
                   Result := False;
                   Exit;
                 end;
      fdNeutral: { Try next one }
    end;
  Result := True;
end;

{ Release any resources allocated within the appender such as file
  handles, network connections, etc.
  It is a programming error to append to a closed appender. }
procedure TLogCustomAppender.Close;
begin
  EnterCriticalSection(FCriticalAppender);
  try
    if FClosed then
      Exit;
    WriteFooter;
    FClosed := True;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;

procedure TLogCustomAppender.DoAppend(const Event: TLogEvent);
begin
  DoAppend(Layout.Format(Event));
end;

{ Returns the error handler for this appender. }
function TLogCustomAppender.GetErrorHandler: ILogErrorHandler;
begin
  Result := FErrorHandler;
end;

{ Returns the filters for this appender. }
function TLogCustomAppender.GetFilters: TInterfaceList;
begin
  Result := FFilters;
end;

{ Returns this appender's layout. }
function TLogCustomAppender.GetLayout: ILogLayout;
begin
  Result := FLayout;
end;

{ Get the name of this appender. The name uniquely identifies the appender. }
function TLogCustomAppender.GetName: string;
begin
  Result := FName;
end;

{ Initialisation. }
procedure TLogCustomAppender.Init;
begin
  inherited Init;
  InitializeCriticalSection(FCriticalAppender);
  FClosed       := False;
  FErrorHandler := TLogOnlyOnceErrorHandler.Create;
  FFilters      := TInterfaceList.Create;
  FThreshold    := All;
end;

{ Clear the list of filters by removing all the filters in it. }
procedure TLogCustomAppender.RemoveAllFilters;
begin
  FFilters.Clear;
end;

{ Delete a filter from the appender's list. }
procedure TLogCustomAppender.RemoveFilter(const Filter: ILogFilter);
begin
  FFilters.Remove(Filter);
end;

{ Configurators call this method to determine if the appender requires
  a layout. If this method returns True, meaning that a layout is required,
  then the configurator will configure a layout using the configuration
  information at its disposal.  If this method returns False, meaning that
  a layout is not required, then layout configuration will be used if available. }
function TLogCustomAppender.RequiresLayout: Boolean;
begin
  Result := True;
end;

{$IFDEF UNICODE}
function TLogCustomAppender.GetEncoding: TEncoding;
begin
  if (FEncoding = nil) then
    Result := TEncoding.Default
  else
    Result := FEncoding;
end;

procedure TLogCustomAppender.SetEncoding(const Value: TEncoding);
begin
  if (FEncoding <> nil) and not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  FEncoding := Value;
end;
{$ENDIF UNICODE}

{ Set the error handler for this appender - it cannot be nil. }
procedure TLogCustomAppender.SetErrorHandler(
  const ErrorHandler: ILogErrorHandler);
begin
  EnterCriticalSection(FCriticalAppender);
  try
    if ErrorHandler = nil then
      LogLog.Warn(NilErrorHandlerMsg)
    else
      FErrorHandler := ErrorHandler;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;

{ Set the layout for this appender. }
procedure TLogCustomAppender.SetLayout(const Layout: ILogLayout);
begin
  FLayout := Layout;
end;

{ Set the name of this appender. The name is used by other
  components to identify this appender. }
procedure TLogCustomAppender.SetName(const Name: string);
begin
  FName := Name;
end;

procedure TLogCustomAppender.WriteFooter;
begin
  if Layout <> nil then
    DoAppend(Layout.Footer);
end;

procedure TLogCustomAppender.WriteHeader;
begin
  if Layout <> nil then
    DoAppend(Layout.Header);
end;

procedure TLogCustomAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  if (Name = ThresholdOpt) and (Value <> '') then
  begin
    FThreshold := TLogLevel.GetLevel(Value, All);
  end
  {$IFDEF UNICODE}
  else if (Name = EncodingOpt) then
  begin
    Encoding := FindEncodingFromName(Value);
  end
  {$ENDIF}
end;

function TLogCustomAppender.isAsSevereAsThreshold(level: TLogLevel): boolean;
begin
  Result := not ((FThreshold <> nil) and (level.Level < FThreshold.Level));
end;

{ TLogNullAppender ------------------------------------------------------------}

{ Do nothing. }
procedure TLogNullAppender.DoAppend(const Message: string);
begin //FI:W519 - ignore FixInsight warning
end;

{ TLogODSAppender -------------------------------------------------------------}

{ Log to debugging output. }
procedure TLogODSAppender.DoAppend(const Message: string);
begin
  OutputDebugString(PChar(Message));
end;

{ TLogStreamAppender ----------------------------------------------------------}

constructor TLogStreamAppender.Create(const Name: string; const Stream: TStream;
  const Layout: ILogLayout);
begin
  inherited Create(Name, Layout);
  FStream := Stream;
end;

destructor TLogStreamAppender.Destroy;
begin
  Close;
  FStream.Free;
  inherited Destroy;
end;

{ Log to the attached stream. }
procedure TLogStreamAppender.DoAppend(const Message: string);
var
  StrStream: TStringStream;
begin
  if FStream <> nil then
  begin
    {$IFDEF UNICODE}
    StrStream := TStringStream.Create(Message, Encoding, False);
    {$ELSE}
    StrStream := TStringStream.Create(Message);
    {$ENDIF}
    try
      FStream.CopyFrom(StrStream, 0);
    finally
      StrStream.Free;
    end;
  end;
end;

{ TLogFileAppender ------------------------------------------------------------}

{ Create a file stream and delegate to the parent class. }
constructor TLogFileAppender.Create(const Name, FileName: string;
  const Layout: ILogLayout; const Append: Boolean);
begin
  inherited Create(Name, nil, Layout);
  FAppend := Append;
  SetOption(FileNameOpt, FileName);
end;

{ create file stream }
procedure TLogFileAppender.SetLogFile(const Name: string);
var
  strPath: string;
  f : TextFile;
begin
  CloseLogFile;
  FFileName := Name;
  if FAppend and FileExists(FFileName) then
  begin
    // append to existing file
    // note that we replace fmShareDenyWrite with fmShareDenyNone for concurrent logging possibility
    FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
    FStream.Seek(0, soFromEnd);
  end
  else
  begin
    // Check if directory exists
    strPath := ExtractFileDir(FFileName);
    if (strPath <> '') and  not DirectoryExists(strPath) then
      ForceDirectories(strPath);

    //FIX 04.10.2006 MHoenemann:
    //  SysUtils.FileCreate() ignores any sharing option (like our fmShareDenyWrite),
    // Creating new file
    AssignFile(f, FFileName);
    try
      ReWrite(f);
    finally
      CloseFile(f);
    end;
    // now use this file
    FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
  end;
  WriteHeader;
end;

{ close file stream }
procedure TLogFileAppender.CloseLogFile;
begin
  if FStream <> nil then
    FreeAndNil(FStream);
end;

procedure TLogFileAppender.SetOption(const Name, Value: string);
begin
  inherited SetOption(Name, Value);
  EnterCriticalSection(FCriticalAppender);
  try
    if (Name = AppendOpt) and (Value <> '') then
    begin
      FAppend := StrToBool(Value, FAppend);
    end
    else if (Name = FileNameOpt) and (Value <> '') then
    begin
      SetLogFile(Value);    // changed by adasen
    end;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;
end;

{ TLogRollingFileAppender }

procedure TLogRollingFileAppender.DoAppend(const msg: string);
begin
  if assigned(FStream) and (FCurrentSize = 0) then
    FCurrentSize := FStream.Size;
  FCurrentSize := FCurrentSize + Length(msg);   // should be faster than TFileStream.Size
  if (FStream <> nil) and (FCurrentSize > FMaxFileSize) then
  begin
    FCurrentSize := 0;
    RollOver;
  end;
  inherited;
end;

{ set defaults }
procedure TLogRollingFileAppender.Init;
begin
  inherited;
  FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
  FMaxBackupIndex := DEFAULT_MAX_BACKUP_INDEX;
end;

{ log file rotation }
procedure TLogRollingFileAppender.RollOver;
var i : integer;
    filename : string;
begin
  // If maxBackups <= 0, then there is no file renaming to be done.
  if FMaxBackupIndex > 0 then
  begin
    // Delete the oldest file, to keep Windows happy.
    DeleteFile(FFileName + '.' + IntToStr(FMaxBackupIndex));
    // Map (maxBackupIndex - 1), ..., 2, 1 to maxBackupIndex, ..., 3, 2
    for i := FMaxBackupIndex - 1 downto 1 do
    begin
      filename := FFileName + '.' + IntToStr(i);
      if FileExists(filename) then
        RenameFile(filename, FFileName + '.' + IntToStr(i+1));
    end;
    // close file
    CloseLogFile;
    // Rename fileName to fileName.1
    RenameFile(FFileName, FFileName + '.1');
    // open new file
    SetLogFile(FFileName);
  end;
end;

procedure TLogRollingFileAppender.SetOption(const Name, Value: string);
var suffix : string;
begin
  inherited SetOption(Name, Value);
  EnterCriticalSection(FCriticalAppender);
  try
    if (Name = MaxFileSizeOpt) and (Value <> '') then
    begin
      // check suffix
      suffix := Copy(Value, Length(Value)-1, 2);
      if suffix = 'KB' then
        FMaxFileSize := StrToIntDef(Copy(Value, 1, Length(Value)-2), 0) * 1024
      else if suffix = 'MB' then
        FMaxFileSize := StrToIntDef(Copy(Value, 1, Length(Value)-2), 0) * 1024 * 1024
      else if suffix = 'GB' then
        FMaxFileSize := StrToIntDef(Copy(Value, 1, Length(Value)-2), 0) * 1024 * 1024 * 1024
      else
        FMaxFileSize := StrToIntDef(Value, 0);
      if FMaxFileSize = 0 then
        FMaxFileSize := DEFAULT_MAX_FILE_SIZE;
    end
    else if (Name = MaxBackupIndexOpt) and (Value <> '') then
    begin
      FMaxBackupIndex := StrToIntDef(Value, DEFAULT_MAX_BACKUP_INDEX);
    end;
  finally
    LeaveCriticalSection(FCriticalAppender);
  end;

end;

{ OptionConvertors ------------------------------------------------------------}

{ Convert string value to Boolean, with default. }
function StrToBool(Value: string; const Default: Boolean): Boolean;
begin
  Value := LowerCase(Value);
  if (Value = 'true') or (Value = 'yes') then
    Result := True
  else if (Value = 'false') or (Value = 'no') then
    Result := False
  else
    Result := Default;
end;

{$IFDEF UNICODE}
function FindEncodingFromName(const Name: string): TEncoding;
begin
  Result := nil;
  if SameText(Name, SANSIEncoding) then
    Result := TEncoding.Default
  else if SameText(Name, SASCIIEncoding) then
    Result := TEncoding.ASCII
  else if SameText(Name, SUnicodeEncoding) then
    Result := TEncoding.Unicode
  else if SameText(Name, SBigEndianEncoding) then
    Result := TEncoding.BigEndianUnicode
  else if SameText(Name, SUTF7Encoding) then
    Result := TEncoding.UTF7
  else if SameText(Name, SUTF8Encoding) then
    Result := TEncoding.UTF8;
end;
{$ENDIF UNICODE}


{ TAppender -------------------------------------------------------------------}

type
  { Holder for an appender reference. }
  TAppender = class(TObject)
  public
    Appender: ILogAppender;
    constructor Create(Appender: ILogAppender);
  end;

constructor TAppender.Create(Appender: ILogAppender);
begin
  inherited Create;
  Self.Appender := Appender;
end;

{ TLogBasicConfigurator -------------------------------------------------------}

constructor TLogBasicConfigurator.Create;
begin
  inherited Create;
  FLoggerFactory := TLogDefaultLoggerFactory.Create;
  FRegistry      := TStringList.Create;
end;

destructor TLogBasicConfigurator.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FRegistry.Count - 1 do
    FRegistry.Objects[Index].Free;
  FRegistry.Free;
  inherited Destroy;
end;

{ Used by subclasses to add a renderer to the hierarchy passed as parameter. }
procedure TLogBasicConfigurator.AddRenderer(const Hierarchy: TLogHierarchy;
  const RenderedName, RendererName: string);
var
  Rendered: TClass;
  Renderer: ILogRenderer;
begin
  LogLog.Debug(RendererMsg, [RendererName, RenderedName]);
  Rendered := FindRendered(RenderedName);
  Renderer := FindRenderer(RendererName);
  if Rendered = nil then
  begin
    LogLog.Error(NoRenderedCreatedMsg, [RenderedName]);
    Exit;
  end;
  if Renderer = nil then
  begin
    LogLog.Error(NoRendererCreatedMsg, [RendererName]);
    Exit;
  end;

  Hierarchy.AddRenderer(Rendered, Renderer);
end;

{ Return a reference to an already defined named appender, or nil if none. }
function TLogBasicConfigurator.AppenderGet(const Name: string): ILogAppender;
var
  Index: Integer;
begin
  Index := FRegistry.IndexOf(Name);
  if Index = -1 then
    Result := nil
  else
    Result := TAppender(FRegistry.Objects[Index]).Appender;
end;

{ Save reference to named appender. }
procedure TLogBasicConfigurator.AppenderPut(const Appender: ILogAppender);
begin
  FRegistry.AddObject(Appender.Name, TAppender.Create(Appender));
end;

{ Add appender to the root logger. If no appender is provided,
  add a TLogODSAppender that uses TLogPatternLayout with the
  TTCCPattern and prints to debugging output for the root logger. }
class procedure TLogBasicConfigurator.Configure(const Appender: ILogAppender);
var
  NewAppender: ILogAppender;
begin
  if Appender = nil then
    NewAppender := TLogODSAppender.Create('ODS',
      TLogPatternLayout.Create(TTCCPattern))
  else
    NewAppender := Appender;
  DefaultHierarchy.Root.AddAppender(NewAppender);
end;

{ Reset the default hierarchy to its default. }
class procedure TLogBasicConfigurator.ResetConfiguration;
begin
  DefaultHierarchy.ResetConfiguration;
end;

{ Initialise standard global settings. }
procedure TLogBasicConfigurator.SetGlobalProps(const Hierarchy: TLogHierarchy;
  const FactoryClassName, Debug, Threshold: string);
begin
  if FactoryClassName <> '' then
  begin
    FLoggerFactory := FindLoggerFactory(FactoryClassName);
    if FLoggerFactory <> nil then
      LogLog.Debug(LoggerFactoryMsg, [FactoryClassName])
    else
      FLoggerFactory := TLogDefaultLoggerFactory.Create;
  end;

  if Debug <> '' then
    LogLog.InternalDebugging := StrToBool(Debug, False);

  if Threshold <> '' then
    DefaultHierarchy.Threshold := TLogLevel.GetLevel(LowerCase(Threshold));
end;

{ TLogPropertyConfigurator ----------------------------------------------------}

{ Split the supplied value into tokens with specified delimiters. }
procedure Tokenise(const Value: string; const Items: TStringList;
  const Delimiters: string);
var
  Index: Integer;
  Item: string;
begin
  Item := '';
  for Index := 1 to Length(Value) do
    if Pos(Value[Index], Delimiters) > 0 then
    begin
      Items.Add(Item);
      Item := '';
    end
    else
      Item := Item + Value[Index];
  if Item <> '' then
    Items.Add(Item);
end;

{ Extract properties with the given prefix from the supplied list
  and send them to the option handler. }
procedure SetSubProps(const Prefix: string; const Props: TStringList;
  const Handler: ILogOptionHandler);
var
  Index: Integer;
begin
  for Index := 0 to Props.Count - 1 do
    if Pos(Prefix, Props.Names[Index]) = 1 then
      Handler.Options[Copy(Props.Names[Index], Length(Prefix) + 2, 255)] :=
        Props.Values[Props.Names[Index]];
end;

{ Read configuration options from a file.
  See DoConfigure for the expected format. }
class procedure TLogPropertyConfigurator.Configure(const Filename: string);
var
  Config: TLogPropertyConfigurator;
begin
  Config := TLogPropertyConfigurator.Create;
  try
    Config.DoConfigure(Filename, DefaultHierarchy);
  finally
    Config.Free;
  end;
end;

{ Read configuration options from properties.
  See DoConfigure for the expected format. }
class procedure TLogPropertyConfigurator.Configure(const Props: TStringList);
var
  Config: TLogPropertyConfigurator;
begin
  Config := TLogPropertyConfigurator.Create;
  try
    Config.DoConfigure(Props, DefaultHierarchy);
  finally
    Config.Free;
  end;
end;

procedure TLogPropertyConfigurator.ConfigureRootLogger(const Props: TStringList;
  const Hierarchy: TLogHierarchy);
var
  Value: string;
begin
  Value := Props.Values[RootLoggerKey];
  if Value = '' then
    LogLog.Debug(NoRootLoggerMsg)
  else
    ParseLogger(Props, Hierarchy.Root, Value);
end;

{ Read configuration options from a file.
  See DoConfigure below for the expected format. }
procedure TLogPropertyConfigurator.DoConfigure(const FileName: string;
  const Hierarchy: TLogHierarchy);
var
  Props: TStringList;
begin
  Props := TStringList.Create;
  try
    try
      Props.LoadFromFile(FileName);
      DoConfigure(Props, Hierarchy);
    except on Ex: Exception do
      begin
        LogLog.Error(BadConfigFileMsg, [FileName, Ex.Message]);
        LogLog.Error(IgnoreConfigMsg, [FileName]);
      end;
    end;
  finally
    Props.Free;
  end;
end;

{ Read configuration from properties. The existing configuration is not
  cleared nor reset. If you require a different behaviour, then call
  ResetConfiguration method before calling Configure.

  The configuration file consists of entries in the format key=value.

  Global Settings

  Where level is used below it indicates one of the following values:

  all|fatal|error|warn|info|debug|off|<custom level name>

  To set a global threshold for all loggers use the following syntax
  (defaults to all):

  log4d.threshold=level

  Logging of internal debugging events can be enabled with the following:

  log4d.configDebug=true

  Appender configuration

  Appender configuration syntax is:

  # For appender named appenderName, set its class.
  log4d.appender.appenderName=nameOfAppenderClass

  # Set appender specific options.
  log4d.appender.appenderName.option1=value1
    :
  log4d.appender.appenderName.optionN=valueN

  For each named appender you can configure its ErrorHandler.
  The syntax for configuring an appender's error handler is:

  log4d.appender.appenderName.errorHandler=nameOfErrorHandlerClass

  For each named appender you can configure its Layout.
  The syntax for configuring an appender's layout and its options is:

  log4d.appender.appenderName.layout=nameOfLayoutClass
  log4d.appender.appenderName.layout.option1=value1
    :
  log4d.appender.appenderName.layout.optionN=valueN

  For each named appender you can configure its Filters. The syntax for
  configuring an appender's filters is (where x is a sequential number):

  log4d.appender.appenderName.filterx=nameOfFilterClass
  log4d.appender.appenderName.filterx.option1=value1
    :
  log4d.appender.appenderName.filterx.optionN=valueN

  Configuring loggers

  The syntax for configuring the root logger is:

  log4d.rootLogger=[level],appenderName[,appenderName]...

  This syntax means that one of the level values (e.g. error, info, or
  debug) can be supplied followed by appender name(s) separated by commas.

  If one of the optional level values is given, the root level is set
  to the corresponding level. If no level value is specified,
  then the root level remains untouched.

  The root logger can be assigned multiple appenders.

  Each appenderName (separated by commas) will be added to the root logger.
  The named appender is defined using the appender syntax defined above.

  For non-root loggers the syntax is almost the same:

  log4d.logger.loggerName=[inherited|level],appenderName[,appenderName]...

  Thus, one of the usual level values can be optionally specified. For any
  of these values the named logger is assigned the corresponding level.
  In addition however, the value "inherited" can be optionally specified which
  means that named logger should inherit its level from the logger hierarchy.

  If no level value is supplied, then the level of the
  named logger remains untouched.

  By default loggers inherit their level from the hierarchy.
  However, if you set the level of a logger and later decide
  that that logger should inherit its level, then you should
  specify "inherited" as the value for the level value.

  Similar to the root logger syntax, each appenderName
  (separated by commas) will be attached to the named logger.

  Logger additivity is set in the following fashion:

  log4d.additive.loggerName=true|false

  ObjectRenderers

  You can customise the way message objects of a given type are converted to
  a string before being logged. This is done by specifying an object renderer
  for the object type would like to customise. The syntax is:

  log4d.renderer.nameOfRenderedClass=nameOfRenderingClass

  As in,

  log4d.renderer.TFruit=TFruitRenderer

  Class Factories

  In case you are using your own sub-types of the TLogLogger class and
  wish to use configuration files, then you must set the LoggerFactory
  for the sub-type that you are using. The syntax is:

  log4d.loggerFactory=nameOfLoggerFactoryClass

  Example

  An example configuration is given below.

  # Set internal debugging
  log4d.configDebug=true

  # Global logging level - don't show debug events
  log4d.threshold=info

  # Set logger factory - this is the default anyway
  log4d.loggerFactory=TLogDefaultLoggerFactory

  # Set root level to log warnings and above - sending to appender ODS
  log4d.rootLogger=warn,ODS

  # Establish logger hierarchy
  # 'myapp' inherits its level from root
  log4d.logger.myapp=inherited,Mem1
  # 'myapp.more' displays all messages (from debug up)
  log4d.logger.myapp.more=debug,Mem2
  # 'myapp.other' doesn't display debug messages
  log4d.logger.myapp.other=info,Mem3
  # 'alt' only displays error and fatal messages
  log4d.logger.alt=error,Mem4,Fil1

  # 'myapp.other' logger doesn't log to its parents - others do
  log4d.additive.myapp.other=false

  # Create root appender - logging to debugging output
  log4d.appender.ODS=TLogODSAppender
  # Using the simple layout, i.e. message only
  log4d.appender.ODS.layout=TLogSimpleLayout

  # Create memo appenders, with layouts
  log4d.appender.Mem1=TMemoAppender
  # Specify the name of the memo component to attach to
  log4d.appender.Mem1.memo=memMyapp
  # Use a pattern layout
  log4d.appender.Mem1.layout=TLogPatternLayout
  # With the specified pattern: runtime (in field of 7 characters),
  # thread id (left justified in field of 8 characters), level,
  # logger, NDC, message, and a new line
  log4d.appender.Mem1.layout.pattern=%7r [%-8t] %p %c %x - %m%n
  # Add a string filter
  log4d.appender.Mem1.filter1=TLogStringFilter
  # That matches on 'x'
  log4d.appender.Mem1.filter1.match=x
  # And accepts all messages containing it
  log4d.appender.Mem1.filter1.acceptOnMatch=false
  # Add a second string filter
  log4d.appender.Mem1.filter2=TLogStringFilter
  # That matches on 'y'
  log4d.appender.Mem1.filter2.match=y
  # And discards all messages containing it
  # Note: messages with 'x' and 'y' will be logged as filter 1 is checked first
  log4d.appender.Mem1.filter2.acceptOnMatch=false

  log4d.appender.Mem2=TMemoAppender
  log4d.appender.Mem2.memo=memMyappMore
  log4d.appender.Mem2.layout=TLogSimpleLayout

  log4d.appender.Mem3=TMemoAppender
  log4d.appender.Mem3.memo=memMyappOther
  log4d.appender.Mem3.layout=TLogHTMLLayout

  log4d.appender.Mem4=TMemoAppender
  log4d.appender.Mem4.memo=memAlt
  log4d.appender.Mem4.layout=TLogPatternLayout
  log4d.appender.Mem4.layout.pattern=>%m<%n

  # Create a file appender
  log4d.appender.Fil1=TLogFileAppender
  log4d.appender.Fil1.filename=C:\Temp\Log4D.log
  log4d.appender.Fil1.errorHandler=TLogOnlyOnceErrorHandler
  log4d.appender.Fil1.layout=TLogPatternLayout
  log4d.appender.Fil1.layout.pattern=%r [%t] %p %c %x - %m%n

  # Nominate renderers - when objects of type TEdit are presented,
  # use TComponentRenderer to display them
  log4d.renderer.TEdit=TComponentRenderer

  Use the # character at the beginning of a line for comments. }
procedure TLogPropertyConfigurator.DoConfigure(const Props: TStringList;
  const Hierarchy: TLogHierarchy);
begin
  SetGlobalProps(Hierarchy, Props.Values[LoggerFactoryKey],
    Props.Values[DebugKey], Props.Values[ThresholdKey]);

  ConfigureRootLogger(Props, Hierarchy);
  ParseLoggersAndRenderers(Props, Hierarchy);

  LogLog.Debug(FinishedConfigMsg, [ClassName]);
end;

const
  Bool: array [Boolean] of string = ('false', 'true');

{ Parse the additivity option for a non-root logger. }
procedure TLogPropertyConfigurator.ParseAdditivityForLogger(
  const Props: TStringList; const Logger: TLogLogger);
var
  Value: string;
begin
  Value := Props.Values[AdditiveKey + Logger.Name];
  LogLog.Debug(HandlingAdditivityMsg,
    [AdditiveKey + Logger.Name, Value]);
  { Touch additivity only if necessary }
  if Value <> '' then
  begin
    Logger.Additive := StrToBool(Value, True);
    LogLog.Debug(SettingAdditivityMsg,
      [Logger.Name, Bool[Logger.Additive]]);
  end;
end;

{ Parse entries for an appender and its constituents. }
function TLogPropertyConfigurator.ParseAppender(const Props: TStringList;
  const AppenderName: string): ILogAppender;
var
  Prefix, SubPrefix: string;
  ErrorHandler: ILogErrorHandler;
  Layout: ILogLayout;
  Filter: ILogFilter;
  Index: Integer;
begin
  Result := AppenderGet(AppenderName);
  if Result <> nil then
  begin
    LogLog.Debug(AppenderDefinedMsg, [AppenderName]);
    Exit;
  end;

  { Appender was not previously initialised. }
  Prefix := AppenderKey + AppenderName;
  Result := FindAppender(Props.Values[Prefix]);
  if Result = nil then
  begin
    LogLog.Error(NoAppenderCreatedMsg, [AppenderName]);
    Exit;
  end;

  Result.Name := AppenderName;

  { Process any error handler entry. }
  SubPrefix    := Prefix + ErrorHandlerKey;
  ErrorHandler := FindErrorHandler(Props.Values[SubPrefix]);
  if ErrorHandler <> nil then
  begin
    Result.ErrorHandler := ErrorHandler;
    LogLog.Debug(ParsingErrorHandlerMsg, [AppenderName]);
    SetSubProps(SubPrefix, Props, ErrorHandler);
    LogLog.Debug(EndErrorHandlerMsg, [AppenderName]);
  end;

  { Process any layout entry. }
  SubPrefix := Prefix + LayoutKey;
  Layout    := FindLayout(Props.Values[SubPrefix]);
  if Layout <> nil then
  begin
    Result.Layout := Layout;
    LogLog.Debug(ParsingLayoutMsg, [AppenderName]);
    SetSubProps(SubPrefix, Props, Layout);
    LogLog.Debug(EndLayoutMsg, [AppenderName]);
  end;
  if Result.RequiresLayout and (Result.Layout = nil) then
    LogLog.Error(LayoutRequiredMsg, [AppenderName]);

  { Process any filter entries. }
  SubPrefix := Prefix + FilterKey;
  for Index := 0 to Props.Count - 1 do
    if (Copy(Props.Names[Index], 1, Length(SubPrefix)) = SubPrefix) and
        (Pos('.', Copy(Props.Names[Index], Length(SubPrefix), 255)) = 0) then
    begin
      Filter := FindFilter(Props.Values[Props.Names[Index]]);
      if Filter = nil then
        Continue;

      Result.AddFilter(Filter);
      LogLog.Debug(ParsingFiltersMsg, [AppenderName]);
      SetSubProps(Props.Names[Index], Props, Filter);
      LogLog.Debug(EndFiltersMsg, [AppenderName]);
    end;

  { Set any options for the appender. }
  SetSubProps(Prefix, Props, Result);

  LogLog.Debug(EndAppenderMsg, [AppenderName]);
  AppenderPut(Result);
end;

{ Parse non-root elements, such as non-root loggers and renderers. }
procedure TLogPropertyConfigurator.ParseLoggersAndRenderers(
  const Props: TStringList; const Hierarchy: TLogHierarchy);
var
  Index: Integer;
  Key, Name: string;
  Logger: TLogLogger;
begin
  for Index := 0 to Props.Count - 1 do
  begin
    Key := Props.Names[Index];
    if Copy(Key, 1, Length(LoggerKey)) = LoggerKey then
    begin
      Name   := Copy(Key, Length(LoggerKey) + 1, 255);
      Logger := Hierarchy.GetLogger(Name, FLoggerFactory);
      Logger.LockLogger;
      try
        ParseLogger(Props, Logger, Props.Values[Key]);
        ParseAdditivityForLogger(Props, Logger);
      finally
        Logger.UnlockLogger;
      end;
    end
    else if Copy(Key, 1, Length(RendererKey)) = RendererKey then
      AddRenderer(Hierarchy,
        Copy(Key, Length(RendererKey) + 1, 255), Props.Values[Key]);
  end;
end;

{ This method must work for the root logger as well. }
procedure TLogPropertyConfigurator.ParseLogger(const Props: TStringList;
  const Logger: TLogLogger; const Value: string);
var
  Appender: ILogAppender;
  Index: Integer;
  Items: TStringList;
begin
  LogLog.Debug(ParsingLoggerMsg, [Logger.Name, Value]);
  Items := TStringList.Create;
  try
    { We must skip over ',' but not white space }
    Tokenise(Value, Items, ',');
    if Items.Count = 0 then
      Exit;
    { If value is not in the form ", appender.." or "", then we should set
      the level of the logger. }
    if Items[0] <> '' then
    begin
      LogLog.Debug(LevelTokenMsg, [Items[0]]);

      { If the level value is inherited, set logger level value to nil.
        We also check that the user has not specified inherited for the
        root logger. }
      if (LowerCase(Items[0]) = InheritedLevel) and
          (Logger.Name <> InternalRootName) then
        Logger.Level := nil
      else
        Logger.Level := TLogLevel.GetLevel(LowerCase(Items[0]));
      LogLog.Debug(SettingLevelMsg, [Logger.Name, Logger.Level.Name]);
    end;

    { Remove all existing appenders. They will be reconstructed below. }
    Logger.RemoveAllAppenders;

    for Index := 1 to Items.Count - 1 do
    begin
      if Items[Index] = '' then
        Continue;
      LogLog.Debug(ParsingAppenderMsg, [Items[Index]]);
      Appender := ParseAppender(Props, Items[Index]);
      if Appender <> nil then
        Logger.AddAppender(Appender);
    end;
  finally
    Items.Free;
  end;
end;

{ Registration ----------------------------------------------------------------}

{ Register a class as an implementor of a particular interface. }
procedure RegisterClass(ClassType: TClass; InterfaceType: TGUID;
  const InterfaceName: string; Names: TStringList; Classes: TClassList);
var
  Index: Integer;
begin
  if ClassType.GetInterfaceEntry(InterfaceType) = nil then
    raise ELogException.Create(Format(InterfaceNotImplMsg,
      [ClassType.ClassName, InterfaceName]));

  Index := Names.IndexOf(ClassType.ClassName);
  if Index = -1 then
  begin
    Names.Add(ClassType.ClassName);
    Classes.Add(ClassType);
  end
  else
    Classes[Index] := ClassType;
end;

{ Create a new instance of a class implementing a particular interface. }
function FindClass(const ClassName: string; InterfaceType: TGUID;
  Names: TStringList; Classes: TClassList): IUnknown;
var
  Index: Integer;
  Creator: ILogDynamicCreate;
begin
  if ClassName = '' then
    Exit;

  Index := Names.IndexOf(ClassName);
  if Index = -1 then
  begin
    LogLog.Error(NoClassMsg, [ClassName]);
    Result := nil;
  end
  else
  begin
{$IFDEF VER120}
    TClass(Classes[Index]).Create.GetInterface(InterfaceType, Result);
{$ELSE}
    Classes[Index].Create.GetInterface(InterfaceType, Result);
{$ENDIF}
    Result.QueryInterface(ILogDynamicCreate, Creator);
    if Creator <> nil then
      Creator.Init;
  end;
end;

var
  AppenderNames: TStringList;
  AppenderClasses: TClassList;

procedure RegisterAppender(const Appender: TClass);
begin
  RegisterClass(Appender, ILogAppender, 'ILogAppender',
    AppenderNames, AppenderClasses);
end;

function FindAppender(const ClassName: string): ILogAppender;
begin
  Result := FindClass(ClassName, ILogAppender, AppenderNames, AppenderClasses)
    as ILogAppender;
end;

var
  LoggerFactoryNames: TStringList;
  LoggerFactoryClasses: TClassList;

procedure RegisterLoggerFactory(const LoggerFactory: TClass);
begin
  RegisterClass(LoggerFactory, ILogLoggerFactory, 'ILogLoggerFactory',
    LoggerFactoryNames, LoggerFactoryClasses);
end;

function FindLoggerFactory(const ClassName: string): ILogLoggerFactory;
begin
  Result := FindClass(ClassName, ILogLoggerFactory,
    LoggerFactoryNames, LoggerFactoryClasses) as ILogLoggerFactory;
end;

var
  ErrorHandlerNames: TStringList;
  ErrorHandlerClasses: TClassList;

procedure RegisterErrorHandler(const ErrorHandler: TClass);
begin
  RegisterClass(ErrorHandler, ILogErrorHandler, 'ILogErrorHandler',
    ErrorHandlerNames, ErrorHandlerClasses);
end;

function FindErrorHandler(const ClassName: string): ILogErrorHandler;
begin
  Result := FindClass(ClassName, ILogErrorHandler, ErrorHandlerNames,
    ErrorHandlerClasses) as ILogErrorHandler;
end;

var
  FilterNames: TStringList;
  FilterClasses: TClassList;

procedure RegisterFilter(const Filter: TClass);
begin
  RegisterClass(Filter, ILogFilter, 'ILogFilter', FilterNames, FilterClasses);
end;

function FindFilter(const ClassName: string): ILogFilter;
begin
  Result := FindClass(ClassName, ILogFilter, FilterNames, FilterClasses)
    as ILogFilter;
end;

var
  LayoutNames: TStringList;
  LayoutClasses: TClassList;

procedure RegisterLayout(const Layout: TClass);
begin
  RegisterClass(Layout, ILogLayout, 'ILogLayout', LayoutNames, LayoutClasses);
end;

function FindLayout(const ClassName: string): ILogLayout;
begin
  Result := FindClass(ClassName, ILogLayout, LayoutNames, LayoutClasses)
    as ILogLayout;
end;

var
  RenderedNames: TStringList;
  RenderedClasses: TClassList;

{ Register a class to be rendered. }
procedure RegisterRendered(const Rendered: TClass);
var
  Index: Integer;
begin
  Index := RenderedNames.IndexOf(Rendered.ClassName);
  if Index = -1 then
  begin
    RenderedNames.Add(Rendered.ClassName);
    RenderedClasses.Add(Rendered);
  end
  else
    RenderedClasses[Index] := Rendered;
end;

{ Return a reference to the named class. }
function FindRendered(const ClassName: string): TClass;
var
  Index: Integer;
begin
  Index := RenderedNames.IndexOf(ClassName);
  if Index = -1 then
  begin
    LogLog.Error(NoClassMsg, [ClassName]);
    Result := nil;
  end
  else
{$IFDEF VER120}
    Result := TClass(RenderedClasses[Index]);
{$ELSE}
    Result := RenderedClasses[Index];
{$ENDIF}
end;

var
  RendererNames: TStringList;
  RendererClasses: TClassList;

procedure RegisterRenderer(const Renderer: TClass);
begin
  RegisterClass(Renderer, ILogRenderer, 'ILogRenderer',
    RendererNames, RendererClasses);
end;

function FindRenderer(const ClassName: string): ILogRenderer;
begin
  Result := FindClass(ClassName, ILogRenderer,
    RendererNames, RendererClasses) as ILogRenderer;
end;

{$IFDEF LINUX}
procedure EnterCriticalSection(var CS: TCriticalSection);
begin
  CS.Enter;
end;

procedure LeaveCriticalSection(var CS: TCriticalSection);
begin
  CS.Leave;
end;

procedure InitializeCriticalSection(var CS: TCriticalSection);
begin
  CS := TCriticalSection.Create;
end;

procedure DeleteCriticalSection(var CS: TCriticalSection);
begin
  CS.Free;
end;

function GetCurrentThreadID: Integer;
begin
  Result := 0;
end;

procedure OutputDebugString(const S: PChar);
begin
  WriteLn(Trim(string(S)));
end;

{$ENDIF}

procedure LevelFree;
var Index : Integer;
begin
  for Index := 0 to Levels.Count - 1 do
    TObject(Levels[Index]).Free;
end;

procedure NDCFree;
var Index : Integer;
begin
  for Index := 0 to NDC.Count - 1 do
    NDC.Objects[Index].Free;
  NDC.Free;
end;

initialization
  { Timestamping. }
  StartTime := Now;
  { Synchronisation. }
  InitializeCriticalSection(CriticalNDC);
  { Standard levels. }
  Levels             := TObjectList.Create;
{$IFDEF VER130}
  Levels.OwnsObjects := True;
{$ENDIF}
{$IF CompilerVersion >= 14}
  Levels.OwnsObjects := True;
{$ENDIF}
  All   := TLogLevel.Create('all',   AllValue);
  Trace := TLogLevel.Create('trace', TraceValue);
  Debug := TLogLevel.Create('debug', DebugValue);
  Info  := TLogLevel.Create('info',  InfoValue);
  Warn  := TLogLevel.Create('warn',  WarnValue);
  Error := TLogLevel.Create('error', ErrorValue);
  Fatal := TLogLevel.Create('fatal', FatalValue);
  Off   := TLogLevel.Create('off',   OffValue);
  { NDC stack. }
  NDC        := TStringList.Create;
  NDC.Sorted := True;
  { Registration setup. }
  AppenderNames        := TStringList.Create;
  AppenderClasses      := TClassList.Create;
  ErrorHandlerNames    := TStringList.Create;
  ErrorHandlerClasses  := TClassList.Create;
  FilterNames          := TStringList.Create;
  FilterClasses        := TClassList.Create;
  LayoutNames          := TStringList.Create;
  LayoutClasses        := TClassList.Create;
  LoggerFactoryNames   := TStringList.Create;
  LoggerFactoryClasses := TClassList.Create;
  RenderedNames        := TStringList.Create;
  RenderedClasses      := TClassList.Create;
  RendererNames        := TStringList.Create;
  RendererClasses      := TClassList.Create;
  { Registration of standard implementations. }
  RegisterLoggerFactory(TLogDefaultLoggerFactory);
  RegisterErrorHandler(TLogFallbackErrorHandler);
  RegisterErrorHandler(TLogOnlyOnceErrorHandler);
  RegisterLayout(TLogHTMLLayout);
  RegisterLayout(TLogPatternLayout);
  RegisterLayout(TLogSimpleLayout);
  RegisterFilter(TLogDenyAllFilter);
  RegisterFilter(TLogLevelMatchFilter);
  RegisterFilter(TLogLevelRangeFilter);
  RegisterFilter(TLogStringFilter);
  RegisterAppender(TLogFileAppender);
  RegisterAppender(TLogNullAppender);
  RegisterAppender(TLogODSAppender);
  RegisterAppender(TLogStreamAppender);
  RegisterAppender(TLogRollingFileAppender);
  { Standard logger factory and hierarchy. }
  DefaultLoggerFactory := TLogDefaultLOggerFactory.Create;
  DefaultLoggerFactory._AddRef;
  DefaultHierarchy     := TLogHierarchy.Create(TLogRoot.Create(Error));
  { Internal logging }
  LogLog           := TLogLog.Create;
  LogLog.Hierarchy := DefaultHierarchy;

finalization

{$IFDEF VER120}
  LevelFree;
{$ENDIF}
  Levels.Free;
  DefaultLoggerFactory._Release;
  DefaultHierarchy.Free;
  { Registration cleanup. }
  AppenderNames.Free;
  AppenderClasses.Free;
  ErrorHandlerNames.Free;
  ErrorHandlerClasses.Free;
  FilterNames.Free;
  FilterClasses.Free;
  LayoutNames.Free;
  LayoutClasses.Free;
  LoggerFactoryNames.Free;
  LoggerFactoryClasses.Free;
  RenderedNames.Free;
  RenderedClasses.Free;
  RendererNames.Free;
  RendererClasses.Free;
  { NDC. }
  NDCFree;
  { Internal logging. }
  LogLog.Free;
  { Synchronisation. }
  DeleteCriticalSection(CriticalNDC);

end.