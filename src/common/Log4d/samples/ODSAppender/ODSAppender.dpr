program ODSAppender;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Log4D in '..\..\src\Log4D.pas';

var
  Logger: TLogLogger;

begin
  //https://mikejustin.wordpress.com/2012/09/12/delphi-and-free-pascal-logging-with-the-log4d-open-source-library/
  try
    // basic configuration - creates a TLogODSAppender
    // (ODS = OutputDebugString)
    TLogBasicConfigurator.Configure;

    // set the log level
    TLogLogger.GetRootLogger.Level := Trace;

    // create a named logger
    Logger := TLogLogger.GetLogger('exampleLogger');

    // write log messages
    Logger.Fatal('fatal output');
    Logger.Error('error output');
    Logger.Warn('warn output');
    Logger.Info('info output');
    Logger.Debug('debug output');
    Logger.Trace('trace output');

    ReadLn;

  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
