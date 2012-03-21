unit KM_ServerEventHandler;
interface
uses
  SysUtils, KM_Defaults, KM_Log
  {$IFDEF MSWindows} ,Windows {$ENDIF}
  ;

//We need a dummy event handler because Events can't be assigned to regular procedures (e.g. in a console application)
type
  TKMServerEventHandler = class
    constructor Create;
    destructor Destroy; override;
    procedure ServerStatusMessage(const aData: string);
  end;


implementation


constructor TKMServerEventHandler.Create;
begin
  Inherited Create;
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  CreateDir(ExeDir + 'Logs');
  fLog := TKMLog.Create(ExeDir+'Logs'+PathDelim+'KaM_Server_'+FormatDateTime('yyyy-mm-d_hh-nn-ss-zzz',Now)+'.log'); //Don't delete old logs
  fLog.AppendLog('Dedicated server event handler created');
end;


destructor TKMServerEventHandler.Destroy;
begin
  FreeAndNil(fLog);
end;


procedure TKMServerEventHandler.ServerStatusMessage(const aData: string);
begin
  Writeln(FormatDateTime('yyyy-mm-dd hh-nn-ss ',Now)+aData);
  fLog.AppendLog(aData);
end;


end.
 
