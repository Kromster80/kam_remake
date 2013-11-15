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
    procedure ServerStatusMessage(const aData: UnicodeString);
  end;


implementation


constructor TKMServerEventHandler.Create;
begin
  inherited Create;
  ExeDir := ExtractFilePath(ParamStr(0));
  CreateDir(ExeDir + 'Logs');
  gLog := TKMLog.Create(ExeDir+'Logs'+PathDelim+'KaM_Server_'+FormatDateTime('yyyy-mm-d_hh-nn-ss-zzz',Now)+'.log'); //Don't delete old logs
  gLog.AddTime('Dedicated server event handler created');
end;


destructor TKMServerEventHandler.Destroy;
begin
  FreeAndNil(gLog);
end;


procedure TKMServerEventHandler.ServerStatusMessage(const aData: UnicodeString);
begin
  Writeln(FormatDateTime('yyyy-mm-dd hh-nn-ss ', Now) + aData);
  gLog.AddTime(aData);
end;


end.

