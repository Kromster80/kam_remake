unit KM_ServerEventHandler;

interface

uses
  SysUtils, KM_CommonTypes
  {$IFDEF MSWindows} ,Windows {$ENDIF}
  {$IFDEF Unix} ,LCLIntf, LCLType {$ENDIF}
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
var ExeDir: String;
begin
  Inherited Create;
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  fLog := TKMLog.Create(ExeDir+'KaM_Server_'+inttostr(GetTickCount)+'.log');
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
 
