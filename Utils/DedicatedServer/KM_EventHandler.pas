unit KM_EventHandler;

interface

uses
  SysUtils;

//We need a dummy event handler because Events can't be assigned to regular procedures (e.g. in a console application)
type
  TKMEventHandler = class
    procedure ServerStatusMessage(const aData: string);
  end;

implementation

procedure TKMEventHandler.ServerStatusMessage(const aData: string);
begin
  Writeln(FormatDateTime('yyyy-mm-dd hh-nn-ss ',Now)+aData);
end;

end.
 