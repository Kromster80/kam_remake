unit KM_Client;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_ClientOverbyte;


type
  TKMClientControl = class
  private
    fClient:TKMClient;
    fConnected:boolean;
    fOnStatusMessage:TGetStrProc;
    fOnRecieveStr:TGetStrProc;
    procedure Error(const S: string);
    procedure Connected(Sender: TObject);
    procedure Disconnected(Sender: TObject);
    procedure RecieveStr(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectTo(const aAddress:string; const aPort:string);
    procedure Disconnect;
    procedure SendText(const aData:string);
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
    property OnRecieveStr:TGetStrProc write fOnRecieveStr;
  end;


implementation


constructor TKMClientControl.Create;
begin
  Inherited;
  fClient := TKMClient.Create;
  fConnected := false;
end;


destructor TKMClientControl.Destroy;
begin
  fClient.Free;
  Inherited;
end;


procedure TKMClientControl.Error(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Error '+S);
end;


procedure TKMClientControl.ConnectTo(const aAddress:string; const aPort:string);
begin
  fClient.OnError := Error;
  fClient.OnSessionDisconnected := Disconnected;
  fClient.OnSessionConnected := Connected;
  fClient.OnRecieveStr := RecieveStr;
  fClient.ConnectTo(aAddress, aPort);
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connecting..');
end;


procedure TKMClientControl.Disconnect;
begin
  fClient.Disconnect;
  fConnected := false;
end;


procedure TKMClientControl.SendText(const aData:string);
begin
  fClient.SendText(aData);
end;


//Recieve from anyone
procedure TKMClientControl.Connected(Sender: TObject);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connected');
  fConnected := true;
end;


procedure TKMClientControl.Disconnected(Sender: TObject);
begin
  fConnected := false;
end;


//Recieve from anyone
procedure TKMClientControl.RecieveStr(const S: string);
begin
  fOnRecieveStr(S);
end;


end.
