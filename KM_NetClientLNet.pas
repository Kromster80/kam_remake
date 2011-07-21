unit KM_NetClientLNet;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, LNet, dialogs;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ClientControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }
type
  TNotifyDataEvent = procedure(aData:pointer; aLength:cardinal)of object;

  TKMNetClientLNet = class
  private
    fSocket:TLTCP;
    fOnError:TGetStrProc;
    fOnConnectSucceed:TNotifyEvent;
    fOnConnectFailed:TGetStrProc;
    fOnSessionDisconnected:TNotifyEvent;
    fOnRecieveData:TNotifyDataEvent;
    procedure Connected(aSocket: TLSocket);
    procedure Disconnected(aSocket: TLSocket);
    procedure Receive(aSocket: TLSocket);
    procedure Error(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor Destroy; override;
    function MyIPString:string;  
    procedure ConnectTo(const aAddress:string; const aPort:string);
    procedure Disconnect;
    procedure SendData(aData:pointer; aLength:cardinal);
    procedure UpdateStateIdle;
    property OnError:TGetStrProc write fOnError;
    property OnConnectSucceed:TNotifyEvent write fOnConnectSucceed;
    property OnConnectFailed:TGetStrProc write fOnConnectFailed;
    property OnSessionDisconnected:TNotifyEvent write fOnSessionDisconnected;
    property OnRecieveData:TNotifyDataEvent write fOnRecieveData;
  end;


implementation


constructor TKMNetClientLNet.Create;
begin
  Inherited Create;
end;


destructor TKMNetClientLNet.Destroy;
begin
  if fSocket<>nil then fSocket.Free;
  Inherited;
end;


function TKMNetClientLNet.MyIPString:string;
begin
  Result := 'Not implemented yet';
end;


procedure TKMNetClientLNet.ConnectTo(const aAddress:string; const aPort:string);
begin
  FreeAndNil(fSocket);
  fSocket := TLTCP.Create(nil);
  fSocket.OnDisconnect := Disconnected;
  fSocket.OnConnect := Connected;
  fSocket.OnReceive := Receive;
  fSocket.OnError := Error;
  fSocket.Timeout := 0;
  try
    fSocket.Connect(aAddress,StrToInt(aPort));
    fSocket.CallAction;
  except
    on E : Exception do
    begin
      //Trap the exception and tell the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      fOnConnectFailed(E.Message);
    end;
  end;
end;


procedure TKMNetClientLNet.Disconnect;
begin
  if fSocket <> nil then fSocket.Disconnect(true);
end;


procedure TKMNetClientLNet.SendData(aData:pointer; aLength:cardinal);
begin
  if fSocket.Connected then
    if fSocket.Send(aData^, aLength) < aLength then
      fOnError('LNet Server: Failed to send packet');
end;


procedure TKMNetClientLNet.Connected(aSocket: TLSocket);
begin
  fOnConnectSucceed(Self);
end;


procedure TKMNetClientLNet.Disconnected(aSocket: TLSocket);
begin
  fOnSessionDisconnected(Self);
end;


procedure TKMNetClientLNet.Receive(aSocket: TLSocket);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
begin
  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := aSocket.Get(P^, BufferSize);

  if L > 0 then //if L=0 then exit;
    fOnRecieveData(P, L);

  FreeMem(P);
end;


procedure TKMNetClientLNet.Error(const msg: string; aSocket: TLSocket);
begin
  fOnError('LNet Client Error: '+msg);
end;


procedure TKMNetClientLNet.UpdateStateIdle;
begin
  if fSocket <> nil then fSocket.CallAction; //Process network events
end;

end.
