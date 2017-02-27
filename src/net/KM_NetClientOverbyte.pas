unit KM_NetClientOverbyte;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, OverbyteIcsWSocket, WinSock;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ClientControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }
type
  TNotifyDataEvent = procedure(aData:pointer; aLength:cardinal)of object;

  TKMNetClientOverbyte = class
  private
    fSocket:TWSocket;
    fOnError:TGetStrProc;
    fOnConnectSucceed:TNotifyEvent;
    fOnConnectFailed:TGetStrProc;
    fOnSessionDisconnected:TNotifyEvent;
    fOnRecieveData:TNotifyDataEvent;
    procedure Connected(Sender: TObject; Error: Word);
    procedure Disconnected(Sender: TObject; Error: Word);
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    function MyIPString:string;
    function SendBufferEmpty: Boolean;
    procedure ConnectTo(const aAddress: string; const aPort: Word);
    procedure Disconnect;
    procedure SendData(aData:pointer; aLength:cardinal);
    property OnError:TGetStrProc write fOnError;
    property OnConnectSucceed:TNotifyEvent write fOnConnectSucceed;
    property OnConnectFailed:TGetStrProc write fOnConnectFailed;
    property OnSessionDisconnected:TNotifyEvent write fOnSessionDisconnected;
    property OnRecieveData:TNotifyDataEvent write fOnRecieveData;
  end;


implementation


constructor TKMNetClientOverbyte.Create;
var wsaData: TWSAData;
begin
  Inherited Create;
  Assert(WSAStartup($101, wsaData) = 0, 'Error in Network');
end;


destructor TKMNetClientOverbyte.Destroy;
begin
  if fSocket <> nil then fSocket.Free;
  Inherited;
end;


function TKMNetClientOverbyte.MyIPString: string;
begin
  if LocalIPList.Count >= 1 then
    Result := LocalIPList[0] //First address should be ours
  else
    Result := '';
end;


procedure TKMNetClientOverbyte.ConnectTo(const aAddress: string; const aPort:Word);
begin
  FreeAndNil(fSocket);
  fSocket := TWSocket.Create(nil);
  fSocket.ComponentOptions := [wsoTcpNoDelay]; //Send packets ASAP (disables Nagle's algorithm)
  fSocket.Proto     := 'tcp';
  fSocket.Addr      := aAddress;
  fSocket.Port      := IntToStr(aPort);
  fSocket.OnSessionClosed := Disconnected;
  fSocket.OnSessionConnected := Connected;
  fSocket.OnDataAvailable := DataAvailable;
  try
    fSocket.Connect;
  except
    on E : Exception do
    begin
      //Trap the exception and tell the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      fOnConnectFailed(E.Message);
    end;
  end;
end;


procedure TKMNetClientOverbyte.Disconnect;
begin
  if fSocket <> nil then
    fSocket.Close;
end;


procedure TKMNetClientOverbyte.SendData(aData:pointer; aLength:cardinal);
begin
  if fSocket.State = wsConnected then //Sometimes this occurs just before disconnect/reconnect
    fSocket.Send(aData, aLength);
end;


function TKMNetClientOverbyte.SendBufferEmpty: Boolean;
begin
  if (fSocket <> nil) and (fSocket.State = wsConnected) then
    Result := fSocket.AllSent
  else
    Result := True;
end;


procedure TKMNetClientOverbyte.Connected(Sender: TObject; Error: Word);
begin
  if Error <> 0 then
    fOnConnectFailed('Error: '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')')
  else
  begin
    fOnConnectSucceed(Self);
    fSocket.SetTcpNoDelayOption; //Send packets ASAP (disables Nagle's algorithm)
    fSocket.SocketSndBufSize := 65535; //WinSock buffer should be bigger than internal buffer
    fSocket.BufSize := 32768;
  end;
end;


procedure TKMNetClientOverbyte.Disconnected(Sender: TObject; Error: Word);
begin
  //Do not exit on error, because when a disconnect error occurs, the client has still disconnected
  if Error <> 0 then
    fOnError('Client: Disconnection error: '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');

  fOnSessionDisconnected(Self);
end;


procedure TKMNetClientOverbyte.DataAvailable(Sender: TObject; Error: Word);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
begin
  if Error <> 0 then
  begin
    fOnError('DataAvailable. Error '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');
    exit;
  end;

  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := TWSocket(Sender).Receive(P, BufferSize);

  if L > 0 then //if L=0 then exit;
    fOnRecieveData(P, L);

  FreeMem(P);
end;


end.
