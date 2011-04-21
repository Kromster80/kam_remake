unit KM_ServerOverbyte;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, WSocket, WSocketS, WinSock;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ServerControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }
type
  THandleEvent = procedure (aHandle:cardinal) of object;
  TDataAvailableEvent = procedure (aHandle:cardinal; const aData:string) of object;

  TKMServer = class
  private
    fSocketServer:TWSocketServer;
    fOnError:TGetStrProc;
    fOnClientConnect:THandleEvent;
    fOnClientDisconnect:THandleEvent;
    fOnDataAvailable:TDataAvailableEvent;
    procedure ClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    procedure SendData(aHandle:cardinal; const aData:string);
    property OnError:TGetStrProc write fOnError;
    property OnClientConnect:THandleEvent write fOnClientConnect;
    property OnClientDisconnect:THandleEvent write fOnClientDisconnect;
    property OnDataAvailable:TDataAvailableEvent write fOnDataAvailable;
  end;


implementation


constructor TKMServer.Create;
var wsaData: TWSAData;
begin
  Inherited Create;
  if WSAStartup($101, wsaData) <> 0 then
    fOnError('Error in Network');
end;


destructor TKMServer.Destroy;
begin
  if fSocketServer<> nil then fSocketServer.Free;
  Inherited;
end;


procedure TKMServer.StartListening(aPort:string);
begin
  fSocketServer := TWSocketServer.Create(nil);
  fSocketServer.Proto  := 'tcp';
  fSocketServer.Addr   := '0.0.0.0'; //Listen to whole range
  fSocketServer.Port   := aPort;
  fSocketServer.OnClientConnect := ClientConnect;
  fSocketServer.OnClientDisconnect := ClientDisconnect;
  fSocketServer.OnDataAvailable := DataAvailable;
  fSocketServer.Listen;
end;


procedure TKMServer.StopListening;
begin
  if fSocketServer <> nil then fSocketServer.Close;
  FreeAndNil(fSocketServer);
end;


//Someone has connected to us
procedure TKMServer.ClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
var i:integer;
begin
  if Error <> 0 then
  begin
    fOnError('ClientConnect. Error #' + IntToStr(Error));
    exit;
  end;

  //Identify index of the Client, so we could address it
  Client.Tag := -1;
  for i:=0 to fSocketServer.ClientCount-1 do
    if Client = fSocketServer.Client[i] then
      Client.Tag := i;
  Assert(Client.Tag <> -1);

  Client.OnDataAvailable := DataAvailable;
  fOnClientConnect(Client.Tag);
end;


procedure TKMServer.ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
var i:integer;
begin
  if Error <> 0 then
  begin
    fOnError('ClientConnect. Error #' + IntToStr(Error));
    exit;
  end;

  //Identify index of the Client, so we could address it
  Client.Tag := -1;
  for i:=0 to fSocketServer.ClientCount-1 do
    if Client = fSocketServer.Client[i] then
      Client.Tag := i;
  Assert(Client.Tag <> -1);

  fOnClientDisconnect(Client.Tag);
end;


//We recieved data from someone
procedure TKMServer.DataAvailable(Sender: TObject; Error: Word);
var Msg:string;
begin
  if Error <> 0 then
  begin
    fOnError('ClientConnect. Error #' + IntToStr(Error));
    exit;
  end;

  Msg := TWSocket(Sender).ReceiveStr;

  fOnDataAvailable(TWSocket(Sender).Tag, Msg);
end;


procedure TKMServer.SendData(aHandle:cardinal; const aData:string);
begin
  fSocketServer.Client[aHandle].SendStr(aData);
end;


end.
