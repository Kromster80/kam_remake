unit KM_NetServerOverbyte;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, WSocket, WSocketS, WinSock;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ServerControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }

//Tagging starts with some number away from -2 -1 0 used as sender/recipient constants
//and off from usual players indexes 1..8, so we could not confuse them by mistake
const FIRST_TAG = 15;

type
  THandleEvent = procedure (aHandle:integer) of object;
  TNotifyDataEvent = procedure(aHandle:integer; aData:pointer; aLength:cardinal)of object;

  TKMNetServerOverbyte = class
  private
    fSocketServer:TWSocketServer;
    fLastTag:integer;
    fOnError:TGetStrProc;
    fOnClientConnect:THandleEvent;
    fOnClientDisconnect:THandleEvent;
    fOnDataAvailable:TNotifyDataEvent;
    procedure ClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    procedure SendData(aHandle:integer; aData:pointer; aLength:cardinal);
    procedure Kick(aHandle:integer);
    property GetLatestHandle:integer read fLastTag;
    property OnError:TGetStrProc write fOnError;
    property OnClientConnect:THandleEvent write fOnClientConnect;
    property OnClientDisconnect:THandleEvent write fOnClientDisconnect;
    property OnDataAvailable:TNotifyDataEvent write fOnDataAvailable;
  end;


implementation


constructor TKMNetServerOverbyte.Create;
var wsaData: TWSAData;
begin
  Inherited Create;
  fLastTag := FIRST_TAG-1; //First client will be fLastTag+1
  if WSAStartup($101, wsaData) <> 0 then
    fOnError('Error in Network');
end;


destructor TKMNetServerOverbyte.Destroy;
begin
  if fSocketServer<> nil then fSocketServer.Free;
  Inherited;
end;


procedure TKMNetServerOverbyte.StartListening(aPort:string);
begin
  FreeAndNil(fSocketServer);
  fSocketServer := TWSocketServer.Create(nil);
  fSocketServer.Proto  := 'tcp';
  fSocketServer.Addr   := '0.0.0.0'; //Listen to whole range
  fSocketServer.Port   := aPort;
  fSocketServer.Banner := '';
  fSocketServer.OnClientConnect := ClientConnect;
  fSocketServer.OnClientDisconnect := ClientDisconnect;
  fSocketServer.OnDataAvailable := DataAvailable;
  fSocketServer.Listen;
end;


procedure TKMNetServerOverbyte.StopListening;
begin
  if fSocketServer <> nil then fSocketServer.Close;
  FreeAndNil(fSocketServer);
end;


//Someone has connected to us
procedure TKMNetServerOverbyte.ClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  if Error <> 0 then
  begin
    fOnError('ClientConnect. Error: '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');
    exit;
  end;

  //Identify index of the Client, so we could address it
  inc(fLastTag);
  Client.Tag := fLastTag;

  Client.OnDataAvailable := DataAvailable;
  fOnClientConnect(Client.Tag);
end;


procedure TKMNetServerOverbyte.ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  if Error <> 0 then
  begin
    fOnError('ClientDisconnect. Error: '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');
    //Do not exit because the client has still disconnected
  end;

  fOnClientDisconnect(Client.Tag);
end;


//We recieved data from someone
procedure TKMNetServerOverbyte.DataAvailable(Sender: TObject; Error: Word);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
begin
  if Error <> 0 then
  begin
    fOnError('DataAvailable. Error: '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');
    exit;
  end;

  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := TWSocket(Sender).Receive(P, BufferSize);

  if L > 0 then //if L=0 then exit;
    fOnDataAvailable(TWSocket(Sender).Tag, P, L);

  FreeMem(P);
end;


//Make sure we send data to specified client
procedure TKMNetServerOverbyte.SendData(aHandle:integer; aData:pointer; aLength:cardinal);
var i:integer;
begin
  for i:=0 to fSocketServer.ClientCount-1 do
    if fSocketServer.Client[i].Tag = aHandle then
    begin
      if fSocketServer.Client[i].State <> wsClosed then //Sometimes this occurs just before ClientDisconnect
        fSocketServer.Client[i].Send(aData, aLength);
    end;
end;


procedure TKMNetServerOverbyte.Kick(aHandle:integer);
var i:integer;
begin
  for i:=0 to fSocketServer.ClientCount-1 do
    if fSocketServer.Client[i].Tag = aHandle then
    begin
      if fSocketServer.Client[i].State <> wsClosed then //Sometimes this occurs just before ClientDisconnect
        fSocketServer.Client[i].Close;
    end;
end;


end.
