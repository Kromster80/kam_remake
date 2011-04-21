unit KM_Server;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, WSocket, WSocketS, WinSock;


const KAM_PORT2 = '56789'; //We can decide on something official later

type TRecievePacketEvent = procedure (const aData:string) of object;

type
  TKMServer = class
  private
    fClientID:cardinal;
    fSocketRecieve:TWSocketServer;
    fOnRecievePacket:TRecievePacketEvent;
    procedure Connected(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening;
    procedure StopListening;
    property OnRecievePacket:TRecievePacketEvent write fOnRecievePacket;
  end;


implementation


constructor TKMServer.Create;
var wsaData: TWSAData;
begin
  Inherited Create;
  fClientID := 0;
  if WSAStartup($101, wsaData) <> 0 then
    Assert(false, 'Server: Error in Network');
end;


destructor TKMServer.Destroy;
begin
  fSocketRecieve.Free;
  Inherited;
end;


procedure TKMServer.StartListening;
begin
  fSocketRecieve := TWSocketServer.Create(nil);
  fSocketRecieve.Proto  := 'tcp';
  fSocketRecieve.Addr   := '0.0.0.0'; //Listen to whole range
  fSocketRecieve.Port   := KAM_PORT2;
  //fSocketRecieve.LineMode  := TRUE;
  //fSocketRecieve.LineEnd   := #13#10;
  fSocketRecieve.OnClientConnect := Connected;
  fSocketRecieve.OnDataAvailable := DataAvailable;
  fSocketRecieve.Listen;
  fOnRecievePacket('Server: Listening..');
end;


procedure TKMServer.StopListening;
begin
  if fSocketRecieve <> nil then fSocketRecieve.Close;
  FreeAndNil(fSocketRecieve);
end;


//Recieve from anyone
procedure TKMServer.Connected(Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  if Error <> 0 then
    fOnRecievePacket('Server: Can''t connect. Error #' + IntToStr(Error))
  else
    fOnRecievePacket('Server: Client connected');

  Client.OnDataAvailable := DataAvailable;
  inc(fClientID);
  Client.Tag := fClientID;
end;


//Recieve from anyone
procedure TKMServer.DataAvailable(Sender: TObject; Error: Word);
var i:Integer; Msg:string;
begin
  Msg := TWSocket(Sender).ReceiveStr;

  //fOnRecievePacket('Server: repeating "'+Msg+'"');

  for i:=0 to fSocketRecieve.ClientCount-1 do
    fSocketRecieve.Client[i].SendStr(inttostr(TWSocket(Sender).Tag)+' said: "'+Msg+'"');
end;


end.
