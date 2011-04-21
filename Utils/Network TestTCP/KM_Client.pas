unit KM_Client;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, WSocket, WinSock;


const KAM_PORT = '56789';


type TRecievePacketEvent = procedure (const aData:string) of object;

type
  TKMClient = class
  private
    fSocket:TWSocket;
    fOnRecievePacket:TRecievePacketEvent;
    procedure Connected(Sender: TObject; Error: Word);
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectTo(const aAddress:string);
    procedure SendText(const aData:string);
    property OnRecievePacket:TRecievePacketEvent write fOnRecievePacket;
  end;


implementation


constructor TKMClient.Create;
var wsaData: TWSAData;
begin
  Inherited Create;
  if WSAStartup($101, wsaData) <> 0 then
    Assert(false, 'Client: Error in Network');
end;


destructor TKMClient.Destroy;
begin
  fSocket.Free;
  Inherited;
end;


procedure TKMClient.ConnectTo(const aAddress:string);
begin
  fSocket := TWSocket.Create(nil);
  fSocket.Proto     := 'tcp';
  fSocket.Addr      := aAddress;
  fSocket.Port      := KAM_PORT;
  //fSocket.LineMode  := TRUE;
  //fSocket.LineEnd   := #13#10;
  fSocket.OnSessionConnected := Connected;
  fSocket.OnDataAvailable := DataAvailable;
  fSocket.Connect;
  fOnRecievePacket('Client: Connecting..');
end;


procedure TKMClient.SendText(const aData:string);
begin
  fSocket.SendStr(aData);
end;


//Recieve from anyone
procedure TKMClient.Connected(Sender: TObject; Error: Word);
begin
  if Error <> 0 then
    fOnRecievePacket('Client: Can''t connect. Error #' + IntToStr(Error))
  else
    fOnRecievePacket('Client: Connected');
end;


//Recieve from anyone
procedure TKMClient.DataAvailable(Sender: TObject; Error: Word);
var Msg:string;
begin
  Msg := TWSocket(Sender).ReceiveStr;
  fOnRecievePacket('Client: '+Msg);
end;


end.
