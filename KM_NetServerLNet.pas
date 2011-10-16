unit KM_NetServerLNet;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, lNet;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ServerControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }

//Tagging starts with some number away from -2 -1 0 used as sender/recipient constants
//and off from usual players indexes 1..8, so we could not confuse them by mistake
const FIRST_TAG = 15;

type
  THandleEvent = procedure (aHandle:integer) of object;
  TNotifyDataEvent = procedure(aHandle:integer; aData:pointer; aLength:cardinal)of object;

  TKMNetServerLNet = class
  private
    fSocketServer:TLTCP;
    fLastTag:integer;
    fOnError:TGetStrProc;
    fOnClientConnect:THandleEvent;
    fOnClientDisconnect:THandleEvent;
    fOnDataAvailable:TNotifyDataEvent;
    procedure ClientConnect(aSocket: TLSocket);
    procedure ClientDisconnect(aSocket: TLSocket);
    procedure ReceiveData(aSocket: TLSocket);
    procedure Error(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    procedure SendData(aHandle:integer; aData:pointer; aLength:cardinal);
    procedure Kick(aHandle:integer);
    procedure UpdateStateIdle;
    property GetLatestHandle:integer read fLastTag;
    property OnError:TGetStrProc write fOnError;
    property OnClientConnect:THandleEvent write fOnClientConnect;
    property OnClientDisconnect:THandleEvent write fOnClientDisconnect;
    property OnDataAvailable:TNotifyDataEvent write fOnDataAvailable;
  end;


implementation


//Tagging starts with some number away from -2 -1 0 used as sender/recipient constants
//and off from usual players indexes 1..8, so we could not confuse them by mistake
constructor TKMNetServerLNet.Create;
begin
  Inherited Create;
  fLastTag := FIRST_TAG-1; //First client will be fLastTag+1
end;


destructor TKMNetServerLNet.Destroy;
begin
  if fSocketServer<> nil then fSocketServer.Free;
  Inherited;
end;


procedure TKMNetServerLNet.StartListening(aPort:string);
begin
  FreeAndNil(fSocketServer);
  fSocketServer := TLTCP.Create(nil);
  fSocketServer.OnError := Error;
  fSocketServer.OnAccept := ClientConnect;
  fSocketServer.OnDisconnect := ClientDisconnect;
  fSocketServer.OnReceive := ReceiveData;
  fSocketServer.Timeout := 0;
  fSocketServer.ReuseAddress := false; //Abort if the port is in use
  if not fSocketServer.Listen(StrToInt(aPort)) then
    raise Exception.Create('Server failed to start');
end;


procedure TKMNetServerLNet.StopListening;
begin
  if fSocketServer <> nil then fSocketServer.Disconnect(true);
  FreeAndNil(fSocketServer);
  fLastTag := FIRST_TAG-1;
end;


//Someone has connected to us
procedure TKMNetServerLNet.ClientConnect(aSocket: TLSocket);
begin
  //Identify index of the Client, so we could address it
  if fLastTag = MaxInt then fLastTag := FIRST_TAG-1; //I'll be surprised if this is ever necessary
  inc(fLastTag);
  aSocket.UserData := Pointer(fLastTag);
  fOnClientConnect(fLastTag);
end;


procedure TKMNetServerLNet.ClientDisconnect(aSocket: TLSocket);
begin
  fOnClientDisconnect(integer(aSocket.UserData));
end;


//We recieved data from someone
procedure TKMNetServerLNet.ReceiveData(aSocket: TLSocket);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
begin
  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := aSocket.Get(P^, BufferSize);

  if L > 0 then //if L=0 then exit;
    fOnDataAvailable(integer(aSocket.UserData), P, L);

  FreeMem(P);
end;


procedure TKMNetServerLNet.Error(const msg: string; aSocket: TLSocket);
begin
  fOnError('LNet Server Error: '+msg);
end;


//Make sure we send data to specified client
procedure TKMNetServerLNet.SendData(aHandle:integer; aData:pointer; aLength:cardinal);
var i:integer;
begin
  for i:=0 to fSocketServer.Count-1 do
    if integer(fSocketServer.Socks[i].UserData) = aHandle then
    begin
      if fSocketServer.Socks[i].Connected then //Sometimes this occurs just before ClientDisconnect
        if fSocketServer.Socks[i].Send(aData^, aLength) < aLength then
          fOnError('LNet Server: Failed to send packet');
    end;
end;


procedure TKMNetServerLNet.Kick(aHandle:integer);
var i:integer;
begin
  for i:=0 to fSocketServer.Count-1 do
    if integer(fSocketServer.Socks[i].UserData) = aHandle then
    begin
      if fSocketServer.Socks[i].Connected then //Sometimes this occurs just before ClientDisconnect
      begin
        fSocketServer.Socks[i].Disconnect(true); //This will trigger fOnClientDisconnect so there's no need to do it manually
      end;
    end;
end;


procedure TKMNetServerLNet.UpdateStateIdle;
begin
  if fSocketServer <> nil then
    fSocketServer.CallAction; //Process network events
end;


end.
