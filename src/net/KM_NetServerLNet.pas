unit KM_NetServerLNet;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, lNet;


{ This unit knows nothing about KaM, it's just a puppet in hands of KM_ServerControl,
doing all the low level work on TCP. So we can replace this unit with other TCP client
without KaM even noticing. }

//Tagging starts with some number away from -2 -1 0 used as sender/recipient constants
//and off from usual players indexes 1..8, so we could not confuse them by mistake
const
  FIRST_TAG = 15;
  MAX_SEND_BUFFER = 1048576; //1 MB


type
  TClientInfo = class
    Tag: Integer;
    Buffer: array of byte;
    BufferLen: Cardinal;
    constructor Create(aTag: Integer);
    destructor Destroy; override;
    procedure AttemptSend(aSocket: TLSocket);
    procedure PutInBuffer(aData:pointer; aLength:cardinal);
    function BufferFull: Boolean;
  end;

  THandleEvent = procedure (aHandle:SmallInt) of object;
  TNotifyDataEvent = procedure(aHandle:SmallInt; aData:pointer; aLength:cardinal)of object;

  TKMNetServerLNet = class
  private
    fSocketServer:TLTCP;
    fLastTag:integer;
    fListening: Boolean;
    fOnError:TGetStrProc;
    fOnClientConnect:THandleEvent;
    fOnClientDisconnect:THandleEvent;
    fOnDataAvailable:TNotifyDataEvent;
    procedure ClientConnect(aSocket: TLSocket);
    procedure ClientDisconnect(aSocket: TLSocket);
    procedure ReceiveData(aSocket: TLSocket);
    procedure CanSend(aSocket: TLSocket);
    procedure Error(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort: Word);
    procedure StopListening;
    procedure SendData(aHandle:SmallInt; aData:pointer; aLength:cardinal);
    procedure Kick(aHandle:SmallInt);
    procedure UpdateStateIdle;
    function GetMaxHandle:SmallInt;
    function GetIP(aHandle:integer): string;
    property OnError:TGetStrProc write fOnError;
    property OnClientConnect:THandleEvent write fOnClientConnect;
    property OnClientDisconnect:THandleEvent write fOnClientDisconnect;
    property OnDataAvailable:TNotifyDataEvent write fOnDataAvailable;
  end;


implementation


constructor TClientInfo.Create(aTag: Integer);
begin
  inherited Create;
  Tag := aTag;
end;


destructor TClientInfo.Destroy;
begin
  inherited;
end;


procedure TClientInfo.AttemptSend(aSocket: TLSocket);
var LenSent: Integer;
begin
  if BufferLen <= 0 then Exit;

  LenSent := aSocket.Send(Buffer[0], BufferLen);

  if LenSent > 0 then
  begin
    BufferLen := BufferLen - LenSent;
    if BufferLen > 0 then
      Move(Buffer[LenSent], Buffer[0], BufferLen);
  end;
end;


procedure TClientInfo.PutInBuffer(aData:pointer; aLength:cardinal);
begin
  SetLength(Buffer, BufferLen + aLength);
  Move(aData^, Buffer[BufferLen], aLength);
  BufferLen := BufferLen + aLength;
end;


function TClientInfo.BufferFull: Boolean;
begin
  Result := BufferLen >= MAX_SEND_BUFFER;
end;

//Tagging starts with some number away from -2 -1 0 used as sender/recipient constants
//and off from usual players indexes 1..8, so we could not confuse them by mistake
constructor TKMNetServerLNet.Create;
begin
  Inherited Create;
  fLastTag := FIRST_TAG-1; //First client will be fLastTag+1
end;


destructor TKMNetServerLNet.Destroy;
begin
  StopListening;
  if fSocketServer<> nil then fSocketServer.Free;
  Inherited;
end;


procedure TKMNetServerLNet.StartListening(aPort: Word);
begin
  FreeAndNil(fSocketServer);
  fSocketServer := TLTCP.Create(nil);
  fSocketServer.OnError := Error;
  fSocketServer.OnAccept := ClientConnect;
  fSocketServer.OnDisconnect := ClientDisconnect;
  fSocketServer.OnReceive := ReceiveData;
  fSocketServer.OnCanSend := CanSend;
  //This is the time it will wait in CallAction for the OS to respond, it's better than calling Sleep(1)
  //This value could be higher, it won't cause delays since it's just a timeout on waiting for network events
  fSocketServer.Timeout := 100;
  fSocketServer.ReuseAddress := True; //Allows us to overwrite an old connection in the wait state rather than saying the port is still in use
  if not fSocketServer.Listen(aPort) then
    raise Exception.Create('Server failed to start');
  fListening := True;
end;


procedure TKMNetServerLNet.StopListening;
begin
  if fSocketServer <> nil then
  begin
    //We have to disconnect all the clients too
    fSocketServer.IterReset;
    while fSocketServer.IterNext do
    begin
      if fSocketServer.Iterator.UserData <> nil then
      begin
        TClientInfo(fSocketServer.Iterator.UserData).Free;
        fSocketServer.Iterator.UserData := nil;
      end;
      fSocketServer.Iterator.Disconnect(True);
    end;
    fSocketServer.Disconnect(True);
  end;
  FreeAndNil(fSocketServer);
  fLastTag := FIRST_TAG-1;
  fListening := False;
end;


//Someone has connected to us
procedure TKMNetServerLNet.ClientConnect(aSocket: TLSocket);
begin
  //Identify index of the Client, so we can address it
  if fLastTag = GetMaxHandle then fLastTag := FIRST_TAG-1; //I'll be surprised if this is ever necessary
  inc(fLastTag);
  aSocket.UserData := TClientInfo.Create(fLastTag);
  fOnClientConnect(fLastTag);
  aSocket.SetState(ssNoDelay, True); //Send packets ASAP (disables Nagle's algorithm)
end;


procedure TKMNetServerLNet.ClientDisconnect(aSocket: TLSocket);
begin
  if aSocket.UserData = nil then Exit;
  fOnClientDisconnect(TClientInfo(aSocket.UserData).Tag);
  TClientInfo(aSocket.UserData).Free;
  aSocket.UserData := nil;
end;


//We recieved data from someone
procedure TKMNetServerLNet.ReceiveData(aSocket: TLSocket);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
begin
  if aSocket.UserData = nil then
  begin
    aSocket.Disconnect(True); //Sometimes disconnecting them fails the first time(?) LNet bug?
    //fOnError('Received data from an unknown client');
    Exit;
  end;

  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := aSocket.Get(P^, BufferSize);

  if L > 0 then //if L=0 then exit;
    fOnDataAvailable(TClientInfo(aSocket.UserData).Tag, P, L);

  FreeMem(P);
end;


procedure TKMNetServerLNet.Error(const msg: string; aSocket: TLSocket);
begin
  fOnError('LNet: '+msg);
  if (aSocket <> nil) and (not aSocket.Connected) then
    ClientDisconnect(aSocket); //Sometimes this is the only event we get when a client disconnects (LNet bug?)
end;


procedure TKMNetServerLNet.CanSend(aSocket: TLSocket);
begin
  if aSocket.UserData = nil then Exit;
  TClientInfo(aSocket.UserData).AttemptSend(aSocket);
end;


//Make sure we send data to specified client
procedure TKMNetServerLNet.SendData(aHandle:SmallInt; aData:pointer; aLength:cardinal);
begin
  fSocketServer.IterReset;
  while fSocketServer.IterNext do
    if fSocketServer.Iterator.Connected then //Sometimes this occurs just before ClientDisconnect
      if (fSocketServer.Iterator.UserData <> nil) and (TClientInfo(fSocketServer.Iterator.UserData).Tag = aHandle) then
      begin
        if TClientInfo(fSocketServer.Iterator.UserData).BufferFull then
        begin
          fOnError('Send buffer full for client '+IntToStr(TClientInfo(fSocketServer.Iterator.UserData).Tag));
          Exit;
        end;
        TClientInfo(fSocketServer.Iterator.UserData).PutInBuffer(aData, aLength);
        TClientInfo(fSocketServer.Iterator.UserData).AttemptSend(fSocketServer.Iterator);
        Exit;
      end;
end;


procedure TKMNetServerLNet.Kick(aHandle:SmallInt);
var Iter: TLSocket;
begin
  fSocketServer.IterReset;
  while fSocketServer.IterNext do
    if (fSocketServer.Iterator.UserData <> nil) and (TClientInfo(fSocketServer.Iterator.UserData).Tag = aHandle) then
    begin
      //Found the client that must be kicked
      Iter := fSocketServer.Iterator; //Calling ClientDisconnect can reset Iterator due to sending a message, so we must remember it (don't use it after this line)
      //Seems to be a bug in LNet where ClientDisconnect is not called sometimes? So call it ourself just in case
      ClientDisconnect(Iter);
      if Iter.ConnectionStatus in [scConnected, scConnecting, scDisconnecting] then
        //We always use forceful disconnects otherwise sockets can be left in FIN_WAIT2 state forever. See:
        //http://stackoverflow.com/questions/9819745/how-tcp-stack-distinguish-close-and-shutdown
        Iter.Disconnect(True)
      else
        fOnError('Warning: Attempted to kick a client that is not connected');
      Exit; //Only one client should have this handle
    end;
  //If we reached here we didn't find a match
  fOnError('Warning: Attempted to kick a client that has already disconnected');
  fOnClientDisconnect(aHandle); //Client has already disconnected somehow, so send the event to ensure they are removed
end;


function TKMNetServerLNet.GetMaxHandle:SmallInt;
begin
  Result := 32767;
end;


procedure TKMNetServerLNet.UpdateStateIdle;
begin
  if fSocketServer <> nil then
    fSocketServer.CallAction; //Process network events

  if fListening and not fSocketServer.Connected then
    raise Exception.Create('Server is no longer listening! Server will restart.');
end;


function TKMNetServerLNet.GetIP(aHandle:integer): string;
begin
  Result := '';
  fSocketServer.IterReset;
  while fSocketServer.IterNext do
    if (fSocketServer.Iterator.UserData <> nil) and (TClientInfo(fSocketServer.Iterator.UserData).Tag = aHandle) then
    begin
      Result := fSocketServer.Iterator.PeerAddress;
      Exit; //Only one client should have this handle
    end;
end;


end.
