unit KM_NetServer;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_CommonTypes, KM_NetServerOverbyte;


{ Contains basic items we need for smooth Net experience:

    - start the server
    - stop the server

    - optionaly report non-important status messages

    - generate replies/messages:
      1. player# has disconnected
      2. player# binding (ID)
      3. players ping
      4. players IPs
      5. ...

    - handle orders from Host
      0. declaration of host (associate Hoster rights with this player)
      1. kick player#
      2. request for players ping
      3. request for players IPs
      4. ...

      //Following commands will be added to TKMessageKind
      mk_PlayerLost
      mk_IndexOnServer
      mk_Ping
      mk_PlayersIP

      mk_IAmHost
      mk_KickPlayer
      mk_WasKicked
      mk_AskPing
      mk_AskIPs
}
type
  TKMNetServer = class
  private
    fClientList:TList; //Remember our clients in list
    fHost:integer;
    fServer:TKMNetServerOverbyte;

    fBufferSize:cardinal;
    fBuffer:array of byte;

    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure ClientConnect(aHandle:integer);
    procedure ClientDisconnect(aHandle:integer);
    procedure SendMessage(aRecipient:integer; aKind:TKMessageKind; aMsg:integer);
    procedure DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
  end;


implementation


constructor TKMNetServer.Create;
begin
  Inherited;
  fClientList := TList.Create;
  fServer := TKMNetServerOverbyte.Create;
end;


destructor TKMNetServer.Destroy;
begin
  fServer.Free;
  fClientList.Free;
  Inherited;
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Error(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Error '+S);
end;


procedure TKMNetServer.StartListening(aPort:string);
begin
  fHost := 0;
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Listening..');
end;


procedure TKMNetServer.StopListening;
begin
  fServer.StopListening;
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMNetServer.ClientConnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Got connection '+inttostr(aHandle));
  fClientList.Add(pointer(aHandle));

  //Let the first client be a Host
  if fHost = -1 then
    fHost := aHandle;

  //@Lewin: We can tell the Client he is going to be a Host (has control over server and game setup)
  //Someone has to be in charge of that sort of things. And later on we can support reassign of Host
  //role, so any Client could be in charge (e.g. if Host is defeated or quit)

  SendMessage(aHandle, mk_IndexOnServer, aHandle);
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Client has disconnected '+inttostr(aHandle));
  fClientList.Remove(pointer(aHandle));

  if fHost = aHandle then
    fHost := -1;

  //todo: Send message to remaining clients that client has disconnected
  SendMessage(NET_RECIPIENT_ALL, mk_ClientLost, aHandle);
end;


//Assemble the packet as [Sender.Recepient.Length.Data]
procedure TKMNetServer.SendMessage(aRecipient:integer; aKind:TKMessageKind; aMsg:integer);
var i:integer; M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  
  M.Write(integer(NET_SENDER_SERVER)); //Make sure constant gets treated as 4byte integer
  M.Write(aRecipient);
  M.Write(Integer(5)); //1byte MessageKind + 4byte aHandle
  M.Write(Byte(aKind));
  M.Write(aMsg);
  if aRecipient = NET_RECIPIENT_ALL then
    for i:=0 to fClientList.Count-1 do
      fServer.SendData(cardinal(fClientList.Items[i]), M.Memory, M.Size)
  else
    fServer.SendData(aRecipient, M.Memory, M.Size);
  M.Free;
end;


//Someone has send us something
//For now just repeat the message to everyone excluding Sender
//Send only complete messages to allow to add server messages inbetween
procedure TKMNetServer.DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
var PacketRecipient:integer; PacketLength:Cardinal; i:integer;
begin
  //Append new data to buffer
  SetLength(fBuffer, fBufferSize + aLength);
  Move(aData^, fBuffer[fBufferSize], aLength);
  fBufferSize := fBufferSize + aLength;

  //Try to read data packet from buffer
  while fBufferSize >= 12 do
  begin
    //We skip PacketSender because Server does not care
    PacketRecipient := PInteger(Cardinal(@fBuffer)+4)^;
    PacketLength := PCardinal(Cardinal(@fBuffer)+8)^;
    if PacketLength <= fBufferSize-12 then
    begin

      case PacketRecipient of
        NET_RECIPIENT_ALL:
            for i:=0 to fClientList.Count-1 do
              if aHandle<>integer(fClientList.Items[i]) then //Do not send to sender
                fServer.SendData(cardinal(fClientList.Items[i]), @fBuffer[0], PacketLength+12);
        NET_RECIPIENT_HOST:
                fServer.SendData(fHost, @fBuffer[0], PacketLength+12);
        else    fServer.SendData(PacketRecipient, @fBuffer[0], PacketLength+12);
      end;

      if 12+PacketLength < fBufferSize then //Check range
        Move(fBuffer[12+PacketLength], fBuffer[0], fBufferSize-PacketLength-12);
      fBufferSize := fBufferSize - PacketLength - 12;
    end else
      Exit;
  end;
end;


end.
