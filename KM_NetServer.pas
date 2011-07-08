unit KM_NetServer;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Windows, KM_CommonTypes, KM_NetServerOverbyte;


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
  TKMServerClient = class
  private
    fHandle:integer;
    fPing:integer;
  public
    constructor Create(aHandle:integer);
    property Handle:integer read fHandle; //ReadOnly
    property Ping:integer read fPing write fPing;
  end;


  TKMClientsList = class
  private
    fCount:integer;
    fItems:array of TKMServerClient;
    function GetItem(Index:integer):TKMServerClient;
  public
    property Count:integer read fCount;
    procedure AddPlayer(aHandle:integer);
    procedure RemPlayer(aHandle:integer);
    procedure Clear;
    property Item[Index:integer]:TKMServerClient read GetItem; default;
    function GetByHandle(aHandle:integer):TKMServerClient;
  end;


  TKMNetServer = class
  private
    fServer:TKMNetServerOverbyte;

    fClientList:TKMClientsList;
    fHostHandle:integer;
    fPingStarted:cardinal;
    fListening:boolean;

    fBufferSize:cardinal;
    fBuffer:array of byte;

    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure ClientConnect(aHandle:integer);
    procedure ClientDisconnect(aHandle:integer);
    procedure SendMessage(aRecipient:integer; aKind:TKMessageKind; aMsg:integer; aText:string);
    procedure RecieveMessage(aSenderHandle:integer; aData:pointer; aLength:cardinal);
    procedure DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    procedure ClearClients;
    procedure MeasurePings;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
    property Listening: boolean read fListening;
  end;


implementation


{ TKMServerClient }
constructor TKMServerClient.Create(aHandle: integer);
begin
  Inherited Create;
  fHandle := aHandle;
end;


{ TKMClientsList }
function TKMClientsList.GetItem(Index: integer): TKMServerClient;
begin
  Result := fItems[Index];
end;


procedure TKMClientsList.AddPlayer(aHandle: integer);
begin
  inc(fCount);
  SetLength(fItems, fCount);
  fItems[fCount-1] := TKMServerClient.Create(aHandle);
end;


procedure TKMClientsList.RemPlayer(aHandle: integer);
var i,ID:integer;
begin
  ID := -1; //Convert Handle to Index
  for i:=0 to fCount-1 do
    if fItems[i].Handle = aHandle then
      ID := i;

  Assert(ID <> -1, 'TKMClientsList. Can not remove player');

  fItems[ID].Free;
  for i:=ID to fCount-2 do
    fItems[i] := fItems[i+1]; //Shift only pointers

  dec(fCount);
  SetLength(fItems, fCount);
end;


procedure TKMClientsList.Clear;
var i:integer;
begin
  for i:=0 to fCount-1 do
    FreeAndNil(fItems[i]);
  fCount := 0;
end;


function TKMClientsList.GetByHandle(aHandle: integer): TKMServerClient;
var i:integer;
begin
  Result := nil;
  for i:=0 to fCount-1 do
    if fItems[i].Handle = aHandle then
    begin
      Result := fItems[i];
      Exit;
    end;
end;


{ TKMNetServer }
constructor TKMNetServer.Create;
begin
  Inherited;
  fClientList := TKMClientsList.Create;
  fServer := TKMNetServerOverbyte.Create;
  fListening := false;
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
  fHostHandle := NET_ADDRESS_EMPTY;
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage('Server: Listening..');
  fListening := true;
end;


procedure TKMNetServer.StopListening;
begin
  fOnStatusMessage := nil;
  fServer.StopListening;
  fListening := false;
end;


procedure TKMNetServer.ClearClients;
begin
  fClientList.Clear;
end;


procedure TKMNetServer.MeasurePings;
begin
  fPingStarted := GetTickCount;
  SendMessage(NET_ADDRESS_ALL, mk_Ping, 0, '');
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMNetServer.ClientConnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage('Server: Client has connected '+inttostr(aHandle));

  fClientList.AddPlayer(aHandle);

  //Let the first client be a Host
  if fHostHandle = NET_ADDRESS_EMPTY then
  begin
    fHostHandle := aHandle;
    if Assigned(fOnStatusMessage) then
      fOnStatusMessage('Server: Host rights assigned to '+inttostr(fHostHandle));
  end;

  //@Lewin: We can tell the Client he is going to be a Host (has control over server and game setup)
  //Someone has to be in charge of that sort of things. And later on we can support reassign of Host
  //role, so any Client could be in charge (e.g. if Host is defeated or quit)
  //@Krom: Sounds good to me. Implement it as you described.

  SendMessage(aHandle, mk_IndexOnServer, aHandle, '');
  MeasurePings;
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage('Server: Client has disconnected '+inttostr(aHandle));

  fClientList.RemPlayer(aHandle);

  if fHostHandle = aHandle then
    fHostHandle := NET_ADDRESS_EMPTY;

  //Send message to all remaining clients that client has disconnected
  SendMessage(NET_ADDRESS_ALL, mk_ClientLost, aHandle, '');
end;


//Assemble the packet as [Sender.Recepient.Length.Data]
procedure TKMNetServer.SendMessage(aRecipient:integer; aKind:TKMessageKind; aMsg:integer; aText:string);
var i:integer; M:TKMemoryStream; PacketLength:integer;
begin
  M := TKMemoryStream.Create;

  M.Write(integer(NET_ADDRESS_SERVER)); //Make sure constant gets treated as 4byte integer
  M.Write(aRecipient);

  PacketLength := 1;
  case NetPacketType[aKind] of
    pfNumber: inc(PacketLength, SizeOf(Integer));
    pfText:   inc(PacketLength, SizeOf(Word) + Length(aText));
  end;

  M.Write(PacketLength);
  M.Write(Byte(aKind));

  case NetPacketType[aKind] of
    pfNumber: M.Write(aMsg);
    pfText:   M.Write(aText);
  end;

  if aRecipient = NET_ADDRESS_ALL then
    for i:=0 to fClientList.Count-1 do
      fServer.SendData(fClientList[i].Handle, M.Memory, M.Size)
  else
    fServer.SendData(aRecipient, M.Memory, M.Size);
  M.Free;
end;


procedure TKMNetServer.RecieveMessage(aSenderHandle:integer; aData:pointer; aLength:cardinal);
var
  i:integer;
  Kind:TKMessageKind;
  M:TKMemoryStream;
  Param:integer;
  Msg:string;
begin
  Assert(aLength >= 1, 'Unexpectedly short message');

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(Kind, SizeOf(TKMessageKind));
  case NetPacketType[Kind] of
    pfNumber: M.Read(Param);
    pfText:   M.Write(Msg);
  end;
  M.Free;

  case Kind of
    mk_AskPingInfo:
            begin
              M := TKMemoryStream.Create;
              M.Write(fClientList.Count);
              for i:=0 to fClientList.Count-1 do
              begin
                M.Write(fClientList[i].Handle);
                M.Write(fClientList[i].Ping);
              end;
              SendMessage(aSenderHandle, mk_PingInfo, 0, M.ReadAsText);
              M.Free;
            end;
    mk_Pong:
            begin
             fClientList.GetByHandle(aSenderHandle).Ping := GetTickCount - fPingStarted;


            end;
  end;

end;


//Someone has send us something
//Send only complete messages to allow to add server messages inbetween
procedure TKMNetServer.DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
var PacketSender,PacketRecipient:integer; PacketLength:Cardinal; i:integer;
begin
  //Append new data to buffer
  SetLength(fBuffer, fBufferSize + aLength);
  Move(aData^, fBuffer[fBufferSize], aLength);
  fBufferSize := fBufferSize + aLength;

  //Try to read data packet from buffer
  while fBufferSize >= 12 do
  begin
    PacketSender := PInteger(fBuffer)^;
    PacketRecipient := PInteger(Cardinal(fBuffer)+4)^;
    PacketLength := PCardinal(Cardinal(fBuffer)+8)^;
    if PacketLength <= fBufferSize-12 then
    begin

      case PacketRecipient of
        NET_ADDRESS_OTHERS: //Transmit to all except sender
            for i:=0 to fClientList.Count-1 do
                if aHandle <> fClientList[i].Handle then
                  fServer.SendData(fClientList[i].Handle, @fBuffer[0], PacketLength+12);
        NET_ADDRESS_ALL: //Transmit to all including sender (used mainly by TextMessages)
                for i:=0 to fClientList.Count-1 do
                  fServer.SendData(fClientList[i].Handle, @fBuffer[0], PacketLength+12);
        NET_ADDRESS_HOST:
                fServer.SendData(fHostHandle, @fBuffer[0], PacketLength+12);
        NET_ADDRESS_SERVER:
                RecieveMessage(PacketSender, @fBuffer[12], PacketLength);
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
