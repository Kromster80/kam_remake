unit KM_NetServer;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_CommonClasses, KM_NetworkClasses, KM_NetworkTypes, KM_Defaults, KM_Utils, VerySimpleXML
     {$IFDEF MSWINDOWS} ,Windows {$ENDIF}
     {$IFDEF WDC} ,KM_NetServerOverbyte {$ENDIF}
     {$IFDEF FPC} ,KM_NetServerLNet {$ENDIF}
     ;


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
}

type
  TKMServerClient = class
  private
    fHandle:integer;
    fRoom:integer;
    fPingStarted:cardinal;
    fPing:word;
    //Each client must have their own receive buffer, so partial messages don't get mixed
    fBufferSize:cardinal;
    fBuffer:array of byte;
  public
    constructor Create(aHandle, aRoom:integer);
    property Handle:integer read fHandle; //ReadOnly
    property Room:integer read fRoom write fRoom;
    property Ping:word read fPing write fPing;
  end;


  TKMClientsList = class
  private
    fCount:integer;
    fItems:array of TKMServerClient;
    function GetItem(Index:integer):TKMServerClient;
  public
    destructor Destroy; override;
    property Count:integer read fCount;
    procedure AddPlayer(aHandle, aRoom:integer);
    procedure RemPlayer(aHandle:integer);
    procedure Clear;
    property Item[Index:integer]:TKMServerClient read GetItem; default;
    function GetByHandle(aHandle:integer):TKMServerClient;
  end;


  TKMNetServer = class
  private
    {$IFDEF WDC} fServer:TKMNetServerOverbyte; {$ENDIF}
    {$IFDEF FPC} fServer:TKMNetServerLNet;     {$ENDIF}

    fClientList:TKMClientsList;
    fListening:boolean;
    BytesTX:Int64; //May exceed 4GB allowed by cardinal
    BytesRX:Int64;

    fMaxRooms:word;
    fHTMLStatusFile:string;
    fWelcomeMessage: UnicodeString;
    fServerName: AnsiString;
    fKickTimeout:word;
    fRoomCount:integer;
    fEmptyGameInfo: TMPGameInfo;
    fRoomInfo:array of record
                         HostHandle:integer;
                         Password: AnsiString;
                         BannedIPs: array of string;
                         GameInfo:TMPGameInfo;
                       end;

    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure Status(const S: string);
    procedure ClientConnect(aHandle:integer);
    procedure ClientDisconnect(aHandle:integer);
    procedure SendMessage(aRecipient: Integer; aKind: TKMessageKind); overload;
    procedure SendMessage(aRecipient: Integer; aKind: TKMessageKind; aParam: Integer); overload;
    procedure SendMessageA(aRecipient: Integer; aKind: TKMessageKind; aText: AnsiString);
    procedure SendMessageW(aRecipient: Integer; aKind: TKMessageKind; aText: UnicodeString);
    procedure SendMessage(aRecipient: Integer; aKind: TKMessageKind; aStream: TKMemoryStream); overload;
    procedure SendMessageToRoom(aKind: TKMessageKind; aRoom: Integer; aStream: TKMemoryStream); overload;
    procedure SendMessageAct(aRecipient: Integer; aKind: TKMessageKind; aStream: TKMemoryStream);
    procedure DoSendData(aRecipient: Integer; aData: Pointer; aLength: Cardinal);
    procedure RecieveMessage(aSenderHandle:integer; aData:pointer; aLength:cardinal);
    procedure DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
    procedure SaveToStream(aStream: TKMemoryStream);
    function IsValidHandle(aHandle:integer):boolean;
    function AddNewRoom:boolean;
    function GetFirstAvailableRoom:integer;
    function GetRoomClientsCount(aRoom:integer):integer;
    function GetFirstRoomClient(aRoom:integer):integer;
    procedure AddClientToRoom(aHandle, aRoom:integer);
    procedure BanPlayerFromRoom(aHandle, aRoom:integer);
    procedure SaveHTMLStatus;
  public
    constructor Create(aMaxRooms:word; aKickTimeout: Word; aHTMLStatusFile, aWelcomeMessage: UnicodeString);
    destructor Destroy; override;
    procedure StartListening(aPort: Word; aServerName: AnsiString);
    procedure StopListening;
    procedure ClearClients;
    procedure MeasurePings;
    procedure UpdateStateIdle;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
    property Listening: boolean read fListening;
    function GetPlayerCount:integer;
    procedure UpdateSettings(aKickTimeout: Word; const aHTMLStatusFile: UnicodeString; const aWelcomeMessage: UnicodeString; const aServerName: AnsiString);
    procedure GetServerInfo(aList: TList);
  end;


implementation

const
  //Server needs to use some text constants locally but can't know about gResTexts
  {$I KM_TextIDs.inc}

{ TKMServerClient }
constructor TKMServerClient.Create(aHandle, aRoom: Integer);
begin
  inherited Create;
  fHandle := aHandle;
  fRoom := aRoom;
  SetLength(fBuffer, 0);
  fBufferSize := 0;
end;


{ TKMClientsList }
destructor TKMClientsList.Destroy;
begin
  Clear; //Free all clients
  inherited;
end;


function TKMClientsList.GetItem(Index: Integer): TKMServerClient;
begin
  Assert(InRange(Index,0,fCount-1),'Tried to access invalid client index');
  Result := fItems[Index];
end;


procedure TKMClientsList.AddPlayer(aHandle, aRoom: integer);
begin
  inc(fCount);
  SetLength(fItems, fCount);
  fItems[fCount-1] := TKMServerClient.Create(aHandle, aRoom);
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
constructor TKMNetServer.Create(aMaxRooms:word; aKickTimeout: word; aHTMLStatusFile, aWelcomeMessage: UnicodeString);
begin
  inherited Create;
  fEmptyGameInfo := TMPGameInfo.Create;
  fEmptyGameInfo.GameTime := -1;
  fMaxRooms := aMaxRooms;
  fKickTimeout := aKickTimeout;
  fHTMLStatusFile := aHTMLStatusFile;
  fWelcomeMessage := aWelcomeMessage;
  fClientList := TKMClientsList.Create;
  {$IFDEF WDC} fServer := TKMNetServerOverbyte.Create; {$ENDIF}
  {$IFDEF FPC} fServer := TKMNetServerLNet.Create;     {$ENDIF}
  fListening := false;
  fRoomCount := 0;
end;


destructor TKMNetServer.Destroy;
begin
  StopListening; //Frees room info
  fServer.Free;
  fClientList.Free;
  fEmptyGameInfo.Free;
  inherited;
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Error(const S: string);
begin
  Status(S);
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Status(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: '+S);
end;


procedure TKMNetServer.StartListening(aPort: Word; aServerName: AnsiString);
begin
  fRoomCount := 0;
  Assert(AddNewRoom); //Must succeed

  fServerName := aServerName;
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  Status('Listening on port ' + IntToStr(aPort));
  fListening := true;
  SaveHTMLStatus;
end;


procedure TKMNetServer.StopListening;
var i:integer;
begin
  fOnStatusMessage := nil;
  fServer.StopListening;
  fListening := false;
  for i:=0 to fRoomCount-1 do
  begin
    FreeAndNil(fRoomInfo[i].GameInfo);
    SetLength(fRoomInfo[i].BannedIPs, 0);
  end;
  SetLength(fRoomInfo,0);
  fRoomCount := 0;
end;


procedure TKMNetServer.ClearClients;
begin
  fClientList.Clear;
end;


procedure TKMNetServer.MeasurePings;
var
  M: TKMemoryStream;
  I: Integer;
  TickCount: DWord;
begin
  TickCount := TimeGet;
  //Sends current ping info to everyone
  M := TKMemoryStream.Create;
  M.Write(fClientList.Count);
  for I := 0 to fClientList.Count - 1 do
  begin
    M.Write(fClientList[I].Handle);
    M.Write(fClientList[I].Ping);
  end;
  SendMessage(NET_ADDRESS_ALL, mk_PingInfo, M);
  M.Free;

  //Measure pings. Iterate backwards so the indexes are maintained after kicking clients
  for I:=fClientList.Count-1 downto 0 do
    if fClientList[I].fPingStarted = 0 then //We have recieved mk_Pong for our previous measurement, so start a new one
    begin
      fClientList[I].fPingStarted := TickCount;
      SendMessage(fClientList[I].fHandle, mk_Ping);
    end
    else
      //If they don't respond within a reasonable time, kick them
      if GetTimeSince(fClientList[I].fPingStarted) > fKickTimeout*1000 then
      begin
        Status('Client timed out '+inttostr(fClientList[I].fHandle));
        SendMessage(fClientList[I].fHandle, mk_Kicked, TX_NET_KICK_TIMEOUT);
        fServer.Kick(fClientList[I].fHandle);
      end;

end;


procedure TKMNetServer.UpdateStateIdle;
begin
  {$IFDEF FPC} fServer.UpdateStateIdle; {$ENDIF}
end;


function TKMNetServer.GetPlayerCount:integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to fClientList.fCount-1 do
    if fClientList.Item[i].fRoom <> -1 then
      inc(Result);
end;


procedure TKMNetServer.UpdateSettings(aKickTimeout: Word; const aHTMLStatusFile: UnicodeString; const aWelcomeMessage: UnicodeString; const aServerName: AnsiString);
begin
  fKickTimeout := aKickTimeout;
  fHTMLStatusFile := aHTMLStatusFile;
  fWelcomeMessage := aWelcomeMessage;
  if fServerName <> aServerName then
    SendMessageA(NET_ADDRESS_ALL, mk_ServerName, aServerName);
  fServerName := aServerName;
end;


procedure TKMNetServer.GetServerInfo(aList: TList);
var I: Integer;
begin
  Assert(aList <> nil);
  for I := 0 to fRoomCount - 1 do
    if GetRoomClientsCount(I) > 0 then
      aList.Add(fRoomInfo[I].GameInfo);
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMNetServer.ClientConnect(aHandle: Integer);
begin
  fClientList.AddPlayer(aHandle, -1); //Clients are not initially put into a room, they choose a room later
  SendMessageA(aHandle, mk_GameVersion, NET_PROTOCOL_REVISON); //First make sure they are using the right version
  if fWelcomeMessage <> '' then SendMessageW(aHandle, mk_WelcomeMessage, fWelcomeMessage); //Welcome them to the server
  SendMessageA(aHandle, mk_ServerName, fServerName);
  SendMessage(aHandle, mk_IndexOnServer, aHandle); //This is the signal that the client may now start sending
end;


procedure TKMNetServer.AddClientToRoom(aHandle, aRoom: Integer);
var I: Integer;
begin
  if fClientList.GetByHandle(aHandle).Room <> -1 then exit; //Changing rooms is not allowed yet

  if aRoom = fRoomCount then
  begin
    if not AddNewRoom then //Create a new room for this client
    begin
      SendMessage(aHandle, mk_RefuseToJoin, TX_NET_INVALID_ROOM);
      fServer.Kick(aHandle);
      Exit;
    end;
  end
  else
    if aRoom = -1 then
    begin
      aRoom := GetFirstAvailableRoom; //Take the first one which has a space (or create a new one if none have spaces)
      if aRoom = -1 then //No rooms available
      begin
        SendMessage(aHandle, mk_RefuseToJoin, TX_NET_INVALID_ROOM);
        fServer.Kick(aHandle);
        Exit;
      end;
    end
    else
      //If the room is outside the valid range
      if not InRange(aRoom,0,fRoomCount-1) then
      begin
        SendMessage(aHandle, mk_RefuseToJoin, TX_NET_INVALID_ROOM);
        fServer.Kick(aHandle);
        Exit;
      end;

  //Make sure the client is not banned by host from this room
  for I:=0 to Length(fRoomInfo[aRoom].BannedIPs)-1 do
    if fRoomInfo[aRoom].BannedIPs[I] = fServer.GetIP(aHandle) then
    begin
      SendMessage(aHandle, mk_RefuseToJoin, TX_NET_BANNED_BY_HOST);
      fServer.Kick(aHandle);
      Exit;
    end;

  Status('Client '+inttostr(aHandle)+' has connected to room '+inttostr(aRoom));
  fClientList.GetByHandle(aHandle).Room := aRoom;

  //Let the first client be a Host
  if fRoomInfo[aRoom].HostHandle = NET_ADDRESS_EMPTY then
  begin
    fRoomInfo[aRoom].HostHandle := aHandle;
    Status('Host rights assigned to '+IntToStr(fRoomInfo[aRoom].HostHandle));
  end;

  SendMessage(aHandle, mk_ConnectedToRoom, fRoomInfo[aRoom].HostHandle);
  MeasurePings;
  SaveHTMLStatus;
end;


procedure TKMNetServer.BanPlayerFromRoom(aHandle, aRoom:integer);
begin
  SetLength(fRoomInfo[aRoom].BannedIPs, Length(fRoomInfo[aRoom].BannedIPs)+1);
  fRoomInfo[aRoom].BannedIPs[Length(fRoomInfo[aRoom].BannedIPs)-1] := fServer.GetIP(aHandle);
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle:integer);
var
  Room: integer;
  Client:TKMServerClient;
  M: TKMemoryStream;
begin
  Client := fClientList.GetByHandle(aHandle);
  if Client = nil then
  begin
    Status('Warning: Client '+inttostr(aHandle)+' was already disconnected');
    exit;
  end;
  Room := Client.Room;
  if Room <> -1 then
    Status('Client '+inttostr(aHandle)+' has disconnected'); //Only log messages for clients who entered a room
  fClientList.RemPlayer(aHandle);

  if Room = -1 then Exit; //The client was not assigned a room yet

  //Send message to all remaining clients that client has disconnected
  SendMessage(NET_ADDRESS_ALL, mk_ClientLost, aHandle);

  //Assign a new host
  if fRoomInfo[Room].HostHandle = aHandle then
  begin
    if GetRoomClientsCount(Room) = 0 then
    begin
      fRoomInfo[Room].HostHandle := NET_ADDRESS_EMPTY; //Room is now empty so we don't need a new host
      fRoomInfo[Room].Password := '';
      fRoomInfo[Room].GameInfo.Free;
      fRoomInfo[Room].GameInfo := TMPGameInfo.Create;
      SetLength(fRoomInfo[Room].BannedIPs, 0);
    end
    else
    begin
      fRoomInfo[Room].HostHandle := GetFirstRoomClient(Room); //Assign hosting rights to the first client in the room

      //Tell everyone about the new host and password/description (so new host knows it)
      M := TKMemoryStream.Create;
      M.Write(fRoomInfo[Room].HostHandle);
      M.WriteA(fRoomInfo[Room].Password);
      M.WriteW(fRoomInfo[Room].GameInfo.Description);
      SendMessageToRoom(mk_ReassignHost, Room, M);
      M.Free;

      Status('Reassigned hosting rights for room '+inttostr(Room)+' to '+inttostr(fRoomInfo[Room].HostHandle));
    end;
  end;
  SaveHTMLStatus;
end;


procedure TKMNetServer.SendMessage(aRecipient: Integer; aKind: TKMessageKind);
var
  M: TKMemoryStream;
begin
  M := TKMemoryStream.Create; //Send empty stream
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessage(aRecipient: Integer; aKind: TKMessageKind; aParam: Integer);
var
  M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.Write(aParam);
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessageA(aRecipient: Integer; aKind: TKMessageKind; aText: AnsiString);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfStringA);

  M := TKMemoryStream.Create;
  M.WriteA(aText);
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessageW(aRecipient: Integer; aKind: TKMessageKind; aText: UnicodeString);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfStringW);

  M := TKMemoryStream.Create;
  M.WriteW(aText);
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessage(aRecipient: Integer; aKind: TKMessageKind; aStream: TKMemoryStream);
begin
  //Send stream without changes
  SendMessageAct(aRecipient, aKind, aStream);
end;


procedure TKMNetServer.SendMessageToRoom(aKind: TKMessageKind; aRoom: Integer; aStream: TKMemoryStream);
var I: Integer;
begin
  //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
  for I := fClientList.Count-1 downto 0 do
    if fClientList[i].Room = aRoom then
      SendMessage(fClientList[i].Handle, aKind, aStream);
end;


//Assemble the packet as [Sender.Recepient.Length.Data]
procedure TKMNetServer.SendMessageAct(aRecipient: Integer; aKind: TKMessageKind; aStream: TKMemoryStream);
var
  I: Integer;
  M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;

  //Header
  M.Write(Integer(NET_ADDRESS_SERVER)); //Make sure constant gets treated as 4byte integer
  M.Write(aRecipient);
  M.Write(Integer(1 + aStream.Size)); //Message kind + data size

  //Contents
  M.Write(Byte(aKind));
  aStream.Position := 0;
  M.CopyFrom(aStream, aStream.Size);

  if M.Size > MAX_PACKET_SIZE then
  begin
    Status('Error: Packet over size limit');
    M.Free;
    Exit;
  end;

  if aRecipient = NET_ADDRESS_ALL then
    //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
    for I := fClientList.Count - 1 downto 0 do
      DoSendData(fClientList[i].Handle, M.Memory, M.Size)
  else
    DoSendData(aRecipient, M.Memory, M.Size);

  M.Free;
end;


//Wrapper around fServer.SendData so we can count TX bytes (don't use fServer.SendData anywhere else)
procedure TKMNetServer.DoSendData(aRecipient: Integer; aData: Pointer; aLength: Cardinal);
begin
  Inc(BytesTX, aLength);
  fServer.SendData(aRecipient, aData, aLength);
end;


procedure TKMNetServer.RecieveMessage(aSenderHandle:integer; aData:pointer; aLength:cardinal);
var
  Kind: TKMessageKind;
  M, M2: TKMemoryStream;
  tmpInteger: Integer;
  tmpStringA: AnsiString;
  Client: TKMServerClient;
  SenderIsHost: Boolean;
  SenderRoom: Integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message');

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(Kind, SizeOf(TKMessageKind));

  //Sometimes client disconnects then we recieve a late packet (e.g. mk_Pong), in which case ignore it
  if fClientList.GetByHandle(aSenderHandle) = nil then
  begin
    Status('Warning: Received data from an unassigned client');
    exit;
  end;

  SenderRoom := fClientList.GetByHandle(aSenderHandle).Room;
  SenderIsHost := (SenderRoom <> -1) and
                  (fRoomInfo[SenderRoom].HostHandle = aSenderHandle);

  case Kind of
    mk_JoinRoom:
            begin
              M.Read(tmpInteger); //Room to join
              if InRange(tmpInteger, 0, Length(fRoomInfo)-1)
              and (fRoomInfo[tmpInteger].HostHandle <> NET_ADDRESS_EMPTY)
              //Once game has started don't ask for passwords so clients can reconnect
              and (fRoomInfo[tmpInteger].GameInfo.GameState = mgsLobby)
              and (fRoomInfo[tmpInteger].Password <> '') then
                SendMessage(aSenderHandle, mk_ReqPassword)
              else
                AddClientToRoom(aSenderHandle, tmpInteger);
            end;
    mk_Password:
            begin
              M.Read(tmpInteger); //Room to join
              M.ReadA(tmpStringA); //Password
              if InRange(tmpInteger, 0, Length(fRoomInfo)-1)
              and (fRoomInfo[tmpInteger].HostHandle <> NET_ADDRESS_EMPTY)
              and (fRoomInfo[tmpInteger].Password = tmpStringA) then
                AddClientToRoom(aSenderHandle, tmpInteger)
              else
                SendMessage(aSenderHandle, mk_ReqPassword);
            end;
    mk_SetPassword:
            if SenderIsHost then
            begin
              M.ReadA(tmpStringA); //Password
              fRoomInfo[SenderRoom].Password := tmpStringA;
            end;
    mk_SetGameInfo:
            if SenderIsHost then
            begin
              fRoomInfo[SenderRoom].GameInfo.LoadFromStream(M);
              SaveHTMLStatus;
            end;
    mk_KickPlayer:
            if SenderIsHost then
            begin
              M.Read(tmpInteger);
              if fClientList.GetByHandle(tmpInteger) <> nil then
              begin
                SendMessage(tmpInteger, mk_Kicked, TX_NET_KICK_BY_HOST);
                fServer.Kick(tmpInteger);
              end;
            end;
    mk_BanPlayer:
            if SenderIsHost then
            begin
              M.Read(tmpInteger);
              if fClientList.GetByHandle(tmpInteger) <> nil then
              begin
                BanPlayerFromRoom(tmpInteger, SenderRoom);
                SendMessage(tmpInteger, mk_Kicked, TX_NET_BANNED_BY_HOST);
                fServer.Kick(tmpInteger);
              end;
            end;
    mk_GiveHost:
            if SenderIsHost then
            begin
              M.Read(tmpInteger);
              if fClientList.GetByHandle(tmpInteger) <> nil then
              begin
                fRoomInfo[SenderRoom].HostHandle := tmpInteger;
                //Tell everyone about the new host and password/description (so new host knows it)
                M2 := TKMemoryStream.Create;
                M2.Write(fRoomInfo[SenderRoom].HostHandle);
                M2.WriteA(fRoomInfo[SenderRoom].Password);
                M2.WriteW(fRoomInfo[SenderRoom].GameInfo.Description);
                SendMessageToRoom(mk_ReassignHost, SenderRoom, M2);
                M2.Free;
              end;
            end;
    mk_ResetBans:
            if SenderIsHost then
            begin
              SetLength(fRoomInfo[SenderRoom].BannedIPs, 0);
            end;
    mk_GetServerInfo:
            begin
              M2 := TKMemoryStream.Create;
              SaveToStream(M2);
              SendMessage(aSenderHandle, mk_ServerInfo, M2);
              M2.Free;
            end;
    mk_Pong:
            begin
              Client := fClientList.GetByHandle(aSenderHandle);
              if (Client.fPingStarted <> 0) then
              begin
                Client.Ping := Math.Min(GetTimeSince(Client.fPingStarted), High(Word));
                Client.fPingStarted := 0;
              end;
            end;
  end;

  M.Free;
end;


//Someone has send us something
//Send only complete messages to allow to add server messages inbetween
procedure TKMNetServer.DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
var PacketSender,PacketRecipient:integer; PacketLength:Cardinal; i,SenderRoom:integer; SenderClient: TKMServerClient;
begin
  Inc(BytesRX, aLength);

  SenderClient := fClientList.GetByHandle(aHandle);
  if SenderClient = nil then
  begin
    Status('Warning: Data Available from an unassigned client');
    exit;
  end;

  //Append new data to buffer
  SetLength( SenderClient.fBuffer, SenderClient.fBufferSize + aLength);
  Move(aData^, SenderClient.fBuffer[SenderClient.fBufferSize], aLength);
  SenderClient.fBufferSize := SenderClient.fBufferSize + aLength;

  //Try to read data packet from buffer
  while SenderClient.fBufferSize >= 12 do
  begin
    PacketSender := PInteger(@SenderClient.fBuffer[0])^;
    PacketRecipient := PInteger(@SenderClient.fBuffer[4])^;
    PacketLength := PCardinal(@SenderClient.fBuffer[8])^;

    //Do some simple range checking to try to detect when there is a serious error or flaw in the code (i.e. Random data in the buffer)
    if not (IsValidHandle(PacketRecipient) and IsValidHandle(PacketSender) and (PacketLength <= MAX_PACKET_SIZE)) then
    begin
      //When we receive corrupt data kick the client since we have no way to recover (if in-game client will auto reconnect)
      Status('Warning: Corrupt data received, kicking client '+IntToStr(aHandle));
      SenderClient.fBufferSize := 0;
      SetLength(SenderClient.fBuffer, 0);
      fServer.Kick(aHandle);
      exit;
    end;

    if PacketLength > SenderClient.fBufferSize-12 then
      exit; //This message was split, so we must wait for the remainder of the message to arrive

    SenderRoom := fClientList.GetByHandle(aHandle).Room;

    //If sender from packet contents doesn't match the socket handle, don't process this packet (client trying to fake sender)
    if PacketSender = aHandle then
      case PacketRecipient of
        NET_ADDRESS_OTHERS: //Transmit to all except sender
            //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
            for i:=fClientList.Count-1 downto 0 do
                if (aHandle <> fClientList[i].Handle) and (SenderRoom = fClientList[i].Room) then
                  DoSendData(fClientList[i].Handle, @SenderClient.fBuffer[0], PacketLength+12);
        NET_ADDRESS_ALL: //Transmit to all including sender (used mainly by TextMessages)
                //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
                for i:=fClientList.Count-1 downto 0 do
                  if SenderRoom = fClientList[i].Room then
                    DoSendData(fClientList[i].Handle, @SenderClient.fBuffer[0], PacketLength+12);
        NET_ADDRESS_HOST:
                if SenderRoom <> -1 then
                  DoSendData(fRoomInfo[SenderRoom].HostHandle, @SenderClient.fBuffer[0], PacketLength+12);
        NET_ADDRESS_SERVER:
                RecieveMessage(PacketSender, @SenderClient.fBuffer[12], PacketLength);
        else    DoSendData(PacketRecipient, @SenderClient.fBuffer[0], PacketLength+12);
      end;

    //Processing that packet may have caused this client to be kicked (joining room where banned)
    //and in that case SenderClient is invalid so we must exit immediately
    if fClientList.GetByHandle(aHandle) = nil then
      Exit;

    if SenderClient.fBufferSize > 12+PacketLength then //Check range
      Move(SenderClient.fBuffer[12+PacketLength], SenderClient.fBuffer[0], SenderClient.fBufferSize-PacketLength-12);
    SenderClient.fBufferSize := SenderClient.fBufferSize - PacketLength - 12;
  end;
end;


procedure TKMNetServer.SaveToStream(aStream: TKMemoryStream);
var
  i, RoomsNeeded, EmptyRoomID: integer;
  NeedEmptyRoom: boolean;
begin
  RoomsNeeded := 0;
  for i:=0 to fRoomCount-1 do
    if GetRoomClientsCount(i) > 0 then
      inc(RoomsNeeded);

  if RoomsNeeded < fMaxRooms then
  begin
    inc(RoomsNeeded); //Need 1 empty room at the end, if there is space
    NeedEmptyRoom := true;
  end
  else
    NeedEmptyRoom := false;

  aStream.Write(RoomsNeeded);
  EmptyRoomID := fRoomCount;
  for i:=0 to fRoomCount-1 do
  begin
    if GetRoomClientsCount(i) = 0 then
    begin
      if EmptyRoomID = fRoomCount then
        EmptyRoomID := i;
    end
    else
    begin
      aStream.Write(i); //RoomID
      fRoomInfo[i].GameInfo.SaveToStream(aStream);
    end;
  end;
  //Write out the empty room at the end
  if NeedEmptyRoom then
  begin
    aStream.Write(EmptyRoomID); //RoomID
    fEmptyGameInfo.SaveToStream(aStream);
  end;
end;


function TKMNetServer.IsValidHandle(aHandle: Integer): Boolean;
begin
  //Can not use "in [...]" with negative numbers
  Result := (aHandle = NET_ADDRESS_OTHERS) or (aHandle = NET_ADDRESS_ALL)
         or (aHandle = NET_ADDRESS_HOST) or (aHandle = NET_ADDRESS_SERVER)
         or InRange(aHandle, FIRST_TAG, fServer.GetMaxHandle);
end;


function TKMNetServer.AddNewRoom:boolean;
begin
  if fRoomCount = fMaxRooms then
  begin
    Result := false;
    Exit;
  end;
  Result := true;
  Inc(fRoomCount);
  SetLength(fRoomInfo,fRoomCount);
  fRoomInfo[fRoomCount-1].HostHandle := NET_ADDRESS_EMPTY;
  fRoomInfo[fRoomCount-1].Password := '';
  fRoomInfo[fRoomCount-1].GameInfo := TMPGameInfo.Create;
  SetLength(fRoomInfo[fRoomCount-1].BannedIPs, 0);
end;


function TKMNetServer.GetFirstAvailableRoom:integer;
var i:integer;
begin
  for i:=0 to fRoomCount-1 do
    if GetRoomClientsCount(i) = 0 then
    begin
      Result := i;
      exit;
    end;
  if AddNewRoom then //Otherwise we must create a room
    Result := fRoomCount-1
  else
    Result := -1;
end;


function TKMNetServer.GetRoomClientsCount(aRoom:integer):integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to fClientList.Count-1 do
    if fClientList[i].Room = aRoom then
      inc(Result);
end;


function TKMNetServer.GetFirstRoomClient(aRoom:integer):integer;
var i:integer;
begin
  for i:=0 to fClientList.Count-1 do
    if fClientList[i].Room = aRoom then
    begin
      Result := fClientList[i].fHandle;
      exit;
    end;
  raise Exception.Create('');
end;


procedure TKMNetServer.SaveHTMLStatus;

  function AddThousandSeparator(S: string; Chr: Char=','): string;
  var I: Integer;
  begin
    Result := S;
    I := Length(S) - 2;
    while I > 1 do
    begin
      Insert(Chr, Result, I);
      I := I - 3;
    end;
  end;

  function ColorToText(aCol: Cardinal): string;
  begin
    Result := '#' + IntToHex(aCol and $FF, 2) + IntToHex((aCol shr 8) and $FF, 2) + IntToHex((aCol shr 16) and $FF, 2);
  end;

const
  BOOL_TEXT: array[Boolean] of string = ('0', '1');
var
  i,k,PlayerCount,ClientCount,RoomCount:integer;
  XML: TXmlVerySimple;
  HTML: string;
  RoomCountNode, ClientCountNode, PlayerCountNode, Node: TXmlNode;
  MyFile:TextFile;
begin
  if fHTMLStatusFile = '' then exit; //Means do not write status

  RoomCount := 0;
  PlayerCount := 0;
  ClientCount := 0;

  XML := TXmlVerySimple.Create;

  try
    //HTML header
    HTML := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+sLineBreak+
            '<HTML>'+sLineBreak+'<HEAD>'+sLineBreak+'  <TITLE>KaM Remake Server Status</TITLE>'+sLineBreak+
            '  <meta http-equiv="content-type" content="text/html; charset=utf-8">'+sLineBreak+'</HEAD>'+sLineBreak;
    HTML := HTML + '<BODY>'+sLineBreak;
    HTML := HTML + '<TABLE border="1">'+sLineBreak+'<TR><TD><b>Room ID</b></TD><TD><b>State</b><TD><b>Player Count</b></TD></TD><TD><b>Map</b></TD><TD><b>Game Time</b></TD><TD><b>Player Names</b></TD></TR>'+sLineBreak;

    //XML header
    XML.Root.NodeName := 'server';
    RoomCountNode := XML.Root.AddChild('roomcount'); //Set it later
    PlayerCountNode := XML.Root.AddChild('playercount');
    ClientCountNode := XML.Root.AddChild('clientcount');
    XML.Root.AddChild('bytessent').Text := IntToStr(BytesTX);
    XML.Root.AddChild('bytesreceived').Text := IntToStr(BytesRX);

    for i:=0 to fRoomCount-1 do
      if GetRoomClientsCount(i) > 0 then
      begin
        inc(RoomCount);
        inc(PlayerCount, fRoomInfo[i].GameInfo.PlayerCount);
        inc(ClientCount, fRoomInfo[i].GameInfo.ConnectedPlayerCount);
        //HTML room info
        HTML := HTML + '<TR><TD>'+IntToStr(i)+'</TD><TD>'+XMLEscape(GameStateText[fRoomInfo[i].GameInfo.GameState])+
                       '</TD><TD>'+IntToStr(fRoomInfo[i].GameInfo.ConnectedPlayerCount)+
                       '</TD><TD>'+XMLEscape(fRoomInfo[i].GameInfo.Map)+
                       '&nbsp;</TD><TD>'+XMLEscape(fRoomInfo[i].GameInfo.GetFormattedTime)+
                       //HTMLPlayersList does escaping itself
                       '&nbsp;</TD><TD>'+fRoomInfo[i].GameInfo.HTMLPlayersList+'</TD></TR>'+sLineBreak;
        //XML room info
        Node := XML.Root.AddChild('room');
        Node.Attribute['id'] := IntToStr(i);
        Node.AddChild('state').Text := GameStateText[fRoomInfo[i].GameInfo.GameState];
        Node.AddChild('roomplayercount').Text := IntToStr(fRoomInfo[i].GameInfo.PlayerCount);
        Node.AddChild('map').Text := fRoomInfo[i].GameInfo.Map;
        Node.AddChild('gametime').Text := fRoomInfo[i].GameInfo.GetFormattedTime;
        with Node.AddChild('players') do
        begin
          for k:=1 to fRoomInfo[i].GameInfo.PlayerCount do
            with AddChild('player') do
            begin
              Text := UnicodeString(fRoomInfo[i].GameInfo.Players[k].Name);
              SetAttribute('color', ColorToText(fRoomInfo[i].GameInfo.Players[k].Color));
              SetAttribute('connected', BOOL_TEXT[fRoomInfo[i].GameInfo.Players[k].Connected]);
              SetAttribute('type', NetPlayerTypeName[fRoomInfo[i].GameInfo.Players[k].PlayerType]);
            end;
        end;
      end;
    //Set counts in XML
    RoomCountNode.Text := IntToStr(RoomCount);
    PlayerCountNode.Text := IntToStr(PlayerCount);
    ClientCountNode.Text := IntToStr(ClientCount);

    //HTML footer
    HTML := HTML + '</TABLE>'+sLineBreak+
                   '<p>Total sent: '+AddThousandSeparator(IntToStr(BytesTX))+' bytes</p>'+sLineBreak+
                   '<p>Total received: '+AddThousandSeparator(IntToStr(BytesRX))+' bytes</p>'+sLineBreak+
                   '</BODY>'+sLineBreak+'</HTML>';

    //Write HTML
    AssignFile(MyFile, fHTMLStatusFile);
    ReWrite(MyFile);
    Write(MyFile,HTML);
    CloseFile(MyFile);
    //Write XML
    XML.SaveToFile(ChangeFileExt(fHTMLStatusFile,'.xml'));
  finally
    XML.Free;
  end;
end;


end.
