unit KM_NetServer;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_CommonTypes, KM_Defaults
     {$IFDEF MSWindows} ,Windows {$ENDIF}
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

      //Following commands will be added to TKMessageKind
      mk_KickPlayer
      mk_WasKicked
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

    fMaxRooms:word;
    fHTMLStatusFile:string;
    fKickTimeout:word;
    fRoomCount:integer;
    fRoomInfo:array of record
                         HostHandle:integer;
                         Joinable:boolean;
                         GameState:string;
                         Players:string;
                       end;

    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure Status(const S: string);
    procedure ClientConnect(aHandle:integer);
    procedure ClientDisconnect(aHandle:integer);
    procedure SendMessage(aRecipient:integer; aKind:TKMessageKind; aMsg:integer; aText:string);
    procedure RecieveMessage(aSenderHandle:integer; aData:pointer; aLength:cardinal);
    procedure DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
    procedure SaveServerInfo(var M:TKMemoryStream);
    function IsValidHandle(aHandle:integer):boolean;
    function AddNewRoom:boolean;
    function GetFirstAvailableRoom:integer;
    function GetRoomPlayersCount(aRoom:integer):integer;
    function GetFirstRoomClient(aRoom:integer):integer;
    procedure AddClientToRoom(aHandle, Room:integer);
    procedure SaveHTMLStatus;
  public
    constructor Create(aMaxRooms:word; aKickTimeout: word; aHTMLStatusFile:string);
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    procedure ClearClients;
    procedure MeasurePings;
    procedure UpdateStateIdle;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
    property Listening: boolean read fListening;
  end;


implementation
  uses KM_Utils; //Needed in Linux for FakeGetTickCount


{ TKMServerClient }
constructor TKMServerClient.Create(aHandle, aRoom: integer);
begin
  Inherited Create;
  fHandle := aHandle;
  fRoom := aRoom;
  SetLength(fBuffer,0);
  fBufferSize := 0;
end;


{ TKMClientsList }
function TKMClientsList.GetItem(Index: integer): TKMServerClient;
begin
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
constructor TKMNetServer.Create(aMaxRooms:word; aKickTimeout: word; aHTMLStatusFile:string);
begin
  Inherited Create;
  fMaxRooms := aMaxRooms;
  fKickTimeout := aKickTimeout;
  fHTMLStatusFile := aHTMLStatusFile;
  fClientList := TKMClientsList.Create;
  {$IFDEF WDC} fServer := TKMNetServerOverbyte.Create; {$ENDIF}
  {$IFDEF FPC} fServer := TKMNetServerLNet.Create;     {$ENDIF}
  fListening := false;
  fRoomCount := 0;
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
  Status('Error '+S);
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Status(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: '+S);
end;


procedure TKMNetServer.StartListening(aPort:string);
begin
  fRoomCount := 0;
  assert(AddNewRoom); //Must succeed

  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  Status('Listening on port '+aPort);
  fListening := true;
  SaveHTMLStatus;
end;


procedure TKMNetServer.StopListening;
begin
  fOnStatusMessage := nil;
  fServer.StopListening;
  fListening := false;
  SetLength(fRoomInfo,0);
  fRoomCount := 0;
end;


procedure TKMNetServer.ClearClients;
begin
  fClientList.Clear;
end;


procedure TKMNetServer.MeasurePings;
var M:TKMemoryStream; i: integer; TickCount:DWord;
begin
  TickCount := {$IFDEF MSWindows}GetTickCount{$ENDIF}
               {$IFDEF Unix} FakeGetTickCount{$ENDIF};
  //Sends current ping info to everyone
  M := TKMemoryStream.Create;
  M.Write(fClientList.Count);
  for i:=0 to fClientList.Count-1 do
  begin
    M.Write(fClientList[i].Handle);
    M.Write(fClientList[i].Ping);
  end;
  SendMessage(NET_ADDRESS_ALL, mk_PingInfo, 0, M.ReadAsText);
  M.Free;
  //Measure pings. Iterate backwards so the indexes are maintained after kicking clients
  for i:=fClientList.Count-1 downto 0 do
    if fClientList[i].fPingStarted = 0 then //We have recieved mk_Pong for our previous measurement, so start a new one
    begin
      fClientList[i].fPingStarted := TickCount;
      SendMessage(fClientList[i].fHandle, mk_Ping, 0, '');
    end
    else
      //If they don't respond within a reasonable time, kick them
      if TickCount-fClientList[i].fPingStarted > fKickTimeout*1000 then
      begin
        Status('Client timed out '+inttostr(fClientList[i].fHandle));
        fServer.Kick(fClientList[i].fHandle);
      end;

end;


procedure TKMNetServer.UpdateStateIdle;
begin
  {$IFDEF FPC} fServer.UpdateStateIdle; {$ENDIF}
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMNetServer.ClientConnect(aHandle:integer);
begin
  fClientList.AddPlayer(aHandle, -1); //Clients are not initially put into a room, they chose a room later
  SendMessage(aHandle, mk_GameVersion, 0, GAME_REVISION); //First make sure they are using the right version
  SendMessage(aHandle, mk_IndexOnServer, aHandle, ''); //This is the signal that the client may now start sending
end;


procedure TKMNetServer.AddClientToRoom(aHandle, Room:integer);
begin
  if fClientList.GetByHandle(aHandle).Room <> -1 then exit; //Changing rooms is not allowed yet

  if Room = fRoomCount then
  begin
    if not AddNewRoom then //Create a new room for this client
    begin
      SendMessage(aHandle, mk_RefuseToJoin, 0, 'Room limit reached');
      fServer.Kick(aHandle);
      Exit;
    end;
  end
  else
    if Room = -1 then
    begin
      Room := GetFirstAvailableRoom; //Take the first one which has a space (or create a new one if none have spaces)
      if Room = -1 then //No rooms available
      begin
        SendMessage(aHandle, mk_RefuseToJoin, 0, 'Server is full');
        fServer.Kick(aHandle);
        Exit;
      end;
    end
    else
      //If the room is outside the valid range
      if not InRange(Room,0,fRoomCount-1) then
      begin
        SendMessage(aHandle, mk_RefuseToJoin, 0, 'Invalid room number');
        fServer.Kick(aHandle);
        Exit;
      end;

  Status('Client '+inttostr(aHandle)+' has connected to room '+inttostr(Room));
  fClientList.GetByHandle(aHandle).Room := Room;

  //Let the first client be a Host
  if fRoomInfo[Room].HostHandle = NET_ADDRESS_EMPTY then
  begin
    fRoomInfo[Room].HostHandle := aHandle;
    SendMessage(aHandle, mk_HostingRights, 0, '');
    Status('Host rights assigned to '+inttostr(fRoomInfo[Room].HostHandle));
  end;

  SendMessage(aHandle, mk_ConnectedToRoom, Room, '');
  MeasurePings;
  SaveHTMLStatus;
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle:integer);
var Room: integer;
begin
  Room := fClientList.GetByHandle(aHandle).Room;
  if Room <> -1 then
    Status('Client '+inttostr(aHandle)+' has disconnected'); //Only log messages for clients who entered a room
  fClientList.RemPlayer(aHandle);

  if Room = -1 then Exit; //The client was not assigned a room yet

  //Send message to all remaining clients that client has disconnected
  SendMessage(NET_ADDRESS_ALL, mk_ClientLost, aHandle, '');

  //Assign a new host
  if fRoomInfo[Room].HostHandle = aHandle then
  begin
    if GetRoomPlayersCount(Room) = 0 then
    begin
      fRoomInfo[Room].HostHandle := NET_ADDRESS_EMPTY; //Room is now empty so we don't need a new host
      fRoomInfo[Room].Joinable := true;
      fRoomInfo[Room].GameState := 'None';
      fRoomInfo[Room].Players := '';
    end
    else
    begin
      fRoomInfo[Room].HostHandle := GetFirstRoomClient(Room); //Assign hosting rights to the first client in the room
      SendMessage(NET_ADDRESS_ALL, mk_ReassignHost, fRoomInfo[Room].HostHandle, ''); //Tell everyone about the new host
      Status('Reassigned hosting rights for room '+inttostr(Room)+' to '+inttostr(fRoomInfo[Room].HostHandle));
    end;
  end;
  SaveHTMLStatus;
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

  if M.Size > MAX_PACKET_SIZE then
  begin
    Status('Error: Packet over size limit');
    exit;
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
    pfText:   M.Read(Msg);
  end;
  M.Free;

  case Kind of
    mk_JoinRoom:  AddClientToRoom(aSenderHandle,Param);
    mk_RoomOpen:  fRoomInfo[ fClientList.GetByHandle(aSenderHandle).Room ].Joinable := true;
    mk_RoomClose: fRoomInfo[ fClientList.GetByHandle(aSenderHandle).Room ].Joinable := false;
    mk_SetGameState:
            begin
              fRoomInfo[ fClientList.GetByHandle(aSenderHandle).Room ].GameState := Msg;
              SaveHTMLStatus;
            end;
    mk_SetPlayerList:
            begin
              fRoomInfo[ fClientList.GetByHandle(aSenderHandle).Room ].Players := Msg;
              SaveHTMLStatus;
            end;
    mk_GetServerInfo:
            begin
              M := TKMemoryStream.Create;
              SaveServerInfo(M);
              SendMessage(aSenderHandle, mk_ServerInfo, 0, M.ReadAsText);
              M.Free;
            end;
    mk_Pong:
            begin
             //Sometimes client disconnects then we recieve a late mk_Pong, in which case ignore it
             if (fClientList.GetByHandle(aSenderHandle) <> nil) and (fClientList.GetByHandle(aSenderHandle).fPingStarted <> 0) then
             begin
               fClientList.GetByHandle(aSenderHandle).Ping := Math.Min({$IFDEF MSWindows}GetTickCount{$ENDIF}
                                                                       {$IFDEF Unix} FakeGetTickCount{$ENDIF}
                                                                       - fClientList.GetByHandle(aSenderHandle).fPingStarted, High(Word));
               fClientList.GetByHandle(aSenderHandle).fPingStarted := 0;
             end;
            end;
  end;

end;


//Someone has send us something
//Send only complete messages to allow to add server messages inbetween
procedure TKMNetServer.DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
var PacketSender,PacketRecipient:integer; PacketLength:Cardinal; i,SenderRoom:integer; SenderClient: TKMServerClient;
begin
  SenderClient := fClientList.GetByHandle(aHandle);
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
      //When we have a corrupt buffer from a client clear it so the next packet can be processed
      Status('Corrupt data received from client '+IntToStr(aHandle));
      SenderClient.fBufferSize := 0;
      SetLength(SenderClient.fBuffer, 0);
      exit;
    end;

    if PacketLength > SenderClient.fBufferSize-12 then
      exit; //This message was split, so we must wait for the remainder of the message to arrive

    SenderRoom := fClientList.GetByHandle(aHandle).Room;

    case PacketRecipient of
      NET_ADDRESS_OTHERS: //Transmit to all except sender
          for i:=0 to fClientList.Count-1 do
              if (aHandle <> fClientList[i].Handle) and (SenderRoom = fClientList[i].Room) then
                fServer.SendData(fClientList[i].Handle, @SenderClient.fBuffer[0], PacketLength+12);
      NET_ADDRESS_ALL: //Transmit to all including sender (used mainly by TextMessages)
              for i:=0 to fClientList.Count-1 do
                if SenderRoom = fClientList[i].Room then
                  fServer.SendData(fClientList[i].Handle, @SenderClient.fBuffer[0], PacketLength+12);
      NET_ADDRESS_HOST:
              fServer.SendData(fRoomInfo[SenderRoom].HostHandle, @SenderClient.fBuffer[0], PacketLength+12);
      NET_ADDRESS_SERVER:
              RecieveMessage(PacketSender, @SenderClient.fBuffer[12], PacketLength);
      else    fServer.SendData(PacketRecipient, @SenderClient.fBuffer[0], PacketLength+12);
    end;

    if 12+PacketLength < SenderClient.fBufferSize then //Check range
      Move(SenderClient.fBuffer[12+PacketLength], SenderClient.fBuffer[0], SenderClient.fBufferSize-PacketLength-12);
    SenderClient.fBufferSize := SenderClient.fBufferSize - PacketLength - 12;
  end;
end;


procedure TKMNetServer.SaveServerInfo(var M:TKMemoryStream);
var i, RoomsNeeded, PlayerCount, EmptyRoomID: integer; NeedEmptyRoom: boolean;
begin
  RoomsNeeded := 0;
  for i:=0 to fRoomCount-1 do
    if GetRoomPlayersCount(i) > 0 then
      inc(RoomsNeeded);

  if RoomsNeeded < fMaxRooms then
  begin
    inc(RoomsNeeded); //Need 1 empty room at the end, if there is space
    NeedEmptyRoom := true;
  end
  else
    NeedEmptyRoom := false;

  M.Write(RoomsNeeded);
  EmptyRoomID := fRoomCount;
  for i:=0 to fRoomCount-1 do
  begin
    PlayerCount := GetRoomPlayersCount(i);
    if PlayerCount = 0 then
    begin
      if EmptyRoomID = fRoomCount then
        EmptyRoomID := i;
    end
    else
    begin
      PlayerCount := GetRoomPlayersCount(i);
      M.Write(i); //RoomID
      M.Write(PlayerCount);
      M.Write(fRoomInfo[i].GameState);
      M.Write(fRoomInfo[i].Players);
    end;
  end;
  //Write out the empty room at the end
  if NeedEmptyRoom then
  begin
    PlayerCount := 0;
    M.Write(EmptyRoomID); //RoomID
    M.Write(PlayerCount);
    M.Write('None'); //Gamestate
    M.Write(''); //Players
  end;
end;


function TKMNetServer.IsValidHandle(aHandle:integer):boolean;
begin
  //Can't use "in [...]" with negative numbers
  Result := (aHandle=NET_ADDRESS_OTHERS)or(aHandle=NET_ADDRESS_ALL)or(aHandle=NET_ADDRESS_HOST)or(aHandle=NET_ADDRESS_SERVER)or
            InRange(aHandle,FIRST_TAG,fServer.GetLatestHandle);
end;


function TKMNetServer.AddNewRoom:boolean;
begin
  if fRoomCount = fMaxRooms then
  begin
    Result := false;
    Exit;
  end;
  Result := true;
  inc(fRoomCount);
  SetLength(fRoomInfo,fRoomCount);
  fRoomInfo[fRoomCount-1].Joinable := true;
  fRoomInfo[fRoomCount-1].HostHandle := NET_ADDRESS_EMPTY;
  fRoomInfo[fRoomCount-1].GameState := '';
  fRoomInfo[fRoomCount-1].Players := '';
end;


function TKMNetServer.GetFirstAvailableRoom:integer;
var i:integer;
begin
  for i:=0 to fRoomCount-1 do
    if fRoomInfo[i].Joinable or (GetRoomPlayersCount(i) = 0) then
    begin
      Result := i;
      fRoomInfo[i].Joinable := true; //Empty rooms are reset to joinable
      exit;
    end;
  if AddNewRoom then //Otherwise we must create a room
    Result := fRoomCount-1
  else
    Result := -1;
end;


function TKMNetServer.GetRoomPlayersCount(aRoom:integer):integer;
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
  Result := -1;
  Assert(false);
end;


procedure TKMNetServer.SaveHTMLStatus;
var
  i:integer;
  HTML:string;
  HTMLFile:TextFile;
begin
  if fHTMLStatusFile = '' then exit; //Means do not write status

  HTML := '<HTML><HEAD><TITLE>KaM Remake Server Status</TITLE></HEAD>'+#13;
  HTML := HTML + '<BODY>'+#13;
  HTML := HTML + '<TABLE border="1"><TR><TD><b>Room ID</b></TD><TD><b>State</b></TD><TD><b>Player Count</b></TD><TD><b>Player Names</b></TD></TR>'+#13;
  for i:=0 to fRoomCount-1 do
    if GetRoomPlayersCount(i) > 0 then
      HTML := HTML + '<TR><TD>'+IntToStr(i)+'</TD><TD>'+fRoomInfo[i].GameState+
                     '</TD><TD>'+IntToStr(GetRoomPlayersCount(i))+'</TD><TD>'+StringReplace(fRoomInfo[i].Players,'|',', ',[rfReplaceAll])+'</TD></TR>'+#13;

  HTML := HTML + '</TABLE></BODY></HTML>';

  AssignFile(HTMLFile, fHTMLStatusFile);
  ReWrite(HTMLFile);
  Write(HTMLFile,HTML);
  CloseFile(HTMLFile);
end;


end.
