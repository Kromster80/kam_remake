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

const
  //todo: This should be in config file for both game and dedicated server
  KICK_TIMEOUT = 20000; //20 sec

type
  TKMServerClient = class
  private
    fHandle:integer;
    fPingStarted:cardinal;
    fPing:word;
  public
    constructor Create(aHandle:integer);
    property Handle:integer read fHandle; //ReadOnly
    property Ping:word read fPing write fPing;
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
    {$IFDEF WDC} fServer:TKMNetServerOverbyte; {$ENDIF}
    {$IFDEF FPC} fServer:TKMNetServerLNet;     {$ENDIF}

    fClientList:TKMClientsList;
    fHostHandle:integer;
    fListening:boolean;

    fBufferSize:cardinal;
    fBuffer:array of byte;

    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure Status(const S: string);
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
    procedure UpdateStateIdle;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
    property Listening: boolean read fListening;
  end;


implementation
  uses KM_Utils; //Needed in Linux for FakeGetTickCount


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
  {$IFDEF WDC} fServer := TKMNetServerOverbyte.Create; {$ENDIF}
  {$IFDEF FPC} fServer := TKMNetServerLNet.Create;     {$ENDIF}
  fListening := false;
  SetLength(fBuffer,0);
  fBufferSize := 0;
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
  SetLength(fBuffer,0);
  fBufferSize := 0;
  fHostHandle := NET_ADDRESS_EMPTY;
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  Status('Listening on port '+aPort);
  fListening := true;
end;


procedure TKMNetServer.StopListening;
begin
  SetLength(fBuffer,0);
  fBufferSize := 0;
  fOnStatusMessage := nil;
  fServer.StopListening;
  fListening := false;
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
  //Measure pings
  for i:=0 to fClientList.Count-1 do
    if fClientList[i].fPingStarted = 0 then //We have recieved mk_Pong for our previous measurement, so start a new one
    begin
      fClientList[i].fPingStarted := TickCount;
      SendMessage(fClientList[i].fHandle, mk_Ping, 0, '');
    end
    else
      //If they don't respond within a reasonable time, kick them
      if TickCount-fClientList[i].fPingStarted > KICK_TIMEOUT then
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
  Status('Client has connected '+inttostr(aHandle));

  fClientList.AddPlayer(aHandle);
  SendMessage(aHandle, mk_GameVersion, 0, GAME_REVISION); //First make sure they are using the right version

  //Let the first client be a Host
  if fHostHandle = NET_ADDRESS_EMPTY then
  begin
    fHostHandle := aHandle;
    SendMessage(aHandle, mk_HostingRights, 0, '');
    Status('Host rights assigned to '+inttostr(fHostHandle));
  end;

  SendMessage(aHandle, mk_IndexOnServer, aHandle, '');
  MeasurePings;
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle:integer);
begin
  Status('Client has disconnected '+inttostr(aHandle));

  fClientList.RemPlayer(aHandle);

  //Send message to all remaining clients that client has disconnected
  SendMessage(NET_ADDRESS_ALL, mk_ClientLost, aHandle, '');

  //Assign a new host
  if fHostHandle = aHandle then
  begin
    if fClientList.Count = 0 then
      fHostHandle := NET_ADDRESS_EMPTY //Server is now empty so we don't need a new host
    else
    begin
      fHostHandle := fClientList[0].fHandle; //Assign hosting rights to the first client
      SendMessage(NET_ADDRESS_ALL, mk_ReassignHost, fHostHandle, ''); //Tell everyone about the new host
      Status('Reassigned hosting rights to '+inttostr(fHostHandle));
    end;
  end;
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
var PacketSender,PacketRecipient:integer; PacketLength:Cardinal; i:integer;
begin
  //Append new data to buffer
  SetLength(fBuffer, fBufferSize + aLength);
  Move(aData^, fBuffer[fBufferSize], aLength);
  fBufferSize := fBufferSize + aLength;

  //Try to read data packet from buffer
  while fBufferSize >= 12 do
  begin
    PacketSender := PInteger(@fBuffer[0])^;
    PacketRecipient := PInteger(@fBuffer[4])^;
    PacketLength := PCardinal(@fBuffer[8])^;
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
