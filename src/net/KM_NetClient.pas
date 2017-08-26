unit KM_NetClient;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_NetworkTypes
  {$IFDEF WDC} ,KM_NetClientOverbyte {$ENDIF}
  {$IFDEF FPC} ,KM_NetClientLNet {$ENDIF}
  {,KM_Log};

{ Contains basic items we need for smooth Net experience:

    - connect to server
    - signal if we have successfully connected to server
    - signal if we could not connect to server

    - disconnect from server (always successful)
    - signal if we were forcefully disconnected by server

    - send binary data to other server clients
    - recieve binary data from other server clients

    - optionaly report non-important status messages }

type
  {$IFDEF WDC}
    TKMNetClientImplementation = class(TKMNetClientOverbyte);
  {$ENDIF}
  {$IFDEF FPC}
    TKMNetClientImplementation = class(TKMNetClientLNet);
  {$ENDIF}

  TKMNetClient = class;
  TNotifySenderDataEvent = procedure (aNetClient: TKMNetClient; aSenderIndex: SmallInt; aData: Pointer; aLength: Cardinal) of object;

  TKMNetClient = class
  private
    fClient: TKMNetClientImplementation;
    fConnected: Boolean;

    fTotalSize: Cardinal;
    fBufferSize: Cardinal;
    fBuffer: array of Byte;

    fOnConnectSucceed: TNotifyEvent;
    fOnConnectFailed: TGetStrProc;
    fOnForcedDisconnect: TNotifyEvent;
    fOnRecieveData: TNotifySenderDataEvent;
    fOnStatusMessage: TGetStrProc;
    procedure Error(const S: string);
    procedure ConnectSucceed(Sender: TObject);
    procedure ConnectFailed(const S: string);
    procedure ForcedDisconnect(Sender: TObject);
    procedure RecieveData(aData: Pointer; aLength: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    property Connected: Boolean read fConnected;
    function MyIPString: string;
    function SendBufferEmpty: Boolean;

    procedure ConnectTo(const aAddress: string; const aPort: Word); //Try to connect to server
    property OnConnectSucceed: TNotifyEvent write fOnConnectSucceed; //Signal success
    property OnConnectFailed: TGetStrProc write fOnConnectFailed; //Signal fail and text description

    procedure Disconnect; //Disconnect from server
    property OnForcedDisconnect: TNotifyEvent write fOnForcedDisconnect; //Signal we were forcelly disconnected

    property OnRecieveData: TNotifySenderDataEvent write fOnRecieveData;
    procedure SendData(aSender, aRecepient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
    procedure UpdateStateIdle;

    property OnStatusMessage: TGetStrProc write fOnStatusMessage;
  end;


implementation


 { TKMNetClient }
constructor TKMNetClient.Create;
begin
  inherited;

  fClient := TKMNetClientImplementation.Create;
  fConnected := False;
  SetLength(fBuffer, 0);
  fBufferSize := 0;
end;


destructor TKMNetClient.Destroy;
begin
  fClient.Free;
  inherited;
end;


function TKMNetClient.MyIPString:string;
begin
  Result := fClient.MyIPString;
end;


function TKMNetClient.SendBufferEmpty: Boolean;
begin
  Result := fClient.SendBufferEmpty;
end;


procedure TKMNetClient.Error(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Error ' + S);
end;


procedure TKMNetClient.ConnectTo(const aAddress: string; const aPort: Word);
begin
  SetLength(fBuffer, 0);
  fBufferSize := 0;
  fClient.OnError := Error;
  fClient.OnConnectSucceed := ConnectSucceed;
  fClient.OnConnectFailed := ConnectFailed;
  fClient.OnSessionDisconnected := ForcedDisconnect;
  fClient.OnRecieveData := RecieveData;
  fClient.ConnectTo(aAddress, aPort);
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connecting..');
end;


procedure TKMNetClient.ConnectSucceed(Sender: TObject);
begin
  fConnected := true;
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connected');
  if Assigned(fOnConnectSucceed) then fOnConnectSucceed(Self);
end;


procedure TKMNetClient.ConnectFailed(const S: string);
begin
  fConnected := false;
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connection failed. '+S);
  if Assigned(fOnConnectFailed) then fOnConnectFailed(S);
end;


procedure TKMNetClient.Disconnect;
begin
  fOnConnectSucceed := nil;
  fOnConnectFailed := nil;
  fOnForcedDisconnect := nil;
  fOnRecieveData := nil;
  fOnStatusMessage := nil;

  SetLength(fBuffer,0);
  fBufferSize := 0;

  fConnected := false;
  fClient.Disconnect;
end;


//Happens in following cases:
//  - when we deliberately disconnect
//  - when connection failed
//  - when server disconnects us
procedure TKMNetClient.ForcedDisconnect(Sender: TObject);
begin
  if fConnected then
  begin
    if Assigned(fOnStatusMessage) then
      fOnStatusMessage('Client: Forced disconnect');
    if Assigned(fOnForcedDisconnect) then
      fOnForcedDisconnect(Self);
  end;
  fConnected := false;
end;


//Assemble the packet as [Sender.Recepient.Length.Data]
//We can pack/clean the header later on (if we hit bandwidth limits)
procedure TKMNetClient.SendData(aSender,aRecepient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
var
  P: Pointer;
begin
  assert(aLength <= MAX_PACKET_SIZE,'Packet over size limit');
  GetMem(P, aLength+6);
  PKMNetHandleIndex(P)^ := aSender;
  PKMNetHandleIndex(cardinal(P)+2)^ := aRecepient;
  PWord(cardinal(P)+4)^ := aLength;
  Move(aData^, Pointer(cardinal(P)+6)^, aLength);
  fClient.SendData(P, aLength+6);
  FreeMem(P);
end;


//Split recieved data into single packets
procedure TKMNetClient.RecieveData(aData: Pointer; aLength: Cardinal);
var
  PacketSender: TKMNetHandleIndex;
  PacketLength: Word;
//  HeaderSize: Byte;
begin
  //Append new data to buffer
  SetLength(fBuffer, fBufferSize + aLength);
  Move(aData^, fBuffer[fBufferSize], aLength);
  fBufferSize := fBufferSize + aLength;
  //gLog.AddTime('############### recieve data: Length = ' + IntToStr(aLength) + ' fBufferSize = ' + IntTOStr(fBufferSize));

  //HeaderSize := 2*SizeOf(TKMNetHandleIndex) +

  while fBufferSize >= 1 do
  begin
    //Try to read data packet from buffer
//    PacksCnt := PCardinal(@fBuffer[0])^;
    //gLog.AddTime('%%%%% receive cumulative packet: packs Cnt = ' + IntToStr(PacksCnt));
    while (fBufferSize >= 7) and (PByte(@fBuffer[0])^ > 0) do
    begin
      PacketSender := PKMNetHandleIndex(@fBuffer[1])^;
      //We skip PacketRecipient because thats us
      PacketLength := PWord(@fBuffer[5])^;

      //gLog.AddTime(Format('pack %d: sender = %s length = %d' , [PacksCnt - PCardinal(@fBuffer[0])^ + 1,
//                                                    GetNetAddressStr(PacketSender), PacketLength]));
      //Buffer is lengthy enough to contain full packet, process it
      if PacketLength <= fBufferSize-7 then
      begin
        Inc(fTotalSize, PacketLength);
        //Skip packet header
        fOnRecieveData(Self, PacketSender, @fBuffer[7], PacketLength);

        //Check if Network was stopped by processing above packet (e.g. version mismatch)
        if not Assigned(fOnRecieveData) then
          Exit;

        //Trim received packet from buffer
        if PacketLength < fBufferSize-7 then //Check range
          Move(fBuffer[7+PacketLength], fBuffer[1], fBufferSize-PacketLength-7);
        fBufferSize := fBufferSize - PacketLength - 6;
        PByte(@fBuffer[0])^ := PByte(@fBuffer[0])^ - 1;
      end else
      begin
        //gLog.AddTime(Format('---xxx break: PacketLength = %d < %d = BufferSize-16', [PacketLength, fBufferSize-16]));
        Break;
      end;
    end;
    if PByte(@fBuffer[0])^ = 0 then
    begin
      Move(fBuffer[1], fBuffer[0], fBufferSize - 1);
      fBufferSize := fBufferSize - 1;
      //gLog.AddTime('---%%%% End of cumulative packet. TotalSize = ' + IntToStr(fTotalSize));
      fTotalSize := 0;
    end else
      Exit;
  end;

//  //Try to read data packet from buffer
//  while fBufferSize >= 12 do
//  begin
//    PacketSender := PInteger(@fBuffer[0])^;
//    //We skip PacketRecipient because thats us
//    PacketLength := PCardinal(@fBuffer[8])^;
//
//    //Buffer is lengthy enough to contain full packet, process it
//    if PacketLength <= fBufferSize-12 then
//    begin
//      //Skip packet header
//      fOnRecieveData(Self, PacketSender, @fBuffer[12], PacketLength);
//
//      //Check if Network was stopped by processing above packet (e.g. version mismatch)
//      if not Assigned(fOnRecieveData) then
//        Exit;
//
//      //Trim received packet from buffer
//      if 12+PacketLength < fBufferSize then //Check range
//        Move(fBuffer[12+PacketLength], fBuffer[0], fBufferSize-PacketLength-12);
//      fBufferSize := fBufferSize - PacketLength - 12;
//    end else
//      Exit;
//  end;
end;


procedure TKMNetClient.UpdateStateIdle;
begin
  {$IFDEF FPC} fClient.UpdateStateIdle; {$ENDIF}
end;


end.
