unit KM_Network;
{$I KaM_Remake.inc}
interface
uses Windows, Classes, SysUtils, WinSock, WSocket;

{
Features to be implemented:
 - Input/output as TByteArrays or something, not strings (that was temporary for testing)
 - Input buffer so we can send multiple packets in quick succession without having to wait for the socket
   to close in DataSent. They should be buffered until the socket is ready, then sent when DataSent occurs
 - Proper error/exception handling for all cases
 - Resend packets that are not recieved (possibly handled at a higher level)
 - CRC checks on each packet
 - Support of unlimited size input (split across multiple packets when it is too large)
}

const MAX_BUFFER = 32; //How many messages to store while UDP is busy

//We can decide on something official later
const KAM_PORT1 = '56789'; //Used for computer to computer or by FIRST copy on a single computer
const KAM_PORT2 = '56790'; //Used for running mutliple copies on one computer (second copy)

type TRecieveKMPacketEvent = procedure (const aData: array of byte; aAddr:string) of object;

type
  TKMNetwork = class
    private
      fSendPort, fRecievePort:string;
      fSocketRecieve: TWSocket;
      fSocketSend: TWSocket;
      fMultipleCopies, fSendSocketUsed: boolean;
      fOnRecieveKMPacket: TRecieveKMPacketEvent; //This event will be run when we recieve a KaM packet. It is our output to the higher level

      fBufAddr: array[0..MAX_BUFFER-1] of string; //-1 so we could use "mod MAX_BUFFER"
      fBufData: array[0..MAX_BUFFER-1] of string;
      fBufReadPos, fBufWritePos: integer;
      procedure BufferAdd(const aAddr:string; const aData: string);
      procedure BufferSendOldest;

      procedure DataAvailable(Sender: TObject; Error: Word);
      procedure DataSent(Sender: TObject; Error: Word);
    public
      fListening: boolean;
      constructor Create(aMultipleCopies:boolean=false);
      destructor Destroy; override;
      function MyIPString:string;
      function MyIPStringAndPort:string;
      procedure StartListening;
      procedure StopListening;
      property OnRecieveKMPacket:TRecieveKMPacketEvent write fOnRecieveKMPacket;
      procedure SendTo(Addr:string; aData:string);
      procedure UpdateState;
  end;


implementation


constructor TKMNetwork.Create(aMultipleCopies:boolean=false);
var wsaData: TWSAData;
begin
  Inherited Create;
  fMultipleCopies := aMultipleCopies;
  fListening := false;
  fSendSocketUsed := false;
  fBufReadPos := 0; //Start writing from first place
  fBufWritePos := 0;

  if WSAStartup($101, wsaData) <> 0 then
  begin
    Assert(false, 'Error in Network');
    exit;
  end;

  fSendPort     := KAM_PORT1;
  fRecievePort  := KAM_PORT1;

  if fMultipleCopies then fRecievePort := KAM_PORT2; //For tests on the same machine with 2 copies

  fSocketRecieve := TWSocket.Create(nil);
  fSocketRecieve.Proto  := 'udp';
  fSocketRecieve.Addr   := '0.0.0.0';
  fSocketRecieve.Port   := fRecievePort;
  fSocketRecieve.OnDataAvailable := DataAvailable;

  fSocketSend := TWSocket.Create(nil);
  fSocketSend.Proto := 'udp';
  fSocketSend.Addr  := '0.0.0.0';
  fSocketSend.Port  := fSendPort;
  fSocketSend.LocalPort := '0'; //System assigns a port for sending automatically
  fSocketSend.OnDataSent := DataSent;
end;


destructor TKMNetwork.Destroy;
begin
  fSocketRecieve.Free;
  fSocketSend.Free;
  Inherited;
end;


procedure TKMNetwork.StartListening;
begin
  if not fMultipleCopies then
    fSocketRecieve.Listen
  else
    try
      fSendPort     := KAM_PORT1;
      fRecievePort  := KAM_PORT1;

      if fMultipleCopies then fRecievePort := KAM_PORT2; //For tests on the same machine with 2 copies
      fSocketRecieve.Proto  := 'udp';
      fSocketRecieve.Addr   := '0.0.0.0';
      fSocketRecieve.Port   := fRecievePort;
      fSocketRecieve.OnDataAvailable := DataAvailable;
      fSocketRecieve.Listen;
      fListening := true;
    except
      on E : ESocketException do
      begin
        //Assume this means the port is already taken, so we are being the second copy
        fSendPort     := KAM_PORT2; //Swap ports
        fRecievePort  := KAM_PORT1;
        //Try again
        fSocketRecieve.Proto  := 'udp';
        fSocketRecieve.Addr   := '0.0.0.0';
        fSocketRecieve.Port   := fRecievePort;
        fSocketRecieve.OnDataAvailable := DataAvailable;
        try
          fSocketRecieve.Listen;
        except
          on E : ESocketException do //todo: add error handling here
        end;
      end;
    end;
end;


procedure TKMNetwork.StopListening;
begin
  fSocketRecieve.Close;
  fListening := false;
end;


function TKMNetwork.MyIPString:string;
begin
  if LocalIPList.Count >= 1 then
    Result := LocalIPList[0] //First address should be ours
  else
    Result := '';
end;


function TKMNetwork.MyIPStringAndPort:string;
begin
  if LocalIPList.Count >= 1 then
    Result := LocalIPList[0] + ' Ports: ' + fSendPort + '/' + fRecievePort //First address should be ours
  else
    Result := '';
end;


//Send to specified players (where would we store IPs and Player-IP bindings?)
//when trying to recover undelivered packets?
procedure TKMNetwork.SendTo(Addr:string; aData:string);
begin
  //Handle the case when DataSent not yet occured
  if fSendSocketUsed then
  begin
    BufferAdd(Addr, aData);
    exit;
  end;

  fSocketSend.Proto := 'udp';
  fSocketSend.Port := fSendPort;
  fSocketSend.Addr := Addr;
  fSocketSend.Connect; //UDP is connectionless. Connect will just open the socket
  fSocketSend.SendStr(aData);
  //fSocketSend.Send(@aData, length(aData));
  fSendSocketUsed := true;
end;


//Recieve from anyone
procedure TKMNetwork.DataAvailable(Sender: TObject; Error: Word);
var Buffer : array[0..1023] of byte;
    Len, SrcLen    : Integer;
    Src    : TSockAddrIn;
    Addr:string;
begin
  //process
  //ReceiveFrom gives us three things:
  //  Buffer: The data
  //  Len: The length of the data
  //  Src: Who sent the data
  SrcLen := SizeOf(Src);
  Len := fSocketRecieve.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
  if Len = -1 then
  begin
    //This means there is an error
    //Assert(false, 'KaM recieve error: '+inttostr(Error)+' last error: '+inttostr(fSocketRecieve.LastError));
    exit;
  end;

  //XXX.XXX.XXX.XXX
  Addr := IntToStr(Ord(Src.sin_addr.S_un_b.s_b1)) +'.'+
          IntToStr(Ord(Src.sin_addr.S_un_b.s_b2)) +'.'+
          IntToStr(Ord(Src.sin_addr.S_un_b.s_b3)) +'.'+
          IntToStr(Ord(Src.sin_addr.S_un_b.s_b4));

  //handle low level errors and convert them to higher ones if required
  //Mute corresponding fWatchlist.Item(aIndex) if required

  //pass message to GIP_Multi (it will be handling the higher level errors)
  if Assigned(fOnRecieveKMPacket) then
    fOnRecieveKMPacket(Slice(Buffer, Len), Addr);
end;


procedure TKMNetwork.DataSent(Sender: TObject; Error: Word);
begin
  fSocketSend.Close; //Once the data is sent, close the socket so it is ready to send new data
  fSendSocketUsed := false;
  BufferSendOldest;
end;


procedure TKMNetwork.UpdateState;
begin
  //
end;


procedure TKMNetwork.BufferAdd(const aAddr:string; const aData: string);
begin
  Assert(fBufAddr[fBufWritePos]='', 'Network buffer overrun');
  fBufAddr[fBufWritePos] := aAddr;
  fBufData[fBufWritePos] := aData;
  fBufWritePos := (fBufWritePos + 1) mod MAX_BUFFER; //Advance to next entry
end;


procedure TKMNetwork.BufferSendOldest;
begin
  if fBufAddr[fBufReadPos] = '' then exit; //Otherwise there's data we need to process
  SendTo(fBufAddr[fBufReadPos], fBufData[fBufReadPos]);
  fBufAddr[fBufReadPos] := '';
  fBufData[fBufReadPos] := '';
  fBufReadPos := (fBufReadPos + 1) mod MAX_BUFFER; //Advance to next entry
end;

end.
