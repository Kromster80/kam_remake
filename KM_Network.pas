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

//We can decide on something official later
const KAM_PORT1 = '56789'; //Used for computer to computer or by FIRST copy on a single computer
const KAM_PORT2 = '56790'; //Used for running mutliple copies on one computer (second copy)

type TRecieveKMPacketEvent = procedure (const aData: string) of object;

//Types of messages that can return errors
type TMessageCode = (mcNone, mcJoin {Ask host if we can join});

//Watchlist for timeout things
type
  TKMWatchlist = class
    private
      fCount:integer;
      fItems:array of record //0..n-1
        Code:TMessageCode;
        Tick:cardinal;
      end;
    public
      function Add(aCode:TMessageCode; aTick:cardinal):cardinal; //Add new item to watchlist
      procedure Mute(aItem:cardinal); //Mute the item
      function Check(aTick:cardinal):TMessageCode; //
  end;

type
  TKMNetwork = class
    private
      fSendPort, fRecievePort:string;
      fSocketRecieve: TWSocket;
      fSocketSend: TWSocket;
      fWatchlist:TKMWatchlist;
      procedure DataAvailable(Sender: TObject; Error: Word);
      procedure DataSent(Sender: TObject; Error: Word);
    public
      OnRecieveKMPacket: TRecieveKMPacketEvent; //This event will be run when we recieve a KaM packet. It is our output to the higher level
      constructor Create(MultipleCopies:boolean=false);
      destructor Destroy; override;
      function MyIPString:string;
      procedure SendTo(Addr:string; aData:string; aCode:TMessageCode=mcNone; aTimeOut:cardinal=0);
      procedure UpdateState;
  end;


implementation


function TKMWatchlist.Add(aCode:TMessageCode; aTick:cardinal):cardinal;
begin
  if fCount >= Length(fItems) then SetLength(fItems, fCount + 16);
  fItems[fCount].Code := aCode;
  fItems[fCount].Tick := aTick;
  Result := fCount;
  inc(fCount);
end;


procedure TKMWatchlist.Mute(aItem:cardinal); //Mute the item
begin
  fItems[aItem].Code := mcNone;
  fItems[aItem].Tick := 0;
end;


function TKMWatchlist.Check(aTick:cardinal):TMessageCode;
var i:integer;
begin
  for i:=0 to fCount-1 do
    if (fItems[i].Tick <> 0) and (aTick >= fItems[i].Tick) then
    begin
      Result := fItems[i].Code;
      Mute(i);
      exit;
    end;
  Result := mcNone;
end;


constructor TKMNetwork.Create(MultipleCopies:boolean=false);
var wsaData: TWSAData;
begin
  Inherited Create;

  if WSAStartup($101, wsaData) <> 0 then
  begin
    Assert(false, 'Error in Network');
    exit;
  end;

  fSendPort     := KAM_PORT1;
  fRecievePort  := KAM_PORT1;

  if MultipleCopies then fRecievePort := KAM_PORT2; //For tests on the same machine with 2 copies

  fSocketRecieve := TWSocket.Create(nil);
  fSocketRecieve.Proto  := 'udp';
  fSocketRecieve.Addr   := '0.0.0.0';
  fSocketRecieve.Port   := fRecievePort;
  fSocketRecieve.OnDataAvailable := DataAvailable;
  if not MultipleCopies then
    fSocketRecieve.Listen
  else
    try
      fSocketRecieve.Listen;
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
        
  fSocketSend := TWSocket.Create(nil);
  fSocketSend.Proto := 'udp';
  fSocketSend.Addr  := '0.0.0.0';
  fSocketSend.Port  := fSendPort;
  fSocketSend.LocalPort := '0'; //System assigns a port for sending automatically
  fSocketSend.OnDataSent := DataSent;

  fWatchlist := TKMWatchlist.Create;
end;


destructor TKMNetwork.Destroy;
begin
  fWatchlist.Free;
  fSocketRecieve.Free;
  fSocketSend.Free;
end;


function TKMNetwork.MyIPString:string;
begin
  if LocalIPList.Count >= 1 then
    Result := LocalIPList[0] //First address should be ours
  else
    Result := '';
end;


//Send to specified players (where would we store IPs and Player-IP bindings?)
//when trying to recover undelivered packets?
procedure TKMNetwork.SendTo(Addr:string; aData:string; aCode:TMessageCode=mcNone; aTimeOut:cardinal=0);
begin
  Assert(fSocketSend.AllSent);
  fSocketSend.Proto := 'udp';
  fSocketSend.Port := fSendPort;
  fSocketSend.Addr := Addr;
  fSocketSend.Connect; //UDP is connectionless. Connect will just open the socket
  fSocketSend.SendStr(aData);
  //fSocketSend.Send(@aData, length(aData));

  if aTimeOut <> 0 then
    fWatchlist.Add(aCode, GetTickCount + aTimeOut); //Add command to watchlist
end;


//Recieve from anyone
procedure TKMNetwork.DataAvailable(Sender: TObject; Error: Word);
var MyString: string;
    Buffer : array [0..1023] of char;
    Len, SrcLen    : Integer;
    Src    : TSockAddrIn;
begin
  //process
  //ReceiveFrom gives us three things:
  //  Buffer: The data
  //  Len: The length of the data
  //  Src: Who sent the data
  SrcLen := SizeOf(Src);
  Len := fSocketRecieve.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);

  //handle low level errors and convert them to higher ones if required
  //Mute corresponding fWatchlist.Item(aIndex) if required

  //pass message to GIP_Multi (it will be handling the higher level errors)
  MyString := copy(Buffer,0,Len);
  if Assigned(OnRecieveKMPacket) then
    OnRecieveKMPacket(MyString);
end;


procedure TKMNetwork.DataSent(Sender: TObject; Error: Word);
begin
  fSocketSend.Close; //Once the data is sent, close the socket so it is ready to send new data
end;


procedure TKMNetwork.UpdateState;
var M:TMessageCode;
begin
  M := fWatchlist.Check(GetTickCount);
  if M <> mcNone then
  begin
    if Assigned(OnRecieveKMPacket) then
      OnRecieveKMPacket('error');
  end;
end;


end.
