unit KM_Network;
{$I KaM_Remake.inc}
interface
uses Classes, WinSock, WSocket;

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

type
  TRecieveKMPacketEvent = procedure (const aData: string) of object;

type
  TKMNetwork = class
    private
      fSendPort, fRecievePort:string;
      fSocketRecieve: TWSocket;
      fSocketSend: TWSocket;
      procedure DataAvailable(Sender: TObject; Error: Word);
      procedure DataSent(Sender: TObject; Error: Word);
    public
      OnRecieveKMPacket: TRecieveKMPacketEvent; //This event will be run when we recieve a KaM packet. It is our output to the higher level
      constructor Create(MultipleCopies:boolean=false);
      destructor Destroy; override;
      procedure SendTo(Addr:string; aData:string);
  end;


implementation


constructor TKMNetwork.Create(MultipleCopies:boolean=false);
begin
  Inherited Create;

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
end;


destructor TKMNetwork.Destroy;
begin
  fSocketRecieve.Free;
  fSocketSend.Free;
end;


//Send to specified players (where would we store IPs and Player-IP bindings?)
//when trying to recover undelivered packets?
procedure TKMNetwork.SendTo(Addr:string; aData:string);
begin
  Assert(fSocketSend.AllSent);
  fSocketSend.Proto := 'udp';
  fSocketSend.Port := fSendPort;
  fSocketSend.Addr := Addr;
  fSocketSend.Connect; //UDP is connectionless. Connect will just open the socket
  fSocketSend.SendStr(aData);
  //fSocketSend.Send(@aData, length(aData));
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

  //pass message to GIP_Multi (it will be handling the higher level errors)
  MyString := copy(Buffer,0,Len);
  if Assigned(OnRecieveKMPacket) then
    OnRecieveKMPacket(MyString);
end;


procedure TKMNetwork.DataSent(Sender: TObject; Error: Word);
begin
  fSocketSend.Close; //Once the data is sent, close the socket so it is ready to send new data
end;


end.
