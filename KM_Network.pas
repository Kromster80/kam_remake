unit KM_Network;
{$I KaM_Remake.inc}
interface
uses Classes, Sockets, KM_Unit1;

const KAM_PORT = '56789'; //We can decide on something official later

type
  //Maybe we can use TKMPacket later, for now it's a string
  TRecieveKMPacketEvent = procedure (const KMPacket:string) of object;

type
  TKMNetwork = class
    private
    public
      OnRecieveKMPacket: TRecieveKMPacketEvent; //This event will be run when we recieve a KaM packet. It is our output to the higher level
      constructor Create;
      destructor Destroy; override;
      procedure Send(aData:string); //This is the input from the higher level, called to send data (to all?)
  end;


implementation


constructor TKMNetwork.Create;
begin
  Inherited Create;
end;


destructor TKMNetwork.Destroy;
begin

end;


procedure TKMNetwork.Send(aData:string);
begin

end;


end.
