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
      procedure Recieve(aData:string);
    public
      OnRecieveKMPacket: TRecieveKMPacketEvent; //This event will be run when we recieve a KaM packet. It is our output to the higher level
      constructor Create;
      destructor Destroy; override;
      procedure Send(aData:string); //This is the input from the higher level, called to send data (to all?)
      procedure SendTo(aData:string);
  end;


implementation


constructor TKMNetwork.Create;
begin
  Inherited Create;
end;


destructor TKMNetwork.Destroy;
begin

end;


//Broadcast to all players
procedure TKMNetwork.Send(aData:string);
begin

end;


//Send to specified players (where would we store IPs and Player-IP bindings?)
//when trying to recover undelivered packets?
procedure TKMNetwork.SendTo(aData:string);
begin

end;


//Recieve from anyone
procedure TKMNetwork.Recieve(aData:string);
begin
  //process

  //handle low level errors and convert them to higher ones if required

  //pass message to GIP_Multi (it will be handling the higher level errors)

end;


end.
