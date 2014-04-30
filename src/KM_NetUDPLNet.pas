unit KM_NetUDPLNet;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils, LNet;


type
  TNotifyAddressDataEvent = procedure(aAddress: string; aData:pointer; aLength:cardinal)of object;

  TKMNetUDPLNet = class
  private
    fUDP:TLUdp;

    fOnError: TGetStrProc;
    fOnRecieveData: TNotifyAddressDataEvent;
    procedure Receive(aSocket: TLSocket);
    procedure Error(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendPacket(const aAddress:string; const aPort:string; aData:pointer; aLength:cardinal);
    procedure Listen(const aPort: string);
    procedure StopListening;
    procedure UpdateStateIdle;
    property OnError:TGetStrProc write fOnError;
    property OnRecieveData:TNotifyAddressDataEvent write fOnRecieveData;
  end;


implementation


constructor TKMNetUDPLNet.Create;
begin
  Inherited Create;
  fUDP := TLUdp.Create(nil);
  fUDP.OnError := Error;
  fUDP.Timeout := 1;
end;


destructor TKMNetUDPLNet.Destroy;
begin
  if fUDP<>nil then fUDP.Free;
  Inherited;
end;


procedure TKMNetUDPLNet.Listen(const aPort:string);
begin
  fUDP.OnReceive := Receive;
  fUDP.Listen(StrToInt(aPort));
  fUDP.CallAction;
end;


procedure TKMNetUDPLNet.StopListening;
begin
  fUDP.Disconnect;
  fUDP.OnReceive := nil;
end;


procedure TKMNetUDPLNet.SendPacket(const aAddress:string; const aPort:string; aData:pointer; aLength:cardinal);
begin
  fUDP.Send(aData^, aLength, aAddress+':'+aPort);
end;


procedure TKMNetUDPLNet.Receive(aSocket: TLSocket);
const
  BufferSize = 10240; //10kb
var
  P:pointer; s:string;
  L:integer; //L could be -1 when no data is available
begin
  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := fUDP.Get(P^, BufferSize, aSocket);

  if L > 0 then //if L=0 then exit;
    fOnRecieveData(aSocket.PeerAddress, P, L);

  FreeMem(P);
end;


procedure TKMNetUDPLNet.Error(const msg: string; aSocket: TLSocket);
begin
  fOnError('LNet UDP Error: '+msg);
end;


procedure TKMNetUDPLNet.UpdateStateIdle;
begin
  if fUDP <> nil then fUDP.CallAction; //Process network events
end;

end.
