unit KM_NetUDPOverbyte;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, OverbyteIcsWSocket, OverbyteIcsWinSock;


type
  TNotifyAddressDataEvent = procedure(const aAddress: string; aData:pointer; aLength:cardinal)of object;

  TKMNetUDPOverbyte = class
  private
    fSocketSend: TWSocket;
    fSocketReceive: TWSocket;

    fOnError: TGetStrProc;
    fOnRecieveData: TNotifyAddressDataEvent;
    procedure DataAvailable(Sender: TObject; Error: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendPacket(const aAddress:string; const aPort: Word; aData:pointer; aLength:cardinal);
    procedure Listen(const aPort: Word);
    procedure StopListening;
    property OnError:TGetStrProc write fOnError;
    property OnRecieveData:TNotifyAddressDataEvent write fOnRecieveData;
  end;


implementation


constructor TKMNetUDPOverbyte.Create;
begin
  Inherited Create;
  fSocketSend := TWSocket.Create(nil);
  fSocketReceive := TWSocket.Create(nil);
end;


destructor TKMNetUDPOverbyte.Destroy;
begin
  fSocketSend.Free;
  fSocketReceive.Free;
  Inherited;
end;


procedure TKMNetUDPOverbyte.Listen(const aPort: Word);
begin
  fSocketReceive.OnDataAvailable := DataAvailable;
  try
    fSocketReceive.Proto     := 'udp';
    fSocketReceive.Addr      := '0.0.0.0';
    fSocketReceive.Port      := IntToStr(aPort);
    fSocketReceive.Listen;
  except
    on E : Exception do
    begin
      //Trap the exception and tell the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      fOnError(E.Message);
    end;
  end;
end;


procedure TKMNetUDPOverbyte.StopListening;
begin
  fSocketReceive.Close;
  fSocketReceive.OnDataAvailable := nil;
end;


procedure TKMNetUDPOverbyte.SendPacket(const aAddress:string; const aPort: Word; aData:pointer; aLength:cardinal);
begin
  fSocketSend.Proto     := 'udp';
  fSocketSend.Addr      := aAddress;
  fSocketSend.Port      := IntToStr(aPort);
  fSocketSend.LocalPort := '0';
  fSocketSend.Connect;
  fSocketSend.Send(aData, aLength);
  fSocketSend.Close;
end;


procedure TKMNetUDPOverbyte.DataAvailable(Sender: TObject; Error: Word);
const
  BufferSize = 10240; //10kb
var
  P:pointer;
  L:integer; //L could be -1 when no data is available
  Src: TSockAddr;
  SrcLen : Integer;
begin
  SrcLen := SizeOf(Src);
  if Error <> 0 then
  begin
    fOnError('UDP DataAvailable. Error '+WSocketErrorDesc(Error)+' (#' + IntToStr(Error)+')');
    exit;
  end;

  GetMem(P, BufferSize+1); //+1 to avoid RangeCheckError when L = BufferSize
  L := TWSocket(Sender).ReceiveFrom(P, BufferSize, Src, SrcLen);

  if L > 0 then
    fOnRecieveData(UnicodeString(WSocket_inet_ntoa(Src.sin_addr)), P, L);

  FreeMem(P);
end;

end.
