unit KM_HTTPClientLNet;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, lNet, URLUtils, lHTTP;

type
  TKMHTTPClientLNet = class
  private
    fHTTPClient:TLHTTPClient;
    HTTPBuffer: string;
    fOnError:TGetStrProc;
    fOnGetCompleted:TGetStrProc;
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure HTTPClientError(const msg: string; aSocket: TLSocket);
    function HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetURL(aURL:string);
    procedure UpdateStateIdle;

    property OnError:TGetStrProc write fOnError;
    property OnGetCompleted:TGetStrProc write fOnGetCompleted;
  end;

implementation


constructor TKMHTTPClientLNet.Create;
begin
  Inherited Create;
  fHTTPClient := TLHTTPClient.Create(nil);
  fHTTPClient.Timeout := 0;
  fHTTPClient.OnInput := HTTPClientInput;
  fHTTPClient.OnError := HTTPClientError;
  fHTTPClient.OnDoneInput := HTTPClientDoneInput;
end;


destructor TKMHTTPClientLNet.Destroy;
begin
  fHTTPClient.Free;
  Inherited;
end;


procedure TKMHTTPClientLNet.GetURL(aURL:string);
var
  Proto, User, Pass, Host, Port, Path : String;
begin
  fHTTPClient.Disconnect(true); //If we were doing something, stop it
  HTTPBuffer := '';
  ParseURL(aURL, Proto, User, Pass, Host, Port, Path);
  fHTTPClient.Host := Host;
  fHTTPClient.URI := Path;
  fHTTPClient.Port := StrToIntDef(Port,80);
  fHTTPClient.SendRequest;
end;


procedure TKMHTTPClientLNet.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect;
  if Assigned(fOnGetCompleted) then
    fOnGetCompleted(HTTPBuffer);
  HTTPBuffer := '';
end;


procedure TKMHTTPClientLNet.HTTPClientError(const msg: string; aSocket: TLSocket);
begin
  if Assigned(fOnError) then
    fOnError(msg);
end;


function TKMHTTPClientLNet.HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer;
var
  oldLength: dword;
begin
  if ASize > 0 then
  begin
    oldLength := Length(HTTPBuffer);
    setlength(HTTPBuffer,oldLength + ASize);
    move(ABuffer^,HTTPBuffer[oldLength + 1], ASize);
  end;
  Result := aSize; // tell the http buffer we read it all
end;


procedure TKMHTTPClientLNet.UpdateStateIdle;
begin
  fHTTPClient.CallAction; //Process network events
end;

end.
 
