unit KM_HTTPClientLNet;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, lNet, URLUtils, lHTTP;

type
  TKMHTTPClientLNet = class
  private
    fHTTPClient: TLHTTPClient;
    HTTPBuffer: AnsiString;
    fOnError: TGetStrProc;
    fOnGetCompleted: TGetStrProc;
    fIsUTF8: Boolean;
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure HTTPClientError(const msg: string; aSocket: TLSocket);
    function HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: PChar; ASize: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetURL(const aURL: string; aIsUTF8: Boolean);
    procedure UpdateStateIdle;

    property OnError: TGetStrProc write fOnError;
    property OnGetCompleted: TGetStrProc write fOnGetCompleted;
  end;

implementation


constructor TKMHTTPClientLNet.Create;
begin
  inherited Create;
  fHTTPClient := TLHTTPClient.Create(nil);
  fHTTPClient.Timeout := 0;
  fHTTPClient.OnInput := HTTPClientInput;
  fHTTPClient.OnError := HTTPClientError;
  fHTTPClient.OnDoneInput := HTTPClientDoneInput;
end;


destructor TKMHTTPClientLNet.Destroy;
begin
  fHTTPClient.Free;
  inherited;
end;


procedure TKMHTTPClientLNet.GetURL(const aURL: string; aIsUTF8: Boolean);
var
  Proto, User, Pass, Host, Port, Path: string;
begin
  fIsUTF8 := aIsUTF8;
  fHTTPClient.Disconnect(true); //If we were doing something, stop it
  HTTPBuffer := '';
  ParseURL(aURL, Proto, User, Pass, Host, Port, Path);
  fHTTPClient.Host := Host;
  fHTTPClient.URI := Path;
  fHTTPClient.Port := StrToIntDef(Port, 80);
  fHTTPClient.SendRequest;
end;


procedure TKMHTTPClientLNet.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
var ReturnString: UnicodeString;
begin
  aSocket.Disconnect;
  if fIsUTF8 then
    ReturnString := UTF8Decode(HTTPBuffer)
  else
    ReturnString := UnicodeString(HTTPBuffer);
  if Assigned(fOnGetCompleted) then
    fOnGetCompleted(ReturnString);
  HTTPBuffer := '';
end;


procedure TKMHTTPClientLNet.HTTPClientError(const msg: string; aSocket: TLSocket);
begin
  if Assigned(fOnError) then
    fOnError(msg);
end;


function TKMHTTPClientLNet.HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: PChar; ASize: Integer): Integer;
var
  oldLength: dword;
begin
  if ASize > 0 then
  begin
    oldLength := Length(HTTPBuffer);
    setlength(HTTPBuffer, oldLength + ASize);
    move(ABuffer^, HTTPBuffer[oldLength + 1], ASize);
  end;
  Result := aSize; // tell the http buffer we read it all
end;


procedure TKMHTTPClientLNet.UpdateStateIdle;
begin
  fHTTPClient.CallAction; //Process network events
end;


end.
 
