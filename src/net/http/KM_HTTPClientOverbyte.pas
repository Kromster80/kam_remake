unit KM_HTTPClientOverbyte;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, OverbyteIcsHttpProt;


type
  TKMHTTPClientOverbyte = class
  private
    fHTTPClient: THTTPCli;
    fOnError: TGetStrProc;
    fOnGetCompleted: TGetStrProc;
    fIsUTF8: Boolean;
    procedure RequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetURL(const aURL: string; aIsUTF8: Boolean);

    property OnError: TGetStrProc write fOnError;
    property OnGetCompleted: TGetStrProc write fOnGetCompleted;
  end;


implementation


constructor TKMHTTPClientOverbyte.Create;
begin
  inherited Create;
  fHTTPClient := THTTPCli.Create(nil);
  fHTTPClient.OnRequestDone := RequestDone;
  fHTTPClient.RcvdStream := TMemoryStream.Create;
end;


destructor TKMHTTPClientOverbyte.Destroy;
begin
  fHTTPClient.RcvdStream.Free; //RcvdStream is created and managed by us
  fHTTPClient.Free;
  inherited;
end;


procedure TKMHTTPClientOverbyte.GetURL(const aURL: string; aIsUTF8: Boolean);
begin
  fIsUTF8 := aIsUTF8;
  fHTTPClient.Abort; //If we were doing something, stop it
  TMemoryStream(fHTTPClient.RcvdStream).Clear;
  fHTTPClient.URL := aURL;
  fHTTPClient.GetASync; //Non-blocking
end;


procedure TKMHTTPClientOverbyte.RequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
  RcvdText: AnsiString;
  ReturnText: UnicodeString;
begin
  if RqType <> httpGET then exit;
  if ErrCode <> 0 then
  begin
    if Assigned(fOnError) then
      fOnError(IntToStr(fHTTPClient.StatusCode) + ' ' + fHTTPClient.ReasonPhrase + ' (#' + IntToStr(ErrCode) + ')');
    exit;
  end;

  RcvdText := '';
  if fHTTPClient.RcvdStream.Size > 0 then
  begin
    SetLength(RcvdText, fHTTPClient.RcvdStream.Size);
    Move(TMemoryStream(fHTTPClient.RcvdStream).Memory^, RcvdText[1], fHTTPClient.RcvdStream.Size);
    TMemoryStream(fHTTPClient.RcvdStream).Clear;
  end;

  if fIsUTF8 then
    ReturnText := UTF8ToUnicodeString(RcvdText)
  else
    ReturnText := UnicodeString(RcvdText);

  if Assigned(fOnGetCompleted) then
    fOnGetCompleted(ReturnText);
end;


end.
 