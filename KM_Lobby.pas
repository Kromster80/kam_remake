unit KM_Lobby;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Controls, KM_Defaults, KromUtils, SysUtils, Math,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  THTTPPostThread = class(TThread)
    private
      fHTTP:TIdHTTP;
      fPost:string;
      fParams:TStringList;
      fCarryObject:TObject;
    public
      ResultMsg:string;
      constructor Create(aPost:string);
      procedure Execute; override;
    end;


type TLobbyMsg = (lmNoReply, lmUserUsed, lmCanJoin);

{Stay in place for set time}
type
  TKMLobby = class
    private
      fServerAddress:string;
    public
      constructor Create(aAddress:string);
      destructor Destroy; override;
      procedure GetIPAsync(aLabel:TKMLabel);
      procedure GetIPAsyncDone(Sender:TObject);
      procedure AskServerForUsername(aLogin, aPass:string);
      procedure AskServerForUsernameDone(Sender:TObject);
      procedure UpdateState();
    end;


implementation


constructor THTTPPostThread.Create(aPost:string);
begin
  fHTTP := TIdHTTP.Create(nil);
  fPost := aPost;
  fParams := TStringList.Create; //Empty for now
  Inherited Create(false);
end;


procedure THTTPPostThread.Execute;
begin
  ResultMsg := fHTTP.Post(fPost, fParams);
  fParams.Free;
  fHTTP.Free;
end;


{ TKMLobby }
constructor TKMLobby.Create(aAddress:string);
begin
  Inherited Create;
  fServerAddress := aAddress;
end;


destructor TKMLobby.Destroy;
begin
  //Logout;
  //Disconnect;
  Inherited;
end;


procedure TKMLobby.GetIPAsync(aLabel:TKMLabel);
begin
  with THTTPPostThread.Create('http://www.whatismyip.com/automation/n09230945.asp') do
  begin
    fCarryObject := aLabel;
    OnTerminate := GetIPAsyncDone;
  end;
end;


procedure TKMLobby.GetIPAsyncDone(Sender:TObject);
begin
  TKMLabel(THTTPPostThread(Sender).fCarryObject).Caption := THTTPPostThread(Sender).ResultMsg;
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerForUsername(aLogin, aPass:string);
//var ParamList:TStringList;
begin
  //Fill parameters (login, pass)
  with THTTPPostThread.Create(fServerAddress+'AddUser.php') do
  begin
    //fCarryObject := aLabel;
    OnTerminate := AskServerForUsernameDone;
  end;
end;


procedure TKMLobby.AskServerForUsernameDone(Sender:TObject);
begin
  //TKMLabel(THTTPPostThread(Sender).fCarryObject).Caption := THTTPPostThread(Sender).ResultMsg;
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.UpdateState();
begin
  //
end;


end.
