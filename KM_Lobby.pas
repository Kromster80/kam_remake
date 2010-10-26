unit KM_Lobby;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Controls, KM_Defaults, KM_CommonTypes, KromUtils, SysUtils, Math,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  THTTPPostThread = class(TThread)
    private
      fHTTP:TIdHTTP;
      fPost:string;
      fParams:TStringList;
      fCarryObject:TNotifyString;
    public
      ResultMsg:string;
      constructor Create(aPost:string; aParams:string);
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
      procedure GetIPAsync(aLabel:TNotifyString);
      procedure GetIPAsyncDone(Sender:TObject);
      procedure AskServerForUsername(aLogin, aPass, aIP:string; aLabel:TNotifyString);
      procedure AskServerForUsernameDone(Sender:TObject);
      procedure UpdateState();
    end;


implementation


constructor THTTPPostThread.Create(aPost:string; aParams:string);
begin
  fHTTP := TIdHTTP.Create(nil);
  fPost := aPost;
  fParams := TStringList.Create; //Empty for now
  fParams.Text := aParams;
  Inherited Create(false);
end;


{ For some reason returned string begins with Unicode Byte-Order Mark (BOM) $EFBBBF
  I googled for it and all solutions suggest to simply ignore/remove it }
procedure THTTPPostThread.Execute;
begin
  ResultMsg := fHTTP.Post(fPost, fParams);
  ResultMsg := StringReplace(ResultMsg,#$EF+#$BB+#$BF,'',[rfReplaceAll]); //Remove BOM
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


procedure TKMLobby.GetIPAsync(aLabel:TNotifyString);
begin
  with THTTPPostThread.Create('http://www.whatismyip.com/automation/n09230945.asp','') do
  begin
    fCarryObject := aLabel;
    OnTerminate := GetIPAsyncDone;
  end;
end;


procedure TKMLobby.GetIPAsyncDone(Sender:TObject);
begin
  THTTPPostThread(Sender).fCarryObject(Self, THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerForUsername(aLogin, aPass, aIP:string; aLabel:TNotifyString);
var ParamList:TStringList;
begin
  ParamList := TStringList.Create;
  ParamList.Add('name='+aLogin);
  ParamList.Add('password='+aPass);
  ParamList.Add('ip='+aIP);

  with THTTPPostThread.Create(fServerAddress+'add_user.php', ParamList.Text) do
  begin
    fCarryObject := aLabel;
    OnTerminate := AskServerForUsernameDone;
  end;

  ParamList.Free;
end;


procedure TKMLobby.AskServerForUsernameDone(Sender:TObject);
begin
  THTTPPostThread(Sender).fCarryObject(Self, THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.UpdateState();
begin
  //
end;


end.
