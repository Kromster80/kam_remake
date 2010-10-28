unit KM_Lobby;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Controls, KM_Defaults, KM_CommonTypes, KromUtils, SysUtils, StrUtils, Math,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Windows;

type
  THTTPPostThread = class(TThread)
    private
      fHTTP:TIdHTTP;
      fPost:string;
      fParams:TStringList;
      fCarryObject:TNotifyString;
      fReturn:PString;
      function ReplyToString(s:string):string;
    public
      ResultMsg:string;
      constructor Create(aPost, aParams:string; aReturn:PString);
      procedure Execute; override;
    end;


type TLobbyMsg = (lmNoReply, lmUserUsed, lmCanJoin);

{Stay in place for set time}
type
  TKMLobby = class
    private
      fServerAddress:string;

      fUserName:string;
      fRoomName:string;

      fPlayersList:string;
      fRoomsList:string;
      fPostsList:string;

    public
      constructor Create(aAddress:string);
      destructor Destroy; override;
      procedure GetIPAsync(aLabel:TNotifyString);
      procedure GetIPAsyncDone(Sender:TObject);
      procedure AskServerForUsername(aLogin, aPass, aIP:string; aLabel:TNotifyString);
      procedure AskServerForUsernameDone(Sender:TObject);

      procedure AskServerFor(aQuery,aParams:string; aReturn:PString);
//      procedure AskServerForDone(Sender:TObject);

{      procedure AskServerForRoomList();
      procedure AskServerForRoomListDone(Sender:TObject);
      procedure AskServerForPostList();
      procedure AskServerForPostListDone(Sender:TObject);
      procedure AskServerToPostMessage(aMsg:string);}

      property PlayersList:string read fPlayersList;
      property RoomsList:string read fRoomsList;
      property PostsList:string read fPostsList;
      procedure UpdateState();
    end;


implementation


constructor THTTPPostThread.Create(aPost, aParams:string; aReturn:PString);
begin
  fHTTP := TIdHTTP.Create(nil);
  fPost := aPost;
  fParams := TStringList.Create; //Empty for now
  fParams.Text := aParams;
  fReturn := aReturn;
  Inherited Create(false);
end;


function THTTPPostThread.ReplyToString(s:string):string;
begin
  s := RightStr(s, Length(s)-Pos('</p>',s)-3);
  Result := StringReplace(s,'<br>','|',[rfReplaceAll, rfIgnoreCase]);
end;

{ For some reason returned string begins with Unicode Byte-Order Mark (BOM) $EFBBBF
  I googled for it and all solutions suggest to simply ignore/remove it }
procedure THTTPPostThread.Execute;
begin
  ResultMsg := fHTTP.Post(fPost, fParams);
  ResultMsg := StringReplace(ResultMsg,#$EF+#$BB+#$BF,'',[rfReplaceAll]); //Remove BOM
  if fReturn<>nil then fReturn^ := ReplyToString(ResultMsg);
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
  with THTTPPostThread.Create('http://www.whatismyip.com/automation/n09230945.asp','',nil) do
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

  fUserName := aLogin;

  ParamList.Add('name='+aLogin);
  ParamList.Add('password='+aPass);
  ParamList.Add('ip='+aIP);

  with THTTPPostThread.Create(fServerAddress+'add_user.php', ParamList.Text, nil) do
  begin
    fCarryObject := aLabel;
    FreeOnTerminate := true;
    OnTerminate := AskServerForUsernameDone;
  end;

  ParamList.Free;
end;


procedure TKMLobby.AskServerForUsernameDone(Sender:TObject);
begin
  THTTPPostThread(Sender).fCarryObject(Self, THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerFor(aQuery,aParams:string; aReturn:PString);
begin
  with THTTPPostThread.Create(fServerAddress+aQuery, aParams, aReturn) do
    FreeOnTerminate := true;
end;


procedure TKMLobby.UpdateState();
begin
  if GetTickCount mod 30 = 0 then AskServerFor('list_users.php', '', @fPlayersList);
  if GetTickCount mod 50 = 0 then AskServerFor('list_rooms.php', '', @fRoomsList);
  if GetTickCount mod 20 = 0 then AskServerFor('list_posts.php', '', @fPostsList);

  if GetTickCount mod 100 = 0 then
    AskServerFor('update_last_visit.php', fUserName, nil); //once a minute

end;


end.
