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

      fUserName:string;
      fRoomName:string;

      fPlayersList:string;
      fRoomsList:string;
      fPostsList:string;
      function ReplyToString(s:string):string;
    public
      constructor Create(aAddress:string);
      destructor Destroy; override;
      procedure GetIPAsync(aLabel:TNotifyString);
      procedure GetIPAsyncDone(Sender:TObject);
      procedure AskServerForUsername(aLogin, aPass, aIP:string; aLabel:TNotifyString);
      procedure AskServerForUsernameDone(Sender:TObject);
      procedure AskServerForUserList();
      procedure AskServerForUserListDone(Sender:TObject);
      procedure AskServerForRoomList();
      procedure AskServerForRoomListDone(Sender:TObject);
      procedure AskServerForPostList();
      procedure AskServerForPostListDone(Sender:TObject);
      procedure AskServerToPostMessage(aMsg:string);

      property PlayersList:string read fPlayersList;
      property RoomsList:string read fRoomsList;
      property PostsList:string read fPostsList;
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


function TKMLobby.ReplyToString(s:string):string;
begin
  s := RightStr(s, Length(s)-Pos('</p>',s)-3);
  Result := StringReplace(s,'<br>','|',[rfReplaceAll, rfIgnoreCase]);
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


procedure TKMLobby.AskServerForUserList();
begin
  with THTTPPostThread.Create(fServerAddress+'list_users.php', '') do
    OnTerminate := AskServerForUserListDone;
end;


procedure TKMLobby.AskServerForUserListDone(Sender:TObject);
begin
  fPlayersList := ReplyToString(THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerForRoomList();
begin
  with THTTPPostThread.Create(fServerAddress+'list_rooms.php', '') do
    OnTerminate := AskServerForRoomListDone;
end;


procedure TKMLobby.AskServerForRoomListDone(Sender:TObject);
begin
  fRoomsList := ReplyToString(THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerForPostList();
begin
  with THTTPPostThread.Create(fServerAddress+'list_posts.php', '') do
    OnTerminate := AskServerForPostListDone;
end;


procedure TKMLobby.AskServerForPostListDone(Sender:TObject);
begin
  fPostsList := ReplyToString(THTTPPostThread(Sender).ResultMsg);
  THTTPPostThread(Sender).Terminate;
end;


procedure TKMLobby.AskServerToPostMessage(aMsg:string);
var ParamList:TStringList;
begin
  ParamList := TStringList.Create;
  ParamList.Add('user_name='+fUserName);
  ParamList.Add('room_name='+fRoomName);
  ParamList.Add('text='+aMsg);
  THTTPPostThread.Create(fServerAddress+'add_post.php', ParamList.Text);
  ParamList.Free;
end;


procedure TKMLobby.UpdateState();
begin
  if GetTickCount mod 30 = 0 then AskServerForUserList;
  if GetTickCount mod 50 = 0 then AskServerForRoomList;
  if GetTickCount mod 20 = 0 then AskServerForPostList;
end;


end.
