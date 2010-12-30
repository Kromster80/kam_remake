unit KM_Lobby;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KromUtils, SysUtils, StrUtils, Math,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Windows;

type
  THTTPPostThread = class(TThread)
    private
      fHTTP:TIdHTTP;
      fPost:string;
      fParams:TStringList;
      fReturn:PString;
      fReturnObject:TNotifyEvent;
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

      fUserTT:THTTPPostThread;
      //fTT:array[1..16]of THTTPPostThread;

      function GetPlayersList():string;
      function GetPostsList():string;
      procedure AskServerFor(i:byte; aQuery,aParams:string; aReturn:PString);
      procedure AskServerForUsername(aLogin, aPass, aIP:string; aReturn:TNotifyEvent);
      procedure AskServerForUsernameDone(Sender:TObject);
    public
      constructor Create(aAddress:string; aLogin,aPass,aIP:string; aReturn:TNotifyEvent);
      destructor Destroy; override;

      procedure PostMessage(aText:string);

      property PlayersList:string read GetPlayersList;
      property RoomsList:string read fRoomsList;
      property PostsList:string read GetPostsList;
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
  s := StringReplace(s,'<br>',#13#10,[rfReplaceAll, rfIgnoreCase]);
  Result := Copy(s, 0, length(s)-3);
end;

{ For some reason returned string begins with Unicode Byte-Order Mark (BOM) $EFBBBF
  I googled for it and all solutions suggest to simply ignore/remove it }
procedure THTTPPostThread.Execute;
begin
  ResultMsg := fHTTP.Post(fPost, fParams);
  ResultMsg := StringReplace(ResultMsg,#$EF+#$BB+#$BF,'',[rfReplaceAll]); //Remove BOM
  if fReturn<>nil then fReturn^ := ReplyToString(ResultMsg) +' /'+ inttostr(random(9));

  if fHTTP.Connected then fHTTP.DisconnectSocket;

  fParams.Free;
  fHTTP.Free;
end;


{ TKMLobby }
constructor TKMLobby.Create(aAddress:string; aLogin,aPass,aIP:string; aReturn:TNotifyEvent);
begin
  Inherited Create;
  fServerAddress := aAddress;
  fUserName := aLogin;
  fRoomName := ''; //no room specified, means Main lobby

  AskServerForUsername(aLogin, aPass, aIP, aReturn);
end;


destructor TKMLobby.Destroy;
begin
  if fUserTT<>nil then fUserTT.Free;
  //Logout;
  //Disconnect;
  Inherited;
end;


{ Returns player list as: PLAYER - (last_seen) }
function TKMLobby.GetPlayersList():string;
var s:TStringList; i:integer;
begin
  s := TStringList.Create;
  s.Text := fPlayersList;

  for i:=0 to s.Count-1 do
    s[i] := Copy(s[i],0,Pos(',',s[i])-1) + ' - ' + Copy(s[i],length(s[i])-6,length(s[i]));

  Result := s.Text;
  s.Free;
end;


{  }
function TKMLobby.GetPostsList():string;
var s:TStringList; //i:integer;
begin
  s := TStringList.Create;
  s.Text := fPostsList;

//  for i:=0 to s.Count-1 do
//    s[i] := Copy(s[i],0,Pos(',',s[i])-1) + ' - ' + Copy(s[i],length(s[i])-4,length(s[i]));

  Result := s.Text;
  s.Free;
end;


procedure TKMLobby.AskServerForUsername(aLogin, aPass, aIP:string; aReturn:TNotifyEvent);
var ParamList:TStringList;
begin
  ParamList := TStringList.Create;
  ParamList.Add('name='+aLogin);
  ParamList.Add('password='+aPass);
  ParamList.Add('ip='+aIP);

  fUserTT := THTTPPostThread.Create(fServerAddress+'add_user.php', ParamList.Text, nil);
  fUserTT.fReturnObject := aReturn;
  fUserTT.FreeOnTerminate := true;
  fUserTT.OnTerminate := AskServerForUsernameDone;

  ParamList.Free;
end;


procedure TKMLobby.AskServerForUsernameDone(Sender:TObject);
begin
  if THTTPPostThread(Sender).ResultMsg = 'allow to enter' then
    THTTPPostThread(Sender).fReturnObject(Self)
  else
    THTTPPostThread(Sender).fReturnObject(nil);

  THTTPPostThread(Sender).Terminate;
  fUserTT := nil;
end;


procedure TKMLobby.AskServerFor(i:byte; aQuery,aParams:string; aReturn:PString);
begin
  with THTTPPostThread.Create(fServerAddress+aQuery, aParams, aReturn) do
    FreeOnTerminate := true;
end;


procedure TKMLobby.PostMessage(aText:string);
var ParamList:TStringList;
begin
  ParamList := TStringList.Create;
  ParamList.Add('user_name='+fUserName);
  ParamList.Add('room_name='+fRoomName);
  ParamList.Add('text='+aText);
  AskServerFor(5, 'add_post.php', ParamList.Text, nil);
  ParamList.Free;
end;


procedure TKMLobby.UpdateState();
begin
  if GetTickCount mod  50 = 0 then AskServerFor(1, 'list_users.php', '', @fPlayersList);
  //if GetTickCount mod  50 = 0 then AskServerFor(2, 'list_rooms.php', '', @fRoomsList);
  if GetTickCount mod  10 = 0 then AskServerFor(3, 'list_posts.php', '', @fPostsList);
  if GetTickCount mod 100 = 0 then AskServerFor(4, 'update_last_visit.php', 'name='+fUserName, nil); //once a minute
end;


end.
