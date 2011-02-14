unit KM_Lobby;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_Network, KromUtils, SysUtils, StrUtils, Math, Windows;


type TLobbyMsg = (lmNoReply, lmUserUsed, lmCanJoin);


type
  TKMLobby = class
    private
      fNetwork:TKMNetwork;
      fServerAddress:string; //Who's the host
      fUserName:string; //our name

      fPlayers:TStringList;

      JoinTick:cardinal;

      fOnSucc:TNotifyEvent;
      fOnFail:TNotifyEvent;
      fOnMessage:TRecieveKMPacketEvent;

      procedure PacketRecieve(const aData: string);
      procedure PacketRecieveJoin(const aData: string);
      procedure PacketSend(const aAddress,aKind,aData:string);
    public
      constructor Create(aNetwork:TKMNetwork; aServerAddress,aUserName:string);
      destructor Destroy; override;

      property OnSucc:TNotifyEvent write fOnSucc;
      property OnFail:TNotifyEvent write fOnFail;
      property OnMessage:TRecieveKMPacketEvent write fOnMessage;

      procedure PostMessage(aText:string);
      procedure UpdateState;
    end;


implementation


{ TKMLobby }
constructor TKMLobby.Create(aNetwork:TKMNetwork; aServerAddress,aUserName:string);
begin
  Inherited Create;

  fNetwork := aNetwork;
  fServerAddress := aServerAddress;
  fUserName := aUserName;
  fPlayers := TStringList.Create;

  if fServerAddress = '' then begin
    JoinTick := 0;
    fPlayers.Add(fUserName); //Add self
    fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  end else begin
    JoinTick := GetTickCount + 3000; //3sec
    PacketSend(fServerAddress, '', 'I wanna join');
    fNetwork.OnRecieveKMPacket := PacketRecieveJoin;
  end;
end;


destructor TKMLobby.Destroy;
begin
  fPlayers.Free;
  Inherited;
end;


procedure TKMLobby.PacketRecieve(const aData: string);
begin
  if aData = 'I wanna join' then
  begin
    PacketSend('127.0.0.1', '', 'jump in');
    exit;
  end;

  if aData = 'I have joined' then
  begin
    PacketSend('127.0.0.1', 'PL', fPlayers.Text);
    fPlayers.Add('127.0.0.1');
    PostMessage('127.0.0.1'+' Has joined');
    exit;
  end;

  if aData[1]+aData[2] = 'PL' then
  begin
    fPlayers.Text := RightStr(aData, length(aData)-2);
    exit;
  end;

  if Assigned(fOnMessage) then
    fOnMessage(aData);
end;


procedure TKMLobby.PacketRecieveJoin(const aData: string);
begin
  if aData = 'jump in' then
  begin
    JoinTick := 0;
    fNetwork.OnRecieveKMPacket := PacketRecieve;
    PacketSend(fServerAddress, '', 'I have joined');
    fOnSucc(Self);
  end else
  if aData = 'timeout' then
  begin
    JoinTick := 0;
    fNetwork.OnRecieveKMPacket := nil;
    fOnFail(Self);
  end;
  //Any other unexpected message will be ignored
end;


procedure TKMLobby.PacketSend(const aAddress,aKind,aData:string);
begin
  fNetwork.SendTo(aAddress, aKind + aData);
end;


procedure TKMLobby.PostMessage(aText:string);
var i:integer;
begin
  fOnMessage(fUserName + ': ' + aText);

  //Send to partners
  for i := 1 to fPlayers.Count-1 do //Eclude self and send to [2nd to last] range
    PacketSend(fPlayers[i], '', aText);

end;


procedure TKMLobby.UpdateState;
begin
  if (JoinTick<>0) and (JoinTick <= GetTickCount) then
    PacketRecieveJoin('timeout'); //Any erroneus string will do
end;


end.
