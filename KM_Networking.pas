unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_Network, KromUtils, SysUtils, StrUtils, Math, Windows;


type TStringEvent = procedure (const aData: string) of object;

type TLANPlayerKind = (lpk_Host, lpk_Joiner);

type TMessageKind = (
                      mkUnknown,
                      mk_AskToJoin,
                      mk_Timeout,
                      mk_AllowToJoin,
                      //mk_RefuseToJoin, //When max players is exceeded
                      mk_VerifyJoin,
                      mk_PlayersList,

                      mk_Text,
                      mk_GameSetup,
                      mk_Gameplay);


type
  TKMNetworking = class
    private
      fNetwork:TKMNetwork;
      fLANPlayerKind: TLANPlayerKind;
      fServerAddress:string; //Who's the host

      fPlayersList:TStringList; //Stores IP addresses for now

      JoinTick:cardinal;
      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TNotifyEvent;
      fOnTextMessage:TStringEvent;
      fOnPlayersList:TStringEvent;

      procedure SendPlayerList;
      procedure DecodePlayersList(const aText:string);
      procedure PacketRecieve(const aData: array of byte; aAddr:string); //Process all commands
      procedure PacketRecieveJoin(const aData: array of byte; aAddr:string); //Process only "Join" commands
      procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string='');
    public
      constructor Create;
      destructor Destroy; override;

      function MyIPString:string;
      function MyIPStringAndPort:string;
      procedure Host(aUserName:string);
      procedure Connect(aServerAddress,aUserName:string);
      procedure Disconnect;
      function Connected: boolean;

      property OnJoinSucc:TNotifyEvent write fOnJoinSucc;
      property OnJoinFail:TNotifyEvent write fOnJoinFail;
      property OnTextMessage:TStringEvent write fOnTextMessage;
      property OnPlayersList:TStringEvent write fOnPlayersList;

      procedure PostMessage(aText:string);
      procedure UpdateState;
    end;


implementation


{ TKMNetworking }
constructor TKMNetworking.Create;
begin
  Inherited;
  fNetwork  := TKMNetwork.Create(MULTIPLE_COPIES);
  fPlayersList  := TStringList.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fPlayersList.Free;
  fNetwork.Free;
  Inherited;
end;


function TKMNetworking.MyIPString:string;
begin
  Result := fNetwork.MyIPString;
end;


function TKMNetworking.MyIPStringAndPort:string;
begin
  Result := fNetwork.MyIPStringAndPort;
end;


procedure TKMNetworking.Host(aUserName:string);
begin
  JoinTick := 0;
  fLANPlayerKind := lpk_Host;
  fPlayersList.Add(MyIPString); //Add self
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
end;


procedure TKMNetworking.Connect(aServerAddress,aUserName:string);
begin
  fServerAddress := aServerAddress;
  fLANPlayerKind := lpk_Joiner;
  JoinTick := GetTickCount + 3000; //3sec
  fNetwork.StartListening;
  PacketSend(fServerAddress, mk_AskToJoin);
  fNetwork.OnRecieveKMPacket := PacketRecieveJoin; //Unless we join use shortlist
end;


procedure TKMNetworking.Disconnect;
begin
  fNetwork.StopListening;
  fPlayersList.Clear;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetwork.fListening;
end;


procedure TKMNetworking.SendPlayerList;
var i:integer;
begin
  for i:=1 to fPlayersList.Count-1 do
    PacketSend(fPlayersList[i], mk_PlayersList, fPlayersList.Text);
end;


procedure TKMNetworking.DecodePlayersList(const aText:string);
begin
  Assert(RightStr(aText,2) = eol); //Make sure aText is complete, it must end with a seperator
  fPlayersList.Text := aText; //Replace the text
end;


procedure TKMNetworking.PacketRecieve(const aData: array of byte; aAddr:string);
var Kind:TMessageKind; Data:string;
begin
  Assert(Length(aData) >= 1, 'Unexpectedly short message'); //Kind, Message

  Kind := TMessageKind(aData[0]);
  if Length(aData) > 1 then
    SetString(Data, PAnsiChar(@aData[1]), Length(aData)-1)
  else
    Data := '';

  case Kind of
    mk_AskToJoin:   PacketSend(aAddr, mk_AllowToJoin);
    mk_VerifyJoin:  begin
                      fPlayersList.Add(aAddr);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
                      SendPlayerList;
                      PostMessage(aAddr+' has joined');
                    end;
    mk_PlayersList: begin
                      DecodePlayersList(Data);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
                    end;
    mk_Text:        if Assigned(fOnTextMessage) then fOnTextMessage(Data);
  end;
end;


procedure TKMNetworking.PacketRecieveJoin(const aData: array of byte; aAddr:string);
var Kind:TMessageKind;
begin
  Kind := TMessageKind(aData[0]);
  case Kind of //Handle only 2 messages kinds
    mk_AllowToJoin: begin
                      JoinTick := 0;
                      fNetwork.OnRecieveKMPacket := PacketRecieve;
                      PacketSend(fServerAddress, mk_VerifyJoin);
                      fOnJoinSucc(Self);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
                    end;
    mk_Timeout:     begin
                      JoinTick := 0;
                      fNetwork.OnRecieveKMPacket := nil;
                      fNetwork.StopListening;
                      fOnJoinFail(Self);
                    end;
  end;
end;


procedure TKMNetworking.PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string='');
begin
  fNetwork.SendTo(aAddress, char(aKind) + aData);
end;


procedure TKMNetworking.PostMessage(aText:string);
var i:integer;
begin
  fOnTextMessage(fPlayersList[0] + ': ' + aText);

  //Send to partners
  for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
    PacketSend(fPlayersList[i], mk_Text, fPlayersList[0] + ': ' + aText);
end;


procedure TKMNetworking.UpdateState;
const MyArray : array[0..0] of byte = (byte(mk_Timeout)); //Convert byte to array
begin
  if (JoinTick<>0) and (JoinTick <= GetTickCount) then
    PacketRecieveJoin(MyArray, '127.0.0.1'); //Time is up, wait no longer
end;


end.
