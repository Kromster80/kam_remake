unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_Network, KromUtils, SysUtils, StrUtils, Math, Windows;


type TStringEvent = procedure (const aData: string) of object;


type TMessageKind = (
                      mkUnknown,
                      mkHandshaking_AskToJoin,
                      mkHandshaking_Timeout,
                      mkHandshaking_AllowToJoin,
                      mkHandshaking_RefuseToJoin,
                      mkHandshaking_VerifyJoin,
                      mkHandshaking_SendPlayersList,

                      mkText,
                      mkGameSetup,
                      mkGameplay);


type
  TKMNetworking = class
    private
      fNetwork:TKMNetwork;
      fServerAddress:string; //Who's the host

      fPlayersList:TStringList; //Stores IP addresses for now

      JoinTick:cardinal;
      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TNotifyEvent;
      fOnTextMessage:TStringEvent;

      procedure SendPlayerList;
      procedure PacketRecieveHost(const aData: array of byte; aAddr:string);
      procedure PacketRecieveJoin(const aData: array of byte; aAddr:string);
      procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string='');
    public
      constructor Create;
      destructor Destroy; override;

      function MyIPString:string;
      procedure Host(aUserName:string);
      procedure Connect(aServerAddress,aUserName:string);
      procedure StopNetwork;
      function Connected: boolean;

      property OnJoinSucc:TNotifyEvent write fOnJoinSucc;
      property OnJoinFail:TNotifyEvent write fOnJoinFail;
      property OnTextMessage:TStringEvent write fOnTextMessage;

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


procedure TKMNetworking.Host(aUserName:string);
begin
  JoinTick := 0;
  fPlayersList.Add(fNetwork.MyIPString); //Add self
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieveHost; //Start listening
end;


procedure TKMNetworking.Connect(aServerAddress,aUserName:string);
begin
  fServerAddress := aServerAddress;
  JoinTick := GetTickCount + 3000; //3sec
  fNetwork.StartListening;
  PacketSend(fServerAddress, mkHandshaking_AskToJoin);
  fNetwork.OnRecieveKMPacket := PacketRecieveJoin; //Unless we join use shortlist
end;


procedure TKMNetworking.StopNetwork;
begin
  fNetwork.StopListening;
  fPlayersList.Clear;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetwork.fListening;
end;


procedure TKMNetworking.SendPlayerList;
var
  MyPlayerList: string;
begin
  //MyPlayerList := fPlayersList.Count
  //for i := 1 to fPlayersList.Count-1 do

  //for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
  //  PacketSend(fPlayersList[i], mkHandshaking_SendPlayersList, MyPlayerList);
end;


procedure TKMNetworking.PacketRecieveHost(const aData: array of byte; aAddr:string);
var Kind:TMessageKind; MyString: string;
begin
  Kind := TMessageKind(aData[0]);
  case Kind of
    mkHandshaking_AskToJoin:   PacketSend(aAddr, mkHandshaking_AllowToJoin);
    mkHandshaking_VerifyJoin:  begin
                                fPlayersList.Add(aAddr);
                                SendPlayerList;
                                PostMessage(aAddr+' has joined');
                               end;
    mkText:                    begin
                                 SetString(MyString, PAnsiChar(@aData[1]), Length(aData)-2);
                                 if Assigned(fOnTextMessage) then fOnTextMessage(MyString);
                               end;
  end;
end;


procedure TKMNetworking.PacketRecieveJoin(const aData: array of byte; aAddr:string);
var Kind:TMessageKind;
begin
  Kind := TMessageKind(aData[0]);
  case Kind of //Handle only 2 messages kinds
    mkHandshaking_AllowToJoin: begin
                                JoinTick := 0;
                                fPlayersList.Add(fNetwork.MyIPString);
                                fPlayersList.Add(fServerAddress);
                                PacketSend(fServerAddress, mkHandshaking_VerifyJoin);
                                fOnJoinSucc(Self);
                               end;
    mkHandshaking_RefuseToJoin:begin
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
  fOnTextMessage(aText);

  //Send to partners
  for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
    PacketSend(fPlayersList[i], mkText, fPlayersList[0] + ': ' + aText);

end;


procedure TKMNetworking.UpdateState;
const MyArray : array[0..0] of byte = (byte(mkHandshaking_RefuseToJoin));
begin
  if (JoinTick<>0) and (JoinTick <= GetTickCount) then
    PacketRecieveHost(MyArray, '127.0.0.1');
end;


end.
