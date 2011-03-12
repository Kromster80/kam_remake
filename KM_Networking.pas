unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, StrUtils, SysUtils, Windows,
  KM_CommonTypes, KM_Defaults,
  KM_NetPlayersList, KM_Network;


type
  TStringEvent = procedure (const aData: string) of object;
  TStreamEvent = procedure (aData: TKMemoryStream) of object;

type
  TLANPlayerKind = (lpk_Host, lpk_Joiner);

  TMessageKind = (  mk_Unknown,
                    mk_AskToJoin,
                    mk_Timeout,
                    mk_AllowToJoin,
                    //mk_RefuseToJoin, //When max players is exceeded
                    mk_VerifyJoin,
                    mk_PlayersList,
                    mk_ReadyToStart, //Joiner telling he's ready
                    mk_MapSelect,

                    mk_Text,
                    mk_GameSetup,
                    mk_Gameplay);

type
  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
    private
      fNetwork:TKMNetwork; //Our Network interface
      fLANPlayerKind: TLANPlayerKind; //Our role
      fHostAddress:string; 
      fMyAddress:string;
      fMyNikname:string;
      fPlayers:TKMPlayersList;

      fMapName:string;

      fJoinTick:cardinal;//Timer to issue timeout event on connection
      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TNotifyEvent;
      fOnTextMessage:TStringEvent;
      fOnPlayersList:TStringEvent;
      fOnMapName:TStringEvent;
      fOnAllReady:TNotifyEvent;
      fOnCommands:TStreamEvent;

      procedure EncodeGameSetup(aStream:TKMemoryStream);

      procedure PacketRecieve(const aData: array of byte; aAddr:string); //Process all commands
      procedure PacketRecieveJoin(const aData: array of byte; aAddr:string); //Process only "Join" commands
      procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string='');
      procedure PacketToAll(aKind:TMessageKind; const aData:string='');
      procedure PacketToHost(aKind:TMessageKind; const aData:string='');
    public
      constructor Create;
      destructor Destroy; override;

      //Lobby
      function MyIPString:string;
      function MyIPStringAndPort:string;
      procedure Host(aUserName:string);
      procedure Connect(aServerAddress,aUserName:string);
      procedure Disconnect;
      function Connected: boolean;
      procedure MapSelect(aName:string);
      procedure ReadyToStart;
      procedure StartGame; //All arguments required are in our class

      //Common
      procedure PostMessage(aText:string);

      //Gameplay
      procedure SendCommands(aStream:TKMemoryStream);

      property OnJoinSucc:TNotifyEvent write fOnJoinSucc;
      property OnJoinFail:TNotifyEvent write fOnJoinFail;
      property OnTextMessage:TStringEvent write fOnTextMessage;
      property OnPlayersList:TStringEvent write fOnPlayersList;
      property OnMapName:TStringEvent write fOnMapName;
      property OnAllReady:TNotifyEvent write fOnAllReady;
      property OnCommands:TStreamEvent write fOnCommands;
      procedure UpdateState;
    end;


implementation


{ TKMNetworking }
constructor TKMNetworking.Create;
begin
  Inherited;
  fNetwork  := TKMNetwork.Create(MULTIPLE_COPIES);
  fPlayers  := TKMPlayersList.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fPlayers.Free;
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
  fJoinTick := 0;
  fHostAddress := ''; //Thats us
  fMyAddress := MyIPString;
  fMyNikname := aUserName;
  fLANPlayerKind := lpk_Host;
  fPlayers.Clear;
  fPlayers.AddPlayer(MyIPString, aUserName);
  fPlayers.SetReady(fMyNikname);
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsStringList);
end;


procedure TKMNetworking.Connect(aServerAddress,aUserName:string);
begin
  fHostAddress := aServerAddress;
  fMyAddress := MyIPString;
  fMyNikname := aUserName;
  fLANPlayerKind := lpk_Joiner;
  fJoinTick := GetTickCount + 3000; //3sec
  fPlayers.Clear;
  fPlayers.AddPlayer(MyIPString, aUserName);
  fNetwork.StartListening;
  PacketToHost(mk_AskToJoin);
  fNetwork.OnRecieveKMPacket := PacketRecieveJoin; //Unless we join use shortlist
end;


procedure TKMNetworking.Disconnect;
begin
  fNetwork.StopListening;
  fPlayers.Clear;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetwork.fListening;
end;


//Tell other players which map we will be using
procedure TKMNetworking.MapSelect(aName:string);
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can select maps');

  fMapName := aName;
  PacketToAll(mk_MapSelect, fMapName);

  if Assigned(fOnMapName) then fOnMapName(fMapName);

  //Compare map availability and CRC
end;


//Joiner indicates that he is ready to start
procedure TKMNetworking.ReadyToStart;
begin
  fPlayers.SetReady(fMyNikname);
  PacketToAll(mk_ReadyToStart, fMyNikname);
end;


//Tell other players we want to start
//Send whole game setup info at once, making sure there are no misunderstandings,
//especially about random values (e.g. start locations)
procedure TKMNetworking.StartGame;
var Msg:TKMemoryStream;
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can start the game');
  Assert(fPlayers.AllReady, 'Not everyone is ready to start');

  Msg := TKMemoryStream.Create;
  EncodeGameSetup(Msg);
  //fPlayers.PacketSend(mk_StartGame, Msg);
  Msg.Free;

  //Now we will await confirmation messages from other players and start the game
end;


//Encode whole set of game settings into a stream (including players list)
procedure TKMNetworking.EncodeGameSetup(aStream:TKMemoryStream);
begin
  aStream.Write(fMapName);
  //aStream.Write(AllianceMode); //Fixed / changeable
  //aStream.Write(StartupConditions);
  //aStream.Write(WinConditions);
  //aStream.Write(DefeatConditions);
  //fPlayers.Save(aStream);
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketToAll(mk_Text, fMyAddress + '/' + fMyNikname + ': ' + aText);
  fOnTextMessage(fMyAddress + '/' + fMyNikname + ': ' + aText);
end;


procedure TKMNetworking.SendCommands(aStream:TKMemoryStream);
//var i:integer;
begin
  //for i:=1 to fPlayersList.Count-1 do
  //todo: send commands to all players
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
                      fPlayers.AddPlayer(aAddr, Data);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsStringList);
                      PacketToAll(mk_PlayersList, fPlayers.GetAsText);
                      PostMessage(aAddr+'/'+Data+' has joined');
                    end;
    mk_PlayersList: begin
                      fPlayers.SetAsText(Data);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsStringList);
                    end;
    mk_ReadyToStart:begin
                      fPlayers.SetReady(Data);
                      if (fLANPlayerKind = lpk_Host) and fPlayers.AllReady and (fPlayers.Count>1) then
                        if Assigned(fOnAllReady) then fOnAllReady(nil);
                    end;
    mk_MapSelect:   begin
                      fMapName := Data;
                      if Assigned(fOnMapName) then fOnMapName(fMapName);
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
                      fJoinTick := 0;
                      fNetwork.OnRecieveKMPacket := PacketRecieve;
                      PacketToHost(mk_VerifyJoin, fMyNikname);
                      fOnJoinSucc(Self);
                    end;
    mk_Timeout:     begin
                      fJoinTick := 0;
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


procedure TKMNetworking.PacketToAll(aKind:TMessageKind; const aData:string='');
var i:integer;
begin
  for i:=1 to fPlayers.Count do
    if fPlayers.IsHuman(i) and (fPlayers.GetNikname(i) <> fMyNikname) then
      PacketSend(fPlayers.GetAddress(i), aKind, aData);
end;


procedure TKMNetworking.PacketToHost(aKind:TMessageKind; const aData:string='');
begin
  Assert(fLANPlayerKind = lpk_Joiner, 'Only joined player can send data to Host');
  PacketSend(fHostAddress, aKind, aData);
end;


procedure TKMNetworking.UpdateState;
const MyArray : array[0..0] of byte = (byte(mk_Timeout)); //Convert byte to array
begin
  if (fJoinTick<>0) and (fJoinTick <= GetTickCount) then
    PacketRecieveJoin(MyArray, '127.0.0.1'); //Time is up, wait no longer
end;


end.
