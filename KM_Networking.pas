unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, StrUtils, SysUtils, Windows,
  KM_CommonTypes, KM_Defaults,
  KM_NetPlayersList, KM_Network, KM_Player;


type
  TStringEvent = procedure (const aData: string) of object;
  TStreamEvent = procedure (aData: TKMemoryStream) of object;

type
  TLANPlayerKind = (lpk_Host, lpk_Joiner);

  TMessageKind = (  mk_Unknown,
                    mk_AskToJoin,
                    mk_Timeout,
                    mk_AllowToJoin,
                    mk_RefuseToJoin, //When max players is exceeded or nikname is taken
                    mk_VerifyJoin,
                    mk_PlayersList,
                    mk_MapSelect,
                    mk_ReadyToStart, //Joiner telling he's ready
                    mk_Start, //Host starting the game

                    mk_Commands,
                    mk_Text);

type
  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
    private
      fNetwork:TKMNetwork; //Our Network interface
      fLANPlayerKind: TLANPlayerKind; //Our role
      fHostAddress:string;
      fMyAddress:string;
      fMyNikname:string;
      fNetPlayers:TKMPlayersList;

      fMapName:string;
      fMissionMode:TMissionMode;

      fJoinTick:cardinal; //Timer to issue timeout event on connection
      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TStringEvent;
      fOnTextMessage:TStringEvent;
      fOnPlayersList:TStringEvent;
      fOnMapName:TStringEvent;
      fOnAllReady:TNotifyEvent;
      fOnCommands:TStringEvent;

      function CheckCanJoin(aAddr, aNik:string):string;
      procedure StartGame;

      procedure PacketRecieve(const aData: array of byte; aAddr:string); //Process all commands
      procedure PacketRecieveJoin(const aData: array of byte; aAddr:string); //Process only "Join" commands
      procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string);
      procedure PacketToAll(aKind:TMessageKind; const aData:string='');
      procedure PacketToHost(aKind:TMessageKind; const aData:string='');
    public
      constructor Create;
      destructor Destroy; override;

      //Lobby
      function MyIPString:string;
      function MyIPStringAndPort:string;
      procedure Host(aUserName:string);
      procedure Join(aServerAddress,aUserName:string);
      procedure Disconnect;
      function Connected: boolean;
      procedure MapSelect(aName:string);
      procedure ReadyToStart;
      procedure StartClick; //All arguments required are in our class

      //Common
      procedure PostMessage(aText:string);

      //Gameplay
      function MissionMode:TMissionMode;
      function PlayerCount:integer;
      function PlayerType(aIndex:integer):TPlayerType;
      procedure SendCommands(aStream:TKMemoryStream);

      property OnJoinSucc:TNotifyEvent write fOnJoinSucc;       //We were allowed to join
      property OnJoinFail:TStringEvent write fOnJoinFail;       //We were refused to join
      property OnTextMessage:TStringEvent write fOnTextMessage; //Text message recieved
      property OnPlayersList:TStringEvent write fOnPlayersList; //Player list updated
      property OnMapName:TStringEvent write fOnMapName;         //Map name updated
      property OnAllReady:TNotifyEvent write fOnAllReady;       //Everyones ready to start playing
      property OnCommands:TStringEvent write fOnCommands;       //Recieved commands
      procedure UpdateState;
    end;


implementation
uses KM_Game, KM_Utils;


{ TKMNetworking }
constructor TKMNetworking.Create;
begin
  Inherited;
  fNetwork  := TKMNetwork.Create(MULTIPLE_COPIES);
  fNetPlayers  := TKMPlayersList.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fNetPlayers.Free;
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
  Disconnect;
  fJoinTick := 0;
  fHostAddress := ''; //Thats us
  fMyAddress := MyIPString;
  fMyNikname := aUserName;
  fLANPlayerKind := lpk_Host;
  fNetPlayers.Clear;
  fNetPlayers.AddPlayer(MyIPString, fMyNikname);
  fNetPlayers.SetReady(fMyNikname);
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersList) then fOnPlayersList(fNetPlayers.AsStringList);
end;


procedure TKMNetworking.Join(aServerAddress,aUserName:string);
begin
  Disconnect;
  fHostAddress := aServerAddress;
  fMyAddress := MyIPString;
  fMyNikname := aUserName;
  fLANPlayerKind := lpk_Joiner;
  fJoinTick := GetTickCount + 3000; //3sec
  fNetPlayers.Clear;
  fNetPlayers.AddPlayer(MyIPString, fMyNikname);
  fNetwork.StartListening;
  PacketToHost(mk_AskToJoin, fMyNikname);
  fNetwork.OnRecieveKMPacket := PacketRecieveJoin; //Unless we join use shortlist
end;


procedure TKMNetworking.Disconnect;
begin
  fNetwork.StopListening;
  fNetPlayers.Clear;
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
  fNetPlayers.SetReady(fMyNikname);
  PacketToAll(mk_ReadyToStart, fMyNikname);
end;


//Tell other players we want to start
//Send whole game setup info at once, making sure there are no misunderstandings,
//especially about random values (e.g. start locations)
procedure TKMNetworking.StartClick;
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can start the game');
  Assert(fNetPlayers.AllReady, 'Not everyone is ready to start');

  //For now we assume that everything is synced

  //Define random parameters (start locations)

  //Let everyone start
  PacketToAll(mk_Start, '');

  StartGame;
end;


procedure TKMNetworking.StartGame;

begin
  fGame.GameStartMP(KMMapNameToPath(fMapName, 'dat'), 'MP game', fNetPlayers.GetStartLoc(fMyNikname));
end;


//See if player can join our game
function TKMNetworking.CheckCanJoin(aAddr, aNik:string):string;
begin
  if fNetPlayers.Count >= MAX_PLAYERS then
    Result := 'No more players can join the game'
  else
  if fNetPlayers.NiknameExists(aNik) then
    Result := 'Player with this nik already joined the game';
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketToAll(mk_Text, fMyAddress + '/' + fMyNikname + ': ' + aText);
  fOnTextMessage(fMyAddress + '/' + fMyNikname + ': ' + aText);
end;


function TKMNetworking.MissionMode:TMissionMode;
begin
  Result := fMissionMode;
end;


function TKMNetworking.PlayerCount:integer;
begin
  Result := fNetPlayers.Count;
end;


function TKMNetworking.PlayerType(aIndex:integer):TPlayerType;
begin
  Result := fNetPlayers.PlayerType(aIndex);
end;


procedure TKMNetworking.SendCommands(aStream:TKMemoryStream);
var s:string;
begin
  SetLength(s, aStream.Size);
  aStream.WriteBuffer(s[1], aStream.Size);
  PacketToAll(mk_Commands, s); //Send commands to all players
end;


procedure TKMNetworking.PacketRecieve(const aData: array of byte; aAddr:string);
var Kind:TMessageKind; Msg:string; ReMsg:string;
begin
  Assert(Length(aData) >= 1, 'Unexpectedly short message'); //Kind, Message

  Kind := TMessageKind(aData[0]);
  if Length(aData) > 1 then
    SetString(Msg, PAnsiChar(@aData[1]), Length(aData)-1)
  else
    Msg := '';

  case Kind of
    mk_AskToJoin:   if fLANPlayerKind = lpk_Host then begin
                      ReMsg := CheckCanJoin(aAddr, Msg);
                      if ReMsg = '' then
                        PacketSend(aAddr, mk_AllowToJoin, 'Allowed')
                      else
                        PacketSend(aAddr, mk_RefuseToJoin, ReMsg);
                    end;
    mk_VerifyJoin:  if fLANPlayerKind = lpk_Host then begin
                      fNetPlayers.AddPlayer(aAddr, Msg);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fNetPlayers.AsStringList);
                      PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
                      PostMessage(aAddr+'/'+Msg+' has joined');
                    end;
    mk_PlayersList: if fLANPlayerKind = lpk_Joiner then begin
                      fNetPlayers.SetAsText(Msg);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fNetPlayers.AsStringList);
                    end;
    mk_ReadyToStart:if fLANPlayerKind = lpk_Host then begin
                      fNetPlayers.SetReady(Msg);
                      if (fLANPlayerKind = lpk_Host) and fNetPlayers.AllReady and (fNetPlayers.Count>1) then
                        if Assigned(fOnAllReady) then fOnAllReady(nil);
                    end;
    mk_MapSelect:   if fLANPlayerKind = lpk_Joiner then begin
                      fMapName := Msg;
                      if Assigned(fOnMapName) then fOnMapName(fMapName);
                    end;
    mk_Start:       if fLANPlayerKind = lpk_Joiner then begin
                      StartGame;
                    end;
    mk_Commands:    if Assigned(fOnCommands) then fOnCommands(Msg);
    mk_Text:        if Assigned(fOnTextMessage) then fOnTextMessage(Msg);
  end;
end;


procedure TKMNetworking.PacketRecieveJoin(const aData: array of byte; aAddr:string);
var Kind:TMessageKind; Msg:string;
begin
  Kind := TMessageKind(aData[0]);
  if Length(aData) > 1 then
    SetString(Msg, PAnsiChar(@aData[1]), Length(aData)-1)
  else
    Msg := '';

  case Kind of //Handle only 2 messages kinds
    mk_AllowToJoin: begin
                      fJoinTick := 0;
                      fNetwork.OnRecieveKMPacket := PacketRecieve;
                      PacketToHost(mk_VerifyJoin, fMyNikname);
                      fOnJoinSucc(Self);
                    end;
    mk_RefuseToJoin:begin
                      fJoinTick := 0;
                      fNetwork.OnRecieveKMPacket := nil;
                      fNetwork.StopListening;
                      fOnJoinFail(Msg);
                    end;
    mk_Timeout:     begin
                      fJoinTick := 0;
                      fNetwork.OnRecieveKMPacket := nil;
                      fNetwork.StopListening;
                      fOnJoinFail('no response');
                    end;
  end;
end;


procedure TKMNetworking.PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string);
begin
  fNetwork.SendTo(aAddress, char(aKind) + aData);
end;


procedure TKMNetworking.PacketToAll(aKind:TMessageKind; const aData:string='');
var i:integer;
begin
  for i:=1 to fNetPlayers.Count do
    if fNetPlayers.IsHuman(i) and (fNetPlayers.GetNikname(i) <> fMyNikname) then
      PacketSend(fNetPlayers.GetAddress(i), aKind, aData);
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
