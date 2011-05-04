unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, {$ENDIF}
  Classes, KromUtils, StrUtils, SysUtils,
  KM_CommonTypes, KM_Defaults,
  KM_MapInfo, KM_NetPlayersList, KM_NetServer, KM_NetClient;

  
type
  TLANPlayerKind = (lpk_Host, lpk_Joiner);

  TMessageKind = (  mk_AskToJoin,
                    mk_AllowToJoin,
                    mk_RefuseToJoin, //When nikname is taken
                    mk_VerifyJoin,

                    mk_Disconnect,  //Joiner telling host he is leaving the lobby/game
                    mk_HostDisconnect, //Host telling joiners the host is exiting (server stops)

                    mk_Poke,        //Tell partner we are still connected

                    mk_Ping,  //Perform on request
                    mk_Pong,
                    mk_PlayersList,

                    mk_StartingLocQuery,    //Ask if we can take that starting location
                    mk_FlagColorQuery,    //Ask if we can take that starting location

                    mk_MapSelect,
                    mk_ReadyToStart, //Joiner telling he's ready
                    mk_Start, //Host starting the game

                    mk_ReadyToPlay, //Joiner tells Host he has loaded the map
                    mk_Play,        //Host tells all can play

                    mk_Commands,
                    mk_Text);


  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
  private
    fNetServer:TKMNetServer;
    fNetClient:TKMNetClient;
    fLANPlayerKind: TLANPlayerKind; //Our role (Host or Joiner)
    fHostAddress:string;
    fMyNikname:string; //Stored to identify our Index in new players list
    fMyIndex:integer;
    fNetPlayers:TKMPlayersList;

    fMapInfo:TKMapInfo; //Everything related to selected map

    fOnJoinSucc:TNotifyEvent;
    fOnJoinFail:TStringEvent;
    fOnTextMessage:TStringEvent;
    fOnPlayersSetup:TNotifyEvent;
    fOnMapName:TStringEvent;
    fOnStartGame:TNotifyEvent;
    fOnPlay:TNotifyEvent;
    fOnDisconnect:TStringEvent;
    fOnPing:TNotifyEvent;
    fOnCommands:TStringEvent;

    procedure ConnectSucceed(Sender:TObject);
    procedure ConnectFailed(const S: string);
    procedure PacketRecieve(aData:pointer; aLength:cardinal); //Process all commands
    procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string);
    procedure PacketToAll(aKind:TMessageKind; const aData:string='');
    procedure PacketToHost(aKind:TMessageKind; const aData:string='');
    procedure StartGame;
  public
    constructor Create;
    destructor Destroy; override;

    property MyIndex:integer read fMyIndex;
    function MyIPString:string;

    //Lobby
    procedure Host(aUserName:string);
    procedure Join(aServerAddress,aUserName:string);
    procedure LeaveLobby;
    procedure Disconnect;
    function  Connected: boolean;
    procedure SelectMap(aName:string);
    procedure SelectLoc(aIndex:integer);
    procedure SelectColor(aIndex:integer);
    procedure ReadyToStart;
    function  CanStart:boolean;
    procedure StartClick; //All required arguments are in our class

    //Common
    procedure Ping;
    procedure PostMessage(aText:string);

    //Gameplay
    property MapInfo:TKMapInfo read fMapInfo;
    property NetPlayers:TKMPlayersList read fNetPlayers;
    procedure GameCreated;
    procedure SendCommands(aStream:TKMemoryStream; aPlayerLoc:byte=0);

    property OnJoinSucc:TNotifyEvent write fOnJoinSucc;         //We were allowed to join
    property OnJoinFail:TStringEvent write fOnJoinFail;         //We were refused to join
    property OnTextMessage:TStringEvent write fOnTextMessage;   //Text message recieved
    property OnPlayersSetup:TNotifyEvent write fOnPlayersSetup; //Player list updated
    property OnMapName:TStringEvent write fOnMapName;           //Map name updated
    property OnStartGame:TNotifyEvent write fOnStartGame;       //Start the game loading
    property OnPlay:TNotifyEvent write fOnPlay;                 //Start the gameplay
    property OnPing:TNotifyEvent write fOnPing;                 //Ping info updated
    property OnDisconnect:TStringEvent write fOnDisconnect;     //Lost connection, was kicked
    property OnCommands:TStringEvent write fOnCommands;         //Recieved GIP commands

    procedure UpdateState;
  end;


implementation
uses KM_Utils;


{ TKMNetworking }
constructor TKMNetworking.Create;
begin
  Inherited;
  fMapInfo := TKMapInfo.Create;
  fNetServer := TKMNetServer.Create;
  fNetClient := TKMNetClient.Create;
  fNetPlayers := TKMPlayersList.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fNetPlayers.Free;
  fNetServer.Free;
  fNetClient.Free;
  fMapInfo.Free;
  Inherited;
end;


function TKMNetworking.MyIPString:string;
begin
  Result := fNetClient.MyIPString;
end;


procedure TKMNetworking.Host(aUserName:string);
begin
  Disconnect;

  fNetServer.OnStatusMessage := fOnTextMessage;
  fNetServer.StartListening(KAM_PORT);

  fHostAddress := ''; //Thats us
  fMyNikname := aUserName;
  fMyIndex := 1;
  fLANPlayerKind := lpk_Host;
  fNetPlayers.Clear;
  fNetPlayers.AddPlayer(MyIPString, fMyNikname); //Selfs tick is not important
  fNetPlayers[fMyIndex].ReadyToStart := true;

  fNetClient.OnRecieveData := PacketRecieve;
  fNetClient.OnConnectSucceed := ConnectSucceed;
  fNetClient.OnConnectFailed := ConnectFailed;
  fNetClient.ConnectTo('127.0.0.1', KAM_PORT);

  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
end;


procedure TKMNetworking.Join(aServerAddress,aUserName:string);
begin
  Disconnect;
  fHostAddress := aServerAddress;
  fMyNikname := aUserName;
  fMyIndex := -1; //Host will send us Players list and we will get our index from there
  fLANPlayerKind := lpk_Joiner;
  fNetPlayers.Clear;

  fNetClient.OnRecieveData := PacketRecieve;
  fNetClient.OnConnectSucceed := ConnectSucceed;
  fNetClient.OnConnectFailed := ConnectFailed;
  fNetClient.ConnectTo(fHostAddress, KAM_PORT);
end;


procedure TKMNetworking.ConnectSucceed(Sender:TObject);
begin
  case fLANPlayerKind of
    lpk_Host:   fOnTextMessage(MyIPString + ' Connected to server');
    lpk_Joiner: PacketToHost(mk_AskToJoin, fMyNikname);
  end;
end;


procedure TKMNetworking.ConnectFailed(const S: string);
begin
  case fLANPlayerKind of
    lpk_Host:   begin
                  fNetClient.OnRecieveData := nil;
                  fNetClient.Disconnect;
                  fOnJoinFail(S);
                end;
    lpk_Joiner: begin
                  fNetClient.OnRecieveData := nil;
                  fNetClient.Disconnect;
                  fOnJoinFail(S);
                end;
  end;
end;


procedure TKMNetworking.LeaveLobby;
begin
  case fLANPlayerKind of
    lpk_Host:   PacketToAll(mk_HostDisconnect);
    lpk_Joiner: PacketToHost(mk_Disconnect, fMyNikname);
  end;
end;


procedure TKMNetworking.Disconnect;
begin
  fOnJoinSucc := nil;
  fOnJoinFail := nil;
  fOnTextMessage := nil;
  fOnPlayersSetup := nil;
  fOnMapName := nil;
  fOnCommands := nil;
  fOnDisconnect := nil;
  fOnPing := nil;

  fNetPlayers.Clear;

  fNetClient.Disconnect;
  if fLANPlayerKind = lpk_Host then
    fNetServer.StopListening;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetClient.Connected;
end;


//Tell other players which map we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectMap(aName:string);
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can select maps');

  fMapInfo.Load(aName, true);
  fNetPlayers.ResetLocAndReady; //Reset start locations

  if not fMapInfo.IsValid then exit;

  PacketToAll(mk_MapSelect, fMapInfo.Folder); //todo: Send map name and CRC
  fNetPlayers[fMyIndex].ReadyToStart := true;

  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
  if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
end;


//Tell other players which start position we would like to use
//Each players choice should be unique
procedure TKMNetworking.SelectLoc(aIndex:integer);
begin
  //Check if position can be taken before sending
  if (not fMapInfo.IsValid) or
     (aIndex > fMapInfo.PlayerCount) or
     (not fNetPlayers.LocAvailable(aIndex)) then
  begin
    if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
    exit;
  end;

  //Host makes rules, Joiner will get confirmation from Host
  fNetPlayers[fMyIndex].StartLocID := aIndex;

  case fLANPlayerKind of
    lpk_Host:   begin
                  PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: PacketToHost(mk_StartingLocQuery, inttostr(aIndex) + fMyNikname);
  end;
end;


//Tell other players which color we will be using
//For now players colors are not unique, many players may have one color
procedure TKMNetworking.SelectColor(aIndex:integer);
begin
  if not fNetPlayers.ColorAvailable(aIndex) then exit;

  //Host makes rules, Joiner will get confirmation from Host
  fNetPlayers[fMyIndex].FlagColorID := aIndex;

  case fLANPlayerKind of
    lpk_Host:   begin
                  PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: begin
                  PacketToHost(mk_FlagColorQuery, inttostr(aIndex) + fMyNikname);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
  end;
end;


//Joiner indicates that he is ready to start
procedure TKMNetworking.ReadyToStart;
begin
  PacketToHost(mk_ReadyToStart, fMyNikname);
end;


function TKMNetworking.CanStart:boolean;
begin
  Result := (fNetPlayers.Count > 1) and fNetPlayers.AllReady and fMapInfo.IsValid;
end;


//Tell other players we want to start
procedure TKMNetworking.StartClick;
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can start the game');

  //Do not allow to start the game
  if not fNetPlayers.AllReady then
  begin
    fOnTextMessage('Can not start: Not everyone is ready to start');
    exit;
  end;

  //Host can not miss a starting location!
  if fNetPlayers.Count > fMapInfo.Playercount then
  begin
    fOnTextMessage('Can not start: Player count exceeds map limit');
    exit;
  end;

  //Define random parameters (start locations and flag colors)
  //This will also remove odd players from the List, they will loose Host in few seconds
  fNetPlayers.DefineSetup(fMapInfo.PlayerCount);

  //Let everyone start with final version of fNetPlayers
  PacketToAll(mk_Start, fNetPlayers.GetAsText);

  StartGame;
end;


procedure TKMNetworking.Ping;
var i:integer;
begin
  for i:=1 to fNetPlayers.Count do
    fNetPlayers[i].PingSent := GetTickCount;

  PacketToAll(mk_Ping, fMyNikname);
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketToAll(mk_Text, MyIPString + '/' + fMyNikname + ': ' + aText);
  //fOnTextMessage(MyIPString + '/' + fMyNikname + ': ' + aText);
end;


//Send our commands to either to all players, or to specified one
procedure TKMNetworking.SendCommands(aStream:TKMemoryStream; aPlayerLoc:byte=0);
var i:integer;
begin
  if aPlayerLoc = 0 then
    PacketToAll(mk_Commands, aStream.ReadAsText) //Send commands to all players
  else
  for i:=1 to fNetPlayers.Count do //todo: optimize and error-check
    if fNetPlayers[i].StartLocID = aPlayerLoc then
      PacketSend(fNetPlayers[i].Address, mk_Commands, aStream.ReadAsText);
end;


procedure TKMNetworking.GameCreated;
begin
  case fLANPlayerKind of
    lpk_Host:   fNetPlayers[fMyIndex].ReadyToPlay := true;
    lpk_Joiner: PacketToHost(mk_ReadyToPlay, fMyNikname);
  end;
end;


procedure TKMNetworking.PacketRecieve(aData:pointer; aLength:cardinal);
var
  Kind:TMessageKind;
  M:TKMemoryStream;
  Msg:string;
  ReMsg:string;
  LocID,ColorID:integer;
  NikID:integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData, aLength);
  M.Read(Kind, SizeOf(TMessageKind));

  case Kind of
    mk_AskToJoin:
            if fLANPlayerKind = lpk_Host then
            begin
              M.Read(Msg);
              ReMsg := fNetPlayers.CheckCanJoin(Msg);
              if ReMsg = '' then
                PacketSend('', mk_AllowToJoin, 'Allowed')
              else
                PacketSend('', mk_RefuseToJoin, ReMsg);
            end;

    mk_AllowToJoin:
            if fLANPlayerKind = lpk_Joiner then
            begin
              PacketToHost(mk_VerifyJoin, fMyNikname);
              fOnJoinSucc(Self);
            end;

    mk_RefuseToJoin:
            if fLANPlayerKind = lpk_Joiner then
            begin
              M.Read(Msg);
              fNetClient.OnRecieveData := nil;
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_VerifyJoin:
            if fLANPlayerKind = lpk_Host then
            begin
              M.Read(Msg);
              fNetPlayers.AddPlayer('', Msg);
              PacketSend('', mk_MapSelect, fMapInfo.Folder); //Send the map first so it doesn't override starting locs
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              PostMessage(Msg+' has joined');
            end;

    mk_Disconnect:
            if fLANPlayerKind = lpk_Host then
            begin
              M.Read(Msg);
              fNetPlayers.RemPlayer(fNetPlayers.NiknameIndex(Msg));
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              PostMessage(Msg+' quit');
            end;

    mk_HostDisconnect:
            if fLANPlayerKind = lpk_Joiner then
            begin
              fNetClient.OnRecieveData := nil;
              fNetClient.Disconnect;
              if Assigned(fOnDisconnect) then
                fOnDisconnect('The host quit');
            end;

    {mk_Poke:
            begin
              M.Read(Msg);
              Assert(fNetPlayers.NiknameIndex(Msg) <> -1, 'Poked by an unknown player: '+Msg);
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].TimeTick := GetTickCount + REPLY_TIMEOUT;
            end;}

    mk_Ping:
            begin
              M.Read(Msg);
              PacketSend(fNetPlayers[fNetPlayers.NiknameIndex(Msg)].Address, mk_Pong, fMyNikname);
            end;

    mk_Pong:
            begin
              M.Read(Msg);
              NikID := fNetPlayers.NiknameIndex(Msg);
              fNetPlayers[NikID].Ping := GetTickCount - fNetPlayers[NikID].PingSent;
              if Assigned(fOnPing) then fOnPing(Self);
            end;

    mk_PlayersList:
            if fLANPlayerKind = lpk_Joiner then begin
              M.Read(Msg);
              fNetPlayers.SetAsText(Msg); //Our index could have changed on players add/removal
              fMyIndex := fNetPlayers.NiknameIndex(fMyNikname);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_MapSelect:
            if fLANPlayerKind = lpk_Joiner then begin
              M.Read(Msg);
              fMapInfo.Load(Msg, true);
              fNetPlayers.ResetLocAndReady; //We can ignore Hosts "Ready" flag for now
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_StartingLocQuery:
            if fLANPlayerKind = lpk_Host then begin
              M.Read(Msg);
              LocID := strtoint(Msg[1]); //Location index
              NikID := fNetPlayers.NiknameIndex(RightStr(Msg, length(Msg)-1)); //Player index
              //Check if position can't be taken
              if fMapInfo.IsValid and
                 (LocID <= fMapInfo.PlayerCount) and
                 fNetPlayers.LocAvailable(LocID) then
              begin //Update Players setup
                fNetPlayers[NikID].StartLocID := LocID;
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              end
              else //Quietly refuse
                PacketSend(fNetPlayers[NikID].Address, mk_PlayersList, fNetPlayers.GetAsText);
            end;

    mk_FlagColorQuery:
            if fLANPlayerKind = lpk_Host then begin
              M.Read(Msg);
              ColorID := strtoint(Msg[1]); //Color index
              NikID := fNetPlayers.NiknameIndex(RightStr(Msg, length(Msg)-1)); //Player index
              //The player list could have changed since the joiner sent this request (over slow connection)
              if fNetPlayers.ColorAvailable(ColorID) then
              begin
                fNetPlayers[NikID].FlagColorID := ColorID;
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              end
              else //Quietly refuse
                PacketSend(fNetPlayers[NikID].Address, mk_PlayersList, fNetPlayers.GetAsText);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ReadyToStart:
            if fLANPlayerKind = lpk_Host then begin
              M.Read(Msg);
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].ReadyToStart := true;
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_Start:
            if fLANPlayerKind = lpk_Joiner then begin
              M.Read(Msg);
              fNetPlayers.SetAsText(Msg);
              fMyIndex := fNetPlayers.NiknameIndex(fMyNikname);
              StartGame;
            end;

    mk_ReadyToPlay:
            if fLANPlayerKind = lpk_Host then begin
              M.Read(Msg);
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].ReadyToPlay := true;
              if fNetPlayers.AllReadyToPlay then
              begin
                PacketToAll(mk_Play,'+'); //todo: Should include lag difference
                if Assigned(fOnPlay) then fOnPlay(Self);
              end;
            end;

    mk_Play:
            if fLANPlayerKind = lpk_Joiner then
              if Assigned(fOnPlay) then fOnPlay(Self);

    mk_Commands:
            begin
              M.Read(Msg);
              if Assigned(fOnCommands) then fOnCommands(Msg);
            end;

    mk_Text:
            begin
              M.Read(Msg);
              if Assigned(fOnTextMessage) then fOnTextMessage(Msg);
            end;
  end;
end;


procedure TKMNetworking.PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.Write(aKind, SizeOf(TMessageKind));
  M.Write(aData);
  fNetClient.SendData(M.Memory, M.Size);
  M.Free;
end;


procedure TKMNetworking.PacketToAll(aKind:TMessageKind; const aData:string='');
begin
  PacketSend('', aKind, aData);
end;


procedure TKMNetworking.PacketToHost(aKind:TMessageKind; const aData:string='');
begin
  Assert(fLANPlayerKind = lpk_Joiner, 'Only joined player can send data to Host');
  PacketSend(fHostAddress, aKind, aData);
end;


procedure TKMNetworking.StartGame;
begin
  if Assigned(fOnStartGame) then
    fOnStartGame(Self);
end;


procedure TKMNetworking.UpdateState;
//var LostPlayers:string;
begin
{
  if (fJoinTick<>0) and (GetTickCount > fJoinTick) then
  begin
    fJoinTick := 0;
    fNetwork.OnRecieveKMPacket := nil;
    fNetwork.StopListening;
    fOnJoinFail('no response');
  end;

  if not Connected then Exit;

  //Test once per half of REPLY_TIMEOUT
  if not fHoldTimeoutChecks then
  if GetTickCount > fLastUpdateTick + (REPLY_TIMEOUT div 2) then
  begin
    case fLANPlayerKind of
      lpk_Joiner:
          begin//Joiner checks if Host is lost
            if (GetTickCount > fNetPlayers[1].TimeTick) and (fNetPlayers[1].TimeTick <> 0) then
            begin
              fNetwork.OnRecieveKMPacket := nil;
              fNetwork.StopListening;
              if Assigned(fOnDisconnect) then fOnDisconnect('Lost connection to Host');
              exit;
            end;
            //Do not send pokes if we are still waiting to recieve the player list
            if fNetPlayers.NiknameIndex(fMyNikname) <> -1 then
              PacketToHost(mk_Poke, fMyNikname); //Tell Host we are still connected
          end;
      lpk_Host:
          begin
            LostPlayers := fNetPlayers.DropMissing(GetTickCount);
            if LostPlayers <> '' then
            begin
              //LostPlayers won't recieve Host messages any more and will loose connection too
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              PostMessage(LostPlayers + ' disconnected');
            end;
            PacketToAll(mk_Poke, fMyNikname); //Tell everyone Host is still running
          end;
    end;
    fLastUpdateTick := GetTickCount;
  end;}
end;


end.
