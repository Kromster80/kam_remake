unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, {$ENDIF}
  Classes, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Player,
  KM_MapInfo, KM_NetPlayersList, KM_NetServer, KM_NetClient;

//todo: Check CRCs of important game data files (units.dat, houses.dat, etc.) to make sure all clients match

type
  TLANPlayerKind = (lpk_Host, lpk_Joiner);
  TLANGameState = (lgs_Lobby, lgs_Game);

  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
  private
    fNetServer:TKMNetServer;
    fNetClient:TKMNetClient;
    fLANPlayerKind: TLANPlayerKind; //Our role (Host or Joiner)
    fLANGameState: TLANGameState;
    fHostAddress:string;
    fMyNikname:string;
    fMyIndexOnServer:integer;
    fMyIndex:integer; //In NetPlayers list
    fNetPlayers:TKMPlayersList;

    fMapInfo:TKMapInfo; //Everything related to selected map

    fOnJoinSucc:TNotifyEvent;
    fOnJoinFail:TStringEvent;
    fOnJoinAssignedHost:TNotifyEvent;
    fOnHostFail:TStringEvent;
    fOnReassignedHost:TNotifyEvent;
    fOnTextMessage:TStringEvent;
    fOnPlayersSetup:TNotifyEvent;
    fOnMapName:TStringEvent;
    fOnStartGame:TNotifyEvent;
    fOnPlay:TNotifyEvent;
    fOnReadyToPlay:TNotifyEvent;
    fOnDisconnect:TStringEvent;
    fOnPingInfo:TNotifyEvent;
    fOnCommands:TStringEvent;

    procedure DecodePingInfo(aInfo:string);
    procedure ForcedDisconnect(const S: string);
    procedure StartGame;

    procedure ConnectSucceed(Sender:TObject);
    procedure ConnectFailed(const S: string);
    procedure PacketRecieve(aSenderIndex:integer; aData:pointer; aLength:cardinal); //Process all commands
    procedure PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
  public
    constructor Create;
    destructor Destroy; override;

    property MyIndex:integer read fMyIndex;
    function MyIPString:string;
    function IsHost:boolean;
    property LANGameState: TLANGameState read fLANGameState write fLANGameState;

    //Lobby
    procedure Host(aUserName:string);
    procedure Join(aServerAddress,aUserName:string);
    procedure LeaveLobby;
    procedure Disconnect;
    function  Connected: boolean;
    procedure SendMapOrSave;
    procedure MatchPlayersToSave(aPlayerID:integer=-1);
    procedure SelectNoMap;
    procedure SelectMap(aName:string);
    procedure SelectSave(aSlot:integer);
    procedure SelectLoc(aIndex:integer; aPlayerIndex:integer);
    procedure SelectTeam(aIndex:integer; aPlayerIndex:integer);
    procedure SelectColor(aIndex:integer; aPlayerIndex:integer);
    function ReadyToStart:boolean;
    function CanStart:boolean;
    procedure StartClick; //All required arguments are in our class
    procedure SendPlayerListAndRefreshPlayersSetup(aPlayerIndex:integer = NET_ADDRESS_OTHERS);

    //Common
    procedure PostMessage(aText:string; aShowName:boolean=false);

    //Gameplay
    property MapInfo:TKMapInfo read fMapInfo;
    property NetPlayers:TKMPlayersList read fNetPlayers;
    procedure GameCreated;
    procedure SendCommands(aStream:TKMemoryStream; aPlayerIndex:TPlayerIndex=-1);

    property OnJoinSucc:TNotifyEvent write fOnJoinSucc;         //We were allowed to join
    property OnJoinFail:TStringEvent write fOnJoinFail;         //We were refused to join
    property OnHostFail:TStringEvent write fOnHostFail;         //Server failed to start (already running a server?)
    property OnJoinAssignedHost:TNotifyEvent write fOnJoinAssignedHost; //We were assigned hosting rights upon connection
    property OnReassignedHost:TNotifyEvent write fOnReassignedHost;     //We were reassigned hosting rights when the host quit

    property OnPlayersSetup:TNotifyEvent write fOnPlayersSetup; //Player list updated
    property OnMapName:TStringEvent write fOnMapName;           //Map name updated
    property OnStartGame:TNotifyEvent write fOnStartGame;       //Start the game loading
    property OnPlay:TNotifyEvent write fOnPlay;                 //Start the gameplay
    property OnReadyToPlay:TNotifyEvent write fOnReadyToPlay;   //Update the list of players ready to play
    property OnPingInfo:TNotifyEvent write fOnPingInfo;         //Ping info updated

    property OnDisconnect:TStringEvent write fOnDisconnect;     //Lost connection, was kicked
    property OnCommands:TStringEvent write fOnCommands;         //Recieved GIP commands

    property OnTextMessage:TStringEvent write fOnTextMessage;   //Text message recieved

    procedure UpdateState(aTick: cardinal);
  end;


implementation


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


function TKMNetworking.IsHost:boolean;
begin
  Result := (fLANPlayerKind = lpk_Host);
end;


//Startup a local server and connect to it as ordinary client
procedure TKMNetworking.Host(aUserName:string);
begin
  fNetServer.StopListening;

  fNetServer.OnStatusMessage := fOnTextMessage;
  try
    fNetServer.StartListening(KAM_PORT);
  except
    on E : Exception do
    begin
      //Server failed to start
      fOnHostFail(E.ClassName+': '+E.Message);
      fNetServer.StopListening;
      exit;
    end;
  end;
  Join('127.0.0.1', aUserName); //Server will assign hosting rights to us as we are the first joiner
end;


procedure TKMNetworking.Join(aServerAddress,aUserName:string);
begin
  Assert(not fNetClient.Connected, 'We were not properly disconnected');

  fMyIndex := -1; //Host will send us PlayerList and we will get our index from there
  fMyIndexOnServer := -1; //Assigned by Server
  fLANGameState := lgs_Lobby; //Game starts in the lobby

  fHostAddress := aServerAddress;
  fMyNikname := aUserName;
  fLANPlayerKind := lpk_Joiner;

  fNetClient.OnRecieveData := PacketRecieve;
  fNetClient.OnConnectSucceed := ConnectSucceed;
  fNetClient.OnConnectFailed := ConnectFailed;
  fNetClient.OnForcedDisconnect := ForcedDisconnect;
  fNetClient.OnStatusMessage := fOnTextMessage;
  fNetClient.ConnectTo(fHostAddress, KAM_PORT);
end;


//Connection was successful, but we still need mk_IndexOnServer to be able to do anything
procedure TKMNetworking.ConnectSucceed(Sender:TObject);
begin
  if Assigned(fOnTextMessage) then
    fOnTextMessage('Connection successful');
end;


procedure TKMNetworking.ConnectFailed(const S: string);
begin
  fNetClient.Disconnect;
  fOnJoinFail(S);
end;


//Send message that we have deliberately disconnected
procedure TKMNetworking.LeaveLobby;
begin
  if IsHost then
    PacketSend(NET_ADDRESS_OTHERS, mk_Disconnect, '', 0) //Host tells everyone when they quit
  else
    PacketSend(NET_ADDRESS_HOST, mk_Disconnect, '', 0); //Joiners should only tell host when they quit
end;


procedure TKMNetworking.Disconnect;
begin
  fOnJoinSucc := nil;
  fOnJoinFail := nil;
  fOnJoinAssignedHost := nil;
  fOnHostFail := nil;
  fOnTextMessage := nil;
  fOnPlayersSetup := nil;
  fOnMapName := nil;
  fOnCommands := nil;
  fOnDisconnect := nil;
  fOnPingInfo := nil;
  fOnReassignedHost := nil;

  fNetPlayers.Clear;
  fNetClient.Disconnect;
  fNetServer.StopListening;
  fNetServer.ClearClients;

  fMapInfo.Free;
  fMapInfo := TKMapInfo.Create;
end;


procedure TKMNetworking.ForcedDisconnect(const S: string);
begin
  fOnDisconnect(S);
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetClient.Connected;
end;


procedure TKMNetworking.DecodePingInfo(aInfo:string);
var
  i:integer;
  M:TKMemoryStream;
  PingCount:integer;
  PlayerHandle:integer;
  PingValue:word;
  LocalHandle:integer;
begin
  M := TKMemoryStream.Create;
  M.WriteAsText(aInfo);
  M.Position := 0;
  M.Read(PingCount);
  for i:=1 to PingCount do
  begin
    M.Read(PlayerHandle);
    LocalHandle := fNetPlayers.ServerToLocal(PlayerHandle);
    M.Read(PingValue);
    //This player might not be in the lobby yet, could still be asking to join. If so we do not care about their ping.
    if LocalHandle <> -1 then
      fNetPlayers[LocalHandle].SetPing(PingValue);
  end;
  M.Free;
end;


procedure TKMNetworking.SendMapOrSave;
begin
  if fMapInfo.IsSave then
  begin
    PacketSend(NET_ADDRESS_OTHERS, mk_SaveSelect, '', fMapInfo.SaveSlot);
    PacketSend(NET_ADDRESS_OTHERS, mk_SaveCRC, '', Integer(fMapInfo.CRC));
  end
  else
  begin
    PacketSend(NET_ADDRESS_OTHERS, mk_MapSelect, fMapInfo.Folder, 0);
    PacketSend(NET_ADDRESS_OTHERS, mk_MapCRC, '', Integer(fMapInfo.CRC));
  end;
end;


procedure TKMNetworking.MatchPlayersToSave(aPlayerID:integer=-1);
var i,k: integer;
begin
  Assert(IsHost, 'Only host can match players');
  Assert(fMapInfo.IsSave, 'Not a save');
  //If we are matching all then reset them all first so we don't get clashes
  if aPlayerID = -1 then
    for i:=1 to fNetPlayers.Count do
      fNetPlayers[i].StartLocation := 0;

  //Add enough AI players automatically (when we are matching all)
  if aPlayerID = -1 then
    for i:=fNetPlayers.GetAICount to fMapInfo.GetAICount-1 do
      fNetPlayers.AddAIPlayer;

  for i:=1 to fNetPlayers.Count do
    for k:=1 to fMapInfo.PlayerCount do
      if (i = aPlayerID) or (aPlayerID = -1) then //-1 means update all players
        if fNetPlayers.LocAvailable(k) then
        begin
          if ((fNetPlayers[i].PlayerType = pt_Computer) and (fMapInfo.PlayerTypes[k-1] = pt_Computer))
          or (fNetPlayers[i].Nikname = fMapInfo.LocationName[k-1]) then
            fNetPlayers[i].StartLocation := k;
        end;
end;


procedure TKMNetworking.SelectNoMap;
begin
  Assert(IsHost, 'Only host can reset map');
  fMapInfo.Free;
  fMapInfo := TKMapInfo.Create;
  PacketSend(NET_ADDRESS_OTHERS, mk_ResetMap, '', 0);
  fNetPlayers.ResetLocAndReady; //Reset start locations
  fNetPlayers[fMyIndex].ReadyToStart := true;
  if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which map we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectMap(aName:string);
begin
  Assert(IsHost, 'Only host can select maps');

  fMapInfo.Load(aName, true);

  if not fMapInfo.IsValid then SelectNoMap;
  fNetPlayers.ResetLocAndReady; //Reset start locations

  SendMapOrSave;

  fNetPlayers[fMyIndex].ReadyToStart := true;

  if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which save we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectSave(aSlot:integer);
begin
  Assert(IsHost, 'Only host can select saves');

  fMapInfo.LoadSavedGame(aSlot, true, true);

  if not fMapInfo.IsValid then SelectNoMap;
  fNetPlayers.ResetLocAndReady; //Reset start locations

  SendMapOrSave;
  if fMapInfo.IsSave then MatchPlayersToSave; //Don't match players if it's not a valid save

  fNetPlayers[fMyIndex].ReadyToStart := true;

  if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which start position we would like to use
//Each players choice should be unique
procedure TKMNetworking.SelectLoc(aIndex:integer; aPlayerIndex:integer);
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
  fNetPlayers[aPlayerIndex].StartLocation := aIndex; //Use aPlayerIndex not fMyIndex because it could be an AI

  case fLANPlayerKind of
    lpk_Host:   SendPlayerListAndRefreshPlayersSetup;
    lpk_Joiner: PacketSend(NET_ADDRESS_HOST, mk_StartingLocQuery, '', aIndex);
  end;
end;


//Tell other players which team we are on. Player selections need not be unique
procedure TKMNetworking.SelectTeam(aIndex:integer; aPlayerIndex:integer);
begin
  fNetPlayers[aPlayerIndex].Team := aIndex; //Use aPlayerIndex not fMyIndex because it could be an AI

  case fLANPlayerKind of
    lpk_Host:   SendPlayerListAndRefreshPlayersSetup;
    lpk_Joiner: PacketSend(NET_ADDRESS_HOST, mk_SetTeam, '', aIndex);
  end;
end;


//Tell other players which color we will be using
//For now players colors are not unique, many players may have one color
procedure TKMNetworking.SelectColor(aIndex:integer; aPlayerIndex:integer);
begin
  if not fNetPlayers.ColorAvailable(aIndex) then exit;

  //Host makes rules, Joiner will get confirmation from Host
  fNetPlayers[aPlayerIndex].FlagColorID := aIndex; //Use aPlayerIndex not fMyIndex because it could be an AI

  case fLANPlayerKind of
    lpk_Host:   SendPlayerListAndRefreshPlayersSetup;
    lpk_Joiner: begin
                  PacketSend(NET_ADDRESS_HOST, mk_FlagColorQuery, '', aIndex);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
  end;
end;


//Joiner indicates that he is ready to start
function TKMNetworking.ReadyToStart:boolean;
begin
  if fMapInfo.IsValid then
  begin
    PacketSend(NET_ADDRESS_HOST, mk_ReadyToStart, '', 0);
    Result := true;
  end
  else
  begin
    if Assigned(fOnTextMessage) then fOnTextMessage('Error: Map failed to load');
    Result := false;
  end;
end;


function TKMNetworking.CanStart:boolean;
var i:integer;
begin
  Result := (fNetPlayers.Count > 1) and fNetPlayers.AllReady and fMapInfo.IsValid;
  if fMapInfo.IsSave then
    for i:=1 to fNetPlayers.Count do
      Result := Result and (fNetPlayers[i].StartLocation <> 0); //In saves everyone must chose a location
end;


//Tell other players we want to start
procedure TKMNetworking.StartClick;
begin
  Assert(IsHost, 'Only host can start the game');

  //Do not allow to start the game
  if not fNetPlayers.AllReady then
  begin
    fOnTextMessage('Can not start: Not everyone is ready to start');
    exit;
  end;

  //Host can not miss a starting location!
  if fNetPlayers.Count > fMapInfo.PlayerCount then
  begin
    fOnTextMessage('Cannot start: Player count exceeds map limit');
    exit;
  end;

  //Define random parameters (start locations and flag colors)
  //This will also remove odd players from the List, they will lose Host in few seconds
  fNetPlayers.DefineSetup(fMapInfo.PlayerCount);

  //Let everyone start with final version of fNetPlayers

  //@Krom: If the server is local then this message will not be recieved by the other players until we have finished this process
  //       because the server will not process and resend the message until then. Adding Application.ProcessMessage; here would fix
  //       it, but possibly cause other problems. (using ProcessMessages is frowned upon because it can create recursive calls)
  //       This is the reason why the server loads the map first. Maybe the NetServer should run on a seperate thread to prevent
  //       the host from locking it up under any circumstances?
  //@Lewin: Thats low priority, but yes, separate thread would be good solution AFAIK
  //todo: TKMNetServer should run in a seperate thread to make it update even while the client (player) is busy (see above)

  PacketSend(NET_ADDRESS_OTHERS, mk_Start, fNetPlayers.GetAsText, 0);

  StartGame;
end;


procedure TKMNetworking.SendPlayerListAndRefreshPlayersSetup(aPlayerIndex:integer = NET_ADDRESS_OTHERS);
var i:integer;
begin
  Assert(IsHost, 'Only host can send player list');

  //In saves we should load team and color from the MapInfo
  if MapInfo.IsSave then
    for i:=1 to NetPlayers.Count do
      if NetPlayers[i].StartLocation <> 0 then
      begin
        NetPlayers[i].FlagColorID := MapInfo.ColorID[NetPlayers[i].StartLocation-1];
        NetPlayers[i].Team := MapInfo.Team[NetPlayers[i].StartLocation-1];
      end
      else
      begin
        NetPlayers[i].FlagColorID := 0;
        NetPlayers[i].Team := 0;
      end;

  PacketSend(aPlayerIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
end;


//We route the message through Server to ensure everyone sees messages in the same order
//with exact same timestamps (possibly added by Server?)
procedure TKMNetworking.PostMessage(aText:string; aShowName:boolean=false);
begin
  if aShowName then aText := fMyNikname+': '+aText;
  PacketSend(NET_ADDRESS_ALL, mk_Text, aText, 0);
end;


//Send our commands to either to all players, or to specified one
procedure TKMNetworking.SendCommands(aStream:TKMemoryStream; aPlayerIndex:TPlayerIndex=-1);
var i:integer;
begin
  if aPlayerIndex = -1 then
    PacketSend(NET_ADDRESS_OTHERS, mk_Commands, aStream.ReadAsText, 0) //Send commands to all players
  else
  for i:=1 to fNetPlayers.Count do
    if fNetPlayers[i].PlayerIndex.PlayerIndex = aPlayerIndex then
      PacketSend(fNetPlayers[i].IndexOnServer, mk_Commands, aStream.ReadAsText, 0);
end;


procedure TKMNetworking.GameCreated;
begin
  case fLANPlayerKind of
    lpk_Host:   begin
                  fNetPlayers[fMyIndex].ReadyToPlay := true;
                  PacketSend(NET_ADDRESS_OTHERS, mk_ReadyToPlay, '', 0);
                  //Check this here because it is possible to start a multiplayer game without other humans, just AI (at least for debugging)
                  if fNetPlayers.AllReadyToPlay then
                  begin
                    PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0); //todo: Should include lag difference
                    if Assigned(fOnPlay) then fOnPlay(Self);
                  end;
                end;
    lpk_Joiner: begin
                  fNetPlayers[fMyIndex].ReadyToPlay := true;
                  PacketSend(NET_ADDRESS_OTHERS, mk_ReadyToPlay, '', 0);
                end;
  end;
end;


procedure TKMNetworking.PacketRecieve(aSenderIndex:integer; aData:pointer; aLength:cardinal);
var
  Kind:TKMessageKind;
  M:TKMemoryStream;
  Param:integer;
  Msg:string;
  ReMsg:string;
  LocID,TeamID,ColorID:integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(Kind, SizeOf(TKMessageKind));
  case NetPacketType[Kind] of
    pfNumber: M.Read(Param);
    pfText:   M.Read(Msg);
  end;
  M.Free;

  case Kind of
    mk_GameVersion:
            begin
              if Msg <> GAME_REVISION then
              begin
                Assert(not IsHost);
                fOnJoinFail('Wrong game version: '+GAME_REVISION+'. Server uses: '+Msg);
                fNetClient.Disconnect;
                exit;
              end;
            end;

    mk_HostingRights:
            begin
              fLANPlayerKind := lpk_Host;
              if Assigned(fOnJoinAssignedHost) then fOnJoinAssignedHost(Self); //Enter the lobby if we had hosting rights assigned to us
              if Assigned(fOnTextMessage) then fOnTextMessage('Server has assigned hosting rights to us');
            end;

    mk_IndexOnServer:
            begin
              fMyIndexOnServer := Param;
              if Assigned(fOnTextMessage) then fOnTextMessage('Index on Server - ' + inttostr(fMyIndexOnServer));
              case fLANPlayerKind of
                lpk_Host:
                    begin
                      fNetPlayers.AddPlayer(fMyNikname, fMyIndexOnServer);
                      fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
                      fNetPlayers[fMyIndex].ReadyToStart := true;
                      if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                    end;
                lpk_Joiner:
                    PacketSend(NET_ADDRESS_HOST, mk_AskToJoin, fMyNikname, 0);
              end;
            end;

    mk_AskToJoin:
            if IsHost then begin
              ReMsg := fNetPlayers.CheckCanJoin(Msg, aSenderIndex);
              if (ReMsg = '') and (fLANGameState = lgs_Game) then
                ReMsg := 'Cannot join while the game is in progress';
              if ReMsg = '' then
              begin
                fNetPlayers.AddPlayer(Msg, aSenderIndex);
                PacketSend(aSenderIndex, mk_AllowToJoin, '', 0);
                SendMapOrSave; //Send the map first so it doesn't override starting locs
                if fMapInfo.IsSave then MatchPlayersToSave(fNetPlayers.ServerToLocal(aSenderIndex)); //Match only this player
                SendPlayerListAndRefreshPlayersSetup;
                PostMessage(Msg+' has joined');
              end
              else
                PacketSend(aSenderIndex, mk_RefuseToJoin, ReMsg, 0);
            end;

    mk_AllowToJoin:
            if fLANPlayerKind = lpk_Joiner then
            begin
              fOnJoinSucc(Self); //Enter lobby
            end;

    mk_RefuseToJoin:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_ClientLost:
            if IsHost then
            begin
              if fNetPlayers.ServerToLocal(Param) = -1 then exit; //Has already disconnected
              PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(Param)].Nikname+' lost connection');
              fNetPlayers.RemPlayer(Param);
              SendPlayerListAndRefreshPlayersSetup;
            end
            else
              if fNetPlayers.ServerToLocal(Param) <> -1 then
                fNetPlayers.RemPlayer(Param); //Remove the player anyway as it might be the host that was lost

    mk_Disconnect:
            case fLANPlayerKind of
              lpk_Host:
                  begin
                    if fNetPlayers.ServerToLocal(aSenderIndex) = -1 then exit; //Has already disconnected
                    PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Nikname+' has quit');
                    fNetPlayers.RemPlayer(aSenderIndex);
                    SendPlayerListAndRefreshPlayersSetup;
                  end;
              lpk_Joiner:
                  begin
                    if fNetPlayers.ServerToLocal(aSenderIndex) = -1 then exit; //Has already disconnected
                    if Assigned(fOnTextMessage) then fOnTextMessage('The host has disconnected');
                    fNetPlayers.RemPlayer(aSenderIndex);
                  end;
            end;

    mk_ReassignHost:
            begin
              if Param = fMyIndexOnServer then
              begin
                //We are now the host
                fLANPlayerKind := lpk_Host;
                fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
                if Assigned(fOnReassignedHost) then fOnReassignedHost(Self); //Lobby/game might need to know that we are now hosting
                fNetPlayers[fMyIndex].ReadyToStart := true; //The host is always ready
                fNetPlayers.SetAIReady; //Set all AI players to ready
                SendPlayerListAndRefreshPlayersSetup;
                PostMessage('Hosting rights reassigned to '+fMyNikname);
              end;
            end;

    mk_Ping:
            begin
              PacketSend(aSenderIndex, mk_Pong, '', 0); //Server will intercept this message
            end;

    mk_PingInfo:
            begin
              DecodePingInfo(Msg);
              if Assigned(fOnPingInfo) then fOnPingInfo(Self);
            end;

    mk_PlayersList:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg); //Our index could have changed on players add/removal
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ResetMap:
            begin
              fMapInfo.Free;
              fMapInfo := TKMapInfo.Create;
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
            end;

    mk_MapSelect:
            if fLANPlayerKind = lpk_Joiner then begin
              fMapInfo.Load(Msg, true);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_MapCRC:
            if fLANPlayerKind = lpk_Joiner then
            begin
              if Integer(fMapInfo.CRC) <> Param then
              begin
                if fMapInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the map '+fMapInfo.Folder)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the map '+fMapInfo.Folder);
                fMapInfo.Free;
                fMapInfo := TKMapInfo.Create;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := false;
                if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              end
            end;

    mk_SaveSelect:
            if fLANPlayerKind = lpk_Joiner then begin
              fMapInfo.LoadSavedGame(Param, true, true);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_SaveCRC:
            if fLANPlayerKind = lpk_Joiner then
            begin
              if Integer(fMapInfo.CRC) <> Param then
              begin
                if fMapInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the save '+fMapInfo.Folder)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the save '+fMapInfo.Folder);
                fMapInfo.Free;
                fMapInfo := TKMapInfo.Create;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := false;
                if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              end
            end;

    mk_StartingLocQuery:
            if IsHost then begin
              LocID := Param;
              if fMapInfo.IsValid and
                 (LocID <= fMapInfo.PlayerCount) and
                 fNetPlayers.LocAvailable(LocID) then
              begin //Update Players setup
                fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].StartLocation := LocID;
                SendPlayerListAndRefreshPlayersSetup;
              end
              else //Quietly refuse
                SendPlayerListAndRefreshPlayersSetup(aSenderIndex);
            end;

    mk_SetTeam:
            if IsHost then begin
              TeamID := Param;
              //Update Players setup
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Team := TeamID;
              SendPlayerListAndRefreshPlayersSetup;
            end;

    mk_FlagColorQuery:
            if IsHost then begin
              ColorID := Param;
              //The player list could have changed since the joiner sent this request (over slow connection)
              if fNetPlayers.ColorAvailable(ColorID) then
              begin
                fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].FlagColorID := ColorID;
                SendPlayerListAndRefreshPlayersSetup;
              end
              else //Quietly refuse
                SendPlayerListAndRefreshPlayersSetup(aSenderIndex);
            end;

    mk_ReadyToStart:
            if IsHost then begin
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToStart := true;
              SendPlayerListAndRefreshPlayersSetup;
            end;

    mk_Start:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg);
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              StartGame;
            end;

    mk_ReadyToPlay:
            begin
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToPlay := true;
              if Assigned(fOnReadyToPlay) then fOnReadyToPlay(Self);
              if IsHost and fNetPlayers.AllReadyToPlay then
              begin
                PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0); //todo: Should include lag difference
                if Assigned(fOnPlay) then fOnPlay(Self);
              end;
            end;

    mk_Play:
            if fLANPlayerKind = lpk_Joiner then
              if Assigned(fOnPlay) then fOnPlay(Self);

    mk_Commands:
            if Assigned(fOnCommands) then fOnCommands(Msg);

    mk_Text:
            if Assigned(fOnTextMessage) then
              fOnTextMessage(Msg);
  end;
end;


//MessageKind.Data(depends on Kind)
procedure TKMNetworking.PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.Write(aKind, SizeOf(TKMessageKind));

  case NetPacketType[aKind] of
    pfNumber: M.Write(aParam);
    pfText:   M.Write(aText);
  end;

  fNetClient.SendData(fMyIndexOnServer, aRecipient, M.Memory, M.Size);
  M.Free;
end;


procedure TKMNetworking.StartGame;
begin
  if Assigned(fOnStartGame) then
    fOnStartGame(Self);
end;


procedure TKMNetworking.UpdateState(aTick: cardinal);
begin
  //Server should measure pings once per second
  if (fNetServer.Listening) and (aTick mod 10 = 0) then
    fNetServer.MeasurePings;
end;


end.
