unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Player, KM_Saves, KM_GameInfo,
  KM_MapInfo, KM_NetPlayersList, KM_DedicatedServer, KM_NetClient, KM_ServerQuery;

//todo: Check CRCs of important game data files (units.dat, houses.dat, etc.) to make sure all clients match

type
  TNetPlayerKind = (lpk_Host, lpk_Joiner);
  TNetGameState = (lgs_None, lgs_Connecting, lgs_Query, lgs_Lobby, lgs_Loading, lgs_Game);
  TNetGameKind = (ngk_None, ngk_Map, ngk_Save);

const
  NetGameStateText:array[TNetGameState] of string = ('None','Connecting','Query','Lobby','Loading','Game');
  NetAllowedPackets:array[TNetGameState] of set of TKMessageKind = (
  [], //lgs_None
  [mk_RefuseToJoin,mk_HostingRights,mk_IndexOnServer,mk_GameVersion,mk_WelcomeMessage,mk_Ping,mk_Pong,mk_ConnectedToRoom], //lgs_Connecting
  [mk_AllowToJoin,mk_RefuseToJoin,mk_Ping,mk_Pong,mk_PingInfo], //lgs_Query
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_Pong,mk_PingInfo,mk_PlayersList,
   mk_StartingLocQuery,mk_SetTeam,mk_FlagColorQuery,mk_ResetMap,mk_MapSelect,mk_MapCRC,mk_SaveSelect,
   mk_SaveCRC,mk_ReadyToStart,mk_Start,mk_Text], //lgs_Lobby
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_Pong,mk_PingInfo,mk_PlayersList,mk_ReadyToPlay,mk_Play,mk_Text], //lgs_Loading
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_Pong,mk_PingInfo,mk_PlayersList,mk_Commands,mk_Text] //lgs_Game
  );

  JOIN_TIMEOUT = 8000; //8 sec. Timeout for join queries


type
  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
  private
    fNetServer:TKMDedicatedServer;
    fNetClient:TKMNetClient;
    fServerQuery:TKMServerQuery;
    fNetPlayerKind: TNetPlayerKind; //Our role (Host or Joiner)
    fNetGameState: TNetGameState;
    fHostAddress:string;
    fMyNikname:string;
    fWelcomeMessage:string;
    fMyIndexOnServer:integer;
    fMyIndex:integer; //In NetPlayers list
    fRoomToJoin:integer; //The room we should join once we hear from the server
    fIgnorePings: integer; //During loading ping measurements will be high, so discard them. (when networking is threaded this might be unnecessary)
    fJoinTimeout:cardinal;
    fNetPlayers:TKMPlayersList;

    fMapInfo:TKMapInfo; //Everything related to selected map
    fSaveInfo: TKMSaveInfo;
    fSelectGameKind: TNetGameKind;

    fOnJoinSucc:TNotifyEvent;
    fOnJoinFail:TStringEvent;
    fOnJoinAssignedHost:TNotifyEvent;
    fOnHostFail:TStringEvent;
    fOnReassignedHost:TNotifyEvent;
    fOnTextMessage:TStringEvent;
    fOnPlayersSetup:TNotifyEvent;
    fOnMapName:TStringEvent;
    fOnStartMap:TStringEvent;
    fOnStartSave:TStringEvent;
    fOnPlay:TNotifyEvent;
    fOnReadyToPlay:TNotifyEvent;
    fOnDisconnect:TStringEvent;
    fOnPingInfo:TNotifyEvent;
    fOnCommands:TStringEvent;

    procedure DecodePingInfo(aInfo:string);
    procedure ForcedDisconnect(const S: string);
    procedure StartGame;
    procedure PlayGame;
    procedure SetGameState(aState:TNetGameState);
    procedure SendMapOrSave;
    function GetGameInfo:TKMGameInfo;

    procedure ConnectSucceed(Sender:TObject);
    procedure ConnectFailed(const S: string);
    procedure PacketRecieve(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal); //Process all commands
    procedure PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
  public
    constructor Create(const aMasterServerAddress:string; aKickTimeout, aPingInterval, aAnnounceInterval:word);
    destructor Destroy; override;

    property MyIndex:integer read fMyIndex;
    property NetGameState:TNetGameState read fNetGameState;
    function MyIPString:string;
    function IsHost:boolean;

    //Lobby
    property ServerQuery:TKMServerQuery read fServerQuery;
    procedure Host(aUserName,aServerName,aPort:string; aAnnounceServer:boolean);
    procedure Join(aServerAddress,aPort,aUserName:string; aRoom:integer);
    procedure LeaveLobby;
    procedure Disconnect;
    function  Connected: boolean;
    procedure MatchPlayersToSave(aPlayerID:integer=-1);
    procedure SelectNoMap;
    procedure SelectMap(const aName:string);
    procedure SelectSave(const aName:string);
    procedure SelectLoc(aIndex:integer; aPlayerIndex:integer);
    procedure SelectTeam(aIndex:integer; aPlayerIndex:integer);
    procedure SelectColor(aIndex:integer; aPlayerIndex:integer);
    function ReadyToStart:boolean;
    function CanStart:boolean;
    procedure StartClick; //All required arguments are in our class
    procedure SendPlayerListAndRefreshPlayersSetup(aPlayerIndex:integer = NET_ADDRESS_OTHERS);

    //Common
    procedure PostMessage(aText:string; aShowName:boolean=false; aTeamOnly:boolean=false);

    //Gameplay
    property MapInfo:TKMapInfo read fMapInfo;
    property SaveInfo:TKMSaveInfo read fSaveInfo;
    property GameInfo:TKMGameInfo read GetGameInfo;
    property SelectGameKind: TNetGameKind read fSelectGameKind;
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
    property OnStartMap:TStringEvent write fOnStartMap;       //Start the game
    property OnStartSave:TStringEvent write fOnStartSave;       //Load the game
    property OnPlay:TNotifyEvent write fOnPlay;                 //Start the gameplay
    property OnReadyToPlay:TNotifyEvent write fOnReadyToPlay;   //Update the list of players ready to play
    property OnPingInfo:TNotifyEvent write fOnPingInfo;         //Ping info updated

    property OnDisconnect:TStringEvent write fOnDisconnect;     //Lost connection, was kicked
    property OnCommands:TStringEvent write fOnCommands;         //Recieved GIP commands

    property OnTextMessage:TStringEvent write fOnTextMessage;   //Text message recieved

    procedure UpdateState(aTick: cardinal);
    procedure UpdateStateIdle;
  end;


implementation


{ TKMNetworking }
constructor TKMNetworking.Create(const aMasterServerAddress:string; aKickTimeout, aPingInterval, aAnnounceInterval:word);
begin
  Inherited Create;
  SetGameState(lgs_None);
  fNetServer := TKMDedicatedServer.Create(1, aKickTimeout, aPingInterval, aAnnounceInterval, aMasterServerAddress, '', '');
  fNetClient := TKMNetClient.Create;
  fNetPlayers := TKMPlayersList.Create;
  fServerQuery := TKMServerQuery.Create(aMasterServerAddress);
end;


destructor TKMNetworking.Destroy;
begin
  fNetPlayers.Free;
  fNetServer.Free;
  fNetClient.Free;
  fServerQuery.Free;
  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);
  Inherited;
end;


function TKMNetworking.MyIPString:string;
begin
  Result := fNetClient.MyIPString;
end;


function TKMNetworking.IsHost:boolean;
begin
  Result := (fNetPlayerKind = lpk_Host);
end;


//Startup a local server and connect to it as ordinary client
procedure TKMNetworking.Host(aUserName,aServerName,aPort:string; aAnnounceServer:boolean);
begin
  fIgnorePings := 0; //Accept pings
  fNetServer.Stop;

  fNetServer.OnMessage := fOnTextMessage;
  try
    fNetServer.Start(aServerName,aPort,aAnnounceServer,false);
  except
    on E : Exception do
    begin
      //Server failed to start
      fOnHostFail(E.Message);
      Exit;
    end;
  end;
  Join('127.0.0.1', aPort, aUserName, 0); //Server will assign hosting rights to us as we are the first joiner
end;


procedure TKMNetworking.Join(aServerAddress,aPort,aUserName:string; aRoom:integer);
begin
  Assert(not fNetClient.Connected, 'We were not properly disconnected');

  fIgnorePings := 0; //Accept pings
  fJoinTimeout := GetTickCount;
  fMyIndex := -1; //Host will send us PlayerList and we will get our index from there
  fMyIndexOnServer := -1; //Assigned by Server
  fRoomToJoin := aRoom;
  SetGameState(lgs_Connecting); //We are still connecting to the server

  fHostAddress := aServerAddress;
  fMyNikname := aUserName;
  fNetPlayerKind := lpk_Joiner;

  fNetClient.OnRecieveData := PacketRecieve;
  fNetClient.OnConnectSucceed := ConnectSucceed;
  fNetClient.OnConnectFailed := ConnectFailed;
  fNetClient.OnForcedDisconnect := ForcedDisconnect;
  fNetClient.OnStatusMessage := fOnTextMessage;
  fNetClient.ConnectTo(fHostAddress, aPort);
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
  fIgnorePings := 0;
  SetGameState(lgs_None);
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
  fWelcomeMessage := '';

  fNetPlayers.Clear;
  fNetClient.Disconnect;
  fNetServer.Stop;

  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);

  fSelectGameKind := ngk_None;
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
  if fIgnorePings > 0 then
  begin
    dec(fIgnorePings);
    exit;
  end;
  if fIgnorePings <> 0 then exit; //-1 means ignore all pings
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
      fNetPlayers[LocalHandle].AddPing(PingValue);
  end;
  M.Free;
end;


procedure TKMNetworking.SendMapOrSave;
begin
  case fSelectGameKind of
    ngk_Save: begin
                PacketSend(NET_ADDRESS_OTHERS, mk_SaveSelect, fSaveInfo.Filename, 0);
                PacketSend(NET_ADDRESS_OTHERS, mk_SaveCRC, '', Integer(fSaveInfo.CRC));
              end;
    ngk_Map:  begin
                PacketSend(NET_ADDRESS_OTHERS, mk_MapSelect, fMapInfo.Filename, 0);
                PacketSend(NET_ADDRESS_OTHERS, mk_MapCRC, '', Integer(fMapInfo.CRC));
              end;
    else      PacketSend(NET_ADDRESS_OTHERS, mk_ResetMap, '', 0);
  end;
end;


function TKMNetworking.GetGameInfo:TKMGameInfo;
begin
  case fSelectGameKind of
    ngk_Save: Result := fSaveInfo.Info;
    ngk_Map:  Result := fMapInfo.Info;
    else      Result := nil;
  end;
end;


procedure TKMNetworking.MatchPlayersToSave(aPlayerID:integer=-1);
var i,k: integer;
begin
  Assert(IsHost, 'Only host can match players');
  Assert(fSelectGameKind = ngk_Save, 'Not a save');
  //If we are matching all then reset them all first so we don't get clashes
  if aPlayerID = -1 then
    for i:=1 to fNetPlayers.Count do
      fNetPlayers[i].StartLocation := 0;

  //Add enough AI players automatically (when we are matching all)
  if aPlayerID = -1 then
    for i:=fNetPlayers.GetAICount to fSaveInfo.Info.AICount-1 do
      fNetPlayers.AddAIPlayer;

  for i:=1 to fNetPlayers.Count do
    for k:=1 to fSaveInfo.Info.PlayerCount do
      if (i = aPlayerID) or (aPlayerID = -1) then //-1 means update all players
        if fNetPlayers.LocAvailable(k) then
        begin
          if ((fNetPlayers[i].PlayerType = pt_Computer) and (fSaveInfo.Info.PlayerTypes[k-1] = pt_Computer))
          or (fNetPlayers[i].Nikname = fSaveInfo.Info.LocationName[k-1]) then
            fNetPlayers[i].StartLocation := k;
        end;
end;


//Clear selection from any map/save
procedure TKMNetworking.SelectNoMap;
begin
  Assert(IsHost, 'Only host can reset map');

  fSelectGameKind := ngk_None;

  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);

  PacketSend(NET_ADDRESS_OTHERS, mk_ResetMap, '', 0);
  fNetPlayers.ResetLocAndReady; //Reset start locations
  fNetPlayers[fMyIndex].ReadyToStart := true;

  if Assigned(fOnMapName) then fOnMapName('');
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which map we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectMap(const aName:string);
begin
  Assert(IsHost, 'Only host can select maps');
  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);
  fMapInfo := TKMapInfo.Create;

  fMapInfo.Load(aName, true);

  if not fMapInfo.IsValid then
  begin
    SelectNoMap;
    Exit;
  end;

  fNetPlayers.ResetLocAndReady; //Reset start locations

  fSelectGameKind := ngk_Map;
  fNetPlayers[fMyIndex].ReadyToStart := True;

  SendMapOrSave;

  if Assigned(fOnMapName) then fOnMapName(fMapInfo.Filename);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which save we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectSave(const aName:string);
begin
  Assert(IsHost, 'Only host can select saves');
  
  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);

  fSaveInfo := TKMSaveInfo.Create(ExeDir + 'SavesMP\', aName);

  if not fSaveInfo.IsValid then
  begin
    SelectNoMap;
    Exit;
  end;

  fNetPlayers.ResetLocAndReady; //Reset start locations

  fSelectGameKind := ngk_Save;
  fNetPlayers[fMyIndex].ReadyToStart := True;

  SendMapOrSave;
  MatchPlayersToSave; //Don't match players if it's not a valid save

  if Assigned(fOnMapName) then fOnMapName(fSaveInfo.Filename);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which start position we would like to use
//Each players choice should be unique
procedure TKMNetworking.SelectLoc(aIndex:integer; aPlayerIndex:integer);
begin
  //Check if position can be taken before sending
  if ((fSelectGameKind = ngk_Map) and ((not fMapInfo.IsValid) or (aIndex > fMapInfo.Info.PlayerCount))) or
     ((fSelectGameKind = ngk_Save) and ((not fSaveInfo.IsValid) or (aIndex > fSaveInfo.Info.PlayerCount))) or
     (fSelectGameKind = ngk_None) or
     (not fNetPlayers.LocAvailable(aIndex)) then
  begin
    if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
    Exit;
  end;

  //Host makes rules, Joiner will get confirmation from Host
  fNetPlayers[aPlayerIndex].StartLocation := aIndex; //Use aPlayerIndex not fMyIndex because it could be an AI

  case fNetPlayerKind of
    lpk_Host:   SendPlayerListAndRefreshPlayersSetup;
    lpk_Joiner: PacketSend(NET_ADDRESS_HOST, mk_StartingLocQuery, '', aIndex);
  end;
end;


//Tell other players which team we are on. Player selections need not be unique
procedure TKMNetworking.SelectTeam(aIndex:integer; aPlayerIndex:integer);
begin
  fNetPlayers[aPlayerIndex].Team := aIndex; //Use aPlayerIndex not fMyIndex because it could be an AI

  case fNetPlayerKind of
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

  case fNetPlayerKind of
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
  if (fSelectGameKind = ngk_Save) and (fNetPlayers[fMyIndex].StartLocation = 0) then
  begin
    if Assigned(fOnTextMessage) then fOnTextMessage('Error: Please select a player to play as');
    Result := false;
    Exit;
  end;

  if ((fSelectGameKind = ngk_Map) and fMapInfo.IsValid) or
     ((fSelectGameKind = ngk_Save) and fSaveInfo.IsValid) then
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
  case fSelectGameKind of
    ngk_Map:  Result := (fNetPlayers.Count > 1) and fNetPlayers.AllReady and fMapInfo.IsValid;
    ngk_Save: begin
                Result := (fNetPlayers.Count > 1) and fNetPlayers.AllReady and fSaveInfo.IsValid;
                for i:=1 to fNetPlayers.Count do //In saves everyone must chose a location
                  Result := Result and (fNetPlayers[i].StartLocation <> 0);
              end;
    else      Result := False;
  end;
end;


//Tell other players we want to start
procedure TKMNetworking.StartClick;
var PlayerCount: byte; ErrorMessage: String;
begin
  Assert(IsHost, 'Only host can start the game');

  //Define random parameters (start locations and flag colors)
  //This will also remove odd players from the List, they will lose Host in few seconds
  case fSelectGameKind of
    ngk_Map:  PlayerCount := fMapInfo.Info.PlayerCount;
    ngk_Save: PlayerCount := fSaveInfo.Info.PlayerCount;
    else      PlayerCount := 0;
  end;

  if not fNetPlayers.ValidateSetup(PlayerCount, ErrorMessage) then
  begin
    fOnTextMessage('Can not start: ' + ErrorMessage);
    Exit;
  end;

  //@Krom: If the server is local then this message will not be recieved by the other players until we have finished this process
  //       because the server will not process and resend the message until then. Adding Application.ProcessMessage; here would fix
  //       it, but possibly cause other problems. (using ProcessMessages is frowned upon because it can create recursive calls)
  //       This is the reason why the server loads the map first. Maybe the NetServer should run on a seperate thread to prevent
  //       the host from locking it up under any circumstances?
  //@Lewin: Thats low priority, but yes, separate thread would be good solution AFAIK
  //todo: TKMNetServer should run in a seperate thread to make it update even while the client (player) is busy (see above)

  //Let everyone start with final version of fNetPlayers
  PacketSend(NET_ADDRESS_OTHERS, mk_Start, fNetPlayers.GetAsText, 0);
  PacketSend(NET_ADDRESS_SERVER, mk_RoomClose, '', 0); //Tell the server this room is now closed

  StartGame;
end;


procedure TKMNetworking.SendPlayerListAndRefreshPlayersSetup(aPlayerIndex:integer = NET_ADDRESS_OTHERS);
var i:integer;
begin
  Assert(IsHost, 'Only host can send player list');

  //In saves we should load team and color from the SaveInfo
  if (fNetGameState = lgs_Lobby) and (fSelectGameKind = ngk_Save) then
    for i:=1 to NetPlayers.Count do
      if NetPlayers[i].StartLocation <> 0 then
      begin
        NetPlayers[i].FlagColorID := fSaveInfo.Info.ColorID[NetPlayers[i].StartLocation-1];
        NetPlayers[i].Team := fSaveInfo.Info.Team[NetPlayers[i].StartLocation-1];
      end
      else
      begin
        NetPlayers[i].FlagColorID := 0;
        NetPlayers[i].Team := 0;
      end;

  if IsHost and (fNetGameState = lgs_Lobby) then
  begin
    if NetPlayers.Count >= MAX_PLAYERS then
      PacketSend(NET_ADDRESS_SERVER, mk_RoomClose, '', 0) //Tell the server this room is now full
    else
      PacketSend(NET_ADDRESS_SERVER, mk_RoomOpen, '', 0); //Tell the server this room is now available
  end;
  if IsHost then
    PacketSend(NET_ADDRESS_SERVER, mk_SetPlayerList, fNetPlayers.GetSimpleAsText, 0);

  PacketSend(aPlayerIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
end;


//We route the message through Server to ensure everyone sees messages in the same order
//with exact same timestamps (possibly added by Server?)
procedure TKMNetworking.PostMessage(aText:string; aShowName:boolean=false; aTeamOnly:boolean=false);
var i: integer;
begin
  if aShowName then
  begin
    if fNetGameState <> lgs_Game then
      aText := fMyNikname+': '+aText
    else
    begin
      if aTeamOnly then
        aText := fMyNikname+' (Team): '+aText
      else
        aText := fMyNikname+' (All): '+aText;
    end;
  end;
  if not aTeamOnly then
    PacketSend(NET_ADDRESS_ALL, mk_Text, aText, 0) //Send to all
  else
    if NetPlayers[fMyIndex].Team = 0 then
      PacketSend(fMyIndexOnServer, mk_Text, aText, 0) //Send to self only if we have no team
    else
      for i:=1 to NetPlayers.Count do
        if (NetPlayers[i].Team = NetPlayers[fMyIndex].Team) and NetPlayers[i].IsHuman and (NetPlayers[i].IndexOnServer <> -1) then
          PacketSend(NetPlayers[i].IndexOnServer, mk_Text, aText, 0); //Send to each player on team (includes self)
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
  case fNetPlayerKind of
    lpk_Host:   begin
                  fNetPlayers[fMyIndex].ReadyToPlay := true;
                  PacketSend(NET_ADDRESS_OTHERS, mk_ReadyToPlay, '', 0);
                  SendPlayerListAndRefreshPlayersSetup; //Initialise the in-game player setup
                  //Check this here because it is possible to start a multiplayer game without other humans, just AI (at least for debugging)
                  if fNetPlayers.AllReadyToPlay then
                  begin
                    PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0);
                    PlayGame;
                  end;
                end;
    lpk_Joiner: begin
                  fNetPlayers[fMyIndex].ReadyToPlay := true;
                  PacketSend(NET_ADDRESS_OTHERS, mk_ReadyToPlay, '', 0);
                end;
  end;
end;


procedure TKMNetworking.PacketRecieve(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal);
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
  M.Read(Kind, SizeOf(TKMessageKind)); //Depending on kind message contains either Text or a Number
  case NetPacketType[Kind] of
    pfNumber: M.Read(Param);
    pfText:   M.Read(Msg);
  end;
  M.Free;

  //Make sure we are allowed to receive this packet at this point
  if not (Kind in NetAllowedPackets[fNetGameState]) then
  begin
    //When querying a host we may receive data such as commands, player setup, etc. These should be ignored.
    if (fNetGameState <> lgs_Query) and Assigned(fOnTextMessage) then
      fOnTextMessage('Error: Received an packet not intended for this state');
    Exit;
  end;

  case Kind of
    mk_GameVersion:
            if Msg <> GAME_REVISION then
            begin
              Assert(not IsHost);
              fOnJoinFail('Wrong game version: '+GAME_REVISION+'. Server uses: '+Msg);
              fNetClient.Disconnect;
              exit;
            end;

    mk_WelcomeMessage:
            begin
              fWelcomeMessage := Msg;
            end;

    mk_HostingRights:
            begin
              fNetPlayerKind := lpk_Host;
              if Assigned(fOnJoinAssignedHost) then fOnJoinAssignedHost(Self); //Enter the lobby if we had hosting rights assigned to us
              if Assigned(fOnTextMessage) then fOnTextMessage('Server has assigned hosting rights to us');
            end;

    mk_IndexOnServer:
            begin
              fMyIndexOnServer := Param;
              if Assigned(fOnTextMessage) then fOnTextMessage('Index on Server - ' + inttostr(fMyIndexOnServer));
              //Now join the room we planned to
              PacketSend(NET_ADDRESS_SERVER, mk_JoinRoom, '', fRoomToJoin);
            end;

    mk_ConnectedToRoom:
            begin
              //We are now clear to proceed with our business
              case fNetPlayerKind of
                lpk_Host:
                    begin
                      fNetPlayers.AddPlayer(fMyNikname, fMyIndexOnServer);
                      fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
                      fNetPlayers[fMyIndex].ReadyToStart := true;
                      if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                      SetGameState(lgs_Lobby);
                      if Assigned(fOnTextMessage) and (fWelcomeMessage <> '') then fOnTextMessage(fWelcomeMessage);
                    end;
                lpk_Joiner:
                begin
                    SetGameState(lgs_Query);
                    fJoinTimeout := GetTickCount; //Wait another X seconds for host to reply before timing out
                    PacketSend(NET_ADDRESS_HOST, mk_AskToJoin, fMyNikname, 0);
                end;
              end;
            end;

    mk_AskToJoin:
            if IsHost then begin
              ReMsg := fNetPlayers.CheckCanJoin(Msg, aSenderIndex);
              if (ReMsg = '') and (fNetGameState = lgs_Game) then
                ReMsg := 'Cannot join while the game is in progress';
              if ReMsg = '' then
              begin
                fNetPlayers.AddPlayer(Msg, aSenderIndex);
                PacketSend(aSenderIndex, mk_AllowToJoin, '', 0);
                SendMapOrSave; //Send the map first so it doesn't override starting locs

                if fSelectGameKind = ngk_Save then MatchPlayersToSave(fNetPlayers.ServerToLocal(aSenderIndex)); //Match only this player
                SendPlayerListAndRefreshPlayersSetup;
                PostMessage(Msg+' has joined');
              end
              else
                PacketSend(aSenderIndex, mk_RefuseToJoin, ReMsg, 0);
            end;

    mk_AllowToJoin:
            if fNetPlayerKind = lpk_Joiner then
            begin
              fOnJoinSucc(Self); //Enter lobby
              SetGameState(lgs_Lobby);
              if Assigned(fOnTextMessage) and (fWelcomeMessage <> '') then fOnTextMessage(fWelcomeMessage);
            end;

    mk_RefuseToJoin:
            if fNetPlayerKind = lpk_Joiner then begin
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_ClientLost:
            if IsHost then
            begin
              if fNetPlayers.ServerToLocal(Param) = -1 then exit; //Has already disconnected or not from our room
              PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(Param)].Nikname+' lost connection');
              if fNetGameState in [lgs_Loading, lgs_Game] then
                fNetPlayers.KillPlayer(Param)
              else
                fNetPlayers.RemPlayer(Param);
              SendPlayerListAndRefreshPlayersSetup;
            end
            else
              if fNetPlayers.ServerToLocal(Param) <> -1 then
              begin
                if fNetGameState in [lgs_Loading, lgs_Game] then
                  fNetPlayers.KillPlayer(Param)
                else
                  fNetPlayers.RemPlayer(Param); //Remove the player anyway as it might be the host that was lost
              end;

    mk_Disconnect:
            if aSenderIndex <> NET_ADDRESS_SERVER then
              case fNetPlayerKind of
                lpk_Host:
                    begin
                      if fNetPlayers.ServerToLocal(aSenderIndex) = -1 then exit; //Has already disconnected
                      PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Nikname+' has quit');
                      if fNetGameState in [lgs_Loading, lgs_Game] then
                        fNetPlayers.KillPlayer(aSenderIndex)
                      else
                        fNetPlayers.RemPlayer(aSenderIndex);
                      SendPlayerListAndRefreshPlayersSetup;
                    end;
                lpk_Joiner:
                    begin
                      if fNetPlayers.ServerToLocal(aSenderIndex) = -1 then exit; //Has already disconnected
                      if Assigned(fOnTextMessage) then fOnTextMessage('The host has disconnected');
                      if fNetGameState in [lgs_Loading, lgs_Game] then
                        fNetPlayers.KillPlayer(aSenderIndex)
                      else
                        fNetPlayers.RemPlayer(aSenderIndex);
                    end;
              end
            else
              ForcedDisconnect('The server was forced to restart due to a data corruption error');

    mk_ReassignHost:
            if Param = fMyIndexOnServer then
            begin
              //We are now the host
              fNetPlayerKind := lpk_Host;
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              if Assigned(fOnReassignedHost) then fOnReassignedHost(Self); //Lobby/game might need to know that we are now hosting

              case fNetGameState of
                lgs_Lobby:   begin
                               fNetPlayers[fMyIndex].ReadyToStart := true; //The host is always ready
                               fNetPlayers.SetAIReady; //Set all AI players to ready
                             end;
                lgs_Loading: begin
                               if Assigned(fOnReadyToPlay) then fOnReadyToPlay(Self);
                               if fNetPlayers.AllReadyToPlay then
                               begin
                                 PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0);
                                 PlayGame;
                               end;
                             end;
              end;

              SendPlayerListAndRefreshPlayersSetup;
              PostMessage('Hosting rights reassigned to '+fMyNikname);
            end;

    mk_Ping:
            PacketSend(aSenderIndex, mk_Pong, '', 0); //Server will intercept this message

    mk_PingInfo:
            begin
              DecodePingInfo(Msg);
              if Assigned(fOnPingInfo) then fOnPingInfo(Self);
            end;

    mk_PlayersList:
            if fNetPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg); //Our index could have changed on players add/removal
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ResetMap:
            begin
              fSelectGameKind := ngk_None;
              FreeAndNil(fMapInfo);
              FreeAndNil(fSaveInfo);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName('');
            end;

    mk_MapSelect:
            if fNetPlayerKind = lpk_Joiner then begin
              fSelectGameKind := ngk_Map;
              FreeAndNil(fMapInfo);
              fMapInfo := TKMapInfo.Create;
              fMapInfo.Load(Msg, true);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Filename);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_MapCRC:
            if fNetPlayerKind = lpk_Joiner then
            begin
              if Integer(fMapInfo.CRC) <> Param then
              begin
                if fMapInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the map '+fMapInfo.Filename)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the map '+fMapInfo.Filename);
                fMapInfo.Free;
                fSelectGameKind := ngk_None;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := false;
                if Assigned(fOnMapName) then fOnMapName('');
              end
            end;

    mk_SaveSelect:
            if fNetPlayerKind = lpk_Joiner then begin
              fSelectGameKind := ngk_Save;
              FreeAndNil(fSaveInfo);
              fSaveInfo := TKMSaveInfo.Create(ExeDir + 'SavesMP\', Msg);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fSaveInfo.Filename);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_SaveCRC:
            if fNetPlayerKind = lpk_Joiner then
              if Integer(fSaveInfo.CRC) <> Param then
              begin
                if fSaveInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the save '+fSaveInfo.Filename)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the save '+fSaveInfo.Filename);
                fSaveInfo.Free;
                fSelectGameKind := ngk_None;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := False;
                if Assigned(fOnMapName) then fOnMapName('');
              end;

    mk_StartingLocQuery:
            if IsHost then begin
              LocID := Param;
              if (GameInfo <> nil) and GameInfo.IsValid and
                 (LocID <= GameInfo.PlayerCount) and
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
            if fNetPlayerKind = lpk_Joiner then begin
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
                PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0);
                PlayGame;
              end;
            end;

    mk_Play:
            if fNetPlayerKind = lpk_Joiner then PlayGame;

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
  SetGameState(lgs_Loading); //Loading has begun (no further players allowed to join)
  fIgnorePings := -1; //Ignore all pings until we have finished loading

  case fSelectGameKind of
    ngk_Map:  fOnStartMap(fMapInfo.Filename);
    ngk_Save: fOnStartSave(fSaveInfo.Filename);
    else      Assert(False);
  end;
end;


procedure TKMNetworking.PlayGame;
begin
  fIgnorePings := 2; //Ignore the next two pings as they may have been measured during loading
  SetGameState(lgs_Game); //The game has begun (no further players allowed to join)
  if Assigned(fOnPlay) then fOnPlay(Self);
end;


procedure TKMNetworking.SetGameState(aState:TNetGameState);
begin
  fNetGameState := aState;
  if (fNetGameState in [lgs_Lobby,lgs_Loading,lgs_Game]) and IsHost and (fMyIndexOnServer <> -1) then
  begin
    PacketSend(NET_ADDRESS_SERVER,mk_SetGameState,NetGameStateText[fNetGameState],0);
    PacketSend(NET_ADDRESS_SERVER, mk_SetPlayerList, fNetPlayers.GetSimpleAsText, 0);
  end;
end;


procedure TKMNetworking.UpdateState(aTick: cardinal);
begin
  //Joining timeout
  if fNetGameState in [lgs_Connecting,lgs_Query] then
    if GetTickCount-fJoinTimeout > JOIN_TIMEOUT then
      fOnJoinFail('Query timed out');
end;


procedure TKMNetworking.UpdateStateIdle;
begin
  fNetServer.UpdateState; //Server measures pings etc.
  //LNet requires network update calls unless it is being used as visual components
  fNetClient.UpdateStateIdle;
  fServerQuery.UpdateStateIdle;
end;


end.
