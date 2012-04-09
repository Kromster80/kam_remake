unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, SysUtils, TypInfo,
  KM_CommonClasses, KM_CommonEvents, KM_NetworkTypes, KM_Defaults,
  KM_Player, KM_Saves, KM_GameInfo, KM_GameOptions,
  KM_Maps, KM_NetPlayersList, KM_DedicatedServer, KM_NetClient, KM_ServerQuery;

//todo: Check CRCs of important game data files (units.dat, houses.dat, etc.) to make sure all clients match

type
  TNetPlayerKind = (lpk_Host, lpk_Joiner);
  TNetGameState = (lgs_None, lgs_Connecting, lgs_Query, lgs_Lobby, lgs_Loading, lgs_Game, lgs_Reconnecting);
  TNetGameKind = (ngk_None, ngk_Map, ngk_Save);

const
  NetMPGameState:array[TNetGameState] of TMPGameState = (mgs_None, mgs_None, mgs_None, mgs_Lobby, mgs_Loading, mgs_Game, mgs_Game);
  NetAllowedPackets:array[TNetGameState] of set of TKMessageKind = (
  [], //lgs_None
  [mk_RefuseToJoin,mk_HostingRights,mk_IndexOnServer,mk_GameVersion,mk_WelcomeMessage,mk_Ping,mk_ConnectedToRoom,mk_PingInfo,mk_Kicked,mk_ServerName], //lgs_Connecting
  [mk_AllowToJoin,mk_RefuseToJoin,mk_Ping,mk_PingInfo,mk_Kicked], //lgs_Query
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_PingInfo,mk_PlayersList,
   mk_StartingLocQuery,mk_SetTeam,mk_FlagColorQuery,mk_ResetMap,mk_MapSelect,mk_MapCRC,mk_SaveSelect,
   mk_SaveCRC,mk_ReadyToStart,mk_Start,mk_Text,mk_Kicked,mk_LangCode,mk_GameOptions,mk_ServerName], //lgs_Lobby
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_PingInfo,mk_PlayersList,mk_ReadyToPlay,mk_Play,mk_Text,mk_Kicked], //lgs_Loading
  [mk_AskToJoin,mk_ClientLost,mk_ReassignHost,mk_Disconnect,mk_Ping,mk_PingInfo,mk_PlayersList,mk_Commands,mk_Text,mk_ResyncFromTick,mk_AskToReconnect,mk_Kicked,mk_ClientReconnected], //lgs_Game
  [mk_HostingRights,mk_IndexOnServer,mk_GameVersion,mk_WelcomeMessage,mk_Ping,mk_ConnectedToRoom,mk_PingInfo,mk_PlayersList,mk_ReconnectionAccepted,mk_RefuseReconnect,mk_Kicked] //lgs_Reconnecting
  );

  JOIN_TIMEOUT = 8000; //8 sec. Timeout for join queries
  RECONNECT_PAUSE = 2000; //Time in ms which we wait before attempting to reconnect (stops the socket from becoming overloaded)


type
  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
  private
    fNetServer:TKMDedicatedServer;
    fNetClient:TKMNetClient;
    fServerQuery:TKMServerQuery;
    fNetPlayerKind: TNetPlayerKind; //Our role (Host or Joiner)
    fNetGameState: TNetGameState;
    fServerAddress:string; //Used for reconnecting
    fServerPort:string; //Used for reconnecting
    fRoomToJoin:integer; //The room we should join once we hear from the server
    fLastProcessedTick:cardinal;
    fReconnectRequested:cardinal; //TickCount at which a reconnection was requested
    fMyLang:string;
    fMyNikname:string;
    fWelcomeMessage:string;
    fServerName:string; //Name of the server we are currently in (shown in the lobby)
    fMyIndexOnServer:integer;
    fMyIndex:integer; //In NetPlayers list
    fIgnorePings: integer; //During loading ping measurements will be high, so discard them. (when networking is threaded this might be unnecessary)
    fJoinTimeout:cardinal;
    fNetPlayers:TKMPlayersList;

    fMapInfo:TKMapInfo; //Everything related to selected map
    fSaveInfo: TKMSaveInfo;
    fSelectGameKind: TNetGameKind;
    fNetGameOptions:TKMGameOptions;

    fOnJoinSucc:TNotifyEvent;
    fOnJoinFail:TStringEvent;
    fOnJoinAssignedHost:TNotifyEvent;
    fOnHostFail:TStringEvent;
    fOnReassignedHost:TNotifyEvent;
    fOnTextMessage:TStringEvent;
    fOnPlayersSetup:TNotifyEvent;
    fOnGameOptions:TNotifyEvent;
    fOnMapName:TStringEvent;
    fOnStartMap:TStringEvent;
    fOnStartSave:TStringEvent;
    fOnPlay:TNotifyEvent;
    fOnReadyToPlay:TNotifyEvent;
    fOnDisconnect:TStringEvent;
    fOnPingInfo:TNotifyEvent;
    fOnMPGameInfoChanged:TNotifyEvent;
    fOnCommands:TStringEvent;
    fOnResyncFromTick:TResyncEvent;

    procedure DecodePingInfo(aInfo:string);
    procedure ForcedDisconnect(const S: string);
    procedure StartGame;
    procedure TryPlayGame;
    procedure PlayGame;
    procedure SetGameState(aState:TNetGameState);
    procedure SendMapOrSave;
    function GetGameInfo:TKMGameInfo;
    procedure DoReconnection;

    procedure ConnectSucceed(Sender:TObject);
    procedure ConnectFailed(const S: string);
    procedure PacketRecieve(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal); //Process all commands
    procedure PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
  public
    constructor Create(const aMasterServerAddress:string; aKickTimeout, aPingInterval, aAnnounceInterval:word; aLang:string);
    destructor Destroy; override;

    property MyIndex:integer read fMyIndex;
    property NetGameState:TNetGameState read fNetGameState;
    function MyIPString:string;
    property ServerName:string read fServerName;
    property ServerAddress:string read fServerAddress;
    property ServerPort:string read fServerPort;
    property ServerRoom:Integer read fRoomToJoin;
    function IsHost:boolean;
    function IsReconnecting:boolean;

    //Lobby
    property ServerQuery:TKMServerQuery read fServerQuery;
    procedure Host(aUserName,aServerName,aPort:string; aAnnounceServer:boolean);
    procedure Join(aServerAddress,aPort,aUserName:string; aRoom:integer; aIsReconnection:boolean=false);
    procedure AnnounceDisconnect;
    procedure Disconnect;
    procedure DropWaitingPlayers(aPlayers:TStringList);
    function  Connected: boolean;
    procedure MatchPlayersToSave(aPlayerID:integer=-1);
    procedure SelectNoMap(aMessage:string);
    procedure SelectMap(const aName:string);
    procedure SelectSave(const aName:string);
    procedure SelectLoc(aIndex:integer; aPlayerIndex:integer);
    procedure SelectTeam(aIndex:integer; aPlayerIndex:integer);
    procedure SelectColor(aIndex:integer; aPlayerIndex:integer);
    procedure KickPlayer(aPlayerIndex:integer);
    function ReadyToStart:boolean;
    function CanStart:boolean;
    procedure StartClick; //All required arguments are in our class
    procedure SendPlayerListAndRefreshPlayersSetup(aPlayerIndex:integer = NET_ADDRESS_OTHERS);
    procedure SendGameOptions;

    //Common
    procedure ConsoleCommand(aText:string);
    procedure PostMessage(aText:string; aShowName:boolean=false; aTeamOnly:boolean=false);
    procedure PostLocalMessage(aText:string; aMakeSound:boolean=true);
    procedure SendMPGameInfo(aGameTime:TDateTime; aMap:string);

    //Gameplay
    property MapInfo:TKMapInfo read fMapInfo;
    property SaveInfo:TKMSaveInfo read fSaveInfo;
    property GameInfo:TKMGameInfo read GetGameInfo;
    property NetGameOptions:TKMGameOptions read fNetGameOptions;
    property SelectGameKind: TNetGameKind read fSelectGameKind;
    property NetPlayers:TKMPlayersList read fNetPlayers;
    property LastProcessedTick:cardinal write fLastProcessedTick;
    procedure GameCreated;
    procedure SendCommands(aStream:TKMemoryStream; aPlayerIndex:TPlayerIndex=-1);
    procedure AttemptReconnection;

    property OnJoinSucc:TNotifyEvent write fOnJoinSucc;         //We were allowed to join
    property OnJoinFail:TStringEvent write fOnJoinFail;         //We were refused to join
    property OnHostFail:TStringEvent write fOnHostFail;         //Server failed to start (already running a server?)
    property OnJoinAssignedHost:TNotifyEvent write fOnJoinAssignedHost; //We were assigned hosting rights upon connection
    property OnReassignedHost:TNotifyEvent write fOnReassignedHost;     //We were reassigned hosting rights when the host quit

    property OnPlayersSetup:TNotifyEvent write fOnPlayersSetup; //Player list updated
    property OnGameOptions:TNotifyEvent write fOnGameOptions; //Game options updated
    property OnMapName:TStringEvent write fOnMapName;           //Map name updated
    property OnStartMap:TStringEvent write fOnStartMap;       //Start the game
    property OnStartSave:TStringEvent write fOnStartSave;       //Load the game
    property OnPlay:TNotifyEvent write fOnPlay;                 //Start the gameplay
    property OnReadyToPlay:TNotifyEvent write fOnReadyToPlay;   //Update the list of players ready to play
    property OnPingInfo:TNotifyEvent write fOnPingInfo;         //Ping info updated
    property OnMPGameInfoChanged:TNotifyEvent write fOnMPGameInfoChanged;

    property OnDisconnect:TStringEvent write fOnDisconnect;     //Lost connection, was kicked
    property OnCommands:TStringEvent write fOnCommands;         //Recieved GIP commands
    property OnResyncFromTick:TResyncEvent write fOnResyncFromTick;

    property OnTextMessage:TStringEvent write fOnTextMessage;   //Text message recieved

    procedure UpdateMultiplayerTeams;
    procedure UpdateState(aTick: cardinal);
    procedure UpdateStateIdle;
  end;


implementation
uses KM_TextLibrary, KM_Sound, KM_Log, KM_Utils, StrUtils, Math;


{ TKMNetworking }
constructor TKMNetworking.Create(const aMasterServerAddress:string; aKickTimeout, aPingInterval, aAnnounceInterval:word; aLang:string);
begin
  Inherited Create;
  SetGameState(lgs_None);
  fMyLang := aLang;
  fNetServer := TKMDedicatedServer.Create(1, aKickTimeout, aPingInterval, aAnnounceInterval, aMasterServerAddress, '', '');
  fNetClient := TKMNetClient.Create;
  fNetPlayers := TKMPlayersList.Create;
  fServerQuery := TKMServerQuery.Create(aMasterServerAddress);
  fNetGameOptions := TKMGameOptions.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fNetPlayers.Free;
  fNetServer.Free;
  fNetClient.Free;
  fServerQuery.Free;
  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);
  FreeAndNil(fNetGameOptions);
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


function TKMNetworking.IsReconnecting:boolean;
begin
  Result := (fNetGameState = lgs_Reconnecting);
end;


//Startup a local server and connect to it as ordinary client
procedure TKMNetworking.Host(aUserName, aServerName, aPort: string; aAnnounceServer: boolean);
begin
  fWelcomeMessage := '';
  fIgnorePings := 0; //Accept pings
  fNetServer.Stop;

  fNetServer.OnMessage := fLog.AppendLog; //Log server messages in case there is a problem, but hide from user
  try
    fNetServer.Start(aServerName, aPort, aAnnounceServer, False);
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


procedure TKMNetworking.Join(aServerAddress,aPort,aUserName:string; aRoom:integer; aIsReconnection:boolean=false);
begin
  Assert(not fNetClient.Connected, 'Cannot connect: We are already connected');

  fWelcomeMessage := '';
  fIgnorePings := 0; //Accept pings
  fJoinTimeout := GetTickCount;
  fMyIndex := -1; //Host will send us PlayerList and we will get our index from there
  fMyIndexOnServer := -1; //Assigned by Server
  fRoomToJoin := aRoom;
  if aIsReconnection then
    SetGameState(lgs_Reconnecting) //Special state so we know we are reconnecting
  else
    SetGameState(lgs_Connecting); //We are still connecting to the server

  fServerAddress := aServerAddress;
  fServerPort := aPort;
  fMyNikname := aUserName;
  fNetPlayerKind := lpk_Joiner;
  fServerName := ''; //Server will tell us once we are joined

  fNetClient.OnRecieveData := PacketRecieve;
  fNetClient.OnConnectSucceed := ConnectSucceed;
  fNetClient.OnConnectFailed := ConnectFailed;
  fNetClient.OnForcedDisconnect := ForcedDisconnect;
  //fNetClient.OnStatusMessage := fOnTextMessage; //For debugging only
  fNetClient.ConnectTo(fServerAddress, fServerPort);
end;


//Connection was successful, but we still need mk_IndexOnServer to be able to do anything
procedure TKMNetworking.ConnectSucceed(Sender:TObject);
begin
  //This is currently unused, the player does not need to see this message
  //PostLocalMessage('Connection successful');
end;


procedure TKMNetworking.ConnectFailed(const S: string);
begin
  fNetClient.Disconnect;
  fOnJoinFail(S);
end;


//Send message that we have deliberately disconnected
procedure TKMNetworking.AnnounceDisconnect;
begin
  if IsHost then
    PacketSend(NET_ADDRESS_OTHERS, mk_Disconnect, '', 0) //Host tells everyone when they quit
  else
    PacketSend(NET_ADDRESS_HOST, mk_Disconnect, '', 0); //Joiners should only tell host when they quit
end;


procedure TKMNetworking.Disconnect;
begin
  fIgnorePings := 0;
  fReconnectRequested := 0; //Cancel any reconnection that was requested
  SetGameState(lgs_None);
  fOnJoinSucc := nil;
  fOnJoinFail := nil;
  fOnJoinAssignedHost := nil;
  fOnHostFail := nil;
  fOnTextMessage := nil;
  fOnPlayersSetup := nil;
  fOnMapName := nil;
  fOnCommands := nil;
  fOnResyncFromTick := nil;
  fOnDisconnect := nil;
  fOnPingInfo := nil;
  fOnReassignedHost := nil;
  fWelcomeMessage := '';

  fNetPlayers.Clear;
  fNetGameOptions.Reset;
  fNetClient.Disconnect;
  fNetServer.Stop;

  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);

  fSelectGameKind := ngk_None;
end;


procedure TKMNetworking.DropWaitingPlayers(aPlayers:TStringList);
var i,ServerIndex:integer;
begin
  Assert(IsHost, 'Only the host is allowed to drop players');
  for i:=0 to aPlayers.Count-1 do
  begin
    ServerIndex := NetPlayers[NetPlayers.NiknameToLocal(aPlayers[i])].IndexOnServer;
    //Make sure this player is properly disconnected from the server
    PacketSend(NET_ADDRESS_SERVER,mk_KickPlayer,'',ServerIndex);
    NetPlayers.DropPlayer(ServerIndex);
    PostMessage('The host dropped '+aPlayers[i]+' from the game');
  end;
  SendPlayerListAndRefreshPlayersSetup;
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
                PacketSend(NET_ADDRESS_OTHERS, mk_SaveSelect, fSaveInfo.FileName, 0);
                PacketSend(NET_ADDRESS_OTHERS, mk_SaveCRC, '', Integer(fSaveInfo.CRC));
              end;
    ngk_Map:  begin
                PacketSend(NET_ADDRESS_OTHERS, mk_MapSelect, fMapInfo.FileName, 0);
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
  if aPlayerID = -1 then
  begin
    //If we are matching all then reset them all first so we don't get clashes
    for i:=1 to fNetPlayers.Count do
      fNetPlayers[i].StartLocation := 0;

    //Add enough AI players automatically (when we are matching all)
    for i:=fNetPlayers.GetAICount to fSaveInfo.Info.AICount-1 do
      if fNetPlayers.Count < MAX_PLAYERS then
        fNetPlayers.AddAIPlayer;

    for i:=1 to MAX_PLAYERS - fSaveInfo.Info.PlayerCount - fNetPlayers.GetClosedCount do
      if fNetPlayers.Count < MAX_PLAYERS then
        fNetPlayers.AddClosedPlayer; //Close unused slots
  end;

  for i:=1 to fNetPlayers.Count do
    for k:=1 to fSaveInfo.Info.PlayerCount do
      if (i = aPlayerID) or (aPlayerID = -1) then //-1 means update all players
        if fNetPlayers.LocAvailable(k) then
        begin
          if ((fNetPlayers[i].IsComputer) and (fSaveInfo.Info.PlayerTypes[k-1] = pt_Computer))
          or (fNetPlayers[i].Nikname = fSaveInfo.Info.LocationName[k-1]) then
            fNetPlayers[i].StartLocation := k;
        end;
end;


//Clear selection from any map/save
procedure TKMNetworking.SelectNoMap(aMessage: string);
begin
  Assert(IsHost, 'Only host can reset map');

  fSelectGameKind := ngk_None;

  FreeAndNil(fMapInfo);
  FreeAndNil(fSaveInfo);

  PacketSend(NET_ADDRESS_OTHERS, mk_ResetMap, '', 0);
  fNetPlayers.ResetLocAndReady; //Reset start locations
  fNetPlayers[fMyIndex].ReadyToStart := True;

  if Assigned(fOnMapName) then fOnMapName(aMessage);
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

  fMapInfo.Load(aName, true, true);

  if not fMapInfo.IsValid then
  begin
    SelectNoMap('Invalid');
    Exit;
  end;

  fNetPlayers.ResetLocAndReady; //Reset start locations

  fSelectGameKind := ngk_Map;
  fNetPlayers[fMyIndex].ReadyToStart := True;

  SendMapOrSave;

  if Assigned(fOnMapName) then fOnMapName(fMapInfo.FileName);
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
    SelectNoMap(fSaveInfo.Info.Title); //State the error, e.g. wrong version
    Exit;
  end;

  fNetPlayers.ResetLocAndReady; //Reset start locations

  NetGameOptions.Peacetime := fSaveInfo.GameOptions.Peacetime;
  SendGameOptions;
  if Assigned(fOnGameOptions) then fOnGameOptions(Self);

  fSelectGameKind := ngk_Save;
  fNetPlayers[fMyIndex].ReadyToStart := True;

  SendMapOrSave;
  MatchPlayersToSave; //Don't match players if it's not a valid save

  if Assigned(fOnMapName) then fOnMapName(fSaveInfo.FileName);
  SendPlayerListAndRefreshPlayersSetup;
end;


//Tell other players which start position we would like to use
//Each players choice should be unique
procedure TKMNetworking.SelectLoc(aIndex:integer; aPlayerIndex:integer);
var LocAvailable:Boolean; NetPlayerIndex: Integer;
begin
  //Check if position can be taken before sending
  LocAvailable := fNetPlayers.LocAvailable(aIndex);
  if ((fSelectGameKind = ngk_Map) and ((not fMapInfo.IsValid) or (aIndex > fMapInfo.Info.PlayerCount))) or
     ((fSelectGameKind = ngk_Save) and ((not fSaveInfo.IsValid) or (aIndex > fSaveInfo.Info.PlayerCount))) or
     (fSelectGameKind = ngk_None) or
     (not LocAvailable and (not IsHost or not fNetPlayers.HostDoesSetup)) then
  begin
    if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
    Exit;
  end;

  //If someone else has this index, switch them (only when HostDoesSetup)
  NetPlayerIndex := fNetPlayers.StartingLocToLocal(aIndex);
  if (NetPlayerIndex <> -1) and IsHost and fNetPlayers.HostDoesSetup then
    fNetPlayers[NetPlayerIndex].StartLocation := fNetPlayers[aPlayerIndex].StartLocation;

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


procedure TKMNetworking.KickPlayer(aPlayerIndex:integer);
begin
  assert(IsHost);
  PacketSend(NET_ADDRESS_SERVER, mk_KickPlayer, '', fNetPlayers[aPlayerIndex].IndexOnServer);
  PostMessage(fNetPlayers[aPlayerIndex].Nikname+' was kicked by the host');
end;


//Joiner indicates that he is ready to start
function TKMNetworking.ReadyToStart:boolean;
begin
  if (fSelectGameKind = ngk_Save) and (fNetPlayers[fMyIndex].StartLocation = 0) then
  begin
    PostLocalMessage(fTextLibrary[TX_LOBBY_ERROR_SELECT_PLAYER]);
    Result := false;
    Exit;
  end;

  if ((fSelectGameKind = ngk_Map) and fMapInfo.IsValid) or
     ((fSelectGameKind = ngk_Save) and fSaveInfo.IsValid) then
  begin
    //Toggle it
    PacketSend(NET_ADDRESS_HOST, mk_ReadyToStart, '', 0);
    Result := not fNetPlayers[fMyIndex].ReadyToStart;
  end
  else
  begin
    PostLocalMessage(fTextLibrary[TX_LOBBY_ERROR_NO_MAP]);
    Result := false;
  end;
end;


function TKMNetworking.CanStart:boolean;
var i:integer;
begin
  case fSelectGameKind of
    ngk_Map:  Result := fNetPlayers.AllReady and fMapInfo.IsValid;
    ngk_Save: begin
                Result := fNetPlayers.AllReady and fSaveInfo.IsValid;
                for i:=1 to fNetPlayers.Count do //In saves everyone must chose a location
                  Result := Result and ((fNetPlayers[i].StartLocation <> 0) or fNetPlayers[i].IsClosed);
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
    PostLocalMessage(Format(fTextLibrary[TX_LOBBY_CANNOT_START], [ErrorMessage]));
    Exit;
  end;

  //Let everyone start with final version of fNetPlayers and fNetGameOptions
  SendGameOptions;
  PacketSend(NET_ADDRESS_OTHERS, mk_Start, fNetPlayers.GetAsText, 0);

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

  fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname); //The host's index can change when players are removed

  fOnMPGameInfoChanged(Self); //Tell the server about the changes

  PacketSend(aPlayerIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
end;


procedure TKMNetworking.SendGameOptions;
begin
  Assert(IsHost, 'Only host can send game options');
  PacketSend(NET_ADDRESS_OTHERS, mk_GameOptions, fNetGameOptions.GetAsText, 0);
end;


procedure TKMNetworking.ConsoleCommand(aText:string);
var s,PlayerID:Integer;
begin
  PostLocalMessage('[$808080]'+aText+'[]');
  s := PosEx(' ',aText);
  if s = 0 then s := Length(aText)+1;

  if SameText(LeftStr(aText, s-1), '/kick') then
  begin
    if not IsHost then
    begin
      PostLocalMessage('Only the host can kick people',False);
      Exit;
    end;
    if (Length(aText) >= s+1) and TryStrToInt(aText[s+1], PlayerID)
    and InRange(PlayerID, 1, fNetPlayers.Count) then
    begin
      if fNetPlayers[PlayerID].IsHuman
      and (PlayerID <> MyIndex) then
        KickPlayer(PlayerID)
      else
        PostLocalMessage('You cannot kick yourself or AI players.',False);
    end
    else
      PostLocalMessage('Invalid syntax. Type /help for more info.',False);
  end
  else
  if SameText(LeftStr(aText, s-1), '/help') then
    PostLocalMessage('The following console commands are available:|'+
                     '    /kick <Player ID> - Kicks a player from the lobby|'+
                   //'    /ban <Player ID> - Kicks and bans a player from the lobby|'+
                   //'    /newhost <Player ID> - Changes the host player|'+
                     '    /help - Displays this page|'+
                     'Player IDs:|'+fNetPlayers.GetPlayersWithIDs,False)
  else
  begin
    PostLocalMessage('Unknown console command "'+aText+'". Type /help for more info.',False);
  end;
end;


//We route the message through Server to ensure everyone sees messages in the same order
//with exact same timestamps (possibly added by Server?)
procedure TKMNetworking.PostMessage(aText:string; aShowName:boolean=false; aTeamOnly:boolean=false);
var i: integer; NameText:String;
begin
  if aShowName then
  begin
    if NetPlayers[fMyIndex].FlagColorID <> 0 then
      NameText := '[$'+IntToHex(FlagColorToTextColor(NetPlayers[fMyIndex].FlagColor) and $00FFFFFF,6)+']'+fMyNikname+'[]'
    else
      NameText := fMyNikname;
    if fNetGameState <> lgs_Game then
      aText := NameText+': '+aText
    else
    begin
      if aTeamOnly then
        aText := NameText+' [$66FF66](Team)[]: '+aText
      else
        aText := NameText+' (All): '+aText;
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


procedure TKMNetworking.PostLocalMessage(aText:string; aMakeSound:boolean=true);
begin
  if Assigned(fOnTextMessage) then
  begin
    fOnTextMessage(aText);
    if aMakeSound then fSoundLib.Play(sfxn_MPChatMessage);
  end;
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


procedure TKMNetworking.AttemptReconnection;
begin
  if fReconnectRequested = 0 then fReconnectRequested := GetTickCount; //Do it soon
end;


procedure TKMNetworking.DoReconnection;
var TempMyIndex:integer;
begin
  if WRITE_RECONNECT_LOG then fLog.AppendLog(Format('DoReconnection: %s',[fMyNikname]));
  fReconnectRequested := 0;
  PostLocalMessage('Attempting to reconnect to the game...');
  //Stop the previous connection without calling Self.Disconnect as that frees everything
  fNetClient.Disconnect;
  TempMyIndex := fMyIndex;
  Join(fServerAddress,fServerPort,fMyNikname,fRoomToJoin, true); //Join the same server/room as before in reconnecting mode
  fMyIndex := TempMyIndex; //Join overwrites it, but we must remember it
end;


procedure TKMNetworking.GameCreated;
begin
  case fNetPlayerKind of
    lpk_Host:   begin
                  fNetPlayers[fMyIndex].ReadyToPlay := true;
                  PacketSend(NET_ADDRESS_OTHERS, mk_ReadyToPlay, '', 0);
                  SendPlayerListAndRefreshPlayersSetup; //Initialise the in-game player setup
                  //Check this here because it is possible to start a multiplayer game without other humans, just AI (at least for debugging)
                  TryPlayGame;
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
  LocID,TeamID,ColorID,PlayerIndex:integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message
  if not Connected then exit;

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
    //When querying or reconnecting to a host we may receive data such as commands, player setup, etc. These should be ignored.
    if not (fNetGameState in [lgs_Query,lgs_Reconnecting]) then
    begin
      flog.AppendLog('Received a packet not intended for this state ('+GetEnumName(TypeInfo(TNetGameState), Integer(fNetGameState))+'): '+GetEnumName(TypeInfo(TKMessageKind), Integer(Kind)));
      PostLocalMessage('Error: Received a packet not intended for this state: '+GetEnumName(TypeInfo(TKMessageKind), Integer(Kind)));
    end;
    Exit;
  end;

  case Kind of
    mk_GameVersion:
            if Msg <> NET_PROTOCOL_REVISON then
            begin
              Assert(not IsHost);
              fOnJoinFail(Format(fTextLibrary[TX_MP_MENU_WRONG_VERSION], [NET_PROTOCOL_REVISON, Msg]));
              fNetClient.Disconnect;
              exit;
            end;

    mk_WelcomeMessage:
            begin
              fWelcomeMessage := Msg;
            end;

    mk_ServerName:
            begin
              fServerName := Msg;
            end;

    mk_HostingRights:
            begin
              fNetPlayerKind := lpk_Host;
              if Assigned(fOnJoinAssignedHost) then fOnJoinAssignedHost(Self); //Enter the lobby if we had hosting rights assigned to us
              PostLocalMessage(fTextLibrary[TX_LOBBY_HOST_RIGHTS],false);
            end;

    mk_IndexOnServer:
            begin
              fMyIndexOnServer := Param;
              //PostLocalMessage('Index on Server - ' + inttostr(fMyIndexOnServer));
              //Now join the room we planned to
              PacketSend(NET_ADDRESS_SERVER, mk_JoinRoom, '', fRoomToJoin);
            end;

    mk_ConnectedToRoom:
            begin
              //We are now clear to proceed with our business
              if fNetGameState = lgs_Reconnecting then
              begin
                if IsHost then
                begin
                  if WRITE_RECONNECT_LOG then fLog.AppendLog('Hosting reconnection');
                  //The other players must have been disconnected too, so we will be the host now
                  SetGameState(lgs_Game); //We are now in control of the game, so we are no longer reconnecting
                  //At this point we now know that every other client was dropped, but we probably missed the disconnect messages
                  fNetPlayers.DisconnectAllClients(fMyNikname); //Mark all human players as disconnected, except for self
                  //Set our new index on server
                  fNetPlayers[fNetPlayers.NiknameToLocal(fMyNikname)].SetIndexOnServer := fMyIndexOnServer;
                end
                else
                begin
                  PacketSend(NET_ADDRESS_HOST, mk_AskToReconnect, fMyNikname, 0);
                  fJoinTimeout := GetTickCount; //Wait another X seconds for host to reply before timing out
                  if WRITE_RECONNECT_LOG then fLog.AppendLog('Asking to reconnect');
                end;
              end
              else
                case fNetPlayerKind of
                  lpk_Host:
                      begin
                        fNetPlayers.AddPlayer(fMyNikname, fMyIndexOnServer, fMyLang);
                        fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
                        fNetPlayers[fMyIndex].ReadyToStart := true;
                        if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                        SetGameState(lgs_Lobby);
                        fSoundLib.Play(sfxn_MPChatMessage); //Sound for joining the lobby
                        if fWelcomeMessage <> '' then PostLocalMessage(fWelcomeMessage, false);
                      end;
                  lpk_Joiner:
                  begin
                      SetGameState(lgs_Query);
                      fJoinTimeout := GetTickCount; //Wait another X seconds for host to reply before timing out
                      PacketSend(NET_ADDRESS_HOST, mk_AskToJoin, fMyNikname, 0);
                  end;
                end;
            end;

    mk_AskToReconnect:
            begin
              PlayerIndex := fNetPlayers.NiknameToLocal(Msg);
              ReMsg := fNetPlayers.CheckCanReconnect(PlayerIndex);
              if WRITE_RECONNECT_LOG then fLog.AppendLog(Msg+' asked to reconnect: '+ReMsg);
              if ReMsg = '' then
              begin
                PostMessage(Msg+' has reconnected');
                fNetPlayers[PlayerIndex].SetIndexOnServer := aSenderIndex; //They will have a new index
                fNetPlayers[PlayerIndex].Connected := true; //This player is now back online
                SendPlayerListAndRefreshPlayersSetup;
                PacketSend(aSenderIndex, mk_ReconnectionAccepted, '', Integer(fLastProcessedTick)); //Tell this client they are back in the game
                PacketSend(NET_ADDRESS_OTHERS, mk_ClientReconnected, '', aSenderIndex); //Tell everyone to ask him to resync
                PacketSend(aSenderIndex, mk_ResyncFromTick, '', Integer(fLastProcessedTick)); //Ask him to resync us
              end
              else
                PacketSend(aSenderIndex, mk_RefuseReconnect, ReMsg, 0);
            end;

    mk_RefuseReconnect:
            begin
              PostLocalMessage('Reconnection failed: '+Msg);
              if Assigned(fOnJoinFail) then fOnJoinFail(Msg);
            end;

    mk_AskToJoin:
            if IsHost then begin
              ReMsg := fNetPlayers.CheckCanJoin(Msg, aSenderIndex);
              if (ReMsg = '') and (fNetGameState <> lgs_Lobby) then
                ReMsg := 'Cannot join while the game is in progress';
              if ReMsg = '' then
              begin
                fNetPlayers.AddPlayer(Msg, aSenderIndex);
                PacketSend(aSenderIndex, mk_AllowToJoin, '', 0);
                SendMapOrSave; //Send the map first so it doesn't override starting locs

                if fSelectGameKind = ngk_Save then MatchPlayersToSave(fNetPlayers.ServerToLocal(aSenderIndex)); //Match only this player
                SendPlayerListAndRefreshPlayersSetup;
                SendGameOptions;
                PostMessage(Msg+' has joined');
              end
              else
                PacketSend(aSenderIndex, mk_RefuseToJoin, ReMsg, 0);
            end;

    mk_LangCode:
            begin
              PlayerIndex := fNetPlayers.ServerToLocal(aSenderIndex);
              if PlayerIndex <> -1 then
                fNetPlayers[PlayerIndex].LangCode := Msg;
              SendPlayerListAndRefreshPlayersSetup;
            end;

    mk_AllowToJoin:
            if fNetPlayerKind = lpk_Joiner then
            begin
              fOnJoinSucc(Self); //Enter lobby
              SetGameState(lgs_Lobby);
              fSoundLib.Play(sfxn_MPChatMessage); //Sound for joining the lobby
              if fWelcomeMessage <> '' then PostLocalMessage(fWelcomeMessage,false);
              PacketSend(NET_ADDRESS_HOST, mk_LangCode, fMyLang, 0);
            end;

    mk_RefuseToJoin:
            if fNetPlayerKind = lpk_Joiner then begin
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_Kicked:
            begin
              fOnDisconnect(Msg);
            end;

    mk_ClientLost:
            if IsHost then
            begin
              PlayerIndex := fNetPlayers.ServerToLocal(Param);
              if PlayerIndex = -1 then exit; //Has already disconnected or not from our room
              if not fNetPlayers[PlayerIndex].Dropped then
              begin
                PostMessage(fNetPlayers[PlayerIndex].Nikname+' lost connection');
                if WRITE_RECONNECT_LOG then fLog.AppendLog(fNetPlayers[PlayerIndex].Nikname+' lost connection');
              end;
              if fNetGameState = lgs_Game then
                fNetPlayers.DisconnectPlayer(Param)
              else
                if fNetGameState = lgs_Loading then
                begin
                  fNetPlayers.DropPlayer(Param);
                  TryPlayGame;
                end
                else
                  fNetPlayers.RemPlayer(Param);
              SendPlayerListAndRefreshPlayersSetup;
            end
            else
              if fNetPlayers.ServerToLocal(Param) <> -1 then
              begin
                if fNetGameState = lgs_Game then
                  fNetPlayers.DisconnectPlayer(Param)
                else
                  if fNetGameState = lgs_Loading then
                    fNetPlayers.DropPlayer(Param)
                  else
                    fNetPlayers.RemPlayer(Param); //Remove the player anyway as it might be the host that was lost
              end;

    mk_Disconnect:
            case fNetPlayerKind of
              lpk_Host:
                  begin
                    if fNetPlayers.ServerToLocal(aSenderIndex) = -1 then exit; //Has already disconnected
                    PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Nikname+' has quit');
                    if fNetGameState in [lgs_Loading, lgs_Game] then
                      fNetPlayers.DropPlayer(aSenderIndex)
                    else
                      fNetPlayers.RemPlayer(aSenderIndex);
                    SendPlayerListAndRefreshPlayersSetup;
                  end;
              lpk_Joiner:
                  begin
                    PlayerIndex := fNetPlayers.ServerToLocal(aSenderIndex);
                    if PlayerIndex = -1 then exit; //Has already disconnected
                    PostLocalMessage(Format(fTextLibrary[TX_MULTIPLAYER_HOST_DISCONNECTED], [fNetPlayers[PlayerIndex].Nikname]));
                    if fNetGameState in [lgs_Loading, lgs_Game] then
                      fNetPlayers.DropPlayer(aSenderIndex)
                    else
                      fNetPlayers.RemPlayer(aSenderIndex);
                  end;
            end;

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
                               SendGameOptions; //Only needs to be sent when in the lobby. Our version becomes standard.
                             end;
                lgs_Loading: begin
                               if Assigned(fOnReadyToPlay) then fOnReadyToPlay(Self);
                               TryPlayGame;
                             end;
              end;

              SendPlayerListAndRefreshPlayersSetup;
              PostMessage('Hosting rights reassigned to '+fMyNikname);
              if WRITE_RECONNECT_LOG then fLog.AppendLog('Hosting rights reassigned to us ('+fMyNikname+')');
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

    mk_GameOptions:
            if fNetPlayerKind = lpk_Joiner then begin
              fNetGameOptions.SetAsText(Msg);
              if Assigned(fOnGameOptions) then fOnGameOptions(Self);
            end;

    mk_ResetMap:
            begin
              fSelectGameKind := ngk_None;
              FreeAndNil(fMapInfo);
              FreeAndNil(fSaveInfo);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName('None');
            end;

    mk_MapSelect:
            if fNetPlayerKind = lpk_Joiner then begin
              fSelectGameKind := ngk_Map;
              FreeAndNil(fMapInfo);
              fMapInfo := TKMapInfo.Create;
              fMapInfo.Load(Msg, true, true);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.FileName);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_MapCRC:
            if fNetPlayerKind = lpk_Joiner then
            begin
              if Integer(fMapInfo.CRC) <> Param then
              begin
                if fMapInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the map '+fMapInfo.FileName)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the map '+fMapInfo.FileName);
                FreeAndNil(fMapInfo);
                fSelectGameKind := ngk_None;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := false;
                if Assigned(fOnMapName) then fOnMapName('None');
              end
            end;

    mk_SaveSelect:
            if fNetPlayerKind = lpk_Joiner then begin
              fSelectGameKind := ngk_Save;
              FreeAndNil(fSaveInfo);
              fSaveInfo := TKMSaveInfo.Create(ExeDir + 'SavesMP\', Msg);
              fNetPlayers.ResetLocAndReady;
              if Assigned(fOnMapName) then fOnMapName(fSaveInfo.FileName);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_SaveCRC:
            if fNetPlayerKind = lpk_Joiner then
              if Integer(fSaveInfo.CRC) <> Param then
              begin
                if fSaveInfo.IsValid then
                  PostMessage('Error: '+fMyNikname+' has a different version of the save '+fSaveInfo.FileName)
                else
                  PostMessage('Error: '+fMyNikname+' does not have the save '+fSaveInfo.FileName);
                FreeAndNil(fSaveInfo);
                fSelectGameKind := ngk_None;
                if fMyIndex <> -1 then //In the process of joining
                  fNetPlayers[fMyIndex].ReadyToStart := False;
                if Assigned(fOnMapName) then fOnMapName('None');
              end;

    mk_StartingLocQuery:
            if IsHost and not fNetPlayers.HostDoesSetup then begin
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
            if IsHost and not fNetPlayers.HostDoesSetup then begin
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
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToStart := not fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToStart;
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
              if IsHost then TryPlayGame;
            end;

    mk_Play:
            if fNetPlayerKind = lpk_Joiner then PlayGame;

    mk_Commands:
            begin
              PlayerIndex := fNetPlayers.ServerToLocal(aSenderIndex);
              if (PlayerIndex<>-1) and not fNetPlayers[PlayerIndex].Dropped then
                if Assigned(fOnCommands) then fOnCommands(Msg);
            end;

    mk_ResyncFromTick:
            begin
              if WRITE_RECONNECT_LOG then fLog.AppendLog('Asked to resync from tick '+IntToStr(Param));
              PlayerIndex := fNetPlayers.ServerToLocal(aSenderIndex);
              if Assigned(fOnResyncFromTick) and (PlayerIndex<>-1) then
              begin
                if WRITE_RECONNECT_LOG then fLog.AppendLog('Resyncing player '+fNetPlayers[PlayerIndex].Nikname);
                fOnResyncFromTick(fNetPlayers[PlayerIndex].PlayerIndex.PlayerIndex,cardinal(Param));
              end;
            end;

    mk_ReconnectionAccepted:
            begin
              //The host has accepted us back into the game!
              if WRITE_RECONNECT_LOG then fLog.AppendLog('Reconnection Accepted');
              PostLocalMessage('Successfully reconnected to the game');
              SetGameState(lgs_Game); //Game is now running once again
              fReconnectRequested := 0; //Cancel any retry in progress
              //Request all other clients to resync us
              PacketSend(NET_ADDRESS_OTHERS, mk_ResyncFromTick, '', Integer(fLastProcessedTick));
            end;

    mk_ClientReconnected:
            begin
              //The host has accepted a disconnected client back into the game. Request this client to resync us
              if Param = fMyIndexOnServer then exit;
              if WRITE_RECONNECT_LOG then fLog.AppendLog('Requesting resync for reconnected client');
              PacketSend(Param, mk_ResyncFromTick, '', Integer(fLastProcessedTick));
            end;

    mk_Text:
            PostLocalMessage(Msg);
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
  PostLocalMessage(fTextLibrary[TX_LOBBY_GAME_STARTED],false);
  SetGameState(lgs_Loading); //Loading has begun (no further players allowed to join)
  fIgnorePings := -1; //Ignore all pings until we have finished loading

  case fSelectGameKind of
    ngk_Map:  fOnStartMap(fMapInfo.FileName);
    ngk_Save: fOnStartSave(fSaveInfo.FileName);
    else      Assert(False);
  end;
end;


procedure TKMNetworking.TryPlayGame;
begin
  if fNetPlayers.AllReadyToPlay then
  begin
    PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0);
    PlayGame;
  end;
end;


procedure TKMNetworking.PlayGame;
begin
  fIgnorePings := 5; //Ignore the next few pings as they will have been measured during loading
  SetGameState(lgs_Game); //The game has begun (no further players allowed to join)
  if Assigned(fOnPlay) then fOnPlay(Self);
end;


procedure TKMNetworking.SetGameState(aState:TNetGameState);
begin
  fNetGameState := aState;
  if (fNetGameState in [lgs_Lobby,lgs_Loading,lgs_Game]) and IsHost and (fMyIndexOnServer <> -1) then
    fOnMPGameInfoChanged(Self);
end;


procedure TKMNetworking.SendMPGameInfo(aGameTime:TDateTime; aMap:string);
var MPGameInfo: TMPGameInfo;
begin
  if not IsHost then exit;
  MPGameInfo := TMPGameInfo.Create;
  if (fNetGameState in [lgs_Lobby,lgs_Loading]) then
  begin
    if GameInfo <> nil then
      aMap := GameInfo.Title
    else
      aMap := '';
  end;
  if (fNetGameState in [lgs_Lobby,lgs_Loading]) then aGameTime := -1;
  MPGameInfo.Map := aMap;
  MPGameInfo.GameTime := aGameTime;
  MPGameInfo.GameState := NetMPGameState[fNetGameState];
  MPGameInfo.Players := fNetPlayers.GetSimpleAsText;
  MPGameInfo.PlayerCount := fNetPlayers.GetConnectedCount;
  PacketSend(NET_ADDRESS_SERVER,mk_SetGameInfo,MPGameInfo.GetAsText,0);
  MPGameInfo.Free;
end;


procedure TKMNetworking.UpdateMultiplayerTeams;
var I,K: Integer;
begin
  for I := 1 to fNetPlayers.Count do
    for K := 1 to fNetPlayers.Count do
      if (fNetPlayers[I].Team = 0) or (fNetPlayers[I].Team <> fNetPlayers[K].Team) then
        fNetPlayers[I].PlayerIndex.Alliances[fNetPlayers[K].PlayerIndex.PlayerIndex] := at_Enemy
      else
        fNetPlayers[I].PlayerIndex.Alliances[fNetPlayers[K].PlayerIndex.PlayerIndex] := at_Ally;
end;


procedure TKMNetworking.UpdateState(aTick: cardinal);
begin
  //Reconnection delay
  if (fReconnectRequested <> 0) and (abs(GetTickCount - fReconnectRequested) > RECONNECT_PAUSE) then DoReconnection;
  //Joining timeout
  if fNetGameState in [lgs_Connecting,lgs_Reconnecting,lgs_Query] then
    if (GetTickCount-fJoinTimeout > JOIN_TIMEOUT) and (fReconnectRequested = 0) then
      if Assigned(fOnJoinFail) then fOnJoinFail('Query timed out');
end;


procedure TKMNetworking.UpdateStateIdle;
begin
  fNetServer.UpdateState; //Server measures pings etc.
  //LNet requires network update calls unless it is being used as visual components
  fNetClient.UpdateStateIdle;
  fServerQuery.UpdateStateIdle;
end;


end.
