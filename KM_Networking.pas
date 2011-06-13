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

  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
  private
    fNetServer:TKMNetServer;
    fNetClient:TKMNetClient;
    fLANPlayerKind: TLANPlayerKind; //Our role (Host or Joiner)
    fHostAddress:string;
    fMyNikname:string;
    fMyIndexOnServer:integer;
    fMyIndex:integer; //In NetPlayers list
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
    procedure SendCommands(aStream:TKMemoryStream; aPlayerID:TPlayerID=play_none);

    property OnJoinSucc:TNotifyEvent write fOnJoinSucc;         //We were allowed to join
    property OnJoinFail:TStringEvent write fOnJoinFail;         //We were refused to join

    property OnPlayersSetup:TNotifyEvent write fOnPlayersSetup; //Player list updated
    property OnMapName:TStringEvent write fOnMapName;           //Map name updated
    property OnStartGame:TNotifyEvent write fOnStartGame;       //Start the game loading
    property OnPlay:TNotifyEvent write fOnPlay;                 //Start the gameplay
    property OnPingInfo:TNotifyEvent write fOnPingInfo;         //Ping info updated

    property OnDisconnect:TStringEvent write fOnDisconnect;     //Lost connection, was kicked
    property OnCommands:TStringEvent write fOnCommands;         //Recieved GIP commands

    property OnTextMessage:TStringEvent write fOnTextMessage;   //Text message recieved

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


//Startup a local server and connect to it as ordinary client
procedure TKMNetworking.Host(aUserName:string);
begin
  fNetServer.StopListening;

  fNetServer.OnStatusMessage := fOnTextMessage;
  fNetServer.StartListening(KAM_PORT);

  Join('127.0.0.1', aUserName);
  fLANPlayerKind := lpk_Host; //todo: Instead of overriding our role - let Server assign Host rights to us (possibly along with IndexOnServer message)
end;


procedure TKMNetworking.Join(aServerAddress,aUserName:string);
begin
  Assert(not fNetClient.Connected, 'We were not properly disconnected');

  fMyIndex := -1; //Host will send us PlayerList and we will get our index from there
  fMyIndexOnServer := -1; //Assigned by Server

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


//Connection was successfull, but we still need mk_IndexOnServer to be able to do anything
procedure TKMNetworking.ConnectSucceed(Sender:TObject);
begin
  if Assigned(fOnTextMessage) then
    fOnTextMessage('Connection successfull');
end;


procedure TKMNetworking.ConnectFailed(const S: string);
begin
  fNetClient.Disconnect;
  fOnJoinFail(S);
end;


//Send message that we have deliberately disconnected
procedure TKMNetworking.LeaveLobby;
begin
  PacketSend(NET_ADDRESS_OTHERS, mk_Disconnect, '', 0);
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
  fOnPingInfo := nil;

  fNetPlayers.Clear;
  fNetClient.Disconnect;
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
  PingValue:integer;
begin
  M := TKMemoryStream.Create;
  M.WriteAsText(aInfo);
  M.Position := 0;
  M.Read(PingCount);
  for i:=1 to PingCount do
  begin
    M.Read(PlayerHandle);
    M.Read(PingValue);
    fNetPlayers[fNetPlayers.ServerToLocal(PlayerHandle)].Ping := PingValue;
  end;
  M.Free;
end;


//Tell other players which map we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.SelectMap(aName:string);
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can select maps');

  fMapInfo.Load(aName, true);
  fNetPlayers.ResetLocAndReady; //Reset start locations

  if not fMapInfo.IsValid then exit;

  PacketSend(NET_ADDRESS_OTHERS, mk_MapSelect, fMapInfo.Folder, 0 {Integer(fMapInfo.CRC)}); //todo: Send CRC as well
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
  fNetPlayers[fMyIndex].StartLocation := aIndex;

  case fLANPlayerKind of
    lpk_Host:   begin
                  PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: PacketSend(NET_ADDRESS_HOST, mk_StartingLocQuery, '', aIndex);
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
                  PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: begin
                  PacketSend(NET_ADDRESS_HOST, mk_FlagColorQuery, '', aIndex);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
  end;
end;


//Joiner indicates that he is ready to start
procedure TKMNetworking.ReadyToStart;
begin
  PacketSend(NET_ADDRESS_HOST, mk_ReadyToStart, '', 0);
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
  PacketSend(NET_ADDRESS_OTHERS, mk_Start, fNetPlayers.GetAsText, 0);

  StartGame;
end;


//Send request to Server to ping everyone
procedure TKMNetworking.Ping;
begin
  PacketSend(NET_ADDRESS_SERVER, mk_AskPingInfo, '', 0);
end;


//We route the message through Server to ensure everyone sees messages in the same order
//with exact same timestamps (possibly added by Server?)
procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketSend(NET_ADDRESS_ALL, mk_Text, aText, 0);
end;


//Send our commands to either to all players, or to specified one
procedure TKMNetworking.SendCommands(aStream:TKMemoryStream; aPlayerID:TPlayerID=play_none);
var i:integer;
begin
  if aPlayerID = play_none then
    PacketSend(NET_ADDRESS_OTHERS, mk_Commands, aStream.ReadAsText, 0) //Send commands to all players
  else
  for i:=1 to fNetPlayers.Count do //todo: optimize and error-check
    if fNetPlayers[i].PlayerIndex.PlayerID = aPlayerID then   //TODO: ERROR HERE
      PacketSend(fNetPlayers[i].IndexOnServer, mk_Commands, aStream.ReadAsText, 0);
end;


procedure TKMNetworking.GameCreated;
begin
  case fLANPlayerKind of
    lpk_Host:   fNetPlayers[fMyIndex].ReadyToPlay := true;
    lpk_Joiner: PacketSend(NET_ADDRESS_HOST, mk_ReadyToPlay, '', 0);
  end;
end;


procedure TKMNetworking.PacketRecieve(aSenderIndex:integer; aData:pointer; aLength:cardinal);
var
  Kind:TKMessageKind;
  M:TKMemoryStream;
  Param:integer;
  Msg:string;
  ReMsg:string;
  LocID,ColorID:integer;
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
            if fLANPlayerKind = lpk_Host then begin
              ReMsg := fNetPlayers.CheckCanJoin(Msg, aSenderIndex);
              if ReMsg = '' then
              begin
                fNetPlayers.AddPlayer(Msg, aSenderIndex);
                PacketSend(aSenderIndex, mk_AllowToJoin, '', 0);
                PacketSend(aSenderIndex, mk_MapSelect, fMapInfo.Folder, 0); //Send the map first so it doesn't override starting locs
                PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                PostMessage(Msg+' has joined');
              end
              else
                PacketSend(aSenderIndex, mk_RefuseToJoin, ReMsg, 0);
            end;

    mk_AllowToJoin:
            if fLANPlayerKind = lpk_Joiner then
              fOnJoinSucc(Self); //Enter lobby

    mk_RefuseToJoin:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_Disconnect:
            case fLANPlayerKind of
              lpk_Host:
                  begin
                    fNetPlayers.RemPlayer(aSenderIndex);
                    PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
                    if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                    PostMessage(fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Nikname+' has quit');
                  end;
              lpk_Joiner:
                  begin
                    fNetClient.Disconnect;
                    if Assigned(fOnDisconnect) then
                      fOnDisconnect('Host has quit');
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

    mk_MapSelect:
            if fLANPlayerKind = lpk_Joiner then begin
              fMapInfo.Load(Msg, true);
              fNetPlayers.ResetLocAndReady; //We can ignore Hosts "Ready" flag for now
              if Assigned(fOnMapName) then fOnMapName(fMapInfo.Folder);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_StartingLocQuery:
            if fLANPlayerKind = lpk_Host then begin
              LocID := Param;
              if fMapInfo.IsValid and
                 (LocID <= fMapInfo.PlayerCount) and
                 fNetPlayers.LocAvailable(LocID) then
              begin //Update Players setup
                fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].StartLocation := LocID;
                PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              end
              else //Quietly refuse
                PacketSend(aSenderIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
            end;

    mk_FlagColorQuery:
            if fLANPlayerKind = lpk_Host then begin
              ColorID := Param;
              //The player list could have changed since the joiner sent this request (over slow connection)
              if fNetPlayers.ColorAvailable(ColorID) then
              begin
                fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].FlagColorID := ColorID;
                PacketSend(NET_ADDRESS_OTHERS, mk_PlayersList, fNetPlayers.GetAsText, 0);
              end
              else //Quietly refuse
                PacketSend(aSenderIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ReadyToStart:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToStart := true;
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_Start:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg);
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              StartGame;
            end;

    mk_ReadyToPlay:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].ReadyToPlay := true;
              if fNetPlayers.AllReadyToPlay then
              begin
                PacketSend(NET_ADDRESS_OTHERS, mk_Play, '', 0); //todo: Should include lag difference
                if Assigned(fOnPlay) then fOnPlay(Self);
              end;
            end;

    mk_Play:
            if fLANPlayerKind = lpk_Joiner then
              if Assigned(fOnPlay) then fOnPlay(Self);

    mk_Commands:
            begin
              if Assigned(fOnCommands) then fOnCommands(Msg);
            end;

    mk_Text:
            begin
              if Assigned(fOnTextMessage) then
                fOnTextMessage(fNetPlayers[fNetPlayers.ServerToLocal(aSenderIndex)].Nikname +': '+ Msg);
            end;
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


procedure TKMNetworking.UpdateState;
begin
  //Nothing yet
end;


end.
