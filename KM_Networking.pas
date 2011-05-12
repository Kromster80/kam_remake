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
    fOnPing:TNotifyEvent;
    fOnCommands:TStringEvent;

    procedure ConnectSucceed(Sender:TObject);
    procedure ConnectFailed(const S: string);
    procedure PacketRecieve(aData:pointer; aLength:cardinal); //Process all commands
    procedure PacketToAll(aKind:TKMessageKind; const aText:string; aParam:integer);
    procedure PacketToHost(aKind:TKMessageKind; const aText:string; aParam:integer);
    procedure PacketSend(aIndexOnServer:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
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
  fNetServer.OnStatusMessage := fOnTextMessage;
  fNetServer.StartListening(KAM_PORT);

  fHostAddress := ''; //Thats us
  fMyNikname := aUserName;
  fMyIndexOnServer := -1; //Unknown yet
  fMyIndex := 1;
  fLANPlayerKind := lpk_Host;

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
  fMyIndexOnServer := -1; //Unknown yet
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
  fOnTextMessage(MyIPString + ' Connected to server');
  //Now wait for mk_IndexOnServer message
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
    lpk_Host:   PacketToAll(mk_HostDisconnect, '', 0);
    lpk_Joiner: PacketToHost(mk_Disconnect, fMyNikname, fMyIndexOnServer);
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

  fNetServer.OnStatusMessage := nil;
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

  PacketToAll(mk_MapSelect, fMapInfo.Folder, Integer(fMapInfo.CRC)); //todo: Send map name and CRC
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
                  PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: PacketToHost(mk_StartingLocQuery, chr(aIndex), 0);
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
                  PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
    lpk_Joiner: begin
                  PacketToHost(mk_FlagColorQuery, chr(aIndex), 0);
                  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                end;
  end;
end;


//Joiner indicates that he is ready to start
procedure TKMNetworking.ReadyToStart;
begin
  PacketToHost(mk_ReadyToStart, '', 0);
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
  PacketToAll(mk_Start, fNetPlayers.GetAsText, 0);

  StartGame;
end;


procedure TKMNetworking.Ping;
begin
  //todo: Send request to Server to ping everyone
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketToAll(mk_Text, MyIPString + '/' + fMyNikname + ': ' + aText, 0);
  //fOnTextMessage(MyIPString + '/' + fMyNikname + ': ' + aText);
end;


//Send our commands to either to all players, or to specified one
procedure TKMNetworking.SendCommands(aStream:TKMemoryStream; aPlayerLoc:byte=0);
var i:integer;
begin
  if aPlayerLoc = 0 then
    PacketToAll(mk_Commands, aStream.ReadAsText, 0) //Send commands to all players
  else
  for i:=1 to fNetPlayers.Count do //todo: optimize and error-check
    if fNetPlayers[i].StartLocID = aPlayerLoc then
      PacketSend(fNetPlayers[i].IndexOnServer, mk_Commands, aStream.ReadAsText, 0);
end;


procedure TKMNetworking.GameCreated;
begin
  case fLANPlayerKind of
    lpk_Host:   fNetPlayers[fMyIndex].ReadyToPlay := true;
    lpk_Joiner: PacketToHost(mk_ReadyToPlay, '', 0);
  end;
end;


procedure TKMNetworking.PacketRecieve(aData:pointer; aLength:cardinal);
var
  Kind:TKMessageKind;
  M:TKMemoryStream;
  Msg:string;
  ReMsg:string;
  LocID,ColorID:integer;
  SenderIndex:integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;

  M.Read(SenderIndex);
  M.Read(Kind, SizeOf(TKMessageKind));

  case Kind of
    mk_IndexOnServer:
            begin
              M.Read(fMyIndexOnServer);
              if Assigned(fOnTextMessage) then fOnTextMessage('Index on Server - ' + inttostr(fMyIndexOnServer));
              case fLANPlayerKind of
                lpk_Host:
                    begin
                      fNetPlayers.Clear;
                      fNetPlayers.AddPlayer(fMyNikname, fMyIndexOnServer);
                      fNetPlayers[fMyIndex].ReadyToStart := true;
                    end;
                lpk_Joiner:
                    PacketToHost(mk_AskToJoin, fMyNikname, 0);
              end;
            end;

    mk_AskToJoin:
            if fLANPlayerKind = lpk_Host then
            begin
              M.Read(Msg); //Players Nikname
              ReMsg := fNetPlayers.CheckCanJoin(Msg, SenderIndex);
              if ReMsg = '' then
              begin
                fNetPlayers.AddPlayer(Msg, SenderIndex);
                PacketSend(SenderIndex, mk_AllowToJoin, '', 0);
                PacketSend(SenderIndex, mk_MapSelect, fMapInfo.Folder, 0); //Send the map first so it doesn't override starting locs
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
                PostMessage(Msg+' has joined');
              end
              else
                PacketSend(SenderIndex, mk_RefuseToJoin, ReMsg, 0);
            end;

    mk_AllowToJoin:
            if fLANPlayerKind = lpk_Joiner then
              fOnJoinSucc(Self); //Enter lobby

    mk_RefuseToJoin:
            if fLANPlayerKind = lpk_Joiner then
            begin
              M.Read(Msg); //Contains error description
              fNetClient.OnRecieveData := nil;
              fNetClient.Disconnect;
              fOnJoinFail(Msg);
            end;

    mk_Disconnect:
            if fLANPlayerKind = lpk_Host then
            begin
              M.Read(Msg);
              fNetPlayers.RemPlayer(SenderIndex);
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
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

    mk_Ping:
            begin
              M.Read(Msg);
              PacketSend(SenderIndex, mk_Pong, '', 0); //Server will intercept this message
            end;

    mk_PlayersList:
            if fLANPlayerKind = lpk_Joiner then begin
              M.Read(Msg);
              fNetPlayers.SetAsText(Msg); //Our index could have changed on players add/removal
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
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
              LocID := byte(Msg[1]); //Location index
              //Check if position can't be taken
              if fMapInfo.IsValid and
                 (LocID <= fMapInfo.PlayerCount) and
                 fNetPlayers.LocAvailable(LocID) then
              begin //Update Players setup
                fNetPlayers[fNetPlayers.ServerToLocal(SenderIndex)].StartLocID := LocID;
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              end
              else //Quietly refuse
                PacketSend(SenderIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
            end;

    mk_FlagColorQuery:
            if fLANPlayerKind = lpk_Host then begin
              M.Read(Msg);
              ColorID := byte(Msg[1]); //Color index
              //The player list could have changed since the joiner sent this request (over slow connection)
              if fNetPlayers.ColorAvailable(ColorID) then
              begin
                fNetPlayers[fNetPlayers.ServerToLocal(SenderIndex)].FlagColorID := ColorID;
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText, 0);
              end
              else //Quietly refuse
                PacketSend(SenderIndex, mk_PlayersList, fNetPlayers.GetAsText, 0);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ReadyToStart:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.ServerToLocal(SenderIndex)].ReadyToStart := true;
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_Start:
            if fLANPlayerKind = lpk_Joiner then begin
              M.Read(Msg);
              fNetPlayers.SetAsText(Msg);
              fMyIndex := fNetPlayers.NiknameToLocal(fMyNikname);
              StartGame;
            end;

    mk_ReadyToPlay:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.ServerToLocal(SenderIndex)].ReadyToPlay := true;
              if fNetPlayers.AllReadyToPlay then
              begin
                PacketToAll(mk_Play, '', 0); //todo: Should include lag difference
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

  M.Free;
end;


procedure TKMNetworking.PacketToAll(aKind:TKMessageKind; const aText:string; aParam:integer);
begin
  PacketSend(NET_RECIPIENT_ALL, aKind, aText, aParam);
end;


procedure TKMNetworking.PacketToHost(aKind:TKMessageKind; const aText:string; aParam:integer);
begin
  Assert(fLANPlayerKind = lpk_Joiner, 'Only joined player can send data to Host');
  PacketSend(NET_RECIPIENT_HOST, aKind, aText, aParam);
end;


// Sender.MessageKind.TextData
procedure TKMNetworking.PacketSend(aIndexOnServer:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.Write(fMyIndexOnServer);
  M.Write(aKind, SizeOf(TKMessageKind));
  M.Write(aText);
  M.Write(aParam);
  fNetClient.SendData(aIndexOnServer, M.Memory, M.Size); //todo: Add recipient index, so Server route the message only to him
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
