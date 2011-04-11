unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, StrUtils, SysUtils, Windows,
  KM_CommonTypes, KM_Defaults,
  KM_NetPlayersList, KM_Network;

  
const
    REPLY_TIMEOUT = 3000; //Default timeout before "could not get reply" message occurs, in Game ticks


type
  TLANPlayerKind = (lpk_Host, lpk_Joiner);

  TMessageKind = (  mk_AskToJoin,
                    mk_AllowToJoin,
                    mk_RefuseToJoin, //When nikname is taken
                    mk_VerifyJoin,

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

type
  //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
    private
      fNetwork:TKMNetwork; //Our Network interface
      fLANPlayerKind: TLANPlayerKind; //Our role (Host or Joiner)
      fHostAddress:string;
      fMyNikname:string; //Stored to identify our Index in new players list
      fMyIndex:integer;
      fNetPlayers:TKMPlayersList;

      fMapName:string;
      fMissionMode:TMissionMode;

      fJoinTick:cardinal; //Timer to issue timeout event on connection
      fLastUpdateTick:cardinal;
      fHoldTimeoutChecks:boolean;

      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TStringEvent;
      fOnTextMessage:TStringEvent;
      fOnPlayersSetup:TNotifyEvent;
      fOnMapName:TStringEvent;
      fOnStartGame:TNotifyEvent;
      fOnAllReady:TNotifyEvent;
      fOnPlay:TNotifyEvent;
      fOnDisconnect:TStringEvent;
      fOnPing:TNotifyEvent;
      fOnCommands:TStringEvent;

      procedure PacketRecieve(const aData: array of byte; aAddr:string); //Process all commands
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
      procedure SelectLoc(aIndex:integer);
      procedure SelectColor(aIndex:integer);
      procedure ReadyToStart;
      procedure StartClick; //All required arguments are in our class

      //Common
      procedure Ping;
      procedure HoldTimeoutChecks;
      procedure PostMessage(aText:string);
      property MyIndex:integer read fMyIndex;
      property LANPlayerKind:TLANPlayerKind read fLANPlayerKind;

      //Gameplay
      property MapName:string read fMapName;
      property MissionMode:TMissionMode read fMissionMode;
      property NetPlayers:TKMPlayersList read fNetPlayers;
      procedure SendCommands(aStream:TKMemoryStream);
      procedure SendConfirmation(aStream:TKMemoryStream; aPlayerLoc:byte);
      procedure GameCreated;

      property OnJoinSucc:TNotifyEvent write fOnJoinSucc;       //We were allowed to join
      property OnJoinFail:TStringEvent write fOnJoinFail;       //We were refused to join
      property OnTextMessage:TStringEvent write fOnTextMessage; //Text message recieved
      property OnPlayersSetup:TNotifyEvent write fOnPlayersSetup; //Player list updated
      property OnMapName:TStringEvent write fOnMapName;         //Map name updated
      property OnAllReady:TNotifyEvent write fOnAllReady;       //Everyones ready to start
      property OnStartGame:TNotifyEvent write fOnStartGame;       //Start the game
      property OnPlay:TNotifyEvent write fOnPlay; //Everyones ready to play
      property OnPing:TNotifyEvent write fOnPing;
      property OnDisconnect:TStringEvent write fOnDisconnect; //Lost connection, was kicked
      property OnCommands:TStringEvent write fOnCommands;       //Recieved commands
      procedure UpdateState;
    end;


implementation
uses KM_Utils;


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
  fMyNikname := aUserName;
  fMyIndex := 1;
  fLANPlayerKind := lpk_Host;
  fNetPlayers.Clear;
  fNetPlayers.AddPlayer(MyIPString, fMyNikname, 0); //Selfs tick is not important
  fNetPlayers[fMyIndex].ReadyToStart := true;
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
end;


procedure TKMNetworking.Join(aServerAddress,aUserName:string);
begin
  Disconnect;
  fHostAddress := aServerAddress;
  fMyNikname := aUserName;
  fMyIndex := -1; //Host will send us Players list and we will get our index from there
  fLANPlayerKind := lpk_Joiner;
  fJoinTick := GetTickCount + REPLY_TIMEOUT; //3sec
  fNetPlayers.Clear;
  fNetwork.StartListening;
  PacketToHost(mk_AskToJoin, fMyNikname);
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Unless we join use shortlist
end;


procedure TKMNetworking.Disconnect;
begin
  fOnJoinSucc := nil;
  fOnJoinFail := nil;
  fOnTextMessage := nil;
  fOnPlayersSetup := nil;
  fOnMapName := nil;
  fOnAllReady := nil;
  fOnCommands := nil;
  fOnDisconnect := nil;
  fOnPing := nil;
  fNetwork.StopListening;
  fNetPlayers.Clear;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetwork.fListening;
end;


//Tell other players which map we will be using
//Players will reset their starting locations and "Ready" status on their own
procedure TKMNetworking.MapSelect(aName:string);
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can select maps');

  fMapName := aName;
  PacketToAll(mk_MapSelect, fMapName);

  fNetPlayers.ResetLocAndReady;
  fNetPlayers[fMyIndex].ReadyToStart := true;
  //PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);

  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
  if Assigned(fOnMapName) then fOnMapName(fMapName);
  //Compare map availability and CRC
end;


//Tell other players which start position we would like to use
//Each players choice should be unique
procedure TKMNetworking.SelectLoc(aIndex:integer);
begin
  if not fNetPlayers.LocAvailable(aIndex) then
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
  //if not fNetPlayers.ColorAvailable(aIndex) then exit;

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
  fNetPlayers[fMyIndex].ReadyToStart := true;
  if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
  PacketToAll(mk_ReadyToStart, fMyNikname);
end;


//Tell other players we want to start
procedure TKMNetworking.StartClick;
const MapLoc = 6; //todo: Take maps MaxPlayers
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can start the game');

  //Do not allow to start the game
  if not fNetPlayers.AllReady or //We might miss a starting location
    ((fNetPlayers[fMyIndex].StartLocID = 0) and (fNetPlayers.Count > MapLoc)) then
    exit;

  //Define random parameters (start locations and flag colors)
  //This will also remove odd players from the List, they will loose Host in few seconds
  fNetPlayers.DefineSetup(MapLoc);

  //Let everyone start with final version of fNetPlayers
  PacketToAll(mk_Start, fNetPlayers.GetAsText);

  if Assigned(fOnStartGame) then fOnStartGame(Self);
end;


procedure TKMNetworking.Ping;
var i:integer;
begin
  for i:=1 to fNetPlayers.Count do
    fNetPlayers[i].PingSent := GetTickCount;

  PacketToAll(mk_Ping, fMyNikname);
end;


procedure TKMNetworking.HoldTimeoutChecks;
begin
  fHoldTimeoutChecks := true;
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  PacketToAll(mk_Text, MyIPString + '/' + fMyNikname + ': ' + aText);
  fOnTextMessage(MyIPString + '/' + fMyNikname + ': ' + aText);
end;


procedure TKMNetworking.SendCommands(aStream:TKMemoryStream);
var s:string;
begin
  SetLength(s, aStream.Size);
  aStream.WriteBuffer(s[1], aStream.Size);
  PacketToAll(mk_Commands, s); //Send commands to all players
end;


procedure TKMNetworking.SendConfirmation(aStream:TKMemoryStream; aPlayerLoc:byte);
var s:string;
begin
  SetLength(s, aStream.Size);
  aStream.WriteBuffer(s[1], aStream.Size);
  PacketToAll(mk_Commands, s); //Send commands to all players
end;


procedure TKMNetworking.GameCreated;
begin
  case fLANPlayerKind of
    lpk_Host:   fNetPlayers[fMyIndex].ReadyToPlay := true;
    lpk_Joiner: PacketToHost(mk_ReadyToPlay, fMyNikname);
  end;
end;


procedure TKMNetworking.PacketRecieve(const aData: array of byte; aAddr:string);
var
  Kind:TMessageKind;
  Msg:string;
  ReMsg:string;
  LocID,ColorID:integer;
  NikID:integer;
begin
  Assert(Length(aData) >= 1, 'Unexpectedly short message'); //Kind, Message

  Kind := TMessageKind(aData[0]);
  if Length(aData) > 1 then
    SetString(Msg, PAnsiChar(@aData[1]), Length(aData)-1)
  else
    Msg := '';

  case Kind of
    mk_AskToJoin:
            if fLANPlayerKind = lpk_Host then begin
              ReMsg := fNetPlayers.CheckCanJoin(aAddr, Msg);
              if ReMsg = '' then
                PacketSend(aAddr, mk_AllowToJoin, 'Allowed')
              else
                PacketSend(aAddr, mk_RefuseToJoin, ReMsg);
            end;

    mk_AllowToJoin:
            if fLANPlayerKind = lpk_Joiner then begin
              fJoinTick := 0;
              fNetwork.OnRecieveKMPacket := PacketRecieve;
              PacketToHost(mk_VerifyJoin, fMyNikname);
              fOnJoinSucc(Self);
            end;

    mk_RefuseToJoin:
            if fLANPlayerKind = lpk_Joiner then begin
              fJoinTick := 0;
              fNetwork.OnRecieveKMPacket := nil;
              fNetwork.StopListening;
              fOnJoinFail(Msg);
            end;

    mk_VerifyJoin:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers.AddPlayer(aAddr, Msg, GetTickCount + REPLY_TIMEOUT);
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              PacketSend(aAddr, mk_MapSelect, fMapName);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              PostMessage(aAddr+'/'+Msg+' has joined');
            end;

    mk_Poke:
            begin
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].TimeTick := GetTickCount + REPLY_TIMEOUT;
            end;

    mk_Ping:
            begin
              PacketSend(fNetPlayers[fNetPlayers.NiknameIndex(Msg)].Address, mk_Pong, fMyNikname);
            end;

    mk_Pong:
            begin
              NikID := fNetPlayers.NiknameIndex(Msg);
              fNetPlayers[NikID].Ping := GetTickCount - fNetPlayers[NikID].PingSent;
              if Assigned(fOnPing) then fOnPing(Self);
            end;

    mk_PlayersList:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg); //Our index could have changed on players add/removal
              fMyIndex := fNetPlayers.NiknameIndex(fMyNikname);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_MapSelect:
            if fLANPlayerKind = lpk_Joiner then begin
              fMapName := Msg;
              fNetPlayers.ResetLocAndReady; //We can ignore Hosts "Ready" flag for now
              if Assigned(fOnMapName) then fOnMapName(fMapName);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_StartingLocQuery:
            if fLANPlayerKind = lpk_Host then begin
              LocID := strtoint(Msg[1]); //Location index
              NikID := fNetPlayers.NiknameIndex(RightStr(Msg, length(Msg)-1)); //Player index
              if fNetPlayers.LocAvailable(LocID) then //Update Players setup
              begin
                fNetPlayers[NikID].StartLocID := LocID;
                PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
                if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              end
              else //Quietly refuse
                PacketSend(aAddr, mk_PlayersList, fNetPlayers.GetAsText);
            end;

    mk_FlagColorQuery:
            if fLANPlayerKind = lpk_Host then begin
              ColorID := strtoint(Msg[1]); //Color index
              NikID := fNetPlayers.NiknameIndex(RightStr(Msg, length(Msg)-1)); //Player index

              fNetPlayers[NikID].FlagColorID := ColorID;
              PacketToAll(mk_PlayersList, fNetPlayers.GetAsText);
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
            end;

    mk_ReadyToStart:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].ReadyToStart := true;
              if Assigned(fOnPlayersSetup) then fOnPlayersSetup(Self);
              if fNetPlayers.AllReady and (fNetPlayers.Count>1) then
                if Assigned(fOnAllReady) then fOnAllReady(nil);
            end;

    mk_Start:
            if fLANPlayerKind = lpk_Joiner then begin
              fNetPlayers.SetAsText(Msg);
              fMyIndex := fNetPlayers.NiknameIndex(fMyNikname);
              if Assigned(fOnStartGame) then fOnStartGame(Self);
            end;

    mk_ReadyToPlay:
            if fLANPlayerKind = lpk_Host then begin
              fNetPlayers[fNetPlayers.NiknameIndex(Msg)].ReadyToPlay := true;
              if fNetPlayers.AllReadyToPlay then
              begin
                PacketToAll(mk_Play,'111111111'); //todo: Should include lag difference
                if Assigned(fOnPlay) then fOnPlay(Self);
              end;
            end;

    mk_Play:
            if fLANPlayerKind = lpk_Joiner then
              if Assigned(fOnPlay) then fOnPlay(Self);

    mk_Commands:
            if Assigned(fOnCommands) then fOnCommands(Msg);

    mk_Text:
            if Assigned(fOnTextMessage) then fOnTextMessage(Msg);
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
    if fNetPlayers[i].IsHuman and (i <> fMyIndex) then
      PacketSend(fNetPlayers[i].Address, aKind, aData);
end;


procedure TKMNetworking.PacketToHost(aKind:TMessageKind; const aData:string='');
begin
  Assert(fLANPlayerKind = lpk_Joiner, 'Only joined player can send data to Host');
  PacketSend(fHostAddress, aKind, aData);
end;


procedure TKMNetworking.UpdateState;
var LostPlayers:string;
begin
  if (fJoinTick<>0) and (GetTickCount > fJoinTick) then
  begin
    fJoinTick := 0;
    fNetwork.OnRecieveKMPacket := nil;
    fNetwork.StopListening;
    fOnJoinFail('no response');
  end;

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
              fOnDisconnect('Lost connection to Host');
              exit;
            end;
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
  end;
end;


end.
