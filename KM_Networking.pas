unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_CommonTypes, KM_Network, KM_Player, KromUtils, SysUtils, StrUtils, Math, Windows;


type TStringEvent = procedure (const aData: string) of object;
type TStreamEvent = procedure (aData: TKMemoryStream) of object;

type TLANPlayerKind = (lpk_Host, lpk_Joiner);

type TMessageKind = (
                      mkUnknown,
                      mk_AskToJoin,
                      mk_Timeout,
                      mk_AllowToJoin,
                      //mk_RefuseToJoin, //When max players is exceeded
                      mk_VerifyJoin,
                      mk_PlayersList,
                      mk_MapSelect,

                      mk_Text,
                      mk_GameSetup,
                      mk_Gameplay);

type
  TKMPlayerInfo = class
    Addr:string;
    Nikname:string;
    PlayerType:TPlayerType;
    FlagColor:cardinal;
    StartLocID:integer;
    Alliances:array[1..MAX_PLAYERS] of TAllianceType;
    ReadyToStart:boolean;
  end;


type //Should handle everything related to players list
  TKMPlayersList = class
    private
      fCount:integer;
      fMyself:string; //
      fPlayers:array [1..MAX_PLAYERS] of TKMPlayerInfo;
      function GetAsText:string;
      procedure SetAsText(aText:string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property Myself:string read fMyself;
      procedure AddSelf(aAddr:string);
      procedure AddPlayer(aAddr:string);
      function AllReady:boolean;
      property AsText:string read GetAsText write SetAsText;
      procedure PacketSend(aKind:TMessageKind; const aData:string=''); //Broadcast to all except self
      procedure EncodeGameSetup(aStream:TKMemoryStream);
    end;


type //Should handle message exchange and routing, interacting with UI
  TKMNetworking = class
    private
      fNetwork:TKMNetwork;
      fLANPlayerKind: TLANPlayerKind;
      fServerAddress:string; //Who's the host

      MapName:string;

      fPlayers:TKMPlayersList;

      JoinTick:cardinal;
      fOnJoinSucc:TNotifyEvent;
      fOnJoinFail:TNotifyEvent;
      fOnTextMessage:TStringEvent;
      fOnPlayersList:TStringEvent;
      fOnMapName:TStringEvent;
      fOnCommands:TStreamEvent;

      procedure EncodeGameSetup(aStream:TKMemoryStream);

      procedure PacketRecieve(const aData: array of byte; aAddr:string); //Process all commands
      procedure PacketRecieveJoin(const aData: array of byte; aAddr:string); //Process only "Join" commands
      procedure PacketSend(const aAddress:string; aKind:TMessageKind; const aData:string='');
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
      property OnCommands:TStreamEvent write fOnCommands;
      procedure UpdateState;
    end;


implementation


constructor TKMPlayersList.Create;
var i:integer;
begin
  for i:=1 to MAX_PLAYERS do
    fPlayers[i] := TKMPlayerInfo.Create;
end;


destructor TKMPlayersList.Destroy;
var i:integer;
begin
  for i:=1 to MAX_PLAYERS do
    fPlayers[i].Free;
  Inherited;
end;


procedure TKMPlayersList.Clear;
begin
  fCount := 0;
end;


function TKMPlayersList.GetAsText:string;
var i:integer;
begin
  Result := '';
  for i:=1 to fCount do
    Result := Result + fPlayers[i].Addr + eol;
end;


procedure TKMPlayersList.SetAsText(aText:string);
begin
  //
end;


procedure TKMPlayersList.AddSelf(aAddr:string);
begin
  fMyself := aAddr;
  AddPlayer(aAddr);
end;


procedure TKMPlayersList.AddPlayer(aAddr:string);
var i:integer;
begin
  inc(fCount);
  fPlayers[fCount].Addr := aAddr;
  fPlayers[fCount].Nikname := 'Player '+inttostr(fCount); //We will add it later
  fPlayers[fCount].PlayerType := pt_Human;
  fPlayers[fCount].FlagColor := 0;
  fPlayers[fCount].StartLocID := fCount;
  for i:=1 to MAX_PLAYERS do
    fPlayers[fCount].Alliances[i] := at_Enemy;
  fPlayers[fCount].ReadyToStart := false;
end;


function TKMPlayersList.AllReady:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    Result := Result and fPlayers[i].ReadyToStart;
end;


//Broadcast to all except self
procedure TKMPlayersList.PacketSend(aKind:TMessageKind; const aData:string='');
var i:integer;
begin
  for i:=1 to fCount do
    if (fPlayers[i].PlayerType <> pt_Computer) and (fPlayers[i].Addr <> fMyself) then
      //fNetworking.SendTo(fPlayers[i].Addr, char(aKind) + aData);
end;


procedure TKMPlayersList.EncodeGameSetup(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Write(fCount);
  for i:=1 to fCount do begin
    //aStream.Write();
  end;
end;


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
  JoinTick := 0;
  fLANPlayerKind := lpk_Host;
  fPlayers.Clear;
  fPlayers.AddSelf(MyIPString);
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsText);
end;


procedure TKMNetworking.Connect(aServerAddress,aUserName:string);
begin
  fServerAddress := aServerAddress;
  fLANPlayerKind := lpk_Joiner;
  JoinTick := GetTickCount + 3000; //3sec
  fPlayers.Clear;
  fPlayers.AddSelf(MyIPString);
  fNetwork.StartListening;
  PacketSend(fServerAddress, mk_AskToJoin);
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

  MapName := aName;
  fPlayers.PacketSend(mk_MapSelect, MapName);

  if Assigned(fOnMapName) then fOnMapName(MapName);

  //Compare map availability and CRC
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
  aStream.Write(MapName);
  //aStream.Write(AllianceMode); //Fixed / changeable
  //aStream.Write(StartupConditions);
  //aStream.Write(WinConditions);
  //aStream.Write(DefeatConditions);
  fPlayers.EncodeGameSetup(aStream);
end;


procedure TKMNetworking.PostMessage(aText:string);
begin
  fPlayers.PacketSend(mk_Text, fPlayers.Myself + ': ' + aText);
  fOnTextMessage(fPlayers.Myself + ': ' + aText);
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
                      fPlayers.AddPlayer(aAddr);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsText);
                      fPlayers.PacketSend(mk_PlayersList, fPlayers.AsText);
                      PostMessage(aAddr+' has joined');
                    end;
    mk_PlayersList: begin
                      fPlayers.AsText := Data;
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayers.AsText);
                    end;
    mk_MapSelect:   begin
                      MapName := Data;
                      if Assigned(fOnMapName) then fOnMapName(MapName);
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
                      JoinTick := 0;
                      fNetwork.OnRecieveKMPacket := PacketRecieve;
                      PacketSend(fServerAddress, mk_VerifyJoin);
                      fOnJoinSucc(Self);
                      //if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
                    end;
    mk_Timeout:     begin
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


procedure TKMNetworking.UpdateState;
const MyArray : array[0..0] of byte = (byte(mk_Timeout)); //Convert byte to array
begin
  if (JoinTick<>0) and (JoinTick <= GetTickCount) then
    PacketRecieveJoin(MyArray, '127.0.0.1'); //Time is up, wait no longer
end;


end.
