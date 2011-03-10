unit KM_Networking;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_CommonTypes, KM_Network, KromUtils, SysUtils, StrUtils, Math, Windows;


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
  TKMNetworking = class
    private
      fNetwork:TKMNetwork;
      fLANPlayerKind: TLANPlayerKind;
      fServerAddress:string; //Who's the host

      MapName:string;

      fPlayersList:TStringList; //Stores IP addresses for now

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


{ TKMNetworking }
constructor TKMNetworking.Create;
begin
  Inherited;
  fNetwork  := TKMNetwork.Create(MULTIPLE_COPIES);
  fPlayersList  := TStringList.Create;
end;


destructor TKMNetworking.Destroy;
begin
  fPlayersList.Free;
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
  fPlayersList.Add(MyIPString); //Add self
  fNetwork.StartListening;
  fNetwork.OnRecieveKMPacket := PacketRecieve; //Start listening
  if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
end;


procedure TKMNetworking.Connect(aServerAddress,aUserName:string);
begin
  fServerAddress := aServerAddress;
  fLANPlayerKind := lpk_Joiner;
  JoinTick := GetTickCount + 3000; //3sec
  fNetwork.StartListening;
  PacketSend(fServerAddress, mk_AskToJoin);
  fNetwork.OnRecieveKMPacket := PacketRecieveJoin; //Unless we join use shortlist
end;


procedure TKMNetworking.Disconnect;
begin
  fNetwork.StopListening;
  fPlayersList.Clear;
end;


function TKMNetworking.Connected: boolean;
begin
  Result := fNetwork.fListening;
end;


//Tell other players which map we will be using
procedure TKMNetworking.MapSelect(aName:string);
var i:integer;
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can select maps');

  //Send to partners
  for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
    PacketSend(fPlayersList[i], mk_MapSelect, aName);

  MapName := aName;
  if Assigned(fOnMapName) then fOnMapName(MapName);

  //Compare map availability and CRC
end;


//Tell other players we want to start
//Send whole game setup info at once, making sure there are no misunderstandings,
//especially about random values (e.g. start locations)
procedure TKMNetworking.StartGame;
var i:integer; AllReady:boolean; Msg:TKMemoryStream;
begin
  Assert(fLANPlayerKind = lpk_Host, 'Only host can start the game');

  //Check if everyone is ready to start
  AllReady := true;
  //for i := 1 to fPlayersList.Count-1 do //Exclude self
    //AllReady := AllReady and fPlayersList[i].Ready;

  if not AllReady then begin
    Assert(AllReady, 'Not everyone is ready to start');
    exit;
  end;

  Msg := TKMemoryStream.Create;
  EncodeGameSetup(Msg);

  //Send game setup to partners
  for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
    //PacketSend(fPlayersList[i], mk_StartGame, Msg);

  Msg.Free;

  //Now we will await confirmation messages from other players and start the game
end;


procedure TKMNetworking.EncodeGameSetup(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Write(MapName);
  //aStream.Write(AllianceMode); //Fixed / changeable
  //aStream.Write(StartupConditions);
  //aStream.Write(WinConditions);
  //aStream.Write(DefeatConditions);
  aStream.Write(fPlayersList.Count);

  for i:=0 to fPlayersList.Count-1 do
  begin
    //aStream.Write(fPlayersList[i].PlayerType); //Human / AI
    aStream.Write(i{fPlayersList[i].StartLocID});
    //aStream.Write(fPlayersList[i].FlagColor);
    //for k:=0 to fPlayersList.Count-1
    //  aStream.Write(fPlayersList[i].Alliance[k]); //Startup
  end;
end;


procedure TKMNetworking.PostMessage(aText:string);
var i:integer;
begin
  fOnTextMessage(fPlayersList[0] + ': ' + aText);

  //Send to partners
  for i := 1 to fPlayersList.Count-1 do //Exclude self and send to [2nd to last] range
    PacketSend(fPlayersList[i], mk_Text, fPlayersList[0] + ': ' + aText);
end;


procedure TKMNetworking.SendCommands(aStream:TKMemoryStream);
//var i:integer;
begin
  //for i:=1 to fPlayersList.Count-1 do
  //todo: send commands to all players
end;


procedure TKMNetworking.PacketRecieve(const aData: array of byte; aAddr:string);
  procedure SendPlayerList;
  var i:integer;
  begin
    for i:=1 to fPlayersList.Count-1 do
      PacketSend(fPlayersList[i], mk_PlayersList, fPlayersList.Text);
  end;


  procedure DecodePlayersList(const aText:string);
  begin
    Assert(RightStr(aText,2) = eol); //Make sure aText is complete, it must end with a seperator
    fPlayersList.Text := aText; //Replace the text
  end;
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
                      fPlayersList.Add(aAddr);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
                      SendPlayerList;
                      PostMessage(aAddr+' has joined');
                    end;
    mk_PlayersList: begin
                      DecodePlayersList(Data);
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
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
                      if Assigned(fOnPlayersList) then fOnPlayersList(fPlayersList.Text);
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
