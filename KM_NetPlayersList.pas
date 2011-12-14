unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, StrUtils, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Player;

const
  PING_COUNT = 20; //Number of pings to store and take the maximum over for latency calculation (pings are measured once per second)

type
  TNetPlayerType = (npt_Human, npt_Computer, npt_Closed);

type
  TKMPlayerInfo = class
  private
    fNikname:string;
    fLangID:byte;
    fIndexOnServer:integer;
    fFlagColorID:integer;    //Flag color, 0 means random
    fPings: array[0..PING_COUNT-1] of word; //Ring buffer
    fPingPos:byte;
    function GetFlagColor:cardinal;
    procedure SetLangID(aID:byte);
  public
    PlayerNetType:TNetPlayerType; //Human, Computer, Closed
    StartLocation:integer;  //Start location, 0 means random
    Team:integer;
    PlayerIndex:TKMPlayer;
    ReadyToStart:boolean;
    ReadyToPlay:boolean;
    Connected:boolean;      //Player is still connected
    Dropped:boolean;        //Host elected to continue play without this player
    procedure AddPing(aPing:word);
    function GetInstantPing:word;
    function GetMaxPing:word;
    function IsHuman:boolean;
    function IsComputer:boolean;
    function IsClosed:boolean;
    function GetPlayerType:TPlayerType;
    function GetNickname:string;
    property Nikname:string read fNikname;
    property LangID:byte read fLangID write SetLangID;
    property IndexOnServer:integer read fIndexOnServer;
    property SetIndexOnServer:integer write fIndexOnServer;
    property FlagColor:cardinal read GetFlagColor;
    property FlagColorID:integer read fFlagColorID write fFlagColorID;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;

  //Handles everything related to players list,
  //but knows nothing about networking nor game setup. Only players.
  TKMPlayersList = class
  private
    fCount:integer;
    fPlayers:array [1..MAX_PLAYERS] of TKMPlayerInfo;
    function GetPlayer(Index:integer):TKMPlayerInfo;
    procedure ValidateLocations(aMaxLoc:byte);
    procedure ValidateColors;
    procedure RemAllClosedPlayers;
    procedure UpdateAIPlayerNames;
  public
    HostDoesSetup: boolean; //Gives host absolute control over locations/teams (not colors)
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count:integer read fCount;

    procedure AddPlayer(aNik:string; aIndexOnServer:integer; aLang:byte=0);
    procedure AddAIPlayer(aSlot:integer=-1);
    procedure AddClosedPlayer(aSlot:integer=-1);
    procedure DisconnectPlayer(aIndexOnServer:integer);
    procedure DisconnectAllClients(aOwnNikname:string);
    procedure DropPlayer(aIndexOnServer:integer);
    procedure RemPlayer(aIndexOnServer:integer);
    procedure RemAIPlayer(ID:integer);
    procedure RemClosedPlayer(ID:integer);
    property Player[Index:integer]:TKMPlayerInfo read GetPlayer; default;

    //Getters
    function ServerToLocal(aIndexOnServer:integer):integer;
    function NiknameToLocal(aNikname:string):integer;
    function StartingLocToLocal(aLoc:integer):integer;
    function PlayerIndexToLocal(aIndex:TPlayerIndex):integer;
    function CheckCanJoin(aNik:string; aIndexOnServer:integer):string;
    function CheckCanReconnect(aLocalIndex:integer):string;
    function LocAvailable(aIndex:integer):boolean;
    function ColorAvailable(aIndex:integer):boolean;
    function AllReady:boolean;
    function AllReadyToPlay:boolean;
    function GetMaxHighestRoundTripLatency:word;
    procedure GetNotReadyToPlayPlayers(aPlayerList:TStringList);
    function GetAICount:integer;
    function GetClosedCount:integer;

    procedure ResetLocAndReady;
    procedure SetAIReady;
    function ValidateSetup(aMaxLoc:byte; out ErrorMsg:String):boolean;

    //Import/Export
    function GetAsText:string; //Gets all relevant information as text string
    procedure SetAsText(const aText:string); //Sets all relevant information from text string
    function GetSimpleAsText:string; //Gets just names as a text string seperated by |
  end;


implementation
uses KM_TextLibrary;


{ TKMPlayerInfo }
procedure TKMPlayerInfo.AddPing(aPing:word);
begin
  fPingPos := (fPingPos+1) mod PING_COUNT;
  fPings[fPingPos] := aPing;
end;


function TKMPlayerInfo.GetFlagColor: cardinal;
begin
  if fFlagColorID <> 0 then
    Result := MP_TEAM_COLORS[fFlagColorID]
  else
    Result := $FF000000; //Black
end;


procedure TKMPlayerInfo.SetLangID(aID:byte);
begin
  if InRange(aID,1,LOCALES_COUNT) then
    fLangID := aID;
end;


function TKMPlayerInfo.GetInstantPing:word;
begin
  Result := fPings[fPingPos];
end;


function TKMPlayerInfo.GetMaxPing:word;
var i: integer;
begin
  Result := 0;
  for i:=0 to PING_COUNT-1 do
    Result := Math.max(Result, fPings[i]);
end;


function TKMPlayerInfo.IsHuman:boolean;
begin
  Result := PlayerNetType = npt_Human;
end;


function TKMPlayerInfo.IsComputer:boolean;
begin
  Result := PlayerNetType = npt_Computer;
end;


function TKMPlayerInfo.IsClosed:boolean;
begin
  Result := PlayerNetType = npt_Closed;
end;


function TKMPlayerInfo.GetPlayerType:TPlayerType;
const PlayerTypes:array[TNetPlayerType] of TPlayerType = (pt_Human, pt_Computer, pt_Computer);
begin
  Result := PlayerTypes[PlayerNetType];
end;


function TKMPlayerInfo.GetNickname:string;
begin
  if IsComputer then Result := fTextLibrary[TX_LOBBY_SLOT_AI_PLAYER]
  else if IsClosed then Result := fTextLibrary[TX_LOBBY_SLOT_CLOSED]
  else Result := Nikname;
end;


procedure TKMPlayerInfo.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fNikname);
  LoadStream.Read(fLangID);
  LoadStream.Read(fIndexOnServer);
  LoadStream.Read(PlayerNetType, SizeOf(PlayerNetType));
  LoadStream.Read(fFlagColorID);
  LoadStream.Read(StartLocation);
  LoadStream.Read(Team);
  LoadStream.Read(ReadyToStart);
  LoadStream.Read(ReadyToPlay);
  LoadStream.Read(Connected);
  LoadStream.Read(Dropped);
end;


procedure TKMPlayerInfo.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fNikname);
  SaveStream.Write(fLangID);
  SaveStream.Write(fIndexOnServer);
  SaveStream.Write(PlayerNetType, SizeOf(PlayerNetType));
  SaveStream.Write(fFlagColorID);
  SaveStream.Write(StartLocation);
  SaveStream.Write(Team);
  SaveStream.Write(ReadyToStart);
  SaveStream.Write(ReadyToPlay);
  SaveStream.Write(Connected);
  SaveStream.Write(Dropped);
end;


{ TKMPlayersList }
constructor TKMPlayersList.Create;
var i:integer;
begin
  Inherited;
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


function TKMPlayersList.GetPlayer(Index:integer):TKMPlayerInfo;
begin
  Result := fPlayers[Index];
end;


//Make sure all starting locations are valid
procedure TKMPlayersList.ValidateLocations(aMaxLoc:byte);
var
  i,k,LocCount:integer;
  UsedLoc:array of boolean;
  AvailableLoc:array[1..MAX_PLAYERS] of byte;
begin
  //All wrong start locations will be reset to "undefined"
  for i:=1 to fCount do
    if not Math.InRange(fPlayers[i].StartLocation, 0, aMaxLoc) then fPlayers[i].StartLocation := 0;

  SetLength(UsedLoc, aMaxLoc+1); //01..aMaxLoc, all false
  for i:=1 to aMaxLoc do UsedLoc[i] := false;

  //Remember all used locations and drop duplicates
  for i:=1 to fCount do
    if UsedLoc[fPlayers[i].StartLocation] then
      fPlayers[i].StartLocation := 0
    else
      UsedLoc[fPlayers[i].StartLocation] := true;

  //Collect available locations in a list
  LocCount := 0;
  for i:=1 to aMaxLoc do
  if not UsedLoc[i] then begin
    inc(LocCount);
    AvailableLoc[LocCount] := i;
  end;

  //Randomize (don't use KaMRandom - we want varied results and PlayerList is synced to clients before start)
  for i:=1 to LocCount do
    SwapInt(AvailableLoc[i], AvailableLoc[Random(LocCount)+1]);

  //Allocate available starting locations
  k := 0;
  for i:=1 to fCount do
  if fPlayers[i].StartLocation = 0 then begin
    inc(k);
    if k<=LocCount then
      fPlayers[i].StartLocation := AvailableLoc[k];
  end;

  //Check for odd players
  for i:=1 to fCount do
    Assert(fPlayers[i].StartLocation <> 0, 'Everyone should have a starting location!');
end;


procedure TKMPlayersList.ValidateColors;
var
  i,k,ColorCount:integer;
  UsedColor:array[0..MP_COLOR_COUNT] of boolean; //0 means Random
  AvailableColor:array[1..MP_COLOR_COUNT] of byte;
begin
  //All wrong colors will be reset to random
  for i:=1 to fCount do
    if not Math.InRange(fPlayers[i].FlagColorID, 0, MP_COLOR_COUNT) then
      fPlayers[i].FlagColorID := 0;

  FillChar(UsedColor, SizeOf(UsedColor), #0);

  //Remember all used colors and drop duplicates
  for i:=1 to fCount do
    if UsedColor[fPlayers[i].FlagColorID] then
      fPlayers[i].FlagColorID := 0
    else
      UsedColor[fPlayers[i].FlagColorID] := true;

  //Collect available colors
  ColorCount := 0;
  for i:=1 to MP_COLOR_COUNT do
  if not UsedColor[i] then begin
    inc(ColorCount);
    AvailableColor[ColorCount] := i;
  end;

  //Randomize (don't use KaMRandom - we want varied results and PlayerList is synced to clients before start)
  for i:=1 to ColorCount do
    SwapInt(AvailableColor[i], AvailableColor[Random(ColorCount)+1]);

  //Allocate available colors
  k := 0;
  for i:=1 to fCount do
  if fPlayers[i].FlagColorID = 0 then
  begin
    inc(k);
    if k<=ColorCount then
      fPlayers[i].FlagColorID := AvailableColor[k];
  end;

  //Check for odd players
  for i:=1 to fCount do
    Assert(fPlayers[i].FlagColorID <> 0, 'Everyone should have a color!');
end;


procedure TKMPlayersList.RemAllClosedPlayers;
var i:integer;
begin
  for i:=fCount downto 1 do
    if Player[i].IsClosed then
      RemClosedPlayer(i);
end;


procedure TKMPlayersList.AddPlayer(aNik:string; aIndexOnServer:integer; aLang:byte=0);
begin
  inc(fCount);
  fPlayers[fCount].fNikname := aNik;
  fPlayers[fCount].fLangID := aLang;
  fPlayers[fCount].fIndexOnServer := aIndexOnServer;
  fPlayers[fCount].PlayerNetType := npt_Human;
  fPlayers[fCount].PlayerIndex := nil;
  fPlayers[fCount].Team := 0;
  fPlayers[fCount].FlagColorID := 0;
  fPlayers[fCount].StartLocation := 0;
  fPlayers[fCount].ReadyToStart := false;
  fPlayers[fCount].ReadyToPlay := false;
  fPlayers[fCount].Connected := true;
  fPlayers[fCount].Dropped := false;
end;


procedure TKMPlayersList.AddAIPlayer(aSlot:integer=-1);
begin
  if aSlot = -1 then
  begin
    inc(fCount);
    aSlot := fCount;
  end;
  fPlayers[aSlot].fNikname := 'AI Player';
  fPlayers[aSlot].fLangID := 0;
  fPlayers[aSlot].fIndexOnServer := -1;
  fPlayers[aSlot].PlayerNetType := npt_Computer;
  fPlayers[aSlot].PlayerIndex := nil;
  fPlayers[aSlot].Team := 0;
  fPlayers[aSlot].FlagColorID := 0;
  fPlayers[aSlot].StartLocation := 0;
  fPlayers[aSlot].ReadyToStart := true;
  fPlayers[aSlot].ReadyToPlay := true;
  fPlayers[aSlot].Connected := true;
  fPlayers[aSlot].Dropped := false;
  UpdateAIPlayerNames;
end;


procedure TKMPlayersList.AddClosedPlayer(aSlot:integer=-1);
begin
  if aSlot = -1 then
  begin
    inc(fCount);
    aSlot := fCount;
  end;
  fPlayers[aSlot].fNikname := 'Closed';
  fPlayers[aSlot].fLangID := 0;
  fPlayers[aSlot].fIndexOnServer := -1;
  fPlayers[aSlot].PlayerNetType := npt_Closed;
  fPlayers[aSlot].PlayerIndex := nil;
  fPlayers[aSlot].Team := 0;
  fPlayers[aSlot].FlagColorID := 0;
  fPlayers[aSlot].StartLocation := 0;
  fPlayers[aSlot].ReadyToStart := true;
  fPlayers[aSlot].ReadyToPlay := true;
  fPlayers[aSlot].Connected := true;
  fPlayers[aSlot].Dropped := false;
end;


//Set player to no longer be connected, but do not remove them from the game
procedure TKMPlayersList.DisconnectPlayer(aIndexOnServer:integer);
var ID:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot disconnect player');
  fPlayers[ID].Connected := false;
end;

//Mark all human players as disconnected (used when reconnecting if all clients were lost)
procedure TKMPlayersList.DisconnectAllClients(aOwnNikname:string);
var i:integer;
begin
  for i:=1 to fCount do
    if (fPlayers[i].IsHuman) and (fPlayers[i].Nikname <> aOwnNikname) then
      fPlayers[i].Connected := false;
end;


//Set player to no longer be on the server, but do not remove their assets from the game
procedure TKMPlayersList.DropPlayer(aIndexOnServer:integer);
var ID:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot drop player');
  fPlayers[ID].Connected := false;
  fPlayers[ID].Dropped := true;
end;


procedure TKMPlayersList.RemPlayer(aIndexOnServer:integer);
var ID,i:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot remove player');
  fPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fPlayers[i] := fPlayers[i+1]; //Shift only pointers

  fPlayers[fCount] := TKMPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
end;


procedure TKMPlayersList.RemAIPlayer(ID:integer);
var i:integer;
begin
  fPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fPlayers[i] := fPlayers[i+1]; //Shift only pointers

  fPlayers[fCount] := TKMPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
  UpdateAIPlayerNames;
end;


procedure TKMPlayersList.RemClosedPlayer(ID:integer);
var i:integer;
begin
  fPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fPlayers[i] := fPlayers[i+1]; //Shift only pointers

  fPlayers[fCount] := TKMPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
end;


procedure TKMPlayersList.UpdateAIPlayerNames;
var i, AICount:integer;
begin
  AICount := 0;
  for i:=1 to fCount do
    if fPlayers[i].PlayerNetType = npt_Computer then
    begin
      inc(AICount);
      fPlayers[i].fNikname := 'AI Player '+IntToStr(AICount);
    end;
end;


function TKMPlayersList.ServerToLocal(aIndexOnServer:integer):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].fIndexOnServer = aIndexOnServer then
    begin
      Result := i;
      Exit;
    end;
end;


//Networking needs to convert Nikname to local index in players list
function TKMPlayersList.NiknameToLocal(aNikname:string):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].fNikname = aNikname then
      Result := i;
end;


function TKMPlayersList.StartingLocToLocal(aLoc:integer):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].StartLocation = aLoc then
      Result := i;
end;


function TKMPlayersList.PlayerIndexToLocal(aIndex:TPlayerIndex):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if (fPlayers[i].PlayerIndex <> nil) and (fPlayers[i].PlayerIndex.PlayerIndex = aIndex) then
      Result := i;
end;


//See if player can join our game
function TKMPlayersList.CheckCanJoin(aNik:string; aIndexOnServer:integer):string;
begin
  if fCount >= MAX_PLAYERS then
    Result := 'Room is full. No more players can join the game'
  else
  if ServerToLocal(aIndexOnServer) <> -1 then
    Result := 'Player with said index already joined the game'
  else
  if NiknameToLocal(aNik) <> -1 then
    Result := 'Player with the same name already joined the game'
  else
  if LeftStr(aNik,length('AI Player')) = 'AI Player' then
    Result := 'Cannot have the same name as AI players'
  else
    Result := '';
end;


//See if player can join our game
function TKMPlayersList.CheckCanReconnect(aLocalIndex:integer):string;
begin
  if aLocalIndex = -1 then
    Result := 'Unknown nickname'
  else
  if Player[aLocalIndex].Connected then
    Result := 'Your previous connection has not yet been dropped, please try again in a moment'
  else
  if Player[aLocalIndex].Dropped then
    Result := 'The host decided to continue playing without you :('
  else
    Result := '';
end;


function TKMPlayersList.LocAvailable(aIndex:integer):boolean;
var i:integer;
begin
  Result := true;
  if aIndex=0 then exit;

  for i:=1 to fCount do
    Result := Result and not (fPlayers[i].StartLocation = aIndex);
end;


function TKMPlayersList.ColorAvailable(aIndex:integer):boolean;
var i:integer;
begin
  Result := true;
  if aIndex=0 then exit;

  for i:=1 to fCount do
    Result := Result and not (fPlayers[i].FlagColorID = aIndex);
end;


function TKMPlayersList.AllReady:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    if fPlayers[i].Connected and fPlayers[i].IsHuman then
      Result := Result and fPlayers[i].ReadyToStart;
end;


function TKMPlayersList.AllReadyToPlay:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    if fPlayers[i].Connected and fPlayers[i].IsHuman then
      Result := Result and fPlayers[i].ReadyToPlay;
end;


function TKMPlayersList.GetMaxHighestRoundTripLatency:word;
var i:integer; Highest, Highest2: word;
begin
  Highest := 0;
  Highest2 := 0;
  for i:=1 to fCount do
    if fPlayers[i].Connected and fPlayers[i].IsHuman then
    begin
      if fPlayers[i].GetMaxPing > Highest then
        Highest := fPlayers[i].GetMaxPing
      else
        if fPlayers[i].GetMaxPing > Highest2 then
          Highest2 := fPlayers[i].GetMaxPing;
    end;
  Result := min(Highest + Highest2, High(Word));
end;


procedure TKMPlayersList.GetNotReadyToPlayPlayers(aPlayerList:TStringList);
var i:integer;
begin
  for i:=1 to fCount do
    if (not fPlayers[i].ReadyToPlay) and fPlayers[i].IsHuman and fPlayers[i].Connected then
      aPlayerList.Add(fPlayers[i].Nikname);
end;


function TKMPlayersList.GetAICount:integer;
var i:integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fPlayers[i].PlayerNetType = npt_Computer then
      inc(Result);
end;


function TKMPlayersList.GetClosedCount:integer;
var i:integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fPlayers[i].PlayerNetType = npt_Closed then
      inc(Result);
end;


procedure TKMPlayersList.ResetLocAndReady;
var i:integer;
begin
  for i:=1 to fCount do
  begin
    fPlayers[i].StartLocation := 0;
    if fPlayers[i].PlayerNetType = npt_Human then //AI/closed players are always ready
      fPlayers[i].ReadyToStart := false;
  end;
end;


procedure TKMPlayersList.SetAIReady;
var i:integer;
begin
  for i:=1 to fCount do
    if fPlayers[i].PlayerNetType in [npt_Computer,npt_Closed] then
    begin
      fPlayers[i].ReadyToStart := true;
      fPlayers[i].ReadyToPlay := true;
    end;
end;


//Convert undefined/random start locations to fixed and assign random colors
//Remove odd players
function TKMPlayersList.ValidateSetup(aMaxLoc:byte; out ErrorMsg:String):boolean;
begin
  if not AllReady then begin
    ErrorMsg := fTextLibrary[TX_LOBBY_EVERYONE_NOT_READY];
    Result := False;
  end else
  if (fCount-GetClosedCount) > aMaxLoc then begin
    ErrorMsg := fTextLibrary[TX_LOBBY_PLAYER_LIMIT];
    Result := False;
  end else begin
    RemAllClosedPlayers; //Closed players are just a marker in the lobby, delete them when the game starts
    ValidateLocations(aMaxLoc);
    ValidateColors;
    Result := True;
  end;
end;


//Save whole amount of data as string to be sent across network to other players
//I estimate it ~50bytes per player at max
//later it will be byte array?
function TKMPlayersList.GetAsText:string;
var i:integer; M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;

  M.Write(HostDoesSetup);
  M.Write(fCount);
  for i:=1 to fCount do
    fPlayers[i].Save(M);

  Result := M.ReadAsText;
  M.Free;
end;


procedure TKMPlayersList.SetAsText(const aText:string);
var i:integer; M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aText);
    M.Read(HostDoesSetup);
    M.Read(fCount);
    for i:=1 to fCount do
      fPlayers[i].Load(M);
  finally
    M.Free;
  end;
end;


function TKMPlayersList.GetSimpleAsText:string;
var i:integer;
begin
  for i:=1 to fCount do
  begin
    Result := Result + StringReplace(fPlayers[i].Nikname,'|','',[rfReplaceAll]);
    if i < fCount then Result := Result + '|';
  end;
end;


end.
