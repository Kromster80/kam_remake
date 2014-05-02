unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, StrUtils, Math, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Hand, KM_ResLocales, KM_NetworkTypes;

const
  PING_COUNT = 20; //Number of pings to store and take the maximum over for latency calculation (pings are measured once per second)
  LOC_RANDOM = 0;
  LOC_SPECTATE = -1;

type
  //Multiplayer info that is filled in Lobby before TKMPlayers are created (thats why it has many mirror fields)
  TKMNetPlayerInfo = class
  private
    fNikname: AnsiString;
    fLangCode: AnsiString;
    fIndexOnServer: Integer;
    fFlagColorID: Integer;    //Flag color, 0 means random
    fPings: array[0 .. PING_COUNT-1] of Word; //Ring buffer
    fPingPos: Byte;
    procedure SetLangCode(const aCode: AnsiString);
    function GetNiknameColored: AnsiString;
  public
    PlayerNetType: TNetPlayerType; //Human, Computer, Closed
    StartLocation: Integer;  //Start location, 0 means random, -1 means spectate
    Team: Integer;
    ReadyToStart: Boolean;
    ReadyToPlay: Boolean;
    Connected: Boolean;      //Player is still connected
    Dropped: Boolean;        //Host elected to continue play without this player
    FPS: Cardinal;
    VotedYes: Boolean;
    procedure AddPing(aPing: Word);
    procedure ResetPingRecord;
    function GetInstantPing: Word;
    function GetMaxPing: Word;
    function IsHuman: Boolean;
    function IsComputer: Boolean;
    function IsClosed: Boolean;
    function IsSpectator: Boolean;
    function GetPlayerType: THandType;
    function SlotName: UnicodeString; //Player name if it's human or computer or closed
    property Nikname: AnsiString read fNikname; //Human player nikname (ANSI-Latin)
    property NiknameColored: AnsiString read GetNiknameColored;
    property LangCode: AnsiString read fLangCode write SetLangCode;
    property IndexOnServer: Integer read fIndexOnServer;
    property SetIndexOnServer: Integer write fIndexOnServer;
    function FlagColor(aDefault: Cardinal = $FF000000): Cardinal;
    property FlagColorID: Integer read fFlagColorID write fFlagColorID;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  //Handles everything related to players list,
  //but knows nothing about networking nor game setup. Only players.
  TKMNetPlayersList = class
  private
    fCount: Integer;
    fNetPlayers: array [1..MAX_LOBBY_SLOTS] of TKMNetPlayerInfo;
    function GetPlayer(aIndex: Integer): TKMNetPlayerInfo;
    procedure ValidateColors;
    procedure RemAllClosedPlayers;
  public
    HostDoesSetup: Boolean; //Gives host absolute control over locations/teams (not colors)
    RandomizeTeamLocations: Boolean; //When the game starts locations are shuffled within each team
    SpectatorsAllowed: Boolean;
    SpectatorSlotsOpen: ShortInt;
    VoteActive: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count:integer read fCount;

    procedure AddPlayer(aNik: AnsiString; aIndexOnServer: Integer; const aLang: AnsiString);
    procedure AddAIPlayer(aSlot: Integer = -1);
    procedure AddClosedPlayer(aSlot: Integer = -1);
    procedure DisconnectPlayer(aIndexOnServer: Integer);
    procedure DisconnectAllClients(aOwnNikname: AnsiString);
    procedure DropPlayer(aIndexOnServer: Integer);
    procedure RemPlayer(aIndex: Integer);
    procedure RemServerPlayer(aIndexOnServer: Integer);
    property Player[aIndex: Integer]: TKMNetPlayerInfo read GetPlayer; default;

    //Getters
    function ServerToLocal(aIndexOnServer: Integer): Integer;
    function NiknameToLocal(aNikname: AnsiString): Integer;
    function StartingLocToLocal(aLoc: Integer): Integer;
    function PlayerIndexToLocal(aIndex: THandIndex): Integer;

    function CheckCanJoin(aNik: AnsiString; aIndexOnServer: Integer): Integer;
    function CheckCanReconnect(aLocalIndex: Integer): Integer;
    function LocAvailable(aIndex: Integer): Boolean;
    function ColorAvailable(aIndex: Integer): Boolean;
    function AllReady: Boolean;
    function AllReadyToPlay: Boolean;
    function GetMaxHighestRoundTripLatency: Word;
    function GetNotReadyToPlayPlayers: TKMByteArray;
    function GetAICount: Integer;
    function GetClosedCount: Integer;
    function GetSpectatorCount: Integer;
    function GetConnectedCount: Integer;
    function FurtherVotesNeededForMajority: Integer;

    procedure ResetLocAndReady;
    procedure ResetReady;
    procedure ResetVote;
    procedure SetAIReady;
    procedure RemAllAIs;
    procedure RemDisconnectedPlayers;
    function ValidateSetup(aHumanUsableLocs, aAIUsableLocs: TPlayerIndexArray; out ErrorMsg: UnicodeString): Boolean;

    //Import/Export
    procedure SaveToStream(aStream: TKMemoryStream); //Gets all relevant information as text string
    procedure LoadFromStream(aStream: TKMemoryStream); //Sets all relevant information
    function GetSlotNames: UnicodeString; //Gets just names as a text string seperated by |
    function GetPlayersWithIDs: UnicodeString;
  end;


implementation
uses KM_ResTexts, KM_Utils;


{ TKMNetPlayerInfo }
procedure TKMNetPlayerInfo.AddPing(aPing: Word);
begin
  fPingPos := (fPingPos + 1) mod PING_COUNT;
  fPings[fPingPos] := aPing;
end;


procedure TKMNetPlayerInfo.ResetPingRecord;
begin
  fPingPos := 0;
  FillChar(fPings, SizeOf(fPings), #0);
end;


function TKMNetPlayerInfo.FlagColor(aDefault: Cardinal = $FF000000): Cardinal;
begin
  if fFlagColorID <> 0 then
    Result := MP_TEAM_COLORS[fFlagColorID]
  else
    Result := aDefault; //Black by default
end;


procedure TKMNetPlayerInfo.SetLangCode(const aCode: AnsiString);
begin
  if gResLocales.IndexByCode(aCode) <> -1 then
    fLangCode := aCode;
end;


function TKMNetPlayerInfo.GetInstantPing: Word;
begin
  Result := fPings[fPingPos];
end;


function TKMNetPlayerInfo.GetMaxPing: Word;
var I: Integer; Worst: Word;
begin
  Result := 0;
  Worst := 0;
  //We should ignore the worst ping so we don't delay game input due to one ping spike
  for I := 0 to PING_COUNT - 1 do
  begin
    if fPings[I] > Worst then
    begin
      Result := Math.max(Result, Worst);
      Worst := fPings[I]
    end
    else
      Result := Math.max(Result, fPings[I]);
  end;
end;


function TKMNetPlayerInfo.IsHuman: Boolean;
begin
  Result := PlayerNetType = nptHuman;
end;


function TKMNetPlayerInfo.IsComputer: Boolean;
begin
  Result := PlayerNetType = nptComputer;
end;


function TKMNetPlayerInfo.IsClosed: Boolean;
begin
  Result := PlayerNetType = nptClosed;
end;


function TKMNetPlayerInfo.IsSpectator: Boolean;
begin
  Result := StartLocation = LOC_SPECTATE;
end;


function TKMNetPlayerInfo.GetPlayerType: THandType;
const
  PlayerTypes: array [TNetPlayerType] of THandType = (hndHuman, hndComputer, hndComputer);
begin
  Result := PlayerTypes[PlayerNetType];
end;


function TKMNetPlayerInfo.SlotName: UnicodeString;
begin
  case PlayerNetType of
    nptHuman:     Result := UnicodeString(Nikname);
    nptComputer:  //In lobby AI players don't have numbers yet (they are added on mission start)
                  Result := gResTexts[TX_LOBBY_SLOT_AI_PLAYER];
    nptClosed:    Result := gResTexts[TX_LOBBY_SLOT_CLOSED];
    else          Result := NO_TEXT;
  end;
end;


function TKMNetPlayerInfo.GetNiknameColored: AnsiString;
begin
  if FlagColorID <> 0 then
    Result := WrapColorA(Nikname, FlagColorToTextColor(FlagColor))
  else
    Result := Nikname;
end;


procedure TKMNetPlayerInfo.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadA(fNikname);
  LoadStream.ReadA(fLangCode);
  LoadStream.Read(fIndexOnServer);
  LoadStream.Read(PlayerNetType, SizeOf(PlayerNetType));
  LoadStream.Read(fFlagColorID);
  LoadStream.Read(StartLocation);
  LoadStream.Read(Team);
  LoadStream.Read(ReadyToStart);
  LoadStream.Read(ReadyToPlay);
  LoadStream.Read(Connected);
  LoadStream.Read(Dropped);
  LoadStream.Read(VotedYes);
end;


procedure TKMNetPlayerInfo.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA(fNikname);
  SaveStream.WriteA(fLangCode);
  SaveStream.Write(fIndexOnServer);
  SaveStream.Write(PlayerNetType, SizeOf(PlayerNetType));
  SaveStream.Write(fFlagColorID);
  SaveStream.Write(StartLocation);
  SaveStream.Write(Team);
  SaveStream.Write(ReadyToStart);
  SaveStream.Write(ReadyToPlay);
  SaveStream.Write(Connected);
  SaveStream.Write(Dropped);
  SaveStream.Write(VotedYes);
end;


{ TKMNetPlayersList }
constructor TKMNetPlayersList.Create;
var I: Integer;
begin
  inherited;
  SpectatorSlotsOpen := MAX_LOBBY_SPECTATORS;
  for I := 1 to MAX_LOBBY_SLOTS do
    fNetPlayers[I] := TKMNetPlayerInfo.Create;
end;


destructor TKMNetPlayersList.Destroy;
var I: Integer;
begin
  for I := 1 to MAX_LOBBY_SLOTS do
    fNetPlayers[I].Free;
  inherited;
end;


procedure TKMNetPlayersList.Clear;
begin
  HostDoesSetup := False;
  RandomizeTeamLocations := False;
  SpectatorsAllowed := False;
  SpectatorSlotsOpen := MAX_LOBBY_SPECTATORS;
  fCount := 0;
end;


function TKMNetPlayersList.GetPlayer(aIndex: Integer): TKMNetPlayerInfo;
begin
  Result := fNetPlayers[aIndex];
end;


procedure TKMNetPlayersList.ValidateColors;
var
  I,K,ColorCount: Integer;
  UsedColor: array [0..MP_COLOR_COUNT] of Boolean; //0 means Random
  AvailableColor: array [1..MP_COLOR_COUNT] of Byte;
begin
  //All wrong colors will be reset to random
  for I := 1 to fCount do
    if not Math.InRange(fNetPlayers[I].FlagColorID, 0, MP_COLOR_COUNT) then
      fNetPlayers[I].FlagColorID := 0;

  FillChar(UsedColor, SizeOf(UsedColor), #0);

  //Remember all used colors and drop duplicates
  for I := 1 to fCount do
    if UsedColor[fNetPlayers[I].FlagColorID] then
      fNetPlayers[I].FlagColorID := 0
    else
      UsedColor[fNetPlayers[I].FlagColorID] := true;

  //Collect available colors
  ColorCount := 0;
  for I := 1 to MP_COLOR_COUNT do
  if not UsedColor[I] then
  begin
    Inc(ColorCount);
    AvailableColor[ColorCount] := I;
  end;

  //Randomize (don't use KaMRandom - we want varied results and PlayerList is synced to clients before start)
  for I := 1 to ColorCount do
    SwapInt(AvailableColor[I], AvailableColor[Random(ColorCount)+1]);

  //Allocate available colors
  K := 0;
  for I := 1 to fCount do
    if fNetPlayers[I].FlagColorID = 0 then
    begin
      Inc(K);
      if K <= ColorCount then
        fNetPlayers[I].FlagColorID := AvailableColor[K];
    end;

  //Check for odd players
  for I := 1 to fCount do
    Assert(fNetPlayers[I].FlagColorID <> 0, 'Everyone should have a color now!');
end;


procedure TKMNetPlayersList.RemAllClosedPlayers;
var
  I: Integer;
begin
  for I := fCount downto 1 do
    if Player[I].IsClosed then
      RemPlayer(I);
end;


procedure TKMNetPlayersList.AddPlayer(aNik: AnsiString; aIndexOnServer: Integer; const aLang: AnsiString);
begin
  Assert(fCount <= MAX_LOBBY_SLOTS, 'Can''t add player');
  Inc(fCount);
  fNetPlayers[fCount].fNikname := aNik;
  fNetPlayers[fCount].fLangCode := aLang;
  fNetPlayers[fCount].fIndexOnServer := aIndexOnServer;
  fNetPlayers[fCount].PlayerNetType := nptHuman;
  //fPlayers[fCount].PlayerIndex := nil;
  fNetPlayers[fCount].Team := 0;
  fNetPlayers[fCount].FlagColorID := 0;
  fNetPlayers[fCount].ReadyToStart := false;
  fNetPlayers[fCount].ReadyToPlay := false;
  fNetPlayers[fCount].Connected := true;
  fNetPlayers[fCount].Dropped := false;
  fNetPlayers[fCount].ResetPingRecord;
  //Check if this player must go in a spectator slot
  if fCount-GetSpectatorCount > MAX_LOBBY_PLAYERS then
    fNetPlayers[fCount].StartLocation := LOC_SPECTATE
  else
    fNetPlayers[fCount].StartLocation := LOC_RANDOM;
end;


procedure TKMNetPlayersList.AddAIPlayer(aSlot: Integer = -1);
begin
  if aSlot = -1 then
  begin
    Assert(fCount <= MAX_LOBBY_SLOTS, 'Can''t add AI player');
    Inc(fCount);
    aSlot := fCount;
  end;
  fNetPlayers[aSlot].fNikname := '';
  fNetPlayers[aSlot].fLangCode := '';
  fNetPlayers[aSlot].fIndexOnServer := -1;
  fNetPlayers[aSlot].PlayerNetType := nptComputer;
  fNetPlayers[aSlot].Team := 0;
  fNetPlayers[aSlot].FlagColorID := 0;
  fNetPlayers[aSlot].StartLocation := 0;
  fNetPlayers[aSlot].ReadyToStart := True;
  fNetPlayers[aSlot].ReadyToPlay := True;
  fNetPlayers[aSlot].Connected := True;
  fNetPlayers[aSlot].Dropped := False;
  fNetPlayers[aSlot].ResetPingRecord;
end;


procedure TKMNetPlayersList.AddClosedPlayer(aSlot: Integer = -1);
begin
  if aSlot = -1 then
  begin
    Assert(fCount < MAX_LOBBY_SLOTS, 'Can''t add closed player');
    Inc(fCount);
    aSlot := fCount;
  end;
  fNetPlayers[aSlot].fNikname := '';
  fNetPlayers[aSlot].fLangCode := '';
  fNetPlayers[aSlot].fIndexOnServer := -1;
  fNetPlayers[aSlot].PlayerNetType := nptClosed;
  fNetPlayers[aSlot].Team := 0;
  fNetPlayers[aSlot].FlagColorID := 0;
  fNetPlayers[aSlot].StartLocation := 0;
  fNetPlayers[aSlot].ReadyToStart := True;
  fNetPlayers[aSlot].ReadyToPlay := True;
  fNetPlayers[aSlot].Connected := True;
  fNetPlayers[aSlot].Dropped := False;
  fNetPlayers[aSlot].ResetPingRecord;
end;


//Set player to no longer be connected, but do not remove them from the game
procedure TKMNetPlayersList.DisconnectPlayer(aIndexOnServer: Integer);
var
  ID: Integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot disconnect player');
  fNetPlayers[ID].Connected := False;
end;

//Mark all human players as disconnected (used when reconnecting if all clients were lost)
procedure TKMNetPlayersList.DisconnectAllClients(aOwnNikname: AnsiString);
var
  I: Integer;
begin
  for I := 1 to fCount do
    if (fNetPlayers[I].IsHuman) and (fNetPlayers[I].Nikname <> aOwnNikname) then
      fNetPlayers[I].Connected := False;
end;


//Set player to no longer be on the server, but do not remove their assets from the game
procedure TKMNetPlayersList.DropPlayer(aIndexOnServer: Integer);
var
  ID: Integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot drop player');
  fNetPlayers[ID].Connected := false;
  fNetPlayers[ID].Dropped := true;
end;


procedure TKMNetPlayersList.RemPlayer(aIndex: Integer);
var
  I: Integer;
begin
  fNetPlayers[aIndex].Free;
  for I := aIndex to fCount - 1 do
    fNetPlayers[I] := fNetPlayers[I + 1]; // Shift only pointers

  fNetPlayers[fCount] := TKMNetPlayerInfo.Create; // Empty players are created but not used
  Dec(fCount);
end;


procedure TKMNetPlayersList.RemServerPlayer(aIndexOnServer: Integer);
var
  ID: Integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot remove non-existing player');
  RemPlayer(ID);
end;


function TKMNetPlayersList.ServerToLocal(aIndexOnServer: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to fCount do
    if fNetPlayers[I].fIndexOnServer = aIndexOnServer then
    begin
      Result := I;
      Exit;
    end;
end;


//Networking needs to convert Nikname to local index in players list
function TKMNetPlayersList.NiknameToLocal(aNikname: AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to fCount do
    if fNetPlayers[I].fNikname = aNikname then
      Result := I;
end;


//Convert known starting location to local index in players list
function TKMNetPlayersList.StartingLocToLocal(aLoc: Integer): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 1 to fCount do
    if fNetPlayers[I].StartLocation = aLoc then
      Result := I;
end;


function TKMNetPlayersList.PlayerIndexToLocal(aIndex: THandIndex): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 1 to Count do
    if (aIndex = fNetPlayers[I].StartLocation - 1) then
      Result := I;
end;


//See if player can join our game
function TKMNetPlayersList.CheckCanJoin(aNik: AnsiString; aIndexOnServer: Integer): Integer;
begin
  if fCount >= MAX_LOBBY_SLOTS then
    Result := TX_NET_ROOM_FULL
  else
  if ServerToLocal(aIndexOnServer) <> -1 then
    Result := TX_NET_SAME_NAME
  else
  if NiknameToLocal(aNik) <> -1 then
    Result := TX_NET_SAME_NAME
  else
  if (aNik = 'AI Player') or (aNik = 'Closed') then
    Result := TX_NET_SAME_NAME
  else
  //If this player must take a spectator spot, check that one is open
  if (fCount-GetSpectatorCount >= MAX_LOBBY_PLAYERS)
  and ((SpectatorSlotsOpen-GetSpectatorCount <= 0) or not SpectatorsAllowed) then
    Result := TX_NET_ROOM_FULL
  else
    Result := -1;
end;


//See if player can join our game
function TKMNetPlayersList.CheckCanReconnect(aLocalIndex: Integer): Integer;
begin
  if aLocalIndex = -1 then
    Result := -2 //Silent failure, client should try again
  else
  if Player[aLocalIndex].Connected then
    Result := -2 //Silent failure, client should try again
  else
  if Player[aLocalIndex].Dropped then
    Result := TX_NET_RECONNECTION_DROPPED
  else
    Result := -1; //Success
end;


function TKMNetPlayersList.LocAvailable(aIndex: Integer): Boolean;
var I: Integer;
begin
  Result := True;
  if (aIndex = LOC_RANDOM) or (aIndex = LOC_SPECTATE) then Exit;

  for I := 1 to fCount do
    Result := Result and (aIndex <> fNetPlayers[I].StartLocation);
end;


function TKMNetPlayersList.ColorAvailable(aIndex: Integer): Boolean;
var I: Integer;
begin
  Result := True;
  if aIndex = 0 then Exit;

  for I := 1 to fCount do
    Result := Result and (aIndex <> fNetPlayers[I].FlagColorID);
end;


function TKMNetPlayersList.AllReady: Boolean;
var I: Integer;
begin
  Result := true;
  for i:=1 to fCount do
    if fNetPlayers[i].Connected and fNetPlayers[i].IsHuman then
      Result := Result and fNetPlayers[i].ReadyToStart;
end;


function TKMNetPlayersList.AllReadyToPlay:boolean;
var I: Integer;
begin
  Result := true;
  for i:=1 to fCount do
    if fNetPlayers[i].Connected and fNetPlayers[i].IsHuman then
      Result := Result and fNetPlayers[i].ReadyToPlay;
end;


function TKMNetPlayersList.GetMaxHighestRoundTripLatency:word;
var I: Integer; Highest, Highest2, PlayerPing: word;
begin
  Highest := 0;
  Highest2 := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].Connected and fNetPlayers[i].IsHuman then
    begin
      PlayerPing := fNetPlayers[i].GetMaxPing;
      if PlayerPing > Highest then
        Highest := PlayerPing
      else
        if PlayerPing > Highest2 then
          Highest2 := PlayerPing;
    end;
  Result := min(Highest + Highest2, High(Word));
end;


function TKMNetPlayersList.GetNotReadyToPlayPlayers: TKMByteArray;
var
  I, K: Integer;
begin
  SetLength(Result, MAX_LOBBY_SLOTS);

  K := 0;
  for I := 1 to fCount do
    if (not fNetPlayers[I].ReadyToPlay) and fNetPlayers[I].IsHuman and fNetPlayers[I].Connected then
    begin
      Result[K] := I;
      Inc(K)
    end;

  SetLength(Result, K);
end;


function TKMNetPlayersList.GetAICount:integer;
var I: Integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].PlayerNetType = nptComputer then
      inc(Result);
end;


function TKMNetPlayersList.GetClosedCount:integer;
var I: Integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].PlayerNetType = nptClosed then
      inc(Result);
end;


function TKMNetPlayersList.GetSpectatorCount:integer;
var I: Integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].IsSpectator then
      inc(Result);
end;


function TKMNetPlayersList.GetConnectedCount:integer;
var I: Integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].IsHuman and fNetPlayers[i].Connected then
      inc(Result);
end;


function TKMNetPlayersList.FurtherVotesNeededForMajority: Integer;
var I, VotedYes, Total: Integer;
begin
  Total := 0;
  VotedYes := 0;
  for I:=1 to fCount do
    if (fNetPlayers[I].PlayerNetType = nptHuman) and (fNetPlayers[I].StartLocation <> LOC_SPECTATE)
    and not fNetPlayers[I].Dropped then
    begin
      Inc(Total);
      if fNetPlayers[I].VotedYes then
        Inc(VotedYes);
    end;
  Result := (Total div 2) + 1 - VotedYes;
end;


procedure TKMNetPlayersList.ResetLocAndReady;
var I: Integer;
begin
  for i:=1 to fCount do
  begin
    if fNetPlayers[i].StartLocation <> LOC_SPECTATE then
      fNetPlayers[i].StartLocation := LOC_RANDOM;
    //AI/closed players are always ready, spectator ready status is not reset by map change
    if (fNetPlayers[i].PlayerNetType = nptHuman) and (fNetPlayers[i].StartLocation <> LOC_SPECTATE) then
      fNetPlayers[i].ReadyToStart := false;
  end;
end;


procedure TKMNetPlayersList.ResetReady;
var I: Integer;
begin
  for i:=1 to fCount do
    //AI/closed players are always ready, spectator ready status is not reset by options change
    if (fNetPlayers[i].PlayerNetType = nptHuman) and (fNetPlayers[i].StartLocation <> LOC_SPECTATE) then
      fNetPlayers[i].ReadyToStart := False;
end;


procedure TKMNetPlayersList.ResetVote;
var I: Integer;
begin
  VoteActive := False;
  for i:=1 to fCount do
    fNetPlayers[i].VotedYes := False;
end;


procedure TKMNetPlayersList.SetAIReady;
var I: Integer;
begin
  for i:=1 to fCount do
    if fNetPlayers[i].PlayerNetType in [nptComputer,nptClosed] then
    begin
      fNetPlayers[i].ReadyToStart := true;
      fNetPlayers[i].ReadyToPlay := true;
    end;
end;


procedure TKMNetPlayersList.RemAllAIs;
var I: Integer;
begin
  for I := fCount downto 1 do
    if Player[I].IsComputer then
      RemPlayer(I);
end;


procedure TKMNetPlayersList.RemDisconnectedPlayers;
var I: Integer;
begin
  for I := fCount downto 1 do
    if not Player[I].Connected then
      RemPlayer(I);
end;


//Convert undefined/random start locations to fixed and assign random colors
//Remove odd players
function TKMNetPlayersList.ValidateSetup(aHumanUsableLocs, aAIUsableLocs: TPlayerIndexArray; out ErrorMsg: UnicodeString):boolean;

  function IsHumanLoc(aLoc: Byte): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I:=0 to Length(aHumanUsableLocs)-1 do
      if aLoc = aHumanUsableLocs[I]+1 then
      begin
        Result := True;
        Exit;
      end;
  end;

  function IsAILoc(aLoc: Byte): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I:=0 to Length(aAIUsableLocs)-1 do
      if aLoc = aAIUsableLocs[I]+1 then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  I, K, J: Integer;
  UsedLoc: array[1..MAX_HANDS] of Boolean;
  AvailableLocHuman, AvailableLocBoth: array [1..MAX_HANDS] of Byte;
  LocHumanCount, LocBothCount: Byte;
  TmpLocHumanCount, TmpLocBothCount: Byte;
  TeamLocs: array of Integer;
begin
  if not AllReady then
  begin
    ErrorMsg := gResTexts[TX_LOBBY_EVERYONE_NOT_READY];
    Result := False;
    Exit;
  end;

  for I := 1 to fCount do
    if fNetPlayers[I].IsSpectator then
      Assert((fNetPlayers[I].PlayerNetType = nptHuman), 'Only humans can spectate');

  //All wrong start locations will be reset to random (fallback since UI should block that anyway)
  for I := 1 to fCount do
    if (fNetPlayers[I].StartLocation <> LOC_RANDOM) and (fNetPlayers[I].StartLocation <> LOC_SPECTATE) then
      if ((fNetPlayers[I].PlayerNetType = nptHuman) and not IsHumanLoc(fNetPlayers[I].StartLocation))
      or ((fNetPlayers[I].PlayerNetType = nptComputer) and not IsAILoc(fNetPlayers[I].StartLocation)) then
        fNetPlayers[I].StartLocation := LOC_RANDOM;

  for I := 1 to MAX_HANDS do
    UsedLoc[I] := False;

  //Remember all used locations and drop duplicates (fallback since UI should block that anyway)
  for I := 1 to fCount do
    if (fNetPlayers[I].StartLocation <> LOC_RANDOM) and (fNetPlayers[I].StartLocation <> LOC_SPECTATE) then
    begin
      if UsedLoc[fNetPlayers[I].StartLocation] then
        fNetPlayers[I].StartLocation := LOC_RANDOM
      else
        UsedLoc[fNetPlayers[I].StartLocation] := true;
    end;

  //Collect available locations in a list
  LocHumanCount := 0;
  LocBothCount := 0;
  for I := 1 to MAX_HANDS do
  if not UsedLoc[I] then
    begin
      if IsHumanLoc(I) and IsAILoc(I) then
      begin
        Inc(LocBothCount);
        AvailableLocBoth[LocBothCount] := I;
      end
      else
        if IsHumanLoc(I) then
        begin
          Inc(LocHumanCount);
          AvailableLocHuman[LocHumanCount] := I;
        end;
    end;

  //Make sure there's enough available locations for everyone who hasn't got one yet
  TmpLocHumanCount := LocHumanCount;
  TmpLocBothCount := LocBothCount;
  for I := 1 to fCount do
    if (fNetPlayers[I].StartLocation = LOC_RANDOM) and (fNetPlayers[I].PlayerNetType <> nptClosed) then
    begin
      if (fNetPlayers[I].PlayerNetType = nptHuman) and (TmpLocHumanCount > 0) then
        Dec(TmpLocHumanCount)
      else
        if TmpLocBothCount > 0 then
          Dec(TmpLocBothCount)
        else
        begin
          ErrorMsg := gResTexts[TX_LOBBY_UNABLE_RANDOM_LOCS];
          Result := False;
          Exit;
        end;
    end;

  RemAllClosedPlayers; //Closed players are just a marker in the lobby, delete them when the game starts

  //Randomize all available lists (don't use KaMRandom - we want varied results and PlayerList is synced to clients before start)
  for I := 1 to LocBothCount do
    SwapInt(AvailableLocBoth[I], AvailableLocBoth[Random(LocBothCount)+1]);
  for I := 1 to LocHumanCount do
    SwapInt(AvailableLocHuman[I], AvailableLocHuman[Random(LocHumanCount)+1]);

  //First assign Human only available locations, after that assign mixed ones together
  for I := 1 to fCount do
    if (fNetPlayers[I].StartLocation = LOC_RANDOM) and (fNetPlayers[I].PlayerNetType = nptHuman) and (LocHumanCount > 0) then
    begin
      fNetPlayers[I].StartLocation := AvailableLocHuman[LocHumanCount];
      Dec(LocHumanCount);
    end;

  //Now allocate locations that can be human or AI
  for I := 1 to fCount do
    if fNetPlayers[I].StartLocation = LOC_RANDOM then
    begin
      Assert(LocBothCount > 0, 'Not enough locations to allocate');
      fNetPlayers[I].StartLocation := AvailableLocBoth[LocBothCount];
      Dec(LocBothCount);
    end;

  //Check for odd players
  for I := 1 to fCount do
    Assert(fNetPlayers[I].StartLocation <> LOC_RANDOM, 'Everyone should have a starting location!');

  //Shuffle locations within each team if requested
  if RandomizeTeamLocations then
    for I := 1 to 4 do //Each team
    begin
      SetLength(TeamLocs, 0); //Reset
      for K := 1 to fCount do
        if (fNetPlayers[K].Team = I) and not fNetPlayers[K].IsSpectator then
        begin
          SetLength(TeamLocs, Length(TeamLocs)+1);
          TeamLocs[Length(TeamLocs)-1] := fNetPlayers[K].StartLocation;
        end;
      //Shuffle the locations
      for K := 0 to Length(TeamLocs)-1 do
        SwapInt(TeamLocs[K], TeamLocs[Random(Length(TeamLocs))]);
      //Assign each location back to a player
      J := 0;
      for K := 1 to fCount do
        if (fNetPlayers[K].Team = I) and not fNetPlayers[K].IsSpectator then
        begin
          fNetPlayers[K].StartLocation := TeamLocs[J];
          Inc(J);
        end;
    end;

  ValidateColors;
  Result := True;
end;


//Save whole amount of data as string to be sent across network to other players
//I estimate it ~50bytes per player at max
//later it will be byte array?
procedure TKMNetPlayersList.SaveToStream(aStream: TKMemoryStream);
var
  I: Integer;
begin
  aStream.Write(HostDoesSetup);
  aStream.Write(RandomizeTeamLocations);
  aStream.Write(SpectatorsAllowed);
  aStream.Write(SpectatorSlotsOpen);
  aStream.Write(VoteActive);
  aStream.Write(fCount);
  for I := 1 to fCount do
    fNetPlayers[I].Save(aStream);
end;


procedure TKMNetPlayersList.LoadFromStream(aStream: TKMemoryStream);
var
  I: Integer;
begin
  aStream.Read(HostDoesSetup);
  aStream.Read(RandomizeTeamLocations);
  aStream.Read(SpectatorsAllowed);
  aStream.Read(SpectatorSlotsOpen);
  aStream.Read(VoteActive);
  aStream.Read(fCount);
  for I := 1 to fCount do
    fNetPlayers[I].Load(aStream);
end;


function TKMNetPlayersList.GetSlotNames: UnicodeString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to fCount do
  begin
    Result := Result + fNetPlayers[I].SlotName;
    if I < fCount then
      Result := Result + '|';
  end;
end;


function TKMNetPlayersList.GetPlayersWithIDs: UnicodeString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to fCount do
  begin
    Result := Result + '   ' + IntToStr(I) + ': ' + UnicodeString(fNetPlayers[I].Nikname);
    if I < fCount then
      Result := Result + '|';
  end;
end;

end.
