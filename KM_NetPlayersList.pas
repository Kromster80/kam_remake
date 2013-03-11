unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, StrUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Player, KM_Locales;

const
  PING_COUNT = 20; //Number of pings to store and take the maximum over for latency calculation (pings are measured once per second)

type
  TNetPlayerType = (nptHuman, nptComputer, nptClosed);

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
    function GetFlagColor: Cardinal;
    procedure SetLangCode(const aCode: AnsiString);
  public
    PlayerNetType: TNetPlayerType; //Human, Computer, Closed
    StartLocation: Integer;  //Start location, 0 means random
    Team: Integer;
    ReadyToStart: Boolean;
    ReadyToPlay: Boolean;
    Connected: Boolean;      //Player is still connected
    Dropped: Boolean;        //Host elected to continue play without this player
    FPS: Cardinal;
    procedure AddPing(aPing: Word);
    procedure ResetPingRecord;
    function GetInstantPing: Word;
    function GetMaxPing: Word;
    function IsHuman: Boolean;
    function IsComputer: Boolean;
    function IsClosed: Boolean;
    function GetPlayerType: TPlayerType;
    function GetNickname: string;
    property Nikname: AnsiString read fNikname;
    property LangCode: AnsiString read fLangCode write SetLangCode;
    property IndexOnServer: Integer read fIndexOnServer;
    property SetIndexOnServer: Integer write fIndexOnServer;
    property FlagColor: Cardinal read GetFlagColor;
    property FlagColorID: Integer read fFlagColorID write fFlagColorID;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  //Handles everything related to players list,
  //but knows nothing about networking nor game setup. Only players.
  TKMNetPlayersList = class
  private
    fCount: Integer;
    fNetPlayers: array [1 .. MAX_PLAYERS] of TKMNetPlayerInfo;
    function GetPlayer(aIndex: Integer): TKMNetPlayerInfo;
    procedure ValidateColors;
    procedure RemAllClosedPlayers;
    procedure UpdateAIPlayerNames;
  public
    HostDoesSetup: Boolean; //Gives host absolute control over locations/teams (not colors)
    RandomizeTeamLocations: Boolean; //When the game starts locations are shuffled within each team
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count:integer read fCount;

    procedure AddPlayer(aNik: string; aIndexOnServer: Integer; const aLang: string = '');
    procedure AddAIPlayer(aSlot: Integer = -1);
    procedure AddClosedPlayer(aSlot: Integer = -1);
    procedure DisconnectPlayer(aIndexOnServer: Integer);
    procedure DisconnectAllClients(aOwnNikname: string);
    procedure DropPlayer(aIndexOnServer: Integer);
    procedure RemPlayer(aIndexOnServer: Integer);
    procedure RemAIPlayer(ID: Integer);
    procedure RemClosedPlayer(ID: Integer);
    property Player[aIndex: Integer]: TKMNetPlayerInfo read GetPlayer; default;

    //Getters
    function ServerToLocal(aIndexOnServer: Integer): Integer;
    function NiknameToLocal(aNikname: string): Integer;
    function StartingLocToLocal(aLoc: Integer): Integer;
    function PlayerIndexToLocal(aIndex: TPlayerIndex): Integer;

    function CheckCanJoin(aNik: string; aIndexOnServer: Integer): string;
    function CheckCanReconnect(aLocalIndex: Integer): string;
    function LocAvailable(aIndex: Integer): Boolean;
    function ColorAvailable(aIndex: Integer): Boolean;
    function AllReady: Boolean;
    function AllReadyToPlay: Boolean;
    function GetMaxHighestRoundTripLatency: Word;
    procedure GetNotReadyToPlayPlayers(aPlayerList: TStringList);
    function GetAICount: Integer;
    function GetClosedCount: Integer;
    function GetConnectedCount: Integer;

    procedure ResetLocAndReady;
    procedure SetAIReady;
    function ValidateSetup(aHumanUsableLocs, aAIUsableLocs: TPlayerIndexArray; out ErrorMsg: string): Boolean;

    //Import/Export
    function GetAsText: string; //Gets all relevant information as text string
    procedure SetAsText(const aText:string); //Sets all relevant information from text string
    function GetSimpleAsText: string; //Gets just names as a text string seperated by |
    function GetPlayersWithIDs: string;
  end;


implementation
uses KM_TextLibrary;


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


function TKMNetPlayerInfo.GetFlagColor: Cardinal;
begin
  if fFlagColorID <> 0 then
    Result := MP_TEAM_COLORS[fFlagColorID]
  else
    Result := $FF000000; //Black
end;


procedure TKMNetPlayerInfo.SetLangCode(const aCode: AnsiString);
begin
  if fLocales.GetIDFromCode(aCode) <> -1 then
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


function TKMNetPlayerInfo.GetPlayerType: TPlayerType;
const
  PlayerTypes: array[TNetPlayerType] of TPlayerType = (pt_Human, pt_Computer, pt_Computer);
begin
  Result := PlayerTypes[PlayerNetType];
end;


function TKMNetPlayerInfo.GetNickname: string;
begin
  case PlayerNetType of
    nptHuman:     Result := Nikname;
    nptComputer:  Result := fTextLibrary[TX_LOBBY_SLOT_AI_PLAYER];
    nptClosed:    Result := fTextLibrary[TX_LOBBY_SLOT_CLOSED];
    else          Result := NO_TEXT;
  end;
end;


procedure TKMNetPlayerInfo.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fNikname);
  LoadStream.Read(fLangCode);
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


procedure TKMNetPlayerInfo.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fNikname);
  SaveStream.Write(fLangCode);
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


{ TKMNetPlayersList }
constructor TKMNetPlayersList.Create;
var I: Integer;
begin
  inherited;
  for I := 1 to MAX_PLAYERS do
    fNetPlayers[I] := TKMNetPlayerInfo.Create;
end;


destructor TKMNetPlayersList.Destroy;
var I: Integer;
begin
  for I := 1 to MAX_PLAYERS do
    fNetPlayers[I].Free;
  inherited;
end;


procedure TKMNetPlayersList.Clear;
begin
  HostDoesSetup := False;
  RandomizeTeamLocations := False;
  fCount := 0;
end;


function TKMNetPlayersList.GetPlayer(aIndex: Integer): TKMNetPlayerInfo;
begin
  Result := fNetPlayers[aIndex];
end;


procedure TKMNetPlayersList.ValidateColors;
var
  i,k,ColorCount:integer;
  UsedColor:array[0..MP_COLOR_COUNT] of boolean; //0 means Random
  AvailableColor:array[1..MP_COLOR_COUNT] of byte;
begin
  //All wrong colors will be reset to random
  for i:=1 to fCount do
    if not Math.InRange(fNetPlayers[i].FlagColorID, 0, MP_COLOR_COUNT) then
      fNetPlayers[i].FlagColorID := 0;

  FillChar(UsedColor, SizeOf(UsedColor), #0);

  //Remember all used colors and drop duplicates
  for i:=1 to fCount do
    if UsedColor[fNetPlayers[i].FlagColorID] then
      fNetPlayers[i].FlagColorID := 0
    else
      UsedColor[fNetPlayers[i].FlagColorID] := true;

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
  if fNetPlayers[i].FlagColorID = 0 then
  begin
    inc(k);
    if k<=ColorCount then
      fNetPlayers[i].FlagColorID := AvailableColor[k];
  end;

  //Check for odd players
  for i:=1 to fCount do
    Assert(fNetPlayers[i].FlagColorID <> 0, 'Everyone should have a color!');
end;


procedure TKMNetPlayersList.RemAllClosedPlayers;
var I: Integer;
begin
  for i:=fCount downto 1 do
    if Player[i].IsClosed then
      RemClosedPlayer(i);
end;


procedure TKMNetPlayersList.AddPlayer(aNik: string; aIndexOnServer: Integer; const aLang: string = '');
begin
  Assert(fCount <= MAX_PLAYERS, 'Can''t add player');
  Inc(fCount);
  fNetPlayers[fCount].fNikname := aNik;
  fNetPlayers[fCount].fLangCode := aLang;
  fNetPlayers[fCount].fIndexOnServer := aIndexOnServer;
  fNetPlayers[fCount].PlayerNetType := nptHuman;
  //fPlayers[fCount].PlayerIndex := nil;
  fNetPlayers[fCount].Team := 0;
  fNetPlayers[fCount].FlagColorID := 0;
  fNetPlayers[fCount].StartLocation := 0;
  fNetPlayers[fCount].ReadyToStart := false;
  fNetPlayers[fCount].ReadyToPlay := false;
  fNetPlayers[fCount].Connected := true;
  fNetPlayers[fCount].Dropped := false;
  fNetPlayers[fCount].ResetPingRecord;
end;


procedure TKMNetPlayersList.AddAIPlayer(aSlot: Integer = -1);
begin
  if aSlot = -1 then
  begin
    Assert(fCount <= MAX_PLAYERS,'Can''t add AI player');
    inc(fCount);
    aSlot := fCount;
  end;
  fNetPlayers[aSlot].fNikname := 'AI Player';
  fNetPlayers[aSlot].fLangCode := '';
  fNetPlayers[aSlot].fIndexOnServer := -1;
  fNetPlayers[aSlot].PlayerNetType := nptComputer;
  fNetPlayers[aSlot].Team := 0;
  fNetPlayers[aSlot].FlagColorID := 0;
  fNetPlayers[aSlot].StartLocation := 0;
  fNetPlayers[aSlot].ReadyToStart := true;
  fNetPlayers[aSlot].ReadyToPlay := true;
  fNetPlayers[aSlot].Connected := true;
  fNetPlayers[aSlot].Dropped := false;
  fNetPlayers[aSlot].ResetPingRecord;
  UpdateAIPlayerNames;
end;


procedure TKMNetPlayersList.AddClosedPlayer(aSlot: Integer = -1);
begin
  if aSlot = -1 then
  begin
    Assert(fCount <= MAX_PLAYERS,'Can''t add closed player');
    inc(fCount);
    aSlot := fCount;
  end;
  fNetPlayers[aSlot].fNikname := 'Closed';
  fNetPlayers[aSlot].fLangCode := '';
  fNetPlayers[aSlot].fIndexOnServer := -1;
  fNetPlayers[aSlot].PlayerNetType := nptClosed;
  fNetPlayers[aSlot].Team := 0;
  fNetPlayers[aSlot].FlagColorID := 0;
  fNetPlayers[aSlot].StartLocation := 0;
  fNetPlayers[aSlot].ReadyToStart := true;
  fNetPlayers[aSlot].ReadyToPlay := true;
  fNetPlayers[aSlot].Connected := true;
  fNetPlayers[aSlot].Dropped := false;
  fNetPlayers[aSlot].ResetPingRecord;
end;


//Set player to no longer be connected, but do not remove them from the game
procedure TKMNetPlayersList.DisconnectPlayer(aIndexOnServer:integer);
var ID:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot disconnect player');
  fNetPlayers[ID].Connected := false;
end;

//Mark all human players as disconnected (used when reconnecting if all clients were lost)
procedure TKMNetPlayersList.DisconnectAllClients(aOwnNikname:string);
var I: Integer;
begin
  for i:=1 to fCount do
    if (fNetPlayers[i].IsHuman) and (fNetPlayers[i].Nikname <> aOwnNikname) then
      fNetPlayers[i].Connected := false;
end;


//Set player to no longer be on the server, but do not remove their assets from the game
procedure TKMNetPlayersList.DropPlayer(aIndexOnServer:integer);
var ID:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot drop player');
  fNetPlayers[ID].Connected := false;
  fNetPlayers[ID].Dropped := true;
end;


procedure TKMNetPlayersList.RemPlayer(aIndexOnServer:integer);
var ID,I: Integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot remove player');
  fNetPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fNetPlayers[i] := fNetPlayers[i+1]; //Shift only pointers

  fNetPlayers[fCount] := TKMNetPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
end;


procedure TKMNetPlayersList.RemAIPlayer(ID:integer);
var I: Integer;
begin
  fNetPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fNetPlayers[i] := fNetPlayers[i+1]; //Shift only pointers

  fNetPlayers[fCount] := TKMNetPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
  UpdateAIPlayerNames;
end;


procedure TKMNetPlayersList.RemClosedPlayer(ID:integer);
var I: Integer;
begin
  fNetPlayers[ID].Free;
  for i:=ID to fCount-1 do
    fNetPlayers[i] := fNetPlayers[i+1]; //Shift only pointers

  fNetPlayers[fCount] := TKMNetPlayerInfo.Create; //Empty players are created but not used
  dec(fCount);
end;


procedure TKMNetPlayersList.UpdateAIPlayerNames;
var i, AICount:integer;
begin
  AICount := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].PlayerNetType = nptComputer then
    begin
      inc(AICount);
      fNetPlayers[i].fNikname := 'AI Player '+IntToStr(AICount);
    end;
end;


function TKMNetPlayersList.ServerToLocal(aIndexOnServer:integer):integer;
var I: Integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fNetPlayers[i].fIndexOnServer = aIndexOnServer then
    begin
      Result := i;
      Exit;
    end;
end;


//Networking needs to convert Nikname to local index in players list
function TKMNetPlayersList.NiknameToLocal(aNikname:string):integer;
var I: Integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fNetPlayers[i].fNikname = aNikname then
      Result := i;
end;


function TKMNetPlayersList.StartingLocToLocal(aLoc:integer):integer;
var I: Integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fNetPlayers[i].StartLocation = aLoc then
      Result := i;
end;


function TKMNetPlayersList.PlayerIndexToLocal(aIndex: TPlayerIndex): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 1 to Count do
    if (aIndex = fNetPlayers[I].StartLocation - 1) then
      Result := I;
end;


//See if player can join our game
function TKMNetPlayersList.CheckCanJoin(aNik:string; aIndexOnServer:integer):string;
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
function TKMNetPlayersList.CheckCanReconnect(aLocalIndex:integer):string;
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


function TKMNetPlayersList.LocAvailable(aIndex:integer):boolean;
var I: Integer;
begin
  Result := true;
  if aIndex=0 then exit;

  for i:=1 to fCount do
    Result := Result and not (fNetPlayers[i].StartLocation = aIndex);
end;


function TKMNetPlayersList.ColorAvailable(aIndex:integer):boolean;
var I: Integer;
begin
  Result := true;
  if aIndex=0 then exit;

  for i:=1 to fCount do
    Result := Result and not (fNetPlayers[i].FlagColorID = aIndex);
end;


function TKMNetPlayersList.AllReady:boolean;
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


procedure TKMNetPlayersList.GetNotReadyToPlayPlayers(aPlayerList:TStringList);
var I: Integer;
begin
  for i:=1 to fCount do
    if (not fNetPlayers[i].ReadyToPlay) and fNetPlayers[i].IsHuman and fNetPlayers[i].Connected then
      aPlayerList.Add(fNetPlayers[i].Nikname);
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


function TKMNetPlayersList.GetConnectedCount:integer;
var I: Integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fNetPlayers[i].IsHuman and fNetPlayers[i].Connected then
      inc(Result);
end;


procedure TKMNetPlayersList.ResetLocAndReady;
var I: Integer;
begin
  for i:=1 to fCount do
  begin
    fNetPlayers[i].StartLocation := 0;
    if fNetPlayers[i].PlayerNetType = nptHuman then //AI/closed players are always ready
      fNetPlayers[i].ReadyToStart := false;
  end;
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


//Convert undefined/random start locations to fixed and assign random colors
//Remove odd players
function TKMNetPlayersList.ValidateSetup(aHumanUsableLocs, aAIUsableLocs: TPlayerIndexArray; out ErrorMsg:String):boolean;

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
  I, K, J:integer;
  UsedLoc:array of boolean;
  AvailableLocHuman, AvailableLocBoth:array[1..MAX_PLAYERS] of byte;
  LocHumanCount, LocBothCount: Byte;
  TmpLocHumanCount, TmpLocBothCount: Byte;
  TeamLocs:array of Integer;
begin
  if not AllReady then begin
    ErrorMsg := fTextLibrary[TX_LOBBY_EVERYONE_NOT_READY];
    Result := False;
    Exit;
  end;

  //All wrong start locations will be reset to "undefined"
  for i:=1 to fCount do
    if ((fNetPlayers[i].PlayerNetType = nptHuman) and not IsHumanLoc(fNetPlayers[i].StartLocation))
    or ((fNetPlayers[i].PlayerNetType = nptComputer) and not IsAILoc(fNetPlayers[i].StartLocation)) then
      fNetPlayers[i].StartLocation := 0;

  SetLength(UsedLoc, MAX_PLAYERS+1); //01..MAX_PLAYERS, all false
  for i:=1 to MAX_PLAYERS do UsedLoc[i] := false;

  //Remember all used locations and drop duplicates
  for i:=1 to fCount do
    if UsedLoc[fNetPlayers[i].StartLocation] then
      fNetPlayers[i].StartLocation := 0
    else
      UsedLoc[fNetPlayers[i].StartLocation] := true;

  //Collect available locations in a list
  LocHumanCount := 0;
  LocBothCount := 0;
  for I:=1 to MAX_PLAYERS do
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
  for i:=1 to fCount do
    if (fNetPlayers[i].StartLocation = 0) and (fNetPlayers[i].PlayerNetType <> nptClosed) then
    begin
      if (fNetPlayers[i].PlayerNetType = nptHuman) and (TmpLocHumanCount > 0) then
        Dec(TmpLocHumanCount)
      else
        if TmpLocBothCount > 0 then
          Dec(TmpLocBothCount)
        else
        begin
          ErrorMsg := fTextLibrary[TX_LOBBY_UNABLE_RANDOM_LOCS];
          Result := False;
          Exit;
        end;
    end;
    
  RemAllClosedPlayers; //Closed players are just a marker in the lobby, delete them when the game starts

  //Randomize all available lists (don't use KaMRandom - we want varied results and PlayerList is synced to clients before start)
  for i:=1 to LocBothCount do
    SwapInt(AvailableLocBoth[i], AvailableLocBoth[Random(LocBothCount)+1]);
  for i:=1 to LocHumanCount do
    SwapInt(AvailableLocHuman[i], AvailableLocHuman[Random(LocHumanCount)+1]);

  //First assign Human only available locations, after that assign mixed ones together
  for i:=1 to fCount do
    if (fNetPlayers[i].StartLocation = 0) and (fNetPlayers[i].PlayerNetType = nptHuman) and (LocHumanCount > 0) then
    begin
      fNetPlayers[i].StartLocation := AvailableLocHuman[LocHumanCount];
      Dec(LocHumanCount);
    end;

  //Now allocate locations that can be human or AI
  for i:=1 to fCount do
    if fNetPlayers[i].StartLocation = 0 then
    begin
      Assert(LocBothCount > 0, 'Not enough locations to allocate');
      fNetPlayers[i].StartLocation := AvailableLocBoth[LocBothCount];
      Dec(LocBothCount);
    end;

  //Check for odd players
  for i:=1 to fCount do
    Assert(fNetPlayers[i].StartLocation <> 0, 'Everyone should have a starting location!');

  //Shuffle locations within each team if requested
  if RandomizeTeamLocations then
    for I := 1 to 4 do //Each team
    begin
      SetLength(TeamLocs, 0); //Reset
      for K := 1 to fCount do
        if fNetPlayers[K].Team = I then
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
        if fNetPlayers[K].Team = I then
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
function TKMNetPlayersList.GetAsText:string;
var I: Integer; M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;

  M.Write(HostDoesSetup);
  M.Write(RandomizeTeamLocations);
  M.Write(fCount);
  for i:=1 to fCount do
    fNetPlayers[i].Save(M);

  Result := M.ReadAsText;
  M.Free;
end;


procedure TKMNetPlayersList.SetAsText(const aText:string);
var I: Integer; M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aText);
    M.Read(HostDoesSetup);
    M.Read(RandomizeTeamLocations);
    M.Read(fCount);
    for i:=1 to fCount do
      fNetPlayers[i].Load(M);
  finally
    M.Free;
  end;
end;


function TKMNetPlayersList.GetSimpleAsText:string;
var I: Integer;
begin
  for i:=1 to fCount do
  begin
    Result := Result + StringReplace(fNetPlayers[i].Nikname,'|','',[rfReplaceAll]);
    if i < fCount then Result := Result + '|';
  end;
end;


function TKMNetPlayersList.GetPlayersWithIDs:string;
var I: Integer;
begin
  for i:=1 to fCount do
  begin
    Result := Result + '   ' +IntToStr(i) + ': ' + fNetPlayers[i].Nikname;
    if i < fCount then Result := Result + '|';
  end;
end;

end.
