unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, StrUtils, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Player, KM_Utils;

const
  PING_COUNT = 20; //Number of pings to store and take the maximum over for latency calculation (pings are measured once per second)

type
  TKMPlayerInfo = class
  private
    fNikname:string;
    fIndexOnServer:integer;
    fFlagColorID:integer;    //Flag color, 0 means random
    fPings: array[0..PING_COUNT-1] of word; //Ring buffer
    fPingPos:byte;
    function GetFlagColor:cardinal;
  public
    PlayerType:TPlayerType; //Human, Computer
    StartLocation:integer;  //Start location, 0 means random
    Team:integer;
    PlayerIndex:TKMPlayer;
    ReadyToStart:boolean;
    ReadyToPlay:boolean;
    Alive:boolean;          //Player is still connected and not defeated
    procedure AddPing(aPing:word);
    function GetInstantPing:word;
    function GetMaxPing:word;
    function IsHuman:boolean;
    property Nikname:string read fNikname;
    property IndexOnServer:integer read fIndexOnServer;
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
    procedure UpdateAIPlayerNames;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count:integer read fCount;

    procedure AddPlayer(aNik:string; aIndexOnServer:integer);
    procedure AddAIPlayer;
    procedure KillPlayer(aIndexOnServer:integer);
    procedure RemPlayer(aIndexOnServer:integer);
    procedure RemAIPlayer(ID:integer);
    property Player[Index:integer]:TKMPlayerInfo read GetPlayer; default;

    //Getters
    function ServerToLocal(aIndexOnServer:integer):integer;
    function NiknameToLocal(aNikname:string):integer;
    function StartingLocToLocal(aLoc:integer):integer;
    function PlayerIndexToLocal(aIndex:TPlayerIndex):integer;
    function CheckCanJoin(aNik:string; aIndexOnServer:integer):string;
    function LocAvailable(aIndex:integer):boolean;
    function ColorAvailable(aIndex:integer):boolean;
    function AllReady:boolean;
    function AllReadyToPlay:boolean;
    function GetMaxHighestRoundTripLatency:word;
    procedure GetNotReadyToPlayPlayers(aPlayerList:TStringList);
    function GetAICount:integer;

    procedure ResetLocAndReady;
    procedure SetAIReady;
    function ValidateSetup(aMaxLoc:byte; out ErrorMsg:String):boolean;

    //Import/Export
    function GetAsText:string; //Gets all relevant information as text string
    procedure SetAsText(const aText:string); //Sets all relevant information from text string
    function GetSimpleAsText:string; //Gets just names as a text string seperated by |
  end;

implementation


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
  Result := PlayerType = pt_Human;
end;


procedure TKMPlayerInfo.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fNikname);
  LoadStream.Read(fIndexOnServer);
  LoadStream.Read(PlayerType, SizeOf(PlayerType));
  LoadStream.Read(fFlagColorID);
  LoadStream.Read(StartLocation);
  LoadStream.Read(Team);
  LoadStream.Read(ReadyToStart);
  LoadStream.Read(ReadyToPlay);
  LoadStream.Read(Alive);
end;


procedure TKMPlayerInfo.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fNikname);
  SaveStream.Write(fIndexOnServer);
  SaveStream.Write(PlayerType, SizeOf(PlayerType));
  SaveStream.Write(fFlagColorID);
  SaveStream.Write(StartLocation);
  SaveStream.Write(Team);
  SaveStream.Write(ReadyToStart);
  SaveStream.Write(ReadyToPlay);
  SaveStream.Write(Alive);
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

  //Randomize
  for i:=1 to LocCount do
    SwapInt(AvailableLoc[i], AvailableLoc[KaMRandom(LocCount)+1]);

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

  //Randomize
  for i:=1 to ColorCount do
    SwapInt(AvailableColor[i], AvailableColor[KaMRandom(ColorCount)+1]);

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


procedure TKMPlayersList.AddPlayer(aNik:string; aIndexOnServer:integer);
begin
  inc(fCount);
  fPlayers[fCount].fNikname := aNik;
  fPlayers[fCount].fIndexOnServer := aIndexOnServer;
  fPlayers[fCount].PlayerType := pt_Human;
  fPlayers[fCount].PlayerIndex := nil;
  fPlayers[fCount].Team := 0;
  fPlayers[fCount].FlagColorID := 0;
  fPlayers[fCount].StartLocation := 0;
  fPlayers[fCount].ReadyToStart := false;
  fPlayers[fCount].ReadyToPlay := false;
  fPlayers[fCount].Alive := true;
end;


procedure TKMPlayersList.AddAIPlayer;
begin
  inc(fCount);
  fPlayers[fCount].fNikname := 'AI Player';
  fPlayers[fCount].fIndexOnServer := -1;
  fPlayers[fCount].PlayerType := pt_Computer;
  fPlayers[fCount].PlayerIndex := nil;
  fPlayers[fCount].Team := 0;
  fPlayers[fCount].FlagColorID := 0;
  fPlayers[fCount].StartLocation := 0;
  fPlayers[fCount].ReadyToStart := true;
  fPlayers[fCount].ReadyToPlay := true;
  fPlayers[fCount].Alive := true;
  UpdateAIPlayerNames;
end;


//Set player to no longer be alive, but do not remove them from the game
procedure TKMPlayersList.KillPlayer(aIndexOnServer:integer);
var ID:integer;
begin
  ID := ServerToLocal(aIndexOnServer);
  Assert(ID <> -1, 'Cannot kill player');
  fPlayers[ID].Alive := false;
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


procedure TKMPlayersList.UpdateAIPlayerNames;
var i, AICount:integer;
begin
  AICount := 0;
  for i:=1 to fCount do
    if fPlayers[i].PlayerType = pt_Computer then
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
    if fPlayers[i].Alive and fPlayers[i].IsHuman then
      Result := Result and fPlayers[i].ReadyToStart;
end;


function TKMPlayersList.AllReadyToPlay:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    if fPlayers[i].Alive and fPlayers[i].IsHuman then
      Result := Result and fPlayers[i].ReadyToPlay;
end;


function TKMPlayersList.GetMaxHighestRoundTripLatency:word;
var i:integer; Highest, Highest2: word;
begin
  Highest := 0;
  Highest2 := 0;
  for i:=1 to fCount do
    if fPlayers[i].Alive and fPlayers[i].IsHuman then
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
    if (not fPlayers[i].ReadyToPlay) and fPlayers[i].IsHuman and fPlayers[i].Alive then
      aPlayerList.Add(fPlayers[i].Nikname);
end;


function TKMPlayersList.GetAICount:integer;
var i:integer;
begin
  Result := 0;
  for i:=1 to fCount do
    if fPlayers[i].PlayerType = pt_Computer then
      inc(Result);
end;


procedure TKMPlayersList.ResetLocAndReady;
var i:integer;
begin
  for i:=1 to fCount do
  begin
    fPlayers[i].StartLocation := 0;
    if fPlayers[i].PlayerType <> pt_Computer then //AI players are always ready
      fPlayers[i].ReadyToStart := false;
  end;
end;


procedure TKMPlayersList.SetAIReady;
var i:integer;
begin
  for i:=1 to fCount do
    if fPlayers[i].PlayerType = pt_Computer then
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
    ErrorMsg := 'Not everyone is ready to start';
    Result := False;
  end else
  if fCount > aMaxLoc then begin
    ErrorMsg := 'Players count exceeds map limit';
    Result := False;
  end else begin
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
    Result := Result + StringReplace(fPlayers[i].Nikname,'|','',[rfReplaceAll]) + '|';
end;


end.
