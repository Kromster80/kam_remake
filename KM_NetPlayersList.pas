unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
  KM_CommonTypes, KM_Defaults,
  KM_Player;


type
  TKMPlayerInfo = class
  private
    fAddress:string;
    fNikname:string;
    fTimeTick:cardinal;
  public
    PlayerType:TPlayerType; //Human, Computer
    FlagColorID:integer; //Flag color, 0 means random
    StartLocID:integer; //Start location, 0 means random
    ReadyToStart:boolean;
    ReadyToPlay:boolean;
    Alive:boolean; //Player is still connected and not defeated

    PingSent:cardinal; //Time of last "ping" message
    Ping:word; //Last known ping
  public
    function IsHuman:boolean;
    property Address:string read fAddress;
    property Nikname:string read fNikname;
    property TimeTick:cardinal read fTimeTick write fTimeTick;
  end;

  //Handles everything related to players list,
  //but knows nothing about networking nor game setup. Only players.
  TKMPlayersList = class
  private
    fCount:integer;
    fPlayers:array [1..MAX_PLAYERS] of TKMPlayerInfo;
    function GetPlayer(Index:integer):TKMPlayerInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count:integer read fCount;

    procedure AddPlayer(aAddr,aNik:string; aTick:cardinal);
    procedure RemPlayer(aIndex:integer);
    property Player[Index:integer]:TKMPlayerInfo read GetPlayer; default;

    //Getters
    function NiknameIndex(aNik:string):integer;
    function CheckCanJoin(aAddr, aNik:string):string;
    function LocAvailable(aIndex:integer):boolean;
    function AllReady:boolean;
    function AllReadyToPlay:boolean;

    procedure ResetLocAndReady;
    function DropMissing(aTick:cardinal):string;
    procedure DefineSetup(aMaxLoc:byte);

    //Import/Export
    function GetAsText:string; //Gets all relevant information as text string
    procedure SetAsText(const aText:string); //Sets all relevant information from text string
  end;

implementation


{ TKMPlayerInfo }
function TKMPlayerInfo.IsHuman:boolean;
begin
  Result := PlayerType = pt_Human;
end;


{ TKMPlayersList }
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


function TKMPlayersList.GetPlayer(Index:integer):TKMPlayerInfo;
begin
  Result := fPlayers[Index];
end;


procedure TKMPlayersList.AddPlayer(aAddr,aNik:string; aTick:cardinal);
begin
  inc(fCount);
  fPlayers[fCount].fAddress := aAddr;
  fPlayers[fCount].fNikname := aNik;
  fPlayers[fCount].PlayerType := pt_Human;
  fPlayers[fCount].FlagColorID := 0;
  fPlayers[fCount].StartLocID := 0;
  fPlayers[fCount].ReadyToStart := false;
  fPlayers[fCount].ReadyToPlay := false;
  fPlayers[fCount].Alive := true;
  fPlayers[fCount].TimeTick := aTick;
end;


procedure TKMPlayersList.RemPlayer(aIndex:integer);
var i:integer;
begin
  Assert(InRange(aIndex, 1, fCount), 'Can not remove player');
  for i:=aIndex to fCount-1 do
    fPlayers[i] := fPlayers[i+1]; //todo: I wonder if that's ill strategy to handle Tsomething

  //Cleanup to avoid consequences of erroneous access
  FillChar(fPlayers[fCount], SizeOf(fPlayers[fCount]), #0);

  dec(fCount);
end;


function TKMPlayersList.NiknameIndex(aNik:string):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].fNikname = aNik then
      Result := i;
end;


//See if player can join our game
function TKMPlayersList.CheckCanJoin(aAddr, aNik:string):string;
begin
  if fCount >= MAX_PLAYERS then
    Result := 'No more players can join the game'
  else
  if NiknameIndex(aNik) <> -1 then
    Result := 'Player with this nik already joined the game';
end;


function TKMPlayersList.LocAvailable(aIndex:integer):boolean;
var i:integer;
begin
  Result := true;
  if aIndex=0 then exit;

  Result := aIndex <= 6; // Check with map max players

  for i:=1 to fCount do
    Result := Result and not (fPlayers[i].StartLocID = aIndex);
end;


function TKMPlayersList.AllReady:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    Result := Result and fPlayers[i].ReadyToStart;
end;


function TKMPlayersList.AllReadyToPlay:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    Result := Result and fPlayers[i].ReadyToPlay;
end;


procedure TKMPlayersList.ResetLocAndReady;
var i:integer;
begin
  for i:=1 to fCount do
  begin
    fPlayers[i].StartLocID := 0;
    fPlayers[i].ReadyToStart := false;
  end;
end;


function TKMPlayersList.DropMissing(aTick:cardinal):string;
var i:integer;
begin
  Result := '';
  for i:=fCount downto 2 do //Don't check Host
    if aTick > fPlayers[i].fTimeTick then
    begin
      Result := Result + fPlayers[i].fNikname;
      RemPlayer(i);
    end;
end;


//Convert undefined/random start locations to fixed
//Remove odd players
procedure TKMPlayersList.DefineSetup(aMaxLoc:byte);
var 
  i,k,LocCount:integer;
  UsedLoc:array[0..MAX_PLAYERS] of boolean;
  AvailableLoc:array[1..MAX_PLAYERS] of byte;
begin

  //All wrong start locations will be reset to "undefined"
  for i:=1 to fCount do
    if fPlayers[i].StartLocID > aMaxLoc then fPlayers[i].StartLocID := 0;

  //Remember all used locations and drop duplicates
  for i:=1 to fCount do
    if UsedLoc[fPlayers[i].StartLocID] then
      fPlayers[i].StartLocID := 0
    else
      UsedLoc[fPlayers[i].StartLocID] := true;

  //Collect available locations
  LocCount := 0;
  for i:=1 to aMaxLoc do
  if not UsedLoc[i] then begin
    inc(LocCount);
    AvailableLoc[LocCount] := i;
  end;

  //Randomize
  for i:=1 to LocCount do
    SwapInt(AvailableLoc[i], AvailableLoc[random(LocCount)+1]);

  //Allocate available starting locations
  k := 0;
  for i:=1 to fCount do
  if fPlayers[i].StartLocID = 0 then begin
    inc(k);
    if k<=LocCount then
      fPlayers[i].StartLocID := AvailableLoc[k];
  end;

  //Drop odd players
  for i:=fCount downto 1 do
  if fPlayers[i].StartLocID = 0 then
    RemPlayer(i);
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
  begin
    M.Write(fPlayers[i].fAddress);
    M.Write(fPlayers[i].fNikname);
    M.Write(fPlayers[i].PlayerType, SizeOf(fPlayers[i].PlayerType));
    M.Write(fPlayers[i].FlagColorID);
    M.Write(fPlayers[i].StartLocID);
    M.Write(fPlayers[i].ReadyToStart);
    M.Write(fPlayers[i].Alive);
  end;

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
    begin
      M.Read(fPlayers[i].fAddress);
      M.Read(fPlayers[i].fNikname);
      M.Read(fPlayers[i].PlayerType, SizeOf(fPlayers[i].PlayerType));
      M.Read(fPlayers[i].FlagColorID);
      M.Read(fPlayers[i].StartLocID);
      M.Read(fPlayers[i].ReadyToStart);
      M.Read(fPlayers[i].Alive);
    end;
  finally
    M.Free;
  end;
end;


end.
