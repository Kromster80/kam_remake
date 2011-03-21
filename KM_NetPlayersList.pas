unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, StrUtils, SysUtils, Windows,
  KM_CommonTypes, KM_Defaults,
  KM_Player;


type
  TKMPlayerInfo = class
    private
      fAddress:string;
      fNikname:string;
    public
      PlayerType:TPlayerType; //Human, Computer
      FlagColor:cardinal; //Flag color, 0 means random
      StartLocID:integer; //Start location, 0 means random
      Alliances:array[1..MAX_PLAYERS] of TAllianceType;
      ReadyToStart:boolean;
    public
      function IsHuman:boolean;
      property Address:string read fAddress;
      property Nikname:string read fNikname;
  end;


//Handles everything related to players list,
//but knows nothing about networking nor game setup. Only players.
type
  TKMPlayersList = class
    private
      fCount:integer;
      fPlayers:array [1..MAX_PLAYERS] of TKMPlayerInfo;
      function GetAsStringList:string;
      function GetPlayerInt(Index:integer):TKMPlayerInfo;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property Count:integer read fCount;

      procedure AddPlayer(aAddr,aNik:string);
      property Player[Index:integer]:TKMPlayerInfo read GetPlayerInt; default;

      //Getters
      function NiknameIndex(aNik:string):integer;
      function GetStartLoc(aNik:string):integer;
      function AllReady:boolean;

      //Import/Export
      property AsStringList:string read GetAsStringList; //Acquire list of players for UI Listbox
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


function TKMPlayersList.GetAsStringList:string;
var i:integer;
begin
  Result := '';
  for i:=1 to fCount do
    Result := Result + fPlayers[i].Address + '/' + fPlayers[i].Nikname + eol;
end;


function TKMPlayersList.GetPlayerInt(Index:integer):TKMPlayerInfo;
begin
  Result := fPlayers[Index];
end;


procedure TKMPlayersList.AddPlayer(aAddr,aNik:string);
var i:integer;
begin
  inc(fCount);
  fPlayers[fCount].fAddress := aAddr;
  fPlayers[fCount].fNikname := aNik;
  fPlayers[fCount].PlayerType := pt_Human;
  fPlayers[fCount].FlagColor := 0;
  fPlayers[fCount].StartLocID := 0;
  for i:=1 to MAX_PLAYERS do
    fPlayers[fCount].Alliances[i] := at_Enemy;
  fPlayers[fCount].ReadyToStart := false;
end;


function TKMPlayersList.NiknameIndex(aNik:string):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].fNikname = aNik then
      Result := i;
end;


function TKMPlayersList.GetStartLoc(aNik:string):integer;
var i:integer;
begin
  Result := -1;
  for i:=1 to fCount do
    if fPlayers[i].Nikname = aNik then
      if fPlayers[i].StartLocID = 0 then Result := i //Order
                                    else Result := fPlayers[i].StartLocID;
  Assert(Result<>-1, 'Net Player is missing');
end;


function TKMPlayersList.AllReady:boolean;
var i:integer;
begin
  Result := true;
  for i:=1 to fCount do
    Result := Result and fPlayers[i].ReadyToStart;
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
    end;
  finally
    M.Free;
  end;
end;


end.
