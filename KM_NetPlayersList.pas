unit KM_NetPlayersList;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, StrUtils, SysUtils, Windows,
  KM_CommonTypes, KM_Defaults,
  KM_Player;


type
  TKMPlayerInfo = class
    Addr:string;
    Nikname:string;
    PlayerType:TPlayerType; //pt_Human, pt_Computer
    FlagColor:cardinal; //Flag color, 0 means random
    StartLocID:integer; //Start location, 0 means random
    Alliances:array[1..MAX_PLAYERS] of TAllianceType;
    ReadyToStart:boolean;
  end;


//Handles everything related to players list,
//but knows nothing about networking nor game setup. Only players.
type
  TKMPlayersList = class
    private
      fCount:integer;
      fPlayers:array [1..MAX_PLAYERS] of TKMPlayerInfo;
      function GetAsStringList:string;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property Count:integer read fCount;

      //Setters
      procedure AddPlayer(aAddr,aNik:string);
      //procedure SetColor(aNik:string; aColor:cardinal);
      //procedure SetStartLoc(aNik:string; aLoc:integer);
      //procedure SetAlliances(aNik:string; aAlliances);
      procedure SetReady(aNik:string{; aReady:boolean});

      //Getters
      function GetAddress(aIndex:integer):string;
      function GetNikname(aIndex:integer):string;
      function NiknameExists(aNik:string):boolean;
      function IsHuman(aIndex:integer):boolean;
      function GetStartLoc(aNik:string):integer;
      function AllReady:boolean;

      //Import/Export
      property AsStringList:string read GetAsStringList; //Acquire list of players for UI Listbox
      function GetAsText:string; //Gets all relevant information as text string
      procedure SetAsText(a:string); //Sets all relevant information from text string
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


function TKMPlayersList.GetAsStringList:string;
var i:integer;
begin
  Result := '';
  for i:=1 to fCount do
    Result := Result + fPlayers[i].Addr + '/' + fPlayers[i].Nikname + eol;
end;


procedure TKMPlayersList.AddPlayer(aAddr,aNik:string);
var i:integer;
begin
  inc(fCount);
  fPlayers[fCount].Addr := aAddr;
  fPlayers[fCount].Nikname := aNik;
  fPlayers[fCount].PlayerType := pt_Human;
  fPlayers[fCount].FlagColor := 0;
  fPlayers[fCount].StartLocID := 0;
  for i:=1 to MAX_PLAYERS do
    fPlayers[fCount].Alliances[i] := at_Enemy;
  fPlayers[fCount].ReadyToStart := false;
end;


procedure TKMPlayersList.SetReady(aNik:string);
var i:integer;
begin
  for i:=1 to fCount do
    if fPlayers[i].Nikname = aNik then
      fPlayers[i].ReadyToStart := true;
end;


function TKMPlayersList.GetAddress(aIndex:integer):string;
begin
  Result := fPlayers[aIndex].Addr;
end;


function TKMPlayersList.GetNikname(aIndex:integer):string;
begin
  Result := fPlayers[aIndex].Nikname;
end;


function TKMPlayersList.NiknameExists(aNik:string):boolean;
var i:integer;
begin
  Result := false;
  for i:=1 to fCount do
    if fPlayers[i].Nikname = aNik then
      Result := true;
end;


function TKMPlayersList.IsHuman(aIndex:integer):boolean;
begin
  Result := fPlayers[aIndex].PlayerType = pt_Human;
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
    M.Write(fPlayers[i].Addr);
    M.Write(fPlayers[i].Nikname);
  end;

  Result := M.ReadAsText;
  M.Free;
end;


procedure TKMPlayersList.SetAsText(a:string);
var i:integer; M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.WriteAsText(a);

  M.Read(fCount);
  for i:=1 to fCount do
  begin
    M.Read(fPlayers[i].Addr);
    M.Read(fPlayers[i].Nikname);
  end;

  M.Free;
end;


end.
