unit KM_PlayersCollection;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_CommonTypes, KM_Defaults, KM_Player, KM_PlayerAI, KM_Utils;

type
  TMissionMode = (mm_Normal, mm_Tactic);

type
  TKMAllPlayers = class
  private
    fPlayerCount:integer;
  public
    fMissionMode: TMissionMode;
    Player:array[1..MAX_PLAYERS] of TKMPlayerAssets;
    PlayerAI:array[1..MAX_PLAYERS] of TKMPlayerAI;
    PlayerAnimals: TKMPlayerAnimals;
    Selected: TObject;
  public
    constructor Create(aPlayerCount:integer);
    destructor Destroy; override;
  public
    procedure AfterMissionInit(aFlattenRoads:boolean);
    procedure SetPlayerCount(aPlayerCount:integer);
    property PlayerCount:integer read fPlayerCount;
    function HousesHitTest(X,Y:Integer):TKMHouse;
    function UnitsHitTestF(aLoc: TKMPointF): TKMUnit;
    function GetHouseByID(aID: Integer): TKMHouse;
    function GetUnitByID(aID: Integer): TKMUnit;
    function HitTest(X,Y:Integer):boolean;
    function GetUnitCount():integer;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
    function CheckAlliance(aPlay1,aPlay2:TPlayerID):TAllianceType;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
    procedure IncAnimStep;
    procedure UpdateState(Tick:cardinal);
    procedure Paint;
  end;

var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player

  
implementation
uses KM_Terrain, KM_UnitTaskDie;


{TKMAllPlayers}
constructor TKMAllPlayers.Create(aPlayerCount:integer);
begin
  Inherited Create;
  SetPlayerCount(aPlayerCount);
  PlayerAnimals := TKMPlayerAnimals.Create;
end;


destructor TKMAllPlayers.Destroy;
var i:integer;
begin
  for i:=1 to MAX_PLAYERS do begin //Free all just in case
    FreeThenNil(Player[i]);
    FreeThenNil(PlayerAI[i]);
  end;
  FreeThenNil(PlayerAnimals);

  MyPlayer := nil;
  Selected := nil;
  Inherited;
end;


procedure TKMAllPlayers.AfterMissionInit(aFlattenRoads:boolean);
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].AfterMissionInit(aFlattenRoads);
end;


procedure TKMAllPlayers.SetPlayerCount(aPlayerCount:integer);
var i:integer;
begin
  fLog.AssertToLog(InRange(PlayerCount,0,MAX_PLAYERS),'PlayerCount exceeded');

  fPlayerCount := aPlayerCount; //Used internally
  for i:=1 to fPlayerCount do begin
    if Player[i]   = nil then Player[i]   := TKMPlayerAssets.Create(TPlayerID(i));
    if PlayerAI[i] = nil then PlayerAI[i] := TKMPlayerAI.Create(Player[i]);
  end;
  for i:=fPlayerCount+1 to MAX_PLAYERS do begin
    FreeThenNil(Player[i]);
    FreeThenNil(PlayerAI[i]);
  end;
end;


function TKMAllPlayers.HousesHitTest(X, Y: Integer): TKMHouse;
var i:integer;
begin
  Result:=nil;
  for i:=1 to fPlayerCount do begin
    Result := Player[i].HousesHitTest(X,Y);
    if Result<>nil then exit; //Assuming that there can't be 2 houses on one tile
  end;
end;


//Floating-point hit test version, required for Projectiles
//Return unit within range of 1 from aLoc
function TKMAllPlayers.UnitsHitTestF(aLoc: TKMPointF): TKMUnit;
var i,X,Y:integer; U:TKMUnit;
begin
  Result := nil;

  for i:=1 to fPlayerCount do
  for Y:=trunc(aLoc.Y) to ceil(aLoc.Y) do //test four related tiles around
  for X:=trunc(aLoc.X) to ceil(aLoc.X) do begin
    U := Player[i].UnitsHitTest(X,Y);
    if U<>nil then
      if (Result=nil) or (GetLength(U.PositionF,aLoc)<GetLength(U.PositionF,Result.PositionF)) then
        Result := U;
  end;

  if Result = nil then
  for Y:=trunc(aLoc.Y) to ceil(aLoc.Y) do //test four related tiles around
  for X:=trunc(aLoc.X) to ceil(aLoc.X) do
    Result := PlayerAnimals.UnitsHitTest(X,Y);
end;
    

function TKMAllPlayers.GetHouseByID(aID: Integer): TKMHouse;
var i:integer;
begin
  Result := nil;
  if aID = 0 then exit;

  for i:=1 to fPlayerCount do
  if Player[i]<>nil then
  begin
    Result := Player[i].GetHouseByID(aID);
    if Result<>nil then Break; //else keep on testing
  end;
end;


function TKMAllPlayers.GetUnitByID(aID: Integer): TKMUnit;
var i:integer;
begin
  Result := nil;
  if aID = 0 then exit;

  for i:=1 to fPlayerCount do
  if Player[i]<>nil then
  begin
    Result := Player[i].GetUnitByID(aID);
    if Result<>nil then Break; //else keep on testing
  end;
  if Result = nil then Result := PlayerAnimals.GetUnitByID(aID);
end;


{ HitTest for houses/units altogether
  Houses have priority over units, so you can't select an occupant.
  (however, this is only true if the house is built)
  Player should not be able to select dying units either }
function TKMAllPlayers.HitTest(X,Y: Integer):boolean;
var H:TKMHouse; U:TKMUnit;
begin
  H := MyPlayer.HousesHitTest(X,Y);
  if (H<>nil) and (H.BuildingState in [hbs_Stone,hbs_Done]) then
    fPlayers.Selected := H
  else begin
    U := MyPlayer.UnitsHitTest(X,Y);
    if (U<>nil) and (not U.IsDeadOrDying) then
      fPlayers.Selected := U
    else
      fPlayers.Selected := H;
  end;

  Result := fPlayers.Selected <> nil;
end;


//Get total unit count for statistics display
function TKMAllPlayers.GetUnitCount():integer;
var i:integer;
begin
  Result:=0;
  for i:=1 to fPlayerCount do
    inc(Result,Player[i].GetUnits.Count);
end;


{Should return closest position where unit can be placed}
function TKMAllPlayers.FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
var
  i:integer;
  P:TKMPointI;
  T:TKMPoint;
  aPass:TPassability; //temp for required passability
begin
  if aUnitType in [ut_Wolf..ut_Duck] then
    aPass := AnimalTerrain[byte(aUnitType)]
  else
    aPass := canWalk;

  for i:=0 to 255 do begin
    P := GetPositionFromIndex(KMPoint(PosX,PosY), i);
    if not fTerrain.TileInMapCoords(P.X,P.Y) then continue;
    T := KMPoint(P.X,P.Y);
    if not fTerrain.CheckPassability(T, aPass) or fTerrain.HasUnit(T) then continue;
    Result := T; //Assign if all test are passed
    exit;
  end;

  Result := KMPoint(0,0); //if function fails to find valid position
end;


{ Check how Player1 feels towards Player2. Note: this is position dependant,
e.g. Play1 may be allied with Play2, but Play2 may be enemy to Play1}
function TKMAllPlayers.CheckAlliance(aPlay1,aPlay2:TPlayerID):TAllianceType;
begin
  Assert(InRange(byte(aPlay1),1,MAX_PLAYERS+1) and InRange(byte(aPlay2),1,MAX_PLAYERS+1)); //Max_players + Animals

  if (aPlay1 = aPlay2) or (aPlay1 = play_Animals) or (aPlay2 = play_Animals) then
    Result := at_Ally
  else
    Result := Player[byte(aPlay1)].fAlliances[byte(aPlay2)];
end;


procedure TKMAllPlayers.Save(SaveStream:TKMemoryStream);
var i:word;
begin
  SaveStream.Write('Players');
  SaveStream.Write(fMissionMode, SizeOf(fMissionMode));
  SaveStream.Write(fPlayerCount);
  for i:=1 to fPlayerCount do
  begin
    Player[i].Save(SaveStream);
    PlayerAI[i].Save(SaveStream); //Saves AI stuff
  end;
  PlayerAnimals.Save(SaveStream);
  SaveStream.Write(MyPlayer.PlayerID, SizeOf(MyPlayer.PlayerID));
end;


procedure TKMAllPlayers.Load(LoadStream:TKMemoryStream);
var i:word; s:string; P:TPlayerID;
begin
  LoadStream.Read(s); if s <> 'Players' then exit;
  LoadStream.Read(fMissionMode, SizeOf(fMissionMode));
  LoadStream.Read(fPlayerCount);
  fLog.AssertToLog(fPlayerCount <= MAX_PLAYERS,'Player count in savegame exceeds MAX_PLAYERS allowed by Remake');
  Selected := nil;

  for i:=1 to fPlayerCount do
  begin
    if Player[i]   = nil then   Player[i] := TKMPlayerAssets.Create(TPlayerID(i));
    if PlayerAI[i] = nil then PlayerAI[i] := TKMPlayerAI.Create(Player[i]);

    Player[i].Load(LoadStream);
    PlayerAI[i].Load(LoadStream);
  end;
  PlayerAnimals.Load(LoadStream);

  LoadStream.Read(P, SizeOf(P));
  MyPlayer := fPlayers.Player[integer(P)];
end;


procedure TKMAllPlayers.SyncLoad();
var i:byte;
begin
  for i:=1 to fPlayerCount do
  begin
    Player[i].SyncLoad;
    PlayerAI[i].SyncLoad;
  end;
  PlayerAnimals.SyncLoad;
end;


procedure TKMAllPlayers.IncAnimStep();
var i:byte;
begin
  for i:=1 to fPlayerCount do
    Player[i].IncAnimStep;
end;


procedure TKMAllPlayers.UpdateState(Tick:cardinal);
var i:byte;
begin
  for i:=1 to fPlayerCount do begin
    DO_WEIGHT_ROUTES := i=1;
    Player[i].UpdateState;

  end;
  PlayerAnimals.UpdateState;

  //This is not ajoined with previous loop since it can result in StopGame which flushes all data
  for i:=1 to fPlayerCount do
    if (Tick+i) mod 20 = 0 then
    begin//Do only one player per Tick
      PlayerAI[i].UpdateState;
    end;
end;


procedure TKMAllPlayers.Paint;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].Paint;
  PlayerAnimals.Paint;
end;




end.
