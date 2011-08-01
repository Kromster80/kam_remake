unit KM_PlayersCollection;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Units_Warrior, KM_Houses, KM_CommonTypes, KM_Defaults, KM_Player, KM_Utils, KM_Points;


{ Players are identified by their starting location }
type
  TKMPlayersCollection = class
  private
    fCount:byte;
    fPlayerList:array of TKMPlayer;
    fPlayerAnimals:TKMPlayerAnimals;
    function GetPlayer(Index:integer):TKMPlayer;
  public
    Selected: TObject; //Unit or House

    constructor Create;
    destructor Destroy; override;

    property Count:byte read fCount;
    property Player[Index:integer]:TKMPlayer read GetPlayer; default;
    property PlayerAnimals:TKMPlayerAnimals read fPlayerAnimals;

    procedure AddPlayers(aCount:byte); //Batch add several players

    procedure RemovePlayer(aIndex:TPlayerIndex);
    procedure AfterMissionInit(aFlattenRoads:boolean);
    procedure UpdateMultiplayerTeams;
    function HousesHitTest(X,Y:Integer):TKMHouse;
    function UnitsHitTestF(aLoc: TKMPointF; aIncludeAnimals:boolean): TKMUnit;
    function GetClosestUnit(aLoc:TKMPoint; aIndex:TPlayerIndex; aAlliance:TAllianceType): TKMUnit;
    function GetClosestHouse(aLoc:TKMPoint; aIndex:TPlayerIndex; aAlliance:TAllianceType; aOnlyCompleted:boolean=true): TKMHouse;
    function GetHouseByID(aID: Integer): TKMHouse;
    function GetUnitByID(aID: Integer): TKMUnit;
    function HitTest(X,Y:Integer):boolean;
    function GetUnitCount:integer;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType; out PlacePoint: TKMPoint):Boolean;
    function CheckAlliance(aPlay1,aPlay2:TPlayerIndex):TAllianceType;
    procedure CleanUpUnitPointer(var aUnit: TKMUnit); overload;
    procedure CleanUpUnitPointer(var aUnit: TKMUnitWarrior); overload;
    procedure CleanUpHousePointer(var aHouse: TKMHouse); overload;
    procedure CleanUpHousePointer(var aHouse: TKMHouseInn); overload;
    procedure CleanUpHousePointer(var aHouse: TKMHouseSchool); overload;
    function RemAnyHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
    function RemAnyUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
    procedure RevealForTeam(aPlayer: TPlayerIndex; Pos:TKMPoint; Radius,Amount:word);
    procedure SyncFogOfWar;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;
    procedure UpdateState(Tick:cardinal);
    procedure Paint;
  end;

var
  fPlayers: TKMPlayersCollection;
  MyPlayer: TKMPlayer; //shortcut to access players player


implementation
uses KM_Terrain, KM_Game, KM_Log;


{TKMAllPlayers}
constructor TKMPlayersCollection.Create;
begin
  Inherited Create;
  fPlayerAnimals := TKMPlayerAnimals.Create(PLAYER_ANIMAL); //Always create Animals
end;


destructor TKMPlayersCollection.Destroy;
var i:integer;
begin
  for i:=0 to fCount-1 do
    FreeThenNil(fPlayerList[i]);

  PlayerAnimals.Free;

  MyPlayer := nil;
  Selected := nil;
  Inherited;
end;


function TKMPlayersCollection.GetPlayer(Index:integer):TKMPlayer;
begin
  Assert(InRange(Index, 0, fCount));
  Result := fPlayerList[Index];
end;


procedure TKMPlayersCollection.AddPlayers(aCount: byte);
var i:integer;
begin
  Assert(fCount+aCount <= MAX_PLAYERS);

  SetLength(fPlayerList, fCount+aCount);

  for i:=fCount to fCount+aCount-1 do
    fPlayerList[i] := TKMPlayer.Create(i);

  fCount := fCount+aCount;
end;


procedure TKMPlayersCollection.AfterMissionInit(aFlattenRoads:boolean);
var i:integer;
begin
  for i:=0 to fCount-1 do
    fPlayerList[i].AfterMissionInit(aFlattenRoads);
end;


procedure TKMPlayersCollection.UpdateMultiplayerTeams;
var i,k:integer;
begin
  for i:=1 to fGame.Networking.NetPlayers.Count do
    for k:=1 to fGame.Networking.NetPlayers.Count do
      if (fGame.Networking.NetPlayers[i].Team = 0) or (fGame.Networking.NetPlayers[i].Team <> fGame.Networking.NetPlayers[k].Team) then
        fGame.Networking.NetPlayers[i].PlayerIndex.Alliances[fGame.Networking.NetPlayers[k].PlayerIndex.PlayerIndex] := at_Enemy
      else
        fGame.Networking.NetPlayers[i].PlayerIndex.Alliances[fGame.Networking.NetPlayers[k].PlayerIndex.PlayerIndex] := at_Ally;
end;


//Remove player aIndex
//todo: Comment and refactor
procedure TKMPlayersCollection.RemovePlayer(aIndex:TPlayerIndex);
var i,k:integer;
begin
  Assert(MyPlayer <> fPlayerList[aIndex], 'Can not remove Player referenced by MyPlayer');

  for i:=0 to fCount-1 do
    fPlayerList[i].Goals.RemoveReference(aIndex);

  FreeThenNil(fPlayerList[aIndex]);

  for i:=aIndex to fCount-2 do
  begin
    fPlayerList[i] := fPlayerList[i+1];
    fPlayerList[i].SetPlayerID(i);
  end;

  dec(fCount);
  SetLength(fPlayerList, fCount);

  for i:=0 to fCount-1 do
    for k:=aIndex to fCount-1 do
      fPlayerList[i].Alliances[k] := fPlayerList[i].Alliances[k+1];

  fTerrain.RemovePlayer(aIndex);
end;


function TKMPlayersCollection.HousesHitTest(X, Y: Integer): TKMHouse;
var i:integer;
begin
  Result:=nil;
  for i:=0 to fCount-1 do
  begin
    Result := fPlayerList[i].HousesHitTest(X,Y);
    if Result<>nil then exit; //Assuming that there can't be 2 houses on one tile
  end;
end;


//Floating-point hit test version, required for Projectiles
//Return unit within range of 1 from aLoc
//TODO: Remove in favor of UnitHitTest on Terrain
function TKMPlayersCollection.UnitsHitTestF(aLoc: TKMPointF; aIncludeAnimals:boolean): TKMUnit;
var i,X,Y:integer; U:TKMUnit;
begin
  Result := nil;

  for i:=0 to fCount-1 do
  for Y:=trunc(aLoc.Y) to ceil(aLoc.Y) do //test four related tiles around
  for X:=trunc(aLoc.X) to ceil(aLoc.X) do begin
    U := fPlayerList[i].UnitsHitTest(X,Y);
    if U<>nil then
      if (Result=nil) or (GetLength(U.PositionF,aLoc)<GetLength(Result.PositionF,aLoc)) then
        Result := U;
  end;

  if (Result = nil) and (aIncludeAnimals) then
  for Y:=trunc(aLoc.Y) to ceil(aLoc.Y) do //test four related tiles around
  for X:=trunc(aLoc.X) to ceil(aLoc.X) do
    Result := PlayerAnimals.UnitsHitTest(X,Y);
end;


function TKMPlayersCollection.GetClosestUnit(aLoc:TKMPoint; aIndex:TPlayerIndex; aAlliance:TAllianceType): TKMUnit;
var i:integer; U:TKMUnit;
begin
  Result := nil;

  for i:=0 to fCount-1 do
  if (aIndex<>i) and (CheckAlliance(aIndex,i) = aAlliance) then
  begin
    U := fPlayerList[i].Units.GetClosestUnit(aLoc);
    if (U<>nil) and ((Result=nil) or (GetLength(U.PositionF, KMPointF(aLoc)) < GetLength(Result.PositionF, KMPointF(aLoc)))) then
      Result := U;
  end;
end;


//Get closest house. Note: we check by house cells, not by entrance
function TKMPlayersCollection.GetClosestHouse(aLoc:TKMPoint; aIndex:TPlayerIndex; aAlliance:TAllianceType; aOnlyCompleted:boolean=true): TKMHouse;
var i:integer; H:TKMHouse;
begin
  Result := nil;

  for i:=0 to fCount-1 do
  if (aIndex<>i) and (CheckAlliance(aIndex,i) = aAlliance) then
  begin
    H := fPlayerList[i].Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, aOnlyCompleted);
    if (H<>nil) and ((Result=nil) or (H.GetDistance(aLoc) < Result.GetDistance(aLoc))) then
      Result := H;
  end;
end;


function TKMPlayersCollection.GetHouseByID(aID: Integer): TKMHouse;
var i:integer;
begin
  Result := nil;
  if aID = 0 then exit;

  for i:=0 to fCount-1 do
  begin
    Result := fPlayerList[i].Houses.GetHouseByID(aID);
    if Result<>nil then exit; //else keep on testing
  end;
end;


function TKMPlayersCollection.GetUnitByID(aID: Integer): TKMUnit;
var i:integer;
begin
  Result := nil;
  if aID = 0 then exit;

  for i:=0 to fCount-1 do
  begin
    Result := fPlayerList[i].Units.GetUnitByID(aID);
    if Result<>nil then exit; //else keep on testing
  end;
  if Result = nil then Result := PlayerAnimals.Units.GetUnitByID(aID);
end;


{ HitTest for houses/units altogether
  Houses have priority over units, so you can't select an occupant.
  (however, this is only true if the house is built)
  Player should not be able to select dying units either }
function TKMPlayersCollection.HitTest(X,Y: Integer):boolean;
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
function TKMPlayersCollection.GetUnitCount:integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to fCount-1 do
    inc(Result, fPlayerList[i].Units.Count);
end;


{Should return closest position where unit can be placed}
function TKMPlayersCollection.FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType; out PlacePoint: TKMPoint):Boolean;
var
  i:integer;
  P:TKMPointI;
  T:TKMPoint;
  aPass:TPassability; //temp for required passability
begin
  Result := False; // if function fails to find valid position

  if aUnitType in [ut_Wolf..ut_Duck] then
    aPass := AnimalTerrain[byte(aUnitType)]
  else
    aPass := CanWalk;

  for i:=0 to 255 do begin
    P := GetPositionFromIndex(KMPoint(PosX,PosY), i);
    if fTerrain.TileInMapCoords(P.X,P.Y) then begin
      T := KMPoint(P);
      if fTerrain.CheckPassability(T, aPass) and (fTerrain.GetWalkConnectID(KMPoint(PosX,PosY)) = fTerrain.GetWalkConnectID(T))
      and not fTerrain.HasUnit(T) then begin
        PlacePoint := T; // Assign if all test are passed
        Result := True;
        exit;
      end;
    end;
  end;
end;


{ Check how Player1 feels towards Player2. Note: this is position dependant,
e.g. Play1 may be allied with Play2, but Play2 may be enemy to Play1}
function TKMPlayersCollection.CheckAlliance(aPlay1,aPlay2:TPlayerIndex):TAllianceType;
begin
  Result := at_Ally;

  if (aPlay1 = PLAYER_ANIMAL) or (aPlay2 = PLAYER_ANIMAL) or (aPlay1 = aPlay2) then
    Exit;

  Result := fPlayerList[aPlay1].Alliances[aPlay2];
end;


procedure TKMPlayersCollection.CleanUpUnitPointer(var aUnit: TKMUnit);
begin
  if (aUnit <> nil) and not fGame.IsExiting then
    aUnit.ReleaseUnitPointer;
  aUnit := nil;
end;

procedure TKMPlayersCollection.CleanUpUnitPointer(var aUnit: TKMUnitWarrior);
begin
  CleanUpUnitPointer(TKMUnit(aUnit));
end;


procedure TKMPlayersCollection.CleanUpHousePointer(var aHouse: TKMHouse);
begin
  if (aHouse <> nil) and not fGame.IsExiting then
    aHouse.ReleaseHousePointer;
  aHouse := nil;
end;


procedure TKMPlayersCollection.CleanUpHousePointer(var aHouse: TKMHouseInn);
begin
  CleanUpHousePointer(TKMHouse(aHouse));
end;


procedure TKMPlayersCollection.CleanUpHousePointer(var aHouse: TKMHouseSchool);
begin
  CleanUpHousePointer(TKMHouse(aHouse));
end;


function TKMPlayersCollection.RemAnyHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
var i:integer;
begin
  Result := false;
  for i:=0 to fCount-1 do
    Result := Result or fPlayerList[i].RemHouse(Position, DoSilent, Simulated, IsEditor);
end;


function TKMPlayersCollection.RemAnyUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
var i:integer;
begin
  Result := false;
  for i:=0 to fCount-1 do
    Result := Result or fPlayerList[i].RemUnit(Position, Simulated);
end;


procedure TKMPlayersCollection.RevealForTeam(aPlayer: TPlayerIndex; Pos:TKMPoint; Radius,Amount:word);
var i:integer;
begin
  fPlayerList[aPlayer].FogOfWar.RevealCircle(Pos,Radius,Amount);
  if fGame.MultiplayerMode then
    for i:=0 to fCount-1 do
      if (i<>aPlayer) and (fPlayerList[aPlayer].Alliances[i] = at_Ally) then
        fPlayerList[i].FogOfWar.RevealCircle(Pos,Radius,Amount);
end;


procedure TKMPlayersCollection.SyncFogOfWar;
var i,k: integer;
begin
  for i:=0 to fCount-1 do
    for k:=0 to fCount-1 do
      if (i<>k) and (fPlayerList[i].Alliances[k] = at_Ally) then
        fPlayerList[k].FogOfWar.SyncFOW(fPlayerList[i].FogOfWar);
end;


procedure TKMPlayersCollection.Save(SaveStream:TKMemoryStream);
var i:word;
begin
  SaveStream.Write('Players');
  SaveStream.Write(fCount);
  for i:=0 to fCount-1 do
    fPlayerList[i].Save(SaveStream);
  PlayerAnimals.Save(SaveStream);
  if not fGame.MultiplayerMode then
    SaveStream.Write(MyPlayer.PlayerIndex)
  else
    SaveStream.Write(Player[0].PlayerIndex); //Multiplayer saves must be identical
end;


procedure TKMPlayersCollection.Load(LoadStream:TKMemoryStream);
var i:word; s:string; PlayerIndex:TPlayerIndex;
begin
  LoadStream.Read(s);
  Assert(s = 'Players', 'Players not found');
  LoadStream.Read(fCount);
  fLog.AssertToLog(fCount <= MAX_PLAYERS,'Player count in savegame exceeds MAX_PLAYERS allowed by Remake');

  SetLength(fPlayerList, fCount);

  for i:=0 to fCount-1 do
  begin
    fPlayerList[i] := TKMPlayer.Create(0);
    fPlayerList[i].Load(LoadStream);
  end;
  PlayerAnimals.Load(LoadStream);

  LoadStream.Read(PlayerIndex);
  MyPlayer := fPlayerList[PlayerIndex];
  Selected := nil;
end;


procedure TKMPlayersCollection.SyncLoad;
var i:byte;
begin
  for i:=0 to fCount-1 do
    fPlayerList[i].SyncLoad;
  PlayerAnimals.SyncLoad;
end;


procedure TKMPlayersCollection.IncAnimStep;
var i:byte;
begin
  for i:=0 to fCount-1 do
    fPlayerList[i].IncAnimStep;
end;


procedure TKMPlayersCollection.UpdateState(Tick:cardinal);
var i:byte;
begin
  for i:=0 to fCount-1 do
    if fGame.GameState in [gsRunning, gsReplay] then
      fPlayerList[i].UpdateState(Tick, i)
    else
      Exit; //PlayerAI can stop the game and clear everything

  PlayerAnimals.UpdateState;
end;


procedure TKMPlayersCollection.Paint;
var i:integer;
begin
  for i:=0 to fCount-1 do
    fPlayerList[i].Paint;
  PlayerAnimals.Paint;
end;


end.
