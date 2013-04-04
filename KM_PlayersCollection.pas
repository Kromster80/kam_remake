unit KM_PlayersCollection;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_UnitGroups, KM_Terrain, KM_Houses,
  KM_Player, KM_PlayerSpectator, KM_Utils, KM_Points;


{ Players are identified by their starting location }
type
  TKMPlayersCollection = class
  private
    fCount: Byte;
    fPlayerList: array of TKMPlayer;
    fPlayerAnimals: TKMPlayerAnimals;
    function GetPlayer(aIndex: Integer): TKMPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Byte read fCount;
    property Player[aIndex: Integer]: TKMPlayer read GetPlayer; default;
    property PlayerAnimals: TKMPlayerAnimals read fPlayerAnimals;

    procedure AddPlayers(aCount: Byte); //Batch add several players

    procedure RemoveEmptyPlayers;
    procedure RemovePlayer(aIndex: TPlayerIndex);
    procedure AfterMissionInit(aFlattenRoads: Boolean);
    function HousesHitTest(X,Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function GroupsHitTest(X, Y: Integer): TKMUnitGroup;
    function GetClosestUnit(aLoc: TKMPoint; aIndex: TPlayerIndex; aAlliance: TAllianceType): TKMUnit;
    function GetClosestHouse(aLoc: TKMPoint; aIndex: TPlayerIndex; aAlliance: TAllianceType; aOnlyCompleted: Boolean = True): TKMHouse;
    function DistanceToEnemyTowers(aLoc: TKMPoint; aIndex: TPlayerIndex): Single;
    procedure GetUnitsInRect(aRect: TKMRect; List: TList);
    function GetHouseByID(aID: Integer): TKMHouse;
    function GetUnitByID(aID: Integer): TKMUnit;
    function GetGroupByID(aID: Integer): TKMUnitGroup;
    function HitTest(X,Y: Integer): TObject;
    function UnitCount:integer;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType: TUnitType; out PlacePoint: TKMPoint; RequiredWalkConnect:byte):Boolean;

    //Check how Player1 feels towards Player2
    //Note: this is position dependant, e.g. Player1 may be allied with
    //      Player2, but Player2 could be an enemy to Player1
    function CheckAlliance(aPlay1, aPlay2: TPlayerIndex): TAllianceType;
    procedure CleanUpUnitPointer(var aUnit: TKMUnit);
    procedure CleanUpGroupPointer(var aGroup: TKMUnitGroup);
    procedure CleanUpHousePointer(var aHouse: TKMHouse);
    procedure RemAnyHouse(Position: TKMPoint);
    procedure RemAnyUnit(Position: TKMPoint);
    procedure RevealForTeam(aPlayer: TPlayerIndex; Pos: TKMPoint; Radius,Amount:word);
    procedure SyncFogOfWar;
    procedure AddDefaultMPGoals(aMissionMode: TKMissionMode);

    procedure Save(SaveStream: TKMemoryStream; aMultiplayer: Boolean);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;

    procedure UpdateState(aTick: Cardinal);
    procedure Paint;
  end;

var
  fPlayers: TKMPlayersCollection;
  MySpectator: TKMSpectator; //Wrap to acces player/fow separately


implementation
uses KM_Game, KM_Log, KM_Resource, KM_AIFields, KM_Units_Warrior;


{ TKMPlayersCollection }
constructor TKMPlayersCollection.Create;
begin
  inherited Create;

  fPlayerAnimals := TKMPlayerAnimals.Create(PLAYER_ANIMAL); //Always create Animals
end;


destructor TKMPlayersCollection.Destroy;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    FreeThenNil(fPlayerList[I]);

  PlayerAnimals.Free;

  inherited;
end;


function TKMPlayersCollection.GetPlayer(aIndex: Integer): TKMPlayer;
begin
  Assert(InRange(aIndex, 0, fCount-1));
  Result := fPlayerList[aIndex];
end;


procedure TKMPlayersCollection.AddPlayers(aCount: Byte);
var I: Integer;
begin
  Assert(fCount+aCount <= MAX_PLAYERS);

  SetLength(fPlayerList, fCount + aCount);

  for I := fCount to fCount + aCount - 1 do
    fPlayerList[I] := TKMPlayer.Create(I);

  fCount := fCount + aCount;
end;


procedure TKMPlayersCollection.AfterMissionInit(aFlattenRoads: Boolean);
var
  I: Integer;
begin
  fAIFields.AfterMissionInit;

  for I := 0 to fCount - 1 do
    fPlayerList[I].AfterMissionInit(aFlattenRoads);
end;


//We assume that if player has no assets it is unused and can be removed, so that remaining players
//will be tightly packed and mission info will display correct player count
//Accessed only by MapEditor when it needs to remove empty players before saving a map
procedure TKMPlayersCollection.RemoveEmptyPlayers;
var I: Integer;
begin
  for I := Count - 1 downto 0 do
    if fPlayerList[I].GetFieldsCount + fPlayerList[I].Houses.Count + fPlayerList[I].Units.Count = 0 then
      RemovePlayer(I);
end;


//Remove player 'aIndex'
//Accessed only by MapEditor when it needs to remove empty players before saving a map
procedure TKMPlayersCollection.RemovePlayer(aIndex: TPlayerIndex);
var i,k:integer;
begin
  //Remove other players goals using this player
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
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].HousesHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 houses on one tile
  end;
end;


function TKMPlayersCollection.UnitsHitTest(X, Y: Integer): TKMUnit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].UnitsHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 units on one tile
  end;
end;


function TKMPlayersCollection.GroupsHitTest(X, Y: Integer): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].GroupsHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 groups on one tile
  end;
end;


function TKMPlayersCollection.GetClosestUnit(aLoc: TKMPoint; aIndex: TPlayerIndex; aAlliance: TAllianceType): TKMUnit;
var
  I: Integer;
  U: TKMUnit;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
  if (aIndex <> I) and (CheckAlliance(aIndex, I) = aAlliance) then
  begin
    U := fPlayerList[I].Units.GetClosestUnit(aLoc);
    if (U <> nil)
    and ((Result = nil) or (KMLengthSqr(U.PositionF, KMPointF(aLoc)) < KMLengthSqr(Result.PositionF, KMPointF(aLoc)))) then
      Result := U;
  end;
end;


//Get closest house. Note: we check by house cells, not by entrance
function TKMPlayersCollection.GetClosestHouse(aLoc: TKMPoint; aIndex: TPlayerIndex; aAlliance: TAllianceType; aOnlyCompleted: Boolean = True): TKMHouse;
var
  I: Integer;
  H: TKMHouse;
begin
  Result := nil;

  //Check all players
  for I := 0 to fCount - 1 do
  if (aIndex <> I) and (Player[aIndex].Alliances[I] = aAlliance) then
  begin
    H := fPlayerList[I].Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, aOnlyCompleted);
    if (H <> nil) and ((Result = nil) or (H.GetDistance(aLoc) < Result.GetDistance(aLoc))) then
      Result := H;
  end;
end;


//Return distance from the tile to the closest enemy tower
function TKMPlayersCollection.DistanceToEnemyTowers(aLoc: TKMPoint; aIndex: TPlayerIndex): Single;
var
  I, K: Integer;
  H: TKMHouseTower;
begin
  Result := MaxSingle;
  for I := 0 to fCount - 1 do
  if (aIndex <> I) and (Player[aIndex].Alliances[I] = at_Enemy) then
  begin
    for K := fPlayerList[I].Houses.Count - 1 downto 0 do
    if fPlayerList[I].Houses[K] is TKMHouseTower then
    begin
      H := TKMHouseTower(fPlayerList[I].Houses[K]);
      //Don't use H.GetDistance (any tile within house) as that's not how tower range works
      Result := Min(Result, KMLength(H.GetPosition, aLoc));
    end;
  end;
end;


function TKMPlayersCollection.GetHouseByID(aID: Integer): TKMHouse;
var I: Integer;
begin
  Result := nil;
  if aID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].Houses.GetHouseByID(aID);
    if Result <> nil then Exit; //else keep on testing
  end;
end;


function TKMPlayersCollection.GetUnitByID(aID: Integer): TKMUnit;
var I: Integer;
begin
  Result := nil;
  if aID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].Units.GetUnitByID(aID);
    if Result <> nil then Exit; //else keep on testing
  end;
  if Result = nil then Result := PlayerAnimals.Units.GetUnitByID(aID);
end;


function TKMPlayersCollection.GetGroupByID(aID: Integer): TKMUnitGroup;
var I: Integer;
begin
  Result := nil;
  if aID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fPlayerList[I].UnitGroups.GetGroupByID(aID);
    if Result <> nil then Exit; //else keep on testing
  end;
end;


function TKMPlayersCollection.HitTest(X,Y: Integer): TObject;
var
  H: TKMHouse;
  U: TKMUnit;
  G: TKMUnitGroup;
begin
  //Houses have priority over units, so you can't select an occupant
  //Selection priority is as follows:
  //BuiltHouses > UnitGroups > Units > IncompleteHouses

  H := HousesHitTest(X,Y);
  if (H <> nil) and (H.BuildingState in [hbs_Stone, hbs_Done]) then
    Result := H
  else begin
    G := GroupsHitTest(X,Y);
    if (G <> nil) then
      Result := G
    else
    begin
      U := UnitsHitTest(X,Y);
      if (U <> nil) and (not U.IsDeadOrDying) then
        Result := U
      else
        Result := H; //Incomplete house or nil
    end;
  end;
end;


//Get total unit count for statistics display
function TKMPlayersCollection.UnitCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
    Inc(Result, fPlayerList[I].Units.Count);
end;


procedure TKMPlayersCollection.GetUnitsInRect(aRect: TKMRect; List: TList);
var I: Integer;
begin
  Assert(List.Count = 0);

  for I := 0 to fCount - 1 do
    fPlayerList[I].Units.GetUnitsInRect(aRect, List);
end;


{Should return closest position where unit can be placed}
function TKMPlayersCollection.FindPlaceForUnit(PosX,PosY:integer; aUnitType: TUnitType; out PlacePoint: TKMPoint; RequiredWalkConnect:byte):Boolean;
var
  I: Integer;
  P: TKMPointI;
  T: TKMPoint;
  Pass: TPassability; //temp for required passability
begin
  Result := False; // if function fails to find valid position
  Pass := fResource.UnitDat[aUnitType].AllowedPassability;

  for I := 0 to 255 do
  begin
    P := GetPositionFromIndex(KMPoint(PosX,PosY), I);
    if fTerrain.TileInMapCoords(P.X,P.Y) then
    begin
      T := KMPoint(P);
      if fTerrain.CheckPassability(T, Pass) and (fTerrain.GetWalkConnectID(T) = RequiredWalkConnect)
      and not fTerrain.HasUnit(T) then
      begin
        PlacePoint := T; // Assign if all test are passed
        Result := True;
        exit;
      end;
    end;
  end;
end;


{ Check how Player1 feels towards Player2. Note: this is position dependant,
e.g. Play1 may be allied with Play2, but Play2 may be enemy to Play1}
function TKMPlayersCollection.CheckAlliance(aPlay1,aPlay2: TPlayerIndex): TAllianceType;
begin
  Result := at_Ally;

  if (aPlay1 = PLAYER_ANIMAL) or (aPlay2 = PLAYER_ANIMAL) or (aPlay1 = aPlay2) then
    Exit;

  Result := fPlayerList[aPlay1].Alliances[aPlay2];
end;


//We need to clean pointers through this method because on games exit we free all objects and in
//destructor we must release all pointers. It is common that there are cross-pointers (units in fight f.e.) that cant be cross-freed
procedure TKMPlayersCollection.CleanUpUnitPointer(var aUnit: TKMUnit);
begin
  if (aUnit <> nil) and not fGame.IsExiting then
    aUnit.ReleaseUnitPointer;
  aUnit := nil;
end;


procedure TKMPlayersCollection.CleanUpGroupPointer(var aGroup: TKMUnitGroup);
begin
  if (aGroup <> nil) and (fGame <> nil) and not fGame.IsExiting then
    aGroup.ReleaseGroupPointer;
  aGroup := nil;
end;


procedure TKMPlayersCollection.CleanUpHousePointer(var aHouse: TKMHouse);
begin
  if (aHouse <> nil) and (fGame <> nil) and not fGame.IsExiting then
    aHouse.ReleaseHousePointer;
  aHouse := nil;
end;


//MapEd procedure to remove any house under cursor
procedure TKMPlayersCollection.RemAnyHouse(Position: TKMPoint);
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPlayerList[I].RemHouse(Position, true, true);
end;


//MapEd procedure to remove any unit under cursor
procedure TKMPlayersCollection.RemAnyUnit(Position: TKMPoint);
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPlayerList[I].RemGroup(Position);
  for I := 0 to fCount - 1 do
    fPlayerList[I].RemUnit(Position);
  fPlayerAnimals.RemUnit(Position);
end;


//Reveal portion of terrain for said player and his allies (if they share vision)
//In singleplayer KaM sometimes you should not see your allies till some time
//todo: Add ShareVision: array [0.. MAX_PLAYERS-1] of Boolean alongside Alliances array
procedure TKMPlayersCollection.RevealForTeam(aPlayer: TPlayerIndex; Pos: TKMPoint; Radius, Amount: Word);
var I: Integer;
begin
  fPlayerList[aPlayer].FogOfWar.RevealCircle(Pos,Radius,Amount);

  for I := 0 to fCount - 1 do
  if (I <> aPlayer) and (fPlayerList[aPlayer].Alliances[I] = at_Ally) then
    fPlayerList[I].FogOfWar.RevealCircle(Pos, Radius, Amount);
end;


//Synchronize FOW between players (e.g. when alliances change)
procedure TKMPlayersCollection.SyncFogOfWar;
var I,K: Integer;
begin
  for I := 0 to fCount - 1 do
  for K := 0 to fCount - 1 do
  if (I <> K) and (fPlayerList[I].Alliances[K] = at_Ally) then
    fPlayerList[K].FogOfWar.SyncFOW(fPlayerList[I].FogOfWar);
end;


procedure TKMPlayersCollection.AddDefaultMPGoals(aMissionMode: TKMissionMode);
var
  I,K: Integer;
  Enemies: array of array of TPlayerIndex;
begin
  SetLength(Enemies, fCount);

  for I := 0 to fCount - 1 do
  begin
    SetLength(Enemies[I], 0);
    for K := 0 to fCount - 1 do
    if (I <> K) and (fPlayerList[I].Alliances[K] = at_Enemy) then
    begin
      SetLength(Enemies[I], Length(Enemies[I])+1);
      Enemies[I, Length(Enemies[I])-1] := K;
    end;
    fPlayerList[I].Goals.AddDefaultMPGoals(aMissionMode <> mm_Tactic, I, Enemies[I]);
  end;
end;


// aMultiplayer - savegames should be identical when in MP mode
procedure TKMPlayersCollection.Save(SaveStream: TKMemoryStream; aMultiplayer: Boolean);
var
  I: Integer;
begin
  SaveStream.Write('Players');
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    fPlayerList[I].Save(SaveStream);
  PlayerAnimals.Save(SaveStream);
end;


procedure TKMPlayersCollection.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.ReadAssert('Players');
  LoadStream.Read(fCount);

  if fCount > MAX_PLAYERS then
    fLog.AddAssert('Player count in savegame exceeds MAX_PLAYERS allowed by Remake');

  SetLength(fPlayerList, fCount);

  for I := 0 to fCount - 1 do
  begin
    fPlayerList[I] := TKMPlayer.Create(0);
    fPlayerList[I].Load(LoadStream);
  end;
  PlayerAnimals.Load(LoadStream);
end;


procedure TKMPlayersCollection.SyncLoad;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPlayerList[I].SyncLoad;

  PlayerAnimals.SyncLoad;
end;


procedure TKMPlayersCollection.IncAnimStep;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPlayerList[I].IncAnimStep;
end;


procedure TKMPlayersCollection.UpdateState(aTick: Cardinal);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  if not fGame.IsPaused and not fGame.IsExiting then
    fPlayerList[I].UpdateState(aTick)
  else
    //PlayerAI can stop the game and clear everything
    Exit;

  PlayerAnimals.UpdateState(aTick); //Animals don't have any AI yet
end;


procedure TKMPlayersCollection.Paint;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPlayerList[I].Paint;

  PlayerAnimals.Paint;
end;


end.
