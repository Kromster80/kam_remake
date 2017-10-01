unit KM_HandsCollection;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Units, KM_UnitGroups, KM_Terrain, KM_Houses,
  KM_Hand, KM_HandSpectator, KM_Utils, KM_Points, KM_Units_Warrior, KM_ResHouses;


//Hands are identified by their starting location
type
  TKMHandsCollection = class
  private
    fCount: Byte;
    fHandsList: array of TKMHand;
    fPlayerAnimals: TKMHandAnimals;
    function GetHand(aIndex: Integer): TKMHand; inline;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Byte read fCount;
    property Hands[aIndex: Integer]: TKMHand read GetHand; default;
    property PlayerAnimals: TKMHandAnimals read fPlayerAnimals;

    procedure AddPlayers(aCount: Byte); //Batch add several players

    procedure RemoveEmptyPlayers;
    procedure RemovePlayer(aIndex: TKMHandIndex);
    procedure AfterMissionInit(aFlattenRoads: Boolean);
    function HousesHitTest(X,Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function GroupsHitTest(X, Y: Integer): TKMUnitGroup;
    function GetClosestUnit(aLoc: TKMPoint; aIndex: TKMHandIndex; aAlliance: TAllianceType): TKMUnit;
    function GetClosestHouse(aLoc: TKMPoint; aIndex: TKMHandIndex; aAlliance: TAllianceType; aOnlyCompleted: Boolean = True): TKMHouse;
    function DistanceToEnemyTowers(aLoc: TKMPoint; aIndex: TKMHandIndex): Single;
    procedure GetUnitsInRect(aRect: TKMRect; List: TList);
    function GetHouseByUID(aUID: Integer): TKMHouse;
    function GetUnitByUID(aUID: Integer): TKMUnit;
    function GetGroupByUID(aUID: Integer): TKMUnitGroup;
    function GetObjectByUID(aUID: Integer): TObject;
    function GetNextHouseWSameType(aHouse: TKMHouse): TKMHouse;
    function GetNextUnitWSameType(aUnit: TKMUnit): TKMUnit;
    function GetNextGroupWSameType(aUnitGroup: TKMUnitGroup): TKMUnitGroup;
    function GetGroupByMember(aWarrior: TKMUnitWarrior): TKMUnitGroup;
    function HitTest(X,Y: Integer): TObject;
    function UnitCount: Integer;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType: TUnitType; out PlacePoint: TKMPoint; RequiredWalkConnect:byte):Boolean;

    //Check how Player1 feels towards Player2
    //Note: this is position dependant, e.g. Player1 may be allied with
    //      Player2, but Player2 could be an enemy to Player1
    function CheckAlliance(aPlay1, aPlay2: TKMHandIndex): TAllianceType;
    function GetTeams: TKMByteSetArray;
    procedure CleanUpUnitPointer(var aUnit: TKMUnit);
    procedure CleanUpGroupPointer(var aGroup: TKMUnitGroup);
    procedure CleanUpHousePointer(var aHouse: TKMHouse);
    procedure RemAnyHouse(Position: TKMPoint);
    procedure RemAnyUnit(Position: TKMPoint);
    procedure RevealForTeam(aPlayer: TKMHandIndex; Pos: TKMPoint; Radius,Amount:word);
    procedure SyncFogOfWar;
    procedure AddDefaultGoalsToAll(aMissionMode: TKMissionMode);
    procedure DisableGoalsForDefeatedHand(aHandIndex: TKMHandIndex);

    procedure Save(SaveStream: TKMemoryStream; aMultiplayer: Boolean);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;

    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;

var
  gHands: TKMHandsCollection;
  gMySpectator: TKMSpectator; //Wrap to access player/fow separately


implementation
uses
  KM_Game, KM_Log, KM_Resource, KM_AIFields, KM_ResUnits, KM_HouseCollection, KM_UnitsCollection;


{ TKMHandsCollection }
constructor TKMHandsCollection.Create;
begin
  inherited Create;

  fPlayerAnimals := TKMHandAnimals.Create(PLAYER_ANIMAL); //Always create Animals
end;


destructor TKMHandsCollection.Destroy;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    FreeThenNil(fHandsList[I]);

  PlayerAnimals.Free;

  inherited;
end;


function TKMHandsCollection.GetHand(aIndex: Integer): TKMHand;
begin
  //We have Range Checks enabled so such an error will be caught and reported already
  //Assert(InRange(aIndex, 0, fCount-1));
  Result := fHandsList[aIndex];
end;


procedure TKMHandsCollection.AddPlayers(aCount: Byte);
var
  I, K: Integer;
begin
  Assert(fCount + aCount <= MAX_HANDS);

  SetLength(fHandsList, fCount + aCount);

  for I := fCount to fCount + aCount - 1 do
    fHandsList[I] := TKMHand.Create(I);

  //Default alliance settings for new players:
  //Existing players treat any new players as enemies
  for I := 0 to fCount - 1 do
    for K := fCount to fCount + aCount - 1 do
      fHandsList[I].Alliances[K] := at_Enemy;

  //New players treat all players as enemies except self
  for I := fCount to fCount + aCount - 1 do
  begin
    for K := 0 to MAX_HANDS - 1 do
      fHandsList[I].Alliances[K] := at_Enemy;
    fHandsList[I].Alliances[I] := at_Ally;
  end;

  fCount := fCount + aCount;
end;


procedure TKMHandsCollection.AfterMissionInit(aFlattenRoads: Boolean);
var
  I: Integer;
begin
  gAIFields.AfterMissionInit;

  for I := 0 to fCount - 1 do
    fHandsList[I].AfterMissionInit(aFlattenRoads);
end;


//We assume that if player has no assets it is unused and can be removed from end
//Don't remove players in the middle, if user uses players 1 and 8 then 2..7 are kept
//Accessed only by MapEditor when it needs to remove empty players before saving a map
procedure TKMHandsCollection.RemoveEmptyPlayers;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if fHandsList[I].HasAssets then
      Exit //Exit as soon as we find a player with assets
    else
      RemovePlayer(I);
end;


//Remove player 'aIndex'
//Accessed only by MapEditor when it needs to remove empty players before saving a map
procedure TKMHandsCollection.RemovePlayer(aIndex: TKMHandIndex);
var
  I, K: Integer;
begin
  //Remove other players goals using this player
  for I := 0 to fCount - 1 do
    fHandsList[I].AI.Goals.RemoveReference(aIndex);

  FreeThenNil(fHandsList[aIndex]);

  for I := aIndex to fCount - 2 do
  begin
    fHandsList[I] := fHandsList[I + 1];
    fHandsList[I].SeTKMHandIndex(I);
  end;

  Dec(fCount);
  SetLength(fHandsList, fCount);

  for I := 0 to fCount - 1 do
    for K := aIndex to fCount - 1 do
      fHandsList[I].Alliances[K] := fHandsList[I].Alliances[K + 1];

  gTerrain.RemovePlayer(aIndex);
end;


function TKMHandsCollection.HousesHitTest(X, Y: Integer): TKMHouse;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].HousesHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 houses on one tile
  end;
end;


function TKMHandsCollection.UnitsHitTest(X, Y: Integer): TKMUnit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].UnitsHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 units on one tile
  end;
end;


function TKMHandsCollection.GroupsHitTest(X, Y: Integer): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].GroupsHitTest(X, Y);
    if Result <> nil then
      Exit; //There can't be 2 groups on one tile
  end;
end;


//Check opponents for closest Unit with given Alliance setting
function TKMHandsCollection.GetClosestUnit(aLoc: TKMPoint; aIndex: TKMHandIndex; aAlliance: TAllianceType): TKMUnit;
var
  I: Integer;
  U: TKMUnit;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
  if (I <> aIndex) and (fHandsList[aIndex].Alliances[I] = aAlliance) then
  begin
    U := fHandsList[I].Units.GetClosestUnit(aLoc);
    if (U <> nil)
    and ((Result = nil) or (KMLengthSqr(U.PositionF, KMPointF(aLoc)) < KMLengthSqr(Result.PositionF, KMPointF(aLoc)))) then
      Result := U;
  end;
end;


//Check opponents for closest House with given Alliance setting
//Note: we check by house cells, not by entrance
function TKMHandsCollection.GetClosestHouse(aLoc: TKMPoint; aIndex: TKMHandIndex; aAlliance: TAllianceType; aOnlyCompleted: Boolean = True): TKMHouse;
var
  I: Integer;
  H: TKMHouse;
begin
  Result := nil;

  //Check all players
  for I := 0 to fCount - 1 do
  if (aIndex <> I) and (Hands[aIndex].Alliances[I] = aAlliance) then
  begin
    H := fHandsList[I].Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, aOnlyCompleted);
    if (H <> nil) and ((Result = nil) or (H.GetDistance(aLoc) < Result.GetDistance(aLoc))) then
      Result := H;
  end;
end;


//Return distance from the tile to the closest enemy tower
function TKMHandsCollection.DistanceToEnemyTowers(aLoc: TKMPoint; aIndex: TKMHandIndex): Single;
var
  I, K: Integer;
  H: TKMHouse;
begin
  Result := MaxSingle;
  for I := 0 to fCount - 1 do
  if Hands[aIndex].Alliances[I] = at_Enemy then
  begin
    for K := fHandsList[I].Houses.Count - 1 downto 0 do
    begin
      H := fHandsList[I].Houses[K];
      if (H is TKMHouseTower) and H.IsComplete
      and not H.IsDestroyed and H.GetHasOwner
      and (H.CurrentAction.State <> hst_Empty) then
        //Don't use H.GetDistance (dist to any tile within house) as that's not how tower range works
        Result := Min(Result, KMLength(H.GetPosition, aLoc));
    end;
  end;
end;


function TKMHandsCollection.GetHouseByUID(aUID: Integer): TKMHouse;
var
  I: Integer;
begin
  Result := nil;
  if aUID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].Houses.GetHouseByUID(aUID);
    if Result <> nil then Exit; //else keep on testing
  end;
end;


function TKMHandsCollection.GetUnitByUID(aUID: Integer): TKMUnit;
var
  I: Integer;
begin
  Result := nil;
  if aUID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].Units.GetUnitByUID(aUID);

    if Result <> nil then
      Break;
  end;

  if Result = nil then
    Result := PlayerAnimals.Units.GetUnitByUID(aUID);
end;


function TKMHandsCollection.GetGroupByUID(aUID: Integer): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;
  if aUID = 0 then Exit;

  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].UnitGroups.GetGroupByUID(aUID);
    if Result <> nil then Exit; //else keep on testing
  end;
end;


function TKMHandsCollection.GetObjectByUID(aUID: Integer): TObject;
begin
  Result := GetHouseByUID(aUID);
  if Result = nil then
  begin
    Result := GetUnitByUID(aUID);
    if Result = nil then
      Result := GetGroupByUID(aUID);
  end;
end;


{
Get next house in house list with the same type for the same owner
Result
    house: next house in unit list
    nil: if NO other house found
}
function TKMHandsCollection.GetNextHouseWSameType(aHouse: TKMHouse): TKMHouse;
var Houses: TKMHousesCollection;
    House, FirstH: TKMHouse;
    Found: Boolean;
    I: Integer;
begin
  Result := nil;
  if (aHouse = nil) or aHouse.IsDestroyed then Exit;

  Found := False;
  FirstH := nil;

  Houses := fHandsList[aHouse.Owner].Houses;

  for I := 0 to Houses.Count - 1 do
  begin
    House := Houses[I];
    if (House.HouseType = aHouse.HouseType) // we are interested in houses with the same type
      and not House.IsDestroyed then        // not destroyed
    begin
      if House = aHouse then
        Found := True               // Mark that we found our house
      else if Found then
      begin
        Result := House;            // Save the next house after Found to Result and Break
        Break;
      end else if FirstH = nil then
        FirstH := House;            // Save 1st house in list in case our house is the last one
    end;
  end;
  if (Result = nil) and Found then // Found should be always True here
    Result := FirstH;
end;


{
Get next unit in unit list with the same type for the same owner
Result
    unit: next unit in unit list
    nil: if NO other unit found
}
function TKMHandsCollection.GetNextUnitWSameType(aUnit: TKMUnit): TKMUnit;
var Units: TKMUnitsCollection;
    U, FirstU: TKMUnit;
    Found: Boolean;
    I: Integer;
begin
  Result := nil;
  if (aUnit = nil) or aUnit.IsDeadOrDying then Exit;

  Found := False;
  FirstU := nil;

  Units := fHandsList[aUnit.Owner].Units;

  for I := 0 to Units.Count - 1 do
  begin
    U := Units[I];
    if (U.UnitType = aUnit.UnitType) // we are interested in units with the same type only
      and not U.IsDeadOrDying        // not dead or dying
      and U.Visible then             // visible
    begin
      if U = aUnit then
        Found := True                // Mark that we found our unit
      else if Found then
      begin
        Result := U;                 // Save the next unit after Found to Result and Break
        Break;
      end else if FirstU = nil then
        FirstU := U;                 // Save 1st unit in list in case our unit is the last one
    end;
  end;
  if (Result = nil) and Found then   // Found should be always True here
    Result := FirstU;
end;


{
Get next unit group in group list with the same type for the same owner
Result
    unit group: next unit group in group list
    nil: if NO other group found
}
function TKMHandsCollection.GetNextGroupWSameType(aUnitGroup: TKMUnitGroup): TKMUnitGroup;
var UnitGroups: TKMUnitGroups;
    Group, FirstG: TKMUnitGroup;
    Found: Boolean;
    I: Integer;
begin
  Result := nil;
  if (aUnitGroup = nil) or aUnitGroup.IsDead then Exit;

  Found := False;
  FirstG := nil;

  UnitGroups := fHandsList[aUnitGroup.Owner].UnitGroups;

  for I := 0 to UnitGroups.Count - 1 do
  begin
    Group := UnitGroups[I];
    if (Group.UnitType = aUnitGroup.UnitType) // we are interested in groups with the same type only
      and not Group.IsDead then               // not dead
    begin
      if Group = aUnitGroup then
        Found := True               // Mark that we found our group
      else if Found then
      begin
        Result := Group;            // Save the next group after Found to Result and Break
        Break;
      end else if FirstG = nil then
        FirstG := Group;            // Save 1st group in list in case our group is the last one
    end;
  end;
  if (Result = nil) and Found then // Found should be always True here
    Result := FirstG;
end;


function TKMHandsCollection.GetGroupByMember(aWarrior: TKMUnitWarrior): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
  begin
    Result := fHandsList[I].UnitGroups.GetGroupByMember(aWarrior);
    if Result <> nil then Exit; //else keep on testing
  end;
end;


function TKMHandsCollection.HitTest(X,Y: Integer): TObject;
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
function TKMHandsCollection.UnitCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
    Inc(Result, fHandsList[I].Units.Count);
end;


procedure TKMHandsCollection.GetUnitsInRect(aRect: TKMRect; List: TList);
var I: Integer;
begin
  Assert(List.Count = 0);

  for I := 0 to fCount - 1 do
    fHandsList[I].Units.GetUnitsInRect(aRect, List);
end;


{Should return closest position where unit can be placed}
function TKMHandsCollection.FindPlaceForUnit(PosX,PosY:integer; aUnitType: TUnitType; out PlacePoint: TKMPoint; RequiredWalkConnect:byte):Boolean;
var
  I: Integer;
  P: TKMPoint;
  Pass: TKMTerrainPassability; //temp for required passability
begin
  Result := False; // if function fails to find valid position
  Pass := gRes.Units[aUnitType].AllowedPassability;

  for I := 0 to 255 do
  begin
    P := GetPositionFromIndex(KMPoint(PosX, PosY), I);
    if gTerrain.TileInMapCoords(P.X, P.Y) then
    begin
      if gTerrain.CheckPassability(P, Pass) and not gTerrain.HasUnit(P)
      //If RequiredWalkConnect is invalid (0) it means we don't care
      and ((RequiredWalkConnect = 0) or (gTerrain.GetWalkConnectID(P) = RequiredWalkConnect)) then
      begin
        PlacePoint := P; // Assign if all test are passed
        Result := True;
        Exit;
      end;
    end;
  end;
end;


{ Check how Player1 feels towards Player2. Note: this is position dependant,
e.g. Play1 may be allied with Play2, but Play2 may be enemy to Play1}
function TKMHandsCollection.CheckAlliance(aPlay1,aPlay2: TKMHandIndex): TAllianceType;
begin
  if (aPlay1 = PLAYER_ANIMAL) or (aPlay2 = PLAYER_ANIMAL) then
    Result := at_Ally //In KaM animals are always friendly
  else
    Result := fHandsList[aPlay1].Alliances[aPlay2];
end;


//Get teams from alliances information
//We consider team as a group of hands, where all hands are symmetrically allied to each other and do not allied to any other hand outside of that group
//Basically that mean standart team in MP game.
//All other possible options, f.e. smth like 1-2 are allied to each other, 3-4 - are also allied, but 5 is allied to 1-2-3-4 we do not consider as team
//other example - 1-2-3 ally/4-5-6 ally/1-7 ally - we have one standart team here: 4-5-6. 1 is allied to 7, but 2 is not, so non of them can be considered as a 'team'
function TKMHandsCollection.GetTeams: TKMByteSetArray;
var
  Allies: TKMByteSetArray;
  I, J, K: Byte;
  HandsChecked: set of Byte;
  CollisionFound: Boolean;
begin
  SetLength(Allies, Count);
  SetLength(Result, Count);

  //Gather aliance info into 'Allies' variable
  for I := 0 to Count - 1 do
  begin
    Allies[I] := [I]; // every hand is Ally to himself by default
    for J := 0 to Count - 1 do
    begin
      if (I <> J) and (CheckAlliance(I,J) = at_Ally) then
        Include(Allies[I], J);
    end;
  end;

  K := 0;
  HandsChecked := [];
  for I := 0 to Count - 1 do
  begin
    CollisionFound := False;
    if (Allies[I] = [I])          //hand has no allies, so we can ignore it
      or (I in HandsChecked) then //hand was checked in other iteration before, ignore it
      Continue;
    //Loop throught hand allies and check if all of them has same ally group
    for J in Allies[I] do
    begin
      if I = J then
        Continue;
      //Check if I-hand and all its allias has absolutely same allies
      //If not - that means all I-Hand allies and J-hand allies can not be in any of teams
      //(f.e. 1-hand allied with 2 and 3, when 2 allied with 1,3 and 4, means all 1234 can not be in any of team (or what we called by standart 'team'))
      if Allies[I] <> Allies[J] then
      begin
        HandsChecked := HandsChecked + Allies[I];
        HandsChecked := HandsChecked + Allies[J];
        CollisionFound := True;
      end;
    end;
    //If no team collisions were found, means that is correct team and we have to save it.
    if not CollisionFound then
    begin
      Result[K] := Allies[I];
      HandsChecked := HandsChecked + Allies[I];
      Inc(K);
    end;
  end;
  SetLength(Result, K);
end;


//We need to clean pointers through this method because on games exit we free all objects and in
//destructor we must release all pointers. It is common that there are cross-pointers (units in fight f.e.) that cant be cross-freed
procedure TKMHandsCollection.CleanUpUnitPointer(var aUnit: TKMUnit);
begin
  if (aUnit <> nil) and not gGame.IsExiting then
    aUnit.ReleaseUnitPointer;
  aUnit := nil;
end;


procedure TKMHandsCollection.CleanUpGroupPointer(var aGroup: TKMUnitGroup);
begin
  if (aGroup <> nil) and (gGame <> nil) and not gGame.IsExiting then
    aGroup.ReleaseGroupPointer;
  aGroup := nil;
end;


procedure TKMHandsCollection.CleanUpHousePointer(var aHouse: TKMHouse);
begin
  if (aHouse <> nil) and (gGame <> nil) and not gGame.IsExiting then
    aHouse.ReleaseHousePointer;
  aHouse := nil;
end;


//MapEd procedure to remove any house under cursor
procedure TKMHandsCollection.RemAnyHouse(Position: TKMPoint);
var
  H: TKMHouse;
begin
  H := HousesHitTest(Position.X, Position.Y);
  if H <> nil then
  begin
    H.DemolishHouse(H.Owner, True);
    fHandsList[H.Owner].Houses.DeleteHouseFromList(H);
  end;
end;


//MapEd procedure to remove any unit under cursor
procedure TKMHandsCollection.RemAnyUnit(Position: TKMPoint);
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fHandsList[I].RemGroup(Position);
  for I := 0 to fCount - 1 do
    fHandsList[I].RemUnit(Position);
  fPlayerAnimals.RemUnit(Position);
end;


//Reveal portion of terrain for said player and his allies (if they share vision)
//In singleplayer KaM sometimes you should not see your allies till some time
procedure TKMHandsCollection.RevealForTeam(aPlayer: TKMHandIndex; Pos: TKMPoint; Radius, Amount: Word);
var
  I: Integer;
begin
  fHandsList[aPlayer].FogOfWar.RevealCircle(Pos,Radius,Amount);

  for I := 0 to fCount - 1 do
  if (I <> aPlayer) and (fHandsList[aPlayer].Alliances[I] = at_Ally) and fHandsList[aPlayer].ShareFOW[I] then
    fHandsList[I].FogOfWar.RevealCircle(Pos, Radius, Amount);
end;


//Synchronize FOW between players (e.g. when alliances change)
procedure TKMHandsCollection.SyncFogOfWar;
var
  I, K: Integer;
begin
  for I := 0 to fCount - 1 do
  for K := 0 to fCount - 1 do
  if (I <> K) and (fHandsList[I].Alliances[K] = at_Ally) and fHandsList[I].ShareFOW[K] then
    fHandsList[K].FogOfWar.SyncFOW(fHandsList[I].FogOfWar);
end;


procedure TKMHandsCollection.AddDefaultGoalsToAll(aMissionMode: TKMissionMode);
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fHandsList[I].AI.AddDefaultGoals(aMissionMode <> mm_Tactic);
end;


procedure TKMHandsCollection.DisableGoalsForDefeatedHand(aHandIndex: TKMHandIndex);
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    if I <> aHandIndex then
      fHandsList[I].AI.Goals.DisableGoalsForHand(aHandIndex);
end;


// aMultiplayer - savegames should be identical when in MP mode
procedure TKMHandsCollection.Save(SaveStream: TKMemoryStream; aMultiplayer: Boolean);
var
  I: Integer;
begin
  SaveStream.WriteA('Players');
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    fHandsList[I].Save(SaveStream);
  PlayerAnimals.Save(SaveStream);
end;


procedure TKMHandsCollection.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.ReadAssert('Players');
  LoadStream.Read(fCount);

  if fCount > MAX_HANDS then
    gLog.AddAssert('Player count in savegame exceeds MAX_PLAYERS allowed by Remake');

  SetLength(fHandsList, fCount);

  for I := 0 to fCount - 1 do
  begin
    fHandsList[I] := TKMHand.Create(0);
    fHandsList[I].Load(LoadStream);
  end;
  PlayerAnimals.Load(LoadStream);
end;


procedure TKMHandsCollection.SyncLoad;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fHandsList[I].SyncLoad;

  PlayerAnimals.SyncLoad;
end;


procedure TKMHandsCollection.IncAnimStep;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fHandsList[I].IncAnimStep;
end;


procedure TKMHandsCollection.UpdateState(aTick: Cardinal);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  if not gGame.IsPaused and not gGame.IsExiting then
    fHandsList[I].UpdateState(aTick)
  else
    //PlayerAI can stop the game and clear everything
    Exit;

  PlayerAnimals.UpdateState(aTick); //Animals don't have any AI yet
end;


procedure TKMHandsCollection.Paint(aRect: TKMRect);
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fHandsList[I].Paint(aRect);

  PlayerAnimals.Paint(aRect);
end;


end.
