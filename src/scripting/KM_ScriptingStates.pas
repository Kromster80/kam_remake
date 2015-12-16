unit KM_ScriptingStates;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils, uPSRuntime,
  KM_CommonTypes, KM_Defaults, KM_Points, KM_Houses, KM_ScriptingIdCache, KM_Units,
  KM_UnitGroups, KM_ResHouses, KM_HouseCollection, KM_ResWares, KM_ScriptingEvents;


type
  TKMScriptStates = class(TKMScriptEntity)
  public
    function ClosestGroup(aPlayer, X, Y, aGroupType: Integer): Integer;
    function ClosestGroupMultipleTypes(aPlayer, X, Y: Integer; aGroupTypes: TByteSet): Integer;
    function ClosestHouse(aPlayer, X, Y, aHouseType: Integer): Integer;
    function ClosestHouseMultipleTypes(aPlayer, X, Y: Integer; aHouseTypes: TByteSet): Integer;
    function ClosestUnit(aPlayer, X, Y, aUnitType: Integer): Integer;
    function ClosestUnitMultipleTypes(aPlayer, X, Y: Integer; aUnitTypes: TByteSet): Integer;

    function ConnectedByRoad(X1, Y1, X2, Y2: Integer): Boolean;
    function ConnectedByWalking(X1, Y1, X2, Y2: Integer): Boolean;

    function FogRevealed(aPlayer: Byte; aX, aY: Word): Boolean;

    function GameTime: Cardinal;

    function GroupAt(aX, aY: Word): Integer;
    function GroupColumnCount(aGroupID: Integer): Integer;
    function GroupDead(aGroupID: Integer): Boolean;
    function GroupIdle(aGroupID: Integer): Boolean;
    function GroupMember(aGroupID, aMemberIndex: Integer): Integer;
    function GroupMemberCount(aGroupID: Integer): Integer;
    function GroupOwner(aGroupID: Integer): Integer;
    function GroupType(aGroupID: Integer): Integer;

    function HouseAt(aX, aY: Word): Integer;
    function HouseBarracksRallyPointX(aBarracks: Integer): Integer;
    function HouseBarracksRallyPointY(aBarracks: Integer): Integer;
    function HouseBuildingProgress(aHouseID: Integer): Word;
    function HouseCanReachResources(aHouseID: Integer): Boolean;
    function HouseDamage(aHouseID: Integer): Integer;
    function HouseDeliveryBlocked(aHouseID: Integer): Boolean;
    function HouseDestroyed(aHouseID: Integer): Boolean;
    function HouseHasOccupant(aHouseID: Integer): Boolean;
    function HouseIsComplete(aHouseID: Integer): Boolean;
    function HouseTypeMaxHealth(aHouseType: Integer): Word;
    function HouseTypeToOccupantType(aHouseType: Integer): Integer;
    function HouseOwner(aHouseID: Integer): Integer;
    function HousePositionX(aHouseID: Integer): Integer;
    function HousePositionY(aHouseID: Integer): Integer;
    function HouseRepair(aHouseID: Integer): Boolean;
    function HouseResourceAmount(aHouseID, aResource: Integer): Integer;
    function HouseSchoolQueue(aHouseID, QueueIndex: Integer): Integer;
    function HouseSiteIsDigged(aHouseID: Integer): Boolean;
    function HouseType(aHouseID: Integer): Integer;
    function HouseTypeName(aHouseType: Byte): AnsiString;
    function HouseUnlocked(aPlayer, aHouseType: Word): Boolean;
    function HouseWareBlocked(aHouseID, aWareType: Integer): Boolean;
    function HouseWeaponsOrdered(aHouseID, aWareType: Integer): Integer;
    function HouseWoodcutterChopOnly(aHouseID: Integer): Boolean;

    function IsFieldAt(aPlayer: ShortInt; X, Y: Word): Boolean;
    function IsWinefieldAt(aPlayer: ShortInt; X, Y: Word): Boolean;
    function IsRoadAt(aPlayer: ShortInt; X, Y: Word): Boolean;

    function KaMRandom: Single;
    function KaMRandomI(aMax: Integer): Integer;
    function LocationCount: Integer;

    function MapTileType(X, Y: Integer): Integer;
    function MapTileRotation(X, Y: Integer): Integer;
    function MapTileHeight(X, Y: Integer): Integer;
    function MapTileObject(X, Y: Integer): Integer;
    function MapWidth: Integer;
    function MapHeight: Integer;

    function MarketFromWare(aMarketID: Integer): Integer;
    function MarketLossFactor: Single;
    function MarketOrderAmount(aMarketID: Integer): Integer;
    function MarketToWare(aMarketID: Integer): Integer;
    function MarketValue(aRes: Integer): Single;
    function PeaceTime: Cardinal;

    function PlayerAllianceCheck(aPlayer1, aPlayer2: Byte): Boolean;
    function PlayerColorText(aPlayer: Byte): AnsiString;
    function PlayerDefeated(aPlayer: Byte): Boolean;
    function PlayerEnabled(aPlayer: Byte): Boolean;
    function PlayerGetAllUnits(aPlayer: Byte): TIntegerArray;
    function PlayerGetAllHouses(aPlayer: Byte): TIntegerArray;
    function PlayerGetAllGroups(aPlayer: Byte): TIntegerArray;
    function PlayerIsAI(aPlayer: Byte): Boolean;
    function PlayerName(aPlayer: Byte): AnsiString;
    function PlayerVictorious(aPlayer: Byte): Boolean;
    function PlayerWareDistribution(aPlayer, aWareType, aHouseType: Byte): Byte;

    function StatAIDefencePositionsCount(aPlayer: Byte): Integer;
    function StatArmyCount(aPlayer: Byte): Integer;
    function StatCitizenCount(aPlayer: Byte): Integer;
    function StatHouseMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
    function StatHouseTypeCount(aPlayer, aHouseType: Byte): Integer;
    function StatHouseTypePlansCount(aPlayer, aHouseType: Byte): Integer;
    function StatPlayerCount: Integer;
    function StatResourceProducedCount(aPlayer, aResType: Byte): Integer;
    function StatResourceProducedMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
    function StatUnitCount(aPlayer: Byte): Integer;
    function StatUnitKilledCount(aPlayer, aUnitType: Byte): Integer;
    function StatUnitKilledMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
    function StatUnitLostCount(aPlayer, aUnitType: Byte): Integer;
    function StatUnitLostMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
    function StatUnitMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
    function StatUnitTypeCount(aPlayer, aUnitType: Byte): Integer;

    function UnitAt(aX, aY: Word): Integer;
    function UnitCarrying(aUnitID: Integer): Integer;
    function UnitDead(aUnitID: Integer): Boolean;
    function UnitDirection(aUnitID: Integer): Integer;
    function UnitHome(aUnitID: Integer): Integer;
    function UnitHunger(aUnitID: Integer): Integer;
    function UnitIdle(aUnitID: Integer): Boolean;
    function UnitLowHunger: Integer;
    function UnitMaxHunger: Integer;
    function UnitOwner(aUnitID: Integer): Integer;
    function UnitPositionX(aUnitID: Integer): Integer;
    function UnitPositionY(aUnitID: Integer): Integer;
    function UnitsGroup(aUnitID: Integer): Integer;
    function UnitType(aUnitID: Integer): Integer;
    function UnitTypeName(aUnitType: Byte): AnsiString;
    function WareTypeName(aWareType: Byte): AnsiString;
  end;


implementation
uses
  KM_AI, KM_Terrain, KM_Game, KM_FogOfWar, KM_HandsCollection, KM_Units_Warrior,
  KM_HouseBarracks, KM_HouseSchool, KM_ResUnits, KM_Log, KM_Utils, KM_HouseMarket,
  KM_Resource, KM_UnitTaskSelfTrain, KM_Sound, KM_Hand, KM_AIDefensePos, KM_CommonClasses,
  KM_UnitsCollection, KM_PathFindingRoad;


  //We need to check all input parameters as could be wildly off range due to
  //mistakes in scripts. In that case we have two options:
  // - skip silently and log
  // - report to player


function HouseTypeValid(aHouseType: Integer): Boolean; inline;
begin
  Result := (aHouseType in [Low(HouseIndexToType)..High(HouseIndexToType)])
            and (HouseIndexToType[aHouseType] <> ht_None); //KaM index 26 is unused (ht_None)
end;


{ TKMScriptStates }
//* Version: 6216
//* Returns the group of the specified player and group type that is closest to the specified coordinates,
//* r -1 if no such group was found.
//* If the group type is -1 any group type will be accepted
//* aPlayer: Player ID
//* X: X coordinate
//* Y: Y coordinate
//* aGroupType: Group type
//* Return: Group ID
function TKMScriptStates.ClosestGroup(aPlayer, X, Y, aGroupType: Integer): Integer;
var
  GTS: TGroupTypeSet;
  G: TKMUnitGroup;
begin
  try
    Result := -1;
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y)
    and ((aGroupType = -1) or (aGroupType in [Byte(Low(TGroupType))..Byte(High(TGroupType))])) then
    begin
      if aGroupType = -1 then
        GTS := [Low(TGroupType)..High(TGroupType)]
      else
        GTS := [TGroupType(aGroupType)];

      G := gHands[aPlayer].UnitGroups.GetClosestGroup(KMPoint(X,Y), GTS);
      if (G <> nil) and not G.IsDead then
      begin
        Result := G.UID;
        fIDCache.CacheGroup(G, G.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestGroup', [aPlayer, X, Y, aGroupType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6216
//* Returns the group of the specified player and group types that is closest to the specified coordinates,
//* or -1 if no such group was found.
//* The group types is a "set of Byte", for example [1,3]
//* aPlayer: Player ID
//* aGroupTypes: Multiple group types
//* Return: Group ID
function TKMScriptStates.ClosestGroupMultipleTypes(aPlayer, X, Y: Integer; aGroupTypes: TByteSet): Integer;
var
  B: Byte;
  GTS: TGroupTypeSet;
  G: TKMUnitGroup;
begin
  try
    Result := -1;
    GTS := [];
    for B in [Byte(Low(TGroupType))..Byte(High(TGroupType))] do
      if B in aGroupTypes then
        GTS := GTS + [TGroupType(B)];

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
    begin
      G := gHands[aPlayer].UnitGroups.GetClosestGroup(KMPoint(X,Y), GTS);
      if G <> nil then
      begin
        Result := G.UID;
        fIDCache.CacheGroup(G, G.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestGroupMultipleTypes', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6216
//* Returns the house of the specified player and house type that is closest to the specified coordinates,
//* or -1 if no such house was found.
//* If the house type is -1 any house type will be accepted
//* aPlayer: Player ID
//* aHouseType: House type
//* Return: House ID
function TKMScriptStates.ClosestHouse(aPlayer, X, Y, aHouseType: Integer): Integer;
var
  HTS: THouseTypeSet;
  H: TKMHouse;
begin
  try
    Result := -1;
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y)
    and ((aHouseType = -1) or HouseTypeValid(aHouseType)) then
    begin
      if aHouseType = -1 then
        HTS := [Low(THouseType)..High(THouseType)]
      else
        HTS := [HouseIndexToType[aHouseType]];

      H := gHands[aPlayer].Houses.FindHouse(HTS, X, Y);
      if H <> nil then
      begin
        Result := H.UID;
        fIDCache.CacheHouse(H, H.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestHouse', [aPlayer, X, Y, aHouseType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6216
//* Returns the house of the specified player and house types that is closest to the specified coordinates,
//* or -1 if no such house was found.
//* The house types is a "set of Byte", for example [11,13,21]
//* aPlayer: Player ID
//* aHouseTypes: Multiple house types
//* Return: House ID
function TKMScriptStates.ClosestHouseMultipleTypes(aPlayer, X, Y: Integer; aHouseTypes: TByteSet): Integer;
var
  B: Byte;
  HTS: THouseTypeSet;
  H: TKMHouse;
begin
  try
    Result := -1;
    HTS := [];
    for B := Low(HouseIndexToType) to High(HouseIndexToType) do
      if (B in aHouseTypes) and (HouseIndexToType[B] <> ht_None) then
        HTS := HTS + [HouseIndexToType[B]];

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
    begin
      H := gHands[aPlayer].Houses.FindHouse(HTS, X, Y);
      if H <> nil then
      begin
        Result := H.UID;
        fIDCache.CacheHouse(H, H.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestHouseMultipleTypes', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6216
//* Returns the unit of the specified player and unit type that is closest to the specified coordinates,
//* or -1 if no such unit was found.
//* If the unit type is -1 any unit type will be accepted
//* aPlayer: Player ID
//* aUnitType: Unit type
//* Return: Unit ID
function TKMScriptStates.ClosestUnit(aPlayer, X, Y, aUnitType: Integer): Integer;
var
  UTS: TUnitTypeSet;
  U: TKMUnit;
begin
  try
    Result := -1;
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y)
    and ((aUnitType = -1) or (aUnitType in [Low(UnitIndexToType)..High(UnitIndexToType)]))  then
    begin
      if aUnitType = -1 then
        UTS := [Low(TUnitType)..High(TUnitType)]
      else
        UTS := [UnitIndexToType[aUnitType]];

      U := gHands[aPlayer].Units.GetClosestUnit(KMPoint(X,Y), UTS);
      if U <> nil then
      begin
        Result := U.UID;
        fIDCache.CacheUnit(U, U.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestUnit', [aPlayer, X, Y, aUnitType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6216
//* Returns the unit of the specified player and unit types that is closest to the specified coordinates,
//* or -1 if no such unit was found.
//* The unit types is a "set of Byte", for example [0,9]
//* aPlayer: Player ID
//* aUnitTypes: Multiple unit types
//* Return: Unit ID
function TKMScriptStates.ClosestUnitMultipleTypes(aPlayer, X, Y: Integer; aUnitTypes: TByteSet): Integer;
var
  B: Byte;
  UTS: TUnitTypeSet;
  U: TKMUnit;
begin
  try
    Result := -1;
    UTS := [];
    for B in [Low(UnitIndexToType)..High(UnitIndexToType)] do
      if B in aUnitTypes then
        UTS := UTS + [UnitIndexToType[B]];

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
    begin
      U := gHands[aPlayer].Units.GetClosestUnit(KMPoint(X,Y), UTS);
      if U <> nil then
      begin
        Result := U.UID;
        fIDCache.CacheUnit(U, U.UID);
      end;
    end
    else
      LogParamWarning('States.ClosestUnit', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6602
//* Check if two tiles are connected by walkable road
//* X1: X start coordinate
//* Y1: Y start coordinate
//* X2: X end coordinate
//* Y2: Y end coordinate
//* Return: Connected
function TKMScriptStates.ConnectedByRoad(X1, Y1, X2, Y2: Integer): Boolean;
begin
  try
    if gTerrain.TileInMapCoords(X1,Y1) and gTerrain.TileInMapCoords(X2,Y2) then
      Result := (gTerrain.GetRoadConnectID(KMPoint(X1, Y1)) <> 0) and
                (gTerrain.GetRoadConnectID(KMPoint(X1, Y1)) = gTerrain.GetRoadConnectID(KMPoint(X2, Y2)))
    else
    begin
      Result := False;
      LogParamWarning('States.ConnectedByRoad', [X1, Y1, X2, Y2]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 6602
//* Check if two tiles are connected by a walkable route
//* X1: X start coordinate
//* Y1: Y start coordinate
//* X2: X end coordinate
//* Y2: Y end coordinate
//* Return: Walkable
function TKMScriptStates.ConnectedByWalking(X1, Y1, X2, Y2: Integer): Boolean;
begin
  try
    if gTerrain.TileInMapCoords(X1,Y1) and gTerrain.TileInMapCoords(X2,Y2) then
      Result := (gTerrain.GetWalkConnectID(KMPoint(X1, Y1)) <> 0) and
                (gTerrain.GetWalkConnectID(KMPoint(X1, Y1)) = gTerrain.GetWalkConnectID(KMPoint(X2, Y2)))
    else
    begin
      Result := False;
      LogParamWarning('States.ConnectedByWalking', [X1, Y1, X2, Y2]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatAIDefencePositionsCount(aPlayer: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].AI.General.DefencePositions.Count
    else
    begin
      Result := 0;
      LogParamWarning('States.StatAIDefencePositionsCount', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatArmyCount(aPlayer: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].Stats.GetArmyCount
    else
    begin
      Result := 0;
      LogParamWarning('States.StatArmyCount', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatCitizenCount(aPlayer: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].Stats.GetCitizensCount
    else
    begin
      Result := 0;
      LogParamWarning('States.StatCitizenCount', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GameTime: Cardinal;
begin
  try
    Result := gGame.GameTickCount;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PeaceTime: Cardinal;
begin
  try
    Result := 600 * gGame.GameOptions.Peacetime;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerAllianceCheck(aPlayer1, aPlayer2: Byte): Boolean;
begin
  try
    if  InRange(aPlayer1, 0, gHands.Count - 1)
    and InRange(aPlayer2, 0, gHands.Count - 1)
    and (gHands[aPlayer1].Enabled)
    and (gHands[aPlayer2].Enabled) then
      Result := gHands[aPlayer1].Alliances[aPlayer2] = at_Ally
    else
    begin
      Result := False;
      LogParamWarning('States.PlayerAllianceCheck', [aPlayer1, aPlayer2]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatHouseMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
var
  B: Byte;
begin
  try
    Result := 0;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
    begin
      for B := Low(HouseIndexToType) to High(HouseIndexToType) do
        if (B in aTypes) and (HouseIndexToType[B] <> ht_None) then
          inc(Result, gHands[aPlayer].Stats.GetHouseQty(HouseIndexToType[B]));
    end
    else
      LogParamWarning('States.StatHouseMultipleTypesCount', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatHouseTypeCount(aPlayer, aHouseType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType) then
      Result := gHands[aPlayer].Stats.GetHouseQty(HouseIndexToType[aHouseType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatHouseTypeCount', [aPlayer, aHouseType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatHouseTypePlansCount(aPlayer, aHouseType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType) then
      Result := gHands[aPlayer].Stats.GetHousePlans(HouseIndexToType[aHouseType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatHouseTypePlansCount', [aPlayer, aHouseType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//* Version: 5057
//* How many active players there are.
//+ Number of players
function TKMScriptStates.StatPlayerCount: Integer;
var
  I: Integer;
begin
  try
    Result := 0;
    for I := 0 to gHands.Count - 1 do
      if gHands[I].Enabled then
        Inc(Result);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerDefeated(aPlayer: Byte): Boolean;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := (gHands[aPlayer].AI.WonOrLost = wol_Lost)
    else
    begin
      Result := False;
      LogParamWarning('States.PlayerDefeated', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerVictorious(aPlayer: Byte): Boolean;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := (gHands[aPlayer].AI.WonOrLost = wol_Won)
    else
    begin
      Result := False;
      LogParamWarning('States.PlayerVictorious', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerWareDistribution(aPlayer, aWareType, aHouseType: Byte): Byte;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and(aWareType in [Low(WareIndexToType) .. High(WareIndexToType)])
    and HouseTypeValid(aHouseType) then
      Result := gHands[aPlayer].Stats.Ratio[WareIndexToType[aWareType], HouseIndexToType[aHouseType]]
    else
    begin
      Result := 0;
      LogParamWarning('States.PlayerWareDistribution', [aPlayer, aWareType, aHouseType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerGetAllUnits(aPlayer: Byte): TIntegerArray;
var
  I, UnitCount: Integer;
  U: TKMUnit;
begin
  try
    SetLength(Result, 0);

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin
      UnitCount := 0;

      //Allocate max required space
      SetLength(Result, gHands[aPlayer].Units.Count);
      for I := 0 to gHands[aPlayer].Units.Count - 1 do
      begin
        U := gHands[aPlayer].Units[I];
        //Skip units in training, they can't be disturbed until they are finished training
        if U.IsDeadOrDying or (U.UnitTask is TTaskSelfTrain) then Continue;
        Result[UnitCount] := U.UID;
        Inc(UnitCount);
      end;

      //Trim to length
      SetLength(Result, UnitCount);
    end
    else
    begin
      LogParamWarning('States.PlayerGetAllUnits', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerGetAllHouses(aPlayer: Byte): TIntegerArray;
var
  I, HouseCount: Integer;
  H: TKMHouse;
begin
  try
    SetLength(Result, 0);

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin
      HouseCount := 0;

      //Allocate max required space
      SetLength(Result, gHands[aPlayer].Houses.Count);
      for I := 0 to gHands[aPlayer].Houses.Count - 1 do
      begin
        H := gHands[aPlayer].Houses[I];
        if H.IsDestroyed then Continue;
        Result[HouseCount] := H.UID;
        Inc(HouseCount);
      end;

      //Trim to length
      SetLength(Result, HouseCount);
    end
    else
    begin
      LogParamWarning('States.PlayerGetAllHouses', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerGetAllGroups(aPlayer: Byte): TIntegerArray;
var
  I, GroupCount: Integer;
  G: TKMUnitGroup;
begin
  try
    SetLength(Result, 0);

    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin
      GroupCount := 0;

      //Allocate max required space
      SetLength(Result, gHands[aPlayer].UnitGroups.Count);
      for I := 0 to gHands[aPlayer].UnitGroups.Count - 1 do
      begin
        G := gHands[aPlayer].UnitGroups[I];
        if G.IsDead then Continue;
        Result[GroupCount] := G.UID;
        Inc(GroupCount);
      end;

      //Trim to length
      SetLength(Result, GroupCount);
    end
    else
    begin
      LogParamWarning('States.PlayerGetAllGroups', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerIsAI(aPlayer: Byte): Boolean;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].HandType = hndComputer
    else
    begin
      Result := False;
      LogParamWarning('States.PlayerIsAI', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitCount(aPlayer: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].Stats.GetUnitQty(ut_Any)
    else
    begin
      Result := 0;
      LogParamWarning('States.StatUnitCount', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
var
  B: Byte;
begin
  try
    Result := 0;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
    begin
      for B := Low(UnitIndexToType) to High(UnitIndexToType) do
        if B in aTypes then
          inc(Result, gHands[aPlayer].Stats.GetUnitQty(UnitIndexToType[B]));
    end
    else
      LogParamWarning('States.StatUnitMultipleTypesCount', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitTypeCount(aPlayer, aUnitType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aUnitType in [Low(UnitIndexToType)..High(UnitIndexToType)])
    then
      Result := gHands[aPlayer].Stats.GetUnitQty(UnitIndexToType[aUnitType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatUnitTypeCount', [aPlayer, aUnitType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitKilledCount(aPlayer, aUnitType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aUnitType in [Low(UnitIndexToType)..High(UnitIndexToType)])
    then
      Result := gHands[aPlayer].Stats.GetUnitKilledQty(UnitIndexToType[aUnitType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatUnitKilledCount', [aPlayer, aUnitType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitKilledMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
var
  B: Byte;
begin
  try
    Result := 0;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
    begin
      for B := Low(UnitIndexToType) to High(UnitIndexToType) do
        if B in aTypes then
          inc(Result, gHands[aPlayer].Stats.GetUnitKilledQty(UnitIndexToType[B]));
    end
    else
      LogParamWarning('States.StatUnitKilledMultipleTypesCount', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitLostCount(aPlayer, aUnitType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aUnitType in [Low(UnitIndexToType)..High(UnitIndexToType)])
    then
      Result := gHands[aPlayer].Stats.GetUnitLostQty(UnitIndexToType[aUnitType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatUnitLostCount', [aPlayer, aUnitType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatUnitLostMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
var
  B: Byte;
begin
  try
    Result := 0;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
    begin
      for B := Low(UnitIndexToType) to High(UnitIndexToType) do
        if B in aTypes then
          inc(Result, gHands[aPlayer].Stats.GetUnitLostQty(UnitIndexToType[B]));
    end
    else
      LogParamWarning('States.StatUnitLostMultipleTypesCount', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatResourceProducedCount(aPlayer, aResType: Byte): Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aResType in [Low(WareIndexToType)..High(WareIndexToType)])
    then
      Result := gHands[aPlayer].Stats.GetWaresProduced(WareIndexToType[aResType])
    else
    begin
      Result := 0;
      LogParamWarning('States.StatResourceProducedCount', [aPlayer, aResType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.StatResourceProducedMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer;
var
  B: Byte;
begin
  try
    Result := 0;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
    begin
      for B := Low(WareIndexToType) to High(WareIndexToType) do
        if B in aTypes then
          inc(Result, gHands[aPlayer].Stats.GetWaresProduced(WareIndexToType[B]));
    end
    else
      LogParamWarning('States.StatResourceProducedMultipleTypesCount', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerColorText(aPlayer: Byte): AnsiString;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := AnsiString(Format('%.6x', [FlagColorToTextColor(gHands[aPlayer].FlagColor) and $FFFFFF]))
    else
    begin
      Result := '';
      LogParamWarning('States.PlayerColorText', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerEnabled(aPlayer: Byte): Boolean;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) then
      Result := gHands[aPlayer].Enabled
    else
    begin
      Result := False;
      LogParamWarning('States.PlayerEnabled', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.PlayerName(aPlayer: Byte): AnsiString;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := AnsiString(gHands[aPlayer].OwnerName)
    else
    begin
      Result := '';
      LogParamWarning('States.PlayerName', [aPlayer]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseAt(aX, aY: Word): Integer;
var
  H: TKMHouse;
begin
  try
    Result := UID_NONE;
    if gTerrain.TileInMapCoords(aX,aY) then
    begin
      H := gHands.HousesHitTest(aX, aY);
      if (H <> nil) and not H.IsDestroyed then
      begin
        Result := H.UID;
        fIDCache.CacheHouse(H, H.UID); //Improves cache efficiency since H will probably be accessed soon
      end;
    end
    else
      LogParamWarning('States.HouseAt', [aX, aY]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseBarracksRallyPointX(aBarracks: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if aBarracks > 0 then
    begin
      H := fIDCache.GetHouse(aBarracks);
      if (H <> nil) and not H.IsDestroyed  and (H.IsComplete) then
        if (H is TKMHouseBarracks) then
          Result := TKMHouseBarracks(H).RallyPoint.X
        else
          LogParamWarning('States.HouseBarracksRallyPointX: Specified house is not Barracks', [aBarracks]);
    end
    else
      LogParamWarning('States.HouseBarracksRallyPointX', [aBarracks]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseBarracksRallyPointY(aBarracks: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if aBarracks > 0 then
    begin
      H := fIDCache.GetHouse(aBarracks);
      if (H <> nil) and not H.IsDestroyed and (H.IsComplete) then
        if (H is TKMHouseBarracks) then
          Result := TKMHouseBarracks(H).RallyPoint.Y
        else
          LogParamWarning('States.HouseBarracksRallyPointY: Specified house is not Barracks', [aBarracks]);
    end
    else
      LogParamWarning('States.HouseBarracksRallyPointY', [aBarracks]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseBuildingProgress(aHouseID: Integer): Word;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) then
        Result := H.BuildingProgress;
    end
    else
      LogParamWarning('States.HouseBuildingProgress', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseCanReachResources(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := not H.ResourceDepletedMsgIssued;
    end
    else
      LogParamWarning('States.HouseCanReachResources', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseDamage(aHouseID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := -1; //-1 if house id is invalid
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.GetDamage;
    end
    else
      LogParamWarning('States.HouseDamage', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseDeliveryBlocked(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := True;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := (not H.WareDelivery);
    end
    else
      LogParamWarning('States.HouseDeliveryBlocked', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseDestroyed(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := True;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.IsDestroyed;
    end
    else
      LogParamWarning('States.HouseDestroyed', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseHasOccupant(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.GetHasOwner;
    end
    else
      LogParamWarning('States.HouseHasOccupant', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseIsComplete(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.IsComplete;
    end
    else
      LogParamWarning('States.HouseIsComplete', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HousePositionX(aHouseID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := UID_NONE;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.GetEntrance.X;
    end
    else
      LogParamWarning('States.HousePositionX', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HousePositionY(aHouseID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := UID_NONE;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.GetEntrance.Y;
    end
    else
      LogParamWarning('States.HousePositionY', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseOwner(aHouseID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := PLAYER_NONE;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.Owner;
    end
    else
      LogParamWarning('States.HouseOwner', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseRepair(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.BuildingRepair;
    end
    else
      LogParamWarning('States.HouseRepair', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseResourceAmount(aHouseID, aResource: Integer): Integer;
var
  H: TKMHouse;
  Res: TWareType;
begin
  try
    Result := -1; //-1 if house id is invalid
    if (aHouseID > 0) and (aResource in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aResource];
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.CheckResIn(Res) + H.CheckResOut(Res); //Count both in and out
    end
    else
      LogParamWarning('States.HouseResourceAmount', [aHouseID, aResource]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Get the unit type in Schools queue
function TKMScriptStates.HouseSchoolQueue(aHouseID, QueueIndex: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := -1;
    if (aHouseID > 0) and InRange(QueueIndex, 0, 5) then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and (H is TKMHouseSchool) then
        Result := UnitTypeToIndex[TKMHouseSchool(H).Queue[QueueIndex]];
    end
    else
      LogParamWarning('States.HouseSchoolQueue', [aHouseID, QueueIndex]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseSiteIsDigged(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := H.BuildingState <> hbs_NoGlyph;
    end
    else
      LogParamWarning('States.HouseSiteIsDigged', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Get the house type
function TKMScriptStates.HouseType(aHouseID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := -1;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        Result := HouseTypeToIndex[H.HouseType] - 1;
    end
    else
      LogParamWarning('States.HouseType', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseTypeMaxHealth(aHouseType: Integer): Word;
begin
  try
    Result := 0;
    if HouseTypeValid(aHouseType) then
      Result := gRes.HouseDat[HouseIndexToType[aHouseType]].MaxHealth
    else
      LogParamWarning('States.HouseTypeMaxHealth', [aHouseType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseTypeName(aHouseType: Byte): AnsiString;
begin
  try
    if HouseTypeValid(aHouseType) then
      Result := '<%' + AnsiString(IntToStr(gRes.HouseDat[HouseIndexToType[aHouseType]].HouseNameTextID)) + '>'
    else
    begin
      Result := '';
      LogParamWarning('States.HouseTypeName', [aHouseType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseTypeToOccupantType(aHouseType: Integer): Integer;
begin
  try
    Result := -1;
    if HouseTypeValid(aHouseType) then
    begin
      Result := UnitTypeToIndex[gRes.HouseDat[HouseIndexToType[aHouseType]].OwnerType];
    end
    else
      LogParamWarning('States.HouseTypeToOccupantType', [aHouseType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseUnlocked(aPlayer, aHouseType: Word): Boolean;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType) then
      Result := gHands[aPlayer].Locks.HouseCanBuild(HouseIndexToType[aHouseType])
    else
    begin
      Result := False;
      LogParamWarning('States.HouseUnlocked', [aPlayer, aHouseType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseWareBlocked(aHouseID, aWareType: Integer): Boolean;
var
  H: TKMHouse;
  Res: TWareType;
begin
  try
    Result := False;
    if (aHouseID > 0) and (aWareType in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aWareType];
      H := fIDCache.GetHouse(aHouseID);
      if (H is TKMHouseStore) then
        Result := TKMHouseStore(H).NotAcceptFlag[Res];
      if (H is TKMHouseBarracks) and (Res in [WARFARE_MIN..WARFARE_MAX]) then
        Result := TKMHouseBarracks(H).NotAcceptFlag[Res];
    end
    else
      LogParamWarning('States.HouseWareBlocked', [aHouseID, aWareType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseWeaponsOrdered(aHouseID, aWareType: Integer): Integer;
var
  H: TKMHouse;
  Res: TWareType;
  I: Integer;
begin
  try
    Result := 0;
    if (aHouseID > 0) and (aWareType in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aWareType];
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) then
        for I := 1 to 4 do
          if gRes.HouseDat[H.HouseType].ResOutput[I] = Res then
          begin
            Result := H.ResOrder[I];
            Exit;
          end;
    end
    else
      LogParamWarning('States.HouseWeaponsOrdered', [aHouseID, aWareType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.HouseWoodcutterChopOnly(aHouseID: Integer): Boolean;
var
  H: TKMHouse;
begin
  try
    Result := False;
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H is TKMHouseWoodcutters then
        Result := TKMHouseWoodcutters(H).WoodcutterMode = wcm_Chop;
    end
    else
      LogParamWarning('States.HouseWoodcutterChopOnly', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.IsFieldAt(aPlayer: ShortInt; X, Y: Word): Boolean;
begin
  try
    Result := False;
    //-1 stands for any player
    if InRange(aPlayer, -1, gHands.Count - 1) and gTerrain.TileInMapCoords(X, Y) then
      Result := gTerrain.TileIsCornField(KMPoint(X,Y))
                and ((aPlayer = -1) or (gTerrain.Land[Y, X].TileOwner = aPlayer))
    else
      LogParamWarning('States.IsFieldAt', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.IsRoadAt(aPlayer: ShortInt; X, Y: Word): Boolean;
begin
  try
    Result := False;
    //-1 stands for any player
    if InRange(aPlayer, -1, gHands.Count - 1) and gTerrain.TileInMapCoords(X, Y) then
      Result := (gTerrain.Land[Y,X].TileOverlay = to_Road)
                and ((aPlayer = -1) or (gTerrain.Land[Y, X].TileOwner = aPlayer))
    else
      LogParamWarning('States.IsRoadAt', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.IsWinefieldAt(aPlayer: ShortInt; X, Y: Word): Boolean;
begin
  try
    Result := False;
    //-1 stands for any player
    if InRange(aPlayer, -1, gHands.Count - 1) and gTerrain.TileInMapCoords(X, Y) then
      Result := gTerrain.TileIsWineField(KMPoint(X,Y))
                and ((aPlayer = -1) or (gTerrain.Land[Y, X].TileOwner = aPlayer))
    else
      LogParamWarning('States.IsWinefieldAt', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.KaMRandom: Single;
begin
  try
    Result := KM_Utils.KaMRandom;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.KaMRandomI(aMax:Integer): Integer;
begin
  try
    //No parameters to check, any integer is fine (even negative)
    Result := KM_Utils.KaMRandom(aMax);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.LocationCount: Integer;
begin
  try
    Result := gHands.Count;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapTileType(X, Y: Integer): Integer;
begin
  try
    if gTerrain.TileInMapCoords(X, Y) then
      Result := gTerrain.Land[Y, X].Terrain
    else
    begin
      Result := -1;
      LogParamWarning('States.MapTileType', [X, Y]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapTileRotation(X, Y: Integer): Integer;
begin
  try
    if gTerrain.TileInMapCoords(X, Y) then
      //In KaM map format values can be >= 4. Convert again just in case it was missed by gTerrain
      Result := gTerrain.Land[Y, X].Rotation mod 4
    else
    begin
      Result := -1;
      LogParamWarning('States.MapTileRotation', [X, Y]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapWidth: Integer;
begin
  try
    Result := gTerrain.MapX
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapHeight: Integer;
begin
  try
    Result := gTerrain.MapY
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapTileHeight(X, Y: Integer): Integer;
begin
  try
    if gTerrain.TileInMapCoords(X, Y) then
      Result := gTerrain.Land[Y, X].Height
    else
    begin
      Result := -1;
      LogParamWarning('States.MapTileHeight', [X, Y]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MapTileObject(X, Y: Integer): Integer;
begin
  try
    if gTerrain.TileInMapCoords(X, Y) then
      Result := gTerrain.Land[Y, X].Obj
    else
    begin
      Result := -1;
      LogParamWarning('States.MapTileObject', [X, Y]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MarketFromWare(aMarketID: Integer): Integer;
var
  H: TKMHouse;
  ResFrom: TWareType;
begin
  try
    Result := -1;
    if aMarketID > 0 then
    begin
      H := fIDCache.GetHouse(aMarketID);
      if (H is TKMHouseMarket)
      and (not H.IsDestroyed)
      and (TKMHouseMarket(H).ResFrom <> TKMHouseMarket(H).ResTo)
      and (TKMHouseMarket(H).ResFrom in [WARE_MIN .. WARE_MAX])
      and (TKMHouseMarket(H).ResTo in [WARE_MIN .. WARE_MAX]) then
      begin
        ResFrom := TKMHouseMarket(H).ResFrom;
        Result := WareTypeToIndex[ResFrom];
      end;
    end
    else
      LogParamWarning('States.MarketFromWare', [aMarketID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MarketLossFactor: Single;
begin
  try
    Result := MARKET_TRADEOFF_FACTOR;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MarketOrderAmount(aMarketID: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if aMarketID > 0 then
    begin
      H := fIDCache.GetHouse(aMarketID);
      if (H is TKMHouseMarket)
      and (not H.IsDestroyed)
      and (TKMHouseMarket(H).ResFrom <> TKMHouseMarket(H).ResTo)
      and (TKMHouseMarket(H).ResFrom in [WARE_MIN .. WARE_MAX])
      and (TKMHouseMarket(H).ResTo in [WARE_MIN .. WARE_MAX]) then
        Result := TKMHouseMarket(H).ResOrder[0];
    end
    else
      LogParamWarning('States.MarketOrderAmount', [aMarketID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MarketToWare(aMarketID: Integer): Integer;
var
  H: TKMHouse;
  ResTo: TWareType;
begin
  try
    Result := -1;
    if aMarketID > 0 then
    begin
      H := fIDCache.GetHouse(aMarketID);
      if (H is TKMHouseMarket)
      and (not H.IsDestroyed)
      and (TKMHouseMarket(H).ResFrom <> TKMHouseMarket(H).ResTo)
      and (TKMHouseMarket(H).ResFrom in [WARE_MIN .. WARE_MAX])
      and (TKMHouseMarket(H).ResTo in [WARE_MIN .. WARE_MAX]) then
      begin
        ResTo := TKMHouseMarket(H).ResTo;
        Result := WareTypeToIndex[ResTo];
      end;
    end
    else
      LogParamWarning('States.MarketToWare', [aMarketID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.MarketValue(aRes: Integer): Single;
var
  Res: TWareType;
begin
  try
    Result := -1; //-1 if ware is invalid
    if aRes in [Low(WareIndexToType)..High(WareIndexToType)] then
    begin
      Res := WareIndexToType[aRes];
      Result := gRes.Wares[Res].MarketPrice;
    end
    else
      LogParamWarning('States.MarketValue', [aRes]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.FogRevealed(aPlayer: Byte; aX, aY: Word): Boolean;
begin
  try
    Result := False;
    if gTerrain.TileInMapCoords(aX,aY)
    and InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      Result := gHands[aPlayer].FogOfWar.CheckTileRevelation(aX, aY) > 0
    else
      LogParamWarning('States.FogRevealed', [aPlayer, aX, aY]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitAt(aX, aY: Word): Integer;
var
  U: TKMUnit;
begin
  try
    Result := UID_NONE;
    if gTerrain.TileInMapCoords(aX,aY) then
    begin
      U := gTerrain.UnitsHitTest(aX, aY);
      if (U <> nil) and not U.IsDeadOrDying then
      begin
        Result := U.UID;
        fIDCache.CacheUnit(U, U.UID); //Improves cache efficiency since U will probably be accessed soon
      end;
    end
    else
      LogParamWarning('States.UnitAt', [aX, aY]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitPositionX(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1; //-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := U.GetPosition.X;
    end
    else
      LogParamWarning('States.UnitPositionX', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitPositionY(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1; //-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := U.GetPosition.Y;
    end
    else
      LogParamWarning('States.UnitPositionY', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitDead(aUnitID: Integer): Boolean;
var
  U: TKMUnit;
begin
  try
    Result := True;
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := U.IsDeadOrDying;
    end
    else
      LogParamWarning('States.UnitDead', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitOwner(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := PLAYER_NONE;
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := U.Owner;
    end
    else
      LogParamWarning('States.UnitOwner', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitDirection(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1;//-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := Byte(U.Direction) - 1;
    end
    else
      LogParamWarning('States.UnitDirection', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitType(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1; //-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := UnitTypeToIndex[U.UnitType];
    end
    else
      LogParamWarning('States.UnitType', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitTypeName(aUnitType: Byte): AnsiString;
begin
  try
    if (aUnitType in [Low(UnitIndexToType) .. High(UnitIndexToType)]) then
      Result := '<%' + AnsiString(IntToStr(gRes.UnitDat[UnitIndexToType[aUnitType]].GUITextID)) + '>'
    else
    begin
      Result := '';
      LogParamWarning('States.UnitTypeName', [aUnitType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.WareTypeName(aWareType: Byte): AnsiString;
begin
  try
    if (aWareType in [Low(WareIndexToType) .. High(WareIndexToType)]) then
      Result := '<%' + AnsiString(IntToStr(gRes.Wares[WareIndexToType[aWareType]].TextID)) + '>'
    else
    begin
      Result := '';
      LogParamWarning('States.WareTypeName', [aWareType]);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitHunger(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1; //-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        Result := Max(U.Condition, 0)*CONDITION_PACE;
    end
    else
      LogParamWarning('States.UnitHunger', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitCarrying(aUnitID: Integer): Integer;
var
  U: TKMUnit;
begin
  try
    Result := -1; //-1 if unit id is invalid
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if (U <> nil) and (U is TKMUnitSerf) and (TKMUnitSerf(U).Carry in [WARE_MIN..WARE_MAX]) then
        Result := WareTypeToIndex[TKMUnitSerf(U).Carry];
    end
    else
      LogParamWarning('States.UnitCarrying', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitHome(aUnitID: Integer): Integer;
var
  U: TKMUnit;
  H: TKMHouse;
begin
  try
    Result := -1;
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if (U <> nil) then
      begin
        H := U.GetHome;
        if (H <> nil) and not H.IsDestroyed then
        begin
          Result := H.UID;
          fIDCache.CacheHouse(H, H.UID); //Improves cache efficiency since H will probably be accessed soon
        end;
      end;
    end
    else
      LogParamWarning('States.UnitHome', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitIdle(aUnitID: Integer): Boolean;
var
  U: TKMUnit;
begin
  try
    Result := False;
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if (U <> nil) then
        Result := U.IsIdle;
    end
    else
      LogParamWarning('States.UnitIdle', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitMaxHunger: Integer;
begin
  try
    Result := UNIT_MAX_CONDITION*CONDITION_PACE;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitLowHunger: Integer;
begin
  try
    Result := UNIT_MIN_CONDITION*CONDITION_PACE;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupAt(aX, aY: Word): Integer;
var
  G: TKMUnitGroup;
begin
  try
    G := gHands.GroupsHitTest(aX, aY);
    if (G <> nil) and not G.IsDead then
    begin
      Result := G.UID;
      fIDCache.CacheGroup(G, G.UID); //Improves cache efficiency since G will probably be accessed soon
    end
    else
      Result := UID_NONE;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.UnitsGroup(aUnitID: Integer): Integer;
var
  U: TKMUnit;
  G: TKMUnitGroup;
begin
  try
    Result := UID_NONE;
    if aUnitID > 0 then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if (U <> nil) and (U is TKMUnitWarrior) then
      begin
        G := gHands[U.Owner].UnitGroups.GetGroupByMember(TKMUnitWarrior(U));
        if G <> nil then
        begin
          Result := G.UID;
          fIDCache.CacheGroup(G, G.UID); //Improves cache efficiency since G will probably be accessed soon
        end;
      end;
    end
    else
      LogParamWarning('States.UnitsGroup', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupDead(aGroupID: Integer): Boolean;
var
  G: TKMUnitGroup;
begin
  try
    Result := True;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := G.IsDead;
    end
    else
      LogParamWarning('States.GroupDead', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupIdle(aGroupID: Integer): Boolean;
var
  G: TKMUnitGroup;
begin
  try
    Result := False;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := G.Order = goNone;
    end
    else
      LogParamWarning('States.GroupIdle', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupOwner(aGroupID: Integer): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := PLAYER_NONE;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := G.Owner;
    end
    else
      LogParamWarning('States.GroupOwner', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupType(aGroupID: Integer): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := -1;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := Byte(G.GroupType);
    end
    else
      LogParamWarning('States.GroupType', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupMemberCount(aGroupID: Integer): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := 0;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := G.Count;
    end
    else
      LogParamWarning('States.GroupMemberCount', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupColumnCount(aGroupID: Integer): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := 0;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        Result := G.UnitsPerRow;
    end
    else
      LogParamWarning('States.GroupColumnCount', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptStates.GroupMember(aGroupID, aMemberIndex: Integer): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := UID_NONE;
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
      begin
        if InRange(aMemberIndex, 0, G.Count-1) then
        begin
          Result := G.Members[aMemberIndex].UID;
          //Improves cache efficiency since unit will probably be accessed soon
          fIDCache.CacheUnit(G.Members[aMemberIndex], Result);
        end
        else
          LogParamWarning('States.GroupMember', [aGroupID, aMemberIndex]);
      end;
    end
    else
      LogParamWarning('States.GroupMember', [aGroupID, aMemberIndex]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


end.
