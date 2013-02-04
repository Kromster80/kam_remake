unit KM_ScriptingESA;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  KM_Defaults, KM_Points, KM_Houses, KM_Units, KM_UnitGroups;


//For caching unit/house/group IDs. Shared between States and Actions.
//Because scripts runs the same on every computer (i.e. no access to MyPlayer)
//we can safely use pointers within the cache
const CACHE_SIZE = 16; //Too big means caching becomes slow
type
  TKMIDCache = class
  private
    fUnitCount: Byte;
    fUnitLastAdded: Byte;
    fUnitCache: array[0..CACHE_SIZE-1] of record
                                            ID: Integer;
                                            U: TKMUnit;
                                          end;
    fHouseCount: Byte;
    fHouseLastAdded: Byte;
    fHouseCache: array[0..CACHE_SIZE-1] of record
                                             ID: Integer;
                                             H: TKMHouse;
                                           end;
    fGroupCount: Byte;
    fGroupLastAdded: Byte;
    fGroupCache: array[0..CACHE_SIZE-1] of record
                                             ID: Integer;
                                             G: TKMUnitGroup;
                                           end;

  public
    function GetUnit(aID:Integer): TKMUnit;
    function GetHouse(aID:Integer): TKMHouse;
    function GetGroup(aID:Integer): TKMUnitGroup;
    procedure UpdateState;
  end;

  //Two classes exposed to scripting States and Actions

  //All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)

  //How to add new a method exposed to the scripting? Three steps:
  //1. Add method to published section here below
  //2. Add method declaration to Compiler (TKMScripting.ScriptOnUses)
  //3. Add method name to Runtime (TKMScripting.LinkRuntime)
  TKMScriptStates = class
  private
    fIDCache: TKMIDCache;
    procedure LogError(aFuncName: string; const aValues: array of Integer);
  public
    constructor Create(aIDCache: TKMIDCache);
    function ArmyCount(aPlayer: Byte): Integer;
    function CitizenCount(aPlayer: Byte): Integer;
    function GameTime: Cardinal;
    function PeaceTime: Cardinal;
    function CheckAlliance(aPlayer1, aPlayer2: Byte): Boolean;
    function HouseTypeCount(aPlayer, aHouseType: Byte): Integer;
    function PlayerCount: Integer;
    function PlayerDefeated(aPlayer: Byte): Boolean;
    function PlayerVictorious(aPlayer: Byte): Boolean;
    function UnitCount(aPlayer: Byte): Integer;
    function UnitTypeCount(aPlayer, aUnitType: Byte): Integer;
    function PlayerName(aPlayer: Byte): AnsiString;
    function PlayerEnabled(aPlayer: Byte): Boolean;
    function HouseAt(aX, aY: Word): Integer;
    function HouseDestroyed(aHouseID: Integer): Boolean;
    function HouseOwner(aHouseID: Integer): Integer;
    function HouseType(aHouseID: Integer): Integer;
    function HouseDamage(aHouseID: Integer): Integer;
    function KaMRandom: Single;
    function KaMRandomI(aMax:Integer): Integer;
    function UnitAt(aX, aY: Word): Integer;
    function UnitDead(aUnitID: Integer): Boolean;
    function UnitOwner(aUnitID: Integer): Integer;
    function UnitType(aUnitID: Integer): Integer;
    function UnitHunger(aUnitID: Integer): Integer;
    function UnitMaxHunger: Integer;
    function UnitLowHunger: Integer;
    function GroupAt(aX, aY: Word): Integer;
    function GroupDead(aGroupID: Integer): Boolean;
    function GroupOwner(aGroupID: Integer): Integer;
    function GroupMemberCount(aGroupID: Integer): Integer;
    function GroupMember(aGroupID, aMemberIndex: Integer): Integer;
  end;

  TKMScriptActions = class
  private
    fIDCache: TKMIDCache;
    procedure LogError(aFuncName: string; const aValues: array of Integer);
  public
    constructor Create(aIDCache: TKMIDCache);
    procedure Defeat(aPlayer: Word);
    procedure Victory(const aVictors: array of Integer; aTeamVictory: Boolean);
    function GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word): Integer;
    function GiveUnit(aPlayer, aType, X,Y, aDir: Word): Integer;
    function GiveAnimal(aType, X,Y: Word): Integer;
    procedure GiveWares(aPlayer, aType, aCount: Word);
    procedure RevealCircle(aPlayer, X, Y, aRadius: Word);
    procedure ShowMsg(aPlayer, aIndex: Word);
    procedure ShowMsgFormatted(aPlayer, aIndex: Word; const Args: array of const);
    procedure UnlockHouse(aPlayer, aHouseType: Word);
    procedure AddHouseDamage(aHouseID: Integer; aDamage: Word);
    procedure DestroyHouse(aHouseID: Integer);
    procedure GiveWaresToHouse(aHouseID: Integer; aType, aCount: Word);
    procedure SetOverlayText(aPlayer, aIndex: Word);
    procedure SetOverlayTextFormatted(aPlayer, aIndex: Word; const Args: array of const);
    procedure SetUnitHunger(aUnitID, aHungerLevel: Integer);
    procedure SetUnitDirection(aUnitID, aDirection: Integer);
    procedure KillUnit(aUnitID: Integer);
    procedure GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word);
    procedure GroupOrderAttackHouse(aGroupID, aHouseID: Integer);
    procedure GroupOrderAttackUnit(aGroupID, aUnitID: Integer);
  end;


implementation
uses KM_AI, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection,
  KM_TextLibrary, KM_ResourceUnit, KM_ResourceResource, KM_ResourceHouse, KM_Log, KM_Utils;


  //We need to check all input parameters as could be wildly off range due to
  //mistakes in scripts. In that case we have two options:
  // - skip silently and log
  // - report to player

{ TKMScriptStates }
constructor TKMScriptStates.Create(aIDCache: TKMIDCache);
begin
  Inherited Create;
  fIDCache := aIDCache;
end;


procedure TKMScriptStates.LogError(aFuncName: string; const aValues: array of Integer);
var
  I: Integer;
  Values: string;
begin
  for I := Low(aValues) to High(aValues) do
    Values := Values + IntToStr(aValues[I]) + IfThen(I<>High(aValues), ', ');
  fLog.AddTime('Mistake in script usage' + aFuncName + ': ' + Values);
end;


function TKMScriptStates.ArmyCount(aPlayer: Byte): Integer;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer].Stats.GetArmyCount
  else
  begin
    Result := 0;
    LogError('States.ArmyCount', [aPlayer]);
  end;
end;


function TKMScriptStates.CitizenCount(aPlayer: Byte): Integer;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer].Stats.GetCitizensCount
  else
  begin
    Result := 0;
    LogError('States.CitizenCount', [aPlayer]);
  end;
end;


function TKMScriptStates.GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


function TKMScriptStates.PeaceTime: Cardinal;
begin
  Result := 600*fGame.GameOptions.Peacetime;
end;


function TKMScriptStates.CheckAlliance(aPlayer1, aPlayer2: Byte): Boolean;
begin
  if  InRange(aPlayer1, 0, fPlayers.Count - 1)
  and InRange(aPlayer2, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer1].Alliances[aPlayer2] = at_Ally
  else
  begin
    Result := False;
    LogError('States.CheckAlliance', [aPlayer1, aPlayer2]);
  end;
end;


function TKMScriptStates.HouseTypeCount(aPlayer, aHouseType: Byte): Integer;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and (aHouseType in [Low(HouseIndexToType)..High(HouseIndexToType)])
  then
    Result := fPlayers[aPlayer].Stats.GetHouseQty(HouseIndexToType[aHouseType])
  else
  begin
    Result := 0;
    LogError('States.HouseTypeCount', [aPlayer, aHouseType]);
  end;
end;


function TKMScriptStates.PlayerCount: Integer;
begin
  Result := fPlayers.Count;
end;


function TKMScriptStates.PlayerDefeated(aPlayer: Byte): Boolean;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := (fPlayers[aPlayer].AI.WonOrLost = wol_Lost)
  else
  begin
    Result := False;
    LogError('States.PlayerDefeated', [aPlayer]);
  end;
end;


function TKMScriptStates.PlayerVictorious(aPlayer: Byte): Boolean;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := (fPlayers[aPlayer].AI.WonOrLost = wol_Won)
  else
  begin
    Result := False;
    LogError('States.PlayerVictorious', [aPlayer]);
  end;
end;


function TKMScriptStates.UnitCount(aPlayer: Byte): Integer;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer].Stats.GetUnitQty(ut_Any)
  else
  begin
    Result := 0;
    LogError('States.UnitCount', [aPlayer]);
  end;
end;


function TKMScriptStates.UnitTypeCount(aPlayer, aUnitType: Byte): Integer;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and (aUnitType in [Low(UnitIndexToType)..High(UnitIndexToType)])
  then
    Result := fPlayers[aPlayer].Stats.GetUnitQty(UnitIndexToType[aUnitType])
  else
  begin
    Result := 0;
    LogError('States.UnitTypeCount', [aPlayer, aUnitType]);
  end;
end;


function TKMScriptStates.PlayerName(aPlayer: Byte): AnsiString;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer].PlayerName
  else
  begin
    Result := '';
    LogError('States.PlayerName', [aPlayer]);
  end;
end;


function TKMScriptStates.PlayerEnabled(aPlayer: Byte): Boolean;
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    Result := fPlayers[aPlayer].Enabled
  else
  begin
    Result := False;
    LogError('States.PlayerEnabled', [aPlayer]);
  end;
end;


function TKMScriptStates.HouseAt(aX, aY: Word): Integer;
var H: TKMHouse;
begin
  H := fPlayers.HousesHitTest(aX, aY);
  if (H <> nil) and not H.IsDestroyed then
    Result := H.ID
  else
    Result := -1;
end;


function TKMScriptStates.HouseDestroyed(aHouseID: Integer): Boolean;
var H: TKMHouse;
begin
  Result := True;
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      Result := H.IsDestroyed;
  end
  else
    LogError('States.HouseDestroyed', [aHouseID]);
end;


function TKMScriptStates.HouseOwner(aHouseID: Integer): Integer;
var H: TKMHouse;
begin
  Result := -1;
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      Result := H.Owner;
  end
  else
    LogError('States.HouseOwner', [aHouseID]);
end;


function TKMScriptStates.HouseType(aHouseID: Integer): Integer;
var H: TKMHouse;
begin
  Result := -1;
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      Result := HouseTypeToIndex[H.HouseType]-1;
  end
  else
    LogError('States.HouseType', [aHouseID]);
end;


function TKMScriptStates.HouseDamage(aHouseID: Integer): Integer;
var H: TKMHouse;
begin
  Result := -1;
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      Result := H.GetDamage;
  end
  else
    LogError('States.HouseDamage', [aHouseID]);
end;


function TKMScriptStates.KaMRandom: Single;
begin
  Result := KM_Utils.KaMRandom;
end;


function TKMScriptStates.KaMRandomI(aMax:Integer): Integer;
begin
  //No parameters to check, any integer is fine (even negative)
  Result := KM_Utils.KaMRandom(aMax);
end;


function TKMScriptStates.UnitAt(aX, aY: Word): Integer;
var U: TKMUnit;
begin
  U := fTerrain.UnitsHitTest(aX, aY);
  if (U <> nil) and not U.IsDead then
    Result := U.ID
  else
    Result := -1;
end;


function TKMScriptStates.UnitDead(aUnitID: Integer): Boolean;
var U: TKMUnit;
begin
  Result := True;
  if aUnitID > 0 then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      Result := U.IsDead;
  end
  else
    LogError('States.UnitDead', [aUnitID]);
end;


function TKMScriptStates.UnitOwner(aUnitID: Integer): Integer;
var U: TKMUnit;
begin
  Result := -1;
  if aUnitID > 0 then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      Result := U.Owner;
  end
  else
    LogError('States.UnitOwner', [aUnitID]);
end;


function TKMScriptStates.UnitType(aUnitID: Integer): Integer;
var U: TKMUnit;
begin
  Result := -1;
  if aUnitID > 0 then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      Result := UnitTypeToIndex[U.UnitType];
  end
  else
    LogError('States.UnitType', [aUnitID]);
end;


function TKMScriptStates.UnitHunger(aUnitID: Integer): Integer;
var U: TKMUnit;
begin
  Result := -1;
  if aUnitID > 0 then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      Result := Max(U.Condition, 0)*CONDITION_PACE;
  end
  else
    LogError('States.UnitHunger', [aUnitID]);
end;


function TKMScriptStates.UnitMaxHunger: Integer;
begin
  Result := UNIT_MAX_CONDITION*CONDITION_PACE;
end;


function TKMScriptStates.UnitLowHunger: Integer;
begin
  Result := UNIT_MIN_CONDITION*CONDITION_PACE;
end;


function TKMScriptStates.GroupAt(aX, aY: Word): Integer;
var G: TKMUnitGroup;
begin
  G := fPlayers.GroupsHitTest(aX, aY);
  if (G <> nil) and not G.IsDead then
    Result := G.ID
  else
    Result := -1;
end;


function TKMScriptStates.GroupDead(aGroupID: Integer): Boolean;
var G: TKMUnitGroup;
begin
  Result := True;
  if aGroupID > 0 then
  begin
    G := fIDCache.GetGroup(aGroupID);
    if G <> nil then
      Result := G.IsDead;
  end
  else
    LogError('States.GroupDead', [aGroupID]);
end;


function TKMScriptStates.GroupOwner(aGroupID: Integer): Integer;
var G: TKMUnitGroup;
begin
  Result := -1;
  if aGroupID > 0 then
  begin
    G := fIDCache.GetGroup(aGroupID);
    if G <> nil then
      Result := G.Owner;
  end
  else
    LogError('States.GroupOwner', [aGroupID]);
end;


function TKMScriptStates.GroupMemberCount(aGroupID: Integer): Integer;
var G: TKMUnitGroup;
begin
  Result := 0;
  if aGroupID > 0 then
  begin
    G := fIDCache.GetGroup(aGroupID);
    if G <> nil then
      Result := G.Count;
  end
  else
    LogError('States.GroupMemberCount', [aGroupID]);
end;


function TKMScriptStates.GroupMember(aGroupID, aMemberIndex: Integer): Integer;
var G: TKMUnitGroup;
begin
  Result := 0;
  if aGroupID > 0 then
  begin
    G := fIDCache.GetGroup(aGroupID);
    if G <> nil then
    begin
      if InRange(aMemberIndex, 0, G.Count-1) then
        Result := G.Members[aMemberIndex].ID
      else
        LogError('States.GroupMember', [aGroupID, aMemberIndex]);
    end;
  end
  else
    LogError('States.GroupMember', [aGroupID, aMemberIndex]);
end;


{ TKMScriptActions }
constructor TKMScriptActions.Create(aIDCache: TKMIDCache);
begin
  Inherited Create;
  fIDCache := aIDCache;
end;


procedure TKMScriptActions.LogError(aFuncName: string; const aValues: array of Integer);
var
  I: Integer;
  Values: string;
begin
  for I := Low(aValues) to High(aValues) do
    Values := Values + IntToStr(aValues[I]) + IfThen(I<>High(aValues), ', ');
  fLog.AddTime('Mistake in script usage' + aFuncName + ': ' + Values);
end;


procedure TKMScriptActions.Defeat(aPlayer: Word);
begin
  //Verify all input parameters
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    fPlayers[aPlayer].AI.Defeat
  else
    LogError('Actions.Defeat', [aPlayer]);
end;


//Sets all player IDs in aVictors to victorious, and all their team members if aTeamVictory is true.
//All other players are set to defeated.
procedure TKMScriptActions.Victory(const aVictors: array of Integer; aTeamVictory: Boolean);
var I,K: Integer;
begin
  //Verify all input parameters
  for I:=0 to Length(aVictors)-1 do
  if not InRange(aVictors[I], 0, fPlayers.Count - 1) then
  begin
    LogError('Actions.Victory', [aVictors[I]]);
    Exit;
  end;

  for I:=0 to Length(aVictors)-1 do
    if fPlayers[aVictors[I]].Enabled then
    begin
      fPlayers[aVictors[I]].AI.Victory;
      if aTeamVictory then
        for K:=0 to fPlayers.Count-1 do
          if fPlayers[K].Enabled and (fPlayers[aVictors[I]].Alliances[K] = at_Ally) then
            fPlayers[K].AI.Victory;
    end;

  //All other players get defeated
  for I:=0 to fPlayers.Count-1 do
    if fPlayers[I].Enabled and (fPlayers[I].AI.WonOrLost = wol_None) then
      fPlayers[I].AI.Defeat;
end;


function TKMScriptActions.GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word): Integer;
var G: TKMUnitGroup;
begin
  Result := -1;
  //Verify all input parameters
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and (aType in [UnitTypeToIndex[WARRIOR_MIN]..UnitTypeToIndex[WARRIOR_MAX]])
  and fTerrain.TileInMapCoords(X,Y)
  and (TKMDirection(aDir+1) in [dir_N..dir_NW]) then
  begin
    G := fPlayers[aPlayer].AddUnitGroup(UnitIndexToType[aType],
                                        KMPoint(X,Y),
                                        TKMDirection(aDir+1),
                                        aColumns,
                                        aCount);
    if G = nil then Exit;
    Result := G.ID;
  end
  else
    LogError('Actions.GiveGroup', [aPlayer, aType, X, Y, aDir, aCount, aColumns]);
end;


function TKMScriptActions.GiveUnit(aPlayer, aType, X, Y, aDir: Word): Integer;
var U: TKMUnit;
begin
  Result := -1;
  //Verify all input parameters
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and (aType in [UnitTypeToIndex[CITIZEN_MIN]..UnitTypeToIndex[CITIZEN_MAX]])
  and fTerrain.TileInMapCoords(X,Y)
  and (TKMDirection(aDir+1) in [dir_N..dir_NW]) then
  begin
    U := fPlayers[aPlayer].AddUnit(UnitIndexToType[aType], KMPoint(X,Y));
    if U = nil then Exit;
    Result := U.ID;
    U.Direction := TKMDirection(aDir+1);
  end
  else
    LogError('Actions.GiveUnit', [aPlayer, aType, X, Y, aDir]);
end;


function TKMScriptActions.GiveAnimal(aType, X, Y: Word): Integer;
var U: TKMUnit;
begin
  Result := -1;
  //Verify all input parameters
  if (aType in [UnitTypeToOldIndex[ANIMAL_MIN]..UnitTypeToOldIndex[ANIMAL_MAX]])
  and fTerrain.TileInMapCoords(X,Y) then
  begin
    U := fPlayers.PlayerAnimals.AddUnit(UnitOldIndexToType[aType], KMPoint(X,Y));
    if U <> nil then
      Result := U.ID;
  end
  else
    LogError('Actions.GiveAnimal', [aType, X, Y]);
end;


procedure TKMScriptActions.GiveWares(aPlayer, aType, aCount: Word);
var
  H: TKMHouse;
begin
  //Verify all input parameters
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and InRange(aCount, 0, High(Word))
  and (aType in [Low(ResourceIndexToType)..High(ResourceIndexToType)]) then
  begin
    H := fPlayers[aPlayer].FindHouse(ht_Store, 1);
    if H <> nil then
    begin
      H.ResAddToIn(ResourceIndexToType[aType], aCount);
      fPlayers[aPlayer].Stats.GoodProduced(ResourceIndexToType[aType], aCount);
    end;
  end
  else
    LogError('Actions.GiveWares', [aPlayer, aType, aCount]);
end;


procedure TKMScriptActions.RevealCircle(aPlayer, X, Y, aRadius: Word);
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and fTerrain.TileInMapCoords(X,Y)
  and InRange(aRadius, 0, 255) then
  begin
    if aRadius = 255 then
      fPlayers[aPlayer].FogOfWar.RevealEverything
    else
      fPlayers[aPlayer].FogOfWar.RevealCircle(KMPoint(X, Y), aRadius, 255);
  end
  else
    LogError('Actions.RevealCircle', [aPlayer, X, Y, aRadius]);
end;


procedure TKMScriptActions.ShowMsg(aPlayer, aIndex: Word);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0))
end;


procedure TKMScriptActions.ShowMsgFormatted(aPlayer, aIndex: Word; const Args: array of const);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.ShowMessage(mkText, Format(fTextLibrary.GetMissionString(aIndex),Args), KMPoint(0,0))
end;


procedure TKMScriptActions.UnlockHouse(aPlayer, aHouseType: Word);
begin
  //Verify all input parameters
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and (aHouseType in [Low(HouseIndexToType) .. High(HouseIndexToType)]) then
    fPlayers[aPlayer].Stats.HouseGranted[HouseIndexToType[aHouseType]] := True
  else
    LogError('Actions.UnlockHouse', [aPlayer, aHouseType]);
end;


procedure TKMScriptActions.AddHouseDamage(aHouseID: Integer; aDamage: Word);
var H: TKMHouse;
begin
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      H.AddDamage(-1, aDamage);
  end
  else
    LogError('Actions.AddHouseDamage', [aHouseID, aDamage]);
end;


procedure TKMScriptActions.DestroyHouse(aHouseID: Integer);
var H: TKMHouse;
begin
  if aHouseID > 0 then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
      H.DemolishHouse(-1, False);
  end
  else
    LogError('Actions.DestroyHouse', [aHouseID]);
end;


procedure TKMScriptActions.GiveWaresToHouse(aHouseID: Integer; aType, aCount: Word);
var
  H: TKMHouse;
  Res: TResourceType;
begin
  Res := ResourceIndexToType[aType];
  if (aType in [Low(ResourceIndexToType)..High(ResourceIndexToType)]) and (aHouseID > 0) then
  begin
    H := fIDCache.GetHouse(aHouseID);
    if H <> nil then
    begin
      if H is TKMHouseBarracks then
      begin
        if Res in [WARFARE_MIN..WARFARE_MAX] then
          H.ResAddToIn(Res, aCount, True) //Barracks only accepts warfare
        //else @Krom: Should we log the specific error here? Or silently ignore?
        //@Lewin: I think we should raise an error. Generally the case should be handled by House.CanAcceptWare: Boolean kind of thing
      end
      else
        //Try to add it, it will be ignored if it's the wrong type and won't overfill due to aFromScript=True
        //todo: @Krom: We should show an error if adding trunks to tannery, House needs methods like CanTakeWare, etc. Overfilling should be silently ignored IMO, since you might want to ensure a house is completely full by adding 5
        H.ResAddToIn(Res, aCount, True);
    end;
    //Silently ignore if house doesn't exist
  end
  else
    LogError('Actions.GiveWaresToHouse', [aHouseID, aType, aCount]);
end;


procedure TKMScriptActions.SetOverlayText(aPlayer, aIndex: Word);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.GamePlayInterface.SetScriptedOverlay(fTextLibrary.GetMissionString(aIndex));
end;


procedure TKMScriptActions.SetOverlayTextFormatted(aPlayer, aIndex: Word; const Args: array of const);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.GamePlayInterface.SetScriptedOverlay(Format(fTextLibrary.GetMissionString(aIndex), Args));
end;


procedure TKMScriptActions.SetUnitHunger(aUnitID, aHungerLevel: Integer);
var U: TKMUnit;
begin
  aHungerLevel := Round(aHungerLevel / CONDITION_PACE);
  if (aUnitID > 0) and InRange(aHungerLevel, 0, UNIT_MAX_CONDITION) then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      U.Condition := aHungerLevel;
  end
  else
    LogError('Actions.SetUnitHunger', [aUnitID, aHungerLevel]);
end;


procedure TKMScriptActions.SetUnitDirection(aUnitID, aDirection: Integer);
var U: TKMUnit;
begin
  if (aUnitID > 0) and (TKMDirection(aDirection+1) in [dir_N..dir_NW]) then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      U.Direction := TKMDirection(aDirection+1);
  end
  else
    LogError('Actions.SetUnitDirection', [aUnitID, aDirection]);
end;


procedure TKMScriptActions.KillUnit(aUnitID: Integer);
var U: TKMUnit;
begin
  if (aUnitID > 0) then
  begin
    U := fIDCache.GetUnit(aUnitID);
    if U <> nil then
      U.KillUnit;
  end
  else
    LogError('Actions.KillUnit', [aUnitID]);
end;


procedure TKMScriptActions.GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word);
var G: TKMUnitGroup;
begin
  if (aGroupID > 0)
  and fTerrain.TileInMapCoords(X,Y)
  and (TKMDirection(aDirection+1) in [dir_N..dir_NW]) then
  begin
    G := fIDCache.GetGroup(aGroupID);
    if (G <> nil) and G.CanWalkTo(KMPoint(X,Y), 0) then
      G.OrderWalk(KMPoint(X,Y), True, TKMDirection(aDirection+1));
  end
  else
    LogError('Actions.GroupOrderWalk', [aGroupID, X, Y, aDirection]);
end;


procedure TKMScriptActions.GroupOrderAttackHouse(aGroupID, aHouseID: Integer);
var
  G: TKMUnitGroup;
  H: TKMHouse;
begin
  if (aGroupID > 0) and (aHouseID > 0) then
  begin
    G := fIDCache.GetGroup(aGroupID);
    H := fIDCache.GetHouse(aHouseID);
    if (G <> nil) and (H <> nil) then
      G.OrderAttackHouse(H, True);
  end
  else
    LogError('Actions.GroupOrderAttackHouse', [aGroupID, aHouseID]);
end;


procedure TKMScriptActions.GroupOrderAttackUnit(aGroupID, aUnitID: Integer);
var
  G: TKMUnitGroup;
  U: TKMUnit;
begin
  if (aGroupID > 0) and (aUnitID > 0) then
  begin
    G := fIDCache.GetGroup(aGroupID);
    U := fIDCache.GetUnit(aUnitID);
    if (G <> nil) and (U <> nil) then
      G.OrderAttackUnit(U, True);
  end
  else
    LogError('Actions.GroupOrderAttackHouse', [aGroupID, aUnitID]);
end;


{TKMIDCache}
function TKMIDCache.GetUnit(aID:Integer): TKMUnit;
var I, NewItem: Shortint;
begin
  for I:=0 to fUnitCount-1 do
    if fUnitCache[i].ID = aID then
    begin
      Result := fUnitCache[i].U;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  if fUnitCount < CACHE_SIZE then
  begin
    NewItem := fUnitCount;
    Inc(fUnitCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fUnitLastAdded;
    Inc(fUnitLastAdded);
    if fUnitLastAdded = CACHE_SIZE then fUnitLastAdded := 0;
  end;

  Result := fPlayers.GetUnitByID(aID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  fUnitCache[NewItem].ID := aID;
  fUnitCache[NewItem].U := Result.GetUnitPointer;
end;


function TKMIDCache.GetHouse(aID:Integer): TKMHouse;
var I, NewItem: Shortint;
begin
  for I:=0 to fHouseCount-1 do
    if fHouseCache[i].ID = aID then
    begin
      Result := fHouseCache[i].H;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  if fHouseCount < CACHE_SIZE then
  begin
    NewItem := fHouseCount;
    Inc(fHouseCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fHouseLastAdded;
    Inc(fHouseLastAdded);
    if fHouseLastAdded = CACHE_SIZE then fHouseLastAdded := 0;
  end;

  Result := fPlayers.GetHouseByID(aID);
  if (Result <> nil) and Result.IsDestroyed then
    Result := nil;

  fHouseCache[NewItem].ID := aID;
  fHouseCache[NewItem].H := Result.GetHousePointer;
end;


function TKMIDCache.GetGroup(aID:Integer): TKMUnitGroup;
var I, NewItem: Shortint;
begin
  for I:=0 to fGroupCount-1 do
    if fGroupCache[i].ID = aID then
    begin
      Result := fGroupCache[i].G;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  if fGroupCount < CACHE_SIZE then
  begin
    NewItem := fGroupCount;
    Inc(fGroupCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fGroupLastAdded;
    Inc(fGroupLastAdded);
    if fGroupLastAdded = CACHE_SIZE then fGroupLastAdded := 0;
  end;

  Result := fPlayers.GetGroupByID(aID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  fGroupCache[NewItem].ID := aID;
  fGroupCache[NewItem].G := Result.GetGroupPointer;
end;


procedure TKMIDCache.UpdateState;
var I: Integer;
begin
  //Clear out dead IDs every now and again
  //Leave them in the cache as nils, because we still might need to lookup that ID
  if fGame.GameTickCount mod 11 = 0 then
  begin
    for I:=0 to fUnitCount-1 do
      if (fUnitCache[i].U <> nil) and fUnitCache[i].U.IsDead then
        fPlayers.CleanUpUnitPointer(fUnitCache[i].U);

    for I:=0 to fHouseCount-1 do
      if (fHouseCache[i].H <> nil) and fHouseCache[i].H.IsDestroyed then
        fPlayers.CleanUpHousePointer(fHouseCache[i].H);

    for I:=0 to fGroupCount-1 do
      if (fGroupCache[i].G <> nil) and fGroupCache[i].G.IsDead then
        fPlayers.CleanUpGroupPointer(fGroupCache[i].G);
  end;
end;


end.
