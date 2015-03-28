unit KM_ScriptingESA;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils, uPSRuntime,
  KM_CommonTypes, KM_Defaults, KM_Points, KM_Houses, KM_ScriptingIdCache, KM_Units,
  KM_UnitGroups, KM_ResHouses, KM_HouseCollection, KM_ResWares;


  //Two classes exposed to scripting States and Actions

  //All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)

  //How to add new a method exposed to the scripting? Three steps:
  //1. Add method to published section here below
  //2. Add method declaration to Compiler (TKMScripting.ScriptOnUses)
  //3. Add method name to Runtime (TKMScripting.LinkRuntime)
type
  TKMScriptEvent = procedure of object;
  TKMScriptEvent1I = procedure (aIndex: Integer) of object;
  TKMScriptEvent2I = procedure (aIndex, aParam: Integer) of object;
  TKMScriptEvent3I = procedure (aIndex, aParam1, aParam2: Integer) of object;
  TKMScriptEvent4I = procedure (aIndex, aParam1, aParam2, aParam3: Integer) of object;

  TScriptErrorType = (se_InvalidParameter, se_Exception, se_CompileError, se_CompileWarning, se_Log);
  TKMScriptErrorEvent = procedure (aType: TScriptErrorType; const aData: UnicodeString) of object;

  TByteSet = set of Byte;

  TKMScriptEntity = class
  protected
    fIDCache: TKMScriptingIdCache;
    fOnScriptError: TKMScriptErrorEvent;
    procedure LogParamWarning(aFuncName: string; const aValues: array of Integer);
  public
    constructor Create(aIDCache: TKMScriptingIdCache);
    property OnScriptError: TKMScriptErrorEvent write fOnScriptError;
  end;

  TKMScriptEvents = class(TKMScriptEntity)
  private
    fExec: TPSExec;

    fProcBeacon: TMethod;
    fProcHouseAfterDestroyed: TMethod;
    fProcHouseBuilt: TMethod;
    fProcHousePlanPlaced: TMethod;
    fProcHousePlanRemoved: TMethod;
    fProcHouseDamaged: TMethod;
    fProcHouseDestroyed: TMethod;
    fProcGroupHungry: TMethod;
    fProcMarketTrade: TMethod;
    fProcMissionStart: TMethod;
    fProcPlanPlacedRoad: TMethod;
    fProcPlanPlacedField: TMethod;
    fProcPlanPlacedWinefield: TMethod;
    fProcPlanRemovedRoad: TMethod;
    fProcPlanRemovedField: TMethod;
    fProcPlanRemovedWinefield: TMethod;
    fProcPlayerDefeated: TMethod;
    fProcPlayerVictory: TMethod;
    fProcTick: TMethod;
    fProcUnitAfterDied: TMethod;
    fProcUnitAttacked: TMethod;
    fProcUnitDied: TMethod;
    fProcUnitTrained: TMethod;
    fProcUnitWounded: TMethod;
    fProcWarriorEquipped: TMethod;

    procedure DoProc(const aProc: TMethod; const aParams: array of Integer);
    function MethodAssigned(aMethod: TMethod): Boolean; inline;
  public
    ExceptionOutsideScript: Boolean; //Flag that the exception occured in a State or Action call not script

    constructor Create(aExec: TPSExec; aIDCache: TKMScriptingIdCache);
    procedure LinkEvents;

    procedure ProcBeacon(aPlayer: TKMHandIndex; aX, aY: Word);
    procedure ProcHouseAfterDestroyed(aHouseType: THouseType; aOwner: TKMHandIndex; aX, aY: Word);
    procedure ProcHouseBuilt(aHouse: TKMHouse);
    procedure ProcHousePlanPlaced(aPlayer: TKMHandIndex; aX, aY: Word; aType: THouseType);
    procedure ProcHousePlanRemoved(aPlayer: TKMHandIndex; aX, aY: Word; aType: THouseType);
    procedure ProcHouseDamaged(aHouse: TKMHouse; aAttacker: TKMUnit);
    procedure ProcHouseDestroyed(aHouse: TKMHouse; aDestroyerIndex: TKMHandIndex);
    procedure ProcGroupHungry(aGroup: TKMUnitGroup);
    procedure ProcMarketTrade(aMarket: TKMHouse; aFrom, aTo: TWareType);
    procedure ProcMissionStart;
    procedure ProcPlanPlaced(aPlayer: TKMHandIndex; aX, aY: Word; aPlanType: TFieldType);
    procedure ProcPlanRemoved(aPlayer: TKMHandIndex; aX, aY: Word; aPlanType: TFieldType);
    procedure ProcPlayerDefeated(aPlayer: TKMHandIndex);
    procedure ProcPlayerVictory(aPlayer: TKMHandIndex);
    procedure ProcTick;
    procedure ProcUnitAfterDied(aUnitType: TUnitType; aOwner: TKMHandIndex; aX, aY: Word);
    procedure ProcUnitAttacked(aUnit, aAttacker: TKMUnit);
    procedure ProcUnitDied(aUnit: TKMUnit; aKillerOwner: TKMHandIndex);
    procedure ProcUnitTrained(aUnit: TKMUnit);
    procedure ProcUnitWounded(aUnit, aAttacker: TKMUnit);
    procedure ProcWarriorEquipped(aUnit: TKMUnit; aGroup: TKMUnitGroup);
  end;

  TKMScriptStates = class(TKMScriptEntity)
  private
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

  TKMScriptActions = class(TKMScriptEntity)
  private
  public
    procedure AIAutoAttackRange(aPlayer: Byte; aRange: Word);
    procedure AIAutoBuild(aPlayer: Byte; aAuto: Boolean);
    procedure AIAutoDefence(aPlayer: Byte; aAuto: Boolean);
    procedure AIAutoRepair(aPlayer: Byte; aAuto: Boolean);
    procedure AIDefencePositionAdd(aPlayer: Byte; X, Y: Integer; aDir, aGroupType: Byte; aRadius: Word; aDefType: Byte);
    procedure AIDefencePositionRemove(aPlayer: Byte; X, Y: Integer);
    procedure AIDefencePositionRemoveAll(aPlayer: Byte);
    procedure AIDefendAllies(aPlayer: Byte; aDefend: Boolean);
    procedure AIEquipRate(aPlayer: Byte; aType: Byte; aRate: Word);
    procedure AIGroupsFormationSet(aPlayer, aType: Byte; aCount, aColumns: Word);
    procedure AIRecruitDelay(aPlayer: Byte; aDelay: Cardinal);
    procedure AIRecruitLimit(aPlayer, aLimit: Byte);
    procedure AISerfsPerHouse(aPlayer: Byte; aSerfs: Single);
    procedure AISoldiersLimit(aPlayer: Byte; aLimit: Integer);
    procedure AIStartPosition(aPlayer: Byte; X, Y: Word);
    procedure AIWorkerLimit(aPlayer, aLimit: Byte);

    procedure CinematicStart(aPlayer: Byte);
    procedure CinematicEnd(aPlayer: Byte);
    procedure CinematicPanTo(aPlayer: Byte; X, Y, Duration: Word);

    function  GiveAnimal(aType, X,Y: Word): Integer;
    function  GiveField(aPlayer, X, Y: Word): Boolean;
    function  GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word): Integer;
    function  GiveHouse(aPlayer, aHouseType, X,Y: Integer): Integer;
    function  GiveHouseSite(aPlayer, aHouseType, X, Y: Integer; aAddMaterials: Boolean): Integer;
    function  GiveUnit(aPlayer, aType, X,Y, aDir: Word): Integer;
    function  GiveRoad(aPlayer, X, Y: Word): Boolean;
    procedure GiveWares(aPlayer, aType, aCount: Word);
    procedure GiveWeapons(aPlayer, aType, aCount: Word);
    function  GiveWinefield(aPlayer, X, Y: Word): Boolean;

    procedure FogCoverAll(aPlayer: Byte);
    procedure FogCoverCircle(aPlayer, X, Y, aRadius: Word);
    procedure FogRevealRect(aPlayer, X1, Y1, X2, Y2: Word);
    procedure FogCoverRect(aPlayer, X1, Y1, X2, Y2: Word);
    procedure FogRevealAll(aPlayer: Byte);
    procedure FogRevealCircle(aPlayer, X, Y, aRadius: Word);

    procedure GroupBlockOrders(aGroupID: Integer; aBlock: Boolean);
    procedure GroupDisableHungryMessage(aGroupID: Integer; aDisable: Boolean);
    procedure GroupHungerSet(aGroupID, aHungerLevel: Integer);
    procedure GroupKillAll(aGroupID: Integer; aSilent: Boolean);
    procedure GroupOrderAttackHouse(aGroupID, aHouseID: Integer);
    procedure GroupOrderAttackUnit(aGroupID, aUnitID: Integer);
    procedure GroupOrderFood(aGroupID: Integer);
    procedure GroupOrderHalt(aGroupID: Integer);
    procedure GroupOrderLink(aGroupID, aDestGroupID: Integer);
    function  GroupOrderSplit(aGroupID: Integer): Integer;
    function  GroupOrderSplitUnit(aGroupID, aUnitID: Integer): Integer;
    procedure GroupOrderStorm(aGroupID: Integer);
    procedure GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word);
    procedure GroupSetFormation(aGroupID: Integer; aNumColumns: Byte);

    procedure HouseAddBuildingMaterials(aHouseID: Integer);
    procedure HouseAddBuildingProgress(aHouseID: Integer);
    procedure HouseAddDamage(aHouseID: Integer; aDamage: Word);
    procedure HouseAddRepair(aHouseID: Integer; aRepair: Word);
    procedure HouseAddWaresTo(aHouseID: Integer; aType, aCount: Word);
    procedure HouseAllow(aPlayer, aHouseType: Word; aAllowed: Boolean);
    function  HouseBarracksEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer;
    procedure HouseBarracksGiveRecruit(aHouseID: Integer);
    procedure HouseDestroy(aHouseID: Integer; aSilent: Boolean);
    procedure HouseDeliveryBlock(aHouseID: Integer; aDeliveryBlocked: Boolean);
    procedure HouseDisableUnoccupiedMessage(aHouseID: Integer; aDisabled: Boolean);
    procedure HouseRepairEnable(aHouseID: Integer; aRepairEnabled: Boolean);
    function  HouseSchoolQueueAdd(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer;
    procedure HouseSchoolQueueRemove(aHouseID, QueueIndex: Integer);
    procedure HouseTakeWaresFrom(aHouseID: Integer; aType, aCount: Word);
    procedure HouseUnlock(aPlayer, aHouseType: Word);
    procedure HouseWoodcutterChopOnly(aHouseID: Integer; aChopOnly: Boolean);
    procedure HouseWareBlock(aHouseID, aWareType: Integer; aBlocked: Boolean);
    procedure HouseWeaponsOrderSet(aHouseID, aWareType, aAmount: Integer);

    procedure Log(aText: AnsiString);

    function MapTileSet(X, Y, aType, aRotation: Integer): Boolean;
    function MapTileHeightSet(X, Y, Height: Integer): Boolean;
    function MapTileObjectSet(X, Y, Obj: Integer): Boolean;

    procedure OverlayTextSet(aPlayer: Shortint; aText: AnsiString);
    procedure OverlayTextSetFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);
    procedure OverlayTextAppend(aPlayer: Shortint; aText: AnsiString);
    procedure OverlayTextAppendFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);

    procedure MarketSetTrade(aMarketID, aFrom, aTo, aAmount: Integer);

    function PlanAddField(aPlayer, X, Y: Word): Boolean;
    function PlanAddHouse(aPlayer, aHouseType, X, Y: Word): Boolean;
    function PlanAddRoad(aPlayer, X, Y: Word): Boolean;
    function PlanAddWinefield(aPlayer, X, Y: Word): Boolean;
    function PlanConnectRoad(aPlayer, X1, Y1, X2, Y2: Integer; aCompleted: Boolean): Boolean;
    function PlanRemove(aPlayer, X, Y: Word): Boolean;

    procedure PlayerAllianceChange(aPlayer1, aPlayer2: Byte; aCompliment, aAllied: Boolean);
    procedure PlayerAddDefaultGoals(aPlayer: Byte; aBuildings: Boolean);
    procedure PlayerDefeat(aPlayer: Word);
    procedure PlayerShareFog(aPlayer1, aPlayer2: Word; aShare: Boolean);
    procedure PlayerWareDistribution(aPlayer, aWareType, aHouseType, aAmount: Byte);
    procedure PlayerWin(const aVictors: array of Integer; aTeamVictory: Boolean);

    procedure PlayWAV(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single);
    procedure PlayWAVFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single);
    procedure PlayWAVAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single; Radius: Single; X, Y: Word);
    function  PlayWAVLooped(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single): Integer;
    function  PlayWAVAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single; Radius: Single; X, Y: Word): Integer;
    procedure StopLoopedWAV(aLoopIndex: Integer);

    procedure RemoveRoad(X, Y: Word);

    procedure SetTradeAllowed(aPlayer, aResType: Word; aAllowed: Boolean);
    procedure ShowMsg(aPlayer: Shortint; aText: AnsiString);
    procedure ShowMsgFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);
    procedure ShowMsgGoto(aPlayer: Shortint; aX, aY: Word; aText: AnsiString);
    procedure ShowMsgGotoFormatted(aPlayer: Shortint; aX, aY: Word; aText: AnsiString; Params: array of const);

    procedure UnitBlock(aPlayer: Byte; aType: Word; aBlock: Boolean);
    function  UnitDirectionSet(aUnitID, aDirection: Integer): Boolean;
    procedure UnitHungerSet(aUnitID, aHungerLevel: Integer);
    procedure UnitKill(aUnitID: Integer; aSilent: Boolean);
    function  UnitOrderWalk(aUnitID: Integer; X, Y: Word): Boolean;
  end;


var
  gScriptEvents: TKMScriptEvents;


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


{ TKMScriptEvents }
constructor TKMScriptEvents.Create(aExec: TPSExec; aIDCache: TKMScriptingIdCache);
begin
  inherited Create(aIDCache);
  fExec := aExec;
end;


procedure TKMScriptEvents.LinkEvents;
begin
  fProcBeacon               := fExec.GetProcAsMethodN('ONBEACON');
  fProcHouseAfterDestroyed  := fExec.GetProcAsMethodN('ONHOUSEAFTERDESTROYED');
  fProcHouseBuilt           := fExec.GetProcAsMethodN('ONHOUSEBUILT');
  fProcHousePlanPlaced      := fExec.GetProcAsMethodN('ONHOUSEPLANPLACED');
  fProcHousePlanRemoved     := fExec.GetProcAsMethodN('ONHOUSEPLANREMOVED');
  fProcHouseDamaged         := fExec.GetProcAsMethodN('ONHOUSEDAMAGED');
  fProcHouseDestroyed       := fExec.GetProcAsMethodN('ONHOUSEDESTROYED');
  fProcGroupHungry          := fExec.GetProcAsMethodN('ONGROUPHUNGRY');
  fProcMarketTrade          := fExec.GetProcAsMethodN('ONMARKETTRADE');
  fProcMissionStart         := fExec.GetProcAsMethodN('ONMISSIONSTART');
  fProcPlanPlacedRoad       := fExec.GetProcAsMethodN('ONPLANROADPLACED');
  fProcPlanPlacedField      := fExec.GetProcAsMethodN('ONPLANFIELDPLACED');
  fProcPlanPlacedWinefield  := fExec.GetProcAsMethodN('ONPLANWINEFIELDPLACED');
  fProcPlanRemovedRoad      := fExec.GetProcAsMethodN('ONPLANROADREMOVED');
  fProcPlanRemovedField     := fExec.GetProcAsMethodN('ONPLANFIELDREMOVED');
  fProcPlanRemovedWinefield := fExec.GetProcAsMethodN('ONPLANWINEFIELDREMOVED');
  fProcPlayerDefeated       := fExec.GetProcAsMethodN('ONPLAYERDEFEATED');
  fProcPlayerVictory        := fExec.GetProcAsMethodN('ONPLAYERVICTORY');
  fProcTick                 := fExec.GetProcAsMethodN('ONTICK');
  fProcUnitAfterDied        := fExec.GetProcAsMethodN('ONUNITAFTERDIED');
  fProcUnitDied             := fExec.GetProcAsMethodN('ONUNITDIED');
  fProcUnitTrained          := fExec.GetProcAsMethodN('ONUNITTRAINED');
  fProcUnitWounded          := fExec.GetProcAsMethodN('ONUNITWOUNDED');
  fProcUnitAttacked         := fExec.GetProcAsMethodN('ONUNITATTACKED');
  fProcWarriorEquipped      := fExec.GetProcAsMethodN('ONWARRIOREQUIPPED');
end;


function TKMScriptEvents.MethodAssigned(aMethod: TMethod): Boolean;
begin
  Result := aMethod.Code <> nil;
end;


//This procedure allows us to keep the exception handling code in one place
procedure TKMScriptEvents.DoProc(const aProc: TMethod; const aParams: array of Integer);
var
  ExceptionProc: TPSProcRec;
  S: UnicodeString;
begin
  try
    case Length(aParams) of
      0: TKMScriptEvent(aProc);
      1: TKMScriptEvent1I(aProc)(aParams[0]);
      2: TKMScriptEvent2I(aProc)(aParams[0], aParams[1]);
      3: TKMScriptEvent3I(aProc)(aParams[0], aParams[1], aParams[2]);
      4: TKMScriptEvent4I(aProc)(aParams[0], aParams[1], aParams[2], aParams[3]);
      else Assert(False);
    end;
  except
    on E: Exception do
      if ExceptionOutsideScript then
      begin
        ExceptionOutsideScript := False; //Reset
        raise; //Exception was in game code not script, so pass up to madExcept
      end
      else
      begin
        S := 'Exception in script: ''' + E.Message + '''';
        ExceptionProc := fExec.GetProcNo(fExec.ExceptionProcNo);
        if ExceptionProc is TPSInternalProcRec then
          S := S + ' in procedure ''' + UnicodeString(TPSInternalProcRec(ExceptionProc).ExportName) + '''';
        fOnScriptError(se_Exception, S);
      end;
  end;
end;


procedure TKMScriptEvents.ProcBeacon(aPlayer: TKMHandIndex; aX, aY: Word);
begin
  if MethodAssigned(fProcBeacon) then
    DoProc(fProcBeacon, [aPlayer, aX, aY]);
end;


procedure TKMScriptEvents.ProcMarketTrade(aMarket: TKMHouse; aFrom, aTo: TWareType);
begin
  if MethodAssigned(fProcMarketTrade) then
  begin
    fIDCache.CacheHouse(aMarket, aMarket.UID); //Improves cache efficiency since aMarket will probably be accessed soon
    DoProc(fProcMarketTrade, [aMarket.UID, WareTypeToIndex[aFrom], WareTypeToIndex[aTo]]);
  end;
end;


procedure TKMScriptEvents.ProcMissionStart;
begin
  if MethodAssigned(fProcMissionStart) then
    DoProc(fProcMissionStart, []);
end;


procedure TKMScriptEvents.ProcTick;
begin
  if MethodAssigned(fProcTick) then
    DoProc(fProcTick, []);
end;


procedure TKMScriptEvents.ProcHouseBuilt(aHouse: TKMHouse);
begin
  if MethodAssigned(fProcHouseBuilt) then
  begin
    fIDCache.CacheHouse(aHouse, aHouse.UID); //Improves cache efficiency since aHouse will probably be accessed soon
    DoProc(fProcHouseBuilt, [aHouse.UID]);
  end;
end;


procedure TKMScriptEvents.ProcHouseDamaged(aHouse: TKMHouse; aAttacker: TKMUnit);
begin
  if MethodAssigned(fProcHouseDamaged) then
  begin
    fIDCache.CacheHouse(aHouse, aHouse.UID); //Improves cache efficiency since aHouse will probably be accessed soon
    if aAttacker <> nil then
    begin
      fIDCache.CacheUnit(aAttacker, aAttacker.UID); //Improves cache efficiency since aAttacker will probably be accessed soon
      DoProc(fProcHouseDamaged, [aHouse.UID, aAttacker.UID]);
    end
    else
      //House was damaged, but we don't know by whom (e.g. by script command)
      DoProc(fProcHouseDamaged, [aHouse.UID, PLAYER_NONE]);
  end;
end;


procedure TKMScriptEvents.ProcHouseDestroyed(aHouse: TKMHouse; aDestroyerIndex: TKMHandIndex);
begin
  if MethodAssigned(fProcHouseDestroyed) then
  begin
    fIDCache.CacheHouse(aHouse, aHouse.UID); //Improves cache efficiency since aHouse will probably be accessed soon
    DoProc(fProcHouseDestroyed, [aHouse.UID, aDestroyerIndex]);
  end;
end;


procedure TKMScriptEvents.ProcHouseAfterDestroyed(aHouseType: THouseType; aOwner: TKMHandIndex; aX, aY: Word);
begin
  if MethodAssigned(fProcHouseAfterDestroyed) then
    DoProc(fProcHouseAfterDestroyed, [HouseTypeToIndex[aHouseType] - 1, aOwner, aX, aY]);
end;


procedure TKMScriptEvents.ProcHousePlanPlaced(aPlayer: TKMHandIndex; aX, aY: Word; aType: THouseType);
begin
  if MethodAssigned(fProcHousePlanPlaced) then
    DoProc(fProcHousePlanPlaced, [aPlayer, aX + gRes.HouseDat[aType].EntranceOffsetX, aY, HouseTypeToIndex[aType] - 1]);
end;


procedure TKMScriptEvents.ProcHousePlanRemoved(aPlayer: TKMHandIndex; aX, aY: Word; aType: THouseType);
begin
  if MethodAssigned(fProcHousePlanRemoved) then
    DoProc(fProcHousePlanRemoved, [aPlayer, aX + gRes.HouseDat[aType].EntranceOffsetX, aY, HouseTypeToIndex[aType] - 1]);
end;


procedure TKMScriptEvents.ProcGroupHungry(aGroup: TKMUnitGroup);
begin
  if MethodAssigned(fProcGroupHungry) then
  begin
    fIDCache.CacheGroup(aGroup, aGroup.UID); //Improves cache efficiency since aGroup will probably be accessed soon
    DoProc(fProcGroupHungry, [aGroup.UID]);
  end;
end;


procedure TKMScriptEvents.ProcUnitDied(aUnit: TKMUnit; aKillerOwner: TKMHandIndex);
begin
  if MethodAssigned(fProcUnitDied) then
  begin
    fIDCache.CacheUnit(aUnit, aUnit.UID); //Improves cache efficiency since aUnit will probably be accessed soon
    DoProc(fProcUnitDied, [aUnit.UID, aKillerOwner]);
  end;
end;


procedure TKMScriptEvents.ProcUnitAfterDied(aUnitType: TUnitType; aOwner: TKMHandIndex; aX, aY: Word);
begin
  if MethodAssigned(fProcUnitAfterDied) then
    DoProc(fProcUnitAfterDied, [UnitTypeToIndex[aUnitType], aOwner, aX, aY]);
end;


procedure TKMScriptEvents.ProcUnitAttacked(aUnit, aAttacker: TKMUnit);
begin
  if MethodAssigned(fProcUnitAttacked) then
  begin
    fIDCache.CacheUnit(aUnit, aUnit.UID); //Improves cache efficiency since aUnit will probably be accessed soon
    if aAttacker <> nil then
    begin
      fIDCache.CacheUnit(aAttacker, aAttacker.UID); //Improves cache efficiency since aAttacker will probably be accessed soon
      DoProc(fProcUnitAttacked, [aUnit.UID, aAttacker.UID]);
    end
    else
      DoProc(fProcUnitAttacked, [aUnit.UID, -1]);
  end;
end;


procedure TKMScriptEvents.ProcUnitTrained(aUnit: TKMUnit);
begin
  if MethodAssigned(fProcUnitTrained) then
  begin
    fIDCache.CacheUnit(aUnit, aUnit.UID); //Improves cache efficiency since aUnit will probably be accessed soon
    DoProc(fProcUnitTrained, [aUnit.UID]);
  end;
end;


procedure TKMScriptEvents.ProcUnitWounded(aUnit, aAttacker: TKMUnit);
begin
  if MethodAssigned(fProcUnitWounded) then
  begin
    fIDCache.CacheUnit(aUnit, aUnit.UID); //Improves cache efficiency since aUnit will probably be accessed soon
    if aAttacker <> nil then
    begin
      fIDCache.CacheUnit(aAttacker, aAttacker.UID); //Improves cache efficiency since aAttacker will probably be accessed soon
      DoProc(fProcUnitWounded, [aUnit.UID, aAttacker.UID]);
    end
    else
      DoProc(fProcUnitWounded, [aUnit.UID, -1]);
  end;
end;


procedure TKMScriptEvents.ProcWarriorEquipped(aUnit: TKMUnit; aGroup: TKMUnitGroup);
begin
  if MethodAssigned(fProcWarriorEquipped) then
  begin
    fIDCache.CacheUnit(aUnit, aUnit.UID); //Improves cache efficiency since aUnit will probably be accessed soon
    fIDCache.CacheGroup(aGroup, aGroup.UID);
    DoProc(fProcWarriorEquipped, [aUnit.UID, aGroup.UID]);
  end;
end;


procedure TKMScriptEvents.ProcPlanPlaced(aPlayer: TKMHandIndex; aX, aY: Word; aPlanType: TFieldType);
begin
  case aPlanType of
    ft_Road: if MethodAssigned(fProcPlanPlacedRoad)      then DoProc(fProcPlanPlacedRoad     , [aPlayer, aX, aY]);
    ft_Corn: if MethodAssigned(fProcPlanPlacedField)     then DoProc(fProcPlanPlacedField    , [aPlayer, aX, aY]);
    ft_Wine: if MethodAssigned(fProcPlanPlacedWinefield) then DoProc(fProcPlanPlacedWinefield, [aPlayer, aX, aY]);
    else     Assert(False);
  end;
end;


procedure TKMScriptEvents.ProcPlanRemoved(aPlayer: TKMHandIndex; aX, aY: Word; aPlanType: TFieldType);
begin
  case aPlanType of
    ft_Road: if MethodAssigned(fProcPlanRemovedRoad)      then DoProc(fProcPlanRemovedRoad     , [aPlayer, aX, aY]);
    ft_Corn: if MethodAssigned(fProcPlanRemovedField)     then DoProc(fProcPlanRemovedField    , [aPlayer, aX, aY]);
    ft_Wine: if MethodAssigned(fProcPlanRemovedWinefield) then DoProc(fProcPlanRemovedWinefield, [aPlayer, aX, aY]);
    else     Assert(False);
  end;
end;


procedure TKMScriptEvents.ProcPlayerDefeated(aPlayer: TKMHandIndex);
begin
  if MethodAssigned(fProcPlayerDefeated) then
    DoProc(fProcPlayerDefeated, [aPlayer]);
end;


procedure TKMScriptEvents.ProcPlayerVictory(aPlayer: TKMHandIndex);
begin
  if MethodAssigned(fProcPlayerVictory) then
    DoProc(fProcPlayerVictory, [aPlayer]);
end;


{ TKMScriptEntity }
constructor TKMScriptEntity.Create(aIDCache: TKMScriptingIdCache);
begin
  inherited Create;
  fIDCache := aIDCache;
end;


procedure TKMScriptEntity.LogParamWarning(aFuncName: string; const aValues: array of Integer);
var
  I: Integer;
  Values: string;
begin
  Values := '';
  for I := Low(aValues) to High(aValues) do
    Values := Values + IntToStr(aValues[I]) + IfThen(I<>High(aValues), ', ');
  fOnScriptError(se_InvalidParameter, 'Invalid parameter(s) passed to ' + aFuncName + ': ' + Values);
end;


{ TKMScriptStates }
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
      Result := gHands[aPlayer].PlayerType = hndComputer
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
      Result := gHands[aPlayer].Stats.GetCanBuild(HouseIndexToType[aHouseType])
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


{ TKMScriptActions }
procedure TKMScriptActions.CinematicStart(aPlayer: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin
      gHands[aPlayer].InCinematic := True;
      gGame.GamePlayInterface.CinematicUpdate;
    end
    else
      LogParamWarning('Actions.CinematicStart', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.CinematicEnd(aPlayer: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin
      gHands[aPlayer].InCinematic := False;
      gGame.GamePlayInterface.CinematicUpdate;
    end
    else
      LogParamWarning('Actions.CinematicEnd', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.CinematicPanTo(aPlayer: Byte; X, Y, Duration: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y)
    and gHands[aPlayer].InCinematic then
    begin
      if aPlayer = MySpectator.HandIndex then
        //Duration is in ticks (1/10 sec), viewport wants miliseconds (1/1000 sec)
        gGame.GamePlayInterface.Viewport.PanTo(KMPointF(X, Y), Duration*100);
    end
    else
      LogParamWarning('Actions.CinematicPanTo', [aPlayer, X, Y, Duration]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayerDefeat(aPlayer: Word);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Defeat
    else
      LogParamWarning('Actions.PlayerDefeat', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayerShareFog(aPlayer1, aPlayer2: Word; aShare: Boolean);
begin
  try
    if  InRange(aPlayer1, 0, gHands.Count - 1)
    and InRange(aPlayer2, 0, gHands.Count - 1)
    and (gHands[aPlayer1].Enabled)
    and (gHands[aPlayer2].Enabled) then
      gHands[aPlayer1].ShareFOW[aPlayer2] := aShare
    else
      LogParamWarning('Actions.PlayerShareFog', [aPlayer1, aPlayer2, Byte(aShare)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Sets all player IDs in aVictors to victorious, and all their team members if aTeamVictory is true.
//All other players are set to defeated.
procedure TKMScriptActions.PlayerWin(const aVictors: array of Integer; aTeamVictory: Boolean);
var
  I, K: Integer;
begin
  try
    //Verify all input parameters
    for I := 0 to Length(aVictors) - 1 do
    if not InRange(aVictors[I], 0, gHands.Count - 1) then
    begin
      LogParamWarning('Actions.PlayerWin', [aVictors[I]]);
      Exit;
    end;

    for I := 0 to Length(aVictors) - 1 do
      if gHands[aVictors[I]].Enabled then
      begin
        gHands[aVictors[I]].AI.Victory;
        if aTeamVictory then
          for K := 0 to gHands.Count - 1 do
            if gHands[K].Enabled and (I <> K) and (gHands[aVictors[I]].Alliances[K] = at_Ally) then
              gHands[K].AI.Victory;
      end;

    //All other players get defeated
    for I := 0 to gHands.Count - 1 do
      if gHands[I].Enabled and (gHands[I].AI.WonOrLost = wol_None) then
        gHands[I].AI.Defeat;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayerWareDistribution(aPlayer, aWareType, aHouseType, aAmount: Byte);
begin
  try
    if (aWareType in [Low(WareIndexToType) .. High(WareIndexToType)])
    and (WareIndexToType[aWareType] in [wt_Steel, wt_Coal, wt_Wood, wt_Corn])
    and HouseTypeValid(aHouseType)
    and InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and InRange(aAmount, 0, 5) then
    begin
      gHands[aPlayer].Stats.Ratio[WareIndexToType[aWareType], HouseIndexToType[aHouseType]] := aAmount;
      gHands[aPlayer].Houses.UpdateResRequest;
    end
    else
      LogParamWarning('Actions.PlayerWareDistribution', [aPlayer, aWareType, aHouseType, aAmount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayerAllianceChange(aPlayer1, aPlayer2: Byte; aCompliment, aAllied: Boolean);
const
  ALLIED: array [Boolean] of TAllianceType = (at_Enemy, at_Ally);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer1, 0, gHands.Count - 1)
    and InRange(aPlayer2, 0, gHands.Count - 1)
    and (aPlayer1 <> aPlayer2)
    and (gHands[aPlayer1].Enabled)
    and (gHands[aPlayer2].Enabled) then
    begin
      gHands[aPlayer1].Alliances[aPlayer2] := ALLIED[aAllied];
      if aAllied then
        gHands[aPlayer2].FogOfWar.SyncFOW(gHands[aPlayer1].FogOfWar);
      if aCompliment then
      begin
        gHands[aPlayer2].Alliances[aPlayer1] := ALLIED[aAllied];
        if aAllied then
          gHands[aPlayer1].FogOfWar.SyncFOW(gHands[aPlayer2].FogOfWar);
      end;
    end
    else
      LogParamWarning('Actions.PlayerAllianceChange', [aPlayer1, aPlayer2, Byte(aCompliment), Byte(aAllied)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayerAddDefaultGoals(aPlayer: Byte; aBuildings: Boolean);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
    begin

      gHands[aPlayer].AI.AddDefaultGoals(aBuildings);
    end
    else
      LogParamWarning('Actions.PlayerAddDefaultGoals', [aPlayer, Byte(aBuildings)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayWAV(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single);
var
  fullFileName: UnicodeString;
begin
  try
    if (aPlayer <> MySpectator.HandIndex) and (aPlayer <> PLAYER_NONE) then Exit;

    fullFileName := ExeDir + gGame.GetScriptSoundFile(aFileName);
    //Silently ignore missing files (player might choose to delete annoying sounds from scripts if he likes)
    if not FileExists(fullFileName) then Exit;
    if InRange(Volume, 0, 1) then
      gSoundPlayer.PlayWAVFromScript(fullFileName, KMPoint(0,0), False, Volume, 0, False)
    else
      LogParamWarning('Actions.PlayWAV: ' + UnicodeString(aFileName), []);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayWAVFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single);
var
  fullFileName: UnicodeString;
begin
  try
    if (aPlayer <> MySpectator.HandIndex) and (aPlayer <> PLAYER_NONE) then Exit;

    fullFileName := ExeDir + gGame.GetScriptSoundFile(aFileName);
    //Silently ignore missing files (player might choose to delete annoying sounds from scripts if he likes)
    if not FileExists(fullFileName) then Exit;
    if InRange(Volume, 0, 1) then
      gSoundPlayer.PlayWAVFromScript(fullFileName, KMPoint(0,0), False, Volume, 0, True)
    else
      LogParamWarning('Actions.PlayWAVFadeMusic: ' + UnicodeString(aFileName), []);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.PlayWAVAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single; Radius: Single; X, Y: Word);
var
  fullFileName: UnicodeString;
begin
  try
    if (aPlayer <> MySpectator.HandIndex) and (aPlayer <> PLAYER_NONE) then Exit;

    fullFileName := ExeDir + gGame.GetScriptSoundFile(aFileName);
    //Silently ignore missing files (player might choose to delete annoying sounds from scripts if he likes)
    if not FileExists(fullFileName) then Exit;
    if InRange(Volume, 0, 4) and (Radius >= 28) and gTerrain.TileInMapCoords(X,Y) then
    begin
      if MySpectator.FogOfWar.CheckTileRevelation(X, Y) > 0 then
        gSoundPlayer.PlayWAVFromScript(fullFileName, KMPoint(X,Y), True, Volume, Radius, False);
    end
    else
      LogParamWarning('Actions.PlayWAVAtLocation: ' + UnicodeString(aFileName), [X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlayWAVLooped(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single): Integer;
begin
  try
    Result := -1;
    if InRange(Volume, 0, 1) then
      Result := gLoopSounds.AddLoopSound(aPlayer, aFileName, KMPoint(0,0), False, Volume, 0)
    else
      LogParamWarning('Actions.PlayWAVLooped: ' + UnicodeString(aFileName), []);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlayWAVAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; Volume: Single; Radius: Single; X, Y: Word): Integer;
begin
  try
    Result := -1;
    if InRange(Volume, 0, 4) and (Radius >= 28) and gTerrain.TileInMapCoords(X,Y) then
      Result := gLoopSounds.AddLoopSound(aPlayer, aFileName, KMPoint(X,Y), True, Volume, Radius)
    else
      LogParamWarning('Actions.PlayWAVAtLocationLooped: ' + UnicodeString(aFileName), [X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.StopLoopedWAV(aLoopIndex: Integer);
begin
  try
    gLoopSounds.RemoveLoopSound(aLoopIndex);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.RemoveRoad(X, Y: Word);
var
  Pos: TKMPoint;
begin
  try
    Pos := KMPoint(X, Y);
    if gTerrain.TileInMapCoords(X, Y) then
    begin
      //Can't remove if tile is locked (house or roadwork)
      if (gTerrain.Land[Y, X].TileOverlay = to_Road) and (gTerrain.Land[Y, X].TileLock = tlNone) then
        gTerrain.RemRoad(Pos);
    end
    else
      LogParamWarning('Actions.RemoveRoad', [X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word): Integer;
var
  G: TKMUnitGroup;
begin
  try
    Result := UID_NONE;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aType in [UnitTypeToIndex[WARRIOR_MIN]..UnitTypeToIndex[WARRIOR_MAX]])
    and gTerrain.TileInMapCoords(X,Y)
    and (TKMDirection(aDir+1) in [dir_N..dir_NW])
    and (aCount > 0)
    and (aColumns > 0) then
    begin
      G := gHands[aPlayer].AddUnitGroup(UnitIndexToType[aType],
                                          KMPoint(X,Y),
                                          TKMDirection(aDir+1),
                                          aColumns,
                                          aCount);
      if G = nil then Exit;
      Result := G.UID;
    end
    else
      LogParamWarning('Actions.GiveGroup', [aPlayer, aType, X, Y, aDir, aCount, aColumns]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveUnit(aPlayer, aType, X, Y, aDir: Word): Integer;
var
  U: TKMUnit;
begin
  try
    Result := UID_NONE;

    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aType in [UnitTypeToIndex[CITIZEN_MIN] .. UnitTypeToIndex[CITIZEN_MAX]])
    and gTerrain.TileInMapCoords(X, Y)
    and (TKMDirection(aDir + 1) in [dir_N .. dir_NW]) then
    begin
      U := gHands[aPlayer].AddUnit(UnitIndexToType[aType], KMPoint(X,Y));
      if U = nil then Exit;
      Result := U.UID;
      U.Direction := TKMDirection(aDir + 1);
      //Make sure the unit is not locked so the script can use commands like UnitOrderWalk.
      //By default newly created units are given SetActionLockedStay
      U.SetActionStay(10, ua_Walk);
    end
    else
      LogParamWarning('Actions.GiveUnit', [aPlayer, aType, X, Y, aDir]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveHouse(aPlayer, aHouseType, X,Y: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := UID_NONE;

    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType)
    and gTerrain.TileInMapCoords(X, Y) then
    begin
      if gTerrain.CanPlaceHouseFromScript(HouseIndexToType[aHouseType], KMPoint(X - gRes.HouseDat[HouseIndexToType[aHouseType]].EntranceOffsetX, Y)) then
      begin
        H := gHands[aPlayer].AddHouse(HouseIndexToType[aHouseType], X, Y, True);
        if H = nil then Exit;
        Result := H.UID;
      end;
    end
    else
      LogParamWarning('Actions.GiveHouse', [aPlayer, aHouseType, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveHouseSite(aPlayer, aHouseType, X, Y: Integer; aAddMaterials: Boolean): Integer;
var
  H: TKMHouse;
  I, K: Integer;
  HA: THouseArea;
  NonEntranceX: Integer;
begin
  try
    Result := -1;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      NonEntranceX := X - gRes.HouseDat[HouseIndexToType[aHouseType]].EntranceOffsetX;
      if gTerrain.CanPlaceHouseFromScript(HouseIndexToType[aHouseType], KMPoint(NonEntranceX, Y)) then
      begin
        H := gHands[aPlayer].AddHouseWIP(HouseIndexToType[aHouseType], KMPoint(NonEntranceX, Y));
        if (H = nil) or (H.IsDestroyed) then
          Exit;

        Result := H.UID;
        HA := gRes.HouseDat[H.HouseType].BuildArea;
        for I := 1 to 4 do
        for K := 1 to 4 do
          if HA[I, K] <> 0 then
          begin
            gTerrain.RemoveObject(KMPoint(NonEntranceX + K - 3, Y + I - 4));
            gTerrain.FlattenTerrain(KMPoint(NonEntranceX + K - 3, Y + I - 4));
            gTerrain.SetTileLock(KMPoint(NonEntranceX + K - 3, Y + I - 4), tlDigged);
          end;

        gTerrain.SetField(H.GetEntrance, aPlayer, ft_Road);
        H.BuildingState := hbs_Wood;
        if aAddMaterials then
        begin
          for I := 0 to gRes.HouseDat[H.HouseType].WoodCost - 1 do
            H.ResAddToBuild(wt_Wood);
          for K := 0 to gRes.HouseDat[H.HouseType].StoneCost - 1 do
            H.ResAddToBuild(wt_Stone);
        end
        else
        begin
          gHands[aPlayer].Deliveries.Queue.AddDemand(H, nil, wt_Wood, gRes.HouseDat[H.HouseType].WoodCost, dt_Once, diHigh4);
          gHands[aPlayer].Deliveries.Queue.AddDemand(H, nil, wt_Stone, gRes.HouseDat[H.HouseType].StoneCost, dt_Once, diHigh4);
        end;
        gHands[aPlayer].BuildList.HouseList.AddHouse(H);
      end;
    end
    else
      LogParamWarning('Actions.GiveHouseSite', [aPlayer, aHouseType, X, Y, byte(aAddMaterials)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIAutoAttackRange(aPlayer: Byte; aRange: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and InRange(aRange, 1, 20) then
      gHands[aPlayer].AI.Setup.AutoAttackRange := aRange
    else
      LogParamWarning('Actions.AIAutoAttackRange', [aPlayer, aRange]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIAutoBuild(aPlayer: Byte; aAuto: Boolean);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.AutoBuild := aAuto
    else
      LogParamWarning('Actions.AIAutoBuild', [aPlayer, Byte(aAuto)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIAutoDefence(aPlayer: Byte; aAuto: Boolean);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.AutoDefend := aAuto
    else
      LogParamWarning('Actions.AIAutoDefence', [aPlayer, Byte(aAuto)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIAutoRepair(aPlayer: Byte; aAuto: Boolean);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
       gHands[aPlayer].AI.Setup.AutoRepair := aAuto
     else
       LogParamWarning('Actions.AIAutoRepair', [aPlayer, Byte(aAuto)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIDefencePositionAdd(aPlayer: Byte; X, Y: Integer; aDir, aGroupType: Byte; aRadius: Word; aDefType: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (TAIDefencePosType(aDefType) in [adt_FrontLine..adt_BackLine])
    and (TGroupType(aGroupType) in [gt_Melee..gt_Mounted])
    and (TKMDirection(aDir+1) in [dir_N..dir_NW])
    and (gTerrain.TileInMapCoords(X, Y)) then
      gHands[aPlayer].AI.General.DefencePositions.Add(KMPointDir(X, Y, TKMDirection(aDir + 1)), TGroupType(aGroupType), aRadius, TAIDefencePosType(aDefType))
  else
    LogParamWarning('Actions.AIDefencePositionAdd', [aPlayer, X, Y, aDir, aGroupType, aRadius, aDefType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIDefencePositionRemove(aPlayer: Byte; X, Y: Integer);
var
  I: Integer;
  DP: TAIDefencePosition;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
      for I := gHands[aPlayer].AI.General.DefencePositions.Count - 1 downto 0 do
      begin
        DP := gHands[aPlayer].AI.General.DefencePositions.Positions[I];
        if DP <> nil then
          if (DP.Position.Loc.X = X)
          and (DP.Position.Loc.Y = Y) then
            gHands[aPlayer].AI.General.DefencePositions.Delete(I);
      end
  else
    LogParamWarning('Actions.AIDefencePositionRemove', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIDefencePositionRemoveAll(aPlayer: Byte);
var
  I: Integer;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled) then
      for I := gHands[aPlayer].AI.General.DefencePositions.Count - 1 downto 0 do
        gHands[aPlayer].AI.General.DefencePositions.Delete(I)
    else
      LogParamWarning('Actions.AIDefencePositionRemoveAll', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIDefendAllies(aPlayer: Byte; aDefend: Boolean);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.DefendAllies := aDefend
    else
      LogParamWarning('Actions.AIDefendAllies', [aPlayer, Byte(aDefend)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIEquipRate(aPlayer: Byte; aType: Byte; aRate: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      case aType of
        0:    gHands[aPlayer].AI.Setup.EquipRateLeather := aRate;
        1:    gHands[aPlayer].AI.Setup.EquipRateIron := aRate;
        else  LogParamWarning('Actions.AIEquipRate, unknown type', [aPlayer, aType, aRate]);
      end
    else
      LogParamWarning('Actions.AIEquipRate', [aPlayer, aType, aRate]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIGroupsFormationSet(aPlayer, aType: Byte; aCount, aColumns: Word);
var
  gt: TGroupType;
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and InRange(aType, 0, 3)
    and (aCount > 0) and (aColumns > 0) then
    begin
      gt := TGroupType(aType);
      gHands[aPlayer].AI.General.DefencePositions.TroopFormations[gt].NumUnits := aCount;
      gHands[aPlayer].AI.General.DefencePositions.TroopFormations[gt].UnitsPerRow := aColumns;
    end
    else
      LogParamWarning('Actions.AIGroupsFormationSet', [aPlayer, aType, aCount, aColumns]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIRecruitDelay(aPlayer: Byte; aDelay: Cardinal);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.RecruitDelay := aDelay
    else
      LogParamWarning('Actions.AIRecruitDelay', [aPlayer, aDelay]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIRecruitLimit(aPlayer, aLimit: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.RecruitCount := aLimit
    else
      LogParamWarning('Actions.AIRecruitLimit', [aPlayer, aLimit]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AISerfsPerHouse(aPlayer: Byte; aSerfs: Single);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.SerfsPerHouse := aSerfs
    else
      LogParamWarning('Actions.AISerfsPerHouse', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AISoldiersLimit(aPlayer: Byte; aLimit: Integer);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aLimit >= -1) then                       //-1 means unlimited; else MaxSoldiers = aLimit
      gHands[aPlayer].AI.Setup.MaxSoldiers := aLimit
    else
      LogParamWarning('Actions.AISoldiersLimit', [aPlayer, aLimit]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIStartPosition(aPlayer: Byte; X, Y: Word);
begin
  try
    if (InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled))
    and (gTerrain.TileInMapCoords(X, Y)) then
      gHands[aPlayer].AI.Setup.StartPosition := KMPoint(X, Y)
    else
      LogParamWarning('Actions.AIStartPosition', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.AIWorkerLimit(aPlayer, aLimit: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].AI.Setup.WorkerCount := aLimit
    else
      LogParamWarning('Actions.AIWorkerLimit', [aPlayer, aLimit]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveAnimal(aType, X, Y: Word): Integer;
var
  U: TKMUnit;
begin
  try
    Result := UID_NONE;

    //Verify all input parameters
    if (aType in [UnitTypeToIndex[ANIMAL_MIN] .. UnitTypeToIndex[ANIMAL_MAX]])
    and gTerrain.TileInMapCoords(X, Y) then
    begin
      U := gHands.PlayerAnimals.AddUnit(UnitIndexToType[aType], KMPoint(X,Y));
      if U <> nil then
        Result := U.UID;
    end
    else
      LogParamWarning('Actions.GiveAnimal', [aType, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveField(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Corn) then
      begin
        Result := True;
        gTerrain.SetField(KMPoint(X, Y), aPlayer, ft_Corn);
      end
    else
      LogParamWarning('Actions.GiveField', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveRoad(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Road) then
      begin
        Result := True;
        gTerrain.SetField(KMPoint(X, Y), aPlayer, ft_Road);
        //Terrain under roads is flattened (fields are not)
        gTerrain.FlattenTerrain(KMPoint(X, Y));
      end
    else
      LogParamWarning('Actions.GiveRoad', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Wares are added to first Store
procedure TKMScriptActions.GiveWares(aPlayer, aType, aCount: Word);
var
  H: TKMHouse;
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and InRange(aCount, 0, High(Word))
    and (aType in [Low(WareIndexToType) .. High(WareIndexToType)]) then
    begin
      H := gHands[aPlayer].FindHouse(ht_Store, 1);
      if H <> nil then
      begin
        H.ResAddToIn(WareIndexToType[aType], aCount);
        gHands[aPlayer].Stats.WareProduced(WareIndexToType[aType], aCount);
      end;
    end
    else
      LogParamWarning('Actions.GiveWares', [aPlayer, aType, aCount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Weapons are added to first Barracks
procedure TKMScriptActions.GiveWeapons(aPlayer, aType, aCount: Word);
var
  H: TKMHouse;
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and InRange(aCount, 0, High(Word))
    and (aType in [Low(WareIndexToType) .. High(WareIndexToType)])
    and (WareIndexToType[aType] in [WARFARE_MIN .. WARFARE_MAX]) then
    begin
      H := gHands[aPlayer].FindHouse(ht_Barracks, 1);
      if H <> nil then
      begin
        H.ResAddToIn(WareIndexToType[aType], aCount);
        gHands[aPlayer].Stats.WareProduced(WareIndexToType[aType], aCount);
      end;
    end
    else
      LogParamWarning('Actions.GiveWeapons', [aPlayer, aType, aCount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GiveWineField(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X, Y) then
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Wine) then
      begin
        Result := True;
        gTerrain.SetField(KMPoint(X, Y), aPlayer, ft_Wine);
      end
    else
      LogParamWarning('Actions.GiveWineField', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogRevealCircle(aPlayer, X, Y, aRadius: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y)
    and InRange(aRadius, 0, 255) then
      gHands[aPlayer].FogOfWar.RevealCircle(KMPoint(X, Y), aRadius, FOG_OF_WAR_MAX)
    else
      LogParamWarning('Actions.FogRevealCircle', [aPlayer, X, Y, aRadius]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogCoverCircle(aPlayer, X, Y, aRadius: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y)
    and InRange(aRadius, 0, 255) then
      gHands[aPlayer].FogOfWar.CoverCircle(KMPoint(X, Y), aRadius)
    else
      LogParamWarning('Actions.FogCoverCircle', [aPlayer, X, Y, aRadius]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogRevealRect(aPlayer, X1, Y1, X2, Y2: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X1,Y1)
    and gTerrain.TileInMapCoords(X2,Y2) then
      gHands[aPlayer].FogOfWar.RevealRect(KMPoint(X1, Y1), KMPoint(X2, Y2), FOG_OF_WAR_MAX)
    else
      LogParamWarning('Actions.FogRevealRect', [aPlayer, X1, Y1, X2, Y2]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogCoverRect(aPlayer, X1, Y1, X2, Y2: Word);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X1,Y1)
    and gTerrain.TileInMapCoords(X2,Y2) then
      gHands[aPlayer].FogOfWar.CoverRect(KMPoint(X1, Y1), KMPoint(X2, Y2))
    else
      LogParamWarning('Actions.FogCoverRect', [aPlayer, X1, Y1, X2, Y2]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogRevealAll(aPlayer: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].FogOfWar.RevealEverything
    else
      LogParamWarning('Actions.FogRevealAll', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.FogCoverAll(aPlayer: Byte);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled) then
      gHands[aPlayer].FogOfWar.CoverEverything
    else
      LogParamWarning('Actions.FogCoverAll', [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Input text is ANSI with libx codes to substitute
procedure TKMScriptActions.ShowMsg(aPlayer: Shortint; aText: AnsiString);
begin
  try
    if (aPlayer = MySpectator.HandIndex) or (aPlayer = PLAYER_NONE) then
      gGame.ShowMessageLocal(mkText, UnicodeString(aText), KMPoint(0,0));
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Input text is ANSI with libx codes to substitute
procedure TKMScriptActions.ShowMsgFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);
begin
  try
    try
      if (aPlayer = MySpectator.HandIndex) or (aPlayer = PLAYER_NONE) then
        gGame.ShowMessageLocalFormatted(mkText, UnicodeString(aText), KMPoint(0,0), Params);
    except
      //Format may throw an exception
      on E: EConvertError do LogParamWarning('Actions.ShowMsgFormatted: '+E.Message, []);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Input text is ANSI with libx codes to substitute
procedure TKMScriptActions.ShowMsgGoto(aPlayer: Shortint; aX, aY: Word; aText: AnsiString);
begin
  try
    if gTerrain.TileInMapCoords(aX, aY) then
    begin
      if (aPlayer = MySpectator.HandIndex) or (aPlayer = PLAYER_NONE) then
        gGame.ShowMessageLocal(mkText, UnicodeString(aText), KMPoint(aX,aY));
    end
    else
      LogParamWarning('Actions.ShowMsgGoto', [aPlayer, aX, aY]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


//Input text is ANSI with libx codes to substitute
procedure TKMScriptActions.ShowMsgGotoFormatted(aPlayer: Shortint; aX, aY: Word; aText: AnsiString; Params: array of const);
begin
  try
    try
      if gTerrain.TileInMapCoords(aX, aY) then
      begin
        if (aPlayer = MySpectator.HandIndex) or (aPlayer = PLAYER_NONE) then
          gGame.ShowMessageLocalFormatted(mkText, UnicodeString(aText), KMPoint(aX,aY), Params);
      end
      else
        LogParamWarning('Actions.ShowMsgGotoFormatted', [aPlayer, aX, aY]);
    except
      //Format may throw an exception
      on E: EConvertError do LogParamWarning('Actions.ShowMsgGotoFormatted: '+E.Message, []);
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseUnlock(aPlayer, aHouseType: Word);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType) then
      gHands[aPlayer].Stats.HouseGranted[HouseIndexToType[aHouseType]] := True
    else
      LogParamWarning('Actions.HouseUnlock', [aPlayer, aHouseType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAllow(aPlayer, aHouseType: Word; aAllowed: Boolean);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType) then
      gHands[aPlayer].Stats.HouseBlocked[HouseIndexToType[aHouseType]] := not aAllowed
    else
      LogParamWarning('Actions.HouseAllow', [aPlayer, aHouseType, Byte(aAllowed)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.SetTradeAllowed(aPlayer, aResType: Word; aAllowed: Boolean);
begin
  try
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aResType in [Low(WareIndexToType)..High(WareIndexToType)]) then
      gHands[aPlayer].Stats.AllowToTrade[WareIndexToType[aResType]] := aAllowed
    else
      LogParamWarning('Actions.SetTradeAllowed', [aPlayer, aResType, Byte(aAllowed)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAddBuildingMaterials(aHouseID: Integer);
var
  I, StoneNeeded, WoodNeeded: Integer;
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        if not H.IsComplete then
        begin
          StoneNeeded := gHands[H.Owner].Deliveries.Queue.TryRemoveDemand(H, wt_Stone, gRes.HouseDat[H.HouseType].StoneCost - H.GetBuildStoneDelivered);
          WoodNeeded := gHands[H.Owner].Deliveries.Queue.TryRemoveDemand(H, wt_Wood, gRes.HouseDat[H.HouseType].WoodCost - H.GetBuildWoodDelivered);
          for I := 0 to WoodNeeded - 1 do
            H.ResAddToBuild(wt_Wood);
          for I := 0 to StoneNeeded - 1 do
            H.ResAddToBuild(wt_Stone);
        end;
    end
    else
      LogParamWarning('Actions.HouseAddBuildingMaterials', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAddBuildingProgress(aHouseID: Integer);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        if (not H.IsComplete)
        and (H.CheckResToBuild) then
        begin
          H.IncBuildingProgress;
          if H.IsStone
          and (gTerrain.Land[H.GetPosition.Y, H.GetPosition.X].TileLock <> tlHouse) then
            gTerrain.SetHouse(H.GetPosition, H.HouseType, hsBuilt, H.Owner);
        end;
    end
    else
      LogParamWarning('Actions.HouseAddBuildingProgress', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAddDamage(aHouseID: Integer; aDamage: Word);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        H.AddDamage(aDamage, nil); //We don't know who did the damage
    end
    else
      LogParamWarning('Actions.HouseAddDamage', [aHouseID, aDamage]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAddRepair(aHouseID: Integer; aRepair: Word);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        H.AddRepair(aRepair);
    end
    else
      LogParamWarning('Actions.HouseAddRepair', [aHouseID, aRepair]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseDestroy(aHouseID: Integer; aSilent: Boolean);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        H.DemolishHouse(PLAYER_NONE, aSilent);
    end
    else
      LogParamWarning('Actions.HouseDestroy', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseAddWaresTo(aHouseID: Integer; aType, aCount: Word);
var
  H: TKMHouse;
  Res: TWareType;
begin
  try
    if (aHouseID > 0) and (aType in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aType];
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        if H.ResCanAddToIn(Res) or H.ResCanAddToOut(Res) then
        begin
          if aCount > 0 then
          begin
            H.ResAddToEitherFromScript(Res, aCount);
            gHands[H.Owner].Stats.WareProduced(Res, aCount);
          end;
        end
        else
          LogParamWarning('Actions.HouseAddWaresTo wrong ware type', [aHouseID, aType, aCount]);
      //Silently ignore if house doesn't exist
    end
    else
      LogParamWarning('Actions.HouseAddWaresTo', [aHouseID, aType, aCount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseTakeWaresFrom(aHouseID: Integer; aType, aCount: Word);
var
  H: TKMHouse;
  Res: TWareType;
begin
  try
    if (aHouseID > 0) and (aType in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aType];
      H := fIDCache.GetHouse(aHouseID);
      if H <> nil then
        //Store/barracks mix input/output (add to input, take from output) so we must process them together
        if H.ResCanAddToIn(Res) or H.ResCanAddToOut(Res) then
        begin
          if aCount > 0 then
          begin
            //Range checking is done within ResTakeFromIn and ResTakeFromOut when aFromScript=True
            //Only one will succeed, we don't care which one it is
            H.ResTakeFromIn(Res, aCount, True);
            H.ResTakeFromOut(Res, aCount, True);
          end;
        end
        else
          LogParamWarning('Actions.HouseTakeWaresFrom wrong ware type', [aHouseID, aType, aCount]);
      //Silently ignore if house doesn't exist
    end
    else
      LogParamWarning('Actions.HouseTakeWaresFrom', [aHouseID, aType, aCount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseRepairEnable(aHouseID: Integer; aRepairEnabled: Boolean);
var H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) then
        H.BuildingRepair := aRepairEnabled;
    end
    else
      LogParamWarning('Actions.HouseRepairEnable', [aHouseID, Byte(aRepairEnabled)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseDeliveryBlock(aHouseID: Integer; aDeliveryBlocked: Boolean);
var H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and gRes.HouseDat[H.HouseType].AcceptsWares then
        H.WareDelivery := not aDeliveryBlocked;
    end
    else
      LogParamWarning('Actions.HouseDeliveryBlock', [aHouseID, Byte(aDeliveryBlocked)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseDisableUnoccupiedMessage(aHouseID: Integer; aDisabled: Boolean);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) then
        H.DisableUnoccupiedMessage := aDisabled;
    end
    else
      LogParamWarning('Actions.HouseDisableUnoccupiedMessage', [aHouseID, Byte(aDisabled)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseWoodcutterChopOnly(aHouseID: Integer; aChopOnly: Boolean);
const
  CHOP_ONLY: array [Boolean] of TWoodcutterMode = (wcm_ChopAndPlant, wcm_Chop);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if H is TKMHouseWoodcutters then
        TKMHouseWoodcutters(H).WoodcutterMode := CHOP_ONLY[aChopOnly];
    end
    else
      LogParamWarning('Actions.HouseWoodcutterChopOnly', [aHouseID, Byte(aChopOnly)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseWareBlock(aHouseID, aWareType: Integer; aBlocked: Boolean);
var
  H: TKMHouse;
  Res: TWareType;
begin
  try
    if (aHouseID > 0)
    and (aWareType in [Low(WareIndexToType) .. High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aWareType];
      H := fIDCache.GetHouse(aHouseID);
      if H is TKMHouseStore then
        TKMHouseStore(H).NotAcceptFlag[Res] := aBlocked;
      if (H is TKMHouseBarracks) and (Res in [WARFARE_MIN..WARFARE_MAX]) then
        TKMHouseBarracks(H).NotAcceptFlag[Res] := aBlocked;
    end
    else
      LogParamWarning('Actions.HouseWareBlock', [aHouseID, aWareType, Byte(aBlocked)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseWeaponsOrderSet(aHouseID, aWareType, aAmount: Integer);
var
  H: TKMHouse;
  Res: TWareType;
  I: Integer;
begin
  try
    if (aHouseID > 0) and InRange(aAmount, 0, MAX_WARES_ORDER)
    and (aWareType in [Low(WareIndexToType) .. High(WareIndexToType)]) then
    begin
      Res := WareIndexToType[aWareType];
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) then
        for I := 1 to 4 do
          if gRes.HouseDat[H.HouseType].ResOutput[I] = Res then
          begin
            H.ResOrder[I] := aAmount;
            Exit;
          end;
    end
    else
      LogParamWarning('Actions.HouseWeaponsOrderSet', [aHouseID, aWareType, aAmount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseSchoolQueueRemove(aHouseID, QueueIndex: Integer);
var
  H: TKMHouse;
begin
  try
    if (aHouseID > 0) and InRange(QueueIndex, 0, 5) then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and (H is TKMHouseSchool) then
        TKMHouseSchool(H).RemUnitFromQueue(QueueIndex);
    end
    else
      LogParamWarning('Actions.HouseSchoolQueueRemove', [aHouseID, QueueIndex]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.HouseSchoolQueueAdd(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if (aHouseID > 0)
    and (aUnitType in [UnitTypeToIndex[CITIZEN_MIN]..UnitTypeToIndex[CITIZEN_MAX]]) then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and (H is TKMHouseSchool) then
        Result := TKMHouseSchool(H).AddUnitToQueue(UnitIndexToType[aUnitType], aCount);
    end
    else
      LogParamWarning('Actions.HouseSchoolQueueAdd', [aHouseID, aUnitType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.HouseBarracksEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer;
var
  H: TKMHouse;
begin
  try
    Result := 0;
    if (aHouseID > 0)
    and (aUnitType in [UnitTypeToIndex[WARRIOR_EQUIPABLE_MIN]..UnitTypeToIndex[WARRIOR_EQUIPABLE_MAX]]) then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and (H is TKMHouseBarracks) then
        Result := TKMHouseBarracks(H).Equip(UnitIndexToType[aUnitType], aCount);
    end
    else
      LogParamWarning('Actions.HouseBarracksEquip', [aHouseID, aUnitType]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.HouseBarracksGiveRecruit(aHouseID: Integer);
var
  H: TKMHouse;
begin
  try
    if aHouseID > 0 then
    begin
      H := fIDCache.GetHouse(aHouseID);
      if (H <> nil) and (H is TKMHouseBarracks) then
        TKMHouseBarracks(H).CreateRecruitInside(False);
    end
    else
      LogParamWarning('Actions.HouseBarracksGiveRecruit', [aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.Log(aText: AnsiString);
begin
  try
    fOnScriptError(se_Log, UnicodeString(aText));
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.MapTileSet(X, Y, aType, aRotation: Integer): Boolean;
begin
  try
    if gTerrain.TileInMapCoords(X, Y) and InRange(aType, 0, 255) and InRange(aRotation, 0, 3) then
      Result := gTerrain.ScriptTryTileSet(X, Y, aType, aRotation)
    else
    begin
      LogParamWarning('Actions.MapTileSet', [X, Y, aType, aRotation]);
      Result := False;
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.MapTileHeightSet(X, Y, Height: Integer): Boolean;
begin
  try
    //Height is vertex based not tile based
    if gTerrain.VerticeInMapCoords(X, Y) and InRange(Height, 0, 100) then
      Result := gTerrain.ScriptTryHeightSet(X, Y, Height)
    else
    begin
      LogParamWarning('Actions.MapTileHeightSet', [X, Y, Height]);
      Result := False;
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.MapTileObjectSet(X, Y, Obj: Integer): Boolean;
begin
  try
    //Objects are vertex based not tile based
    if gTerrain.VerticeInMapCoords(X, Y) and InRange(Obj, 0, 255) then
      Result := gTerrain.ScriptTryObjectSet(X, Y, Obj)
    else
    begin
      LogParamWarning('Actions.MapTileObjectSet', [X, Y, Obj]);
      Result := False;
    end;
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.OverlayTextSet(aPlayer: Shortint; aText: AnsiString);
begin
  try
    //Text from script should be only ANSI Latin, but UI is Unicode, so we switch it
    if InRange(aPlayer, -1, gHands.Count - 1) then //-1 means all players
      gGame.OverlaySet(UnicodeString(aText), aPlayer)
    else
      LogParamWarning('Actions.OverlayTextSet: '+UnicodeString(aText), [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.OverlayTextSetFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);
begin
  try
    if InRange(aPlayer, -1, gHands.Count - 1) then //-1 means all players
    begin
      try
        //Text from script should be only ANSI Latin, but UI is Unicode, so we switch it
        gGame.OverlaySetFormatted(UnicodeString(aText), Params, aPlayer);
      except
        //Format may throw an exception
        on E: EConvertError do LogParamWarning('Actions.OverlayTextSetFormatted: EConvertError: '+E.Message, []);
      end;
    end
    else
      LogParamWarning('Actions.OverlayTextSetFormatted: '+UnicodeString(aText), [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.OverlayTextAppend(aPlayer: Shortint; aText: AnsiString);
begin
  try
    //Text from script should be only ANSI Latin, but UI is Unicode, so we switch it
    if InRange(aPlayer, -1, gHands.Count - 1) then //-1 means all players
      gGame.OverlayAppend(UnicodeString(aText), aPlayer)
    else
      LogParamWarning('Actions.OverlayTextAppend: '+UnicodeString(aText), [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.OverlayTextAppendFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const);
begin
  try
    if InRange(aPlayer, -1, gHands.Count - 1) then //-1 means all players
    begin
      try
        //Text from script should be only ANSI Latin, but UI is Unicode, so we switch it
        gGame.OverlayAppendFormatted(UnicodeString(aText), Params, aPlayer);
      except
        //Format may throw an exception
        on E: EConvertError do LogParamWarning('Actions.OverlayTextAppendFormatted: EConvertError: '+E.Message, []);
      end;
    end
    else
      LogParamWarning('Actions.OverlayTextAppendFormatted: '+UnicodeString(aText), [aPlayer]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.MarketSetTrade(aMarketID, aFrom, aTo, aAmount: Integer);
var
  H: TKMHouse;
  ResFrom, ResTo: TWareType;
begin
  try
    if (aMarketID > 0)
    and (aFrom in [Low(WareIndexToType)..High(WareIndexToType)])
    and (aTo in [Low(WareIndexToType)..High(WareIndexToType)]) then
    begin
      H := fIDCache.GetHouse(aMarketID);
      ResFrom := WareIndexToType[aFrom];
      ResTo := WareIndexToType[aTo];
      if (H is TKMHouseMarket) and not H.IsDestroyed
      and TKMHouseMarket(H).AllowedToTrade(ResFrom)
      and TKMHouseMarket(H).AllowedToTrade(ResTo) then
      begin
        if (TKMHouseMarket(H).ResFrom <> ResFrom) or (TKMHouseMarket(H).ResTo <> ResTo) then
        begin
          TKMHouseMarket(H).ResOrder[0] := 0; //First we must cancel the current trade
          TKMHouseMarket(H).ResFrom := ResFrom;
          TKMHouseMarket(H).ResTo := ResTo;
        end;
        TKMHouseMarket(H).ResOrder[0] := aAmount; //Set the new trade
      end;
    end
    else
      LogParamWarning('Actions.MarketSetTrade', [aMarketID, aFrom, aTo, aAmount]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanAddRoad(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Road) then
      begin
        Result := True;
        gHands[aPlayer].BuildList.FieldworksList.AddField(KMPoint(X, Y), ft_Road);
      end;
    end
    else
      LogParamWarning('Actions.PlanAddRoad', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanAddField(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Corn) then
      begin
        Result := True;
        gHands[aPlayer].BuildList.FieldworksList.AddField(KMPoint(X, Y), ft_Corn);
      end;
    end
    else
      LogParamWarning('Actions.PlanAddField', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanAddWinefield(aPlayer, X, Y: Word): Boolean;
begin
  try
    Result := False;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[aPlayer].CanAddFieldPlan(KMPoint(X, Y), ft_Wine) then
      begin
        Result := True;
        gHands[aPlayer].BuildList.FieldworksList.AddField(KMPoint(X, Y), ft_Wine);
      end;
    end
    else
      LogParamWarning('Actions.PlanAddWinefield', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanConnectRoad(aPlayer, X1, Y1, X2, Y2: Integer; aCompleted: Boolean): Boolean;
var
  Points: TKMPointList;
  PlanExists: Boolean;
  I: Integer;
  Path: TPathFindingRoad;
begin
  try
    Result := False;
    if InRange(aPlayer, 0, gHands.Count - 1)
    and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X1, Y1)
    and gTerrain.TileInMapCoords(X2, Y2) then
    begin
      Path := TPathFindingRoad.Create(aPlayer);
      Points := TKMPointList.Create;
      try
        PlanExists := Path.Route_ReturnToWalkable(KMPoint(X1, Y1), KMPoint(X2, Y2), 0, Points);
        if not PlanExists then
          Exit;
        for I := 0 to Points.Count - 1 do
          if gHands[aPlayer].CanAddFieldPlan(Points[I], ft_Road) then
            if not aCompleted then
              gHands[aPlayer].BuildList.FieldworksList.AddField(Points[I], ft_Road)
            else
            begin
              gTerrain.SetField(Points[I], aPlayer, ft_Road);
              gTerrain.FlattenTerrain(Points[I]);
            end;
        Result := True;
      finally
        Points.Free;
        Path.Free;
      end;
    end
    else
      LogParamWarning('Actions.PlanConnectRoad', [aPlayer, X1, Y1, X2, Y2]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanRemove(aPlayer, X, Y: Word): Boolean;
var
  HT: THouseType;
begin
  try
    Result := False;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      HT := gHands[aPlayer].BuildList.HousePlanList.GetPlan(KMPoint(X, Y));
      if HT <> ht_None then
      begin
        gHands[aPlayer].BuildList.HousePlanList.RemPlan(KMPoint(X, Y));
        gHands[aPlayer].Stats.HousePlanRemoved(HT);
        Result := True;
      end;
      if gHands[aPlayer].BuildList.FieldworksList.HasField(KMPoint(X, Y)) <> ft_None then
      begin
        gHands[aPlayer].BuildList.FieldworksList.RemFieldPlan(KMPoint(X, Y));
        Result := True;
      end;
    end
    else
      LogParamWarning('Actions.PlanRemove', [aPlayer, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.PlanAddHouse(aPlayer, aHouseType, X, Y: Word): Boolean;
begin
  try
    Result := False;
    //Verify all input parameters
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and HouseTypeValid(aHouseType)
    and gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[aPlayer].CanAddHousePlan(KMPoint(X, Y), HouseIndexToType[aHouseType]) then
      begin
        Result := True;
        gHands[aPlayer].AddHousePlan(HouseIndexToType[aHouseType], KMPoint(X, Y));
      end;
    end
    else
      LogParamWarning('Actions.PlanAddHouse', [aPlayer, aHouseType, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.UnitBlock(aPlayer: Byte; aType: Word; aBlock: Boolean);
begin
  try
    if InRange(aPlayer, 0, gHands.Count - 1) and (gHands[aPlayer].Enabled)
    and (aType in [Low(UnitIndexToType) .. High(UnitIndexToType)]) then
      gHands[aPlayer].Stats.UnitBlocked[UnitIndexToType[aType]] := aBlock
    else
      LogParamWarning('Actions.UnitBlock', [aPlayer, aType, Byte(aBlock)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.UnitHungerSet(aUnitID, aHungerLevel: Integer);
var
  U: TKMUnit;
begin
  try
    aHungerLevel := Round(aHungerLevel / CONDITION_PACE);
    if (aUnitID > 0) and InRange(aHungerLevel, 0, UNIT_MAX_CONDITION) then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        U.Condition := aHungerLevel;
    end
    else
      LogParamWarning('Actions.UnitHungerSet', [aUnitID, aHungerLevel]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.UnitDirectionSet(aUnitID, aDirection: Integer): Boolean;
var
  U: TKMUnit;
begin
  try
    Result := False;
    if (aUnitID > 0) and (TKMDirection(aDirection+1) in [dir_N..dir_NW]) then
    begin
      U := fIDCache.GetUnit(aUnitID);
      //Can only make idle units outside houses change direction so we don't mess up tasks and cause crashes
      if (U <> nil) and U.IsIdle and U.Visible then
      begin
        Result := True;
        U.Direction := TKMDirection(aDirection+1);
      end;
    end
    else
      LogParamWarning('Actions.UnitDirectionSet', [aUnitID, aDirection]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.UnitOrderWalk(aUnitID: Integer; X, Y: Word): Boolean;
var
  U: TKMUnit;
begin
  try
    Result := False;

    if (aUnitID > 0) and gTerrain.TileInMapCoords(X, Y) then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U = nil then Exit; //Unit could have long died, or never existed

      //Animals cant be ordered to walk, they use Steering instead
      if (U.UnitType in [ANIMAL_MIN..ANIMAL_MAX]) then
        LogParamWarning('Actions.UnitOrderWalk is not supported for animals', [aUnitID, X, Y])
      else
        //Can only make idle or units in houses walk so we don't mess up tasks and cause crashes
        if U.IsIdle and U.Visible then
        begin
          Result := True;
          U.SetActionWalkToSpot(KMPoint(X,Y), ua_Walk);
        end;
    end
    else
      LogParamWarning('Actions.UnitOrderWalk', [aUnitID, X, Y]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.UnitKill(aUnitID: Integer; aSilent: Boolean);
var
  U: TKMUnit;
begin
  try
    if (aUnitID > 0) then
    begin
      U := fIDCache.GetUnit(aUnitID);
      if U <> nil then
        //Force delay to let the unit choose when to die, because this could be called in the middle of an event
        U.KillUnit(PLAYER_NONE, not aSilent, True);
    end
    else
      LogParamWarning('Actions.UnitKill', [aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupBlockOrders(aGroupID: Integer; aBlock: Boolean);
var
  G: TKMUnitGroup;
begin
  try
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        G.BlockOrders := aBlock;
    end
    else
      LogParamWarning('Actions.GroupBlockOrders', [aGroupID, Byte(aBlock)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupDisableHungryMessage(aGroupID: Integer; aDisable: Boolean);
var
  G: TKMUnitGroup;
begin
  try
    if aGroupID > 0 then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        G.DisableHungerMessage := aDisable;
    end
    else
      LogParamWarning('Actions.GroupDisableHungryMessage', [aGroupID, Byte(aDisable)]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupHungerSet(aGroupID, aHungerLevel: Integer);
var
  G: TKMUnitGroup;
  I: Integer;
begin
  try
    aHungerLevel := Round(aHungerLevel / CONDITION_PACE);
    if (aGroupID > 0) and InRange(aHungerLevel, 0, UNIT_MAX_CONDITION) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        for I := 0 to G.Count - 1 do
          if (G.Members[I] <> nil) and (not G.Members[I].IsDeadOrDying) then
            G.Members[I].Condition := aHungerLevel;
    end
    else
      LogParamWarning('Actions.GroupHungerSet', [aGroupID, aHungerLevel]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupKillAll(aGroupID: Integer; aSilent: Boolean);
var
  G: TKMUnitGroup;
  I: Integer;
begin
  try
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        for I := G.Count - 1 downto 0 do
          G.Members[I].KillUnit(PLAYER_NONE, not aSilent, True);
    end
    else
      LogParamWarning('Actions.GroupKillAll', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word);
var
  G: TKMUnitGroup;
begin
  try
    if (aGroupID > 0)
    and gTerrain.TileInMapCoords(X, Y)
    and (TKMDirection(aDirection + 1) in [dir_N..dir_NW]) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if (G <> nil) and G.CanWalkTo(KMPoint(X,Y), 0) then
        G.OrderWalk(KMPoint(X,Y), True, TKMDirection(aDirection+1));
    end
    else
      LogParamWarning('Actions.GroupOrderWalk', [aGroupID, X, Y, aDirection]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderAttackHouse(aGroupID, aHouseID: Integer);
var
  G: TKMUnitGroup;
  H: TKMHouse;
begin
  try
    if (aGroupID > 0) and (aHouseID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      H := fIDCache.GetHouse(aHouseID);
      if (G <> nil) and (H <> nil) then
        G.OrderAttackHouse(H, True);
    end
    else
      LogParamWarning('Actions.GroupOrderAttackHouse', [aGroupID, aHouseID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderAttackUnit(aGroupID, aUnitID: Integer);
var
  G: TKMUnitGroup;
  U: TKMUnit;
begin
  try
    if (aGroupID > 0) and (aUnitID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      U := fIDCache.GetUnit(aUnitID);

      //Player can not attack animals
      if (G <> nil) and (U <> nil) and (U.Owner <> PLAYER_ANIMAL) then
        G.OrderAttackUnit(U, True);
    end
    else
      LogParamWarning('Actions.GroupOrderAttackUnit', [aGroupID, aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderFood(aGroupID: Integer);
var
  G: TKMUnitGroup;
begin
  try
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if (G <> nil) then
        G.OrderFood(True);
    end
    else
      LogParamWarning('Actions.GroupOrderFood', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderStorm(aGroupID: Integer);
var
  G: TKMUnitGroup;
begin
  try
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if (G <> nil) and (G.GroupType = gt_Melee) then
        G.OrderStorm(True);
    end
    else
      LogParamWarning('Actions.GroupOrderStorm', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderHalt(aGroupID: Integer);
var
  G: TKMUnitGroup;
begin
  try
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if (G <> nil) then
        G.OrderHalt(True);
    end
    else
      LogParamWarning('Actions.GroupOrderHalt', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupOrderLink(aGroupID, aDestGroupID: Integer);
var
  G, G2: TKMUnitGroup;
begin
  try
    if (aGroupID > 0) and (aDestGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      G2 := fIDCache.GetGroup(aDestGroupID);
      if (G <> nil) and (G2 <> nil) and (G.Owner = G2.Owner) then  //Check group owners to prevent "DNA Modifications" ;D
        G.OrderLinkTo(G2, True);
    end
    else
      LogParamWarning('Actions.GroupOrderLink', [aGroupID, aDestGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GroupOrderSplit(aGroupID: Integer): Integer;
var
  G, G2: TKMUnitGroup;
begin
  try
    Result := UID_NONE;
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if (G <> nil) then
      begin
        G2 := G.OrderSplit(True);
        if G2 <> nil then
          Result := G2.UID;
      end;
    end
    else
      LogParamWarning('Actions.GroupOrderSplit', [aGroupID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


function TKMScriptActions.GroupOrderSplitUnit(aGroupID, aUnitID: Integer): Integer;
var
  G, G2: TKMUnitGroup;
  U: TKMUnit;
begin
  try
    Result := UID_NONE;
    if (aGroupID > 0)
    and (aUnitID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      U := fIDCache.GetUnit(aUnitID);
      if (G <> nil)
      and (U <> nil)
      and (G.HasMember(U)) then
      begin
        G2 := G.OrderSplitUnit(U, True);
        if G2 <> nil then
          Result := G2.UID;
      end;
    end
    else
      LogParamWarning('Actions.GroupOrderSplitSelected', [aGroupID, aUnitID]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


procedure TKMScriptActions.GroupSetFormation(aGroupID: Integer; aNumColumns: Byte);
var
  G: TKMUnitGroup;
begin
  try
    if (aGroupID > 0) then
    begin
      G := fIDCache.GetGroup(aGroupID);
      if G <> nil then
        G.UnitsPerRow := aNumColumns;
    end
    else
      LogParamWarning('Actions.GroupSetFormation', [aGroupID, aNumColumns]);
  except
    gScriptEvents.ExceptionOutsideScript := True; //Don't blame script for this exception
    raise;
  end;
end;


end.
