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


end.
