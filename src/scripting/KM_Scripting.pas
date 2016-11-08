unit KM_Scripting;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  uPSCompiler, uPSRuntime, uPSUtils, uPSDisassembly,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ScriptingActions, KM_ScriptingEvents, KM_ScriptingIdCache, KM_ScriptingStates,
  KM_Houses, KM_Units, KM_UnitGroups, KM_ResHouses;

  //Dynamic scripts allow mapmakers to control the mission flow

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
  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fCampaignDataTypeCode: AnsiString;
    fByteCode: AnsiString;
    fExec: TPSExec;
    fErrorString: UnicodeString; //Info about found mistakes (Unicode, can be localized later on)
    fWarningsString: UnicodeString;

    fHasErrorOccured: Boolean; //Has runtime error occurred? (only display first error)
    fScriptLogFile: UnicodeString;
    fOnScriptError: TUnicodeStringEvent;

    fStates: TKMScriptStates;
    fActions: TKMScriptActions;
    fIDCache: TKMScriptingIdCache;

    function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
    procedure ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Longint; ProcNo, Position: Cardinal; const PropData: tbtString);
    function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
    procedure CompileScript;
    procedure LinkRuntime;

    procedure HandleScriptError(aType: TScriptErrorType; const aMsg: UnicodeString);

    procedure SaveVar(SaveStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
    procedure LoadVar(LoadStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
  public
    constructor Create(aOnScriptError: TUnicodeStringEvent);
    destructor Destroy; override;

    property ErrorString: UnicodeString read fErrorString;
    property WarningsString: UnicodeString read fWarningsString;
    procedure LoadFromFile(aFileName: UnicodeString; aCampaignDataTypeFile: UnicodeString; aCampaignData: TKMemoryStream);
    procedure ExportDataToText;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure SaveCampaignData(SaveStream: TKMemoryStream);
    procedure LoadCampaignData(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


const
  MAX_LOG_SIZE = 1024 * 1024; //1 MB
  CAMPAIGN_DATA_TYPE = 'TCampaignData'; //Type of the global variable
  CAMPAIGN_DATA_VAR = 'CampaignData'; //Name of the global variable
  VALID_GLOBAL_VAR_TYPES: set of TPSBaseType = [
    btU8, //Byte, Boolean, Enums
    btS32, //Integer
    btSingle, //Single
    btString, //Means AnsiString in PascalScript. No need for scripts to use Unicode since LIBX files take care of that.
    btStaticArray, btArray, //Static and Dynamic Arrays
    btRecord, btSet];


implementation
uses
  KromUtils, KM_Game, KM_Log;


const
  SCRIPT_LOG_EXT = '.LOG.txt';


{ TKMScripting }
constructor TKMScripting.Create(aOnScriptError: TUnicodeStringEvent);
begin
  inherited Create;

  // Create an instance of the script executer
  fExec := TPSExec.Create;
  fIDCache := TKMScriptingIdCache.Create;

  // Global object to get events
  gScriptEvents := TKMScriptEvents.Create(fExec, fIDCache);
  fStates := TKMScriptStates.Create(fIDCache);
  fActions := TKMScriptActions.Create(fIDCache);

  fOnScriptError := aOnScriptError;
  gScriptEvents.OnScriptError := HandleScriptError;
  fStates.OnScriptError := HandleScriptError;
  fActions.OnScriptError := HandleScriptError;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(gScriptEvents);
  FreeAndNil(fStates);
  FreeAndNil(fActions);
  FreeAndNil(fIDCache);
  FreeAndNil(fExec);
  inherited;
end;


procedure TKMScripting.HandleScriptError(aType: TScriptErrorType; const aMsg: UnicodeString);
var
  fl: textfile;
begin
  gLog.AddTime('Script: ' + aMsg); //Always log the error to global game log

  //Log to map specific log file
  if fScriptLogFile <> '' then
  begin
    AssignFile(fl, fScriptLogFile);
    if not FileExists(fScriptLogFile) then
      Rewrite(fl)
    else
      if GetFileSize(fScriptLogFile) > MAX_LOG_SIZE then
      begin
        //Reset the log if it gets too long so poorly written scripts don't waste disk space
        Rewrite(fl);
        WriteLn(fl, Format('%23s   %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now),
                'Log file exceeded ' + IntToStr(MAX_LOG_SIZE) + ' bytes and was reset']));
      end
      else
        Append(fl);
    WriteLn(fl, Format('%23s   %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), aMsg]));
    CloseFile(fl);
  end;

  //Display compile errors in-game
  if aType = se_CompileError then
    fOnScriptError(StringReplace(aMsg, EolW, '|', [rfReplaceAll]));

  //Serious runtime errors should be shown to the player
  if aType in [se_Exception] then
  begin
    //Only show the first message in-game to avoid spamming the player
    if not fHasErrorOccured then
      fOnScriptError('Error(s) have occured in the mission script. ' +
                         'Please check the log file for further details. First error:||' + aMsg);
    fHasErrorOccured := True;
  end;
end;


procedure TKMScripting.LoadFromFile(aFileName: UnicodeString; aCampaignDataTypeFile: UnicodeString; aCampaignData: TKMemoryStream);
begin
  fScriptLogFile := ChangeFileExt(aFileName, SCRIPT_LOG_EXT);
  fErrorString := '';
  fWarningsString := '';

  if not FileExists(aFileName) then
  begin
    gLog.AddNoTime(aFileName + ' was not found. It is okay for mission to have no dynamic scripts.');
    Exit;
  end;

  if (aCampaignDataTypeFile <> '') and FileExists(aCampaignDataTypeFile) then
    fCampaignDataTypeCode := ReadTextA(aCampaignDataTypeFile)
  else
    fCampaignDataTypeCode := '';

  fScriptCode := ReadTextA(aFileName);

  CompileScript;

  if fErrorString <> '' then
    HandleScriptError(se_CompileError, 'Script compile errors:' + EolW + fErrorString);
  if fWarningsString <> '' then
    HandleScriptError(se_CompileWarning, 'Script compile warnings:' + EolW + fWarningsString);

  if aCampaignData <> nil then
    LoadCampaignData(aCampaignData);
end;


//The OnUses callback function is called for each "uses" in the script.
//It's always called with the parameter 'SYSTEM' at the top of the script.
//For example: uses ii1, ii2;
//This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'
function TKMScripting.ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
var CampaignDataType: TPSType;
begin
  if Name = 'SYSTEM' then
  begin

    if fCampaignDataTypeCode <> '' then
      try
        CampaignDataType := Sender.AddTypeS(CAMPAIGN_DATA_TYPE, fCampaignDataTypeCode);
        Sender.AddUsedVariable(CAMPAIGN_DATA_VAR, CampaignDataType);
      except
        on E: Exception do
          fErrorString := fErrorString + 'Error in declaration of global campaign data type|';
      end;
    //Register classes and methods to the script engine.
    //After that they can be used from within the script.
    with Sender.AddClassN(nil, AnsiString(fStates.ClassName)) do
    begin
      Sender.AddTypeS('TIntegerArray', 'array of Integer'); //Needed for PlayerGetAllUnits
      Sender.AddTypeS('TByteSet', 'set of Byte'); //Needed for Closest*MultipleTypes

      RegisterMethod('function ClosestGroup(aPlayer, X, Y, aGroupType: Integer): Integer');
      RegisterMethod('function ClosestHouse(aPlayer, X, Y, aHouseType: Integer): Integer');
      RegisterMethod('function ClosestUnit(aPlayer, X, Y, aUnitType: Integer): Integer');

      RegisterMethod('function ClosestGroupMultipleTypes(aPlayer, X, Y: Integer; aGroupTypes: TByteSet): Integer');
      RegisterMethod('function ClosestHouseMultipleTypes(aPlayer, X, Y: Integer; aHouseTypes: TByteSet): Integer');
      RegisterMethod('function ClosestUnitMultipleTypes(aPlayer, X, Y: Integer; aUnitTypes: TByteSet): Integer');

      RegisterMethod('function ConnectedByRoad(X1, Y1, X2, Y2: Integer): Boolean');
      RegisterMethod('function ConnectedByWalking(X1, Y1, X2, Y2: Integer): Boolean');

      RegisterMethod('function GameTime: Cardinal');
      RegisterMethod('function KaMRandom: Single');
      RegisterMethod('function KaMRandomI(aMax:Integer): Integer');
      RegisterMethod('function LocationCount: Integer');
      RegisterMethod('function MarketFromWare(aMarketID: Integer): Integer');
      RegisterMethod('function MarketLossFactor: Single');
      RegisterMethod('function MarketOrderAmount(aMarketID: Integer): Integer');
      RegisterMethod('function MarketToWare(aMarketID: Integer): Integer');
      RegisterMethod('function MarketValue(aRes: Integer): Single');
      RegisterMethod('function PeaceTime: Cardinal');

      RegisterMethod('function FogRevealed(aPlayer: Byte; aX, aY: Word): Boolean');

      RegisterMethod('function GroupAt(aX, aY: Word): Integer');
      RegisterMethod('function GroupColumnCount(aGroupID: Integer): Integer');
      RegisterMethod('function GroupDead(aGroupID: Integer): Boolean');
      RegisterMethod('function GroupIdle(aGroupID: Integer): Boolean');
      RegisterMethod('function GroupMember(aGroupID, aMemberIndex: Integer): Integer');
      RegisterMethod('function GroupMemberCount(aGroupID: Integer): Integer');
      RegisterMethod('function GroupOwner(aGroupID: Integer): Integer');
      RegisterMethod('function GroupType(aGroupID: Integer): Integer');

      RegisterMethod('function HouseAt(aX, aY: Word): Integer');
      RegisterMethod('function HouseBarracksRallyPointX(aBarracks: Integer): Integer');
      RegisterMethod('function HouseBarracksRallyPointY(aBarracks: Integer): Integer');
      RegisterMethod('function HouseBuildingProgress(aHouseID: Integer): Word');
      RegisterMethod('function HouseCanReachResources(aHouseID: Integer): Boolean)');
      RegisterMethod('function HouseDamage(aHouseID: Integer): Integer');
      RegisterMethod('function HouseDeliveryBlocked(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseDestroyed(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseHasOccupant(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseIsComplete(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseTypeMaxHealth(aHouseType: Integer): Word');
      RegisterMethod('function HouseOwner(aHouseID: Integer): Integer');
      RegisterMethod('function HouseTypeToOccupantType(aHouseID: Integer): Integer');
      RegisterMethod('function HousePositionX(aHouseID: Integer): Integer');
      RegisterMethod('function HousePositionY(aHouseID: Integer): Integer');
      RegisterMethod('function HouseRepair(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseResourceAmount(aHouseID, aResource: Integer): Integer');
      RegisterMethod('function HouseSchoolQueue(aHouseID, QueueIndex: Integer): Integer');
      RegisterMethod('function HouseSiteIsDigged(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseType(aHouseID: Integer): Integer');
      RegisterMethod('function HouseTypeName(aHouseType: Byte): AnsiString');
      RegisterMethod('function HouseUnlocked(aPlayer, aHouseType: Word): Boolean');
      RegisterMethod('function HouseWoodcutterChopOnly(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseWareBlocked(aHouseID, aWareType: Integer): Boolean');
      RegisterMethod('function HouseWeaponsOrdered(aHouseID, aWareType: Integer): Integer');

      RegisterMethod('function IsFieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');
      RegisterMethod('function IsWinefieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');
      RegisterMethod('function IsRoadAt(aPlayer: ShortInt; X, Y: Word): Boolean');

      RegisterMethod('function MapTileType(X, Y: Integer): Integer');
      RegisterMethod('function MapTileRotation(X, Y: Integer): Integer');
      RegisterMethod('function MapTileHeight(X, Y: Integer): Integer');
      RegisterMethod('function MapTileObject(X, Y: Integer): Integer');
      RegisterMethod('function MapTilePassability(X, Y: Integer; aPassability: Byte): Boolean');
      RegisterMethod('function MapWidth: Integer');
      RegisterMethod('function MapHeight: Integer');

      RegisterMethod('function PlayerAllianceCheck(aPlayer1, aPlayer2: Byte): Boolean');
      RegisterMethod('function PlayerColorText(aPlayer: Byte): AnsiString');
      RegisterMethod('function PlayerDefeated(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerEnabled(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerGetAllUnits(aPlayer: Byte): TIntegerArray');
      RegisterMethod('function PlayerGetAllHouses(aPlayer: Byte): TIntegerArray');
      RegisterMethod('function PlayerGetAllGroups(aPlayer: Byte): TIntegerArray');
      RegisterMethod('function PlayerIsAI(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerName(aPlayer: Byte): AnsiString');
      RegisterMethod('function PlayerVictorious(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerWareDistribution(aPlayer, aWareType, aHouseType: Byte): Byte');

      RegisterMethod('function StatAIDefencePositionsCount(aPlayer: Byte): Integer');
      RegisterMethod('function StatArmyCount(aPlayer: Byte): Integer');
      RegisterMethod('function StatCitizenCount(aPlayer: Byte): Integer');
      RegisterMethod('function StatHouseMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
      RegisterMethod('function StatHouseTypeCount(aPlayer, aHouseType: Byte): Integer');
      RegisterMethod('function StatHouseTypePlansCount(aPlayer, aHouseType: Byte): Integer');
      RegisterMethod('function StatPlayerCount: Integer');
      RegisterMethod('function StatResourceProducedCount(aPlayer, aResType: Byte): Integer');
      RegisterMethod('function StatResourceProducedMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
      RegisterMethod('function StatUnitCount(aPlayer: Byte): Integer');
      RegisterMethod('function StatUnitKilledCount(aPlayer, aUnitType: Byte): Integer');
      RegisterMethod('function StatUnitKilledMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
      RegisterMethod('function StatUnitLostCount(aPlayer, aUnitType: Byte): Integer');
      RegisterMethod('function StatUnitLostMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
      RegisterMethod('function StatUnitMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
      RegisterMethod('function StatUnitTypeCount(aPlayer, aUnitType: Byte): Integer');

      RegisterMethod('function UnitAt(aX, aY: Word): Integer');
      RegisterMethod('function UnitCarrying(aUnitID: Integer): Integer');
      RegisterMethod('function UnitDead(aUnitID: Integer): Boolean');
      RegisterMethod('function UnitDirection(aUnitID: Integer): Integer');
      RegisterMethod('function UnitsGroup(aUnitID: Integer): Integer');
      RegisterMethod('function UnitHome(aUnitID: Integer): Integer');
      RegisterMethod('function UnitHPCurrent(aUnitID: Integer): Integer');
      RegisterMethod('function UnitHPMax(aUnitID: Integer): Integer');
      RegisterMethod('function UnitHPInvulnerable(aUnitID: Integer): Boolean');
      RegisterMethod('function UnitHunger(aUnitID: Integer): Integer');
      RegisterMethod('function UnitIdle(aUnitID: Integer): Boolean');
      RegisterMethod('function UnitLowHunger: Integer');
      RegisterMethod('function UnitMaxHunger: Integer');
      RegisterMethod('function UnitOwner(aUnitID: Integer): Integer');
      RegisterMethod('function UnitPositionX(aHouseID: Integer): Integer');
      RegisterMethod('function UnitPositionY(aHouseID: Integer): Integer');
      RegisterMethod('function UnitType(aUnitID: Integer): Integer');
      RegisterMethod('function UnitTypeName(aUnitType: Byte): AnsiString');
      RegisterMethod('function WareTypeName(aWareType: Byte): AnsiString');
    end;

    with Sender.AddClassN(nil, AnsiString(fActions.ClassName)) do
    begin
      RegisterMethod('procedure AIAutoAttackRange(aPlayer: Byte; aRange: Word)');
      RegisterMethod('procedure AIAutoBuild(aPlayer: Byte; aAuto: Boolean)');
      RegisterMethod('procedure AIAutoDefence(aPlayer: Byte; aAuto: Boolean)');
      RegisterMethod('procedure AIAutoRepair(aPlayer: Byte; aAuto: Boolean)');
      RegisterMethod('procedure AIDefencePositionAdd(aPlayer: Byte; X, Y: Integer; aDir, aGroupType: Byte; aRadius: Word; aDefType: Byte)');
      RegisterMethod('procedure AIDefencePositionRemove(aPlayer: Byte; X, Y: Integer)');
      RegisterMethod('procedure AIDefencePositionRemoveAll(aPlayer: Byte)');
      RegisterMethod('procedure AIDefendAllies(aPlayer: Byte; aDefend: Boolean)');
      RegisterMethod('procedure AIEquipRate(aPlayer: Byte; aType: Byte; aRate: Word)');
      RegisterMethod('procedure AIGroupsFormationSet(aPlayer, aType: Byte; aCount, aColumns: Word)');
      RegisterMethod('procedure AIRecruitDelay(aPlayer, aDelay: Cardinal)');
      RegisterMethod('procedure AIRecruitLimit(aPlayer, aLimit: Byte)');
      RegisterMethod('procedure AISerfsFactor(aPlayer: Byte; aLimit: Single)');
      RegisterMethod('procedure AISoldiersLimit(aPlayer: Byte; aLimit: Integer)');
      RegisterMethod('procedure AIStartPosition(aPlayer: Byte; X, Y: Word)');
      RegisterMethod('procedure AIWorkerLimit(aPlayer, aLimit: Byte)');

      RegisterMethod('procedure CinematicStart(aPlayer: Byte)');
      RegisterMethod('procedure CinematicEnd(aPlayer: Byte)');
      RegisterMethod('procedure CinematicPanTo(aPlayer: Byte; X, Y, Duration: Word)');

      RegisterMethod('procedure FogCoverAll(aPlayer: Byte)');
      RegisterMethod('procedure FogCoverCircle(aPlayer, X, Y, aRadius: Word)');
      RegisterMethod('procedure FogCoverRect(aPlayer, X1, Y1, X2, Y2: Word)');
      RegisterMethod('procedure FogRevealAll(aPlayer: Byte)');
      RegisterMethod('procedure FogRevealCircle(aPlayer, X, Y, aRadius: Word)');
      RegisterMethod('procedure FogRevealRect(aPlayer, X1, Y1, X2, Y2: Word)');

      RegisterMethod('function  GiveAnimal(aType, X,Y: Word): Integer');
      RegisterMethod('function  GiveField(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  GiveGroup(aPlayer, aType, X, Y, aDir, aCount, aColumns: Word): Integer');
      RegisterMethod('function  GiveHouse(aPlayer, aHouseType, X,Y: Integer): Integer');
      RegisterMethod('function  GiveHouseSite(aPlayer, aHouseType, X, Y: Integer; aAddMaterials: Boolean): Integer');
      RegisterMethod('function  GiveRoad(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  GiveUnit(aPlayer, aType, X,Y, aDir: Word): Integer');
      RegisterMethod('procedure GiveWares(aPlayer, aType, aCount: Word)');
      RegisterMethod('procedure GiveWeapons(aPlayer, aType, aCount: Word)');
      RegisterMethod('function  GiveWineField(aPlayer, X, Y: Word): Boolean');

      RegisterMethod('procedure GroupBlockOrders(aGroupID: Integer; aBlock: Boolean)');
      RegisterMethod('procedure GroupDisableHungryMessage(aGroupID: Integer; aDisable: Boolean)');
      RegisterMethod('procedure GroupHungerSet(aGroupID, aHungerLevel: Integer)');
      RegisterMethod('procedure GroupKillAll(aGroupID: Integer; aSilent: Boolean)');
      RegisterMethod('procedure GroupOrderAttackHouse(aGroupID, aHouseID: Integer)');
      RegisterMethod('procedure GroupOrderAttackUnit(aGroupID, aUnitID: Integer)');
      RegisterMethod('procedure GroupOrderFood(aGroupID: Integer)');
      RegisterMethod('procedure GroupOrderHalt(aGroupID: Integer)');
      RegisterMethod('procedure GroupOrderLink(aGroupID, aDestGroupID: Integer)');
      RegisterMethod('function  GroupOrderSplit(aGroupID: Integer): Integer');
      RegisterMethod('function  GroupOrderSplitUnit(aGroupID, aUnitID: Integer): Integer');
      RegisterMethod('procedure GroupOrderStorm(aGroupID: Integer)');
      RegisterMethod('procedure GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word)');
      RegisterMethod('procedure GroupSetFormation(aGroupID: Integer; aNumColumns: Byte)');

      RegisterMethod('procedure HouseAddBuildingMaterials(aHouseID: Integer)');
      RegisterMethod('procedure HouseAddBuildingProgress(aHouseID: Integer)');
      RegisterMethod('procedure HouseAddDamage(aHouseID: Integer; aDamage: Word)');
      RegisterMethod('procedure HouseAddRepair(aHouseID: Integer; aRepair: Word)');
      RegisterMethod('procedure HouseAddWaresTo(aHouseID: Integer; aType, aCount: Word)');
      RegisterMethod('procedure HouseAllow(aPlayer, aHouseType: Word; aAllowed: Boolean)');
      RegisterMethod('function  HouseBarracksEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
      RegisterMethod('procedure HouseBarracksGiveRecruit(aHouseID: Integer)');
      RegisterMethod('procedure HouseDeliveryBlock(aHouseID: Integer; aDeliveryBlocked: Boolean)');
      RegisterMethod('procedure HouseDestroy(aHouseID: Integer; aSilent: Boolean)');
      RegisterMethod('procedure HouseDisableUnoccupiedMessage(aHouseID: Integer; aDisabled: Boolean)');
      RegisterMethod('procedure HouseRepairEnable(aHouseID: Integer; aRepairEnabled: Boolean)');
      RegisterMethod('function  HouseSchoolQueueAdd(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
      RegisterMethod('procedure HouseSchoolQueueRemove(aHouseID, QueueIndex: Integer)');
      RegisterMethod('procedure HouseTakeWaresFrom(aHouseID: Integer; aType, aCount: Word)');
      RegisterMethod('procedure HouseUnlock(aPlayer, aHouseType: Word)');
      RegisterMethod('procedure HouseWoodcutterChopOnly(aHouseID: Integer; aChopOnly: Boolean)');
      RegisterMethod('procedure HouseWareBlock(aHouseID, aWareType: Integer; aBlocked: Boolean)');
      RegisterMethod('procedure HouseWeaponsOrderSet(aHouseID, aWareType, aAmount: Integer)');

      RegisterMethod('procedure Log(aText: AnsiString)');
      RegisterMethod('procedure MarketSetTrade(aMarketID, aFrom, aTo, aAmount: Integer)');

      RegisterMethod('function MapTileSet(X, Y, aType, aRotation: Integer): Boolean');
      RegisterMethod('function MapTileHeightSet(X, Y, Height: Integer): Boolean');
      RegisterMethod('function MapTileObjectSet(X, Y, Obj: Integer): Boolean');

      RegisterMethod('procedure OverlayTextSet(aPlayer: Shortint; aText: AnsiString)');
      RegisterMethod('procedure OverlayTextSetFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');
      RegisterMethod('procedure OverlayTextAppend(aPlayer: Shortint; aText: AnsiString)');
      RegisterMethod('procedure OverlayTextAppendFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');

      RegisterMethod('function  PlanAddField(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  PlanAddHouse(aPlayer, aHouseType, X, Y: Word): Boolean');
      RegisterMethod('function  PlanAddRoad(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  PlanAddWinefield(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  PlanConnectRoad(aPlayer, X1, Y1, X2, Y2: Integer; aCompleted: Boolean): Boolean');
      RegisterMethod('function  PlanRemove(aPlayer, X, Y: Word): Boolean');

      RegisterMethod('procedure PlayerAddDefaultGoals(aPlayer: Byte; aBuildings: Boolean)');
      RegisterMethod('procedure PlayerAllianceChange(aPlayer1, aPlayer2: Byte; aCompliment, aAllied: Boolean)');
      RegisterMethod('procedure PlayerDefeat(aPlayer: Word)');
      RegisterMethod('procedure PlayerShareBeacons(aPlayer1, aPlayer2: Word; aCompliment, aShare: Boolean)');
      RegisterMethod('procedure PlayerShareFog(aPlayer1, aPlayer2: Word; aCompliment, aShare: Boolean)');
      RegisterMethod('procedure PlayerWareDistribution(aPlayer, aWareType, aHouseType, aAmount: Byte)');
      RegisterMethod('procedure PlayerWin(const aVictors: array of Integer; aTeamVictory: Boolean)');

      RegisterMethod('procedure PlayWAV(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single)');
      RegisterMethod('procedure PlayWAVFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single)');
      RegisterMethod('procedure PlayWAVAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word)');
      RegisterMethod('function  PlayWAVLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
      RegisterMethod('function  PlayWAVAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
      RegisterMethod('procedure StopLoopedWAV(aLoopIndex: Integer)');

      RegisterMethod('procedure RemoveRoad(X, Y: Word)');

      RegisterMethod('procedure SetTradeAllowed(aPlayer, aResType: Word; aAllowed: Boolean)');
      RegisterMethod('procedure ShowMsg(aPlayer: ShortInt; aText: AnsiString)');
      RegisterMethod('procedure ShowMsgFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');
      RegisterMethod('procedure ShowMsgGoto(aPlayer: Shortint; aX, aY: Word; aText: AnsiString)');
      RegisterMethod('procedure ShowMsgGotoFormatted(aPlayer: Shortint; aX, aY: Word; aText: AnsiString; Params: array of const)');

      RegisterMethod('procedure UnitBlock(aPlayer: Byte; aType: Word; aBlock: Boolean)');
      RegisterMethod('function  UnitDirectionSet(aUnitID, aDirection: Integer): Boolean');
      RegisterMethod('procedure UnitHPChange(aUnitID, aHP: Integer)');
      RegisterMethod('procedure UnitHPSetInvulnerable(aUnitID: Integer; aInvulnerable: Boolean)');
      RegisterMethod('procedure UnitHungerSet(aUnitID, aHungerLevel: Integer)');
      RegisterMethod('procedure UnitKill(aUnitID: Integer; aSilent: Boolean)');
      RegisterMethod('function  UnitOrderWalk(aUnitID: Integer; X, Y: Word): Boolean');
    end;

    //Register objects
    AddImportedClassVariable(Sender, 'States', AnsiString(fStates.ClassName));
    AddImportedClassVariable(Sender, 'Actions', AnsiString(fActions.ClassName));

    Result := True;
  end
  else
    Result := False;
end;


procedure TKMScripting.ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Integer; ProcNo, Position: Cardinal; const PropData: tbtString);
begin
  //There's no variable type info here
  //GetVarCount is not including this current variable yet either
end;


{ The OnExportCheck callback function is called for each function in the script
  (Also for the main proc, with '!MAIN' as a Proc^.Name). ProcDecl contains the
  result type and parameter types of a function using this format:
  ProcDecl: ResultType + ' ' + Parameter1 + ' ' + Parameter2 + ' '+Parameter3 + .....
  Parameter: ParameterType+TypeName
  ParameterType is @ for a normal parameter and ! for a var parameter.
  A result type of 0 means no result}
function TKMScripting.ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
const
  Procs: array [0..23] of record
    Names: AnsiString;
    ParamCount: Byte;
    Typ: array [0..4] of Byte;
    Dir: array [0..3] of TPSParameterMode;
  end =
  (
  (Names: 'ONHOUSEBUILT';           ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONHOUSEDAMAGED';         ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONHOUSEDESTROYED';       ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONHOUSEAFTERDESTROYED';  ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONHOUSEPLANPLACED';      ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONHOUSEPLANREMOVED';     ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONGROUPHUNGRY';          ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONMARKETTRADE';          ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONMISSIONSTART';         ParamCount: 0; Typ: (0, 0,     0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANROADPLACED';       ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANFIELDPLACED';      ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANWINEFIELDPLACED';  ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANROADREMOVED';      ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANFIELDREMOVED';     ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLANWINEFIELDREMOVED'; ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLAYERDEFEATED';       ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONPLAYERVICTORY';        ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONTICK';                 ParamCount: 0; Typ: (0, 0,     0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONUNITDIED';             ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONUNITAFTERDIED';        ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONUNITATTACKED';         ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONUNITTRAINED';          ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONUNITWOUNDED';          ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'ONWARRIOREQUIPPED';      ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn))
  );
var
  I: Integer;
begin
  Result := True;
  for I := Low(Procs) to High(Procs) do
    if (Proc.Name = Procs[I].Names) then
      if not ExportCheck(Sender, Proc, Slice(Procs[I].Typ, Procs[I].ParamCount+1), Slice(Procs[I].Dir, Procs[I].ParamCount)) then
      begin
        //Something is wrong, show an error
        //todo: Sender.MakeError reports the wrong line number so the user has no idea what the error is
        Sender.MakeError(Procs[I].Names, ecTypeMismatch, '');
        Result := False;
        Exit;
      end;
end;


procedure TKMScripting.CompileScript;
var
  I: Integer;
  Compiler: TPSPascalCompiler;
begin
  Compiler := TPSPascalCompiler.Create; // create an instance of the compiler
  try
    Compiler.OnUses := ScriptOnUses; // assign the OnUses event
    Compiler.OnUseVariable := ScriptOnUseVariable;
    Compiler.OnExportCheck := ScriptOnExportCheck; // Assign the onExportCheck event

    Compiler.AllowNoEnd := True; //Scripts only use event handlers now, main section is unused
    Compiler.BooleanShortCircuit := True; //Like unchecking "Complete booolean evaluation" in Delphi compiler options

    if not Compiler.Compile(fScriptCode) then  // Compile the Pascal script into bytecode
    begin
      for I := 0 to Compiler.MsgCount - 1 do
        fErrorString := fErrorString + UnicodeString(Compiler.Msg[I].MessageToString) + EolW;
      Exit;
    end
      else
        for I := 0 to Compiler.MsgCount - 1 do
          fWarningsString := fWarningsString + UnicodeString(Compiler.Msg[I].MessageToString) + EolW;

    Compiler.GetOutput(fByteCode); // Save the output of the compiler in the string Data.
  finally
    Compiler.Free;
  end;

  LinkRuntime;
end;


//Link the ByteCode with used functions and load it into Executioner
procedure TKMScripting.LinkRuntime;

  function ValidateVarType(aType: TPSTypeRec): UnicodeString;
  var
    I: Integer;
  begin
    //Check against our set of allowed types
    if not (aType.BaseType in VALID_GLOBAL_VAR_TYPES) then
    begin
      Result := Format('Unsupported global variable type %d (%s)|', [aType.BaseType, UnicodeString(aType.ExportName)]);
      Exit;
    end;

    //Check elements of arrays/records are valid too
    case aType.BaseType of
      btArray,
      btStaticArray:
        Result := ValidateVarType(TPSTypeRec_Array(aType).ArrayType);
      btRecord:
        begin
          Result := '';
          for I := 0 to TPSTypeRec_Record(aType).FieldTypes.Count - 1 do
            Result := Result + ValidateVarType(TPSTypeRec_Record(aType).FieldTypes[I]);
        end;
    end;
  end;

var
  ClassImp: TPSRuntimeClassImporter;
  I: Integer;
  V: PIFVariant;
begin
  //Create an instance of the runtime class importer
  ClassImp := TPSRuntimeClassImporter.Create;
  try
    //Register classes and their exposed methods to Runtime (must be uppercase)
    with ClassImp.Add(TKMScriptStates) do
    begin

      RegisterMethod(@TKMScriptStates.ClosestGroup,   'CLOSESTGROUP');
      RegisterMethod(@TKMScriptStates.ClosestHouse,   'CLOSESTHOUSE');
      RegisterMethod(@TKMScriptStates.ClosestUnit,    'CLOSESTUNIT');

      RegisterMethod(@TKMScriptStates.ClosestGroupMultipleTypes,   'CLOSESTGROUPMULTIPLETYPES');
      RegisterMethod(@TKMScriptStates.ClosestHouseMultipleTypes,   'CLOSESTHOUSEMULTIPLETYPES');
      RegisterMethod(@TKMScriptStates.ClosestUnitMultipleTypes,    'CLOSESTUNITMULTIPLETYPES');

      RegisterMethod(@TKMScriptStates.ConnectedByRoad,    'CONNECTEDBYROAD');
      RegisterMethod(@TKMScriptStates.ConnectedByWalking, 'CONNECTEDBYWALKING');

      RegisterMethod(@TKMScriptStates.GameTime,          'GAMETIME');
      RegisterMethod(@TKMScriptStates.KaMRandom,         'KAMRANDOM');
      RegisterMethod(@TKMScriptStates.KaMRandomI,        'KAMRANDOMI');
      RegisterMethod(@TKMScriptStates.LocationCount,     'LOCATIONCOUNT');
      RegisterMethod(@TKMScriptStates.MarketFromWare,    'MARKETFROMWARE');
      RegisterMethod(@TKMScriptStates.MarketLossFactor,  'MARKETLOSSFACTOR');
      RegisterMethod(@TKMScriptStates.MarketOrderAmount, 'MARKETORDERAMOUNT');
      RegisterMethod(@TKMScriptStates.MarketToWare,      'MARKETTOWARE');
      RegisterMethod(@TKMScriptStates.MarketValue,       'MARKETVALUE');
      RegisterMethod(@TKMScriptStates.PeaceTime,         'PEACETIME');

      RegisterMethod(@TKMScriptStates.FogRevealed,      'FOGREVEALED');

      RegisterMethod(@TKMScriptStates.GroupAt,          'GROUPAT');
      RegisterMethod(@TKMScriptStates.GroupDead,        'GROUPDEAD');
      RegisterMethod(@TKMScriptStates.GroupIdle,        'GROUPIDLE');
      RegisterMethod(@TKMScriptStates.GroupMember,      'GROUPMEMBER');
      RegisterMethod(@TKMScriptStates.GroupColumnCount, 'GROUPCOLUMNCOUNT');
      RegisterMethod(@TKMScriptStates.GroupMemberCount, 'GROUPMEMBERCOUNT');
      RegisterMethod(@TKMScriptStates.GroupOwner,       'GROUPOWNER');
      RegisterMethod(@TKMScriptStates.GroupType,        'GROUPTYPE');

      RegisterMethod(@TKMScriptStates.HouseAt,                  'HOUSEAT');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointX, 'HOUSEBARRACKSRALLYPOINTX');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointY, 'HOUSEBARRACKSRALLYPOINTY');
      RegisterMethod(@TKMScriptStates.HouseBuildingProgress,    'HOUSEBUILDINGPROGRESS');
      RegisterMethod(@TKMScriptStates.HouseCanReachResources,   'HOUSECANREACHRESOURCES');
      RegisterMethod(@TKMScriptStates.HouseDamage,              'HOUSEDAMAGE');
      RegisterMethod(@TKMScriptStates.HouseDeliveryBlocked,     'HOUSEDELIVERYBLOCKED');
      RegisterMethod(@TKMScriptStates.HouseDestroyed,           'HOUSEDESTROYED');
      RegisterMethod(@TKMScriptStates.HouseHasOccupant,         'HOUSEHASOCCUPANT');
      RegisterMethod(@TKMScriptStates.HouseIsComplete,          'HOUSEISCOMPLETE');
      RegisterMethod(@TKMScriptStates.HouseOwner,               'HOUSEOWNER');
      RegisterMethod(@TKMScriptStates.HousePositionX,           'HOUSEPOSITIONX');
      RegisterMethod(@TKMScriptStates.HousePositionY,           'HOUSEPOSITIONY');
      RegisterMethod(@TKMScriptStates.HouseRepair,              'HOUSEREPAIR');
      RegisterMethod(@TKMScriptStates.HouseResourceAmount,      'HOUSERESOURCEAMOUNT');
      RegisterMethod(@TKMScriptStates.HouseType,                'HOUSETYPE');
      RegisterMethod(@TKMScriptStates.HouseTypeMaxHealth,       'HOUSETYPEMAXHEALTH');
      RegisterMethod(@TKMScriptStates.HouseTypeName,            'HOUSETYPENAME');
      RegisterMethod(@TKMScriptStates.HouseTypeToOccupantType,  'HOUSETYPETOOCCUPANTTYPE');
      RegisterMethod(@TKMScriptStates.HouseSchoolQueue,         'HOUSESCHOOLQUEUE');
      RegisterMethod(@TKMScriptStates.HouseSiteIsDigged,        'HOUSESITEISDIGGED');
      RegisterMethod(@TKMScriptStates.HouseUnlocked,            'HOUSEUNLOCKED');
      RegisterMethod(@TKMScriptStates.HouseWoodcutterChopOnly,  'HOUSEWOODCUTTERCHOPONLY');
      RegisterMethod(@TKMScriptStates.HouseWareBlocked,         'HOUSEWAREBLOCKED');
      RegisterMethod(@TKMScriptStates.HouseWeaponsOrdered,      'HOUSEWEAPONSORDERED');

      RegisterMethod(@TKMScriptStates.IsFieldAt,            'ISFIELDAT');
      RegisterMethod(@TKMScriptStates.IsWinefieldAt,        'ISWINEFIELDAT');
      RegisterMethod(@TKMScriptStates.IsRoadAt,             'ISROADAT');

      RegisterMethod(@TKMScriptStates.MapTileType,             'MAPTILETYPE');
      RegisterMethod(@TKMScriptStates.MapTileRotation,         'MAPTILEROTATION');
      RegisterMethod(@TKMScriptStates.MapTileHeight,           'MAPTILEHEIGHT');
      RegisterMethod(@TKMScriptStates.MapTileObject,           'MAPTILEOBJECT');
      RegisterMethod(@TKMScriptStates.MapTilePassability,      'MAPTILEPASSABILITY');
      RegisterMethod(@TKMScriptStates.MapWidth,                'MAPWIDTH');
      RegisterMethod(@TKMScriptStates.MapHeight,               'MAPHEIGHT');

      RegisterMethod(@TKMScriptStates.PlayerAllianceCheck,    'PLAYERALLIANCECHECK');
      RegisterMethod(@TKMScriptStates.PlayerColorText,        'PLAYERCOLORTEXT');
      RegisterMethod(@TKMScriptStates.PlayerDefeated,         'PLAYERDEFEATED');
      RegisterMethod(@TKMScriptStates.PlayerEnabled,          'PLAYERENABLED');
      RegisterMethod(@TKMScriptStates.PlayerGetAllUnits,      'PLAYERGETALLUNITS');
      RegisterMethod(@TKMScriptStates.PlayerGetAllHouses,     'PLAYERGETALLHOUSES');
      RegisterMethod(@TKMScriptStates.PlayerGetAllGroups,     'PLAYERGETALLGROUPS');
      RegisterMethod(@TKMScriptStates.PlayerIsAI,             'PLAYERISAI');
      RegisterMethod(@TKMScriptStates.PlayerName,             'PLAYERNAME');
      RegisterMethod(@TKMScriptStates.PlayerVictorious,       'PLAYERVICTORIOUS');
      RegisterMethod(@TKMScriptStates.PlayerWareDistribution, 'PLAYERWAREDISTRIBUTION');

      RegisterMethod(@TKMScriptStates.StatAIDefencePositionsCount,              'STATAIDEFENCEPOSITIONSCOUNT');
      RegisterMethod(@TKMScriptStates.StatArmyCount,                            'STATARMYCOUNT');
      RegisterMethod(@TKMScriptStates.StatCitizenCount,                         'STATCITIZENCOUNT');
      RegisterMethod(@TKMScriptStates.StatHouseMultipleTypesCount,              'STATHOUSEMULTIPLETYPESCOUNT');
      RegisterMethod(@TKMScriptStates.StatHouseTypeCount,                       'STATHOUSETYPECOUNT');
      RegisterMethod(@TKMScriptStates.StatHouseTypePlansCount,                  'STATHOUSETYPEPLANSCOUNT');
      RegisterMethod(@TKMScriptStates.StatPlayerCount,                          'STATPLAYERCOUNT');
      RegisterMethod(@TKMScriptStates.StatResourceProducedCount,                'STATRESOURCEPRODUCEDCOUNT');
      RegisterMethod(@TKMScriptStates.StatResourceProducedMultipleTypesCount,   'STATRESOURCEPRODUCEDMULTIPLETYPESCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitCount,                            'STATUNITCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitKilledCount,                      'STATUNITKILLEDCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitKilledMultipleTypesCount,         'STATUNITKILLEDMULTIPLETYPESCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitLostCount,                        'STATUNITLOSTCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitLostMultipleTypesCount,           'STATUNITLOSTMULTIPLETYPESCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitMultipleTypesCount,               'STATUNITMULTIPLETYPESCOUNT');
      RegisterMethod(@TKMScriptStates.StatUnitTypeCount,                        'STATUNITTYPECOUNT');

      RegisterMethod(@TKMScriptStates.UnitAt,         'UNITAT');
      RegisterMethod(@TKMScriptStates.UnitCarrying,   'UNITCARRYING');
      RegisterMethod(@TKMScriptStates.UnitDead,       'UNITDEAD');
      RegisterMethod(@TKMScriptStates.UnitDirection,  'UNITDIRECTION');
      RegisterMethod(@TKMScriptStates.UnitsGroup,     'UNITSGROUP');
      RegisterMethod(@TKMScriptStates.UnitHome,       'UNITHOME');
      RegisterMethod(@TKMScriptStates.UnitHPCurrent,  'UNITHPCURRENT');
      RegisterMethod(@TKMScriptStates.UnitHPMax,      'UNITHPMAX');
      RegisterMethod(@TKMScriptStates.UnitHPInvulnerable,'UNITHPINVULNERABLE');
      RegisterMethod(@TKMScriptStates.UnitHunger,     'UNITHUNGER');
      RegisterMethod(@TKMScriptStates.UnitIdle,       'UNITIDLE');
      RegisterMethod(@TKMScriptStates.UnitLowHunger,  'UNITLOWHUNGER');
      RegisterMethod(@TKMScriptStates.UnitMaxHunger,  'UNITMAXHUNGER');
      RegisterMethod(@TKMScriptStates.UnitOwner,      'UNITOWNER');
      RegisterMethod(@TKMScriptStates.UnitPositionX,  'UNITPOSITIONX');
      RegisterMethod(@TKMScriptStates.UnitPositionY,  'UNITPOSITIONY');
      RegisterMethod(@TKMScriptStates.UnitType,       'UNITTYPE');
      RegisterMethod(@TKMScriptStates.UnitTypeName,   'UNITTYPENAME');
      RegisterMethod(@TKMScriptStates.WareTypeName,   'WARETYPENAME');
    end;

    with ClassImp.Add(TKMScriptActions) do
    begin
      RegisterMethod(@TKMScriptActions.AIAutoAttackRange,          'AIAUTOATTACKRANGE');
      RegisterMethod(@TKMScriptActions.AIAutoBuild,                'AIAUTOBUILD');
      RegisterMethod(@TKMScriptActions.AIAutoDefence,              'AIAUTODEFENCE');
      RegisterMethod(@TKMScriptActions.AIAutoRepair,               'AIAUTOREPAIR');
      RegisterMethod(@TKMScriptActions.AIDefencePositionAdd,       'AIDEFENCEPOSITIONADD');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemove,    'AIDEFENCEPOSITIONREMOVE');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemoveAll, 'AIDEFENCEPOSITIONREMOVEALL');
      RegisterMethod(@TKMScriptActions.AIDefendAllies,             'AIDEFENDALLIES');
      RegisterMethod(@TKMScriptActions.AIEquipRate,                'AIEQUIPRATE');
      RegisterMethod(@TKMScriptActions.AIGroupsFormationSet,       'AIGROUPSFORMATIONSET');
      RegisterMethod(@TKMScriptActions.AIRecruitDelay,             'AIRECRUITDELAY');
      RegisterMethod(@TKMScriptActions.AIRecruitLimit,             'AIRECRUITLIMIT');
      RegisterMethod(@TKMScriptActions.AISerfsPerHouse,            'AISERFSPERHOUSE');
      RegisterMethod(@TKMScriptActions.AISoldiersLimit,            'AISOLDIERSLIMIT');
      RegisterMethod(@TKMScriptActions.AIStartPosition,            'AISTARTPOSITION');
      RegisterMethod(@TKMScriptActions.AIWorkerLimit,              'AIWORKERLIMIT');

      RegisterMethod(@TKMScriptActions.CinematicStart,    'CINEMATICSTART');
      RegisterMethod(@TKMScriptActions.CinematicEnd,      'CINEMATICEND');
      RegisterMethod(@TKMScriptActions.CinematicPanTo,    'CINEMATICPANTO');

      RegisterMethod(@TKMScriptActions.FogCoverAll,       'FOGCOVERALL');
      RegisterMethod(@TKMScriptActions.FogCoverCircle,    'FOGCOVERCIRCLE');
      RegisterMethod(@TKMScriptActions.FogCoverRect,      'FOGCOVERRECT');
      RegisterMethod(@TKMScriptActions.FogRevealAll,      'FOGREVEALALL');
      RegisterMethod(@TKMScriptActions.FogRevealCircle,   'FOGREVEALCIRCLE');
      RegisterMethod(@TKMScriptActions.FogRevealRect,     'FOGREVEALRECT');

      RegisterMethod(@TKMScriptActions.GiveAnimal,    'GIVEANIMAL');
      RegisterMethod(@TKMScriptActions.GiveField,     'GIVEFIELD');
      RegisterMethod(@TKMScriptActions.GiveGroup,     'GIVEGROUP');
      RegisterMethod(@TKMScriptActions.GiveUnit,      'GIVEUNIT');
      RegisterMethod(@TKMScriptActions.GiveHouse,     'GIVEHOUSE');
      RegisterMethod(@TKMScriptActions.GiveHouseSite, 'GIVEHOUSESITE');
      RegisterMethod(@TKMScriptActions.GiveRoad,      'GIVEROAD');
      RegisterMethod(@TKMScriptActions.GiveWares,     'GIVEWARES');
      RegisterMethod(@TKMScriptActions.GiveWeapons,   'GIVEWEAPONS');
      RegisterMethod(@TKMScriptActions.GiveWineField, 'GIVEWINEFIELD');

      RegisterMethod(@TKMScriptActions.GroupBlockOrders,          'GROUPBLOCKORDERS');
      RegisterMethod(@TKMScriptActions.GroupDisableHungryMessage, 'GROUPDISABLEHUNGRYMESSAGE');
      RegisterMethod(@TKMScriptActions.GroupHungerSet,            'GROUPHUNGERSET');
      RegisterMethod(@TKMScriptActions.GroupKillAll,              'GROUPKILLALL');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackHouse,     'GROUPORDERATTACKHOUSE');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackUnit,      'GROUPORDERATTACKUNIT');
      RegisterMethod(@TKMScriptActions.GroupOrderFood,            'GROUPORDERFOOD');
      RegisterMethod(@TKMScriptActions.GroupOrderHalt,            'GROUPORDERHALT');
      RegisterMethod(@TKMScriptActions.GroupOrderLink,            'GROUPORDERLINK');
      RegisterMethod(@TKMScriptActions.GroupOrderSplit,           'GROUPORDERSPLIT');
      RegisterMethod(@TKMScriptActions.GroupOrderSplitUnit,       'GROUPORDERSPLITUNIT');
      RegisterMethod(@TKMScriptActions.GroupOrderStorm,           'GROUPORDERSTORM');
      RegisterMethod(@TKMScriptActions.GroupOrderWalk,            'GROUPORDERWALK');
      RegisterMethod(@TKMScriptActions.GroupSetFormation,         'GROUPSETFORMATION');

      RegisterMethod(@TKMScriptActions.HouseAddBuildingMaterials,     'HOUSEADDBUILDINGMATERIALS');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingProgress,      'HOUSEADDBUILDINGPROGRESS');
      RegisterMethod(@TKMScriptActions.HouseAddDamage,                'HOUSEADDDAMAGE');
      RegisterMethod(@TKMScriptActions.HouseAddRepair,                'HOUSEADDREPAIR');
      RegisterMethod(@TKMScriptActions.HouseAddWaresTo,               'HOUSEADDWARESTO');
      RegisterMethod(@TKMScriptActions.HouseAllow,                    'HOUSEALLOW');
      RegisterMethod(@TKMScriptActions.HouseBarracksEquip,            'HOUSEBARRACKSEQUIP');
      RegisterMethod(@TKMScriptActions.HouseBarracksGiveRecruit,      'HOUSEBARRACKSGIVERECRUIT');
      RegisterMethod(@TKMScriptActions.HouseDeliveryBlock,            'HOUSEDELIVERYBLOCK');
      RegisterMethod(@TKMScriptActions.HouseDisableUnoccupiedMessage, 'HOUSEDISABLEUNOCCUPIEDMESSAGE');
      RegisterMethod(@TKMScriptActions.HouseDestroy,                  'HOUSEDESTROY');
      RegisterMethod(@TKMScriptActions.HouseRepairEnable,             'HOUSEREPAIRENABLE');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueAdd,           'HOUSESCHOOLQUEUEADD');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueRemove,        'HOUSESCHOOLQUEUEREMOVE');
      RegisterMethod(@TKMScriptActions.HouseTakeWaresFrom,            'HOUSETAKEWARESFROM');
      RegisterMethod(@TKMScriptActions.HouseUnlock,                   'HOUSEUNLOCK');
      RegisterMethod(@TKMScriptActions.HouseWoodcutterChopOnly,       'HOUSEWOODCUTTERCHOPONLY');
      RegisterMethod(@TKMScriptActions.HouseWareBlock,                'HOUSEWAREBLOCK');
      RegisterMethod(@TKMScriptActions.HouseWeaponsOrderSet,          'HOUSEWEAPONSORDERSET');

      RegisterMethod(@TKMScriptActions.Log,                        'LOG');
      RegisterMethod(@TKMScriptActions.MarketSetTrade,             'MARKETSETTRADE');

      RegisterMethod(@TKMScriptActions.MapTileSet,                 'MAPTILESET');
      RegisterMethod(@TKMScriptActions.MapTileHeightSet,           'MAPTILEHEIGHTSET');
      RegisterMethod(@TKMScriptActions.MapTileObjectSet,           'MAPTILEOBJECTSET');

      RegisterMethod(@TKMScriptActions.OverlayTextSet,             'OVERLAYTEXTSET');
      RegisterMethod(@TKMScriptActions.OverlayTextSetFormatted,    'OVERLAYTEXTSETFORMATTED');
      RegisterMethod(@TKMScriptActions.OverlayTextAppend,          'OVERLAYTEXTAPPEND');
      RegisterMethod(@TKMScriptActions.OverlayTextAppendFormatted, 'OVERLAYTEXTAPPENDFORMATTED');

      RegisterMethod(@TKMScriptActions.PlanAddField,      'PLANADDFIELD');
      RegisterMethod(@TKMScriptActions.PlanAddHouse,      'PLANADDHOUSE');
      RegisterMethod(@TKMScriptActions.PlanAddRoad,       'PLANADDROAD');
      RegisterMethod(@TKMScriptActions.PlanAddWinefield,  'PLANADDWINEFIELD');
      RegisterMethod(@TKMScriptActions.PlanConnectRoad,   'PLANCONNECTROAD');
      RegisterMethod(@TKMScriptActions.PlanRemove,        'PLANREMOVE');

      RegisterMethod(@TKMScriptActions.PlayerAllianceChange,  'PLAYERALLIANCECHANGE');
      RegisterMethod(@TKMScriptActions.PlayerAddDefaultGoals, 'PLAYERADDDEFAULTGOALS');
      RegisterMethod(@TKMScriptActions.PlayerDefeat,          'PLAYERDEFEAT');
      RegisterMethod(@TKMScriptActions.PlayerShareBeacons,    'PLAYERSHAREBEACONS');
      RegisterMethod(@TKMScriptActions.PlayerShareFog,        'PLAYERSHAREFOG');
      RegisterMethod(@TKMScriptActions.PlayerWareDistribution,'PLAYERWAREDISTRIBUTION');
      RegisterMethod(@TKMScriptActions.PlayerWin,             'PLAYERWIN');

      RegisterMethod(@TKMScriptActions.PlayWAV,                 'PLAYWAV');
      RegisterMethod(@TKMScriptActions.PlayWAVFadeMusic,        'PLAYWAVFADEMUSIC');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocation,       'PLAYWAVATLOCATION');
      RegisterMethod(@TKMScriptActions.PlayWAVLooped,           'PLAYWAVLOOPED');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocationLooped, 'PLAYWAVATLOCATIONLOOPED');
      RegisterMethod(@TKMScriptActions.StopLoopedWAV,           'STOPLOOPEDWAV');

      RegisterMethod(@TKMScriptActions.RemoveRoad,            'REMOVEROAD');

      RegisterMethod(@TKMScriptActions.SetTradeAllowed,       'SETTRADEALLOWED');
      RegisterMethod(@TKMScriptActions.ShowMsg,               'SHOWMSG');
      RegisterMethod(@TKMScriptActions.ShowMsgFormatted,      'SHOWMSGFORMATTED');
      RegisterMethod(@TKMScriptActions.ShowMsgGoto,           'SHOWMSGGOTO');
      RegisterMethod(@TKMScriptActions.ShowMsgGotoFormatted,  'SHOWMSGGOTOFORMATTED');

      RegisterMethod(@TKMScriptActions.UnitBlock,         'UNITBLOCK');
      RegisterMethod(@TKMScriptActions.UnitDirectionSet,  'UNITDIRECTIONSET');
      RegisterMethod(@TKMScriptActions.UnitHPChange,      'UNITHPCHANGE');
      RegisterMethod(@TKMScriptActions.UnitHPSetInvulnerable,'UNITHPSETINVULNERABLE');
      RegisterMethod(@TKMScriptActions.UnitHungerSet,     'UNITHUNGERSET');
      RegisterMethod(@TKMScriptActions.UnitKill,          'UNITKILL');
      RegisterMethod(@TKMScriptActions.UnitOrderWalk,     'UNITORDERWALK');
    end;

    //Append classes info to Exec
    RegisterClassLibraryRuntime(fExec, ClassImp);

    if not fExec.LoadData(fByteCode) then // Load the data from the Data string.
    begin
      { For some reason the script could not be loaded. This is usually the case when a
        library that has been used at compile time isn't registered at runtime. }
      fErrorString := fErrorString + 'Unknown error in loading bytecode to Exec|';
      Exit;
    end;

    //Check global variables in script to be only of supported type
    for I := 0 to fExec.GetVarCount - 1 do
    begin
      V := fExec.GetVarNo(I);
      //Promote to Unicode just to make compiler happy
      if SameText(UnicodeString(V.FType.ExportName), 'TKMScriptStates')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptActions') then
        Continue;

      fErrorString := fErrorString + ValidateVarType(V.FType);
      if fErrorString <> '' then
      begin
        //Don't allow the script to run
        fExec.Clear;
        Exit;
      end;
    end;

    //Link script objects with objects
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('STATES')), fStates);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('ACTIONS')), fActions);
  finally
    ClassImp.Free;
  end;

  //Link events into the script
  gScriptEvents.LinkEvents;
end;


procedure TKMScripting.ExportDataToText;
var
  s: string;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    IFPS3DataToText(fByteCode, s);
    SL.Text := s;
    ForceDirectories(ExeDir  + 'Export' + PathDelim);
    SL.SaveToFile(ExeDir + 'Export' + PathDelim + 'script_DataText.txt');
  finally
    SL.Free;
  end;
end;


procedure TKMScripting.LoadVar(LoadStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
var
  ElemCount: Integer;
  I: Integer;
  Offset: Cardinal;
begin
  //See uPSRuntime line 1630 for algo idea
  case aType.BaseType of
    btU8:            LoadStream.Read(tbtu8(Src^)); //Byte, Boolean
    btS32:           LoadStream.Read(tbts32(Src^)); //Integer
    btSingle:        LoadStream.Read(tbtsingle(Src^));
    btString:        LoadStream.ReadA(tbtString(Src^));
    btStaticArray:begin
                    LoadStream.Read(ElemCount);
                    Assert(ElemCount = TPSTypeRec_StaticArray(aType).Size, 'Script array element count mismatches saved count');
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      LoadVar(LoadStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btArray:      begin
                    LoadStream.Read(ElemCount);
                    PSDynArraySetLength(Pointer(Src^), aType, ElemCount);
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      LoadVar(LoadStream, Pointer(IPointer(Src^) + Offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btRecord:     begin
                    LoadStream.Read(ElemCount);
                    Assert(ElemCount = TPSTypeRec_Record(aType).FieldTypes.Count, 'Script record element count mismatches saved count');
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[I]);
                      LoadVar(LoadStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Record(aType).FieldTypes[I]);
                    end;
                  end;
    btSet:        begin
                    LoadStream.Read(ElemCount);
                    Assert(ElemCount = TPSTypeRec_Set(aType).RealSize, 'Script set element count mismatches saved count');
                    LoadStream.Read(Src^, ElemCount);
                  end;
    //Already checked and reported as an error in LinkRuntime, no need to crash it here
    //else Assert(False);
  end;
end;


procedure TKMScripting.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  LoadStream.ReadAssert('Script');
  LoadStream.ReadHugeString(fScriptCode);
  LoadStream.ReadA(fCampaignDataTypeCode);

  if fScriptCode <> '' then
    CompileScript;

  //Read script variables
  LoadStream.Read(I);
  Assert(I = fExec.GetVarCount, 'Script variable count mismatches saved variables count');
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    LoadVar(LoadStream, @PPSVariantData(V).Data, V.FType);
  end;

  //The log path can't be stored in the save since it might be in MapsMP or MapsDL on different clients
  fScriptLogFile := ExeDir + ChangeFileExt(gGame.GetMissionFile, SCRIPT_LOG_EXT);
  if not DirectoryExists(ExtractFilePath(fScriptLogFile)) then
    fScriptLogFile := '';
end;


procedure TKMScripting.LoadCampaignData(LoadStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
  S: AnsiString;
begin
  //Campaign data format might change. If so, do not load it
  LoadStream.ReadA(S);
  if S <> fCampaignDataTypeCode then
    Exit;

  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    if V.FType.ExportName = FastUppercase(CAMPAIGN_DATA_TYPE) then
    begin
      LoadVar(LoadStream, @PPSVariantData(V).Data, V.FType);
      Exit;
    end;
  end;
end;


procedure TKMScripting.SaveVar(SaveStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
var
  ElemCount: Integer;
  I: Integer;
  Offset: Cardinal;
begin
  //See uPSRuntime line 1630 for algo idea
  case aType.BaseType of
    btU8:            SaveStream.Write(tbtu8(Src^)); //Byte, Boolean
    btS32:           SaveStream.Write(tbts32(Src^)); //Integer
    btSingle:        SaveStream.Write(tbtsingle(Src^));
    btString:        SaveStream.WriteA(tbtString(Src^));
    btStaticArray:begin
                    ElemCount := TPSTypeRec_StaticArray(aType).Size;
                    SaveStream.Write(ElemCount);
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      SaveVar(SaveStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btArray:      begin
                    ElemCount := PSDynArrayGetLength(Pointer(Src^), aType);
                    SaveStream.Write(ElemCount);
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      SaveVar(SaveStream, Pointer(IPointer(Src^) + Offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btRecord:     begin
                    ElemCount := TPSTypeRec_Record(aType).FieldTypes.Count;
                    SaveStream.Write(ElemCount);
                    for I := 0 to ElemCount - 1 do
                    begin
                      Offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[I]);
                      SaveVar(SaveStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Record(aType).FieldTypes[I]);
                    end;
                  end;
    btSet:        begin
                    ElemCount := TPSTypeRec_Set(aType).RealSize;
                    SaveStream.Write(ElemCount);
                    SaveStream.Write(Src^, ElemCount);
                  end;
    //Already checked and reported as an error in LinkRuntime, no need to crash it here
    //else Assert(False);
  end;
end;


procedure TKMScripting.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  SaveStream.WriteA('Script');

  //Write script code
  SaveStream.WriteHugeString(fScriptCode);
  SaveStream.WriteA(fCampaignDataTypeCode);

  //Write script global variables
  SaveStream.Write(fExec.GetVarCount);
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    SaveVar(SaveStream, @PPSVariantData(V).Data, V.FType);
  end;
end;


procedure TKMScripting.SaveCampaignData(SaveStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  SaveStream.WriteA(fCampaignDataTypeCode);
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    if V.FType.ExportName = FastUppercase(CAMPAIGN_DATA_TYPE) then
    begin
      SaveVar(SaveStream, @PPSVariantData(V).Data, V.FType);
      Exit;
    end;
  end;
end;


procedure TKMScripting.UpdateState;
begin
  gScriptEvents.ProcTick;
  fIDCache.UpdateState;
end;


end.
