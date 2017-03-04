unit KM_Scripting;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  uPSCompiler, uPSRuntime, uPSUtils, uPSDisassembly,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ScriptingActions, KM_ScriptingEvents, KM_ScriptingIdCache, KM_ScriptingStates, KM_ScriptingUtils,
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
    fUtils: TKMScriptUtils;

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
  fUtils := TKMScriptUtils.Create(fIDCache);

  fOnScriptError := aOnScriptError;
  gScriptEvents.OnScriptError := HandleScriptError;
  fStates.OnScriptError := HandleScriptError;
  fActions.OnScriptError := HandleScriptError;
  fUtils.OnScriptError := HandleScriptError;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(gScriptEvents);
  FreeAndNil(fStates);
  FreeAndNil(fActions);
  FreeAndNil(fIDCache);
  FreeAndNil(fExec);
  FreeAndNil(fUtils);
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
  if (aType = se_CompileError) and Assigned(fOnScriptError) then
    fOnScriptError(StringReplace(aMsg, EolW, '|', [rfReplaceAll]));

  //Serious runtime errors should be shown to the player
  if aType in [se_Exception] then
  begin
    //Only show the first message in-game to avoid spamming the player
    if not fHasErrorOccured and Assigned(fOnScriptError) then
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
      RegisterMethod('function ClosestGroupMultipleTypes(aPlayer, X, Y: Integer; aGroupTypes: TByteSet): Integer');
      RegisterMethod('function ClosestHouse(aPlayer, X, Y, aHouseType: Integer): Integer');
      RegisterMethod('function ClosestHouseMultipleTypes(aPlayer, X, Y: Integer; aHouseTypes: TByteSet): Integer');
      RegisterMethod('function ClosestUnit(aPlayer, X, Y, aUnitType: Integer): Integer');
      RegisterMethod('function ClosestUnitMultipleTypes(aPlayer, X, Y: Integer; aUnitTypes: TByteSet): Integer');

      RegisterMethod('function ConnectedByRoad(X1, Y1, X2, Y2: Integer): Boolean');
      RegisterMethod('function ConnectedByWalking(X1, Y1, X2, Y2: Integer): Boolean');

      RegisterMethod('function FogRevealed(aPlayer: Byte; aX, aY: Word): Boolean');

      RegisterMethod('function GameTime: Cardinal');

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
      RegisterMethod('function HouseOwner(aHouseID: Integer): Integer');
      RegisterMethod('function HousePositionX(aHouseID: Integer): Integer');
      RegisterMethod('function HousePositionY(aHouseID: Integer): Integer');
      RegisterMethod('function HouseRepair(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseResourceAmount(aHouseID, aResource: Integer): Integer');
      RegisterMethod('function HouseSchoolQueue(aHouseID, QueueIndex: Integer): Integer');
      RegisterMethod('function HouseSiteIsDigged(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseType(aHouseID: Integer): Integer');
      RegisterMethod('function HouseTypeMaxHealth(aHouseType: Integer): Word');
      RegisterMethod('function HouseTypeName(aHouseType: Byte): AnsiString');
      RegisterMethod('function HouseTypeToOccupantType(aHouseID: Integer): Integer');
      RegisterMethod('function HouseUnlocked(aPlayer, aHouseType: Word): Boolean');
      RegisterMethod('function HouseWoodcutterChopOnly(aHouseID: Integer): Boolean');
      RegisterMethod('function HouseWareBlocked(aHouseID, aWareType: Integer): Boolean');
      RegisterMethod('function HouseWeaponsOrdered(aHouseID, aWareType: Integer): Integer');

      RegisterMethod('function IsFieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');
      RegisterMethod('function IsRoadAt(aPlayer: ShortInt; X, Y: Word): Boolean');
      RegisterMethod('function IsWinefieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');

      RegisterMethod('function KaMRandom: Single');
      RegisterMethod('function KaMRandomI(aMax:Integer): Integer');

      RegisterMethod('function LocationCount: Integer');

      RegisterMethod('function MapHeight: Integer');
      RegisterMethod('function MapTileHeight(X, Y: Integer): Integer');
      RegisterMethod('function MapTileObject(X, Y: Integer): Integer');
      RegisterMethod('function MapTilePassability(X, Y: Integer; aPassability: Byte): Boolean');
      RegisterMethod('function MapTileRotation(X, Y: Integer): Integer');
      RegisterMethod('function MapTileType(X, Y: Integer): Integer');
      RegisterMethod('function MapWidth: Integer');

      RegisterMethod('function MarketFromWare(aMarketID: Integer): Integer');
      RegisterMethod('function MarketLossFactor: Single');
      RegisterMethod('function MarketOrderAmount(aMarketID: Integer): Integer');
      RegisterMethod('function MarketToWare(aMarketID: Integer): Integer');
      RegisterMethod('function MarketValue(aRes: Integer): Single');

      RegisterMethod('function PeaceTime: Cardinal');

      RegisterMethod('function PlayerAllianceCheck(aPlayer1, aPlayer2: Byte): Boolean');
      RegisterMethod('function PlayerColorText(aPlayer: Byte): AnsiString');
      RegisterMethod('function PlayerDefeated(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerEnabled(aPlayer: Byte): Boolean');
      RegisterMethod('function PlayerGetAllGroups(aPlayer: Byte): TIntegerArray');
      RegisterMethod('function PlayerGetAllHouses(aPlayer: Byte): TIntegerArray');
      RegisterMethod('function PlayerGetAllUnits(aPlayer: Byte): TIntegerArray');
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
      RegisterMethod('function UnitsGroup(aUnitID: Integer): Integer');
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

      RegisterMethod('procedure CinematicEnd(aPlayer: Byte)');
      RegisterMethod('procedure CinematicPanTo(aPlayer: Byte; X, Y, Duration: Word)');
      RegisterMethod('procedure CinematicStart(aPlayer: Byte)');

      RegisterMethod('procedure FogCoverAll(aPlayer: Byte)');
      RegisterMethod('procedure FogCoverCircle(aPlayer, X, Y, aRadius: Word)');
      RegisterMethod('procedure FogCoverRect(aPlayer, X1, Y1, X2, Y2: Word)');
      RegisterMethod('procedure FogRevealAll(aPlayer: Byte)');
      RegisterMethod('procedure FogRevealCircle(aPlayer, X, Y, aRadius: Word)');
      RegisterMethod('procedure FogRevealRect(aPlayer, X1, Y1, X2, Y2: Word)');

      RegisterMethod('function  GiveAnimal(aType, X,Y: Word): Integer');
      RegisterMethod('function  GiveField(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  GiveFieldAged(aPlayer, X, Y: Word; aStage: Byte; aRandomAge: Boolean): Boolean');
      RegisterMethod('function  GiveGroup(aPlayer, aType, X, Y, aDir, aCount, aColumns: Word): Integer');
      RegisterMethod('function  GiveHouse(aPlayer, aHouseType, X,Y: Integer): Integer');
      RegisterMethod('function  GiveHouseSite(aPlayer, aHouseType, X, Y: Integer; aAddMaterials: Boolean): Integer');
      RegisterMethod('function  GiveRoad(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  GiveUnit(aPlayer, aType, X,Y, aDir: Word): Integer');
      RegisterMethod('procedure GiveWares(aPlayer, aType, aCount: Word)');
      RegisterMethod('procedure GiveWeapons(aPlayer, aType, aCount: Word)');
      RegisterMethod('function  GiveWineField(aPlayer, X, Y: Word): Boolean');
      RegisterMethod('function  GiveWineFieldAged(aPlayer, X, Y: Word; aStage: Byte; aRandomAge: Boolean): Boolean');

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

      RegisterMethod('function MapTileHeightSet(X, Y, Height: Integer): Boolean');
      RegisterMethod('function MapTileObjectSet(X, Y, Obj: Integer): Boolean');
      RegisterMethod('function MapTileSet(X, Y, aType, aRotation: Integer): Boolean');

      RegisterMethod('procedure OverlayTextAppend(aPlayer: Shortint; aText: AnsiString)');
      RegisterMethod('procedure OverlayTextAppendFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');
      RegisterMethod('procedure OverlayTextSet(aPlayer: Shortint; aText: AnsiString)');
      RegisterMethod('procedure OverlayTextSetFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');

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
      RegisterMethod('procedure PlayWAVAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word)');
      RegisterMethod('function  PlayWAVAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
      RegisterMethod('procedure PlayWAVFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single)');
      RegisterMethod('function  PlayWAVLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
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

    with Sender.AddClassN(nil, AnsiString(fUtils.ClassName)) do
    begin
      RegisterMethod('function AbsI(aValue: Integer): Integer');
      RegisterMethod('function AbsS(aValue: Single): Single');

      RegisterMethod('function ArrayElementCount(aElement: AnsiString; aArray: array of AnsiString): Integer');
      RegisterMethod('function ArrayElementCountB(aElement: Boolean; aArray: array of Boolean): Integer');
      RegisterMethod('function ArrayElementCountI(aElement: Integer; aArray: array of Integer): Integer');
      RegisterMethod('function ArrayElementCountS(aElement: Single; aArray: array of Single): Integer');

      RegisterMethod('function ArrayHasElement(aElement: AnsiString; aArray: array of AnsiString): Boolean');
      RegisterMethod('function ArrayHasElementB(aElement: Boolean; aArray: array of Boolean): Boolean');
      RegisterMethod('function ArrayHasElementI(aElement: Integer; aArray: array of Integer): Boolean');
      RegisterMethod('function ArrayHasElementS(aElement: Single; aArray: array of Single): Boolean');

      RegisterMethod('function EnsureRangeS(aValue, aMin, aMax: Single): Single');
      RegisterMethod('function EnsureRangeI(aValue, aMin, aMax: Integer): Integer');

      RegisterMethod('function IfThen(aBool: Boolean; aTrue, aFalse: AnsiString): AnsiString');
      RegisterMethod('function IfThenI(aBool: Boolean; aTrue, aFalse: Integer): Integer');
      RegisterMethod('function IfThenS(aBool: Boolean; aTrue, aFalse: Single): Single');

      RegisterMethod('function InAreaI(aX, aY, aXMin, aYMin, aXMax, aYMax: Integer): Boolean');
      RegisterMethod('function InAreaS(aX, aY, aXMin, aYMin, aXMax, aYMax: Single): Boolean');

      RegisterMethod('function InRangeI(aValue, aMin, aMax: Integer): Boolean');
      RegisterMethod('function InRangeS(aValue, aMin, aMax: Single): Boolean');

      RegisterMethod('function MaxI(A, B: Integer): Integer');
      RegisterMethod('function MaxS(A, B: Single): Single');

      RegisterMethod('function MaxInArrayI(aArray: array of Integer): Integer');
      RegisterMethod('function MaxInArrayS(aArray: array of Single): Single');

      RegisterMethod('function MinI(A, B: Integer): Integer');
      RegisterMethod('function MinS(A, B: Single): Single');

      RegisterMethod('function MinInArrayI(aArray: array of Integer): Integer');
      RegisterMethod('function MinInArrayS(aArray: array of Single): Single');

      RegisterMethod('function Power(Base, Exponent: Extended): Extended');

      RegisterMethod('function Sqr(A: Extended): Extended');

      RegisterMethod('function SumI(aArray: array of Integer): Integer');
      RegisterMethod('function SumS(aArray: array of Single): Single');

      RegisterMethod('function TimeToString(aTicks: Integer): AnsiString');
    end;

    //Register objects
    AddImportedClassVariable(Sender, 'States', AnsiString(fStates.ClassName));
    AddImportedClassVariable(Sender, 'Actions', AnsiString(fActions.ClassName));
    AddImportedClassVariable(Sender, 'Utils', AnsiString(fUtils.ClassName));

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
  Procs: array [0..27] of record
    Names: AnsiString;
    ParamCount: Byte;
    Typ: array [0..4] of Byte;
    Dir: array [0..3] of TPSParameterMode;
  end =
  (
  (Names: 'OnGroupHungry';          ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnGroupOrderAttackHouse';ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnGroupOrderAttackUnit'; ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnGroupOrderLink';       ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnGroupOrderSplit';      ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnHouseAfterDestroyed';  ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnHouseBuilt';           ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnHouseDamaged';         ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnHouseDestroyed';       ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnHousePlanPlaced';      ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnHousePlanRemoved';     ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnMarketTrade';          ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnMissionStart';         ParamCount: 0; Typ: (0, 0,     0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnPlanFieldPlaced';      ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlanFieldRemoved';     ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlanRoadPlaced';       ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlanRoadRemoved';      ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlanWinefieldPlaced';  ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlanWinefieldRemoved'; ParamCount: 3; Typ: (0, btS32, btS32, btS32, 0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnPlayerDefeated';       ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnPlayerVictory';        ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnTick';                 ParamCount: 0; Typ: (0, 0,     0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnUnitAfterDied';        ParamCount: 4; Typ: (0, btS32, btS32, btS32, btS32); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnUnitAttacked';         ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnUnitDied';             ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnUnitTrained';          ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),
  (Names: 'OnUnitWounded';          ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

  (Names: 'OnWarriorEquipped';      ParamCount: 2; Typ: (0, btS32, btS32, 0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn))
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
    //Register classes and their exposed methods to Runtime
    //(uppercase is not needed, FastUpperCase does this well. See uPSRuntime.pas, line 11387)
    with ClassImp.Add(TKMScriptStates) do
    begin

      RegisterMethod(@TKMScriptStates.ClosestGroup,                             'ClosestGroup');
      RegisterMethod(@TKMScriptStates.ClosestGroupMultipleTypes,                'ClosestGroupMultipleTypes');
      RegisterMethod(@TKMScriptStates.ClosestHouse,                             'ClosestHouse');
      RegisterMethod(@TKMScriptStates.ClosestHouseMultipleTypes,                'ClosestHouseMultipleTypes');
      RegisterMethod(@TKMScriptStates.ClosestUnit,                              'ClosestUnit');
      RegisterMethod(@TKMScriptStates.ClosestUnitMultipleTypes,                 'ClosestUnitMultipleTypes');

      RegisterMethod(@TKMScriptStates.ConnectedByRoad,                          'ConnectedByRoad');
      RegisterMethod(@TKMScriptStates.ConnectedByWalking,                       'ConnectedByWalking');

      RegisterMethod(@TKMScriptStates.FogRevealed,                              'FogRevealed');

      RegisterMethod(@TKMScriptStates.GameTime,                                 'GameTime');

      RegisterMethod(@TKMScriptStates.GroupAt,                                  'GroupAt');
      RegisterMethod(@TKMScriptStates.GroupColumnCount,                         'GroupColumnCount');
      RegisterMethod(@TKMScriptStates.GroupDead,                                'GroupDead');
      RegisterMethod(@TKMScriptStates.GroupIdle,                                'GroupIdle');
      RegisterMethod(@TKMScriptStates.GroupMember,                              'GroupMember');
      RegisterMethod(@TKMScriptStates.GroupMemberCount,                         'GroupMemberCount');
      RegisterMethod(@TKMScriptStates.GroupOwner,                               'GroupOwner');
      RegisterMethod(@TKMScriptStates.GroupType,                                'GroupType');

      RegisterMethod(@TKMScriptStates.HouseAt,                                  'HouseAt');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointX,                 'HouseBarracksRallyPointX');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointY,                 'HouseBarracksRallyPointY');
      RegisterMethod(@TKMScriptStates.HouseBuildingProgress,                    'HouseBuildingProgress');
      RegisterMethod(@TKMScriptStates.HouseCanReachResources,                   'HouseCanReachResources');
      RegisterMethod(@TKMScriptStates.HouseDamage,                              'HouseDamage');
      RegisterMethod(@TKMScriptStates.HouseDeliveryBlocked,                     'HouseDeliveryBlocked');
      RegisterMethod(@TKMScriptStates.HouseDestroyed,                           'HouseDestroyed');
      RegisterMethod(@TKMScriptStates.HouseHasOccupant,                         'HouseHasOccupant');
      RegisterMethod(@TKMScriptStates.HouseIsComplete,                          'HouseIsComplete');
      RegisterMethod(@TKMScriptStates.HouseOwner,                               'HouseOwner');
      RegisterMethod(@TKMScriptStates.HousePositionX,                           'HousePositionX');
      RegisterMethod(@TKMScriptStates.HousePositionY,                           'HousePositionY');
      RegisterMethod(@TKMScriptStates.HouseRepair,                              'HouseRepair');
      RegisterMethod(@TKMScriptStates.HouseResourceAmount,                      'HouseResourceAmount');
      RegisterMethod(@TKMScriptStates.HouseSchoolQueue,                         'HouseSchoolQueue');
      RegisterMethod(@TKMScriptStates.HouseSiteIsDigged,                        'HouseSiteIsDigged');
      RegisterMethod(@TKMScriptStates.HouseType,                                'HouseType');
      RegisterMethod(@TKMScriptStates.HouseTypeMaxHealth,                       'HouseTypeMaxHealth');
      RegisterMethod(@TKMScriptStates.HouseTypeName,                            'HouseTypeName');
      RegisterMethod(@TKMScriptStates.HouseTypeToOccupantType,                  'HouseTypeToOccupantType');
      RegisterMethod(@TKMScriptStates.HouseUnlocked,                            'HouseUnlocked');
      RegisterMethod(@TKMScriptStates.HouseWoodcutterChopOnly,                  'HouseWoodcutterChopOnly');
      RegisterMethod(@TKMScriptStates.HouseWareBlocked,                         'HouseWareBlocked');
      RegisterMethod(@TKMScriptStates.HouseWeaponsOrdered,                      'HouseWeaponsOrdered');

      RegisterMethod(@TKMScriptStates.IsFieldAt,                                'IsFieldAt');
      RegisterMethod(@TKMScriptStates.IsRoadAt,                                 'IsRoadAt');
      RegisterMethod(@TKMScriptStates.IsWinefieldAt,                            'IsWinefieldAt');

      RegisterMethod(@TKMScriptStates.KaMRandom,                                'KaMRandom');
      RegisterMethod(@TKMScriptStates.KaMRandomI,                               'KaMRandomI');

      RegisterMethod(@TKMScriptStates.LocationCount,                            'LocationCount');

      RegisterMethod(@TKMScriptStates.MapHeight,                                'MapHeight');
      RegisterMethod(@TKMScriptStates.MapTileHeight,                            'MapTileHeight');
      RegisterMethod(@TKMScriptStates.MapTileObject,                            'MapTileObject');
      RegisterMethod(@TKMScriptStates.MapTilePassability,                       'MapTilePassability');
      RegisterMethod(@TKMScriptStates.MapTileRotation,                          'MapTileRotation');
      RegisterMethod(@TKMScriptStates.MapTileType,                              'MapTileType');
      RegisterMethod(@TKMScriptStates.MapWidth,                                 'MapWidth');

      RegisterMethod(@TKMScriptStates.MarketFromWare,                           'MarketFromWare');
      RegisterMethod(@TKMScriptStates.MarketLossFactor,                         'MarketLossFactor');
      RegisterMethod(@TKMScriptStates.MarketOrderAmount,                        'MarketOrderAmount');
      RegisterMethod(@TKMScriptStates.MarketToWare,                             'MarketToWare');
      RegisterMethod(@TKMScriptStates.MarketValue,                              'MarketValue');

      RegisterMethod(@TKMScriptStates.PeaceTime,                                'PeaceTime');

      RegisterMethod(@TKMScriptStates.PlayerAllianceCheck,                      'PlayerAllianceCheck');
      RegisterMethod(@TKMScriptStates.PlayerColorText,                          'PlayerColorText');
      RegisterMethod(@TKMScriptStates.PlayerDefeated,                           'PlayerDefeated');
      RegisterMethod(@TKMScriptStates.PlayerEnabled,                            'PlayerEnabled');
      RegisterMethod(@TKMScriptStates.PlayerGetAllGroups,                       'PlayerGetAllGroups');
      RegisterMethod(@TKMScriptStates.PlayerGetAllHouses,                       'PlayerGetAllHouses');
      RegisterMethod(@TKMScriptStates.PlayerGetAllUnits,                        'PlayerGetAllUnits');
      RegisterMethod(@TKMScriptStates.PlayerIsAI,                               'PlayerIsAI');
      RegisterMethod(@TKMScriptStates.PlayerName,                               'PlayerName');
      RegisterMethod(@TKMScriptStates.PlayerVictorious,                         'PlayerVictorious');
      RegisterMethod(@TKMScriptStates.PlayerWareDistribution,                   'PlayerWareDistribution');

      RegisterMethod(@TKMScriptStates.StatAIDefencePositionsCount,              'StatAIDefencePositionsCount');
      RegisterMethod(@TKMScriptStates.StatArmyCount,                            'StatArmyCount');
      RegisterMethod(@TKMScriptStates.StatCitizenCount,                         'StatCitizenCount');
      RegisterMethod(@TKMScriptStates.StatHouseMultipleTypesCount,              'StatHouseMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatHouseTypeCount,                       'StatHouseTypeCount');
      RegisterMethod(@TKMScriptStates.StatHouseTypePlansCount,                  'StatHouseTypePlansCount');
      RegisterMethod(@TKMScriptStates.StatPlayerCount,                          'StatPlayerCount');
      RegisterMethod(@TKMScriptStates.StatResourceProducedCount,                'StatResourceProducedCount');
      RegisterMethod(@TKMScriptStates.StatResourceProducedMultipleTypesCount,   'StatResourceProducedMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitCount,                            'StatUnitCount');
      RegisterMethod(@TKMScriptStates.StatUnitKilledCount,                      'StatUnitKilledCount');
      RegisterMethod(@TKMScriptStates.StatUnitKilledMultipleTypesCount,         'StatUnitKilledMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitLostCount,                        'StatUnitLostCount');
      RegisterMethod(@TKMScriptStates.StatUnitLostMultipleTypesCount,           'StatUnitLostMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitMultipleTypesCount,               'StatUnitMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitTypeCount,                        'StatUnitTypeCount');

      RegisterMethod(@TKMScriptStates.UnitAt,                                   'UnitAt');
      RegisterMethod(@TKMScriptStates.UnitCarrying,                             'UnitCarrying');
      RegisterMethod(@TKMScriptStates.UnitDead,                                 'UnitDead');
      RegisterMethod(@TKMScriptStates.UnitDirection,                            'UnitDirection');
      RegisterMethod(@TKMScriptStates.UnitHome,                                 'UnitHome');
      RegisterMethod(@TKMScriptStates.UnitHPCurrent,                            'UnitHPCurrent');
      RegisterMethod(@TKMScriptStates.UnitHPMax,                                'UnitHPMax');
      RegisterMethod(@TKMScriptStates.UnitHPInvulnerable,                       'UnitHPInvulnerable');
      RegisterMethod(@TKMScriptStates.UnitHunger,                               'UnitHunger');
      RegisterMethod(@TKMScriptStates.UnitIdle,                                 'UnitIdle');
      RegisterMethod(@TKMScriptStates.UnitLowHunger,                            'UnitLowHunger');
      RegisterMethod(@TKMScriptStates.UnitMaxHunger,                            'UnitMaxHunger');
      RegisterMethod(@TKMScriptStates.UnitOwner,                                'UnitOwner');
      RegisterMethod(@TKMScriptStates.UnitPositionX,                            'UnitPositionX');
      RegisterMethod(@TKMScriptStates.UnitPositionY,                            'UnitPositionY');
      RegisterMethod(@TKMScriptStates.UnitsGroup,                               'UnitsGroup');
      RegisterMethod(@TKMScriptStates.UnitType,                                 'UnitType');
      RegisterMethod(@TKMScriptStates.UnitTypeName,                             'UnitTypeName');

      RegisterMethod(@TKMScriptStates.WareTypeName,                             'WareTypeName');
    end;

    with ClassImp.Add(TKMScriptActions) do
    begin
      RegisterMethod(@TKMScriptActions.AIAutoAttackRange,                       'AIAutoAttackRange');
      RegisterMethod(@TKMScriptActions.AIAutoBuild,                             'AIAutoBuild');
      RegisterMethod(@TKMScriptActions.AIAutoDefence,                           'AIAutoDefence');
      RegisterMethod(@TKMScriptActions.AIAutoRepair,                            'AIAutoRepair');
      RegisterMethod(@TKMScriptActions.AIDefencePositionAdd,                    'AIDefencePositionAdd');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemove,                 'AIDefencePositionRemove');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemoveAll,              'AIDefencePositionRemoveAll');
      RegisterMethod(@TKMScriptActions.AIDefendAllies,                          'AIDefendAllies');
      RegisterMethod(@TKMScriptActions.AIEquipRate,                             'AIEquipRate');
      RegisterMethod(@TKMScriptActions.AIGroupsFormationSet,                    'AIGroupsFormationSet');
      RegisterMethod(@TKMScriptActions.AIRecruitDelay,                          'AIRecruitDelay');
      RegisterMethod(@TKMScriptActions.AIRecruitLimit,                          'AIRecruitLimit');
      RegisterMethod(@TKMScriptActions.AISerfsPerHouse,                         'AISerfsPerHouse');
      RegisterMethod(@TKMScriptActions.AISoldiersLimit,                         'AISoldiersLimit');
      RegisterMethod(@TKMScriptActions.AIStartPosition,                         'AIStartPosition');
      RegisterMethod(@TKMScriptActions.AIWorkerLimit,                           'AIWorkerLimit');

      RegisterMethod(@TKMScriptActions.CinematicEnd,                            'CinematicEnd');
      RegisterMethod(@TKMScriptActions.CinematicPanTo,                          'CinematicPanTo');
      RegisterMethod(@TKMScriptActions.CinematicStart,                          'CinematicStart');

      RegisterMethod(@TKMScriptActions.FogCoverAll,                             'FogCoverAll');
      RegisterMethod(@TKMScriptActions.FogCoverCircle,                          'FogCoverCircle');
      RegisterMethod(@TKMScriptActions.FogCoverRect,                            'FogCoverRect');
      RegisterMethod(@TKMScriptActions.FogRevealAll,                            'FogRevealAll');
      RegisterMethod(@TKMScriptActions.FogRevealCircle,                         'FogRevealCircle');
      RegisterMethod(@TKMScriptActions.FogRevealRect,                           'FogRevealRect');

      RegisterMethod(@TKMScriptActions.GiveAnimal,                              'GiveAnimal');
      RegisterMethod(@TKMScriptActions.GiveField,                               'GiveField');
      RegisterMethod(@TKMScriptActions.GiveFieldAged,                           'GiveFieldAged');
      RegisterMethod(@TKMScriptActions.GiveGroup,                               'GiveGroup');
      RegisterMethod(@TKMScriptActions.GiveUnit,                                'GiveUnit');
      RegisterMethod(@TKMScriptActions.GiveHouse,                               'GiveHouse');
      RegisterMethod(@TKMScriptActions.GiveHouseSite,                           'GiveHouseSite');
      RegisterMethod(@TKMScriptActions.GiveRoad,                                'GiveRoad');
      RegisterMethod(@TKMScriptActions.GiveWares,                               'GiveWares');
      RegisterMethod(@TKMScriptActions.GiveWeapons,                             'GiveWeapons');
      RegisterMethod(@TKMScriptActions.GiveWineField,                           'GiveWineField');
      RegisterMethod(@TKMScriptActions.GiveWineFieldAged,                       'GiveWineFieldAged');

      RegisterMethod(@TKMScriptActions.GroupBlockOrders,                        'GroupBlockOrders');
      RegisterMethod(@TKMScriptActions.GroupDisableHungryMessage,               'GroupDisableHungryMessage');
      RegisterMethod(@TKMScriptActions.GroupHungerSet,                          'GroupHungerSet');
      RegisterMethod(@TKMScriptActions.GroupKillAll,                            'GroupKillAll');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackHouse,                   'GroupOrderAttackHouse');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackUnit,                    'GroupOrderAttackUnit');
      RegisterMethod(@TKMScriptActions.GroupOrderFood,                          'GroupOrderFood');
      RegisterMethod(@TKMScriptActions.GroupOrderHalt,                          'GroupOrderHalt');
      RegisterMethod(@TKMScriptActions.GroupOrderLink,                          'GroupOrderLink');
      RegisterMethod(@TKMScriptActions.GroupOrderSplit,                         'GroupOrderSplit');
      RegisterMethod(@TKMScriptActions.GroupOrderSplitUnit,                     'GroupOrderSplitUnit');
      RegisterMethod(@TKMScriptActions.GroupOrderStorm,                         'GroupOrderStorm');
      RegisterMethod(@TKMScriptActions.GroupOrderWalk,                          'GroupOrderWalk');
      RegisterMethod(@TKMScriptActions.GroupSetFormation,                       'GroupSetFormation');

      RegisterMethod(@TKMScriptActions.HouseAddBuildingMaterials,               'HouseAddBuildingMaterials');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingProgress,                'HouseAddBuildingProgress');
      RegisterMethod(@TKMScriptActions.HouseAddDamage,                          'HouseAddDamage');
      RegisterMethod(@TKMScriptActions.HouseAddRepair,                          'HouseAddRepair');
      RegisterMethod(@TKMScriptActions.HouseAddWaresTo,                         'HouseAddWaresTo');
      RegisterMethod(@TKMScriptActions.HouseAllow,                              'HouseAllow');
      RegisterMethod(@TKMScriptActions.HouseBarracksEquip,                      'HouseBarracksEquip');
      RegisterMethod(@TKMScriptActions.HouseBarracksGiveRecruit,                'HouseBarracksGiveRecruit');
      RegisterMethod(@TKMScriptActions.HouseDeliveryBlock,                      'HouseDeliveryBlock');
      RegisterMethod(@TKMScriptActions.HouseDisableUnoccupiedMessage,           'HouseDisableUnoccupiedMessage');
      RegisterMethod(@TKMScriptActions.HouseDestroy,                            'HouseDestroy');
      RegisterMethod(@TKMScriptActions.HouseRepairEnable,                       'HouseRepairEnable');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueAdd,                     'HouseSchoolQueueAdd');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueRemove,                  'HouseSchoolQueueRemove');
      RegisterMethod(@TKMScriptActions.HouseTakeWaresFrom,                      'HouseTakeWaresFrom');
      RegisterMethod(@TKMScriptActions.HouseUnlock,                             'HouseUnlock');
      RegisterMethod(@TKMScriptActions.HouseWoodcutterChopOnly,                 'HouseWoodcutterChopOnly');
      RegisterMethod(@TKMScriptActions.HouseWareBlock,                          'HouseWareBlock');
      RegisterMethod(@TKMScriptActions.HouseWeaponsOrderSet,                    'HouseWeaponsOrderSet');

      RegisterMethod(@TKMScriptActions.Log,                                     'Log');

      RegisterMethod(@TKMScriptActions.MarketSetTrade,                          'MarketSetTrade');

      RegisterMethod(@TKMScriptActions.MapTileSet,                              'MapTileSet');
      RegisterMethod(@TKMScriptActions.MapTileHeightSet,                        'MapTileHeightSet');
      RegisterMethod(@TKMScriptActions.MapTileObjectSet,                        'MapTileObjectSet');

      RegisterMethod(@TKMScriptActions.OverlayTextAppend,                       'OverlayTextAppend');
      RegisterMethod(@TKMScriptActions.OverlayTextAppendFormatted,              'OverlayTextAppendFormatted');
      RegisterMethod(@TKMScriptActions.OverlayTextSet,                          'OverlayTextSet');
      RegisterMethod(@TKMScriptActions.OverlayTextSetFormatted,                 'OverlayTextSetFormatted');

      RegisterMethod(@TKMScriptActions.PlanAddField,                            'PlanAddField');
      RegisterMethod(@TKMScriptActions.PlanAddHouse,                            'PlanAddHouse');
      RegisterMethod(@TKMScriptActions.PlanAddRoad,                             'PlanAddRoad');
      RegisterMethod(@TKMScriptActions.PlanAddWinefield,                        'PlanAddWinefield');
      RegisterMethod(@TKMScriptActions.PlanConnectRoad,                         'PlanConnectRoad');
      RegisterMethod(@TKMScriptActions.PlanRemove,                              'PlanRemove');

      RegisterMethod(@TKMScriptActions.PlayerAllianceChange,                    'PlayerAllianceChange');
      RegisterMethod(@TKMScriptActions.PlayerAddDefaultGoals,                   'PlayerAddDefaultGoals');
      RegisterMethod(@TKMScriptActions.PlayerDefeat,                            'PlayerDefeat');
      RegisterMethod(@TKMScriptActions.PlayerShareBeacons,                      'PlayerShareBeacons');
      RegisterMethod(@TKMScriptActions.PlayerShareFog,                          'PlayerShareFog');
      RegisterMethod(@TKMScriptActions.PlayerWareDistribution,                  'PlayerWareDistribution');
      RegisterMethod(@TKMScriptActions.PlayerWin,                               'PlayerWin');

      RegisterMethod(@TKMScriptActions.PlayWAV,                                 'PlayWAV');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocation,                       'PlayWAVAtLocation');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocationLooped,                 'PlayWAVAtLocationLooped');
      RegisterMethod(@TKMScriptActions.PlayWAVFadeMusic,                        'PlayWAVFadeMusic');
      RegisterMethod(@TKMScriptActions.PlayWAVLooped,                           'PlayWAVLooped');
      RegisterMethod(@TKMScriptActions.StopLoopedWAV,                           'StopLoopedWAV');

      RegisterMethod(@TKMScriptActions.RemoveRoad,                              'RemoveRoad');

      RegisterMethod(@TKMScriptActions.SetTradeAllowed,                         'SetTradeAllowed');

      RegisterMethod(@TKMScriptActions.ShowMsg,                                 'ShowMsg');
      RegisterMethod(@TKMScriptActions.ShowMsgFormatted,                        'ShowMsgFormatted');
      RegisterMethod(@TKMScriptActions.ShowMsgGoto,                             'ShowMsgGoto');
      RegisterMethod(@TKMScriptActions.ShowMsgGotoFormatted,                    'ShowMsgGotoFormatted');

      RegisterMethod(@TKMScriptActions.UnitBlock,                               'UnitBlock');
      RegisterMethod(@TKMScriptActions.UnitDirectionSet,                        'UnitDirectionSet');
      RegisterMethod(@TKMScriptActions.UnitHPChange,                            'UnitHPChange');
      RegisterMethod(@TKMScriptActions.UnitHPSetInvulnerable,                   'UnitHPSetInvulnerable');
      RegisterMethod(@TKMScriptActions.UnitHungerSet,                           'UnitHungerSet');
      RegisterMethod(@TKMScriptActions.UnitKill,                                'UnitKill');
      RegisterMethod(@TKMScriptActions.UnitOrderWalk,                           'UnitOrderWalk');
    end;

    with ClassImp.Add(TKMScriptUtils) do
    begin
      RegisterMethod(@TKMScriptUtils.AbsI,                                      'AbsI');
      RegisterMethod(@TKMScriptUtils.AbsS,                                      'AbsS');

      RegisterMethod(@TKMScriptUtils.ArrayElementCount,                         'ArrayElementCount');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountB,                        'ArrayElementCountB');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountI,                        'ArrayElementCountI');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountS,                        'ArrayElementCountS');

      RegisterMethod(@TKMScriptUtils.ArrayHasElement,                           'ArrayHasElement');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementB,                          'ArrayHasElementB');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementI,                          'ArrayHasElementI');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementS,                          'ArrayHasElementS');

      RegisterMethod(@TKMScriptUtils.EnsureRangeI,                              'EnsureRangeI');
      RegisterMethod(@TKMScriptUtils.EnsureRangeS,                              'EnsureRangeS');

      RegisterMethod(@TKMScriptUtils.IfThen,                                    'IfThen');
      RegisterMethod(@TKMScriptUtils.IfThenI,                                   'IfThenI');
      RegisterMethod(@TKMScriptUtils.IfThenS,                                   'IfThenS');

      RegisterMethod(@TKMScriptUtils.InAreaI,                                   'InAreaI');
      RegisterMethod(@TKMScriptUtils.InAreaS,                                   'InAreaS');

      RegisterMethod(@TKMScriptUtils.InRangeI,                                  'InRangeI');
      RegisterMethod(@TKMScriptUtils.InRangeS,                                  'InRangeS');

      RegisterMethod(@TKMScriptUtils.MaxI,                                      'MaxI');
      RegisterMethod(@TKMScriptUtils.MaxS,                                      'MaxS');

      RegisterMethod(@TKMScriptUtils.MaxInArrayI,                               'MaxInArrayI');
      RegisterMethod(@TKMScriptUtils.MaxInArrayS,                               'MaxInArrayS');

      RegisterMethod(@TKMScriptUtils.MinI,                                      'MinI');
      RegisterMethod(@TKMScriptUtils.MinS,                                      'MinS');

      RegisterMethod(@TKMScriptUtils.MinInArrayI,                               'MinInArrayI');
      RegisterMethod(@TKMScriptUtils.MinInArrayS,                               'MinInArrayS');

      RegisterMethod(@TKMScriptUtils.Power,                                     'POWER');

      RegisterMethod(@TKMScriptUtils.SumI,                                      'SumI');
      RegisterMethod(@TKMScriptUtils.SumS,                                      'SumS');

      RegisterMethod(@TKMScriptUtils.Sqr,                                       'Sqr');

      RegisterMethod(@TKMScriptUtils.TimeToString,                              'TimeToString');
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
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptActions')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptUtils') then
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
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('UTILS')), fUtils);
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
