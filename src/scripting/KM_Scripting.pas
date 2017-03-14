unit KM_Scripting;
{$I KaM_Remake.inc}
{$WARN IMPLICIT_STRING_CAST OFF}
interface
uses
  Classes, SysUtils,
  uPSCompiler, uPSRuntime, uPSUtils, uPSDisassembly, uPSDebugger, uPSPreProcessor,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ScriptingActions, KM_ScriptingEvents, KM_ScriptingIdCache, KM_ScriptingStates, KM_ScriptingTypes, KM_ScriptingUtils,
  KM_Houses, KM_Units, KM_UnitGroups, KM_ResHouses;

  //Dynamic scripts allow mapmakers to control the mission flow

  //Three classes exposed to scripting States, Actions and Utils

  //All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)

  //How to add new a method exposed to the scripting? Three steps:
  //1. Add method to published section here below
  //2. Add method declaration to Compiler (TKMScripting.ScriptOnUses)
  //3. Add method name to Runtime (TKMScripting.LinkRuntime)

type


  // Scripts can be included one into another with PreProcessor directives {$I filename.script} or {$INCLUDE filename.script}
  // This structure collects included files info
  TKMScriptFilesCollection = class
  private
    fMainFileInfo: TKMScriptFileInfo;
    fMainFilePath: UnicodeString;
    fHasDefDirectives: Boolean;
    fIncludedCnt: Integer;
    fIncluded: array of TKMScriptFileInfo;

    function GetIncluded(aIndex: Integer): TKMScriptFileInfo;

  public
    constructor Create;

    property Included[I: Integer]: TKMScriptFileInfo read GetIncluded; default;
    property IncludedCount: Integer read fIncludedCnt;
    procedure AddIncludeInfo(aIncludeInfo: TKMScriptFileInfo);
    function FindCodeLine(aLine: AnsiString; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;
  end;


  TKMScriptErrorHandler = class
  private
    fErrorString: TKMScriptErrorMessage; //Info about found mistakes (Unicode, can be localized later on)
    fWarningsString: TKMScriptErrorMessage;

    fHasErrorOccured: Boolean; //Has runtime error occurred? (only display first error)
    fScriptLogFile: UnicodeString;
    fOnScriptError: TUnicodeStringEvent;
    procedure SetScriptLogFile(aScriptLogFile: UnicodeString);
    function AppendErrorPrefix(aPrefix: UnicodeString; var aError: TKMScriptErrorMessage): TKMScriptErrorMessage;
  public
    constructor Create(aOnScriptError: TUnicodeStringEvent);

    property ScriptLogFile: UnicodeString read fScriptLogFile write SetScriptLogFile;

    procedure HandleScriptError(aType: TKMScriptErrorType; aError: TKMScriptErrorMessage);
    procedure HandleScriptErrorString(aType: TKMScriptErrorType; aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
    function HasErrors: Boolean;
    procedure AppendError(aError: TKMScriptErrorMessage);
    procedure AppendWarning(aWarning: TKMScriptErrorMessage);
    procedure AppendErrorStr(aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
    procedure AppendWarningStr(aWarningString: UnicodeString; aDetailedWarningString: UnicodeString = '');
    procedure HandleErrors;
  end;


  TKMScriptingPreProcessor = class
  private
    fDestroyErrorHandler: Boolean;
    fScriptFilesInfo: TKMScriptFilesCollection;
    fErrorHandler: TKMScriptErrorHandler;

    procedure AfterPreProcess;
    procedure BeforePreProcess(aMainFileName: UnicodeString; aMainFileText: AnsiString);

    function ScriptOnNeedFile(Sender: TPSPreProcessor; const aCallingFileName: AnsiString; var aFileName, aOutput: AnsiString): Boolean;
    procedure ScriptOnProcessDirective(Sender: TPSPreProcessor; Parser: TPSPascalPreProcessorParser; const Active: Boolean;
                                        const DirectiveName, DirectiveParam: tbtString; var Continue: Boolean);
  public
    constructor Create; overload;
    constructor Create(aOnScriptError: TUnicodeStringEvent); overload;
    constructor Create(aOnScriptError: TUnicodeStringEvent; aErrorHandler: TKMScriptErrorHandler); overload;
    destructor Destroy; override;

    property ScriptFilesInfo: TKMScriptFilesCollection read fScriptFilesInfo;

    function ScriptMightChangeAfterPreProcessing: Boolean;
    function PreProcessFile(aFileName: UnicodeString): Boolean; overload;
    function PreProcessFile(aFileName: UnicodeString; var aScriptCode: AnsiString): Boolean; overload;
  end;


  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fCampaignDataTypeCode: AnsiString;
    fByteCode: AnsiString;
    fDebugByteCode: AnsiString;
    fExec: TPSDebugExec;

    fErrorHandler: TKMScriptErrorHandler;
    fPreProcessor: TKMScriptingPreProcessor;

    fStates: TKMScriptStates;
    fActions: TKMScriptActions;
    fIDCache: TKMScriptingIdCache;
    fUtils: TKMScriptUtils;

    procedure CompileScript;
    procedure LinkRuntime;

    procedure SaveVar(SaveStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
    procedure LoadVar(LoadStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);

    function GetScriptFilesInfo: TKMScriptFilesCollection;
    function GetCodeLine(aRowNum: Cardinal): AnsiString;
    function FindCodeLine(aRowNumber: Integer; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;
  public
    constructor Create(aOnScriptError: TUnicodeStringEvent);
    destructor Destroy; override;

    function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
    procedure ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Longint; ProcNo, Position: Cardinal; const PropData: tbtString);
    function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;

    property ScriptCode: AnsiString read fScriptCode;
    property ScriptFilesInfo: TKMScriptFilesCollection read GetScriptFilesInfo;
    property PreProcessor: TKMScriptingPreProcessor read fPreProcessor;

    function GetErrorMessage(aErrorMsg: TPSPascalCompilerMessage): TKMScriptErrorMessage; overload;
    function GetErrorMessage(aErrorType, aShortErrorDescription: UnicodeString; aRow, aCol: Integer): TKMScriptErrorMessage; overload;

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
  Math, KromUtils, KM_Game, KM_Log, KM_Utils;

const
  SCRIPT_LOG_EXT = '.log.txt';


{Regular procedures and functions to wrap TKMScripting procedures and functions}
function ScriptOnUsesFunc(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
begin
  Result := False;
  if gGame <> nil then
    Result := gGame.Scripting.ScriptOnUses(Sender, Name);
end;


procedure ScriptOnUseVariableProc(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Integer; ProcNo, Position: Cardinal; const PropData: tbtString);
begin
  if gGame <> nil  then
    gGame.Scripting.ScriptOnUseVariable(Sender, VarType, VarNo, ProcNo, Position, PropData);
end;


function ScriptOnExportCheckFunc(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
begin
  Result := False;
  if gGame <> nil then
    Result := gGame.Scripting.ScriptOnExportCheck(Sender, Proc, ProcDecl);
end;


{ TKMScripting }
constructor TKMScripting.Create(aOnScriptError: TUnicodeStringEvent);
begin
  inherited Create;

  // Create an instance of the script executer
  fExec := TPSDebugExec.Create;
  fIDCache := TKMScriptingIdCache.Create;

  // Global object to get events
  gScriptEvents := TKMScriptEvents.Create(fExec, fIDCache);
  fStates := TKMScriptStates.Create(fIDCache);
  fActions := TKMScriptActions.Create(fIDCache);
  fUtils := TKMScriptUtils.Create(fIDCache);

  fErrorHandler := TKMScriptErrorHandler.Create(aOnScriptError);
  fPreProcessor := TKMScriptingPreProcessor.Create(aOnScriptError, fErrorHandler); //Use same error handler for PreProcessor and Scripting

  gScriptEvents.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fStates.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fActions.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fUtils.OnScriptError := fErrorHandler.HandleScriptErrorString;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(gScriptEvents);
  FreeAndNil(fStates);
  FreeAndNil(fActions);
  FreeAndNil(fIDCache);
  FreeAndNil(fExec);
  FreeAndNil(fUtils);
  FreeAndNil(fErrorHandler);
  FreeAndNil(fPreProcessor);
  inherited;
end;


procedure TKMScripting.LoadFromFile(aFileName: UnicodeString; aCampaignDataTypeFile: UnicodeString; aCampaignData: TKMemoryStream);
begin
  if not fPreProcessor.PreProcessFile(aFileName, fScriptCode) then
    Exit; // Continue only if PreProcess was successful;

  if (aCampaignDataTypeFile <> '') and FileExists(aCampaignDataTypeFile) then
    fCampaignDataTypeCode := ReadTextA(aCampaignDataTypeFile)
  else
    fCampaignDataTypeCode := '';

  CompileScript;

  fErrorHandler.HandleErrors;

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
          fErrorHandler.AppendErrorStr('Error in declaration of global campaign data type|');
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

      RegisterMethod('function Format(aFormatting: string; aData: array of const): string;');

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
  Procs: array [0..23] of record
    Names: AnsiString;
    ParamCount: Byte;
    Typ: array [0..4] of Byte;
    Dir: array [0..3] of TPSParameterMode;
  end =
  (
  (Names: 'OnGroupHungry';          ParamCount: 1; Typ: (0, btS32, 0,     0,     0    ); Dir: (pmIn, pmIn, pmIn, pmIn)),

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
    Compiler.OnUses := ScriptOnUsesFunc; // assign the OnUses event
    Compiler.OnUseVariable := ScriptOnUseVariableProc;
    Compiler.OnExportCheck := ScriptOnExportCheckFunc; // Assign the onExportCheck event

    Compiler.AllowNoEnd := True; //Scripts only use event handlers now, main section is unused
    Compiler.BooleanShortCircuit := True; //Like unchecking "Complete booolean evaluation" in Delphi compiler options

    if not Compiler.Compile(fScriptCode) then  // Compile the Pascal script into bytecode
    begin
      for I := 0 to Compiler.MsgCount - 1 do
        fErrorHandler.AppendError(GetErrorMessage(Compiler.Msg[I]));
      Exit;
    end
      else
        for I := 0 to Compiler.MsgCount - 1 do
          fErrorHandler.AppendWarning(GetErrorMessage(Compiler.Msg[I]));

    Compiler.GetOutput(fByteCode);            // Save the output of the compiler in the string Data.
    Compiler.GetDebugOutput(fDebugByteCode);  // Save the debug output of the compiler
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

      RegisterMethod(@TKMScriptUtils.Format,                                    'Format');

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
      fErrorHandler.AppendErrorStr('Unknown error in loading bytecode to Exec|');
      Exit;
    end;

    if fExec.DebugEnabled then
      fExec.LoadDebugData(fDebugByteCode);

    //Check global variables in script to be only of supported type
    for I := 0 to fExec.GetVarCount - 1 do
    begin
      V := fExec.GetVarNo(I);
      //Promote to Unicode just to make compiler happy
      if SameText(UnicodeString(V.FType.ExportName), 'TKMScriptStates')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptActions')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptUtils') then
        Continue;

      fErrorHandler.AppendErrorStr(ValidateVarType(V.FType));
      if fErrorHandler.HasErrors then
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
  fErrorHandler.ScriptLogFile := ExeDir + ChangeFileExt(gGame.GetMissionFile, SCRIPT_LOG_EXT);
end;


function TKMScripting.GetScriptFilesInfo: TKMScriptFilesCollection;
begin
  Result := fPreProcessor.fScriptFilesInfo;
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


function TKMScripting.GetCodeLine(aRowNum: Cardinal): AnsiString;
var Strings: TStringList;
begin
  Strings := TStringList.Create;
  Strings.Text := fScriptCode;
  Result := AnsiString(Strings[aRowNum - 1]);
  Strings.Free;
end;


function TKMScripting.FindCodeLine(aRowNumber: Integer; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;
begin
  Result := fPreProcessor.fScriptFilesInfo.FindCodeLine(GetCodeLine(aRowNumber), aFileNamesArr, aRowsArr);
end;


function TKMScripting.GetErrorMessage(aErrorMsg: TPSPascalCompilerMessage): TKMScriptErrorMessage;
begin
  Result := GetErrorMessage(aErrorMsg.ErrorType, EolW + '[' + aErrorMsg.ErrorType + '] ' + aErrorMsg.ShortMessageToString + EolW, aErrorMsg.Row, aErrorMsg.Col);
end;


function TKMScripting.GetErrorMessage(aErrorType, aShortErrorDescription: UnicodeString; aRow, aCol: Integer): TKMScriptErrorMessage;
var I, CodeLinesFound: Integer;
    FileNamesArr: TStringArray;
    RowsArr: TIntegerArray;
    ErrorMsg, DetailedErrorMsg, ErrorMsgTemplate, FirstError, ErrorTemplate: UnicodeString;
begin
  ErrorMsg := '';
  ErrorTemplate := 'in ''%s'' at [%d:%d]';

  // Most of the scripts probably will not use Include or Define directives.
  // Then script code after pre-processing should be identical to original main script file
  // That mean we do not need to find line of code by it text (which could be indefinite due to multiple code lines with the same text)
  // But use aRow parameter instead
  if not fPreProcessor.ScriptMightChangeAfterPreProcessing then
    ErrorMsg := Format(ErrorTemplate, [ScriptFilesInfo.fMainFileInfo.FileName, aRow, aCol])
  else
  begin
    //Try to find line of code in all script files (main file and included files)
    CodeLinesFound := FindCodeLine(aRow, FileNamesArr, RowsArr);
    case CodeLinesFound of
      0:    ;
      1:    ErrorMsg := Format(ErrorTemplate, [FileNamesArr[0], RowsArr[0], aCol]);
      else  begin
              // Its unlikely, but possible, if we find several lines with the same code. Lets show them all then
              ErrorMsg := 'Actual ' + aErrorType + ' position couldn''t be recognised.' + EolW;
              DetailedErrorMsg := ErrorMsg;
              ErrorMsg := ErrorMsg + 'Check log for details. First position:' + EolW;
              // Show first position in game message, while all others - in the chat
              FirstError := Format(ErrorTemplate, [FileNamesArr[0], RowsArr[0], aCol]);
              ErrorMsg := ErrorMsg + FirstError;
              DetailedErrorMsg := DetailedErrorMsg + 'Possible positions: ' + EolW + FirstError;
              // Other possible error positions are appended to detailed message
              for I := 1 to CodeLinesFound - 1 do
                DetailedErrorMsg := DetailedErrorMsg + EolW + Format(ErrorTemplate, [FileNamesArr[I], RowsArr[I], aCol]);
            end;
    end;
  end;

  ErrorMsgTemplate := aShortErrorDescription + '%s' + EolW;

  // Show game message only for errors. Do not show it for hints or warnings.
  if aErrorType = 'Error' then
    Result.GameMessage := Format(ErrorMsgTemplate, [ErrorMsg])
  else
    Result.GameMessage := '';

  if DetailedErrorMsg <> '' then
    Result.LogMessage := Format(ErrorMsgTemplate, [DetailedErrorMsg])
  else
    Result.LogMessage := Format(ErrorMsgTemplate, [ErrorMsg]);
end;


{TKMScriptErrorHandler}
constructor TKMScriptErrorHandler.Create(aOnScriptError: TUnicodeStringEvent);
begin
  fOnScriptError := aOnScriptError;
end;


procedure TKMScriptErrorHandler.AppendError(aError: TKMScriptErrorMessage);
begin
  fErrorString.GameMessage := fErrorString.GameMessage + aError.GameMessage;
  fErrorString.LogMessage := fErrorString.LogMessage + aError.LogMessage;
end;


procedure TKMScriptErrorHandler.AppendWarning(aWarning: TKMScriptErrorMessage);
begin
  fWarningsString.GameMessage := fWarningsString.GameMessage + aWarning.GameMessage;
  fWarningsString.LogMessage := fWarningsString.LogMessage + aWarning.LogMessage;
end;


procedure TKMScriptErrorHandler.AppendErrorStr(aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
begin
  fErrorString.GameMessage := fErrorString.GameMessage + aErrorString;
  fErrorString.LogMessage := fErrorString.LogMessage + aDetailedErrorString;
end;


procedure TKMScriptErrorHandler.AppendWarningStr(aWarningString: UnicodeString; aDetailedWarningString: UnicodeString = '');
begin
  fWarningsString.GameMessage := fWarningsString.GameMessage + aWarningString;
  fWarningsString.LogMessage := fWarningsString.LogMessage + aDetailedWarningString;
end;


function TKMScriptErrorHandler.AppendErrorPrefix(aPrefix: UnicodeString; var aError: TKMScriptErrorMessage): TKMScriptErrorMessage;
begin
  // Append prefix only for non-empty messages
  if aError.GameMessage <> '' then
    aError.GameMessage := aPrefix + aError.GameMessage;

  if aError.LogMessage <> '' then
    aError.LogMessage := aPrefix + aError.LogMessage;
  Result := aError;
end;


function TKMScriptErrorHandler.HasErrors: Boolean;
begin
  Result := fErrorString.GameMessage <> '';
end;


procedure TKMScriptErrorHandler.HandleErrors;
begin
  HandleScriptError(se_CompileError, AppendErrorPrefix('Script compile errors:' + EolW, fErrorString));
  HandleScriptError(se_CompileWarning, AppendErrorPrefix('Script compile warnings:' + EolW, fWarningsString));
end;


procedure TKMScriptErrorHandler.SetScriptLogFile(aScriptLogFile: UnicodeString);
begin
  fScriptLogFile := aScriptLogFile;
  if not DirectoryExists(ExtractFilePath(fScriptLogFile)) then
    fScriptLogFile := '';
end;


procedure TKMScriptErrorHandler.HandleScriptError(aType: TKMScriptErrorType; aError: TKMScriptErrorMessage);
begin
  HandleScriptErrorString(aType, aError.GameMessage, aError.LogMessage);
end;


procedure TKMScriptErrorHandler.HandleScriptErrorString(aType: TKMScriptErrorType; aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
var
  fl: TextFile;
  LogErrorMsg: UnicodeString;
begin
  if aDetailedErrorString <> '' then
    LogErrorMsg := aDetailedErrorString
  else
    LogErrorMsg := aErrorString;

  if LogErrorMsg = '' then //No errors occur
    Exit;

  gLog.AddTime('Script: ' + LogErrorMsg); //Always log the error to global game log

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
    WriteLn(fl, Format('%23s   %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), LogErrorMsg]));
    CloseFile(fl);
  end;

  aErrorString := StringReplace(aErrorString, EolW, '|', [rfReplaceAll]);

  //Display compile errors in-game
  if (aType in [se_CompileError, se_PreprocessorError]) and Assigned(fOnScriptError) then
    fOnScriptError(aErrorString);

  //Serious runtime errors should be shown to the player
  if aType in [se_Exception] then
  begin
    //Only show the first message in-game to avoid spamming the player
    if not fHasErrorOccured and Assigned(fOnScriptError) then
      fOnScriptError('Error(s) have occured in the mission script. ' +
                     'Please check the log file for further details. First error:||' + aErrorString);
    fHasErrorOccured := True;
  end;
end;


{TKMScriptingPreProcessor}
constructor TKMScriptingPreProcessor.Create;
begin
  Create(nil);
end;


constructor TKMScriptingPreProcessor.Create(aOnScriptError: TUnicodeStringEvent);
begin
  Create(aOnScriptError, TKMScriptErrorHandler.Create(aOnScriptError));
  fDestroyErrorHandler := True;
end;


constructor TKMScriptingPreProcessor.Create(aOnScriptError: TUnicodeStringEvent; aErrorHandler: TKMScriptErrorHandler);
begin
  fScriptFilesInfo := TKMScriptFilesCollection.Create;

  fErrorHandler := aErrorHandler;
  fDestroyErrorHandler := False;
end;


destructor TKMScriptingPreProcessor.Destroy;
begin
  FreeAndNil(fScriptFilesInfo);
  //Error Handler could be destroyed already
  if fDestroyErrorHandler then
    FreeAndNil(fErrorHandler);
  inherited;
end;


procedure TKMScriptingPreProcessor.BeforePreProcess(aMainFileName: UnicodeString; aMainFileText: AnsiString);
begin
  fScriptFilesInfo.fMainFilePath := ExtractFilePath(aMainFileName);
  fScriptFilesInfo.fMainFileInfo.FullFilePath := aMainFileName;
  fScriptFilesInfo.fMainFileInfo.FileName := ExtractFileName(aMainFileName);
  fScriptFilesInfo.fMainFileInfo.FileText := aMainFileText;
end;


procedure TKMScriptingPreProcessor.AfterPreProcess;
begin
  SetLength(fScriptFilesInfo.fIncluded, fScriptFilesInfo.fIncludedCnt);
end;


function TKMScriptingPreProcessor.ScriptMightChangeAfterPreProcessing: Boolean;
begin
  Result := (fScriptFilesInfo.fIncludedCnt <> 0) or fScriptFilesInfo.fHasDefDirectives;
end;


function TKMScriptingPreProcessor.PreProcessFile(aFileName: UnicodeString): Boolean;
var ScriptCode: AnsiString;
begin
  Result := PreProcessFile(aFileName, ScriptCode);
end;


function TKMScriptingPreProcessor.PreProcessFile(aFileName: UnicodeString; var aScriptCode: AnsiString): Boolean;
var
  PreProcessor: TPSPreProcessor;
  MainScriptCode: AnsiString;
begin
  Result := False;
  fErrorHandler.ScriptLogFile := ChangeFileExt(aFileName, SCRIPT_LOG_EXT);

  if not FileExists(aFileName) then
  begin
    gLog.AddNoTime(aFileName + ' was not found. It is okay for mission to have no dynamic scripts.');
    Exit;
  end;

  MainScriptCode := ReadTextA(aFileName);
  PreProcessor := TPSPreProcessor.Create;
  try
    PreProcessor.OnNeedFile := ScriptOnNeedFile;
    PreProcessor.OnProcessDirective := ScriptOnProcessDirective;
    PreProcessor.MainFileName := AnsiString(aFileName);
    PreProcessor.MainFile := MainScriptCode;
    BeforePreProcess(aFileName, MainScriptCode);
    try
      PreProcessor.PreProcess(PreProcessor.MainFileName, aScriptCode);
      AfterPreProcess;
      Result := True; // If PreProcess has been done succesfully
    except
      on E: Exception do
        fErrorHandler.HandleScriptErrorString(se_PreprocessorError, 'Script preprocessing errors:' + EolW + E.Message);
    end;
  finally
    PreProcessor.Free;
  end;
end;


procedure TKMScriptingPreProcessor.ScriptOnProcessDirective(Sender: TPSPreProcessor; Parser: TPSPascalPreProcessorParser; const Active: Boolean;
                                                            const DirectiveName, DirectiveParam: tbtString; var Continue: Boolean);
begin
  // Most of the scripts do not have directives.
  // save in fHasDefDirectives, when script do have IFDEF or IFNDEF directive, which can change script code after pre-processing
  if not fScriptFilesInfo.fHasDefDirectives
    and Active
    and ((DirectiveName = 'IFDEF') or (DirectiveName = 'IFNDEF')) then
    fScriptFilesInfo.fHasDefDirectives := True;
end;


function TKMScriptingPreProcessor.ScriptOnNeedFile(Sender: TPSPreProcessor; const aCallingFileName: AnsiString; var aFileName, aOutput: AnsiString): Boolean;
var
  S, FileExt: String;
  IncludedScriptFileInfo: TKMScriptFileInfo;
begin
  Result := False;

  S := ExtractFilePath(aCallingFileName);
  if S = '' then S := ExtractFilePath(ParamStr(0));
  aFileName := AnsiString(S) + aFileName;

  FileExt := ExtractFileExt(aFileName);
  // Check included file extension
  if FileExt <> '.script' then
    raise Exception.Create(Format('Error including ''%s'' from ''%s'': |Wrong extension: ''%s''',
                                  [ExtractFileName(aFileName), ExtractFileName(aCallingFileName), FileExt]));

  // Check included file folder
  if ExtractFilePath(aFileName) <> fScriptFilesInfo.fMainFilePath then
    raise Exception.Create(Format('Error including ''%s'' from ''%s'': |included script files should be in the same folder as main script file',
                                  [aFileName, ExtractFileName(aCallingFileName)]));

  if FileExists(aFileName) then
  begin
    aOutput := ReadTextA(aFileName);

    IncludedScriptFileInfo.FullFilePath := aFileName;
    IncludedScriptFileInfo.FileName := ExtractFileName(aFileName);
    IncludedScriptFileInfo.FileText := aOutput;

    fScriptFilesInfo.AddIncludeInfo(IncludedScriptFileInfo);

    Result := True;
  end;
end;


{TKMScriptFilesCollection}
constructor TKMScriptFilesCollection.Create;
begin
  fIncludedCnt := 0;
  fHasDefDirectives := False;
  SetLength(fIncluded, 8);
end;


procedure TKMScriptFilesCollection.AddIncludeInfo(aIncludeInfo: TKMScriptFileInfo);
begin
  if Length(fIncluded) >= fIncludedCnt then
    SetLength(fIncluded, fIncludedCnt + 8);

  fIncluded[fIncludedCnt] := aIncludeInfo;
  Inc(fIncludedCnt);
end;


function TKMScriptFilesCollection.GetIncluded(aIndex: Integer): TKMScriptFileInfo;
begin
  Result := fIncluded[aIndex];
end;


//Try to find line of code in all script files
//Return number of occurences
function TKMScriptFilesCollection.FindCodeLine(aLine: AnsiString; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;

  procedure AddFoundLineInfo(var aFoundCnt: Integer; aFileNameFound: UnicodeString; aRowFound: Integer);
  begin
    if (aFoundCnt >= Length(aFileNamesArr))
      or (aFoundCnt >= Length(aRowsArr)) then
    begin
      SetLength(aFileNamesArr, aFoundCnt + 8);
      SetLength(aRowsArr, aFoundCnt + 8);
    end;

    aFileNamesArr[aFoundCnt] := aFileNameFound;
    aRowsArr[aFoundCnt] := aRowFound;

    Inc(aFoundCnt);
  end;

  procedure FindLine(var aFoundCnt: Integer; aScriptFileInfo: TKMScriptFileInfo; var aStrings: TStringList);
  var I: Integer;
  begin
    aStrings.Clear;
    aStrings.Text := aScriptFileInfo.FileText;

    //Find all occurences of aLine in FileText
    for I := 0 to aStrings.Count - 1 do
      if aStrings[I] = aLine then
        AddFoundLineInfo(aFoundCnt, aScriptFileInfo.FileName, I + 1);
  end;
var Strings: TStringList;
    I, aFoundCnt: Integer;
begin
  Strings := TStringList.Create; // Create TStringList only once for all files

  aFoundCnt := 0;
  //Find in main script file first
  FindLine(aFoundCnt, fMainFileInfo, Strings);

  for I := 0 to fIncludedCnt - 1 do
    //then find in included script files
    FindLine(aFoundCnt, fIncluded[I], Strings);

  Result := aFoundCnt;
  Strings.Free;
end;


end.
