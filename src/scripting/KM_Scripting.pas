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
    function FindCodeLine(const aLine: AnsiString; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;
  end;


  TKMScriptErrorHandler = class
  private
    fErrorString: TKMScriptErrorMessage; //Info about found mistakes (Unicode, can be localized later on)
    fWarningsString: TKMScriptErrorMessage;

    fHasErrorOccured: Boolean; //Has runtime error occurred? (only display first error)
    fScriptLogFile: UnicodeString;
    fOnScriptError: TUnicodeStringEvent;
    procedure SetScriptLogFile(const aScriptLogFile: UnicodeString);
    function AppendErrorPrefix(const aPrefix: UnicodeString; var aError: TKMScriptErrorMessage): TKMScriptErrorMessage;
  public
    constructor Create(aOnScriptError: TUnicodeStringEvent);

    property ScriptLogFile: UnicodeString read fScriptLogFile write SetScriptLogFile;
    property ErrorString: TKMScriptErrorMessage read fErrorString;
    property WarningsString: TKMScriptErrorMessage read fWarningsString;

    procedure HandleScriptError(aType: TKMScriptErrorType; aError: TKMScriptErrorMessage);
    procedure HandleScriptErrorString(aType: TKMScriptErrorType; aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
    function HasErrors: Boolean;
    function HasWarnings: Boolean;
    procedure AppendError(aError: TKMScriptErrorMessage);
    procedure AppendWarning(aWarning: TKMScriptErrorMessage);
    procedure AppendErrorStr(const aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
    procedure AppendWarningStr(const aWarningString: UnicodeString; aDetailedWarningString: UnicodeString = '');
    procedure HandleErrors;
  end;


  TKMScriptingPreProcessor = class
  private
    fDestroyErrorHandler: Boolean;
    fScriptFilesInfo: TKMScriptFilesCollection;
    fErrorHandler: TKMScriptErrorHandler;

    procedure AfterPreProcess;
    procedure BeforePreProcess(const aMainFileName: UnicodeString; const aMainFileText: AnsiString);

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
    function PreProcessFile(const aFileName: UnicodeString): Boolean; overload;
    function PreProcessFile(const aFileName: UnicodeString; var aScriptCode: AnsiString): Boolean; overload;
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
    constructor Create(aOnScriptError: TUnicodeStringEvent); // Scripting has to be created via special TKMScriptingCreator
  public
    destructor Destroy; override;

    property ErrorHandler: TKMScriptErrorHandler read fErrorHandler;

    function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
    procedure ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Longint; ProcNo, Position: Cardinal; const PropData: tbtString);
    function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;

    property ScriptCode: AnsiString read fScriptCode;
    property ScriptFilesInfo: TKMScriptFilesCollection read GetScriptFilesInfo;
    property PreProcessor: TKMScriptingPreProcessor read fPreProcessor;

    function GetErrorMessage(aErrorMsg: TPSPascalCompilerMessage): TKMScriptErrorMessage; overload;
    function GetErrorMessage(aErrorType, aShortErrorDescription: UnicodeString; aRow, aCol: Integer): TKMScriptErrorMessage; overload;

    procedure LoadFromFile(const aFileName, aCampaignDataTypeFile: UnicodeString; aCampaignData: TKMemoryStream);
    procedure ExportDataToText;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure SaveCampaignData(SaveStream: TKMemoryStream);
    procedure LoadCampaignData(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


  //Scripting creator
  TKMScriptingCreator = class
  public
    class function CreateScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
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
  Math, KromUtils, KM_Game, KM_Log, KM_CommonUtils;

const
  SCRIPT_LOG_EXT = '.log.txt';

var
  gScripting: TKMScripting;


{Regular procedures and functions to wrap TKMScripting procedures and functions}
function ScriptOnUsesFunc(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
begin
  Result := False;
  if gScripting <> nil then
    Result := gScripting.ScriptOnUses(Sender, Name);
end;


procedure ScriptOnUseVariableProc(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Integer; ProcNo, Position: Cardinal; const PropData: tbtString);
begin
  if gScripting <> nil  then
    gScripting.ScriptOnUseVariable(Sender, VarType, VarNo, ProcNo, Position, PropData);
end;


function ScriptOnExportCheckFunc(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
begin
  Result := False;
  if gScripting <> nil then
    Result := gScripting.ScriptOnExportCheck(Sender, Proc, ProcDecl);
end;


{ TKMScriptingCreator }
//We need to save pointer to scripting object (in gScripting), as it is used by ScriptOnUsesFunc/ScriptOnUseVariableProc/ScriptOnExportCheckFunc
//These functions are regular methods and need TKMScripting object in global scope
class function TKMScriptingCreator.CreateScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
begin
  if gScripting <> nil then // Should never happen in 1 application, as only 1 TKMScripting object is needed usually
    FreeAndNil(gScripting);

  gScripting := TKMScripting.Create(aOnScriptError);
  Result := gScripting;
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
  gScripting := nil;
  inherited;
end;


procedure TKMScripting.LoadFromFile(const aFileName, aCampaignDataTypeFile: UnicodeString; aCampaignData: TKMemoryStream);
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
  procedure RegisterMethodCheck(aClass: TPSCompileTimeClass; const aDecl: String);
  begin
    // We are fine with Assert, cos it will trigger for devs during development
    if not aClass.RegisterMethod(AnsiString(aDecl)) then
      Assert(False, Format('Error registering "%s"', [aDecl]));
  end;
var
  CampaignDataType: TPSType;
  c: TPSCompileTimeClass;
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

    // Common
    Sender.AddTypeS('TIntegerArray', 'array of Integer'); //Needed for PlayerGetAllUnits
    Sender.AddTypeS('TByteSet', 'set of Byte'); //Needed for Closest*MultipleTypes

    Sender.AddTypeS('TKMAudioFormat', '(af_Wav, af_Ogg)'); //Needed for PlaySound

    // Types needed for MapTilesArraySet function
    Sender.AddTypeS('TKMTileChangeType', '(tctTerrain, tctHeight, tctObject)');
    Sender.AddTypeS('TKMTileChangeTypeSet', 'set of TKMTileChangeType');
    Sender.AddTypeS('TKMTerrainTileBrief', 'record X, Y, Terrain, Rotation, Height, Obj: Byte; ChangeSet: TKMTileChangeTypeSet; end');

    // Register classes and methods to the script engine.
    // After that they can be used from within the script.
    c := Sender.AddClassN(nil, AnsiString(fStates.ClassName));
    RegisterMethodCheck(c, 'function ClosestGroup(aPlayer, X, Y, aGroupType: Integer): Integer');
    RegisterMethodCheck(c, 'function ClosestGroupMultipleTypes(aPlayer, X, Y: Integer; aGroupTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function ClosestHouse(aPlayer, X, Y, aHouseType: Integer): Integer');
    RegisterMethodCheck(c, 'function ClosestHouseMultipleTypes(aPlayer, X, Y: Integer; aHouseTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function ClosestUnit(aPlayer, X, Y, aUnitType: Integer): Integer');
    RegisterMethodCheck(c, 'function ClosestUnitMultipleTypes(aPlayer, X, Y: Integer; aUnitTypes: TByteSet): Integer');

    RegisterMethodCheck(c, 'function ConnectedByRoad(X1, Y1, X2, Y2: Integer): Boolean');
    RegisterMethodCheck(c, 'function ConnectedByWalking(X1, Y1, X2, Y2: Integer): Boolean');

    RegisterMethodCheck(c, 'function FogRevealed(aPlayer: Byte; aX, aY: Word): Boolean');

    RegisterMethodCheck(c, 'function GameTime: Cardinal');

    RegisterMethodCheck(c, 'function GroupAt(aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function GroupColumnCount(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function GroupDead(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function GroupIdle(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function GroupMember(aGroupID, aMemberIndex: Integer): Integer');
    RegisterMethodCheck(c, 'function GroupMemberCount(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function GroupOwner(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function GroupType(aGroupID: Integer): Integer');

    RegisterMethodCheck(c, 'function HouseAt(aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function HouseBarracksRallyPointX(aBarracks: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseBarracksRallyPointY(aBarracks: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseBuildingProgress(aHouseID: Integer): Word');
    RegisterMethodCheck(c, 'function HouseCanReachResources(aHouseID: Integer): Boolean)');
    RegisterMethodCheck(c, 'function HouseDamage(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseDeliveryBlocked(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseDeliveryMode(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseDestroyed(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseHasOccupant(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseIsComplete(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseOwner(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HousePositionX(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HousePositionY(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseRepair(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseResourceAmount(aHouseID, aResource: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseSchoolQueue(aHouseID, QueueIndex: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseSiteIsDigged(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseType(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseTypeMaxHealth(aHouseType: Integer): Word');
    RegisterMethodCheck(c, 'function HouseTypeName(aHouseType: Byte): AnsiString');
    RegisterMethodCheck(c, 'function HouseTypeToOccupantType(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function HouseUnlocked(aPlayer, aHouseType: Word): Boolean');
    RegisterMethodCheck(c, 'function HouseWoodcutterChopOnly(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseWareBlocked(aHouseID, aWareType: Integer): Boolean');
    RegisterMethodCheck(c, 'function HouseWeaponsOrdered(aHouseID, aWareType: Integer): Integer');

    RegisterMethodCheck(c, 'function IsFieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function IsRoadAt(aPlayer: ShortInt; X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function IsWinefieldAt(aPlayer: ShortInt; X, Y: Word): Boolean');

    RegisterMethodCheck(c, 'function KaMRandom: Single');
    RegisterMethodCheck(c, 'function KaMRandomI(aMax:Integer): Integer');

    RegisterMethodCheck(c, 'function LocationCount: Integer');

    RegisterMethodCheck(c, 'function MapHeight: Integer');
    RegisterMethodCheck(c, 'function MapTileHeight(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function MapTileObject(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function MapTilePassability(X, Y: Integer; aPassability: Byte): Boolean');
    RegisterMethodCheck(c, 'function MapTileRotation(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function MapTileType(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function MapWidth: Integer');

    RegisterMethodCheck(c, 'function MarketFromWare(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function MarketLossFactor: Single');
    RegisterMethodCheck(c, 'function MarketOrderAmount(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function MarketToWare(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function MarketValue(aRes: Integer): Single');

    RegisterMethodCheck(c, 'function PeaceTime: Cardinal');

    RegisterMethodCheck(c, 'function PlayerAllianceCheck(aPlayer1, aPlayer2: Byte): Boolean');
    RegisterMethodCheck(c, 'function PlayerColorText(aPlayer: Byte): AnsiString');
    RegisterMethodCheck(c, 'function PlayerDefeated(aPlayer: Byte): Boolean');
    RegisterMethodCheck(c, 'function PlayerEnabled(aPlayer: Byte): Boolean');
    RegisterMethodCheck(c, 'function PlayerGetAllGroups(aPlayer: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function PlayerGetAllHouses(aPlayer: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function PlayerGetAllUnits(aPlayer: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function PlayerIsAI(aPlayer: Byte): Boolean');
    RegisterMethodCheck(c, 'function PlayerName(aPlayer: Byte): AnsiString');
    RegisterMethodCheck(c, 'function PlayerVictorious(aPlayer: Byte): Boolean');
    RegisterMethodCheck(c, 'function PlayerWareDistribution(aPlayer, aWareType, aHouseType: Byte): Byte');

    RegisterMethodCheck(c, 'function StatAIDefencePositionsCount(aPlayer: Byte): Integer');
    RegisterMethodCheck(c, 'function StatArmyCount(aPlayer: Byte): Integer');
    RegisterMethodCheck(c, 'function StatCitizenCount(aPlayer: Byte): Integer');
    RegisterMethodCheck(c, 'function StatHouseMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function StatHouseTypeCount(aPlayer, aHouseType: Byte): Integer');
    RegisterMethodCheck(c, 'function StatHouseTypePlansCount(aPlayer, aHouseType: Byte): Integer');
    RegisterMethodCheck(c, 'function StatPlayerCount: Integer');
    RegisterMethodCheck(c, 'function StatResourceProducedCount(aPlayer, aResType: Byte): Integer');
    RegisterMethodCheck(c, 'function StatResourceProducedMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function StatUnitCount(aPlayer: Byte): Integer');
    RegisterMethodCheck(c, 'function StatUnitKilledCount(aPlayer, aUnitType: Byte): Integer');
    RegisterMethodCheck(c, 'function StatUnitKilledMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function StatUnitLostCount(aPlayer, aUnitType: Byte): Integer');
    RegisterMethodCheck(c, 'function StatUnitLostMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function StatUnitMultipleTypesCount(aPlayer: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function StatUnitTypeCount(aPlayer, aUnitType: Byte): Integer');

    RegisterMethodCheck(c, 'function UnitAt(aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function UnitCarrying(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitDead(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function UnitDirection(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitHome(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitHPCurrent(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitHPMax(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitHPInvulnerable(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function UnitHunger(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitIdle(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function UnitLowHunger: Integer');
    RegisterMethodCheck(c, 'function UnitMaxHunger: Integer');
    RegisterMethodCheck(c, 'function UnitOwner(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitPositionX(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitPositionY(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitsGroup(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitType(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function UnitTypeName(aUnitType: Byte): AnsiString');

    RegisterMethodCheck(c, 'function WareTypeName(aWareType: Byte): AnsiString');

    c := Sender.AddClassN(nil, AnsiString(fActions.ClassName));
    RegisterMethodCheck(c, 'procedure AIAutoAttackRange(aPlayer: Byte; aRange: Word)');
    RegisterMethodCheck(c, 'procedure AIAutoBuild(aPlayer: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'procedure AIAutoDefence(aPlayer: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'procedure AIAutoRepair(aPlayer: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'procedure AIDefencePositionAdd(aPlayer: Byte; X, Y: Integer; aDir, aGroupType: Byte; aRadius: Word; aDefType: Byte)');
    RegisterMethodCheck(c, 'procedure AIDefencePositionRemove(aPlayer: Byte; X, Y: Integer)');
    RegisterMethodCheck(c, 'procedure AIDefencePositionRemoveAll(aPlayer: Byte)');
    RegisterMethodCheck(c, 'procedure AIDefendAllies(aPlayer: Byte; aDefend: Boolean)');
    RegisterMethodCheck(c, 'procedure AIEquipRate(aPlayer: Byte; aType: Byte; aRate: Word)');
    RegisterMethodCheck(c, 'procedure AIGroupsFormationSet(aPlayer, aType: Byte; aCount, aColumns: Word)');
    RegisterMethodCheck(c, 'procedure AIRecruitDelay(aPlayer, aDelay: Cardinal)');
    RegisterMethodCheck(c, 'procedure AIRecruitLimit(aPlayer, aLimit: Byte)');
    RegisterMethodCheck(c, 'procedure AISerfsFactor(aPlayer: Byte; aLimit: Single)');
    RegisterMethodCheck(c, 'procedure AISoldiersLimit(aPlayer: Byte; aLimit: Integer)');
    RegisterMethodCheck(c, 'procedure AIStartPosition(aPlayer: Byte; X, Y: Word)');
    RegisterMethodCheck(c, 'procedure AIWorkerLimit(aPlayer, aLimit: Byte)');

    RegisterMethodCheck(c, 'procedure CinematicEnd(aPlayer: Byte)');
    RegisterMethodCheck(c, 'procedure CinematicPanTo(aPlayer: Byte; X, Y, Duration: Word)');
    RegisterMethodCheck(c, 'procedure CinematicStart(aPlayer: Byte)');

    RegisterMethodCheck(c, 'procedure FogCoverAll(aPlayer: Byte)');
    RegisterMethodCheck(c, 'procedure FogCoverCircle(aPlayer, X, Y, aRadius: Word)');
    RegisterMethodCheck(c, 'procedure FogCoverRect(aPlayer, X1, Y1, X2, Y2: Word)');
    RegisterMethodCheck(c, 'procedure FogRevealAll(aPlayer: Byte)');
    RegisterMethodCheck(c, 'procedure FogRevealCircle(aPlayer, X, Y, aRadius: Word)');
    RegisterMethodCheck(c, 'procedure FogRevealRect(aPlayer, X1, Y1, X2, Y2: Word)');

    RegisterMethodCheck(c, 'function  GiveAnimal(aType, X,Y: Word): Integer');
    RegisterMethodCheck(c, 'function  GiveField(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  GiveFieldAged(aPlayer, X, Y: Word; aStage: Byte; aRandomAge: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  GiveGroup(aPlayer, aType, X, Y, aDir, aCount, aColumns: Word): Integer');
    RegisterMethodCheck(c, 'function  GiveHouse(aPlayer, aHouseType, X,Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveHouseSite(aPlayer, aHouseType, X, Y: Integer; aAddMaterials: Boolean): Integer');
    RegisterMethodCheck(c, 'function  GiveRoad(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  GiveUnit(aPlayer, aType, X,Y, aDir: Word): Integer');
    RegisterMethodCheck(c, 'procedure GiveWares(aPlayer, aType, aCount: Word)');
    RegisterMethodCheck(c, 'procedure GiveWeapons(aPlayer, aType, aCount: Word)');
    RegisterMethodCheck(c, 'function  GiveWineField(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  GiveWineFieldAged(aPlayer, X, Y: Word; aStage: Byte; aRandomAge: Boolean): Boolean');

    RegisterMethodCheck(c, 'procedure GroupBlockOrders(aGroupID: Integer; aBlock: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupDisableHungryMessage(aGroupID: Integer; aDisable: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupHungerSet(aGroupID, aHungerLevel: Integer)');
    RegisterMethodCheck(c, 'procedure GroupKillAll(aGroupID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupOrderAttackHouse(aGroupID, aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderAttackUnit(aGroupID, aUnitID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderFood(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderHalt(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderLink(aGroupID, aDestGroupID: Integer)');
    RegisterMethodCheck(c, 'function  GroupOrderSplit(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupOrderSplitUnit(aGroupID, aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'procedure GroupOrderStorm(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderWalk(aGroupID: Integer; X, Y, aDirection: Word)');
    RegisterMethodCheck(c, 'procedure GroupSetFormation(aGroupID: Integer; aNumColumns: Byte)');

    RegisterMethodCheck(c, 'procedure HouseAddBuildingMaterials(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddBuildingProgress(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddDamage(aHouseID: Integer; aDamage: Word)');
    RegisterMethodCheck(c, 'procedure HouseAddRepair(aHouseID: Integer; aRepair: Word)');
    RegisterMethodCheck(c, 'procedure HouseAddWaresTo(aHouseID: Integer; aType, aCount: Word)');
    RegisterMethodCheck(c, 'procedure HouseAllow(aPlayer, aHouseType: Word; aAllowed: Boolean)');
    RegisterMethodCheck(c, 'function  HouseBarracksEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'procedure HouseBarracksGiveRecruit(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseDeliveryBlock(aHouseID: Integer; aDeliveryBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseDestroy(aHouseID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseDisableUnoccupiedMessage(aHouseID: Integer; aDisabled: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseRepairEnable(aHouseID: Integer; aRepairEnabled: Boolean)');
    RegisterMethodCheck(c, 'function  HouseSchoolQueueAdd(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'procedure HouseSchoolQueueRemove(aHouseID, QueueIndex: Integer)');
    RegisterMethodCheck(c, 'procedure HouseTakeWaresFrom(aHouseID: Integer; aType, aCount: Word)');
    RegisterMethodCheck(c, 'procedure HouseUnlock(aPlayer, aHouseType: Word)');
    RegisterMethodCheck(c, 'procedure HouseWoodcutterChopOnly(aHouseID: Integer; aChopOnly: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWareBlock(aHouseID, aWareType: Integer; aBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWeaponsOrderSet(aHouseID, aWareType, aAmount: Integer)');

    RegisterMethodCheck(c, 'procedure Log(aText: AnsiString)');

    RegisterMethodCheck(c, 'procedure MarketSetTrade(aMarketID, aFrom, aTo, aAmount: Integer)');

    RegisterMethodCheck(c, 'function MapTileHeightSet(X, Y, Height: Integer): Boolean');
    RegisterMethodCheck(c, 'function MapTileObjectSet(X, Y, Obj: Integer): Boolean');
    RegisterMethodCheck(c, 'function MapTileSet(X, Y, aType, aRotation: Integer): Boolean');
    RegisterMethodCheck(c, 'function MapTilesArraySet(aTiles: array of TKMTerrainTileBrief; aRevertOnFail, aShowDetailedErrors: Boolean): Boolean');

    RegisterMethodCheck(c, 'procedure OverlayTextAppend(aPlayer: Shortint; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure OverlayTextAppendFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');
    RegisterMethodCheck(c, 'procedure OverlayTextSet(aPlayer: Shortint; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');

    RegisterMethodCheck(c, 'function  PlanAddField(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddHouse(aPlayer, aHouseType, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddRoad(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddWinefield(aPlayer, X, Y: Word): Boolean');
    RegisterMethodCheck(c, 'function  PlanConnectRoad(aPlayer, X1, Y1, X2, Y2: Integer; aCompleted: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  PlanRemove(aPlayer, X, Y: Word): Boolean');

    RegisterMethodCheck(c, 'procedure PlayerAddDefaultGoals(aPlayer: Byte; aBuildings: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAllianceChange(aPlayer1, aPlayer2: Byte; aCompliment, aAllied: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerDefeat(aPlayer: Word)');
    RegisterMethodCheck(c, 'procedure PlayerShareBeacons(aPlayer1, aPlayer2: Word; aCompliment, aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerShareFog(aPlayer1, aPlayer2: Word; aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerShareFogCompliment(aPlayer1, aPlayer2: Word; aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerWareDistribution(aPlayer, aWareType, aHouseType, aAmount: Byte)');
    RegisterMethodCheck(c, 'procedure PlayerWin(const aVictors: array of Integer; aTeamVictory: Boolean)');

    RegisterMethodCheck(c, 'function PlayWAV(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function PlayWAVAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function PlayWAVAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function PlayWAVFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function PlayWAVLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'procedure StopLoopedWAV(aLoopIndex: Integer)');

    RegisterMethodCheck(c, 'function PlayOGG(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function PlayOGGAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function PlayOGGAtLocationLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single; aRadius: Single; aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'function PlayOGGFadeMusic(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function PlayOGGLooped(aPlayer: ShortInt; const aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'procedure StopLoopedOGG(aLoopIndex: Integer)');
    RegisterMethodCheck(c, 'function PlaySound(aPlayer: ShortInt; const aFileName: AnsiString; aAudioFormat: TKMAudioFormat; ' +
                            'aVolume: Single; aFadeMusic, aLooped: Boolean): Integer');
    RegisterMethodCheck(c, 'function PlaySoundAtLocation(aPlayer: ShortInt; const aFileName: AnsiString; aAudioFormat: TKMAudioFormat; ' +
                            'aVolume: Single; aFadeMusic, aLooped: Boolean; aRadius: Single; aX, aY: Word): Integer');
    RegisterMethodCheck(c, 'procedure StopSound(aSoundIndex: Integer)');

    RegisterMethodCheck(c, 'procedure RemoveRoad(X, Y: Word)');

    RegisterMethodCheck(c, 'procedure SetTradeAllowed(aPlayer, aResType: Word; aAllowed: Boolean)');

    RegisterMethodCheck(c, 'procedure ShowMsg(aPlayer: ShortInt; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure ShowMsgFormatted(aPlayer: Shortint; aText: AnsiString; Params: array of const)');
    RegisterMethodCheck(c, 'procedure ShowMsgGoto(aPlayer: Shortint; aX, aY: Word; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure ShowMsgGotoFormatted(aPlayer: Shortint; aX, aY: Word; aText: AnsiString; Params: array of const)');

    RegisterMethodCheck(c, 'procedure UnitBlock(aPlayer: Byte; aType: Word; aBlock: Boolean)');
    RegisterMethodCheck(c, 'function  UnitDirectionSet(aUnitID, aDirection: Integer): Boolean');
    RegisterMethodCheck(c, 'procedure UnitHPChange(aUnitID, aHP: Integer)');
    RegisterMethodCheck(c, 'procedure UnitHPSetInvulnerable(aUnitID: Integer; aInvulnerable: Boolean)');
    RegisterMethodCheck(c, 'procedure UnitHungerSet(aUnitID, aHungerLevel: Integer)');
    RegisterMethodCheck(c, 'procedure UnitKill(aUnitID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'function  UnitOrderWalk(aUnitID: Integer; X, Y: Word): Boolean');

    c := Sender.AddClassN(nil, AnsiString(fUtils.ClassName));
    RegisterMethodCheck(c, 'function AbsI(aValue: Integer): Integer');
    RegisterMethodCheck(c, 'function AbsS(aValue: Single): Single');

    RegisterMethodCheck(c, 'function ArrayElementCount(aElement: AnsiString; aArray: array of AnsiString): Integer');
    RegisterMethodCheck(c, 'function ArrayElementCountB(aElement: Boolean; aArray: array of Boolean): Integer');
    RegisterMethodCheck(c, 'function ArrayElementCountI(aElement: Integer; aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function ArrayElementCountS(aElement: Single; aArray: array of Single): Integer');

    RegisterMethodCheck(c, 'function ArrayHasElement(aElement: AnsiString; aArray: array of AnsiString): Boolean');
    RegisterMethodCheck(c, 'function ArrayHasElementB(aElement: Boolean; aArray: array of Boolean): Boolean');
    RegisterMethodCheck(c, 'function ArrayHasElementI(aElement: Integer; aArray: array of Integer): Boolean');
    RegisterMethodCheck(c, 'function ArrayHasElementS(aElement: Single; aArray: array of Single): Boolean');

    RegisterMethodCheck(c, 'function EnsureRangeS(aValue, aMin, aMax: Single): Single');
    RegisterMethodCheck(c, 'function EnsureRangeI(aValue, aMin, aMax: Integer): Integer');

    RegisterMethodCheck(c, 'function Format(aFormatting: string; aData: array of const): string;');

    RegisterMethodCheck(c, 'function IfThen(aBool: Boolean; aTrue, aFalse: AnsiString): AnsiString');
    RegisterMethodCheck(c, 'function IfThenI(aBool: Boolean; aTrue, aFalse: Integer): Integer');
    RegisterMethodCheck(c, 'function IfThenS(aBool: Boolean; aTrue, aFalse: Single): Single');

    RegisterMethodCheck(c, 'function InAreaI(aX, aY, aXMin, aYMin, aXMax, aYMax: Integer): Boolean');
    RegisterMethodCheck(c, 'function InAreaS(aX, aY, aXMin, aYMin, aXMax, aYMax: Single): Boolean');

    RegisterMethodCheck(c, 'function InRangeI(aValue, aMin, aMax: Integer): Boolean');
    RegisterMethodCheck(c, 'function InRangeS(aValue, aMin, aMax: Single): Boolean');

    RegisterMethodCheck(c, 'function MaxI(A, B: Integer): Integer');
    RegisterMethodCheck(c, 'function MaxS(A, B: Single): Single');

    RegisterMethodCheck(c, 'function MaxInArrayI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function MaxInArrayS(aArray: array of Single): Single');

    RegisterMethodCheck(c, 'function MinI(A, B: Integer): Integer');
    RegisterMethodCheck(c, 'function MinS(A, B: Single): Single');

    RegisterMethodCheck(c, 'function MinInArrayI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function MinInArrayS(aArray: array of Single): Single');

    RegisterMethodCheck(c, 'function Power(Base, Exponent: Extended): Extended');

    RegisterMethodCheck(c, 'function Sqr(A: Extended): Extended');

    RegisterMethodCheck(c, 'function SumI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function SumS(aArray: array of Single): Single');

    RegisterMethodCheck(c, 'function TimeToString(aTicks: Integer): AnsiString');

    // Register objects
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
      RegisterMethod(@TKMScriptStates.HouseDeliveryMode,                        'HouseDeliveryMode');
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
      RegisterMethod(@TKMScriptActions.MapTilesArraySet,                        'MapTilesArraySet');
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
      RegisterMethod(@TKMScriptActions.PlayerShareFogCompliment,                'PlayerShareFogCompliment');
      RegisterMethod(@TKMScriptActions.PlayerWareDistribution,                  'PlayerWareDistribution');
      RegisterMethod(@TKMScriptActions.PlayerWin,                               'PlayerWin');

      RegisterMethod(@TKMScriptActions.PlayWAV,                                 'PlayWAV');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocation,                       'PlayWAVAtLocation');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocationLooped,                 'PlayWAVAtLocationLooped');
      RegisterMethod(@TKMScriptActions.PlayWAVFadeMusic,                        'PlayWAVFadeMusic');
      RegisterMethod(@TKMScriptActions.PlayWAVLooped,                           'PlayWAVLooped');
      RegisterMethod(@TKMScriptActions.StopLoopedWAV,                           'StopLoopedWAV');

      RegisterMethod(@TKMScriptActions.PlayOGG,                                 'PlayOGG');
      RegisterMethod(@TKMScriptActions.PlayOGGAtLocation,                       'PlayOGGAtLocation');
      RegisterMethod(@TKMScriptActions.PlayOGGAtLocationLooped,                 'PlayOGGAtLocationLooped');
      RegisterMethod(@TKMScriptActions.PlayOGGFadeMusic,                        'PlayOGGFadeMusic');
      RegisterMethod(@TKMScriptActions.PlayOGGLooped,                           'PlayOGGLooped');
      RegisterMethod(@TKMScriptActions.StopLoopedOGG,                           'StopLoopedOGG');

      RegisterMethod(@TKMScriptActions.PlaySound,                               'PlaySound');
      RegisterMethod(@TKMScriptActions.PlaySoundAtLocation,                     'PlaySoundAtLocation');
      RegisterMethod(@TKMScriptActions.StopSound,                               'StopSound');

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
  // That mean we do not need to find line of code by its text (which could be indefinite due to multiple code lines with the same text)
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


procedure TKMScriptErrorHandler.AppendErrorStr(const aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '');
begin
  fErrorString.GameMessage := fErrorString.GameMessage + aErrorString;
  fErrorString.LogMessage := fErrorString.LogMessage + aDetailedErrorString;
end;


procedure TKMScriptErrorHandler.AppendWarningStr(const aWarningString: UnicodeString; aDetailedWarningString: UnicodeString = '');
begin
  fWarningsString.GameMessage := fWarningsString.GameMessage + aWarningString;
  fWarningsString.LogMessage := fWarningsString.LogMessage + aDetailedWarningString;
end;


function TKMScriptErrorHandler.AppendErrorPrefix(const aPrefix: UnicodeString; var aError: TKMScriptErrorMessage): TKMScriptErrorMessage;
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


function TKMScriptErrorHandler.HasWarnings: Boolean;
begin
  Result := fWarningsString.GameMessage <> '';
end;



procedure TKMScriptErrorHandler.HandleErrors;
begin
  HandleScriptError(se_CompileError, AppendErrorPrefix('Script compile errors:' + EolW, fErrorString));
  HandleScriptError(se_CompileWarning, AppendErrorPrefix('Script compile warnings:' + EolW, fWarningsString));
end;


procedure TKMScriptErrorHandler.SetScriptLogFile(const aScriptLogFile: UnicodeString);
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


procedure TKMScriptingPreProcessor.BeforePreProcess(const aMainFileName: UnicodeString; const aMainFileText: AnsiString);
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


function TKMScriptingPreProcessor.PreProcessFile(const aFileName: UnicodeString): Boolean;
var ScriptCode: AnsiString;
begin
  Result := PreProcessFile(aFileName, ScriptCode);
end;


function TKMScriptingPreProcessor.PreProcessFile(const aFileName: UnicodeString; var aScriptCode: AnsiString): Boolean;
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
  // save in fHasDefDirectives, when script do have IFDEF or IFNDEF directive, which might change script code after pre-processing
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
//Returns number of occurences
function TKMScriptFilesCollection.FindCodeLine(const aLine: AnsiString; out aFileNamesArr: TStringArray; out aRowsArr: TIntegerArray): Integer;

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
