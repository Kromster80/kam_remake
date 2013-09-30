unit KM_Units;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KromUtils, TypInfo, Types,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Utils,
  KM_Terrain, KM_ResHouses, KM_ResWares, KM_Houses;

//Memo on directives:
//Dynamic - declared and used (overriden) occasionally
//Virtual - declared and used (overriden) always
//Abstract - declared but must be overriden in child classes

type
  TKMUnit = class;
  TKMUnitWorker = class;
  TKMUnitEvent = procedure(aUnit: TKMUnit) of object;
  TKMUnitFromEvent = procedure(aUnit: TKMUnit; aFrom: THandIndex) of object;

  TActionResult = (ActContinues, ActDone, ActAborted); //

  TUnitAction = class
  protected
    fActionType: TUnitActionType;
    fUnit: TKMUnit;
  public
    Locked: Boolean; //Means that unit can't take part in interaction, must stay on its tile
    StepDone: Boolean; //True when single action element is done (unit walked to new tile, single attack loop done)
    constructor Create(aUnit: TKMUnit; aActionType: TUnitActionType; aLocked: Boolean);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    function ActName: TUnitActionName; virtual; abstract;
    property ActionType: TUnitActionType read fActionType;
    function GetExplanation: UnicodeString; virtual; abstract;
    function Execute: TActionResult; virtual; abstract;
    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Paint; virtual;
  end;

  TTaskResult = (TaskContinues, TaskDone); //There's no difference between Done and Aborted

  TUnitTask = class
  protected
    fTaskName: TUnitTaskName;
    fUnit: TKMUnit; //Unit who's performing the Task
    fPhase: Byte;
    fPhase2: Byte;
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;

    property Phase: Byte read fPhase write fPhase;
    property TaskName: TUnitTaskName read fTaskName;
    function WalkShouldAbandon: Boolean; dynamic;

    function Execute: TTaskResult; virtual; abstract;
    procedure Save(SaveStream: TKMemoryStream); virtual;
  end;


  TKMUnit = class
  protected //Accessible for child classes
    fUID: Integer; //unique unit ID, used for save/load to sync to
    fUnitType: TUnitType;
    fUnitTask: TUnitTask;
    fCurrentAction: TUnitAction;
    fThought: TKMUnitThought;
    fHitPoints: Byte;
    fHitPointCounter: Cardinal; //Counter for hit point restoration, separate cos it resets on first hit
    fCondition: Integer; //Unit condition, when it reaches zero unit should die (rarely can be negative due to WalkExchange)
    fTicker: Cardinal; //ticks of life for the unit (allows to spread updates)
    fOwner: THandIndex;
    fHome: TKMHouse;
    fPosition: TKMPointF;
    fVisible: Boolean;
    fIsDead: Boolean;
    fKillASAP: Boolean;
    fKillASAPShowAnimation: Boolean;
    fPointerCount: Word;
    fInHouse: TKMHouse; //House we are currently in
    fCurrPosition: TKMPoint; //Where we are now
    fPrevPosition: TKMPoint; //Where we were
    fNextPosition: TKMPoint; //Where we will be. Next tile in route or same tile if stay on place
    fDirection: TKMDirection; //

    function GetDesiredPassability: TPassability;
    function GetHitPointsMax: Byte;
    procedure SetDirection(aValue: TKMDirection);
    procedure SetAction(aAction: TUnitAction; aStep: Integer = 0);
    procedure SetNextPosition(aLoc: TKMPoint);
    function CanAccessHome: Boolean;

    procedure UpdateThoughts;
    function UpdateVisibility: Boolean;
    procedure UpdateHitPoints;
    procedure DoKill(aShowAnimation: Boolean);
  public
    AnimStep: Integer;
    IsExchanging: Boolean; //Current walk is an exchange, used for sliding
    OnUnitDied: TKMUnitFromEvent;
    OnUnitTrained: TKMUnitEvent;

    constructor Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex);
    constructor Load(LoadStream: TKMemoryStream); dynamic;
    procedure SyncLoad; virtual;
    destructor Destroy; override;

    function GetUnitPointer: TKMUnit; //Returns self and adds one to the pointer counter
    procedure ReleaseUnitPointer;  //Decreases the pointer counter
    property GetPointerCount: Word read fPointerCount;

    procedure KillUnit(aFrom: THandIndex; aShowAnimation, aForceDelay: Boolean); virtual; //Creates TTaskDie which then will Close the unit from further access
    procedure CloseUnit(aRemoveTileUsage: Boolean = True); dynamic;

    property UID: Integer read fUID;
    property PrevPosition: TKMPoint read fPrevPosition;
    property NextPosition: TKMPoint read fNextPosition write SetNextPosition;
    property Direction: TKMDirection read fDirection write SetDirection;

    function HitTest(X,Y: Integer; const UT: TUnitType = ut_Any): Boolean;

    procedure SetActionAbandonWalk(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk);
    procedure SetActionFight(aAction: TUnitActionType; aOpponent: TKMUnit);
    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse); virtual;
    procedure SetActionStay(aTimeToStay: Integer; aAction: TUnitActionType; aStayStill: Boolean=true; aStillFrame:byte=0; aStep: Integer=0);
    procedure SetActionStorm(aRow: Integer);
    procedure SetActionSteer;
    procedure SetActionLockedStay(aTimeToStay: Integer; aAction: TUnitActionType; aStayStill: Boolean=true; aStillFrame:byte=0; aStep: Integer=0);

    procedure SetActionWalk(aLocB: TKMPoint; aActionType:TUnitActionType; aDistance:single; aTargetUnit: TKMUnit; aTargetHouse: TKMHouse);
    procedure SetActionWalkToHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TUnitActionType = ua_Walk);
    procedure SetActionWalkFromHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TUnitActionType = ua_Walk);
    procedure SetActionWalkToUnit(aUnit: TKMUnit; aDistance:single; aActionType:TUnitActionType=ua_Walk);
    procedure SetActionWalkFromUnit(aUnit: TKMUnit; aDistance: Single; aActionType:TUnitActionType=ua_Walk);
    procedure SetActionWalkToSpot(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk; aUseExactTarget: Boolean=true);
    procedure SetActionWalkPushed(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk);

    procedure Feed(Amount: Single);
    procedure AbandonWalk;
    property DesiredPassability: TPassability read GetDesiredPassability;
    property Owner: THandIndex read fOwner;
    property GetHome: TKMHouse read fHome;
    property GetUnitAction: TUnitAction read fCurrentAction;
    property UnitTask: TUnitTask read fUnitTask;
    property UnitType: TUnitType read fUnitType;
    function GetUnitActText: UnicodeString;
    property Condition: Integer read fCondition write fCondition;
    procedure SetOwner(aOwner: THandIndex);
    procedure HitPointsDecrease(aAmount: Byte; aFrom: THandIndex);
    property HitPointsMax: Byte read GetHitPointsMax;
    procedure CancelUnitTask;
    property Visible: Boolean read fVisible write fVisible;
    procedure SetInHouse(aInHouse: TKMHouse);
    property GetInHouse: TKMHouse read fInHouse;
    property IsDead: Boolean read fIsDead;
    function IsDeadOrDying: Boolean;
    function CanGoEat: Boolean;
    property GetPosition: TKMPoint read fCurrPosition;
    procedure SetPosition(aPos: TKMPoint);
    property PositionF: TKMPointF read fPosition write fPosition;
    property Thought: TKMUnitThought read fThought write fThought;
    function GetMovementVector: TKMPointF;
    function IsIdle: Boolean;
    procedure TrainInHouse(aSchool: TKMHouseSchool);

    function CanStepTo(X,Y: Integer; aPass: TPassability): Boolean;
    function CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean; overload;
    function CanWalkTo(aTo: TKMPoint; aPass: TPassability; aDistance: Single): Boolean; overload;
    function CanWalkTo(aFrom, aTo: TKMPoint; aDistance: Single): Boolean; overload;
    function CanWalkTo(aFrom, aTo: TKMPoint; aPass: TPassability; aDistance: Single): Boolean; overload;
    function CanWalkTo(aFrom: TKMPoint; aHouse: TKMHouse; aPass: TPassability; aDistance: Single): Boolean; overload;
    function CanWalkDiagonaly(aFrom, aTo: TKMPoint): Boolean;
    procedure VertexRem(aLoc: TKMPoint);
    function VertexUsageCompatible(aFrom, aTo: TKMPoint): Boolean;
    procedure VertexAdd(aFrom, aTo: TKMPoint);
    procedure Walk(aFrom, aTo: TKMPoint);
    function GetActivityText: UnicodeString; virtual;
    function GetSlide(aCheck: TCheckAxis): Single;
    function PathfindingShouldAvoid: Boolean; virtual;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    function UpdateState: Boolean; virtual;
    procedure Paint; virtual;
  end;

  //This is a common class for units going out of their homes for resources
  TKMUnitCitizen = class(TKMUnit)
  private
    function FindHome: Boolean;
    function InitiateMining: TUnitTask;
    procedure IssueResourceDepletedMessage;
  public
    function WorkPlanProductValid(aProduct: TWareType): Boolean;
    function CanWorkAt(aLoc: TKMPoint; aGatheringScript: TGatheringScript): Boolean;
    function GetActivityText: UnicodeString; override;
    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;


  TKMUnitRecruit = class(TKMUnit)
  private
    function FindHome: Boolean;
    function InitiateActivity: TUnitTask;
  public
    function UpdateState: Boolean; override;
    procedure Paint; override;
    procedure DestroyInBarracks;
  end;

  //Serf - transports all wares between houses
  TKMUnitSerf = class(TKMUnit)
  private
    fCarry: TWareType;
  public
    constructor Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    procedure Deliver(aFrom: TKMHouse; toHouse: TKMHouse; Res: TWareType; aID: integer); overload;
    procedure Deliver(aFrom: TKMHouse; toUnit: TKMUnit; Res: TWareType; aID: integer); overload;
    function TryDeliverFrom(aFrom: TKMHouse): Boolean;

    property Carry: TWareType read fCarry;
    procedure CarryGive(Res: TWareType);
    procedure CarryTake;
    procedure SetNewDelivery(aDelivery:TUnitTask);

    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;

  //Worker class - builds everything in game
  TKMUnitWorker = class(TKMUnit)
  public
    procedure BuildHouse(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildHouseRepair(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildField(aField: TFieldType; aLoc: TKMPoint; aIndex: Integer);
    procedure BuildHouseArea(aHouseType: THouseType; aLoc: TKMPoint; aIndex: Integer);
    function PickRandomSpot(aList: TKMPointDirList; out Loc: TKMPointDir): Boolean;

    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;


  //Animals
  TKMUnitAnimal = class(TKMUnit)
  private
    fFishCount: Byte; //1-5
  public
    constructor Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex); overload;
    constructor Load(LoadStream: TKMemoryStream); override;
    property FishCount: byte read fFishCount;
    function ReduceFish: Boolean;
    procedure Save(SaveStream: TKMemoryStream); override;
    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;


implementation
uses
  KM_CommonTypes, KM_Game, KM_RenderPool, KM_RenderAux, KM_ResTexts, KM_Scripting,
  KM_HandsCollection, KM_FogOfWar, KM_Units_Warrior, KM_Resource, KM_ResUnits, KM_HouseInn,

  KM_UnitActionAbandonWalk,
  KM_UnitActionFight,
  KM_UnitActionGoInOut,
  KM_UnitActionStay,
  KM_UnitActionSteer,
  KM_UnitActionStormAttack,
  KM_UnitActionWalkTo,

  KM_UnitTaskAttackHouse,
  KM_UnitTaskBuild,
  KM_UnitTaskDelivery,
  KM_UnitTaskDie,
  KM_UnitTaskGoEat,
  KM_UnitTaskGoHome,
  KM_UnitTaskGoOutShowHungry,
  KM_UnitTaskMining,
  KM_UnitTaskSelfTrain,
  KM_UnitTaskThrowRock;


{ TKMUnitCitizen }
//Find home for unit
function TKMUnitCitizen.FindHome: Boolean;
var H: TKMHouse;
begin
  Result:=false;
  H := gHands[fOwner].Houses.FindEmptyHouse(fUnitType, fCurrPosition);
  if H <> nil then
  begin
    fHome  := H.GetHousePointer;
    Result := true;
  end;
end;


//Used to improve efficiency of finding work plan
function TKMUnitCitizen.WorkPlanProductValid(aProduct: TWareType): Boolean;
begin
  Result := (fHome.CheckResOut(aProduct) < MAX_WARES_IN_HOUSE);
end;


procedure TKMUnitCitizen.Paint;
var
  Act: TUnitActionType;
  XPaintPos, YPaintPos: Single;
  ID: Integer;
begin
  inherited;
  if not fVisible then exit;
  if fCurrentAction = nil then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  YPaintPos := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  ID := fUID * Byte(not (fCurrentAction.fActionType in [ua_Die, ua_Eat]));

  case fCurrentAction.fActionType of
    ua_Walk:
      begin
        fRenderPool.AddUnit(fUnitType, ID, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
        if fResource.UnitDat[fUnitType].SupportsAction(ua_WalkArm) then
          fRenderPool.AddUnit(fUnitType, ID, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, false);
      end;
    ua_Work..ua_Eat:
        fRenderPool.AddUnit(fUnitType, ID, Act, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
    ua_WalkArm .. ua_WalkBooty2:
      begin
        fRenderPool.AddUnit(fUnitType, ID, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
        fRenderPool.AddUnit(fUnitType, ID, Act, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, false);
      end;
  end;

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fUnitType, Act, Direction, fThought, XPaintPos, YPaintPos);
end;


function TKMUnitCitizen.CanWorkAt(aLoc: TKMPoint; aGatheringScript: TGatheringScript): Boolean;
var
  I: Integer;
begin
  case aGatheringScript of
    gs_WoodCutterPlant: begin
                          //Woodcutters should not plant trees on our own or our ally's house plans
                          //(it's very annoying if they do)
                          Result := True;
                          for I := 0 to gHands.Count - 1 do
                            if (I = fOwner) or (gHands[fOwner].Alliances[I] = at_Ally) then
                              Result := Result and not gHands[I].BuildList.HousePlanList.HasPlan(aLoc);
                        end;
    else Result := True;
  end;
end;


function TKMUnitCitizen.GetActivityText: UnicodeString;
begin
  Result := Inherited GetActivityText; //Default
  if fUnitTask is TTaskMining then
    Result := TTaskMining(fUnitTask).GetActivityText;
end;


function TKMUnitCitizen.UpdateState: Boolean;
var
  houseInn: TKMHouseInn;
begin
  Result := True; //Required for override compatibility
  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName + ' has no action at start of TKMUnitCitizen.UpdateState', fCurrPosition);

  //Reset unit activity if home was destroyed, except when unit is dying or eating (finish eating/dying first)
  if (fHome <> nil)
  and fHome.IsDestroyed
  and not(fUnitTask is TTaskDie)
  and not(fUnitTask is TTaskGoEat) then
  begin
    if (fCurrentAction is TUnitActionWalkTo)
    and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
      AbandonWalk;
    if (UnitTask is TTaskMining) and (GetUnitAction is TUnitActionStay) and (fInHouse = fHome) then
      SetActionStay(0, ua_Walk); //If we were working inside the house stop
    FreeAndNil(fUnitTask);
    gHands.CleanUpHousePointer(fHome);
  end;

  if inherited UpdateState then exit;
  if IsDead then exit; //Caused by SelfTrain.Abandoned

  fThought := th_None;

{  if fUnitTask=nil then //Which is always nil if 'inherited UpdateState' works properly
  if not TestHunger then
  if not TestHasHome then
  if not TestAtHome then
  if not TestMining then
    Idle..}


  //See if need to get to eat
  if fCondition < UNIT_MIN_CONDITION then
  begin
    houseInn := gHands[fOwner].FindInn(fCurrPosition,Self,not fVisible);
    if houseInn <> nil then
      fUnitTask := TTaskGoEat.Create(houseInn, Self)
    else
      if (fHome <> nil) and not fVisible then
        //If we inside home - go out and return (I suspect that was same task as TTaskGoEat in KaM though)
        fUnitTask := TTaskGoOutShowHungry.Create(Self)
  end;

  if fUnitTask = nil then //If Unit still got nothing to do, nevermind hunger
    if fHome = nil then
      if FindHome then
        fUnitTask := TTaskGoHome.Create(Self) //Home found - go there
      else begin
        fThought := th_Quest; //Always show quest when idle, unlike serfs who randomly show it
        SetActionStay(20, ua_Walk) //There's no home, but there will hopefully be one soon so don't sleep too long
      end
    else
      if fVisible then //Unit is not at home, but it has one
      begin
        if CanAccessHome then
          fUnitTask := TTaskGoHome.Create(Self)
        else
          SetActionStay(60, ua_Walk) //Home can't be reached
      end else begin
        fUnitTask := InitiateMining; //Unit is at home, so go get a job
        if fUnitTask = nil then //We didn't find any job to do - rest at home
          SetActionStay(fResource.HouseDat[fHome.HouseType].WorkerRest*10, ua_Walk);
      end;

  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at end of TKMUnitCitizen.UpdateState', fCurrPosition);
end;


procedure TKMUnitCitizen.IssueResourceDepletedMessage;
var
  Msg: Word;
begin
  case fHome.HouseType of
    ht_Quary:       Msg := TX_MSG_STONE_DEPLETED;
    ht_CoalMine:    Msg := TX_MSG_COAL_DEPLETED;
    ht_IronMine:    Msg := TX_MSG_IRON_DEPLETED;
    ht_GoldMine:    Msg := TX_MSG_GOLD_DEPLETED;
    ht_Woodcutters: Msg := TX_MSG_WOODCUTTER_DEPLETED;
    ht_FisherHut:   if not gTerrain.CanFindFishingWater(KMPointBelow(fHome.GetEntrance), fResource.UnitDat[fUnitType].MiningRange) then
                      Msg := TX_MSG_FISHERMAN_TOO_FAR
                    else
                      Msg := TX_MSG_FISHERMAN_CANNOT_CATCH;
    else            Msg := 0;
  end;

  Assert(Msg <> 0, fResource.HouseDat[fHome.HouseType].HouseName+' resource cant possibly deplet');

  if (fOwner = MySpectator.HandIndex) and not gGame.IsReplay then //Don't show message for other players or during replays
    gGame.ShowMessage(mkHouse, gResTexts[Msg], fHome.GetEntrance);

  fHome.ResourceDepletedMsgIssued := True;
end;


function TKMUnitCitizen.InitiateMining:TUnitTask;
var Res: Integer; TM: TTaskMining;
begin
  Result := nil;

  if not KMSamePoint(fCurrPosition, fHome.GetEntrance) then
    raise ELocError.Create('Mining from wrong spot',fCurrPosition);

  Res := 1;
  //Check if House has production orders
  //Ask the house what order we should make
  if fResource.HouseDat[fHome.HouseType].DoesOrders then
  begin
    Res := fHome.PickOrder;
    if Res = 0 then Exit;
  end;

  TM := TTaskMining.Create(Self, fResource.HouseDat[fHome.HouseType].ResOutput[Res]);

  if TM.WorkPlan.ResourceDepleted and not fHome.ResourceDepletedMsgIssued then
    IssueResourceDepletedMessage;

  if TM.WorkPlan.IsIssued
  and ((TM.WorkPlan.Resource1 = wt_None) or (fHome.CheckResIn(TM.WorkPlan.Resource1) >= TM.WorkPlan.Count1))
  and ((TM.WorkPlan.Resource2 = wt_None) or (fHome.CheckResIn(TM.WorkPlan.Resource2) >= TM.WorkPlan.Count2))
  and (fHome.CheckResOut(TM.WorkPlan.Product1) < MAX_WARES_IN_HOUSE)
  and (fHome.CheckResOut(TM.WorkPlan.Product2) < MAX_WARES_IN_HOUSE) then
  begin
    //if fResource.HouseDat[fHome.HouseType].DoesOrders then
      //Take order to production
      //fHome.ResOrder[Res] := fHome.ResOrder[Res] - 1;
    Result := TM;
  end
  else
  begin
    TM.Free;
    Result := nil;
  end;
end;


{ TKMUnitRecruit }
function TKMUnitRecruit.FindHome: Boolean;
var H: TKMHouse;
begin
  Result  := false;
  H := gHands[fOwner].Houses.FindEmptyHouse(fUnitType, fCurrPosition);
  if H <> nil then
  begin
    fHome  := H.GetHousePointer;
    Result := true;
  end;
end;


procedure TKMUnitRecruit.Paint;
var
  Act: TUnitActionType;
  XPaintPos, YPaintPos: Single;
  ID: Integer;
begin
  inherited;
  if not fVisible then exit;
  if fCurrentAction = nil then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  YPaintPos := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  ID := fUID * Byte(not (fCurrentAction.fActionType in [ua_Die, ua_Eat]));

  case fCurrentAction.fActionType of
    ua_Walk:
      begin
        fRenderPool.AddUnit(fUnitType, ID, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
        if fResource.UnitDat[fUnitType].SupportsAction(ua_WalkArm) then
          fRenderPool.AddUnit(fUnitType, ID, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, false);
      end;
    ua_Work..ua_Eat:
        fRenderPool.AddUnit(fUnitType, ID, Act, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
    ua_WalkArm .. ua_WalkBooty2:
      begin
        fRenderPool.AddUnit(fUnitType, ID, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);
        fRenderPool.AddUnit(fUnitType, ID, Act, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, false);
      end;
  end;

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fUnitType, Act, Direction, fThought, XPaintPos, YPaintPos);
end;


procedure TKMUnitRecruit.DestroyInBarracks;
begin
  //Dispose of current action/task BEFORE we close the unit (action might need to check fPosition if recruit was about to walk out to eat)
  //Normally this isn't required because TTaskDie takes care of it all, but recruits in barracks don't use TaskDie.
  SetAction(nil);
  FreeAndNil(fUnitTask);

  CloseUnit(False); //Don't remove tile usage, we are inside the barracks
end;


function TKMUnitRecruit.UpdateState: Boolean;
var H: TKMHouseInn;
begin
  Result := True; //Required for override compatibility
  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at start of TKMUnitRecruit.UpdateState',fCurrPosition);

  //Reset unit activity if home was destroyed, except when unit is dying or eating (finish eating/dying first)
  if (fHome <> nil)
  and fHome.IsDestroyed
  and not(fUnitTask is TTaskDie)
  and not(fUnitTask is TTaskGoEat) then
  begin
    if (fCurrentAction is TUnitActionWalkTo)
    and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
      AbandonWalk;
    FreeAndNil(fUnitTask);
    gHands.CleanUpHousePointer(fHome);
  end;

  if inherited UpdateState then exit;
  if IsDead then exit; //Caused by SelfTrain.Abandoned

  fThought := th_None;

  if fCondition < UNIT_MIN_CONDITION then
  begin
    H := gHands[fOwner].FindInn(fCurrPosition,Self,not fVisible);
    if H <> nil then
      fUnitTask := TTaskGoEat.Create(H,Self)
    else
      if (fHome <> nil) and not fVisible then
        fUnitTask := TTaskGoOutShowHungry.Create(Self);
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    if fHome=nil then
      if FindHome then
        fUnitTask := TTaskGoHome.Create(Self) //Home found - go there
      else begin
        fThought := th_Quest; //Always show quest when idle, unlike serfs who randomly show it
        SetActionStay(120, ua_Walk) //There's no home
      end
    else
      if fVisible then //Unit is not at home, but it has one
      begin
        if CanAccessHome then
          fUnitTask := TTaskGoHome.Create(Self)
        else
          SetActionStay(60, ua_Walk) //Home can't be reached
      end else begin
        fUnitTask := InitiateActivity; //Unit is at home, so go get a job
        if fUnitTask=nil then //We didn't find any job to do - rest at home
          SetActionStay(Max(fResource.HouseDat[fHome.HouseType].WorkerRest,1)*10, ua_Walk); //By default it's 0, don't scan that often
      end;

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at end of TKMUnitRecruit.UpdateState',fCurrPosition);
end;


function TKMUnitRecruit.InitiateActivity: TUnitTask;
var
  Enemy: TKMUnit;
begin
  Result := nil;

  //See if we are in a tower and have something to throw
  if not (fHome is TKMHouseTower) or (fHome.CheckResIn(wt_Stone) <= 0) then
    Exit;

  Enemy := gTerrain.UnitsHitTestWithinRad(fCurrPosition, RANGE_WATCHTOWER_MIN, RANGE_WATCHTOWER_MAX, fOwner, at_Enemy, dir_NA, not RANDOM_TARGETS);

  //Note: In actual game there might be two Towers nearby,
  //both throwing a stone into the same enemy. We should not
  //negate that fact, thats real-life situation.

  if Enemy <> nil then
    Result := TTaskThrowRock.Create(Self, Enemy);
end;


{ TKMSerf }
constructor TKMUnitSerf.Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex);
begin
  inherited;
  fCarry := wt_None;
end;


procedure TKMUnitSerf.Deliver(aFrom, toHouse: TKMHouse; Res: TWareType; aID: integer);
begin
  fThought := th_None; //Clear ? thought
  fUnitTask := TTaskDeliver.Create(Self, aFrom, toHouse, Res, aID);
end;


procedure TKMUnitSerf.Deliver(aFrom: TKMHouse; toUnit: TKMUnit; Res: TWareType; aID: integer);
begin
  fThought := th_None; //Clear ? thought
  fUnitTask := TTaskDeliver.Create(Self, aFrom, toUnit, Res, aID);
end;


function TKMUnitSerf.TryDeliverFrom(aFrom: TKMHouse): Boolean;
var T: TUnitTask;
begin
  //Remember current task
  T := fUnitTask;
  //Try to get a new one
  gHands[Owner].Deliveries.Queue.AskForDelivery(Self, aFrom);

  //Return True if we've got a new deliery
  Result := fUnitTask <> T;

  //If we got ourselves a new task then skip to resource-taking part, as we are already in this house
  if Result and (aFrom <> nil) then
    fUnitTask.Phase := 2; //Skip  of the new task
end;


constructor TKMUnitSerf.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fCarry, SizeOf(fCarry));
end;


procedure TKMUnitSerf.Paint;
var
  Act: TUnitActionType;
  XPaintPos, YPaintPos: Single;
  ID: Integer;
begin
  inherited;
  if not fVisible then exit;
  if fCurrentAction = nil then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  YPaintPos := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  ID := fUID * Byte(not (fCurrentAction.fActionType in [ua_Die, ua_Eat]));

  fRenderPool.AddUnit(UnitType, ID, Act, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);

  if fUnitTask is TTaskDie then Exit; //Do not show unnecessary arms

  if Carry <> wt_None then
    fRenderPool.AddUnitCarry(Carry, ID, Direction, AnimStep, XPaintPos, YPaintPos)
  else
    fRenderPool.AddUnit(UnitType, ID, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, false);

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fUnitType, Act, Direction, fThought, XPaintPos, YPaintPos);
end;


procedure TKMUnitSerf.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fCarry, SizeOf(fCarry));
end;


function TKMUnitSerf.UpdateState: Boolean;
var
  H: TKMHouseInn;
  OldThought: TKMUnitThought;
  WasIdle: Boolean;
begin
  Result := True; //Required for override compatibility
  WasIdle := IsIdle;
  if fCurrentAction = nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at start of TKMUnitSerf.UpdateState',fCurrPosition);
  if inherited UpdateState then exit;

  OldThought := fThought;
  fThought := th_None;

  if fCondition < UNIT_MIN_CONDITION then
  begin
    H := gHands[fOwner].FindInn(fCurrPosition, Self);
    if H <> nil then
      fUnitTask := TTaskGoEat.Create(H, Self);
  end;

  //Only show quest thought if we have been idle since the last update (not HadTask)
  //and not thinking anything else (e.g. death)
  if fUnitTask = nil then
  begin
    if WasIdle and (OldThought = th_None) and (KaMRandom(2) = 0) then
      fThought := th_Quest;
    SetActionStay(60,ua_Walk); //Stay idle
  end;

  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at end of TKMUnitSerf.UpdateState', fCurrPosition);
end;


procedure TKMUnitSerf.CarryGive(Res:TWareType);
begin
  Assert(fCarry=wt_None, 'Giving Serf another Carry');
  fCarry := Res;
end;


procedure TKMUnitSerf.CarryTake;
begin
  Assert(Carry <> wt_None, 'Taking wrong resource from Serf');
  fCarry := wt_None;
end;


procedure TKMUnitSerf.SetNewDelivery(aDelivery:TUnitTask);
begin
  fUnitTask := aDelivery;
end;


{ TKMWorker }
procedure TKMUnitWorker.BuildHouse(aHouse: TKMHouse; aIndex: Integer);
begin
  fUnitTask := TTaskBuildHouse.Create(Self, aHouse, aIndex);
end;


procedure TKMUnitWorker.BuildField(aField: TFieldType; aLoc: TKMPoint; aIndex: Integer);
begin
  case aField of
    ft_Road: fUnitTask := TTaskBuildRoad.Create(Self, aLoc, aIndex);
    ft_Corn: fUnitTask := TTaskBuildField.Create(Self, aLoc, aIndex);
    ft_Wine: fUnitTask := TTaskBuildWine.Create(Self, aLoc, aIndex);
    else     begin
              Assert(false, 'Unexpected Field Type');
              fUnitTask := nil;
              Exit;
             end;
  end;
end;


procedure TKMUnitWorker.BuildHouseArea(aHouseType: THouseType; aLoc: TKMPoint; aIndex: Integer);
begin
  fUnitTask := TTaskBuildHouseArea.Create(Self, aHouseType, aLoc, aIndex);
end;


procedure TKMUnitWorker.BuildHouseRepair(aHouse: TKMHouse; aIndex: Integer);
begin
  fUnitTask := TTaskBuildHouseRepair.Create(Self, aHouse, aIndex);
end;


//Given a list check the locations in it and pick those that can be walked to
//excluding current location. We assume that caller already made the list
//with only walkable valid tiles
function TKMUnitWorker.PickRandomSpot(aList: TKMPointDirList; out Loc: TKMPointDir): Boolean;
var
  I, MyCount: Integer;
  Spots: array of Word;
begin
  SetLength(Spots, aList.Count);

  //Scan the list and pick suitable locations
  MyCount := 0;
  for I := 0 to aList.Count - 1 do
  if not KMSamePoint(aList[I].Loc, GetPosition)
  and CanWalkTo(aList[I].Loc, 0) then
  begin
    Spots[MyCount] := I;
    Inc(MyCount);
  end;

  Result := (MyCount > 0);
  if Result then
    Loc := aList[Spots[KaMRandom(MyCount)]];
end;


procedure TKMUnitWorker.Paint;
var
  XPaintPos, YPaintPos: Single;
  ID: Integer;
begin
  inherited;
  if not fVisible then exit;
  if fCurrentAction = nil then exit;

  XPaintPos := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  YPaintPos := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  ID := fUID * Byte(not (fCurrentAction.fActionType in [ua_Die, ua_Eat]));

  fRenderPool.AddUnit(UnitType, ID, fCurrentAction.fActionType, Direction, AnimStep, XPaintPos, YPaintPos, gHands[fOwner].FlagColor, true);

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fUnitType, fCurrentAction.ActionType, Direction, fThought, XPaintPos, YPaintPos);
end;


function TKMUnitWorker.UpdateState: Boolean;
var
  H: TKMHouseInn;
begin
  Result := True; //Required for override compatibility
  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at start of TKMUnitWorker.UpdateState',fCurrPosition);
  if inherited UpdateState then exit;

  if fCondition < UNIT_MIN_CONDITION then
  begin
    H := gHands[fOwner].FindInn(fCurrPosition, Self);
    if H <> nil then
      fUnitTask := TTaskGoEat.Create(H, Self);
  end;

  if (fThought = th_Build)and(fUnitTask = nil) then
    fThought := th_None; //Remove build thought if we are no longer doing anything

  //If we are still stuck on a house for some reason, get off it ASAP
  Assert(gTerrain.Land[fCurrPosition.Y, fCurrPosition.X].TileLock <> tlHouse);

  if (fUnitTask = nil) and (fCurrentAction = nil) then SetActionStay(20, ua_Walk);

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at end of TKMUnitWorker.UpdateState',fCurrPosition);
end;


{ TKMUnitAnimal }
constructor TKMUnitAnimal.Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex);
begin
  inherited;

  //Always start with 5 fish in the group
  if aUnitType = ut_Fish then
    fFishCount := 5;
end;


constructor TKMUnitAnimal.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fFishCount);
end;


function TKMUnitAnimal.ReduceFish: Boolean;
begin
  Result := fUnitType = ut_Fish;
  if not Result then Exit;

  if fFishCount > 1 then
    Dec(fFishCount)
  else
    KillUnit(-1, True, False);
end;


procedure TKMUnitAnimal.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fFishCount);
end;


function TKMUnitAnimal.UpdateState: Boolean;
begin
  Result:=true; //Required for override compatibility

  fCurrPosition := KMPointRound(fPosition);

  if fCurrentAction=nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName + ' has no action at start of TKMUnitAnimal.UpdateState', fCurrPosition);

  case fCurrentAction.Execute of
    ActContinues: exit;
    ActDone:      FreeAndNil(fCurrentAction);
    ActAborted:   FreeAndNil(fCurrentAction);
  end;
  fCurrPosition := KMPointRound(fPosition);


  Assert((fUnitTask = nil) or (fUnitTask is TTaskDie));
  if fUnitTask is TTaskDie then
  case fUnitTask.Execute of
    TaskContinues:  exit;
    TaskDone:       Assert(false); //TTaskDie never returns TaskDone yet
  end;


  //First make sure the animal isn't stuck (check passibility of our position)
  if (not gTerrain.CheckPassability(fCurrPosition, DesiredPassability))
  or gTerrain.CheckAnimalIsStuck(fCurrPosition, DesiredPassability) then
  begin
    KillUnit(-1, True, False); //Animal is stuck so it dies
    Exit;
  end;

  SetActionSteer;

  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName + ' has no action at end of TKMUnitAnimal.UpdateState', fCurrPosition);
end;


//For fish the action is the number of fish in the group
procedure TKMUnitAnimal.Paint;
var
  Act: TUnitActionType;
  XPaintPos, YPaintPos: single;
begin
  inherited;
  if fCurrentAction = nil then exit;
  if fUnitType = ut_Fish then
    Act := FishCountAct[fFishCount]
  else
    Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  YPaintPos := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  //Make fish/watersnakes more visible in the MapEd
  if (gGame.GameMode = gmMapEd) and (fUnitType in [ut_Fish, ut_Watersnake, ut_Seastar]) then
    fRenderAux.Circle(fPosition.X - 0.5,
                      gTerrain.FlatToHeight(fPosition.X - 0.5, fPosition.Y - 0.5),
                      0.5, $30FF8000, $60FF8000);

  //Animals share the same WalkTo logic as other units and they exchange places if necessary
  //Animals can be picked only in MapEd
  fRenderPool.AddUnit(fUnitType, fUID * Byte(gGame.IsMapEditor), Act, Direction, AnimStep, XPaintPos, YPaintPos, $FFFFFFFF, True);
end;


{ TKMUnit }
constructor TKMUnit.Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: THandIndex);
begin
  inherited Create;
  fUID           := aID;
  fTicker       := fUID; //Units update states will be spread more evenly that way
  fPointerCount := 0;
  fIsDead       := false;
  fKillASAP     := false;
  fThought      := th_None;
  fHome         := nil;
  fInHouse      := nil;
  fPosition     := KMPointF(aLoc);
  fCurrPosition := aLoc;
  fPrevPosition := aLoc; //Init values
  fNextPosition := aLoc; //Init values
  fOwner        := aOwner;
  fUnitType     := aUnitType;
  fDirection    := dir_S;
  fVisible      := true;
  IsExchanging  := false;
  AnimStep      := UnitStillFrames[fDirection]; //Use still frame at begining, so units don't all change frame on first tick
  //Units start with a random amount of condition ranging from 0.5 to 0.7 (KaM uses 0.6 for all units)
  //By adding the random amount they won't all go eat at the same time and cause crowding, blockages, food shortages and other problems.
  if (gGame <> nil) and (gGame.GameMode <> gmMapEd) then
    fCondition    := Round(UNIT_MAX_CONDITION * (UNIT_CONDITION_BASE + KaMRandomS(UNIT_CONDITION_RANDOM)))
  else
    fCondition    := Round(UNIT_MAX_CONDITION * UNIT_CONDITION_BASE);
  fHitPoints      := HitPointsMax;
  fHitPointCounter := 1;

  SetActionLockedStay(10, ua_Walk); //Must be locked for this initial pause so animals don't get pushed
  gTerrain.UnitAdd(NextPosition,Self);

  //The area around the unit should be visible at the start of the mission
  if InRange(fOwner, 0, MAX_HANDS-1) then //Not animals
    gHands.RevealForTeam(fOwner, fCurrPosition, fResource.UnitDat[fUnitType].Sight, FOG_OF_WAR_MAX);
end;


destructor TKMUnit.Destroy;
begin
  if not IsDead then gTerrain.UnitRem(NextPosition); //Happens only when removing player from map on GameStart (network)
  FreeAndNil(fCurrentAction);
  FreeAndNil(fUnitTask);
  SetInHouse(nil); //Free pointer
  inherited;
end;


constructor TKMUnit.Load(LoadStream: TKMemoryStream);
var HasTask,HasAct: Boolean; TaskName:TUnitTaskName; ActName: TUnitActionName;
begin
  inherited Create;
  LoadStream.Read(fUnitType, SizeOf(fUnitType));
  LoadStream.Read(HasTask);
  if HasTask then
  begin
    LoadStream.Read(TaskName, SizeOf(TaskName));
    case TaskName of
      utn_Unknown:         Assert(false, 'TaskName can''t be handled');
      utn_SelfTrain:       fUnitTask := TTaskSelfTrain.Load(LoadStream);
      utn_Deliver:         fUnitTask := TTaskDeliver.Load(LoadStream);
      utn_BuildRoad:       fUnitTask := TTaskBuildRoad.Load(LoadStream);
      utn_BuildWine:       fUnitTask := TTaskBuildWine.Load(LoadStream);
      utn_BuildField:      fUnitTask := TTaskBuildField.Load(LoadStream);
      utn_BuildHouseArea:  fUnitTask := TTaskBuildHouseArea.Load(LoadStream);
      utn_BuildHouse:      fUnitTask := TTaskBuildHouse.Load(LoadStream);
      utn_BuildHouseRepair:fUnitTask := TTaskBuildHouseRepair.Load(LoadStream);
      utn_GoHome:          fUnitTask := TTaskGoHome.Load(LoadStream);
      utn_AttackHouse:     fUnitTask := TTaskAttackHouse.Load(LoadStream);
      utn_ThrowRock:       fUnitTask := TTaskThrowRock.Load(LoadStream);
      utn_GoEat:           fUnitTask := TTaskGoEat.Load(LoadStream);
      utn_Mining:          fUnitTask := TTaskMining.Load(LoadStream);
      utn_Die:             fUnitTask := TTaskDie.Load(LoadStream);
      utn_GoOutShowHungry: fUnitTask := TTaskGoOutShowHungry.Load(LoadStream);
      else                 Assert(false, 'TaskName can''t be handled');
    end;
  end
  else
    fUnitTask := nil;

  LoadStream.Read(HasAct);
  if HasAct then
  begin
    LoadStream.Read(ActName, SizeOf(ActName));
    case ActName of
      uan_Stay:        fCurrentAction := TUnitActionStay.Load(LoadStream);
      uan_WalkTo:      fCurrentAction := TUnitActionWalkTo.Load(LoadStream);
      uan_AbandonWalk: fCurrentAction := TUnitActionAbandonWalk.Load(LoadStream);
      uan_GoInOut:     fCurrentAction := TUnitActionGoInOut.Load(LoadStream);
      uan_Fight:       fCurrentAction := TUnitActionFight.Load(LoadStream);
      uan_StormAttack: fCurrentAction := TUnitActionStormAttack.Load(LoadStream);
      uan_Steer:       fCurrentAction := TUnitActionSteer.Load(LoadStream);
      else             Assert(false, 'ActName can''t be handled');
  end;
  end
  else
    fCurrentAction := nil;

  LoadStream.Read(fThought, SizeOf(fThought));
  LoadStream.Read(fCondition);
  LoadStream.Read(fTicker);
  LoadStream.Read(fHitPoints);
  LoadStream.Read(fHitPointCounter);
  LoadStream.Read(fInHouse, 4);
  LoadStream.Read(fOwner, SizeOf(fOwner));
  LoadStream.Read(fHome, 4); //Substitute it with reference on SyncLoad
  LoadStream.Read(fPosition);
  LoadStream.Read(fVisible);
  LoadStream.Read(fIsDead);
  LoadStream.Read(fKillASAP);
  LoadStream.Read(fKillASAPShowAnimation);
  LoadStream.Read(IsExchanging);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fUID);
  LoadStream.Read(AnimStep);
  LoadStream.Read(fDirection);
  LoadStream.Read(fCurrPosition);
  LoadStream.Read(fPrevPosition);
  LoadStream.Read(fNextPosition);
end;


procedure TKMUnit.SyncLoad;
begin
  if fUnitTask<>nil then fUnitTask.SyncLoad;
  if fCurrentAction<>nil then fCurrentAction.SyncLoad;
  fHome := gHands.GetHouseByUID(cardinal(fHome));
  fInHouse := gHands.GetHouseByUID(cardinal(fInHouse));
end;


procedure TKMUnit.TrainInHouse(aSchool: TKMHouseSchool);
begin
  fUnitTask := TTaskSelfTrain.Create(Self, aSchool);
end;


{Returns self and adds on to the pointer counter}
function TKMUnit.GetUnitPointer: TKMUnit;
begin
  inc(fPointerCount);
  Result := Self;
end;


{Decreases the pointer counter}
//Should be used only by gHands for clarity sake
procedure TKMUnit.ReleaseUnitPointer;
begin
  if fPointerCount < 1 then
    raise ELocError.Create('Unit remove pointer', PrevPosition);
  dec(fPointerCount);
end;


{Erase everything related to unit status to exclude it from being accessed by anything but the old pointers}
procedure TKMUnit.CloseUnit(aRemoveTileUsage: Boolean = True);
begin
  //if not KMSamePoint(fCurrPosition,NextPosition) then
  //  Assert(false, 'Not sure where to die?');

  if fHome <> nil then
  begin
    fHome.GetHasOwner := False;
    gHands.CleanUpHousePointer(fHome);
  end;

  if aRemoveTileUsage then gTerrain.UnitRem(fNextPosition); //Must happen before we nil NextPosition

  fIsDead       := true;
  fThought      := th_None;
  fPosition     := KMPointF(0,0);
  fCurrPosition := KMPoint(0,0);
  fPrevPosition := fCurrPosition;
  fNextPosition := fCurrPosition;
  fOwner        := -1;
  //Do not reset the unit type when they die as we still need to know during Load
  //fUnitType     := ut_None;
  fDirection    := dir_NA;
  fVisible      := false;
  fCondition    := 0;
  AnimStep      := 0;
  FreeAndNil(fCurrentAction);
  FreeAndNil(fUnitTask);

  Assert(MySpectator.Selected <> Self,
    'Removed units should be flushed from UI earlier inTaskDie or never appear there when training cancelled or alike');
end;


{Call this procedure to properly kill a unit. ForceDelay means we always use KillASAP}
//killing a unit is done in 3 steps
// Kill - release all unit-specific tasks
// TTaskDie - perform dying animation
// CloseUnit - erase all unit data and hide it from further access
procedure TKMUnit.KillUnit(aFrom: THandIndex; aShowAnimation, aForceDelay: Boolean);
begin
  //Don't kill unit if it's already dying
  if IsDeadOrDying then
    Exit;

  //From this moment onwards the unit is guaranteed to die (no way to avoid it even with KillASAP), so
  //signal to our owner that we have died (doesn't have to be assigned since f.e. animals don't use it)
  //This must be called before actually killing the unit because fScripting uses it
  //and script is not allowed to touch dead/dying/KillASAP units
  if Assigned(OnUnitDied) then
    OnUnitDied(Self, aFrom);

  //Wait till units exchange (1 tick) and then do the killing
  if aForceDelay
  or ((fCurrentAction is TUnitActionWalkTo) and TUnitActionWalkTo(fCurrentAction).DoingExchange) then
  begin
    fKillASAP := True; //Unit will be killed ASAP, when unit is ready for it
    fKillASAPShowAnimation := aShowAnimation;
    Exit;
  end;

  //If we didn't exit above, we are safe to do the kill now (no delay from KillASAP required)
  DoKill(aShowAnimation);
end;


procedure TKMUnit.DoKill(aShowAnimation: Boolean);
begin
  fThought := th_None; //Reset thought
  SetAction(nil); //Dispose of current action (TTaskDie will set it to LockedStay)
  FreeAndNil(fUnitTask); //Should be overriden to dispose of Task-specific items
  fUnitTask := TTaskDie.Create(Self, aShowAnimation);
end;


procedure TKMUnit.SetOwner(aOwner: THandIndex);
begin
  fOwner := aOwner;
end;


procedure TKMUnit.SetPosition(aPos: TKMPoint);
begin
  //This is only used by the map editor, set all positions to aPos
  Assert(gGame.GameMode = gmMapEd);
  if not gTerrain.CanPlaceUnit(aPos, UnitType) then Exit;

  gTerrain.UnitRem(fCurrPosition);
  fCurrPosition := aPos;
  fNextPosition := aPos;
  fPrevPosition := aPos;
  fPosition := KMPointF(aPos);
  gTerrain.UnitAdd(fCurrPosition, Self);
end;


function TKMUnit.CanAccessHome: Boolean;
begin
  Result := (fHome = nil) or CanWalkTo(KMPointBelow(fHome.GetEntrance), CanWalk, 0);
end;


function TKMUnit.GetUnitActText: UnicodeString;
begin
  Result := fCurrentAction.GetExplanation;
end;


//Return TRUE if unit was killed
procedure TKMUnit.HitPointsDecrease(aAmount: Byte; aFrom: THandIndex);
begin
  Assert(aAmount > 0, '0 damage should be handled outside so not to reset HPCounter');

  //When we are first hit reset the counter
  if fHitPoints = HitPointsMax then
    fHitPointCounter := 1;

  // Sign of aAmount does not affect (how come it ever did 0_o)
  fHitPoints := Max(fHitPoints - aAmount, 0);
  if (fHitPoints = 0) and not IsDeadOrDying then
    //Make sure to kill only once
    KillUnit(aFrom, True, False);
end;


function TKMUnit.GetHitPointsMax: Byte;
begin
  Result := fResource.UnitDat[fUnitType].HitPoints;
end;


procedure TKMUnit.CancelUnitTask;
begin
  if (fUnitTask <> nil)
  and (fCurrentAction is TUnitActionWalkTo)
  and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
    AbandonWalk;
  FreeAndNil(fUnitTask);
end;


procedure TKMUnit.SetInHouse(aInHouse: TKMHouse);
begin
  gHands.CleanUpHousePointer(fInHouse);
  if aInHouse <> nil then
    fInHouse := aInHouse.GetHousePointer;
end;


function TKMUnit.HitTest(X,Y: Integer; const UT:TUnitType = ut_Any): Boolean;
begin
  Result := (X = fCurrPosition.X) and //Comparing X,Y to CurrentPosition separately, cos they can be negative numbers
            (Y = fCurrPosition.Y) and
            ((fUnitType = UT) or (UT = ut_Any));
end;


//As long as we only ever set PrevPos to NextPos and do so everytime before NextPos changes,
//there can be no problems (as were occurring in GetSlide)
//This procedure ensures that these values always get updated correctly so we don't get a problem
//where GetLength(PrevPosition,NextPosition) > sqrt(2)
procedure TKMUnit.SetNextPosition(aLoc: TKMPoint);
begin
  fPrevPosition := NextPosition;
  fNextPosition := aLoc;
  //If we're not using dynamic fog of war we only need to update it when the unit steps on a new tile
  if not DYNAMIC_FOG_OF_WAR and (fOwner <> PLAYER_ANIMAL) then
    gHands.RevealForTeam(fOwner, fCurrPosition, fResource.UnitDat[fUnitType].Sight, FOG_OF_WAR_INC);
end;


//Only ClearUnit can set fDirection to NA, no other circumstances it is allowed
procedure TKMUnit.SetDirection(aValue: TKMDirection);
begin
  Assert(aValue <> dir_NA);
  fDirection := aValue;

  //if fCurrentAction is TUnitActionStay then
  //  AnimStep := UnitStillFrames[fDirection];
end;


//Assign the following Action to unit and set AnimStep
procedure TKMUnit.SetAction(aAction: TUnitAction; aStep: Integer=0);
begin
  AnimStep := aStep;
  if aAction = nil then
  begin
    FreeAndNil(fCurrentAction);
    exit;
  end;
  if not fResource.UnitDat[fUnitType].SupportsAction(aAction.ActionType) then
  begin
    Assert(false, 'Unit '+fResource.UnitDat[UnitType].GUIName+' was asked to do unsupported action');
    FreeAndNil(aAction);
    exit;
  end;
  if fCurrentAction <> aAction then
  begin
    fCurrentAction.Free;
    fCurrentAction := aAction;
  end;
end;


procedure TKMUnit.SetActionFight(aAction: TUnitActionType; aOpponent: TKMUnit);
var Cycle, Step: Byte;
begin
  //Archers should start in the reloading if they shot recently phase to avoid rate of fire exploit
  Step := 0; //Default
  Cycle := max(fResource.UnitDat[UnitType].UnitAnim[aAction, Direction].Count, 1);
  if (TKMUnitWarrior(Self).IsRanged) and TKMUnitWarrior(Self).NeedsToReload(Cycle) then
  begin
    //Skip the unit's animation forward to 1 step AFTER firing
    case UnitType of
      ut_Arbaletman,
      ut_Bowman:     Step := (FIRING_DELAY+(gGame.GameTickCount-TKMUnitWarrior(Self).LastShootTime)) mod Cycle;
      ut_Slingshot:  Step := (SLINGSHOT_FIRING_DELAY+(gGame.GameTickCount-TKMUnitWarrior(Self).LastShootTime)) mod Cycle;
    end;
  end;

  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    raise ELocError.Create('Unit fight overrides walk', fCurrPosition);
  SetAction(TUnitActionFight.Create(Self, aAction, aOpponent), Step);
end;


procedure TKMUnit.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse);
begin
  SetAction(TUnitActionGoInOut.Create(Self, aAction, aGoDir, aHouse));
end;


procedure TKMUnit.SetActionStay(aTimeToStay: Integer; aAction: TUnitActionType; aStayStill: Boolean=true; aStillFrame:byte=0; aStep: Integer=0);
begin
  //When standing still in walk, use default frame
  if (aAction = ua_Walk) and aStayStill then
  begin
    aStillFrame := UnitStillFrames[Direction];
    aStep := UnitStillFrames[Direction];
  end;
  SetAction(TUnitActionStay.Create(Self, aTimeToStay, aAction, aStayStill, aStillFrame, false), aStep);
end;


procedure TKMUnit.SetActionStorm(aRow: Integer);
begin
  SetAction(TUnitActionStormAttack.Create(Self, ua_Walk, aRow), 0); //Action is ua_Walk for that is the inital one
end;


procedure TKMUnit.SetActionSteer;
begin
  SetAction(TUnitActionSteer.Create(Self, ua_Walk, True), 0);
end;


//Same as above but we will ignore get-out-of-the-way (push) requests from interaction system
procedure TKMUnit.SetActionLockedStay(aTimeToStay: Integer; aAction: TUnitActionType; aStayStill: Boolean=true; aStillFrame:byte=0; aStep: Integer=0);
begin
  //When standing still in walk, use default frame
  if (aAction = ua_Walk) and aStayStill then
  begin
    aStillFrame := UnitStillFrames[Direction];
    aStep := UnitStillFrames[Direction];
  end;
  SetAction(TUnitActionStay.Create(Self, aTimeToStay, aAction, aStayStill, aStillFrame, true), aStep);
end;


//WalkTo action with exact options (retranslated from WalkTo if Obstcale met)
procedure TKMUnit.SetActionWalk(aLocB: TKMPoint; aActionType:TUnitActionType; aDistance:single; aTargetUnit: TKMUnit; aTargetHouse: TKMHouse);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then Assert(false);
  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, false, aTargetUnit, aTargetHouse));
end;


//Approach house
procedure TKMUnit.SetActionWalkToHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TUnitActionType = ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    Assert(False);

  SetAction(TUnitActionWalkTo.Create( Self,               //Who's walking
                                      //Target position is the closest cell to our current position (only used for estimating in path finding)
                                      aHouse.GetClosestCell(Self.GetPosition),
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      false,              //If we were pushed
                                      nil,                //Unit
                                      aHouse              //House
                                      ));
end;


procedure TKMUnit.SetActionWalkFromHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TUnitActionType = ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    Assert(False);

  //todo: Make unit walk away from House
  SetActionStay(20, aActionType);
end;


//Approach unit
procedure TKMUnit.SetActionWalkToUnit(aUnit: TKMUnit; aDistance: Single; aActionType:TUnitActionType=ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
   Assert(False);

  Assert(aDistance >= 1, 'Should not walk to units place');
  SetAction(TUnitActionWalkTo.Create( Self,               //Who's walking
                                      aUnit.fCurrPosition,//Target position
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      False,              //If we were pushed
                                      aUnit,              //Unit
                                      nil                 //House
                                      ));
end;


procedure TKMUnit.SetActionWalkFromUnit(aUnit: TKMUnit; aDistance: Single; aActionType:TUnitActionType=ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    Assert(False);

  //todo: Make unit walk away from Unit
  SetActionStay(20, aActionType);
end;


//Walk to spot or its neighbourhood
procedure TKMUnit.SetActionWalkToSpot(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk; aUseExactTarget: Boolean=true);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    Assert(False, 'Interrupting unabandonable Walk action');

  if not aUseExactTarget and not (Self is TKMUnitWarrior) and not (Self.fUnitTask is TTaskMining) then
    Assert(False, 'Only true warriors don''t care ''bout exact location on reposition; Miners compete over resources, so they can handle is location is taken already');

  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, 0, false, nil, nil, aUseExactTarget));
end;


//We were pushed (walk to spot with wider Passability)
procedure TKMUnit.SetActionWalkPushed(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk);
begin
  //1. Only idle units can be pushed, for they are low priority to busy units
  //2. If unit can't get away it will re-push itself once again
  Assert(((GetUnitAction is TUnitActionStay) and (not GetUnitAction.Locked)) or
         ((GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(GetUnitAction).CanAbandonExternal));

  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, 0, true, nil, nil));
  //Once pushed, unit will try to walk away, if he bumps into more units he will
  //
end;


procedure TKMUnit.SetActionAbandonWalk(aLocB: TKMPoint; aActionType:TUnitActionType=ua_Walk);
var TempVertexOccupied: TKMPoint;
begin
  if GetUnitAction is TUnitActionWalkTo then
  begin
    TempVertexOccupied := TUnitActionWalkTo(GetUnitAction).fVertexOccupied;
    TUnitActionWalkTo(GetUnitAction).fVertexOccupied := KMPoint(0,0); //So it doesn't try to DecVertex on destroy (now it's AbandonWalk's responsibility)
  end
  else
    TempVertexOccupied := KMPoint(0,0);

  SetAction(TUnitActionAbandonWalk.Create(Self, aLocB, TempVertexOccupied, aActionType), AnimStep); //Use the current animation step, to ensure smooth transition
end;


procedure TKMUnit.AbandonWalk;
begin
  if GetUnitAction is TUnitActionWalkTo then
    SetActionAbandonWalk(NextPosition, ua_Walk)
  else
    SetActionLockedStay(0, ua_Walk); //Error
end;


//Specific unit desired passability may depend on several factors
function TKMUnit.GetDesiredPassability: TPassability;
begin
  Result := fResource.UnitDat[fUnitType].DesiredPassability;

  //Delivery to unit
  if (fUnitType = ut_Serf)
  and (fUnitTask is TTaskDeliver)
  and (TTaskDeliver(fUnitTask).DeliverKind = dk_ToUnit)
  then
    Result := CanWalk;

  //Preparing house area
  if (fUnitType = ut_Worker) and (fUnitTask is TTaskBuildHouseArea)
  and TTaskBuildHouseArea(fUnitTask).Digging
  then
    Result := CanWorker; //Special mode that allows us to walk on building sites

  //Miners at work need to go off roads
  if (fUnitType in [ut_Woodcutter, ut_Farmer, ut_Fisher, ut_StoneCutter])
  and (fUnitTask is TTaskMining)
  then
    Result := CanWalk;
end;


procedure TKMUnit.Feed(Amount: Single);
begin
  fCondition := Math.min(fCondition + Round(Amount), UNIT_MAX_CONDITION);
end;


//It's better not to start doing anything with dying units
function TKMUnit.IsDeadOrDying: Boolean;
begin
  Result := fIsDead or fKillASAP or (fUnitTask is TTaskDie);
end;


function TKMUnit.CanGoEat: Boolean;
begin
  Result := gHands[fOwner].FindInn(fCurrPosition,Self) <> nil;
end;


function TKMUnit.CanWalkDiagonaly(aFrom, aTo: TKMPoint): Boolean;
begin
  Result := gTerrain.CanWalkDiagonaly(aFrom, aTo.X, aTo.Y);
end;


function TKMUnit.CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := gTerrain.Route_CanBeMade(GetPosition, aTo, DesiredPassability, aDistance);
end;


function TKMUnit.CanWalkTo(aTo: TKMPoint; aPass: TPassability; aDistance: Single): Boolean;
begin
  Result := gTerrain.Route_CanBeMade(GetPosition, aTo, aPass, aDistance);
end;


function TKMUnit.CanWalkTo(aFrom, aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := gTerrain.Route_CanBeMade(aFrom, aTo, DesiredPassability, aDistance);
end;


function TKMUnit.CanWalkTo(aFrom, aTo: TKMPoint; aPass: TPassability; aDistance: Single): Boolean;
begin
  Result := gTerrain.Route_CanBeMade(aFrom, aTo, aPass, aDistance);
end;


//Check if a route can be made to any tile around this house
function TKMUnit.CanWalkTo(aFrom: TKMPoint; aHouse: TKMHouse; aPass: TPassability; aDistance: Single): Boolean;
var
  I: Integer;
  Cells: TKMPointList;
begin
  Result := False;
  Cells := TKMPointList.Create;
  try
    aHouse.GetListOfCellsWithin(Cells);
    for I := 0 to Cells.Count - 1 do
      Result := Result or gTerrain.Route_CanBeMade(aFrom, Cells[I], aPass, aDistance);
  finally
    Cells.Free;
  end;
end;


function TKMUnit.CanStepTo(X,Y: Integer; aPass: TPassability): Boolean;
begin
  Result := gTerrain.TileInMapCoords(X,Y)
        and (gTerrain.Land[Y,X].IsUnit = nil)
        and (gTerrain.CheckPassability(KMPoint(X,Y), aPass))
        and (not KMStepIsDiag(GetPosition, KMPoint(X,Y)) //Only check vertex usage if the step is diagonal
             or (not gTerrain.HasVertexUnit(KMGetDiagVertex(GetPosition, KMPoint(X,Y)))))
        and (gTerrain.CanWalkDiagonaly(GetPosition, X, Y));
end;


procedure TKMUnit.UpdateThoughts;
begin
  if (fThought <> th_Death) and (fCondition <= UNIT_MIN_CONDITION div 3) then
    fThought := th_Death;

  if (fThought in [th_Death, th_Eat]) and (fCondition > UNIT_MIN_CONDITION) then
    fThought := th_None;

  if (fUnitTask is TTaskDie) then //Clear thought if we are in the process of dying
    fThought := th_None;
end;


//Return true if the unit has to be killed due to lack of space
function TKMUnit.UpdateVisibility: Boolean;
begin
  Result := False;
  if fInHouse = nil then Exit; //There's nothing to update, we are always visible

  if fInHouse.IsDestroyed then //Someone has destroyed the house we were in
  begin
    fVisible := True;
    //If we are walking into/out of the house then don't set our position, ActionGoInOut will sort it out
    if (not (GetUnitAction is TUnitActionGoInOut))
    or (not TUnitActionGoInOut(GetUnitAction).GetHasStarted)
    or (TUnitActionGoInOut(GetUnitAction).GetWaitingForPush) then
    begin
      //Position in a spiral nearest to entrance of house, updating IsUnit.
      if not gHands.FindPlaceForUnit(fInHouse.GetEntrance.X, fInHouse.GetEntrance.Y, UnitType, fCurrPosition, gTerrain.GetWalkConnectID(fInHouse.GetEntrance)) then
      begin
        //There is no space for this unit so it must be destroyed
        //todo: re-route to KillUnit and let it sort out that unit is invisible and cant be placed
        if (gHands <> nil) and (fOwner <> PLAYER_NONE) then
        begin
          gHands[fOwner].Stats.UnitLost(fUnitType);
          fScripting.ProcUnitDied(Self, -1);
        end;
        CloseUnit(False); //Close the unit without removing tile usage (because this unit was in a house it has none)
        Result := true;
        exit;
      end;
      //OnWarriorWalkOut usually happens in TUnitActionGoInOut, otherwise the warrior doesn't get assigned a group
      if (Self is TKMUnitWarrior) and Assigned(TKMUnitWarrior(Self).OnWarriorWalkOut) then
        TKMUnitWarrior(Self).OnWarriorWalkOut(TKMUnitWarrior(Self));

      //Make sure these are reset properly
      Assert(not gTerrain.HasUnit(fCurrPosition));
      IsExchanging := false;
      fPosition := KMPointF(fCurrPosition);
      fPrevPosition := fCurrPosition;
      fNextPosition := fCurrPosition;
      gTerrain.UnitAdd(fCurrPosition, Self); //Unit was not occupying tile while inside the house, hence just add do not remove
      if GetUnitAction is TUnitActionGoInOut then
        SetActionLockedStay(0, ua_Walk); //Abandon the walk out in this case

      if (UnitTask is TTaskGoEat) and (TTaskGoEat(UnitTask).Eating) then
      begin
        FreeAndNil(fUnitTask); //Stop the eating animation and makes the unit appear
        SetActionStay(0, ua_Walk); //Free the current action and give the unit a temporary one
      end;
      //If we were idle abandon our action so we look for a new house immediately (rather than after 20 seconds for the fisherman)
      if (UnitTask = nil) and (GetUnitAction is TUnitActionStay) and not TUnitActionStay(GetUnitAction).Locked then
        SetActionStay(0, ua_Walk); //Free the current action and give the unit a temporary one
    end;
    SetInHouse(nil); //Can't be in a destroyed house
  end;
end;


procedure TKMUnit.VertexAdd(aFrom, aTo: TKMPoint);
begin
  gTerrain.UnitVertexAdd(aFrom, aTo);
end;

procedure TKMUnit.VertexRem(aLoc: TKMPoint);
begin
  gTerrain.UnitVertexRem(aLoc); //Unoccupy vertex
end;


function TKMUnit.VertexUsageCompatible(aFrom, aTo: TKMPoint): Boolean;
begin
  Result := gTerrain.VertexUsageCompatible(aFrom, aTo);
end;


procedure TKMUnit.Walk(aFrom, aTo: TKMPoint);
begin
  gTerrain.UnitWalk(aFrom, aTo, Self)
end;


function TKMUnit.GetActivityText: UnicodeString;
const
  TASK_TEXT: array[TUnitTaskName] of Integer = (
      -1,-1,                   //utn_Unknown, utn_SelfTrain
      TX_UNIT_TASK_DELVERING,  //utn_Deliver
      TX_UNIT_TASK_ROAD,       //utn_BuildRoad
      TX_UNIT_TASK_WINEFIELD,  //utn_BuildWine
      TX_UNIT_TASK_FIELD,      //utn_BuildField
      TX_UNIT_TASK_HOUSE_SITE, //utn_BuildHouseArea
      TX_UNIT_TASK_HOUSE,      //utn_BuildHouse
      TX_UNIT_TASK_REPAIRING,  //utn_BuildHouseRepair
      TX_UNIT_TASK_HOME,       //utn_GoHome
      TX_UNIT_TASK_INN,        //utn_GoEat
      -1,                      //utn_Mining (overridden by Citizen)
      -1,                      //utn_Die (never visible)
      TX_UNIT_TASK_INN,        //utn_GoOutShowHungry
      -1,                      //utn_AttackHouse (overridden by Warrior)
      -1                       //utn_ThrowRock (never visible)
    );
begin
  if (fUnitTask <> nil) and (TASK_TEXT[fUnitTask.TaskName] <> -1) then
    Result := gResTexts[TASK_TEXT[fUnitTask.TaskName]]
  else
    Result := gResTexts[TX_UNIT_TASK_IDLE];
end;


procedure TKMUnit.UpdateHitPoints;
begin
  //Use fHitPointCounter as a counter to restore hit points every X ticks (Humbelum says even when in fights)
  if HITPOINT_RESTORE_PACE = 0 then Exit; //0 pace means don't restore

  if (fHitPointCounter mod HITPOINT_RESTORE_PACE = 0) and (fHitPoints < HitPointsMax) then
    Inc(fHitPoints);

  Inc(fHitPointCounter); //Increasing each tick by 1 would require 13,6 years to overflow Cardinal
end;


function TKMUnit.GetSlide(aCheck: TCheckAxis): Single;
//Pixel positions (waypoints) for sliding around other units. Uses a lookup to save on-the-fly calculations.
//Follows a sort of a bell curve (normal distribution) shape for realistic acceleration/deceleration.
//I tweaked it by hand to look similar to KaM.
//1st row for straight, 2nd for diagonal sliding
const
  SlideLookup: array[1..2, 0..Round(CELL_SIZE_PX * 1.42)] of byte = ( //1.42 instead of 1.41 because we want to round up just in case (it was causing a crash because Round(40*sqrt(2)) = 57 but Round(40*1.41) = 56)
    (0,0,0,0,0,0,1,1,2,2,3,3,4,5,6,7,7,8,8,9,9,9,9,8,8,7,7,6,5,4,3,3,2,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,6,6,6,5,5,5,4,4,4,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0,0,0,0));
var DY,DX, PixelPos, LookupDiagonal: shortint;
begin
  Result := 0;

  //When going into a house, units "slide" towards the door when it is not on center
  if GetUnitAction is TUnitActionGoInOut then
    Result := Result+TUnitActionGoInOut(GetUnitAction).GetDoorwaySlide(aCheck);

  if (not IsExchanging) or not (GetUnitAction.ActName in [uan_WalkTo, uan_GoInOut]) then exit;

  //Uses Y because a walk in the Y means a slide in the X
  DX := sign(NextPosition.X - fPosition.X);
  DY := sign(NextPosition.Y - fPosition.Y);
  if (aCheck = ax_X) and (DY = 0) then exit; //Unit is not shifted
  if (aCheck = ax_Y) and (DX = 0) then exit;

  LookupDiagonal := abs(DX) + abs(DY); //which gives us swith: 1-straight, 2-diagonal.

  if aCheck = ax_X then
  begin
    PixelPos := Round(abs(fPosition.Y-PrevPosition.Y)*CELL_SIZE_PX*sqrt(LookupDiagonal)); //Diagonal movement *sqrt(2)
    Result := Result+(DY*SlideLookup[LookupDiagonal,PixelPos])/CELL_SIZE_PX;
  end;
  if aCheck = ax_Y then
  begin
    PixelPos := Round(abs(fPosition.X-PrevPosition.X)*CELL_SIZE_PX*sqrt(LookupDiagonal)); //Diagonal movement *sqrt(2)
    Result := Result-(DX*SlideLookup[LookupDiagonal,PixelPos])/CELL_SIZE_PX;
  end;
end;


function TKMUnit.PathfindingShouldAvoid: Boolean;
begin
  Result := not (fCurrentAction is TUnitActionWalkTo); //If we're walking, pathfinding should not route around us
end;


function TKMUnit.GetMovementVector: TKMPointF;
var MovementSpeed:single;
begin
  if (GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(GetUnitAction).DoesWalking then
    MovementSpeed := fResource.UnitDat[fUnitType].Speed
  else
  if (GetUnitAction is TUnitActionStormAttack) then
    MovementSpeed := TUnitActionStormAttack(GetUnitAction).GetSpeed
  else
    MovementSpeed := 0;

  Result.X := KMGetVertex(fDirection).X * MovementSpeed;
  Result.Y := KMGetVertex(fDirection).Y * MovementSpeed;
end;


function TKMUnit.IsIdle: Boolean;
begin
  Result := (fUnitTask = nil) and ((fCurrentAction is TUnitActionStay) and not TUnitActionStay(fCurrentAction).Locked);
end;


procedure TKMUnit.Save(SaveStream: TKMemoryStream);
var
  HasTask, HasAct: Boolean;
  ActName: TUnitActionName;
begin
  SaveStream.Write(fUnitType, SizeOf(fUnitType));

  HasTask := fUnitTask <> nil; //Thats our switch to know if unit should write down his task.
  SaveStream.Write(HasTask);
  if HasTask then
  begin
    //We save TaskName to know which Task class to load
    SaveStream.Write(fUnitTask.TaskName, SizeOf(fUnitTask.TaskName));
    fUnitTask.Save(SaveStream);
  end;

  HasAct := fCurrentAction <> nil;
  SaveStream.Write(HasAct);
  if HasAct then
  begin
    ActName := fCurrentAction.ActName; //Can not pass function result to Write
    //We save ActName to know which Task class to load
    SaveStream.Write(ActName, SizeOf(ActName));
    fCurrentAction.Save(SaveStream);
  end;

  SaveStream.Write(fThought, SizeOf(fThought));
  SaveStream.Write(fCondition);
  SaveStream.Write(fTicker);
  SaveStream.Write(fHitPoints);
  SaveStream.Write(fHitPointCounter);

  if fInHouse <> nil then
    SaveStream.Write(fInHouse.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(fOwner, SizeOf(fOwner));

  if fHome <> nil then
    SaveStream.Write(fHome.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(fPosition);
  SaveStream.Write(fVisible);
  SaveStream.Write(fIsDead);
  SaveStream.Write(fKillASAP);
  SaveStream.Write(fKillASAPShowAnimation);
  SaveStream.Write(IsExchanging);
  SaveStream.Write(fPointerCount);

  SaveStream.Write(fUID);
  SaveStream.Write(AnimStep);
  SaveStream.Write(fDirection);
  SaveStream.Write(fCurrPosition);
  SaveStream.Write(fPrevPosition);
  SaveStream.Write(fNextPosition);
end;


{Here are common Unit.UpdateState routines}
function TKMUnit.UpdateState: Boolean;
begin
  //There are layers of unit activity (bottom to top):
  // - Action (Atom creating layer (walk 1frame, etc..))
  // - Task (Action creating layer)
  // - specific UpdateState (Task creating layer)

  Result := True;

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action at start of TKMUnit.UpdateState',fCurrPosition);

  //UpdateState can happen right after unit gets killed (Exchange still in progress)
  if fKillASAP
  and not ((fCurrentAction is TUnitActionWalkTo) and TUnitActionWalkTo(fCurrentAction).DoingExchange) then
  begin
    DoKill(fKillASAPShowAnimation);
    fKillASAP := False;
    Assert(IsDeadOrDying); //Just in case KillUnit failed
  end;

  Inc(fTicker);

  //Update hunger
  if (fTicker mod CONDITION_PACE = 0)
  and (fCondition > 0)
  and not ((fUnitTask is TTaskGoEat) and TTaskGoEat(fUnitTask).Eating) then
    //Make unit hungry as long as they are not currently eating in the inn
    Dec(fCondition);

  //Unit killing could be postponed by few ticks, hence fCondition could be <0
  if fCondition <= 0 then
    KillUnit(-1, True, False);

  //We only need to update fog of war regularly if we're using dynamic fog of war, otherwise only update it when the unit moves
  if DYNAMIC_FOG_OF_WAR and (fTicker mod 10 = 0) then
    gHands.RevealForTeam(fOwner, fCurrPosition, fResource.UnitDat[fUnitType].Sight, FOG_OF_WAR_INC);

  UpdateThoughts;
  UpdateHitPoints;
  if UpdateVisibility then exit; //incase units home was destroyed. Returns true if the unit was killed due to lack of space

  //Shortcut to freeze unit in place if it's on an unwalkable tile. We use fNextPosition rather than fCurrPosition
  //because once we have taken a step from a tile we no longer care about it. (fNextPosition matches up with IsUnit in terrain)
  if fCurrentAction is TUnitActionWalkTo then
    if DesiredPassability = CanWalkRoad then
    begin
      if not gTerrain.CheckPassability(fNextPosition, CanWalk) then
        raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' on unwalkable tile at '+KM_Points.TypeToString(fNextPosition)+' pass CanWalk',fNextPosition);
    end else
    if not gTerrain.CheckPassability(fNextPosition, DesiredPassability) then
      raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' on unwalkable tile at '+KM_Points.TypeToString(fNextPosition)+' pass '+GetEnumName(TypeInfo(TPassability), Byte(DesiredPassability)),fNextPosition);


  //
  //Performing Tasks and Actions now
  //------------------------------------------------------------------------------------------------
  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].GUIName+' has no action in TKMUnit.UpdateState',fCurrPosition);

  fCurrPosition := KMPointRound(fPosition);
  case fCurrentAction.Execute of
    ActContinues: begin fCurrPosition := KMPointRound(fPosition); exit; end;
    ActAborted:   begin FreeAndNil(fCurrentAction); FreeAndNil(fUnitTask); end;
    ActDone:      FreeAndNil(fCurrentAction);
  end;
  fCurrPosition := KMPointRound(fPosition);

  if fUnitTask <> nil then
  case fUnitTask.Execute of
    TaskContinues:  exit;
    TaskDone:       FreeAndNil(fUnitTask);
  end;

  //If we get to this point then it means that common part is done and now
  //we can perform unit-specific activities (ask for job, etc..)
  Result := false;
end;


procedure TKMUnit.Paint;
begin
  //Here should be catched any cases where unit has no current action - this is a flaw in TTasks somewhere
  //Unit always meant to have some Action performed.
  //However, do not assert it here because then the player cannot close the message (paint happens repeatedly)
  //We check it at the start and end of UpdateState, that is the only place.
  if fCurrentAction <> nil then
    fCurrentAction.Paint;

  if SHOW_POINTER_DOTS then
    fRenderAux.UnitPointers(fPosition.X + 0.5 + GetSlide(ax_X), fPosition.Y + 1   + GetSlide(ax_Y), fPointerCount);
end;


{ TUnitTask }
constructor TUnitTask.Create(aUnit: TKMUnit);
begin
  inherited Create;
  fTaskName := utn_Unknown;
  Assert(aUnit <> nil);
  fUnit := aUnit.GetUnitPointer;
  fUnit.SetActionLockedStay(0,ua_Walk);
  fPhase    := 0;
  fPhase2   := 0;
end;


constructor TUnitTask.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fTaskName, SizeOf(fTaskName));
  LoadStream.Read(fUnit, 4);//Substitute it with reference on SyncLoad
  LoadStream.Read(fPhase);
  LoadStream.Read(fPhase2);
end;


procedure TUnitTask.SyncLoad;
begin
  fUnit := gHands.GetUnitByUID(cardinal(fUnit));
end;


destructor TUnitTask.Destroy;
begin
  fUnit.Thought := th_None; //Stop any thoughts
  gHands.CleanUpUnitPointer(fUnit);
  fPhase        := high(byte)-1; //-1 so that if it is increased on the next run it won't overrun before exiting
  fPhase2       := high(byte)-1;
  inherited;
end;


function TUnitTask.WalkShouldAbandon: Boolean;
begin
  Result := False; //Only used in some child classes
end;


procedure TUnitTask.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fTaskName, SizeOf(fTaskName)); //Save task type before anything else for it will be used on loading to create specific task type
  if fUnit <> nil then
    SaveStream.Write(fUnit.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fPhase);
  SaveStream.Write(fPhase2);
end;


{ TUnitAction }
constructor TUnitAction.Create(aUnit: TKMUnit; aActionType: TUnitActionType; aLocked: Boolean);
begin
  inherited Create;

  //Unit who will be performing the action
  //Does not require pointer tracking because action should always be destroyed before the unit that owns it
  fUnit       := aUnit;
  fActionType := aActionType;
  Locked      := aLocked;
  StepDone    := False;
end;


constructor TUnitAction.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fActionType, SizeOf(fActionType));
  LoadStream.Read(fUnit, 4);
  LoadStream.Read(Locked);
  LoadStream.Read(StepDone);
end;


procedure TUnitAction.SyncLoad;
begin
  fUnit := gHands.GetUnitByUID(cardinal(fUnit));
end;


procedure TUnitAction.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fActionType, SizeOf(fActionType));
  if fUnit <> nil then
    SaveStream.Write(fUnit.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(Locked);
  SaveStream.Write(StepDone);
end;


procedure TUnitAction.Paint;
begin
  //Used for debug, paint action properties here
end;


end.
