unit KM_Units;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KromUtils,
  KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units_WorkPlan, KM_Points;

//Memo on directives:
//Dynamic - declared and used (overriden) occasionally
//Virtual - declared and used (overriden) always
//Abstract - declared but used only in child classes

type TCheckAxis = (ax_X, ax_Y);

  TKMUnit = class;
  TKMUnitWorker = class;

  TActionResult = (ActContinues, ActDone, ActAborted); //

  TUnitAction = class
  protected
    fActionName: TUnitActionName;
    fActionType: TUnitActionType;
  public
    Locked: boolean; //Means that unit can't take part in interaction, must stay on its tile
    StepDone: boolean; //True when single action element is done (unit walked to new tile, single attack loop done)
    constructor Create(aActionType: TUnitActionType);
    constructor Load(LoadStream:TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    property GetActionType: TUnitActionType read fActionType;
    function GetExplanation:string; virtual; abstract;
    function Execute(KMUnit: TKMUnit):TActionResult; virtual; abstract;
    procedure Save(SaveStream:TKMemoryStream); virtual;
    procedure Paint; virtual;
  end;

  TTaskResult = (TaskContinues, TaskDone); //There's no difference between Done and Aborted

  TUnitTask = class
  protected
    fTaskName:TUnitTaskName;
    fUnit:TKMUnit; //Unit who's performing the Task
    fPhase:byte;
    fPhase2:byte;
  public
    constructor Create(aUnit:TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); virtual;
    procedure SyncLoad; dynamic;
    destructor Destroy; override;

    function WalkShouldAbandon:boolean; dynamic;
    property Phase:byte read fPhase write fPhase;

    function Execute:TTaskResult; virtual; abstract;
    procedure Save(SaveStream:TKMemoryStream); virtual;
  end;


  TKMUnit = class
  protected //Accessible for child classes
    fID:integer; //unique unit ID, used for save/load to sync to
    fUnitType: TUnitType;
    fUnitTask: TUnitTask;
    fCurrentAction: TUnitAction;
    fThought:TUnitThought;
    fHitPoints:byte;
    fHitPointCounter: cardinal;
    fCondition:integer; //Unit condition, when it reaches zero unit should die
    fOwner:TPlayerIndex;
    fHome:TKMHouse;
    fPosition: TKMPointF;
    fVisible:boolean;
    fIsDead:boolean;
    fKillASAP:boolean;
    fPointerCount:integer;
    fInHouse: TKMHouse; //House we are currently in
    fCurrPosition: TKMPoint; //Where we are now
    fPrevPosition: TKMPoint; //Where we were
    fNextPosition: TKMPoint; //Where we will be. Next tile in route or same tile if stay on place
    fDirection: TKMDirection; //

    procedure SetDirection(aValue:TKMDirection);
    procedure SetAction(aAction: TUnitAction; aStep:integer=0);
    function GetSlide(aCheck:TCheckAxis): single;

    procedure UpdateHunger;
    procedure UpdateFOW;
    procedure UpdateThoughts;
    function UpdateVisibility:boolean;
    procedure UpdateHitPoints;
  public
    AnimStep: integer;
    IsExchanging:boolean; //Current walk is an exchange, used for sliding

    constructor Create(aOwner: shortint; PosX, PosY:integer; aUnitType:TUnitType);
    constructor Load(LoadStream:TKMemoryStream); dynamic;
    procedure SyncLoad; virtual;
    destructor Destroy; override;

    function GetUnitPointer:TKMUnit; //Returns self and adds one to the pointer counter
    procedure ReleaseUnitPointer;  //Decreases the pointer counter
    property GetPointerCount:integer read fPointerCount;

    procedure KillUnit; virtual; //Creates TTaskDie which then will Close the unit from further access
    procedure CloseUnit(aRemoveTileUsage:boolean=true); dynamic;

    property ID:integer read fID;
    property PrevPosition: TKMPoint read fPrevPosition;
    property NextPosition: TKMPoint read fNextPosition;
    property Direction:TKMDirection read fDirection write SetDirection;

    function HitTest(X,Y:integer; const UT:TUnitType = ut_Any): Boolean;
    procedure UpdateNextPosition(aLoc:TKMPoint);

    procedure SetActionAbandonWalk(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk);
    procedure SetActionFight(aAction: TUnitActionType; aOpponent:TKMUnit);
    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse:TKMHouse); virtual;
    procedure SetActionStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);
    procedure SetActionStorm(aRow:integer);
    procedure SetActionLockedStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);

    procedure SetActionWalk(aLocB:TKMPoint; aActionType:TUnitActionType; aDistance:single; aWalkToNear:boolean; aTargetUnit:TKMUnit; aTargetHouse:TKMHouse);
    procedure SetActionWalkToHouse(aHouse:TKMHouse; aDistance:single; aActionType:TUnitActionType=ua_Walk); overload;
    procedure SetActionWalkToUnit(aUnit:TKMUnit; aDistance:single; aActionType:TUnitActionType=ua_Walk); overload;
    procedure SetActionWalkToSpot(aLocB:TKMPoint; aDistance:single=0; aActionType:TUnitActionType=ua_Walk); overload;
    procedure SetActionWalkPushed(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk);
    procedure SetActionWalkToNear(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk; aTargetCanBeReached:boolean=true);

    procedure Feed(Amount:single);
    procedure AbandonWalk;
    function GetDesiredPassability(aIgnoreRoads:boolean=false):TPassability;
    property GetOwner:TPlayerIndex read fOwner;
    function CanAccessHome: boolean;
    property GetHome:TKMHouse read fHome;
    property GetUnitAction: TUnitAction read fCurrentAction;
    property GetUnitTask: TUnitTask read fUnitTask;
    property SetUnitTask: TUnitTask write fUnitTask;
    property UnitType: TUnitType read fUnitType;
    function GetUnitTaskText:string;
    function GetUnitActText:string;
    property Condition: integer read fCondition write fCondition;
    procedure SetFullCondition;
    function  HitPointsDecrease(aAmount:integer; aPenetratesDefence:boolean):boolean;
    procedure HitPointsIncrease(aAmount:integer);
    property GetHitPoints:byte read fHitPoints;
    function GetMaxHitPoints:byte;
    procedure CancelUnitTask;
    property Visible: boolean read fVisible write fVisible;
    procedure SetInHouse(aInHouse:TKMHouse);
    property IsDead:boolean read fIsDead;
    function IsDeadOrDying:boolean;
    function IsArmyUnit:boolean;
    function CanGoEat:boolean;
    property GetPosition:TKMPoint read fCurrPosition;
    procedure SetPosition(aPos:TKMPoint);
    property PositionF:TKMPointF read fPosition write fPosition;
    property Thought:TUnitThought read fThought write fThought;
    function GetMovementVector: TKMPointF;

    procedure Save(SaveStream:TKMemoryStream); virtual;
    function UpdateState:boolean; virtual;
    procedure Paint; virtual;
  end;

  //This is a common class for units going out of their homes for resources
  TKMUnitCitizen = class(TKMUnit)
  private
    fWorkPlan:TUnitWorkPlan; //todo: Move into TKMTaskMining
    function FindHome:boolean;
    function InitiateMining:TUnitTask;
    procedure IssueResourceDepletedMessage;
  public
    constructor Create(aOwner: shortint; PosX, PosY:integer; aUnitType:TUnitType);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    procedure Save(SaveStream:TKMemoryStream); override;
    function UpdateState:boolean; override;
    procedure Paint; override;
  end;


  TKMUnitRecruit = class(TKMUnit)
  private
    function FindHome:boolean;
    function InitiateActivity:TUnitTask;
  public
    function UpdateState:boolean; override;
    procedure Paint; override;
    procedure DestroyInBarracks;
  end;

  //Serf class - transports all goods in game between houses
  TKMUnitSerf = class(TKMUnit)
  private
    fCarry: TResourceType;
  public
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure Save(SaveStream:TKMemoryStream); override;

    property Carry: TResourceType read fCarry;
    procedure CarryGive(Res:TResourceType);
    procedure CarryTake;
    function GetActionFromQueue(aHouse:TKMHouse=nil):TUnitTask;
    procedure SetNewDelivery(aDelivery:TUnitTask);

    function UpdateState:boolean; override;
    procedure Paint; override;
  end;

  //Worker class - builds everything in game
  TKMUnitWorker = class(TKMUnit)
  private
    function GetActionFromQueue:TUnitTask;
  public
    function UpdateState:boolean; override;
    procedure Paint; override;
  end;


  //Animals
  TKMUnitAnimal = class(TKMUnit)
    fFishCount:byte; //1-5
  public
    constructor Create(aOwner: shortint; PosX, PosY:integer; aUnitType:TUnitType); overload;
    constructor Load(LoadStream:TKMemoryStream); override;
    function ReduceFish:boolean;
    procedure Save(SaveStream:TKMemoryStream); override;
    function UpdateState:boolean; override;
    procedure Paint; override;
  end;


  TKMUnitsCollection = class(TKMList)
  private
    function GetUnit(Index: Integer): TKMUnit;
    procedure SetUnit(Index: Integer; Item: TKMUnit);
    property Units[Index: Integer]: TKMUnit read GetUnit write SetUnit; //Use instead of Items[.]
  public
    destructor Destroy; override;
    function Add(aOwner:TPlayerIndex; aUnitType:TUnitType; PosX, PosY:integer; AutoPlace:boolean=true):TKMUnit;
    function AddGroup(aOwner:TPlayerIndex;  aUnitType:TUnitType; PosX, PosY:integer; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
    procedure RemoveUnit(aUnit:TKMUnit);
    procedure OwnerUpdate(aOwner:TPlayerIndex);
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    function GetUnitByID(aID: Integer): TKMUnit;
    procedure GetLocations(var Loc:TKMPointList; aUnitType:TUnitType=ut_Any);
    function GetClosestUnit(aPoint: TKMPoint):TKMUnit;
    function GetTotalPointers: integer;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Render, KM_RenderAux, KM_TextLibrary, KM_PlayersCollection, KM_Viewport, KM_Game,
KM_UnitActionAbandonWalk, KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionStay, KM_UnitActionWalkTo, KM_UnitActionStormAttack,
KM_Units_Warrior, KM_Terrain, KM_ResourceGFX, KM_Log,

KM_UnitTaskGoOutShowHungry, KM_UnitTaskBuild, KM_UnitTaskDie, KM_UnitTaskGoHome, KM_UnitTaskDelivery, KM_UnitTaskGoEat, KM_UnitTaskAttackHouse, KM_UnitTaskSelfTrain, KM_UnitTaskThrowRock, KM_UnitTaskMining;


{ TKMUnitCitizen }
constructor TKMUnitCitizen.Create(aOwner:TPlayerIndex; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  fWorkPlan := TUnitWorkPlan.Create;
end;


constructor TKMUnitCitizen.Load(LoadStream:TKMemoryStream);
var HasPlan:boolean;
begin
  Inherited;
  fWorkPlan := TUnitWorkPlan.Create;
  LoadStream.Read(HasPlan);
  if HasPlan then
  begin
    fWorkPlan.Load(LoadStream);
    if fUnitTask is TTaskMining then
      TTaskMining(fUnitTask).WorkPlan := fWorkPlan; //restore reference
  end;
end;


destructor TKMUnitCitizen.Destroy;
begin
  FreeAndNil(fWorkPlan);
  Inherited;
end;


{ Find home for unit }
function TKMUnitCitizen.FindHome:boolean;
var H:TKMHouse;
begin
  Result:=false;
  H := fPlayers.Player[fOwner].Houses.FindEmptyHouse(fUnitType,fCurrPosition);
  if H<>nil then begin
    fHome  := H.GetHousePointer;
    Result := true;
  end;
end;


procedure TKMUnitCitizen.Paint;
var Act:TUnitActionType; XPaintPos, YPaintPos: single;
begin
  Inherited;
  if not fVisible then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X+0.5+GetSlide(ax_X);
  YPaintPos := fPosition.Y+ 1 +GetSlide(ax_Y);

  case fCurrentAction.fActionType of
    ua_Walk:
      begin
        fRender.RenderUnit(fUnitType, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
        if fResource.UnitDat[fUnitType].SupportsAction(ua_WalkArm) then
          fRender.RenderUnit(fUnitType, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, false);
      end;
    ua_Work..ua_Eat:
        fRender.RenderUnit(fUnitType, Act, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
    ua_WalkArm .. ua_WalkBooty2:
      begin
        fRender.RenderUnit(fUnitType, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
        fRender.RenderUnit(fUnitType, Act, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, false);
      end;
  end;

  if fThought<>th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, fPosition.Y+1);
end;


procedure TKMUnitCitizen.Save(SaveStream:TKMemoryStream);
var HasPlan:boolean;
begin
  Inherited;
  HasPlan := fWorkPlan<>nil;
  SaveStream.Write(HasPlan);
  if HasPlan then fWorkPlan.Save(SaveStream);
end;


function TKMUnitCitizen.UpdateState:boolean;
var H:TKMHouseInn;
begin
  Result:=true; //Required for override compatibility

  //Reset unit activity if home was destroyed, except when unit is dying or eating (finish eating/dying first)
  if (fHome<>nil)and(fHome.IsDestroyed)and(not(fUnitTask is TTaskDie))and(not(fUnitTask is TTaskGoEat)) then
  begin
    if (fCurrentAction is TUnitActionWalkTo)and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then AbandonWalk;
    FreeAndNil(fUnitTask);
    fPlayers.CleanUpHousePointer(fHome);
  end;

  if Inherited UpdateState then exit;
  if IsDead then exit; //Caused by SelfTrain.Abandoned

  fThought := th_None;

{  if fUnitTask=nil then //Which is always nil if 'Inherited UpdateState' works properly
  if not TestHunger then
  if not TestHasHome then
  if not TestAtHome then
  if not TestMining then
    Idle..}


  if fCondition<UNIT_MIN_CONDITION then
  begin
    H := fPlayers.Player[fOwner].FindInn(fCurrPosition,Self,not fVisible);
    if H<>nil then
      fUnitTask := TTaskGoEat.Create(H,Self)
    else
      if (fHome <> nil) and not fVisible then
        fUnitTask:=TTaskGoOutShowHungry.Create(Self)
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    if fHome=nil then
      if FindHome then
        fUnitTask := TTaskGoHome.Create(Self) //Home found - go there
      else begin
        fThought := th_Quest; //Always show quest when idle, unlike serfs who randomly show it
        SetActionStay(60, ua_Walk) //There's no home
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
        if fUnitTask=nil then //We didn't find any job to do - rest at home
          SetActionStay(fResource.HouseDat[fHome.HouseType].WorkerRest*10, ua_Walk);
      end;

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);
end;


procedure TKMUnitCitizen.IssueResourceDepletedMessage;
var Msg:string;
begin
  if not fHome.ResourceDepletedMsgIssued then
  begin
    case fHome.HouseType of
      ht_Quary:    Msg := fTextLibrary.GetTextString(290);
      ht_CoalMine: Msg := fTextLibrary.GetTextString(291);
      ht_IronMine: Msg := fTextLibrary.GetTextString(292);
      ht_GoldMine: Msg := fTextLibrary.GetTextString(293);
      ht_FisherHut: if not fTerrain.CanFindFishingWater(KMPointBelow(fHome.GetEntrance),RANGE_FISHERMAN) then
                      Msg := fTextLibrary[TX_UNITS_FISHERMAN_TOO_FAR]
                    else
                      Msg := fTextLibrary[TX_UNITS_FISHERMAN_CANNOT_CATCH];
      else         begin Assert(false, fResource.HouseDat[fHome.HouseType].HouseName+' resource cant possibly deplet'); Msg := ''; end;
    end;
    if (Msg <> '') and (fOwner = MyPlayer.PlayerIndex) then //Don't show message for other players
      fGame.fGamePlayInterface.MessageIssue(msgHouse, Msg, fHome.GetEntrance);
    fHome.ResourceDepletedMsgIssued := true;
  end;
end;


function TKMUnitCitizen.InitiateMining:TUnitTask;
var Res:integer;
begin
  Result := nil;

  if not KMSamePoint(fCurrPosition, fHome.GetEntrance) then
    raise ELocError.Create(fTextLibrary[TX_UNITS_MINING_WRONG_SPOT],fCurrPosition);

  Res := 1;
  //Check if House has production orders
  //Random pick from whole amount
  if fHome.DoesOrders then
  begin
    Res := fHome.PickRandomOrder;
    if Res = 0 then Exit;
  end;

  fWorkPlan.FindPlan(fUnitType,fHome.HouseType,fResource.HouseDat[fHome.HouseType].ResOutput[Res],KMPointBelow(fHome.GetEntrance));

  if fWorkPlan.ResourceDepleted then
    IssueResourceDepletedMessage;

  if fWorkPlan.IsIssued
  and ((fWorkPlan.Resource1=rt_None)or(fHome.CheckResIn(fWorkPlan.Resource1)>=fWorkPlan.Count1))
  and ((fWorkPlan.Resource2=rt_None)or(fHome.CheckResIn(fWorkPlan.Resource2)>=fWorkPlan.Count2))
  and (fHome.CheckResOut(fWorkPlan.Product1)<MAX_RES_IN_HOUSE)
  and (fHome.CheckResOut(fWorkPlan.Product2)<MAX_RES_IN_HOUSE) then
  begin
    if fHome.DoesOrders then
      fHome.ResEditOrder(Res, -1); //Take order
    Result := TTaskMining.Create(fWorkPlan, Self);
  end;
end;


{ TKMUnitRecruit }
function TKMUnitRecruit.FindHome:boolean;
var H:TKMHouse;
begin
  Result  := false;
  H := fPlayers.Player[fOwner].Houses.FindEmptyHouse(fUnitType,fCurrPosition);
  if H<>nil then begin
    fHome  := H.GetHousePointer;
    Result := true;
  end;
end;


procedure TKMUnitRecruit.Paint;
var Act:TUnitActionType; XPaintPos, YPaintPos: single;
begin
  Inherited;
  if not fVisible then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X+0.5+GetSlide(ax_X);
  YPaintPos := fPosition.Y+ 1 +GetSlide(ax_Y);

  case fCurrentAction.fActionType of
  ua_Walk:
    begin
      fRender.RenderUnit(fUnitType, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
      if fResource.UnitDat[fUnitType].SupportsAction(ua_WalkArm) then
        fRender.RenderUnit(fUnitType, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, false);
    end;
  ua_Work..ua_Eat:
      fRender.RenderUnit(fUnitType, Act, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
  ua_WalkArm .. ua_WalkBooty2:
    begin
      fRender.RenderUnit(fUnitType, ua_Walk, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);
      fRender.RenderUnit(fUnitType, Act, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, false);
    end;
  end;

  if fThought<>th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, fPosition.Y+1);
end;


procedure TKMUnitRecruit.DestroyInBarracks;
begin
  if fPlayers.Selected = Self then fPlayers.Selected := nil;
  if fGame.fGamePlayInterface.ShownUnit = Self then fGame.fGamePlayInterface.ShowUnitInfo(nil);
  fTerrain.UnitAdd(NextPosition, Self); //CloseUnit removes it, and as we are currently in a house we must first add it
  CloseUnit;
end;


function TKMUnitRecruit.UpdateState:boolean;
var H:TKMHouseInn;
begin
  Result:=true; //Required for override compatibility

  //Reset unit activity if home was destroyed, except when unit is dying or eating (finish eating/dying first)
  if (fHome<>nil)and(fHome.IsDestroyed)and(not(fUnitTask is TTaskDie))and(not(fUnitTask is TTaskGoEat)) then
  begin
    if (fCurrentAction is TUnitActionWalkTo)and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then AbandonWalk;
    FreeAndNil(fUnitTask);
    fPlayers.CleanUpHousePointer(fHome);
  end;

  if Inherited UpdateState then exit;
  if IsDead then exit; //Caused by SelfTrain.Abandoned

  fThought := th_None;


  if fCondition<UNIT_MIN_CONDITION then
  begin
    H:=fPlayers.Player[fOwner].FindInn(fCurrPosition,Self,not fVisible);
    if H<>nil then
      fUnitTask:=TTaskGoEat.Create(H,Self)
    else
      if (fHome <> nil) and not fVisible then
        fUnitTask:=TTaskGoOutShowHungry.Create(Self)
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
          SetActionStay(fResource.HouseDat[fHome.HouseType].WorkerRest*10, ua_Walk);
      end;

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);
end;


function TKMUnitRecruit.InitiateActivity:TUnitTask;
var
  Enemy:TKMUnit;
begin
  Result := nil;
  
  if (fHome is TKMHouseTower) and (not FREE_ROCK_THROWING) and (fHome.CheckResIn(rt_Stone) <= 0) then
    Exit; //Nothing to throw

  Enemy := fTerrain.UnitsHitTestWithinRad(fCurrPosition, RANGE_WATCHTOWER_MIN, RANGE_WATCHTOWER_MAX, fOwner, at_Enemy, dir_NA);

  //Note: In actual game there might be two Towers nearby,
  //both throwing a stone into the same enemy. We should not
  //negate that fact, thats real-life situation.

  if Enemy <> nil then
    Result := TTaskThrowRock.Create(Self, Enemy);
end;


{ TKMSerf }
constructor TKMUnitSerf.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fCarry, SizeOf(fCarry));
end;


procedure TKMUnitSerf.Paint;
var Act:TUnitActionType; XPaintPos, YPaintPos: single;
begin
  Inherited;
  if not fVisible then exit;
  Act := fCurrentAction.fActionType;

  XPaintPos := fPosition.X+0.5+GetSlide(ax_X);
  YPaintPos := fPosition.Y+ 1 +GetSlide(ax_Y);

  fRender.RenderUnit(UnitType, Act, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);

  if fUnitTask is TTaskDie then exit; //Do not show unnecessary arms

  if Carry <> rt_None then
    fRender.RenderUnitCarry(Carry, Direction, AnimStep, XPaintPos, YPaintPos)
  else
    fRender.RenderUnit(UnitType, ua_WalkArm, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, false);

  if fThought <> th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, YPaintPos);
end;


procedure TKMUnitSerf.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fCarry, SizeOf(fCarry));
end;


function TKMUnitSerf.UpdateState:boolean;
var
  H:TKMHouseInn;
  OldThought:TUnitThought;
begin
  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  OldThought:=fThought;
  fThought:=th_None;

  if fCondition<UNIT_MIN_CONDITION then begin
    H:=fPlayers.Player[fOwner].FindInn(fCurrPosition,Self);
    if H<>nil then
      fUnitTask:=TTaskGoEat.Create(H,Self);
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    fUnitTask := GetActionFromQueue;

  //Only show quest thought if we are idle and not thinking anything else (e.g. death)
  if fUnitTask=nil then begin
    if (KaMRandom(2)=0) and (OldThought=th_None) then fThought:=th_Quest; //
    SetActionStay(60,ua_Walk); //Stay idle
  end;

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);
end;


procedure TKMUnitSerf.CarryGive(Res:TResourceType);
begin
  Assert(fCarry=rt_None, 'Giving Serf another Carry');
  fCarry := Res;
end;


procedure TKMUnitSerf.CarryTake;
begin
  Assert(Carry <> rt_None, 'Taking wrong resource from Serf');
  fCarry := rt_None;
end;


function TKMUnitSerf.GetActionFromQueue(aHouse:TKMHouse=nil):TUnitTask;
begin
  Result := fPlayers.Player[fOwner].DeliverList.AskForDelivery(Self, aHouse);
end;


procedure TKMUnitSerf.SetNewDelivery(aDelivery:TUnitTask);
begin
  fUnitTask := aDelivery;
end;


{ TKMWorker }
procedure TKMUnitWorker.Paint;
var XPaintPos, YPaintPos: single;
begin
  Inherited;
  if not fVisible then exit;

  XPaintPos := fPosition.X+0.5+GetSlide(ax_X);
  YPaintPos := fPosition.Y+ 1 +GetSlide(ax_Y);

  fRender.RenderUnit(UnitType, fCurrentAction.fActionType, Direction, AnimStep, XPaintPos, YPaintPos, fPlayers.Player[fOwner].FlagColor, true);

  if fThought<>th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, YPaintPos);
end;


function TKMUnitWorker.UpdateState:boolean;
var
  H:TKMHouseInn;
  OutOfWay: TKMPoint;
begin
  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  if fCondition<UNIT_MIN_CONDITION then begin
    H:=fPlayers.Player[fOwner].FindInn(fCurrPosition,Self);
    if H<>nil then
      fUnitTask:=TTaskGoEat.Create(H,Self);
  end;

  if (fThought = th_Build)and(fUnitTask = nil) then
    fThought := th_None; //Remove build thought if we are no longer doing anything

  //If we are still stuck on a house for some reason, get off it ASAP
  if (mu_HouseFenceNoWalk = fTerrain.Land[fCurrPosition.Y,fCurrPosition.X].Markup) then
  begin
    assert(fPlayers.HousesHitTest(fCurrPosition.X,fCurrPosition.Y) <> nil);
    OutOfWay := KMPointBelow(fPlayers.HousesHitTest(fCurrPosition.X,fCurrPosition.Y).GetEntrance);
    SetActionWalkToSpot(OutOfWay, 0, ua_Walk);
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    fUnitTask:=GetActionFromQueue;

  if (fUnitTask=nil) and (fCurrentAction=nil) then SetActionStay(20,ua_Walk);

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);
end;


function TKMUnitWorker.GetActionFromQueue:TUnitTask;
begin
                     Result:=fPlayers.Player[fOwner].BuildList.AskForHouseRepair(Self);
  if Result=nil then Result:=fPlayers.Player[fOwner].BuildList.AskForHousePlan(Self);
  if Result=nil then Result:=fPlayers.Player[fOwner].BuildList.AskForRoad(Self);
  if Result=nil then Result:=fPlayers.Player[fOwner].BuildList.AskForHouse(Self);
end;


{ TKMUnitAnimal }
constructor TKMUnitAnimal.Create(aOwner: shortint; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  if aUnitType = ut_Fish then fFishCount := 5;  //Always start with 5 fish in the group
end;


constructor TKMUnitAnimal.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fFishCount)
end;


function TKMUnitAnimal.ReduceFish:boolean;
begin
  Result := false;
  if fUnitType <> ut_Fish then exit;
  if fFishCount > 1 then
  begin
    fFishCount := fFishCount-1;
    Result := true;
  end
  else if fFishCount = 1 then
  begin
    KillUnit;
    Result := true;
  end;
end;


procedure TKMUnitAnimal.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fFishCount);
end;


function TKMUnitAnimal.UpdateState:boolean;
var
  Spot:TKMPoint; //Target spot where unit will go
  SpotJit:byte;
begin
  Result:=true; //Required for override compatibility

  fCurrPosition := KMPointRound(fPosition);

  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition); //Someone has nilled our action!

  case fCurrentAction.Execute(Self) of
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
  if (not fTerrain.CheckPassability(fCurrPosition,GetDesiredPassability))
  or fTerrain.CheckAnimalIsStuck(fCurrPosition,GetDesiredPassability,false) then begin
    KillUnit; //Animal is stuck so it dies
    exit;
  end;

  SpotJit:=16; //Initial Spot jitter, it limits number of Spot guessing attempts reducing the range to 0
  repeat //Where unit should go, keep picking until target is walkable for the unit
    dec(SpotJit,1);
    Spot := fTerrain.EnsureTileInMapCoords(fCurrPosition.X+KaMRandomS(SpotJit),fCurrPosition.Y+KaMRandomS(SpotJit));
  until((SpotJit=0)or(fTerrain.Route_CanBeMade(fCurrPosition,Spot,GetDesiredPassability,0, false)));

  if KMSamePoint(fCurrPosition,Spot) then
    SetActionStay(20, ua_Walk)
  else
    SetActionWalkToSpot(Spot);

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);
end;


//For fish the action is the number of fish in the group
procedure TKMUnitAnimal.Paint;
const FishCountAct:array[1..5]of TUnitActionType = (ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Work1);
var Act:TUnitActionType;
begin
  Inherited;
  if fUnitType = ut_Fish then
    Act := FishCountAct[fFishCount]
  else
    Act := fCurrentAction.fActionType;

  fRender.RenderUnit(fUnitType, Act, Direction, AnimStep, fPosition.X+0.5+GetSlide(ax_X), fPosition.Y+1+GetSlide(ax_Y), $FFFFFFFF, true);
end;


{ TKMUnit }
constructor TKMUnit.Create(aOwner:TPlayerIndex; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited Create;
  fID           := fGame.GetNewID;
  fPointerCount := 0;
  fIsDead       := false;
  fKillASAP     := false;
  fThought      := th_None;
  fHome         := nil;
  fInHouse      := nil;
  fPosition.X   := PosX;
  fPosition.Y   := PosY;
  fCurrPosition := KMPoint(PosX,PosY);
  fPrevPosition := fCurrPosition; //Init values
  fNextPosition := fCurrPosition; //Init values
  fOwner        := aOwner;
  fUnitType     := aUnitType;
  Direction     := dir_S;
  fVisible      := true;
  IsExchanging  := false;
  AnimStep      := UnitStillFrames[Direction]; //Use still frame at begining, so units don't all change frame on first tick
  //Units start with a random amount of condition ranging from 3/4 to full.
  //This means that they won't all go eat at the same time and cause crowding, blockages, food shortages and other problems.
  if fGame.GameState <> gsEditor then
    fCondition    := UNIT_MAX_CONDITION - KaMRandom(UNIT_MAX_CONDITION div 4)
  else
    fCondition    := UNIT_MAX_CONDITION div 2;
  fHitPoints    := GetMaxHitPoints;
  fHitPointCounter := 1;

  SetActionStay(10, ua_Walk);
  fTerrain.UnitAdd(NextPosition,Self);
end;


destructor TKMUnit.Destroy;
begin
  if not IsDead then fTerrain.UnitRem(NextPosition); //Happens only when removing player from map on GameStart (network)
  FreeAndNil(fCurrentAction);
  FreeAndNil(fUnitTask);
  SetInHouse(nil); //Free pointer
  Inherited;
end;


constructor TKMUnit.Load(LoadStream:TKMemoryStream);
var HasTask,HasAct:boolean; TaskName:TUnitTaskName; ActName: TUnitActionName;
begin
  Inherited Create;
  LoadStream.Read(fUnitType, SizeOf(fUnitType));
  LoadStream.Read(HasTask);
  if HasTask then
  begin
    LoadStream.Read(TaskName, SizeOf(TaskName)); //Save task type before anything else for it will be used on loading to create specific task type
    LoadStream.Seek(-SizeOf(TaskName), soFromCurrent); //rewind
    case TaskName of
      utn_Unknown:         Assert(false, 'TaskName can''t be handled');
      utn_SelfTrain:       fUnitTask := TTaskSelfTrain.Load(LoadStream);
      utn_Deliver:         fUnitTask := TTaskDeliver.Load(LoadStream);
      utn_BuildRoad:       fUnitTask := TTaskBuildRoad.Load(LoadStream);
      utn_BuildWine:       fUnitTask := TTaskBuildWine.Load(LoadStream);
      utn_BuildField:      fUnitTask := TTaskBuildField.Load(LoadStream);
      utn_BuildWall:       fUnitTask := TTaskBuildWall.Load(LoadStream);
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
    LoadStream.Seek(-SizeOf(ActName), soFromCurrent); //rewind
    case ActName of
      uan_Unknown:     Assert(false, 'ActName can''t be handled');
      uan_Stay:        fCurrentAction := TUnitActionStay.Load(LoadStream);
      uan_WalkTo:      fCurrentAction := TUnitActionWalkTo.Load(LoadStream);
      uan_AbandonWalk: fCurrentAction := TUnitActionAbandonWalk.Load(LoadStream);
      uan_GoInOut:     fCurrentAction := TUnitActionGoInOut.Load(LoadStream);
      uan_Fight:       fCurrentAction := TUnitActionFight.Load(LoadStream);
      uan_StormAttack: fCurrentAction := TUnitActionStormAttack.Load(LoadStream);
      else             Assert(false, 'ActName can''t be handled');
    end;
  end
  else
    fCurrentAction := nil;

  LoadStream.Read(fThought, SizeOf(fThought));
  LoadStream.Read(fCondition);
  LoadStream.Read(fHitPoints);
  LoadStream.Read(fHitPointCounter);
  LoadStream.Read(fInHouse, 4);
  LoadStream.Read(fOwner, SizeOf(fOwner));
  LoadStream.Read(fHome, 4); //Substitute it with reference on SyncLoad
  LoadStream.Read(fPosition);
  LoadStream.Read(fVisible);
  LoadStream.Read(fIsDead);
  LoadStream.Read(fKillASAP);
  LoadStream.Read(IsExchanging);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fID);
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
  fHome := fPlayers.GetHouseByID(cardinal(fHome));
  fInHouse := fPlayers.GetHouseByID(cardinal(fInHouse));
end;


{Returns self and adds on to the pointer counter}
function TKMUnit.GetUnitPointer:TKMUnit;
begin
  inc(fPointerCount);
  Result := Self;
end;


{Decreases the pointer counter}
procedure TKMUnit.ReleaseUnitPointer;
begin
  if fPointerCount < 1 then
    raise ELocError.Create('Unit remove pointer',PrevPosition);
  dec(fPointerCount);
end;


{Erase everything related to unit status to exclude it from being accessed by anything but the old pointers}
procedure TKMUnit.CloseUnit(aRemoveTileUsage:boolean=true);
begin
  //if not KMSamePoint(fCurrPosition,NextPosition) then
  //  Assert(false, 'Not sure where to die?');

  if fHome<>nil then
  begin
    fHome.GetHasOwner := false;
    fPlayers.CleanUpHousePointer(fHome);
  end;

  if aRemoveTileUsage then fTerrain.UnitRem(fNextPosition); //Must happen before we nil NextPosition

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

  if (fGame.fGamePlayInterface <> nil) and (Self = fGame.fGamePlayInterface.ShownUnit) then
    fGame.fGamePlayInterface.ClearShownUnit; //If this unit is being shown then we must clear it otherwise it sometimes crashes
  //MapEd doesn't need this yet
end;


{Call this procedure to properly kill a unit}
//killing a unit is done in 3 steps
// Kill - release all unit-specific tasks
// TTaskDie - perform dying animation
// CloseUnit - erase all unit data and hide it from further access
procedure TKMUnit.KillUnit;
begin
  if fPlayers.Selected = Self then fPlayers.Selected := nil;
  if fGame.fGamePlayInterface.ShownUnit = Self then fGame.fGamePlayInterface.ShowUnitInfo(nil);
  if (fUnitTask is TTaskDie) then exit; //Don't kill unit if it's already dying

  //Wait till units exchange (1 tick) and then do the killing
  if (fCurrentAction is TUnitActionWalkTo) and TUnitActionWalkTo(fCurrentAction).DoingExchange then
  begin
    fKillASAP := true; //Unit will be killed ASAP
    exit;
  end;

  //Update statistics
  if (fPlayers<>nil) and (fOwner <> PLAYER_NONE) and (fOwner <> PLAYER_ANIMAL) then
    fPlayers.Player[fOwner].Stats.UnitLost(fUnitType);

  fThought := th_None; //Reset thought
  SetAction(nil); //Dispose of current action (TTaskDie will set it to LockedStay)
  FreeAndNil(fUnitTask); //Should be overriden to dispose of Task-specific items
  fUnitTask := TTaskDie.Create(Self);
end;


procedure TKMUnit.SetPosition(aPos:TKMPoint);
begin
  Assert(fGame.GameState=gsEditor); //This is only used by the map editor, set all positions to aPos
  fTerrain.UnitRem(fCurrPosition);
  fCurrPosition := aPos;
  fNextPosition := aPos;
  fPrevPosition := aPos;
  fPosition := KMPointF(aPos);
  fTerrain.UnitAdd(fCurrPosition, Self);
end;


function TKMUnit.CanAccessHome:boolean;
begin
  Result := (fHome = nil) or fTerrain.Route_CanBeMade(GetPosition, KMPointBelow(fHome.GetEntrance), canWalk, 0, false);
end;


function TKMUnit.GetUnitTaskText:string;
begin
  Result:='Idle';                                      {----------} //Thats allowed width
  if fUnitTask is TTaskSelfTrain        then Result := 'Train';
  if fUnitTask is TTaskDeliver          then Result := 'Deliver';
  if fUnitTask is TTaskBuildRoad        then Result := 'Build road';
  if fUnitTask is TTaskBuildWine        then Result := 'Build wine field';
  if fUnitTask is TTaskBuildField       then Result := 'Build corn field';
  if fUnitTask is TTaskBuildWall        then Result := 'Build wall';
  if fUnitTask is TTaskBuildHouseArea   then Result := 'Prepare house area';
  if fUnitTask is TTaskBuildHouse       then Result := 'Build house';
  if fUnitTask is TTaskBuildHouseRepair then Result := 'Repair house';
  if fUnitTask is TTaskGoHome           then Result := 'Go home';
  if fUnitTask is TTaskGoEat            then Result := 'Go to eat';
  if fUnitTask is TTaskMining           then Result := 'Mine resources';
  if fUnitTask is TTaskDie              then Result := 'Die';
  if fUnitTask is TTaskAttackHouse      then Result := 'Attack House';
  if fUnitTask is TTaskGoOutShowHungry  then Result := 'Show hunger';
  if fUnitTask is TTaskThrowRock        then Result := 'Throwing rock';
end;


function TKMUnit.GetUnitActText:string;
begin
  Result := fCurrentAction.GetExplanation;
end;


procedure TKMUnit.SetFullCondition;
begin
  fCondition := UNIT_MAX_CONDITION;
end;


//Return TRUE if unit was killed
function TKMUnit.HitPointsDecrease(aAmount:integer; aPenetratesDefence:boolean):boolean;
begin
  Result := false;
  //When we are first hit reset the counter
  if (aAmount > 0) and (fHitPoints = GetMaxHitPoints) then fHitPointCounter := 1;
  // Defence modifier
  if not aPenetratesDefence then
    aAmount := aAmount div Math.max(fResource.UnitDat[fUnitType].Defence, 1); //Not needed, but animals have 0 defence
  // Sign of aAmount does not affect
  fHitPoints := EnsureRange(fHitPoints - abs(aAmount), 0, GetMaxHitPoints);
  if (fHitPoints = 0) and not IsDeadOrDying then begin //Kill only once
    KillUnit;
    Result := true;
  end;
end;


procedure TKMUnit.HitPointsIncrease(aAmount:integer);
begin
  // Sign of aAmount does not affect
  fHitPoints := EnsureRange(fHitPoints + abs(aAmount), 0, GetMaxHitPoints);
end;


function TKMUnit.GetMaxHitPoints:byte;
begin
  Result := EnsureRange(fResource.UnitDat[fUnitType].HitPoints*40,0,255);
  // *40 - [LifePoints/OneHitPoint] (MaxHitPoint in Units.dat - 4) 4*40 = 160
end;


procedure TKMUnit.CancelUnitTask;
begin
  if (fUnitTask <> nil)and(fCurrentAction is TUnitActionWalkTo)and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then
    AbandonWalk;
  FreeAndNil(fUnitTask);
end;


procedure TKMUnit.SetInHouse(aInHouse:TKMHouse);
begin
  fPlayers.CleanUpHousePointer(fInHouse);
  if aInHouse <> nil then
    fInHouse := aInHouse.GetHousePointer;
end;


function TKMUnit.HitTest(X,Y:Integer; const UT:TUnitType = ut_Any): Boolean;
begin
  Result := (X = fCurrPosition.X) and //Comparing X,Y to CurrentPosition separately, cos they  is negative numbers
            (Y = fCurrPosition.Y) and
            ((fUnitType=UT)or(UT=ut_Any));
end;


//As long as we only ever set PrevPos to NextPos and do so everytime before NextPos changes,
//there can be no problems (as were occurring in GetSlide)
//This procedure ensures that these values always get updated correctly so we don't get a problem
//where GetLength(PrevPosition,NextPosition) > sqrt(2)
procedure TKMUnit.UpdateNextPosition(aLoc:TKMPoint);
begin
  fPrevPosition := NextPosition;
  fNextPosition := aLoc;
end;


//Only ClearUnit can set fDirection to NA, no other circumstances it is allowed
procedure TKMUnit.SetDirection(aValue:TKMDirection);
begin
  Assert(aValue<>dir_NA);
  fDirection := aValue;
end;


//Assign the following Action to unit and set AnimStep
procedure TKMUnit.SetAction(aAction: TUnitAction; aStep:integer=0);
begin
  AnimStep := aStep;
  if aAction = nil then
  begin
    FreeAndNil(fCurrentAction);
    exit;
  end;
  if not fResource.UnitDat[fUnitType].SupportsAction(aAction.GetActionType) then
  begin
    Assert(false, 'Unit '+fResource.UnitDat[UnitType].UnitName+' was asked to do unsupported action');
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
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    raise ELocError.Create('Unit fight overrides walk',fCurrPosition);
  SetAction(TUnitActionFight.Create(aAction, aOpponent, Self));
end;


procedure TKMUnit.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse:TKMHouse);
begin
  SetAction(TUnitActionGoInOut.Create(aAction, Self, aGoDir, aHouse));
end;


procedure TKMUnit.SetActionStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);
begin
  //When standing still in walk, use default frame
  if (aAction = ua_Walk)and(aStayStill) then
  begin
    aStillFrame := UnitStillFrames[Direction];
    aStep := UnitStillFrames[Direction];
  end;
  SetAction(TUnitActionStay.Create(aTimeToStay, aAction, aStayStill, aStillFrame, false), aStep);
end;


procedure TKMUnit.SetActionStorm(aRow:integer);
begin
  SetAction(TUnitActionStormAttack.Create(ua_Walk, aRow), 0); //Action is ua_Walk for that is the inital one
end;


//Same as above but we will ignore get-out-of-the-way (push) requests from interaction system
procedure TKMUnit.SetActionLockedStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);
begin
  //When standing still in walk, use default frame
  if (aAction = ua_Walk)and(aStayStill) then
  begin
    aStillFrame := UnitStillFrames[Direction];
    aStep := UnitStillFrames[Direction];
  end;
  SetAction(TUnitActionStay.Create(aTimeToStay, aAction, aStayStill, aStillFrame, true), aStep);
end;


//WalkTo action with exact options (retranslated from WalkTo if Obstcale met)
procedure TKMUnit.SetActionWalk(aLocB:TKMPoint; aActionType:TUnitActionType; aDistance:single; aWalkToNear:boolean; aTargetUnit:TKMUnit; aTargetHouse:TKMHouse);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then Assert(false);
  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, false, aWalkToNear, aTargetUnit, aTargetHouse));
end;


//Approach house
// Route will be made to House.Entrance, but will be Done as soon as we are at required range to any(!) side of the house
procedure TKMUnit.SetActionWalkToHouse(aHouse:TKMHouse; aDistance:single; aActionType:TUnitActionType=ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then Assert(false);

  SetAction(TUnitActionWalkTo.Create( Self,               //Who's walking
                                      //Target position is the closest cell to our current position (only used for estimating in path finding)
                                      aHouse.GetClosestCell(Self.GetPosition),
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      false,              //If we were pushed
                                      false,              //WalkToNear
                                      nil,                //Unit
                                      aHouse              //House
                                      ));
end;


//Approach unit
procedure TKMUnit.SetActionWalkToUnit(aUnit:TKMUnit; aDistance:single; aActionType:TUnitActionType=ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then Assert(false);

  Assert(aDistance>=1,'Should not walk to units place');
  SetAction(TUnitActionWalkTo.Create( Self,               //Who's walking
                                      aUnit.fCurrPosition,  //Target position
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      false,              //If we were pushed
                                      false,              //WalkToNear
                                      aUnit,              //Unit
                                      nil                 //House
                                      ));
end;


//Walk to spot
procedure TKMUnit.SetActionWalkToSpot(aLocB:TKMPoint; aDistance:single=0; aActionType:TUnitActionType=ua_Walk);
begin
  if (GetUnitAction is TUnitActionWalkTo) and not TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
    Assert(false);
  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, false, false, nil, nil));
end;


//We were pushed (walk to spot with wider Passability)
procedure TKMUnit.SetActionWalkPushed(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk);
begin
  //1. Only idle units can be pushed, for they are low priority to busy units
  //2. If unit can't get away it will re-push itself once again
  Assert(((GetUnitAction is TUnitActionStay) and (not GetUnitAction.Locked)) or
         ((GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(GetUnitAction).CanAbandonExternal));

  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, 0, true, false, nil, nil));
  //Once pushed, unit will try to walk away, if he bumps into more units he will
  //
end;


//Walk to spot neighbourhood
procedure TKMUnit.SetActionWalkToNear(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk; aTargetCanBeReached:boolean=true);
begin
  Assert(Self is TKMUnitWarrior, 'True warriors don''t care ''bout exact location on reposition');
  SetAction(TUnitActionWalkTo.Create(Self, aLocB, aActionType, 0, false, true, nil, nil, aTargetCanBeReached));
end;


procedure TKMUnit.SetActionAbandonWalk(aLocB:TKMPoint; aActionType:TUnitActionType=ua_Walk);
var TempVertexOccupied: TKMPoint;
begin
  if GetUnitAction is TUnitActionWalkTo then
  begin
    TempVertexOccupied := TUnitActionWalkTo(GetUnitAction).fVertexOccupied;
    TUnitActionWalkTo(GetUnitAction).fVertexOccupied := KMPoint(0,0); //So it doesn't try to DecVertex on destroy (now it's AbandonWalk's responsibility)
  end
  else TempVertexOccupied := KMPoint(0,0);
  SetAction(TUnitActionAbandonWalk.Create(aLocB,TempVertexOccupied, aActionType),AnimStep); //Use the current animation step, to ensure smooth transition
end;


procedure TKMUnit.AbandonWalk;
begin
  if GetUnitAction is TUnitActionWalkTo then
    SetActionAbandonWalk(NextPosition, ua_Walk)
  else
    SetActionLockedStay(0, ua_Walk); //Error
end;


//Specific unit desired passability may depend on several factors
function TKMUnit.GetDesiredPassability(aIgnoreRoads:boolean=false):TPassability;
begin
  Result := fResource.UnitDat[fUnitType].DesiredPassability;

  //Delivery to unit
  if (fUnitType = ut_Serf)
  and (fUnitTask is TTaskDeliver)
  and (TTaskDeliver(fUnitTask).DeliverKind = dk_ToUnit)
  then
    Result := CanWalk;

  //Preparing house area
  if (fUnitType = ut_Worker)
  and(((fUnitTask is TTaskBuildHouseArea)
  and(TTaskBuildHouseArea(fUnitTask).fPhase > 1)) //Worker has arrived on site
  or (mu_HouseFenceNoWalk = fTerrain.Land[fCurrPosition.Y,fCurrPosition.X].Markup)) //If we are standing on site after building
  then
    Result := CanWorker; //Special mode that allows us to walk on building sites

  //Thats for 'miners' at work
  if (fUnitType in [ut_Woodcutter, ut_Farmer, ut_Fisher, ut_StoneCutter])
  and(fUnitTask is TTaskMining)
  then
    Result := CanWalk;

  //aIgnoreRoads means we are desperate enough to ignore roads (e.g. when looking for an Inn)
  if aIgnoreRoads then
    Result := CanWalk;
end;


procedure TKMUnit.Feed(Amount:single);
begin
  fCondition := Math.min(fCondition + round(Amount), UNIT_MAX_CONDITION);
end;


//It's better not to start doing anything with dying units
function TKMUnit.IsDeadOrDying:boolean;
begin
  Result := fIsDead or (fUnitTask is TTaskDie) or fKillASAP;
end;


{Check wherever this unit is armed}
function TKMUnit.IsArmyUnit:boolean;
begin
  Result := fUnitType in [ut_Militia .. ut_Barbarian];
end;


function TKMUnit.CanGoEat:boolean;
begin
  Result := fPlayers.Player[fOwner].FindInn(fCurrPosition,Self) <> nil;
end;


procedure TKMUnit.UpdateHunger;
begin
  if fCondition>0 then //Make unit hungry as long as they are not currently eating in the inn
    if not((fUnitTask is TTaskGoEat) and (TTaskGoEat(fUnitTask).Eating)) then
      dec(fCondition);

  //Feed the unit automatically. Don't align it with dec(fCondition) cos FOW uses it as a timer
  if (not DO_UNIT_HUNGER)and(fCondition<UNIT_MIN_CONDITION+100) then fCondition := UNIT_MAX_CONDITION;

  //Unit killing could be postponed by few ticks, hence fCondition could be <0
  if fCondition <= 0 then
    KillUnit;
end;


//Can use fCondition as a sort of counter to reveal terrain X times a sec
procedure TKMUnit.UpdateFOW;
begin
  if fCondition mod 10 = 0 then
    fPlayers.RevealForTeam(fOwner, fCurrPosition, fResource.UnitDat[fUnitType].Sight, FOG_OF_WAR_INC);
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
function TKMUnit.UpdateVisibility:boolean;
begin
  Result := false;
  if fInHouse = nil then exit; //There's nothing to update, we are always visible

  if fInHouse.IsDestroyed then //Someone has destroyed the house we were in
  begin
    fVisible := true;
    //If we are walking into/out of the house then don't set our position, ActionGoInOut will sort it out
    if (not (GetUnitAction is TUnitActionGoInOut)) or (not TUnitActionGoInOut(GetUnitAction).GetHasStarted) then
    begin
      //Position in a spiral nearest to entrance of house, updating IsUnit.
      if not fPlayers.FindPlaceForUnit(fInHouse.GetEntrance.X, fInHouse.GetEntrance.Y, UnitType, fCurrPosition) then
      begin
        //There is no space for this unit so it must be destroyed
        if (fPlayers<>nil) and (fOwner <> PLAYER_NONE) and (fOwner <> PLAYER_ANIMAL) then
          fPlayers.Player[fOwner].Stats.UnitLost(fUnitType);
        FreeAndNil(fCurrentAction);
        FreeAndNil(fUnitTask);
        CloseUnit(false); //Close the unit without removing tile usage (because this unit was in a house it has none)
        Result := true;
        exit;
      end;
      //Make sure these are reset properly
      assert(not fTerrain.HasUnit(fCurrPosition));
      IsExchanging := false;
      fPosition := KMPointF(fCurrPosition);
      fPrevPosition := fCurrPosition;
      fNextPosition := fCurrPosition;
      fTerrain.UnitAdd(fCurrPosition, Self); //Unit was not occupying tile while inside the house, hence just add do not remove
      if GetUnitAction is TUnitActionGoInOut then SetActionLockedStay(0,ua_Walk); //Abandon the walk out in this case
    end;
    SetInHouse(nil); //Can't be in a destroyed house
  end;
end;


procedure TKMUnit.UpdateHitPoints;
begin
  //Use fHitPointCounter as a counter to restore hit points every X ticks
  if (GetUnitAction is TUnitActionFight) and not fGame.GlobalSettings.fHitPointRestoreInFights then exit;
  if fGame.GlobalSettings.fHitPointRestorePace = 0 then exit; //0 pace means don't restore
  if fHitPointCounter mod fGame.GlobalSettings.fHitPointRestorePace = 0 then
    HitPointsIncrease(Round(0.3*GetMaxHitPoints)); //Add 30% of MaxHitPoints
  inc(fHitPointCounter);
end;


function TKMUnit.GetSlide(aCheck:TCheckAxis): single;
var DY,DX, PixelPos, LookupDiagonal: shortint;
begin
  Result := 0;
  if (not IsExchanging) or not (GetUnitAction.fActionName in [uan_WalkTo,uan_GoInOut]) then exit;

  //Uses Y because a walk in the Y means a slide in the X
  DX := sign(NextPosition.X - fPosition.X);
  DY := sign(NextPosition.Y - fPosition.Y);
  if (aCheck = ax_X) and (DY = 0) then exit; //Unit is not shifted
  if (aCheck = ax_Y) and (DX = 0) then exit;

  LookupDiagonal := abs(DX) + abs(DY); //which gives us swith: 1-straight, 2-diagonal.

  if aCheck = ax_X then begin
    PixelPos := Round(abs(fPosition.Y-PrevPosition.Y)*CELL_SIZE_PX*sqrt(LookupDiagonal)); //Diagonal movement *sqrt(2)
    Result := (DY*SlideLookup[LookupDiagonal,PixelPos])/CELL_SIZE_PX;
  end;
  if aCheck = ax_Y then begin
    PixelPos := Round(abs(fPosition.X-PrevPosition.X)*CELL_SIZE_PX*sqrt(LookupDiagonal)); //Diagonal movement *sqrt(2)
    Result := -(DX*SlideLookup[LookupDiagonal,PixelPos])/CELL_SIZE_PX;
  end;
end;


function TKMUnit.GetMovementVector: TKMPointF;
var MovementSpeed:single;
begin
  if (GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(GetUnitAction).DoesWalking then
    MovementSpeed := fResource.UnitDat[fUnitType].Speed
  else
  if (GetUnitAction is TUnitActionStormAttack) then
    MovementSpeed := fResource.UnitDat[fUnitType].Speed * STORM_SPEEDUP
  else
    MovementSpeed := 0;

  Result.X := KMGetVertex(fDirection).X * MovementSpeed;
  Result.Y := KMGetVertex(fDirection).Y * MovementSpeed;
end;


procedure TKMUnit.Save(SaveStream:TKMemoryStream);
var HasTask,HasAct:boolean;
begin
  SaveStream.Write(fUnitType, SizeOf(fUnitType));

  HasTask := fUnitTask <> nil; //Thats our switch to know if unit should write down his task.
  SaveStream.Write(HasTask);
  if HasTask then fUnitTask.Save(SaveStream); //TaskType gets written first in fUnitTask.Save

  HasAct := fCurrentAction <> nil;
  SaveStream.Write(HasAct);
  if HasAct then fCurrentAction.Save(SaveStream);

  SaveStream.Write(fThought, SizeOf(fThought));
  SaveStream.Write(fCondition);
  SaveStream.Write(fHitPoints);
  SaveStream.Write(fHitPointCounter);

  if fInHouse <> nil then
    SaveStream.Write(fInHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(fOwner, SizeOf(fOwner));

  if fHome <> nil then
    SaveStream.Write(fHome.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(fPosition);
  SaveStream.Write(fVisible);
  SaveStream.Write(fIsDead);
  SaveStream.Write(fKillASAP);
  SaveStream.Write(IsExchanging);
  SaveStream.Write(fPointerCount);

  SaveStream.Write(fID);
  SaveStream.Write(AnimStep);
  SaveStream.Write(fDirection);
  SaveStream.Write(fCurrPosition);
  SaveStream.Write(fPrevPosition);
  SaveStream.Write(fNextPosition);
end;


{Here are common Unit.UpdateState routines}
function TKMUnit.UpdateState:boolean;
begin
  //There are layers of unit activity (bottom to top):
  // - Action (Atom creating layer (walk 1frame, etc..))
  // - Task (Action creating layer)
  // - specific UpdateState (Task creating layer)

  Result := true;

  //UpdateState can happen right after unit gets killed (Exchange still in progress)
  if fKillASAP and not ((fCurrentAction is TUnitActionWalkTo) and TUnitActionWalkTo(fCurrentAction).DoingExchange) then begin
    KillUnit;
    fKillASAP := false;
    assert(IsDeadOrDying); //Just in case KillUnit failed
  end;

  UpdateHunger;
  UpdateFOW;
  UpdateThoughts;
  UpdateHitPoints;
  if UpdateVisibility then exit; //incase units home was destroyed. Returns true if the unit was killed due to lack of space

  //Shortcut to freeze unit in place if it's on an unwalkable tile. We use fNextPosition rather than fCurrPosition
  //because once we have taken a step from a tile we no longer care about it. (fNextPosition matches up with IsUnit in terrain)
  if fCurrentAction is TUnitActionWalkTo then
    if GetDesiredPassability = CanWalkRoad then
    begin
      if not fTerrain.CheckPassability(fNextPosition, CanWalk) then
        raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' on unwalkable tile at '+KM_Points.TypeToString(fNextPosition)+' pass canWalk',fNextPosition);
    end else
    if not fTerrain.CheckPassability(fNextPosition, GetDesiredPassability) then
      raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' on unwalkable tile at '+KM_Points.TypeToString(fNextPosition)+' pass '+PassabilityStr[GetDesiredPassability],fNextPosition);


  //
  //Performing Tasks and Actions now
  //------------------------------------------------------------------------------------------------
  if fCurrentAction=nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action in TKMUnit.UpdateState',fCurrPosition);

  fCurrPosition := KMPointRound(fPosition);
  case fCurrentAction.Execute(Self) of
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
  if fCurrentAction=nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action!',fCurrPosition);

  //Here should be catched any cases where unit has no current action - this is a flaw in TTasks somewhere
  //Unit always meant to have some Action performed.

  fCurrentAction.Paint;

  if SHOW_POINTER_DOTS then
    fRenderAux.UnitPointers(fPosition.X + 0.5 + GetSlide(ax_X), fPosition.Y + 1   + GetSlide(ax_Y), GetPointerCount);
end;


{ TUnitTask }
constructor TUnitTask.Create(aUnit:TKMUnit);
begin
  Inherited Create;
  fTaskName := utn_Unknown;
  Assert(aUnit <> nil);
  fUnit := aUnit.GetUnitPointer;
  fUnit.SetActionLockedStay(0,ua_Walk);
  fPhase    := 0;
  fPhase2   := 0;
end;


constructor TUnitTask.Load(LoadStream:TKMemoryStream);
begin
  Inherited Create;
  LoadStream.Read(fTaskName, SizeOf(fTaskName));
  LoadStream.Read(fUnit, 4);//Substitute it with reference on SyncLoad
  LoadStream.Read(fPhase);
  LoadStream.Read(fPhase2);
end;


procedure TUnitTask.SyncLoad;
begin
  fUnit := fPlayers.GetUnitByID(cardinal(fUnit));
end;


destructor TUnitTask.Destroy;
begin
  fUnit.Thought := th_None; //Stop any thoughts
  fPlayers.CleanUpUnitPointer(fUnit);
  fPhase        := high(byte)-1; //-1 so that if it is increased on the next run it won't overrun before exiting
  fPhase2       := high(byte)-1;
  Inherited;
end;


function TUnitTask.WalkShouldAbandon:boolean;
begin
  Result := false; //Only used in some child classes
end;


procedure TUnitTask.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(fTaskName, SizeOf(fTaskName)); //Save task type before anything else for it will be used on loading to create specific task type
  if fUnit <> nil then
    SaveStream.Write(fUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fPhase);
  SaveStream.Write(fPhase2);
end;


{ TUnitAction }
constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionName := uan_Unknown;
  fActionType := aActionType;
  StepDone    := false;
end;


constructor TUnitAction.Load(LoadStream:TKMemoryStream);
begin
  Inherited Create;
  LoadStream.Read(fActionName, SizeOf(fActionName));
  LoadStream.Read(fActionType, SizeOf(fActionType));
  LoadStream.Read(Locked);
  LoadStream.Read(StepDone);
end;


procedure TUnitAction.SyncLoad;
begin
  //Should be virtual
end;


procedure TUnitAction.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(fActionName, SizeOf(fActionName));
  SaveStream.Write(fActionType, SizeOf(fActionType));
  SaveStream.Write(Locked);
  SaveStream.Write(StepDone);
end;


procedure TUnitAction.Paint;
begin
  //Used for debug, paint action properties here
end;


{ TKMUnitsCollection }
destructor TKMUnitsCollection.Destroy;
begin
  //No need to free units individually since they are Freed by TKMList.Clear command.
  Inherited;
end;


function TKMUnitsCollection.GetUnit(Index: Integer): TKMUnit;
begin
  Result := TKMUnit(Items[Index]);
end;


procedure TKMUnitsCollection.SetUnit(Index: Integer; Item: TKMUnit);
begin
  Items[Index] := Item;
end;


{ AutoPlace means should we find a spot for this unit or just place it where we are told.
  Used for creating units still inside schools }
function TKMUnitsCollection.Add(aOwner: shortint; aUnitType: TUnitType; PosX, PosY:integer; AutoPlace:boolean=true):TKMUnit;
var U:Integer; P:TKMPoint;
begin
  P := KMPoint(0,0); // Will have 0:0 if no place found
  if AutoPlace then begin
    fPlayers.FindPlaceForUnit(PosX,PosY,aUnitType, P);
    PosX := P.X;
    PosY := P.Y;
  end;

  if fTerrain.HasUnit(KMPoint(PosX,PosY)) then
    raise ELocError.Create('No space for '+fResource.UnitDat[aUnitType].UnitName+', tile occupied by '+fResource.UnitDat[fTerrain.Land[PosY,PosX].IsUnit.UnitType].UnitName,KMPoint(PosX,PosY));

  if not fTerrain.TileInMapCoords(PosX, PosY) then begin
    fLog.AppendLog('Unable to add unit to '+KM_Points.TypeToString(KMPoint(PosX,PosY)));
    Result := nil;
    exit;
  end;

  case aUnitType of
    ut_Serf:    U := Inherited Add(TKMUnitSerf.Create(aOwner,PosX,PosY,aUnitType));
    ut_Worker:  U := Inherited Add(TKMUnitWorker.Create(aOwner,PosX,PosY,aUnitType));

    ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                U := Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_Recruit: U := Inherited Add(TKMUnitRecruit.Create(aOwner,PosX,PosY,aUnitType));

    ut_Militia..ut_Barbarian:   U := Inherited Add(TKMUnitWarrior.Create(aOwner,PosX,PosY,aUnitType));

    ut_Wolf..ut_Duck:           U := Inherited Add(TKMUnitAnimal.Create(aOwner,PosX,PosY,aUnitType));

    else                        raise ELocError.Create('Add '+fResource.UnitDat[aUnitType].UnitName,KMPoint(PosX, PosY));
  end;

  Result := Units[U];
end;


function TKMUnitsCollection.AddGroup(aOwner:TPlayerIndex;  aUnitType:TUnitType; PosX, PosY:integer; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
var U:TKMUnit; Commander,W:TKMUnitWarrior; i:integer; UnitPosition:TKMPoint; DoesFit: boolean;
begin
  Assert(aDir <> dir_NA);
  aUnitPerRow := Math.min(aUnitPerRow,aUnitCount); //Can have more rows than units
  if not (aUnitType in [ut_Militia .. ut_Barbarian]) then
  begin
    for i:=1 to aUnitCount do
    begin
      UnitPosition := GetPositionInGroup2(PosX, PosY, aDir, i, aUnitPerRow, fTerrain.MapX, fTerrain.MapY, DoesFit);
      U := Add(aOwner, aUnitType, UnitPosition.X, UnitPosition.Y); //U will be _nil_ if unit didn't fit on map
      if U<>nil then
      begin
        fPlayers.Player[aOwner].Stats.UnitCreated(aUnitType, false);
        U.Direction := aDir;
        U.AnimStep  := UnitStillFrames[aDir];
      end;
    end;
    Result := nil; //Dunno what to return here
    exit; // Don't do anything else for citizens
  end;

  //Add commander first
  Commander := TKMUnitWarrior(Add(aOwner, aUnitType, PosX, PosY));
  Result := Commander;

  if Commander=nil then exit; //Don't add group without a commander
  fPlayers.Player[aOwner].Stats.UnitCreated(aUnitType, false);

  Commander.Direction := aDir;
  Commander.AnimStep  := UnitStillFrames[aDir];
  Commander.OrderLocDir := KMPointDir(Commander.OrderLocDir.Loc, aDir); //So when they click Halt for the first time it knows where to place them

  //In MapEditor we need only fMapEdMembersCount property, without actual members
  if aMapEditor then begin
    Commander.fMapEdMembersCount := aUnitCount-1; //Skip commander
    Commander.UnitsPerRow := aUnitPerRow; //Must be set at the end AFTER adding members
    exit;
  end;

  for i:=2 to aUnitCount do begin //Commander already placed
    UnitPosition := GetPositionInGroup2(PosX, PosY, aDir, i, aUnitPerRow, fTerrain.MapX, fTerrain.MapY, DoesFit);
    W := TKMUnitWarrior(Add(aOwner, aUnitType, UnitPosition.X, UnitPosition.Y)); //W will be _nil_ if unit didn't fit on map
    if W<>nil then
    begin
      fPlayers.Player[aOwner].Stats.UnitCreated(aUnitType, false);
      W.Direction := aDir;
      W.AnimStep  := UnitStillFrames[aDir];
      Commander.AddMember(W);
      W.fCondition := Commander.fCondition; //Whole group will have same condition
    end;
  end;
  Commander.UnitsPerRow := aUnitPerRow; //Must be set at the end AFTER adding members
end;


procedure TKMUnitsCollection.RemoveUnit(aUnit:TKMUnit);
begin
  aUnit.CloseUnit; //Should free up the unit properly (freeing terrain usage and memory)
  aUnit.Free;
  Remove(aUnit);
end;


procedure TKMUnitsCollection.OwnerUpdate(aOwner:TPlayerIndex);
var i:integer;
begin
  for i:=0 to Count-1 do
    Units[i].fOwner := aOwner;
end;


function TKMUnitsCollection.HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
var i:integer;
begin
  Result:= nil;
  for i:=0 to Count-1 do
    if Units[i].HitTest(X,Y,UT) and (not Units[i].IsDead) then
    begin
      Result := Units[i];
      exit;
    end;
end;


function TKMUnitsCollection.GetUnitByID(aID: Integer): TKMUnit;
var i:integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if aID = Units[i].ID then
    begin
      Result := Units[i];
      exit;
    end;
end;


procedure TKMUnitsCollection.GetLocations(var Loc:TKMPointList; aUnitType:TUnitType=ut_Any);
var i:integer;
begin
  Loc.Clearup;
  for i:=0 to Count-1 do
    if aUnitType in [ut_Any, Units[i].fUnitType] then
    if Units[i].fVisible then //Excludes units inside of houses and recently died ones
      Loc.AddEntry(Units[i].fCurrPosition)
end;


function TKMUnitsCollection.GetClosestUnit(aPoint: TKMPoint):TKMUnit;
var
  i: integer;
  BestDist,Dist: single;
begin
  Result := nil;
  BestDist := MaxSingle; //Any distance will be closer than that
  for i:=0 to Count-1 do
    if (not Units[i].IsDeadOrDying) and (Units[i].fVisible) then
    begin
      Dist := GetLength(Units[i].GetPosition, aPoint);
      if Dist < BestDist then
      begin
        BestDist := Dist;
        Result := Units[i];
      end;
    end;
end;


function TKMUnitsCollection.GetTotalPointers: integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to Count-1 do
    inc(Result, Units[i].GetPointerCount);
end;


procedure TKMUnitsCollection.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('Units');
  SaveStream.Write(Count);
  for i := 0 to Count - 1 do
    Units[i].Save(SaveStream);
end;


procedure TKMUnitsCollection.Load(LoadStream:TKMemoryStream);
var i,UnitCount:integer; s:string; UnitType:TUnitType;
begin
  LoadStream.Read(s);
  Assert(s = 'Units');
  LoadStream.Read(UnitCount);
  for i := 0 to UnitCount - 1 do
  begin
    LoadStream.Read(UnitType, SizeOf(UnitType));
    LoadStream.Seek(-SizeOf(UnitType), soFromCurrent); //rewind
    case UnitType of
      ut_Serf:                  Inherited Add(TKMUnitSerf.Load(LoadStream));
      ut_Worker:                Inherited Add(TKMUnitWorker.Load(LoadStream));
      ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                                Inherited Add(TKMUnitCitizen.Load(LoadStream));
      ut_Recruit:               Inherited Add(TKMUnitRecruit.Load(LoadStream));
      ut_Militia..ut_Barbarian: Inherited Add(TKMUnitWarrior.Load(LoadStream));
      //ut_Bowman:   Inherited Add(TKMUnitArcher.Load(LoadStream)); //I guess it will be stand-alone
      ut_Wolf..ut_Duck:         Inherited Add(TKMUnitAnimal.Load(LoadStream));
      else fLog.AssertToLog(false, 'Unknown unit type in Savegame')
    end;
  end;
end;


procedure TKMUnitsCollection.SyncLoad;
var i:integer;
begin
  for i:=0 to Count-1 do
  begin
    case Units[i].fUnitType of
      ut_Serf:                  TKMUnitSerf(Items[i]).SyncLoad;
      ut_Worker:                TKMUnitWorker(Items[i]).SyncLoad;
      ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                                TKMUnitCitizen(Items[i]).SyncLoad;
      ut_Recruit:               TKMUnitRecruit(Items[i]).SyncLoad;
      ut_Militia..ut_Barbarian: TKMUnitWarrior(Items[i]).SyncLoad;
      //ut_Bowman:              TKMUnitArcher(Items[i]).SyncLoad; //I guess it will be stand-alone
      ut_Wolf..ut_Duck:         TKMUnitAnimal(Items[i]).SyncLoad;
    end;
  end;
end;


procedure TKMUnitsCollection.UpdateState;
var
  i, ID: integer;
  IDsToDelete: array of integer;
begin
  ID := 0;
  for i:=0 to Count-1 do
  if not Units[i].IsDead then
    Units[i].UpdateState
  else //Else try to destroy the unit object if all pointers are freed
    if (Items[i] <> nil) and FREE_POINTERS and (Units[i].fPointerCount = 0) then
    begin
      SetLength(IDsToDelete,ID+1);
      IDsToDelete[ID] := i;
      inc(ID);
    end;

  //Must remove list entry after for loop is complete otherwise the indexes change
  for i:=ID-1 downto 0 do
  begin
    Units[IDsToDelete[i]].Free; //Because no one needs this anymore it must DIE!!!!! :D
    Units[IDsToDelete[i]] := nil;
    Delete(IDsToDelete[i]);
  end;

  //   --     POINTER FREEING SYSTEM - DESCRIPTION     --   //
  //  This system was implemented because unit and house objects cannot be freed until all pointers
  //  to them (in tasks, delivery queue, etc.) have been freed, otherwise we have pointer integrity
  //  issues.

  //   --     ROUGH OUTLINE     --   //
  // - Units and houses have fPointerCount, which is the number of pointers to them. (e.g. tasks,
  //   deliveries) This is kept up to date by the thing that is using the pointer. On create it uses
  //   GetUnitPointer to get the pointer and increase the pointer count and on destroy it decreases
  //   it with ReleaseUnitPointer.
  // - When a unit dies, the object is not destroyed. Instead a flag (boolean) is set to say that we
  //   want to destroy but can't because there still might be pointers to the unit. From then on
  //   every update state it checks to see if the pointer count is 0 yet. If it is then the unit is
  //   destroyed.
  // - For each place that contains a pointer, it should check everytime the pointer is used to see
  //   if it has been destroy. If it has then we free the pointer and reduce the count.
  //   (and do any other action nececary due to the unit/house dying)

end;


procedure TKMUnitsCollection.Paint;
var i:integer; x1,x2,y1,y2:smallint; Margin:shortint;
begin
  if TEST_VIEW_CLIP_INSET then Margin:=-1 else Margin:=1;
  x1 := fViewport.GetClip.Left - Margin;
  x2 := fViewport.GetClip.Right + Margin;
  y1 := fViewport.GetClip.Top - Margin;
  y2 := fViewport.GetClip.Bottom + Margin*3; //There might be units standing on tall hills below

  for i:=0 to Count-1 do
  if (Items[i] <> nil) and (not Units[i].IsDead) then
  if (InRange(Units[i].fPosition.X,x1,x2) and InRange(Units[i].fPosition.Y,y1,y2)) then
    Units[i].Paint;
end;


end.
