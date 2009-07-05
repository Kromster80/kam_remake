unit KM_Units;
interface
uses
  KM_Defaults, windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Houses, KromUtils, SysUtils, MMSystem, KM_Units_WorkPlan;

type    
  TKMUnit = class;
  TKMUnitSerf = class;
  TKMUnitWorker = class;

  TUnitAction = class(TObject)
  private
    fActionType: TUnitActionType;
    IsStepDone: boolean; //True when single action element is done (unit walked to new tile, single attack loop done)
  public
    constructor Create(aActionType: TUnitActionType);
    procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); virtual; abstract;
    property ActionType: TUnitActionType read fActionType;
  end;

      {Walk to somewhere}
      TUnitActionWalkTo = class(TUnitAction)
      private
        fWalker:TKMUnit;
        fWalkFrom,fWalkTo:TKMPoint;
        fRouteBuilt:boolean;
        fWalkToSpot:boolean;
        fPass:TPassability; //Desired passability set once on Create
        fIgnorePass:boolean;
        NodeCount:word; //Should be positive
        Nodes:array[1..TEST_MAX_WALK_PATH] of TKMPoint; //In fact it's much shorter array
        NodePos:integer;
        DoesWalking:boolean;
        DoEvade:boolean; //Command to make exchange maneuver with other unit
        Explanation:string; //Debug only, explanation what unit is doing
      public
        constructor Create(KMUnit: TKMUnit; LocB,Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true; const aIgnorePass:boolean=false);
        function ChoosePassability(KMUnit: TKMUnit; DoIgnorePass:boolean):TPassability;
        function DoUnitInteraction():boolean;
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      {This is a simple action making unit go inside/outside of house}
      TUnitActionGoIn = class(TUnitAction)
      private
        fStep:single;
        fDir:TGoInDirection;
        fHouseType:THouseType;
        fStartX:single;
        fHasStarted:boolean;
      public
        constructor Create(aAction: TUnitActionType; aDirection:TGoInDirection; aHouseType:THouseType=ht_None);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      {Stay in place for set time}
      TUnitActionStay = class(TUnitAction)
      private
        StayStill:boolean;
        TimeToStay:integer;
        StillFrame:byte;
        ActionType:TUnitActionType;
      public
        constructor Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0);
        function HowLongLeftToStay():integer;
        procedure MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

  TUnitTask = class(TObject)
  private
    Phase:byte;
    Phase2:byte;
  public
    procedure Execute(out TaskDone:boolean); virtual; abstract;
  end;

    TTaskSelfTrain = class(TUnitTask)
    private
      fUnit:TKMUnit;
      fSchool:TKMHouseSchool;
    public
      constructor Create(aUnit:TKMUnit; aSchool:TKMHouseSchool);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskDeliver = class(TUnitTask)
    private
      fSerf:TKMUnitSerf;
      fFrom:TKMHouse;
      fToHouse:TKMHouse;
      fToUnit:TKMUnit;
      fResourceType:TResourceType;
      ID:integer;
    public
      constructor Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
      procedure AbandonDelivery();
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildRoad = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fLoc:TKMPoint;
      ID:integer;
    public
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildWine = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fLoc:TKMPoint;
      ID:integer;
    public
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildField = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fLoc:TKMPoint;
      ID:integer;
    public
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildHouseArea = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fHouse:TKMHouse;
      TaskID:integer;
      Step:byte;
      ListOfCells:array[1..4*4]of TKMPoint;
    public
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildHouse = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fHouse:TKMHouse;
      TaskID:integer;
      LocCount:byte; //Count for locations around the house from where worker can build
      CurLoc:byte; //Current WIP location
      Cells:array[1..4*4]of record
        Loc:TKMPoint; //List of surrounding cells and directions
        Dir:TKMDirection;
      end;
    public
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildHouseRepair = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fHouse:TKMHouse;
      TaskID:integer;
      LocCount:byte; //Count for locations around the house from where worker can build
      CurLoc:byte; //Current WIP location
      Cells:array[1..4*4]of record
        Loc:TKMPoint; //List of surrounding cells and directions
        Dir:TKMDirection;
      end;
    public
      constructor Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskGoHome = class(TUnitTask)
    private
      fUnit:TKMUnit;
      fDestPos:TKMPoint;
    public
      constructor Create(aTo:TKMPoint; aUnit:TKMUnit);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskGoEat = class(TUnitTask)
    private
      fUnit:TKMUnit;
      fInn:TKMHouseInn;
      PlaceID:byte; //Units place in Inn
    public
      constructor Create(aInn:TKMHouseInn; aUnit:TKMUnit);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskMining = class(TUnitTask)
    private
      WorkPlan:TUnitWorkPlan;
      fUnit:TKMUnit;
      BeastID:byte;
    public
      constructor Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit; aHouse:TKMHouse);
      procedure Execute(out TaskDone:boolean); override;
    end;

    {Yep, this is a Task}
    TTaskDie = class(TUnitTask)
    private
      fUnit:TKMUnit;
    public
      constructor Create(aUnit:TKMUnit);
      procedure Execute(out TaskDone:boolean); override;
    end;

  TKMUnit = class(TObject)
  private
    fUnitType: TUnitType;
    fUnitTask: TUnitTask;
    fCurrentAction: TUnitAction;
    fThought:TUnitThought;
    fCondition:integer; //Unit condition, when it reaches zero unit should die
    Speed:single;
    fOwner:TPlayerID;
    fHome:TKMHouse;
    fPosition: TKMPointF;
    PrevPosition: TKMPoint;
    NextPosition: TKMPoint; //Thats where unit is going to. Next tile in route or same tile if stay on place
    fLastUpdateTime: Cardinal;
    AnimStep: integer;
    fVisible:boolean;
    //function UnitAtHome():boolean; Test if Unit is invisible and Pos matches fHome.GetEntrance
    //Whenever we need to remove the unit within UpdateState routine, but we can't cos it will affect
    //UpdateState cycle. So we need to finish the cycle and only then remove the unit. Property is public
    ScheduleForRemoval:boolean;
  public
    Direction: TKMDirection;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    destructor Destroy; override;
    procedure KillUnit;
    function GetSupportedActions: TUnitActionTypeSet; virtual;
    property UnitAction: TUnitAction read fCurrentAction;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
    procedure SetAction(aAction: TUnitAction; aStep:integer);
    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouseType:THouseType=ht_None);
    procedure SetActionStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);
    procedure SetActionWalk(aKMUnit: TKMUnit; aLocB,aAvoid:TKMPoint; aActionType:TUnitActionType=ua_Walk; aWalkToSpot:boolean=true; aIgnorePass:boolean=false);
    procedure Feed(Amount:single);
    property GetOwner:TPlayerID read fOwner;
    property GetHome:TKMHouse read fHome;
    property SetUnitTask: TUnitTask write fUnitTask;
    property GetUnitType: TUnitType read fUnitType;
    function GetUnitTaskText():string;
    function GetUnitActText():string;
    property GetCondition: integer read fCondition;
    property IsVisible: boolean read fVisible;
    property IsDestroyed:boolean read ScheduleForRemoval;
    procedure RemoveUntrainedFromSchool();
    //property IsAtHome: boolean read UnitAtHome;
    function GetPosition():TKMPoint;
    function UpdateState():boolean; virtual;
    procedure Paint; virtual;
  end;

  //This is a common class for units going out of their homes for resources
  TKMUnitCitizen = class(TKMUnit)
  public
    WorkPlan:TUnitWorkPlan;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    destructor Destroy; override;
    function FindHome():boolean;
    function UpdateState():boolean; override;
    function InitiateMining():TUnitTask;
    procedure Paint(); override;
  end;

  //Serf class - transports all goods ingame between houses
  TKMUnitSerf = class(TKMUnit)
    Carry: TResourceType;
  public
    function UpdateState():boolean; override;
    procedure Paint(); override;
    function GiveResource(Res:TResourceType):boolean;
    function TakeResource(Res:TResourceType):boolean;
    function GetActionFromQueue():TUnitTask;
  end;

  //Worker class - builds everything ingame
  TKMUnitWorker = class(TKMUnit)
  public
    function UpdateState():boolean; override;
    procedure Paint(); override;
    function GetActionFromQueue():TUnitTask;
  end;

  //Possibly melee warrior class? with Archer class separate?
  TKMUnitWarrior = class(TKMUnit)
    fIsCommander:boolean; //Wherever the unit is a leader of a group and has a shtandart
    fCommanderID:TKMUnit; //ID of commander unit
    fFlagAnim:cardinal;
    fOrder:TWarriorOrder;
    fOrderLoc:TKMPoint;
  public
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPoint);
    function UpdateState():boolean; override;
    procedure Paint(); override;
  end;

  //Animals
  TKMUnitAnimal = class(TKMUnit)
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    function UpdateState():boolean; override;
    procedure Paint(); override;
  end;


  TKMUnitsCollection = class(TKMList) //List of TKMUnits
  private
    //Groups:array of integer;
  public
    function Add(aOwner:TPlayerID;  aUnitType:TUnitType; PosX, PosY:integer; AutoPlace:boolean=true):TKMUnit;
    function AddGroup(aOwner:TPlayerID;  aUnitType:TUnitType; PosX, PosY:integer; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
    procedure Rem(aUnit:TKMUnit);
    procedure UpdateState;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
    procedure GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
    procedure Paint();
  end;

implementation
uses KM_Unit1, KM_Render, KM_DeliverQueue, KM_Users, KM_LoadSFX, KM_Viewport, KM_Game;


{ TKMUnitCitizen }
constructor TKMUnitCitizen.Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  WorkPlan:=TUnitWorkPlan.Create;
end;


destructor TKMUnitCitizen.Destroy;
begin
  FreeAndNil(WorkPlan);
  Inherited;
end;


function TKMUnitCitizen.FindHome():boolean;
var KMHouse:TKMHouse;
begin
  Result:=false;
  KMHouse:=fPlayers.Player[byte(fOwner)].FindEmptyHouse(fUnitType,GetPosition);
  if KMHouse<>nil then begin
    fHome:=KMHouse;
    Result:=true;
  end;
end;


procedure TKMUnitCitizen.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
inherited;
if not fVisible then exit;
UnitType:=byte(fUnitType);
AnimAct:=byte(fCurrentAction.fActionType);
AnimDir:=byte(Direction);

  if MakeShowUnitRoutes then
  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderDebugUnitRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,
                            TUnitActionWalkTo(fCurrentAction).Nodes,
                            TUnitActionWalkTo(fCurrentAction).NodePos,
                            $FF00FFFF);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
    if ua_WalkArm in UnitSupportedActions[byte(UnitType)] then
    fRender.RenderUnit(UnitType,       9, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,false);
  end;
ua_Work..ua_Eat:
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
ua_WalkArm .. ua_WalkBooty2:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,false);
  end;
end;

  if fThought<>th_None then
  fRender.RenderUnitThought(fThought, AnimStep, fPosition.X+0.5, fPosition.Y - 0.5);
end;


function TKMUnitCitizen.UpdateState():boolean;
var H:TKMHouseInn; RestTime: integer;
begin
  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  fThought:=th_None;

{  if fUnitTask=nil then //Which is always nil if 'Inherited UpdateState' works properly
  if not TestHunger then
  if not TestHasHome then
  if not TestAtHome then
  if not TestMining then
    Idle..}

//Here come unit tasks in priorities
//Priority no.1 - find self a food
//Priority no.2 - find self a home
//Priority no.3 - find self a work
    if fCondition<UNIT_MIN_CONDITION then begin
      //@Lewin: Can you tweak it such a way that it returned list of available Inns and then choice was performed by both distance and food availability? See StoneMines mission for example - all units go to nearest Inn and it's immidiately emptied! Noone goes to second Inn
      //@Krom:  I've done that, but it just chooses the closest inn that has food, can be walked to and is not full. But that's all KaM ever did so I don't think it's a problem.
      //        A few comments: - Why did you allow 10 extra invisible units in the inn? KaM always limited it to 6, which I think was good because it meant that you needed multiple inns. In KaM you can always see what's happenen, and invisible eaters go against this.
      //                        - I assume that you've set the UNIT_MIN_CONDITION condition high for debugging, but could it be a bit lower? Because now units go and eat right at the start because of my random condidtion thing. Maybe at 2/3 condition they go eat?
      //                        - We need a second UNIT_MIN_CONDITION (which will be half the current one I think) and once units have less than that much they will show the death thought all of the time. This could just be calculated as (UNIT_MIN_CONDITION div 2) I suppose, although we might want to change it.
      //        Please give me your thoughts on all of these matters.
      H:=fPlayers.Player[byte(fOwner)].FindInn(GetPosition);
      if H<>nil then
        fUnitTask:=TTaskGoEat.Create(H,Self)
      else //If there are no Inns available or no food in them
        //StayStillAndDieSoon(Warriors) or GoOutsideShowHungryThought(Citizens) or IgnoreHunger(Workers,Serfs)
        //for now - IgnoreHunger for all
    end;

    if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
      if (fHome=nil) then
        if FindHome then begin
          fThought:=th_Home;
          fUnitTask:=TTaskGoHome.Create(fHome.GetEntrance(Self),Self) //Home found - go there
        end else begin
          if random(2)=0 then fThought:=th_Quest;
          SetActionStay(120, ua_Walk) //There's no home
        end
      else
        if fVisible then begin//Unit is not at home, still it has one
          fThought:=th_Home;
          fUnitTask:=TTaskGoHome.Create(fHome.GetEntrance(Self),Self)
        end else
          fUnitTask:=InitiateMining; //Unit is at home, so go get a job

  if fHome <> nil then
       RestTime := HouseDAT[integer(fHome.GetHouseType)].WorkerRest*10
  else RestTime := 120; //Unit may not have a home; if so, load a default value

  if fUnitTask=nil then begin
    if random(2)=0 then fThought:=th_Quest;
    SetActionStay(RestTime, ua_Walk); //Absolutely nothing to do ...
  end;

  Assert(fCurrentAction<>nil,'Unit has no action!');
end;


function TKMUnitCitizen.InitiateMining():TUnitTask;
var i,Tmp,Res:integer;
begin
Result:=nil;

Res:=1;
//Check if House has production orders
//Random pick from all amount;
if HousePlaceOrders[byte(fHome.GetHouseType)] then begin
  Tmp:= fHome.CheckResOrder(1)+fHome.CheckResOrder(2)+fHome.CheckResOrder(3)+fHome.CheckResOrder(4);
  if Tmp=0 then exit; //No orders
  Tmp:=Random(Tmp)+1; //Pick random
  for i:=1 to 4 do begin
    if InRange(Tmp,1,fHome.CheckResOrder(i)) then Res:=i;
    dec(Tmp,fHome.CheckResOrder(i));
  end;
end;

//if not KMSamePoint(GetPosition,fHome.GetEntrance) then begin
//  Assert(KMSamePoint(GetPosition,fHome.GetEntrance),'Asking for work from wrong spot');
//  fViewport.SetCenter(GetPosition.X,GetPosition.Y);
//end;

WorkPlan.FindPlan(fUnitType,fHome.GetHouseType,HouseOutput[byte(fHome.GetHouseType),Res],KMPointY1(fHome.GetEntrance(Self)));

if not WorkPlan.IsIssued then exit;
if (WorkPlan.Resource1<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource1)<WorkPlan.Count1) then exit;
if (WorkPlan.Resource2<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource2)<WorkPlan.Count2) then exit;
if fHome.CheckResOut(WorkPlan.Product1)>=MaxResInHouse then exit;
if fHome.CheckResOut(WorkPlan.Product2)>=MaxResInHouse then exit;

if HousePlaceOrders[byte(fHome.GetHouseType)] then
  fHome.ResRemOrder(Res);

Result:=TTaskMining.Create(WorkPlan,Self,fHome);
end;


{ TKMSerf }
procedure TKMUnitSerf.Paint();
var AnimAct,AnimDir:integer;
begin
  inherited;
  if not fVisible then exit;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  if MakeShowUnitRoutes then
  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderDebugUnitRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,TUnitActionWalkTo(fCurrentAction).Nodes,TUnitActionWalkTo(fCurrentAction).NodePos,$FFFF00FF);

  fRender.RenderUnit(byte(GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);

  if fUnitTask is TTaskDie then exit; //Do not show unnecessary arms

  if Carry<>rt_None then
    fRender.RenderUnitCarry(integer(Carry), AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1)
  else
    fRender.RenderUnit(byte(GetUnitType), 9, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,false);

  if fThought<>th_None then
  fRender.RenderUnitThought(fThought, AnimStep, fPosition.X+0.5, fPosition.Y - 0.5);
end;


function TKMUnitSerf.UpdateState():boolean;
var
  H:TKMHouseInn;
begin
  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  fThought:=th_None;

  if fCondition<UNIT_MIN_CONDITION then begin
    H:=fPlayers.Player[byte(fOwner)].FindInn(GetPosition);
    if H<>nil then
      fUnitTask:=TTaskGoEat.Create(H,Self)
    else //If there's no Inn or no food in it
      //StayStillAndDieSoon(Warriors) or GoOutsideShowHungryThought(Citizens) or IgnoreHunger(Workers,Serfs)
      //for now - IgnoreHunger for all
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    fUnitTask:=GetActionFromQueue;

  if fUnitTask=nil then begin
    if random(2)=0 then fThought:=th_Quest; //
    SetActionStay(60,ua_Walk); //Stay idle
  end;

  Assert(fCurrentAction<>nil,'Unit has no action!');
end;


function TKMUnitSerf.GiveResource(Res:TResourceType):boolean;
begin
  Result:=false;
  if Carry<>rt_None then exit;
  Carry:=Res;
  Result:=true;
end;


function TKMUnitSerf.TakeResource(Res:TResourceType):boolean;
begin
  Result:=true;
  if Carry=rt_None then
    Result:=false
  else
    Carry:=rt_None;
end;


function TKMUnitSerf.GetActionFromQueue():TUnitTask;
begin
  Result:=fPlayers.Player[byte(fOwner)].DeliverList.AskForDelivery(Self);
end;


{ TKMWorker }
procedure TKMUnitWorker.Paint();
var AnimAct,AnimDir:integer;
begin
  inherited;
  if not fVisible then exit;

  if MakeShowUnitRoutes then
  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderDebugUnitRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,TUnitActionWalkTo(fCurrentAction).Nodes,TUnitActionWalkTo(fCurrentAction).NodePos,$FFFFFFFF);

  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(byte(GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);

  if fThought<>th_None then
  fRender.RenderUnitThought(fThought, AnimStep, fPosition.X+0.5, fPosition.Y - 0.5);
end;


function TKMUnitWorker.UpdateState():boolean;
var
  H:TKMHouseInn;
begin
  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  if fCondition<UNIT_MIN_CONDITION then begin
    H:=fPlayers.Player[byte(fOwner)].FindInn(GetPosition);
    if H<>nil then
      fUnitTask:=TTaskGoEat.Create(H,Self)
    else //If there's no Inn or no food in it
      //StayStillAndDieSoon(Warriors) or GoOutsideShowHungryThought(Citizens) or IgnoreHunger(Workers,Serfs)
      //for now - IgnoreHunger for all
  end;

  if fUnitTask=nil then //If Unit still got nothing to do, nevermind hunger
    fUnitTask:=GetActionFromQueue;

  if fUnitTask=nil then SetActionStay(20,ua_Walk);

  Assert(fCurrentAction<>nil,'Unit has no action!');
end;


function TKMUnitWorker.GetActionFromQueue():TUnitTask;
begin
                   Result:=fPlayers.Player[byte(fOwner)].BuildList.AskForHouseRepair(Self,Self.GetPosition);
if Result=nil then Result:=fPlayers.Player[byte(fOwner)].BuildList.AskForHousePlan(Self,Self.GetPosition);
if Result=nil then Result:=fPlayers.Player[byte(fOwner)].BuildList.AskForHouse(Self,Self.GetPosition);
if Result=nil then Result:=fPlayers.Player[byte(fOwner)].BuildList.AskForRoad(Self,Self.GetPosition);
end;


{ TKMwarrior }
constructor TKMUnitWarrior.Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  fFlagAnim:=0;
  fIsCommander:=false;
  fCommanderID:=nil;
  fOrder:=wo_Stop;
  fOrderLoc:=KMPoint(0,0);
end;


function TKMUnitWarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;


procedure TKMUnitWarrior.PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPoint);
begin
  fOrder:=aWarriorOrder;
  fOrderLoc:=aLoc;
end;


function TKMUnitWarrior.UpdateState():boolean;
begin
  inc(fFlagAnim);

  //@Lewin:
  //Override current action if there's an Order in queue paying attention
  //to unit WalkTo current position (let the unit arrive on next tile first!)
  //As well let the unit finish it's curent Attack action before taking a new order
  //This should make units response delayed, is it a good idea?
  //@Krom: I think that sounds fine. To be deleted
  
  //Dispatch new order when warrior finished previous action part 
  if (fOrder=wo_Walk) and UnitAction.IsStepDone then
  begin
    SetActionWalk(Self,fOrderLoc,KMPoint(0,0));
    fOrder:=wo_Stop; 
  end;

  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;

  SetActionStay(50,ua_Walk);

  Assert(fCurrentAction<>nil,'Unit has no action!');
end;


procedure TKMUnitWarrior.Paint();
var UnitType,AnimAct,AnimDir:byte;
begin
inherited;
  UnitType:=byte(fUnitType);
  AnimAct:=byte(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=byte(Direction);
  
  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
  if fIsCommander then
  fRender.RenderUnit(UnitType,       9, AnimDir, fFlagAnim, byte(fOwner), fPosition.X, fPosition.Y-0.75,false);

  if fThought<>th_None then
  fRender.RenderUnitThought(fThought, AnimStep, fPosition.X+0.5, fPosition.Y - 0.5);
end;


{ TKMUnitAnimal }
function TKMUnitAnimal.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk];
end;


procedure TKMUnitAnimal.Paint();
var AnimAct,AnimDir:integer;
begin
inherited;
  AnimAct:=byte(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=byte(Direction);
  fRender.RenderUnit(byte(Self.GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
end;


function TKMUnitAnimal.UpdateState():boolean;
var
  TimeDelta: Cardinal;
  ActDone,TaskDone: Boolean;
  Spot:TKMPoint; //Target spot where unit will go
  SpotJit:byte; 
begin
  Result:=true; //Required for override compatibility

  ActDone:=true;
  TaskDone:=true;
  TimeDelta:= TimeGetTime - fLastUpdateTime;
  fLastUpdateTime:= TimeGetTime;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, ActDone);

  if ActDone then FreeAndNil(fCurrentAction) else exit;

  if fUnitTask <> nil then
    fUnitTask.Execute(TaskDone);

  if TaskDone then FreeAndNil(fUnitTask) else exit;

  SpotJit:=8; //Initial Spot jitter, it limits number of Spot guessing attempts reducing the range to 0
  repeat //Where unit should go, keep picking until target is walkable for the unit
    dec(SpotJit,1);
    Spot:=fTerrain.SetTileInMapCoords(GetPosition.X+RandomS(SpotJit),GetPosition.Y+RandomS(SpotJit));
  until((SpotJit=0)or(fTerrain.Route_CanBeMade(GetPosition,Spot,AnimalTerrain[byte(GetUnitType)],true)));

  if KMSamePoint(GetPosition,Spot) then begin
    SetActionStay(20, ua_Walk);
    exit;
  end;

  SetActionWalk(Self, Spot, KMPoint(0,0));
  if not TUnitActionWalkTo(fCurrentAction).fRouteBuilt then SetActionStay(5, ua_Walk);

  Assert(fCurrentAction<>nil,'Unit has no action!');
end;


{ TKMUnit }
constructor TKMUnit.Create(const aOwner:TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited Create;
  ScheduleForRemoval:=false;
  fThought := th_None;
  fHome:=nil;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  PrevPosition:=GetPosition;
  NextPosition:=GetPosition;
  fOwner:= aOwner;
  fUnitType:=aUnitType;
  Direction:=dir_S;
  fVisible:=true;
  Speed:=UnitStat[byte(aUnitType)].Speed/24;
  SetActionStay(10,ua_Walk);
  //Units start with a random amount of condition ranging from 3/4 to full.
  //This means that they won't all go eat at the same time and cause crowding, blockages, food shortages and other problems.
  //Note: Warriors of the same group will need to be set the same if they are created at the begining of the mission
  fCondition:=UNIT_MAX_CONDITION-Random(UNIT_MAX_CONDITION div 4);
  fPlayers.Player[byte(fOwner)].CreatedUnit(fUnitType);
  fTerrain.UnitAdd(NextPosition);
end;

destructor TKMUnit.Destroy;
begin
  fTerrain.UnitRem(NextPosition);
  FreeAndNil(fCurrentAction);
  FreeAndNil(fUnitTask);
  if Assigned(fPlayers) and Assigned(fPlayers.Player[byte(fOwner)]) then
    fPlayers.Player[byte(fOwner)].DestroyedUnit(fUnitType);
  Inherited;
end;


{Call this procedure to properly kill unit}
procedure TKMUnit.KillUnit;
begin
  if (fUnitTask is TTaskDie) then exit; //Don't kill unit if it's already dying

  if Self is TKMUnitWarrior then begin
    TKMUnitWarrior(Self).fIsCommander:=false; //Remove commanders flag
    //Reassign commanders flag to another unit
  end;

  //Abandon delivery if any
  if (Self is TKMUnitSerf) and (Self.fUnitTask is TTaskDeliver) then
    TTaskDeliver(Self.fUnitTask).AbandonDelivery;

  fThought:=th_None; //Reset thought
  SetAction(nil,0); //Dispose of current action
  FreeAndNil(fUnitTask); //Should be overriden to dispose of Task-specific items
  fUnitTask:=TTaskDie.Create(Self);
end;


function TKMUnit.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= UnitSupportedActions[integer(fUnitType)];
end;

function TKMUnit.GetPosition():TKMPoint;
begin
  Result:=KMPointRound(fPosition);
end;

function TKMUnit.GetUnitTaskText():string;
begin
  Result:='Idle';
  if fUnitTask is TTaskSelfTrain then Result:='Self-training in school';
  if fUnitTask is TTaskDeliver then Result:='Delivering';
  if fUnitTask is TTaskBuildRoad then Result:='Building road';
  if fUnitTask is TTaskBuildWine then Result:='Building wine field';
  if fUnitTask is TTaskBuildField then Result:='Building corn field';
  if fUnitTask is TTaskBuildHouseArea then Result:='Preparing house area';
  if fUnitTask is TTaskBuildHouse then Result:='Building house';
  if fUnitTask is TTaskGoHome then Result:='Going home';
  if fUnitTask is TTaskGoEat then Result:='Going to eat';
  if fUnitTask is TTaskMining then Result:='Mining resources';
end;


function TKMUnit.GetUnitActText():string;
begin
  Result:=' - ';
  if fCurrentAction is TUnitActionWalkTo then
    Result:=TUnitActionWalkTo(fCurrentAction).Explanation;
end;


function TKMUnit.HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
begin
  Result:= (X = GetPosition.X) and (Y = GetPosition.Y) and ((fUnitType=UT)or(UT=ut_Any)) and not (fUnitType in [ut_Wolf..ut_Duck]);
end;


procedure TKMUnit.SetAction(aAction: TUnitAction; aStep:integer);
begin
  AnimStep:=aStep;
  if aAction = nil then
  begin
    FreeAndNil(fCurrentAction);
    Exit;
  end;
  if not (aAction.ActionType in GetSupportedActions) then
  begin
    FreeAndNil(aAction);
    exit;
  end;
  if fCurrentAction <> aAction then
  begin
    fCurrentAction.Free;
    fCurrentAction := aAction;
  end;
end;


procedure TKMUnit.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouseType:THouseType=ht_None);
begin
  SetAction(TUnitActionGoIn.Create(aAction, aGoDir, aHouseType),0);
end;


procedure TKMUnit.SetActionStay(aTimeToStay:integer; aAction: TUnitActionType; aStayStill:boolean=true; aStillFrame:byte=0; aStep:integer=0);
begin
  SetAction(TUnitActionStay.Create(aTimeToStay, aAction, aStayStill, aStillFrame),aStep);
end;


procedure TKMUnit.SetActionWalk(aKMUnit: TKMUnit; aLocB,aAvoid:TKMPoint; aActionType:TUnitActionType=ua_Walk; aWalkToSpot:boolean=true; aIgnorePass:boolean=false);
begin
  SetAction(TUnitActionWalkTo.Create(aKMUnit, aLocB, aAvoid, aActionType, aWalkToSpot, aIgnorePass),0);
end;


procedure TKMUnit.Feed(Amount:single);
begin
  fCondition:=min(fCondition+round(Amount),UNIT_MAX_CONDITION);
end;


procedure TKMUnit.RemoveUntrainedFromSchool();
begin
  if Assigned(fPlayers) and Assigned(fPlayers.Player[byte(fOwner)]) then
    fPlayers.Player[byte(fOwner)].DestroyedUnit(fUnitType);
  ScheduleForRemoval:=true;
end;

{Here are common Unit.UpdateState routines}
function TKMUnit.UpdateState():boolean;
var
  TimeDelta: Cardinal;
  ActDone,TaskDone: Boolean;
begin
  //There are layers of unit activity (bottom to top):
  // - Action (Atom creating layer (walk 1frame, etc..))
  // - Task (Action creating layer)
  // - specific UpdateState (Task creating layer)

  //Parallel to Action&Task each tick runs common UpdateState (see below)
  // - Become hungrier
  // - update FOW
  // - die cos of hunger        

  Result:=true;

  //Make unit hungry
  if fCondition>0 then dec(fCondition);

  //Feed the unit automatically
  if (not DO_UNIT_HUNGER)and(fCondition<UNIT_MIN_CONDITION+100) then fCondition:=UNIT_MAX_CONDITION;

  //Can use fCondition as a sort of counter to reveal terrain X times a sec
  if fCondition mod 10 = 0 then fTerrain.RevealCircle(GetPosition,UnitStat[byte(fUnitType)].Sight,20,fOwner);

  if (fThought<>th_Death) and (fCondition <= UNIT_MIN_CONDITION div 3) then
    fThought:=th_Death;

  if (fThought=th_Death) and (fCondition > UNIT_MIN_CONDITION) then
    fThought:=th_None;

  if fCondition=0 then
    KillUnit;

  //Reset unit activity if home was destroyed, except when unit is dying
  if (fHome<>nil)and(fHome.IsDestroyed)and(not(fUnitTask is TTaskDie)) then begin
    fHome:=nil;
    FreeAndNil(fCurrentAction);
    FreeAndNil(fUnitTask);
    fVisible:=true;
  end;

  ActDone:=true;
  TaskDone:=true;
  TimeDelta:= TimeGetTime - fLastUpdateTime;
  fLastUpdateTime:= TimeGetTime;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, ActDone);

  if ActDone then FreeAndNil(fCurrentAction) else exit;

  if fUnitTask <> nil then
    fUnitTask.Execute(TaskDone);

  if TaskDone then FreeAndNil(fUnitTask) else exit;

  //If we get to this point then it means that common part is done and now
  //we can perform unit-specific activities (ask for job, etc..)
  Result:=false;
end;


procedure TKMUnit.Paint();
begin
  //Assert(fUnitTask<>nil,'Unit '+TypeToString(fUnitType)+' has no task!');
  if fCurrentAction=nil then
  begin
    Assert(fCurrentAction<>nil,'Unit '+TypeToString(fUnitType)+' has no action!');
    SetActionStay(10, ua_Walk);
  end;
  //Here should be catched any cases where unit has no current action - this is a flaw in TTasks somewhere
  //Unit always meant to have some Action performed.
end;


{ TTaskSelfTrain }
{Train itself in school}
constructor TTaskSelfTrain.Create(aUnit:TKMUnit; aSchool:TKMHouseSchool);
begin
  fUnit:=aUnit;
  fSchool:=aSchool;
  Phase:=0;
  fUnit.fVisible:=false;
  fUnit.SetActionStay(0,ua_Walk);
end;


procedure TTaskSelfTrain.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case Phase of
  0: begin
      fSchool.SetState(hst_Work,0);
      fSchool.fCurrentAction.SubActionWork(ha_Work1,30);
      SetActionStay(29,ua_Walk);
    end;
  1: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work2,30);
      SetActionStay(29,ua_Walk);
    end;
  2: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work3,30);
      SetActionStay(29,ua_Walk);
    end;
  3: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work4,30);
      SetActionStay(29,ua_Walk);
    end;
  4: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work5,30);
      SetActionStay(29,ua_Walk);
    end;
  5: begin
      fSchool.SetState(hst_Idle,10);
      SetActionStay(9,ua_Walk);
     end;
  6: begin
      SetActionGoIn(ua_Walk,gid_Out,ht_School);
      fSchool.UnitTrainingComplete;
     end;
  7: TaskDone:=true;
end;
inc(Phase);
end;


{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
begin
  Assert((toHouse=nil)or(toUnit=nil),'Deliver to House AND Unit?');
  fSerf:=aSerf;
  fFrom:=aFrom;
  fToHouse:=toHouse;
  fToUnit:=toUnit;
  fResourceType:=Res;
  Phase:=0;
  ID:=aID;
  fSerf.SetActionStay(0,ua_Walk);
end;


procedure TTaskDeliver.AbandonDelivery();
begin
  fPlayers.Player[byte(fSerf.fOwner)].DeliverList.AbandonDelivery(ID);
end;


procedure TTaskDeliver.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

with fSerf do
case Phase of
0: begin
    SetActionWalk(fSerf,KMPointY1(fFrom.GetEntrance(Self)), KMPoint(0,0));
   end;
1: if not fFrom.IsDestroyed then
     SetActionGoIn(ua_Walk,gid_In,fFrom.GetHouseType)
   else begin
     AbandonDelivery;
     TaskDone:=true;
   end;
2: SetActionStay(5,ua_Walk);
3: if not fFrom.IsDestroyed then
   begin
     if fFrom.ResTakeFromOut(fResourceType) then begin
       GiveResource(fResourceType);
       fPlayers.Player[byte(fOwner)].DeliverList.TakenOffer(ID);
     end else begin
       fPlayers.Player[byte(fOwner)].DeliverList.AbandonDelivery(ID);
       Assert(false,'Resource''s gone..');
     end;
     SetActionGoIn(ua_Walk,gid_Out,fFrom.GetHouseType);
   end else begin
     AbandonDelivery;
     TaskDone:=true;
   end;
4: if Carry=rt_None then TaskDone:=true else SetActionStay(0,ua_Walk);
end;

//Deliver into complete house
if fToHouse<>nil then
if fToHouse.IsComplete then
with fSerf do
case Phase of
5: SetActionWalk(fSerf,KMPointY1(fToHouse.GetEntrance(Self)), KMPoint(0,0));
6: SetActionGoIn(ua_Walk,gid_In,fToHouse.GetHouseType);
7: SetActionStay(5,ua_Walk);
8: begin
     fToHouse.ResAddToIn(Carry);
     TakeResource(Carry);
     SetActionGoIn(ua_walk,gid_Out,fToHouse.GetHouseType);
     fPlayers.Player[byte(fOwner)].DeliverList.GaveDemand(ID);
     fPlayers.Player[byte(fOwner)].DeliverList.AbandonDelivery(ID);
   end;
9: TaskDone:=true;
end;

//Deliver into wip house
if fToHouse<>nil then
if not fToHouse.IsComplete then
with fSerf do
case Phase of
5: SetActionWalk(fSerf,KMPointY1(fToHouse.GetEntrance(Self)), KMPoint(0,0), ua_Walk);
6: begin
     fToHouse.ResAddToBuild(Carry);
     TakeResource(Carry);
     fPlayers.Player[byte(fOwner)].DeliverList.GaveDemand(ID);
     fPlayers.Player[byte(fOwner)].DeliverList.AbandonDelivery(ID);
     TaskDone:=true;
   end;
end;

//Deliver to builder
if fToUnit<>nil then
with fSerf do
case Phase of
5: SetActionWalk(fSerf,fToUnit.GetPosition, KMPoint(0,0),ua_Walk,false);
6: begin
      TakeResource(Carry);
      if (fToUnit<>nil)and(fToUnit.fUnitTask<>nil) then begin
        inc(fToUnit.fUnitTask.Phase);
        fToUnit.SetActionStay(0,ua_Work1);
      end;
    fPlayers.Player[byte(fOwner)].DeliverList.GaveDemand(ID);
    fPlayers.Player[byte(fOwner)].DeliverList.AbandonDelivery(ID);
    TaskDone:=true;
   end;
end;

inc(Phase);
if (fSerf.fCurrentAction=nil)and(not TaskDone) then
  Assert(false);
end;


{ TTaskBuildRoad }
constructor TTaskBuildRoad.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  fWorker:=aWorker;
  fLoc:=aLoc;
  Phase:=0;
  ID:=aID;
  fWorker.SetActionStay(0,ua_Walk);
end;


procedure TTaskBuildRoad.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
0: begin
   SetActionWalk(fWorker,fLoc, KMPoint(0,0));
   fThought := th_Build;
   end;
1: begin
   fThought := th_None;
   fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
   fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
   SetActionStay(11,ua_Work1,false);
   end;
2: begin
   fTerrain.IncDigState(fLoc);
   SetActionStay(11,ua_Work1,false);
   end;
3: begin
   fTerrain.IncDigState(fLoc);
   SetActionStay(11,ua_Work1,false);
   fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(nil, fWorker, rt_Stone, 1, dt_Once, di_High);
   end;

4: begin
   SetActionStay(30,ua_Work1);
   fThought:=th_Stone;
   end;

5: begin
   SetActionStay(11,ua_Work2,false);
   fThought:=th_None;
   end;
6: begin
   fTerrain.IncDigState(fLoc);
   SetActionStay(11,ua_Work2,false);
   end;
7: begin
   fTerrain.IncDigState(fLoc);
   fTerrain.FlattenTerrain(fLoc); //Flatten the terrain slightly on and around the road
   if MapElem[fTerrain.Land[fLoc.Y,fLoc.X].Obj+1].Properties[mep_Quad]=1 then
     fTerrain.Land[fLoc.Y,fLoc.X].Obj:=255; //Remove fields and other quads as they won't fit with road
   SetActionStay(11,ua_Work2,false);
   end;
8: begin
   fTerrain.SetRoad(fLoc,fOwner);
   fPlayers.Player[byte(fOwner)].BuildList.CloseRoad(ID);
   SetActionStay(5,ua_Work2);
   fTerrain.RemMarkup(fLoc);
   end;
9: TaskDone:=true;
end;
if Phase<>4 then inc(Phase); //Phase=4 is when worker waits for rt_Stone
end;


{ TTaskBuildWine }
constructor TTaskBuildWine.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
fWorker:=aWorker;
fLoc:=aLoc;
Phase:=0;
ID:=aID;
fWorker.SetActionStay(0,ua_Walk);
end;


procedure TTaskBuildWine.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
 0: begin
      SetActionWalk(fWorker,fLoc, KMPoint(0,0));
      fThought := th_Build;
    end;
 1: begin
      fThought := th_None;
      fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
      fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
      SetActionStay(11,ua_Work1,false);
    end;
 2: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(11,ua_Work1,false);
    end;
 3: begin
      fTerrain.IncDigState(fLoc);
      SetActionStay(11,ua_Work1,false);
      fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(nil,fWorker,rt_Wood, 1, dt_Once, di_High);
    end;
 4: begin
      SetActionStay(30,ua_Work1);
      fThought:=th_Wood;
    end;
 5: begin
      SetActionStay(11*4,ua_Work2,false);
      fThought:=th_None;
    end;  
 6: begin
      fTerrain.SetField(fLoc,fOwner,ft_Wine);
      fPlayers.Player[byte(fOwner)].BuildList.CloseRoad(ID);
      SetActionStay(5,ua_Work2);
      fTerrain.RemMarkup(fLoc);
    end;
 7: TaskDone:=true;
end;
if Phase<>4 then inc(Phase); //Phase=4 is when worker waits for rt_Stone
end;


{ TTaskBuildField }
constructor TTaskBuildField.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
fWorker:=aWorker;
fLoc:=aLoc;
Phase:=0;
Phase2:=0;
ID:=aID;
fWorker.SetActionStay(0,ua_Walk);
end;


procedure TTaskBuildField.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
  0: begin
       SetActionWalk(fWorker,fLoc, KMPoint(0,0));
       fThought := th_Build;
     end;
  1: begin
      fTerrain.SetMarkup(fLoc,mu_UnderConstruction);
      fTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
      SetActionStay(0,ua_Walk);
     end;
  2: begin
      SetActionStay(11,ua_Work1,false);
      inc(Phase2);
      if Phase2 in [4,8,10] then fTerrain.IncDigState(fLoc);
     end;
  3: begin
      fThought := th_None; //Keep thinking build until it's done
      fPlayers.Player[byte(fOwner)].BuildList.CloseRoad(ID);
      fTerrain.SetField(fLoc,fOwner,ft_Corn);
      SetActionStay(5,ua_Walk);
      fTerrain.RemMarkup(fLoc);
     end;
  4: TaskDone:=true;
end;
if Phase2 in [0,10] then inc(Phase);
end;


{ TTaskBuildHouseArea }
constructor TTaskBuildHouseArea.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
var i,k:integer;
begin
  fWorker:=aWorker;
  fHouse:=aHouse;
  Phase:=0;
  TaskID:=aID;
  Step:=0;
  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[byte(fHouse.GetHouseType),i,k]<>0 then begin
    inc(Step);
    ListOfCells[Step]:=KMPoint(fHouse.GetPosition.X+k-3,fHouse.GetPosition.Y + i - 4);
  end;
  fWorker.SetActionStay(0,ua_Walk);
end;


{Prepare building site - flatten terrain}
procedure TTaskBuildHouseArea.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fWorker do
case Phase of
0:  begin
      SetActionWalk(fWorker,fHouse.GetEntrance(Self), KMPoint(0,0));
      fThought := th_Build;
    end;
1:  if not fHouse.IsDestroyed then begin //House plan was cancelled before worker has arrived on site
      fTerrain.SetHouse(fHouse.GetPosition, fHouse.GetHouseType, hs_Fence, fOwner);
      fHouse.SetBuildingState(hbs_NoGlyph);
      SetActionStay(5,ua_Walk);
      fThought := th_None;
    end else begin
      TaskDone:=true;
      fThought := th_None;
    end;
2:  SetActionWalk(fWorker,ListOfCells[Step], KMPoint(0,0), ua_Walk, true, true);
3:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
4:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
5:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
6:  begin
      SetActionStay(11,ua_Work1,false);
      fTerrain.FlattenTerrain(ListOfCells[Step]);
      if not fHouse.IsDestroyed then
      if KMSamePoint(fHouse.GetEntrance(Self),ListOfCells[Step]) then
        fTerrain.SetRoad(fHouse.GetEntrance(Self), fOwner);

      fTerrain.Land[ListOfCells[Step].Y,ListOfCells[Step].X].Obj:=255; //All objects are removed
      dec(Step);
    end;
7:  SetActionWalk(fWorker,KMPointY1(fHouse.GetEntrance(Self)), KMPoint(0,0), ua_Walk, true, true);
8:  begin
      fPlayers.Player[byte(fOwner)].BuildList.CloseHousePlan(TaskID);
      fHouse.SetBuildingState(hbs_Wood);
      fPlayers.Player[byte(fOwner)].BuildList.AddNewHouse(fHouse); //Add the house to JobList, so then all workers could take it
      with HouseDAT[byte(fHouse.GetHouseType)] do begin
        fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(fHouse, nil, rt_Wood, WoodCost, dt_Once, di_High);
        fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(fHouse, nil, rt_Stone, StoneCost, dt_Once, di_High);
      end;
      TaskDone:=true;
    end;
end;
inc(Phase);
if (Phase=7)and(Step>0) then Phase:=2; //Repeat with next cell
end;


{ TTaskBuildHouse }
constructor TTaskBuildHouse.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
  var i,k:integer; ht:byte; Loc:TKMPoint;
  procedure AddLoc(X,Y:word; Dir:TKMDirection);
  begin
    //First check that the passabilty is correct, as the house may be placed against blocked terrain
    if not (canWalk in fTerrain.Land[Y,X].Passability) then exit;
    inc(LocCount);
    Cells[LocCount].Loc:=KMPoint(X,Y);
    Cells[LocCount].Dir:=Dir;
  end;
begin
  fWorker:=aWorker;
  fHouse:=aHouse;
  Loc:=fHouse.GetPosition;
  Phase:=0;
  Phase2:=0;
  TaskID:=aID;
  LocCount:=0;
  CurLoc:=0;
  ht:=byte(fHouse.GetHouseType);
  //Create list of surrounding tiles and directions
  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[ht,i,k]<>0 then
    if (i=1)or(HousePlanYX[ht,i-1,k]=0) then AddLoc(Loc.X + k - 3, Loc.Y + i - 4 - 1, dir_S) else //Up
    if (i=4)or(HousePlanYX[ht,i+1,k]=0) then AddLoc(Loc.X + k - 3, Loc.Y + i - 4 + 1, dir_N) else //Down
    if (k=4)or(HousePlanYX[ht,i,k+1]=0) then AddLoc(Loc.X + k - 3 + 1, Loc.Y + i - 4, dir_W) else //Right
    if (k=1)or(HousePlanYX[ht,i,k-1]=0) then AddLoc(Loc.X + k - 3 - 1, Loc.Y + i - 4, dir_E);     //Left
  fWorker.SetActionStay(0,ua_Walk);
end;


{Build the house}
procedure TTaskBuildHouse.Execute(out TaskDone:boolean);
begin
  TaskDone:=false;
  with fWorker do
    case Phase of
      //Pick random location and go there
      0: begin
           //If house has not enough resource to be built, consider building task is done
           //and look for a new task that has enough resouces. Once this house has building resources
           //delivered it will be available from build queue again.
           if not fHouse.CheckResToBuild then begin
             TaskDone:=true; //Drop the task
             fThought := th_None;
             exit;
           end;
           fThought := th_Build;
           CurLoc:=Random(LocCount)+1;
           SetActionWalk(fWorker,Cells[CurLoc].Loc, KMPoint(0,0));
         end;
      1: begin
           Direction:=Cells[CurLoc].Dir;
           SetActionStay(0,ua_Walk);
         end;
      2: begin
           SetActionStay(5,ua_Work,false,0,0); //Start animation
           Direction:=Cells[CurLoc].Dir;
           if fHouse.IsStone then fTerrain.SetHouse(fHouse.GetPosition, fHouse.GetHouseType, hs_Built, fOwner); //Remove house plan when we start the stone phase (it is still required for wood)
         end;
      3: begin
           //Cancel building no matter progress if resource depleted or must eat
           //@Lewin: Is this right behaviour, that worker can drop building task for food?
           //On a side note: Unit should have a CanGoEat function, cos if there are no Inns task shouldn't be dropped like that
           if (not fHouse.CheckResToBuild)or(fCondition<UNIT_MIN_CONDITION) then begin
             TaskDone:=true; //Drop the task
             fThought := th_None;
             exit;
           end;
           fHouse.IncBuildingProgress;
           SetActionStay(6,ua_Work,false,0,5); //Do building and end animation
           inc(Phase2);
         end;
      4: begin
           fPlayers.Player[byte(fOwner)].BuildList.CloseHouse(TaskID);
           TaskDone:=true;
           fThought := th_None;
         end;
    end;
  inc(Phase);
  if (Phase=4) and (not fHouse.IsComplete) then //If animation cycle is done
    if Phase2 mod 5 = 0 then //if worker did [5] hits from same spot
      Phase:=0 //Then goto new spot
    else
      Phase:=2; //else do more hits
end;


{ TTaskBuildHouseRepair }
constructor TTaskBuildHouseRepair.Create(aWorker:TKMUnitWorker; aHouse:TKMHouse; aID:integer);
  var i,k:integer; ht:byte; Loc:TKMPoint;
  procedure AddLoc(X,Y:word; Dir:TKMDirection);
  begin
    inc(LocCount);
    Cells[LocCount].Loc:=KMPoint(X,Y);
    Cells[LocCount].Dir:=Dir;
  end;
begin
  fWorker:=aWorker;
  fHouse:=aHouse;
  Loc:=fHouse.GetPosition;
  Phase:=0;
  Phase2:=0;
  TaskID:=aID;
  LocCount:=0;
  CurLoc:=0;
  ht:=byte(fHouse.GetHouseType);
  //Create list of surrounding tiles and directions
  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[ht,i,k]<>0 then
    if (i=1)or(HousePlanYX[ht,i-1,k]=0) then AddLoc(Loc.X + k - 3, Loc.Y + i - 4 - 1, dir_S) else //Up
    if (i=4)or(HousePlanYX[ht,i+1,k]=0) then AddLoc(Loc.X + k - 3, Loc.Y + i - 4 + 1, dir_N) else //Down
    if (k=4)or(HousePlanYX[ht,i,k+1]=0) then AddLoc(Loc.X + k - 3 + 1, Loc.Y + i - 4, dir_W) else //Right
    if (k=1)or(HousePlanYX[ht,i,k-1]=0) then AddLoc(Loc.X + k - 3 - 1, Loc.Y + i - 4, dir_E);     //Left
  fWorker.SetActionStay(0,ua_Walk);
end;


{Repair the house}
procedure TTaskBuildHouseRepair.Execute(out TaskDone:boolean);
begin
  if (not fHouse.IsDamaged)or(not fHouse.BuildingRepair) then begin
   TaskDone:=true; //Drop the task
   fWorker.fThought := th_None;
   exit;
  end;

  TaskDone:=false;
  with fWorker do
    case Phase of
      //Pick random location and go there
      0: begin
           fThought := th_Build;
           CurLoc:=Random(LocCount)+1;
           SetActionWalk(fWorker,Cells[CurLoc].Loc, KMPoint(0,0));
         end;
      1: begin
           Direction:=Cells[CurLoc].Dir;
           SetActionStay(0,ua_Walk);
         end;
      2: begin
           SetActionStay(5,ua_Work,false,0,0); //Start animation
           Direction:=Cells[CurLoc].Dir;
         end;
      3: begin
           if (fCondition<UNIT_MIN_CONDITION) then begin
             TaskDone:=true; //Drop the task
             exit;
           end;
           fHouse.AddRepair;
           SetActionStay(6,ua_Work,false,0,5); //Do building and end animation
           inc(Phase2);
         end;
      4: begin
           fThought := th_None;
           fPlayers.Player[byte(fOwner)].BuildList.CloseHouse(TaskID);
           TaskDone:=true;
         end;
    end;
  inc(Phase);
  if (Phase=4) and (fHouse.IsDamaged) then //If animation cycle is done
    if Phase2 mod 5 = 0 then //if worker did [5] hits from same spot
      Phase:=0 //Then goto new spot
    else
      Phase:=2; //else do more hits
end;


{ TTaskMining }
constructor TTaskMining.Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit; aHouse:TKMHouse);
begin
WorkPlan:=aWorkPlan;
fUnit:=aUnit;
Phase:=0;
Phase2:=0;
BeastID:=0;
fUnit.SetActionStay(0,ua_Walk);
end;


{This is execution of Resource mining}
procedure TTaskMining.Execute(out TaskDone:boolean);
const SkipWalk=7; SkipWork=29; //Skip to certain Phases
var Dir:integer; TimeToWork, StillFrame:integer;
begin
TaskDone:=false;
with fUnit do
  case Phase of
    0: if WorkPlan.HasToWalk then begin
         fHome.SetState(hst_Empty,0);
         SetActionGoIn(WorkPlan.WalkTo,gid_Out,fHome.GetHouseType); //Walk outside the house
       end else begin
         Phase:=SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
         SetActionStay(0,ua_Walk);
         exit;
       end;
    1: SetActionWalk(fUnit,WorkPlan.Loc, KMPoint(0,0),WorkPlan.WalkTo);
    2: //IF resource still exists on location
       begin //Choose direction and time to work
         Dir:=integer(Direction);
         if UnitSprite[integer(fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count<=1 then
           for Dir:=1 to 8 do
             if UnitSprite[integer(fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count>1 then break;
         Dir:=min(Dir,8);
         //Some actions have specific directions
         if WorkPlan.GatheringScript = gs_WoodCutterPlant then Dir:=1;
         if WorkPlan.GatheringScript = gs_WoodCutterCut   then Dir:=8; //Will need to be improved later to choose the direction based on the direction of approch. For now always cut from the bottom left.
         Direction:=TKMDirection(Dir);
         TimeToWork:=WorkPlan.WorkCyc*max(UnitSprite[integer(fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count,1);
         SetActionStay(TimeToWork, WorkPlan.WorkType, false);
       end; 
    3: begin if WorkPlan.GatheringScript = gs_WoodCutterCut then
             begin
               SetActionStay(10, WorkPlan.WorkType, true, 5);
             end
             else
               SetActionStay(0, WorkPlan.WorkType);
       end;
    4: begin StillFrame := 0;
             case WorkPlan.GatheringScript of //Perform special tasks if required
               gs_StoneCutter: fTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
               gs_FarmerSow:   fTerrain.SowCorn(WorkPlan.Loc);
               gs_FarmerCorn:  fTerrain.CutCorn(WorkPlan.Loc);
               gs_FarmerWine:  fTerrain.CutGrapes(WorkPlan.Loc);
               gs_WoodCutterPlant: fTerrain.SetTree(WorkPlan.Loc,ChopableTrees[Random(length(ChopableTrees))+1,1]);
               gs_WoodCutterCut:   begin fTerrain.FallTree(WorkPlan.Loc); StillFrame := 5; end;
             end;
         SetActionStay(WorkPlan.AfterWorkDelay, WorkPlan.WorkType, true, StillFrame);
       end;
    5: begin
         if WorkPlan.GatheringScript = gs_WoodCutterCut then
           fTerrain.ChopTree(WorkPlan.Loc); //Make the tree turn into a stump
         SetActionWalk(fUnit,KMPointY1(fHome.GetEntrance(Self)), KMPoint(0,0),WorkPlan.WalkFrom); //Go home
         fThought := th_Home;
       end;
    6: SetActionGoIn(WorkPlan.WalkFrom,gid_In,fHome.GetHouseType); //Go inside

    {Unit back at home and can process its booty now}
    7: begin
        fThought := th_None;
        Phase2:=1;
        fHome.SetState(hst_Work,0); //Set house to Work state
        fHome.ResTakeFromIn(WorkPlan.Resource1); //Count should be added
        fHome.ResTakeFromIn(WorkPlan.Resource2);
        fHome.fCurrentAction.SubActionAdd([ha_Smoke]);
        if WorkPlan.GatheringScript = gs_SwineBreeder then begin
          BeastID:=TKMHouseSwineStable(fHome).FeedBeasts;
          if BeastID<>0 then
            TKMHouseSwineStable(fHome).TakeBeast(BeastID);
        end;
        if WorkPlan.ActCount>=Phase2 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[Phase2].Act,WorkPlan.HouseAct[Phase2].TimeToWork);
           //Keep unit idling till next Phase, Idle time is -1 to compensate TaskExecution Phase
           SetActionStay(WorkPlan.HouseAct[Phase2].TimeToWork-1,ua_Walk);
        end else begin
           Phase:=SkipWork;
           SetActionStay(0,ua_Walk);
           exit;
        end;
       end;
    8..28: begin //Allow for 20 different "house work" phases
           inc(Phase2);
           if (Phase2=2)and(WorkPlan.GatheringScript = gs_HorseBreeder) then BeastID:=TKMHouseSwineStable(fHome).FeedBeasts;
           if WorkPlan.ActCount>=Phase2 then begin
             fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[Phase2].Act,WorkPlan.HouseAct[Phase2].TimeToWork);
             SetActionStay(WorkPlan.HouseAct[Phase2].TimeToWork-1,ua_Walk);
           end else begin
             Phase:=SkipWork;
             SetActionStay(0,ua_Walk);
             exit;
           end;
       end;
    29: begin
          case WorkPlan.GatheringScript of
            gs_CoalMiner: fTerrain.DecCoalDeposit(WorkPlan.Loc);
            gs_GoldMiner: fTerrain.DecOreDeposit(WorkPlan.Loc,rt_GoldOre);
            gs_IronMiner: fTerrain.DecOreDeposit(WorkPlan.Loc,rt_IronOre);
          end;
          if WorkPlan.GatheringScript = gs_HorseBreeder then begin
            if BeastID<>0 then begin
              TKMHouseSwineStable(fHome).TakeBeast(BeastID);
              fHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
            end;
          end else
          if WorkPlan.GatheringScript = gs_SwineBreeder then begin
            if BeastID<>0 then begin
              fHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
              fHome.ResAddToOut(WorkPlan.Product2,WorkPlan.ProdCount2);
            end;
          end else begin
            fHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
            fHome.ResAddToOut(WorkPlan.Product2,WorkPlan.ProdCount2); //This is unused tbh
          end;

          fHome.SetState(hst_Idle,WorkPlan.AfterWorkIdle);
          SetActionStay(WorkPlan.AfterWorkIdle-1,ua_Walk);
        end;
    30: TaskDone:=true;
  end;
inc(Phase);
if (fUnit.fCurrentAction=nil)and(not TaskDone) then
  Assert(false);
end;


{ TTaskGoHome }
constructor TTaskGoHome.Create(aTo:TKMPoint; aUnit:TKMUnit);
begin
  fDestPos:=aTo;
  fUnit:=aUnit;
  Phase:=0;
  fUnit.SetActionStay(0,ua_Walk);
end;


procedure TTaskGoHome.Execute(out TaskDone:boolean);
begin
  TaskDone:=false;
  with fUnit do
  case Phase of
    0: SetActionWalk(fUnit,KMPointY1(fDestPos), KMPoint(0,0));
    1: SetActionGoIn(ua_Walk,gid_In,fUnit.fHome.GetHouseType);
    2: begin
        SetActionStay(5,ua_Walk);
        fHome.SetState(hst_Idle,0);
       end;
    3: TaskDone:=true;
  end;
  inc(Phase);
  if (fUnit.fCurrentAction=nil)and(not TaskDone) then
    Assert(false);
end;


{ TTaskDie }
constructor TTaskDie.Create(aUnit:TKMUnit);
begin
  fUnit:=aUnit;
  Phase:=0;
  fUnit.SetActionStay(0,ua_Walk);
end;


procedure TTaskDie.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case Phase of
  0: if not fVisible then begin
       if fHome<>nil then begin
         fHome.SetState(hst_Idle,0);
         fHome.SetState(hst_Empty,0);
         SetActionGoIn(ua_Walk,gid_Out,fUnit.fHome.GetHouseType);
       end
       else
         SetActionGoIn(ua_Walk,gid_Out); //Inn or Store or etc.. for units without home.
                                         //Which means that our current approach to deduce housetype from
                                         //fUnit.fHome is wrong
     end else
     SetActionStay(0,ua_Walk);
  1: SetActionStay(16-1,ua_Die,false);
  2: begin
      if fHome<>nil then fHome.GetHasOwner:=false;
      //Schedule Unit for removal and remove it after fUnits.UpdateState is done
      fUnit.ScheduleForRemoval:=true;
      //SetActionStay(0,ua_Die);
      TaskDone:=false; //Does matter. We don't want unit to grab another task after dying is done
      exit;
     end;
end;
inc(Phase);
end;


{ TTaskGoEat }
constructor TTaskGoEat.Create(aInn:TKMHouseInn; aUnit:TKMUnit);
begin
  fInn:=aInn;
  fUnit:=aUnit;
  PlaceID:=0;
  Phase:=0;
  fUnit.SetActionStay(0,ua_Walk);
end;


procedure TTaskGoEat.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

with fUnit do
case Phase of
 0: begin
      if fHome<>nil then fHome.SetState(hst_Empty,0);
      if not fVisible then SetActionGoIn(ua_Walk,gid_Out,fUnit.fHome.GetHouseType) else
                           SetActionStay(0,ua_Walk); //Walk outside the house
    end;
 1: begin
      fThought := th_Eat;
      SetActionWalk(fUnit,KMPointY1(fInn.GetEntrance(Self)), KMPoint(0,0));
    end;
 2: begin
      fThought := th_None;
      SetActionGoIn(ua_Walk,gid_In,ht_Inn); //Enter Inn
      PlaceID:=fInn.EaterGetsInside(fUnitType);
    end;
 3: if (fInn.CheckResIn(rt_Bread)>0)and(PlaceID<>0) then begin
      fInn.ResTakeFromIn(rt_Bread);
      SetActionStay(29*8,ua_Eat,false);
      Feed(UNIT_MAX_CONDITION/3);
      fInn.UpdateEater(PlaceID,2); //Order is Wine-Bread-Sausages-Fish
    end else
      SetActionStay(0,ua_Walk);
 4: if (fCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Sausages)>0)and(PlaceID<>0) then begin
      fInn.ResTakeFromIn(rt_Sausages);
      SetActionStay(29*8,ua_Eat,false);
      Feed(UNIT_MAX_CONDITION/2);
      fInn.UpdateEater(PlaceID,3);
    end else
      SetActionStay(0,ua_Walk);
 5: if (fCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Wine)>0)and(PlaceID<>0) then begin
      fInn.ResTakeFromIn(rt_Wine);
      SetActionStay(29*8,ua_Eat,false);
      Feed(UNIT_MAX_CONDITION/4);
      fInn.UpdateEater(PlaceID,1);
    end else
      SetActionStay(0,ua_Walk);
 6: if (fCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Fish)>0)and(PlaceID<>0) then begin
      fInn.ResTakeFromIn(rt_Fish);
      SetActionStay(29*8,ua_Eat,false);
      Feed(UNIT_MAX_CONDITION/4);
      fInn.UpdateEater(PlaceID,4);
    end else
      SetActionStay(0,ua_Walk);
 7: begin
      SetActionGoIn(ua_Walk,gid_Out,ht_Inn); //Exit Inn
      fInn.EatersGoesOut(PlaceID);
    end;
 8: TaskDone:=true;
end;
inc(Phase);
if (fUnit.fCurrentAction=nil)and(not TaskDone) then
  Assert(false);
end;


{ TUnitAction }
constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
  IsStepDone:=false;
end;


{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create(KMUnit: TKMUnit; LocB,Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true; const aIgnorePass:boolean=false);
begin
  Assert(LocB.X*LocB.Y<>0,'Illegal WalkTo 0;0');

  Inherited Create(aActionType);
  fWalker       := KMUnit;
  fWalkFrom     := fWalker.GetPosition;
  fWalkTo       := LocB;
  fWalkToSpot   := aWalkToSpot;
  fIgnorePass   := aIgnorePass; //Store incase we need it in DoUnitInteraction re-routing
  fPass         := ChoosePassability(fWalker, fIgnorePass);

  NodePos       :=1;
  fRouteBuilt   :=false;     

  if KMSamePoint(fWalkFrom,fWalkTo) then //We don't care for this case, Execute will report action is done immediately
    exit; //so we don't need to perform any more processing

  //Build a route A*
  fTerrain.Route_Make(fWalkFrom, fWalkTo, Avoid, fPass, fWalkToSpot, NodeCount, Nodes); //Try to make the route with fPass

  fRouteBuilt:=NodeCount>0;
  if not fRouteBuilt then
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with default fPass');

  if not fRouteBuilt then begin //Build a route with canWalk
    fTerrain.Route_Make(fWalkFrom, fWalkTo, Avoid, canWalk, fWalkToSpot, NodeCount, Nodes); //Try to make a route
    fRouteBuilt:=NodeCount>0;
  end;

  if not fRouteBuilt then
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with canWalk');
end;


function TUnitActionWalkTo.ChoosePassability(KMUnit: TKMUnit; DoIgnorePass:boolean):TPassability;
begin
  case KMUnit.fUnitType of //Select desired passability depending on unit type
    ut_Serf..ut_Fisher,ut_StoneCutter..ut_Recruit: Result:=canWalkRoad; //Citizens except Worker
    ut_Wolf..ut_Duck: Result:=AnimalTerrain[byte(KMUnit.fUnitType)] //Animals
    else Result:=canWalk; //Worker, Warriors
  end;

  if not DO_SERFS_WALK_ROADS then Result:=canWalk; //Reset everyone to canWalk for debug

  //Thats for 'miners' at work
  if (KMUnit.fUnitType in [ut_Woodcutter,ut_Farmer,ut_Fisher,ut_StoneCutter])and(KMUnit.fUnitTask is TTaskMining) then
    Result:=canWalk;

  if DoIgnorePass then Result:=canAll; //Thats for Workers walking on house area and maybe some other cases?
end;


function TUnitActionWalkTo.DoUnitInteraction():boolean;
var U:TKMUnit;
begin
  Result:=true;
  if not DO_UNIT_INTERACTION then exit;
  if NodePos>=NodeCount then exit;                                            //Check if that the last node in route anyway
  if fTerrain.Land[Nodes[NodePos+1].Y,Nodes[NodePos+1].X].IsUnit=0 then exit; //Check if there's a unit blocking the way

  U:=fPlayers.UnitsHitTest(Nodes[NodePos+1].X,Nodes[NodePos+1].Y);

  //If there's yet no Unit on the way but tile is pre-occupied
  if U=nil then begin
    //Do nothing and wait till unit is actually there so we can interact with it
    Explanation:='Can''t walk. No Unit on the way but tile is occupied';
    Result:=false;
    exit;
  end;

  if (fWalker.GetUnitType in [ut_Wolf..ut_Duck])and(not(U.GetUnitType in [ut_Wolf..ut_Duck])) then begin
    Explanation:='Unit is animal and therefor has no priority in movement';
    Result:=false;
    exit;
  end;

  //If Unit on the way is idling
  if (U.fCurrentAction is TUnitActionStay) then begin
    if TUnitActionStay(U.fCurrentAction).ActionType=ua_Walk then begin //Unit stays idle, not working or something
      //Force Unit to go away
      U.SetActionWalk(U,fTerrain.GetOutOfTheWay(U.GetPosition,fWalker.GetPosition,canWalk), KMPoint(0,0));
      Explanation:='Unit blocking the way but it''s forced to get away';
      Result:=false; //Next frame tile will be free and unit will walk there
      exit;
    end else begin
      //If Unit on the way is doing something and won't move away
      {StartWalkingAround}
      Explanation:='Unit on the way is doing something and won''t move away';
      Result:=false;
        fWalker.SetActionWalk
        (
            fWalker,
            fWalkTo,
            KMPoint(Nodes[NodePos+1].X,Nodes[NodePos+1].Y), //Avoid this tile
            fActionType,
            fWalkToSpot,
            fIgnorePass
        );
      exit;
      { UNRESOLVED! }
    end;
  end;

  //If Unit on the way is walking somewhere
  if (U.fCurrentAction is TUnitActionWalkTo) then begin //Unit is walking
    //Check unit direction to be opposite and exchange, but only if the unit is staying on tile, not walking
    if (min(byte(U.Direction),byte(fWalker.Direction))+4 = max(byte(U.Direction),byte(fWalker.Direction))) then begin
      if TUnitActionWalkTo(U.fCurrentAction).DoesWalking then begin
        //Unit yet not arrived on tile, wait till it does, otherwise there might be 2 units on one tile
        Explanation:='Unit on the way is walking '+TypeToString(U.Direction)+'. Waiting till it walks into spot and then exchange';
        Result:=false;
        exit;
      end else begin
        //Graphically both units are walking side-by-side, but logically they simply walk through each-other.
        TUnitActionWalkTo(U.fCurrentAction).DoEvade:=true;
        TUnitActionWalkTo(fWalker.fCurrentAction).DoEvade:=true;
        Explanation:='Unit on the way is walking opposite direction. Perform an exchange';
        Result:=true;
        exit;
      end
    end else begin
      if TUnitActionWalkTo(U.fCurrentAction).DoesWalking then begin
        //Simply wait till it walks away
        Explanation:='Unit on the way is walking '+TypeToString(U.Direction)+'. Waiting till it walks away '+TypeToString(fWalker.Direction);
        Result:=false;
        exit;
      end else begin
        //Unit isn't walking away - go around it
        Explanation:='Unit on the way is walking '+TypeToString(U.Direction)+'. Waiting till it walks away '+TypeToString(fWalker.Direction);
        Result:=false;
        {fWalker.SetActionWalk
        (
            fWalker,
            fWalkTo,
            KMPoint(Nodes[NodePos+1].X,Nodes[NodePos+1].Y), //Avoid this tile
            fActionType,
            fWalkToSpot,
            fIgnorePass
        );}
        exit;
        { UNRESOLVED! } //see SolveDiamond
      end;
    end;
  end;

  if (U.fCurrentAction is TUnitActionGoIn) then begin //Unit is walking into house, we can wait
    Explanation:='Unit is walking into house, we can wait';
    Result:=false; //Temp
    exit;
  end;
    //If enything else - wait
    Assert(false,'DO_UNIT_INTERACTION');
end;


procedure TUnitActionWalkTo.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection = ((dir_NW,dir_W,dir_SW),(dir_N,dir_NA,dir_S),(dir_NE,dir_E,dir_SE));
var
  DX,DY:shortint; WalkX,WalkY,Distance:single;
begin
  DoEnd:= False;
  IsStepDone:=false;
  DoesWalking:=false; //Set it to false at start of update

  //Happens whe e.g. Serf stays in front of Store and gets Deliver task
  if KMSamePoint(fWalkFrom,fWalkTo) then begin
    DoEnd:=true;
    exit;
  end;

  //Somehow route was not built, this is an error
  if not fRouteBuilt then begin  
    fLog.AddToLog('Unable to walk a route since it''s unbuilt');
    //DEBUG, should be wrapped somehow for the release
    fViewport.SetCenter(fWalker.GetPosition.X,fWalker.GetPosition.Y);
    fGame.PauseGame(true);
    //Pop-up some message
  end;

  //Execute the route in series of moves
  TimeDelta:=0.1;
  Distance:= TimeDelta * fWalker.Speed;

  //Check if unit has arrived on tile
  if Equals(fWalker.fPosition.X,Nodes[NodePos].X,Distance/2) and Equals(fWalker.fPosition.Y,Nodes[NodePos].Y,Distance/2) then begin

    IsStepDone:=true; //Unit stepped on a new tile

    //Set precise position to avoid rounding errors
    fWalker.fPosition.X:=Nodes[NodePos].X;
    fWalker.fPosition.Y:=Nodes[NodePos].Y;

    //Update unit direction according to next Node 
    if NodePos+1<=NodeCount then begin
      DX := sign(Nodes[NodePos+1].X - Nodes[NodePos].X); //-1,0,1
      DY := sign(Nodes[NodePos+1].Y - Nodes[NodePos].Y); //-1,0,1
      fWalker.Direction:=DirectionsBitfield[DX,DY];
    end;

    //Perform interaction
    if not DoUnitInteraction() then
    begin
      //fWalker gets nulled if DoUnitInteraction creates new path, hence we need to querry KMUnit instead
      DoEnd:=KMUnit.GetUnitType in [ut_Wolf..ut_Duck]; //Animals have no tasks hence they can choose new WalkTo spot no problem
      exit; //Do no further walking until unit interaction is solved
    end;

    inc(NodePos);

    if NodePos>NodeCount then begin
      DoEnd:=true;
      exit;
    end else begin
      fWalker.PrevPosition:=fWalker.NextPosition; ////////////////
      fWalker.NextPosition:=Nodes[NodePos];
      fTerrain.UnitWalk(fWalker.PrevPosition,fWalker.NextPosition); //Pre-occupy next tile
    end;
  end;

  if NodePos>NodeCount then
    Assert(false,'TUnitAction is being overrun for some reason - error!');

  WalkX := Nodes[NodePos].X - fWalker.fPosition.X;
  WalkY := Nodes[NodePos].Y - fWalker.fPosition.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  //fWalker.Direction:=DirectionsBitfield[DX,DY];

  if (DX <> 0) and (DY <> 0) then
    Distance:=Distance / 1.41; {sqrt (2) = 1.41421 }

  fWalker.fPosition.X:= fWalker.fPosition.X + DX*min(Distance,abs(WalkX));
  fWalker.fPosition.Y:= fWalker.fPosition.Y + DY*min(Distance,abs(WalkY));

  inc(fWalker.AnimStep);

  DoesWalking:=true; //Now it's definitely true that unit did walked one step
end;


{ TUnitActionGoIn }
constructor TUnitActionGoIn.Create(aAction: TUnitActionType; aDirection:TGoInDirection; aHouseType:THouseType=ht_None);
begin
  Inherited Create(aAction);
  fDir:=aDirection;
  fHouseType:=aHouseType;
  fHasStarted:=false;
  if fDir=gid_In then fStep:=1  //go Inside (one cell up)
                 else fStep:=0; //go Outside (one cell down)
end;


procedure TUnitActionGoIn.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Distance:single;
begin
  if not fHasStarted then
    fStartX := KMUnit.fPosition.X;

  fHasStarted:=true;
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * KMUnit.Speed;

  if fDir=gid_In then
    KMUnit.Direction:=dir_N  //go Inside (one cell up)
  else
    KMUnit.Direction:=dir_S; //go Outside (one cell down)

  //First step on going inside
  if fStep=1 then begin
    KMUnit.fThought := th_None;
    KMUnit.NextPosition:=KMPoint(KMUnit.GetPosition.X,KMUnit.GetPosition.Y-1);
    fTerrain.UnitWalk(KMUnit.GetPosition,KMUnit.NextPosition);
    if (KMUnit.fHome<>nil)and(KMUnit.fHome.GetHouseType=ht_Barracks) then //Unit home is barracks
      TKMHouseBarracks(KMUnit.fHome).RecruitsInside:=TKMHouseBarracks(KMUnit.fHome).RecruitsInside + 1;
  end;

  //First step on going outside
  if fStep=0 then begin
    KMUnit.NextPosition:=KMPointY1(KMUnit.GetPosition);
    //if fTerrain.Land[KMUnit.NextPosition.Y,KMUnit.NextPosition.X].IsUnit<>0 then exit; //Do not exit if tile is occupied
    fTerrain.UnitWalk(KMUnit.GetPosition,KMUnit.NextPosition);
    if (KMUnit.fHome<>nil)and(KMUnit.fHome.GetHouseType=ht_Barracks) then //Unit home is barracks
      TKMHouseBarracks(KMUnit.fHome).RecruitsInside:=TKMHouseBarracks(KMUnit.fHome).RecruitsInside - 1;
 end;

  fStep := fStep - Distance * shortint(fDir);
  KMUnit.fPosition.Y := KMUnit.fPosition.Y - Distance * shortint(fDir);

  //Attempt at making unit go towards entrance. I couldn't work out the algorithm
  //@Lewin: Try using Mix function, I sketched it for you
  //@Krom: Thanks, I've nearly got it working. I divided the offset by 4 because otherwise they move
  //       too much. I haven't done it for Y yet, just X. I don't know if it is like KaM though, it
  //       is hardly noticable anyway. Are there any house that have a big offset?
  //       What do you think?
  //@Lewin: Don't forget the case when unit is walking out of the house ;)
  //        Y controls position at which unit becomes invisible. So far I cut it at (KMUnit.fVisible := fStep >= 0.3;)
  //        Stables have the biggest offset
  //        I think TUnitActionGoIn should be split into few pieces, so each
  //        of them would fit on one screen (30lines)
  //@Krom:  Sounds like a good idea, I think I'll leave this mess for you to sort out,
  //        I'm not really confident and I don't really understand what I'm doing. :P
  if fHouseType <> ht_None then
    KMUnit.fPosition.X := Mix(fStartX,fStartX + ((HouseDAT[byte(fHouseType)].EntranceOffsetXpx/4)/CELL_SIZE_PX),fStep);

  KMUnit.fVisible := fStep >= 0.3; //Make unit invisible when it's inside of House

  if (fStep<=0)or(fStep>=1) then
  begin
    DoEnd:=true;
    KMUnit.fPosition.X := fStartX;
  end
  else
    inc(KMUnit.AnimStep);
end;


{ TUnitActionStay }
constructor TUnitActionStay.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0);
begin
  Inherited Create(aActionType);
  StayStill:=aStayStill;
  TimeToStay:=aTimeToStay;
  ActionType:=aActionType;
  StillFrame:=aStillFrame;
end;


//If someone whats to know how much time unit has to stay
function TUnitActionStay.HowLongLeftToStay():integer;
begin
  Result:=EnsureRange(TimeToStay,0,maxint);
end;


procedure TUnitActionStay.MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
begin
  //Do not play sounds if unit is invisible to MyPlayer
  if fTerrain.CheckRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y, MyPlayer.PlayerID) < 255 then exit;

  case KMUnit.GetUnitType of //Various UnitTypes and ActionTypes
    ut_Worker: case ActionType of
                 ua_Work:  if Step = 3 then fSoundLib.Play(sfx_housebuild,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_Dig,KMUnit.GetPosition,true);
                 ua_Work2: if Step = 8 then fSoundLib.Play(sfx_pave,KMUnit.GetPosition,true);
               end;
    ut_Farmer: case ActionType of
                 ua_Work:  if Step = 8 then fSoundLib.Play(sfx_corncut,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_cornsow,KMUnit.GetPosition,true,0.8);
               end;
    ut_StoneCutter: if ActionType = ua_Work then
                           if Step = 3 then fSoundLib.Play(sfx_minestone,KMUnit.GetPosition,true,1.4);
    ut_WoodCutter: case ActionType of
                     ua_Work: if (KMUnit.AnimStep mod Cycle = 5) and (KMUnit.Direction <> dir_N) then fSoundLib.Play(sfx_choptree,KMUnit.GetPosition,true)
                     else     if (KMUnit.AnimStep mod Cycle = 0) and (KMUnit.Direction =  dir_N) then fSoundLib.Play(sfx_WoodcutterDig,KMUnit.GetPosition,true);
                   end;
  end;
end;


procedure TUnitActionStay.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Cycle,Step:byte;
begin
  if not StayStill then begin 

    Cycle:=max(UnitSprite[byte(KMUnit.GetUnitType)].Act[byte(ActionType)].Dir[byte(KMUnit.Direction)].Count,1);
    Step:=KMUnit.AnimStep mod Cycle;

    IsStepDone:=KMUnit.AnimStep mod Cycle = 0;

    if TimeToStay >= 1 then MakeSound(KMUnit, Cycle, Step);

    inc(KMUnit.AnimStep);
  end
  else
  begin
    KMUnit.AnimStep:=StillFrame;
    IsStepDone:=true;
  end;

  dec(TimeToStay);
  DoEnd := TimeToStay<=0;
end;


{ TKMUnitsCollection }
{ AutoPlace means should we find a spot for this unit or just place it where we are told.
  Used for creating units still inside schools }
function TKMUnitsCollection.Add(aOwner: TPlayerID; aUnitType: TUnitType; PosX, PosY:integer; AutoPlace:boolean=true):TKMUnit;
var U:Integer; P:TKMPoint;
begin
  if AutoPlace then begin
    P:=FindPlaceForUnit(PosX,PosY,aUnitType);
    PosX:=P.X;
    PosY:=P.Y;
  end;

  if not fTerrain.TileInMapCoords(PosX, PosY) then begin
    fLog.AppendLog('Unable to add unit to '+TypeToString(KMPoint(PosX,PosY)));
    Result:=nil;
    exit;
  end;

  U:=-1;
  case aUnitType of
    ut_Serf:    U:= Inherited Add(TKMUnitSerf.Create(aOwner,PosX,PosY,aUnitType));
    ut_Worker:  U:= Inherited Add(TKMUnitWorker.Create(aOwner,PosX,PosY,aUnitType));

    ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                U:= Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_Recruit: U:= Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_Militia..ut_Barbarian:   U:= Inherited Add(TKMUnitWarrior.Create(aOwner,PosX,PosY,aUnitType));
    //ut_Bowman:   Inherited Add(TKMUnitArcher.Create(aOwner,PosX,PosY,aUnitType)); //I guess it will be stand-alone

    ut_Wolf..ut_Duck:           U:= Inherited Add(TKMUnitAnimal.Create(aOwner,PosX,PosY,aUnitType));

    else
    Assert(false,'Such unit doesn''t exist yet - '+TypeToString(aUnitType));
  end;

  if U=-1 then Result:=nil else Result:=TKMUnit(Items[U]);
end;


function TKMUnitsCollection.AddGroup(aOwner:TPlayerID;  aUnitType:TUnitType; PosX, PosY:integer; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
const DirAngle:array[TKMDirection]of word =   (0,  0,   45,  90,   135, 180,   225, 270,   315);
const DirRatio:array[TKMDirection]of single = (0,  1, 1.41,   1,  1.41,   1,  1.41,   1,  1.41);
var U:TKMUnit; i,x,y,px,py:integer;
begin
  Result:=nil;
  for i:=1 to aUnitCount do begin

    px:=(i-1) mod aUnitPerRow - aUnitPerRow div 2;
    py:=(i-1) div aUnitPerRow;

    x:=round( px*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) - py*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) );
    y:=round( px*DirRatio[aDir]*sin(DirAngle[aDir]/180*pi) + py*DirRatio[aDir]*cos(DirAngle[aDir]/180*pi) );

    U:=Add(aOwner, aUnitType, PosX + x, PosY + y);
    if U<>nil then U.Direction:=aDir; //U will be _nil_ if unit didn't fit on map
    if (U<>nil) and KMSamePoint(U.GetPosition,KMPoint(PosX,PosY)) then begin
      TKMUnitWarrior(U).fIsCommander:=true;
      Result:=U;
    end;
  end;
end;


procedure TKMUnitsCollection.Rem(aUnit:TKMUnit);
begin
  Remove(aUnit);
end;


function TKMUnitsCollection.HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if TKMUnit(Items[I]).HitTest(X, Y, UT) then
    begin
      Result:= TKMUnit(Items[I]);
      Break;
    end;
end;


{Should return closest position where unit can be placed}
function TKMUnitsCollection.FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
var
  aPass:TPassability; //temp for required passability
  Span:integer; //Span length
  X,Y:integer; //Temp position
  mDir:TMoveDirection; //Direction to test
  i:integer;
  function TryOut(aX,aY:integer):boolean;
  begin
    Result:= fTerrain.TileInMapCoords(aX,aY) and fTerrain.CheckPassability(KMPoint(aX,aY),aPass) and (HitTest(aX,aY)=nil);
  end;
begin
  if aUnitType in [ut_Wolf..ut_Duck] then
    aPass:=AnimalTerrain[byte(aUnitType)]
  else
    aPass:=canWalk;

  if TryOut(PosX,PosY) then begin
    Result:=KMPoint(PosX,PosY);
    exit;
  end;

  //Should swirl around input point
  Span:=1; X:=PosX; Y:=PosY; mDir:=TMoveDirection(3);
  repeat
    mDir:=TMoveDirection((byte(mDir)+1)mod 4); //wrap around
    case mDir of
      mdPosX: for i:=X+1 to     X+Span do begin inc(X); if TryOut(X,Y) then break; end;
      mdPosY: for i:=Y+1 to     Y+Span do begin inc(Y); if TryOut(X,Y) then break; end;
      mdNegX: for i:=X-1 downto X-Span do begin dec(X); if TryOut(X,Y) then break; end;
      mdNegY: for i:=Y-1 downto Y-Span do begin dec(Y); if TryOut(X,Y) then break; end;
    end;
    if mDir in [mdPosY,mdNegY] then inc(Span); //increase span every second turn
  until(TryOut(X,Y) or (Span=10)); //Catch the end

  Result:=KMPoint(X,Y);
end;


procedure TKMUnitsCollection.GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
var i:integer;
begin
  Loc.Clearup;
  for I := 0 to Count - 1 do
    if (TKMUnit(Items[I]).fOwner=aOwner)and not(TKMUnit(Items[I]).fUnitType in [ut_Wolf..ut_Duck]) then
      Loc.AddEntry(TKMUnit(Items[I]).GetPosition);
end;


procedure TKMUnitsCollection.Paint();
var i:integer; x1,x2,y1,y2,Margin:integer;
begin
  if TestViewportClipInset then Margin:=-3 else Margin:=3;
  x1:=fViewport.GetClip.Left-Margin;  x2:=fViewport.GetClip.Right+Margin;
  y1:=fViewport.GetClip.Top -Margin;  y2:=fViewport.GetClip.Bottom+Margin;

  for I := 0 to Count - 1 do
  if (InRange(TKMUnit(Items[I]).fPosition.X,x1,x2) and InRange(TKMUnit(Items[I]).fPosition.Y,y1,y2)) then
    TKMUnit(Items[I]).Paint();
end;


procedure TKMUnitsCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMUnit(Items[I]).UpdateState;

  //After all units are updated we can safely remove those that died.  
  for I := Count - 1 downto 0 do
    if TKMUnit(Items[I]).ScheduleForRemoval then begin
      TKMUnit(Items[I]).Free;
      Rem(TKMUnit(Items[I]));
    end;

end;

end.
