unit KM_Units;
interface
uses
  KM_Defaults, windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Houses, KromUtils;

type
  //Switch to set if unit goes into house or out of it
  TGoInDirection = (gid_In=1, gid_Out=-1);

  TKMUnit = class;
  TKMUnitSerf = class;
  TKMUnitWorker = class;

  TUnitWorkPlan = class
  private
    Issued:boolean;
    Loc:TKMPoint;
    WalkTo:TUnitActionType;
    WorkType:TUnitActionType;
    WorkCyc:integer;
    GatheringScript:TGatheringScript;
    AfterWorkDelay:integer;
    WalkFrom:TUnitActionType;
    Resource1:TResourceType; Count1:byte;
    Resource2:TResourceType; Count2:byte;
    ActCount:byte;
    HouseAct:array[1..16] of record
      Act:THouseActionType;
      TimeToWork:byte;
    end;
    Product:TResourceType;
    ProductCount:byte;
  procedure FillDefaults();
  procedure FindPlan(aUnitType:TUnitType; aHome:THouseType; aProduct:TResourceType; aLoc:TKMPoint);
  end;

  TUnitAction = class(TObject)
  private
    fActionType: TUnitActionType;
  public
    constructor Create(aActionType: TUnitActionType);
    procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); virtual; abstract;
    property ActionType: TUnitActionType read fActionType;
  end;

      {Simple movement between cells}
      //Obsolete

      {Walk to somewhere}
      TUnitActionWalkTo = class(TUnitAction)
      private
        fDestPos: TKMPoint;
        fRouteBuilt:boolean;
        NodeCount:integer;
        Nodes:array[1..1024] of TKMPoint;
        NodePos:integer;
      public
        constructor Create(LocA,LocB:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      {This is a simple action making unit go inside/outside of house}
      TUnitActionGoIn = class(TUnitAction)
      private
        fStep:single;
        fDir:shortint;
      public
        constructor Create(aAction: TUnitActionType; aDirection:TGoInDirection);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      {Stay in place for set time}
      TUnitActionStay = class(TUnitAction)
      private
      StayStill:boolean;
      TimeToStay:integer;
       public
        constructor Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
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
      Phase:byte;
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
    fResource:TResourceType;
    ID:integer;
    public
    constructor Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
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
      fInn:TKMHouse;
    public
    constructor Create(aInn:TKMHouse; aUnit:TKMUnit);
    procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskMining = class(TUnitTask)
    private
      WorkPlan:TUnitWorkPlan;
      fUnit:TKMUnit;
    public
    constructor Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit; aHouse:TKMHouse);
    procedure Execute(out TaskDone:boolean); override;
    end;

  TKMUnit = class(TObject)
  private
    Speed:single;
    fOwner:TPlayerID;
    fHome:TKMHouse;
    fCurrentAction: TUnitAction;
    fPosition: TKMPointF;
    fLastUpdateTime: Cardinal;
    fUnitType: TUnitType;
    AnimStep: integer;
    fVisible:boolean;
    fUnitTask:TUnitTask;
    fCondition:integer; //Unit condition, when it reaches zero unit should die
    //function UnitAtHome():boolean; Test if Unit is invisible and Pos matches fHome.GetEntrance
  public
    Direction: TKMDirection;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    destructor Destroy; override;
    function GetSupportedActions: TUnitActionTypeSet; virtual;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
    procedure SetAction(aAction: TUnitAction; aStep:integer=0);
    property UnitTask: TUnitTask write fUnitTask;
    property GetUnitType: TUnitType read fUnitType;
    function GetUnitTaskText():string;
    property GetCondition: integer read fCondition;
    property IsVisible: boolean read fVisible;
    //property IsAtHome: boolean read UnitAtHome;
    function GetPosition():TKMPoint;
    procedure UpdateState; virtual;
    procedure Paint; virtual; abstract;
  end;

  //This is a common class for units going out of their homes for resources
  TKMUnitCitizen = class(TKMUnit)
  public
    WorkPlan:TUnitWorkPlan;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    function FindHome():boolean;
    procedure UpdateState; override;
    function InitiateMining():TUnitTask;
    procedure Paint(); override;
  end;

  //Serf class - transports all goods ingame between houses
  TKMUnitSerf = class(TKMUnit)
    Carry: TResourceType;
  public
    procedure UpdateState; override;
    procedure Paint(); override;
    function GiveResource(Res:TResourceType):boolean;
    function TakeResource(Res:TResourceType):boolean;
    function GetActionFromQueue():TUnitTask;
  end;

  //Worker class - builds everything ingame
  TKMUnitWorker = class(TKMUnit)
  public
    procedure UpdateState; override;
    procedure Paint(); override;
    function GetActionFromQueue():TUnitTask;
  end;

  //Possibly melee warrior class? with Archer class separate?
  TKMUnitWarrior = class(TKMUnit)
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure UpdateState; override;
    procedure Paint(); override;
  end;

  TKMUnitsCollection = class(TKMList)
  public
    function Add(aOwner:TPlayerID;  aUnitType:TUnitType; PosX, PosY:integer):TKMUnit;
    procedure UpdateState;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    procedure GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
    procedure Paint();
  end;

implementation
uses KM_Unit1, KM_Render, KM_DeliverQueue, KM_Users, KM_Settings;


{Whole thing should be moved to units Task}
{Houses are only a place on map, they should not issue or perform tasks (except Training)}
{Everything should be issued by units!}
{Where to go, which walking style, what to do on location, for how long}
{How to go back in case success, incase bad luck}
{What to take from supply, how much, take2, much2}
{What to do, Work/Cycles, What resource to add to Output, how much}
{E.g. CoalMine: Miner arrives at home and Idles for 5sec, then takes a work task (depending on ResOut count)
Since Loc is 0,0 he immidietely skips to Phase X where he switches house to Work1 (and self busy for same framecount)
Then Work2 and Work3 same way. Then adds resource to out and everything to Idle for 5sec.}
{E.g. Farmer arrives at home and Idles for 5sec, then takes a work task (depending on ResOut count, HouseType and Need to sow corn)
...... then switches house to Work1 (and self busy for same framecount)
Then Work2 and Work3 same way. Then adds resource to out and everything to Idle for 5sec.}
procedure TUnitWorkPlan.FillDefaults();
begin
  Issued:=true;
  Loc:=KMPoint(0,0);
  WalkTo:=ua_Walk;
  WorkType:=ua_Work;
  WorkCyc:=0;
  GatheringScript:=gs_None;
  AfterWorkDelay:=0;
  WalkFrom:=ua_Walk;
  Resource1:=rt_None; Count1:=0;
  Resource2:=rt_None; Count2:=0;
  ActCount:=0;
  Product:=rt_None;
  ProductCount:=0;
end;

procedure TUnitWorkPlan.FindPlan(aUnitType:TUnitType; aHome:THouseType; aProduct:TResourceType; aLoc:TKMPoint);
  procedure WalkStyle(aLoc:TKMPoint; aTo,aWork:TUnitActionType; aCycles,aDelay:byte; aFrom:TUnitActionType; aScript:TGatheringScript); overload;
  begin
    Loc:=aLoc;
    WalkTo:=aTo;
    WorkType:=aWork;
    WorkCyc:=aCycles;
    AfterWorkDelay:=aDelay;
    GatheringScript:=aScript;
    WalkFrom:=aFrom;
  end;
  procedure SubActAdd(aAct:THouseActionType; aCycles:byte);
  begin
    inc(ActCount); HouseAct[ActCount].Act:=aAct;
    HouseAct[ActCount].TimeToWork:=HouseDAT[byte(aHome)].Anim[byte(aAct)].Count * aCycles;
  end;
  procedure ResourcePlan(Res1:TResourceType; Qty1:byte; Res2:TResourceType; Qty2:byte; Prod:TResourceType);
  begin
    Resource1:=Res1; Count1:=Qty1;
    Resource2:=Res2; Count2:=Qty2;
    Product:=Prod;
    ProductCount:=HouseDAT[byte(aHome)].ResProductionX;
  end;
begin
FillDefaults;
//Now we need to fill only specific properties
if (aUnitType=ut_LamberJack)and(aHome=ht_SawMill) then begin
  ResourcePlan(rt_Trunk,1,rt_None,0,rt_Wood);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Miner)and(aHome=ht_CoalMine) then begin
  ResourcePlan(rt_None,0,rt_None,0,rt_Coal);
  GatheringScript:=gs_CoalMiner;
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Miner)and(aHome=ht_IronMine) then begin
  ResourcePlan(rt_None,0,rt_None,0,rt_IronOre);
  GatheringScript:=gs_IronMiner;
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Miner)and(aHome=ht_GoldMine) then begin
  ResourcePlan(rt_None,0,rt_None,0,rt_GoldOre);
  GatheringScript:=gs_GoldMiner;
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Metallurgist)and(aHome=ht_IronSmithy) then begin
  ResourcePlan(rt_IronOre,1,rt_Coal,1,rt_Steel);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
end else
if (aUnitType=ut_Metallurgist)and(aHome=ht_Metallurgists) then begin
  ResourcePlan(rt_GoldOre,1,rt_Coal,1,rt_Gold);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
  SubActAdd(ha_Work4,2);
end else
if (aUnitType=ut_Smith)and(aHome=ht_ArmorSmithy)and(aProduct=rt_MetalArmor) then begin
  ResourcePlan(rt_Steel,1,rt_Coal,1,rt_MetalArmor);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work2,2);
end else
if (aUnitType=ut_Smith)and(aHome=ht_ArmorSmithy)and(aProduct=rt_MetalShield) then begin
  ResourcePlan(rt_Steel,1,rt_Coal,1,rt_MetalShield);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work2,2);
end else
if (aUnitType=ut_Smith)and(aHome=ht_WeaponSmithy)and(aProduct=rt_Sword) then begin
  ResourcePlan(rt_Steel,1,rt_Coal,1,rt_Sword);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Smith)and(aHome=ht_WeaponSmithy)and(aProduct=rt_Hallebard) then begin
  ResourcePlan(rt_Steel,1,rt_Coal,1,rt_Hallebard);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Smith)and(aHome=ht_WeaponSmithy)and(aProduct=rt_Arbalet) then begin
  ResourcePlan(rt_Steel,1,rt_Coal,1,rt_Arbalet);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Lamberjack)and(aHome=ht_ArmorWorkshop)and(aProduct=rt_Armor) then begin
  ResourcePlan(rt_Leather,1,rt_None,0,rt_Armor);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
end else
if (aUnitType=ut_Lamberjack)and(aHome=ht_ArmorWorkshop)and(aProduct=rt_Shield) then begin
  ResourcePlan(rt_Wood,1,rt_None,0,rt_Shield);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
end else
if (aUnitType=ut_Lamberjack)and(aHome=ht_WeaponWorkshop)and(aProduct=rt_Axe) then begin
  ResourcePlan(rt_Wood,2,rt_None,0,rt_Axe);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Lamberjack)and(aHome=ht_WeaponWorkshop)and(aProduct=rt_Pike) then begin
  ResourcePlan(rt_Wood,2,rt_None,0,rt_Pike);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Lamberjack)and(aHome=ht_WeaponWorkshop)and(aProduct=rt_Bow) then begin
  ResourcePlan(rt_Wood,2,rt_None,0,rt_Bow);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
  SubActAdd(ha_Work5,1);
end else
if (aUnitType=ut_Baker)and(aHome=ht_Mill) then begin
  ResourcePlan(rt_Corn,1,rt_None,0,rt_Flour);
  SubActAdd(ha_Work2,12);
end else
if (aUnitType=ut_Baker)and(aHome=ht_Bakery) then begin
  ResourcePlan(rt_Flour,1,rt_None,0,rt_Bread);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
end else
if (aUnitType=ut_Farmer)and(aHome=ht_Farm) then begin
  if fTerrain.FindCorn(aLoc,12).X<>0 then begin
    ResourcePlan(rt_None,0,rt_None,0,rt_Corn);
    WalkStyle(fTerrain.FindCorn(aLoc,12),ua_WalkTool,ua_Work,6,0,ua_WalkBooty,gs_FarmerCorn);
  end else
  if fTerrain.FindCornField(aLoc,12).X<>0 then
    WalkStyle(fTerrain.FindCornField(aLoc,12),ua_Walk,ua_Work1,6,0,ua_Walk,gs_FarmerSow)
  else
    Issued:=false;
end else
if (aUnitType=ut_Farmer)and(aHome=ht_Wineyard) then begin
  if fTerrain.FindGrapes(aLoc,12).X<>0 then begin
    ResourcePlan(rt_None,0,rt_None,0,rt_Wine);
    WalkStyle(fTerrain.FindGrapes(aLoc,12),ua_WalkTool2,ua_Work2,6,0,ua_WalkBooty2,gs_FarmerWine);
    SubActAdd(ha_Work1,1);
    SubActAdd(ha_Work2,8);
    SubActAdd(ha_Work5,1);
  end else
    Issued:=false;
end else
if (aUnitType=ut_StoneCutter)and(aHome=ht_Quary) then begin
  if fTerrain.FindStone(aLoc,12).X<>0 then begin
    ResourcePlan(rt_None,0,rt_None,0,rt_Stone);
    WalkStyle(fTerrain.FindStone(aLoc,12),ua_Walk,ua_Work,6,10,ua_WalkTool,gs_StoneCutter);
    SubActAdd(ha_Work1,1);
    SubActAdd(ha_Work2,8);
    SubActAdd(ha_Work5,1);
  end else
    Issued:=false;
end else
if (aUnitType=ut_WoodCutter)and(aHome=ht_Woodcutters) then begin
  if fTerrain.FindTree(aLoc,12).X<>0 then begin
    ResourcePlan(rt_None,0,rt_None,0,rt_Trunk);
    WalkStyle(fTerrain.FindTree(aLoc,12),ua_WalkBooty,ua_Work,6,10,ua_WalkTool2,gs_WoodCutterCut);
  end else
  if fTerrain.FindPlaceForTree(aLoc,12).X<>0 then
    WalkStyle(fTerrain.FindPlaceForTree(aLoc,12),ua_WalkTool,ua_Work1,6,0,ua_Walk,gs_WoodCutterPlant)
  else
    Issued:=false;
end else
if (aUnitType=ut_Butcher)and(aHome=ht_Tannery) then begin
  ResourcePlan(rt_Skin,1,rt_None,0,rt_Leather);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,6);
end else
if (aUnitType=ut_Butcher)and(aHome=ht_Butchers) then begin
  ResourcePlan(rt_Pig,1,rt_None,0,rt_Sousages);
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,1);
  SubActAdd(ha_Work3,1);
  SubActAdd(ha_Work4,1);
end else
if (aUnitType=ut_Recruit)and(aHome=ht_Barracks) then begin
  Issued:=false; //Let him idle
end else
  Assert(false,'There''s yet no working plan for '+TypeToString(aUnitType)+' in '+TypeToString(aHome));
end;


{ TKMUnitCitizen }
constructor TKMUnitCitizen.Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
Inherited;
WorkPlan:=TUnitWorkPlan.Create;
end;


function TKMUnitCitizen.FindHome():boolean;
var KMHouse:TKMHouse;
begin
Result:=false;
KMHouse:=ControlList.FindEmptyHouse(fUnitType);
if KMHouse<>nil then begin fHome:=KMHouse; Result:=true; end;
end;


procedure TKMUnitCitizen.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
if not fVisible then exit;
UnitType:=byte(fUnitType);
AnimAct:=byte(fCurrentAction.fActionType);
AnimDir:=byte(Direction);

  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,TUnitActionWalkTo(fCurrentAction).Nodes,$FF00FFFF);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
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
end;


procedure TKMUnitCitizen.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  Inherited;
  DoEnd:=true;
  TaskDone:=true;
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if fUnitTask <> nil then
    fUnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if fUnitTask <> nil then begin
    fUnitTask.Free;
    fUnitTask:=nil
  end;

//Here come unit tasks in priorities
//Priority no.1 - find self a food
//Priority no.2 - find self a home
//Priority no.3 - find self a work
    if fCondition<1200 then
      if ControlList.FindHouse(ht_Inn,round(fPosition.X),round(fPosition.Y))<>nil then
        fUnitTask:=TTaskGoEat.Create(ControlList.FindHouse(ht_Inn,round(fPosition.X),round(fPosition.Y)),Self)
      else
        //StayStillAndDieSoon
    else
    if (fHome=nil) then
      if FindHome then
        fUnitTask:=TTaskGoHome.Create(fHome.GetEntrance,Self)
      else
        SetAction(TUnitActionStay.Create(120, ua_Walk))
    else
    //if not at home - go home
      fUnitTask:=InitiateMining;

if fUnitTask=nil then SetAction(TUnitActionStay.Create(120, ua_Walk));
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

WorkPlan.FindPlan(fUnitType,fHome.GetHouseType,HouseOutput[byte(fHome.GetHouseType),Res],GetPosition);

if not WorkPlan.Issued then exit;
if (WorkPlan.Resource1<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource1)<WorkPlan.Count1) then exit;
if (WorkPlan.Resource2<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource2)<WorkPlan.Count2) then exit;
if fHome.CheckResOut(WorkPlan.Product)>=MaxResInHouse then exit;

if HousePlaceOrders[byte(fHome.GetHouseType)] then
  fHome.RemOrder(Res);

Result:=TTaskMining.Create(WorkPlan,Self,fHome);
end;


{ TKMSerf }
procedure TKMUnitSerf.Paint();
var AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,TUnitActionWalkTo(fCurrentAction).Nodes,$FFFF00FF);

  fRender.RenderUnit(byte(GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);

  if Carry<>rt_None then
    fRender.RenderUnitCarry(integer(Carry), AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1)
  else
    fRender.RenderUnit(byte(GetUnitType), 9, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,false);
end;


procedure TKMUnitSerf.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  Inherited;
  DoEnd:=true;
  TaskDone:=true;
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if fUnitTask <> nil then
    fUnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if fUnitTask <> nil then begin
    fUnitTask.Free;
    fUnitTask:=nil
  end;

  fUnitTask:=GetActionFromQueue;

  if fUnitTask=nil then SetAction(TUnitActionStay.Create(10,ua_Walk)); //Stay idle
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
Carry:=rt_None;
Result:=true;
end;


function TKMUnitSerf.GetActionFromQueue():TUnitTask;
begin
Result:=ControlList.DeliverList.AskForDelivery(Self);
end;


{ TKMWorker }
procedure TKMUnitWorker.Paint();
var AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;

  if fCurrentAction is TUnitActionWalkTo then
    fRender.RenderRoute(TUnitActionWalkTo(fCurrentAction).NodeCount,TUnitActionWalkTo(fCurrentAction).Nodes,$FFFFFFFF);

  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(byte(GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
end;


procedure TKMUnitWorker.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  Inherited;
  DoEnd:=true;
  TaskDone:=true;
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if fUnitTask <> nil then
    fUnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if fUnitTask <> nil then begin
    fUnitTask.Free;
    fUnitTask:=nil
  end;

  fUnitTask:=GetActionFromQueue;

  if fUnitTask=nil then SetAction(TUnitActionStay.Create(20,ua_Walk));
end;


function TKMUnitWorker.GetActionFromQueue():TUnitTask;
begin
Result:=ControlList.BuildList.AskForHousePlan(Self,Self.GetPosition);
if Result=nil then
Result:=ControlList.BuildList.AskForHouse(Self,Self.GetPosition);
if Result=nil then
Result:=ControlList.BuildList.AskForRoad(Self,Self.GetPosition);
end;


{ TKMwarrior }
function TKMUnitWarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;


procedure TKMUnitWarrior.Paint();
var AnimAct,AnimDir:integer;
begin
AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
AnimDir:=integer(Direction);
fRender.RenderUnit(byte(Self.GetUnitType), AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1,true);
end;


procedure TKMUnitWarrior.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  Inherited;
  DoEnd:=true;
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if DoEnd then
    SetAction(TUnitActionStay.Create(50,ua_Walk))
end;

{ TKMUnit }

constructor TKMUnit.Create(const aOwner:TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited Create;
  fHome:=nil;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fOwner:= aOwner;
  fUnitType:=aUnitType;
  Direction:=dir_S;
  fVisible:=true;
  Speed:=UnitStat[byte(aUnitType)].Speed/24;
  SetAction(TUnitActionStay.Create(10,ua_Walk));
  fCondition:=UNIT_MAX_CONDITION;
  fMissionSettings.CreatedUnit(fUnitType);
end;

destructor TKMUnit.Destroy;
begin
  //Abandon delivery or other current task
  //Abandon home
  //if fUnitTask is TTaskDeliver then ControlList.DeliverList.AbandonDelivery(TTaskDeliver(fUnitTask).ID);
  //fHome.GetHasOwner:=false;
  fCurrentAction.Free;
  fUnitTask.Free;
  fMissionSettings.DestroyedUnit(fUnitType);
  Inherited;
end;

function TKMUnit.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= UnitSupportedActions[integer(fUnitType)];
end;

function TKMUnit.GetPosition():TKMPoint;
begin
  Result.X:=round(fPosition.X);
  Result.Y:=round(fPosition.Y);
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

function TKMUnit.HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
begin
  Result:= (X = round(fPosition.X)) and (Y = round(fPosition.Y)) and ((fUnitType=UT)or(UT=ut_Any));
end;

procedure TKMUnit.SetAction(aAction: TUnitAction; aStep:integer=0);
begin
AnimStep:=aStep;
  if aAction = nil then
  begin
    fCurrentAction.Free;
    fCurrentAction:= nil;
    Exit;
  end;
  if not (aAction.ActionType in GetSupportedActions) then
  begin
    aAction.Free;
    exit;
  end;
  if fCurrentAction <> aAction then
  begin
    fCurrentAction.Free;
    fCurrentAction:= aAction;
  end;
end;


procedure TKMUnit.UpdateState();
begin
//dec(fCondition); //Disabled for now
//if fCondition<=0 then Die
end;

{ TTaskSelfTrain }
{Train itself in school}
constructor TTaskSelfTrain.Create(aUnit:TKMUnit; aSchool:TKMHouseSchool);
begin
  fUnit:=aUnit;
  fSchool:=aSchool;
  Phase:=0;
  fUnit.fVisible:=false;
end;


procedure TTaskSelfTrain.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case Phase of
  0: begin
      fSchool.SetState(hst_Work,0);
      fSchool.fCurrentAction.SubActionWork(ha_Work1,30);
      SetAction(TUnitActionStay.Create(29,ua_Walk));
    end;
  1: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work2,30);
      SetAction(TUnitActionStay.Create(29,ua_Walk));
    end;
  2: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work3,30);
      SetAction(TUnitActionStay.Create(29,ua_Walk));
    end;
  3: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work4,30);
      SetAction(TUnitActionStay.Create(29,ua_Walk));
    end;
  4: begin
      fSchool.fCurrentAction.SubActionWork(ha_Work5,30);
      SetAction(TUnitActionStay.Create(29,ua_Walk));
    end;
  5: begin
      fSchool.SetState(hst_Idle,10);
      SetAction(TUnitActionStay.Create(9,ua_Walk));
     end;
  6: begin
      SetAction(TUnitActionGoIn.Create(ua_Walk,gid_Out));
      fSchool.UnitIsTrained;
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
fResource:=Res;
Phase:=0;
ID:=aID;
end;

procedure TTaskDeliver.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

with fSerf do
case Phase of
0: SetAction(TUnitActionWalkTo.Create(fSerf.GetPosition,KMPointY1(fFrom.GetEntrance)));
1: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
2: SetAction(TUnitActionStay.Create(5,ua_Walk));
3: begin
     if fFrom.ResTakeFromOut(fResource) then
       GiveResource(fResource)
     else
     begin
       SetAction(TUnitActionGoIn.Create(ua_Walk,gid_Out));
       ControlList.DeliverList.CloseDelivery(ID);
       TaskDone:=true;
     end;
   end;
4: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_Out));
end;

//Deliver into complete house
if fToHouse<>nil then
if fToHouse.IsComplete then
with fSerf do
case Phase of
5: SetAction(TUnitActionWalkTo.Create(fSerf.GetPosition,KMPointY1(fToHouse.GetEntrance)));
6: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
7: SetAction(TUnitActionStay.Create(5,ua_Walk));
8: begin
     fToHouse.ResAddToIn(Carry);
     TakeResource(Carry);
     SetAction(TUnitActionGoIn.Create(ua_walk,gid_Out));
     ControlList.DeliverList.CloseDelivery(ID);
   end;
9: TaskDone:=true;
end;

//Deliver into wip house
if fToHouse<>nil then
if not fToHouse.IsComplete then
with fSerf do
case Phase of
5: SetAction(TUnitActionWalkTo.Create(fSerf.GetPosition,KMPointY1(fToHouse.GetEntrance)));
6: begin
     fToHouse.ResAddToBuild(Carry);
     TakeResource(Carry);
     ControlList.DeliverList.CloseDelivery(ID);
   end;
7: TaskDone:=true;
end;

//Deliver to builder
if fToUnit<>nil then
with fSerf do
case Phase of
5: SetAction(TUnitActionWalkTo.Create(fSerf.GetPosition,KMPointY1(fToUnit.fPosition)));
6: begin
     TakeResource(Carry);
     inc(fToUnit.fUnitTask.Phase);
     fToUnit.SetAction(TUnitActionStay.Create(0,ua_Work1));
   end;
7: ControlList.DeliverList.CloseDelivery(ID);
8: TaskDone:=true;
end;
inc(Phase);
end;

{ TTaskBuildRoad }
constructor TTaskBuildRoad.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
  fWorker:=aWorker;
  fLoc:=aLoc;
  Phase:=0;
  ID:=aID;
end;

procedure TTaskBuildRoad.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
0: SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,fLoc));
1: begin
   fTerrain.RemMarkup(fLoc);
   SetAction(TUnitActionStay.Create(11,ua_Work1,false));
   end;
2: begin
   fTerrain.IncFieldState(fLoc);
   SetAction(TUnitActionStay.Create(11,ua_Work1,false));
   end;
3: begin
   fTerrain.IncFieldState(fLoc);
   SetAction(TUnitActionStay.Create(11,ua_Work1,false));
   end;
4: ControlList.DeliverList.AddNewDemand(nil, fWorker, rt_Stone, dt_Once, di_High);

5: SetAction(TUnitActionStay.Create(30,ua_Work1));
6: begin
   SetAction(TUnitActionStay.Create(11,ua_Work2,false));
   end;
7: begin
   fTerrain.IncFieldState(fLoc);
   SetAction(TUnitActionStay.Create(11,ua_Work2,false));
   end;
8: begin
   fTerrain.IncFieldState(fLoc);
   SetAction(TUnitActionStay.Create(11,ua_Work2,false));
   end;
9: begin
   fTerrain.SetField(fLoc,fOwner,fdt_Road);
   ControlList.BuildList.CloseRoad(ID);
   SetAction(TUnitActionStay.Create(5,ua_Work2));
   end;
10:TaskDone:=true;
end;
if Phase<>5 then inc(Phase); //Phase=5 is when worker waits for rt_Stone
end;

{ TTaskBuildWine }
constructor TTaskBuildWine.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
fWorker:=aWorker;
fLoc:=aLoc;
Phase:=0;
ID:=aID;
end;

procedure TTaskBuildWine.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
0: SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,fLoc));
1: fTerrain.RemMarkup(fLoc);
2: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
3: fTerrain.IncFieldState(fLoc);
4: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
5: fTerrain.IncFieldState(fLoc);
6: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
7: ControlList.DeliverList.AddNewDemand(nil,fWorker,rt_Wood, dt_Once, di_High);

8: SetAction(TUnitActionStay.Create(30,ua_Work1));

9: SetAction(TUnitActionStay.Create(44,ua_Work2,false));
10:begin
    fTerrain.SetField(fLoc,fOwner,fdt_Wine);
    fTerrain.CutGrapes(fLoc);
    ControlList.BuildList.CloseRoad(ID);
   end;
11:TaskDone:=true;
end;
if Phase<>8 then inc(Phase); //Phase=8 is when worker waits for rt_Stone
end;

{ TTaskBuildField }
constructor TTaskBuildField.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aID:integer);
begin
fWorker:=aWorker;
fLoc:=aLoc;
Phase:=0;
ID:=aID;
end;

procedure TTaskBuildField.Execute(out TaskDone:boolean);
const Cycle=11;
begin
TaskDone:=false;
with fWorker do
case Phase of
  0: SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,fLoc));
  1: fTerrain.RemMarkup(fLoc);
  2: SetAction(TUnitActionStay.Create(44,ua_Work1,false));
  3: fTerrain.IncFieldState(fLoc);
  4: SetAction(TUnitActionStay.Create(44,ua_Work1,false));
  5: fTerrain.IncFieldState(fLoc);
  6: SetAction(TUnitActionStay.Create(22,ua_Work1,false));
  7: begin
       ControlList.BuildList.CloseRoad(ID);
       fTerrain.SetField(fLoc,fOwner,fdt_Field);
     end;
  8: SetAction(TUnitActionStay.Create(5,ua_Work1));
  9: TaskDone:=true;
end;
inc(Phase);
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
end;

{Prepare building site - flatten terrain}
procedure TTaskBuildHouseArea.Execute(out TaskDone:boolean);
var i:integer;
begin
TaskDone:=false;
with fWorker do
case Phase of
0:  SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,fHouse.GetEntrance));
1:  begin
      fTerrain.SetHousePlan(fHouse.GetPosition, fHouse.GetHouseType, fdt_HouseWIP);
      fHouse.SetBuildingState(hbs_NoGlyph);
    end;
2:  SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,ListOfCells[Step]));
3:  begin
      SetAction(TUnitActionStay.Create(11,ua_Work1,false));
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
4:  begin
      SetAction(TUnitActionStay.Create(11,ua_Work1,false));
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
5:  begin
      SetAction(TUnitActionStay.Create(11,ua_Work1,false));
      fTerrain.FlattenTerrain(ListOfCells[Step]);
    end;
6:  begin
      SetAction(TUnitActionStay.Create(11,ua_Work1,false));
      fTerrain.FlattenTerrain(ListOfCells[Step]); dec(Step);
    end;
7:  begin
      ControlList.BuildList.CloseHousePlan(TaskID);
      fHouse.SetBuildingState(hbs_Wood);
      ControlList.BuildList.AddNewHouse(fHouse); //Add the house to JobList, so then all workers could take it
      for i:=1 to HouseDAT[byte(fHouse.GetHouseType)].WoodCost do
        ControlList.DeliverList.AddNewDemand(fHouse,nil,rt_Wood,dt_Once,di_Norm);
      for i:=1 to HouseDAT[byte(fHouse.GetHouseType)].StoneCost do
        ControlList.DeliverList.AddNewDemand(fHouse,nil,rt_Stone,dt_Once,di_Norm);
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
           //and look for new task that has enough resouce. Once this house has resource delivered
           //it will be available to be built again.
           if not fHouse.CheckResToBuild then begin
             TaskDone:=true; //Drop the task
             exit;
           end;
           CurLoc:=Random(LocCount)+1;
           SetAction(TUnitActionWalkTo.Create(fWorker.GetPosition,Cells[CurLoc].Loc));
         end;
      1: begin
           Direction:=Cells[CurLoc].Dir;
           if not fHouse.IsStarted then fTerrain.SetHousePlan(fHouse.GetPosition, fHouse.GetHouseType, fdt_House);
         end;
      2: begin
           SetAction(TUnitActionStay.Create(5,ua_Work,false),0); //Start animation
           Direction:=Cells[CurLoc].Dir;
         end;
      3: begin
           //Cancel building no matter progress if resource depleted
           if not fHouse.CheckResToBuild then begin
             TaskDone:=true; //Drop the task
             exit;
           end;
           fHouse.IncBuildingProgress;
           SetAction(TUnitActionStay.Create(6,ua_Work,false),5); //Do building and end animation
           inc(Phase2);
         end;
      4: begin
           ControlList.BuildList.CloseHouse(TaskID);
           TaskDone:=true;
         end;
    end;
  inc(Phase);
  if (Phase=4) and (not fHouse.IsComplete) then //If animation cycle is done
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
end;

{This is execution of Resource mining}
procedure TTaskMining.Execute(out TaskDone:boolean);
const SkipWalk=6; SkipWork=11; //Skip to certain Phases
var Dir:integer; TimeToWork:integer;
begin
TaskDone:=false;
with fUnit do
  case Phase of
    0: if WorkPlan.Loc.X=0 then begin
         Phase:=SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
         exit;
       end else begin
         fHome.SetState(hst_Empty,0);
         SetAction(TUnitActionGoIn.Create(WorkPlan.WalkTo,gid_Out)); //Walk outside the house
       end;
    1: SetAction(TUnitActionWalkTo.Create(fUnit.GetPosition,WorkPlan.Loc,WorkPlan.WalkTo));
    2: //IF resource still exists on location
       begin //Choose direction and time to work
         Dir:=integer(fUnit.Direction);
         if UnitSprite[integer(fUnit.fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count<=1 then
           for Dir:=1 to 8 do
             if UnitSprite[integer(fUnit.fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count>1 then break;
         Dir:=min(Dir,8);
         fUnit.Direction:=TKMDirection(Dir);
         TimeToWork:=WorkPlan.WorkCyc*max(UnitSprite[integer(fUnit.fUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count,1);
         SetAction(TUnitActionStay.Create(TimeToWork, WorkPlan.WorkType, false));
       end;
    3: begin case WorkPlan.GatheringScript of //Perform special tasks if required
               gs_FarmerSow:   fTerrain.InitGrowth(WorkPlan.Loc);
               gs_FarmerCorn:  fTerrain.CutCorn(WorkPlan.Loc);
               gs_FarmerWine:  fTerrain.CutGrapes(WorkPlan.Loc);
               gs_WoodCutterPlant: fTerrain.AddTree(WorkPlan.Loc,ChopableTrees[Random(length(ChopableTrees))+1,1]);
               gs_WoodCutterCut:   fTerrain.ChopTree(WorkPlan.Loc);
             end;
         SetAction(TUnitActionStay.Create(WorkPlan.AfterWorkDelay, WorkPlan.WorkType));
       end;
    4: SetAction(TUnitActionWalkTo.Create(fUnit.GetPosition,KMPointY1(fHome.GetEntrance),WorkPlan.WalkFrom)); //Go home
    5: SetAction(TUnitActionGoIn.Create(WorkPlan.WalkFrom,gid_In)); //Go inside

    {Unit back at home and can process its booty now}
    6: begin
        fHome.SetState(hst_Work,0); //Set house to Work state
        fHome.ResTakeFromIn(WorkPlan.Resource1); //Count should be added
        fHome.ResTakeFromIn(WorkPlan.Resource2);
        fHome.fCurrentAction.SubActionAdd([ha_Smoke]);
       end;
    7: if WorkPlan.ActCount>=1 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[1].Act,WorkPlan.HouseAct[1].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[1].TimeToWork,ua_Walk)); //Keepp unit idling till next Phase
       end else begin Phase:=SkipWork; exit; end;
    8: if WorkPlan.ActCount>=2 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[2].Act,WorkPlan.HouseAct[2].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[2].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    9:if WorkPlan.ActCount>=3 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[3].Act,WorkPlan.HouseAct[3].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[3].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    10:if WorkPlan.ActCount>=4 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[4].Act,WorkPlan.HouseAct[4].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[4].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    11: begin
          case WorkPlan.GatheringScript of
            gs_CoalMiner:;
            gs_GoldMiner:;
            gs_IronMiner:;
          end;
          fHome.ResAddToOut(WorkPlan.Product,WorkPlan.ProductCount);
          fHome.SetState(hst_Idle,15);
          SetAction(TUnitActionStay.Create(15,ua_Walk));
        end;
    12: TaskDone:=true;
  end;
inc(Phase);
end;

{ TTaskGoHome }
constructor TTaskGoHome.Create(aTo:TKMPoint; aUnit:TKMUnit);
begin
  fDestPos:=aTo;
  fUnit:=aUnit;
  Phase:=0;
end;

procedure TTaskGoHome.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fUnit do
case Phase of
  0: SetAction(TUnitActionWalkTo.Create(fUnit.GetPosition,KMPointY1(fDestPos)));
  1: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
  2: begin
        SetAction(TUnitActionStay.Create(5,ua_Walk));
        fHome.SetState(hst_Idle,0);
     end;
  3: TaskDone:=true;
end;
inc(Phase);
end;


{ TTaskGoEat }
constructor TTaskGoEat.Create(aInn:TKMHouse; aUnit:TKMUnit);
begin
  fInn:=aInn;
  fUnit:=aUnit;
  Phase:=0;
end;

procedure TTaskGoEat.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

fUnit.fCondition:= UNIT_MAX_CONDITION;
TaskDone:=true;
exit;

with fUnit do
case Phase of
 0: begin
      if fHome<>nil then fHome.SetState(hst_Empty,0);
      if not fVisible then SetAction(TUnitActionGoIn.Create(ua_Walk,gid_Out)); //Walk outside the house
    end;
 1: SetAction(TUnitActionWalkTo.Create(GetPosition,fInn.GetPosition));
 2: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In)); //Enter Inn
 3: if fInn.CheckResIn(rt_Bread)>0 then begin
      fInn.ResTakeFromIn(rt_Bread);
      //Choose spot
      //fVisible:=true; Direction:=dir_N; //Make it overdraw
      SetAction(TUnitActionStay.Create(29,ua_Eat,false));
      inc(fCondition,round(UNIT_MAX_CONDITION/3));
    end;
 4: if (fCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Sousages)>0) then begin
      fInn.ResTakeFromIn(rt_Sousages);
      SetAction(TUnitActionStay.Create(29,ua_Eat,false));
      inc(fCondition,round(UNIT_MAX_CONDITION/2));
    end;
 5: if (fCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Wine)>0) then begin
      fInn.ResTakeFromIn(rt_Wine);
      SetAction(TUnitActionStay.Create(29,ua_Eat,false));
      inc(fCondition,round(UNIT_MAX_CONDITION/6));
    end;
 6: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_Out)); //Exit Inn
 7: TaskDone:=true;
end;
inc(Phase);
end;


{ TUnitAction }
constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

//First approach lets make a route for unit depending on static obstacles
//Then handle new static obstacles (if any) along with dynamic ones (LookAhead only one tile)
{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create(LocA,LocB:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  Inherited Create(aActionType);
  fDestPos:= LocB;
  fRouteBuilt:=false;
  NodePos:=1;

  //Build a route A*
  fTerrain.MakeRoute(LocA, LocB, CanWalk, NodeCount, Nodes);
  fRouteBuilt:=NodeCount>1;
//  Assert(fRouteBuilt,'Unable to make a route');
end;

procedure TUnitActionWalkTo.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX, DY, Distance:single;
begin
  //Execute the route in series of moves
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * KMUnit.Speed;

  if Equals(KMUnit.fPosition.X,Nodes[NodePos].X) and Equals(KMUnit.fPosition.Y,Nodes[NodePos].Y) then
    inc(NodePos);

  if NodePos>NodeCount then
    DoEnd:=true
  else begin

    DX:= Nodes[NodePos].X - KMUnit.fPosition.X;
    DY:= Nodes[NodePos].Y - KMUnit.fPosition.Y;

    if (DX<0) and (DY<0) then KMUnit.Direction:=dir_NW;
    if (DX<0) and (DY=0) then KMUnit.Direction:=dir_W;
    if (DX<0) and (DY>0) then KMUnit.Direction:=dir_SW;
    if (DX=0) and (DY>0) then KMUnit.Direction:=dir_S;
    if (DX>0) and (DY>0) then KMUnit.Direction:=dir_SE;
    if (DX>0) and (DY=0) then KMUnit.Direction:=dir_E;
    if (DX>0) and (DY<0) then KMUnit.Direction:=dir_NE;
    if (DX=0) and (DY<0) then KMUnit.Direction:=dir_N;

    if (DX <> 0) and (DY <> 0) then
      Distance:=Distance / sqrt (2);

    KMUnit.fPosition.X:= KMUnit.fPosition.X + sign(DX)*min(Distance,abs(DX));
    KMUnit.fPosition.Y:= KMUnit.fPosition.Y + sign(DY)*min(Distance,abs(DY));
    inc(KMUnit.AnimStep);
  end;
end;

{ TUnitActionGoIn }
constructor TUnitActionGoIn.Create(aAction: TUnitActionType; aDirection:TGoInDirection);
begin
  Inherited Create(aAction);
  fDir:=shortint(aDirection);
  if fDir>0 then
    fStep:=1   //go Inside (one cell up)
  else
    fStep:=0; //go Outside (one cell down)
end;

procedure TUnitActionGoIn.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Distance:single;
begin
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * KMUnit.Speed;

  if fDir>0 then
    KMUnit.Direction:=dir_N   //go Inside (one cell up)
  else
    KMUnit.Direction:=dir_S; //go Outside (one cell down)

  if (fStep=0)and(fDir=shortint(gid_Out)) then //Unit goes from house
  if (KMUnit.fHome<>nil)and(KMUnit.fHome.GetHouseType=ht_Barracks) then //Unit home is barracks
  if KMLength(KMUnit.GetPosition, KMUnit.fHome.GetEntrance)=0 then
    TKMHouseBarracks(KMUnit.fHome).RecruitsInside:=TKMHouseBarracks(KMUnit.fHome).RecruitsInside - 1;

  fStep := fStep - Distance * fDir;
  KMUnit.fPosition.Y := KMUnit.fPosition.Y - Distance * fDir;
  KMUnit.fVisible := fStep >= 0.3; //Make unit invisible when it's inside of House

  if (fStep<=0)and(fDir=shortint(gid_In)) then //Unit went into house
  if (KMUnit.fHome<>nil)and(KMUnit.fHome.GetHouseType=ht_Barracks) then //Unit home is barracks
  if KMLength(KMUnit.GetPosition, KMUnit.fHome.GetEntrance)=0 then
    TKMHouseBarracks(KMUnit.fHome).RecruitsInside:=TKMHouseBarracks(KMUnit.fHome).RecruitsInside + 1;

  if (fStep<=0)or(fStep>=1) then
    DoEnd:=true
  else
    inc(KMUnit.AnimStep);
end;


{ TUnitActionStay }
constructor TUnitActionStay.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
begin
  Inherited Create(aActionType);
  StayStill:=aStayStill;
  TimeToStay:=aTimeToStay;
end;

procedure TUnitActionStay.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
begin
  dec(TimeToStay);
  DoEnd := TimeToStay<=0;
  if not StayStill then inc(KMUnit.AnimStep);
end;


{ TKMUnitsCollection }
function TKMUnitsCollection.Add(aOwner: TPlayerID; aUnitType: TUnitType; PosX, PosY:integer):TKMUnit;
var T:Integer;
begin
  T:=-1;
  case aUnitType of
    ut_Serf:    T:= Inherited Add(TKMUnitSerf.Create(aOwner,PosX,PosY,aUnitType));
    ut_Worker:  T:= Inherited Add(TKMUnitWorker.Create(aOwner,PosX,PosY,aUnitType));

    ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                T:= Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_Recruit: T:= Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_Militia..ut_Barbarian:   T:= Inherited Add(TKMUnitWarrior.Create(aOwner,PosX,PosY,aUnitType));
    //ut_Bowman:   Inherited Add(TKMUnitArcher.Create(aOwner,PosX,PosY,aUnitType)); //I guess it will be stand-alone

    else Assert(false,'Such unit doesn''t exists yet - '+TypeToString(aUnitType));
  end;
  if T=-1 then Result:=nil else Result:=TKMUnit(Items[T]);
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

procedure TKMUnitsCollection.GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
var
  i:integer;
begin
  Loc.Clearup;
  for I := 0 to Count - 1 do
    if TKMUnit(Items[I]).fOwner=aOwner then
      Loc.AddEntry(TKMUnit(Items[I]).GetPosition);
end;

procedure TKMUnitsCollection.Paint();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMUnit(Items[I]).Paint();
end;

procedure TKMUnitsCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMUnit(Items[I]).UpdateState;
end;

end.
