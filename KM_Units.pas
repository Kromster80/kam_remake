unit KM_Units;
interface
uses
  KM_Defaults, windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Global_Data, KM_Houses, KromUtils;

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
    HouseAct:array of record
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

      {Walk somewhere}
      TUnitActionMove = class(TUnitAction)
      private
        fDestPos: TKMPoint;
      public
        constructor Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      {This is a simple action making unit go inside/outside of house}
      TUnitActionGoIn = class(TUnitAction)
      private
        fStep:single;
        fDir:integer;
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
  public
    procedure Execute(out TaskDone:boolean); virtual; abstract;
  end;

    TTaskDeliver = class(TUnitTask)
    private
    fSerf:TKMUnitSerf;
    fFrom:TKMPoint;
    fToHouse:TKMHouse;
    fToUnit:TKMUnit;
    fResource:TResourceType;
    ID:integer;
    public
    constructor Create(aSerf:TKMUnitSerf; aFrom:TKMPoint; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
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
      fLoc:TKMPoint;
      fHouseType:THouseType;
      TaskID:integer;
      Step:byte;
      ListOfCells:array[1..4*4]of TKMPoint;
    public
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aHouseType:THouseType; aID:integer);
      procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildHouse = class(TUnitTask)
    private
      fWorker:TKMUnitWorker;
      fLoc:TKMPoint;
      fHouse:TKMHouse;
      TaskID:integer;
      Step:byte;
      //ListOfCells:array[1..4*4]of TKMPoint; //Should be list of surrounding cells and directions
    public
      constructor Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aHouse:TKMHouse; aID:integer);
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
  private
    fHome:TKMHouse;
    fCurrentAction: TUnitAction;
    fPosition: TKMPointF;
    fLastUpdateTime: Cardinal;
    fOwner:TPlayerID;
    fUnitType: TUnitType;
    AnimStep: integer;
    fVisible:boolean;
    UnitTask:TUnitTask;
  public
    Direction: TKMDirection;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    destructor Destroy; override;
    function GetSupportedActions: TUnitActionTypeSet; virtual;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
    procedure SetAction(aAction: TUnitAction);
    property GetUnitType: TUnitType read fUnitType;
    function GetPosition():TKMPoint;
    procedure UpdateState; virtual; abstract;
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

  //Worker class - builds everuthing ingame
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
  private
    fSelectedUnit: TKMUnit;
  public
    procedure Add(aOwner:TPlayerID;  aUnitType:TUnitType; PosX, PosY:integer);
    procedure Rem(PosX,PosY:integer);
    procedure UpdateState;
    function HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    procedure GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
    procedure Paint();
    property SelectedUnit: TKMUnit read fSelectedUnit;
  end;

implementation
uses KM_Unit1, KM_Render;


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
  setlength(HouseAct,0+1);
  Product:=rt_None;
  ProductCount:=0;
end;

procedure TUnitWorkPlan.FindPlan(aUnitType:TUnitType; aHome:THouseType; aProduct:TResourceType; aLoc:TKMPoint);
  procedure SubActAdd(aAct:THouseActionType; aCycles:byte);
  begin
    setlength(HouseAct,length(HouseAct)+1);
    HouseAct[length(HouseAct)-1].Act:=aAct;
    HouseAct[length(HouseAct)-1].TimeToWork:=HouseDAT[byte(aHome)].Anim[byte(aAct)].Count * aCycles;
  end;
begin
FillDefaults;
//Now we need to fill only specific properties
if (aUnitType=ut_LamberJack)and(aHome=ht_SawMill) then begin
  Resource1:=rt_Trunk; Count1:=1;
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
  Product:=rt_Wood;
  ProductCount:=ResourceProductionX[byte(Product)];
end else
if (aUnitType=ut_Miner)and(aHome=ht_CoalMine) then begin
  GatheringScript:=gs_CoalMiner;
  SubActAdd(ha_Work1,1);
  SubActAdd(ha_Work2,8);
  SubActAdd(ha_Work5,1);
  Product:=rt_Coal;
  ProductCount:=ResourceProductionX[byte(Product)];
end else
if (aUnitType=ut_Baker)and(aHome=ht_Mill) then begin
  Resource1:=rt_Corn; Count1:=1;
  SubActAdd(ha_Work2,12);
  Product:=rt_Flour;
  ProductCount:=ResourceProductionX[byte(Product)];
end else
if (aUnitType=ut_Baker)and(aHome=ht_Bakery) then begin
  Resource1:=rt_Flour; Count1:=1;
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
  SubActAdd(ha_Work2,2);
  SubActAdd(ha_Work3,2);
  Product:=rt_Bread;
  ProductCount:=ResourceProductionX[byte(Product)];
end else
if (aUnitType=ut_Farmer)and(aHome=ht_Farm) then begin
  if fTerrain.FindCorn(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindCorn(aLoc,12);
    WalkTo:=ua_WalkTool;
    WorkType:=ua_Work;
    WorkCyc:=6;
    GatheringScript:=gs_FarmerCorn;
    WalkFrom:=ua_WalkBooty;
    Product:=rt_Corn;
    ProductCount:=ResourceProductionX[byte(Product)];
  end else
  if fTerrain.FindCornField(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindCornField(aLoc,12);
    WalkTo:=ua_Walk;
    WorkType:=ua_Work1;
    WorkCyc:=6;
    GatheringScript:=gs_FarmerSow;
    WalkFrom:=ua_Walk;
  end else
    Issued:=false;
end else
if (aUnitType=ut_Farmer)and(aHome=ht_Wineyard) then begin
  if fTerrain.FindGrapes(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindGrapes(aLoc,12);
    WalkTo:=ua_WalkTool2;
    WorkType:=ua_Work2;
    WorkCyc:=6;
    GatheringScript:=gs_FarmerWine;
    WalkFrom:=ua_WalkBooty2;
    SubActAdd(ha_Work1,1);
    SubActAdd(ha_Work2,8);
    SubActAdd(ha_Work5,1);
    Product:=rt_Wine;
    ProductCount:=ResourceProductionX[byte(Product)];
  end else
    Issued:=false;
end else
if (aUnitType=ut_StoneCutter)and(aHome=ht_Quary) then begin
  if fTerrain.FindStone(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindStone(aLoc,12);
    WalkTo:=ua_Walk;
    WorkType:=ua_Work;
    WorkCyc:=6;
    GatheringScript:=gs_StoneCutter;
    AfterWorkDelay:=10;
    WalkFrom:=ua_WalkTool;
    SubActAdd(ha_Work1,1);
    SubActAdd(ha_Work2,8);
    SubActAdd(ha_Work5,1);
    Product:=rt_Stone;
    ProductCount:=ResourceProductionX[byte(Product)];
  end else
    Issued:=false;
end else
if (aUnitType=ut_WoodCutter)and(aHome=ht_Woodcutters) then begin
  if fTerrain.FindTree(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindTree(aLoc,12);
    WalkTo:=ua_WalkBooty;
    WorkType:=ua_Work;
    WorkCyc:=6;
    GatheringScript:=gs_WoodCutterCut;
    AfterWorkDelay:=10;
    WalkFrom:=ua_WalkTool2;
    Product:=rt_Trunk;
    ProductCount:=ResourceProductionX[byte(Product)];
  end else
  if fTerrain.FindPlaceForTree(aLoc,12).X<>0 then begin
    Loc:=fTerrain.FindPlaceForTree(aLoc,12);
    WalkTo:=ua_WalkTool;
    WorkType:=ua_Work1;
    WorkCyc:=6;
    GatheringScript:=gs_WoodCutterPlant;
    WalkFrom:=ua_Walk;
  end else
    Issued:=false;
end else
  Issued:=false;
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
UnitType:=integer(fUnitType);
AnimAct:=integer(fCurrentAction.fActionType);
AnimDir:=integer(Direction);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType,       9, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
  end;
ua_Work..ua_Eat:
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
ua_WalkArm .. ua_WalkBooty2:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
  end;
end;
end;

procedure TKMUnitCitizen.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if UnitTask <> nil then
    UnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if UnitTask <> nil then begin
    UnitTask.Free;
    UnitTask:=nil
  end;

//Here come unit tasks in priorities
//Priority no.1 - find self a food
//Priority no.2 - find self a home
//Priority no.3 - find self a work
{    if (Starving) then
      if FindInn then
        GoEat
      else
        StayStillAndDieSoon
    else}
    if (fHome=nil) then
      if FindHome then
        UnitTask:=TTaskGoHome.Create(fHome.GetPosition,Self)
      else
        SetAction(TUnitActionStay.Create(120, ua_Walk))
    else
      UnitTask:=InitiateMining;

if UnitTask=nil then SetAction(TUnitActionStay.Create(120, ua_Walk));
end;


function TKMUnitCitizen.InitiateMining():TUnitTask;
begin
Result:=nil;

WorkPlan.FindPlan(fUnitType,fHome.GetHouseType,HouseOutput[byte(fHome.GetHouseType),1],GetPosition);

if not WorkPlan.Issued then exit;
if (WorkPlan.Resource1<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource1)<WorkPlan.Count1) then exit;
if (WorkPlan.Resource2<>rt_None)and(fHome.CheckResIn(WorkPlan.Resource2)<WorkPlan.Count2) then exit;
if fHome.CheckResOut(WorkPlan.Product)>=MaxResInHouse then exit;

Result:=TTaskMining.Create(WorkPlan,Self,fHome);
end;


{ TKMSerf }

procedure TKMUnitSerf.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;
  UnitType:=1;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);

  if Carry<>rt_None then
    fRender.RenderUnitCarry(integer(Carry), AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1)
  else
    fRender.RenderUnit(UnitType, 9, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMUnitSerf.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if UnitTask <> nil then
    UnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if UnitTask <> nil then begin
    UnitTask.Free;
    UnitTask:=nil
  end;

  UnitTask:=GetActionFromQueue;

  if UnitTask=nil then SetAction(TUnitActionStay.Create(10,ua_Walk)); //Stay idle
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
var UnitType:integer; AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;
  UnitType:=10;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMUnitWorker.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);

  if not DoEnd then exit;

  if UnitTask <> nil then
    UnitTask.Execute(TaskDone);

  if not TaskDone then exit;

  if UnitTask <> nil then begin
    UnitTask.Free;
    UnitTask:=nil
  end;

  UnitTask:=GetActionFromQueue;

  if UnitTask=nil then SetAction(TUnitActionStay.Create(10,ua_Walk));
end;

function TKMUnitWorker.GetActionFromQueue():TUnitTask;
begin
Result:=ControlList.BuildList.AskForHousePlan(Self,KMPoint(1,1));
if Result=nil then
Result:=ControlList.BuildList.AskForHouse(Self,KMPoint(1,1));
if Result=nil then
Result:=ControlList.BuildList.AskForRoad(Self,KMPoint(1,1));
end;

{ TKMwarrior }

function TKMUnitWarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;

procedure TKMUnitWarrior.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
UnitType:=22;
AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
AnimDir:=integer(Direction);
fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMUnitWarrior.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
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
  Direction:=dir_N;
  fVisible:=true;
  Speed:=UnitSpeeds[byte(aUnitType)];
  SetAction(TUnitActionStay.Create(10,ua_Walk));
end;

destructor TKMUnit.Destroy;
begin
  Inherited;
  fCurrentAction.Free;
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

function TKMUnit.HitTest(X, Y: Integer; const UT:TUnitType = ut_Any): Boolean;
begin
  Result:= (X = round(fPosition.X)) and (Y = round(fPosition.Y)) and ((fUnitType=UT)or(UT=ut_Any));
end;

procedure TKMUnit.SetAction(aAction: TUnitAction);
begin
AnimStep:=0;
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

{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aFrom:TKMPoint; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
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
var KMHouse:TKMHouse;
begin
TaskDone:=false;

if fResource=rt_Wood then
  Phase:=Phase;

with fSerf do
case Phase of
0: SetAction(TUnitActionMove.Create(KMPointY1(fFrom)));
1: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
2: SetAction(TUnitActionStay.Create(5,ua_Walk));
3: begin
     KMHouse:=ControlList.HousesHitTest(fFrom.X,fFrom.Y);
     if (KMHouse <> nil) and KMHouse.ResTakeFromOut(fResource) then
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

//Deliver into house

if fToHouse<>nil then
with fSerf do
case Phase of
5: SetAction(TUnitActionMove.Create(KMPointY1(fToHouse.GetPosition)));
6: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
7: SetAction(TUnitActionStay.Create(5,ua_Walk));
8: begin
     fToHouse.ResAddToIn(Carry);
     TakeResource(Carry);
   end;
9: SetAction(TUnitActionGoIn.Create(ua_walk,gid_Out));
10: ControlList.DeliverList.CloseDelivery(ID);
11: TaskDone:=true;
end;

//Deliver to builder
if fToUnit<>nil then
with fSerf do
case Phase of
5: SetAction(TUnitActionMove.Create(KMPointY1(fToUnit.fPosition)));
6: TakeResource(Carry);
7: begin
     inc(fToUnit.UnitTask.Phase);
     fToUnit.SetAction(TUnitActionStay.Create(0,ua_Work1));
   end;
8: ControlList.DeliverList.CloseDelivery(ID);
9: TaskDone:=true;
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
0: SetAction(TUnitActionMove.Create(fLoc));
1: fTerrain.RemMarkup(fLoc);
2: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
3: fTerrain.IncFieldState(fLoc);
4: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
5: fTerrain.IncFieldState(fLoc);
6: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
7: ControlList.DeliverList.AddNewDemand(nil, fWorker, rt_Stone, dt_Once);

8: SetAction(TUnitActionStay.Create(30,ua_Work1));

9: ;
10:SetAction(TUnitActionStay.Create(11,ua_Work2,false));
11:fTerrain.IncFieldState(fLoc);
12:SetAction(TUnitActionStay.Create(11,ua_Work2,false));
13:fTerrain.IncFieldState(fLoc);
14:SetAction(TUnitActionStay.Create(11,ua_Work2,false));
15:begin
    fTerrain.SetField(fLoc,fOwner,fdt_Road);
    ControlList.BuildList.CloseRoad(ID);
   end;
16:SetAction(TUnitActionStay.Create(5,ua_Work2));
17:TaskDone:=true;
end;
if Phase<>8 then inc(Phase); //Phase=8 is when worker waits for rt_Stone
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
0: SetAction(TUnitActionMove.Create(fLoc));
1: fTerrain.RemMarkup(fLoc);
2: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
3: fTerrain.IncFieldState(fLoc);
4: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
5: fTerrain.IncFieldState(fLoc);
6: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
7: ControlList.DeliverList.AddNewDemand(nil,fWorker,rt_Wood, dt_Once);

8: SetAction(TUnitActionStay.Create(30,ua_Work1));

9: SetAction(TUnitActionStay.Create(44,ua_Work2,false));
10:begin
    fTerrain.SetField(fLoc,fOwner,fdt_Wine);
    fTerrain.InitGrowth(fLoc);
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
  0: SetAction(TUnitActionMove.Create(fLoc));
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
constructor TTaskBuildHouseArea.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aHouseType:THouseType; aID:integer);
var i,k:integer;
begin
  fWorker:=aWorker;
  fLoc:=aLoc;
  fHouseType:=aHouseType;
  Phase:=0;
  TaskID:=aID;
  Step:=0;
  for i:=1 to 4 do for k:=1 to 4 do
  if HousePlanYX[byte(fHouseType),i,k]<>0 then begin
    inc(Step);
    ListOfCells[Step]:=KMPoint(fLoc.X+k-3,fLoc.Y + i - 4);
  end;
end;

{Prepare building site - flatten terrain}
procedure TTaskBuildHouseArea.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fWorker do
case Phase of
0:  SetAction(TUnitActionMove.Create(fLoc));
1:  fTerrain.SetHousePlan(fLoc, fHouseType, fdt_HouseWIP);
2:  ControlList.HousesHitTest(fLoc.X,fLoc.Y).SetBuildingState(hbs_NoGlyph);
3:  SetAction(TUnitActionMove.Create(ListOfCells[Step]));
4:  SetAction(TUnitActionStay.Create(11,ua_Work1,false));
5:  fTerrain.FlattenTerrain(ListOfCells[Step]);
6:  SetAction(TUnitActionStay.Create(11,ua_Work1,false));
7:  fTerrain.FlattenTerrain(ListOfCells[Step]);
8:  SetAction(TUnitActionStay.Create(11,ua_Work1,false));
9:  fTerrain.FlattenTerrain(ListOfCells[Step]);
10: SetAction(TUnitActionStay.Create(11,ua_Work1,false));
11: begin fTerrain.FlattenTerrain(ListOfCells[Step]); dec(Step); end;
12: begin
      ControlList.BuildList.CloseHousePlan(TaskID);
      //ControlList.AddHouse(fOwner, fHouseType, fLoc);
      ControlList.HousesHitTest(fLoc.X,fLoc.Y).SetBuildingState(hbs_Wood);
      ControlList.BuildList.AddNewHouse(fLoc,ControlList.HousesHitTest(fLoc.X,fLoc.Y)); //Add the house to JobList, so then all workers could take it
      end;
13: TaskDone:=true;
end;
inc(Phase);
if (Phase=12)and(Step>0) then Phase:=3; //Repeat with next cell
end;


{ TTaskBuildHouse }
constructor TTaskBuildHouse.Create(aWorker:TKMUnitWorker; aLoc:TKMPoint; aHouse:TKMHouse; aID:integer);
begin
  fWorker:=aWorker;
  fLoc:=aLoc;
  fHouse:=aHouse;
  Phase:=0;
  TaskID:=aID;
  Step:=0;
end;

{Build the house}
procedure TTaskBuildHouse.Execute(out TaskDone:boolean);
begin
  TaskDone:=false;
  with fWorker do
    case Phase of
      0: SetAction(TUnitActionMove.Create(fLoc));
      1: fTerrain.SetHousePlan(fLoc, fHouse.GetHouseType, fdt_None);
      2: fHouse.IncBuildingProgress;
      3: SetAction(TUnitActionStay.Create(11,ua_Work,false));
      4: begin
           ControlList.BuildList.CloseHouse(TaskID);
           TaskDone:=true;
         end;
    end;
  inc(Phase);
  if (Phase=4) and (not fHouse.IsComplete) then Phase:=2;
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
    1: SetAction(TUnitActionMove.Create(WorkPlan.Loc,WorkPlan.WalkTo));
    2: begin //Choose direction and time to work
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
               gs_FarmerWine:  fTerrain.InitGrowth(WorkPlan.Loc);
               gs_WoodCutterPlant: fTerrain.AddTree(WorkPlan.Loc,ChopableTrees[Random(length(ChopableTrees))+1,1]);
               gs_WoodCutterCut:   fTerrain.ChopTree(WorkPlan.Loc);
             end;
       SetAction(TUnitActionStay.Create(WorkPlan.AfterWorkDelay, WorkPlan.WorkType));
       end;
    4: SetAction(TUnitActionMove.Create(KMPointY1(fHome.GetPosition),WorkPlan.WalkFrom)); //Go home
    5: SetAction(TUnitActionGoIn.Create(WorkPlan.WalkFrom,gid_In)); //Go inside

    {Unit back at home and can process its booty now}
    6: begin
        fHome.SetState(hst_Work,0); //Set house to Work state
        fHome.ResTakeFromIn(WorkPlan.Resource1); //Count should be added
        fHome.ResTakeFromIn(WorkPlan.Resource2);
        fHome.fCurrentAction.SubActionAdd([ha_Smoke]);
       end;
    7: if length(WorkPlan.HouseAct)>1 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[1].Act,WorkPlan.HouseAct[1].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[1].TimeToWork,ua_Walk)); //Kepp unit idling till next Phase
       end else begin Phase:=SkipWork; exit; end;
    8: if length(WorkPlan.HouseAct)>2 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[2].Act,WorkPlan.HouseAct[2].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[2].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    9:if length(WorkPlan.HouseAct)>3 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[3].Act,WorkPlan.HouseAct[3].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[3].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    10:if length(WorkPlan.HouseAct)>4 then begin
           fHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[4].Act,WorkPlan.HouseAct[4].TimeToWork);
           SetAction(TUnitActionStay.Create(WorkPlan.HouseAct[4].TimeToWork,ua_Walk));
       end else begin Phase:=SkipWork; exit; end;
    11: begin
          case WorkPlan.GatheringScript of
            gs_CoalMiner:;
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
  0: SetAction(TUnitActionMove.Create(KMPointY1(fDestPos)));
  1: SetAction(TUnitActionGoIn.Create(ua_Walk,gid_In));
  2: begin
        fHome.OwnerGoesIn;
        SetAction(TUnitActionStay.Create(5,ua_Walk));
        fHome.SetState(hst_Idle,0);
     end;
  3: TaskDone:=true;
end;
inc(Phase);
end;


{ TUnitAction }
constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;


{ TUnitActionMove }
constructor TUnitActionMove.Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  Inherited Create(aActionType);
  fDestPos:= Loc;
end;

procedure TUnitActionMove.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX, DY, Distance:single;
begin
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * KMUnit.Speed;
  DX:= fDestPos.X - KMUnit.fPosition.X;
  DY:= fDestPos.Y - KMUnit.fPosition.Y;

  if (DX<0) and (DY<0) then KMUnit.Direction:=dir_NW;
  if (DX<0) and (DY=0) then KMUnit.Direction:=dir_W;
  if (DX<0) and (DY>0) then KMUnit.Direction:=dir_SW;
  if (DX=0) and (DY>0) then KMUnit.Direction:=dir_S;
  if (DX>0) and (DY>0) then KMUnit.Direction:=dir_SE;
  if (DX>0) and (DY=0) then KMUnit.Direction:=dir_E;
  if (DX>0) and (DY<0) then KMUnit.Direction:=dir_NE;
  if (DX=0) and (DY<0) then KMUnit.Direction:=dir_N;

  //Diagonal movement should take sqrt(2) more time
  if (DX <> 0) and (DY <> 0) then
    Distance:=Distance / sqrt (2);

    KMUnit.fPosition.X:= KMUnit.fPosition.X + sign(DX)*min(Distance,abs(DX));
    KMUnit.fPosition.Y:= KMUnit.fPosition.Y + sign(DY)*min(Distance,abs(DY));

  if (KMUnit.fPosition.X = fDestPos.X) and (KMUnit.fPosition.Y = fDestPos.Y) then
    DoEnd:= True
  else
    inc(KMUnit.AnimStep);
end;

{ TUnitActionGoIn }
constructor TUnitActionGoIn.Create(aAction: TUnitActionType; aDirection:TGoInDirection);
begin
  Inherited Create(aAction);
  fDir:=integer(aDirection);
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

  fStep := fStep - Distance * fDir;
  KMUnit.fPosition.Y := KMUnit.fPosition.Y - Distance * fDir;

  KMUnit.fVisible := fStep >= 0.3; //Make unit invisible when it's inside of House

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
procedure TKMUnitsCollection.Add(aOwner: TPlayerID; aUnitType: TUnitType; PosX, PosY:integer);
begin
  case aUnitType of
    ut_Serf:         Inherited Add(TKMUnitSerf.Create(aOwner,PosX,PosY,aUnitType));
    ut_Worker:       Inherited Add(TKMUnitWorker.Create(aOwner,PosX,PosY,aUnitType));

    ut_WoodCutter..ut_Fisher: Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));
    //ut_Worker
    ut_StoneCutter..ut_Metallurgist: Inherited Add(TKMUnitCitizen.Create(aOwner,PosX,PosY,aUnitType));

    ut_HorseScout:   Inherited Add(TKMUnitWarrior.Create(aOwner,PosX,PosY,aUnitType));

    else Assert(false,'Such unit doesn''t exists yet - '+TypeToString(aUnitType));
  end;
end;

procedure TKMUnitsCollection.Rem(PosX,PosY:integer);
begin
  if HitTest(PosX,PosY)<>nil then Remove(HitTest(PosX,PosY));
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
  fSelectedUnit:= Result;
end;

procedure TKMUnitsCollection.GetLocations(aOwner:TPlayerID; out Loc:TKMPointList);
var
  i:integer;
begin
  Loc.Clearup;
  for I := 0 to Count - 1 do
    if TKMUnit(Items[I]).fOwner=aOwner then
      Loc.AddEntry(KMPointRound(TKMUnit(Items[I]).fPosition));
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
