unit KM_Units;
interface
uses
  KM_Defaults, windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Global_Data, KM_Houses, KromUtils;

type

  TKMUnit = class;
  TKMSerf = class;
  TKMWorker = class;

  TUnitAction = class(TObject)
  private
    fActionType: TUnitActionType;
  public
    constructor Create(aActionType: TUnitActionType);
    procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); virtual; abstract;
    property ActionType: TUnitActionType read fActionType;
  end;

      TMoveUnitAction = class(TUnitAction)
      private
        fDestPos: TKMPoint;
      public
        constructor Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      //This is a simple action making unit go inside/outside of house
      TGoInUnitAction = class(TUnitAction)
      private
        fStep:single;
        fDir:integer;
      public
        constructor Create(aAction: TUnitActionType; aDirection:TGoInDirection);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      //Stay in place for set time
      TStayUnitAction = class(TUnitAction)
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
    fSerf:TKMSerf;
    fFrom:TKMPoint;
    fToHouse:TKMHouse;
    fToUnit:TKMUnit;
    fResource:TResourceType;
    ID:integer;
    public
    constructor Create(aSerf:TKMSerf; aFrom:TKMPoint; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
    procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildRoad = class(TUnitTask)
    private
    fWorker:TKMWorker;
    fLoc:TKMPoint;
    ID:integer;
    public
    constructor Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
    procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildWine = class(TUnitTask)
    private
    fWorker:TKMWorker;
    fLoc:TKMPoint;
    ID:integer;
    public
    constructor Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
    procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildField = class(TUnitTask)
    private
    fWorker:TKMWorker;
    fLoc:TKMPoint;
    ID:integer;
    public
    constructor Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
    procedure Execute(out TaskDone:boolean); override;
    end;

    TTaskBuildHouseArea = class(TUnitTask)
    private
    fWorker:TKMWorker;
    fLoc:TKMPoint;
    fHouseType:THouseType;
    TaskID:integer;
    Step:byte;
    ListOfCells:array[1..4*4]of TKMPoint;
    public
    constructor Create(aWorker:TKMWorker; aLoc:TKMPoint; aHouseType:THouseType; aID:integer);
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
      fHome:TKMHouse;
      fUnit:TKMUnit;
      fPlace:TKMPoint;
      fGatheringScript:TGatheringScript;
      fResource:TResourceType;
      fCount:integer;
      fAction1,fAction2,fAction3:TUnitActionType;
      fTimeToWork:integer; //Number of work cycles to perform
      fTimeToStay:integer; //Stay in place after work is done
      fTimeToIdle:integer; //Frames to idle
    public
    constructor Create(aHome:TKMHouse; aUnit:TKMUnit; aPlace:TKMPoint; aGatheringScript:TGatheringScript);
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
    procedure UpdateState; virtual; abstract;
    procedure Paint; virtual; abstract;
  end;

  //This is a common class for units going out of their homes for resources
  TKMHunter = class(TKMUnit)
  public
    function FindHome():boolean;
    procedure UpdateState; override;
    function InitiateMining():TUnitTask;
    procedure Paint(); override;
  end;

  //I grouped those so tight, but really, they don't have to use all 24 lines
{  TKMFarmer = class(TKMHunter) public function InitiateMining():TUnitTask; override; end;
  TKMStoneCutter = class(TKMHunter) public function InitiateMining():TUnitTask; override; end;
  TKMWoodCutter = class(TKMHunter) public function InitiateMining():TUnitTask; override; end;}
  //TKMFisher = class(TKMHunter) public function InitiateMining():boolean; override; end;

  //This is a common class for all those units sitting inside of houses
  TKMHomeSitting = class(TKMUnit)
  public
    function FindHome():boolean;
    procedure UpdateState; override;
    procedure Paint(); override;
  end;

  //Serf class - transports all goods ingame between houses
  TKMSerf = class(TKMUnit)
    Carry: TResourceType;
  public
    procedure UpdateState; override;
    procedure Paint(); override;
    function GiveResource(Res:TResourceType):boolean;
    function TakeResource(Res:TResourceType):boolean;
    function GetActionFromQueue():TUnitTask;
  end;

  //Worker class - builds everuthing ingame
  TKMWorker = class(TKMUnit)
  public
    procedure UpdateState; override;
    procedure Paint(); override;
    function GetActionFromQueue():TUnitTask;
  end;

  //Possibly melee warrior class? with Archer class separate?
  TKMWarrior = class(TKMUnit)
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

{ TKMHunter }

function TKMHunter.FindHome():boolean;
var KMHouse:TKMHouse;
begin
Result:=false;
KMHouse:=ControlList.FindEmptyHouse(fUnitType);
if KMHouse<>nil then begin fHome:=KMHouse; Result:=true; end;
end;

procedure TKMHunter.Paint();
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

procedure TKMHunter.UpdateState;
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
        SetAction(TStayUnitAction.Create(120, ua_Walk))
    else
      UnitTask:=InitiateMining;

if UnitTask=nil then SetAction(TStayUnitAction.Create(120, ua_Walk));
end;


function TKMHunter.InitiateMining():TUnitTask;
var aPlace:TKMPoint;
begin

Result:=nil;
if fHome.CheckResOut(rt_All)>=MaxResInHouse then exit;

case Self.fUnitType of

  ut_Farmer:      begin
                    if Self.fHome.GetHouseType=ht_Farm then
                    begin
                      aPlace:=fTerrain.FindCorn(fHome.GetPosition,12);
                      if aPlace.X<>0 then
                        Result:=TTaskMining.Create(fHome,Self,aPlace,gs_FarmerCorn)
                      else begin
                        aPlace:=fTerrain.FindCornField(fHome.GetPosition,12);
                        if aPlace.X<>0 then
                          Result:=TTaskMining.Create(fHome,Self,aPlace,gs_FarmerSow);
                      end;
                    end;
                    if Self.fHome.GetHouseType=ht_Wineyard then
                    begin
                      aPlace:=fTerrain.FindGrapes(fHome.GetPosition,12);
                      if aPlace.X<>0 then
                        Result:=TTaskMining.Create(fHome,Self,aPlace,gs_FarmerWine);
                    end;
                  end;

  ut_StoneCutter: begin
                  //aPlace:=fMap.FindStones(fPosition);
                  aPlace:=KMPoint(6,8);
                  Result:=TTaskMining.Create(fHome,Self,aPlace,gs_StoneCutter);
                  end;

  ut_WoodCutter:  begin
                      aPlace:=fTerrain.FindTree(fHome.GetPosition,10);
                      if aPlace.X<>0 then
                        Result:=TTaskMining.Create(fHome,Self,aPlace,gs_WoodCutterCut)
                      else begin
                        aPlace:=fTerrain.FindPlaceForTree(fHome.GetPosition,10);
                        if aPlace.X<>0 then
                          Result:=TTaskMining.Create(fHome,Self,aPlace,gs_WoodCutterPlant);
                      end;
                  end;

end;

end;


{ TKMHomeSitting }

function TKMHomeSitting.FindHome():boolean;
var KMHouse:TKMHouse;
begin
Result:=false;
KMHouse:=ControlList.FindEmptyHouse(fUnitType);
if KMHouse<>nil then
  begin
    fHome:=KMHouse;
    Result:=true;
  end;
end;

procedure TKMHomeSitting.Paint();
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
end;
end;

procedure TKMHomeSitting.UpdateState;
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
        SetAction(TStayUnitAction.Create(120, ua_Walk))
end;

{ TKMSerf }

procedure TKMSerf.Paint();
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

procedure TKMSerf.UpdateState;
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

  if UnitTask=nil then SetAction(TStayUnitAction.Create(10,ua_Walk)); //Stay idle
end;

function TKMSerf.GiveResource(Res:TResourceType):boolean;
begin
Result:=false;
if Carry<>rt_None then exit;
Carry:=Res;
Result:=true;
end;

function TKMSerf.TakeResource(Res:TResourceType):boolean;
begin
Carry:=rt_None;
Result:=true;
end;

function TKMSerf.GetActionFromQueue():TUnitTask;
begin
Result:=ControlList.DeliverList.AskForDelivery(Self);
end;

{ TKMWorker }

procedure TKMWorker.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;
  UnitType:=10;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMWorker.UpdateState;
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

  if UnitTask=nil then SetAction(TStayUnitAction.Create(10,ua_Walk));
end;

function TKMWorker.GetActionFromQueue():TUnitTask;
begin
Result:=ControlList.BuildList.AskForHouseToBuild(Self,KMPoint(1,1));
if Result=nil then
Result:=ControlList.BuildList.AskForRoadToBuild(Self,KMPoint(1,1));
end;

{ TKMwarrior }

function TKMwarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;

procedure TKMwarrior.Paint();
var UnitType:integer; AnimAct,AnimDir:integer;
begin
UnitType:=22;
AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
AnimDir:=integer(Direction);
fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMwarrior.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if DoEnd then
    SetAction(TStayUnitAction.Create(50,ua_Walk))
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
  SetAction(TStayUnitAction.Create(10,ua_Walk));
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
constructor TTaskDeliver.Create(aSerf:TKMSerf; aFrom:TKMPoint; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
begin
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
with fSerf do
case Phase of
0: SetAction(TMoveUnitAction.Create(KMPointY1(fFrom)));
1: SetAction(TGoInUnitAction.Create(ua_Walk,gid_In));
2: SetAction(TStayUnitAction.Create(5,ua_Walk));
3: begin
     KMHouse:=ControlList.HousesHitTest(fFrom.X,fFrom.Y);
     if (KMHouse <> nil) and KMHouse.ResTakeFromOut(fResource) then
       GiveResource(fResource)
     else
     begin
       SetAction(TGoInUnitAction.Create(ua_Walk,gid_Out));
       ControlList.DeliverList.CloseDelivery(ID);
       TaskDone:=true;
     end;
   end;
4: SetAction(TGoInUnitAction.Create(ua_Walk,gid_Out));
end;

//Deliver into house

if fToHouse<>nil then
with fSerf do
case Phase of
5: SetAction(TMoveUnitAction.Create(KMPointY1(fToHouse.GetPosition)));
6: SetAction(TGoInUnitAction.Create(ua_Walk,gid_In));
7: SetAction(TStayUnitAction.Create(5,ua_Walk));
8: begin
     fToHouse.ResAddToIn(Carry);
     TakeResource(Carry);
   end;
9: SetAction(TGoInUnitAction.Create(ua_walk,gid_Out));
10: ControlList.DeliverList.CloseDelivery(ID);
11: TaskDone:=true;
end;

//Deliver to builder
if fToUnit<>nil then
with fSerf do
case Phase of
5: SetAction(TMoveUnitAction.Create(KMPointY1(fToUnit.fPosition)));
6: TakeResource(Carry);
7: begin
     inc(fToUnit.UnitTask.Phase);
     fToUnit.SetAction(TStayUnitAction.Create(0,ua_Work1));
   end;
8: ControlList.DeliverList.CloseDelivery(ID);
9: TaskDone:=true;
end;
inc(Phase);
end;

{ TTaskBuildRoad }
constructor TTaskBuildRoad.Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
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
0: SetAction(TMoveUnitAction.Create(fLoc));
1: fTerrain.RemMarkup(fLoc);
2: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
3: fTerrain.IncFieldState(fLoc);
4: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
5: fTerrain.IncFieldState(fLoc);
6: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
7: ControlList.DeliverList.AddNewDemand(nil, fWorker, rt_Stone, dt_Once);

8: SetAction(TStayUnitAction.Create(30,ua_Work1));

9: ;
10:SetAction(TStayUnitAction.Create(11,ua_Work2,false));
11:fTerrain.IncFieldState(fLoc);
12:SetAction(TStayUnitAction.Create(11,ua_Work2,false));
13:fTerrain.IncFieldState(fLoc);
14:SetAction(TStayUnitAction.Create(11,ua_Work2,false));
15:begin
    fTerrain.SetField(fLoc,fOwner,fdt_Road);
    ControlList.BuildList.CloseRoadToBuild(ID);
   end;
16:SetAction(TStayUnitAction.Create(5,ua_Work2));
17:TaskDone:=true;
end;
if Phase<>8 then inc(Phase); //Phase=8 is when worker waits for rt_Stone
end;

{ TTaskBuildWine }
constructor TTaskBuildWine.Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
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
0: SetAction(TMoveUnitAction.Create(fLoc));
1: fTerrain.RemMarkup(fLoc);
2: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
3: fTerrain.IncFieldState(fLoc);
4: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
5: fTerrain.IncFieldState(fLoc);
6: SetAction(TStayUnitAction.Create(11,ua_Work1,false));
7: ControlList.DeliverList.AddNewDemand(nil,fWorker,rt_Wood, dt_Once);

8: SetAction(TStayUnitAction.Create(30,ua_Work1));

9: SetAction(TStayUnitAction.Create(44,ua_Work2,false));
10:begin
    fTerrain.SetField(fLoc,fOwner,fdt_Wine);
    ControlList.BuildList.CloseRoadToBuild(ID);
   end;
11:TaskDone:=true;
end;
if Phase<>8 then inc(Phase); //Phase=8 is when worker waits for rt_Stone
end;

{ TTaskBuildField }
constructor TTaskBuildField.Create(aWorker:TKMWorker; aLoc:TKMPoint; aID:integer);
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
  0: SetAction(TMoveUnitAction.Create(fLoc));
  1: fTerrain.RemMarkup(fLoc);
  2: SetAction(TStayUnitAction.Create(44,ua_Work1,false));
  3: fTerrain.IncFieldState(fLoc);
  4: SetAction(TStayUnitAction.Create(44,ua_Work1,false));
  5: fTerrain.IncFieldState(fLoc);
  6: SetAction(TStayUnitAction.Create(22,ua_Work1,false));
  7: begin
       ControlList.BuildList.CloseRoadToBuild(ID);
       fTerrain.SetField(fLoc,fOwner,fdt_Field);
     end;
  8: SetAction(TStayUnitAction.Create(5,ua_Work1));
  9: TaskDone:=true;
end;
inc(Phase);
end;

{ TTaskBuildHouseArea }
constructor TTaskBuildHouseArea.Create(aWorker:TKMWorker; aLoc:TKMPoint; aHouseType:THouseType; aID:integer);
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
    ListOfCells[Step]:=KMPoint(fLoc.X + HouseXOffset[byte(fHouseType)]+k-4,fLoc.Y + i - 4);
  end;
end;

procedure TTaskBuildHouseArea.Execute(out TaskDone:boolean);
begin
TaskDone:=false;
with fWorker do
case Phase of
0:  SetAction(TMoveUnitAction.Create(fLoc));
1:  fTerrain.SetHousePlan(fLoc, fHouseType, fdt_HouseWIP);
2:  SetAction(TMoveUnitAction.Create(ListOfCells[Step]));
3:  SetAction(TStayUnitAction.Create(11,ua_Work1,false));
4:  fTerrain.FlattenTerrain(ListOfCells[Step]);
5:  SetAction(TStayUnitAction.Create(11,ua_Work1,false));
6:  fTerrain.FlattenTerrain(ListOfCells[Step]);
7:  SetAction(TStayUnitAction.Create(11,ua_Work1,false));
8:  fTerrain.FlattenTerrain(ListOfCells[Step]);
9:  SetAction(TStayUnitAction.Create(11,ua_Work1,false));
10: begin fTerrain.FlattenTerrain(ListOfCells[Step]); dec(Step); end;
11: TaskDone:=true;
end;
inc(Phase);
if (Phase=11)and(Step>0) then Phase:=2; //Repeat with next cell
end;

{ TTaskMining }
constructor TTaskMining.Create(aHome:TKMHouse; aUnit:TKMUnit; aPlace:TKMPoint; aGatheringScript:TGatheringScript);
begin
fHome:=aHome;
fUnit:=aUnit;
fPlace:=aPlace;
fGatheringScript:=aGatheringScript;
//I don't know simple way to store various elements in array,
//so I have to use Type(integer(Type)) conversion
fResource:=TResourceType(UnitMiningPlan[byte(aGatheringScript),1]);
fCount:=UnitMiningPlan[byte(aGatheringScript),2];
fAction1:=TUnitActionType(UnitMiningPlan[byte(aGatheringScript),3]);
fAction2:=TUnitActionType(UnitMiningPlan[byte(aGatheringScript),4]);
fAction3:=TUnitActionType(UnitMiningPlan[byte(aGatheringScript),5]);
fTimeToWork:=UnitMiningPlan[byte(aGatheringScript),6];
fTimeToStay:=UnitMiningPlan[byte(aGatheringScript),7];
fTimeToIdle:=UnitMiningPlan[byte(aGatheringScript),8];
Phase:=0;
end;

procedure TTaskMining.Execute(out TaskDone:boolean);
var Dir:integer;
begin
TaskDone:=false;
with fUnit do
case Phase of
0: fHome.SetState(hst_Empty);
1: SetAction(TGoInUnitAction.Create(fAction1,gid_Out));
2: SetAction(TMoveUnitAction.Create(fPlace,fAction1));
3: begin //Choose direction and time to work
     Dir:=integer(fUnit.Direction);
     if UnitSprite[integer(fUnit.fUnitType)].Act[integer(fAction2)].Dir[Dir].Count<=1 then
       for Dir:=1 to 8 do
         if UnitSprite[integer(fUnit.fUnitType)].Act[integer(fAction2)].Dir[Dir].Count>1 then break;
     Dir:=min(Dir,8);
   fUnit.Direction:=TKMDirection(Dir);
   fTimeToWork:=fTimeToWork*max(UnitSprite[integer(fUnit.fUnitType)].Act[integer(fAction2)].Dir[Dir].Count,1);
   end;
4: SetAction(TStayUnitAction.Create(fTimeToWork, fAction2,false));

5: case fUnit.GetUnitType of
        ut_Farmer : if fGatheringScript=gs_FarmerSow then
                      fTerrain.InitGrowth(fPlace)
                    else
                    if fGatheringScript=gs_FarmerCorn then
                      fTerrain.CutCorn(fPlace);
        ut_WoodCutter : if fGatheringScript=gs_WoodCutterPlant then
                          fTerrain.AddTree(fPlace,ChopableTrees[Random(length(ChopableTrees))+1,1])
                        else
                        if fGatheringScript=gs_WoodCutterCut then
                          fTerrain.ChopTree(fPlace);
        ut_StoneCutter:;
        ut_Fisher:;
   end;

 //FarmerWine - decFieldAge
 //StoneCutter- decStone
 //Fisher     - remFish

6: SetAction(TStayUnitAction.Create(fTimeToStay, fAction2));

7: SetAction(TMoveUnitAction.Create(KMPointY1(fHome.GetPosition),fAction3));
8: SetAction(TGoInUnitAction.Create(fAction3,gid_In));
9: fHome.ResAddToOut(fResource,fCount);
10:fHome.SetState(hst_Idle);
11:SetAction(TStayUnitAction.Create(fTimeToIdle,ua_Walk));
12:TaskDone:=true;
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
0: SetAction(TMoveUnitAction.Create(KMPointY1(fDestPos)));
1: SetAction(TGoInUnitAction.Create(ua_Walk,gid_In));
2: fHome.OwnerGoesIn;
3: SetAction(TStayUnitAction.Create(5,ua_Walk));
4: fHome.SetState(hst_Idle);
5: TaskDone:=true;
end;
inc(Phase);
end;

{ TUnitAction }

constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

{ TMoveUnitAction }

constructor TMoveUnitAction.Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  Inherited Create(aActionType);
  fDestPos:= Loc;
end;

procedure TMoveUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
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

constructor TGoInUnitAction.Create(aAction: TUnitActionType; aDirection:TGoInDirection);
begin
  Inherited Create(aAction);
  fDir:=integer(aDirection);
  if fDir>0 then
    fStep:=1   //go Inside (one cell up)
  else
    fStep:=0; //go Outside (one cell down)
end;

procedure TGoInUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
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

  KMUnit.fVisible := fStep >= 0.3;

  if (fStep<=0)or(fStep>=1) then
    DoEnd:=true
  else
    inc(KMUnit.AnimStep);
end;

constructor TStayUnitAction.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
begin
  Inherited Create(aActionType);
  StayStill:=aStayStill;
  TimeToStay:=aTimeToStay;
end;

procedure TStayUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
begin
  DoEnd:= False;
  if not StayStill then inc(KMUnit.AnimStep);
  dec(TimeToStay);
  if TimeToStay<=0 then
    DoEnd:=true;
end;

{ TKMUnitsCollection }

procedure TKMUnitsCollection.Add(aOwner: TPlayerID; aUnitType: TUnitType; PosX, PosY:integer);
begin
  case aUnitType of
    ut_Serf:         Inherited Add(TKMSerf.Create(aOwner,PosX,PosY,aUnitType));

    ut_Worker:       Inherited Add(TKMWorker.Create(aOwner,PosX,PosY,aUnitType));

    ut_StoneCutter:  Inherited Add(TKMHunter.Create(aOwner,PosX,PosY,aUnitType));
    ut_WoodCutter:   Inherited Add(TKMHunter.Create(aOwner,PosX,PosY,aUnitType));
    ut_Farmer:       Inherited Add(TKMHunter.Create(aOwner,PosX,PosY,aUnitType));

    ut_Baker:        Inherited Add(TKMHomeSitting.Create(aOwner,PosX,PosY,aUnitType));
    ut_Lamberjack:   Inherited Add(TKMHomeSitting.Create(aOwner,PosX,PosY,aUnitType));

    ut_HorseScout:   Inherited Add(TKMwarrior.Create(aOwner,PosX,PosY,aUnitType));
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
