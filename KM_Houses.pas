unit KM_Houses;
interface
uses windows, math, classes, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Global_Data, KM_Defaults;

  {Everything related to houses is here}
type        
  TKMHouse = class;

  THouseAction = class(TObject)
  private
    TimeToAct:integer;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
  public
    constructor Create(aHouseState: THouseState; const aTime:integer=0);
    procedure SetState(aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType; const aTime:integer=0);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    procedure Execute(KMHouse: TKMHouse; TimeDelta: single; out DoEnd: Boolean);
    property ActionType: THouseState read fHouseState;
  end;

  THouseTask = class(TObject)
  private
  public
   // constructor Create();
    procedure Execute(KMHouse:TKMHouse; out TaskDone:boolean); virtual; abstract;
  end;

    TTaskIdle = class(THouseTask)
    private
      TimeToIdle:integer;
    public
      constructor Create(KMHouse: TKMHouse; aTimeToIdle:integer);
      procedure Execute(KMHouse:TKMHouse; out TaskDone:boolean); override;
    end;

    TTaskProduce = class(THouseTask)
    private
      fPlanID:integer;
      Phase:integer;
    public
      constructor Create(KMHouse: TKMHouse; aPlanID:integer);
      procedure Execute(KMHouse:TKMHouse; out TaskDone:boolean); override;
    end;

  TKMHouse = class(TObject)
  private
    fPosition: TKMPoint;
    fHouseType: THouseType;
    fBuildState: THouseBuildState;
    fOwnerID: TPlayerID;

    fBuildingProgress: word; //That is how many efforts were put into building (Wooding+Stoning)
    fHealth: word; //House condition/health

    fHasOwner: boolean; //which is some TKMUnit
    fOwnerAtHome: boolean;
    fBuildingRepair: boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: boolean; //If on then no wares will be delivered here

    fOutputTypes:array[1..4]of TResourceType;
    fInputTypes:array[1..4] of TResourceType;
    fResourceIn:array[1..4] of byte;
    fResourceOut:array[1..4]of byte;

    fCurrentAction: THouseAction;
    HouseTask:THouseTask;
    fProductionPlan:integer; //Plan according to which house functions at the moment

    fLastUpdateTime: Cardinal;
    AnimStep: integer;
    procedure SetWareDelivery(AVal:boolean);
  public
    constructor Create(PosX,PosY:integer; aHouseType:THouseType; aOwner:TPlayerID; aBuildState:THouseBuildState);
    destructor Destroy; override;
    procedure Activate;
    function HitTest(X, Y: Integer): Boolean; overload;

    procedure SetBuildingState(aState: THouseBuildState);
    procedure IncBuildingProgress;
    function IsComplete:boolean;


    procedure SetState(aState: THouseState);
    procedure OwnerGoesIn;
    procedure OwnerGoesOut;
    function CheckResIn(aResource:TResourceType):byte;
    function CheckResOut(aResource:TResourceType):byte;
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1);
    function ResTakeFromIn(aResource:TResourceType):boolean;
    function ResTakeFromOut(aResource:TResourceType):boolean;

    property GetPosition:TKMPoint read fPosition;
    property GetHouseType:THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write SetWareDelivery;
    property GetHasOwner:boolean read fHasOwner;

    function GetProductionTask():THouseTask;
    procedure UpdateState;
    procedure Paint();
  end;

  TKMHousesCollection = class(TList)
  private
    fSelectedHouse: TKMHouse;
  public
    procedure Add(aOwner: TPlayerID; aHouseType: THouseType; PosX,PosY:integer);
    procedure AddPlan(aOwner: TPlayerID; aHouseType: THouseType; PosX,PosY:integer);
    procedure Rem(PosX,PosY:integer);
    procedure Clear; override;
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
    procedure Paint();
    property SelectedHouse: TKMHouse read fSelectedHouse write fSelectedHouse;
  end;

implementation
uses KM_DeliverQueue, KM_Unit1, KM_Terrain, KM_Render;


{ TKMHouse }

constructor TKMHouse.Create(PosX,PosY:integer; aHouseType:THouseType; aOwner:TPlayerID; aBuildState:THouseBuildState);
begin
  Inherited Create;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fHouseType:=aHouseType;
  fBuildState:=aBuildState;
  fOwnerID:=aOwner;
  fHasOwner:=false;
  fBuildingRepair:=false;
  fWareDelivery:=true;
  fOwnerAtHome:=false;
  fBuildingProgress:=0;
  if aBuildState=hbs_Done then Self.Activate;
  fTerrain.SetHousePlan(fPosition,fHouseType,fdt_None); //Sets passability
  fTerrain.SetTileOwnership(fPosition,fHouseType,play_1);
end;

destructor TKMHouse.Destroy;
begin
  Inherited;
  fCurrentAction.Free;
end;

procedure TKMHouse.Activate;
var i,k:integer;
begin
  fProductionPlan:=HouseProductionPlanID[byte(fHouseType)];

  fCurrentAction:=THouseAction.Create(hst_Empty);
  fCurrentAction.SubActionAdd([ha_FlagShtok,ha_Flag1..ha_Flag3]);

  fOutputTypes[1]:=HouseOutput[byte(fHouseType),1];
  fInputTypes[1]:=HouseInput[byte(fHouseType),1];
  for i:=1 to 4 do
    case fInputTypes[i] of
      rt_None:    ;
      rt_Warfare: ControlList.DeliverList.AddNewDemand(Self,nil,fInputTypes[i],dt_Constant);
      rt_All:     ControlList.DeliverList.AddNewDemand(Self,nil,fInputTypes[i],dt_Constant);
      else for k:=1 to 5 do //Every new house needs 5 resourceunits
          ControlList.DeliverList.AddNewDemand(Self,nil,fInputTypes[i],dt_Once);
    end;

end;

function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result:=false;
if (X-fPosition.X+3 in [1..4])and(Y-fPosition.Y+4 in [1..4]) then
if HousePlanYX[integer(fHouseType),Y-fPosition.Y+4,X-fPosition.X+3]<>0 then
  Result:=true;
end;

procedure TKMHouse.SetBuildingState(aState: THouseBuildState);
begin
  fBuildState:=aState;
end;

{Increase building progress of house. When it reaches some point Stoning replaces Wooding
 and then it's done and house should be finalized}
procedure TKMHouse.IncBuildingProgress;
begin
  if IsComplete then exit;
  inc(fBuildingProgress);
  //inc(fHealth,5); //Should depend on build steps and full health
  if (fBuildState=hbs_Wood)and(fBuildingProgress = HouseDAT[byte(fHouseType)].WoodCount) then begin
    fBuildState:=hbs_Stone;
    fBuildingProgress:=0;
  end;
  if (fBuildState=hbs_Stone)and(fBuildingProgress = HouseDAT[byte(fHouseType)].StoneCount) then begin
    fBuildState:=hbs_Done;
    Activate;
  end;
end;


{Check if house is completely built, nevermind the damage}
function TKMHouse.IsComplete():boolean;
begin
  Result := fBuildState = hbs_Done;
end;

function TKMHouse.CheckResIn(aResource:TResourceType):byte;
var i:integer;
begin
Result:=0;
  for i:=1 to 3 do
  if (aResource = fInputTypes[i])or(aResource=rt_All) then
    inc(Result,fResourceIn[i]);
end;

function TKMHouse.CheckResOut(aResource:TResourceType):byte;
var i:integer;
begin
Result:=0;
  for i:=1 to 3 do
  if (aResource = fOutputTypes[i])or(aResource=rt_All) then
    inc(Result,fResourceOut[i]);
end;

procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var i,k:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 3 do
  if aResource = fOutputTypes[i] then
    begin
      inc(fResourceOut[i],aCount);
      for k:=1 to aCount do
        ControlList.DeliverList.AddNewOffer(Self,aResource);
    end;
end;


procedure TKMHouse.ResAddToIn(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 3 do
  if aResource = fInputTypes[i] then
    inc(fResourceIn[i],aCount);
end;


function TKMHouse.ResTakeFromIn(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
  for i:=1 to 3 do
  if aResource = fInputTypes[i] then begin
    dec(fResourceIn[i]);
    ControlList.DeliverList.AddNewDemand(Self,nil,aResource,dt_Once);
    Result:=true;
  end;
end;


function TKMHouse.ResTakeFromOut(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
  for i:=1 to 3 do
  if aResource = fOutputTypes[i] then begin
    dec(fResourceOut[i]);
    Result:=true;
  end;
end;


procedure TKMHouse.SetState(aState: THouseState);
begin
  fCurrentAction.SetState(aState);
end;


procedure TKMHouse.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd,TaskDone: Boolean;
begin
if fBuildState<>hbs_Done then exit;
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  Assert(fCurrentAction<>nil,'House has no action to execute!');
  fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if not DoEnd then exit;

  if fCurrentAction.fHouseState=hst_Empty then exit;

  if HouseTask<>nil then
    HouseTask.Execute(Self, TaskDone);
  if not TaskDone then exit;

  HouseTask:=GetProductionTask;
//  if HouseTask=nil then
//  HouseTask:=TTaskIdle.Create(Self, 5);
end;

function TKMHouse.GetProductionTask():THouseTask;
begin
if ( CheckResOut(TResourceType(HouseProductionPlan[fProductionPlan,5]))<MaxResInHouse ) and
( CheckResIn(TResourceType(HouseProductionPlan[fProductionPlan,1]))>0 ) then
  Result:=TTaskProduce.Create(Self,fProductionPlan)
else
  Result:=TTaskIdle.Create(Self,15);
end;

procedure TKMHouse.OwnerGoesIn;
begin
fOwnerAtHome:=true;
end;

procedure TKMHouse.OwnerGoesOut;
begin
fOwnerAtHome:=false;
end;               

procedure TKMHouse.Paint;
begin
case fBuildState of
  hbs_Glyph: fRender.RenderHouseBuild(byte(fHouseType),fPosition.X, fPosition.Y);
  hbs_NoGlyph:; //Nothing
  hbs_Wood:  fRender.RenderHouseWood(byte(fHouseType), fBuildingProgress/HouseDAT[byte(fHouseType)].WoodCount, fPosition.X, fPosition.Y);
  hbs_Stone: fRender.RenderHouseStone(byte(fHouseType), fBuildingProgress/HouseDAT[byte(fHouseType)].StoneCount, fPosition.X, fPosition.Y);
else begin
//Render base
fRender.RenderHouse(byte(fHouseType),fPosition.X, fPosition.Y);
//Render supplies
fRender.RenderHouseSupply(byte(fHouseType),fResourceIn,fResourceOut,fPosition.X, fPosition.Y);
//Render animation
if fCurrentAction=nil then exit;
fRender.RenderHouseWork(byte(fHouseType),integer(fCurrentAction.fSubAction),AnimStep,integer(fOwnerID),fPosition.X, fPosition.Y);
end;
end;
end;

procedure TKMHouse.SetWareDelivery(AVal:boolean);
begin
  fWareDelivery := AVal;
  //@Krom: Here we should either enable or disable delivery of wares to this building.
  //I don't really understand how the delivery system works, so maybe you could do this? 
end;

{ THouseAction }

constructor THouseAction.Create(aHouseState: THouseState; const aTime:integer=0);
begin
  Inherited Create;
  SetState(aHouseState);
  TimeToAct:=aTime;
end;

procedure THouseAction.SetState(aHouseState: THouseState);
begin
fHouseState:=aHouseState;
  if aHouseState=hst_Idle then begin
    SubActionRem([ha_Work1..ha_Smoke]); //remove all work attributes
    SubActionAdd([ha_Idle]);
  end;
  if aHouseState=hst_Work then begin
    SubActionRem([ha_Idle]);
  end;
  if aHouseState=hst_Empty then begin
    SubActionRem([ha_Idle]);
  end;
end;

procedure THouseAction.SubActionWork(aActionSet: THouseActionType; const aTime:integer=0);
begin
  SubActionRem([ha_Work1..ha_Work5]);
  fSubAction:= fSubAction + [aActionSet];
  if aTime<>0 then TimeToAct:=aTime;
end;

procedure THouseAction.SubActionAdd(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction + aActionSet;
end;

procedure THouseAction.SubActionRem(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction - aActionSet;
end;

procedure THouseAction.Execute(KMHouse: TKMHouse; TimeDelta: single; out DoEnd: Boolean);
begin
  DoEnd:= False;
  inc(KMHouse.AnimStep);
  dec(TimeToAct);
  if TimeToAct<=0 then begin DoEnd:= True; {TimeToAct:=0;} end; //Action is complete
end;

{ TTaskIdle }

constructor TTaskIdle.Create(KMHouse: TKMHouse; aTimeToIdle:integer);
begin
TimeToIdle:=aTimeToIdle;
if KMHouse.fOwnerAtHome then
  KMHouse.SetState(hst_Idle)
else
  KMHouse.SetState(hst_Empty)
end;

procedure TTaskIdle.Execute(KMHouse:TKMHouse; out TaskDone:boolean);
var
  TimeDelta: integer;
begin
TaskDone:=false;
TimeDelta:=1;
TimeToIdle:=TimeToIdle-TimeDelta;
if TimeToIdle<=0 then
  TaskDone:=true;
end;

{ TTaskProduce }

constructor TTaskProduce.Create(KMHouse: TKMHouse; aPlanID:integer);
begin
fPlanID:=aPlanID;
Phase:=0;
end;

procedure TTaskProduce.Execute(KMHouse:TKMHouse; out TaskDone:boolean);
var t:integer;
begin
TaskDone:=false;
case Phase of
0: KMHouse.SetState(hst_Work);
1: KMHouse.ResTakeFromIn(TResourceType(HouseProductionPlan[fPlanID,1]));
2: KMHouse.fCurrentAction.SubActionAdd([ha_Smoke]);
3: if HouseProductionPlan[fPlanID,4]<>0 then begin
     t:=HouseDAT[byte(KMHouse.fHouseType)].Anim[HouseProductionPlan[fPlanID,3]].Count * HouseProductionPlan[fPlanID,4];
     KMHouse.fCurrentAction.SubActionWork(THouseActionType(HouseProductionPlan[fPlanID,3]),t);
   end;
//5: KMHouse.fCurrentAction.SubActionWork([ha_Work3],15);
//6: KMHouse.fCurrentAction.SubActionWork([ha_Work4],15);
//7: KMHouse.fCurrentAction.SubActionWork([ha_Work5],15);
8: KMHouse.ResAddToOut(TResourceType(HouseProductionPlan[fPlanID,5]),HouseProductionPlan[fPlanID,6]);
9: TaskDone:=true;
end;
inc(Phase);
end;

{ TKMHousesCollection }

procedure TKMHousesCollection.Add(aOwner: TPlayerID; aHouseType: THouseType; PosX,PosY:integer);
begin
Inherited Add(TKMHouse.Create(PosX,PosY,aHouseType,aOwner,hbs_Done));
end;

{Add a plan for house}
procedure TKMHousesCollection.AddPlan(aOwner: TPlayerID; aHouseType: THouseType; PosX,PosY:integer);
begin
Inherited Add(TKMHouse.Create(PosX,PosY,aHouseType,aOwner, hbs_Glyph));
end;

procedure TKMHousesCollection.Rem(PosX,PosY:integer);
begin
  if HitTest(PosX,PosY)<>nil then Remove(HitTest(PosX,PosY));
end;

procedure TKMHousesCollection.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited;
end;

function TKMHousesCollection.HitTest(X, Y: Integer): TKMHouse;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if TKMHouse(Items[I]).HitTest(X, Y) then
    begin
      Result:= TKMHouse(Items[I]);
      Break;
    end;
  if Result <> nil then fSelectedHouse:= Result;
end;

function TKMHousesCollection.FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
var
  i: integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
      if (HouseOwnerUnit[byte(TKMHouse(Items[I]).fHouseType)]=aUnitType)and(not TKMHouse(Items[I]).fHasOwner) then
      begin
        Result:= TKMHouse(Items[I]);
        TKMHouse(Items[I]).fHasOwner:=true;
        exit;
      end;
end;

procedure TKMHousesCollection.Paint();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMHouse(Items[I]).Paint();
end;

procedure TKMHousesCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMHouse(Items[I]).UpdateState;
end;

end.
