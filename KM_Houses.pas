unit KM_Houses;
interface
uses windows, math, classes, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults;

  {Everything related to houses is here}
type
  TKMHouse = class;

  THouseAction = class(TObject)
  private
    fHouse:TKMHouse;
    TimeToAct:integer;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
  public
    constructor Create(aHouse:TKMHouse; aHouseState: THouseState; const aTime:integer=0);
    procedure SetState(aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType; const aTime:integer=0);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    property ActionType: THouseState read fHouseState;
  end;


  TKMHouse = class(TObject)
  private
    fPosition: TKMPoint;
    fHouseType: THouseType;
    fBuildState: THouseBuildState;
    fOwner: TPlayerID;

    fBuildSupplyWood: byte;
    fBuildSupplyStone: byte;
    fBuildReserve: byte; //Take one resource into reserve and "build from it"
    fBuildingProgress: word; //That is how many efforts were put into building (Wooding+Stoning)
    fHealth: word; //House condition/health

    fHasOwner: boolean; //which is some TKMUnit
    fBuildingRepair: boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: boolean; //If on then no wares will be delivered here

    fResourceIn:array[1..4] of byte;
    fResourceOut:array[1..4]of byte;

    fResourceOrder:array[1..4]of word; //If HousePlaceOrders=true then here are production orders

    fLastUpdateTime: Cardinal;
    FlagAnimStep: integer; //Used for Flags and Burning animation
    WorkAnimStep: integer; //Used for Work and etc.. which is not in sync with Flags
    procedure SetWareDelivery(AVal:boolean);
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle

    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    destructor Destroy; override;
    procedure Activate;
    function HitTest(X, Y: Integer): Boolean; overload;

    procedure SetBuildingState(aState: THouseBuildState);
    procedure IncBuildingProgress;
    function IsStarted:boolean;
    function IsComplete:boolean;

    procedure SetState(aState: THouseState; aTime:integer);
    function GetState:THouseState;

    function CheckResIn(aResource:TResourceType):byte;
    function CheckResOut(aResource:TResourceType):byte;
    function CheckResOrder(ID:byte):word;
    function CheckResToBuild():boolean;
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); virtual; //override for School and etc..
    procedure ResAddToBuild(aResource:TResourceType);
    function ResTakeFromIn(aResource:TResourceType):boolean;
    function ResTakeFromOut(aResource:TResourceType):boolean;
    procedure AddOrder(ID:byte; const Amount:byte=1);
    procedure RemOrder(ID:byte; const Amount:byte=1);

    property GetPosition:TKMPoint read fPosition;
    function GetEntrance():TKMPoint;
    property GetHouseType:THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write SetWareDelivery;
    property GetHasOwner:boolean read fHasOwner write fHasOwner;
    property GetHealth:word read fHealth;

    procedure UpdateState;
    procedure Paint();
  end;

  {School has one unique property - queue of units to be trained, 1 wip + 5 in line}
  TKMHouseSchool = class(TKMHouse)
  public
    UnitQueue:array[1..6]of TUnitType; //Also used in UI
    UnitTrainProgress:byte; //Was it 150 steps in KaM?
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); override;
    procedure AddUnitToQueue(aUnit:TUnitType); //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(id:integer); //Should remove unit from queue and shift rest up
    procedure UnitIsStart; //This should Create new unit and start training cycle
    procedure UnitIsTrained; //This should shift queue filling rest with ut_None
  end;

  {Barracks has 11 resources and Recruits}
  TKMHouseBarracks = class(TKMHouse)
  public
    ResourceCount:array[1..11]of word;
    RecruitsInside:integer;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure AddResource(aResource:TResourceType);
    procedure AddMultiResource(aResource:TResourceType; aCount:integer);
    function TakeResource(aResource:TResourceType):boolean;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  public
    ResourceCount:array[1..28]of word;
    NotAcceptFlag:array[1..28]of boolean;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure AddResource(aResource:TResourceType);
    procedure AddMultiResource(aResource:TResourceType; aCount:integer);
    function TakeResource(aResource:TResourceType):boolean;
  end;


  TKMHousesCollection = class(TList)
  private
    fSelectedHouse: TKMHouse;
    function DoAddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID; aHBS:THouseBuildState):TKMHouse;
  public
    function AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
    function AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
    procedure Rem(PosX,PosY:integer);
    procedure Clear; override;
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
    function FindHouse(aType:THouseType; X,Y:word): TKMHouse;
    procedure Paint();
    property SelectedHouse: TKMHouse read fSelectedHouse write fSelectedHouse;
  end;

implementation
uses KM_DeliverQueue, KM_Unit1, KM_Terrain, KM_Render, KM_Units, KM_Users;


{ TKMHouse }
constructor TKMHouse.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
begin
  Inherited Create;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fHouseType:=aHouseType;
  fBuildState:=aBuildState;
  fOwner:=aOwner;
  fBuildSupplyWood:=0;
  fBuildSupplyStone:=0;
  fBuildingProgress:=0;
  fHealth:=0;
  fHasOwner:=false;
  fBuildingRepair:=false;
  fWareDelivery:=true;
  fResourceOrder[1]:=0;
  fResourceOrder[2]:=0;
  fResourceOrder[3]:=0;
  fResourceOrder[4]:=0;

  fTerrain.SetTileOwnership(fPosition,fHouseType,fOwner);

  if aBuildState=hbs_Done then begin //House was placed on map already Built e.g. in mission maker
    Self.Activate;
    fHealth:=HouseDAT[byte(fHouseType)].MaxHealth;
    fTerrain.SetHousePlan(fPosition,fHouseType,fdt_House); //Sets passability
  end else
    fTerrain.SetHousePlan(fPosition,fHouseType,fdt_None);
end;

destructor TKMHouse.Destroy;
begin
  fCurrentAction.Free;
  fTerrain.SetTileOwnership(fPosition,fHouseType,play_none);
  if fBuildState=hbs_Done then if Assigned(fPlayers.Player[byte(fOwner)]) then fPlayers.Player[byte(fOwner)].DestroyedHouse(fHouseType);
  Inherited;
end;

procedure TKMHouse.Activate;
var i,k:integer;
begin
  fPlayers.Player[byte(fOwner)].CreatedHouse(fHouseType); //Only activated houses count
  fTerrain.RevealCircle(fPosition,HouseDAT[byte(fHouseType)].Sight,100,fOwner);

  fCurrentAction:=THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_FlagShtok,ha_Flag1..ha_Flag3]);

  for i:=1 to 4 do
    case HouseInput[byte(fHouseType),i] of
      rt_None:    ;
      rt_Warfare: fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Always, di_Norm);
      rt_All:     fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Always, di_Norm);
      else for k:=1 to 5 do //Every new house needs 5 resourceunits
          fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Once, di_Norm);
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
 {Keep track on stone/wood reserve here as well}
procedure TKMHouse.IncBuildingProgress;
begin
  if IsComplete then exit;

  if (fBuildState=hbs_Wood)and(fBuildReserve = 0) then begin
    dec(fBuildSupplyWood);
    inc(fBuildReserve,50);
  end;
  if (fBuildState=hbs_Stone)and(fBuildReserve = 0) then begin
    dec(fBuildSupplyStone);
    inc(fBuildReserve,50);
  end;

  inc(fBuildingProgress,5); //is how many effort was put into building nevermind applied damage
  dec(fBuildReserve,5); //This is reserve we build from
  inc(fHealth,5);

  if (fBuildState=hbs_Wood)and(fBuildingProgress = HouseDAT[byte(fHouseType)].WoodCost*50) then begin
    fBuildState:=hbs_Stone;
    fBuildingProgress:=0;
  end;
  if (fBuildState=hbs_Stone)and(fBuildingProgress = HouseDAT[byte(fHouseType)].StoneCost*50) then begin
    fBuildState:=hbs_Done;
    Activate;
  end;
end; 

{Check if house is started to build, so to know if we need to init the building site or not}
function TKMHouse.IsStarted():boolean;
begin
  Result := fBuildingProgress > 0;
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
  for i:=1 to 4 do
  if (aResource = HouseInput[byte(fHouseType),i])or(aResource=rt_All) then
    inc(Result,fResourceIn[i]);
end;

function TKMHouse.CheckResOut(aResource:TResourceType):byte;
var i:integer;
begin
Result:=0;
  for i:=1 to 4 do
  if (aResource = HouseOutput[byte(fHouseType),i])or(aResource=rt_All) then
    inc(Result,fResourceOut[i]);
end;


{Check amount of order for given ID}
function TKMHouse.CheckResOrder(ID:byte):word;
begin
  Result:=fResourceOrder[ID];
end;


{Check if house has enough resource supply to be built depending on it's state}
function TKMHouse.CheckResToBuild():boolean;
begin
  Result:=false;
  if fBuildState=hbs_Wood then
    Result:=(fBuildSupplyWood>0)or(fBuildReserve>0);
  if fBuildState=hbs_Stone then
    Result:=(fBuildSupplyStone>0)or(fBuildReserve>0);
end;


procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = HouseOutput[byte(fHouseType),i] then
    begin
      inc(fResourceOut[i],aCount);
      fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,aResource,aCount);
    end;
end;


procedure TKMHouse.ResAddToIn(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  if HouseInput[byte(fHouseType),1]=rt_All then
    TKMHouseStore(Self).AddResource(aResource)
  else
  if HouseInput[byte(fHouseType),1]=rt_Warfare then
    TKMHouseBarracks(Self).AddResource(aResource)
  else
    for i:=1 to 4 do
    if aResource = HouseInput[byte(fHouseType),i] then
      inc(fResourceIn[i],aCount);
end;


procedure TKMHouse.ResAddToBuild(aResource:TResourceType);
begin
  case aResource of
    rt_Wood: inc(fBuildSupplyWood);
    rt_Stone: inc(fBuildSupplyStone);
  else Assert(false,'WIP house is not supposed to recieve '+TypeToString(aResource)+', right?');
  end;
end;


function TKMHouse.ResTakeFromIn(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = HouseInput[byte(fHouseType),i] then begin
    Assert(fResourceIn[i]>0);
    dec(fResourceIn[i]);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(Self,nil,aResource,dt_Once,di_Norm);
    Result:=true;
  end;
end;


function TKMHouse.ResTakeFromOut(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
case fHouseType of
ht_Store: if TKMHouseStore(Self).ResourceCount[byte(aResource)]>0 then begin
            dec(TKMHouseStore(Self).ResourceCount[byte(aResource)]);
            Result:=true;
          end;
else
          for i:=1 to 4 do
          if aResource = HouseOutput[byte(fHouseType),i] then begin
            dec(fResourceOut[i]);
            Result:=true;
            exit;
          end;
end;
end;


procedure TKMHouse.AddOrder(ID:byte; const Amount:byte=1);
begin
fResourceOrder[ID]:=EnsureRange(fResourceOrder[ID]+Amount,0,MAX_ORDER);
end;


procedure TKMHouse.RemOrder(ID:byte; const Amount:byte=1);
begin
  fResourceOrder[ID]:=EnsureRange(fResourceOrder[ID]-Amount,0,MAX_ORDER);
end;


procedure TKMHouse.SetState(aState: THouseState; aTime:integer);
begin
  fCurrentAction.TimeToAct:=aTime;
  fCurrentAction.SetState(aState);
end;


function TKMHouse.GetState:THouseState;
begin
  Result:=fCurrentAction.fHouseState;
end;


procedure TKMHouse.UpdateState;
begin
  if fBuildState<>hbs_Done then exit;
  
  fLastUpdateTime:= GetTickCount;

  inc(FlagAnimStep);
  inc(WorkAnimStep);
  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if FlagAnimStep mod 10 = 0 then fTerrain.RevealCircle(fPosition,HouseDAT[byte(fHouseType)].Sight,10,fOwner);
end;


{Return Entrance of the house, which is different than house position sometimes}
function TKMHouse.GetEntrance():TKMPoint;
begin
  Result.X:=GetPosition.X + HouseDAT[byte(fHouseType)].EntranceOffsetX;
  Result.Y:=GetPosition.Y;
end;


procedure TKMHouse.Paint;
begin
case fBuildState of
  hbs_Glyph: fRender.RenderHouseBuild(byte(fHouseType),fPosition.X, fPosition.Y);
  hbs_NoGlyph:; //Nothing
  hbs_Wood:
    begin
      fRender.RenderHouseWood(byte(fHouseType),
      fBuildingProgress/50/HouseDAT[byte(fHouseType)].WoodCost, //0...1 range
      fPosition.X, fPosition.Y);
      fRender.RenderHouseBuildSupply(byte(fHouseType), fBuildSupplyWood, fBuildSupplyStone, fPosition.X, fPosition.Y);
    end;
  hbs_Stone:
    begin
      fRender.RenderHouseStone(byte(fHouseType),
      fBuildingProgress/50/HouseDAT[byte(fHouseType)].StoneCost, //0...1 range
      fPosition.X, fPosition.Y);
      fRender.RenderHouseBuildSupply(byte(fHouseType), fBuildSupplyWood, fBuildSupplyStone, fPosition.X, fPosition.Y);
    end;
  else begin
    fRender.RenderHouseStone(byte(fHouseType),1,fPosition.X, fPosition.Y);
    fRender.RenderHouseSupply(byte(fHouseType),fResourceIn,fResourceOut,fPosition.X, fPosition.Y);
    if fCurrentAction=nil then exit;
    fRender.RenderHouseWork(byte(fHouseType),integer(fCurrentAction.fSubAction),WorkAnimStep,integer(fOwner),fPosition.X, fPosition.Y);
  end;
end;
end;

procedure TKMHouse.SetWareDelivery(AVal:boolean);
begin
  fWareDelivery := AVal;
  //@Krom: Here we should either enable or disable delivery of wares to this building.
  //I don't really understand how the delivery system works, so maybe you could do this?
  //@Lewin:Later, cos I don't know how it works either, although I've designed it LOL
  //This feature is not very important for now.
  //@Krom: True, it's not very important so do it later.
  //@Lewin: Okay, I've made it work. House only stores WareDelivery option, it is accessed
  //from DeliverQueue when choosing delivery job.
  //P.S. Keep this chat, it's fun to read ;-) 
end;


constructor TKMHouseSchool.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(UnitQueue) do
    UnitQueue[i]:=ut_None;
end;

procedure TKMHouseSchool.ResAddToIn(aResource:TResourceType; const aCount:integer=1);
begin
Inherited;
//If there's no unit in training then UnitIsStart
//So far thats that wouldbe an only proof way to avoid 2units train at once;
end;


procedure TKMHouseSchool.AddUnitToQueue(aUnit:TUnitType);
var i:integer;
begin
  for i:=1 to length(UnitQueue) do
  if UnitQueue[i]=ut_None then begin
    UnitQueue[i]:=aUnit;
    if i=1 then UnitIsStart;
    break;
  end;
end;


procedure TKMHouseSchool.RemUnitFromQueue(id:integer);
//var TK:TKMUnit;
begin
  //DoCancelTraining and remove untrained unit
  UnitIsTrained;
end;


procedure TKMHouseSchool.UnitIsStart;
var TK:TKMUnit;
begin
  //If there's yet no unit in training
  if CheckResIn(rt_Gold)=0 then exit;
  ResTakeFromIn(rt_Gold);
  TK:=fPlayers.Player[byte(fOwner)].AddUnit(UnitQueue[1],GetEntrance);//Create Unit
  TK.UnitTask:=TTaskSelfTrain.Create(TK,Self);
  //pUnit:=@TK;
end;


procedure TKMHouseSchool.UnitIsTrained;
var i:integer;
begin
  for i:=1 to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty
  if UnitQueue[1]<>ut_None then UnitIsStart;
  UnitTrainProgress:=0;
end;


constructor TKMHouseStore.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(ResourceCount) do begin
    ResourceCount[i]:=0;
    NotAcceptFlag[i]:=false;
  end;
end;


procedure TKMHouseStore.AddResource(aResource:TResourceType);
begin
inc(ResourceCount[byte(aResource)]);
fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,aResource,1);
end;


procedure TKMHouseStore.AddMultiResource(aResource:TResourceType; aCount:integer);
var i:integer;
begin
if InRange(aCount,1,1000) then
if aResource=rt_All then
  for i:=1 to length(ResourceCount) do begin
    inc(ResourceCount[i],aCount);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,TResourceType(i),aCount);
  end else begin
    inc(ResourceCount[byte(aResource)],aCount);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,aResource,aCount);
  end;
end;


function TKMHouseStore.TakeResource(aResource:TResourceType):boolean;
begin
if ResourceCount[byte(aResource)]>0 then begin
  dec(ResourceCount[byte(aResource)]);
  Result:=true;
end else begin
  Assert(false,'ResourceCount[byte(aResource)]>=0');
  Result:=false;
end;
end;


constructor TKMHouseBarracks.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(ResourceCount) do
    ResourceCount[i]:=0;
  RecruitsInside:=0;
end;


procedure TKMHouseBarracks.AddResource(aResource:TResourceType);
begin
  inc(ResourceCount[byte(aResource)-16]);
end;


procedure TKMHouseBarracks.AddMultiResource(aResource:TResourceType; aCount:integer);
var i:integer;
begin
if aResource=rt_Warfare then
  for i:=1 to length(ResourceCount) do inc(ResourceCount[i],aCount)
else
  inc(ResourceCount[byte(aResource)-16],aCount);    
end;


function TKMHouseBarracks.TakeResource(aResource:TResourceType):boolean;
begin
if ResourceCount[byte(aResource)-16]>0 then begin
  dec(ResourceCount[byte(aResource)-16]);
  Result:=true;
end else begin
  Assert(false,'ResourceCount[byte(aResource)-16]>=0');
  Result:=false;
end;
end;


{ THouseAction }
constructor THouseAction.Create(aHouse:TKMHouse; aHouseState: THouseState; const aTime:integer=0);
begin
  Inherited Create;
  fHouse:=aHouse;
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
  fHouse.WorkAnimStep:=0;
end;

procedure THouseAction.SubActionAdd(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction + aActionSet;
end;

procedure THouseAction.SubActionRem(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction - aActionSet;
end;


{ TKMHousesCollection }
function TKMHousesCollection.DoAddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID; aHBS:THouseBuildState):TKMHouse;
var T:integer;
begin
case aHouseType of
  ht_School:   T:=Inherited Add(TKMHouseSchool.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Barracks: T:=Inherited Add(TKMHouseBarracks.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Store:    T:=Inherited Add(TKMHouseStore.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  else         T:=Inherited Add(TKMHouse.Create(aHouseType,PosX,PosY,aOwner,aHBS));
end;
  if T=-1 then Result:=nil else Result:=Items[T];
end;

function TKMHousesCollection.AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
begin
  Result:=DoAddHouse(aHouseType,PosX,PosY,aOwner,hbs_Done);
end;

{Add a plan for house}
function TKMHousesCollection.AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
begin
  Result:=DoAddHouse(aHouseType,PosX,PosY,aOwner,hbs_Glyph);
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
end;


function TKMHousesCollection.FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
var
  i: integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if (TUnitType(HouseDAT[byte(TKMHouse(Items[I]).fHouseType)].OwnerType+1)=aUnitType)and //If Unit can work in here
       (not TKMHouse(Items[I]).fHasOwner)and                              //If there's yet no owner
       (TKMHouse(Items[I]).IsComplete) then                               //If house is built
    begin
      Result:= TKMHouse(Items[I]);
      if TKMHouse(Items[I]).fHouseType<>ht_Barracks then TKMHouse(Items[I]).fHasOwner:=true; //Become owner except Barracks;
      exit;
    end;
end;


function TKMHousesCollection.FindHouse(aType:THouseType; X,Y:word): TKMHouse;
var
  i: integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
      if (TKMHouse(Items[I]).fHouseType=aType)and
      (TKMHouse(Items[I]).IsComplete) then
      begin
        Result:= TKMHouse(Items[I]);
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
