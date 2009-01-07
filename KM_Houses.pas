unit KM_Houses;
interface
uses windows, math, classes, KromUtils, OpenGL, dglOpenGL, KromOGLUtils, KM_Global_Data, KM_Defaults;

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
    fOwnerID: TPlayerID;

    fBuildingProgress: word; //That is how many efforts were put into building (Wooding+Stoning)
    fHealth: word; //House condition/health

    fHasOwner: boolean; //which is some TKMUnit
    fOwnerAtHome: boolean;
    fBuildingRepair: boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: boolean; //If on then no wares will be delivered here

    fResourceIn:array[1..4] of byte;
    fResourceOut:array[1..4]of byte;

    fLastUpdateTime: Cardinal;
    FlagAnimStep: integer; //Used for Flags and Burning animation
    WorkAnimStep: integer; //Used for Work and etc..
    procedure SetWareDelivery(AVal:boolean);
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle

    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    destructor Destroy; override;
    procedure Activate;
    function HitTest(X, Y: Integer): Boolean; overload;

    procedure SetBuildingState(aState: THouseBuildState);
    procedure IncBuildingProgress;
    function IsComplete:boolean;

    procedure SetState(aState: THouseState; aTime:integer);
    function GetState:THouseState;
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

    procedure UpdateState;
    procedure Paint();
  end;

  {School has one unique property - queue of units to be trained, 1 wip + 5 in line}
  TKMHouseSchool = class(TKMHouse)
  public
    UnitQueue:array[1..6]of TUnitType; //Also used in UI
    UnitTrainProgress:byte; //Was it 12 steps in KaM?
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure AddUnitToQueue(aUnit:TUnitType); //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(id:integer); //Should remove unit from queue and shift rest up
    procedure UnitIsTrained; //This should Create new unit and shift queue filling rest with ut_None
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
    AcceptFlag:array[1..28]of boolean;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure AddResource(aResource:TResourceType);
    procedure AddMultiResource(aResource:TResourceType; aCount:integer);
    function TakeResource(aResource:TResourceType):boolean;
  end;


  TKMHousesCollection = class(TList)
  private
    fSelectedHouse: TKMHouse;
    procedure DoAddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID; aHBS:THouseBuildState);
  public
    procedure AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID);
    procedure AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID);
    procedure Rem(PosX,PosY:integer);
    procedure Clear; override;
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
    function FindStore(): TKMHouseStore;
    procedure Paint();
    property SelectedHouse: TKMHouse read fSelectedHouse write fSelectedHouse;
  end;

implementation
uses KM_DeliverQueue, KM_Unit1, KM_Terrain, KM_Render;


{ TKMHouse }
constructor TKMHouse.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
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
  fCurrentAction:=THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_FlagShtok,ha_Flag1..ha_Flag3]);

  for i:=1 to 4 do
    case HouseInput[byte(fHouseType),i] of
      rt_None:    ;
      rt_Warfare: ControlList.DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Always);
      rt_All:     ControlList.DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Always);
      else for k:=1 to 5 do //Every new house needs 5 resourceunits
          ControlList.DeliverList.AddNewDemand(Self,nil,HouseInput[byte(fHouseType),i],dt_Once);
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

procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = HouseOutput[byte(fHouseType),i] then
    begin
      inc(fResourceOut[i],aCount);
      ControlList.DeliverList.AddNewOffer(Self,aResource,aCount);
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


function TKMHouse.ResTakeFromIn(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = HouseInput[byte(fHouseType),i] then begin
    Assert(fResourceIn[i]>0);
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
          end;
end;
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
var
  TimeDelta: Cardinal;
begin
  if fBuildState<>hbs_Done then exit;

  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;

  inc(FlagAnimStep);
  inc(WorkAnimStep);
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
    fRender.RenderHouse(byte(fHouseType),fPosition.X, fPosition.Y);
    fRender.RenderHouseSupply(byte(fHouseType),fResourceIn,fResourceOut,fPosition.X, fPosition.Y);
    if fCurrentAction=nil then exit;
    fRender.RenderHouseWork(byte(fHouseType),integer(fCurrentAction.fSubAction),WorkAnimStep,integer(fOwnerID),fPosition.X, fPosition.Y);
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
end;


constructor TKMHouseSchool.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(UnitQueue) do
    UnitQueue[i]:=ut_None;
end;


procedure TKMHouseSchool.AddUnitToQueue(aUnit:TUnitType);
var i:integer;
begin
  for i:=1 to length(UnitQueue) do
  if UnitQueue[i]=ut_None then begin
    UnitQueue[i]:=aUnit;
    break;
  end;
end;


procedure TKMHouseSchool.RemUnitFromQueue(id:integer);
var i:integer;
begin
  //if id=1 then //DoCancelTraining and reset UnitTrainProgress!
  for i:=id to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift up
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty
end;


procedure TKMHouseSchool.UnitIsTrained;
var i:integer;
begin
  ControlList.AddUnit(fOwnerID,UnitQueue[1],fPosition);//Create Unit
  //Unit should recieve a Task to go outside of School, one cell down
  for i:=1 to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty
end;


constructor TKMHouseStore.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(ResourceCount) do begin
    ResourceCount[i]:=0;
    AcceptFlag[i]:=true;
  end;
end;


procedure TKMHouseStore.AddResource(aResource:TResourceType);
begin
inc(ResourceCount[byte(aResource)]);
ControlList.DeliverList.AddNewOffer(Self,aResource,1);
end;


procedure TKMHouseStore.AddMultiResource(aResource:TResourceType; aCount:integer);
var i:integer;
begin
if aResource=rt_All then
  for i:=1 to length(ResourceCount) do begin
    inc(ResourceCount[i],aCount);
    ControlList.DeliverList.AddNewOffer(Self,TResourceType(i),aCount);
  end else begin
    inc(ResourceCount[byte(aResource)],aCount);
    ControlList.DeliverList.AddNewOffer(Self,aResource,aCount);
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
//ControlList.DeliverList.AddNewOffer(Self,aResource,1); Doesn't need that
end;


procedure TKMHouseBarracks.AddMultiResource(aResource:TResourceType; aCount:integer);
var i:integer;
begin
if aResource=rt_Warfare then
  for i:=1 to length(ResourceCount) do begin
    inc(ResourceCount[i],aCount);
    //ControlList.DeliverList.AddNewOffer(Self,TResourceType(i),aCount); Doesn't need that
  end else begin
    inc(ResourceCount[byte(aResource)-16],aCount);
    //ControlList.DeliverList.AddNewOffer(Self,aResource,aCount); Doesn't need that
  end;
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

procedure TKMHousesCollection.DoAddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID; aHBS:THouseBuildState);
begin
case aHouseType of
  ht_School: Inherited Add(TKMHouseSchool.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Barracks: Inherited Add(TKMHouseBarracks.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Store:  Inherited Add(TKMHouseStore.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  else       Inherited Add(TKMHouse.Create(aHouseType,PosX,PosY,aOwner,aHBS));
end;
end;

procedure TKMHousesCollection.AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID);
begin
DoAddHouse(aHouseType,PosX,PosY,aOwner,hbs_Done);
end;

{Add a plan for house}
procedure TKMHousesCollection.AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID);
begin
DoAddHouse(aHouseType,PosX,PosY,aOwner,hbs_Glyph);
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
    if (HouseOwnerUnit[byte(TKMHouse(Items[I]).fHouseType)]=aUnitType)and //If Unit can work in here
       (not TKMHouse(Items[I]).fHasOwner)and                              //If there's yet no owner
       (TKMHouse(Items[I]).IsComplete) then                               //If house is built
    begin
      Result:= TKMHouse(Items[I]);
      if TKMHouse(Items[I]).fHouseType<>ht_Barracks then TKMHouse(Items[I]).fHasOwner:=true; //Become owner except Barracks;
      exit;
    end;
end;

function TKMHousesCollection.FindStore(): TKMHouseStore;
var
  i: integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
      if (TKMHouse(Items[I]).fHouseType=ht_Store)and
      (TKMHouse(Items[I]).IsComplete) then
      begin
        Result:= TKMHouseStore(Items[I]);
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
