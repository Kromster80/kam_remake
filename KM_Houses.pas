unit KM_Houses;
interface
uses Windows, Math, Classes, KromUtils, KM_Utils, OpenGL, dglOpenGL, KromOGLUtils, KM_Defaults, SysUtils, MMSystem,KM_CommonTypes;

  {Everything related to houses is here}
type
  TKMHouse = class;

  THouseAction = class(TObject)
  private
    fHouse:TKMHouse;
    TimeToAct:integer; //This one is unused in fact. All house states are controlled by Units
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
  public
    constructor Create(aHouse:TKMHouse; aHouseState: THouseState; const aTime:integer=0);
    procedure SetState(aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType; const aTime:integer=0);
    function GetWorkID():byte;
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    property ActionType: THouseState read fHouseState;
  end;


  TKMHouse = class(TObject)
  private
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
    fHouseType: THouseType; //House type
    fBuildState: THouseBuildState; // = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);
    fOwner: TPlayerID; //House owner player, determines flag color as well

    fBuildSupplyWood: byte; //How much Wood was delivered to house building site
    fBuildSupplyStone: byte; //How much Stone was delivered to house building site
    fBuildReserve: byte; //Take one build supply resource into reserve and "build from it"
    fBuildingProgress: word; //That is how many efforts were put into building (Wooding+Stoning)
    fDamage: word; //Damaged inflicted to house

    fHasOwner: boolean; //which is some TKMUnit
    fBuildingRepair: boolean; //If on and the building is damaged then labourers will come and repair it
    fRepairID:integer; //Switch to remember TaskID of asked repair
    fWareDelivery: boolean; //If on then no wares will be delivered here

    fResourceIn:array[1..4] of byte; //Resource count in input
    fResourceOut:array[1..4]of byte; //Resource count in output

    fResourceOrder:array[1..4]of word; //If HousePlaceOrders=true then here are production orders

    fLastUpdateTime: cardinal;
    FlagAnimStep: cardinal; //Used for Flags and Burning animation
    WorkAnimStep: cardinal; //Used for Work and etc.. which is not in sync with Flags

    ScheduleForRemoval:boolean;
    procedure SetWareDelivery(AVal:boolean);

    procedure MakeSound();
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle

    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    destructor Destroy; override;

    procedure Activate(aWasBuilt:boolean);
    procedure DemolishHouse(DoSilent:boolean);

    property GetPosition:TKMPoint read fPosition;
    function GetEntrance(DebugSender:TObject):TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property GetHouseType:THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write SetWareDelivery;
    property GetHasOwner:boolean read fHasOwner write fHasOwner;
    function GetHealth():word;

    procedure SetBuildingState(aState: THouseBuildState);
    property GetBuildingState: THouseBuildState read fBuildState;
    procedure IncBuildingProgress;
    procedure AddDamage(aAmount:word);
    procedure AddRepair(aAmount:word=5);
    procedure UpdateDamage();
    procedure EnableRepair();
    procedure DisableRepair();

    function IsStarted:boolean;
    function IsStone:boolean;
    function IsComplete:boolean;
    function IsDamaged:boolean;
    property IsDestroyed:boolean read ScheduleForRemoval;

    procedure SetState(aState: THouseState; aTime:integer);
    function GetState:THouseState;

    function CheckResIn(aResource:TResourceType):word; virtual;
    function CheckResOut(aResource:TResourceType):byte;
    function CheckResOrder(ID:byte):word;
    function CheckResToBuild():boolean;
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); virtual; //override for School and etc..
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToBuild(aResource:TResourceType);
    function ResTakeFromIn(aResource:TResourceType):boolean;
    function ResTakeFromOut(aResource:TResourceType):boolean;
    procedure ResAddOrder(ID:byte; const Amount:byte=1);
    procedure ResRemOrder(ID:byte; const Amount:byte=1);

    procedure UpdateState;
    procedure Paint; virtual;
  end;

  {SwineStable has unique property - it needs to accumulate some resource before production begins, also special animation}
  TKMHouseSwineStable = class(TKMHouse)
  public
    BeastAge:array[1..5]of byte; //Each beasts "age". Once Best reaches age 3+1 it's ready
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    function FeedBeasts():byte;
    procedure TakeBeast(ID:byte);
    procedure Paint; override;
  end;

  TKMHouseInn = class(TKMHouse)
  private
    Eater:array[1..6]of record //only 6 units are allowed in the inn
      UnitType:TUnitType;
      FoodKind:byte; //What kind of food eater eats
      AnimStep:cardinal;
    end;
  public
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    function EaterGetsInside(aUnitType:TUnitType):byte;
    procedure UpdateEater(aID:byte; aFoodKind:byte);
    procedure EatersGoesOut(aID:byte);
    function HasFood:boolean;
    function HasSpace:boolean;
    procedure Paint(); override; //Render all eaters
  end;

  {School has one unique property - queue of units to be trained, 1 wip + 5 in line}
  TKMHouseSchool = class(TKMHouse)
  private
    UnitWIP:Pointer;
    HideOneGold:boolean; //Hide the gold incase Player cancels the training, then we won't need to tweak DeliverQueue order
    UnitTrainProgress:byte; //Was it 150 steps in KaM?
  public
    UnitQueue:array[1..6]of TUnitType; //Also used in UI
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); override;
    procedure AddUnitToQueue(aUnit:TUnitType); //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(id:integer); //Should remove unit from queue and shift rest up
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure UnitTrainingComplete; //This should shift queue filling rest with ut_None
    function GetTrainingProgress():byte;
  end;

  {Barracks has 11 resources and Recruits}
  TKMHouseBarracks = class(TKMHouse)
  public
    ResourceCount:array[1..11]of word;
    RecruitsInside:integer;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure AddMultiResource(aResource:TResourceType; const aCount:word=1);
    function CheckResIn(aResource:TResourceType):word; override;
    function TakeResource(aResource:TResourceType):boolean;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  public
    ResourceCount:array[1..28]of word;
    NotAcceptFlag:array[1..28]of boolean;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
    procedure ToggleAcceptFlag(aTagID:byte);
    procedure AddMultiResource(aResource:TResourceType; const aCount:word=1);
  end;


  TKMHousesCollection = class(TKMList)
  private
    fSelectedHouse: TKMHouse;
    function DoAddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID; aHBS:THouseBuildState):TKMHouse;
  public
    function AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
    function AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerID):TKMHouse;
    function Rem(aHouse:TKMHouse):boolean;
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindHouse(aType:THouseType; X,Y:word; const Index:byte=1): TKMHouse;
    procedure Paint();
    property SelectedHouse: TKMHouse read fSelectedHouse write fSelectedHouse;
  end;

implementation
uses KM_DeliverQueue, KM_Unit1, KM_Terrain, KM_Render, KM_Units, KM_PlayersCollection, KM_SoundFX, KM_Viewport;


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
  fDamage:=0;
  fHasOwner:=false;
  fBuildingRepair:=false; //Repair mode off by default
  fRepairID:=0;
  fWareDelivery:=true;
  fResourceOrder[1]:=0;
  fResourceOrder[2]:=0;
  fResourceOrder[3]:=0;
  fResourceOrder[4]:=0;
  ScheduleForRemoval:=false;

  if aBuildState=hbs_Done then begin //House was placed on map already Built e.g. in mission maker
    Self.Activate(false);
    fBuildingProgress:=HouseDAT[byte(fHouseType)].MaxHealth;
    fTerrain.SetHouse(fPosition, fHouseType, hs_Built, fOwner); //Sets passability
  end else
    fTerrain.SetHouse(fPosition, fHouseType, hs_Plan, play_None); //Terrain remains neutral yet
end;

destructor TKMHouse.Destroy;
begin
  FreeAndNil(fCurrentAction);
  if (fBuildState=hbs_Done) and Assigned(fPlayers) and Assigned(fPlayers.Player[byte(fOwner)]) then
    fPlayers.Player[byte(fOwner)].DestroyedHouse(fHouseType);
  Inherited;
end;

procedure TKMHouse.Activate(aWasBuilt:boolean);
var i:integer; Res:TResourceType;
begin
  fPlayers.Player[byte(fOwner)].CreatedHouse(fHouseType,aWasBuilt); //Only activated houses count
  fTerrain.RevealCircle(fPosition,HouseDAT[byte(fHouseType)].Sight,100,fOwner);

  fCurrentAction:=THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_FlagShtok,ha_Flag1..ha_Flag3]);

  for i:=1 to 4 do
  begin
    Res := HouseInput[byte(fHouseType),i];
    with fPlayers.Player[byte(fOwner)].DeliverList do
    case Res of
      rt_None:    ;
      rt_Warfare: AddNewDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      rt_All:     AddNewDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      else        AddNewDemand(Self, nil, Res, 5, dt_Once,   di_Norm); //Every new house needs 5 resourceunits
    end;
  end;

end;


procedure TKMHouse.DemolishHouse(DoSilent:boolean);
begin
  if not DoSilent then
    if GetBuildingState = hbs_Glyph then fSoundLib.Play(sfx_click)
    else fSoundLib.Play(sfx_HouseDestroy,GetPosition);
  ScheduleForRemoval:=true;
  //Dispose of delivery tasks performed in DeliverQueue unit
  fPlayers.Player[byte(fOwner)].DeliverList.RemoveOffer(Self);
  fPlayers.Player[byte(fOwner)].DeliverList.RemoveDemand(Self);
  fTerrain.SetHouse(fPosition,fHouseType,hs_None,play_none);
  fTerrain.AddHouseRemainder(fPosition,fHouseType,fBuildState);
end;


{Return Entrance of the house, which is different than house position sometimes}
function TKMHouse.GetEntrance(DebugSender:TObject):TKMPoint;
begin
  //fLog.AppendLog(TypeToString(GetPosition));
  Result.X:=GetPosition.X + HouseDAT[byte(fHouseType)].EntranceOffsetX;
  Result.Y:=GetPosition.Y;
end;


function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result:=false;
  if (X-fPosition.X+3 in [1..4])and(Y-fPosition.Y+4 in [1..4]) then
  if HousePlanYX[integer(fHouseType),Y-fPosition.Y+4,X-fPosition.X+3]<>0 then begin
    Result:=true;
    exit;
  end;
end;


function TKMHouse.GetHealth():word;
begin
  Result:=EnsureRange(fBuildingProgress-fDamage,0,maxword);
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

  if (fBuildState=hbs_Wood)and(fBuildingProgress = HouseDAT[byte(fHouseType)].WoodCost*50) then begin
    fBuildState:=hbs_Stone;
    //fBuildingProgress:=0;
  end;
  if (fBuildState=hbs_Stone)and(fBuildingProgress-HouseDAT[byte(fHouseType)].WoodCost*50 = HouseDAT[byte(fHouseType)].StoneCost*50) then begin
    fBuildState:=hbs_Done;
    Activate(true);
  end;
end;


{Add damage to the house}
procedure TKMHouse.AddDamage(aAmount:word);
begin
  fDamage:= fDamage + aAmount;
  if (BuildingRepair)and(fRepairID=0) then
    fRepairID:=MyPlayer.BuildList.AddHouseRepair(Self);
  UpdateDamage();
end;


{Add repair to the house}
procedure TKMHouse.AddRepair(aAmount:word=5);
begin
  fDamage:= EnsureRange(fDamage - aAmount,0,maxword);
  if (fDamage=0)and(fRepairID<>0) then begin
    MyPlayer.BuildList.CloseHouseRepair(fRepairID);
    fRepairID:=0;
  end;
  UpdateDamage();
end;


{Update house damage animation}
procedure TKMHouse.UpdateDamage();
begin
  fCurrentAction.SubActionRem([ha_Fire1,ha_Fire2,ha_Fire3,ha_Fire4,ha_Fire5,ha_Fire6,ha_Fire7,ha_Fire8]);
  if fDamage >   0 then fCurrentAction.SubActionAdd([ha_Fire1]);
  if fDamage >  50 then fCurrentAction.SubActionAdd([ha_Fire2]);
  if fDamage > 100 then fCurrentAction.SubActionAdd([ha_Fire3]);
  if fDamage > 150 then fCurrentAction.SubActionAdd([ha_Fire4]);
  if fDamage > 200 then fCurrentAction.SubActionAdd([ha_Fire5]);
  if fDamage > 250 then fCurrentAction.SubActionAdd([ha_Fire6]);
  if fDamage > 300 then fCurrentAction.SubActionAdd([ha_Fire7]);
  if fDamage > 350 then fCurrentAction.SubActionAdd([ha_Fire8]);
  {House gets destroyed in UpdateState loop}
end;


{if house is damaged then add repair to buildlist}
procedure TKMHouse.EnableRepair();
begin
  BuildingRepair:=true;
  AddDamage(0); //Shortcut to refresh of damage
end;


{if house is damaged then remove repair from buildlist and free up workers}
procedure TKMHouse.DisableRepair();
begin
  BuildingRepair:=false;
  AddRepair(0); //Shortcut to refresh of damage
end;


{Check if house is started to build, so to know if we need to init the building site or not}
function TKMHouse.IsStarted():boolean;
begin
  Result := fBuildingProgress > 0;
end;


function TKMHouse.IsStone:boolean;
begin
  Result := fBuildState = hbs_Stone;
end;


{Check if house is completely built, nevermind the damage}
function TKMHouse.IsComplete():boolean;
begin
  Result := fBuildState = hbs_Done;
end;


{Check if house is damaged}
function TKMHouse.IsDamaged():boolean;
begin
  Result := fDamage <> 0;
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


{How much resources house has in Input}
function TKMHouse.CheckResIn(aResource:TResourceType):word;
var i:integer;
begin
Result:=0;
  for i:=1 to 4 do
  if (aResource = HouseInput[byte(fHouseType),i])or(aResource=rt_All) then
    inc(Result,fResourceIn[i]);
end;


{How much resources house has in Output}
function TKMHouse.CheckResOut(aResource:TResourceType):byte;
var i:integer;
begin
Result:=0;
  for i:=1 to 4 do
  if (aResource = HouseOutput[byte(fHouseType),i])or(aResource=rt_All) then
    inc(Result,fResourceOut[i]);
end;


{Check amount of placed order for given ID}
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


procedure TKMHouse.ResAddToIn(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  if HouseInput[byte(fHouseType),1]=rt_All then
    TKMHouseStore(Self).AddMultiResource(aResource)
  else
  if HouseInput[byte(fHouseType),1]=rt_Warfare then
    TKMHouseBarracks(Self).AddMultiResource(aResource)
  else
    for i:=1 to 4 do
    if aResource = HouseInput[byte(fHouseType),i] then
      inc(fResourceIn[i],aCount);
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


{Add resources to building process}
procedure TKMHouse.ResAddToBuild(aResource:TResourceType);
begin
  case aResource of
    rt_Wood: inc(fBuildSupplyWood);
    rt_Stone: inc(fBuildSupplyStone);
  else fLog.AssertToLog(false,'WIP house is not supposed to recieve '+TypeToString(aResource)+', right?');
  end;
end;


function TKMHouse.ResTakeFromIn(aResource:TResourceType):boolean;
var i:integer;
begin
Result:=false;
if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = HouseInput[byte(fHouseType),i] then begin
    fLog.AssertToLog(fResourceIn[i]>0,'fResourceIn[i]>0');
    dec(fResourceIn[i]);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(Self,nil,aResource,1,dt_Once,di_Norm);
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


{Place production order}
procedure TKMHouse.ResAddOrder(ID:byte; const Amount:byte=1);
begin
  fResourceOrder[ID]:=EnsureRange(fResourceOrder[ID]+Amount,0,MAX_ORDER);
end;


{Reduce production order amount}
procedure TKMHouse.ResRemOrder(ID:byte; const Amount:byte=1);
begin
  fResourceOrder[ID]:=EnsureRange(fResourceOrder[ID]-Amount,0,MAX_ORDER);
end;


procedure TKMHouse.MakeSound();
var WorkID,Step:byte;
begin

  //Do not play sounds if house is invisible to MyPlayer
  if fTerrain.CheckTileRevelation(fPosition.X, fPosition.Y, MyPlayer.PlayerID) < 255 then exit;

  WorkID:=fCurrentAction.GetWorkID;
  if WorkID=0 then exit;

  Step:=HouseDAT[byte(fHouseType)].Anim[WorkID].Count;
  if Step=0 then exit;
  Step:=WorkAnimStep mod Step;

  case fHouseType of //Various buildings and HouseActions producing sounds
    ht_Mill:          if (WorkID = 2)and(Step = 0) then fSoundLib.Play(sfx_mill,GetPosition);
    ht_CoalMine:      if (WorkID = 1)and(Step = 5) then fSoundLib.Play(sfx_coaldown,GetPosition)
                      else if (WorkID = 1)and(Step = 24) then fSoundLib.Play(sfx_CoalMineThud,GetPosition,true,0.8)
                      else if (WorkID = 2)and(Step = 7) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 8) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4) //echo
                      else if (WorkID = 5)and(Step = 1) then fSoundLib.Play(sfx_coaldown,GetPosition);
    ht_IronMine:      if (WorkID = 2)and(Step = 7) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 8) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4); //echo
    ht_GoldMine:      if (WorkID = 2)and(Step = 5) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 6) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4); //echo
    ht_SawMill:       if (WorkID = 2)and(Step = 1) then fSoundLib.Play(sfx_saw,GetPosition);
    ht_Wineyard:      if (WorkID = 2)and(Step in [1,7,13,19]) then fSoundLib.Play(sfx_wineStep,GetPosition)
                      else if (WorkID = 5)and(Step = 14) then fSoundLib.Play(sfx_wineDrain,GetPosition,true,1.5)
                      else if (WorkID = 1)and(Step = 10) then fSoundLib.Play(sfx_wineDrain,GetPosition,true,1.5);
    ht_Bakery:        if (WorkID = 3)and(Step in [6,25]) then fSoundLib.Play(sfx_BakerSlap,GetPosition);
    ht_Quary:         if (WorkID = 2)and(Step in [4,13]) then fSoundLib.Play(sfx_QuarryClink,GetPosition)
                      else if (WorkID = 5)and(Step in [4,13,22]) then fSoundLib.Play(sfx_QuarryClink,GetPosition);
    ht_WeaponSmithy:  if (WorkID = 1)and(Step in [17,22]) then fSoundLib.Play(sfx_BlacksmithFire,GetPosition)
                      else if (WorkID = 2)and(Step in [10,25]) then fSoundLib.Play(sfx_BlacksmithBang,GetPosition)
                      else if (WorkID = 3)and(Step in [10,25]) then fSoundLib.Play(sfx_BlacksmithBang,GetPosition)
                      else if (WorkID = 4)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire,GetPosition)
                      else if (WorkID = 5)and(Step = 12) then fSoundLib.Play(sfx_BlacksmithBang,GetPosition);
    ht_ArmorSmithy:   if (WorkID = 2)and(Step in [13,28]) then fSoundLib.Play(sfx_BlacksmithBang,GetPosition)
                      else if (WorkID = 3)and(Step in [13,28]) then fSoundLib.Play(sfx_BlacksmithBang,GetPosition)
                      else if (WorkID = 4)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire,GetPosition)
                      else if (WorkID = 5)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire,GetPosition);
    ht_Metallurgists: if (WorkID = 3)and(Step = 6) then fSoundLib.Play(sfx_metallurgists,GetPosition)
                      else if (WorkID = 4)and(Step in [16,20]) then fSoundLib.Play(sfx_wineDrain,GetPosition);
    ht_IronSmithy:    if (WorkID = 2)and(Step in [1,16]) then fSoundLib.Play(sfx_metallurgists,GetPosition)
                      else if (WorkID = 3)and(Step = 1) then fSoundLib.Play(sfx_metallurgists,GetPosition)
                      else if (WorkID = 3)and(Step = 13) then fSoundLib.Play(sfx_wineDrain,GetPosition);
    ht_WeaponWorkshop:if (WorkID = 2)and(Step in [1,10,19]) then fSoundLib.Play(sfx_saw,GetPosition)
                      else if (WorkID = 3)and(Step in [10,21]) then fSoundLib.Play(sfx_CarpenterHammer,GetPosition)
                      else if (WorkID = 4)and(Step in [2,13]) then fSoundLib.Play(sfx_CarpenterHammer,GetPosition);
    ht_ArmorWorkshop: if (WorkID = 2)and(Step in [3,13,23]) then fSoundLib.Play(sfx_saw,GetPosition)
                      else if (WorkID = 3)and(Step in [17,28]) then fSoundLib.Play(sfx_CarpenterHammer,GetPosition)
                      else if (WorkID = 4)and(Step in [10,20]) then fSoundLib.Play(sfx_CarpenterHammer,GetPosition);
    ht_Tannery:       if (WorkID = 2)and(Step = 5) then fSoundLib.Play(sfx_Leather,GetPosition,true,0.8);
    ht_Butchers:      if (WorkID = 2)and(Step in [8,16,24]) then fSoundLib.Play(sfx_ButcherCut,GetPosition)
                      else if (WorkID = 3)and(Step in [9,21]) then fSoundLib.Play(sfx_SausageString,GetPosition);
    ht_Swine:         if ((WorkID = 2)and(Step in [10,20]))or((WorkID = 3)and(Step = 1)) then fSoundLib.Play(sfx_ButcherCut,GetPosition);
  end;
end;


procedure TKMHouse.UpdateState;
begin
  if fBuildState<>hbs_Done then exit;

  fLastUpdateTime := TimeGetTime;

  if (GetHealth=0)and(fBuildState>=hbs_Wood) then DemolishHouse(false);

  MakeSound(); //Make some sound/noise along the work

  inc(FlagAnimStep);
  inc(WorkAnimStep);

  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if FlagAnimStep mod 10 = 0 then fTerrain.RevealCircle(fPosition,HouseDAT[byte(fHouseType)].Sight,10,fOwner);
end;


procedure TKMHouse.Paint();
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
      (fBuildingProgress/50-HouseDAT[byte(fHouseType)].WoodCost)/HouseDAT[byte(fHouseType)].StoneCost, //0...1 range
      fPosition.X, fPosition.Y);
      fRender.RenderHouseBuildSupply(byte(fHouseType), fBuildSupplyWood, fBuildSupplyStone, fPosition.X, fPosition.Y);
    end;
  else begin
    fRender.RenderHouseStone(byte(fHouseType),1,fPosition.X, fPosition.Y);
    fRender.RenderHouseSupply(byte(fHouseType),fResourceIn,fResourceOut,fPosition.X, fPosition.Y);
    if fCurrentAction=nil then exit;
    fRender.RenderHouseWork(byte(fHouseType),integer(fCurrentAction.fSubAction),WorkAnimStep,byte(fOwner),fPosition.X, fPosition.Y);
  end;
end;
end;

procedure TKMHouse.SetWareDelivery(AVal:boolean);
begin
  fWareDelivery := AVal;
end;


{TKMHouseSwineStable}
constructor TKMHouseSwineStable.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(BeastAge) do
    BeastAge[i]:=0;
end;


//Return ID of beast that has grown up
function TKMHouseSwineStable.FeedBeasts():byte;
var i:integer;
begin
  Result:=0;
  inc(BeastAge[Random(5)+1]); //Let's hope it never overflows MAX
  for i:=1 to length(BeastAge) do
    if BeastAge[i]>3 then
      Result:=i;
end;


procedure TKMHouseSwineStable.TakeBeast(ID:byte);
begin
  if ID<>0 then
    if BeastAge[ID]>3 then
      BeastAge[ID]:=0;
end;


procedure TKMHouseSwineStable.Paint;
var i:integer;
begin
  inherited;
  if (fBuildState<>hbs_Done) then exit;
  for i:=1 to 5 do
    if BeastAge[i]>0 then
      fRender.RenderHouseStableBeasts(byte(fHouseType), i, min(BeastAge[i],3), WorkAnimStep, fPosition.X, fPosition.Y);
  if (fCurrentAction<>nil) then //Overlay, not entirely correct, but works ok
  fRender.RenderHouseWork(byte(fHouseType),integer(fCurrentAction.fSubAction),WorkAnimStep,byte(fOwner),fPosition.X, fPosition.Y);
end;


constructor TKMHouseInn.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=low(Eater) to high(Eater) do
    Eater[i].UnitType:=ut_None;
end;


function TKMHouseInn.EaterGetsInside(aUnitType:TUnitType):byte;
var i:integer;
begin
  Result:=0;
  for i:=low(Eater) to high(Eater) do
  if Eater[i].UnitType=ut_None then
  begin
    Eater[i].UnitType:=aUnitType;
    Eater[i].FoodKind:=0;
    Eater[i].AnimStep:=FlagAnimStep;
    Result:=i;
    exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aID:byte; aFoodKind:byte);
begin
  if aID=0 then exit;
  Eater[aID].FoodKind:=aFoodKind; //Order is Wine-Bread-Sausages-Fish
  Eater[aID].AnimStep:=0;
end;


procedure TKMHouseInn.EatersGoesOut(aID:byte);
begin
  if aID=0 then exit;
  Eater[aID].UnitType:=ut_None;
end;


function TKMHouseInn.HasFood:boolean;
begin
  Result:=(CheckResIn(rt_Sausages)+CheckResIn(rt_Bread)+CheckResIn(rt_Wine)+CheckResIn(rt_Fish)>0);
end;

function TKMHouseInn.HasSpace:boolean;
var
  i: integer;
begin
  Result:=false;
  for i:=low(Eater) to high(Eater) do
    Result := Result or (Eater[i].UnitType=ut_None);
end;


procedure TKMHouseInn.Paint;
const
  offX:array[1..3]of single = (-0.5, 0.0, 0.5);
  offY:array[1..3]of single = ( 0.35, 0.4, 0.45);
var i:integer; UnitType,AnimAct,AnimDir:byte; AnimStep:cardinal;
begin
  inherited;
  if (fBuildState<>hbs_Done) then exit;

  for i:=1 to 6 do
  if (Eater[i].UnitType<>ut_None)and(Eater[i].FoodKind<>0) then
  begin
    UnitType:=byte(Eater[i].UnitType);
    AnimAct:=byte(ua_Eat);
    AnimDir:=Eater[i].FoodKind*2 - 1 + ((i-1) div 3);
    fLog.AssertToLog(InRange(AnimDir,1,8),'InRange(AnimDir,1,8)');
    AnimStep:=FlagAnimStep-Eater[i].AnimStep; //Delta is our AnimStep
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner),
      fPosition.X+offX[(i-1) mod 3 +1],
      fPosition.Y+offY[(i-1) mod 3 +1], false);
  end;
end;


constructor TKMHouseSchool.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(UnitQueue) do
    UnitQueue[i]:=ut_None;
  UnitWIP:=nil;
end;


procedure TKMHouseSchool.ResAddToIn(aResource:TResourceType; const aCount:integer=1);
begin
  Inherited;
  if UnitWIP=nil then StartTrainingUnit;
end;


procedure TKMHouseSchool.AddUnitToQueue(aUnit:TUnitType);
var i:integer;
begin
  for i:=1 to length(UnitQueue) do
  if UnitQueue[i]=ut_None then begin
    UnitQueue[i]:=aUnit;
    if i=1 then StartTrainingUnit; //If thats the first unit then start training it
    break;
  end;
end;


//DoCancelTraining and remove untrained unit
procedure TKMHouseSchool.RemUnitFromQueue(id:integer);
var i:integer;
begin
  if UnitQueue[id]=ut_None then exit; //Ignore clicks on empty queue items

  if id = 1 then begin
    SetState(hst_Idle,0);
    if UnitWIP<>nil then begin
      TKMUnit(UnitWIP).RemoveUntrainedFromSchool; //Make sure unit started training
      HideOneGold:=false;
    end;
    UnitWIP:=nil;
  end;

  for i:=id to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty

  if id = 1 then
    if UnitQueue[1]<>ut_None then StartTrainingUnit;
end;


procedure TKMHouseSchool.StartTrainingUnit;
begin
  //If there's yet no unit in training
  if UnitQueue[1]=ut_None then exit;
  if CheckResIn(rt_Gold)=0 then exit;
  HideOneGold:=true;
  UnitWIP:=fPlayers.Player[byte(fOwner)].TrainUnit(UnitQueue[1],GetEntrance(Self));//Create Unit
  TKMUnit(UnitWIP).SetUnitTask:=TTaskSelfTrain.Create(UnitWIP,Self);
end;


//To be called only by Unit itself when it's trained!
procedure TKMHouseSchool.UnitTrainingComplete;
var i:integer;
begin
  UnitWIP:=nil;
  ResTakeFromIn(rt_Gold); //Do the goldtaking
  HideOneGold:=false;
  for i:=1 to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty
  if UnitQueue[1]<>ut_None then StartTrainingUnit;
  UnitTrainProgress:=0;
end;


function TKMHouseSchool.GetTrainingProgress():byte;
begin
  Result:=0;
  if UnitWIP=nil then exit;
  Result:=EnsureRange(round(
  ((fCurrentAction.GetWorkID-1)*30+30-TUnitActionStay(TKMUnit(UnitWIP).UnitAction).HowLongLeftToStay)
  /1.5),0,100); //150 steps into 0..100 range
  //Substeps could be asked from Unit.ActionStay.TimeToStay, but it's a private field now
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


procedure TKMHouseStore.AddMultiResource(aResource:TResourceType; const aCount:word=1);
var i:integer;
begin
if aResource=rt_All then
  for i:=1 to length(ResourceCount) do begin
    ResourceCount[i]:=EnsureRange(ResourceCount[i]+aCount,0,MAXWORD);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,TResourceType(i),aCount);
  end
else
if aResource in [rt_Trunk..rt_Fish] then begin
    ResourceCount[byte(aResource)]:=EnsureRange(ResourceCount[byte(aResource)]+aCount,0,MAXWORD);
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewOffer(Self,aResource,aCount);
  end
else
fLog.AssertToLog(false,'Cant''t add such resource '+TypeToString(aResource));
end;


procedure TKMHouseStore.ToggleAcceptFlag(aTagID:byte);
var i:integer; ApplyCheat:boolean;
begin
  if aTagID=0 then exit; //@Lewin: Dunno why thats happening sometimes..

  if CHEATS_ENABLED then begin
    ApplyCheat:=true;

    for i:=1 to length(ResourceCount) do
      ApplyCheat := ApplyCheat and (NotAcceptFlag[i] = bool(CheatStorePattern[i]));

    if ApplyCheat and (aTagID=26) then begin
      AddMultiResource(rt_All,10);
      exit;
    end;
  end;

  NotAcceptFlag[aTagID] := not NotAcceptFlag[aTagID];
end;


constructor TKMHouseBarracks.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerID; aBuildState:THouseBuildState);
var i:integer;
begin
  Inherited;
  for i:=1 to length(ResourceCount) do
    ResourceCount[i]:=0;
  RecruitsInside:=0;
end;


procedure TKMHouseBarracks.AddMultiResource(aResource:TResourceType; const aCount:word=1);
//const MAX_WARFARE_IN_BARRACKS:=200;  //Has to influence ware delivery
var i:integer;
begin
if aResource=rt_Warfare then
  for i:=1 to length(ResourceCount) do
    ResourceCount[i]:=EnsureRange(ResourceCount[i]+aCount,0,MAXWORD)
else
if aResource in [rt_Shield..rt_Horse] then
  ResourceCount[byte(aResource)-16]:=EnsureRange(ResourceCount[byte(aResource)-16]+aCount,0,MAXWORD)
else
  fLog.AssertToLog(false,'Cant''t add such resource '+TypeToString(aResource));
end;


function TKMHouseBarracks.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [rt_Shield..rt_Horse] then
    Result:=ResourceCount[byte(aResource)-16]
  else
    Result:=0;
end;


function TKMHouseBarracks.TakeResource(aResource:TResourceType):boolean;
begin
  Result:=false;
  if aResource in [rt_Shield..rt_Horse] then
    if ResourceCount[byte(aResource)-16]>0 then begin
      dec(ResourceCount[byte(aResource)-16]);
      Result:=true;
    end else
      fLog.AssertToLog(false,'ResourceCount[byte(aResource)-16]<0');
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

function THouseAction.GetWorkID():byte;
begin
  if ha_Work1 in fSubAction then Result:=1 else
  if ha_Work2 in fSubAction then Result:=2 else
  if ha_Work3 in fSubAction then Result:=3 else
  if ha_Work4 in fSubAction then Result:=4 else
  if ha_Work5 in fSubAction then Result:=5 else
    Result:=0;
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
  ht_Swine:    T:=Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Stables:  T:=Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  ht_Inn:      T:=Inherited Add(TKMHouseInn.Create(aHouseType,PosX,PosY,aOwner,aHBS));
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


function TKMHousesCollection.Rem(aHouse:TKMHouse):boolean;
begin
  Remove(aHouse);
  Result := true;
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


//Should find closest house to Loc
function TKMHousesCollection.FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
var
  i: integer;
  Dist,Bid:single;
begin
  Result:= nil;
  Bid:=0;

  for I := 0 to Count - 1 do
    if (TUnitType(HouseDAT[byte(TKMHouse(Items[I]).fHouseType)].OwnerType+1)=aUnitType)and //If Unit can work in here
       (not TKMHouse(Items[I]).fHasOwner)and                              //If there's yet no owner
       (TKMHouse(Items[I]).IsComplete) then                               //If house is built
    begin

      Dist:=KMLength(Loc,TKMHouse(Items[I]).GetPosition);

      //Always prefer Towers to Barracks by making Barracks Bid much less attractive
      if TKMHouse(Items[I]).GetHouseType = ht_Barracks then Dist:=Dist*1000;

      if (Bid=0)or(Bid>Dist) then
      begin
        Bid:=Dist;
        Result := TKMHouse(Items[I]);
      end;

    end;

 if Result<>nil then
 if Result.fHouseType<>ht_Barracks then Result.fHasOwner:=true; //Become owner except Barracks;
end;


function TKMHousesCollection.FindHouse(aType:THouseType; X,Y:word; const Index:byte=1): TKMHouse;
var
  i,id: integer;
  UsePosition: boolean;
  BestMatch,Dist: single;
begin
  Result := nil;
  id := 0;
  BestMatch := -1; //Use -1 value to init variable on first run
  UsePosition := X*Y<>0; //Calculate this once to save computing lots of multiplications
  fLog.AssertToLog((not UsePosition)or(Index=1), 'Can''t find house basing both on Position and Index');

  for I := 0 to Count - 1 do
  if (TKMHouse(Items[I]).fHouseType=aType) and (TKMHouse(Items[I]).IsComplete) then
  begin
      inc(id);
      if UsePosition then
      begin
          Dist := GetLength(TKMHouse(Items[I]).GetPosition,KMPoint(X,Y));
          if BestMatch = -1 then BestMatch := Dist; //Initialize for first use
          if Dist < BestMatch then begin
            BestMatch := Dist;
            Result := TKMHouse(Items[I]);
          end;
      end else
          if Index = id then begin//Take the N-th result
            Result := TKMHouse(Items[I]);
            exit;
          end;
  end;
end;


procedure TKMHousesCollection.Paint();
var i:integer; x1,x2,y1,y2,Margin:integer;
begin
  if TestViewportClipInset then Margin:=-3 else Margin:=3;
  x1:=fViewport.GetClip.Left-Margin;  x2:=fViewport.GetClip.Right+Margin;
  y1:=fViewport.GetClip.Top -Margin;  y2:=fViewport.GetClip.Bottom+Margin;

  for I := 0 to Count - 1 do
  if (InRange(TKMHouse(Items[I]).fPosition.X,x1,x2) and InRange(TKMHouse(Items[I]).fPosition.Y,y1,y2)) then
    TKMHouse(Items[I]).Paint();
end;

procedure TKMHousesCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMHouse(Items[I]).UpdateState;

  //After all houses are updated we can safely remove those that were destroyed
  for I := Count - 1 downto 0 do
    if TKMHouse(Items[I]).ScheduleForRemoval then begin
      TKMHouse(Items[I]).Free;
      Rem(TKMHouse(Items[I]));
    end;
end;

end.
