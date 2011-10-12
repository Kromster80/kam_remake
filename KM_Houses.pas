unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
   Classes, KromUtils, Math, SysUtils,
   KM_CommonTypes, KM_Defaults, KM_ResourceGFX, KM_ResourceHouse, KM_Points, KM_Utils;

  {Everything related to houses is here}
type
  TKMHouse = class;

  THouseAction = class
  private
    fHouse:TKMHouse;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
    procedure SetHouseState(aHouseState: THouseState);
  public
    constructor Create(aHouse:TKMHouse; aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    property State: THouseState read fHouseState write SetHouseState;
    property SubAction:THouseActionSet read fSubAction;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


  TKMHouse = class
  private
    fID:integer; //unique ID, used for save/load to sync to
    fHouseType: THouseType; //House type
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
    fBuildState: THouseBuildState; // = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);
    fOwner: TPlayerIndex; //House owner player, determines flag color as well

    fBuildSupplyWood: byte; //How much Wood was delivered to house building site
    fBuildSupplyStone: byte; //How much Stone was delivered to house building site
    fBuildReserve: byte; //Take one build supply resource into reserve and "build from it"
    fBuildingProgress: word; //That is how many efforts were put into building (Wooding+Stoning)
    fDamage: word; //Damaged inflicted to house

    fHasOwner: boolean; //which is some TKMUnit
    fBuildingRepair: boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: boolean; //If on then no wares will be delivered here

    fResourceIn:array[1..4] of byte; //Resource count in input
    fResourceDeliveryCount:array[1..4] of word; //Count of the resources we have ordered for the input (used for ware distribution)
    fResourceOut:array[1..4]of byte; //Resource count in output
    fResourceOrder:array[1..4]of word; //If HousePlaceOrders=true then here are production orders

    FlagAnimStep: cardinal; //Used for Flags and Burning animation
    WorkAnimStep: cardinal; //Used for Work and etc.. which is not in sync with Flags

    fIsDestroyed:boolean;
    RemoveRoadWhenDemolish:boolean;
    fPointerCount:integer;
    fTimeSinceUnoccupiedReminder:integer;

    procedure Activate(aWasBuilt:boolean);
    procedure CloseHouse(IsEditor:boolean=false); virtual;
    procedure SetWareDelivery(aVal:boolean);

    procedure MakeSound; dynamic; //Swine/stables make extra sounds
    function GetResDistribution(aID:byte):byte; //Will use GetRatio from mission settings to find distribution amount
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle
    ResourceDepletedMsgIssued: boolean;
    DoorwayUse: byte; //number of units using our door way. Used for sliding.

    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;
    function GetHousePointer:TKMHouse; //Returns self and adds one to the pointer counter
    procedure ReleaseHousePointer; //Decreases the pointer counter
    property PointerCount:integer read fPointerCount;

    procedure DemolishHouse(DoSilent:boolean; NoRubble:boolean=false);
    property ID:integer read fID;

    property GetPosition:TKMPoint read fPosition;
    procedure SetPosition(aPos:TKMPoint); //Used only by map editor
    function GetEntrance:TKMPoint;
    function GetClosestCell(aPos:TKMPoint):TKMPoint;
    function GetDistance(aPos:TKMPoint):single;
    procedure GetListOfCellsAround(Cells:TKMPointDirList; aPassability:TPassability);
    procedure GetListOfCellsWithin(Cells:TKMPointList);
    function GetRandomCellWithin:TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property HouseType:THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write SetWareDelivery;
    property GetHasOwner:boolean read fHasOwner write fHasOwner;
    property GetOwner:TPlayerIndex read fOwner;
    function GetHealth:word;

    property BuildingState: THouseBuildState read fBuildState write fBuildState;
    procedure IncBuildingProgress;
    function MaxHealth:word;
    function  AddDamage(aAmount:word; aIsEditor:boolean=false):boolean;
    procedure AddRepair(aAmount:word=5);
    procedure UpdateDamage;
    procedure EnableRepair;
    procedure DisableRepair;
    procedure RepairToggle;

    function IsStone:boolean;
    function IsComplete:boolean;
    function IsDamaged:boolean;
    property IsDestroyed:boolean read fIsDestroyed;
    property GetDamage:word read fDamage;

    procedure SetState(aState: THouseState);
    function GetState:THouseState;

    function CheckResIn(aResource:TResourceType):word; virtual;
    function CheckResOut(aResource:TResourceType):byte;
    function CheckResOrder(aID:byte):word; virtual;
    function PickRandomOrder:byte;
    function CheckResToBuild:boolean;
    procedure ResAddToIn(aResource:TResourceType; const aCount:word=1); virtual; //override for School and etc..
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToBuild(aResource:TResourceType);
    procedure ResTakeFromIn(aResource:TResourceType; aCount:byte=1);
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); virtual;
    procedure ResEditOrder(aID:byte; aAmount:integer); virtual;

    procedure Save(SaveStream:TKMemoryStream); virtual;

    procedure IncAnimStep;
    procedure UpdateResRequest;
    procedure UpdateState;
    procedure Paint; virtual;
  end;

  {SwineStable has unique property - it needs to accumulate some resource before production begins, also special animation}
  TKMHouseSwineStable = class(TKMHouse)
  private
    BeastAge:array[1..5]of byte; //Each beasts "age". Once Best reaches age 3+1 it's ready
  public
    constructor Load(LoadStream:TKMemoryStream); override;
    function FeedBeasts:byte;
    procedure TakeBeast(aID:byte);
    procedure MakeSound; override;
    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Paint; override;
  end;

  TKMHouseInn = class(TKMHouse)
  private
    Eater:array[1..6]of record //only 6 units are allowed in the inn
      UnitType:TUnitType;
      FoodKind:byte; //What kind of food eater eats
      EatStep:cardinal;
    end;
  public
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    function EaterGetsInside(aUnitType:TUnitType):byte;
    procedure UpdateEater(aID:byte; aFoodKind:byte);
    procedure EatersGoesOut(aID:byte);
    function HasFood:boolean;
    function HasSpace:boolean;
    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Paint; override; //Render all eaters
  end;


  { Marketplace }
  TKMHouseMarket = class(TKMHouse)
  protected
    fResFrom, fResTo: TResourceType;
    fResources:array[WARE_MIN..WARE_MAX] of Word;
    procedure AttemptExchange;
    procedure SetResFrom(Value: TResourceType);
    procedure SetResTo(Value: TResourceType);
  public
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;

    property ResFrom:TResourceType read fResFrom write SetResFrom;
    property ResTo:TResourceType read fResTo write SetResTo;
    function RatioFrom: Byte;
    function RatioTo: Byte;

    function CheckResIn(aResource:TResourceType):word; override;
    function CheckResOrder(aID:byte):word; override;
    procedure ResAddToIn(aResource: TResourceType; const aCount:word=1); override;
    procedure ResEditOrder(aID:byte; aAmount:integer); override;
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); override;

    procedure Save(SaveStream:TKMemoryStream); override;
  end;


  {School has one unique property - queue of units to be trained, 1 wip + 5 in line}
  TKMHouseSchool = class(TKMHouse)
  private
    UnitWIP:Pointer;  //can't replace with TKMUnit since it will lead to circular reference in KM_House-KM_Units
    HideOneGold:boolean; //Hide the gold incase Player cancels the training, then we won't need to tweak DeliverQueue order
    UnitTrainProgress:byte; //Was it 150 steps in KaM?
    procedure CloseHouse(IsEditor:boolean=false); override;
  public
    UnitQueue:array[1..6]of TUnitType; //Also used in UI
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure ResAddToIn(aResource:TResourceType; const aCount:word=1); override;
    procedure AddUnitToQueue(aUnit:TUnitType; aCount:byte); //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(aID:integer); //Should remove unit from queue and shift rest up
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure UnitTrainingComplete; //This should shift queue filling rest with ut_None
    function GetTrainingProgress:byte;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

  {Barracks has 11 resources and Recruits}
  TKMHouseBarracks = class(TKMHouse)
  private
    ResourceCount:array[WARFARE_MIN..WARFARE_MAX]of word;
  public
    RecruitsList: TList;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    procedure ResAddToIn(aResource:TResourceType; const aCount:word=1); override;
    function CheckResIn(aResource:TResourceType):word; override;
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); override;
    function CanEquip(aUnitType: TUnitType):boolean;
    procedure Equip(aUnitType: TUnitType; aCount:byte);
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  private
    ResourceCount:array[WARE_MIN..WARE_MAX]of word;
  public
    NotAcceptFlag:array[WARE_MIN..WARE_MAX]of boolean;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure ToggleAcceptFlag(aRes:TResourceType);
    procedure ResAddToIn(aResource:TResourceType; const aCount:word=1); override;
    function CheckResIn(aResource:TResourceType):word; override;
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


  TKMHouseTower = class(TKMHouse)
  public
    procedure Paint; override;
  end;


  TKMHousesCollection = class(TKMList)
  private
    fSelectedHouse: TKMHouse;
    function AddToCollection(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint; aHBS:THouseBuildState):TKMHouse;
    function GetHouse(Index: Integer): TKMHouse;
    procedure SetHouse(Index: Integer; Item: TKMHouse);
    property Houses[Index: Integer]: TKMHouse read GetHouse write SetHouse; //Use instead of Items[.]
  public
    function AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint; RelativeEntrance:boolean):TKMHouse;
    function AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint):TKMHouse;
    procedure RemoveHouse(aHouse:TKMHouse);
    procedure OwnerUpdate(aOwner:TPlayerIndex);
    function HitTest(X, Y: Integer): TKMHouse;
    function GetHouseByID(aID: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindHouse(aType:THouseType; X,Y:word; const aIndex:byte=1; aOnlyCompleted:boolean=true): TKMHouse;
    function GetTotalPointers: integer;
    property SelectedHouse: TKMHouse read fSelectedHouse write fSelectedHouse;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;
    procedure UpdateResRequest; //Change resource requested counts for all houses
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_UnitTaskSelfTrain, KM_DeliverQueue, KM_Terrain, KM_Render, KM_RenderAux, KM_Units, KM_Units_Warrior, KM_PlayersCollection, KM_Sound, KM_Viewport, KM_Game, KM_TextLibrary, KM_Player, KM_ResourceResource;


{ TKMHouse }
constructor TKMHouse.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
var i: byte;
begin
  Inherited Create;
  fPosition   := KMPoint (PosX, PosY);
  Assert(not((PosX = 0) or (PosY = 0))); // Can create only on map
  fHouseType  := aHouseType;
  fBuildState := aBuildState;
  fOwner      := aOwner;

  fBuildSupplyWood  := 0;
  fBuildSupplyStone := 0;
  fBuildReserve     := 0;
  fBuildingProgress := 0;
  fDamage           := 0; //Undamaged yet

  fHasOwner         := false;
  //Initially repair is [off]. But for AI it's controlled by a command in DAT script
  fBuildingRepair   := false; //Don't set it yet because we don't always know who are AIs yet (in multiplayer) It is set in first UpdateState
  DoorwayUse        := 0;
  fWareDelivery     := true;

  for i:=1 to 4 do
  begin
    fResourceIn[i]  := 0;
    fResourceDeliveryCount[i] := 0;
    fResourceOut[i] := 0;
    fResourceOrder[i]:=0;
  end;

  fIsDestroyed      := false;
  RemoveRoadWhenDemolish := fTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay <> to_Road;
  fPointerCount     := 0;
  fTimeSinceUnoccupiedReminder   := TIME_BETWEEN_MESSAGES;

  fID := fGame.GetNewID;
  ResourceDepletedMsgIssued := false;

  if aBuildState = hbs_Done then //House was placed on map already Built e.g. in mission maker
  begin 
    Activate(false);
    fBuildingProgress := fResource.HouseDat[fHouseType].MaxHealth;
    fTerrain.SetHouse(fPosition, fHouseType, hs_Built, fOwner, fGame.GameState <> gsEditor); //Sets passability and flattens terrain if we're not in the map editor
  end else
    fTerrain.SetHouse(fPosition, fHouseType, hs_Plan, -1); //Terrain remains neutral yet
end;


constructor TKMHouse.Load(LoadStream:TKMemoryStream);
var i:integer; HasAct:boolean;
begin
  Inherited Create;
  LoadStream.Read(fHouseType, SizeOf(fHouseType));
  LoadStream.Read(fPosition);
  LoadStream.Read(fBuildState, SizeOf(fBuildState));
  LoadStream.Read(fOwner, SizeOf(fOwner));
  LoadStream.Read(fBuildSupplyWood);
  LoadStream.Read(fBuildSupplyStone);
  LoadStream.Read(fBuildReserve);
  LoadStream.Read(fBuildingProgress, SizeOf(fBuildingProgress));
  LoadStream.Read(fDamage, SizeOf(fDamage));
  LoadStream.Read(fHasOwner);
  LoadStream.Read(fBuildingRepair);
  LoadStream.Read(fWareDelivery);
  for i:=1 to 4 do LoadStream.Read(fResourceIn[i]);
  for i:=1 to 4 do LoadStream.Read(fResourceDeliveryCount[i]);
  for i:=1 to 4 do LoadStream.Read(fResourceOut[i]);
  for i:=1 to 4 do LoadStream.Read(fResourceOrder[i], SizeOf(fResourceOrder[i]));
  LoadStream.Read(FlagAnimStep);
  LoadStream.Read(WorkAnimStep);
  LoadStream.Read(fIsDestroyed);
  LoadStream.Read(RemoveRoadWhenDemolish);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fTimeSinceUnoccupiedReminder);
  LoadStream.Read(fID);
  LoadStream.Read(HasAct);
  if HasAct then
  begin
    fCurrentAction := THouseAction.Create(nil, hst_Empty); //Create action object
    fCurrentAction.Load(LoadStream); //Load actual data into object
  end;
  LoadStream.Read(ResourceDepletedMsgIssued);
  LoadStream.Read(DoorwayUse);
end;


procedure TKMHouse.SyncLoad;
begin
  //Should be virtual
end;


destructor TKMHouse.Destroy;
begin
  FreeAndNil(fCurrentAction);
  Inherited;
end;


{Returns self and adds on to the pointer counter}
function TKMHouse.GetHousePointer:TKMHouse;
begin
  inc(fPointerCount);
  Result := Self;
end;


{Decreases the pointer counter}
procedure TKMHouse.ReleaseHousePointer;
begin
  if fPointerCount < 1 then
    raise ELocError.Create('House remove pointer for '+fResource.HouseDat[fHouseType].HouseName, fPosition);
  dec(fPointerCount);
end;


procedure TKMHouse.CloseHouse(IsEditor:boolean=false);
begin
  fIsDestroyed := true;
  BuildingRepair := false; //Otherwise labourers will take task to repair when the house is destroyed
  if (RemoveRoadWhenDemolish) and (not (BuildingState in [hbs_Stone, hbs_Done]) or IsEditor) then
  begin
    if fTerrain.Land[GetEntrance.Y,GetEntrance.X].TileOverlay = to_Road then
    begin
      fTerrain.RemRoad(GetEntrance);
      if not IsEditor then
        fTerrain.Land[GetEntrance.Y,GetEntrance.X].TileOverlay := to_Dig3; //Remove road and leave dug earth behind
    end;
  end;
  FreeAndNil(fCurrentAction);
  //Leave disposing of units inside the house to themselves
end;


procedure TKMHouse.Activate(aWasBuilt:boolean);
var i:integer; Res:TResourceType;
begin
  fPlayers.Player[fOwner].Stats.HouseCreated(fHouseType,aWasBuilt); //Only activated houses count
  fPlayers.RevealForTeam(fOwner,fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);

  fCurrentAction:=THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_FlagShtok,ha_Flag1..ha_Flag3]);

  for i:=1 to 4 do
  begin
    Res := fResource.HouseDat[fHouseType].ResInput[i];
    with fPlayers.Player[fOwner].DeliverList do
    case Res of
      rt_None:    ;
      rt_Warfare: AddNewDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      rt_All:     AddNewDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      else
      begin
        AddNewDemand(Self, nil, Res, GetResDistribution(i), dt_Once,   di_Norm); //Every new house needs 5 resourceunits
        inc(fResourceDeliveryCount[i],GetResDistribution(i)); //Keep track of how many resources we have on order (for distribution of wares)
      end;
    end;
  end;

end;


procedure TKMHouse.DemolishHouse(DoSilent:boolean; NoRubble:boolean=false);
begin
  if fPlayers.Selected = Self then fPlayers.Selected := nil;
  if (fGame.fGamePlayInterface <> nil) and (fGame.fGamePlayInterface.ShownHouse = Self) then fGame.fGamePlayInterface.ShowHouseInfo(nil);

  if not DoSilent then
    if (BuildingState <> hbs_Glyph) and not NoRubble then
      fSoundLib.Play(sfx_HouseDestroy, fPosition);
      
  //Dispose of delivery tasks performed in DeliverQueue unit
  fPlayers.Player[fOwner].DeliverList.RemoveOffer(Self);
  fPlayers.Player[fOwner].DeliverList.RemoveDemand(Self);
  fPlayers.Player[fOwner].RepairList.RemoveHouse(Self);
  fPlayers.Player[fOwner].BuildList.RemoveHouse(Self);
  fTerrain.SetHouse(fPosition,fHouseType,hs_None,-1);
  //Road is removed in CloseHouse
  if not NoRubble then fTerrain.AddHouseRemainder(fPosition,fHouseType,fBuildState);

  CloseHouse(NoRubble);
end;


//Used by MapEditor
procedure TKMHouse.SetPosition(aPos:TKMPoint);
begin
  Assert(fGame.GameState = gsEditor);
  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  fTerrain.SetHouse(fPosition,fHouseType,hs_None,-1);
  fTerrain.RemRoad(GetEntrance);
  if fTerrain.CanPlaceHouse(aPos, HouseType, MyPlayer) then
  begin
    fPosition.X := aPos.X - fResource.HouseDat[fHouseType].EntranceOffsetX;
    fPosition.Y := aPos.Y;
  end;
  fTerrain.SetHouse(fPosition,fHouseType,hs_Built,fOwner);
  fTerrain.SetRoad(GetEntrance,fOwner);
end;


{Return Entrance of the house, which is different than house position sometimes}
function TKMHouse.GetEntrance:TKMPoint;
begin
  Result.X := GetPosition.X + fResource.HouseDat[fHouseType].EntranceOffsetX;
  Result.Y := GetPosition.Y;
  Assert((Result.X > 0) and (Result.Y > 0));
end;


{Returns the closest cell of the house to aPos}
function TKMHouse.GetClosestCell(aPos:TKMPoint):TKMPoint;
var C:TKMPointList;
begin
  C := TKMPointList.Create;
  GetListOfCellsWithin(C);
  if not C.GetClosest(aPos, Result) then
    Assert(false, 'Could not find closest house cell');
  C.Free;
end;


{Return distance from aPos to the closest house tile}
function TKMHouse.GetDistance(aPos:TKMPoint):single;
var C:TKMPointList; i:integer;
begin
  C := TKMPointList.Create;
  try
    GetListOfCellsWithin(C);

    Result := GetLength(C.List[1], aPos);
    for i:=2 to C.Count do
      Result := Math.min(Result, GetLength(C.List[i], aPos));
  finally
    C.Free;
  end;
end;


procedure TKMHouse.GetListOfCellsAround(Cells:TKMPointDirList; aPassability:TPassability);
var
  i,k:integer;
  Loc:TKMPoint;
  HouseArea:THouseArea;

  procedure AddLoc(X,Y:word; Dir:TKMDirection);
  begin
    //First check that the passabilty is correct, as the house may be placed against blocked terrain
    if not fTerrain.CheckPassability(KMPoint(X,Y),aPassability) then exit;
    Cells.AddEntry(KMPointDir(KMPoint(X,Y), Dir));
  end;

begin

  Cells.Clearup;
  Loc := fPosition;
  HouseArea := fResource.HouseDat[fHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
  if HouseArea[i,k]<>0 then
  begin
    if (i=1)or(HouseArea[i-1,k]=0) then
      AddLoc(Loc.X + k - 3, Loc.Y + i - 4 - 1, dir_S); //Above
    if (i=4)or(HouseArea[i+1,k]=0) then
      AddLoc(Loc.X + k - 3, Loc.Y + i - 4 + 1, dir_N); //Below
    if (k=4)or(HouseArea[i,k+1]=0) then
      AddLoc(Loc.X + k - 3 + 1, Loc.Y + i - 4, dir_W); //FromRight
    if (k=1)or(HouseArea[i,k-1]=0) then
      AddLoc(Loc.X + k - 3 - 1, Loc.Y + i - 4, dir_E); //FromLeft
  end;
end;


procedure TKMHouse.GetListOfCellsWithin(Cells:TKMPointList);
var i,k:integer; Loc:TKMPoint; HouseArea:THouseArea;
begin
  Cells.Clearup;
  Loc := fPosition;
  HouseArea := fResource.HouseDat[fHouseType].BuildArea;

  for i:=max(Loc.Y-3,1) to Loc.Y do for k:=max(Loc.X-2,1) to min(Loc.X+1,fTerrain.MapX) do
  if HouseArea[i-Loc.Y+4,k-Loc.X+3]<>0 then
    Cells.AddEntry(KMPoint(k,i));
end;


function TKMHouse.GetRandomCellWithin:TKMPoint;
var Cells:TKMPointList;
begin
  Cells := TKMPointList.Create;
  GetListOfCellsWithin(Cells);
  Assert(Cells.GetRandom(Result));
  Cells.Free;
end;


function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X-fPosition.X+3 in [1..4]) and
            (Y-fPosition.Y+4 in [1..4]) and
            (fResource.HouseDat[fHouseType].BuildArea[Y-fPosition.Y+4, X-fPosition.X+3] <> 0);
end;


function TKMHouse.GetHealth:word;
begin
  Result := max(fBuildingProgress - fDamage, 0);
end;


{Increase building progress of house. When it reaches some point Stoning replaces Wooding
 and then it's done and house should be finalized}
 {Keep track on stone/wood reserve here as well}
procedure TKMHouse.IncBuildingProgress;
begin
  if IsComplete then exit;

  if (fBuildState=hbs_Wood) and (fBuildReserve = 0) then begin
    dec(fBuildSupplyWood);
    inc(fBuildReserve,50);
  end;
  if (fBuildState=hbs_Stone) and (fBuildReserve = 0) then begin
    dec(fBuildSupplyStone);
    inc(fBuildReserve,50);
  end;

  inc(fBuildingProgress, 5); //is how many effort was put into building nevermind applied damage
  dec(fBuildReserve, 5); //This is reserve we build from

  if (fBuildState=hbs_Wood) and (fBuildingProgress = fResource.HouseDat[fHouseType].WoodCost*50) then
    fBuildState := hbs_Stone;

  if (fBuildState=hbs_Stone) and (fBuildingProgress-fResource.HouseDat[fHouseType].WoodCost*50 = fResource.HouseDat[fHouseType].StoneCost*50) then
  begin
    fBuildState := hbs_Done;
    fPlayers.Player[fOwner].Stats.HouseEnded(fHouseType);
    Activate(true);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      fPlayers.Player[fOwner].RepairList.AddHouse(Self);
  end;
end;


function TKMHouse.MaxHealth:word;
begin
  Result := fResource.HouseDat[fHouseType].MaxHealth;
end;


{Add damage to the house, positive number}
//Return TRUE if house was destroyed
function TKMHouse.AddDamage(aAmount:word; aIsEditor:boolean=false):boolean;
begin
  Result := false;
  if IsDestroyed then
    Exit;

  if fBuildState < hbs_Wood then
  begin
    fPlayers.Player[fOwner].RemHouse(GetEntrance, true);
    Exit;
  end;

  fDamage := Math.min(fDamage + aAmount, MaxHealth);
  if (fBuildState = hbs_Done) and BuildingRepair then
    fPlayers.Player[fOwner].RepairList.AddHouse(Self);

  if fBuildState = hbs_Done then
    UpdateDamage; //Only update fire if the house is complete

  if (GetHealth = 0) and not aIsEditor then
  begin
    DemolishHouse(false); //Destroyed by Enemy
    if Assigned(fPlayers) and Assigned(fPlayers.Player[fOwner]) then
      if (fBuildState = hbs_Done) then
        fPlayers.Player[fOwner].Stats.HouseLost(fHouseType);
    Result := true;
  end;
end;


{Add repair to the house}
procedure TKMHouse.AddRepair(aAmount:word=5);
begin
  fDamage := EnsureRange(fDamage - aAmount, 0, High(Word));
  if fDamage = 0 then
    fPlayers.Player[fOwner].RepairList.RemoveHouse(Self);
  UpdateDamage;
end;


{Update house damage animation}
procedure TKMHouse.UpdateDamage;
var Dmg: word;
begin
  Dmg := MaxHealth div 8; //There are 8 fire places for each house, so the increment for each fire level is Max_Health / 8
  fCurrentAction.SubActionRem([ha_Fire1,ha_Fire2,ha_Fire3,ha_Fire4,ha_Fire5,ha_Fire6,ha_Fire7,ha_Fire8]);
  if fDamage > 0*Dmg then fCurrentAction.SubActionAdd([ha_Fire1]);
  if fDamage > 1*Dmg then fCurrentAction.SubActionAdd([ha_Fire2]);
  if fDamage > 2*Dmg then fCurrentAction.SubActionAdd([ha_Fire3]);
  if fDamage > 3*Dmg then fCurrentAction.SubActionAdd([ha_Fire4]);
  if fDamage > 4*Dmg then fCurrentAction.SubActionAdd([ha_Fire5]);
  if fDamage > 5*Dmg then fCurrentAction.SubActionAdd([ha_Fire6]);
  if fDamage > 6*Dmg then fCurrentAction.SubActionAdd([ha_Fire7]);
  if fDamage > 7*Dmg then fCurrentAction.SubActionAdd([ha_Fire8]);
  {House gets destroyed in UpdateState loop}
end;


{if house is damaged then add repair to buildlist}
procedure TKMHouse.EnableRepair;
begin
  BuildingRepair := true;
  AddDamage(0); //Shortcut to refresh of damage
end;


{if house is damaged then remove repair from buildlist and free up workers}
procedure TKMHouse.DisableRepair;
begin
  BuildingRepair := false;
  AddRepair(0); //Shortcut to refresh of damage
end;


procedure TKMHouse.RepairToggle;
begin
  if BuildingRepair then DisableRepair else EnableRepair;
end;


function TKMHouse.IsStone:boolean;
begin
  Result := fBuildState = hbs_Stone;
end;


{Check if house is completely built, nevermind the damage}
function TKMHouse.IsComplete:boolean;
begin
  Result := fBuildState = hbs_Done;
end;


{Check if house is damaged}
function TKMHouse.IsDamaged:boolean;
begin
  Result := fDamage <> 0;
end;


procedure TKMHouse.SetState(aState: THouseState);
begin
  fCurrentAction.State := aState;
end;


function TKMHouse.GetState:THouseState;
begin
  Result := fCurrentAction.State;
end;


{How much resources house has in Input}
function TKMHouse.CheckResIn(aResource:TResourceType):word;
var i:integer;
begin
  Result := 0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResInput[i]) or (aResource = rt_All) then
    inc(Result, fResourceIn[i]);
end;


{How much resources house has in Output}
function TKMHouse.CheckResOut(aResource:TResourceType):byte;
var i:integer;
begin
  Result := 0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResOutput[i]) or (aResource = rt_All) then
    inc(Result, fResourceOut[i]);
end;


{Check amount of placed order for given ID}
function TKMHouse.CheckResOrder(aID:byte):word;
begin
  //AI always order production of everything. Could be changed later with a script command to only make certain things
  if (fPlayers.Player[fOwner].PlayerType = pt_Computer) and (fResource.HouseDat[fHouseType].ResOutput[aID] <> rt_None) then
    Result := 1
  else
    Result := fResourceOrder[aID];
end;


function TKMHouse.PickRandomOrder:byte;
var i:byte; O:array[1..4]of byte; OCount:byte;
begin
  OCount := 0;
  FillChar(O, SizeOf(O), #0);
  for i:=1 to 4 do
  if CheckResOrder(i) > 0 then
  begin
    inc(OCount);
    O[OCount] := i;
  end;

  if OCount > 0 then
    Result := O[KaMRandom(OCount)+1] //Pick random from available orders
  else
    Result := 0;
end;


{Check if house has enough resource supply to be built depending on it's state}
function TKMHouse.CheckResToBuild:boolean;
begin
  case fBuildState of
    hbs_Wood:   Result := (fBuildSupplyWood > 0) or (fBuildReserve > 0);
    hbs_Stone:  Result := (fBuildSupplyStone > 0) or (fBuildReserve > 0);
    else        Result := False;
  end;
end;


//todo: Store/Barracks/Market don't really have an In/Out separation. The code enforcing it looks just confusing and behaves unexpected
//Maybe it's better to rule out In/Out? No, it is required to separate what can be taken out of the house and what not.
//But.. if we add "Evacuate" button to all house the separation becomes artificial..
procedure TKMHouse.ResAddToIn(aResource:TResourceType; const aCount:word=1);
var i:integer;
begin
  Assert(aResource <> rt_None);

  for i:=1 to 4 do
    if aResource = fResource.HouseDat[fHouseType].ResInput[i] then
      inc(fResourceIn[i], aCount);
end;


procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResOutput[i] then
    begin
      inc(fResourceOut[i], aCount);
      fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self, aResource, aCount);
    end;
end;


{Add resources to building process}
procedure TKMHouse.ResAddToBuild(aResource:TResourceType);
begin
  case aResource of
    rt_Wood: inc(fBuildSupplyWood);
    rt_Stone: inc(fBuildSupplyStone);
  else raise ELocError.Create('WIP house is not supposed to recieve '+fResource.Resources[aResource].Name+', right?', fPosition);
  end;
end;


//Take resource from Input and order more of that kind if DistributionRatios allow
procedure TKMHouse.ResTakeFromIn(aResource:TResourceType; aCount:byte=1);
var i,k:integer;
begin
  Assert(aResource<>rt_None);

  for i:=1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResInput[i] then begin
    Assert(fResourceIn[i] >= aCount, 'fResourceIn[i]<0');
    dec(fResourceIn[i],aCount);
    dec(fResourceDeliveryCount[i],aCount);
    //Only request a new resource if it is allowed by the distribution of wares for our parent player
    for k:=1 to aCount do
      if fResourceDeliveryCount[i] < GetResDistribution(i) then
      begin
        fPlayers.Player[fOwner].DeliverList.AddNewDemand(Self,nil,aResource,1,dt_Once,di_Norm);
        inc(fResourceDeliveryCount[i]);
      end;
    exit;
  end;
end;


procedure TKMHouse.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  Assert(aResource<>rt_None);
  Assert(not(fHouseType in [ht_Store,ht_Barracks]));
  for i:=1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResOutput[i] then begin
    Assert(aCount <= fResourceOut[i]);
    dec(fResourceOut[i], aCount);
    exit;
  end;
end;


{ Edit production order as + / - }
procedure TKMHouse.ResEditOrder(aID:byte; aAmount:integer);
begin
  fResourceOrder[aID] := EnsureRange(fResourceOrder[aID] + aAmount, 0, MAX_ORDER);
end;


function TKMHouse.GetResDistribution(aID:byte):byte;
begin
  Result := fPlayers.Player[fOwner].Stats.GetRatio(fResource.HouseDat[fHouseType].ResInput[aID],fHouseType);
end;


procedure TKMHouse.MakeSound;
var Work:THouseActionType; Step:byte;
begin
  //Do not play sounds if house is invisible to MyPlayer
  if MyPlayer.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then exit;
  if fCurrentAction = nil then exit; //no action means no sound ;)

  if ha_Work1 in fCurrentAction.SubAction then Work := ha_Work1 else
  if ha_Work2 in fCurrentAction.SubAction then Work := ha_Work2 else
  if ha_Work3 in fCurrentAction.SubAction then Work := ha_Work3 else
  if ha_Work4 in fCurrentAction.SubAction then Work := ha_Work4 else
  if ha_Work5 in fCurrentAction.SubAction then Work := ha_Work5 else
    Exit; //No work is going on

  Step := fResource.HouseDat[fHouseType].Anim[Work].Count;
  if Step=0 then exit;

  Step := WorkAnimStep mod Step;

  case fHouseType of //Various buildings and HouseActions producing sounds
    ht_School:        if (Work = ha_Work5)and(Step = 28) then fSoundLib.Play(sfx_SchoolDing, fPosition); //Ding as the clock strikes 12
    ht_Mill:          if (Work = ha_Work2)and(Step = 0) then fSoundLib.Play(sfx_mill, fPosition);
    ht_CoalMine:      if (Work = ha_Work1)and(Step = 5) then fSoundLib.Play(sfx_coalDown, fPosition)
                      else if (Work = ha_Work1)and(Step = 24) then fSoundLib.Play(sfx_CoalMineThud, fPosition,true,0.8)
                      else if (Work = ha_Work2)and(Step = 7) then fSoundLib.Play(sfx_mine, fPosition)
                      else if (Work = ha_Work2)and(Step = 8) then fSoundLib.Play(sfx_mine, fPosition,true,0.4) //echo
                      else if (Work = ha_Work5)and(Step = 1) then fSoundLib.Play(sfx_coalDown, fPosition);
    ht_IronMine:      if (Work = ha_Work2)and(Step = 7) then fSoundLib.Play(sfx_mine, fPosition)
                      else if (Work = ha_Work2)and(Step = 8) then fSoundLib.Play(sfx_mine, fPosition,true,0.4); //echo
    ht_GoldMine:      if (Work = ha_Work2)and(Step = 5) then fSoundLib.Play(sfx_mine, fPosition)
                      else if (Work = ha_Work2)and(Step = 6) then fSoundLib.Play(sfx_mine, fPosition,true,0.4); //echo
    ht_Sawmill:       if (Work = ha_Work2)and(Step = 1) then fSoundLib.Play(sfx_saw, fPosition);
    ht_Wineyard:      if (Work = ha_Work2)and(Step in [1,7,13,19]) then fSoundLib.Play(sfx_wineStep, fPosition)
                      else if (Work = ha_Work5)and(Step = 14) then fSoundLib.Play(sfx_wineDrain, fPosition,true,1.5)
                      else if (Work = ha_Work1)and(Step = 10) then fSoundLib.Play(sfx_wineDrain, fPosition,true,1.5);
    ht_Bakery:        if (Work = ha_Work3)and(Step in [6,25]) then fSoundLib.Play(sfx_BakerSlap, fPosition);
    ht_Quary:         if (Work = ha_Work2)and(Step in [4,13]) then fSoundLib.Play(sfx_QuarryClink, fPosition)
                      else if (Work = ha_Work5)and(Step in [4,13,22]) then fSoundLib.Play(sfx_QuarryClink, fPosition);
    ht_WeaponSmithy:  if (Work = ha_Work1)and(Step in [17,22]) then fSoundLib.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work2)and(Step in [10,25]) then fSoundLib.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work3)and(Step in [10,25]) then fSoundLib.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work4)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work5)and(Step = 12) then fSoundLib.Play(sfx_BlacksmithBang, fPosition);
    ht_ArmorSmithy:   if (Work = ha_Work2)and(Step in [13,28]) then fSoundLib.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work3)and(Step in [13,28]) then fSoundLib.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work4)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work5)and(Step in [8,22]) then fSoundLib.Play(sfx_BlacksmithFire, fPosition);
    ht_Metallurgists: if (Work = ha_Work3)and(Step = 6) then fSoundLib.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work4)and(Step in [16,20]) then fSoundLib.Play(sfx_wineDrain, fPosition);
    ht_IronSmithy:    if (Work = ha_Work2)and(Step in [1,16]) then fSoundLib.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work3)and(Step = 1) then fSoundLib.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work3)and(Step = 13) then fSoundLib.Play(sfx_wineDrain, fPosition);
    ht_WeaponWorkshop:if (Work = ha_Work2)and(Step in [1,10,19]) then fSoundLib.Play(sfx_saw, fPosition)
                      else if (Work = ha_Work3)and(Step in [10,21]) then fSoundLib.Play(sfx_CarpenterHammer, fPosition)
                      else if (Work = ha_Work4)and(Step in [2,13]) then fSoundLib.Play(sfx_CarpenterHammer, fPosition);
    ht_ArmorWorkshop: if (Work = ha_Work2)and(Step in [3,13,23]) then fSoundLib.Play(sfx_saw, fPosition)
                      else if (Work = ha_Work3)and(Step in [17,28]) then fSoundLib.Play(sfx_CarpenterHammer, fPosition)
                      else if (Work = ha_Work4)and(Step in [10,20]) then fSoundLib.Play(sfx_CarpenterHammer, fPosition);
    ht_Tannery:       if (Work = ha_Work2)and(Step = 5) then fSoundLib.Play(sfx_Leather, fPosition,true,0.8);
    ht_Butchers:      if (Work = ha_Work2)and(Step in [8,16,24]) then fSoundLib.Play(sfx_ButcherCut, fPosition)
                      else if (Work = ha_Work3)and(Step in [9,21]) then fSoundLib.Play(sfx_SausageString, fPosition);
    ht_Swine:         if ((Work = ha_Work2)and(Step in [10,20]))or((Work = ha_Work3)and(Step = 1)) then fSoundLib.Play(sfx_ButcherCut, fPosition);
    //ht_WatchTower:  Sound handled by projectile itself
  end;
end;


procedure TKMHouse.Save(SaveStream:TKMemoryStream);
var i:integer; HasAct:boolean;
begin
  SaveStream.Write(fHouseType, SizeOf(fHouseType));
  SaveStream.Write(fPosition);
  SaveStream.Write(fBuildState, SizeOf(fBuildState));
  SaveStream.Write(fOwner, SizeOf(fOwner));
  SaveStream.Write(fBuildSupplyWood);
  SaveStream.Write(fBuildSupplyStone);
  SaveStream.Write(fBuildReserve);
  SaveStream.Write(fBuildingProgress, SizeOf(fBuildingProgress));
  SaveStream.Write(fDamage, SizeOf(fDamage));
  SaveStream.Write(fHasOwner);
  SaveStream.Write(fBuildingRepair);
  SaveStream.Write(fWareDelivery);
  for i:=1 to 4 do SaveStream.Write(fResourceIn[i]);
  for i:=1 to 4 do SaveStream.Write(fResourceDeliveryCount[i]);
  for i:=1 to 4 do SaveStream.Write(fResourceOut[i]);
  for i:=1 to 4 do SaveStream.Write(fResourceOrder[i], SizeOf(fResourceOrder[i]));
  SaveStream.Write(FlagAnimStep);
  SaveStream.Write(WorkAnimStep);
  SaveStream.Write(fIsDestroyed);
  SaveStream.Write(RemoveRoadWhenDemolish);
  SaveStream.Write(fPointerCount);
  SaveStream.Write(fTimeSinceUnoccupiedReminder);
  SaveStream.Write(fID);
  HasAct := fCurrentAction <> nil;
  SaveStream.Write(HasAct);
  if HasAct then fCurrentAction.Save(SaveStream);
  SaveStream.Write(ResourceDepletedMsgIssued);
  SaveStream.Write(DoorwayUse);
end;


procedure TKMHouse.IncAnimStep;
begin
  inc(FlagAnimStep);
  inc(WorkAnimStep);
  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if FOG_OF_WAR_ENABLE then
  if FlagAnimStep mod 10 = 0 then
    fPlayers.RevealForTeam(fOwner, fPosition,fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);
end;


//Request more resources (if distribution of wares has changed)
procedure TKMHouse.UpdateResRequest;
var i:byte; Count:shortint;
begin
  for i:=1 to 4 do
    if not (fResource.HouseDat[fHouseType].ResInput[i] in [rt_All, rt_Warfare, rt_None]) then
    if fResourceDeliveryCount[i] < GetResDistribution(i) then
    begin
      Count := GetResDistribution(i)-fResourceDeliveryCount[i];
      fPlayers.Player[fOwner].DeliverList.AddNewDemand(
        Self, nil, fResource.HouseDat[fHouseType].ResInput[i], Count, dt_Once, di_Norm);

      inc(fResourceDeliveryCount[i], Count);
    end;
end;


procedure TKMHouse.UpdateState;
begin
  if fBuildState<>hbs_Done then exit; //Don't update unbuilt houses
  //Toggle repair for AI players (allows AI to turn repair on and off based on requirements)
  if (not fBuildingRepair) and (fPlayers.Player[fOwner].PlayerType = pt_Computer) and (fPlayers.Player[fOwner].AI.HouseAutoRepair) then
    EnableRepair;
  if fBuildingRepair and (fPlayers.Player[fOwner].PlayerType = pt_Computer) and (not fPlayers.Player[fOwner].AI.HouseAutoRepair) then
    DisableRepair;

  //Show unoccupied message if needed and house belongs to human player and can have owner at all and not a barracks
  if (not fHasOwner) and (fResource.HouseDat[fHouseType].OwnerType <> ut_None) and (fHouseType <> ht_Barracks) then
  begin
    dec(fTimeSinceUnoccupiedReminder);
    if fTimeSinceUnoccupiedReminder = 0 then
    begin
      if fOwner = MyPlayer.PlayerIndex then
        fGame.fGamePlayInterface.MessageIssue(msgHouse,fTextLibrary.GetTextString(295),GetEntrance);
      fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
    end;
  end
  else
    fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES;

  if not fIsDestroyed then MakeSound; //Make some sound/noise along the work

  IncAnimStep;
end;


procedure TKMHouse.Paint;
begin
  case fBuildState of
    hbs_Glyph: fRender.AddHouseTablet(fHouseType, fPosition);
    hbs_NoGlyph:; //Nothing
    hbs_Wood:
      begin
        fRender.RenderHouseWood(fHouseType,
        fBuildingProgress/50/fResource.HouseDat[fHouseType].WoodCost, //0...1 range
        fPosition);
        fRender.AddHouseBuildSupply(fHouseType, fBuildSupplyWood, fBuildSupplyStone, fPosition);
      end;
    hbs_Stone:
      begin
        fRender.RenderHouseStone(fHouseType,
        (fBuildingProgress/50-fResource.HouseDat[fHouseType].WoodCost)/fResource.HouseDat[fHouseType].StoneCost, //0...1 range
        fPosition);
        fRender.AddHouseBuildSupply(fHouseType, fBuildSupplyWood, fBuildSupplyStone, fPosition);
      end;
    else begin
      fRender.RenderHouseStone(fHouseType,1,fPosition);
      fRender.RenderHouseSupply(fHouseType,fResourceIn,fResourceOut,fPosition);
      if fCurrentAction<>nil then
        fRender.RenderHouseWork(fHouseType, fCurrentAction.SubAction, WorkAnimStep,fPosition,fPlayers.Player[fOwner].FlagColor);
    end;
  end;
end;

procedure TKMHouse.SetWareDelivery(aVal:boolean);
begin
  fWareDelivery := aVal;
end;


{TKMHouseSwineStable}
constructor TKMHouseSwineStable.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(BeastAge, SizeOf(BeastAge));
end;


//Return ID of beast that has grown up
function TKMHouseSwineStable.FeedBeasts:byte;
var i:integer;
begin
  Result:=0;
  inc(BeastAge[KaMRandom(5)+1]); //Let's hope it never overflows MAX
  for i:=1 to length(BeastAge) do
    if BeastAge[i]>3 then
      Result:=i;
end;


procedure TKMHouseSwineStable.TakeBeast(aID:byte);
begin
  if (aID<>0) and (BeastAge[aID]>3) then
    BeastAge[aID] := 0;
end;


//Make beast noises - each beast makes a noise (if it exists) with two second pauses between each one
procedure TKMHouseSwineStable.MakeSound;
var i:byte;
begin
  Inherited;
  if MyPlayer.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then exit;
  for i:=0 to 4 do
    if BeastAge[i+1]>0 then
      if (FlagAnimStep + 20*i) mod 100 = 0 then
      begin
        if fHouseType = ht_Stables then
          fSoundLib.Play(TSoundFX(byte(sfx_Horse1) + PseudoRandom(4)), fPosition); //sfx_Horse1..sfx_Horse4
        if fHouseType = ht_Swine   then
          fSoundLib.Play(TSoundFX(byte(sfx_Pig1)   + PseudoRandom(4)), fPosition); //sfx_Pig1..sfx_Pig4
      end;
end;


procedure TKMHouseSwineStable.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
end;


procedure TKMHouseSwineStable.Paint;
var i:integer;
begin
  Inherited;
  //We render beasts on top of the HouseWork (which is mostly flames in this case), because otherwise
  //Swinefarm looks okay, but Stables are totaly wrong - flames are right on horses backs!
  if fBuildState=hbs_Done then
    for i:=1 to 5 do
      if BeastAge[i]>0 then
        fRender.RenderHouseStableBeasts(fHouseType, i, min(BeastAge[i],3), WorkAnimStep, fPosition);

  //But Animal Breeders should be on top of beasts
  if fCurrentAction<>nil then
    fRender.RenderHouseWork(fHouseType,
                            fCurrentAction.SubAction * [ha_Work1, ha_Work2, ha_Work3, ha_Work4, ha_Work5],
                            WorkAnimStep,fPosition,fPlayers.Player[fOwner].FlagColor);
end;


{ TKMHouseInn }
constructor TKMHouseInn.Create(aHouseType: THouseType; PosX, PosY: integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
var i:integer;
begin
  Inherited;

  for i:=Low(Eater) to High(Eater) do
    Eater[i].UnitType := ut_None;
end;


constructor TKMHouseInn.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(Eater, SizeOf(Eater));
end;


//EatStep := FlagAnimStep, cos increases it each frame, we don't need to increase all 6 AnimSteps manually
function TKMHouseInn.EaterGetsInside(aUnitType:TUnitType):byte;
var i:integer;
begin
  Result:=0;
  for i:=low(Eater) to high(Eater) do
  if Eater[i].UnitType=ut_None then
  begin
    Eater[i].UnitType := aUnitType;
    Eater[i].FoodKind := 0;
    Eater[i].EatStep  := FlagAnimStep;
    Result := i;
    exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aID:byte; aFoodKind:byte);
begin
  if aID=0 then exit;
  Assert(aFoodKind in [1..4], 'Wrong food kind');
  Eater[aID].FoodKind := aFoodKind; //Order is Wine-Bread-Sausages-Fish
  Eater[aID].EatStep  := FlagAnimStep; //FlagAnimStep-Eater[i].EatStep = 0
end;


procedure TKMHouseInn.EatersGoesOut(aID:byte);
begin
  if aID=0 then exit;
  Eater[aID].UnitType:=ut_None;
end;


function TKMHouseInn.HasFood:boolean;
begin
  Result := (CheckResIn(rt_Sausages)+CheckResIn(rt_Bread)+CheckResIn(rt_Wine)+CheckResIn(rt_Fish)>0);
end;


function TKMHouseInn.HasSpace:boolean;
var
  i: integer;
begin
  Result:=false;
  for i:=low(Eater) to high(Eater) do
    Result := Result or (Eater[i].UnitType=ut_None);
end;


procedure TKMHouseInn.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(Eater, SizeOf(Eater));
end;


procedure TKMHouseInn.Paint;
const
  OffX:array[1..3]of single = (-0.5, 0.0, 0.5);
  OffY:array[1..3]of single = ( 0.35, 0.4, 0.45);
var i:integer; AnimDir:TKMDirection; AnimStep:cardinal;
begin
  Inherited;
  if (fBuildState<>hbs_Done) then exit;

  for i:=low(Eater) to high(Eater) do
  if (Eater[i].UnitType<>ut_None) and (Eater[i].FoodKind<>0) then
  begin 
    AnimDir  := TKMDirection(Eater[i].FoodKind*2 - 1 + ((i-1) div 3));
    AnimStep := FlagAnimStep-Eater[i].EatStep; //Delta is our AnimStep

    fRender.RenderUnit(Eater[i].UnitType, ua_Eat, AnimDir, AnimStep,
      fPosition.X+OffX[(i-1) mod 3 +1],
      fPosition.Y+OffY[(i-1) mod 3 +1],
      fPlayers.Player[fOwner].FlagColor, false);
  end;
end;


{ TKMHouseMarket }
constructor TKMHouseMarket.Create(aHouseType: THouseType; PosX,PosY: integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
begin
  inherited;

  fResFrom := rt_None;
  fResTo := rt_None;
end;


function TKMHouseMarket.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [WARE_MIN..WARE_MAX] then
    Result := fResources[aResource]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


function TKMHouseMarket.CheckResOrder(aID: byte): word;
begin
  Result := fResourceOrder[1];
end;


function TKMHouseMarket.RatioFrom: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> rt_None) and (fResTo <> rt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := fResource.Resources[fResFrom].MarketPrice;
    CostTo := fResource.Resources[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostTo/Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


function TKMHouseMarket.RatioTo: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> rt_None) and (fResTo <> rt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := fResource.Resources[fResFrom].MarketPrice;
    CostTo := fResource.Resources[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostFrom/Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


procedure TKMHouseMarket.ResAddToIn(aResource: TResourceType; const aCount:word=1);
begin
  inc(fResources[aResource], aCount);

  //If user cancelled the exchange (or began new one with different resources already)
  //then incoming resourced should be added to Offer list immediately
  //We don't want Marketplace to act like a Store
  if (aResource = fResFrom) and (fResourceOrder[1] > 0) then
    AttemptExchange
  else
    fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self, aResource, aCount);
end;


procedure TKMHouseMarket.AttemptExchange;
var TradeCount: Word;
begin
  Assert((fResFrom <> rt_None) and (fResTo <> rt_None) and (fResFrom <> fResTo));

  if (fResourceOrder[1] > 0) and (fResources[fResFrom] >= RatioFrom) then
  begin
    //How much can we trade
    TradeCount := Min((fResources[fResFrom] div RatioFrom), fResourceOrder[1]);

    dec(fResources[fResFrom], TradeCount * RatioFrom);
    dec(fResourceDeliveryCount[1], TradeCount * RatioFrom);
    dec(fResourceOrder[1], TradeCount);
    inc(fResources[fResTo], TradeCount * RatioTo);
    fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self, fResTo, TradeCount * RatioTo);
  end;
end;


procedure TKMHouseMarket.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
begin
  Assert(aCount <= fResources[aResource]);

  dec(fResources[aResource], aCount);
end;


procedure TKMHouseMarket.SetResFrom(Value: TResourceType);
begin
  if fResourceOrder[1] > 0 then Exit;
  fResFrom := Value;
  if fResTo = fResFrom then
    fResTo := rt_None;
end;


procedure TKMHouseMarket.SetResTo(Value: TResourceType);
begin
  if fResourceOrder[1] > 0 then Exit;
  fResTo := Value;
  if fResFrom = fResTo then
    fResFrom := rt_None;
end;


//Player has changed the amount of order
procedure TKMHouseMarket.ResEditOrder(aID:byte; aAmount:integer);
var Count: integer;
begin
  if (fResFrom = rt_None) or (fResTo = rt_None) or (fResFrom = fResTo) then Exit;

  fResourceOrder[1] := EnsureRange(fResourceOrder[1] + aAmount, 0, MAX_ORDER);

  //Try to make an exchange from existing resources
  AttemptExchange;

  //If player cancelled exchange then move all remainders of From resource to Offers list
  if fResourceOrder[1] = 0 then
    fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self, fResFrom, fResources[fResFrom]);

  //@Lewin: If player has cancelled the exchange and then started it again resources will not be
  //removed from offers list and perhaps serf will carry them off the marketplace

  //How much do we need to ask to add to delivery system
  Count := (fResourceOrder[1] * RatioFrom - fResourceDeliveryCount[1]);

  if Count > 0 then
    fPlayers.Player[fOwner].DeliverList.AddNewDemand(Self, nil, fResFrom, Count, dt_Once, di_Norm)
  else begin
    fPlayers.Player[fOwner].DeliverList.RemoveDemand(Self);
    fPlayers.Player[fOwner].DeliverList.AddNewDemand(Self, nil, fResFrom, fResourceOrder[1] * RatioFrom, dt_Once, di_Norm)
  end;

  //@Lewin: There's a flaw, if we order to exchange From=1500 resources they all will be
  //immediately added to delivery list. Maybe we should add 5-10 and add new demands to list upon
  //recieving resources instead? Or we let than be handled by delivery list which will be smart
  //enough to handicap such massive deliveries (to allow other deliveries run as well)
  fResourceDeliveryCount[1] := fResourceOrder[1] * RatioFrom;
end;


constructor TKMHouseMarket.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fResFrom, SizeOf(fResFrom));
  LoadStream.Read(fResTo, SizeOf(fResTo));
  LoadStream.Read(fResources, SizeOf(fResources));
end;


procedure TKMHouseMarket.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fResFrom, SizeOf(fResFrom));
  SaveStream.Write(fResTo, SizeOf(fResTo));
  SaveStream.Write(fResources, SizeOf(fResources));
end;


{ TKMHouseSchool }
constructor TKMHouseSchool.Create(aHouseType: THouseType; PosX, PosY: integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
var i:integer;
begin
  Inherited;

  for i:=Low(UnitQueue) to High(UnitQueue) do
    UnitQueue[i] := ut_None;
end;


constructor TKMHouseSchool.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(UnitWIP, 4);
  LoadStream.Read(HideOneGold);
  LoadStream.Read(UnitTrainProgress);
  LoadStream.Read(UnitQueue, SizeOf(UnitQueue));
end;


procedure TKMHouseSchool.SyncLoad;
begin
  UnitWIP := fPlayers.GetUnitByID(cardinal(UnitWIP));
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSchool.CloseHouse(IsEditor:boolean=false);
var i:integer;
begin
  for i:=2 to length(UnitQueue) do UnitQueue[i]:=ut_None;
  RemUnitFromQueue(1); //Remove WIP unit
  Inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSchool.ResAddToIn(aResource:TResourceType; const aCount:word=1);
begin
  Inherited;

  if UnitWIP = nil then
    StartTrainingUnit;
end;


procedure TKMHouseSchool.AddUnitToQueue(aUnit:TUnitType; aCount:byte);
var i,k:integer;
begin
  for k:=1 to aCount do
  for i:=1 to length(UnitQueue) do
  if UnitQueue[i]=ut_None then begin
    UnitQueue[i] := aUnit;
    if i=1 then StartTrainingUnit; //If thats the first unit then start training it
    break;
  end;
end;


//DoCancelTraining and remove untrained unit
procedure TKMHouseSchool.RemUnitFromQueue(aID:integer);
var i:integer;
begin
  if UnitQueue[aID] = ut_None then exit; //Ignore clicks on empty queue items

  if aID = 1 then begin
    SetState(hst_Idle);
    if UnitWIP<>nil then begin
      fTerrain.UnitAdd(GetEntrance, UnitWIP); //We removed it on StartTraining and must restore so CloseUnit properly removes it
      TKMUnit(UnitWIP).CloseUnit; //Make sure unit started training
      HideOneGold := false;
    end;
    UnitWIP := nil;
  end;

  for i:=aID to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty

  if aID = 1 then
    if UnitQueue[1]<>ut_None then StartTrainingUnit;
end;


procedure TKMHouseSchool.StartTrainingUnit;
begin
  //If there's yet no unit in training
  if UnitQueue[1] = ut_None then exit;
  if CheckResIn(rt_Gold) = 0 then exit;
  HideOneGold := true;
  UnitWIP := fPlayers.Player[fOwner].TrainUnit(UnitQueue[1],GetEntrance);//Create Unit
  WorkAnimStep := 0;
  fTerrain.UnitRem(GetEntrance); //Adding a unit automatically sets IsUnit, but as the unit is inside for this case we don't want that
  TKMUnit(UnitWIP).SetUnitTask := TTaskSelfTrain.Create(UnitWIP,Self);
end;


//To be called only by Unit itself when it's trained!
procedure TKMHouseSchool.UnitTrainingComplete;
var i:integer;
begin
  UnitWIP := nil;
  ResTakeFromIn(rt_Gold); //Do the goldtaking
  HideOneGold:=false;
  for i:=1 to length(UnitQueue)-1 do UnitQueue[i]:=UnitQueue[i+1]; //Shift by one
  UnitQueue[length(UnitQueue)]:=ut_None; //Set the last one empty
  if UnitQueue[1]<>ut_None then StartTrainingUnit;
  UnitTrainProgress:=0;
end;


//Return training progress as in 0..100% range
function TKMHouseSchool.GetTrainingProgress:byte;
begin
  if UnitWIP = nil then
    Result := 0
  else
    Result := EnsureRange(round((
              byte(ha_Work2 in fCurrentAction.SubAction) * 30 +
              byte(ha_Work3 in fCurrentAction.SubAction) * 60 +
              byte(ha_Work4 in fCurrentAction.SubAction) * 90 +
              byte(ha_Work5 in fCurrentAction.SubAction) * 120 +
              byte(fCurrentAction.State = hst_Work) * WorkAnimStep
              )/1.5), 0, 100);
end;


procedure TKMHouseSchool.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if TKMUnit(UnitWIP) <> nil then
    SaveStream.Write(TKMUnit(UnitWIP).ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(HideOneGold);
  SaveStream.Write(UnitTrainProgress);
  SaveStream.Write(UnitQueue, SizeOf(UnitQueue));
end;


{ TKMHouseStore }
constructor TKMHouseStore.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
var FirstStore: TKMHouseStore; w:TResourceType;
begin
  Inherited;
  //A new storehouse should inherrit the accept properies of the first storehouse of that player,
  //which stops a sudden flow of unwanted resources to it as soon as it is create.
  FirstStore := TKMHouseStore(fPlayers[fOwner].FindHouse(ht_Store,1));
  if (FirstStore <> nil) and not FirstStore.IsDestroyed then
    for w:=WARE_MIN to WARE_MAX do
      NotAcceptFlag[w] := FirstStore.NotAcceptFlag[w];
end;


constructor TKMHouseStore.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


procedure TKMHouseStore.ResAddToIn(aResource:TResourceType; const aCount:word=1);
var i:TResourceType;
begin
  case aResource of
    rt_All:     for i:=Low(ResourceCount) to High(ResourceCount) do begin
                  ResourceCount[i] := EnsureRange(ResourceCount[i]+aCount, 0, High(Word));
                  fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self, i, aCount);
                end;
    WARE_MIN..
    WARE_MAX:   begin
                  ResourceCount[aResource]:=EnsureRange(ResourceCount[aResource]+aCount, 0, High(Word));
                  fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self,aResource,aCount);
                end;
    else        raise ELocError.Create('Cant''t add '+fResource.Resources[aResource].Name, GetPosition);
  end;
end;


function TKMHouseStore.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [WARE_MIN..WARE_MAX] then
    Result := ResourceCount[aResource]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


procedure TKMHouseStore.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
begin
  Assert(aCount <= ResourceCount[aResource]);

  dec(ResourceCount[aResource], aCount);
end;


procedure TKMHouseStore.ToggleAcceptFlag(aRes:TResourceType);
var i:TResourceType; ApplyCheat:boolean;
begin
  Assert(aRes in [WARE_MIN .. WARE_MAX]); //Dunno why thats happening sometimes..

  if CHEATS_ENABLED and (MULTIPLAYER_CHEATS or not fGame.MultiplayerMode) then begin
    ApplyCheat := true;

    for i:=Low(ResourceCount) to High(ResourceCount) do
      ApplyCheat := ApplyCheat and (NotAcceptFlag[i] = boolean(CheatStorePattern[i]));

    if ApplyCheat and (aRes = rt_Arbalet) then begin
      ResAddToIn(rt_All, 10);
      exit;
    end;
    if ApplyCheat and (aRes = rt_Horse) and not fGame.MultiplayerMode then begin
      fGame.RequestGameHold(gr_Win);
      exit;
    end;
    if ApplyCheat and (aRes = WARE_MAX) and not fGame.MultiplayerMode then begin
      fGame.RequestGameHold(gr_Defeat);
      exit;
    end;
  end;

  NotAcceptFlag[aRes] := not NotAcceptFlag[aRes];
end;


procedure TKMHouseStore.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


{ TKMHouseBarracks }
constructor TKMHouseBarracks.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
begin
  Inherited;
  RecruitsList := TList.Create;
end;


constructor TKMHouseBarracks.Load(LoadStream:TKMemoryStream);
var i,aCount:integer; U:TKMUnit;
begin
  Inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  RecruitsList := TList.Create;
  LoadStream.Read(aCount);
  if aCount <> 0 then
    for i := 1 to aCount do
    begin
      LoadStream.Read(U, 4); //subst on syncload
      RecruitsList.Add(U);
    end;
end;


procedure TKMHouseBarracks.SyncLoad;
var i:integer;
begin
  for i:=0 to RecruitsList.Count-1 do
    RecruitsList.Items[i] := fPlayers.GetUnitByID(cardinal(RecruitsList.Items[i]));
end;


destructor TKMHouseBarracks.Destroy;
begin
  RecruitsList.Free;
  Inherited;
end;


procedure TKMHouseBarracks.ResAddToIn(aResource:TResourceType; const aCount:word=1);
var i:TResourceType;
begin
  case aResource of
    rt_Warfare: for i:=Low(ResourceCount) to High(ResourceCount) do
                  ResourceCount[i] := EnsureRange(ResourceCount[i]+aCount, 0, High(Word));
    WARFARE_MIN..
    WARFARE_MAX:   ResourceCount[aResource] := EnsureRange(ResourceCount[aResource]+aCount, 0, High(Word));
    else        raise ELocError.Create('Cant''t add ' + fResource.Resources[aResource].Name, GetPosition);
  end;
end;


function TKMHouseBarracks.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [WARFARE_MIN..WARFARE_MAX] then
    Result := ResourceCount[aResource]
  else
  if aResource in [rt_Wood, rt_Stone] then
    Result := 0 //Wood/stone in building stage
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


procedure TKMHouseBarracks.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
begin
  Assert(aCount <= ResourceCount[aResource]);
  dec(ResourceCount[aResource], aCount);
end;


function TKMHouseBarracks.CanEquip(aUnitType: TUnitType):boolean;
var i:integer;
begin
  Result := RecruitsList.Count > 0; //Can't equip anything without recruits

  for i:=1 to 4 do
  if TroopCost[aUnitType,i] <> rt_None then //Can't equip if we don't have a required resource
    Result := Result and (ResourceCount[TroopCost[aUnitType,i]] > 0);
end;


//Equip a new soldier and make him walk out of the house
procedure TKMHouseBarracks.Equip(aUnitType: TUnitType; aCount:byte);
var i,k:integer;
    Soldier:TKMUnitWarrior;
    LinkUnit:TKMUnitWarrior;
begin
  Assert(aUnitType in [WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX]);

  LinkUnit := nil;

  for k := 1 to aCount do
  begin
    //Make sure we have enough resources to equip a unit
    if not CanEquip(aUnitType) then Exit;

    //Take resources
    for i:=1 to 4 do
      if TroopCost[aUnitType,i] <> rt_None then
        dec(ResourceCount[TroopCost[aUnitType,i]]);

    TKMUnitRecruit(RecruitsList.Items[0]).DestroyInBarracks; //Special way to kill the unit because it is in a house
    RecruitsList.Delete(0); //Delete first recruit in the list

    //Make new unit
    Soldier := TKMUnitWarrior(fPlayers.Player[fOwner].AddUnit(aUnitType,GetEntrance,false,true));
    fTerrain.UnitRem(GetEntrance); //Adding a unit automatically sets IsUnit, but as the unit is inside for this case we don't want that
    Soldier.Visible := false; //Make him invisible as he is inside the barracks
    Soldier.Condition := Round(TROOPS_TRAINED_CONDITION*UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
    Soldier.OrderLocDir := KMPointDir(KMPointBelow(GetEntrance),dir_N); //Position in front of the barracks facing north
    Soldier.SetActionGoIn(ua_Walk, gd_GoOutside, Self);

    //AI do not need auto linking, they manage linking themselves
    if fPlayers.Player[fOwner].PlayerType = pt_Human then
    begin
      if LinkUnit = nil then
        LinkUnit := Soldier.FindLinkUnit(GetEntrance);
      if LinkUnit <> nil then
        Soldier.OrderLinkTo(LinkUnit);
      LinkUnit := Soldier; //For future units if Count > 1
    end;
  end;
end;


procedure TKMHouseBarracks.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(RecruitsList.Count);
  for i:=1 to RecruitsList.Count do
    SaveStream.Write(TKMUnit(RecruitsList.Items[i-1]).ID) //Store ID
end;


{ THouseAction }
constructor THouseAction.Create(aHouse:TKMHouse; aHouseState: THouseState);
begin
  Inherited Create;
  fHouse := aHouse;
  SetHouseState(aHouseState);
end;


procedure THouseAction.SetHouseState(aHouseState: THouseState);
begin
  fHouseState := aHouseState;
  case fHouseState of
    hst_Idle:   begin
                  SubActionRem([ha_Work1..ha_Smoke]); //remove all work attributes
                  SubActionAdd([ha_Idle]);
                end;
    hst_Work:   SubActionRem([ha_Idle]);
    hst_Empty:  SubActionRem([ha_Idle]);
  end;
end;


procedure THouseAction.SubActionWork(aActionSet: THouseActionType);
begin
  SubActionRem([ha_Work1..ha_Work5]); //Remove all work
  fSubAction := fSubAction + [aActionSet];
  if fHouse.fHouseType <> ht_Mill then fHouse.WorkAnimStep := 0; //Exception for mill so that the windmill doesn't jump frames
end;


procedure THouseAction.SubActionAdd(aActionSet: THouseActionSet);
begin
  fSubAction := fSubAction + aActionSet;
end;


procedure THouseAction.SubActionRem(aActionSet: THouseActionSet);
begin
  fSubAction := fSubAction - aActionSet;
end;


procedure THouseAction.Save(SaveStream:TKMemoryStream);
begin
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fHouseState, SizeOf(fHouseState));
  SaveStream.Write(fSubAction, SizeOf(fSubAction));
end;


procedure THouseAction.Load(LoadStream:TKMemoryStream);
begin
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fHouseState, SizeOf(fHouseState));
  LoadStream.Read(fSubAction, SizeOf(fSubAction));
end;


procedure TKMHouseTower.Paint;
var i,k:integer;
begin
  Inherited;

  if SHOW_ATTACK_RADIUS then
    for i:=-round(RANGE_WATCHTOWER_MAX)-1 to round(RANGE_WATCHTOWER_MAX) do
    for k:=-round(RANGE_WATCHTOWER_MAX)-1 to round(RANGE_WATCHTOWER_MAX) do
    if InRange(GetLength(i,k),RANGE_WATCHTOWER_MIN,RANGE_WATCHTOWER_MAX) then
    if fTerrain.TileInMapCoords(GetPosition.X+k,GetPosition.Y+i) then
      fRenderAux.Quad(GetPosition.X+k,GetPosition.Y+i, $40FFFFFF);
end;


{ TKMHousesCollection }
function TKMHousesCollection.AddToCollection(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint; aHBS:THouseBuildState):TKMHouse;
var I:integer;
begin
  case aHouseType of
    ht_Swine:       I := Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Stables:     I := Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Inn:         I := Inherited Add(TKMHouseInn.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Marketplace: I := Inherited Add(TKMHouseMarket.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_School:      I := Inherited Add(TKMHouseSchool.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Barracks:    I := Inherited Add(TKMHouseBarracks.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Store:       I := Inherited Add(TKMHouseStore.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_WatchTower:  I := Inherited Add(TKMHouseTower.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    else            I := Inherited Add(TKMHouse.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  end;

  if I = -1 then
    Result := nil
  else
    Result := Items[I];
end;


function TKMHousesCollection.GetHouse(Index: Integer): TKMHouse;
begin
  Result := TKMHouse(Items[Index])
end;


procedure TKMHousesCollection.SetHouse(Index: Integer; Item: TKMHouse);
begin
  Items[Index] := Item;
end;


function TKMHousesCollection.AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint; RelativeEntrance:boolean):TKMHouse;
begin
  if RelativeEntrance then
    Result := AddToCollection(aHouseType,PosX - fResource.HouseDat[aHouseType].EntranceOffsetX,PosY,aOwner,hbs_Done)
  else
    Result := AddToCollection(aHouseType,PosX,PosY,aOwner,hbs_Done);
end;


{Add a plan for house}
function TKMHousesCollection.AddPlan(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint):TKMHouse;
begin
  Result := AddToCollection(aHouseType,PosX,PosY,aOwner,hbs_Glyph);
end;


procedure TKMHousesCollection.RemoveHouse(aHouse:TKMHouse);
begin
  aHouse.CloseHouse(True); //Should free up the house properly (freeing terrain usage and memory)
  aHouse.Free;
  Remove(aHouse);
end;


procedure TKMHousesCollection.OwnerUpdate(aOwner:TPlayerIndex);
var i:integer;
begin
  for i:=0 to Count-1 do
    Houses[i].fOwner := aOwner;
end;


function TKMHousesCollection.HitTest(X, Y: Integer): TKMHouse;
var i:integer;
begin
  Result:= nil;
  for i:=0 to Count-1 do
    if Houses[i].HitTest(X, Y) and (not Houses[i].IsDestroyed) then
    begin
      Result := TKMHouse(Items[i]);
      Break;
    end;
end;


function TKMHousesCollection.GetHouseByID(aID: Integer): TKMHouse;
var i:integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if aID = Houses[i].ID then
    begin
      Result := Houses[i];
      exit;
    end;
end;


//Should find closest house to Loc
function TKMHousesCollection.FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
var i:integer;
  Dist,Bid:single;
begin
  Result:= nil;
  Bid:=0;

  for i:=0 to Count-1 do
    if (fResource.HouseDat[Houses[i].fHouseType].OwnerType = aUnitType) and //If Unit can work in here
       (not Houses[i].fHasOwner) and                              //If there's yet no owner
       (not Houses[i].IsDestroyed) and
       (Houses[i].IsComplete) then                               //If house is built
    begin

      Dist:=KMLength(Loc,Houses[i].GetPosition);

      //Always prefer Towers to Barracks by making Barracks Bid much less attractive
      //In case of multiple barracks, prefer the one with less recruits already
      if Houses[i].HouseType = ht_Barracks then Dist:=(Dist*1000) + (TKMHouseBarracks(Houses[i]).RecruitsList.Count*10000);

      if (Bid=0)or(Bid>Dist) then
      begin
        Bid:=Dist;
        Result := Houses[i];
      end;

    end;

  if Result<>nil then
  if Result.fHouseType<>ht_Barracks then Result.fHasOwner:=true; //Become owner except Barracks;
end;


//Find closest house to given position
//or
//Find house by index (1st, 2nd)
function TKMHousesCollection.FindHouse(aType:THouseType; X,Y:word; const aIndex:byte=1; aOnlyCompleted:boolean=true): TKMHouse;
var
  i,id: integer;
  UsePosition: boolean;
  BestMatch,Dist: single;
begin
  Result := nil;
  id := 0;
  BestMatch := MaxSingle; //Any distance will be closer than that
  UsePosition := X*Y<>0; //Calculate this once to save computing lots of multiplications
  Assert((not UsePosition)or(aIndex=1), 'Can''t find house basing both on Position and Index');

  for i:=0 to Count-1 do
  if ((Houses[i].fHouseType = aType) or (aType = ht_Any)) and (Houses[i].IsComplete or not aOnlyCompleted) and not Houses[i].fIsDestroyed then
  begin
      inc(id);
      if UsePosition then
      begin
          Dist := GetLength(Houses[i].GetPosition,KMPoint(X,Y));
          if BestMatch = -1 then BestMatch := Dist; //Initialize for first use
          if Dist < BestMatch then begin
            BestMatch := Dist;
            Result := Houses[i];
          end;
      end else
          if aIndex = id then begin//Take the N-th result
            Result := Houses[i];
            exit;
          end;
  end;
end;


procedure TKMHousesCollection.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('Houses');
  //Multiplayer saves must be identical, thus we force that no house is selected
  if (fSelectedHouse <> nil) and not fGame.MultiplayerMode then
    SaveStream.Write(fSelectedHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(Count);
  for i := 0 to Count - 1 do
  begin
    //We save house type to know which house class to load
    SaveStream.Write(Houses[i].HouseType, SizeOf(Houses[i].HouseType));
    Houses[i].Save(SaveStream);
  end;
end;


procedure TKMHousesCollection.Load(LoadStream:TKMemoryStream);
var i,HouseCount:integer; s:string; HouseType:THouseType;
begin
  LoadStream.Read(s);
  Assert(s = 'Houses');
  LoadStream.Read(fSelectedHouse, 4);
  LoadStream.Read(HouseCount);
  for i := 0 to HouseCount - 1 do
  begin
    LoadStream.Read(HouseType, SizeOf(HouseType));
    case HouseType of
      ht_Swine:       Inherited Add(TKMHouseSwineStable.Load(LoadStream));
      ht_Stables:     Inherited Add(TKMHouseSwineStable.Load(LoadStream));
      ht_Inn:         Inherited Add(TKMHouseInn.Load(LoadStream));
      ht_Marketplace: Inherited Add(TKMHouseMarket.Load(LoadStream));
      ht_School:      Inherited Add(TKMHouseSchool.Load(LoadStream));
      ht_Barracks:    Inherited Add(TKMHouseBarracks.Load(LoadStream));
      ht_Store:       Inherited Add(TKMHouseStore.Load(LoadStream));
      ht_WatchTower:  Inherited Add(TKMHouseTower.Load(LoadStream));
      else            Inherited Add(TKMHouse.Load(LoadStream));
    end;
  end;
end;


procedure TKMHousesCollection.SyncLoad;
var i:integer;
begin
  fSelectedHouse := fPlayers.GetHouseByID(cardinal(fSelectedHouse));
  for i := 0 to Count - 1 do
  begin
    Houses[i].SyncLoad;
    if Houses[i].fCurrentAction<>nil then
      Houses[i].fCurrentAction.fHouse := fPlayers.GetHouseByID(cardinal(Houses[i].fCurrentAction.fHouse));
  end;
end;


//Update resource requested counts for all houses
procedure TKMHousesCollection.UpdateResRequest;
var i:integer;
begin
  for i:=0 to Count-1 do
  if (not Houses[i].IsDestroyed) and (Houses[i].fBuildState = hbs_Done) then
    Houses[i].UpdateResRequest;
end;


procedure TKMHousesCollection.UpdateState;
var
  i,ID:integer;
  IDsToDelete: array of integer;
begin
  ID := 0;
  for i:=0 to Count-1 do
  if not Houses[i].IsDestroyed then
    Houses[i].UpdateState
  else //Else try to destroy the house object if all pointers are freed
    if FREE_POINTERS and (Houses[i].PointerCount = 0) then
    begin
      SetLength(IDsToDelete,ID+1);
      IDsToDelete[ID] := i;
      inc(ID);
    end;
  //Must remove list entry after for loop is complete otherwise the indexes change
  if ID <> 0 then
    for i:=ID-1 downto 0 do
    begin
      TKMHouse(Items[IDsToDelete[i]]).Free; //Because no one needs this anymore it must DIE!!!!! :D
      Delete(IDsToDelete[i]);
    end;
end;


procedure TKMHousesCollection.IncAnimStep;
var i:integer;
begin
  for i := 0 to Count - 1 do
    Houses[i].IncAnimStep;
end;


function TKMHousesCollection.GetTotalPointers: integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to Count-1 do
    Result := Result + Houses[i].PointerCount;
end;


procedure TKMHousesCollection.Paint;
var i:integer; x1,x2,y1,y2,Margin:integer;
begin
  if TEST_VIEW_CLIP_INSET then Margin:=-3 else Margin:=3;
  x1:=fViewport.GetClip.Left-Margin;  x2:=fViewport.GetClip.Right+Margin;
  y1:=fViewport.GetClip.Top -Margin;  y2:=fViewport.GetClip.Bottom+Margin;

  for i := 0 to Count - 1 do
  if not Houses[i].IsDestroyed then
  if (InRange(Houses[i].fPosition.X,x1,x2) and InRange(Houses[i].fPosition.Y,y1,y2)) then
    Houses[i].Paint;
end;



end.
