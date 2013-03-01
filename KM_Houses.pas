unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
   Classes, KromUtils, Math, SysUtils,
   KM_CommonClasses, KM_Defaults, KM_Terrain, KM_Points;

  {Everything related to houses is here}
type
  TWoodcutterMode = (wcm_Chop, wcm_ChopAndPlant);

  TKMHouse = class;
  TKMHouseEvent = procedure(aHouse: TKMHouse) of object;
  TKMHouseFromEvent = procedure(aHouse: TKMHouse; aFrom: TPlayerIndex) of object;

  THouseAction = class
  private
    fHouse: TKMHouse;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
    procedure SetHouseState(aHouseState: THouseState);
  public
    constructor Create(aHouse:TKMHouse; aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    property State: THouseState read fHouseState write SetHouseState;
    property SubAction: THouseActionSet read fSubAction;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


  TKMHouse = class
  private
    fID: Integer; //unique ID, used for save/load to sync to
    fHouseType: THouseType; //House type

    fBuildSupplyWood: Byte; //How much Wood was delivered to house building site
    fBuildSupplyStone: Byte; //How much Stone was delivered to house building site
    fBuildReserve: Byte; //Take one build supply resource into reserve and "build from it"
    fBuildingProgress: Word; //That is how many efforts were put into building (Wooding+Stoning)
    fDamage: Word; //Damaged inflicted to house

    fHasOwner: Boolean; //which is some TKMUnit
    fBuildingRepair: Boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: Boolean; //If on then no wares will be delivered here

    fResourceIn: array[1..4] of Byte; //Resource count in input
    fResourceDeliveryCount: array[1..4] of Word; //Count of the resources we have ordered for the input (used for ware distribution)
    fResourceOut: array[1..4]of Byte; //Resource count in output
    fResourceOrder: array[1..4]of Word; //If HousePlaceOrders=true then here are production orders
    fLastOrderProduced: Byte; //Last order we made (1..4)

    WorkAnimStep: cardinal; //Used for Work and etc.. which is not in sync with Flags

    fIsDestroyed: Boolean;
    RemoveRoadWhenDemolish: Boolean;
    fPointerCount: Cardinal;
    fTimeSinceUnoccupiedReminder: Integer;

    procedure Activate(aWasBuilt: Boolean); virtual;
    procedure EnableRepair;
    procedure DisableRepair;

    procedure MakeSound; dynamic; //Swine/stables make extra sounds
    function GetResDistribution(aID:byte):byte; //Will use GetRatio from mission settings to find distribution amount
  protected
    fBuildState: THouseBuildState; // = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);
    FlagAnimStep: Cardinal; //Used for Flags and Burning animation
    fOwner: TPlayerIndex; //House owner player, determines flag color as well
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle
    ResourceDepletedMsgIssued: boolean;
    DoorwayUse: byte; //number of units using our door way. Used for sliding.
    OnDestroyed: TKMHouseFromEvent;

    constructor Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;
    function GetHousePointer: TKMHouse; //Returns self and adds one to the pointer counter
    procedure ReleaseHousePointer; //Decreases the pointer counter
    property PointerCount: Cardinal read fPointerCount;

    procedure DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False); virtual;
    property ID: Integer read fID;

    property GetPosition: TKMPoint read fPosition;
    procedure SetPosition(aPos: TKMPoint); //Used only by map editor
    function GetEntrance: TKMPoint;
    function GetClosestCell(aPos: TKMPoint): TKMPoint;
    function GetDistance(aPos: TKMPoint): Single;
    function InReach(aPos: TKMPoint; aDistance: Single): Boolean;
    procedure GetListOfCellsAround(Cells: TKMPointDirList; aPassability: TPassability);
    procedure GetListOfCellsWithin(Cells: TKMPointList);
    function GetRandomCellWithin: TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property HouseType: THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write fWareDelivery;
    property GetHasOwner:boolean read fHasOwner write fHasOwner;
    property Owner:TPlayerIndex read fOwner;
    function GetHealth:word;
    function GetBuildWoodDelivered: Byte;
    function GetBuildStoneDelivered: Byte;

    property BuildingState: THouseBuildState read fBuildState write fBuildState;
    procedure IncBuildingProgress;
    function MaxHealth:word;
    procedure AddDamage(aFrom: TPlayerIndex; aAmount: Word; aIsEditor: Boolean = False);
    procedure AddRepair(aAmount:word=5);
    procedure UpdateDamage;
    procedure RepairToggle;

    function IsStone:boolean;
    function IsComplete:boolean;
    function IsDamaged:boolean;
    property IsDestroyed:boolean read fIsDestroyed;
    property GetDamage:word read fDamage;

    procedure SetState(aState: THouseState);
    function GetState: THouseState;

    function CheckResIn(aResource:TResourceType):word; virtual;
    function CheckResOut(aResource:TResourceType):byte;
    function CheckResOrder(aID:byte):word; virtual;
    function PickOrder:byte;
    procedure SetLastOrderProduced(aResource:TResourceType);
    function CheckResToBuild:boolean;
    function GetMaxInRes: Word;
    procedure ResAddToIn(aResource:TResourceType; aCount:word=1; aFromScript:boolean=false); virtual; //override for School and etc..
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToBuild(aResource:TResourceType);
    procedure ResTakeFromIn(aResource:TResourceType; aCount:byte=1);
    procedure ResTakeFromOut(aResource:TResourceType; const aCount: Word=1); virtual;
    procedure ResEditOrder(aID:byte; aAmount:integer); virtual;
    function ResCanAddToIn(aRes: TResourceType): Boolean; virtual;

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
    Eater: array [1..6] of record //only 6 units are allowed in the inn
      UnitType: TUnitType;
      FoodKind: TResourceType; //What kind of food eater eats
      EatStep: Cardinal;
    end;
  public
    constructor Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    function EaterGetsInside(aUnitType:TUnitType):byte;
    procedure UpdateEater(aID:byte; aFoodKind: TResourceType);
    procedure EatersGoesOut(aID:byte);
    function HasFood:boolean;
    function HasSpace:boolean;
    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Paint; override; //Render all eaters
  end;


  {School has one unique property - queue of units to be trained, 1 wip + 5 in line}
  TKMHouseSchool = class(TKMHouse)
  private
    UnitWIP: Pointer;  //can't replace with TKMUnit since it will lead to circular reference in KM_House-KM_Units
    fHideOneGold: Boolean; //Hide the gold incase Player cancels the training, then we won't need to tweak DeliverQueue order
    fTrainProgress: Byte; //Was it 150 steps in KaM?
  public
    Queue: array [0..5] of TUnitType; //Used in UI. First item is the unit currently being trained, 1..5 are the actual queue
    constructor Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False); override;
    procedure ResAddToIn(aResource: TResourceType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function AddUnitToQueue(aUnit: TUnitType; aCount: Byte): Byte; //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure UnitTrainingComplete(aUnit: Pointer); //This should shift queue filling rest with ut_None
    function GetTrainingProgress: Single;
    function QueueIsEmpty: Boolean;
    property HideOneGold: Boolean read fHideOneGold;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  {Barracks has 11 resources and Recruits}
  TKMHouseBarracks = class(TKMHouse)
  private
    ResourceCount: array [WARFARE_MIN..WARFARE_MAX] of Word;
  public
    NotAcceptFlag: array [WARFARE_MIN .. WARFARE_MAX] of Boolean;
    RecruitsList: TList;
    constructor Create(aID: cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    procedure Activate(aWasBuilt: Boolean); override;
    procedure DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False); override;
    procedure ResAddToIn(aResource: TResourceType; aCount: Word = 1; aFromScript: Boolean = False); override;
    procedure ResTakeFromOut(aResource: TResourceType; const aCount: Word = 1); override;
    function CheckResIn(aResource: TResourceType): Word; override;
    function CanTakeResOut(aResource: TResourceType): Boolean;
    function ResCanAddToIn(aRes: TResourceType): Boolean; override;
    function CanEquip(aUnitType: TUnitType): Boolean;
    procedure ToggleAcceptFlag(aRes: TResourceType);
    function Equip(aUnitType: TUnitType; aCount: Byte): Byte;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  private
    ResourceCount: array [WARE_MIN .. WARE_MAX] of Word;
    procedure Activate(aWasBuilt: Boolean); override;
  public
    NotAcceptFlag: array [WARE_MIN .. WARE_MAX] of Boolean;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False); override;
    procedure ToggleAcceptFlag(aRes: TResourceType);
    procedure ResAddToIn(aResource: TResourceType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function CheckResIn(aResource: TResourceType): Word; override;
    procedure ResTakeFromOut(aResource: TResourceType; const aCount: Word = 1); override;
    function ResCanAddToIn(aRes: TResourceType): Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    end;


  TKMHouseTower = class(TKMHouse)
  public
    procedure Paint; override; //Render debug radius overlay
  end;


  TKMHouseWoodcutters = class(TKMHouse)
  private
    fWoodcutterMode: TWoodcutterMode;
    procedure SetWoodcutterMode(aWoodcutterMode: TWoodcutterMode);
  public
    property WoodcutterMode: TWoodcutterMode read fWoodcutterMode write SetWoodcutterMode;
    constructor Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


  TKMHousesCollection = class
  private
    fHouses: TKMList; //Private to hide methods we don't want to expose
    function AddToCollection(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerIndex; aHBS:THouseBuildState):TKMHouse;
    function GetHouse(aIndex: Integer): TKMHouse;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddHouse(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerIndex; RelativeEntrance:boolean):TKMHouse;
    function AddHouseWIP(aHouseType: THouseType; PosX,PosY:integer; aOwner: TPlayerIndex):TKMHouse;
    property Count: Integer read GetCount;
    procedure OwnerUpdate(aOwner:TPlayerIndex);
    property Houses[aIndex: Integer]: TKMHouse read GetHouse; default;
    function HitTest(X, Y: Integer): TKMHouse;
    function GetHouseByID(aID: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType: TUnitType; Loc: TKMPoint): TKMHouse;
    function FindHouse(aType:THouseType; X,Y:word; const aIndex:byte=1; aOnlyCompleted:boolean=true): TKMHouse;
    function GetTotalPointers: Cardinal;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;
    procedure UpdateResRequest; //Change resource requested counts for all houses
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses
  KM_CommonTypes, KM_DeliverQueue, KM_RenderPool, KM_RenderAux, KM_HouseMarket, KM_Units, KM_Scripting,
  KM_Units_Warrior, KM_PlayersCollection, KM_Sound, KM_Game, KM_TextLibrary, KM_Player,
  KM_Resource, KM_ResourceResource, KM_ResourceHouse, KM_Utils;


{ TKMHouse }
constructor TKMHouse.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
var i: byte;
begin
  Assert((PosX <> 0) and (PosY <> 0)); // Can create only on map

  inherited Create;

  fPosition   := KMPoint (PosX, PosY);
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

  fID := aID;
  ResourceDepletedMsgIssued := false;

  if aBuildState = hbs_Done then //House was placed on map already Built e.g. in mission maker
  begin
    Activate(False);
    fBuildingProgress := fResource.HouseDat[fHouseType].MaxHealth;
    fTerrain.SetHouse(fPosition, fHouseType, hsBuilt, fOwner, (fGame <> nil) and (fGame.GameMode <> gmMapEd)); //Sets passability and flattens terrain if we're not in the map editor
  end else
    fTerrain.SetHouse(fPosition, fHouseType, hsFence, fOwner); //Terrain remains neutral yet
end;


constructor TKMHouse.Load(LoadStream:TKMemoryStream);
var i:integer; HasAct:boolean;
begin
  inherited Create;
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
  LoadStream.Read(fLastOrderProduced);
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
  inherited;
end;


{Returns self and adds on to the pointer counter}
function TKMHouse.GetHousePointer: TKMHouse;
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


procedure TKMHouse.Activate(aWasBuilt: Boolean);
var I: Integer; Res: TResourceType;
begin
  fPlayers[fOwner].Stats.HouseCreated(fHouseType,aWasBuilt); //Only activated houses count
  fPlayers.RevealForTeam(fOwner, fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_MAX);

  fCurrentAction:=THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_Flagpole,ha_Flag1..ha_Flag3]);

  UpdateDamage; //House might have been damaged during construction, so show flames when it is built

  for i:=1 to 4 do
  begin
    Res := fResource.HouseDat[fHouseType].ResInput[i];
    with fPlayers[fOwner].Deliveries.Queue do
    case Res of
      rt_None:    ;
      rt_Warfare: AddDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      rt_All:     AddDemand(Self, nil, Res, 1, dt_Always, di_Norm);
      else
      begin
        AddDemand(Self, nil, Res, GetResDistribution(i), dt_Once,   di_Norm); //Every new house needs 5 resourceunits
        inc(fResourceDeliveryCount[i],GetResDistribution(i)); //Keep track of how many resources we have on order (for distribution of wares)
      end;
    end;
  end;

end;


procedure TKMHouse.DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False);
var I: Integer; R: TResourceType;
begin
  if IsDestroyed then Exit;

  //We must do this before setting fIsDestroyed for scripting
  OnDestroyed(Self, aFrom);

  //If anyone still has a pointer to the house he should check for IsDestroyed flag
  fIsDestroyed := True;

  //Play sound
  if (fBuildState > hbs_NoGlyph) and not IsEditor then
    fSoundLib.Play(sfx_HouseDestroy, fPosition);

  fPlayers[fOwner].Stats.GoodConsumed(rt_Wood, fBuildSupplyWood);
  fPlayers[fOwner].Stats.GoodConsumed(rt_Stone, fBuildSupplyStone);

  for I := 1 to 4 do
  begin
    R := fResource.HouseDat[fHouseType].ResInput[I];
    if R in [WARE_MIN..WARE_MAX] then
      fPlayers[fOwner].Stats.GoodConsumed(R, fResourceIn[I]);
    R := fResource.HouseDat[fHouseType].ResOutput[I];
    if R in [WARE_MIN..WARE_MAX] then
      fPlayers[fOwner].Stats.GoodConsumed(R, fResourceOut[I]);
  end;

  fTerrain.SetHouse(fPosition, fHouseType, hsNone, -1);

  //Leave rubble
  if not IsEditor then
    fTerrain.AddHouseRemainder(fPosition, fHouseType, fBuildState);

  BuildingRepair := False; //Otherwise labourers will take task to repair when the house is destroyed
  if RemoveRoadWhenDemolish and (not (BuildingState in [hbs_Stone, hbs_Done]) or IsEditor) then
  begin
    if fTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay = to_Road then
    begin
      fTerrain.RemRoad(GetEntrance);
      if not IsEditor then
        fTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay := to_Dig3; //Remove road and leave dug earth behind
    end;
  end;

  FreeAndNil(fCurrentAction);

  //Leave disposing of units inside the house to themselves
end;


//Used by MapEditor
procedure TKMHouse.SetPosition(aPos: TKMPoint);
begin
  Assert(fGame.GameMode = gmMapEd);
  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  fTerrain.SetHouse(fPosition, fHouseType, hsNone, -1);
  fTerrain.RemRoad(GetEntrance);
  if MyPlayer.CanAddHousePlan(aPos, HouseType) then
  begin
    fPosition.X := aPos.X - fResource.HouseDat[fHouseType].EntranceOffsetX;
    fPosition.Y := aPos.Y;
  end;
  fTerrain.SetHouse(fPosition, fHouseType, hsBuilt, fOwner);
  fTerrain.SetField(GetEntrance, fOwner, ft_Road);
end;


{Return Entrance of the house, which is different than house position sometimes}
function TKMHouse.GetEntrance: TKMPoint;
begin
  Result.X := GetPosition.X + fResource.HouseDat[fHouseType].EntranceOffsetX;
  Result.Y := GetPosition.Y;
  Assert((Result.X > 0) and (Result.Y > 0));
end;


{Returns the closest cell of the house to aPos}
function TKMHouse.GetClosestCell(aPos: TKMPoint): TKMPoint;
var
  C: TKMPointList;
begin
  C := TKMPointList.Create;
  GetListOfCellsWithin(C);
  if not C.GetClosest(aPos, Result) then
    Assert(false, 'Could not find closest house cell');
  C.Free;
end;


{Return distance from aPos to the closest house tile}
function TKMHouse.GetDistance(aPos: TKMPoint): Single;
var
  I, K: Integer;
  Loc: TKMPoint;
  Test: Single;
  HouseArea: THouseArea;
begin
  Result := -1;
  Loc := fPosition;
  HouseArea := fResource.HouseDat[fHouseType].BuildArea;

  for I := max(Loc.Y - 3, 1) to Loc.Y do
    for K := max(Loc.X - 2, 1) to min(Loc.X + 1, fTerrain.MapX) do
      if HouseArea[I - Loc.Y + 4, K - Loc.X + 3] <> 0 then
      begin
        Test := KMLength(aPos, KMPoint(K, I));
        if (Result < 0) or (Test < Result) then
          Result := Test;
      end;
end;


//Check if house is within reach of given Distance (optimized version for PathFinding)
//Check precise distance when we are close enough
function TKMHouse.InReach(aPos: TKMPoint; aDistance: Single): Boolean;
begin
  //+6 is the worst case with the barracks, distance from fPosition to top left tile of house could be > 5
  if KMLengthDiag(aPos, fPosition) >= aDistance + 6 then
    Result := False //We are sure they are not close enough to the house
  else
    //We need to perform a precise check
    Result := GetDistance(aPos) <= aDistance;
end;


procedure TKMHouse.GetListOfCellsAround(Cells: TKMPointDirList; aPassability: TPassability);
var
  I,K: Integer;
  Loc: TKMPoint;
  HA: THouseArea;

  procedure AddLoc(X,Y: Word; Dir: TKMDirection);
  begin
    //Check that the passabilty is correct, as the house may be placed against blocked terrain
    if fTerrain.CheckPassability(KMPoint(X,Y), aPassability) then
      Cells.AddItem(KMPointDir(X, Y, Dir));
  end;

begin
  Cells.Clear;
  Loc := fPosition;
  HA := fResource.HouseDat[fHouseType].BuildArea;

  for I := 1 to 4 do for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    if (I = 1) or (HA[I-1,K] = 0) then
      AddLoc(Loc.X + K - 3, Loc.Y + I - 4 - 1, dir_S); //Above
    if (I = 4) or (HA[I+1,K] = 0) then
      AddLoc(Loc.X + K - 3, Loc.Y + I - 4 + 1, dir_N); //Below
    if (K = 4) or (HA[I,K+1] = 0) then
      AddLoc(Loc.X + K - 3 + 1, Loc.Y + I - 4, dir_W); //FromRight
    if (K = 1) or (HA[I,K-1] = 0) then
      AddLoc(Loc.X + K - 3 - 1, Loc.Y + I - 4, dir_E); //FromLeft
  end;
end;


procedure TKMHouse.GetListOfCellsWithin(Cells: TKMPointList);
var
  i,k: Integer;
  Loc: TKMPoint;
  HouseArea: THouseArea;
begin
  Cells.Clear;
  Loc := fPosition;
  HouseArea := fResource.HouseDat[fHouseType].BuildArea;

  for i := max(Loc.Y - 3, 1) to Loc.Y do
    for K := max(Loc.X - 2, 1) to min(Loc.X + 1, fTerrain.MapX) do
      if HouseArea[i - Loc.Y + 4, K - Loc.X + 3] <> 0 then
        Cells.AddEntry(KMPoint(K, i));
end;


function TKMHouse.GetRandomCellWithin: TKMPoint;
var
  Cells: TKMPointList;
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


function TKMHouse.GetBuildWoodDelivered: Byte;
begin
  case fBuildState of
    hbs_Stone,
    hbs_Done: Result := fResource.HouseDat[fHouseType].WoodCost;
    hbs_Wood: Result := fBuildSupplyWood+Ceil(fBuildingProgress/50);
    else      Result := 0;
  end;
end;


function TKMHouse.GetBuildStoneDelivered: Byte;
begin
  case fBuildState of
    hbs_Done:  Result := fResource.HouseDat[fHouseType].StoneCost;
    hbs_Wood:  Result := fBuildSupplyStone;
    hbs_Stone: Result := fBuildSupplyStone+Ceil(fBuildingProgress/50)-fResource.HouseDat[fHouseType].WoodCost;
    else       Result := 0;
  end;
end;


{Increase building progress of house. When it reaches some point Stoning replaces Wooding
 and then it's done and house should be finalized}
 {Keep track on stone/wood reserve here as well}
procedure TKMHouse.IncBuildingProgress;
begin
  if IsComplete then Exit;

  if (fBuildState=hbs_Wood) and (fBuildReserve = 0) then begin
    dec(fBuildSupplyWood);
    inc(fBuildReserve, 50);
  end;
  if (fBuildState=hbs_Stone) and (fBuildReserve = 0) then begin
    dec(fBuildSupplyStone);
    inc(fBuildReserve, 50);
  end;

  inc(fBuildingProgress, 5); //is how many effort was put into building nevermind applied damage
  dec(fBuildReserve, 5); //This is reserve we build from

  if (fBuildState=hbs_Wood) and (fBuildingProgress = fResource.HouseDat[fHouseType].WoodCost*50) then
    fBuildState := hbs_Stone;

  if (fBuildState=hbs_Stone) and (fBuildingProgress-fResource.HouseDat[fHouseType].WoodCost*50 = fResource.HouseDat[fHouseType].StoneCost*50) then
  begin
    fBuildState := hbs_Done;
    fPlayers[fOwner].Stats.HouseEnded(fHouseType);
    Activate(True);
    fScripting.ProcHouseBuilt(Self);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      fPlayers[fOwner].BuildList.RepairList.AddHouse(Self);
  end;
end;


function TKMHouse.MaxHealth: Word;
begin
  if fBuildState = hbs_NoGlyph then
    Result := 0
  else
    Result := fResource.HouseDat[fHouseType].MaxHealth;
end;


{Add damage to the house, positive number}
procedure TKMHouse.AddDamage(aFrom: TPlayerIndex; aAmount: Word; aIsEditor: Boolean = False);
begin
  if IsDestroyed then
    Exit;

  //(NoGlyph houses MaxHealth = 0 destroyed instantly)
  fDamage := Math.min(fDamage + aAmount, MaxHealth);
  if (fBuildState = hbs_Done) then
  begin
    if BuildingRepair then
      fPlayers[fOwner].BuildList.RepairList.AddHouse(Self);

    //Update fire if the house is complete
    UpdateDamage;
  end;

  //Properly release house assets
  if (GetHealth = 0) and not aIsEditor then
    DemolishHouse(aFrom);
end;


{Add repair to the house}
procedure TKMHouse.AddRepair(aAmount: Word = 5);
begin
  fDamage := EnsureRange(fDamage - aAmount, 0, High(Word));
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
  BuildingRepair := True;
  AddDamage(-1, 0); //Shortcut to refresh of damage
end;


{if house is damaged then remove repair from buildlist and free up workers}
procedure TKMHouse.DisableRepair;
begin
  BuildingRepair := false;
  AddRepair(0); //Shortcut to refresh of damage
end;


procedure TKMHouse.RepairToggle;
begin
  if BuildingRepair then
    DisableRepair
  else
    EnableRepair;
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
function TKMHouse.IsDamaged: Boolean;
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
function TKMHouse.CheckResIn(aResource: TResourceType): Word;
var i:integer;
begin
  Result := 0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResInput[i]) or (aResource = rt_All) then
    inc(Result, fResourceIn[i]);
end;


{How much resources house has in Output}
function TKMHouse.CheckResOut(aResource: TResourceType): Byte;
var i:integer;
begin
  Result := 0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResOutput[i]) or (aResource = rt_All) then
    inc(Result, fResourceOut[i]);
end;


{Check amount of placed order for given ID}
function TKMHouse.CheckResOrder(aID: Byte): Word;
const
  //Values are proportional to how many types of troops need this armament
  DefOrderCount: array [WEAPON_MIN..WEAPON_MAX] of Byte = (
    2, 2, 4, 4, 2, 2, 1, 1, 1, 1);
    //rt_Shield, rt_MetalShield, rt_Armor, rt_MetalArmor, rt_Axe,
    //rt_Sword, rt_Pike, rt_Hallebard, rt_Bow, rt_Arbalet
begin
  //AI always order production of everything. Could be changed later with a script command to only make certain things
  if (fPlayers[fOwner].PlayerType = pt_Computer)
  and (fResource.HouseDat[fHouseType].ResOutput[aID] <> rt_None) then
    Result := DefOrderCount[fResource.HouseDat[fHouseType].ResOutput[aID]]
  else
    Result := fResourceOrder[aID];
end;


function TKMHouse.PickOrder:byte;
var i, Res: byte; Ware: TResourceType;
begin
  Result := 0;
  for i:=0 to 3 do
  begin
    Res := ((fLastOrderProduced+i) mod 4)+1; //1..4
    Ware := fResource.HouseDat[fHouseType].ResOutput[Res];
    if (CheckResOrder(Res) > 0) //Player has ordered some of this
    and (CheckResOut(Ware) < MAX_RES_IN_HOUSE) //Output of this is not full
    //Check we have wares to produce this weapon. If both are the same type check > 1 not > 0
    and ((WarfareCosts[Ware,1] <> WarfareCosts[Ware,2]) or (CheckResIn(WarfareCosts[Ware,1]) > 1))
    and ((WarfareCosts[Ware,1] = rt_None) or (CheckResIn(WarfareCosts[Ware,1]) > 0))
    and ((WarfareCosts[Ware,2] = rt_None) or (CheckResIn(WarfareCosts[Ware,2]) > 0)) then
    begin
      Result := Res;
      exit;
    end;
  end;
end;


procedure TKMHouse.SetLastOrderProduced(aResource:TResourceType);
var i: byte;
begin
  if aResource <> rt_None then
    for i:=1 to 4 do
      if fResource.HouseDat[HouseType].ResOutput[i] = aResource then
        fLastOrderProduced := i;
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


function TKMHouse.GetMaxInRes: Word;
begin
  if fHouseType in [ht_Store, ht_Barracks, ht_Marketplace] then
    Result := High(Word)
  else
    Result := MAX_RES_IN_HOUSE; //All other houses can only stock 5 for now
end;


//Maybe it's better to rule out In/Out? No, it is required to separate what can be taken out of the house and what not.
//But.. if we add "Evacuate" button to all house the separation becomes artificial..
procedure TKMHouse.ResAddToIn(aResource:TResourceType; aCount:word=1; aFromScript:boolean=false);
var I,OrdersRemoved: Integer;
begin
  Assert(aResource <> rt_None);

  for I := 1 to 4 do
    if aResource = fResource.HouseDat[fHouseType].ResInput[I] then
    begin
      //Don't allow the script to overfill houses
      if aFromScript then aCount := Min(aCount, GetMaxInRes - fResourceIn[I]);
      Inc(fResourceIn[I], aCount);
      if aFromScript then
      begin
        Inc(fResourceDeliveryCount[I], aCount);
        OrdersRemoved := fPlayers[fOwner].Deliveries.Queue.TryRemoveDemand(Self, aResource, aCount);
        Dec(fResourceDeliveryCount[I], OrdersRemoved);
      end;
    end;
end;


procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var I: Integer;
begin
  if aResource = rt_None then exit;
  for I := 1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResOutput[I] then
    begin
      inc(fResourceOut[I], aCount);
      fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, aResource, aCount);
    end;
end;


{Add resources to building process}
procedure TKMHouse.ResAddToBuild(aResource:TResourceType);
begin
  case aResource of
    rt_Wood: Inc(fBuildSupplyWood);
    rt_Stone: Inc(fBuildSupplyStone);
  else raise ELocError.Create('WIP house is not supposed to recieve '+fResource.Resources[aResource].Title+', right?', fPosition);
  end;
end;


function TKMHouse.ResCanAddToIn(aRes: TResourceType): Boolean;
var I: Integer;
begin
  Result := False;
  for I := 1 to 4 do
  if aRes = fResource.HouseDat[fHouseType].ResInput[I] then
    Result := True;
end;


//Take resource from Input and order more of that kind if DistributionRatios allow
procedure TKMHouse.ResTakeFromIn(aResource:TResourceType; aCount:byte=1);
var I,K: Integer;
begin
  Assert(aResource <> rt_None);

  for I := 1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResInput[I] then
  begin
    Assert(fResourceIn[I] >= aCount, 'fResourceIn[i] < 0');
    Dec(fResourceIn[I], aCount);
    fResourceDeliveryCount[I] := Max(fResourceDeliveryCount[I] - aCount, 0);
    //Only request a new resource if it is allowed by the distribution of wares for our parent player
    for K := 1 to aCount do
      if fResourceDeliveryCount[I] < GetResDistribution(I) then
      begin
        fPlayers[fOwner].Deliveries.Queue.AddDemand(Self, nil, aResource, 1, dt_Once, di_Norm);
        Inc(fResourceDeliveryCount[I]);
      end;
    Exit;
  end;
end;


procedure TKMHouse.ResTakeFromOut(aResource:TResourceType; const aCount: Word=1);
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
  Result := fPlayers[fOwner].Stats.Ratio[fResource.HouseDat[fHouseType].ResInput[aID],fHouseType];
end;


procedure TKMHouse.MakeSound;
var
  Work: THouseActionType;
  Step: Byte;
begin
  if SKIP_SOUND then Exit;

  if fCurrentAction = nil then exit; //no action means no sound ;)

  if ha_Work1 in fCurrentAction.SubAction then Work := ha_Work1 else
  if ha_Work2 in fCurrentAction.SubAction then Work := ha_Work2 else
  if ha_Work3 in fCurrentAction.SubAction then Work := ha_Work3 else
  if ha_Work4 in fCurrentAction.SubAction then Work := ha_Work4 else
  if ha_Work5 in fCurrentAction.SubAction then Work := ha_Work5 else
    Exit; //No work is going on

  Step := fResource.HouseDat[fHouseType].Anim[Work].Count;
  if Step = 0 then Exit;

  Step := WorkAnimStep mod Step;

  //Do not play sounds if house is invisible to MyPlayer
  //This check is slower so we do it after other Exit checks
  if MyPlayer.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y, true) < 255 then exit;

  case fHouseType of //Various buildings and HouseActions producing sounds
    ht_School:        if (Work = ha_Work5)and(Step = 28) then fSoundLib.Play(sfx_SchoolDing, fPosition); //Ding as the clock strikes 12
    ht_Mill:          if (Work = ha_Work2)and(Step = 0) then fSoundLib.Play(sfx_mill, fPosition);
    ht_CoalMine:      if (Work = ha_Work1)and(Step = 5) then fSoundLib.Play(sfx_coalDown, fPosition)
                      else if (Work = ha_Work1)and(Step = 24) then fSoundLib.Play(sfx_CoalMineThud, fPosition,true,0.8)
                      else if (Work = ha_Work2)and(Step = 7) then fSoundLib.Play(sfx_mine, fPosition)
                      else if (Work = ha_Work5)and(Step = 1) then fSoundLib.Play(sfx_coalDown, fPosition);
    ht_IronMine:      if (Work = ha_Work2)and(Step = 7) then fSoundLib.Play(sfx_mine, fPosition);
    ht_GoldMine:      if (Work = ha_Work2)and(Step = 5) then fSoundLib.Play(sfx_mine, fPosition);
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
  SaveStream.Write(fLastOrderProduced);
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
    fPlayers.RevealForTeam(fOwner, fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);
end;


//Request more resources (if distribution of wares has changed)
//todo: Situation: I have timber set to 5 for the weapons workshop, and no timber in my village.
//      I change timber to 0 for the weapons workshop. My woodcutter starts again and 5 timber is still
//      taken to the weapons workshop because the request doesn't get canceled.
//      Maybe it's possible to cancel the current requests if no serf has taken them yet?
procedure TKMHouse.UpdateResRequest;
var
  I: Byte;
  Count, Excess: ShortInt;
begin
  for I := 1 to 4 do
    if not (fResource.HouseDat[fHouseType].ResInput[I] in [rt_All, rt_Warfare, rt_None]) then
    begin

      //Not enough resources ordered, add new demand
      if fResourceDeliveryCount[I] < GetResDistribution(I) then
      begin
        Count := GetResDistribution(I)-fResourceDeliveryCount[I];
        fPlayers[fOwner].Deliveries.Queue.AddDemand(
          Self, nil, fResource.HouseDat[fHouseType].ResInput[I], Count, dt_Once, di_Norm);

        inc(fResourceDeliveryCount[I], Count);
      end;

      //Too many resources ordered, attempt to remove demand if nobody has taken it yet
      if fResourceDeliveryCount[I] > GetResDistribution(I) then
      begin
        Excess := fResourceDeliveryCount[I]-GetResDistribution(I);
        Count := fPlayers[fOwner].Deliveries.Queue.TryRemoveDemand(
                   Self, fResource.HouseDat[fHouseType].ResInput[I], Excess);

        dec(fResourceDeliveryCount[I], Count); //Only reduce it by the number that were actually removed
      end;

    end;
end;


procedure TKMHouse.UpdateState;
begin
  if fBuildState<>hbs_Done then exit; //Don't update unbuilt houses
  //Toggle repair for AI players (allows AI to turn repair on and off based on requirements)
  if (not fBuildingRepair) and (fPlayers[fOwner].PlayerType = pt_Computer) and (fPlayers[fOwner].AI.HouseAutoRepair) then
    EnableRepair;
  if fBuildingRepair and (fPlayers[fOwner].PlayerType = pt_Computer) and (not fPlayers[fOwner].AI.HouseAutoRepair) then
    DisableRepair;

  //Show unoccupied message if needed and house belongs to human player and can have owner at all and not a barracks
  if (not fHasOwner) and (fResource.HouseDat[fHouseType].OwnerType <> ut_None) and (fHouseType <> ht_Barracks) then
  begin
    dec(fTimeSinceUnoccupiedReminder);
    if fTimeSinceUnoccupiedReminder = 0 then
    begin
      if (fOwner = MyPlayer.PlayerIndex) and not fGame.IsReplay then
        fGame.ShowMessage(mkHouse, fTextLibrary[TX_MSG_HOUSE_UNOCCUPIED], GetEntrance);
      fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
    end;
  end
  else
    fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES;

  if not fIsDestroyed then MakeSound; //Make some sound/noise along the work

  IncAnimStep;
end;


procedure TKMHouse.Paint;
var
  H: TKMHouseDatClass;
  Progress: Single;
begin
  H := fResource.HouseDat[fHouseType];
  case fBuildState of
    hbs_NoGlyph:; //Nothing
    hbs_Wood:   begin
                  Progress := fBuildingProgress / 50 / H.WoodCost;
                  fRenderPool.AddHouseWood(fHouseType, fPosition, Progress); //0...1 range
                  fRenderPool.AddHouseBuildSupply(fHouseType, fPosition, fBuildSupplyWood, fBuildSupplyStone);
                end;
    hbs_Stone:  begin
                  Progress := (fBuildingProgress / 50 - H.WoodCost) / H.StoneCost;
                  fRenderPool.AddHouseStone(fHouseType, fPosition, Progress); //0...1 range
                  fRenderPool.AddHouseBuildSupply(fHouseType, fPosition, fBuildSupplyWood, fBuildSupplyStone);
                end;
    else        begin
                  if HOUSE_BUILDING_STEP <> 0 then
                    if HOUSE_BUILDING_STEP < 0.5 then
                      fRenderPool.AddHouseWood(fHouseType, fPosition, HOUSE_BUILDING_STEP * 2)
                    else
                      fRenderPool.AddHouseStone(fHouseType, fPosition, (HOUSE_BUILDING_STEP - 0.5) * 2)
                  else
                  begin
                    fRenderPool.AddHouseStone(fHouseType, fPosition, 1);
                    fRenderPool.AddHouseSupply(fHouseType, fPosition, fResourceIn, fResourceOut);
                    if fCurrentAction <> nil then
                      fRenderPool.AddHouseWork(fHouseType, fPosition, fCurrentAction.SubAction, WorkAnimStep, fPlayers[fOwner].FlagColor);
                  end;
                end;
  end;

  if SHOW_POINTER_DOTS then
    fRenderAux.UnitPointers(fPosition.X + 0.5, fPosition.Y + 1, fPointerCount);
end;


{TKMHouseSwineStable}
constructor TKMHouseSwineStable.Load(LoadStream:TKMemoryStream);
begin
  inherited;
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
var I: Byte;
begin
  inherited;
  if MyPlayer.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y, true) < 255 then Exit;

  for I := 0 to 4 do
  if BeastAge[I+1] > 0 then
  if (FlagAnimStep + 20*I) mod 100 = 0 then
  begin
    if fHouseType = ht_Stables then
      fSoundLib.Play(TSoundFX(byte(sfx_Horse1) + Random(4)), fPosition); //sfx_Horse1..sfx_Horse4
    if fHouseType = ht_Swine   then
      fSoundLib.Play(TSoundFX(byte(sfx_Pig1)   + Random(4)), fPosition); //sfx_Pig1..sfx_Pig4
  end;
end;


procedure TKMHouseSwineStable.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
end;


procedure TKMHouseSwineStable.Paint;
var i:integer;
begin
  inherited;
  //We render beasts on top of the HouseWork (which is mostly flames in this case), because otherwise
  //Swinefarm looks okay, but Stables are totaly wrong - flames are right on horses backs!
  if fBuildState=hbs_Done then
    for i:=1 to 5 do
      if BeastAge[i]>0 then
        fRenderPool.AddHouseStableBeasts(fHouseType, fPosition, i, min(BeastAge[i],3), WorkAnimStep);

  //But Animal Breeders should be on top of beasts
  if fCurrentAction<>nil then
    fRenderPool.AddHouseWork(fHouseType, fPosition,
                            fCurrentAction.SubAction * [ha_Work1, ha_Work2, ha_Work3, ha_Work4, ha_Work5],
                            WorkAnimStep, fPlayers[fOwner].FlagColor);
end;


{ TKMHouseInn }
constructor TKMHouseInn.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
var i:integer;
begin
  inherited;

  for i:=Low(Eater) to High(Eater) do
    Eater[i].UnitType := ut_None;
end;


constructor TKMHouseInn.Load(LoadStream:TKMemoryStream);
begin
  inherited;
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
    Eater[i].FoodKind := rt_None;
    Eater[i].EatStep  := FlagAnimStep;
    Result := i;
    exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aID: byte; aFoodKind: TResourceType);
begin
  if aID=0 then exit;
  Assert(aFoodKind in [rt_Wine, rt_Bread, rt_Sausages, rt_Fish], 'Wrong food kind');
  Eater[aID].FoodKind := aFoodKind; //Order is Wine-Bread-Sausages-Fish
  Eater[aID].EatStep  := FlagAnimStep; //FlagAnimStep-Eater[i].EatStep = 0
end;


procedure TKMHouseInn.EatersGoesOut(aID: Byte);
begin
  if aID <> 0 then
    Eater[aID].UnitType := ut_None;
end;


function TKMHouseInn.HasFood:boolean;
begin
  Result := (CheckResIn(rt_Sausages)+CheckResIn(rt_Bread)+CheckResIn(rt_Wine)+CheckResIn(rt_Fish)>0);
end;


function TKMHouseInn.HasSpace:boolean;
var
  i: integer;
begin
  Result := false;
  for i:=Low(Eater) to High(Eater) do
    Result := Result or (Eater[i].UnitType = ut_None);
end;


procedure TKMHouseInn.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(Eater, SizeOf(Eater));
end;


procedure TKMHouseInn.Paint;
  //Chose eater animation direction (1357 face south, 2468 face north)
  function AnimDir(i: Integer): TKMDirection;
  begin
    case Eater[i].FoodKind of
      rt_Wine:    Result  := TKMDirection(1 * 2 - 1 + ((i-1) div 3));
      rt_Bread:   Result  := TKMDirection(2 * 2 - 1 + ((i-1) div 3));
      rt_Sausages:Result  := TKMDirection(3 * 2 - 1 + ((i-1) div 3));
      rt_Fish:    Result  := TKMDirection(4 * 2 - 1 + ((i-1) div 3));
    else Result := dir_NA;
    end;
  end;
const
  OffX: array [0..2] of single = (-0.5, 0.0, 0.5);
  OffY: array [0..2] of single = (-0.05, 0, 0.05);
var
  i: Integer;
  AnimStep: Cardinal;
begin
  inherited;
  if fBuildState <> hbs_Done then exit;

  for i := Low(Eater) to High(Eater) do
  begin
    if (Eater[i].UnitType = ut_None) or (Eater[i].FoodKind = rt_None) then Continue;

    AnimStep := FlagAnimStep - Eater[i].EatStep; //Delta is our AnimStep

    fRenderPool.AddHouseEater(fPosition, Eater[i].UnitType, ua_Eat,
                              AnimDir(i), AnimStep,
                              OffX[(i-1) mod 3], OffY[(i-1) mod 3],
                              fPlayers[fOwner].FlagColor);
  end;
end;


{ TKMHouseSchool }
constructor TKMHouseSchool.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
var I: Integer;
begin
  inherited;

  for I := 0 to High(Queue) do
    Queue[I] := ut_None;
end;


constructor TKMHouseSchool.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(UnitWIP, 4);
  LoadStream.Read(fHideOneGold);
  LoadStream.Read(fTrainProgress);
  LoadStream.Read(Queue, SizeOf(Queue));
end;


procedure TKMHouseSchool.SyncLoad;
begin
  UnitWIP := fPlayers.GetUnitByID(Cardinal(UnitWIP));
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSchool.DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False);
var
  I: Integer;
begin
  for I := 1 to High(Queue) do
    Queue[I] := ut_None;
  RemUnitFromQueue(0); //Remove WIP unit

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSchool.ResAddToIn(aResource: TResourceType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  inherited;

  if UnitWIP = nil then
    StartTrainingUnit;
end;


//Add units to training queue
//aCount allows to add several units at once (but not more than Schools queue can fit)
//Returns the number of units successfully added to the queue
function TKMHouseSchool.AddUnitToQueue(aUnit: TUnitType; aCount: Byte): Byte;
var I, K: Integer;
begin
  Result := 0;
  for K := 1 to aCount do
  for I := 1 to High(Queue) do
  if Queue[I] = ut_None then
  begin
    Inc(Result);
    Queue[I] := aUnit;
    if I = 1 then
      StartTrainingUnit; //If thats the first unit then start training it
    Break;
  end;
end;


//DoCancelTraining and remove untrained unit
procedure TKMHouseSchool.RemUnitFromQueue(aID: Byte);
var I: Integer;
begin
  if Queue[aID] = ut_None then Exit; //Ignore clicks on empty queue items

  if aID = 0 then
  begin
    SetState(hst_Idle);
    if UnitWIP <> nil then
    begin //Make sure unit started training
      TKMUnit(UnitWIP).CloseUnit(False); //Don't remove tile usage, we are inside the school
      fHideOneGold := False;
    end;
    UnitWIP := nil;
    Queue[0] := ut_None; //Removed the in training unit
    StartTrainingUnit; //Start on the next unit in the queue
  end
  else
  begin
    for I := aID to High(Queue) - 1 do
      Queue[I] := Queue[I+1]; //Shift by one
    Queue[High(Queue)] := ut_None; //Set the last one empty
  end;
end;


procedure TKMHouseSchool.StartTrainingUnit;
var I: Integer;
begin
  if Queue[0] <> ut_None then exit; //If there's currently no unit in training
  if Queue[1] = ut_None then exit; //If there is a unit waiting to be trained
  if CheckResIn(rt_Gold) = 0 then exit; //There must be enough gold to perform training

  fHideOneGold := true;
  for I := 0 to High(Queue) - 1 do
    Queue[I] := Queue[I+1]; //Shift by one
  Queue[High(Queue)] := ut_None; //Set the last one empty

  //Create the Unit
  UnitWIP := fPlayers[fOwner].TrainUnit(Queue[0], GetEntrance);
  TKMUnit(UnitWIP).TrainInHouse(Self); //Let the unit start the training task

  WorkAnimStep := 0;
end;


//Unit reports back to School that it was trained
procedure TKMHouseSchool.UnitTrainingComplete(aUnit: Pointer);
begin
  Assert(aUnit = UnitWIP, 'Should be called only by Unit itself when it''s trained');

  UnitWIP := nil;
  Queue[0] := ut_None; //Clear the unit in training
  ResTakeFromIn(rt_Gold); //Do the goldtaking
  fPlayers[fOwner].Stats.GoodConsumed(rt_Gold);
  fHideOneGold := False;
  fTrainProgress := 0;

  //Attempt to start training next unit in queue
  StartTrainingUnit;
end;


//Return training progress of a unit in 0.0 - 1.0 range
function TKMHouseSchool.GetTrainingProgress: Single;
begin
  if UnitWIP = nil then
    Result := 0
  else
    Result := (
              Byte(ha_Work2 in fCurrentAction.SubAction) * 30 +
              Byte(ha_Work3 in fCurrentAction.SubAction) * 60 +
              Byte(ha_Work4 in fCurrentAction.SubAction) * 90 +
              Byte(ha_Work5 in fCurrentAction.SubAction) * 120 +
              Byte(fCurrentAction.State = hst_Work) * WorkAnimStep
              ) / 150;
end;


function TKMHouseSchool.QueueIsEmpty: Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to High(Queue) do
    Result := Result and (Queue[I] = ut_None);
end;


procedure TKMHouseSchool.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if TKMUnit(UnitWIP) <> nil then
    SaveStream.Write(TKMUnit(UnitWIP).ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fHideOneGold);
  SaveStream.Write(fTrainProgress);
  SaveStream.Write(Queue, SizeOf(Queue));
end;


{ TKMHouseStore }
procedure TKMHouseStore.Activate(aWasBuilt:boolean);
var
  FirstStore: TKMHouseStore;
  RT: TResourceType;
begin
  inherited;
  //A new storehouse should inherrit the accept properies of the first storehouse of that player,
  //which stops a sudden flow of unwanted resources to it as soon as it is create.
  FirstStore := TKMHouseStore(fPlayers[fOwner].FindHouse(ht_Store, 1));
  if (FirstStore <> nil) and not FirstStore.IsDestroyed then
    for RT := WARE_MIN to WARE_MAX do
      NotAcceptFlag[RT] := FirstStore.NotAcceptFlag[RT];
end;


constructor TKMHouseStore.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


procedure TKMHouseStore.ResAddToIn(aResource: TResourceType; aCount: Word = 1; aFromScript: Boolean = False);
var R: TResourceType;
begin
  case aResource of
    rt_All:     for R := Low(ResourceCount) to High(ResourceCount) do begin
                  ResourceCount[R] := EnsureRange(ResourceCount[R]+aCount, 0, High(Word));
                  fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, R, aCount);
                end;
    WARE_MIN..
    WARE_MAX:   begin
                  ResourceCount[aResource]:=EnsureRange(ResourceCount[aResource]+aCount, 0, High(Word));
                  fPlayers[fOwner].Deliveries.Queue.AddOffer(Self,aResource,aCount);
                end;
    else        raise ELocError.Create('Cant''t add '+fResource.Resources[aResource].Title, GetPosition);
  end;
end;


function TKMHouseStore.ResCanAddToIn(aRes: TResourceType): Boolean;
begin
  Result := (aRes in [WARE_MIN..WARE_MAX]);
end;


function TKMHouseStore.CheckResIn(aResource: TResourceType): Word;
begin
  if aResource in [WARE_MIN..WARE_MAX] then
    Result := ResourceCount[aResource]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


procedure TKMHouseStore.DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False);
var
  R: TResourceType;
begin
  for R := WARE_MIN to WARE_MAX do
    fPlayers[fOwner].Stats.GoodConsumed(R, ResourceCount[R]);

  inherited;
end;


procedure TKMHouseStore.ResTakeFromOut(aResource:TResourceType; const aCount: Word=1);
begin
  Assert(aCount <= ResourceCount[aResource]);

  dec(ResourceCount[aResource], aCount);
end;


procedure TKMHouseStore.ToggleAcceptFlag(aRes: TResourceType);
var
  R: TResourceType;
  ApplyCheat: Boolean;
begin
  Assert(aRes in [WARE_MIN .. WARE_MAX]); //Dunno why thats happening sometimes..

  //We need to skip cheats in MP replays too, not just MP games, so don't use fGame.IsMultiplayer
  if CHEATS_ENABLED and (MULTIPLAYER_CHEATS or not (fGame.GameMode in [gmMulti, gmReplayMulti])) then
  begin
    ApplyCheat := True;

    //Check the cheat pattern
    for R := Low(ResourceCount) to High(ResourceCount) do
      ApplyCheat := ApplyCheat and (NotAcceptFlag[R] = boolean(CheatStorePattern[R]));

    if ApplyCheat then
    case aRes of
      rt_Arbalet: begin
                    ResAddToIn(rt_All, 10);
                    fPlayers[fOwner].Stats.GoodProduced(rt_All, 10);
                    Exit;
                  end;
      rt_Horse:   if not fGame.IsMultiplayer then
                  begin
                    //Game results cheats should not be used in MP even in debug
                    //MP does Win/Defeat differently (without Hold)
                    fGame.RequestGameHold(gr_Win);
                    Exit;
                  end;
      rt_Fish:    if not fGame.IsMultiplayer then
                  begin
                    //Game results cheats should not be used in MP even in debug
                    //MP does Win/Defeat differently (without Hold)
                    fGame.RequestGameHold(gr_Defeat);
                    Exit;
                  end;
    end;
  end;

  NotAcceptFlag[aRes] := not NotAcceptFlag[aRes];
end;


procedure TKMHouseStore.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


{ TKMHouseBarracks }
constructor TKMHouseBarracks.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
begin
  inherited;
  RecruitsList := TList.Create;
end;


constructor TKMHouseBarracks.Load(LoadStream: TKMemoryStream);
var
  I,aCount: Integer;
  U: TKMUnit;
begin
  inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  RecruitsList := TList.Create;
  LoadStream.Read(aCount);
  for I := 0 to aCount - 1 do
  begin
    LoadStream.Read(U, 4); //subst on syncload
    RecruitsList.Add(U);
  end;
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


procedure TKMHouseBarracks.SyncLoad;
var I: Integer;
begin
  for I := 0 to RecruitsList.Count - 1 do
    RecruitsList.Items[I] := fPlayers.GetUnitByID(Cardinal(RecruitsList.Items[I]));
end;


destructor TKMHouseBarracks.Destroy;
begin
  RecruitsList.Free;
  inherited;
end;


procedure TKMHouseBarracks.Activate(aWasBuilt: Boolean);
var
  FirstBarracks: TKMHouseBarracks;
  RT: TResourceType;
begin
  inherited;
  //A new Barracks should inherit the accept properies of the first Barracksof that player,
  //which stops a sudden flow of unwanted resources to it as soon as it is create.
  FirstBarracks := TKMHouseBarracks(fPlayers[fOwner].FindHouse(ht_Barracks, 1));
  if (FirstBarracks <> nil) and not FirstBarracks.IsDestroyed then
    for RT := WARFARE_MIN to WARFARE_MAX do
      NotAcceptFlag[RT] := FirstBarracks.NotAcceptFlag[RT];
end;


procedure TKMHouseBarracks.DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False);
var
  R: TResourceType;
begin
  //Recruits are no longer under our control so we forget about them (UpdateVisibility will sort it out)
  //Otherwise it can cause crashes while saving under the right conditions when a recruit is then killed.
  RecruitsList.Clear;

  for R := WARFARE_MIN to WARFARE_MAX do
    fPlayers[fOwner].Stats.GoodConsumed(R, ResourceCount[R]);

  inherited;
end;


procedure TKMHouseBarracks.ResAddToIn(aResource:TResourceType; aCount:word=1; aFromScript:boolean=false);
begin
  Assert(aResource in [WARFARE_MIN..WARFARE_MAX], 'Invalid resource added to barracks');

  ResourceCount[aResource] := EnsureRange(ResourceCount[aResource]+aCount, 0, High(Word));
  fPlayers[fOwner].Deliveries.Queue.AddOffer(Self,aResource,aCount);
end;


function TKMHouseBarracks.ResCanAddToIn(aRes: TResourceType): Boolean;
begin
  Result := (aRes in [WARFARE_MIN..WARFARE_MAX]);
end;


function TKMHouseBarracks.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [WARFARE_MIN..WARFARE_MAX] then
    Result := ResourceCount[aResource]
  else
    Result := 0; //Including Wood/stone in building stage
end;


procedure TKMHouseBarracks.ResTakeFromOut(aResource:TResourceType; const aCount: Word=1);
begin
  Assert(aCount <= ResourceCount[aResource]);
  dec(ResourceCount[aResource], aCount);
end;


function TKMHouseBarracks.CanTakeResOut(aResource: TResourceType): Boolean;
begin
  Assert(aResource in [WARFARE_MIN .. WARFARE_MAX]);
  Result := (ResourceCount[aResource] > 0);
end;


procedure TKMHouseBarracks.ToggleAcceptFlag(aRes: TResourceType);
begin
  Assert(aRes in [WARFARE_MIN .. WARFARE_MAX]);

  NotAcceptFlag[aRes] := not NotAcceptFlag[aRes];
end;


function TKMHouseBarracks.CanEquip(aUnitType: TUnitType): Boolean;
var I: Integer;
begin
  Result := RecruitsList.Count > 0; //Can't equip anything without recruits

  for I := 1 to 4 do
  if TroopCost[aUnitType, I] <> rt_None then //Can't equip if we don't have a required resource
    Result := Result and (ResourceCount[TroopCost[aUnitType, I]] > 0);
end;


//Equip a new soldier and make him walk out of the house
//Return the number of units successfully equipped
function TKMHouseBarracks.Equip(aUnitType: TUnitType; aCount: Byte): Byte;
var
  I, K: Integer;
  Soldier: TKMUnitWarrior;
begin
  Result := 0;
  Assert(aUnitType in [WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX]);

  for K := 0 to aCount - 1 do
  begin
    //Make sure we have enough resources to equip a unit
    if not CanEquip(aUnitType) then Exit;

    //Take resources
    for I := 1 to 4 do
    if TroopCost[aUnitType, I] <> rt_None then
    begin
      Dec(ResourceCount[TroopCost[aUnitType, I]]);
      fPlayers[fOwner].Stats.GoodConsumed(TroopCost[aUnitType, I]);
      fPlayers[fOwner].Deliveries.Queue.RemOffer(Self, TroopCost[aUnitType, I], 1);
    end;

    //Special way to kill the Recruit because it is in a house
    TKMUnitRecruit(RecruitsList.Items[0]).DestroyInBarracks;
    RecruitsList.Delete(0); //Delete first recruit in the list

    //Make new unit
    Soldier := TKMUnitWarrior(fPlayers[fOwner].TrainUnit(aUnitType, GetEntrance));
    Soldier.SetInHouse(Self); //Put him in the barracks, so if it is destroyed while he is inside he is placed somewhere
    Soldier.Visible := False; //Make him invisible as he is inside the barracks
    Soldier.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
    //Soldier.OrderLoc := KMPointBelow(GetEntrance); //Position in front of the barracks facing north
    Soldier.SetActionGoIn(ua_Walk, gd_GoOutside, Self);
    Inc(Result);
  end;
end;


procedure TKMHouseBarracks.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(RecruitsList.Count);
  for I := 0 to RecruitsList.Count - 1 do
    SaveStream.Write(TKMUnit(RecruitsList.Items[I]).ID); //Store ID
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


{ TKMHouseWoodcutters }
constructor TKMHouseWoodcutters.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
begin
  inherited;
  WoodcutterMode := wcm_ChopAndPlant;
end;


constructor TKMHouseWoodcutters.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fWoodcutterMode, SizeOf(fWoodcutterMode));
end;


procedure TKMHouseWoodcutters.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fWoodcutterMode, SizeOf(fWoodcutterMode));
end;


procedure TKMHouseWoodcutters.SetWoodcutterMode(aWoodcutterMode: TWoodcutterMode);
begin
  fWoodcutterMode := aWoodcutterMode;
  //If we're allowed to plant again, we should reshow the depleted message if we are changed to cut and run out of trees
  if fWoodcutterMode = wcm_ChopAndPlant then
    ResourceDepletedMsgIssued := False;
end;


{ THouseAction }
constructor THouseAction.Create(aHouse:TKMHouse; aHouseState: THouseState);
begin
  inherited Create;
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
var I, K: Integer;
begin
  inherited;

  if SHOW_ATTACK_RADIUS then
    for I := -Round(RANGE_WATCHTOWER_MAX)-1 to Round(RANGE_WATCHTOWER_MAX) do
    for K := -Round(RANGE_WATCHTOWER_MAX)-1 to Round(RANGE_WATCHTOWER_MAX) do
    if InRange(GetLength(I, K), RANGE_WATCHTOWER_MIN, RANGE_WATCHTOWER_MAX) then
    if fTerrain.TileInMapCoords(GetPosition.X+K, GetPosition.Y+I) then
      fRenderAux.Quad(GetPosition.X+K, GetPosition.Y+I, $40FFFFFF);
end;


{ TKMHousesCollection }
constructor TKMHousesCollection.Create;
begin
  inherited;
  fHouses := TKMList.Create;
end;


destructor TKMHousesCollection.Destroy;
begin
  fHouses.Free;
  inherited;
end;


function TKMHousesCollection.AddToCollection(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; aHBS: THouseBuildState): TKMHouse;
var ID: Cardinal;
begin
  ID := fGame.GetNewID;

  case aHouseType of
    ht_Swine,
    ht_Stables:     Result := TKMHouseSwineStable.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Inn:         Result := TKMHouseInn.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Marketplace: Result := TKMHouseMarket.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_School:      Result := TKMHouseSchool.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Barracks:    Result := TKMHouseBarracks.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Store:       Result := TKMHouseStore.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_WatchTower:  Result := TKMHouseTower.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Woodcutters: Result := TKMHouseWoodcutters.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    else            Result := TKMHouse.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
  end;

  if Result <> nil then
    fHouses.Add(Result);
end;


function TKMHousesCollection.GetCount: Integer;
begin
  Result := fHouses.Count;
end;


function TKMHousesCollection.GetHouse(aIndex: Integer): TKMHouse;
begin
  Result := fHouses[aIndex];
end;


function TKMHousesCollection.AddHouse(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; RelativeEntrance: Boolean):TKMHouse;
begin
  if RelativeEntrance then
    Result := AddToCollection(aHouseType, PosX - fResource.HouseDat[aHouseType].EntranceOffsetX, PosY, aOwner, hbs_Done)
  else
    Result := AddToCollection(aHouseType, PosX, PosY, aOwner, hbs_Done);
end;


{Add a plan for house}
function TKMHousesCollection.AddHouseWIP(aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex): TKMHouse;
begin
  Result := AddToCollection(aHouseType, PosX, PosY, aOwner, hbs_NoGlyph);
end;


procedure TKMHousesCollection.OwnerUpdate(aOwner:TPlayerIndex);
var i:integer;
begin
  for i:=0 to Count-1 do
    Houses[i].fOwner := aOwner;
end;


function TKMHousesCollection.HitTest(X, Y: Integer): TKMHouse;
var I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if Houses[I].HitTest(X, Y) and (not Houses[I].IsDestroyed) then
    begin
      Result := Houses[I];
      Break;
    end;
end;


function TKMHousesCollection.GetHouseByID(aID: Integer): TKMHouse;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if aID = Houses[I].ID then
    begin
      Result := Houses[I];
      exit;
    end;
end;


//Should find closest house to Loc
function TKMHousesCollection.FindEmptyHouse(aUnitType: TUnitType; Loc: TKMPoint): TKMHouse;
var
  I: Integer;
  Dist, BestBid: Single;
begin
  Result := nil;
  BestBid := MaxSingle;

  for I := 0 to Count - 1 do
    if (fResource.HouseDat[Houses[i].fHouseType].OwnerType = aUnitType) and //If Unit can work in here
       (not Houses[i].fHasOwner) and                              //If there's yet no owner
       (not Houses[i].IsDestroyed) and
       (Houses[i].IsComplete) then                               //If house is built
    begin
      //Recruits should not go to a barracks with ware delivery switched off
      if (Houses[i].HouseType = ht_Barracks) and (not Houses[i].WareDelivery) then Continue;
      if not fTerrain.Route_CanBeMade(Loc, KMPointBelow(Houses[i].GetEntrance), canWalk, 0) then Continue;

      Dist := KMLengthSqr(Loc, Houses[i].GetPosition);

      //Always prefer Towers to Barracks by making Barracks Bid much less attractive
      //In case of multiple barracks, prefer the closer one (players should make multiple schools or use WareDelivery to control it)
      if Houses[i].HouseType = ht_Barracks then
        Dist := Dist * 1000;

      if Dist < BestBid then
      begin
        BestBid := Dist;
        Result := Houses[i];
      end;

    end;

  if (Result <> nil) and (Result.fHouseType <> ht_Barracks) then
    Result.fHasOwner := True; //Become owner except Barracks;
end;


//Find closest house to given position
//or
//Find house by index (1st, 2nd)
function TKMHousesCollection.FindHouse(aType:THouseType; X,Y:word; const aIndex:byte=1; aOnlyCompleted:boolean=true): TKMHouse;
var
  I,ID: Integer;
  UsePosition: Boolean;
  BestMatch,Dist: Single;
begin
  Result := nil;
  ID := 0;
  BestMatch := MaxSingle; //Any distance will be closer than that
  UsePosition := X*Y <> 0; //Calculate this once to save computing lots of multiplications
  Assert((not UsePosition) or (aIndex = 1), 'Can''t find house basing both on Position and Index');

  for I := 0 to Count - 1 do
  if ((Houses[I].fHouseType = aType) or (aType = ht_Any))
  and (Houses[I].IsComplete or not aOnlyCompleted)
  and not Houses[I].fIsDestroyed then
  begin
    Inc(ID);
    if UsePosition then
    begin
      Dist := KMLengthSqr(Houses[I].GetPosition,KMPoint(X,Y));
      if BestMatch = -1 then BestMatch := Dist; //Initialize for first use
      if Dist < BestMatch then
      begin
        BestMatch := Dist;
        Result := Houses[I];
      end;
    end
    else
      //Take the N-th result
      if aIndex = ID then
      begin
        Result := Houses[I];
        Exit;
      end;
  end;
end;


procedure TKMHousesCollection.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('Houses');

  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
  begin
    //We save house type to know which house class to load
    SaveStream.Write(Houses[I].HouseType, SizeOf(Houses[I].HouseType));
    Houses[I].Save(SaveStream);
  end;
end;


procedure TKMHousesCollection.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
  HouseType: THouseType;
  T: TKMHouse;
begin
  LoadStream.ReadAssert('Houses');

  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(HouseType, SizeOf(HouseType));
    case HouseType of
      ht_Swine,
      ht_Stables:     T := TKMHouseSwineStable.Load(LoadStream);
      ht_Inn:         T := TKMHouseInn.Load(LoadStream);
      ht_Marketplace: T := TKMHouseMarket.Load(LoadStream);
      ht_School:      T := TKMHouseSchool.Load(LoadStream);
      ht_Barracks:    T := TKMHouseBarracks.Load(LoadStream);
      ht_Store:       T := TKMHouseStore.Load(LoadStream);
      ht_WatchTower:  T := TKMHouseTower.Load(LoadStream);
      ht_Woodcutters: T := TKMHouseWoodcutters.Load(LoadStream);
      else            T := TKMHouse.Load(LoadStream);
    end;

    if T <> nil then
      fHouses.Add(T);
  end;
end;


procedure TKMHousesCollection.SyncLoad;
var I:integer;
begin
  for I := 0 to Count - 1 do
  begin
    Houses[I].SyncLoad;
    if Houses[I].fCurrentAction<>nil then
      Houses[I].fCurrentAction.fHouse := fPlayers.GetHouseByID(cardinal(Houses[I].fCurrentAction.fHouse));
  end;
end;


//Update resource requested counts for all houses
procedure TKMHousesCollection.UpdateResRequest;
var I: Integer;
begin
  for I := 0 to Count - 1 do
  if (not Houses[I].IsDestroyed) and (Houses[I].fBuildState = hbs_Done) then
    Houses[I].UpdateResRequest;
end;


procedure TKMHousesCollection.UpdateState;
var
  I: Integer;
begin
  for I := Count - 1 downto 0  do
    if not Houses[I].IsDestroyed then
      Houses[I].UpdateState
    else
      if FREE_POINTERS and (Houses[I].PointerCount = 0) then
        fHouses.Delete(I); //Because no one needs this anymore it must DIE!!!!! :D
end;


procedure TKMHousesCollection.IncAnimStep;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Houses[I].IncAnimStep;
end;


function TKMHousesCollection.GetTotalPointers: Cardinal;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Houses[I].PointerCount;
end;


procedure TKMHousesCollection.Paint;
const Margin = 3;
var
  I: Integer;
  Rect: TKMRect;
begin
  //Compensate for big houses near borders or standing on hills
  Rect := KMRectGrow(fGame.Viewport.GetClip, Margin);

  for I := 0 to Count - 1 do
  if not Houses[I].IsDestroyed and KMInRect(Houses[I].fPosition, Rect) then
    Houses[I].Paint;
end;


end.
