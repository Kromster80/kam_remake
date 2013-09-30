unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Terrain, KM_ResHouses, KM_ResWares;

{Everything related to houses is here}
type
  TWoodcutterMode = (wcm_Chop, wcm_ChopAndPlant);

  TKMHouse = class;
  TKMHouseEvent = procedure(aHouse: TKMHouse) of object;
  TKMHouseFromEvent = procedure(aHouse: TKMHouse; aFrom: THandIndex) of object;

  THouseAction = class
  private
    fHouse: TKMHouse;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
    procedure SetHouseState(aHouseState: THouseState);
  public
    constructor Create(aHouse: TKMHouse; aHouseState: THouseState);
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
    fUID: Integer; //unique ID, used for save/load to sync to
    fHouseType: THouseType; //House type

    fBuildSupplyWood: Byte; //How much Wood was delivered to house building site
    fBuildSupplyStone: Byte; //How much Stone was delivered to house building site
    fBuildReserve: Byte; //Take one build supply resource into reserve and "build from it"
    fBuildingProgress: Word; //That is how many efforts were put into building (Wooding+Stoning)
    fDamage: Word; //Damaged inflicted to house

    fHasOwner: Boolean; //which is some TKMUnit
    fBuildingRepair: Boolean; //If on and the building is damaged then labourers will come and repair it
    fWareDelivery: Boolean; //If on then no wares will be delivered here

    fResourceIn: array [1..4] of Byte; //Resource count in input
    fResourceDeliveryCount: array[1..4] of Word; //Count of the resources we have ordered for the input (used for ware distribution)
    fResourceOut: array [1..4]of Byte; //Resource count in output
    fResourceOrder: array [1..4]of Word; //If HousePlaceOrders=true then here are production orders
    fLastOrderProduced: Byte;
    fResOrderDesired: array [1..4]of Single;

    WorkAnimStep: Cardinal; //Used for Work and etc.. which is not in sync with Flags
    fIsOnSnow: Boolean;
    fSnowStep: Single;

    fIsDestroyed: Boolean;
    RemoveRoadWhenDemolish: Boolean;
    fPointerCount: Cardinal;
    fTimeSinceUnoccupiedReminder: Integer;
    fDisableUnoccupiedMessage: Boolean;

    procedure CheckOnSnow;

    procedure MakeSound; dynamic; //Swine/stables make extra sounds
    function GetResDistribution(aID: Byte): Byte; //Will use GetRatio from mission settings to find distribution amount
  protected
    fBuildState: THouseBuildState; // = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);
    FlagAnimStep: Cardinal; //Used for Flags and Burning animation
    fOwner: THandIndex; //House owner player, determines flag color as well
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
    procedure Activate(aWasBuilt: Boolean); virtual;
    function GetResOrder(aId: Byte): Integer; virtual;
    procedure SetBuildingRepair(aValue: Boolean);
    procedure SetResOrder(aId: Byte; aValue: Integer); virtual;
  public
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle
    ResourceDepletedMsgIssued: Boolean;
    DoorwayUse: Byte; //number of units using our door way. Used for sliding.
    OnDestroyed: TKMHouseFromEvent;

    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;
    function GetHousePointer: TKMHouse; //Returns self and adds one to the pointer counter
    procedure ReleaseHousePointer; //Decreases the pointer counter
    property PointerCount: Cardinal read fPointerCount;

    procedure DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False); virtual;
    property UID: Integer read fUID;

    property GetPosition: TKMPoint read fPosition;
    procedure SetPosition(aPos: TKMPoint); //Used only by map editor
    procedure OwnerUpdate(aOwner: THandIndex);
    function GetEntrance: TKMPoint;
    function GetClosestCell(aPos: TKMPoint): TKMPoint;
    function GetDistance(aPos: TKMPoint): Single;
    function InReach(aPos: TKMPoint; aDistance: Single): Boolean;
    procedure GetListOfCellsAround(Cells: TKMPointDirList; aPassability: TPassability);
    procedure GetListOfCellsWithin(Cells: TKMPointList);
    function GetRandomCellWithin: TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property HouseType: THouseType read fHouseType;
    property BuildingRepair: Boolean read fBuildingRepair write SetBuildingRepair;
    property WareDelivery: Boolean read fWareDelivery write fWareDelivery;
    property GetHasOwner: Boolean read fHasOwner write fHasOwner; //There's a citizen who runs this house
    property Owner: THandIndex read fOwner;
    property DisableUnoccupiedMessage: Boolean read fDisableUnoccupiedMessage write fDisableUnoccupiedMessage;
    function GetHealth: Word;
    function GetBuildWoodDelivered: Byte;
    function GetBuildStoneDelivered: Byte;

    property BuildingState: THouseBuildState read fBuildState write fBuildState;
    procedure IncBuildingProgress;
    function MaxHealth: Word;
    procedure AddDamage(aFrom: THandIndex; aAmount: Word; aIsEditor: Boolean = False);
    procedure AddRepair(aAmount: Word = 5);
    procedure UpdateDamage;

    function IsStone: Boolean;
    function IsComplete: Boolean;
    function IsDamaged: Boolean;
    property IsDestroyed: Boolean read fIsDestroyed;
    property GetDamage: Word read fDamage;

    procedure SetState(aState: THouseState);
    function GetState: THouseState;

    function CheckResIn(aWare: TWareType): Word; virtual;
    function CheckResOut(aWare: TWareType): Byte;
    function PickOrder: Byte;
    function CheckResToBuild: Boolean;
    function GetMaxInRes: Word;
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual; //override for School and etc..
    procedure ResAddToOut(aWare: TWareType; const aCount: Integer = 1);
    procedure ResAddToBuild(aWare: TWareType);
    procedure ResTakeFromIn(aWare: TWareType; aCount: Byte = 1);
    procedure ResTakeFromOut(aWare: TWareType; const aCount: Word = 1); virtual;
    function ResCanAddToIn(aRes: TWareType): Boolean; virtual;
    property ResOrder[aId: Byte]: Integer read GetResOrder write SetResOrder;

    procedure Save(SaveStream: TKMemoryStream); virtual;

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
    constructor Load(LoadStream: TKMemoryStream); override;
    function FeedBeasts: Byte;
    procedure TakeBeast(aID: Byte);
    procedure MakeSound; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override;
  end;

  TKMHouseInn = class(TKMHouse)
  private
    Eater: array [1..6] of record //only 6 units are allowed in the inn
      UnitType: TUnitType;
      FoodKind: TWareType; //What kind of food eater eats
      EatStep: Cardinal;
    end;
  public
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    function EaterGetsInside(aUnitType: TUnitType): Byte;
    procedure UpdateEater(aID: Byte; aFoodKind: TWareType);
    procedure EatersGoesOut(aID: Byte);
    function HasFood: Boolean;
    function HasSpace: Boolean;
    procedure Save(SaveStream: TKMemoryStream); override;
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
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False); override;
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function AddUnitToQueue(aUnit: TUnitType; aCount: Byte): Byte; //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure UnitTrainingComplete(aUnit: Pointer); //This should shift queue filling rest with ut_None
    function GetTrainingProgress: Single;
    function QueueIsEmpty: Boolean;
    property HideOneGold: Boolean read fHideOneGold;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  private
    ResourceCount: array [WARE_MIN .. WARE_MAX] of Word;
  protected
    procedure Activate(aWasBuilt: Boolean); override;
  public
    NotAcceptFlag: array [WARE_MIN .. WARE_MAX] of Boolean;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False); override;
    procedure ToggleAcceptFlag(aRes: TWareType);
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function CheckResIn(aWare: TWareType): Word; override;
    procedure ResTakeFromOut(aWare: TWareType; const aCount: Word = 1); override;
    function ResCanAddToIn(aRes: TWareType): Boolean; override;
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
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_CommonTypes, KM_RenderPool, KM_RenderAux, KM_Units, KM_Scripting,
  KM_HandsCollection, KM_ResSound, KM_Sound, KM_Game, KM_ResTexts,
  KM_Resource, KM_Utils, KM_FogOfWar, KM_AI;


{ TKMHouse }
constructor TKMHouse.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
var I: Byte;
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

  fHasOwner         := False;
  //Initially repair is [off]. But for AI it's controlled by a command in DAT script
  fBuildingRepair   := False; //Don't set it yet because we don't always know who are AIs yet (in multiplayer) It is set in first UpdateState
  DoorwayUse        := 0;
  fWareDelivery     := True;

  for i:=1 to 4 do
  begin
    fResourceIn[i]  := 0;
    fResourceDeliveryCount[i] := 0;
    fResourceOut[i] := 0;
    fResourceOrder[i]:=0;
  end;

  fIsDestroyed      := false;
  RemoveRoadWhenDemolish := gTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay <> to_Road;
  fPointerCount     := 0;
  fTimeSinceUnoccupiedReminder   := TIME_BETWEEN_MESSAGES;

  fUID := aUID;
  ResourceDepletedMsgIssued := false;

  if aBuildState = hbs_Done then //House was placed on map already Built e.g. in mission maker
  begin
    Activate(False);
    fBuildingProgress := fResource.HouseDat[fHouseType].MaxHealth;
    gTerrain.SetHouse(fPosition, fHouseType, hsBuilt, fOwner, (gGame <> nil) and (gGame.GameMode <> gmMapEd)); //Sets passability and flattens terrain if we're not in the map editor
  end
  else
    gTerrain.SetHouse(fPosition, fHouseType, hsFence, fOwner); //Terrain remains neutral yet

  //Built houses accumulate snow slowly, pre-placed houses are already covered
  CheckOnSnow;
  fSnowStep := Byte(aBuildState = hbs_Done);
end;


constructor TKMHouse.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
  HasAct: Boolean;
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
  for I:=1 to 4 do LoadStream.Read(fResourceIn[I]);
  for I:=1 to 4 do LoadStream.Read(fResourceDeliveryCount[I]);
  for I:=1 to 4 do LoadStream.Read(fResourceOut[I]);
  for I:=1 to 4 do LoadStream.Read(fResourceOrder[I], SizeOf(fResourceOrder[I]));
  for I:=1 to 4 do LoadStream.Read(fResOrderDesired[I], SizeOf(fResOrderDesired[I]));
  LoadStream.Read(fLastOrderProduced);
  LoadStream.Read(FlagAnimStep);
  LoadStream.Read(WorkAnimStep);
  LoadStream.Read(fIsOnSnow);
  LoadStream.Read(fSnowStep);
  LoadStream.Read(fIsDestroyed);
  LoadStream.Read(RemoveRoadWhenDemolish);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fTimeSinceUnoccupiedReminder);
  LoadStream.Read(fDisableUnoccupiedMessage);
  LoadStream.Read(fUID);
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
  if fCurrentAction <> nil then
    fCurrentAction.fHouse := gHands.GetHouseByUID(Cardinal(fCurrentAction.fHouse));
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
var I: Integer; Res: TWareType;
begin
  gHands[fOwner].Stats.HouseCreated(fHouseType, aWasBuilt); //Only activated houses count
  gHands.RevealForTeam(fOwner, fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_MAX);

  fCurrentAction := THouseAction.Create(Self, hst_Empty);
  fCurrentAction.SubActionAdd([ha_Flagpole, ha_Flag1..ha_Flag3]);

  UpdateDamage; //House might have been damaged during construction, so show flames when it is built

  for I := 1 to 4 do
  begin
    Res := fResource.HouseDat[fHouseType].ResInput[I];
    with gHands[fOwner].Deliveries.Queue do
    case Res of
      wt_None:    ;
      wt_Warfare: AddDemand(Self, nil, Res, 1, dt_Always, diNorm);
      wt_All:     AddDemand(Self, nil, Res, 1, dt_Always, diNorm);
      else        begin
                    AddDemand(Self, nil, Res, GetResDistribution(I), dt_Once,   diNorm); //Every new house needs 5 resourceunits
                    inc(fResourceDeliveryCount[I],GetResDistribution(I)); //Keep track of how many resources we have on order (for distribution of wares)
                  end;
    end;
  end;
end;


//IsSilent parameter is used by Editor and scripts
procedure TKMHouse.DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False);
var I: Integer; R: TWareType;
begin
  if IsDestroyed then Exit;

  //We must do this before setting fIsDestroyed for scripting
  OnDestroyed(Self, aFrom);

  //If anyone still has a pointer to the house he should check for IsDestroyed flag
  fIsDestroyed := True;

  //Play sound
  if (fBuildState > hbs_NoGlyph) and not IsSilent then
    gSoundPlayer.Play(sfx_HouseDestroy, fPosition);

  gHands[fOwner].Stats.WareConsumed(wt_Wood, fBuildSupplyWood);
  gHands[fOwner].Stats.WareConsumed(wt_Stone, fBuildSupplyStone);

  for I := 1 to 4 do
  begin
    R := fResource.HouseDat[fHouseType].ResInput[I];
    if R in [WARE_MIN..WARE_MAX] then
      gHands[fOwner].Stats.WareConsumed(R, fResourceIn[I]);
    R := fResource.HouseDat[fHouseType].ResOutput[I];
    if R in [WARE_MIN..WARE_MAX] then
      gHands[fOwner].Stats.WareConsumed(R, fResourceOut[I]);
  end;

  gTerrain.SetHouse(fPosition, fHouseType, hsNone, -1);

  //Leave rubble
  if not IsSilent then
    gTerrain.AddHouseRemainder(fPosition, fHouseType, fBuildState);

  BuildingRepair := False; //Otherwise labourers will take task to repair when the house is destroyed
  if RemoveRoadWhenDemolish and ((BuildingState in [hbs_NoGlyph, hbs_Wood]) or IsSilent) then
  begin
    if gTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay = to_Road then
    begin
      gTerrain.RemRoad(GetEntrance);
      if not IsSilent then
        gTerrain.Land[GetEntrance.Y, GetEntrance.X].TileOverlay := to_Dig3; //Remove road and leave dug earth behind
    end;
  end;

  FreeAndNil(fCurrentAction);

  //Leave disposing of units inside the house to themselves
end;


//Used by MapEditor
procedure TKMHouse.SetPosition(aPos: TKMPoint);
var
  WasOnSnow: Boolean;
begin
  Assert(gGame.GameMode = gmMapEd);
  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  gTerrain.SetHouse(fPosition, fHouseType, hsNone, -1);
  gTerrain.RemRoad(GetEntrance);
  if gHands[MySpectator.HandIndex].CanAddHousePlan(aPos, HouseType) then
  begin
    fPosition.X := aPos.X - fResource.HouseDat[fHouseType].EntranceOffsetX;
    fPosition.Y := aPos.Y;
  end;
  gTerrain.SetHouse(fPosition, fHouseType, hsBuilt, fOwner);
  gTerrain.SetField(GetEntrance, fOwner, ft_Road);

  //Do not remove all snow if house is moved from snow to snow
  WasOnSnow := fIsOnSnow;
  CheckOnSnow;
  if not WasOnSnow or not fIsOnSnow then
    fSnowStep := 0;
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
  Result := KMPoint(0,0);
  C := TKMPointList.Create;
  try
    GetListOfCellsWithin(C);
    if not C.GetClosest(aPos, Result) then
      Assert(false, 'Could not find closest house cell');
  finally
    C.Free;
  end;
end;


{Return distance from aPos to the closest house tile}
function TKMHouse.GetDistance(aPos: TKMPoint): Single;
var
  I, K: Integer;
  Loc: TKMPoint;
  HA: THouseArea;
begin
  Result := MaxSingle;
  Loc := fPosition;
  HA := fResource.HouseDat[fHouseType].BuildArea;

  for I := max(Loc.Y - 3, 1) to Loc.Y do
  for K := max(Loc.X - 2, 1) to min(Loc.X + 1, gTerrain.MapX) do
  if HA[I - Loc.Y + 4, K - Loc.X + 3] <> 0 then
    Result := Min(Result, KMLength(aPos, KMPoint(K, I)));
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
    if gTerrain.CheckPassability(KMPoint(X,Y), aPassability) then
      Cells.Add(KMPointDir(X, Y, Dir));
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
    for K := max(Loc.X - 2, 1) to min(Loc.X + 1, gTerrain.MapX) do
      if HouseArea[i - Loc.Y + 4, K - Loc.X + 3] <> 0 then
        Cells.Add(KMPoint(K, i));
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

  if (fBuildState=hbs_Wood) and (fBuildReserve = 0) then
  begin
    dec(fBuildSupplyWood);
    inc(fBuildReserve, 50);
  end;
  if (fBuildState=hbs_Stone) and (fBuildReserve = 0) then
  begin
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
    gHands[fOwner].Stats.HouseEnded(fHouseType);
    Activate(True);
    fScripting.ProcHouseBuilt(Self);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      gHands[fOwner].BuildList.RepairList.AddHouse(Self);
  end;
end;


function TKMHouse.MaxHealth: Word;
begin
  if fBuildState = hbs_NoGlyph then
    Result := 0
  else
    Result := fResource.HouseDat[fHouseType].MaxHealth;
end;


procedure TKMHouse.OwnerUpdate(aOwner: THandIndex);
begin
  fOwner := aOwner;
end;


{Add damage to the house, positive number}
procedure TKMHouse.AddDamage(aFrom: THandIndex; aAmount: Word; aIsEditor: Boolean = False);
begin
  if IsDestroyed then
    Exit;

  //(NoGlyph houses MaxHealth = 0 destroyed instantly)
  fDamage := Math.min(fDamage + aAmount, MaxHealth);
  if IsComplete then
  begin
    if BuildingRepair then
      gHands[fOwner].BuildList.RepairList.AddHouse(Self);

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


procedure TKMHouse.SetBuildingRepair(aValue: Boolean);
begin
  fBuildingRepair := aValue;

  if fBuildingRepair then
  begin
    if IsComplete and IsDamaged and not IsDestroyed then
      gHands[fOwner].BuildList.RepairList.AddHouse(Self);
  end
  else
    //Worker checks on house and will cancel the walk if Repair is turned off
    //RepairList removes the house automatically too
end;


function TKMHouse.IsStone: Boolean;
begin
  Result := fBuildState = hbs_Stone;
end;


{Check if house is completely built, nevermind the damage}
function TKMHouse.IsComplete: Boolean;
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


function TKMHouse.GetState: THouseState;
begin
  Result := fCurrentAction.State;
end;


//Check if house is placed mostly on snow
procedure TKMHouse.CheckOnSnow;
var
  I: Byte;
  SnowTiles: Byte;
  Cells: TKMPointList;
begin
  Cells := TKMPointList.Create;

  GetListOfCellsWithin(Cells);

  SnowTiles := 0;
  for I := 0 to Cells.Count - 1 do
    SnowTiles := SnowTiles + Byte(gTerrain.TileIsSnow(Cells[I].X, Cells[I].Y));

  fIsOnSnow := SnowTiles > (Cells.Count div 2);

  Cells.Free;
end;


{How much resources house has in Input}
function TKMHouse.CheckResIn(aWare: TWareType): Word;
var i:integer;
begin
  Result := 0;
  for i:=1 to 4 do
  if (aWare = fResource.HouseDat[fHouseType].ResInput[i]) or (aWare = wt_All) then
    inc(Result, fResourceIn[i]);
end;


{How much resources house has in Output}
function TKMHouse.CheckResOut(aWare: TWareType): Byte;
var I: Integer;
begin
  Result := 0;
  for I := 1 to 4 do
  if (aWare = fResource.HouseDat[fHouseType].ResOutput[I]) or (aWare = wt_All) then
    Inc(Result, fResourceOut[I]);
end;


{Check amount of placed order for given ID}
function TKMHouse.GetResOrder(aID: Byte): Integer;
begin
  Result := fResourceOrder[aID];
end;


//Input value is integer because we might get a -100 order from outside and need to fit it to range
//properly
procedure TKMHouse.SetResOrder(aID: Byte; aValue: Integer);
var
  I: Integer;
  TotalDesired: Integer;
begin
  fResourceOrder[aID] := EnsureRange(aValue, 0, MAX_WARES_ORDER);

  //Calculate desired production ratio (so that we are not affected by fResourceOrder which decreases till 0)
  TotalDesired := fResourceOrder[1] + fResourceOrder[2] + fResourceOrder[3] + fResourceOrder[4];
  for I := 1 to 4 do
    fResOrderDesired[I] := fResourceOrder[I] / TotalDesired;
end;


//Select order we will be making
//Order picking in sequential, so that if orders for 1st = 6 and for 2nd = 2
//then the production will go like so: 12121111
function TKMHouse.PickOrder: Byte;
var
  I, Res: Byte;
  Ware: TWareType;
  BestBid: Single;
  TotalLeft: Integer;
  LeftRatio: array [1..4] of Single;
begin
  Result := 0;

  if WARFARE_ORDER_SEQUENTIAL then
    for I := 0 to 3 do
    begin
      Res := ((fLastOrderProduced + I) mod 4) + 1; //1..4
      Ware := fResource.HouseDat[fHouseType].ResOutput[Res];
      if (ResOrder[Res] > 0) //Player has ordered some of this
      and (CheckResOut(Ware) < MAX_WARES_IN_HOUSE) //Output of this is not full
      //Check we have wares to produce this weapon. If both are the same type check > 1 not > 0
      and ((WarfareCosts[Ware,1] <> WarfareCosts[Ware,2]) or (CheckResIn(WarfareCosts[Ware,1]) > 1))
      and ((WarfareCosts[Ware,1] = wt_None) or (CheckResIn(WarfareCosts[Ware,1]) > 0))
      and ((WarfareCosts[Ware,2] = wt_None) or (CheckResIn(WarfareCosts[Ware,2]) > 0)) then
      begin
        Result := Res;
        fLastOrderProduced := Res;
        Break;
      end;
    end;

  if WARFARE_ORDER_PROPORTIONAL then
  begin
    //See the ratio between items that were made (since last order amount change)
    TotalLeft := fResourceOrder[1] + fResourceOrder[2] + fResourceOrder[3] + fResourceOrder[4];
    for I := 1 to 4 do
      LeftRatio[I] := fResourceOrder[I] / TotalLeft;

    //Left   Desired
    //0.5    0.6
    //0.3    0.3
    //0.2    0.1

    //Find order that which production ratio is the smallest
    BestBid := -MaxSingle;
    for I := 1 to 4 do
    if (ResOrder[I] > 0) then //Player has ordered some of this
    begin
      Ware := fResource.HouseDat[fHouseType].ResOutput[I];

      if (CheckResOut(Ware) < MAX_WARES_IN_HOUSE) //Output of this is not full
      //Check we have enough wares to produce this weapon. If both are the same type check > 1 not > 0
      and ((WarfareCosts[Ware,1] <> WarfareCosts[Ware,2]) or (CheckResIn(WarfareCosts[Ware,1]) > 1))
      and ((WarfareCosts[Ware,1] = wt_None) or (CheckResIn(WarfareCosts[Ware,1]) > 0))
      and ((WarfareCosts[Ware,2] = wt_None) or (CheckResIn(WarfareCosts[Ware,2]) > 0))
      and (LeftRatio[I] - fResOrderDesired[I] > BestBid) then
      begin
        Result := I;
        BestBid := LeftRatio[Result] - fResOrderDesired[Result];
      end;
    end;
  end;

  if Result <> 0 then
    Dec(fResourceOrder[Result]);
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
    Result := MAX_WARES_IN_HOUSE; //All other houses can only stock 5 for now
end;


//Maybe it's better to rule out In/Out? No, it is required to separate what can be taken out of the house and what not.
//But.. if we add "Evacuate" button to all house the separation becomes artificial..
procedure TKMHouse.ResAddToIn(aWare: TWareType; aCount:word=1; aFromScript:boolean=false);
var I,OrdersRemoved: Integer;
begin
  Assert(aWare <> wt_None);

  for I := 1 to 4 do
  if aWare = fResource.HouseDat[fHouseType].ResInput[I] then
  begin
    //Don't allow the script to overfill houses
    if aFromScript then aCount := Min(aCount, GetMaxInRes - fResourceIn[I]);
    Inc(fResourceIn[I], aCount);
    if aFromScript then
    begin
      Inc(fResourceDeliveryCount[I], aCount);
      OrdersRemoved := gHands[fOwner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
      Dec(fResourceDeliveryCount[I], OrdersRemoved);
    end;
  end;
end;


procedure TKMHouse.ResAddToOut(aWare: TWareType; const aCount:integer=1);
var I: Integer;
begin
  if aWare = wt_None then exit;
  for I := 1 to 4 do
  if aWare = fResource.HouseDat[fHouseType].ResOutput[I] then
    begin
      inc(fResourceOut[I], aCount);
      gHands[fOwner].Deliveries.Queue.AddOffer(Self, aWare, aCount);
    end;
end;


{Add resources to building process}
procedure TKMHouse.ResAddToBuild(aWare: TWareType);
begin
  case aWare of
    wt_Wood:  Inc(fBuildSupplyWood);
    wt_Stone: Inc(fBuildSupplyStone);
    else      raise ELocError.Create('WIP house is not supposed to recieve ' + fResource.Wares[aWare].Title + ', right?', fPosition);
  end;
end;


function TKMHouse.ResCanAddToIn(aRes: TWareType): Boolean;
var I: Integer;
begin
  Result := False;
  for I := 1 to 4 do
  if aRes = fResource.HouseDat[fHouseType].ResInput[I] then
    Result := True;
end;


//Take resource from Input and order more of that kind if DistributionRatios allow
procedure TKMHouse.ResTakeFromIn(aWare: TWareType; aCount: Byte=1);
var I,K: Integer;
begin
  Assert(aWare <> wt_None);

  for I := 1 to 4 do
  if aWare = fResource.HouseDat[fHouseType].ResInput[I] then
  begin
    Assert(fResourceIn[I] >= aCount, 'fResourceIn[i] < 0');
    Dec(fResourceIn[I], aCount);
    fResourceDeliveryCount[I] := Max(fResourceDeliveryCount[I] - aCount, 0);
    //Only request a new resource if it is allowed by the distribution of wares for our parent player
    for K := 1 to aCount do
      if fResourceDeliveryCount[I] < GetResDistribution(I) then
      begin
        gHands[fOwner].Deliveries.Queue.AddDemand(Self, nil, aWare, 1, dt_Once, diNorm);
        Inc(fResourceDeliveryCount[I]);
      end;
    Exit;
  end;
end;


procedure TKMHouse.ResTakeFromOut(aWare: TWareType; const aCount: Word=1);
var i:integer;
begin
  Assert(aWare<>wt_None);
  Assert(not(fHouseType in [ht_Store,ht_Barracks]));
  for i:=1 to 4 do
  if aWare = fResource.HouseDat[fHouseType].ResOutput[i] then
  begin
    Assert(aCount <= fResourceOut[i]);
    dec(fResourceOut[i], aCount);
    exit;
  end;
end;


function TKMHouse.GetResDistribution(aID: Byte): Byte;
begin
  Result := gHands[fOwner].Stats.Ratio[fResource.HouseDat[fHouseType].ResInput[aID],fHouseType];
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

  //Do not play sounds if house is invisible to MySpectator
  //This check is slower so we do it after other Exit checks
  if MySpectator.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then exit;

  case fHouseType of //Various buildings and HouseActions producing sounds
    ht_School:        if (Work = ha_Work5)and(Step = 28) then gSoundPlayer.Play(sfx_SchoolDing, fPosition); //Ding as the clock strikes 12
    ht_Mill:          if (Work = ha_Work2)and(Step = 0) then gSoundPlayer.Play(sfx_mill, fPosition);
    ht_CoalMine:      if (Work = ha_Work1)and(Step = 5) then gSoundPlayer.Play(sfx_coalDown, fPosition)
                      else if (Work = ha_Work1)and(Step = 24) then gSoundPlayer.Play(sfx_CoalMineThud, fPosition,true,0.8)
                      else if (Work = ha_Work2)and(Step = 7) then gSoundPlayer.Play(sfx_mine, fPosition)
                      else if (Work = ha_Work5)and(Step = 1) then gSoundPlayer.Play(sfx_coalDown, fPosition);
    ht_IronMine:      if (Work = ha_Work2)and(Step = 7) then gSoundPlayer.Play(sfx_mine, fPosition);
    ht_GoldMine:      if (Work = ha_Work2)and(Step = 5) then gSoundPlayer.Play(sfx_mine, fPosition);
    ht_Sawmill:       if (Work = ha_Work2)and(Step = 1) then gSoundPlayer.Play(sfx_saw, fPosition);
    ht_Wineyard:      if (Work = ha_Work2)and(Step in [1,7,13,19]) then gSoundPlayer.Play(sfx_wineStep, fPosition)
                      else if (Work = ha_Work5)and(Step = 14) then gSoundPlayer.Play(sfx_wineDrain, fPosition,true,1.5)
                      else if (Work = ha_Work1)and(Step = 10) then gSoundPlayer.Play(sfx_wineDrain, fPosition,true,1.5);
    ht_Bakery:        if (Work = ha_Work3)and(Step in [6,25]) then gSoundPlayer.Play(sfx_BakerSlap, fPosition);
    ht_Quary:         if (Work = ha_Work2)and(Step in [4,13]) then gSoundPlayer.Play(sfx_QuarryClink, fPosition)
                      else if (Work = ha_Work5)and(Step in [4,13,22]) then gSoundPlayer.Play(sfx_QuarryClink, fPosition);
    ht_WeaponSmithy:  if (Work = ha_Work1)and(Step in [17,22]) then gSoundPlayer.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work2)and(Step in [10,25]) then gSoundPlayer.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work3)and(Step in [10,25]) then gSoundPlayer.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work4)and(Step in [8,22]) then gSoundPlayer.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work5)and(Step = 12) then gSoundPlayer.Play(sfx_BlacksmithBang, fPosition);
    ht_ArmorSmithy:   if (Work = ha_Work2)and(Step in [13,28]) then gSoundPlayer.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work3)and(Step in [13,28]) then gSoundPlayer.Play(sfx_BlacksmithBang, fPosition)
                      else if (Work = ha_Work4)and(Step in [8,22]) then gSoundPlayer.Play(sfx_BlacksmithFire, fPosition)
                      else if (Work = ha_Work5)and(Step in [8,22]) then gSoundPlayer.Play(sfx_BlacksmithFire, fPosition);
    ht_Metallurgists: if (Work = ha_Work3)and(Step = 6) then gSoundPlayer.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work4)and(Step in [16,20]) then gSoundPlayer.Play(sfx_wineDrain, fPosition);
    ht_IronSmithy:    if (Work = ha_Work2)and(Step in [1,16]) then gSoundPlayer.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work3)and(Step = 1) then gSoundPlayer.Play(sfx_metallurgists, fPosition)
                      else if (Work = ha_Work3)and(Step = 13) then gSoundPlayer.Play(sfx_wineDrain, fPosition);
    ht_WeaponWorkshop:if (Work = ha_Work2)and(Step in [1,10,19]) then gSoundPlayer.Play(sfx_saw, fPosition)
                      else if (Work = ha_Work3)and(Step in [10,21]) then gSoundPlayer.Play(sfx_CarpenterHammer, fPosition)
                      else if (Work = ha_Work4)and(Step in [2,13]) then gSoundPlayer.Play(sfx_CarpenterHammer, fPosition);
    ht_ArmorWorkshop: if (Work = ha_Work2)and(Step in [3,13,23]) then gSoundPlayer.Play(sfx_saw, fPosition)
                      else if (Work = ha_Work3)and(Step in [17,28]) then gSoundPlayer.Play(sfx_CarpenterHammer, fPosition)
                      else if (Work = ha_Work4)and(Step in [10,20]) then gSoundPlayer.Play(sfx_CarpenterHammer, fPosition);
    ht_Tannery:       if (Work = ha_Work2)and(Step = 5) then gSoundPlayer.Play(sfx_Leather, fPosition,true,0.8);
    ht_Butchers:      if (Work = ha_Work2)and(Step in [8,16,24]) then gSoundPlayer.Play(sfx_ButcherCut, fPosition)
                      else if (Work = ha_Work3)and(Step in [9,21]) then gSoundPlayer.Play(sfx_SausageString, fPosition);
    ht_Swine:         if ((Work = ha_Work2)and(Step in [10,20]))or((Work = ha_Work3)and(Step = 1)) then gSoundPlayer.Play(sfx_ButcherCut, fPosition);
    //ht_WatchTower:  Sound handled by projectile itself
  end;
end;


procedure TKMHouse.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
  HasAct: Boolean;
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
  for I:=1 to 4 do SaveStream.Write(fResourceIn[I]);
  for I:=1 to 4 do SaveStream.Write(fResourceDeliveryCount[I]);
  for I:=1 to 4 do SaveStream.Write(fResourceOut[I]);
  for I:=1 to 4 do SaveStream.Write(fResourceOrder[I], SizeOf(fResourceOrder[I]));
  for I:=1 to 4 do SaveStream.Write(fResOrderDesired[I], SizeOf(fResOrderDesired[I]));
  SaveStream.Write(fLastOrderProduced);
  SaveStream.Write(FlagAnimStep);
  SaveStream.Write(WorkAnimStep);
  SaveStream.Write(fIsOnSnow);
  SaveStream.Write(fSnowStep);
  SaveStream.Write(fIsDestroyed);
  SaveStream.Write(RemoveRoadWhenDemolish);
  SaveStream.Write(fPointerCount);
  SaveStream.Write(fTimeSinceUnoccupiedReminder);
  SaveStream.Write(fDisableUnoccupiedMessage);
  SaveStream.Write(fUID);
  HasAct := fCurrentAction <> nil;
  SaveStream.Write(HasAct);
  if HasAct then fCurrentAction.Save(SaveStream);
  SaveStream.Write(ResourceDepletedMsgIssued);
  SaveStream.Write(DoorwayUse);
end;


procedure TKMHouse.IncAnimStep;
const
  //How much ticks it takes for a house to become completely covered in snow
  SNOW_TIME = 300;
var
  WasOnSnow: Boolean;
begin
  Inc(FlagAnimStep);
  Inc(WorkAnimStep);

  if (FlagAnimStep mod 10 = 0) and gGame.IsMapEditor then
  begin
    WasOnSnow := fIsOnSnow;
    CheckOnSnow;
    if not WasOnSnow or not fIsOnSnow then
      fSnowStep := 0;
  end;

  if fIsOnSnow and (fSnowStep < 1) then
    fSnowStep := Min(fSnowStep + (1 + Byte(gGame.IsMapEditor) * 10) / SNOW_TIME, 1);

  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if DYNAMIC_FOG_OF_WAR then
  if FlagAnimStep mod 10 = 0 then
    gHands.RevealForTeam(fOwner, fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);
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
    if not (fResource.HouseDat[fHouseType].ResInput[I] in [wt_All, wt_Warfare, wt_None]) then
    begin

      //Not enough resources ordered, add new demand
      if fResourceDeliveryCount[I] < GetResDistribution(I) then
      begin
        Count := GetResDistribution(I)-fResourceDeliveryCount[I];
        gHands[fOwner].Deliveries.Queue.AddDemand(
          Self, nil, fResource.HouseDat[fHouseType].ResInput[I], Count, dt_Once, diNorm);

        inc(fResourceDeliveryCount[I], Count);
      end;

      //Too many resources ordered, attempt to remove demand if nobody has taken it yet
      if fResourceDeliveryCount[I] > GetResDistribution(I) then
      begin
        Excess := fResourceDeliveryCount[I]-GetResDistribution(I);
        Count := gHands[fOwner].Deliveries.Queue.TryRemoveDemand(
                   Self, fResource.HouseDat[fHouseType].ResInput[I], Excess);

        dec(fResourceDeliveryCount[I], Count); //Only reduce it by the number that were actually removed
      end;

    end;
end;


procedure TKMHouse.UpdateState;
//var HouseName: string;
begin
  if not IsComplete then Exit; //Don't update unbuilt houses

  //Show unoccupied message if needed and house belongs to human player and can have owner at all and not a barracks
  if not fDisableUnoccupiedMessage and not fHasOwner
  and (fResource.HouseDat[fHouseType].OwnerType <> ut_None) and (fHouseType <> ht_Barracks) then
  begin
    Dec(fTimeSinceUnoccupiedReminder);
    if fTimeSinceUnoccupiedReminder = 0 then
    begin
      //Hide messages for wrong player, in replays, and if we have lost
      if (fOwner = MySpectator.HandIndex) and not gGame.IsReplay and (gHands[fOwner].AI.WonOrLost <> wol_Lost) then
      begin
        //HouseName := fResource.HouseDat[HouseType].HouseName;
        //We can't paste houses name instead of %s like that because of plurals and feminine/masculine attrib
        gGame.ShowMessage(mkHouse, gResTexts[TX_MSG_HOUSE_UNOCCUPIED], GetEntrance);
      end;
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
                  fRenderPool.AddHouse(fHouseType, fPosition, Progress, 0, 0);
                  fRenderPool.AddHouseBuildSupply(fHouseType, fPosition, fBuildSupplyWood, fBuildSupplyStone);
                end;
    hbs_Stone:  begin
                  Progress := (fBuildingProgress / 50 - H.WoodCost) / H.StoneCost;
                  fRenderPool.AddHouse(fHouseType, fPosition, 1, Progress, 0);
                  fRenderPool.AddHouseBuildSupply(fHouseType, fPosition, fBuildSupplyWood, fBuildSupplyStone);
                end;
    else        begin
                  //Incase we need to render house at desired step in debug mode
                  if HOUSE_BUILDING_STEP = 0 then
                  begin
                    if fIsOnSnow then
                      fRenderPool.AddHouse(fHouseType, fPosition, 1, 1, fSnowStep)
                    else
                      fRenderPool.AddHouse(fHouseType, fPosition, 1, 1, 0);
                    fRenderPool.AddHouseSupply(fHouseType, fPosition, fResourceIn, fResourceOut);
                    if fCurrentAction <> nil then
                      fRenderPool.AddHouseWork(fHouseType, fPosition, fCurrentAction.SubAction, WorkAnimStep, gHands[fOwner].FlagColor);
                  end
                  else
                    fRenderPool.AddHouse(fHouseType, fPosition,
                      Min(HOUSE_BUILDING_STEP * 3, 1),
                      EnsureRange(HOUSE_BUILDING_STEP * 3 - 1, 0, 1),
                      Max(HOUSE_BUILDING_STEP * 3 - 2, 0));
                end;
  end;

  if SHOW_POINTER_DOTS then
    fRenderAux.UnitPointers(fPosition.X + 0.5, fPosition.Y + 1, fPointerCount);
end;


{TKMHouseSwineStable}
constructor TKMHouseSwineStable.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(BeastAge, SizeOf(BeastAge));
end;


//Return ID of beast that has grown up
function TKMHouseSwineStable.FeedBeasts: Byte;
var i:integer;
begin
  Result:=0;
  inc(BeastAge[KaMRandom(5)+1]); //Let's hope it never overflows MAX
  for i:=1 to length(BeastAge) do
    if BeastAge[i]>3 then
      Result:=i;
end;


procedure TKMHouseSwineStable.TakeBeast(aID: Byte);
begin
  if (aID<>0) and (BeastAge[aID]>3) then
    BeastAge[aID] := 0;
end;


//Make beast noises - each beast makes a noise (if it exists) with two second pauses between each one
procedure TKMHouseSwineStable.MakeSound;
var I: Byte;
begin
  inherited;
  if MySpectator.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then Exit;

  for I := 0 to 4 do
  if BeastAge[I+1] > 0 then
  if (FlagAnimStep + 20*I) mod 100 = 0 then
  begin
    if fHouseType = ht_Stables then
      gSoundPlayer.Play(TSoundFX(byte(sfx_Horse1) + Random(4)), fPosition); //sfx_Horse1..sfx_Horse4
    if fHouseType = ht_Swine   then
      gSoundPlayer.Play(TSoundFX(byte(sfx_Pig1)   + Random(4)), fPosition); //sfx_Pig1..sfx_Pig4
  end;
end;


procedure TKMHouseSwineStable.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
end;


procedure TKMHouseSwineStable.Paint;
var I: Integer;
begin
  inherited;
  //We render beasts on top of the HouseWork (which is mostly flames in this case), because otherwise
  //Swinefarm looks okay, but Stables are totaly wrong - flames are right on horses backs!
  if fBuildState = hbs_Done then
    for I := 1 to 5 do
      if BeastAge[I] > 0 then
        fRenderPool.AddHouseStableBeasts(fHouseType, fPosition, I, Min(BeastAge[I],3), WorkAnimStep);

  //But Animal Breeders should be on top of beasts
  if fCurrentAction <> nil then
    fRenderPool.AddHouseWork(fHouseType, fPosition,
                            fCurrentAction.SubAction * [ha_Work1, ha_Work2, ha_Work3, ha_Work4, ha_Work5],
                            WorkAnimStep, gHands[fOwner].FlagColor);
end;


{ TKMHouseInn }
constructor TKMHouseInn.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
var I: Integer;
begin
  inherited;

  for I := Low(Eater) to High(Eater) do
    Eater[I].UnitType := ut_None;
end;


constructor TKMHouseInn.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(Eater, SizeOf(Eater));
end;


//EatStep := FlagAnimStep, cos increases it each frame, we don't need to increase all 6 AnimSteps manually
function TKMHouseInn.EaterGetsInside(aUnitType: TUnitType): Byte;
var i:integer;
begin
  Result:=0;
  for i:=low(Eater) to high(Eater) do
  if Eater[i].UnitType=ut_None then
  begin
    Eater[i].UnitType := aUnitType;
    Eater[i].FoodKind := wt_None;
    Eater[i].EatStep  := FlagAnimStep;
    Result := i;
    exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aID: byte; aFoodKind: TWareType);
begin
  if aID=0 then exit;
  Assert(aFoodKind in [wt_Wine, wt_Bread, wt_Sausages, wt_Fish], 'Wrong food kind');
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
  Result := (CheckResIn(wt_Sausages)+CheckResIn(wt_Bread)+CheckResIn(wt_Wine)+CheckResIn(wt_Fish)>0);
end;


function TKMHouseInn.HasSpace:boolean;
var
  i: integer;
begin
  Result := false;
  for i:=Low(Eater) to High(Eater) do
    Result := Result or (Eater[i].UnitType = ut_None);
end;


procedure TKMHouseInn.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(Eater, SizeOf(Eater));
end;


procedure TKMHouseInn.Paint;
  //Chose eater animation direction (1357 face south, 2468 face north)
  function AnimDir(i: Integer): TKMDirection;
  begin
    case Eater[i].FoodKind of
      wt_Wine:    Result  := TKMDirection(1 * 2 - 1 + ((i-1) div 3));
      wt_Bread:   Result  := TKMDirection(2 * 2 - 1 + ((i-1) div 3));
      wt_Sausages:Result  := TKMDirection(3 * 2 - 1 + ((i-1) div 3));
      wt_Fish:    Result  := TKMDirection(4 * 2 - 1 + ((i-1) div 3));
    else Result := dir_NA;
    end;
  end;
const
  OffX: array [0..2] of single = ( -0.5, 0, 0.5);
  OffY: array [0..2] of single = (-0.05, 0, 0.05);
var
  i: Integer;
  AnimStep: Cardinal;
begin
  inherited;
  if fBuildState <> hbs_Done then exit;

  for i := Low(Eater) to High(Eater) do
  begin
    if (Eater[i].UnitType = ut_None) or (Eater[i].FoodKind = wt_None) then Continue;

    AnimStep := FlagAnimStep - Eater[i].EatStep; //Delta is our AnimStep

    fRenderPool.AddHouseEater(fPosition, Eater[i].UnitType, ua_Eat,
                              AnimDir(i), AnimStep,
                              OffX[(i-1) mod 3], OffY[(i-1) mod 3],
                              gHands[fOwner].FlagColor);
  end;
end;


{ TKMHouseSchool }
constructor TKMHouseSchool.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
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
  Inherited;
  UnitWIP := gHands.GetUnitByUID(Cardinal(UnitWIP));
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSchool.DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False);
var
  I: Integer;
begin
  for I := 1 to High(Queue) do
    Queue[I] := ut_None;
  RemUnitFromQueue(0); //Remove WIP unit

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSchool.ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
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
  if CheckResIn(wt_Gold) = 0 then exit; //There must be enough gold to perform training

  fHideOneGold := true;
  for I := 0 to High(Queue) - 1 do
    Queue[I] := Queue[I+1]; //Shift by one
  Queue[High(Queue)] := ut_None; //Set the last one empty

  //Create the Unit
  UnitWIP := gHands[fOwner].TrainUnit(Queue[0], GetEntrance);
  TKMUnit(UnitWIP).TrainInHouse(Self); //Let the unit start the training task

  WorkAnimStep := 0;
end;


//Unit reports back to School that it was trained
procedure TKMHouseSchool.UnitTrainingComplete(aUnit: Pointer);
begin
  Assert(aUnit = UnitWIP, 'Should be called only by Unit itself when it''s trained');

  UnitWIP := nil;
  Queue[0] := ut_None; //Clear the unit in training
  ResTakeFromIn(wt_Gold); //Do the goldtaking
  gHands[fOwner].Stats.WareConsumed(wt_Gold);
  fHideOneGold := False;
  fTrainProgress := 0;

  //Attempt to start training next unit in queue
  StartTrainingUnit;
end;


//Return training progress of a unit in 0 - 1 range
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
    SaveStream.Write(TKMUnit(UnitWIP).UID) //Store ID, then substitute it with reference on SyncLoad
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
  RT: TWareType;
begin
  inherited;
  //A new storehouse should inherrit the accept properies of the first storehouse of that player,
  //which stops a sudden flow of unwanted resources to it as soon as it is create.
  FirstStore := TKMHouseStore(gHands[fOwner].FindHouse(ht_Store, 1));
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


procedure TKMHouseStore.ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
var R: TWareType;
begin
  case aWare of
    wt_All:     for R := Low(ResourceCount) to High(ResourceCount) do begin
                  ResourceCount[R] := EnsureRange(ResourceCount[R]+aCount, 0, High(Word));
                  gHands[fOwner].Deliveries.Queue.AddOffer(Self, R, aCount);
                end;
    WARE_MIN..
    WARE_MAX:   begin
                  ResourceCount[aWare]:=EnsureRange(ResourceCount[aWare]+aCount, 0, High(Word));
                  gHands[fOwner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
                end;
    else        raise ELocError.Create('Cant''t add '+fResource.Wares[aWare].Title, GetPosition);
  end;
end;


function TKMHouseStore.ResCanAddToIn(aRes: TWareType): Boolean;
begin
  Result := (aRes in [WARE_MIN..WARE_MAX]);
end;


function TKMHouseStore.CheckResIn(aWare: TWareType): Word;
begin
  if aWare in [WARE_MIN..WARE_MAX] then
    Result := ResourceCount[aWare]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


procedure TKMHouseStore.DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False);
var
  R: TWareType;
begin
  for R := WARE_MIN to WARE_MAX do
    gHands[fOwner].Stats.WareConsumed(R, ResourceCount[R]);

  inherited;
end;


procedure TKMHouseStore.ResTakeFromOut(aWare: TWareType; const aCount: Word=1);
begin
  Assert(aCount <= ResourceCount[aWare]);

  Dec(ResourceCount[aWare], aCount);
end;


procedure TKMHouseStore.ToggleAcceptFlag(aRes: TWareType);
const
  //Using shortints instead of bools makes it look much neater in code-view
  CheatPattern: array [WARE_MIN..WARE_MAX] of Byte = (
    0,0,1,0,0,
    0,1,0,1,0,
    1,0,0,0,1,
    1,0,0,0,1,
    1,1,1,1,1,
    0,0,0);
var
  R: TWareType;
  ApplyCheat: Boolean;
begin
  Assert(aRes in [WARE_MIN .. WARE_MAX]); //Dunno why thats happening sometimes..

  //We need to skip cheats in MP replays too, not just MP games, so don't use fGame.IsMultiplayer
  if CHEATS_ENABLED and (MULTIPLAYER_CHEATS or not (gGame.GameMode in [gmMulti, gmReplayMulti])) then
  begin
    ApplyCheat := True;

    //Check the cheat pattern
    for R := Low(ResourceCount) to High(ResourceCount) do
      ApplyCheat := ApplyCheat and (NotAcceptFlag[R] = boolean(CheatPattern[R]));

    if ApplyCheat then
    case aRes of
      wt_Arbalet: begin
                    ResAddToIn(wt_All, 10);
                    gHands[fOwner].Stats.WareProduced(wt_All, 10);
                    Exit;
                  end;
      wt_Horse:   if not gGame.IsMultiplayer then
                  begin
                    //Game results cheats should not be used in MP even in debug
                    //MP does Win/Defeat differently (without Hold)
                    gGame.RequestGameHold(gr_Win);
                    Exit;
                  end;
      wt_Fish:    if not gGame.IsMultiplayer then
                  begin
                    //Game results cheats should not be used in MP even in debug
                    //MP does Win/Defeat differently (without Hold)
                    gGame.RequestGameHold(gr_Defeat);
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


{ TKMHouseWoodcutters }
constructor TKMHouseWoodcutters.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
begin
  inherited;
  WoodcutterMode := wcm_ChopAndPlant;
end;


constructor TKMHouseWoodcutters.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fWoodcutterMode, SizeOf(fWoodcutterMode));
end;


procedure TKMHouseWoodcutters.Save(SaveStream: TKMemoryStream);
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
constructor THouseAction.Create(aHouse: TKMHouse; aHouseState: THouseState);
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


procedure THouseAction.Save(SaveStream: TKMemoryStream);
begin
  if fHouse <> nil then
    SaveStream.Write(fHouse.UID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fHouseState, SizeOf(fHouseState));
  SaveStream.Write(fSubAction, SizeOf(fSubAction));
end;


procedure THouseAction.Load(LoadStream: TKMemoryStream);
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
    if gTerrain.TileInMapCoords(GetPosition.X+K, GetPosition.Y+I) then
      fRenderAux.Quad(GetPosition.X+K, GetPosition.Y+I, $40FFFFFF);
end;


end.
