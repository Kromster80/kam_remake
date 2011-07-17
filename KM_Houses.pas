unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   Classes, KromUtils, Math, SysUtils,
   KM_CommonTypes, KM_Defaults, KM_Utils, KM_ResourceGFX, KM_ResourceHouse;

  {Everything related to houses is here}
type
  TKMHouse = class;

  THouseAction = class
  private
    fHouse:TKMHouse;
    fHouseState: THouseState;
    fSubAction: THouseActionSet;
    function GetWorkID:byte;
  public
    constructor Create(aHouse:TKMHouse; aHouseState: THouseState);
    procedure SetState(aHouseState: THouseState);
    procedure SubActionWork(aActionSet: THouseActionType);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    property ActionType: THouseState read fHouseState;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


  TKMHouse = class
  private
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
    fResourceDeliveryCount:array[1..4] of byte; //Count of the resources we have ordered for the input (used for ware distribution)
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
    ID:integer; //unique ID, used for save/load to sync to
    fCurrentAction: THouseAction; //Current action, withing HouseTask or idle
    ResourceDepletedMsgIssued: boolean;
    DoorwayUse: byte; //number of units using our door way. Used for sliding.

    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;
    function GetHousePointer:TKMHouse; //Returns self and adds one to the pointer counter
    procedure ReleaseHousePointer; //Decreases the pointer counter
    property GetPointerCount:integer read fPointerCount;

    procedure DemolishHouse(DoSilent:boolean; NoRubble:boolean=false);

    property GetPosition:TKMPoint read fPosition;
    procedure SetPosition(aPos:TKMPoint); //Used only by map editor
    function GetEntrance:TKMPoint;
    function GetClosestCell(aPos:TKMPoint):TKMPoint;
    function GetDistance(aPos:TKMPoint):single;
    procedure GetListOfCellsAround(Cells:TKMPointDirList; aPassability:TPassability);
    procedure GetListOfCellsWithin(Cells:TKMPointList);
    function GetRandomCellWithin:TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    function HouseArea:THouseArea;
    function DoesOrders:boolean;
    property GetHouseType:THouseType read fHouseType;
    property BuildingRepair:boolean read fBuildingRepair write fBuildingRepair;
    property WareDelivery:boolean read fWareDelivery write SetWareDelivery;
    property GetHasOwner:boolean read fHasOwner write fHasOwner;
    property GetOwner:TPlayerIndex read fOwner;
    function GetHealth:word;

    property BuildingState: THouseBuildState read fBuildState write fBuildState;
    procedure IncBuildingProgress;
    function GetMaxHealth:word;
    function  AddDamage(aAmount:word):boolean;
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
    function CheckResOrder(aID:byte):word;
    function CheckResToBuild:boolean;
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); virtual; //override for School and etc..
    procedure ResAddToOut(aResource:TResourceType; const aCount:integer=1);
    procedure ResAddToBuild(aResource:TResourceType);
    procedure ResTakeFromIn(aResource:TResourceType; aCount:byte=1);
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); virtual;
    procedure ResEditOrder(aID:byte; Amount:integer);

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
    constructor Load(LoadStream:TKMemoryStream); override;
    function EaterGetsInside(aUnitType:TUnitType):byte;
    procedure UpdateEater(aID:byte; aFoodKind:byte);
    procedure EatersGoesOut(aID:byte);
    function HasFood:boolean;
    function HasSpace:boolean;
    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Paint; override; //Render all eaters
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
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure ResAddToIn(aResource:TResourceType; const aCount:integer=1); override;
    procedure AddUnitToQueue(aUnit:TUnitType); //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(aID:integer); //Should remove unit from queue and shift rest up
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure UnitTrainingComplete; //This should shift queue filling rest with ut_None
    function GetTrainingProgress:byte;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

  {Barracks has 11 resources and Recruits}
  TKMHouseBarracks = class(TKMHouse)
  private
    ResourceCount:array[1..11]of word;
  public
    RecruitsList: TList;
    constructor Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    procedure AddMultiResource(aResource:TResourceType; const aCount:word=1);
    function CheckResIn(aResource:TResourceType):word; override;
    procedure ResTakeFromOut(aResource:TResourceType; const aCount:integer=1); override;
    function CanEquip(aUnitType: TUnitType):boolean;
    procedure Equip(aUnitType: TUnitType);
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

  {Storehouse keeps all the resources and flags for them}
  TKMHouseStore = class(TKMHouse)
  private
    ResourceCount:array[1..28]of word;
  public
    NotAcceptFlag:array[1..28]of boolean;
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure ToggleAcceptFlag(aRes:TResourceType);
    procedure AddMultiResource(aResource:TResourceType; const aCount:word=1);
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
    function Rem(aHouse:TKMHouse):boolean;
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
uses KM_UnitTaskSelfTrain, KM_DeliverQueue, KM_Terrain, KM_Render, KM_Units, KM_Units_Warrior, KM_PlayersCollection, KM_Sound, KM_Viewport, KM_Game, KM_TextLibrary, KM_UnitActionStay, KM_Player;


{ TKMHouse }
constructor TKMHouse.Create(aHouseType:THouseType; PosX,PosY:integer; aOwner:TPlayerIndex; aBuildState:THouseBuildState);
var i: byte;
begin
  Inherited Create;
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

  ID    := fGame.GetNewID;
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
  LoadStream.Read(FlagAnimStep, SizeOf(FlagAnimStep));
  LoadStream.Read(WorkAnimStep, SizeOf(WorkAnimStep));
  LoadStream.Read(fIsDestroyed);
  LoadStream.Read(RemoveRoadWhenDemolish);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fTimeSinceUnoccupiedReminder);
  LoadStream.Read(ID);
  LoadStream.Read(HasAct);
  if HasAct then begin
    fCurrentAction := THouseAction.Create(nil, hst_Empty); //Create placeholder to fill
    fCurrentAction.Load(LoadStream);
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
  fPlayers.Player[fOwner].FogOfWar.RevealCircle(fPosition, fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);

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
      fSoundLib.Play(sfx_HouseDestroy,GetPosition);
      
  //Dispose of delivery tasks performed in DeliverQueue unit
  fPlayers.Player[fOwner].DeliverList.RemoveOffer(Self);
  fPlayers.Player[fOwner].DeliverList.RemoveDemand(Self);
  fPlayers.Player[fOwner].BuildList.RemoveHouseRepair(Self);
  fPlayers.Player[fOwner].BuildList.RemoveHouse(Self);
  fTerrain.SetHouse(fPosition,fHouseType,hs_None,-1);
  //Road is removed in CloseHouse
  if not NoRubble then fTerrain.AddHouseRemainder(fPosition,fHouseType,fBuildState);

  CloseHouse(NoRubble);
end;


//Used by MapEditor
procedure TKMHouse.SetPosition(aPos:TKMPoint);
begin
  Assert(fGame.GameState=gsEditor);
  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  fTerrain.SetHouse(fPosition,fHouseType,hs_None,-1);
  fTerrain.RemRoad(GetEntrance);
  if fTerrain.CanPlaceHouse(aPos, GetHouseType, MyPlayer) then
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
end;


{Returns the closest cell of the house to aPos}
function TKMHouse.GetClosestCell(aPos:TKMPoint):TKMPoint;
var C:TKMPointList; i:integer;
begin
  C := TKMPointList.Create;
  try
    GetListOfCellsWithin(C);

    Result := C.List[1];
    for i:=2 to C.Count do
      if GetLength(C.List[i], aPos) < GetLength(Result, aPos) then
        Result := C.List[i];
  finally
    C.Free;
  end;
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

  procedure AddLoc(X,Y:word; Dir:TKMDirection);
  begin
    //First check that the passabilty is correct, as the house may be placed against blocked terrain
    if not fTerrain.CheckPassability(KMPoint(X,Y),aPassability) then exit;
    Cells.AddEntry(KMPointDir(KMPoint(X,Y),byte(Dir)));
  end;

begin

  Cells.Clearup;
  Loc := fPosition;

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
var i,k:integer; Loc:TKMPoint;
begin
  Cells.Clearup;
  Loc := fPosition;

  for i:=max(Loc.Y-3,1) to Loc.Y do for k:=max(Loc.X-2,1) to min(Loc.X+1,fTerrain.MapX) do
  if HouseArea[i-Loc.Y+4,k-Loc.X+3]<>0 then
    Cells.AddEntry(KMPoint(k,i));
end;


function TKMHouse.GetRandomCellWithin:TKMPoint;
var Cells:TKMPointList;
begin
  Cells := TKMPointList.Create;
  GetListOfCellsWithin(Cells);
  Result := Cells.GetRandom;
  Cells.Free;
end;


function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result:=false;
  if (X-fPosition.X+3 in [1..4])and(Y-fPosition.Y+4 in [1..4]) then
  if HouseArea[Y-fPosition.Y+4,X-fPosition.X+3]<>0 then begin
    Result:=true;
    exit;
  end;
end;


function TKMHouse.GetHealth:word;
begin
  Result := max(fBuildingProgress-fDamage, 0);
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

  if (fBuildState=hbs_Wood)and(fBuildingProgress = fResource.HouseDat[fHouseType].WoodCost*50) then
    fBuildState:=hbs_Stone;

  if (fBuildState=hbs_Stone)and(fBuildingProgress-fResource.HouseDat[fHouseType].WoodCost*50 = fResource.HouseDat[fHouseType].StoneCost*50) then
  begin
    fBuildState := hbs_Done;
    fPlayers.Player[fOwner].Stats.HouseEnded(fHouseType);
    Activate(true);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      fPlayers.Player[fOwner].BuildList.AddHouseRepair(Self);
  end;
end;


function TKMHouse.GetMaxHealth:word;
begin
  Result := fResource.HouseDat[fHouseType].WoodCost*50 + fResource.HouseDat[fHouseType].StoneCost*50;
end;


{Add damage to the house, positive number}
//Return TRUE if house was destroyed
function TKMHouse.AddDamage(aAmount:word):boolean;
begin
  Result := false;
  if IsDestroyed then
    Exit;

  if fBuildState < hbs_Wood then
  begin
    fPlayers.Player[fOwner].RemHouse(GetEntrance, true);
    Exit;
  end;

  fDamage := Math.min(fDamage + aAmount, GetMaxHealth);
  if (fBuildState = hbs_Done) and BuildingRepair then
    fPlayers.Player[fOwner].BuildList.AddHouseRepair(Self);

  if fBuildState = hbs_Done then
    UpdateDamage; //Only update fire if the house is complete

  if GetHealth = 0 then
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
  fDamage:= EnsureRange(fDamage - aAmount,0,High(Word));
  if (fDamage=0) then
    fPlayers.Player[fOwner].BuildList.RemoveHouseRepair(Self);
  UpdateDamage;
end;


{Update house damage animation}
procedure TKMHouse.UpdateDamage;
var Dmg: integer;
begin
  Dmg := GetMaxHealth div 8; //There are 8 fire places for each house, so the increment for each fire level is Max_Health / 8
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
  fCurrentAction.SetState(aState);
end;


function TKMHouse.GetState:THouseState;
begin
  Result := fCurrentAction.fHouseState;
end;


{How much resources house has in Input}
function TKMHouse.CheckResIn(aResource:TResourceType):word;
var i:integer;
begin
  Result:=0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResInput[i])or(aResource=rt_All) then
    inc(Result,fResourceIn[i]);
end;


{How much resources house has in Output}
function TKMHouse.CheckResOut(aResource:TResourceType):byte;
var i:integer;
begin
  Result:=0;
  for i:=1 to 4 do
  if (aResource = fResource.HouseDat[fHouseType].ResOutput[i])or(aResource=rt_All) then
    inc(Result,fResourceOut[i]);
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


{Check if house has enough resource supply to be built depending on it's state}
function TKMHouse.CheckResToBuild:boolean;
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
  if fResource.HouseDat[fHouseType].ResInput[1] = rt_All then
    TKMHouseStore(Self).AddMultiResource(aResource, aCount)
  else
  if fResource.HouseDat[fHouseType].ResInput[1] = rt_Warfare then
    TKMHouseBarracks(Self).AddMultiResource(aResource, aCount)
  else
    for i:=1 to 4 do
    if aResource = fResource.HouseDat[fHouseType].ResInput[i] then
      inc(fResourceIn[i],aCount);
end;


procedure TKMHouse.ResAddToOut(aResource:TResourceType; const aCount:integer=1);
var i:integer;
begin
  if aResource=rt_None then exit;
  for i:=1 to 4 do
  if aResource = fResource.HouseDat[fHouseType].ResOutput[i] then
    begin
      inc(fResourceOut[i],aCount);
      fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self,aResource,aCount);
    end;
end;


{Add resources to building process}
procedure TKMHouse.ResAddToBuild(aResource:TResourceType);
begin
  case aResource of
    rt_Wood: inc(fBuildSupplyWood);
    rt_Stone: inc(fBuildSupplyStone);
  else raise ELocError.Create('WIP house is not supposed to recieve '+TypeToString(aResource)+', right?', fPosition);
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
procedure TKMHouse.ResEditOrder(aID:byte; Amount:integer);
begin
  fResourceOrder[aID] := EnsureRange(fResourceOrder[aID]+Amount,0,MAX_ORDER);
end;


function TKMHouse.GetResDistribution(aID:byte):byte;
begin
  Result := fPlayers.Player[fOwner].Stats.GetRatio(fResource.HouseDat[fHouseType].ResInput[aID],fHouseType);
end;


procedure TKMHouse.MakeSound;
var WorkID,Step:byte;
begin
  //Do not play sounds if house is invisible to MyPlayer
  if MyPlayer.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then exit;
  if fCurrentAction = nil then exit; //no action means no sound ;)

  WorkID := fCurrentAction.GetWorkID;
  if WorkID=0 then exit;

  Step := fResource.HouseDat[fHouseType].Anim[WorkID].Count;
  if Step=0 then exit;

  //WatchTower has only 1 anim frame at Work2, hence it repeats the sound each frame
  //any value MOD 1 = 0
  //We override it by localy setting AnimCount to 100, so only first frame gets sound
  if fHouseType = ht_WatchTower then Step := 100;

  Step := WorkAnimStep mod Step;

  case fHouseType of //Various buildings and HouseActions producing sounds
    ht_School:        if (WorkID = 5)and(Step = 28) then fSoundLib.Play(sfx_SchoolDing,GetPosition); //Ding as the clock strikes 12
    ht_Mill:          if (WorkID = 2)and(Step = 0) then fSoundLib.Play(sfx_mill,GetPosition);
    ht_CoalMine:      if (WorkID = 1)and(Step = 5) then fSoundLib.Play(sfx_coalDown,GetPosition)
                      else if (WorkID = 1)and(Step = 24) then fSoundLib.Play(sfx_CoalMineThud,GetPosition,true,0.8)
                      else if (WorkID = 2)and(Step = 7) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 8) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4) //echo
                      else if (WorkID = 5)and(Step = 1) then fSoundLib.Play(sfx_coalDown,GetPosition);
    ht_IronMine:      if (WorkID = 2)and(Step = 7) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 8) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4); //echo
    ht_GoldMine:      if (WorkID = 2)and(Step = 5) then fSoundLib.Play(sfx_mine,GetPosition)
                      else if (WorkID = 2)and(Step = 6) then fSoundLib.Play(sfx_mine,GetPosition,true,0.4); //echo
    ht_Sawmill:       if (WorkID = 2)and(Step = 1) then fSoundLib.Play(sfx_saw,GetPosition);
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
    //ht_WatchTower:  Handled by projectile itself 
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
  SaveStream.Write(FlagAnimStep, SizeOf(FlagAnimStep));
  SaveStream.Write(WorkAnimStep, SizeOf(WorkAnimStep));
  SaveStream.Write(fIsDestroyed);
  SaveStream.Write(RemoveRoadWhenDemolish);
  SaveStream.Write(fPointerCount);
  SaveStream.Write(fTimeSinceUnoccupiedReminder);
  SaveStream.Write(ID);
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
    fPlayers.Player[fOwner].FogOfWar.RevealCircle(fPosition,fResource.HouseDat[fHouseType].Sight, FOG_OF_WAR_INC);
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
    hbs_Glyph: fRender.RenderHouseBuild(fHouseType, fPosition);
    hbs_NoGlyph:; //Nothing
    hbs_Wood:
      begin
        fRender.RenderHouseWood(fHouseType,
        fBuildingProgress/50/fResource.HouseDat[fHouseType].WoodCost, //0...1 range
        fPosition);
        fRender.RenderHouseBuildSupply(fHouseType, fBuildSupplyWood, fBuildSupplyStone, fPosition);
      end;
    hbs_Stone:
      begin
        fRender.RenderHouseStone(fHouseType,
        (fBuildingProgress/50-fResource.HouseDat[fHouseType].WoodCost)/fResource.HouseDat[fHouseType].StoneCost, //0...1 range
        fPosition);
        fRender.RenderHouseBuildSupply(fHouseType, fBuildSupplyWood, fBuildSupplyStone, fPosition);
      end;
    else begin
      fRender.RenderHouseStone(fHouseType,1,fPosition);
      fRender.RenderHouseSupply(fHouseType,fResourceIn,fResourceOut,fPosition);
      if fCurrentAction<>nil then
        fRender.RenderHouseWork(fHouseType,integer(fCurrentAction.fSubAction),WorkAnimStep,fPosition,fPlayers.Player[fOwner].FlagColor);
    end;
  end;
end;

procedure TKMHouse.SetWareDelivery(aVal:boolean);
begin
  fWareDelivery := aVal;
end;


function TKMHouse.HouseArea: THouseArea;
begin
  Result := fResource.HouseDat[fHouseType].BuildArea;
end;


//@Lewin: I'm not sure if we should route some properties from ResourceHouseDat in following way instead of accessing them directly in-place
function TKMHouse.DoesOrders: boolean;
begin
  Result := fResource.HouseDat[fHouseType].DoesOrders;
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
  inc(BeastAge[Random(5)+1]); //Let's hope it never overflows MAX
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
                            integer(fCurrentAction.fSubAction * [ha_Work1, ha_Work2, ha_Work3, ha_Work4, ha_Work5]),
                            WorkAnimStep,fPosition,fPlayers.Player[fOwner].FlagColor);
end;


{ TKMHouseInn }
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
var i:integer; UnitType,AnimDir:byte; AnimStep:cardinal;
begin
  Inherited;
  if (fBuildState<>hbs_Done) then exit;

  for i:=low(Eater) to high(Eater) do
  if (Eater[i].UnitType<>ut_None) and (Eater[i].FoodKind<>0) then
  begin
    UnitType := byte(Eater[i].UnitType);
    AnimDir  := Eater[i].FoodKind*2 - 1 + ((i-1) div 3);
    AnimStep := FlagAnimStep-Eater[i].EatStep; //Delta is our AnimStep

    fRender.RenderUnit(UnitType, byte(ua_Eat), AnimDir, AnimStep,
      fPosition.X+OffX[(i-1) mod 3 +1],
      fPosition.Y+OffY[(i-1) mod 3 +1],
      fPlayers.Player[fOwner].FlagColor, false);
  end;
end;


{ TKMHouseSchool }
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


function TKMHouseSchool.GetTrainingProgress:byte;
begin
  Result:=0;
  if UnitWIP=nil then exit;
  Result:=EnsureRange(round(
  ((fCurrentAction.GetWorkID-1)*30+30-TUnitActionStay(TKMUnit(UnitWIP).GetUnitAction).HowLongLeftToStay)
  /1.5),0,100); //150 steps into 0..100 range
  //Substeps could be asked from Unit.ActionStay.TimeToStay, but it's a private field now
end;


procedure TKMHouseSchool.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if TKMUnit(UnitWIP) <> nil then
    SaveStream.Write(TKMUnit(UnitWIP).ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(HideOneGold);
  SaveStream.Write(UnitTrainProgress);
  SaveStream.Write(UnitQueue, SizeOf(UnitQueue));
end;


{ TKMHouseStore }
constructor TKMHouseStore.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


procedure TKMHouseStore.AddMultiResource(aResource:TResourceType; const aCount:word=1);
var i:integer;
begin
  case aResource of
    rt_All:     for i:=1 to length(ResourceCount) do begin
                  ResourceCount[i] := EnsureRange(ResourceCount[i]+aCount,0,High(Word));
                  fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self,TResourceType(i),aCount);
                end;
    rt_Trunk..
    rt_Fish:    begin
                  ResourceCount[byte(aResource)]:=EnsureRange(ResourceCount[byte(aResource)]+aCount,0,High(Word));
                  fPlayers.Player[fOwner].DeliverList.AddNewOffer(Self,aResource,aCount);
                end;
    else        raise ELocError.Create('Cant''t add '+TypeToString(aResource), GetPosition);
  end;
end;


function TKMHouseStore.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [rt_Trunk..rt_Fish] then
    Result := ResourceCount[byte(aResource)]
  else
    Result := 0;
end;


procedure TKMHouseStore.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
begin
  Assert(aCount <= ResourceCount[byte(aResource)]);

  dec(ResourceCount[byte(aResource)], aCount);
end;


procedure TKMHouseStore.ToggleAcceptFlag(aRes:TResourceType);
var i:integer; ApplyCheat:boolean;
begin
  Assert(aRes in [rt_Trunk .. rt_Fish]); //Dunno why thats happening sometimes..

  if CHEATS_ENABLED then begin
    ApplyCheat := true;

    for i:=1 to length(ResourceCount) do
      ApplyCheat := ApplyCheat and (NotAcceptFlag[i] = boolean(CheatStorePattern[i]));

    if ApplyCheat and (aRes = rt_Arbalet) then begin
      AddMultiResource(rt_All, 10);
      exit;
    end;
    if ApplyCheat and (aRes = rt_Horse) then begin
      fGame.RequestGameHold(gr_Win);
      exit;
    end;
    if ApplyCheat and (aRes = rt_Fish) then begin
      fGame.RequestGameHold(gr_Defeat);
      exit;
    end;
  end;

  NotAcceptFlag[byte(aRes)] := not NotAcceptFlag[byte(aRes)];
end;


procedure TKMHouseStore.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


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


procedure TKMHouseBarracks.AddMultiResource(aResource:TResourceType; const aCount:word=1);
var i:integer;
begin
  case aResource of
    rt_Warfare: for i:=1 to length(ResourceCount) do
                ResourceCount[i] := EnsureRange(ResourceCount[i]+aCount,0,High(Word));
    rt_Shield..
    rt_Horse:   ResourceCount[byte(aResource)-16]:=EnsureRange(ResourceCount[byte(aResource)-16]+aCount,0,High(Word))
    else        raise ELocError.Create('Cant''t add '+TypeToString(aResource), GetPosition);
  end;
end;


function TKMHouseBarracks.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [rt_Shield..rt_Horse] then
    Result:=ResourceCount[byte(aResource)-16]
  else
    Result:=0;
end;


procedure TKMHouseBarracks.ResTakeFromOut(aResource:TResourceType; const aCount:integer=1);
begin
  Assert(aResource in [rt_Shield..rt_Horse]);
  Assert(aCount <= ResourceCount[byte(aResource)-16]);
  dec(ResourceCount[byte(aResource)-16], aCount);
end;


function TKMHouseBarracks.CanEquip(aUnitType: TUnitType):boolean;
var i:integer;
begin
  Result := RecruitsList.Count > 0; //Can't equip anything without recruits

  for i:=1 to 4 do
  if TroopCost[aUnitType,i]<>0 then //Can't equip if we don't have a required resource
    Result := Result and (ResourceCount[TroopCost[aUnitType,i]] > 0);
end;


procedure TKMHouseBarracks.Equip(aUnitType: TUnitType);
var i:integer;
    Soldier:TKMUnitWarrior;
    LinkUnit:TKMUnitWarrior;
begin
  //Equip a new soldier and make him walk out of the house
  //First make sure unit is valid and we have resources to equip him
  if (not (aUnitType in [ut_Militia..ut_Barbarian])) or (not CanEquip(aUnitType)) then exit;

  //Take resources
  for i:=1 to 4 do
    if TroopCost[aUnitType,i]<>0 then
      dec(ResourceCount[TroopCost[aUnitType,i]]);

  TKMUnitRecruit(RecruitsList.Items[0]).DestroyInBarracks; //Special way to kill the unit because it is in a house
  RecruitsList.Delete(0); //Delete first recruit in the list

  //Make new unit
  Soldier := TKMUnitWarrior(fPlayers.Player[fOwner].AddUnit(aUnitType,GetEntrance,false,true));
  fTerrain.UnitRem(GetEntrance); //Adding a unit automatically sets IsUnit, but as the unit is inside for this case we don't want that
  Soldier.Visible := false; //Make him invisible as he is inside the barracks
  Soldier.Condition := Round(TROOPS_TRAINED_CONDITION*UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
  Soldier.OrderLocDir := KMPointDir(KMPointY1(GetEntrance),0); //Position in front of the barracks facing north
  Soldier.SetActionGoIn(ua_Walk, gd_GoOutside, Self);

  //AI do not need auto linking, they manage linking themselves
  if fPlayers.Player[fOwner].PlayerType = pt_Human then
    LinkUnit := Soldier.FindLinkUnit(GetEntrance)
  else LinkUnit := nil;
  if LinkUnit <> nil then
    Soldier.OrderLinkTo(LinkUnit);
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
  SetState(aHouseState);
end;


procedure THouseAction.SetState(aHouseState: THouseState);
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


function THouseAction.GetWorkID:byte;
begin
  if ha_Work1 in fSubAction then Result := 1 else
  if ha_Work2 in fSubAction then Result := 2 else
  if ha_Work3 in fSubAction then Result := 3 else
  if ha_Work4 in fSubAction then Result := 4 else
  if ha_Work5 in fSubAction then Result := 5 else
    Result := 0;
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
    SaveStream.Write(Zero);
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
      fRender.RenderDebugQuad(GetPosition.X+k,GetPosition.Y+i); 
end;


{ TKMHousesCollection }
function TKMHousesCollection.AddToCollection(aHouseType: THouseType; PosX,PosY:integer; aOwner: shortint; aHBS:THouseBuildState):TKMHouse;
var T:integer;
begin
  case aHouseType of
    ht_Swine:    T := Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Stables:  T := Inherited Add(TKMHouseSwineStable.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Inn:      T := Inherited Add(TKMHouseInn.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_School:   T := Inherited Add(TKMHouseSchool.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Barracks: T := Inherited Add(TKMHouseBarracks.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_Store:    T := Inherited Add(TKMHouseStore.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    ht_WatchTower: T := Inherited Add(TKMHouseTower.Create(aHouseType,PosX,PosY,aOwner,aHBS));
    else         T := Inherited Add(TKMHouse.Create(aHouseType,PosX,PosY,aOwner,aHBS));
  end;
    if T=-1 then Result := nil else Result := Items[T];
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


function TKMHousesCollection.Rem(aHouse:TKMHouse):boolean;
begin
  Remove(aHouse);
  Result := true;
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
      if Houses[i].GetHouseType = ht_Barracks then Dist:=(Dist*1000) + (TKMHouseBarracks(Houses[i]).RecruitsList.Count*10000);

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
  if (fSelectedHouse <> nil) and not fGame.MultiplayerMode then //Multiplayer saves must be identical
    SaveStream.Write(fSelectedHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(Count);
  for i := 0 to Count - 1 do
    Houses[i].Save(SaveStream);
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
    LoadStream.Seek(-SizeOf(HouseType), soFromCurrent); //rewind
    case HouseType of //Create some placeholder unit
      ht_Swine:    Inherited Add(TKMHouseSwineStable.Load(LoadStream));
      ht_Stables:  Inherited Add(TKMHouseSwineStable.Load(LoadStream));
      ht_Inn:      Inherited Add(TKMHouseInn.Load(LoadStream));
      ht_School:   Inherited Add(TKMHouseSchool.Load(LoadStream));
      ht_Barracks: Inherited Add(TKMHouseBarracks.Load(LoadStream));
      ht_Store:    Inherited Add(TKMHouseStore.Load(LoadStream));
      else         Inherited Add(TKMHouse.Load(LoadStream));
//    else Assert(false, 'Uknown house type in Savegame')
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
    if FREE_POINTERS and (Houses[i].GetPointerCount = 0) then
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
  Result:=0;
  for i:=0 to Count-1 do
    Result:=Result+Houses[i].GetPointerCount;
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
