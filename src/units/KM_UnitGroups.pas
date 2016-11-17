unit KM_UnitGroups;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, Types,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_Points, KM_Houses, KM_Units,
  KM_Units_Warrior;

type
  TKMUnitGroup = class;
  TKMUnitGroupEvent = procedure(aGroup: TKMUnitGroup) of object;
  TKMTurnDirection = (tdNone, tdCW, tdCCW);
  TKMInitialOrder = (ioNoOrder, ioSendGroup, ioAttackPosition);

  TKMGroupOrder = (
    goNone,         //Last order was executed and now we have nothing to do
    goWalkTo,       //Ordered to walk somewhere or just change formation
    goAttackHouse,  //Attack house
    goAttackUnit,   //Attack specific unit
    goStorm         //Run forward
    );

  //MapEd allows to set order for a group that will be executed on mission start
  TKMMapEdOrder = record
    Order: TKMInitialOrder;
    Pos: TKMPointDir;
  end;

  //Group of warriors
  TKMUnitGroup = class
  private
    fUID: Integer;
    fPointerCount: Cardinal;
    fTicker: Cardinal;
    fTargetFollowTicker: Cardinal;
    fOwner: TKMHandIndex;
    fMembers: TList;
    fOffenders: TList;
    fSelected: TKMUnitWarrior; //Unit selected by player in GUI. Should not be saved or affect game logic for MP consistency.
    fUnitsPerRow: Word;
    fTimeSinceHungryReminder: Integer;
    fGroupType: TGroupType;
    fDisableHungerMessage: Boolean;
    fBlockOrders: Boolean;

    fOrder: TKMGroupOrder; //Remember last order incase we need to repeat it (e.g. to joined members)
    fOrderLoc: TKMPointDir; //Dir is the direction to face after order

    //Avoid accessing these directly
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetGroup: TKMUnitGroup; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use public OrderHouseTarget instead.

    fMapEdCount: Word;

    function GetCount: Integer;
    function GetMember(aIndex: Integer): TKMUnitWarrior;
    function GetNearestMember(aUnit: TKMUnitWarrior): Integer; overload;
    function GetNearestMember(aLoc: TKMPoint): TKMUnitWarrior; overload;
    function GetMemberLoc(aIndex: Integer): TKMPointExact;
    procedure SetMapEdCount(aCount: Word);
    procedure SetUnitsPerRow(aCount: Word);
    procedure SetDirection(Value: TKMDirection);
    procedure SetCondition(aValue: Integer);
    procedure SetPosition(aValue: TKMPoint);
    procedure ClearOrderTarget;
    procedure ClearOffenders;
    procedure HungarianReorderMembers;

    function GetOrderTargetUnit: TKMUnit;
    function GetOrderTargetGroup: TKMUnitGroup;
    function GetOrderTargetHouse: TKMHouse;
    procedure SetOrderTargetUnit(aUnit: TKMUnit);
    procedure SetOrderTargetHouse(aHouse: TKMHouse);
    procedure UpdateOrderTargets;

    procedure CheckForFight;
    procedure CheckOrderDone;
    procedure UpdateHungerMessage;

    procedure Member_Died(aMember: TKMUnitWarrior);
    procedure Member_PickedFight(aMember: TKMUnitWarrior; aEnemy: TKMUnit);

    function GetCondition: Integer;
    function GetDirection: TKMDirection;
    function GetPosition: TKMPoint;
    procedure SetSelected(aValue: TKMUnitWarrior);
  public
    //Each group can have initial order
    //SendGroup - walk to some location
    //AttackPosition - attack something at position (or walk there if its empty)
    MapEdOrder: TKMMapEdOrder;
    OnGroupDied: TKMUnitGroupEvent;

    constructor Create(aID: Cardinal; aCreator: TKMUnitWarrior); overload;
    constructor Create(aID: Cardinal; aOwner: TKMHandIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aCount: Word); overload;
    constructor Create(LoadStream: TKMemoryStream); overload;
    procedure SyncLoad;
    procedure Save(SaveStream: TKMemoryStream);
    destructor Destroy; override;

    function GetGroupPointer: TKMUnitGroup;
    procedure ReleaseGroupPointer;
    procedure AddMember(aWarrior: TKMUnitWarrior; aIndex: Integer = -1);
    function MemberByUID(aUID: Integer): TKMUnitWarrior;
    function HitTest(X,Y: Integer): Boolean;
    procedure SelectFlagBearer;
    function HasMember(aWarrior: TKMUnit): Boolean;
    procedure ResetAnimStep;
    function InFight(aCountCitizens: Boolean = False): Boolean; //Fighting and can't take any orders from player
    function IsAttackingHouse: Boolean; //Attacking house
    function IsAttackingUnit: Boolean;
    function IsIdleToAI(aAllowWalking: Boolean = False): Boolean;
    function IsPositioned(aLoc: TKMPoint; Dir: TKMDirection): Boolean;
    function CanTakeOrders: Boolean;
    function CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean;
    function FightMaxRange: Single;
    function IsRanged: Boolean;
    function IsDead: Boolean;
    function UnitType: TUnitType;
    function GetOrderText: UnicodeString;
    property GroupType: TGroupType read fGroupType;
    property UID: Integer read fUID;
    property Count: Integer read GetCount;
    property MapEdCount: Word read fMapEdCount write SetMapEdCount;
    property Members[aIndex: Integer]: TKMUnitWarrior read GetMember;
    property Owner: TKMHandIndex read fOwner;
    property Position: TKMPoint read GetPosition write SetPosition;
    property Direction: TKMDirection read GetDirection write SetDirection;
    property UnitsPerRow: Word read fUnitsPerRow write SetUnitsPerRow;
    property SelectedUnit: TKMUnitWarrior read fSelected write SetSelected;
    property Condition: Integer read GetCondition write SetCondition;
    property Order: TKMGroupOrder read fOrder;
    property DisableHungerMessage: Boolean read fDisableHungerMessage write fDisableHungerMessage;
    property BlockOrders: Boolean read fBlockOrders write fBlockOrders;

    property OrderTargetUnit: TKMUnit read GetOrderTargetUnit write SetOrderTargetUnit;
    property OrderTargetGroup: TKMUnitGroup read GetOrderTargetGroup;
    property OrderTargetHouse: TKMHouse read GetOrderTargetHouse write SetOrderTargetHouse;

    procedure OrderAttackHouse(aHouse: TKMHouse; aClearOffenders: Boolean);
    procedure OrderAttackUnit(aUnit: TKMUnit; aClearOffenders: Boolean);
    procedure OrderFood(aClearOffenders: Boolean; aHungryOnly: Boolean = False);
    procedure OrderFormation(aTurnAmount: TKMTurnDirection; aColumnsChange: ShortInt; aClearOffenders: Boolean);
    procedure OrderHalt(aClearOffenders: Boolean);
    procedure OrderLinkTo(aTargetGroup: TKMUnitGroup; aClearOffenders: Boolean);
    procedure OrderNone;
    procedure OrderRepeat;
    function OrderSplit(aClearOffenders: Boolean; aSplitSingle: Boolean = False): TKMUnitGroup;
    function OrderSplitUnit(aUnit: TKMUnit; aClearOffenders: Boolean): TKMUnitGroup;
    procedure OrderSplitLinkTo(aGroup: TKMUnitGroup; aCount: Word; aClearOffenders: Boolean);
    procedure OrderStorm(aClearOffenders: Boolean);
    procedure OrderWalk(aLoc: TKMPoint; aClearOffenders: Boolean; aDir: TKMDirection = dir_NA);

    procedure UpdateState;
    procedure Paint;
  end;


  //Collection of Groups
  TKMUnitGroups = class
  private
    fGroups: TKMList;

    function GetCount: Integer;
    function GetGroup(aIndex: Integer): TKMUnitGroup;
  public
    constructor Create;
    destructor Destroy; override;

    function AddGroup(aWarrior: TKMUnitWarrior): TKMUnitGroup; overload;
    function AddGroup(aOwner: TKMHandIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aCount: Word): TKMUnitGroup; overload;
    procedure RemGroup(aGroup: TKMUnitGroup);

    property Count: Integer read GetCount;
    property Groups[aIndex: Integer]: TKMUnitGroup read GetGroup; default;
    function GetGroupByUID(aUID: Integer): TKMUnitGroup;
    function GetGroupByMember(aUnit: TKMUnitWarrior): TKMUnitGroup;
    function HitTest(X,Y: Integer): TKMUnitGroup;
    function GetClosestGroup(aPoint: TKMPoint; aTypes: TGroupTypeSet = [Low(TGroupType)..High(TGroupType)]): TKMUnitGroup;

    function WarriorTrained(aUnit: TKMUnitWarrior): TKMUnitGroup;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint(aRect: TKMRect);
  end;


implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_Utils, KM_ResTexts, KM_RenderPool,
  KM_Hungarian, KM_UnitActionWalkTo, KM_PerfLog, KM_AI, KM_ResUnits, KM_ScriptingEvents,
  KM_UnitActionStormAttack;


const
  HUNGER_CHECK_FREQ = 10; //Check warrior hunger every 1 second


{ TKMUnitGroup }
//Create a Group from a single warrior (short version)
constructor TKMUnitGroup.Create(aID: Cardinal; aCreator: TKMUnitWarrior);
begin
  inherited Create;

  fUID := aID;
  fOwner := aCreator.Owner;
  fGroupType := UnitGroups[aCreator.UnitType];
  fMembers := TList.Create;
  fOffenders := TList.Create;

  //So when they click Halt for the first time it knows where to place them
  fOrderLoc := KMPointDir(aCreator.GetPosition.X, aCreator.GetPosition.Y, aCreator.Direction);

  AddMember(aCreator);
  UnitsPerRow := 1;
end;


//Create a Group from script (creates all the warriors as well)
constructor TKMUnitGroup.Create(aID: Cardinal; aOwner: TKMHandIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aCount: Word);
var
  Warrior: TKMUnitWarrior;
  I: Integer;
  DoesFit: Boolean;
  UnitLoc: TKMPoint;
  NewCondition: Word;
  DesiredArea: Byte;
begin
  inherited Create;

  fUID := aID;
  fOwner := aOwner;
  fGroupType := UnitGroups[aUnitType];
  fMembers := TList.Create;
  fOffenders := TList.Create;

  //So when they click Halt for the first time it knows where to place them
  fOrderLoc := KMPointDir(PosX, PosY, aDir);

  //Whole group should have the same condition
  NewCondition := Round(UNIT_MAX_CONDITION * (UNIT_CONDITION_BASE + KaMRandomS(UNIT_CONDITION_RANDOM)));

  if gGame.IsMapEditor then
  begin
    //In MapEd we create only flagholder, other members are virtual
    Warrior := TKMUnitWarrior(gHands[aOwner].AddUnit(aUnitType, KMPoint(PosX, PosY), False));
    if Warrior <> nil then
    begin
      Warrior.Direction := aDir;
      Warrior.AnimStep  := UnitStillFrames[aDir];
      AddMember(Warrior);
      Warrior.Condition := UNIT_MAX_CONDITION div 2; //Half-fed
      fMapEdCount := aCount;
    end;
  end
  else
  begin
    //We want all of the Group memmbers to be placed in one area
    DesiredArea := gTerrain.GetWalkConnectID(KMPoint(PosX, PosY));
    for I := 0 to aCount - 1 do
    begin
      UnitLoc := GetPositionInGroup2(PosX, PosY, aDir, I, aUnitPerRow, gTerrain.MapX, gTerrain.MapY, DoesFit);
      if not DoesFit then Continue;

      Warrior := TKMUnitWarrior(gHands[aOwner].AddUnit(aUnitType, UnitLoc, True, DesiredArea));
      if Warrior = nil then Continue;

      Warrior.Direction := aDir;
      Warrior.AnimStep  := UnitStillFrames[aDir];
      AddMember(Warrior);
      Warrior.Condition := NewCondition;
    end;
  end;

  //We could not set it earlier cos it's limited by Count
  UnitsPerRow := aUnitPerRow;
end;


//Load the Group from savegame
constructor TKMUnitGroup.Create(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
  W: TKMUnitWarrior;
begin
  inherited Create;
  fMembers := TList.Create;
  fOffenders := TList.Create;

  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fUID);
  LoadStream.Read(fOwner);
  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(W, 4); //subst on syncload
    fMembers.Add(W);
  end;

  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(W, 4); //subst on syncload
    fOffenders.Add(W);
  end;

  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fOrderLoc);
  LoadStream.Read(fOrderTargetGroup, 4); //subst on syncload
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fTicker);
  LoadStream.Read(fTargetFollowTicker);
  LoadStream.Read(fTimeSinceHungryReminder);
  LoadStream.Read(fUnitsPerRow);
  LoadStream.Read(fDisableHungerMessage);
  Loadstream.Read(fBlockOrders);
end;


procedure TKMUnitGroup.SyncLoad;
var I: Integer;
begin
  inherited;

  //Assign event handlers after load
  for I := 0 to Count - 1 do
  begin
    fMembers[I] := TKMUnitWarrior(gHands.GetUnitByUID(Cardinal(fMembers[I])));
    Members[I].OnWarriorDied := Member_Died;
    Members[I].OnPickedFight := Member_PickedFight;
  end;

  for I := 0 to fOffenders.Count - 1 do
    fOffenders[I] := TKMUnitWarrior(gHands.GetUnitByUID(Cardinal(TKMUnitWarrior(fOffenders[I]))));

  fOrderTargetGroup := gHands.GetGroupByUID(Cardinal(fOrderTargetGroup));
  fOrderTargetHouse := gHands.GetHouseByUID(Cardinal(fOrderTargetHouse));
  fOrderTargetUnit  := gHands.GetUnitByUID(Cardinal(fOrderTargetUnit));
end;


destructor TKMUnitGroup.Destroy;
begin
  //We don't release unit pointers from fMembers, because the group is only destroyed when fMembers.Count = 0
  //or when the game is canceled (then it doesn't matter)
  fMembers.Free;

  //We need to release offenders pointers
  ClearOffenders;
  fOffenders.Free;

  ClearOrderTarget; //Free pointers

  inherited;
end;


function TKMUnitGroup.FightMaxRange: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  if Members[I].GetFightMaxRange > Result then
    Result := Members[I].GetFightMaxRange;
end;


procedure TKMUnitGroup.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  inherited;
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  SaveStream.Write(fUID);
  SaveStream.Write(fOwner);
  SaveStream.Write(fMembers.Count);
  for I := 0 to fMembers.Count - 1 do
    SaveStream.Write(Members[I].UID);
  SaveStream.Write(fOffenders.Count);
  for I := 0 to fOffenders.Count - 1 do
    SaveStream.Write(TKMUnitWarrior(fOffenders[I]).UID);
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fOrderLoc);
  if fOrderTargetGroup <> nil then
    SaveStream.Write(fOrderTargetGroup.UID)
  else
    SaveStream.Write(Integer(0));
  if fOrderTargetHouse <> nil then
    SaveStream.Write(fOrderTargetHouse.UID)
  else
    SaveStream.Write(Integer(0));
  if fOrderTargetUnit <> nil then
    SaveStream.Write(fOrderTargetUnit.UID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fPointerCount);
  SaveStream.Write(fTicker);
  SaveStream.Write(fTargetFollowTicker);
  SaveStream.Write(fTimeSinceHungryReminder);
  SaveStream.Write(fUnitsPerRow);
  SaveStream.Write(fDisableHungerMessage);
  SaveStream.Write(fBlockOrders);
end;


//Group condition is the Min from all members (so that AI feeds the Group when needed)
function TKMUnitGroup.GetCondition: Integer;
var
  I: Integer;
begin
  Result := UNIT_MAX_CONDITION; //Assign const incase Count=0
  for I := 0 to Count - 1 do
    Result := Min(Result, Members[I].Condition);
end;


function TKMUnitGroup.GetCount: Integer;
begin
  Result := fMembers.Count;
end;


function TKMUnitGroup.GetDirection: TKMDirection;
begin
  Result := fOrderLoc.Dir;
end;


function TKMUnitGroup.GetMember(aIndex: Integer): TKMUnitWarrior;
begin
  Result := fMembers.Items[aIndex];
end;


//Get member order location within formation
function TKMUnitGroup.GetMemberLoc(aIndex: Integer): TKMPointExact;
begin
  //Allow off map positions so GetClosestTile works properly
  Result.Loc := GetPositionInGroup2(fOrderLoc.Loc.X, fOrderLoc.Loc.Y,
                                    fOrderLoc.Dir, aIndex, fUnitsPerRow,
                                    gTerrain.MapX, gTerrain.MapY,
                                    Result.Exact);
  //Fits on map and is on passable terrain
  Result.Exact := Result.Exact and gTerrain.CheckPassability(Result.Loc, tpWalk);
end;


function TKMUnitGroup.GetNearestMember(aUnit: TKMUnitWarrior): Integer;
var
  I: Integer;
  Dist, Best: Single;
begin
  Result := -1;
  Best := MaxSingle;
  for I := 0 to Count - 1 do
  if (Members[I] <> aUnit) and not Members[I].IsDeadOrDying then
  begin
    Dist := KMLengthSqr(aUnit.GetPosition, Members[I].GetPosition);
    if Dist < Best then
    begin
      Best := Dist;
      Result := I;
    end;
  end;
end;


function TKMUnitGroup.GetNearestMember(aLoc: TKMPoint): TKMUnitWarrior;
var
  I: Integer;
  Dist, Best: Single;
begin
  Result := nil;
  Best := MaxSingle;
  for I := 0 to Count - 1 do
  if not Members[I].IsDeadOrDying then
  begin
    Dist := KMLengthSqr(aLoc, Members[I].GetPosition);
    if Dist < Best then
    begin
      Best := Dist;
      Result := Members[I];
    end;
  end;
end;


//Returns self and adds on to the pointer counter
function TKMUnitGroup.GetGroupPointer: TKMUnitGroup;
begin
  Inc(fPointerCount);
  Result := Self;
end;


//Decreases the pointer counter
//Should be used only by gHands for clarity sake
procedure TKMUnitGroup.ReleaseGroupPointer;
begin
  if fPointerCount < 1 then
    raise ELocError.Create('Group remove pointer', Position);
  Dec(fPointerCount);
end;


//Get current groups location (we use flagholder)
function TKMUnitGroup.GetPosition: TKMPoint;
begin
  if not IsDead then
    Result := Members[0].GetPosition
  else
    Result := KMPoint(0,0);
end;


procedure TKMUnitGroup.SetPosition(aValue: TKMPoint);
begin
  Assert(gGame.IsMapEditor);
  Members[0].SetPosition(aValue);
  fOrderLoc.Loc := Members[0].GetPosition; //Don't assume we can move to aValue
end;


procedure TKMUnitGroup.SetSelected(aValue: TKMUnitWarrior);
begin
  Assert(HasMember(aValue), 'Cant''t select unit that is not a groups member');
  fSelected := aValue;
end;


procedure TKMUnitGroup.SetCondition(aValue: Integer);
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Members[I].Condition := aValue;
end;


procedure TKMUnitGroup.SetDirection(Value: TKMDirection);
begin
  Assert(gGame.IsMapEditor);
  fOrderLoc.Dir := Value;
  Members[0].Direction := Value;
end;


procedure TKMUnitGroup.SetMapEdCount(aCount: Word);
begin
  fMapEdCount := aCount;

  // Ensure that fUnitsPerRow is valid (less than or equal to fMapEdCount)
  SetUnitsPerRow(fUnitsPerRow);
end;


procedure TKMUnitGroup.SetUnitsPerRow(aCount: Word);
begin
  if gGame.IsMapEditor then
    fUnitsPerRow := EnsureRange(aCount, 1, fMapEdCount)
  else
    fUnitsPerRow := EnsureRange(aCount, 1, Count);
end;


procedure TKMUnitGroup.AddMember(aWarrior: TKMUnitWarrior; aIndex: Integer = -1);
begin
  Assert(fMembers.IndexOf(aWarrior) = -1, 'We already have this Warrior in group');
  if aIndex <> -1 then
    fMembers.Insert(aIndex, aWarrior.GetUnitPointer)
  else
    fMembers.Add(aWarrior.GetUnitPointer);

  //Member reports to Group if something happens to him, so that Group can apply its logic
  aWarrior.OnPickedFight := Member_PickedFight;
  aWarrior.OnWarriorDied := Member_Died;
end;


function TKMUnitGroup.HasMember(aWarrior: TKMUnit): Boolean;
begin
  Result := fMembers.IndexOf(aWarrior) <> -1;
end;


//Used by the MapEd after changing direction (so warriors are frozen on the right frame)
procedure TKMUnitGroup.ResetAnimStep;
begin
  Assert(gGame.IsMapEditor);
  Members[0].AnimStep := UnitStillFrames[Members[0].Direction];
end;


//If the player is allowed to issue orders to group
function TKMUnitGroup.CanTakeOrders: Boolean;
begin
  Result := (IsRanged or not InFight) and (not fBlockOrders);
end;


function TKMUnitGroup.CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := (Count > 0) and Members[0].CanWalkTo(aTo, aDistance);
end;


//Group is dead, but still exists cos of pointers to it
function TKMUnitGroup.IsDead: Boolean;
begin
  Result := (Count = 0);
end;


function TKMUnitGroup.IsRanged: Boolean;
begin
  Result := (fGroupType = gt_Ranged);
end;


//Member reports that he has died (or been killed)
procedure TKMUnitGroup.Member_Died(aMember: TKMUnitWarrior);
var
  I: Integer;
  NewSel: Integer;
begin
  I := fMembers.IndexOf(aMember);
  Assert(I <> -1, 'No such member');

  if (aMember = fSelected) then
  begin
    fSelected := nil;

    //Transfer selection to nearest member
    NewSel := GetNearestMember(aMember);
    if NewSel <> -1 then
      fSelected := Members[NewSel];
  end;

  fMembers.Delete(I);

  //Move nearest member to placeholders place
  if I = 0 then
  begin
    NewSel := GetNearestMember(aMember);
    if NewSel <> -1 then
      fMembers.Exchange(NewSel, 0);
  end;

  gHands.CleanUpUnitPointer(TKMUnit(aMember));

  SetUnitsPerRow(fUnitsPerRow);

  //If Group has died report to owner
  if IsDead and Assigned(OnGroupDied) then
    OnGroupDied(Self);

  //Only repeat the order if we are not in a fight (since bowmen can still take orders when fighting)
  if not IsDead and CanTakeOrders and not InFight then
    OrderRepeat;
end;


//Member got in a fight
//Remember who we are fighting with, to guide idle units to
//This only works for melee offenders(?)
procedure TKMUnitGroup.Member_PickedFight(aMember: TKMUnitWarrior; aEnemy: TKMUnit);
begin
  if (aEnemy is TKMUnitWarrior) then
    fOffenders.Add(aEnemy.GetUnitPointer);
end;


//If we picked up a fight, while doing any other order - manage it here
procedure TKMUnitGroup.CheckForFight;
var
  I,K: Integer;
  U: TKMUnit;
  FightWasOrdered: Boolean;
begin
  //Verify we still have foes
  for I := fOffenders.Count - 1 downto 0 do
  if TKMUnitWarrior(fOffenders[I]).IsDeadOrDying then
  begin
    U := fOffenders[I]; //Need to pass var
    gHands.CleanUpUnitPointer(U);
    fOffenders.Delete(I);
    if fOffenders.Count = 0 then
      OrderRepeat;
  end;

  //Fight is over
  if fOffenders.Count = 0 then Exit;

  if IsRanged then
  begin
    FightWasOrdered := False;
    for I := 0 to Count - 1 do
      if not Members[I].InFight then
        //If there are several enemies within range, shooting any of the offenders is first priority
        //If there are no offenders in range then CheckForEnemy will pick a new target
        //Archers stay still and attack enemies only within their range without walking to/from them
        for K := 0 to fOffenders.Count - 1 do
          if Members[I].WithinFightRange(TKMUnitWarrior(fOffenders[K]).GetPosition) then
          begin
            Members[I].OrderFight(TKMUnitWarrior(fOffenders[K]));
            FightWasOrdered := True;
          end;

    //If nobody in the group is in a fight and all offenders are out of range then clear offenders
    //(archers should forget about out of range offenders since they won't walk to them like melee)
    if not FightWasOrdered and not InFight then
    begin
      fOffenders.Clear;
      OrderRepeat;
    end;
  end
  else
  begin
    //Idle members should help their comrades
    for I := 0 to Count - 1 do
    if not Members[I].InFight then
      Members[I].OrderWalk(TKMUnitWarrior(fOffenders[KaMRandom(fOffenders.Count)]).NextPosition, False);
  end;
end;


//Check if order has been executed and if necessary attempt to repeat it
procedure TKMUnitGroup.CheckOrderDone;
var
  I: Integer;
  OrderExecuted: Boolean;
  P: TKMPointExact;
  U: TKMUnitWarrior;
begin
  OrderExecuted := False;

  //1. Check the Order
  //2. Attempt to finish the order
  case fOrder of
    goNone:         OrderExecuted := False;
    goWalkTo:       begin
                      OrderExecuted := True;
                      for I := 0 to Count - 1 do
                      begin
                        OrderExecuted := OrderExecuted and Members[I].IsIdle and Members[I].OrderDone;

                        if Members[I].OrderDone then
                        begin
                          //If the unit is idle make them face the right direction
                          if Members[I].IsIdle
                          and (fOrderLoc.Dir <> dir_NA) and (Members[I].Direction <> fOrderLoc.Dir) then
                          begin
                            Members[I].Direction := fOrderLoc.Dir;
                            Members[I].SetActionStay(50, ua_Walk); //Make sure the animation still frame is updated
                          end;
                        end
                        else
                          //Guide Idle and pushed units back to their places
                          if Members[I].IsIdle
                          or ((Members[I].GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(Members[I].GetUnitAction).WasPushed) then
                          begin
                            P := GetMemberLoc(I);
                            Members[I].OrderWalk(P.Loc, P.Exact);
                          end;
                      end;
                    end;
    goAttackHouse:  begin
                      //It is TaskAttackHouse responsibility to execute it
                      OrderExecuted := (OrderTargetHouse = nil);
                    end;
    goAttackUnit:   begin
                      if IsRanged then
                      begin
                        //Ranged units must kill target unit only
                        //Then they will attack anything within their reach by themselves
                        OrderExecuted := (OrderTargetUnit = nil);

                        if not OrderExecuted then
                          //If our leader is out of range (enemy has walked away) we need to walk closer
                          if (KMLength(fOrderLoc.Loc, OrderTargetUnit.GetPosition) > Members[0].GetFightMaxRange) then
                            OrderAttackUnit(OrderTargetUnit, False)
                          else
                            //Our leader is in range so each member should get into position
                            for I := 0 to Count - 1 do
                            if Members[I].IsIdle then
                            begin
                              P := GetMemberLoc(I);
                              if KMSamePoint(Members[I].GetPosition, P.Loc)
                              or (KMLength(Members[I].GetPosition, OrderTargetUnit.GetPosition) <= Members[I].GetFightMaxRange) then
                              begin
                                //We are at the right spot, so face towards enemy
                                Members[I].Direction := KMGetDirection(Members[I].GetPosition, OrderTargetUnit.GetPosition);
                                Members[I].FaceDir := Members[I].Direction;
                                if not Members[I].CheckForEnemy then
                                  //If we are too close to shoot, make sure the animation still frame is still updated
                                  Members[I].SetActionStay(10, ua_Walk);
                              end
                              else
                              begin
                                //Too far away. Walk to the enemy in our formation
                                Members[I].OrderWalk(P.Loc, P.Exact);
                                Members[I].FaceDir := fOrderLoc.Dir;
                              end;
                            end;
                      end
                      else
                      begin
                        //Melee units must kill target unit and its Group
                        OrderExecuted := (OrderTargetUnit = nil) and (OrderTargetGroup = nil);

                        if OrderTargetUnit <> nil then
                        begin
                          //See if target is escaping
                          if not KMSamePoint(OrderTargetUnit.NextPosition, fOrderLoc.Loc) then
                          begin
                            Inc(fTargetFollowTicker);
                            //It's wasteful to run pathfinding to correct route every step of the way, so if the target unit
                            //is within 4 tiles, update every step. Within 8, every 2 steps, 12, every 3 steps, etc.
                            if fTargetFollowTicker mod Max((Round(KMLengthDiag(GetPosition, OrderTargetUnit.GetPosition)) div 4), 1) = 0 then
                              OrderAttackUnit(OrderTargetUnit, False);
                          end;

                          for I := 0 to Count - 1 do
                            if Members[I].IsIdle then
                            begin
                              P := GetMemberLoc(I);
                              Members[I].OrderWalk(P.Loc, P.Exact);
                            end;
                        end;


                        //If Enemy was killed, but target Group still exists
                        if (OrderTargetUnit = nil) and (OrderTargetGroup <> nil) then
                        begin
                          //Old enemy has died, change target to his comrades
                          U := OrderTargetGroup.GetNearestMember(Members[0].GetPosition);
                          Assert(U <> nil, 'We checked that Group is not dead, hence we should have a valid Unit');
                          OrderAttackUnit(U, False);
                        end;
                      end;
                    end;
    goStorm:        OrderExecuted := False;
  end;

  if OrderExecuted then
  begin
    for I := 0 to Count - 1 do
    if (fOrderLoc.Dir <> dir_NA) and Members[I].IsIdle then //Don't change direction whilst f.e. walking
      Members[I].Direction := fOrderLoc.Dir;
    OrderNone;
  end;
end;


//Fighting with citizens does not count by default
function TKMUnitGroup.InFight(aCountCitizens: Boolean = False): Boolean;
var I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
  if Members[I].InFight(aCountCitizens) then
  begin
    Result := True;
    Exit;
  end;
end;


function TKMUnitGroup.IsAttackingHouse: Boolean;
var I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
    if (Members[I].UnitTask <> nil)
    and (Members[I].UnitTask.TaskName = utn_AttackHouse) then
    begin
      Result := True;
      Exit;
    end;
end;


function TKMUnitGroup.IsAttackingUnit: Boolean;
begin
  Result := (fOrder = goAttackUnit) and (OrderTargetUnit <> nil);
end;


function TKMUnitGroup.IsIdleToAI(aAllowWalking: Boolean = False): Boolean;
begin
  //First check that the previous order has completed
  if fOrder = goWalkTo then
    Result := aAllowWalking or (KMLengthDiag(Position, fOrderLoc.Loc) < 2)
  else
    Result := (fOrder = goNone);

  //Even fighting citizens should also stop the AI repositioning the group
  Result := Result and not InFight(True);
  //Also wait until we have dealt with all offenders
  Result := Result and (fOffenders.Count = 0);
end;


function TKMUnitGroup.IsPositioned(aLoc:TKMPoint; Dir: TKMDirection): Boolean;
var I: Integer; P: TKMPointExact; U: TKMUnitWarrior;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    P.Loc := GetPositionInGroup2(aLoc.X, aLoc.Y, Dir, I, fUnitsPerRow,
                                 gTerrain.MapX, gTerrain.MapY,
                                 P.Exact);
    U := Members[I];
    Result := U.IsIdle and KMSamePoint(U.GetPosition, P.Loc) and (U.Direction = Dir);
    if not Result then Exit;
  end;
end;


function TKMUnitGroup.MemberByUID(aUID: Integer): TKMUnitWarrior;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  if (Members[I].UID = aUID) and not Members[I].IsDead then
  begin
    Result := Members[I];
    Break;
  end;
end;


function TKMUnitGroup.HitTest(X,Y: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
  if Members[I].HitTest(X, Y) and not Members[I].IsDead then
  begin
    Result := True;
    Break;
  end;
end;


procedure TKMUnitGroup.SelectFlagBearer;
begin
  fSelected := fMembers[0];
end;


//All units are assigned TTaskAttackHouse which does everything for us (move to position, hit house, abandon, etc.)
procedure TKMUnitGroup.OrderAttackHouse(aHouse: TKMHouse; aClearOffenders: Boolean);
var
  I: Integer;
begin
  Assert(aHouse <> nil);

  //Can attack only enemy houses
  if gHands[Owner].Alliances[aHouse.Owner] <> at_Enemy then Exit;

  if aClearOffenders and CanTakeOrders then ClearOffenders;

  fOrder := goAttackHouse;
  fOrderLoc := KMPointDir(0, 0, dir_NA);
  OrderTargetHouse := aHouse;

  for I := 0 to Count - 1 do
    Members[I].OrderAttackHouse(aHouse);
end;


procedure TKMUnitGroup.OrderAttackUnit(aUnit: TKMUnit; aClearOffenders: Boolean);
var
  I: Integer;
  NodeList: TKMPointList;
  P: TKMPointExact;
begin
  Assert(aUnit <> nil);

  //If unit is already dying ignore the order
  if aUnit.IsDeadOrDying then Exit;

  //Can attack only enemy units
  if gHands[Owner].Alliances[aUnit.Owner] <> at_Enemy then Exit;

  if aClearOffenders and CanTakeOrders then
    ClearOffenders;

  if IsRanged then
  begin
    //Ranged units should walk in formation to within range of the enemy
    fOrder := goAttackUnit;
    OrderTargetUnit := aUnit;

    //First choose fOrderLoc, which is where the leader will stand to shoot
    if (KMLength(Members[0].GetPosition, OrderTargetUnit.GetPosition) > Members[0].GetFightMaxRange) then
    begin
      NodeList := TKMPointList.Create;
      if gGame.Pathfinding.Route_Make(Members[0].GetPosition, OrderTargetUnit.NextPosition, [tpWalk], Members[0].GetFightMaxRange, nil, NodeList) then
      begin
        fOrderLoc.Loc := NodeList[NodeList.Count-1];
        fOrderLoc.Dir := KMGetDirection(NodeList[NodeList.Count-1], OrderTargetUnit.NextPosition);
        HungarianReorderMembers; //We are about to get them to walk to fOrderLoc
      end
      else
      begin
        OrderTargetUnit := nil; //Target cannot be reached, so abort completely
        fOrder := goNone;
        NodeList.Free;
        Exit;
      end;
      NodeList.Free;
    end
    else
    begin
      fOrderLoc.Loc := Members[0].GetPosition; //Leader is already within range
      fOrderLoc.Dir := KMGetDirection(Members[0].GetPosition, OrderTargetUnit.NextPosition);
    end;

    //Next assign positions for each member (including leader)
    for I := 0 to Count - 1 do
    begin
      //Check target in range, and if not - chase it / back up from it
      P := GetMemberLoc(I);
      if not KMSamePoint(Members[I].GetPosition, P.Loc)
      and((KMLength(Members[I].NextPosition, OrderTargetUnit.GetPosition) > Members[I].GetFightMaxRange)
       or (KMLength(Members[I].NextPosition, OrderTargetUnit.GetPosition) < Members[I].GetFightMinRange)) then
      begin
        //Too far/close. Walk to the enemy in formation
        Members[I].OrderWalk(P.Loc, P.Exact);
        Members[I].FaceDir := fOrderLoc.Dir;
      end
      else
        if not Members[I].IsIdle then
        begin
          Members[I].OrderWalk(Members[I].NextPosition, True); //We are at the right spot already, just need to abandon what we are doing
          Members[I].FaceDir := fOrderLoc.Dir;
        end
        else
        begin
          //We are within range, so face towards the enemy
          //Don't fight this specific enemy, giving archers exact targets is too abusable in MP. Choose random target in that direction.
          Members[I].Direction := KMGetDirection(Members[I].GetPosition, aUnit.GetPosition);
          Members[I].FaceDir := Members[I].Direction;
          if not Members[I].CheckForEnemy then
            //If we are too close to shoot, make sure the animation still frame is still updated
            Members[I].SetActionStay(10, ua_Walk);
        end;
    end;
  end
  else
  begin
    //Walk in formation towards enemy,
    //Members will take care of attack when we approach
    OrderWalk(aUnit.NextPosition, False);

    //Revert Order to proper one (we disguise Walk)
    fOrder := goAttackUnit;
    fOrderLoc := KMPointDir(aUnit.NextPosition, dir_NA); //Remember where unit stand
    OrderTargetUnit := aUnit;
  end;
end;


//Order some food for troops
procedure TKMUnitGroup.OrderFood(aClearOffenders: Boolean; aHungryOnly: Boolean = False);
var I: Integer;
begin
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  for I := 0 to Count - 1 do
    if not aHungryOnly or (Members[I].Condition <= UNIT_MIN_CONDITION) then
      Members[I].OrderFood;

  OrderHalt(False);
end;


procedure TKMUnitGroup.OrderFormation(aTurnAmount: TKMTurnDirection; aColumnsChange: ShortInt; aClearOffenders: Boolean);
begin
  if IsDead then Exit;
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  //If it is yet unset - use first members direction
  if fOrderLoc.Dir = dir_NA then
    fOrderLoc.Dir := Members[0].Direction;

  case aTurnAmount of
    tdCW:   fOrderLoc.Dir := KMNextDirection(fOrderLoc.Dir);
    tdCCW:  fOrderLoc.Dir := KMPrevDirection(fOrderLoc.Dir);
  end;

  SetUnitsPerRow(Max(fUnitsPerRow + aColumnsChange, 0));

  OrderRepeat;
end;


//Forcefull termination of any activity
procedure TKMUnitGroup.OrderHalt(aClearOffenders: Boolean);
begin
  if aClearOffenders and CanTakeOrders then
    ClearOffenders;

  //Halt is not a true order, it is just OrderWalk
  //hose target depends on previous activity
  case fOrder of
    goNone:         if not KMSamePoint(fOrderLoc.Loc, KMPoint(0,0)) then
                      OrderWalk(fOrderLoc.Loc, False)
                    else
                      OrderWalk(Members[0].NextPosition, False);
    goWalkTo:       OrderWalk(Members[0].NextPosition, False);
    goAttackHouse:  OrderWalk(Members[0].NextPosition, False);
    goAttackUnit:   OrderWalk(Members[0].NextPosition, False);
    goStorm:        OrderWalk(Members[0].NextPosition, False);
  end;
end;


procedure TKMUnitGroup.OrderLinkTo(aTargetGroup: TKMUnitGroup; aClearOffenders: Boolean);
var U: TKMUnit;
begin
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  //Any could have died since the time order was issued due to Net delay
  if IsDead or aTargetGroup.IsDead then Exit;

  //Only link to same group type
  if aTargetGroup.GroupType <> GroupType then Exit;

  //Can't link to self for obvious reasons
  if aTargetGroup = Self then Exit;

  //Move our members and self to the new group
  while (fMembers.Count <> 0) do
  begin
    U := Members[0];
    aTargetGroup.AddMember(Members[0]);
    gHands.CleanUpUnitPointer(U);
    fMembers.Delete(0);
  end;

  //In MP commands execution may be delayed, check if we still selected
  if gMySpectator.Selected = Self then
  begin
    gMySpectator.Selected := aTargetGroup;
    //What if fSelected died by now
    if not fSelected.IsDeadOrDying then
    begin
      Assert(aTargetGroup.HasMember(fSelected), 'Make sure we joined selected unit');
      aTargetGroup.fSelected := fSelected;
    end;
  end;

  //Repeat targets group order to newly linked members
  aTargetGroup.OrderRepeat;
end;


procedure TKMUnitGroup.OrderNone;
var
  I: Integer;
begin
  fOrder := goNone;
  //fOrderLoc remains old
  ClearOrderTarget;

  for I := 0 to Count - 1 do
    Members[I].OrderNone;
end;


//Repeat last order e.g. if new members have joined
procedure TKMUnitGroup.OrderRepeat;
begin
  case fOrder of
    goNone:         OrderHalt(False);
    goWalkTo:       OrderWalk(fOrderLoc.Loc, False);
    goAttackHouse:  if OrderTargetHouse <> nil then OrderAttackHouse(OrderTargetHouse, False);
    goAttackUnit:   if OrderTargetUnit <> nil then OrderAttackUnit(OrderTargetUnit, False);
    goStorm:        ;
  end;
end;


//Split group in half
//or split different unit types apart
function TKMUnitGroup.OrderSplit(aClearOffenders: Boolean; aSplitSingle: Boolean = False): TKMUnitGroup;
var
  I: Integer;
  NewGroup: TKMUnitGroup;
  NewLeader: TKMUnitWarrior;
  MultipleTypes: Boolean;
  U: TKMUnit;
begin
  Result := nil;
  if IsDead then Exit;
  if Count < 2 then Exit;
  //If leader is storming don't allow splitting the group (makes it too easy to withdraw)
  if Members[0].GetUnitAction is TUnitActionStormAttack then Exit;
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  //If there are different unit types in the group, split should just split them first
  MultipleTypes := False;

  //Choose the new leader
  if aSplitSingle then
    NewLeader := Members[Count - 1]
  else
  begin
    NewLeader := Members[(Count div 2) + (Min(fUnitsPerRow, Count div 2) div 2)];

    for I := 1 to Count - 1 do
      if Members[I].UnitType <> Members[0].UnitType then
      begin
        MultipleTypes := True;
        //New commander is first unit of different type, for simplicity
        NewLeader := Members[I];
        Break;
      end;
  end;
  //Remove from the group
  NewLeader.ReleaseUnitPointer;
  fMembers.Remove(NewLeader);

  NewGroup := gHands[Owner].UnitGroups.AddGroup(NewLeader);
  NewGroup.OnGroupDied := OnGroupDied;

  if not aSplitSingle then
    //Split by UnitTypes or by Count (make NewGroup half or smaller half)
    for I := Count - 1 downto 0 do
      if (MultipleTypes and (Members[I].UnitType = NewLeader.UnitType))
         or (not MultipleTypes and (Count > NewGroup.Count + 1)) then
      begin
        U := Members[I];
        gHands.CleanUpUnitPointer(U);
        NewGroup.AddMember(Members[I], 1); // Join new group (insert next to commander)
        fMembers.Delete(I); // Leave this group
      end;

  //Keep the selected unit Selected
  if NewGroup.HasMember(fSelected) or aSplitSingle then
  begin
    gMySpectator.Selected := NewGroup;
    NewGroup.fSelected := fSelected;
  end;

  //Make sure units per row is still valid for both groups
  UnitsPerRow := fUnitsPerRow;
  NewGroup.UnitsPerRow := fUnitsPerRow;

  //If we are hungry then don't repeat message each time we split, give new commander our counter
  NewGroup.fTimeSinceHungryReminder := fTimeSinceHungryReminder;

  //Commander OrderLoc must always be valid, but because this guy wasn't a commander it might not be
  NewGroup.fOrderLoc := KMPointDir(NewLeader.GetPosition, fOrderLoc.Dir);

  //Tell both groups to reposition
  OrderHalt(False);
  NewGroup.OrderHalt(False);

  Result := NewGroup; //Return the new group in case somebody is interested in it
end;


//Split ONE certain unit from the group
function TKMUnitGroup.OrderSplitUnit(aUnit: TKMUnit; aClearOffenders: Boolean): TKMUnitGroup;
var
  NewGroup: TKMUnitGroup;
  NewLeader: TKMUnitWarrior;
begin
  Result := nil;
  if not HasMember(aUnit) then Exit;
  if IsDead then Exit;
  if Count < 2 then Exit;
  if aClearOffenders
  and CanTakeOrders then
    ClearOffenders;

  //Delete from group
  NewLeader := TKMUnitWarrior(aUnit);
  fMembers.Remove(NewLeader);
  NewLeader.ReleaseUnitPointer;

  //Give new group
  NewGroup := gHands[Owner].UnitGroups.AddGroup(NewLeader);
  NewGroup.OnGroupDied := OnGroupDied;
  NewGroup.fSelected := NewLeader;
  NewGroup.fTimeSinceHungryReminder := fTimeSinceHungryReminder;
  NewGroup.fOrderLoc := KMPointDir(NewLeader.GetPosition, fOrderLoc.Dir);

  //Set units per row
  UnitsPerRow := fUnitsPerRow;
  NewGroup.UnitsPerRow := 1;

  //Save unit selection
  if NewGroup.HasMember(fSelected) then
  begin
    gMySpectator.Selected := NewGroup;
    NewGroup.fSelected := fSelected;
  end;

  //Halt both groups
  OrderHalt(False);
  NewGroup.OrderHalt(False);

  //Return NewGroup as result
  Result := NewGroup;
end;


//Splits X number of men from the group and adds them to the new commander
procedure TKMUnitGroup.OrderSplitLinkTo(aGroup: TKMUnitGroup; aCount: Word; aClearOffenders: Boolean);
var
  I: Integer;
  U: TKMUnit;
begin
  //Make sure to leave someone in the group
  Assert(aCount < Count);
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  //Take units from the end, to keep flagholder
  for I := fMembers.Count - 1 downto fMembers.Count - aCount do
  begin
    U := Members[I];
    gHands.CleanUpUnitPointer(U);
    aGroup.AddMember(Members[I]);
    fMembers.Delete(I);
  end;

  //Make sure units per row is still valid
  SetUnitsPerRow(UnitsPerRow);

  //Tell both groups to reposition
  OrderHalt(False);
  aGroup.OrderHalt(False);
end;


procedure TKMUnitGroup.OrderStorm(aClearOffenders: Boolean);
var I: Integer;
begin
  //Don't allow ordering a second storm attack while there is still one active (possible due to network lag)
  if not CanTakeOrders then Exit;
  if aClearOffenders and CanTakeOrders then ClearOffenders;

  fOrder := goStorm;
  fOrderLoc := KMPointDir(0, 0, dir_NA);
  ClearOrderTarget;

  //Each next row delayed by few ticks to avoid crowding
  for I := 0 to Count - 1 do
    Members[I].OrderStorm(I div fUnitsPerRow);
end;


procedure TKMUnitGroup.OrderWalk(aLoc: TKMPoint; aClearOffenders: Boolean; aDir: TKMDirection = dir_NA);
var
  I: Integer;
  NewDir: TKMDirection;
  P: TKMPointExact;
begin
  if IsDead then Exit;

  if aClearOffenders and CanTakeOrders then
    ClearOffenders;

  if aDir = dir_NA then
    if fOrderLoc.Dir = dir_NA then
      NewDir := Members[0].Direction
    else
      NewDir := fOrderLoc.Dir
  else
    NewDir := aDir;

  fOrderLoc := KMPointDir(aLoc, NewDir);
  ClearOrderTarget;

  if IsPositioned(aLoc, NewDir) then
    Exit; //No need to actually walk, all members are at the correct location and direction

  fOrder := goWalkTo;
  HungarianReorderMembers;

  for I := 0 to Count - 1 do
  begin
    P := GetMemberLoc(I);
    Members[I].OrderWalk(P.Loc, P.Exact);
    Members[I].FaceDir := NewDir;
  end;
end;


function TKMUnitGroup.UnitType: TUnitType;
begin
  Result := Members[0].UnitType;
end;


function TKMUnitGroup.GetOrderText: UnicodeString;
begin
  case fOrder of
    goNone:         Result := 'Idle';
    goWalkTo:       Result := 'Walk';
    goAttackHouse:  Result := 'Attack house';
    goAttackUnit:   Result := 'Attack unit';
    goStorm:        Result := 'Storm';
  end;
  Result := Result + '(' + IntToStr(fOffenders.Count) + ')';
end;


//Tell the player to feed us if we are hungry
procedure TKMUnitGroup.UpdateHungerMessage;
var
  I: Integer;
  SomeoneHungry: Boolean;
begin
  if IsDead then Exit;

  SomeoneHungry := False;
  for I := 0 to Count - 1 do
    if (Members[I] <> nil) 
    and not Members[I].IsDeadOrDying
    begin
      SomeoneHungry := SomeoneHungry
                       or ((Members[I].Condition < UNIT_MIN_CONDITION)
                       and not Members[I].RequestedFood);
      if SomeoneHungry then Break;
    end;

  if SomeoneHungry then
  begin
    dec(fTimeSinceHungryReminder, HUNGER_CHECK_FREQ);
    if fTimeSinceHungryReminder < 1 then
    begin
      gScriptEvents.ProcGroupHungry(Self);
      if not fDisableHungerMessage then
        gGame.ShowMessage(mkUnit, TX_MSG_TROOP_HUNGRY, Position, fOwner);
      fTimeSinceHungryReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
    end;
  end
  else
    fTimeSinceHungryReminder := 0;
end;


procedure TKMUnitGroup.ClearOrderTarget;
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  gHands.CleanUpUnitPointer(fOrderTargetUnit);
  gHands.CleanUpGroupPointer(fOrderTargetGroup);
  gHands.CleanUpHousePointer(fOrderTargetHouse);
end;


procedure TKMUnitGroup.ClearOffenders;
var I: Integer; U: TKMUnit;
begin
  for I := fOffenders.Count - 1 downto 0 do
  begin
    U := fOffenders[I]; //Need to pass variable
    gHands.CleanUpUnitPointer(U);
  end;
  fOffenders.Clear;
end;


procedure TKMUnitGroup.HungarianReorderMembers;
var
  Agents, Tasks: TKMPointList;
  I: Integer;
  NewOrder: TKMCardinalArray;
  NewMembers: TList;
begin
  if DO_PERF_LOGGING then gGame.PerfLog.EnterSection(psHungarian);
  if not HUNGARIAN_GROUP_ORDER then Exit;
  if fMembers.Count <= 1 then Exit; //If it's just the leader we can't rearrange
  Agents := TKMPointList.Create;
  Tasks := TKMPointList.Create;

  //todo: Process each unit type seperately in mixed groups so their order is maintained

  //Skip leader, he can't be reordered because he holds the flag
  //(tossing flag around is quite complicated and looks unnatural in KaM)
  for I := 1 to fMembers.Count - 1 do
  begin
    Agents.Add(Members[I].GetPosition);
    Tasks.Add(GetMemberLoc(I).Loc);
  end;

  //hu_Individual as we'd prefer 20 members to take 1 step than 1 member to take 10 steps (minimize individual work rather than total work)
  NewOrder := HungarianMatchPoints(Tasks, Agents, hu_Individual);
  NewMembers := TList.Create;
  NewMembers.Add(Members[0]);

  for I := 1 to fMembers.Count - 1 do
    NewMembers.Add(fMembers[NewOrder[I - 1] + 1]);

  fMembers.Free;
  fMembers := NewMembers;

  Agents.Free;
  Tasks.Free;
  if DO_PERF_LOGGING then gGame.PerfLog.LeaveSection(psHungarian);
end;


function TKMUnitGroup.GetOrderTargetUnit: TKMUnit;
begin
  //If the target unit has died then return nil
  //Don't clear fOrderTargetUnit here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetUnit <> nil) and fOrderTargetUnit.IsDeadOrDying then
    Result := nil
  else
    Result := fOrderTargetUnit;
end;


function TKMUnitGroup.GetOrderTargetGroup: TKMUnitGroup;
begin
  //If the target group has died then return nil
  //Don't clear fOrderTargetGroup here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetGroup <> nil) and fOrderTargetGroup.IsDead then
    Result := nil
  else
    Result := fOrderTargetGroup;
end;


function TKMUnitGroup.GetOrderTargetHouse: TKMHouse;
begin
  //If the target house has been destroyed then return nil
  //Don't clear fOrderTargetHouse here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetHouse <> nil) and fOrderTargetHouse.IsDestroyed then
    Result := nil
  else
    Result := fOrderTargetHouse;
end;


procedure TKMUnitGroup.SetOrderTargetUnit(aUnit: TKMUnit);
var G: TKMUnitGroup;
begin
  //Remove previous value
  ClearOrderTarget;
  if (aUnit <> nil) and not (aUnit.IsDeadOrDying) then
  begin
    fOrderTargetUnit := aUnit.GetUnitPointer; //Else it will be nil from ClearOrderTarget
    if (aUnit is TKMUnitWarrior) and not IsRanged then
    begin
      G := gHands[aUnit.Owner].UnitGroups.GetGroupByMember(TKMUnitWarrior(aUnit));
      //Target warrior won't have a group while he's walking out of the barracks
      if G <> nil then
        fOrderTargetGroup := G.GetGroupPointer;
    end;
  end;
end;


procedure TKMUnitGroup.SetOrderTargetHouse(aHouse: TKMHouse);
begin
  //Remove previous value
  ClearOrderTarget;
  if (aHouse <> nil) and not aHouse.IsDestroyed then
    fOrderTargetHouse := aHouse.GetHousePointer; //Else it will be nil from ClearOrderTarget
end;


//Clear target if it is dead
procedure TKMUnitGroup.UpdateOrderTargets;
begin
  if (fOrderTargetUnit <> nil) and (fOrderTargetUnit.IsDeadOrDying) then
    gHands.CleanUpUnitPointer(fOrderTargetUnit);

  if (fOrderTargetHouse <> nil) and (fOrderTargetHouse.IsDestroyed) then
    gHands.CleanUpHousePointer(fOrderTargetHouse);

  if (fOrderTargetGroup <> nil) and fOrderTargetGroup.IsDead then
    gHands.CleanUpGroupPointer(fOrderTargetGroup);
end;


procedure TKMUnitGroup.UpdateState;
begin
  Inc(fTicker);
  if IsDead then Exit;

  UpdateOrderTargets;

  if fTicker mod HUNGER_CHECK_FREQ = 0 then
    UpdateHungerMessage;

  if fTicker mod 5 = 0 then
    CheckForFight;

  if not InFight and (fTicker mod 7 = 0) then
    CheckOrderDone;
end;


procedure TKMUnitGroup.Paint;
var
  FlagCarrier: TKMUnitWarrior;
  UnitPos: TKMPointF;
  FlagColor: Cardinal;
  FlagStep: Cardinal;
  I: Integer;
  NewPos: TKMPoint;
  DoesFit: Boolean;
begin
  if IsDead then Exit;

  FlagCarrier := Members[0]; //

  if not FlagCarrier.Visible then Exit;
  if FlagCarrier.IsDeadOrDying then Exit;

  UnitPos.X := FlagCarrier.PositionF.X + UNIT_OFF_X + FlagCarrier.GetSlide(ax_X);
  UnitPos.Y := FlagCarrier.PositionF.Y + UNIT_OFF_Y + FlagCarrier.GetSlide(ax_Y);

  //Highlight selected group
  FlagColor := gHands[FlagCarrier.Owner].FlagColor;
  if gMySpectator.Selected = Self then
    //If base color is brighter than $FFFF40 then use black highlight
    if (FlagColor and $FF) + (FlagColor shr 8 and $FF) + (FlagColor shr 16 and $FF) > $240 then
      FlagColor := $FF404040
    else
      FlagColor := $FFFFFFFF;

  //In MapEd units fTicker always the same, use Terrain instead
  FlagStep := IfThen(gGame.GameMode = gmMapEd, gTerrain.AnimStep, fTicker);

  //Flag needs to be rendered above or below unit depending on direction (see AddUnitFlag)

  if FlagCarrier.Direction in [dir_SE, dir_S, dir_SW, dir_W] then
    UnitPos.Y := UnitPos.Y - FLAG_X_OFFSET
  else
    UnitPos.Y := UnitPos.Y + FLAG_X_OFFSET;

  fRenderPool.AddUnitFlag(FlagCarrier.UnitType, FlagCarrier.GetUnitAction.ActionType,
    FlagCarrier.Direction, FlagStep, UnitPos.X, UnitPos.Y, FlagColor);

  //Paint virtual members in MapEd mode
  for I := 1 to fMapEdCount - 1 do
  begin
    NewPos := GetPositionInGroup2(fOrderLoc.Loc.X, fOrderLoc.Loc.Y, fOrderLoc.Dir, I, fUnitsPerRow, gTerrain.MapX, gTerrain.MapY, DoesFit);
    if not DoesFit then Continue; //Don't render units that are off the map in the map editor
    UnitPos.X := NewPos.X + UNIT_OFF_X; //MapEd units don't have sliding
    UnitPos.Y := NewPos.Y + UNIT_OFF_Y;
    fRenderPool.AddUnit(FlagCarrier.UnitType, 0, ua_Walk, fOrderLoc.Dir, UnitStillFrames[fOrderLoc.Dir], UnitPos.X, UnitPos.Y, gHands[FlagCarrier.Owner].FlagColor, True);
  end;
end;


{ TKMUnitGroups }
constructor TKMUnitGroups.Create;
begin
  inherited Create;

  fGroups := TKMList.Create;
end;


destructor TKMUnitGroups.Destroy;
begin
  fGroups.Free;

  inherited;
end;


function TKMUnitGroups.GetCount: Integer;
begin
  Result := fGroups.Count;
end;


function TKMUnitGroups.GetGroup(aIndex: Integer): TKMUnitGroup;
begin
  Result := fGroups[aIndex];
end;


function TKMUnitGroups.AddGroup(aWarrior: TKMUnitWarrior): TKMUnitGroup;
begin
  Result := TKMUnitGroup.Create(gGame.GetNewUID, aWarrior);
  fGroups.Add(Result)
end;


function TKMUnitGroups.AddGroup(aOwner: TKMHandIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aCount: Word): TKMUnitGroup;
begin
  Result := nil;
  Assert(aUnitType in [WARRIOR_MIN..WARRIOR_MAX]);

  Result := TKMUnitGroup.Create(gGame.GetNewUID, aOwner, aUnitType, PosX, PosY, aDir, aUnitPerRow, aCount);

  //If group failed to create (e.g. due to being placed on unwalkable position)
  //then its memberCount = 0
  if not Result.IsDead then
    fGroups.Add(Result)
  else
    FreeAndNil(Result);
end;


function TKMUnitGroups.GetGroupByUID(aUID: Integer): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if aUID = Groups[I].UID then
    begin
      Result := Groups[I];
      Break;
    end;
end;


function TKMUnitGroups.GetGroupByMember(aUnit: TKMUnitWarrior): TKMUnitGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Groups[I].HasMember(aUnit) then
    begin
      Result := fGroups[I];
      Break;
    end;
end;


//Warrior has been trained and we need to see where to place him
//Return group he was assigned to
function TKMUnitGroups.WarriorTrained(aUnit: TKMUnitWarrior): TKMUnitGroup;
var
  LinkUnit: TKMUnitWarrior;
begin
  Result := nil; //Makes compiler happy

  case gHands[aUnit.Owner].HandType of
    hndHuman:    begin
                   LinkUnit := aUnit.FindLinkUnit(aUnit.GetPosition);
                   if LinkUnit <> nil then
                   begin
                     //Link to other group
                     Result := gHands[aUnit.Owner].UnitGroups.GetGroupByMember(LinkUnit);
                     Result.AddMember(aUnit);
                     //Form a square (rather than a long snake like in TSK/TPR)
                     Result.UnitsPerRow := Ceil(Sqrt(Result.Count));
                     Result.OrderRepeat;
                   end
                   else
                   begin
                     //Create a new group with this one warrior
                     Result := TKMUnitGroup.Create(gGame.GetNewUID, aUnit);
                     fGroups.Add(Result);
                   end;
                 end;
    hndComputer: begin
                   Result := TKMUnitGroup.Create(gGame.GetNewUID, aUnit);
                   fGroups.Add(Result);
                 end;
  end;
end;


function TKMUnitGroups.HitTest(X,Y: Integer): TKMUnitGroup;
var
  I: Integer;
  U: TKMUnit;
begin
  Result := nil;
  U := gTerrain.UnitsHitTest(X,Y);
  if (U <> nil) and (U is TKMUnitWarrior) then
  for I := 0 to Count - 1 do
    if Groups[I].HitTest(X,Y) then
    begin
      Result := Groups[I];
      Break;
    end;
end;


function TKMUnitGroups.GetClosestGroup(aPoint: TKMPoint; aTypes: TGroupTypeSet = [Low(TGroupType)..High(TGroupType)]): TKMUnitGroup;
var
  I: Integer;
  BestDist, Dist: Single;
begin
  Result := nil;
  BestDist := MaxSingle; //Any distance will be closer than that
  for I := 0 to Count - 1 do
    if (Groups[I].GroupType in aTypes) and not Groups[I].IsDead then
    begin
      Dist := KMLengthSqr(Groups[I].GetPosition, aPoint);
      if Dist < BestDist then
      begin
        BestDist := Dist;
        Result := Groups[I];
      end;
    end;
end;


procedure TKMUnitGroups.RemGroup(aGroup: TKMUnitGroup);
begin
  fGroups.Remove(aGroup);
end;


procedure TKMUnitGroups.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.WriteA('UnitGroups');
  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
    Groups[I].Save(SaveStream);
end;


procedure TKMUnitGroups.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
begin
  LoadStream.ReadAssert('UnitGroups');
  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
    fGroups.Add(TKMUnitGroup.Create(LoadStream));
end;


procedure TKMUnitGroups.SyncLoad;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Groups[I].SyncLoad;
end;


procedure TKMUnitGroups.UpdateState;
var
  I: Integer;
begin
  //We delete dead groups only next tick after they died
  //so that gMySpectator.Selected could register their death and reset
  //(this could be outdated with Spectators appearence)
  for I := Count - 1 downto 0 do
  if FREE_POINTERS
  and Groups[I].IsDead
  and (Groups[I].fPointerCount = 0) then
    fGroups.Delete(I);

  for I := 0 to Count - 1 do
  if not Groups[I].IsDead then
    Groups[I].UpdateState;
end;


procedure TKMUnitGroups.Paint(aRect: TKMRect);
const
  Margin = 2;
var
  I: Integer;
  growRect: TKMRect;
begin
  //Add additional margin to compensate for units height
  growRect := KMRectGrow(aRect, Margin);

  for I := 0 to Count - 1 do
  if not Groups[I].IsDead and KMInRect(Groups[I].Members[0].PositionF, growRect) then
    Groups[I].Paint;
end;


end.
