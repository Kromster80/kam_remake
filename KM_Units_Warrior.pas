unit KM_Units_Warrior;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Utils, KM_Terrain, KM_Units, KM_Houses, KM_Points;

type

  TKMUnitWarrior = class;
  TKMWarriorEvent = procedure(aWarrior: TKMUnitWarrior) of object;

  //Possibly melee warrior class? with Archer class separate?
  TKMUnitWarrior = class(TKMUnit)
  private
    fRequestedFood: Boolean;

    fNewOrder: TWarriorOrder;
    fOrder: TWarriorOrder;
    fOrderLoc: TKMPointDir; //Dir is the direction to face after order
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use public OrderHouseTarget instead.
    fStormDelay: Word;
    fUseExactTarget: Boolean; //Do we try to reach exact position or is it e.g. unwalkable

    function CanInterruptAction: Boolean;

    procedure ClearOrderTarget;
    procedure SetOrderTarget(aUnit: TKMUnit);
    function GetOrderTarget: TKMUnit;
    function GetOrderHouseTarget: TKMHouse;
  public
    OnKilled: TKMWarriorEvent;

    constructor Create(aID: Cardinal; aUnitType: TUnitType; PosX, PosY: Word; aOwner: TPlayerIndex);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure CloseUnit(aRemoveTileUsage: Boolean = True); override;
    destructor Destroy; override;

    procedure KillUnit; override;

    property RequestedFood: Boolean read fRequestedFood write fRequestedFood; //Cleared by Serf delivering food
    procedure SetOrderHouseTarget(aHouse: TKMHouse);

  //Commands from player
    procedure OrderFood;
    procedure OrderNone;
    procedure OrderStorm(aDelay: Word);
    procedure OrderWalk(aLoc: TKMPointDir; aUseExactTarget: Boolean = True);
    procedure OrderAttackUnit(aTargetUnit: TKMUnit);
    procedure OrderAttackHouse(aTargetHouse: TKMHouse);
    function OrderDone: Boolean;

    function GetFightMinRange: Single;
    function GetFightMaxRange(aTileBased: Boolean = False): Single;
    function WithinFightRange(Value: TKMPoint): Boolean;
    property OrderTarget: TKMUnit read GetOrderTarget write SetOrderTarget;
    property OrderLocDir: TKMPointDir read fOrderLoc write fOrderLoc;
    property GetOrder: TWarriorOrder read fOrder;
    property UseExactTarget: Boolean read fUseExactTarget;

    function IsRanged: Boolean;
    function FindLinkUnit(aLoc: TKMPoint): TKMUnitWarrior;
    function GetActivityText: string; override;

    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse); override;

    function CheckForEnemy: Boolean;
    function FindEnemy: TKMUnit;
    procedure FightEnemy(aEnemy: TKMUnit);

    procedure Save(SaveStream: TKMemoryStream); override;
    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;


implementation
uses KM_CommonTypes, KM_DeliverQueue, KM_Game, KM_TextLibrary, KM_PlayersCollection, KM_RenderPool, KM_RenderAux,
  KM_UnitTaskAttackHouse,
  KM_UnitActionAbandonWalk, KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionWalkTo, KM_UnitActionStay,
  KM_UnitActionStormAttack, KM_Resource, KM_ResourceUnit;


{ TKMUnitWarrior }
constructor TKMUnitWarrior.Create(aID: Cardinal; aUnitType: TUnitType; PosX, PosY: Word; aOwner: TPlayerIndex);
begin
  inherited;
  fOrderTargetUnit   := nil;
  fOrderTargetHouse  := nil;
  fRequestedFood     := False;
  fNewOrder          := woNone;
  fOrder             := woNone;
  fOrderLoc          := KMPointDir(PosX, PosY, dir_NA);
end;


constructor TKMUnitWarrior.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fNewOrder, SizeOf(fNewOrder));
  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fOrderLoc);
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fRequestedFood);
  LoadStream.Read(fStormDelay);
  LoadStream.Read(fUseExactTarget);
end;


procedure TKMUnitWarrior.SyncLoad;
begin
  inherited;
  fOrderTargetUnit := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fOrderTargetUnit)));
  fOrderTargetHouse := fPlayers.GetHouseByID(cardinal(fOrderTargetHouse));
end;


procedure TKMUnitWarrior.CloseUnit;
begin
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);
  fNewOrder := woNone;
  fOrder := woNone;
  inherited;
end;


destructor TKMUnitWarrior.Destroy;
begin
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);

  inherited;
end;


procedure TKMUnitWarrior.KillUnit;
begin
  if not IsDeadOrDying then
  begin
    ClearOrderTarget; //This ensures that pointer usage tracking is reset
    OnKilled(Self);
  end;

  inherited;
end;


//Order some food for troops
procedure TKMUnitWarrior.OrderFood;
begin
  if (fCondition < (UNIT_MAX_CONDITION * TROOPS_FEED_MAX)) and not fRequestedFood then
  begin
    fPlayers[fOwner].Deliveries.Queue.AddDemand(nil, Self, rt_Food, 1, dt_Once, di_High);
    fRequestedFood := True;
  end;
end;


procedure TKMUnitWarrior.OrderNone;
begin
  fNewOrder := woNone;
  fUseExactTarget := False;
  ClearOrderTarget;
end;


procedure TKMUnitWarrior.OrderStorm(aDelay: Word);
begin
  fNewOrder := woStorm;
  ClearOrderTarget;

  fStormDelay := aDelay;
end;


procedure TKMUnitWarrior.ClearOrderTarget;
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);
end;


procedure TKMUnitWarrior.SetOrderTarget(aUnit: TKMUnit);
begin
  //Remove previous value
  ClearOrderTarget;
  if aUnit <> nil then
    fOrderTargetUnit := aUnit.GetUnitPointer; //Else it will be nil from ClearOrderTarget
end;


function TKMUnitWarrior.GetOrderTarget: TKMUnit;
begin
  //If the target unit has died then clear it
  if (fOrderTargetUnit <> nil) and (fOrderTargetUnit.IsDead) then ClearOrderTarget;
  Result := fOrderTargetUnit;
end;


procedure TKMUnitWarrior.SetOrderHouseTarget(aHouse: TKMHouse);
begin
  //Remove previous value
  ClearOrderTarget;
  if aHouse <> nil then
    fOrderTargetHouse := aHouse.GetHousePointer; //Else it will be nil from ClearOrderTarget
end;


function TKMUnitWarrior.GetOrderHouseTarget:TKMHouse;
begin
  //If the target house has been destroyed then clear it
  if (fOrderTargetHouse <> nil) and (fOrderTargetHouse.IsDestroyed) then ClearOrderTarget;
  Result := fOrderTargetHouse;
end;


//At which range we can fight
function TKMUnitWarrior.GetFightMaxRange(aTileBased: Boolean = False): Single;
begin
  case fUnitType of
    ut_Bowman:      Result := RANGE_BOWMAN_MAX / (Byte(REDUCE_SHOOTING_RANGE) + 1);
    ut_Arbaletman:  Result := RANGE_ARBALETMAN_MAX / (Byte(REDUCE_SHOOTING_RANGE) + 1);
    ut_Slingshot:   Result := RANGE_SLINGSHOT_MAX / (Byte(REDUCE_SHOOTING_RANGE) + 1);
    //During storm attack we look for enemies 1.42 tiles away so we engage enemies easier and don't accidentially walk past them diagonally
    else            if aTileBased and not (GetUnitAction is TUnitActionStormAttack) then
                      Result := 1 //Enemy must maximum be 1 tile away
                    else
                      Result := 1.42; //slightly bigger than sqrt(2) for diagonal fights
  end;
end;


//At which range we can fight
function TKMUnitWarrior.GetFightMinRange:single;
begin
  case fUnitType of
    ut_Bowman:      Result := RANGE_BOWMAN_MIN;
    ut_Arbaletman:  Result := RANGE_ARBALETMAN_MIN;
    ut_Slingshot:   Result := RANGE_SLINGSHOT_MIN;
    else            Result := 0.5;
  end;
end;


function TKMUnitWarrior.WithinFightRange(Value: TKMPoint): Boolean;
begin
  Result := InRange(KMLength(NextPosition, Value), GetFightMinRange, GetFightMaxRange);
end;


function TKMUnitWarrior.IsRanged: Boolean;
begin
  Result := fResource.UnitDat[fUnitType].FightType = ft_Ranged;
end;


function TKMUnitWarrior.FindLinkUnit(aLoc: TKMPoint): TKMUnitWarrior;
var
  I,K: Integer;
  FoundUnit: TKMUnit;
begin
  Result := nil;

  //Replacing it with fTerrain.UnitsHitTestWithinRad sounds plausible, but would require
  //to change input parameters to include TKMUnitWarrior, fOwner, UnitType.
  //I think thats just not worth it
  for I := -LINK_RADIUS to LINK_RADIUS do
  for K := -LINK_RADIUS to LINK_RADIUS do
  if (GetLength(I,K) < LINK_RADIUS) //Check within circle area
  and fTerrain.TileInMapCoords(aLoc.X+I, aLoc.Y+K) then //Do not pass negative coordinates to fTerrain.UnitsHitTest
  begin
    FoundUnit := fTerrain.Land[aLoc.Y+K, aLoc.X+I].IsUnit; //Use IsUnit rather than HitTest because it's faster and we don't care whether the unit is visible (as long as it's on an IsUnit)
    if (FoundUnit is TKMUnitWarrior) and
       (FoundUnit <> Self) and
       (FoundUnit.Owner = fOwner) and
       (not FoundUnit.IsDeadOrDying) and //Can't link to a dying unit
       (UnitGroups[FoundUnit.UnitType] = UnitGroups[fUnitType]) then //They must be the same group type
    begin
      Result := TKMUnitWarrior(FoundUnit);
      Exit;
    end;
  end;
end;


function TKMUnitWarrior.GetActivityText: string;
begin
  case fOrder of
    woNone: ;
    woWalk: ;
    woWalkOut: ;
    woAttackUnit: ;
    woAttackHouse: ;
    woStorm: ;
  end;
  Result := '';
  {if fCurrentAction is TUnitActionFight then
  begin
    if IsRanged then
      Result := fTextLibrary[TX_UNIT_TASK_FIRING]
    else
      Result := fTextLibrary[TX_UNIT_TASK_FIGHTING];
  end
  else if fCurrentAction is TUnitActionStormAttack then
    Result := fTextLibrary[TX_UNIT_TASK_STORM_ATTACK]
  //Sometimes only commanders are given order to walk to the unit, so check their action as well
  else if ((fCurrentAction is TUnitActionWalkTo) and (TUnitActionWalkTo(fCurrentAction).WalkingToUnit)
       or  (GetCommander.fCurrentAction is TUnitActionWalkTo) and (TUnitActionWalkTo(GetCommander.fCurrentAction).WalkingToUnit)) then
    Result := fTextLibrary[TX_UNIT_TASK_ATTACKING]
  else if (fCurrentAction is TUnitActionWalkTo) or (fCurrentAction is TUnitActionAbandonWalk) then
    Result := fTextLibrary[TX_UNIT_TASK_MOVING]
  else if (fUnitTask is TTaskAttackHouse) then
    Result := fTextLibrary[TX_UNIT_TASK_ATTACKING_HOUSE]
  else
    Result := fTextLibrary[TX_UNIT_TASK_IDLE];}
end;


procedure TKMUnitWarrior.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse);
begin
  Assert(aGoDir = gd_GoOutside, 'Walking inside is not implemented yet');
  Assert(aHouse.HouseType = ht_Barracks, 'Only Barracks so far');
  inherited;
  fOrder := woWalkOut;
end;


procedure TKMUnitWarrior.OrderWalk(aLoc: TKMPointDir; aUseExactTarget: Boolean = True);
begin
  Assert(aLoc.Dir <> dir_NA);

  fNewOrder := woWalk;
  fOrderLoc := aLoc;
  fUseExactTarget := aUseExactTarget;
  ClearOrderTarget;
end;


//Attack works like this: Commander tracks target unit in walk action. Members are ordered to walk to formation with commaner at target unit's location.
//If target moves in WalkAction, commander will reissue PlaceOrder with aOnlySetMembers = true, so members will walk to new location.
procedure TKMUnitWarrior.OrderAttackUnit(aTargetUnit: TKMUnit);
begin
  //todo: Support archers attacking units that cannot be reached by foot, e.g. ones up on a wall.

  fNewOrder := woAttackUnit; //Only commander has order Attack, other units have walk to (this means they walk in formation and not in a straight line meeting the enemy one at a time
  fOrderLoc := KMPointDir(aTargetUnit.GetPosition, fOrderLoc.Dir);
  SetOrderHouseTarget(nil);
  SetOrderTarget(aTargetUnit);

  //Only the commander tracks the target, group members are just told to walk to the position
  OrderWalk(KMPointDir(aTargetUnit.GetPosition, fOrderLoc.Dir), True); //Only set members
end;


function TKMUnitWarrior.OrderDone: Boolean;
begin
  Result := False;

  //Did we performed the Order?
  case fOrder of
    woNone:         Result := True;
    woWalk:         begin
                      if not fUseExactTarget or KMSamePoint(GetPosition, fOrderLoc.Loc) then
                        Result := True
                      else
                      begin
                        Result := False;
                        {//Maybe unit from different group took our place
                        U := fTerrain.UnitsHitTest(fOrderLoc.Loc.X, fOrderLoc.Loc.Y);
                        if U <> nil then}
                      end;
                    end;
    woWalkOut:      Result := IsIdle;
    woAttackUnit:   Result := (GetOrderTarget = nil);
    woAttackHouse:  Result := (GetOrderHouseTarget = nil);
    woStorm:        Result := IsIdle;
  end;
end;


//All units are assigned TTaskAttackHouse which does everything for us (move to position, hit house, abandon, etc.) }
procedure TKMUnitWarrior.OrderAttackHouse(aTargetHouse: TKMHouse);
begin
  fNewOrder := woAttackHouse;
  SetOrderTarget(nil);
  SetOrderHouseTarget(aTargetHouse);
end;


procedure TKMUnitWarrior.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fNewOrder, SizeOf(fNewOrder));
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fOrderLoc);
  if fOrderTargetHouse <> nil then
    SaveStream.Write(fOrderTargetHouse.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if fOrderTargetUnit <> nil then
    SaveStream.Write(fOrderTargetUnit.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fRequestedFood);
  SaveStream.Write(fStormDelay);
  SaveStream.Write(fUseExactTarget);
end;


function TKMUnitWarrior.CheckForEnemy: Boolean;
var FoundEnemy: TKMUnit;
begin
  Result := false; //Didn't find anyone to fight
  FoundEnemy := FindEnemy;
  if FoundEnemy = nil then exit;
  FightEnemy(FoundEnemy);
  Result := true; //Found someone
end;


function TKMUnitWarrior.FindEnemy: TKMUnit;
var TestDir: TKMDirection;
begin
  Result := nil; //No one to fight
  if not ENABLE_FIGHTING then exit;
  if not CanInterruptAction then exit;

  if IsRanged then
  begin
    //We are busy with an action (e.g. in a fight)
    if (GetUnitAction <> nil) and GetUnitAction.Locked then Exit;

    //We are shooting at house
    if (fUnitTask <> nil) and (fUnitTask is TTaskAttackHouse) then Exit;

    //Archers should only look for opponents when they are idle or when they are finishing another fight (function is called by TUnitActionFight)
    if (GetUnitAction is TUnitActionWalkTo)
    and ((GetOrderTarget = nil) or GetOrderTarget.IsDeadOrDying or not WithinFightRange(GetOrderTarget.GetPosition))
    then
      Exit;
  end;

  if IsRanged then
    TestDir := Direction //Use direction for ranged attacks, if it was not already specified
  else
    TestDir := dir_NA;

  //This function should not be run too often, as it will take some time to execute (e.g. with lots of warriors in the range area to check)
  Result := fTerrain.UnitsHitTestWithinRad(GetPosition, GetFightMinRange, GetFightMaxRange(true), Owner, at_Enemy, TestDir, not RANDOM_TARGETS);

  //Only stop attacking a house if it's a warrior
  if (fUnitTask <> nil) and (fUnitTask is TTaskAttackHouse) and (GetUnitAction is TUnitActionStay) and not (Result is TKMUnitWarrior) then
    Result := nil;
end;


procedure TKMUnitWarrior.FightEnemy(aEnemy: TKMUnit);
begin
  Assert(aEnemy <> nil, 'Fight no one?');

  //Free the task or set it up to be resumed afterwards
  if UnitTask <> nil then
  begin
    if (UnitTask is TTaskAttackHouse) and not (aEnemy is TKMUnitWarrior) then
      TTaskAttackHouse(UnitTask).Phase := 0 //Reset task so it will resume after the fight
    else
      FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
  end;

  //Attempt to resume walks/attacks after interuption
  if (GetUnitAction is TUnitActionWalkTo)
  and (fOrder = woAttackUnit)
  and not (aEnemy is TKMUnitWarrior) then
  begin
    if GetOrderTarget <> nil then
      fNewOrder := woAttackUnit
    else
      fNewOrder := woWalk;
  end;

  SetActionFight(ua_Work, aEnemy);
  if aEnemy is TKMUnitWarrior then
  begin
    TKMUnitWarrior(aEnemy).CheckForEnemy; //Let opponent know he is attacked
  end;
end;


{ See if we can abandon other actions in favor of more important things }
function TKMUnitWarrior.CanInterruptAction:boolean;
begin
  if GetUnitAction is TUnitActionWalkTo      then Result := TUnitActionWalkTo(GetUnitAction).CanAbandonExternal and GetUnitAction.StepDone else //Only when unit is idling during Interaction pauses
  if(GetUnitAction is TUnitActionStay) and
    (UnitTask      is TTaskAttackHouse)      then Result := true else //We can abandon attack house if the action is stay
  if GetUnitAction is TUnitActionStay        then Result := not GetUnitAction.Locked else //Initial pause before leaving barracks is locked
  if GetUnitAction is TUnitActionAbandonWalk then Result := GetUnitAction.StepDone and not GetUnitAction.Locked else //Abandon walk should never be abandoned, it will exit within 1 step anyway
  if GetUnitAction is TUnitActionGoInOut     then Result := not GetUnitAction.Locked else //Never interupt leaving barracks
  if GetUnitAction is TUnitActionStormAttack then Result := not GetUnitAction.Locked else //Never interupt storm attack
  if GetUnitAction is TUnitActionFight       then Result := IsRanged or not GetUnitAction.Locked //Only allowed to interupt ranged fights
  else Result := true;
end;


function TKMUnitWarrior.UpdateState: Boolean;
begin
  if fCurrentAction = nil then
    raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action at start of TKMUnitWarrior.UpdateState',fCurrPosition);

  if IsDeadOrDying then
  begin
    Result := True; //Required for override compatibility
    inherited UpdateState;
    Exit;
  end;

  if fCondition < UNIT_MIN_CONDITION then
    fThought := th_Eat; //th_Death checked in parent UpdateState

  {//Help out our fellow group members in combat if we are not fighting and someone else is
  if (fState <> ws_Engage) and (ChosenFoe <> nil) then
    if IsRanged then
    begin
      //Archers should abandon walk to start shooting if there is a foe
      if WithinFightRange(ChosenFoe.GetPosition)
      and (GetUnitAction is TUnitActionWalkTo)
      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
      begin
        if TUnitActionWalkTo(GetUnitAction).CanAbandonExternal then
          SetActionStay(0, ua_Walk)
        else
          AbandonWalk;
      end;
      //But if we are already idle then just start shooting right away
      if WithinFightRange(ChosenFoe.GetPosition)
        and(GetUnitAction is TUnitActionStay) then
      begin
        //Archers - If foe is reachable then turn in that direction and CheckForEnemy
        Direction := KMGetDirection(PositionF, ChosenFoe.PositionF);
        AnimStep := UnitStillFrames[Direction];
        CheckForEnemy;
      end;
    end
    else
    begin
      //Melee
      //todo: Try to avoid making a route through other units. Path finding should weight tiles with units high,
      //      tiles with fighting (locked) units very high so we route around the locked the battle rather
      //      than getting stuck trying to walk through fighting units (this will make the fighting system appear smarter)
      fOrder := wo_AttackUnit;
      fState := ws_Engage; //Special state so we don't issue this order continuously
      SetOrderTarget(ChosenFoe);
    end;}


  //Part 1 - Take orders into execution if there are any
  //Part 2 - UpdateState
  //Part 3 -

  //Make sure attack order is still valid
  if ((fNewOrder = woAttackUnit) and (GetOrderTarget = nil))
  or ((fNewOrder = woAttackHouse) and (GetOrderHouseTarget = nil)) then
    fOrder := woNone;

  //Override current action if there's an Order in queue paying attention
  //to unit WalkTo current position (let the unit arrive on next tile first!)
  //As well let the unit finish it's curent Attack action before taking a new order
  //This should make units response a bit delayed.
  case fNewOrder of
    woNone: ;
    woWalk:         begin
                      //We can update existing Walk action with minimum changes
                      if (GetUnitAction is TUnitActionWalkTo)
                      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(fOrderLoc.Loc, 0, fUseExactTarget);
                        fNewOrder := woNone;
                        fOrder := woWalk;
                      end
                      else
                      //Other actions are harder to interrupt
                      if CanInterruptAction then
                      begin
                        FreeAndNil(fUnitTask);
                        SetActionWalkToSpot(fOrderLoc.Loc, ua_Walk, fUseExactTarget);
                        fNewOrder := woNone;
                        fOrder := woWalk;
                      end;
                    end;
    woWalkOut:      ;
    woAttackUnit:   begin
                      if (GetUnitAction is TUnitActionWalkTo)
                      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        //If we are not the commander then walk to near
                        //todo: Do not WalkTo enemies location if we are archers, stay in place
                        TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(GetOrderTarget, GetFightMaxRange);
                        fNewOrder := woNone;
                        fOrder := woAttackUnit;
                      end;

                      //Take attack order
                      if CanInterruptAction
                      and (GetOrderTarget <> nil)
                      and not WithinFightRange(GetOrderTarget.GetPosition) then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        SetActionWalkToUnit(GetOrderTarget, GetFightMaxRange, ua_Walk);
                        fNewOrder := woNone;
                        fOrder := woAttackUnit;
                        //todo: We need a ws_AttackingUnit to make this work properly for archers, so they know to shoot the enemy after finishing the walk and follow him if he keeps moving away.
                        //todo: If an archer is too close to attack, move back
                      end;
                    end;
    woAttackHouse:  begin
                      //Abandon walk so we can take attack house
                      if (GetUnitAction is TUnitActionWalkTo)
                      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
                        AbandonWalk;

                      //Take attack house order
                      if CanInterruptAction then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        fUnitTask := TTaskAttackHouse.Create(Self, GetOrderHouseTarget);
                        fOrderLoc := KMPointDir(GetPosition, fOrderLoc.Dir); //Once the house is destroyed we will position where we are standing
                        fNewOrder := woNone;
                        fOrder := woAttackHouse;
                      end;
                    end;
    woStorm:        begin
                      //Abandon walk so we can take attack house or storm attack order
                      if (GetUnitAction is TUnitActionWalkTo)
                      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
                        AbandonWalk;

                      //Storm
                      if CanInterruptAction then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        SetActionStorm(fStormDelay);
                        fNewOrder := woNone;
                        fOrder := woStorm;
                      end;
                    end;
  end;



  if (fTicker mod 5 = 0) then CheckForEnemy; //Split into seperate procedure so it can be called from other places

  Result := True; //Required for override compatibility
  if inherited UpdateState then exit;


  {//This means we are idle, so make sure our direction is right and if we are commander reposition our troops if needed
  PositioningDone := true;
  if fCommander = nil then
  if (fState = ws_Walking) or (fState = ws_RepositionPause) then
  begin
    //Wait for self and all team members to be in position before we set fState to None (means we no longer worry about group position)
    if not (UnitTask is TTaskAttackHouse) and not (GetUnitAction is TUnitActionWalkTo)
    and not (GetUnitAction is TUnitActionAbandonWalk)
    and not KMSamePoint(GetPosition,fOrderLoc.Loc)
    and CanWalkTo(fOrderLoc.Loc, 0) then
    begin
      SetActionWalkToSpot(fOrderLoc.Loc); //Walk to correct position
      fState := ws_Walking;
    end;

    //If we have no crew then just exit
    if fMembers <> nil then
      //Tell everyone to reposition
      for i:=0 to fMembers.Count-1 do
        //Must wait for unit(s) to get into position before we have truely finished walking
        if PositioningDone then
          PositioningDone := TKMUnitWarrior(fMembers.Items[i]).RePosition
        else
          TKMUnitWarrior(fMembers.Items[i]).RePosition; //We must call it directly like this, if we used the above method then lazy boolean evaluation will skip it.
  end;}

  //Make sure we didn't get given an action above
  if GetUnitAction <> nil then exit;

  SetActionStay(50, ua_Walk);

  {if fState = ws_Walking then
  begin
    fState := ws_RepositionPause; //Means we are in position and waiting until we turn
    SetActionStay(4+KaMRandom(2),ua_Walk); //Pause 0.5 secs before facing right direction. Slight random amount so they don't look so much like robots ;) (actually they still do, we need to add more randoms)
    //Do not check for enemy, let archers face right direction first (enemies could be behind = unattackable)
  end
  else
  begin
    if fState = ws_RepositionPause then
    begin
      if fOrderLoc.Dir <> dir_NA then //This should not be the case but will be used as a temporary fix until we refactor into TGroup
        Direction := fOrderLoc.Dir; //Face the way we were told to after our walk (this creates a short pause before we fix direction)
      CheckForEnemy; //Important for archers, check for enemy once we are in position
      if PositioningDone then
        fState := ws_None;
    end;
    if (GetUnitAction = nil) then //CheckForEnemy could have assigned an action
    begin
      if PositioningDone then
        SetActionStay(50,ua_Walk) //Idle if we did not receive a walk action above
      else
        SetActionStay(5,ua_Walk);
    end;
  end; }

  if fCurrentAction=nil then raise ELocError.Create(fResource.UnitDat[UnitType].UnitName+' has no action at end of TKMUnitWarrior.UpdateState',fCurrPosition);
end;


procedure TKMUnitWarrior.Paint;
var
  Act: TUnitActionType;
  UnitPos: TKMPointF;
  I,K: Integer;
begin
  inherited;
  if not fVisible then exit;
  Act := fCurrentAction.ActionType;

  UnitPos.X := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  UnitPos.Y := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  fRenderPool.AddUnit(fUnitType, Act, Direction, AnimStep, UnitPos.X, UnitPos.Y, fPlayers[fOwner].FlagColor, True);

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fThought, UnitPos.X, UnitPos.Y);

  if SHOW_ATTACK_RADIUS then
    if IsRanged then
    for I := -Round(GetFightMaxRange) - 1 to Round(GetFightMaxRange) do
    for K := -Round(GetFightMaxRange) - 1 to Round(GetFightMaxRange) do
    if InRange(GetLength(I, K), GetFightMinRange, GetFightMaxRange) then
    if fTerrain.TileInMapCoords(GetPosition.X + K, GetPosition.Y + I) then
      fRenderAux.Quad(GetPosition.X + K, GetPosition.Y + I, $40FFFFFF);
end;


end.
