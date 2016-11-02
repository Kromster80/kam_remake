unit KM_Units_Warrior;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KromUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Terrain, KM_Units;


type
  TKMUnitWarrior = class;
  TKMWarriorEvent = procedure(aWarrior: TKMUnitWarrior) of object;
  TKMWarrior2Event = procedure(aWarrior: TKMUnitWarrior; aUnit: TKMUnit) of object;

  //What player has ordered us to do
  TKMWarriorOrder = (
    woNone, //No orders
    woWalk, //Walk somewhere
    woWalkOut, //Walk out of Barracks
    woAttackUnit, //Attack someone
    woAttackHouse, //Attack house
    woStorm //Do Storm attack
  );

  TKMUnitWarrior = class(TKMUnit)
  private
    fNextOrder: TKMWarriorOrder; //New order we should perform as soon as we can change tasks
    fOrder: TKMWarriorOrder; //Order we are performing
    fOrderLoc: TKMPoint; //Dir is the direction to face after order
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use public OrderHouseTarget instead.
    fUseExactTarget: Boolean; //Do we try to reach exact position or is it e.g. unwalkable
    fLastShootTime: Cardinal; //Used to prevent archer rate of fire exploit

    fRequestedFood: Boolean;
    fStormDelay: Word;

    function CanInterruptAction: Boolean;
    procedure FightEnemy(aEnemy: TKMUnit);

    procedure ClearOrderTarget;
    procedure SetOrderTarget(aUnit: TKMUnit);
    function GetOrderTarget: TKMUnit;
    function GetOrderHouseTarget: TKMHouse;
    procedure SetOrderHouseTarget(aHouse: TKMHouse);
    procedure UpdateOrderTargets;

    procedure TakeNextOrder;
    procedure WalkedOut;
  public
    OnWarriorDied: TKMWarriorEvent; //Separate event from OnUnitDied to report to Group
    OnPickedFight: TKMWarrior2Event;
    OnWarriorWalkOut: TKMWarriorEvent;
    FaceDir: TKMDirection; //Direction we should face after walking. Only check for enemies in this direction.

    constructor Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: TKMHandIndex);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure CloseUnit(aRemoveTileUsage: Boolean = True); override;
    destructor Destroy; override;

    function GetWarriorActivityText(aIsAttackingUnit: Boolean): UnicodeString;
    procedure KillUnit(aFrom: TKMHandIndex; aShowAnimation, aForceDelay: Boolean); override;

    //Commands from TKMUnitGroup
    procedure OrderFood;
    procedure OrderNone;
    procedure OrderStorm(aDelay: Word);
    procedure OrderWalk(aLoc: TKMPoint; aUseExactTarget: Boolean = True);
    procedure OrderAttackHouse(aTargetHouse: TKMHouse);
    procedure OrderFight(aTargetUnit: TKMUnit);

    function GetFightMinRange: Single;
    function GetFightMaxRange(aTileBased: Boolean = False): Single;
    function WithinFightRange(Value: TKMPoint): Boolean;
    function OrderDone: Boolean;
    property RequestedFood: Boolean read fRequestedFood write fRequestedFood; //Cleared by Serf delivering food
    property LastShootTime: Cardinal read fLastShootTime;
    function IsRanged: Boolean;
    function InFight(aCountCitizens: Boolean = False): Boolean;
    function InAGroup: Boolean;
    function NeedsToReload(aFightAnimLength: Byte): Boolean;
    procedure SetLastShootTime;
    function FindLinkUnit(aLoc: TKMPoint): TKMUnitWarrior;
    function CheckForEnemy: Boolean;
    function FindEnemy: TKMUnit;
    function PathfindingShouldAvoid: Boolean; override;

    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse); override;

    procedure Save(SaveStream: TKMemoryStream); override;
    function UpdateState: Boolean; override;
    procedure Paint; override;
  end;


implementation
uses
  KM_ResTexts, KM_HandsCollection, KM_RenderPool, KM_RenderAux, KM_UnitTaskAttackHouse, KM_HandLogistics,
  KM_UnitActionAbandonWalk, KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionWalkTo, KM_UnitActionStay,
  KM_UnitActionStormAttack, KM_Resource, KM_ResUnits, KM_Hand,
  KM_ResWares, KM_Game, KM_ResHouses;


{ TKMUnitWarrior }
constructor TKMUnitWarrior.Create(aID: Cardinal; aUnitType: TUnitType; aLoc: TKMPoint; aOwner: TKMHandIndex);
begin
  inherited;
  fOrderTargetUnit   := nil;
  fOrderTargetHouse  := nil;
  fRequestedFood     := False;
  fNextOrder         := woNone;
  fOrder             := woNone;
  fOrderLoc          := aLoc;
end;


constructor TKMUnitWarrior.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fNextOrder, SizeOf(fNextOrder));
  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fOrderLoc);
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fRequestedFood);
  LoadStream.Read(fStormDelay);
  LoadStream.Read(fUseExactTarget);
  LoadStream.Read(FaceDir);
  LoadStream.Read(fLastShootTime);
end;


procedure TKMUnitWarrior.SyncLoad;
begin
  inherited;
  fOrderTargetUnit := TKMUnitWarrior(gHands.GetUnitByUID(cardinal(fOrderTargetUnit)));
  fOrderTargetHouse := gHands.GetHouseByUID(cardinal(fOrderTargetHouse));
  if GetUnitAction is TUnitActionGoInOut then
    TUnitActionGoInOut(GetUnitAction).OnWalkedOut := WalkedOut;
end;


procedure TKMUnitWarrior.CloseUnit;
begin
  //This ensures that pointer usage tracking is reset
  ClearOrderTarget;

  fNextOrder := woNone;
  inherited;
end;


destructor TKMUnitWarrior.Destroy;
begin
  //This ensures that pointer usage tracking is reset
  ClearOrderTarget;

  inherited;
end;


procedure TKMUnitWarrior.KillUnit(aFrom: TKMHandIndex; aShowAnimation, aForceDelay: Boolean);
var AlreadyDeadOrDying: Boolean;
begin
  AlreadyDeadOrDying := IsDeadOrDying; //Inherrited will kill the unit
  inherited;

  //After inherited so script events can still check which group the warrior is from
  if not AlreadyDeadOrDying then
  begin
    ClearOrderTarget; //This ensures that pointer usage tracking is reset

    //Report to Group that we have died
    if Assigned(OnWarriorDied) then
      OnWarriorDied(Self);
  end;
end;


//Order some food for troops
procedure TKMUnitWarrior.OrderFood;
begin
  if (fCondition < (UNIT_MAX_CONDITION * TROOPS_FEED_MAX)) and not fRequestedFood then
  begin
    gHands[fOwner].Deliveries.Queue.AddDemand(nil, Self, wt_Food, 1, dtOnce, diHigh2);
    fRequestedFood := True;
  end;
end;


procedure TKMUnitWarrior.OrderNone;
begin
  ClearOrderTarget;

  fNextOrder := woNone;
  fUseExactTarget := False;
end;


procedure TKMUnitWarrior.OrderStorm(aDelay: Word);
begin
  //Can't order another storm attack until the current one stops
  if GetUnitAction is TUnitActionStormAttack then Exit;

  ClearOrderTarget;

  fNextOrder := woStorm;
  fStormDelay := aDelay;
end;


procedure TKMUnitWarrior.ClearOrderTarget;
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  gHands.CleanUpUnitPointer(fOrderTargetUnit);
  gHands.CleanUpHousePointer(fOrderTargetHouse);
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
  //If the target unit has died then return nil
  //Don't clear fOrderTargetUnit here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetUnit <> nil) and (fOrderTargetUnit.IsDead) then
    Result := nil
  else
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
  //If the target house has been destroyed then return nil
  //Don't clear fOrderTargetHouse here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetHouse <> nil) and (fOrderTargetHouse.IsDestroyed) then
    Result := nil
  else
    Result := fOrderTargetHouse;
end;


//Clear target unit/house if they are dead/destroyed
procedure TKMUnitWarrior.UpdateOrderTargets;
begin
  if (fOrderTargetUnit <> nil) and fOrderTargetUnit.IsDead then
    gHands.CleanUpUnitPointer(fOrderTargetUnit);

  if (fOrderTargetHouse <> nil) and fOrderTargetHouse.IsDestroyed then
    gHands.CleanUpHousePointer(fOrderTargetHouse);
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


//Is unit a part of the group
//(units are independent when leaving barracks, till they find a group to link to)
function TKMUnitWarrior.InAGroup: Boolean;
begin
  //Event is assigned when unit is added to a group, so we can use it as a marker
  Result := Assigned(OnWarriorDied);
end;


//Used to prevent rate of fire exploit
function TKMUnitWarrior.NeedsToReload(aFightAnimLength: Byte): Boolean;
begin
  Result := (fLastShootTime <> 0) and ((gGame.GameTickCount - fLastShootTime) < aFightAnimLength);
end;


//Used to prevent rate of fire exploit
procedure TKMUnitWarrior.SetLastShootTime;
begin
  fLastShootTime := gGame.GameTickCount;
end;


//We are actively fighting with an enemy
function TKMUnitWarrior.InFight(aCountCitizens: Boolean = False): Boolean;
begin
  Result := (GetUnitAction is TUnitActionFight)
            and (aCountCitizens or (TUnitActionFight(GetUnitAction).GetOpponent is TKMUnitWarrior))
            and not TUnitActionFight(GetUnitAction).GetOpponent.IsDeadOrDying;
end;


function TKMUnitWarrior.IsRanged: Boolean;
begin
  Result := gRes.UnitDat[fUnitType].FightType = ft_Ranged;
end;


function TKMUnitWarrior.FindLinkUnit(aLoc: TKMPoint): TKMUnitWarrior;
var
  I: Integer;
  FoundUnits: TList;
  U: TKMUnit;
  Best, L: Single;
begin
  Result := nil;
  Best := MaxSingle;

  FoundUnits := TList.Create;
  gHands[fOwner].Units.GetUnitsInRect(KMRect(aLoc.X-LINK_RADIUS,
                                               aLoc.Y-LINK_RADIUS,
                                               aLoc.X+LINK_RADIUS,
                                               aLoc.Y+LINK_RADIUS),
                                        FoundUnits);

  for I := 0 to FoundUnits.Count - 1 do
  begin
    U := FoundUnits[I];
    if (U is TKMUnitWarrior)
    and (U <> Self)
    and (UnitGroups[U.UnitType] = UnitGroups[fUnitType]) //They must be the same group type
    and TKMUnitWarrior(U).InAGroup then //Check if warrior belongs to some Group
    begin
      L := KMLength(aLoc, U.GetPosition);
      if (L < Best) then
      begin
        Best := L;
        Result := TKMUnitWarrior(U);
      end;
    end;
  end;

  FoundUnits.Free;
end;


//Only the group knows the difference between Walking and Attacking unit, so we need aIsAttackingUnit parameter
function TKMUnitWarrior.GetWarriorActivityText(aIsAttackingUnit: Boolean): UnicodeString;
begin
  //We can't rely on fOrder because it does not get reset, so look at actions/tasks
  if fCurrentAction is TUnitActionFight then
    if IsRanged then
      Result := gResTexts[TX_UNIT_TASK_FIRING]
    else
      Result := gResTexts[TX_UNIT_TASK_FIGHTING]
  else
  if fCurrentAction is TUnitActionStormAttack then
    Result := gResTexts[TX_UNIT_TASK_STORM_ATTACK]
  else
  if fUnitTask is TTaskAttackHouse then
    Result := gResTexts[TX_UNIT_TASK_ATTACKING_HOUSE]
  else
  if fCurrentAction is TUnitActionGoInOut then
    Result := gResTexts[TX_UNIT_TASK_MOVING]
  else
  if fCurrentAction is TUnitActionWalkTo then
    if aIsAttackingUnit then
      Result := gResTexts[TX_UNIT_TASK_ATTACKING]
    else
      Result := gResTexts[TX_UNIT_TASK_MOVING]
  else
    Result := gResTexts[TX_UNIT_TASK_IDLE];
end;


procedure TKMUnitWarrior.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse: TKMHouse);
begin
  Assert(aGoDir = gd_GoOutside, 'Walking inside is not implemented yet');
  Assert(aHouse.HouseType = ht_Barracks, 'Only Barracks so far');
  inherited;

  TUnitActionGoInOut(GetUnitAction).OnWalkedOut := WalkedOut;
end;


procedure TKMUnitWarrior.OrderWalk(aLoc: TKMPoint; aUseExactTarget: Boolean = True);
begin
  ClearOrderTarget;

  fNextOrder := woWalk;
  fOrderLoc := aLoc;
  fUseExactTarget := aUseExactTarget;
end;


function TKMUnitWarrior.OrderDone: Boolean;
begin
  Result := False;
  if fNextOrder <> woNone then Exit; //We haven't had time to take the order yet, so return false

  //Did we performed the Order?
  case fOrder of
    woNone:         Result := True;
    woWalk:         begin
                      if not fUseExactTarget or KMSamePoint(GetPosition, fOrderLoc) then
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
  fNextOrder := woAttackHouse;
  SetOrderHouseTarget(aTargetHouse);
end;


procedure TKMUnitWarrior.OrderFight(aTargetUnit: TKMUnit);
begin
  fNextOrder := woAttackUnit;
  SetOrderTarget(aTargetUnit);
end;


procedure TKMUnitWarrior.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fNextOrder, SizeOf(fNextOrder));
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fOrderLoc);
  if fOrderTargetHouse <> nil then
    SaveStream.Write(fOrderTargetHouse.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if fOrderTargetUnit <> nil then
    SaveStream.Write(fOrderTargetUnit.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fRequestedFood);
  SaveStream.Write(fStormDelay);
  SaveStream.Write(fUseExactTarget);
  SaveStream.Write(FaceDir);
  SaveStream.Write(fLastShootTime);
end;


function TKMUnitWarrior.PathfindingShouldAvoid: Boolean;
begin
  Result := Inherited PathfindingShouldAvoid;
  Result := Result and (fNextOrder = woNone); //If we have been given an order we're about to move somewhere 
end;


function TKMUnitWarrior.CheckForEnemy: Boolean;
var
  NewEnemy: TKMUnit;
begin
  Result := False; //Didn't find anyone to fight

  //Ranged units should not check for enemy while walking or when facing the wrong way
  if IsRanged and ((not IsIdle) or ((FaceDir <> Direction) and (FaceDir <> dir_NA))) then Exit;

  NewEnemy := FindEnemy;
  if NewEnemy <> nil then
  begin
    OnPickedFight(Self, NewEnemy);
    //If the target is close enough attack it now, otherwise OnPickedFight will handle it through Group.OffendersList
    //Remember that AI's AutoAttackRange feature means a melee warrior can pick a fight with someone out of range
    if WithinFightRange(NewEnemy.GetPosition) then
      FightEnemy(NewEnemy);
    Result := True; //Found someone
  end;
end;


function TKMUnitWarrior.FindEnemy: TKMUnit;
var
  TestDir: TKMDirection;
  Range: Single;
begin
  Result := nil; //No one to fight
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

  Range := GetFightMaxRange(true);
  //AI has an "auto attack range" for melee like in TSK/TPR so you can't sneak past them (when idle)
  if not IsRanged and IsIdle and (gHands[fOwner].HandType = hndComputer) then
    Range := Max(Range, gHands[fOwner].AI.Setup.AutoAttackRange);

  //This function should not be run too often, as it will take some time to execute (e.g. with lots of warriors in the range area to check)
  Result := gTerrain.UnitsHitTestWithinRad(GetPosition, GetFightMinRange, Range, Owner, at_Enemy, TestDir, not RANDOM_TARGETS);

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

  SetActionFight(ua_Work, aEnemy);
  if aEnemy is TKMUnitWarrior then
    TKMUnitWarrior(aEnemy).CheckForEnemy; //Let opponent know he is attacked ASAP
end;


{ See if we can abandon other actions in favor of more important things }
function TKMUnitWarrior.CanInterruptAction: Boolean;
begin
  if GetUnitAction is TUnitActionWalkTo      then Result := TUnitActionWalkTo(GetUnitAction).CanAbandonExternal and GetUnitAction.StepDone else //Only when unit is idling during Interaction pauses
  if(GetUnitAction is TUnitActionStay) and
    (UnitTask      is TTaskAttackHouse)      then Result := True else //We can abandon attack house if the action is stay
  if GetUnitAction is TUnitActionStay        then Result := not GetUnitAction.Locked else //Initial pause before leaving barracks is locked
  if GetUnitAction is TUnitActionAbandonWalk then Result := GetUnitAction.StepDone and not GetUnitAction.Locked else //Abandon walk should never be abandoned, it will exit within 1 step anyway
  if GetUnitAction is TUnitActionGoInOut     then Result := not GetUnitAction.Locked else //Never interupt leaving barracks
  if GetUnitAction is TUnitActionStormAttack then Result := not GetUnitAction.Locked else //Never interupt storm attack
  if GetUnitAction is TUnitActionFight       then Result := IsRanged or not GetUnitAction.Locked //Only allowed to interupt ranged fights
  else Result := true;
end;



//Override current action if there's an Order in queue paying attention
//to unit WalkTo current position (let the unit arrive on next tile first!)
//As well let the unit finish it's curent Attack action before taking a new order
//This should make units response a bit delayed.
procedure TKMUnitWarrior.TakeNextOrder;
var
  loc: TKMPoint;
begin
  //Make sure attack orders are still valid
  if ((fNextOrder = woAttackUnit) and (GetOrderTarget = nil))
  or ((fNextOrder = woAttackHouse) and (GetOrderHouseTarget = nil)) then
    fNextOrder := woNone;

  case fNextOrder of
    woNone: ;
    woWalk:         begin
                      //We can update existing Walk action with minimum changes
                      if (GetUnitAction is TUnitActionWalkTo)
                      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse

                        if fUseExactTarget then
                          loc := fOrderLoc
                        else
                          loc := gTerrain.GetClosestTile(fOrderLoc, GetPosition, GetDesiredPassability, False);

                        TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(loc, 0);
                        fNextOrder := woNone;
                        fOrder := woWalk;
                      end
                      else
                      //Other actions are harder to interrupt
                      if CanInterruptAction then
                      begin
                        FreeAndNil(fUnitTask);

                        if fUseExactTarget then
                          loc := fOrderLoc
                        else
                          loc := gTerrain.GetClosestTile(fOrderLoc, GetPosition, GetDesiredPassability, False);

                        SetActionWalkToSpot(loc, ua_Walk);
                        fNextOrder := woNone;
                        fOrder := woWalk;
                      end;
                    end;
    woWalkOut:      ;
    woAttackUnit:   begin
                      if CanInterruptAction then
                      begin
                        FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
                        fNextOrder := woNone;
                        fOrder := woAttackUnit;
                        fOrderLoc := GetOrderTarget.GetPosition;
                        FightEnemy(GetOrderTarget);
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
                        fOrderLoc := GetPosition; //Once the house is destroyed we will position where we are standing
                        fNextOrder := woNone;
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
                        fNextOrder := woNone;
                        fOrder := woStorm;
                      end;
                    end;
  end;
end;


//Warrior has walked out of the Barracks
procedure TKMUnitWarrior.WalkedOut;
begin
  //Report for duty (Groups will link us or create a new group)
  if Assigned(OnWarriorWalkOut) then
    OnWarriorWalkOut(Self);
end;


{procedure TKMUnitWarrior.ChaseTargetUnit;
begin
  if InFight or (GetOrderTarget = nil) or not CanInterruptAction then Exit;

  //--We don't take advantage of ChangeWalkTo yet for the sake of simplicity?
  if IsRanged then
  begin
    //Check target in range, and if not - chase it / back up from it
    if (KMLength(GetPosition, GetOrderTarget.GetPosition) > GetFightMaxRange) then
    begin
      //Too far away
      if (GetUnitAction is TUnitActionWalkTo)
      and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
        TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(GetOrderTarget, GetFightMaxRange)
      else
      if CanInterruptAction then
        SetActionWalkToUnit(GetOrderTarget, GetFightMaxRange, ua_Walk);
    end
    else
    if (KMLength(GetPosition, GetOrderTarget.GetPosition) < GetFightMinRange) then
    begin
      //todo: Archer is too close, back up
    end
    else
      //WithinRange
      FightEnemy(GetOrderTarget);
  end
  else
  //IsMelee
  begin
    if (GetUnitAction is TUnitActionWalkTo)
    and not TUnitActionWalkTo(GetUnitAction).DoingExchange then
      TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(GetOrderTarget, 1)
    else
    if CanInterruptAction then
      SetActionWalkToUnit(GetOrderTarget, 1, ua_Walk);
  end;

  fOrder := woAttackUnit;
  fOrderLoc := GetOrderTarget.GetPosition;
end;}


function TKMUnitWarrior.UpdateState: Boolean;
begin
  if fCurrentAction = nil then
    raise ELocError.Create(gRes.UnitDat[UnitType].GUIName+' has no action at start of TKMUnitWarrior.UpdateState',fCurrPosition);

  if IsDeadOrDying then
  begin
    Result := True; //Required for override compatibility
    inherited UpdateState;
    Exit;
  end;

  UpdateOrderTargets;

  if fCondition < UNIT_MIN_CONDITION then
    fThought := th_Eat; //th_Death checked in parent UpdateState

  //Part 1 - Take orders into execution if there are any
  //Part 2 - UpdateState
  //Part 3 -

  if fNextOrder <> woNone then
    TakeNextOrder;

  if (fTicker mod 8 = 0) and not InFight then
    CheckForEnemy; //Split into seperate procedure so it can be called from other places

  Result := True; //Required for override compatibility
  if inherited UpdateState then exit;

  //Make sure we didn't get an action above
  if GetUnitAction <> nil then
    Exit;

  SetActionStay(50, ua_Walk);
end;


procedure TKMUnitWarrior.Paint;
var
  Act: TUnitActionType;
  UnitPos: TKMPointF;
  I,K: Integer;
begin
  inherited;
  if not fVisible then Exit;

  Act := fCurrentAction.ActionType;
  UnitPos.X := fPosition.X + UNIT_OFF_X + GetSlide(ax_X);
  UnitPos.Y := fPosition.Y + UNIT_OFF_Y + GetSlide(ax_Y);

  fRenderPool.AddUnit(fUnitType, fUID, Act, Direction, AnimStep, UnitPos.X, UnitPos.Y, gHands[fOwner].FlagColor, True);

  if fThought <> th_None then
    fRenderPool.AddUnitThought(fUnitType, Act, Direction, fThought, UnitPos.X, UnitPos.Y);

  if SHOW_ATTACK_RADIUS then
    if IsRanged then
    for I := -Round(GetFightMaxRange) - 1 to Round(GetFightMaxRange) do
    for K := -Round(GetFightMaxRange) - 1 to Round(GetFightMaxRange) do
    if InRange(GetLength(I, K), GetFightMinRange, GetFightMaxRange) then
    if gTerrain.TileInMapCoords(GetPosition.X + K, GetPosition.Y + I) then
      gRenderAux.Quad(GetPosition.X + K, GetPosition.Y + I, $40FFFFFF);
end;


end.
