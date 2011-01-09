unit KM_Units_Warrior;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math,
  KM_CommonTypes, KM_Defaults, KM_Utils, KM_Units, KM_Houses;

type //Possibly melee warrior class? with Archer class separate?
  TKMUnitWarrior = class(TKMUnit)
  private
  {Individual properties}
    fFlagAnim:cardinal;
    fRequestedFood:boolean;
    fTimeSinceHungryReminder:integer;
    fState:TWarriorState; //This property is individual to each unit, including commander
    fOrder:TWarriorOrder;
    fOrderLoc:TKMPointDir; //Dir is the direction to face after order
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use public OrderHouseTarget instead.
  {Commander properties}
    fUnitsPerRow:integer;
    fMembers:TList;
    function RePosition():boolean; //Used by commander to check if troops are standing in the correct position. If not this will tell them to move and return false
    procedure SetUnitsPerRow(aVal:integer);
    function CanInterruptAction:boolean;
    procedure UpdateHungerMessage();

    procedure ClearOrderTarget;
    procedure SetOrderTarget(aUnit:TKMUnit);
    function GetOrderTarget:TKMUnit;
    function GetOrderHouseTarget:TKMHouse;
  public
    fCommander:TKMUnitWarrior; //ID of commander unit, if nil then unit is commander itself and has a shtandart
  {MapEdProperties} //Don't need to be accessed nor saved during gameplay
    fMapEdMembersCount:integer;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad(); override;
    destructor Destroy; override;

    procedure KillUnit; override;

    procedure AddMember(aWarrior:TKMUnitWarrior);
    function GetCommander:TKMUnitWarrior;
    function GetMemberCount:integer;
    property RequestedFood:boolean write fRequestedFood; //Cleared by Serf delivering food
    procedure SetGroupFullCondition;
    procedure SetOrderHouseTarget(aHouse:TKMHouse);
    property GetWarriorState: TWarriorState read fState;

  //Commands from player
    procedure OrderHalt(aTurnAmount:shortint=0; aLineAmount:shortint=0);
    procedure OrderLinkTo(aNewCommander:TKMUnitWarrior); //Joins entire group to NewCommander
    procedure OrderFood;
    procedure OrderSplit; //Split group in half and assign another commander
    procedure OrderStorm;
    procedure OrderSplitLinkTo(aNewCommander:TKMUnitWarrior; aNumberOfMen:integer); //Splits X number of men from the group and adds them to the new commander
    procedure OrderWalk(aLoc:TKMPointDir; aOnlySetMembers:boolean=false); reintroduce; overload;
    procedure OrderWalk(aLoc:TKMPoint; aNewDir:TKMDirection=dir_NA); reintroduce; overload;
    procedure OrderAttackUnit(aTargetUnit:TKMUnit; aOnlySetMembers:boolean=false);
    procedure OrderAttackHouse(aTargetHouse:TKMHouse);

    function GetFightMinRange():single;
    function GetFightMaxRange():single;
    property UnitsPerRow:integer read fUnitsPerRow write SetUnitsPerRow;
    property OrderTarget:TKMUnit read GetOrderTarget write SetOrderTarget;
    property OrderLocDir:TKMPointDir read fOrderLoc write fOrderLoc;
    property GetOrder:TWarriorOrder read fOrder;
    function GetRow:integer;
    function GetRandomFoeFromMembers: TKMUnitWarrior;
    function ArmyIsBusy(IgnoreArchers:boolean=false):boolean;
    procedure ReissueOrder;

    function IsSameGroup(aWarrior:TKMUnitWarrior):boolean;
    function FindLinkUnit(aLoc:TKMPoint):TKMUnitWarrior;

    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse:TKMHouse); override;

    function CheckForEnemy(aDir:TKMDirection=dir_NA):boolean;
    function FindEnemy(aDir:TKMDirection=dir_NA):TKMUnit;
    procedure FightEnemy(aEnemy:TKMUnit);

    procedure Save(SaveStream:TKMemoryStream); override;
    function UpdateState():boolean; override;
    procedure Paint(); override;
  end;


implementation
uses KM_DeliverQueue, KM_Game, KM_TextLibrary, KM_PlayersCollection, KM_Render, KM_Terrain, KM_UnitTaskAttackHouse,
  KM_UnitActionAbandonWalk, KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionWalkTo, KM_UnitActionStay,
  KM_UnitActionStormAttack;


{ TKMUnitWarrior }
constructor TKMUnitWarrior.Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  fCommander         := nil;
  fOrderTargetUnit   := nil;
  fOrderTargetHouse  := nil;
  fRequestedFood     := false;
  fFlagAnim          := 0;
  fTimeSinceHungryReminder := 0;
  fOrder             := wo_None;
  fState             := ws_None;
  fOrderLoc          := KMPointDir(PosX,PosY,0);
  fUnitsPerRow       := 1;
  fMembers           := nil; //Only commander units will have it initialized
  fMapEdMembersCount := 0; //Used only in MapEd
end;


constructor TKMUnitWarrior.Load(LoadStream:TKMemoryStream);
var i,aCount:integer; W:TKMUnitWarrior;
begin
  Inherited;
  LoadStream.Read(fCommander, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fFlagAnim);
  LoadStream.Read(fRequestedFood);
  LoadStream.Read(fTimeSinceHungryReminder);
  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fState, SizeOf(fState));
  LoadStream.Read(fOrderLoc,SizeOf(fOrderLoc));
  LoadStream.Read(fUnitsPerRow);
  LoadStream.Read(aCount);
  if aCount <> 0 then
  begin
    fMembers := TList.Create;
    for i := 1 to aCount do
    begin
      LoadStream.Read(W, 4); //subst on syncload
      fMembers.Add(W);
    end;
  end else
    fMembers := nil;
end;


procedure TKMUnitWarrior.SyncLoad();
var i:integer;
begin
  Inherited;
  fCommander := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fCommander)));
  fOrderTargetUnit := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fOrderTargetUnit)));
  fOrderTargetHouse := TKMHouse(fPlayers.GetHouseByID(cardinal(fOrderTargetHouse)));
  if fMembers<>nil then
    for i:=1 to fMembers.Count do
      fMembers.Items[i-1] := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fMembers.Items[i-1])));
end;


destructor TKMUnitWarrior.Destroy;
begin
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);

  FreeAndNil(fMembers);
  Inherited;
end;


procedure TKMUnitWarrior.KillUnit;
var i,NewCommanderID:integer; Test,Nearest:single; NewCommander:TKMUnitWarrior;
begin
  if IsDeadOrDying then exit; //Don't kill unit if it's already dying

  //Kill group member
  if fCommander <> nil then
  begin
    fCommander.fMembers.Remove((Self));
    //Now make the group reposition if they were idle (halt has IsDead check in case commander is dead too)
    if (fCommander.fState <> ws_Walking) and (not (fUnitTask is TTaskAttackHouse))
    and not fCommander.ArmyIsBusy then
      fCommander.OrderHalt;
  end;

  //Kill group commander
  if fCommander = nil then
  begin
    NewCommander := nil;
    if (fMembers <> nil) and (fMembers.Count <> 0) then
    begin
      //Get nearest neighbour and give him the Flag
      NewCommanderID := 0;
      Nearest := maxSingle;
      for i:=1 to fMembers.Count do begin
        Test := GetLength(GetPosition, TKMUnitWarrior(fMembers.Items[i-1]).GetPosition);
        if Test < Nearest then begin
          Nearest := Test;
          NewCommanderID := i-1;
        end;
      end;

      NewCommander := TKMUnitWarrior(fMembers.Items[NewCommanderID]);
      NewCommander.fCommander := nil; //Become a commander
      NewCommander.fUnitsPerRow := fUnitsPerRow; //Transfer group properties
      NewCommander.fMembers := TList.Create;

      //Transfer all members to new commander
      for i:=1 to fMembers.Count do
        if i-1 <> NewCommanderID then begin
          TKMUnitWarrior(fMembers.Items[i-1]).fCommander := NewCommander; //Reassign new Commander
          NewCommander.fMembers.Add(fMembers.Items[i-1]); //Reassign membership
        end;

      //Make sure units per row is still valid
      NewCommander.fUnitsPerRow := min(NewCommander.fUnitsPerRow,NewCommander.fMembers.Count+1);
      //Now make the new commander reposition or keep walking where we are going (don't stop group walking because leader dies, we could be in danger)
      NewCommander.fOrderLoc := fOrderLoc;
      NewCommander.SetOrderTarget(fOrderTargetUnit);
      NewCommander.SetOrderHouseTarget(fOrderTargetHouse);

      //Transfer walk/attack
      if (GetUnitAction is TUnitActionWalkTo) and (fState = ws_Walking) then
      begin
        if GetOrderTarget <> nil then
          NewCommander.fOrder := wo_AttackUnit
        else
          NewCommander.fOrder := wo_Walk;
      end;
      if fUnitTask is TTaskAttackHouse then
        NewCommander.fOrder := wo_AttackHouse
      else
        //If we were walking/attacking then it is handled above. Otherwise just reposition
        if (fState <> ws_Walking) and not NewCommander.ArmyIsBusy then
          NewCommander.OrderWalk(KMPointDir(NewCommander.GetPosition,fOrderLoc.Dir)); //Else use position of new commander and direction of group

      //Now set ourself to new commander, so that we have some way of referencing units after they die(?)
      fCommander := NewCommander;
    end;
    fPlayers.PlayerAI[byte(fOwner)].CommanderDied(Self, NewCommander); //Tell our AI that we have died so it can update defence positions, etc.
  end;

  ClearOrderTarget; //This ensures that pointer usage tracking is reset

  Inherited;
end;


procedure TKMUnitWarrior.AddMember(aWarrior:TKMUnitWarrior);
begin
  if fCommander <> nil then exit; //Only commanders may have members
  if fMembers = nil then fMembers := TList.Create;
  fMembers.Add(aWarrior);
end;


procedure TKMUnitWarrior.SetGroupFullCondition;
var i:integer;
begin
  SetFullCondition;
  if (fMembers <> nil) then //If we have members then give them full condition too
    for i:=0 to fMembers.Count-1 do
      TKMUnitWarrior(fMembers.Items[i]).SetFullCondition;
end;


{Note that this function returns Members count, Groups count with commander is +1}
function TKMUnitWarrior.GetMemberCount:integer;
begin
  if (fCommander <> nil) or (fMembers = nil) then
    Result := 0
  else
    Result := fMembers.Count;
end;


{Return Commander or Self if unit is single}
function TKMUnitWarrior.GetCommander:TKMUnitWarrior;
begin
  if fCommander <> nil then
    Result := fCommander
  else
    Result := Self;
end;


function TKMUnitWarrior.RePosition():boolean;
var ClosestTile:TKMPoint;
begin
  Result := true;
  if (fState = ws_None) and (Direction <> TKMDirection(fOrderLoc.Dir+1)) then
    fState := ws_RepositionPause; //Make sure we always face the right way if somehow state is gets to None without doing this

  if fOrderLoc.Loc.X = 0 then exit;

  if fState = ws_None then
    ClosestTile := fTerrain.GetClosestTile(fOrderLoc.Loc, GetPosition, CanWalk);

  //See if we are in position already or if we can't reach the position, (closest tile differs from target tile) because we don't retry for that case.
  if (fState = ws_None) and (KMSamePoint(GetPosition,fOrderLoc.Loc) or (not KMSamePoint(ClosestTile,fOrderLoc.Loc))) then
    exit;

  //This means we are not in position, return false and move into position (unless we are currently walking)
  Result := false;
  if CanInterruptAction and (fState = ws_None) and (not (GetUnitAction is TUnitActionWalkTo)) then
  begin
    SetActionWalkToSpot(fOrderLoc.Loc);
    fState := ws_Walking;
  end;
end;


procedure TKMUnitWarrior.OrderHalt(aTurnAmount:shortint=0; aLineAmount:shortint=0);
var HaltPoint: TKMPointDir;
begin
  if IsDead then exit; //Can happen e.g. when entire group dies at once due to hunger
  //Pass command to Commander unit, but avoid recursively passing command to Self
  if (fCommander <> nil) and (fCommander <> Self) then
  begin
    fCommander.OrderHalt(aTurnAmount,aLineAmount);
    exit;
  end;

  if fOrderLoc.Loc.X = 0 then //If it is invalid, use commander's values
    HaltPoint := KMPointDir(NextPosition.X,NextPosition.Y,Direction)
  else
    if fState = ws_Walking then //If we are walking use commander's location, but order Direction
      HaltPoint := KMPointDir(NextPosition.X,NextPosition.Y,fOrderLoc.Dir)
    else
      HaltPoint := fOrderLoc;

  HaltPoint.Dir := byte(KMLoopDirection(HaltPoint.Dir+aTurnAmount+1))-1; //Add the turn amount, using loop in case it goes over 7

  if fMembers <> nil then
    SetUnitsPerRow(fUnitsPerRow+aLineAmount);

  OrderWalk(HaltPoint);
end;


procedure TKMUnitWarrior.OrderLinkTo(aNewCommander:TKMUnitWarrior); //Joins entire group to NewCommander
var i:integer; AddedSelf: boolean;
begin
  //Redirect command so that both units are Commanders
  if (GetCommander<>Self) or (aNewCommander.GetCommander<>aNewCommander) then begin
    GetCommander.OrderLinkTo(aNewCommander.GetCommander);
    exit;
  end;

  //Only link to same group type
  if UnitGroups[byte(fUnitType)] <> UnitGroups[byte(aNewCommander.fUnitType)] then exit;

  //Can't link to self for obvious reasons
  if aNewCommander = Self then exit;

  fCommander := aNewCommander;
  AddedSelf := false;

  //Move our members and self to the new commander
  if fMembers <> nil then
  begin
    for i:=0 to fMembers.Count-1 do
    begin
      //Put the commander in the right place (in to the middle of his members)
      if i = fUnitsPerRow div 2 then
      begin
        aNewCommander.AddMember(Self);
        AddedSelf := true;
      end;
      aNewCommander.AddMember(TKMUnitWarrior(fMembers.Items[i]));
      TKMUnitWarrior(fMembers.Items[i]).fCommander := aNewCommander;
    end;
    FreeAndNil(fMembers); //We are not a commander now so nil our memebers list (they have been moved to new commander)
  end;

  if not AddedSelf then
    aNewCommander.AddMember(Self);

  //Tell commander to reissue the order so that the new members do it
  fCommander.ReissueOrder;
end;


procedure TKMUnitWarrior.OrderSplit; //Split group in half and assign another commander
var i, DeletedCount: integer; NewCommander:TKMUnitWarrior; MultipleTypes: boolean;
begin
  if GetMemberCount = 0 then exit; //Only commanders have members

  //If there are different unit types in the group, split should just split them first
  MultipleTypes := false;
  NewCommander  := nil; //init
  for i := 0 to fMembers.Count-1 do
    if TKMUnitWarrior(fMembers.Items[i]).UnitType <> fUnitType then
    begin
      MultipleTypes := true;
      NewCommander := TKMUnitWarrior(fMembers.Items[i]); //New commander is first unit of different type, for simplicity
      break;
    end;

  //Choose the new commander (if we haven't already due to multiple types) and remove him from members
  if not MultipleTypes then
    NewCommander := fMembers.Items[((fMembers.Count+1) div 2)+(min(fUnitsPerRow,(fMembers.Count+1) div 2) div 2)-1];
  fMembers.Remove(NewCommander);

  NewCommander.fUnitsPerRow := fUnitsPerRow;
  NewCommander.fTimeSinceHungryReminder := fTimeSinceHungryReminder; //If we are hungry then don't repeat message each time we split, give new commander our counter
  NewCommander.fCommander := nil;
  //Commander OrderLoc must always be valid, but because this guy wasn't a commander it might not be
  NewCommander.fOrderLoc := KMPointDir(NewCommander.GetPosition, fOrderLoc.Dir);

  DeletedCount := 0;
  for i := 0 to fMembers.Count-1 do
  begin
    //Either split evenly, or when there are multiple types, split if they are different to the commander (us)
    if (MultipleTypes and(TKMUnitWarrior(fMembers.Items[i-DeletedCount]).UnitType <> fUnitType)) or
      ((not MultipleTypes)and(i-DeletedCount >= fMembers.Count div 2)) then
    begin
      NewCommander.AddMember(fMembers.Items[i-DeletedCount]); //Join new commander
      TKMUnitWarrior(fMembers.Items[i-DeletedCount]).fCommander := NewCommander;
      fMembers.Delete(i-DeletedCount); //Leave this commander
      inc(DeletedCount);
    end; //Else stay with this commander
  end;

  if GetMemberCount = 0 then FreeAndNil(fMembers); //If we had a group of only 2 units

  //Make sure units per row is still valid for both groups
  fUnitsPerRow := min(fUnitsPerRow, GetMemberCount+1);
  NewCommander.fUnitsPerRow := min(fUnitsPerRow, NewCommander.GetMemberCount+1);

  //Tell both commanders to reposition
  OrderHalt;
  NewCommander.OrderHalt;
end;


//Splits X number of men from the group and adds them to the new commander
procedure TKMUnitWarrior.OrderSplitLinkTo(aNewCommander:TKMUnitWarrior; aNumberOfMen:integer);
var i, DeletedCount: integer;
begin
  Assert(aNumberOfMen < GetMemberCount+1); //Not allowed to take the commander, only members (if you want the command too use normal LinkTo)
    
  //Take units from the end of fMembers
  DeletedCount := 0;
  for i := fMembers.Count-1 downto 0 do
    if DeletedCount < aNumberOfMen then
    begin
      aNewCommander.AddMember(fMembers.Items[i]);
      TKMUnitWarrior(fMembers.Items[i]).fCommander := aNewCommander;
      fMembers.Delete(i);
      inc(DeletedCount);
    end;

  if GetMemberCount = 0 then FreeAndNil(fMembers); //All members taken

  //Make sure units per row is still valid
  fUnitsPerRow := min(fUnitsPerRow, GetMemberCount+1);

  //Tell both commanders to reposition
  OrderHalt;
  aNewCommander.OrderHalt;
end;


//Order some food for troops
procedure TKMUnitWarrior.OrderFood;
var i:integer;
begin
  if (fCondition<(UNIT_MAX_CONDITION*TROOPS_FEED_MAX)) and not (fRequestedFood) then begin
    fPlayers.Player[byte(fOwner)].DeliverList.AddNewDemand(nil, Self, rt_Food, 1, dt_Once, di_Norm);
    fRequestedFood := true;
  end;
  //Commanders also tell troops to ask for some food
  if (fCommander = nil) and (fMembers <> nil) then
    for i := 0 to fMembers.Count-1 do
      TKMUnitWarrior(fMembers.Items[i]).OrderFood;
  OrderHalt;
end;


procedure TKMUnitWarrior.OrderStorm;
var i:integer;
begin
  fOrder := wo_Storm;
  fState := ws_None; //Clear other states
  SetOrderTarget(nil);
  SetOrderHouseTarget(nil);

  if (fCommander = nil) and (fMembers <> nil) then
    for i := 0 to fMembers.Count-1 do
      TKMUnitWarrior(fMembers.Items[i]).OrderStorm;
end;


procedure TKMUnitWarrior.ClearOrderTarget;
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);
end;


procedure TKMUnitWarrior.SetOrderTarget(aUnit:TKMUnit);
begin
  //Remove previous value
  ClearOrderTarget;
  if aUnit <> nil then
    fOrderTargetUnit := aUnit.GetUnitPointer; //Else it will be nil from ClearOrderTarget
end;


function TKMUnitWarrior.GetOrderTarget:TKMUnit;
begin
  //If the target unit has died then clear it
  if (fOrderTargetUnit <> nil) and (fOrderTargetUnit.IsDead) then ClearOrderTarget;
  Result := fOrderTargetUnit;
end;


procedure TKMUnitWarrior.SetOrderHouseTarget(aHouse:TKMHouse);
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


//Check which row we are in
function TKMUnitWarrior.GetRow:integer;
var i: integer;
begin
  Result := 1;
  if fCommander <> nil then
    for i:=1 to fCommander.fMembers.Count do
      if Self = TKMUnitWarrior(fCommander.fMembers.Items[i-1]) then
      begin
        Result := (i div fCommander.UnitsPerRow)+1; //First row is 1 not 0
        exit;
      end;
end;


function TKMUnitWarrior.GetRandomFoeFromMembers: TKMUnitWarrior;
var Foes: TList; i: integer;
begin
  Assert(fCommander = nil); //This should only be called for commanders
  Foes := TList.Create;
  if (GetUnitAction is TUnitActionFight) and (TUnitActionFight(GetUnitAction).GetOpponent <> nil)
  and (TUnitActionFight(GetUnitAction).GetOpponent is TKMUnitWarrior) then
    Foes.Add(TUnitActionFight(GetUnitAction).GetOpponent);

  if (fMembers <> nil) and (fMembers.Count > 0) then
    for i:=1 to fMembers.Count do
      if (TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction is TUnitActionFight)
      and (TUnitActionFight(TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction).GetOpponent <> nil)
      and (TUnitActionFight(TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction).GetOpponent is TKMUnitWarrior) then
        Foes.Add(TUnitActionFight(TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction).GetOpponent);

  if Foes.Count > 0 then
    Result := TKMUnitWarrior(Foes.Items[Random(Foes.Count)])
  else
    Result := nil;
  Foes.Free;
end;


//Is the player able to issue orders to our group?
function TKMUnitWarrior.ArmyIsBusy(IgnoreArchers:boolean=false):boolean;
var i: integer;
begin
  Assert(fCommander = nil); //This should only be called for commanders
  Result := false;
  if IgnoreArchers and (GetFightMaxRange >= 2) then exit; //Archers are never busy
  if (GetUnitAction is TUnitActionStormAttack)
  or ((GetUnitAction is TUnitActionFight)and(TUnitActionFight(GetUnitAction).GetOpponent is TKMUnitWarrior)) then
    Result := true //We are busy if the commander is storm attacking or fighting a warrior
  else
    //Busy if a member is fighting a warrior
    if (fMembers <> nil) and (fMembers.Count > 0) then
      for i:=1 to fMembers.Count do
        if (TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction is TUnitActionFight)
        and(TUnitActionFight(TKMUnitWarrior(fMembers.Items[i-1]).GetUnitAction).GetOpponent is TKMUnitWarrior) then
        begin
          Result := true;
          exit;
        end;
end;


//At which range we can fight
function TKMUnitWarrior.GetFightMaxRange():single;
begin
  case fUnitType of
    ut_Bowman:      Result := RANGE_BOWMAN_MAX;
    ut_Arbaletman:  Result := RANGE_ARBALETMAN_MAX;
    else            Result := 1.42; //slightly bigger than sqrt(2) for diagonal fights
  end;
end;


//At which range we can fight
function TKMUnitWarrior.GetFightMinRange():single;
begin
  case fUnitType of
    ut_Bowman:      Result := RANGE_BOWMAN_MIN;
    ut_Arbaletman:  Result := RANGE_ARBALETMAN_MIN;
    else            Result := 1; //Any tile that is not our own
  end;
end;


//See if we are in the same group as aWarrior by comparing commanders
function TKMUnitWarrior.IsSameGroup(aWarrior:TKMUnitWarrior):boolean;
begin
  Result := (GetCommander = aWarrior.GetCommander);
end;


function TKMUnitWarrior.FindLinkUnit(aLoc:TKMPoint):TKMUnitWarrior;
var i,k:integer; FoundUnit:TKMUnit;
begin
  Result := nil;

  //Replacing it with fTerrain.UnitsHitTestWithinRad sounds plausible, but would require
  //to change input parameters to include TKMUnitWarrior, fOwner, UnitType.
  //I think thats just not worth it
  for i:=-LINK_RADIUS to LINK_RADIUS do
  for k:=-LINK_RADIUS to LINK_RADIUS do
  if GetLength(i,k) <= LINK_RADIUS then //Check circle area
  begin
    FoundUnit := fTerrain.UnitsHitTest(aLoc.X+i, aLoc.Y+k); //off-map coords will be skipped
    if (FoundUnit is TKMUnitWarrior) and
       (FoundUnit.GetOwner = fOwner) and
       (FoundUnit.UnitType = fUnitType) then //For initial linking they must be the same type, not just same group type
    begin
      Result := TKMUnitWarrior(FoundUnit);
      exit;
    end;
  end;
end;


procedure TKMUnitWarrior.SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse:TKMHouse);
begin
  Assert(aGoDir = gd_GoOutside, 'Walking inside is not implemented yet');
  Inherited;
  fOrder := wo_WalkOut;
end;


procedure TKMUnitWarrior.SetUnitsPerRow(aVal:integer);
begin
  if (fCommander = nil) and (fMembers <> nil) then
    fUnitsPerRow := EnsureRange(aVal,1,fMembers.Count+1);
  if (fCommander = nil) and (fMembers = nil) and (fMapEdMembersCount<>0) then //Special case for MapEd
    fUnitsPerRow := EnsureRange(aVal,1,fMapEdMembersCount+1);
end;


//Reissue our current order, or just halt if we don't have one
procedure TKMUnitWarrior.ReissueOrder;
begin
  Assert(fCommander = nil);

  if (fUnitTask is TTaskAttackHouse) and (fOrderTargetHouse <> nil) then
    OrderAttackHouse(fOrderTargetHouse)
  else
    if (fOrderTargetUnit <> nil) and (fState = ws_Walking) then
      OrderAttackUnit(fOrderTargetUnit)
    else
      if fState = ws_Walking then
        OrderWalk(fOrderLoc)
      else
        OrderHalt;
end;


//Notice: any warrior can get Order (from its commander), but only commander should get Orders from Player
procedure TKMUnitWarrior.OrderWalk(aLoc:TKMPointDir; aOnlySetMembers:boolean=false);
var i:integer; NewLoc:TKMPoint;
begin
  if (fCommander <> nil) or (not aOnlySetMembers) then
  begin
    fOrder    := wo_Walk;
    fState    := ws_None; //Clear other states
    fOrderLoc := aLoc;
    SetOrderTarget(nil);
    SetOrderHouseTarget(nil);
  end;

  if (fCommander=nil)and(fMembers <> nil) then //Don't give group orders if unit has no crew
  for i:=1 to fMembers.Count do begin
    NewLoc := GetPositionInGroup2(aLoc.Loc.X, aLoc.Loc.Y, TKMDirection(aLoc.Dir+1),
                                  i+1, fUnitsPerRow, fTerrain.MapX, fTerrain.MapY, true); //Allow off map positions so GetClosestTile works properly
    TKMUnitWarrior(fMembers.Items[i-1]).OrderWalk(KMPointDir(NewLoc.X,NewLoc.Y,aLoc.Dir))
  end;
end;


procedure TKMUnitWarrior.OrderWalk(aLoc:TKMPoint; aNewDir:TKMDirection=dir_NA);
var NewP:TKMPointDir;
begin
  //keep old direction if group had an order to walk somewhere
  if aNewDir <> dir_NA then
    NewP := KMPointDir(aLoc.X, aLoc.Y, byte(aNewDir)-1)
  else
  if (aNewDir=dir_NA) and (fOrderLoc.Loc.X <> 0) then
    NewP := KMPointDir(aLoc.X, aLoc.Y, fOrderLoc.Dir)
  else
    NewP := KMPointDir(aLoc.X, aLoc.Y, Direction);

  OrderWalk(NewP);
end;


//Attack works like this: Commander tracks target unit in walk action. Members are ordered to walk to formation with commaner at target unit's location.
//If target moves in WalkAction, commander will reissue PlaceOrder with aOnlySetMembers = true, so members will walk to new location.
procedure TKMUnitWarrior.OrderAttackUnit(aTargetUnit:TKMUnit; aOnlySetMembers:boolean=false);
begin
  //todo: Support archers attacking units that cannot be reached by foot, e.g. ones up on a wall.
  if (fCommander <> nil) or (not aOnlySetMembers) then
  begin
    fOrder := wo_AttackUnit; //Only commander has order Attack, other units have walk to (this means they walk in formation and not in a straight line meeting the enemy one at a time
    fState := ws_None; //Clear other states
    fOrderLoc := KMPointDir(aTargetUnit.GetPosition,fOrderLoc.Dir);
    SetOrderHouseTarget(nil);
    SetOrderTarget(aTargetUnit);
  end;
  //Only the commander tracks the target, group members are just told to walk to the position
  OrderWalk(KMPointDir(aTargetUnit.GetPosition,fOrderLoc.Dir),true); //Only set members
end;


{ Attack House works like this:
All units are assigned TTaskAttackHouse which does everything for us
(move to position, hit house, abandon, etc.) }
procedure TKMUnitWarrior.OrderAttackHouse(aTargetHouse:TKMHouse);
var i: integer;
begin
  fOrder := wo_AttackHouse;
  fState := ws_None; //Clear other states
  SetOrderTarget(nil);
  SetOrderHouseTarget(aTargetHouse);

  //Transmit order to all members if we have any
  if (fCommander = nil) and (fMembers <> nil) then
    for i:=0 to fMembers.Count-1 do
      TKMUnitWarrior(fMembers.Items[i]).OrderAttackHouse(aTargetHouse);
end;


procedure TKMUnitWarrior.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  Inherited;
  if fCommander <> nil then
    SaveStream.Write(fCommander.ID) //Store ID
  else
    SaveStream.Write(Zero);
  if fOrderTargetUnit <> nil then
    SaveStream.Write(fOrderTargetUnit.ID) //Store ID
  else
    SaveStream.Write(Zero);
  if fOrderTargetHouse <> nil then
    SaveStream.Write(fOrderTargetHouse.ID) //Store ID
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fFlagAnim);
  SaveStream.Write(fRequestedFood);
  SaveStream.Write(fTimeSinceHungryReminder);
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fState, SizeOf(fState));
  SaveStream.Write(fOrderLoc,SizeOf(fOrderLoc));
  SaveStream.Write(fUnitsPerRow);
  //Only save members if we are a commander
  if (fMembers <> nil) and (fCommander = nil) then
  begin
    SaveStream.Write(fMembers.Count);
    for i:=1 to fMembers.Count do
      if TKMUnitWarrior(fMembers.Items[i-1]) <> nil then
        SaveStream.Write(TKMUnitWarrior(fMembers.Items[i-1]).ID) //Store ID
      else
        SaveStream.Write(Zero);
  end else
    SaveStream.Write(Zero);
end;


//Tell the player to feed us if we are hungry
procedure TKMUnitWarrior.UpdateHungerMessage();
var i:integer; SomeoneHungry:boolean;
begin
  if (fOwner = MyPlayer.PlayerID) and (fCommander = nil) then
  begin
    SomeoneHungry := (fCondition < UNIT_MIN_CONDITION); //Check commander
    if (fMembers <> nil) and (not SomeoneHungry) then
      for i:=0 to fMembers.Count-1 do
      begin
        SomeoneHungry := SomeoneHungry or (TKMUnitWarrior(fMembers.List[i]).Condition < UNIT_MIN_CONDITION);
        if SomeoneHungry then break;
      end;

    if SomeoneHungry then
    begin
      dec(fTimeSinceHungryReminder);
      if fTimeSinceHungryReminder < 1 then
      begin
        fGame.fGamePlayInterface.MessageIssue(msgUnit,fTextLibrary.GetTextString(296),GetPosition);
        fTimeSinceHungryReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
      end;
    end
    else
      fTimeSinceHungryReminder := 0;
  end;
end;


function TKMUnitWarrior.CheckForEnemy(aDir:TKMDirection=dir_NA):boolean;
var FoundEnemy: TKMUnit;
begin
  Result := false; //Didn't find anyone to fight
  FoundEnemy := FindEnemy(aDir);
  if FoundEnemy = nil then exit;
  FightEnemy(FoundEnemy);
  Result := true; //Found someone
end;


function TKMUnitWarrior.FindEnemy(aDir:TKMDirection=dir_NA):TKMUnit;
begin
  Result := nil; //No one to fight
  if not ENABLE_FIGHTING then exit;
  if not CanInterruptAction then exit;
  //Archers should only look for opponents when they are idle or when they are finishing another fight (function is called by TUnitActionFight)
  if (GetFightMaxRange >= 2) and (((not (GetUnitAction is TUnitActionStay)) and
                                 not((GetUnitAction is TUnitActionFight) and not GetUnitAction.Locked))
                                 or (GetUnitTask is TTaskAttackHouse)) then exit; //Never look for enemies when shooting a house

  if (aDir = dir_NA) and (GetFightMaxRange >= 2) then
    aDir := Direction; //Use direction for ranged attacks, if it was not already specified

  //This function should not be run too often, as it will take some time to execute (e.g. with lots of warriors in the range area to check)
  Result := fTerrain.UnitsHitTestWithinRad(GetPosition, GetFightMinRange, GetFightMaxRange, GetOwner, at_Enemy, aDir);
  //Only stop attacking a house if it's a warrior
  if (GetUnitTask is TTaskAttackHouse) and not (Result is TKMUnitWarrior) then
    Result := nil;
end;


procedure TKMUnitWarrior.FightEnemy(aEnemy:TKMUnit);
begin
  Assert(aEnemy <> nil, 'Fight no one?');

  //Free the task or set it up to be resumed afterwards
  if GetUnitTask <> nil then
  begin
    if (GetUnitTask is TTaskAttackHouse) and not (aEnemy is TKMUnitWarrior) then
      TTaskAttackHouse(GetUnitTask).Phase := 0 //Reset task so it will resume after the fight
    else
      FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
  end;

  //Attempt to resume walks/attacks after interuption
  if (GetUnitAction is TUnitActionWalkTo) and (fState = ws_Walking) and not (aEnemy is TKMUnitWarrior) then
  begin
    if GetOrderTarget <> nil then
      fOrder := wo_AttackUnit
    else
      fOrder := wo_Walk;
  end;

  SetActionFight(ua_Work, aEnemy);
  if aEnemy is TKMUnitWarrior then
  begin
    TKMUnitWarrior(aEnemy).CheckForEnemy; //Let opponent know he is attacked
    if fCommander = nil then fOrderLoc := KMPointDir(GetPosition,fOrderLoc.Dir); //so that after the fight we stay where we are
  end;
end;


{ See if we can abandon other actions in favor of more important things }
function TKMUnitWarrior.CanInterruptAction:boolean;
begin
  if GetUnitAction is TUnitActionWalkTo      then Result := TUnitActionWalkTo(GetUnitAction).CanAbandonExternal and GetUnitAction.StepDone else //Only when unit is idling during Interaction pauses
  if(GetUnitAction is TUnitActionStay) and
    (GetUnitTask   is TTaskAttackHouse)      then Result := true else //We can abandon attack house if the action is stay
  if GetUnitAction is TUnitActionStay        then Result := not GetUnitAction.Locked else //Initial pause before leaving barracks is locked
  if GetUnitAction is TUnitActionAbandonWalk then Result := GetUnitAction.StepDone and not GetUnitAction.Locked else //Abandon walk should never be abandoned, it will exit within 1 step anyway
  if GetUnitAction is TUnitActionGoInOut     then Result := not GetUnitAction.Locked else //Never interupt leaving barracks
  if GetUnitAction is TUnitActionStormAttack then Result := not GetUnitAction.Locked else //Never interupt storm attack
  if GetUnitAction is TUnitActionFight       then Result := (GetFightMaxRange >= 2) or not GetUnitAction.Locked //Only allowed to interupt ranged fights
  else Result := true;
end;


function TKMUnitWarrior.UpdateState():boolean;
var
  i:integer;
  PositioningDone:boolean;
  ChosenFoe: TKMUnitWarrior;
begin
  if IsDeadOrDying then
  begin
    Result:=true; //Required for override compatibility
    Inherited UpdateState;
    exit;
  end;

  if fCommander <> nil then
  begin
    if fCommander.IsDeadOrDying then fGame.GameError(GetPosition, 'fCommander.IsDeadOrDying');
    if fCommander.fCommander <> nil then fGame.GameError(GetPosition, 'fCommander.fCommander <> nil');
  end;
  if GetCommander.fCommander <> nil then fGame.GameError(GetPosition, 'GetCommander.fCommander <> nil');

  inc(fFlagAnim);
  if fCondition < UNIT_MIN_CONDITION then fThought := th_Eat; //th_Death checked in parent UpdateState
  if fFlagAnim mod 10 = 0 then UpdateHungerMessage();

  //Choose a random foe from our commander, then use that from here on (only if needed and not every tick)
  if (fFlagAnim mod 10 = 0) and GetCommander.ArmyIsBusy then
    ChosenFoe := GetCommander.GetRandomFoeFromMembers
  else
    ChosenFoe := nil;

  if (fState = ws_Engage) and ((not GetCommander.ArmyIsBusy) or (not(GetUnitAction is TUnitActionWalkTo))) then
  begin
    fState := ws_None; //As soon as combat is over set the state back
    //Tell commanders to reposition after a fight
    if fCommander = nil then OrderWalk(GetPosition); //Don't use halt because that returns us to fOrderLoc
  end;

  //Help out our fellow group members in combat if we are not fighting and someone else is
  if (fState <> ws_Engage) and (ChosenFoe <> nil) then
    if GetFightMaxRange < 2 then
    begin
      //Melee
      fOrder := wo_AttackUnit;
      fState := ws_Engage; //Special state so we don't issue this order continuously
      SetOrderTarget(ChosenFoe);
    end
    else
    begin
      //Archers should abandon walk to start shooting if there is a foe
      if InRange(GetLength(NextPosition, ChosenFoe.GetPosition), GetFightMinRange, GetFightMaxRange)
      and(GetUnitAction is TUnitActionWalkTo)and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then
        AbandonWalk;
      //But if we are already idle then just start shooting right away
      if InRange(GetLength(GetPosition, ChosenFoe.GetPosition), GetFightMinRange, GetFightMaxRange)
        and(GetUnitAction is TUnitActionStay) then
      begin
        //Archers - If foe is reachable then turn in that direction and CheckForEnemy
        Direction := KMGetDirection(GetPosition, ChosenFoe.GetPosition);
        CheckForEnemy;
      end;
    end;

  //Override current action if there's an Order in queue paying attention
  //to unit WalkTo current position (let the unit arrive on next tile first!)
  //As well let the unit finish it's curent Attack action before taking a new order
  //This should make units response a bit delayed.


  //New walking order
  if (fOrder=wo_Walk) then begin
    //Change WalkTo
    if (GetUnitAction is TUnitActionWalkTo)and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then begin
      if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
      TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(fOrderLoc.Loc, 0, fCommander <> nil);
      fOrder := wo_None;
      fState := ws_Walking;
    end
    else
    //Set WalkTo
    if CanInterruptAction then
    begin
      if GetUnitTask <> nil then FreeAndNil(fUnitTask);
      if fCommander = nil then
        SetActionWalkToSpot(fOrderLoc.Loc)
      else
        SetActionWalkToNear(fOrderLoc.Loc);
      fOrder := wo_None;
      fState := ws_Walking;
    end;
  end;


  //Make sure attack order is still valid
  if (fOrder=wo_AttackUnit) and (GetOrderTarget = nil) then fOrder := wo_None;
  if (fOrder=wo_AttackHouse) and (GetOrderHouseTarget = nil) then fOrder := wo_None;

  //Change walk in order to attack
  if (fOrder=wo_AttackUnit) and (GetUnitAction is TUnitActionWalkTo) //If we are already walking then change the walk to the new location
  and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then begin
    if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
    //If we are not the commander then walk to near
    TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(GetOrderTarget.NextPosition, GetFightMaxRange, fCommander <> nil, GetOrderTarget);
    fOrder := wo_None;
    if (fState <> ws_Engage) then fState := ws_Walking;
  end;

  //Take attack order
  if (fOrder=wo_AttackUnit) and CanInterruptAction then
  begin
    if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
    SetActionWalkToUnit(GetOrderTarget, GetFightMaxRange, ua_Walk);
    fOrder := wo_None;
    //todo: We need a ws_AttackingUnit to make this work properly for archers, so they know to shoot the enemy after finishing the walk and follow him if he keeps moving away.
    //todo: If an archer is too close to attack, move back
    if (fState <> ws_Engage) then fState := ws_Walking; //Difference between walking and attacking is not noticable, since when we reach the enemy we start fighting
  end;

  //Abandon walk so we can take attack house or storm attack order
  if ((fOrder=wo_AttackHouse) or (fOrder=wo_Storm)) and (GetUnitAction is TUnitActionWalkTo)
  and(not TUnitActionWalkTo(GetUnitAction).DoingExchange) then
    AbandonWalk;

  //Take attack house order
  if (fOrder=wo_AttackHouse) and CanInterruptAction then
  begin
    if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
    SetUnitTask := TTaskAttackHouse.Create(Self,GetOrderHouseTarget);
    fOrderLoc := KMPointDir(GetPosition,fOrderLoc.Dir); //Once the house is destroyed we will position where we are standing
    fOrder := wo_None;
  end;

  //Storm
  if (fOrder=wo_Storm) and CanInterruptAction then
  begin
    if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
    SetActionStorm(GetRow);
    fOrder := wo_None;
    fState := ws_None; //Not needed for storm attack
  end;

  if fFlagAnim mod 10 = 0 then CheckForEnemy; //Split into seperate procedure so it can be called from other places

  Result:=true; //Required for override compatibility
  if Inherited UpdateState then exit;


  //This means we are idle, so make sure our direction is right and if we are commander reposition our troops if needed
  PositioningDone := true;
  if fCommander = nil then
  if (fState = ws_Walking) or (fState = ws_RepositionPause) then
  begin
    //Wait for self and all team members to be in position before we set fState to None (means we no longer worry about group position)
    if (not (GetUnitTask is TTaskAttackHouse)) and (not (GetUnitAction is TUnitActionWalkTo)) and
       (not KMSamePoint(GetPosition,fOrderLoc.Loc)) and fTerrain.Route_CanBeMade(GetPosition,fOrderLoc.Loc,GetDesiredPassability,0, false) then
    begin
      SetActionWalkToSpot(fOrderLoc.Loc); //Walk to correct position
      fState := ws_Walking;
    end;

    //If we have no crew then just exit
    if fMembers <> nil then
      //Tell everyone to reposition
      for i:=0 to fMembers.Count-1 do
        //Must wait for unit(s) to get into position before we have truely finished walking
        PositioningDone := TKMUnitWarrior(fMembers.Items[i]).RePosition and PositioningDone; //NOTE: RePosition function MUST go before PositioningDone variable otherwise it won't check the second value if the first is true!!!
  end;

  //Make sure we didn't get given an action above
  if GetUnitAction <> nil then exit;
    
  if fState = ws_Walking then
  begin
    fState := ws_RepositionPause; //Means we are in position and waiting until we turn
    SetActionStay(4+Random(2),ua_Walk); //Pause 0.5 secs before facing right direction. Slight random amount so they don't look so much like robots ;) (actually they still do, we need to add more randoms)
    CheckForEnemy(TKMDirection(fOrderLoc.Dir+1)); //Check for enemy once here, mainly important to make archers shoot as soon as they have finished walking
  end
  else
  begin
    if fState = ws_RepositionPause then
    begin
      Direction := TKMDirection(fOrderLoc.Dir+1); //Face the way we were told to after our walk (this creates a short pause before we fix direction)
      if PositioningDone then
        fState := ws_None;
    end;
    if PositioningDone then
      SetActionStay(50,ua_Walk) //Idle if we did not receive a walk action above
    else
      SetActionStay(5,ua_Walk);
  end;

  if fCurrentAction = nil then
    fGame.GameError(GetPosition, 'Warrior has no action');
end;


procedure TKMUnitWarrior.Paint();
var
  UnitTyp, AnimAct, AnimDir, TeamColor:byte;
  XPaintPos, YPaintPos: single;
  i:integer;
  UnitPosition: TKMPoint;
begin
  Inherited;
  if not fVisible then exit;
  UnitTyp  := byte(fUnitType);
  AnimAct  := byte(fCurrentAction.GetActionType); //should correspond with UnitAction
  AnimDir  := byte(Direction);

  XPaintPos := fPosition.X + 0.5 + GetSlide(ax_X);
  YPaintPos := fPosition.Y + 1   + GetSlide(ax_Y);

  fRender.RenderUnit(UnitTyp, AnimAct, AnimDir, AnimStep, byte(fOwner), XPaintPos, YPaintPos, true);

  //Paint flag
  if (fCommander=nil) and not IsDeadOrDying then begin
    //todo: Fix flag offsets
    //XPaintPos := XPaintPos + FlagXOffset[UnitType]/CELL_SIZE_PX;
    YPaintPos := YPaintPos + FlagYOffset[UnitTyp]/CELL_SIZE_PX; //@Lewin: Feel free to tweak FlagHeight, needs also Xoffset depending on direction (E/W)
    TeamColor := byte(fOwner);
    if (fPlayers.Selected is TKMUnitWarrior) and (TKMUnitWarrior(fPlayers.Selected).GetCommander = Self) then TeamColor := byte(play_animals); //Highlight with White color
    fRender.RenderUnitFlag(UnitTyp,   9, AnimDir, fFlagAnim, TeamColor, XPaintPos, YPaintPos, false);
  end;

  if fThought<>th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, YPaintPos);

  //Paint members in MapEd mode
  if fMapEdMembersCount<>0 then
  for i:=1 to fMapEdMembersCount do begin
    UnitPosition := GetPositionInGroup2(GetPosition.X, GetPosition.Y, Direction, i+1, fUnitsPerRow, fTerrain.MapX, fTerrain.MapY);
    XPaintPos := UnitPosition.X + 0.5; //MapEd units don't have sliding anyway
    YPaintPos := UnitPosition.Y + 1  ;
    fRender.RenderUnit(UnitTyp, AnimAct, AnimDir, AnimStep, byte(fOwner), XPaintPos, YPaintPos, true);
  end;
end;


end.
