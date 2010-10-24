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
    fFoe:TKMUnitWarrior; //An enemy unit which is currently in combat with one of our memebers (commander use only!) Use only public Foe property!
    fUnitsPerRow:integer;
    fMembers:TList;
    function RePosition: boolean; //Used by commander to check if troops are standing in the correct position. If not this will tell them to move and return false
    procedure SetUnitsPerRow(aVal:integer);
    procedure UpdateHungerMessage();

    procedure SetFoe(aUnit:TKMUnitWarrior);
    function GetFoe:TKMUnitWarrior;

    procedure ClearOrderTarget;
    procedure SetOrderTarget(aUnit:TKMUnit);
    function GetOrderTarget:TKMUnit;
  public
    fCommander:TKMUnitWarrior; //ID of commander unit, if nil then unit is commander itself and has a shtandart
  {MapEdProperties} //Don't need to be accessed nor saved during gameplay
    fMapEdMembersCount:integer;
    constructor Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad(); override;
    destructor Destroy; override;

    procedure KillUnit; override;
  //Commands
    procedure AddMember(aWarrior:TKMUnitWarrior);
    function GetCommander:TKMUnitWarrior;
    function GetMemberCount:integer;
    procedure Halt(aTurnAmount:shortint=0; aLineAmount:shortint=0);
    procedure LinkTo(aNewCommander:TKMUnitWarrior); //Joins entire group to NewCommander
    procedure Split; //Split group in half and assign another commander
    procedure SplitLinkTo(aNewCommander:TKMUnitWarrior; aNumberOfMen:integer); //Splits X number of men from the group and adds them to the new commander

    procedure SetGroupFullCondition;
    procedure OrderFood;
    procedure SetOrderHouseTarget(aHouse:TKMHouse);
    function GetOrderHouseTarget:TKMHouse;

    property RequestedFood:boolean write fRequestedFood; //Cleared by Serf delivering food
    property GetWarriorState: TWarriorState read fState;
    function GetFightType():TFightType;
    property UnitsPerRow:integer read fUnitsPerRow write SetUnitsPerRow;
    property OrderTarget:TKMUnit read GetOrderTarget write SetOrderTarget;
    property Foe:TKMUnitWarrior read GetFoe write SetFoe;
    property OrderLocDir:TKMPointDir read fOrderLoc write fOrderLoc;

    function IsSameGroup(aWarrior:TKMUnitWarrior):boolean;
    function FindLinkUnit(aLoc:TKMPoint):TKMUnitWarrior;

    procedure SetActionGoIn(aAction: TUnitActionType; aGoDir: TGoInDirection; aHouse:TKMHouse); override;

    procedure PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPointDir; aOnlySetMemebers:boolean=false); reintroduce; overload;
    procedure PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPoint; aNewDir:TKMDirection=dir_NA); reintroduce; overload;
    procedure PlaceOrder(aWarriorOrder:TWarriorOrder; aTargetUnit:TKMUnit; aOnlySetMemebers:boolean=false); reintroduce; overload;
    procedure PlaceOrder(aWarriorOrder:TWarriorOrder; aTargetHouse:TKMHouse); reintroduce; overload;
    function CheckForEnemy():boolean;

    function CanInterruptAction:boolean;

    procedure Save(SaveStream:TKMemoryStream); override;
    function UpdateState():boolean; override;
    procedure Paint(); override;
  end;


implementation
uses KM_DeliverQueue, KM_Game, KM_LoadLib, KM_PlayersCollection, KM_Render, KM_Terrain, KM_UnitTaskAttackHouse,
  KM_UnitActionAbandonWalk, KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionWalkTo, KM_UnitActionStay;


{ TKMwarrior }
constructor TKMUnitWarrior.Create(const aOwner: TPlayerID; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited;
  fCommander    := nil;
  fOrderTargetUnit  := nil;
  fOrderTargetHouse := nil;
  fFoe          := nil;
  fRequestedFood  := false;
  fFlagAnim     := 0;
  fTimeSinceHungryReminder := 0;
  fOrder        := wo_None;
  fState        := ws_None;
  fOrderLoc     := KMPointDir(PosX,PosY,0);
  fUnitsPerRow  := 1;
  fMembers      := nil; //Only commander units will have it initialized
  fMapEdMembersCount := 0; //Used only in MapEd
end;


constructor TKMUnitWarrior.Load(LoadStream:TKMemoryStream);
var i,aCount:integer; W:TKMUnitWarrior;
begin
  Inherited;
  LoadStream.Read(fCommander, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fFoe, 4); //subst on syncload
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
  fFoe := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fFoe)));
  if fMembers<>nil then
    for i:=1 to fMembers.Count do
      fMembers.Items[i-1] := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fMembers.Items[i-1])));
end;


destructor TKMUnitWarrior.Destroy;
begin
  if fOrderTargetUnit<>nil then fOrderTargetUnit.ReleaseUnitPointer;
  if fOrderTargetHouse<>nil then fOrderTargetHouse.ReleaseHousePointer;
  if fFoe<>nil then fFoe.ReleaseUnitPointer;

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
    //Now make the group reposition (halt has IsDead check in case commander is dead too)
    fCommander.Halt;
  end;

  //Kill group commander
  if (fCommander = nil) and (fMembers <> nil) and (fMembers.Count <> 0) then
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
    //Use OrderLoc if possible
    if fOrderLoc.Loc.X <> 0 then
      NewCommander.PlaceOrder(wo_Walk,fOrderLoc)
    else
      NewCommander.PlaceOrder(wo_Walk,NewCommander.GetPosition,NewCommander.Direction); //Else use position of new commander

    //Now set ourself to new commander, so that we have some way of referencing units after they die(?)
    fCommander := NewCommander;
  end;

  ClearOrderTarget; //This ensures that pointer usage tracking is reset
  SetFoe(nil); //This ensures that pointer usage tracking is reset

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


function TKMUnitWarrior.RePosition: boolean;
var ClosestTile:TKMPoint;
begin
  Result := true;
  if (fState = ws_None) and (Direction <> TKMDirection(fOrderLoc.Dir+1)) then
    fState := ws_RepositionPause; //Make sure we always face the right way if somehow state is gets to None without doing this

  if fOrderLoc.Loc.X = 0 then exit;

  if fState = ws_None then
    ClosestTile := fTerrain.GetClosestTile(fOrderLoc.Loc,GetPosition,canWalk);

  //See if we are in position or if we can't reach position, because we don't retry for that case.
  if (fState = ws_None) and (KMSamePoint(GetPosition,ClosestTile) or (not KMSamePoint(ClosestTile,fOrderLoc.Loc))) then
    exit;

  //This means we are not in position, return false and move into position (unless we are currently walking)
  Result := false;
  if (fState = ws_None) and (not (GetUnitAction is TUnitActionWalkTo)) then
  begin
    SetActionWalk(ClosestTile);
    fState := ws_Walking;
  end;
end;


procedure TKMUnitWarrior.Halt(aTurnAmount:shortint=0; aLineAmount:shortint=0);
var HaltPoint: TKMPointDir;
begin
  if IsDead then exit; //Can happen e.g. when entire group dies at once due to hunger
  //Pass command to Commander unit, but avoid recursively passing command to Self
  if (fCommander <> nil) and (fCommander <> Self) then
  begin
    fCommander.Halt(aTurnAmount,aLineAmount);
    exit;
  end;

  if fOrderLoc.Loc.X = 0 then //If it is invalid, use commander's values
    HaltPoint := KMPointDir(NextPosition.X,NextPosition.Y,byte(Direction)-1)
  else
    if fState = ws_Walking then //If we are walking use commander's location, but order Direction
      HaltPoint := KMPointDir(NextPosition.X,NextPosition.Y,fOrderLoc.Dir)
    else
      HaltPoint := fOrderLoc;

  HaltPoint.Dir := byte(KMLoopDirection(HaltPoint.Dir+aTurnAmount+1))-1; //Add the turn amount, using loop in case it goes over 7

  if fMembers <> nil then
    SetUnitsPerRow(fUnitsPerRow+aLineAmount);

  PlaceOrder(wo_Walk, HaltPoint);
end;


procedure TKMUnitWarrior.LinkTo(aNewCommander:TKMUnitWarrior); //Joins entire group to NewCommander
var i:integer; AddedSelf: boolean;
begin
  //Redirect command so that both units are Commanders
  if (GetCommander<>Self) or (aNewCommander.GetCommander<>aNewCommander) then begin
    GetCommander.LinkTo(aNewCommander.GetCommander);
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
  //Tell commander to reposition
  fCommander.Halt;
end;


procedure TKMUnitWarrior.Split; //Split group in half and assign another commander
var i, DeletedCount: integer; NewCommander:TKMUnitWarrior; MultipleTypes: boolean;
begin
  if GetMemberCount = 0 then exit; //Only commanders have members

  //If there are different unit types in the group, split should just split them first
  MultipleTypes := false;
  NewCommander  := nil; //init
  for i := 0 to fMembers.Count-1 do
    if TKMUnitWarrior(fMembers.Items[i]).GetUnitType <> fUnitType then
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
    if (MultipleTypes and(TKMUnitWarrior(fMembers.Items[i-DeletedCount]).GetUnitType <> fUnitType)) or
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
  Halt;
  NewCommander.Halt;
end;

//Splits X number of men from the group and adds them to the new commander
procedure TKMUnitWarrior.SplitLinkTo(aNewCommander:TKMUnitWarrior; aNumberOfMen:integer);
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
  Halt;
  aNewCommander.Halt;
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
  Halt;
end;


procedure TKMUnitWarrior.ClearOrderTarget;
begin
  //Set fOrderTarget to nil, removing pointer if it's still valid
  if fOrderTargetUnit <> nil then
  begin
    fOrderTargetUnit.ReleaseUnitPointer;
    fOrderTargetUnit := nil;
  end;
  if fOrderTargetHouse <> nil then
  begin
    fOrderTargetHouse.ReleaseHousePointer;
    fOrderTargetHouse := nil;
  end;
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


//Set fFoe to nil, removing pointer if it's still valid
procedure TKMUnitWarrior.SetFoe(aUnit:TKMUnitWarrior);
begin
  if fFoe <> nil then begin
    fFoe.ReleaseUnitPointer;
    fFoe := nil;
  end;

  if aUnit <> nil then
    fFoe := TKMUnitWarrior(aUnit.GetUnitPointer) //Else it will be nil from ClearFoe
end;


//If the target unit has died then clear it
function TKMUnitWarrior.GetFoe:TKMUnitWarrior;
begin
  if (fFoe <> nil) and (fFoe.IsDead) then begin
    fFoe.ReleaseUnitPointer;
    fFoe := nil;
  end;
  Result := fFoe;
end;


function TKMUnitWarrior.GetFightType():TFightType;
begin
  Result := WarriorFightType[fUnitType];
end;


//See if we are in the same group as aWarrior by comparing commanders
function TKMUnitWarrior.IsSameGroup(aWarrior:TKMUnitWarrior):boolean;
begin
  Result := (GetCommander = aWarrior.GetCommander);
end;


function TKMUnitWarrior.FindLinkUnit(aLoc:TKMPoint):TKMUnitWarrior;
var i,k:integer; FoundUnit: TKMUnit;
begin
  Result := nil;
  for i:=-LINK_RADIUS to LINK_RADIUS do
  for k:=-LINK_RADIUS to LINK_RADIUS do
  if GetLength(i,k) <= LINK_RADIUS then //Check circle area
  begin
    FoundUnit := fPlayers.Player[byte(fOwner)].UnitsHitTest(aLoc.X+i, aLoc.Y+k);
    if (FoundUnit is TKMUnitWarrior) and
       (FoundUnit.GetUnitType = GetUnitType) then //For initial linking they must be the same type, not just same group type
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


//Notice: any warrior can get Order (from its commander), but only commander should get Orders from Player
procedure TKMUnitWarrior.PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPointDir; aOnlySetMemebers:boolean=false);
var i:integer; NewLoc:TKMPoint;
begin
  if (fCommander <> nil) or (not aOnlySetMemebers) then
  begin
    fOrder    := aWarriorOrder;
    fState    := ws_None; //Clear other states
    fOrderLoc := aLoc;
  end;

  if (fCommander=nil)and(fMembers <> nil) then //Don't give group orders if unit has no crew
  for i:=1 to fMembers.Count do begin
    NewLoc := GetPositionInGroup2(aLoc.Loc.X, aLoc.Loc.Y, TKMDirection(aLoc.Dir+1),
                                  i+1, fUnitsPerRow, fTerrain.MapX, fTerrain.MapY);
    TKMUnitWarrior(fMembers.Items[i-1]).PlaceOrder(aWarriorOrder, KMPointDir(NewLoc.X,NewLoc.Y,aLoc.Dir))
  end;
end;


procedure TKMUnitWarrior.PlaceOrder(aWarriorOrder:TWarriorOrder; aLoc:TKMPoint; aNewDir:TKMDirection=dir_NA);
var NewP:TKMPointDir;
begin
  //keep old direction if group had an order to walk somewhere
  if aNewDir <> dir_NA then
    NewP := KMPointDir(aLoc.X, aLoc.Y, byte(aNewDir)-1)
  else
  if (aNewDir=dir_NA) and (fOrderLoc.Loc.X <> 0) then
    NewP := KMPointDir(aLoc.X, aLoc.Y, fOrderLoc.Dir)
  else
    NewP := KMPointDir(aLoc.X, aLoc.Y, byte(Direction)-1);

  PlaceOrder(aWarriorOrder, NewP);
end;


procedure TKMUnitWarrior.PlaceOrder(aWarriorOrder:TWarriorOrder; aTargetUnit:TKMUnit; aOnlySetMemebers:boolean=false);
begin
  if (aWarriorOrder <> wo_Attack) or (aTargetUnit = nil) then exit; //Only allow house attacks with target unit for now

  //Attack works like this: Commander tracks target unit in walk action. Members are ordered to walk to formation with commaner at target unit's location.
  //If target moves in WalkAction, commander will reissue PlaceOrder with aOnlySetMemebers = true, so memebers will walk to new location.

  if (fCommander <> nil) or (not aOnlySetMemebers) then
  begin
    fOrder := aWarriorOrder; //Only commander has order Attack, other units have walk to (this means they walk in formation and not in a straight line meeting the enemy one at a time
    fState := ws_None; //Clear other states
    SetOrderTarget(aTargetUnit);
  end;
  PlaceOrder(wo_Walk,KMPointDir(aTargetUnit.GetPosition,fOrderLoc.Dir),true); //Give memebers order to walk to approperiate positions
end;


{ Attack House works like this:
All units are assigned TTaskAttackHouse which does everything for us
(move to position, hit house, abandon, etc.) }
procedure TKMUnitWarrior.PlaceOrder(aWarriorOrder:TWarriorOrder; aTargetHouse:TKMHouse);
var i: integer;
begin
  Assert((aWarriorOrder=wo_AttackHouse)and(aTargetHouse<>nil));

  fOrder := aWarriorOrder;
  fState := ws_None; //Clear other states
  SetOrderHouseTarget(aTargetHouse);

  //Transmit order to all members if we have any
  if (fCommander = nil) and (fMembers <> nil) then
    for i:=0 to fMembers.Count-1 do
      TKMUnitWarrior(fMembers.Items[i]).PlaceOrder(aWarriorOrder, aTargetHouse);
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
  if fFoe <> nil then
    SaveStream.Write(fFoe.ID) //Store ID
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
        SomeoneHungry := SomeoneHungry or (TKMUnitWarrior(fMembers.List[i]).GetCondition < UNIT_MIN_CONDITION);
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


function TKMUnitWarrior.CheckForEnemy():boolean;
var i,k,WCount,OCount:shortint;
    U, BestU: TKMUnit;
    Warriors,Others: array[1..8] of TKMUnit;
begin
  Result := false; //Did we pick a fight?
  if not ENABLE_FIGHTING then exit;
  if not CanInterruptAction then exit;

  //This function should not be run too often, as it will take some time to execute (e.g. with 200 warriors it could take a while)
  WCount := 0;
  OCount := 0;
  BestU := nil;

  for i := -1 to 1 do
  for k := -1 to 1 do
  if (i<>0) or (k<>0) then
  if fTerrain.TileInMapCoords(GetPosition.X+i, GetPosition.Y+k) then
  if fTerrain.CanWalkDiagonaly(GetPosition,KMPoint(GetPosition.X+i,GetPosition.Y+k)) then //Don't fight through tree trunks
  begin
    U := fPlayers.UnitsHitTest(GetPosition.X+i,GetPosition.Y+k);
    //Must not dead/dying, not inside a house, not from our team and an enemy
    if (U <> nil) and (U.IsVisible) and (not U.IsDeadOrDying) and (fPlayers.CheckAlliance(GetOwner,U.GetOwner) = at_Enemy) then
    begin
      //We'd rather fight a warrior, so store them seperatly
      if U is TKMUnitWarrior then
      begin
        inc(WCount);
        Warriors[WCount] := U;
        //If they is a warrior right in front of us then choose him to fight rather than turning
        if KMSamePoint(KMGetPointInDir(GetPosition,Direction),U.GetPosition) then begin
          BestU := U;
          break;
        end;
      end
      else
      begin
        inc(OCount);
        Others[OCount] := U;
      end;
    end;
  end;

  //Preferance goes: Unit in front of us > Random warrior > Random citizen (if we are not busy)
  if BestU = nil then
    if WCount > 0 then
      BestU := Warriors[Random(WCount)+1]
    else
    if (OCount > 0) and not((GetUnitTask is TTaskAttackHouse) and TTaskAttackHouse(GetUnitTask).DestroyingHouse) then
      BestU := Others[Random(OCount)+1]
    else
      exit; //noone found

  SetActionFight(ua_Work, BestU);
  fOrderLoc := KMPointDir(GetPosition,fOrderLoc.Dir); //so that after the fight we stay where we are
  if BestU is TKMUnitWarrior then TKMUnitWarrior(BestU).CheckForEnemy; //Let opponent know he is attacked
  Result := true; //We found someone to fight
end;


{ See if we can abandon other actions in favor of more important things }
function TKMUnitWarrior.CanInterruptAction:boolean;
begin
  if GetUnitAction is TUnitActionWalkTo      then Result := TUnitActionWalkTo(GetUnitAction).CanAbandonExternal else //Only when unit is idling during Interaction pauses
  if GetUnitAction is TUnitActionStay        then Result := not TUnitActionStay(GetUnitAction).Locked else //Initial pause before leaving barracks is locked
  if GetUnitAction is TUnitActionAbandonWalk then Result := false else //Abandon walk should never be abandoned, it will exit within 1 step anyway
  if GetUnitAction is TUnitActionGoInOut     then Result := false else //Never interupt leaving barracks
  if GetUnitAction is TUnitActionFight       then Result := false //Never interupt a fight
  else Result := true;
end;


function TKMUnitWarrior.UpdateState():boolean;
  procedure UpdateFoe; //If noone is fighting - Halt
  var i:integer;
  begin
    if (Foe = nil) or (GetUnitAction is TUnitActionFight) then exit;
    if fMembers <> nil then for i:=0 to fMembers.Count-1 do
      if TKMUnit(fMembers.Items[i]).GetUnitAction is TUnitActionFight then exit;
    Foe := nil; //Nil foe because no one is fighting
    Halt; //Reposition because the fight has just finished
  end;

var
  i:integer;
  PositioningDone:boolean;
begin
  inc(fFlagAnim);
  if fCondition < UNIT_MIN_CONDITION then fThought := th_Eat; //th_Death checked in parent UpdateState
  if fFlagAnim mod 10 = 0 then UpdateHungerMessage();

  if fCommander=nil then
    UpdateFoe;

  if (fState = ws_Engage) and ((GetCommander.Foe = nil) or (not(GetUnitAction is TUnitActionWalkTo))) then
    fState := ws_None; //As soon as combat is over set the state back

  //Help out our fellow group members in combat if we are not fighting and someone else is
  if CanInterruptAction and (fState <> ws_Engage) and (GetCommander.Foe <> nil) then
  begin
    fOrder := wo_Attack;
    fState := ws_Engage; //Special state so we don't issue this order continuously
    SetOrderTarget(GetCommander.Foe);
  end;

  //Override current action if there's an Order in queue paying attention
  //to unit WalkTo current position (let the unit arrive on next tile first!)
  //As well let the unit finish it's curent Attack action before taking a new order
  //This should make units response a bit delayed.


  //New walking order
  if (fOrder=wo_Walk) then begin
    //Change WalkTo
    if (GetUnitAction is TUnitActionWalkTo) then begin
      if GetUnitTask <> nil then FreeAndNil(fUnitTask); //e.g. TaskAttackHouse
      TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(fOrderLoc.Loc, fCommander <> nil);
      fOrder := wo_None;
      fState := ws_Walking;
    end
    else
    //Set WalkTo
    if GetUnitAction.StepDone and CanInterruptAction then
    begin
      if GetUnitTask <> nil then FreeAndNil(fUnitTask);
      SetActionWalk(fOrderLoc.Loc, ua_Walk, true, false, fCommander <> nil);
      fOrder := wo_None;
      fState := ws_Walking;
    end;
  end;


  //Make sure attack order is still valid
  if (fOrder=wo_Attack) and (GetOrderTarget = nil) then fOrder := wo_None;
  if (fOrder=wo_AttackHouse) and (GetOrderHouseTarget = nil) then fOrder := wo_None;

  //Change walk in order to attack
  if (fOrder=wo_Attack) and (GetUnitAction is TUnitActionWalkTo) //If we are already walking then change the walk to the new location
  then begin
    //If we are not the commander then walk to near
    TUnitActionWalkTo(GetUnitAction).ChangeWalkTo(GetOrderTarget.NextPosition, fCommander <> nil, GetOrderTarget);
    fOrder := wo_None;
    if (fState <> ws_Engage) then fState := ws_Walking;
  end;

  //Take attack order
  if (fOrder=wo_Attack) and GetUnitAction.StepDone and CanInterruptAction then
  begin
    SetActionWalk(GetOrderTarget.NextPosition, KMPoint(0,0), ua_Walk, true, GetOrderTarget);
    fOrder := wo_None;
    if (fState <> ws_Engage) then fState := ws_Walking; //Difference between walking and attacking is not noticable, since when we reach the enemy we start fighting
  end;

  //Take attack house order
  if (fOrder=wo_AttackHouse) and GetUnitAction.StepDone and CanInterruptAction then
  begin
    SetUnitTask := TTaskAttackHouse.Create(Self,GetOrderHouseTarget);
    fOrder := wo_None;
    fState := ws_Walking; //Reposition after task exits
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
       (not KMSamePoint(GetPosition,fOrderLoc.Loc)) and fTerrain.Route_CanBeMade(GetPosition,fOrderLoc.Loc,GetDesiredPassability,true) then
    begin
      SetActionWalk(KMPoint(fOrderLoc)); //Walk to correct position
      fState := ws_Walking;
    end;

    //If we have no crew then just exit
    if fMembers <> nil then
      //Tell everyone to reposition
      for i:=0 to fMembers.Count-1 do
        //Must wait for unit(s) to get into position before we have truely finished walking
        PositioningDone := PositioningDone and TKMUnitWarrior(fMembers.Items[i]).RePosition;

  end;

  //Make sure we didn't get given an action above
  if GetUnitAction <> nil then exit;


  if fState = ws_Walking then
  begin
    fState := ws_RepositionPause; //Means we are in position and waiting until we turn
    SetActionStay(4+Random(2),ua_Walk); //Pause 0.5 secs before facing right direction. Slight random amount so they don't look so much like robots ;) (actually they still do, we need to add more randoms)
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
  UnitType, AnimAct, AnimDir, TeamColor:byte;
  XPaintPos, YPaintPos: single;
  i:integer;
  UnitPosition: TKMPoint;
begin
Inherited;
  if not fVisible then exit;
  UnitType := byte(fUnitType);
  AnimAct  := byte(fCurrentAction.GetActionType); //should correspond with UnitAction
  AnimDir  := byte(Direction);

  XPaintPos := fPosition.X + 0.5 + GetSlide(ax_X);
  YPaintPos := fPosition.Y + 1   + GetSlide(ax_Y);

  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), XPaintPos, YPaintPos, true);

  if (fCommander=nil) and not IsDeadOrDying then begin
    //todo: Fix flag offsets
    //XPaintPos := XPaintPos + FlagXOffset[UnitType]/CELL_SIZE_PX;
    YPaintPos := YPaintPos + FlagYOffset[UnitType]/CELL_SIZE_PX; //@Lewin: Feel free to tweak FlagHeight, needs also Xoffset depending on direction (E/W)
    TeamColor := byte(fOwner);
    if (fPlayers.Selected is TKMUnitWarrior) and (TKMUnitWarrior(fPlayers.Selected).GetCommander = Self) then TeamColor := byte(play_animals); //Highlight with White color
    fRender.RenderUnitFlag(UnitType,   9, AnimDir, fFlagAnim, TeamColor, XPaintPos, YPaintPos, false);
  end;

  if fThought<>th_None then
    fRender.RenderUnitThought(fThought, XPaintPos, YPaintPos);

  //Paint members in MapEd mode
  if fMapEdMembersCount<>0 then
  for i:=1 to fMapEdMembersCount do begin
    UnitPosition := GetPositionInGroup2(GetPosition.X, GetPosition.Y, Direction, i+1, fUnitsPerRow, fTerrain.MapX, fTerrain.MapY);
    XPaintPos := UnitPosition.X + 0.5; //MapEd units don't have sliding anyway
    YPaintPos := UnitPosition.Y + 1  ;
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, byte(fOwner), XPaintPos, YPaintPos, true);
  end;

end;


end.
