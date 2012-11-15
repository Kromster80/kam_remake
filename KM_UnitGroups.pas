unit KM_UnitGroups;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils, Types,
     KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_Points, KM_Houses, KM_Units,
     KM_Units_Warrior;

type
  TKMTurnDirection = (tdNone, tdCW, tdCCW);

  TKMGroupOrder = (
    goNone,         //Last order was executed and now we have nothing to do
    goWalkTo,       //Ordered to walk somewhere or just change formation
    goAttackHouse,  //Attack house
    goAttackUnit,   //Attack specific unit
    goStorm         //Run forward
    );

  TKMUnitGroup = class
  private
    fID: Cardinal;
    fPointerCount: Cardinal;
    fTicker: Cardinal;
    fMembers: TList;
    fOffenders: TList;
    fSelected: TKMUnitWarrior; //Unit selected by player in GUI
    fUnitsPerRow: Word;
    fTimeSinceHungryReminder: Integer;
    fGroupType: TGroupType;

    fOrder: TKMGroupOrder; //Remember last order incase we need to repeat it (e.g. to joined members)
    fOrderLoc: TKMPointDir; //Dir is the direction to face after order
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetGroup: TKMUnitGroup; //Unit we are ordered to attack. This property should never be accessed, use public OrderTarget instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use public OrderHouseTarget instead.

    fMapEdCount: Word;

    function GetCount: Integer;
    function GetMember(aIndex: Integer): TKMUnitWarrior;
    function GetNearestMember(aUnit: TKMUnitWarrior): Integer; overload;
    function GetNearestMember(aLoc: TKMPoint): TKMUnitWarrior; overload;
    function GetRandomMember: TKMUnitWarrior;
    function GetMemberLoc(aIndex: Integer): TKMPointExact;
    procedure SetUnitsPerRow(aCount: Word);
    function GetOwner: TPlayerIndex;
    procedure SetDirection(Value: TKMDirection);
    procedure SetCondition(aValue: Integer);
    procedure SetPosition(aValue: TKMPoint);
    procedure ClearOrderTarget;

    function GetOrderTargetUnit: TKMUnit;
    function GetOrderTargetGroup: TKMUnitGroup;
    function GetOrderTargetHouse: TKMHouse;
    procedure SetOrderTargetUnit(aUnit: TKMUnit);
    procedure SetOrderTargetHouse(aHouse: TKMHouse);

    procedure CheckForFight;
    procedure CheckOrderDone;
    procedure UpdateHungerMessage;

    procedure Member_Killed(aMember: TKMUnitWarrior);
    procedure Member_PickedFight(aMember: TKMUnitWarrior; aEnemy: TKMUnit);

    function GetCondition: Integer;
    function GetDirection: TKMDirection;
    function GetPosition: TKMPoint;
  public

    constructor Create(aID: Cardinal; aCreator: TKMUnitWarrior); overload;
    constructor Create(aID: Cardinal; aOwner: TPlayerIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word); overload;
    constructor Create(LoadStream: TKMemoryStream); overload;
    procedure SyncLoad;
    procedure Save(SaveStream: TKMemoryStream);
    destructor Destroy; override;

    function GetGroupPointer: TKMUnitGroup;
    procedure ReleaseGroupPointer;
    procedure AddMember(aWarrior: TKMUnitWarrior; aIndex: Integer = -1);
    function HitTest(X,Y: Integer): Boolean;
    procedure SelectHitTest(X,Y: Integer);
    function HasMember(aWarrior: TKMUnit): Boolean;
    function InFight: Boolean; //Fighting and can't take any orders from player
    function IsAttackingHouse: Boolean; //Attacking house
    function CanTakeOrders: Boolean;
    function CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean;
    function IsRanged: Boolean;
    function IsDead: Boolean;
    function UnitType: TUnitType;
    function GetOrderText: string;
    property GroupType: TGroupType read fGroupType;
    property ID: Cardinal read fID;
    property Count: Integer read GetCount;
    property MapEdCount: Word read fMapEdCount write fMapEdCount;
    property Members[aIndex: Integer]: TKMUnitWarrior read GetMember;
    property Owner: TPlayerIndex read GetOwner;
    property Position: TKMPoint read GetPosition write SetPosition;
    property Direction: TKMDirection read GetDirection write SetDirection;
    property UnitsPerRow: Word read fUnitsPerRow write SetUnitsPerRow;
    property SelectedUnit: TKMUnitWarrior read fSelected;
    property Condition: Integer read GetCondition write SetCondition;
    property Order: TKMGroupOrder read fOrder;

    property OrderTargetUnit: TKMUnit read GetOrderTargetUnit write SetOrderTargetUnit;
    property OrderTargetGroup: TKMUnitGroup read GetOrderTargetGroup;
    property OrderTargetHouse: TKMHouse read GetOrderTargetHouse write SetOrderTargetHouse;

    procedure OrderAttackHouse(aHouse: TKMHouse);
    procedure OrderAttackUnit(aUnit: TKMUnit);
    procedure OrderFood;
    procedure OrderFormation(aTurnAmount: TKMTurnDirection; aColumnsChange: ShortInt);
    procedure OrderHalt;
    procedure OrderLinkTo(aTargetGroup: TKMUnitGroup);
    procedure OrderNone;
    procedure OrderRepeat;
    procedure OrderSplit;
    procedure OrderSplitLinkTo(aGroup: TKMUnitGroup; aCount: Word);
    procedure OrderStorm;
    procedure OrderWalk(aLoc: TKMPoint; aDir: TKMDirection = dir_NA);

    procedure UpdateState;
    procedure Paint;
  end;


  TKMUnitGroups = class
  private
    fGroups: TList;

    function GetCount: Integer;
    function GetGroup(aIndex: Integer): TKMUnitGroup;
  public
    constructor Create;
    destructor Destroy; override;

    function AddGroup(aWarrior: TKMUnitWarrior): TKMUnitGroup; overload;
    function AddGroup(aOwner: TPlayerIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word): TKMUnitGroup; overload;
    procedure RemGroup(aGroup: TKMUnitGroup);

    property Count: Integer read GetCount;
    property Groups[aIndex: Integer]: TKMUnitGroup read GetGroup; default;
    function GetGroupByID(aID: Integer): TKMUnitGroup;
    function GetGroupByMember(aUnit: TKMUnitWarrior): TKMUnitGroup;
    function HitTest(X,Y: Integer): TKMUnitGroup;

    procedure WarriorTrained(aUnit: TKMUnitWarrior);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Game, KM_Player, KM_PlayersCollection, KM_Resource, KM_ResourceUnit, KM_Terrain, KM_Utils, KM_TextLibrary, KM_RenderPool;


const
  HUNGER_CHECK_FREQ = 10; //Check warrior hunger every 1 second
  UG_PASS_FOCUS_TO_LIVE = True; //When selected unit is killed - change selection to nearest unit


{ TKMUnitGroup }
constructor TKMUnitGroup.Create(aID: Cardinal; aCreator: TKMUnitWarrior);
begin
  inherited Create;

  fID := aID;
  fGroupType := UnitGroups[aCreator.UnitType];
  fMembers := TList.Create;
  fOffenders := TList.Create;

  //So when they click Halt for the first time it knows where to place them
  fOrderLoc := KMPointDir(aCreator.GetPosition.X, aCreator.GetPosition.Y, aCreator.Direction);

  AddMember(aCreator);
  UnitsPerRow := 1;
end;


constructor TKMUnitGroup.Create(aID: Cardinal; aOwner: TPlayerIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word);
var
  Warrior: TKMUnitWarrior;
  I: Integer;
  DoesFit: Boolean;
  UnitLoc: TKMPoint;
  Condition: Word;
begin
  inherited Create;

  fID := aID;
  fGroupType := UnitGroups[aUnitType];
  fMembers := TList.Create;
  fOffenders := TList.Create;

  //So when they click Halt for the first time it knows where to place them
  fOrderLoc := KMPointDir(PosX, PosY, aDir);

  //Whole group should have same condition
  Condition := Round(UNIT_MAX_CONDITION * (UNIT_CONDITION_BASE + KaMRandomS(UNIT_CONDITION_RANDOM)));

  if fGame.IsMapEditor then
  begin
    Warrior := TKMUnitWarrior(fPlayers[aOwner].Units.Add(aOwner, aUnitType, PosX, PosY, True, fTerrain.GetWalkConnectID(KMPoint(PosX,PosY))));
    if Warrior <> nil then
    begin
      Warrior.Direction := aDir;
      Warrior.AnimStep  := UnitStillFrames[aDir];
      AddMember(Warrior);
      Warrior.Condition := UNIT_MAX_CONDITION div 2;
      fMapEdCount := aUnitCount;
    end;
  end
  else
  for I := 0 to aUnitCount - 1 do
  begin
    UnitLoc := GetPositionInGroup2(PosX, PosY, aDir, I, aUnitPerRow, fTerrain.MapX, fTerrain.MapY, DoesFit);
    if not DoesFit then Continue;

    Warrior := TKMUnitWarrior(fPlayers[aOwner].Units.Add(aOwner, aUnitType, UnitLoc.X, UnitLoc.Y, True, fTerrain.GetWalkConnectID(KMPoint(PosX,PosY))));
    if Warrior = nil then Continue;

    fPlayers[aOwner].Stats.UnitCreated(aUnitType, False);
    Warrior.Direction := aDir;
    Warrior.AnimStep  := UnitStillFrames[aDir];
    AddMember(Warrior);
    Warrior.Condition := Condition;
  end;

  //We could not set it earlier cos it's limited by Count
  UnitsPerRow := aUnitPerRow;
end;


constructor TKMUnitGroup.Create(LoadStream: TKMemoryStream);
var
  I, aCount: Integer;
  W: TKMUnitWarrior;
begin
  inherited Create;
  fMembers := TList.Create;
  fOffenders := TList.Create;

  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fID);
  LoadStream.Read(aCount);
  for I := 0 to aCount - 1 do
  begin
    LoadStream.Read(W, 4); //subst on syncload
    fMembers.Add(W);
  end;

  LoadStream.Read(aCount);
  for I := 0 to aCount - 1 do
  begin
    LoadStream.Read(W, 4); //subst on syncload
    fOffenders.Add(W);
  end;

  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fOrderLoc);
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fSelected, 4); //subst on syncload
  LoadStream.Read(fTicker);
  LoadStream.Read(fTimeSinceHungryReminder);
  LoadStream.Read(fUnitsPerRow);
end;


procedure TKMUnitGroup.SyncLoad;
var I: Integer;
begin
  inherited;

  for I := 0 to Count - 1 do
  begin
    fMembers[I] := TKMUnitWarrior(fPlayers.GetUnitByID(Cardinal(fMembers[I])));
    Members[I].OnKilled := Member_Killed;
    Members[I].OnPickedFight := Member_PickedFight;
  end;

  for I := 0 to fOffenders.Count - 1 do
    fOffenders[I] := TKMUnitWarrior(fPlayers.GetUnitByID(Cardinal(TKMUnitWarrior(fOffenders[I]))));

  fOrderTargetUnit := fPlayers.GetUnitByID(cardinal(fOrderTargetUnit));
  fOrderTargetHouse := fPlayers.GetHouseByID(cardinal(fOrderTargetHouse));
  fSelected := TKMUnitWarrior(fPlayers.GetUnitByID(Cardinal(fSelected)));
end;


destructor TKMUnitGroup.Destroy;
begin
  fMembers.Free;
  fOffenders.Free;

  inherited;
end;


procedure TKMUnitGroup.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  inherited;
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  SaveStream.Write(fID);
  SaveStream.Write(fMembers.Count);
  for I := 0 to fMembers.Count - 1 do
    SaveStream.Write(Members[I].ID);
  SaveStream.Write(fOffenders.Count);
  for I := 0 to fOffenders.Count - 1 do
    SaveStream.Write(TKMUnitWarrior(fOffenders[I]).ID);
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fOrderLoc);
  if fOrderTargetHouse <> nil then
    SaveStream.Write(fOrderTargetHouse.ID)
  else
    SaveStream.Write(Integer(0));
  if fOrderTargetUnit <> nil then
    SaveStream.Write(fOrderTargetUnit.ID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fPointerCount);
  if fSelected <> nil then
    SaveStream.Write(fSelected.ID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fTicker);
  SaveStream.Write(fTimeSinceHungryReminder);
  SaveStream.Write(fUnitsPerRow);
end;


function TKMUnitGroup.GetCondition: Integer;
var
  I: Integer;
begin
  Result := UNIT_MAX_CONDITION;
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
                                    fTerrain.MapX, fTerrain.MapY,
                                    Result.Exact);
  Result.Exact := Result.Exact and fTerrain.CheckPassability(Result.Loc, CanWalk);
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


function TKMUnitGroup.GetRandomMember: TKMUnitWarrior;
var
  I,K: Integer;
  Live: array of Word;
begin
  Result := nil;

  K := 0;
  SetLength(Live, Count);
  for I := 0 to Count - 1 do
  if not Members[I].IsDeadOrDying then
  begin
    Live[K] := I;
    Inc(K);
  end;

  if K <> 0 then
    Result := Members[Live[KaMRandom(K)]];
end;


{Returns self and adds on to the pointer counter}
function TKMUnitGroup.GetGroupPointer: TKMUnitGroup;
begin
  Inc(fPointerCount);
  Result := Self;
end;


{Decreases the pointer counter}
//Should be used only by fPlayers for clarity sake
procedure TKMUnitGroup.ReleaseGroupPointer;
begin
  if fPointerCount < 1 then
    raise ELocError.Create('Group remove pointer', Position);
  Dec(fPointerCount);
end;


function TKMUnitGroup.GetOwner: TPlayerIndex;
begin
  Result := Members[0].Owner;
end;


//Get current groups location (we use flagholder)
function TKMUnitGroup.GetPosition: TKMPoint;
begin
  Result := Members[0].GetPosition;
end;


procedure TKMUnitGroup.SetPosition(aValue: TKMPoint);
begin
  Assert(fGame.IsMapEditor);
  fOrderLoc.Loc := aValue;
  Members[0].SetPosition(aValue);
end;


procedure TKMUnitGroup.SetCondition(aValue: Integer);
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Members[I].Condition := aValue;
end;


procedure TKMUnitGroup.SetDirection(Value: TKMDirection);
begin
  Assert(fGame.IsMapEditor);
  fOrderLoc.Dir := Value;
  Members[0].Direction := Value;
end;


procedure TKMUnitGroup.SetUnitsPerRow(aCount: Word);
begin
  if fGame.IsMapEditor then
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
  aWarrior.OnPickedFight := Member_PickedFight;
  aWarrior.OnKilled := Member_Killed;
end;


function TKMUnitGroup.HasMember(aWarrior: TKMUnit): Boolean;
begin
  Result := fMembers.IndexOf(aWarrior) <> -1;
end;


//If the player is allowed to issue orders to group
function TKMUnitGroup.CanTakeOrders: Boolean;
begin
  Result := IsRanged or not InFight; //Ranged units can always take orders
end;


function TKMUnitGroup.CanWalkTo(aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := Members[0].CanWalkTo(aTo, aDistance);
end;


//Group is dead, but still exists cos of pointers to it
function TKMUnitGroup.IsDead: Boolean;
begin
  Result := fMembers.Count = 0;
end;


function TKMUnitGroup.IsRanged: Boolean;
begin
  Result := fResource.UnitDat[Members[0].UnitType].FightType = ft_Ranged;
end;


//Member reports that he has died (or been killed)
procedure TKMUnitGroup.Member_Killed(aMember: TKMUnitWarrior);
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
    if UG_PASS_FOCUS_TO_LIVE then
    begin
      NewSel := GetNearestMember(aMember);
      if NewSel <> -1 then
        fSelected := Members[NewSel];
    end;
  end;

  Members[I].ReleaseUnitPointer;
  fMembers.Delete(I);

  //Move nearest member to placeholders place
  if I = 0 then
  begin
    NewSel := GetNearestMember(aMember);
    if NewSel <> -1 then
      fMembers.Exchange(NewSel, 0);
  end;

  SetUnitsPerRow(fUnitsPerRow);

  if not IsDead and CanTakeOrders then
    OrderRepeat;
end;


//Member got in a fight
//Remember who we are fighting with, to guide idle units to
//This only works for melee offenders
procedure TKMUnitGroup.Member_PickedFight(aMember: TKMUnitWarrior; aEnemy: TKMUnit);
begin
  if (aEnemy is TKMUnitWarrior) then
    fOffenders.Add(aEnemy.GetUnitPointer);
end;


//If we picked up a fight, while doing any other order - manage it
procedure TKMUnitGroup.CheckForFight;
var
  I,K: Integer;
begin
  //Verify we still have foes
  for I := fOffenders.Count - 1 downto 0 do
  if TKMUnitWarrior(fOffenders[I]).IsDeadOrDying then
  begin
    TKMUnitWarrior(fOffenders[I]).ReleaseUnitPointer;
    fOffenders.Delete(I);
    if fOffenders.Count = 0 then
      OrderRepeat;
  end;

  //Fight is over
  if fOffenders.Count = 0 then Exit;

  if IsRanged then
    for I := 0 to Count - 1 do
    begin
      //Try shooting the offenders
      if not Members[I].InFight then
        for K := 0 to fOffenders.Count - 1 do
        if Members[I].WithinFightRange(TKMUnitWarrior(fOffenders[K]).GetPosition) then
          Members[I].OrderFight(TKMUnitWarrior(fOffenders[K]))
        else
          //todo: Try to walk near and attack?
    end
  else
  begin
    //Let idle help fellow members
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

                        //Guide Idle units to their places
                        if Members[I].IsIdle then
                          if Members[I].OrderDone then
                          begin
                            if fOrderLoc.Dir <> dir_NA then
                              Members[I].Direction := fOrderLoc.Dir;
                          end
                          else
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
                        //Then they will attacck anything within their reach by themselves
                        OrderExecuted := (OrderTargetUnit = nil);

                        if not OrderExecuted then
                          OrderAttackUnit(fOrderTargetUnit);

                        {if OrderExecuted then
                        //Look for more enemies ASAP (instead of waiting 5 ticks in Unit.UpdateState)
                        for I := 0 to Count - 1 do
                          Members[I].CheckForEnemy;}
                      end
                      else
                      begin
                        //Melee units must kill target unit and its Group
                        OrderExecuted := (OrderTargetUnit = nil) and (OrderTargetGroup = nil);

                        //See if target is escaping
                        if (OrderTargetUnit <> nil)
                        and not KMSamePoint(OrderTargetUnit.NextPosition, fOrderLoc.Loc) then
                          OrderAttackUnit(fOrderTargetUnit);

                        //If Enemy was killed, but target Group still exists
                        if (OrderTargetUnit = nil) and (OrderTargetGroup <> nil) then
                        begin
                          //Old enemy has died, change target to his comrades
                          U := fOrderTargetGroup.GetNearestMember(Members[0].GetPosition);
                          Assert(U <> nil, 'We checked that Group is not dead, hence we should have a valid Unit');
                          OrderAttackUnit(U);
                        end;
                      end;
                    end;
    goStorm:        OrderExecuted := False;
  end;

  if OrderExecuted then
  begin
    for I := 0 to Count - 1 do
    if fOrderLoc.Dir <> dir_NA then
      Members[I].Direction := fOrderLoc.Dir;
    OrderNone;
  end;
end;


//Fighting with citizens does not count
function TKMUnitGroup.InFight: Boolean;
var I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
  if Members[I].InFight then
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


function TKMUnitGroup.HitTest(X,Y: Integer): Boolean;
var I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
  if Members[I].HitTest(X, Y) and not Members[I].IsDead then
  begin
    Result := True;
    Break;
  end;
end;


procedure TKMUnitGroup.SelectHitTest(X,Y: Integer);
var
  U: TKMUnit;
begin
  U := fTerrain.UnitsHitTest(X,Y);
  Assert((U <> nil) and (U is TKMUnitWarrior) and HasMember(TKMUnitWarrior(U)),
    'Should match with HitTest that selected this group in TKMPlayersCollection.SelectHitTest');

  fSelected := TKMUnitWarrior(U);
end;


//All units are assigned TTaskAttackHouse which does everything for us (move to position, hit house, abandon, etc.)
procedure TKMUnitGroup.OrderAttackHouse(aHouse: TKMHouse);
var I: Integer;
begin
  fOrder := goAttackHouse;
  fOrderLoc := KMPointDir(0, 0, dir_NA);
  OrderTargetHouse := aHouse;

  for I := 0 to Count - 1 do
    Members[I].OrderAttackHouse(aHouse);
end;


procedure TKMUnitGroup.OrderAttackUnit(aUnit: TKMUnit);
var I: Integer;
begin
  Assert(aUnit <> nil);

  if IsRanged then
  begin
    //Update Order
    fOrder := goAttackUnit;
    fOrderLoc := KMPointDir(aUnit.NextPosition, dir_NA); //Remember where unit stand
    OrderTargetUnit := aUnit;

    for I := 0 to Count - 1 do
    if not Members[I].InFight then
    begin
      //Check target in range, and if not - chase it / back up from it
      if (KMLength(Members[I].GetPosition, OrderTargetUnit.GetPosition) > Members[I].GetFightMaxRange) then
        //Too far away
        Members[I].OrderWalk(fOrderTargetUnit.NextPosition)
      else
      if (KMLength(Members[I].GetPosition, OrderTargetUnit.GetPosition) < Members[I].GetFightMinRange) then
        //todo: Archer is too close, back up
      else
        //WithinRange
        Members[I].OrderFight(OrderTargetUnit);
    end;
  end
  else
  begin
    //Walk in formation towards enemy,
    //Members will take care of attack when we approach
    OrderWalk(aUnit.NextPosition);

    //Revert Order to proper one (we disguise Walk)
    fOrder := goAttackUnit;
    fOrderLoc := KMPointDir(aUnit.NextPosition, dir_NA); //Remember where unit stand
    OrderTargetUnit := aUnit;
  end;
end;


//Order some food for troops
procedure TKMUnitGroup.OrderFood;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Members[I].OrderFood;

  OrderHalt;
end;


procedure TKMUnitGroup.OrderFormation(aTurnAmount: TKMTurnDirection; aColumnsChange: ShortInt);
begin
  if IsDead then Exit;

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
procedure TKMUnitGroup.OrderHalt;
begin
  //Halt is not a true order, it is just OrderWalk
  //hose target depends on previous activity
  case fOrder of
    goNone:         OrderWalk(fOrderLoc.Loc);
    goWalkTo:       OrderWalk(Members[0].NextPosition);
    goAttackHouse:  OrderWalk(Members[0].NextPosition);
    goAttackUnit:   OrderWalk(Members[0].NextPosition);
    goStorm:        OrderWalk(Members[0].NextPosition);
  end;
end;


procedure TKMUnitGroup.OrderLinkTo(aTargetGroup: TKMUnitGroup);
begin
  //Any could have died since the time order was issued due to Net delay
  if IsDead or aTargetGroup.IsDead then Exit;

  //Only link to same group type
  if aTargetGroup.GroupType <> GroupType then Exit;

  //Can't link to self for obvious reasons
  if aTargetGroup = Self then Exit;

  //Move our members and self to the new group
  while (fMembers.Count <> 0) do
  begin
    aTargetGroup.AddMember(Members[0]);
    Members[0].ReleaseUnitPointer;
    fMembers.Delete(0);
  end;

  //In MP commands execution may be delayed, check if we still selected
  if fPlayers.Selected = Self then
  begin
    fPlayers.Selected := aTargetGroup;
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
    goNone:         OrderHalt;
    goWalkTo:       OrderWalk(fOrderLoc.Loc);
    goAttackHouse:  if OrderTargetHouse <> nil then OrderAttackHouse(fOrderTargetHouse);
    goAttackUnit:   if OrderTargetUnit <> nil then OrderAttackUnit(fOrderTargetUnit);
    goStorm:        ;
  end;
end;


//Split group in half
//or split different unit types apart
procedure TKMUnitGroup.OrderSplit;
var
  I: Integer;
  NewGroup: TKMUnitGroup;
  NewLeader: TKMUnitWarrior;
  MultipleTypes: Boolean;
begin
  if IsDead then Exit;
  if Count < 2 then Exit;

  //Choose the new leader
  NewLeader := Members[(Count div 2) + (Min(fUnitsPerRow, Count div 2) div 2)];

  //If there are different unit types in the group, split should just split them first
  MultipleTypes := False;
  for I := 1 to Count - 1 do
    if Members[I].UnitType <> Members[0].UnitType then
    begin
      MultipleTypes := True;
      //New commander is first unit of different type, for simplicity
      NewLeader := Members[I];
      Break;
    end;

  //Remove from the group
  NewLeader.ReleaseUnitPointer;
  fMembers.Remove(NewLeader);

  NewGroup := fPlayers[Owner].UnitGroups.AddGroup(NewLeader);

  //Split by UnitTypes or by Count (make NewGroup half or smaller half)
  for I := Count - 1 downto 0 do
  if (MultipleTypes and (Members[I].UnitType = NewLeader.UnitType))
  or (not MultipleTypes and (Count > NewGroup.Count + 1)) then
  begin
    Members[I].ReleaseUnitPointer;
    NewGroup.AddMember(Members[I], 1); // Join new group (insert next to commander)
    fMembers.Delete(I); // Leave this group
  end;

  //Make sure units per row is still valid for both groups
  UnitsPerRow := fUnitsPerRow;
  NewGroup.UnitsPerRow := fUnitsPerRow;

  //If we are hungry then don't repeat message each time we split, give new commander our counter
  NewGroup.fTimeSinceHungryReminder := fTimeSinceHungryReminder;

  //Commander OrderLoc must always be valid, but because this guy wasn't a commander it might not be
  NewGroup.fOrderLoc := KMPointDir(NewLeader.GetPosition, fOrderLoc.Dir);

  //Tell both groups to reposition
  OrderHalt;
  NewGroup.OrderHalt;
end;


//Splits X number of men from the group and adds them to the new commander
procedure TKMUnitGroup.OrderSplitLinkTo(aGroup: TKMUnitGroup; aCount: Word);
var
  I: Integer;
begin
  //Make sure to leave someone in the group
  Assert(aCount < Count);

  //Take units from the end, to keep flagholder
  for I := fMembers.Count - 1 downto fMembers.Count - aCount do
  begin
    Members[I].ReleaseUnitPointer;
    aGroup.AddMember(Members[I]);
    fMembers.Delete(I);
  end;

  //Make sure units per row is still valid
  SetUnitsPerRow(UnitsPerRow);

  //Tell both groups to reposition
  OrderHalt;
  aGroup.OrderHalt;
end;


procedure TKMUnitGroup.OrderStorm;
var I: Integer;
begin
  //Don't allow ordering a second storm attack while there is still one active (possible due to network lag)
  if not CanTakeOrders then Exit;

  fOrder := goStorm;
  fOrderLoc := KMPointDir(0, 0, dir_NA);
  ClearOrderTarget;

  //Each next row delayed by few ticks to avoid crowding
  for I := 0 to Count - 1 do
    Members[I].OrderStorm(I div fUnitsPerRow);
end;


procedure TKMUnitGroup.OrderWalk(aLoc: TKMPoint; aDir: TKMDirection = dir_NA);
var
  I: Integer;
  NewDir: TKMDirection;
  P: TKMPointExact;
begin
  if IsDead then Exit;

  if aDir = dir_NA then
    if fOrderLoc.Dir = dir_NA then
      NewDir := Members[0].Direction
    else
      NewDir := fOrderLoc.Dir
  else
    NewDir := aDir;

  fOrder := goWalkTo;
  fOrderLoc := KMPointDir(aLoc, NewDir);
  ClearOrderTarget;

  for I := 0 to Count - 1 do
  begin
    P := GetMemberLoc(I);
    Members[I].OrderWalk(P.Loc, P.Exact);
  end;
end;


function TKMUnitGroup.UnitType: TUnitType;
begin
  //todo: Used when playing confirmation sounds.
  //Maybe we need to pick flag carrier, or strongest unit, or fSelected, or random?
  Result := Members[0].UnitType;
end;


function TKMUnitGroup.GetOrderText: string;
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
      if (Owner = MyPlayer.PlayerIndex) and not fGame.IsReplay then
        fGame.ShowMessage(mkUnit, fTextLibrary[TX_MSG_TROOP_HUNGRY], Position);
      fTimeSinceHungryReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
    end;
  end
  else
    fTimeSinceHungryReminder := 0;
end;


procedure TKMUnitGroup.ClearOrderTarget;
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  fPlayers.CleanUpUnitPointer(fOrderTargetUnit);
  fPlayers.CleanUpGroupPointer(fOrderTargetGroup);
  fPlayers.CleanUpHousePointer(fOrderTargetHouse);
end;


function TKMUnitGroup.GetOrderTargetUnit: TKMUnit;
begin
  //If the target unit has died then clear it
  if (fOrderTargetUnit <> nil) and fOrderTargetUnit.IsDeadOrDying then
    fPlayers.CleanUpUnitPointer(fOrderTargetUnit);

  Result := fOrderTargetUnit;
end;


function TKMUnitGroup.GetOrderTargetGroup: TKMUnitGroup;
begin
  //If the target group has died then clear it
  if (fOrderTargetGroup <> nil) and fOrderTargetGroup.IsDead then
    fPlayers.CleanUpGroupPointer(fOrderTargetGroup);

  Result := fOrderTargetGroup;
end;


function TKMUnitGroup.GetOrderTargetHouse: TKMHouse;
begin
  //If the target house has been destroyed then clear it
  if (fOrderTargetHouse <> nil) and fOrderTargetHouse.IsDestroyed then ClearOrderTarget;
  Result := fOrderTargetHouse;
end;


procedure TKMUnitGroup.SetOrderTargetUnit(aUnit: TKMUnit);
begin
  //Remove previous value
  ClearOrderTarget;
  if aUnit <> nil then
  begin
    fOrderTargetUnit := aUnit.GetUnitPointer; //Else it will be nil from ClearOrderTarget
    if (aUnit is TKMUnitWarrior) and not IsRanged then
      fOrderTargetGroup := fPlayers[aUnit.Owner].UnitGroups.GetGroupByMember(TKMUnitWarrior(aUnit)).GetGroupPointer;
  end;
end;


procedure TKMUnitGroup.SetOrderTargetHouse(aHouse: TKMHouse);
begin
  //Remove previous value
  ClearOrderTarget;
  if aHouse <> nil then
    fOrderTargetHouse := aHouse.GetHousePointer; //Else it will be nil from ClearOrderTarget
end;


procedure TKMUnitGroup.UpdateState;
begin
  Inc(fTicker);
  if IsDead then Exit;

  if fTicker mod HUNGER_CHECK_FREQ = 0 then
    UpdateHungerMessage;

  if fTicker mod 5 = 0 then
    CheckForFight;

  if not InFight and (fTicker mod 11 = 0) then
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
  FlagColor := IfThen(fPlayers.Selected = Self, $FFFFFFFF, fPlayers[FlagCarrier.Owner].FlagColor);

  //In MapEd units fTicker always the same, use Terrain instead
  FlagStep := IfThen(fGame.GameMode = gmMapEd, fTerrain.AnimStep, fTicker);

  //Flag needs to be rendered above or below unit depending on direction (see AddUnitFlag)

  if FlagCarrier.Direction in [dir_SE, dir_S, dir_SW, dir_W] then
    UnitPos.Y := UnitPos.Y - 0.01
  else
    UnitPos.Y := UnitPos.Y + 0.01;

  fRenderPool.AddUnitFlag(FlagCarrier.UnitType, FlagCarrier.GetUnitAction.ActionType,
    FlagCarrier.Direction, FlagCarrier.AnimStep, FlagStep, UnitPos.X, UnitPos.Y,
    FlagColor);

  //Paint virtual members in MapEd mode
  for I := 1 to fMapEdCount - 1 do
  begin
    NewPos := GetPositionInGroup2(fOrderLoc.Loc.X, fOrderLoc.Loc.Y, fOrderLoc.Dir, I, fUnitsPerRow, fTerrain.MapX, fTerrain.MapY, DoesFit);
    if not DoesFit then Continue; //Don't render units that are off the map in the map editor
    UnitPos.X := NewPos.X + UNIT_OFF_X; //MapEd units don't have sliding
    UnitPos.Y := NewPos.Y + UNIT_OFF_Y;
    fRenderPool.AddUnit(FlagCarrier.UnitType, ua_Walk, fOrderLoc.Dir, 0, UnitPos.X, UnitPos.Y, fPlayers[FlagCarrier.Owner].FlagColor, True);
  end;
end;


{ TKMUnitGroups }
constructor TKMUnitGroups.Create;
begin
  inherited Create;

  fGroups := TList.Create;
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
  Result := TKMUnitGroup.Create(fGame.GetNewID, aWarrior);
  fGroups.Add(Result)
end;


function TKMUnitGroups.AddGroup(aOwner: TPlayerIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word): TKMUnitGroup;
begin
  Result := nil;

  Assert(aDir <> dir_NA);
  Assert(aUnitType in [WARRIOR_MIN..WARRIOR_MAX]);

  Result := TKMUnitGroup.Create(fGame.GetNewID, aOwner, aUnitType, PosX, PosY, aDir, aUnitPerRow, aUnitCount);

  //If group failed to create (e.g. due to being placed on unwalkable position)
  //then its memberCount = 0
  if not Result.IsDead then
    fGroups.Add(Result)
  else
    FreeAndNil(Result);
end;


function TKMUnitGroups.GetGroupByID(aID: Integer): TKMUnitGroup;
var i:integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if aID = Groups[i].ID then
    begin
      Result := Groups[i];
      exit;
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
      Result := Groups[I];
      Break;
    end;
end;


procedure TKMUnitGroups.WarriorTrained(aUnit: TKMUnitWarrior);
var
  LinkUnit: TKMUnitWarrior;
  LinkGroup: TKMUnitGroup;
begin
  case fPlayers[aUnit.Owner].PlayerType of
    pt_Human:    begin
                   LinkUnit := aUnit.FindLinkUnit(aUnit.GetPosition);
                   if LinkUnit <> nil then
                   begin
                     LinkGroup := fPlayers[aUnit.Owner].UnitGroups.GetGroupByMember(LinkUnit);
                     LinkGroup.AddMember(aUnit);
                   end
                   else
                   begin
                     fGroups.Add(TKMUnitGroup.Create(fGame.GetNewID, aUnit));
                   end;
                 end;
    pt_Computer: fGroups.Add(TKMUnitGroup.Create(fGame.GetNewID, aUnit));
  end;
end;


function TKMUnitGroups.HitTest(X,Y: Integer): TKMUnitGroup;
var
  I: Integer;
  U: TKMUnit;
begin
  Result := nil;
  U := fTerrain.UnitsHitTest(X,Y);
  if (U <> nil) and (U is TKMUnitWarrior) then
  for I := 0 to Count - 1 do
    if Groups[I].HitTest(X,Y) then
    begin
      Result := Groups[I];
      Break;
    end;
end;


procedure TKMUnitGroups.RemGroup(aGroup: TKMUnitGroup);
begin
  fGroups.Remove(aGroup);
end;


procedure TKMUnitGroups.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('UnitGroups');
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
  for I := Count - 1 downto 0 do
    if not Groups[I].IsDead then
      Groups[I].UpdateState
    else
    if FREE_POINTERS and (Groups[I].fPointerCount = 0) then
    begin
      Groups[I].Free;
      fGroups.Delete(I);
    end;
end;


procedure TKMUnitGroups.Paint;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  if not Groups[I].IsDead then
    Groups[I].Paint;
end;


end.
