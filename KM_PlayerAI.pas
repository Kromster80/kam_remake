unit KM_PlayerAI;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
    KM_CommonClasses, KM_Defaults, KM_Terrain, KM_AIAttacks, KM_Houses, KM_Units, KM_Units_Warrior, KM_Utils, KM_Points;

type //For now IDs must match with KaM
  TAIDefencePosType = (adt_FrontLine=0, //Front line troops may not go on attacks, they are for defence
                       adt_BackLine=1); //Back line troops may attack

  TAIDefencePosition = class
  private
    fCurrentCommander: TKMUnitWarrior; //Commander of group currently occupying position
    procedure SetCurrentCommander(aCommander: TKMUnitWarrior);
    procedure ClearCurrentCommander;
  public
    Position: TKMPointDir; //Position and direction the group defending will stand
    GroupType: TGroupType; //Type of group to defend this position (e.g. melee)
    DefenceRadius: integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    DefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    constructor Create(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
    constructor Load(LoadStream:TKMemoryStream);
    destructor Destroy; override;
    property CurrentCommander: TKMUnitWarrior read fCurrentCommander write SetCurrentCommander;
    procedure Save(SaveStream:TKMemoryStream);
    procedure SyncLoad;
    function IsFullyStocked(aAmount: integer):boolean;
  end;

  TKMPlayerAI = class
  private
    fPlayerIndex: TPlayerIndex;
    fTimeOfLastAttackMessage: cardinal;
    fLastEquippedTime: cardinal;
    fHasWonOrLost:boolean; //Has this player won/lost? If so, do not check goals
    fAttacks: TAIAttacks;
    fAutobuild:boolean;

    procedure CheckDefeated;
    procedure CheckGoals;
    procedure CheckUnitCount;
    procedure CheckArmiesCount;
    procedure CheckArmy;
    procedure OrderAttack(aCommander: TKMUnitWarrior; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
    procedure RestockPositionWith(aDefenceGroup, aCommander:TKMUnitWarrior);
    function FindPlaceForWarrior(aWarrior:TKMUnitWarrior; aCanLinkToExisting, aTakeClosest:boolean):boolean;
    procedure RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);

  public
    ReqWorkers, ReqSerfFactor, ReqRecruits: word; //Number of each unit type required
    EquipRate: word; //Number of ticks between soldiers being equipped
    RecruitTrainTimeout: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    TownDefence, MaxSoldiers, Aggressiveness: integer; //-1 means not used or default
    StartPosition: TKMPoint; //Defines roughly where to defend and build around
    TroopFormations: array[TGroupType] of record //Defines how defending troops will be formatted. 0 means leave unchanged.
                                            NumUnits, UnitsPerRow:integer;
                                          end;
    DefencePositionsCount: integer;
    DefencePositions: array of TAIDefencePosition;

    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property Autobuild:boolean read fAutobuild write fAutobuild;
    property Attacks: TAIAttacks read fAttacks;

    procedure OwnerUpdate(aPlayer:TPlayerIndex);

    procedure CommanderDied(DeadCommander, NewCommander: TKMUnitWarrior);
    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker:TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker:TKMUnitWarrior);
    procedure WarriorEquipped(aWarrior: TKMUnitWarrior);

    function HouseAutoRepair:boolean; //Do we automatically repair all houses?
    procedure AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;

  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_TextLibrary, KM_Goals, KM_Player, KM_PlayerStats, KM_UnitTaskAttackHouse,
     KM_Resource, KM_Sound, KM_MessageStack, KM_EventProcess;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = false;
  AI_LINK_IDLE = false;


constructor TAIDefencePosition.Create(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
begin
  Inherited Create;
  Position := aPos;
  GroupType := aGroupType;
  DefenceRadius := aDefenceRadius;
  DefenceType := aDefenceType;
  CurrentCommander := nil; //Unoccupied
end;


destructor TAIDefencePosition.Destroy;
begin
  ClearCurrentCommander; //Ensure pointer is removed
  Inherited;
end;


procedure TAIDefencePosition.ClearCurrentCommander;
begin
  fPlayers.CleanUpUnitPointer(TKMUnit(fCurrentCommander));
end;


procedure TAIDefencePosition.SetCurrentCommander(aCommander: TKMUnitWarrior);
begin
  ClearCurrentCommander;
  if aCommander <> nil then
    fCurrentCommander := TKMUnitWarrior(aCommander.GetUnitPointer);
end;


procedure TAIDefencePosition.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(Position);
  SaveStream.Write(GroupType, SizeOf(GroupType));
  SaveStream.Write(DefenceRadius);
  SaveStream.Write(DefenceType, SizeOf(DefenceType));
  if fCurrentCommander <> nil then
    SaveStream.Write(fCurrentCommander.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TAIDefencePosition.Load(LoadStream:TKMemoryStream);
begin
  Inherited Create;
  LoadStream.Read(Position);
  LoadStream.Read(GroupType, SizeOf(GroupType));
  LoadStream.Read(DefenceRadius);
  LoadStream.Read(DefenceType, SizeOf(DefenceType));
  LoadStream.Read(fCurrentCommander, 4); //subst on syncload
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentCommander := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fCurrentCommander)));
end;


function TAIDefencePosition.IsFullyStocked(aAmount: integer):boolean;
begin
  Result := (CurrentCommander <> nil) and (CurrentCommander.GetMemberCount+1 >= aAmount);
end;


{ TKMPlayerAI }
constructor TKMPlayerAI.Create(aPlayerIndex: TPlayerIndex);
var i: TGroupType;
begin
  Inherited Create;

  fPlayerIndex := aPlayerIndex;
  fHasWonOrLost := false;
  fTimeOfLastAttackMessage := 0;
  DefencePositionsCount := 0;

  fAttacks := TAIAttacks.Create;

  //Set some defaults (these are not measured from KaM)
  ReqWorkers := 3;
  ReqRecruits := 5; //This means the number in the barracks, watchtowers are counted seperately
  EquipRate := 1000; //Measured in KaM: AI equips 1 soldier every ~100 seconds
  ReqSerfFactor := 10; //Means 1 serf per building
  RecruitTrainTimeout := 0; //Can train at start
  fAutobuild := true; //In KaM it is on by default, and most missions turn it off
  StartPosition := KMPoint(1,1);
  MaxSoldiers := high(MaxSoldiers); //No limit by default
  TownDefence := 100; //In KaM 100 is standard, although we don't completely understand this command
  Aggressiveness := 100; //No idea what the default for this is, it's barely used
  for i:=low(TGroupType) to high(TGroupType) do
  begin
    TroopFormations[i].NumUnits := 9; //These are the defaults in KaM
    TroopFormations[i].UnitsPerRow := 3;
  end;
end;


destructor TKMPlayerAI.Destroy;
var i: integer;
begin
  fAttacks.Free;
  for i:=0 to DefencePositionsCount-1 do DefencePositions[i].Free;
  Inherited;
end;


procedure TKMPlayerAI.CheckDefeated;
var
  Defeat: Boolean;
  Stat: TKMPlayerStats;
begin
  Stat := fPlayers[fPlayerIndex].Stats;

  //@Lewin: I have wrote my own vision of "Defeated" player here,
  //please discuss it with me if you have other ideas
  //@Krom: I think you should have to destroy storehouses, that's how it works in KaM.
  //Also requiring workers and recruits to be destroyed might be confusing for the player.
  //I'd rather keep it simple and like KaM: Army, store, school, barracks.

  Defeat := (Stat.GetHouseQty(ht_School) = 0) and
            (Stat.GetUnitQty(ut_Worker) = 0) and
            (Stat.GetUnitQty(ut_Recruit) = 0) and
            (Stat.GetArmyCount = 0) and
            (Stat.GetHouseQty(ht_Barracks) = 0) and
            (Stat.GetHouseQty(ht_TownHall) = 0) and
            (Stat.GetHouseQty(ht_SiegeWorkshop) = 0);

  //Let the event system know (defeat may trigger events for other players)
  if Defeat then
    fEventsManager.ProcDefeated(fPlayerIndex);
end;


procedure TKMPlayerAI.CheckGoals;

  function GoalConditionSatisfied(aGoal: TKMGoal): Boolean;
  var Stat: TKMPlayerStats;
  begin
    Assert((aGoal.GoalCondition = gc_Time) or (aGoal.PlayerIndex <> -1), 'Only gc_Time can have nil Player');

    Result := False;
    if aGoal.PlayerIndex <> -1 then
      Stat := fPlayers[aGoal.PlayerIndex].Stats
    else
      Stat := nil;

    case aGoal.GoalCondition of
      gc_BuildTutorial:     Result := Stat.GetHouseQty(ht_Tannery) = 0; //For some reason this goal is gs_False in KaM, that's why check is =0 not  > 0
      gc_Time:              Result := fGame.CheckTime(aGoal.GoalTime);
      gc_Buildings:         Result := (Stat.GetHouseQty(ht_Store) > 0) or (Stat.GetHouseQty(ht_School) > 0) or (Stat.GetHouseQty(ht_Barracks) > 0);
      gc_Troops:            Result := (Stat.GetArmyCount > 0);
      gc_MilitaryAssets:    Result := (Stat.GetArmyCount > 0) or (Stat.GetHouseQty(ht_Barracks) > 0) or (Stat.GetHouseQty(ht_CoalMine) > 0) or
                                      (Stat.GetHouseQty(ht_WeaponWorkshop) > 0) or (Stat.GetHouseQty(ht_ArmorWorkshop) > 0) or (Stat.GetHouseQty(ht_Stables) > 0) or
                                      (Stat.GetHouseQty(ht_IronMine) > 0) or (Stat.GetHouseQty(ht_IronSmithy) > 0) or (Stat.GetHouseQty(ht_WeaponSmithy) > 0) or
                                      (Stat.GetHouseQty(ht_ArmorSmithy) > 0) or (Stat.GetHouseQty(ht_TownHall) > 0) or (Stat.GetHouseQty(ht_SiegeWorkshop) > 0);
      gc_SerfsAndSchools:   Result := (Stat.GetHouseQty(ht_School) > 0) or (Stat.GetUnitQty(ut_Serf) > 0);
      gc_EconomyBuildings:  Result := (Stat.GetHouseQty(ht_Store) > 0) or (Stat.GetHouseQty(ht_School) > 0) or (Stat.GetHouseQty(ht_Inn) > 0);
      else                  Assert(false, 'Unknown goal');
    end;
    if aGoal.GoalStatus = gs_False then
      Result := not Result; //Reverse condition
  end;

var i: integer; VictorySatisfied, SurvivalSatisfied: boolean;
begin
  if fHasWonOrLost then Exit; //If player has elected to play on past victory or defeat then do not check for any further goals

  //Assume they will win/survive, then prove it with goals
  VictorySatisfied  := True;
  SurvivalSatisfied := True;

  with fPlayers[fPlayerIndex] do
  for I := 0 to Goals.Count - 1 do //Test each goal to see if it has occured
    if GoalConditionSatisfied(Goals[I]) then
    begin
      //Display message if set and not already shown and not a blank text
      if (Goals[I].MessageToShow <> 0)
      and not Goals[I].MessageHasShown
      //todo: replace with EVT files
      and (fTextLibrary[Goals[I].MessageToShow] <> '') then
      begin
        if MyPlayer = fPlayers[fPlayerIndex] then
          fGame.fGamePlayInterface.MessageIssue(mkText, fTextLibrary[Goals[I].MessageToShow], KMPoint(0,0));
        Goals.SetMessageHasShown(I);
      end;
    end
    else
    begin
      if Goals[I].GoalType = glt_Victory then
        VictorySatisfied := False;
      if Goals[I].GoalType = glt_Survive then
        SurvivalSatisfied := False;
    end;

  //Now we know if player has been defeated or won
  fHasWonOrLost := not SurvivalSatisfied or VictorySatisfied;

  //You can't win and lose at the same time. In KaM defeats override victories, except
  //when there are no goals defined, in which case you win for some weird reason...
  //But given that having no goals is pretty pointless we'll make defeat override so you can't
  //win battle missions by waiting for your troops to simultainiously starve to death.

  //Let everyone know in MP mode
  if (fGame.GameState <> gsReplay)
  and (fGame.MultiplayerMode or (MyPlayer = fPlayers[fPlayerIndex])) then
    if not SurvivalSatisfied then
      fGame.PlayerDefeat(fPlayerIndex)
    else
    if VictorySatisfied then
      fGame.PlayerVictory(fPlayerIndex);
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMPlayerAI.CheckUnitCount;
var
  i,k:integer;
  H:THouseType;
  UT:TUnitType;
  HS:TKMHouseSchool;
  UnitReq:array[TUnitType]of integer;
  Schools:array of TKMHouseSchool;

  function CheckUnitRequirements(Req:integer; aUnitType:TUnitType):boolean;
  begin
    //We summ up requirements for e.g. Recruits required at Towers and Barracks
    if fPlayers[fPlayerIndex].Stats.GetUnitQty(aUnitType) < (Req+UnitReq[aUnitType]) then
    begin
      dec(UnitReq[aUnitType]); //So other schools don't order same unit
      HS.AddUnitToQueue(aUnitType, 1);
      Result := true;
    end
    else
      Result := false;
  end;
begin
  //Find school and make sure it's free of tasks
  FillChar(UnitReq,SizeOf(UnitReq),#0); //Clear up

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  for H := Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[H].IsValid and (fResource.HouseDat[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
      inc(UnitReq[fResource.HouseDat[H].OwnerType], fPlayers[fPlayerIndex].Stats.GetHouseQty(H));

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, fPlayers[fPlayerIndex].Stats.GetHouseQty(ht_School));
  k := 1;
  HS := TKMHouseSchool(fPlayers[fPlayerIndex].FindHouse(ht_School,k));
  while HS <> nil do
  begin
    Schools[k-1] := HS;
    for i:=1 to 6 do //Decrease requirement for each unit in training
      if HS.UnitQueue[i]<>ut_None then
        dec(UnitReq[HS.UnitQueue[i]]); //Can be negative and compensated by e.g. ReqRecruits
    inc(k);
    HS := TKMHouseSchool(fPlayers[fPlayerIndex].FindHouse(ht_School,k));
  end;

  //Order the training
  for k:=1 to Length(Schools) do
  begin
    HS := Schools[k-1];
    if (HS<>nil)and(HS.UnitQueue[1]=ut_None) then
    begin
      //Order citizen training
      for UT:=Low(UnitReq) to High(UnitReq) do
        if (UnitReq[UT] > 0) and
           (UnitReq[UT] > fPlayers[fPlayerIndex].Stats.GetUnitQty(UT)) and
           (UT <> ut_None) then
        begin
          dec(UnitReq[UT]); //So other schools don't order same unit
          HS.AddUnitToQueue(UT, 1);
          break; //Don't need more UnitTypes yet
        end;

      //If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      //Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)
      if (HS.UnitQueue[1] = ut_None) then //Still haven't found a match...
        if not CheckUnitRequirements(Round((10/ReqSerfFactor)*fPlayers[fPlayerIndex].Stats.GetHouseQty(ht_Any)), ut_Serf) then
          if not CheckUnitRequirements(ReqWorkers, ut_Worker) then
            if fGame.CheckTime(RecruitTrainTimeout) then //Recruits can only be trained after this time
              if not CheckUnitRequirements(ReqRecruits * fPlayers[fPlayerIndex].Stats.GetHouseQty(ht_Barracks), ut_Recruit) then
                break; //There's no unit demand at all
    end;
  end;
end;


procedure TKMPlayerAI.CheckArmiesCount;
var
  Barracks:array of TKMHouseBarracks;
  HB:TKMHouseBarracks;
  GType: TGroupType;
  i,k:integer;
  TrainedSomething:boolean;
  GroupReq: array[TGroupType] of integer;
begin
  if fGame.IsPeaceTime then Exit; //Do not process train soldiers during peacetime
  if fPlayers[fPlayerIndex].Stats.GetArmyCount >= MaxSoldiers then Exit; //Don't train if we have reached our limit
  if not fGame.CheckTime(fLastEquippedTime+EquipRate) then Exit; //Delay between equipping soldiers for KaM compatibility
  fLastEquippedTime := fGame.GameTickCount;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  for k:=0 to DefencePositionsCount-1 do
    with DefencePositions[k] do
    if CurrentCommander = nil then
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits)
    else
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits - (TKMUnitWarrior(CurrentCommander).GetMemberCount+1));

  //Find barracks
  SetLength(Barracks, fPlayers[fPlayerIndex].Stats.GetHouseQty(ht_Barracks));
  k := 1;
  HB := TKMHouseBarracks(fPlayers[fPlayerIndex].FindHouse(ht_Barracks,k));
  while HB <> nil do
  begin
    Barracks[k-1] := HB;
    inc(k);
    HB := TKMHouseBarracks(fPlayers[fPlayerIndex].FindHouse(ht_Barracks,k));
  end;

  //Train troops where possible in each barracks
  for k:=1 to Length(Barracks) do
  begin
    HB := Barracks[k-1];
    //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
    i := 0;
    repeat
      GType := TGroupType(KaMRandom(4)); //Pick random from overall count
      inc(i);
    until (GroupReq[GType] > 0) or (i > 9); //Limit number of attempts to guarantee it doesn't loop forever

    for i:=1 to 3 do
    begin
      TrainedSomething := false;
      if AITroopTrainOrder[GType,i] <> ut_None then
        while HB.CanEquip(AITroopTrainOrder[GType,i]) and (GroupReq[GType] > 0) and
              (fPlayers[fPlayerIndex].Stats.GetArmyCount < MaxSoldiers) do
        begin
          HB.Equip(AITroopTrainOrder[GType,i], 1);
          dec(GroupReq[GType]);
          TrainedSomething := true;
          if EquipRate > 0 then break; //Only equip 1 soldier when we have a restricted equip rate
        end;
      if TrainedSomething and (EquipRate > 0) then break; //Only equip 1 soldier when we have a restricted equip rate
    end;
  end;
end;


procedure TKMPlayerAI.OrderAttack(aCommander: TKMUnitWarrior; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
var
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit  := nil;

  //Find target
  case aTarget of
    att_ClosestUnit:                  TargetUnit := fPlayers.GetClosestUnit(aCommander.GetPosition, fPlayerIndex, at_Enemy);
    att_ClosestBuildingFromArmy:      TargetHouse := fPlayers.GetClosestHouse(aCommander.GetPosition, fPlayerIndex, at_Enemy, false);
    att_ClosestBuildingFromStartPos:  TargetHouse := fPlayers.GetClosestHouse(StartPosition, fPlayerIndex, at_Enemy, false);
    att_CustomPosition:               begin
                                        TargetHouse := fPlayers.HousesHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetHouse <> nil) and
                                           (fPlayers.CheckAlliance(fPlayerIndex, TargetHouse.GetOwner) = at_Ally) then
                                          TargetHouse := nil;
                                        TargetUnit := fTerrain.UnitsHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetUnit <> nil) and
                                           (fPlayers.CheckAlliance(fPlayerIndex, TargetUnit.GetOwner) = at_Ally) then
                                          TargetUnit := nil;
                                      end;
  end;

  //Choose best option
  if TargetHouse <> nil then
    aCommander.OrderAttackHouse(TargetHouse)
  else if TargetUnit <> nil then
    aCommander.OrderAttackUnit(TargetUnit)
  else if aTarget = att_CustomPosition then
    aCommander.OrderWalk(aCustomPos);
end;


procedure TKMPlayerAI.RestockPositionWith(aDefenceGroup, aCommander:TKMUnitWarrior);
var Needed: integer;
begin
  Needed := TroopFormations[UnitGroups[aDefenceGroup.UnitType]].NumUnits - (aDefenceGroup.GetMemberCount+1);
  if Needed <= 0 then exit;
  if aCommander.GetMemberCount+1 <= Needed then
    aCommander.OrderLinkTo(aDefenceGroup) //Link entire group
  else
    aCommander.OrderSplitLinkTo(aDefenceGroup,Needed); //Link only as many units as are needed
end;


function TKMPlayerAI.FindPlaceForWarrior(aWarrior:TKMUnitWarrior; aCanLinkToExisting, aTakeClosest:boolean):boolean;
var k, MenRequired, Matched: integer;
    Distance, Best: single;
begin
  Result := false;
  Matched := -1;  Best := 9999;
  for k:=0 to DefencePositionsCount-1 do
  begin
    if aCanLinkToExisting then
      MenRequired := TroopFormations[DefencePositions[k].GroupType].NumUnits
    else MenRequired := 1; //If not aCanLinkToExisting then a group with 1 member or more counts as fully stocked already
    if (DefencePositions[k].GroupType = UnitGroups[aWarrior.UnitType]) and
       not DefencePositions[k].IsFullyStocked(MenRequired) then
    begin
      //Take closest position that is empty or requries restocking
      Distance := GetLength(aWarrior.GetPosition,DefencePositions[k].Position.Loc);
      if Distance < Best then
      begin
        Matched := k;
        Best := Distance;
        if not aTakeClosest then break; //Take first one we find - that's what KaM does
      end;
    end;
  end;
  if Matched <> -1 then
  begin
    Result := true;
    if DefencePositions[Matched].CurrentCommander = nil then
    begin //New position
      DefencePositions[Matched].CurrentCommander := aWarrior.GetCommander;
      aWarrior.OrderWalk(DefencePositions[Matched].Position);
    end
    else //Restock existing position
      RestockPositionWith(DefencePositions[Matched].CurrentCommander,aWarrior.GetCommander);
  end;
end;


procedure TKMPlayerAI.CheckArmy;
var AttackTotalAvailable: integer; //Total number of warriors available to attack the enemy
    AttackGroupsCount: array[TGroupType] of integer;
    AttackGroups: array[TGroupType] of array of TKMUnitWarrior;

  procedure AddToAvailableToAttack(aCommander: TKMUnitWarrior);
  var GT: TGroupType;
  begin
    GT := UnitGroups[aCommander.UnitType];
    if Length(AttackGroups[GT]) <= AttackGroupsCount[GT] then
      SetLength(AttackGroups[GT],AttackGroupsCount[GT]+10);
    AttackGroups[GT,AttackGroupsCount[GT]] := aCommander;
    inc(AttackGroupsCount[GT]);
    AttackTotalAvailable := AttackTotalAvailable + aCommander.GetMemberCount+1;
  end;

var i, k, j: integer;
    G: TGroupType;
    Positioned: boolean;
    NeedsLinkingTo: array[TGroupType] of TKMUnitWarrior;
begin
  AttackTotalAvailable := 0;
  for G:=Low(TGroupType) to High(TGroupType) do
  begin
    NeedsLinkingTo[G] := nil;
    AttackGroupsCount[G] := 0;
  end;

  //Hotfix until we refactor AI: Make sure no defence position commander is dead or not a commander
  for k:=0 to DefencePositionsCount-1 do
    with DefencePositions[k] do
      if (CurrentCommander <> nil) and (CurrentCommander.IsDeadOrDying or not CurrentCommander.IsCommander) then
        CurrentCommander := nil;

  //Iterate units list in search of warrior commanders, and then check the following: Hunger, (feed) formation, (units per row) position (from defence positions)
  for i:=0 to fPlayers[fPlayerIndex].Units.Count-1 do
  begin
    if TKMUnit(fPlayers[fPlayerIndex].Units.Items[i]) is TKMUnitWarrior then
      with TKMUnitWarrior(fPlayers[fPlayerIndex].Units.Items[i]) do
      if not IsDeadOrDying and Visible then //Ignore warriors which are dead or still in barracks
      begin
        //ALL WARRIORS: Check hunger and feed
        if (Condition < UNIT_MIN_CONDITION) then GetCommander.OrderFood;
        //ONLY COMMANDERS:
        if IsCommander then
        begin
          //If the warrior is busy then skip this group because the AI should not give orders to fighting warriors
          if ArmyInFight or (GetUnitTask is TTaskAttackHouse) or (OrderTarget <> nil) then
          begin
            //If this group belongs to a defence position and they are too far away we should disassociate
            //them from the defence position so new warriors can take up the defence if needs be
            for k:=0 to DefencePositionsCount-1 do
              with DefencePositions[k] do
                if (CurrentCommander = GetCommander) and (KMLength(Position.Loc, GetPosition) > DefenceRadius) then
                  CurrentCommander := nil;
            Continue;
          end;

          if fGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

          //Check formation. If the script has defined a group with more units per row than there should be, do not change it
          if UnitsPerRow < TroopFormations[UnitGroups[UnitType]].UnitsPerRow then
            UnitsPerRow := TroopFormations[UnitGroups[UnitType]].UnitsPerRow;
          //Position this group to defend if they already belong to a defence position
          Positioned := false;
          for k:=0 to DefencePositionsCount-1 do
            if DefencePositions[k].CurrentCommander = GetCommander then
            begin
              if GetCommander.CanWalkTo(DefencePositions[k].Position.Loc, 0) then
                OrderWalk(DefencePositions[k].Position);
              Positioned := true; //We already have a position, finished with this group

              //If this group is available to attack then count them
              if DefencePositions[k].DefenceType = adt_BackLine then
                AddToAvailableToAttack(GetCommander);

              //In KaM the order of defence positions is the priority: The first defined is higher priority
              for j:=0 to k-1 do
                if (DefencePositions[j].CurrentCommander = nil) and
                   (DefencePositions[j].GroupType = UnitGroups[UnitType]) then
                   begin
                     DefencePositions[k].CurrentCommander := nil; //Leave current position
                     DefencePositions[j].CurrentCommander := GetCommander; //Take new position
                     break;
                   end;

              break;
            end;

          //Look for group that needs additional members, or a new position to defend
          //In this case we choose the closest group, then move to a higher priority one later (see above)
          //This means at the start of the mission troops will take the position they are placed at rather than swapping around
          if not Positioned then
            Positioned := FindPlaceForWarrior(TKMUnitWarrior(fPlayers[fPlayerIndex].Units.Items[i]), AI_LINK_IDLE, AI_FILL_CLOSEST);

          //Just chill and link with other idle groups
          if not Positioned then
          begin
            AddToAvailableToAttack(GetCommander); //Idle groups may also attack
            if AI_LINK_IDLE then
              //If this group doesn't have enough members
              if (GetMemberCount+1 < TroopFormations[UnitGroups[UnitType]].NumUnits) then
                if NeedsLinkingTo[UnitGroups[UnitType]] = nil then
                  NeedsLinkingTo[UnitGroups[UnitType]] := GetCommander //Flag us as needing to be added to
                else
                begin
                  RestockPositionWith(NeedsLinkingTo[UnitGroups[UnitType]],GetCommander);
                  if NeedsLinkingTo[UnitGroups[UnitType]].GetMemberCount+1 >= TroopFormations[UnitGroups[UnitType]].NumUnits then
                    NeedsLinkingTo[UnitGroups[UnitType]] := nil; //Group is now full
                end;
          end;

        end;
      end;
  end;

  //Now process AI attacks (we have compiled a list of warriors available to attack)
  if not fGame.IsPeaceTime then
    for i:=0 to Attacks.Count - 1 do
    if Attacks.MayOccur(i, AttackTotalAvailable, AttackGroupsCount, fGame.GameTickCount) then //Check conditions are right
    begin
      //Order groups to attack
      if Attacks[i].TakeAll then
      begin
        for G:=Low(TGroupType) to High(TGroupType) do
          for j:=1 to AttackGroupsCount[G] do
            OrderAttack(AttackGroups[G, integer(j)-1], Attacks[i].Target, Attacks[i].CustomPosition);
      end
      else
      begin
        for G:=Low(TGroupType) to High(TGroupType) do
          for j:=1 to Attacks[i].GroupAmounts[G] do
            OrderAttack(AttackGroups[G, integer(j)-1], Attacks[i].Target, Attacks[i].CustomPosition);
      end;
      Attacks.Occured(i); //We can't set the flag to property record directly
    end;
end;


procedure TKMPlayerAI.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fPlayerIndex := aPlayer;
end;


//This is run by commanders when they die.
//Dead commander is the one that died, NewCommander is the one that replaced him.
//We need to update CurrentCommander for defence positions in this case.
procedure TKMPlayerAI.CommanderDied(DeadCommander, NewCommander: TKMUnitWarrior);
var i: integer;
begin
  for i:=0 to DefencePositionsCount-1 do
    with DefencePositions[i] do
      if CurrentCommander = DeadCommander then
        CurrentCommander := NewCommander; //Don't need to use GetPointer/ReleasePointer because setting CurrentCommander does that
end;


procedure TKMPlayerAI.WarriorEquipped(aWarrior: TKMUnitWarrior);
begin
  Assert(aWarrior.IsCommander); //A warrior walking out of the barracks should not be linked yet
  FindPlaceForWarrior(aWarrior, true, AI_FILL_CLOSEST);
end;


procedure TKMPlayerAI.RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
var i: integer;
begin
  if fPlayers[fPlayerIndex].PlayerType = pt_Human then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for i := 0 to DefencePositionsCount-1 do
    with DefencePositions[i] do
      if (CurrentCommander <> nil) and (not CurrentCommander.ArmyInFight)
      and (CurrentCommander.OrderTarget = nil)
      and (KMLength(CurrentCommander.GetPosition, aAttacker.GetPosition) <= DefenceRadius) then
        CurrentCommander.OrderAttackUnit(aAttacker);
end;


//aHouse is our house that was attacked
procedure TKMPlayerAI.HouseAttackNotification(aHouse: TKMHouse; aAttacker:TKMUnitWarrior);
begin
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:
      begin
        if fGame.CheckTime(fTimeOfLastAttackMessage + TIME_ATTACK_WARNINGS) then
        begin
          //Process anyway for multiplayer consistency
          //(and it is desired behaviour: if player saw attack,
          //don't notify him as soon as he looks away)
          fTimeOfLastAttackMessage := fGame.GameTickCount;
          if (MyPlayer = fPlayers[fPlayerIndex]) and (GetLength(fGame.Viewport.Position, KMPointF(aHouse.GetPosition)) >= DISTANCE_FOR_WARNINGS) then
            fSoundLib.PlayNotification(an_Town);
        end;
      end;
    pt_Computer:
      RetaliateAgainstThreat(aAttacker);
  end;
end;


//aUnit is our unit that was attacked
procedure TKMPlayerAI.UnitAttackNotification(aUnit: TKMUnit; aAttacker:TKMUnitWarrior);
begin
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:
      if fGame.CheckTime(fTimeOfLastAttackMessage + TIME_ATTACK_WARNINGS) then
      begin
        fTimeOfLastAttackMessage := fGame.GameTickCount; //Process anyway for multiplayer consistency (and it is desired behaviour: if player saw attack, don't notify him as soon as he looks away)
        if (MyPlayer = fPlayers[fPlayerIndex]) and (GetLength(fGame.Viewport.Position, KMPointF(aUnit.GetPosition)) >= DISTANCE_FOR_WARNINGS) then
        begin
          if aUnit is TKMUnitWarrior then
            fSoundLib.PlayNotification(an_Troops)
          else
            fSoundLib.PlayNotification(an_Citizens);
        end;
      end;
    pt_Computer:
      begin
        //If we are attacked, then we should counter attack the attacker!
        if aUnit is TKMUnitWarrior then
          with TKMUnitWarrior(aUnit).GetCommander do
            if not ArmyInFight then
              OrderAttackUnit(aAttacker);
        RetaliateAgainstThreat(aAttacker); //Nearby soldiers should come to assist
      end;
  end;
end;


//Do we automatically repair all houses?
//For now use fAutobuild, which is what KaM does. Later we can add a script command to turn this on and off
//Also could be changed later to disable repairing when under attack? (only repair if the enemy goes away?)
function TKMPlayerAI.HouseAutoRepair:boolean;
begin
  Result := fAutobuild;
end;


procedure TKMPlayerAI.AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
begin
  SetLength(DefencePositions, DefencePositionsCount+1);
  DefencePositions[DefencePositionsCount] := TAIDefencePosition.Create(aPos,aGroupType,aDefenceRadius,aDefenceType);
  inc(DefencePositionsCount);
end;


procedure TKMPlayerAI.Save(SaveStream:TKMemoryStream);
var i: integer;
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(fPlayerIndex);
  SaveStream.Write(fHasWonOrLost);
  SaveStream.Write(fTimeOfLastAttackMessage);
  SaveStream.Write(fLastEquippedTime);
  SaveStream.Write(ReqWorkers);
  SaveStream.Write(ReqSerfFactor);
  SaveStream.Write(ReqRecruits);
  SaveStream.Write(EquipRate);
  SaveStream.Write(RecruitTrainTimeout);
  SaveStream.Write(TownDefence);
  SaveStream.Write(MaxSoldiers);
  SaveStream.Write(Aggressiveness);
  SaveStream.Write(StartPosition);
  SaveStream.Write(fAutobuild);
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(DefencePositionsCount);
  for i:=0 to DefencePositionsCount-1 do
    DefencePositions[i].Save(SaveStream);

  Attacks.Save(SaveStream);
end;


procedure TKMPlayerAI.Load(LoadStream:TKMemoryStream);
var I: Integer;
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(fPlayerIndex);
  LoadStream.Read(fHasWonOrLost);
  LoadStream.Read(fTimeOfLastAttackMessage);
  LoadStream.Read(fLastEquippedTime);
  LoadStream.Read(ReqWorkers);
  LoadStream.Read(ReqSerfFactor);
  LoadStream.Read(ReqRecruits);
  LoadStream.Read(EquipRate);
  LoadStream.Read(RecruitTrainTimeout);
  LoadStream.Read(TownDefence);
  LoadStream.Read(MaxSoldiers);
  LoadStream.Read(Aggressiveness);
  LoadStream.Read(StartPosition);
  LoadStream.Read(fAutobuild);
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(DefencePositionsCount);
  SetLength(DefencePositions, DefencePositionsCount);
  for I := 0 to DefencePositionsCount - 1 do
    DefencePositions[I] := TAIDefencePosition.Load(LoadStream);

  Attacks.Load(LoadStream);
end;


procedure TKMPlayerAI.SyncLoad;
var I: Integer;
begin
  for I := 0 to DefencePositionsCount - 1 do
    DefencePositions[I].SyncLoad;
end;


procedure TKMPlayerAI.UpdateState;
begin
  //Check if player has been defeated for Events
  CheckDefeated;

  //Check goals for all players to maintain multiplayer consistency
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:     CheckGoals; //This procedure manages victory, loss and messages all in one
    pt_Computer:  if (MyPlayer <> fPlayers[fPlayerIndex]) then
                  begin
                    CheckUnitCount; //Train new units (citizens, serfs, workers and recruits) if needed
                    CheckArmy; //Feed army, position defence, arrange/organise groups
                    CheckArmiesCount; //Train new soldiers if needed
                    //CheckHouseCount; //Build new houses if needed
                    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
                    //CheckAndIssueAttack; //Attack enemy
                    //Anything Else?
                  end;
  end;
end;


end.
