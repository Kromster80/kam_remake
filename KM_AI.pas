unit KM_AI;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, Math,
    KM_CommonClasses, KM_Defaults, KM_Terrain,
    KM_AIAttacks, KM_Houses, KM_Units, KM_UnitGroups, KM_Units_Warrior, KM_Utils, KM_Points,
    KM_AISetup, KM_AIMayor, KM_AIGeneral, KM_AIDefensePos;


type
  TWonOrLost = (wol_None, wol_Won, wol_Lost);

  TKMPlayerAI = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fMayor: TKMayor;
    fGeneral: TKMGeneral;
    fDefencePositions: TAIDefencePositions;

    fLastEquippedTime: cardinal;
    fWonOrLost: TWonOrLost; //Has this player won/lost? If so, do not check goals
    fAttacks: TAIAttacks;

    procedure CheckDefeated;
    procedure CheckGoals;
    procedure CheckArmiesCount;
    procedure CheckArmy;
    procedure CheckCanAttack;
    procedure OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
    procedure RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property Attacks: TAIAttacks read fAttacks;
    property Setup: TKMPlayerAISetup read fSetup;
    property Mayor: TKMayor read fMayor;
    property General: TKMGeneral read fGeneral;
    property DefencePositions: TAIDefencePositions read fDefencePositions;

    property WonOrLost: TWonOrLost read fWonOrLost;

    procedure OwnerUpdate(aPlayer: TPlayerIndex);

    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);

    function HouseAutoRepair: Boolean; //Do we automatically repair all houses?

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Goals, KM_Player, KM_PlayerStats,
     KM_Sound, KM_Events;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = false;
  AI_LINK_IDLE = false;


{ TKMPlayerAI }
constructor TKMPlayerAI.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;

  fOwner := aPlayerIndex;
  fSetup := TKMPlayerAISetup.Create;
  fMayor := TKMayor.Create(fOwner, fSetup);
  fGeneral := TKMGeneral.Create(fOwner, fSetup);
  fWonOrLost := wol_None;

  fDefencePositions := TAIDefencePositions.Create;

  fAttacks := TAIAttacks.Create;
end;


destructor TKMPlayerAI.Destroy;
begin
  fGeneral.Free;
  fMayor.Free;
  fSetup.Free;
  fAttacks.Free;
  fDefencePositions.Free;

  inherited;
end;


procedure TKMPlayerAI.CheckDefeated;
var
  Defeat: Boolean;
  Stat: TKMPlayerStats;
begin
  Stat := fPlayers[fOwner].Stats;

  //KaM defeat conditions are merge of two things: simplicity and objective.
  //They imply that enemy is powerful enough to destroy all houses and units,
  //but destroying Store-School-Barracks-Army is enough to show that.
  //Of course opponent can rebuild with workers, but that will take a lot of time in
  //already half-ruined city.

  Defeat := (Stat.GetHouseQty([ht_School, ht_Barracks, ht_Store, ht_TownHall, ht_SiegeWorkshop]) = 0) and
            (Stat.GetArmyCount = 0);

  //Let the event system know (defeat may trigger events for other players)
  if Defeat then
    fEventsManager.ProcDefeated(fOwner);
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
      gc_BuildTutorial:     Result := Stat.GetHouseQty([ht_Tannery]) = 0; //For some reason this goal is gs_False in KaM, that's why check is =0 not  > 0
      //gc_Time is disabled as we process messages in Event system now. Return true so players
      //do not have to wait for all messages to show before they are allowed to win (same in TPR)
      gc_Time:              Result := True;
      gc_Buildings:         Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Barracks]) > 0);
      gc_Troops:            Result := (Stat.GetArmyCount > 0);
      gc_MilitaryAssets:    Result := (Stat.GetArmyCount > 0) or
                                      (Stat.GetHouseQty([ht_Barracks, ht_CoalMine, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_Stables,
                                                         ht_IronMine, ht_IronSmithy ,ht_WeaponSmithy, ht_ArmorSmithy, ht_TownHall,
                                                         ht_SiegeWorkshop]) > 0);
      gc_SerfsAndSchools:   Result := (Stat.GetHouseQty([ht_School]) > 0) or (Stat.GetUnitQty(ut_Serf) > 0);
      gc_EconomyBuildings:  Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Inn]) > 0);
      else                  Assert(false, 'Unknown goal');
    end;
    if aGoal.GoalStatus = gs_False then
      Result := not Result; //Reverse condition
  end;

var
  I: Integer;
  VictorySatisfied, SurvivalSatisfied: Boolean;
begin
  //If player has elected to play on past victory or defeat
  //then do not check for any further goals
  if fWonOrLost <> wol_None then Exit;

  //Assume they will win/survive, then prove it with goals
  VictorySatisfied  := True;
  SurvivalSatisfied := True;

  with fPlayers[fOwner] do
  for I := 0 to Goals.Count - 1 do //Test each goal to see if it has occured
    if GoalConditionSatisfied(Goals[I]) then
    begin
      //Messages in goals have been replaced by EVT files, so this code is disabled now,
      //but kept in case we need it for something later. (conversion process?)

      //Display message if set and not already shown and not a blank text
      {if (Goals[I].MessageToShow <> 0)
      and not Goals[I].MessageHasShown
      and (fTextLibrary[Goals[I].MessageToShow] <> '') then
      begin
        if MyPlayer = fPlayers[fPlayerIndex] then
          fGameG.ShowMessage(mkText, fTextLibrary[Goals[I].MessageToShow], KMPoint(0,0));
        Goals.SetMessageHasShown(I);
      end;}
    end
    else
    begin
      if Goals[I].GoalType = glt_Victory then
        VictorySatisfied := False;
      if Goals[I].GoalType = glt_Survive then
        SurvivalSatisfied := False;
    end;

  //Now we know if player has been defeated or won
  if not SurvivalSatisfied then
    fWonOrLost := wol_Lost
  else
    if VictorySatisfied then
      fWonOrLost := wol_Won;

  //You can't win and lose at the same time. In KaM defeats override victories, except
  //when there are no goals defined, in which case you win for some weird reason...
  //But given that having no goals is pretty pointless we'll make defeat override so you can't
  //win battle missions by waiting for your troops to simultainiously starve to death.

  //Let everyone know in MP mode
  if not fGame.IsReplay
  and (fGame.IsMultiplayer or (MyPlayer = fPlayers[fOwner])) then
    if not SurvivalSatisfied then
      fGame.PlayerDefeat(fOwner)
    else
    if VictorySatisfied then
      fGame.PlayerVictory(fOwner);
end;


procedure TKMPlayerAI.CheckArmiesCount;
var
  Barracks: array of TKMHouseBarracks;
  HB: TKMHouseBarracks;
  GT: TGroupType;
  I,K: Integer;
  UT: TUnitType;
  TrainedSomething, CanEquipIron, CanEquipLeather: Boolean;
  GroupReq: array [TGroupType] of Integer;
begin
  if fGame.IsPeaceTime then Exit; //Do not train soldiers during peacetime

  if fPlayers[fOwner].Stats.GetArmyCount >= Setup.MaxSoldiers then Exit; //Don't train if we have reached our limit

  //Delay between equipping soldiers for KaM compatibility
  CanEquipIron := fGame.CheckTime(fLastEquippedTime + Setup.EquipRateIron);
  CanEquipLeather := fGame.CheckTime(fLastEquippedTime + Setup.EquipRateLeather);

  if not CanEquipIron and not CanEquipLeather then Exit;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  for I := 0 to fDefencePositions.Count - 1 do
  with fDefencePositions[I] do
  if CurrentGroup = nil then
    Inc(GroupReq[GroupType], fDefencePositions.TroopFormations[GroupType].NumUnits)
  else
    Inc(GroupReq[GroupType], Max(fDefencePositions.TroopFormations[GroupType].NumUnits - CurrentGroup.Count, 0));

  //If we don't need anyone - Exit
  I := 0;
  for GT := Low(GroupReq) to High(GroupReq) do
    Inc(I, GroupReq[GT]);
  if I = 0 then Exit;

  //Find barracks
  SetLength(Barracks, fPlayers[fOwner].Stats.GetHouseQty(ht_Barracks));
  I := 0;
  HB := TKMHouseBarracks(fPlayers[fOwner].FindHouse(ht_Barracks, I+1));
  while HB <> nil do
  begin
    Barracks[I] := HB;
    Inc(I);
    HB := TKMHouseBarracks(fPlayers[fOwner].FindHouse(ht_Barracks, I+1));
  end;

  //Train troops where possible in each barracks
  for I := 0 to High(Barracks) do
  begin
    HB := Barracks[I];
    //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
    K := 0;
    repeat
      GT := TGroupType(KaMRandom(4)); //Pick random from overall count
      Inc(K);
    until (GroupReq[GT] > 0) or (K > 9); //Limit number of attempts to guarantee it doesn't loop forever

    if GroupReq[GT] = 0 then Break; //Don't train

    for K := 1 to 3 do
    begin
      TrainedSomething := False;
      UT := AITroopTrainOrder[GT, K];
      if (UT <> ut_None)
      and ((CanEquipIron and (UT in WARRIORS_IRON)) or (CanEquipLeather and not (UT in WARRIORS_IRON))) then
        while HB.CanEquip(UT)
        and (GroupReq[GT] > 0)
        and (fPlayers[fOwner].Stats.GetArmyCount < Setup.MaxSoldiers) do
        begin
          HB.Equip(UT, 1);
          Dec(GroupReq[GT]);
          TrainedSomething := True;
          fLastEquippedTime := fGame.GameTickCount; //Only reset it when we actually trained something
          if Setup.GetEquipRate(UT) > 0 then
            Break; //Only equip 1 soldier when we have a restricted equip rate
        end;
      if TrainedSomething and (Setup.GetEquipRate(UT) > 0) then
        Break; //Only equip 1 soldier when we have a restricted equip rate
    end;
  end;
end;


procedure TKMPlayerAI.OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
var
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit  := nil;

  //Find target
  case aTarget of
    att_ClosestUnit:                  TargetUnit := fPlayers.GetClosestUnit(aGroup.Position, fOwner, at_Enemy);
    att_ClosestBuildingFromArmy:      TargetHouse := fPlayers.GetClosestHouse(aGroup.Position, fOwner, at_Enemy, false);
    att_ClosestBuildingFromStartPos:  TargetHouse := fPlayers.GetClosestHouse(Setup.StartPosition, fOwner, at_Enemy, false);
    att_CustomPosition:               begin
                                        TargetHouse := fPlayers.HousesHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetHouse <> nil) and
                                           (fPlayers.CheckAlliance(fOwner, TargetHouse.Owner) = at_Ally) then
                                          TargetHouse := nil;
                                        TargetUnit := fTerrain.UnitsHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetUnit <> nil) and
                                           (fPlayers.CheckAlliance(fOwner, TargetUnit.Owner) = at_Ally) then
                                          TargetUnit := nil;
                                      end;
  end;

  //Choose best option
  if TargetHouse <> nil then
    aGroup.OrderAttackHouse(TargetHouse)
  else if TargetUnit <> nil then
    aGroup.OrderAttackUnit(TargetUnit)
  else if aTarget = att_CustomPosition then
    aGroup.OrderWalk(aCustomPos);
end;


procedure TKMPlayerAI.CheckArmy;
var
  I: Integer;
  G: TGroupType;
  Positioned: Boolean;
  Group: TKMUnitGroup;
  NeedsLinkingTo: array [TGroupType] of TKMUnitGroup;
begin
  for G := Low(TGroupType) to High(TGroupType) do
    NeedsLinkingTo[G] := nil;

  //Check: Hunger, (feed) formation, (units per row) position (from defence positions)
  for I := 0 to fPlayers[fOwner].UnitGroups.Count - 1 do
  if not fPlayers[fOwner].UnitGroups[I].IsDead then
  begin
    Group := fPlayers[fOwner].UnitGroups[I];

    //Check hunger and order food
    //todo: Not sure we could order food for e.g. fighting groups, maybe move below
    if (Group.Condition < UNIT_MIN_CONDITION) then
      Group.OrderFood;

    if fGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

    //Check formation. If the script has defined a group with more units per row than there should be, do not change it
    //todo: That should be checked only once on creation of the group
    if Group.UnitsPerRow < fDefencePositions.TroopFormations[Group.GroupType].UnitsPerRow then
      Group.UnitsPerRow := fDefencePositions.TroopFormations[Group.GroupType].UnitsPerRow;

    //We already have a position, finished with this group
    Positioned := fDefencePositions.FindPositionOf(Group) <> nil;

    if Positioned then Continue;

    //Look for group that needs additional members, or a new position to defend
    //In this case we choose the closest group, then move to a higher priority one later (see above)
    //This means at the start of the mission troops will take the position they are placed at rather than swapping around
    Positioned := fDefencePositions.FindPlaceForGroup(Group, AI_LINK_IDLE, AI_FILL_CLOSEST);

    if Positioned then Continue;

    //Just chill and link with other idle groups
    if AI_LINK_IDLE then
      //If this group doesn't have enough members
      if (Group.Count < fDefencePositions.TroopFormations[Group.GroupType].NumUnits) then
        if NeedsLinkingTo[Group.GroupType] = nil then
          NeedsLinkingTo[Group.GroupType] := Group //Flag us as needing to be added to
        else
        begin
          fDefencePositions.RestockPositionWith(NeedsLinkingTo[UnitGroups[Group.UnitType]], Group);
          if NeedsLinkingTo[Group.GroupType].Count >= fDefencePositions.TroopFormations[UnitGroups[Group.UnitType]].NumUnits then
            NeedsLinkingTo[Group.GroupType] := nil; //Group is now full
        end;
  end;
end;


procedure TKMPlayerAI.CheckCanAttack;
var
  AttackTotalAvailable: Word; //Total number of warriors available to attack the enemy
  AttackGroupsCount: array [TGroupType] of Word;
  AttackGroups: array [TGroupType] of array of TKMUnitGroup;

  procedure AddToAvailableToAttack(aGroup: TKMUnitGroup);
  var GT: TGroupType;
  begin
    GT := UnitGroups[aGroup.UnitType];
    if Length(AttackGroups[GT]) <= AttackGroupsCount[GT] then
      SetLength(AttackGroups[GT],AttackGroupsCount[GT] + 10);
    AttackGroups[GT, AttackGroupsCount[GT]] := aGroup;
    Inc(AttackGroupsCount[GT]);
    Inc(AttackTotalAvailable, aGroup.Count);
  end;

var
  I, K: Integer;
  G: TGroupType;
  Group: TKMUnitGroup;
  DP: TAIDefencePosition;
begin
  //Do not process attack or defence during peacetime
  if fGame.IsPeaceTime then Exit;

  AttackTotalAvailable := 0;
  for G := Low(TGroupType) to High(TGroupType) do
    AttackGroupsCount[G] := 0;

  //Take all idling Groups that are:
  // - not linked to any Defence positions
  // - are in backline defence positions
  for I := 0 to fPlayers[fOwner].UnitGroups.Count - 1 do
  begin
    Group := fPlayers[fOwner].UnitGroups[I];
    if not Group.IsDead //Ignore warriors which are dead
    and not Group.InFight
    and not (Group.Order in [goAttackUnit, goAttackHouse, goStorm]) then
    begin
      DP := fDefencePositions.FindPositionOf(Group);
      if (DP = nil) or (DP.DefenceType = adt_BackLine) then
        AddToAvailableToAttack(Group);
    end;
  end;

  //Now process AI attacks (we have compiled a list of warriors available to attack)
  for I := 0 to Attacks.Count - 1 do
  if Attacks.MayOccur(I, AttackTotalAvailable, AttackGroupsCount, fGame.GameTickCount) then //Check conditions are right
  begin
    //Order groups to attack
    if Attacks[I].TakeAll then
    begin
      for G := Low(TGroupType) to High(TGroupType) do
        for K := 0 to AttackGroupsCount[G] - 1 do
          OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
    end
    else
    begin
      for G := Low(TGroupType) to High(TGroupType) do
        for K := 0 to Attacks[I].GroupAmounts[G] - 1 do
          OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
    end;
    Attacks.Occured(I); //We can't set the flag to property record directly
  end;
end;


procedure TKMPlayerAI.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fMayor.OwnerUpdate(fOwner);
  fGeneral.OwnerUpdate(fOwner);
end;


procedure TKMPlayerAI.RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
var
  I: Integer;
  Group: TKMUnitGroup;
begin
  if fPlayers[fOwner].PlayerType = pt_Human then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for i := 0 to fDefencePositions.Count - 1 do
  begin
    Group := fDefencePositions[i].CurrentGroup;
    if (Group <> nil) and not Group.IsDead
      and not Group.InFight
      and Group.IsAttackingEnemy
      and (KMLengthDiag(Group.Position, aAttacker.GetPosition) <= fDefencePositions[I].Radius) then
        Group.OrderAttackUnit(aAttacker);
  end;
end;


//aHouse is our house that was attacked
procedure TKMPlayerAI.HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
begin
  case fPlayers[fOwner].PlayerType of
    pt_Human:
      begin
        //No fight alerts in replays, and only show alerts for ourselves
        if (not fGame.IsReplay) and (fOwner = MyPlayer.PlayerIndex) then
          fGame.Alerts.AddFight(KMPointF(aHouse.GetPosition), fOwner, an_Town);
      end;
    pt_Computer:
      RetaliateAgainstThreat(aAttacker);
  end;
end;


//aUnit is our unit that was attacked
procedure TKMPlayerAI.UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);
const
  NotifyKind: array [Boolean] of TAttackNotification = (an_Citizens, an_Troops);
var
  Group: TKMUnitGroup;
begin
  case fPlayers[fOwner].PlayerType of
    pt_Human:
      //No fight alerts in replays, and only show alerts for ourselves
      if (not fGame.IsReplay) and (fOwner = MyPlayer.PlayerIndex) then
        fGame.Alerts.AddFight(aUnit.PositionF, fOwner, NotifyKind[aUnit is TKMUnitWarrior]);
    pt_Computer:
      begin
        //If we are attacked, then we should counter attack the attacker!
        if aUnit is TKMUnitWarrior then
        begin
          Group := fPlayers[fOwner].UnitGroups.GetGroupByMember(TKMUnitWarrior(aUnit));
          //If we are already in the process of attacking something, don't change our minds,
          //otherwise you can make a unit walk backwards and forwards forever between two groups of archers.
          if not Group.InFight and Group.IsAttackingEnemy then
            Group.OrderAttackUnit(aAttacker);
        end;
        RetaliateAgainstThreat(aAttacker); //Nearby soldiers should come to assist
      end;
  end;
end;


//Do we automatically repair all houses?
//For now use fAutobuild, which is what KaM does. Later we can add a script command to turn this on and off
//Also could be changed later to disable repairing when under attack? (only repair if the enemy goes away?)
function TKMPlayerAI.HouseAutoRepair: Boolean;
begin
  Result := fSetup.AutoBuild;
end;


procedure TKMPlayerAI.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(fLastEquippedTime);
  SaveStream.Write(fOwner);
  SaveStream.Write(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Save(SaveStream);
  fAttacks.Save(SaveStream);
  fDefencePositions.Save(SaveStream);
  fGeneral.Save(SaveStream);
  fMayor.Save(SaveStream);
end;


procedure TKMPlayerAI.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(fLastEquippedTime);
  LoadStream.Read(fOwner);
  LoadStream.Read(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Load(LoadStream);
  fAttacks.Load(LoadStream);
  fDefencePositions.Load(LoadStream);
  fGeneral.Load(LoadStream);
  fMayor.Load(LoadStream);
end;


procedure TKMPlayerAI.SyncLoad;
begin
  fDefencePositions.SyncLoad;
end;


//todo: Updates should be well separated, maybe we can make an interleaved array or something
//where updates will stacked to execute 1 at a tick
//OR maybe we can collect all Updates into one list and run them from there (sounds like a better more manageble idea)
procedure TKMPlayerAI.UpdateState(aTick: Cardinal);
begin
  //Check if player has been defeated for Events
  if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
    CheckDefeated;

  //Check goals for all players to maintain multiplayer consistency
  //AI does not care if it won or lost and Human dont need Mayor and Army management
  case fPlayers[fOwner].PlayerType of
    pt_Human:     begin
                    if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
                      CheckGoals; //This procedure manages victory and loss
                  end;
    pt_Computer:  begin
                    fMayor.UpdateState(aTick);
                    fGeneral.UpdateState(aTick);
                    if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
                    begin
                      fDefencePositions.UpdateState;
                      CheckArmy; //Feed army, position defence, arrange/organise groups
                      CheckCanAttack;
                      CheckArmiesCount; //Train new soldiers if needed
                    end;

                    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
                    //CheckAndIssueAttack; //Attack enemy
                    //Anything Else?
                  end;
  end;
end;


end.
