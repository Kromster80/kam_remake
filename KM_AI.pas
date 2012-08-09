unit KM_AI;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, Math,
    KM_CommonClasses, KM_Defaults, KM_Terrain,
    KM_AIAttacks, KM_Houses, KM_Units, KM_Units_Warrior, KM_Utils, KM_Points,
    KM_AISetup, KM_AIMayor, KM_AIGeneral, KM_AIDefensePos;


type
  TKMPlayerAI = class
  private
    fPlayerIndex: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fMayor: TKMayor;
    fGeneral: TKMGeneral;

    fLastEquippedTime: cardinal;
    fHasWonOrLost: boolean; //Has this player won/lost? If so, do not check goals
    fAttacks: TAIAttacks;

    procedure CheckDefeated;
    procedure CheckGoals;
    procedure CheckArmiesCount;
    procedure CheckArmy;
    procedure CheckCanAttack;
    procedure OrderAttack(aCommander: TKMUnitWarrior; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
    procedure RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
  public
    DefencePositions: TAIDefencePositions;

    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property Attacks: TAIAttacks read fAttacks;
    property Setup: TKMPlayerAISetup read fSetup;
    property Mayor: TKMayor read fMayor;
    property General: TKMGeneral read fGeneral;

    procedure OwnerUpdate(aPlayer:TPlayerIndex);

    procedure CommanderDied(DeadCommander, NewCommander: TKMUnitWarrior);
    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker:TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker:TKMUnitWarrior);
    procedure WarriorEquipped(aWarrior: TKMUnitWarrior);

    function HouseAutoRepair:boolean; //Do we automatically repair all houses?

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Goals, KM_Player, KM_PlayerStats, KM_UnitTaskAttackHouse,
     KM_Sound, KM_Events, KM_UnitActionWalkTo;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = false;
  AI_LINK_IDLE = false;


{ TKMPlayerAI }
constructor TKMPlayerAI.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;

  fPlayerIndex := aPlayerIndex;
  fSetup := TKMPlayerAISetup.Create;
  fMayor := TKMayor.Create(fPlayerIndex, fSetup);
  fGeneral := TKMGeneral.Create(fPlayerIndex, fSetup);
  fHasWonOrLost := false;

  DefencePositions := TAIDefencePositions.Create();

  fAttacks := TAIAttacks.Create;
end;


destructor TKMPlayerAI.Destroy;
begin
  fGeneral.Free;
  fMayor.Free;
  fSetup.Free;
  fAttacks.Free;
  DefencePositions.Free;

  inherited;
end;


procedure TKMPlayerAI.CheckDefeated;
var
  Defeat: Boolean;
  Stat: TKMPlayerStats;
begin
  Stat := fPlayers[fPlayerIndex].Stats;

  //KaM defeat conditions are merge of two things: simplicity and objective.
  //They imply that enemy is powerful enough to destroy all houses and units,
  //but destroying Store-School-Barracks-Army is enough to show that.
  //Of course opponent can rebuild with workers, but that will take a lot of time in
  //already half-ruined city.

  Defeat := (Stat.GetHouseQty(ht_School) = 0) and
            (Stat.GetArmyCount = 0) and
            (Stat.GetHouseQty(ht_Barracks) = 0) and
            (Stat.GetHouseQty(ht_Store) = 0) and
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
      //gc_Time is disabled as we process messages in Event system now. Return true so players
      //do not have to wait for all messages to show before they are allowed to win (same in TPR)
      gc_Time:              Result := True;
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

var
  I: Integer;
  VictorySatisfied, SurvivalSatisfied: Boolean;
begin
  //If player has elected to play on past victory or defeat
  //then do not check for any further goals
  if fHasWonOrLost then Exit;

  //Assume they will win/survive, then prove it with goals
  VictorySatisfied  := True;
  SurvivalSatisfied := True;

  with fPlayers[fPlayerIndex] do
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
  fHasWonOrLost := not SurvivalSatisfied or VictorySatisfied;

  //You can't win and lose at the same time. In KaM defeats override victories, except
  //when there are no goals defined, in which case you win for some weird reason...
  //But given that having no goals is pretty pointless we'll make defeat override so you can't
  //win battle missions by waiting for your troops to simultainiously starve to death.

  //Let everyone know in MP mode
  if not fGame.IsReplay
  and (fGame.IsMultiplayer or (MyPlayer = fPlayers[fPlayerIndex])) then
    if not SurvivalSatisfied then
      fGame.PlayerDefeat(fPlayerIndex)
    else
    if VictorySatisfied then
      fGame.PlayerVictory(fPlayerIndex);
end;


procedure TKMPlayerAI.CheckArmiesCount;
var
  Barracks:array of TKMHouseBarracks;
  HB:TKMHouseBarracks;
  GType: TGroupType;
  i,k:integer;
  UT: TUnitType;
  TrainedSomething, CanEquipIron, CanEquipLeather:boolean;
  GroupReq: array[TGroupType] of integer;
begin
  if fGame.IsPeaceTime then Exit; //Do not process train soldiers during peacetime
  if fPlayers[fPlayerIndex].Stats.GetArmyCount >= Setup.MaxSoldiers then Exit; //Don't train if we have reached our limit
  //Delay between equipping soldiers for KaM compatibility
  CanEquipIron := fGame.CheckTime(fLastEquippedTime+Setup.EquipRateIron);
  CanEquipLeather := fGame.CheckTime(fLastEquippedTime+Setup.EquipRateLeather);
  if not CanEquipIron and not CanEquipLeather then Exit;
  fLastEquippedTime := fGame.GameTickCount;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  for k:=0 to DefencePositions.Count - 1 do
    with DefencePositions[k] do
    if CurrentCommander = nil then
      inc(GroupReq[GroupType], DefencePositions.TroopFormations[GroupType].NumUnits)
    else
      inc(GroupReq[GroupType], DefencePositions.TroopFormations[GroupType].NumUnits - (TKMUnitWarrior(CurrentCommander).GetMemberCount+1));

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
      UT := AITroopTrainOrder[GType,i];
      if (UT <> ut_None)
      and ((CanEquipIron and (UT in WARRIORS_IRON)) or (CanEquipLeather and not (UT in WARRIORS_IRON))) then
        while HB.CanEquip(UT) and (GroupReq[GType] > 0) and
              (fPlayers[fPlayerIndex].Stats.GetArmyCount < Setup.MaxSoldiers) do
        begin
          HB.Equip(UT, 1);
          dec(GroupReq[GType]);
          TrainedSomething := true;
          if Setup.GetEquipRate(UT) > 0 then break; //Only equip 1 soldier when we have a restricted equip rate
        end;
      if TrainedSomething and (Setup.GetEquipRate(UT) > 0) then break; //Only equip 1 soldier when we have a restricted equip rate
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
    att_ClosestBuildingFromStartPos:  TargetHouse := fPlayers.GetClosestHouse(Setup.StartPosition, fPlayerIndex, at_Enemy, false);
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


procedure TKMPlayerAI.CheckArmy;
var i, k, j: integer;
    G: TGroupType;
    Positioned: boolean;
    NeedsLinkingTo: array[TGroupType] of TKMUnitWarrior;
begin
  for G := Low(TGroupType) to High(TGroupType) do
    NeedsLinkingTo[G] := nil;

  //Hotfix until we refactor AI: Make sure no defence position commander is dead or not a commander
  for k:=0 to DefencePositions.Count - 1 do
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
          if ArmyInFight or (UnitTask is TTaskAttackHouse) or (OrderTarget <> nil) then
          begin
            //If this group belongs to a defence position and they are too far away we should disassociate
            //them from the defence position so new warriors can take up the defence if needs be
            for k:=0 to DefencePositions.Count - 1 do
              with DefencePositions[k] do
                if (CurrentCommander = GetCommander) and (KMLength(Position.Loc, GetPosition) > Radius) then
                  CurrentCommander := nil;
            Continue;
          end;

          if fGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

          //Check formation. If the script has defined a group with more units per row than there should be, do not change it
          if UnitsPerRow < DefencePositions.TroopFormations[UnitGroups[UnitType]].UnitsPerRow then
            UnitsPerRow := DefencePositions.TroopFormations[UnitGroups[UnitType]].UnitsPerRow;
          //Position this group to defend if they already belong to a defence position
          Positioned := false;
          for k:=0 to DefencePositions.Count - 1 do
            if DefencePositions[k].CurrentCommander = GetCommander then
            begin
              //If they are not already walking and can reach their position, tell them to walk there
              if not (GetCommander.GetUnitAction is TUnitActionWalkTo)
              and GetCommander.CanWalkTo(DefencePositions[k].Position.Loc, 0) then
                OrderWalk(DefencePositions[k].Position);
              Positioned := true; //We already have a position, finished with this group

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
            Positioned := DefencePositions.FindPlaceForWarrior(TKMUnitWarrior(fPlayers[fPlayerIndex].Units.Items[i]), AI_LINK_IDLE, AI_FILL_CLOSEST);

          //Just chill and link with other idle groups
          if not Positioned then
          begin
            if AI_LINK_IDLE then
              //If this group doesn't have enough members
              if (GetMemberCount+1 < DefencePositions.TroopFormations[UnitGroups[UnitType]].NumUnits) then
                if NeedsLinkingTo[UnitGroups[UnitType]] = nil then
                  NeedsLinkingTo[UnitGroups[UnitType]] := GetCommander //Flag us as needing to be added to
                else
                begin
                  DefencePositions.RestockPositionWith(NeedsLinkingTo[UnitGroups[UnitType]],GetCommander);
                  if NeedsLinkingTo[UnitGroups[UnitType]].GetMemberCount+1 >= DefencePositions.TroopFormations[UnitGroups[UnitType]].NumUnits then
                    NeedsLinkingTo[UnitGroups[UnitType]] := nil; //Group is now full
                end;
          end;

        end;
      end;
  end;
end;


procedure TKMPlayerAI.CheckCanAttack;
var
  AttackTotalAvailable: integer; //Total number of warriors available to attack the enemy
  AttackGroupsCount: array[TGroupType] of integer;
  AttackGroups: array[TGroupType] of array of TKMUnitWarrior;

  procedure AddToAvailableToAttack(aCommander: TKMUnitWarrior);
  var GT: TGroupType;
  begin
    GT := UnitGroups[aCommander.UnitType];
    if Length(AttackGroups[GT]) <= AttackGroupsCount[GT] then
      SetLength(AttackGroups[GT],AttackGroupsCount[GT] + 10);
    AttackGroups[GT, AttackGroupsCount[GT]] := aCommander;
    Inc(AttackGroupsCount[GT]);
    AttackTotalAvailable := AttackTotalAvailable + aCommander.GetMemberCount + 1;
  end;

var
  I, K: Integer;
  G: TGroupType;
  W: TKMUnitWarrior;
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
  for I := 0 to fPlayers[fPlayerIndex].Units.Count - 1 do
  if TKMUnit(fPlayers[fPlayerIndex].Units.Items[I]) is TKMUnitWarrior then
  begin
    W := TKMUnitWarrior(fPlayers[fPlayerIndex].Units.Items[I]);
    if not W.IsDeadOrDying //Ignore warriors which are dead
    and W.Visible  //Ignore warriors which are still in barracks
    and W.IsCommander
    and not W.ArmyInFight
    and not (W.UnitTask is TTaskAttackHouse)
    and not (W.OrderTarget <> nil)
    and W.IsIdle then
    begin
      DP := DefencePositions.FindPositionOf(W);
      if (DP = nil) or (DP.DefenceType = adt_BackLine) then
        AddToAvailableToAttack(W);
    end;
  end;

  //Now process AI attacks (we have compiled a list of warriors available to attack)
  if not fGame.IsPeaceTime then
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
  fPlayerIndex := aPlayer;
  fMayor.OwnerUpdate(aPlayer);
end;


//This is run by commanders when they die.
//Dead commander is the one that died, NewCommander is the one that replaced him.
//We need to update CurrentCommander for defence positions in this case.
procedure TKMPlayerAI.CommanderDied(DeadCommander, NewCommander: TKMUnitWarrior);
begin
  DefencePositions.ReplaceCommander(DeadCommander, NewCommander);
end;


procedure TKMPlayerAI.WarriorEquipped(aWarrior: TKMUnitWarrior);
begin
  Assert(aWarrior.IsCommander); //A warrior walking out of the barracks should not be linked yet
  DefencePositions.FindPlaceForWarrior(aWarrior, True, AI_FILL_CLOSEST);
end;


procedure TKMPlayerAI.RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
var I: Integer;
begin
  if fPlayers[fPlayerIndex].PlayerType = pt_Human then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for i := 0 to DefencePositions.Count - 1 do
    with DefencePositions[i] do
      if (CurrentCommander <> nil) and not CurrentCommander.IsDeadOrDying
      and CurrentCommander.IsCommander and (not CurrentCommander.ArmyInFight)
      and (CurrentCommander.OrderTarget = nil)
      and (KMLength(CurrentCommander.GetPosition, aAttacker.GetPosition) <= Radius) then
        CurrentCommander.OrderAttackUnit(aAttacker);
end;


//aHouse is our house that was attacked
procedure TKMPlayerAI.HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
begin
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:
      begin
        //No fight alerts in replays, and only show alerts for ourselves
        if (not fGame.IsReplay) and (fPlayerIndex = MyPlayer.PlayerIndex) then
          fGame.Alerts.AddFight(KMPointF(aHouse.GetPosition), fPlayerIndex, an_Town);
      end;
    pt_Computer:
      RetaliateAgainstThreat(aAttacker);
  end;
end;


//aUnit is our unit that was attacked
procedure TKMPlayerAI.UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);
const
  NotifyKind: array [Boolean] of TAttackNotification = (an_Citizens, an_Troops);
begin
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:
      //No fight alerts in replays, and only show alerts for ourselves
      if (not fGame.IsReplay) and (fPlayerIndex = MyPlayer.PlayerIndex) then
        fGame.Alerts.AddFight(aUnit.PositionF, fPlayerIndex, NotifyKind[aUnit is TKMUnitWarrior]);
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
function TKMPlayerAI.HouseAutoRepair: Boolean;
begin
  Result := fSetup.AutoBuild;
end;


procedure TKMPlayerAI.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(fPlayerIndex);
  SaveStream.Write(fHasWonOrLost);
  SaveStream.Write(fLastEquippedTime);

  fSetup.Save(SaveStream);
  fAttacks.Save(SaveStream);
  DefencePositions.Save(SaveStream);
  fGeneral.Save(SaveStream);
  fMayor.Save(SaveStream);
end;


procedure TKMPlayerAI.Load(LoadStream:TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(fPlayerIndex);
  LoadStream.Read(fHasWonOrLost);
  LoadStream.Read(fLastEquippedTime);

  fSetup.Load(LoadStream);
  fAttacks.Load(LoadStream);
  DefencePositions.Load(LoadStream);
  fGeneral.Load(LoadStream);
  fMayor.Load(LoadStream);
end;


procedure TKMPlayerAI.SyncLoad;
begin
  DefencePositions.SyncLoad;
end;


procedure TKMPlayerAI.UpdateState;
begin
  //Check if player has been defeated for Events
  CheckDefeated;

  //Check goals for all players to maintain multiplayer consistency
  //AI does not care if it won or lost and Human dont need Mayor and Army management
  case fPlayers[fPlayerIndex].PlayerType of
    pt_Human:     CheckGoals; //This procedure manages victory and loss
    pt_Computer:  begin
                    fMayor.UpdateState;

                    CheckArmy; //Feed army, position defence, arrange/organise groups
                    CheckCanAttack;
                    CheckArmiesCount; //Train new soldiers if needed

                    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
                    //CheckAndIssueAttack; //Attack enemy
                    //Anything Else?
                  end;
  end;
end;


end.
