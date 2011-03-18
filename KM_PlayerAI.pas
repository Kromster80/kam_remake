unit KM_PlayerAI;
{$I KaM_Remake.inc}
interface
uses Classes, Math, KromUtils,
    KM_CommonTypes, KM_Defaults, KM_Houses, KM_Player, KM_Units, KM_Units_Warrior, KM_Utils;

type
  TAIDefencePosType = (adt_FrontLine, //Front line troops may not go on attacks, they are for defence
                       adt_BackLine); //Back line troops may attack

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

type
  TKMPlayerAI = class
  private
    Assets:TKMPlayerAssets; //This is just alias for Players assets
    fTimeOfLastAttackMessage: cardinal;

    procedure CheckGoals;
    procedure CheckUnitCount;
    procedure CheckArmiesCount;
    procedure CheckArmy;
  public
    ReqWorkers, ReqSerfFactor, ReqRecruits: word; //Number of each unit type required
    RecruitTrainTimeout: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    TownDefence, MaxSoldiers, Aggressiveness: integer; //-1 means not used or default
    StartPosition: TKMPoint; //Defines roughly where to defend and build around
    Autobuild:boolean;
    TroopFormations: array[TGroupType] of record //Defines how defending troops will be formatted. 0 means leave unchanged.
                                            NumUnits, UnitsPerRow:integer;
                                          end;
    DefencePositionsCount: integer;
    DefencePositions: array of TAIDefencePosition;
    ScriptedAttacksCount: integer;
    ScriptedAttacks: array of TAIAttack;
    constructor Create(aAssets:TKMPlayerAssets);
    destructor Destroy; override;

    procedure CommanderDied(DeadCommander, NewCommander: TKMUnitWarrior);
    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker:TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker:TKMUnitWarrior);

    function GetHouseRepair:boolean; //Do we automatically repair all houses?
    procedure AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
    procedure AddAttack(aAttack: TAIAttack);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;

implementation
uses KM_Game, KM_PlayersCollection, KM_TextLibrary, KM_PlayerStats, KM_Sound, KM_Viewport;

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
  fPlayers.CleanUpUnitPointer(fCurrentCommander);
end;


procedure TAIDefencePosition.SetCurrentCommander(aCommander: TKMUnitWarrior);
begin
  ClearCurrentCommander;
  if aCommander <> nil then
    fCurrentCommander := TKMUnitWarrior(aCommander.GetUnitPointer);
end;


procedure TAIDefencePosition.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(Position, SizeOf(Position));
  SaveStream.Write(GroupType, SizeOf(GroupType));
  SaveStream.Write(DefenceRadius);
  SaveStream.Write(DefenceType, SizeOf(DefenceType));
  if fCurrentCommander <> nil then
    SaveStream.Write(fCurrentCommander.ID) //Store ID
  else
    SaveStream.Write(Zero);
end;


constructor TAIDefencePosition.Load(LoadStream:TKMemoryStream);
begin
  Inherited Create;
  LoadStream.Read(Position, SizeOf(Position));
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


constructor TKMPlayerAI.Create(aAssets:TKMPlayerAssets);
var i: TGroupType;
begin
  Inherited Create;
  Assets := aAssets;
  fTimeOfLastAttackMessage := 0;
  DefencePositionsCount := 0;
  ScriptedAttacksCount := 0;
  //Set some defaults (these are not measured from KaM)
  ReqWorkers := 3;
  ReqRecruits := 5; //This means the number in the barracks, watchtowers are counted seperately
  ReqSerfFactor := 10; //Means 1 serf per building
  RecruitTrainTimeout := 0; //Can train at start
  Autobuild := true; //In KaM it is on by default, and most missions turn it off
  StartPosition := KMPoint(0,0);
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
  for i:=0 to DefencePositionsCount-1 do DefencePositions[i].Free;
  Inherited;
end;


procedure TKMPlayerAI.CheckGoals;

  function GoalConditionSatisfied(aGoal: TPlayerGoal):boolean;
  var MS: TKMPlayerStats;
  begin
    Result := false;

    if aGoal.Player <> play_none then
      MS := fPlayers.Player[byte(aGoal.Player)].Stats
    else
      MS := nil; //Will trigger an error unless it's not gc_Time

    case aGoal.GoalCondition of //todo: add all goal condition checks properly and confirm unknowns with tests in KaM
      gc_BuildTutorial:     Result := MS.GetHouseQty(ht_Tannery)>0;
      gc_Time:              Result := fGame.CheckTime(aGoal.GoalTime);
      gc_Buildings:         Result := (MS.GetHouseQty(ht_Store)>0)or(MS.GetHouseQty(ht_School)>0)or(MS.GetHouseQty(ht_Barracks)>0);
      gc_Troops:            Result := (MS.GetArmyCount>0);
      gc_MilitaryAssets:    Result := (MS.GetArmyCount>0);
      gc_SerfsAndSchools:   Result := (MS.GetHouseQty(ht_School)>0)or(MS.GetUnitQty(ut_Serf)>0);
      gc_EconomyBuildings:  Result := ((MS.GetHouseQty(ht_Store)>0)or(MS.GetHouseQty(ht_School)>0)or(MS.GetHouseQty(ht_Inn)>0));
      else                  Assert(false, 'Unknown goal');
    end;
    if aGoal.GoalStatus = gs_False then
      Result := not Result; //Reverse condition
  end;

var i: integer; VictorySatisfied, SurvivalSatisfied: boolean;
begin
  if not CHECK_WIN_CONDITIONS then exit; //Debug switch

  if fGame.GameState = gsReplay then exit; //Don't check conditions in Replay
  if fGame.PlayOnState <> gr_Cancel then exit; //If player has elected to play on past victory or defeat then do not check for any further goals
  
  VictorySatisfied  := true; //Assume they will win/survive, then prove it with goals
  SurvivalSatisfied := true;

  with Assets do
  for i:=0 to fGoalCount-1 do //Test each goal to see if it has occured
    if GoalConditionSatisfied(fGoals[i]) then
    begin
      //Display message if set and not already shown and not a blank text
      if (fGoals[i].MessageToShow <> 0) and (not fGoals[i].MessageHasShown) and (fTextLibrary.GetTextString(fGoals[i].MessageToShow) <> '') then
      begin
        fGame.fGamePlayInterface.MessageIssue(msgText,fTextLibrary.GetTextString(fGoals[i].MessageToShow),KMPoint(0,0));
        fGoals[i].MessageHasShown := true;
      end;
    end
    else
    begin
      if fGoals[i].GoalType = glt_Victory then
        VictorySatisfied := false;
      if fGoals[i].GoalType = glt_Survive then
        SurvivalSatisfied := false;
    end;

  if VictorySatisfied then
    fGame.GameHold(true, gr_Win); //They win

  if not SurvivalSatisfied then
    fGame.GameHold(true, gr_Defeat); //They lose
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMPlayerAI.CheckUnitCount;
var
  i,k:integer;
  UnitType:TUnitType;
  HS:TKMHouseSchool;
  UnitReq:array[1..HOUSE_COUNT]of integer; //There are only ~10 unit types, but using HOUSE_COUNT is easier
  Schools:array of TKMHouseSchool;

  function CheckUnitRequirements(Req:integer; aUnitType:TUnitType):boolean;
  begin
    //We summ up requirements for e.g. Recruits required at Towers and Barracks
    if Assets.Stats.GetUnitQty(aUnitType) < (Req+UnitReq[byte(aUnitType)]) then
    begin
      dec(UnitReq[byte(aUnitType)]); //So other schools don't order same unit
      HS.AddUnitToQueue(aUnitType);
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
  for i:=1 to HOUSE_COUNT do
    if (HouseDAT[i].OwnerType<>-1) and (THouseType(i)<>ht_Barracks) then
      inc(UnitReq[HouseDAT[i].OwnerType+1], Assets.Stats.GetHouseQty(THouseType(i)));

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools,Assets.Stats.GetHouseQty(ht_School));
  k := 1;
  HS := TKMHouseSchool(Assets.FindHouse(ht_School,k));
  while HS <> nil do
  begin
    Schools[k-1] := HS;
    for i:=1 to 6 do //Decrease requirement for each unit in training
      if HS.UnitQueue[i]<>ut_None then
        dec(UnitReq[byte(HS.UnitQueue[i])]); //Can be negative and compensated by e.g. ReqRecruits
    inc(k);
    HS := TKMHouseSchool(Assets.FindHouse(ht_School,k));
  end;

  //Order the training
  for k:=1 to Length(Schools) do
  begin
    HS := Schools[k-1];
    if (HS<>nil)and(HS.UnitQueue[1]=ut_None) then
    begin
      //Order citizen training
      for i:=1 to length(UnitReq) do
        if UnitReq[i] > Assets.Stats.GetUnitQty(TUnitType(i)) then
        begin
          UnitType := TUnitType(i);
          if UnitType <> ut_None then
          begin
            dec(UnitReq[i]); //So other schools don't order same unit
            HS.AddUnitToQueue(UnitType);
            break; //Don't need more UnitTypes yet
          end;
        end;

      //If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      //Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)
      if (HS.UnitQueue[1] = ut_None) then //Still haven't found a match...
        if not CheckUnitRequirements(Round((10/ReqSerfFactor)*Assets.Stats.GetHouseQty(ht_None)), ut_Serf) then
          if not CheckUnitRequirements(ReqWorkers, ut_Worker) then
            if fGame.CheckTime(RecruitTrainTimeout) then //Recruits can only be trained after this time
              if not CheckUnitRequirements(ReqRecruits * Assets.Stats.GetHouseQty(ht_Barracks), ut_Recruit) then
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
  GroupReq: array[TGroupType] of integer;
begin
  if Assets.Stats.GetArmyCount >= MaxSoldiers then exit; //Don't train if we have reached our limit

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq,SizeOf(GroupReq),#0); //Clear up
  for k:=0 to DefencePositionsCount-1 do
    with DefencePositions[k] do
    if CurrentCommander = nil then
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits)
    else
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits - (TKMUnitWarrior(CurrentCommander).GetMemberCount+1));

  //Find barracks
  SetLength(Barracks, Assets.Stats.GetHouseQty(ht_Barracks));
  k := 1;
  HB := TKMHouseBarracks(Assets.FindHouse(ht_Barracks,k));
  while HB <> nil do
  begin
    Barracks[k-1] := HB;
    inc(k);
    HB := TKMHouseBarracks(Assets.FindHouse(ht_Barracks,k));
  end;

  //Train troops where possible in each barracks
  for k:=1 to Length(Barracks) do
  begin  
    HB := Barracks[k-1];
    //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
    i := 0;
    repeat
      GType := TGroupType(Random(4)+1); //Pick random from overall count
      inc(i);
    until (GroupReq[GType] > 0) or (i > 9); //Limit number of attempts to guarantee it doesn't loop forever

    for i:=1 to 3 do
      if AITroopTrainOrder[GType,i] <> ut_None then
      while HB.CanEquip(AITroopTrainOrder[GType,i]) and (GroupReq[GType] > 0) and
            (Assets.Stats.GetArmyCount < MaxSoldiers) do
      begin
        HB.Equip(AITroopTrainOrder[GType,i]);
        dec(GroupReq[GType]);
      end;
  end;
end;


procedure TKMPlayerAI.CheckArmy;

  procedure RestockPositionWith(aDefenceGroup, aCommander:TKMUnitWarrior);
  var Needed: integer;
  begin
    Needed := TroopFormations[UnitGroups[byte(aDefenceGroup.UnitType)]].NumUnits - (aDefenceGroup.GetMemberCount+1);
    if aCommander.GetMemberCount+1 <= Needed then
      aCommander.OrderLinkTo(aDefenceGroup) //Link entire group
    else
      aCommander.OrderSplitLinkTo(aDefenceGroup,Needed); //Link only as many units as are needed
  end;

var i, k, j, Matched: integer;
    Distance, Best: single;
    Positioned: boolean;
    NeedsLinkingTo: array[TGroupType] of TKMUnitWarrior;
begin
  for i:=byte(low(TGroupType)) to byte(high(TGroupType)) do
    NeedsLinkingTo[TGroupType(i)] := nil;

  //Iterate units list in search of warrior commanders, and then check the following: Hunger, (feed) formation, (units per row) position (from defence positions)
  for i:=0 to Assets.Units.Count-1 do
  begin
    if TKMUnit(Assets.Units.Items[i]) is TKMUnitWarrior then
      with TKMUnitWarrior(Assets.Units.Items[i]) do
        if (fCommander = nil) and not IsDeadOrDying then
        begin
          //Check hunger and feed
          if (Condition < UNIT_MIN_CONDITION) then
            OrderFood;

          //Check formation. If the script has defined a group with more units per row than there should be, do not change it
          if UnitGroups[byte(UnitType)] <> gt_None then
            if UnitsPerRow < TroopFormations[UnitGroups[byte(UnitType)]].UnitsPerRow then
              UnitsPerRow := TroopFormations[UnitGroups[byte(UnitType)]].UnitsPerRow;

          if ArmyIsBusy then exit;
          //Position this group to defend if they already belong to a defence position
          Positioned := false;
          for k:=0 to DefencePositionsCount-1 do
            if DefencePositions[k].CurrentCommander = GetCommander then
            begin
              OrderWalk(DefencePositions[k].Position);
              Positioned := true; //We already have a position, finished with this group

              //If we are a less important position and there is a higher priority position not full we must step up
              for j:=0 to DefencePositionsCount-1 do
                if (DefencePositions[j].GroupType = UnitGroups[byte(UnitType)]) and
                   (j < k) and //Positions defined first are top priority, so keep them stocked
                   not DefencePositions[j].IsFullyStocked(TroopFormations[DefencePositions[j].GroupType].NumUnits) then
                   begin
                     DefencePositions[k].CurrentCommander := nil; //Leave current position
                     if DefencePositions[j].CurrentCommander <> nil then
                       RestockPositionWith(DefencePositions[j].CurrentCommander,GetCommander) //Restock it
                     else
                       DefencePositions[j].CurrentCommander := GetCommander; //Take new position
                     break;
                   end;

              break;
            end;

          //Look for group that needs additional members, or a new position to defend
          //In this case we choose the closest group, then move to a higher priority one later (see above)
          //This means at the start of the mission troops will take the position they are placed at rather than swapping around
          Matched := -1;  Best := 9999;
          if not Positioned then
            for k:=0 to DefencePositionsCount-1 do
              if (DefencePositions[k].GroupType = UnitGroups[byte(UnitType)]) and
                 not DefencePositions[k].IsFullyStocked(TroopFormations[DefencePositions[k].GroupType].NumUnits) then
              begin
                //Take closest position that is empty or requries restocking
                Distance := GetLength(GetPosition,DefencePositions[k].Position.Loc);
                if Distance < Best then
                begin
                  Matched := k;
                  Best := Distance;
                end;
              end;
          if Matched <> -1 then
          begin
            Positioned := true;
            if DefencePositions[Matched].CurrentCommander = nil then
            begin //New position
              DefencePositions[Matched].CurrentCommander := GetCommander;
              OrderWalk(DefencePositions[Matched].Position);
            end
            else //Restock existing position
              RestockPositionWith(DefencePositions[Matched].CurrentCommander,GetCommander);
          end;

          //Just chill and link with other idle groups
          if not Positioned then
            //If this group doesn't have enough members
            if (GetMemberCount+1 < TroopFormations[UnitGroups[byte(UnitType)]].NumUnits) then
              if NeedsLinkingTo[UnitGroups[byte(UnitType)]] = nil then
                NeedsLinkingTo[UnitGroups[byte(UnitType)]] := GetCommander //Flag us as needing to be added to
              else
              begin
                RestockPositionWith(NeedsLinkingTo[UnitGroups[byte(UnitType)]],GetCommander);
                if NeedsLinkingTo[UnitGroups[byte(UnitType)]].GetMemberCount+1 >= TroopFormations[UnitGroups[byte(UnitType)]].NumUnits then
                  NeedsLinkingTo[UnitGroups[byte(UnitType)]] := nil; //Group is now full
              end;

        end;
  end;
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


procedure TKMPlayerAI.HouseAttackNotification(aHouse: TKMHouse; aAttacker:TKMUnitWarrior);
begin
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
  begin
    if fGame.CheckTime(fTimeOfLastAttackMessage + TIME_ATTACK_WARNINGS)
    and (GetLength(fViewport.GetCenter, KMPointF(aHouse.GetPosition)) >= DISTANCE_FOR_WARNINGS) then
    begin
      //fSoundLib.PlayWarning(sp_BuildingsAttacked);
      fTimeOfLastAttackMessage := fGame.GetTickCount;
    end;
  end;
  if Assets.PlayerType = pt_Computer then
  begin
    //todo: Defend
  end;
end;


procedure TKMPlayerAI.UnitAttackNotification(aUnit: TKMUnit; aAttacker:TKMUnitWarrior);
begin
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
  begin
    if fGame.CheckTime(fTimeOfLastAttackMessage + TIME_ATTACK_WARNINGS)
    and (GetLength(fViewport.GetCenter, KMPointF(aUnit.GetPosition)) >= DISTANCE_FOR_WARNINGS) then
    begin
      //fSoundLib.PlayWarning(sp_UnitsAttacked);
      fTimeOfLastAttackMessage := fGame.GetTickCount;
    end;
  end;
  if Assets.PlayerType = pt_Computer then
  begin
    //todo: Defend
  end;
end;


//Do we automatically repair all houses?
//For now use Autobuild, which is what KaM does. Later we can add a script command to turn this on and off
//Also could be changed later to disable repairing when under attack? (only repair if the enemy goes away?)
function TKMPlayerAI.GetHouseRepair:boolean;
begin
  Result := Autobuild;
end;


procedure TKMPlayerAI.AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
begin
  setlength(DefencePositions,DefencePositionsCount+1);
  DefencePositions[DefencePositionsCount] := TAIDefencePosition.Create(aPos,aGroupType,aDefenceRadius,aDefenceType);
  inc(DefencePositionsCount);
end;


procedure TKMPlayerAI.AddAttack(aAttack: TAIAttack);
begin
  setlength(ScriptedAttacks,ScriptedAttacksCount+1);
  ScriptedAttacks[ScriptedAttacksCount] := aAttack;
  inc(ScriptedAttacksCount);
end;


procedure TKMPlayerAI.Save(SaveStream:TKMemoryStream);
var i: integer;
begin
  SaveStream.Write(fTimeOfLastAttackMessage);
  SaveStream.Write(ReqWorkers);
  SaveStream.Write(ReqSerfFactor);
  SaveStream.Write(ReqRecruits);
  SaveStream.Write(RecruitTrainTimeout);
  SaveStream.Write(TownDefence);
  SaveStream.Write(MaxSoldiers);
  SaveStream.Write(Aggressiveness);
  SaveStream.Write(StartPosition);
  SaveStream.Write(Autobuild);
  SaveStream.Write(TroopFormations,SizeOf(TroopFormations));
  SaveStream.Write(DefencePositionsCount);
  for i:=0 to DefencePositionsCount-1 do
    DefencePositions[i].Save(SaveStream);
  SaveStream.Write(ScriptedAttacksCount);
  for i:=0 to ScriptedAttacksCount-1 do
    SaveStream.Write(ScriptedAttacks[i], SizeOf(ScriptedAttacks[i]));
end;


procedure TKMPlayerAI.Load(LoadStream:TKMemoryStream);
var i: integer;
begin
  LoadStream.Read(fTimeOfLastAttackMessage);
  LoadStream.Read(ReqWorkers);
  LoadStream.Read(ReqSerfFactor);
  LoadStream.Read(ReqRecruits);
  LoadStream.Read(RecruitTrainTimeout);
  LoadStream.Read(TownDefence);
  LoadStream.Read(MaxSoldiers);
  LoadStream.Read(Aggressiveness);
  LoadStream.Read(StartPosition);
  LoadStream.Read(Autobuild);
  LoadStream.Read(TroopFormations,SizeOf(TroopFormations));
  LoadStream.Read(DefencePositionsCount);
  SetLength(DefencePositions, DefencePositionsCount);
  for i:=0 to DefencePositionsCount-1 do
    DefencePositions[i] := TAIDefencePosition.Load(LoadStream);
  LoadStream.Read(ScriptedAttacksCount);
  SetLength(ScriptedAttacks, ScriptedAttacksCount);
  for i:=0 to ScriptedAttacksCount-1 do
    LoadStream.Read(ScriptedAttacks[i], SizeOf(ScriptedAttacks[i]));
end;


//So far this whole procedure is a placeholder
procedure TKMPlayerAI.SyncLoad;
var i: integer;
begin
  for i:=0 to DefencePositionsCount-1 do
    DefencePositions[i].SyncLoad;
end;


procedure TKMPlayerAI.UpdateState;
begin
  //Check goals only for MyPlayer
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
  begin
    CheckGoals; //This procedure manages victory, loss and messages all in one
  end
  else
  if Assets.PlayerType=pt_Computer then
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

end.
