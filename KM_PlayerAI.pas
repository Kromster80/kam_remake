unit KM_PlayerAI;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KromUtils, KM_Player, KM_Utils, KM_Units_Warrior;

type
  TAIDefencePosType = (adt_FrontLine, //Top priority to defend, will be replaced by back line troops when they die, and troops will not go on attacks as they are defending an important position
                       adt_BackLine); //Lower priority defence, can go on AI attacks (these are often placed behind the main defence line as replacement/attacking troops)

  //@Lewin: Your loading bug is here. CurrentCommander pointer gets trashed on load
  //I suggest you make it a new class TAIDefencePositions with proper
  //Create/Save/SyncLoad/Load procedures
  //
  TAIDefencePosition = record
                         Position: TKMPointDir; //Position and direction the group defending will stand
                         GroupType: TGroupType; //Type of group to defend this position (e.g. melee)
                         DefenceRadius: integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
                         DefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
                         CurrentCommander: TKMUnitWarrior; //Commander of group currently occupying position
                       end;

type
  TKMPlayerAI = class
  private
    Assets:TKMPlayerAssets; //This is just alias for Players assets
  public
    ReqWorkers, ReqSerfFactor, ReqRecruits: word; //Nunber of each unit type required
    RecruitTrainTimeout: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    TownDefence, MaxSoldiers, Aggressiveness: integer; //-1 means not use or default
    StartPosition: TKMPoint; //Defines roughly where to defend and build around
    Autobuild:boolean;
    TroopFormations: array[TGroupType] of record //Defines how defending troops will be formatted. 0 means leave unchanged.
                                            NumUnits, NumRows:integer;
                                          end;
    DefencePositionsCount: integer;
    DefencePositions: array of TAIDefencePosition;
    ScriptedAttacksCount: integer;
    ScriptedAttacks: array of TAIAttack;
    constructor Create(aAssets:TKMPlayerAssets);
    procedure CheckGoals;
    procedure CheckUnitCount();
    procedure CheckArmiesCount();
    procedure CheckArmy();
  public
    function GetHouseRepair:boolean; //Do we automatically repair all houses?
    procedure AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
    procedure AddAttack(aAttack: TAIAttack);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
    procedure UpdateState;
  end;

implementation
uses KM_Houses, KM_Units, KM_Game, KM_PlayersCollection, KM_Settings, KM_LoadLib;

constructor TKMPlayerAI.Create(aAssets:TKMPlayerAssets);
var i: TGroupType;
begin
  Inherited Create;
  Assets := aAssets;
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
    TroopFormations[i].NumUnits := 12;
    TroopFormations[i].NumRows := 4;
  end;
end;


procedure TKMPlayerAI.CheckGoals;

  function GoalConditionSatisfied(aGoal: TPlayerGoal):boolean;
  var MS: TMissionSettings;
  begin
    Result := false;

    if aGoal.Player <> play_None then
      MS := fPlayers.Player[byte(aGoal.Player)].fMissionSettings
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
        fGame.fGameplayInterface.MessageIssue(msgText,fTextLibrary.GetTextString(fGoals[i].MessageToShow),KMPoint(0,0));
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
procedure TKMPlayerAI.CheckUnitCount();
var
  i,k:integer;
  UnitType:TUnitType;
  HS:TKMHouseSchool;
  UnitReq:array[1..HOUSE_COUNT]of integer; //There are only ~10 unit types, but using HOUSE_COUNT is easier
  Schools:array of TKMHouseSchool;

  function CheckUnitRequirements(Req:integer; aUnitType:TUnitType):boolean;
  begin
    if Assets.GetUnitQty(aUnitType) < (Req+UnitReq[integer(aUnitType)]) then
    begin
      dec(UnitReq[integer(aUnitType)]); //So other schools don't order same unit
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
  for i:=1 to HOUSE_COUNT do begin //Count overall unit requirement
    if THouseType(i)<>ht_Barracks then //Exclude Barracks
    if HouseDAT[i].OwnerType<>-1 then //Exclude houses without owners
    inc(UnitReq[HouseDAT[i].OwnerType+1], Assets.GetHouseQty(THouseType(i)));
  end;

  SetLength(Schools,Assets.GetHouseQty(ht_School));
  k := 1;
  HS := TKMHouseSchool(Assets.FindHouse(ht_School,k));
  while HS <> nil do
  begin
    Schools[k-1] := HS;
    if HS.UnitQueue[1]<>ut_None then
      dec(UnitReq[integer(HS.UnitQueue[1])]); //Decrease requirement for each unit in training
    inc(k);
    HS := TKMHouseSchool(Assets.FindHouse(ht_School,k));
  end;

  for k:=1 to Length(Schools) do
  begin
    HS := Schools[k-1];
    if (HS<>nil)and(HS.UnitQueue[1]=ut_None) then
    begin
      for i:=1 to length(UnitReq) do
        if UnitReq[i] > Assets.GetUnitQty(TUnitType(i)) then
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
        if not CheckUnitRequirements(Round((10/ReqSerfFactor)*Assets.GetHouseQty(ht_None)), ut_Serf) then
          if not CheckUnitRequirements(ReqWorkers, ut_Worker) then
            if fGame.CheckTime(RecruitTrainTimeout) then //Recruits can only be trained after this time
              CheckUnitRequirements(ReqRecruits, ut_Recruit);
    end;
  end;
end;


procedure TKMPlayerAI.CheckArmiesCount();
var
  Barracks:array of TKMHouseBarracks;
  HB:TKMHouseBarracks;
  GType: TGroupType;
  i,k:integer;
  GroupReq: array[TGroupType] of integer;
begin
  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq,SizeOf(GroupReq),#0); //Clear up
  for k:=0 to DefencePositionsCount-1 do
    with DefencePositions[k] do
    if CurrentCommander = nil then
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits)
    else
      inc(GroupReq[GroupType], TroopFormations[GroupType].NumUnits - (TKMUnitWarrior(CurrentCommander).GetMemberCount+1));

  //Find barracks
  SetLength(Barracks,Assets.GetHouseQty(ht_Barracks));
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
    for GType:=gt_Melee to gt_Mounted do
      for i:=1 to 3 do
        if AITroopTrainOrder[GType,i] <> ut_None then
        while HB.CanEquip(AITroopTrainOrder[GType,i]) and (GroupReq[GType] > 0) do
        begin
          HB.Equip(AITroopTrainOrder[GType,i]);
          dec(GroupReq[GType]);
        end;
  end;
end;


procedure TKMPlayerAI.CheckArmy();

  procedure RestockPositionWith(aDefenceGroup, aCommander:TKMUnitWarrior);
  var Needed: integer;
  begin
    Needed := TroopFormations[UnitGroups[byte(aDefenceGroup.UnitType)]].NumUnits - (aDefenceGroup.GetMemberCount+1);
    if aCommander.GetMemberCount+1 <= Needed then
      aCommander.LinkTo(aDefenceGroup) //Link entire group
    else
      aCommander.SplitLinkTo(aDefenceGroup,Needed); //Link only as many units as are needed
  end;

var i, k, Matched: integer;
    Distance, Best: single;
    Positioned: boolean;
    NeedsLinkingTo: array[TGroupType] of TKMUnitWarrior;
begin
  //Cleanup when commander of a defence position dies
  for i:=0 to DefencePositionsCount-1 do
    if (DefencePositions[i].CurrentCommander <> nil) and DefencePositions[i].CurrentCommander.IsDeadOrDying then
      if DefencePositions[i].CurrentCommander.fCommander <> nil then
        DefencePositions[i].CurrentCommander := TKMUnitWarrior(DefencePositions[i].CurrentCommander.fCommander.GetUnitPointer)
      else
      begin
        DefencePositions[i].CurrentCommander.ReleaseUnitPointer;
        DefencePositions[i].CurrentCommander := nil;
      end;

  for i:=byte(low(TGroupType)) to byte(high(TGroupType)) do
    NeedsLinkingTo[TGroupType(i)] := nil;

  //Iterate units list in search of warrior commanders, and then check the following: Hunger, (feed) formation, (units per row) position (from defence positions)
  for i:=0 to Assets.GetUnits.Count-1 do
  begin
    if TKMUnit(Assets.GetUnits.Items[i]) is TKMUnitWarrior then
      with TKMUnitWarrior(Assets.GetUnits.Items[i]) do
        if (fCommander = nil) and not IsDeadOrDying then
        begin
          //Check hunger and feed
          if (GetCondition < UNIT_MIN_CONDITION) then
            OrderFood;

          //Check formation
          if UnitGroups[byte(UnitType)] <> gt_None then
            if TroopFormations[UnitGroups[byte(UnitType)]].NumRows > 1 then
              UnitsPerRow := TroopFormations[UnitGroups[byte(UnitType)]].NumRows;
          
          //todo: only do this if the unit is not attacking/in combat
          //Position this group to defend if they already belong to a defence position
          //todo: Restock defence positions listed first with ones listed later (in script order = priority)
          Positioned := false;
          for k:=0 to DefencePositionsCount-1 do
            if DefencePositions[k].CurrentCommander = GetCommander then
            begin
              //todo: Note: KaM does NOT split groups that are too full (as this can only occur if the person who made the mission designed it that way)
              //      Perhaps we shouldn't either?
              if GetMemberCount+1 > TroopFormations[DefencePositions[k].GroupType].NumUnits then
                Split; //If there are too many troops, split group in half and the right number will automatically rejoin us
              PlaceOrder(wo_Walk,DefencePositions[k].Position);
              Positioned := true; //We already have a position, finished with this group
              break;
            end;

          //Look for group that needs additional members, or a new position to defend
          Matched := -1;  Best := 9999;
          if not Positioned then
            for k:=0 to DefencePositionsCount-1 do
              if (DefencePositions[k].GroupType = UnitGroups[byte(UnitType)]) and (((DefencePositions[k].CurrentCommander <> nil)
              and (DefencePositions[k].CurrentCommander.GetMemberCount+1 < TroopFormations[DefencePositions[k].GroupType].NumUnits))
              or  (DefencePositions[k].CurrentCommander = nil)) then
              begin
                //Front line always higher priority than backline
                Distance := (byte(DefencePositions[k].DefenceType = adt_BackLine)*1000) + GetLength(GetPosition,DefencePositions[k].Position.Loc);
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
              DefencePositions[Matched].CurrentCommander := TKMUnitWarrior(GetCommander.GetUnitPointer);
              PlaceOrder(wo_Walk,DefencePositions[Matched].Position);
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
  DefencePositions[DefencePositionsCount].Position := aPos;
  DefencePositions[DefencePositionsCount].GroupType := aGroupType;
  DefencePositions[DefencePositionsCount].DefenceRadius := aDefenceRadius;
  DefencePositions[DefencePositionsCount].DefenceType := aDefenceType;
  DefencePositions[DefencePositionsCount].CurrentCommander := nil; //Unoccupied
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
    SaveStream.Write(DefencePositions[i], SizeOf(DefencePositions[i]));
  SaveStream.Write(ScriptedAttacksCount);
  for i:=0 to ScriptedAttacksCount-1 do
    SaveStream.Write(ScriptedAttacks[i], SizeOf(ScriptedAttacks[i]));
end;


procedure TKMPlayerAI.Load(LoadStream:TKMemoryStream);
var i: integer;
begin
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
    LoadStream.Read(DefencePositions[i], SizeOf(DefencePositions[i]));
  LoadStream.Read(ScriptedAttacksCount);
  SetLength(ScriptedAttacks, ScriptedAttacksCount);
  for i:=0 to ScriptedAttacksCount-1 do
    LoadStream.Read(ScriptedAttacks[i], SizeOf(ScriptedAttacks[i]));
end;


//So far this whole procedure is a placeholder
procedure TKMPlayerAI.SyncLoad();
begin
  //
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

    //todo: Should not run every update state, it takes too long and doesn't need to anyway
    CheckArmy; //Feed army, position defence, arrange/organise groups
    CheckArmiesCount; //Train new soldiers if needed
    //CheckHouseCount; //Build new houses if needed
    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
    //CheckAndIssueAttack; //Attack enemy
    //Anything Else?
  end;
end;

end.
