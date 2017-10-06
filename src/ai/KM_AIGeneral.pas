unit KM_AIGeneral;
{$I KaM_Remake.inc}
interface
uses
  KM_AISetup, KM_AIAttacks, KM_AIDefensePos,
  KM_Units, KM_UnitGroups,
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  TKMGeneral = class
  private
    fLastEquippedTimeIron, fLastEquippedTimeLeather: Cardinal;
    fOwner: TKMHandIndex;
    fSetup: TKMHandAISetup;
    fAttacks: TAIAttacks;
    fDefencePositions: TAIDefencePositions;

    procedure CheckArmy;
    procedure CheckArmyCount;
    procedure CheckAttacks;
    procedure CheckAutoAttack;
    procedure CheckAutoDefend;
    procedure OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
  public
    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    property Attacks: TAIAttacks read fAttacks;
    property DefencePositions: TAIDefencePositions read fDefencePositions;
    procedure RetaliateAgainstThreat(aAttacker: TKMUnit);
    procedure WarriorEquipped(aGroup: TKMUnitGroup);

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses
  Classes, Math,
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_Houses, KM_HouseBarracks,
  KM_ResHouses, KM_NavMesh, KM_CommonUtils;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = False;
  AI_LINK_IDLE = False;


{ TKMGeneral }
constructor TKMGeneral.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fAttacks := TAIAttacks.Create;
  fDefencePositions := TAIDefencePositions.Create;
end;


destructor TKMGeneral.Destroy;
begin
  fDefencePositions.Free;
  fAttacks.Free;

  inherited;
end;


procedure TKMGeneral.AfterMissionInit;
begin

end;


procedure TKMGeneral.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fLastEquippedTimeIron);
  SaveStream.Write(fLastEquippedTimeLeather);
  fAttacks.Save(SaveStream);
  fDefencePositions.Save(SaveStream);
end;


procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fLastEquippedTimeIron);
  LoadStream.Read(fLastEquippedTimeLeather);
  fAttacks.Load(LoadStream);
  fDefencePositions.Load(LoadStream);
end;


procedure TKMGeneral.SyncLoad;
begin
  fDefencePositions.SyncLoad;
end;


procedure TKMGeneral.CheckArmyCount;

  function CanEquipIron: Boolean;
  begin
    Result := fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeIron + fSetup.EquipRateIron);
  end;

  function CanEquipLeather: Boolean;
  begin
    Result := fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeLeather + fSetup.EquipRateLeather);
  end;

var
  Barracks: array of TKMHouseBarracks;
  HB: TKMHouseBarracks;
  GT: TGroupType;
  I,K: Integer;
  UT: TUnitType;
  GroupReq: TGroupTypeArray;
begin
  if gGame.IsPeaceTime then Exit; //Do not train soldiers during peacetime

  //Don't train if we have reached our limit
  if (fSetup.MaxSoldiers <> -1) and (gHands[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers) then
    Exit;

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
  SetLength(Barracks, gHands[fOwner].Stats.GetHouseQty(ht_Barracks));
  I := 0;
  HB := TKMHouseBarracks(gHands[fOwner].FindHouse(ht_Barracks, I+1));
  while HB <> nil do
  begin
    Barracks[I] := HB;
    Inc(I);
    HB := TKMHouseBarracks(gHands[fOwner].FindHouse(ht_Barracks, I+1));
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

    if GroupReq[GT] = 0 then
      Break; // Don't train

    for K := Low(AITroopTrainOrder[GT]) to High(AITroopTrainOrder[GT]) do
    begin
      UT := AITroopTrainOrder[GT, K];

      if (UT <> ut_None) then
        while ((CanEquipIron and (UT in WARRIORS_IRON)) or (CanEquipLeather and not (UT in WARRIORS_IRON)))
        and HB.CanEquip(UT)
        and (GroupReq[GT] > 0)
        and ((fSetup.MaxSoldiers = -1) or (gHands[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers)) do
        begin
          HB.Equip(UT, 1);
          Dec(GroupReq[GT]);
          //Only reset it when we actually trained something (in IronThenLeather mode we don't count them separately)
          if (UT in WARRIORS_IRON) or (fSetup.ArmyType = atIronThenLeather) then
            fLastEquippedTimeIron := gGame.GameTickCount;
          if not (UT in WARRIORS_IRON) or (fSetup.ArmyType = atIronThenLeather) then
            fLastEquippedTimeLeather := gGame.GameTickCount;
        end;
    end;
  end;
end;


//Check army food level and positioning
procedure TKMGeneral.CheckArmy;
var
  I: Integer;
  GroupType: TGroupType;
  Group: TKMUnitGroup;
  NeedsLinkingTo: array [TGroupType] of TKMUnitGroup;
begin
  for GroupType := Low(TGroupType) to High(TGroupType) do
    NeedsLinkingTo[GroupType] := nil;

  //Check: Hunger, (feed) formation, (units per row) position (from defence positions)
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
  begin
    Group := gHands[fOwner].UnitGroups[I];

    if not Group.IsDead
    and Group.IsIdleToAI then
    begin
      //Check hunger and order food
      if (Group.Condition < UNIT_MIN_CONDITION) then
        //Cheat for autobuild AI: Only feed hungry group members (food consumption lower and more predictable)
        Group.OrderFood(True, fSetup.AutoBuild);

      if gGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

      //We already have a position, finished with this group
      if fDefencePositions.FindPositionOf(Group) <> nil then Continue;

      //Look for a new position to defend
      //In this case we choose the closest group, then move to a higher priority one later (see above)
      //This means at the start of the mission troops will take the position they are placed at rather than swapping around
      //With auto defence we reset defence positions regularly, so take closest rather than reshuffling all the time (newly equipped warriors still take highest priority)
      if fDefencePositions.FindPlaceForGroup(Group, AI_FILL_CLOSEST or fSetup.AutoDefend) then Continue;

      //Just chill and link with other idle groups
      if AI_LINK_IDLE then
      begin
        GroupType := Group.GroupType; //Remember it because Group might get emptied
        if NeedsLinkingTo[GroupType] = nil then
        begin
          //If this group doesn't have enough members
          if (Group.Count < fDefencePositions.TroopFormations[GroupType].NumUnits) then
            NeedsLinkingTo[GroupType] := Group //Flag us as needing to be added to
        end
        else
        begin
          //Look for group that needs additional members
          fDefencePositions.RestockPositionWith(NeedsLinkingTo[GroupType], Group);
          if NeedsLinkingTo[GroupType].Count >= fDefencePositions.TroopFormations[GroupType].NumUnits then
            NeedsLinkingTo[GroupType] := nil; //Group is now full
        end;
      end;
    end;
  end;
end;


procedure TKMGeneral.CheckAttacks;
var
  MenAvailable: TGroupTypeArray; //Total number of warriors available to attack the enemy
  GroupsAvailable: TGroupTypeArray;
  MaxGroupsAvailable: Integer;
  AttackGroups: array [TGroupType] of array of TKMUnitGroup;

  procedure AddAvailable(aGroup: TKMUnitGroup);
  var GT: TGroupType;
  begin
    GT := UnitGroups[aGroup.UnitType];
    if Length(AttackGroups[GT]) <= GroupsAvailable[GT] then
      SetLength(AttackGroups[GT], GroupsAvailable[GT] + 10);
    AttackGroups[GT, GroupsAvailable[GT]] := aGroup;
    Inc(GroupsAvailable[GT]);
    MaxGroupsAvailable := Max(MaxGroupsAvailable, GroupsAvailable[GT]);
    Inc(MenAvailable[GT], aGroup.Count);
  end;

var
  I, K, J: Integer;
  G: TGroupType;
  Group: TKMUnitGroup;
  DP: TAIDefencePosition;
  UnitsSent: Integer;
  AttackLaunched: Boolean;
  Looped: Integer;
begin
  //Do not process attack or defence during peacetime
  if gGame.IsPeaceTime then Exit;

  Looped := 0;
  repeat
    AttackLaunched := False;
    MaxGroupsAvailable := 0;
    for G := Low(TGroupType) to High(TGroupType) do
    begin
      GroupsAvailable[G] := 0;
      MenAvailable[G] := 0;
    end;

    //Order of units is prioritized:
    //1. Take all idling Groups that are not linked to any Defence positions
    for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
    begin
      Group := gHands[fOwner].UnitGroups[I];
      if not Group.IsDead
      and Group.IsIdleToAI(True) then
      begin
        DP := fDefencePositions.FindPositionOf(Group);
        if DP = nil then
          AddAvailable(Group);
      end;
    end;
    //2. Take back line defence positions, lowest priority first
    for I := fDefencePositions.Count-1 downto 0 do
      if (fDefencePositions[I].DefenceType = adt_BackLine)
      and (fDefencePositions[I].CurrentGroup <> nil)
      and not fDefencePositions[I].CurrentGroup.IsDead
      and fDefencePositions[I].CurrentGroup.IsIdleToAI(True) then
        AddAvailable(fDefencePositions[I].CurrentGroup);

    //Now process AI attacks (we have compiled a list of warriors available to attack)
    for I := 0 to Attacks.Count - 1 do
    if Attacks.CanOccur(I, MenAvailable, GroupsAvailable, gGame.GameTickCount) then //Check conditions are right
    begin
      AttackLaunched := True;
      //Order groups to attack
      UnitsSent := 0;
      if Attacks[I].TakeAll then
      begin
        //Repeatedly send one of each group type until we have sent the required amount (mixed army)
        for K := 0 to MaxGroupsAvailable - 1 do
          for G := Low(TGroupType) to High(TGroupType) do
            if (UnitsSent < Attacks[I].TotalMen) and (K < GroupsAvailable[G]) then
            begin
              OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
              Inc(UnitsSent, AttackGroups[G, K].Count);
            end;
      end
      else
      begin
        //First send the number of each group as requested by the attack
        for G := Low(TGroupType) to High(TGroupType) do
          for K := 0 to Attacks[I].GroupAmounts[G] - 1 do
          begin
            OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
            Inc(UnitsSent, AttackGroups[G, K].Count);
          end;

        //If we still haven't sent enough men, send more groups out of the types allowed until we have
        if UnitsSent < Attacks[I].TotalMen then
          for K := 0 to MaxGroupsAvailable - 1 do
            for G := Low(TGroupType) to High(TGroupType) do
            begin
              //Start index after the ones we've already sent above (ones required by attack)
              J := K + Attacks[I].GroupAmounts[G];
              if (Attacks[I].GroupAmounts[G] > 0) and (UnitsSent < Attacks[I].TotalMen)
              and (J < GroupsAvailable[G]) then
              begin
                OrderAttack(AttackGroups[G, J], Attacks[I].Target, Attacks[I].CustomPosition);
                Inc(UnitsSent, AttackGroups[G, J].Count);
              end;
            end;
      end;
      Attacks.HasOccured(I); //We can't set the flag to property record directly
      Break; //Only order 1 attack per update, since all the numbers of groups available need recalculating
    end;
    Inc(Looped);
  //if there's no target available we could loop forever ordering the same attack, so limit it
  until (not AttackLaunched) or (Looped > 10);
end;


procedure TKMGeneral.CheckAutoAttack;
var
  SimpleAttack: TAIAttack;
  H: TKMHouse;
begin
  //Simple test for now
  FillChar(SimpleAttack, SizeOf(SimpleAttack), #0);

  SimpleAttack.AttackType := aat_Repeating;
  SimpleAttack.Target := att_ClosestBuildingFromStartPos;
  SimpleAttack.TotalMen := fDefencePositions.AverageUnitsPerGroup *
                           fDefencePositions.GetBacklineCount div 2;
  SimpleAttack.TakeAll := True;

  Attacks.Clear;
  Attacks.AddAttack(SimpleAttack);

  //If start position isn't set, set it to first storehouse (used for targeting attacks)
  if (fSetup.StartPosition.X <= 1) and (fSetup.StartPosition.Y <= 1) then
  begin
    H := gHands[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1);
    if H <> nil then
      fSetup.StartPosition := H.Entrance;
  end;

  //See how many soldiers we need to launch an attack

  //Check if we have enough troops we can take into attack (Backline formations)

  //Check if we can train more soldiers (ignoring EquipRate?)

  //Make decision about attack

  //Choose place to attack
end;


procedure TKMGeneral.CheckAutoDefend;

  function EnsureWalkable(var Loc: TKMPoint): Boolean;
  var
    IX, IY, BestDistSqr: Integer;
    Best: TKMPoint;
  begin
    if gTerrain.CheckPassability(Loc, tpWalk) then
    begin
      Result := True;
      Exit;
    end;
    Result := False;
    BestDistSqr := High(Integer);
    for IY := Max(1, Loc.Y-2) to Min(gTerrain.MapY, Loc.Y+2) do
      for IX := Max(1, Loc.X-2) to Min(gTerrain.MapX, Loc.X+2) do
        if gTerrain.CheckPassability(KMPoint(IX, IY), tpWalk)
        and (KMLengthSqr(Loc, KMPoint(IX, IY)) < BestDistSqr) then
        begin
          BestDistSqr := KMLengthSqr(Loc, KMPoint(IX, IY));
          Best := KMPoint(IX, IY);
          Result := True;
        end;
    if Result then
      Loc := Best;
  end;

var
  Outline1, Outline2: TKMWeightSegments;
  I, K: Integer;
  Locs: TKMPointDirTagList;
  Loc: TKMPoint;
  LocI: TKMPoint;
  FaceDir: TKMDirection;
  SegLength, Ratio: Single;
  DefCount: Byte;
  GT: TGroupType;
  DPT: TAIDefencePosType;
  Weight: Cardinal;
  BacklineCount: Integer;
begin
  //Get defence Outline with weights representing how important each segment is
  gAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);

  fDefencePositions.Clear;
  BacklineCount := 0;

  Locs := TKMPointDirTagList.Create;
  try
    //Make list of defence positions
    for I := 0 to High(Outline2) do
    begin
      FaceDir := KMGetDirection(KMPointF(Outline2[I].A), KMPerpendecular(Outline2[I].A, Outline2[I].B));

      //Longer segments will get several DefencePositions
      SegLength := KMLength(Outline2[I].A, Outline2[I].B);
      DefCount := Max(Trunc(SegLength / 5), 1); //At least 1, otherwise we might leave a bridge undefended

      for K := 0 to DefCount - 1 do
      begin
        Ratio := (K + 1) / (DefCount + 1);
        Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
        Weight := Round(Outline2[I].Weight * 100);
        //Make sure each segment gets 1 defence position before filling others (in the middle of the segment line)
        if K = ((DefCount - 1) div 2) then
          Weight := Weight + 10000;

        Locs.Add(KMPointDir(Loc, FaceDir), Weight);
      end;
    end;

    //Sort according to positions weight
    Locs.SortByTag;

    //Add defence positions
    for I := Locs.Count - 1 downto 0 do
    begin
      LocI := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 1);
      Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
      if not EnsureWalkable(Loc) then
        Continue;

      //Mix group types deterministicly based on Loc, so they don't change for a given position
      case (Loc.X*2 + Loc.Y*2) mod 3 of
        0:   GT := gt_AntiHorse;
        else GT := gt_Melee;
      end;
      //Low weight positions are set to backline (when one segment has more than one position)
      if Locs.Tag[I] < 10000 then
        DPT := adt_BackLine
      else
        DPT := adt_FrontLine;

      fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), GT, 25, DPT);
      if DPT = adt_BackLine then Inc(BacklineCount);

      LocI := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 4);
      Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
      if not EnsureWalkable(Loc) then
        Continue;

      fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), gt_Ranged, 25, DPT);
      if DPT = adt_BackLine then Inc(BacklineCount);
    end;

    //Add extra backline defence positions after adding all the front line ones so they are lower priority
    for I := Locs.Count - 1 downto 0 do
    if BacklineCount < 12 then
    begin
      //Try to add backline defence positions behind front line, if there's space
      Loc := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 7);
      if gTerrain.TileInMapCoords(Loc.X, Loc.Y, 3) then
      begin
        if not EnsureWalkable(Loc) then
          Continue;

        //Mix group types deterministicly based on Loc, so they don't change for a given position
        case (Loc.X*2 + Loc.Y*2) mod 3 of
          0:   GT := gt_AntiHorse;
          1:   GT := gt_Ranged;
          else GT := gt_Melee;
        end;
        fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), GT, 35, adt_BackLine);
        Inc(BacklineCount);
      end;
    end;
  finally
    Locs.Free;
  end;

  //Compare existing defence positions with the sample
    //Get the ratio between sample and existing troops
    //Check all segments to have proportional troops count
    //Add or remove defence positions
end;


//See if we can attack our enemies
procedure TKMGeneral.OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
var
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit  := nil;

  //Find target
  case aTarget of
    att_ClosestUnit:                  TargetUnit := gHands.GetClosestUnit(aGroup.Position, fOwner, at_Enemy);
    att_ClosestBuildingFromArmy:      TargetHouse := gHands.GetClosestHouse(aGroup.Position, fOwner, at_Enemy, false);
    att_ClosestBuildingFromStartPos:  TargetHouse := gHands.GetClosestHouse(fSetup.StartPosition, fOwner, at_Enemy, false);
    att_CustomPosition:               begin
                                        TargetHouse := gHands.HousesHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetHouse <> nil) and
                                           (gHands.CheckAlliance(fOwner, TargetHouse.Owner) = at_Ally) then
                                          TargetHouse := nil;

                                        TargetUnit := gTerrain.UnitsHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetUnit <> nil)
                                        and ((gHands.CheckAlliance(fOwner, TargetUnit.Owner) = at_Ally)
                                            or TargetUnit.IsDeadOrDying) then
                                          TargetUnit := nil;
                                      end;
  end;

  //Choose best option
  if TargetHouse <> nil then
    aGroup.OrderAttackHouse(TargetHouse, True)
  else if TargetUnit <> nil then
    aGroup.OrderAttackUnit(TargetUnit, True)
  else if aTarget = att_CustomPosition then
    aGroup.OrderWalk(aCustomPos, True);
end;


procedure TKMGeneral.RetaliateAgainstThreat(aAttacker: TKMUnit);
var
  I: Integer;
  Group: TKMUnitGroup;
begin
  if gHands[fOwner].HandType = hndHuman then Exit;

  //Attacker may be already dying (e.g. killed by script)
  //We could retaliate against his whole group however
  if (aAttacker = nil) or aAttacker.IsDeadOrDying or (aAttacker is TKMUnitRecruit) then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for I := 0 to fDefencePositions.Count - 1 do
  begin
    Group := fDefencePositions[I].CurrentGroup;
    if (Group <> nil)
    and not Group.IsDead
    and Group.IsIdleToAI(True) //Units walking to their defence position can retaliate (but not if pursuing an enemy)
    //@Lewin: Is it right that Group defends against attackers within the Rad
    //rather than defending property within the Rad?
    //Think of archer, he attacks property in AI defense radius, but stands utself outside of the radius
    //should AI retaliate against him or not?
    //@Krom: Yes it's right the way it is now. It should be the attacker not the victim.
    //Otherwise the AI sends much more groups when you shoot them with 1 bowmen in the campaigns.
    //Right now it seems to be working almost the same as in the original game.
    and (KMLengthDiag(Group.Position, aAttacker.GetPosition) <= fDefencePositions[I].Radius) then
      Group.OrderAttackUnit(aAttacker, True);
  end;
end;


//Trained warrior reports for duty
procedure TKMGeneral.WarriorEquipped(aGroup: TKMUnitGroup);
begin
  fDefencePositions.FindPlaceForGroup(aGroup, AI_FILL_CLOSEST);
end;


procedure TKMGeneral.UpdateState(aTick: Cardinal);
begin
  //Update defence positions locations
  if fSetup.AutoDefend then
    //Checking mod result against MAX_HANDS causes first update to happen ASAP
    if (aTick + Byte(fOwner)) mod (MAX_HANDS * 120) = MAX_HANDS then
      CheckAutoDefend;

  //See if we can launch an attack
  if fSetup.AutoAttack then
    if (aTick + Byte(fOwner)) mod (MAX_HANDS * 120) = 1 then
      CheckAutoAttack;

  if (aTick + Byte(fOwner)) mod MAX_HANDS = 0 then
  begin
    fDefencePositions.UpdateState;
    CheckArmy; //Feed army, position defence, arrange/organise groups
    CheckAttacks;
    CheckArmyCount; //Train new soldiers if needed

    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
    //CheckAndIssueAttack; //Attack enemy
    //Anything Else?
  end;
end;


end.