unit KM_AIGeneral;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_AISetup, KM_AIDefensePos,
  KM_UnitGroups, KM_Units, KM_AIAttacks;


type
  TKMGeneral = class
  private
    fLastEquippedTime: Cardinal;
    fOwner: THandIndex;
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
    constructor Create(aPlayer: THandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: THandIndex);
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
uses KM_HandsCollection, KM_Hand, KM_Terrain, KM_Game, KM_HouseBarracks,
  KM_AIFields, KM_NavMesh, KM_Houses, KM_Utils, KM_ResHouses;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = False;
  AI_LINK_IDLE = False;


{ TKMGeneral }
constructor TKMGeneral.Create(aPlayer: THandIndex; aSetup: TKMHandAISetup);
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


procedure TKMGeneral.OwnerUpdate(aPlayer: THandIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fLastEquippedTime);
  fAttacks.Save(SaveStream);
  fDefencePositions.Save(SaveStream);
end;


procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fLastEquippedTime);
  fAttacks.Load(LoadStream);
  fDefencePositions.Load(LoadStream);
end;


procedure TKMGeneral.SyncLoad;
begin
  fDefencePositions.SyncLoad;
end;


procedure TKMGeneral.CheckArmyCount;
var
  Barracks: array of TKMHouseBarracks;
  HB: TKMHouseBarracks;
  GT: TGroupType;
  I,K: Integer;
  UT: TUnitType;
  TrainedSomething, CanEquipIron, CanEquipLeather: Boolean;
  GroupReq: TGroupTypeArray;
begin
  if gGame.IsPeaceTime then Exit; //Do not train soldiers during peacetime

  //Don't train if we have reached our limit
  if (fSetup.MaxSoldiers <> -1) and (gHands[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers) then
    Exit;

  //Delay between equipping soldiers for KaM compatibility
  CanEquipIron := gGame.CheckTime(fLastEquippedTime + fSetup.EquipRateIron);
  CanEquipLeather := gGame.CheckTime(fLastEquippedTime + fSetup.EquipRateLeather);

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

    if GroupReq[GT] = 0 then Break; //Don't train

    for K := 1 to 3 do
    begin
      TrainedSomething := False;
      UT := AITroopTrainOrder[GT, K];
      if (UT <> ut_None)
      and ((CanEquipIron and (UT in WARRIORS_IRON)) or (CanEquipLeather and not (UT in WARRIORS_IRON))) then
        while HB.CanEquip(UT)
        and (GroupReq[GT] > 0)
        and ((fSetup.MaxSoldiers = -1) or (gHands[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers)) do
        begin
          HB.Equip(UT, 1);
          Dec(GroupReq[GT]);
          TrainedSomething := True;
          fLastEquippedTime := gGame.GameTickCount; //Only reset it when we actually trained something
          if fSetup.GetEquipRate(UT) > 0 then
            Break; //Only equip 1 soldier when we have a restricted equip rate
        end;
      if TrainedSomething and (fSetup.GetEquipRate(UT) > 0) then
        Break; //Only equip 1 soldier when we have a restricted equip rate
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
        Group.OrderFood(True);

      if gGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

      //We already have a position, finished with this group
      if fDefencePositions.FindPositionOf(Group) <> nil then Continue;

      //Look for a new position to defend
      //In this case we choose the closest group, then move to a higher priority one later (see above)
      //This means at the start of the mission troops will take the position they are placed at rather than swapping around
      if fDefencePositions.FindPlaceForGroup(Group, AI_FILL_CLOSEST) then Continue;

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
begin
  //Do not process attack or defence during peacetime
  if gGame.IsPeaceTime then Exit;

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
    and (fDefencePositions[I].CurrentGroup <> nil) then
      AddAvailable(fDefencePositions[I].CurrentGroup);

  //Now process AI attacks (we have compiled a list of warriors available to attack)
  for I := 0 to Attacks.Count - 1 do
  if Attacks.CanOccur(I, MenAvailable, GroupsAvailable, gGame.GameTickCount) then //Check conditions are right
  begin
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
  end;
end;


procedure TKMGeneral.CheckAutoAttack;
begin
  //See how many soldiers we need to launch an attack

  //Check if we have enough troops we can take into attack (Backline formations)

  //Check if we can train more soldiers (ignoring EquipRate?)

  //Make decision about attack

  //Choose place to attack
end;


procedure TKMGeneral.CheckAutoDefend;
var
  Outline1, Outline2: TKMWeightSegments;
  I, K: Integer;
  Locs: TKMPointDirTagList;
  Loc: TKMPoint;
  LocI: TKMPoint;
  FaceDir: TKMDirection;
  SegLength, Ratio: Single;
  DefCount: Byte;
begin
  //Get defence Outline with weights representing how important each segment is
  fAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);

  fDefencePositions.Clear;

  Locs := TKMPointDirTagList.Create;
  try
    //Make list of defence positions
    for I := 0 to High(Outline2) do
    begin
      FaceDir := KMGetDirection(KMPointF(Outline2[I].A), KMPerpendecular(Outline2[I].A, Outline2[I].B));

      //Longer segments will get several DefencePositions
      SegLength := KMLength(Outline2[I].A, Outline2[I].B);
      DefCount := Trunc(SegLength / 5);

      for K := 0 to DefCount - 1 do
      begin
        Ratio := (K + 1) / (DefCount + 1);
        Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
        Locs.Add(KMPointDir(Loc, FaceDir), Round(Outline2[I].Weight * 100));
      end;
    end;

    //Sort according to positions weight
    Locs.SortByTag;

    //Add defence positions
    for I := Locs.Count - 1 downto 0 do
    begin
      LocI := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 1);
      Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
      fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), gt_Melee, 25, adt_FrontLine);

      LocI := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 4);
      Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
      fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), gt_Ranged, 25, adt_FrontLine);
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
                                        if (TargetUnit <> nil) and
                                           (gHands.CheckAlliance(fOwner, TargetUnit.Owner) = at_Ally) then
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
  if gHands[fOwner].PlayerType = hndHuman then Exit;

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
    if (aTick + Byte(fOwner)) mod (MAX_HANDS * 120) = 0 then
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

