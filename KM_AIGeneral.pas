unit KM_AIGeneral;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_AISetup, KM_AIDefensePos,
  KM_UnitGroups, KM_Units_Warrior, KM_AIAttacks;


type
  TKMGeneral = class
  private
    fLastEquippedTime: Cardinal;
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fAttacks: TAIAttacks;
    fDefencePositions: TAIDefencePositions;

    procedure CheckArmiesCount;
    procedure CheckArmy;
    procedure CheckCanAttack;
    procedure CheckDefences;
    procedure OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    property Attacks: TAIAttacks read fAttacks;
    property DefencePositions: TAIDefencePositions read fDefencePositions;
    procedure RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses KM_PlayersCollection, KM_Player, KM_Terrain, KM_Game,
  KM_AIFields, KM_NavMesh, KM_Houses, KM_Units, KM_Utils;


const
  //For compatibility with KaM these must be false. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.
  AI_FILL_CLOSEST = false;
  AI_LINK_IDLE = false;


{ TKMGeneral }
constructor TKMGeneral.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
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


procedure TKMGeneral.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLastEquippedTime);
  fAttacks.Save(SaveStream);
  fDefencePositions.Save(SaveStream);
end;


procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fLastEquippedTime);
  fAttacks.Load(LoadStream);
  fDefencePositions.Load(LoadStream);
end;


procedure TKMGeneral.SyncLoad;
begin
  fDefencePositions.SyncLoad;
end;


procedure TKMGeneral.CheckArmiesCount;
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

  if fPlayers[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers then Exit; //Don't train if we have reached our limit

  //Delay between equipping soldiers for KaM compatibility
  CanEquipIron := fGame.CheckTime(fLastEquippedTime + fSetup.EquipRateIron);
  CanEquipLeather := fGame.CheckTime(fLastEquippedTime + fSetup.EquipRateLeather);

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
        and (fPlayers[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers) do
        begin
          HB.Equip(UT, 1);
          Dec(GroupReq[GT]);
          TrainedSomething := True;
          fLastEquippedTime := fGame.GameTickCount; //Only reset it when we actually trained something
          if fSetup.GetEquipRate(UT) > 0 then
            Break; //Only equip 1 soldier when we have a restricted equip rate
        end;
      if TrainedSomething and (fSetup.GetEquipRate(UT) > 0) then
        Break; //Only equip 1 soldier when we have a restricted equip rate
    end;
  end;
end;


procedure TKMGeneral.CheckArmy;
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
  begin
    Group := fPlayers[fOwner].UnitGroups[I];

    if not Group.IsDead
    and not Group.InFight
    and not (Group.Order in [goAttackHouse, goAttackUnit, goStorm]) then
    begin

      //Check hunger and order food
      if (Group.Condition < UNIT_MIN_CONDITION) then
        Group.OrderFood(True);

      if fGame.IsPeaceTime then Continue; //Do not process attack or defence during peacetime

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
end;


procedure TKMGeneral.CheckCanAttack;
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
    if not Group.IsDead
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


procedure TKMGeneral.CheckDefences;
var
  Outline1, Outline2: TKMWeightSegments;
  I: Integer;
  Loc: TKMPoint;
  LocI: TKMPointI;
  FaceDir: TKMDirection;
begin
  //Get defence Outline with weights representing how important each segment is
  fAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);

  fDefencePositions.Clear;

  //Create missing defence positions
  for I := fDefencePositions.Count to High(Outline2) do
  begin
    FaceDir := KMGetDirection(KMPointF(Outline2[I].A), KMPerpendecular(Outline2[I].A, Outline2[I].B));

    Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, 0.5));
    LocI := KMGetPointInDir(Loc, KMAddDirection(FaceDir, 4), 1);
    Loc := fTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    fDefencePositions.AddDefencePosition(KMPointDir(Loc, FaceDir), gt_Melee, 12, adt_FrontLine);

    Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, 0.5));
    LocI := KMGetPointInDir(Loc, KMAddDirection(FaceDir, 4), 4);
    Loc := fTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    fDefencePositions.AddDefencePosition(KMPointDir(Loc, FaceDir), gt_Ranged, 12, adt_FrontLine);
  end;

  //Compare existing defence positions with the sample
    //Get the ratio between sample and existing troops
    //Check all segments to have proportional troops count
    //Add or remove defence positions
end;


procedure TKMGeneral.OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
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
    att_ClosestBuildingFromStartPos:  TargetHouse := fPlayers.GetClosestHouse(fSetup.StartPosition, fOwner, at_Enemy, false);
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
    aGroup.OrderAttackHouse(TargetHouse, True)
  else if TargetUnit <> nil then
    aGroup.OrderAttackUnit(TargetUnit, True)
  else if aTarget = att_CustomPosition then
    aGroup.OrderWalk(aCustomPos, True);
end;


procedure TKMGeneral.RetaliateAgainstThreat(aAttacker: TKMUnitWarrior);
var
  I: Integer;
  Group: TKMUnitGroup;
begin
  if fPlayers[fOwner].PlayerType = pt_Human then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for I := 0 to fDefencePositions.Count - 1 do
  begin
    Group := fDefencePositions[I].CurrentGroup;
    if (Group <> nil)
    and not Group.IsDead
    and not Group.InFight
    and not (Group.Order in [goAttackHouse, goAttackUnit, goStorm])
    and (KMLengthDiag(Group.Position, aAttacker.GetPosition) <= fDefencePositions[I].Radius) then
      Group.OrderAttackUnit(aAttacker, True);
  end;
end;


procedure TKMGeneral.UpdateState(aTick: Cardinal);
begin
  if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
  begin
    fDefencePositions.UpdateState;
    CheckArmy; //Feed army, position defence, arrange/organise groups
    CheckCanAttack;
    CheckArmiesCount; //Train new soldiers if needed

    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
    //CheckAndIssueAttack; //Attack enemy
    //Anything Else?
  end;

  //Manage defence positions
  if fSetup.AutoDefend then
    if (aTick + Byte(fOwner)) mod (MAX_PLAYERS * 120) = 0 then
      CheckDefences;
end;


end.

