unit KM_HouseBarracks;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, Types,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_ResHouses, KM_ResWares;


type
  //Barracks have 11 resources and Recruits
  TKMHouseBarracks = class(TKMHouse)
  private
    fRecruitsList: TList;
    fResourceCount: array [WARFARE_MIN..WARFARE_MAX] of Word;
  public
    MapEdRecruitCount: Word; //Only used by MapEd
    NotAcceptFlag: array [WARFARE_MIN .. WARFARE_MAX] of Boolean;
    RallyPoint: TKMPoint;
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    procedure Activate(aWasBuilt: Boolean); override;
    procedure DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False); override;
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    procedure ResTakeFromOut(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function CheckResIn(aWare: TWareType): Word; override;
    function ResCanAddToIn(aRes: TWareType): Boolean; override;

    function ResOutputAvailable(aRes: TWareType; const aCount: Word): Boolean; override;
    function CanEquip(aUnitType: TUnitType): Boolean;
    function RecruitsCount: Integer;
    procedure RecruitsAdd(aUnit: Pointer);
    procedure RecruitsRemove(aUnit: Pointer);
    procedure ToggleAcceptFlag(aRes: TWareType);
    function IsRallyPointSet: Boolean;
    function Equip(aUnitType: TUnitType; aCount: Byte): Byte;
    procedure CreateRecruitInside(aIsMapEd: Boolean);
  end;


implementation
uses
  KM_Units, KM_Units_Warrior, KM_HandsCollection, KM_ResUnits, KM_Hand;


{ TKMHouseBarracks }
constructor TKMHouseBarracks.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: THandIndex; aBuildState: THouseBuildState);
begin
  inherited;
  fRecruitsList := TList.Create;
  RallyPoint := KMPointBelow(GetEntrance);
end;


constructor TKMHouseBarracks.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
  U: TKMUnit;
begin
  inherited;
  LoadStream.Read(fResourceCount, SizeOf(fResourceCount));
  fRecruitsList := TList.Create;
  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(U, 4); //subst on syncload
    fRecruitsList.Add(U);
  end;
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
  LoadStream.Read(RallyPoint);
end;


procedure TKMHouseBarracks.SyncLoad;
var I: Integer;
begin
  inherited;
  for I := 0 to RecruitsCount - 1 do
    fRecruitsList.Items[I] := gHands.GetUnitByUID(Cardinal(fRecruitsList.Items[I]));
end;


destructor TKMHouseBarracks.Destroy;
begin
  fRecruitsList.Free;
  inherited;
end;


procedure TKMHouseBarracks.Activate(aWasBuilt: Boolean);
var
  FirstBarracks: TKMHouseBarracks;
  WT: TWareType;
begin
  inherited;
  //A new Barracks should inherit the accept properies of the first Barracks of that player,
  //which stops a sudden flow of unwanted wares to it as soon as it is created.
  FirstBarracks := TKMHouseBarracks(gHands[fOwner].FindHouse(ht_Barracks, 1));
  if (FirstBarracks <> nil) and not FirstBarracks.IsDestroyed then
    for WT := WARFARE_MIN to WARFARE_MAX do
      NotAcceptFlag[WT] := FirstBarracks.NotAcceptFlag[WT];
end;


procedure TKMHouseBarracks.DemolishHouse(aFrom: THandIndex; IsSilent: Boolean = False);
var
  R: TWareType;
begin
  //Recruits are no longer under our control so we forget about them (UpdateVisibility will sort it out)
  //Otherwise it can cause crashes while saving under the right conditions when a recruit is then killed.
  fRecruitsList.Clear;

  for R := WARFARE_MIN to WARFARE_MAX do
    gHands[fOwner].Stats.WareConsumed(R, fResourceCount[R]);

  inherited;
end;


procedure TKMHouseBarracks.RecruitsAdd(aUnit: Pointer);
begin
  fRecruitsList.Add(aUnit);
end;


function TKMHouseBarracks.RecruitsCount: Integer;
begin
  Result := fRecruitsList.Count;
end;


procedure TKMHouseBarracks.RecruitsRemove(aUnit: Pointer);
begin
  fRecruitsList.Remove(aUnit);
end;


procedure TKMHouseBarracks.ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  Assert(aWare in [WARFARE_MIN..WARFARE_MAX], 'Invalid resource added to barracks');

  fResourceCount[aWare] := EnsureRange(fResourceCount[aWare]+aCount, 0, High(Word));
  gHands[fOwner].Deliveries.Queue.AddOffer(Self, aWare, aCount);
end;


function TKMHouseBarracks.ResCanAddToIn(aRes: TWareType): Boolean;
begin
  Result := (aRes in [WARFARE_MIN..WARFARE_MAX]);
end;


function TKMHouseBarracks.CheckResIn(aWare: TWareType): Word;
begin
  if aWare in [WARFARE_MIN..WARFARE_MAX] then
    Result := fResourceCount[aWare]
  else
    Result := 0; //Including Wood/stone in building stage
end;


procedure TKMHouseBarracks.ResTakeFromOut(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  if aFromScript then
  begin
    aCount := Min(aCount, fResourceCount[aWare]);
    if aCount > 0 then
    begin
      gHands[fOwner].Stats.WareConsumed(aWare, aCount);
      gHands[fOwner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;
  Assert(aCount <= fResourceCount[aWare]);
  dec(fResourceCount[aWare], aCount);
end;


function TKMHouseBarracks.ResOutputAvailable(aRes: TWareType; const aCount: Word): Boolean;
begin
  Assert(aRes in [WARFARE_MIN .. WARFARE_MAX]);
  Result := (fResourceCount[aRes] >= aCount);
end;


procedure TKMHouseBarracks.ToggleAcceptFlag(aRes: TWareType);
begin
  Assert(aRes in [WARFARE_MIN .. WARFARE_MAX]);

  NotAcceptFlag[aRes] := not NotAcceptFlag[aRes];
end;


function TKMHouseBarracks.IsRallyPointSet: Boolean;
begin
   Result := not KMSamePoint(RallyPoint, KMPointBelow(GetEntrance));
end;


function TKMHouseBarracks.CanEquip(aUnitType: TUnitType): Boolean;
var
  I: Integer;
begin
  Result := RecruitsCount > 0; //Can't equip anything without recruits
  Result := Result and not gHands[fOwner].Stats.UnitBlocked[aUnitType];

  for I := 1 to 4 do
  if TroopCost[aUnitType, I] <> wt_None then //Can't equip if we don't have a required resource
    Result := Result and (fResourceCount[TroopCost[aUnitType, I]] > 0);
end;


//Equip a new soldier and make him walk out of the house
//Return the number of units successfully equipped
function TKMHouseBarracks.Equip(aUnitType: TUnitType; aCount: Byte): Byte;
var
  I, K: Integer;
  Soldier: TKMUnitWarrior;
begin
  Result := 0;
  Assert(aUnitType in [WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX]);

  for K := 0 to aCount - 1 do
  begin
    //Make sure we have enough resources to equip a unit
    if not CanEquip(aUnitType) then Exit;

    //Take resources
    for I := 1 to 4 do
    if TroopCost[aUnitType, I] <> wt_None then
    begin
      Dec(fResourceCount[TroopCost[aUnitType, I]]);
      gHands[fOwner].Stats.WareConsumed(TroopCost[aUnitType, I]);
      gHands[fOwner].Deliveries.Queue.RemOffer(Self, TroopCost[aUnitType, I], 1);
    end;

    //Special way to kill the Recruit because it is in a house
    TKMUnitRecruit(fRecruitsList.Items[0]).DestroyInBarracks;
    fRecruitsList.Delete(0); //Delete first recruit in the list

    //Make new unit
    Soldier := TKMUnitWarrior(gHands[fOwner].TrainUnit(aUnitType, GetEntrance));
    Soldier.SetInHouse(Self); //Put him in the barracks, so if it is destroyed while he is inside he is placed somewhere
    Soldier.Visible := False; //Make him invisible as he is inside the barracks
    Soldier.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
    //Soldier.OrderLoc := KMPointBelow(GetEntrance); //Position in front of the barracks facing north
    Soldier.SetActionGoIn(ua_Walk, gd_GoOutside, Self);
    if Assigned(Soldier.OnUnitTrained) then
      Soldier.OnUnitTrained(Soldier);
    Inc(Result);
  end;
end;


procedure TKMHouseBarracks.CreateRecruitInside(aIsMapEd: Boolean);
var U: TKMUnit;
begin
  if aIsMapEd then
    Inc(MapEdRecruitCount)
  else
  begin
    U := gHands[fOwner].TrainUnit(ut_Recruit, GetEntrance);
    U.Visible := False;
    U.SetInHouse(Self);
    U.SetHome(Self); //When walking out Home is used to remove recruit from barracks
    RecruitsAdd(U);
    gHands[fOwner].Stats.UnitCreated(ut_Recruit, False);
  end;
end;


procedure TKMHouseBarracks.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  inherited;

  SaveStream.Write(fResourceCount, SizeOf(fResourceCount));
  SaveStream.Write(RecruitsCount);
  for I := 0 to RecruitsCount - 1 do
    SaveStream.Write(TKMUnit(fRecruitsList.Items[I]).UID); //Store ID
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
  SaveStream.Write(RallyPoint);
end;


end.
