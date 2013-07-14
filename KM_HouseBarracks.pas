unit KM_HouseBarracks;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math,
  KM_CommonClasses, KM_Defaults,
  KM_Houses, KM_ResHouses, KM_ResWares;


type
  //Barracks has 11 resources and Recruits
  TKMHouseBarracks = class(TKMHouse)
  private
    ResourceCount: array [WARFARE_MIN..WARFARE_MAX] of Word;
  public
    NotAcceptFlag: array [WARFARE_MIN .. WARFARE_MAX] of Boolean;
    RecruitsList: TList;
    constructor Create(aID: cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    procedure Activate(aWasBuilt: Boolean); override;
    procedure DemolishHouse(aFrom: TPlayerIndex; IsSilent: Boolean = False); override;
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    procedure ResTakeFromOut(aWare: TWareType; const aCount: Word = 1); override;
    function CheckResIn(aWare: TWareType): Word; override;
    function CanTakeResOut(aWare: TWareType): Boolean;
    function ResCanAddToIn(aRes: TWareType): Boolean; override;
    function CanEquip(aUnitType: TUnitType): Boolean;
    procedure ToggleAcceptFlag(aRes: TWareType);
    function Equip(aUnitType: TUnitType; aCount: Byte): Byte;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Units, KM_Units_Warrior, KM_PlayersCollection, KM_ResUnits;


{ TKMHouseBarracks }
constructor TKMHouseBarracks.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
begin
  inherited;
  RecruitsList := TList.Create;
end;


constructor TKMHouseBarracks.Load(LoadStream: TKMemoryStream);
var
  I,aCount: Integer;
  U: TKMUnit;
begin
  inherited;
  LoadStream.Read(ResourceCount, SizeOf(ResourceCount));
  RecruitsList := TList.Create;
  LoadStream.Read(aCount);
  for I := 0 to aCount - 1 do
  begin
    LoadStream.Read(U, 4); //subst on syncload
    RecruitsList.Add(U);
  end;
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


procedure TKMHouseBarracks.SyncLoad;
var I: Integer;
begin
  Inherited;
  for I := 0 to RecruitsList.Count - 1 do
    RecruitsList.Items[I] := fPlayers.GetUnitByID(Cardinal(RecruitsList.Items[I]));
end;


destructor TKMHouseBarracks.Destroy;
begin
  RecruitsList.Free;
  inherited;
end;


procedure TKMHouseBarracks.Activate(aWasBuilt: Boolean);
var
  FirstBarracks: TKMHouseBarracks;
  RT: TWareType;
begin
  inherited;
  //A new Barracks should inherit the accept properies of the first Barracks of that player,
  //which stops a sudden flow of unwanted wares to it as soon as it is created.
  FirstBarracks := TKMHouseBarracks(fPlayers[fOwner].FindHouse(ht_Barracks, 1));
  if (FirstBarracks <> nil) and not FirstBarracks.IsDestroyed then
    for RT := WARFARE_MIN to WARFARE_MAX do
      NotAcceptFlag[RT] := FirstBarracks.NotAcceptFlag[RT];
end;


procedure TKMHouseBarracks.DemolishHouse(aFrom: TPlayerIndex; IsSilent: Boolean = False);
var
  R: TWareType;
begin
  //Recruits are no longer under our control so we forget about them (UpdateVisibility will sort it out)
  //Otherwise it can cause crashes while saving under the right conditions when a recruit is then killed.
  RecruitsList.Clear;

  for R := WARFARE_MIN to WARFARE_MAX do
    fPlayers[fOwner].Stats.WareConsumed(R, ResourceCount[R]);

  inherited;
end;


procedure TKMHouseBarracks.ResAddToIn(aWare: TWareType; aCount:word=1; aFromScript:boolean=false);
begin
  Assert(aWare in [WARFARE_MIN..WARFARE_MAX], 'Invalid resource added to barracks');

  ResourceCount[aWare] := EnsureRange(ResourceCount[aWare]+aCount, 0, High(Word));
  fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, aWare, aCount);
end;


function TKMHouseBarracks.ResCanAddToIn(aRes: TWareType): Boolean;
begin
  Result := (aRes in [WARFARE_MIN..WARFARE_MAX]);
end;


function TKMHouseBarracks.CheckResIn(aWare: TWareType): Word;
begin
  if aWare in [WARFARE_MIN..WARFARE_MAX] then
    Result := ResourceCount[aWare]
  else
    Result := 0; //Including Wood/stone in building stage
end;


procedure TKMHouseBarracks.ResTakeFromOut(aWare: TWareType; const aCount: Word=1);
begin
  Assert(aCount <= ResourceCount[aWare]);
  dec(ResourceCount[aWare], aCount);
end;


function TKMHouseBarracks.CanTakeResOut(aWare: TWareType): Boolean;
begin
  Assert(aWare in [WARFARE_MIN .. WARFARE_MAX]);
  Result := (ResourceCount[aWare] > 0);
end;


procedure TKMHouseBarracks.ToggleAcceptFlag(aRes: TWareType);
begin
  Assert(aRes in [WARFARE_MIN .. WARFARE_MAX]);

  NotAcceptFlag[aRes] := not NotAcceptFlag[aRes];
end;


function TKMHouseBarracks.CanEquip(aUnitType: TUnitType): Boolean;
var I: Integer;
begin
  Result := RecruitsList.Count > 0; //Can't equip anything without recruits

  for I := 1 to 4 do
  if TroopCost[aUnitType, I] <> wt_None then //Can't equip if we don't have a required resource
    Result := Result and (ResourceCount[TroopCost[aUnitType, I]] > 0);
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
      Dec(ResourceCount[TroopCost[aUnitType, I]]);
      fPlayers[fOwner].Stats.WareConsumed(TroopCost[aUnitType, I]);
      fPlayers[fOwner].Deliveries.Queue.RemOffer(Self, TroopCost[aUnitType, I], 1);
    end;

    //Special way to kill the Recruit because it is in a house
    TKMUnitRecruit(RecruitsList.Items[0]).DestroyInBarracks;
    RecruitsList.Delete(0); //Delete first recruit in the list

    //Make new unit
    Soldier := TKMUnitWarrior(fPlayers[fOwner].TrainUnit(aUnitType, GetEntrance));
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


procedure TKMHouseBarracks.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  inherited;
  SaveStream.Write(ResourceCount, SizeOf(ResourceCount));
  SaveStream.Write(RecruitsList.Count);
  for I := 0 to RecruitsList.Count - 1 do
    SaveStream.Write(TKMUnit(RecruitsList.Items[I]).ID); //Store ID
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
end;


end.
