unit KM_AIDefensePos;
{$I KaM_Remake.inc}
interface
uses Classes, Math,
  KM_CommonClasses, KM_Defaults, KM_UnitGroups, KM_Points;


type
  //For now IDs must match with KaM
  TAIDefencePosType = (adt_FrontLine=0, //Front line troops may not go on attacks, they are for defence
                       adt_BackLine=1); //Back line troops may attack

  TAIDefencePosition = class
  private
    fCurrentGroup: TKMUnitGroup; //Commander of group currently occupying position

    fGroupType: TGroupType; //Type of group to defend this position (e.g. melee)
    fPosition: TKMPointDir; //Position and direction the group defending will stand
    fRadius: Integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    procedure SetCurrentGroup(aGroup: TKMUnitGroup);
  public
    DefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    constructor Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    property CurrentGroup: TKMUnitGroup read fCurrentGroup write SetCurrentGroup;
    property GroupType: TGroupType read fGroupType; //Type of group to defend this position (e.g. melee)
    property Position: TKMPointDir read fPosition; //Position and direction the group defending will stand
    property Radius: Integer read fRadius write fRadius; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    function IsFullyStocked(aAmount: Integer): Boolean;
    function UITitle: string;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;

  TAIDefencePositions = class
  private
    fCount: Integer;
    fPositions: array of TAIDefencePosition;
    function GetPosition(aIndex: Integer): TAIDefencePosition;
  public
    //Defines how defending troops will be formatted. 0 means leave unchanged.
    TroopFormations: array [TGroupType] of TKMFormation;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddDefencePosition(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    function FindPlaceForGroup(aGroup: TKMUnitGroup; aCanLinkToExisting, aTakeClosest: Boolean): Boolean;
    procedure RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
    function FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;

    property Count: Integer read fCount;
    property Positions[aIndex: Integer]: TAIDefencePosition read GetPosition; default;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


implementation
uses KM_PlayersCollection;


{ TAIDefencePosition }
constructor TAIDefencePosition.Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
begin
  inherited Create;
  fPosition := aPos;
  fGroupType := aGroupType;
  fRadius := aRadius;
  DefenceType := aDefenceType;
  CurrentGroup := nil; //Unoccupied
end;


destructor TAIDefencePosition.Destroy;
begin
  CurrentGroup := nil; //Ensure pointer is removed
  inherited;
end;


procedure TAIDefencePosition.SetCurrentGroup(aGroup: TKMUnitGroup);
begin
  //Release previous group
  fPlayers.CleanUpGroupPointer(fCurrentGroup);

  //Take new one
  if aGroup <> nil then
    fCurrentGroup := aGroup.GetGroupPointer;
end;


procedure TAIDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fPosition);
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  SaveStream.Write(fRadius);
  SaveStream.Write(DefenceType, SizeOf(DefenceType));
  if fCurrentGroup <> nil then
    SaveStream.Write(fCurrentGroup.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TAIDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fPosition);
  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fRadius);
  LoadStream.Read(DefenceType, SizeOf(DefenceType));
  LoadStream.Read(fCurrentGroup, 4); //subst on syncload
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentGroup := fPlayers.GetGroupByID(Cardinal(fCurrentGroup));
end;


function TAIDefencePosition.UITitle: string;
const T: array [TGroupType] of string = ('Melee', 'AntiHorse', 'Ranged', 'Mounted');
begin
  Result := T[fGroupType];
end;


function TAIDefencePosition.IsFullyStocked(aAmount: integer): Boolean;
begin
  Result := (CurrentGroup <> nil) and (CurrentGroup.Count >= aAmount);
end;


procedure TAIDefencePosition.UpdateState;
begin
  //If the group is Dead or too far away we should disassociate
  //them from the defence position so new warriors can take up the defence if needs be
  if (CurrentGroup = nil)
  or CurrentGroup.IsDead
  or ((CurrentGroup.InFight or (CurrentGroup.Order in [goAttackHouse, goAttackUnit]))
      and (KMLengthDiag(Position.Loc, CurrentGroup.Position) > Radius)) then
    CurrentGroup := nil;

  //Tell group to walk to its position
  //It's easier to repeat the order than check that all members are in place
  if (CurrentGroup <> nil)
  and not CurrentGroup.InFight
  and (CurrentGroup.Order = goNone)
  and CurrentGroup.CanWalkTo(Position.Loc, 0) then
    CurrentGroup.OrderWalk(Position.Loc, Position.Dir);
end;


{ TKMPlayerAI }
constructor TAIDefencePositions.Create;
var GT: TGroupType;
begin
  inherited Create;

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    TroopFormations[GT].NumUnits := 9; //These are the defaults in KaM
    TroopFormations[GT].UnitsPerRow := 3;
  end;
end;


destructor TAIDefencePositions.Destroy;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPositions[I].Free;
  inherited;
end;


function TAIDefencePositions.GetPosition(aIndex: Integer): TAIDefencePosition;
begin
  Result := fPositions[aIndex];
end;


procedure TAIDefencePositions.AddDefencePosition(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
begin
  SetLength(fPositions, fCount + 1);
  fPositions[fCount] := TAIDefencePosition.Create(aPos, aGroupType, aRadius, aDefenceType);
  inc(fCount);
end;


procedure TAIDefencePositions.Clear;
begin
  fCount := 0;
end;


function TAIDefencePositions.FindPlaceForGroup(aGroup: TKMUnitGroup; aCanLinkToExisting, aTakeClosest: Boolean): Boolean;
var
  I, MenRequired, Matched: Integer;
  Distance, Best: Single;
begin
  Result := False;
  Matched := -1;
  Best := MaxSingle;

  for I := 0 to Count - 1 do
  if Positions[I].GroupType = UnitGroups[aGroup.UnitType] then
  begin
    //If not aCanLinkToExisting then a group with 1 member or more counts as fully stocked already
    if aCanLinkToExisting then
      MenRequired := TroopFormations[Positions[I].GroupType].NumUnits
    else
      MenRequired := 1;

    if not Positions[I].IsFullyStocked(MenRequired) then
    begin
      //Take closest position that is empty or requries restocking
      Distance := KMLengthSqr(aGroup.Position, Positions[I].Position.Loc);
      if Distance < Best then
      begin
        Matched := I;
        Best := Distance;
        if not aTakeClosest then break; //Take first one we find - that's what KaM does
      end;
    end;
  end;

  if Matched <> -1 then
  begin
    Result := True;
    if Positions[Matched].CurrentGroup = nil then
    begin //New position
      Positions[Matched].CurrentGroup := aGroup;
      aGroup.OrderWalk(Positions[Matched].Position.Loc);
    end
    else //Restock existing position
      RestockPositionWith(Positions[Matched].CurrentGroup, aGroup);
  end;
end;


procedure TAIDefencePositions.RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
var Needed: integer;
begin
  Needed := TroopFormations[aDefenceGroup.GroupType].NumUnits - (aDefenceGroup.Count);
  if Needed <= 0 then exit;
  if aGroup.Count+1 <= Needed then
    aGroup.OrderLinkTo(aDefenceGroup) //Link entire group
  else
    aGroup.OrderSplitLinkTo(aDefenceGroup, Needed); //Link only as many units as are needed
end;


//Find DefencePosition to which this Commander belongs
//(Result could be nil if CommanderCount > PositionsCount
function TAIDefencePositions.FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
  if Positions[I].CurrentGroup = aGroup then
  begin
    Result := Positions[I];
    Break;
  end;
end;


procedure TAIDefencePositions.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(fCount);

  for I := 0 to fCount - 1 do
    fPositions[i].Save(SaveStream);
end;


procedure TAIDefencePositions.Load(LoadStream:TKMemoryStream);
var I: Integer;
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(fCount);
  SetLength(fPositions, fCount);

  for I := 0 to fCount - 1 do
    fPositions[I] := TAIDefencePosition.Load(LoadStream);
end;


procedure TAIDefencePositions.SyncLoad;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fPositions[I].SyncLoad;
end;


procedure TAIDefencePositions.UpdateState;
var I,K: Integer;
begin
  //Make sure no defence position Group is dead
  for I := 0 to Count - 1 do
    fPositions[I].UpdateState;

  //In KaM the order of defence positions is the priority: The first defined is higher priority
  for I := 0 to Count - 1 do
  if (fPositions[I].CurrentGroup = nil) then
    for K := I + 1 to Count - 1 do
    if fPositions[I].GroupType = fPositions[K].GroupType then
    begin
      fPositions[I].CurrentGroup := fPositions[K].CurrentGroup; //Take new position
      fPositions[K].CurrentGroup := nil; //Leave current position
      Break;
    end;
end;


end.
