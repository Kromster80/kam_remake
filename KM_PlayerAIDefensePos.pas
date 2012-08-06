unit KM_PlayerAIDefensePos;
{$I KaM_Remake.inc}
interface
uses Classes,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Units_Warrior, KM_Points;


type
  //For now IDs must match with KaM
  TAIDefencePosType = (adt_FrontLine=0, //Front line troops may not go on attacks, they are for defence
                       adt_BackLine=1); //Back line troops may attack

  TAIDefencePosition = class
  private
    fCurrentCommander: TKMUnitWarrior; //Commander of group currently occupying position
    fPosition: TKMPointDir; //Position and direction the group defending will stand
    fRadius: Integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    procedure SetCurrentCommander(aCommander: TKMUnitWarrior);
    procedure ClearCurrentCommander;
  public
    GroupType: TGroupType; //Type of group to defend this position (e.g. melee)
    DefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    constructor Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    constructor Load(LoadStream:TKMemoryStream);
    destructor Destroy; override;
    property CurrentCommander: TKMUnitWarrior read fCurrentCommander write SetCurrentCommander;
    property Position: TKMPointDir read fPosition; //Position and direction the group defending will stand
    property Radius: Integer read fRadius; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    function IsFullyStocked(aAmount: integer): Boolean;
    procedure Save(SaveStream:TKMemoryStream);
    procedure SyncLoad;
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

    procedure AddDefencePosition(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    function FindPlaceForWarrior(aWarrior: TKMUnitWarrior; aCanLinkToExisting, aTakeClosest: Boolean): Boolean;
    procedure RestockPositionWith(aDefenceGroup, aCommander: TKMUnitWarrior);
    function FindPositionOf(aCommander: TKMUnitWarrior): TAIDefencePosition;
    procedure ReplaceCommander(aDeadCommander, aNewCommander: TKMUnitWarrior);

    property Count: Integer read fCount;
    property Positions[aIndex: Integer]: TAIDefencePosition read GetPosition; default;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses KM_PlayersCollection;


{ TAIDefencePosition }
constructor TAIDefencePosition.Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
begin
  inherited Create;
  fPosition := aPos;
  GroupType := aGroupType;
  fRadius := aRadius;
  DefenceType := aDefenceType;
  CurrentCommander := nil; //Unoccupied
end;


destructor TAIDefencePosition.Destroy;
begin
  ClearCurrentCommander; //Ensure pointer is removed
  inherited;
end;


procedure TAIDefencePosition.ClearCurrentCommander;
begin
  fPlayers.CleanUpUnitPointer(TKMUnit(fCurrentCommander));
end;


procedure TAIDefencePosition.SetCurrentCommander(aCommander: TKMUnitWarrior);
begin
  ClearCurrentCommander;
  if aCommander <> nil then
    fCurrentCommander := TKMUnitWarrior(aCommander.GetUnitPointer);
end;


procedure TAIDefencePosition.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(fPosition);
  SaveStream.Write(GroupType, SizeOf(GroupType));
  SaveStream.Write(fRadius);
  SaveStream.Write(DefenceType, SizeOf(DefenceType));
  if fCurrentCommander <> nil then
    SaveStream.Write(fCurrentCommander.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TAIDefencePosition.Load(LoadStream:TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fPosition);
  LoadStream.Read(GroupType, SizeOf(GroupType));
  LoadStream.Read(fRadius);
  LoadStream.Read(DefenceType, SizeOf(DefenceType));
  LoadStream.Read(fCurrentCommander, 4); //subst on syncload
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentCommander := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fCurrentCommander)));
end;


function TAIDefencePosition.IsFullyStocked(aAmount: integer): Boolean;
begin
  Result := (CurrentCommander <> nil) and (CurrentCommander.GetMemberCount + 1 >= aAmount);
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


function TAIDefencePositions.FindPlaceForWarrior(aWarrior: TKMUnitWarrior; aCanLinkToExisting, aTakeClosest: Boolean): Boolean;
var
  I, MenRequired, Matched: Integer;
  Distance, Best: Single;
begin
  Result := False;
  Matched := -1;
  Best := MaxInt;

  for I := 0 to Count - 1 do
  if Positions[I].GroupType = UnitGroups[aWarrior.UnitType] then
  begin
    //If not aCanLinkToExisting then a group with 1 member or more counts as fully stocked already
    if aCanLinkToExisting then
      MenRequired := TroopFormations[Positions[I].GroupType].NumUnits
    else
      MenRequired := 1;

    if not Positions[I].IsFullyStocked(MenRequired) then
    begin
      //Take closest position that is empty or requries restocking
      Distance := GetLength(aWarrior.GetPosition, Positions[I].Position.Loc);
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
    if Positions[Matched].CurrentCommander = nil then
    begin //New position
      Positions[Matched].CurrentCommander := aWarrior.GetCommander;
      aWarrior.OrderWalk(Positions[Matched].Position);
    end
    else //Restock existing position
      RestockPositionWith(Positions[Matched].CurrentCommander, aWarrior.GetCommander);
  end;
end;


procedure TAIDefencePositions.ReplaceCommander(aDeadCommander, aNewCommander: TKMUnitWarrior);
var
  DP: TAIDefencePosition;
begin
  DP := FindPositionOf(aDeadCommander);

  //DP could be nil if dead commander was not part of any DP
  if DP <> nil then
    DP.CurrentCommander := aNewCommander; //Don't need to use GetPointer/ReleasePointer because setting CurrentCommander does that
end;


procedure TAIDefencePositions.RestockPositionWith(aDefenceGroup, aCommander: TKMUnitWarrior);
var Needed: integer;
begin
  Needed := TroopFormations[UnitGroups[aDefenceGroup.UnitType]].NumUnits - (aDefenceGroup.GetMemberCount+1);
  if Needed <= 0 then exit;
  if aCommander.GetMemberCount+1 <= Needed then
    aCommander.OrderLinkTo(aDefenceGroup) //Link entire group
  else
    aCommander.OrderSplitLinkTo(aDefenceGroup,Needed); //Link only as many units as are needed
end;


//Find DefencePosition to which this Commander belongs
//(Result could be nil if CommanderCount > PositionsCount
function TAIDefencePositions.FindPositionOf(aCommander: TKMUnitWarrior): TAIDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
  if Positions[I].CurrentCommander = aCommander then
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


end.
