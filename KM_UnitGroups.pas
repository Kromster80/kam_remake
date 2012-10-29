unit KM_UnitGroups;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
     KM_Defaults, KM_CommonTypes, KM_Points, KM_PlayersCollection,
     KM_UnitActionFight, KM_Units_Warrior;

type
  TKMUnitGroup = class
  private
    fMembers: TList; // Warriors. List of TKMUnitWarrior.
    fUnitsPerRow: Integer;
    fPosition: TKMPointDir;

    function GetCount: Integer;
    function GetMember(aIndex: Integer): TKMUnitWarrior;
    procedure SetUnitsPerRow(aCount: Integer);
    procedure SetPosition(aValue: TKMPointDir);
  public
    constructor Create(aCreator: TKMUnitWarrior); overload;
    constructor Create(aOwner: TPlayerIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word; aMapEditor: Boolean = False); overload;
    destructor Destroy; override;

    procedure AddMember(aWarrior: TKMUnitWarrior);
    function ArmyInFight: Boolean;
    function CanTakeOrders: Boolean;
    function IsRanged: Boolean;

    property Count: Integer read GetCount;
    property Members[aIndex: Integer]: TKMUnitWarrior read GetMember;
    property Position: TKMPointDir read fPosition write SetPosition;
    property UnitsPerRow: Integer read fUnitsPerRow write SetUnitsPerRow;
  end;


  TKMUnitGroups = class
  private
    fGroups: TList;
    fOwner: TPlayerIndex;

    function GetCount: Integer;
    function GetGroup(aIndex: Integer): TKMUnitGroup;
  public
    constructor Create(aOwner: TPlayerIndex);
    destructor Destroy; override;

    function AddGroup(aOwner: TPlayerIndex; aUnitType: TUnitType; PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word; aMapEditor: Boolean = False): TKMUnitGroup;

    property Count: Integer read GetCount;
    property Groups[aIndex: Integer]: TKMUnitGroup read GetGroup; default;
  end;


implementation
uses KM_Resource, KM_ResourceUnit, KM_Terrain, KM_Units;


{ TKMUnitGroup }
constructor TKMUnitGroup.Create(aCreator: TKMUnitWarrior);
begin
  inherited Create;

  fMembers := TList.Create;
  fUnitsPerRow := 1;

  AddMember(aCreator);
end;


constructor TKMUnitGroup.Create(aOwner: TPlayerIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word;
  aMapEditor: Boolean);
var
  Commander: TKMUnitWarrior;
begin
  inherited Create;

  //Add commander first
  Commander := TKMUnitWarrior(fPlayers[aOwner].Units.Add(aOwner, aUnitType, PosX, PosY));
  if Commander = nil then
    Exit;
  fPlayers[aOwner].Stats.UnitCreated(aUnitType, False);


end;


destructor TKMUnitGroup.Destroy;
begin
  fMembers.Free;

  inherited;
end;


function TKMUnitGroup.GetCount: Integer;
begin
  Result := fMembers.Count;
end;


function TKMUnitGroup.GetMember(aIndex: Integer): TKMUnitWarrior;
begin
  // Assert is not needed
  // If Index is wrong, then TList.Get raise exception
  Result := TKMUnitWarrior(fMembers.Items[aIndex]);
end;


procedure TKMUnitGroup.SetPosition(aValue: TKMPointDir);
begin
  fPosition := aValue;
end;


procedure TKMUnitGroup.SetUnitsPerRow(aCount: Integer);
begin
  fUnitsPerRow := EnsureRange(aCount, 1, fMembers.Count);
end;


procedure TKMUnitGroup.AddMember(aWarrior: TKMUnitWarrior);
begin
  Assert(fMembers.IndexOf(aWarrior) = -1, 'We already have this Warrior in group');
  fMembers.Add(aWarrior);
end;


//If the player is allowed to issue orders to group
function TKMUnitGroup.CanTakeOrders: Boolean;
begin
  Result := IsRanged or not ArmyInFight; //Ranged units can always take orders
end;


function TKMUnitGroup.IsRanged: Boolean;
begin
  Result := fResource.UnitDat[Members[0].UnitType].FightType = ft_Ranged;
end;


function TKMUnitGroup.ArmyInFight: Boolean;
var I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
    if (Members[I].GetUnitAction is TUnitActionFight)
    and(TUnitActionFight(Members[I].GetUnitAction).GetOpponent is TKMUnitWarrior)
    and not(TUnitActionFight(Members[I].GetUnitAction).GetOpponent.IsDeadOrDying) then
    begin
      Result := True;
      Exit;
    end;
end;


{ TKMUnitGroups }
constructor TKMUnitGroups.Create(aOwner: TPlayerIndex);
begin
  inherited Create;

  fOwner := aOwner;
  fGroups := TList.Create;
end;


destructor TKMUnitGroups.Destroy;
begin
  fGroups.Free;

  inherited;
end;


function TKMUnitGroups.GetCount: Integer;
begin
  Result := fGroups.Count;
end;


function TKMUnitGroups.GetGroup(aIndex: Integer): TKMUnitGroup;
begin
  Result := fGroups[aIndex];
end;


function TKMUnitGroups.AddGroup(aOwner: TPlayerIndex; aUnitType: TUnitType;
  PosX, PosY: Word; aDir: TKMDirection; aUnitPerRow, aUnitCount: Word;
  aMapEditor: Boolean): TKMUnitGroup;
begin
  Result := nil;

  Assert(aDir <> dir_NA);
  Assert(aUnitType in [WARRIOR_MIN..WARRIOR_MAX]);

  Result := TKMUnitGroup.Create(aOwner, aUnitType, PosX, PosY, aDir, aUnitPerRow, aUnitCount, aMapEditor);

  //If group failed to create (e.g. due to being placed on unwalkable position)
  //then its memberCount = 0
  if Result.Count > 0 then
    fGroups.Add(Result)
  else
    FreeAndNil(Result);
end;


end.
