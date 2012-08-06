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
    TroopFormations: array[TGroupType] of record //Defines how defending troops will be formatted. 0 means leave unchanged.
                                            NumUnits, UnitsPerRow: Integer;
                                          end;

    constructor Create;
    destructor Destroy; override;

    procedure AddDefencePosition(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);

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


procedure TAIDefencePositions.Save(SaveStream:TKMemoryStream);
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
