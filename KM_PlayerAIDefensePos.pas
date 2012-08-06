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
    procedure SetCurrentCommander(aCommander: TKMUnitWarrior);
    procedure ClearCurrentCommander;
  public
    Position: TKMPointDir; //Position and direction the group defending will stand
    GroupType: TGroupType; //Type of group to defend this position (e.g. melee)
    DefenceRadius: integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    DefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    constructor Create(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
    constructor Load(LoadStream:TKMemoryStream);
    destructor Destroy; override;
    property CurrentCommander: TKMUnitWarrior read fCurrentCommander write SetCurrentCommander;
    procedure Save(SaveStream:TKMemoryStream);
    procedure SyncLoad;
    function IsFullyStocked(aAmount: integer):boolean;
  end;

  TAIDefencePositions = class
  private
    fCount: Integer;
  public
    TroopFormations: array[TGroupType] of record //Defines how defending troops will be formatted. 0 means leave unchanged.
                                            NumUnits, UnitsPerRow: Integer;
                                          end;
    DefencePositions: array of TAIDefencePosition;

    constructor Create;
    destructor Destroy; override;

    procedure AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);

    property Count: Integer read fCount;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses KM_PlayersCollection;


{ TAIDefencePosition }
constructor TAIDefencePosition.Create(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
begin
  inherited Create;
  Position := aPos;
  GroupType := aGroupType;
  DefenceRadius := aDefenceRadius;
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
  SaveStream.Write(Position);
  SaveStream.Write(GroupType, SizeOf(GroupType));
  SaveStream.Write(DefenceRadius);
  SaveStream.Write(DefenceType, SizeOf(DefenceType));
  if fCurrentCommander <> nil then
    SaveStream.Write(fCurrentCommander.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TAIDefencePosition.Load(LoadStream:TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(Position);
  LoadStream.Read(GroupType, SizeOf(GroupType));
  LoadStream.Read(DefenceRadius);
  LoadStream.Read(DefenceType, SizeOf(DefenceType));
  LoadStream.Read(fCurrentCommander, 4); //subst on syncload
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentCommander := TKMUnitWarrior(fPlayers.GetUnitByID(cardinal(fCurrentCommander)));
end;


function TAIDefencePosition.IsFullyStocked(aAmount: integer):boolean;
begin
  Result := (CurrentCommander <> nil) and (CurrentCommander.GetMemberCount+1 >= aAmount);
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
    DefencePositions[I].Free;
  inherited;
end;


procedure TAIDefencePositions.AddDefencePosition(aPos:TKMPointDir; aGroupType:TGroupType; aDefenceRadius:integer; aDefenceType:TAIDefencePosType);
begin
  SetLength(DefencePositions, fCount+1);
  DefencePositions[fCount] := TAIDefencePosition.Create(aPos,aGroupType,aDefenceRadius,aDefenceType);
  inc(fCount);
end;


procedure TAIDefencePositions.Save(SaveStream:TKMemoryStream);
var i: integer;
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(fCount);
  for i:=0 to fCount-1 do
    DefencePositions[i].Save(SaveStream);
end;


procedure TAIDefencePositions.Load(LoadStream:TKMemoryStream);
var I: Integer;
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(fCount);
  SetLength(DefencePositions, fCount);

  for I := 0 to fCount - 1 do
    DefencePositions[I] := TAIDefencePosition.Load(LoadStream);
end;


procedure TAIDefencePositions.SyncLoad;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    DefencePositions[I].SyncLoad;
end;


end.
