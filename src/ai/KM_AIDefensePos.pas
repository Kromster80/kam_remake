unit KM_AIDefensePos;
{$I KaM_Remake.inc}
interface
uses
  KM_UnitGroups,
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  //For now IDs must match with KaM
  TAIDefencePosType = (adt_FrontLine=0, //Front line troops may not go on attacks, they are for defence
                       adt_BackLine=1); //Back line troops may attack

  TKMFormation = record NumUnits, UnitsPerRow: Integer; end;

  TAIDefencePosition = class
  private
    fDefenceType: TAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    fGroupType: TGroupType; //Type of group to defend this position (e.g. melee)
    fPosition: TKMPointDir; //Position and direction the group defending will stand
    fRadius: Integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved

    fCurrentGroup: TKMUnitGroup; //Commander of group currently occupying position
    procedure SetCurrentGroup(aGroup: TKMUnitGroup);
    procedure SetGroupType(const Value: TGroupType);
    procedure SetDefenceType(const Value: TAIDefencePosType);
    procedure SetPosition(const Value: TKMPointDir);
  public
    constructor Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;

    property DefenceType: TAIDefencePosType read fDefenceType write SetDefenceType;
    property GroupType: TGroupType read fGroupType write SetGroupType; //Type of group to defend this position (e.g. melee)
    property Position: TKMPointDir read fPosition write SetPosition; //Position and direction the group defending will stand
    property Radius: Integer read fRadius write fRadius; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved

    property CurrentGroup: TKMUnitGroup read fCurrentGroup write SetCurrentGroup;
    function CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
  end;


  TAIDefencePositions = class
  private
    fPositions: TKMList;
    function GetPosition(aIndex: Integer): TAIDefencePosition; inline;
    function GetCount: Integer; inline;
  public
    //Defines how defending troops will be formatted. 0 means leave unchanged.
    TroopFormations: array [TGroupType] of TKMFormation;

    constructor Create;
    destructor Destroy; override;

    procedure Add(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
    procedure Clear;
    property Count: Integer read GetCount;
    procedure Delete(aIndex: Integer);
    property Positions[aIndex: Integer]: TAIDefencePosition read GetPosition; default;
    function GetBacklineCount: Integer;
    function AverageUnitsPerGroup: Integer;

    function FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;
    procedure RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
    function FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;
    procedure GetArmyDemand(out aFootmen: Integer; out aPikemen: Integer; out aHorsemen: Integer; out aArchers: Integer);

    procedure MoveUp(aIndex: Integer);
    procedure MoveDown(aIndex: Integer);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses
  Math,
  KM_Game, KM_HandsCollection, KM_RenderAux;


{ TAIDefencePosition }
constructor TAIDefencePosition.Create(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
begin
  inherited Create;
  fPosition := aPos;
  fGroupType := aGroupType;
  fRadius := aRadius;
  fDefenceType := aDefenceType;
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
  gHands.CleanUpGroupPointer(fCurrentGroup);

  //Take new one
  if aGroup <> nil then
    fCurrentGroup := aGroup.GetGroupPointer;
end;


procedure TAIDefencePosition.SetDefenceType(const Value: TAIDefencePosType);
begin
  Assert(gGame.IsMapEditor);
  fDefenceType := Value;
end;


procedure TAIDefencePosition.SetGroupType(const Value: TGroupType);
begin
  Assert(gGame.IsMapEditor);
  fGroupType := Value;
end;


procedure TAIDefencePosition.SetPosition(const Value: TKMPointDir);
begin
  Assert(gGame.IsMapEditor);
  fPosition := Value;
end;


procedure TAIDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fPosition);
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  SaveStream.Write(fRadius);
  SaveStream.Write(fDefenceType, SizeOf(fDefenceType));
  if fCurrentGroup <> nil then
    SaveStream.Write(fCurrentGroup.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TAIDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fPosition);
  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fRadius);
  LoadStream.Read(fDefenceType, SizeOf(fDefenceType));
  LoadStream.Read(fCurrentGroup, 4); //subst on syncload
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentGroup := gHands.GetGroupByUID(Cardinal(fCurrentGroup));
end;


function TAIDefencePosition.CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
begin
  Result := (fGroupType = UnitGroups[aGroup.UnitType]);

  if not Result then Exit;

  //Empty position accepts anything (e.g. happens at mission start)
  Result := (CurrentGroup = nil) or
            (aGroup.Count = 1);
  //@Lewin: Plz check me here, KaM did not linked big groups to filled Positions?
  //@Krom: That's correct. To be deleted

  Result := Result and ((CurrentGroup = nil) or (CurrentGroup.Count < aMaxUnits));
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
  and CurrentGroup.IsIdleToAI
  and CurrentGroup.CanWalkTo(Position.Loc, 0) then
    CurrentGroup.OrderWalk(Position.Loc, True, Position.Dir);
end;


{ TAIDefencePositions }
constructor TAIDefencePositions.Create;
var
  GT: TGroupType;
begin
  inherited Create;

  fPositions := TKMList.Create;

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    TroopFormations[GT].NumUnits := 9; //These are the defaults in KaM
    TroopFormations[GT].UnitsPerRow := 3;
  end;
end;


destructor TAIDefencePositions.Destroy;
begin
  fPositions.Free;

  inherited;
end;


function TAIDefencePositions.GetCount: Integer;
begin
  Result := fPositions.Count;
end;


function TAIDefencePositions.GetPosition(aIndex: Integer): TAIDefencePosition;
begin
  Result := fPositions[aIndex];
end;


procedure TAIDefencePositions.Add(aPos: TKMPointDir; aGroupType: TGroupType; aRadius: Integer; aDefenceType: TAIDefencePosType);
begin
  fPositions.Add(TAIDefencePosition.Create(aPos, aGroupType, aRadius, aDefenceType));
end;


procedure TAIDefencePositions.Clear;
begin
  fPositions.Clear;
end;


procedure TAIDefencePositions.Delete(aIndex: Integer);
begin
  //Note: Order of positions is important (AI fills them with troops from 0 to N)
  fPositions.Delete(aIndex);
end;


function TAIDefencePositions.GetBacklineCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Positions[I].fDefenceType = adt_BackLine then
      Inc(Result);
end;


function TAIDefencePositions.AverageUnitsPerGroup: Integer;
var GT: TGroupType; TypeCount: Integer;
begin
  Result := 0;
  TypeCount := 0;
  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    Result := Result + TroopFormations[GT].NumUnits;
    Inc(TypeCount);
  end;
  Result := Result div TypeCount;
end;


function TAIDefencePositions.FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;
var
  I, Matched: Integer;
  Distance, Best: Single;
begin
  Result := False;
  Matched := -1;
  Best := MaxSingle;

  //Try to link to existing group
  for I := 0 to Count - 1 do
  if Positions[I].CanAccept(aGroup, TroopFormations[aGroup.GroupType].NumUnits) then
  begin
    //Take closest position that is empty or requries restocking
    Distance := KMLengthSqr(aGroup.Position, Positions[I].Position.Loc);
    if Distance < Best then
    begin
      Matched := I;
      Best := Distance;
      //KaM fills defence positions by their order of creation
      //Take first one we find - that's what KaM does
      if not aTakeClosest then Break;
    end;
  end;

  if Matched <> -1 then
  begin
    Result := True;
    if Positions[Matched].CurrentGroup = nil then
    begin
      //New position
      Positions[Matched].CurrentGroup := aGroup;
      if aGroup.UnitsPerRow < TroopFormations[aGroup.GroupType].UnitsPerRow then
        aGroup.UnitsPerRow := TroopFormations[aGroup.GroupType].UnitsPerRow;
      aGroup.OrderWalk(Positions[Matched].Position.Loc, True);
    end
    else
      //Append to existing position
      RestockPositionWith(Positions[Matched].CurrentGroup, aGroup);
  end;
end;


procedure TAIDefencePositions.RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
var
  Needed: integer;
begin
  Needed := TroopFormations[aDefenceGroup.GroupType].NumUnits - aDefenceGroup.Count;
  if Needed <= 0 then exit;
  if aGroup.Count <= Needed then
    aGroup.OrderLinkTo(aDefenceGroup, True) //Link entire group
  else
    aGroup.OrderSplitLinkTo(aDefenceGroup, Needed, True); //Link only as many units as are needed

  if aDefenceGroup.UnitsPerRow < TroopFormations[aDefenceGroup.GroupType].UnitsPerRow then
    aDefenceGroup.UnitsPerRow := TroopFormations[aDefenceGroup.GroupType].UnitsPerRow;
end;


//Find DefencePosition to which this Commander belongs
//(Result could be nil if CommanderCount > PositionsCount
function TAIDefencePositions.FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  if Positions[I].CurrentGroup = aGroup then
  begin
    Result := Positions[I];
    Break;
  end;
end;


procedure TAIDefencePositions.GetArmyDemand(out aFootmen: Integer; out aPikemen: Integer; out aHorsemen: Integer; out aArchers: Integer);
var I, Required: Integer;
begin
  aFootmen := 0;
  aPikemen := 0;
  aHorsemen := 0;
  aArchers := 0;
  for I := 0 to Count - 1 do
  begin
    Required := TroopFormations[Positions[I].GroupType].NumUnits;
    case Positions[I].GroupType of
      gt_Melee:     Inc(aFootmen, Required);
      gt_AntiHorse: Inc(aPikemen, Required);
      gt_Ranged:    Inc(aArchers, Required);
      gt_Mounted:   Inc(aHorsemen, Required);
    end;
  end;
end;


procedure TAIDefencePositions.MoveUp(aIndex: Integer);
begin
  if aIndex < Count-1 then
    fPositions.Exchange(aIndex, aIndex+1);
end;


procedure TAIDefencePositions.MoveDown(aIndex: Integer);
begin
  if aIndex > 0 then
    fPositions.Exchange(aIndex, aIndex-1);
end;


procedure TAIDefencePositions.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.WriteA('DefencePositions');
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(Count);

  for I := 0 to Count - 1 do
    Positions[I].Save(SaveStream);
end;


procedure TAIDefencePositions.Load(LoadStream: TKMemoryStream);
var I, NewCount: Integer;
begin
  LoadStream.ReadAssert('DefencePositions');
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(NewCount);

  for I := 0 to NewCount - 1 do
    fPositions.Add(TAIDefencePosition.Load(LoadStream));
end;


procedure TAIDefencePositions.SyncLoad;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Positions[I].SyncLoad;
end;


procedure TAIDefencePositions.UpdateState;
var I,K: Integer;
begin
  //Make sure no defence position Group is dead
  for I := 0 to Count - 1 do
    Positions[I].UpdateState;

  //In KaM the order of defence positions is the priority: The first defined is higher priority
  for I := 0 to Count - 1 do
  if (Positions[I].CurrentGroup = nil) then
    for K := I + 1 to Count - 1 do
    if Positions[I].GroupType = Positions[K].GroupType then
    begin
      Positions[I].CurrentGroup := Positions[K].CurrentGroup; //Take new position
      Positions[K].CurrentGroup := nil; //Leave current position
      Break;
    end;
end;


procedure TAIDefencePositions.Paint;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    if Positions[I].fDefenceType = adt_FrontLine then
      gRenderAux.Quad(Positions[I].fPosition.Loc.X, Positions[I].fPosition.Loc.Y, $FFFF0000)
    else
      gRenderAux.Quad(Positions[I].fPosition.Loc.X, Positions[I].fPosition.Loc.Y, $FF00FF00)
end;


end.
