unit KM_UnitsCollection;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, Types,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Terrain, KM_Units;

//Memo on directives:
//Dynamic - declared and used (overriden) occasionally
//Virtual - declared and used (overriden) always
//Abstract - declared but must be overriden in child classes

type
  TKMUnitsCollection = class
  private
    fUnits: TKMList;
    function GetUnit(aIndex: Integer): TKMUnit; inline;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddUnit(aOwner: TKMHandIndex; aUnitType: TUnitType; aLoc: TKMPoint; aAutoPlace: boolean = True; aRequiredWalkConnect: Byte = 0): TKMUnit;
    procedure AddUnitToList(aUnit: TKMUnit);
    property Count: Integer read GetCount;
    property Units[aIndex: Integer]: TKMUnit read GetUnit; default; //Use instead of Items[.]
    procedure RemoveUnit(aUnit: TKMUnit);
    procedure DeleteUnitFromList(aUnit: TKMUnit);
    procedure OwnerUpdate(aOwner: TKMHandIndex);
    function HitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;
    function GetUnitByUID(aUID: Integer): TKMUnit;
    function GetClosestUnit(aPoint: TKMPoint; aTypes: TUnitTypeSet = [Low(TUnitType)..High(TUnitType)]): TKMUnit;
    procedure GetUnitsInRect(aRect: TKMRect; List: TList);
    function GetTotalPointers: Integer;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint(aRect: TKMRect);
  end;


implementation
uses
  KM_Game, KM_HandsCollection, KM_Log, KM_Resource, KM_ResUnits, KM_Units_Warrior;


{ TKMUnitsCollection }
constructor TKMUnitsCollection.Create;
begin
  inherited Create;

  fUnits := TKMList.Create;
end;


destructor TKMUnitsCollection.Destroy;
begin
  //No need to free units individually since they are Freed by TKMList.Clear command in destructor
  fUnits.Free;
  inherited;
end;


function TKMUnitsCollection.GetCount: Integer;
begin
  Result := fUnits.Count;
end;


function TKMUnitsCollection.GetUnit(aIndex: Integer): TKMUnit;
begin
  Result := fUnits[aIndex];
end;


//AutoPlace means we should try to find a spot for this unit instead of just placing it where we were told to
function TKMUnitsCollection.AddUnit(aOwner: TKMHandIndex; aUnitType: TUnitType; aLoc: TKMPoint; aAutoPlace: Boolean = True; aRequiredWalkConnect: Byte = 0): TKMUnit;
var
  ID: Cardinal;
  PlaceTo: TKMPoint;
begin
  if aAutoPlace then
  begin
    PlaceTo := KMPOINT_ZERO; // Will have 0:0 if no place found
    if aRequiredWalkConnect = 0 then
      aRequiredWalkConnect := gTerrain.GetWalkConnectID(aLoc);
    gHands.FindPlaceForUnit(aLoc.X, aLoc.Y, aUnitType, PlaceTo, aRequiredWalkConnect);
  end
  else
    PlaceTo := aLoc;

  //Check if Pos is within map coords first, as other checks rely on this
  if not gTerrain.TileInMapCoords(PlaceTo.X, PlaceTo.Y) then
  begin
    gLog.AddTime('Unable to add unit to ' + KM_Points.TypeToString(PlaceTo));
    Result := nil;
    Exit;
  end;

  if gTerrain.HasUnit(PlaceTo) then
    raise ELocError.Create('No space for ' + gRes.Units[aUnitType].GUIName +
                           ' at ' + TypeToString(aLoc) +
                           ', tile is already occupied by ' + gRes.Units[TKMUnit(gTerrain.Land[PlaceTo.Y,PlaceTo.X].IsUnit).UnitType].GUIName,
                           PlaceTo);

  ID := gGame.GetNewUID;
  case aUnitType of
    ut_Serf:                          Result := TKMUnitSerf.Create(ID, aUnitType, PlaceTo, aOwner);
    ut_Worker:                        Result := TKMUnitWorker.Create(ID, aUnitType, PlaceTo, aOwner);
    ut_WoodCutter..ut_Fisher,
    {ut_Worker,}
    ut_StoneCutter..ut_Metallurgist:  Result := TKMUnitCitizen.Create(ID, aUnitType, PlaceTo, aOwner);
    ut_Recruit:                       Result := TKMUnitRecruit.Create(ID, aUnitType, PlaceTo, aOwner);
    WARRIOR_MIN..WARRIOR_MAX:         Result := TKMUnitWarrior.Create(ID, aUnitType, PlaceTo, aOwner);
    ANIMAL_MIN..ANIMAL_MAX:           Result := TKMUnitAnimal.Create(ID, aUnitType, PlaceTo, aOwner);
    else                              raise ELocError.Create('Add ' + gRes.Units[aUnitType].GUIName, PlaceTo);
  end;

  if Result <> nil then
    fUnits.Add(Result);
end;

procedure TKMUnitsCollection.AddUnitToList(aUnit: TKMUnit);
begin
  Assert(gGame.GameMode = gmMapEd); // Allow to add existing Unit directly only in MapEd
  if aUnit <> nil then
    fUnits.Add(aUnit);
end;


procedure TKMUnitsCollection.RemoveUnit(aUnit: TKMUnit);
begin
  aUnit.CloseUnit; //Should free up the unit properly (freeing terrain usage and memory)
  fUnits.Remove(aUnit); //Will free the unit
end;


procedure TKMUnitsCollection.DeleteUnitFromList(aUnit: TKMUnit);
begin
  Assert(gGame.GameMode = gmMapEd); // Allow to delete existing Unit directly only in MapEd
  if (aUnit <> nil) then
    fUnits.Extract(aUnit);  // use Extract instead of Delete, cause Delete nils inner objects somehow
end;


procedure TKMUnitsCollection.OwnerUpdate(aOwner: TKMHandIndex);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Units[I].SetOwner(aOwner);
end;


function TKMUnitsCollection.HitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Units[I].HitTest(X,Y,UT) and not Units[I].IsDead then
    begin
      Result := Units[I];
      Exit;
    end;
end;


function TKMUnitsCollection.GetUnitByUID(aUID: Integer): TKMUnit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if aUID = Units[I].UID then
    begin
      Result := Units[I];
      exit;
    end;
end;


procedure TKMUnitsCollection.GetUnitsInRect(aRect: TKMRect; List: TList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  if KMInRect(Units[I].PositionF, aRect) and not Units[I].IsDeadOrDying then
    List.Add(Units[I]);
end;


function TKMUnitsCollection.GetClosestUnit(aPoint: TKMPoint; aTypes: TUnitTypeSet = [Low(TUnitType)..High(TUnitType)]): TKMUnit;
var
  I: Integer;
  BestDist, Dist: Single;
begin
  Result := nil;
  BestDist := MaxSingle; //Any distance will be closer than that
  for I := 0 to Count - 1 do
    if not Units[I].IsDeadOrDying and Units[I].Visible and (Units[I].UnitType in aTypes) then
    begin
      Dist := KMLengthSqr(Units[I].GetPosition, aPoint);
      if Dist < BestDist then
      begin
        BestDist := Dist;
        Result := Units[I];
      end;
    end;
end;


function TKMUnitsCollection.GetTotalPointers: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Units[I].GetPointerCount);
end;


procedure TKMUnitsCollection.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.WriteA('Units');
  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
  begin
    //We save unit type to know which unit class to load
    SaveStream.Write(Units[I].UnitType, SizeOf(Units[I].UnitType));
    Units[I].Save(SaveStream);
  end;
end;


procedure TKMUnitsCollection.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
  UnitType: TUnitType;
  U: TKMUnit;
begin
  LoadStream.ReadAssert('Units');
  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(UnitType, SizeOf(UnitType));
    case UnitType of
      ut_Serf:                  U := TKMUnitSerf.Load(LoadStream);
      ut_Worker:                U := TKMUnitWorker.Load(LoadStream);
      ut_WoodCutter..ut_Fisher,{ut_Worker,}ut_StoneCutter..ut_Metallurgist:
                                U := TKMUnitCitizen.Load(LoadStream);
      ut_Recruit:               U := TKMUnitRecruit.Load(LoadStream);
      WARRIOR_MIN..WARRIOR_MAX: U := TKMUnitWarrior.Load(LoadStream);
      ANIMAL_MIN..ANIMAL_MAX:   U := TKMUnitAnimal.Load(LoadStream);
      else                      U := nil;
    end;

    if U <> nil then
      fUnits.Add(U)
    else
      gLog.AddAssert('Unknown unit type in Savegame');
  end;
end;


procedure TKMUnitsCollection.SyncLoad;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Units[I].SyncLoad;
end;


procedure TKMUnitsCollection.UpdateState;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not Units[I].IsDead then
      Units[I].UpdateState
    else
      if FREE_POINTERS and (Units[I].GetPointerCount = 0) then
        fUnits.Delete(I);

  //   --     POINTER FREEING SYSTEM - DESCRIPTION     --   //
  //  This system was implemented because unit and house objects cannot be freed until all pointers
  //  to them (in tasks, delivery queue, etc.) have been freed, otherwise we have pointer integrity
  //  issues.

  //   --     ROUGH OUTLINE     --   //
  // - Units and houses have fPointerCount, which is the number of pointers to them. (e.g. tasks,
  //   deliveries) This is kept up to date by the thing that is using the pointer. On create it uses
  //   GetUnitPointer to get the pointer and increase the pointer count and on destroy it decreases
  //   it with ReleaseUnitPointer.
  // - When a unit dies, the object is not destroyed. Instead a flag (boolean) is set to say that we
  //   want to destroy but can't because there still might be pointers to the unit. From then on
  //   every update state it checks to see if the pointer count is 0 yet. If it is then the unit is
  //   destroyed.
  // - For each place that contains a pointer, it should check everytime the pointer is used to see
  //   if it has been destroy. If it has then we free the pointer and reduce the count.
  //   (and do any other action nececary due to the unit/house dying)
end;


procedure TKMUnitsCollection.Paint(aRect: TKMRect);
const
  Margin = 2;
var
  I: Integer;
  growRect: TKMRect;
begin
  //Add additional margin to compensate for units height
  growRect := KMRectGrow(aRect, Margin);

  for I := 0 to Count - 1 do
  if (Units[I] <> nil) and not Units[I].IsDead and KMInRect(Units[I].PositionF, growRect) then
    Units[I].Paint;
end;


end.
