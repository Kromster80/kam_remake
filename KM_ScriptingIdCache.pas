unit KM_ScriptingIdCache;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses, KM_Units, KM_UnitGroups;


//For caching unit/house/group IDs. Shared between States and Actions.
//Because scripts runs the same on every computer (i.e. no access to MySpectator)
//we can safely use pointers within the cache
const
  CACHE_SIZE = 16; //Too big means caching becomes slow

type
  TKMScriptingIdCache = class
  private
    //3 separate bins used because we need to access class-specific fields (IsDead)

    //@Lewin: We could rework this in two ways:
    //1. Keep 3 buckets, but replace them with instances of single class which
    //   will cache Integer/Pointer pairs, and keep pointer type in var
    //2. We could further adjoin everything into 1 bucket since IDs are unique among all objects
    //   but that could affect performance a bit
    //But for that we would need to introduce common ancestor to Units/Houses/Groups
    //that will have IsDead and maybe few other methods (after Release). What do you think?

    //We employ circular buffers and store only position in buffer
    fUnitLastAdded: Byte;
    fUnitCache: array [0..CACHE_SIZE-1] of record ID: Integer; U: TKMUnit; end;
    fHouseLastAdded: Byte;
    fHouseCache: array[0..CACHE_SIZE-1] of record ID: Integer; H: TKMHouse; end;
    fGroupLastAdded: Byte;
    fGroupCache: array[0..CACHE_SIZE-1] of record ID: Integer; G: TKMUnitGroup; end;
  public
    procedure CacheUnit(aUnit: TKMUnit; aID: Integer);
    procedure CacheHouse(aHouse: TKMHouse; aID: Integer);
    procedure CacheGroup(aGroup: TKMUnitGroup; aID: Integer);
    function GetUnit(aID:Integer): TKMUnit;
    function GetHouse(aID:Integer): TKMHouse;
    function GetGroup(aID:Integer): TKMUnitGroup;
    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_PlayersCollection;


{ TKMScriptingIdCache }
procedure TKMScriptingIdCache.CacheUnit(aUnit: TKMUnit; aID: Integer);
var I: ShortInt;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
  if fUnitCache[I].ID = aID then
    Exit; //Already in cache

  //We need to release pointer if we remove unit from cache
  if fUnitCache[fUnitLastAdded].U <> nil then
    fPlayers.CleanUpUnitPointer(fUnitCache[fUnitLastAdded].U);

  fUnitCache[fUnitLastAdded].ID := aID;
  //We could be asked to cache that certain ID is nil (saves us time scanning Units to find out that this Id is removed)
  if aUnit <> nil then
    fUnitCache[fUnitLastAdded].U := aUnit.GetUnitPointer
  else
    fUnitCache[fUnitLastAdded].U := nil;

  fUnitLastAdded := (fUnitLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheHouse(aHouse: TKMHouse; aID: Integer);
var I: ShortInt;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
  if fHouseCache[i].ID = aID then
    Exit; //Already in cache

  //We need to release pointer if we remove house from cache
  if fHouseCache[fHouseLastAdded].H <> nil then
    fPlayers.CleanUpHousePointer(fHouseCache[fHouseLastAdded].H);

  fHouseCache[fHouseLastAdded].ID := aID;
  //We could be asked to cache that certain ID is nil (saves us time scanning Houses to find out that this Id is removed)
  if aHouse <> nil then
    fHouseCache[fHouseLastAdded].H := aHouse.GetHousePointer
  else
    fHouseCache[fHouseLastAdded].H := nil;

  fHouseLastAdded := (fHouseLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheGroup(aGroup: TKMUnitGroup; aID: Integer);
var I: ShortInt;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
  if fGroupCache[I].ID = aID then
    Exit; //Already in cache

  //We need to release pointer if we remove group from cache
  if fGroupCache[fGroupLastAdded].G <> nil then
    fPlayers.CleanUpGroupPointer(fGroupCache[fGroupLastAdded].G);

  fGroupCache[fGroupLastAdded].ID := aID;
  //We could be asked to cache that certain ID is nil (saves us time scanning Groups to find out that this Id is removed)
  if aGroup <> nil then
    fGroupCache[fGroupLastAdded].G := aGroup.GetGroupPointer
  else
    fGroupCache[fGroupLastAdded].G := nil;

  fGroupLastAdded := (fGroupLastAdded + 1) mod CACHE_SIZE;
end;


function TKMScriptingIdCache.GetUnit(aID: Integer): TKMUnit;
var I: ShortInt;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
  if fUnitCache[I].ID = aID then
  begin
    Result := fUnitCache[I].U;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := fPlayers.GetUnitByID(aID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  CacheUnit(Result, aID);
end;


function TKMScriptingIdCache.GetHouse(aID:Integer): TKMHouse;
var I: ShortInt;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
  if fHouseCache[I].ID = aID then
  begin
    Result := fHouseCache[I].H;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := fPlayers.GetHouseByID(aID);
  if (Result <> nil) and Result.IsDestroyed then
    Result := nil;

  CacheHouse(Result, aID);
end;


function TKMScriptingIdCache.GetGroup(aID:Integer): TKMUnitGroup;
var I: ShortInt;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
  if fGroupCache[I].ID = aID then
  begin
    Result := fGroupCache[I].G;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := fPlayers.GetGroupByID(aID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  CacheGroup(Result, aID);
end;


procedure TKMScriptingIdCache.UpdateState;
var
  I: ShortInt;
begin
  //Clear out dead IDs every now and again
  //Leave them in the cache as nils, because we still might need to lookup that ID
  if fGame.GameTickCount mod 11 = 0 then
  begin
    for I := Low(fUnitCache) to High(fUnitCache) do
      if (fUnitCache[I].U <> nil) and fUnitCache[I].U.IsDead then
        fPlayers.CleanUpUnitPointer(fUnitCache[I].U);

    for I := Low(fHouseCache) to High(fHouseCache) do
      if (fHouseCache[I].H <> nil) and fHouseCache[I].H.IsDestroyed then
        fPlayers.CleanUpHousePointer(fHouseCache[I].H);

    for I := Low(fGroupCache) to High(fGroupCache) do
      if (fGroupCache[I].G <> nil) and fGroupCache[I].G.IsDead then
        fPlayers.CleanUpGroupPointer(fGroupCache[I].G);
  end;
end;


end.
