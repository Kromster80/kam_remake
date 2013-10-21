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
    //We employ circular buffers and store only position in buffer
    fUnitLastAdded: Byte;
    fUnitCache: array [0..CACHE_SIZE-1] of record UID: Integer; U: TKMUnit; end;
    fHouseLastAdded: Byte;
    fHouseCache: array[0..CACHE_SIZE-1] of record UID: Integer; H: TKMHouse; end;
    fGroupLastAdded: Byte;
    fGroupCache: array[0..CACHE_SIZE-1] of record UID: Integer; G: TKMUnitGroup; end;
  public
    procedure CacheUnit(aUnit: TKMUnit; aUID: Integer);
    procedure CacheHouse(aHouse: TKMHouse; aUID: Integer);
    procedure CacheGroup(aGroup: TKMUnitGroup; aUID: Integer);
    function GetUnit(aUID: Integer): TKMUnit;
    function GetHouse(aUID: Integer): TKMHouse;
    function GetGroup(aUID: Integer): TKMUnitGroup;
    procedure UpdateState;
  end;


implementation
uses KM_Game, KM_HandsCollection;


{ TKMScriptingIdCache }
procedure TKMScriptingIdCache.CacheUnit(aUnit: TKMUnit; aUID: Integer);
var I: ShortInt;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
  if fUnitCache[I].UID = aUID then
    Exit; //Already in cache

  //We need to release pointer if we remove unit from cache
  if fUnitCache[fUnitLastAdded].U <> nil then
    gHands.CleanUpUnitPointer(fUnitCache[fUnitLastAdded].U);

  fUnitCache[fUnitLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Units to find out that this UID is removed)
  if aUnit <> nil then
    fUnitCache[fUnitLastAdded].U := aUnit.GetUnitPointer
  else
    fUnitCache[fUnitLastAdded].U := nil;

  fUnitLastAdded := (fUnitLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheHouse(aHouse: TKMHouse; aUID: Integer);
var I: ShortInt;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
  if fHouseCache[i].UID = aUID then
    Exit; //Already in cache

  //We need to release pointer if we remove house from cache
  if fHouseCache[fHouseLastAdded].H <> nil then
    gHands.CleanUpHousePointer(fHouseCache[fHouseLastAdded].H);

  fHouseCache[fHouseLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Houses to find out that this UID is removed)
  if aHouse <> nil then
    fHouseCache[fHouseLastAdded].H := aHouse.GetHousePointer
  else
    fHouseCache[fHouseLastAdded].H := nil;

  fHouseLastAdded := (fHouseLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheGroup(aGroup: TKMUnitGroup; aUID: Integer);
var I: ShortInt;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
  if fGroupCache[I].UID = aUID then
    Exit; //Already in cache

  //We need to release pointer if we remove group from cache
  if fGroupCache[fGroupLastAdded].G <> nil then
    gHands.CleanUpGroupPointer(fGroupCache[fGroupLastAdded].G);

  fGroupCache[fGroupLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Groups to find out that this UID is removed)
  if aGroup <> nil then
    fGroupCache[fGroupLastAdded].G := aGroup.GetGroupPointer
  else
    fGroupCache[fGroupLastAdded].G := nil;

  fGroupLastAdded := (fGroupLastAdded + 1) mod CACHE_SIZE;
end;


function TKMScriptingIdCache.GetUnit(aUID: Integer): TKMUnit;
var I: ShortInt;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
  if fUnitCache[I].UID = aUID then
  begin
    Result := fUnitCache[I].U;
    if (Result <> nil) and Result.IsDeadOrDying then
      Result := nil;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetUnitByUID(aUID);
  if (Result <> nil) and Result.IsDeadOrDying then
    Result := nil;

  CacheUnit(Result, aUID);
end;


function TKMScriptingIdCache.GetHouse(aUID:Integer): TKMHouse;
var I: ShortInt;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
  if fHouseCache[I].UID = aUID then
  begin
    Result := fHouseCache[I].H;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetHouseByUID(aUID);
  if (Result <> nil) and Result.IsDestroyed then
    Result := nil;

  CacheHouse(Result, aUID);
end;


function TKMScriptingIdCache.GetGroup(aUID: Integer): TKMUnitGroup;
var I: ShortInt;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
  if fGroupCache[I].UID = aUID then
  begin
    Result := fGroupCache[I].G;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetGroupByUID(aUID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  CacheGroup(Result, aUID);
end;


procedure TKMScriptingIdCache.UpdateState;
var
  I: ShortInt;
begin
  //Clear out dead IDs every now and again
  //Leave them in the cache as nils, because we still might need to lookup that UID
  if gGame.GameTickCount mod 11 = 0 then
  begin
    for I := Low(fUnitCache) to High(fUnitCache) do
      if (fUnitCache[I].U <> nil) and fUnitCache[I].U.IsDeadOrDying then
        gHands.CleanUpUnitPointer(fUnitCache[I].U);

    for I := Low(fHouseCache) to High(fHouseCache) do
      if (fHouseCache[I].H <> nil) and fHouseCache[I].H.IsDestroyed then
        gHands.CleanUpHousePointer(fHouseCache[I].H);

    for I := Low(fGroupCache) to High(fGroupCache) do
      if (fGroupCache[I].G <> nil) and fGroupCache[I].G.IsDead then
        gHands.CleanUpGroupPointer(fGroupCache[I].G);
  end;
end;


end.
