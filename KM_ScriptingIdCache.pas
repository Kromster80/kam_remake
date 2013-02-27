unit KM_ScriptingIdCache;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses, KM_Units, KM_UnitGroups;


//For caching unit/house/group IDs. Shared between States and Actions.
//Because scripts runs the same on every computer (i.e. no access to MyPlayer)
//we can safely use pointers within the cache
const
  CACHE_SIZE = 16; //Too big means caching becomes slow

type
  TKMScriptingIdCache = class
  private
    fUnitCount: Byte;
    fUnitLastAdded: Byte;
    fUnitCache: array[0..CACHE_SIZE-1] of record
                                            ID: Integer;
                                            U: TKMUnit;
                                          end;
    fHouseCount: Byte;
    fHouseLastAdded: Byte;
    fHouseCache: array[0..CACHE_SIZE-1] of record
                                             ID: Integer;
                                             H: TKMHouse;
                                           end;
    fGroupCount: Byte;
    fGroupLastAdded: Byte;
    fGroupCache: array[0..CACHE_SIZE-1] of record
                                             ID: Integer;
                                             G: TKMUnitGroup;
                                           end;

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
var I, NewItem: Shortint;
begin
  for I := 0 to fUnitCount - 1 do
    if fUnitCache[i].ID = aID then
      Exit; //Already in cache

  if fUnitCount < CACHE_SIZE then
  begin
    NewItem := fUnitCount;
    Inc(fUnitCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fUnitLastAdded;
    Inc(fUnitLastAdded);
    if fUnitLastAdded = CACHE_SIZE then fUnitLastAdded := 0;
  end;

  fUnitCache[NewItem].ID := aID;
  if aUnit <> nil then
    fUnitCache[NewItem].U := aUnit.GetUnitPointer
  else
    fUnitCache[NewItem].U := nil;
end;


procedure TKMScriptingIdCache.CacheHouse(aHouse: TKMHouse; aID: Integer);
var I, NewItem: Shortint;
begin
  for I := 0 to fHouseCount - 1 do
    if fHouseCache[i].ID = aID then
      Exit; //Already in cache

  if fHouseCount < CACHE_SIZE then
  begin
    NewItem := fHouseCount;
    Inc(fHouseCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fHouseLastAdded;
    Inc(fHouseLastAdded);
    if fHouseLastAdded = CACHE_SIZE then fHouseLastAdded := 0;
  end;

  fHouseCache[NewItem].ID := aID;
  if aHouse <> nil then
    fHouseCache[NewItem].H := aHouse.GetHousePointer
  else
    fHouseCache[NewItem].H := nil;
end;


procedure TKMScriptingIdCache.CacheGroup(aGroup: TKMUnitGroup; aID: Integer);
var I, NewItem: Shortint;
begin
  for I := 0 to fGroupCount - 1 do
    if fGroupCache[i].ID = aID then
      Exit; //Already in cache

  if fGroupCount < CACHE_SIZE then
  begin
    NewItem := fGroupCount;
    Inc(fGroupCount);
  end
  else
  begin
    //Cache full, overwrite the oldest item
    NewItem := fGroupLastAdded;
    Inc(fGroupLastAdded);
    if fGroupLastAdded = CACHE_SIZE then fGroupLastAdded := 0;
  end;

  fGroupCache[NewItem].ID := aID;
  if aGroup <> nil then
    fGroupCache[NewItem].G := aGroup.GetGroupPointer
  else
    fGroupCache[NewItem].G := nil;
end;


function TKMScriptingIdCache.GetUnit(aID:Integer): TKMUnit;
var I: Shortint;
begin
  for I := 0 to fUnitCount - 1 do
    if fUnitCache[i].ID = aID then
    begin
      Result := fUnitCache[i].U;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  Result := fPlayers.GetUnitByID(aID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  CacheUnit(Result, aID);
end;


function TKMScriptingIdCache.GetHouse(aID:Integer): TKMHouse;
var I: Shortint;
begin
  for I := 0 to fHouseCount - 1 do
    if fHouseCache[i].ID = aID then
    begin
      Result := fHouseCache[i].H;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  Result := fPlayers.GetHouseByID(aID);
  if (Result <> nil) and Result.IsDestroyed then
    Result := nil;

  CacheHouse(Result, aID);
end;


function TKMScriptingIdCache.GetGroup(aID:Integer): TKMUnitGroup;
var I: Shortint;
begin
  for I := 0 to fGroupCount - 1 do
    if fGroupCache[i].ID = aID then
    begin
      Result := fGroupCache[i].G;
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
  I: Integer;
begin
  //Clear out dead IDs every now and again
  //Leave them in the cache as nils, because we still might need to lookup that ID
  if fGame.GameTickCount mod 11 = 0 then
  begin
    for I := 0 to fUnitCount - 1 do
      if (fUnitCache[I].U <> nil) and fUnitCache[I].U.IsDead then
        fPlayers.CleanUpUnitPointer(fUnitCache[I].U);

    for I := 0 to fHouseCount - 1 do
      if (fHouseCache[I].H <> nil) and fHouseCache[I].H.IsDestroyed then
        fPlayers.CleanUpHousePointer(fHouseCache[I].H);

    for I := 0 to fGroupCount - 1 do
      if (fGroupCache[I].G <> nil) and fGroupCache[I].G.IsDead then
        fPlayers.CleanUpGroupPointer(fGroupCache[I].G);
  end;
end;


end.
