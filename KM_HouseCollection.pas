unit KM_HouseCollection;
{$I KaM_Remake.inc}
interface
uses
   Math,
   KM_CommonClasses, KM_Defaults, KM_Points,
   KM_ResHouses, KM_Houses, KM_Terrain;

type
  TKMHousesCollection = class
  private
    fHouses: TKMList; //Private to hide methods we don't want to expose
    function AddToCollection(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; aHBS: THouseBuildState):TKMHouse;
    function GetHouse(aIndex: Integer): TKMHouse;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddHouse(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; RelativeEntrance: Boolean):TKMHouse;
    function AddHouseWIP(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex): TKMHouse;
    property Count: Integer read GetCount;
    procedure OwnerUpdate(aOwner: TPlayerIndex);
    property Houses[aIndex: Integer]: TKMHouse read GetHouse; default;
    function HitTest(X, Y: Integer): TKMHouse;
    function GetHouseByID(aID: Integer): TKMHouse;
    function FindEmptyHouse(aUnitType: TUnitType; Loc: TKMPoint): TKMHouse;
    function FindHouse(aType: THouseType; X,Y: Word; const aIndex: Byte = 1; aOnlyCompleted: Boolean = True): TKMHouse;
    function GetTotalPointers: Cardinal;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;
    procedure UpdateResRequest; //Change resource requested counts for all houses
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_HouseBarracks, KM_HouseMarket, KM_Game, KM_Resource;


{ TKMHousesCollection }
constructor TKMHousesCollection.Create;
begin
  inherited;
  fHouses := TKMList.Create;
end;


destructor TKMHousesCollection.Destroy;
begin
  fHouses.Free;
  inherited;
end;


function TKMHousesCollection.AddToCollection(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; aHBS: THouseBuildState): TKMHouse;
var ID: Cardinal;
begin
  ID := fGame.GetNewID;

  case aHouseType of
    ht_Swine,
    ht_Stables:     Result := TKMHouseSwineStable.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Inn:         Result := TKMHouseInn.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Marketplace: Result := TKMHouseMarket.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_School:      Result := TKMHouseSchool.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Barracks:    Result := TKMHouseBarracks.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Store:       Result := TKMHouseStore.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_WatchTower:  Result := TKMHouseTower.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    ht_Woodcutters: Result := TKMHouseWoodcutters.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
    else            Result := TKMHouse.Create(ID, aHouseType,PosX,PosY, aOwner, aHBS);
  end;

  if Result <> nil then
    fHouses.Add(Result);
end;


function TKMHousesCollection.GetCount: Integer;
begin
  Result := fHouses.Count;
end;


function TKMHousesCollection.GetHouse(aIndex: Integer): TKMHouse;
begin
  Result := fHouses[aIndex];
end;


function TKMHousesCollection.AddHouse(aHouseType: THouseType; PosX,PosY: Integer; aOwner: TPlayerIndex; RelativeEntrance: Boolean):TKMHouse;
begin
  if RelativeEntrance then
    Result := AddToCollection(aHouseType, PosX - fResource.HouseDat[aHouseType].EntranceOffsetX, PosY, aOwner, hbs_Done)
  else
    Result := AddToCollection(aHouseType, PosX, PosY, aOwner, hbs_Done);
end;


{Add a plan for house}
function TKMHousesCollection.AddHouseWIP(aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex): TKMHouse;
begin
  Result := AddToCollection(aHouseType, PosX, PosY, aOwner, hbs_NoGlyph);
end;


procedure TKMHousesCollection.OwnerUpdate(aOwner: TPlayerIndex);
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Houses[I].OwnerUpdate(aOwner);
end;


function TKMHousesCollection.HitTest(X, Y: Integer): TKMHouse;
var I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if Houses[I].HitTest(X, Y) and (not Houses[I].IsDestroyed) then
    begin
      Result := Houses[I];
      Break;
    end;
end;


function TKMHousesCollection.GetHouseByID(aID: Integer): TKMHouse;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if aID = Houses[I].ID then
    begin
      Result := Houses[I];
      exit;
    end;
end;


//Should find closest house to Loc
function TKMHousesCollection.FindEmptyHouse(aUnitType: TUnitType; Loc: TKMPoint): TKMHouse;
var
  I: Integer;
  Dist, BestBid: Single;
begin
  Result := nil;
  BestBid := MaxSingle;

  for I := 0 to Count - 1 do
    if (fResource.HouseDat[Houses[I].HouseType].OwnerType = aUnitType) and //If Unit can work in here
       (not Houses[I].GetHasOwner) and                              //If there's yet no owner
       (not Houses[I].IsDestroyed) and
       (Houses[I].IsComplete) then                               //If house is built
    begin
      //Recruits should not go to a barracks with ware delivery switched off
      if (Houses[I].HouseType = ht_Barracks) and (not Houses[I].WareDelivery) then Continue;
      if not gTerrain.Route_CanBeMade(Loc, KMPointBelow(Houses[I].GetEntrance), canWalk, 0) then Continue;

      Dist := KMLengthSqr(Loc, Houses[I].GetPosition);

      //Always prefer Towers to Barracks by making Barracks Bid much less attractive
      //In case of multiple barracks, prefer the closer one (players should make multiple schools or use WareDelivery to control it)
      if Houses[I].HouseType = ht_Barracks then
        Dist := Dist * 1000;

      if Dist < BestBid then
      begin
        BestBid := Dist;
        Result := Houses[I];
      end;

    end;

  if (Result <> nil) and (Result.HouseType <> ht_Barracks) then
    Result.GetHasOwner := True; //Become owner except Barracks;
end;


//Find closest house to given position
//or
//Find house by index (1st, 2nd)
function TKMHousesCollection.FindHouse(aType: THouseType; X, Y: word; const aIndex: Byte = 1; aOnlyCompleted: Boolean = True): TKMHouse;
var
  I,ID: Integer;
  UsePosition: Boolean;
  BestMatch,Dist: Single;
begin
  Result := nil;
  ID := 0;
  BestMatch := MaxSingle; //Any distance will be closer than that
  UsePosition := X*Y <> 0; //Calculate this once to save computing lots of multiplications
  Assert((not UsePosition) or (aIndex = 1), 'Can''t find house basing both on Position and Index');

  for I := 0 to Count - 1 do
  if ((Houses[I].HouseType = aType) or (aType = ht_Any))
  and (Houses[I].IsComplete or not aOnlyCompleted)
  and not Houses[I].IsDestroyed then
  begin
    Inc(ID);
    if UsePosition then
    begin
      Dist := KMLengthSqr(Houses[I].GetPosition,KMPoint(X,Y));
      if BestMatch = -1 then BestMatch := Dist; //Initialize for first use
      if Dist < BestMatch then
      begin
        BestMatch := Dist;
        Result := Houses[I];
      end;
    end
    else
      //Take the N-th result
      if aIndex = ID then
      begin
        Result := Houses[I];
        Exit;
      end;
  end;
end;


procedure TKMHousesCollection.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('Houses');

  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
  begin
    //We save house type to know which house class to load
    SaveStream.Write(Houses[I].HouseType, SizeOf(Houses[I].HouseType));
    Houses[I].Save(SaveStream);
  end;
end;


procedure TKMHousesCollection.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
  HouseType: THouseType;
  T: TKMHouse;
begin
  LoadStream.ReadAssert('Houses');

  LoadStream.Read(NewCount);
  for I := 0 to NewCount - 1 do
  begin
    LoadStream.Read(HouseType, SizeOf(HouseType));
    case HouseType of
      ht_Swine,
      ht_Stables:     T := TKMHouseSwineStable.Load(LoadStream);
      ht_Inn:         T := TKMHouseInn.Load(LoadStream);
      ht_Marketplace: T := TKMHouseMarket.Load(LoadStream);
      ht_School:      T := TKMHouseSchool.Load(LoadStream);
      ht_Barracks:    T := TKMHouseBarracks.Load(LoadStream);
      ht_Store:       T := TKMHouseStore.Load(LoadStream);
      ht_WatchTower:  T := TKMHouseTower.Load(LoadStream);
      ht_Woodcutters: T := TKMHouseWoodcutters.Load(LoadStream);
      else            T := TKMHouse.Load(LoadStream);
    end;

    if T <> nil then
      fHouses.Add(T);
  end;
end;


procedure TKMHousesCollection.SyncLoad;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Houses[I].SyncLoad;
end;


//Update resource requested counts for all houses
procedure TKMHousesCollection.UpdateResRequest;
var I: Integer;
begin
  for I := 0 to Count - 1 do
  if Houses[I].IsComplete and not Houses[I].IsDestroyed then
    Houses[I].UpdateResRequest;
end;


procedure TKMHousesCollection.UpdateState;
var
  I: Integer;
begin
  for I := Count - 1 downto 0  do
    if not Houses[I].IsDestroyed then
      Houses[I].UpdateState
    else
      if FREE_POINTERS and (Houses[I].PointerCount = 0) then
        fHouses.Delete(I); //Because no one needs this anymore it must DIE!!!!! :D
end;


procedure TKMHousesCollection.IncAnimStep;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Houses[I].IncAnimStep;
end;


function TKMHousesCollection.GetTotalPointers: Cardinal;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Houses[I].PointerCount;
end;


procedure TKMHousesCollection.Paint;
const Margin = 3;
var
  I: Integer;
  Rect: TKMRect;
begin
  //Compensate for big houses near borders or standing on hills
  Rect := KMRectGrow(fGame.Viewport.GetClip, Margin);

  for I := 0 to Count - 1 do
  if not Houses[I].IsDestroyed and KMInRect(Houses[I].GetPosition, Rect) then
    Houses[I].Paint;
end;


end.
