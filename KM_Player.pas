unit KM_Player;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_ArmyEvaluation, KM_BuildList, KM_DeliverQueue, KM_FogOfWar,
  KM_Goals, KM_Houses, KM_Terrain, KM_PlayerAI, KM_PlayerStats, KM_Units;


type
  TPlayerType = (
        pt_Human,
        pt_Computer);

  TKMPlayerCommon = class
  private
    fPlayerIndex: TPlayerIndex; //Which ID this player is
    fUnits: TKMUnitsCollection;
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;
    property PlayerIndex: TPlayerIndex read fPlayerIndex;
    property Units: TKMUnitsCollection read fUnits;

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace: Boolean=true): TKMUnit;
    procedure RemUnit(Position: TKMPoint);
    function UnitsHitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    procedure UpdateState(aUpdateAI: Boolean); virtual;
    procedure Paint; virtual;
  end;


  TKMPlayer = class(TKMPlayerCommon)
  private
    fAI: TKMPlayerAI;
    fBuildList: TKMBuildList; //Not the best name for buildingManagement
    fDeliverList: TKMDeliverQueue;
    fHouses: TKMHousesCollection;
    fRoadsList: TKMPointList; //Used only once to speedup mission loading, then freed
    fStats: TKMPlayerStats;
    fGoals: TKMGoals;
    fFogOfWar: TKMFogOfWar; //Stores FOW info for current player, which includes
    fArmyEval: TKMArmyEvaluation; // Can used by all players

    fPlayerName: string;
    fPlayerType: TPlayerType;
    fFlagColor: Cardinal;
    fCenterScreen: TKMPoint;
    fAlliances: array[0..MAX_PLAYERS-1] of TAllianceType;

    fSkipWinConditionCheck: Boolean;
    fSkipDefeatConditionCheck: Boolean;

    function GetColorIndex: Byte;

    function  GetAlliances(aIndex: Integer): TAllianceType;
    procedure SetAlliances(aIndex: Integer; aValue: TAllianceType);
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property AI: TKMPlayerAI read fAI;
    property BuildList: TKMBuildList read fBuildList;
    property DeliverList: TKMDeliverQueue read fDeliverList;
    property Houses: TKMHousesCollection read fHouses;
    property Stats: TKMPlayerStats read fStats;
    property Goals: TKMGoals read fGoals;
    property FogOfWar: TKMFogOfWar read fFogOfWar;
    property ArmyEval: TKMArmyEvaluation read fArmyEval;

    procedure SetPlayerID(aNewIndex: TPlayerIndex);
    property PlayerName: string read fPlayerName write fPlayerName;
    property PlayerType: TPlayerType read fPlayerType write fPlayerType; //Is it Human or AI
    property FlagColor: Cardinal read fFlagColor write fFlagColor;
    property FlagColorIndex: Byte read GetColorIndex;
    property Alliances[aIndex: Integer]: TAllianceType read GetAlliances write SetAlliances;
    property CenterScreen: TKMPoint read fCenterScreen write fCenterScreen;

    procedure AfterMissionInit(aFlattenRoads: Boolean);
    procedure SkipWinConditionCheck;
    procedure SkipDefeatConditionCheck;

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace: Boolean=true; WasTrained: Boolean = False): TKMUnit; reintroduce;
    procedure AddUnitAndLink(aUnitType: TUnitType; Position: TKMPoint);
    function AddUnitGroup(aUnitType: TUnitType; Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor: Boolean = False): TKMUnit;

    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
    procedure TrainingDone(aUnit: TKMUnit);

    function CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
    function CanAddFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
    function CanAddHousePlan(aLoc: TKMPoint; aHouseType: THouseType): Boolean;

    function AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace: Boolean): TKMHouse;
    procedure AddRoadToList(aLoc: TKMPoint);
    //procedure AddRoadConnect(LocA,LocB: TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType: TFieldType);
    procedure ToggleFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType; aMakeSound:Boolean);
    procedure ToggleFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
    procedure AddHouseWIP(aHouseType: THouseType; aLoc: TKMPoint; out House: TKMHouse);
    procedure RemHouse(Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
    procedure RemHousePlan(Position: TKMPoint);
    procedure RemFieldPlan(Position: TKMPoint; aMakeSound:Boolean);
    procedure RemFakeFieldPlan(Position: TKMPoint);
    function FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean = False): TKMHouseInn;
    function FindHouse(aType: THouseType; aPosition: TKMPoint; Index: Byte=1): TKMHouse; overload;
    function FindHouse(aType: THouseType; Index: Byte=1): TKMHouse; overload;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    procedure GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);

    function GetFieldsCount: Integer;
    procedure GetFieldPlans(aList: TKMPointTagList; aRect: TKMRect; aIncludeFake:Boolean);
    procedure GetPlansBorders(aList: TKMPointDirList; aRect: TKMRect);
    procedure GetPlansTablets(aList: TKMPointTagList; aRect: TKMRect);

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure IncAnimStep;
    procedure UpdateState(aUpdateAI: Boolean); override;
    procedure Paint; override;
  end;


  TKMPlayerAnimals = class (TKMPlayerCommon)
  public
    function GetFishInWaterBody(aWaterID: Byte; FindHighestCount: Boolean=True): TKMUnitAnimal;
  end;


implementation
uses KM_PlayersCollection, KM_Resource, KM_ResourceHouse, KM_Sound,
  KM_Units_Warrior;


{ TKMPlayerCommon }
constructor TKMPlayerCommon.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;
  fPlayerIndex  := aPlayerIndex;
  fUnits        := TKMUnitsCollection.Create;
end;


destructor TKMPlayerCommon.Destroy;
begin
  FreeThenNil(fUnits);
  inherited;
end;


function TKMPlayerCommon.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace: Boolean=true): TKMUnit;
begin
  Result := fUnits.Add(fPlayerIndex, aUnitType, Position.X, Position.Y, AutoPlace);
end;


procedure TKMPlayerCommon.Paint;
begin
  fUnits.Paint;
end;


procedure TKMPlayerCommon.RemUnit(Position: TKMPoint);
var U: TKMUnit;
begin
  U := fUnits.HitTest(Position.X, Position.Y);
  if U <> nil then
    fUnits.RemoveUnit(U);
end;


procedure TKMPlayerCommon.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('PlayerCommon');
  fUnits.Save(SaveStream);
end;


procedure TKMPlayerCommon.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerCommon');
  fUnits.Load(LoadStream);
end;


procedure TKMPlayerCommon.SyncLoad;
begin
  fUnits.SyncLoad;
end;


function TKMPlayerCommon.UnitsHitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;
begin
  Result:= fUnits.HitTest(X, Y, UT);
end;


procedure TKMPlayerCommon.UpdateState;
begin
  fUnits.UpdateState;
end;


{ TKMPlayer }
constructor TKMPlayer.Create(aPlayerIndex: TPlayerIndex);
var I: Integer;
begin
  inherited Create(aPlayerIndex);
  fAI           := TKMPlayerAI.Create(fPlayerIndex);
  fFogOfWar     := TKMFogOfWar.Create(fTerrain.MapX, fTerrain.MapY);
  fGoals        := TKMGoals.Create;
  fStats        := TKMPlayerStats.Create;
  fRoadsList    := TKMPointList.Create;
  fHouses       := TKMHousesCollection.Create;
  fDeliverList  := TKMDeliverQueue.Create;
  fBuildList    := TKMBuildList.Create;
  fArmyEval     := TKMArmyEvaluation.Create(Self);

  fPlayerName   := 'Player ' + IntToStr(aPlayerIndex);
  fPlayerType   := pt_Computer;
  for I := 0 to MAX_PLAYERS - 1 do
    fAlliances[I] := at_Enemy; //Everyone is enemy by default

  fSkipWinConditionCheck := False;
  fSkipDefeatConditionCheck := False;
  fFlagColor := DefaultTeamColors[fPlayerIndex]; //Init with default color, later replaced by Script
end;


//Destruction order is important as Houses and Units need to access
//Stats/Deliveries and other collection in their Destroy/Abandon/Demolish methods
destructor TKMPlayer.Destroy;
begin
  inherited; //Free fUnits first
  FreeThenNil(fArmyEval);
  FreeThenNil(fRoadsList);
  FreeThenNil(fHouses);

  //Should be freed after Houses and Units, as they write Stats on Destroy
  FreeThenNil(fStats);
  FreeThenNil(fGoals);
  FreeThenNil(fFogOfWar);
  FreeThenNil(fDeliverList);
  FreeThenNil(fBuildList);
  FreeThenNil(fAI);
end;


//Add unit of aUnitType to Position
//AutoPlace - add unit to nearest available spot if Position is already taken (or unwalkable)
//WasTrained - the uniot was trained by player and therefor counted by Stats
function TKMPlayer.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace: Boolean=true; WasTrained: Boolean=false): TKMUnit;
begin
  Result := Inherited AddUnit(aUnitType, Position, AutoPlace);

  if Result <> nil then
  begin
    if aUnitType = ut_Worker then
      fBuildList.AddWorker(TKMUnitWorker(Result));

    fStats.UnitCreated(aUnitType, WasTrained);
  end;
end;


//Add the unit and link it to closest group
procedure TKMPlayer.AddUnitAndLink(aUnitType: TUnitType; Position: TKMPoint);
var
  U: TKMUnit;
  W: TKMUnitWarrior;
begin
  U := AddUnit(aUnitType, Position);

  if (U <> nil) and (U is TKMUnitWarrior) then
  begin
    W := TKMUnitWarrior(U).FindLinkUnit(U.GetPosition);
    if W <> nil then
      TKMUnitWarrior(U).OrderLinkTo(W);
  end;
end;


//Start training unit in school/barracks
//User can cancel the training, so we don't add unit to stats just yet
function TKMPlayer.TrainUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
begin
  Result := fUnits.Add(fPlayerIndex, aUnitType, Position.X, Position.Y, false);
  //Do not add unit to statistic just yet, wait till it's training complete
end;


procedure TKMPlayer.TrainingDone(aUnit: TKMUnit);
begin
  if aUnit.UnitType = ut_Worker then
    fBuildList.AddWorker(TKMUnitWorker(aUnit));

  fStats.UnitCreated(aUnit.UnitType, True);
end;


function TKMPlayer.AddUnitGroup(aUnitType: TUnitType; Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor: Boolean=false): TKMUnit;
begin
  Result := fUnits.AddGroup(fPlayerIndex, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount, aMapEditor);
  //Add unit to statistic inside the function for some units may not fit on map
end;


function TKMPlayer.AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace: Boolean): TKMHouse;
begin
  Result := fHouses.AddHouse(aHouseType, PosX, PosY, fPlayerIndex, RelativeEntrace);
end;


//When adding roads from script we want to batch them all into one list to save time
//on WalkConnect and other calculations
procedure TKMPlayer.AddRoadToList(aLoc: TKMPoint);
begin
  Assert(fRoadsList <> nil);
  fRoadsList.AddEntry(aLoc);
end;


//Lay out all roads at once to save time on Terrain lighting/passability recalculations
procedure TKMPlayer.AfterMissionInit(aFlattenRoads: Boolean);
var I: Integer;
begin
  if fRoadsList <> nil then
  begin
    fTerrain.SetRoads(fRoadsList, fPlayerIndex);
    if aFlattenRoads then
      fTerrain.FlattenTerrain(fRoadsList);
    FreeAndNil(fRoadsList);
  end;

  for I := 0 to fPlayers.Count - 1 do
    if fPlayerIndex <> I then
      fArmyEval.AddEnemy(fPlayers[I]);
end;


procedure TKMPlayer.SetPlayerID(aNewIndex: TPlayerIndex);
begin
  fPlayerIndex := aNewIndex;
  fUnits.OwnerUpdate(aNewIndex);
  fHouses.OwnerUpdate(aNewIndex);
  fAI.OwnerUpdate(aNewIndex);
end;


procedure TKMPlayer.AddField(aLoc: TKMPoint; aFieldType: TFieldType);
begin
  fTerrain.SetField(aLoc, fPlayerIndex, aFieldType);
end;


//See comment on CanAddFakeFieldPlan
function TKMPlayer.CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
var I: Integer;
begin
  Result := fTerrain.CanAddField(aLoc, aFieldType)
            and (fFogOfWar.CheckTileRevelation(aLoc.X, aLoc.Y, False) > 0)
            and (fBuildList.FieldworksList.HasField(aLoc) = ft_None)
            and not fBuildList.HousePlanList.HasPlan(aLoc);
  //Don't allow placing on allies plans either
  if Result then
    for I := 0 to fPlayers.Count - 1 do
      if (I <> fPlayerIndex) and (fPlayers.CheckAlliance(fPlayerIndex, I) = at_Ally) then
        Result := (fPlayers[i].fBuildList.FieldworksList.HasField(aLoc) = ft_None)
                   and not fPlayers[i].fBuildList.HousePlanList.HasPlan(aLoc);
end;


//This differs from above only in that it uses HasFakeField instead of HasField.
//We need it because the user expects to be blocked by fake field plans, but the gameplay should not.
//When the result effects the outcome of the game, the above function should be used instead.
function TKMPlayer.CanAddFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
var I: Integer;
begin
  Result := fTerrain.CanAddField(aLoc, aFieldType)
            and (fFogOfWar.CheckTileRevelation(aLoc.X, aLoc.Y, False) > 0)
            and (fBuildList.FieldworksList.HasFakeField(aLoc) = ft_None)
            and not fBuildList.HousePlanList.HasPlan(aLoc);
  //Don't allow placing on allies plans either
  if Result then
    for I := 0 to fPlayers.Count - 1 do
      if (I <> fPlayerIndex) and (fPlayers.CheckAlliance(fPlayerIndex, I) = at_Ally) then
        Result := (fPlayers[i].fBuildList.FieldworksList.HasField(aLoc) = ft_None)
                   and not fPlayers[i].fBuildList.HousePlanList.HasPlan(aLoc);
end;


function TKMPlayer.CanAddHousePlan(aLoc: TKMPoint; aHouseType: THouseType): Boolean;
var i,k,j,s,t,tx,ty:integer; HA:THouseArea;
begin
  Result := fTerrain.CanPlaceHouse(aLoc, aHouseType);
  if not Result then Exit;

  HA := fResource.HouseDat[aHouseType].BuildArea;
  for i:=1 to 4 do
  for k:=1 to 4 do
  if HA[i,k] <> 0 then
  begin
    tx := aLoc.X - fResource.HouseDat[aHouseType].EntranceOffsetX + k - 3;
    ty := aLoc.Y + i - 4;
    Result := Result and fTerrain.TileInMapCoords(tx, ty, 1)
                     and (fFogOfWar.CheckTileRevelation(tx, ty, false) > 0);
    //This checks below require tx;ty to be within the map so exit immediately if they are not
    if not Result then exit;

    //This tile must not contain fields/houses of allied players or self
    for j := 0 to fPlayers.Count - 1 do
      if (j = fPlayerIndex) or (fPlayers.CheckAlliance(fPlayerIndex, j) = at_Ally) then
      begin
        Result := Result and (fPlayers[j].fBuildList.FieldworksList.HasField(KMPoint(tx,ty)) = ft_None);
        //Surrounding tiles must not be a house
        for s:=-1 to 1 do
          for t:=-1 to 1 do
            Result := Result and not fPlayers[j].fBuildList.HousePlanList.HasPlan(KMPoint(tx+s,ty+t));
      end;
  end;
end;


//Due to lag there could be already plans placed by user in previous ticks
//Check if Plan can be placed once again, as we might have conflicting commands caused by lag
//This is called by GIP when a place field command is processed
procedure TKMPlayer.ToggleFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType; aMakeSound:Boolean);
var Plan: TFieldType;
begin
  Assert(aFieldType in [ft_Road, ft_Corn, ft_Wine, ft_Wall], 'Placing wrong FieldType');

  Plan := fBuildList.FieldworksList.HasField(aLoc);
  if aFieldType = Plan then //Same plan - remove it
    RemFieldPlan(aLoc,aMakeSound)
  else
    if CanAddFieldPlan(aLoc, aFieldType) then
    begin
      if aMakeSound and (Self = MyPlayer) then fSoundLib.Play(sfx_placemarker);
      fBuildList.FieldworksList.AddField(aLoc, aFieldType)
    end
    else
    begin
      if aMakeSound and (Self = MyPlayer) then fSoundLib.Play(sfx_CantPlace, 4.0);
      if Plan = ft_None then //If we can't build because there's some other plan, that's ok
      begin
        //Can't build here anymore because something changed between click and command processing, so remove any fake plans
        fBuildList.FieldworksList.RemFakeField(aLoc);
        fBuildList.FieldworksList.RemFakeDeletedField(aLoc);
      end;
    end;
end;


//This procedure does not effect gameplay, it only changes fake field plans to make it look better for the user
//It is called when the user clicks to place a field plan
procedure TKMPlayer.ToggleFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
var Plan: TFieldType;
begin
  Assert(aFieldType in [ft_Road, ft_Corn, ft_Wine, ft_Wall], 'Placing wrong fake FieldType');

  Plan := fBuildList.FieldworksList.HasFakeField(aLoc);
  if aFieldType = Plan then //Same plan - remove it
  begin
    fBuildList.FieldworksList.RemFakeField(aLoc); //Remove our fake marker which is shown to the user
    fBuildList.FieldworksList.AddFakeDeletedField(aLoc); //This will hide the real field until it is deleted from game
    if Self = MyPlayer then fSoundLib.Play(sfx_Click);
  end
  else
    if CanAddFakeFieldPlan(aLoc, aFieldType) then
    begin
      fBuildList.FieldworksList.AddFakeField(aLoc, aFieldType);
      if Self = MyPlayer then
        fSoundLib.Play(sfx_placemarker);
    end
    else
      if Self = MyPlayer then
        fSoundLib.Play(sfx_CantPlace, 4.0);
end;


//Used mainly for testing purposes
{procedure TKMPlayer.AddRoadConnect(LocA,LocB: TKMPoint);
var
  NodeList: TKMPointList;
  RoadExists: Boolean;
  I: Integer;
begin
  NodeList := TKMPointList.Create;
  try
    RoadExists := fTerrain.PathFinding.Route_Make(LocA, LocB, CanMakeRoads, 0, nil, NodeList);

    if RoadExists then
      for I := 1 to NodeList.Count do
        AddField(NodeList.List[i], ft_Road);
  finally
    FreeAndNil(NodeList);
  end;
end;}


procedure TKMPlayer.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
var
  Loc: TKMPoint;
begin
  Loc.X := aLoc.X - fResource.HouseDat[aHouseType].EntranceOffsetX;
  Loc.Y := aLoc.Y;

  fBuildList.HousePlanList.AddPlan(aHouseType, Loc);
  fStats.HousePlanned(aHouseType);
  if Self = MyPlayer then fSoundLib.Play(sfx_placemarker);
end;


procedure TKMPlayer.AddHouseWIP(aHouseType: THouseType; aLoc: TKMPoint; out House: TKMHouse);
begin
  House := fHouses.AddHouseWIP(aHouseType, aLoc.X, aLoc.Y, fPlayerIndex);
  fStats.HouseStarted(aHouseType);
end;


//Player wants to remove own house
procedure TKMPlayer.RemHouse(Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
var H: TKMHouse;
begin
  if not DoSilent then fSoundLib.Play(sfx_Click);

  H := fHouses.HitTest(Position.X, Position.Y);
  if H = nil then Exit; //Due to network delays the house might have already been destroyed by now

  H.DemolishHouse(DoSilent, IsEditor);
  if H.BuildingState = hbs_Done then //Only Done houses are treated as Self-Destruct
    fStats.HouseSelfDestruct(H.HouseType)
  else
    fStats.HouseEnded(H.HouseType);
end;


procedure TKMPlayer.RemHousePlan(Position: TKMPoint);
var
  HT: THouseType;
begin
  HT := fBuildList.HousePlanList.GetPlan(Position);
  if not fResource.HouseDat[HT].IsValid then exit; //Due to network delays house might not exist now
  fBuildList.HousePlanList.RemPlan(Position);
  fStats.HousePlanRemoved(HT);
  if Self = MyPlayer then fSoundLib.Play(sfx_Click);
end;


//This is called by the GIP when an erase command is processed
procedure TKMPlayer.RemFieldPlan(Position: TKMPoint; aMakeSound:Boolean);
begin
  fBuildList.FieldworksList.RemFieldPlan(Position);
  if aMakeSound and (Self = MyPlayer) then fSoundLib.Play(sfx_Click);
end;


//This is called immediately when the user clicks erase on a field plan.
//We know that an erase command is queued and will be processed in some ticks,
//so we AddFakeDeletedField which lets the user think the field was removed,
//while the game does not know the difference.
procedure TKMPlayer.RemFakeFieldPlan(Position: TKMPoint);
begin
  fBuildList.FieldworksList.RemFakeField(Position); //Remove our fake marker which is shown to the user
  fBuildList.FieldworksList.AddFakeDeletedField(Position); //This will hide the real field until it is deleted from game
  if Self = MyPlayer then fSoundLib.Play(sfx_Click);
end;


function TKMPlayer.FindHouse(aType: THouseType; aPosition: TKMPoint; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;


function TKMPlayer.FindHouse(aType: THouseType; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, 0, 0, Index);
end;


function TKMPlayer.FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean=false): TKMHouseInn;
var
  H: TKMHouseInn;
  I: Integer;
  Dist, BestMatch: Single;
begin
  //This function will return the best inn for a unit at Loc, base on distance, food available and space available.
  //Will return nil if no suitable inn is available
  Result := nil;
  I := 1;
  BestMatch := 9999;
  if UnitIsAtHome then inc(Loc.Y); //From outside the door of the house

  H := TKMHouseInn(FindHouse(ht_Inn));
  repeat
    //First make sure that it is valid
    if (H <> nil) and H.HasFood and H.HasSpace
    and aUnit.CanWalkTo(Loc, KMPointBelow(H.GetEntrance), CanWalk, 0) then
    begin
      //Take the closest inn out of the ones that are suitable
      Dist := GetLength(H.GetPosition, Loc);
      if Dist < BestMatch then
      begin
        Result := H;
        BestMatch := Dist;
      end;
    end;

    inc(I);
    H := TKMHouseInn(FindHouse(ht_Inn, I));
  until(H = nil);
end;


function TKMPlayer.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;


function TKMPlayer.GetColorIndex: Byte;
var i: Integer;
begin
  Result := 3; //3 = Black which can be the default when a non-palette 32 bit color value is used
  for i:=0 to 255 do
    if fResource.Palettes.DefDal.Color32(i) = fFlagColor then
      Result := i;
end;


function  TKMPlayer.GetAlliances(aIndex: Integer): TAllianceType;
begin
  Result := fAlliances[aIndex];
end;


procedure TKMPlayer.SetAlliances(aIndex: Integer; aValue: TAllianceType);
begin
  fAlliances[aIndex] := aValue;
end;


{ See if player owns any Fields/Roads/Walls (has any assets on Terrain)
  Queried by MapEditor>SaveDAT;
  Might also be used to show Players strength (or builder/warrior balance) in Tavern
  If Player has none and no Units/Houses we can assume it's empty and does not needs to be saved }
function TKMPlayer.GetFieldsCount: Integer;
var i,k: Integer;
begin
  Result := 0;
  for i := 1 to fTerrain.MapY do
  for k := 1 to fTerrain.MapX do
    if fTerrain.Land[i,k].TileOwner = fPlayerIndex then
      inc(Result);
end;


procedure TKMPlayer.GetFieldPlans(aList: TKMPointTagList; aRect: TKMRect; aIncludeFake:Boolean);
var
  I: TPlayerIndex;
begin
  fBuildList.FieldworksList.GetFields(aList, aRect, aIncludeFake);

  for I := 0 to fPlayers.Count - 1 do
    if (I <> fPlayerIndex) and (fPlayers.CheckAlliance(fPlayerIndex, I) = at_Ally) then
      fPlayers[I].BuildList.FieldworksList.GetFields(aList, aRect, aIncludeFake);
end;


procedure TKMPlayer.GetPlansBorders(aList: TKMPointDirList; aRect: TKMRect);
var
  I: TPlayerIndex;
begin
  fBuildList.HousePlanList.GetBorders(aList, aRect);

  for I := 0 to fPlayers.Count - 1 do
    if (I <> fPlayerIndex) and (fPlayers.CheckAlliance(fPlayerIndex, I) = at_Ally) then
      fPlayers[I].BuildList.HousePlanList.GetBorders(aList, aRect);
end;


procedure TKMPlayer.GetPlansTablets(aList: TKMPointTagList; aRect: TKMRect);
var
  I: TPlayerIndex;
begin
  fBuildList.HousePlanList.GetTablets(aList, aRect);

  for I := 0 to fPlayers.Count - 1 do
    if (I <> fPlayerIndex) and (fPlayers.CheckAlliance(fPlayerIndex, I) = at_Ally) then
      fPlayers[I].BuildList.HousePlanList.GetTablets(aList, aRect);
end;


procedure TKMPlayer.GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);
var
  i,k,j,s,t: Integer;
  P2: TKMPoint;
  AllowBuild: Boolean;
  HA: THouseArea;

  //Replace existing icon with a Block
  procedure BlockPoint(aPoint: TKMPoint; aID: Integer);
  var v: integer; Replaced: Boolean;
  begin
    Replaced := False;
    for v := aList.Count downto 1 do
      if KMSamePoint(aList[v], aPoint) then
      begin
        if Replaced then
          aList.RemoveEntry(aPoint)
        else
          aList.Tag[v] := aID; //Replace existing Mark with a blocker
        Replaced := True;
        //Keep on replacing since entrance has 2 entries in the list
      end;

    //Otherwise add a blocker
    if not Replaced then
      aList.AddEntry(aPoint, aID, 0);
  end;
begin
  //Get basic Marks
  fTerrain.GetHouseMarks(aLoc, aHouseType, aList);

  //Override marks if there are House/FieldPlans (only we know about our plans)
  //and or FogOfWar
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for i:=1 to 4 do for k:=1 to 4 do
  if (HA[i,k] <> 0)
  and fTerrain.TileInMapCoords(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX,aLoc.Y+i-4,1) then
  begin
    //This can't be done earlier since values can be off-map
    P2 := KMPoint(aLoc.X+k-3-fResource.HouseDat[aHouseType].EntranceOffsetX, aLoc.Y+i-4);

    //Forbid planning on unrevealed areas and fieldplans
    AllowBuild := (fFogOfWar.CheckTileRevelation(P2.X, P2.Y, False) > 0);

    //This tile must not contain fields/houses of allied players or self
    for j := 0 to fPlayers.Count - 1 do
      if (j = fPlayerIndex) or (fPlayers.CheckAlliance(fPlayerIndex, j) = at_Ally) then
        AllowBuild := AllowBuild and (fPlayers[j].fBuildList.FieldworksList.HasField(P2) = ft_None)
                                 and not fPlayers[j].fBuildList.HousePlanList.HasPlan(P2);

    //Check surrounding tiles in +/- 1 range for other houses pressence
    for s:=-1 to 1 do for t:=-1 to 1 do
      if (s<>0) or (t<>0) then //This is a surrounding tile, not the actual tile
        for j := 0 to fPlayers.Count - 1 do
          if ((j = fPlayerIndex) or (fPlayers.CheckAlliance(fPlayerIndex, j) = at_Ally))
          and fPlayers[j].fBuildList.HousePlanList.HasPlan(KMPoint(P2.X+s,P2.Y+t)) then
          begin
            BlockPoint(KMPoint(P2.X+s,P2.Y+t), 479); //Block surrounding points
            AllowBuild := False;
          end;

    //Mark the tile according to previous check results
    if not AllowBuild then
      if HA[i,k] = 2 then
        BlockPoint(P2, 482)
      else
        if aHouseType in [ht_GoldMine, ht_IronMine] then
          BlockPoint(P2, 480)
        else
          BlockPoint(P2, 479);
  end;
end;


procedure TKMPlayer.SkipWinConditionCheck;
begin
  fSkipWinConditionCheck := true;
end;


procedure TKMPlayer.SkipDefeatConditionCheck;
begin
  fSkipDefeatConditionCheck := true;
end;


procedure TKMPlayer.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  fAI.Save(SaveStream);
  fBuildList.Save(SaveStream);
  fDeliverList.Save(SaveStream);
  fFogOfWar.Save(SaveStream);
  fGoals.Save(SaveStream);
  fHouses.Save(SaveStream);
  fStats.Save(SaveStream);

  SaveStream.Write(fPlayerIndex);
  SaveStream.Write(fPlayerType, SizeOf(fPlayerType));
  SaveStream.Write(fAlliances, SizeOf(fAlliances));
  SaveStream.Write(fCenterScreen);
  SaveStream.Write(fSkipWinConditionCheck);
  SaveStream.Write(fSkipDefeatConditionCheck);
  SaveStream.Write(fFlagColor);
end;


procedure TKMPlayer.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  fAI.Load(LoadStream);
  fBuildList.Load(LoadStream);
  fDeliverList.Load(LoadStream);
  fFogOfWar.Load(LoadStream);
  fGoals.Load(LoadStream);
  fHouses.Load(LoadStream);
  fStats.Load(LoadStream);

  LoadStream.Read(fPlayerIndex);
  LoadStream.Read(fPlayerType, SizeOf(fPlayerType));
  LoadStream.Read(fAlliances, SizeOf(fAlliances));
  LoadStream.Read(fCenterScreen);
  LoadStream.Read(fSkipWinConditionCheck);
  LoadStream.Read(fSkipDefeatConditionCheck);
  LoadStream.Read(fFlagColor);
end;


procedure TKMPlayer.SyncLoad;
begin
  inherited;
  fHouses.SyncLoad;
  fDeliverList.SyncLoad;
  fBuildList.SyncLoad;
  fAI.SyncLoad;
end;


procedure TKMPlayer.IncAnimStep;
begin
  fHouses.IncAnimStep;
end;


//aUpdateAI flag means we can perform CPU intensive AI update
procedure TKMPlayer.UpdateState(aUpdateAI: Boolean);
begin
  Inherited;

  fHouses.UpdateState;
  fFogOfWar.UpdateState; //We might optimize it for AI somehow, to make it work coarse and faster

  fBuildList.UpdateState; //todo: Make it less frequent

  if aUpdateAI then
  begin
    fAI.UpdateState;
    //fArmyEval.UpdateState;
  end;
end;


procedure TKMPlayer.Paint;
begin
  inherited;
  fHouses.Paint;
end;


{ TKMPlayerAnimals }
function TKMPlayerAnimals.GetFishInWaterBody(aWaterID: Byte; FindHighestCount: Boolean=True): TKMUnitAnimal;
var
  i, HighestGroupCount: Integer;
  U: TKMUnit;
begin
  Result := nil;
  if aWaterID = 0 then Exit; //Fish should always be in valid water
  HighestGroupCount := 0;

  for i:=0 to fUnits.Count-1 do
  begin
    U := fUnits[i]; //Store locally

    if (U <> nil)
    and (U.UnitType = ut_Fish)
    and (not U.IsDeadOrDying) //Fish are killed when they are caught or become stuck
    and (fTerrain.Land[U.GetPosition.Y, U.GetPosition.X].WalkConnect[wcFish] = aWaterID)
    and (TKMUnitAnimal(U).FishCount > HighestGroupCount) then
    begin
      Result := TKMUnitAnimal(U);
      //This is for time saving when we don't actually care which group is returned
      if not FindHighestCount then Exit;
      HighestGroupCount := Result.FishCount;
    end;
  end;
end;


end.
