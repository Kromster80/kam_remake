unit KM_Hand;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_ArmyEvaluation, KM_BuildList, KM_DeliverQueue, KM_FogOfWar,
  KM_HouseCollection, KM_Houses, KM_Terrain, KM_AI, KM_HandStats, KM_Units, KM_UnitsCollection, KM_Units_Warrior, KM_UnitGroups,
  KM_ResHouses;


type
  THandType = (
        hndHuman,
        hndComputer);

  //Player manages its assets
  TKMHandCommon = class
  private
    fHandIndex: THandIndex; //Index of this hand in gHands
    fUnits: TKMUnitsCollection;
  public
    constructor Create(aHandIndex: THandIndex);
    destructor Destroy; override;
    property HandIndex: THandIndex read fHandIndex;
    property Units: TKMUnitsCollection read fUnits;

    function AddUnit(aUnitType: TUnitType; aLoc: TKMPoint): TKMUnit;
    procedure RemUnit(Position: TKMPoint);
    function UnitsHitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    procedure UpdateState(aTick: Cardinal); virtual;
    procedure Paint(aRect: TKMRect); virtual;
  end;


  TKMHand = class(TKMHandCommon)
  private
    fAI: TKMHandAI;
    fArmyEval: TKMArmyEvaluation;
    fBuildList: TKMBuildList; //Not the best name for buildingManagement
    fDeliveries: TKMDeliveries;
    fFogOfWar: TKMFogOfWar; //Stores FOW info for current player, which includes
    fHouses: TKMHousesCollection;
    fRoadsList: TKMPointList; //Used only once to speedup mission loading, then freed
    fStats: TKMHandStats;
    fUnitGroups: TKMUnitGroups;

    fOwnerNikname: AnsiString;
    fPlayerType: THandType;
    fFlagColor: Cardinal;
    fCenterScreen: TKMPoint;
    fAlliances: array [0..MAX_HANDS-1] of TAllianceType;
    fShareFOW: array [0..MAX_HANDS-1] of Boolean;

    function GetColorIndex: Byte;

    function  GetAlliances(aIndex: Integer): TAllianceType;
    procedure SetAlliances(aIndex: Integer; aValue: TAllianceType);
    function  GetShareFOW(aIndex: Integer): Boolean;
    procedure SetShareFOW(aIndex: Integer; aValue: Boolean);
    procedure GroupDied(aGroup: TKMUnitGroup);
    procedure HouseDestroyed(aHouse: TKMHouse; aFrom: THandIndex);
    procedure UnitDied(aUnit: TKMUnit; aFrom: THandIndex);
    procedure UnitTrained(aUnit: TKMUnit);
    procedure WarriorWalkedOut(aUnit: TKMUnitWarrior);
  public
    Enabled: Boolean;

    constructor Create(aHandIndex: THandIndex);
    destructor Destroy; override;

    property AI: TKMHandAI read fAI;
    property BuildList: TKMBuildList read fBuildList;
    property Deliveries: TKMDeliveries read fDeliveries;
    property Houses: TKMHousesCollection read fHouses;
    property Stats: TKMHandStats read fStats;
    property FogOfWar: TKMFogOfWar read fFogOfWar;
    property ArmyEval: TKMArmyEvaluation read fArmyEval;
    property UnitGroups: TKMUnitGroups read fUnitGroups;

    procedure SetPlayerID(aNewIndex: THandIndex);
    procedure SetOwnerNikname(aName: AnsiString); //MP owner nikname (empty in SP)
    function OwnerName: UnicodeString; //Universal owner name
    function HasAssets: Boolean;
    property PlayerType: THandType read fPlayerType write fPlayerType; //Is it Human or AI
    property FlagColor: Cardinal read fFlagColor write fFlagColor;
    property FlagColorIndex: Byte read GetColorIndex;
    property Alliances[aIndex: Integer]: TAllianceType read GetAlliances write SetAlliances;
    property ShareFOW[aIndex: Integer]: Boolean read GetShareFOW write SetShareFOW;
    property CenterScreen: TKMPoint read fCenterScreen write fCenterScreen;

    procedure AfterMissionInit(aFlattenRoads: Boolean);

    function AddUnit(aUnitType: TUnitType; aLoc: TKMPoint; AutoPlace: Boolean = True; aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False): TKMUnit; reintroduce;
    function AddUnitGroup(aUnitType: TUnitType; Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aCount: Word): TKMUnitGroup;

    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;

    function CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
    function CanAddFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
    function CanAddHousePlan(aLoc: TKMPoint; aHouseType: THouseType): Boolean;
    function CanAddHousePlanAI(aX, aY: Word; aHouseType: THouseType; aCheckInfluence: Boolean): Boolean;

    procedure AddRoadToList(aLoc: TKMPoint);
    //procedure AddRoadConnect(LocA,LocB: TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType: TFieldType);
    procedure ToggleFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType; aMakeSound: Boolean);
    procedure ToggleFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
    function AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace: Boolean): TKMHouse;
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
    function AddHouseWIP(aHouseType: THouseType; aLoc: TKMPoint): TKMHouse;
    procedure RemGroup(Position: TKMPoint);
    procedure RemHouse(Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
    procedure RemHousePlan(Position: TKMPoint);
    procedure RemFieldPlan(Position: TKMPoint; aMakeSound:Boolean);
    procedure RemFakeFieldPlan(Position: TKMPoint);
    function FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean = False): TKMHouseInn;
    function FindHouse(aType: THouseType; aPosition: TKMPoint; Index: Byte=1): TKMHouse; overload;
    function FindHouse(aType: THouseType; Index: Byte=1): TKMHouse; overload;
    function HitTest(X,Y: Integer): TObject;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function GroupsHitTest(X, Y: Integer): TKMUnitGroup;
    function ObjectByUID(aUID: Integer): TObject;
    procedure GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);

    function GetFieldsCount: Integer;
    procedure GetFieldPlans(aList: TKMPointTagList; aRect: TKMRect; aIncludeFake: Boolean);
    procedure GetHousePlans(aList: TKMPointDirList; aRect: TKMRect);
    procedure GetPlansTablets(aList: TKMPointTagList; aRect: TKMRect);

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure IncAnimStep;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint(aRect: TKMRect); override;
  end;


  TKMHandAnimals = class (TKMHandCommon)
  public
    function GetFishInWaterBody(aWaterID: Byte; FindHighestCount: Boolean=True): TKMUnitAnimal;
  end;


implementation
uses KM_HandsCollection, KM_Resource, KM_ResSound, KM_Sound, KM_Game,
   KM_ResTexts, KM_AIFields, KM_Scripting;


const
  HANDS_NAMES_OFFSET = 100;


{ TKMHandCommon }
constructor TKMHandCommon.Create(aHandIndex: THandIndex);
begin
  inherited Create;
  fHandIndex  := aHandIndex;
  fUnits        := TKMUnitsCollection.Create;
end;


destructor TKMHandCommon.Destroy;
begin
  FreeThenNil(fUnits);
  inherited;
end;


function TKMHandCommon.AddUnit(aUnitType: TUnitType; aLoc: TKMPoint): TKMUnit;
begin
  //Animals are autoplaced by default
  Result := fUnits.AddUnit(fHandIndex, aUnitType, aLoc, True);
end;


procedure TKMHandCommon.Paint(aRect: TKMRect);
begin
  if not fGame.IsMapEditor or (mlUnits in fGame.MapEditor.VisibleLayers) then
    fUnits.Paint(aRect);
end;


procedure TKMHandCommon.RemUnit(Position: TKMPoint);
var U: TKMUnit;
begin
  Assert(fGame.IsMapEditor);

  U := fUnits.HitTest(Position.X, Position.Y);
  if U <> nil then
    fUnits.RemoveUnit(U);
end;


procedure TKMHandCommon.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('PlayerCommon');
  fUnits.Save(SaveStream);
end;


procedure TKMHandCommon.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerCommon');
  fUnits.Load(LoadStream);
end;


procedure TKMHandCommon.SyncLoad;
begin
  fUnits.SyncLoad;
end;


function TKMHandCommon.UnitsHitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;
begin
  Result := fUnits.HitTest(X, Y, UT);
end;


procedure TKMHandCommon.UpdateState(aTick: Cardinal);
begin
  fUnits.UpdateState;
end;


{ TKMHand }
constructor TKMHand.Create(aHandIndex: THandIndex);
var I: Integer;
begin
  inherited Create(aHandIndex);

  Enabled := True;

  fAI           := TKMHandAI.Create(fHandIndex);
  fFogOfWar     := TKMFogOfWar.Create(gTerrain.MapX, gTerrain.MapY);
  fStats        := TKMHandStats.Create;
  fRoadsList    := TKMPointList.Create;
  fHouses       := TKMHousesCollection.Create;
  fDeliveries   := TKMDeliveries.Create;
  fBuildList    := TKMBuildList.Create;
  fArmyEval     := TKMArmyEvaluation.Create(aHandIndex);
  fUnitGroups   := TKMUnitGroups.Create;

  fOwnerNikname := '';
  fPlayerType   := hndComputer;
  for I := 0 to MAX_HANDS - 1 do
  begin
    fAlliances[I] := at_Enemy; //Everyone is enemy by default
    fShareFOW[I] := True; //Share FOW between allies by default (it only affects allied players)
  end;

  fFlagColor := DefaultTeamColors[fHandIndex]; //Init with default color, later replaced by Script
end;


//Destruction order is important as Houses and Units need to access
//Stats/Deliveries and other collection in their Destroy/Abandon/Demolish methods
destructor TKMHand.Destroy;
begin
  //Groups freed before units since we need to release pointers they have to units
  FreeThenNil(fUnitGroups);

  //Free units
  inherited;

  FreeThenNil(fArmyEval);
  FreeThenNil(fRoadsList);
  FreeThenNil(fHouses);

  //Should be freed after Houses and Units, as they write Stats on Destroy
  FreeThenNil(fStats);
  FreeThenNil(fFogOfWar);
  FreeThenNil(fDeliveries);
  FreeThenNil(fBuildList);
  FreeThenNil(fAI);
end;


//Place unit of aUnitType to aLoc via script
//AutoPlace - add unit to nearest available spot if aLoc is already taken (or unwalkable)
function TKMHand.AddUnit(aUnitType: TUnitType; aLoc: TKMPoint; AutoPlace: Boolean = True; aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False): TKMUnit;
var
  G: TKMUnitGroup;
begin
  Result := fUnits.AddUnit(fHandIndex, aUnitType, aLoc, AutoPlace, aRequiredWalkConnect);

  //Unit failed to add, that happens
  if Result = nil then Exit;

  Result.OnUnitDied := UnitDied;
  Result.OnUnitTrained := UnitTrained; //Used for debug Scout placed by a cheat

  if Result is TKMUnitWarrior then
    TKMUnitWarrior(Result).OnWarriorWalkOut := WarriorWalkedOut;

  if Result is TKMUnitWorker then
    fBuildList.AddWorker(TKMUnitWorker(Result));
  if Result is TKMUnitSerf then
    fDeliveries.AddSerf(TKMUnitSerf(Result));

  if not aCheat then
    fStats.UnitCreated(aUnitType, False)
  else
    if Result is TKMUnitWarrior then
    begin
      //When we place a cheat Scout we want to rig it immediately

      //It's simpler to count cheat scouts as initial, rather than trained.
      //Then we don't need to mess with initial recruits.
      fStats.UnitCreated(aUnitType, False);

      G := fUnitGroups.WarriorTrained(TKMUnitWarrior(Result));
      Assert(G <> nil, 'It is certain that equipped warrior creates or finds some group to join to');
      G.OnGroupDied := GroupDied;

      //Scripting doesn't care about scouts added this way.
      //It could cause issues if the scripter assumes the warrior came from the player's barracks.
      //The event is "OnWarriorEquipped" not "OnWarriorCreated".
      //fScripting.ProcWarriorEquipped(Result, G);
    end;
end;


//Start training unit in School/Barracks
//User can cancel the training, so we don't add unit to stats just yet
function TKMHand.TrainUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
begin
  Result := fUnits.AddUnit(fHandIndex, aUnitType, Position, False);
  Result.OnUnitDied := UnitDied;
  Result.OnUnitTrained := UnitTrained;

  if Result is TKMUnitWarrior then
    TKMUnitWarrior(Result).OnWarriorWalkOut := WarriorWalkedOut;

  //Adding a unit automatically sets gTerrain.IsUnit, but since the unit was trained
  //inside School/Barracks we don't need that
  gTerrain.UnitRem(Position);

  //Do not add unit to statistic just yet, wait till it's training complete
end;


procedure TKMHand.UnitTrained(aUnit: TKMUnit);
begin
  if aUnit.UnitType = ut_Worker then
    fBuildList.AddWorker(TKMUnitWorker(aUnit));
  if aUnit.UnitType = ut_Serf then
    fDeliveries.AddSerf(TKMUnitSerf(aUnit));

  //Warriors don't trigger "OnTrained" event, they trigger "WarriorEquipped" in WarriorWalkedOut below
  if not (aUnit is TKMUnitWarrior) then
    fScripting.ProcUnitTrained(aUnit);

  fStats.UnitCreated(aUnit.UnitType, True);
end;


procedure TKMHand.WarriorWalkedOut(aUnit: TKMUnitWarrior);
var G: TKMUnitGroup;
begin
  G := fUnitGroups.WarriorTrained(aUnit);
  Assert(G <> nil, 'It is certain that equipped warrior creates or finds some group to join to');
  G.OnGroupDied := GroupDied;
  if PlayerType = hndComputer then
  begin
    AI.General.WarriorEquipped(G);
    G := UnitGroups.GetGroupByMember(aUnit); //AI might assign warrior to different group
  end;
  fScripting.ProcWarriorEquipped(aUnit, G);
end;


function TKMHand.AddUnitGroup(aUnitType: TUnitType; Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aCount: Word): TKMUnitGroup;
var
  I: Integer;
begin
  Assert(aDir <> dir_NA);
  Result := nil;

  if aUnitType in [CITIZEN_MIN..CITIZEN_MAX] then
    for I := 0 to aCount - 1 do
      AddUnit(aUnitType, Position, True)
  else
  if aUnitType in [WARRIOR_MIN..WARRIOR_MAX] then
    Result := fUnitGroups.AddGroup(fHandIndex, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aCount);

  //Group can be nil if it fails to be placed on terrain (e.g. because of terrain height passability)
  if Result <> nil then
    Result.OnGroupDied := GroupDied;

  //Units will be added to statistic inside the function for some units may not fit on map
end;


//When adding roads from script we want to batch them all into one list to save time
//on WalkConnect and other calculations
procedure TKMHand.AddRoadToList(aLoc: TKMPoint);
begin
  Assert(fRoadsList <> nil);

  //Sometimes maps can have roads placed outside of map bounds - ignore them
  //(on 80x80 map Loc range is 1..79, which is not obvious when placing roads manually in script)
  if gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
    fRoadsList.Add(aLoc);
end;


//Lay out all roads at once to save time on Terrain lighting/passability recalculations
procedure TKMHand.AfterMissionInit(aFlattenRoads: Boolean);
begin
  if fRoadsList <> nil then
  begin
    gTerrain.SetRoads(fRoadsList, fHandIndex, not aFlattenRoads); //If we are flattening roads that will update WalkConnect anyway
    if aFlattenRoads then
      gTerrain.FlattenTerrain(fRoadsList);
    FreeAndNil(fRoadsList);
  end;

  fAI.Mayor.AfterMissionInit;
end;


procedure TKMHand.SetPlayerID(aNewIndex: THandIndex);
begin
  fHandIndex := aNewIndex;
  fUnits.OwnerUpdate(aNewIndex);
  fHouses.OwnerUpdate(aNewIndex);
  fAI.OwnerUpdate(aNewIndex);
end;


procedure TKMHand.SetOwnerNikname(aName: AnsiString); //MP owner nikname (empty in SP)
begin
  fOwnerNikname := aName;
end;


procedure TKMHand.AddField(aLoc: TKMPoint; aFieldType: TFieldType);
begin
  gTerrain.SetField(aLoc, fHandIndex, aFieldType);
end;


//See comment on CanAddFakeFieldPlan
function TKMHand.CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
var I: Integer;
begin
  Result := gTerrain.CanAddField(aLoc.X, aLoc.Y, aFieldType)
            and (fBuildList.FieldworksList.HasField(aLoc) = ft_None)
            and not fBuildList.HousePlanList.HasPlan(aLoc);
  //Don't allow placing on allies plans either
  if Result then
    for I := 0 to gHands.Count - 1 do
      if (I <> fHandIndex) and (fAlliances[I] = at_Ally) then
        Result := Result and (gHands[i].fBuildList.FieldworksList.HasField(aLoc) = ft_None)
                         and not gHands[i].fBuildList.HousePlanList.HasPlan(aLoc);
end;


//This differs from above only in that it uses HasFakeField instead of HasField.
//We need it because the user expects to be blocked by fake field plans, but the gameplay should not.
//When the result effects the outcome of the game, the above function should be used instead.
function TKMHand.CanAddFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
var I: Integer;
begin
  Result := gTerrain.CanAddField(aLoc.X, aLoc.Y, aFieldType)
            and (fBuildList.FieldworksList.HasFakeField(aLoc) = ft_None)
            and not fBuildList.HousePlanList.HasPlan(aLoc);
  //Don't allow placing on allies plans either
  if Result then
    for I := 0 to gHands.Count - 1 do
      if (I <> fHandIndex) and (fAlliances[I] = at_Ally) then
        Result := Result and (gHands[i].fBuildList.FieldworksList.HasField(aLoc) = ft_None)
                         and not gHands[i].fBuildList.HousePlanList.HasPlan(aLoc);
end;


function TKMHand.CanAddHousePlan(aLoc: TKMPoint; aHouseType: THouseType): Boolean;
var I,K,J,S,T,Tx,Ty: Integer; HA: THouseArea;
begin
  Result := gTerrain.CanPlaceHouse(aLoc, aHouseType);
  if not Result then Exit;

  HA := fResource.HouseDat[aHouseType].BuildArea;
  for I := 1 to 4 do
  for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Tx := aLoc.X - fResource.HouseDat[aHouseType].EntranceOffsetX + K - 3;
    Ty := aLoc.Y + I - 4;
    //AI ignores FOW (this function is used from scripting)
    Result := Result and gTerrain.TileInMapCoords(Tx, Ty, 1)
                     and ((fPlayerType = hndComputer) or (fFogOfWar.CheckTileRevelation(Tx, Ty) > 0));
    //This checks below require Tx;Ty to be within the map so exit immediately if they are not
    if not Result then exit;

    //This tile must not contain fields/houses of allied players or self
    for J := 0 to gHands.Count - 1 do
      if (J = fHandIndex) or (fAlliances[J] = at_Ally) then
      begin
        Result := Result and (gHands[J].fBuildList.FieldworksList.HasField(KMPoint(Tx,Ty)) = ft_None);
        //Surrounding tiles must not be a house
        for S:=-1 to 1 do
          for T:=-1 to 1 do
            Result := Result and not gHands[J].fBuildList.HousePlanList.HasPlan(KMPoint(Tx+S,Ty+T));
      end;
  end;
end;


function TKMHand.CanAddHousePlanAI(aX, aY: Word; aHouseType: THouseType; aCheckInfluence: Boolean): Boolean;
var
  I, K, J, S, T, Tx, Ty: Integer;
  HA: THouseArea;
  EnterOff: ShortInt;
  TerOwner: THandIndex;
begin
  Result := False;

  //Check if we can place house on terrain, this also makes sure the house is
  //at least 1 tile away from map border (skip that below)
  if not gTerrain.CanPlaceHouse(KMPoint(aX, aY), aHouseType) then
    Exit;

  //Perform additional cheks for AI
  HA := fResource.HouseDat[aHouseType].BuildArea;
  EnterOff := fResource.HouseDat[aHouseType].EntranceOffsetX;
  for I := 1 to 4 do
  for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Tx := aX + K - 3 - EnterOff;
    Ty := aY + I - 4;

    //Make sure we don't block existing roads
    if gTerrain.CheckPassability(KMPoint(Tx, Ty), CanWalkRoad) then
      Exit;

    //Check with influence maps
    if aCheckInfluence and AI_GEN_INFLUENCE_MAPS then
    begin
      //Check if tile's blocked
      if (fAIFields.Influences.AvoidBuilding[Ty, Tx] > 0) then
        Exit;

      //Check ownership for entrance (good enough since it does not changes that fast)
      if (HA[I,K] = 2) then
      begin
        TerOwner := fAIFields.Influences.GetBestOwner(Tx,Ty);
        if ((TerOwner <> fHandIndex) and (TerOwner <> PLAYER_NONE)) then
          Exit;
      end;
    end;

    //Avoid placing houses in choke-points _/house\_ by checking upper corners
    if not (aHouseType in [ht_GoldMine, ht_IronMine]) then
      if (gTerrain.Land[Ty-1, Tx - 1].Passability * [CanMakeRoads, CanWalkRoad] = [])
      or (gTerrain.Land[Ty-1, Tx + 1].Passability * [CanMakeRoads, CanWalkRoad] = [])
      then
        Exit;

    //Make sure we can add road below house, full width + 1 on each side
    //Terrain already checked we are 1 tile away from map edge
    if (I = 4) then
      if (gTerrain.Land[Ty+1, Tx - 1].Passability * [CanMakeRoads, CanWalkRoad] = [])
      or (gTerrain.Land[Ty+1, Tx    ].Passability * [CanMakeRoads, CanWalkRoad] = [])
      or (gTerrain.Land[Ty+1, Tx + 1].Passability * [CanMakeRoads, CanWalkRoad] = [])
      then
        Exit;

    //This tile must not contain fields/houseplans of allied players or self
    for J := 0 to gHands.Count - 1 do
      if (J = fHandIndex) or (fAlliances[J] = at_Ally) then
      begin
        if (gHands[J].fBuildList.FieldworksList.HasField(KMPoint(Tx,Ty)) <> ft_None) then
          Exit;

        //Surrounding tiles must not be a house
        for S := -1 to 1 do
        for T := -1 to 1 do
        if gHands[J].fBuildList.HousePlanList.HasPlan(KMPoint(Tx+S,Ty+T)) then
          Exit;
      end;
  end;

  Result := True;
end;


//Due to lag there could be already plans placed by user in previous ticks
//Check if Plan can be placed once again, as we might have conflicting commands caused by lag
//This is called by GIP when a place field command is processed
procedure TKMHand.ToggleFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType; aMakeSound:Boolean);
var Plan: TFieldType;
begin
  Assert(aFieldType in [ft_Road, ft_Corn, ft_Wine], 'Placing wrong FieldType');

  Plan := fBuildList.FieldworksList.HasField(aLoc);
  if aFieldType = Plan then //Same plan - remove it
    RemFieldPlan(aLoc,aMakeSound)
  else
    if CanAddFieldPlan(aLoc, aFieldType) then
    begin
      if aMakeSound and not fGame.IsReplay
      and (HandIndex = MySpectator.HandIndex) then
        gSoundPlayer.Play(sfx_placemarker);
      fBuildList.FieldworksList.AddField(aLoc, aFieldType)
    end
    else
    begin
      if aMakeSound and not fGame.IsReplay
      and (HandIndex = MySpectator.HandIndex) then
        gSoundPlayer.Play(sfx_CantPlace, 4);
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
procedure TKMHand.ToggleFakeFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
var Plan: TFieldType;
begin
  Assert(aFieldType in [ft_Road, ft_Corn, ft_Wine], 'Placing wrong fake FieldType');

  Plan := fBuildList.FieldworksList.HasFakeField(aLoc);
  if aFieldType = Plan then //Same plan - remove it
  begin
    fBuildList.FieldworksList.RemFakeField(aLoc); //Remove our fake marker which is shown to the user
    fBuildList.FieldworksList.AddFakeDeletedField(aLoc); //This will hide the real field until it is deleted from game
    if HandIndex = MySpectator.HandIndex then gSoundPlayer.Play(sfx_Click);
  end
  else
    if CanAddFakeFieldPlan(aLoc, aFieldType) then
    begin
      fBuildList.FieldworksList.AddFakeField(aLoc, aFieldType);
      if HandIndex = MySpectator.HandIndex then
        gSoundPlayer.Play(sfx_placemarker);
    end
    else
      if HandIndex = MySpectator.HandIndex then
        gSoundPlayer.Play(sfx_CantPlace, 4);
end;


//Used mainly for testing purposes
{procedure TKMHand.AddRoadConnect(LocA,LocB: TKMPoint);
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


function TKMHand.AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace: Boolean): TKMHouse;
begin
  Result := fHouses.AddHouse(aHouseType, PosX, PosY, fHandIndex, RelativeEntrace);
  Result.OnDestroyed := HouseDestroyed;

  //Set default house repair mode
  if fPlayerType = hndComputer then
    Result.BuildingRepair := fAI.Mayor.AutoRepair;
end;


//Add plan of a house, house is not created until after worker flattens the terrain
procedure TKMHand.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
var
  Loc: TKMPoint;
begin
  Loc.X := aLoc.X - fResource.HouseDat[aHouseType].EntranceOffsetX;
  Loc.Y := aLoc.Y;

  fBuildList.HousePlanList.AddPlan(aHouseType, Loc);
  fStats.HousePlanned(aHouseType);
  fScripting.ProcHousePlanPlaced(fHandIndex, Loc.X, Loc.Y, aHouseType);

  if (HandIndex = MySpectator.HandIndex) and not fGame.IsReplay then
    gSoundPlayer.Play(sfx_placemarker);
end;


function TKMHand.AddHouseWIP(aHouseType: THouseType; aLoc: TKMPoint): TKMHouse;
begin
  Result := fHouses.AddHouseWIP(aHouseType, aLoc.X, aLoc.Y, fHandIndex);
  Result.OnDestroyed := HouseDestroyed;

  //Set default house repair mode
  if fPlayerType = hndComputer then
    Result.BuildingRepair := fAI.Mayor.AutoRepair;

  fStats.HouseStarted(aHouseType);
end;


//Player wants to remove own house
procedure TKMHand.RemHouse(Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
var H: TKMHouse;
begin
  //Sound is handled in DemolishHouse
  H := fHouses.HitTest(Position.X, Position.Y);
  if H = nil then Exit; //Due to network delays the house might have already been destroyed by now

  H.DemolishHouse(fHandIndex, IsEditor);
end;


procedure TKMHand.RemHousePlan(Position: TKMPoint);
var
  HT: THouseType;
begin
  HT := fBuildList.HousePlanList.GetPlan(Position);
  if HT = ht_None then Exit; //Due to network delays house might not exist now

  fBuildList.HousePlanList.RemPlan(Position);
  fStats.HousePlanRemoved(HT);
  if (HandIndex = MySpectator.HandIndex) and not fGame.IsReplay then
    gSoundPlayer.Play(sfx_Click);
end;


//This is called by the GIP when an erase command is processed
procedure TKMHand.RemFieldPlan(Position: TKMPoint; aMakeSound: Boolean);
begin
  fBuildList.FieldworksList.RemFieldPlan(Position);
  if aMakeSound and not fGame.IsReplay and (HandIndex = MySpectator.HandIndex) then gSoundPlayer.Play(sfx_Click);
end;


procedure TKMHand.RemGroup(Position: TKMPoint);
var Group: TKMUnitGroup;
begin
  Assert(fGame.IsMapEditor);

  Group := fUnitGroups.HitTest(Position.X, Position.Y);
  if Group <> nil then
    fUnitGroups.RemGroup(Group);
end;

//This is called immediately when the user clicks erase on a field plan.
//We know that an erase command is queued and will be processed in some ticks,
//so we AddFakeDeletedField which lets the user think the field was removed,
//while the game does not know the difference.
procedure TKMHand.RemFakeFieldPlan(Position: TKMPoint);
begin
  fBuildList.FieldworksList.RemFakeField(Position); //Remove our fake marker which is shown to the user
  fBuildList.FieldworksList.AddFakeDeletedField(Position); //This will hide the real field until it is deleted from game
  if HandIndex = MySpectator.HandIndex then gSoundPlayer.Play(sfx_Click);
end;


function TKMHand.FindHouse(aType: THouseType; aPosition: TKMPoint; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;


function TKMHand.FindHouse(aType: THouseType; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, 0, 0, Index);
end;


function TKMHand.FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean=false): TKMHouseInn;
var
  H: TKMHouseInn;
  I: Integer;
  Dist, BestMatch: Single;
begin
  //This function will return the best inn for a unit at Loc, base on distance, food available and space available.
  //Will return nil if no suitable inn is available
  Result := nil;
  I := 1;
  BestMatch := MaxSingle;
  if UnitIsAtHome then inc(Loc.Y); //From outside the door of the house

  H := TKMHouseInn(FindHouse(ht_Inn));
  repeat
    //First make sure that it is valid
    if (H <> nil) and H.HasFood and H.HasSpace
    and aUnit.CanWalkTo(Loc, KMPointBelow(H.GetEntrance), CanWalk, 0) then
    begin
      //Take the closest inn out of the ones that are suitable
      Dist := KMLengthSqr(H.GetPosition, Loc);
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


//Does the player has any assets (without assets player is harmless)
function TKMHand.HasAssets: Boolean;
begin
  Result := (Units.Count > 0) or (Houses.Count > 0);
end;


function TKMHand.HitTest(X, Y: Integer): TObject;
var
  H: TKMHouse;
  U: TKMUnit;
  G: TKMUnitGroup;
begin
  //Houses have priority over units, so you can't select an occupant
  //Selection priority is as follows:
  //BuiltHouses > UnitGroups > Units > IncompleteHouses

  H := HousesHitTest(X,Y);
  if (H <> nil) and (H.BuildingState in [hbs_Stone, hbs_Done]) then
    Result := H
  else
  begin
    G := GroupsHitTest(X,Y);
    if (G <> nil) then
      Result := G
    else
    begin
      U := UnitsHitTest(X,Y);
      if (U <> nil) and (not U.IsDeadOrDying) then
        Result := U
      else
        Result := H; //Incomplete house or nil
    end;
  end;
end;


//Which house whas destroyed and by whom
procedure TKMHand.HouseDestroyed(aHouse: TKMHouse; aFrom: THandIndex);
begin
  //Dispose of delivery tasks performed in DeliverQueue unit
  if aHouse.BuildingState in [hbs_Wood .. hbs_Done] then
  begin
    Deliveries.Queue.RemAllOffers(aHouse);
    Deliveries.Queue.RemDemand(aHouse);
  end;

  //Only Done houses are treated as Self-Destruct, Lost, Destroyed
  if aHouse.BuildingState in [hbs_NoGlyph .. hbs_Stone] then
    fStats.HouseEnded(aHouse.HouseType)
  else
    //Distribute honors
    if aFrom = fHandIndex then
      fStats.HouseSelfDestruct(aHouse.HouseType)
    else
    begin
      Stats.HouseLost(aHouse.HouseType);

      if aFrom <> -1 then
        gHands[aFrom].Stats.HouseDestroyed(aHouse.HouseType);
    end;

  //Scripting events happen AFTER updating statistics
  fScripting.ProcHouseDestroyed(aHouse, aFrom);

  //MySpectator is nil during loading, when houses can be destroyed at the start
  if MySpectator <> nil then
  begin
    if MySpectator.Highlight = aHouse then
      MySpectator.Highlight := nil;
    if MySpectator.Selected = aHouse then
      MySpectator.Selected := nil;
  end;
end;


function TKMHand.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;


function TKMHand.GroupsHitTest(X, Y: Integer): TKMUnitGroup;
begin
  Result:= fUnitGroups.HitTest(X, Y);
end;


function TKMHand.ObjectByUID(aUID: Integer): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Units.Count - 1 do
    if aUID = Units[I].UID then
    begin
      Result := Units[I];
      Exit;
    end;
end;


function TKMHand.GetColorIndex: Byte;
var
  I: Integer;
begin
  Result := 3; //3 = Black which can be the default when a non-palette 32 bit color value is used
  for I := 0 to 255 do
    if fResource.Palettes.DefDal.Color32(I) = fFlagColor then
      Result := I;
end;


function TKMHand.OwnerName: UnicodeString;
begin
  //Default names
  if PlayerType = hndHuman then
    Result := gResTexts[TX_PLAYER_YOU]
  else
    Result := Format(gResTexts[TX_PLAYER_X], [fHandIndex + 1]);

  //Try to take player name from mission text if we are in SP
  //Do not use names in MP ot avoid confusion of AI players with real player niknames
  if fGame.GameMode in [gmSingle, gmMapEd, gmReplaySingle] then
    if fGame.TextMission.HasText(HANDS_NAMES_OFFSET + fHandIndex) then
      if PlayerType = hndHuman then
        Result := gResTexts[TX_PLAYER_YOU] + ' (' + fGame.TextMission[HANDS_NAMES_OFFSET + fHandIndex] + ')'
      else
        Result := fGame.TextMission[HANDS_NAMES_OFFSET + fHandIndex];

  //If this location is controlled by an MP player - show his nik
  if fOwnerNikname <> '' then
    Result := UnicodeString(fOwnerNikname);
end;


function TKMHand.GetAlliances(aIndex: Integer): TAllianceType;
begin
  Result := fAlliances[aIndex];
end;


procedure TKMHand.SetAlliances(aIndex: Integer; aValue: TAllianceType);
begin
  fAlliances[aIndex] := aValue;
end;


function  TKMHand.GetShareFOW(aIndex: Integer): Boolean;
begin
  Result := fShareFOW[aIndex];
end;


procedure TKMHand.SetShareFOW(aIndex: Integer; aValue: Boolean);
begin
  fShareFOW[aIndex] := aValue;
end;


{ See if player owns any Fields/Roads/Walls (has any assets on Terrain)
  If Player has none and no Units/Houses we can assume it's empty and does not needs to be saved
  Queried by MapEditor.SaveDAT;
  Might also be used to show Players strength (or builder/warrior balance) in Tavern }
function TKMHand.GetFieldsCount: Integer;
var
  I,K: Integer;
begin
  Result := 0;
  for I := 1 to gTerrain.MapY do
  for K := 1 to gTerrain.MapX do
    if gTerrain.Land[I,K].TileOwner = fHandIndex then
      Inc(Result);
end;


procedure TKMHand.GetFieldPlans(aList: TKMPointTagList; aRect: TKMRect; aIncludeFake: Boolean);
var
  I: THandIndex;
begin
  fBuildList.FieldworksList.GetFields(aList, aRect, aIncludeFake);

  for I := 0 to gHands.Count - 1 do
    if (I <> fHandIndex) and (gHands.CheckAlliance(fHandIndex, I) = at_Ally) then
      gHands[I].BuildList.FieldworksList.GetFields(aList, aRect, aIncludeFake);
end;


procedure TKMHand.GetHousePlans(aList: TKMPointDirList; aRect: TKMRect);
var
  I: THandIndex;
begin
  fBuildList.HousePlanList.GetOutlines(aList, aRect);

  for I := 0 to gHands.Count - 1 do
    if (I <> fHandIndex) and (gHands.CheckAlliance(fHandIndex, I) = at_Ally) then
      gHands[I].BuildList.HousePlanList.GetOutlines(aList, aRect);
end;


procedure TKMHand.GetPlansTablets(aList: TKMPointTagList; aRect: TKMRect);
var
  I: THandIndex;
begin
  fBuildList.HousePlanList.GetTablets(aList, aRect);

  for I := 0 to gHands.Count - 1 do
    if (I <> fHandIndex) and (gHands.CheckAlliance(fHandIndex, I) = at_Ally) then
      gHands[I].BuildList.HousePlanList.GetTablets(aList, aRect);
end;


procedure TKMHand.GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);
  //Replace existing icon with a Block
  procedure BlockPoint(aPoint: TKMPoint; aID: Integer);
  var I: Integer;
  begin
    //Remove all existing marks on this tile (entrance can have 2 entries)
    for I := aList.Count - 1 downto 0 do
      if KMSamePoint(aList[I], aPoint) then
        aList.Remove(aPoint);

    aList.Add(aPoint, aID);
  end;

var
  I,K,J,S,T: Integer;
  P2: TKMPoint;
  AllowBuild: Boolean;
  HA: THouseArea;
begin
  //Get basic Marks
  gTerrain.GetHouseMarks(aLoc, aHouseType, aList);

  //Override marks if there are House/FieldPlans (only we know about our plans) and or FogOfWar
  HA := fResource.HouseDat[aHouseType].BuildArea;

  for I := 1 to 4 do for K := 1 to 4 do
  if (HA[I,K] <> 0)
  and gTerrain.TileInMapCoords(aLoc.X+K-3-fResource.HouseDat[aHouseType].EntranceOffsetX, aLoc.Y+I-4, 1) then
  begin
    //This can't be done earlier since values can be off-map
    P2 := KMPoint(aLoc.X+K-3-fResource.HouseDat[aHouseType].EntranceOffsetX, aLoc.Y+I-4);

    //Forbid planning on unrevealed areas and fieldplans
    AllowBuild := (fFogOfWar.CheckTileRevelation(P2.X, P2.Y) > 0);

    //This tile must not contain fields/houses of allied players or self
    if AllowBuild then
    for J := 0 to gHands.Count - 1 do
    if ((J = fHandIndex) or (gHands.CheckAlliance(fHandIndex, J) = at_Ally))
    and ((gHands[J].fBuildList.FieldworksList.HasField(P2) <> ft_None)
       or gHands[J].fBuildList.HousePlanList.HasPlan(P2)) then
       AllowBuild := False;

    //Check surrounding tiles in +/- 1 range for other houses pressence
    if AllowBuild then
    for S := -1 to 1 do for T := -1 to 1 do
      if (S<>0) or (T<>0) then //This is a surrounding tile, not the actual tile
        for J := 0 to gHands.Count - 1 do
          if ((J = fHandIndex) or (gHands.CheckAlliance(fHandIndex, J) = at_Ally))
          and gHands[J].fBuildList.HousePlanList.HasPlan(KMPoint(P2.X+S,P2.Y+T)) then
          begin
            BlockPoint(KMPoint(P2.X+S,P2.Y+T), TC_BLOCK); //Block surrounding points
            AllowBuild := False;
          end;

    //Mark the tile according to previous check results
    if not AllowBuild then
      if HA[I,K] = 2 then
        BlockPoint(P2, TC_BLOCK_ENTRANCE)
      else
        if aHouseType in [ht_GoldMine, ht_IronMine] then
          BlockPoint(P2, TC_BLOCK_MINE)
        else
          BlockPoint(P2, TC_BLOCK);
  end;
end;


procedure TKMHand.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Enabled);
  if not Enabled then Exit;

  inherited;
  fAI.Save(SaveStream);
  fBuildList.Save(SaveStream);
  fDeliveries.Save(SaveStream);
  fFogOfWar.Save(SaveStream);
  fHouses.Save(SaveStream);
  fStats.Save(SaveStream);
  fUnitGroups.Save(SaveStream);

  SaveStream.Write(fHandIndex);
  SaveStream.WriteA(fOwnerNikname);
  SaveStream.Write(fPlayerType, SizeOf(fPlayerType));
  SaveStream.Write(fAlliances, SizeOf(fAlliances));
  SaveStream.Write(fShareFOW, SizeOf(fShareFOW));
  SaveStream.Write(fCenterScreen);
  SaveStream.Write(fFlagColor);
end;


procedure TKMHand.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Enabled);
  if not Enabled then Exit;

  inherited;
  fAI.Load(LoadStream);
  fBuildList.Load(LoadStream);
  fDeliveries.Load(LoadStream);
  fFogOfWar.Load(LoadStream);
  fHouses.Load(LoadStream);
  fStats.Load(LoadStream);
  fUnitGroups.Load(LoadStream);

  LoadStream.Read(fHandIndex);
  LoadStream.ReadA(fOwnerNikname);
  LoadStream.Read(fPlayerType, SizeOf(fPlayerType));
  LoadStream.Read(fAlliances, SizeOf(fAlliances));
  LoadStream.Read(fShareFOW, SizeOf(fShareFOW));
  LoadStream.Read(fCenterScreen);
  LoadStream.Read(fFlagColor);
end;


procedure TKMHand.SyncLoad;
var I: Integer;
begin
  if not Enabled then Exit;

  inherited;

  //Assign event handler after load
  for I := 0 to fUnits.Count - 1 do
  begin
    fUnits[I].OnUnitDied := UnitDied;
    fUnits[I].OnUnitTrained := UnitTrained;
    if fUnits[I] is TKMUnitWarrior then
      TKMUnitWarrior(fUnits[I]).OnWarriorWalkOut := WarriorWalkedOut;
  end;

  fUnitGroups.SyncLoad;

  //Assign event handler after load
  for I := 0 to fUnitGroups.Count - 1 do
    fUnitGroups[I].OnGroupDied := GroupDied;

  fHouses.SyncLoad;

  //Assign event handler after load
  for I := 0 to fHouses.Count - 1 do
    fHouses[I].OnDestroyed := HouseDestroyed;

  fDeliveries.SyncLoad;
  fBuildList.SyncLoad;
  fAI.SyncLoad;
end;


procedure TKMHand.IncAnimStep;
begin
  if not Enabled then Exit;

  fHouses.IncAnimStep;
end;


procedure TKMHand.UnitDied(aUnit: TKMUnit; aFrom: THandIndex);
begin
  Stats.UnitLost(aUnit.UnitType);
  if aFrom <> -1 then
    gHands[aFrom].Stats.UnitKilled(aUnit.UnitType);

  //Call script event after updating statistics
  fScripting.ProcUnitDied(aUnit, aFrom);

  //MySpectator is nil during loading
  if MySpectator <> nil then
  begin
    if MySpectator.Highlight = aUnit then
      MySpectator.Highlight := nil;
    if MySpectator.Selected = aUnit then
      MySpectator.Selected := nil;
  end;
end;


procedure TKMHand.GroupDied(aGroup: TKMUnitGroup);
begin
  //Groups arent counted in statistics
  if MySpectator.Highlight = aGroup then
    MySpectator.Highlight := nil;
  if MySpectator.Selected = aGroup then
    MySpectator.Selected := nil;
end;


procedure TKMHand.UpdateState(aTick: Cardinal);
begin
  if not Enabled then Exit;

  //Update Groups logic before Units
  fUnitGroups.UpdateState;

  inherited;

  fHouses.UpdateState;
  fFogOfWar.UpdateState; //We might optimize it for AI somehow, to make it work coarse and faster

  //Distribute AI updates among different Ticks to avoid slowdowns
  if (aTick + Byte(fHandIndex)) mod 10 = 0 then
  begin
    fBuildList.UpdateState;
    fDeliveries.UpdateState;
  end;

  //AI update takes care of it's own interleaving, so run it every tick
  fAI.UpdateState(aTick);

  //if (aTick + Byte(fPlayerIndex)) mod 20 = 0 then
    //fArmyEval.UpdateState;

  if (fGame.MissionMode = mm_Normal) and (aTick mod CHARTS_SAMPLING_FOR_ECONOMY = 0)
  or (fGame.MissionMode = mm_Tactic) and (aTick mod CHARTS_SAMPLING_FOR_TACTICS = 0)
  or (aTick = 1) then
    fStats.UpdateState;
end;


procedure TKMHand.Paint(aRect: TKMRect);
begin
  if not Enabled then Exit;

  inherited;

  if not fGame.IsMapEditor or (mlUnits in fGame.MapEditor.VisibleLayers) then
    fUnitGroups.Paint(aRect);

  if not fGame.IsMapEditor or (mlHouses in fGame.MapEditor.VisibleLayers) then
    fHouses.Paint(aRect);
end;


{ TKMHandAnimals }
function TKMHandAnimals.GetFishInWaterBody(aWaterID: Byte; FindHighestCount: Boolean=True): TKMUnitAnimal;
var
  I, HighestGroupCount: Integer;
  U: TKMUnit;
begin
  Result := nil;
  if aWaterID = 0 then Exit; //Fish should always be in valid water
  HighestGroupCount := 0;

  for I := 0 to fUnits.Count - 1 do
  begin
    U := fUnits[I]; //Store locally

    if (U <> nil)
    and (U.UnitType = ut_Fish)
    and (not U.IsDeadOrDying) //Fish are killed when they are caught or become stuck
    and (gTerrain.Land[U.GetPosition.Y, U.GetPosition.X].WalkConnect[wcFish] = aWaterID)
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
