unit KM_Player;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_PlayerAI, KM_Units, KM_Houses, KM_BuildList, KM_DeliverQueue,
  KM_PlayerStats, KM_Goals, KM_FogOfWar, KM_ArmyEvaluation;


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

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
    procedure RemUnit(Position: TKMPoint);
    function UnitsHitTest(X, Y: Integer; const UT: TUnitType = ut_Any): TKMUnit;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    procedure UpdateState(aUpdateAI: Boolean); virtual;
    procedure Paint; virtual;
  end;


  TKMPlayer = class (TKMPlayerCommon)
  private
    fAI:TKMPlayerAI;
    fBuildList: TKMBuildList; //Not the best name for buildingManagement
    fDeliverList:TKMDeliverQueue;
    fHouses:TKMHousesCollection;
    fRoadsList:TKMPointList; //Used only once to speedup mission loading, then freed
    fStats:TKMPlayerStats;
    fGoals:TKMGoals;
    fFogOfWar:TKMFogOfWar; //Stores FOW info for current player, which includes
    fArmyEval:TKMArmyEvaluation; // Can used by all players

    fPlayerName: string;
    fPlayerType:TPlayerType;
    fFlagColor:cardinal;
    fCenterScreen:TKMPoint;
    fAlliances: array[0..MAX_PLAYERS-1] of TAllianceType;

    fSkipWinConditionCheck:boolean;
    fSkipDefeatConditionCheck:boolean;

    function GetColorIndex:byte;

    function  GetAlliances(Index:integer):TAllianceType;
    procedure SetAlliances(Index:integer; aValue:TAllianceType);
  public
    constructor Create(aPlayerIndex:TPlayerIndex);
    destructor Destroy; override;

    property AI:TKMPlayerAI read fAI;
    property BuildList: TKMBuildList read fBuildList;
    property DeliverList:TKMDeliverQueue read fDeliverList;
    property Houses:TKMHousesCollection read fHouses;
    property Stats:TKMPlayerStats read fStats;
    property Goals:TKMGoals read fGoals;
    property FogOfWar:TKMFogOfWar read fFogOfWar;
    property ArmyEval:TKMArmyEvaluation read fArmyEval;

    procedure SetPlayerID(aNewIndex:TPlayerIndex);
    property PlayerName: string read fPlayerName write fPlayerName;
    property PlayerType:TPlayerType read fPlayerType write fPlayerType; //Is it Human or AI
    property FlagColor:cardinal read fFlagColor write fFlagColor;
    property FlagColorIndex:byte read GetColorIndex;
    property Alliances[Index:integer]:TAllianceType read GetAlliances write SetAlliances;
    property CenterScreen:TKMPoint read fCenterScreen write fCenterScreen;

    procedure AfterMissionInit(aFlattenRoads:boolean);
    procedure SkipWinConditionCheck;
    procedure SkipDefeatConditionCheck;

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false): TKMUnit; reintroduce;
    procedure AddUnitAndLink(aUnitType: TUnitType; Position: TKMPoint);
    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
    procedure TrainingDone(aUnit: TKMUnit);

    function CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;

    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
    function AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace:boolean):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint);
    procedure AddRoadsToList(aLoc: TKMPoint);
    procedure AddRoadConnect(LocA,LocB:TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType: TFieldType);
    procedure AddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
    procedure AddHouseWIP(aHouseType: THouseType; aLoc: TKMPoint; out House: TKMHouse);
    procedure RemHouse(Position: TKMPoint; DoSilent:boolean; IsEditor:boolean=false);
    procedure RemHousePlan(Position: TKMPoint);
    procedure RemFieldPlan(Position: TKMPoint);
    function FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; Index:byte=1): TKMHouse; overload;
    function FindHouse(aType:THouseType; Index:byte=1): TKMHouse; overload;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    procedure GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);

    function GetFieldsCount: Integer;

    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Load(LoadStream:TKMemoryStream); override;
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
uses KM_Terrain, KM_Sound, KM_PlayersCollection, KM_Resource, KM_ResourceHouse, KM_Units_Warrior;


{ TKMPlayerCommon }
constructor TKMPlayerCommon.Create(aPlayerIndex:TPlayerIndex);
begin
  Inherited Create;
  fPlayerIndex  := aPlayerIndex;
  fUnits        := TKMUnitsCollection.Create;
end;


destructor TKMPlayerCommon.Destroy;
begin
  FreeThenNil(fUnits);
  Inherited;
end;


function TKMPlayerCommon.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
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
constructor TKMPlayer.Create(aPlayerIndex:TPlayerIndex);
var i: integer;
begin
  Inherited Create (aPlayerIndex);
  fAI           := TKMPlayerAI.Create(fPlayerIndex);
  fFogOfWar     := TKMFogOfWar.Create(fTerrain.MapX, fTerrain.MapY);
  fGoals        := TKMGoals.Create;
  fStats        := TKMPlayerStats.Create;
  fRoadsList    := TKMPointList.Create;
  fHouses       := TKMHousesCollection.Create;
  fDeliverList  := TKMDeliverQueue.Create;
  fBuildList   := TKMBuildList.Create;
  fArmyEval     := TKMArmyEvaluation.Create(Self);

  fPlayerName   := 'Player ' + IntToStr(aPlayerIndex);
  fPlayerType   := pt_Computer;
  for i:=0 to MAX_PLAYERS-1 do
    fAlliances[i] := at_Enemy; //Everyone is enemy by default

  fSkipWinConditionCheck := false;
  fSkipDefeatConditionCheck := false;
  fFlagColor := DefaultTeamColors[fPlayerIndex]; //Init with default color, later replaced by Script
end;


//Destruction order is important as Houses and Units need to access
//Stats/Deliveries and other collection in their Destroy/Abandon/Demolish methods
destructor TKMPlayer.Destroy;
begin
  Inherited;
  FreeThenNil(fArmyEval);
  FreeThenNil(fRoadsList);
  FreeThenNil(fHouses);

  //Should be freed after Houses and Units, as they write to it on Destroy
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
function TKMPlayer.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false):TKMUnit;
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
function TKMPlayer.TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
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


function TKMPlayer.AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
begin
  Result := fUnits.AddGroup(fPlayerIndex, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount, aMapEditor);
  //Add unit to statistic inside the function for some units may not fit on map
end;


function TKMPlayer.AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace:boolean):TKMHouse;
begin
  Result := fHouses.AddHouse(aHouseType, PosX, PosY, fPlayerIndex, RelativeEntrace);
end;


procedure TKMPlayer.AddRoad(aLoc: TKMPoint);
begin
  //if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  //The AddPlan function should do the check, but if we enforce it here then it will create lots of problems
  //with the original missions. (I've also seem some fan missions where they have road over wrong tiles)

  fTerrain.SetRoad(aLoc, fPlayerIndex);
end;


procedure TKMPlayer.AddRoadsToList(aLoc: TKMPoint);
begin
  if fRoadsList=nil then exit;
  fRoadsList.AddEntry(aLoc);
end;


//Lay out all roads at once to save time on Terrain lighting/passability recalculations
procedure TKMPlayer.AfterMissionInit(aFlattenRoads:boolean);
var i : Integer;
begin
  if fRoadsList<>nil then begin
    fTerrain.SetRoads(fRoadsList,fPlayerIndex);
    if aFlattenRoads then fTerrain.FlattenTerrain(fRoadsList);
    FreeAndNil(fRoadsList);
  end;
  for i := 0 to fPlayers.Count-1 do
    if fPlayerIndex <> i then fArmyEval.AddEnemy(fPlayers[i]);
end;


procedure TKMPlayer.SetPlayerID(aNewIndex:TPlayerIndex);
begin
  fPlayerIndex := aNewIndex;
  fUnits.OwnerUpdate(aNewIndex);
  fHouses.OwnerUpdate(aNewIndex);
  fAI.OwnerUpdate(aNewIndex);
end;


function TKMPlayer.CanAddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType): Boolean;
begin
  Result := fTerrain.CanAddField(aLoc, aFieldType)
            and (fFogOfWar.CheckTileRevelation(aLoc.X, aLoc.Y, False) > 0)
            and (fBuildList.FieldworksList.HasField(aLoc) = ft_None)
            and not fBuildList.HousePlanList.HasPlan(aLoc);
end;


procedure TKMPlayer.AddField(aLoc: TKMPoint; aFieldType: TFieldType);
begin
  fTerrain.SetField(aLoc, fPlayerIndex, aFieldType);
end;


{DoSilent means that there will be no sound when markup is placed, needed e.g. when script used}
procedure TKMPlayer.AddFieldPlan(aLoc: TKMPoint; aFieldType: TFieldType);
begin
  Assert(aFieldType in [ft_Road, ft_Corn, ft_Wine, ft_Wall], 'Placing wrong FieldType');

  if CanAddFieldPlan(aLoc, aFieldType) then
  begin
    fBuildList.FieldworksList.AddField(aLoc, aFieldType);
    if Self = MyPlayer then
      fSoundLib.Play(sfx_placemarker);
  end
  else
    if Self = MyPlayer then
      fSoundLib.Play(sfx_CantPlace, aLoc, False, 4.0);
end;


//Used mainly for testing purposes
procedure TKMPlayer.AddRoadConnect(LocA,LocB:TKMPoint);
var
  NodeList: TKMPointList;
  RoadExists: Boolean;
  i: integer;
begin
  NodeList := TKMPointList.Create;
  try
    RoadExists := fTerrain.PathFinding.Route_Make(LocA, LocB, CanMakeRoads, 0, nil, NodeList);

    if RoadExists then
      for i:=1 to NodeList.Count do
        AddRoad(NodeList.List[i]);
  finally
    FreeAndNil(NodeList);
  end;
end;


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
  fTerrain.SetHouse(aLoc, aHouseType, hs_Fence, fPlayerIndex);
  fStats.HouseStarted(aHouseType);
end;


//Player wants to remove own house
procedure TKMPlayer.RemHouse(Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
var H: TKMHouse;
begin
  if not DoSilent then fSoundLib.Play(sfx_Click);

  H := fHouses.HitTest(Position.X, Position.Y);
  if (H = nil) and IsEditor then Exit; //Editor is allowed to ask to remove non-existant house
  Assert(H <> nil);

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
  Assert(fResource.HouseDat[HT].IsValid);
  fBuildList.HousePlanList.RemPlan(Position);
  fStats.HousePlanRemoved(HT);
  if Self = MyPlayer then fSoundLib.Play(sfx_Click);
end;


procedure TKMPlayer.RemFieldPlan(Position: TKMPoint);
begin
  fBuildList.FieldworksList.RemFieldPlan(Position);
  if Self = MyPlayer then fSoundLib.Play(sfx_Click);
end;


function TKMPlayer.FindHouse(aType: THouseType; aPosition: TKMPoint; Index:byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;


function TKMPlayer.FindHouse(aType:THouseType; Index:byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, 0, 0, Index);
end;


function TKMPlayer.FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
var
  H: TKMHouseInn;
  i: integer;
  Dist, BestMatch: single;
begin
  //This function will return the best inn for a unit at Loc, base on distance, food available and space available.
  //Will return nil if no suitable inn is available
  Result := nil;
  i:=1;
  BestMatch := 9999;
  if UnitIsAtHome then inc(Loc.Y); //From outside the door of the house

  H := TKMHouseInn(FindHouse(ht_Inn));
  repeat
    //First make sure that it is valid
    if (H<>nil)and(H.HasFood)and(H.HasSpace)and(fTerrain.Route_CanBeMade(Loc,KMPointBelow(H.GetEntrance),aUnit.GetDesiredPassability(true),0, false)) then
    begin
      //Take the closest inn out of the ones that are suitable
      Dist := GetLength(H.GetPosition,Loc);
      if Dist < BestMatch then
      begin
        Result := H;
        BestMatch := Dist;
      end;
    end;

    inc(i);
    H:=TKMHouseInn(FindHouse(ht_Inn,i));
  until(H = nil);
end;


function TKMPlayer.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;


function TKMPlayer.GetColorIndex:byte;
var i:integer;
begin
  Result := 3; //3 = Black which can be the default when a non-palette 32 bit color value is used
  for i:=0 to 255 do
    if fResource.Palettes.DefDal.Color32(i) = fFlagColor then
      Result := i;
end;


function  TKMPlayer.GetAlliances(Index:integer):TAllianceType;
begin
  Result := fAlliances[Index];
end;


procedure TKMPlayer.SetAlliances(Index:integer; aValue:TAllianceType);
begin
  fAlliances[Index] := aValue;
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


procedure TKMPlayer.GetHouseMarks(aLoc: TKMPoint; aHouseType: THouseType; aList: TKMPointTagList);
var
  i,k,s,t:integer;
  P2:TKMPoint;
  AllowBuild:boolean;
  HA: THouseArea;

  //Replace existing icon with a Block
  procedure BlockPoint(aPoint: TKMPoint; aID: Integer);
  var v: integer; Replaced: Boolean;
  begin
    Replaced := False;
    for v := aList.Count downto 1 do
      if KMSamePoint(aList.List[v], aPoint) then
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
    AllowBuild := (fFogOfWar.CheckTileRevelation(P2.X, P2.Y, False) > 0)
                  and (fBuildList.FieldworksList.HasField(P2) = ft_None);

    //Check surrounding tiles in +/- 1 range for other houses pressence
    for s:=-1 to 1 do for t:=-1 to 1 do
    if (s<>0) or (t<>0) then //This is a surrounding tile, not the actual tile
    if fBuildList.HousePlanList.HasPlan(KMPoint(P2.X+s,P2.Y+t)) then
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


procedure TKMPlayer.Save(SaveStream:TKMemoryStream);
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


procedure TKMPlayer.Load(LoadStream:TKMemoryStream);
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
  Inherited;
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
  Inherited;
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
