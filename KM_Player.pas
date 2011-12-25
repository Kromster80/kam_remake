unit KM_Player;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_PlayerAI, KM_Units, KM_Houses, KM_DeliverQueue,
  KM_PlayerStats, KM_Goals, KM_FogOfWar, KM_ArmyEvaluation, KM_Points;


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
    fBuildList:TKMBuildingQueue;
    fRepairList: TKMRepairQueue;
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
    property BuildList:TKMBuildingQueue read fBuildList;
    property RepairList:TKMRepairQueue read fRepairList;
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
    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
    function AddHouse(aHouseType: THouseType; PosX, PosY:word; RelativeEntrace:boolean):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint);
    procedure AddRoadsToList(aLoc: TKMPoint);
    procedure AddRoadConnect(LocA,LocB:TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType:TFieldType);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean);
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean);
    function RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
    function RemPlan(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false):boolean;
    function FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; Index:byte=1): TKMHouse; overload;
    function FindHouse(aType:THouseType; Index:byte=1): TKMHouse; overload;
    function HousesHitTest(X, Y: Integer): TKMHouse;

    function GetFieldsCount:integer;

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
uses KM_Terrain, KM_Sound, KM_PlayersCollection, KM_ResourceGFX;


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
var s: string;
begin
  LoadStream.Read(s);
  Assert(s = 'PlayerCommon');
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


{ TKMPlayerAssets }
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
  fBuildList    := TKMBuildingQueue.Create;
  fRepairList   := TKMRepairQueue.Create;
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
  FreeThenNil(fRepairList);
  FreeThenNil(fAI);
end;


//Add unit of aUnitType to Position
//AutoPlace - add unit to nearest available spot if Position is already taken (or unwalkable)
//WasTrained - the uniot was trained by player and therefor counted by Stats
function TKMPlayer.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false):TKMUnit;
begin
  Result := Inherited AddUnit(aUnitType, Position, AutoPlace);

  if Result <> nil then
    fStats.UnitCreated(aUnitType, WasTrained);
end;


//Start training unit in school/barracks
//User can cancel the training, so we don't add unit to stats just yet
function TKMPlayer.TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
begin
  Result := fUnits.Add(fPlayerIndex, aUnitType, Position.X, Position.Y, false);
  //Do not add unit to statistic just yet, wait till it's training complete
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


procedure TKMPlayer.AddField(aLoc: TKMPoint; aFieldType:TFieldType);
begin
  fTerrain.SetField(aLoc,fPlayerIndex,aFieldType);
end;


{DoSilent means that there will be no sound when markup is placed, needed e.g. when script used}
procedure TKMPlayer.AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean);
begin
  if not fTerrain.CanPlaceRoad(aLoc,aMarkup,Self) then
  begin
    if not DoSilent then
      fSoundLib.Play(sfx_CantPlace,aLoc,false,4.0);
    exit;
  end;
  fTerrain.SetMarkup(aLoc, aMarkup);
  case aMarkup of
    mu_RoadPlan: BuildList.AddNewRoad(aLoc, ft_Road);
    mu_FieldPlan: BuildList.AddNewRoad(aLoc, ft_Corn);
    mu_WinePlan: BuildList.AddNewRoad(aLoc, ft_Wine);
    mu_WallPlan: BuildList.AddNewRoad(aLoc, ft_Wall);
    else Assert(false,'Wrong markup');
  end;
  if not DoSilent then
    fSoundLib.Play(sfx_placemarker);
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


procedure TKMPlayer.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean);
var KMHouse:TKMHouse; Loc:TKMPoint;
begin
  Loc.X := aLoc.X - fResource.HouseDat[aHouseType].EntranceOffsetX;
  Loc.Y := aLoc.Y;
  KMHouse := fHouses.AddPlan(aHouseType, Loc.X, Loc.Y, fPlayerIndex);
  fTerrain.SetHouse(Loc, aHouseType, hs_Plan, fPlayerIndex);
  fStats.HouseStarted(aHouseType);
  BuildList.AddNewHousePlan(KMHouse);
  if not DoSilent then fSoundLib.Play(sfx_placemarker);
end;


//Player wants to remove own house
function TKMPlayer.RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
var fHouse:TKMHouse;
begin
  Result := BuildList.CancelHousePlan(Position,Simulated);
  if Result and not DoSilent then fSoundLib.Play(sfx_Click);
  fHouse := fHouses.HitTest(Position.X, Position.Y);
  if fHouse<>nil then
  begin
    if not Simulated then begin
      fHouse.DemolishHouse(DoSilent,IsEditor);
      if fHouse.BuildingState = hbs_Done then //Only Done houses are treated as Self-Destruct
        fStats.HouseSelfDestruct(fHouse.HouseType)
      else
        fStats.HouseEnded(fHouse.HouseType);
    end;
    Result := true;
  end;
end;


function TKMPlayer.RemPlan(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false):boolean;
begin
  Result := BuildList.CancelRoad(Position,Simulated);
  if (Result) and (not Simulated) then
  begin
    if not DoSilent then fSoundLib.Play(sfx_Click);
    fTerrain.RemMarkup(Position);
  end;
end;


function TKMPlayer.FindHouse(aType:THouseType; aPosition: TKMPoint; Index:byte=1): TKMHouse;
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
    if fResource.Palettes[DEF_PAL].Color32(i) = fFlagColor then
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
function TKMPlayer.GetFieldsCount:integer;
var i,k:integer;
begin
  Result := 0;
  for i:=1 to fTerrain.MapY do
  for k:=1 to fTerrain.MapX do
    if fTerrain.Land[i,k].TileOwner = fPlayerIndex then
      inc(Result);
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
  fRepairList.Save(SaveStream);
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
  fRepairList.Load(LoadStream);
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
  fRepairList.SyncLoad;
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
