unit KM_Player;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
      KM_Defaults, KM_Utils,
      KM_Units, KM_Houses, KM_DeliverQueue, KM_CommonTypes, KM_PlayerStats, KM_Goals;


type
  TPlayerType = (
        pt_None, //used in Networking setup for missing players
        pt_Human, 
        pt_Computer);


type
  TKMPlayerAssets = class
  private
    fBuildList: TKMBuildingQueue;
    fDeliverList: TKMDeliverQueue;
    fHouses: TKMHousesCollection;
    fUnits: TKMUnitsCollection;
    fRoadsList:TKMPointList; //Used only once to speedup mission loading, then freed
    fStats: TKMPlayerStats;
    fGoals: TKMGoals;

    fPlayerID:TPlayerID; //Which ID this player is
    fPlayerType:TPlayerType;
    fFlagColor:cardinal;

    fSkipWinConditionCheck:boolean;
    fSkipDefeatConditionCheck:boolean;

    procedure SetPlayerType(aType:TPlayerType);
    function GetColorIndex:byte;

  public
    fAlliances: array[1..MAX_PLAYERS] of TAllianceType;

    constructor Create(aPlayerID:TPlayerID);
    destructor Destroy; override;

    property BuildList:TKMBuildingQueue read fBuildList;
    property DeliverList:TKMDeliverQueue read fDeliverList;
    property Houses:TKMHousesCollection read fHouses;
    property Units:TKMUnitsCollection read fUnits;
    property Stats:TKMPlayerStats read fStats;
    property Goals:TKMGoals read fGoals;

    property PlayerID:TPlayerID read fPlayerID;
    property PlayerType:TPlayerType read fPlayerType write SetPlayerType; //Is it Human or AI or Animals
    property FlagColor:cardinal read fFlagColor write fFlagColor;
    property FlagColorIndex:byte read GetColorIndex;

    procedure SkipWinConditionCheck;
    procedure SkipDefeatConditionCheck;

  public

    procedure AfterMissionInit(aFlattenRoads:boolean);

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false): TKMUnit;
    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
    function AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint; DoFlatten:boolean=true);
    procedure AddRoadsToList(aLoc: TKMPoint);
    procedure AddRoadConnect(LocA,LocB:TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType:TFieldType);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none);
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
    function RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
    function RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
    function RemPlan(Position: TKMPoint; Simulated:boolean=false):boolean;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; const Index:byte=1): TKMHouse; overload;
    function FindHouse(aType:THouseType; const Index:byte=1): TKMHouse; overload;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    function GetHouseByID(aID: Integer): TKMHouse;
    function GetUnitByID(aID: Integer): TKMUnit;
    procedure GetUnitLocations(out Loc:TKMPointList);

    function GetHouseWip(aType:THouseType):integer;
    function GetFieldsCount:integer;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure IncAnimStep;
    procedure UpdateState;
    procedure Paint;
  end;


type
  TKMPlayerAnimals = class
  private
    fUnits: TKMUnitsCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property Units:TKMUnitsCollection read fUnits;

    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
    function RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
    function GetUnitByID(aID: Integer): TKMUnit;
    function GetFishInWaterBody(aWaterID:byte; FindHighestCount:boolean=true): TKMUnitAnimal;
    procedure GetFishLocations(out Loc:TKMPointList);
    function UnitsHitTest(X, Y: Integer): TKMUnit;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Terrain, KM_Sound, KM_PathFinding, KM_PlayersCollection, KM_ResourceGFX;


{ TKMPlayerAssets }
constructor TKMPlayerAssets.Create(aPlayerID:TPlayerID);
var i: integer;
begin
  Inherited Create;
  fPlayerID     := aPlayerID;
  fPlayerType   := pt_Computer;
  fGoals        := TKMGoals.Create;
  fStats        := TKMPlayerStats.Create;
  fRoadsList    := TKMPointList.Create;
  fUnits        := TKMUnitsCollection.Create;
  fHouses       := TKMHousesCollection.Create;
  fDeliverList  := TKMDeliverQueue.Create;
  fBuildList    := TKMBuildingQueue.Create;
  for i:=1 to MAX_PLAYERS do
    fAlliances[i] := at_Enemy; //Everyone is enemy by default

  fSkipWinConditionCheck := false;
  fSkipDefeatConditionCheck := false;
  fFlagColor := DefaultTeamColors[byte(aPlayerID)]; //Init with default color, later replaced by Script
end;


destructor TKMPlayerAssets.Destroy;
begin
  FreeThenNil(fRoadsList);
  FreeThenNil(fUnits);
  FreeThenNil(fHouses);
  FreeThenNil(fStats); //Used by Houses and Units
  FreeThenNil(fGoals);
  FreeThenNil(fDeliverList);
  FreeThenNil(fBuildList);
  Inherited;
end;


function TKMPlayerAssets.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false):TKMUnit;
begin
  //Animals must get redirected to animal player
  if aUnitType in [ut_Wolf..ut_Duck] then
  begin
    Result := fPlayers.PlayerAnimals.AddUnit(aUnitType,Position,AutoPlace);
    exit;
  end;

  Result := fUnits.Add(fPlayerID, aUnitType, Position.X, Position.Y, AutoPlace);
  if Result <> nil then
    fStats.UnitCreated(aUnitType, WasTrained);
end;


function TKMPlayerAssets.TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
begin
  Result := fUnits.Add(fPlayerID, aUnitType, Position.X, Position.Y, false);
  //Do not add unit to statistic just yet, wait till it's training complete
end;


function TKMPlayerAssets.AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
begin
  Result := fUnits.AddGroup(fPlayerID, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount, aMapEditor);
  //Add unit to statistic inside the function for some units may not fit on map
end;


function TKMPlayerAssets.AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
var xo:integer;
begin
  xo:=HouseDAT[byte(aHouseType)].EntranceOffsetX;
  Result:=fHouses.AddHouse(aHouseType, Position.X-xo, Position.Y, fPlayerID);
end;


procedure TKMPlayerAssets.AddRoad(aLoc: TKMPoint; DoFlatten:boolean=true);
begin
  //if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  //The AddPlan function should do the check, but if we enforce it here then it will create lots of problems
  //with the original missions. (I've also seem some fan missions where they have road over wrong tiles)

  fTerrain.SetRoad(aLoc,fPlayerID);
  if DoFlatten then
    fTerrain.FlattenTerrain(aLoc); //Flatten the terrain for road
end;


procedure TKMPlayerAssets.AddRoadsToList(aLoc: TKMPoint);
begin
  if fRoadsList=nil then exit;
  fRoadsList.AddEntry(aLoc);
end;


procedure TKMPlayerAssets.AfterMissionInit(aFlattenRoads:boolean);
begin
  if fRoadsList<>nil then begin
    fTerrain.SetRoads(fRoadsList,fPlayerID);
    if aFlattenRoads then fTerrain.FlattenTerrain(fRoadsList);
    FreeAndNil(fRoadsList);
  end;
end;


procedure TKMPlayerAssets.AddField(aLoc: TKMPoint; aFieldType:TFieldType);
begin
  fTerrain.SetField(aLoc,fPlayerID,aFieldType);
end;


{DoSilent means that there will be no sound when markup is placed, needed e.g. when script used}
procedure TKMPlayerAssets.AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none);
begin
  if not fTerrain.CanPlaceRoad(aLoc,aMarkup,PlayerRevealID) then
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
    else fLog.AssertToLog(false,'Wrong markup');
  end;
  if not DoSilent then
    fSoundLib.Play(sfx_placemarker);
end;


//Used mainly for testing purposes
procedure TKMPlayerAssets.AddRoadConnect(LocA,LocB:TKMPoint);
var fPath:TPathFinding; i:integer; NodeList:TKMPointList;
begin
  fPath := TPathFinding.Create(LocA, LocB, CanMakeRoads, 0, nil);
  NodeList:=TKMPointList.Create;
  fPath.ReturnRoute(NodeList);
  fPath.Free;

  for i:=1 to NodeList.Count do
    AddRoad(NodeList.List[i]);

  FreeAndNil(NodeList);
end;


procedure TKMPlayerAssets.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint);
var KMHouse:TKMHouse; Loc:TKMPoint;
begin
  Loc.X := aLoc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX;
  Loc.Y := aLoc.Y;
  KMHouse := fHouses.AddPlan(aHouseType, Loc.X, Loc.Y, fPlayerID);
  fTerrain.SetHouse(Loc, aHouseType, hs_Plan, fPlayerID);
  BuildList.AddNewHousePlan(KMHouse);
end;


//Player wants to remove own house
function TKMPlayerAssets.RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
var fHouse:TKMHouse;
begin
  Result := BuildList.CancelHousePlan(Position,Simulated);
  fHouse := fHouses.HitTest(Position.X, Position.Y);
  if fHouse<>nil then
  begin
    if not Simulated then begin
      fHouse.DemolishHouse(DoSilent,IsEditor);
      fStats.HouseSelfDestruct(fHouse.GetHouseType);
    end;
    Result := true;
  end;
end;


function TKMPlayerAssets.RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
var FoundUnit:TKMUnit;
begin
  Result := false;
  FoundUnit := fUnits.HitTest(Position.X, Position.Y);
  if FoundUnit<>nil then
  begin
    if not Simulated then
      fUnits.RemoveUnit(FoundUnit);
    Result := true;
  end;
end;


function TKMPlayerAssets.RemPlan(Position: TKMPoint; Simulated:boolean=false):boolean;
begin
  Result := BuildList.CancelRoad(Position,Simulated);
  if (Result) and (not Simulated) then
  begin
    fSoundLib.Play(sfx_Click);
    fTerrain.RemMarkup(Position);
  end;
end;


function TKMPlayerAssets.FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
begin
  Result:=fHouses.FindEmptyHouse(aUnitType, Loc);
end;


function TKMPlayerAssets.FindHouse(aType:THouseType; aPosition: TKMPoint; const Index:byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;


function TKMPlayerAssets.FindHouse(aType:THouseType; const Index:byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, 0, 0, Index);
end;


function TKMPlayerAssets.FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
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
     if (H<>nil)and(H.HasFood)and(H.HasSpace)and(fTerrain.Route_CanBeMade(Loc,KMPointY1(H.GetEntrance),aUnit.GetDesiredPassability(true),0, false)) then
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


function TKMPlayerAssets.UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
begin
  Result:= fUnits.HitTest(X, Y, UT);
end;


function TKMPlayerAssets.GetHouseByID(aID: Integer): TKMHouse;
begin
  Result := fHouses.GetHouseByID(aID);
end;


function TKMPlayerAssets.GetUnitByID(aID: Integer): TKMUnit;
begin
  Result := fUnits.GetUnitByID(aID);
end;


procedure TKMPlayerAssets.GetUnitLocations(out Loc:TKMPointList);
begin
  fUnits.GetLocations(Loc, ut_Any);
end;


function TKMPlayerAssets.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;


procedure TKMPlayerAssets.SetPlayerType(aType:TPlayerType);
begin
  case aType of
    pt_None:      begin
                    //todo: Erase player data
                  end;
    pt_Human,
    pt_Computer:  fPlayerType := aType;
    else Assert(false, 'Unexpected player type');
  end;
end;


function TKMPlayerAssets.GetColorIndex:byte;
var i:integer;
begin
  Result := 3; //3 = Black which can be the default when a non-pallete 32 bit color value is used
  for i:=0 to 255 do
    if fResource.GetColor32(i, DEF_PAL) = fFlagColor then
      Result := i;
end;


function TKMPlayerAssets.GetHouseWip(aType:THouseType):integer;
begin
  Result := fBuildList.GetHouseWipCount(aType);
end;


{ See if player owns any Fields/Roads/Walls (has any assets on Terrain)
  Queried by MapEditor>SaveDAT;
  Might also be used to show Players strength (or builder/warrior balance) in Tavern
  If Player has none and no Units/Houses we can assume it's empty and does not needs to be saved }
function TKMPlayerAssets.GetFieldsCount:integer;
var i,k:integer;
begin
  Result := 0;
  for i:=1 to fTerrain.MapY do
  for k:=1 to fTerrain.MapX do
    if fTerrain.Land[i,k].TileOwner = fPlayerID then
      inc(Result);
end;


procedure TKMPlayerAssets.SkipWinConditionCheck;
begin
  fSkipWinConditionCheck := true;
end;


procedure TKMPlayerAssets.SkipDefeatConditionCheck;
begin
  fSkipDefeatConditionCheck := true;
end;


procedure TKMPlayerAssets.Save(SaveStream:TKMemoryStream);
begin
  fUnits.Save(SaveStream);
  fHouses.Save(SaveStream);
  fDeliverList.Save(SaveStream);
  fBuildList.Save(SaveStream);
  fStats.Save(SaveStream);
  fGoals.Save(SaveStream);

  SaveStream.Write(fPlayerID, SizeOf(fPlayerID));
  SaveStream.Write(fPlayerType, SizeOf(fPlayerType));
  SaveStream.Write(fAlliances, SizeOf(fAlliances));
  SaveStream.Write(fSkipWinConditionCheck);
  SaveStream.Write(fSkipDefeatConditionCheck);
  SaveStream.Write(fFlagColor);
end;


procedure TKMPlayerAssets.Load(LoadStream:TKMemoryStream);
begin
  fUnits.Load(LoadStream);
  fHouses.Load(LoadStream);
  fDeliverList.Load(LoadStream);
  fBuildList.Load(LoadStream);
  fStats.Load(LoadStream);
  fGoals.Load(LoadStream);

  LoadStream.Read(fPlayerID, SizeOf(fPlayerID));
  LoadStream.Read(fPlayerType, SizeOf(fPlayerType));
  LoadStream.Read(fAlliances, SizeOf(fAlliances));
  LoadStream.Read(fSkipWinConditionCheck);
  LoadStream.Read(fSkipDefeatConditionCheck);
  LoadStream.Read(fFlagColor);
end;


procedure TKMPlayerAssets.SyncLoad;
begin
  fUnits.SyncLoad;
  fHouses.SyncLoad;
  fDeliverList.SyncLoad;
  fBuildList.SyncLoad;
end;


procedure TKMPlayerAssets.IncAnimStep;
begin
  fHouses.IncAnimStep;
end;


procedure TKMPlayerAssets.UpdateState;
begin
  fUnits.UpdateState;
  fHouses.UpdateState;
end;


procedure TKMPlayerAssets.Paint;
begin
  fUnits.Paint;
  fHouses.Paint;
end;


{ TKMPlayerAnimals }
procedure TKMPlayerAnimals.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write('Animals');
  fUnits.Save(SaveStream);
end;


procedure TKMPlayerAnimals.Load(LoadStream:TKMemoryStream);
var s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Animals');
  fUnits.Load(LoadStream);
end;


procedure TKMPlayerAnimals.SyncLoad;
begin
  fUnits.SyncLoad;
end;


function TKMPlayerAnimals.UnitsHitTest(X, Y: Integer): TKMUnit;
begin
  Result := fUnits.HitTest(X,Y);
end;


procedure TKMPlayerAnimals.UpdateState;
begin
  fUnits.UpdateState;
end;


procedure TKMPlayerAnimals.Paint;
begin
  fUnits.Paint;
end;


constructor TKMPlayerAnimals.Create;
begin
  Inherited;
  fUnits := TKMUnitsCollection.Create;
end;


destructor TKMPlayerAnimals.Destroy;
begin
  FreeThenNil(fUnits);
  Inherited;
end;


function TKMPlayerAnimals.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
begin
  Result := fUnits.Add(play_animals, aUnitType, Position.X, Position.Y, AutoPlace);
end;


function TKMPlayerAnimals.RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
var FoundUnit:TKMUnit;
begin
  Result := false;
  FoundUnit := fUnits.HitTest(Position.X, Position.Y);
  if FoundUnit<>nil then
  begin
    if not Simulated then
      fUnits.RemoveUnit(FoundUnit);
    Result := true;
  end;
end;


function TKMPlayerAnimals.GetUnitByID(aID: Integer): TKMUnit;
begin
  Result := fUnits.GetUnitByID(aID);
end;


function TKMPlayerAnimals.GetFishInWaterBody(aWaterID:byte; FindHighestCount:boolean=true): TKMUnitAnimal;
var i, HighestGroupCount: integer;
begin
  Result := nil;
  if aWaterID = 0 then exit; //Fish should always be in valid water
  HighestGroupCount := 0;
  with fUnits do
  begin
    for i:=0 to Count-1 do
    if (fUnits.Items[i] <> nil) and (TKMUnit(fUnits.Items[i]).UnitType = ut_Fish) then
    begin
      if fTerrain.Land[TKMUnit(fUnits.Items[i]).GetPosition.Y,TKMUnit(fUnits.Items[i]).GetPosition.X].WalkConnect[wcFish] = aWaterID then
        if TKMUnitAnimal(fUnits.Items[i]).fFishCount > HighestGroupCount then
        begin
          Result := TKMUnitAnimal(fUnits.Items[i]);
          if not FindHighestCount then exit; //This is for time saving when we don't actually care which group is returned
          HighestGroupCount := TKMUnitAnimal(fUnits.Items[i]).fFishCount;
        end;
    end;
  end;
end;


procedure TKMPlayerAnimals.GetFishLocations(out Loc:TKMPointList);
begin
  fUnits.GetLocations(Loc, ut_Fish);
end;


end.
