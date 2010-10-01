unit KM_Player;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, KM_Defaults, KM_Units, KM_Houses, KM_DeliverQueue, KM_Settings, KM_CommonTypes, KM_Utils;


type
  TPlayerType = (pt_Human, pt_Computer, pt_Animals);


type
  TKMPlayerAssets = class
  private
    fRoadsList:TKMPointList;
    fUnits: TKMUnitsCollection;
    fHouses: TKMHousesCollection;
    fDeliverList: TKMDeliverQueue;
    fBuildList: TKMBuildingQueue;
  public
    fAlliances: array[1..MAX_PLAYERS] of TAllianceType;
    SkipWinConditionCheck: boolean;
    constructor Create(aPlayerID:TPlayerID);
    destructor Destroy; override;
  public
    fMissionSettings: TMissionSettings; //Required to be public so it can be accessed from LoadDAT
    PlayerID:TPlayerID; //Which ID this player is
    PlayerType: TPlayerType; //Is it Human or AI or Animals
    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true; WasTrained:boolean=false): TKMUnit;
    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
    function AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint; DoFlatten:boolean=true);
    procedure AddRoadsToList(aLoc: TKMPoint);
    procedure AfterMissionInit(aFlattenRoads:boolean);
    procedure AddField(aLoc: TKMPoint; aFieldType:TFieldType);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none);
    procedure AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; PlayerRevealID:TPlayerID=play_none);
    procedure AutoRoadConnect(LocA,LocB:TKMPoint);
    function RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
    function RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
    function RemPlan(Position: TKMPoint; Simulated:boolean=false):boolean;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindInn(Loc:TKMPoint; aUnit:TKMUnit; UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; const Index:byte=1): TKMHouse; overload;
    function FindHouse(aType:THouseType; const Index:byte=1): TKMHouse; overload;
    function UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    function GetHouseByID(aID: Integer): TKMHouse;
    function GetUnitByID(aID: Integer): TKMUnit;
    procedure GetUnitLocations(out Loc:TKMPointList);
    function HousesHitTest(X, Y: Integer): TKMHouse;
    property DeliverList:TKMDeliverQueue read fDeliverList;
    property BuildList:TKMBuildingQueue read fBuildList;

    procedure CreatedHouse(aType:THouseType; aWasBuilt:boolean);
    procedure CreatedUnit(aType:TUnitType; aWasTrained:boolean);
    procedure DestroyedHouse(aType:THouseType);
    procedure DestroyedUnit(aType:TUnitType);
    procedure UpdateReqDone(aType:THouseType);

    function GetCanBuild(aType:THouseType):boolean;
    function GetHouseQty(aType:THouseType):integer;
    function GetTotalHouseQty():integer;
    function GetUnitQty(aType:TUnitType):integer;
    function GetHouseCount():integer;
    function GetUnitCount():integer;
    property GetHouses:TKMHousesCollection read fHouses;
    property GetUnits:TKMUnitsCollection read fUnits;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
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
    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
    function RemUnit(Position: TKMPoint; Simulated:boolean=false):boolean;
    function GetUnitByID(aID: Integer): TKMUnit;
    function GetFishInWaterBody(aWaterID:byte; FindHighestCount:boolean=true): TKMUnitAnimal;
    procedure GetFishLocations(out Loc:TKMPointList);
    function GetUnitCount: integer;
    function GetUnitByIndex(aIndex:integer): TKMUnit;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    procedure UpdateState;
    procedure Paint;
  end;

implementation
uses KM_Terrain, KM_Sound, KM_PathFinding, KM_PlayersCollection;


{ TKMPlayerAssets }
constructor TKMPlayerAssets.Create(aPlayerID:TPlayerID);
var i: integer;
begin
  Inherited Create;
  PlayerID      := aPlayerID;
  PlayerType    := pt_Computer;
  fMissionSettings := TMissionSettings.Create;
  fRoadsList    := TKMPointList.Create; //Used only once on mission loading, then freed
  fUnits        := TKMUnitsCollection.Create;
  fHouses       := TKMHousesCollection.Create;
  fDeliverList  := TKMDeliverQueue.Create;
  fBuildList    := TKMBuildingQueue.Create;
  for i:=1 to MAX_PLAYERS do
    fAlliances[i] := at_Enemy; //Everyone is enemy by default
  SkipWinConditionCheck := false;
end;


destructor TKMPlayerAssets.Destroy;
begin
  FreeThenNil(fMissionSettings);
  FreeThenNil(fRoadsList);
  FreeThenNil(fUnits);
  FreeThenNil(fHouses);
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

  Result := fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y, AutoPlace);
  if Result <> nil then
    CreatedUnit(aUnitType, WasTrained);
end;


function TKMPlayerAssets.TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
begin
  Result := fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y, false);
  //Do not add unit to statistic just yet, wait till it's training complete
end;


function TKMPlayerAssets.AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word; aMapEditor:boolean=false):TKMUnit;
begin
  Result := fUnits.AddGroup(PlayerID, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount, aMapEditor);
  //Add unit to statistic inside the function for some units may not fit on map
end;


function TKMPlayerAssets.AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
var xo:integer;
begin
  xo:=HouseDAT[byte(aHouseType)].EntranceOffsetX;
  Result:=fHouses.AddHouse(aHouseType, Position.X-xo, Position.Y, PlayerID);
end;


procedure TKMPlayerAssets.AddRoad(aLoc: TKMPoint; DoFlatten:boolean=true);
begin
  //if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  //The AddPlan function should do the check, but if we enforce it here then it will create lots of problems
  //with the original missions. (I've also seem some fan missions where they have road over wrong tiles)

  fTerrain.SetRoad(aLoc,PlayerID);
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
    fTerrain.SetRoads(fRoadsList,PlayerID);
    if aFlattenRoads then fTerrain.FlattenTerrain(fRoadsList);
    FreeAndNil(fRoadsList);
  end;
end;


procedure TKMPlayerAssets.AddField(aLoc: TKMPoint; aFieldType:TFieldType);
begin
  fTerrain.SetField(aLoc,PlayerID,aFieldType);
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

procedure TKMPlayerAssets.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; PlayerRevealID:TPlayerID=play_none);
var KMHouse:TKMHouse;
begin
  aLoc.X:=aLoc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX;
  KMHouse:=fHouses.AddPlan(aHouseType, aLoc.X, aLoc.Y, PlayerID);
  fTerrain.SetHouse(aLoc, aHouseType, hs_Plan, PlayerID);
  BuildList.AddNewHousePlan(KMHouse);
end;


procedure TKMPlayerAssets.AutoRoadConnect(LocA,LocB:TKMPoint);
var fPath:TPathFinding; i:integer; NodeList:TKMPointList;
begin
  fPath := TPathFinding.Create(LocA, LocB, KMPoint(0,0), canMakeRoads, true);
  NodeList:=TKMPointList.Create;
  fPath.ReturnRoute(NodeList);
  fPath.Free;

  for i:=1 to NodeList.Count do
    AddRoad(NodeList.List[i]);

  FreeAndNil(NodeList);
end;


function TKMPlayerAssets.RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
var fHouse:TKMHouse;
begin
  Result := BuildList.CancelHousePlan(Position,Simulated);
  fHouse := fHouses.HitTest(Position.X, Position.Y);
  if fHouse<>nil then
  begin
    if not Simulated then
      fHouse.DemolishHouse(DoSilent,IsEditor);
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
    fSoundLib.Play(sfx_click);
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
     if (H<>nil)and(H.HasFood)and(H.HasSpace)and(fTerrain.Route_CanBeMade(Loc,KMPointY1(H.GetEntrance),aUnit.GetDesiredPassability(true),true)) then
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


procedure TKMPlayerAssets.CreatedHouse(aType:THouseType; aWasBuilt:boolean);
begin
  fMissionSettings.CreatedHouse(aType,aWasBuilt);
end;

procedure TKMPlayerAssets.CreatedUnit(aType:TUnitType; aWasTrained:boolean);
begin
  fMissionSettings.CreatedUnit(aType,aWasTrained);
end;

procedure TKMPlayerAssets.DestroyedHouse(aType:THouseType);
begin
  fMissionSettings.DestroyedHouse(aType);
end;

procedure TKMPlayerAssets.DestroyedUnit(aType:TUnitType);
begin
  if Assigned(fMissionSettings) then fMissionSettings.DestroyedUnit(aType);
end;

procedure TKMPlayerAssets.UpdateReqDone(aType:THouseType);
begin
  fMissionSettings.UpdateReqDone(aType);
end;

function TKMPlayerAssets.GetCanBuild(aType:THouseType):boolean;
begin
  Result:=fMissionSettings.GetCanBuild(aType);
end;

function TKMPlayerAssets.GetHouseQty(aType:THouseType):integer;
begin
  Result:=fMissionSettings.GetHouseQty(aType);
end;


function TKMPlayerAssets.GetTotalHouseQty():integer;
begin
  Result:=fMissionSettings.GetTotalHouseQty;
end;


function TKMPlayerAssets.GetUnitQty(aType:TUnitType):integer;
begin
  Result := fMissionSettings.GetUnitQty(aType);
end;


function TKMPlayerAssets.GetHouseCount():integer;
begin
  Result := fHouses.Count;
end;


function TKMPlayerAssets.GetUnitCount():integer;
begin
  Result := fUnits.Count;
end;


procedure TKMPlayerAssets.Save(SaveStream:TKMemoryStream);
begin
  fUnits.Save(SaveStream);
  fHouses.Save(SaveStream);
  fDeliverList.Save(SaveStream);
  fBuildList.Save(SaveStream);
  fMissionSettings.Save(SaveStream);
  SaveStream.Write(PlayerID, SizeOf(PlayerID));
  SaveStream.Write(PlayerType, SizeOf(PlayerType));
  SaveStream.Write(fAlliances, SizeOf(fAlliances));
  SaveStream.Write(SkipWinConditionCheck);
end;


procedure TKMPlayerAssets.Load(LoadStream:TKMemoryStream);
begin
  fUnits.Load(LoadStream);
  fHouses.Load(LoadStream);
  fDeliverList.Load(LoadStream);
  fBuildList.Load(LoadStream);
  fMissionSettings.Load(LoadStream);
  LoadStream.Read(PlayerID, SizeOf(PlayerID));
  LoadStream.Read(PlayerType, SizeOf(PlayerType));
  LoadStream.Read(fAlliances, SizeOf(fAlliances));
  LoadStream.Read(SkipWinConditionCheck);
end;


procedure TKMPlayerAssets.SyncLoad();
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
  LoadStream.Read(s); if s <> 'Animals' then exit;
  fUnits.Load(LoadStream);
end;


procedure TKMPlayerAnimals.SyncLoad();
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
  inherited;
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
    if (fUnits.List[i] <> nil) and (TKMUnit(fUnits.List[i]).GetUnitType = ut_Fish) then
    begin
      if fTerrain.Land[TKMUnit(fUnits.List[i]).GetPosition.Y,TKMUnit(fUnits.List[i]).GetPosition.X].WalkConnect[3] = aWaterID then
        if TKMUnitAnimal(fUnits.List[i]).fFishCount > HighestGroupCount then
        begin
          Result := TKMUnitAnimal(fUnits.List[i]);
          if not FindHighestCount then exit; //This is for time saving when we don't actually care which group is returned
          HighestGroupCount := TKMUnitAnimal(fUnits.List[i]).fFishCount;
        end;
    end;
  end;
end;


procedure TKMPlayerAnimals.GetFishLocations(out Loc:TKMPointList);
begin
  fUnits.GetLocations(Loc, ut_Fish);
end;


function TKMPlayerAnimals.GetUnitCount: integer;
begin
  Result := fUnits.GetUnitCount;
end;


function TKMPlayerAnimals.GetUnitByIndex(aIndex:integer): TKMUnit;
begin
  Result := fUnits.GetUnitByIndex(aIndex);
end;


end.
