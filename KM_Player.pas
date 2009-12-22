unit KM_Player;
interface
uses Classes, KromUtils, SysUtils, KM_Defaults, KM_Units, KM_Houses, KM_DeliverQueue, KM_Settings, KM_CommonTypes, KM_Utils;


type
  TPlayerType = (pt_Human, pt_Computer, pt_Animals);


type
  TKMPlayerAssets = class
  private
    fUnits: TKMUnitsCollection;
    fHouses: TKMHousesCollection;
    fDeliverList: TKMDeliverQueue;
    fBuildList: TKMBuildingQueue;
  public
    constructor Create(aPlayerID:TPlayerID);
    destructor Destroy; override;
  public
    fMissionSettings: TMissionSettings; //Required to be public so it can be accessed from LoadDAT
    PlayerID:TPlayerID; //Which ID this player is
    PlayerType: TPlayerType; //Is it Human or AI or Animals
    function AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
    function TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
    function AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint; DoFlatten:boolean=true);
    procedure AddField(aLoc: TKMPoint; aFieldType:TFieldType);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none);
    function AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none):boolean;
    procedure AutoRoadConnect(LocA,LocB:TKMPoint);
    function RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false; IsEditor:boolean=false):boolean;
    //procedure RemUnit(Position: TKMPoint);
    function RemPlan(Position: TKMPoint; Simulated:boolean=false):boolean;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindInn(Loc:TKMPoint; UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; const Index:byte=1): TKMHouse;
    function UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
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
    function GetUnitQty(aType:TUnitType):integer;
    function GetHouseCount():integer;
    function GetUnitCount():integer;
    property GetHouses:TKMHousesCollection read fHouses;
    property GetUnits:TKMUnitsCollection read fUnits;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load;
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
    function GetFishInWaterBody(aWaterID:byte; FindHighestCount:boolean=true): TKMUnitAnimal;
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure UpdateState;
    procedure Paint;
  end;

implementation
uses KM_Terrain, KM_SoundFX, KM_PathFinding, KM_PlayersCollection;


{ TKMPlayerAssets }
constructor TKMPlayerAssets.Create(aPlayerID:TPlayerID);
begin
  PlayerID := aPlayerID;
  fMissionSettings := TMissionSettings.Create;
  fUnits := TKMUnitsCollection.Create;
  fHouses := TKMHousesCollection.Create;
  fDeliverList := TKMDeliverQueue.Create;
  fBuildList := TKMBuildingQueue.Create;
end;


destructor TKMPlayerAssets.Destroy;
begin
  FreeAndNil(fMissionSettings);
  FreeAndNil(fUnits);
  FreeAndNil(fHouses);
  FreeAndNil(fDeliverList);
  FreeAndNil(fBuildList);
  inherited;
end;


function TKMPlayerAssets.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true):TKMUnit;
begin
  //Animals must get redirected to animal player
  if aUnitType in [ut_Wolf..ut_Duck] then
  begin
    Result := fPlayers.PlayerAnimals.AddUnit(aUnitType,Position,AutoPlace);
    exit;
  end;

  Result := fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y, AutoPlace);
  if Result = nil then exit;
  CreatedUnit(aUnitType, false);
end;


function TKMPlayerAssets.TrainUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
begin
  Result := fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y, false);
  //Do not add unit to statistic just yet, wait till it's training complete
end;


function TKMPlayerAssets.AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
begin
  Result := fUnits.AddGroup(PlayerID, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount);
  //Add unit to statistic inside the function for some units may not fir on map
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
    fSoundLib.Play(sfx_placemarker,aLoc,false);
end;

function TKMPlayerAssets.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none):boolean;
var KMHouse:TKMHouse;
begin
  Result:=false;
  if not fTerrain.CanPlaceHouse(aLoc,aHouseType,PlayerRevealID) then
  begin
    if not DoSilent then
      fSoundLib.Play(sfx_CantPlace,aLoc,false,4.0);
    exit;
  end;
  aLoc.X:=aLoc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX;
  KMHouse:=fHouses.AddPlan(aHouseType, aLoc.X, aLoc.Y, PlayerID);
  fTerrain.SetHouse(aLoc, aHouseType, hs_Plan, PlayerID);
  BuildList.AddNewHousePlan(KMHouse);
  Result:=true;
  if not DoSilent then
    fSoundLib.Play(sfx_placemarker,aLoc,false);
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

{procedure TKMPlayerAssets.RemUnit(Position: TKMPoint);
begin
  fUnits.Rem(Position);
end;}

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
  Result:=fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;

function TKMPlayerAssets.FindInn(Loc:TKMPoint; UnitIsAtHome:boolean=false): TKMHouseInn;
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
         
   H:=TKMHouseInn(FindHouse(ht_Inn,KMPoint(0,0),1));
   repeat
     //First make sure that it is valid
     if (H<>nil)and(H.HasFood)and(H.HasSpace)and(fTerrain.Route_CanBeMade(Loc,KMPointY1(H.GetEntrance),canWalk,true)) then
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
     H:=TKMHouseInn(FindHouse(ht_Inn,KMPoint(0,0),i));
   until(H = nil);
end;


function TKMPlayerAssets.UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
begin
  Result:= fUnits.HitTest(X, Y, UT);
end;

procedure TKMPlayerAssets.GetUnitLocations(out Loc:TKMPointList);
begin
  fUnits.GetLocations(PlayerID,Loc);
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

function TKMPlayerAssets.GetUnitQty(aType:TUnitType):integer;
begin
  Result:=fMissionSettings.GetUnitQty(aType);
end;


function TKMPlayerAssets.GetHouseCount():integer;
begin
  Result:=fHouses.Count;
end;


function TKMPlayerAssets.GetUnitCount():integer;
begin
  Result:=fUnits.Count;
end;


procedure TKMPlayerAssets.Save(SaveStream:TKMemoryStream);
begin
    fUnits.Save(SaveStream);
    fHouses.Save(SaveStream);
    fDeliverList.Save(SaveStream);
    fBuildList.Save(SaveStream);
    fMissionSettings.Save(SaveStream);
    SaveStream.Write(PlayerID,4);
    SaveStream.Write(PlayerType,4);
end;


procedure TKMPlayerAssets.Load;
begin
  //todo: load
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
  SaveStream.Write('Animals',7);
  fUnits.Save(SaveStream);
end;


procedure TKMPlayerAnimals.Load(LoadStream:TKMemoryStream);
var c:array[1..64]of char;
begin
  LoadStream.Read(c,7); //if s <> 'Animals' then exit;
  fUnits.Load(LoadStream);
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
  fUnits := TKMUnitsCollection.Create;
end;


destructor TKMPlayerAnimals.Destroy;
begin
  FreeAndNil(fUnits);
  inherited;
end;


function TKMPlayerAnimals.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true): TKMUnit;
begin
  Result := fUnits.Add(play_animals, aUnitType, Position.X, Position.Y, AutoPlace);
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


end.
