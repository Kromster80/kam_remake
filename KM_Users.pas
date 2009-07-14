unit KM_Users;
interface
uses
  Windows, Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_DeliverQueue, KM_Defaults, KM_Settings;

type
  TPlayerType = (pt_Human, pt_Computer, pt_Animals);
  TMissionMode = (mm_Normal, mm_Tactic);


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
    function AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
    function AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
    procedure AddRoad(aLoc: TKMPoint);
    procedure AddField(aLoc: TKMPoint; aFieldType:TFieldType);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none);
    function AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none):boolean;
    function RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false):boolean;
    procedure RemUnit(Position: TKMUnit);
    function RemPlan(Position: TKMPoint; Simulated:boolean=false):boolean;
    function FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
    function FindInn(Loc:TKMPoint;UnitIsAtHome:boolean=false): TKMHouseInn;
    function FindHouse(aType:THouseType; aPosition: TKMPoint; const Index:byte=1): TKMHouse;
    function UnitsHitTest(X, Y: Integer; const UT:TUnitType = ut_Any): TKMUnit;
    procedure GetUnitLocations(out Loc:TKMPointList);
    function HousesHitTest(X, Y: Integer): TKMHouse;
    property DeliverList:TKMDeliverQueue read fDeliverList;
    property BuildList:TKMBuildingQueue read fBuildList;

    procedure CreatedHouse(aType:THouseType);
    procedure CreatedUnit(aType:TUnitType);
    procedure DestroyedHouse(aType:THouseType);
    procedure DestroyedUnit(aType:TUnitType);
    procedure UpdateReqDone(aType:THouseType);

    function GetCanBuild(aType:THouseType):boolean;
    function GetHouseQty(aType:THouseType):integer;
    function GetUnitQty(aType:TUnitType):integer;
  public
    procedure UpdateState;
    procedure Paint;
  end;


type
  TKMPlayerAI = class
  private
    Assets:TKMPlayerAssets;
  public
    constructor Create(aAssets:TKMPlayerAssets);
    procedure CheckDefeatConditions();
    procedure CheckCitizenCount();
    procedure UpdateState;
  end;


type
  TKMAllPlayers = class
  private
    fPlayerCount:integer;
  public
    Player:array[1..MAX_PLAYERS] of TKMPlayerAssets;
    PlayerAI:array[1..MAX_PLAYERS] of TKMPlayerAI;
    SelectedHouse: TKMHouse;
    SelectedUnit: TKMUnit;
  public
    constructor Create(PlayerCount:integer);
    destructor Destroy; override;
  public
    property PlayerCount:integer read fPlayerCount;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function GetUnitCount():integer;
  public
    procedure UpdateState(Tick:cardinal);
    procedure Paint;
  end;



var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player
  MissionMode: TMissionMode;

  
implementation
uses
  KM_Terrain, KM_LoadSFX, KM_Game;


constructor TKMPlayerAI.Create(aAssets:TKMPlayerAssets);
begin
  Inherited Create;
  Assets:=aAssets;
end;


procedure TKMPlayerAI.CheckDefeatConditions();
begin
  if (Assets.fMissionSettings.GetHouseQty(ht_Store)=0)
  and(Assets.fMissionSettings.GetHouseQty(ht_School)=0)
  and(Assets.fMissionSettings.GetHouseQty(ht_Barracks)=0)
  //and(ArmyCount=0)
  then
    fGame.StopGame(gr_Defeat);

end;


procedure TKMPlayerAI.CheckCitizenCount();
var i:integer; UnitType:TUnitType; H:TKMHouse; HC:TKMHousesCollection;
begin
  H := Assets.FindHouse(ht_School,KMPoint(0,0),1);
  if H <> nil then
    if TKMHouseSchool(H).UnitQueue[1]<>ut_None then exit;

  HC:=Assets.fHouses;
  for i:=0 to HC.Count-1 do
  if TKMHouse(HC.Items[i]).IsComplete then
  if not TKMHouse(HC.Items[i]).GetHasOwner then
  if TKMHouse(HC.Items[i]).GetHouseType <> ht_Barracks then begin
    UnitType := TUnitType(HouseDAT[byte(TKMHouse(HC.Items[i]).GetHouseType)].OwnerType+1);
    if UnitType <> ut_None then break; //Don't need more UnitTypes yet
  end;

  if UnitType <> ut_None then begin
    H := Assets.FindHouse(ht_School,KMPoint(0,0),1);
    if H <> nil then TKMHouseSchool(H).AddUnitToQueue(UnitType);
  end;
end;


procedure TKMPlayerAI.UpdateState();
begin
  //Check defeat only for MyPlayer
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
    CheckDefeatConditions //Store+Barracks+School+Armies = 0
  else
  
  if Assets.PlayerType=pt_Computer then begin
  CheckCitizenCount; //Train new citizens if needed
  //CheckHouseCount; //Build new houses if needed
  //CheckArmiesCount; //Train new soldiers if needed
  //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
  //CheckAndIssueAttack; //Attack enemy
  //Anything Else?
  end;
end;


{ TKMPlayerAssets }
function TKMPlayerAssets.AddUnit(aUnitType: TUnitType; Position: TKMPoint; AutoPlace:boolean=true):TKMUnit;
begin
  Result:=fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y, AutoPlace);
end;


function TKMPlayerAssets.AddGroup(aUnitType:TUnitType; Position: TKMPoint; aDir:TKMDirection; aUnitPerRow, aUnitCount:word):TKMUnit;
begin
  Result:=fUnits.AddGroup(PlayerID, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aUnitCount);
end;


function TKMPlayerAssets.AddHouse(aHouseType: THouseType; Position: TKMPoint):TKMHouse;
var xo,i,k:integer;
begin
  //First flatten the terrain at the location of the house
  for i:=4 downto 1 do for k:=4 downto 1 do
    if HousePlanYX[byte(aHouseType),i,k]<>0 then
      fTerrain.FlattenTerrain(KMPoint(Position.X+k-3,Position.Y+i-4));

  xo:=HouseDAT[byte(aHouseType)].EntranceOffsetX;
  Result:=fHouses.AddHouse(aHouseType, Position.X-xo, Position.Y, PlayerID);
end;


procedure TKMPlayerAssets.AddRoad(aLoc: TKMPoint);
begin
  //if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  //The AddPlan function should do the check, but if we enforce it here then it will create lots of problems
  //with the original missions. (I've also seem some fan missions where they have road over wrong tiles)

  fTerrain.SetRoad(aLoc,PlayerID);
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
    else fLog.AssertToLog(false,'Wrong markup');
  end;
  if not DoSilent then
    fSoundLib.Play(sfx_placemarker,aLoc,false);
end;

function TKMPlayerAssets.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean; PlayerRevealID:TPlayerID=play_none):boolean;
var KMHouse:TKMHouse;
begin
  Result:=false;
  aLoc.X:=aLoc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX;
  if not fTerrain.CanPlaceHouse(aLoc,aHouseType,PlayerRevealID) then
  begin
    if not DoSilent then
      fSoundLib.Play(sfx_CantPlace,aLoc,false,4.0);
    exit;
  end;
  KMHouse:=fHouses.AddPlan(aHouseType, aLoc.X, aLoc.Y, PlayerID);
  fTerrain.SetHouse(aLoc, aHouseType, hs_Plan, PlayerID);
  BuildList.AddNewHousePlan(KMHouse);
  Result:=true;
  if not DoSilent then
    fSoundLib.Play(sfx_placemarker,aLoc,false);
end;

function TKMPlayerAssets.RemHouse(Position: TKMPoint; DoSilent:boolean; Simulated:boolean=false):boolean;
var fHouse:TKMHouse;
begin
  Result := BuildList.CancelHousePlan(Position,Simulated);
  fHouse := fHouses.HitTest(Position.X, Position.Y);
  if fHouse<>nil then
  begin
    if not Simulated then
      fHouse.DemolishHouse(DoSilent);
    Result := true;
  end;
end;

procedure TKMPlayerAssets.RemUnit(Position: TKMUnit);
begin
  fUnits.Rem(Position);
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
  Result:=fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;

function TKMPlayerAssets.FindInn(Loc:TKMPoint;UnitIsAtHome:boolean=false): TKMHouseInn;
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
     if (H<>nil)and(H.HasFood)and(H.HasSpace)and(fTerrain.Route_CanBeMade(Loc,KMPointY1(H.GetEntrance(Self)),canWalkRoad,true)) then
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
   until H = nil;
end;

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


procedure TKMPlayerAssets.Paint;
begin
  fUnits.Paint;
  fHouses.Paint;
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


procedure TKMPlayerAssets.CreatedHouse(aType:THouseType);
begin
  fMissionSettings.CreatedHouse(aType);
end;

procedure TKMPlayerAssets.CreatedUnit(aType:TUnitType);
begin
  fMissionSettings.CreatedUnit(aType);
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


procedure TKMPlayerAssets.UpdateState;
begin
  fUnits.UpdateState;
  fHouses.UpdateState;
end;


{TKMAllPlayers}
constructor TKMAllPlayers.Create(PlayerCount:integer);
var i:integer;
begin
  fLog.AssertToLog(InRange(PlayerCount,1,MAX_PLAYERS),'PlayerCount exceeded');

  fPlayerCount:=PlayerCount; //Used internally
  for i:=1 to fPlayerCount do begin
    Player[i]:=TKMPlayerAssets.Create(TPlayerID(i));
    PlayerAI[i]:=TKMPlayerAI.Create(Player[i]);
  end;
end;

destructor TKMAllPlayers.Destroy;
var i:integer;
begin
  for i:=1 to fPlayerCount do begin
    FreeAndNil(Player[i]);
    FreeAndNil(PlayerAI[i]);
  end;

  MyPlayer:=nil;
  SelectedHouse:=nil;
  inherited;
end;


function TKMAllPlayers.HousesHitTest(X, Y: Integer): TKMHouse;
var i:integer;
begin
  Result:=nil;
  for i:=1 to fPlayerCount do begin
    Result:= Player[i].HousesHitTest(X,Y);
    if Result<>nil then Break; //else keep on testing
  end;
end;


function TKMAllPlayers.UnitsHitTest(X, Y: Integer): TKMUnit;
var i:integer;
begin
  Result:=nil;
  for i:=1 to fPlayerCount do begin
    Result:= Player[i].UnitsHitTest(X,Y);
    if Result<>nil then Break; //else keep on testing
  end;
end;


//Get total unit count
function TKMAllPlayers.GetUnitCount():integer;
var i:integer;
begin
  Result:=0;
  for i:=1 to fPlayerCount do
    inc(Result,Player[i].fUnits.Count);
end;


procedure TKMAllPlayers.UpdateState(Tick:cardinal);
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].UpdateState;

  //This is not ajoined with previous loop since it can result in StopGame which flushes all data
  for i:=1 to fPlayerCount do
    if (Tick+i) mod 20 = 0 then //Do only one player per Tick
      PlayerAI[i].UpdateState;
end;


procedure TKMAllPlayers.Paint;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].Paint;
end;




end.
