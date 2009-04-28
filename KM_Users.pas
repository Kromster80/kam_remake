unit KM_Users;
interface
uses
  Windows, Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_DeliverQueue, KM_Defaults, KM_Settings;

type
  TPlayerType = (pt_Human, pt_Computer, pt_Animals);
  TMissionMode = (mm_Normal, mm_Tactic);

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
    function AddUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
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
    function FindHouse(aType:THouseType; X,Y:word; const Index:byte=1): TKMHouse;
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


  TKMAllPlayers = class
  private
    fPlayerCount:integer;
  public
    Player:array[1..MAX_PLAYERS] of TKMPlayerAssets;
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
    procedure UpdateState;
    procedure Paint;
  end;

var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player
  MissionMode: TMissionMode;

implementation

uses
  KM_Terrain, KM_LoadSFX;


{ TKMPlayerAssets }
function TKMPlayerAssets.AddUnit(aUnitType: TUnitType; Position: TKMPoint):TKMUnit;
begin
  Result:=fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y);
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
    else Assert(false,'Wrong markup');
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
  Result := false;
  fHouse:=fHouses.HitTest(Position.X, Position.Y);
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
  Result := BuildList.RemRoad(Position,Simulated);
  if (Result) and (not Simulated) then
  begin
    fSoundLib.Play(sfx_click,Position,false);
    fTerrain.RemMarkup(Position);
  end;
end;

function TKMPlayerAssets.FindEmptyHouse(aUnitType:TUnitType; Loc:TKMPoint): TKMHouse;
begin
  Result:=fHouses.FindEmptyHouse(aUnitType, Loc);
end;

function TKMPlayerAssets.FindHouse(aType:THouseType; X,Y:word; const Index:byte=1): TKMHouse;
begin
  Result:=fHouses.FindHouse(aType, X, Y, Index);
end;


constructor TKMPlayerAssets.Create(aPlayerID:TPlayerID);
begin
  PlayerID:=aPlayerID;
  fMissionSettings:= TMissionSettings.Create;
  fUnits:= TKMUnitsCollection.Create;
  fHouses:= TKMHousesCollection.Create;
  fDeliverList:= TKMDeliverQueue.Create;
  fBuildList:= TKMBuildingQueue.Create;
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
  Assert(InRange(PlayerCount,1,MAX_PLAYERS),'PlayerCount exceeded');

  fPlayerCount:=PlayerCount; //Used internally
  for i:=1 to fPlayerCount do
    Player[i]:=TKMPlayerAssets.Create(TPlayerID(i));
end;

destructor TKMAllPlayers.Destroy;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    FreeAndNil(Player[i]);

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


procedure TKMAllPlayers.UpdateState;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].UpdateState;
end;


procedure TKMAllPlayers.Paint;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].Paint;
end;




end.
