unit KM_Users;
interface
uses
  Windows, Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_DeliverQueue, KM_Defaults, KM_Settings;

type
  TPlayerType = (pt_Human, pt_Computer, pt_Animals);

  TKMPlayerAssets = class
  private
    fMissionSettings: TMissionSettings;
    fUnits: TKMUnitsCollection;
    fHouses: TKMHousesCollection;
    fDeliverList: TKMDeliverQueue;
    fBuildList: TKMBuildingQueue;
  public
    constructor Create(aPlayerID:TPlayerID);
    destructor Destroy; override;
  public
    PlayerID:TPlayerID; //Which ID this player is
    PlayerType: TPlayerType; //Is it Human or AI or Animals
    function AddUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
    procedure AddHouse(aHouseType: THouseType; Position: TKMPoint);
    procedure AddRoad(aLoc: TKMPoint; aMarkup:TMarkup);
    procedure AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean);
    function AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean):boolean;
    procedure RemHouse(Position: TKMPoint);
    procedure RemUnit(Position: TKMUnit);
    procedure RemPlan(Position: TKMPoint);
    function FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
    function FindHouse(aType:THouseType; X,Y:word): TKMHouse;
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
  public
    procedure UpdateState;
    procedure Paint;
  end;

var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player

implementation

uses
  KM_Terrain, KM_LoadSFX;


{ TKMPlayerAssets }
function TKMPlayerAssets.AddUnit(aUnitType: TUnitType; Position: TKMPoint): TKMUnit;
begin
  Result:=fUnits.Add(PlayerID, aUnitType, Position.X, Position.Y);
end;


procedure TKMPlayerAssets.AddHouse(aHouseType: THouseType; Position: TKMPoint);
var xo:integer;
begin
  xo:=HouseDAT[byte(aHouseType)].EntranceOffsetX;
  fHouses.AddHouse(aHouseType, Position.X-xo, Position.Y, PlayerID)
end;


procedure TKMPlayerAssets.AddRoad(aLoc: TKMPoint; aMarkup:TMarkup);
begin
  if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  case aMarkup of
    mu_RoadPlan: fTerrain.SetField(aLoc,PlayerID,fdt_Road);
    mu_FieldPlan: fTerrain.SetField(aLoc,PlayerID,fdt_Field);
    mu_WinePlan: fTerrain.SetField(aLoc,PlayerID,fdt_Wine);
    else Assert(false,'Wrong markup');
  end;
end;


{DoSilent means that there will be no sound when markup is placed, needed e.g. when script used}
procedure TKMPlayerAssets.AddRoadPlan(aLoc: TKMPoint; aMarkup:TMarkup; DoSilent:boolean);
begin
  if not fTerrain.CanPlaceRoad(aLoc,aMarkup) then exit;
  fTerrain.SetMarkup(aLoc, aMarkup);
  case aMarkup of
    mu_RoadPlan: BuildList.AddNewRoad(aLoc, fdt_Road);
    mu_FieldPlan: BuildList.AddNewRoad(aLoc, fdt_Field);
    mu_WinePlan: BuildList.AddNewRoad(aLoc, fdt_Wine);
    else Assert(false,'Wrong markup');
  end;
  if not DoSilent then
  fSoundLib.Play(sfx_placemarker,aLoc,false);
end;

function TKMPlayerAssets.AddHousePlan(aHouseType: THouseType; aLoc: TKMPoint; DoSilent:boolean):boolean;
var KMHouse:TKMHouse;
begin
  Result:=false;
  aLoc.X:=aLoc.X-HouseDAT[byte(aHouseType)].EntranceOffsetX;
  if not fTerrain.CanPlaceHouse(aLoc,aHouseType) then exit;
  KMHouse:=fHouses.AddPlan(aHouseType, aLoc.X, aLoc.Y, PlayerID);
  fTerrain.SetHousePlan(aLoc, aHouseType, fdt_HousePlan);
  fTerrain.SetTileOwnership(aLoc,aHouseType, PlayerID);
  BuildList.AddNewHousePlan(KMHouse);
  Result:=true;
  if not DoSilent then
  fSoundLib.Play(sfx_placemarker,aLoc,false);
end;

procedure TKMPlayerAssets.RemHouse(Position: TKMPoint);
begin
  fHouses.Rem(Position.X, Position.Y);
end;

procedure TKMPlayerAssets.RemUnit(Position: TKMUnit);
begin
  fUnits.Rem(Position);
end;

procedure TKMPlayerAssets.RemPlan(Position: TKMPoint);
begin
  if BuildList.RemRoad(Position) then
    fTerrain.RemMarkup(Position);
end;

function TKMPlayerAssets.FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
begin
  Result:=fHouses.FindEmptyHouse(aUnitType);
end;

function TKMPlayerAssets.FindHouse(aType:THouseType; X,Y:word): TKMHouse;
begin
  Result:=fHouses.FindHouse(aType, X, Y);
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
