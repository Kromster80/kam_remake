unit KM_PlayerStats;
{$I KaM_Remake.inc}
interface
uses Classes,
  KM_CommonClasses, KM_Defaults;


{These are mission specific settings and stats for each player}
type
  TKMPlayerStats = class
  private
    fBuildReqDone: array [THouseType] of Boolean; //If building requirements performed or assigned from script
    Houses: array [THouseType] of packed record
      Planned,          //Houseplans were placed
      PlanRemoved,      //Houseplans were removed
      Started,          //Construction started
      Ended,            //Construction ended (either done or destroyed/cancelled)
      Initial,          //created by script on mission start
      Built,            //constructed by player
      SelfDestruct,     //deconstructed by player
      Lost,             //lost from attacks and self-demolished
      Destroyed: Word;  //damage to other players
    end;
    Units: array [HUMANS_MIN..HUMANS_MAX] of packed record
      Initial,          //Provided at mission start
      Trained,          //Trained by player
      Lost,             //Died of hunger or killed
      Killed: Word;     //Killed (incl. self)
    end;
    Goods: array [WARE_MIN..WARE_MAX] of packed record
      Produced: Word;
    end;
    fResourceRatios: array [1..4, 1..4]of Byte;
    function GetHouseReleased(aType: THouseType): Boolean;
    procedure SetHouseReleased(aType: THouseType; aValue: Boolean);
    function GetRatio(aRes: TResourceType; aHouse: THouseType): Byte;
    procedure SetRatio(aRes: TResourceType; aHouse: THouseType; aValue: Byte);
    procedure UpdateReqDone(aType: THouseType);
  public
    AllowToBuild: array [THouseType] of Boolean; //Allowance derived from mission script
    AllowToTrade: array [WARE_MIN..WARE_MAX] of Boolean; //Allowance derived from mission script
    constructor Create;

    //Input reported by Player
    procedure GoodProduced(aRes: TResourceType; aCount: Integer);
    procedure HousePlanned(aType: THouseType);
    procedure HousePlanRemoved(aType: THouseType);
    procedure HouseStarted(aType: THouseType);
    procedure HouseEnded(aType: THouseType);
    procedure HouseCreated(aType: THouseType; aWasBuilt:boolean);
    procedure HouseLost(aType: THouseType);
    procedure HouseSelfDestruct(aType: THouseType);
    procedure HouseDestroyed(aType: THouseType);
    procedure UnitCreated(aType: TUnitType; aWasTrained:boolean);
    procedure UnitLost(aType: TUnitType);
    procedure UnitKilled(aType: TUnitType);

    property HouseReleased[aType: THouseType]: boolean read GetHouseReleased write SetHouseReleased;
    property Ratio[aRes: TResourceType; aHouse: THouseType]: Byte read GetRatio write SetRatio;

    //Output
    function GetHouseQty(aType: THouseType): Integer;
    function GetHouseWip(aType: THouseType): Integer;
    function GetUnitQty(aType: TUnitType): Integer;
    function GetArmyCount:Integer;
    function GetCanBuild(aType: THouseType):boolean;

    function GetCitizensTrained:cardinal;
    function GetCitizensLost:cardinal;
    function GetCitizensKilled:cardinal;
    function GetHousesBuilt:cardinal;
    function GetHousesLost:cardinal;
    function GetHousesDestroyed:cardinal;
    function GetWarriorsTrained:cardinal;
    function GetWarriorsKilled:cardinal;
    function GetWarriorsLost:cardinal;
    function GetGoodsProduced:cardinal;
    function GetWeaponsProduced:cardinal;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Resource;


const
  //For now it is the same as KaM
  //The number means how many items should be in houses input max
  DistributionDefaults: array[1..4,1..4]of byte = (
    (5,4,0,0),
    (5,3,4,5),
    (5,3,0,0),
    (5,3,2,0)
    );


{ TKMPlayerStats }
constructor TKMPlayerStats.Create;
var H: THouseType; W: TResourceType; i,k:integer;
begin
  inherited;
  for H:=Low(THouseType) to High(THouseType) do
    AllowToBuild[H] := True;

  for W:=WARE_MIN to WARE_MAX do
    AllowToTrade[W] := True;

  //Release Store at the start of the game by default
  HouseReleased[ht_Store] := True;

  for i:=1 to 4 do for k:=1 to 4 do
    fResourceRatios[i,k] := DistributionDefaults[i,k];
end;


procedure TKMPlayerStats.UpdateReqDone(aType: THouseType);
var i: THouseType;
begin
  for i:=Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[i].ReleasedBy = aType then
      HouseReleased[i] := true;
end;


procedure TKMPlayerStats.HousePlanned(aType: THouseType);
begin
  inc(Houses[aType].Planned);
end;


procedure TKMPlayerStats.HousePlanRemoved(aType: THouseType);
begin
  inc(Houses[aType].PlanRemoved);
end;


//New house in progress
procedure TKMPlayerStats.HouseStarted(aType: THouseType);
begin
  inc(Houses[aType].Started);
end;


//House building process was ended. We don't really know if it was canceled or destroyed or finished
//Other House** methods will handle that
procedure TKMPlayerStats.HouseEnded(aType: THouseType);
begin
  inc(Houses[aType].Ended);
end;


//New house, either built by player or created by mission script
procedure TKMPlayerStats.HouseCreated(aType: THouseType; aWasBuilt:boolean);
begin
  if aWasBuilt then
    inc(Houses[aType].Built)
  else
    inc(Houses[aType].Initial);
  UpdateReqDone(aType);
end;


//Destroyed by enemy
procedure TKMPlayerStats.HouseLost(aType: THouseType);
begin
  inc(Houses[aType].Lost);
end;


procedure TKMPlayerStats.HouseSelfDestruct(aType: THouseType);
begin
  inc(Houses[aType].SelfDestruct);
end;


//Player has destroyed an enemy house
procedure TKMPlayerStats.HouseDestroyed(aType: THouseType);
begin
  inc(Houses[aType].Destroyed);
end;


procedure TKMPlayerStats.UnitCreated(aType: TUnitType; aWasTrained:boolean);
begin
  if aWasTrained then
    inc(Units[aType].Trained)
  else
    inc(Units[aType].Initial);
end;


procedure TKMPlayerStats.UnitLost(aType: TUnitType);
begin
  inc(Units[aType].Lost);
end;


procedure TKMPlayerStats.UnitKilled(aType: TUnitType);
begin
  inc(Units[aType].Killed);
end;


procedure TKMPlayerStats.GoodProduced(aRes: TResourceType; aCount:integer);
begin
  if aRes<>rt_None then
    inc(Goods[aRes].Produced, aCount);
end;


//How many houses are there
function TKMPlayerStats.GetHouseQty(aType: THouseType): Integer;
var H: THouseType;
begin
  Result := 0;
  case aType of
    ht_None:    ;
    ht_Any:     for H := Low(THouseType) to High(THouseType) do
                if fResource.HouseDat[H].IsValid then
                  Inc(Result, Houses[H].Initial + Houses[H].Built - Houses[H].SelfDestruct - Houses[H].Lost);
    else        Result := Houses[aType].Initial + Houses[aType].Built - Houses[aType].SelfDestruct - Houses[aType].Lost;
  end;
end;

//How many houses are planned and in progress
function TKMPlayerStats.GetHouseWip(aType: THouseType): Integer;
var H: THouseType;
begin
  Result := 0;
  case aType of
    ht_None:    ;
    ht_Any:     for H := Low(THouseType) to High(THouseType) do
                if fResource.HouseDat[H].IsValid then
                  Inc(Result, Houses[H].Started + Houses[H].Planned - Houses[H].Ended - Houses[H].PlanRemoved);
    else        Result := Houses[aType].Started + Houses[aType].Planned - Houses[aType].Ended - Houses[aType].PlanRemoved;
  end;
end;


function TKMPlayerStats.GetUnitQty(aType: TUnitType): Integer;
var UT: TUnitType;
begin
  Result := 0;
  case aType of
    ut_None:    ;
    ut_Any:     for UT := HUMANS_MIN to HUMANS_MAX do
                  Result := Units[UT].Initial + Units[UT].Trained - Units[UT].Lost;
    else        begin
                  Result := Units[aType].Initial + Units[aType].Trained - Units[aType].Lost;
                  if aType = ut_Recruit then
                    for UT := WARRIOR_EQUIPABLE_MIN to WARRIOR_EQUIPABLE_MAX do
                      dec(Result, Units[UT].Trained); //Trained soldiers use a recruit
                end;
  end;
end;


function TKMPlayerStats.GetArmyCount: Integer;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Result := Result + GetUnitQty(UT);
end;


//Houses might be blocked by mission script
function TKMPlayerStats.GetCanBuild(aType: THouseType): Boolean;
begin
  Result := HouseReleased[aType] AND AllowToBuild[aType];
end;


function TKMPlayerStats.GetRatio(aRes: TResourceType; aHouse: THouseType): Byte;
begin
  Result := 5; //Default should be 5, for house/resource combinations that don't have a setting (on a side note this should be the only place the resourse limit is defined)
  case aRes of
    rt_Steel: if aHouse = ht_WeaponSmithy   then Result := fResourceRatios[1,1] else
              if aHouse = ht_ArmorSmithy    then Result := fResourceRatios[1,2];
    rt_Coal:  if aHouse = ht_IronSmithy     then Result := fResourceRatios[2,1] else
              if aHouse = ht_Metallurgists  then Result := fResourceRatios[2,2] else
              if aHouse = ht_WeaponSmithy   then Result := fResourceRatios[2,3] else
              if aHouse = ht_ArmorSmithy    then Result := fResourceRatios[2,4];
    rt_Wood:  if aHouse = ht_ArmorWorkshop  then Result := fResourceRatios[3,1] else
              if aHouse = ht_WeaponWorkshop then Result := fResourceRatios[3,2];
    rt_Corn:  if aHouse = ht_Mill           then Result := fResourceRatios[4,1] else
              if aHouse = ht_Swine          then Result := fResourceRatios[4,2] else
              if aHouse = ht_Stables        then Result := fResourceRatios[4,3];
  end;
end;


procedure TKMPlayerStats.SetRatio(aRes: TResourceType; aHouse: THouseType; aValue: Byte);
begin
  case aRes of
    rt_Steel: if aHouse = ht_WeaponSmithy   then fResourceRatios[1,1] := aValue else
              if aHouse = ht_ArmorSmithy    then fResourceRatios[1,2] := aValue;
    rt_Coal:  if aHouse = ht_IronSmithy     then fResourceRatios[2,1] := aValue else
              if aHouse = ht_Metallurgists  then fResourceRatios[2,2] := aValue else
              if aHouse = ht_WeaponSmithy   then fResourceRatios[2,3] := aValue else
              if aHouse = ht_ArmorSmithy    then fResourceRatios[2,4] := aValue;
    rt_Wood:  if aHouse = ht_ArmorWorkshop  then fResourceRatios[3,1] := aValue else
              if aHouse = ht_WeaponWorkshop then fResourceRatios[3,2] := aValue;
    rt_Corn:  if aHouse = ht_Mill           then fResourceRatios[4,1] := aValue else
              if aHouse = ht_Swine          then fResourceRatios[4,2] := aValue else
              if aHouse = ht_Stables        then fResourceRatios[4,3] := aValue;
    else Assert(False, 'Unexpected resource at SetRatio');
  end;
end;


//The value includes only citizens, Warriors are counted separately
function TKMPlayerStats.GetCitizensTrained:cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Trained);
end;


function TKMPlayerStats.GetCitizensLost:cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Lost);
end;


function TKMPlayerStats.GetCitizensKilled:cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Killed);
end;


function TKMPlayerStats.GetHousesBuilt:cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := Low(THouseType) to High(THouseType) do
    inc(Result, Houses[HT].Built);
end;



function TKMPlayerStats.GetHousesLost:cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := Low(THouseType) to High(THouseType) do
    inc(Result, Houses[HT].Lost);
end;


function TKMPlayerStats.GetHousesDestroyed: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := Low(THouseType) to High(THouseType) do
  if fResource.HouseDat[HT].IsValid then
    Inc(Result, Houses[HT].Destroyed);
end;


//The value includes all Warriors
function TKMPlayerStats.GetWarriorsTrained: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Trained);
end;


function TKMPlayerStats.GetWarriorsLost: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Lost);
end;


function TKMPlayerStats.GetWarriorsKilled: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Killed);
end;


//Everything except weapons
function TKMPlayerStats.GetGoodsProduced: Cardinal;
var RT: TResourceType;
begin
  Result := 0;
  for RT := WARE_MIN to WARE_MAX do
  if fResource.Resources[RT].IsGood then
    Inc(Result, Goods[RT].Produced);
end;


//KaM includes all weapons and armor, but not horses
function TKMPlayerStats.GetWeaponsProduced: Cardinal;
var RT: TResourceType;
begin
  Result := 0;
  for RT := WARE_MIN to WARE_MAX do
  if fResource.Resources[RT].IsWeapon then
    Inc(Result, Goods[RT].Produced);
end;


procedure TKMPlayerStats.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('PlayerStats');
  SaveStream.Write(Houses, SizeOf(Houses));
  SaveStream.Write(Units, SizeOf(Units));
  SaveStream.Write(Goods, SizeOf(Goods));
  SaveStream.Write(fResourceRatios, SizeOf(fResourceRatios));
  SaveStream.Write(AllowToBuild, SizeOf(AllowToBuild));
  SaveStream.Write(AllowToTrade, SizeOf(AllowToTrade));
  SaveStream.Write(fBuildReqDone, SizeOf(fBuildReqDone));
end;


procedure TKMPlayerStats.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerStats');
  LoadStream.Read(Houses, SizeOf(Houses));
  LoadStream.Read(Units, SizeOf(Units));
  LoadStream.Read(Goods, SizeOf(Goods));
  LoadStream.Read(fResourceRatios, SizeOf(fResourceRatios));
  LoadStream.Read(AllowToBuild, SizeOf(AllowToBuild));
  LoadStream.Read(AllowToTrade, SizeOf(AllowToTrade));
  LoadStream.Read(fBuildReqDone, SizeOf(fBuildReqDone));
end;


function TKMPlayerStats.GetHouseReleased(aType: THouseType): boolean;
begin
  Result := fBuildReqDone[aType];
end;


procedure TKMPlayerStats.SetHouseReleased(aType: THouseType; aValue: boolean);
begin
  fBuildReqDone[aType] := aValue;
end;


end.
