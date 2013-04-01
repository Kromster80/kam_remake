unit KM_PlayerStats;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults;


//These are stats for each player
type
  THouseStats = packed record
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

  TUnitStats = packed record
    Initial,          //Provided at mission start
    Trained,          //Trained by player
    Lost,             //Died of hunger or killed
    Killed: Word;     //Killed (incl. self)
  end;

  TWareStats = packed record
    Initial: Cardinal;
    Produced: Cardinal;
    Consumed: Cardinal;
  end;

  //Player statistics (+ ratios, house unlock, trade permissions)
  TKMPlayerStats = class
  private
    fChartCount: Integer;
    fChartCapacity: Integer;
    fChartHouses: TKMCardinalArray;
    fChartCitizens: TKMCardinalArray;
    fChartArmy: TKMCardinalArray;
    fChartWares: array [WARE_MIN..WARE_MAX] of TKMCardinalArray;

    fHouseUnlocked: array [THouseType] of Boolean; //If building requirements performed
    Houses: array [THouseType] of THouseStats;
    Units: array [HUMANS_MIN..HUMANS_MAX] of TUnitStats;
    Wares: array [WARE_MIN..WARE_MAX] of TWareStats;
    fResourceRatios: array [1..4, 1..4]of Byte;
    function GetChartWares(aWare: TWareType): TKMCardinalArray;
    function GetRatio(aRes: TWareType; aHouse: THouseType): Byte;
    procedure SetRatio(aRes: TWareType; aHouse: THouseType; aValue: Byte);
    procedure UpdateReqDone(aType: THouseType);
  public
    HouseBlocked: array [THouseType] of Boolean; //Allowance derived from mission script
    HouseGranted: array [THouseType] of Boolean; //Allowance derived from mission script

    AllowToTrade: array [WARE_MIN..WARE_MAX] of Boolean; //Allowance derived from mission script
    constructor Create;

    //Input reported by Player
    procedure WareInitial(aRes: TWareType; aCount: Cardinal);
    procedure WareProduced(aRes: TWareType; aCount: Cardinal);
    procedure WareConsumed(aRes: TWareType; aCount: Cardinal = 1);
    procedure HousePlanned(aType: THouseType);
    procedure HousePlanRemoved(aType: THouseType);
    procedure HouseStarted(aType: THouseType);
    procedure HouseEnded(aType: THouseType);
    procedure HouseCreated(aType: THouseType; aWasBuilt: Boolean);
    procedure HouseLost(aType: THouseType);
    procedure HouseSelfDestruct(aType: THouseType);
    procedure HouseDestroyed(aType: THouseType);
    procedure UnitCreated(aType: TUnitType; aWasTrained: Boolean);
    procedure UnitLost(aType: TUnitType);
    procedure UnitKilled(aType: TUnitType);

    property Ratio[aRes: TWareType; aHouse: THouseType]: Byte read GetRatio write SetRatio;

    //Output
    function GetHouseQty(aType: THouseType): Integer; overload;
    function GetHouseQty(aType: array of THouseType): Integer; overload;
    function GetHouseWip(aType: THouseType): Integer; overload;
    function GetHouseWip(aType: array of THouseType): Integer; overload;
    function GetHouseTotal(aType: THouseType): Integer;
    function GetUnitQty(aType: TUnitType): Integer;
    function GetUnitKilledQty(aType: TUnitType): Integer;
    function GetUnitLostQty(aType: TUnitType): Integer;
    function GetWareBalance(aRT: TWareType): Integer;
    function GetArmyCount: Integer;
    function GetCitizensCount: Integer;
    function GetCanBuild(aType: THouseType): Boolean;

    function GetCitizensTrained: Cardinal;
    function GetCitizensLost: Cardinal;
    function GetCitizensKilled: Cardinal;
    function GetHousesBuilt: Cardinal;
    function GetHousesLost: Cardinal;
    function GetHousesDestroyed: Cardinal;
    function GetWarriorsTrained: Cardinal;
    function GetWarriorsKilled: Cardinal;
    function GetWarriorsLost: Cardinal;
    function GetWaresProduced(aRT: TWareType): Cardinal;
    function GetCivilProduced: Cardinal;
    function GetWeaponsProduced: Cardinal;

    property ChartCount: Integer read fChartCount;
    property ChartHouses: TKMCardinalArray read fChartHouses;
    property ChartCitizens: TKMCardinalArray read fChartCitizens;
    property ChartArmy: TKMCardinalArray read fChartArmy;
    property ChartWares[aWare: TWareType]: TKMCardinalArray read GetChartWares;
    function ChartWaresEmpty(aWare: TWareType): Boolean;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


implementation
uses KM_Resource;


const
  //These have been adjusted slightly from the old KaM defaults.
  //The number means how many items should be in houses input max, and also affects delivery priority.
  DistributionDefaults: array[1..4,1..4]of byte = (
    (5,5,0,0),
    (5,3,4,4),
    (3,4,0,0),
    (4,5,3,0)
    );


{ TKMPlayerStats }
constructor TKMPlayerStats.Create;
var
  W: TWareType;
  I,K: Integer;
begin
  inherited;

  for W := WARE_MIN to WARE_MAX do
    AllowToTrade[W] := True;

  //Release Store at the start of the game by default
  fHouseUnlocked[ht_Store] := True;

  for I:=1 to 4 do for K:=1 to 4 do
    fResourceRatios[I,K] := DistributionDefaults[I,K];
end;


procedure TKMPlayerStats.UpdateReqDone(aType: THouseType);
var H: THouseType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    if fResource.HouseDat[H].ReleasedBy = aType then
      fHouseUnlocked[H] := True;
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


procedure TKMPlayerStats.WareInitial(aRes: TWareType; aCount: Cardinal);
begin
  if not DISPLAY_CHARTS_RESULT then Exit;
  if aRes <> wt_None then
    Inc(Wares[aRes].Initial, aCount);
end;


procedure TKMPlayerStats.WareProduced(aRes: TWareType; aCount: Cardinal);
var R: TWareType;
begin
  if aRes <> wt_None then
    case aRes of
      wt_All:     for R := WARE_MIN to WARE_MAX do
                    Inc(Wares[R].Produced, aCount);
      WARE_MIN..
      WARE_MAX:   Inc(Wares[aRes].Produced, aCount);
      else        Assert(False, 'Cant''t add produced ware ' + fResource.Wares[aRes].Title);
    end;
end;


procedure TKMPlayerStats.WareConsumed(aRes: TWareType; aCount: Cardinal = 1);
begin
  if not DISPLAY_CHARTS_RESULT then Exit;
  if aRes <> wt_None then
    Inc(Wares[aRes].Consumed, aCount);
end;


//How many complete houses are there
function TKMPlayerStats.GetHouseQty(aType: THouseType): Integer;
var H: THouseType;
begin
  Result := 0;
  case aType of
    ht_None:    ;
    ht_Any:     for H := HOUSE_MIN to HOUSE_MAX do
                  Inc(Result, Houses[H].Initial + Houses[H].Built - Houses[H].SelfDestruct - Houses[H].Lost);
    else        Result := Houses[aType].Initial + Houses[aType].Built - Houses[aType].SelfDestruct - Houses[aType].Lost;
  end;
end;


//How many complete houses there are
function TKMPlayerStats.GetHouseQty(aType: array of THouseType): Integer;
var
  I: Integer;
  H: THouseType;
begin
  Result := 0;
  if (Length(aType) = 0) then
    Assert(False, 'Quering wrong house type')
  else
  if (Length(aType) = 1) and (aType[0] = ht_Any) then
  begin
    for H := HOUSE_MIN to HOUSE_MAX do
      Inc(Result, Houses[H].Initial + Houses[H].Built - Houses[H].SelfDestruct - Houses[H].Lost);
  end
  else
  for I := Low(aType) to High(aType) do
    if fResource.HouseDat[aType[I]].IsValid then
      Inc(Result, Houses[aType[I]].Initial + Houses[aType[I]].Built - Houses[aType[I]].SelfDestruct - Houses[aType[I]].Lost)
    else
      Assert(False, 'Quering wrong house type');
end;


//How many houses are planned and in progress
function TKMPlayerStats.GetHouseWip(aType: THouseType): Integer;
var H: THouseType;
begin
  Result := 0;
  case aType of
    ht_None:    ;
    ht_Any:     for H := HOUSE_MIN to HOUSE_MAX do
                  Inc(Result, Houses[H].Started + Houses[H].Planned - Houses[H].Ended - Houses[H].PlanRemoved);
    else        Result := Houses[aType].Started + Houses[aType].Planned - Houses[aType].Ended - Houses[aType].PlanRemoved;
  end;
end;


//How many houses are planned in progress and ready
function TKMPlayerStats.GetHouseTotal(aType: THouseType): Integer;
begin
  Result := GetHouseQty(aType) + GetHouseWip(aType);
end;


//How many houses are planned and in progress
function TKMPlayerStats.GetHouseWip(aType: array of THouseType): Integer;
var
  I: Integer;
  H: THouseType;
begin
  Result := 0;
  if (Length(aType) = 0) then
    Assert(False, 'Quering wrong house type')
  else
  if (Length(aType) = 1) and (aType[0] = ht_Any) then
  begin
    for H := HOUSE_MIN to HOUSE_MAX do
      Inc(Result, Houses[H].Started + Houses[H].Planned - Houses[H].Ended - Houses[H].PlanRemoved);
  end
  else
  for I := Low(aType) to High(aType) do
    if fResource.HouseDat[aType[I]].IsValid then
      Inc(Result, Houses[aType[I]].Started + Houses[aType[I]].Planned - Houses[aType[I]].Ended - Houses[aType[I]].PlanRemoved)
    else
      Assert(False, 'Quering wrong house type');
end;


function TKMPlayerStats.GetUnitQty(aType: TUnitType): Integer;
var UT: TUnitType;
begin
  Result := 0;
  case aType of
    ut_None:    ;
    ut_Any:     for UT := HUMANS_MIN to HUMANS_MAX do
                  Inc(Result, Units[UT].Initial + Units[UT].Trained - Units[UT].Lost);
    else        begin
                  Result := Units[aType].Initial + Units[aType].Trained - Units[aType].Lost;
                  if aType = ut_Recruit then
                    for UT := WARRIOR_EQUIPABLE_MIN to WARRIOR_EQUIPABLE_MAX do
                      dec(Result, Units[UT].Trained); //Trained soldiers use a recruit
                end;
  end;
end;


function TKMPlayerStats.GetUnitKilledQty(aType: TUnitType): Integer;
begin
  Result := Units[aType].Killed;
end;


function TKMPlayerStats.GetUnitLostQty(aType: TUnitType): Integer;
begin
  Result := Units[aType].Lost;
end;


function TKMPlayerStats.GetWareBalance(aRT: TWareType): Integer;
var RT: TWareType;
begin
  Result := 0;
  case aRT of
    wt_None:    ;
    wt_All:     for RT := WARE_MIN to WARE_MAX do
                  Inc(Result, Wares[RT].Initial + Wares[RT].Produced - Wares[RT].Consumed);
    wt_Warfare: for RT := WARFARE_MIN to WARFARE_MAX do
                  Inc(Result, Wares[RT].Initial + Wares[RT].Produced - Wares[RT].Consumed);
    else        Result := Wares[aRT].Initial + Wares[aRT].Produced - Wares[aRT].Consumed;
  end;
end;


function TKMPlayerStats.GetArmyCount: Integer;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, GetUnitQty(UT));
end;


function TKMPlayerStats.GetCitizensCount: Integer;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Inc(Result, GetUnitQty(UT));
end;


//Houses might be blocked by mission script
function TKMPlayerStats.GetCanBuild(aType: THouseType): Boolean;
begin
  Result := (fHouseUnlocked[aType] or HouseGranted[aType]) and not HouseBlocked[aType];
end;


function TKMPlayerStats.GetRatio(aRes: TWareType; aHouse: THouseType): Byte;
begin  
  Result := 5; //Default should be 5, for house/resource combinations that don't have a setting (on a side note this should be the only place the resourse limit is defined)
  case aRes of
    wt_Steel: if aHouse = ht_WeaponSmithy   then Result := fResourceRatios[1,1] else
              if aHouse = ht_ArmorSmithy    then Result := fResourceRatios[1,2];
    wt_Coal:  if aHouse = ht_IronSmithy     then Result := fResourceRatios[2,1] else
              if aHouse = ht_Metallurgists  then Result := fResourceRatios[2,2] else
              if aHouse = ht_WeaponSmithy   then Result := fResourceRatios[2,3] else
              if aHouse = ht_ArmorSmithy    then Result := fResourceRatios[2,4];
    wt_Wood:  if aHouse = ht_ArmorWorkshop  then Result := fResourceRatios[3,1] else
              if aHouse = ht_WeaponWorkshop then Result := fResourceRatios[3,2];
    wt_Corn:  if aHouse = ht_Mill           then Result := fResourceRatios[4,1] else
              if aHouse = ht_Swine          then Result := fResourceRatios[4,2] else
              if aHouse = ht_Stables        then Result := fResourceRatios[4,3];
    else      //Handled in 1st row to avoid repeating in if .. else lines
  end;
end;


procedure TKMPlayerStats.SetRatio(aRes: TWareType; aHouse: THouseType; aValue: Byte);
begin
  case aRes of
    wt_Steel: if aHouse = ht_WeaponSmithy   then fResourceRatios[1,1] := aValue else
              if aHouse = ht_ArmorSmithy    then fResourceRatios[1,2] := aValue;
    wt_Coal:  if aHouse = ht_IronSmithy     then fResourceRatios[2,1] := aValue else
              if aHouse = ht_Metallurgists  then fResourceRatios[2,2] := aValue else
              if aHouse = ht_WeaponSmithy   then fResourceRatios[2,3] := aValue else
              if aHouse = ht_ArmorSmithy    then fResourceRatios[2,4] := aValue;
    wt_Wood:  if aHouse = ht_ArmorWorkshop  then fResourceRatios[3,1] := aValue else
              if aHouse = ht_WeaponWorkshop then fResourceRatios[3,2] := aValue;
    wt_Corn:  if aHouse = ht_Mill           then fResourceRatios[4,1] := aValue else
              if aHouse = ht_Swine          then fResourceRatios[4,2] := aValue else
              if aHouse = ht_Stables        then fResourceRatios[4,3] := aValue;
    else      Assert(False, 'Unexpected resource at SetRatio');
  end;
end;


//The value includes only citizens, Warriors are counted separately
function TKMPlayerStats.GetCitizensTrained: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Trained);
end;


function TKMPlayerStats.GetCitizensLost: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Lost);
end;


function TKMPlayerStats.GetCitizensKilled: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    inc(Result, Units[UT].Killed);
end;


function TKMPlayerStats.GetHousesBuilt: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
    Inc(Result, Houses[HT].Built);
end;


function TKMPlayerStats.GetHousesLost: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
    Inc(Result, Houses[HT].Lost);
end;


function TKMPlayerStats.GetHousesDestroyed: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
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


function TKMPlayerStats.GetWaresProduced(aRT: TWareType): Cardinal;
var RT: TWareType;
begin
  Result := 0;
  case aRT of
    wt_None:    ;
    wt_All:     for RT := WARE_MIN to WARE_MAX do
                  Inc(Result, Wares[RT].Produced);
    wt_Warfare: for RT := WARFARE_MIN to WARFARE_MAX do
                  Inc(Result, Wares[RT].Produced);
    else        Result := Wares[aRT].Produced;
  end;
end;


//Everything except weapons
function TKMPlayerStats.GetCivilProduced: Cardinal;
var RT: TWareType;
begin
  Result := 0;
  for RT := WARE_MIN to WARE_MAX do
  if not (RT in [WEAPON_MIN..WEAPON_MAX]) then
    Inc(Result, Wares[RT].Produced);
end;


//KaM includes all weapons and armor, but not horses
function TKMPlayerStats.GetWeaponsProduced: Cardinal;
var RT: TWareType;
begin
  Result := 0;
  for RT := WEAPON_MIN to WEAPON_MAX do
    Inc(Result, Wares[RT].Produced);
end;


function TKMPlayerStats.GetChartWares(aWare: TWareType): TKMCardinalArray;
var
  RT: TWareType;
  I: Integer;
begin
  case aWare of
    WARE_MIN..WARE_MAX: Result := fChartWares[aWare];
    wt_All:             begin
                          //Create new array and fill it (otherwise we assign pointers and corrupt data)
                          SetLength(Result, fChartCount);
                          for I := 0 to fChartCount - 1 do
                            Result[I] := 0;
                          for RT := WARE_MIN to WARE_MAX do
                          for I := 0 to fChartCount - 1 do
                            Result[I] := Result[I] + fChartWares[RT][I];
                        end;
    wt_Warfare:         begin
                          //Create new array and fill it (otherwise we assign pointers and corrupt data)
                          SetLength(Result, fChartCount);
                          for I := 0 to fChartCount - 1 do
                            Result[I] := 0;
                          for RT := WARFARE_MIN to WARFARE_MAX do
                          for I := 0 to fChartCount - 1 do
                            Result[I] := Result[I] + fChartWares[RT][I];
                        end;
    wt_Food:            begin
                          //Create new array and fill it (otherwise we assign pointers and corrupt data)
                          SetLength(Result, fChartCount);
                          for I := 0 to fChartCount - 1 do
                            Result[I] := fChartWares[wt_Bread][I] + fChartWares[wt_Sausages][I] + fChartWares[wt_Wine][I] + fChartWares[wt_Fish][I];
                        end;
    else                begin
                          //Return empty array
                          SetLength(Result, fChartCount);
                          for I := 0 to fChartCount - 1 do
                            Result[I] := 0;
                        end;
  end;
end;


function TKMPlayerStats.ChartWaresEmpty(aWare: TWareType): Boolean;
var
  RT: TWareType;
begin
  case aWare of
    WARE_MIN..WARE_MAX: Result := ChartWares[aWare][fChartCount-1] = 0;
    wt_All:             begin
                          Result := True;
                          for RT := WARE_MIN to WARE_MAX do
                          if ChartWares[RT][fChartCount-1] <> 0 then
                            Result := False;
                        end;
    wt_Warfare:         begin
                          Result := True;
                          for RT := WARFARE_MIN to WARFARE_MAX do
                          if ChartWares[RT][fChartCount-1] <> 0 then
                            Result := False;
                        end;
    wt_Food:            Result := ChartWares[wt_Wine][fChartCount-1] +
                                  ChartWares[wt_Bread][fChartCount-1] +
                                  ChartWares[wt_Sausages][fChartCount-1] +
                                  ChartWares[wt_Fish][fChartCount-1] = 0;
    else                Result := True;
  end;
end;


procedure TKMPlayerStats.Save(SaveStream: TKMemoryStream);
var R: TWareType;
begin
  SaveStream.Write('PlayerStats');
  SaveStream.Write(Houses, SizeOf(Houses));
  SaveStream.Write(Units, SizeOf(Units));
  SaveStream.Write(Wares, SizeOf(Wares));
  SaveStream.Write(fResourceRatios, SizeOf(fResourceRatios));
  SaveStream.Write(HouseBlocked, SizeOf(HouseBlocked));
  SaveStream.Write(HouseGranted, SizeOf(HouseGranted));
  SaveStream.Write(AllowToTrade, SizeOf(AllowToTrade));
  SaveStream.Write(fHouseUnlocked, SizeOf(fHouseUnlocked));

  SaveStream.Write(fChartCount);
  if fChartCount <> 0 then
  begin
    SaveStream.Write(fChartHouses[0], SizeOf(fChartHouses[0]) * fChartCount);
    SaveStream.Write(fChartCitizens[0], SizeOf(fChartCitizens[0]) * fChartCount);
    SaveStream.Write(fChartArmy[0], SizeOf(fChartArmy[0]) * fChartCount);
    for R := WARE_MIN to WARE_MAX do
      SaveStream.Write(fChartWares[R][0], SizeOf(fChartWares[R][0]) * fChartCount);
  end;
end;


procedure TKMPlayerStats.Load(LoadStream: TKMemoryStream);
var R: TWareType;
begin
  LoadStream.ReadAssert('PlayerStats');
  LoadStream.Read(Houses, SizeOf(Houses));
  LoadStream.Read(Units, SizeOf(Units));
  LoadStream.Read(Wares, SizeOf(Wares));
  LoadStream.Read(fResourceRatios, SizeOf(fResourceRatios));
  LoadStream.Read(HouseBlocked, SizeOf(HouseBlocked));
  LoadStream.Read(HouseGranted, SizeOf(HouseGranted));
  LoadStream.Read(AllowToTrade, SizeOf(AllowToTrade));
  LoadStream.Read(fHouseUnlocked, SizeOf(fHouseUnlocked));

  LoadStream.Read(fChartCount);
  if fChartCount <> 0 then
  begin
    fChartCapacity := fChartCount;
    SetLength(fChartHouses, fChartCount);
    SetLength(fChartCitizens, fChartCount);
    SetLength(fChartArmy, fChartCount);
    LoadStream.Read(fChartHouses[0], SizeOf(fChartHouses[0]) * fChartCount);
    LoadStream.Read(fChartCitizens[0], SizeOf(fChartCitizens[0]) * fChartCount);
    LoadStream.Read(fChartArmy[0], SizeOf(fChartArmy[0]) * fChartCount);
    for R := WARE_MIN to WARE_MAX do
    begin
      SetLength(fChartWares[R], fChartCount);
      LoadStream.Read(fChartWares[R][0], SizeOf(fChartWares[R][0]) * fChartCount);
    end;
  end;
end;


procedure TKMPlayerStats.UpdateState;
var I: TWareType;
begin
  if not DISPLAY_CHARTS_RESULT then Exit;

  //Store player stats in Chart

  //Grow the list
  if fChartCount >= fChartCapacity then
  begin
    fChartCapacity := fChartCount + 32;
    SetLength(fChartHouses, fChartCapacity);
    SetLength(fChartCitizens, fChartCapacity);
    SetLength(fChartArmy, fChartCapacity);
    for I := WARE_MIN to WARE_MAX do
      SetLength(fChartWares[I], fChartCapacity);
  end;

  fChartHouses[fChartCount] := GetHouseQty(ht_Any);
  fChartArmy[fChartCount] := GetArmyCount;
  //We don't want recruits on the citizens Chart on the results screen.
  //If we include recruits the citizens Chart drops by 50-100 at peacetime because all the recruits
  //become soldiers, and continually fluctuates. Recruits dominate the Chart, meaning you can't use
  //it for the intended purpose of looking at your villagers. The army Chart already indicates when
  //you trained soldiers, no need to see big variations in the citizens Chart because of recruits.
  fChartCitizens[fChartCount] := GetCitizensCount - GetUnitQty(ut_Recruit);

  for I := WARE_MIN to WARE_MAX do
    fChartWares[I, fChartCount] := Wares[I].Produced;

  Inc(fChartCount);
end;


end.
