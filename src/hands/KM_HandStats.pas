unit KM_HandStats;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults,
  KM_ResHouses, KM_ResWares, KM_WareDistribution;


//These are stats for each player
type
  THouseStats = packed record
    Planned,             //Houseplans were placed
    PlanRemoved,         //Houseplans were removed
    Started,             //Construction started
    Ended,               //Construction ended (either done or destroyed/cancelled)
    Initial,             //created by script on mission start
    Built,               //constructed by player
    SelfDestruct,        //deconstructed by player
    Lost,                //lost from attacks and self-demolished
    Destroyed: Cardinal; //damage to other players
  end;

  TUnitStats = packed record
    Initial,          //Provided at mission start
    Trained,          //Trained by player
    Lost,             //Died of hunger or killed
    Killed: Cardinal; //Killed (incl. self)
  end;

  TWareStats = packed record
    Initial: Cardinal;
    Produced: Cardinal;
    Consumed: Cardinal;
  end;

  //Player statistics (+ ratios, house unlock, trade permissions)
  TKMHandStats = class
  private
    fChartCount: Integer;
    fChartCapacity: Integer;
    fChartHouses: TKMCardinalArray;
    fChartCitizens: TKMCardinalArray;
    fChartArmy: array [WARRIOR_MIN..WARRIOR_MAX] of TKMCardinalArray;
    fChartArmyTotal: array [WARRIOR_MIN..WARRIOR_MAX] of TKMCardinalArray;
    fArmyEmpty: array [WARRIOR_MIN..WARRIOR_MAX] of Boolean;
    fChartWares: array [WARE_MIN..WARE_MAX] of TKMCardinalArray;

    Houses: array [THouseType] of THouseStats;
    Units: array [HUMANS_MIN..HUMANS_MAX] of TUnitStats;
    Wares: array [WARE_MIN..WARE_MAX] of TWareStats;
    fWareDistribution: TKMWareDistribution;
    function GetChartWares(aWare: TWareType): TKMCardinalArray;
    function GetChartArmy(aWarrior: TUnitType): TKMCardinalArray;
    function GetChartArmyTotal(aWarrior: TUnitType): TKMCardinalArray;
  public
    constructor Create;
    destructor Destroy; override;

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

    property WareDistribution: TKMWareDistribution read fWareDistribution;

    //Output
    function GetHouseQty(aType: THouseType): Integer; overload;
    function GetHouseQty(aType: array of THouseType): Integer; overload;
    function GetHouseWip(aType: THouseType): Integer; overload;
    function GetHousePlans(aType: THouseType): Integer; overload;
    function GetHouseWip(aType: array of THouseType): Integer; overload;
    function GetHouseTotal(aType: THouseType): Integer;
    function GetUnitQty(aType: TUnitType): Integer;
    function GetUnitKilledQty(aType: TUnitType): Integer;
    function GetUnitLostQty(aType: TUnitType): Integer;
    function GetWareBalance(aRT: TWareType): Integer;
    function GetArmyCount: Integer;
    function GetCitizensCount: Integer;

    function GetCitizensTrained: Cardinal;
    function GetCitizensLost: Cardinal;
    function GetCitizensKilled: Cardinal;
    function GetHousesBuilt: Cardinal;
    function GetHousesLost: Cardinal;
    function GetHousesDestroyed: Cardinal;
    function GetWarriorsTrained: Cardinal;
    function GetWarriorsTotal(aWarriorType: TUnitType): Cardinal;
    function GetWarriorsKilled: Cardinal;
    function GetWarriorsLost: Cardinal;
    function GetWaresProduced(aRT: TWareType): Cardinal;
    function GetCivilProduced: Cardinal;
    function GetWeaponsProduced: Cardinal;
    function GetWarfareProduced: Cardinal;

    property ChartCount: Integer read fChartCount;
    property ChartHouses: TKMCardinalArray read fChartHouses;
    property ChartCitizens: TKMCardinalArray read fChartCitizens;
    property ChartWares[aWare: TWareType]: TKMCardinalArray read GetChartWares;
    property ChartArmy[aWarrior: TUnitType]: TKMCardinalArray read GetChartArmy;
    property ChartArmyTotal[aWarrior: TUnitType]: TKMCardinalArray read GetChartArmyTotal;
    function ChartWaresEmpty(aWare: TWareType): Boolean;
    function ChartArmyEmpty(aWarrior: TUnitType): Boolean;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


implementation
uses
  KM_Resource, KM_GameApp;


{ TKMHandStats }
constructor TKMHandStats.Create;
var
  WT: TUnitType;
begin
  inherited;

  fWareDistribution := TKMWareDistribution.Create;

  for WT := WARRIOR_MIN to WARRIOR_MAX do
    fArmyEmpty[WT] := True;
end;


destructor TKMHandStats.Destroy;
begin
  FreeAndNil(fWareDistribution);
  inherited;
end;


procedure TKMHandStats.HousePlanned(aType: THouseType);
begin
  Inc(Houses[aType].Planned);
end;


procedure TKMHandStats.HousePlanRemoved(aType: THouseType);
begin
  Inc(Houses[aType].PlanRemoved);
end;


//New house in progress
procedure TKMHandStats.HouseStarted(aType: THouseType);
begin
  Inc(Houses[aType].Started);
end;


//House building process was ended. We don't really know if it was canceled or destroyed or finished
//Other House** methods will handle that
procedure TKMHandStats.HouseEnded(aType: THouseType);
begin
  Inc(Houses[aType].Ended);
end;


//New house, either built by player or created by mission script
procedure TKMHandStats.HouseCreated(aType: THouseType; aWasBuilt:boolean);
begin
  if aWasBuilt then
    Inc(Houses[aType].Built)
  else
    Inc(Houses[aType].Initial);
end;


//Destroyed by enemy
procedure TKMHandStats.HouseLost(aType: THouseType);
begin
  Inc(Houses[aType].Lost);
end;


procedure TKMHandStats.HouseSelfDestruct(aType: THouseType);
begin
  Inc(Houses[aType].SelfDestruct);
end;


//Player has destroyed an enemy house
procedure TKMHandStats.HouseDestroyed(aType: THouseType);
begin
  Inc(Houses[aType].Destroyed);
end;


procedure TKMHandStats.UnitCreated(aType: TUnitType; aWasTrained:boolean);
begin
  if aWasTrained then
    Inc(Units[aType].Trained)
  else
    Inc(Units[aType].Initial);
end;


procedure TKMHandStats.UnitLost(aType: TUnitType);
begin
  Inc(Units[aType].Lost);
end;


procedure TKMHandStats.UnitKilled(aType: TUnitType);
begin
  Inc(Units[aType].Killed);
end;


procedure TKMHandStats.WareInitial(aRes: TWareType; aCount: Cardinal);
begin
  if not DISPLAY_CHARTS_RESULT then Exit;
  if aRes <> wt_None then
    Inc(Wares[aRes].Initial, aCount);
end;


procedure TKMHandStats.WareProduced(aRes: TWareType; aCount: Cardinal);
var R: TWareType;
begin
  if aRes <> wt_None then
    case aRes of
      wt_All:     for R := WARE_MIN to WARE_MAX do
                    Inc(Wares[R].Produced, aCount);
      WARE_MIN..
      WARE_MAX:   Inc(Wares[aRes].Produced, aCount);
      else        Assert(False, 'Cant''t add produced ware ' + gRes.Wares[aRes].Title);
    end;
end;


procedure TKMHandStats.WareConsumed(aRes: TWareType; aCount: Cardinal = 1);
begin
  if not DISPLAY_CHARTS_RESULT then Exit;
  if aRes <> wt_None then
    Inc(Wares[aRes].Consumed, aCount);
end;


//How many complete houses are there
function TKMHandStats.GetHouseQty(aType: THouseType): Integer;
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
function TKMHandStats.GetHouseQty(aType: array of THouseType): Integer;
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
  if aType[I] in [HOUSE_MIN..HOUSE_MAX] then
    Inc(Result, Houses[aType[I]].Initial + Houses[aType[I]].Built - Houses[aType[I]].SelfDestruct - Houses[aType[I]].Lost)
  else
    Assert(False, 'Quering wrong house type');
end;


//How many houses are planned and in progress
function TKMHandStats.GetHouseWip(aType: THouseType): Integer;
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


//How many house plans player has at certain moment...
function TKMHandStats.GetHousePlans(aType: THouseType): Integer;
begin
  Result := Houses[aType].Planned - Houses[aType].PlanRemoved;
end;


//How many houses are planned in progress and ready
function TKMHandStats.GetHouseTotal(aType: THouseType): Integer;
begin
  Result := GetHouseQty(aType) + GetHouseWip(aType);
end;


//How many houses are planned and in progress
function TKMHandStats.GetHouseWip(aType: array of THouseType): Integer;
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
  if aType[I] in [HOUSE_MIN..HOUSE_MAX] then
    Inc(Result, Houses[aType[I]].Started + Houses[aType[I]].Planned - Houses[aType[I]].Ended - Houses[aType[I]].PlanRemoved)
  else
    Assert(False, 'Quering wrong house type');
end;


function TKMHandStats.GetUnitQty(aType: TUnitType): Integer;
var
  UT: TUnitType;
begin
  Result := 0;
  case aType of
    ut_None: ;
    ut_Any:     for UT := HUMANS_MIN to HUMANS_MAX do
                  Inc(Result, Units[UT].Initial + Units[UT].Trained - Units[UT].Lost);
    else        begin
                  Result := Units[aType].Initial + Units[aType].Trained - Units[aType].Lost;
                  if aType = ut_Recruit then
                    for UT := WARRIOR_MIN to WARRIOR_MAX do
                      dec(Result, Units[UT].Trained); //Trained soldiers use a recruit
                end;
  end;
end;


function TKMHandStats.GetUnitKilledQty(aType: TUnitType): Integer;
begin
  Result := Units[aType].Killed;
end;


function TKMHandStats.GetUnitLostQty(aType: TUnitType): Integer;
begin
  Result := Units[aType].Lost;
end;


//How many wares player has right now
function TKMHandStats.GetWareBalance(aRT: TWareType): Integer;
var
  RT: TWareType;
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


function TKMHandStats.GetArmyCount: Integer;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, GetUnitQty(UT));
end;


function TKMHandStats.GetCitizensCount: Integer;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Inc(Result, GetUnitQty(UT));
end;


//The value includes only citizens, Warriors are counted separately
function TKMHandStats.GetCitizensTrained: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Inc(Result, Units[UT].Trained);
end;


function TKMHandStats.GetCitizensLost: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Inc(Result, Units[UT].Lost);
end;


function TKMHandStats.GetCitizensKilled: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Inc(Result, Units[UT].Killed);
end;


function TKMHandStats.GetHousesBuilt: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
    Inc(Result, Houses[HT].Built);
end;


function TKMHandStats.GetHousesLost: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
    Inc(Result, Houses[HT].Lost);
end;


function TKMHandStats.GetHousesDestroyed: Cardinal;
var HT: THouseType;
begin
  Result := 0;
  for HT := HOUSE_MIN to HOUSE_MAX do
    Inc(Result, Houses[HT].Destroyed);
end;


//The value includes all Warriors
function TKMHandStats.GetWarriorsTrained: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Trained);
end;


function TKMHandStats.GetWarriorsTotal(aWarriorType: TUnitType): Cardinal;
begin
  Result := Units[aWarriorType].Initial + Units[aWarriorType].Trained;
end;


function TKMHandStats.GetWarriorsLost: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Lost);
end;


function TKMHandStats.GetWarriorsKilled: Cardinal;
var UT: TUnitType;
begin
  Result := 0;
  for UT := WARRIOR_MIN to WARRIOR_MAX do
    Inc(Result, Units[UT].Killed);
end;


function TKMHandStats.GetWaresProduced(aRT: TWareType): Cardinal;
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
function TKMHandStats.GetCivilProduced: Cardinal;
var RT: TWareType;
begin
  Result := 0;
  for RT := WARE_MIN to WARE_MAX do
  if not (RT in [WEAPON_MIN..WEAPON_MAX]) then
    Inc(Result, Wares[RT].Produced);
end;


//KaM includes all weapons and armor, but not horses
function TKMHandStats.GetWeaponsProduced: Cardinal;
var RT: TWareType;
begin
  Result := 0;
  for RT := WEAPON_MIN to WEAPON_MAX do
    Inc(Result, Wares[RT].Produced);
end;


function TKMHandStats.GetWarfareProduced: Cardinal;
var RT: TWareType;
begin
  Result := 0;
  for RT := WARFARE_MIN to WARFARE_MAX do
    Inc(Result, Wares[RT].Produced);
end;


function TKMHandStats.GetChartWares(aWare: TWareType): TKMCardinalArray;
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


function TKMHandStats.GetChartArmy(aWarrior: TUnitType): TKMCardinalArray;
var
  WT: TUnitType;
  I: Integer;
begin
  case aWarrior of
    WARRIOR_MIN..WARRIOR_MAX: Result := fChartArmy[aWarrior];
    ut_Any:                   begin
                                //Create new array and fill it (otherwise we assign pointers and corrupt data)
                                SetLength(Result, fChartCount);
                                for I := 0 to fChartCount - 1 do
                                  Result[I] := 0;
                                for WT := WARRIOR_MIN to WARRIOR_MAX do
                                  for I := 0 to fChartCount - 1 do
                                    Result[I] := Result[I] + fChartArmy[WT][I];
                              end;
    else                      begin
                                //Return empty array
                                SetLength(Result, fChartCount);
                                for I := 0 to fChartCount - 1 do
                                  Result[I] := 0;
                              end;
  end;
end;


function TKMHandStats.GetChartArmyTotal(aWarrior: TUnitType): TKMCardinalArray;
var
  I: Integer;
  WT: TUnitType;
begin
  case aWarrior of
    WARRIOR_MIN..WARRIOR_MAX: Result := fChartArmyTotal[aWarrior];
    ut_Any:                   begin
                                //Create new array and fill it (otherwise we assign pointers and corrupt data)
                                SetLength(Result, fChartCount);
                                for I := 0 to fChartCount - 1 do
                                  Result[I] := 0;
                                for WT := WARRIOR_MIN to WARRIOR_MAX do
                                  for I := 0 to fChartCount - 1 do
                                    Result[I] := Result[I] + fChartArmyTotal[WT][I];
                              end;
    else                      begin
                                //Return empty array
                                SetLength(Result, fChartCount);
                                for I := 0 to fChartCount - 1 do
                                  Result[I] := 0;
                              end;
  end;
end;


function TKMHandStats.ChartWaresEmpty(aWare: TWareType): Boolean;
var
  RT: TWareType;
begin
  case aWare of
    WARE_MIN..WARE_MAX: Result := (fChartCount = 0) or (ChartWares[aWare][fChartCount-1] = 0);
    wt_All:             begin
                          Result := True;
                          if fChartCount > 0 then
                            for RT := WARE_MIN to WARE_MAX do
                              if ChartWares[RT][fChartCount-1] <> 0 then
                                Result := False;
                        end;
    wt_Warfare:         begin
                          Result := True;
                          if fChartCount > 0 then
                            for RT := WARFARE_MIN to WARFARE_MAX do
                              if ChartWares[RT][fChartCount-1] <> 0 then
                                Result := False;
                        end;
    wt_Food:            Result := (fChartCount = 0) or
                                  (ChartWares[wt_Wine][fChartCount-1] +
                                   ChartWares[wt_Bread][fChartCount-1] +
                                   ChartWares[wt_Sausages][fChartCount-1] +
                                   ChartWares[wt_Fish][fChartCount-1] = 0);
    else                Result := True;
  end;
end;


function TKMHandStats.ChartArmyEmpty(aWarrior: TUnitType): Boolean;
var
  WT: TUnitType;
begin
  case aWarrior of
    WARRIOR_MIN..WARRIOR_MAX:
                        Result := (fChartCount = 0) or (fArmyEmpty[aWarrior]);
    ut_Any:             begin
                          Result := True;
                          if fChartCount > 0 then
                            for WT := WARRIOR_MIN to WARRIOR_MAX do
                              if not fArmyEmpty[WT] then
                              begin
                                Result := False;
                                Break;
                              end;
                        end;
    else                Result := True;
  end;
end;


procedure TKMHandStats.Save(SaveStream: TKMemoryStream);
var
  R: TWareType;
  W: TUnitType;
begin
  SaveStream.WriteA('PlayerStats');
  SaveStream.Write(Houses, SizeOf(Houses));
  SaveStream.Write(Units, SizeOf(Units));
  SaveStream.Write(Wares, SizeOf(Wares));
  fWareDistribution.Save(SaveStream);

  SaveStream.Write(fChartCount);
  if fChartCount <> 0 then
  begin
    SaveStream.Write(fChartHouses[0], SizeOf(fChartHouses[0]) * fChartCount);
    SaveStream.Write(fChartCitizens[0], SizeOf(fChartCitizens[0]) * fChartCount);

    for R := WARE_MIN to WARE_MAX do
      SaveStream.Write(fChartWares[R][0], SizeOf(fChartWares[R][0]) * fChartCount);
    for W := WARRIOR_MIN to WARRIOR_MAX do
    begin
      SaveStream.Write(fChartArmy[W][0], SizeOf(fChartArmy[W][0]) * fChartCount);
      SaveStream.Write(fChartArmyTotal[W][0], SizeOf(fChartArmyTotal[W][0]) * fChartCount);
    end;
  end;
end;


procedure TKMHandStats.Load(LoadStream: TKMemoryStream);
var
  I: TWareType;
  J: TUnitType;
begin
  LoadStream.ReadAssert('PlayerStats');
  LoadStream.Read(Houses, SizeOf(Houses));
  LoadStream.Read(Units, SizeOf(Units));
  LoadStream.Read(Wares, SizeOf(Wares));
  fWareDistribution.Load(LoadStream);

  LoadStream.Read(fChartCount);
  if fChartCount <> 0 then
  begin
    fChartCapacity := fChartCount;
    SetLength(fChartHouses, fChartCount);
    SetLength(fChartCitizens, fChartCount);
    LoadStream.Read(fChartHouses[0], SizeOf(fChartHouses[0]) * fChartCount);
    LoadStream.Read(fChartCitizens[0], SizeOf(fChartCitizens[0]) * fChartCount);
    for I := WARE_MIN to WARE_MAX do
    begin
      SetLength(fChartWares[I], fChartCount);
      LoadStream.Read(fChartWares[I][0], SizeOf(fChartWares[I][0]) * fChartCount);
    end;
    for J := WARRIOR_MIN to WARRIOR_MAX do
    begin
      SetLength(fChartArmy[J], fChartCount);
      LoadStream.Read(fChartArmy[J][0], SizeOf(fChartArmy[J][0]) * fChartCount);
      SetLength(fChartArmyTotal[J], fChartCount);
      LoadStream.Read(fChartArmyTotal[J][0], SizeOf(fChartArmyTotal[J][0]) * fChartCount);
    end;
  end;
end;


procedure TKMHandStats.UpdateState;
var
  I: TWareType;
  J: TUnitType;
  ArmyQty: Integer;
begin
  if not DISPLAY_CHARTS_RESULT then Exit;

  //Store player stats in Chart

  //Grow the list
  if fChartCount >= fChartCapacity then
  begin
    fChartCapacity := fChartCount + 32;
    SetLength(fChartHouses, fChartCapacity);
    SetLength(fChartCitizens, fChartCapacity);
    for I := WARE_MIN to WARE_MAX do
      SetLength(fChartWares[I], fChartCapacity);
    for J := WARRIOR_MIN to WARRIOR_MAX do
    begin
      SetLength(fChartArmy[J], fChartCapacity);
      SetLength(fChartArmyTotal[J], fChartCapacity);
    end;
  end;
  fChartHouses[fChartCount] := GetHouseQty(ht_Any);
  //We don't want recruits on the citizens Chart on the results screen.
  //If we include recruits the citizens Chart drops by 50-100 at peacetime because all the recruits
  //become soldiers, and continually fluctuates. Recruits dominate the Chart, meaning you can't use
  //it for the intended purpose of looking at your villagers. The army Chart already indicates when
  //you trained soldiers, no need to see big variations in the citizens Chart because of recruits.
  fChartCitizens[fChartCount] := GetCitizensCount - GetUnitQty(ut_Recruit);

  for I := WARE_MIN to WARE_MAX do
    fChartWares[I, fChartCount] := Wares[I].Produced;

  for J := WARRIOR_MIN to WARRIOR_MAX do
  begin
    ArmyQty := GetUnitQty(J);
    fChartArmy[J, fChartCount] := ArmyQty;
    if (fArmyEmpty[J] and (ArmyQty > 0)) then
      fArmyEmpty[J] := False;
  end;

  for J := WARRIOR_MIN to WARRIOR_MAX do
  begin
    ArmyQty := GetWarriorsTotal(J);
    fChartArmyTotal[J, fChartCount] := ArmyQty;
    if (fArmyEmpty[J] and (ArmyQty > 0)) then
      fArmyEmpty[J] := False;
  end;

  Inc(fChartCount);
end;


end.
