unit KM_Mayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_CityPlanner, KM_PathfindingRoad;


type
  TKMMayorType = (mtCustom, mtWarrior);

  TKMMayorSetup = class
    AutoBuild: Boolean;
    AutoRepair: Boolean;
    Strong: Boolean;
    Wooden: Boolean;
    RecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    RecruitFactor: Byte;
    SerfFactor: Byte;
    WorkerFactor: Byte;
    constructor Create;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fCityPlanner: TKMCityPlanner;
    fMayorSetup: TKMMayorSetup;
    fPathFindingRoad: TPathFindingRoad;

    fRoadBelowStore: Boolean;

    function HouseCount(aHouse: THouseType): Integer;
    procedure TryBuildHouse(aHouse: THouseType);

    procedure CheckUnitCount;
    procedure CheckHouseVitalCount;
    procedure CheckHouseMiningCount;
    procedure CheckHouseFoodCount;
    procedure CheckHouseDefenceCount;
    procedure CheckHouseWeaponryCount;
    procedure CheckHouseCount;
    procedure CheckRoadsCount;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    property MayorSetup: TKMMayorSetup read fMayorSetup;
    property CityPlanner: TKMCityPlanner read fCityPlanner;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer:TPlayerIndex);

    procedure UpdateState;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource, KM_Utils;


const //Sample list made by AntonP
  WarriorHouses: array [0..44] of THouseType = (
  ht_School, ht_Inn, ht_Quary, ht_Quary, ht_Quary,
  ht_Woodcutters, ht_Woodcutters, ht_Woodcutters, ht_Woodcutters, ht_Woodcutters,
  ht_Sawmill, ht_Sawmill, ht_Woodcutters, ht_GoldMine, ht_CoalMine,
  ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_CoalMine, ht_CoalMine,
  ht_IronMine, ht_IronMine, ht_CoalMine, ht_IronMine, ht_WeaponSmithy,
  ht_WeaponSmithy, ht_Wineyard, ht_Wineyard, ht_Wineyard, ht_Store,
  ht_Barracks, ht_Farm, ht_Farm, ht_Farm, ht_Mill,
  ht_Mill, ht_Bakery, ht_Bakery, ht_School, ht_IronSmithy,
  ht_IronSmithy, ht_Farm, ht_Swine, ht_WeaponSmithy, ht_ArmorSmithy
  );


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner{, fPathFindingRoad});
  fMayorSetup := TKMMayorSetup.Create;
end;


destructor TKMayor.Destroy;
begin
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  fMayorSetup.Free;
  inherited;
end;


{ Check existing unit count vs house count and train missing citizens }
procedure TKMayor.CheckUnitCount;
var
  HS: TKMHouseSchool;
  UnitReq: array [TUnitType] of Integer;

  function CheckUnitRequirements(Req:integer; aUnitType:TUnitType):boolean;
  begin
    //We summ up requirements for e.g. Recruits required at Towers and Barracks
    if fPlayers[fOwner].Stats.GetUnitQty(aUnitType) < (Req+UnitReq[aUnitType]) then
    begin
      dec(UnitReq[aUnitType]); //So other schools don't order same unit
      HS.AddUnitToQueue(aUnitType, 1);
      Result := true;
    end
    else
      Result := false;
  end;

var
  i,k:integer;
  H:THouseType;
  UT:TUnitType;
  Schools:array of TKMHouseSchool;
  P: TKMPlayer;
begin
  //todo: When training new units make sure we have enough gold left to train
  //stonemason-woodcutter-carpenter-2miners-metallurgist. In other words -
  //dont waste gold if it's not producing yet

  P := fPlayers[fOwner];

  //Find school and make sure it's free of tasks
  FillChar(UnitReq,SizeOf(UnitReq),#0); //Clear up

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  for H := Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[H].IsValid and (fResource.HouseDat[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
      inc(UnitReq[fResource.HouseDat[H].OwnerType], P.Stats.GetHouseQty(H));

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, P.Stats.GetHouseQty(ht_School));
  K := 1;
  HS := TKMHouseSchool(P.FindHouse(ht_School,K));
  while HS <> nil do
  begin
    Schools[K-1] := HS;
    for I := 0 to High(HS.Queue) do //Decrease requirement for each unit in training
      if HS.Queue[I] <> ut_None then
        Dec(UnitReq[HS.Queue[I]]); //Can be negative and compensated by e.g. ReqRecruits
    inc(K);
    HS := TKMHouseSchool(P.FindHouse(ht_School,K));
  end;

  //Order the training
  for k:=1 to Length(Schools) do
  begin
    HS := Schools[k-1];
    if (HS<>nil)and(HS.QueueIsEmpty) then
    begin
      //Order citizen training
      for UT:=Low(UnitReq) to High(UnitReq) do
        if (UnitReq[UT] > 0) and
           (UnitReq[UT] > P.Stats.GetUnitQty(UT)) and
           (UT <> ut_None) then
        begin
          dec(UnitReq[UT]); //So other schools don't order same unit
          HS.AddUnitToQueue(UT, 1);
          break; //Don't need more UnitTypes yet
        end;

      //If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      //Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)
      if HS.QueueIsEmpty then //Still haven't found a match...
        if not CheckUnitRequirements(Round((10/fMayorSetup.SerfFactor) * P.Stats.GetHouseQty(ht_Any)), ut_Serf) then
          if not CheckUnitRequirements(fMayorSetup.WorkerFactor, ut_Worker) then
            if fGame.CheckTime(fMayorSetup.RecruitDelay) then //Recruits can only be trained after this time
              if not CheckUnitRequirements(fMayorSetup.RecruitFactor * P.Stats.GetHouseQty(ht_Barracks), ut_Recruit) then
                Break; //There's no unit demand at all
    end;
  end;
end;


procedure TKMayor.TryBuildHouse(aHouse: THouseType);
  //In fact we want to connect to nearest road piece (not necessarily built yet)
  function TryConnectToRoad(aLoc: TKMPoint): Boolean;
  var
    I: Integer;
    P: TKMPlayer;
    H: TKMHouse;
    LocPlan, LocTo: TKMPoint;
    NodeList: TKMPointList;
    RoadExists: Boolean;
  begin
    Result := False;
    P := fPlayers[fOwner];

    //Find nearest wip or ready house
    H := P.Houses.FindHouse(ht_Any, aLoc.X, aLoc.Y, 1, False);
    if H = nil then Exit; //We are screwed, no houses left
    LocTo := KMPointBelow(H.GetEntrance);
    //Check building plans, maybe there's another plan nearby we can connect to with road
    if P.BuildList.HousePlanList.FindPlan(aLoc, LocPlan) then
      if KMLength(aLoc, LocPlan) <= KMLength(aLoc, LocTo) then
        LocTo := LocPlan;

    NodeList := TKMPointList.Create;
    try
      RoadExists := fPathFindingRoad.Route_Make(aLoc, LocTo, [CanMakeRoads, CanWalkRoad], NodeList);

      if not RoadExists then Exit;

      for I := 0 to NodeList.Count - 1 do
        //We must check if we can add the plan ontop of plans placed earlier in this turn
        if P.CanAddFieldPlan(NodeList[I], ft_Road) then
          P.BuildList.FieldworksList.AddField(NodeList[I], ft_Road);
      Result := True;
    finally
      NodeList.Free;
    end;
  end;

var
  I, K: Integer;
  Loc: TKMPoint;
  P: TKMPlayer;
  NodeTagList: TKMPointTagList;
begin
  P := fPlayers[fOwner];

  //Skip disabled houses
  if not P.Stats.GetCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  if not fCityPlanner.FindPlaceForHouse(aHouse, Loc) then
    //Maybe we get more lucky next tick
    Exit;

  //Place house after road is planned
  P.AddHousePlan(aHouse, Loc);

  //Try to connect newly planned house to road network
  if not TryConnectToRoad(KMPointBelow(Loc)) then Exit;

  //Build fields for Farm
  if aHouse = ht_Farm then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y + 2, fTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y), 0);

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 16) - 1 do
        P.BuildList.FieldworksList.AddField(NodeTagList[I], ft_Corn);
    finally
      NodeTagList.Free;
    end;
  end;

  //Build fields for Wineyard
  if aHouse = ht_Wineyard then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y + 2, fTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Wine) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y), 0);

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 14) - 1 do
        P.BuildList.FieldworksList.AddField(NodeTagList[I], ft_Wine);
    finally
      NodeTagList.Free;
    end;
  end;
end;


function TKMayor.HouseCount(aHouse: THouseType): Integer;
begin
  Result := fPlayers[fOwner].Stats.GetHouseQty(aHouse) + fPlayers[fOwner].Stats.GetHouseWip(aHouse);
end;


procedure TKMayor.CheckHouseVitalCount;
var Req: Integer;
begin
  //Build at least one Store and add one more for each 30 houses
  Req := 1 + (HouseCount(ht_Any) div 30);
  if Req > HouseCount(ht_Store) then
    TryBuildHouse(ht_Store);

  //Build at least one School and one more if Barracks are built
  Req := 1 + HouseCount(ht_Barracks);
  if Req > HouseCount(ht_School) then
    TryBuildHouse(ht_School);

  //Build at least one Inn and one more for every 60 citizens
  Req := 1 + fPlayers[fOwner].Stats.GetCitizensCount div 60;
  if Req > HouseCount(ht_Inn) then
    TryBuildHouse(ht_Inn);
end;


//Basic mining facilities
procedure TKMayor.CheckHouseMiningCount;
var Req: Integer;
begin
  //todo: When building new houses make sure we have enough building materials
  //left to build quary-woodcutters-sawmill. In other words - dont waste stone/wood
  //if it's not producing yet

  //Competitive opponent needs at least 3 quaries build early and 2 more after Sawmill
  Req := 2 + Byte(fMayorSetup.Strong) + Byte(fMayorSetup.Strong and (HouseCount(ht_Sawmill) > 0)) * 1;
  if Req > HouseCount(ht_Quary) then
    TryBuildHouse(ht_Quary);

  //Competitive opponent needs at least 3 woodcutters build early and 2 more after Sawmill
  Req := 1 + Byte(fMayorSetup.Strong) + Byte(fMayorSetup.Strong and (HouseCount(ht_Sawmill) > 0)) * 2;
  if Req > HouseCount(ht_Woodcutters) then
    TryBuildHouse(ht_Woodcutters);

  //Sawmills count depends only on Woodcutters, build 1 per each two
  Req := 1 + HouseCount(ht_Woodcutters) div 2;
  if Req > HouseCount(ht_Sawmill) then
    TryBuildHouse(ht_Sawmill);

  //Competitive opponent needs at least 2 gold mines and maybe 2 more later on?
  Req := 2 + Byte(fMayorSetup.Strong) * 2;
  if Req > HouseCount(ht_CoalMine) then
    TryBuildHouse(ht_CoalMine);

  //Competitive opponent needs at least 2 gold mines and maybe 2 more later on?
  Req := 1 + Byte(HouseCount(ht_Metallurgists) > 0);
  if Req > HouseCount(ht_GoldMine) then
    TryBuildHouse(ht_GoldMine);

  //Competitive opponent needs at least 2 gold mines and maybe 2 more later on?
  Req := 1;
  if Req > HouseCount(ht_Metallurgists) then
    TryBuildHouse(ht_Metallurgists);
end;


procedure TKMayor.CheckHouseFoodCount;
var Req: Integer;
begin
  //Build at least 2 Farms early on
  Req := 1 + Byte(fMayorSetup.Strong) * 2 * Byte(HouseCount(ht_Swine) > 0)
           + fPlayers[fOwner].Stats.GetCitizensCount div 30;
  if Req > HouseCount(ht_Farm) then
    TryBuildHouse(ht_Farm);

  Req := 1 + Byte(fMayorSetup.Strong) + HouseCount(ht_Farm) div 4;
  if Req > HouseCount(ht_Mill) then
    TryBuildHouse(ht_Mill);

  Req := 1 + Byte(fMayorSetup.Strong) + HouseCount(ht_Mill) div 2;
  if Req > HouseCount(ht_Bakery) then
    TryBuildHouse(ht_Bakery);

  Req := 1 + Byte(fMayorSetup.Strong) + HouseCount(ht_Farm) div 4;
  if Req > HouseCount(ht_Swine) then
    TryBuildHouse(ht_Swine);

  Req := 1 + HouseCount(ht_Swine) div 2;
  if Req > HouseCount(ht_Butchers) then
    TryBuildHouse(ht_Butchers);

  Req := 2 + Byte(fMayorSetup.Strong) * 2
           + fPlayers[fOwner].Stats.GetCitizensCount div 30;
  if Req > HouseCount(ht_Wineyard) then
    TryBuildHouse(ht_Wineyard);
end;


procedure TKMayor.CheckHouseDefenceCount;
begin

end;


procedure TKMayor.CheckHouseWeaponryCount;
var
  Req: Integer;
begin
  //Metallurgists produce gold for Inns and Barracks
  Req := 1 + Byte(HouseCount(ht_Barracks) > 0);
  if Req > HouseCount(ht_Metallurgists) then
    TryBuildHouse(ht_Metallurgists);

  if fMayorSetup.Wooden then
  begin
    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_Tannery) then
      TryBuildHouse(ht_Tannery);

    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_ArmorWorkshop) then
      TryBuildHouse(ht_ArmorWorkshop);

    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_WeaponWorkshop) then
      TryBuildHouse(ht_WeaponWorkshop);
  end
  else
  begin
    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_IronMine) then
      TryBuildHouse(ht_IronMine);

    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_IronSmithy) then
      TryBuildHouse(ht_IronSmithy);

    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_WeaponSmithy) then
      TryBuildHouse(ht_WeaponSmithy);

    //
    Req := 1 + Byte(fMayorSetup.Strong);
    if Req > HouseCount(ht_ArmorSmithy) then
      TryBuildHouse(ht_ArmorSmithy);
  end;

  //One barracks is enough
  Req := 1;
  if Req > HouseCount(ht_Barracks) then
    TryBuildHouse(ht_Barracks);
end;


procedure TKMayor.AfterMissionInit;
begin
  fCityPlanner.UpdateInfluence;
end;


procedure TKMayor.CheckHouseCount;
begin
  //Number of simultaneous WIP houses is limited to 3
  if (fPlayers[fOwner].Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  //Check if we have Store/Inn/School in adequate counts
  CheckHouseVitalCount;

  CheckHouseMiningCount;

  if fMayorSetup.Strong then
  begin
    CheckHouseWeaponryCount;
    CheckHouseDefenceCount;
    CheckHouseFoodCount;
  end
  else
  begin
    CheckHouseFoodCount;
    CheckHouseDefenceCount;
    CheckHouseWeaponryCount;
  end;

  //Check if we need to demolish depleted mining houses
  //Not sure if AI should do that though
end;


procedure TKMayor.CheckRoadsCount;
var
  P: TKMPlayer;
  Store: TKMHouse;
  StoreLoc: TKMPoint;
  I, K: Integer;
begin
  P := fPlayers[fOwner];

  //This is one time task to build roads around Store
  //When town becomes larger add road around Store to make traffic smoother
  if not fRoadBelowStore and (P.Stats.GetCitizensCount > 20) then
  begin
    fRoadBelowStore := True;

    Store := P.Houses.FindHouse(ht_Store, 0, 0, 1);
    if Store = nil then Exit;
    StoreLoc := Store.GetPosition;

    for I := Max(StoreLoc.Y - 3, 1) to Min(StoreLoc.Y + 2, fTerrain.MapY - 1) do
    for K := StoreLoc.X - 2 to StoreLoc.X + 2 do
    if P.CanAddFieldPlan(KMPoint(K, I), ft_Road) then
      P.BuildList.FieldworksList.AddField(KMPoint(K, I), ft_Road);
  end;
end;


procedure TKMayor.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
end;


procedure TKMayor.UpdateState;
begin
  //Train new units (citizens, serfs, workers and recruits) if needed
  CheckUnitCount;

  if fMayorSetup.AutoBuild then
  begin
    CheckHouseCount;

    //Build more roads if necessary
    CheckRoadsCount;
  end;
end;


procedure TKMayor.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);

  fCityPlanner.Save(SaveStream);
  fPathFindingRoad.Save(SaveStream);
  fMayorSetup.Save(SaveStream);
end;


procedure TKMayor.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);

  fCityPlanner.Load(LoadStream);
  fPathFindingRoad.Load(LoadStream);
  fMayorSetup.Load(LoadStream);
end;


{ TKMMayorSetup }
constructor TKMMayorSetup.Create;
begin
  inherited;
  AutoBuild := True; //In KaM it is On by default, and most missions turn it off
  AutoRepair := False; //In KaM it is Off by default

  SerfFactor := 10; //Means 1 serf per building
  WorkerFactor := 6;
  RecruitFactor := 5; //This means the number in the barracks, watchtowers are counted seperately
  RecruitDelay := 0; //Can train at start

  Strong := (KaMRandom > 0.5);
  Wooden := (KaMRandom > 0.5);
end;


procedure TKMMayorSetup.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(AutoBuild);
  LoadStream.Read(AutoRepair);
  LoadStream.Read(SerfFactor);
  LoadStream.Read(RecruitDelay);
  LoadStream.Read(RecruitFactor);
  LoadStream.Read(WorkerFactor);

  LoadStream.Read(Strong);
end;


procedure TKMMayorSetup.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(AutoBuild);
  SaveStream.Write(AutoRepair);
  SaveStream.Write(SerfFactor);
  SaveStream.Write(RecruitDelay);
  SaveStream.Write(RecruitFactor);
  SaveStream.Write(WorkerFactor);

  SaveStream.Write(Strong);
end;


end.

