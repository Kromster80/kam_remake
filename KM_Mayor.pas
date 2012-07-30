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
    QuaryCount: Byte;
    RecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    RecruitFactor: Byte;
    SchoolCount: Byte;
    SerfFactor: Byte;
    WoodCount: Byte;
    WorkerFactor: Byte;
    constructor Create;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fMayorSetup: TKMMayorSetup;

    function HouseCount(aHouse: THouseType): Integer;
    procedure TryBuildHouse(aHouse: THouseType);

    procedure CheckUnitCount;
    procedure CheckHouseVitalCount;
    procedure CheckHouseMiningCount;
    procedure CheckHouseFoodCount;
    procedure CheckHouseDefenceCount;
    procedure CheckHouseWeaponryCount;
    procedure CheckHouseCount;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    property MayorSetup: TKMMayorSetup read fMayorSetup;

    procedure OwnerUpdate(aPlayer:TPlayerIndex);

    procedure UpdateState;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource;


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
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);
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
const RAD = 4;
var
  I, K: Integer;
  Loc: TKMPoint;
  Store: TKMHouse;
  P: TKMPlayer;
  NodeList: TKMPointList;
  NodeTagList: TKMPointTagList;
  RoadExists: Boolean;
begin
  P := fPlayers[fOwner];

  //Skip disabled houses
  if not P.Stats.GetCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited to 3
  if (P.Stats.GetHouseWip(ht_Any) >= 3) then Exit;

  if not fCityPlanner.FindPlaceForHouse(aHouse, Loc) then Exit;

  P.AddHousePlan(aHouse, Loc);
  Loc := KMPointBelow(Loc);

  Store := P.Houses.FindHouse(ht_Any, Loc.X, Loc.Y, 1, True);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_Make(Loc, KMPointBelow(Store.GetEntrance), [CanMakeRoads, CanWalkRoad], 0, nil, NodeList, False);

    if RoadExists then
      for I := 0 to NodeList.Count - 1 do
        //We must check if we can add the plan ontop of plans placed earlier in this turn
        if P.CanAddFieldPlan(NodeList[I], ft_Road) then
          P.BuildList.FieldworksList.AddField(NodeList[I], ft_Road);
  finally
    NodeList.Free;
  end;

  //Build fields for Farm
  if aHouse = ht_Farm then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y + 1, fTerrain.MapY - 1) to Min(Loc.Y + 1 + RAD - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - RAD, 1) to Min(Loc.X + RAD, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Corn) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X) + Abs(I + 1 - Loc.Y) * 2, 0);

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count - 1, 15) do
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
      for I := Min(Loc.Y + 1, fTerrain.MapY - 1) to Min(Loc.Y + 1 + RAD - 1, fTerrain.MapY - 1) do
      for K := Max(Loc.X - RAD, 1) to Min(Loc.X + RAD, fTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ft_Wine) then
          NodeTagList.AddEntry(KMPoint(K, I), Abs(K - Loc.X) + Abs(I + 1 - Loc.Y) * 2, 0);

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count - 1, 12) do
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
begin
  //Build at least one Store and add one more for each 40 houses
  if (HouseCount(ht_Store) = 0)
  or (HouseCount(ht_Any) div 40 - HouseCount(ht_Store) + 1 > 0) then
    TryBuildHouse(ht_Store);

  //Build at least one School and one more if Barracks are built
  if (HouseCount(ht_School) = 0)
  or (HouseCount(ht_Barracks) - HouseCount(ht_School) + 1 > 0) then
    TryBuildHouse(ht_School);

  //Build at least one Inn and one more for every 60 citizens
  if (HouseCount(ht_Inn) = 0)
  or (fPlayers[fOwner].Stats.GetCitizensCount div 60 - HouseCount(ht_Inn) + 1 > 0) then
    TryBuildHouse(ht_Inn);
end;


procedure TKMayor.CheckHouseMiningCount;
begin
  if (HouseCount(ht_Quary) = 0)
  or ((HouseCount(ht_Sawmill) > 0) and (HouseCount(ht_Quary) < 2)) then
    TryBuildHouse(ht_Quary);

  if (HouseCount(ht_Woodcutters) = 0)
  or ((HouseCount(ht_Sawmill) > 0) and (HouseCount(ht_Woodcutters) < 2)) then
    TryBuildHouse(ht_Woodcutters);

  if (HouseCount(ht_Sawmill) = 0)
  or (HouseCount(ht_Woodcutters) div 2 > HouseCount(ht_Sawmill)) then
    TryBuildHouse(ht_Sawmill);

  if (HouseCount(ht_GoldMine) = 0) then
    TryBuildHouse(ht_GoldMine);
end;


procedure TKMayor.CheckHouseFoodCount;
begin
  if (HouseCount(ht_Farm) < 2)
  or ((HouseCount(ht_Swine) > 0) and (HouseCount(ht_Farm) < 3)) then
    TryBuildHouse(ht_Farm);

  if (HouseCount(ht_Mill) = 0) then
    TryBuildHouse(ht_Mill);

  if (HouseCount(ht_Bakery) = 0) then
    TryBuildHouse(ht_Bakery);

  if (HouseCount(ht_Swine) < 1) then
    TryBuildHouse(ht_Swine);

  if (HouseCount(ht_Butchers) = 0) then
    TryBuildHouse(ht_Butchers);

  if (HouseCount(ht_Wineyard) < 2) then
    TryBuildHouse(ht_Wineyard);
end;


procedure TKMayor.CheckHouseDefenceCount;
begin

end;


procedure TKMayor.CheckHouseWeaponryCount;
begin
  if (HouseCount(ht_Tannery) = 0) then
    TryBuildHouse(ht_Tannery);
end;


procedure TKMayor.CheckHouseCount;
begin
  //Check if we have Store/Inn/School in adequate counts
  CheckHouseVitalCount;

  //Check if we have basic resource mining houses (stone, wood, gold)
  CheckHouseMiningCount;

  //Check if produce adequate food supply
  CheckHouseFoodCount;

  //
  CheckHouseDefenceCount;

  //Check if we make enough weaponry
  CheckHouseWeaponryCount;

  //Check if we need to demolish depleted mining houses

end;


procedure TKMayor.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
end;


procedure TKMayor.UpdateState;
begin
  CheckUnitCount; //Train new units (citizens, serfs, workers and recruits) if needed

  if fMayorSetup.AutoBuild then
    CheckHouseCount;
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

  QuaryCount := 3;
  SchoolCount := 1;
  WoodCount := 3;
end;


procedure TKMMayorSetup.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(AutoBuild);
  LoadStream.Read(AutoRepair);
  LoadStream.Read(SerfFactor);
  LoadStream.Read(RecruitDelay);
  LoadStream.Read(RecruitFactor);
  LoadStream.Read(WorkerFactor);

  LoadStream.Read(QuaryCount);
  LoadStream.Read(SchoolCount);
  LoadStream.Read(WoodCount);
end;


procedure TKMMayorSetup.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(AutoBuild);
  SaveStream.Write(AutoRepair);
  SaveStream.Write(SerfFactor);
  SaveStream.Write(RecruitDelay);
  SaveStream.Write(RecruitFactor);
  SaveStream.Write(WorkerFactor);

  SaveStream.Write(QuaryCount);
  SaveStream.Write(SchoolCount);
  SaveStream.Write(WoodCount);
end;


end.

