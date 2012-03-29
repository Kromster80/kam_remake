unit KM_Mayor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_CityPlanner, KM_PathfindingRoad;


type
  TKMayor = class
  private
    fOwner: TPlayerIndex;
    fCityPlanner: TKMCityPlanner;
    fPathFindingRoad: TPathFindingRoad;

    fAutobuild: Boolean;

    fSerfFactor: Word;
    fWorkerFactor: Word;
    fRecruitFactor: Word;
    fRecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks

    function HouseCount(aHouse: THouseType): Integer;

    procedure CheckUnitCount;

    procedure CheckHouseVitalCount;
    procedure CheckHouseMiningCount;
    procedure CheckHouseFoodCount;
    procedure CheckHouseDefenceCount;
    procedure CheckHouseWeaponryCount;
    procedure CheckHouseCount;
    function TryBuildHouse(aHouse: THouseType): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    destructor Destroy; override;

    property Autobuild: Boolean read fAutobuild write fAutobuild;
    property SerfFactor: Word read fSerfFactor write fSerfFactor;
    property WorkerFactor: Word read fWorkerFactor write fWorkerFactor;
    property RecruitFactor: Word read fRecruitFactor write fRecruitFactor;
    property RecruitDelay: Cardinal read fRecruitDelay write fRecruitDelay;

    procedure UpdateState;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TPathFindingRoad.Create(fOwner);

  fAutobuild := True; //In KaM it is on by default, and most missions turn it off
  fSerfFactor := 10; //Means 1 serf per building
  fWorkerFactor := 6;
  fRecruitFactor := 5; //This means the number in the barracks, watchtowers are counted seperately
  fRecruitDelay := 0; //Can train at start
end;


destructor TKMayor.Destroy;
begin
  fCityPlanner.Free;
  fPathFindingRoad.Free;
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
  k := 1;
  HS := TKMHouseSchool(P.FindHouse(ht_School,k));
  while HS <> nil do
  begin
    Schools[k-1] := HS;
    for i:=1 to 6 do //Decrease requirement for each unit in training
      if HS.UnitQueue[i]<>ut_None then
        dec(UnitReq[HS.UnitQueue[i]]); //Can be negative and compensated by e.g. ReqRecruits
    inc(k);
    HS := TKMHouseSchool(P.FindHouse(ht_School,k));
  end;

  //Order the training
  for k:=1 to Length(Schools) do
  begin
    HS := Schools[k-1];
    if (HS<>nil)and(HS.UnitQueue[1]=ut_None) then
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
      if (HS.UnitQueue[1] = ut_None) then //Still haven't found a match...
        if not CheckUnitRequirements(Round((10/fSerfFactor) * P.Stats.GetHouseQty(ht_Any)), ut_Serf) then
          if not CheckUnitRequirements(fWorkerFactor, ut_Worker) then
            if fGame.CheckTime(fRecruitDelay) then //Recruits can only be trained after this time
              if not CheckUnitRequirements(fRecruitFactor * P.Stats.GetHouseQty(ht_Barracks), ut_Recruit) then
                Break; //There's no unit demand at all
    end;
  end;
end;


function TKMayor.TryBuildHouse(aHouse: THouseType): Boolean;
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
  //P.FogOfWar.RevealEverything;
  Result := False;

  if ((P.Stats.GetHouseQty(ht_Any) < 3) and (P.Stats.GetHouseWip(ht_Any) >= 1))
  or (P.Stats.GetHouseWip(ht_Any) >= 3) then
    Exit;

  Result := fCityPlanner.FindPlaceForHouse(aHouse, Loc);
  if not Result then Exit;

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
end;


function TKMayor.HouseCount(aHouse: THouseType): Integer;
begin
  Result := fPlayers[fOwner].Stats.GetHouseQty(aHouse) + fPlayers[fOwner].Stats.GetHouseWip(aHouse);
end;


procedure TKMayor.CheckHouseVitalCount;
begin
  if HouseCount(ht_Store) = 0 then
    TryBuildHouse(ht_Store);

  if HouseCount(ht_School) = 0 then
    TryBuildHouse(ht_School);

  if HouseCount(ht_Inn) = 0 then
    TryBuildHouse(ht_Inn);
end;


procedure TKMayor.CheckHouseMiningCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Quary]) then
  begin
    if (HouseCount(ht_Quary) = 0) then
      TryBuildHouse(ht_Quary);

    if (HouseCount(ht_Sawmill) > 1) and (HouseCount(ht_Quary) < 2) then
      TryBuildHouse(ht_Quary);
  end;

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Woodcutters]) then
  begin
    if (HouseCount(ht_Woodcutters) = 0) then
      TryBuildHouse(ht_Woodcutters);

    if (HouseCount(ht_Sawmill) > 1) and (HouseCount(ht_Woodcutters) < 2) then
      TryBuildHouse(ht_Woodcutters);
  end;

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Sawmill]) and (HouseCount(ht_Sawmill) = 0) then
    TryBuildHouse(ht_Sawmill);
end;


procedure TKMayor.CheckHouseFoodCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Farm]) and (HouseCount(ht_Farm) < 2) then
    TryBuildHouse(ht_Farm);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Mill]) and (HouseCount(ht_Mill) = 0) then
    TryBuildHouse(ht_Mill);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Bakery]) and (HouseCount(ht_Bakery) = 0) then
    TryBuildHouse(ht_Bakery);


  if (fPlayers[fOwner].Stats.HouseReleased[ht_Swine]) and (HouseCount(ht_Swine) < 1) then
    TryBuildHouse(ht_Swine);

  if (fPlayers[fOwner].Stats.HouseReleased[ht_Butchers]) and (HouseCount(ht_Butchers) = 0) then
    TryBuildHouse(ht_Butchers);


  if (fPlayers[fOwner].Stats.HouseReleased[ht_Wineyard]) and (HouseCount(ht_Wineyard) < 2) then
    TryBuildHouse(ht_Wineyard);
end;


procedure TKMayor.CheckHouseDefenceCount;
begin

end;


procedure TKMayor.CheckHouseWeaponryCount;
begin
  if (fPlayers[fOwner].Stats.HouseReleased[ht_Tannery]) and (HouseCount(ht_Tannery) = 0) then
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

  //Pick best house to build
  {BestHouse := ht_None;
  for H := Low(THouseType) to High(THouseType) do
  begin
    if fHouseImportance[H] > Max(fHouseImportance[BestHouse], 0.5) then
      BestHouse := H;
  end;

  if BestHouse <> ht_None then
    if fCityPlanner.FindPlaceForHouse(BestHouse, Loc) then
      BuildHouse(BestHouse, Loc);}
end;


procedure TKMayor.UpdateState;
begin
  CheckUnitCount; //Train new units (citizens, serfs, workers and recruits) if needed

  if fAutobuild then
    CheckHouseCount;
end;


procedure TKMayor.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fAutobuild);
  SaveStream.Write(fSerfFactor);
  SaveStream.Write(fWorkerFactor);
  SaveStream.Write(fRecruitFactor);
  SaveStream.Write(fRecruitDelay);
end;


procedure TKMayor.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fAutobuild);
  LoadStream.Read(fSerfFactor);
  LoadStream.Read(fWorkerFactor);
  LoadStream.Read(fRecruitFactor);
  LoadStream.Read(fRecruitDelay);
end;


end.

