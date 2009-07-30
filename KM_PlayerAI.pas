unit KM_PlayerAI;
interface
uses KM_Defaults, KromUtils, KM_Player, KM_Utils;


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

implementation
uses KM_Houses, KM_Units, KM_Game, KM_PlayersCollection;

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
  and(Assets.fMissionSettings.GetArmyCount=0)
  then
    fGame.StopGame(gr_Defeat);

end;


procedure TKMPlayerAI.CheckCitizenCount();
var i:integer; UnitType:TUnitType; H:TKMHouse;
begin
  //Find school and make sure it's free of tasks
  H := Assets.FindHouse(ht_School,KMPoint(0,0),1);
  if (H=nil)or(TKMHouseSchool(H).UnitQueue[1]<>ut_None) then exit;

  UnitType := ut_None;

  //@Lewin:
  //I think this is the way to go instead of querrying all houses:
  //Just need to adjoin counts of houses which have same owner types
  //@Krom: Seems to be working perfectly! :D To be deleted

  for i:=1 to HOUSE_COUNT do begin
    if THouseType(i)<>ht_Barracks then //Exclude Barracks
    if HouseDAT[i].OwnerType<>-1 then
    if Assets.GetHouseQty(THouseType(i))>Assets.GetUnitQty(TUnitType(HouseDAT[i].OwnerType+1)) then
      UnitType := TUnitType(HouseDAT[i].OwnerType+1);
    if UnitType <> ut_None then break; //Don't need more UnitTypes yet
  end;

  if UnitType = ut_None then exit;

  TKMHouseSchool(H).AddUnitToQueue(UnitType);
end;


procedure TKMPlayerAI.UpdateState();
begin
  //Check defeat only for MyPlayer
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
    CheckDefeatConditions //Store+Barracks+School+Armies = 0
  else
  
  if Assets.PlayerType=pt_Computer then begin
  CheckCitizenCount; //Train new citizens if needed
  //CheckSerfCount; //train more serfs according to WORKER_FACTOR
  //CheckRecruitCount //Train more recruits according to RECRUTS
  //CheckArmyHunger; //issue tasks to feed troops
  //CheckHouseCount; //Build new houses if needed
  //CheckArmiesCount; //Train new soldiers if needed
  //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
  //CheckAndIssueAttack; //Attack enemy
  //Anything Else?
  end;
end;

end.
 