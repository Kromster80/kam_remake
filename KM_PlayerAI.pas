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
var i:integer; UnitType:TUnitType; H:TKMHouse; HC:TKMHousesCollection;
begin
  //Find school and make sure it's free of tasks
  H := Assets.FindHouse(ht_School,KMPoint(0,0),1);
  if H <> nil then
    if TKMHouseSchool(H).UnitQueue[1]<>ut_None then exit;

  UnitType := ut_None;
  HC:=Assets.GetHouses;
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

end.
 