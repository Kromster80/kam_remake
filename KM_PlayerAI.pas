unit KM_PlayerAI;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KromUtils, KM_Player, KM_Utils;


type
  TKMPlayerAI = class
  private
    Assets:TKMPlayerAssets; //This is just alias for Players assets
  public
    ReqWorkers, ReqSerfFactor, ReqRecruits: word; //Nunber of each unit type required
    RecruitTrainTimeout: longword; //Recruits (for barracks) can only be trained after this many ticks
    constructor Create(aAssets:TKMPlayerAssets);
    procedure CheckDefeatConditions();
    procedure CheckUnitCount();
  public
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SyncLoad();
    procedure UpdateState;
  end;

implementation
uses KM_Houses, KM_Units, KM_Game, KM_PlayersCollection;

constructor TKMPlayerAI.Create(aAssets:TKMPlayerAssets);
begin
  Inherited Create;
  Assets := aAssets;
  //Set some defaults (these are not measure from KaM)
  ReqWorkers := 3;
  ReqRecruits := 10; //This means the number in the barracks, watchtowers are counted seperately
  ReqSerfFactor := 5; //Means 2 serfs per building
  RecruitTrainTimeout := 0; //Can train at start
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


procedure TKMPlayerAI.CheckUnitCount();
var
  i,k:integer;
  UnitType:TUnitType;
  H:TKMHouse;
  UnitReq:array[1..HOUSE_COUNT]of integer; //There are only ~10 unit types, but using HOUSE_COUNT is easier
  Schools:array of TKMHouse;

  function CheckUnitRequirements(Req:integer; aUnitType:TUnitType):boolean;
  begin
    if Assets.GetUnitQty(aUnitType) < (Req+UnitReq[integer(aUnitType)]) then
    begin
      dec(UnitReq[integer(aUnitType)]); //So other schools don't order same unit
      TKMHouseSchool(H).AddUnitToQueue(aUnitType);
      Result := true;
    end
    else
      Result := false;
  end;
begin
  //Find school and make sure it's free of tasks

  UnitType := ut_None;
  FillChar(UnitReq,SizeOf(UnitReq),#0); //Clear up

  //Citizens
  for i:=1 to HOUSE_COUNT do begin //Count overall unit requirement
    if THouseType(i)<>ht_Barracks then //Exclude Barracks
    if HouseDAT[i].OwnerType<>-1 then //Exclude houses without owners
    inc(UnitReq[HouseDAT[i].OwnerType+1], Assets.GetHouseQty(THouseType(i)));
  end;

  SetLength(Schools,Assets.GetHouseQty(ht_School));
  k := 1;
  H := Assets.FindHouse(ht_School,k);
  while H <> nil do
  begin
    Schools[k-1] := H;
    if (H<>nil)and(H is TKMHouseSchool)and(TKMHouseSchool(H).UnitQueue[1]<>ut_None) then
      dec(UnitReq[integer(TKMHouseSchool(H).UnitQueue[1])]); //Decrease requirement for each unit in training
    inc(k);
    H := Assets.FindHouse(ht_School,k);
  end;

  for k:=1 to Length(Schools) do
  begin
    H := Schools[k-1];
    if (H<>nil)and(H is TKMHouseSchool)and(TKMHouseSchool(H).UnitQueue[1]=ut_None) then
    begin
      for i:=1 to length(UnitReq) do
        if UnitReq[i] > Assets.GetUnitQty(TUnitType(i)) then
        begin
          UnitType := TUnitType(i);
          if UnitType <> ut_None then
          begin
            dec(UnitReq[i]); //So other schools don't order same unit
            TKMHouseSchool(H).AddUnitToQueue(UnitType);
            break; //Don't need more UnitTypes yet
          end;
        end;
      //If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      //Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)
      if (TKMHouseSchool(H).UnitQueue[1] = ut_None) then //Still haven't found a match...
        if not CheckUnitRequirements(Round((10/ReqSerfFactor)*Assets.GetTotalHouseQty),ut_Serf) then
          if not CheckUnitRequirements(ReqWorkers,ut_Worker) then
            if fGame.CheckTime(RecruitTrainTimeout) then //Recruits can only be trained after this time
              CheckUnitRequirements(ReqRecruits,ut_Recruit);
    end;
  end;
end;


procedure TKMPlayerAI.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(ReqWorkers);
  SaveStream.Write(ReqSerfFactor);
  SaveStream.Write(ReqRecruits);
  SaveStream.Write(RecruitTrainTimeout,4);
end;


procedure TKMPlayerAI.Load(LoadStream:TKMemoryStream);
begin
  LoadStream.Read(ReqWorkers);
  LoadStream.Read(ReqSerfFactor);
  LoadStream.Read(ReqRecruits);
  LoadStream.Read(RecruitTrainTimeout,4);
end;


//So far this whole procedure is a placeholder
procedure TKMPlayerAI.SyncLoad();
begin
  //
end;                      


procedure TKMPlayerAI.UpdateState;
begin
  //Check defeat only for MyPlayer
  if (MyPlayer=Assets)and(Assets.PlayerType=pt_Human) then
    CheckDefeatConditions //Store+Barracks+School+Armies = 0
  else
  
  if Assets.PlayerType=pt_Computer then begin
  CheckUnitCount; //Train new units (citizens, serfs, workers and recruits) if needed
  
  //CheckArmyHunger; //issue tasks to feed troops
  //CheckHouseCount; //Build new houses if needed
  //CheckArmiesCount; //Train new soldiers if needed
  //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
  //CheckAndIssueAttack; //Attack enemy
  //Anything Else?
  end;
end;

end.
