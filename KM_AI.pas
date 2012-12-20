unit KM_AI;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, Math,
    KM_CommonClasses, KM_Defaults,
    KM_Houses, KM_Units, KM_UnitGroups, KM_Units_Warrior, KM_Points,
    KM_AISetup, KM_AIMayor, KM_AIGeneral;


type
  TWonOrLost = (wol_None, wol_Won, wol_Lost);

  TKMPlayerAI = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;
    fMayor: TKMayor;
    fGeneral: TKMGeneral;

    fWonOrLost: TWonOrLost; //Has this player won/lost? If so, do not check goals

    procedure CheckDefeated;
    procedure CheckGoals;
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property Setup: TKMPlayerAISetup read fSetup;
    property Mayor: TKMayor read fMayor;
    property General: TKMGeneral read fGeneral;

    procedure Defeat; //Defeat the player, this is not reversible

    property WonOrLost: TWonOrLost read fWonOrLost;

    procedure OwnerUpdate(aPlayer: TPlayerIndex);

    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);

    function HouseAutoRepair: Boolean; //Do we automatically repair all houses?

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Goals, KM_Player, KM_PlayerStats,
     KM_Sound, KM_Scripting;


{ TKMPlayerAI }
constructor TKMPlayerAI.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;

  fOwner := aPlayerIndex;
  fSetup := TKMPlayerAISetup.Create;
  fMayor := TKMayor.Create(fOwner, fSetup);
  fGeneral := TKMGeneral.Create(fOwner, fSetup);
  fWonOrLost := wol_None;
end;


destructor TKMPlayerAI.Destroy;
begin
  fGeneral.Free;
  fMayor.Free;
  fSetup.Free;

  inherited;
end;


//Defeat Player (from scripting?), this is not reversible.
//Defeated player remains in place, but does no actions
procedure TKMPlayerAI.Defeat;
begin
  if fWonOrLost <> wol_Lost then
  begin
    fWonOrLost := wol_Lost;

    //Script may have additional event processors
    fScripting.ProcPlayerDefeated(fOwner);
  end;
end;


procedure TKMPlayerAI.CheckDefeated;
var
  DoDefeat: Boolean;
  Stat: TKMPlayerStats;
begin
  Stat := fPlayers[fOwner].Stats;

  //KaM defeat conditions are merge of two things: simplicity and objective.
  //They imply that enemy is powerful enough to destroy all houses and units,
  //but destroying Store-School-Barracks-Army is enough to show that.
  //Of course opponent can rebuild with workers, but that will take a lot of time in
  //already half-ruined city.

  DoDefeat := (Stat.GetHouseQty([ht_School, ht_Barracks, ht_Store, ht_TownHall, ht_SiegeWorkshop]) = 0) and
            (Stat.GetArmyCount = 0);

  //Let the event system know (defeat may trigger events for other players)
  if DoDefeat then
    Defeat;
end;


procedure TKMPlayerAI.CheckGoals;

  function GoalConditionSatisfied(aGoal: TKMGoal): Boolean;
  var Stat: TKMPlayerStats;
  begin
    Assert((aGoal.GoalCondition = gc_Time) or (aGoal.PlayerIndex <> -1), 'Only gc_Time can have nil Player');

    Result := False;
    if aGoal.PlayerIndex <> -1 then
      Stat := fPlayers[aGoal.PlayerIndex].Stats
    else
      Stat := nil;

    case aGoal.GoalCondition of
      gc_BuildTutorial:     Result := Stat.GetHouseQty([ht_Tannery]) = 0; //For some reason this goal is gs_False in KaM, that's why check is =0 not  > 0
      //gc_Time is disabled as we process messages in Event system now. Return true so players
      //do not have to wait for all messages to show before they are allowed to win (same in TPR)
      gc_Time:              Result := True;
      gc_Buildings:         Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Barracks]) > 0);
      gc_Troops:            Result := (Stat.GetArmyCount > 0);
      gc_MilitaryAssets:    Result := (Stat.GetArmyCount > 0) or
                                      (Stat.GetHouseQty([ht_Barracks, ht_CoalMine, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_Stables,
                                                         ht_IronMine, ht_IronSmithy ,ht_WeaponSmithy, ht_ArmorSmithy, ht_TownHall,
                                                         ht_SiegeWorkshop]) > 0);
      gc_SerfsAndSchools:   Result := (Stat.GetHouseQty([ht_School]) > 0) or (Stat.GetUnitQty(ut_Serf) > 0);
      gc_EconomyBuildings:  Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Inn]) > 0);
      else                  Assert(false, 'Unknown goal');
    end;
    if aGoal.GoalStatus = gs_False then
      Result := not Result; //Reverse condition
  end;

var
  I: Integer;
  VictorySatisfied, SurvivalSatisfied: Boolean;
begin
  //If player has elected to play on past victory or defeat
  //then do not check for any further goals
  if fWonOrLost <> wol_None then Exit;

  //Assume they will win/survive, then prove it with goals
  VictorySatisfied  := True;
  SurvivalSatisfied := True;

  with fPlayers[fOwner] do
  for I := 0 to Goals.Count - 1 do //Test each goal to see if it has occured
    if GoalConditionSatisfied(Goals[I]) then
    begin
      //Messages in goals have been replaced by EVT files, so this code is disabled now,
      //but kept in case we need it for something later. (conversion process?)

      //Display message if set and not already shown and not a blank text
      {if (Goals[I].MessageToShow <> 0)
      and not Goals[I].MessageHasShown
      and (fTextLibrary[Goals[I].MessageToShow] <> '') then
      begin
        if MyPlayer = fPlayers[fPlayerIndex] then
          fGameG.ShowMessage(mkText, fTextLibrary[Goals[I].MessageToShow], KMPoint(0,0));
        Goals.SetMessageHasShown(I);
      end;}
    end
    else
    begin
      if Goals[I].GoalType = glt_Victory then
        VictorySatisfied := False;
      if Goals[I].GoalType = glt_Survive then
        SurvivalSatisfied := False;
    end;

  //Now we know if player has been defeated or won
  if not SurvivalSatisfied then
    fWonOrLost := wol_Lost
  else
    if VictorySatisfied then
      fWonOrLost := wol_Won;

  //You can't win and lose at the same time. In KaM defeats override victories, except
  //when there are no goals defined, in which case you win for some weird reason...
  //But given that having no goals is pretty pointless we'll make defeat override so you can't
  //win battle missions by waiting for your troops to simultainiously starve to death.

  //Let everyone know in MP mode
  if not fGame.IsReplay
  and (fGame.IsMultiplayer or (MyPlayer = fPlayers[fOwner])) then
    if not SurvivalSatisfied then
      fGame.PlayerDefeat(fOwner)
    else
    if VictorySatisfied then
      fGame.PlayerVictory(fOwner);
end;


procedure TKMPlayerAI.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
  fMayor.OwnerUpdate(fOwner);
  fGeneral.OwnerUpdate(fOwner);
end;


//aHouse is our house that was attacked
procedure TKMPlayerAI.HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
begin
  case fPlayers[fOwner].PlayerType of
    pt_Human:
      begin
        //No fight alerts in replays, and only show alerts for ourselves
        if (not fGame.IsReplay) and (fOwner = MyPlayer.PlayerIndex) then
          fGame.Alerts.AddFight(KMPointF(aHouse.GetPosition), fOwner, an_Town);
      end;
    pt_Computer:
      fGeneral.RetaliateAgainstThreat(aAttacker);
  end;
end;


//aUnit is our unit that was attacked
procedure TKMPlayerAI.UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);
const
  NotifyKind: array [Boolean] of TAttackNotification = (an_Citizens, an_Troops);
var
  Group: TKMUnitGroup;
begin
  case fPlayers[fOwner].PlayerType of
    pt_Human:
      //No fight alerts in replays, and only show alerts for ourselves
      if not fGame.IsReplay and (fOwner = MyPlayer.PlayerIndex) then
        fGame.Alerts.AddFight(aUnit.PositionF, fOwner, NotifyKind[aUnit is TKMUnitWarrior]);
    pt_Computer:
      begin
        //If we are attacked, then we should counter attack the attacker!
        if aUnit is TKMUnitWarrior then
        begin
          Group := fPlayers[fOwner].UnitGroups.GetGroupByMember(TKMUnitWarrior(aUnit));
          Assert(Group <> nil, 'Each Warrior must belong to some Group');
          //If we are already in the process of attacking something, don't change our minds,
          //otherwise you can make a unit walk backwards and forwards forever between two groups of archers.
          if not Group.IsDead
          and not Group.InFight then
            Group.OrderAttackUnit(aAttacker, True);
        end;
        fGeneral.RetaliateAgainstThreat(aAttacker); //Nearby soldiers should come to assist
      end;
  end;
end;


//Do we automatically repair all houses?
//For now use fAutobuild, which is what KaM does. Later we can add a script command to turn this on and off
//Also could be changed later to disable repairing when under attack? (only repair if the enemy goes away?)
function TKMPlayerAI.HouseAutoRepair: Boolean;
begin
  Result := fSetup.AutoBuild;
end;


procedure TKMPlayerAI.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('PlayerAI');
  SaveStream.Write(fOwner);
  SaveStream.Write(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Save(SaveStream);
  fGeneral.Save(SaveStream);
  fMayor.Save(SaveStream);
end;


procedure TKMPlayerAI.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(fOwner);
  LoadStream.Read(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Load(LoadStream);
  fGeneral.Load(LoadStream);
  fMayor.Load(LoadStream);
end;


procedure TKMPlayerAI.SyncLoad;
begin
  fGeneral.SyncLoad;
end;


//todo: Updates should be well separated, maybe we can make an interleaved array or something
//where updates will stacked to execute 1 at a tick
//OR maybe we can collect all Updates into one list and run them from there (sounds like a better more manageble idea)
procedure TKMPlayerAI.UpdateState(aTick: Cardinal);
begin
  //Check if player has been defeated for Events
  if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
    CheckDefeated;

  //Check goals for all players to maintain multiplayer consistency
  //AI does not care if it won or lost and Human dont need Mayor and Army management
  case fPlayers[fOwner].PlayerType of
    pt_Human:     begin
                    if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
                      CheckGoals; //This procedure manages victory and loss
                  end;
    pt_Computer:  begin
                    fMayor.UpdateState(aTick);
                    fGeneral.UpdateState(aTick);
                  end;
  end;
end;


end.
