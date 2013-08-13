unit KM_AI;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_Defaults,
  KM_Houses, KM_Units, KM_UnitGroups, KM_Units_Warrior, KM_Points,
  KM_AISetup, KM_AIMayor, KM_AIGeneral, KM_AIGoals;


type
  TWonOrLost = (wol_None, wol_Won, wol_Lost);

  //Things that player does automatically
  //Player AI exists both for AI and Human players, but for AI it does significantly more
  TKMPlayerAI = class
  private
    fOwner: TPlayerIndex;
    fGoals: TKMGoals;
    fSetup: TKMPlayerAISetup;
    fMayor: TKMayor;
    fGeneral: TKMGeneral;

    fWonOrLost: TWonOrLost; //Has this player won/lost? If so, do not check goals

    procedure CheckGoals;
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;

    property Setup: TKMPlayerAISetup read fSetup;
    property Mayor: TKMayor read fMayor;
    property General: TKMGeneral read fGeneral;
    property Goals: TKMGoals read fGoals;

    procedure Defeat; //Defeat the player, this is not reversible
    procedure Victory; //Set this player as victorious, this is not reversible
    procedure AddDefaultGoals(aBuildings: Boolean);
    property WonOrLost: TWonOrLost read fWonOrLost;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);
    procedure HouseAttackNotification(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
    procedure UnitAttackNotification(aUnit: TKMUnit; aAttacker: TKMUnitWarrior);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses
  KM_Game, KM_PlayersCollection, KM_Player, KM_PlayerStats,
  KM_ResSound, KM_Scripting, KM_ResHouses;


{ TKMPlayerAI }
constructor TKMPlayerAI.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;

  fOwner := aPlayerIndex;
  fSetup := TKMPlayerAISetup.Create;
  fMayor := TKMayor.Create(fOwner, fSetup);
  fGeneral := TKMGeneral.Create(fOwner, fSetup);
  fGoals := TKMGoals.Create;
  fWonOrLost := wol_None;
end;


destructor TKMPlayerAI.Destroy;
begin
  fGoals.Free;
  fGeneral.Free;
  fMayor.Free;
  fSetup.Free;

  inherited;
end;


//Defeat Player (from scripting?), this is not reversible.
//Defeated player remains in place, but does no actions
procedure TKMPlayerAI.Defeat;
begin
  if fWonOrLost = wol_None then
  begin
    fWonOrLost := wol_Lost;

    //Let the game know
    fGame.PlayerDefeat(fOwner);

    //Script may have additional event processors
    fScripting.ProcPlayerDefeated(fOwner);
  end;
end;


//Set player to victorious (from scripting), this is not reversible.
//You probably need to make sure all other players are defeated or victorious too
//otherwise it will look odd.
procedure TKMPlayerAI.Victory;
begin
  if fWonOrLost = wol_None then
  begin
    fWonOrLost := wol_Won;

    //Let everyone know in MP mode
    if not fGame.IsReplay and (fGame.IsMultiplayer or (MySpectator.PlayerIndex = fOwner)) then
      fGame.PlayerVictory(fOwner);

    //Script may have additional event processors
    fScripting.ProcPlayerVictory(fOwner);
  end;
end;


procedure TKMPlayerAI.AddDefaultGoals(aBuildings: Boolean);
var
  I: Integer;
  Enemies: array of TPlayerIndex;
begin
  SetLength(Enemies, 0);
  for I := 0 to gPlayers.Count - 1 do
    if (I <> fOwner) and (gPlayers[fOwner].Alliances[I] = at_Enemy) then
    begin
      SetLength(Enemies, Length(Enemies)+1);
      Enemies[Length(Enemies)-1] := I;
    end;
  Goals.AddDefaultGoals(aBuildings, fOwner, Enemies);
end;


procedure TKMPlayerAI.CheckGoals;

  function GoalConditionSatisfied(aGoal: TKMGoal): Boolean;
  var Stat: TKMPlayerStats;
  begin
    Assert((aGoal.GoalCondition = gc_Time) or (aGoal.PlayerIndex <> -1), 'Only gc_Time can have nil Player');

    Result := False;
    if aGoal.PlayerIndex <> -1 then
      Stat := gPlayers[aGoal.PlayerIndex].Stats
    else
      Stat := nil;

    case aGoal.GoalCondition of
      gc_BuildTutorial:     Result := True; //Deprecated
      //gc_Time is disabled as we process messages in Event system now. Return true so players
      //do not have to wait for all messages to show before they are allowed to win (same in TPR)
      gc_Time:              Result := True; //Deprecated
      gc_Buildings:         Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Barracks]) > 0);
      gc_Troops:            Result := (Stat.GetArmyCount > 0);
      gc_MilitaryAssets:    Result := (Stat.GetArmyCount > 0) or
                                      (Stat.GetHouseQty([ht_Barracks, ht_CoalMine, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_Stables,
                                                         ht_IronMine, ht_IronSmithy ,ht_WeaponSmithy, ht_ArmorSmithy, ht_TownHall,
                                                         ht_SiegeWorkshop]) > 0);
      gc_SerfsAndSchools:   Result := (Stat.GetHouseQty([ht_School]) > 0) or (Stat.GetUnitQty(ut_Serf) > 0);
      gc_EconomyBuildings:  Result := (Stat.GetHouseQty([ht_Store, ht_School, ht_Inn]) > 0);
      else                  Assert(False, 'Unknown goal');
    end;
    if aGoal.GoalStatus = gs_False then
      Result := not Result; //Reverse condition
  end;

var
  I: Integer;
  HasVictoryGoal: Boolean;
  VictorySatisfied, SurvivalSatisfied: Boolean;
begin
  //If player has elected to play on past victory or defeat
  //then do not check for any further goals
  if fWonOrLost <> wol_None then Exit;

  //Assume they will win/survive, then prove it with goals
  HasVictoryGoal := False;
  VictorySatisfied  := True;
  SurvivalSatisfied := True;

  with gPlayers[fOwner] do
  for I := 0 to Goals.Count - 1 do
  case Goals[I].GoalType of
    glt_Victory:  begin
                    //In a sandbox or script-ruled mission there may be no victory conditions in Goals
                    //so we make sure player wins by Goals only if he has such goals
                    HasVictoryGoal := True;
                    VictorySatisfied := VictorySatisfied and GoalConditionSatisfied(Goals[I]);
                  end;
    glt_Survive:  SurvivalSatisfied := SurvivalSatisfied and GoalConditionSatisfied(Goals[I]);
  end;
      //Messages in goals have been replaced by SCRIPT files, so this code is disabled now,
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

  //You can't win and lose at the same time. In KaM defeats override victories, except
  //when there are no goals defined, in which case you win for some weird reason...
  //But given that having no goals is pretty pointless we'll make defeat override so you can't
  //win battle missions by waiting for your troops to simultainiously starve to death.

  //Now we know if player has been defeated or won
  if not SurvivalSatisfied then
    Defeat
  else
    if HasVictoryGoal and VictorySatisfied then
      Victory;
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
  case gPlayers[fOwner].PlayerType of
    pt_Human:
      begin
        //No fight alerts in replays, and only show alerts for ourselves
        if (not fGame.IsReplay) and (fOwner = MySpectator.PlayerIndex) then
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
  case gPlayers[fOwner].PlayerType of
    pt_Human:
      //No fight alerts in replays, and only show alerts for ourselves
      if not fGame.IsReplay and (fOwner = MySpectator.PlayerIndex) then
        fGame.Alerts.AddFight(aUnit.PositionF, fOwner, NotifyKind[aUnit is TKMUnitWarrior]);
    pt_Computer:
      begin
        //If we are attacked, then we should counter attack the attacker!
        if aUnit is TKMUnitWarrior then
        begin
          Group := gPlayers[fOwner].UnitGroups.GetGroupByMember(TKMUnitWarrior(aUnit));
          //It's ok for the group to be nil, the warrior could still be walking out of the barracks
          if (Group <> nil) and not Group.IsDead then
            //If we are already in the process of attacking something, don't change our minds,
            //otherwise you can make a unit walk backwards and forwards forever between two groups of archers
            if not Group.InFight then
              //Make sure the group could possibly reach the offenders
              if Group.CanWalkTo(aAttacker.GetPosition, Group.FightMaxRange) then
                Group.OrderAttackUnit(aAttacker, True);
        end;
        fGeneral.RetaliateAgainstThreat(aAttacker); //Nearby soldiers should come to assist
      end;
  end;
end;


procedure TKMPlayerAI.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('PlayerAI');
  SaveStream.Write(fOwner);
  SaveStream.Write(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Save(SaveStream);
  fGeneral.Save(SaveStream);
  fMayor.Save(SaveStream);
  fGoals.Save(SaveStream);
end;


procedure TKMPlayerAI.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('PlayerAI');
  LoadStream.Read(fOwner);
  LoadStream.Read(fWonOrLost, SizeOf(fWonOrLost));

  fSetup.Load(LoadStream);
  fGeneral.Load(LoadStream);
  fMayor.Load(LoadStream);
  fGoals.Load(LoadStream);
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
  //Check goals for all players to maintain multiplayer consistency
  //AI victory/defeat is used in scripts (e.g. OnPlayerDefeated in battle tutorial)
  if (aTick + Byte(fOwner)) mod MAX_PLAYERS = 0 then
    CheckGoals; //This procedure manages victory and loss

  case gPlayers[fOwner].PlayerType of
    pt_Human:     begin
                    //Humans dont need Mayor and Army management
                  end;
    pt_Computer:  begin
                    fMayor.UpdateState(aTick);
                    fGeneral.UpdateState(aTick);
                  end;
  end;
end;


end.
