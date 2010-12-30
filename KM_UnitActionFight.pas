unit KM_UnitActionFight;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KromUtils, Math, SysUtils, KM_Units;



{Fight until we die or the opponent dies}
type
TUnitActionFight = class(TUnitAction)
  private
    AimingDelay:integer;
    fOpponent:TKMUnit; //Who we are fighting with
  public
    constructor Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad(); override;
    procedure MakeSound(KMUnit: TKMUnit; IsHit:boolean);
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Sound, KM_Units_Warrior, KM_Game;


{ TUnitActionFight }
constructor TUnitActionFight.Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
begin
  Inherited Create(aActionType);
  fActionName     := uan_Fight;
  Locked          := true;
  AimingDelay     := -1;
  fOpponent       := aOpponent.GetUnitPointer;
  aUnit.Direction := KMGetDirection(aUnit.GetPosition, fOpponent.GetPosition); //Face the opponent from the beginning
end;


destructor TUnitActionFight.Destroy;
begin
  fPlayers.CleanUpUnitPointer(fOpponent);
  Inherited;
end;


constructor TUnitActionFight.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fOpponent, 4);
  LoadStream.Read(AimingDelay);
end;


procedure TUnitActionFight.SyncLoad();
begin
  Inherited;
  fOpponent := fPlayers.GetUnitByID(cardinal(fOpponent));
end;


procedure TUnitActionFight.MakeSound(KMUnit: TKMUnit; IsHit:boolean);
begin
  //Do not play sounds if unit is invisible to MyPlayer
  if fTerrain.CheckTileRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y, MyPlayer.PlayerID) < 255 then exit;
  
  case KMUnit.UnitType of
    ut_Arbaletman: fSoundLib.Play(sfx_CrossbowDraw,KMUnit.GetPosition); //Aiming
    ut_Bowman:     fSoundLib.Play(sfx_BowDraw,KMUnit.GetPosition); //Aiming
    else           begin
                     {if IsHit then
                       fSoundLib.Play(sfx_BowShoot,KMUnit.GetPosition,true)
                     else
                       fSoundLib.Play(sfx_BowShoot,KMUnit.GetPosition,true);}
                   end;
  end;
end;


function TUnitActionFight.Execute(KMUnit: TKMUnit):TActionResult;
var Cycle,Step:byte; IsHit: boolean; Damage: word; ut,ot:byte;
begin
  Result := ActContinues; //Continue action by default, if there is no one to fight then exit
  //See if Opponent has walked away (i.e. Serf) or died
  if (fOpponent.IsDeadOrDying) or not InRange(GetLength(KMUnit.GetPosition, fOpponent.GetPosition), TKMUnitWarrior(KMUnit).GetFightMinRange, TKMUnitWarrior(KMUnit).GetFightMaxRange) then
  begin
    //After killing an opponent there is a very high chance that there is another enemy to be fought immediately
    //Try to start fighting that enemy by reusing this FightAction, rather than destorying it and making a new one
    Locked := false; //Fight can be interrupted by FindEnemy, otherwise it will always return nil!
    fOpponent.ReleaseUnitPointer; //We are finished with the old opponent
    fOpponent := TKMUnitWarrior(KMUnit).FindEnemy; //Find a new opponent
    if fOpponent <> nil then
    begin
      //Start fighting this opponent by resetting the action
      fOpponent.GetUnitPointer; //Add to pointer count
      Locked := true;
      AimingDelay := -1;
      KMUnit.AnimStep := 0;
      KMUnit.Direction := KMGetDirection(KMUnit.GetPosition, fOpponent.GetPosition); //Face the opponent from the beginning
    end
    else
    begin
      //No one else to fight, so we exit
      Result := ActDone;
      exit;
    end;
  end;

  Cycle := max(UnitSprite[byte(KMUnit.UnitType)].Act[byte(GetActionType)].Dir[byte(KMUnit.Direction)].Count,1);
  Step  := KMUnit.AnimStep mod Cycle;

  //Opponent can walk next to us, keep facing him
  KMUnit.Direction := KMGetDirection(KMUnit.GetPosition, fOpponent.GetPosition);

  if TKMUnitWarrior(KMUnit).GetFightMaxRange >= 2 then begin
    if Step = FIRING_DELAY then
    begin
      if AimingDelay=-1 then //Initialize
      begin
        MakeSound(KMUnit, false); //IsHit means IsShoot for bowmen (false means aiming)
        AimingDelay := AIMING_DELAY_MIN+Random(AIMING_DELAY_ADD);
      end;

      if AimingDelay>0 then begin
        dec(AimingDelay);
        exit; //do not increment AnimStep, just exit;
      end;

      case KMUnit.UnitType of
        ut_Arbaletman: fGame.fProjectiles.AddItem(KMUnit.PositionF, fOpponent.PositionF, pt_Bolt, KMUnit.GetOwner, true);
        ut_Bowman:     fGame.fProjectiles.AddItem(KMUnit.PositionF, fOpponent.PositionF, pt_Arrow, KMUnit.GetOwner, true);
        else Assert(false, 'Unknown shooter');
      end;

      AimingDelay := -1; //Reset

    end;
  end else begin
    //Melee units place hit on step 5
    if Step = 5 then
    begin
      ut := byte(KMUnit.UnitType);
      ot := byte(fOpponent.UnitType);
      Damage := UnitStat[ut].Attack; //Base damage
      if InRange(ot, low(UnitGroups), high(UnitGroups)) then
        Damage := Damage + UnitStat[ut].AttackHorseBonus * byte(UnitGroups[ot] = gt_Mounted); //Add Anti-horse bonus
      Damage := Damage * (GetDirModifier(KMUnit.Direction,fOpponent.Direction)+1); //Direction modifier
      Damage := Damage div max(UnitStat[ot].Defence,1); //Not needed, but animals have 0 defence

      IsHit := (Damage >= Random(101)); //0..100

      if IsHit then
        if fOpponent.HitPointsDecrease(1) then
          if (fPlayers <> nil) and (fPlayers.Player[byte(KMUnit.GetOwner)] <> nil) then
            fPlayers.Player[byte(KMUnit.GetOwner)].fPlayerStats.UnitKilled(fOpponent.UnitType);

      MakeSound(KMUnit, IsHit); //2 sounds for hit and for miss
    end;
  end;

  //Aiming Archers may miss few ticks, so don't put anything critical below!

  StepDone := (KMUnit.AnimStep mod Cycle = 0) or (TKMUnitWarrior(KMUnit).GetFightMaxRange >= 2); //Archers may abandon at any time as they need to walk off imediantly
  inc(KMUnit.AnimStep);

  //If our group (commander) does not have a foe, set it to our opponent
  if (fOpponent is TKMUnitWarrior)
     and not (fOpponent.IsDeadOrDying)
     and((TKMUnitWarrior(KMUnit).GetFightMaxRange >= 2) or (GetLength(KMUnit.GetPosition, fOpponent.GetPosition) < 1.5))
     and (TKMUnitWarrior(KMUnit).GetCommander.Foe = nil) then
    TKMUnitWarrior(KMUnit).GetCommander.Foe := TKMUnitWarrior(fOpponent).GetCommander;
end;


procedure TUnitActionFight.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fOpponent <> nil then
    SaveStream.Write(fOpponent.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(AimingDelay);
end;




end.
