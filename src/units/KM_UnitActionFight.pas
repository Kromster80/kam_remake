unit KM_UnitActionFight;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_CommonUtils, KromUtils, Math, SysUtils, KM_Units, KM_Points;


//Fight until we die or the opponent dies
type
  TUnitActionFight = class(TUnitAction)
  private
    fFightDelay: Integer; //Pause for this many ticks before going onto the next Step
    fOpponent: TKMUnit; //Who we are fighting with
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying

    //Execute is broken up into multiple methods
      function ExecuteValidateOpponent(Step:byte): TActionResult;
      function ExecuteProcessRanged(Step:byte):boolean;
      function ExecuteProcessMelee(Step:byte):boolean;

    function UpdateVertexUsage(aFrom, aTo: TKMPoint):boolean;
    procedure IncVertex(aFrom, aTo: TKMPoint);
    procedure DecVertex;
    procedure MakeSound(IsHit:boolean);
  public
    constructor Create(aUnit: TKMUnit; aActionType: TUnitActionType; aOpponent: TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    procedure SyncLoad; override;
    property GetOpponent: TKMUnit read fOpponent;
    function Execute: TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_ResSound, KM_Sound, KM_Units_Warrior, KM_Resource, KM_Projectiles,
  KM_ResUnits, KM_Hand;


const
  STRIKE_STEP = 5; //Melee units place hit on step 5

  MeleeSoundsHit: array [0..14] of TSoundFX = (
    sfx_Melee34, sfx_Melee35, sfx_Melee36, sfx_Melee41, sfx_Melee42,
    sfx_Melee44, sfx_Melee45, sfx_Melee46, sfx_Melee47, sfx_Melee48,
    sfx_Melee49, sfx_Melee50, sfx_Melee55, sfx_Melee56, sfx_Melee57);

  MeleeSoundsMiss: array [0..8] of TSoundFX = (
    sfx_Melee37, sfx_Melee38, sfx_Melee39,
    sfx_Melee40, sfx_Melee43, sfx_Melee51,
    sfx_Melee52, sfx_Melee53, sfx_Melee54);


{ TUnitActionFight }
constructor TUnitActionFight.Create(aUnit: TKMUnit; aActionType: TUnitActionType; aOpponent: TKMUnit);
begin
  inherited Create(aUnit, aActionType, True);
  fFightDelay     := -1;
  fOpponent       := aOpponent.GetUnitPointer;
  aUnit.Direction := KMGetDirection(fUnit.PositionF, fOpponent.PositionF); //Face the opponent from the beginning
  fVertexOccupied := KMPOINT_ZERO;
  if KMStepIsDiag(fUnit.GetPosition, fOpponent.GetPosition) and not TKMUnitWarrior(fUnit).IsRanged then
    IncVertex(fUnit.GetPosition, fOpponent.GetPosition);
end;


destructor TUnitActionFight.Destroy;
begin
  gHands.CleanUpUnitPointer(fOpponent);
  if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
    DecVertex;
  inherited;
end;


constructor TUnitActionFight.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fOpponent, 4);
  LoadStream.Read(fFightDelay);
  LoadStream.Read(fVertexOccupied);
end;


procedure TUnitActionFight.SyncLoad;
begin
  inherited;
  fOpponent := gHands.GetUnitByUID(cardinal(fOpponent));
end;


function TUnitActionFight.ActName: TUnitActionName;
begin
  Result := uan_Fight;
end;


function TUnitActionFight.GetExplanation: UnicodeString;
begin
  Result := 'Fighting';
end;


function TUnitActionFight.UpdateVertexUsage(aFrom, aTo: TKMPoint):boolean;
begin
  Result := true;
  if KMStepIsDiag(aFrom, aTo) then
  begin
    //If the new target has the same vertex as the old one, no change is needed
    if KMSamePoint(KMGetDiagVertex(aFrom, aTo), fVertexOccupied) then Exit;
    //Otherwise the new target's vertex is different to the old one, so remove old vertex usage and add new
    DecVertex;
    if fUnit.VertexUsageCompatible(aFrom, aTo) then
      IncVertex(aFrom, aTo)
    else
      Result := false; //This vertex is being used so we can't fight
  end
  else
    //The new target is not diagonal, make sure any old vertex usage is removed
    DecVertex;
end;


procedure TUnitActionFight.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell gTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'Fight vertex in use');

  fUnit.VertexAdd(aFrom, aTo);
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionFight.DecVertex;
begin
  //Tell gTerrain that this vertex is not being used anymore
  if KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then exit;

  fUnit.VertexRem(fVertexOccupied);
  fVertexOccupied := KMPOINT_ZERO;
end;


procedure TUnitActionFight.MakeSound(IsHit:boolean);
var
  //Battlecry is the most noticable random sound, we would like to repeat it exactly the same in each replay (?)
  MakeBattleCry: Boolean;
begin
  //Randomly make a battle cry. KaMRandom must always happen regardless of tile revelation
  MakeBattleCry := KaMRandom(20) = 0;

  //Do not play sounds if unit is invisible to gMySpectator
  //We should not use KaMRandom below this line because sound playback depends on FOW and is individual for each player
  if gMySpectator.FogOfWar.CheckTileRevelation(fUnit.GetPosition.X, fUnit.GetPosition.Y) < 255 then Exit;

  if MakeBattleCry then
    gSoundPlayer.PlayWarrior(fUnit.UnitType, sp_BattleCry, fUnit.PositionF);

  case fUnit.UnitType of
    ut_Arbaletman: gSoundPlayer.Play(sfx_CrossbowDraw, fUnit.PositionF); // Aiming
    ut_Bowman:     gSoundPlayer.Play(sfx_BowDraw,      fUnit.PositionF); // Aiming
    ut_Slingshot:  gSoundPlayer.Play(sfx_SlingerShoot, fUnit.PositionF);
    else           begin
                     if IsHit then
                       gSoundPlayer.Play(MeleeSoundsHit[Random(Length(MeleeSoundsHit))], fUnit.PositionF)
                     else
                       gSoundPlayer.Play(MeleeSoundsMiss[Random(Length(MeleeSoundsMiss))], fUnit.PositionF);
                   end;
  end;
end;


function TUnitActionFight.ExecuteValidateOpponent(Step: Byte): TActionResult;
begin
  Result := ar_ActContinues;
  //See if Opponent has walked away (i.e. Serf) or died
  if fOpponent.IsDeadOrDying //Don't continue to fight dead units
  or not fOpponent.Visible //Don't continue to fight units that have went into a house
  or not TKMUnitWarrior(fUnit).WithinFightRange(fOpponent.GetPosition)
  or not fUnit.CanWalkDiagonaly(fUnit.GetPosition, fOpponent.GetPosition) then //Might be a tree between us now
  begin
    //After killing an opponent there is a very high chance that there is another enemy to be fought immediately
    //Try to start fighting that enemy by reusing this FightAction, rather than destroying it and making a new one
    Locked := False; //Fight can be interrupted by FindEnemy, otherwise it will always return nil!
    gHands.CleanUpUnitPointer(fOpponent); //We are finished with the old opponent
    fOpponent := TKMUnitWarrior(fUnit).FindEnemy; //Find a new opponent
    if fOpponent <> nil then
    begin
      //Start fighting this opponent by resetting the action
      fOpponent.GetUnitPointer; //Add to pointer count
      TKMUnitWarrior(fUnit).OnPickedFight(TKMUnitWarrior(fUnit), fOpponent);
      Locked := true;
      fFightDelay := -1;
      //Ranged units should turn to face the new opponent immediately
      if TKMUnitWarrior(fUnit).IsRanged then
        fUnit.Direction := KMGetDirection(fUnit.PositionF, fOpponent.PositionF)
      else
        //Melee: If we haven't yet placed our strike, reset the animation step
        //Otherwise finish this strike then we can face the new opponent automatically
        if Step <= STRIKE_STEP then
          fUnit.AnimStep := 0; //Rest fight animation/sequence
    end
    else
    begin
      //No one else to fight, so we exit
      Result := ar_ActDone;
    end;
  end;
end;


//A result of true means exit from Execute
function TUnitActionFight.ExecuteProcessRanged(Step: Byte): Boolean;
begin
  Result := False;

  if Step = FIRING_DELAY then
  begin
    if fFightDelay = -1 then //Initialize
    begin
      if fUnit.UnitType <> ut_Slingshot then MakeSound(False);
      if fUnit.UnitType = ut_Arbaletman then
        fFightDelay := CROSSBOWMEN_AIMING_DELAY_MIN + KaMRandom(CROSSBOWMEN_AIMING_DELAY_ADD)
      else
        fFightDelay := BOWMEN_AIMING_DELAY_MIN + KaMRandom(BOWMEN_AIMING_DELAY_ADD);
    end;

    if fFightDelay > 0 then
    begin
      Dec(fFightDelay);
      Result := True; //do not increment AnimStep, just exit;
      Exit;
    end;

    if fUnit.UnitType = ut_Slingshot then MakeSound(False);
    TKMUnitWarrior(fUnit).SetLastShootTime; //Record last time the warrior shot

    //Fire the arrow
    case fUnit.UnitType of
      ut_Arbaletman:  gProjectiles.AimTarget(fUnit.PositionF, fOpponent, pt_Bolt, fUnit, RANGE_ARBALETMAN_MAX, RANGE_ARBALETMAN_MIN);
      ut_Bowman:      gProjectiles.AimTarget(fUnit.PositionF, fOpponent, pt_Arrow, fUnit, RANGE_BOWMAN_MAX, RANGE_BOWMAN_MIN);
      ut_Slingshot:   ;
      else            raise Exception.Create('Unknown shooter');
    end;

    fFightDelay := -1; //Reset
  end;

  //todo: @Lewin: Looks like this can be adjoined with above?
  if Step = SLINGSHOT_FIRING_DELAY then
    if fUnit.UnitType = ut_Slingshot then
    begin
      gProjectiles.AimTarget(fUnit.PositionF, fOpponent, pt_SlingRock, fUnit, RANGE_SLINGSHOT_MAX, RANGE_SLINGSHOT_MIN);
      TKMUnitWarrior(fUnit).SetLastShootTime; //Record last time the warrior shot
    end;
end;


//A result of true means exit from Execute
function TUnitActionFight.ExecuteProcessMelee(Step: Byte): Boolean;
var
  IsHit: Boolean;
  Damage: Word;
begin
  Result := False;

  if Step = 1 then
  begin
    //Tell the Opponent we are attacking him
    gHands[fOpponent.Owner].AI.UnitAttackNotification(fOpponent, TKMUnitWarrior(fUnit));

    //Tell our AI that we are in a battle and might need assistance! (only for melee battles against warriors)
    if (fOpponent is TKMUnitWarrior) and not TKMUnitWarrior(fUnit).IsRanged then
      gHands[fUnit.Owner].AI.UnitAttackNotification(fUnit, TKMUnitWarrior(fOpponent), False);
  end;

  //Melee units place hit on this step
  if Step = STRIKE_STEP then
  begin
    //Base damage is the unit attack strength + AttackHorse if the enemy is mounted
    Damage := gRes.Units[fUnit.UnitType].Attack;
    if (fOpponent.UnitType in [low(UnitGroups) .. high(UnitGroups)]) and (UnitGroups[fOpponent.UnitType] = gt_Mounted) then
      Damage := Damage + gRes.Units[fUnit.UnitType].AttackHorse;

    Damage := Damage * (GetDirModifier(fUnit.Direction,fOpponent.Direction)+1); //Direction modifier
    //Defence modifier
    Damage := Damage div Math.max(gRes.Units[fOpponent.UnitType].Defence, 1); //Not needed, but animals have 0 defence

    IsHit := (Damage >= KaMRandom(101)); //Damage is a % chance to hit
    if IsHit then
      fOpponent.HitPointsDecrease(1, fUnit);

    MakeSound(IsHit); //Different sounds for hit and for miss
  end;

  //In KaM melee units pause for 1 tick on Steps [0,3,6]. Made it random so troops are not striking in sync,
  //plus it adds randomness to battles
  if Step in [0,3,6] then
  begin
    if fFightDelay = -1 then //Initialize
      fFightDelay := KaMRandom(2);

    if fFightDelay > 0 then
    begin
      dec(fFightDelay);
      Result := True; //Means exit from Execute
      Exit;
    end;

    fFightDelay := -1; //Reset
  end;
end;


function TUnitActionFight.Execute: TActionResult;
var
  Cycle, Step: Byte;
begin
  Cycle := max(gRes.Units[fUnit.UnitType].UnitAnim[ActionType, fUnit.Direction].Count, 1);
  Step  := fUnit.AnimStep mod Cycle;

  Result := ExecuteValidateOpponent(Step);
  if Result = ar_ActDone then Exit;
  Step := fUnit.AnimStep mod Cycle; //Can be changed by ExecuteValidateOpponent, so recalculate it

  //Opponent can walk next to us, keep facing him
  if Step = 0 then //Only change direction between strikes, otherwise it looks odd
    fUnit.Direction := KMGetDirection(fUnit.PositionF, fOpponent.PositionF);

  //If the vertex usage has changed we should update it
  if not TKMUnitWarrior(fUnit).IsRanged then //Ranged units do not use verticies
    if not UpdateVertexUsage(fUnit.GetPosition, fOpponent.GetPosition) then
    begin
      //The vertex is being used so we can't fight
      Result := ar_ActDone;
      Exit;
    end;

  if TKMUnitWarrior(fUnit).IsRanged then
  begin
    if ExecuteProcessRanged(Step) then
      Exit;
  end
  else
    if ExecuteProcessMelee(Step) then
      Exit;

  //Aiming Archers and pausing melee may miss a few ticks, (exited above) so don't put anything critical below!

  StepDone := (fUnit.AnimStep mod Cycle = 0) or TKMUnitWarrior(fUnit).IsRanged; //Archers may abandon at any time as they need to walk off imediantly
  Inc(fUnit.AnimStep);
end;


procedure TUnitActionFight.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  if fOpponent <> nil then
    SaveStream.Write(fOpponent.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fFightDelay);
  SaveStream.Write(fVertexOccupied);
end;




end.
