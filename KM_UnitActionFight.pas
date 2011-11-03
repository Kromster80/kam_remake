unit KM_UnitActionFight;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KromUtils, Math, SysUtils, KM_Units, KM_Points;

{Fight until we die or the opponent dies}
type
TUnitActionFight = class(TUnitAction)
  private
    fFightDelay:integer; //Pause for this many ticks before going onto the next Step
    fOpponent:TKMUnit; //Who we are fighting with
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying

    //Execute is broken up into multiple methods
      function ExecuteValidateOpponent(KMUnit: TKMUnit):TActionResult;
      function ExecuteProcessRanged(KMUnit: TKMUnit; Step:byte):boolean;
      function ExecuteProcessMelee(KMUnit: TKMUnit; Step:byte):boolean;

    function UpdateVertexUsage(aFrom, aTo: TKMPoint):boolean;
    procedure IncVertex(aFrom, aTo: TKMPoint);
    procedure DecVertex;
    procedure MakeSound(KMUnit: TKMUnit; IsHit:boolean);
  public
    constructor Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    function GetExplanation:string; override;
    procedure SyncLoad; override;
    property GetOpponent: TKMUnit read fOpponent;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Sound, KM_Units_Warrior, KM_Game, KM_ResourceGFX;


{ TUnitActionFight }
constructor TUnitActionFight.Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
begin
  Inherited Create(aActionType);
  fActionName     := uan_Fight;
  Locked          := true;
  fFightDelay      := -1;
  fOpponent       := aOpponent.GetUnitPointer;
  aUnit.Direction := KMGetDirection(aUnit.GetPosition, fOpponent.GetPosition); //Face the opponent from the beginning
  fVertexOccupied := KMPoint(0,0);
  if KMStepIsDiag(aUnit.GetPosition, fOpponent.GetPosition) and not TKMUnitWarrior(aUnit).IsRanged then
    IncVertex(aUnit.GetPosition, fOpponent.GetPosition);
end;


destructor TUnitActionFight.Destroy;
begin
  fPlayers.CleanUpUnitPointer(fOpponent);
  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
    DecVertex;
  Inherited;
end;


constructor TUnitActionFight.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fOpponent, 4);
  LoadStream.Read(fFightDelay);
  LoadStream.Read(fVertexOccupied);
end;


procedure TUnitActionFight.SyncLoad;
begin
  Inherited;
  fOpponent := fPlayers.GetUnitByID(cardinal(fOpponent));
end;


function TUnitActionFight.GetExplanation: string;
begin
  Result := 'Fighting';
end;


function TUnitActionFight.UpdateVertexUsage(aFrom, aTo: TKMPoint):boolean;
begin
  Result := true;
  //If the vertex usage has changed we should update it
  if not KMSamePoint(KMGetDiagVertex(aFrom, aTo), fVertexOccupied) then
  begin
    DecVertex;
    if KMStepIsDiag(aFrom, aTo) then
    begin
      if fTerrain.VertexUsageCompatible(aFrom, aTo) then
        IncVertex(aFrom, aTo)
      else
        //This vertex is being used so we can't fight
        Result := false;
    end;
  end;
end;


procedure TUnitActionFight.IncVertex(aFrom, aTo: TKMPoint);
begin
  //Tell fTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPoint(0,0)), 'Fight vertex in use');

  fTerrain.UnitVertexAdd(aFrom,aTo);
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TUnitActionFight.DecVertex;
begin
  //Tell fTerrain that this vertex is not being used anymore
  if KMSamePoint(fVertexOccupied, KMPoint(0,0)) then exit;

  fTerrain.UnitVertexRem(fVertexOccupied);
  fVertexOccupied := KMPoint(0,0);
end;


procedure TUnitActionFight.MakeSound(KMUnit: TKMUnit; IsHit:boolean);
begin
  //Randomly make a battle cry
  if KaMRandom(20) = 0 then
    fSoundLib.PlayWarrior(KMUnit.UnitType, sp_BattleCry, KMUnit.PositionF);

  //Do not play sounds if unit is invisible to MyPlayer
  //We should not use KaMRandom below this line because sound playback depends on FOW and is individual for each player
  if MyPlayer.FogOfWar.CheckTileRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y) < 255 then exit;
  
  case KMUnit.UnitType of
    ut_Arbaletman: fSoundLib.Play(sfx_CrossbowDraw, KMUnit.PositionF); //Aiming
    ut_Bowman:     fSoundLib.Play(sfx_BowDraw,      KMUnit.PositionF); //Aiming
    ut_Slingshot:  fSoundLib.Play(sfx_SlingerShoot, KMUnit.PositionF);
    else           begin
                     if IsHit then
                       fSoundLib.Play(MeleeSoundsHit[Random(Length(MeleeSoundsHit))], KMUnit.PositionF)
                     else
                       fSoundLib.Play(MeleeSoundsMiss[Random(Length(MeleeSoundsMiss))], KMUnit.PositionF);
                   end;
  end;
end;


function TUnitActionFight.ExecuteValidateOpponent(KMUnit: TKMUnit):TActionResult;
begin
  Result := ActContinues;
  //See if Opponent has walked away (i.e. Serf) or died
  if (fOpponent.IsDeadOrDying) or (not fOpponent.Visible) //Don't continue to fight dead units in units that have gone into a house
  or not InRange(GetLength(KMUnit.GetPosition, fOpponent.GetPosition), TKMUnitWarrior(KMUnit).GetFightMinRange, TKMUnitWarrior(KMUnit).GetFightMaxRange)
  or not fTerrain.CanWalkDiagonaly(KMUnit.GetPosition, fOpponent.GetPosition) then //Might be a tree between us now
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
      fFightDelay := -1;
      //Do not face the new opponent or reset the animation step, wait until this strike is over
    end
    else
    begin
      //Tell commanders to reposition after a fight, if we don't have other plans (order)
      if TKMUnitWarrior(KMUnit).IsCommander and not TKMUnitWarrior(KMUnit).ArmyInFight and
         (TKMUnitWarrior(KMUnit).GetOrder = wo_None) and (KMUnit.GetUnitTask = nil) then
        TKMUnitWarrior(KMUnit).OrderWalk(KMUnit.GetPosition); //Don't use halt because that returns us to fOrderLoc
      //No one else to fight, so we exit
      Result := ActDone;
    end;
  end;
end;


//A result of true means exit from Execute
function TUnitActionFight.ExecuteProcessRanged(KMUnit: TKMUnit; Step:byte):boolean;
begin
  Result := false;
  if Step = FIRING_DELAY then
  begin
    if fFightDelay=-1 then //Initialize
    begin
      if KMUnit.UnitType <> ut_Slingshot then MakeSound(KMUnit, false);
      fFightDelay := AIMING_DELAY_MIN+KaMRandom(AIMING_DELAY_ADD);
    end;

    if fFightDelay>0 then begin
      dec(fFightDelay);
      Result := true; //do not increment AnimStep, just exit;
      exit;
    end;
    if KMUnit.UnitType = ut_Slingshot then MakeSound(KMUnit, false);

    //Fire the arrow
    case KMUnit.UnitType of
      ut_Arbaletman: fGame.Projectiles.AimTarget(KMUnit.PositionF, fOpponent, pt_Bolt, KMUnit.GetOwner, RANGE_ARBALETMAN_MAX, RANGE_ARBALETMAN_MIN);
      ut_Bowman:     fGame.Projectiles.AimTarget(KMUnit.PositionF, fOpponent, pt_Arrow, KMUnit.GetOwner, RANGE_BOWMAN_MAX, RANGE_BOWMAN_MIN);
      ut_Slingshot:  ;
      else Assert(false, 'Unknown shooter');
    end;

    fFightDelay := -1; //Reset
  end;
  if Step = SLINGSHOT_FIRING_DELAY then
    if KMUnit.UnitType = ut_Slingshot then
      fGame.Projectiles.AimTarget(KMUnit.PositionF, fOpponent, pt_SlingRock, KMUnit.GetOwner, RANGE_SLINGSHOT_MAX, RANGE_SLINGSHOT_MIN);
end;


//A result of true means exit from Execute
function TUnitActionFight.ExecuteProcessMelee(KMUnit: TKMUnit; Step:byte):boolean;
var IsHit: boolean; Damage: word;
begin
  Result := false;
  //Melee units place hit on step 5
  if Step = 5 then
  begin
    //Base damage is the unit attack strength + AttackHorse if the enemy is mounted
    Damage := fResource.UnitDat[KMUnit.UnitType].Attack;
    if (fOpponent.UnitType in [low(UnitGroups) .. high(UnitGroups)]) and (UnitGroups[fOpponent.UnitType] = gt_Mounted) then
      Damage := Damage + fResource.UnitDat[KMUnit.UnitType].AttackHorse;

    Damage := Damage * (GetDirModifier(KMUnit.Direction,fOpponent.Direction)+1); //Direction modifier
    //Defence modifier
    Damage := Damage div Math.max(fResource.UnitDat[fOpponent.UnitType].Defence, 1); //Not needed, but animals have 0 defence

    IsHit := (Damage >= KaMRandom(101)); //Damage is a % chance to hit
    if IsHit then
      if fOpponent.HitPointsDecrease(1) then
        fPlayers.Player[KMUnit.GetOwner].Stats.UnitKilled(fOpponent.UnitType);

    MakeSound(KMUnit, IsHit); //Different sounds for hit and for miss
  end;

  //In KaM melee units pause for 1 tick on Steps [0,3,6]. Made it random so troops are not striking in sync,
  //plus it adds randomness to battles
  if Step in [0,3,6] then
  begin
    if fFightDelay=-1 then //Initialize
    begin
      fFightDelay := KaMRandom(2);
    end;

    if fFightDelay>0 then begin
      dec(fFightDelay);
      Result := true; //Means exit from Execute
      exit;
    end;

    fFightDelay := -1; //Reset
  end;
end;


function TUnitActionFight.Execute(KMUnit: TKMUnit):TActionResult;
var Cycle,Step:byte;
begin
  Result := ExecuteValidateOpponent(KMUnit);
  if Result = ActDone then exit;

  Cycle := max(fResource.UnitDat[KMUnit.UnitType].UnitAnim[ActionType, KMUnit.Direction].Count, 1);
  Step  := KMUnit.AnimStep mod Cycle;

  //Opponent can walk next to us, keep facing him
  if Step = 0 then //Only change direction between strikes, otherwise it looks odd
    KMUnit.Direction := KMGetDirection(KMUnit.GetPosition, fOpponent.GetPosition);

  //If the vertex usage has changed we should update it
  if not TKMUnitWarrior(KMUnit).IsRanged then //Ranged units do not use verticies
    if not UpdateVertexUsage(KMUnit.GetPosition, fOpponent.GetPosition) then
    begin
      //The vertex is being used so we can't fight
      Result := ActDone;
      exit;
    end;

  if Step = 1 then
  begin
    //Tell the Opponent we are attacking him
    fPlayers.Player[fOpponent.GetOwner].AI.UnitAttackNotification(fOpponent, TKMUnitWarrior(KMUnit));

    //Tell our AI that we are in a battle and might need assistance! (only for melee battles against warriors)
    if (fOpponent is TKMUnitWarrior) and not TKMUnitWarrior(KMUnit).IsRanged then
      fPlayers.Player[KMUnit.GetOwner].AI.UnitAttackNotification(KMUnit, TKMUnitWarrior(fOpponent));
  end;

  if TKMUnitWarrior(KMUnit).IsRanged then
  begin
    if ExecuteProcessRanged(KMUnit, Step) then
      exit;
  end
  else
    if ExecuteProcessMelee(KMUnit, Step) then
      exit;

  //Aiming Archers and pausing melee may miss a few ticks, (exited above) so don't put anything critical below!

  StepDone := (KMUnit.AnimStep mod Cycle = 0) or TKMUnitWarrior(KMUnit).IsRanged; //Archers may abandon at any time as they need to walk off imediantly
  inc(KMUnit.AnimStep);
end;


procedure TUnitActionFight.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fOpponent <> nil then
    SaveStream.Write(fOpponent.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fFightDelay);
  SaveStream.Write(fVertexOccupied);
end;




end.
