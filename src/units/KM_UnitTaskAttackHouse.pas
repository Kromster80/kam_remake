unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_CommonUtils, KM_Houses, KM_Units, KM_Units_Warrior, KM_Points;


type
  // Attack a house
  TTaskAttackHouse = class(TUnitTask)
  private
    fHouse: TKMHouse;
    fDestroyingHouse: Boolean; //House destruction in progress
  public
    constructor Create(aWarrior: TKMUnitWarrior; aHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property DestroyingHouse: Boolean read fDestroyingHouse;
    function WalkShouldAbandon: Boolean; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_ResSound, KM_Sound, KM_Resource, KM_Projectiles, KM_Game, KM_ResUnits;


const
  MeleeSoundsHouse: array [0..12] of TSoundFX = (
    sfx_Melee37, sfx_Melee38, sfx_Melee39, sfx_Melee40, sfx_Melee41,
    sfx_Melee42, sfx_Melee43, sfx_Melee47, sfx_Melee51, sfx_Melee52,
    sfx_Melee53, sfx_Melee54, sfx_Melee57
  );


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnitWarrior; aHouse: TKMHouse);
begin
  inherited Create(aWarrior);
  fTaskName := utn_AttackHouse;
  fHouse := aHouse.GetHousePointer;
  fDestroyingHouse := False;
end;


constructor TTaskAttackHouse.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fDestroyingHouse);
end;


procedure TTaskAttackHouse.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Cardinal(fHouse));
end;


destructor TTaskAttackHouse.Destroy;
begin
  gHands.CleanUpHousePointer(fHouse);
  inherited;
end;


function TTaskAttackHouse.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed;
end;


function TTaskAttackHouse.Execute: TTaskResult;
var
   AnimLength: Integer;
   Delay, Cycle: Byte;
begin
  Result := tr_TaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := tr_TaskDone;
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
  case fPhase of
    0: if IsRanged then
         if fHouse.GetDistance(GetPosition) < GetFightMinRange then
           //Archer is too close, try to step back to the minimum range
           SetActionWalkFromHouse(fHouse, GetFightMinRange)
         else
         if fHouse.GetDistance(GetPosition) > GetFightMaxRange then
           SetActionWalkToHouse(fHouse, GetFightMaxRange)
         else
           SetActionStay(0, ua_Walk)
       else
         SetActionWalkToHouse(fHouse, 1);
    1: begin
         if IsRanged then
         begin
           //Check if the walk failed
           if (fHouse.GetDistance(GetPosition) < GetFightMinRange) or (fHouse.GetDistance(GetPosition) > GetFightMaxRange) then
           begin
             SetActionStay(0, ua_Walk);
             Result := tr_TaskDone;
             Exit;
           end;
           //Calculate base aiming delay
           if UnitType = ut_Arbaletman then
             Delay := CROSSBOWMEN_AIMING_DELAY_MIN+KaMRandom(CROSSBOWMEN_AIMING_DELAY_ADD)
           else
             Delay := BOWMEN_AIMING_DELAY_MIN+KaMRandom(BOWMEN_AIMING_DELAY_ADD);

           //Prevent rate of fire exploit by making archers pause for longer if they shot recently
           Cycle := max(gRes.Units[UnitType].UnitAnim[ua_Work, Direction].Count, 1);
           if NeedsToReload(Cycle) then
             Delay := Delay + Cycle-(gGame.GameTickCount-LastShootTime);

           SetActionLockedStay(Delay,ua_Work,true); //Pretend to aim
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.Entrance); //Look at house
           if gMySpectator.FogOfWar.CheckTileRevelation(Round(PositionF.X), Round(PositionF.Y)) >= 255 then
             case UnitType of
               ut_Arbaletman: gSoundPlayer.Play(sfx_CrossbowDraw, PositionF); //Aiming
               ut_Bowman:     gSoundPlayer.Play(sfx_BowDraw,      PositionF); //Aiming
               ut_Slingshot:  gSoundPlayer.Play(sfx_SlingerShoot, PositionF); //Aiming
               else           raise Exception.Create('Unknown shooter');
             end;
         end
         else
         begin
           //Check if the walk failed
           if fHouse.GetDistance(GetPosition) > GetFightMaxRange then
           begin
             SetActionStay(0, ua_Walk);
             Result := tr_TaskDone;
             Exit;
           end;
           SetActionLockedStay(0,ua_Work,false); //Melee units pause after the hit
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetClosestCell(GetPosition)); //Look at house
         end;
       end;
    2: begin
         fDestroyingHouse := True;
         if IsRanged then
           SetActionLockedStay(FIRING_DELAY, ua_Work, False, 0, 0) //Start shooting
         else
           SetActionLockedStay(6, ua_Work, False, 0, 0); //Start the hit
       end;
    3: begin
         if IsRanged then
         begin
           //Launch the missile and forget about it
           //Shooting range is not important now, houses don't walk (except Howl's Moving Castle perhaps)
           //todo: Slingers (rogues) should launch rock part on SLINGSHOT_FIRING_DELAY like they do in ActionFight (animation looks wrong now)
           case UnitType of
             ut_Arbaletman: gProjectiles.AimTarget(PositionF, fHouse, pt_Bolt, fUnit, RANGE_ARBALETMAN_MAX, RANGE_ARBALETMAN_MIN);
             ut_Bowman:     gProjectiles.AimTarget(PositionF, fHouse, pt_Arrow, fUnit, RANGE_BOWMAN_MAX, RANGE_BOWMAN_MIN);
             ut_Slingshot:  gProjectiles.AimTarget(PositionF, fHouse, pt_SlingRock, fUnit, RANGE_SLINGSHOT_MAX, RANGE_SLINGSHOT_MIN);
             else           raise Exception.Create('Unknown shooter');
           end;
           SetLastShootTime; //Record last time the warrior shot
           AnimLength := gRes.Units[UnitType].UnitAnim[ua_Work, Direction].Count;
           SetActionLockedStay(AnimLength - FIRING_DELAY, ua_Work, False, 0, FIRING_DELAY); //Reload for next attack
           fPhase := 0; //Go for another shot (will be 1 after inc below)
         end
         else
         begin
           SetActionLockedStay(6, ua_Work, False, 0, 6); // Pause for next attack

           //All melee units do 2 damage per strike
           fHouse.AddDamage(2, fUnit);

           //Play a sound. We should not use KaMRandom here because sound playback depends on FOW and is individual for each player
           if gMySpectator.FogOfWar.CheckTileRevelation(GetPosition.X, GetPosition.Y) >= 255 then
             gSoundPlayer.Play(MeleeSoundsHouse[Random(Length(MeleeSoundsHouse))], PositionF);

           fPhase := 1; //Go for another hit (will be 2 after inc below)
         end;
       end;
  end;

  Inc(fPhase);
end;


procedure TTaskAttackHouse.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  if fHouse <> nil then
    SaveStream.Write(fHouse.UID)
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fDestroyingHouse);
end;


end.
