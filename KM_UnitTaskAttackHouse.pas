unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KM_Units_Warrior, KM_Points;

  {Attack a house}
type
  TTaskAttackHouse = class(TUnitTask)
  private
    fHouse: TKMHouse;
    fDestroyingHouse: Boolean; //House destruction in progress
    LocID: Byte; //Current attack location
  public
    CanAbandon: Boolean;
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
uses KM_PlayersCollection, KM_Sound, KM_Resource, KM_Projectiles;


const
  MeleeSoundsHouse: array [0..12] of TSoundFX = (
    sfx_Melee37,
    sfx_Melee38,
    sfx_Melee39,
    sfx_Melee40,
    sfx_Melee41,
    sfx_Melee42,
    sfx_Melee43,
    sfx_Melee47,
    sfx_Melee51,
    sfx_Melee52,
    sfx_Melee53,
    sfx_Melee54,
    sfx_Melee57);


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnitWarrior; aHouse:TKMHouse);
begin
  inherited Create(aWarrior);
  fTaskName := utn_AttackHouse;
  fHouse := aHouse.GetHousePointer;
  fDestroyingHouse := false;
  LocID  := 0;
  CanAbandon := True;
end;


constructor TTaskAttackHouse.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fDestroyingHouse);
  LoadStream.Read(LocID);
  LoadStream.Read(CanAbandon);
end;


procedure TTaskAttackHouse.SyncLoad;
begin
  inherited;
  fHouse := fPlayers.GetHouseByID(cardinal(fHouse));
end;


destructor TTaskAttackHouse.Destroy;
begin
  fPlayers.CleanUpHousePointer(fHouse);
  inherited;
end;


function TTaskAttackHouse.WalkShouldAbandon:boolean;
begin
  Result := fHouse.IsDestroyed;
end;


function TTaskAttackHouse.Execute: TTaskResult;
var AnimLength:integer;
begin
  Result := TaskContinues;
  CanAbandon := True; //Default

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := TaskDone;
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
           if UnitType = ut_Arbaletman then
             SetActionLockedStay(CROSSBOWMEN_AIMING_DELAY_MIN+KaMRandom(CROSSBOWMEN_AIMING_DELAY_ADD),ua_Work,true) //Pretend to aim
           else
             SetActionLockedStay(BOWMEN_AIMING_DELAY_MIN+KaMRandom(BOWMEN_AIMING_DELAY_ADD),ua_Work,true); //Pretend to aim
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetEntrance); //Look at house
           if MySpectator.FogOfWar.CheckTileRevelation(Round(PositionF.X), Round(PositionF.Y), true) >= 255 then
             case UnitType of
               ut_Arbaletman: fSoundLib.Play(sfx_CrossbowDraw, PositionF); //Aiming
               ut_Bowman:     fSoundLib.Play(sfx_BowDraw,      PositionF); //Aiming
               ut_Slingshot:  fSoundLib.Play(sfx_SlingerShoot, PositionF); //Aiming
               else           Assert(false, 'Unknown shooter');
             end;
         end
         else
         begin
           SetActionLockedStay(0,ua_Work,false); //Melee units pause after the hit
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetClosestCell(GetPosition)); //Look at house
         end;
       end;
    2: begin
         //Let the house know it is being attacked
         fPlayers[fHouse.Owner].AI.HouseAttackNotification(fHouse, TKMUnitWarrior(fUnit));
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
           case UnitType of
             ut_Arbaletman: fProjectiles.AimTarget(PositionF, fHouse, pt_Bolt, Owner, RANGE_ARBALETMAN_MAX, RANGE_ARBALETMAN_MIN);
             ut_Bowman:     fProjectiles.AimTarget(PositionF, fHouse, pt_Arrow, Owner, RANGE_BOWMAN_MAX, RANGE_BOWMAN_MIN);
             ut_Slingshot:  fProjectiles.AimTarget(PositionF, fHouse, pt_SlingRock, Owner, RANGE_SLINGSHOT_MAX, RANGE_SLINGSHOT_MIN);
             else Assert(false, 'Unknown shooter');
           end;
           AnimLength := fResource.UnitDat[UnitType].UnitAnim[ua_Work, Direction].Count;
           SetActionLockedStay(AnimLength-FIRING_DELAY-1,ua_Work,false,0,FIRING_DELAY); //Reload for next attack
           fPhase := 0; //Go for another shot (will be 1 after inc below)
           CanAbandon := False; //Can't abandoned while archer is reloading (halt exploit)
         end
         else
         begin
           SetActionLockedStay(6,ua_Work,false,0,6); //Pause for next attack

           //All melee units do 2 damage per strike
           fHouse.AddDamage(fUnit.Owner, 2);

           //Play a sound. We should not use KaMRandom here because sound playback depends on FOW and is individual for each player
           if MySpectator.FogOfWar.CheckTileRevelation(GetPosition.X, GetPosition.Y, true) >= 255 then
             fSoundLib.Play(MeleeSoundsHouse[Random(Length(MeleeSoundsHouse))], PositionF);

           fPhase := 1; //Go for another hit (will be 2 after inc below)
           CanAbandon := False; //Can't abandoned while withdrawing weapon (halt exploit)
         end;
       end;
  end;

  inc(fPhase);
end;


procedure TTaskAttackHouse.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fDestroyingHouse);
  SaveStream.Write(LocID);
  SaveStream.Write(CanAbandon);
end;


end.
