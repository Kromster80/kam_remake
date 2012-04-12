unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonClasses, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KM_Units_Warrior, SysUtils, KM_Points;

  {Attack a house}
type
  TTaskAttackHouse = class(TUnitTask)
  private
    fHouse:TKMHouse;
    fDestroyingHouse:boolean; //House destruction in progress
    LocID:byte; //Current attack location
  public
    constructor Create(aWarrior: TKMUnitWarrior; aHouse:TKMHouse);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property DestroyingHouse:boolean read fDestroyingHouse;
    function WalkShouldAbandon:boolean; override;
    function Execute:TTaskResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Sound, KM_Resource;


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnitWarrior; aHouse:TKMHouse);
begin
  Inherited Create(aWarrior);
  fTaskName := utn_AttackHouse;
  fHouse := aHouse.GetHousePointer;
  fDestroyingHouse := false;
  LocID  := 0;
end;


constructor TTaskAttackHouse.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fDestroyingHouse);
  LoadStream.Read(LocID);
end;


procedure TTaskAttackHouse.SyncLoad;
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(cardinal(fHouse));
end;


destructor TTaskAttackHouse.Destroy;
begin
  fPlayers.CleanUpHousePointer(fHouse);
  Inherited;
end;


function TTaskAttackHouse.WalkShouldAbandon:boolean;
begin
  Result := fHouse.IsDestroyed;
end;


function TTaskAttackHouse.Execute:TTaskResult;
var AnimLength:integer;
begin
  Result := TaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := TaskDone;
    //Commander should reposition his men after destroying the house
    if TKMUnitWarrior(fUnit).IsCommander then
      TKMUnitWarrior(fUnit).OrderWalk(fUnit.GetPosition); //Don't use halt because that returns us to fOrderLoc
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
  case fPhase of
    0: if IsRanged then
         if fHouse.GetDistance(GetPosition) < GetFightMinRange then
           //todo: Archer is too close, try to step back to the minimum range
           //SetActionWalkFromHouse(fHouse, GetFightMinRange)
           Result := TaskDone
         else
           SetActionWalkToHouse(fHouse, GetFightMaxRange)
       else
         SetActionWalkToHouse(fHouse, 1);
    1: begin
         //Once we've reached the house, if the player clicks halt we reposition here
         if IsCommander then
           OrderLocDir := KMPointDir(GetPosition, OrderLocDir.Dir);

         if IsRanged then begin
           SetActionLockedStay(AIMING_DELAY_MIN+KaMRandom(AIMING_DELAY_ADD),ua_Work,true); //Pretend to aim
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetEntrance); //Look at house
           if MyPlayer.FogOfWar.CheckTileRevelation(Round(PositionF.X), Round(PositionF.Y), true) >= 255 then
             case UnitType of
               ut_Arbaletman: fSoundLib.Play(sfx_CrossbowDraw, PositionF); //Aiming
               ut_Bowman:     fSoundLib.Play(sfx_BowDraw,      PositionF); //Aiming
               ut_Slingshot:  fSoundLib.Play(sfx_SlingerShoot, PositionF); //Aiming
               else Assert(false, 'Unknown shooter');
             end;
         end else begin
           SetActionLockedStay(0,ua_Work,false); //@Lewin: Maybe melee units can randomly pause for 1-2 frames as well?
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetClosestCell(GetPosition)); //Look at house
         end;
       end;
    2: begin
         //Let the house know it is being attacked
         fPlayers.Player[fHouse.GetOwner].AI.HouseAttackNotification(fHouse, TKMUnitWarrior(fUnit));
         fDestroyingHouse := true;
         if IsRanged then
           SetActionLockedStay(FIRING_DELAY,ua_Work,false,0,0) //Start shooting
         else
           SetActionLockedStay(6,ua_Work,false,0,0); //Start the hit
       end;
    3: begin
         if IsRanged then
         begin //Launch the missile and forget about it
           //Shooting range is not important now, houses don't walk (except Howl's Moving Castle perhaps)
           case UnitType of
             ut_Arbaletman: fGame.Projectiles.AimTarget(PositionF, fHouse, pt_Bolt, GetOwner, RANGE_ARBALETMAN_MAX, RANGE_ARBALETMAN_MIN);
             ut_Bowman:     fGame.Projectiles.AimTarget(PositionF, fHouse, pt_Arrow, GetOwner, RANGE_BOWMAN_MAX, RANGE_BOWMAN_MIN);
             ut_Slingshot:  fGame.Projectiles.AimTarget(PositionF, fHouse, pt_SlingRock, GetOwner, RANGE_SLINGSHOT_MAX, RANGE_SLINGSHOT_MIN);
             else Assert(false, 'Unknown shooter');
           end;
           AnimLength := fResource.UnitDat[UnitType].UnitAnim[ua_Work, Direction].Count;
           SetActionLockedStay(AnimLength-FIRING_DELAY-1,ua_Work,false,0,FIRING_DELAY); //Reload for next attack
           fPhase := 0; //Go for another shot (will be 1 after inc below)
         end else begin
           SetActionLockedStay(6,ua_Work,false,0,6); //Pause for next attack
           if fHouse.AddDamage(2) then //All melee units do 2 damage per strike
             if (fPlayers <> nil) and (fPlayers.Player[GetOwner] <> nil) then
               fPlayers.Player[GetOwner].Stats.HouseDestroyed(fHouse.HouseType);

           //Play a sound. We should not use KaMRandom here because sound playback depends on FOW and is individual for each player
           if MyPlayer.FogOfWar.CheckTileRevelation(GetPosition.X, GetPosition.Y, true) >= 255 then
             fSoundLib.Play(MeleeSoundsHouse[Random(Length(MeleeSoundsHouse))], PositionF);

           fPhase := 1; //Go for another hit (will be 2 after inc below)
         end;
       end;
  end;

  inc(fPhase);
end;


procedure TTaskAttackHouse.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fDestroyingHouse);
  SaveStream.Write(LocID);
end;


end.
