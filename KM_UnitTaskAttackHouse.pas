unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KM_Units_Warrior, SysUtils, KM_Points;

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
uses KM_Game, KM_PlayersCollection, KM_Sound;


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
  if fHouse.IsDestroyed then
  begin
    Result := TaskDone;
    //Commander should reposition his men after destroying the house
    if TKMUnitWarrior(fUnit).IsCommander then
      TKMUnitWarrior(fUnit).OrderWalk(fUnit.GetPosition); //Don't use halt because that returns us to fOrderLoc
    exit;
  end;

  with fUnit do
  case fPhase of
    0: if TKMUnitWarrior(fUnit).IsRanged then
         //todo: Sort out cases when archers are too close, either step back to the minimum range or don't participate in the attack
         SetActionWalkToHouse(fHouse, RANGE_BOWMAN_MAX / (byte(REDUCE_SHOOTING_RANGE)+1))
       else
         SetActionWalkToHouse(fHouse, 1);
    1: begin
         //Once we've reached the house, if the player clicks halt we reposition here
         if TKMUnitWarrior(fUnit).IsCommander then
           TKMUnitWarrior(fUnit).OrderLocDir := KMPointDir(GetPosition,TKMUnitWarrior(fUnit).OrderLocDir.Dir);

         if TKMUnitWarrior(fUnit).IsRanged then begin
           SetActionLockedStay(AIMING_DELAY_MIN+KaMRandom(AIMING_DELAY_ADD),ua_Work,true); //Pretend to aim
           if not KMSamePoint(GetPosition, fHouse.GetClosestCell(GetPosition)) then //Unbuilt houses can be attacked from within
             Direction := KMGetDirection(GetPosition, fHouse.GetEntrance); //Look at house
           case UnitType of
             ut_Arbaletman: fSoundLib.Play(sfx_CrossbowDraw,GetPosition,true); //Aiming
             ut_Bowman:     fSoundLib.Play(sfx_BowDraw,GetPosition,true); //Aiming
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
         if TKMUnitWarrior(fUnit).IsRanged then
           SetActionLockedStay(FIRING_DELAY,ua_Work,false,0,0) //Start shooting
         else
           SetActionLockedStay(6,ua_Work,false,0,0); //Start the hit
       end;
    3: begin
         if TKMUnitWarrior(fUnit).IsRanged then
         begin //Launch the missile and forget about it
           //Shooting range is not important now, houses don't walk (except Howl's Moving Castle perhaps)
           case UnitType of
             ut_Arbaletman: fGame.Projectiles.AimTarget(PositionF, fHouse, pt_Bolt, GetOwner);
             ut_Bowman:     fGame.Projectiles.AimTarget(PositionF, fHouse, pt_Arrow, GetOwner);
             else Assert(false, 'Unknown shooter');
           end;
           AnimLength := UnitSprite[byte(UnitType)].Act[byte(ua_Work)].Dir[byte(Direction)].Count;
           SetActionLockedStay(AnimLength-FIRING_DELAY-1,ua_Work,false,0,FIRING_DELAY); //Reload for next attack
           fPhase := 0; //Go for another shot (will be 1 after inc below)
         end else begin
           SetActionLockedStay(6,ua_Work,false,0,6); //Pause for next attack
           if fHouse.AddDamage(2) then //All melee units do 2 damage per strike
             if (fPlayers <> nil) and (fPlayers.Player[GetOwner] <> nil) then
               fPlayers.Player[GetOwner].Stats.HouseDestroyed(fHouse.GetHouseType);
           //todo: Melee house hit sound
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
    SaveStream.Write(Zero);
  SaveStream.Write(fDestroyingHouse);
  SaveStream.Write(LocID);
end;


end.
