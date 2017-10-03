unit KM_UnitTaskThrowRock;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Units;


{Throw a rock}
type
  TTaskThrowRock = class(TUnitTask)
  private
    fTarget: TKMUnit;
    fFlightTime: Word; //Thats how long it will take a stone to hit it's target
  public
    constructor Create(aUnit, aTarget: TKMUnit);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_Projectiles, KM_ResWares, KM_Hand;


{ TTaskThrowRock }
constructor TTaskThrowRock.Create(aUnit, aTarget: TKMUnit);
begin
  inherited Create(aUnit);
  fTaskName := utn_ThrowRock;
  fTarget := aTarget.GetUnitPointer;
end;


destructor TTaskThrowRock.Destroy;
begin
  if not fUnit.GetHome.IsDestroyed and (fUnit.GetHome.GetState = hst_Work) then
    fUnit.GetHome.SetState(hst_Idle); //Make sure we don't abandon and leave our tower with "working" animations
  gHands.CleanUpUnitPointer(fTarget);
  inherited;
end;


constructor TTaskThrowRock.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fTarget, 4);
  LoadStream.Read(fFlightTime);
end;


procedure TTaskThrowRock.SyncLoad;
begin
  inherited;
  fTarget := gHands.GetUnitByUID(cardinal(fTarget));
end;


function TTaskThrowRock.Execute: TTaskResult;
begin
  Result := tr_TaskContinues;

  //Target could have been killed by another Tower or in a fight
  if fUnit.GetHome.IsDestroyed or ((fTarget<>nil) and fTarget.IsDeadOrDying) then
  begin
    Result := tr_TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          GetHome.SetState(hst_Work); //Set house to Work state
          GetHome.CurrentAction.SubActionWork(ha_Work2); //show Recruits back
          SetActionStay(2, ua_Walk); //pretend to be taking the stone
        end;
    1:  begin
          GetHome.ResTakeFromIn(wt_Stone, 1);
          gHands[Owner].Stats.WareConsumed(wt_Stone);
          fFlightTime := gProjectiles.AimTarget(PositionF, fTarget, pt_TowerRock, fUnit, RANGE_WATCHTOWER_MAX, RANGE_WATCHTOWER_MIN);
          gHands.CleanUpUnitPointer(fTarget); //We don't need it anymore
          SetActionLockedStay(1, ua_Walk);
        end;
    2:  SetActionLockedStay(fFlightTime, ua_Walk); //Pretend to look how it goes
    3:  begin
          GetHome.SetState(hst_Idle);
          SetActionStay(20, ua_Walk); //Idle before throwing another rock
        end;
    else Result := tr_TaskDone;
  end;
  Inc(fPhase);
end;


procedure TTaskThrowRock.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fTarget <> nil then
    SaveStream.Write(fTarget.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fFlightTime);
end;



end.
