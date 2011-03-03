unit KM_UnitTaskDie;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_Units, KromUtils, SysUtils;

type
  {Yep, this is a Task}
  TTaskDie = class(TUnitTask)
    public
      constructor Create(aUnit:TKMUnit);
      function Execute:TTaskResult; override;
    end;


implementation
uses KM_Sound, KM_PlayersCollection, KM_ResourceGFX, KM_Units_Warrior;


{ TTaskDie }
constructor TTaskDie.Create(aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_Die;
  //Shortcut to remove the pause before the dying animation which makes fights look odd
  if aUnit.Visible then
  begin
    fPhase := 1; //Phase 0 can be skipped when the unit is visible
    Execute;
  end;
end;


function TTaskDie.Execute:TTaskResult;
var SequenceLength:smallint;
begin
  Result := TaskContinues;
  with fUnit do
  case fPhase of
    0:  if Visible then
          SetActionLockedStay(0,ua_Walk)
        else begin
          if GetHome<>nil then begin
            GetHome.SetState(hst_Idle);
            GetHome.SetState(hst_Empty);
          end;
          SetActionGoIn(ua_Walk,gd_GoOutside,fPlayers.HousesHitTest(fUnit.NextPosition.X,fUnit.NextPosition.Y));
        end;
    1:  begin
          SequenceLength := fResource.GetUnitSequenceLength(UnitType,ua_Die,Direction);
          if fUnit is TKMUnitAnimal then //Animals don't have a dying sequence. Can be changed later.
            SetActionLockedStay(SequenceLength,ua_Walk,false)
          else
            SetActionLockedStay(SequenceLength,ua_Die,false);
          if fUnit is TKMUnitWarrior then
            fSoundLib.PlayWarrior(fUnit.UnitType, sp_Death);
        end;
    else begin
          fUnit.CloseUnit;          //This will FreeAndNil the Task and mark unit as "closed"
          Result := TaskContinues;  //Running UpdateState will exit without further changes
          exit;                     //Next UpdateState won't happen cos unit is "closed"
        end;
  end;
  inc(fPhase);
end;


end.
