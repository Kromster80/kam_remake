unit KM_UnitTaskDie;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_Units, SysUtils;

type
  {Yep, this is a Task}
  TTaskDie = class(TUnitTask)
  private
    fShowAnimation: Boolean;
  public
    constructor Create(aUnit: TKMUnit; aShowAnimation: Boolean);
    constructor Load(LoadStream: TKMemoryStream); override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_ResSound, KM_Sound, KM_HandsCollection, KM_Resource, KM_Units_Warrior, KM_ScriptingEvents,
  KM_ResUnits;


{ TTaskDie }
constructor TTaskDie.Create(aUnit: TKMUnit; aShowAnimation: Boolean);
begin
  inherited Create(aUnit);
  fTaskName := utn_Die;
  fShowAnimation := aShowAnimation;
  //Shortcut to remove the pause before the dying animation which makes fights look odd
  if aUnit.Visible then
  begin
    fPhase := 1; //Phase 0 can be skipped when the unit is visible
    Execute;
  end;
end;


constructor TTaskDie.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fShowAnimation);
end;


procedure TTaskDie.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fShowAnimation);
end;


function TTaskDie.Execute: TTaskResult;
var
  SequenceLength: SmallInt;
  TempOwner: TKMHandIndex;
  TempUnitType: TUnitType;
  TempX, TempY: Word;
begin
  Result := tr_TaskContinues;
  with fUnit do
  case fPhase of
    0:    if Visible then
            SetActionLockedStay(0, ua_Walk)
          else
          begin
            if (GetHome <> nil) and not GetHome.IsDestroyed then
            begin
              GetHome.SetState(hst_Idle);
              GetHome.SetState(hst_Empty);
            end;
            SetActionGoIn(ua_Walk, gd_GoOutside, gHands.HousesHitTest(fUnit.NextPosition.X, fUnit.NextPosition.Y));
          end;
    1:    begin
            if not fShowAnimation or (fUnit is TKMUnitAnimal) then //Animals don't have a dying sequence. Can be changed later.
              SetActionLockedStay(0, ua_Walk, False)
            else
            begin
              SequenceLength := gRes.Units[UnitType].UnitAnim[ua_Die, Direction].Count;
              SetActionLockedStay(SequenceLength, ua_Die, False);
              //Do not play sounds if unit is invisible to gMySpectator
              //We should not use KaMRandom below this line because sound playback depends on FOW and is individual for each player
              if gMySpectator.FogOfWar.CheckTileRevelation(fUnit.GetPosition.X, fUnit.GetPosition.Y) >= 255 then
              begin
                if fUnit is TKMUnitWarrior then
                  gSoundPlayer.PlayWarrior(fUnit.UnitType, sp_Death, fUnit.PositionF)
                else
                  gSoundPlayer.PlayCitizen(fUnit.UnitType, sp_Death, fUnit.PositionF);
              end;
            end;
          end;
    else  begin
            //Store them before they get lost forever
            TempOwner := fUnit.Owner;
            TempUnitType := fUnit.UnitType;
            TempX := fUnit.GetPosition.X;
            TempY := fUnit.GetPosition.Y;

            fUnit.CloseUnit;          //This will FreeAndNil the Task and mark unit as "closed"

            //Can't run this in CloseUnit since it is used from other places like barracks equipping and we only want it for normal deaths
            //Notify the script that the unit is now gone from the game
            gScriptEvents.ProcUnitAfterDied(TempUnitType, TempOwner, TempX, TempY);

            Result := tr_TaskContinues;  //Running UpdateState will exit without further changes
            Exit;                     //Next UpdateState won't happen cos unit is "closed"
          end;
  end;
  Inc(fPhase);
end;


end.
