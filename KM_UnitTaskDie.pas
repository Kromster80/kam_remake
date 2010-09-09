unit KM_UnitTaskDie;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Units, KM_Houses, KromUtils, SysUtils;

type
  {Yep, this is a Task}
  TTaskDie = class(TUnitTask)
    private
      SequenceLength:integer;
    public
      constructor Create(aUnit:TKMUnit);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_Sound, KM_PlayersCollection, KM_UnitActionWalkTo, KM_Game, KM_Utils, KM_ResourceGFX, KM_Units_Warrior;


{ TTaskDie }
constructor TTaskDie.Create(aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_Die;
  fUnit.SetActionLockedStay(0,ua_Walk);
  SequenceLength := fResource.GetUnitSequenceLength(fUnit.GetUnitType,ua_Die,fUnit.Direction);
  if fUnit is TKMUnitAnimal then SequenceLength := 0; //Animals don't have a dying sequence. Can be changed later.
end;


constructor TTaskDie.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(SequenceLength);
end;


procedure TTaskDie.Execute(out TaskDone:boolean);
begin
TaskDone := false;
with fUnit do
case fPhase of
  0: if not IsVisible then begin
       if GetHome<>nil then begin
         GetHome.SetState(hst_Idle);
         GetHome.SetState(hst_Empty);
         SetActionGoIn(ua_Walk,gd_GoOutside,fUnit.GetHome);
       end
       else
         SetActionGoIn(ua_Walk,gd_GoOutside,fPlayers.HousesHitTest(fUnit.NextPosition.X,fUnit.NextPosition.Y));
                                         //Inn or Store or etc.. for units without home.
                                         //Which means that our current approach to deduce housetype from
                                         //fUnit.fHome is wrong
     end else
     SetActionLockedStay(0,ua_Walk);
  1: if SequenceLength > 0 then
     begin
       SetActionLockedStay(SequenceLength,ua_Die,false);
       if fUnit is TKMUnitWarrior then
         fSoundLib.PlayWarrior(fUnit.GetUnitType, sp_Death);
     end
     else SetActionLockedStay(0,ua_Walk);
  else begin
      fUnit.CloseUnit;
      exit;
     end;
end;
inc(fPhase);
end;


procedure TTaskDie.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(SequenceLength);
end;



end.
