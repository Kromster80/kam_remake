unit KM_UnitTaskSelfTrain;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Houses, KM_Units, KromUtils, SysUtils;


{Train in school}
type
  TTaskSelfTrain = class(TUnitTask)
    private
      fSchool:TKMHouseSchool;
    public
      constructor Create(aUnit:TKMUnit; aSchool:TKMHouseSchool);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      procedure Abandon(); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_PlayersCollection, KM_UnitActionWalkTo, KM_Game, KM_Utils;


{ TTaskSelfTrain }
constructor TTaskSelfTrain.Create(aUnit:TKMUnit; aSchool:TKMHouseSchool);
begin
  Inherited Create(aUnit);
  fTaskName := utn_SelfTrain;
  fSchool   := TKMHouseSchool(aSchool.GetHousePointer); //GetHouse returnes TKMHouse, not TKMHouseSchool
  fUnit.SetVisibility := false;
  fUnit.SetActionStay(0, ua_Walk);
end;


constructor TTaskSelfTrain.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fSchool, 4);
end;


procedure TTaskSelfTrain.SyncLoad();
begin
  Inherited;
  fSchool := TKMHouseSchool(fPlayers.GetHouseByID(cardinal(fSchool)));
end;


destructor TTaskSelfTrain.Destroy;
begin
  if fSchool <> nil then fSchool.ReleaseHousePointer;
  Inherited;
end;


//Abort if someone has destroyed our school
procedure TTaskSelfTrain.Abandon;
var TempUnit: TKMUnit;
begin
  TempUnit := fUnit; //Make local copy of the pointer because Inherited will set the pointer to nil
  Inherited;
  TempUnit.CloseUnit; //CloseUnit at last, cos it will FreeAndNil TTask
end;


procedure TTaskSelfTrain.Execute(out TaskDone:boolean);
begin
  TaskDone := false;

  if fSchool.IsDestroyed then
  begin
    Abandon;
    TaskDone:=true;
    exit;
  end;

  with fUnit do
    case fPhase of
      0: begin
          fSchool.SetState(hst_Work);
          fSchool.fCurrentAction.SubActionWork(ha_Work1);
          SetActionStay(29,ua_Walk);
        end;
      1: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work2);
          SetActionStay(29,ua_Walk);
        end;
      2: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work3);
          SetActionStay(29,ua_Walk);
        end;
      3: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work4);
          SetActionStay(29,ua_Walk);
        end;
      4: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work5);
          SetActionStay(29,ua_Walk);
        end;
      5: begin
          fSchool.SetState(hst_Idle);
          SetActionStay(9,ua_Walk);
         end;
      6: begin
          SetActionGoIn(ua_Walk,gd_GoOutside,fSchool);
          fSchool.UnitTrainingComplete;
          fPlayers.Player[byte(GetOwner)].CreatedUnit(GetUnitType,true);
         end;
      else TaskDone:=true;
    end;
  inc(fPhase);
end;


procedure TTaskSelfTrain.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fSchool <> nil then
    SaveStream.Write(fSchool.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
end;


end.
