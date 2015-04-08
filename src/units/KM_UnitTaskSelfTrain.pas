unit KM_UnitTaskSelfTrain;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_Houses, KM_HouseSchool, KM_Units, SysUtils;


//Train citizen in school
type
  TTaskSelfTrain = class(TUnitTask)
  private
    fSchool: TKMHouseSchool;
  public
    constructor Create(aUnit: TKMUnit; aSchool: TKMHouseSchool);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_Game;


{ TTaskSelfTrain }
constructor TTaskSelfTrain.Create(aUnit: TKMUnit; aSchool: TKMHouseSchool);
begin
  inherited Create(aUnit);
  fTaskName := utn_SelfTrain;
  fSchool   := TKMHouseSchool(aSchool.GetHousePointer);
  fUnit.Visible := False;
end;


constructor TTaskSelfTrain.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fSchool, 4);
end;


procedure TTaskSelfTrain.SyncLoad;
begin
  inherited;
  fSchool := TKMHouseSchool(gHands.GetHouseByUID(Cardinal(fSchool)));
end;


destructor TTaskSelfTrain.Destroy;
begin
  if gGame.IsExiting then Exit; //fSchool will already be freed
  if (fPhase <= 5) and not fSchool.IsDestroyed then fSchool.SetState(hst_Idle); //If we abandon for some reason, clear the school animation
  gHands.CleanUpHousePointer(TKMHouse(fSchool));
  inherited;
end;


function TTaskSelfTrain.Execute:TTaskResult;
begin
  Result := TaskContinues;

  //If the school has been destroyed then this task should not be running (school frees it on CloseHouse)
  //However, if we are past phase 6 (task ends on phase 7) then the school does not know about us (we have stepped outside)
  if fSchool.IsDestroyed and (fPhase <= 6) then
  begin //School will cancel the training on own destruction
    Assert(False, 'Unexpected error. Destoyed school erases the task');
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0: begin
          fSchool.SetState(hst_Work);
          fSchool.fCurrentAction.SubActionWork(ha_Work1);
          SetActionLockedStay(29, ua_Walk);
        end;
      1: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work2);
          SetActionLockedStay(29, ua_Walk);
        end;
      2: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work3);
          SetActionLockedStay(29, ua_Walk);
        end;
      3: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work4);
          SetActionLockedStay(29, ua_Walk);
        end;
      4: begin
          fSchool.fCurrentAction.SubActionWork(ha_Work5);
          SetActionLockedStay(29, ua_Walk);
        end;
      5: begin
          fSchool.SetState(hst_Idle);
          SetActionLockedStay(9, ua_Walk);
         end;
      6: begin
          fUnit.SetInHouse(fSchool); //Put him in the school, so if it is destroyed while he is inside he is placed somewhere
          SetActionGoIn(ua_Walk, gd_GoOutside, fSchool);
          fSchool.UnitTrainingComplete(fUnit);
          if Assigned(fUnit.OnUnitTrained) then
            fUnit.OnUnitTrained(fUnit);
         end;
      else Result := TaskDone;
    end;
  Inc(fPhase);
end;


procedure TTaskSelfTrain.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fSchool <> nil then
    SaveStream.Write(fSchool.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
end;


end.
