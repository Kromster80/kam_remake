unit KM_UnitTaskThrowRock;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Units, KromUtils, SysUtils;


{Throw a rock}
type
  TTaskThrowRock = class(TUnitTask)
    private
      fTarget:TKMUnit;
    public
      constructor Create(aUnit,aTarget:TKMUnit);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_PlayersCollection, KM_UnitActionWalkTo, KM_Game, KM_Utils, KM_Projectiles;


{ TTaskThrowRock }
constructor TTaskThrowRock.Create(aUnit,aTarget:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_ThrowRock;
  fTarget := aTarget;
  if fUnit <> nil then fUnit.SetActionLockedStay(0, ua_Walk);
end;


constructor TTaskThrowRock.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fTarget, 4);
end;


procedure TTaskThrowRock.SyncLoad();
begin
  Inherited;
  fTarget := fPlayers.GetUnitByID(cardinal(fTarget));
end;


procedure TTaskThrowRock.Execute(out TaskDone:boolean);
begin
  TaskDone := false;

  if fUnit.GetHome.IsDestroyed then begin
    Abandon;
    TaskDone := true;
    exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         GetHome.ResTakeFromIn(rt_Stone, 1);
         GetHome.SetState(hst_Work); //Set house to Work state
         GetHome.fCurrentAction.SubActionWork(ha_Work2); //show Recruits back
         SetActionStay(20,ua_Walk); //2sec?
         fGame.fProjectiles.AddItem(KMPointF(GetPosition.X - 0.5, GetPosition.Y - 1), fTarget.PositionF, pt_Rock);
       end;
    1: begin
         SetActionStay(1,ua_Walk);
         //if fTarget <> nil then fTarget.KillUnit; //It might be killed by now
         GetHome.SetState(hst_Idle);
       end;
    else TaskDone := true;
  end;
  inc(fPhase);
  if (fUnit.GetUnitAction=nil)and(not TaskDone) then
    fLog.AssertToLog(false,'(TTaskThrowRock.fCurrentAction=nil)and(not TaskDone)');
end;


procedure TTaskThrowRock.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fTarget <> nil then
    SaveStream.Write(fTarget.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
end;



end.
