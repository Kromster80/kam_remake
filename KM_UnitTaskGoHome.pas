unit KM_UnitTaskGoHome;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Units, KM_Houses, KromUtils, SysUtils;

type
  TTaskGoHome = class(TUnitTask)
    public
      constructor Create(aUnit:TKMUnit);
      procedure Execute(out TaskDone:boolean); override;
    end;


implementation
uses KM_PlayersCollection, KM_UnitActionWalkTo, KM_Game, KM_Utils;


{ TTaskGoHome }
constructor TTaskGoHome.Create(aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_GoHome;
end;


procedure TTaskGoHome.Execute(out TaskDone:boolean);
begin
  TaskDone:=false;
  if fUnit.GetHome.IsDestroyed then begin
    Abandon;
    TaskDone:=true;
    exit;
  end;
  with fUnit do
  case fPhase of
    0: begin
         Thought := th_Home;
         SetActionWalk(fUnit,KMPointY1(GetHome.GetEntrance));
       end;
    1: SetActionGoIn(ua_Walk, gd_GoInside, GetHome);
    2: begin
        Thought := th_None; //Only stop thinking once we are right inside
        GetHome.SetState(hst_Idle);
        SetActionStay(5,ua_Walk);
       end;
    else TaskDone:=true;
  end;
  inc(fPhase);
  if (fUnit.GetUnitAction=nil)and(not TaskDone) then
    fGame.GameError(fUnit.GetPosition, 'Go home No action, no TaskDone!');
end;



end.
