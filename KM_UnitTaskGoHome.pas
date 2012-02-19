unit KM_UnitTaskGoHome;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KM_Units, SysUtils, KM_Points;

type
  TTaskGoHome = class(TUnitTask)
    public
      constructor Create(aUnit:TKMUnit);
      function Execute:TTaskResult; override;
    end;


implementation


{ TTaskGoHome }
constructor TTaskGoHome.Create(aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_GoHome;
end;


function TTaskGoHome.Execute:TTaskResult;
begin
  Result := TaskContinues;
  if fUnit.GetHome.IsDestroyed then begin
    Result := TaskDone;
    exit;
  end;
  with fUnit do
  case fPhase of
    0: begin
         Thought := th_Home;
         SetActionWalkToSpot(KMPointBelow(GetHome.GetEntrance));
       end;
    1: SetActionGoIn(ua_Walk, gd_GoInside, GetHome);
    2: begin
        Thought := th_None; //Only stop thinking once we are right inside
        GetHome.SetState(hst_Idle);
        SetActionStay(5,ua_Walk);
       end;
    else Result := TaskDone;
  end;

  inc(fPhase);
end;



end.
