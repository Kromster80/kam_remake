unit KM_UnitTaskGoHome;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units, KM_Points;


type
  TTaskGoHome = class(TUnitTask)
  public
    constructor Create(aUnit: TKMUnit);
    function Execute:TTaskResult; override;
  end;


implementation


{ TTaskGoHome }
constructor TTaskGoHome.Create(aUnit: TKMUnit);
begin
  inherited;

  fTaskName := utn_GoHome;
end;


function TTaskGoHome.Execute: TTaskResult;
begin
  Result := tr_TaskContinues;

  if fUnit.GetHome.IsDestroyed then
  begin
    Result := tr_TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          Thought := th_Home;
          SetActionWalkToSpot(GetHome.PointBelowEntrance);
        end;
    1:  SetActionGoIn(ua_Walk, gd_GoInside, GetHome);
    2:  begin
          Thought := th_None; //Only stop thinking once we are right inside
          GetHome.SetState(hst_Idle);
          SetActionStay(5, ua_Walk);
        end;
    else Result := tr_TaskDone;
  end;

  Inc(fPhase);
end;


end.
