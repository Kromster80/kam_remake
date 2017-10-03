unit KM_UnitActionStay;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KromUtils, KM_CommonClasses, KM_Units, SysUtils, Math, KM_Points;

type
  {Stay in place for set time}
  TUnitActionStay = class(TUnitAction)
  private
    StayStill:boolean;
    TimeToStay:integer;
    StillFrame:byte;
    procedure MakeSound(Cycle, Step: Byte);
  public
    constructor Create(aUnit: TKMUnit; aTimeToStay:integer; aActionType:TUnitActionType; aStayStill:boolean; aStillFrame:byte; aLocked:boolean);
    constructor Load(LoadStream:TKMemoryStream); override;
    function ActName: TUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    function Execute: TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_Sound, KM_ResSound, KM_Resource, KM_ResUnits;


{ TUnitActionStay }
constructor TUnitActionStay.Create(aUnit: TKMUnit; aTimeToStay:integer; aActionType:TUnitActionType; aStayStill:boolean; aStillFrame:byte; aLocked:boolean);
begin
  inherited Create(aUnit, aActionType, aLocked);
  StayStill   := aStayStill;
  TimeToStay  := aTimeToStay;
  StillFrame  := aStillFrame;
end;


constructor TUnitActionStay.Load(LoadStream:TKMemoryStream);
begin
  inherited;
  LoadStream.Read(StayStill);
  LoadStream.Read(TimeToStay);
  LoadStream.Read(StillFrame);
end;


function TUnitActionStay.ActName: TUnitActionName;
begin
  Result := uan_Stay;
end;


function TUnitActionStay.GetExplanation: UnicodeString;
begin
  Result := 'Staying';
end;


procedure TUnitActionStay.MakeSound(Cycle, Step: Byte);
begin
  if SKIP_SOUND then Exit;

  //Do not play sounds if unit is invisible to gMySpectator
  if gMySpectator.FogOfWar.CheckTileRevelation(fUnit.GetPosition.X, fUnit.GetPosition.Y) < 255 then exit;

  //Various UnitTypes and ActionTypes produce all the sounds
  case fUnit.UnitType of
    ut_Worker:      case ActionType of
                      ua_Work:  if Step = 3 then gSoundPlayer.Play(sfx_housebuild,fUnit.PositionF);
                      ua_Work1: if Step = 0 then gSoundPlayer.Play(sfx_Dig,fUnit.PositionF);
                      ua_Work2: if Step = 8 then gSoundPlayer.Play(sfx_Pave,fUnit.PositionF);
                    end;
    ut_Farmer:      case ActionType of
                      ua_Work:  if Step = 8 then gSoundPlayer.Play(sfx_CornCut,fUnit.PositionF);
                      ua_Work1: if Step = 0 then gSoundPlayer.Play(sfx_CornSow,fUnit.PositionF,true,0.6);
                    end;
    ut_StoneCutter: if ActionType = ua_Work then
                      if Step = 3 then gSoundPlayer.Play(sfx_minestone,fUnit.PositionF,true,1.4);
    ut_WoodCutter:  case ActionType of
                      ua_Work: if (fUnit.AnimStep mod Cycle = 3) and (fUnit.Direction <> dir_N) then gSoundPlayer.Play(sfx_ChopTree, fUnit.PositionF,true)
                      else     if (fUnit.AnimStep mod Cycle = 0) and (fUnit.Direction =  dir_N) then gSoundPlayer.Play(sfx_WoodcutterDig, fUnit.PositionF,true);
                    end;
  end;
end;


function TUnitActionStay.Execute: TActionResult;
var
  Cycle, Step: Byte;
begin
  if not StayStill then
  begin
    Cycle := max(gRes.Units[fUnit.UnitType].UnitAnim[ActionType, fUnit.Direction].Count, 1);
    Step  := fUnit.AnimStep mod Cycle;

    StepDone := fUnit.AnimStep mod Cycle = 0;

    if TimeToStay >= 1 then MakeSound(Cycle, Step);

    inc(fUnit.AnimStep);
  end
  else
  begin
    fUnit.AnimStep := StillFrame;
    StepDone := true;
  end;

  dec(TimeToStay);
  if TimeToStay<=0 then
    Result := ar_ActDone
  else
    Result := ar_ActContinues;
end;


procedure TUnitActionStay.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(StayStill);
  SaveStream.Write(TimeToStay);
  SaveStream.Write(StillFrame);
end;




end.
