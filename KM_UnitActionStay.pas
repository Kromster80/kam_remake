unit KM_UnitActionStay;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KromUtils, KM_CommonTypes, KM_Units, SysUtils, Math;

{Stay in place for set time}
type
TUnitActionStay = class(TUnitAction)
  private
    StayStill:boolean;
    TimeToStay:integer;
    StillFrame:byte;
  public
    Locked: boolean;
    constructor Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0; const aLocked:boolean=false);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad(); override;
    function HowLongLeftToStay():integer;
    procedure MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Sound;


{ TUnitActionStay }
constructor TUnitActionStay.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0; const aLocked:boolean=false);
begin
  Inherited Create(aActionType);
  fActionName := uan_Stay;
  StayStill   := aStayStill;
  TimeToStay  := aTimeToStay;
  StillFrame  := aStillFrame;
  Locked      := aLocked;
end;


constructor TUnitActionStay.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(StayStill);
  LoadStream.Read(TimeToStay);
  LoadStream.Read(StillFrame,SizeOf(StillFrame));
  LoadStream.Read(Locked);
end;


procedure TUnitActionStay.SyncLoad();
begin
  Inherited;
  //nothing, FPC doesn't likes it missing for some reason
end;


//If someone whats to know how much time unit has to stay
function TUnitActionStay.HowLongLeftToStay():integer;
begin
  Result := EnsureRange(TimeToStay, 0, maxint);
end;


procedure TUnitActionStay.MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
begin
  //Do not play sounds if unit is invisible to MyPlayer
  if fTerrain.CheckTileRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y, MyPlayer.PlayerID) < 255 then exit;

  case KMUnit.GetUnitType of //Various UnitTypes and ActionTypes
    ut_Worker: case GetActionType of
                 ua_Work:  if Step = 3 then fSoundLib.Play(sfx_housebuild,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_Dig,KMUnit.GetPosition,true);
                 ua_Work2: if Step = 8 then fSoundLib.Play(sfx_pave,KMUnit.GetPosition,true);
               end;
    ut_Farmer: case GetActionType of
                 ua_Work:  if Step = 8 then fSoundLib.Play(sfx_corncut,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_cornsow,KMUnit.GetPosition,true,0.8);
               end;
    ut_StoneCutter: if GetActionType = ua_Work then
                           if Step = 3 then fSoundLib.Play(sfx_minestone,KMUnit.GetPosition,true,1.4);
    ut_WoodCutter: case GetActionType of
                     ua_Work: if (KMUnit.AnimStep mod Cycle = 5) and (KMUnit.Direction <> dir_N) then fSoundLib.Play(sfx_choptree,KMUnit.GetPosition,true)
                     else     if (KMUnit.AnimStep mod Cycle = 0) and (KMUnit.Direction =  dir_N) then fSoundLib.Play(sfx_WoodcutterDig,KMUnit.GetPosition,true);
                   end;
  end;
end;


function TUnitActionStay.Execute(KMUnit: TKMUnit):TActionResult;
var Cycle,Step:byte;
begin
  if not StayStill then
  begin
    Cycle := max(UnitSprite[byte(KMUnit.GetUnitType)].Act[byte(GetActionType)].Dir[byte(KMUnit.Direction)].Count,1);
    Step  := KMUnit.AnimStep mod Cycle;

    StepDone := KMUnit.AnimStep mod Cycle = 0;

    if TimeToStay >= 1 then MakeSound(KMUnit, Cycle, Step);

    inc(KMUnit.AnimStep);
  end
  else
  begin
    KMUnit.AnimStep := StillFrame;
    StepDone := true;
  end;

  dec(TimeToStay);
  if TimeToStay<=0 then
    Result := ActDone
  else
    Result := ActContinues;
end;


procedure TUnitActionStay.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(StayStill);
  SaveStream.Write(TimeToStay);
  SaveStream.Write(StillFrame,SizeOf(StillFrame));
  SaveStream.Write(Locked);
end;




end.
