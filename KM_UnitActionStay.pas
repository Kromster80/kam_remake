unit KM_UnitActionStay;
interface
uses Classes, KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Units, SysUtils, Math;

{Stay in place for set time}
type
TUnitActionStay = class(TUnitAction)
  private
    StayStill:boolean;
    TimeToStay:integer;
    StillFrame:byte;
    ActionType:TUnitActionType;
  public
    Locked: boolean;
    constructor Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0; const aLocked:boolean=false);
    constructor Load(LoadStream:TMemoryStream);
    function HowLongLeftToStay():integer;
    procedure MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
    procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
    procedure Save(SaveStream:TMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_UnitActionWalkTo, KM_SoundFX;


{ TUnitActionStay }
constructor TUnitActionStay.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true; const aStillFrame:byte=0; const aLocked:boolean=false);
begin
  Inherited Create(aActionType);
  fActionName := uan_Stay;
  StayStill  := aStayStill;
  TimeToStay := aTimeToStay;
  ActionType := aActionType;
  StillFrame := aStillFrame;
  Locked     := aLocked;
end;


constructor TUnitActionStay.Load(LoadStream:TMemoryStream);
begin
  Inherited;
  LoadStream.Read(StayStill,4);
  LoadStream.Read(TimeToStay,4);
  LoadStream.Read(StillFrame,4);
  LoadStream.Read(ActionType,4);
  LoadStream.Read(Locked,4);
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
    ut_Worker: case ActionType of
                 ua_Work:  if Step = 3 then fSoundLib.Play(sfx_housebuild,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_Dig,KMUnit.GetPosition,true);
                 ua_Work2: if Step = 8 then fSoundLib.Play(sfx_pave,KMUnit.GetPosition,true);
               end;
    ut_Farmer: case ActionType of
                 ua_Work:  if Step = 8 then fSoundLib.Play(sfx_corncut,KMUnit.GetPosition,true);
                 ua_Work1: if Step = 0 then fSoundLib.Play(sfx_cornsow,KMUnit.GetPosition,true,0.8);
               end;
    ut_StoneCutter: if ActionType = ua_Work then
                           if Step = 3 then fSoundLib.Play(sfx_minestone,KMUnit.GetPosition,true,1.4);
    ut_WoodCutter: case ActionType of
                     ua_Work: if (KMUnit.AnimStep mod Cycle = 5) and (KMUnit.Direction <> dir_N) then fSoundLib.Play(sfx_choptree,KMUnit.GetPosition,true)
                     else     if (KMUnit.AnimStep mod Cycle = 0) and (KMUnit.Direction =  dir_N) then fSoundLib.Play(sfx_WoodcutterDig,KMUnit.GetPosition,true);
                   end;
  end;
end;


procedure TUnitActionStay.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Cycle,Step:byte;
begin
  if not StayStill then
  begin
    Cycle := max(UnitSprite[byte(KMUnit.GetUnitType)].Act[byte(ActionType)].Dir[byte(KMUnit.Direction)].Count,1);
    Step  := KMUnit.AnimStep mod Cycle;

    IsStepDone := KMUnit.AnimStep mod Cycle = 0;

    if TimeToStay >= 1 then MakeSound(KMUnit, Cycle, Step);

    inc(KMUnit.AnimStep);
  end
  else
  begin
    KMUnit.AnimStep := StillFrame;
    IsStepDone := true;
  end;

  dec(TimeToStay);
  DoEnd := TimeToStay<=0;
end;


procedure TUnitActionStay.Save(SaveStream:TMemoryStream);
begin
  inherited;
  SaveStream.Write(StayStill,4);
  SaveStream.Write(TimeToStay,4);
  SaveStream.Write(StillFrame,4);
  SaveStream.Write(ActionType,4);
  SaveStream.Write(Locked,4);
end;




end.
