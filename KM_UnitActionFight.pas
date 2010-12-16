unit KM_UnitActionFight;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KromUtils, Math, SysUtils, KM_Units;



{Fight until we die or the opponent dies}
type
TUnitActionFight = class(TUnitAction)
  private
    fOpponent:TKMUnit; //Who we are fighting with
  public
    constructor Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad(); override;
    procedure MakeSound(KMUnit: TKMUnit; IsHit:boolean);
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Sound, KM_Units_Warrior, KM_Game;


{ TUnitActionFight }
constructor TUnitActionFight.Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
begin
  Inherited Create(aActionType);
  fActionName     := uan_Fight;
  Locked          := true;
  fOpponent       := aOpponent.GetUnitPointer; //Mark as a used pointer in case the unit dies without us noticing. Remove pointer on destroy
  aUnit.Direction := KMGetDirection(aUnit.GetPosition, fOpponent.GetPosition); //Face the opponent from the beginning
end;


destructor TUnitActionFight.Destroy;
begin
  fPlayers.CleanUpUnitPointer(fOpponent);
  Inherited;
end;


constructor TUnitActionFight.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fOpponent, 4);
end;


procedure TUnitActionFight.SyncLoad();
begin
  Inherited;
  fOpponent := fPlayers.GetUnitByID(cardinal(fOpponent));
end;


procedure TUnitActionFight.MakeSound(KMUnit: TKMUnit; IsHit:boolean);
begin
  //Do not play sounds if unit is invisible to MyPlayer
  if fTerrain.CheckTileRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y, MyPlayer.PlayerID) < 255 then exit;

{  case KMUnit.UnitType of //Various UnitTypes and ActionTypes
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
  end;}
end;


function TUnitActionFight.Execute(KMUnit: TKMUnit):TActionResult;
var Cycle,Step:byte; IsHit: boolean; Damage: word; ut,ot:byte;
begin
  //See if Opponent has walked away (i.e. Serf) or died
  if (fOpponent.IsDeadOrDying) or (GetLength(KMUnit.GetPosition, fOpponent.GetPosition) > TKMUnitWarrior(KMUnit).GetFightRange) then
    Result := ActDone
  else
    Result := ActContinues;

  Cycle := max(UnitSprite[byte(KMUnit.UnitType)].Act[byte(GetActionType)].Dir[byte(KMUnit.Direction)].Count,1);
  Step  := KMUnit.AnimStep mod Cycle;

  //Opponent can walk next to us, keep facing him
  KMUnit.Direction := KMGetDirection(KMUnit.GetPosition, fOpponent.GetPosition);

  if TKMUnitWarrior(KMUnit).GetFightRange >= 2 then begin
    if Step = 5 then
    begin
      MakeSound(KMUnit, true); //2 sounds for hit and for miss
      fGame.fProjectiles.AddItem(KMUnit.PositionF, fOpponent.PositionF, pt_Arrow); //Release arrow/bolt
    end;
  end else begin
    //Melee units place hit on step 5
    if Step = 5 then
    begin
      ut := byte(KMUnit.UnitType);
      ot := byte(fOpponent.UnitType);
      Damage := UnitStat[ut].Attack; //Base damage
      if InRange(ot, low(UnitGroups), high(UnitGroups)) then
        Damage := Damage + UnitStat[ut].AttackHorseBonus * byte(UnitGroups[ot] = gt_Mounted); //Add Anti-horse bonus
      Damage := Damage * (GetDirModifier(KMUnit.Direction,fOpponent.Direction)+1); //Direction modifier
      Damage := Damage div max(UnitStat[ot].Defence,1); //Not needed, but animals have 0 defence

      IsHit := (Damage >= Random(101)); //0..100

      if IsHit then
        fOpponent.HitPointsDecrease;

      MakeSound(KMUnit, IsHit); //2 sounds for hit and for miss
    end;
  end;

  StepDone := KMUnit.AnimStep mod Cycle = 0;
  inc(KMUnit.AnimStep);


  if (fOpponent is TKMUnitWarrior)
     and not (fOpponent.IsDeadOrDying)
     and (GetLength(KMUnit.GetPosition, fOpponent.GetPosition) < 1.5) then
    TKMUnitWarrior(KMUnit).GetCommander.Foe := TKMUnitWarrior(fOpponent); //Set our group's foe to this enemy, thus making it constantly change in large fights so no specific unit will be targeted
    //todo: That isn't very efficient because pointer tracking is constantly changing... should only happen every second or something.
end;


procedure TUnitActionFight.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fOpponent <> nil then
    SaveStream.Write(fOpponent.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
end;




end.
