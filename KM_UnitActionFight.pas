unit KM_UnitActionFight;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KromUtils, Math, SysUtils, KM_Units;

type TFightType = (ft_Melee, ft_Ranged); //Dunno if we really need it?

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
    procedure Execute(KMUnit: TKMUnit; out DoEnd: Boolean); override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Sound, KM_Game, KM_Units_Warrior;


{ TUnitActionFight }
constructor TUnitActionFight.Create(aActionType:TUnitActionType; aOpponent, aUnit:TKMUnit);
begin
  Inherited Create(aActionType);
  fActionName := uan_Fight;
  fOpponent := aOpponent.GetSelf; //Mark as a used pointer in case the unit dies without us noticing. Remove pointer on destroy
  aUnit.Direction := KMGetDirection(aUnit.GetPosition, fOpponent.GetPosition); //Face the opponent from the beginning
end;


destructor TUnitActionFight.Destroy;
begin
  fOpponent.RemovePointer;
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
  fOpponent := fPlayers.GetUnitByID(integer(fOpponent));
end;


procedure TUnitActionFight.MakeSound(KMUnit: TKMUnit; IsHit:boolean);
begin
  //Do not play sounds if unit is invisible to MyPlayer
  if fTerrain.CheckTileRevelation(KMUnit.GetPosition.X, KMUnit.GetPosition.Y, MyPlayer.PlayerID) < 255 then exit;

{  case KMUnit.GetUnitType of //Various UnitTypes and ActionTypes
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


procedure TUnitActionFight.Execute(KMUnit: TKMUnit; out DoEnd: Boolean);

  function CheckDoEnd:boolean;
  begin
    Result := (fOpponent.GetUnitTask is TTaskDie) or //Unit is Killed
              (GetLength(KMUnit.GetPosition, fOpponent.GetPosition) > 1.5) or //Unit walked away (i.e. Serf)
               fOpponent.IsDead; //unlikely, since unit is already performed TTaskDie
  end;

var Cycle,Step:byte; DirectionModifier:byte; IsHit: boolean; Damage: word;
begin
  DoEnd := CheckDoEnd;
  if DoEnd then
    exit; //e.g. if other unit kills opponent, exit now

  Cycle := max(UnitSprite[byte(KMUnit.GetUnitType)].Act[byte(GetActionType)].Dir[byte(KMUnit.Direction)].Count,1);
  Step  := KMUnit.AnimStep mod Cycle;

  KMUnit.Direction := KMGetDirection(KMUnit.GetPosition, fOpponent.GetPosition); //Always face the opponent

  //Only hit unit on step 5
  if Step = 5 then
  begin
    DirectionModifier := GetDirModifier(KMUnit.Direction,fOpponent.Direction);
    Damage := ((UnitStat[byte(KMUnit.GetUnitType)].Attack+(UnitStat[byte(KMUnit.GetUnitType)].AttackHorseBonus)*byte(UnitGroups[byte(fOpponent.GetUnitType)] = gt_Mounted)) * DirectionModifier)
              div max(UnitStat[byte(fOpponent.GetUnitType)].Defence,1); //Not needed, but animals have 0 defence

    IsHit := (Damage >= Random(101)); //0..100

    if IsHit then
      fOpponent.HitPointsDecrease;

    MakeSound(KMUnit, IsHit); //2 sounds for hit and for miss
  end;

  IsStepDone := KMUnit.AnimStep mod Cycle = 0;
  inc(KMUnit.AnimStep);

  DoEnd := CheckDoEnd;
  if (not DoEnd) and (fOpponent is TKMUnitWarrior) then
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
