unit KM_UnitActionFight;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KromUtils, Math, SysUtils, KM_Units;

type TFightType = (ft_Melee, ft_Ranged); //Dunno if we really need it?

{Stay in place for set time}
type
TUnitActionFight = class(TUnitAction)
  private
    fOpponent:TKMUnit; //Who we are fighting with
  public
    constructor Create(aActionType:TUnitActionType; aOpponent:TKMUnit);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad(); override;
    procedure MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
    procedure Execute(KMUnit: TKMUnit; out DoEnd: Boolean); override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_SoundFX;


{ TUnitActionFight }
constructor TUnitActionFight.Create(aActionType:TUnitActionType; aOpponent:TKMUnit);
begin
  Inherited Create(aActionType);
  fOpponent := aOpponent;
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


procedure TUnitActionFight.MakeSound(KMUnit: TKMUnit; Cycle,Step:byte);
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


procedure TUnitActionFight.Execute(KMUnit: TKMUnit; out DoEnd: Boolean);
var Cycle,Step:byte;
begin

  Cycle := max(UnitSprite[byte(KMUnit.GetUnitType)].Act[byte(GetActionType)].Dir[byte(KMUnit.Direction)].Count,1);
  Step  := KMUnit.AnimStep mod Cycle;

  MakeSound(KMUnit, Cycle, Step);

  if Step = 5 then fOpponent.HitPointsDecrease(1 + random(5)); //todo: put real formula here

  IsStepDone := KMUnit.AnimStep mod Cycle = 0;
  inc(KMUnit.AnimStep);

  DoEnd := (fOpponent.GetUnitTask is TTaskDie) or //Unit is Killed
  //         (GetLength(fUnit.GetPosition, fOpponent.GetPosition) > 1.5) or //Unit walked away (i.e. Serf)
           (fOpponent.GetHitPoints<=0) or //same as Killed?
           fOpponent.IsDead; //unlikely, since unit is already performed TTaskDie

end;


procedure TUnitActionFight.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  if fOpponent <> nil then
    SaveStream.Write(fOpponent.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
end;




end.
