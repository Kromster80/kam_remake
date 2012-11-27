unit KM_ScriptingESA;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  KM_CommonClasses, KM_Defaults, KM_Points;

type
  //Two classes exposed to scripting States and Actions

  //All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)

  //How to add new a method exposed to the scripting? Three steps:
  //1. Add method to published section here below
  //2. Add method declaration to Compiler (TKMScripting.ScriptOnUses)
  //3. Add method name to Runtime (TKMScripting.LinkRuntime)
  TKMScriptStates = class
    function GameTime: Cardinal;
  end;

  TKMScriptActions = class
    procedure Defeat(aPlayer: Word);
    procedure GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word);
    procedure ShowMsg(aPlayer, aIndex: Word);
  end;


implementation
uses KM_AI, KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection,
  KM_TextLibrary, KM_ResourceUnit;


function TKMScriptStates.GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


procedure TKMScriptActions.Defeat(aPlayer: Word);
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    fPlayers[aPlayer].AI.Defeat;
end;


procedure TKMScriptActions.GiveGroup(aPlayer, aType, X,Y, aDir, aCount, aColumns: Word);
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1)
  and fTerrain.TileInMapCoords(X,Y)
  and (TKMDirection(aDir+1) in [dir_N..dir_NW]) then
    fPlayers[aPlayer].AddUnitGroup(UnitIndexToType[aType],
                                   KMPoint(X,Y),
                                   TKMDirection(aDir+1),
                                   aColumns,
                                   aCount);
end;


procedure TKMScriptActions.ShowMsg(aPlayer, aIndex: Word);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0));
end;


end.
