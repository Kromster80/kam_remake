unit KM_ScriptingESA;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  uPSCompiler, uPSRuntime,
  KM_CommonClasses, KM_Defaults, KM_Points;

const
  MAX_PARAMS = 4;

type
  TKMEventTrigger = (
    etDefeated,       //[] Certain player has been defeated, we rely on Conditions to generate that event
    etHouseBuilt);    //[House] Certain house was built

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
    procedure Defeat(aPlayer: Integer);
    procedure ShowMsg(aPlayer: Integer; aIndex: Word);
  end;


implementation
uses KM_AI, KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection, KM_TextLibrary;


function TKMScriptStates.GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


procedure TKMScriptActions.Defeat(aPlayer: Integer);
begin
  if InRange(aPlayer, 0, fPlayers.Count - 1) then
    fPlayers[aPlayer].AI.Defeat;
end;


procedure TKMScriptActions.ShowMsg(aPlayer: Integer; aIndex: Word);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0));
end;


end.
