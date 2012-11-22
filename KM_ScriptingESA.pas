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

  TKMScriptEvents = class
  private
    //We need to collect all events into one batch to be able to process them together
    fCount: Word;
    fItems: array of record
      Trigger: TKMEventTrigger;
      Params: array [0..MAX_PARAMS-1] of Integer;
    end;
  public
    procedure Add(aTrigger: TKMEventTrigger; aParams: array of Integer);
    procedure Clear;
    function Contains(aTrigger: TKMEventTrigger; aParams: array of Integer): Word;
    property Count: Word read fCount;
  published
    { Methods we expose to scripts }
    function HouseBuilt(aPlayer: Integer; aHouseIndex: Integer): Byte;
    function PlayerDefeated(aPlayer: Integer): Boolean;
  end;

  TKMScriptStates = class
  published
    { Methods we expose to scripts }
    function GameTime: Cardinal;
  end;

  TKMScriptActions = class
  published
    { Methods we expose to scripts }
    procedure ShowMsg(aPlayer: Integer; aIndex: Word);
  end;


implementation
uses KM_AI, KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection, KM_TextLibrary;


{ TKMScriptEvents }
procedure TKMScriptEvents.Add(aTrigger: TKMEventTrigger; aParams: array of Integer);
var
  I: Integer;
begin
  if fCount > High(fItems) then
    SetLength(fItems, fCount + 8);
  fItems[fCount].Trigger := aTrigger;
  for I := Low(aParams) to High(aParams) do
    fItems[fCount].Params[I] := aParams[I];
  Inc(fCount);
end;


procedure TKMScriptEvents.Clear;
begin
  fCount := 0;
end;


//See how many times requested Trigger is met in a list,
//we return Count since in rare cases two identical events could be registered
//in one tick. e.g. UnitDied event in a fight
function TKMScriptEvents.Contains(aTrigger: TKMEventTrigger; aParams: array of Integer): Word;
var
  I,K: Integer;
  Match: Boolean;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
  if fItems[I].Trigger = aTrigger then
  begin
    Match := True;
    for K := 0 to High(aParams) do
      Match := Match and (fItems[I].Params[K] = aParams[K]);
    if Match then
      Inc(Result);
  end;
end;


function TKMScriptEvents.HouseBuilt(aPlayer: Integer; aHouseIndex: Integer): Byte;
begin
  //Check house by its KaM index to keep it consistent with DAT scripts
  //Return number in case Player has built several houses at once
  Result := Contains(etHouseBuilt, [aPlayer, aHouseIndex]);
end;


function TKMScriptEvents.PlayerDefeated(aPlayer: Integer): Boolean;
begin
  Result := Contains(etDefeated, [aPlayer]) > 0;
end;


function TKMScriptStates.GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


procedure TKMScriptActions.ShowMsg(aPlayer: Integer; aIndex: Word);
begin
  if aPlayer = MyPlayer.PlayerIndex then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0));
end;


end.
