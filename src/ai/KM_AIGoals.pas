unit KM_AIGoals;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults;


type
  TKMGoal = packed record
    GoalType: TGoalType; //Victory, survive, neither
    GoalCondition: TGoalCondition; //Buildings, troops, time passing
    GoalStatus: TGoalStatus; //Must this condition be true or false (same as alive or dead) for victory/surival to occur?
    GoalTime: Cardinal; //Only used with ga_Time. Amount of time (in game ticks) that must pass before this goal is complete
    MessageToShow: Integer; //Message to be shown when the goal is completed
    MessageHasShown: Boolean; //Whether we have shown this message yet
    HandIndex: TKMHandIndex; //Player whose buildings or troops must be destroyed
    Disabled: Boolean;
  end;
  //Because the goal system is hard to understand, here are some examples:
  {Destroy troops of player 2 in order to win
  Script command: !ADD_GOAL 4 1 0 2
  GoalType=glt_Victory
  GoalCondition=gc_Troops
  GoalStatus=gs_False         //Troops must be dead, non-existant. i.e. the condition that player 2 has troops must be FALSE.
  Player=play_2
  }

  {Save (protect) troops of player 1 or else you will lose the game
  Script command: !ADD_LOST_GOAL 4 0 0 1
  GoalType=glt_Survive
  GoalCondition=gc_Troops
  GoalStatus=gs_True         //Troops must be alive. i.e. the condition that player 1 has troops must be TRUE otherwise you lose.
  Player=play_1
  }

  {Display message 500 after 10 minutes (no goal, just message)
  Script command: !ADD_GOAL 2 0 500 600
  GoalType=glt_None
  GoalCondition=gc_Time
  GoalStatus=gs_True      //Time must have passed
  GoalTime=600
  MessageToShow=500
  }

  {Display message 510 upon buildings of player 4 being destroyed
  Script command: !ADD_GOAL 3 1 510 4 (in this case the script command would also require the buildings to be destroyed for a victory condition, as mentioned bellow)
  GoalType=glt_None            //If this was set to victory or survive not only would the message be displayed, but it would also be a condition for winning/losing
  GoalCondition=gc_Buildings
  GoalStatus=gs_False         //Buildings must be destroyed
  MessageToShow=500
  }


type
  TKMGoals = class
  private
    fCount: Integer;
    fGoals: array of TKMGoal;
    function GetGoal(aIndex: Integer): TKMGoal;
    procedure SetGoal(aIndex: Integer; const Value: TKMGoal);
  public
    procedure Clear;
    property Count: Integer read fCount;
    property Item[aIndex: Integer]: TKMGoal read GetGoal write SetGoal; default;
    procedure AddGoal(aType: TGoalType; aCondition: TGoalCondition; aStatus: TGoalStatus; aTime: Cardinal; aMessageToShow: Integer; aHandIndex: TKMHandIndex); overload;
    procedure AddGoal(aGoal: TKMGoal); overload;
    procedure Delete(aIndex: Integer);
    procedure RemoveReference(aHandIndex: TKMHandIndex);
    procedure DisableGoalsForHand(aHandIndex: TKMHandIndex);
    procedure SetMessageHasShown(aIndex: Integer);
    procedure AddDefaultGoals(aBuildings: Boolean; aOurPlayerIndex: TKMHandIndex; const aEnemyIndexes: array of TKMHandIndex);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure ExportMessages(const aPath: UnicodeString);
  end;


implementation
uses
  Classes, SysUtils, Math;


{ TKMGoals }
function TKMGoals.GetGoal(aIndex: Integer): TKMGoal;
begin
  Result := fGoals[aIndex];
end;


procedure TKMGoals.SetGoal(aIndex: Integer; const Value: TKMGoal);
begin
  fGoals[aIndex] := Value;
end;


procedure TKMGoals.AddGoal(aType: TGoalType; aCondition: TGoalCondition; aStatus: TGoalStatus; aTime: Cardinal; aMessageToShow: Integer; aHandIndex: TKMHandIndex);
begin
  SetLength(fGoals, fCount + 1);
  fGoals[fCount].GoalType := aType;
  fGoals[fCount].GoalCondition := aCondition;
  fGoals[fCount].GoalStatus := aStatus;
  fGoals[fCount].GoalTime := aTime;
  fGoals[fCount].MessageToShow := aMessageToShow;
  fGoals[fCount].HandIndex := aHandIndex;
  fGoals[fCount].MessageHasShown := False;
  fGoals[fCount].Disabled := False;
  Inc(fCount);
end;


procedure TKMGoals.AddGoal(aGoal: TKMGoal);
begin
  if fCount >= Length(fGoals) then
    SetLength(fGoals, fCount + 16);

  fGoals[fCount] := aGoal;
  Inc(fCount);
end;


procedure TKMGoals.Clear;
begin
  fCount := 0;
end;


procedure TKMGoals.Delete(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, Count - 1));

  if (aIndex <> Count - 1) then
    Move(fGoals[aIndex + 1], fGoals[aIndex], (fCount - 1 - aIndex) * SizeOf(TKMGoal));

  Dec(fCount);
end;


procedure TKMGoals.DisableGoalsForHand(aHandIndex: TKMHandIndex);
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    if fGoals[I].HandIndex = aHandIndex then
      fGoals[I].Disabled := True;
end;


// We don't want anyones goal to use deleted player
// Used when we delete certain player from MapEd
procedure TKMGoals.RemoveReference(aHandIndex: TKMHandIndex);
var
  I: Integer;
begin
  for I := fCount - 1 downto 0 do
    if fGoals[I].HandIndex > aHandIndex then
      fGoals[I].HandIndex := pred(fGoals[I].HandIndex)
    else if fGoals[I].HandIndex = aHandIndex then
      Delete(I);
end;


procedure TKMGoals.SetMessageHasShown(aIndex: Integer);
begin
  fGoals[aIndex].MessageHasShown := True;
end;


procedure TKMGoals.AddDefaultGoals(aBuildings: Boolean; aOurPlayerIndex: TKMHandIndex; const aEnemyIndexes: array of TKMHandIndex);
var
  I: Integer;
  gc: TGoalCondition;
begin
  if aBuildings then
    gc := gc_Buildings
  else
    gc := gc_Troops;

  // Default Defeat condition is to lose army/town
  AddGoal(glt_Survive, gc, gs_True, 0, 0, aOurPlayerIndex);

  // Default Victory conditions is to kill armies / destroy towns of all other players
  for I := 0 to Length(aEnemyIndexes) - 1 do
    AddGoal(glt_Victory, gc, gs_False, 0, 0, aEnemyIndexes[I]);
end;


procedure TKMGoals.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    SaveStream.Write(fGoals[I], SizeOf(fGoals[I]));
end;


procedure TKMGoals.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.Read(fCount);
  SetLength(fGoals, fCount);
  for I := 0 to fCount - 1 do
    LoadStream.Read(fGoals[I], SizeOf(fGoals[I]));
end;


//In-house method to convert KaM 'show_message' goals into EVT scripts
procedure TKMGoals.ExportMessages(const aPath: UnicodeString);
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;

  for I := 0 to fCount - 1 do
  if Item[I].MessageToShow > 0 then
    SL.Add('TIME -1 ' +
           IntToStr(Item[I].GoalTime) +
           ' SHOW_MESSAGE 0 ' +
           IntToStr(Item[I].MessageToShow)); //-529 for TSK, -549 for TPR

  if SL.Count > 0 then
    SL.SaveToFile(aPath);
  SL.Free;
end;


end.
