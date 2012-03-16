unit KM_Goals;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonClasses, KM_Defaults;


type
  TKMGoal = packed record
    GoalType: TGoalType; //Victory, survive, neither
    GoalCondition: TGoalCondition; //Buildings, troops, time passing
    GoalStatus: TGoalStatus; //Must this condition be true or false (same as alive or dead) for victory/surival to occour?
    GoalTime: Cardinal; //Only used with ga_Time. Amount of time (in game ticks) that must pass before this goal is complete
    MessageToShow: Integer; //Message to be shown when the goal is completed
    MessageHasShown: Boolean; //Whether we have shown this message yet
    PlayerIndex: ShortInt; //Player whose buildings or troops must be destroyed
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
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Item[aIndex: Integer]: TKMGoal read GetGoal; default;
    procedure AddGoal(aType: TGoalType; aCondition: TGoalCondition; aStatus: TGoalStatus; aTime: Cardinal; aMessageToShow: Integer; aPlayerIndex: ShortInt);
    procedure RemGoal(aIndex: Integer);
    procedure RemoveReference(aPlayerIndex: TPlayerIndex);
    procedure SetMessageHasShown(aIndex: Integer);
    procedure AddDefaultMPGoals(aBuildings: Boolean; aOurPlayerIndex: TPlayerIndex; aEnemyIndexes: array of TPlayerIndex);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure ExportMessages(aPath: string);
  end;


implementation


{ TKMGoals }
constructor TKMGoals.Create;
begin
  inherited

end;


destructor TKMGoals.Destroy;
begin

  inherited;
end;


function TKMGoals.GetGoal(aIndex: Integer): TKMGoal;
begin
  Result := fGoals[aIndex];
end;


procedure TKMGoals.AddGoal(aType: TGoalType; aCondition: TGoalCondition; aStatus: TGoalStatus; aTime: Cardinal; aMessageToShow: Integer; aPlayerIndex: ShortInt);
begin
  SetLength(fGoals, fCount + 1);
  fGoals[fCount].GoalType := aType;
  fGoals[fCount].GoalCondition := aCondition;
  fGoals[fCount].GoalStatus := aStatus;
  fGoals[fCount].GoalTime := aTime;
  fGoals[fCount].MessageToShow := aMessageToShow;
  fGoals[fCount].PlayerIndex := aPlayerIndex;
  fGoals[fCount].MessageHasShown := False;
  Inc(fCount);
end;


procedure TKMGoals.RemGoal(aIndex: Integer);
begin
  if aIndex <> fCount then
    Move(fGoals[aIndex + 1], fGoals[aIndex], (fCount - 1 - aIndex) * SizeOf(TKMGoal));
  Dec(fCount);
  SetLength(fGoals, fCount);
end;


//We don't want anyones goal to use deleted player
//Used when we delete certain player from MapEd
procedure TKMGoals.RemoveReference(aPlayerIndex: TPlayerIndex);
var
  I: Integer;
begin
  for I := fCount - 1 downto 0 do
    if fGoals[I].PlayerIndex > aPlayerIndex then
      fGoals[I].PlayerIndex := pred(fGoals[I].PlayerIndex)
    else if fGoals[I].PlayerIndex = aPlayerIndex then
      RemGoal(I);
end;


procedure TKMGoals.SetMessageHasShown(aIndex: Integer);
begin
  fGoals[aIndex].MessageHasShown := True;
end;


procedure TKMGoals.AddDefaultMPGoals(aBuildings: Boolean; aOurPlayerIndex: TPlayerIndex; aEnemyIndexes: array of TPlayerIndex);
var
  I: Integer;
  GoalCondition: TGoalCondition;
begin
  if aBuildings then
    GoalCondition := gc_Buildings
  else
    GoalCondition := gc_Troops;
  //Defeat condition
  AddGoal(glt_Survive, GoalCondition, gs_True, 0, 0, aOurPlayerIndex);
  //Victory conditions
  for I := 0 to Length(aEnemyIndexes) - 1 do
    AddGoal(glt_Victory, GoalCondition, gs_False, 0, 0, aEnemyIndexes[I]);
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
procedure TKMGoals.ExportMessages(aPath: string);
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
