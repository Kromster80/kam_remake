unit KM_Goals;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonTypes, KM_Defaults;


type
  TKMGoal = packed record
    GoalType: TGoalType; //Victory, survive, neither
    GoalCondition: TGoalCondition; //Buildings, troops, time passing
    GoalStatus: TGoalStatus; //Must this condition be true or false (same as alive or dead) for victory/surival to occour?
    GoalTime: cardinal; //Only used with ga_Time. Amount of time (in game ticks) that must pass before this goal is complete
    MessageToShow: integer; //Message to be shown when the goal is completed
    MessageHasShown: boolean; //Whether we have shown this message yet
    PlayerIndex: shortint; //Player whose buildings or troops must be destroyed
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
    fCount:integer;
    fGoals:array of TKMGoal;
    function GetGoal(Index:integer):TKMGoal;
  public
    constructor Create;
    destructor Destroy; override;

    property Count:integer read fCount;
    property Item[Index:integer]:TKMGoal read GetGoal; default;
    procedure AddGoal(aGoalType: TGoalType; aGoalCondition: TGoalCondition; aGoalStatus: TGoalStatus; aGoalTime: cardinal; aMessageToShow: integer; aPlayerIndex: shortint);
    procedure RemGoal(aIndex:integer);
    procedure RemoveReference(aPlayerIndex:TPlayerIndex);
    procedure SetMessageHasShown(aIndex:integer);

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;

implementation


{ TKMGoals }
constructor TKMGoals.Create;
begin
  Inherited
  //
end;


destructor TKMGoals.Destroy;
begin
  //
  Inherited;
end;


function TKMGoals.GetGoal(Index:integer):TKMGoal;
begin
  Result := fGoals[Index];
end;


procedure TKMGoals.AddGoal(aGoalType: TGoalType; aGoalCondition: TGoalCondition; aGoalStatus: TGoalStatus; aGoalTime: cardinal; aMessageToShow: integer; aPlayerIndex: shortint);
begin
  SetLength(fGoals,fCount+1);
  fGoals[fCount].GoalType         := aGoalType;
  fGoals[fCount].GoalCondition    := aGoalCondition;
  fGoals[fCount].GoalStatus       := aGoalStatus;
  fGoals[fCount].GoalTime         := aGoalTime;
  fGoals[fCount].MessageToShow    := aMessageToShow;
  fGoals[fCount].PlayerIndex      := aPlayerIndex;
  fGoals[fCount].MessageHasShown  := false;
  inc(fCount);
end;


procedure TKMGoals.RemGoal(aIndex:integer);
var i:integer; //Must be an integer so it doesn't crash when fCount=1
begin
  for i := aIndex to fCount-2 do
    fGoals[i] := fGoals[i+1]; //shift remaining items
  dec(fCount);
  setlength(fGoals, fCount);
end;


//We don't want anyones goal to use deleted player
//Used when we delete certain player from the network game right after init
procedure TKMGoals.RemoveReference(aPlayerIndex:TPlayerIndex);
var i:integer;
begin
  for i:=fCount-1 downto 0 do
    if fGoals[i].PlayerIndex > aPlayerIndex then
      fGoals[i].PlayerIndex := pred(fGoals[i].PlayerIndex)
    else
    if fGoals[i].PlayerIndex = aPlayerIndex then
      RemGoal(i);
end;


procedure TKMGoals.SetMessageHasShown(aIndex:integer);
begin
  fGoals[aIndex].MessageHasShown := true;
end;


procedure TKMGoals.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write(fCount);
  for i:=0 to fCount-1 do
    SaveStream.Write(fGoals[i], SizeOf(fGoals[i]));
end;


procedure TKMGoals.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  LoadStream.Read(fCount);
  SetLength(fGoals, fCount);
  for i:=0 to fCount-1 do
    LoadStream.Read(fGoals[i], SizeOf(fGoals[i]));
end;


end.
