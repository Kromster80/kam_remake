unit KM_ArmyEvaluation;
interface
uses
  Classes, Contnrs, KromUtils,
  KM_Defaults, KM_PlayerStats;


type
  TKMEvaluation = class
  public
    fEmemyIndex : TPlayerIndex; // to make sure on get
    fVictoryChance : Single;
    fPower : Single;
    fUnitTypesPower : array [ut_Militia..ut_Barbarian] of Single;
    constructor Create;
    procedure Reset;
  end;


  // This class evaluate self army relatively enemy armies
  TKMArmyEvaluation = class
  private
    fSelfPlayer : TObject; // SelfPlayer
    fEnemies : TObjectList;
    fEvals : array [0..MAX_PLAYERS-1] of TKMEvaluation; // Results of evaluetion

    function GetEnemiesCount : integer;
    function GetEvaluationByIndex(Index : TPlayerIndex) : TKMEvaluation;
    procedure ResetEvaluation; // Reset to default
    procedure EvaluateChance(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
    procedure EvaluatePower(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
  public
    constructor Create(SelfPlayer : TObject);
    destructor Destroy; override;

    function AddEnemy(Player : TObject) : Integer;
    procedure ClearEnemies;
    function RemoveEnemy(Player : TObject) : Integer;

    procedure UpdateState; // Call to update evaluation

    property EnemiesCount : integer read GetEnemiesCount;
    property Evaluations[Index:TPlayerIndex]:TKMEvaluation read GetEvaluationByIndex;
  end;



procedure InitUnitStatEvals;


implementation
uses Math, KM_Player;

var
  // Evals matrix. 1 - Power ratio, 2 - Chance
  UnitStatEvals : array [ut_Militia..ut_Barbarian, ut_Militia..ut_Barbarian, 1..2] of Single;

{ TKMArmyEvaluation assets}
constructor TKMEvaluation.Create;
begin
  fEmemyIndex := 0;
  Reset;
end;


procedure TKMEvaluation.Reset;
begin
  fVictoryChance := 0.0;
  fPower := 0.0;
  FillChar(fUnitTypesPower, SizeOf(fUnitTypesPower), #0);
end;


{ TKMArmyEvaluation assets}
constructor TKMArmyEvaluation.Create(SelfPlayer : TObject);
var i : Integer;
begin
  inherited Create;

  Assert(SelfPlayer is TKMPlayer);
  fSelfPlayer := SelfPlayer;

  fEnemies := TObjectList.Create(false); // false - Container will not controls the memory. See TObjectList
  for i := 0 to MAX_PLAYERS-1 do
  begin
    fEvals[i] := TKMEvaluation.Create;
    fEvals[i].fEmemyIndex := i;
  end;
end;


destructor TKMArmyEvaluation.Destroy;
var i : Integer;
begin
  for i := 0 to MAX_PLAYERS-1 do
    fEvals[i].Free;
  FreeThenNil(fEnemies);
  inherited;
end;


function TKMArmyEvaluation.GetEnemiesCount : Integer;
begin
  Result := fEnemies.Count;
end;


function TKMArmyEvaluation.GetEvaluationByIndex(Index : TPlayerIndex) : TKMEvaluation;
begin
  Assert(Index <= MAX_PLAYERS-1);
  Result := fEvals[Index];
end;


procedure TKMArmyEvaluation.ResetEvaluation;
var i : TPlayerIndex;
begin
  for i := 0 to MAX_PLAYERS-1 do
    fEvals[i].Reset;
end;


procedure TKMArmyEvaluation.EvaluateChance(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
var
  Res : TKMEvaluation;
begin
  Res := fEvals[PlayerIndex];
  Res.fVictoryChance := 1.0;
end;


procedure TKMArmyEvaluation.EvaluatePower(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
var
  SelfStats : TKMPlayerStats;
  EnemySize : Integer;
  Eval : TKMEvaluation;
  i, j : TUnitType;
  EnemyQty, SelfQty : Integer;
  UnitSum : Single;
begin
  SelfStats := (fSelfPlayer as TKMPlayer).Stats;
  Eval := fEvals[PlayerIndex];
  EnemySize := Stats.GetArmyCount;
  Eval.fPower := 0.0;
  for i := ut_Militia to ut_Barbarian do begin
    SelfQty := SelfStats.GetUnitQty(i);
    if SelfQty = 0 then begin
      Eval.fUnitTypesPower[i] := 0.0;
      continue;
    end;
    UnitSum := 0.0;
    for j := ut_Militia to ut_Barbarian do begin
      EnemyQty := Stats.GetUnitQty(j);
      if EnemyQty <> 0 then
        UnitSum := UnitSum + SelfQty * UnitStatEvals[i,j,1] / EnemyQty * (EnemyQty / EnemySize);
    end;
    Eval.fUnitTypesPower[i] := UnitSum;
    Eval.fPower := Eval.fPower + Eval.fUnitTypesPower[i];
  end;
end;


function TKMArmyEvaluation.AddEnemy(Player : TObject) : Integer;
begin
  Assert(Player is TKMPlayer);
  Assert(fEnemies.Count <= MAX_PLAYERS-1);
  Result := fEnemies.Add(Player);
end;


procedure TKMArmyEvaluation.ClearEnemies;
begin
  fEnemies.Clear;
end;


function TKMArmyEvaluation.RemoveEnemy(Player : TObject) : Integer;
begin
  Assert(Player is TKMPlayer);
  Result := fEnemies.Remove(Player);
end;


procedure TKMArmyEvaluation.UpdateState;
var i : Integer;
    Player : TKMPlayer;
begin
  ResetEvaluation;
  for i := 0 to fEnemies.Count-1 do begin
    Player := TKMPlayer(fEnemies[i]);

    // check evaluating conditions here

    EvaluatePower(Player.Stats, Player.PlayerIndex);
    EvaluateChance(Player.Stats, Player.PlayerIndex);
  end;
end;


procedure InitUnitStatEvals;
var
  i,j : Integer;
  a,b,c : Single;
begin
  for i := Byte(ut_Militia) to Byte(ut_Barbarian) do
    for j := Byte(ut_Militia) to Byte(ut_Barbarian) do begin
      a := UnitStat[i].HitPoints / UnitStat[j].HitPoints;
      b := UnitStat[i].Attack;
      if InRange(j, low(UnitGroups), high(UnitGroups)) then
        b := b + UnitStat[i].AttackHorseBonus * byte(UnitGroups[j] = gt_Mounted);
      b := b / max(UnitStat[j].Defence,1);
      c := UnitStat[j].Attack;
      if InRange(i, low(UnitGroups), high(UnitGroups)) then
        c := c + UnitStat[j].AttackHorseBonus * byte(UnitGroups[i] = gt_Mounted);
      c := c / max(UnitStat[i].Defence,1);
      UnitStatEvals[TUnitType(i), TUnitType(j), 1] := b * a / c;
    end;
end;


end.
