unit KM_ArmyEvaluation;
interface
uses
  Classes, Contnrs, KromUtils,
  KM_Defaults, KM_PlayerStats;


type
  TKMEvaluation = class
  public
    fEnemyIndex : TPlayerIndex; // to make sure on get
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
uses Math, KM_Player, KM_ResourceGFX;


var
  // Evals matrix. 1 - Power ratio, 2 - Chance
  UnitStatEvals : array [ut_Militia..ut_Barbarian, ut_Militia..ut_Barbarian, 1..2] of Single;


{ TKMArmyEvaluation assets}
constructor TKMEvaluation.Create;
begin
  fEnemyIndex := 0;
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

  //Container will not controls the memory.
  //@Crow: Уточни, почему выбран такой подход? Можно ли заменить на TList?
  fEnemies := TObjectList.Create(false); 
  for i := 0 to MAX_PLAYERS-1 do
  begin
    fEvals[i] := TKMEvaluation.Create;
    fEvals[i].fEnemyIndex := i;
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


//@Crow: Пиши пожалуйтс акомментарии по коду (какова цель метода, почему выбрано определенное решение?)
procedure TKMArmyEvaluation.EvaluatePower(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
var
  SelfStats : TKMPlayerStats;
  Eval : TKMEvaluation;
  i, j : TUnitType;
  EnemyQty, SelfQty : Integer;
  PowerSum : Single;
begin
  SelfStats := (fSelfPlayer as TKMPlayer).Stats;
  Eval := fEvals[PlayerIndex];
  Eval.fPower := 0.0;
  for i := ut_Militia to ut_Barbarian do begin
    SelfQty := SelfStats.GetUnitQty(i);
    if SelfQty = 0 then begin
      Eval.fUnitTypesPower[i] := 0.0;
      continue;
    end;
    PowerSum := 0.0;
    for j := ut_Militia to ut_Barbarian do begin
      EnemyQty := Stats.GetUnitQty(j);
      PowerSum := PowerSum + UnitStatEvals[i,j,1] * EnemyQty;
    end;
    if PowerSum = 0 then Eval.fUnitTypesPower[i] := 0.0
    else Eval.fUnitTypesPower[i] := SelfQty / PowerSum;
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
var 
  i : Integer;
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


//@Crow: Попробуй пожалуйста использовать более говорящие имена, чем АБЦ
procedure InitUnitStatEvals;
var
  i,j : TUnitType;
  a,b,c : Single;
begin
  for i := ut_Militia to ut_Barbarian do
    for j := ut_Militia to ut_Barbarian do begin
      a := fResource.UnitDat[i].HitPoints / fResource.UnitDat[j].HitPoints;
      b := fResource.UnitDat[i].Attack;
      if j in [low(UnitGroups) .. high(UnitGroups)] then
        b := b + fResource.UnitDat[i].AttackHorseBonus * byte(UnitGroups[j] = gt_Mounted);
      b := b / max(fResource.UnitDat[j].Defence,1);
      c := fResource.UnitDat[j].Attack;
      if i in [low(UnitGroups) .. high(UnitGroups)] then
        c := c + fResource.UnitDat[j].AttackHorseBonus * byte(UnitGroups[i] = gt_Mounted);
      c := c / max(fResource.UnitDat[i].Defence,1);
      UnitStatEvals[i, j, 1] := b * a / c;
    end;
end;


end.
