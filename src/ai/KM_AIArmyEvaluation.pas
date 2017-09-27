unit KM_AIArmyEvaluation;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults;


type
  TKMEvaluation = record
    EnemyIndex: TKMHandIndex; //Evaluation made against this player
    VictoryChance: Single;
    Power: Single;
    UnitTypesPower: array [WARRIOR_MIN .. WARRIOR_MAX] of Single;
  end;


  //This class evaluate self army relatively enemy armies
  TKMArmyEvaluation = class
  private
    fOwner: TKMHandIndex;
    fEvals: array [0 .. MAX_HANDS - 1] of TKMEvaluation; //Results of evaluetion

    function GetEvaluation(aIndex: TKMHandIndex): TKMEvaluation;
    procedure Reset;
    procedure EvaluatePower(aEnemyIndex: TKMHandIndex);
  public
    constructor Create(aOwner: TKMHandIndex);
    destructor Destroy; override;

    property Evaluations[aIndex: TKMHandIndex]: TKMEvaluation read GetEvaluation;
    procedure UpdateState; //Call to update evaluation
  end;


procedure InitUnitStatEvals;


implementation
uses
  Math,
  KM_Hand, KM_HandsCollection, KM_HandStats,
  KM_Resource, KM_ResUnits;


var
  //Evals matrix. 1 - Power ratio, 2 - Chance
  UnitPower: array [WARRIOR_MIN .. WARRIOR_MAX, WARRIOR_MIN .. WARRIOR_MAX] of Single;


{ TKMArmyEvaluation }
constructor TKMArmyEvaluation.Create(aOwner: TKMHandIndex);
var I: Integer;
begin
  inherited Create;

  fOwner := aOwner;

  for I := 0 to MAX_HANDS - 1 do
    fEvals[I].EnemyIndex := I;
end;


destructor TKMArmyEvaluation.Destroy;
begin

  inherited;
end;


function TKMArmyEvaluation.GetEvaluation(aIndex: TKMHandIndex): TKMEvaluation;
begin
  Result := fEvals[aIndex];
end;


procedure TKMArmyEvaluation.Reset;
begin
  FillChar(fEvals, SizeOf(fEvals), #0);
end;


//Calculate our power against specified player
procedure TKMArmyEvaluation.EvaluatePower(aEnemyIndex: TKMHandIndex);
var
  SelfStats, EnemyStats: TKMHandStats;
  Eval: TKMEvaluation;
  I, K: TUnitType;
  EnemyQty, SelfQty: Integer;
  PowerSum: Single;
begin
  SelfStats := gHands[fOwner].Stats;
  EnemyStats := gHands[aEnemyIndex].Stats;

  Eval := fEvals[aEnemyIndex];
  Eval.Power := 0;
  for I := WARRIOR_MIN to WARRIOR_MAX do
  begin
    SelfQty := SelfStats.GetUnitQty(I);
    if SelfQty = 0 then
    begin
      Eval.UnitTypesPower[I] := 0;
      continue;
    end;
    PowerSum := 0;
    for K := WARRIOR_MIN to WARRIOR_MAX do
    begin
      EnemyQty := EnemyStats.GetUnitQty(K);
      PowerSum := PowerSum + UnitPower[I, K] * EnemyQty;
    end;
    if PowerSum = 0 then
      Eval.UnitTypesPower[I] := 0
    else
      Eval.UnitTypesPower[I] := SelfQty / PowerSum;
    Eval.Power := Eval.Power + Eval.UnitTypesPower[I];
  end;
end;


procedure TKMArmyEvaluation.UpdateState;
var
  I: Integer;
begin
  Reset;

  for I := 0 to gHands.Count - 1 do
  if gHands[I].Enabled
  and (gHands[fOwner].Alliances[I] = at_Enemy) then
    EvaluatePower(I);
end;


//Calculate unit strength against each other
procedure InitUnitStatEvals;
var
  I, K: TUnitType;
  C1, C2: TKMUnitSpec;
  HpRatio, DirectPow, OppositePow: Single;
begin
  for I := WARRIOR_MIN to WARRIOR_MAX do
  begin
    C1 := gRes.Units[I];

    for K := WARRIOR_MIN to WARRIOR_MAX do
    begin
      C2 := gRes.Units[K];

      HpRatio := C1.HitPoints / C2.HitPoints;
      DirectPow := C1.Attack + C1.AttackHorse * Byte(UnitGroups[K] = gt_Mounted) / Max(C2.Defence, 1);
      OppositePow := C2.Attack + C2.AttackHorse * Byte(UnitGroups[I] = gt_Mounted) / Max(C1.Defence, 1);
      UnitPower[I, K] := HpRatio * DirectPow / OppositePow;
    end;
  end;
end;


end.
