unit KM_ArmyEvaluation;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils,
  KM_Defaults, KM_PlayerStats;


type
  TKMEvaluation = record
    EnemyIndex: TPlayerIndex; //Evaluation made against this player
    VictoryChance: Single;
    Power: Single;
    UnitTypesPower: array [WARRIOR_MIN .. WARRIOR_MAX] of Single;
  end;


  //This class evaluate self army relatively enemy armies
  TKMArmyEvaluation = class
  private
    fOwner: TPlayerIndex;
    fEvals: array [0 .. MAX_PLAYERS - 1] of TKMEvaluation; //Results of evaluetion

    function GetEvaluation(aIndex: TPlayerIndex): TKMEvaluation;
    procedure Reset;
    procedure EvaluatePower(aEnemyIndex: TPlayerIndex);
  public
    constructor Create(aOwner: TPlayerIndex);
    destructor Destroy; override;

    property Evaluations[aIndex: TPlayerIndex]: TKMEvaluation read GetEvaluation;
    procedure UpdateState; //Call to update evaluation
  end;


procedure InitUnitStatEvals;


implementation
uses Math, KM_PlayersCollection, KM_Resource, KM_ResourceUnit;


var
  //Evals matrix. 1 - Power ratio, 2 - Chance
  UnitPower: array [WARRIOR_MIN .. WARRIOR_MAX, WARRIOR_MIN .. WARRIOR_MAX] of Single;


{ TKMArmyEvaluation }
constructor TKMArmyEvaluation.Create(aOwner: TPlayerIndex);
var I: Integer;
begin
  inherited Create;

  fOwner := aOwner;

  for I := 0 to MAX_PLAYERS - 1 do
    fEvals[I].EnemyIndex := I;
end;


destructor TKMArmyEvaluation.Destroy;
begin

  inherited;
end;


function TKMArmyEvaluation.GetEvaluation(aIndex: TPlayerIndex): TKMEvaluation;
begin
  Result := fEvals[aIndex];
end;


procedure TKMArmyEvaluation.Reset;
begin
  FillChar(fEvals, SizeOf(fEvals), #0);
end;


//Calculate our power against specified player
procedure TKMArmyEvaluation.EvaluatePower(aEnemyIndex: TPlayerIndex);
var
  SelfStats, EnemyStats: TKMPlayerStats;
  Eval: TKMEvaluation;
  I, K: TUnitType;
  EnemyQty, SelfQty: Integer;
  PowerSum: Single;
begin
  SelfStats := fPlayers[fOwner].Stats;
  EnemyStats := fPlayers[aEnemyIndex].Stats;

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

  for I := 0 to fPlayers.Count - 1 do
  if (I <> fOwner) and fPlayers[I].Enabled
  and (fPlayers[fOwner].Alliances[I] = at_Enemy) then
    EvaluatePower(I);
end;


//Calculate unit strength against each other
procedure InitUnitStatEvals;
var
  I, K: TUnitType;
  C1, C2: TKMUnitDATClass;
  HpRatio, DirectPow, OppositePow: Single;
begin
  for I := WARRIOR_MIN to WARRIOR_MAX do
  begin
    C1 := fResource.UnitDat[I];

    for K := WARRIOR_MIN to WARRIOR_MAX do
    begin
      C2 := fResource.UnitDat[K];

      HpRatio := C1.HitPoints / C2.HitPoints;
      DirectPow := C1.Attack + C1.AttackHorse * Byte(UnitGroups[K] = gt_Mounted) / max(C2.Defence, 1);
      OppositePow := C2.Attack + C2.AttackHorse * Byte(UnitGroups[I] = gt_Mounted) / max(C1.Defence, 1);
      UnitPower[I, K] := HpRatio * DirectPow / OppositePow;
    end;
  end;
end;


end.
