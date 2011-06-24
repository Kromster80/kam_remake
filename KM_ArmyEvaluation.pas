unit KM_ArmyEvaluation;

interface

uses
  Classes, Contnrs, KromUtils,
  KM_Defaults, KM_PlayerStats;

type

  TKMEvaluation = class
  private
  protected
  public

    fEmemyIndex : TPlayerIndex; // to make sure on get
    fVictoryChance : Single;
    fPower : Single;
    fUnitTypesPower : array [ut_Militia..ut_Barbarian] of Single;

    constructor Create;

    procedure Reset;
    
  end;

  // This class evaluate self army relatively enemy armies
  TKMArmyEvaluation = class (TObject)
  private

    fSelfPlayer : TObject; // SelfPlayer
    fEnemies : TObjectList;
    fEvals : array [0..MAX_PLAYERS] of TKMEvaluation; // Results of evaluetion (PKMEvaluation)

    function GetEnemiesCount : integer;                              
    function GetEvaluationByIndex(Index : TPlayerIndex) : TKMEvaluation;
    procedure ResetEvaluation; // Reset to default                            
    procedure EvaluateChance(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);
    procedure EvaluatePower(Stats : TKMPlayerStats; PlayerIndex : TPlayerIndex);

  protected
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

implementation

uses KM_Player;

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
  if not(SelfPlayer is TKMPlayer) then fSelfPlayer := nil
  else fSelfPlayer := SelfPlayer;
  fEnemies := TObjectList.Create(false); // false - Container will not controls the memory. See TObjectList
  for i := 0 to MAX_PLAYERS do begin
    fEvals[i] := TKMEvaluation.Create;
    fEvals[i].fEmemyIndex := i;
  end;
end;

destructor TKMArmyEvaluation.Destroy;
var i : Integer;
begin
  for i := 0 to MAX_PLAYERS do
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
  Assert(Index <= MAX_PLAYERS);
  Result := fEvals[Index];
end;

procedure TKMArmyEvaluation.ResetEvaluation;
var i : TPlayerIndex;
begin
  for i := 0 to MAX_PLAYERS do
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
  Res : TKMEvaluation;
begin
  Res := fEvals[PlayerIndex];
  Res.fPower := 1.0;
end;

function TKMArmyEvaluation.AddEnemy(Player : TObject) : Integer;
begin
  Result := -1;
  if not(Player is TKMPlayer) then exit; 
  if fEnemies.Count >= MAX_PLAYERS then exit;
  fEnemies.Add(Player);
end;

procedure TKMArmyEvaluation.ClearEnemies;
begin
  fEnemies.Clear;
end;

function TKMArmyEvaluation.RemoveEnemy(Player : TObject) : Integer;
begin
  Result := -1;
  if not(Player is TKMPlayer) then exit;
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

end.
