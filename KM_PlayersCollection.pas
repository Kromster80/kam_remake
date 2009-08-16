unit KM_PlayersCollection;
interface
uses
  Windows, Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_Defaults, KM_Settings, KM_Player, KM_PlayerAI;

type
  TMissionMode = (mm_Normal, mm_Tactic);

type
  TKMAllPlayers = class
  private
    fPlayerCount:integer;
  public
    Player:array[1..MAX_PLAYERS] of TKMPlayerAssets;
    PlayerAI:array[1..MAX_PLAYERS] of TKMPlayerAI;
    //SelectedHouse: TKMHouse;
    //SelectedUnit: TKMUnit;
    Selected: TObject;
  public
    constructor Create(PlayerCount:integer);
    destructor Destroy; override;
  public
    property PlayerCount:integer read fPlayerCount;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function GetUnitCount():integer;
  public
    procedure UpdateState(Tick:cardinal);
    procedure Paint;
  end;

var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player
  MissionMode: TMissionMode;

  
implementation
uses KM_CommonTypes;


{TKMAllPlayers}
constructor TKMAllPlayers.Create(PlayerCount:integer);
var i:integer;
begin
  fLog.AssertToLog(InRange(PlayerCount,1,MAX_PLAYERS),'PlayerCount exceeded');

  fPlayerCount:=PlayerCount; //Used internally
  for i:=1 to fPlayerCount do begin
    Player[i]:=TKMPlayerAssets.Create(TPlayerID(i));
    PlayerAI[i]:=TKMPlayerAI.Create(Player[i]);
  end;
end;

destructor TKMAllPlayers.Destroy;
var i:integer;
begin
  for i:=1 to fPlayerCount do begin
    FreeAndNil(Player[i]);
    FreeAndNil(PlayerAI[i]);
  end;

  MyPlayer:=nil;
  Selected:=nil;
  inherited;
end;

function TKMAllPlayers.HousesHitTest(X, Y: Integer): TKMHouse;
var i:integer;
begin
  Result:=nil;
  for i:=1 to fPlayerCount do begin
    Result:= Player[i].HousesHitTest(X,Y);
    if Result<>nil then Break; //else keep on testing
  end;
end;


function TKMAllPlayers.UnitsHitTest(X, Y: Integer): TKMUnit;
var i:integer;
begin
  Result:=nil;
  for i:=1 to fPlayerCount do begin
    Result:= Player[i].UnitsHitTest(X,Y);
    if Result<>nil then Break; //else keep on testing
  end;
end;


//Get total unit count
function TKMAllPlayers.GetUnitCount():integer;
var i:integer;
begin
  Result:=0;
  for i:=1 to fPlayerCount do
    inc(Result,Player[i].GetUnitCount);
end;


procedure TKMAllPlayers.UpdateState(Tick:cardinal);
var i:word;
begin
  for i:=1 to fPlayerCount do
    Player[i].UpdateState;

  //This is not ajoined with previous loop since it can result in StopGame which flushes all data
  for i:=1 to fPlayerCount do
    if (Tick+i) mod 20 = 0 then //Do only one player per Tick
      PlayerAI[i].UpdateState;
end;


procedure TKMAllPlayers.Paint;
var i:integer;
begin
  for i:=1 to fPlayerCount do
    Player[i].Paint;
end;




end.
