unit KM_PlayersCollection;
interface
uses
  Windows, Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_Houses, KM_Defaults, KM_Player, KM_PlayerAI, KM_Utils;

type
  TMissionMode = (mm_Normal, mm_Tactic);

type
  TKMAllPlayers = class
  private
    fPlayerCount:integer;
  public
    Player:array[1..MAX_PLAYERS] of TKMPlayerAssets;
    PlayerAI:array[1..MAX_PLAYERS] of TKMPlayerAI;
    PlayerAnimals: TKMPlayerAnimals;
    Selected: TObject;
  public
    constructor Create(PlayerCount:integer);
    destructor Destroy; override;
  public
    property PlayerCount:integer read fPlayerCount;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function HitTest(X, Y: Integer):boolean;
    function GetUnitCount():integer;
    function FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
  public
    procedure Save(SaveStream:TMemoryStream);
    procedure Load;
    procedure UpdateState(Tick:cardinal);
    procedure Paint;
  end;

var
  fPlayers: TKMAllPlayers;
  MyPlayer: TKMPlayerAssets; //shortcut to access players player
  MissionMode: TMissionMode;

  
implementation
uses KM_CommonTypes, KM_Terrain;


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
  PlayerAnimals:=TKMPlayerAnimals.Create;
end;

destructor TKMAllPlayers.Destroy;
var i:integer;
begin
  for i:=1 to fPlayerCount do begin
    FreeAndNil(Player[i]);
    FreeAndNil(PlayerAI[i]);
  end;
  FreeAndNil(PlayerAnimals);

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


{HitTest for houses/units altogether}
function TKMAllPlayers.HitTest(X, Y: Integer):boolean;
var H:TKMHouse;
begin
  //Houses have priority over units, so you can't select an occupant.
  //However, this is only true if the house is built
  H := MyPlayer.HousesHitTest(CursorXc, CursorYc);

  if (H<>nil)and(H.GetBuildingState in [hbs_Stone,hbs_Done]) then
    fPlayers.Selected := H
  else
    fPlayers.Selected := MyPlayer.UnitsHitTest(CursorXc, CursorYc);
  if fPlayers.Selected = nil then
    fPlayers.Selected := H;

  Result := fPlayers.Selected <> nil;
end;


//Get total unit count
function TKMAllPlayers.GetUnitCount():integer;
var i:integer;
begin
  Result:=0;
  for i:=1 to fPlayerCount do
    inc(Result,Player[i].GetUnitCount);
end;


{Should return closest position where unit can be placed}
function TKMAllPlayers.FindPlaceForUnit(PosX,PosY:integer; aUnitType:TUnitType):TKMPoint;
var
  aPass:TPassability; //temp for required passability
  Span:integer; //Span length
  X,Y:integer; //Temp position
  mDir:TMoveDirection; //Direction to test
  i:integer;
  function TryOut(aX,aY:integer):boolean;
  begin
    Result:= fTerrain.TileInMapCoords(aX,aY) and fTerrain.CheckPassability(KMPoint(aX,aY),aPass) and (not fTerrain.HasUnit(KMPoint(aX,aY)));
  end;
begin
  if aUnitType in [ut_Wolf..ut_Duck] then
    aPass:=AnimalTerrain[byte(aUnitType)]
  else
    aPass:=canWalk;

  if TryOut(PosX,PosY) then begin
    Result:=KMPoint(PosX,PosY);
    exit;
  end;

  //Should swirl around input point
  Span:=1; X:=PosX; Y:=PosY; mDir:=TMoveDirection(3);
  repeat
    mDir:=TMoveDirection((byte(mDir)+1)mod 4); //wrap around
    case mDir of
      mdPosX: for i:=X+1 to     X+Span do begin inc(X); if TryOut(X,Y) then break; end;
      mdPosY: for i:=Y+1 to     Y+Span do begin inc(Y); if TryOut(X,Y) then break; end;
      mdNegX: for i:=X-1 downto X-Span do begin dec(X); if TryOut(X,Y) then break; end;
      mdNegY: for i:=Y-1 downto Y-Span do begin dec(Y); if TryOut(X,Y) then break; end;
    end;
    if mDir in [mdPosY,mdNegY] then inc(Span); //increase span every second turn
  until(TryOut(X,Y) or (Span=10)); //Catch the end

  Result:=KMPoint(X,Y);
end;


procedure TKMAllPlayers.Save(SaveStream:TMemoryStream);
var i:word;
begin
  SaveStream.Write('Players',7);
  SaveStream.Write(fPlayerCount,4);
  for i:=1 to fPlayerCount do
  begin
    Player[i].Save(SaveStream);
    PlayerAI[i].Save(SaveStream); //Saves AI stuff
  end;
  PlayerAnimals.Save(SaveStream); //Saves AI stuff
end;


procedure TKMAllPlayers.Load;
begin
  //Load
end;


procedure TKMAllPlayers.UpdateState(Tick:cardinal);
var i:word;
begin
  for i:=1 to fPlayerCount do
    Player[i].UpdateState;
  PlayerAnimals.UpdateState;

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
  PlayerAnimals.Paint;
end;




end.
