unit KM_TPlayer;
{$I ..\KaM_Remake.inc}

interface

uses KM_Defaults, SysUtils, KM_Unit1, KromUtils;

type
  TPlayer = class
  private
    fHouseCount: integer;
    fRoadCount: integer;
    fClearUpCount: integer;
  public
    House: array of record
      PosX, PosY: integer;
      Kind: byte;
      Damage: integer;
    end;

    constructor Create;
    property HouseCount: integer read fHouseCount;
    property RoadCount: integer read fRoadCount;
    property ClearUpCount: integer read fClearUpCount;
    procedure AddHouse(HouseID, PosX, PosY: integer);
    procedure RemHouse(HouseID: integer);
    function HitTest(PosX, PosY: integer): integer;
    function GetAllHouseStrings: string;
  end;

TGroundProp = (gpN = 0, gpR = 1, gpF = 2, gpW = 3); //None-Road-Farm-Wine

type
  TMission = class
  public
    Player: array [1 .. 8] of TPlayer;
    Roads: array [1 .. MaxMapSize, 1 .. MaxMapSize] of TGroundProp;
    Owner: array [1 .. MaxMapSize, 1 .. MaxMapSize] of byte;
    ActivePlayer: integer;
    SetMapFile: string;
    SetHumanPlayer: integer;
    constructor Create();
    procedure RemHouse(PosX, PosY: integer);
    procedure RemRoad(PosX, PosY: integer);
    procedure AddRoad(PosX, PosY, _Owner: integer; aGP: TGroundProp);
    function GetAllRoadStrings(_Owner: integer): string;
  end;

var
  Mission: TMission;

implementation

constructor TPlayer.Create();
var
  i: integer;
begin
  fHouseCount := 0;
  fClearUpCount := 0;
end;

procedure TPlayer.AddHouse(HouseID, PosX, PosY: integer);
var
  i, k: integer;
begin
  for i := 0 to 3 do
    for k := 0 to 3 do
    begin
      if HousePlanYX[HouseID, i + 1, k + 1] <> 0 then
        if HitTest(PosX - 3 + k, PosY - 3 + i) <> 0 then
          exit;
    end;
  inc(fHouseCount);
  setlength(House, fHouseCount + 1);
  House[fHouseCount].PosX := PosX;
  House[fHouseCount].PosY := PosY;
  House[fHouseCount].Kind := HouseID;
  House[fHouseCount].Damage := 0;
end;

procedure TPlayer.RemHouse(HouseID: integer);
var
  i: integer;
begin
  for i := HouseID to HouseCount - 1 do
    House[i] := House[i + 1];
  dec(fHouseCount);
  setlength(House, fHouseCount + 1);
end;

function TPlayer.HitTest(PosX, PosY: integer): integer;
var
  k: integer;
begin
  Result := 0;
  for k := 1 to HouseCount do
    if (PosX - House[k].PosX + 4 in [1 .. 4]) and (PosY - House[k].PosY + 4 in [1 .. 4]) then
      if HousePlanYX[House[k].Kind, PosY - House[k].PosY + 4, PosX - House[k].PosX + 4] <> 0 then
        Result := k;
end;

function TPlayer.GetAllHouseStrings(): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to HouseCount do
    Result := Result + '!SET_HOUSE ' + IntToStr(House[i].Kind - 1) + ' ' + IntToStr(House[i].PosX - 1) + ' ' +
      IntToStr(House[i].PosY - 1) + EolA;
end;

constructor TMission.Create();
var
  i: integer;
begin
  ActivePlayer := 1;
  for i := 1 to 8 do
    Player[i] := TPlayer.Create;
end;

//Remove house by given X,Y
//Check all players houses if they are in a way
procedure TMission.RemHouse(PosX, PosY: integer);
var
  i, k: integer;
begin
  for i := 1 to 8 do
    for k := 1 to Player[i].HouseCount do
      if (PosX - Player[i].House[k].PosX + 3 in [1 .. 4]) and (PosY - Player[i].House[k].PosY + 4 in [1 .. 4]) then
        if HousePlanYX[Player[i].House[k].Kind, PosY - Player[i].House[k].PosY + 4,
          PosX - Player[i].House[k].PosX + 3] <> 0 then
          Player[i].RemHouse(k);
end;

//Remove road by given X,Y
procedure TMission.RemRoad(PosX, PosY: integer);
begin
  Roads[PosX, PosY] := gpN;
  Owner[PosX, PosY] := 0;
end;

procedure TMission.AddRoad(PosX, PosY, _Owner: integer; aGP: TGroundProp);
begin
  if (PosX = 0) or (PosY = 0) then
    exit;
  Roads[PosX, PosY] := aGP;
  Owner[PosX, PosY] := _Owner;
end;

function TMission.GetAllRoadStrings(_Owner: integer): string;
var
  i, k, Num: integer;
begin
  Result := '';
  Num := 0;
  for i := 1 to Map.Y do
    for k := 1 to Map.X do
      if (Roads[k, i] <> gpN) and (Owner[k, i] = _Owner) then
      begin
        if Roads[k, i] = gpR then
          Result := Result + '!SET_STREET ' + IntToStr(k - 1) + ' ' + IntToStr(i - 1) + ' ';
        if Roads[k, i] = gpF then
          Result := Result + '!SET_FIELD ' + IntToStr(k - 1) + ' ' + IntToStr(i - 1) + ' ';
        if Roads[k, i] = gpW then
          Result := Result + '!SET_WINEFIELD ' + IntToStr(k - 1) + ' ' + IntToStr(i - 1) + ' ';
        inc(Num);
        if Num mod 4 = 0 then
          Result := Result + EolA;
      end;
end;

end.
