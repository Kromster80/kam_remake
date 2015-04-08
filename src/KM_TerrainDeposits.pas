unit KM_TerrainDeposits;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_Defaults, KM_Points, KM_Terrain, KM_Units;


type
  TRawDeposit = (rdStone, rdCoal, rdIron, rdGold, rdFish);

const
  DEPOSIT_COLORS: array[TRawDeposit] of Cardinal = (
    $FFBFBFBF, //rdStone - gray
    $FF606060, //rdCoal - black
    $FFBF4040, //rdIron - iron
    $FF00FFFF, //rdGold - gold
    $FFE3BB5B  //rdFish - light blue
  );

type
  //Scans the map and reports raw resources deposits info
  TKMDeposits = class
  private
    fArea: array [TRawDeposit] of array of array of Word;
    fAreaCount: array [TRawDeposit] of Integer;
    fAreaAmount: array [TRawDeposit] of array of Integer;
    fAreaLoc: array [TRawDeposit] of array of TKMPointF;
    function GetCount(aMat: TRawDeposit): Integer;
    function GetAmount(aMat: TRawDeposit; aIndex: Integer): Integer;
    function GetLocation(aMat: TRawDeposit; aIndex: Integer): TKMPointF;
    function TileDepositExists(aMat: TRawDeposit; X,Y: Word): Boolean;
    function TileDeposit(aMat: TRawDeposit; X,Y: Word): Byte;
    procedure FloodFill(const aMat: array of TRawDeposit);
    procedure RecalcAmounts(const aMat: array of TRawDeposit);
  public
    property Count[aMat: TRawDeposit]: Integer read GetCount;
    property Amount[aMat: TRawDeposit; aIndex: Integer]: Integer read GetAmount;
    property Location[aMat: TRawDeposit; aIndex: Integer]: TKMPointF read GetLocation;
    procedure UpdateAreas(const aMat: array of TRawDeposit);
  end;


implementation


{ TKMDeposits }
function TKMDeposits.GetAmount(aMat: TRawDeposit; aIndex: Integer): Integer;
begin
  Result := fAreaAmount[aMat, aIndex];
end;


function TKMDeposits.GetCount(aMat: TRawDeposit): Integer;
begin
  Result := fAreaCount[aMat];
end;


function TKMDeposits.GetLocation(aMat: TRawDeposit; aIndex: Integer): TKMPointF;
begin
  Result := fAreaLoc[aMat, aIndex];
end;


//Check whether deposit exist and do proper action
//TileIsWater is used to make an area from whole water body - not only connected fish
function TKMDeposits.TileDepositExists(aMat: TRawDeposit; X,Y: Word) : Boolean;
begin
  if aMat = rdFish then
    Result := gTerrain.TileIsWater(KMPoint(X,Y))
  else
    Result := TileDeposit(aMat,X,Y) > 0;
end;


//Get tile resource deposit
function TKMDeposits.TileDeposit(aMat: TRawDeposit; X,Y: Word): Byte;
var
  curUnit: TKMUnit;
begin
  case aMat of
    rdStone: Result := 3 * gTerrain.TileIsStone(X, Y); //3 stone produced from each chunk
    rdCoal:  Result := gTerrain.TileIsCoal(X, Y);
    rdIron:  Result := gTerrain.TileIsIron(X, Y);
    rdGold:  Result := gTerrain.TileIsGold(X, Y);
    rdFish:  begin
               curUnit := gTerrain.Land[Y, X].IsUnit;
               if (curUnit <> nil) and (curUnit is TKMUnitAnimal) and (curUnit.UnitType = ut_Fish) then
                 Result := 2 * TKMUnitAnimal(curUnit).FishCount //You get 2 fish from each trip
               else
                 Result := 0;
             end
    else     Result := 0;
  end;
end;


procedure TKMDeposits.FloodFill(const aMat: array of TRawDeposit);
var
  R: TRawDeposit;
  AreaID: Word;
  AreaAmount: Integer;
  //Procedure uses recurence to check test area then it creates one deposit
  //Deposit is created when tiles are connected - but not diagonally
  procedure FillArea(X,Y: Word);
  begin
    //Untested area that matches passability
    if (fArea[R,Y,X] = 0) and (TileDepositExists(R,X,Y)) then
    begin
      fArea[R,Y,X] := AreaID;
      Inc(AreaAmount);
      //We must test diagonals for at least fish since they can be taken from water through diagonals
      if X-1 >= 1 then
      begin
        if Y-1 >= 1 then               FillArea(X-1, Y-1);
                                       FillArea(X-1, Y);
        if Y+1 <= gTerrain.MapY-1 then FillArea(X-1, Y+1);
      end;

      if Y-1 >= 1 then                 FillArea(X, Y-1);
      if Y+1 <= gTerrain.MapY-1 then   FillArea(X, Y+1);

      if X+1 <= gTerrain.MapX-1 then
      begin
        if Y-1 >= 1 then               FillArea(X+1, Y-1);
                                       FillArea(X+1, Y);
        if Y+1 <= gTerrain.MapY-1 then FillArea(X+1, Y+1);
      end;
    end;
  end;
var
  I,K,J: Integer;
begin
  Assert(gTerrain <> nil);
  for J := Low(aMat) to High(aMat) do
  begin
    R := aMat[J];

    SetLength(fArea[R], 0, 0);
    SetLength(fArea[R], gTerrain.MapY, gTerrain.MapX);

    AreaID := 0;
    for I := 1 to gTerrain.MapY - 1 do
    for K := 1 to gTerrain.MapX - 1 do
    if (fArea[R,I,K] = 0) and TileDepositExists(R,K,I) then
    begin
      Inc(AreaID);
      AreaAmount := 0;
      FillArea(K,I);

      if AreaAmount <= 1 then //Revert
      begin
        Dec(AreaID);
        AreaAmount := 0;
        fArea[R,I,K] := 0;
      end;
    end;

    fAreaCount[R] := AreaID;
  end;
end;


procedure TKMDeposits.RecalcAmounts(const aMat: array of TRawDeposit);
var
  I, K, J: Integer;
  R: TRawDeposit;
  AreaID: Integer;
  AreaSize: array of Integer;
  AreaPos: TKMPointArray; //Used as accumulator
begin
  for J := Low(aMat) to High(aMat) do
  begin
    R := aMat[J];

    //Clear old values
    SetLength(fAreaAmount[R], 0);
    SetLength(AreaSize, 0);
    SetLength(AreaPos, 0);
    SetLength(fAreaAmount[R], fAreaCount[R]);
    SetLength(fAreaLoc[R], fAreaCount[R]);
    SetLength(AreaSize, fAreaCount[R]);
    SetLength(AreaPos, fAreaCount[R]);

    //Fill array of resource amounts per area
    for I := 1 to gTerrain.MapY - 1 do
    for K := 1 to gTerrain.MapX - 1 do
    if fArea[R, I, K] <> 0 then
    begin
      //Do -1 to make the lists 0 based
      AreaID := fArea[R, I, K] - 1;
      //Increase amount of resource in area
      Inc(fAreaAmount[R, AreaID], TileDeposit(R, K, I));

      //Accumulate area locations
      AreaPos[AreaID].X := AreaPos[AreaID].X + K;
      AreaPos[AreaID].Y := AreaPos[AreaID].Y + I;
      Inc(AreaSize[AreaID]);
    end;

    //Get average locations
    for I := 0 to fAreaCount[R] - 1 do
    begin
      fAreaLoc[R, I].X := AreaPos[I].X / AreaSize[I] - 0.5;
      fAreaLoc[R, I].Y := AreaPos[I].Y / AreaSize[I] - 0.5;
    end;
  end;
end;


procedure TKMDeposits.UpdateAreas(const aMat: array of TRawDeposit);
begin
  //Use connected areas flood fill to detect deposit areas
  FloodFill(aMat);

  //Determine deposit amounts and location
  RecalcAmounts(aMat);
end;


end.
