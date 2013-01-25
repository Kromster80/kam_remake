unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Terrain, KM_Units, KM_RenderPool;


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

  TMarkerType = (mtNone, mtDefence, mtRevealFOW);

  TKMMapEdMarker = record
    MarkerType: TMarkerType;
    Owner: TPlayerIndex;
    Index: SmallInt;
  end;

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

  //Designed to store MapEd specific data and methods
  TKMMapEditor = class
  private
    fDeposits: TKMDeposits;
    fRevealers: array [0..MAX_PLAYERS-1] of TKMPointTagList;
    fVisibleLayers: TMapEdLayerSet;
    function GetRevealer(aIndex: Byte): TKMPointTagList;
  public
    DefaultHuman: TPlayerIndex;
    PlayerHuman: array [0..MAX_PLAYERS - 1] of Boolean;
    PlayerAI: array [0..MAX_PLAYERS - 1] of Boolean;
    constructor Create;
    destructor Destroy; override;
    property Deposits: TKMDeposits read fDeposits;
    property Revealers[aIndex: Byte]: TKMPointTagList read GetRevealer;
    property VisibleLayers: TMapEdLayerSet read fVisibleLayers write fVisibleLayers;
    function HitTest(X,Y: Integer): TKMMapEdMarker;
    procedure Update;
    procedure PaintUI;
    procedure Paint(aLayer: TPaintLayer);
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_RenderAux, KM_RenderUI, KM_AIFields;


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
    Result := fTerrain.TileIsWater(KMPoint(X+1,Y+1))
  else
    Result := TileDeposit(aMat,X,Y) > 0;
end;


//Get tile resource deposit
function TKMDeposits.TileDeposit(aMat: TRawDeposit; X,Y: Word): Byte;
var
curUnit : TKMUnit;
begin
  case aMat of
    rdStone: Result := 3*fTerrain.TileIsStone(X+1, Y+1); //3 stone produced by each time
    rdCoal:  Result := fTerrain.TileIsCoal(X+1, Y+1);
    rdIron:  Result := fTerrain.TileIsIron(X+1, Y+1);
    rdGold:  Result := fTerrain.TileIsGold(X+1, Y+1);
    rdFish:  begin
               curUnit := fTerrain.Land[Y + 1, X + 1].IsUnit;
               if (curUnit <> nil) and (curUnit is TKMUnitAnimal) and (curUnit.UnitType = ut_Fish) then
                 Result := 2*TKMUnitAnimal(curUnit).FishCount //You get 2 fish from each trip
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
      if X-1 >= 0 then
      begin
        if Y-1 >= 0 then               FillArea(X-1, Y-1);
                                       FillArea(X-1, Y);
        if Y+1 <= fTerrain.MapY-1 then FillArea(X-1, Y+1);
      end;

      if Y-1 >= 0 then                 FillArea(X, Y-1);
      if Y+1 <= fTerrain.MapY-1 then   FillArea(X, Y+1);

      if X+1 <= fTerrain.MapX-1 then
      begin
        if Y-1 >= 0 then               FillArea(X+1, Y-1);
                                       FillArea(X+1, Y);
        if Y+1 <= fTerrain.MapY-1 then FillArea(X+1, Y+1);
      end;
    end;
  end;
var
  I,K,J: Integer;
begin
  Assert(fTerrain <> nil);
  for J := Low(aMat) to High(aMat) do
  begin
    R := aMat[J];

    SetLength(fArea[R], 0, 0);
    SetLength(fArea[R], fTerrain.MapY, fTerrain.MapX);

    AreaID := 0;
    for I := 0 to fTerrain.MapY - 1 do
    for K := 0 to fTerrain.MapX - 1 do
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
  AreaPos: array of TKMPointI; //Used as accumulator
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
    for I := 0 to fTerrain.MapY - 1 do
    for K := 0 to fTerrain.MapX - 1 do
    if fArea[R, I, K] <> 0 then
    begin
      //Do -1 to make the lists 0 based
      AreaID := fArea[R, I, K] - 1;
      //Increase amount of resource in area
      Inc(fAreaAmount[R, AreaID], TileDeposit(R, K, I));

      //Accumulate area locations (add +1 because of Terrain is 1..N)
      AreaPos[AreaID].X := AreaPos[AreaID].X + K + 1;
      AreaPos[AreaID].Y := AreaPos[AreaID].Y + I + 1;
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


{ TKMMapEditor }
constructor TKMMapEditor.Create;
var
  I: Integer;
begin
  inherited Create;

  fDeposits := TKMDeposits.Create;

  fVisibleLayers := [mlObjects, mlHouses, mlUnits, mlDeposits];

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I] := TKMPointTagList.Create;
end;


destructor TKMMapEditor.Destroy;
var
  I: Integer;
begin
  FreeAndNil(fDeposits);

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I].Free;

  inherited;
end;


function TKMMapEditor.GetRevealer(aIndex: Byte): TKMPointTagList;
begin
  Result := fRevealers[aIndex];
end;


function TKMMapEditor.HitTest(X, Y: Integer): TKMMapEdMarker;
var I,K: Integer;
begin
  if mlDefences in fVisibleLayers then
  begin
    for I := 0 to fPlayers.Count - 1 do
      for K := 0 to fPlayers[I].AI.General.DefencePositions.Count - 1 do
        if (fPlayers[I].AI.General.DefencePositions[K].Position.Loc.X = X)
        and (fPlayers[I].AI.General.DefencePositions[K].Position.Loc.Y = Y) then
        begin
          Result.MarkerType := mtDefence;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;

  if mlRevealFOW in fVisibleLayers then
  begin
    for I := 0 to fPlayers.Count - 1 do
      for K := 0 to fRevealers[I].Count - 1 do
        if (fRevealers[I][K].X = X) and (fRevealers[I][K].Y = Y) then
        begin
          Result.MarkerType := mtRevealFOW;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;

  //Else nothing is found
  Result.MarkerType := mtNone;
  Result.Owner := PLAYER_NONE;
  Result.Index := -1;
end;


procedure TKMMapEditor.Update;
begin
  fDeposits.UpdateAreas([rdStone, rdCoal, rdIron, rdGold, rdFish]);
  //@Krom: This line takes ages, it makes the map editor lock up every few seconds.
  //       On my PC the map editor is basically unusable unless I comment this out.
  //       Why do we care about AI fields in the map editor anyway? And why would be
  //       Init everything over and over, shouldn't we just do it once after loading?
  fAIFields.AfterMissionInit;
  fAIFields.UpdateState(0);
end;


procedure TKMMapEditor.PaintUI;
  procedure PaintTextInShape(aText: string; X,Y: SmallInt; aLineColor: Cardinal);
  var
    W: Integer;
  begin
    //Paint the background
    W := 10 + 10 * Length(aText);
    TKMRenderUI.WriteShape(X - W div 2, Y - 10, W, 20, $80000000);
    TKMRenderUI.WriteOutline(X - W div 2, Y - 10, W, 20, 2, aLineColor);

    //Paint the label on top of the background
    TKMRenderUI.WriteText(X, Y - 7, 0, aText, fnt_Metal, taCenter, $FFFFFFFF);
  end;

var
  I, K: Integer;
  R: TRawDeposit;
  LocF: TKMPointF;
  ScreenLoc: TKMPointI;
begin
  if mlDeposits in fVisibleLayers then
  begin
    for R := Low(TRawDeposit) to High(TRawDeposit) do
      for I := 0 to fDeposits.Count[R] - 1 do
      //Ignore water areas with 0 fish in them
      if fDeposits.Amount[R, I] > 0 then
      begin
        LocF := fTerrain.FlatToHeight(fDeposits.Location[R, I]);
        ScreenLoc := fGame.Viewport.MapToScreen(LocF);

        //At extreme zoom coords may become out of range of SmallInt used in controls painting
        if KMInRect(ScreenLoc, fGame.Viewport.ViewRect) then
          PaintTextInShape(IntToStr(fDeposits.Amount[R, I]), ScreenLoc.X, ScreenLoc.Y, DEPOSIT_COLORS[R]);
      end;
  end;

  if mlDefences in fGame.MapEditor.VisibleLayers then
  begin
    for I := 0 to fPlayers.Count - 1 do
      for K := 0 to fPlayers[I].AI.General.DefencePositions.Count - 1 do
      begin
        LocF := fTerrain.FlatToHeight(KMPointF(fPlayers[I].AI.General.DefencePositions[K].Position.Loc));
        ScreenLoc := fGame.Viewport.MapToScreen(LocF);

        if KMInRect(ScreenLoc, fGame.Viewport.ViewRect) then
        begin
          PaintTextInShape(IntToStr(K), ScreenLoc.X, ScreenLoc.Y - 15, $F0FF8000);
          PaintTextInShape(fPlayers[I].AI.General.DefencePositions[K].UITitle, ScreenLoc.X, ScreenLoc.Y, $F0FF8000);
        end;
      end;
  end;
end;


procedure TKMMapEditor.Paint(aLayer: TPaintLayer);
var
  I, K: Integer;
  Loc: TKMPoint;
  LocDir: TKMPointDir;
begin
  if mlDefences in fVisibleLayers then
  for I := 0 to fPlayers.Count - 1 do
  for K := 0 to fPlayers[I].AI.General.DefencePositions.Count - 1 do
  begin
    LocDir := fPlayers[I].AI.General.DefencePositions[K].Position;
    case aLayer of
      plTerrain:  fRenderAux.CircleOnTerrain(LocDir.Loc.X, LocDir.Loc.Y,
                               fPlayers[I].AI.General.DefencePositions[K].Radius,
                               fPlayers[I].FlagColor AND $20FFFF80,
                               fPlayers[I].FlagColor);
      plCursors:  fRenderPool.RenderCursorBuildIcon(LocDir.Loc,
                      510 + Byte(LocDir.Dir), fPlayers[I].FlagColor);
    end;
  end;

  if mlRevealFOW in fVisibleLayers then
  for I := 0 to fPlayers.Count - 1 do
  for K := 0 to fRevealers[I].Count - 1 do
  begin
    Loc := fRevealers[I][K];
    case aLayer of
      plTerrain:  fRenderAux.CircleOnTerrain(Loc.X, Loc.Y,
                                           fRevealers[I].Tag[K],
                                           fPlayers[I].FlagColor AND $20FFFFFF,
                                           fPlayers[I].FlagColor);
      plCursors:  fRenderPool.RenderCursorBuildIcon(Loc,
                      394, fPlayers[I].FlagColor);
    end;
  end;
end;


end.
