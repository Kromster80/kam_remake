unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes,
  KM_Defaults, KM_Points, KM_Terrain;


type
  TRawDeposit = (rdStone, rdCoal, rdIron, rdGold);

  //Designed to store MapEd specific data and methods
  TKMMapEditor = class
  private
    fArea: array [TRawDeposit] of array of array of Word;
    fAreaCount: array [TRawDeposit] of Integer;
    fAreaAmount: array [TRawDeposit] of array of Integer;
    fAreaLoc: array [TRawDeposit] of array of TKMPointF;
    fShowDefencePositions: Boolean;
    function GetAreaCount(aMat: TRawDeposit): Integer;
    function GetAreaAmount(aMat: TRawDeposit; aIndex: Integer): Integer;
    function GetAreaLoc(aMat: TRawDeposit; aIndex: Integer): TKMPointF;
    function TileDeposit(aMat: TRawDeposit; X,Y: Word): Byte;
    procedure FloodFill(const aMat: array of TRawDeposit);
    procedure RecalcAmounts(const aMat: array of TRawDeposit);
    procedure UpdateAreas(const aMat: array of TRawDeposit);
  public
    constructor Create;
    property AreaCount[aMat: TRawDeposit]: Integer read GetAreaCount;
    property AreaAmount[aMat: TRawDeposit; aIndex: Integer]: Integer read GetAreaAmount;
    property AreaLoc[aMat: TRawDeposit; aIndex: Integer]: TKMPointF read GetAreaLoc;
    procedure Update;
    procedure RenderOverlays;
  end;


implementation
uses  KM_PlayersCollection, KM_RenderAux;


{ TKMMapEditor }
constructor TKMMapEditor.Create;
begin
  inherited Create;

  Assert(fTerrain <> nil);
end;


function TKMMapEditor.GetAreaAmount(aMat: TRawDeposit; aIndex: Integer): Integer;
begin
  Result := fAreaAmount[aMat, aIndex];
end;


function TKMMapEditor.GetAreaCount(aMat: TRawDeposit): Integer;
begin
  Result := fAreaCount[aMat];
end;


function TKMMapEditor.GetAreaLoc(aMat: TRawDeposit; aIndex: Integer): TKMPointF;
begin
  Result := fAreaLoc[aMat, aIndex];
end;


//Get tile resource deposit
function TKMMapEditor.TileDeposit(aMat: TRawDeposit; X,Y: Word): Byte;
begin
  case aMat of
    rdStone: Result := 3*fTerrain.TileIsStone(X+1, Y+1); //3 stone produced by each time
    rdCoal:  Result := fTerrain.TileIsCoal(X+1, Y+1);
    rdIron:  Result := fTerrain.TileIsIron(X+1, Y+1);
    rdGold:  Result := fTerrain.TileIsGold(X+1, Y+1);
    else     Result := 0;
  end;
end;


procedure TKMMapEditor.FloodFill(const aMat: array of TRawDeposit);
var
  R: TRawDeposit;
  AreaID: Word;
  Count: Integer;

  procedure FillArea(X,Y: Word);
  begin
    //Untested area that matches passability
    if (fArea[R,Y,X] = 0) and (TileDeposit(R,X,Y) > 0) then
    begin
      fArea[R,Y,X] := AreaID;
      Inc(Count);
      //Dont test diagonals to save time
      if X-1 >= 0 then     FillArea(X-1, Y);
      if Y-1 >= 0 then     FillArea(X, Y-1);
      if Y+1 <= fTerrain.MapY-1 then FillArea(X, Y+1);
      if X+1 <= fTerrain.MapX-1 then FillArea(X+1, Y);
    end;
  end;
var I,K,J: Integer;
begin
  for J := Low(aMat) to High(aMat) do
  begin
    R := aMat[J];

    SetLength(fArea[R], 0, 0);
    SetLength(fArea[R], fTerrain.MapY, fTerrain.MapX);

    AreaID := 0;
    for I := 0 to fTerrain.MapY - 1 do
    for K := 0 to fTerrain.MapX - 1 do
    if (fArea[R,I,K] = 0) and (TileDeposit(R,K,I) > 0) then
    begin
      Inc(AreaID);
      Count := 0;
      FillArea(K,I);

      if Count <= 1 then //Revert
      begin
        Dec(AreaID);
        Count := 0;
        fArea[R,I,K] := 0;
      end;
    end;

    fAreaCount[R] := AreaID;
  end;
end;


procedure TKMMapEditor.RecalcAmounts(const aMat: array of TRawDeposit);
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


procedure TKMMapEditor.UpdateAreas(const aMat: array of TRawDeposit);
begin
  //Use connected areas flood fill to detect deposit areas
  FloodFill(aMat);

  //Determine deposit amounts and location
  RecalcAmounts(aMat);
end;


procedure TKMMapEditor.Update;
begin
  UpdateAreas([rdStone, rdCoal, rdIron, rdGold]);
end;


procedure TKMMapEditor.RenderOverlays;
var I, K: Integer;
begin
 { if fShowDefencePositions then
  begin
    for I := 0 to fPlayers.Count - 1 do
      for K := 0 to fPlayers[I].AI.DefencePositionsCount - 1 do
      begin
        Label_DefencePos.Caption := GetEnumName(TypeInfo(TGroupType), Ord(fPlayers[I].AI.DefencePositions[K].GroupType));

        MapLoc := fTerrain.FlatToHeight(KMPointF(fPlayers[I].AI.DefencePositions[K].Position.Loc));
        ScreenLoc := fGame.Viewport.MapToScreen(MapLoc);

        //Paint the background
        Shape_DefencePos.Width := 10 + 10 * Length(Label_DefencePos.Caption);
        Shape_DefencePos.Left := ScreenLoc.X - Shape_DefencePos.Width div 2;
        Shape_DefencePos.Top := ScreenLoc.Y - 10;
        Shape_DefencePos.Paint;
        //Paint the label on top of the background
        Label_DefencePos.Left := ScreenLoc.X;
        Label_DefencePos.Top := ScreenLoc.Y - 7;
        Label_DefencePos.Paint;
      end;
    Label_DefencePos.Hide; //Only make it visible while we need it
    Shape_DefencePos.Hide;
  end; }
end;



end.
