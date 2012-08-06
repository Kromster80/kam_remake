unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics,
  KM_CommonClasses, KM_Units, KM_Terrain, KM_Houses, KM_Defaults, KM_Player, KM_Utils, KM_Points;


//Influence maps, navmeshes, etc
type
  TKMAIFields = class
  private
    fCount: Integer;
    fInfluenceMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Byte;
    fInfluenceMinMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Integer;
    procedure UpdateInfluenceMaps;
    procedure UpdateNavMesh;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure ExportInfluenceMaps;

    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  fAIFields: TKMAIFields;


implementation
uses KM_Game, KM_Log, KM_PlayersCollection, KM_RenderAux;


{ TKMAIFields }
constructor TKMAIFields.Create;
begin
  inherited Create;

  //
end;


destructor TKMAIFields.Destroy;
begin
  //
  inherited;
end;


procedure TKMAIFields.AfterMissionInit;
begin
  fCount := fPlayers.Count;
  SetLength(fInfluenceMap, fCount);
  SetLength(fInfluenceMinMap, fCount);
end;


procedure TKMAIFields.UpdateInfluenceMaps;
var
  I, J, K: Integer; T: Byte;
  H: Integer;
begin
  Assert(fTerrain <> nil);

  //Update direct influence maps
  for J := 0 to fCount - 1 do
  begin
    for I := 1 to fTerrain.MapY - 1 do
    for K := 1 to fTerrain.MapX - 1 do
      fInfluenceMap[J, I, K] := Byte(fTerrain.Land[I, K].TileOwner = J) * 255;

    for H := 0 to 255 do
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
    if CanWalk in fTerrain.Land[I,K].Passability then
    begin
      T := Max(Max(Max(Max( fInfluenceMap[J, I-1, K],
                            fInfluenceMap[J, I, K-1]),
                        Max(fInfluenceMap[J, I+1, K],
                            fInfluenceMap[J, I, K+1])) - 2,
                    Max(Max(fInfluenceMap[J, I-1, K-1],
                            fInfluenceMap[J, I-1, K+1]),
                        Max(fInfluenceMap[J, I+1, K+1],
                            fInfluenceMap[J, I+1, K-1])) - 3), 0);
      fInfluenceMap[J, I, K] := Max(fInfluenceMap[J, I, K], T);
    end;

    with TBitmap.Create do
    begin
      Width := fTerrain.MapX;
      Height:= fTerrain.MapY;
      PixelFormat := pf32bit;
      for I := 0 to Height-1 do
        for K := 0 to Width-1 do
          Canvas.Pixels[K,I] := fInfluenceMap[J, I, K];
      SaveToFile(ExeDir + 'Infl'+IntToStr(J)+'.bmp');
    end;
  end;

  for J := 0 to fCount - 1 do
  begin
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
    begin
      fInfluenceMinMap[J, I, K] := fInfluenceMap[J, I, K];
      if not (CanWalk in fTerrain.Land[I,K].Passability) then
        fInfluenceMinMap[J, I, K] := -512;

      for H := 0 to fCount - 1 do
      if H <> J then
        fInfluenceMinMap[J, I, K] := fInfluenceMinMap[J, I, K] - fInfluenceMap[H, I, K];
    end;
    with TBitmap.Create do
    begin
      Width := fTerrain.MapX;
      Height:= fTerrain.MapY;
      PixelFormat := pf32bit;
      for I := 0 to Height-1 do
        for K := 0 to Width-1 do
          Canvas.Pixels[K,I] := EnsureRange((fInfluenceMinMap[J, I, K] + 255), 0, 255);
      SaveToFile(ExeDir + 'InflMin'+IntToStr(J)+'.bmp');
    end;
  end;
end;


procedure TKMAIFields.UpdateNavMesh;
begin
  //1.Uniform grid
  //2.Contours around obstacles with Marching Squares
  //3.Merge contours with grid
  //
end;


procedure TKMAIFields.ExportInfluenceMaps;
var
  I, J, K: Integer;
begin
  for J := 0 to fCount - 1 do
  with TBitmap.Create do
  begin
    Width := fTerrain.MapX;
    Height:= fTerrain.MapY;
    PixelFormat := pf32bit;
    for I := 0 to Height-1 do
      for K := 0 to Width-1 do
        Canvas.Pixels[K,I] := fInfluenceMap[J, I, K];
    SaveToFile(ExeDir + 'Export\Influence map Player'+IntToStr(J) + '.bmp');
  end;

  for J := 0 to fCount - 1 do
  with TBitmap.Create do
  begin
    Width := fTerrain.MapX;
    Height:= fTerrain.MapY;
    PixelFormat := pf32bit;
    for I := 0 to Height-1 do
      for K := 0 to Width-1 do
        Canvas.Pixels[K,I] := fInfluenceMap[J, I, K];
    SaveToFile(ExeDir + 'Export\Influence map Player'+IntToStr(J) + '.bmp');
  end;
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  UpdateInfluenceMaps;
end;


procedure TKMAIFields.Paint(aRect: TKMRect);
var I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
  for K := aRect.Left to aRect.Right do
    fRenderAux.Quad(K, I, fInfluenceMap[MyPlayer.PlayerIndex, I, K] or $B0000000);
end;


end.
