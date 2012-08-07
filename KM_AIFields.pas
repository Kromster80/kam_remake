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
    fPlayerCount: Integer;
    fShowInfluenceMap: Boolean;
    fShowNavMesh: Boolean;

    fInfluenceMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Byte;
    fInfluenceMinMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Integer;

    fNavMesh: record
      PCount: Integer;
      Polies: array of record
        NCount: Integer;
        Nodes: array of TKMPointI;
        NCount1: Integer;
        Nodes1: array of TKMPointI;
        NCountS: Integer;
        NodesS: array of TKMPointI;
        NCountG: Integer;
        NodesG: array of TKMPointI;
      end;
    end;

    procedure NavMeshBaseGrid;
    procedure NavMeshObstacles;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure ExportInfluenceMaps;

    procedure UpdateInfluenceMaps;
    procedure UpdateNavMesh;
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  fAIFields: TKMAIFields;


implementation
uses KM_Game, KM_Log, KM_PlayersCollection, KM_RenderAux, KM_PolySimplify;


{ TKMAIFields }
constructor TKMAIFields.Create;
begin
  inherited Create;

  fShowNavMesh := True;
end;


destructor TKMAIFields.Destroy;
begin
  //
  inherited;
end;


procedure TKMAIFields.AfterMissionInit;
begin
  fPlayerCount := fPlayers.Count;
  SetLength(fInfluenceMap, fPlayerCount);
  SetLength(fInfluenceMinMap, fPlayerCount);
end;


procedure TKMAIFields.UpdateInfluenceMaps;
var
  I, J, K: Integer; T: Byte;
  H: Integer;
begin
  Assert(fTerrain <> nil);

  //Update direct influence maps
  for J := 0 to fPlayerCount - 1 do
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

  for J := 0 to fPlayerCount - 1 do
  begin
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
    begin
      fInfluenceMinMap[J, I, K] := fInfluenceMap[J, I, K];
      if not (CanWalk in fTerrain.Land[I,K].Passability) then
        fInfluenceMinMap[J, I, K] := -512;

      for H := 0 to fPlayerCount - 1 do
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
  NavMeshObstacles;

  //1.Uniform grid
  //2.Contours around obstacles with Marching Squares
  //3.Merge contours with grid
  //
end;


procedure TKMAIFields.ExportInfluenceMaps;
var
  I, J, K: Integer;
begin
  for J := 0 to fPlayerCount - 1 do
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

  for J := 0 to fPlayerCount - 1 do
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


procedure TKMAIFields.NavMeshBaseGrid;
var
  I,K: Integer;
begin
  //for I := 1 to fTerrain.MapY - 1 do
  //for K := 1 to fTerrain.MapX - 1 do

end;


procedure TKMAIFields.NavMeshObstacles;
type
  TStepDirection = (sdNone, sdUp, sdRight, sdDown, sdLeft);
var
  Tmp: array of array of Byte;
  PrevStep, NextStep: TStepDirection;

  procedure Step(X,Y: Word);
    function IsTilePassable(aX, aY: Word): Boolean;
    begin
      Result := InRange(aX, 1, fTerrain.MapX-1)
                and InRange(aY, 1, fTerrain.MapY-1)
                and (Tmp[aY,aX] > 0);
      //Mark tiles we've been on, so they do not trigger new duplicate contour
      if Result then
        Tmp[aY,aX] := Tmp[aY,aX] - (Tmp[aY,aX] div 2);
    end;
  var
    State: Byte;
  begin
    prevStep := nextStep;

    //Assemble bitmask
    State :=  Byte(IsTilePassable(X  ,Y  )) +
              Byte(IsTilePassable(X+1,Y  )) * 2 +
              Byte(IsTilePassable(X  ,Y+1)) * 4 +
              Byte(IsTilePassable(X+1,Y+1)) * 8;

    //Where do we go from here
    case State of
      1:  nextStep := sdUp;
      2:  nextStep := sdRight;
      3:  nextStep := sdRight;
      4:  nextStep := sdLeft;
      5:  nextStep := sdUp;
      6:  if (prevStep = sdUp) then
            nextStep := sdLeft
          else
            nextStep := sdRight;
      7:  nextStep := sdRight;
      8:  nextStep := sdDown;
      9:  if (prevStep = sdRight) then
            nextStep := sdUp
          else
            nextStep := sdDown;
      10: nextStep := sdDown;
      11: nextStep := sdDown;
      12: nextStep := sdLeft;
      13: nextStep := sdUp;
      14: nextStep := sdLeft;
      else nextStep := sdNone;
    end;
  end;

  procedure WalkPerimeter(aStartX, aStartY: Word);
  var
    X, Y: Integer;
  begin
    X := aStartX;
    Y := aStartY;
    nextStep := sdNone;

    SetLength(fNavMesh.Polies, fNavMesh.PCount + 1);
    fNavMesh.Polies[fNavMesh.PCount].NCount := 0;

    repeat
      Step(X, Y);

      case NextStep of
        sdUp:     Dec(Y);
        sdRight:  Inc(X);
        sdDown:   Inc(Y);
        sdLeft:   Dec(X);
        else
      end;

      //Append new node vertice
      with fNavMesh.Polies[fNavMesh.PCount] do
      begin
        if Length(Nodes) <= NCount then
          SetLength(Nodes, NCount + 32);
        Nodes[NCount] := KMPointI(X, Y);
        Inc(NCount);
      end;
    until((X = aStartX) and (Y = aStartY));

    //Do not include too small regions
    if fNavMesh.Polies[fNavMesh.PCount].NCount >= 12 then
      Inc(fNavMesh.PCount);
  end;

var
  I, K: Integer;
  C1, C2, C3, C4: Boolean;
  P0, P1, P2: Integer;
begin
  SetLength(Tmp, fTerrain.MapY+1, fTerrain.MapX+1);

  //Copy map to temp array where we can use Keys 0-1-2 for internal purposes
  //0 - no obstacle
  //1 - parsed obstacle
  //2 - non-parsed obstacle
  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
    Tmp[I,K] := 2 - Byte(CanWalk in fTerrain.Land[I,K].Passability) * 2;

  fNavMesh.PCount := 0;
  for I := 1 to fTerrain.MapY - 2 do
  for K := 1 to fTerrain.MapX - 2 do
  begin
    //Find new seed among unparsed obstacles
    //C1-C2
    //C3-C4
    C1 := (Tmp[I,K] = 2);
    C2 := (Tmp[I,K+1] = 2);
    C3 := (Tmp[I+1,K] = 2);
    C4 := (Tmp[I+1,K+1] = 2);

    //todo: Skip cases where C1..C4 are all having value of 1-2
    if (C1 or C2 or C3 or C4) <> (C1 and C2 and C3 and C4) then
      WalkPerimeter(K,I);
  end;

  //Basic simplify
  for I := 0 to fNavMesh.PCount - 1 do
  with fNavMesh.Polies[I] do
  begin
    NCount1 := 0;
    SetLength(Nodes1, NCount+1);
    for K := 0 to NCount - 1 do
    begin
      P0 := (K - 1 + NCount) mod NCount;
      P1 := K;
      P2 := (K + 1) mod NCount;
      if ((Nodes[P0].X <> Nodes[P1].X) or (Nodes[P1].X <> Nodes[P2].X))
      and ((Nodes[P0].Y <> Nodes[P1].Y) or (Nodes[P1].Y <> Nodes[P2].Y)) then
      begin
        Nodes1[NCount1] := Nodes[K];
        Inc(NCount1);
      end;
    end;
    Nodes1[NCount1] := Nodes1[0];
    Inc(NCount1);
    SetLength(Nodes1, NCount1);
  end;

  for I := 0 to fNavMesh.PCount - 1 do
  with fNavMesh.Polies[I] do
  begin
    SetLength(NodesS, NCount1);
    NCountS := PolySimplify(2, Nodes1, NodesS);
    SetLength(NodesS, NCountS);
  end;
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  //UpdateInfluenceMaps;
  NavMeshObstacles;
end;


procedure TKMAIFields.Paint(aRect: TKMRect);
var I, K: Integer; TX, TY: Single;
begin
  if fShowInfluenceMap then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      fRenderAux.Quad(K, I, fInfluenceMap[MyPlayer.PlayerIndex, I, K] or $B0000000);

  {if fShowNavMesh then
    for I := 0 to fNavMesh.PCount - 1 do
    for K := 0 to fNavMesh.Polies[I].NCount - 1 do
    with fNavMesh.Polies[I] do
    begin
      TX := Nodes[(K + 1) mod NCount].X;
      TY := Nodes[(K + 1) mod NCount].Y;
      fRenderAux.LineOnTerrain(Nodes[K].X, Nodes[K].Y, TX, TY, $FFFF00FF);
    end;}

  if fShowNavMesh then
    for I := 0 to fNavMesh.PCount - 1 do
    for K := 0 to fNavMesh.Polies[I].NCount1 - 1 do
    with fNavMesh.Polies[I] do
    begin
      TX := Nodes1[(K + 1) mod NCount1].X;
      TY := Nodes1[(K + 1) mod NCount1].Y;
      fRenderAux.LineOnTerrain(Nodes1[K].X, Nodes1[K].Y, TX, TY, $FFFF00FF);
    end;

  if fShowNavMesh then
    for I := 0 to fNavMesh.PCount - 1 do
    with fNavMesh.Polies[I] do
    for K := 0 to NCountS - 1 do
    begin
      TX := NodesS[(K + 1) mod NCountS].X;
      TY := NodesS[(K + 1) mod NCountS].Y;
      fRenderAux.LineOnTerrain(NodesS[K].X, NodesS[K].Y, TX, TY, $FF00FF00);
    end;

end;


end.
