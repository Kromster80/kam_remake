unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics, Delaunay,
  KM_CommonClasses, KM_CommonTypes, KM_Terrain, KM_Defaults, KM_Player, KM_Utils, KM_Points, KM_PolySimplify;

type
  TKMOutline = array of record
    Node: TKMPoint;
    Open: Boolean;
  end;

  //Strcucture to describe NavMesh layout
  TKMNavMesh = class
  private
    fVertices: TKMPointArray;
    //Edges: array of array [0..1] of Word;
    Polygons: array of record
      Indices: array of Word;
      Area: Word; //Area of this polygon
      {Neighbours: array of record //Neighbour polygons
        Index: Word; //Index of polygon
        Touch: Byte; //Length of border between
      end;}
      fOwner: array [0..MAX_PLAYERS-1] of TPlayerIndex;
    end;

    function GetBestOwner(aIndex: Integer): TPlayerIndex;
    procedure UpdateOwnership;
  public
    //GetOwnerOutline:
    procedure UpdateState(aTick: Cardinal);
  end;

  //Influence maps, navmeshes, etc
  TKMAIFields = class
  private
    fPlayerCount: Integer;
    fShowInfluenceMap: Boolean;
    fShowNavMesh: Boolean;

    fInfluenceMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Byte;
    fInfluenceMinMap: array of array [0..MAX_MAP_SIZE, 0..MAX_MAP_SIZE] of Integer;

    fRawOutlines: TKMShapesArray;
    fRawOutlines2: TKMShapesArray;
    fSimpleOutlines: TKMShapesArray;
    fDelaunay: TDelaunay;

    fRawDelaunay: TKMTriMesh;

    fNavMesh: TKMNavMesh;

    procedure NavMeshGenerate;
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
uses KM_Game, KM_Log, KM_PlayersCollection, KM_RenderAux;


{ TKMAIFields }
constructor TKMAIFields.Create;
begin
  inherited Create;

  fShowNavMesh := True;
end;


destructor TKMAIFields.Destroy;
begin
  FreeAndNil(fDelaunay);

  inherited;
end;


procedure TKMAIFields.AfterMissionInit;
begin
  //Book space for all players;
  fPlayerCount := fPlayers.Count;
  if AI_GEN_INFLUENCE_MAPS then
  begin
    SetLength(fInfluenceMap, fPlayerCount);
    SetLength(fInfluenceMinMap, fPlayerCount);
    UpdateInfluenceMaps;
  end;

  if AI_GEN_NAVMESH then
  begin
    UpdateNavMesh;
    fNavMesh.UpdateOwnership;
  end;
end;


procedure TKMAIFields.UpdateInfluenceMaps;
var
  I, J, K: Integer; T: Byte;
  H: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
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


procedure TKMAIFields.NavMeshGenerate;
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

    SetLength(fRawOutlines.Shape, fRawOutlines.Count + 1);
    fRawOutlines.Shape[fRawOutlines.Count].Count := 0;

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
      with fRawOutlines.Shape[fRawOutlines.Count] do
      begin
        if Length(Nodes) <= Count then
          SetLength(Nodes, Count + 32);
        Nodes[Count] := KMPointI(X, Y);
        Inc(Count);
      end;
    until((X = aStartX) and (Y = aStartY));

    //Do not include too small regions
    if fRawOutlines.Shape[fRawOutlines.Count].Count >= 12 then
      Inc(fRawOutlines.Count);
  end;

var
  I, K, L, M: Integer;
  C1, C2, C3, C4: Boolean;
  MeshDensityX, MeshDensityY: Byte;
  SizeX, SizeY: Word;
  PX,PY,TX,TY: Integer;
  Skip: Boolean;
begin
  SetLength(Tmp, fTerrain.MapY+1, fTerrain.MapX+1);

  //Copy map to temp array where we can use Keys 0-1-2 for internal purposes
  //0 - no obstacle
  //1 - parsed obstacle
  //2 - non-parsed obstacle
  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
    Tmp[I,K] := 2 - Byte(CanOwn in fTerrain.Land[I,K].Passability) * 2;

  fRawOutlines.Count := 0;
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

  SimplifyStraights(fRawOutlines, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fRawOutlines2);

  with TKMSimplifyShapes.Create(2, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1)) do
  begin
    Execute(fRawOutlines2, fSimpleOutlines);
    Free;
  end;

  //Fill Delaunay triangles
  fDelaunay := TDelaunay.Create(-1, -1, fTerrain.MapX, fTerrain.MapY);
  fDelaunay.Tolerance := 1;
  for I := 0 to fSimpleOutlines.Count - 1 do
  with fSimpleOutlines.Shape[I] do
    for K := 0 to Count - 1 do
      fDelaunay.AddPoint(Nodes[K].X, Nodes[K].Y);

  //Add more points on edges
  SizeX := fTerrain.MapX-1;
  SizeY := fTerrain.MapY-1;
  MeshDensityX := SizeX div 15; //once per 15 tiles
  MeshDensityY := SizeY div 15;
  for I := 0 to MeshDensityY do
  for K := 0 to MeshDensityX do
  if (I = 0) or (I = MeshDensityY) or (K = 0) or (K = MeshDensityX) then
  begin
    Skip := False;
    PX := Round(SizeX / MeshDensityX * K);
    PY := Round(SizeY / MeshDensityY * I);

    //Don't add point to obstacle outline if there's one below
    for L := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[L] do
      for M := 0 to Count - 1 do
      begin
        TX := Nodes[(M + 1) mod Count].X;
        TY := Nodes[(M + 1) mod Count].Y;
        if InRange(PX, Nodes[M].X, TX) and InRange(PY, Nodes[M].Y, TY)
        or InRange(PX, TX, Nodes[M].X) and InRange(PY, TY, Nodes[M].Y) then
          Skip := True;
      end;

    if not Skip then
      fDelaunay.AddPoint(PX, PY);
  end;

  //Tolerance must be a little higher than longest span we expect from polysimplification
  //so that not a single node was placed on an outline segment (otherwise RemObstaclePolys will not be able to trace outlines)
  fDelaunay.Tolerance := 7;
  for I := 1 to MeshDensityY - 1 do
  for K := 1 to MeshDensityX - Byte(I mod 2 = 1) - 1 do
    fDelaunay.AddPoint(Round(SizeX / MeshDensityX * (K + Byte(I mod 2 = 1) / 2)), Round(SizeY / MeshDensityY * I));

  fDelaunay.Mesh;

  //Bring triangulated mesh back
  SetLength(fRawDelaunay.Vertices, fDelaunay.VerticeCount);
  for I := 0 to fDelaunay.VerticeCount - 1 do
  begin
    fRawDelaunay.Vertices[I].X := Round(fDelaunay.Vertex[I].X);
    fRawDelaunay.Vertices[I].Y := Round(fDelaunay.Vertex[I].Y);
  end;
  SetLength(fRawDelaunay.Polygons, fDelaunay.PolyCount);
  for I := 0 to fDelaunay.PolyCount - 1 do
  begin
    fRawDelaunay.Polygons[I,0] := fDelaunay.Triangle^[I].vv0;
    fRawDelaunay.Polygons[I,1] := fDelaunay.Triangle^[I].vv1;
    fRawDelaunay.Polygons[I,2] := fDelaunay.Triangle^[I].vv2;
  end;

  ForceOutlines(fRawDelaunay, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fSimpleOutlines);

  RemoveObstaclePolies(fRawDelaunay, fSimpleOutlines);

  RemoveFrame(fRawDelaunay);

  CheckForDegenerates(fRawDelaunay);//}

  Assert(Length(fRawDelaunay.Polygons) > 8);

  fNavMesh := TKMNavMesh.Create;

  //Bring triangulated mesh back
  SetLength(fNavMesh.fVertices, Length(fRawDelaunay.Vertices));
  for I := 0 to High(fRawDelaunay.Vertices) do
  begin
    fNavMesh.fVertices[I].X := Round(fRawDelaunay.Vertices[I].X);
    fNavMesh.fVertices[I].Y := Round(fRawDelaunay.Vertices[I].Y);
  end;
  SetLength(fNavMesh.Polygons, Length(fRawDelaunay.Polygons));
  for I := 0 to High(fRawDelaunay.Polygons) do
  begin
    SetLength(fNavMesh.Polygons[I].Indices, 3);
    fNavMesh.Polygons[I].Indices[0] := fRawDelaunay.Polygons[I,0];
    fNavMesh.Polygons[I].Indices[1] := fRawDelaunay.Polygons[I,1];
    fNavMesh.Polygons[I].Indices[2] := fRawDelaunay.Polygons[I,2];
  end;
end;


procedure TKMAIFields.UpdateNavMesh;
begin
  if not AI_GEN_NAVMESH then Exit;

  NavMeshGenerate;
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  //Maybe we recalculate Influences or navmesh sectors once in a while

  fNavMesh.UpdateState(aTick);
end;


//Render debug symbols
procedure TKMAIFields.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  TX, TY: Single;
  x1,x2,y1,y2: Single;
  Col: Cardinal;
begin
  if AI_GEN_INFLUENCE_MAPS and fShowInfluenceMap then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      fRenderAux.Quad(K, I, fInfluenceMap[MyPlayer.PlayerIndex, I, K] or $B0000000);

  {if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fNavMesh.PCount - 1 do
    for K := 0 to fNavMesh.Polies[I].NCount - 1 do
    with fNavMesh.Polies[I] do
    begin
      TX := Nodes[(K + 1) mod NCount].X;
      TY := Nodes[(K + 1) mod NCount].Y;
      fRenderAux.Line(Nodes[K].X, Nodes[K].Y, TX, TY, $FFFF00FF);
    end;}

  //Raw obstacle outlines
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fRawOutlines2.Count - 1 do
    for K := 0 to fRawOutlines2.Shape[I].Count - 1 do
    with fRawOutlines2.Shape[I] do
    begin
      TX := Nodes[(K + 1) mod Count].X;
      TY := Nodes[(K + 1) mod Count].Y;
      fRenderAux.Line(Nodes[K].X, Nodes[K].Y, TX, TY, $FFFF00FF);
    end;

  //Raw navmesh triangles
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to High(fNavMesh.Polygons) do
    begin

      K := fNavMesh.GetBestOwner(I);
      if K <> PLAYER_NONE then
        Col := fPlayers[K].FlagColor and $FFFFFF or $A0000000
      else
        Col := $60FF0000;
      fRenderAux.Triangle(
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[0]].X,
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[0]].Y,
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[1]].X,
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[1]].Y,
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[2]].X,
        fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[2]].Y, Col);
    end;

  //Raw navmesh mesh
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to High(fNavMesh.Polygons) do
    for K := 0 to High(fNavMesh.Polygons[I].Indices) do
    begin
      x1 := fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[K]].X;
      y1 := fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[K]].Y;
      J := (K + 1) mod Length(fNavMesh.Polygons[I].Indices);
      x2 := fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[J]].X;
      y2 := fNavMesh.fVertices[fNavMesh.Polygons[I].Indices[J]].Y;
      fRenderAux.Line(x1,y1,x2,y2, $FFFF8000, $F0F0);
    end;

  //Raw navmesh mesh
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to High(fNavMesh.fVertices) do
      fRenderAux.Text(fNavMesh.fVertices[I].X,fNavMesh.fVertices[I].Y, IntToStr(I), $FF000000);

  //Simplified obstacle outlines
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fSimpleOutlines.Count - 1 do
    for K := 0 to fSimpleOutlines.Shape[I].Count - 1 do
    with fSimpleOutlines.Shape[I] do
    begin
      TX := Nodes[(K + 1) mod Count].X;
      TY := Nodes[(K + 1) mod Count].Y;
      fRenderAux.Line(Nodes[K].X, Nodes[K].Y, TX, TY, $FF00FF00, $FF00);
    end;
end;


{ TKMNavMesh }

function TKMNavMesh.GetBestOwner(aIndex: Integer): TPlayerIndex;
var I, Best: Integer;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to MAX_PLAYERS - 1 do
  if Polygons[aIndex].fOwner[I] > Best then
  begin
    Best := I;
    Result := I;
  end;
end;


procedure TKMNavMesh.UpdateOwnership;
  function GetPolyByTile(X,Y: Word): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to High(Polygons) do
    with Polygons[I] do
    if KMPointInTriangle(KMPointI(X,Y), fVertices[Indices[0]], fVertices[Indices[1]], fVertices[Indices[2]]) then
    begin
      Result := I;
      Break;
    end;
  end;
var
  I, K: Integer;
  Poly: Integer;
begin
  for I := 0 to High(Polygons) do
    for K := 0 to MAX_PLAYERS - 1 do
      Polygons[I].fOwner[K] := PLAYER_NONE;

  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
  if fTerrain.Land[I,K].TileOwner <> PLAYER_NONE then
  begin
    Poly := GetPolyByTile(K,I);
    if Poly <> -1 then
      Inc(Polygons[Poly].fOwner[fTerrain.Land[I,K].TileOwner]);
  end;
end;


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin
  //Maybe we recalculate Influences or navmesh sectors once in a while

  if (aTick mod 60 = 0) then UpdateOwnership;
end;


end.
