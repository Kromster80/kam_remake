unit KM_NavMesh;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics, Delaunay,
  KM_CommonClasses, KM_Defaults,
  KM_Points, KM_PolySimplify;

type
  TKMWeightSegments = array of record
    A,B: TKMPoint;
    Weight: Single;
  end;

  //Strcucture to describe NavMesh layout
  TKMNavMesh = class
  private
    //Keep a copy of these temp arrays for debug rendering
    fRawOutlines: TKMShapesArray;
    fSimpleOutlines: TKMShapesArray;
    fRawMesh: TKMTriMesh;

    //Working data
    fNodeCount: Integer;
    fPolyCount: Integer;
    fNodes: array of record
      Loc: TKMPoint;
      Nearby: array of Word; //Indexes of connected nodes
      Owner: array [0..MAX_PLAYERS-1] of Byte;
    end;
    fPolygons: array of record
      Indices: array [0..2] of Word;
      NearbyCount: Byte; //could be 0 .. 3
      Nearby: array [0..2] of Word; //Neighbour polygons
    end;

    //Process involves many steps executed in a functional way
    procedure GenerateTileOutline(out aTileOutlines: TKMShapesArray);
    procedure TriangulateOutlines;
    procedure AssembleNavMesh;

    procedure InitConnectivity;
    function GetBestOwner(aIndex: Integer): TPlayerIndex;
    function NodeEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
    function PolyEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
    procedure UpdateOwnership;
  public
    procedure Init;
    procedure GetDefenceOutline(aOwner: TPlayerIndex; out aOutline1, aOutline2: TKMWeightSegments);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


implementation
uses KM_AIFields, KM_PlayersCollection, KM_RenderAux, KM_Terrain;


{ TKMNavMesh }
procedure TKMNavMesh.Init;
var
  TileOutlines: TKMShapesArray;
begin
  //Convert tilemap into vector outlines
  GenerateTileOutline(TileOutlines);

  //Remove extra points on straights
  SimplifyStraights(TileOutlines, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fRawOutlines);

  //Perform outlines simplification
  with TKMSimplifyShapes.Create(2, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1)) do
  begin
    Execute(fRawOutlines, fSimpleOutlines);
    Free;
  end;

  //Triangulate everything
  TriangulateOutlines;

  //Force mesh triangulation to be along outlines
  ForceOutlines(fRawMesh, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fSimpleOutlines);

  //Remove polygons within obstacles
  RemoveObstaclePolies(fRawMesh, fSimpleOutlines);

  //Remove outside frame (required by Delaunay)
  RemoveFrame(fRawMesh);

  //Make sure we dont have degenerate polys left
  CheckForDegenerates(fRawMesh);

  Assert(Length(fRawMesh.Polygons) > 8);

  //Fill in NavMesh structure
  AssembleNavMesh;
end;


procedure TKMNavMesh.GenerateTileOutline(out aTileOutlines: TKMShapesArray);
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

    SetLength(aTileOutlines.Shape, aTileOutlines.Count + 1);
    aTileOutlines.Shape[aTileOutlines.Count].Count := 0;

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
      with aTileOutlines.Shape[aTileOutlines.Count] do
      begin
        if Length(Nodes) <= Count then
          SetLength(Nodes, Count + 32);
        Nodes[Count] := KMPointI(X, Y);
        Inc(Count);
      end;
    until((X = aStartX) and (Y = aStartY));

    //Do not include too small regions
    if aTileOutlines.Shape[aTileOutlines.Count].Count >= 12 then
      Inc(aTileOutlines.Count);
  end;

var
  I, K: Integer;
  C1, C2, C3, C4: Boolean;
begin
  SetLength(Tmp, fTerrain.MapY+1, fTerrain.MapX+1);

  //Copy map to temp array where we can use Keys 0-1-2 for internal purposes
  //0 - no obstacle
  //1 - parsed obstacle
  //2 - non-parsed obstacle
  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
    Tmp[I,K] := 2 - Byte(CanOwn in fTerrain.Land[I,K].Passability) * 2;

  aTileOutlines.Count := 0;
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
end;


procedure TKMNavMesh.TriangulateOutlines;
var
  fDelaunay: TDelaunay;
  I, K, L, M: Integer;
  MeshDensityX, MeshDensityY: Byte;
  SizeX, SizeY: Word;
  PX,PY,TX,TY: Integer;
  Skip: Boolean;
begin
  //Fill area with Delaunay triangles
  fDelaunay := TDelaunay.Create(-1, -1, fTerrain.MapX, fTerrain.MapY);
  try
    //Points that are closer than that will be skipped
    fDelaunay.Tolerance := 1;
    for I := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[I] do
      for K := 0 to Count - 1 do
        fDelaunay.AddPoint(Nodes[K].X, Nodes[K].Y);

    //Add more points along edges to get even density
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

    //Add more supporting points into the middle to get more even mesh
    //Tolerance must be a little higher than longest span we expect from polysimplification
    //so that not a single node was placed on an outline segment (otherwise RemObstaclePolys will not be able to trace outlines)
    fDelaunay.Tolerance := 7;
    for I := 1 to MeshDensityY - 1 do
    for K := 1 to MeshDensityX - Byte(I mod 2 = 1) - 1 do
      fDelaunay.AddPoint(Round(SizeX / MeshDensityX * (K + Byte(I mod 2 = 1) / 2)), Round(SizeY / MeshDensityY * I));

    //Do the Delaunay magick
    fDelaunay.Mesh;

    //Get triangulated mesh back
    SetLength(fRawMesh.Vertices, fDelaunay.VerticeCount);
    for I := 0 to fDelaunay.VerticeCount - 1 do
    begin
      fRawMesh.Vertices[I].X := Round(fDelaunay.Vertex[I].X);
      fRawMesh.Vertices[I].Y := Round(fDelaunay.Vertex[I].Y);
    end;
    SetLength(fRawMesh.Polygons, fDelaunay.PolyCount);
    for I := 0 to fDelaunay.PolyCount - 1 do
    begin
      fRawMesh.Polygons[I,0] := fDelaunay.Triangle^[I].vv0;
      fRawMesh.Polygons[I,1] := fDelaunay.Triangle^[I].vv1;
      fRawMesh.Polygons[I,2] := fDelaunay.Triangle^[I].vv2;
    end;
  finally
    FreeAndNil(fDelaunay);
  end;
end;


procedure TKMNavMesh.AssembleNavMesh;
var
  I: Integer;
begin
  fNodeCount := Length(fRawMesh.Vertices);
  fPolyCount := Length(fRawMesh.Polygons);

  //Bring triangulated mesh back
  SetLength(fNodes, fNodeCount);
  for I := 0 to fNodeCount - 1 do
    fNodes[I].Loc := KMPoint(fRawMesh.Vertices[I]);

  SetLength(fPolygons, fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    fPolygons[I].Indices[0] := fRawMesh.Polygons[I,0];
    fPolygons[I].Indices[1] := fRawMesh.Polygons[I,1];
    fPolygons[I].Indices[2] := fRawMesh.Polygons[I,2];
  end;

  InitConnectivity;

  UpdateOwnership;
end;


procedure TKMNavMesh.InitConnectivity;
  procedure DoConnectNodes(I, N1, N2: Word);
  begin
    with fNodes[I] do
    begin
      SetLength(Nearby, Length(Nearby) + 2);
      Nearby[High(Nearby)-1] := N1;
      Nearby[High(Nearby)] := N2;
    end;
  end;
  procedure DoConnectPolys(aPoly, N1, N2, N3: Word);
  var
    I: Integer;
    K1,K2,K3: Word;
  begin
    for I := 0 to fPolyCount - 1 do
    if (I <> aPoly) then
    with fPolygons[I] do
    begin
      K1 := Indices[0];
      K2 := Indices[1];
      K3 := Indices[2];
      //Need to check all combinations
      if ((K1 = N2) and (K2 = N1)) or ((K1 = N1) and (K2 = N3)) or ((K1 = N3) and (K2 = N2))
      or ((K2 = N2) and (K3 = N1)) or ((K2 = N1) and (K3 = N3)) or ((K2 = N3) and (K3 = N2))
      or ((K3 = N2) and (K1 = N1)) or ((K3 = N1) and (K1 = N3)) or ((K3 = N3) and (K1 = N2))
      then
      begin
        Nearby[NearbyCount] := aPoly;
        Inc(NearbyCount);
      end;
    end;
  end;
var
  I: Integer;
begin
  //Set nodes nearbys to fill influence field
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  begin
    DoConnectNodes(Indices[0], Indices[1], Indices[2]);
    DoConnectNodes(Indices[1], Indices[0], Indices[2]);
    DoConnectNodes(Indices[2], Indices[0], Indices[1]);
  end;

  //Erase
  for I := 0 to fPolyCount - 1 do
  begin
    fPolygons[I].NearbyCount := 0;
    fPolygons[I].Nearby[0] := 0;
    fPolygons[I].Nearby[1] := 0;
    fPolygons[I].Nearby[2] := 0;
  end;

  //Set polys connectivity to be able to expand
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
    DoConnectPolys(I, Indices[0], Indices[1], Indices[2]);
end;


//Copy ownership values from influence map
//  for now those values are more accurate
procedure TKMNavMesh.UpdateOwnership;
var
  I, K: Integer;
begin
  if not AI_GEN_NAVMESH then Exit;

  for I := 0 to fNodeCount - 1 do
    for K := 0 to fPlayers.Count - 1 do
      fNodes[I].Owner[K] := Min(255, Max(
        Max(fAIFields.Influences.Ownership[K, Max(fNodes[I].Loc.Y, 1), Max(fNodes[I].Loc.X, 1)],
            fAIFields.Influences.Ownership[K, Max(fNodes[I].Loc.Y, 1), Min(fNodes[I].Loc.X+1, fTerrain.MapX - 1)]),
        Max(fAIFields.Influences.Ownership[K, Min(fNodes[I].Loc.Y+1, fTerrain.MapY - 1), Max(fNodes[I].Loc.X, 1)],
            fAIFields.Influences.Ownership[K, Min(fNodes[I].Loc.Y+1, fTerrain.MapY - 1), Min(fNodes[I].Loc.X+1, fTerrain.MapX - 1)])
            ));
end;


function TKMNavMesh.GetBestOwner(aIndex: Integer): TPlayerIndex;
var
  I: Integer;
  Best: Byte;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to fPlayers.Count - 1 do
  if fNodes[aIndex].Owner[I] > Best then
  begin
    Best := fNodes[aIndex].Owner[I];
    Result := I;
  end;
end;


function TKMNavMesh.NodeEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fPlayers.Count - 1 do
  if (I <> aOwner) and (fPlayers.CheckAlliance(aOwner, I) = at_Enemy) then
    Result := Result + fNodes[aIndex].Owner[I];
end;


function TKMNavMesh.PolyEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fPlayers.Count - 1 do
  if (I <> aOwner) and (fPlayers.CheckAlliance(aOwner, I) = at_Enemy) then
    Result := Result + (fNodes[fPolygons[aIndex].Indices[0]].Owner[I]
                      + fNodes[fPolygons[aIndex].Indices[1]].Owner[I]
                      + fNodes[fPolygons[aIndex].Indices[2]].Owner[I]) div 3;
end;


procedure TKMNavMesh.GetDefenceOutline(aOwner: TPlayerIndex; out aOutline1, aOutline2: TKMWeightSegments);
const
  AP_CLEAR = 0;
  AP_SEED = 255;
var
  AreaID: Byte;
  AreaPolys: array of Byte;
  AreaEnemy: array [1..254] of Word;

  procedure AddPoly(var aEdges: TKMEdgesArray; const aIndex: Integer);
  begin
    AreaPolys[aIndex] := AP_SEED;
    with fPolygons[aIndex] do
    begin
      SetLength(aEdges.Nodes, aEdges.Count + 3);
      aEdges.Nodes[aEdges.Count  , 0] := Indices[0];
      aEdges.Nodes[aEdges.Count  , 1] := Indices[1];
      aEdges.Nodes[aEdges.Count+1, 0] := Indices[1];
      aEdges.Nodes[aEdges.Count+1, 1] := Indices[2];
      aEdges.Nodes[aEdges.Count+2, 0] := Indices[2];
      aEdges.Nodes[aEdges.Count+2, 1] := Indices[0];
      Inc(aEdges.Count, 3);
    end;
  end;

  procedure ConvertEdgesToOutline(const aInEdges: TKMEdgesArray; out aOutline: TKMWeightSegments);
  var
    I,K: Integer;
    Edges: TKMEdgesArray;
  begin
    //Duplicate to avoid spoiling (we need to copy arrays manually)
    Edges.Count := aInEdges.Count;
    SetLength(Edges.Nodes, Edges.Count);
    for I := 0 to aInEdges.Count - 1 do
    begin
      Edges.Nodes[I][0] := aInEdges.Nodes[I][0];
      Edges.Nodes[I][1] := aInEdges.Nodes[I][1];
    end;

    //Remove duplicate Edges, that will leave us with an outline
    for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] > -1) and (Edges.Nodes[I, 1] > -1) then
    for K := I + 1 to Edges.Count - 1 do
    if (Edges.Nodes[K, 0] > -1) and (Edges.Nodes[K, 1] > -1) then
    if (Edges.Nodes[I, 0] = Edges.Nodes[K, 1]) and (Edges.Nodes[I, 1] = Edges.Nodes[K, 0]) then
    begin
      Edges.Nodes[I, 0] := -1;
      Edges.Nodes[I, 1] := -1;
      Edges.Nodes[K, 0] := -1;
      Edges.Nodes[K, 1] := -1;
    end;

    //3. Detect and dismiss inner Edges
    //Separate Edges into open (having 2 polys) and closed (only 1 poly)
    //Once again we take advantage of the fact that polys built in CW order
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    for K := 0 to Edges.Count - 1 do
    if (Edges.Nodes[K, 0] <> -1) then
    if ((Edges.Nodes[K, 0] = Indices[1]) and (Edges.Nodes[K, 1] = Indices[0]))
    or ((Edges.Nodes[K, 0] = Indices[2]) and (Edges.Nodes[K, 1] = Indices[1]))
    or ((Edges.Nodes[K, 0] = Indices[0]) and (Edges.Nodes[K, 1] = Indices[2])) then
    begin
      //Mark outer Edges
      Edges.Nodes[K, 0] := -1000 - Edges.Nodes[K, 0];
      Edges.Nodes[K, 1] := -1000 - Edges.Nodes[K, 1];
    end;
    K := 0;
    for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] >= 0) then
    begin //Dismiss inner Edges
      Edges.Nodes[I, 0] := -1;
      Edges.Nodes[I, 1] := -1;
    end
    else
    if (Edges.Nodes[I, 0] < -1) then
    begin //Promote marked outer Edges back
      Edges.Nodes[I, 0] := -Edges.Nodes[I, 0] - 1000;
      Edges.Nodes[I, 1] := -Edges.Nodes[I, 1] - 1000;
      Inc(K);
    end;

    //4. Now we can assemble suboptimal outline from kept Edges
    SetLength(aOutline, K);
    K := 0;
    for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] >= 0) then
    begin
      aOutline[K].A := fNodes[Edges.Nodes[I,0]].Loc;
      aOutline[K].B := fNodes[Edges.Nodes[I,1]].Loc;
      aOutline[K].Weight := NodeEnemyPresence(Edges.Nodes[I,0], aOwner) + NodeEnemyPresence(Edges.Nodes[I,1], aOwner);
      Inc(K);
    end;
  end;

  procedure FloodFillPolys(aIndex: Integer);
  var I: Integer;
  begin
    with fPolygons[aIndex] do
    for I := 0 to NearbyCount - 1 do
    if (AreaPolys[Nearby[I]] = AP_CLEAR) then
    begin
      AreaPolys[Nearby[I]] := AreaID; //Mark as explored
      AreaEnemy[AreaID] := Max(AreaEnemy[AreaID], PolyEnemyPresence(Nearby[I], aOwner));
      FloodFillPolys(Nearby[I]);
    end;
  end;

var
  I,K: Integer;
  Edges: TKMEdgesArray;
begin
  SetLength(AreaPolys, fPolyCount);

  //1. Get ownership area
  //Collect edges of polys that are well within our ownership area
  Edges.Count := 0;
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  if ((fNodes[Indices[0]].Owner[aOwner] >= OWN_MARGIN)
    or (fNodes[Indices[1]].Owner[aOwner] >= OWN_MARGIN)
    or (fNodes[Indices[2]].Owner[aOwner] >= OWN_MARGIN))
  and (fNodes[Indices[0]].Owner[aOwner] >= OWN_THRESHOLD)
  and (fNodes[Indices[1]].Owner[aOwner] >= OWN_THRESHOLD)
  and (fNodes[Indices[2]].Owner[aOwner] >= OWN_THRESHOLD)
  and (GetBestOwner(Indices[0]) = aOwner)
  and (GetBestOwner(Indices[1]) = aOwner)
  and (GetBestOwner(Indices[2]) = aOwner) then
    AddPoly(Edges, I);


  //Obtain suboptimal outline
  ConvertEdgesToOutline(Edges, aOutline1);

  //5. Remove spans that face isolated areas

  for I := Low(AreaEnemy) to High(AreaEnemy) do
    AreaEnemy[I] := 0;

  //Mark inner polys
  //(already made above)

  //Floodfill outer polys skipping inner ones, remember best enemy influence
  AreaID := 0;
  for I := 0 to fPolyCount - 1 do
  if (AreaPolys[I] = AP_SEED) then
    for K := 0 to fPolygons[I].NearbyCount - 1 do
    if (AreaPolys[fPolygons[I].Nearby[K]] = AP_CLEAR) then
    begin
      Inc(AreaID);
      FloodFillPolys(fPolygons[I].Nearby[K]);
    end;

  //if enemy influence < 128 then mark as isolated
  for I := 0 to fPolyCount - 1 do
  if (AreaPolys[I] <> AP_CLEAR)
  and (AreaPolys[I] <> AP_SEED)
  and (AreaEnemy[AreaPolys[I]] < 128) then
    AddPoly(Edges, I);

  ConvertEdgesToOutline(Edges, aOutline2);

  //6. See if we can expand our area while reducing outline length

  //7. Deal with allies
  //   Two players could be on same island and share defence lines,
  //   also they dont need defence line between them
end;


procedure TKMNavMesh.Save(SaveStream: TKMemoryStream);
var
  I,K: Integer;
begin
  SaveStream.Write('NavMesh');

  SaveStream.Write(fNodeCount);
  for I := 0 to fNodeCount - 1 do
  begin
    SaveStream.Write(fNodes[I].Loc);

    SaveStream.Write(Integer(Length(fNodes[I].Nearby)));
    for K := 0 to Length(fNodes[I].Nearby) - 1 do
      SaveStream.Write(fNodes[I].Nearby[K]);

    SaveStream.Write(fNodes[I].Owner, SizeOf(fNodes[I].Owner));
  end;

  SaveStream.Write(fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    SaveStream.Write(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
    SaveStream.Write(fPolygons[I].NearbyCount);
    SaveStream.Write(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
  end;
end;


procedure TKMNavMesh.Load(LoadStream: TKMemoryStream);
var
  I,K,H: Integer;
begin
  LoadStream.ReadAssert('NavMesh');

  LoadStream.Read(fNodeCount);
  SetLength(fNodes, fNodeCount);
  for I := 0 to fNodeCount - 1 do
  begin
    LoadStream.Read(fNodes[I].Loc);

    LoadStream.Read(H);
    SetLength(fNodes[I].Nearby, H);
    for K := 0 to H - 1 do
      LoadStream.Read(fNodes[I].Nearby[K]);

    LoadStream.Read(fNodes[I].Owner, SizeOf(fNodes[I].Owner));
  end;

  LoadStream.Read(fPolyCount);
  SetLength(fPolygons, fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    LoadStream.Read(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
    LoadStream.Read(fPolygons[I].NearbyCount);
    LoadStream.Read(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
  end;
end;


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin
  if aTick mod 600 = 0 then
    UpdateOwnership;
end;


//Render debug symbols
procedure TKMNavMesh.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  T1, T2: TKMPointF;
  //Col, Col2: Cardinal;
  //Sz: Single;
  Outline1, Outline2: TKMWeightSegments;
begin
  if not AI_GEN_NAVMESH then Exit;

  //Raw obstacle outlines
  if OVERLAY_NAVMESH then
    for I := 0 to fRawOutlines.Count - 1 do
    for K := 0 to fRawOutlines.Shape[I].Count - 1 do
    with fRawOutlines.Shape[I] do
      fRenderAux.Line(Nodes[K], Nodes[(K + 1) mod Count], $FFFF00FF);

  //NavMesh polys coverage
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
      fRenderAux.Triangle(
        fNodes[fPolygons[I].Indices[0]].Loc.X,
        fNodes[fPolygons[I].Indices[0]].Loc.Y,
        fNodes[fPolygons[I].Indices[1]].Loc.X,
        fNodes[fPolygons[I].Indices[1]].Loc.Y,
        fNodes[fPolygons[I].Indices[2]].Loc.X,
        fNodes[fPolygons[I].Indices[2]].Loc.Y, $60FF0000);

  //NavMesh edges
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    for K := 0 to 2 do
    begin
      T1 := KMPointF(fNodes[Indices[K]].Loc);
      J := (K + 1) mod 2;
      T2 := KMPointF(fNodes[Indices[J]].Loc);
      fRenderAux.Line(T1, T2, $FFFF8000, $F0F0);
    end;

  //NavMesh vertice ids
  {if OVERLAY_NAVMESH then
    for I := 0 to High(fVertices) do
      fRenderAux.Text(fVertices[I].X,fVertices[I].Y, IntToStr(I), $FF000000); //}

  {//NavMesh polys ids
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    begin
      T1.X := (fNodes[Indices[0]].Loc.X + fNodes[Indices[1]].Loc.X + fNodes[Indices[2]].Loc.X) / 3;
      T1.Y := (fNodes[Indices[0]].Loc.Y + fNodes[Indices[1]].Loc.Y + fNodes[Indices[2]].Loc.Y) / 3;
      fRenderAux.Text(Round(T1.X), Round(T1.Y) + 1, IntToStr(I), $FF000000);
    end;//}

  {//Simplified obstacle outlines
  if OVERLAY_NAVMESH then
    for I := 0 to fSimpleOutlines.Count - 1 do
    for K := 0 to fSimpleOutlines.Shape[I].Count - 1 do
    with fSimpleOutlines.Shape[I] do
      fRenderAux.Line(Nodes[K], Nodes[(K + 1) mod Count], $FF00FF00, $FF00);//}

  {//NavMesh influences
  if OVERLAY_NAVMESH then
    for I := 0 to fNodeCount - 1 do
    begin
      K := GetBestOwner(I);
      if K <> PLAYER_NONE then
      begin
        Col := fPlayers[K].FlagColor or $FF000000;
        Col2 := IfThen(fNodes[I].Owner[K] = 255, $FFFFFFFF);
        Sz := Max(fNodes[I].Owner[K] - 128, 0) / 64;

        fRenderAux.CircleOnTerrain(
          fNodes[I].Loc.X,
          fNodes[I].Loc.Y, Sz, Col, Col2);
      end;
    end;//}

  //Defence outlines
  if OVERLAY_DEFENCES then
    for I := 0 to fPlayers.Count - 1 do
    begin
      GetDefenceOutline(I, Outline1, Outline2);

      for K := 0 to High(Outline1) do
      begin
        fRenderAux.Line(Outline1[K].A, Outline1[K].B, $FF00FFFF, $FF00);
      end;

      for K := 0 to High(Outline2) do
      begin
        fRenderAux.Line(Outline2[K].A, Outline2[K].B, $B000FF00);
        T1 := KMPointF(Outline2[K].A);
        T2 := KMPerpendecular(Outline2[K].A, Outline2[K].B);
        fRenderAux.Line(T1, T2, $B000FF00);
      end;
    end;
end;


end.
