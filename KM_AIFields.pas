unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, Graphics, Delaunay,
  KM_CommonClasses, KM_CommonTypes, KM_Terrain, KM_Defaults, KM_Player, KM_Utils, KM_Points, KM_PolySimplify;

type
  TKMWeightSegments = array of record
    A,B: TKMPoint;
    Weight: Single;
  end;

  //Strcucture to describe NavMesh layout
  TKMNavMesh = class
  private
    fNodeCount: Integer;
    fPolyCount: Integer;
    fNodes: array of record
      Loc: TKMPoint;
      Nearby: array of Word;
      Owner: array [0..MAX_PLAYERS-1] of Byte;
    end;
    //Edges: array of array [0..1] of Word;
    fPolygons: array of record
      Indices: array [0..2] of Word;
      //Nearby: array of Word; //Neighbour polygons
    end;
    procedure InitConnectivity;
    function GetBestOwner(aIndex: Integer): TPlayerIndex;
    procedure UpdateOwnership;
    procedure UpdateInfluence;
  public
    constructor Create(const aTriMesh: TKMTriMesh);
    procedure GetDefenceOutline(aOwner: TPlayerIndex; out aOutline: TKMWeightSegments);
    procedure UpdateState(aTick: Cardinal);
  end;

  //Influence maps, navmeshes, etc
  TKMAIFields = class
  private
    fPlayerCount: Integer;
    fShowNavMesh: Boolean;

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

    procedure UpdateNavMesh;
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  fAIFields: TKMAIFields;


implementation
uses KM_Game, KM_Log, KM_PlayersCollection, KM_RenderAux;


{ TKMNavMesh }
constructor TKMNavMesh.Create(const aTriMesh: TKMTriMesh);
var I: Integer;
begin
  inherited Create;

  fNodeCount := Length(aTriMesh.Vertices);
  fPolyCount := Length(aTriMesh.Polygons);

  //Bring triangulated mesh back
  SetLength(fNodes, fNodeCount);
  for I := 0 to fNodeCount - 1 do
    fNodes[I].Loc := KMPoint(aTriMesh.Vertices[I]);

  SetLength(fPolygons, fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    fPolygons[I].Indices[0] := aTriMesh.Polygons[I,0];
    fPolygons[I].Indices[1] := aTriMesh.Polygons[I,1];
    fPolygons[I].Indices[2] := aTriMesh.Polygons[I,2];
  end;

  InitConnectivity;
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
  {procedure DoConnectPolys(aPoly, N1, N2, N3: Word);
  var
    I: Integer;
  begin
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    if (I <> aPoly)
    and ((Indices[0] = N1) or (Indices[0] = N2) or (Indices[0] = N3)
      or (Indices[0] = N1) or (Indices[0] = N2) or (Indices[0] = N3)
      or (Indices[0] = N1) or (Indices[0] = N2) or (Indices[0] = N3))
    then
    begin
      SetLength(Nearby, Length(Nearby) + 1);
      Nearby[High(Nearby)] := aPoly;
    end;
  end;}
var I: Integer;
begin
  //Set nodes nearbies to fill influence field
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  begin
    DoConnectNodes(Indices[0], Indices[1], Indices[2]);
    DoConnectNodes(Indices[1], Indices[0], Indices[2]);
    DoConnectNodes(Indices[2], Indices[0], Indices[1]);
  end;

  //Set polys connectivity to be able to expand
  {for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
    DoConnectPolys(I, Indices[0], Indices[1], Indices[2]);}
end;


procedure TKMNavMesh.UpdateInfluence;
var
  OwnerID: TPlayerIndex;

  procedure DoFill(const aIndex: Integer);
  var K: Integer; ExpectedValue: Byte;
  begin
    with fNodes[aIndex] do
    for K := 0 to High(Nearby) do
    begin
      ExpectedValue := Max(Owner[OwnerID] - Trunc(KMLength(Loc, fNodes[Nearby[K]].Loc)) * 2, 0);
      if ExpectedValue > fNodes[Nearby[K]].Owner[OwnerID] then
      begin
        fNodes[Nearby[K]].Owner[OwnerID] := ExpectedValue;
        DoFill(Nearby[K]);
      end;
    end;
  end;
var
  I: Integer;
begin
  for OwnerID := 0 to MAX_PLAYERS - 1 do
    for I := 0 to fNodeCount - 1 do
      if fNodes[I].Owner[OwnerID] = 255 then
        DoFill(I);
end;


function TKMNavMesh.GetBestOwner(aIndex: Integer): TPlayerIndex;
var I, Best: Byte;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to MAX_PLAYERS - 1 do
  if fNodes[aIndex].Owner[I] > Best then
  begin
    Best := fNodes[aIndex].Owner[I];
    Result := I;
  end;
end;


procedure TKMNavMesh.GetDefenceOutline(aOwner: TPlayerIndex; out aOutline: TKMWeightSegments);
var
  I, K: Integer;
  ECount: Integer;
  Edges: array of array [0..1] of Integer;
  EdgeOpen: array of Boolean;
begin
  //1. Get ownership area
  //Collect all the edges that are within our ownership area
  //We include small polys that are close neighbours to get moderate area coverage
  ECount := 0;
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  if (fNodes[Indices[0]].Owner[aOwner] > 248)
  or (fNodes[Indices[1]].Owner[aOwner] > 248)
  or (fNodes[Indices[2]].Owner[aOwner] > 248) then
  begin
    SetLength(Edges, ECount + 3);
    Edges[ECount  , 0] := Indices[0];
    Edges[ECount  , 1] := Indices[1];
    Edges[ECount+1, 0] := Indices[1];
    Edges[ECount+1, 1] := Indices[2];
    Edges[ECount+2, 0] := Indices[2];
    Edges[ECount+2, 1] := Indices[0];
    Inc(ECount, 3);
  end;

  //2. Obtain suboptimal outline
  //Remove duplicate edges, that will leave us with an outline
  for I := 0 to ECount - 1 do
  if (Edges[I, 0] > -1) and (Edges[I, 1] > -1) then
  for K := I + 1 to ECount - 1 do
  if (Edges[K, 0] > -1) and (Edges[K, 1] > -1) then
  if (Edges[I, 0] = Edges[K, 1]) and (Edges[I, 1] = Edges[K, 0]) then
  begin
    Edges[I, 0] := -1;
    Edges[I, 1] := -1;
    Edges[K, 0] := -1;
    Edges[K, 1] := -1;
  end;

  //3. Detect and dismiss inner edges
  //Separate edges into open (having 2 polys) and closed (only 1 poly)
  //Once again we take advantage of the fact that polys built in CW order
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  for K := 0 to ECount - 1 do
  if (Edges[K, 0] <> -1) then
  if ((Edges[K, 0] = Indices[1]) and (Edges[K, 1] = Indices[0]))
  or ((Edges[K, 0] = Indices[2]) and (Edges[K, 1] = Indices[1]))
  or ((Edges[K, 0] = Indices[0]) and (Edges[K, 1] = Indices[2])) then
  begin
    //Mark outer edges
    Edges[K, 0] := -1000 - Edges[K, 0];
    Edges[K, 1] := -1000 - Edges[K, 1];
  end;
  K := 0;
  for I := 0 to ECount - 1 do
  if (Edges[I, 0] >= 0) then
  begin //Dismiss inner edges
    Edges[I, 0] := -1;
    Edges[I, 1] := -1;
  end
  else
  if (Edges[I, 0] < -1) then
  begin //Promote marked outer edges back
    Edges[I, 0] := -Edges[I, 0] - 1000;
    Edges[I, 1] := -Edges[I, 1] - 1000;
    Inc(K);
  end;

  //4. We can assemble suboptimal outline now
  SetLength(aOutline, K);
  K := 0;
  for I := 0 to ECount - 1 do
  if (Edges[I, 0] >= 0) then
  begin
    aOutline[K].A := fNodes[Edges[I,0]].Loc;
    aOutline[K].B := fNodes[Edges[I,1]].Loc;
    aOutline[K].Weight := 1;
    Inc(K);
  end;

  //5. Remove spans that face isolated areas

  //6. See if we can expand our area while reducing outline length
end;


procedure TKMNavMesh.UpdateOwnership;
  function GetPolyByTile(X,Y: Word): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    if KMPointInTriangle(KMPoint(X,Y), fNodes[Indices[0]].Loc, fNodes[Indices[1]].Loc, fNodes[Indices[2]].Loc) then
    begin
      Result := I;
      Break;
    end;
  end;
var
  I, K: Integer;
  Poly: Integer;
begin
  for I := 0 to fNodeCount - 1 do
    for K := 0 to MAX_PLAYERS - 1 do
      fNodes[I].Owner[K] := 0;

  for I := 1 to fTerrain.MapY - 1 do
  for K := 1 to fTerrain.MapX - 1 do
  if fTerrain.Land[I,K].TileOwner <> PLAYER_NONE then
  begin
    Poly := GetPolyByTile(K,I);
    if Poly <> -1 then
    begin
      fNodes[fPolygons[Poly].Indices[0]].Owner[fTerrain.Land[I,K].TileOwner] := 255;
      fNodes[fPolygons[Poly].Indices[1]].Owner[fTerrain.Land[I,K].TileOwner] := 255;
      fNodes[fPolygons[Poly].Indices[2]].Owner[fTerrain.Land[I,K].TileOwner] := 255;
    end;
  end;
end;


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin
  //Maybe we recalculate Influences or navmesh sectors once in a while

  if (aTick mod 60 = 0) then
  begin
    UpdateOwnership;
    UpdateInfluence;
  end;
end;


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

  if AI_GEN_NAVMESH then
  begin
    UpdateNavMesh;
    fNavMesh.UpdateOwnership;
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

  fNavMesh := TKMNavMesh.Create(fRawDelaunay);
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
  T1, T2: TKMPointF;
  Col: Cardinal;
  Outline: TKMWeightSegments;
begin
  //Raw obstacle outlines
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fRawOutlines2.Count - 1 do
    for K := 0 to fRawOutlines2.Shape[I].Count - 1 do
    with fRawOutlines2.Shape[I] do
      fRenderAux.Line(Nodes[K], Nodes[(K + 1) mod Count], $FFFF00FF);

  //NavMesh polys coverage
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fNavMesh.fPolyCount - 1 do
      fRenderAux.Triangle(
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[0]].Loc.X,
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[0]].Loc.Y,
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[1]].Loc.X,
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[1]].Loc.Y,
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[2]].Loc.X,
        fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[2]].Loc.Y, $60FF0000);

  //NavMesh edges
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fNavMesh.fPolyCount - 1 do
    with fNavMesh.fPolygons[I] do
    for K := 0 to 2 do
    begin
      T1 := KMPointF(fNavMesh.fNodes[Indices[K]].Loc);
      J := (K + 1) mod 2;
      T2 := KMPointF(fNavMesh.fNodes[Indices[J]].Loc);
      fRenderAux.Line(T1, T2, $FFFF8000, $F0F0);
    end;

  //NavMesh vertice ids
  {if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to High(fNavMesh.fVertices) do
      fRenderAux.Text(fNavMesh.fVertices[I].X,fNavMesh.fVertices[I].Y, IntToStr(I), $FF000000); //}

  //Simplified obstacle outlines
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fSimpleOutlines.Count - 1 do
    for K := 0 to fSimpleOutlines.Shape[I].Count - 1 do
    with fSimpleOutlines.Shape[I] do
      fRenderAux.Line(Nodes[K], Nodes[(K + 1) mod Count], $FF00FF00, $FF00);

  //NavMesh influences
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fNavMesh.fNodeCount - 1 do
    begin
      K := fNavMesh.GetBestOwner(I);
      if K <> PLAYER_NONE then
        Col := (fPlayers[K].FlagColor and $FFFFFF) or (fNavMesh.fNodes[I].Owner[K] shl 24)
      else
        Col := $FF000000;
      fRenderAux.Dot(
        fNavMesh.fNodes[I].Loc.X,
        fNavMesh.fNodes[I].Loc.Y, Col, 0.4);
    end;

  if AI_GEN_NAVMESH and fShowNavMesh then
  for I := 0 to MAX_PLAYERS - 1 do
  begin
    fNavMesh.GetDefenceOutline(I, Outline);
    for K := 0 to High(Outline) do
    begin
      fRenderAux.Line(Outline[K].A, Outline[K].B, $FF00F0F0);
      T1 := KMPointF(Outline[K].A);
      T2 := KMPerpendecular(Outline[K].A, Outline[K].B);
      fRenderAux.Line(T1, T2, $FF00F0F0);
    end;
  end;
end;


end.
