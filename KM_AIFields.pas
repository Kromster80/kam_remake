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
      //Area: Word; //Area of this polygon
      {Neighbours: array of record //Neighbour polygons
        Index: Word; //Index of polygon
        Touch: Byte; //Length of border between
      end;}
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
  TX, TY: Single;
  x1,x2,y1,y2: Single;
  Col: Cardinal;
begin
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

  //NavMesh polys
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
    for K := 0 to 2 do
    begin
      x1 := fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[K]].Loc.X;
      y1 := fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[K]].Loc.Y;
      J := (K + 1) mod 2;
      x2 := fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[J]].Loc.X;
      y2 := fNavMesh.fNodes[fNavMesh.fPolygons[I].Indices[J]].Loc.Y;
      fRenderAux.Line(x1,y1,x2,y2, $FFFF8000, $F0F0);
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
    begin
      TX := Nodes[(K + 1) mod Count].X;
      TY := Nodes[(K + 1) mod Count].Y;
      fRenderAux.Line(Nodes[K].X, Nodes[K].Y, TX, TY, $FF00FF00, $FF00);
    end;

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
        fNavMesh.fNodes[I].Loc.Y, Col, 2);
    end;
end;


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
  procedure DoConnect(I, N1, N2: Word);
  begin
    with fNodes[I] do
    begin
      SetLength(Nearby, Length(Nearby) + 2);
      Nearby[High(Nearby)-1] := N1;
      Nearby[High(Nearby)] := N2;
    end;
  end;
var I: Integer;
begin
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  begin
    DoConnect(Indices[0], Indices[1], Indices[2]);
    DoConnect(Indices[1], Indices[0], Indices[2]);
    DoConnect(Indices[2], Indices[0], Indices[1]);
  end;
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
begin
  //todo: Maybe use vertices floodfill for analysis?
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

  UpdateInfluence;
end;


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin
  //Maybe we recalculate Influences or navmesh sectors once in a while

  if (aTick mod 60 = 0) then UpdateOwnership;
end;


end.
