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
    function GetPolyByTile(X,Y: Word): Integer;
    function GetBestOwner(aIndex: Integer): TPlayerIndex;
    function GetEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
    procedure UpdateOwnership;
    procedure UpdateInfluence;
  public
    constructor Create(const aTriMesh: TKMTriMesh);
    procedure GetDefenceOutline(aOwner: TPlayerIndex; out aOutline: TKMWeightSegments);
    function EnemyPresence(aLoc: TKMpoint): Word;
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
    fSimpleOutlines: TKMShapesArray;
    fRawMesh: TKMTriMesh;

    fNavMesh: TKMNavMesh;

    procedure NavMeshGenerate;
    function GetBestOwner(X, Y: Word): TPlayerIndex;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterMissionInit;
    property NavMesh: TKMNavMesh read fNavMesh;
    procedure ExportInfluenceMaps;

    procedure UpdateInfluenceMaps;
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  fAIFields: TKMAIFields;


implementation
uses KM_PlayersCollection, KM_RenderAux;


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
      ExpectedValue := Max(Owner[OwnerID] - Round(KMLengthDiag(Loc, fNodes[Nearby[K]].Loc)), 0);
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
  for OwnerID := 0 to fPlayers.Count - 1 do
    for I := 0 to fNodeCount - 1 do
      if fNodes[I].Owner[OwnerID] = 255 then
        DoFill(I);
end;


function TKMNavMesh.GetPolyByTile(X,Y: Word): Integer;
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


function TKMNavMesh.GetEnemyPresence(aIndex: Integer; aOwner: TPlayerIndex): Word;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fPlayers.Count - 1 do
  if (I <> aOwner) and (fPlayers.CheckAlliance(aOwner, I) = at_Enemy) then
    Result := Result + fNodes[aIndex].Owner[I];
end;


function TKMNavMesh.EnemyPresence(aLoc: TKMpoint): Word;
begin
  //
end;


procedure TKMNavMesh.GetDefenceOutline(aOwner: TPlayerIndex; out aOutline: TKMWeightSegments);
const
  //Should be within 16 tiles and at least one corner within 8 tiles
  OWN_MARGIN = 248;
  OWN_THRESHOLD = 240;
var
  I, K: Integer;
  ECount: Integer;
  Edges: array of array [0..1] of Integer;
begin
  //1. Get ownership area
  //Collect edges of polys that are well within our ownership area
  ECount := 0;
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

  //4. Now we can assemble suboptimal outline from kept edges
  SetLength(aOutline, K);
  K := 0;
  for I := 0 to ECount - 1 do
  if (Edges[I, 0] >= 0) then
  begin
    aOutline[K].A := fNodes[Edges[I,0]].Loc;
    aOutline[K].B := fNodes[Edges[I,1]].Loc;
    aOutline[K].Weight := GetEnemyPresence(Edges[I,0], aOwner) + GetEnemyPresence(Edges[I,1], aOwner);
    Inc(K);
  end;

  //5. Remove spans that face isolated areas

  //6. See if we can expand our area while reducing outline length
end;


procedure TKMNavMesh.UpdateOwnership;
var
  I, K: Integer;
  Poly: Integer;
begin
  //Clear
  for I := 0 to fNodeCount - 1 do
    for K := 0 to fPlayers.Count - 1 do
      fNodes[I].Owner[K] := 0;

  //Ownership are players assets (houses, maybe fields?)
  for I := 0 to fPlayers.Count - 1 do
  for K := 0 to fPlayers[I].Houses.Count - 1 do
  begin
    Poly := GetPolyByTile(fPlayers[I].Houses[K].GetEntrance.X, fPlayers[I].Houses[K].GetPosition.Y - 1);
    if Poly <> -1 then
    begin
      fNodes[fPolygons[Poly].Indices[0]].Owner[I] := 255;
      fNodes[fPolygons[Poly].Indices[1]].Owner[I] := 255;
      fNodes[fPolygons[Poly].Indices[2]].Owner[I] := 255;
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

  fShowInfluenceMap := True;
  fShowNavMesh := False;
end;


destructor TKMAIFields.Destroy;
begin

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
    NavMeshGenerate;
    fNavMesh.UpdateOwnership;
  end;
end;


procedure TKMAIFields.NavMeshGenerate;
type
  TStepDirection = (sdNone, sdUp, sdRight, sdDown, sdLeft);
var
  fTileOutlines: TKMShapesArray;
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

    SetLength(fTileOutlines.Shape, fTileOutlines.Count + 1);
    fTileOutlines.Shape[fTileOutlines.Count].Count := 0;

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
      with fTileOutlines.Shape[fTileOutlines.Count] do
      begin
        if Length(Nodes) <= Count then
          SetLength(Nodes, Count + 32);
        Nodes[Count] := KMPointI(X, Y);
        Inc(Count);
      end;
    until((X = aStartX) and (Y = aStartY));

    //Do not include too small regions
    if fTileOutlines.Shape[fTileOutlines.Count].Count >= 12 then
      Inc(fTileOutlines.Count);
  end;

var
  fDelaunay: TDelaunay;
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

  fTileOutlines.Count := 0;
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

  SimplifyStraights(fTileOutlines, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fRawOutlines);

  with TKMSimplifyShapes.Create(2, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1)) do
  begin
    Execute(fRawOutlines, fSimpleOutlines);
    Free;
  end;

  //Fill Delaunay triangles
  fDelaunay := TDelaunay.Create(-1, -1, fTerrain.MapX, fTerrain.MapY);
  try
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

  ForceOutlines(fRawMesh, KMRect(0, 0, fTerrain.MapX-1, fTerrain.MapY-1), fSimpleOutlines);

  RemoveObstaclePolies(fRawMesh, fSimpleOutlines);

  RemoveFrame(fRawMesh);

  CheckForDegenerates(fRawMesh);//}

  Assert(Length(fRawMesh.Polygons) > 8);

  fNavMesh := TKMNavMesh.Create(fRawMesh);
end;


function TKMAIFields.GetBestOwner(X, Y: Word): TPlayerIndex;
var
  I: Integer;
  Best: Byte;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to fPlayers.Count - 1 do
  if fInfluenceMap[I,Y,X] > Best then
  begin
    Best := fInfluenceMap[I,Y,X];
    Result := I;
  end;
end;


procedure TKMAIFields.UpdateInfluenceMaps;
var
  J: Integer;
  procedure DoFill(X,Y: Integer; V: Word);
  begin
    if (V > fInfluenceMap[J, Y, X])
    and InRange(Y, 2, fTerrain.MapY - 2)
    and InRange(X, 2, fTerrain.MapX - 2)
    and (CanOwn in fTerrain.Land[Y,X].Passability) then
    begin
      fInfluenceMap[J, Y, X] := V;
      DoFill(X, Y-1, Max(V - 2, 0));
      DoFill(X-1, Y, Max(V - 2, 0));
      DoFill(X+1, Y, Max(V - 2, 0));
      DoFill(X, Y+1, Max(V - 2, 0));
      {DoFill(X-1, Y-1, Max(V - 3, 0));
      DoFill(X+1, Y-1, Max(V - 3, 0));
      DoFill(X-1, Y+1, Max(V - 3, 0));
      DoFill(X+1, Y+1, Max(V - 3, 0));}
    end;
  end;
var
  I, K, L: Integer;
  H: Integer;
begin
  if not AI_GEN_INFLUENCE_MAPS then Exit;
  Assert(fTerrain <> nil);

  //Update direct influence maps
  for J := 0 to fPlayerCount - 1 do
  begin
    for I := 1 to fTerrain.MapY - 1 do
    for K := 1 to fTerrain.MapX - 1 do
    begin
      fInfluenceMap[J, I, K] := Byte(fTerrain.Land[I, K].TileOwner = J) * 254;
    end;
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
    if fInfluenceMap[J, I, K] = 254 then
      DoFill(K,I,fInfluenceMap[J, I, K]+1);
  end;

  for J := 0 to fPlayerCount - 1 do
  begin
    L := -65535;
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
    begin
      fInfluenceMinMap[J, I, K] := fInfluenceMap[J, I, K];

      for H := 0 to fPlayerCount - 1 do
      if H <> J then
        fInfluenceMinMap[J, I, K] := fInfluenceMinMap[J, I, K] - fInfluenceMap[H, I, K];

      L := Max(L, fInfluenceMinMap[J, I, K]);
    end;
    for I := 2 to fTerrain.MapY - 2 do
    for K := 2 to fTerrain.MapX - 2 do
      fInfluenceMinMap[J, I, K] := fInfluenceMinMap[J, I, K] + (255 - L);
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


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  //Maybe we recalculate Influences or navmesh sectors once in a while

  if aTick mod 60 = 0 then
    UpdateInfluenceMaps;

  fNavMesh.UpdateState(aTick);
end;


//Render debug symbols
procedure TKMAIFields.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  T1, T2: TKMPointF;
  Col, Col2: Cardinal;
  Outline: TKMWeightSegments;
begin
  if AI_GEN_INFLUENCE_MAPS and fShowInfluenceMap then
    for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      Col := $80000000;
      J := GetBestOwner(K,I);
      if J <> PLAYER_NONE then
      begin
        Col := (fPlayers[J].FlagColor and $FFFFFF) or (Byte(Max(fInfluenceMinMap[J,I,K]-128, 0) * 2) shl 24);
      end;

      fRenderAux.Quad(K, I, Col);
    end;

  //Raw obstacle outlines
  if AI_GEN_NAVMESH and fShowNavMesh then
    for I := 0 to fRawOutlines.Count - 1 do
    for K := 0 to fRawOutlines.Shape[I].Count - 1 do
    with fRawOutlines.Shape[I] do
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
      Col := $FF000000;
      Col2 := $00000000;

      K := fNavMesh.GetBestOwner(I);
      if K <> PLAYER_NONE then
      begin
        Col := (fPlayers[K].FlagColor and $FFFFFF) or (Byte(Max(fNavMesh.fNodes[I].Owner[K]-128, 0) * 2) shl 24);
        Col2 := IfThen(fNavMesh.fNodes[I].Owner[K] = 255, $FFFFFFFF);
      end;

      fRenderAux.CircleOnTerrain(
        fNavMesh.fNodes[I].Loc.X,
        fNavMesh.fNodes[I].Loc.Y, 0.4, Col, Col2);
    end;

  if AI_GEN_NAVMESH and fShowNavMesh then
  for I := 0 to fPlayers.Count - 1 do
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
