unit KM_TerrainFinder;
{$I KaM_Remake.inc}
interface
uses Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points;

type
  //General implementation of algorithm that finds something on terrain without building a route to it
  TKMTerrainFinderCommon = class
  private
    MapX, MapY: Word;
    fStart: TKMPointArray;
    fRadius: Byte;
    fMaxCount: Word;
    BestDist: Byte;
    BestLoc: TKMPoint;
    Visited: array of array of Byte;

    //Temp for list that we need to fill
    fLocs: TKMPointTagList;

    procedure InitVisited;
    procedure SaveTile(const X,Y: Word; aWalkDistance: Byte);
    procedure UseFinder;
  protected
    fPassability: TPassabilitySet;
    function CanWalkHere(const X,Y: Word): Boolean; virtual; abstract;
    function CanUse(const X,Y: Word): Boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassabilitySet; out aEnd: TKMPoint): Boolean; overload;
    procedure FindNearest(aStart: TKMPointArray; aRadius: Byte; aPassability: TPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    procedure GetTilesWithinDistance(aStart: TKMPoint; aRadius: Byte; aPass: TPassability; aList: TKMPointList);
    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
  end;


  //Simple algorithm implementation required to override abstract methods
  TKMTerrainFinder = class(TKMTerrainFinderCommon)
  protected
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  end;


implementation
uses KM_ResourceMapElements, KM_Terrain;


{ TKMTerrainFinder }
constructor TKMTerrainFinderCommon.Create;
begin
  inherited;

  MapX := fTerrain.MapX;
  MapY := fTerrain.MapY;
  SetLength(Visited, MapY+1, MapX+1);
end;


destructor TKMTerrainFinderCommon.Destroy;
begin

  inherited;
end;


function TKMTerrainFinderCommon.FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassabilitySet; out aEnd: TKMPoint): Boolean;
begin
  SetLength(fStart, 1);
  fStart[0] := aStart;
  fRadius := aRadius;
  fPassability := aPassability;
  fMaxCount := 1;

  UseFinder;

  aEnd := BestLoc;
  Result := BestLoc.X <> 0;
end;


procedure TKMTerrainFinderCommon.FindNearest(aStart: TKMPointArray; aRadius: Byte; aPassability: TPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fStart := aStart;
  fRadius := aRadius;
  fPassability := aPassability;
  fMaxCount := aMaxCount;
  fLocs := aLocs;

  fLocs.Clear;
  UseFinder;

  //Prepare output
  fLocs.SortByTag;
  fLocs.Count := Min(fMaxCount, fLocs.Count);
end;


procedure TKMTerrainFinderCommon.SaveTile(const X, Y: Word; aWalkDistance: Byte);
var I: Integer;
begin
  if fMaxCount = 1 then
  begin
    BestDist := aWalkDistance;
    BestLoc := KMPoint(X,Y);
  end
  else
  begin
    //Use parent class to check only for KMPoint but ignore Tag
    I := TKMPointList(fLocs).IndexOf(KMPoint(X,Y));
    if I <> -1 then
      fLocs.Tag[I] := aWalkDistance //New is always smaller
    else
      fLocs.AddEntry(KMPoint(X,Y), aWalkDistance);

    //If we have enough points we can be picky and take only better distances
    if fLocs.Count >= fMaxCount then
      BestDist := aWalkDistance;
  end;
end;


procedure TKMTerrainFinderCommon.InitVisited;
var
  I,K: Integer;
begin
  //Initially all tiles are unexplored
  for I := 1 to MapY do
  for K := 1 to MapX do
    Visited[I,K] := 255;
end;


procedure TKMTerrainFinderCommon.UseFinder;
  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(const X,Y: Word; aWalkDistance: Byte);
  begin
    //If new path is longer than old we don't care about it
    if (aWalkDistance >= Visited[Y,X]) then Exit;

    //Check if we can walk through this tile
    if not CanWalkHere(X,Y) then Exit;

    //Check if we can take this tile
    if CanUse(X,Y) then
      SaveTile(X,Y,aWalkDistance);

    //Mark this tile as visited
    Visited[Y,X] := aWalkDistance;

    //Run again on surrounding tiles
    //We check only 4 neighbors, because that x6 times faster than 8 neighbors
    //and we don't really care for perfect circle test
    if (aWalkDistance + 1 <= BestDist) then
    begin
      if X-1 >= 1 then     Visit(X-1, Y, aWalkDistance+1);
      if Y-1 >= 1 then     Visit(X, Y-1, aWalkDistance+1);
      if Y+1 <= MapY then  Visit(X, Y+1, aWalkDistance+1);
      if X+1 <= MapX then  Visit(X+1, Y, aWalkDistance+1);
    end;
  end;
var
  I: Integer;
begin
  //Assign Rad to local variable,
  //when we find better Loc we reduce the Rad to skip any farther Locs
  BestDist := fRadius;
  BestLoc := KMPoint(0,0);

  InitVisited;

  //Starting tile is at walking distance zero
  //When using multiple points they will overlap if new WalkDistance is shorter
  for I := Low(fStart) to High(fStart) do
    Visit(fStart[I].X, fStart[I].Y, 0);
end;


//Fills aList with all of the tiles within aRadius of aStart with aPass using either a
//simple radius or a floodfill walking distance calculation depending on USE_WALKING_DISTANCE
procedure TKMTerrainFinderCommon.GetTilesWithinDistance(aStart: TKMPoint; aRadius: Byte; aPass: TPassability; aList: TKMPointList);
const
  STRAIGHT_COST = 5;
  DIAG_COST = Round(STRAIGHT_COST * 1.41);
  MAX_RAD = (255 - DIAG_COST) div STRAIGHT_COST;

  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(X,Y: Word; aWalkDistance: Byte);
  var Xt, Yt: Word;
  begin
    //Test whether this tile is valid and exit immediately if not
    //Multiply the radius by 10 because of diagonal approximation (straight=10, diagonal=14)
    if (aWalkDistance > aRadius * STRAIGHT_COST) or
    not (aPass in fTerrain.Land[Y,X].Passability) then Exit;
    Xt := aStart.X - X + aRadius;
    Yt := aStart.Y - Y + aRadius;
    if (aWalkDistance >= Visited[Yt,Xt]) then Exit;

    //Only add to results once (255 is the intial value)
    if Visited[Yt,Xt] = 255 then
      aList.AddEntry(KMPoint(X,Y));

    //Mark this tile as visited
    Visited[Yt,Xt] := aWalkDistance;

    //Run again on surrounding tiles
    //We use +10 for straights and +14 for diagonals rather than +1 and +1.41 then div by 10 in
    //calculations so we can still store it as bytes to save space and time
    if X-1 >= 1 then
    begin
      if (Y-1 >= 1) and not MapElem[fTerrain.Land[Y,X].Obj].DiagonalBlocked then
        Visit(X-1, Y-1, aWalkDistance + DIAG_COST);
      Visit(X-1, Y, aWalkDistance + STRAIGHT_COST);
      if (Y+1 <= MapY) and not MapElem[fTerrain.Land[Y+1,X].Obj].DiagonalBlocked then
        Visit(X-1,Y+1, aWalkDistance + DIAG_COST);
    end;

    if Y-1 >= 1 then     Visit(X, Y-1, aWalkDistance + STRAIGHT_COST);
    if Y+1 <= MapY then Visit(X, Y+1, aWalkDistance + STRAIGHT_COST);

    if X+1 <= MapX then
    begin
      if (Y-1 >= 1) and not MapElem[fTerrain.Land[Y,X+1].Obj].DiagonalBlocked then
        Visit(X+1, Y-1, aWalkDistance + DIAG_COST);
      Visit(X+1, Y, aWalkDistance + STRAIGHT_COST);
      if (Y+1 <= MapY) and not MapElem[fTerrain.Land[Y+1,X+1].Obj].DiagonalBlocked then
        Visit(X+1, Y+1, aWalkDistance + DIAG_COST);
    end;
  end;

var I,K: Integer;
begin
  if USE_WALKING_DISTANCE then
  begin
    //Because we use 10 for straight and 14 for diagonal in byte storage 24 is the maximum allowed
    Assert(aRadius <= MAX_RAD, 'GetTilesWithinDistance can''t handle radii > ' + IntToStr(MAX_RAD));
    SetLength(Visited, aRadius * 2 + 1, aRadius * 2 + 1);
    for I := 0 to aRadius * 2 do
      for K := 0 to aRadius * 2 do
        Visited[I, K] := 255; //Maximum distance so we will always prefer the route we find

    Visit(aStart.X, aStart.Y, 0); //Starting tile is at walking distance zero
  end
  else
  begin
    for I := max(aStart.Y-aRadius, 1) to min(aStart.Y+aRadius, MapY-1) do
      for K := max(aStart.X-aRadius, 1) to min(aStart.X+aRadius, MapX-1) do
        if (aPass in fTerrain.Land[I,K].Passability) and (KMLengthDiag(aStart, KMPoint(K,I)) <= aRadius) then
          aList.AddEntry(KMPoint(K,I));
  end;
end;


procedure TKMTerrainFinderCommon.Save(SaveStream: TKMemoryStream);
begin
  //Everything we have so far is Temp
end;


procedure TKMTerrainFinderCommon.Load(LoadStream: TKMemoryStream);
begin
  //Everything we have so far is Temp
end;


{ TKMTerrainFinder }
function TKMTerrainFinder.CanUse(const X, Y: Word): Boolean;
begin
  //We don't need to check anything extra in common case
  Result := True;
end;


function TKMTerrainFinder.CanWalkHere(const X, Y: Word): Boolean;
begin
  Result := (fPassability * fTerrain.Land[Y,X].Passability <> []);
end;


end.
