unit KM_TerrainFinder;
{$I KaM_Remake.inc}
interface
uses Math, KM_Defaults, KM_CommonClasses, KM_Points;

type
  //General implementation of algorithm that finds something on terrain
  TKMTerrainFinderCommon = class
  private
    fStart: TKMPoint;
    fRadius: Byte;
    fPassability: TPassability;
    MapX, MapY: Word;
    BestDist: Byte;
    BestLoc: TKMPoint;
    Visited: array of array of Byte;
  protected
    function CanWalkHere(const X,Y: Word): Boolean; virtual; abstract;
    function CanUse(const X,Y: Word): Boolean; virtual; abstract;
  public
    constructor Create;
    function FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; out aEnd: TKMPoint): Boolean;
    procedure GetTilesWithinDistance(aStart: TKMPoint; aRadius: Byte; aPass: TPassability; aList: TKMPointList);
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
end;


function TKMTerrainFinderCommon.FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; out aEnd: TKMPoint): Boolean;
  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(const X,Y: Word; aWalkDistance: Byte);
  var
    Xt, Yt: Word;
  begin
    if not (fPassability in fTerrain.Land[Y,X].Passability)
    or not CanWalkHere(X,Y) then Exit;

    Xt := fStart.X - X + fRadius;
    Yt := fStart.Y - Y + fRadius;

    //If new path is longer than old we don't care about it
    if (aWalkDistance >= Visited[Xt,Yt]) then Exit;

    if CanUse(X,Y) then
    begin
      BestDist := aWalkDistance;
      BestLoc := KMPoint(X,Y);
    end;

    //Mark this tile as visited
    Visited[Xt,Yt] := aWalkDistance;

    //Run again on surrounding tiles
    //We check only 4 neighbors, because that x6 times faster than 8 neighbors
    //and we don't really care for perfect circle test
    if (aWalkDistance + 2 <= BestDist) then
    begin
      if X-1 >= 1 then     Visit(X-1, Y, aWalkDistance+2);
      if Y-1 >= 1 then     Visit(X, Y-1, aWalkDistance+2);
      if Y+1 <= MapY then  Visit(X, Y+1, aWalkDistance+2);
      if X+1 <= MapX then  Visit(X+1, Y, aWalkDistance+2);
    end;
  end;
var
  I,K: Integer;
begin
  Assert(aRadius <= 120, 'GetTilesWithinDistance can''t handle radii > 80');

  fStart := aStart;
  fRadius := aRadius;
  fPassability := aPassability;

  //Assign Rad to local variable,
  //when we find better Loc we reduce the Rad to skip any farther Locs
  BestDist := aRadius * 2;
  BestLoc := KMPoint(0,0);

  SetLength(Visited, 2*fRadius+1, 2*fRadius+1);
  for I := 0 to 2 * fRadius do
  for K := 0 to 2 * fRadius do
    Visited[I,K] := 255; //Initially all tiles are unexplored

  Visit(fStart.X, fStart.Y, 0); //Starting tile is at walking distance zero

  aEnd := BestLoc;
  Result := BestLoc.X <> 0;
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
    if (aWalkDistance >= Visited[Xt,Yt]) then Exit;

    //Only add to results once (255 is the intial value)
    if Visited[Xt,Yt] = 255 then
      aList.AddEntry(KMPoint(X,Y));

    //Mark this tile as visited
    Visited[Xt,Yt] := aWalkDistance;

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
    Assert(aRadius <= MAX_RAD, 'GetTilesWithinDistance can''t handle radii > MAX_RAD');
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


{ TKMTerrainFinder }
function TKMTerrainFinder.CanUse(const X, Y: Word): Boolean;
begin
  //We don't need to check anything extra in common case
  Result := True;
end;


function TKMTerrainFinder.CanWalkHere(const X, Y: Word): Boolean;
begin
  //We don't need to check anything extra in common case
  Result := True;
end;


end.
