unit KM_TerrainFinder;
{$I KaM_Remake.inc}
interface
uses Math, SysUtils, KM_Defaults, KM_CommonClasses, KM_Points;

type
  //General implementation of algorithm that finds something on terrain
  TKMTerrainFinderCommon = class
  private
    MapX, MapY: Word;
    fStart: TKMPoint;
    fRadius: Byte;
    fPassability: TPassability;
    fMaxCount: Word;
    fOutput: TKMPointTagList;
    BestDist: Byte;
    BestLoc: TKMPoint;
    Visited: array of array of Byte;
    procedure InitVisited;
    procedure SaveTile(const X,Y: Word; aWalkDistance: Byte);
    procedure UseFinder;
  protected
    function CanWalkHere(const X,Y: Word): Boolean; virtual; abstract;
    function CanUse(const X,Y: Word): Boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; out aEnd: TKMPoint): Boolean; overload;
    function FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; aMaxCount: Word; out aEnd: TKMPointArray): Boolean; overload;
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
  fOutput := TKMPointTagList.Create;
end;


destructor TKMTerrainFinderCommon.Destroy;
begin
  fOutput.Free;
  inherited;
end;


function TKMTerrainFinderCommon.FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; out aEnd: TKMPoint): Boolean;
begin
  fStart := aStart;
  fRadius := aRadius;
  fPassability := aPassability;
  fMaxCount := 1;

  UseFinder;

  aEnd := BestLoc;
  Result := BestLoc.X <> 0;
end;


function TKMTerrainFinderCommon.FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TPassability; aMaxCount: Word; out aEnd: TKMPointArray): Boolean;
var
  I: Integer;
begin
  fStart := aStart;
  fRadius := aRadius;
  fPassability := aPassability;
  fMaxCount := aMaxCount;
  fOutput.Clear;

  UseFinder;

  fOutput.SortByTag;

  SetLength(aEnd, Min(fMaxCount, fOutput.Count));
  for I := 0 to Min(fMaxCount, fOutput.Count) - 1 do
    aEnd[I] := fOutput.Items[I];

  Result := Min(fMaxCount, fOutput.Count) <> 0;
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
    I := TKMPointList(fOutput).IndexOf(KMPoint(X,Y));
    if I <> -1 then
      fOutput.Tag[I] := aWalkDistance //New is always smaller
    else
      fOutput.AddEntry(KMPoint(X,Y), aWalkDistance);

    //If we have enough points we can be picky
    if fOutput.Count >= fMaxCount then
      BestDist := aWalkDistance;
  end;
end;


procedure TKMTerrainFinderCommon.InitVisited;
var
  I,K: Integer;
begin
  SetLength(Visited, 2*fRadius+1, 2*fRadius+1);
  for I := 0 to 2 * fRadius do
  for K := 0 to 2 * fRadius do
    Visited[I,K] := 255; //Initially all tiles are unexplored
end;


procedure TKMTerrainFinderCommon.UseFinder;
  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(const X,Y: Word; aWalkDistance: Byte);
  var
    Xt, Yt: Word;
  begin
    //These exit conditions are arranged in order of how long we estimate they will take
    if not (fPassability in fTerrain.Land[Y,X].Passability) then Exit;
    //If new path is longer than old we don't care about it
    Xt := fStart.X - X + fRadius;
    Yt := fStart.Y - Y + fRadius;
    if (aWalkDistance >= Visited[Xt,Yt]) then Exit;
    if not CanWalkHere(X,Y) then Exit;

    if CanUse(X,Y) then
      SaveTile(X,Y,aWalkDistance);

    //Mark this tile as visited
    Visited[Xt,Yt] := aWalkDistance;

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
begin
  //Assign Rad to local variable,
  //when we find better Loc we reduce the Rad to skip any farther Locs
  BestDist := fRadius;
  BestLoc := KMPoint(0,0);

  InitVisited;

  Visit(fStart.X, fStart.Y, 0); //Starting tile is at walking distance zero
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
