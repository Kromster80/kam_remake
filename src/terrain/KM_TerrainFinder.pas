unit KM_TerrainFinder;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points;

type
  //General implementation of algorithm that finds something on terrain without building a route to it
  TKMTerrainFinderCommon = class
  private
    fMapX: Word;
    fMapY: Word;
    fStart: TKMPointArray;
    fRadius: Byte;
    fMaxCount: Word;
    BestDist: Byte;
    BestLoc: TKMPoint;
    fVisited: array of array of Byte;

    //Temp for list that we need to fill
    fLocs: TKMPointTagList;

    procedure InitVisited;
    procedure SaveTile(const X,Y: Word; aWalkDistance: Byte);
    procedure UseFinder;
  protected
    fPassability: TKMTerrainPassabilitySet;
    function CanWalkHere(const X,Y: Word): Boolean; virtual; abstract;
    function CanUse(const X,Y: Word): Boolean; virtual; abstract;
  public
    constructor Create;
    function FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TKMTerrainPassabilitySet; out aEnd: TKMPoint): Boolean; overload;
    procedure FindNearest(aStart: TKMPointArray; aRadius: Byte; aPassability: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
  end;


  //Simple algorithm implementation required to override abstract methods
  TKMTerrainFinder = class(TKMTerrainFinderCommon)
  protected
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  public
    procedure GetTilesWithinDistance(aStart: TKMPoint; aRadius: Byte; aPass: TKMTerrainPassability; aList: TKMPointList);
  end;


implementation
uses
  KM_ResMapElements, KM_Terrain;


{ TKMTerrainFinder }
constructor TKMTerrainFinderCommon.Create;
begin
  inherited;

  fMapX := gTerrain.MapX;
  fMapY := gTerrain.MapY;
  SetLength(fVisited, fMapY+1, fMapX+1);
end;


function TKMTerrainFinderCommon.FindNearest(aStart: TKMPoint; aRadius: Byte; aPassability: TKMTerrainPassabilitySet; out aEnd: TKMPoint): Boolean;
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


procedure TKMTerrainFinderCommon.FindNearest(aStart: TKMPointArray; aRadius: Byte; aPassability: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fStart := aStart;
  fRadius := aRadius;
  fPassability := aPassability;
  fMaxCount := aMaxCount;
  fLocs := aLocs;

  fLocs.Clear;
  UseFinder;

  //Prepare output
  if fMaxCount = 1 then
  begin
    if BestLoc.X <> 0 then
      fLocs.Add(BestLoc, BestDist)
  end
  else
  begin
    fLocs.SortByTag;
    fLocs.Count := Min(fMaxCount, fLocs.Count);
  end;
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
      fLocs.Add(KMPoint(X,Y), aWalkDistance);

    //If we have enough points we can be picky and take only better distances
    if fLocs.Count >= fMaxCount then
      BestDist := aWalkDistance;
  end;
end;


procedure TKMTerrainFinderCommon.InitVisited;
var
  I, K: Integer;
begin
  // Initially all tiles are unexplored (iterating 1..N)
  for I := 1 to fMapY do
  for K := 1 to fMapX do
    fVisited[I, K] := 255;
end;


procedure TKMTerrainFinderCommon.UseFinder;
  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(const X,Y: Word; aWalkDistance: Byte);
  begin
    //If new path is longer than old we don't care about it
    if (aWalkDistance >= fVisited[Y,X]) then Exit;

    //Check if we can walk through this tile
    if not CanWalkHere(X, Y) then Exit;

    //Check if we can take this tile
    if CanUse(X, Y) then
      SaveTile(X, Y, aWalkDistance);

    //Mark this tile as visited
    fVisited[Y,X] := aWalkDistance;

    //Run again on surrounding tiles
    //We check only 4 neighbors, because that x6 times faster than 8 neighbors
    //and we don't really care for perfect circle test
    if (aWalkDistance + 1 <= BestDist) then
    begin
      if X-1 >=     1 then Visit(X-1, Y, aWalkDistance+1);
      if Y-1 >=     1 then Visit(X, Y-1, aWalkDistance+1);
      if Y+1 <= fMapY then Visit(X, Y+1, aWalkDistance+1);
      if X+1 <= fMapX then Visit(X+1, Y, aWalkDistance+1);
    end;
  end;
var
  I: Integer;
begin
  //Assign Rad to local variable,
  //when we find better Loc we reduce the Rad to skip any farther Locs
  BestDist := fRadius;
  BestLoc := KMPOINT_ZERO;

  InitVisited;

  //Starting tile is at walking distance zero
  //When using multiple points they will overlap if new WalkDistance is shorter
  for I := Low(fStart) to High(fStart) do
    Visit(fStart[I].X, fStart[I].Y, 0);
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
  Result := (fPassability * gTerrain.Land[Y,X].Passability <> []);
end;


//Fills aList with all of the tiles within aRadius of aStart with aPass using either a
//simple radius or a floodfill walking distance calculation depending on USE_WALKING_DISTANCE
procedure TKMTerrainFinder.GetTilesWithinDistance(aStart: TKMPoint; aRadius: Byte; aPass: TKMTerrainPassability; aList: TKMPointList);
const
  STRAIGHT_COST = 5;
  DIAG_COST = 7; // 5 * 1.41
  MAX_RAD = (255 - DIAG_COST) div STRAIGHT_COST;

  //Uses a floodfill style algorithm but only on a small area (with aRadius)
  procedure Visit(X,Y: Word; aWalkDistance: Byte);
  var
    visitX, visitY: Word;
  begin
    //Test whether this tile is valid and exit immediately if not
    //Multiply the radius because of diagonal approximation (straight=5, diagonal=7)
    if (aWalkDistance > aRadius * STRAIGHT_COST)
    or not (aPass in gTerrain.Land[Y,X].Passability) then
      Exit;

    visitX := aStart.X - X + aRadius;
    visitY := aStart.Y - Y + aRadius;
    if (aWalkDistance >= fVisited[visitY, visitX]) then Exit;

    //Only add to results once (255 is the initial value)
    if fVisited[visitY, visitX] = 255 then
      aList.Add(KMPoint(X, Y));

    //Mark this tile as visited
    fVisited[visitY, visitX] := aWalkDistance;

    //Run again on surrounding tiles
    //We use +5 for straights and +7 for diagonals rather than +1 and +1.41 then div by 5 in
    //calculations so we can still store it as bytes to save space and time
    if X-1 >= 1 then
    begin
      if (Y-1 >= 1) and not gMapElements[gTerrain.Land[Y,X].Obj].DiagonalBlocked then
        Visit(X-1, Y-1, aWalkDistance + DIAG_COST);
      Visit(X-1, Y, aWalkDistance + STRAIGHT_COST);
      if (Y+1 <= fMapY) and not gMapElements[gTerrain.Land[Y+1,X].Obj].DiagonalBlocked then
        Visit(X-1,Y+1, aWalkDistance + DIAG_COST);
    end;

    if Y-1 >=     1 then Visit(X, Y-1, aWalkDistance + STRAIGHT_COST);
    if Y+1 <= fMapY then Visit(X, Y+1, aWalkDistance + STRAIGHT_COST);

    if X+1 <= fMapX then
    begin
      if (Y-1 >= 1) and not gMapElements[gTerrain.Land[Y,X+1].Obj].DiagonalBlocked then
        Visit(X+1, Y-1, aWalkDistance + DIAG_COST);
      Visit(X+1, Y, aWalkDistance + STRAIGHT_COST);
      if (Y+1 <= fMapY) and not gMapElements[gTerrain.Land[Y+1,X+1].Obj].DiagonalBlocked then
        Visit(X+1, Y+1, aWalkDistance + DIAG_COST);
    end;
  end;

var
  I, K: Integer;
begin
  if USE_WALKING_DISTANCE then
  begin
    //Because we use 10 for straight and 14 for diagonal in byte storage 24 is the maximum allowed
    Assert(aRadius <= MAX_RAD, 'GetTilesWithinDistance can''t handle radii > ' + IntToStr(MAX_RAD));

    SetLength(fVisited, aRadius * 2 + 1, aRadius * 2 + 1);
    for I := 0 to aRadius * 2 do
      for K := 0 to aRadius * 2 do
        fVisited[I, K] := 255; //Maximum distance so we will always prefer the route we find

    Visit(aStart.X, aStart.Y, 0); //Starting tile is at walking distance zero
  end
  else
  begin
    for I := Max(aStart.Y-aRadius, 1) to Min(aStart.Y+aRadius, fMapY-1) do
      for K := Max(aStart.X-aRadius, 1) to Min(aStart.X+aRadius, fMapX-1) do
        if (aPass in gTerrain.Land[I,K].Passability) and (KMLengthDiag(aStart, KMPoint(K,I)) <= aRadius) then
          aList.Add(KMPoint(K,I));
  end;
end;


end.
