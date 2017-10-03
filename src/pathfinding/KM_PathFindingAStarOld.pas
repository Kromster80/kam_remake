unit KM_PathFindingAStarOld;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Math, KromUtils, KM_PathFinding,
  KM_CommonClasses, KM_Defaults, KM_Terrain, KM_Points;


type
  //This is a helper class for TTerrain
  //Here should be pathfinding and all associated stuff
  //I think we should refactor this unit and move some TTerrain methods here
  TPathFindingAStarOld = class(TPathFinding)
  private
    fNewCost: Integer;
    fMinCost: record
      Cost: Integer;
      ID: Word;
      Pos: TKMPoint;
    end;
    ORef: array of array of Word; //References to OpenList, Sized as map
    OCount: Word;
    OList: array of record //List of checked cells
      Pos: TKMPoint;
      CostTo: Word;
      Estim: Word;
      Parent: Word;//Reference to parent
    end;
  protected
    function MakeRoute: Boolean; override;
    procedure ReturnRoute(NodeList: TKMPointList); override;
  end;


implementation


{ TPathFindingAStarOld }
function TPathFindingAStarOld.MakeRoute: Boolean;
const c_closed = 65535;
var I, X, Y: Integer;
begin
  //Erase previous values
  SetLength(ORef, 0);
  SetLength(ORef, gTerrain.MapY+1, gTerrain.MapX+1);

  //Initialize first element
  OCount := 1;
  ORef[fLocA.Y, fLocA.X] := OCount;
  SetLength(OList, OCount + 1);
  OList[OCount].Pos     := fLocA;
  OList[OCount].CostTo  := 0;
  OList[OCount].Estim   := (abs(fLocB.X-fLocA.X) + abs(fLocB.Y-fLocA.Y)) * 10;
  OList[OCount].Parent  := 0;

  //Seed
  fMinCost.Cost := 0;
  fMinCost.ID := 1;
  fMinCost.Pos := fLocA;

  while not DestinationReached(fMinCost.Pos.X, fMinCost.Pos.Y) and (fMinCost.Cost <> 65535) do
  begin

    OList[fMinCost.ID].Estim := c_closed;

    //Check all surrounding cells and issue costs to them
    for y := Math.max(fMinCost.Pos.Y-1,1) to Math.min(fMinCost.Pos.Y+1, gTerrain.MapY-1) do
    for x := Math.max(fMinCost.Pos.X-1,1) to Math.min(fMinCost.Pos.X+1, gTerrain.MapX-1) do
    if ORef[y,x] = 0 then //Cell is new
    begin
      if CanWalkTo(fMinCost.Pos, x, y) then
      begin
        inc(OCount);
        if OCount >= Length(OList) then
          SetLength(OList, OCount + 128); //Allocate slightly more space

        OList[OCount].Pos := KMPoint(x,y);

        if IsWalkableTile(X, Y) then
        begin
          ORef[y,x] := OCount;
          OList[OCount].Parent := ORef[fMinCost.Pos.Y, fMinCost.Pos.X];
          OList[OCount].CostTo := OList[OList[OCount].Parent].CostTo + MovementCost(fMinCost.Pos.X, fMinCost.Pos.Y, X, Y);
          OList[OCount].Estim := (abs(x-fLocB.X) + abs(y-fLocB.Y)) * 10; //Use Estim even if destination is Passability, as it will make it faster. Target should be in the right direction even though it's not our destination.
        end
        else //If cell doen't meets Passability then mark it as Closed
          OList[OCount].Estim := c_closed;
      end;

    end
    else //Else cell is old
    begin

      //If route through new cell is shorter than ORef[y,x] then
      if OList[ORef[y,x]].Estim <> c_closed then
      if CanWalkTo(fMinCost.Pos, x, y) then
      begin
        fNewCost := MovementCost(fMinCost.Pos.X, fMinCost.Pos.Y, X, Y);
        if OList[fMinCost.ID].CostTo + fNewCost < OList[ORef[y,x]].CostTo then
        begin
          OList[ORef[y,x]].Parent := ORef[fMinCost.Pos.Y,fMinCost.Pos.X];
          OList[ORef[y,x]].CostTo := OList[fMinCost.ID].CostTo + fNewCost;
          //OList[ORef[y,x]].Estim:=(abs(x-fLocB.X) + abs(y-fLocB.Y))*10;
        end;
      end;
    end;

    //Find next cell with least (Estim+CostTo)
    fMinCost.Cost := 65535;
    for i := OCount downto 1 do //'downto' works faster here
      if OList[i].Estim <> c_closed then
        if (OList[i].Estim + OList[i].CostTo) < fMinCost.Cost then
        begin
          fMinCost.Cost := OList[i].Estim + OList[i].CostTo;
          fMinCost.ID := i;
          fMinCost.Pos := OList[i].Pos;
        end;
  end;

  Result := DestinationReached(fMinCost.Pos.X, fMinCost.Pos.Y);
  //Assert(fMinCost.Cost<>65535, 'FloodFill test failed and there''s no possible route A-B');
end;


procedure TPathFindingAStarOld.ReturnRoute(NodeList: TKMPointList);
var
  I: Integer;
begin
  NodeList.Clear;

  //Assemble the route
  I := fMinCost.ID;
  repeat
    NodeList.Add(OList[I].Pos);
    I := OList[I].Parent;
  until I = 0;

  //Reverse the list, since path is assembled LocB > LocA
  NodeList.Inverse;

  //Cache long paths
  if CACHE_PATHFINDING and (NodeList.Count > 20) then
    AddToCache(NodeList);
end;


end.
