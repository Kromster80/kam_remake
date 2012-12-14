unit KM_PathFindingAStarNew;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, KromUtils, KM_PathFinding,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_Terrain, KM_Points, BinaryHeap;


type
  TANode = class
             X,Y: SmallInt;
             CostTo: Word;
             Estim: Word;
             Parent: TANode;
           end;

  //This is a helper class for TTerrain
  //Here should be pathfinding and all associated stuff
  //I think we should refactor this unit and move some TTerrain methods here
  TPathFindingAStarNew = class(TPathFinding)
  private
    Heap: TBinaryHeap;
    MinN: TANode;
    ORef: array of array of TANode; //References to OpenList, Sized as map
    function HeapCmp(A,B: Pointer): Boolean;
    procedure Reset;
  protected
    function MakeRoute: Boolean; override;
    procedure ReturnRoute(NodeList: TKMPointList); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TPathFindingAStarNew }
constructor TPathFindingAStarNew.Create;
begin
  inherited;

  Heap := TBinaryHeap.Create(High(Word));
  Heap.Cmp := HeapCmp;
end;


destructor TPathFindingAStarNew.Destroy;
var
  I: Integer;
begin
  Reset;
  Heap.Free;

  inherited;
end;


function TPathFindingAStarNew.HeapCmp(A, B: Pointer): Boolean;
begin
  if A = nil then
    Result := True
  else
    Result := (B = nil) or (TANode(A).Estim + TANode(A).CostTo < TANode(B).Estim + TANode(B).CostTo);
end;


procedure TPathFindingAStarNew.Reset;
var
  I,K: Integer;
begin
  for I := 0 to High(ORef) do
  for K := 0 to High(ORef[I]) do
  if ORef[I,K] <> nil then
  begin
    ORef[I,K].Free;
    ORef[I,K] := nil;
  end;
end;


function TPathFindingAStarNew.MakeRoute: Boolean;
const c_closed = 65535;
var
  N: TANode;
  X, Y: Integer;
  NewCost: Integer;
begin
  //Clear previous data
  Reset;
  SetLength(ORef, fTerrain.MapY+1, fTerrain.MapX+1);

  //Initialize first element
  N := TANode.Create;
  ORef[fLocA.Y, fLocA.X] := N;
  N.X       := fLocA.X;
  N.Y       := fLocA.Y;
  N.Estim   := (abs(fLocB.X-fLocA.X) + abs(fLocB.Y-fLocA.Y)) * 10;
  N.Parent  := nil;

  //Seed
  MinN := N;

  while (MinN <> nil) and not DestinationReached(MinN.X, MinN.Y) do
  begin

    MinN.Estim := c_closed;

    //Check all surrounding cells and issue costs to them
    for y := Math.max(MinN.Y-1,1) to Math.min(MinN.Y+1, fTerrain.MapY-1) do
    for x := Math.max(MinN.X-1,1) to Math.min(MinN.X+1, fTerrain.MapX-1) do
    if ORef[y,x] = nil then //Cell is new
    begin
      if CanWalkTo(KMPoint(MinN.X, MinN.Y), x, y) then
      begin

        N := TANode.Create;
        ORef[y,x] := N;
        N.X := x;
        N.Y := y;
        N.Parent := MinN;

        if IsWalkableTile(X, Y) then
        begin
          N.CostTo := MinN.CostTo + MovementCost(MinN.X, MinN.Y, X, Y);
          N.Estim := (abs(x-fLocB.X) + abs(y-fLocB.Y)) * 10; //Use Estim even if destination is Passability, as it will make it faster. Target should be in the right direction even though it's not our destination.
          Heap.Push(N);
        end
        else //If cell doen't meets Passability then mark it as Closed
          N.Estim := c_closed;

      end;
    end
    else //Else cell is old
    begin

      //If route through new cell is shorter than ORef[y,x] then
      if ORef[y,x].Estim <> c_closed then
      if CanWalkTo(KMPoint(MinN.X, MinN.Y), x, y) then
      begin
        NewCost := MovementCost(MinN.X, MinN.Y, X, Y);
        if MinN.CostTo + NewCost < ORef[y,x].CostTo then
        begin
          ORef[y,x].Parent := MinN;
          ORef[y,x].CostTo := MinN.CostTo + NewCost;
        end;
      end;
    end;

    //Find next cell with least (Estim+CostTo)
    if Heap.IsEmpty then
      Break;

    MinN := Heap.Pop;
  end;

  //Route found, no longer need the lookups
  Heap.Clear;

  Result := DestinationReached(MinN.X, MinN.Y);
  //Assert(fMinCost.Cost<>65535, 'FloodFill test failed and there''s no possible route A-B');
end;


procedure TPathFindingAStarNew.ReturnRoute(NodeList: TKMPointList);
var
  N: TANode;
begin
  NodeList.Clear;

  //Assemble the route
  N := MinN;
  while N <> nil do
  begin
    NodeList.AddEntry(KMPoint(N.X, N.Y));
    N := N.Parent;
  end;

  //Reverse the list, since path is assembled LocB > LocA
  NodeList.Inverse;

  //Cache long paths
  if CACHE_PATHFINDING and (NodeList.Count > 20) then
    AddToCache(NodeList);
end;


end.
