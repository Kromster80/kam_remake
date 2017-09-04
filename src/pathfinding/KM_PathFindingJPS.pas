unit KM_PathFindingJPS;
{$I KaM_Remake.inc}
interface
uses
  Types, Math, SysUtils, KM_PathFinding, BinaryHeap, KM_Points, KM_CommonClasses, KM_Terrain;


type
  TJPSPoint = class
    x,y: Word;
    Estim: Word; //Estimated distance to End
    g, f: Single; //G - CostTo, F - CostTo + Estim
    opened, closed: Boolean;
    parent: TJPSPoint;
  end;

  TPointArray = array of TPoint;

  //Jump-Point-Search pathfinder
  //based on JavaScript implementation by aniero / https://github.com/aniero
  TPathFindingJPS = class(TPathFinding)
  private
    Nodes: array of array of TJPSPoint;
    startNode, endNode: TJPSPoint;
    openList: TBinaryHeap;

    function HeapCmp(A,B: Pointer): Boolean;
    procedure Reset;
    function getNodeAt(x, y: SmallInt): TJPSPoint;
    procedure identifySuccessors(node: TJPSPoint);
    function findNeighbors(const node: TJPSPoint): TPointArray;
    function jump(x, y, px, py: SmallInt): TPoint;
  protected
    function IsWalkableTile(aX, aY: SmallInt): Boolean; reintroduce;
    function MakeRoute: Boolean; override;
    procedure ReturnRoute(NodeList: TKMPointList); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TPathFindingJPS }
constructor TPathFindingJPS.Create;
begin
  inherited;

  openList := TBinaryHeap.Create(High(Word));
  openList.Cmp := HeapCmp;
end;


destructor TPathFindingJPS.Destroy;
begin
  openList.Free;

  inherited;
end;


function TPathFindingJPS.HeapCmp(A, B: Pointer): Boolean;
begin
  if A = nil then
    Result := True
  else
    Result := (B = nil) or (TJPSPoint(A).f < TJPSPoint(B).f);
end;


procedure TPathFindingJPS.Reset;
var
  I,K: Integer;
begin
  openList.Clear;

  for I := 0 to High(Nodes) do
    for K := 0 to High(Nodes[I]) do
      FreeAndNil(Nodes[I,K]);
end;


function TPathFindingJPS.getNodeAt(x, y: SmallInt): TJPSPoint;
begin
  if Nodes[y,x] = nil then
  begin
    Nodes[y,x] := TJPSPoint.Create;
    Nodes[y,x].x := x;
    Nodes[y,x].y := y;
  end;

  Result := Nodes[y,x];
end;


//Find and return the the path.
function TPathFindingJPS.MakeRoute: Boolean;
var
  Node: TJPSPoint;
begin
  SetLength(Nodes, gTerrain.MapY+1, gTerrain.MapX+1);

  startNode := getNodeAt(fLocA.X, fLocA.Y);
  endNode := getNodeAt(fLocB.X, fLocB.Y);

  // set the `g` and `f` value of the start node to be 0
  startNode.g := 0;
  startNode.f := 0;

  openList.Push(startNode);
  startNode.opened := True;

  while not openList.IsEmpty do
  begin
    // pop the position of node which has the minimum `f` value.
    Node := openList.Pop;
    Node.closed := True;

    if Node = endNode then
    begin
      Result := True;
      Exit;
    end;

    identifySuccessors(Node);
  end;

  // fail to find the path
  Reset;
  Result := False;
end;


procedure TPathFindingJPS.ReturnRoute(NodeList: TKMPointList);
var
  Node: TJPSPoint;
begin
  NodeList.Clear;

  Node := endNode;

  while Node <> nil do
  begin
    NodeList.Add(KMPoint(Node.X, Node.Y));
    Node := Node.parent;
  end;

  //Reverse the array
  NodeList.Inverse;
  NodeList.SparseToDense;

  Reset;
end;


{**
 * Identify successors for the given node. Runs a jump point search in the
 * direction of each available neighbor, adding any points found to the open
 * list.
 * @protected
 *}
procedure TPathFindingJPS.identifySuccessors(node: TJPSPoint);
var
  endX, endY: SmallInt;
  x,y,jx,jy: SmallInt;
  neighbors: TPointArray;
  neighbor, jumpPoint: TPoint;
  jumpNode: TJPSPoint;
  I: Integer;
  d, ng: Single;
begin
    endX := endNode.X;
    endY := endNode.Y;
    x := node.X;
    y := node.Y;

    neighbors := findNeighbors(node);
    for I := 0 to High(neighbors) do
    begin
        neighbor := neighbors[i];
        jumpPoint := jump(neighbor.X, neighbor.Y, x, y);
        if (jumpPoint.X <> -1) then
        begin

            jx := jumpPoint.X;
            jy := jumpPoint.Y;
            jumpNode := getNodeAt(jx, jy);

            if (jumpNode.closed) then
                Continue;

            // include distance, as parent may not be immediately adjacent:
            d := Sqrt(Sqr(jx - x) + Sqr(jy - y));
            ng := node.g + d; // next `g` value

            if (not jumpNode.opened) or (ng < jumpNode.g) then
            begin
                jumpNode.g := ng;
                if jumpNode.Estim = 0 then
                  jumpNode.Estim := (Abs(jx - endX) + Abs(jy - endY));
                jumpNode.f := jumpNode.g + jumpNode.Estim;
                jumpNode.parent := node;

                if not jumpNode.opened then
                begin
                    openList.Push(jumpNode);
                    jumpNode.opened := True;
                end
                else
                begin
                    openList.UpdateItem(jumpNode);
                end;
            end;
        end;
    end;
end;


function TPathFindingJPS.IsWalkableTile(aX, aY: SmallInt): Boolean;
begin
  Result := gTerrain.TileInMapCoords(aX, aY) and (inherited IsWalkableTile(aX, aY));
end;


{**
 Search recursively in the direction (parent -> child), stopping only when a
 * jump point is found.
 * @protected
 * @return Array.<[number, number]> The x, y coordinate of the jump point
 *     found, or null if not found
 *}
function TPathFindingJPS.jump(x, y, px, py: SmallInt): TPoint;
var
  dx, dy: SmallInt;
  jx, jy: TPoint;
begin
  if not IsWalkableTile(x, y) then
  begin
    Result := Point(-1, -1);
    Exit;
  end
  else
  if (getNodeAt(x,y) = endNode) then
  begin
    Result := Point(x, y);
    Exit;
  end;

  dx := x - px;
  dy := y - py;

  // check for forced neighbors
  // along the diagonal
  if (dx <> 0) and (dy <> 0) then
  begin
    if ((IsWalkableTile(x - dx, y + dy) and not IsWalkableTile(x - dx, y)) or
        (IsWalkableTile(x + dx, y - dy) and not IsWalkableTile(x, y - dy))) then
    begin
      Result := Point(x, y);
      Exit;
    end;
  end
  // horizontally/vertically
  else
  begin
    if( dx <> 0 ) then // moving along x
    begin
      if((IsWalkableTile(x + dx, y + 1) and not IsWalkableTile(x, y + 1)) or
         (IsWalkableTile(x + dx, y - 1) and not IsWalkableTile(x, y - 1))) then
      begin
        Result := Point(x, y);
        Exit;
      end;
    end
    else
    begin
      if((IsWalkableTile(x + 1, y + dy) and not IsWalkableTile(x + 1, y)) or
         (IsWalkableTile(x - 1, y + dy) and not IsWalkableTile(x - 1, y))) then
      begin
        Result := Point(x, y);
        Exit;
      end;
    end;
  end;

  // when moving diagonally, must check for vertical/horizontal jump points
  if (dx <> 0) and (dy <> 0) then
  begin
    jx := jump(x + dx, y, x, y);
    jy := jump(x, y + dy, x, y);
    if (jx.x <> -1) or (jy.x <> -1) then
    begin
      Result := Point(x, y);
      Exit;
    end;
  end;

  // moving diagonally, must make sure one of the vertical/horizontal
  // neighbors is open to allow the path
  if (IsWalkableTile(x + dx, y) or IsWalkableTile(x, y + dy)) then
    Result := jump(x + dx, y + dy, x, y)
  else
    Result := Point(-1, -1);
end;


//Find the neighbors for the given node. If the node has a parent,
//prune the neighbors based on the jump point search algorithm, otherwise
//return all available neighbors.
//@return {Array.<[number, number]>} The neighbors found.
function TPathFindingJPS.findNeighbors(const node: TJPSPoint): TPointArray;
var
  count: SmallInt;
  procedure Push(ax,ay: SmallInt);
  begin
    Result[count].X := ax;
    Result[count].Y := ay;
    Inc(count);
  end;
var
  parent: TJPSPoint;
  x,y: SmallInt;
  px, py, dx, dy: SmallInt;
begin
  count := 0;
  SetLength(Result, 8);

  parent := node.parent;
  x := node.x;
  y := node.y;

    // directed pruning: can ignore most neighbors, unless forced.
    if (parent <> nil) then
    begin
        px := parent.x;
        py := parent.y;
        // get the normalized direction of travel
        dx := Round((x - px) / max(abs(x - px), 1));
        dy := Round((y - py) / max(abs(y - py), 1));

        // search diagonally
        if (dx <> 0) and (dy <> 0) then
        begin
            if IsWalkableTile(x, y + dy) then
              Push(x, y + dy);
            if IsWalkableTile(x + dx, y) then
              Push(x + dx, y);
            if IsWalkableTile(x, y + dy) or IsWalkableTile(x + dx, y) then
              Push(x + dx, y + dy);
            if (not IsWalkableTile(x - dx, y)) and IsWalkableTile(x, y + dy) then
              Push(x - dx, y + dy);
            if (not IsWalkableTile(x, y - dy)) and IsWalkableTile(x + dx, y) then
              Push(x + dx, y - dy);
        end
        // search horizontally/vertically
        else
        begin
            if (dx = 0) then
            begin
                if IsWalkableTile(x, y + dy) then
                begin
                    if IsWalkableTile(x, y + dy) then
                      Push(x, y + dy);
                    if not IsWalkableTile(x + 1, y) then
                      Push(x + 1, y + dy);
                    if not IsWalkableTile(x - 1, y) then
                      Push(x - 1, y + dy);
                end;
            end
            else
            begin
                if IsWalkableTile(x + dx, y) then
                begin
                    if IsWalkableTile(x + dx, y) then
                      Push(x + dx, y);
                    if not IsWalkableTile(x, y + 1) then
                      Push(x + dx, y + 1);
                    if not IsWalkableTile(x, y - 1) then
                      Push(x + dx, y - 1);
                end;
            end;
        end;
    end
    // return all neighbors (if parent = nil)
    else
    begin
      if IsWalkableTile(x, y-1) then
        Push(x, y-1);
      if IsWalkableTile(x+1, y) then
        Push(x+1, y);
      if IsWalkableTile(x, y+1) then
        Push(x, y+1);
      if IsWalkableTile(x-1, y) then
        Push(x-1, y);

      if IsWalkableTile(x-1, y-1) then
        Push(x-1, y-1);
      if IsWalkableTile(x+1, y-1) then
        Push(x+1, y-1);
      if IsWalkableTile(x+1, y+1) then
        Push(x+1, y+1);
      if IsWalkableTile(x-1, y+1) then
        Push(x-1, y+1);
    end;

  SetLength(Result, count);
end;


end.
