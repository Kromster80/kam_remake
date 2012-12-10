unit Unit_Finder;
interface
uses Types, Math;


const
  MAX_SIZE = 255;

type
  TJPSPoint = class
    x,y: Word;
    h: Word;
    g, f: Single;
    opened, closed: Boolean;
    parent: TJPSPoint;
  end;

  TPointArray = array of TPoint;


  TFinder = class
  private
    startNode, endNode: TJPSPoint;

    openListCount: Word;
    openList: array [0..1000] of TJPSPoint;

    function OpenListEmpty: Boolean;
    procedure OpenListPush(const aPoint: TJPSPoint);
    function OpenListPop: TJPSPoint;

    function getNodeAt(x, y: Word): TJPSPoint;
    function backtrace(aEnd: TJPSPoint): TPointArray;
    procedure identifySuccessors(const node: TJPSPoint);
    function findNeighbors(const node: TJPSPoint): TPointArray;
    function jump(x, y, px, py: Word): TPoint;
  public
    constructor Create;
    destructor Destroy; override;

    function MakeRoute(aStart, aEnd: TPoint): TPointArray;

  end;

  TMap = class
  public
    Map: array [0 .. MAX_SIZE - 1, 0 .. MAX_SIZE - 1] of Boolean;
    function IsInside(x, y: SmallInt): Boolean;
    function IsWalkableAt(x, y: SmallInt): Boolean;
  end;


var
  Map: TMap;



implementation


{ TMap }
function TMap.IsInside(x, y: SmallInt): Boolean;
begin
  Result := InRange(x, 0, MAX_SIZE) and InRange(y, 0, MAX_SIZE);
end;


function TMap.IsWalkableAt(x, y: SmallInt): Boolean;
begin
  Result := IsInside(x, y) and Map[y,x];
end;


{ TFinder }
constructor TFinder.Create;
begin
  inherited Create;

end;


destructor TFinder.Destroy;
begin

  inherited;
end;


function TFinder.OpenListEmpty: Boolean;
begin
  Result := openListCount = 0;
end;


procedure TFinder.OpenListPush(const aPoint: TJPSPoint);
begin
  Move(openList[0], openList[1], openListCount * SizeOf(openList[0]));
  openList[0] := aPoint;
  Inc(openListCount);
end;


function TFinder.OpenListPop: TJPSPoint;
var
  I: Integer;
  Best: Integer;
begin
  //Return smallest f
  Best := 0;
  for I := 1 to openListCount - 1 do
  if openList[Best].f < openList[I].f then
    Best := I;

  Result := openList[Best];

  if Best <> openListCount then
    Move(openList[Best + 1], openList[Best], (openListCount - Best) * SizeOf(openList[0]));

  Dec(openListCount);
end;


function TFinder.MakeRoute(aStart, aEnd: TPoint): TPointArray;
var
  Node: TJPSPoint;
begin
  startNode := TJPSPoint.Create;
  endNode := TJPSPoint.Create;

  startNode.x := aStart.X;
  startNode.y := aStart.Y;
  endNode.x := aEnd.X;
  endNode.y := aEnd.Y;

  startNode.g := 0;
  startNode.f := 0;

  OpenListPush(startNode);

  startNode.opened := True;

  while (not OpenListEmpty) do
  begin
    // pop the position of node which has the minimum `f` value.
    Node := OpenListPop;
    Node.closed := True;

    if (Node.X = endNode.X) and (Node.Y = endNode.Y) then
    begin
      Result := backtrace(endNode);
    end;

    identifySuccessors(Node);
  end;
end;


function TFinder.getNodeAt(x, y: Word): TJPSPoint;
var
  I: Integer;
begin
  for I := 0 to openListCount - 1 do
  if (openList[I].x = x) and (openList[I].y = y) then
  begin
    Result := openList[I];
    Exit;
  end
  else
  begin
    Result := TJPSPoint.Create;
    Result.x := x;
    Result.y := y;
  end;
end;


function TFinder.backtrace(aEnd: TJPSPoint): TPointArray;
var
  Node: TJPSPoint;
begin
  Node := aEnd;

  while Node.parent <> nil do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].X := Node.X;
    Result[High(Result)].Y := Node.Y;
    Node := Node.parent;
  end;

  SetLength(Result, Length(Result) + 1);
  Result[High(Result)].X := Node.X;
  Result[High(Result)].Y := Node.Y;
end;


//Identify successors for the given node. Runs a jump point search in the
//direction of each available neighbor, adding any points found to the open
//list
procedure TFinder.identifySuccessors(const node: TJPSPoint);
var
  endX, endY: Word;
  x,y,jx,jy: Word;
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
            begin
                Continue;
            end;

            // include distance, as parent may not be immediately adjacent:
            d := sqrt(sqr(jx - x) + sqr(jy - y));
            ng := node.g + d; // next `g` value

            if (not jumpNode.opened) or (ng < jumpNode.g) then
            begin
                jumpNode.g := ng;
                if jumpNode.h = 0 then
                  jumpNode.h := (abs(jx - endX) + abs(jy - endY));
                jumpNode.f := jumpNode.g + jumpNode.h;
                jumpNode.parent := node;

                if not jumpNode.opened then
                begin
                    OpenListPush(jumpNode);
                    jumpNode.opened := True;
                end
                else begin
                    //openList.updateItem(jumpNode);
                end;
            end;
        end;
    end;
end;


//Search recursively in the direction (parent -> child), stopping only when a
// jump point is found.
function  TFinder.jump(x, y, px, py: Word): TPoint;
var
  dx, dy: Word;
  jx, jy: TPoint;
begin
    dx := x - px;
    dy := y - py;

    if not Map.IsWalkableAt(x, y) then
    begin
        Result := Point(-1, -1);
        Exit;
    end
    else if (x = endNode.x) and (y = endNode.y) then
    begin
        Result := Point(x, y);
        Exit;
    end;

    // check for forced neighbors
    // along the diagonal
    if (dx <> 0) and (dy <> 0) then
    begin
        if ((Map.isWalkableAt(x - dx, y + dy) and not Map.isWalkableAt(x - dx, y)) or
            (Map.isWalkableAt(x + dx, y - dy) and not Map.isWalkableAt(x, y - dy))) then
        begin
          Result := Point(x, y);
          Exit;
        end;
    end
    // horizontally/vertically
    else begin
        if( dx <> 0 ) then
        begin // moving along x
            if((Map.isWalkableAt(x + dx, y + 1) and not Map.isWalkableAt(x, y + 1)) or
               (Map.isWalkableAt(x + dx, y - 1) and not Map.isWalkableAt(x, y - 1))) then
            begin
                Result := Point(x, y);
                Exit;
            end;
        end
        else begin
            if((Map.isWalkableAt(x + 1, y + dy) and not Map.isWalkableAt(x + 1, y)) or
               (Map.isWalkableAt(x - 1, y + dy) and not Map.isWalkableAt(x - 1, y))) then
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
    if (Map.isWalkableAt(x + dx, y) or Map.isWalkableAt(x, y + dy)) then
    begin
        Result := jump(x + dx, y + dy, x, y);
    end else begin
        Result := Point(-1, -1);
    end
end;

//Find the neighbors for the given node. If the node has a parent,
//prune the neighbors based on the jump point search algorithm, otherwise
//return all available neighbors.
//@return {Array.<[number, number]>} The neighbors found.
function TFinder.findNeighbors(const node: TJPSPoint): TPointArray;
var
  count: Word;
  procedure Add(ax,ay: Word);
  begin
    Result[count].X := ax;
    Result[count].Y := ay;
    Inc(count);
  end;
var
  parent: TJPSPoint;
  x,y: Word;
  px, py, dx, dy: Word;
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
            if Map.IsWalkableAt(x, y + dy) then
            begin
              Add(x, y + dy);
            end;
            if Map.IsWalkableAt(x + dx, y) then
            begin
              Add(x + dx, y);
            end;
            if Map.IsWalkableAt(x, y + dy) or Map.IsWalkableAt(x + dx, y) then
            begin
              Add(x + dx, y + dy);
            end;
            if (not Map.IsWalkableAt(x - dx, y)) and (not Map.IsWalkableAt(x, y + dy)) then
            begin
              Add(x - dx, y + dy);
            end;
            if (not Map.IsWalkableAt(x, y - dy)) and (not Map.IsWalkableAt(x + dx, y)) then
            begin
              Add(x + dx, y - dy);
            end;
        end
        // search horizontally/vertically
        else begin
            if (dx = 0) then
            begin
                if Map.IsWalkableAt(x, y + dy) then
                begin
                    if Map.IsWalkableAt(x, y + dy) then
                    begin
                      Add(x, y + dy);
                    end;
                    if not Map.IsWalkableAt(x + 1, y) then
                    begin
                      Add(x + 1, y + dy);
                    end;
                    if not Map.IsWalkableAt(x - 1, y) then
                    begin
                      Add(x - 1, y + dy);
                    end;
                end;
            end
            else begin
                if Map.IsWalkableAt(x + dx, y) then
                begin
                    if Map.IsWalkableAt(x + dx, y) then
                    begin
                      Add(x + dx, y);
                    end;
                    if not Map.IsWalkableAt(x, y + 1) then
                    begin
                      Add(x + dx, y + 1);
                    end;
                    if not Map.IsWalkableAt(x, y - 1) then
                    begin
                      Add(x + dx, y - 1);
                    end;
                end
            end;
        end;
    end
    // return all neighbors if parent = nil
    else begin
      if Map.IsWalkableAt(x, y-1) then
        Add(x, y-1);
      if Map.IsWalkableAt(x+1, y) then
        Add(x+1, y);
      if Map.IsWalkableAt(x, y+1) then
        Add(x, y+1);
      if Map.IsWalkableAt(x-1, y) then
        Add(x-1, y);

      if Map.IsWalkableAt(x-1, y-1) then
        Add(x-1, y-1);
      if Map.IsWalkableAt(x+1, y-1) then
        Add(x+1, y-1);
      if Map.IsWalkableAt(x+1, y+1) then
        Add(x+1, y+1);
      if Map.IsWalkableAt(x-1, y+1) then
        Add(x-1, y+1);
    end;

    //Invert array since we should have Pushed values to it, not appended
  SetLength(Result, count);
end;


end.
