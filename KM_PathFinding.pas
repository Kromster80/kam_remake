unit KM_PathFinding;
{$I KaM_Remake.inc}
interface
uses SysUtils, Math, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_Terrain, KM_Points;


type
  TDestinationPoint = (
    dp_Location, //Walk to location
    dp_Passability, //Walk to desired passability
    dp_House //Approach house from any side (workers and warriors)
    );

  //This is a helper class for TTerrain
  //Here should be pathfinding and all associated stuff
  //I think we should refactor this unit and move some TTerrain methods here
  TPathFinding = class
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
  private
    fLocA: TKMPoint;
    fLocB: TKMPoint;
    fPass: TPassability;
    fTargetWalkConnect: TWalkConnect;
    fTargetNetwork: Byte;
    fDistance: Single;
    fIsInteractionAvoid: Boolean;
    fDestination: TDestinationPoint;
    fTargetHouse: TKMHouse;
    function IsDestinationReached: Boolean;
    function MakeRoute: Boolean;
    procedure ReturnRoute(NodeList: TKMPointList);
  public
    constructor Create;
    function Route_Make(aLocA, aLocB: TKMPoint; aPass: TPassability; aDistance: Single; aTargetHouse: TKMHouse; NodeList: TKMPointList): Boolean;
    function Route_MakeAvoid(aLocA, aLocB: TKMPoint; aPass: TPassability; aDistance: Single; aTargetHouse: TKMHouse; NodeList: TKMPointList): Boolean;
    function Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aTargetWalkConnect: TWalkConnect; aTargetNetwork: Byte; aPass: TPassability; NodeList: TKMPointList): Boolean;
    end;


implementation


{ TPathFinding }
constructor TPathFinding.Create;
begin
  inherited;
end;


//Find a route from A to B which meets aPass Passability
//Results should be written as NodeCount of waypoint nodes to Nodes
function TPathFinding.Route_Make(aLocA, aLocB: TKMPoint; aPass:TPassability; aDistance:single; aTargetHouse:TKMHouse; NodeList:TKMPointList): Boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass;
  fTargetNetwork := 0;
  fTargetWalkConnect := wcWalk;
  fDistance := aDistance;
  fIsInteractionAvoid := False;
  fTargetHouse := aTargetHouse;
  if fTargetHouse = nil then
    fDestination := dp_Location
  else
    fDestination := dp_House;

  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end else
    NodeList.Clear;
end;


//We are using Interaction Avoid mode (go around busy units)
function TPathFinding.Route_MakeAvoid(aLocA, aLocB:TKMPoint; aPass:TPassability; aDistance:single; aTargetHouse:TKMHouse; NodeList:TKMPointList):boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass;
  fTargetNetwork := 0;
  fTargetWalkConnect := wcWalk;
  fDistance := aDistance;
  fIsInteractionAvoid := True;
  fTargetHouse := aTargetHouse;
  if fTargetHouse = nil then
    fDestination := dp_Location
  else
    fDestination := dp_House;

  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end;
end;


//Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
function TPathFinding.Route_ReturnToWalkable(aLocA, aLocB: TKMPoint; aTargetWalkConnect:TWalkConnect; aTargetNetwork:byte; aPass:TPassability; NodeList:TKMPointList): Boolean;
begin
  Result := False;

  fLocA := aLocA;
  fLocB := aLocB;
  fPass := aPass; //Should be unused here
  fTargetNetwork := aTargetNetwork;
  fTargetWalkConnect := aTargetWalkConnect;
  fDistance := 0;
  fIsInteractionAvoid := False;
  fTargetHouse := nil;
  fDestination := dp_Passability;

  if MakeRoute then
  begin
    ReturnRoute(NodeList);
    Result := True;
  end else
    NodeList.Clear;
end;


function TPathFinding.IsDestinationReached: Boolean;
begin
  case fDestination of
    dp_Location:    Result := KMLength(fMinCost.Pos, fLocB) <= fDistance;
    dp_Passability: Result := fTerrain.GetConnectID(fTargetWalkConnect, fMinCost.Pos) = fTargetNetwork;
    dp_House:       Result := fTargetHouse.InReach(fMinCost.Pos, fDistance);
    else            Result := true;
  end;
end;


function TPathFinding.MakeRoute: Boolean;
const c_closed = 65535;
var i,x,y: Integer;
begin
  //Erase previous values
  SetLength(ORef, 0);
  SetLength(ORef, fTerrain.MapY+1, fTerrain.MapX+1);

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

  while not IsDestinationReached and (fMinCost.Cost <> 65535) do
  begin

    OList[fMinCost.ID].Estim := c_closed;

    //Check all surrounding cells and issue costs to them
    for y := Math.max(fMinCost.Pos.Y-1,1) to Math.min(fMinCost.Pos.Y+1, fTerrain.MapY-1) do
    for x := Math.max(fMinCost.Pos.X-1,1) to Math.min(fMinCost.Pos.X+1, fTerrain.MapX-1) do
    if ORef[y,x] = 0 then //Cell is new
    begin
      if fTerrain.CanWalkDiagonaly(fMinCost.Pos, KMPoint(x,y)) then
      begin

        inc(OCount);
        if OCount >= Length(OList) then
          SetLength(OList, OCount + 128); //Allocate slightly more space

        OList[OCount].Pos := KMPoint(x,y);

        if (fPass in fTerrain.Land[y,x].Passability) then //If cell meets Passability then estimate it
        begin
          ORef[y,x] := OCount;
          OList[OCount].Parent := ORef[fMinCost.Pos.Y,fMinCost.Pos.X];
          OList[OCount].CostTo := OList[OList[OCount].Parent].CostTo + Round(GetLength(KMPoint(x,y),fMinCost.Pos) * 10);
          //Do not add extra cost if the tile is the target, as it can cause a longer route to be chosen
          if not KMSamePoint(fLocB, KMPoint(x,y)) then
          begin
            if DO_WEIGHT_ROUTES and (fTerrain.Land[y,x].IsUnit <> nil) then
              Inc(OList[OCount].CostTo, 10); //Unit = 1 extra tile
            if fIsInteractionAvoid and fTerrain.TileIsLocked(KMPoint(x,y)) then
              Inc(OList[OCount].CostTo, 500); //In interaction avoid mode, working unit = 50 tiles
          end;
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
      begin
        fNewCost := Round(GetLength(KMPoint(x,y),fMinCost.Pos) * 10);
        if DO_WEIGHT_ROUTES and (fTerrain.Land[y,x].IsUnit <> nil) then
          Inc(fNewCost, 10); //Unit = 1 extra tile
        if fIsInteractionAvoid and fTerrain.TileIsLocked(KMPoint(x,y)) then
          Inc(fNewCost, 500); //In interaction avoid mode, working unit = 50 tiles
        if OList[fMinCost.ID].CostTo + fNewCost < OList[ORef[y,x]].CostTo then
        begin
          OList[ORef[y,x]].Parent:=ORef[fMinCost.Pos.Y,fMinCost.Pos.X];
          OList[ORef[y,x]].CostTo:=OList[fMinCost.ID].CostTo + fNewCost;
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

  Result := IsDestinationReached;
  //Assert(fMinCost.Cost<>65535, 'FloodFill test failed and there''s no possible route A-B');
end;


procedure TPathFinding.ReturnRoute(NodeList: TKMPointList);
var
  I: Integer;
begin
  NodeList.Clear;

  //Assemble the route
  I := fMinCost.ID;
  repeat
    NodeList.AddEntry(OList[I].Pos);
    I := OList[I].Parent;
  until I = 0;

  //Reverse the list, since path is assembled LocB > LocA
  NodeList.Inverse;
end;


end.
