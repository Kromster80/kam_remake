unit KM_PathFinding;
{$I KaM_Remake.inc}
interface            
uses SysUtils, Math, KromUtils, KM_Defaults, KM_Terrain, KM_Utils, KM_CommonTypes, KM_Houses;

type TDestinationPoint = (dp_Location, dp_Passability, dp_House);

type
  { Here should be pathfinding and all associated stuff }
  TPathFinding = class
  private
    NewCost:integer;
    MinCost:record
      Cost:integer;
      ID:word;
      Pos:TKMPoint;
    end;
    ORef:array[1..MAX_MAP_SIZE,1..MAX_MAP_SIZE] of word; //Ref to OpenList
    OCount:word;
    OList:array of record //List of checked cells
      Pos:TKMPoint;
      CostTo:word;
      Estim:word;
      Parent:word;//Reference to parent
    end;
  private
    LocA:TKMPoint;
    LocB:TKMPoint;
    Pass:TPassability;
    TargetWalkConnect:TWalkConnect;
    TargetNetwork:byte;
    fDistance:single;
    IsInteractionAvoid:boolean;
    fDestination:TDestinationPoint;
    fTargetHouse: TKMHouse;
    fRouteSuccessfullyBuilt:boolean;
    function CheckRouteCanExist:boolean;
    procedure InitRoute;
    function IsDestinationReached:boolean;
    function MakeRoute:boolean;
  public
    constructor Create(aLocA, aLocB:TKMPoint; aPass:TPassability; aDistance:single; aTargetHouse:TKMHouse; aIsInteractionAvoid:boolean=false); overload;
    constructor Create(aLocA:TKMPoint; aTargetWalkConnect:TWalkConnect; aTargetNetwork:byte; fPass:TPassability; aLocB:TKMPoint); overload;
    procedure ReturnRoute(var NodeList:TKMPointList);
    property RouteSuccessfullyBuilt:boolean read fRouteSuccessfullyBuilt;
  end;

implementation


constructor TPathFinding.Create(aLocA, aLocB:TKMPoint; aPass:TPassability; aDistance:single; aTargetHouse:TKMHouse; aIsInteractionAvoid:boolean=false);
begin
  Inherited Create;
  LocA := aLocA;
  LocB := aLocB;
  Pass := aPass;
  TargetNetwork := 0; //erase just in case
  TargetWalkConnect := wcWalk; //erase just in case
  fDistance := aDistance;
  IsInteractionAvoid := aIsInteractionAvoid;
  fRouteSuccessfullyBuilt := false;
  fTargetHouse := aTargetHouse;
  if fTargetHouse = nil then
    fDestination := dp_Location
  else
    fDestination := dp_House;

  if not CheckRouteCanExist then exit;

  InitRoute;
  fRouteSuccessfullyBuilt := MakeRoute; //
end;


constructor TPathFinding.Create(aLocA:TKMPoint; aTargetWalkConnect:TWalkConnect; aTargetNetwork:byte; fPass:TPassability; aLocB:TKMPoint);
begin
  Inherited Create;
  LocA := aLocA;
  LocB := aLocB; //Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
  Pass := fPass; //Should be unused here
  TargetWalkConnect := aTargetWalkConnect;
  TargetNetwork := aTargetNetwork;
  fDistance := 0;
  fRouteSuccessfullyBuilt := false;
  fDestination := dp_Passability;

  InitRoute;
  fRouteSuccessfullyBuilt := MakeRoute; //
end;


//Don't try to make a route if it's obviously impossible
function TPathFinding.CheckRouteCanExist:boolean;
begin
  if IsInteractionAvoid then fTerrain.RebuildWalkConnect(wcAvoid); //Rebuild on demand
  if fDestination = dp_House then
    Result := fTerrain.Route_CanBeMadeToHouse(LocA,fTargetHouse,Pass,fDistance,IsInteractionAvoid)
  else
    Result := fTerrain.Route_CanBeMade(LocA,LocB,Pass,fDistance,IsInteractionAvoid);
end;


procedure TPathFinding.InitRoute;
begin
  FillChar(ORef,SizeOf(ORef),#0);
  setlength(OList,0); //reset length
  OCount:=1;
  ORef[LocA.Y,LocA.X]:=OCount;
  setlength(OList,OCount+1);
  OList[OCount].Pos     := LocA;
  OList[OCount].CostTo  := 0;
  OList[OCount].Estim   := (abs(LocB.X-LocA.X) + abs(LocB.Y-LocA.Y)) * 10;
  OList[OCount].Parent  := 0;
end;


function TPathFinding.IsDestinationReached:boolean;
begin
  case fDestination of
    dp_Location:    Result := KMLength(MinCost.Pos,LocB) <= fDistance;
    dp_Passability: Result := fTerrain.GetConnectID(TargetWalkConnect, MinCost.Pos) = TargetNetwork;
    dp_House:       Result := fTargetHouse.GetDistance(MinCost.Pos) <= fDistance;
    else            Result := true;
  end;
end;


function TPathFinding.MakeRoute:boolean;
const c_closed=65535;
var i,x,y:integer;
begin

  repeat

    //Find cell with least (Estim+CostTo)
    MinCost.Cost:=65535;

    for i:=OCount downto 1 do //'downto' works faster here
    if OList[i].Estim<>c_closed then
    if (OList[i].Estim+OList[i].CostTo) < MinCost.Cost then begin
      MinCost.Cost:=OList[i].Estim+OList[i].CostTo;
      MinCost.ID:=i;
      MinCost.Pos:=OList[i].Pos;
    end;

    //Keep looking if we haven't reached destination
    if not IsDestinationReached then begin

      OList[MinCost.ID].Estim:=c_closed;

      //Check all surrounding cells and issue costs to them
      for y:=Math.max(MinCost.Pos.Y-1,1) to Math.min(MinCost.Pos.Y+1, fTerrain.MapY-1) do
      for x:=Math.max(MinCost.Pos.X-1,1) to Math.min(MinCost.Pos.X+1, fTerrain.MapX-1) do
        if ORef[y,x]=0 then //Cell is new
        if fTerrain.CanWalkDiagonaly(MinCost.Pos,KMPoint(x,y)) then
        //If we are in InteractionAvoid mode then don't use tiles with workers on them
        //and avoid other tiles with Locked units, but that requires reworking
        //But e.g. melee warriors might ignore this and fight their way through enemies?
        if (not IsInteractionAvoid) or (not fTerrain.TileIsLocked(KMPoint(x,y))) then
        begin

          inc(OCount);
          if (length(OList)-1)<OCount then setlength(OList, OCount+128); //Allocate slightly more space
          OList[OCount].Pos:=KMPoint(x,y);

          if (Pass in fTerrain.Land[y,x].Passability) then //If cell meets Passability then estimate it
          begin
            ORef[y,x]:=OCount;
            OList[OCount].Parent:=ORef[MinCost.Pos.Y,MinCost.Pos.X];
            OList[OCount].CostTo:=OList[OList[OCount].Parent].CostTo+round(GetLength(KMPoint(x,y),MinCost.Pos)*10); //
            if DO_WEIGHT_ROUTES and not KMSamePoint(LocB, KMPoint(x,y)) then //Do not add extra cost if the tile is the target, as it can cause a longer route to be chosen
              inc(OList[OCount].CostTo, byte(fTerrain.Land[y,x].IsUnit<>nil)*10); //Unit=1tile
            OList[OCount].Estim:=(abs(x-LocB.X) + abs(y-LocB.Y)) *10; //Use Estim even if destination is Passability, as it will make it faster. Target should be in the right direction even though it's not our destination.
          end else //If cell doen't meets Passability then mark it as Closed
            OList[OCount].Estim:=c_closed;

        end else begin

          //If route through new cell is shorter than ORef[y,x] then
          if OList[ORef[y,x]].Estim<>c_closed then begin
            NewCost:=round(GetLength(KMPoint(x,y),MinCost.Pos)*10);
            if OList[MinCost.ID].CostTo + NewCost < OList[ORef[y,x]].CostTo then begin
              OList[ORef[y,x]].Parent:=ORef[MinCost.Pos.Y,MinCost.Pos.X];
              OList[ORef[y,x]].CostTo:=OList[MinCost.ID].CostTo + NewCost;
            if DO_WEIGHT_ROUTES then
              inc(OList[ORef[y,x]].CostTo, byte(fTerrain.Land[y,x].IsUnit<>nil)*10); //Unit=1tile
              //OList[ORef[y,x]].Estim:=(abs(x-LocB.X) + abs(y-LocB.Y))*10;
            end;
          end;

        end;
    end;

  until( IsDestinationReached or // Destination point is reached
         (MinCost.Cost=65535)); // There's no more open cells available

  Result := IsDestinationReached;
  //Assert(MinCost.Cost<>65535, 'FloodFill test failed and there''s no possible route A-B');
end;


procedure TPathFinding.ReturnRoute(var NodeList:TKMPointList);
var i,k:integer; NodesCount:integer;
begin
  NodeList.Clearup;

  if not fRouteSuccessfullyBuilt then
  begin
    NodeList.Clearup; //Something went wrong
    exit;
  end;

  //Calculate NodeCount
  k:=MinCost.ID; NodesCount:=0;
  repeat
    inc(NodesCount);
    k:=OList[k].Parent;
  until(k=0);

  {if NodeCount > length(Nodes) then begin
    NodeCount:=0; //Something went wrong
    Nodes[0]:=LocA;
    exit;
  end;}

  //Assemble the route reversing the list, since path is LocB>LocA in fact
  k:=MinCost.ID;
  for i:=1 to NodesCount do begin
    NodeList.AddEntry(OList[k].Pos);
    //Nodes[NodeCount-i]:=OList[k].Pos;
    k:=OList[k].Parent;
  end;

  NodeList.Inverse;
end;


end.

