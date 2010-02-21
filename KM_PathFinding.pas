unit KM_PathFinding;
interface            
uses ExtCtrls, SysUtils, Controls, Forms, KromUtils, KM_Defaults, KM_Terrain, KM_Utils, KM_CommonTypes;

type TDestinationPoint = (dp_Location, dp_Passability);
const TEST_MAX_WALK_PATH = 8192; //todo: 8k is too small for mission TGE #4 (192x192 map by Alanbach)

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
    ORef:array[1..MaxMapSize,1..MaxMapSize] of word; //Ref to OpenList
    OCount:word;
    OList:array[1..TEST_MAX_WALK_PATH]of record //List of checked cells
      Pos:TKMPoint;
      CostTo:word;
      Estim:word;
      Parent:word;//Reference to parent
    end;
  private
    LocA:TKMPoint;
    LocB:TKMPoint;
    Avoid:TKMPoint;
    Pass:TPassability;
    TargetRoadNetworkID:byte;
    WalkToSpot:boolean;
    IsInteractionAvoid:boolean;
    fDestination:TDestinationPoint;
    fRouteSuccessfullyBuilt:boolean;
    function CheckRouteCanExist():boolean;
    procedure InitRoute();
    function MakeRoute():boolean;
    function IsDestinationReached():boolean;
  public
    constructor Create(aLocA, aLocB, aAvoid:TKMPoint; aPass:TPassability; aWalkToSpot:boolean; aIsInteractionAvoid:boolean=false); overload;
    constructor Create(aLocA:TKMPoint; aTargetRoadNetworkID:byte; fPass:TPassability; aLocB:TKMPoint); overload;
    procedure ReturnRoute(out NodeList:TKMPointList);
    property RouteSuccessfullyBuilt:boolean read fRouteSuccessfullyBuilt;
  end;

implementation
uses KM_Unit1, KM_UnitActionWalkTo;


constructor TPathFinding.Create(aLocA, aLocB, aAvoid:TKMPoint; aPass:TPassability; aWalkToSpot:boolean; aIsInteractionAvoid:boolean=false);
begin
  LocA := aLocA;
  LocB := aLocB;
  Avoid := aAvoid;
  Pass := aPass;
  TargetRoadNetworkID := 0; //erase just in case
  WalkToSpot := aWalkToSpot;
  IsInteractionAvoid := aIsInteractionAvoid;
  fRouteSuccessfullyBuilt := false;
  fDestination:=dp_Location;

  if not CheckRouteCanExist then exit;

  InitRoute();
  fRouteSuccessfullyBuilt := MakeRoute(); //
end;


constructor TPathFinding.Create(aLocA:TKMPoint; aTargetRoadNetworkID:byte; fPass:TPassability; aLocB:TKMPoint);
begin
  LocA := aLocA;
  LocB := aLocB; //Even though we are only going to a road network it is useful to know where our target is so we start off in the right direction (makes algorithm faster/work over long distances)
  Avoid := KMPoint(0,0); //erase just in case
  Pass := fPass; //Should be unused here
  TargetRoadNetworkID := aTargetRoadNetworkID;
  WalkToSpot := false;
  fRouteSuccessfullyBuilt := false;
  fDestination:=dp_Passability;

  InitRoute();
  fRouteSuccessfullyBuilt := MakeRoute(); //
end;


procedure TPathFinding.ReturnRoute(out NodeList:TKMPointList);
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
    //Nodes[0]:=LocA;


  //Should ajoin straight pieces to reduce mem usage
  //Important rule:
  // - First node is LocA,
  // - next to last node is neighbour of LocB (important for delivery to workers),
  // - last node is LocB

  {if NodeCount>3 then begin
  k:=2;
  for i:=3 to NodeCount-1 do begin //simplify within LocA+1 .. LocB-2 range
  // i/k are always -1 since array is [0..Count-1] range
    if (sign(Nodes[k-1].X-Nodes[k-2].X) = sign(Nodes[i-1].X-Nodes[i-2].X))and //Direction matches
       (sign(Nodes[k-1].Y-Nodes[k-2].Y) = sign(Nodes[i-1].Y-Nodes[i-2].Y)) then begin
      Nodes[k-1]:=Nodes[i-1];
    end else begin
      inc(k);
      Nodes[k-1]:=Nodes[i-1];
    end;
  end;
  inc(k);
  Nodes[k-1]:=Nodes[NodeCount-1];
  NodeCount:=k;
  end;}
end;



function TPathFinding.CheckRouteCanExist():boolean;
begin
  //Don't try to make a route if it's obviously impossible
  if IsInteractionAvoid then
  begin
    fTerrain.RebuildWalkConnect(canWalkAvoid);
    Result := fTerrain.Route_CanBeMade(LocA,LocB,canWalkAvoid,WalkToSpot);
  end
  else
    Result := fTerrain.Route_CanBeMade(LocA,LocB,Pass,WalkToSpot);
end;


procedure TPathFinding.InitRoute();
begin
  FillChar(ORef,SizeOf(ORef),#0);
  FillChar(OList,SizeOf(OList),#0);
  OCount:=1;
  ORef[LocA.Y,LocA.X]:=OCount;
  OList[OCount].Pos:=LocA;
  OList[OCount].CostTo:=0;
  OList[OCount].Estim:=(abs(LocB.X-LocA.X) + abs(LocB.Y-LocA.Y))*10;
  OList[OCount].Parent:=0;
end;


function TPathFinding.IsDestinationReached():boolean;
begin
  case fDestination of
    dp_Location:    Result := KMSamePoint(MinCost.Pos,LocB) or ((not WalkToSpot) and (KMLength(MinCost.Pos,LocB)<1.5));
    dp_Passability: if Pass = canWorker then
                      Result := fTerrain.GetWalkConnectID(MinCost.Pos) = TargetRoadNetworkID
                    else
                      Result := fTerrain.GetRoadConnectID(MinCost.Pos) = TargetRoadNetworkID;
    else            Result := true;
  end;
end;


function TPathFinding.MakeRoute():boolean;
const c_closed=65535;
var i,x,y:integer;
begin

  repeat

    //Find cell with least (Estim+CostTo)
    MinCost.Cost:=65535;
    //for i:=1 to OCount do //'downto' works faster here
    for i:=OCount downto 1 do
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
      for y:=MinCost.Pos.Y-1 to MinCost.Pos.Y+1 do for x:=MinCost.Pos.X-1 to MinCost.Pos.X+1 do

      if fTerrain.TileInMapCoords(x,y) then //Ignore those outside of MapCoords
      if fTerrain.CanWalkDiagonaly(MinCost.Pos,KMPoint(x,y)) then
      if (not IsInteractionAvoid) or (fTerrain.Land[y,x].Markup <> mu_UnderConstruction) then //If we are in InteractionAvoid mode then don't use tiles with workers on them (under contstruction)
      if not KMSamePoint(KMPoint(x,y),Avoid) then //If there are any cells in Avoid list then avoid them

        if ORef[y,x]=0 then begin //Cell is new

          inc(OCount);
          OList[OCount].Pos:=KMPoint(x,y);

          if (Pass in fTerrain.Land[y,x].Passability) then //If cell meets Passability then estimate it
          begin
            ORef[y,x]:=OCount;
            OList[OCount].Parent:=ORef[MinCost.Pos.Y,MinCost.Pos.X];
            OList[OCount].CostTo:=OList[OList[OCount].Parent].CostTo+round(GetLength(KMPoint(x,y),MinCost.Pos)*10); //
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
              //OList[ORef[y,x]].Estim:=(abs(x-LocB.X) + abs(y-LocB.Y))*10;
            end;
          end;

        end;
    end;

  until( (OCount+8>=length(OList))or //List can't hold any more cells,
         IsDestinationReached or // Destination point is reached
         (MinCost.Cost=65535)); // There's no more open cells available

  Result := IsDestinationReached;
  //fLog.AssertToLog(MinCost.Cost<>65535, 'FloodFill test failed and there''s no possible route A-B');
end;


end.

