unit KM_UnitActionWalkTo;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Houses, KM_Units, SysUtils, Math;

type
  TDestinationCheck = (dc_NoChanges, dc_RouteChanged, dc_NoRoute);
  TTargetDiedCheck = (tc_NoChanges, tc_TargetUpdated, tc_Died);
  TObstacleCheck = (oc_NoObstacle, oc_ReRouteMade, oc_NoRoute);

//INTERACTION CONSTANTS: (may need to be tweaked for optimal performance)
//TIMEOUT is the time after which each solution things will be checked.
//FREQ is the frequency that it will be checked, to save CPU time.
//     e.g. 10 means check when TIMEOUT is first reached then every 10 ticks after that.
//     Lower FREQ will mean faster solutions but more CPU usage. Only solutions with time consuming checks have FREQ
const
  EXCHANGE_TIMEOUT = 0;                      //Pass with unit
  PUSH_TIMEOUT     = 1;                      //Push unit out of the way
  PUSHED_TIMEOUT   = 10;                     //Try a different way when pushed
  DODGE_TIMEOUT    = 5;     DODGE_FREQ = 8;  //Pass with a unit on a tile next to our target if they want to
  AVOID_TIMEOUT    = 10;    AVOID_FREQ = 20; //Go around busy units
  SIDESTEP_TIMEOUT = 10; SIDESTEP_FREQ = 15; //Step to empty tile next to target
  WAITING_TIMEOUT  = 40;                     //After this time we can be forced to exchange


type
  TUnitActionWalkTo = class(TUnitAction)
    private
      fWalker:TKMUnit; //Who's walking
      fTargetUnit:TKMUnit; //Folow this unit
      fTargetHouse:TKMHouse; //Go to this House
      fWalkFrom, fWalkTo, fNewWalkTo:TKMPoint;
      fWalkToSpot:byte; //How close we need to get to our aim
      fPass:TPassability; //Desired passability set once on Create
      DoesWalking, WaitingOnStep, fDestBlocked:boolean;
      DoExchange:boolean; //Command to make exchange maneuver with other unit, should use MakeExchange when vertex use needs to be set
      fInteractionCount, fLastSideStepNodePos: integer;
      fInteractionStatus: TInteractionStatus;
      function AssembleTheRoute():boolean;
      function CheckForNewDestination():TDestinationCheck;
      function CheckTargetHasDied():TTargetDiedCheck;
      function CheckForObstacle():TObstacleCheck;
      function CheckInteractionFreq(aIntCount,aTimeout,aFreq:integer):boolean;
      function DoUnitInteraction():boolean;
        //Sub functions split out of DoUnitInteraction (these are the solutions)
        function IntCheckIfPushing(fOpponent:TKMUnit):boolean;
        function IntSolutionPush(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
        function IntSolutionExchange(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
        function IntCheckIfPushed(HighestInteractionCount:integer):boolean;
        function IntSolutionDodge(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
        function IntSolutionAvoid(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
        function IntSolutionSideStep(aPosition:TKMPoint; HighestInteractionCount:integer):boolean;

      procedure ChangeStepTo(aPos: TKMPoint);
      procedure PerformExchange(ForcedExchangePos:TKMPoint);
      procedure IncVertex;
      procedure DecVertex;
      procedure SetInitValues;
      function CanAbandonInternal: boolean;
      function GetNextNextPosition():TKMPoint;
      function GetEffectivePassability:TPassability; //Returns passability that unit is allowed to walk on
      procedure ExplanationLogAdd;
    public
      NodeList:TKMPointList;
      fVertexOccupied: TKMPoint; //Public because it needs to be used by AbandonWalk
      NodePos:integer;
      Explanation:string; //Debug only, explanation what unit is doing
      ExplanationLog:TStringList;
      constructor Create(aUnit: TKMUnit; aLocB:TKMPoint; aActionType:TUnitActionType; aWalkToSpot:byte; aSetPushed:boolean; aWalkToNear:boolean; aTargetUnit:TKMUnit; aTargetHouse:TKMHouse);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      function CanAbandonExternal: boolean;
      property GetInteractionStatus:TInteractionStatus read fInteractionStatus;
      procedure ChangeWalkTo(aLoc:TKMPoint; const aWalkToNear:boolean=false; aNewTargetUnit:TKMUnit=nil); //Modify route to go to this destination instead
      function Execute(KMUnit: TKMUnit):TActionResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Terrain, KM_UnitActionGoInOut, KM_UnitActionStay, Controls, KM_Units_Warrior;


{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create(aUnit: TKMUnit; aLocB:TKMPoint; aActionType:TUnitActionType; aWalkToSpot:byte; aSetPushed:boolean; aWalkToNear:boolean; aTargetUnit:TKMUnit; aTargetHouse:TKMHouse);
var RouteBuilt:boolean; //Check if route was built, otherwise return nil
begin
  Inherited Create(aActionType);
  fActionName   := uan_WalkTo;
  Locked        := false; //Eqiuvalent to Can AbandonExternal?
  fWalker       := aUnit;
  if aTargetUnit <> nil then fTargetUnit := aTargetUnit.GetUnitPointer;
  if aTargetHouse <> nil then fTargetHouse := aTargetHouse.GetHousePointer;
  fWalkFrom     := fWalker.GetPosition;
  fWalkToSpot   := aWalkToSpot;
  fNewWalkTo    := KMPoint(0,0);
  fPass         := fWalker.GetDesiredPassability;
  if aWalkToNear then
    fWalkTo     := fTerrain.GetClosestTile(aLocB,aUnit.GetPosition,fPass)
  else
    fWalkTo     := aLocB;

  if WRITE_WALKTO_LOG then begin
    ExplanationLog := TStringList.Create;
    if FileExists(ExeDir+'ExpLog'+inttostr(fWalker.ID)+'.txt') then
      ExplanationLog.LoadFromFile(ExeDir+'ExpLog'+inttostr(fWalker.ID)+'.txt');
  end;

  Explanation := 'Walk action created';
  ExplanationLogAdd;

  if fWalkTo.X*fWalkTo.Y = 0 then
    fGame.GameError(fWalkTo, 'WalkTo 0:0');

  NodeList      := TKMPointList.Create; //Freed on destroy
  SetInitValues;

  if KMSamePoint(fWalkFrom,fWalkTo) then //We don't care for this case, Execute will report action is done immediately
    exit; //so we don't need to perform any more processing

  RouteBuilt := AssembleTheRoute();
  //Due to rare circumstances (e.g. floodfill doesn't take notice of CanWalkDiagonally i.e. trees on road corners)
  // there are times when trying to build a route along roads will fail.
  // To reduce crash errors, try rebuilding it with just canWalk. This will normally fix the problem and
  // It's not a big deal if occasionally units walk off the road.
  if (not RouteBuilt) and (fPass = canWalkRoad) then
  begin
    fPass := canWalk;
    RouteBuilt := AssembleTheRoute();
  end;

  if aSetPushed then begin
    fInteractionStatus:=kis_Pushed; //So that unit knows it was pushed not just walking somewhere
    Explanation:='We were asked to get out of the way';
    ExplanationLogAdd;
    fPass := GetEffectivePassability; //Units are allowed to step off roads when they are pushed
    //Because the passability has changed we might need to reassemble the route if it failed in create
    if not RouteBuilt then RouteBuilt := AssembleTheRoute();
  end;

  if not RouteBuilt then
  begin
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with default fPass');
    exit; //NoList.Count = 0, means it will exit in Execute
  end;
end;


procedure TUnitActionWalkTo.ExplanationLogAdd;
begin
  if not WRITE_WALKTO_LOG then exit;
  ExplanationLog.Add(Format(
  '%d'+#9+'%d:%d > %d:%d > %d:%d'+#9+Explanation+'',
  [
  fGame.GetTickCount,
  fWalker.PrevPosition.X,
  fWalker.PrevPosition.Y,
  fWalker.GetPosition.X,
  fWalker.GetPosition.Y,
  fWalker.NextPosition.X,
  fWalker.NextPosition.Y  
  ]))
end;


procedure TUnitActionWalkTo.SetInitValues;
begin
  NodePos       := 1;
  DoExchange    := false;
  DoesWalking   := false;
  WaitingOnStep := false;
  fDestBlocked  := false;
  fLastSideStepNodePos := -2; //Start negitive so it is at least 2 less than NodePos at the start
  fVertexOccupied      := KMPoint(0,0);
  fInteractionCount    := 0;
  fInteractionStatus   := kis_None;
end;


constructor TUnitActionWalkTo.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fWalker, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fTargetUnit, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fWalkFrom);
  LoadStream.Read(fWalkTo);
  LoadStream.Read(fVertexOccupied);
  LoadStream.Read(fNewWalkTo);
  LoadStream.Read(fWalkToSpot);
  LoadStream.Read(fDestBlocked);
  LoadStream.Read(fLastSideStepNodePos);
  LoadStream.Read(fPass, SizeOf(fPass));
  LoadStream.Read(DoesWalking);
  LoadStream.Read(DoExchange);
  LoadStream.Read(WaitingOnStep);
  LoadStream.Read(fInteractionCount);
  LoadStream.Read(fInteractionStatus, SizeOf(fInteractionStatus));

  NodeList := TKMPointList.Create; //Freed on destroy
  NodeList.Load(LoadStream);
  LoadStream.Read(NodePos);
end;


procedure TUnitActionWalkTo.SyncLoad();
begin
  Inherited;
  fWalker       := fPlayers.GetUnitByID(cardinal(fWalker));
  fTargetUnit   := fPlayers.GetUnitByID(cardinal(fTargetUnit));
end;


destructor TUnitActionWalkTo.Destroy;
begin
  if WRITE_WALKTO_LOG then begin
    Explanation := 'WalkTo destroyed at'+floattostr(fWalker.PositionF.X)+':'+floattostr(fWalker.PositionF.Y);
    ExplanationLogAdd;
    ExplanationLog.SaveToFile(ExeDir+'ExpLog'+inttostr(fWalker.ID)+'.txt');
  end;
  FreeAndNil(NodeList);

  if not KMSamePoint(fVertexOccupied,KMPoint(0,0)) then
    DecVertex;

  fWalker.IsExchanging := false;

  if fTargetUnit <> nil then fTargetUnit.ReleaseUnitPointer;
  if fTargetHouse <> nil then fTargetHouse.ReleaseHousePointer;
  Inherited;
end;


function TUnitActionWalkTo.CanAbandonInternal: boolean;
begin
  Result := (fInteractionStatus<>kis_Pushed) //Can be removed, but decreases effectiveness
            and (not DoExchange); //Other unit could have set this
end;


{ Returns true only when unit is stuck for some reason }
function TUnitActionWalkTo.CanAbandonExternal: boolean;
begin
  Result := (not DoExchange) //Other unit could have set this
            and KMSamePointF(KMPointF(fWalker.GetPosition),fWalker.PositionF);
end;


procedure TUnitActionWalkTo.PerformExchange(ForcedExchangePos:TKMPoint);
begin
  //If we are being forced to exchange then modify our route to make the exchange,
  //  then return to the tile we are currently on, then continue the route
  if not KMSamePoint(ForcedExchangePos,KMPoint(0,0)) then
  begin
    Explanation:='We were forced to exchange places';
    ExplanationLogAdd;
    DoExchange := true;
    if KMLength(ForcedExchangePos,NodeList.List[NodePos+1]) >= 1.5 then
      NodeList.InjectEntry(NodePos+1,fWalker.GetPosition); //We must back-track if we cannot continue our route from the new tile
    NodeList.InjectEntry(NodePos+1,ForcedExchangePos);
    if KMSamePoint(fWalker.GetPosition, ForcedExchangePos) then
      fGame.GameError(fWalker.GetPosition, 'Exchange to same place');
    fWalker.Direction := KMGetDirection(fWalker.GetPosition,ForcedExchangePos);
    DoesWalking:=true;
  end
  else
  begin
    //Unforced exchanging
    Explanation:='We were asked to exchange places';
    ExplanationLogAdd;
    DoExchange := true;
  end;
end;


//Used for dodging and side stepping
procedure TUnitActionWalkTo.ChangeStepTo(aPos: TKMPoint);
begin
  if (NodePos+2 <= NodeList.Count) and (KMLength(aPos,NodeList.List[NodePos+2]) < 1.5) then
    NodeList.List[NodePos+1] := aPos //We can simply replace the entry because it is near the next tile
  else //Otherwise we must inject it
    NodeList.InjectEntry(NodePos+1,aPos);

  fWalker.Direction := KMGetDirection(fWalker.GetPosition,aPos); //Face the new tile
end;


function TUnitActionWalkTo.AssembleTheRoute():boolean;
var i:integer; NodeList2:TKMPointList; TmpPass: TPassability;
begin
  TmpPass := fPass;
  //Build a piece of route to return to nearest road piece connected to destination road network
  if (fPass = canWalkRoad) and (fWalkToSpot=0) then //That is Citizens walking to spot
    if (fTerrain.GetRoadConnectID(fWalkFrom) <> fTerrain.GetRoadConnectID(fWalkTo)) and  //NoRoad returns 0
      (fTerrain.GetRoadConnectID(fWalkTo) <> 0) then //Don't bother returning to the road if our target is off road anyway
      fTerrain.Route_ReturnToRoad(fWalkFrom, fWalkTo, fTerrain.GetRoadConnectID(fWalkTo), NodeList);

  //If we are a worker on a construction site, build a piece of route to return to nearest walkable tile on the
  if fPass = canWorker then //That is Workers on a construction site
    if (fTerrain.GetWalkConnectID(fWalkFrom) <> fTerrain.GetWalkConnectID(fWalkTo)) and  //Not walkable returns 0
      (fTerrain.GetWalkConnectID(fWalkTo) <> 0) then //Don't bother returning to the road if our target is not walkable
    begin
      fTerrain.Route_ReturnToWalkable(fWalkFrom, fWalkTo, fTerrain.GetWalkConnectID(fWalkTo), NodeList);
      TmpPass := canWalk; //After this piece of route we are in walk mode
    end;

  //Build a route A*
  if NodeList.Count=0 then //Build a route from scratch
    fTerrain.Route_Make(fWalkFrom, fWalkTo, fPass, fWalkToSpot, NodeList) //Try to make the route with fPass
  else begin //Append route to existing part
    NodeList2 := TKMPointList.Create;
    fTerrain.Route_Make(NodeList.List[NodeList.Count], fWalkTo, TmpPass, fWalkToSpot, NodeList2); //Try to make the route with fPass
    for i:=2 to NodeList2.Count do
      NodeList.AddEntry(NodeList2.List[i]);
    FreeAndNil(NodeList2);
  end;

  Result := NodeList.Count > 0;
end;


function TUnitActionWalkTo.CheckForNewDestination():TDestinationCheck;
begin
  if KMSamePoint(fNewWalkTo,KMPoint(0,0)) then
    Result := dc_NoChanges
  else begin
    Result := dc_RouteChanged;
    fWalkTo := fNewWalkTo;
    fWalkFrom := NodeList.List[NodePos];
    fNewWalkTo := KMPoint(0,0);
    //Delete everything past NodePos and add new route from there
    NodeList.Count := NodePos;
    if not AssembleTheRoute() then
      Result := dc_NoRoute;
  end;
end;


function TUnitActionWalkTo.CheckTargetHasDied():TTargetDiedCheck;
begin
  if (fTargetUnit=nil) or not fTargetUnit.IsDeadOrDying then
    Result := tc_NoChanges
  else begin
    if (fWalker is TKMUnitWarrior) and (fTargetUnit is TKMUnitWarrior) and (TKMUnitWarrior(fWalker).GetWarriorState <> ws_Engage) then
    begin
      //If a warrior is following a unit it means we are attacking it. (for now anyway)
      //So if this unit dies we must now follow it's commander
      fTargetUnit := TKMUnitWarrior(fTargetUnit).GetCommander.GetUnitPointer;
      //If unit becomes nil that is fine, we will simply walk to it's last known location. But update fOrderLoc to make sure this happens!
      TKMUnitWarrior(fWalker).OrderLocDir := KMPointDir(fWalkTo,TKMUnitWarrior(fWalker).OrderLocDir.Dir);
      Result := tc_TargetUpdated;
    end else
      Result := tc_Died;
  end;
end;


{ There's unexpected immovable obstacle on our way (tree, wall, house)
1. go around the obstacle and keep on walking
2. rebuild the route
We chose #2 because if something's changed it's likely that there are many new obstacles,
but in best approch:
  try to walk around few obstacles and if failed - build new route}
function TUnitActionWalkTo.CheckForObstacle():TObstacleCheck;
begin
  Result := oc_NoObstacle;
  //If there's an unexpected obstacle (i.e. the terrain has changed since we calculated the route)
  //Use GetOurPassability so that canWalkRoad is changed to canWalk because walking off the road does not count as an obstacle
  if (not fTerrain.CheckPassability(NodeList.List[NodePos+1],GetEffectivePassability)) or
     (not fTerrain.CanWalkDiagonaly(fWalker.GetPosition,NodeList.List[NodePos+1])) then

    {//Try to find a walkaround @Lewin: Please check me on this one, is it a valid solution?
    if IntSolutionSideStep(NodeList.List[NodePos+1],fInteractionCount) then
      Result:= oc_NoObstacle
    else}

    if fTerrain.Route_CanBeMade(fWalker.GetPosition,fWalkTo,GetEffectivePassability,fWalkToSpot) then
    begin
      fWalker.SetActionWalkToSpot(fWalkTo,GetActionType,fWalkToSpot);
      //todo 1: replace with regard to TargetUnit/TargetHouse!
      Result:= oc_ReRouteMade;
    end else
      Result := oc_NoRoute;
end;


procedure TUnitActionWalkTo.IncVertex;
begin
  //Tell fTerrain that this vertex is being used so no other unit walks over the top of us
  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
  begin
    fGame.GameError(fVertexOccupied, 'IncVertex');
    exit;
  end;

  fTerrain.UnitVertexAdd(KMGetDiagVertex(fWalker.PrevPosition,fWalker.NextPosition));
  fVertexOccupied := KMGetDiagVertex(fWalker.PrevPosition,fWalker.NextPosition);
end;


procedure TUnitActionWalkTo.DecVertex;
begin
  //Tell fTerrain that this vertex is not being used anymore
  if KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
  begin
    fGame.GameError(fVertexOccupied, 'DecVertex 0:0');
    exit;
  end;

  fTerrain.UnitVertexRem(fVertexOccupied);
  fVertexOccupied := KMPoint(0,0);
end;


function TUnitActionWalkTo.IntCheckIfPushing(fOpponent:TKMUnit):boolean;
begin
  Result := false;
  //If we are asking someone to move away then just wait until they are gone
  if (fInteractionStatus <> kis_Pushing) then exit;
    if (fOpponent.GetUnitAction is TUnitActionWalkTo) and //Make sure they are still moving out of the way
      (TUnitActionWalkTo(fOpponent.GetUnitAction).GetInteractionStatus = kis_Pushed) then
    begin
      Explanation := 'Unit is blocking the way and has been asked to move';
      ExplanationLogAdd;
      Result := true; //Means exit DoUnitInteraction
    end
    else
    begin //We pushed a unit out of the way but someone else took it's place! Now we must start over to solve problem with this new opponent
      fInteractionCount := 0;
      fInteractionStatus := kis_Trying;
      Explanation := 'Someone took the pushed unit''s place';
      ExplanationLogAdd;
    end;
end;


{ We can push idling unit }
function TUnitActionWalkTo.IntSolutionPush(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
var OpponentPassability: TPassability;
begin
  Result := false;
  if HighestInteractionCount < PUSH_TIMEOUT then exit;
  //Ask the other unit to step aside, only if they are idle!
  if (fOpponent.GetUnitAction is TUnitActionStay) and
     (not TUnitActionStay(fOpponent.GetUnitAction).Locked) then
  begin
    fInteractionStatus := kis_Pushing;
    OpponentPassability := fOpponent.GetDesiredPassability;
    if OpponentPassability = canWalkRoad then OpponentPassability := canWalk;

    if not CanAbandonInternal then begin
      fGame.GameError(fWalker.GetPosition, 'Unit walk IntSolutionPush');
      exit;
    end;

    fOpponent.SetActionWalkPushed(fTerrain.GetOutOfTheWay(fOpponent.GetPosition,fWalker.GetPosition,OpponentPassability));

    Explanation := 'Unit was blocking the way but it has been forced to go away now';
    ExplanationLogAdd; //Next frame tile will be free and unit will walk there
    Result := true; //Means exit DoUnitInteraction
  end;
end;


function TUnitActionWalkTo.IntSolutionExchange(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
begin
  Result := false;
  //Do not initiate exchanges if we are in DestBlocked mode, as we are zero priority and other units will
  if not fDestBlocked and (((HighestInteractionCount >= EXCHANGE_TIMEOUT) and (fInteractionStatus <> kis_Pushed)) or //When pushed this timeout/counter is different
     (fInteractionStatus = kis_Pushed)) then //If we get pushed then always try exchanging (if we are here then there is no free tile)
  begin //Try to exchange with the other unit if they are willing
    //If Unit on the way is walking somewhere and not exchanging with someone else
    if (fOpponent.GetUnitAction is TUnitActionWalkTo) and (not TUnitActionWalkTo(fOpponent.GetUnitAction).DoExchange)
      //Unit not yet arrived on tile, wait till it does, otherwise there might be 2 units on one tile
      and (not TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking) then
    //Check that our tile is walkable for the opponent! (we could be a worker on a building site)
    if (TUnitActionWalkTo(fOpponent.GetUnitAction).GetEffectivePassability in fTerrain.Land[fWalker.GetPosition.Y,fWalker.GetPosition.X].Passability) then
    begin
      //Check unit's future position is where we are now and exchange (use NodeList rather than direction as it's not always right)
      if KMSamePoint(TUnitActionWalkTo(fOpponent.GetUnitAction).GetNextNextPosition, fWalker.GetPosition) then
      begin
        //Graphically both units are walking side-by-side, but logically they simply walk through each-other.
        TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange

        Explanation:='Unit in the way is walking in the opposite direction. Performing an exchange';
        ExplanationLogAdd;
        DoExchange := true;
        //They both will exchange next tick
        Result := true; //Means exit DoUnitInteraction
      end;
      //Now try to force the unit to exchange IF they are in the waiting phase
      if TUnitActionWalkTo(fOpponent.GetUnitAction).fInteractionStatus = kis_Waiting then
      begin
        //Because we are forcing this exchange we must inject into the other unit's nodelist by passing our current position
        TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(fWalker.GetPosition);

        Explanation:='Unit in the way is in waiting phase. Forcing an exchange';
        ExplanationLogAdd;
        DoExchange := true;
        //They both will exchange next tick
        Result := true; //Means exit DoUnitInteraction
      end;
    end;
  end;
end;


function TUnitActionWalkTo.IntCheckIfPushed(HighestInteractionCount:integer):boolean;
begin
  Result := false;
  //If we were asked to move away then all we are allowed to do is push and exchanging, no re-routing, dodging etc. so we must exit here before any more tests
  if fInteractionStatus = kis_Pushed then
  begin
    //If we've been trying to get out of the way for a while but we haven't found a solution, (i.e. other unit is stuck) try a different direction
    if HighestInteractionCount >= PUSHED_TIMEOUT then
    begin
      //Try getting out of the way again. After this is run our object no longer exists, so we must exit everything immediately

      fInteractionStatus := kis_None;

      if not CanAbandonInternal then begin
        fGame.GameError(fWalker.GetPosition, 'Unit walk IntCheckIfPushed');
        exit;
      end;

      fWalker.SetActionWalkToSpot(fTerrain.GetOutOfTheWay(fWalker.GetPosition,KMPoint(0,0),GetEffectivePassability));
      //todo 1: replace with regard to TargetUnit/TargetHouse!
      { kis_Pushed is given only to Idling units, so ... }
      Result := true; //Means exit DoUnitInteraction
      exit;
    end;
    inc(fInteractionCount);
    Explanation := 'We were pushed and are now waiting for a space to clear for us';
    ExplanationLogAdd;
    Result := true; //Means exit DoUnitInteraction
  end;
end;


function TUnitActionWalkTo.IntSolutionDodge(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
var i:shortint;
    TempPos: TKMPoint;
    fAltOpponent:TKMUnit;
begin
  //If there is a unit on one of the tiles either side of target that wants to swap, do so
  Result := false;
  if HighestInteractionCount >= DODGE_TIMEOUT then
  //UnitsHitTest (used twice here) is fairly CPU intensive, so don't run it every time
  if CheckInteractionFreq(HighestInteractionCount,DODGE_TIMEOUT,DODGE_FREQ) then
  begin
    //Tiles to the left (-1) and right (+1) (relative to unit) of the one we are walking to
    for i := -1 to 1 do
    if i <> 0 then
    begin
      TempPos := KMGetPointInDir(fWalker.GetPosition,KMLoopDirection(byte(KMGetDirection(fWalker.GetPosition,NodeList.List[NodePos+1]))+i));
      if fTerrain.TileInMapCoords(TempPos.X,TempPos.Y) and fTerrain.CanWalkDiagonaly(fWalker.GetPosition,TempPos)
        and (GetEffectivePassability in fTerrain.Land[TempPos.Y,TempPos.X].Passability) then //First make sure tile is on map and walkable!
      if fTerrain.HasUnit(TempPos) then //Now see if it has a unit
      begin
        //There is a unit here, first find our alternate opponent
        fAltOpponent := fPlayers.UnitsHitTest(TempPos.X, TempPos.Y);

        //Make sure unit really exists, is walking and has arrived on tile
        if (fAltOpponent <> nil) and (fAltOpponent.GetUnitAction is TUnitActionWalkTo) and
          (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).DoExchange)
          and (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).DoesWalking)
          and ((not KMStepIsDiag(fWalker.NextPosition,NodeList.List[NodePos+1])) //Isn't diagonal
          or ((KMStepIsDiag(fWalker.NextPosition,NodeList.List[NodePos+1])       //...or is diagonal and...
          and not fTerrain.HasVertexUnit(KMGetDiagVertex(fWalker.GetPosition,TempPos))))) then //...vertex is free
          if KMSamePoint(TUnitActionWalkTo(fAltOpponent.GetUnitAction).GetNextNextPosition, fWalker.GetPosition) then //Now see if they want to exchange with us
          begin
            //Perform exchange from our position to TempPos
            TUnitActionWalkTo(fAltOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange

            Explanation:='Unit on tile next to target tile wants to swap. Performing an exchange';
            ExplanationLogAdd;
            DoExchange := true;
            ChangeStepTo(TempPos);
            //They both will exchange next tick
            Result := true; //Means exit DoUnitInteraction
          end;
      end;
    end;
  end;
end;


function TUnitActionWalkTo.IntSolutionAvoid(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
begin
  Result := false;
  if (HighestInteractionCount >= AVOID_TIMEOUT) or fDestBlocked then
  //Route_MakeAvoid is very CPU intensive, so don't run it every time
  if CheckInteractionFreq(HighestInteractionCount,AVOID_TIMEOUT,AVOID_FREQ) then
  begin
    //If the blockage won't go away because it's busy (not walking) then try going around it by re-routing our route and avoiding that tile
    if not KMSamePoint(fOpponent.GetPosition,fWalkTo) then // Not the target position (can't go around if it is)
    if fDestBlocked or fOpponent.GetUnitAction.Locked
    then
      if fTerrain.Route_MakeAvoid(fWalker.GetPosition,fWalkTo,GetEffectivePassability,fWalkToSpot,NodeList) then //Make sure the route can be made, if not, we must simply wait
      begin
        //NodeList has now been re-routed, so we need to re-init everything else and start walk again
        SetInitValues;
        Explanation := 'Unit in the way is working so we will re-route around it';
        ExplanationLogAdd;
        fDestBlocked := false;
        //Exit, then on next tick new walk will start
        Result := true; //Means exit DoUnitInteraction
      end
      else
      begin
        fDestBlocked := true; //When in this mode we are zero priority as we cannot reach our destination. This allows serfs with stone to get through and clear our path.
        fInteractionStatus := kis_Waiting; //If route cannot be made it means our destination is currently not available (workers in the way) So allow us to be pushed.
        Explanation := 'Our destination is blocked by busy units';
        ExplanationLogAdd;
      end;
  end;
end;


{This solution tries to find an unoccupied tile where unit can side-step}
function TUnitActionWalkTo.IntSolutionSideStep(aPosition:TKMPoint; HighestInteractionCount:integer):boolean;
var SideStepTest:TKMPoint;
begin
  Result := false;
  if (HighestInteractionCount < SIDESTEP_TIMEOUT) or DoExchange then exit;
  if KMSamePoint(aPosition, fWalkTo) then exit; //Someone stays right on target, no point in side-stepping
  if not CheckInteractionFreq(HighestInteractionCount, SIDESTEP_TIMEOUT, SIDESTEP_FREQ) then exit; //FindSideStepPosition is CPU intensive, so don't run it every time

  //Find a node
  if NodePos+2 > NodeList.Count then //Tell Terrain about our next position if we can
    SideStepTest := fTerrain.FindSideStepPosition(fWalker.GetPosition,aPosition, KMPoint(0,0), NodePos-fLastSideStepNodePos < 2)
  else
    SideStepTest := fTerrain.FindSideStepPosition(fWalker.GetPosition,aPosition, NodeList.List[NodePos+2], NodePos-fLastSideStepNodePos < 2);

  if KMSamePoint(SideStepTest, KMPoint(0,0)) then exit; //It could be 0,0 if all tiles were blocked

  //Someone took it, so exit out and hope for better luck next time
  if fTerrain.HasUnit(SideStepTest) then
    Result := true //Means exit DoUnitInteraction without change
  else
  begin
    //Modify our route to go via this tile
    Explanation := 'Sidestepping to a tile next to target';
    ExplanationLogAdd;
    ChangeStepTo(SideStepTest);
    fLastSideStepNodePos := NodePos;
    Result := true; //Means exit DoUnitInteraction
  end;
end;


//States whether we are allowed to run time consuming tests
function TUnitActionWalkTo.CheckInteractionFreq(aIntCount,aTimeout,aFreq:integer):boolean;
begin
  Result := ((aIntCount - aTimeout) mod aFreq = 0) and (aIntCount - aTimeout >= 0);
end;


function TUnitActionWalkTo.DoUnitInteraction():boolean;
var
  fOpponent:TKMUnit;
  HighestInteractionCount: integer;
begin
  Result:=true; //false = interaction yet unsolved, stay and wait.
  if not DO_UNIT_INTERACTION then exit;

  //If there's a unit using this vertex to walk diagonally then we must wait, they will be finished after this step
  if KMStepIsDiag(fWalker.GetPosition,NodeList.List[NodePos+1]) and
    fTerrain.HasVertexUnit(KMGetDiagVertex(fWalker.GetPosition,NodeList.List[NodePos+1])) then
  begin
    Explanation := 'Diagonal vertex is being used, we must wait';
    ExplanationLogAdd;
    Result := false;
    exit;
  end;

  //If there's no unit we can keep on walking, interaction does not need to be solved
  if not fTerrain.HasUnit(NodeList.List[NodePos+1]) then exit;
  //From now on there is a blockage, so don't allow to walk unless the problem is resolved
  Result := false;

  //Find the unit that is in our path
  fOpponent := fPlayers.UnitsHitTest(NodeList.List[NodePos+1].X, NodeList.List[NodePos+1].Y);
  //If there's currently no unit in the way but tile is pre-occupied
  if fOpponent = nil then begin
    //Do nothing and wait till unit is actually there so we can interact with it
    Explanation:='Can''t walk. No Unit in the way but tile is occupied';
    ExplanationLogAdd;
    exit;
  end;

  //If we are in DestBlocked mode then only use our counter so we are always zero priority until our path clears
  if ((fOpponent.GetUnitAction is TUnitActionWalkTo) and not fDestBlocked) then
    HighestInteractionCount := max(fInteractionCount,TUnitActionWalkTo(fOpponent.GetUnitAction).fInteractionCount)
  else HighestInteractionCount := fInteractionCount;

  //Animals are low priority compared to other units, unless they are stuck (surrounded by units)
  if (fWalker.GetUnitType in [ut_Wolf..ut_Duck])
    and not fTerrain.CheckAnimalIsStuck(fWalker.GetPosition,fPass) then
  begin
    Explanation:='Unit is animal and therefore has no priority in movement';
    ExplanationLogAdd;
    exit;
  end;

  if (fOpponent.GetUnitAction is TUnitActionGoInOut) then begin //Unit is walking into house, we can wait
    Explanation:='Unit is walking into house, we can wait';
    ExplanationLogAdd;
    exit;
  end;

  if fDestBlocked then fInteractionStatus := kis_Waiting;

  //INTERACTION SOLUTIONS: Split into different sections or "solutions". If true returned it means exit.

  //If we are asking someone to move away then just wait until they are gone
  if IntCheckIfPushing(fOpponent) then exit;
  if IntSolutionPush(fOpponent,HighestInteractionCount) then exit;
  if IntSolutionExchange(fOpponent,HighestInteractionCount) then exit;
  if IntCheckIfPushed(fInteractionCount) then exit;
  if not fDestBlocked then fInteractionStatus := kis_Trying; //If we reach this point then we don't have a solution...
  if IntSolutionDodge(fOpponent,HighestInteractionCount) then exit;
  if IntSolutionAvoid(fOpponent,fInteractionCount) then exit;
  if IntSolutionSideStep(fOpponent.GetPosition,fInteractionCount) then exit;

  //We will allow other units to force an exchange with us as we haven't found a solution or our destination is blocked
  if (fInteractionCount >= WAITING_TIMEOUT) or fDestBlocked then fInteractionStatus := kis_Waiting;

  //If we haven't exited yet we must increment the counters so we know how long we've been here
  inc(fInteractionCount);
end;


function TUnitActionWalkTo.GetNextNextPosition():TKMPoint;
begin
  if NodePos+1 > NodeList.Count then
    Result:=KMPoint(0,0) //Error
  else Result:=NodeList.List[NodePos+1];
end;


//Modify route to go to this destination instead. Kind of like starting the walk over again but without recreating the action
procedure TUnitActionWalkTo.ChangeWalkTo(aLoc:TKMPoint; const aWalkToNear:boolean=false; aNewTargetUnit:TKMUnit=nil);
begin
  if fWalkTo.X*fWalkTo.Y = 0 then
   fGame.GameError(fWalkTo, 'Change Walk To 0:0');

  if aWalkToNear then
    fNewWalkTo := fTerrain.GetClosestTile(aLoc, fWalker.GetPosition, fPass)
  else
    fNewWalkTo := aLoc;

  //Change target if we need to
  if fTargetUnit <> nil then begin
    fTargetUnit.ReleaseUnitPointer;
    fTargetUnit := nil;
  end;
  if aNewTargetUnit <> nil then
    fTargetUnit := aNewTargetUnit.GetUnitPointer; //Change target
end;


function TUnitActionWalkTo.GetEffectivePassability:TPassability; //Returns passability that unit is allowed to walk on
begin
  //Road walking is only recomended. (i.e. for route building) We are allowed to step off the road sometimes.
  if fPass = canWalkRoad then
    Result := canWalk
  else
    Result := fPass;
end;


function TUnitActionWalkTo.Execute(KMUnit: TKMUnit):TActionResult;
var
  DX,DY:shortint;
  WalkX,WalkY,Distance:single;
  AllowToWalk:boolean;
begin
  Result := ActContinues;
  StepDone := false;
  DoesWalking := false; //Set it to false at start of update

  //Happens whe e.g. Serf stays in front of Store and gets Deliver task
  if KMSamePoint(fWalkFrom,fWalkTo) then begin
    Result := ActDone;
    exit;
  end;

  //Route was not built
  if NodeList.Count = 0 then begin
    Result := ActAborted;
    exit;
  end;

  //Walk complete - NodePos cannot be greater than NodeCount (this should not happen, cause is unknown but for now this check stops crashes)
  if NodePos > NodeList.Count then begin
    if KMStepIsDiag(fWalker.PrevPosition,fWalker.NextPosition) then DecVertex; //Unoccupy vertex
    fWalker.IsExchanging := false; //Disable sliding (in case it was set in previous step)
    Result := ActDone;
    exit;
  end;

  //Execute the route in series of moves
  Distance := ACTION_TIME_DELTA * fWalker.GetSpeed;

  //Check if unit has arrived on tile
  if KMSamePointF(fWalker.PositionF, KMPointF(NodeList.List[NodePos]), Distance/2) then
  begin

    //Set precise position to avoid rounding errors
    fWalker.PositionF := KMPointF(NodeList.List[NodePos]);

    if (NodePos > 1) and (not WaitingOnStep) and KMStepIsDiag(NodeList.List[NodePos-1],NodeList.List[NodePos]) then
      DecVertex; //Unoccupy vertex

    WaitingOnStep := true;

    StepDone := true; //Unit stepped on a new tile
    fWalker.IsExchanging := false; //Disable sliding (in case it was set in previous step)


    { Update destination point }

    //Make changes to our route if we are supposed to be tracking a unit
    if CanAbandonInternal and (fTargetUnit <> nil) and (not fTargetUnit.IsDeadOrDying) and not KMSamePoint(fTargetUnit.GetPosition,fWalkTo) then
    begin
      ChangeWalkTo(fTargetUnit.GetPosition,false,fTargetUnit); //If target unit has moved then change course and follow it (don't reset target unit)
      //If we are a warrior commander tell our memebers to use this new position
      if (fWalker is TKMUnitWarrior) and (TKMUnitWarrior(fWalker).fCommander = nil) then
        TKMUnitWarrior(fWalker).PlaceOrder(wo_Attack,fTargetUnit,true); //Give members new position
    end;

    //Check if we need to walk to a new destination
    if CanAbandonInternal and (CheckForNewDestination=dc_NoRoute) then begin
      Result := ActAborted;
      exit;
    end;

    //Check for units nearby to fight
    if CanAbandonInternal and (fWalker is TKMUnitWarrior) then
      if TKMUnitWarrior(fWalker).CheckForEnemy then
        //If we've picked a fight it means this action no longer exists,
        //so we must exit out (don't set DoEnd as that will now apply to fight action)
        exit;

    //Walk complete
    if not DoExchange then
    if (NodePos=NodeList.Count)
      or (round(KMLength(fWalker.GetPosition,fWalkTo)) <= fWalkToSpot)
      //todo: add proximity test for Houses, cos distance from the other side will be much smaller
      or ((fTargetHouse <> nil) and (KMLength(fWalker.GetPosition,fTargetHouse.GetPosition) < fWalkToSpot)) //
      or ((fTargetUnit <> nil) and (KMLength(fWalker.GetPosition,fTargetUnit.GetPosition) < 1.5)) //If we are walking to a unit check to see if we've met the unit early
      or ((fWalker.GetUnitTask <> nil) and fWalker.GetUnitTask.WalkShouldAbandon) //See if task wants us to abandon
    then begin
      if (fWalkToSpot>0) and ((fWalker.GetUnitTask = nil) or (not fWalker.GetUnitTask.WalkShouldAbandon)) then //Don't update direction if we are abandoning because of task request
        fWalker.Direction := KMGetDirection(NodeList.List[NodePos],fWalkTo); //Face tile (e.g. worker)
      Result := ActDone;
      exit;
    end;


    //Check if target unit (warrior) has died and if so abandon our walk and so delivery task can exit itself
    if CanAbandonInternal then
      case CheckTargetHasDied of
        tc_NoChanges, tc_TargetUpdated:;
        tc_Died: begin Result := ActAborted; exit; end;
      end;

    //This is sometimes caused by unit interaction changing the route so simply ignore it
    if KMSamePoint(NodeList.List[NodePos],NodeList.List[NodePos+1]) then
    begin
      inc(NodePos); //Inc the node pos and exit so this step is simply skipped
      exit; //Will take next step during next execute
    end;

    //If we were in Worker mode but have now reached the walk network of our destination switch to canWalk mode to avoid walking on other building sites
    if (fPass = canWorker) and (fTerrain.GetWalkConnectID(fWalkTo) <> 0) and
      (fTerrain.GetWalkConnectID(fWalkTo) = fTerrain.GetWalkConnectID(NodeList.List[NodePos])) then
      fPass := canWalk;

    //Update unit direction according to next Node
    fWalker.Direction := KMGetDirection(NodeList.List[NodePos],NodeList.List[NodePos+1]);

    //Check if we can walk to next tile in the route
    if CanAbandonInternal then
    case CheckForObstacle of
      oc_NoObstacle:;
      oc_ReRouteMade: exit; //New route will pick-up
      oc_NoRoute: begin Result := ActAborted; exit; end; //
    end;



    //Perform exchange
    //Both exchanging units have DoExchange:=true assigned by 1st unit, hence 2nd should not try doing UnitInteraction!
    if DoExchange then begin
      inc(NodePos);

      fWalker.UpdateNextPosition(NodeList.List[NodePos]);

      //We don't need to perform UnitWalk since exchange means the same tiles will be occupied,
      //and without UnitWalk there's a guarantee no other unit will step on this tile!
      fInteractionStatus := kis_None;
      DoExchange := false;
      fWalker.IsExchanging := true; //So unit knows that it must slide
      fInteractionCount := 0;
      if KMStepIsDiag(fWalker.PrevPosition,fWalker.NextPosition) then IncVertex; //Occupy the vertex
    end else
    begin
      AllowToWalk := DoUnitInteraction();

      if not AllowToWalk then
      begin
        if (KMUnit.GetUnitType in [ut_Wolf..ut_Duck]) and  //Animals have no tasks hence they can choose new WalkTo spot no problem, unless they are stuck
                  not fTerrain.CheckAnimalIsStuck(fWalker.GetPosition,fPass) then
                  Result := ActDone;
        exit; //Do no further walking until unit interaction is solved
      end else fInteractionCount := 0; //Reset the counter when there is no blockage and we can walk

      inc(NodePos);
      fWalker.UpdateNextPosition(NodeList.List[NodePos]);

      if GetLength(fWalker.PrevPosition,fWalker.NextPosition) > 1.5 then begin
        fGame.GameError(fWalker.PrevPosition, 'Unit walk length>1.5');
        exit;
      end;

      if fTerrain.Land[fWalker.PrevPosition.Y,fWalker.PrevPosition.X].IsUnit = 0 then begin
        fGame.GameError(fWalker.PrevPosition, 'Unit walk Prev position IsUnit = 0');
        exit;
      end;

      fTerrain.UnitWalk(fWalker.PrevPosition,fWalker.NextPosition); //Pre-occupy next tile
      if KMStepIsDiag(fWalker.PrevPosition,fWalker.NextPosition) then IncVertex; //Occupy the vertex
    end;

  end;
  WaitingOnStep := false;

  if NodePos>NodeList.Count then
    fGame.GameError(fWalker.GetPosition, 'WalkTo overrun');

  WalkX := NodeList.List[NodePos].X - fWalker.PositionF.X;
  WalkY := NodeList.List[NodePos].Y - fWalker.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fWalker.PositionF:=KMPointF(fWalker.PositionF.X + DX*min(Distance,abs(WalkX)),
                              fWalker.PositionF.Y + DY*min(Distance,abs(WalkY)));

  inc(fWalker.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
  DoesWalking:=true; //Now it's definitely true that unit did walked one step
end;


procedure TUnitActionWalkTo.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fWalker <> nil then
    SaveStream.Write(fWalker.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  if fTargetUnit <> nil then
    SaveStream.Write(fTargetUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fWalkFrom);
  SaveStream.Write(fWalkTo);
  SaveStream.Write(fVertexOccupied);
  SaveStream.Write(fNewWalkTo);
  SaveStream.Write(fWalkToSpot);
  SaveStream.Write(fDestBlocked);
  SaveStream.Write(fLastSideStepNodePos);
  SaveStream.Write(fPass,SizeOf(fPass));
  SaveStream.Write(DoesWalking);
  SaveStream.Write(DoExchange);
  SaveStream.Write(WaitingOnStep);
  SaveStream.Write(fInteractionCount);
  SaveStream.Write(fInteractionStatus,SizeOf(fInteractionStatus));
  NodeList.Save(SaveStream);
  SaveStream.Write(NodePos);
end;


end.
