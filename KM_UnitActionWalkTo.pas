unit KM_UnitActionWalkTo;
interface
uses Classes, KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Player, KM_Units, SysUtils, Math;

{Walk to somewhere}
type
  //@All: Most of these are not being used for anything, should be cleaned up later once main processes are working.
  TInteractionStatus = (kis_None,       //We are not involved in any interaction to our knowledge (we are just walking)
                        kis_Exchanging, //We are about to exchange positions with the other unit
                        kis_Pushing,    //We are pushing an idle unit out of the way
                        kis_Pushed,     //We were pushed (idle then asked to move)
                        kis_Trying,     //We are currently stuck and trying to find a solution
                        kis_Waiting     //We are waiting for the give up timeout. During this time other units may tell us to swap with them
  );

const
  TInteractionStatusNames: array[TInteractionStatus] of string = ('None', 'Exchanging', 'Pushing', 'Pushed', 'Trying', 'Waiting');

type
  TUnitActionWalkTo = class(TUnitAction)
    private
      fWalker, fLastOpponent:TKMUnit;
      fWalkFrom, fWalkTo, fAvoid:TKMPoint;
      fWalkToSpot:boolean;
      fPass:TPassability; //Desired passability set once on Create
      DoesWalking:boolean;
      DoExchange:boolean; //Command to make exchange maneuver with other unit.
      fInteractionCount, fGiveUpCount: integer;
      fInteractionStatus: TInteractionStatus;
      function AssembleTheRoute():boolean;
      function CheckCanWalk():boolean;
      function DoUnitInteraction():boolean;
    public
      NodeList:TKMPointList;
      NodePos:integer;
      fRouteBuilt:boolean;
      Explanation:string; //Debug only, explanation what unit is doing
      constructor Create(KMUnit: TKMUnit; LocB,Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true);
      constructor Load(LoadStream: TKMemoryStream);
      destructor Destroy; override;
      function GetNextPosition():TKMPoint;
      function GetNextNextPosition():TKMPoint;
      property GetInteractionStatus:TInteractionStatus read fInteractionStatus;
      procedure PerformExchange(ForcedExchangePos:TKMPoint);
      procedure DodgeTo(aPos: TKMPoint);
      procedure SetPushedValues;
      procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;
            

implementation
uses KM_Houses, KM_Game, KM_PlayersCollection, KM_Terrain, KM_Viewport, KM_UnitActionGoInOut, KM_UnitActionStay;


{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create(KMUnit: TKMUnit; LocB, Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true);
begin
  fLog.AssertToLog(LocB.X*LocB.Y<>0,'Illegal WalkTo 0;0');

  Inherited Create(aActionType);
  fActionName := uan_WalkTo;
  fWalker       := KMUnit;
  fWalkFrom     := fWalker.GetPosition;
  fWalkTo       := LocB;
  fAvoid        := Avoid;
  fWalkToSpot   := aWalkToSpot;
  fPass         := fWalker.GetDesiredPassability;

  NodeList      := TKMPointList.Create; //Freed on destroy
  NodePos       := 1;
  fRouteBuilt   := false;
  DoExchange    := false;
  DoesWalking   := false;
  fInteractionCount := 0;
  fGiveUpCount := 0;
  fInteractionStatus := kis_None;
  fLastOpponent := nil;

  if KMSamePoint(fWalkFrom,fWalkTo) then //We don't care for this case, Execute will report action is done immediately
    exit; //so we don't need to perform any more processing

  fRouteBuilt := AssembleTheRoute();
end;


constructor TUnitActionWalkTo.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fWalker, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fLastOpponent, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fWalkFrom,4);
  LoadStream.Read(fWalkTo,4);
  LoadStream.Read(fAvoid,4);
  LoadStream.Read(fWalkToSpot);
  LoadStream.Read(fPass,SizeOf(fPass));
  LoadStream.Read(DoesWalking);
  LoadStream.Read(DoExchange);
  LoadStream.Read(fInteractionCount);
  LoadStream.Read(fGiveUpCount);
  LoadStream.Read(fInteractionStatus,SizeOf(fInteractionStatus));

  NodeList := TKMPointList.Create; //Freed on destroy
  NodeList.Load(LoadStream);
  LoadStream.Read(NodePos);
  LoadStream.Read(fRouteBuilt);
//  LoadStream.Read(Explanation, length(Explanation));
end;


destructor TUnitActionWalkTo.Destroy;
begin
  FreeAndNil(NodeList);
  Inherited;
end;


procedure TUnitActionWalkTo.SetPushedValues;
begin //This is run after creation for units that were requested to get out of the way
  fInteractionStatus:=kis_Pushed; //So that unit knows it was pushed not just walking somewhere
  Explanation:='We were asked to get out of the way';
  fPass:=canWalk; //Units are allowed to step off roads when they are pushed
  //Because the passability has changed we might need to reassemble the route if it failed in create
  if not fRouteBuilt then fRouteBuilt := AssembleTheRoute();
end;


procedure TUnitActionWalkTo.PerformExchange(ForcedExchangePos:TKMPoint);
begin
  Explanation:='We were asked to exchange places';
  fInteractionStatus := kis_Exchanging;
  DoExchange := true;

  //If we are being forced to exchange then modify our route to make the exchange, then return the tile we are currently on, then continue the route
  if not KMSamePoint(ForcedExchangePos,KMPoint(0,0)) then
  begin
    Explanation:='We were forced to exchange places';
    if KMLength(ForcedExchangePos,NodeList.List[NodePos+1]) >= 1.5 then
      NodeList.InjectEntry(NodePos+1,fWalker.GetPosition); //We must back-track if we cannot continue our route from the new tile
    NodeList.InjectEntry(NodePos+1,ForcedExchangePos);
    if KMSamePoint(fWalker.GetPosition,ForcedExchangePos) then fLog.AssertToLog(false,'Perform Exchange to same place?');
    fWalker.Direction := KMGetDirection(fWalker.GetPosition,ForcedExchangePos);
    DoesWalking:=true;
  end;
end;


procedure TUnitActionWalkTo.DodgeTo(aPos: TKMPoint);
begin
  if not KMSamePoint(NodeList.List[NodeList.Count],NodeList.List[NodeList.Count-1]) then
   NodePos := NodePos;


  if (NodePos+2 <= NodeList.Count) and (KMLength(aPos,NodeList.List[NodePos+2]) < 1.5) then
  begin
    NodeList.List[NodePos+1] := aPos; //We can simply replace the entry because it is near the next tile
    if KMSamePoint(aPos,NodeList.List[NodePos+2]) then //If we are actually walking to the tile AFTER this one...
      NodeList.RemoveEntry(aPos); //Remove it so we don't get Walk to same spot error.
      //@Krom: This will be done better later on by running a procedure to "optimise" the route after we modify it, which will remove duplicate
      //       entries like this from anywhere after NodePos. Will probably do some other stuff too, (maybe taking shortcuts if it will work) I still need to think about it.
  end
  else //Otherwise we must inject it
    NodeList.InjectEntry(NodePos+1,aPos);
  fWalker.Direction := KMGetDirection(fWalker.GetPosition,aPos); //Face the new tile
end;


function TUnitActionWalkTo.AssembleTheRoute():boolean;
var i:integer; NodeList2:TKMPointList;
begin
  //Build a piece of route to return to nearest road piece connected to destination road network
  if (fPass = canWalkRoad) and (fWalkToSpot) then //That is Citizens walking to spot
    if fTerrain.GetRoadConnectID(fWalkFrom) <> fTerrain.GetRoadConnectID(fWalkTo) then //NoRoad returns 0
      fTerrain.Route_ReturnToRoad(fWalkFrom, fTerrain.GetRoadConnectID(fWalkTo), NodeList);

  //Build a route A*
  if NodeList.Count=0 then //Build a route from scratch
    fTerrain.Route_Make(fWalkFrom, fWalkTo, fAvoid, fPass, fWalkToSpot, NodeList) //Try to make the route with fPass
  else begin //Append route to existing part
    NodeList2 := TKMPointList.Create;
    fTerrain.Route_Make(NodeList.List[NodeList.Count], fWalkTo, fAvoid, fPass, fWalkToSpot, NodeList2); //Try to make the route with fPass
    for i:=2 to NodeList2.Count do
      NodeList.AddEntry(NodeList2.List[i]);
    FreeAndNil(NodeList2);
  end;
  
  Result := NodeList.Count > 0;

  if not Result then //If route still unbuilt..
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with default fPass');

  {if not Result then begin //Build a route with canWalk
    fTerrain.Route_Make(fWalkFrom, fWalkTo, fAvoid, canWalk, fWalkToSpot, NodeList); //Try to make a route
    Result := NodeList.Count>0;
  end;

  if not Result then
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with canWalk');}
end;


function TUnitActionWalkTo.CheckCanWalk():boolean;
  function GetOurPassability:TPassability;
  begin //Needed mainly because of routes that start off-road but also some interaction solutions may force units off roads
    if fPass = canWalkRoad then Result := canWalk
    else Result := fPass;
  end;
begin
  Result := true;
  //If there's an unexpected obstacle (i.e. the terrain has changed since we calculated the route)
  //Use GetOurPassability so that canWalkRoad is changed to canWalk because walking off the road does not count as an obstacle
  if not fTerrain.CheckPassability(NodeList.List[NodePos+1],GetOurPassability) then
    //Try to find a walkaround
    if fTerrain.Route_CanBeMade(fWalker.GetPosition,fWalkTo,GetOurPassability,fWalkToSpot) then
    begin
      fWalker.SetActionWalk(fWalker,fWalkTo,KMPoint(0,0),GetActionType,fWalkToSpot);
      Result:=false;
    end
    else
    begin
      if fWalker.GetUnitTask <> nil then fWalker.GetUnitTask.Abandon; //Else stop and abandon the task (if we have one)
      Result:=false;
    end;
end;


function TUnitActionWalkTo.DoUnitInteraction():boolean;
var
  fOpponent, fAltOpponent:TKMUnit; TempInt, i: integer; TempPos: TKMPoint;
const //Times after which various things will happen. Will need to be tweaked later for optimal performance.
  EXCHANGE_TIMEOUT = 0;
  PUSHED_EXCHANGE_TIMEOUT = 0; //When pushed only exchange if there is no way to push another unit. @All: Not needed, it just causes lock ups. Will be removed soon unless I find a reason not to
  PUSH_TIMEOUT = 1;
  PUSHED_TIMEOUT = 20;
  DODGE_TIMEOUT = 5;
  AVOID_TIMEOUT = 10;
  WAITING_TIMEOUT = 40;
  TOTAL_GIVEUP_TIMEOUT = 200;
begin
  Result:=true; //false = interaction yet unsolved, stay and wait. @All: Always seems to be set to false unless first checks fail. Needs clean up.
  if not DO_UNIT_INTERACTION then exit;

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
    Result:=false;
    exit;
  end;

  //Animals are low priority compared to other units
  if (fWalker.GetUnitType in [ut_Wolf..ut_Duck])and(not(fOpponent.GetUnitType in [ut_Wolf..ut_Duck])) then begin
    Explanation:='Unit is animal and therefore has no priority in movement';
    Result:=false;
    exit;
  end;

  if (fOpponent.GetUnitAction is TUnitActionGoInOut) then begin //Unit is walking into house, we can wait
    Explanation:='Unit is walking into house, we can wait';
    Result:=false;
    exit;
  end;

  //If we are asking someone to move away then just wait until they are gone
  if (fInteractionStatus = kis_Pushing) then
    if (fLastOpponent = fOpponent) and //Make sure they are still the same unit and are still moving out of the way
      (fOpponent.GetUnitAction is TUnitActionWalkTo) then
    begin
      Explanation := 'Unit is blocking the way and has been asked to move';
      Result := false;
      exit;
    end
    else
    begin //We pushed a unit out of the way but someone else took it's place! Now we must start over to solve problem with this new opponent
      fInteractionCount := 0;
      fGiveUpCount := 0;
      fInteractionStatus := kis_Trying;
      Explanation := 'Someone took the pushed unit''s place';
    end;


  ////////////////////////////////     MAIN SECTION      //////////////////////////////////////////
  //Try various things depending on how many times we've been here before
  if fInteractionCount >= PUSH_TIMEOUT then
  begin
    //Ask the other unit to step aside, only if they are idle!
    if (fOpponent.GetUnitAction is TUnitActionStay) and (fOpponent.GetUnitActionType = ua_Walk)
      and (not TUnitActionStay(fOpponent.GetUnitAction).Locked) then //Unit is idle, (not working or something) and not locked
    begin //Force Unit to go away
      fInteractionStatus := kis_Pushing;
      fOpponent.SetActionWalk(fOpponent, fTerrain.GetOutOfTheWay(fOpponent.GetPosition,fWalker.GetPosition,canWalk));
      TUnitActionWalkTo(fOpponent.GetUnitAction).SetPushedValues;
      fLastOpponent := fOpponent; //So we know who we are pushing later in case something changes
      Explanation := 'Unit was blocking the way but it has been forced to go away now';
      Result := false; //Next frame tile will be free and unit will walk there
      exit;
    end;
  end;

  if ((fInteractionCount >= EXCHANGE_TIMEOUT) and (fInteractionStatus <> kis_Pushed)) or //When pushed this timeout/counter is different
     ((fGiveUpCount >= PUSHED_EXCHANGE_TIMEOUT) and (fInteractionStatus = kis_Pushed)) then
  begin //Try to exchange with the other unit if they are willing
    //If Unit on the way is walking somewhere and not exchanging with someone else
    if (fOpponent.GetUnitAction is TUnitActionWalkTo) and (not TUnitActionWalkTo(fOpponent.GetUnitAction).DoExchange)
      //Unit not yet arrived on tile, wait till it does, otherwise there might be 2 units on one tile
      and (not TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking) then
    begin
      //Check unit's future position is where we are now and exchange (use NodeList rather than direction as it's not always right)
      if KMSamePoint(TUnitActionWalkTo(fOpponent.GetUnitAction).GetNextNextPosition, fWalker.GetPosition) then
      begin
        //Graphically both units are walking side-by-side, but logically they simply walk through each-other.
        //TODO: Make units slide sideways while passing
        TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange
        fLastOpponent := fOpponent; //So we know who we are supposed to be exchanging with later

        Explanation:='Unit in the way is walking in the opposite direction. Performing an exchange';
        fInteractionStatus := kis_Exchanging; //Unnecessary?
        DoExchange := true;
        Result:=false; //They both will exchange next tick
        exit;
      end;
      //Now try to force the unit to exchange IF they are in the waiting phase
      if TUnitActionWalkTo(fOpponent.GetUnitAction).fInteractionStatus = kis_Waiting then
      begin
        //Because we are forcing this exchange we must inject into the other unit's nodelist by passing our current position
        TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(fWalker.GetPosition);
        fLastOpponent := fOpponent;

        Explanation:='Unit in the way is in waiting phase. Forcing an exchange';
        fInteractionStatus := kis_Exchanging; //Unnecessary?
        DoExchange := true;
        Result:=false; //They both will exchange next tick
        exit;
      end;
    end;
  end;

  //If we were asked to move away then all we are allowed to do is push and exchanging, no re-routing, dodging etc. so we must exit here before any more tests
  if fInteractionStatus = kis_Pushed then
  begin
    //If we've been trying to get out of the way for a while but we haven't found a solution, (i.e. other unit is stuck) try a different direction
    if fInteractionCount >= PUSHED_TIMEOUT then
    begin
      TempInt := fGiveUpCount; //Carry over the give up counter
      Create(fWalker,fTerrain.GetOutOfTheWay(fWalker.GetPosition,fWalker.GetPosition,canWalk),fAvoid,GetActionType,fWalkToSpot);
      SetPushedValues;
      fGiveUpCount := TempInt;
      Result := false;
      exit;
    end;
    inc(fInteractionCount);
    inc(fGiveUpCount);
    Explanation := 'We were pushed and are now waiting for a space to clear for us';
    Result := false;
    exit;
  end;

  if fInteractionCount >= DODGE_TIMEOUT then
  begin
    //If there is a unit on one of the tiles either side of target that wants to swap, do so
    fInteractionStatus := kis_Trying;

    //Tiles to the left (-1) and right (+1) (relative to unit) of the one we are walking to
    for i := -1 to 1 do
    if i <> 0 then
    begin
    TempPos := KMGetPointInDir(fWalker.GetPosition,KMLoopDirection(byte(KMGetDirection(fWalker.GetPosition,NodeList.List[NodePos+1]))+i));
    if fTerrain.TileInMapCoords(TempPos.X,TempPos.Y) and fTerrain.CanWalkDiagonaly(fWalker.GetPosition,TempPos)
      and (fPass in fTerrain.Land[TempPos.Y,TempPos.X].Passability) then //First make sure tile is on map and walkable!
    if fTerrain.HasUnit(TempPos) then //Now see if it has a unit
    begin
      //There is a unit here, first find our alternate opponent
      fAltOpponent := fPlayers.UnitsHitTest(TempPos.X, TempPos.Y);
      
      //Make sure unit really exists, is walking and has arrived on tile
      if (fAltOpponent <> nil) and (fAltOpponent.GetUnitAction is TUnitActionWalkTo) and 
        (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).DoExchange)
        and (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).DoesWalking) then
      if KMSamePoint(TUnitActionWalkTo(fAltOpponent.GetUnitAction).GetNextNextPosition, fWalker.GetPosition) then //Now see if they want to exchange with us
      begin
        //Perform exchange from our position to TempPos
        TUnitActionWalkTo(fAltOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange
        fLastOpponent := fAltOpponent; //So we know who we are supposed to be exchanging with later

        Explanation:='Unit on tile next to target tile wants to swap. Performing an exchange';
        fInteractionStatus := kis_Exchanging; //Unnecessary?
        DoExchange := true;
        DodgeTo(TempPos);
        Result:=false; //They both will exchange next tick
        exit;
      end;
    end;
    end;
  end;

  if fInteractionCount >= WAITING_TIMEOUT then
  begin
    //We are waiting for the give up timeout to happpen OR another unit to tell us to swap with them
    fInteractionStatus := kis_Waiting;
  end;

  //TO BE ADDED:
  if fInteractionCount >= AVOID_TIMEOUT then
  begin
    {//If the blockage won't go away because it's busy (not walking) then try going around it by re-routing our route and avoiding that tile
    if not (fOpponent.GetUnitAction is TUnitActionWalkTo) then
    if not KMSamePoint(fOpponent.GetPosition,fWalkTo) then // Not the target position (can't go around if it is)
      begin
        //Start walking around
        Explanation := 'Unit in the way is working so we will go around it';
        fWalker.SetActionWalk(fWalker,fWalkTo,NodeList.List[NodePos+1],GetActionType,fWalkToSpot);
        Result := false; //Keep 'false' for the matter of Execute cycle still running
        exit;
      end;} //@Lewin: it has a bug, please look into it
  end;


  if fGiveUpCount >= TOTAL_GIVEUP_TIMEOUT then
  begin
    //Give up and re-route without using tile that is currently blocking us (if possible)
  end;

  //If we haven't exited yet we must increment the counters so we know how long we've been here
  inc(fInteractionCount);
  inc(fGiveUpCount);










  ///////////////////// SOME OF THE OLD CODE TO BE COPIED FROM /////////////////////



  {
  if TUnitActionWalkTo(fOpponent.GetUnitAction).DoExchange then begin
    //Don't mess with DoExchange opponents
    Explanation:='DoEv';
    Result:=false;
    exit;
  end;

  //
  //Now we should solve unexpected obstacles situations
  //------------------------------------------------------------------------------------------------
  //Abort action and task if we can't go around the obstacle

  //
  //Now we should solve fights
  //------------------------------------------------------------------------------------------------

  if fWalker is TKMUnitWarrior then
  if fWalker.GetOwner<>fOpponent.GetOwner then
  begin
//    fWalker.SetActionFight
  end;

  //
  //Now we try to solve different Unit-Unit situations
  //------------------------------------------------------------------------------------------------

  //If Unit on the way is staying
  if (fOpponent.GetUnitAction is TUnitActionStay) then
  begin
    if fOpponent.GetUnitActionType = ua_Walk then //Unit stays idle, not working or something
    begin //Force Unit to go away
      //fOpponent.SetActionWalk(fOpponent, fTerrain.GetOutOfTheWay(fOpponent.GetPosition,fWalker.GetPosition,canWalk));
      fOpponent.SetActionWalk(fOpponent, fWalker.GetPosition); //Thats the fastest way to solve complex crowds
      //TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking:=true;
      Explanation := 'Unit was blocking the way but it''s forced to get away now';
      TUnitActionWalkTo(fOpponent.GetUnitAction).Explanation:='We were asked to walk away';
      Result := false; //Next frame tile will be free and unit will walk there
      exit;
    end else
    begin //If Unit on the way is doing something and won't move away
      if KMSamePoint(fOpponent.GetPosition,fWalkTo) then // Target position is occupied by another unit
      begin
        Explanation := 'Target position is occupied by another Unit, can''t do nothing about it but wait. '+inttostr(random(123));
        Result := false;
        exit;
      end else
      begin //This is not our target, so we can walk around
        {StartWalkingAround}
        {Explanation := 'Unit on the way is doing something and can''t be forced to go away';
        fWalker.SetActionWalk(fWalker,fWalkTo,NodeList.List[NodePos+1],GetActionType,fWalkToSpot);
        Result := false; //Keep 'false' for the matter of Execute cycle still running
        exit;
      end;
    end;
  end;

  //If Unit on the way is walking somewhere
  if (fOpponent.GetUnitAction is TUnitActionWalkTo) then
  begin //Unit is walking
    //Check unit direction to be opposite and exchange, but only if the unit is staying on tile, not walking
    if (min(byte(fOpponent.Direction),byte(fWalker.Direction))+4 = max(byte(fOpponent.Direction),byte(fWalker.Direction))) then
    begin
      if TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking then
      begin //Unit yet not arrived on tile, wait till it does, otherwise there might be 2 units on one tile
        Explanation:='Unit on the way is walking '+TypeToString(fOpponent.Direction)+'. Waiting till it walks into spot and then exchange';
        Result:=false;
        exit;
      end else
      begin
        {//Graphically both units are walking side-by-side, but logically they simply walk through each-other.
        Explanation:='Unit on the way is walking opposite direction. Performing an exchange';
        TUnitActionWalkTo(fOpponent.GetUnitAction).Explanation:='We were asked to exchange places';
        TUnitActionWalkTo(fWalker.GetUnitAction).DoExchange:=true;
        TUnitActionWalkTo(fOpponent.GetUnitAction).DoExchange:=true;
        fLog.AddToLog(TypeToString(fWalker.GetPosition));
        fLog.AddToLog(TypeToString(fOpponent.GetPosition));
        //TUnitActionWalkTo(fWalker.GetUnitAction).DoesWalking:=true;
        }//TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking:=true;
        {Result:=false; //They both will exchange next tick
        exit;
      end;
    end else
    begin //Unit walking direction is not opposite
      if TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking then
      begin //Simply wait till it walks away
        Explanation:='Unit on the way is walking '+TypeToString(fOpponent.Direction)+'. Waiting till it walks away '+TypeToString(fWalker.Direction);
        Result:=false;
        exit;
      end else
      begin
        if KMSamePoint(fOpponent.GetPosition, fWalkTo) then
        begin
          //inject a random walkaround node
          Explanation:='Unit on the way is walking in place obstructing our target. Forcing it to go away.';

          {//Solution A
          //Walk through 7x50 group time  ~105sec
          T:=KMPoint(0,0);
          //This could be potential flaw, but so far it worked out allright
          if TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1 <= TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.Count then
          T := fTerrain.FindNewNode(
            fOpponent.GetPosition,
            TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.List[TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1],
            canWalk);
          if KMSamePoint(T,KMPoint(0,0)) then
          begin
            Explanation:='Unit on the way is walking in place obstructing our target. Can''t walk around. '+inttostr(random(123));
            Result := false;
            exit;
            // UNRESOLVED !
          end;
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking:=true;
          //}

          {//Solution B
          //Walk through 7x50 group time  ~143sec
          //Make opponent to exchange places with Walker
          T:=fOpponent.GetPosition;
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          T:=fWalker.GetPosition;
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking:=true;
          //}
          //In fact this is not very smart idea, cos fOpponent may recieve endless InjectEntries!
          {Result := false;
          exit;
        end else
        begin //Unit has WalkTo action, but is standing in place - don't mess with it and go around
          {Explanation:='Unit on the way is walking still. Go around it. '+inttostr(random(123));
          fWalker.SetActionWalk(fWalker,fWalkTo,fOpponent.GetPosition,GetActionType,fWalkToSpot);
          }{Result := false; //Keep 'false' for the matter of Execute cycle still running
          exit;
        end;
      end;
    end;
  end;}
end;


function TUnitActionWalkTo.GetNextPosition():TKMPoint;
begin
  if NodePos > NodeList.Count then
    Result:=KMPoint(0,0) //Error
  else Result:=NodeList.List[NodePos];
end;


function TUnitActionWalkTo.GetNextNextPosition():TKMPoint;
begin
  if NodePos+1 > NodeList.Count then
    Result:=KMPoint(0,0) //Error
  else Result:=NodeList.List[NodePos+1];
end;


procedure TUnitActionWalkTo.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX,DY:shortint; WalkX,WalkY,Distance:single; AllowToWalk:boolean;
begin
  DoEnd := false;
  GetIsStepDone := false;
  DoesWalking := false; //Set it to false at start of update

  //Happens whe e.g. Serf stays in front of Store and gets Deliver task
  if KMSamePoint(fWalkFrom,fWalkTo) then begin
    DoEnd := true;
    exit;
  end;

  //Somehow route was not built, this is an error
  if not fRouteBuilt then begin
    fLog.AddToLog('Unable to walk a route since it''s unbuilt');
    fViewport.SetCenter(fWalker.GetPosition.X,fWalker.GetPosition.Y);
    fGame.PauseGame(true);
    //Pop-up some message
  end;

  //Walk complete - NodePos cannot be greater than NodeCount (this should not happen, cause is unknown but for now this check stops crashes)
  if NodePos > NodeList.Count then begin
    DoEnd:=true;
    exit;
  end;

  //Execute the route in series of moves
  TimeDelta := 0.1;
  Distance := TimeDelta * fWalker.GetSpeed;

  //Check if unit has arrived on tile
  if Equals(fWalker.PositionF.X,NodeList.List[NodePos].X,Distance/2) and
     Equals(fWalker.PositionF.Y,NodeList.List[NodePos].Y,Distance/2) then
  begin

    GetIsStepDone := true; //Unit stepped on a new tile

    //Set precise position to avoid rounding errors
    fWalker.PositionF := KMPointF(NodeList.List[NodePos].X,NodeList.List[NodePos].Y);

    //Walk complete
    if NodePos=NodeList.Count then begin
      DoEnd:=true;
      exit;
    end;


    //Update unit direction according to next Node
    fWalker.Direction := KMGetDirection(NodeList.List[NodePos],NodeList.List[NodePos+1]);
    if KMSamePoint(NodeList.List[NodePos],NodeList.List[NodePos+1]) then
      fLog.AssertToLog(false,'Walk to same place?');

    //Check if we can walk to next tile in the route
    if not CheckCanWalk then exit; //

    //Perform exchange
    //Both exchanging units have DoExchange:=true assigned by 1st unit, hence 2nd should not try doing UnitInteraction!
    if DoExchange then begin
      inc(NodePos);
      fWalker.PrevPosition:=fWalker.NextPosition;
      fWalker.NextPosition:=NodeList.List[NodePos];
      //fLog.AddToLog('DoExchange');
      //We don't need to perform UnitWalk since exchange means the same tiles will be occupied,
      //and without UnitWalk there's a guarantee no other unit will step on this tile!
      fInteractionStatus := kis_None;
      DoExchange := false;
      fInteractionCount := 0;
    end else
    begin
      AllowToWalk := DoUnitInteraction();

      if not AllowToWalk then
      begin
        DoEnd := KMUnit.GetUnitType in [ut_Wolf..ut_Duck]; //Animals have no tasks hence they can choose new WalkTo spot no problem
        exit; //Do no further walking until unit interaction is solved
      end else fInteractionCount := 0; //Reset the counter when there is no blockage and we can walk
      if fInteractionStatus = kis_None then fGiveUpCount := 0; //Reset the give up counter if we are walking normally (not exchanging or dodging)

      inc(NodePos);
      fWalker.PrevPosition:=fWalker.NextPosition;
      fWalker.NextPosition:=NodeList.List[NodePos];
      fTerrain.UnitWalk(fWalker.PrevPosition,fWalker.NextPosition); //Pre-occupy next tile
    end;
  end;

  if NodePos>NodeList.Count then
    fLog.AssertToLog(false,'TUnitAction is being overrun for some reason - error!');

  WalkX := NodeList.List[NodePos].X - fWalker.PositionF.X;
  WalkY := NodeList.List[NodePos].Y - fWalker.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fWalker.PositionF:=KMPointF(fWalker.PositionF.X + DX*min(Distance,abs(WalkX)),
                              fWalker.PositionF.Y + DY*min(Distance,abs(WalkY)));

  inc(fWalker.AnimStep);
  DoesWalking:=true; //Now it's definitely true that unit did walked one step
end;


procedure TUnitActionWalkTo.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  if fWalker <> nil then
    SaveStream.Write(fWalker.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  if fLastOpponent <> nil then
    SaveStream.Write(fLastOpponent.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fWalkFrom,4);
  SaveStream.Write(fWalkTo,4);
  SaveStream.Write(fAvoid,4);
  SaveStream.Write(fWalkToSpot);
  SaveStream.Write(fPass,SizeOf(fPass));
  SaveStream.Write(DoesWalking);
  SaveStream.Write(DoExchange);
  SaveStream.Write(fInteractionCount);
  fGiveUpCount:=77777777;
  SaveStream.Write(fGiveUpCount);
  SaveStream.Write(fInteractionStatus,SizeOf(fInteractionStatus));
  NodeList.Save(SaveStream);
  SaveStream.Write(NodePos);
  SaveStream.Write(fRouteBuilt);
//  SaveStream.Write(Explanation, length(Explanation));
end;


end.
