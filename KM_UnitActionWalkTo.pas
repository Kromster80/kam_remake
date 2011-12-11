unit KM_UnitActionWalkTo;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KromUtils, KM_CommonTypes, KM_Houses, KM_Units, SysUtils, Math, KM_Points;

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
    fWalkFrom:TKMPoint; //Walking from this spot, used only in Create
    fWalkTo:TKMPoint; //Where are we going to
    fNewWalkTo:TKMPoint; //If we recieve a new TargetLoc it will be stored here
    fDistance:single; //How close we need to get to our aim
    fUseExactTarget: boolean; //If we don't care about exact position
    fTargetUnit:TKMUnit; //Folow this unit
    fTargetHouse:TKMHouse; //Go to this House
    fPass:TPassability; //Desired passability set once on Create
    fDoesWalking, fWaitingOnStep, fDestBlocked:boolean;
    fDoExchange:boolean; //Command to make exchange maneuver with other unit, should use MakeExchange when vertex use needs to be set
    fInteractionCount, fLastSideStepNodePos: integer;
    fInteractionStatus: TInteractionStatus;
    function AssembleTheRoute:boolean;
    function CheckForNewDestination: TDestinationCheck;
    function CheckTargetHasDied:TTargetDiedCheck;
    function CheckForObstacle:TObstacleCheck;
    function CheckWalkComplete:boolean;
    function CheckInteractionFreq(aIntCount,aTimeout,aFreq:integer):boolean;
    function DoUnitInteraction:boolean;
      //Sub functions split out of DoUnitInteraction (these are the solutions)
      function IntCheckIfPushing(fOpponent:TKMUnit):boolean;
      function IntSolutionPush(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
      function IntSolutionExchange(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
      function IntCheckIfPushed(HighestInteractionCount:integer):boolean;
      function IntSolutionDodge(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
      function IntSolutionAvoid(fOpponent: TKMUnit): Boolean;
      function IntSolutionSideStep(aPosition:TKMPoint; HighestInteractionCount:integer):boolean;

    procedure ChangeStepTo(aPos: TKMPoint);
    procedure PerformExchange(ForcedExchangePos:TKMPoint);
    procedure IncVertex;
    procedure DecVertex;
    procedure SetInitValues;
    function CanAbandonInternal: boolean;
    function GetNextNextPosition(out NextNextPos:TKMPoint):boolean;
    function GetEffectivePassability:TPassability; //Returns passability that unit is allowed to walk on
    procedure ExplanationLogCreate;
    procedure ExplanationLogAdd;
  private //Debug items
    NodePos:integer;
    NodeList:TKMPointList;
    Explanation:string; //Debug only, explanation what unit is doing
    ExplanationLog:TStringList;
  public
    fVertexOccupied: TKMPoint; //Public because it needs to be used by AbandonWalk
    constructor Create(aUnit: TKMUnit; aLocB:TKMPoint; aActionType:TUnitActionType; aDistance:single; aSetPushed:boolean; aTargetUnit: TKMUnit; aTargetHouse: TKMHouse; aUseExactTarget: boolean=true);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure  SyncLoad; override;
    destructor Destroy; override;

    function ActName: TUnitActionName; override;
    function CanAbandonExternal: boolean;
    property DoesWalking:boolean read fDoesWalking;
    property DoingExchange:boolean read fDoExchange; //Critical piece, must not be abandoned
    function GetExplanation:string; override;

    //Modify route to go to this destination instead
    procedure ChangeWalkTo(aLoc: TKMPoint; aDistance: Single; aUseExactTarget: Boolean = True); overload;
    procedure ChangeWalkTo(aNewTargetUnit: TKMUnit; aDistance: Single); overload;

    function Execute: TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
    procedure Paint; override; //Used only for debug so far
  end;


implementation
uses
  KM_RenderAux, KM_Game, KM_PlayersCollection, KM_Terrain, KM_UnitActionGoInOut, KM_UnitActionStay,
  KM_Units_Warrior, KM_UnitTaskMining, KM_Player, KM_Log, KM_ResourceGFX;


{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create( aUnit: TKMUnit;
                                      aLocB:TKMPoint;
                                      aActionType:TUnitActionType;
                                      aDistance:single;
                                      aSetPushed:boolean;
                                      aTargetUnit: TKMUnit;
                                      aTargetHouse: TKMHouse;
                                      aUseExactTarget: boolean=true);
var RouteBuilt:boolean; //Check if route was built, otherwise return nil
begin
  Inherited Create(aUnit, aActionType, False);
  if not fTerrain.TileInMapCoords(aLocB.X, aLocB.Y) then
    raise ELocError.Create('Invalid Walk To for '+fResource.UnitDat[aUnit.UnitType].UnitName,aLocB);

  fDistance     := aDistance;
  //               aSetPushed Dooesn't need to be rememberred (it is used only in Create here)

  if aTargetUnit  <> nil then fTargetUnit  := aTargetUnit.GetUnitPointer;
  if aTargetHouse <> nil then fTargetHouse := aTargetHouse.GetHousePointer;

  fWalkFrom     := fUnit.GetPosition;
  fNewWalkTo    := KMPoint(0,0);
  fPass         := fUnit.GetDesiredPassability;
  fUseExactTarget := aUseExactTarget;

  if fUseExactTarget then
    fWalkTo := aLocB
  else
    fWalkTo := fTerrain.GetClosestTile(aLocB, aUnit.GetPosition, fPass, aUseExactTarget);


  ExplanationLogCreate;
  Explanation := 'Walk action created';
  ExplanationLogAdd;

  if fWalkTo.X*fWalkTo.Y = 0 then
    raise ELocError.Create('WalkTo 0:0', fWalkTo);

  NodeList      := TKMPointList.Create; //Freed on destroy
  SetInitValues;

  if KMSamePoint(fWalkFrom,fWalkTo) then //We don't care for this case, Execute will report action is done immediately
    exit; //so we don't need to perform any more processing

  RouteBuilt := AssembleTheRoute;
  //Due to rare circumstances (e.g. floodfill doesn't take notice of CanWalkDiagonally i.e. trees on road corners)
  // there are times when trying to build a route along roads will fail.
  // To reduce crash errors, try rebuilding it with just CanWalk. This will normally fix the problem and
  // It's not a big deal if occasionally units walk off the road.
  if (not RouteBuilt) and (fPass = CanWalkRoad) then
  begin
    fPass := CanWalk;
    RouteBuilt := AssembleTheRoute;
  end;

  if aSetPushed then begin
    fInteractionStatus := kis_Pushed; //So that unit knows it was pushed not just walking somewhere
    Explanation := 'We were asked to get out of the way';
    ExplanationLogAdd;
    fPass := GetEffectivePassability; //Units are allowed to step off roads when they are pushed
    //Because the passability has changed we might need to reassemble the route if it failed in create
    if not RouteBuilt then RouteBuilt := AssembleTheRoute;
  end;

  if not RouteBuilt then
  begin
    fLog.AddToLog('Unable to make a route for '+fResource.UnitDat[aUnit.UnitType].UnitName+' from '+KM_Points.TypeToString(fWalkFrom)+' to '+KM_Points.TypeToString(fWalkTo)+' with default fPass');
    exit; //NoList.Count = 0, means it will exit in Execute
  end;
end;


procedure TUnitActionWalkTo.ExplanationLogCreate;
begin
  if not WRITE_WALKTO_LOG then Exit;

  ExplanationLog := TStringList.Create;
  if FileExists(ExeDir+'ExpLog'+inttostr(fUnit.ID)+'.txt') then
    ExplanationLog.LoadFromFile(ExeDir+'ExpLog'+inttostr(fUnit.ID)+'.txt');
end;


procedure TUnitActionWalkTo.ExplanationLogAdd;
begin
  if not WRITE_WALKTO_LOG then exit;
  ExplanationLog.Add(Format(
  '%d'+#9+'%d:%d > %d:%d > %d:%d'+#9+Explanation+'',
  [
  fGame.GameTickCount,
  fUnit.PrevPosition.X,
  fUnit.PrevPosition.Y,
  fUnit.GetPosition.X,
  fUnit.GetPosition.Y,
  fUnit.NextPosition.X,
  fUnit.NextPosition.Y
  ]))
end;


procedure TUnitActionWalkTo.SetInitValues;
begin
  NodePos       := 1;
  fDoExchange   := false;
  fDoesWalking   := false;
  fWaitingOnStep := false;
  fDestBlocked  := false;
  fLastSideStepNodePos := -2; //Start negitive so it is at least 2 less than NodePos at the start
  fVertexOccupied      := KMPoint(0,0);
  fInteractionCount    := 0;
  fInteractionStatus   := kis_None;
end;


constructor TUnitActionWalkTo.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fWalkFrom);
  LoadStream.Read(fWalkTo);
  LoadStream.Read(fNewWalkTo);
  LoadStream.Read(fDistance);
  LoadStream.Read(fUseExactTarget);
  LoadStream.Read(fTargetUnit, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fTargetHouse, 4); //substitute it with reference on SyncLoad
  LoadStream.Read(fPass, SizeOf(fPass));
  LoadStream.Read(fDoesWalking);
  LoadStream.Read(fWaitingOnStep);
  LoadStream.Read(fDestBlocked);
  LoadStream.Read(fDoExchange);
  LoadStream.Read(fInteractionCount);
  LoadStream.Read(fLastSideStepNodePos);
  LoadStream.Read(fInteractionStatus, SizeOf(fInteractionStatus));

  LoadStream.Read(fVertexOccupied);
  NodeList := TKMPointList.Create;
  NodeList.Load(LoadStream);
  LoadStream.Read(NodePos);
end;


procedure TUnitActionWalkTo.SyncLoad;
begin
  Inherited;
  fTargetUnit   := fPlayers.GetUnitByID(cardinal(fTargetUnit));
  fTargetHouse  := fPlayers.GetHouseByID(cardinal(fTargetHouse));
end;


destructor TUnitActionWalkTo.Destroy;
begin
  if fDoExchange and not fGame.IsExiting then
    Assert(not fDoExchange, 'Oops, thats a very bad situation');

  if WRITE_WALKTO_LOG then begin
    Explanation := 'WalkTo destroyed at'+floattostr(fUnit.PositionF.X)+':'+floattostr(fUnit.PositionF.Y);
    ExplanationLogAdd;
    ExplanationLog.SaveToFile(ExeDir+'ExpLog'+inttostr(fUnit.ID)+'.txt');
  end;

  FreeAndNil(NodeList);

  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
    DecVertex;

  fUnit.IsExchanging := false;

  fPlayers.CleanUpUnitPointer(fTargetUnit);
  fPlayers.CleanUpHousePointer(fTargetHouse);
  Inherited;
end;


function TUnitActionWalkTo.CanAbandonInternal: boolean;
begin
  Result := (fInteractionStatus <> kis_Pushed) //Can be removed, but decreases effectiveness
            and (not fDoExchange); //Other unit could have set this
end;


{ Returns true only when unit is stuck for some reason }
function TUnitActionWalkTo.CanAbandonExternal: boolean;
begin
  Result := (not fDoExchange) //Other unit could have set this
            and KMSamePointF(KMPointF(fUnit.GetPosition),fUnit.PositionF);
end;


function TUnitActionWalkTo.ActName: TUnitActionName;
begin
  Result := uan_WalkTo;
end;


function TUnitActionWalkTo.GetExplanation:string;
begin
  Result := TInteractionStatusNames[fInteractionStatus] + ': '+Explanation;
end;


procedure TUnitActionWalkTo.PerformExchange(ForcedExchangePos:TKMPoint);
begin
  //If we are being forced to exchange then modify our route to make the exchange,
  //  then return to the tile we are currently on, then continue the route
  if not KMSamePoint(ForcedExchangePos,KMPoint(0,0)) then
  begin
    Explanation := 'We were forced to exchange places';
    ExplanationLogAdd;
    fDoExchange := true;
    if KMLength(ForcedExchangePos,NodeList.List[NodePos+1]) >= 1.5 then
      NodeList.InjectEntry(NodePos+1, fUnit.GetPosition); //We must back-track if we cannot continue our route from the new tile
    NodeList.InjectEntry(NodePos+1, ForcedExchangePos);
    if KMSamePoint(fUnit.GetPosition, ForcedExchangePos) then
      raise ELocError.Create('Exchange to same place', fUnit.GetPosition);
    fUnit.Direction := KMGetDirection(fUnit.GetPosition, ForcedExchangePos);
    fDoesWalking := true;
  end
  else
  begin
    //Unforced exchanging
    Explanation:='We were asked to exchange places';
    ExplanationLogAdd;
    fDoExchange := true;
  end;
end;


//Used for dodging and side stepping
procedure TUnitActionWalkTo.ChangeStepTo(aPos: TKMPoint);
begin
  if (NodePos+2 <= NodeList.Count) and (KMLength(aPos,NodeList.List[NodePos+2]) < 1.5) then
    NodeList.List[NodePos+1] := aPos //We can simply replace the entry because it is near the next tile
  else //Otherwise we must inject it
    NodeList.InjectEntry(NodePos+1,aPos);

  fUnit.Direction := KMGetDirection(fUnit.GetPosition, aPos); //Face the new tile
end;


function TUnitActionWalkTo.AssembleTheRoute:boolean;
var i:integer; NodeList2:TKMPointList; TmpPass: TPassability;
begin
  TmpPass := fPass;
  //Build a piece of route to return to nearest road piece connected to destination road network
  if (fPass = CanWalkRoad) and (fDistance=0) then //That is Citizens walking to spot
    if (fTerrain.GetRoadConnectID(fWalkFrom) <> fTerrain.GetRoadConnectID(fWalkTo)) and  //NoRoad returns 0
      (fTerrain.GetRoadConnectID(fWalkTo) <> 0) then //Don't bother returning to the road if our target is off road anyway
      fTerrain.Pathfinding.Route_ReturnToWalkable(fWalkFrom, fWalkTo, wcRoad, fTerrain.GetRoadConnectID(fWalkTo), CanWalk, NodeList);

  //If we are a worker on a construction site, build a piece of route to return to nearest walkable tile on the
  if fPass = CanWorker then //That is Workers on a construction site
    if (fTerrain.GetWalkConnectID(fWalkFrom) <> fTerrain.GetWalkConnectID(fWalkTo)) and  //Not walkable returns 0
      (fTerrain.GetWalkConnectID(fWalkTo) <> 0) then //Don't bother returning to the road if our target is not walkable
    begin
      fTerrain.Pathfinding.Route_ReturnToWalkable(fWalkFrom, fWalkTo, wcWalk, fTerrain.GetWalkConnectID(fWalkTo), CanWorker, NodeList);
      TmpPass := CanWalk; //After this piece of route we are in walk mode
    end;

  //Build a route A*
  if NodeList.Count=0 then //Build a route from scratch
    fTerrain.Pathfinding.Route_Make(fWalkFrom, fWalkTo, fPass, fDistance, fTargetHouse, NodeList) //Try to make the route with fPass
  else begin //Append route to existing part
    NodeList2 := TKMPointList.Create;
    fTerrain.Pathfinding.Route_Make(NodeList.List[NodeList.Count], fWalkTo, TmpPass, fDistance, fTargetHouse, NodeList2); //Try to make the route with fPass
    //If this part of the route fails, the whole route has failed. At minimum Route_Make returns count=1 (fWalkTo)
    if NodeList2.Count = 0 then NodeList.Clearup; //Clear NodeList so we return false
    for i:=2 to NodeList2.Count do
      NodeList.AddEntry(NodeList2.List[i]);
    FreeAndNil(NodeList2);
  end;

  Result := NodeList.Count > 0;
end;


function TUnitActionWalkTo.CheckForNewDestination: TDestinationCheck;
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
    if not AssembleTheRoute then
      Result := dc_NoRoute;
  end;
end;


function TUnitActionWalkTo.CheckTargetHasDied:TTargetDiedCheck;
var TempTargetUnit: TKMUnit;
begin
  if (fTargetUnit=nil) or not fTargetUnit.IsDeadOrDying then
    Result := tc_NoChanges
  else begin
    if (fUnit is TKMUnitWarrior) and (fTargetUnit is TKMUnitWarrior) and (TKMUnitWarrior(fUnit).GetWarriorState <> ws_Engage) then
    begin
      //If a warrior is following a unit it means we are attacking it. (for now anyway)
      //So if this unit dies we must now follow it's commander
      TempTargetUnit := fTargetUnit;
      fPlayers.CleanUpUnitPointer(fTargetUnit);
      fTargetUnit := TKMUnitWarrior(TempTargetUnit).GetCommander.GetUnitPointer;
      //If unit becomes nil that is fine, we will simply walk to it's last known location. But update fOrderLoc to make sure this happens!
      TKMUnitWarrior(fUnit).OrderLocDir := KMPointDir(fWalkTo,TKMUnitWarrior(fUnit).OrderLocDir.Dir);
      //If we are an AI player then we do not keep walking to a dead enemy's position, we halt (and go home) instead
      if (fUnit is TKMUnitWarrior) and (fPlayers.Player[fUnit.GetOwner].PlayerType = pt_Computer) then
        TKMUnitWarrior(fUnit).OrderHalt;
      Result := tc_TargetUpdated;
    end else
      Result := tc_Died;
  end;
end;


{ There's unexpected immovable obstacle on our way (suddenly grown up tree, wall, house)
1. go around the obstacle and keep on walking
2. rebuild the route from current position from scratch}
function TUnitActionWalkTo.CheckForObstacle:TObstacleCheck;
begin
  Result := oc_NoObstacle;

  if (not fTerrain.CheckPassability(NodeList.List[NodePos+1],GetEffectivePassability)) or
     (not fTerrain.CanWalkDiagonaly(fUnit.GetPosition,NodeList.List[NodePos+1])) then

    //Try side stepping the obsticle.
    //By making HighestInteractionCount be the required timeout, we assure the solution is always checked
    if IntSolutionSideStep(NodeList.List[NodePos+1],SIDESTEP_TIMEOUT) then
      Result := oc_NoObstacle
    else
    //Completely re-route if no simple side step solution is available
    if ((fTargetHouse = nil) and fTerrain.Route_CanBeMade(fUnit.GetPosition,fWalkTo,GetEffectivePassability,fDistance, false))
    or ((fTargetHouse <> nil) and fTerrain.Route_CanBeMadeToHouse(fUnit.GetPosition,fTargetHouse,GetEffectivePassability,fDistance, false)) then
    begin
      fUnit.SetActionWalk(fWalkTo, fActionType, fDistance, fTargetUnit, fTargetHouse);
      Result := oc_ReRouteMade;
    end else
      Result := oc_NoRoute;
end;


{ Walk is complete when one of the following is true:
  - We reached last node en route irregardless of walkTarget (position, house, unit)
  - We were walking to spot and required range is reached
  - We were walking to house and required range to house is reached
  - We were walking to unit and met it early
  - The Task wants us to abandon
}
function TUnitActionWalkTo.CheckWalkComplete:boolean;
begin
  Result := (NodePos=NodeList.Count)
            or ((fTargetHouse = nil) and (round(KMLength(fUnit.GetPosition,fWalkTo)) <= fDistance))
            or ((fTargetHouse <> nil) and (fTargetHouse.GetDistance(fUnit.GetPosition) <= fDistance))
            or ((fTargetUnit <> nil) and (KMLength(fUnit.GetPosition,fTargetUnit.GetPosition) <= fDistance))
            or ((fUnit.GetUnitTask <> nil) and fUnit.GetUnitTask.WalkShouldAbandon);
end;


procedure TUnitActionWalkTo.IncVertex;
begin
  //Tell fTerrain that this vertex is being used so no other unit walks over the top of us
  if not KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
    raise ELocError.Create('IncVertex',fVertexOccupied);

  fTerrain.UnitVertexAdd(fUnit.PrevPosition, fUnit.NextPosition);
  fVertexOccupied := KMGetDiagVertex(fUnit.PrevPosition, fUnit.NextPosition);
end;


procedure TUnitActionWalkTo.DecVertex;
begin
  //Tell fTerrain that this vertex is not being used anymore
  if KMSamePoint(fVertexOccupied, KMPoint(0,0)) then
    raise ELocError.Create('DecVertex 0:0',fVertexOccupied);

  fTerrain.UnitVertexRem(fVertexOccupied);
  fVertexOccupied := KMPoint(0,0);
end;


function TUnitActionWalkTo.IntCheckIfPushing(fOpponent: TKMUnit): Boolean;
begin
  Result := False;

  //If we are asking someone to move away then just wait until they are gone
  if (fInteractionStatus <> kis_Pushing) then
    Exit;

  //Make sure they are still moving out of the way
  if (fOpponent.GetUnitAction is TUnitActionWalkTo)
  and (TUnitActionWalkTo(fOpponent.GetUnitAction).fInteractionStatus = kis_Pushed) then
  begin
    Explanation := 'Unit is blocking the way and has been asked to move';
    ExplanationLogAdd;
    Result := True; //Means exit DoUnitInteraction
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
  Result := False;

  if HighestInteractionCount < PUSH_TIMEOUT then
    Exit;

  //Ask the other unit to step aside, only if they are idle!
  if (fOpponent.GetUnitAction is TUnitActionStay)
  and not TUnitActionStay(fOpponent.GetUnitAction).Locked then
  begin
    //We must alert the opponent to our presence because it looks bad when you warrior is pushed
    //by the enemy instead of fighting them.
    //CheckAlliance is for optimisation since pushing allies doesn't matter
    if (fOpponent is TKMUnitWarrior)
    and (fPlayers.CheckAlliance(fOpponent.GetOwner, fUnit.GetOwner) = at_Enemy)
    and TKMUnitWarrior(fOpponent).CheckForEnemy then
      Exit;

    fInteractionStatus := kis_Pushing;
    OpponentPassability := fOpponent.GetDesiredPassability;
    if OpponentPassability = CanWalkRoad then
      OpponentPassability := CanWalk;

    if not CanAbandonInternal then
      raise ELocError.Create('Unit walk IntSolutionPush', fUnit.GetPosition);

    fOpponent.SetActionWalkPushed(fTerrain.GetOutOfTheWay(fOpponent.GetPosition, fUnit.GetPosition, OpponentPassability));

    Explanation := 'Unit was blocking the way but it has been forced to go away now';
    ExplanationLogAdd; //Hopefully next tick tile will be free and we will walk there
    Result := True; //Means exit DoUnitInteraction
  end;
end;


function TUnitActionWalkTo.IntSolutionExchange(fOpponent:TKMUnit; HighestInteractionCount:integer):boolean;
var OpponentNextNextPos: TKMPoint;
begin
  Result := False;

  //Do not initiate exchanges if we are in DestBlocked mode, as we are zero priority and other units will
  if not fDestBlocked
  and (((HighestInteractionCount >= EXCHANGE_TIMEOUT) and (fInteractionStatus <> kis_Pushed)) or //When pushed this timeout/counter is different
     (fInteractionStatus = kis_Pushed)) then //If we get pushed then always try exchanging (if we are here then there is no free tile)
  begin //Try to exchange with the other unit if they are willing
    //If Unit on the way is walking somewhere and not exchanging with someone else
    if (fOpponent.GetUnitAction is TUnitActionWalkTo)
    and (not TUnitActionWalkTo(fOpponent.GetUnitAction).fDoExchange)
    //Unit not yet arrived on tile, wait till it does, otherwise there might be 2 units on one tile
    and (not TUnitActionWalkTo(fOpponent.GetUnitAction).fDoesWalking)
    //Diagonal vertex must not be in use
    and ((not KMStepIsDiag(fUnit.GetPosition,NodeList.List[NodePos+1])) or (not fTerrain.HasVertexUnit(KMGetDiagVertex(fUnit.GetPosition,NodeList.List[NodePos+1])))) then
      //Check that our tile is walkable for the opponent! (we could be a worker on a building site)
      if (TUnitActionWalkTo(fOpponent.GetUnitAction).GetEffectivePassability in fTerrain.Land[fUnit.GetPosition.Y,fUnit.GetPosition.X].Passability) then
      begin
        //Check unit's future position is where we are now and exchange (use NodeList rather than direction as it's not always right)
        if TUnitActionWalkTo(fOpponent.GetUnitAction).GetNextNextPosition(OpponentNextNextPos) then
        begin
          if KMSamePoint(OpponentNextNextPos, fUnit.GetPosition) then
          begin
            //Graphically both units are walking side-by-side, but logically they simply walk through each-other.
            TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange

            Explanation := 'Unit in the way is walking in the opposite direction. Performing an exchange';
            ExplanationLogAdd;
            fDoExchange := true;
            //They both will exchange next tick
            Result := true; //Means exit DoUnitInteraction
          end
          else //Otherwise try to force the unit to exchange IF they are in the waiting phase
            if TUnitActionWalkTo(fOpponent.GetUnitAction).fInteractionStatus = kis_Waiting then
            begin
              //Because we are forcing this exchange we must inject into the other unit's nodelist by passing our current position
              TUnitActionWalkTo(fOpponent.GetUnitAction).PerformExchange(fUnit.GetPosition);

              Explanation := 'Unit in the way is in waiting phase. Forcing an exchange';
              ExplanationLogAdd;
              fDoExchange := true;
              //They both will exchange next tick
              Result := true; //Means exit DoUnitInteraction
            end;
        end;
      end;
  end;
end;


//If we were asked to move away then all we are allowed to do is push and exchanging,
//no re-routing, dodging etc. so we must exit here before any more tests
function TUnitActionWalkTo.IntCheckIfPushed(HighestInteractionCount:integer):boolean;
begin
  Result := false;

  if fInteractionStatus = kis_Pushed then
  begin
    //If we've been trying to get out of the way for a while but we haven't found a solution,
    //(i.e. other unit is stuck) try a different direction
    if HighestInteractionCount >= PUSHED_TIMEOUT then
    begin

      fInteractionStatus := kis_None;
      if not CanAbandonInternal then //in fact tests only for fDoExchange
        raise ELocError.Create('Unit walk IntCheckIfPushed',fUnit.GetPosition);

      //Since only Idle units can be pushed, we don't need to carry on TargetUnit/TargetHouse/etc props
      fUnit.SetActionWalkPushed(fTerrain.GetOutOfTheWay(fUnit.GetPosition,KMPoint(0,0),GetEffectivePassability));
      //This action has now been freed, so we must exit without changing anything
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
var i:byte; //Test 2 options really
    TempPos, OpponentNextNextPos: TKMPoint;
    fAltOpponent:TKMUnit;
begin
  //If there is a unit on one of the tiles either side of target that wants to swap, do so
  Result := false;
  if HighestInteractionCount >= DODGE_TIMEOUT then
  //UnitsHitTest (used twice here) is fairly CPU intensive, so don't run it every time
  if CheckInteractionFreq(HighestInteractionCount,DODGE_TIMEOUT,DODGE_FREQ) then
  begin
    //Tiles to the left (-1) and right (+1) (relative to unit) of the one we are walking to
    for i := 0 to 1 do
    begin
      if i = 0 then TempPos := KMGetPointInDir(fUnit.GetPosition,KMPrevDirection((KMGetDirection(fUnit.GetPosition,NodeList.List[NodePos+1])))).Loc;
      if i = 1 then TempPos := KMGetPointInDir(fUnit.GetPosition,KMNextDirection((KMGetDirection(fUnit.GetPosition,NodeList.List[NodePos+1])))).Loc;
      if fTerrain.TileInMapCoords(TempPos.X,TempPos.Y) and fTerrain.CanWalkDiagonaly(fUnit.GetPosition,TempPos)
        and (GetEffectivePassability in fTerrain.Land[TempPos.Y,TempPos.X].Passability) then //First make sure tile is on map and walkable!
      if fTerrain.HasUnit(TempPos) then //Now see if it has a unit
      begin
        //There is a unit here, first find our alternate opponent
        fAltOpponent := fTerrain.UnitsHitTest(TempPos.X, TempPos.Y);

        //Make sure unit really exists, is walking and has arrived on tile
        if (fAltOpponent <> nil) and (fAltOpponent.GetUnitAction is TUnitActionWalkTo) and
          (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).fDoExchange)
          and (not TUnitActionWalkTo(fAltOpponent.GetUnitAction).fDoesWalking)
          and ((not KMStepIsDiag(fUnit.NextPosition,NodeList.List[NodePos+1])) //Isn't diagonal
          or ((KMStepIsDiag(fUnit.NextPosition,NodeList.List[NodePos+1])       //...or is diagonal and...
          and not fTerrain.HasVertexUnit(KMGetDiagVertex(fUnit.GetPosition,TempPos))))) then //...vertex is free
          if TUnitActionWalkTo(fAltOpponent.GetUnitAction).GetNextNextPosition(OpponentNextNextPos) then
            if KMSamePoint(OpponentNextNextPos, fUnit.GetPosition) then //Now see if they want to exchange with us
            begin
              //Perform exchange from our position to TempPos
              TUnitActionWalkTo(fAltOpponent.GetUnitAction).PerformExchange(KMPoint(0,0)); //Request unforced exchange

              Explanation:='Unit on tile next to target tile wants to swap. Performing an exchange';
              ExplanationLogAdd;
              fDoExchange := true;
              ChangeStepTo(TempPos);
              //They both will exchange next tick
              Result := true; //Means exit DoUnitInteraction
              exit; //Once we've found a solution, do NOT check the other alternative dodge position (when for loop i=1)
            end;
      end;
    end;
  end;
end;


//If the blockage won't go away because it's busy (Locked by other unit) then try going around it
//by re-routing our route and avoiding that tile and all other Locked tiles
function TUnitActionWalkTo.IntSolutionAvoid(fOpponent: TKMUnit): Boolean;
var MaxLength:integer;
begin
  Result := False;

  if (fInteractionCount >= AVOID_TIMEOUT) or fDestBlocked then
  //Route_MakeAvoid is very CPU intensive, so don't run it every time
  if CheckInteractionFreq(fInteractionCount, AVOID_TIMEOUT, AVOID_FREQ) then
  begin

    //Can't go around our target position unless it's a house
    if (not KMSamePoint(fOpponent.GetPosition, fWalkTo)) or (fTargetHouse <> nil) then
    begin
      //We will accept an alternative route up to 3 times greater than the amount we would have been walking anyway
      MaxLength := Math.max(NodeList.Count - NodePos, 15) * 3; //Remainder of our route times 3. Always prepared to walk 15 tiles (e.g. around houses)
      if fDestBlocked or fOpponent.GetUnitAction.Locked then
        if fTerrain.Pathfinding.Route_MakeAvoid(fUnit.GetPosition,fWalkTo,GetEffectivePassability,fDistance,fTargetHouse,NodeList,MaxLength) then //Make sure the route can be made, if not, we must simply wait
        begin
          //NodeList has now been re-routed, so we need to re-init everything else and start walk again
          SetInitValues;
          Explanation := 'Unit in the way is working so we will re-route around it';
          ExplanationLogAdd;
          fDestBlocked := False;
          //Exit, then on next tick new walk will start
          Result := True; //Means exit DoUnitInteraction
        end
        else
        begin
          fDestBlocked := True; //When in this mode we are zero priority as we cannot reach our destination. This allows serfs with stone to get through and clear our path.
          fInteractionStatus := kis_Waiting; //If route cannot be made it means our destination is currently not available (workers in the way) So allow us to be pushed.
          Explanation := 'Our destination is blocked by busy units';
          ExplanationLogAdd;
        end;
    end;
  end;
end;


{This solution tries to find an unoccupied tile where unit can side-step}
function TUnitActionWalkTo.IntSolutionSideStep(aPosition:TKMPoint; HighestInteractionCount:integer):boolean;
var SideStepTest:TKMPoint; Found: Boolean;
begin
  Result := false; //Should only return true if a sidestep was taken (for use in CheckForObstacle)
  if (HighestInteractionCount < SIDESTEP_TIMEOUT) or fDoExchange then exit;
  if KMSamePoint(aPosition, fWalkTo) then exit; //Someone stays right on target, no point in side-stepping
  if not CheckInteractionFreq(HighestInteractionCount, SIDESTEP_TIMEOUT, SIDESTEP_FREQ) then exit; //FindSideStepPosition is CPU intensive, so don't run it every time

  //Find a node
  if NodePos+2 > NodeList.Count then //Tell Terrain about our next position if we can
    Found := fTerrain.FindSideStepPosition(fUnit.GetPosition, aPosition, KMPoint(0,0), GetEffectivePassability, SideStepTest, NodePos-fLastSideStepNodePos < 2)
  else
    Found := fTerrain.FindSideStepPosition(fUnit.GetPosition, aPosition, NodeList.List[NodePos+2], GetEffectivePassability, SideStepTest, NodePos-fLastSideStepNodePos < 2);

  if not Found then exit; //It could be 0,0 if all tiles were blocked (return false)

  //Otherwise the sidestep is valid so modify our route to go via this tile
  Explanation := 'Sidestepping to a tile next to target';
  ExplanationLogAdd;
  ChangeStepTo(SideStepTest);
  fLastSideStepNodePos := NodePos;
  Result := true; //Means exit DoUnitInteraction, but also means a sidestep has been taken (for use in CheckForObstacle)
end;


//States whether we are allowed to run time consuming tests
//@Lewin: I don't quite understand this logic here, could you please explain it?
function TUnitActionWalkTo.CheckInteractionFreq(aIntCount, aTimeout, aFreq: Integer): Boolean;
begin
  Result := (aIntCount - aTimeout >= 0) and ((aIntCount - aTimeout) mod aFreq = 0);
end;


function TUnitActionWalkTo.DoUnitInteraction: Boolean;
var
  fOpponent:TKMUnit;
  HighestInteractionCount: integer;
begin
  Result:=true; //false = interaction yet unsolved, stay and wait.
  if not DO_UNIT_INTERACTION then exit;

  //If there's a unit using this vertex to walk diagonally then we must wait, they will be finished after this step
  if KMStepIsDiag(fUnit.GetPosition,NodeList.List[NodePos+1]) and
    fTerrain.HasVertexUnit(KMGetDiagVertex(fUnit.GetPosition,NodeList.List[NodePos+1])) then
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
  fOpponent := fTerrain.UnitsHitTest(NodeList.List[NodePos+1].X, NodeList.List[NodePos+1].Y);
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
  if (fUnit.UnitType in [ANIMAL_MIN..ANIMAL_MAX])
    and not fTerrain.CheckAnimalIsStuck(fUnit.GetPosition,fPass) then
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
  if IntSolutionAvoid(fOpponent) then Exit;
  if IntSolutionSideStep(fOpponent.GetPosition,fInteractionCount) then exit;

  //We will allow other units to force an exchange with us as we haven't found a solution or our destination is blocked
  if (fInteractionCount >= WAITING_TIMEOUT) or fDestBlocked then fInteractionStatus := kis_Waiting;

  //If we haven't exited yet we must increment the counters so we know how long we've been here
  inc(fInteractionCount);
end;


function TUnitActionWalkTo.GetNextNextPosition(out NextNextPos:TKMPoint):boolean;
begin
  if InRange(NodePos, 1, NodeList.Count) then
  begin
    NextNextPos := NodeList.List[NodePos+1];
    Result := true;
  end
  else
  begin
    NextNextPos := KMPoint(0,0);
    Result := false; //Our route is not that long, so there is no "NextNext" position
    //@Crow: This is not an error. That old comment "Error" was a mistake, I see why you thought it was that way.
    //       I have changed it to return boolean and use "out" as you have been doing in other places.
    //       Please check I did it right. Thanks for your great help, you're doing a good job :) From Lewin. To be deleted.
  end;
end;


//Modify route to go to this destination instead. Kind of like starting the walk over again but without recreating the action
procedure TUnitActionWalkTo.ChangeWalkTo(aLoc: TKMPoint; aDistance: Single; aUseExactTarget: Boolean = True);
begin
  if not fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
    raise ELocError.Create('Invalid Change Walk To for '+fResource.UnitDat[fUnit.UnitType].UnitName, aLoc);

  //We are no longer being pushed
  if fInteractionStatus = kis_Pushed then
    fInteractionStatus := kis_None;

  fDistance   := aDistance;
  fUseExactTarget := aUseExactTarget;

  if fUseExactTarget then
    fNewWalkTo := aLoc
  else
    fNewWalkTo := fTerrain.GetClosestTile(aLoc, fUnit.GetPosition, fPass, fUseExactTarget);

  //Release pointers if we had them
  fPlayers.CleanUpHousePointer(fTargetHouse);
  fPlayers.CleanUpUnitPointer(fTargetUnit);
end;


procedure TUnitActionWalkTo.ChangeWalkTo(aNewTargetUnit: TKMUnit; aDistance: Single);
begin
  //We are no longer being pushed
  if fInteractionStatus = kis_Pushed then
    fInteractionStatus := kis_None;

  fDistance   := aDistance;
  fUseExactTarget := True; //When chasing unit it is always an exact target

  fNewWalkTo := aNewTargetUnit.GetPosition;

  //Release pointers if we had them
  fPlayers.CleanUpHousePointer(fTargetHouse);
  fPlayers.CleanUpUnitPointer(fTargetUnit);
  if aNewTargetUnit <> nil then
    fTargetUnit := aNewTargetUnit.GetUnitPointer; //Change target
end;


function TUnitActionWalkTo.GetEffectivePassability:TPassability; //Returns passability that unit is allowed to walk on
begin
  //Road walking is only recomended. (i.e. for route building) We are allowed to step off the road sometimes.
  if fPass = CanWalkRoad then
    Result := CanWalk
  else
    Result := fPass;
end;


function TUnitActionWalkTo.Execute: TActionResult;
var
  DX,DY: Shortint;
  WalkX,WalkY,Distance: Single;
  ThisAction: TUnitAction;
  U: TKMUnit;
begin
  Result := ActContinues;
  StepDone := False;
  fDoesWalking := False; //Set it to false at start of update

  //Happens whe e.g. Serf stays in front of Store and gets Deliver task
  if KMSamePoint(fWalkFrom, fWalkTo) then begin
    Result := ActDone;
    Exit;
  end;

  //Route was not built
  if NodeList.Count = 0 then begin
    Result := ActAborted;
    Exit;
  end;

  //Walk complete - NodePos cannot be greater than NodeCount (this should not happen, cause is unknown but for now this check stops crashes)
  if NodePos > NodeList.Count then begin
    if KMStepIsDiag(fUnit.PrevPosition, fUnit.NextPosition) then
      DecVertex; //Unoccupy vertex
    fUnit.IsExchanging := False; //Disable sliding (in case it was set in previous step)
    Result := ActDone;
    Exit;
  end;

  //Execute the route in series of moves
  Distance := fResource.UnitDat[fUnit.UnitType].Speed;

  //Check if unit has arrived on tile
  if KMSamePointF(fUnit.PositionF, KMPointF(NodeList.List[NodePos]), Distance/2) then
  begin

    //Set precise position to avoid rounding errors
    fUnit.PositionF := KMPointF(NodeList.List[NodePos]);

    if (NodePos > 1) and (not fWaitingOnStep) and KMStepIsDiag(NodeList.List[NodePos-1],NodeList.List[NodePos]) then
      DecVertex; //Unoccupy vertex

    fWaitingOnStep := true;

    StepDone := true; //Unit stepped on a new tile
    fUnit.IsExchanging := false; //Disable sliding (in case it was set in previous step)


    { Update destination point }

    //Make changes to our route if we are supposed to be following a unit
    if CanAbandonInternal
      and (fTargetUnit <> nil)
      and (not fTargetUnit.IsDeadOrDying)
      and not KMSamePoint(fTargetUnit.GetPosition, fWalkTo) then
    begin
      //If target unit has moved then change course and keep following it
      ChangeWalkTo(fTargetUnit, fDistance);
      //If we are a warrior commander tell our memebers to use this new position
      if (fUnit is TKMUnitWarrior) and TKMUnitWarrior(fUnit).IsCommander then
      begin
        TKMUnitWarrior(fUnit).OrderAttackUnit(fTargetUnit, True); //Give members new position
        TKMUnitWarrior(fUnit).OrderLocDir := KMPointDir(fTargetUnit.GetPosition, TKMUnitWarrior(fUnit).OrderLocDir.Dir); //This line is notsuperflous
      end;
    end;

    //Check if we need to walk to a new destination
    if CanAbandonInternal and (CheckForNewDestination = dc_NoRoute) then begin
      Result := ActAborted;
      exit;
    end;

    //Check for units nearby to fight
    if CanAbandonInternal and (fUnit is TKMUnitWarrior) then
      if TKMUnitWarrior(fUnit).CheckForEnemy then
        //If we've picked a fight it means this action no longer exists,
        //so we must exit out (don't set DoEnd as that will now apply to fight action)
        exit;

    //Walk complete
    if not fDoExchange and CheckWalkComplete then
    begin
      if (fDistance>0) and ((fUnit.GetUnitTask = nil) or (not fUnit.GetUnitTask.WalkShouldAbandon))
      and not KMSamePoint(NodeList.List[NodePos],fWalkTo) then //Happens rarely when we asked to sidestep towards our not locked target (Warrior)
        fUnit.Direction := KMGetDirection(NodeList.List[NodePos],fWalkTo); //Face tile (e.g. worker)
      Result := ActDone;
      exit;
    end;

    //Abandon if someone already mines our resource
    if (fUnit.GetUnitTask is TTaskMining) and (NodePos+1 = NodeList.Count) and not fUseExactTarget then
    begin
      U := fTerrain.UnitsHitTest(NodeList.List[NodePos+1].X, NodeList.List[NodePos+1].Y);
      Assert((U = nil) or (U.GetUnitAction <> nil));
      if (U <> nil) and U.GetUnitAction.Locked then
      begin
        Result := ActDone; //TaskMining will care about the rest (and find new workplan)
        Exit;
      end;
    end;

    //Check if target unit (warrior) has died and if so abandon our walk and so delivery task can exit itself
    if CanAbandonInternal then
      case CheckTargetHasDied of
        tc_NoChanges, tc_TargetUpdated:;
        tc_Died: begin Result := ActAborted; exit; end;
      end;

    //This is sometimes caused by unit interaction changing the route so simply ignore it
    if KMSamePoint(NodeList.List[NodePos], NodeList.List[NodePos+1]) then
    begin
      inc(NodePos); //Inc the node pos and exit so this step is simply skipped
      Exit; //Will take next step during next execute
    end;

    //If we were in Worker mode but have now reached the walk network of our destination switch to CanWalk mode to avoid walking on other building sites
    if (fPass = CanWorker) and (fTerrain.GetWalkConnectID(fWalkTo) <> 0) and
      (fTerrain.GetWalkConnectID(fWalkTo) = fTerrain.GetWalkConnectID(NodeList.List[NodePos])) then
      fPass := CanWalk;

    //Update unit direction according to next Node
    fUnit.Direction := KMGetDirection(NodeList.List[NodePos],NodeList.List[NodePos+1]);

    //Check if we can walk to next tile in the route
    if CanAbandonInternal then
    case CheckForObstacle of
      oc_NoObstacle:;
      oc_ReRouteMade: exit; //New route will pick-up
      oc_NoRoute: begin Result := ActAborted; exit; end; //
    end;

    //Perform exchange
    //Both exchanging units have fDoExchange:=true assigned by 1st unit, hence 2nd should not try doing UnitInteraction!
    if fDoExchange then
    begin
    
       //If this is a diagonal exchange we must make sure someone (other than the other unit) is not crossing our path
      if KMStepIsDiag(fUnit.GetPosition,NodeList.List[NodePos+1])
      and (not fTerrain.VertexUsageCompatible(fUnit.GetPosition,NodeList.List[NodePos+1])) then
        Exit; //Someone is crossing the path of our exchange, so we will wait until they are out of the way (this check guarantees both units in the exchange will wait)

      inc(NodePos);

      fUnit.UpdateNextPosition(NodeList.List[NodePos]);

      //Check if we are the first or second unit (has the swap already been performed?)
      if fUnit = fTerrain.Land[fUnit.PrevPosition.Y,fUnit.PrevPosition.X].IsUnit then
        fTerrain.UnitSwap(fUnit.PrevPosition,fUnit.NextPosition,fUnit);

      fInteractionStatus := kis_None;
      fDoExchange := false;
      fUnit.IsExchanging := true; //So unit knows that it must slide
      fInteractionCount := 0;
      if KMStepIsDiag(fUnit.PrevPosition,fUnit.NextPosition) then IncVertex; //Occupy the vertex
    end else
    begin
      ThisAction := fUnit.GetUnitAction; //We need to know whether DoUnitInteraction destroys Self (this action)
      U := fUnit; //Could be destroyed by DoUnitInteraction, we need to check afterwards

      if not DoUnitInteraction then
      begin
        //If ThisAction <> ThisUnit.GetUnitAction means DoUnitInteraction destroyed this action, so we must exit immediately
        if (ThisAction = U.GetUnitAction)
        and (fUnit.UnitType in [ANIMAL_MIN..ANIMAL_MAX])
        and not fTerrain.CheckAnimalIsStuck(fUnit.GetPosition,fPass) then
          Result := ActDone; //Animals have no tasks hence they can choose new WalkTo spot no problem, unless they are stuck

        Exit; //Do no further walking until unit interaction is solved
      end else
        fInteractionCount := 0; //Reset the counter when there is no blockage and we can walk

      inc(NodePos);
      fUnit.UpdateNextPosition(NodeList.List[NodePos]);

      if GetLength(fUnit.PrevPosition,fUnit.NextPosition) > 1.5 then
        raise ELocError.Create('Unit walk length>1.5', fUnit.PrevPosition);

      if fTerrain.Land[fUnit.PrevPosition.Y,fUnit.PrevPosition.X].IsUnit = nil then
        raise ELocError.Create('Unit walk Prev position IsUnit = nil',fUnit.PrevPosition);

      fTerrain.UnitWalk(fUnit.PrevPosition, fUnit.NextPosition,fUnit); //Pre-occupy next tile
      if KMStepIsDiag(fUnit.PrevPosition, fUnit.NextPosition) then IncVertex; //Occupy the vertex
    end;

  end;
  fWaitingOnStep := false;

  if NodePos>NodeList.Count then
    raise ELocError.Create('WalkTo overrun', fUnit.GetPosition);

  WalkX := NodeList.List[NodePos].X - fUnit.PositionF.X;
  WalkY := NodeList.List[NodePos].Y - fUnit.PositionF.Y;
  DX := sign(WalkX); //-1,0,1
  DY := sign(WalkY); //-1,0,1

  if (DX <> 0) and (DY <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fUnit.PositionF := KMPointF(fUnit.PositionF.X + DX*min(Distance,abs(WalkX)),
                              fUnit.PositionF.Y + DY*min(Distance,abs(WalkY)));

  inc(fUnit.AnimStep);
  StepDone := false; //We are not actually done because now we have just taken another step
  fDoesWalking := True; //Now it's definitely true that unit did walked one step
end;


procedure TUnitActionWalkTo.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fWalkFrom);
  SaveStream.Write(fWalkTo);
  SaveStream.Write(fNewWalkTo);
  SaveStream.Write(fDistance);
  SaveStream.Write(fUseExactTarget);
  if fTargetUnit <> nil then
    SaveStream.Write(fTargetUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  if fTargetHouse <> nil then
    SaveStream.Write(fTargetHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));

  SaveStream.Write(fPass,SizeOf(fPass));
  SaveStream.Write(fDoesWalking);
  SaveStream.Write(fWaitingOnStep);
  SaveStream.Write(fDestBlocked);
  SaveStream.Write(fDoExchange);
  SaveStream.Write(fInteractionCount);
  SaveStream.Write(fLastSideStepNodePos);
  SaveStream.Write(fInteractionStatus,SizeOf(fInteractionStatus));

  SaveStream.Write(fVertexOccupied);
  NodeList.Save(SaveStream);
  SaveStream.Write(NodePos);
end;


procedure TUnitActionWalkTo.Paint;
begin
  if SHOW_UNIT_ROUTES and fGame.AllowDebugRendering then
    fRenderAux.UnitRoute(NodeList, NodePos, byte(fUnit.UnitType));
end;


end.
