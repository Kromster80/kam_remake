unit KM_UnitActionWalkTo;
interface
uses KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Player, KM_Units, SysUtils, Math;

      {Walk to somewhere}
type
  TUnitActionWalkTo = class(TUnitAction)
    private
      fWalker:TKMUnit;
      fWalkFrom,fWalkTo,fAvoid:TKMPoint;
      fWalkToSpot:boolean;
      fPass:TPassability; //Desired passability set once on Create
      fIgnorePass:boolean;
      DoesWalking:boolean;
      DoEvade:boolean; //Command to make exchange maneuver with other unit
      function ChoosePassability(KMUnit: TKMUnit; DoIgnorePass:boolean):TPassability;
      function AssembleTheRoute():boolean;
      function DoUnitInteraction():boolean;
    public
      NodeList:TKMPointList;
      NodePos:integer;
      fRouteBuilt:boolean;
      Explanation:string; //Debug only, explanation what unit is doing
      constructor Create(KMUnit: TKMUnit; LocB,Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true; const aIgnorePass:boolean=false);
      destructor Destroy; override;
      function GetNextPosition():TKMPoint;
      procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
    end;
            

implementation
uses KM_Houses, KM_Game, KM_PlayersCollection, KM_Terrain, KM_Viewport;


{ TUnitActionWalkTo }
constructor TUnitActionWalkTo.Create(KMUnit: TKMUnit; LocB, Avoid:TKMPoint; const aActionType:TUnitActionType=ua_Walk; const aWalkToSpot:boolean=true; const aIgnorePass:boolean=false);
begin
  fLog.AssertToLog(LocB.X*LocB.Y<>0,'Illegal WalkTo 0;0');

  Inherited Create(aActionType);
  fWalker       := KMUnit;
  fWalkFrom     := fWalker.GetPosition;
  fWalkTo       := LocB;
  fAvoid        := Avoid;
  fWalkToSpot   := aWalkToSpot;
  fIgnorePass   := aIgnorePass; //Store incase we need it in DoUnitInteraction re-routing
  fPass         := ChoosePassability(fWalker, fIgnorePass);

  NodeList      := TKMPointList.Create; //Freed on destroy
  NodePos       := 1;
  fRouteBuilt   := false;

  if KMSamePoint(fWalkFrom,fWalkTo) then //We don't care for this case, Execute will report action is done immediately
    exit; //so we don't need to perform any more processing

  fRouteBuilt := AssembleTheRoute();
end;


destructor TUnitActionWalkTo.Destroy;
begin
  FreeAndNil(NodeList);
  Inherited;
end;


function TUnitActionWalkTo.ChoosePassability(KMUnit: TKMUnit; DoIgnorePass:boolean):TPassability;
begin
  case KMUnit.GetUnitType of //Select desired passability depending on unit type
    ut_Serf..ut_Fisher,ut_StoneCutter..ut_Recruit: Result:=canWalkRoad; //Citizens except Worker
    ut_Wolf..ut_Duck: Result:=AnimalTerrain[byte(KMUnit.GetUnitType)] //Animals
    else Result:=canWalk; //Worker, Warriors
  end;

  if not DO_SERFS_WALK_ROADS then Result:=canWalk; //Reset everyone to canWalk for debug

  //Thats for 'miners' at work
  if (KMUnit.GetUnitType in [ut_Woodcutter,ut_Farmer,ut_Fisher,ut_StoneCutter]) then
  if (KMUnit.GetUnitTask is TTaskMining) then
    Result:=canWalk;

  if DoIgnorePass then Result:=canAll; //Thats for Workers walking on house area and maybe some other cases?
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

  //If route still unbuilt..
  Result := NodeList.Count > 0;
  if not Result then
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with default fPass');

  if not Result then begin //Build a route with canWalk
    fTerrain.Route_Make(fWalkFrom, fWalkTo, fAvoid, canWalk, fWalkToSpot, NodeList); //Try to make a route
    Result := NodeList.Count>0;
  end;

  if not Result then
    fLog.AddToLog('Unable to make a route '+TypeToString(fWalkFrom)+' > '+TypeToString(fWalkTo)+'with canWalk');

end;


function TUnitActionWalkTo.DoUnitInteraction():boolean;
var fOpponent:TKMUnit; T:TKMPoint;
begin
  Result:=true; //false = interaction yet unsolved, stay and wait
  if not DO_UNIT_INTERACTION then exit;

  //Check if that the last node in route anyway
  if NodePos >= NodeList.Count then exit;

  //Check if there's a unit blocking the way
  if fTerrain.Land[NodeList.List[NodePos+1].Y,NodeList.List[NodePos+1].X].IsUnit=0 then exit;

  fOpponent:=fPlayers.UnitsHitTest(NodeList.List[NodePos+1].X,NodeList.List[NodePos+1].Y);

  //If there's yet no Unit on the way but tile is pre-occupied
  if fOpponent = nil then begin
    //Do nothing and wait till unit is actually there so we can interact with it
    Explanation:='Can''t walk. No Unit on the way but tile is occupied';
    Result:=false;
    exit;
  end;

  if (fWalker.GetUnitType in [ut_Wolf..ut_Duck])and(not(fOpponent.GetUnitType in [ut_Wolf..ut_Duck])) then begin
    Explanation:='Unit is animal and therefor has no priority in movement';
    Result:=false;
    exit;
  end;

  //
  //Now we should solve unexpected obstacles situations
  //------------------------------------------------------------------------------------------------

  //Abort action and task if we can't go around the obstacle

  //
  //Now we try to solve different Unit-Unit situations
  //------------------------------------------------------------------------------------------------

  //If Unit on the way is staying
  if (fOpponent.GetUnitAction is TUnitActionStay) then
  begin
    if fOpponent.GetUnitActionType = ua_Walk then //Unit stays idle, not working or something
    begin 
      //Force Unit to go away
      fOpponent.SetActionWalk(fOpponent, fTerrain.GetOutOfTheWay(fOpponent.GetPosition,fWalker.GetPosition,canWalk), KMPoint(0,0));
      Explanation := 'Unit was blocking the way but it''s forced to get away now';
      Result := false; //Next frame tile will be free and unit will walk there
      exit;
    end else
    begin
    //If Unit on the way is doing something and won't move away
      if KMSamePoint(fOpponent.GetPosition,fWalkTo) then // Target position is occupied by another unit
      begin
        Explanation := 'Target position is occupied by another Unit, can''t do nothing about it but wait. '+inttostr(random(123));
        Result := false;
        exit;
        { UNRESOLVED ! }
      end else
      begin
        {StartWalkingAround}
        Explanation:='Unit on the way is doing something and can''t be forced to go away';
          fWalker.SetActionWalk
          (
              fWalker,
              fWalkTo,
              KMPoint(NodeList.List[NodePos+1].X,NodeList.List[NodePos+1].Y), //Avoid this tile
              GetActionType,
              fWalkToSpot,
              fIgnorePass
          );
        Result := false; //Keep 'false' for the matter of Execute cycle still running
        exit;     
      end;
    end;
  end;

  //If Unit on the way is walking somewhere
  if (fOpponent.GetUnitAction is TUnitActionWalkTo) then begin //Unit is walking
    //Check unit direction to be opposite and exchange, but only if the unit is staying on tile, not walking
    if (min(byte(fOpponent.Direction),byte(fWalker.Direction))+4 = max(byte(fOpponent.Direction),byte(fWalker.Direction))) then
    begin
      if TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking then
      begin
        //Unit yet not arrived on tile, wait till it does, otherwise there might be 2 units on one tile
        Explanation:='Unit on the way is walking '+TypeToString(fOpponent.Direction)+'. Waiting till it walks into spot and then exchange';
        Result:=false;
        exit;
      end else
      begin
        //Graphically both units are walking side-by-side, but logically they simply walk through each-other.
        TUnitActionWalkTo(fOpponent.GetUnitAction).DoEvade:=true;
        TUnitActionWalkTo(fWalker.GetUnitAction).DoEvade:=true;
        Explanation:='Unit on the way is walking opposite direction. Performing an exchange';
        Result:=true;
        exit;
      end
    end else
    //Unit walking direction is not opposite
    begin
      if TUnitActionWalkTo(fOpponent.GetUnitAction).DoesWalking then
      begin
        //Simply wait till it walks away
        Explanation:='Unit on the way is walking '+TypeToString(fOpponent.Direction)+'. Waiting till it walks away '+TypeToString(fWalker.Direction);
        Result:=false;
        exit;
      end else
      begin
        if KMSamePoint(fOpponent.GetPosition, fWalkTo) then
        begin
          //inject a random walkaround node
          Explanation:='Unit on the way is walking in place obstructing our target. Forcing it to go away.';

          //Solution A
          //Walk through 7x50 group time  ~105sec
          {T:=KMPoint(0,0);
          //This could be potential flaw, but so far it worked out allright
          if TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1 <= TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.Count then
          T := fTerrain.FindNewNode(
            fOpponent.GetPosition,
            TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.List[TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1],
            //fOpponent.NextPosition,
            canWalk);
          if KMSamePoint(T,KMPoint(0,0)) then
          begin
            Explanation:='Unit on the way is walking in place obstructing our target. Can''t walk around. '+inttostr(random(123));
            Result := false;
            exit;
            // UNRESOLVED !
          end;
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          }

          //Solution B
          //Walk through 7x50 group time  ~105sec
          T:=fOpponent.GetPosition; //Elegant enough?
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          T:=fWalker.GetPosition;
          TUnitActionWalkTo(fOpponent.GetUnitAction).NodeList.InjectEntry(TUnitActionWalkTo(fOpponent.GetUnitAction).NodePos+1,T);
          

          //In fact this is not very smart idea, cos fOpponent may recieve endless InjectEntries!
          Result := false;
          exit;
        end else
        begin
          //Unit has WalkTo action, but is standing in place - don't mess with it and go around
          Explanation:='Unit on the way is walking still. Go around it. '+inttostr(random(123));
          fWalker.SetActionWalk
          (
              fWalker,
              fWalkTo,
              fOpponent.GetPosition, //KMPoint(Nodes[NodePos+1].X,Nodes[NodePos+1].Y), //Avoid this tile
              GetActionType,
              fWalkToSpot,
              fIgnorePass
          );
          Result := false; //Keep 'false' for the matter of Execute cycle still running
          exit;
        end;
      end;
    end;
  end;

  if (fOpponent.GetUnitAction is TUnitActionGoIn) then begin //Unit is walking into house, we can wait
    Explanation:='Unit is walking into house, we can wait';
    Result:=false; //Temp
    exit;
  end;
    //If enything else - wait
    fLog.AssertToLog(false,'DO_UNIT_INTERACTION');
end;


function TUnitActionWalkTo.GetNextPosition():TKMPoint;
begin
  if NodePos > NodeList.Count then
    Result:=KMPoint(0,0) //Error
  else Result:=NodeList.List[NodePos];
end;


procedure TUnitActionWalkTo.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
const DirectionsBitfield:array[-1..1,-1..1]of TKMDirection = ((dir_NW,dir_W,dir_SW),(dir_N,dir_NA,dir_S),(dir_NE,dir_E,dir_SE));
var
  DX,DY:shortint; WalkX,WalkY,Distance:single;
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
    //DEBUG, should be wrapped somehow for the release
    fViewport.SetCenter(fWalker.GetPosition.X,fWalker.GetPosition.Y);
    fGame.PauseGame(true);
    //Pop-up some message
  end;

  //Execute the route in series of moves
  TimeDelta:=0.1;
  Distance:= TimeDelta * fWalker.GetSpeed;

  //Check if unit has arrived on tile
  //@Krom: It is crashing here sometimes when units get stuck under a house.
  //Normally it pauses when the route can't be built. The unit just starts to move
  //(shows animation and moves one step) then it crashes here. Has the CanMakeRoute
  //check been removed or something?
  if Equals(fWalker.PositionF.X,NodeList.List[NodePos].X,Distance/2) and Equals(fWalker.PositionF.Y,NodeList.List[NodePos].Y,Distance/2) then begin

    GetIsStepDone:=true; //Unit stepped on a new tile

    //Set precise position to avoid rounding errors
    fWalker.PositionF:=KMPointF(NodeList.List[NodePos].X,NodeList.List[NodePos].Y);

    //Update unit direction according to next Node 
    if NodePos+1<=NodeList.Count then begin
      DX := sign(NodeList.List[NodePos+1].X - NodeList.List[NodePos].X); //-1,0,1
      DY := sign(NodeList.List[NodePos+1].Y - NodeList.List[NodePos].Y); //-1,0,1
      fWalker.Direction := DirectionsBitfield[DX,DY];
    end;

    //Perform interaction
    if not DoUnitInteraction() then
    begin
      //fWalker gets nulled if DoUnitInteraction creates new path, hence we need to querry KMUnit instead
      DoEnd := KMUnit.GetUnitType in [ut_Wolf..ut_Duck]; //Animals have no tasks hence they can choose new WalkTo spot no problem
      exit; //Do no further walking until unit interaction is solved
    end;

    inc(NodePos);

    if NodePos>NodeList.Count then begin
      DoEnd:=true;
      exit;
    end else begin
      fWalker.PrevPosition:=fWalker.NextPosition; ////////////////
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

  //fWalker.Direction:=DirectionsBitfield[DX,DY];

  if (DX <> 0) and (DY <> 0) then
    Distance:=Distance / 1.41; {sqrt (2) = 1.41421 }

  fWalker.PositionF:=KMPointF(fWalker.PositionF.X + DX*min(Distance,abs(WalkX)),
                              fWalker.PositionF.Y + DY*min(Distance,abs(WalkY)));

  inc(fWalker.AnimStep);

  DoesWalking:=true; //Now it's definitely true that unit did walked one step
end;


end.
 