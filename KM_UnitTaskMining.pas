unit KM_UnitTaskMining;
{$I KaM_Remake.inc}
interface
uses Math, 
    KM_CommonTypes, KM_Units, KM_Units_Workplan, KM_Points;


{Perform resource mining}
type
  TTaskMining = class(TUnitTask)
    private
      fBeastID:byte;
      function ResourceExists:boolean;
    public
      WorkPlan:TUnitWorkPlan;
      constructor Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit);
      destructor Destroy; override;
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad; override;
      function WalkTargetBlocked(aBlockingUnit: TKMUnit):boolean;
      function Execute:TTaskResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_Defaults, KM_Houses, KM_PlayersCollection, KM_Terrain;


{ TTaskMining }
constructor TTaskMining.Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_Mining;
  WorkPlan  := aWorkPlan;
  fBeastID   := 0;
end;


destructor TTaskMining.Destroy;
begin
  if (not fUnit.GetHome.IsDestroyed) and (fUnit.GetHome.GetState = hst_Work) then
    fUnit.GetHome.SetState(hst_Idle); //Make sure we don't abandon and leave our house with "working" animations
  Inherited;
end;


constructor TTaskMining.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fBeastID);
  //Don't load WorkPlan, it's linked by TKMUnitCitizen
end;


procedure TTaskMining.SyncLoad;
begin
  Inherited;
  //nothing to sync, Workplan is assigned by unit
end;


//This procedure is run by the walk action when the walk target is blocked by a busy unit.
//The unit might have taken our resource, if so we should find a new one.
function TTaskMining.WalkTargetBlocked(aBlockingUnit: TKMUnit):boolean;
var OldLoc: TKMPoint;
begin
  //Is the unit taking our resource? (are they are mining with the same gathering script)
  if (aBlockingUnit.GetUnitTask is TTaskMining) and
    (TTaskMining(aBlockingUnit.GetUnitTask).WorkPlan.GatheringScript = WorkPlan.GatheringScript) then
  begin
    Result := true;
    OldLoc := WorkPlan.Loc;
    //Tell the work plan to find a new resource of the same gathering script
    if WorkPlan.FindDifferentResource(KMPointY1(fUnit.GetHome.GetEntrance), WorkPlan.Loc) then
    begin
      if not KMSamePoint(OldLoc,WorkPlan.Loc) then
      begin
        fUnit.SetActionAbandonWalk(fUnit.NextPosition,WorkPlan.WalkTo); //Abandon the current walk
        fPhase := 1 //Set the walk again
      end;
    end
    else
    begin
      fUnit.SetActionAbandonWalk(fUnit.NextPosition,WorkPlan.WalkTo); //Abandon the current walk
      fPhase := 99; //Exit the task
    end;
  end
  else
    Result := false;
end;


function TTaskMining.ResourceExists:boolean;
begin
  with fTerrain do
  case WorkPlan.GatheringScript of
    gs_StoneCutter:     Result := TileIsStone(KMPoint(WorkPlan.Loc.X, WorkPlan.Loc.Y - 1)) > 0; //Check stone deposit above Loc, which is walkable tile
    gs_FarmerSow:       Result := TileIsCornField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0);
    gs_FarmerCorn:      begin
                          Result := TileIsCornField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 65535);
                          if Result then exit; //Resource still exists so exit
                          //If corn has been cut we can possibly plant new corn here to save time
                          Result := TileIsCornField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0);
                          if Result then
                            with WorkPlan do
                            begin
                              GatheringScript := gs_FarmerSow; //Switch to sowing corn rather than cutting
                              WalkFrom   := ua_WalkTool; //Carry our scythe back (without the corn) as the player saw us take it out
                              WorkType   := ua_Work1;
                              WorkCyc    := 10;
                              Product1   := rt_None; //Don't produce corn
                              ProdCount1 := 0;
                            end;
                        end;
    gs_FarmerWine:      Result := TileIsWineField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 65535);
    gs_FisherCatch:     Result := CatchFish(KMPointDir(WorkPlan.Loc.X,WorkPlan.Loc.Y,WorkPlan.WorkDir),true);
    gs_WoodCutterPlant: Result := CheckPassability(WorkPlan.Loc, CanPlantTrees);
    gs_WoodCutterCut:   Result := ObjectIsChopableTree(KMGetVertexTile(WorkPlan.Loc, TKMDirection(WorkPlan.WorkDir+1)), 4) and (Land[KMGetVertexTile(WorkPlan.Loc, TKMDirection(WorkPlan.WorkDir+1)).Y, KMGetVertexTile(WorkPlan.Loc, TKMDirection(WorkPlan.WorkDir+1)).X].TreeAge >= TreeAgeFull)
    else                Result := true;
  end;
end;


{This is execution of Resource mining}
function TTaskMining.Execute:TTaskResult;
const SkipWalk=8; SkipWork=30; //Skip to certain Phases
var Dir:integer; TimeToWork, StillFrame:integer; ResAcquired:boolean;
begin
  Result := TaskContinues;

  //there's no point in doing a task if we can't return home
  if (fUnit.GetHome <> nil) and fUnit.GetHome.IsDestroyed then
  begin
    Result := TaskDone;
    exit;
  end;

  with fUnit do
  case fPhase of
    0: if WorkPlan.HasToWalk then begin
         GetHome.SetState(hst_Empty);
         SetActionGoIn(WorkPlan.WalkTo, gd_GoOutside, GetHome); //Walk outside the house
       end else begin
         fPhase := SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
         SetActionLockedStay(0,ua_Walk);
         exit;
       end;
       //We cannot assume that the walk is still valid because the terrain could have changed while we were walking out of the house.
    1: SetActionWalkToSpot(WorkPlan.Loc, 0, WorkPlan.WalkTo);
    2: //Before work tasks for specific mining jobs
       if WorkPlan.GatheringScript = gs_FisherCatch then begin
         Direction := TKMDirection(WorkPlan.WorkDir+1);
         SetActionLockedStay(13, ua_Work1, false); //Throw the line out
       end else
         SetActionLockedStay(0, WorkPlan.WalkTo);
    3: //IF resource still exists on location
       if ResourceExists then
       begin //Choose direction and time to work
         if WorkPlan.WorkDir = -1 then
           Dir := byte(Direction) //Keep direction from walk (i.e. it doesn't matter)
         else
         begin
           Dir := byte(WorkPlan.WorkDir+1);
           if UnitSprite[byte(UnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count<=1 then
             for Dir:=1 to 8 do
               if UnitSprite[byte(UnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count>1 then break;
           Dir := Math.min(Dir,8);
           Direction := TKMDirection(Dir);
         end;
         TimeToWork := WorkPlan.WorkCyc * Math.max(UnitSprite[byte(UnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count,1);
         SetActionLockedStay(TimeToWork, WorkPlan.WorkType, false);
       end
       else
       begin
         Result := TaskDone;
         exit;
       end;
    4: //After work tasks for specific mining jobs
       case WorkPlan.GatheringScript of
         gs_WoodCutterCut:  SetActionLockedStay(10, WorkPlan.WorkType, true, 5, 5); //Wait for the tree to start falling down
         gs_FisherCatch:    SetActionLockedStay(15, ua_Work, false); //Pull the line in
         else               SetActionLockedStay(0, WorkPlan.WorkType);
       end;
    5: begin
         StillFrame := 0;
         case WorkPlan.GatheringScript of //Perform special tasks if required
           gs_StoneCutter:     fTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
           gs_FarmerSow:       fTerrain.SowCorn(WorkPlan.Loc);
           gs_FarmerCorn:      fTerrain.CutCorn(WorkPlan.Loc);
           gs_FarmerWine:      fTerrain.CutGrapes(WorkPlan.Loc);
           gs_FisherCatch:     begin fTerrain.CatchFish(KMPointDir(WorkPlan.Loc.X,WorkPlan.Loc.Y,WorkPlan.WorkDir)); WorkPlan.WorkType := ua_WalkTool; end;
           gs_WoodCutterPlant: fTerrain.SetTree(WorkPlan.Loc,fTerrain.ChooseTreeToPlant(WorkPlan.Loc));
           gs_WoodCutterCut:   begin
           fTerrain.FallTree(KMGetVertexTile(WorkPlan.Loc, TKMDirection(WorkPlan.WorkDir+1))); StillFrame := 5;
           end;
         end;
         SetActionLockedStay(WorkPlan.AfterWorkDelay, WorkPlan.WorkType, true, StillFrame, StillFrame);
       end;
    6: begin
         if WorkPlan.GatheringScript = gs_WoodCutterCut then
           fTerrain.ChopTree(WorkPlan.Loc); //Make the tree turn into a stump
         SetActionWalkToSpot(KMPointY1(GetHome.GetEntrance), 0, WorkPlan.WalkFrom); //Go home
         Thought := th_Home;
       end;
    7: SetActionGoIn(WorkPlan.WalkFrom, gd_GoInside, GetHome); //Go inside

    {Unit back at home and can process its booty now}
    8: begin
        Thought := th_None;
        fPhase2 := 1;
        GetHome.SetState(hst_Work); //Set house to Work state
        if WorkPlan.Resource1 <> rt_None then GetHome.ResTakeFromIn(WorkPlan.Resource1, WorkPlan.Count1);
        if WorkPlan.Resource2 <> rt_None then GetHome.ResTakeFromIn(WorkPlan.Resource2, WorkPlan.Count2);
        GetHome.fCurrentAction.SubActionAdd([ha_Smoke]);
        if WorkPlan.GatheringScript = gs_SwineBreeder then begin //Swines get feed and taken immediately
          fBeastID := TKMHouseSwineStable(GetHome).FeedBeasts;
          TKMHouseSwineStable(GetHome).TakeBeast(fBeastID);
        end;
        if WorkPlan.ActCount >= fPhase2 then begin
           GetHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
           //Keep unit idling till next Phase, Idle time is -1 to compensate TaskExecution Phase
           SetActionStay(WorkPlan.HouseAct[fPhase2].TimeToWork-1,ua_Walk);
        end else begin
           fPhase := SkipWork; //Skip to step 30
           SetActionLockedStay(0,ua_Walk);
           exit;
        end;
       end;
    9..29: begin //Allow for 20 different "house work" phases
           inc(fPhase2);
           if (fPhase2 = 2)and(WorkPlan.GatheringScript = gs_HorseBreeder) then
             fBeastID := TKMHouseSwineStable(GetHome).FeedBeasts; //Feed a horse
           if WorkPlan.ActCount >= fPhase2 then
           begin
             GetHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
             if WorkPlan.ActCount > fPhase2 then
               SetActionStay(WorkPlan.HouseAct[fPhase2].TimeToWork-1,ua_Walk) //-1 to compensate units UpdateState run
             else
               SetActionStay(WorkPlan.HouseAct[fPhase2].TimeToWork-2,ua_Walk) //-2 to compensate 2 UpdateStates of a unit in last Act
           end else begin
             fPhase := SkipWork;
             SetActionLockedStay(0,ua_Walk);
             exit;
           end;
       end;
    30: begin
          if WorkPlan.GatheringScript = gs_HorseBreeder then
            TKMHouseSwineStable(GetHome).TakeBeast(fBeastID); //Take the horse after feeding

          case WorkPlan.GatheringScript of
            gs_CoalMiner:    ResAcquired := fTerrain.DecOreDeposit(WorkPlan.Loc, rt_Coal);
            gs_GoldMiner:    ResAcquired := fTerrain.DecOreDeposit(WorkPlan.Loc, rt_GoldOre);
            gs_IronMiner:    ResAcquired := fTerrain.DecOreDeposit(WorkPlan.Loc, rt_IronOre);
            gs_SwineBreeder: ResAcquired := fBeastID<>0;
            gs_HorseBreeder: ResAcquired := fBeastID<>0;
            else             ResAcquired := true;
          end;

          if ResAcquired then begin
            GetHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
            GetHome.ResAddToOut(WorkPlan.Product2,WorkPlan.ProdCount2);
            fPlayers.Player[fUnit.GetOwner].Stats.GoodProduced(WorkPlan.Product1,WorkPlan.ProdCount1);
            fPlayers.Player[fUnit.GetOwner].Stats.GoodProduced(WorkPlan.Product2,WorkPlan.ProdCount2);
          end;

          GetHome.SetState(hst_Idle);
          SetActionStay(WorkPlan.AfterWorkIdle-1,ua_Walk);
        end;
    else Result := TaskDone;
  end;
  inc(fPhase);
end;


procedure TTaskMining.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fBeastID);
  //Don't save WorkPlan, we'll use link to TKMUnitCitizen.WorkPlan.
end;


end.
