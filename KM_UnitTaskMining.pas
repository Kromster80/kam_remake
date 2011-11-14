unit KM_UnitTaskMining;
{$I KaM_Remake.inc}
interface
uses Math, SysUtils,
    KM_CommonTypes, KM_Units, KM_Units_Workplan, KM_Points, KM_Defaults;


{Perform resource mining}
type
  TTaskMining = class(TUnitTask)
  private
    fBeastID:byte;
    fWorkPlan:TUnitWorkPlan;
    function ResourceExists:boolean;
  public
    constructor Create(aUnit:TKMUnit; aRes:TResourceType);
    destructor Destroy; override;
    constructor Load(LoadStream:TKMemoryStream); override;
    property WorkPlan:TUnitWorkPlan read fWorkPlan;
    procedure FindAnotherWorkPlan(aCalledInternally:boolean=false);
    function Execute:TTaskResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_Houses, KM_PlayersCollection, KM_Terrain, KM_ResourceGFX;


{ TTaskMining }
constructor TTaskMining.Create(aUnit:TKMUnit; aRes:TResourceType);
begin
  Inherited Create(aUnit);
  fTaskName := utn_Mining;
  fWorkPlan := TUnitWorkPlan.Create;
  fBeastID  := 0;

  if aUnit.GetHome is TKMHouseWoodcutters then
    fWorkPlan.WoodcutterMode := TKMHouseWoodcutters(aUnit.GetHome).WoodcutterMode;

  fWorkPlan.FindPlan(aUnit.UnitType, aUnit.GetHome.HouseType, aRes, KMPointBelow(aUnit.GetHome.GetEntrance));
end;


destructor TTaskMining.Destroy;
begin
  if (not fUnit.GetHome.IsDestroyed) and (fUnit.GetHome.GetState = hst_Work) then
    fUnit.GetHome.SetState(hst_Idle); //Make sure we don't abandon and leave our house with "working" animations
  FreeAndNil(fWorkPlan);
  Inherited;
end;


constructor TTaskMining.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  fWorkPlan := TUnitWorkPlan.Create;
  fWorkPlan.Load(LoadStream);
  LoadStream.Read(fBeastID);
end;


//Try to find alternative target for our WorkPlan
//Happens when we discover that resource is gone or is occupied by another busy unit
//Return false if new plan could not be found
procedure TTaskMining.FindAnotherWorkPlan(aCalledInternally:boolean=false);
var OldLoc: TKMPoint;
begin
  OldLoc := WorkPlan.Loc;
  //Tell the work plan to find a new resource of the same gathering script
  if WorkPlan.FindDifferentResource(fUnit.UnitType, KMPointBelow(fUnit.GetHome.GetEntrance), WorkPlan.Loc) then
  begin
    if not KMSamePoint(OldLoc,WorkPlan.Loc) then
    begin
      fUnit.SetActionAbandonWalk(fUnit.NextPosition, WorkPlan.ActionWalkTo); //Abandon the current walk
      //Set the walk again (by setting fPhase to 1)
      if aCalledInternally then
        fPhase := 0 //Will become 1 after this loop
      else
        fPhase := 1;
      exit;
    end;
  end;
  //Otherwise abandon as there is no other work plan available
  fUnit.SetActionAbandonWalk(fUnit.NextPosition, WorkPlan.ActionWalkTo); //Abandon the current walk
  fPhase := 99; //Exit the task on next update, since this function could be called externally (by WalkTo)
end;


function TTaskMining.ResourceExists:boolean;
begin
  with fTerrain do
  case WorkPlan.GatheringScript of
    gs_StoneCutter:     Result := TileIsStone(WorkPlan.Loc.X, WorkPlan.Loc.Y-1) > 0; //Check stone deposit above Loc, which is walkable tile
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
                              ActionWalkFrom  := ua_WalkTool; //Carry our scythe back (without the corn) as the player saw us take it out
                              ActionWorkType  := ua_Work1;
                              WorkCyc    := 10;
                              Product1   := rt_None; //Don't produce corn
                              ProdCount1 := 0;
                            end;
                        end;
    gs_FarmerWine:      Result := TileIsWineField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 65535);
    gs_FisherCatch:     Result := CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir),true);
    gs_WoodCutterPlant: Result := CheckPassability(WorkPlan.Loc, CanPlantTrees);
    gs_WoodCutterCut:   Result := ObjectIsChopableTree(KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir), 4) and (Land[KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir).Y, KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir).X].TreeAge >= TreeAgeFull)
    else                Result := true;
  end;
end;


{This is execution of Resource mining}
function TTaskMining.Execute:TTaskResult;
const SkipWalk=8; SkipWork=30; //Skip to certain Phases
var D:TKMDirection; TimeToWork, StillFrame:integer; ResAcquired:boolean;
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
         SetActionGoIn(WorkPlan.ActionWalkTo, gd_GoOutside, GetHome); //Walk outside the house
       end else begin
         fPhase := SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
         SetActionLockedStay(0, ua_Walk);
         Exit;
       end;
       //We cannot assume that the walk is still valid because the terrain could have changed while we were walking out of the house.
    1: SetActionWalkToSpot(WorkPlan.Loc, WorkPlan.ActionWalkTo);
    2: //Before work tasks for specific mining jobs
       if WorkPlan.GatheringScript = gs_FisherCatch then begin
         Direction := WorkPlan.WorkDir;
         SetActionLockedStay(13, ua_Work1, false); //Throw the line out
       end else
         SetActionLockedStay(0, WorkPlan.ActionWalkTo);
    3: if not ResourceExists then
         FindAnotherWorkPlan(true)
       else
       begin //Choose direction and time to work

         if WorkPlan.WorkDir <> dir_NA then
           Direction := WorkPlan.WorkDir;

         if fResource.UnitDat[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count < 1 then
           for D:=dir_N to dir_NW do
             if fResource.UnitDat[UnitType].UnitAnim[WorkPlan.ActionWorkType, D].Count > 1 then
             begin
               Direction := D;
               Break;
             end;
         TimeToWork := WorkPlan.WorkCyc * Math.max(fResource.UnitDat[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count, 1);
         SetActionLockedStay(TimeToWork, WorkPlan.ActionWorkType, false);
       end;
    4: //After work tasks for specific mining jobs
       case WorkPlan.GatheringScript of
         gs_WoodCutterCut:  SetActionLockedStay(10, WorkPlan.ActionWorkType, true, 5, 5); //Wait for the tree to start falling down
         gs_FisherCatch:    SetActionLockedStay(15, ua_Work, false); //Pull the line in
         else               SetActionLockedStay(0, WorkPlan.ActionWorkType);
       end;
    5: begin
         StillFrame := 0;
         case WorkPlan.GatheringScript of //Perform special tasks if required
           gs_StoneCutter:     fTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
           gs_FarmerSow:       fTerrain.SowCorn(WorkPlan.Loc);
           gs_FarmerCorn:      fTerrain.CutCorn(WorkPlan.Loc);
           gs_FarmerWine:      fTerrain.CutGrapes(WorkPlan.Loc);
           gs_FisherCatch:     begin fTerrain.CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir)); WorkPlan.ActionWorkType := ua_WalkTool; end;
           gs_WoodCutterPlant: fTerrain.SetTree(WorkPlan.Loc,fTerrain.ChooseTreeToPlant(WorkPlan.Loc));
           gs_WoodCutterCut:   begin fTerrain.FallTree(KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir)); StillFrame := 5; end;
         end;
         SetActionLockedStay(WorkPlan.AfterWorkDelay, WorkPlan.ActionWorkType, true, StillFrame, StillFrame);
       end;
    6: begin
         if WorkPlan.GatheringScript = gs_WoodCutterCut then
           fTerrain.ChopTree(WorkPlan.Loc); //Make the tree turn into a stump
         SetActionWalkToSpot(KMPointBelow(GetHome.GetEntrance), WorkPlan.ActionWalkFrom); //Go home
         Thought := th_Home;
       end;
    7: SetActionGoIn(WorkPlan.ActionWalkFrom, gd_GoInside, GetHome); //Go inside

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
  fWorkPlan.Save(SaveStream);
  SaveStream.Write(fBeastID);
end;


end.
