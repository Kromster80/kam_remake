unit KM_UnitTaskMining;
interface
uses Math, KromUtils, SysUtils, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KM_Units_Workplan;


{Perform resource mining}
type
  TTaskMining = class(TUnitTask)
  private
    BeastID:byte;
    function ResourceExists():boolean;
  public
    WorkPlan:TUnitWorkPlan;
    constructor Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit; aHouse:TKMHouse);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad(); override;
    procedure Execute(out TaskDone:boolean); override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;

implementation
uses KM_Terrain, KM_UnitActionWalkTo;


{ TTaskMining }
constructor TTaskMining.Create(aWorkPlan:TUnitWorkPlan; aUnit:TKMUnit; aHouse:TKMHouse);
begin
  Inherited Create(aUnit);
  fTaskName := utn_Mining;
  WorkPlan  := aWorkPlan;
  BeastID   := 0;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskMining.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(BeastID);
end;


procedure TTaskMining.SyncLoad();
begin
  Inherited;
  WorkPlan := TKMUnitCitizen(fUnit).WorkPlan; //Relink instead of reading-finding
end;


function TTaskMining.ResourceExists():boolean;
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
                              WalkFrom := ua_WalkTool; //Carry our scythe back (without the corn) as the player saw us take it out
                              WorkType := ua_Work1;
                              WorkCyc := 10;
                              Product1 := rt_None; //Don't produce corn
                              ProdCount1 := 0;
                            end;
                        end;
    gs_FarmerWine:      Result := TileIsWineField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 65535);
    gs_FisherCatch:     Result := CatchFish(KMPointDir(WorkPlan.Loc.X,WorkPlan.Loc.Y,WorkPlan.WorkDir),true);
    gs_WoodCutterPlant: Result := CheckPassability(WorkPlan.Loc, CanPlantTrees);
    gs_WoodCutterCut:   Result := ObjectIsChopableTree(KMGetVertexTile(WorkPlan.Loc.X,WorkPlan.Loc.Y,TKMDirection(WorkPlan.WorkDir+1)), 4) and (Land[KMGetVertexTile(WorkPlan.Loc.X,WorkPlan.Loc.Y,TKMDirection(WorkPlan.WorkDir+1)).Y, KMGetVertexTile(WorkPlan.Loc.X,WorkPlan.Loc.Y,TKMDirection(WorkPlan.WorkDir+1)).X].TreeAge >= TreeAgeFull)
    else                Result := true;
  end;
end;


{This is execution of Resource mining}
procedure TTaskMining.Execute(out TaskDone:boolean);
const SkipWalk=8; SkipWork=30; //Skip to certain Phases
var Dir:integer; TimeToWork, StillFrame:integer;
begin
TaskDone:=false;
if fUnit.GetHome <> nil then if fUnit.GetHome.IsDestroyed then
begin
  //Make sure we always exit if our home is destroyed
  TaskDone := true;
  exit;
end;
with fUnit do
  case fPhase of
    0: if WorkPlan.HasToWalk then begin
         GetHome.SetState(hst_Empty,0);
         SetActionGoIn(WorkPlan.WalkTo,gd_GoOutside,GetHome); //Walk outside the house
       end else begin
         fPhase:=SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
         SetActionLockedStay(0,ua_Walk);
         exit;
       end;
    1: SetActionWalk(fUnit,WorkPlan.Loc, WorkPlan.WalkTo);
    2: begin //Before work tasks for specific mining jobs
         if WorkPlan.GatheringScript = gs_FisherCatch then
         begin
           Direction := TKMDirection(WorkPlan.WorkDir+1);
           SetActionStay(13, ua_Work1, false); //Throw the line out
         end
         else
           SetActionLockedStay(0, WorkPlan.WalkTo);
       end;
    3: //IF resource still exists on location
       if ResourceExists then
       begin //Choose direction and time to work
         //If WorkDir is -1 it means keep direction from walk (i.e. it doesn't matter)
         if WorkPlan.WorkDir <> -1 then
         begin
           Dir:=integer(WorkPlan.WorkDir+1);
           if UnitSprite[byte(GetUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count<=1 then
             for Dir:=1 to 8 do
               if UnitSprite[byte(GetUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count>1 then break;
           Dir:=min(Dir,8);
           Direction:=TKMDirection(Dir);
         end
         else Dir := byte(Direction); //Use direction from walk
         TimeToWork:=WorkPlan.WorkCyc*max(UnitSprite[byte(GetUnitType)].Act[byte(WorkPlan.WorkType)].Dir[Dir].Count,1);
         SetActionStay(TimeToWork, WorkPlan.WorkType, false);
       end
       else
       begin
         TaskDone := true;
         exit;
       end;
    4: begin //After work tasks for specific mining jobs
             if WorkPlan.GatheringScript = gs_WoodCutterCut then
             begin
               SetActionStay(10, WorkPlan.WorkType, true, 5, 5); //Wait for the tree to start falling down
             end
             else
             if WorkPlan.GatheringScript = gs_FisherCatch then
             begin
               SetActionStay(15, ua_Work, false); //Pull the line in
             end
             else
               SetActionLockedStay(0, WorkPlan.WorkType);
       end;
    5: begin StillFrame := 0;
             case WorkPlan.GatheringScript of //Perform special tasks if required
               gs_StoneCutter:     fTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
               gs_FarmerSow:       fTerrain.SowCorn(WorkPlan.Loc);
               gs_FarmerCorn:      fTerrain.CutCorn(WorkPlan.Loc);
               gs_FarmerWine:      fTerrain.CutGrapes(WorkPlan.Loc);
               gs_FisherCatch:     begin fTerrain.CatchFish(KMPointDir(WorkPlan.Loc.X,WorkPlan.Loc.Y,WorkPlan.WorkDir)); WorkPlan.WorkType := ua_WalkTool; end;
               gs_WoodCutterPlant: fTerrain.SetTree(WorkPlan.Loc,fTerrain.ChooseTreeToPlant(WorkPlan.Loc));
               gs_WoodCutterCut:   begin
               fTerrain.FallTree(KMGetVertexTile(WorkPlan.Loc.X,WorkPlan.Loc.Y,TKMDirection(WorkPlan.WorkDir+1))); StillFrame := 5;
               end;
             end;
         SetActionStay(WorkPlan.AfterWorkDelay, WorkPlan.WorkType, true, StillFrame, StillFrame);
       end;
    6: begin
         if WorkPlan.GatheringScript = gs_WoodCutterCut then
           fTerrain.ChopTree(WorkPlan.Loc); //Make the tree turn into a stump
         SetActionWalk(fUnit,KMPointY1(GetHome.GetEntrance), WorkPlan.WalkFrom); //Go home
         Thought := th_Home;
       end;
    7: SetActionGoIn(WorkPlan.WalkFrom,gd_GoInside,GetHome); //Go inside

    {Unit back at home and can process its booty now}
    8: begin
        Thought := th_None;
        fPhase2:=1;
        GetHome.SetState(hst_Work,0); //Set house to Work state
        GetHome.ResTakeFromIn(WorkPlan.Resource1,WorkPlan.Count1);
        GetHome.ResTakeFromIn(WorkPlan.Resource2,WorkPlan.Count2);
        GetHome.fCurrentAction.SubActionAdd([ha_Smoke]);
        if WorkPlan.GatheringScript = gs_SwineBreeder then begin
          BeastID:=TKMHouseSwineStable(GetHome).FeedBeasts;
          if BeastID<>0 then
            TKMHouseSwineStable(GetHome).TakeBeast(BeastID);
        end;
        if WorkPlan.ActCount>=fPhase2 then begin
           GetHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act,WorkPlan.HouseAct[fPhase2].TimeToWork);
           //Keep unit idling till next Phase, Idle time is -1 to compensate TaskExecution Phase
           SetActionStay(WorkPlan.HouseAct[fPhase2].TimeToWork-1,ua_Walk);
        end else begin
           fPhase:=SkipWork;
           SetActionLockedStay(0,ua_Walk);
           exit;
        end;
       end;
    9..29: begin //Allow for 20 different "house work" phases
           inc(fPhase2);
           if (fPhase2 = 2)and(WorkPlan.GatheringScript = gs_HorseBreeder) then
             BeastID := TKMHouseSwineStable(GetHome).FeedBeasts; //Feed a horse
           if WorkPlan.ActCount >= fPhase2 then
           begin
             GetHome.fCurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act,WorkPlan.HouseAct[fPhase2].TimeToWork);
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
          case WorkPlan.GatheringScript of
            gs_CoalMiner: fTerrain.DecOreDeposit(WorkPlan.Loc,rt_Coal);
            gs_GoldMiner: fTerrain.DecOreDeposit(WorkPlan.Loc,rt_GoldOre);
            gs_IronMiner: fTerrain.DecOreDeposit(WorkPlan.Loc,rt_IronOre);
          end;
          if WorkPlan.GatheringScript = gs_HorseBreeder then begin
            if BeastID<>0 then begin
              TKMHouseSwineStable(GetHome).TakeBeast(BeastID);
              GetHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
            end;
          end else
          if WorkPlan.GatheringScript = gs_SwineBreeder then begin
            if BeastID<>0 then begin
              GetHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
              GetHome.ResAddToOut(WorkPlan.Product2,WorkPlan.ProdCount2);
            end;
          end else begin
            GetHome.ResAddToOut(WorkPlan.Product1,WorkPlan.ProdCount1);
            GetHome.ResAddToOut(WorkPlan.Product2,WorkPlan.ProdCount2); //This is unused tbh
          end;

          GetHome.SetState(hst_Idle,WorkPlan.AfterWorkIdle);
          SetActionStay(WorkPlan.AfterWorkIdle-1,ua_Walk);
        end;
    else TaskDone:=true;
  end;
inc(fPhase);
if (fUnit.GetUnitAction=nil)and(not TaskDone) then
  fLog.AssertToLog(false,'(fUnit.fCurrentAction=nil)and(not TaskDone)');
end;


procedure TTaskMining.Save(SaveStream:TKMemoryStream);
begin
  inherited;
  SaveStream.Write(BeastID);
end;

end.
