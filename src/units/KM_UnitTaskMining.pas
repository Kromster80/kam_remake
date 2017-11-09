unit KM_UnitTaskMining;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Units, KM_Units_Workplan, KM_Terrain,
  KM_ResWares;


type
  // Resource mining task
  TTaskMining = class(TUnitTask)
  private
    fBeastID: Byte;
    fWorkPlan: TUnitWorkPlan;
    function ResourceExists: Boolean;
    function ResourceTileIsLocked: Boolean;
    function ChooseToCutOrPlant: TPlantAct;
    procedure FindAnotherWorkPlan;
  public
    constructor Create(aUnit: TKMUnit; aWare: TWareType);
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    function GetActivityText: UnicodeString;
    property WorkPlan: TUnitWorkPlan read fWorkPlan;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Houses, KM_HandsCollection, KM_Resource, KM_ResMapElements, KM_ResTexts, KM_ResHouses,
  KM_Hand, KM_ResUnits;


{ TTaskMining }
constructor TTaskMining.Create(aUnit: TKMUnit; aWare: TWareType);
begin
  inherited Create(aUnit);

  fTaskName := utn_Mining;
  fWorkPlan := TUnitWorkPlan.Create;
  fBeastID  := 0;

  fWorkPlan.FindPlan( fUnit,
                      fUnit.GetHome.HouseType,
                      aWare,
                      aUnit.GetHome.PointBelowEntrance,
                      ChooseToCutOrPlant
                      );
end;


destructor TTaskMining.Destroy;
begin
  // Make sure we don't abandon and leave our house with "working" animations
  if (not fUnit.GetHome.IsDestroyed)
  and (fUnit.GetHome.GetState = hst_Work) then
    fUnit.GetHome.SetState(hst_Idle);

  FreeAndNil(fWorkPlan);

  inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TTaskMining.WalkShouldAbandon:boolean;
begin
  Result := false;
  Assert(fUnit is TKMUnitCitizen);
  if fPhase = 2 then //Unit is walking to mine-position
    Result := ResourceTileIsLocked or //If someone takes our place
              not ResourceExists or //Resource has gone
              not TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, WorkPlan.GatheringScript);
end;


//Chose if we don't care or prefer specific activity
//depending on orders or clogged output
function TTaskMining.ChooseToCutOrPlant: TPlantAct;
begin
  Result := taAny;

  case fUnit.GetHome.HouseType of
    ht_Woodcutters: case TKMHouseWoodcutters(fUnit.GetHome).WoodcutterMode of
                      wcm_Chop:         Result := taCut;
                      wcm_ChopAndPlant: if fUnit.GetHome.CheckResOut(wt_Trunk) >= MAX_WARES_IN_HOUSE then
                                          Result := taPlant
                                        else
                                          Result := taAny;
                    end;
    ht_Farm:        if fUnit.GetHome.CheckResOut(wt_Corn) >= MAX_WARES_IN_HOUSE then
                      Result := taPlant
                    else
                      Result := taAny;
    else Result := taAny; //We don't care since other housetypes don't have concurent activities
  end;
end;


constructor TTaskMining.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  fWorkPlan := TUnitWorkPlan.Create;
  fWorkPlan.Load(LoadStream);
  LoadStream.Read(fBeastID);
end;


function TTaskMining.GetActivityText: UnicodeString;
begin
  case WorkPlan.GatheringScript of
    gs_StoneCutter:     Result := gResTexts[TX_UNIT_TASK_STONE];
    gs_FarmerSow:       Result := gResTexts[TX_UNIT_TASK_SOW_CORN];
    gs_FarmerCorn:      Result := gResTexts[TX_UNIT_TASK_CUTTING_CORN];
    gs_FarmerWine:      Result := gResTexts[TX_UNIT_TASK_GRAPES];
    gs_FisherCatch:     Result := gResTexts[TX_UNIT_TASK_FISHING];
    gs_WoodCutterCut:   Result := gResTexts[TX_UNIT_TASK_CUT_TREE];
    gs_WoodCutterPlant: Result := gResTexts[TX_UNIT_TASK_PLANT_TREE];
    else                Result := 'Unknown';
  end;
end;


//Try to find alternative target for our WorkPlan
//Happens when we discover that resource is gone or is occupied by another busy unit
//Return false if new plan could not be found
procedure TTaskMining.FindAnotherWorkPlan;
var OldLoc: TKMPoint; OldDir:TKMDirection;
begin
  OldLoc := WorkPlan.Loc;
  OldDir := WorkPlan.WorkDir;

  //Tell the work plan to find a new resource of the same gathering script
  if WorkPlan.FindDifferentResource(fUnit, fUnit.GetHome.PointBelowEntrance, OldLoc) then
  begin
    //Must always give us a new location (or same location but different direction)
    Assert((OldDir <> WorkPlan.WorkDir) or not KMSamePoint(OldLoc, WorkPlan.Loc));
    fPhase := 0; //Set the walk again (Will become 1 after this loop)
    fUnit.SetActionLockedStay(0, WorkPlan.ActionWalkTo);
  end else
  begin
    fPhase := 99; //Abandon as there is no other work plan available (Exit the task on next update)
    fUnit.SetActionLockedStay(0, WorkPlan.ActionWalkTo);
  end;
end;


function TTaskMining.ResourceTileIsLocked: Boolean;
var P: TKMPoint;
begin
  if WorkPlan.GatheringScript = gs_WoodCutterCut then
  begin
    P := KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir);
    //Check all tiles around the tree, like we do in TKMTerrain.FindTree
    Result := gTerrain.TileIsLocked(P)
      or ((P.X > 1) and gTerrain.TileIsLocked(KMPoint(P.X-1, P.Y))) //if K=1, K-1 will be off map
      or ((P.Y > 1) and gTerrain.TileIsLocked(KMPoint(P.X, P.Y-1)))
      or ((P.X > 1) and (P.Y > 1) and gTerrain.TileIsLocked(KMPoint(P.X-1, P.Y-1)))
  end
  else
    Result := gTerrain.TileIsLocked(WorkPlan.Loc);
end;


function TTaskMining.ResourceExists: Boolean;
var P: TKMPoint;
begin
  with gTerrain do
  case WorkPlan.GatheringScript of
    gs_StoneCutter:     Result := TileIsStone(WorkPlan.Loc.X, WorkPlan.Loc.Y-1) > 0; //Check stone deposit above Loc, which is walkable tile
    gs_FarmerSow:       Result := TileIsCornField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0);
    gs_FarmerCorn:      begin
                          Result := TileIsCornField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = CORN_AGE_MAX);
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
                              Product1   := wt_None; //Don't produce corn
                              ProdCount1 := 0;
                            end;
                        end;
    gs_FarmerWine:      Result := TileIsWineField(WorkPlan.Loc) and (Land[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = CORN_AGE_MAX);
    gs_FisherCatch:     Result := CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir),true);
    gs_WoodCutterPlant: Result := TileGoodForTree(WorkPlan.Loc.X, WorkPlan.Loc.Y);
    gs_WoodCutterCut:   begin
                          P := KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir);
                          Result := ObjectIsChopableTree(P, caAgeFull) and (Land[P.Y, P.X].TreeAge >= TREE_AGE_FULL);
                        end;
    else                Result := True;
  end;
end;


{This is execution of Resource mining}
function TTaskMining.Execute: TTaskResult;
const
  // Shortcuts to skip certain Phases
  SkipWalk = 9;
  SKIP_WORK = 11 + MAX_WORKPLAN; //Skip to certain Phases
var
  D: TKMDirection;
  TimeToWork, StillFrame: Integer;
  ResAcquired: Boolean;
begin
  Result := TaskContinues;

  //there's no point in doing a task if we can't return home
  if (fUnit.GetHome <> nil) and fUnit.GetHome.IsDestroyed then
  begin
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  if WorkPlan.HasToWalk then
        begin
          GetHome.SetState(hst_Empty);
          SetActionGoIn(WorkPlan.ActionWalkTo, gd_GoOutside, GetHome); //Walk outside the house

          //Woodcutter takes his axe with him when going to chop trees
          if (WorkPlan.GatheringScript = gs_WoodCutterCut) then
            GetHome.CurrentAction.SubActionRem([ha_Flagpole]);
        end
        else
        begin
          fPhase := SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
          SetActionLockedStay(0, ua_Walk);
          Exit;
        end;

    1:  //We cannot assume that the walk is still valid because the terrain could have changed while we were walking out of the house.
        SetActionWalkToSpot(WorkPlan.Loc, WorkPlan.ActionWalkTo);

    2: //Check if we are at the location. WalkTo could have failed or resource could have been exhausted
       if not KMSamePoint(NextPosition, WorkPlan.Loc) or not ResourceExists or
          not TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, WorkPlan.GatheringScript) then
         FindAnotherWorkPlan
       else
         SetActionLockedStay(0, WorkPlan.ActionWalkTo);

    3: //Before work tasks for specific mining jobs
       if WorkPlan.GatheringScript = gs_FisherCatch then
       begin
         Direction := WorkPlan.WorkDir;
         SetActionLockedStay(13, ua_Work1, false); //Throw the line out
       end else
         SetActionLockedStay(0, WorkPlan.ActionWalkTo);

    4: //Choose direction and time to work
       begin
         if WorkPlan.WorkDir <> dir_NA then
           Direction := WorkPlan.WorkDir;

         if gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count < 1 then
           for D := dir_N to dir_NW do
             if gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, D].Count > 1 then
             begin
               Direction := D;
               Break;
             end;

         TimeToWork := WorkPlan.WorkCyc * Math.max(gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count, 1);
         SetActionLockedStay(TimeToWork, WorkPlan.ActionWorkType, False);
       end;
    5: //After work tasks for specific mining jobs
       case WorkPlan.GatheringScript of
         gs_WoodCutterCut:  SetActionLockedStay(10, WorkPlan.ActionWorkType, true, 5, 5); //Wait for the tree to start falling down
         gs_FisherCatch:    SetActionLockedStay(15, ua_Work, false); //Pull the line in
         else               SetActionLockedStay(0, WorkPlan.ActionWorkType);
       end;
    6: begin
         StillFrame := 0;
         case WorkPlan.GatheringScript of //Perform special tasks if required
           gs_StoneCutter:      gTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
           gs_FarmerSow:        gTerrain.SowCorn(WorkPlan.Loc);
           gs_FarmerCorn:       gTerrain.CutCorn(WorkPlan.Loc);
           gs_FarmerWine:       gTerrain.CutGrapes(WorkPlan.Loc);
           gs_FisherCatch:      begin
                                  gTerrain.CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir));
                                  WorkPlan.ActionWorkType := ua_WalkTool;
                                end;
           gs_WoodCutterPlant:  //If the player placed a house plan here while we were digging don't place the
                                //tree so the house plan isn't canceled. This is actually the same as TSK/TPR IIRC
                                if TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, gs_WoodCutterPlant) then
                                  gTerrain.SetObject(WorkPlan.Loc, gTerrain.ChooseTreeToPlant(WorkPlan.Loc));
           gs_WoodCutterCut:    begin
                                  gTerrain.FallTree(KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir));
                                  StillFrame := 5;
                                end;
         end;
         SetActionLockedStay(WorkPlan.AfterWorkDelay, WorkPlan.ActionWorkType, True, StillFrame, StillFrame);
       end;
    7: begin
         //Removing the tree and putting a stump is handled in gTerrain.UpdateState from FallingTrees list
         SetActionWalkToSpot(GetHome.PointBelowEntrance, WorkPlan.ActionWalkFrom); //Go home
         Thought := th_Home;
       end;
    8: SetActionGoIn(WorkPlan.ActionWalkFrom, gd_GoInside, GetHome); //Go inside

    {Unit back at home and can process its booty now}
    9:    begin
            Thought := th_None;
            fPhase2 := 0;
            GetHome.SetState(hst_Work);

            //Take required resources
            if WorkPlan.Resource1 <> wt_None then GetHome.ResTakeFromIn(WorkPlan.Resource1, WorkPlan.Count1);
            if WorkPlan.Resource2 <> wt_None then GetHome.ResTakeFromIn(WorkPlan.Resource2, WorkPlan.Count2);
            gHands[fUnit.Owner].Stats.WareConsumed(WorkPlan.Resource1, WorkPlan.Count1);
            gHands[fUnit.Owner].Stats.WareConsumed(WorkPlan.Resource2, WorkPlan.Count2);

            GetHome.CurrentAction.SubActionAdd([ha_Smoke]);
            if WorkPlan.GatheringScript = gs_SwineBreeder then
            begin //Swines get feed and taken immediately
              fBeastID := TKMHouseSwineStable(GetHome).FeedBeasts;
              TKMHouseSwineStable(GetHome).TakeBeast(fBeastID);
            end;

            if WorkPlan.ActCount >= fPhase2 then
            begin
              GetHome.CurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
              //Keep unit idling till next Phase, Idle time is -1 to compensate TaskExecution Phase
              SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork-1, ua_Walk);
            end
            else
            begin
              fPhase := SKIP_WORK; //Skip to work complete
              SetActionLockedStay(0, ua_Walk);
              Exit;
            end;
          end;
    10..10 + MAX_WORKPLAN:
          begin
            Inc(fPhase2);

            //Feed a horse/pig
            if (WorkPlan.GatheringScript = gs_HorseBreeder) and (fPhase2 = 1) then
              fBeastID := TKMHouseSwineStable(GetHome).FeedBeasts;

            //Keep on working
            if fPhase2 <= WorkPlan.ActCount then
            begin
              GetHome.CurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
              if fPhase < WorkPlan.ActCount then
                SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork-1, ua_Walk) //-1 to compensate units UpdateState run
              else
                SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork-2, ua_Walk) //-2 to compensate 2 UpdateStates of a unit in last Act
            end
            else
            begin
              fPhase := SKIP_WORK; //Skip to step 31
              SetActionLockedStay(0, ua_Walk);
              Exit;
            end;
          end;
    11 + MAX_WORKPLAN:
          begin
            if WorkPlan.GatheringScript = gs_HorseBreeder then
              TKMHouseSwineStable(GetHome).TakeBeast(fBeastID); //Take the horse after feeding

            case WorkPlan.GatheringScript of
              gs_CoalMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wt_Coal);
              gs_GoldMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wt_GoldOre);
              gs_IronMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wt_IronOre);
              gs_SwineBreeder: ResAcquired := fBeastID <> 0;
              gs_HorseBreeder: ResAcquired := fBeastID <> 0;
              else             ResAcquired := true;
            end;

            if ResAcquired then
            begin
              GetHome.ResAddToOut(WorkPlan.Product1, WorkPlan.ProdCount1);
              GetHome.ResAddToOut(WorkPlan.Product2, WorkPlan.ProdCount2);
              gHands[fUnit.Owner].Stats.WareProduced(WorkPlan.Product1, WorkPlan.ProdCount1);
              gHands[fUnit.Owner].Stats.WareProduced(WorkPlan.Product2, WorkPlan.ProdCount2);
            end;

            GetHome.SetState(hst_Idle);
            SetActionLockedStay(WorkPlan.AfterWorkIdle-1, ua_Walk);
          end;
    else  Result := TaskDone;
  end;
  inc(fPhase);
end;


procedure TTaskMining.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  fWorkPlan.Save(SaveStream);
  SaveStream.Write(fBeastID);
end;


end.
