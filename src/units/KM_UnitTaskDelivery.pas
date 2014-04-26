unit KM_UnitTaskDelivery;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Units, KM_ResWares;


type
  TDeliverKind = (dk_ToHouse, dk_ToConstruction, dk_ToUnit);

  TTaskDeliver = class(TUnitTask)
  private
    fFrom: TKMHouse;
    fToHouse: TKMHouse;
    fToUnit: TKMUnit;
    fWareType: TWareType;
    fDeliverID: Integer;
    fDeliverKind: TDeliverKind;
    procedure CheckForBetterDestination;
  public
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; toHouse: TKMHouse; Res: TWareType; aID: Integer); overload;
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; toUnit: TKMUnit; Res: TWareType; aID: Integer); overload;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    property DeliverKind: TDeliverKind read fDeliverKind;
    function Execute: TTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses KM_HandsCollection, KM_Units_Warrior, KM_Log, KM_HouseBarracks, KM_Hand;


{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; toHouse: TKMHouse; Res: TWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fTaskName := utn_Deliver;

  Assert((aFrom <> nil) and (toHouse <> nil) and (Res <> wt_None), 'Serf ' + IntToStr(fUnit.UID) + ': invalid delivery task');

  if WRITE_DELIVERY_LOG then
    gLog.AddTime('Serf ' + IntToStr(fUnit.UID) + ' created delivery task ' + IntToStr(fDeliverID));

  fFrom    := aFrom.GetHousePointer;
  fToHouse := toHouse.GetHousePointer;
  //Check it once to begin with as the house could become complete before the task exits (in rare circumstances when the task
  // does not exit until long after the ware has been delivered due to walk interactions)
  if toHouse.IsComplete then
    fDeliverKind := dk_ToHouse
  else
    fDeliverKind := dk_ToConstruction;

  fWareType   := Res;
  fDeliverID  := aID;
end;


constructor TTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; toUnit: TKMUnit; Res: TWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fTaskName := utn_Deliver;

  Assert((aFrom<>nil) and (toUnit<>nil) and ((toUnit is TKMUnitWarrior) or (toUnit is TKMUnitWorker)) and (Res <> wt_None), 'Serf '+inttostr(fUnit.UID)+': invalid delivery task');
  if WRITE_DELIVERY_LOG then gLog.AddTime('Serf '+inttostr(fUnit.UID)+' created delivery task '+inttostr(fDeliverID));

  fFrom    := aFrom.GetHousePointer;
  fToUnit  := toUnit.GetUnitPointer;
  fDeliverKind := dk_ToUnit;
  fWareType := Res;
  fDeliverID    := aID;
end;


constructor TTaskDeliver.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fFrom, 4);
  LoadStream.Read(fToHouse, 4);
  LoadStream.Read(fToUnit, 4);
  LoadStream.Read(fWareType, SizeOf(fWareType));
  LoadStream.Read(fDeliverID);
  LoadStream.Read(fDeliverKind, SizeOf(fDeliverKind));
end;


procedure TTaskDeliver.SyncLoad;
begin
  inherited;
  fFrom    := gHands.GetHouseByUID(Cardinal(fFrom));
  fToHouse := gHands.GetHouseByUID(Cardinal(fToHouse));
  fToUnit  := gHands.GetUnitByUID(Cardinal(fToUnit));
end;


destructor TTaskDeliver.Destroy;
begin
  if WRITE_DELIVERY_LOG then gLog.AddTime('Serf '+inttostr(fUnit.UID)+' abandoned delivery task '+inttostr(fDeliverID)+' at phase ' + inttostr(fPhase));

  if fDeliverID <> 0 then
    gHands[fUnit.Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);

  if TKMUnitSerf(fUnit).Carry <> wt_None then
  begin
    gHands[fUnit.Owner].Stats.WareConsumed(TKMUnitSerf(fUnit).Carry);
    TKMUnitSerf(fUnit).CarryTake; //empty hands
  end;

  gHands.CleanUpHousePointer(fFrom);
  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpUnitPointer(fToUnit);
  inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TTaskDeliver.WalkShouldAbandon: Boolean;
begin
  Result := false;

  //After step 2 we don't care if From is destroyed or doesn't have the ware
  if fPhase <= 2 then
    Result := Result or fFrom.IsDestroyed or not fFrom.ResOutputAvailable(fWareType, 1);

  //Until we implement "wares recycling" we just abandon the delivery if target is destroyed/dead
  if (fDeliverKind = dk_ToHouse) and (fPhase <= 8) then
    Result := Result or fToHouse.IsDestroyed;

  if (fDeliverKind = dk_ToConstruction) and (fPhase <= 6) then
    Result := Result or fToHouse.IsDestroyed;

  if (fDeliverKind = dk_ToUnit) and (fPhase <= 6) then
    Result := Result or (fToUnit=nil) or fToUnit.IsDeadOrDying;
end;


procedure TTaskDeliver.CheckForBetterDestination;
var
  NewToHouse: TKMHouse;
  NewToUnit: TKMUnit;
begin
  gHands[fUnit.Owner].Deliveries.Queue.CheckForBetterDemand(fDeliverID, NewToHouse, NewToUnit);

  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpUnitPointer(fToUnit);
  if NewToHouse <> nil then
  begin
    fToHouse := NewToHouse.GetHousePointer;
    if fToHouse.IsComplete then
      fDeliverKind := dk_ToHouse
    else
      fDeliverKind := dk_ToConstruction;
  end
  else
  begin
    fToUnit := NewToUnit.GetUnitPointer;
    fDeliverKind := dk_ToUnit;
  end;
end;


function TTaskDeliver.Execute:TTaskResult;
begin
  Result := TaskContinues;

  if WalkShouldAbandon and fUnit.Visible then
  begin
    Result := TaskDone;
    Exit;
  end;

  with TKMUnitSerf(fUnit) do
  case fPhase of
    0:  begin
          SetActionWalkToSpot(KMPointBelow(fFrom.GetEntrance));
        end;
    1:  begin
          SetActionGoIn(ua_Walk, gd_GoInside, fFrom);
        end;
    2:  begin
          //Barracks can consume the resource (by equipping) before we arrive
          //All houses can have resources taken away by script at any moment
          if not fFrom.ResOutputAvailable(fWareType, 1) then
          begin
            SetActionGoIn(ua_Walk, gd_GoOutside, fFrom); //Step back out
            fPhase := 99; //Exit next run
            Exit;
          end;
          SetActionLockedStay(5,ua_Walk); //Wait a moment inside
          fFrom.ResTakeFromOut(fWareType);
          CarryGive(fWareType);
          CheckForBetterDestination; //Must run before TakenOffer so Offer is still valid
          gHands[Owner].Deliveries.Queue.TakenOffer(fDeliverID);
        end;
    3:  if fFrom.IsDestroyed then //We have the resource, so we don't care if house is destroyed
          SetActionLockedStay(0, ua_Walk)
        else
          SetActionGoIn(ua_Walk, gd_GoOutside, fFrom);
    4:  SetActionLockedStay(0, ua_Walk); //Thats a placeholder left for no obvious reason
  end;

  //Deliver into complete house
  if (fDeliverKind = dk_ToHouse) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
    5:  SetActionWalkToSpot(KMPointBelow(fToHouse.GetEntrance));
    6:  SetActionGoIn(ua_Walk, gd_GoInside, fToHouse);
    7:  SetActionLockedStay(5, ua_Walk); //wait a bit inside
    8:  begin
          fToHouse.ResAddToIn(Carry);
          CarryTake;

          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while trying to GoOut

          //Now look for another delivery from inside this house
          if TKMUnitSerf(fUnit).TryDeliverFrom(fToHouse) then
          begin
            //After setting new unit task we should free self.
            //Note do not set TaskDone:=true as this will affect the new task
            Self.Free;
            Exit;
          end else
            //No delivery found then just step outside
            SetActionGoIn(ua_Walk, gd_GoOutside, fToHouse);
        end;
    else Result := TaskDone;
  end;

  //Deliver into wip house
  if (fDeliverKind = dk_ToConstruction) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
    //It's the only place in KaM that used to access houses entrance from diagonals. Supposably it
    //was made similar to workers - tile was declared  "Under construction" and could accept wares
    //from any side, or something alike. Removing of Distance=1 from here simplifies our WalkToSpot method.
    //Since this change some people have complained because it's hard for serfs to get wares to the site
    //when workers block the enterance. But it is much simpler this way so we don't have a problem really.
    5:  SetActionWalkToSpot(KMPointBelow(fToHouse.GetEntrance));
    6:  begin
          Direction := KMGetDirection(GetPosition, fToHouse.GetEntrance);
          fToHouse.ResAddToBuild(Carry);
          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1, ua_Walk);
        end;
    else Result := TaskDone;
  end;

  //Deliver to builder or soldier
  if fDeliverKind = dk_ToUnit then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
    5:  SetActionWalkToUnit(fToUnit, 1.42, ua_Walk); //When approaching from diagonal
    6:  begin
          //See if the unit has moved. If so we must try again
          if KMLengthDiag(fUnit.GetPosition, fToUnit.GetPosition) > 1.5 then
          begin
            SetActionWalkToUnit(fToUnit, 1.42, ua_Walk); //Walk to unit again
            fPhase := 6;
            Exit;
          end;
          //Worker
          if (fToUnit.UnitType = ut_Worker) and (fToUnit.UnitTask <> nil) then
          begin
            fToUnit.UnitTask.Phase := fToUnit.UnitTask.Phase + 1;
            fToUnit.SetActionLockedStay(0, ua_Work1); //Tell the worker to resume work by resetting his action (causes task to execute)
          end;
          //Warrior
          if (fToUnit is TKMUnitWarrior) then
          begin
            fToUnit.Feed(UNIT_MAX_CONDITION); //Feed the warrior
            TKMUnitWarrior(fToUnit).RequestedFood := False;
          end;
          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while staying
          SetActionLockedStay(5, ua_Walk); //Pause breifly (like we are handing over the ware/food)
        end;
    7:  begin
          //After feeding troops, serf should walk away, but ToUnit could be dead by now
          if (fToUnit is TKMUnitWarrior) then
          begin
            if TKMUnitSerf(fUnit).TryDeliverFrom(nil) then
            begin
              //After setting new unit task we should free self.
              //Note do not set TaskDone:=true as this will affect the new task
              Self.Free;
              Exit;
            end else
              //No delivery found then just walk back to our From house
              //even if it's destroyed, its location is still valid
              //Don't walk to spot as it doesn't really matter
              SetActionWalkToHouse(fFrom, 5);
          end else
            SetActionStay(0, ua_Walk); //If we're not feeding a warrior then ignore this step
        end;
    else Result := TaskDone;
  end;

  Inc(fPhase);
end;


procedure TTaskDeliver.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fFrom <> nil then
    SaveStream.Write(fFrom.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  if fToHouse <> nil then
    SaveStream.Write(fToHouse.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  if fToUnit <> nil then
    SaveStream.Write(fToUnit.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fWareType, SizeOf(fWareType));
  SaveStream.Write(fDeliverID);
  SaveStream.Write(fDeliverKind, SizeOf(fDeliverKind));
end;


end.
