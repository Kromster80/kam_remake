unit KM_UnitTaskDelivery;
{$I KaM_Remake.inc}
interface
uses Classes,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_Terrain, KM_Units, SysUtils, KM_Points;


type
  TDeliverKind = (dk_ToHouse, dk_ToConstruction, dk_ToUnit);


  {Perform delivery}
  TTaskDeliver = class(TUnitTask)
  private
    fFrom:TKMHouse;
    fToHouse:TKMHouse;
    fToUnit:TKMUnit;
    fResourceType:TResourceType;
    fDeliverID:integer;
    fDeliverKind:TDeliverKind;
  public
    constructor Create(aSerf:TKMUnitSerf; aTerrain: TTerrain; aFrom:TKMHouse; toHouse:TKMHouse; Res:TResourceType; aID:integer); overload;
    constructor Create(aSerf:TKMUnitSerf; aTerrain: TTerrain; aFrom:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer); overload;
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon:boolean; override;
    property DeliverKind: TDeliverKind read fDeliverKind;
    function Execute:TTaskResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Units_Warrior, KM_Log;


{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aTerrain: TTerrain; aFrom:TKMHouse; toHouse:TKMHouse; Res:TResourceType; aID:integer);
begin
  Inherited Create(aSerf, aTerrain);
  fTaskName := utn_Deliver;

  Assert((aFrom<>nil) and (toHouse<>nil) and (Res <> rt_None), 'Serf '+inttostr(fUnit.ID)+': invalid delivery task');
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' created delivery task '+inttostr(fDeliverID));

  fFrom    := aFrom.GetHousePointer;
  fToHouse := toHouse.GetHousePointer;
  //Check it once to begin with as the house could become complete before the task exits (in rare circumstances when the task
  // does not exit until long after the ware has been delivered due to walk interactions)
  if toHouse.IsComplete then
    fDeliverKind := dk_ToHouse
  else
    fDeliverKind := dk_ToConstruction;
  fResourceType := Res;
  fDeliverID    := aID;
end;


constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aTerrain: TTerrain; aFrom:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
begin
  Inherited Create(aSerf, aTerrain);
  fTaskName := utn_Deliver;

  Assert((aFrom<>nil) and (toUnit<>nil) and ((toUnit is TKMUnitWarrior) or (toUnit is TKMUnitWorker)) and (Res <> rt_None), 'Serf '+inttostr(fUnit.ID)+': invalid delivery task');
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' created delivery task '+inttostr(fDeliverID));

  fFrom    := aFrom.GetHousePointer;
  fToUnit  := toUnit.GetUnitPointer;
  fDeliverKind := dk_ToUnit;
  fResourceType := Res;
  fDeliverID    := aID;
end;


constructor TTaskDeliver.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fFrom, 4);
  LoadStream.Read(fToHouse, 4);
  LoadStream.Read(fToUnit, 4);
  LoadStream.Read(fResourceType, SizeOf(fResourceType));
  LoadStream.Read(fDeliverID);
  LoadStream.Read(fDeliverKind, SizeOf(fDeliverKind));
end;


procedure TTaskDeliver.SyncLoad;
begin
  Inherited;
  fFrom    := fPlayers.GetHouseByID(cardinal(fFrom));
  fToHouse := fPlayers.GetHouseByID(cardinal(fToHouse));
  fToUnit  := fPlayers.GetUnitByID(cardinal(fToUnit));
end;


destructor TTaskDeliver.Destroy;
begin
  if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' abandoned delivery task '+inttostr(fDeliverID)+' at phase ' + inttostr(fPhase));

  if fDeliverID<>0 then fPlayers.Player[fUnit.GetOwner].DeliverList.AbandonDelivery(fDeliverID);
  if TKMUnitSerf(fUnit).Carry <> rt_None then TKMUnitSerf(fUnit).CarryTake; //empty hands

  fPlayers.CleanUpHousePointer(fFrom);
  fPlayers.CleanUpHousePointer(fToHouse);
  fPlayers.CleanUpUnitPointer(fToUnit);
  Inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TTaskDeliver.WalkShouldAbandon:boolean;
begin
  Result := false;

  //After step 2 we don't care if From is destroyed
  if fPhase <= 2 then
    Result := Result or fFrom.IsDestroyed;

  //Until we implement "goods recycling" we just abandon the delivery if target is destroyed/dead
  if (fDeliverKind = dk_ToHouse) and (fPhase <= 8) then
    Result := Result or fToHouse.IsDestroyed;

  if (fDeliverKind = dk_ToConstruction) and (fPhase <= 6) then
    Result := Result or fToHouse.IsDestroyed;

  if (fDeliverKind = dk_ToUnit) and (fPhase <= 6) then
    Result := Result or (fToUnit=nil) or fToUnit.IsDeadOrDying;
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
          SetActionGoIn(ua_Walk,gd_GoInside,fFrom);
        end;
    2:  begin
          SetActionStay(5,ua_Walk); //Wait a moment inside
          fFrom.ResTakeFromOut(fResourceType);
          CarryGive(fResourceType);
          fPlayers.Player[GetOwner].DeliverList.TakenOffer(fDeliverID);
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
    7:  SetActionStay(5, ua_Walk); //wait a bit inside
    8:  begin
          fToHouse.ResAddToIn(Carry);
          CarryTake;

          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
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
    //@Lewin: It's the only place in KaM that used to access houses entrance from diagonals, right?
    //        I suspect it was made similar to workers - tile was declared  "Under construction" and
    //        could accept goods from any side, or something alike. 
    //        I have removed this Distance=1 from here because it simplifies our WalkToSpot method.
    //        Please give me your thoughts on this case - maybe we need to use Distance=n
    //        in some other occasions of WalkToSpot too?
    //@Krom: Since this change some people have complained because it's hard for serfs to get wares to the site
    //       when workers block the enterance. But it is much simpler this way so I don't have a problem really.
    5:  SetActionWalkToSpot(KMPointBelow(fToHouse.GetEntrance));
    6:  begin
          fToHouse.ResAddToBuild(Carry);
          CarryTake;
          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
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
          if KMLength(fUnit.GetPosition,fToUnit.GetPosition) > 1.5 then
          begin
            SetActionWalkToUnit(fToUnit, 1.42, ua_Walk); //Walk to unit again
            fPhase := 6;
            Exit;
          end;
          //Worker
          if (fToUnit.UnitType = ut_Worker) and (fToUnit.GetUnitTask<>nil) then
          begin
            fToUnit.GetUnitTask.Phase := fToUnit.GetUnitTask.Phase + 1;
            fToUnit.SetActionLockedStay(0, ua_Work1); //Tell the worker to resume work by resetting his action (causes task to execute)
          end;
          //Warrior
          if (fToUnit is TKMUnitWarrior) then
          begin
            fToUnit.SetFullCondition; //Feed the warrior
            TKMUnitWarrior(fToUnit).RequestedFood := false;
          end;
          CarryTake;
          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while staying
          SetActionLockedStay(5, ua_Walk); //Pause breifly (like we are handing over the goods)
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

  inc(fPhase);
end;


procedure TTaskDeliver.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fFrom <> nil then
    SaveStream.Write(fFrom.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  if fToHouse <> nil then
    SaveStream.Write(fToHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  if fToUnit <> nil then
    SaveStream.Write(fToUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fResourceType, SizeOf(fResourceType));
  SaveStream.Write(fDeliverID);
  SaveStream.Write(fDeliverKind, SizeOf(fDeliverKind));
end;


end.
