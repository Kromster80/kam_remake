unit KM_UnitTaskDelivery;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Houses, KM_Units, SysUtils, KM_Points;

type
  TDeliverKind = (dk_ToHouse, dk_ToUnit);

{Perform delivery}
type
  TTaskDeliver = class(TUnitTask)
    private
      fFrom:TKMHouse;
      fToHouse:TKMHouse;
      fToUnit:TKMUnit;
      fResourceType:TResourceType;
      fDeliverID:integer;
    public
      DeliverKind:TDeliverKind;
      constructor Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad; override;
      destructor Destroy; override;
      function WalkShouldAbandon:boolean; override;
      function Execute:TTaskResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

implementation
uses KM_PlayersCollection, KM_Units_Warrior, KM_Log, KM_TextLibrary;


{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
begin
  Inherited Create(aSerf);
  fTaskName := utn_Deliver;

  if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' created delivery task '+inttostr(fDeliverID));
  Assert((toHouse=nil)or(toUnit=nil),'Serf '+inttostr(fUnit.ID)+' deliver to House AND Unit?');

  if aFrom   <> nil then fFrom    := aFrom.GetHousePointer;
  if toHouse <> nil then fToHouse := toHouse.GetHousePointer;
  if toUnit  <> nil then fToUnit  := toUnit.GetUnitPointer;
  if toHouse <> nil then DeliverKind := dk_ToHouse; //It's easier to check this than toHouse<>nil
  if toUnit  <> nil then DeliverKind := dk_ToUnit;

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
  LoadStream.Read(DeliverKind, SizeOf(DeliverKind));
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
  TKMUnitSerf(fUnit).CarryTake(false); //empty hands

  fPlayers.CleanUpHousePointer(fFrom);
  fPlayers.CleanUpHousePointer(fToHouse);
  fPlayers.CleanUpUnitPointer(fToUnit);
  Inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TTaskDeliver.WalkShouldAbandon:boolean;
begin
  Result := false;
  if (fPhase in [0,1,2]) then Result := fFrom.IsDestroyed;

  if (DeliverKind = dk_ToHouse) and fToHouse.IsComplete then
  if (fPhase in [5,6,7,8]) then Result := fToHouse.IsDestroyed;

  if (DeliverKind = dk_ToHouse) and not fToHouse.IsComplete then
  if (fPhase in [5,6]) then Result := fToHouse.IsDestroyed;

  if (DeliverKind = dk_ToUnit) then
  if (fPhase in [5,6]) then Result := (fToUnit=nil) or fToUnit.IsDeadOrDying;
end;


function TTaskDeliver.Execute:TTaskResult;
var NewDelivery: TUnitTask;
begin
  Result := TaskContinues;

  if WalkShouldAbandon then begin
    Result := TaskDone;
    exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' going to take '+TypeToString(fResourceType)+' from '+KM_Points.TypeToString(GetPosition));
          SetActionWalkToSpot(KMPointBelow(fFrom.GetEntrance));
        end;
    1:  begin
          if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' taking '+TypeToString(fResourceType)+' from '+KM_Points.TypeToString(GetPosition));
          SetActionGoIn(ua_Walk,gd_GoInside,fFrom)
        end;
    2:  begin
          if WRITE_DELIVERY_LOG then fLog.AppendLog('Serf '+inttostr(fUnit.ID)+' taking '+TypeToString(fResourceType)+' from '+KM_Points.TypeToString(GetPosition));
          SetActionStay(5,ua_Walk); //Wait a moment inside
          fFrom.ResTakeFromOut(fResourceType);
          TKMUnitSerf(fUnit).CarryGive(fResourceType);
          fPlayers.Player[GetOwner].DeliverList.TakenOffer(fDeliverID);
        end;
    3:  if fFrom.IsDestroyed then //We have the resource, so we don't care if house is destroyed
          SetActionLockedStay(0,ua_Walk)
        else
          SetActionGoIn(ua_Walk,gd_GoOutside,fFrom);
    4:  SetActionLockedStay(0,ua_Walk); //thats a placeholder?
  end;

  //Deliver into complete house
  if (DeliverKind = dk_ToHouse) and fToHouse.IsComplete then
  with fUnit do
  case fPhase of
    0..4:;
    5:  SetActionWalkToSpot(KMPointBelow(fToHouse.GetEntrance));
    6:  SetActionGoIn(ua_Walk,gd_GoInside,fToHouse);
    7:  SetActionStay(5,ua_Walk); //wait a bit inside
    8:  begin
          fToHouse.ResAddToIn(TKMUnitSerf(fUnit).Carry);
          TKMUnitSerf(fUnit).CarryTake;

          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while trying to GoOut

          //Now look for another delivery from inside this house
          NewDelivery := TKMUnitSerf(fUnit).GetActionFromQueue(fToHouse);
          if NewDelivery <> nil then
          begin //Take this new delivery
            NewDelivery.Phase := 2; //Skip to resource-taking part of the new task
            TKMUnitSerf(fUnit).SetNewDelivery(NewDelivery);
            Self.Free; //After setting new unit task we should free self. Note do not set TaskDone:=true as this will affect the new task
            exit;
          end else //No delivery found then just step outside
            SetActionGoIn(ua_Walk,gd_GoOutside,fToHouse);
        end;
    else Result := TaskDone;
  end;

  //Deliver into wip house
  if (DeliverKind = dk_ToHouse) and not fToHouse.IsComplete then
  with fUnit do
  case fPhase of
    0..4:;
    5:  SetActionWalkToSpot(fToHouse.GetEntrance,1); 
    6:  begin
          fToHouse.ResAddToBuild(TKMUnitSerf(fUnit).Carry);
          TKMUnitSerf(fUnit).CarryTake;
          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1,ua_Walk);
        end;
    else Result := TaskDone;
  end;

  //Deliver to builder or soldier
  if DeliverKind = dk_ToUnit then
  with fUnit do
  case fPhase of
    0..4:;
    5:  SetActionWalkToUnit(fToUnit, 1.42, ua_Walk); //When approaching from diagonal
    6:  begin
          //See if the unit has moved. If so we must try again
          if KMLength(fUnit.GetPosition,fToUnit.GetPosition) > 1.5 then
          begin
            fPhase := 5; //Walk to unit again
            SetActionStay(0,ua_Walk);
            exit;
          end;
          //Worker
          if (fToUnit.UnitType = ut_Worker)and(fToUnit.GetUnitTask<>nil) then
          begin
            fToUnit.GetUnitTask.Phase := fToUnit.GetUnitTask.Phase + 1;
            fToUnit.SetActionLockedStay(0,ua_Work1); //Tell the worker to resume work by resetting his action (causes task to execute)
          end;
          //Warrior
          if (fToUnit is TKMUnitWarrior) then
          begin
            fToUnit.SetFullCondition; //Feed the warrior
            TKMUnitWarrior(fToUnit).RequestedFood := false;
          end;
          TKMUnitSerf(fUnit).CarryTake;
          fPlayers.Player[GetOwner].DeliverList.GaveDemand(fDeliverID);
          fPlayers.Player[GetOwner].DeliverList.AbandonDelivery(fDeliverID);
          fDeliverID := 0; //So that it can't be abandoned if unit dies while staying
          SetActionLockedStay(5, ua_Walk); //Pause breifly (like we are handing over the goods)
        end;
    7:  begin
          //After feeding troops, ask for new delivery and if there is none, walk back to the place we came from so we don't leave serfs all over the battlefield
          if (fToUnit <> nil) and (fToUnit is TKMUnitWarrior) then
          begin
            NewDelivery := TKMUnitSerf(fUnit).GetActionFromQueue;
            if NewDelivery <> nil then begin
              TKMUnitSerf(fUnit).SetNewDelivery(NewDelivery);
              Self.Free; //After setting new unit task we should free self. Note do not set TaskDone:=true as this will affect the new task
              exit;
            end else //No delivery found then just walk back to our from house
              SetActionWalkToSpot(KMPointBelow(fFrom.GetEntrance),5); //Don't walk to spot as it doesn't really matter
          end else
            SetActionStay(0,ua_Walk); //If we're not feeding a warrior then ignore this step
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
  SaveStream.Write(DeliverKind, SizeOf(DeliverKind));
end;


end.
