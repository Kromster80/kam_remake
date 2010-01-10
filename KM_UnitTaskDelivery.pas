unit KM_UnitTaskDelivery;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KromUtils, SysUtils, Math;

type
  TDeliverKind = (dk_House, dk_Unit);

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
      procedure SyncLoad(); override;
      destructor Destroy; override;
      procedure Abandon; override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

implementation
uses KM_PlayersCollection, KM_Terrain, KM_UnitActionWalkTo, KM_SoundFX;


{ TTaskDeliver }
constructor TTaskDeliver.Create(aSerf:TKMUnitSerf; aFrom:TKMHouse; toHouse:TKMHouse; toUnit:TKMUnit; Res:TResourceType; aID:integer);
begin
  Inherited Create(aSerf);
  fTaskName := utn_Deliver;
  fLog.AssertToLog((toHouse=nil)or(toUnit=nil),'Deliver to House AND Unit?');
  if aFrom <> nil then fFrom:=aFrom.GetSelf;
  if toHouse <> nil then fToHouse:=toHouse.GetSelf;
  if toUnit <> nil then fToUnit:=toUnit.GetSelf;
  fResourceType:=Res;
  fDeliverID:=aID;

  if toHouse<>nil then
    DeliverKind:=dk_House;
  if toUnit<>nil then
    DeliverKind:=dk_Unit;

  fUnit.SetActionLockedStay(0,ua_Walk);
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


procedure TTaskDeliver.SyncLoad();
begin
  Inherited;
  fFrom    := fPlayers.GetHouseByID(integer(fFrom));
  fToHouse := fPlayers.GetHouseByID(integer(fToHouse));
  fToUnit  := fPlayers.GetUnitByID(integer(fToUnit));
end;


destructor TTaskDeliver.Destroy;
begin
  if fFrom    <> nil then fFrom.RemovePointer;
  if fToHouse <> nil then fToHouse.RemovePointer;
  if fToUnit  <> nil then fToUnit.RemovePointer;
  Inherited Destroy;
end;

procedure TTaskDeliver.Abandon();
begin
  fPlayers.Player[byte(fUnit.GetOwner)].DeliverList.AbandonDelivery(fDeliverID);
  Inherited;
end;


procedure TTaskDeliver.Execute(out TaskDone:boolean);
var NewDelivery: TUnitTask;
begin
TaskDone:=false;

with fUnit do
case fPhase of
0: if not fFrom.IsDestroyed then
     SetActionWalk(fUnit,KMPointY1(fFrom.GetEntrance))
   else begin
     Abandon;
     TaskDone:=true;
   end;
1: if not fFrom.IsDestroyed then
     SetActionGoIn(ua_Walk,gd_GoInside,fFrom)
   else begin
     Abandon;
     TaskDone:=true;
   end;
2: if not fFrom.IsDestroyed then
   begin
     if fFrom.ResTakeFromOut(fResourceType) then begin
       TKMUnitSerf(fUnit).GiveResource(fResourceType);
       fPlayers.Player[byte(GetOwner)].DeliverList.TakenOffer(fDeliverID);
     end else begin
       fPlayers.Player[byte(GetOwner)].DeliverList.AbandonDelivery(fDeliverID);
       fLog.AssertToLog(false,'Resource''s gone..');
     end;
     SetActionStay(5,ua_Walk); //Wait a moment inside
   end else begin
     PlaceUnitAfterHouseDestroyed; //Unit was invisible while inside. Must show it
     Abandon;
     TaskDone:=true;
   end;
3: if not fFrom.IsDestroyed then
   begin
     SetActionGoIn(ua_Walk,gd_GoOutside,fFrom);
   end else begin
     PlaceUnitAfterHouseDestroyed(); //Unit was invisible while inside. Must show it. Delivery may still continue even though house was just destroyed
     SetActionLockedStay(0,ua_Walk);
   end;
4: if TKMUnitSerf(fUnit).Carry=rt_None then TaskDone:=true else SetActionLockedStay(0,ua_Walk);
end;

//Deliver into complete house
if DeliverKind = dk_House then
  if fToHouse.IsComplete then
  with fUnit do
  case fPhase of
  0..4:;
  5: if not fToHouse.IsDestroyed then
       SetActionWalk(fUnit,KMPointY1(fToHouse.GetEntrance))
     else begin
       TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
       Abandon;
       TaskDone:=true;
     end;
  6: if not fToHouse.IsDestroyed then
       SetActionGoIn(ua_Walk,gd_GoInside,fToHouse)
     else begin
       TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
       Abandon;
       TaskDone:=true;
     end;
  7: SetActionStay(5,ua_Walk);
  8: if not fToHouse.IsDestroyed then
     begin
       fToHouse.ResAddToIn(TKMUnitSerf(fUnit).Carry);
       TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
       fPlayers.Player[byte(GetOwner)].DeliverList.GaveDemand(fDeliverID);
       fPlayers.Player[byte(GetOwner)].DeliverList.AbandonDelivery(fDeliverID);

       //Now look for another delivery from inside this house
       NewDelivery := TKMUnitSerf(fUnit).GetActionFromQueue(fToHouse);
       if NewDelivery <> nil then
       begin
         //Take this new delivery
         NewDelivery.Phase := 2; //Skip to resource-taking part of the new task @Lewin: please confirm it's ok. To be deleted.
         TKMUnitSerf(fUnit).SetNewDelivery(NewDelivery);
         Self.Free; //After setting new unit task we should free self. Note do not set TaskDone:=true as this will affect the new task
         exit;
       end
       else //No delivery found then just step outside
         SetActionGoIn(ua_walk,gd_GoOutside,fToHouse);

     end else begin
       PlaceUnitAfterHouseDestroyed; //Unit was invisible while inside. Must show it
       TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
       Abandon;
       TaskDone:=true;
     end;
  else TaskDone:=true;
  end;

//Deliver into wip house
if DeliverKind = dk_House then
  if not fToHouse.IsComplete then
  if not fToHouse.IsDestroyed then
  begin
    with fUnit do
    case fPhase of
    0..4:;
    5: SetActionWalk(fUnit,KMPointY1(fToHouse.GetEntrance));
    6: begin
         fToHouse.ResAddToBuild(TKMUnitSerf(fUnit).Carry);
         TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
         fPlayers.Player[byte(GetOwner)].DeliverList.GaveDemand(fDeliverID);
         fPlayers.Player[byte(GetOwner)].DeliverList.AbandonDelivery(fDeliverID);
         SetActionStay(1,ua_Walk);
       end;
    else TaskDone:=true;
    end;
  end else begin
    TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
    Abandon;
    TaskDone:=true;
  end;

//Deliver to builder
if DeliverKind = dk_Unit then
with fUnit do
case fPhase of
0..4:;
5: if (fToUnit<>nil)and(fToUnit.GetUnitTask<>nil)and(not fToUnit.IsDead) then
     SetActionWalk(fUnit, fToUnit.GetPosition, KMPoint(0,0), ua_Walk, false)
   else
   begin
     TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
     fPlayers.Player[byte(GetOwner)].DeliverList.GaveDemand(fDeliverID);
     fPlayers.Player[byte(GetOwner)].DeliverList.AbandonDelivery(fDeliverID);
     TaskDone:=true;
   end;
6: begin
      TKMUnitSerf(fUnit).TakeResource(TKMUnitSerf(fUnit).Carry);
      if (fToUnit<>nil)and(fToUnit.GetUnitTask<>nil)and(not fToUnit.IsDead)and(not(fToUnit.GetUnitTask is TTaskDie)) then begin
        fToUnit.GetUnitTask.Phase := fToUnit.GetUnitTask.Phase + 1;
        fToUnit.SetActionStay(0,ua_Work1);
      end;
      fPlayers.Player[byte(GetOwner)].DeliverList.GaveDemand(fDeliverID);
      fPlayers.Player[byte(GetOwner)].DeliverList.AbandonDelivery(fDeliverID);
      SetActionStay(1,ua_Walk);
   end;
else TaskDone:=true;
end;

if TaskDone then exit;
inc(fPhase);
if fUnit.GetUnitAction=nil then
  fLog.AssertToLog(false,'fSerf.fCurrentAction=nil)and(not TaskDone)');
end;


procedure TTaskDeliver.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fFrom <> nil then
    SaveStream.Write(fFrom.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  if fToHouse <> nil then
    SaveStream.Write(fToHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  if fToUnit <> nil then
    SaveStream.Write(fToUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fResourceType, SizeOf(fResourceType));
  SaveStream.Write(fDeliverID);
  SaveStream.Write(DeliverKind, SizeOf(DeliverKind));
end;


end.
