unit KM_UnitTaskAttackHouse;
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KromUtils, SysUtils;


{Attack a house}
type
    TTaskAttackHouse = class(TUnitTask)
    private
      fHouse:TKMHouse;
    public
      constructor Create(aWarrior: TKMUnit; aHouse:TKMHouse);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      procedure Abandon; override;
      function WalkShouldAbandon:boolean; override;
      procedure Execute(out TaskDone:boolean); override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;

implementation
uses KM_PlayersCollection, KM_Units_Warrior, KM_UnitActionWalkTo;


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnit; aHouse:TKMHouse);
begin
  Inherited Create(aWarrior);
  if aHouse <> nil then fHouse:=aHouse.GetSelf;
  fUnit.SetActionLockedStay(0,ua_Walk);
end;


constructor TTaskAttackHouse.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
end;


procedure TTaskAttackHouse.SyncLoad();
begin
  Inherited;
  fHouse    := fPlayers.GetHouseByID(cardinal(fHouse));
end;


destructor TTaskAttackHouse.Destroy;
begin
  if fHouse <> nil then fHouse.RemovePointer;
  Inherited Destroy;
end;


procedure TTaskAttackHouse.Abandon();
begin
  Inherited;
end;


function TTaskAttackHouse.WalkShouldAbandon:boolean;
begin
  Result := fHouse.IsDestroyed; //Stop walking if the house has been destroyed already
end;


procedure TTaskAttackHouse.Execute(out TaskDone:boolean);
begin
TaskDone:=false;

with fUnit do
case fPhase of
0: if not fHouse.IsDestroyed then
   begin
     //Choose location and walk to it (will be different if we are a ranged unit)
   end
   else
   begin
     Abandon;
     TaskDone:=true;
   end;
1: if not fHouse.IsDestroyed then
   begin
     //Hit/shoot the house (possibly using Fight action modified to be in house rather than unit mode? Should be pretty much the same otherwise...
   end
   else
   begin
     Abandon;
     TaskDone:=true;
   end;
   //...anything else?
end;

  if TaskDone then exit;
  inc(fPhase);
  if fUnit.GetUnitAction=nil then
    fLog.AssertToLog(false,'fSerf.fCurrentAction=nil)and(not TaskDone)');
end;


procedure TTaskAttackHouse.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fHouse, 4);
end;


end.
