unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KM_Units_Warrior, KromUtils, SysUtils;

{Attack a house}
type
  TTaskAttackHouse = class(TUnitTask)
    private
      fHouse:TKMHouse;
      fDestroyingHouse:boolean;
      fFightType:TFightType;
      LocID:byte; //Current attack location
      Cells:TKMPointDirList; //List of surrounding cells and directions
      function PosUsed(aPos: TKMPoint):boolean;
    public
      constructor Create(aWarrior: TKMUnitWarrior; aHouse:TKMHouse);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      property DestroyingHouse:boolean read fDestroyingHouse;
      function WalkShouldAbandon:boolean; override;
      function Execute():TTaskResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Terrain;


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnitWarrior; aHouse:TKMHouse);
begin
  Inherited Create(aWarrior);
  fTaskName := utn_AttackHouse;
  fHouse := aHouse.GetHousePointer;
  fDestroyingHouse := false;
  fFightType := aWarrior.GetFightType;
  LocID  := 0;
  Cells  := TKMPointDirList.Create; //Pass pre-made list to make sure we Free it in the same unit
  case fFightType of
    ft_Melee: fHouse.GetListOfCellsAround(Cells, aWarrior.GetDesiredPassability);
    ft_Ranged: fHouse.GetListOfCellsWithin(Cells);
    else Assert(false, 'Unknown FightType');
  end;
end;


constructor TTaskAttackHouse.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fDestroyingHouse);
  LoadStream.Read(LocID);
  Cells := TKMPointDirList.Create;
  Cells.Load(LoadStream);
end;


procedure TTaskAttackHouse.SyncLoad();
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(cardinal(fHouse));
end;


destructor TTaskAttackHouse.Destroy;
begin
  if fHouse <> nil then fHouse.ReleaseHousePointer;
  FreeAndNil(Cells);
  Inherited;
end;


{Position is [used] when there's another warrior attacking the same house from queried spot?}
function TTaskAttackHouse.PosUsed(aPos: TKMPoint):boolean;
var HitUnit: TKMUnit;
begin
  HitUnit := fPlayers.UnitsHitTest(aPos.X,aPos.Y);
  Result :=
    (HitUnit <> nil)and //Unit below
    (HitUnit.GetUnitTask is TTaskAttackHouse)and //Attacking the same house
    (TTaskAttackHouse(HitUnit.GetUnitTask).LocID <> 0)and //Attack is in progress
    (KMSamePoint(TTaskAttackHouse(HitUnit.GetUnitTask).Cells.List[TTaskAttackHouse(HitUnit.GetUnitTask).LocID].Loc,aPos)); //Attacks from the same spot that we want to use
end;



function TTaskAttackHouse.WalkShouldAbandon:boolean;
begin
  Result := false;

  if fHouse.IsDestroyed then begin
    Result := true;
    exit;
  end;

  if PosUsed(Cells.List[LocID].Loc) then
  begin
    Result := true;
    fPhase := 0; //Try to start again with a new spot
  end;
end;


function TTaskAttackHouse.Execute():TTaskResult;

    function PickRandomSpot(): byte;
    var i, MyCount: integer; Spots: array[1..16] of byte;
    begin
      MyCount := 0;
      for i:=1 to Cells.Count do
      if not PosUsed(Cells.List[i].Loc) then //Is someone else is using it
      if fTerrain.Route_CanBeMade(fUnit.GetPosition, Cells.List[i].Loc ,fUnit.GetDesiredPassability, 0) then
      begin
        inc(MyCount);
        Spots[MyCount] := i;
        //ALWAYS choose our current location if it is available to save walking
        if KMSamePoint(Cells.List[i].Loc,fUnit.GetPosition) then
        begin
          Result := i;
          exit;
        end;
      end;
      if MyCount > 0 then
        Result := Spots[Random(MyCount)+1]
      else
        Result := 0;
    end;

begin
  Result := TaskContinues;

  //If the house is destroyed drop the task
  if fHouse.IsDestroyed then
  begin
    Result := TaskDone;
    exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         if fFightType=ft_Ranged then begin
           SetActionWalk(fHouse.GetPosition, fHouse, 8);
         end else begin
           LocID := PickRandomSpot(); //Choose random location around the house and walk to it
           if LocID = 0 then
           begin
             //All cells are taken/inaccessable
             Result := TaskDone;
             exit;
           end;
           SetActionWalk(Cells.List[LocID].Loc);
           fDestroyingHouse := false;
         end;
       end;
    1: if fFightType=ft_Ranged then begin
         
         SetActionStay(Random(8),ua_Work,true); //Pretend to aim
         Direction := KMGetDirection(GetPosition, fHouse.GetEntrance); //Look at house
       end else
         SetActionLockedStay(0,ua_Work,false); //@Lewin: Maybe melee units can randomly pause for 1-2 frames as well?
    2: begin
         if fFightType=ft_Ranged then begin
           SetActionLockedStay(4,ua_Work,false,0,0); //Start shooting
           fDestroyingHouse := true;
         end else begin
           SetActionLockedStay(6,ua_Work,false,0,0); //Start the hit
           Direction := TKMDirection(Cells.List[LocID].Dir); //Face target
           fDestroyingHouse := true;
         end;
       end;
    3: begin
         if fFightType=ft_Ranged then begin //Launch the missile and forget about it
           //Shooting range is not important now, houses don't walk (except Howl's Moving Castle perhaps)
           fGame.fProjectiles.AddItem(PositionF, KMPointF(Cells.GetRandom.Loc), pt_Arrow); //Release arrow/bolt
           SetActionLockedStay(24,ua_Work,false,0,4); //Reload for next attack
           //Bowmen/crossbowmen do 1 damage per shot and occasionally miss altogether
           fPhase := 0; //Go for another shot (will be 1 after inc below)
         end else begin
           SetActionLockedStay(6,ua_Work,false,0,6); //Pause for next attack
           fHouse.AddDamage(2); //All melee units do 2 damage per strike
           fPhase := 1; //Go for another hit (will be 2 after inc below)
         end;
       end;
  end;

  inc(fPhase);
end;


procedure TTaskAttackHouse.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fDestroyingHouse);
  SaveStream.Write(LocID);
  Cells.Save(SaveStream);
end;


end.
