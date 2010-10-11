unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Utils, KM_Houses, KM_Units, KromUtils, SysUtils;


{Attack a house}
type
    TTaskAttackHouse = class(TUnitTask)
    private
      fHouse:TKMHouse;
      LocID:byte; //Current attack location
      Cells:TKMPointDirList; //List of surrounding cells and directions
      function PosUsed(aPos: TKMPoint):boolean;
    public
      constructor Create(aWarrior: TKMUnit; aHouse:TKMHouse);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      function WalkShouldAbandon:boolean; override;
      function Execute():TTaskResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Terrain;


{ TTaskAttackHouse }
constructor TTaskAttackHouse.Create(aWarrior: TKMUnit; aHouse:TKMHouse);
begin
  Inherited Create(aWarrior);
  fTaskName := utn_AttackHouse;

  fLog.AssertToLog(aHouse<>nil, 'Trying to attack NIL house?');
  fHouse := aHouse.GetHousePointer;
  LocID  := 0;
  Cells  := TKMPointDirList.Create; //Pass pre-made list to make sure we Free it in the same unit
  fHouse.GetListOfCellsAround(Cells, aWarrior.GetDesiredPassability);
end;


constructor TTaskAttackHouse.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
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
      if fTerrain.TileInMapCoords(Cells.List[i].Loc.X,Cells.List[i].Loc.Y) and (not PosUsed(Cells.List[i].Loc)) then //Is someone else is using it
      if fTerrain.Route_CanBeMade(fUnit.GetPosition, Cells.List[i].Loc ,fUnit.GetDesiredPassability, true) then
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
       //Choose location and walk to it (will be different if we are a ranged unit)
       LocID := PickRandomSpot();
       if LocID = 0 then
       begin
         //All cells are taken/inaccessable
         Result := TaskDone;
         exit;
       end;
       SetActionWalk(Cells.List[LocID].Loc);
     end;
  1: begin
       SetActionStay(6,ua_Work,false,0,0); //Start animation
       Direction := TKMDirection(Cells.List[LocID].Dir); //Face target
     end;
  2: begin
       SetActionStay(6,ua_Work,false,0,6); //Pause for next attack
       fHouse.AddDamage(2); //All melee units do 1 damage per strike
       fPhase := 0; //Do another hit (will be 1 after inc below)
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
  SaveStream.Write(LocID);
  Cells.Save(SaveStream);
end;


end.
