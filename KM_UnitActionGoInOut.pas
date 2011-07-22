unit KM_UnitActionGoInOut;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils, KM_CommonTypes, KM_Defaults, KM_Houses, KM_Units, KM_Utils, KM_Points;


type TBestExit = (be_None, be_Left, be_Center, be_Right);

{This is a [fairly :P] simple action making unit go inside/outside of house}
type
  TUnitActionGoInOut = class(TUnitAction)
  private
    fUnit: TKMUnit;
    fStep:single;
    fHouse:TKMHouse;
    fDirection:TGoInDirection;
    fDoor:TKMPointF;
    fStreet:TKMPoint;
    fHasStarted:boolean;
    fWaitingForPush:boolean;
    fUsedDoorway:boolean;
    procedure IncDoorway;
    procedure DecDoorway;
    function FindBestExit(aLoc:TKMPoint; aUnit:TKMUnit):TBestExit;
    function ValidTileToGo(aLocX, aLocY:word; aUnit:TKMUnit):boolean; //using X,Y looks more clear
    procedure WalkIn(aUnit:TKMUnit);
    procedure WalkOut(aUnit:TKMUnit);
  public
    constructor Create(aAction: TUnitActionType; aUnit:TKMUnit; aDirection:TGoInDirection; aHouse:TKMHouse);
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function GetExplanation:string; override;
    property GetHasStarted: boolean read fHasStarted;
    function Execute(KMUnit: TKMUnit):TActionResult; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_UnitActionStay, KM_ResourceGFX, KM_UnitActionWalkTo;


constructor TUnitActionGoInOut.Create(aAction: TUnitActionType; aUnit:TKMUnit; aDirection:TGoInDirection; aHouse:TKMHouse);
begin
  Inherited Create(aAction);
  fActionName     := uan_GoInOut;
  fUnit           := aUnit.GetUnitPointer;
  Locked          := true;
  //We might stuck trying to exit when house gets destroyed (1)
  //and we might be dying in destroyed house (2)
  if aHouse<>nil then fHouse := aHouse.GetHousePointer
                 else fHouse := nil;
  fDirection      := aDirection;
  fHasStarted     := false;
  fWaitingForPush := false;

  if fDirection = gd_GoInside then
    fStep := 1  //go Inside (one cell up)
  else
    fStep := 0; //go Outside (one cell down)
end;


constructor TUnitActionGoInOut.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fStep);
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fUnit, 4);
  LoadStream.Read(fDirection, SizeOf(fDirection));
  LoadStream.Read(fDoor);
  LoadStream.Read(fStreet);
  LoadStream.Read(fHasStarted);
  LoadStream.Read(fWaitingForPush);
  LoadStream.Read(fUsedDoorway);
end;


procedure TUnitActionGoInOut.SyncLoad;
begin
  Inherited;
  fHouse := fPlayers.GetHouseByID(cardinal(fHouse));
  fUnit := fPlayers.GetUnitByID(cardinal(fUnit))
end;


destructor TUnitActionGoInOut.Destroy;
begin
  if fUsedDoorway then DecDoorway;
  fPlayers.CleanUpHousePointer(fHouse);
  //A bug can occur because this action is destroyed early when a unit is told to die. If we are still invisible
  //then TTaskDie assumes we are inside and creates a new GoOut action. Therefore if we are invisible we do not occupy a tile.
  if (fDirection = gd_GoOutside) and (fHasStarted) and (fUnit<>nil) and (not fUnit.Visible) and
    (fTerrain.Land[fUnit.NextPosition.Y,fUnit.NextPosition.X].IsUnit = fUnit) then
  begin
    fTerrain.UnitRem(fUnit.NextPosition);
    if not KMSamePointF(fDoor, KMPointF(0,0)) then fUnit.PositionF := fDoor; //Put us back inside the house
  end;
  if (fUnit <> nil) and (fDirection = gd_GoOutside) and (fUnit.Visible) then fUnit.SetInHouse(nil); //We are not in any house now
  fPlayers.CleanUpUnitPointer(fUnit);
  Inherited;
end;


function TUnitActionGoInOut.GetExplanation: string;
begin
  Result := 'Walking in/out';
end;


procedure TUnitActionGoInOut.IncDoorway;
begin
  Assert(not fUsedDoorway, 'Inc doorway when already in use?');

  if fHouse<>nil then inc(fHouse.DoorwayUse);
  fUsedDoorway := true;
end;


procedure TUnitActionGoInOut.DecDoorway;
begin
  Assert(fUsedDoorway, 'Dec doorway when not in use?');

  if fHouse<>nil then dec(fHouse.DoorwayUse);
  fUsedDoorway := false;
end;


//Attempt to find a tile below the door (on the street) we can walk to
//We can push idle units away. Check center first
function TUnitActionGoInOut.FindBestExit(aLoc:TKMPoint; aUnit:TKMUnit):TBestExit;
begin
  if ValidTileToGo(aLoc.X, aLoc.Y, aUnit)   then Result := be_Center
  else
  if ValidTileToGo(aLoc.X-1, aLoc.Y, aUnit) then Result := be_Left
  else
  if ValidTileToGo(aLoc.X+1, aLoc.Y, aUnit) then Result := be_Right
  else
    Result := be_None;
end;


//Check that tile is walkable and there's no unit blocking it or that unit can be pushed away
function TUnitActionGoInOut.ValidTileToGo(aLocX, aLocY:word; aUnit:TKMUnit):boolean; //using X,Y looks more clear
var U:TKMUnit;
begin
  Result := fTerrain.TileInMapCoords(aLocX, aLocY)
        and (fTerrain.CheckPassability(KMPoint(aLocX, aLocY), aUnit.GetDesiredPassability))
        and (fTerrain.CanWalkDiagonaly(aUnit.GetPosition, KMPoint(aLocX, aLocY)));

  if not Result then exit;

  //If there's some unit we need to do a better check on him
  if (fTerrain.Land[aLocY, aLocX].IsUnit <> nil) then
  begin
    U := fTerrain.UnitsHitTest(aLocX, aLocY); //Let's see who is standing there
    Result := (U <> nil) and
              (U.GetUnitAction is TUnitActionStay) and
              (not TUnitActionStay(U.GetUnitAction).Locked);
    if Result then
      U.SetActionWalkPushed( fTerrain.GetOutOfTheWay(U.GetPosition, KMPoint(0,0), CanWalk) );
  end;
end;


procedure TUnitActionGoInOut.WalkIn(aUnit:TKMUnit);
begin
  aUnit.Direction := dir_N;  //one cell up
  aUnit.Thought := th_None;
  aUnit.UpdateNextPosition(KMPoint(aUnit.GetPosition.X, aUnit.GetPosition.Y-1));
  fTerrain.UnitRem(aUnit.GetPosition); //Unit does not occupy a tile while inside
end;


procedure TUnitActionGoInOut.WalkOut(aUnit:TKMUnit);
begin
  aUnit.Direction := KMGetDirection(KMPointRound(fDoor), fStreet);
  aUnit.UpdateNextPosition(fStreet);
  fTerrain.UnitAdd(aUnit.NextPosition, aUnit); //Unit was not occupying tile while inside
  if (aUnit.GetHome <> nil)
  and (aUnit.GetHome.GetHouseType = ht_Barracks) //Unit home is barracks
  and (aUnit.GetHome = fHouse) then //And is the house we are walking from
    TKMHouseBarracks(aUnit.GetHome).RecruitsList.Remove(aUnit);
end;


function TUnitActionGoInOut.Execute(KMUnit: TKMUnit):TActionResult;
var Distance:single; U:TKMUnit;
begin
  Result := ActContinues;

  if not fHasStarted then //Set Door and Street locations
  begin

    fDoor := KMPointF(KMUnit.GetPosition.X, KMUnit.GetPosition.Y - fStep);
    fStreet := KMPoint(KMUnit.GetPosition.X, KMUnit.GetPosition.Y + 1 - round(fStep));
    if (fHouse<>nil) then
      fDoor.X := fDoor.X + (fResource.HouseDat[fHouse.GetHouseType].EntranceOffsetXpx/4)/CELL_SIZE_PX;

    case fDirection of
      gd_GoInside:  WalkIn(KMUnit);
      gd_GoOutside: begin
                      case FindBestExit(fStreet, KMUnit) of
                        be_Left:    fStreet.X := fStreet.X - 1;
                        be_Center:  ;
                        be_Right:   fStreet.X := fStreet.X + 1;
                        else        exit; //All street tiles are blocked by busy units. Do not exit the house, just wait
                      end;

                      //If there's an idling unit, wait till it goes away
                      if (fTerrain.Land[fStreet.Y,fStreet.X].IsUnit <> nil) then
                      begin
                        fWaitingForPush := true;
                        fHasStarted := true;
                        exit; //Wait until our push request is dealt with before we move out
                      end;

                      WalkOut(KMUnit); //All checks done so we can walk out now
                    end;
    end;
    if fStreet.X = KMPointRound(fDoor).X then //We are walking straight
      IncDoorway;

    fHasStarted := true;
  end;


  if fWaitingForPush then
  begin
    U := fTerrain.Land[fStreet.Y,fStreet.X].IsUnit;
    if (U = nil) then
    begin
      fWaitingForPush := false;
      WalkOut(KMUnit);
      if fStreet.X = KMPointRound(fDoor).X then //We are walking straight
        IncDoorway;
    end
    else
      //Make sure they are still moving out of the way (it could be a different unit now)
      if (U.GetUnitAction is TUnitActionWalkTo) and TUnitActionWalkTo(U.GetUnitAction).HasBeenPushed then
        exit //Wait until our push request is dealt with before we move out
      else
      begin
        fHasStarted := false; //This unit switched places with another or gave up, so we must start again
        fWaitingForPush := false;
        exit;
      end;
  end;

  //IsExchanging can be updated while we have completed less than 20% of the move. If it is changed after that
  //the unit makes a noticable "jump". This needs to be updated after starting because we don't know about an
  //exchanging unit until they have also started walking (otherwise only 1 of the units will have IsExchanging = true)
  if (shortint(fDirection) - fStep < 0.2) and
     (fStreet.X = KMPointRound(fDoor).X) and //We are walking straight
     (fHouse <> nil) then
     KMUnit.IsExchanging := (fHouse.DoorwayUse > 1);

  Assert((fHouse = nil) or KMSamePoint(KMPointRound(fDoor),fHouse.GetEntrance)); //Must always go in/out the entrance of the house
  Distance := KMUnit.GetSpeed;

  //Actual speed is slower if we are moving diagonally, due to the fact we are moving in X and Y
  if (fStreet.X-fDoor.X <> 0) then
    Distance := Distance / 1.41; {sqrt (2) = 1.41421 }

  fStep := fStep - Distance * shortint(fDirection);
  KMUnit.PositionF := KMPointF(Mix(fStreet.X,fDoor.X,fStep),Mix(fStreet.Y,fDoor.Y,fStep));
  KMUnit.Visible := (fHouse=nil) or (fHouse.IsDestroyed) or (fStep >= 0.3); //Make unit invisible when it's inside of House

  if (fStep<=0)or(fStep>=1) then
  begin
    Result := ActDone;
    KMUnit.IsExchanging := false;
    if fUsedDoorway then DecDoorway;
    if fDirection = gd_GoInside then
    begin
      KMUnit.PositionF := fDoor;
      if (KMUnit.GetHome<>nil)and(KMUnit.GetHome.GetHouseType=ht_Barracks) //Unit home is barracks
      and(KMUnit.GetHome = fHouse)and(not KMUnit.GetHome.IsDestroyed) then //And is the house we are walking into and it's not destroyed
        TKMHouseBarracks(KMUnit.GetHome).RecruitsList.Add(KMUnit); //Add the recruit once it is inside, otherwise it can be equipped while still walking in!
      //Set us as inside even if the house is destroyed. In that case UpdateVisibility will sort things out.
      if fHouse<>nil then KMUnit.SetInHouse(fHouse);
    end
    else
    begin
      KMUnit.PositionF := KMPointF(fStreet.X,fStreet.Y);
      KMUnit.SetInHouse(nil); //We are not in a house any longer
    end;
  end
  else
    inc(KMUnit.AnimStep);
end;


procedure TUnitActionGoInOut.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fStep);
  if fHouse <> nil then
    SaveStream.Write(fHouse.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  if fUnit <> nil then
    SaveStream.Write(fUnit.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(fDirection, SizeOf(fDirection));
  SaveStream.Write(fDoor);
  SaveStream.Write(fStreet);
  SaveStream.Write(fHasStarted);
  SaveStream.Write(fWaitingForPush);
  SaveStream.Write(fUsedDoorway);
end;


end.
