unit KM_UnitActionGoInOut;
interface
uses KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Player, KM_Units, SysUtils, Math;


{This is a simple action making unit go inside/outside of house}
type
  TUnitActionGoInOut = class(TUnitAction)
    private
        fStep:single;
        fDirection:TGoInDirection;
        fHouseType:THouseType;
        fDoor:TKMPointF;
        fStreet:TKMPoint;
//        fStartX:single;
        fHasStarted:boolean;
    public
        constructor Create(aAction: TUnitActionType; aDirection:TGoInDirection; aHouseType:THouseType=ht_None);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
    end;


implementation
uses KM_Houses, KM_Game, KM_PlayersCollection, KM_Terrain, KM_Viewport;


constructor TUnitActionGoInOut.Create(aAction: TUnitActionType; aDirection:TGoInDirection; aHouseType:THouseType=ht_None);
begin
  Inherited Create(aAction);
  fDirection    := aDirection;
  fHouseType    := aHouseType;
  fHasStarted   := false;
  
  if fDirection = gd_GoInside then
    fStep := 1  //go Inside (one cell up)
  else
    fStep := 0; //go Outside (one cell down)
end;


procedure TUnitActionGoInOut.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Distance:single;
begin
  DoEnd:= False;
  TimeDelta:=0.1;

  if not fHasStarted then //Set Door and Street locations
  begin
  
    fDoor := KMPointF(KMUnit.GetPosition.X, KMUnit.GetPosition.Y - fStep);
    fStreet := KMPoint(KMUnit.GetPosition.X, KMUnit.GetPosition.Y + 1 - round(fStep));
    if byte(fHouseType) in [1..length(HouseDAT)] then
      fDoor.X := fDoor.X + (HouseDAT[byte(fHouseType)].EntranceOffsetXpx/4)/CELL_SIZE_PX;


    if fDirection=gd_GoInside then
    begin
      KMUnit.Direction:=dir_N;  //one cell up
      KMUnit.Thought := th_None;
      KMUnit.NextPosition := KMPoint(KMUnit.GetPosition.X,KMUnit.GetPosition.Y-1);
      fTerrain.UnitWalk(KMUnit.GetPosition, KMUnit.NextPosition);
      if (KMUnit.GetHome<>nil) and (KMUnit.GetHome.GetHouseType=ht_Barracks) then //Units home is barracks
        TKMHouseBarracks(KMUnit.GetHome).RecruitsInside := TKMHouseBarracks(KMUnit.GetHome).RecruitsInside + 1;
    end;

    if fDirection=gd_GoOutSide then
    begin
      //ChooseSpotToWalkOut
      if fTerrain.Land[fStreet.Y,fStreet.X].IsUnit = 0 then
      begin
        KMUnit.Direction:=dir_S;
        //fStreet.X := fStreet.X
      end else
      //  if fTerrain.TileInMapCoords(fStreet.X-1,fStreet.Y) and (fTerrain.Land[fStreet.Y,fStreet.X-1].IsUnit = 0) then
      //    fStreet.X := fStreet.X - 1
      //  else
      //  if fTerrain.TileInMapCoords(fStreet.X+1,fStreet.Y) and (fTerrain.Land[fStreet.Y,fStreet.X+1].IsUnit = 0) then
      //    fStreet.X := fStreet.X - 1
      //  else
        exit; //Do not exit the house if all street tiles are blocked by units, just wait

      KMUnit.NextPosition := fStreet;
      fTerrain.UnitWalk(KMUnit.GetPosition,KMUnit.NextPosition);
      if (KMUnit.GetHome<>nil)and(KMUnit.GetHome.GetHouseType=ht_Barracks) then //Unit home is barracks
        TKMHouseBarracks(KMUnit.GetHome).RecruitsInside:=TKMHouseBarracks(KMUnit.GetHome).RecruitsInside - 1;
    end;

    fHasStarted:=true;
  end;

  Distance:= TimeDelta * KMUnit.GetSpeed;
  fStep := fStep - Distance * shortint(fDirection);
  KMUnit.PositionF := KMPointF(Mix(fStreet.X,fDoor.X,fStep),Mix(fStreet.Y,fDoor.Y,fStep));
  KMUnit.SetVisibility := fStep >= 0.3; //Make unit invisible when it's inside of House

  if (fStep<=0)or(fStep>=1) then
  begin
    DoEnd:=true;
    if fDirection = gd_GoInside then
      KMUnit.PositionF := fDoor
    else
      KMUnit.PositionF := KMPointF(fStreet.X,fStreet.Y);
  end
  else
    inc(KMUnit.AnimStep);
end;

end.
