unit KM_UnitActionGoInOut;
interface
uses KM_Defaults, KromUtils, KM_Utils, KM_CommonTypes, KM_Player, KM_Units, SysUtils, Math;


{This is a simple action making unit go inside/outside of house}
type
  TUnitActionGoInOut = class(TUnitAction)
    private
        fStep:single;
        fDir:TGoInDirection;
        fHouseType:THouseType;
        fStartX:single;
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
  fDir:=aDirection;
  fHouseType:=aHouseType;
  fHasStarted:=false;
  if fDir=gd_GoInside then fStep:=1  //go Inside (one cell up)
                      else fStep:=0; //go Outside (one cell down)
end;


procedure TUnitActionGoInOut.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var Distance:single; OffX:single;
begin
  if not fHasStarted then
    fStartX := KMUnit.PositionF.X;

  fHasStarted:=true;
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * KMUnit.GetSpeed;

  if fDir=gd_GoInside then
    KMUnit.Direction:=dir_N  //one cell up
  else
    KMUnit.Direction:=dir_S; //one cell down

  //First step on going inside
  if fStep=1 then begin
    KMUnit.Thought := th_None;
    KMUnit.NextPosition:=KMPoint(KMUnit.GetPosition.X,KMUnit.GetPosition.Y-1);
    fTerrain.UnitWalk(KMUnit.GetPosition,KMUnit.NextPosition);
    if (KMUnit.GetHome<>nil)and(KMUnit.GetHome.GetHouseType=ht_Barracks) then //Unit home is barracks
      TKMHouseBarracks(KMUnit.GetHome).RecruitsInside:=TKMHouseBarracks(KMUnit.GetHome).RecruitsInside + 1;
  end;

  //First step on going outside
  if fStep=0 then begin
    KMUnit.NextPosition:=KMPointY1(KMUnit.GetPosition);
    //if fTerrain.Land[KMUnit.NextPosition.Y,KMUnit.NextPosition.X].IsUnit<>0 then exit; //Do not exit if tile is occupied
    fTerrain.UnitWalk(KMUnit.GetPosition,KMUnit.NextPosition);
    if (KMUnit.GetHome<>nil)and(KMUnit.GetHome.GetHouseType=ht_Barracks) then //Unit home is barracks
      TKMHouseBarracks(KMUnit.GetHome).RecruitsInside:=TKMHouseBarracks(KMUnit.GetHome).RecruitsInside - 1;
 end;

  fStep := fStep - Distance * shortint(fDir);

  if fHouseType <> ht_None then
    OffX := ((HouseDAT[byte(fHouseType)].EntranceOffsetXpx/4)/CELL_SIZE_PX)
  else
    OffX := 0;

  KMUnit.PositionF := KMPointF(
    Mix(fStartX,fStartX + OffX,fStep),
    KMUnit.PositionF.Y - Distance * shortint(fDir)
    );  

  KMUnit.SetVisibility := fStep >= 0.3; //Make unit invisible when it's inside of House

  if (fStep<=0)or(fStep>=1) then
  begin
    DoEnd:=true;
    KMUnit.PositionF := KMPointF(fStartX,KMUnit.PositionF.Y);
  end
  else
    inc(KMUnit.AnimStep);
end;

end.
