unit KM_UnitTaskGoEat;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses, SysUtils, KM_Points;


type
  //Go to eat
  TTaskGoEat = class(TUnitTask)
    private
      fInn: TKMHouseInn;
      PlaceID: Byte; //Units place in Inn
      fFoodsEaten: Byte;
    public
      constructor Create(aInn: TKMHouseInn; aUnit: TKMUnit);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure SyncLoad; override;
      destructor Destroy; override;
      function Eating: Boolean;
      function Execute: TTaskResult; override;
      procedure Save(SaveStream: TKMemoryStream); override;
    end;


implementation
uses KM_PlayersCollection;


{ TTaskGoEat }
constructor TTaskGoEat.Create(aInn: TKMHouseInn; aUnit: TKMUnit);
begin
  inherited Create(aUnit);
  fTaskName := utn_GoEat;
  fInn      := TKMHouseInn(aInn.GetHousePointer);
  PlaceID   := 0;
  fFoodsEaten := 0;
end;


constructor TTaskGoEat.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fInn, 4);
  LoadStream.Read(PlaceID);
  LoadStream.Read(fFoodsEaten);
end;


procedure TTaskGoEat.SyncLoad;
begin
  inherited;
  fInn := TKMHouseInn(fPlayers.GetHouseByID(Cardinal(fInn)));
end;


destructor TTaskGoEat.Destroy;
begin
  //May happen when we die while desperatley trying to get some food
  if Eating then
    fInn.EatersGoesOut(PlaceID);

  fPlayers.CleanUpHousePointer(TKMHouse(fInn));
  inherited;
end;


function TTaskGoEat.Eating: Boolean;
begin
  Result := PlaceID <> 0;
end;


function TTaskGoEat.Execute: TTaskResult;
begin
  Result := TaskContinues;

  if fInn.IsDestroyed then
  begin
    Result := TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
   0: begin
        Thought := th_Eat;
        if (GetHome <> nil) and not GetHome.IsDestroyed then GetHome.SetState(hst_Empty);
        if not Visible and (GetInHouse <> nil) and not GetInHouse.IsDestroyed then
          SetActionGoIn(ua_Walk, gd_GoOutside, GetInHouse) //Walk outside the house
        else
          SetActionLockedStay(0, ua_Walk); //Skip this step
      end;
   1: SetActionWalkToSpot(KMPointBelow(fInn.GetEntrance));
   2: begin
        SetActionGoIn(ua_Walk, gd_GoInside, fInn); //Enter Inn
        PlaceID := fInn.EaterGetsInside(UnitType);
        //If there's no free place in the Inn skip to the step where we go out hungry
        if PlaceID = 0 then begin
          fPhase := 7;
          Exit;
        end;
      end;
   3: //Typically when unit comes to Inn he is at 13%
      //Order is Bread-Sausages-Wine-Fish
      //Units are fed acording to this: (from knightsandmerchants.de tips and tricks)
      //Bread    = +40%
      //Sausages = +60%
      //Wine     = +20%
      //Fish     = +50%
      //We allow unit to eat 2 foods at most and only until he is at full condition
      //@Lewin: Why do we allow only 2 foods at max? Bread+Wine are going to fed him to 70% at most
      //My proposed change is to use (Condition < UNIT_MAX_CONDITION * 0.9)
      //and remove fFoodsEaten at all
      //@Krom: I thought in KaM units never ate more than 2 food items. The problem is the third item
      //       is often a waste, e.g. bread+wine+fish = 1.1, plus the unit already had about 0.1, so
      //       that's wasting 0.2 of food. It would be more efficient for the unit to come back later.
      //       Although we should test in KaM whether units eat more than two items.
      //todo: Test whether units eat more than two items at once in TPR (bread+wine+fish)
      if (Condition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Bread)>0)and(fFoodsEaten<2) then
      begin
        fInn.ResTakeFromIn(rt_Bread);
        fPlayers.Player[fUnit.GetOwner].Stats.GoodConsumed(rt_Bread);
        Inc(fFoodsEaten);
        SetActionLockedStay(29*4, ua_Eat, False);
        Feed(UNIT_MAX_CONDITION * 0.4);
        fInn.UpdateEater(PlaceID, rt_Bread);
      end else
        SetActionLockedStay(0,ua_Walk);
   4: if (Condition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Sausages)>0)and(fFoodsEaten<2) then begin
        fInn.ResTakeFromIn(rt_Sausages);
        fPlayers.Player[fUnit.GetOwner].Stats.GoodConsumed(rt_Sausages);
        Inc(fFoodsEaten);
        SetActionLockedStay(29*4, ua_Eat, False);
        Feed(UNIT_MAX_CONDITION * 0.6);
        fInn.UpdateEater(PlaceID, rt_Sausages);
      end else
        SetActionLockedStay(0,ua_Walk);
   5: if (Condition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Wine)>0)and(fFoodsEaten<2) then begin
        fInn.ResTakeFromIn(rt_Wine);
        fPlayers.Player[fUnit.GetOwner].Stats.GoodConsumed(rt_Wine);
        Inc(fFoodsEaten);
        SetActionLockedStay(29*4, ua_Eat, False);
        Feed(UNIT_MAX_CONDITION * 0.2);
        fInn.UpdateEater(PlaceID, rt_Wine);
      end else
        SetActionLockedStay(0,ua_Walk);
   6: if (Condition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Fish)>0)and(fFoodsEaten<2) then begin
        fInn.ResTakeFromIn(rt_Fish);
        fPlayers.Player[fUnit.GetOwner].Stats.GoodConsumed(rt_Fish);
        Inc(fFoodsEaten);
        SetActionLockedStay(29*4, ua_Eat, False);
        Feed(UNIT_MAX_CONDITION * 0.5);
        fInn.UpdateEater(PlaceID, rt_Fish);
      end else
        SetActionLockedStay(0,ua_Walk);
   7: begin
        //Stop showing hungry if we no longer are,
        //but if we are then walk out of the inn thinking hungry
        //so that the player will know that we haven't been fed
        if Condition < UNIT_MAX_CONDITION * 0.9 then
          Thought := th_Eat
        else
          Thought := th_None;
        SetActionGoIn(ua_Walk, gd_GoOutside, fInn); //Exit Inn
        fInn.EatersGoesOut(PlaceID);
        PlaceID := 0;
      end;
   else
      Result := TaskDone;
  end;
  Inc(fPhase);
end;


procedure TTaskGoEat.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fInn <> nil then
    SaveStream.Write(fInn.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(PlaceID);
  SaveStream.Write(fFoodsEaten);
end;


end.
