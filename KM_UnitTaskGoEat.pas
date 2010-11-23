unit KM_UnitTaskGoEat;
{$I KaM_Remake.inc}
interface
uses Classes, KM_CommonTypes, KM_Defaults, KM_Units, KM_Houses, KromUtils, SysUtils;


{Throw a rock}
type
  TTaskGoEat = class(TUnitTask)
    private
      fInn:TKMHouseInn;
      PlaceID:byte; //Units place in Inn
    public
      constructor Create(aInn:TKMHouseInn; aUnit:TKMUnit);
      constructor Load(LoadStream:TKMemoryStream); override;
      procedure SyncLoad(); override;
      destructor Destroy; override;
      function Eating:boolean;
      function Execute():TTaskResult; override;
      procedure Save(SaveStream:TKMemoryStream); override;
    end;


implementation
uses KM_PlayersCollection, KM_Utils;


{ TTaskGoEat }
constructor TTaskGoEat.Create(aInn:TKMHouseInn; aUnit:TKMUnit);
begin
  Inherited Create(aUnit);
  fTaskName := utn_GoEat;
  fInn      := TKMHouseInn(aInn.GetHousePointer);
  PlaceID   := 0;
end;


constructor TTaskGoEat.Load(LoadStream:TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fInn, 4);
  LoadStream.Read(PlaceID);
end;


procedure TTaskGoEat.SyncLoad();
begin
  Inherited;
  fInn := TKMHouseInn(fPlayers.GetHouseByID(cardinal(fInn)));
end;


//May happen when we die while desperatley trying to get some food
destructor TTaskGoEat.Destroy;
begin
  if PlaceID<>0 then fInn.EatersGoesOut(PlaceID);
  if fInn <> nil then fInn.ReleaseHousePointer;
  Inherited;
end;


function TTaskGoEat.Eating:boolean;
begin
  Result := PlaceID <> 0;
end;


function TTaskGoEat.Execute():TTaskResult;
begin
  Result := TaskContinues;

  if fInn.IsDestroyed then
  begin
    Result := TaskDone;
    exit;
  end;

  with fUnit do
  case fPhase of
   0: begin
        Thought := th_Eat;
        if GetHome<>nil then GetHome.SetState(hst_Empty);
        if not IsVisible then SetActionGoIn(ua_Walk,gd_GoOutside,fUnit.GetHome) else
                              SetActionLockedStay(0,ua_Walk); //Walk outside the house
      end;
   1: SetActionWalkToSpot(KMPointY1(fInn.GetEntrance));
   2: begin
        SetActionGoIn(ua_Walk,gd_GoInside,fInn); //Enter Inn
        PlaceID := fInn.EaterGetsInside(UnitType);
      end;
   3: //Units are fed acording to this: (from knightsandmerchants.de tips and tricks)
      //Bread    = +40%
      //Sausages = +60%
      //Wine     = +20%
      //Fish     = +50%
      if (GetCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Bread)>0)and(PlaceID<>0) then begin
        fInn.ResTakeFromIn(rt_Bread);
        SetActionStay(29*4,ua_Eat,false);
        Feed(UNIT_MAX_CONDITION*0.4);
        fInn.UpdateEater(PlaceID,2); //Order is Wine-Bread-Sausages-Fish
      end else
        SetActionLockedStay(0,ua_Walk);
   4: if (GetCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Sausages)>0)and(PlaceID<>0) then begin
        fInn.ResTakeFromIn(rt_Sausages);
        SetActionStay(29*4,ua_Eat,false);
        Feed(UNIT_MAX_CONDITION*0.6);
        fInn.UpdateEater(PlaceID,3);
      end else
        SetActionLockedStay(0,ua_Walk);
   5: if (GetCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Wine)>0)and(PlaceID<>0) then begin
        fInn.ResTakeFromIn(rt_Wine);
        SetActionStay(29*4,ua_Eat,false);
        Feed(UNIT_MAX_CONDITION*0.2);
        fInn.UpdateEater(PlaceID,1);
      end else
        SetActionLockedStay(0,ua_Walk);
   6: if (GetCondition<UNIT_MAX_CONDITION)and(fInn.CheckResIn(rt_Fish)>0)and(PlaceID<>0) then begin
        fInn.ResTakeFromIn(rt_Fish);
        SetActionStay(29*4,ua_Eat,false);
        Feed(UNIT_MAX_CONDITION*0.5);
        fInn.UpdateEater(PlaceID,4);
      end else
        SetActionLockedStay(0,ua_Walk);
   7: begin
        //Stop showing hungry if we no longer are, but if we are then walk out of the inn thinking hungry so that the player will know that we haven't been fed
        if GetCondition<UNIT_MAX_CONDITION then
          Thought := th_Eat else Thought := th_None;
        SetActionGoIn(ua_Walk,gd_GoOutside,fInn); //Exit Inn
        fInn.EatersGoesOut(PlaceID);
        PlaceID:=0;
      end;
   else Result := TaskDone;
  end;
  inc(fPhase);
end;


procedure TTaskGoEat.Save(SaveStream:TKMemoryStream);
begin
  Inherited;
  if fInn <> nil then
    SaveStream.Write(fInn.ID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Zero);
  SaveStream.Write(PlaceID);
end;



end.
