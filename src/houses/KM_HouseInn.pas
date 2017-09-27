unit KM_HouseInn;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses,
  KM_ResWares, KM_ResHouses,
  KM_CommonClasses, KM_Defaults;


type
  TKMHouseInn = class(TKMHouse)
  private
    Eater: array [0..5] of record //only 6 units are allowed in the inn
      UnitType: TUnitType;
      FoodKind: TWareType; //What kind of food eater eats
      EatStep: Cardinal;
    end;
  public
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    function EaterGetsInside(aUnitType: TUnitType): ShortInt;
    procedure UpdateEater(aIndex: ShortInt; aFoodKind: TWareType);
    procedure EatersGoesOut(aIndex: ShortInt);
    function HasFood: Boolean;
    function HasSpace: Boolean;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override; //Render all eaters
  end;


implementation
uses
  KM_RenderPool,
  KM_Hand, KM_HandsCollection,
  KM_Points;


{ TKMHouseInn }
constructor TKMHouseInn.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := Low(Eater) to High(Eater) do
    Eater[I].UnitType := ut_None;
end;


constructor TKMHouseInn.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(Eater, SizeOf(Eater));
end;


//EatStep := FlagAnimStep, cos increases it each frame, we don't need to increase all 6 AnimSteps manually
function TKMHouseInn.EaterGetsInside(aUnitType: TUnitType): ShortInt;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Eater) to High(Eater) do
  if Eater[I].UnitType = ut_None then
  begin
    Eater[I].UnitType := aUnitType;
    Eater[I].FoodKind := wt_None;
    Eater[I].EatStep  := FlagAnimStep;
    Result := I;
    Exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aIndex: ShortInt; aFoodKind: TWareType);
begin
  if aIndex = -1 then Exit;
  Assert(aFoodKind in [wt_Wine, wt_Bread, wt_Sausages, wt_Fish], 'Wrong kind of food in Inn');

  Eater[aIndex].FoodKind := aFoodKind; //Order is Wine-Bread-Sausages-Fish
  Eater[aIndex].EatStep  := FlagAnimStep; //Eat animation step will be difference between FlagAnim and EatStep
end;


procedure TKMHouseInn.EatersGoesOut(aIndex: ShortInt);
begin
  if aIndex <> -1 then
    Eater[aIndex].UnitType := ut_None;
end;


function TKMHouseInn.HasFood: Boolean;
begin
  Result := CheckResIn(wt_Sausages) + CheckResIn(wt_Bread) + CheckResIn(wt_Wine) + CheckResIn(wt_Fish) > 0;
end;


function TKMHouseInn.HasSpace: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Eater) to High(Eater) do
    Result := Result or (Eater[I].UnitType = ut_None);
end;


procedure TKMHouseInn.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(Eater, SizeOf(Eater));
end;


procedure TKMHouseInn.Paint;
  //Chose eater animation direction (135 face south, 246 face north)
  function AnimDir(aIndex: Integer): TKMDirection;
  begin
    case Eater[aIndex].FoodKind of
      wt_Wine:      Result  := TKMDirection(1 * 2 - 1 + (aIndex div 3));
      wt_Bread:     Result  := TKMDirection(2 * 2 - 1 + (aIndex div 3));
      wt_Sausages:  Result  := TKMDirection(3 * 2 - 1 + (aIndex div 3));
      wt_Fish:      Result  := TKMDirection(4 * 2 - 1 + (aIndex div 3));
      else          Result  := dir_NA;
    end;
  end;
const
  offX: array [0..2] of Single = ( -0.5, 0, 0.5);
  offY: array [0..2] of Single = (-0.05, 0, 0.05);
var
  I: Integer;
  AnimStep: Cardinal;
begin
  inherited;
  if fBuildState <> hbs_Done then exit;

  for I := Low(Eater) to High(Eater) do
  begin
    if (Eater[I].UnitType = ut_None) or (Eater[I].FoodKind = wt_None) then Continue;

    AnimStep := FlagAnimStep - Eater[I].EatStep; //Delta is our AnimStep

    gRenderPool.AddHouseEater(fPosition, Eater[I].UnitType, ua_Eat,
                              AnimDir(I), AnimStep,
                              offX[I mod 3], offY[I mod 3],
                              gHands[fOwner].FlagColor);
  end;
end;


end.
