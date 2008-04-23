unit KM_Houses;
interface
uses windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain, KM_Global_Data, KM_Defaults;

type

  THouseType = (
  ht_Sawmill=1,        ht_IronSmithy=2, ht_WeaponSmithy=3, ht_CoalMine=4,       ht_IronMine=5,
  ht_GoldMine=6,       ht_FisherHut=7,  ht_Bakery=8,       ht_Farm=9,           ht_Woodcutter=10,
  ht_ArmorSmithy=11,   ht_Store=12,     ht_Stables=13,     ht_School=14,        ht_Quary=15,
  ht_Metallurgist=16,  ht_Swine=17,     ht_WatchTower=18,  ht_TownHall=19,      ht_WeaponWorkshop=20,
  ht_ArmorWorkshop=21, ht_Barracks=22,  ht_Mill=23,        ht_SiegeWorkshop=24, ht_Butchers=25,
  ht_Tannery=26,       ht_NA=27,        ht_Inn=28,         ht_Wineyard=29);

  THouseActionType = ( hat_Idle, hat_Work );

  THouseActionSet = set of (
  ha_Work1=1, ha_Work2=2, ha_Work3=3, ha_Work4=4, ha_Work5=5, //Start, InProgress, .., .., Finish
  ha_Smoke=6, ha_FlagShtok=7, ha_Idle=8,
  ha_Flag1=9, ha_Flag2=10, ha_Flag3=11,
  ha_Fire1=12, ha_Fire2=13, ha_Fire3=14, ha_Fire4=15, ha_Fire5=16, ha_Fire6=17, ha_Fire7=18, ha_Fire8=19);

  THouseState = (hs_Planing, hs_Wooding, hs_Stone, hs_Damage);

  TKMHouse = class;

  THouseAction = class(TObject)
  private
    fActionType: THouseActionType;
    fSubAction: THouseActionSet;
  public
    constructor Create(aActionType: THouseActionType);
    procedure ActionSet(aActionType: THouseActionType);
    procedure SubActionWork(aActionSet: THouseActionSet);
    procedure SubActionAdd(aActionSet: THouseActionSet);
    procedure SubActionRem(aActionSet: THouseActionSet);
    procedure Execute(KMHouse: TKMHouse; TimeDelta: single; out DoEnd: Boolean);// virtual; abstract;
    property ActionType: THouseActionType read fActionType;
  end;

  TKMHouse = class(TObject)
  private
    fHouseType: THouseType;
    fCurrentAction: THouseAction;
    fResourceIn:array[1..5]of byte;
    fResourceOut:array[1..5]of byte;
    fPosition: TPos;
    fLastUpdateTime: Cardinal;
    AnimStep: integer;
  public
    constructor Create(PosX,PosY:integer; aHouseType:THouseType);
    destructor Destroy; override;
    function HitTest(X, Y: Integer): Boolean; overload;
    procedure SetAction(aAction: THouseAction);
    procedure AddResource(aResource:TResourceType);
    function RemResource(aResource:TResourceType):boolean;
    procedure UpdateState;
    procedure Paint();// virtual; abstract;
  end;

  TKMSawmill = class(TKMHouse) end;
  TKMIronSmithy = class(TKMHouse) end;
  TKMWeaponSmithy = class(TKMHouse) end;
  TKMCoalMine = class(TKMHouse) end;
  TKMIronMine = class(TKMHouse) end;
  TKMGoldMine = class(TKMHouse) end;
  TKMFisherHut = class(TKMHouse) end;
  TKMBakery = class(TKMHouse) end;
  TKMFarm = class(TKMHouse) end;
  TKMWoodcutter = class(TKMHouse) end;
  TKMArmorSmithy = class(TKMHouse) end;
  TKMStore = class(TKMHouse) end;
  TKMStables = class(TKMHouse) end;
  TKMSchool = class(TKMHouse) end;
  TKMQuary = class(TKMHouse) end;
  TKMMetallurgist = class(TKMHouse) end;
  TKMSwine = class(TKMHouse) end;
  TKMWatchTower = class(TKMHouse) end;
  TKMTownHall = class(TKMHouse) end;
  TKMWeaponWorkshop = class(TKMHouse) end;
  TKMArmorWorkshop = class(TKMHouse) end;
  TKMBarracks = class(TKMHouse) end;
  TKMMill = class(TKMHouse) end;
  TKMSiegeWorkshop = class(TKMHouse) end;
  TKMButchers = class(TKMHouse) end;
  TKMTannery = class(TKMHouse) end;
//    ht_NA:  Inherited Add(TKMMill = class(TKMHouse) end;
  TKMInn = class(TKMHouse) end;
  TKMWineyard = class(TKMHouse) end;

  TKMHousesCollection = class(TList)
  private
    fSelectedHouse: TKMHouse;
  public
    procedure Add(aHouseType: THouseType; PosX,PosY:integer);
    procedure Rem(PosX,PosY:integer);
    procedure Clear; override;
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMHouse;
    procedure Paint();
    property SelectedHouse: TKMHouse read fSelectedHouse; 
  end;

implementation

{ TKMHouse }

constructor TKMHouse.Create(PosX,PosY:integer; aHouseType:THouseType);
begin
  Inherited Create;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fHouseType:=aHouseType;
  fCurrentAction:=THouseAction.Create(hat_Idle);
  fCurrentAction.SubActionAdd([ha_FlagShtok]);
  fCurrentAction.SubActionAdd([ha_Flag1]);
  fCurrentAction.SubActionAdd([ha_Flag2]);
  fCurrentAction.SubActionAdd([ha_Flag3]);
//  fResourceIn[1]:=5;
end;

destructor TKMHouse.Destroy;
begin
  Inherited;
  fCurrentAction.Free;
end;

function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result:=false;
if (X-fPosition.X+3 in [1..4])and(Y-fPosition.Y+4 in [1..4]) then
if HousePlanYX[integer(fHouseType),Y-fPosition.Y+4,X-fPosition.X+3]<>0 then
  Result:=true;
end;

procedure TKMHouse.AddResource(aResource:TResourceType);
begin
if aResource=rt_None then exit;
inc(fResourceIn[1]);
end;

function TKMHouse.RemResource(aResource:TResourceType):boolean;
begin
Result:=false;
if fResourceOut[1]<=0 then exit;
dec(fResourceOut[1]);
Result:=true;
end;

procedure TKMHouse.SetAction(aAction: THouseAction);
begin
  if aAction = nil then
  begin
    fCurrentAction.Free;
    fCurrentAction:= nil;
    Exit;
  end;
  if fCurrentAction <> aAction then
  begin
    fCurrentAction.Free;
    fCurrentAction:= aAction;
  end;
end;

procedure TKMHouse.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
//  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if DoEnd then
    SetAction(nil);

//inc(AnimStep);
if (fCurrentAction.fActionType=hat_Idle)and(fResourceIn[1]>=1) then begin
  dec(fResourceIn[1]);
  AnimStep:=0;
  fCurrentAction.ActionSet(hat_Work);
end;
if fCurrentAction.fActionType=hat_Work then begin
  fCurrentAction.SubActionAdd([ha_Smoke]);
  if AnimStep=HouseDAT[integer(fHouseType)].Anim[1].Count then
  fCurrentAction.SubActionAdd([ha_Work1]);
  if AnimStep>=10 then
  fCurrentAction.SubActionAdd([ha_Work2]);
  if AnimStep>=20 then
  fCurrentAction.SubActionAdd([ha_Work3]);
  if AnimStep>=30 then
  fCurrentAction.SubActionAdd([ha_Work4]);
  if AnimStep>=40 then
  fCurrentAction.SubActionAdd([ha_Work5]);
  if AnimStep>=50 then begin
  fCurrentAction.ActionSet(hat_Idle);
  inc(fResourceOut[1]);
  end;
  end;
end;

procedure TKMHouse.Paint;
var Owner:integer; AnimKind:integer;
begin
Owner:=2;//should be inherited from =Player=
AnimKind:=2;
//Render base
fRender.RenderHouse(integer(fHouseType),fPosition.X, fPosition.Y);
//Render supplies
fRender.RenderHouseSupply(integer(fHouseType),fResourceIn,fResourceOut,fPosition.X, fPosition.Y);
//Render animation
if fCurrentAction=nil then exit;
fRender.RenderHouseWork(integer(fHouseType),integer(fCurrentAction.fSubAction),AnimStep,1,fPosition.X, fPosition.Y);

end;

{ THouseAction }

constructor THouseAction.Create(aActionType: THouseActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

procedure THouseAction.ActionSet(aActionType: THouseActionType);
begin
fActionType:=aActionType;
  if aActionType=hat_Idle then begin
    SubActionRem([ha_Work1..ha_Smoke]); //remove all work attributes
    SubActionAdd([ha_Idle]);
  end;
  if aActionType=hat_Work then begin
    SubActionRem([ha_Idle]);
  end;
end;

procedure THouseAction.SubActionWork(aActionSet: THouseActionSet);
begin
  SubActionRem([ha_Work1..ha_Work5]);
  fSubAction:= fSubAction + aActionSet;
end;

procedure THouseAction.SubActionAdd(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction + aActionSet;
end;

procedure THouseAction.SubActionRem(aActionSet: THouseActionSet);
begin
  fSubAction:= fSubAction - aActionSet;
end;

procedure THouseAction.Execute(KMHouse: TKMHouse; TimeDelta: single; out DoEnd: Boolean);
begin
  DoEnd:= False;

  inc(KMHouse.AnimStep);

{  if (KMHouse.AnimStep>10) and (KMHouse.fCurrentAction=nil)
  then KMHouse.SetAction(THouseAction.Add(ha_Smoke));
  if (KMHouse.AnimStep>20)// and (KMHouse.fCurrentAction=nil)
  then KMHouse.SetAction(THouseAction.Add(ha_Smoke));  }
//  TMoveUnitAction.Create

//  DoEnd:= True;
end;

{ TKMHousesCollection }

procedure TKMHousesCollection.Add(aHouseType: THouseType; PosX,PosY:integer);
var i,k:integer; xo:integer;
begin
xo:=HouseXOffset[integer(aHouseType)];

//for i:=0 to 3 do for k:=0 to 3 do
//  if HousePlanYX[integer(aHouseType),i,k]<>0 then HitTest(PosX,PosY)
  case aHouseType of
    ht_Sawmill:         Inherited Add( TKMSawmill.Create(PosX+xo,PosY,aHouseType));
    ht_IronSmithy:      Inherited Add( TKMIronSmithy.Create(PosX+xo,PosY,aHouseType));
    ht_WeaponSmithy:    Inherited Add( TKMWeaponSmithy.Create(PosX+xo,PosY,aHouseType));
    ht_CoalMine:        Inherited Add( TKMCoalMine.Create(PosX+xo,PosY,aHouseType));
    ht_IronMine:        Inherited Add( TKMIronMine.Create(PosX+xo,PosY,aHouseType));
    ht_GoldMine:        Inherited Add( TKMGoldMine.Create(PosX+xo,PosY,aHouseType));
    ht_FisherHut:       Inherited Add( TKMFisherHut.Create(PosX+xo,PosY,aHouseType));
    ht_Bakery:          Inherited Add( TKMBakery.Create(PosX+xo,PosY,aHouseType));
    ht_Farm:            Inherited Add( TKMFarm.Create(PosX+xo,PosY,aHouseType));
    ht_Woodcutter:      Inherited Add( TKMWoodcutter.Create(PosX+xo,PosY,aHouseType));
    ht_ArmorSmithy:     Inherited Add( TKMArmorSmithy.Create(PosX+xo,PosY,aHouseType));
    ht_Store:           Inherited Add( TKMStore.Create(PosX+xo,PosY,aHouseType));
    ht_Stables:         Inherited Add( TKMStables.Create(PosX+xo,PosY,aHouseType));
    ht_School:          Inherited Add( TKMSchool.Create(PosX+xo,PosY,aHouseType));
    ht_Quary:           Inherited Add( TKMQuary.Create(PosX+xo,PosY,aHouseType));
    ht_Metallurgist:    Inherited Add( TKMMetallurgist.Create(PosX+xo,PosY,aHouseType));
    ht_Swine:           Inherited Add( TKMSwine.Create(PosX+xo,PosY,aHouseType));
    ht_WatchTower:      Inherited Add( TKMWatchTower.Create(PosX+xo,PosY,aHouseType));
    ht_TownHall:        Inherited Add( TKMTownHall.Create(PosX+xo,PosY,aHouseType));
    ht_WeaponWorkshop:  Inherited Add( TKMWeaponWorkshop.Create(PosX+xo,PosY,aHouseType));
    ht_ArmorWorkshop:   Inherited Add( TKMArmorWorkshop.Create(PosX+xo,PosY,aHouseType));
    ht_Barracks:        Inherited Add( TKMBarracks.Create(PosX+xo,PosY,aHouseType));
    ht_Mill:            Inherited Add( TKMMill.Create(PosX+xo,PosY,aHouseType));
    ht_SiegeWorkshop:   Inherited Add( TKMSiegeWorkshop.Create(PosX+xo,PosY,aHouseType));
    ht_Butchers:        Inherited Add( TKMButchers.Create(PosX+xo,PosY,aHouseType));
    ht_Tannery:         Inherited Add( TKMTannery.Create(PosX+xo,PosY,aHouseType));
//    ht_NA:              Inherited Add( TKMMill.Create(PosX+xo,PosY,aHouseType));
    ht_Inn:             Inherited Add( TKMInn.Create(PosX+xo,PosY,aHouseType));
    ht_Wineyard:        Inherited Add( TKMWineyard.Create(PosX+xo,PosY,aHouseType));
  end;
end;

procedure TKMHousesCollection.Rem(PosX,PosY:integer);
begin
  if HitTest(PosX,PosY)<>nil then Remove(HitTest(PosX,PosY));
end;

procedure TKMHousesCollection.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited;
end;

function TKMHousesCollection.HitTest(X, Y: Integer): TKMHouse;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if TKMHouse(Items[I]).HitTest(X, Y) then
    begin
      Result:= TKMHouse(Items[I]);
      Break;
    end;
  fSelectedHouse:= Result;
end;

procedure TKMHousesCollection.Paint();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMHouse(Items[I]).Paint();
end;

procedure TKMHousesCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMHouse(Items[I]).UpdateState;
end;

end.
