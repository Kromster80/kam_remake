unit KM_ResourceHouse;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Forms, Graphics, SysUtils, Math, dglOpenGL, KM_Defaults, KM_TextLibrary, Classes, KM_CommonTypes
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, Zstream {$ENDIF};


type
  THouseAnim = array[1..19] of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;

  THouseBuildSupply = array[1..12] of packed record MoveX,MoveY:integer; end;
  THouseSupply = array[1..4,1..5]of smallint;

  TKMHouseDat = packed record
    StonePic,WoodPic,WoodPal,StonePal:smallint;
    SupplyIn:THouseSupply;
    SupplyOut:THouseSupply;
    Anim:THouseAnim;
    WoodPicSteps,StonePicSteps:word;
    a1:smallint;
    EntranceOffsetX,EntranceOffsetY:shortint;
    EntranceOffsetXpx,EntranceOffsetYpx:shortint; //When entering house units go for the door, which is offset by these values
    BuildArea:array[1..10,1..10]of shortint;
    WoodCost,StoneCost:byte;
    BuildSupply:THouseBuildSupply;
    a5,SizeArea:smallint;
    SizeX,SizeY,sx2,sy2:shortint;
    WorkerWork,WorkerRest:smallint;
    ResInput,ResOutput:array[1..4]of shortint; //KaM_Remake will use it's own tables for this matter
    ResProductionX:shortint;
    MaxHealth,Sight:smallint;
    OwnerType:shortint;
    Foot1:array[1..12]of shortint; //Sound indices
    Foot2:array[1..12]of smallint; //vs sprite ID
  end;

  THouseArea = array[1..4,1..4]of byte;
  THouseRes = array[1..4]of TResourceType;

  //This class wraps KaM House info and hides unused fields
  TKMHouseDatClass = class
  private
    fHouseType:THouseType; //Our class
    fHouseDat:TKMHouseDat;
    function GetArea:THouseArea;
    function GetAcceptsGoods:boolean;
    function GetDoesOrders:boolean;
    function GetGUIIcon:word;
    function GetHouseName:string;
    function GetHouseUnlock:THouseTypeSet;
    function GetResInput:THouseRes;
    function GetResOutput:THouseRes;
    function GetOwnerType:TUnitType;
    function GetProducesGoods:boolean;
  public
    constructor Create(aHouseType:THouseType);
    function IsValid:boolean;
    procedure LoadFromStream(Stream:TMemoryStream);
    //Derived from KaM
    property StonePic:smallint read fHouseDat.StonePic;
    property WoodPic:smallint read fHouseDat.WoodPic;
    property WoodPal:smallint read fHouseDat.WoodPal;
    property StonePal:smallint read fHouseDat.StonePal;
    property SupplyIn:THouseSupply read fHouseDat.SupplyIn;
    property SupplyOut:THouseSupply read fHouseDat.SupplyOut;
    property Anim:THouseAnim read fHouseDat.Anim;
    property WoodPicSteps:word read fHouseDat.WoodPicSteps;
    property StonePicSteps:word read fHouseDat.StonePicSteps;
    property EntranceOffsetX:shortint read fHouseDat.EntranceOffsetX;
    property EntranceOffsetXpx:shortint read fHouseDat.EntranceOffsetXpx;
    property WoodCost:byte read fHouseDat.WoodCost;
    property StoneCost:byte read fHouseDat.StoneCost;
    property BuildSupply:THouseBuildSupply read fHouseDat.BuildSupply;
    property WorkerRest:smallint read fHouseDat.WorkerRest;
    property ResProductionX:shortint read fHouseDat.ResProductionX;
    property MaxHealth:smallint read fHouseDat.MaxHealth;
    property Sight:smallint read fHouseDat.Sight;
    property OwnerType:TUnitType read GetOwnerType;
    //Additional properties added by Remake
    property AcceptsGoods:boolean read GetAcceptsGoods;
    property BuildArea:THouseArea read GetArea;
    property DoesOrders:boolean read GetDoesOrders;
    property HouseName:string read GetHouseName;
    property HouseUnlock:THouseTypeSet read GetHouseUnlock;
    property GUIIcon:word read GetGUIIcon;
    property ProducesGoods:boolean read GetProducesGoods;
    property ResInput:THouseRes read GetResInput;
    property ResOutput:THouseRes read GetResOutput;
  end;

  //Swine&Horses, 5 beasts in each house, 3 ages for each beast
  TKMHouseBeastAnim = packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;

  TKMHouseDatCollection = class
  private
    fItems: array[THouseType] of TKMHouseDatClass;
    fBeastAnim: array[1..2,1..5,1..3] of TKMHouseBeastAnim;
    function GetHouseDat(aType:THouseType):TKMHouseDatClass;
    function GetBeastAnim(aType:THouseType; aBeast, aAge:integer):TKMHouseBeastAnim;
  public
    constructor Create;
    destructor Destroy; override;

    property HouseDat[aType:THouseType]:TKMHouseDatClass read GetHouseDat; default;
    property BeastAnim[aType:THouseType; aBeast, aAge:integer]:TKMHouseBeastAnim read GetBeastAnim;

    procedure LoadHouseDat(aPath:string);
    procedure ExportCSV(aPath: string);
  end;


const
  //1-building area //2-entrance
  HousePlanYX:array[THouseType] of THouseArea = (
    ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //0
    ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //0
    ((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1)), //Armor smithy
    ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1)), //Armor workshop
    ((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2)), //Bakery
    ((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Barracks
    ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2)), //Butchers
    ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0)), //Coal mine
    ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Farm
    ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1)), //Fisher hut
    ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0)), //Gold mine
    ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1)), //Inn
    ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1)), //Iron mine
    ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1)), //Iron smithy
    ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Metallurgist
    ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Mill
    ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Quarry
    ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Sawmill
    ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //School
    ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1)), //Siege workshop
    ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1)), //Stables
    ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Store
    ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2)), //Swine
    ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Tannery
    ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Town hall
    ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0)), //Watch tower
    ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon smithy
    ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon workshop
    ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2)),  //Wineyard
    ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0)) //Woodcutter
    );

  //Building of certain house allows player to build following houses,
  //unless they are blocked in mission script of course
  HouseRelease:array[THouseType]of THouseTypeSet = (
    [], [],
    [], [], [], [], [],
    [],
    [ht_Mill,ht_Swine,ht_Stables], //Farm
    [],
    [ht_Metallurgists], //GoldMine
    [ht_Quary], //Inn
    [ht_IronSmithy], //IronMine
    [ht_WeaponSmithy,ht_ArmorSmithy,ht_SiegeWorkshop], //IronSmithy
    [ht_TownHall], //Metallurgists
    [ht_Bakery], //Mill
    [ht_Woodcutters,ht_WatchTower], //Quary
    [ht_Farm, ht_Wineyard, ht_CoalMine, ht_IronMine, ht_GoldMine, ht_WeaponWorkshop, ht_Barracks, ht_FisherHut], //Sawmill
    [ht_Inn],  //School
    [],
    [],
    [ht_School], //Store
    [ht_Butchers,ht_Tannery], //Swine
    [ht_ArmorWorkshop],  //Tannery
    [],
    [],
    [],
    [],
    [],
    [ht_Sawmill] //Woodcutters
    );

  //Does house output needs to be ordered by Player or it keeps on producing by itself
  HouseDoesOrders_:array[THouseType] of boolean = (
    false, false,
    true, true, false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,true ,false,false,
    false,false,false,false,true ,true ,false,false);

  GUIBuildIcons_:array[0..HouseDatCount]of word = (
    0, //ht_None
    301, 302, 303, 304, 305,
    306, 307, 308, 309, 310,
    311, 312, 313, 314, 315,
    316, 317, 318, 319, 320,
    321, 322, 323, 324, 325,
    326, 327, 328, 329{, 338});


  //What does house produces
  HouseOutput_:array[0..HouseDatCount] of THouseRes = (
    (rt_None,       rt_None,       rt_None,       rt_None), //None
    (rt_Wood,       rt_None,       rt_None,       rt_None), //Sawmill
    (rt_Steel,      rt_None,       rt_None,       rt_None), //Iron smithy
    (rt_Sword,      rt_Hallebard,  rt_Arbalet,    rt_None), //Weapon smithy
    (rt_Coal,       rt_None,       rt_None,       rt_None), //Coal mine
    (rt_IronOre,    rt_None,       rt_None,       rt_None), //Iron mine
    (rt_GoldOre,    rt_None,       rt_None,       rt_None), //Gold mine
    (rt_Fish,       rt_None,       rt_None,       rt_None), //Fisher hut
    (rt_Bread,      rt_None,       rt_None,       rt_None), //Bakery
    (rt_Corn,       rt_None,       rt_None,       rt_None), //Farm
    (rt_Trunk,      rt_None,       rt_None,       rt_None), //Woodcutter
    (rt_MetalArmor, rt_MetalShield,rt_None,       rt_None), //Armor smithy
    (rt_All,        rt_None,       rt_None,       rt_None), //Store
    (rt_Horse,      rt_None,       rt_None,       rt_None), //Stables
    (rt_None,       rt_None,       rt_None,       rt_None), //School
    (rt_Stone,      rt_None,       rt_None,       rt_None), //Quarry
    (rt_Gold,       rt_None,       rt_None,       rt_None), //Metallurgist
    (rt_Pig,        rt_Skin,       rt_None,       rt_None), //Swine
    (rt_None,       rt_None,       rt_None,       rt_None), //Watch tower
    (rt_None,       rt_None,       rt_None,       rt_None), //Town hall
    (rt_Axe,        rt_Pike,       rt_Bow,        rt_None), //Weapon workshop
    (rt_Shield,     rt_Armor,      rt_None,       rt_None), //Armor workshop
    (rt_None,       rt_None,       rt_None,       rt_None), //Barracks
    (rt_Flour,      rt_None,       rt_None,       rt_None), //Mill
    (rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop
    (rt_Sausages,   rt_None,       rt_None,       rt_None), //Butcher
    (rt_Leather,    rt_None,       rt_None,       rt_None), //Tannery
    (rt_None,       rt_None,       rt_None,       rt_None), //N/A
    (rt_None,       rt_None,       rt_None,       rt_None), //Inn
    (rt_Wine,       rt_None,       rt_None,       rt_None) //Wineyard
    );

  //What house requires
  HouseInput_:array[0..HouseDatCount] of THouseRes = (
    (rt_None,       rt_None,       rt_None,       rt_None), //None
    (rt_Trunk,      rt_None,       rt_None,       rt_None), //Sawmill
    (rt_IronOre,    rt_Coal,       rt_None,       rt_None), //Iron smithy
    (rt_Coal,       rt_Steel,      rt_None,       rt_None), //Weapon smithy
    (rt_None,       rt_None,       rt_None,       rt_None), //Coal mine
    (rt_None,       rt_None,       rt_None,       rt_None), //Iron mine
    (rt_None,       rt_None,       rt_None,       rt_None), //Gold mine
    (rt_None,       rt_None,       rt_None,       rt_None), //Fisher hut
    (rt_Flour,      rt_None,       rt_None,       rt_None), //Bakery
    (rt_None,       rt_None,       rt_None,       rt_None), //Farm
    (rt_None,       rt_None,       rt_None,       rt_None), //Woodcutter
    (rt_Steel,      rt_Coal,       rt_None,       rt_None), //Armor smithy
    (rt_All,        rt_None,       rt_None,       rt_None), //Store
    (rt_Corn,       rt_None,       rt_None,       rt_None), //Stables
    (rt_Gold,       rt_None,       rt_None,       rt_None), //School
    (rt_None,       rt_None,       rt_None,       rt_None), //Quarry
    (rt_GoldOre,    rt_Coal,       rt_None,       rt_None), //Metallurgist
    (rt_Corn,       rt_None,       rt_None,       rt_None), //Swine
    (rt_Stone,      rt_None,       rt_None,       rt_None), //Watch tower
    (rt_Gold,       rt_None,       rt_None,       rt_None), //Town hall
    (rt_Wood,       rt_None,       rt_None,       rt_None), //Weapon workshop
    (rt_Wood,       rt_Leather,    rt_None,       rt_None), //Armor workshop
    (rt_Warfare,    rt_None,       rt_None,       rt_None), //Barracks
    (rt_Corn,       rt_None,       rt_None,       rt_None), //Mill
    (rt_Wood,       rt_Steel,      rt_None,       rt_None), //Siege workshop
    (rt_Pig,        rt_None,       rt_None,       rt_None), //Butcher
    (rt_Skin,       rt_None,       rt_None,       rt_None), //Tannery
    (rt_None,       rt_None,       rt_None,       rt_None), //N/A
    (rt_Bread,      rt_Sausages,   rt_Wine,       rt_Fish), //Inn
    (rt_None,       rt_None,       rt_None,       rt_None) //Wineyard
    );


implementation
uses KromUtils, KM_Utils;


{ TKMHouseDatClass }
constructor TKMHouseDatClass.Create(aHouseType: THouseType);
begin
  Inherited Create;
  fHouseType := aHouseType;
end;


function TKMHouseDatClass.GetAcceptsGoods: boolean;
begin
  Result := not (ResInput[1] in [rt_None,rt_All,rt_Warfare]); //Exclude aggregate types
end;

function TKMHouseDatClass.GetArea: THouseArea;
begin
  Result := HousePlanYX[fHouseType];
end;


function TKMHouseDatClass.GetDoesOrders: boolean;
begin
  Result := HouseDoesOrders_[fHouseType];
end;


function TKMHouseDatClass.GetGUIIcon: word;
begin
  Result := GUIBuildIcons_[HouseKaMOrder[fHouseType]];
end;


function TKMHouseDatClass.GetHouseName: string;
begin
  if IsValid then
    Result := fTextLibrary.GetTextString(siHouseNames+HouseKaMOrder[fHouseType])
  else
    Result := 'N/A';
end;


function TKMHouseDatClass.GetHouseUnlock: THouseTypeSet;
begin
  Result := HouseRelease[fHouseType];
end;


function TKMHouseDatClass.GetOwnerType: TUnitType;
begin
  Result := TUnitType(fHouseDat.OwnerType+1);
end;


function TKMHouseDatClass.GetProducesGoods: boolean;
begin
  Result := not (ResOutput[1] in [rt_None,rt_All,rt_Warfare]); //Exclude aggregate types
end;

function TKMHouseDatClass.GetResInput: THouseRes;
begin
  Result := HouseInput_[HouseKaMOrder[fHouseType]];
end;

function TKMHouseDatClass.GetResOutput: THouseRes;
begin
  Result := HouseOutput_[HouseKaMOrder[fHouseType]];
end;

function TKMHouseDatClass.IsValid: boolean;
begin
  Result := (fHouseType in [Low(THouseType)..High(THouseType)]-[ht_None, ht_Any]);
end;


procedure TKMHouseDatClass.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fHouseDat, SizeOf(TKMHouseDat));
end;


{ TKMHouseDatCollection }
constructor TKMHouseDatCollection.Create;
var H:THouseType;
begin
  Inherited;

  for H := Low(THouseType) to High(THouseType) do
    fItems[H] := TKMHouseDatClass.Create(H);
end;


destructor TKMHouseDatCollection.Destroy;
var H:THouseType;
begin
  for H := Low(THouseType) to High(THouseType) do
    FreeAndNil(fItems[H]);

  Inherited;
end;


function TKMHouseDatCollection.GetHouseDat(aType:THouseType): TKMHouseDatClass;
begin
  Result := fItems[aType];
end;


function TKMHouseDatCollection.GetBeastAnim(aType:THouseType; aBeast, aAge: integer): TKMHouseBeastAnim;
begin
  Assert(aType in [ht_Swine, ht_Stables]);
  Assert(InRange(aBeast, 1, 5));
  Assert(InRange(aAge, 1, 3));
  case aType of
    ht_Swine:   Result := fBeastAnim[1, aBeast, aAge];
    ht_Stables: Result := fBeastAnim[2, aBeast, aAge];
  end;
end;


procedure TKMHouseDatCollection.LoadHouseDat(aPath: string);
var
  S:TKMemoryStream;
  i:integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStream.Create;
  S.LoadFromFile(aPath);

  S.Read(fBeastAnim, SizeOf(fBeastAnim){30*70}); //Swine&Horses animations

  //Read the records one by one because we need to reorder them and skip one in the middle
  for i:=0 to HouseDatCount-1 do
  if HouseKaMType[i] <> ht_None then
    fItems[HouseKaMType[i]].LoadFromStream(S)
  else
    S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

  S.Free;

  //ExportCSV(ExeDir+'Houses.csv');
end;


procedure TKMHouseDatCollection.ExportCSV(aPath: string);
var i:THouseType; Ap:string; S:TStringList; j,k:integer;
  procedure AddField(aField:string); overload; begin Ap := Ap + aField + ';'; end;
  procedure AddField(aField:integer); overload; begin Ap := Ap + inttostr(aField) + ';'; end;
begin
  S := TStringList.Create;

  Ap := 'House name;WoodCost;StoneCost';
  S.Append(Ap);

  for i:=Low(THouseType) to High(THouseType) do begin
    Ap := '';
    AddField(fItems[i].HouseName);
    S.Append(Ap);
    for j := 1 to 10 do
    begin
      Ap := '';
      for k:=1 to 10 do
        AddField(fItems[i].BuildArea[j,k]);
      S.Append(Ap);
    end;
    Ap := '';
    AddField(fItems[i].StoneCost);
    S.Append(Ap);
  end;

  S.SaveToFile(aPath);
  S.Free;
end;
end.
