unit KM_ResourceHouse;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KM_CommonTypes, KM_Defaults;


type
  THouseAnim = array[THouseActionType] of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;

  THouseBuildSupply = array[1..2,1..6] of packed record MoveX,MoveY:integer; end;
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
    function GetMaxHealth:word;
    function GetResInput:THouseRes;
    function GetResOutput:THouseRes;
    function GetOwnerType:TUnitType;
    function GetProducesGoods:boolean;
    function GetReleasedBy:THouseType;
    function GetTabletIcon:word;
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
    property Sight:smallint read fHouseDat.Sight;
    property OwnerType:TUnitType read GetOwnerType;
    //Additional properties added by Remake
    property AcceptsGoods:boolean read GetAcceptsGoods;
    property BuildArea:THouseArea read GetArea;
    property DoesOrders:boolean read GetDoesOrders;
    property GUIIcon:word read GetGUIIcon;
    property HouseName:string read GetHouseName;
    property MaxHealth:word read GetMaxHealth;
    property ProducesGoods:boolean read GetProducesGoods;
    property ReleasedBy:THouseType read GetReleasedBy;
    property ResInput:THouseRes read GetResInput;
    property ResOutput:THouseRes read GetResOutput;
    property TabletIcon:word read GetTabletIcon;
  end;

  //Swine&Horses, 5 beasts in each house, 3 ages for each beast
  TKMHouseBeastAnim = packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;

  TKMHouseDatCollection = class
  private
    fCRC:cardinal;
    fItems: array[THouseType] of TKMHouseDatClass;
    fBeastAnim: array[1..2,1..5,1..3] of TKMHouseBeastAnim;
    function LoadHouseDat(aPath: string):Cardinal;
    function GetHouseDat(aType:THouseType):TKMHouseDatClass;
    function GetBeastAnim(aType:THouseType; aBeast, aAge:integer):TKMHouseBeastAnim;
  public
    constructor Create;
    destructor Destroy; override;

    property HouseDat[aType:THouseType]:TKMHouseDatClass read GetHouseDat; default;
    property BeastAnim[aType:THouseType; aBeast, aAge:integer]:TKMHouseBeastAnim read GetBeastAnim;
    property CRC:cardinal read fCRC; //Return hash of all values

    procedure ExportCSV(aPath: string);
  end;

type
  THouseInfo = record
    PlanYX:THouseArea;
    DoesOrders:byte; //Does house output needs to be ordered by Player or it's producing by itself
    BuildIcon:word;  //Icon in GUI
    TabletIcon:word; //House area WIP tablet
    Input:THouseRes;
    Output:THouseRes;
    ReleasedBy:THouseType; //Which house type allows to build this house type
  end;

const
  //Remake stores additional house properties here. This looks like House.Dat, but hardcoded.
  //I listed all fields explicitely except for ht_None/ht_Any to be sure nothing is forgotten
  HouseDatX:array[THouseType] of THouseInfo = (
    ( //None
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0));
    DoesOrders: 0;
    BuildIcon:  0;
    TabletIcon: 0;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_None;
    ),
    ( //Any
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0));
    DoesOrders: 0;
    BuildIcon:  0;
    TabletIcon: 0;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_None;
    ),
    ( //Armor smithy
    PlanYX:     ((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1));
    DoesOrders: 1;
    BuildIcon:  311;
    TabletIcon: 261;
    Input:      (rt_Steel,      rt_Coal,       rt_None,       rt_None);
    Output:     (rt_MetalArmor, rt_MetalShield,rt_None,       rt_None);
    ReleasedBy: ht_IronSmithy;
    ),
    ( //Armor workshop
    PlanYX:     ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1));
    DoesOrders: 1;
    BuildIcon:  321;
    TabletIcon: 271;
    Input:      (rt_Wood,       rt_Leather,    rt_None,       rt_None);
    Output:     (rt_Shield,     rt_Armor,      rt_None,       rt_None);
    ReleasedBy: ht_Tannery;
    ),
    ( //Bakery
    PlanYX:     ((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2));
    DoesOrders: 0;
    BuildIcon:  308;
    TabletIcon: 258;
    Input:      (rt_Flour,      rt_None,       rt_None,       rt_None);
    Output:     (rt_Bread,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Mill;
    ),
    ( //Barracks
    PlanYX:     ((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    DoesOrders: 0;
    BuildIcon:  322;
    TabletIcon: 272;
    Input:      (rt_Warfare,    rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Butchers
    PlanYX:     ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2));
    DoesOrders: 0;
    BuildIcon:  325;
    TabletIcon: 275;
    Input:      (rt_Pig,        rt_None,       rt_None,       rt_None);
    Output:     (rt_Sausages,   rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Swine;
    ),
    ( //Coal mine
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0));
    DoesOrders: 0;
    BuildIcon:  304;
    TabletIcon: 254;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Coal,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Farm
    PlanYX:     ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    DoesOrders: 0;
    BuildIcon:  309;
    TabletIcon: 259;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Corn,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Fisher hut
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1));
    DoesOrders: 0;
    BuildIcon:  307;
    TabletIcon: 257;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Fish,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Gold mine
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0));
    DoesOrders: 0;
    BuildIcon:  306;
    TabletIcon: 256;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_GoldOre,    rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Inn
    PlanYX:     ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1));
    DoesOrders: 0;
    BuildIcon:  328;
    TabletIcon: 278;
    Input:      (rt_Bread,      rt_Sausages,   rt_Wine,       rt_Fish);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_School;
    ),
    ( //Iron mine
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1));
    DoesOrders: 0;
    BuildIcon:  305;
    TabletIcon: 255;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_IronOre,    rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Iron smithy
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1));
    DoesOrders: 0;
    BuildIcon:  302;
    TabletIcon: 252;
    Input:      (rt_IronOre,    rt_Coal,       rt_None,       rt_None);
    Output:     (rt_Steel,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_IronMine;
    ),
    ( //Marketplace
    PlanYX:     ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    DoesOrders: 0;
    BuildIcon:  327;
    TabletIcon: 277;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Store;//Metallurgists;
    ),
    ( //Metallurgist
    PlanYX:     ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    DoesOrders: 0;
    BuildIcon:  316;
    TabletIcon: 266;
    Input:      (rt_GoldOre,    rt_Coal,       rt_None,       rt_None);
    Output:     (rt_Gold,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_GoldMine;
    ),
    ( //Mill
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    DoesOrders: 0;
    BuildIcon:  323;
    TabletIcon: 273;
    Input:      (rt_Corn,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Flour,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Farm;
    ),
    ( //Quarry
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    DoesOrders: 0;
    BuildIcon:  315;
    TabletIcon: 265;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Stone,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Inn;
    ),
    ( //Sawmill
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    DoesOrders: 0;
    BuildIcon:  301;
    TabletIcon: 251;
    Input:      (rt_Trunk,      rt_None,       rt_None,       rt_None);
    Output:     (rt_Wood,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Woodcutters;
    ),
    ( //School
    PlanYX:     ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    DoesOrders: 0;
    BuildIcon:  314;
    TabletIcon: 264;
    Input:      (rt_Gold,       rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Store;
    ),
    ( //Siege workshop
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1));
    DoesOrders: 1;
    BuildIcon:  324;
    TabletIcon: 274;
    Input:      (rt_Wood,       rt_Steel,      rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_IronSmithy;
    ),
    ( //Stables
    PlanYX:     ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1));
    DoesOrders: 0;
    BuildIcon:  313;
    TabletIcon: 263;
    Input:      (rt_Corn,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Horse,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Farm;
    ),
    ( //Store
    PlanYX:     ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    DoesOrders: 0;
    BuildIcon:  312;
    TabletIcon: 262;
    Input:      (rt_All,        rt_None,       rt_None,       rt_None);
    Output:     (rt_All,        rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_None; //
    ),
    ( //Swine
    PlanYX:     ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    DoesOrders: 0;
    BuildIcon:  317;
    TabletIcon: 267;
    Input:      (rt_Corn,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Pig,        rt_Skin,       rt_None,       rt_None);
    ReleasedBy: ht_Farm;
    ),
    ( //Tannery
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    DoesOrders: 0;
    BuildIcon:  326;
    TabletIcon: 276;
    Input:      (rt_Skin,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Leather,    rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Swine;
    ),
    ( //Town hall
    PlanYX:     ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    DoesOrders: 0;
    BuildIcon:  319;
    TabletIcon: 269;
    Input:      (rt_Gold,       rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Metallurgists;
    ),
    ( //Watch tower
    PlanYX:   ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    DoesOrders: 0;
    BuildIcon:  318;
    TabletIcon: 268;
    Input:      (rt_Stone,      rt_None,       rt_None,       rt_None);
    Output:     (rt_None,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Quary;
    ),
    ( //Weapon smithy
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    DoesOrders: 1;
    BuildIcon:  303;
    TabletIcon: 253;
    Input:      (rt_Coal,       rt_Steel,      rt_None,       rt_None);
    Output:     (rt_Sword,      rt_Hallebard,  rt_Arbalet,    rt_None);
    ReleasedBy: ht_IronSmithy;
    ),
    ( //Weapon workshop
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    DoesOrders: 1;
    BuildIcon:  320;
    TabletIcon: 270;
    Input:      (rt_Wood,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Axe,        rt_Pike,       rt_Bow,        rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Wineyard
    PlanYX:     ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2));
    DoesOrders: 0;
    BuildIcon:  329;
    TabletIcon: 279;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Wine,       rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Sawmill;
    ),
    ( //Woodcutter
    PlanYX:     ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0));
    DoesOrders: 0;
    BuildIcon:  310;
    TabletIcon: 260;
    Input:      (rt_None,       rt_None,       rt_None,       rt_None);
    Output:     (rt_Trunk,      rt_None,       rt_None,       rt_None);
    ReleasedBy: ht_Quary;
    )
    );

  //These tables are used to convert between KaM script IDs and Remake ebums
  HouseDatCount = 29;
  //KaM scripts and HouseDat address houses in this order
  HouseKaMType: array[0..HouseDatCount-1] of THouseType = (
  ht_Sawmill, ht_IronSmithy, ht_WeaponSmithy, ht_CoalMine, ht_IronMine,
  ht_GoldMine, ht_FisherHut, ht_Bakery, ht_Farm, ht_Woodcutters,
  ht_ArmorSmithy, ht_Store, ht_Stables, ht_School, ht_Quary,
  ht_Metallurgists, ht_Swine, ht_WatchTower, ht_TownHall, ht_WeaponWorkshop,
  ht_ArmorWorkshop, ht_Barracks, ht_Mill, ht_SiegeWorkshop, ht_Butchers,
  ht_Tannery, ht_None, ht_Inn, ht_Wineyard);

  //THouseType corresponds to this index in KaM scripts and libs
  //KaM scripts are 0 based, so we must use HouseKaMOrder[H]-1 in script usage. Other cases are 1 based.
  HouseKaMOrder: array[THouseType] of byte = (0, 0,
  11, 21, 8, 22, 25, 4, 9, 7, 6, 28,
  5, 2, 30, 16, 23, 15, 1, 14, 24, 13, 12,
  17, 26, 19, 18, 3, 20, 29, 10);


implementation
uses KromUtils, KM_TextLibrary, KM_ResourceUnit;


{ TKMHouseDatClass }
constructor TKMHouseDatClass.Create(aHouseType: THouseType);
begin
  Inherited Create;
  fHouseType := aHouseType;
end;


function TKMHouseDatClass.GetAcceptsGoods: boolean;
begin
  Result := not (ResInput[1] in [rt_None]); //Exclude houses that do not receive resources
end;


function TKMHouseDatClass.GetArea: THouseArea;
begin
  Result := HouseDatX[fHouseType].PlanYX;
end;


function TKMHouseDatClass.GetDoesOrders: boolean;
begin
  Result := HouseDatX[fHouseType].DoesOrders <> 0;
end;


function TKMHouseDatClass.GetGUIIcon: word;
begin
  Result := HouseDatX[fHouseType].BuildIcon;
end;


function TKMHouseDatClass.GetHouseName: string;
begin
  if IsValid then
    Result := fTextLibrary.GetTextString(siHouseNames+HouseKaMOrder[fHouseType])
  else
    Result := 'N/A';
end;


//MaxHealth is always a cost of construction * 50
function TKMHouseDatClass.GetMaxHealth: word;
begin
  Result := (fHouseDat.WoodCost + fHouseDat.StoneCost) * 50;
end;


//fHouseDat.OwnerType is read from DAT file and is shortint, it can be out of range (i.e. -1)
function TKMHouseDatClass.GetOwnerType: TUnitType;
begin
  if InRange(fHouseDat.OwnerType, Low(UnitKaMType), High(UnitKaMType)) then
    Result := UnitKaMType[fHouseDat.OwnerType]
  else
    Result := ut_None;
end;


function TKMHouseDatClass.GetProducesGoods: boolean;
begin
  Result := not (ResOutput[1] in [rt_None,rt_All,rt_Warfare]); //Exclude aggregate types
end;


function TKMHouseDatClass.GetReleasedBy: THouseType;
begin
  Result := HouseDatX[fHouseType].ReleasedBy;
end;


function TKMHouseDatClass.GetResInput: THouseRes;
begin
  Result := HouseDatX[fHouseType].Input;
end;


function TKMHouseDatClass.GetResOutput: THouseRes;
begin
  Result := HouseDatX[fHouseType].Output;
end;


function TKMHouseDatClass.GetTabletIcon: word;
begin
  Result := HouseDatX[fHouseType].TabletIcon;
end;


function TKMHouseDatClass.IsValid: boolean;
begin
  Result := not (fHouseType in [ht_None, ht_Any]);
end;


procedure TKMHouseDatClass.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fHouseDat, SizeOf(TKMHouseDat));
end;


{ TKMHouseDatCollection }
constructor TKMHouseDatCollection.Create;
var H:THouseType; i:integer;
begin
  Inherited;

  for H := Low(THouseType) to High(THouseType) do
    fItems[H] := TKMHouseDatClass.Create(H);

  fCRC := LoadHouseDat(ExeDir+'data\defines\houses.dat');

  fItems[ht_Marketplace].fHouseType := ht_Marketplace;
  fItems[ht_Marketplace].fHouseDat.StonePic := 150;
  fItems[ht_Marketplace].fHouseDat.WoodPic := 151;
  fItems[ht_Marketplace].fHouseDat.WoodPal := 152;
  fItems[ht_Marketplace].fHouseDat.StonePal := 153;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,1] := 154;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,2] := 155;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,3] := 156;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,4] := 157;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,5] := 158;
  fItems[ht_Marketplace].fHouseDat.SupplyOut[1,1] := 159;
  fItems[ht_Marketplace].fHouseDat.SupplyOut[1,2] := 160;
  fItems[ht_Marketplace].fHouseDat.SupplyOut[1,3] := 161;
  fItems[ht_Marketplace].fHouseDat.SupplyOut[1,4] := 162;
  fItems[ht_Marketplace].fHouseDat.SupplyOut[1,5] := 163;
  fItems[ht_Marketplace].fHouseDat.WoodPicSteps := 23;
  fItems[ht_Marketplace].fHouseDat.StonePicSteps := 140;
  fItems[ht_Marketplace].fHouseDat.EntranceOffsetX := 1;
  fItems[ht_Marketplace].fHouseDat.EntranceOffsetXpx := 0;
  fItems[ht_Marketplace].fHouseDat.WoodCost := 5;
  fItems[ht_Marketplace].fHouseDat.StoneCost := 6;
  for i:=1 to 6 do begin
    fItems[ht_Marketplace].fHouseDat.BuildSupply[1,i].MoveX := -55;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[1,i].MoveY := 15;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[2,i].MoveX := 28;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[2,i].MoveY := 15;
  end;
  fItems[ht_Marketplace].fHouseDat.Sight := 9;

  //ExportCSV(ExeDir+'Houses.csv');
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


//Return CRC of loaded file
//CRC should be calculated right away, cos file may be swapped after loading
function TKMHouseDatCollection.LoadHouseDat(aPath: string):Cardinal;
var
  S:TKMemoryStream;
  i:integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(aPath);

    S.Read(fBeastAnim, SizeOf(fBeastAnim){30*70}); //Swine&Horses animations

    //Read the records one by one because we need to reorder them and skip one in the middle
    for i:=0 to HouseDatCount-1 do
    if HouseKaMType[i] <> ht_None then
      fItems[HouseKaMType[i]].LoadFromStream(S)
    else
      S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
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
