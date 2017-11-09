unit KM_ResHouses;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults,
  KM_ResWares;


type
  THouseType = (ht_None, ht_Any,
    ht_ArmorSmithy,     ht_ArmorWorkshop,   ht_Bakery,        ht_Barracks,      ht_Butchers,
    ht_CoalMine,        ht_Farm,            ht_FisherHut,     ht_GoldMine,      ht_Inn,
    ht_IronMine,        ht_IronSmithy,      ht_Marketplace,   ht_Metallurgists, ht_Mill,
    ht_Quary,           ht_Sawmill,         ht_School,        ht_SiegeWorkshop, ht_Stables,
    ht_Store,           ht_Swine,           ht_Tannery,       ht_TownHall,      ht_WatchTower,
    ht_WeaponSmithy,    ht_WeaponWorkshop,  ht_Wineyard,      ht_Woodcutters    );

  THouseTypeSet = set of THouseType;

const
  HOUSE_MIN = ht_ArmorSmithy;
  HOUSE_MAX = ht_Woodcutters;

type
  THouseAnim = array [THouseActionType] of TKMAnimLoop;

  THouseBuildSupply = array [1..2,1..6] of packed record MoveX, MoveY: Integer; end;
  THouseSupply = array [1..4, 1..5] of SmallInt;

  //House fields as they are in a DAT file
  TKMHouseDat = packed record
    StonePic, WoodPic, WoodPal, StonePal: SmallInt;
    SupplyIn: THouseSupply;
    SupplyOut: THouseSupply;
    Anim: THouseAnim;
    WoodPicSteps, StonePicSteps: Word;
    a1: SmallInt;
    EntranceOffsetX, EntranceOffsetY: ShortInt;
    EntranceOffsetXpx, EntranceOffsetYpx: ShortInt; //When entering house units go for the door, which is offset by these values
    BuildArea: array [1..10,1..10] of ShortInt;
    WoodCost,StoneCost: Byte;
    BuildSupply: THouseBuildSupply;
    a5,SizeArea: SmallInt;
    SizeX,SizeY,sx2,sy2: ShortInt;
    WorkerWork,WorkerRest: SmallInt;
    ResInput,ResOutput: array [1..4] of ShortInt; //KaM_Remake will use its own tables for this matter
    ResProductionX: ShortInt;
    MaxHealth,Sight: SmallInt;
    OwnerType: ShortInt;
    Foot1: array [1..12] of ShortInt; //Sound indices
    Foot2: array [1..12] of SmallInt; //vs sprite ID
  end;

  THouseArea = array [1..4, 1..4] of Byte;
  THouseRes = array [1..4] of TWareType;

  //This class wraps KaM House info
  //it hides unused fields and adds new ones
  TKMHouseSpec = class
  private
    fHouseType: THouseType; //Our class
    fNameTextID: Integer;
    fHouseDat: TKMHouseDat;
    function GetArea: THouseArea;
    function GetDoesOrders: Boolean;
    function GetGUIIcon: Word;
    function GetHouseName: UnicodeString;
    function GetResInput: THouseRes;
    function GetResOutput: THouseRes;
    function GetOwnerType: TUnitType;
    function GetReleasedBy: THouseType;
    function GetTabletIcon: Word;
    function GetSnowPic: SmallInt;
    function GetUnoccupiedMsgId: SmallInt;
  public
    constructor Create(aHouseType: THouseType);
    procedure LoadFromStream(Stream: TMemoryStream);
    //Property accessors:
    //Derived from KaM
    property StonePic:smallint read fHouseDat.StonePic;
    property WoodPic:smallint read fHouseDat.WoodPic;
    property WoodPal:smallint read fHouseDat.WoodPal;
    property StonePal:smallint read fHouseDat.StonePal;
    property SupplyIn: THouseSupply read fHouseDat.SupplyIn;
    property SupplyOut: THouseSupply read fHouseDat.SupplyOut;
    property Anim: THouseAnim read fHouseDat.Anim;
    property WoodPicSteps:word read fHouseDat.WoodPicSteps;
    property StonePicSteps:word read fHouseDat.StonePicSteps;
    property EntranceOffsetX:shortint read fHouseDat.EntranceOffsetX;
    property EntranceOffsetXpx:shortint read fHouseDat.EntranceOffsetXpx;
    property EntranceOffsetYpx:shortint read fHouseDat.EntranceOffsetYpx;
    property WoodCost:byte read fHouseDat.WoodCost;
    property StoneCost:byte read fHouseDat.StoneCost;
    property BuildSupply: THouseBuildSupply read fHouseDat.BuildSupply;
    property WorkerRest:smallint read fHouseDat.WorkerRest;
    property ResProductionX:shortint read fHouseDat.ResProductionX;
    property Sight:smallint read fHouseDat.Sight;
    property OwnerType: TUnitType read GetOwnerType;
    //Additional properties added by Remake
    property BuildArea: THouseArea read GetArea;
    property DoesOrders:boolean read GetDoesOrders;
    property GUIIcon:word read GetGUIIcon;
    property HouseName: UnicodeString read GetHouseName;
    property HouseNameTextID: Integer read fNameTextID;
    property ReleasedBy: THouseType read GetReleasedBy;
    property ResInput: THouseRes read GetResInput;
    property ResOutput: THouseRes read GetResOutput;
    property TabletIcon:word read GetTabletIcon;
    property UnoccupiedMsgId:SmallInt read GetUnoccupiedMsgId;
    property SnowPic: SmallInt read GetSnowPic;
    //Functions
    function AcceptsWares: Boolean;
    function MaxHealth: Word;
    function ProducesWares: Boolean;
    procedure Outline(aList: TKMPointList);
  end;


  TKMResHouses = class
  private
    fCRC: Cardinal;
    fItems: array [HOUSE_MIN..HOUSE_MAX] of TKMHouseSpec;
    //Swine&Horses, 5 beasts in each house, 3 ages for each beast
    fBeastAnim: array [1..2,1..5,1..3] of TKMAnimLoop;
    fMarketBeastAnim: array [1..3] of TKMAnimLoop;
    function LoadHouseDat(aPath: string): Cardinal;
    function GetHouseDat(aType: THouseType): TKMHouseSpec; inline;
    function GetBeastAnim(aType: THouseType; aBeast, aAge:integer): TKMAnimLoop;
  public
    constructor Create;
    destructor Destroy; override;

    function IsValid(aType: THouseType): Boolean;

    property HouseDat[aType: THouseType]: TKMHouseSpec read GetHouseDat; default;
    property BeastAnim[aType: THouseType; aBeast, aAge: Integer]: TKMAnimLoop read GetBeastAnim;
    property CRC: Cardinal read fCRC; //Return hash of all values

    procedure ExportCSV(aPath: string);
  end;


const
  //Sprites in the marketplace
  MarketWaresOffsetX = -93;
  MarketWaresOffsetY = -88;
  MarketWareTexStart = 1724; //ID of where market ware sprites start. Allows us to relocate them easily.
  MarketWares: array[TWareType] of record
                                         TexStart: Integer; //Tex ID for first sprite
                                         Count: Integer; //Total sprites for this resource
                                       end
  = (
      (TexStart: 0; Count: 0;), //rt_None

      (TexStart: MarketWareTexStart+237; Count: 20;), //rt_Trunk
      (TexStart: MarketWareTexStart+47;  Count: 36;), //rt_Stone
      (TexStart: MarketWareTexStart+94;  Count: 19;), //rt_Wood
      (TexStart: MarketWareTexStart+113; Count: 11;), //rt_IronOre
      (TexStart: MarketWareTexStart+135; Count: 12;), //rt_GoldOre
      (TexStart: MarketWareTexStart+207; Count: 11;), //rt_Coal
      (TexStart: MarketWareTexStart+130; Count: 5;),  //rt_Steel
      (TexStart: MarketWareTexStart+147; Count: 9;),  //rt_Gold
      (TexStart: MarketWareTexStart+1;   Count: 23;), //rt_Wine
      (TexStart: MarketWareTexStart+24;  Count: 23;), //rt_Corn
      (TexStart: MarketWareTexStart+218; Count: 12;), //rt_Bread
      (TexStart: MarketWareTexStart+186; Count: 12;), //rt_Flour
      (TexStart: MarketWareTexStart+156; Count: 9;),  //rt_Leather
      (TexStart: MarketWareTexStart+283; Count: 16;), //rt_Sausages
      (TexStart: MarketWareTexStart+299; Count: 6;),  //rt_Pig
      (TexStart: MarketWareTexStart+230; Count: 7;),  //rt_Skin
      (TexStart: MarketWareTexStart+85;  Count: 9;),  //rt_Shield
      (TexStart: MarketWareTexStart+127; Count: 3;),  //rt_MetalShield
      (TexStart: MarketWareTexStart+165; Count: 6;),  //rt_Armor
      (TexStart: MarketWareTexStart+124; Count: 3;),  //rt_MetalArmor
      (TexStart: MarketWareTexStart+201; Count: 6;),  //rt_Axe
      (TexStart: MarketWareTexStart+183; Count: 3;),  //rt_Sword
      (TexStart: MarketWareTexStart+171; Count: 6;),  //rt_Pike
      (TexStart: MarketWareTexStart+198; Count: 3;),  //rt_Hallebard
      (TexStart: MarketWareTexStart+177; Count: 6;),  //rt_Bow
      (TexStart: MarketWareTexStart+83;  Count: 2;),  //rt_Arbalet
      (TexStart: 0;                      Count: 2;),  //rt_Horse (defined in fMarketBeastAnim)
      (TexStart: MarketWareTexStart+305; Count: 19;), //rt_Fish

      (TexStart: 0; Count: 0;), //rt_All
      (TexStart: 0; Count: 0;), //rt_Warfare
      (TexStart: 0; Count: 0;)  //rt_Food
    );

  //These tables are used to convert between KaM script IDs and Remake enums
  HOUSE_DAT_COUNT = 30;
  //KaM scripts and HouseDat address houses in this order
  HouseIndexToType: array [0 .. HOUSE_DAT_COUNT - 1] of THouseType = (
    ht_Sawmill, ht_IronSmithy, ht_WeaponSmithy, ht_CoalMine, ht_IronMine,
    ht_GoldMine, ht_FisherHut, ht_Bakery, ht_Farm, ht_Woodcutters,
    ht_ArmorSmithy, ht_Store, ht_Stables, ht_School, ht_Quary,
    ht_Metallurgists, ht_Swine, ht_WatchTower, ht_TownHall, ht_WeaponWorkshop,
    ht_ArmorWorkshop, ht_Barracks, ht_Mill, ht_SiegeWorkshop, ht_Butchers,
    ht_Tannery, ht_None, ht_Inn, ht_Wineyard, ht_Marketplace);

  //THouseType corresponds to this index in KaM scripts and libs
  //KaM scripts are 0 based, so we must use HouseTypeToIndex[H]-1 in script usage. Other cases are 1 based.
  HouseTypeToIndex: array [THouseType] of Byte = (0, 0,
    11, 21, 8, 22, 25, 4, 9, 7, 6, 28,
    5, 2, 30, 16, 23, 15, 1, 14, 24, 13, 12,
    17, 26, 19, 18, 3, 20, 29, 10);


implementation
uses
  KromUtils, KM_Outline, KM_Points, KM_PolySimplify, KM_ResTexts, KM_ResUnits;


type
  //Additional house info appended to classic format
  THouseInfo = record
    PlanYX: THouseArea;
    NeedsPlayerOrder: Boolean; //Does house output needs to be ordered by Player or it's producing by itself
    BuildIcon: Word;  //Icon in GUI
    TabletSpriteId: Word; //House area WIP tablet
    Input: THouseRes;
    Output: THouseRes;
    UnlockedByHouse: THouseType; //Which house type allows to build this house type
    SnowSpriteId: SmallInt;
  end;

const
  //Remake stores additional house properties here. This looks like House.Dat, but hardcoded.
  HouseDatX: array [HOUSE_MIN..HOUSE_MAX] of THouseInfo = (
    ( //Armor smithy
    PlanYX:           ((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        311;
    TabletSpriteId:   261;
    Input:            (wt_Coal,       wt_Steel,      wt_None,       wt_None);
    Output:           (wt_MetalShield,wt_MetalArmor, wt_None,       wt_None);
    UnlockedByHouse:  ht_IronSmithy;
    SnowSpriteId:     -1;
    ),
    ( //Armor workshop
    PlanYX:           ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        321;
    TabletSpriteId:   271;
    Input:            (wt_Wood,       wt_Leather,    wt_None,       wt_None);
    Output:           (wt_Shield,     wt_Armor,      wt_None,       wt_None);
    UnlockedByHouse:  ht_Tannery;
    SnowSpriteId:     2067;
    ),
    ( //Bakery
    PlanYX:           ((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        308;
    TabletSpriteId:   258;
    Input:            (wt_Flour,      wt_None,       wt_None,       wt_None);
    Output:           (wt_Bread,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Mill;
    SnowSpriteId:     2054;
    ),
    ( //Barracks
    PlanYX:           ((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        322;
    TabletSpriteId:   272;
    Input:            (wt_Warfare,    wt_None,       wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     -1;
    ),
    ( //Butchers
    PlanYX:           ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        325;
    TabletSpriteId:   275;
    Input:            (wt_Pig,        wt_None,       wt_None,       wt_None);
    Output:           (wt_Sausages,   wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Swine;
    SnowSpriteId:     2066;
    ),
    ( //Coal mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        304;
    TabletSpriteId:   254;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Coal,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     -1;
    ),
    ( //Farm
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        309;
    TabletSpriteId:   259;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Corn,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     2055;
    ),
    ( //Fisher hut
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        307;
    TabletSpriteId:   257;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Fish,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     2053;
    ),
    ( //Gold mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        306;
    TabletSpriteId:   256;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_GoldOre,    wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     -1;
    ),
    ( //Inn
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        328;
    TabletSpriteId:   278;
    Input:            (wt_Bread,      wt_Sausages,   wt_Wine,       wt_Fish);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Store;
    SnowSpriteId:     2063;
    ),
    ( //Iron mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        305;
    TabletSpriteId:   255;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_IronOre,    wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     2052;
    ),
    ( //Iron smithy
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        302;
    TabletSpriteId:   252;
    Input:            (wt_IronOre,    wt_Coal,       wt_None,       wt_None);
    Output:           (wt_Steel,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_IronMine;
    SnowSpriteId:     2051;
    ),
    ( //Marketplace
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        327;
    TabletSpriteId:   277;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     -1;
    ),
    ( //Metallurgist
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        316;
    TabletSpriteId:   266;
    Input:            (wt_GoldOre,    wt_Coal,       wt_None,       wt_None);
    Output:           (wt_Gold,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_GoldMine;
    SnowSpriteId:     2068;
    ),
    ( //Mill
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        323;
    TabletSpriteId:   273;
    Input:            (wt_Corn,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Flour,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Farm;
    SnowSpriteId:     2062;
    ),
    ( //Quarry
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        315;
    TabletSpriteId:   265;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Stone,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_School;
    SnowSpriteId:     2058;
    ),
    ( //Sawmill
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        301;
    TabletSpriteId:   251;
    Input:            (wt_Trunk,      wt_None,       wt_None,       wt_None);
    Output:           (wt_Wood,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Woodcutters;
    SnowSpriteId:     2050;
    ),
    ( //School
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        314;
    TabletSpriteId:   264;
    Input:            (wt_Gold,       wt_None,       wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Store;
    SnowSpriteId:     2059;
    ),
    ( //Siege workshop
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        324;
    TabletSpriteId:   274;
    Input:            (wt_Wood,       wt_Steel,      wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_IronSmithy;
    SnowSpriteId:     -1;
    ),
    ( //Stables
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        313;
    TabletSpriteId:   263;
    Input:            (wt_Corn,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Horse,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Farm;
    SnowSpriteId:     -1;
    ),
    ( //Store
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        312;
    TabletSpriteId:   262;
    Input:            (wt_All,        wt_None,       wt_None,       wt_None);
    Output:           (wt_All,        wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_None; //
    SnowSpriteId:     2056;
    ),
    ( //Swine
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        317;
    TabletSpriteId:   267;
    Input:            (wt_Corn,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Pig,        wt_Skin,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Farm;
    SnowSpriteId:     2064;
    ),
    ( //Tannery
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        326;
    TabletSpriteId:   276;
    Input:            (wt_Skin,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Leather,    wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Swine;
    SnowSpriteId:     -1;
    ),
    ( //Town hall
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        319;
    TabletSpriteId:   269;
    Input:            (wt_Gold,       wt_None,       wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Metallurgists;
    SnowSpriteId:     -1;
    ),
    ( //Watch tower
    PlanYX:   ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        318;
    TabletSpriteId:   268;
    Input:            (wt_Stone,      wt_None,       wt_None,       wt_None);
    Output:           (wt_None,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Quary;
    SnowSpriteId:     2060;
    ),
    ( //Weapon smithy
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        303;
    TabletSpriteId:   253;
    Input:            (wt_Coal,       wt_Steel,      wt_None,       wt_None);
    Output:           (wt_Sword,      wt_Hallebard,  wt_Arbalet,    wt_None);
    UnlockedByHouse:  ht_IronSmithy;
    SnowSpriteId:     -1;
    ),
    ( //Weapon workshop
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        320;
    TabletSpriteId:   270;
    Input:            (wt_Wood,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Axe,        wt_Pike,       wt_Bow,        wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     2061;
    ),
    ( //Wineyard
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        329;
    TabletSpriteId:   279;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Wine,       wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_Sawmill;
    SnowSpriteId:     2065;
    ),
    ( //Woodcutter
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        310;
    TabletSpriteId:   260;
    Input:            (wt_None,       wt_None,       wt_None,       wt_None);
    Output:           (wt_Trunk,      wt_None,       wt_None,       wt_None);
    UnlockedByHouse:  ht_School;
    SnowSpriteId:     2057;
    )
    );


  //For some reason in KaM the piles of building supply are not aligned, each one has a different offset.
  //These values were taking from the barracks offsets and are for use with new houses.
  BuildSupplyOffsets: THouseBuildSupply = ( ((MoveX:  0; MoveY: 0), (MoveX: -7; MoveY: 0), (MoveX:-26; MoveY: 0),  //Wood 1-3
                                             (MoveX:-26; MoveY: 0), (MoveX:-26; MoveY:-1), (MoveX:-26; MoveY:-4)), //Wood 4-6
                                            ((MoveX:  0; MoveY: 0), (MoveX:  0; MoveY: 0), (MoveX: -7; MoveY: 0),  //Stone 1-3
                                             (MoveX: -7; MoveY:-4), (MoveX:-16; MoveY:-4), (MoveX:-16; MoveY:-4)));//Stone 4-6


  //'This house is unoccupied' msg index
  HouseTypeToUnoccupiedMsgIndex: array[THouseType] of ShortInt = (
    -1, -1,     //ut_None, ut_Any
    0,1,2,
    -1,         //ht_Barracks
    3,4,5,6,7,
    -1,         //ht_Inn
    8,9,
    -1,         //ht_Marketplace
    10,11,12,13,
    -1,         //ht_School
    14,15,
    -1,         //ht_Store
    16,17,
    -1,         //ht_TownHall
    18,19,20,21,22);


{ TKMHouseDatClass }
constructor TKMHouseSpec.Create(aHouseType: THouseType);
begin
  inherited Create;
  fHouseType := aHouseType;
  fNameTextID := TX_HOUSES_NAMES__29 + HouseTypeToIndex[fHouseType] - 1; //May be overridden for new houses
end;


function TKMHouseSpec.AcceptsWares: boolean;
begin
  Result := (ResInput[1] <> wt_None) or //Exclude houses that do not receive wares
            (fHouseType = ht_Marketplace); //Marketplace also accepts wares
end;


function TKMHouseSpec.GetArea: THouseArea;
begin
  Result := HouseDatX[fHouseType].PlanYX;
end;


function TKMHouseSpec.GetDoesOrders: Boolean;
begin
  Result := HouseDatX[fHouseType].NeedsPlayerOrder;
end;


function TKMHouseSpec.GetGUIIcon: Word;
begin
  Result := HouseDatX[fHouseType].BuildIcon;
end;


function TKMHouseSpec.GetHouseName: UnicodeString;
begin
  Result := gResTexts[fNameTextID];
end;


//MaxHealth is always a cost of construction * 50
function TKMHouseSpec.MaxHealth: Word;
begin
  Result := (fHouseDat.WoodCost + fHouseDat.StoneCost) * 50;
end;


//Write houses outline into given list
procedure TKMHouseSpec.Outline(aList: TKMPointList);
var
  I, K: Integer;
  Tmp: TKMByte2Array;
  Outlines: TKMShapesArray;
begin
  aList.Clear;
  SetLength(Tmp, 6, 6);

  for I := 0 to 3 do
  for K := 0 to 3 do
    Tmp[I+1,K+1] := Byte(HouseDatX[fHouseType].PlanYX[I+1,K+1] > 0);

  GenerateOutline(Tmp, 2, Outlines);
  Assert(Outlines.Count = 1, 'Houses are expected to have single outline');

  for I := 0 to Outlines.Shape[0].Count - 1 do
    aList.Add(KMPoint(Outlines.Shape[0].Nodes[I].X, Outlines.Shape[0].Nodes[I].Y));
end;


function TKMHouseSpec.GetOwnerType: TUnitType;
begin
  //fHouseDat.OwnerType is read from DAT file and is shortint, it can be out of range (i.e. -1)
  if InRange(fHouseDat.OwnerType, Low(UnitIndexToType), High(UnitIndexToType)) then
    Result := UnitIndexToType[fHouseDat.OwnerType]
  else
    Result := ut_None;
end;


function TKMHouseSpec.ProducesWares: Boolean;
begin
  Result := not (ResOutput[1] in [wt_None, wt_All, wt_Warfare]); //Exclude aggregate types
end;


function TKMHouseSpec.GetReleasedBy: THouseType;
begin
  Result := HouseDatX[fHouseType].UnlockedByHouse;
end;


function TKMHouseSpec.GetResInput: THouseRes;
begin
  Result := HouseDatX[fHouseType].Input;
end;


function TKMHouseSpec.GetResOutput: THouseRes;
begin
  Result := HouseDatX[fHouseType].Output;
end;


function TKMHouseSpec.GetSnowPic: SmallInt;
begin
  Result := HouseDatX[fHouseType].SnowSpriteId;
end;


function TKMHouseSpec.GetTabletIcon: Word;
begin
  Result := HouseDatX[fHouseType].TabletSpriteId;
end;


function TKMHouseSpec.GetUnoccupiedMsgId: SmallInt;
var HouseUnnocupiedMsgIndex: ShortInt;
begin
  Result := -1;
  HouseUnnocupiedMsgIndex := HouseTypeToUnoccupiedMsgIndex[fHouseType];
  if HouseUnnocupiedMsgIndex <> -1 then
    Result := TX_MSG_HOUSE_UNOCCUPIED__22 + HouseUnnocupiedMsgIndex;
end;


procedure TKMHouseSpec.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fHouseDat, SizeOf(TKMHouseDat));
end;


{ TKMResHouses }
constructor TKMResHouses.Create;

  procedure AddAnimation(aHouse: THouseType; aAnim: THouseActionType; aMoveX, aMoveY: Integer; const aSteps: array of SmallInt);
  var I: Integer;
  begin
    with fItems[aHouse].fHouseDat.Anim[aAnim] do
    begin
      MoveX := aMoveX;
      MoveY := aMoveY;
      Count := length(aSteps);
      for I := 1 to Count do
        Step[I] := aSteps[I-1];
    end;
  end;

  procedure AddMarketBeastAnim(aBeast: Integer; const aStep: array of SmallInt);
  var I: Integer;
  begin
    // Beast anims are 0 indexed
    for I := 1 to 30 do
      fMarketBeastAnim[aBeast].Step[I] := aStep[I - 1] + MarketWareTexStart - 1;
  end;

var H: THouseType; I: Integer;
begin
  inherited;

  for H := HOUSE_MIN to HOUSE_MAX do
    fItems[H] := TKMHouseSpec.Create(H);

  fCRC := LoadHouseDat(ExeDir+'data' + PathDelim + 'defines' + PathDelim + 'houses.dat');

  fItems[ht_Tannery].fHouseDat.Anim[ha_Flag3].Count := 0; //fix for tannery 2 flags at one place. Flag3 is unnecessary

  fItems[ht_Marketplace].fHouseType := ht_Marketplace;
  fItems[ht_Marketplace].fHouseDat.OwnerType := -1; //No unit works here (yet anyway)
  fItems[ht_Marketplace].fHouseDat.StonePic := 150;
  fItems[ht_Marketplace].fHouseDat.WoodPic := 151;
  fItems[ht_Marketplace].fHouseDat.WoodPal := 152;
  fItems[ht_Marketplace].fHouseDat.StonePal := 153;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,1] := 154;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,2] := 155;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,3] := 156;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,4] := 157;
  fItems[ht_Marketplace].fHouseDat.SupplyIn[1,5] := 158;
  fItems[ht_Marketplace].fHouseDat.WoodPicSteps := 23;
  fItems[ht_Marketplace].fHouseDat.StonePicSteps := 140;
  fItems[ht_Marketplace].fHouseDat.EntranceOffsetX := 1;
  fItems[ht_Marketplace].fHouseDat.EntranceOffsetXpx := 4; //Enterance is slightly to the left
  fItems[ht_Marketplace].fHouseDat.EntranceOffsetYpx := 10;
  fItems[ht_Marketplace].fHouseDat.WoodCost := 5;
  fItems[ht_Marketplace].fHouseDat.StoneCost := 6;
  for I := 1 to 6 do begin
    fItems[ht_Marketplace].fHouseDat.BuildSupply[1,I].MoveX := -55+ BuildSupplyOffsets[1,I].MoveX;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[1,I].MoveY := 15 + BuildSupplyOffsets[1,I].MoveY;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[2,I].MoveX := 28 + BuildSupplyOffsets[2,I].MoveX;
    fItems[ht_Marketplace].fHouseDat.BuildSupply[2,I].MoveY := 20 + BuildSupplyOffsets[2,I].MoveY;
  end;
  fItems[ht_Marketplace].fHouseDat.Sight := 10;
  fItems[ht_Marketplace].fHouseDat.SizeArea := 11;
  fItems[ht_Marketplace].fHouseDat.SizeX := 4;
  fItems[ht_Marketplace].fHouseDat.SizeY := 3;
  fItems[ht_Marketplace].fHouseDat.MaxHealth := 550;
  AddAnimation(ht_Marketplace, ha_Flag1, -80, -33, [1165,1166,1167,1163,1164]);
  AddAnimation(ht_Marketplace, ha_Flag2, -73, -7, [1163,1164,1165,1166,1167]);
  AddAnimation(ht_Marketplace, ha_Flag3, 73, -80, [1161,1162,1158,1159,1160]);
  AddAnimation(ht_Marketplace, ha_Fire1, 18, -83, [1623,1624,1625,1620,1621,1622]);
  AddAnimation(ht_Marketplace, ha_Fire2, 78, -67, [1637,1632,1633,1634,1635,1636]);
  AddAnimation(ht_Marketplace, ha_Fire3, -30, -103, [1620,1621,1622,1623,1624,1625]);
  AddAnimation(ht_Marketplace, ha_Fire4, -3, -54, [1617,1618,1619,1614,1615,1616]);
  AddAnimation(ht_Marketplace, ha_Fire5, -12, -38, [1632,1633,1634,1635,1636,1637]);
  AddAnimation(ht_Marketplace, ha_Fire6, 39, -47, [1629,1630,1631,1626,1627,1628]);
  AddAnimation(ht_Marketplace, ha_Fire7, 25, 13, [1635,1636,1637,1632,1633,1634]);
  AddAnimation(ht_Marketplace, ha_Fire8, -82, -40, [1621,1622,1623,1624,1625,1620]);
  //Now add horse animations for the market
  AddMarketBeastAnim(1,[278, 277, 276, 275, 274, 273, 272, 271, 271, 271, 271, 271, 272,
                        273, 274, 275, 276, 277, 277, 278, 279, 280, 281, 282, 282, 282,
                        282, 281, 280, 279]);
  fMarketBeastAnim[1].Count := 30;
  fMarketBeastAnim[1].MoveX := MarketWaresOffsetX;
  fMarketBeastAnim[1].MoveY := MarketWaresOffsetY;
  AddMarketBeastAnim(2,[266, 265, 264, 263, 262, 261, 260, 259, 258, 257, 258, 259, 260,
                        261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 270, 270, 270,
                        270, 269, 268, 267]);
  fMarketBeastAnim[2].Count := 30;
  fMarketBeastAnim[2].MoveX := MarketWaresOffsetX;
  fMarketBeastAnim[2].MoveY := MarketWaresOffsetY;

  //ExportCSV(ExeDir+'Houses.csv');
end;


destructor TKMResHouses.Destroy;
var H: THouseType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    FreeAndNil(fItems[H]);

  inherited;
end;


function TKMResHouses.GetHouseDat(aType: THouseType): TKMHouseSpec;
begin
  Result := fItems[aType];
end;


function TKMResHouses.IsValid(aType: THouseType): Boolean;
begin
  Result := aType in [HOUSE_MIN..HOUSE_MAX];
end;


function TKMResHouses.GetBeastAnim(aType: THouseType; aBeast, aAge: Integer): TKMAnimLoop;
begin
  Assert(aType in [ht_Swine, ht_Stables, ht_Marketplace]);
  Assert(InRange(aBeast, 1, 5));
  Assert(InRange(aAge, 1, 3));
  case aType of
    ht_Swine:       Result := fBeastAnim[1, aBeast, aAge];
    ht_Stables:     Result := fBeastAnim[2, aBeast, aAge];
    ht_Marketplace: Result := fMarketBeastAnim[aBeast];
  end;
end;


//Return CRC of loaded file
//CRC should be calculated right away, cos file may be swapped after loading
function TKMResHouses.LoadHouseDat(aPath: string): Cardinal;
var
  S: TKMemoryStream;
  i:integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(aPath);

    S.Read(fBeastAnim, SizeOf(fBeastAnim){30*70}); //Swine&Horses animations

    //Read the records one by one because we need to reorder them and skip one in the middle
    for i:=0 to 28 do //KaM has only 28 houses
    if HouseIndexToType[i] <> ht_None then
      fItems[HouseIndexToType[i]].LoadFromStream(S)
    else
      S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


procedure TKMResHouses.ExportCSV(aPath: string);
var
  HT: THouseType;
  S: string;
  SL: TStringList;
  I, K: Integer;
  procedure AddField(aField: string); overload;
  begin S := S + aField + ';'; end;
  procedure AddField(aField: Integer); overload;
  begin S := S + IntToStr(aField) + ';'; end;

begin
  SL := TStringList.Create;

  S := 'House name;WoodCost;StoneCost;ResProductionX';
  SL.Append(S);

  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    S := '';
    AddField(fItems[HT].HouseName);
    AddField(fItems[HT].WoodCost);
    AddField(fItems[HT].StoneCost);
    AddField(fItems[HT].ResProductionX);
    SL.Append(S);
    for I := 1 to 4 do
    begin
      S := '';
      for K := 1 to 4 do
        AddField(fItems[HT].BuildArea[I, K]);
      SL.Append(S);
    end;
    S := '';
    SL.Append(S);
  end;

  SL.SaveToFile(aPath);
  SL.Free;
end;


end.
