unit KM_Defaults;
interface
uses Classes, KromUtils, KM_LoadLib;

type
  TKMList = class(TList)
  public
    procedure Clear; override;
  end;


type TKMPointList = class
  public
    Count:integer;
    List:array of TKMPoint;
    constructor Create;
    procedure Clearup;
    procedure AddEntry(aLoc:TKMPoint);
  end;


{Cursors}
type
  cmCursorMode = (cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Houses);
const
  c_Default=1; c_Info=452;
  c_Dir0=511; c_Dir1=512; c_Dir2=513; c_Dir3=514; c_Dir4=515; c_Dir5=516; c_Dir6=517; c_Dir7=518; c_DirN=519;
  c_Scroll0=4; c_Scroll1=7; c_Scroll2=3; c_Scroll3=9; c_Scroll4=5; c_Scroll5=8; c_Scroll6=2; c_Scroll7=6;

  Cursors:array[1..19]of integer = (1,452,511,512,513,514,515,516,517,518,519,2,3,4,5,6,7,8,9);

{Controls}
type
  T3DButtonStateSet = set of (bs_Highlight, bs_Down, bs_Disabled);
  TMouseButton2 = (mb2None, mb2Left, mb2Right);

{Fonts}
type //Indexing should start from 1.
  TKMFont = (fnt_Adam=1, fnt_Antiqua, fnt_Briefing, fnt_Font01, fnt_Game,
             fnt_Grey, fnt_KMLobby0, fnt_KMLobby1, fnt_KMLobby2, fnt_KMLobby3,
             fnt_KMLobby4, fnt_MainA, fnt_MainB, fnt_MainMapGold, fnt_Metal,
             fnt_Mini, fnt_Minimum, fnt_Outline, fnt_System, fnt_Won);
const //Font01.fnt seems to be damaged..
  FontFiles: array[1..20]of string = (
  'adam','antiqua','briefing','font01-damaged','game','grey','kmlobby0','kmlobby1','kmlobby2','kmlobby3',
  'kmlobby4','maina','mainb','mainmapgold','metal','mini','mininum','outline','system','won');
//using 0 as default, with exceptions. Only used fonts have been checked, so this will need to be updated as we add new ones.
  FontCharSpacing: array[TKMFont] of integer = (0,0,0,0,0,-1,0,0,0,0,0,0,0,0,1,1,1,-1,0,0);


  ScrollCursorOffset = 17;
  CursorOffsetsX:array[1..19] of integer = (0,0,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset);
  CursorOffsetsY:array[1..19] of integer = (0,0,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0,0,ScrollCursorOffset,ScrollCursorOffset);

type
  TKMDirection = (dir_NA=0, dir_N=1, dir_NE=2, dir_E=3, dir_SE=4, dir_S=5, dir_SW=6, dir_W=7, dir_NW=8);

{Resources}
type
  TResourceType = (rt_None=0, rt_All=30, rt_Warfare=31,
    rt_Trunk     =1  , rt_Stone      =2 , rt_Wood       =3 , rt_IronOre     =4 ,
    rt_GoldOre   =5  , rt_Coal       =6 , rt_Steel      =7 , rt_Gold        =8 ,
    rt_Wine      =9  , rt_Corn       =10, rt_Bread      =11, rt_Flour       =12,
    rt_Leather   =13 , rt_Sousages   =14, rt_Pig        =15, rt_Skin        =16,
    rt_WoodShield=17 , rt_MetalShield=18, rt_Armor      =19, rt_MetalArmor  =20,
    rt_Axe       =21 , rt_Sword      =22, rt_Pike       =23, rt_Hallebard   =24,
    rt_Bow       =25 , rt_Arbalet    =26, rt_Horse      =27, rt_Fish        =28);
const
  ResourceProductionX:array[1..28]of byte = (
    1,3,2,1,1,1,2,3,
    1,1,2,1,2,3,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,2);

{Units}
type
  TUnitType = ( ut_None=0, ut_Any=40,
    ut_Serf=1,          ut_Woodcutter=2,    ut_Miner=3,         ut_AnimalBreeder=4,
    ut_Farmer=5,        ut_Lamberjack=6,    ut_Baker=7,         ut_Butcher=8,
    ut_Fisher=9,        ut_Worker=10,       ut_StoneCutter=11,  ut_Smith=12,
    ut_Metallurgist=13, ut_Recruit=14,      ut_HorseScout=22);
//15 Militia        //16 AxeFighter     //17 Swordsman      //18 Bowman
//19 Arbaletman     //20 Pikeman        //21 Hallebardman   //22 HorseScout
//23 Cavalry        //24 Barbarian      //25 Peasant        //26 Slingshot
//27 MetalBarbarian //28 Horseman       //29 Catapult       //30 Arbalest

//31 Wolf           //32 Fish           //33 Watersnake     //34 Seastar
//35 Crab           //36 Waterflower    //37 Waterleaf      //38 Duck

type
  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;


const {Actions names}
  UnitAct:array[1..14]of string = ('ua_Walk', 'ua_Work', 'ua_Spec', 'ua_Die', 'ua_Work1',
             'ua_Work2', 'ua_WorkEnd', 'ua_Eat', 'ua_WalkArm', 'ua_WalkTool',
             'ua_WalkBooty', 'ua_WalkTool2', 'ua_WalkBooty2', 'ua_Unknown');
  //specifies what actions unit can perform, should be ajoined with speeds and other tables
  UnitSupportedActions:array[1..14]of TUnitActionTypeSet = (
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkTool2],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die..ua_WalkBooty2],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Work..ua_WalkBooty],
    [ua_Walk, ua_Work, ua_Die, ua_Eat, ua_Work1, ua_Work2],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkBooty],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Spec, ua_Die, ua_Eat]
    );

  UnitSpeeds:array[1..40]of single =(
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,                //Civilian units
    1,1,1,1,1,1,1,1.5,1.5,1,1,1,1,1.5,0.5,0.5,  //Army units
    1,1,1,1,1,1,1,1,1,1);                       //Animals

type
  TGatheringScript = (
    gs_WoodCutterCut=1, gs_WoodCutterPlant=2,
    gs_FarmerSow=3, gs_FarmerCorn=4, gs_FarmerWine=5,
    gs_Fisher=6,
    gs_StoneCutter=7);

const
//Acquired resource, Count of resource, GoToWork, Work, GoFromWork, Work cycles, AfterWork Wait , Home idling
//{Work time whould be specified in frames according to animation}
  UnitMiningPlan:array[1..7,1..8]of byte = (
    (byte(rt_Trunk), 1, byte(ua_WalkBooty), byte(ua_Work) , byte(ua_WalkTool2) , 6, 15, 10), //Chop the tree
    (byte(rt_None) , 0, byte(ua_WalkTool) , byte(ua_Work1), byte(ua_Walk)      , 6, 0, 10), //Plant new tree

    (byte(rt_None) , 0, byte(ua_Walk)     , byte(ua_Work1), byte(ua_Walk)      , 6, 0, 10), //Seed the corn
    (byte(rt_Corn) , 1, byte(ua_WalkTool) , byte(ua_Work) , byte(ua_WalkBooty) , 6, 0, 10), //Gather crops
    (byte(rt_Wine) , 1, byte(ua_WalkTool2), byte(ua_Work2), byte(ua_WalkBooty2), 6, 0, 50), //Gather grapes

    (byte(rt_Fish) , 1, byte(ua_Walk)     , byte(ua_Walk) , byte(ua_Walk)      , 6, 0, 10), //Catch fish

    (byte(rt_Stone), 3, byte(ua_Walk)     , byte(ua_Work) , byte(ua_WalkTool)  , 6, 10, 50)  //Cut stone
    );


{Houses game}
type
  THouseType = ( ht_None=0,
    ht_Sawmill=1,        ht_IronSmithy=2, ht_WeaponSmithy=3, ht_CoalMine=4,       ht_IronMine=5,
    ht_GoldMine=6,       ht_FisherHut=7,  ht_Bakery=8,       ht_Farm=9,           ht_Woodcutters=10,
    ht_ArmorSmithy=11,   ht_Store=12,     ht_Stables=13,     ht_School=14,        ht_Quary=15,
    ht_Metallurgists=16, ht_Swine=17,     ht_WatchTower=18,  ht_TownHall=19,      ht_WeaponWorkshop=20,
    ht_ArmorWorkshop=21, ht_Barracks=22,  ht_Mill=23,        ht_SiegeWorkshop=24, ht_Butchers=25,
    ht_Tannery=26,       ht_NA=27,        ht_Inn=28,         ht_Wineyard=29);

  //House has 3 basic states: no owner inside, owner inside, owner working inside
  THouseState = ( hst_Empty, hst_Idle, hst_Work );
  //These are house building states
  THouseBuildState = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);

  THouseActionType = (
  ha_Work1=1, ha_Work2=2, ha_Work3=3, ha_Work4=4, ha_Work5=5, //Start, InProgress, .., .., Finish
  ha_Smoke=6, ha_FlagShtok=7, ha_Idle=8,
  ha_Flag1=9, ha_Flag2=10, ha_Flag3=11,
  ha_Fire1=12, ha_Fire2=13, ha_Fire3=14, ha_Fire4=15, ha_Fire5=16, ha_Fire6=17, ha_Fire7=18, ha_Fire8=19);
  THouseActionSet = set of THouseActionType;

const
  HouseAction:array[1..19] of string = (
  'ha_Work1', 'ha_Work2', 'ha_Work3', 'ha_Work4', 'ha_Work5', //Start, InProgress, .., .., Finish
  'ha_Smoke', 'ha_FlagShtok', 'ha_Idle ',
  'ha_Flag1', 'ha_Flag2', 'ha_Flag3',
  'ha_Fire1', 'ha_Fire2', 'ha_Fire3', 'ha_Fire4', 'ha_Fire5', 'ha_Fire6', 'ha_Fire7', 'ha_Fire8');

const
  //what kind kind of unit should dwell in that house
  HouseOwnerUnit:array[1..29]of TUnitType = (
    ut_Lamberjack, ut_Metallurgist, ut_Smith, ut_Miner, ut_Miner,
    ut_Miner, ut_Fisher, ut_Baker, ut_Farmer, ut_Woodcutter,
    ut_Smith, ut_None, ut_AnimalBreeder, ut_None, ut_StoneCutter,
    ut_Metallurgist, ut_AnimalBreeder, ut_Recruit, ut_None, ut_Lamberjack,
    ut_Lamberjack, ut_Recruit, ut_Baker, ut_Lamberjack, ut_Butcher,
    ut_Butcher, ut_None, ut_None, ut_Farmer);


  School_Order:array[1..14] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Smith, ut_Metallurgist, ut_Recruit);

{
  (byte(rt_Trunk), 1, byte(ha_Work1),  8, byte(rt_Wood) , 2)  //SawMill
  (byte(rt_Corn ), 1, byte(rt_None) ,  0, byte(rt_Flour), 1), //Mill
  (byte(rt_Flour), 1, byte(ha_Work2),  8, byte(rt_Bread), 2), //Bakery
);}

type
THouseProductionPlan = (pp_SawMill, pp_IronSmithy);

const
//Offset from house center to entrance
HouseXOffset:array[1..29]of shortint =
( 1, 0, 1, 1, 0, 0, 1,-1, 1, 0, 1, 1, 0, 1, 0, 1,-1, 0, 1, 1, 1, 1, 0, 1,-1, 0, 0, 1,-1);
//1-building area //2-entrance
HousePlanYX:array[1..29,1..4,1..4]of byte = (
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Sawmill        //1
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1)), //Iron smithy    //21
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon smithy  //244
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0)), //Coal mine      //134
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1)), //Iron mine      //61
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0)), //Gold mine      //239
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1)), //Fisher hut     //81
((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2)), //Bakery         //101
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Farm           //124
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0)), //Woodcutter     //142
((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1)), //Armor smithy   //41
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Store          //138
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1)), //Stables        //146
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //School         //250
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Quarry         //211
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Metallurgist   //235
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2)), //Swine          //368
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0)), //Watch tower    //255
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Town hall      //1657
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon workshop//273
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1)), //Armor workshop //663
((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Barracks       //334
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Mill           //358
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1)), //Siege workshop //1681
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2)), //Butcher        //397
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Tannery        //668
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //N/A
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1)), //Inn            //363
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2))  //Wineyard       //378
);

//What house produces
HouseOutput:array[1..29,1..4] of TResourceType = (
(rt_Wood,       rt_None,       rt_None,       rt_None), //Sawmill        //1
(rt_None,       rt_None,       rt_None,       rt_None), //Iron smithy    //21
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon smithy  //244
(rt_Coal,       rt_None,       rt_None,       rt_None), //Coal mine      //134
(rt_IronOre,    rt_None,       rt_None,       rt_None), //Iron mine      //61
(rt_GoldOre,    rt_None,       rt_None,       rt_None), //Gold mine      //239
(rt_Fish,       rt_None,       rt_None,       rt_None), //Fisher hut     //81
(rt_Bread,      rt_None,       rt_None,       rt_None), //Bakery         //101
(rt_Corn,       rt_None,       rt_None,       rt_None), //Farm           //124
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Woodcutter     //142
(rt_None,       rt_None,       rt_None,       rt_None), //Armor smithy   //41
(rt_All,        rt_None,       rt_None,       rt_None), //Store          //138
(rt_Horse,      rt_None,       rt_None,       rt_None), //Stables        //146
(rt_None,       rt_None,       rt_None,       rt_None), //School         //250
(rt_Stone,      rt_None,       rt_None,       rt_None), //Quarry         //211
(rt_None,       rt_None,       rt_None,       rt_None), //Metallurgist   //235
(rt_None,       rt_None,       rt_None,       rt_None), //Swine          //368
(rt_None,       rt_None,       rt_None,       rt_None), //Watch tower    //255
(rt_None,       rt_None,       rt_None,       rt_None), //Town hall      //1657
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon workshop//273
(rt_None,       rt_None,       rt_None,       rt_None), //Armor workshop //663
(rt_None,       rt_None,       rt_None,       rt_None), //Barracks       //334
(rt_Flour,      rt_None,       rt_None,       rt_None), //Mill           //358
(rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop //1681
(rt_Sousages,   rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_None,       rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_None,       rt_None,       rt_None,       rt_None), //Inn            //363
(rt_Wine,       rt_None,       rt_None,       rt_None)  //Wineyard       //378
);

//What house requires
HouseInput:array[1..29,1..4] of TResourceType = (
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Sawmill        //1
(rt_None,       rt_None,       rt_None,       rt_None), //Iron smithy    //21
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon smithy  //244
(rt_None,       rt_None,       rt_None,       rt_None), //Coal mine      //134
(rt_None,       rt_None,       rt_None,       rt_None), //Iron mine      //61
(rt_None,       rt_None,       rt_None,       rt_None), //Gold mine      //239
(rt_None,       rt_None,       rt_None,       rt_None), //Fisher hut     //81
(rt_Flour,      rt_None,       rt_None,       rt_None), //Bakery         //101
(rt_None,       rt_None,       rt_None,       rt_None), //Farm           //124
(rt_None,       rt_None,       rt_None,       rt_None), //Woodcutter     //142
(rt_None,       rt_None,       rt_None,       rt_None), //Armor smithy   //41
(rt_All,        rt_None,       rt_None,       rt_None), //Store          //138
(rt_Corn,       rt_None,       rt_None,       rt_None), //Stables        //146
(rt_Gold,       rt_None,       rt_None,       rt_None), //School         //250
(rt_None,       rt_None,       rt_None,       rt_None), //Quarry         //211
(rt_None,       rt_None,       rt_None,       rt_None), //Metallurgist   //235
(rt_Corn,       rt_None,       rt_None,       rt_None), //Swine          //368
(rt_Stone,      rt_None,       rt_None,       rt_None), //Watch tower    //255
(rt_Gold,       rt_None,       rt_None,       rt_None), //Town hall      //1657
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon workshop//273
(rt_None,       rt_None,       rt_None,       rt_None), //Armor workshop //663
(rt_Warfare,    rt_None,       rt_None,       rt_None), //Barracks       //334
(rt_Corn,       rt_None,       rt_None,       rt_None), //Mill           //358
(rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop //1681
(rt_None,       rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_None,       rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_Bread,      rt_Sousages,   rt_Wine,       rt_Fish), //Inn            //363
(rt_None,       rt_None,       rt_None,       rt_None)  //Wineyard       //378
);

{Houses UI}
const
  HouseCount=25;
  GUIBuildIcons:array[1..HouseCount]of word = (
  314, 328, 315, 310, 301,
  309, 323, 308, 317, 325,
  329, 306, 304, 316, 320,
  326, 321, 313, 305, 302,
  303, 311, 322, 312, 318);


{Terrain}
type
  TFieldType = (fdt_None=0, fdt_Road=1, fdt_Field=2, fdt_Wine=3,
                fdt_RoadWIP=4, fdt_FieldWIP=5, fdt_WineWIP=6, fdt_HousePlan=7, fdt_HouseWIP=8);

  TFieldSpecial = (fs_None,
                   fs_Corn1, fs_Corn2,
                   fs_Wine1, fs_Wine2, fs_Wine3, fs_Wine4,
                   fs_Dig1, fs_Dig2, fs_Dig3, fs_Dig4 );

  TMarkup = (mu_None, mu_RoadPlan, mu_FieldPlan, mu_WinePlan);

  TBorderType = (bt_None=0, bt_Field=1, bt_Wine=2, bt_HousePlan=3, bt_HouseBuilding=4);

const
  //Chopable tree, Chopdown animation,
  //Grow1, Grow2, Grow3, Grow4, Chop, Remainder
  ChopableTrees:array[1..13,1..6]of byte = (
  (  88,  89,  90,  90,  91,  37), //duplicate
  (  92,  93,  94,  95,  96,  49),
  (  97,  98,  99, 100, 101,  41),
  ( 102, 103, 104, 105, 106,  45),
  ( 107, 108, 109, 110, 111,  41),
  ( 112, 113, 114, 114, 115,  25), //duplicate
  ( 116, 117, 118, 119, 120,  25),
  ( 121, 122, 123, 124, 125,  64),
  ( 149, 150, 151, 151, 152,  29), //duplicate
  ( 153, 154, 155, 155, 156,  29), //duplicate
  ( 157, 158, 159, 160, 161,  33),
  ( 162, 163, 164, 165, 166,  33),
  ( 167, 168, 169, 170, 171,  33));

//   1      //Select road tile and rotation
//  8*2     //depending on surrounding tiles
//   4      //Bitfield
RoadsConnectivity:array [0..15,1..2]of byte = (
(249,0),(249,0),(249,1),(251,3),
(249,0),(249,0),(251,0),(253,0),
(249,1),(251,2),(249,1),(253,3),
(251,1),(253,2),(253,1),(255,0));

{DeliverList}
type
  TDemandType = (dt_Once, dt_Always); //Is this one-time demand like usual, or constant (storehouse, barracks)

{Utility}
const //Render scale
ZoomLevels:array[1..7]of single = (0.25,0.5,0.75,1,1.5,2,4);


type
  TPlayerID = (play_none, play_1=3, play_2, play_3, play_4, play_5, play_6);
var
  //Players colors
  TeamColors:array[1..8]of cardinal = (
  255 +  60 shr 8 +  45 shr 16,
  255 + 192 shr 8 +   0 shr 16,
   60 + 200 shr 8 +  40 shr 16,
  255 +  60 shr 8 +  45 shr 16,
  255 + 192 shr 8 +   0 shr 16,
   60 + 200 shr 8 +  40 shr 16,
  255 +  60 shr 8 +  45 shr 16,
  255 + 192 shr 8 +   0 shr 16);

function TypeToString(t:THouseType):string; overload
function TypeToString(t:TResourceType):string; overload
function TypeToString(t:TUnitType):string; overload

implementation

function TypeToString(t:TUnitType):string;
var s:string;
begin
if byte(t) in [1..14] then
  s:=fTextLibrary.GetTextString(siUnitNames+byte(t))
else
  s:='N/A';
Result:=s;
end;


function TypeToString(t:THouseType):string;
var s:string;
begin
if byte(t) in [1..29] then
  s:=fTextLibrary.GetTextString(siHouseNames+byte(t))
else
  s:='N/A';
Result:=s;
end;


function TypeToString(t:TResourceType):string;
var s:string;
begin
if byte(t) in [1..28] then
  s:=fTextLibrary.GetTextString(siResourceNames+byte(t))
else
  s:='N/A';
Result:=s;
end;

{ TKMList }

procedure TKMList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited;
end;


constructor TKMPointList.Create;
begin end;

procedure TKMPointList.Clearup;
begin
  Count:=0;
  setlength(List,0);
end;

procedure TKMPointList.AddEntry(aLoc:TKMPoint);
begin
  inc(Count);
  if Count>length(List)-1 then setlength(List,Count+10);
  List[Count]:=aLoc;
end;


end.
