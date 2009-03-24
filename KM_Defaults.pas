unit KM_Defaults;
interface
uses Windows, Classes, SysUtils, KromUtils, dglOpenGL, MMSystem;

//Global const
const                             
  CELL_SIZE_PX=40;      //Single cell size in pixels (width)
  CELL_HEIGHT_DIV=32;   //Height divider
  ToolBarWidth=224;     //Toolbar width in game
  Overlap=0.0;          //UV position overlap (to avoid edge artefacts in render), GL_CLAMP made it obsolete
  DEF_PAL=2;            //Default palette to use when generating full-color RGB textures
  GAME_LOGIC_PACE=100;  //Game logic should be updated each 100ms
  TERRAIN_PACE=1000;    //Terrain gets updated once each 1000ms
  TERRAIN_FOG_OF_WAR_MIN=8;  //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  TERRAIN_FOG_OF_WAR_ACT=16; //Until this value FOW is not rendered at all
  TERRAIN_FOG_OF_WAR_MAX=24; //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  FPSLag=1;             //lag between frames, 1000/FPSLag = max allowed FPS
  FPS_INTERVAL=1000;    //time between FPS measurements, more=accurate
  SCROLLSPEED = 1;      //This is the speed that the viewport will scroll every 100 ms, in cells
  SCROLLFLEX = 4;       //This is the number of pixels either side of the edge of the screen which will count as scrolling
  GAME_VERSION = 'Alpha'; //Game version string displayed in menu corner
  MENU_DESIGN_X = 1024; //Thats the size menu was designed for. All elements are placed in this size
  MENU_DESIGN_Y = 768;

var
  MakeGameSprites:boolean=true;        //Whenever to make Units/Houses graphics or not, saves time for GUI debug
  MakeTeamColors:boolean=false;         //Whenever to make team colors or not, saves RAM for debug
  ShowTerrainWires:boolean=false;
  MakeDrawPagesOverlay:boolean=false;   //Draw colored overlays ontop of panels, usefull for making layout
  MakeDrawRoutes:boolean=true;          //Draw unit routes when they are chosen
  MakeShowUnitMove:boolean=true;        //Draw unit movement overlay
  WriteResourceInfoToTXT:boolean=false;  //Whenever to write txt files with defines data properties
  WriteAllTexturesToBMP:boolean=false;  //Whenever to write all generated textures to BMP on loading
  TestViewportClipInset:boolean=false;  //Renders smaller area to see if everything gets clipped well
  TERRAIN_FOG_OF_WAR_ENABLE:boolean=false;//Whenever fog of war is enabled or not
  DO_UNIT_INTERACTION:boolean=false;    //Debug for unit interaction

const
  MaxHouses=255;        //Maximum houses one player can own
  MaxResInHouse=5;      //Maximum resource items allowed to be in house (it's 5, but I use 3 for testing)
  MAX_ORDER=999;        //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc)
  MAX_TEX_RESOLUTION=1024;       //Maximum texture resolution client can handle (used for packing sprites)

const   HOUSE_COUNT = 30;       //Number of KaM houses is 29. 30=Wall I wanna test ingame )
        MAX_PLAYERS = 6;        //Maximum players per map
        SAVEGAME_COUNT = 10;    //Savegame slots available

        //Here we store options that are hidden somewhere in code
        MAX_WARFARE_IN_BARRACKS = 20;
        GOLD_TO_SCHOOLS_IMPORTANT = true;       //Whenever gold delivery to schools is highly important
        FOOD_TO_INN_IMPORTANT = true;           //Whenever food delivery to inns is highly important
        UNIT_MAX_CONDITION = 15*600;            //*min of life. In KaM it's 45min
        UNIT_MIN_CONDITION = 5*600;             //If unit condition is less it will look for Inn


{Cursors}
type
  cmCursorMode = (cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Houses);
const
  c_Default=1; c_Info=452;
  c_Dir0=511; c_Dir1=512; c_Dir2=513; c_Dir3=514; c_Dir4=515; c_Dir5=516; c_Dir6=517; c_Dir7=518; c_DirN=519;
  c_Scroll0=4; c_Scroll1=7; c_Scroll2=3; c_Scroll3=9; c_Scroll4=5; c_Scroll5=8; c_Scroll6=2; c_Scroll7=6;

  Cursors:array[1..19]of integer = (1,452,511,512,513,514,515,516,517,518,519,2,3,4,5,6,7,8,9);

  ScrollCursorOffset = 17;
  CursorOffsetsX:array[1..19] of integer = (0,0,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset);
  CursorOffsetsY:array[1..19] of integer = (0,0,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0,0,ScrollCursorOffset,ScrollCursorOffset);

{Controls}
type
  T3DButtonStateSet = set of (bs_Highlight, bs_Down, bs_Disabled);
  TFlatButtonStateSet = set of (fbs_Highlight, fbs_Selected, fbs_Disabled);

{Palettes}
const
 pal_map=1; pal_0=2; pal_1=3; pal_2=4; pal_3=5; pal_4=6; pal_5=7; pal_set=8; pal_set2=9; pal_lin=10;
 pal2_mapgold=11; pal2_setup=12; pal2_1=13;

 //I couldn't find matching palettes for several entries, so I marked them 0 
 RX5Pal:array[1..40]of byte = (
 12,12,12,12,12,12,9,0,0,1,
 1,1,1,1,1,1,12,12,12,13,
 0,0,0,0,12,1,1,1,1,1,
 12,12,12,12,12,12,12,12,12,12);

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
  FontCharSpacing: array[TKMFont] of shortint = (0,0,0,0,1,-1,0,0,0,0,0,0,0,0,1,1,1,-1,0,0);

  FontPal:array[1..20]of byte =
  //Those 10 are unknown Pal, no existing Pal matches them well
  (10,2,1,10,2,2,1,8,8,9,
   9,8,10,8,2,8,8,2,10,9);

type
  TKMDirection = (dir_NA=0, dir_N=1, dir_NE=2, dir_E=3, dir_SE=4, dir_S=5, dir_SW=6, dir_W=7, dir_NW=8);

  
{Resources}
type
  TResourceType = (rt_None=0, rt_All=30, rt_Warfare=31,
    rt_Trunk     =1  , rt_Stone      =2 , rt_Wood       =3 , rt_IronOre     =4 ,
    rt_GoldOre   =5  , rt_Coal       =6 , rt_Steel      =7 , rt_Gold        =8 ,
    rt_Wine      =9  , rt_Corn       =10, rt_Bread      =11, rt_Flour       =12,
    rt_Leather   =13 , rt_Sausages   =14, rt_Pig        =15, rt_Skin        =16,
    rt_Shield    =17 , rt_MetalShield=18, rt_Armor      =19, rt_MetalArmor  =20,
    rt_Axe       =21 , rt_Sword      =22, rt_Pike       =23, rt_Hallebard   =24,
    rt_Bow       =25 , rt_Arbalet    =26, rt_Horse      =27, rt_Fish        =28);

const
ProductionCosts:array[17..26,1..2]of TResourceType = (
(rt_None,rt_Wood),    //rt_Shield
(rt_Coal,rt_Steel),   //rt_MetalShield
(rt_None,rt_Leather), //rt_Armor
(rt_Coal,rt_Steel),   //rt_MetalArmor
(rt_Wood,rt_Wood),    //rt_Axe
(rt_Coal,rt_Steel),   //rt_Sword
(rt_Wood,rt_Wood),    //rt_Pike
(rt_Coal,rt_Steel),   //rt_Hallebard
(rt_Wood,rt_Wood),    //rt_Bow
(rt_Coal,rt_Steel)    //rt_Arbalet
);

{Units}
type
  TUnitType = ( ut_None=0, ut_Any=40,
    ut_Serf=1,          ut_Woodcutter=2,    ut_Miner=3,         ut_AnimalBreeder=4,
    ut_Farmer=5,        ut_Lamberjack=6,    ut_Baker=7,         ut_Butcher=8,
    ut_Fisher=9,        ut_Worker=10,       ut_StoneCutter=11,  ut_Smith=12,
    ut_Metallurgist=13, ut_Recruit=14,
    ut_Militia=15,      ut_AxeFighter=16,   ut_Swordsman=17,    ut_Bowman=18,
    ut_Arbaletman=19,   ut_Pikeman=20,      ut_Hallebardman=21, ut_HorseScout=22,
    ut_Cavalry=23,      ut_Barbarian=24,
    //ut_Peasant=25,    ut_Slingshot=26,    ut_MetalBarbarian=27,ut_Horseman=28,
    //ut_Catapult=29,   ut_Ballista=30,
    ut_Wolf=31,         ut_Fish=32,         ut_Watersnake=33,   ut_Seastar=34,
    ut_Crab=35,         ut_Waterflower=36,  ut_Waterleaf=37,    ut_Duck=38);

const
  Army_Flag=4962;
  Thought_Eat=6250;
  Thought_Home=6258;
  Thought_Build=6266;
  Thought_Stone=6275;
  Thought_Wood=6282;
  Thought_Death=6290;
  Thought_Quest=6298;

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
    [ua_Walk, ua_Spec, ua_Die, ua_Eat] //Recruit
    );

type
  TGatheringScript = (
    gs_None=0,
    gs_WoodCutterCut, gs_WoodCutterPlant,
    gs_FarmerSow, gs_FarmerCorn, gs_FarmerWine,
    gs_Fisher,
    gs_StoneCutter,
    gs_CoalMiner, gs_GoldMiner, gs_IronMiner);

{Houses game}
type
  THouseType = ( ht_None=0,
    ht_Sawmill=1,        ht_IronSmithy=2, ht_WeaponSmithy=3, ht_CoalMine=4,       ht_IronMine=5,
    ht_GoldMine=6,       ht_FisherHut=7,  ht_Bakery=8,       ht_Farm=9,           ht_Woodcutters=10,
    ht_ArmorSmithy=11,   ht_Store=12,     ht_Stables=13,     ht_School=14,        ht_Quary=15,
    ht_Metallurgists=16, ht_Swine=17,     ht_WatchTower=18,  ht_TownHall=19,      ht_WeaponWorkshop=20,
    ht_ArmorWorkshop=21, ht_Barracks=22,  ht_Mill=23,        ht_SiegeWorkshop=24, ht_Butchers=25,
    ht_Tannery=26,       ht_NA=27,        ht_Inn=28,         ht_Wineyard=29,
    ht_Wall=30);

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
  'ha_Smoke', 'ha_FlagShtok', 'ha_Idle',
  'ha_Flag1', 'ha_Flag2', 'ha_Flag3',
  'ha_Fire1', 'ha_Fire2', 'ha_Fire3', 'ha_Fire4', 'ha_Fire5', 'ha_Fire6', 'ha_Fire7', 'ha_Fire8');

  StatHouseOrder:array[1..11,1..4] of THouseType = (
  (ht_Quary, ht_Woodcutters, ht_FisherHut, ht_Farm),
  (ht_Wineyard, ht_Mill, ht_Bakery, ht_Swine),
  (ht_Stables, ht_Butchers, ht_Tannery, ht_Metallurgists),
  (ht_IronSmithy, ht_WeaponSmithy, ht_ArmorSmithy, ht_CoalMine),
  (ht_IronMine, ht_GoldMine, ht_Sawmill, ht_WeaponWorkshop),
  (ht_ArmorWorkshop, ht_SiegeWorkshop, ht_Barracks, ht_TownHall),
  (ht_WatchTower, ht_Store, ht_School, ht_Inn),

  (ht_None, ht_None, ht_None, ht_None),
  (ht_None, ht_None, ht_None, ht_None),
  (ht_None, ht_None, ht_None, ht_None),
  (ht_None, ht_None, ht_None, ht_None)
  );   

  StatUnitOrder:array[1..11,1..5] of TUnitType =
  (
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_None, ut_None, ut_None, ut_None, ut_None),

  (ut_None, ut_None, ut_None, ut_None, ut_None),
  (ut_StoneCutter, ut_Woodcutter, ut_Fisher, ut_Farmer, ut_Baker),
  (ut_AnimalBreeder, ut_Butcher, ut_Metallurgist, ut_Smith, ut_Miner),
  (ut_Lamberjack, ut_Recruit, ut_None, ut_Serf, ut_Worker)
  );

  //Building of the house allows player to build following houses
  BuildingAllowed:array[1..HOUSE_COUNT,1..8]of THouseType = (
  (ht_Farm, ht_Wineyard, ht_CoalMine, ht_IronMine, ht_GoldMine, ht_WeaponWorkshop, ht_Barracks, ht_FisherHut), //Sawmill
  (ht_WeaponSmithy,ht_ArmorSmithy,ht_SiegeWorkshop,ht_None,ht_None,ht_None,ht_None,ht_None), //IronSmithy
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //CoalMine
  (ht_IronSmithy,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //IronMine
  (ht_Metallurgists,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //GoldMine
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Mill,ht_Swine,ht_Stables,ht_None,ht_None,ht_None,ht_None,ht_None), //Farm
  (ht_SawMill,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Woodcutters
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_School,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Store
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Inn,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),  //School
  (ht_Woodcutters,ht_WatchTower,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Quary
  (ht_TownHall,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Metallurgists
  (ht_Butchers,ht_Tannery,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Swine
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Bakery,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Mill
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_ArmorWorkShop,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),  //Tannery
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Quary,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Inn
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None)
  );

const
  School_Order:array[1..14] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);
    

const
//1-building area //2-entrance
HousePlanYX:array[1..HOUSE_COUNT,1..4,1..4]of byte = (
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
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2)), //Wineyard       //378
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,1,0))  //Wall
);

//Does house output needs to be ordered or it keeps on producing it itself
HousePlaceOrders:array[1..HOUSE_COUNT] of boolean = (
false,false,true,false,false,false,false,false,false,false,
true,false,false,false,false,false,false,false,false,true,
true,false,false,true,false,false,false,false,false,false);

//What house produces
HouseOutput:array[1..HOUSE_COUNT,1..4] of TResourceType = (
(rt_Wood,       rt_None,       rt_None,       rt_None), //Sawmill        //1
(rt_Steel,      rt_None,       rt_None,       rt_None), //Iron smithy    //21
(rt_Sword,      rt_Hallebard,  rt_Arbalet,    rt_None), //Weapon smithy  //244
(rt_Coal,       rt_None,       rt_None,       rt_None), //Coal mine      //134
(rt_IronOre,    rt_None,       rt_None,       rt_None), //Iron mine      //61
(rt_GoldOre,    rt_None,       rt_None,       rt_None), //Gold mine      //239
(rt_Fish,       rt_None,       rt_None,       rt_None), //Fisher hut     //81
(rt_Bread,      rt_None,       rt_None,       rt_None), //Bakery         //101
(rt_Corn,       rt_None,       rt_None,       rt_None), //Farm           //124
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Woodcutter     //142
(rt_MetalArmor, rt_MetalShield,rt_None,       rt_None), //Armor smithy   //41
(rt_All,        rt_None,       rt_None,       rt_None), //Store          //138
(rt_Horse,      rt_None,       rt_None,       rt_None), //Stables        //146
(rt_None,       rt_None,       rt_None,       rt_None), //School         //250
(rt_Stone,      rt_None,       rt_None,       rt_None), //Quarry         //211
(rt_Gold,       rt_None,       rt_None,       rt_None), //Metallurgist   //235
(rt_Pig,        rt_Skin,       rt_None,       rt_None), //Swine          //368
(rt_None,       rt_None,       rt_None,       rt_None), //Watch tower    //255
(rt_None,       rt_None,       rt_None,       rt_None), //Town hall      //1657
(rt_Axe,        rt_Pike,       rt_Bow,        rt_None), //Weapon workshop//273
(rt_Shield,     rt_Armor,      rt_None,       rt_None), //Armor workshop //663
(rt_None,       rt_None,       rt_None,       rt_None), //Barracks       //334
(rt_Flour,      rt_None,       rt_None,       rt_None), //Mill           //358
(rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop //1681
(rt_Sausages,   rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_Leather,    rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_None,       rt_None,       rt_None,       rt_None), //Inn            //363
(rt_Wine,       rt_None,       rt_None,       rt_None), //Wineyard       //378
(rt_None,       rt_None,       rt_None,       rt_None)  //Wall
);

//What house requires
HouseInput:array[1..HOUSE_COUNT,1..4] of TResourceType = (
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Sawmill        //1
(rt_IronOre,    rt_Coal,       rt_None,       rt_None), //Iron smithy    //21
(rt_Coal,       rt_Steel,      rt_None,       rt_None), //Weapon smithy  //244
(rt_None,       rt_None,       rt_None,       rt_None), //Coal mine      //134
(rt_None,       rt_None,       rt_None,       rt_None), //Iron mine      //61
(rt_None,       rt_None,       rt_None,       rt_None), //Gold mine      //239
(rt_None,       rt_None,       rt_None,       rt_None), //Fisher hut     //81
(rt_Flour,      rt_None,       rt_None,       rt_None), //Bakery         //101
(rt_None,       rt_None,       rt_None,       rt_None), //Farm           //124
(rt_None,       rt_None,       rt_None,       rt_None), //Woodcutter     //142
(rt_Steel,      rt_Coal,       rt_None,       rt_None), //Armor smithy   //41
(rt_All,        rt_None,       rt_None,       rt_None), //Store          //138
(rt_Corn,       rt_None,       rt_None,       rt_None), //Stables        //146
(rt_Gold,       rt_None,       rt_None,       rt_None), //School         //250
(rt_None,       rt_None,       rt_None,       rt_None), //Quarry         //211
(rt_GoldOre,    rt_Coal,       rt_None,       rt_None), //Metallurgist   //235
(rt_Corn,       rt_None,       rt_None,       rt_None), //Swine          //368
(rt_Stone,      rt_None,       rt_None,       rt_None), //Watch tower    //255
(rt_Gold,       rt_None,       rt_None,       rt_None), //Town hall      //1657
(rt_Wood,       rt_None,       rt_None,       rt_None), //Weapon workshop//273
(rt_Wood,       rt_Leather,    rt_None,       rt_None), //Armor workshop //663
(rt_Warfare,    rt_None,       rt_None,       rt_None), //Barracks       //334
(rt_Corn,       rt_None,       rt_None,       rt_None), //Mill           //358
(rt_Wood,       rt_Steel,      rt_None,       rt_None), //Siege workshop //1681
(rt_Pig,        rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_Skin,       rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_Bread,      rt_Sausages,   rt_Wine,       rt_Fish), //Inn            //363
(rt_None,       rt_None,       rt_None,       rt_None), //Wineyard       //378
(rt_None,       rt_None,       rt_None,       rt_None)  //Wall
);

{Houses UI}
const
  GUIBuildIcons:array[1..HOUSE_COUNT]of word = (
  314, 328, 315, 310, 301,
  309, 323, 308, 317, 325,
  329, 306, 304, 316, 320,
  326, 321, 313, 305, 302,
  303, 311, 322, 312, 318,
  307, 319, 324, 312, 330);


{Terrain}
type
  TFieldType = (fdt_None=0, fdt_Road=1, fdt_Field=2, fdt_Wine=3,
                fdt_RoadWIP=4, fdt_FieldWIP=5, fdt_WineWIP=6, fdt_HousePlan=7, fdt_HouseWIP=8, fdt_House=9, fdt_HouseRoad=10);

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
  TPlayerID = (play_none=0, play_1=1, play_2=2, play_3=3, play_4=4, play_5=5, play_6=6);

  {@Lewin:If you know other names- please fill in }
  TSoundFX = (
    sfx_corncut=1,
    sfx_dig,
    sfx_pave,
    sfx_minestone,
    sfx_cornsow,
    sfx_choptree,
    sfx_housebuild,
    sfx_placemarker,
    sfx_click,
    sfx_mill,
    sfx_saw,
    sfx_wineStep,
    sfx_wineDrain,
    sfx_metallurgists,
    sfx_coalDown,
    sfx_Pig1,sfx_Pig2,sfx_Pig3,sfx_Pig4,
    sfx_Mine,
    sfx_unknown21,
    sfx_Leather,
    sfx_BakerSlap,
    sfx_CoalMineThud,
    sfx_ButcherCut,
    sfx_SausageString,
    sfx_QuarryClink,
    sfx_TreeDown,
    sfx_WoodcutterDig,
    sfx_CantPlace,
    sfx_MessageOpen,
    sfx_MessageClose,
    sfx_MessageNotice,
    { 34-57 are melee attack sounds }
    sfx_BowDraw=58,
    sfx_ArrowHit,
    sfx_CrossbowShoot,
    sfx_CrossbowDraw,
    sfx_BowShoot,
    sfx_BlacksmithBang,
    sfx_BlacksmithFire,
    sfx_CarpenterHammer,
    sfx_Horse1,sfx_Horse2,sfx_Horse3,sfx_Horse4,
    sfx_unknown70,
    sfx_HouseDestroy,
    sfx_SchoolDing,
    sfx_SlingerShoot,
    sfx_BalistaShoot,
    sfx_CatapultShoot,
    sfx_unknown76,
    sfx_CatapultReload,
    sfx_SiegeBuildingSmash
        );

var
  //Players colors
  TeamColors:array[1..8]of cardinal = (
  $FF3040FF, //Red
  $FF00C0FF, //Orange
  $FF00FFFF, //Yellow
  $FF28C840, //Green
  $FFC0C040, //Cyan
  $FFC00000, //Blue
  $FF00FFFF, //Yellow
  $FF28C840  //Green
  );

  GlobalTickCount:integer=0;

  MinimapList:GLint;

  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  ExeDir:string;

  CursorMode:record //It's easier to store it in record
    Mode:cmCursorMode;
    Param:byte;
  end;

  Scrolling: boolean;

  CursorX,CursorY:single;    //Precise cursor position on map
  CursorXn,CursorYn:integer; //Cursor position node
  CursorXc,CursorYc:integer; //Cursor position cell

  //Pallete for RX bitmaps
  //There are 9 palette files Map, Pal0-5, Setup and Setup2
  //+1 linear
  //+2lbm
  Pal:array[1..13,1..256,1..3]of byte;

  RXData:array [1..5]of record
    Title:string;
    Qty:integer;
    Pal:array[1..9500] of byte;
    Size:array[1..9500,1..2] of word;
    Pivot:array[1..9500] of record x,y:integer; end;
    Data:array[1..9500] of array of byte;
    NeedTeamColors:boolean;
  end;

  GFXData: array [1..5,1..9500] of record
    TexID,AltID: GLUint; //AltID used for team colors
    u1,v1,u2,v2: single;
    PxWidth,PxHeight:word;
  end;

  FontData:array[1..32]of record
    Title:TKMFont;
    TexID:GLUint;
    Pal:array[0..255]of byte;
    Letters:array[0..255]of record
      Width,Height:word;
      Add:array[1..4]of word;
      Data:array[1..4096] of byte;
      u1,v1,u2,v2:single;
    end;
  end;

HouseDAT1:array[1..30,1..35]of smallint; //Pigs and Horses
HouseDAT:array[1..HOUSE_COUNT] of packed record
  StonePic,WoodPic,WoodPal,StonePal:smallint;
  SupplyIn:array[1..4,1..5]of smallint;
  SupplyOut:array[1..4,1..5]of smallint;
  Anim:array[1..19] of record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;
  WoodPicSteps,StonePicSteps:word;
  a1:smallint;
  EntranceOffsetX,EntranceOffsetY:shortint;
  EntranceOffsetXpx,EntranceOffsetYpx:shortint;
  BuildArea:array[1..10,1..10]of shortint;
  WoodCost,StoneCost:byte;
  BuildSupply:array[1..12] of record MoveX,MoveY:integer; end;
  a5,SizeArea:smallint;
  SizeX,SizeY,sx2,sy2:shortint;
  WorkerWork,WorkerRest:smallint;
  ResInput,ResOutput:array[1..4]of shortint; //KaM_Remake will use it's own tables for this matter
  ResProductionX:shortint;
  MaxHealth,Sight:smallint;
  OwnerType:shortint;
  Foot:array[1..36]of shortint;
end;

SerfCarry:array[1..28] of packed record
  Dir:array[1..8]of packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;
end;

UnitStat:array[1..41]of record
  x1,Attack,AttackHorseBonus,x4,HitPoints,Speed,x7,Sight:smallint;
  x9,x10:shortint;
  CanWalkOut,x11:smallint;
end;
UnitSprite2:array[1..41,1..18]of smallint;
UnitSprite:array[1..41]of packed record
  Act:array[1..14]of packed record
    Dir:array[1..8]of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;
  end;
end;

  MapElemQty:integer=254; //Default qty
  MapElem:array[1..512]of packed record
    Step:array[1..30]of smallint; //60
    Count:word;                   //62
    u1:array[1..16]of word;       //94
    u2:shortint;                  //95
    u3,u4:word;                   //99
  end;

  TileMMColor:array[1..256]of record R,G,B:single; end;

  
type
  TKMList = class(TList)
  public
    procedure Clear; override;
  end;


type TKMPointList = class
  public
    Count:integer;
    List:array of TKMPoint; //1..Count
    procedure Clearup;
    procedure AddEntry(aLoc:TKMPoint);
    function GetRandom():TKMPoint;
  end;


{This is custom logging system}
type
  TKMLog = class
  private
    fl:textfile;
    logfile:string;
    PreviousTick:cardinal;
    procedure AddLine(text:string);
    procedure AddLineNoTime(text:string);
  public
    constructor Create(path:string);
    procedure AppendLog(text:string); overload;
    procedure AppendLog(text:string; num:integer); overload;
    procedure AppendLog(num:integer; text:string); overload;
    procedure AppendLog(text:string; Res:boolean); overload;
    procedure AppendLog(a,b:integer); overload;
    procedure AddToLog(text:string);
  end;

var
  fLog: TKMLog;

function TypeToString(t:THouseType):string; overload
function TypeToString(t:TResourceType):string; overload
function TypeToString(t:TUnitType):string; overload
function TypeToString(t:TKMPoint):string; overload

implementation
uses KM_LoadLib;

{Reset log file}
constructor TKMLog.Create(path:string);
begin
  logfile:=path;
  assignfile(fl,logfile);
  rewrite(fl);
  closefile(fl);
  AddToLog('');
  AddToLog('');
  AddToLog('Log is up and running');
end;

{Lines are timestamped, each line invokes file open/close for writing,
meaning no lines will be lost if Remake crashes}
procedure TKMLog.AddLine(text:string);
var Delta:cardinal;
begin
  Delta:=TimeGetTime - PreviousTick;
  PreviousTick:=TimeGetTime;
  if Delta>100000 then Delta:=0; //ommit first usage
  assignfile(fl,logfile);
  append(fl);
  writeln(fl,#9+inttostr(Delta)+'ms'+#9+text);
  closefile(fl);
end;

{Same line but without timestamp}
procedure TKMLog.AddLineNoTime(text:string);
begin
  assignfile(fl,logfile);
  append(fl);
  writeln(fl,#9+#9+text);
  closefile(fl);
end;

procedure TKMLog.AppendLog(text:string);
begin
  AddLine(text);
end;

procedure TKMLog.AppendLog(text:string; num:integer);
begin
  AddLine(text+' '+inttostr(num));
end;

procedure TKMLog.AppendLog(num:integer; text:string);
begin
  AddLine(inttostr(num)+' '+text);
end;

procedure TKMLog.AppendLog(text:string; Res:boolean);
var s:string;
begin
  if Res then s:='done' else s:='fail';
  AddLine(text+' ... '+s);
end;

procedure TKMLog.AppendLog(a,b:integer);
begin
  AddLine(inttostr(a)+' : '+inttostr(b));
end;

procedure TKMLog.AddToLog(text:string);
begin
  AddLineNoTime(text);
end;


{TypeToString routines}
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
if byte(t) in [1..HOUSE_COUNT] then
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


function TypeToString(t:TKMPoint):string;
begin
Result:='('+inttostr(t.x)+';'+inttostr(t.y)+')';
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


{ TKMPointList }
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

function TKMPointList.GetRandom():TKMPoint;
begin
  Result:=List[random(Count)+1];
end;



end.
