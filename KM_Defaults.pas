unit KM_Defaults;
interface
uses Windows, Classes, SysUtils, KromUtils, dglOpenGL, MMSystem;

//Global const
const                             
  CELL_SIZE_PX=40;              //Single cell size in pixels (width)
  CELL_HEIGHT_DIV=33.333;       //Height divider
  ToolBarWidth=224;             //Toolbar width in game
  Overlap=0.0;                  //UV position overlap (to avoid edge artefacts in render), GL_CLAMP made it obsolete
  DEF_PAL=2;                    //Default palette to use when generating full-color RGB textures
  GAME_LOGIC_PACE=100;          //Game logic should be updated each 100ms
  TERRAIN_PACE=1000;            //Terrain gets updated once each 1000ms
  TERRAIN_FOG_OF_WAR_MIN=8;     //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  TERRAIN_FOG_OF_WAR_ACT=16;    //Until this value FOW is not rendered at all
  TERRAIN_FOG_OF_WAR_MAX=24;    //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  TEST_MAX_WALK_PATH=8192;      //A* max test length (with max value of MapX*MapY-1 in worst case)
  FPSLag=1;                     //lag between frames, 1000/FPSLag = max allowed FPS
  FPS_INTERVAL=1000;            //time between FPS measurements, more=accurate
  SCROLLSPEED = 1;              //This is the speed that the viewport will scroll every 100 ms, in cells
  SCROLLFLEX = 4;               //This is the number of pixels either side of the edge of the screen which will count as scrolling
  GAME_VERSION = 'Alpha';       //Game version string displayed in menu corner
  MENU_DESIGN_X = 1024;         //Thats the size menu was designed for. All elements are placed in this size
  MENU_DESIGN_Y = 768;
  MENU_SINGLE_MAPS_COUNT = 14;  //Number of single player maps to display in menu

var
  //These should be TRUE
  MakeTerrainAnim       :boolean=true;  //Should we animate water and swamps
  MakeUnitSprites       :boolean=true;  //Whenever to make Units graphics or not, saves time for GUI debug
  MakeHouseSprites      :boolean=true;  //Whenever to make Houses graphics or not, saves time for GUI debug
  MakeTeamColors        :boolean=true;  //Whenever to make team colors or not, saves RAM for debug
  DO_UNIT_INTERACTION   :boolean=false;  //Debug for unit interaction
  DO_UNIT_HUNGER        :boolean=true;  //Wherever units get hungry or not
  DO_SERFS_WALK_ROADS   :boolean=true;  //Wherever serfs should walk only on roads
  FORCE_RESOLUTION      :boolean=false;  //Whether to change resolution on start up

  //These should be ... enabled sometime
  FOG_OF_WAR_ENABLE     :boolean=false; //Whenever dynamic fog of war is enabled or not

  //These should be FALSE
  ShowTerrainWires      :boolean=false; //Makes terrain height visible
  ShowSpriteOverlay     :boolean=false; //Render outline around every sprite
  MakeDrawPagesOverlay  :boolean=false; //Draw colored overlays ontop of panels, usefull for making layout
  MakeShowUnitRoutes    :boolean=false; //Draw unit routes when they are chosen
  MakeShowUnitMove      :boolean=false; //Draw unit movement overlay
  WriteResourceInfoToTXT:boolean=false; //Whenever to write txt files with defines data properties on loading
  WriteAllTexturesToBMP :boolean=false; //Whenever to write all generated textures to BMP on loading (very time consuming)
  TestViewportClipInset :boolean=false; //Renders smaller area to see if everything gets clipped well
  MOUSEWHEEL_ZOOM_ENABLE:boolean=true; //Should we allow to zoom in game or not
  RENDER_3D             :boolean=false; //Experimental 3D render
  SHOW_WALK_CONNECT     :boolean=false; //Show floodfill areas of interconnected areas
  SHOW_ALL_ON_MINIMAP   :boolean=false; //Whenever to display other players on minimap

  //Statistics
  CtrlPaintCount:integer;               //How many Controls were painted

const
  MaxHouses=255;        //Maximum houses one player can own
  MaxResInHouse=5;      //Maximum resource items allowed to be in house (it's 5, but I use 3 for testing)
  MAX_ORDER=999;        //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc)
  MAX_TEX_RESOLUTION=1024;       //Maximum texture resolution client can handle (used for packing sprites)

const   HOUSE_COUNT = 30;       //Number of KaM houses is 29. 30=Wall I wanna test ingame )
        MAX_PLAYERS = 8;        //Maximum players per map
        SAVEGAME_COUNT = 10;    //Savegame slots available in game menu

        //Here we store options that are hidden somewhere in code
        MAX_WARFARE_IN_BARRACKS = 20;
        GOLD_TO_SCHOOLS_IMPORTANT = true;       //Whenever gold delivery to schools is highly important
        FOOD_TO_INN_IMPORTANT = true;           //Whenever food delivery to inns is highly important
        UNIT_MAX_CONDITION = 45*600;            //*min of life. In KaM it's 45min
        UNIT_MIN_CONDITION = 10*600;             //If unit condition is less it will look for Inn

type
  TRenderMode = (rm2D, rm3D);

{Cursors}
type
  TCursorMode = (cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Houses);
  
const
  SETTINGS_FILE = 'KaM_Remake_Settings.ini';

  c_Default=1; c_Info=452;
  c_Dir0=511; c_Dir1=512; c_Dir2=513; c_Dir3=514; c_Dir4=515; c_Dir5=516; c_Dir6=517; c_Dir7=518; c_DirN=519;
  c_Scroll0=4; c_Scroll1=7; c_Scroll2=3; c_Scroll3=9; c_Scroll4=5; c_Scroll5=8; c_Scroll6=2; c_Scroll7=6;

  Cursors:array[1..19]of integer = (1,452,511,512,513,514,515,516,517,518,519,2,3,4,5,6,7,8,9);

  ScrollCursorOffset = 17;
  CursorOffsetsX:array[1..19] of integer = (0,0,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset);
  CursorOffsetsY:array[1..19] of integer = (0,9,0,0,0,0,0,0,0,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0,0,ScrollCursorOffset,ScrollCursorOffset);

{Controls}
type
  TButtonStyle = (bsMenu, bsGame);
  T3DButtonStateSet = set of (bs_Highlight, bs_Down, bs_Disabled);
  TFlatButtonStateSet = set of (fbs_Highlight, fbs_Selected, fbs_Disabled);

const
  LocalesCount = 6;
  Locales:array[1..LocalesCount,1..2]of shortstring = (
  ('eng','English'),
  ('deu','Deutch'),
  ('pol','Polish'),
  ('hun','Hungarian'),
  ('nld','Nederlands'),
  ('rus','Russian'));

{Massages}
type
  TKMMessageType = (msgText=491, msgHouse, msgUnit, msgHorn, msgQuill, msgScroll);


{Palettes}
const
 //Palette filename corresponds with pal_**** constant, except pal_lin which is generated proceduraly (filename doesn't matter for it)
 PalFiles:array[1..13]of string = (
 'map.bbm', 'pal0.bbm', 'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', 'setup.bbm', 'setup2.bbm', 'map.bbm',
 'mapgold.lbm', 'setup.lbm', 'pal1.lbm');
 pal_map=1; pal_0=2; pal_1=3; pal_2=4; pal_3=5; pal_4=6; pal_5=7; pal_set=8; pal_set2=9; pal_lin=10;
 pal2_mapgold=11; pal2_setup=12; pal2_1=13;

 //I couldn't find matching palettes for several entries, so I marked them 0 
 RX5Pal:array[1..40]of byte = (
 12,12,12,12,12,12,9,9,9,1,
 1,1,1,1,1,1,12,12,12,13,
 0,0,0,0,12,1,1,1,1,1,
 12,12,12,12,12,12,12,0,0,0);
 //I couldn't find matching palettes for several entries, so I marked them 0 
 RX6Pal:array[1..20]of byte = (
 8,8,8,8,8,8,9,9,9,1,
 1,1,1,1,1,1,0,0,0,0);

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
const
  TKMDirectionS: array[0..8]of string = ('N/A', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW');

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
  WarfareCosts:array[17..26,1..2]of TResourceType = (
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

{ Terrain }
type TPassability = (canAll,
                     canWalk, canWalkRoad, canBuild, canBuildIron, canBuildGold,
                     canMakeRoads, canMakeFields, canPlantTrees, canFish, canCrab);
     TPassabilitySet = set of TPassability;

const PassabilityStr:array[0..11] of string = (
    'None',
    'canAll',       // Cart blanche, e.g. for workers building house are which is normaly unwalkable} //Fenced house area (tiles that have been leveled) are unwalkable. People aren't allowed on construction sites
    'canWalk',      // General passability of tile for any walking units
    'canWalkRoad',  // Type of passability for Serfs when transporting goods, only roads have it
    'canBuild',     // Can we build a house on this tile?
    'canBuildIron', // Special allowance for Iron Mines
    'canBuildGold', // Special allowance for Gold Mines
    'canMakeRoads', // Thats less strict than house building, roads can be placed almost everywhere where units can walk, except e.g. bridges
    'canMakeFields',// Thats more strict than roads, cos e.g. on beaches you can't make fields
    'canPlantTrees',// If Forester can plant a tree here, dunno if it's the same as fields
    'canFish',      // Water tiles where fish can move around
    'canCrab'       // Sand tiles where crabs can move around
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

//Defines which animal prefers which terrain
const AnimalTerrain: array[31..38] of TPassability = (
    canWalk, canFish, canFish, canFish, canCrab, canFish, canFish, canFish);

//Direction order used in unit placement, makes swirl around input point
type TMoveDirection = (mdPosX=0, mdPosY=1, mdNegX=2, mdNegY=3);

type TGoInDirection = (gid_In=1, gid_Out=-1); //Switch to set if unit goes into house or out of it

type //Army_Flag=4962,
  TUnitThought = (th_None=0, th_Eat=1, th_Home, th_Build, th_Stone, th_Wood, th_Death, th_Quest);

const //Corresponding indices in units.rx
  ThoughtBounds:array[1..7,1..2] of word = (
  (6250,6257),(6258,6265),(6266,6273),(6274,6281),(6282,6289),(6290,6297),(6298,6305)
  );
      
type
  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;

  TWarriorOrder = (wo_Stop, wo_Walk);

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
    gs_CoalMiner, gs_GoldMiner, gs_IronMiner,
    gs_HorseBreeder, gs_SwineBreeder);

{Houses in game}
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

  //Statistics page in game menu
  //0=space, 1=house, 2=unit
  StatCount:array[1..8,1..8]of byte = (
  (1,2,0,1,2,0,1,2),
  (1,1,2,0,1,1,2,0),
  (1,1,2,0,1,1,2,0),
  (1,1,2,0,1,1,2,0),
  (1,1,1,2,0,0,0,0),
  (1,1,1,1,2,0,0,0),
  (1,1,2,0,0,0,0,0),
  (1,1,1,1,0,2,2,0));

  StatHouse:array[1..28] of THouseType = (
  ht_Quary, ht_Woodcutters, ht_FisherHut,
  ht_Farm, ht_Wineyard, ht_Mill, ht_Bakery,
  ht_Swine, ht_Stables, ht_Butchers, ht_Tannery,
  ht_Metallurgists, ht_IronSmithy, ht_ArmorSmithy, ht_WeaponSmithy,
  ht_CoalMine, ht_IronMine, ht_GoldMine,
  ht_Sawmill, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_SiegeWorkshop,
  ht_Barracks, ht_WatchTower,
  ht_TownHall, ht_Store, ht_School, ht_Inn );

  StatUnit:array[1..14] of TUnitType = (
  ut_StoneCutter, ut_Woodcutter, ut_Fisher,
  ut_Farmer, ut_Baker,
  ut_AnimalBreeder, ut_Butcher,
  ut_Metallurgist, ut_Smith,
  ut_Miner,
  ut_Lamberjack,
  ut_Recruit,
  ut_Serf, ut_Worker );

  //Building of certain house allows player to build following houses,
  //unless they are blocked in mission script of course
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
  (ht_Woodcutters,ht_WatchTower,ht_Wall,ht_None,ht_None,ht_None,ht_None,ht_None), //Quary
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

  Barracks_Order:array[1..9] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  //Number means ResourceType as it is stored in Barracks, hence it's not rt_Something
  TroopCost:array[ut_Militia..ut_Cavalry,1..4] of byte = (
  (5, 0, 0, 0), //Militia
  (1, 3, 5, 0), //Axefighter
  (2, 4, 6, 0), //Swordfighter
  (3, 9, 0, 0), //Bowman
  (4,10, 0, 0), //Crossbowman
  (3, 7, 0, 0), //Lance Carrier
  (4, 8, 0, 0), //Pikeman
  (1, 3, 5,11), //Scout
  (2, 4, 6,11)  //Knight
  );


const
//1-building area //2-entrance
HousePlanYX:array[1..HOUSE_COUNT,1..4,1..4]of byte = (
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Sawmill
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1)), //Iron smithy
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon smithy
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0)), //Coal mine
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1)), //Iron mine
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0)), //Gold mine
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1)), //Fisher hut
((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2)), //Bakery
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Farm
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0)), //Woodcutter
((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1)), //Armor smithy
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Store
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1)), //Stables
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //School
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Quarry
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Metallurgist
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2)), //Swine
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0)), //Watch tower
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Town hall
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon workshop
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1)), //Armor workshop
((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Barracks
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Mill
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1)), //Siege workshop
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2)), //Butcher
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Tannery
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //N/A
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1)), //Inn
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2)), //Wineyard
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,1,0))  //Wall
);

//Does house output needs to be ordered by Player or it keeps on producing by itself
HousePlaceOrders:array[1..HOUSE_COUNT] of boolean = (
false,false,true,false,false,false,false,false,false,false,
true,false,false,false,false,false,false,false,false,true,
true,false,false,true,false,false,false,false,false,false);

//What does house produces
HouseOutput:array[1..HOUSE_COUNT,1..4] of TResourceType = (
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
(rt_Wine,       rt_None,       rt_None,       rt_None), //Wineyard       
(rt_None,       rt_None,       rt_None,       rt_None)  //Wall
);

//What house requires
HouseInput:array[1..HOUSE_COUNT,1..4] of TResourceType = (
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
(rt_None,       rt_None,       rt_None,       rt_None), //Wineyard      
(rt_None,       rt_None,       rt_None,       rt_None)  //Wall
);


{Houses UI}
const
  GUIBuildIcons:array[1..HOUSE_COUNT]of word = (
  301, 302, 303, 304, 305,
  306, 307, 308, 309, 310,
  311, 312, 313, 314, 315,
  316, 317, 318, 319, 320,
  321, 322, 323, 324, 325,
  326, 327, 328, 329, 338);

  GUIHouseOrder:array[1..HOUSE_COUNT]of THouseType = (
    ht_School, ht_Inn, ht_Quary, ht_Woodcutters, ht_Sawmill,
    ht_Farm, ht_Mill, ht_Bakery, ht_Swine, ht_Butchers,
    ht_Wineyard, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_WeaponWorkshop,
    ht_Tannery, ht_ArmorWorkshop, ht_Stables, ht_IronMine, ht_IronSmithy,
    ht_WeaponSmithy, ht_ArmorSmithy, ht_Barracks, ht_Store, ht_WatchTower,
    ht_FisherHut, ht_TownHall, ht_SiegeWorkshop, ht_None, ht_Wall);

{Terrain}
type
  TFieldType = (ft_None=0, ft_Road, ft_Corn, ft_Wine); //This is used only for querrying
  THouseStage = (hs_None, hs_Plan, hs_Fence, hs_Built);

  TTileOverlay = (to_None=0, to_Dig1, to_Dig2, to_Dig3, to_Dig4, to_Road );

  TMarkup = (mu_None=0, mu_RoadPlan, mu_FieldPlan, mu_WinePlan, mu_HousePlan, mu_HouseFence, mu_House, mu_UnderConstruction);
  //Nothing
  //Road/Corn/Wine ropes
  //Rope outline of house area
  //Fence outline of house area
  //Actual house, which is not rendered and is used in here to siplify whole thing
  //Underconstruction tile, house area being flattened and roadworks

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

//Ages at which tree grows up / changes sprite
TreeAge1 = 200;  //I did measured only corn, and it was ~195sec
TreeAge2 = 400;
TreeAge3 = 600;
TreeAgeFull = 800; //Tree is old enough to be chopped

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

//The frame shown when a unit is standing still in ua_walk. Same for all units!
const UnitStillFrames: array[TKMDirection] of byte = (0,3,2,2,1,6,7,6,6);

type
  TPlayerID = (play_none=0, play_1=1, play_2=2, play_3=3, play_4=4, play_5=5, play_6=6, play_7=7, play_8=8);

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
    sfx_unknown21, //Pig?
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
    sfx_Melee34, //Killed by shot?
    sfx_Melee35, //Killed by stone?
    sfx_Melee36, //Killed by stone?
    sfx_Melee37, //Smacked?
    sfx_Melee38,
    sfx_Melee39,
    sfx_Melee40,
    sfx_Melee41, //House hit
    sfx_Melee42, //Clung?
    sfx_Melee43,
    sfx_Melee44, //Killed
    sfx_Melee45,
    sfx_Melee46,
    sfx_Melee47, //House hit
    sfx_Melee48,
    sfx_Melee49,
    sfx_Melee50,
    sfx_Melee51, //Sword-sword
    sfx_Melee52, //Sword-sword
    sfx_Melee53, //Sword-sword
    sfx_Melee54, //Sword-sword
    sfx_Melee55,
    sfx_Melee56,
    sfx_Melee57,
    sfx_BowDraw,
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
    //Below are TPR sounds ...
    sfx_SlingerShoot,
    sfx_BalistaShoot,
    sfx_CatapultShoot,
    sfx_unknown76,
    sfx_CatapultReload,
    sfx_SiegeBuildingSmash
        );

type
  //Properties of map elements, e.g. passibility. Mostly unknown.
  TMapElemProperties = (
    mep_u1=1,
    mep_u2,
    mep_u3,
    mep_u4,
    mep_CuttableTree,   //This tree can be cut by a woodcutter
    mep_u6,
    mep_Double,         //Draw two sprites per object (grapes)
    mep_u8,
    mep_AllBlocked,     //All passibility blocked. Can't walk, build, swim, etc.
    mep_u10,
    mep_Quad,           //Draw 4 sprites per object (corn)
    mep_u12,
    mep_u13,
    mep_u14,
    mep_u15,
    mep_u16);

var
  //Players colors
  TeamColors:array[1..MAX_PLAYERS]of cardinal = (
  $FF3040FF, //Red
  $FF00C0FF, //Orange
  $FF00FFFF, //Yellow
  $FF28C840, //Green
  $FFC0C040, //Cyan
  $FFC00000, //Blue
  $FFFF00FF, //Violet
  $FF282828  //Black
  );

  GlobalTickCount:integer=-1; //So that first number after inc() would be 0
  GameplayTickCount:integer=0; //So that first tick will be #1

  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  ExeDir:string;

  CursorMode:record //It's easier to store it in record
    Mode:TCursorMode;
    Param:byte;
  end;

  Scrolling: boolean;

  CursorX,CursorY:single;    //Precise cursor position on map
  CursorXc,CursorYc:integer; //Cursor position cell

  //Pallete for RX bitmaps
  //There are 9 palette files Map, Pal0-5, Setup and Setup2
  //+1 linear
  //+2lbm
  Pal:array[1..13,1..256,1..3]of byte;

  RXData:array [1..6]of record
    Title:string;
    Qty:integer;
    Pal:array[1..9500] of byte;
    Size:array[1..9500,1..2] of word;
    Pivot:array[1..9500] of record x,y:integer; end;
    Data:array[1..9500] of array of byte;
    NeedTeamColors:boolean;
  end;

  GFXData: array [1..6,1..9500] of record
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

  //Swine&Horses, 5 beasts in each house, 3 ages for each beast
  HouseDATs:array[1..2,1..5,1..3] of packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;

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
  EntranceOffsetXpx,EntranceOffsetYpx:shortint; //When entering house units go for the door, which is offset by these values
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
  Foot:array[1..36]of shortint; //Sound indices vs sprite ID
end;

//Resource types serf carries around
SerfCarry:array[1..28] of packed record
  Dir:array[1..8]of packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;
end;

//
UnitStat:array[1..41]of record
  x1,Attack,AttackHorseBonus,x4,HitPoints,Speed,x7,Sight:smallint;
  x9,x10:shortint;
  CanWalkOut,x11:smallint;
end;

UnitSprite:array[1..41]of packed record
  Act:array[1..14]of packed record
    Dir:array[1..8]of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;
  end;
end;
UnitSprite2:array[1..41,1..18]of smallint; //Sound indices vs sprite ID

  MapElemQty:integer=254; //Default qty
  MapElem:array[1..512]of packed record
    Step:array[1..30]of smallint;               //60
    Count:word;                                 //62
    Properties:array[TMapElemProperties]of word; //94
    u2:shortint;                                //95
    CanBeRemoved,u4:word;                       //99
  end;

  //Unused by KaM Remake
  PatternDAT:array[1..256]of packed record
    MinimapColor:byte;
    Walkable:byte;  //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
    Buildable:byte; //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
    TileType:byte;  //This looks like a 0..31 bitfield, --||--
    u1:byte; //Boolean IsTransitionTile?
    u2:byte;
  end;
  TileTable:array[1..30,1..30]of packed record
    Tile1,Tile2,Tile3:byte;
    b1,b2,b3,b4,b5,b6,b7:boolean;
  end;

  //Minimap tile colors, computed on tileset loading
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
    procedure Clearup; virtual;
    procedure AddEntry(aLoc:TKMPoint); dynamic;
    function RemoveEntry(aLoc:TKMPoint):cardinal; virtual;
    function GetRandom():TKMPoint;
  end;


type TKMPointTagList = class (TKMPointList)
  public
    Tag,Tag2:array of integer; //1..Count
    procedure Clearup; override;
    procedure AddEntry(aLoc:TKMPoint; aTag,aTag2:cardinal); reintroduce;
    function RemoveEntry(aLoc:TKMPoint):cardinal; override;
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
    //AppendLog adds the line to Log along with time passed since previous line added
    procedure AppendLog(text:string); overload;
    procedure AppendLog(text:string; num:integer); overload;
    procedure AppendLog(text:string; num:single ); overload;
    procedure AppendLog(num:integer; text:string); overload;
    procedure AppendLog(text:string; Res:boolean); overload;
    procedure AppendLog(a,b:integer); overload;
    //Add line if TestValue=false
    procedure AssertToLog(TestValue:boolean; MessageText:string);
    //AddToLog simply adds the text
    procedure AddToLog(text:string);
  end;

type TKMEvent = (evMouseDown=256, evMouseUp, evMouseMove); //reserve 2bytes at least

{This is custom logging system}
type
  TKMEventLog = class
  private
    fe:textfile;
    logfile:string; //Path to log file
  public
    constructor Create(path:string);
    procedure AddToLog(TickCount:integer; EventID:TKMEvent; Param1,Param2,Param3,Param4:integer);
  end;

var
  fLog: TKMLog;
  fEventLog: TKMEventLog;

function TypeToString(t:THouseType):string; overload
function TypeToString(t:TResourceType):string; overload
function TypeToString(t:TUnitType):string; overload
function TypeToString(t:TKMPoint):string; overload
function TypeToString(t:TKMDirection):string; overload

implementation
uses KM_LoadLib;

{Reset event log file}
constructor TKMEventLog.Create(path:string);
begin
  logfile:=path;
  assignfile(fe,logfile);
  rewrite(fe);
  closefile(fe);
end;

procedure TKMEventLog.AddToLog(TickCount:integer; EventID:TKMEvent; Param1,Param2,Param3,Param4:integer);
begin
  assignfile(fe,logfile);
  append(fe);
  writeln(fe,TickCount,' ',integer(EventID),' ',Param1,' ',Param2,' ',Param3,' ',Param4);
  closefile(fe);
end;

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

procedure TKMLog.AppendLog(text:string; num:single);
begin
  AddLine(text+' '+FloatToStr(num));
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

procedure TKMLog.AssertToLog(TestValue:boolean; MessageText:string);
begin
  if not TestValue then
  AddLine('ASSERTION FAILED! Msg: ' + MessageText);
  Assert(TestValue, 'ASSERTION FAILED! Msg: ' + MessageText);
end;

procedure TKMLog.AddToLog(text:string);
begin
  AddLineNoTime(text);
end;


{TypeToString routines}
function TypeToString(t:TUnitType):string;
var s:string;
begin
if byte(t) in [1..29] then
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


function TypeToString(t:TKMDirection):string;
begin
  Result:=TKMDirectionS[byte(t)];
end;


{ TKMList }
procedure TKMList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    TObject(Items[I]).Free;
    Items[I]:=nil;
  end;
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
  if Count>length(List)-1 then setlength(List,Count+32);
  List[Count]:=aLoc;
end;


{Remove point from the list if is there. Return 'true' if succeded}
function TKMPointList.RemoveEntry(aLoc:TKMPoint):cardinal;
var i: integer; Found: boolean;
begin
  Result:=0;
  Found := false;
  for i:=1 to Count do
  begin
    if (KMSamePoint(List[i],aLoc) and (not Found)) then
    begin
      dec(Count);
      Found := true;
      Result:=i;
    end;
    if (Found) and (i < Count) then List[i] := List[i+1];
  end;
end;


function TKMPointList.GetRandom():TKMPoint;
begin
  if Count=0 then Result:=KMPoint(0,0)
             else Result:=List[random(Count)+1];
end;


procedure TKMPointTagList.Clearup;
begin
  inherited;
  setlength(Tag,0);
  setlength(Tag2,0);
end;


procedure TKMPointTagList.AddEntry(aLoc:TKMPoint; aTag,aTag2:cardinal);
begin
  inherited AddEntry(aLoc);
  if Count>length(Tag)-1 then setlength(Tag,Count+32);
  if Count>length(Tag2)-1 then setlength(Tag2,Count+32);
  Tag[Count]:=aTag;
  Tag2[Count]:=aTag2;
end;


function TKMPointTagList.RemoveEntry(aLoc:TKMPoint):cardinal;
var i: integer;
begin
  Result:= inherited RemoveEntry(aLoc);

  for i:=Result to Count-1 do
  begin
    Tag[i] := Tag[i+1];
    Tag2[i] := Tag2[i+1];
  end;
end;


end.
