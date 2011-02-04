unit KM_Defaults;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, dglOpenGL, KM_CommonTypes;

//Global const
const
//|===================| <- constant name length                 
  CELL_SIZE_PX          = 40;           //Single cell size in pixels (width)
  CELL_HEIGHT_DIV       = 33.333;       //Height divider, controlls terrains pseudo-3d look
  ToolBarWidth          = 224;          //Toolbar width in game
  //GAME_LOGIC_PACE       = 100;          //Game logic should be updated each 100ms
  TERRAIN_PACE          = 10;           //Terrain gets updated once per ** ticks (10 by default), Warning, it affects tree-corn growth rate
  //SPEEDUP_MULTIPLIER    = 10;           //Increase of game pace on F8
  ACTION_TIME_DELTA     = 0.1;          //Multiplied with units speed gives distance unit walks per frame

  FOG_OF_WAR_MIN        = 80;           //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  FOG_OF_WAR_ACT        = 160;          //Until this value FOW is not rendered at all
  FOG_OF_WAR_MAX        = 255;          //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  FOG_OF_WAR_INC        = 128;          //Increment for FOW
  FOG_OF_WAR_DEC        = 12;           //Decrement for FOW

  FPS_LAG               = 33;           //Allowed lag between frames, 1000/FPSLag = max allowed FPS, 1 means unlimited
  FPS_INTERVAL          = 1000;         //Time in ms between FPS measurements, bigger value = more accurate result
  SCROLLSPEED           = 1;            //This is the speed that the viewport will scroll every 100 ms, in cells
  SCROLLFLEX            = 4;            //This is the number of pixels either side of the edge of the screen which will count as scrolling
  MENU_DESIGN_X         = 1024;         //Thats the size menu was designed for. All elements are placed in this size
  MENU_DESIGN_Y         = 768;          //Thats the size menu was designed for. All elements are placed in this size
  MENU_SP_MAPS_COUNT    = 14;           //Number of single player maps to display in menu

  GAME_VERSION          = '2nd Fighting Demo r1371';       //Game version string displayed in menu corner
  SAVE_VERSION          = 'r1371';       //Should be updated for every release (each time save format is changed)
  REPLAY_VERSION        = 1371;          //Used in replay file format

var
  //These should be TRUE
  MAKE_ANIM_TERRAIN     :boolean=true;  //Should we animate water and swamps
  MAKE_UNIT_SPRITES     :boolean=true;  //Whenever to make Units graphics or not, saves time for GUI debug
  MAKE_HOUSE_SPRITES    :boolean=true;  //Whenever to make Houses graphics or not, saves time for GUI debug
  MAKE_TEAM_COLORS      :boolean=true;  //Whenever to make team colors or not, saves RAM for debug
  DO_UNIT_HUNGER        :boolean=true;  //Wherever units get hungry or not
  DO_SERFS_WALK_ROADS   :boolean=true;  //Wherever serfs should walk only on roads
  CHEATS_ENABLED        :boolean=true;  //Enable cheats in game (add_resource, instant_win, etc)
  FREE_POINTERS         :boolean=true;  //If true, units/houses will be freed and removed from the list once they are no longer needed
  CAP_MAX_FPS           :boolean=true;  //Should limit rendering performance to avoid GPU overheating (disable to measure debug performance)
  CRASH_ON_REPLAY       :boolean=true;  //Crash as soon as replay consistency fails (random numbers mismatch)

  //Implemented
  MOUSEWHEEL_ZOOM_ENABLE:boolean=true; //Should we allow to zoom in game or not
  DO_UNIT_INTERACTION   :boolean=true; //Debug for unit interaction
  SMOOTH_SCROLLING      :boolean=true; //Smooth viewport scrolling
  ENABLE_FIGHTING       :boolean=true; //Allow fighting
  SHOW_MAPED_IN_MENU    :boolean=true; //Allows to hide all map-editor related pages from main menu
  DO_WEIGHT_ROUTES      :boolean=true; //Add additional cost to tiles in A* if they are occupied by other units (IsUnit=1)
  //Not fully implemented yet
  CHECK_WIN_CONDITIONS  :boolean=true; //Disable for debug missions where enemies aren't properly set
  LOAD_UNIT_RX_FULL     :boolean=false; //Clip UnitsRX to 7885 sprites until we add TPR ballista/catapult support
  FOG_OF_WAR_ENABLE     :boolean=false; //Whenever dynamic fog of war is enabled or not
  KAM_WATER_DRAW        :boolean=false; //Sketching Kam-like sand underwater
  ENABLE_MP_IN_MENU     :boolean=false; //Keep Multiplayer disabled until it's rigged
                       
  //These are debug things, should be FALSE
  {User interface options}
  SHOW_DEBUG_CONTROLS   :boolean=false; //Show debug panel / Form1 menu (F11)
  SHOW_CONTROLS_OVERLAY :boolean=false; //Draw colored overlays ontop of controls, usefull for making layout (F6)! always Off here
  ENABLE_DESIGN_CONTORLS:boolean=false; //Enable special mode to allow to move/edit controls
   MODE_DESIGN_CONTORLS :boolean=false; //Special mode to move/edit controls activated by F7, it must block OnClick events! always Off here
  SHOW_1024_768_OVERLAY :boolean=false; //Render constraining frame
  {Gameplay display}
  SHOW_TERRAIN_WIRES    :boolean=false; //Makes terrain height visible
  SHOW_UNIT_ROUTES      :boolean=false; //Draw unit routes
  SHOW_PROJECTILES      :boolean=false; //Shows projectiles trajectory
  SHOW_POINTER_DOTS     :boolean=false; //Show pointer count as small dots below unit
  SHOW_UNIT_MOVEMENT    :boolean=false; //Draw unit movement overlay (occupied tile), Only if unit interaction enabled
  SHOW_WALK_CONNECT     :boolean=false; //Show floodfill areas of interconnected areas
  TEST_VIEW_CLIP_INSET  :boolean=false; //Renders smaller area to see if everything gets clipped well
  SHOW_SPRITES_RECT     :boolean=false; //Render outline around every sprite
  SHOW_ATTACK_RADIUS    :boolean=false; //Render towers/archers attack radius
  DISPLAY_SOUNDS        :boolean=false; //Display sounds on map
  RENDER_3D             :boolean=false; //Experimental 3D render
  {Stats}
  SHOW_SPRITE_COUNT     :boolean=false; //display rendered controls/sprites count
  SHOW_POINTER_COUNT    :boolean=false; //Show debug total count of unit/house pointers being tracked
  SHOW_CMDQUEUE_COUNT   :boolean=false; //Show how many commands were processed and stored by TGameInputProcess
  {Gameplay cheats}
  FREE_ROCK_THROWING    :boolean=false; //Throwing a rock from Tower costs nothing. To debug throw algoritm
  REDUCE_SHOOTING_RANGE :boolean=false; //Reduce shooting range for debug
  {Data output}
  WRITE_DECODED_MISSION :boolean=false; //Save decoded mission as txt file 
  WRITE_DELIVERY_LOG    :boolean=false; //Write even more output into log + slows down game noticably
  WRITE_WALKTO_LOG      :boolean=false; //Write even more output into log + slows down game noticably
  WriteResourceInfoToTXT:boolean=false; //Whenever to write txt files with defines data properties on loading
  WriteAllTexturesToBMP :boolean=false; //Whenever to write all generated textures to BMP on loading (extremely time consuming)

  //Statistic
  CtrlPaintCount:word; //How many Controls were painted in last frame

  //Utility
  Zero:integer=0; //used in SaveStream to represent NIL

const
  MAX_RES_IN_HOUSE=5;     //Maximum resource items allowed to be in house
  MAX_ORDER=999;          //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc)
  MAX_TEX_RESOLUTION=512; //Maximum texture resolution client can handle (used for packing sprites)
  RX7_SPRITE_COUNT = 22;  //Number of sprites to load for RX7 from the folder \Sprites\

const
  HOUSE_COUNT = 29;       //Number of KaM houses is 29
  MAX_PLAYERS = 8;        //Maximum players per map
  SAVEGAME_COUNT = 10;    //Savegame slots available in game menu
  AUTOSAVE_SLOT = 10;     //Slot ID used for autosaving
  AUTOSAVE_COUNT = 3;

const //Here we store options that are hidden somewhere in code
  MAX_WARFARE_IN_BARRACKS = 255;          //Maximum number of weapons in the barracks from producers. Not a big problem as they are not from the store.
  MAX_WARFARE_IN_BARRACKS_FROM_STORE = 32;//Maximum number of weapons in the barracks from storehouse. e.g. AI starts with 999 axes, don't spend entire game filling up the barracks.
  GOLD_TO_SCHOOLS_IMPORTANT = true;       //Whenever gold delivery to schools is highly important
  FOOD_TO_INN_IMPORTANT = true;           //Whenever food delivery to inns is highly important
  UNIT_MAX_CONDITION = 45*600;            //*min of life. In KaM it's 45min
  UNIT_MIN_CONDITION = 6*600;             //If unit condition is less it will look for Inn. In KaM it's 6min
  TIME_BETWEEN_MESSAGES = 4*600;          //Time between messages saying house is unoccupied or unit is hungry. In KaM it's 4 minutes
  TROOPS_FEED_MAX = 0.75;                 //Maximum amount of condition a troop can have to order food (more than this means they won't order food)
  TROOPS_TRAINED_CONDITION = 0.75;        //Condition troops start with when trained

  //Unit mining ranges. (measured from KaM)
  RANGE_WOODCUTTER  = 10;
  RANGE_FARMER      = 8;
  RANGE_STONECUTTER = 14;
  RANGE_FISHERMAN   = 12;

  //Archer properties
  RANGE_ARBALETMAN_MAX  = 10.99; //KaM: Unit standing 10 tiles from us will be shot, 11 tiles not
  RANGE_BOWMAN_MAX      = 10.99;
  RANGE_WATCHTOWER_MAX  = 6.99; //Measured in KaM. Distance from the doorway of tower

  RANGE_ARBALETMAN_MIN  = 4; //KaM: We will shoot a unit standing 4 tiles away, but not one standing 3 tiles away
  RANGE_BOWMAN_MIN      = 4;
  RANGE_WATCHTOWER_MIN  = 0; //In KaM towers have no minimum range, they will shoot any unit less than the range

  LINK_RADIUS = 8; //Radius to search for groups to link to after being trained at the barracks

  FIRING_DELAY = 0; //on which frame archer fires his arrow/bolt
  AIMING_DELAY_MIN = 4; //minimum time for archer to aim
  AIMING_DELAY_ADD = 8; //random component


type
  TCampaign = (cmp_Nil, cmp_TSK, cmp_TPR, cmp_Custom);

const
  //Maps count in Campaigns
  MAX_MAPS = 32;
  TSK_MAPS = 20;
  TPR_MAPS = 14;

  //X/Y locations of battlefields on campaign map in menu
  TSK_Campaign_Maps: array [1..TSK_MAPS,1..2] of word = (
  (170,670),(160,455),(320,595),(442,625),(395,525),(350,420),( 95,345),(140,190),(550,520),(735,510),
  (885,550),(305,290),(380,270),(475,290),(580,290),(820,175),(700,145),(595, 40),(720, 80),(820, 50));

  TPR_Campaign_Maps: array [1..TPR_MAPS,1..2] of word = (
  (175,525),(180,465),(110,435),(180,420),(230,440),(370,475),(130,205),(350,185),(515,355),(590,360),
  (665,190),(760,285),(775,220),(730,160));

  CampIntermediate = 20; //Place intermediate nodes every N pixels

type
  TRenderMode = (rm2D, rm3D);

{Cursors}
type
  TCursorMode = (
                  cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Wall, cm_Houses, //Gameplay
                  cm_Height, cm_Tiles, cm_Objects, cm_Units); //MapEditor

const
  MAPED_HEIGHT_CIRCLE = 0;
  MAPED_HEIGHT_SQUARE = 1;

  MAPED_TILES_COLS = 6;
  MAPED_TILES_ROWS = 8;


const
  SETTINGS_FILE = 'KaM_Remake_Settings.ini';

  //Cursor names and GUI sprite index
  c_Default=1; c_Info=452; c_Attack=457; c_JoinYes=460; c_JoinNo=450; c_Edit=453;
  c_Dir0=511; c_Dir1=512; c_Dir2=513; c_Dir3=514; c_Dir4=515; c_Dir5=516; c_Dir6=517; c_Dir7=518; c_DirN=519;
  c_Scroll0=4; c_Scroll1=7; c_Scroll2=3; c_Scroll3=9; c_Scroll4=5; c_Scroll5=8; c_Scroll6=2; c_Scroll7=6;
  c_Invisible=999;

  Cursors:array[1..24]of integer = (1,452,457,460,450,453,511,512,513,514,515,516,517,518,519,2,3,4,5,6,7,8,9,999);

  ScrollCursorOffset = 17;
  CursorOffsetsX:array[1..24] of integer = (0,0,20, 0, 0, 0, 0, 1,1,1,0,-1,-1,-1,0,0,ScrollCursorOffset,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0);
  CursorOffsetsY:array[1..24] of integer = (0,9,10,18,20,-1, 0,-1,0,1,1, 1, 0,-1,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0,0,ScrollCursorOffset,ScrollCursorOffset,0);

const DirCursorSqrSize  = 33; //Length of square sides
      DirCursorNARadius = 14;  //Radius of centeral part that is dir_NA

{Controls}
type
  TButtonStyle = (bsMenu, bsGame); //Menu buttons are metal, game buttons are stone
  T3DButtonStateSet = set of (bs_Highlight, bs_Down, bs_Disabled);
  TFlatButtonStateSet = set of (fbs_Highlight, fbs_Selected, fbs_Disabled);

const
  LOCALES_COUNT = 8;
  Locales:array[1..LOCALES_COUNT, 1..2]of shortstring = (
  ('eng', 'English'),
  ('ger', 'German'),
  ('pol', 'Polish'),
  ('svk', 'Slovak'), //New one
  ('fr',  'French'), //New one
  ('hun', 'Hungarian'),
  ('dut', 'Dutch'),
  ('rus', 'Russian'));


type TGameResultMsg = (     //Game result
        gr_Win,         //Player has won the game
        gr_Defeat,      //Player was defeated
        gr_Cancel,      //Game was cancelled (unfinished)
        gr_Error,       //Some known error occured
        gr_Silent,      //Used when loading savegame from running game (show no screens)
        gr_ReplayEnd,   //Replay was cancelled - return to menu without screens
        gr_MapEdEnd);   //Map Editor was closed - return to menu without screens

               
{Palettes}
type TKMPal = (pal_map=1, pal_0=2, pal_1=3, pal_2=4, pal_3=5, pal_4=6, pal_5=7, pal_set=8, pal_set2=9, pal_lin=10,
               pal2_mapgold=11, pal2_setup=12);
const
  DEF_PAL = pal_0;            //Default palette to use when generating full-color RGB textures

var //There are 9 palette files Map, Pal0-5, Setup and Setup2 +1 linear +2lbm
  Pal:array[TKMPal,0..255,1..3]of byte;

const
 //Palette filename corresponds with pal_**** constant, except pal_lin which is generated proceduraly (filename doesn't matter for it)
 PalFiles:array[TKMPal]of string = (
 'map.bbm', 'pal0.bbm', 'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', 'setup.bbm', 'setup2.bbm', 'map.bbm',
 'mapgold.lbm', 'setup.lbm');

 RX5Pal:array[1..40]of TKMPal = (
 pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup, pal_set2, pal_set2, pal_set2, pal_map,
 pal_map, pal_map, pal_map, pal_map, pal_map, pal_map,pal2_setup,pal2_setup,pal2_setup,pal2_mapgold,
 pal2_mapgold,pal2_mapgold,pal2_mapgold,pal2_mapgold,pal2_setup, pal_map, pal_map, pal_map, pal_map, pal_map,
 pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup, pal_lin, pal_lin, pal_lin);
 //I couldn't find matching palettes for the 17th and 18th entries
 RX6Pal:array[1..20]of TKMPal = (
 pal_set,pal_set,pal_set,pal_set,pal_set,pal_set,pal_set2,pal_set2,pal_set2,pal_map,
 pal_map,pal_map,pal_map,pal_map,pal_map,pal_map,pal_lin,pal_lin,pal_lin,pal_lin);

{Fonts}
type //Indexing should start from 1.
  TKMFont = (fnt_Antiqua=1,fnt_Briefing, fnt_Game,     fnt_Grey,
             fnt_KMLobby0, fnt_KMLobby1, fnt_KMLobby2, fnt_KMLobby3,
             fnt_KMLobby4, fnt_MainA,    fnt_MainB,    fnt_MainMapGold,
             fnt_Metal,    fnt_Mini,     fnt_Outline,  fnt_System,
             fnt_Won);

const //Font01.fnt seems to be damaged..
  FontFiles: array[TKMFont]of string = (
  'antiqua','briefing','game','grey','kmlobby0','kmlobby1','kmlobby2','kmlobby3','kmlobby4','maina',
  'mainb','mainmapgold','metal','mini','outline','system','won');

  //adam - unused
  //font01 - damaged
  //minimum - unused (looks just like mini with even smaller digits)

  //Note: Fonts with palette 0 are using custom coloring,
  //since no existing palette matches them well and they are monochrome
  FontPal:array[TKMFont]of TKMPal =
  (pal_0, pal_map,pal_lin, pal_0,pal2_setup,pal2_setup,pal2_setup,pal2_setup,pal2_setup, pal_set,
   pal_lin,pal2_mapgold, pal_0,pal_lin, pal_0,pal_lin, pal_set2);

//Which MapEditor page is being shown. Add more as they are needed.
type TKMMapEdShownPage = (esp_Unknown, esp_Terrain, esp_Buildings, esp_Units);

type
  TAllianceType = (at_Enemy=0, at_Ally=1);

type
  TKMDirection = (dir_NA=0, dir_N=1, dir_NE=2, dir_E=3, dir_SE=4, dir_S=5, dir_SW=6, dir_W=7, dir_NW=8);
const
  TKMDirectionS: array[0..8]of string = ('N/A', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW');
  TKMCursorDirections: array[TKMDirection]of integer = (c_DirN,c_Dir0,c_Dir1,c_Dir2,c_Dir3,c_Dir4,c_Dir5,c_Dir6,c_Dir7);

{Resources}
type
  TResourceType = (
    rt_None      =0 , rt_All        =30, rt_Warfare    =31, rt_Food        =32, //Special resource types
    rt_Trunk     =1 , rt_Stone      =2 , rt_Wood       =3 , rt_IronOre     =4 ,
    rt_GoldOre   =5 , rt_Coal       =6 , rt_Steel      =7 , rt_Gold        =8 ,
    rt_Wine      =9 , rt_Corn       =10, rt_Bread      =11, rt_Flour       =12,
    rt_Leather   =13, rt_Sausages   =14, rt_Pig        =15, rt_Skin        =16,
    rt_Shield    =17, rt_MetalShield=18, rt_Armor      =19, rt_MetalArmor  =20,
    rt_Axe       =21, rt_Sword      =22, rt_Pike       =23, rt_Hallebard   =24,
    rt_Bow       =25, rt_Arbalet    =26, rt_Horse      =27, rt_Fish        =28);

const //Using shortints instead of bools makes it look much neater in code-view
  CheatStorePattern:array[1..28]of byte = (
  0,0,1,0,0,
  0,1,0,1,0,
  1,0,0,0,1,
  1,0,0,0,1,
  1,1,1,1,1,
  0,0,0); 

const {Aligned to right to use them in GUI costs display as well}
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
type TPassability = (CanWalk=1, CanWalkRoad, CanBuild, CanBuildIron, CanBuildGold,
                     CanMakeRoads, CanMakeFields, CanPlantTrees, CanFish, CanCrab,
                     CanWolf, CanElevate, CanWorker); //13bits so far
     TPassabilitySet = set of TPassability;

type TWalkConnect = (wcWalk, wcRoad, wcFish, wcAvoid);

const PassabilityStr:array[TPassability] of string = (
    'canWalk',      // General passability of tile for any walking units
    'canWalkRoad',  // Type of passability for Serfs when transporting goods, only roads have it
    'canBuild',     // Can we build a house on this tile?
    'canBuildIron', // Special allowance for Iron Mines
    'canBuildGold', // Special allowance for Gold Mines
    'canMakeRoads', // Thats less strict than house building, roads can be placed almost everywhere where units can walk, except e.g. bridges
    'canMakeFields',// Thats more strict than roads, cos e.g. on beaches you can't make fields
    'canPlantTrees',// If Forester can plant a tree here, dunno if it's the same as fields
    'canFish',      // Water tiles where fish can move around
    'canCrab',      // Sand tiles where crabs can move around
    'canWolf',      // Soil tiles where wolfs can move around
    'canElevate',   // Nodes which are forbidden to be elevated by workers (house basements, water, etc..)
    'canWorker'     // Like canWalk but allows walking on building sites
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


//Used to separate close-combat units from archers (they use different fighting logic)
type TFightType = (ft_Melee=0, ft_Ranged);

const WarriorFightType: array[ut_Militia..ut_Barbarian] of TFightType = (
    ft_Melee,ft_Melee,ft_Melee, //ut_Militia, ut_AxeFighter, ut_Swordsman
    ft_Ranged,ft_Ranged,        //ut_Bowman, ut_Arbaletman
    ft_Melee,ft_Melee,  //ut_Pikeman, ut_Hallebardman,
    ft_Melee,ft_Melee,      //ut_HorseScout, ut_Cavalry,
    ft_Melee                    //ut_Barbarian
    //TPR Army
    {ft_Melee,        //ut_Peasant
    ft_Ranged,           //ut_Slingshot
    ft_Melee,            //ut_MetalBarbarian
    ft_Melee,          //ut_Horseman
    ft_Ranged,ft_Ranged, //ut_Catapult, ut_Ballista,}
    );


//Used for AI defence and linking troops
type TGroupType = (gt_None=0, gt_Melee, gt_AntiHorse, gt_Ranged, gt_Mounted);

const UnitGroups: array[15..24] of TGroupType = (
    gt_Melee,gt_Melee,gt_Melee, //ut_Militia, ut_AxeFighter, ut_Swordsman
    gt_Ranged,gt_Ranged,        //ut_Bowman, ut_Arbaletman
    gt_AntiHorse,gt_AntiHorse,  //ut_Pikeman, ut_Hallebardman,
    gt_Mounted,gt_Mounted,      //ut_HorseScout, ut_Cavalry,
    gt_Melee                    //ut_Barbarian
    //TPR Army
    {gt_AntiHorse,        //ut_Peasant
    gt_Ranged,           //ut_Slingshot
    gt_Melee,            //ut_MetalBarbarian
    gt_Mounted,          //ut_Horseman
    gt_Ranged,gt_Ranged, //ut_Catapult, ut_Ballista,}
    );

//AI's prefences for training troops
const AITroopTrainOrder: array[TGroupType,1..3] of TUnitType = (
  (ut_None,         ut_None,       ut_None),
  (ut_Swordsman,    ut_AxeFighter, ut_Militia),
  (ut_Hallebardman, ut_Pikeman,    ut_None),
  (ut_Arbaletman,   ut_Bowman,     ut_None),
  (ut_Cavalry,      ut_HorseScout, ut_None));


const FlagXOffset: array[TGroupType, 1..8] of shortint = (
    ( 0,  0,  0,  0,  0,  0,  0,  0),  //gt_None
    (10, -1,  2,  1, -6,-10,  4, 13),  //gt_Melee
    ( 6,  5,  7, -3,-10, -4, 10,  9),  //gt_AntiHorse
    ( 8,  6,  6, -6, -8, -3,  8,  6),  //gt_Ranged
    ( 6,  2,  3, -5,-10, -8,  5,  6)); //gt_Mounted

const FlagYOffset: array[TGroupType, 1..8] of shortint = (
    ( 0,  0,  0,  0,  0,  0,  0,  0),  //gt_None
    (28, 30, 30, 26, 25, 24, 25, 27),  //gt_Melee
    (23, 25, 25, 21, 20, 19, 20, 22),  //gt_AntiHorse
    (28, 30, 30, 26, 25, 24, 25, 27),  //gt_Ranged
    ( 4, 16, 16,  4,  5,  2,  3,  4)); //gt_Mounted


//Defines which animal prefers which terrain
const AnimalTerrain: array[31..38] of TPassability = (
    CanWolf, CanFish, CanFish, CanFish, CanCrab, CanFish, CanFish, CanFish);

type TGoInDirection = (gd_GoInside=1, gd_GoOutside=-1); //Switch to set if unit goes into house or out of it

type //Army_Flag=4962,
  TUnitThought = (th_None=0, th_Eat=1, th_Home, th_Build, th_Stone, th_Wood, th_Death, th_Quest);

const //Corresponding indices in units.rx
  ThoughtBounds:array[1..7,1..2] of word = (
  (6250,6257),(6258,6265),(6266,6273),(6274,6281),(6282,6289),(6290,6297),(6298,6305)
  );

type TProjectileType = (pt_Arrow=1, pt_Bolt, pt_TowerRock); {pt_BallistaRock, }

const //Corresponding indices in units.rx //pt_Arrow, pt_Bolt are unused
  ProjectileBounds:array[TProjectileType,1..2] of word = (
  (0,0),(0,0),(4186,4190)
  );

type
  TUnitTaskName = ( utn_Unknown=0, //Uninitialized task to detect bugs
        utn_SelfTrain, utn_Deliver,        utn_BuildRoad,  utn_BuildWine,        utn_BuildField,
        utn_BuildWall, utn_BuildHouseArea, utn_BuildHouse, utn_BuildHouseRepair, utn_GoHome,
        utn_GoEat,     utn_Mining,         utn_Die,        utn_GoOutShowHungry,  utn_AttackHouse,
        utn_ThrowRock);

type
  TUnitActionName = ( uan_Unknown=0, //Uninitialized action to detect bugs
        uan_Stay, uan_WalkTo, uan_GoInOut, uan_AbandonWalk, uan_Fight, uan_StormAttack);

type
  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;

  TWarriorOrder = (
    wo_None,
    wo_Walk,
    wo_WalkOut,
    wo_AttackUnit,
    wo_AttackHouse,
    wo_Storm
  );

  TWarriorState = (
    ws_None, //Warrior is idle
    ws_Walking, //Warrior is in the process of walking by player instruction (could have been ordered to attack too because there is no difference)
    ws_RepositionPause, //Warrior has just finished walking and is pausing breifly before repositioning (i.e. rotating to the final facing direction) Without this pause it looks too quick odd.
    ws_Engage //One or more of our group members are in combat and we are on our way to help them.
  );

{Walk to somewhere}
type
  //Status of interaction
  TInteractionStatus = (kis_None,       //We have not yet encountered an interaction (we are just walking)
                        kis_Pushing,    //We are pushing an idle unit out of the way
                        kis_Pushed,     //We were pushed (idle then asked to move)
                        kis_Trying,     //We are or have been stuck (difference between this and kis_None is only for debug)
                        kis_Waiting     //We have been stuck for a while so allow other units to swap with us
  );

//These are only for debug
const
  TInteractionStatusNames: array[TInteractionStatus] of string = ('None', 'Pushing', 'Pushed', 'Trying', 'Waiting');

const {Actions names}
  UnitAct:array[1..14]of string = ('ua_Walk', 'ua_Work', 'ua_Spec', 'ua_Die', 'ua_Work1',
             'ua_Work2', 'ua_WorkEnd', 'ua_Eat', 'ua_WalkArm', 'ua_WalkTool',
             'ua_WalkBooty', 'ua_WalkTool2', 'ua_WalkBooty2', 'ua_Unknown');
  //specifies what actions unit can perform, should be ajoined with speeds and other tables
  UnitSupportedActions:array[ut_Serf .. ut_Barbarian]of TUnitActionTypeSet = (
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkTool2],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die..ua_WalkBooty2],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Work1..ua_WalkBooty],
    [ua_Walk, ua_Work, ua_Die, ua_Eat, ua_Work1, ua_Work2],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkBooty],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Spec, ua_Die, ua_Eat], //Recruit
    [ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Eat], //Militia
    [ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat]
    );

type
  TGatheringScript = (
    gs_None=0,
    gs_WoodCutterCut, gs_WoodCutterPlant,
    gs_FarmerSow, gs_FarmerCorn, gs_FarmerWine,
    gs_FisherCatch,
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
  (ht_Sawmill,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Woodcutters
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_School,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Store
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Inn,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),  //School
  (ht_Woodcutters,ht_WatchTower,ht_None{,ht_Wall},ht_None,ht_None,ht_None,ht_None,ht_None), //Quary
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
  (ht_ArmorWorkshop,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),  //Tannery
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None),
  (ht_Quary,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None), //Inn
  (ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None)
  //,(ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None,ht_None)
  );

const
  School_Order:array[1..14] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);

  Barracks_Order:array[1..9] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  MapEd_Order:array[1..10] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian);

  MapEd_Icon:array[1..10] of word = (
    5, 6, 7, 8, 9,
    10, 11, 12, 13, 14);

  Animal_Order:array[1..8] of TUnitType = (
    ut_Wolf, ut_Fish,        ut_Watersnake, ut_Seastar,
    ut_Crab, ut_Waterflower, ut_Waterleaf,  ut_Duck);

  Animal_Icon:array[1..8] of word = (
    15, 16, 17, 18,
    19, 20, 21, 22);

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

  //For now it is the same as KaM
  //The number means how many items should be in houses input max
  DistributionDefaults: array[1..4,1..4]of byte = (
  (5,4,0,0),
  (5,3,4,5),
  (5,3,0,0),
  (5,3,2,0)
  );

const
//Tiles table made by JBSnorro, thanks to him :)
MapEdTileRemap:array[1..256]of integer = (
 1,73,74,75,37,21,22, 38, 33, 34, 32,181,173,177,129,130,131,132,133, 49,193,197,217,225,  0,  0, 45, 24, 13, 23,208,224,
27,76,77,78,36,39,40,198,100,101,102,189,169,185,134,135,136,137,138,124,125,126,229,218,219,220, 46, 11,  5,  0, 26,216,
28,79,80,81,35,88,89, 90, 70, 71, 72,182,174,178,196,139,140,141,142,127,128,  0,230,226,227,228, 47,204,205,206,203,207,
29,82,83,84,85,86,87,  0,112,113,114,190,170,186,161,162,163,164,165,106,107,108,233,234,231,  0, 48,221,213,214,199,200,
30,94,95,96,57,58,59,  0,103,104,105,183,175,179,157,202,158,159,160,117,118,119,209,210,241,245,194,248, 65, 66,195, 25,
31, 9,19,20,41,42,43, 44,  6,  7, 10,191,171,187,149,150,151,152, 16,242,243,244,235,238,239,240,  0, 50,172, 52,222,223,
18,67,68,69,91,92,93,  0,  3,  4,  2,184,176,180,145,146,147,148,  8,115,116,120,236,237,143,144,  0, 53,167, 55,215,232,
17,97,98,99, 0, 0, 0,  0, 12, 14, 15,192,168,188,153,154,155,156,  0,121,122,123,211,212,201,  0,246,166, 51, 54,  0,  0);
// 247 - doesn't work in game, replaced with random road


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
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2))  //Wineyard
//,((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,1,0))  //Wall
);

//Does house output needs to be ordered by Player or it keeps on producing by itself
HousePlaceOrders:array[1..HOUSE_COUNT] of boolean = (
false,false,true ,false,false,false,false,false,false,false,
true ,false,false,false,false,false,false,false,false,true ,
true ,false,false,true ,false,false,false,false,false{,false});

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
(rt_Wine,       rt_None,       rt_None,       rt_None){, //Wineyard
(rt_None,       rt_None,       rt_None,       rt_None)}  //Wall
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
(rt_None,       rt_None,       rt_None,       rt_None){, //Wineyard
(rt_None,       rt_None,       rt_None,       rt_None)}  //Wall
);


{Houses UI}
const
  GUIBuildIcons:array[1..HOUSE_COUNT]of word = (
  301, 302, 303, 304, 305,
  306, 307, 308, 309, 310,
  311, 312, 313, 314, 315,
  316, 317, 318, 319, 320,
  321, 322, 323, 324, 325,
  326, 327, 328, 329{, 338});

  GUIHouseOrder:array[1..HOUSE_COUNT]of THouseType = (
    ht_School, ht_Inn, ht_Quary, ht_Woodcutters, ht_Sawmill,
    ht_Farm, ht_Mill, ht_Bakery, ht_Swine, ht_Butchers,
    ht_Wineyard, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_WeaponWorkshop,
    ht_Tannery, ht_ArmorWorkshop, ht_Stables, ht_IronMine, ht_IronSmithy,
    ht_WeaponSmithy, ht_ArmorSmithy, ht_Barracks, ht_Store, ht_WatchTower,
    ht_FisherHut, ht_TownHall, ht_SiegeWorkshop, ht_None{, ht_Wall});

{Terrain}
type
  TFieldType = (ft_None=0, ft_Road, ft_Corn,
                ft_InitWine, //Reset terrain rotation and set grapes ground
                ft_Wine,
                ft_Wall); //This is used only for querying
  THouseStage = (hs_None, hs_Plan, hs_Fence, hs_Built);

  TTileOverlay = (to_None=0, to_Dig1, to_Dig2, to_Dig3, to_Dig4, to_Road, to_Wall );

  TMarkup = (
        mu_None=0,              //Nothing
        mu_RoadPlan,            //Road/Corn/Wine ropes
        mu_FieldPlan,           //Road/Corn/Wine ropes
        mu_WinePlan,            //Road/Corn/Wine ropes
        mu_WallPlan,            //Wall plan, how does it looks?
        mu_HousePlan,           //Rope outline of house area, walkable
        mu_HouseFenceCanWalk,   //Wooden fence outline of house area, undigged and walkable
        mu_HouseFenceNoWalk,    //Wooden fence outline of house area, digged and non-walkable

        mu_House,               //Actual house, which is not rendered and is used in here to siplify whole thing
        mu_UnderConstruction    //Underconstruction tile, house area being flattened and roadworks
        );

  TBorderType = (bt_None=0, bt_Field=1, bt_Wine=2, bt_HousePlan=3, bt_HouseBuilding=4);

const
  //Chopable tree, Chopdown animation,
  //Grow1, Grow2, Grow3, Grow4, Chop, Remainder
  ChopableTrees:array[1..13,1..6]of byte = (
  //For grass
  (  88,  89,  90,  90,  91,  37), //duplicate
  (  97,  98,  99, 100, 101,  41),
  ( 102, 103, 104, 105, 106,  45),
  ( 107, 108, 109, 110, 111,  41),
  ( 112, 113, 114, 114, 115,  25), //duplicate
  ( 116, 117, 118, 119, 120,  25),
  //For grass and yellow
  (  92,  93,  94,  95,  96,  49),
  //For yellow soil only
  ( 121, 122, 123, 124, 125,  64),
  //For dirt (pine trees)
  ( 149, 150, 151, 151, 152,  29), //duplicate
  ( 153, 154, 155, 155, 156,  29), //duplicate
  ( 157, 158, 159, 160, 161,  33),
  ( 162, 163, 164, 165, 166,  33),
  ( 167, 168, 169, 170, 171,  33));

//Ages at which tree grows up / changes sprite
TreeAge1 = 250;  //I did measured only corn, and it was ~195sec
TreeAge2 = 500;
TreeAgeFull = 800; //Tree is old enough to be chopped

CORN_AGE1 = 10; //When Corn reaches this age - seeds appear, 0 means unplanted field
CORN_AGE2 = 220;   //Numbers are measured from KaM, ~195sec
CORN_AGE3 = 430;
CORN_AGEFULL = 640; //Corn ready to be harvested

WINE_AGE1 = 10; //When Wine reaches this age - seeds appear, 0 means unplanted field
WINE_AGE2 = 220;   //Numbers are measured from KaM, ~195sec
WINE_AGE3 = 430;
WINE_AGEFULL = 640; //Corn ready to be harvested


//   1      //Select road tile and rotation
//  8*2     //depending on surrounding tiles
//   4      //Bitfield
RoadsConnectivity:array [0..15,1..2]of byte = (
(248,0),(248,0),(248,1),(250,3),
(248,0),(248,0),(250,0),(252,0),
(248,1),(250,2),(248,1),(252,3),
(250,1),(252,2),(252,1),(254,0));

{DeliverList}
type
  TDemandType = (dt_Once, dt_Always); //Is this one-time demand like usual, or constant (storehouse, barracks)

//The frame shown when a unit is standing still in ua_Walk. Same for all units!
const UnitStillFrames: array[TKMDirection] of byte = (0,3,2,2,1,6,7,6,6);

type
  TPlayerID = (play_none=0, play_1=1, play_2=2, play_3=3, play_4=4, play_5=5, play_6=6, play_7=7, play_8=8, play_animals=9);


  TSoundFX = (
    sfx_CornCut=1,
    sfx_Dig,
    sfx_Pave,
    sfx_MineStone,
    sfx_CornSow,
    sfx_ChopTree,
    sfx_housebuild,
    sfx_placemarker,
    sfx_Click,
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
    sfx_Melee45, //Killed
    sfx_Melee46, //Killed
    sfx_Melee47, //House hit
    sfx_Melee48, //injured?
    sfx_Melee49, //injured?
    sfx_Melee50,
    sfx_Melee51, //Sword-sword, hit blocked?
    sfx_Melee52, //Sword-sword, hit blocked?
    sfx_Melee53, //Sword-sword, hit blocked?
    sfx_Melee54, //Sword-sword, hit blocked?
    sfx_Melee55, //Killed?
    sfx_Melee56, //Barbarian Killed?
    sfx_Melee57, //House hit?
    sfx_BowDraw,
    sfx_ArrowHit,
    sfx_CrossbowShoot,  //60
    sfx_CrossbowDraw,
    sfx_BowShoot,       //62
    sfx_BlacksmithBang,
    sfx_BlacksmithFire,
    sfx_CarpenterHammer, //65
    sfx_Horse1,sfx_Horse2,sfx_Horse3,sfx_Horse4,
    sfx_RockThrow,
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

const SSoundFX:array[TSoundFX] of string = (
    'sfx_CornCut',
    'sfx_Dig',
    'sfx_Pave',
    'sfx_MineStone',
    'sfx_CornSow',
    'sfx_ChopTree',
    'sfx_housebuild',
    'sfx_placemarker',
    'sfx_Click',
    'sfx_mill',
    'sfx_saw',
    'sfx_wineStep',
    'sfx_wineDrain',
    'sfx_metallurgists',
    'sfx_coalDown',
    'sfx_Pig1','sfx_Pig2','sfx_Pig3','sfx_Pig4',
    'sfx_Mine',
    'sfx_unknown21', //Pig?
    'sfx_Leather',
    'sfx_BakerSlap',
    'sfx_CoalMineThud',
    'sfx_ButcherCut',
    'sfx_SausageString',
    'sfx_QuarryClink',
    'sfx_TreeDown',
    'sfx_WoodcutterDig',
    'sfx_CantPlace',
    'sfx_MessageOpen',
    'sfx_MessageClose',
    'sfx_MessageNotice',
    'sfx_Melee34', //Killed by shot?
    'sfx_Melee35', //Killed by stone?
    'sfx_Melee36', //Killed by stone?
    'sfx_Melee37', //Smacked?
    'sfx_Melee38',
    'sfx_Melee39',
    'sfx_Melee40',
    'sfx_Melee41', //House hit
    'sfx_Melee42', //Clung?
    'sfx_Melee43',
    'sfx_Melee44', //Killed
    'sfx_Melee45', //Killed
    'sfx_Melee46', //Killed
    'sfx_Melee47', //House hit
    'sfx_Melee48', //injured?
    'sfx_Melee49', //injured?
    'sfx_Melee50',
    'sfx_Melee51', //Sword-sword', hit blocked?
    'sfx_Melee52', //Sword-sword', hit blocked?
    'sfx_Melee53', //Sword-sword', hit blocked?
    'sfx_Melee54', //Sword-sword', hit blocked?
    'sfx_Melee55', //Killed?
    'sfx_Melee56', //Barbarian Killed?
    'sfx_Melee57', //House hit?
    'sfx_BowDraw',
    'sfx_ArrowHit',
    'sfx_CrossbowShoot',  //60
    'sfx_CrossbowDraw',
    'sfx_BowShoot',       //62
    'sfx_BlacksmithBang',
    'sfx_BlacksmithFire',
    'sfx_CarpenterHammer', //65
    'sfx_Horse1','sfx_Horse2','sfx_Horse3','sfx_Horse4',
    'sfx_RockThrow',
    'sfx_HouseDestroy',
    'sfx_SchoolDing',
    //Below are TPR sounds ...
    'sfx_SlingerShoot',
    'sfx_BalistaShoot',
    'sfx_CatapultShoot',
    'sfx_unknown76',
    'sfx_CatapultReload',
    'sfx_SiegeBuildingSmash');


//Sounds to play on different warrior orders
type TSoundToPlay = (sp_Select, sp_Eat, sp_RotLeft, sp_RotRight, sp_Split, sp_Join, sp_Halt, sp_Move, sp_Attack,
                     sp_Formation, sp_Death, sp_BattleCry, sp_StormAttack);


type
  TAIAttackType = (                  //0 is an old repeating TSK attack that does not support new TPR features, so we always replace it with 1
                   aat_Once=1,       //Attack will occur once (after the set time has passed and if they have enough troops
                   aat_Repeating=2); //Attack will happen multiple times, (after delay time) whenever the AI has enough troops

  TAIAttackTarget = (att_ClosestUnit=0, //Closest enemy unit (untested as to whether this is relative to army or start position)
                     att_ClosestBuildingFromArmy=1, //Closest building from the group(s) lauching the attack
                     att_ClosestBuildingFromStartPos=2, //Closest building from the AI's start position
                     att_CustomPosition=3); //Custom point defined with CustomPosition

  TAIAttack = record
    AttackType: TAIAttackType; //Once or repeating
    Delay: cardinal; //The attack will not occur before this time has passed
    TotalMen: integer; //Number of idle (i.e. back line) warriors required in the AI army before the attack will launch
    GroupAmounts: array[TGroupType] of byte; //How many squads of each group type will be taken
    TakeAll: boolean; //Used instead of GroupAmounts, chooses groups randomly taking at most TotalMen warriors
    Target: TAIAttackTarget;
    Range: integer; //Will only occur when target is within this tile range (not properly tested yet)
    CustomPosition: TKMPoint; //Used when Target = att_CustomPosition
  end;

  //@Krom: Sketch of the goal and message displaying system used in KaM (from scripting point of view anyway)
  //       Please let me know your thoughts. This is very similar to that used in KaM and is quite flexable/expandable. (we can add more parameters/conditions as well as existing KaM ones, possibly using a new script command)
  //       Somethings are probably named unclearly, please give me suggestions or change them. Goals are the one part
  //       of scripting that seems to confuse everyone at first, mainly because of the TGoalStatus. In 99% of cases gs_True and gt_Defeat
  //       go together, because the if the defeat conditions is NOT true you lose, not the other way around. I guess it should be called a
  //       "survival" conditions rather than defeat.
  //       I put some examples below to give you an idea of how it works. Remember this is basically a copy of the goal scripting system in KaM,
  //       not something I designed. It can change, this is just easiest to implement from script compatability point of view.
  //       Talk to me about it on Skype/ICQ sometime :)

  TGoalType = (glt_None=0,  //Means: It is not required for victory or defeat (e.g. simply display a message)
               glt_Victory, //Means: "The following condition must be true for you to win"
               glt_Survive);//Means: "The following condition must be true or else you lose"
  //Conditions are the same numbers as in KaM script
  TGoalCondition = (//gc_Unknown0=0,      //Not used/unknown
                    gc_BuildTutorial=1,   //Must build a tannery (and other buildings from tutorial?) for it to be true. In KaM tutorial messages will be dispalyed if this is a goal
                    gc_Time=2,            //A certain time must pass
                    gc_Buildings=3,       //Storehouse, school, barracks
                    gc_Troops=4,          //All troops
                    //gc_Unknown5=5,        //Not used/unknown
                    gc_MilitaryAssets=6,  //All Troops, Coal mine, Weapons Workshop, Tannery, Armory workshop, Stables, Iron mine, Iron smithy, Weapons smithy, Armory smithy, Barracks, Town hall and Vehicles Workshop
                    gc_SerfsAndSchools=7, //Serfs (possibly all citizens?) and schoolhouses
                    gc_EconomyBuildings=8 //School, Inn and Storehouse
                    //We can come up with our own
                    );

  TGoalStatus = (gs_True=0, gs_False=1); //Weird that it's inverted, but KaM uses it that way

const
  GoalConditionStr: array [TGoalCondition] of string =
  ({'Unknown',} 'Build Tannery', 'Time', 'StoreSchoolBarracks', 'Troops', 'Unknown',
   'Military assets', 'Serfs&Schools', 'School Inn Store');

  GoalStatusStr: array [TGoalStatus] of string =
  ('True', 'False');

type
  TPlayerGoal = record
    GoalType: TGoalType; //Victory, survive, neither
    GoalCondition: TGoalCondition; //Buildings, troops, time passing
    GoalStatus: TGoalStatus; //Must this condition be true or false (same as alive or dead) for victory/surival to occour?
    GoalTime: cardinal; //Only used with ga_Time. Amount of time (in game ticks) that must pass before this goal is complete
    MessageToShow: integer; //Message to be shown when the goal is completed
    MessageHasShown: boolean; //Whether we have shown this message yet
    Player: TPlayerID; //Player whose buildings or troops must be destroyed
  end;
  //Because the goal system is hard to understand, here are some examples:
  {Destroy troops of player 2 in order to win
  Script command: !ADD_GOAL 4 1 0 2
  GoalType=glt_Victory
  GoalCondition=gc_Troops
  GoalStatus=gs_False         //Troops must be dead, non-existant. i.e. the condition that player 2 has troops must be FALSE.
  Player=play_2
  }

  {Save (protect) troops of player 1 or else you will lose the game
  Script command: !ADD_LOST_GOAL 4 0 0 1
  GoalType=glt_Survive
  GoalCondition=gc_Troops
  GoalStatus=gs_True         //Troops must be alive. i.e. the condition that player 1 has troops must be TRUE otherwise you lose.
  Player=play_1
  }

  {Display message 500 after 10 minutes (no goal, just message)
  Script command: !ADD_GOAL 2 0 500 600
  GoalType=glt_None
  GoalCondition=gc_Time
  GoalStatus=gs_True      //Time must have passed
  GoalTime=600
  MessageToShow=500
  }

  {Display message 510 upon buildings of player 4 being destroyed
  Script command: !ADD_GOAL 3 1 510 4 (in this case the script command would also require the buildings to be destroyed for a victory condition, as mentioned bellow)
  GoalType=glt_None            //If this was set to victory or survive not only would the message be displayed, but it would also be a condition for winning/losing
  GoalCondition=gc_Buildings
  GoalStatus=gs_False         //Buildings must be destroyed
  MessageToShow=500
  }


//Pixel positions (waypoints) for sliding around other units. Uses a lookup to save on-the-fly calculations.
//Follows a sort of a bell curve (normal distribution) shape for realistic acceleration/deceleration.
//I tweaked it by hand to look similar to KaM.
//1st row for straight, 2nd for diagonal sliding
const
  SlideLookup: array[1..2, 0..round(CELL_SIZE_PX*1.41)] of byte = (
    (0,0,0,0,0,0,1,1,2,2,3,3,4,5,6,7,7,8,8,9,9,9,9,8,8,7,7,6,5,4,3,3,2,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,6,6,6,5,5,5,4,4,4,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0,0,0));


const
  RESOLUTION_COUNT = 12;
  SupportedResolutions: array[1..RESOLUTION_COUNT,1..2] of word=(
  (1024,768),
  (1152,864),
  (1280,800),
  (1280,960),
  (1280,1024),
  (1366,768),
  (1440,900),
  (1600,900),
  (1600,1200),
  (1680,1050),
  (1920,1080),
  (1920,1200)
  );

const
  MAPSIZES_COUNT = 10;
  MapSize: array[1..MAPSIZES_COUNT] of word=( 32, 48, 64, 80, 96, 112, 128, 144, 160, 176 );

var
  //Indexes are the same as above. Contains the highest refresh rate for each resolution. If 0 then not supported.
  SupportedRefreshRates: array[1..RESOLUTION_COUNT] of word;

  //Players colors (as they appear in KaM when the color is not specified in the script, copied from pallete values)
  //@Krom: These new IDs should be used as defaults when loading a script because some missions may not include a color ID and rely on
  //       the defaults. The first 5 are good but I dislike that the last 3 are fairly similar, so maybe for the map editor we should have
  //       different defaults? Give me your thoughts.
  //@Lewin: I guess dark-blue and orange-brown will be noticably different from existing colors
  //        tbh I don't like default colors that much, they are quite dull (greyish).
  //Default IDs from KaM:
  {229, //Red
  36,  //Cyan
  106, //Green
  20,  //Magenta
  233, //Yellow
  213, //Grey
  3,   //Black
  3,   //Black
  255  //White}
  DefaultTeamColors:array[1..MAX_PLAYERS+1]of cardinal = (
  $FF0707FF, //Red
  $FFE3BB5B, //Cyan
  $FF27A700, //Green
  $FFFF67FF, //Magenta
  $FF07FFFF, //Yellow
  $FF577B7B, //Grey
  $FF000000, //Black
  $FF000000, //Black
  $FFFFFFFF //White, used for highlighting army flag on selection, team_animals
  );


  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  ExeDir:string;

  GameCursor: record
    Float:TKMPointF;    //Precise cursor position in map coords
    Cell:TKMPoint;      //Cursor position cell
    SState:TShiftState; //Thats actually used to see if Left or Right mouse button is pressed
    Mode:TCursorMode;   //Modes used in game (building, unit, road, etc..)
    Tag1:byte;          //Tag to know building type, unit type, brush size
    Tag2:byte;          //Additional tag for MapEd (brush shape)
  end;

  RXData:array [1..8]of record
    Title:string;
    Qty:integer;
    Flag:array of byte;
    Size:array of record X,Y:word; end;
    Pivot:array of record x,y:integer; end;
    Data:array of array of byte;
    RGBA:array of array of cardinal; //Expanded image
    Mask:array of array of cardinal; //Mask for team colors
    HasMask:array of boolean; //Mask for team colors
    NeedTeamColors:boolean;
  end;

  GFXData: array [1..8] of array of record
    TexID,AltID: GLUint; //AltID used for team colors
    u1,v1,u2,v2: single;
    PxWidth,PxHeight:word;
  end;

  FontData:array[TKMFont]of record
    Title:TKMFont;
    TexID:GLUint;
    Unk1,WordSpacing,CharSpacing,Unk3:smallint; //BaseCharHeight?, Unknown, CharSpacingX, LineOffset?
    Pal:array[0..255]of byte;
    Letters:array[0..255]of record
      Width,Height:word;
      Add1,Add2,YOffset,Add4:word; //Add1-4 always 0
      Data:array of byte;
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
    Foot1:array[1..12]of shortint; //Sound indices
    Foot2:array[1..12]of smallint; //vs sprite ID
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
    HitPoints,Attack,AttackHorseBonus,x4,Defence,Speed,x7,Sight:smallint;
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

  //Trees and other terrain elements properties
  MapElemQty:integer=254; //Default qty
  ActualMapElemQty:integer; //Usable qty read from RX file
  ActualMapElem:array[1..254]of integer; //pointers to usable MapElem's
  OriginalMapElem:array[1..256]of integer; //pointers of usable MapElem's back to map objects. (reverse lookup to one above) 256 is no object.
  MapElem:array[1..512]of packed record
    Step:array[1..30]of smallint;           //60
    Count:word;                             //62
    MoveX,MoveY:integer;                    //70
    CuttableTree:longbool;                  //This tree can be cut by a woodcutter
    DiagonalBlocked:longbool;               //Can't walk diagonally accross this object (mainly trees)
    AllBlocked:longbool;                    //All passibility blocked. Can't walk, build, swim, etc.
    WineOrCorn:longbool;                    //Draw multiple (4 or 2) sprites per object (corn or grapes)
    CanGrow:longbool;                       //This object can grow (i.e. change to another object)
    DontPlantNear:longbool;                 //This object can't be planted within one tile of
    Stump:shortint;                         //95 Tree stump
    CanBeRemoved:longbool;                  //99 //Can be removed in favor of building house
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
  TileMMColor:array[1..256]of record R,G,B:byte; end;


implementation


end.
