unit KM_Defaults;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, dglOpenGL, KM_Points;


//Global const
const
//|===================| <- constant name length
  CELL_SIZE_PX          = 40;           //Single cell size in pixels (width)
  CELL_HEIGHT_DIV       = 33.333;       //Height divider, controlls terrains pseudo-3d look
  TOOLBAR_WIDTH         = 224;          //Toolbar width in game
  //GAME_LOGIC_PACE       = 100;          //Game logic should be updated each 100ms
  TERRAIN_PACE          = 10;           //Terrain gets updated once per ** ticks (10 by default), Warning, it affects tree-corn growth rate

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

  GAME_REVISION         = 'r2400';       //Should be updated for every release (each time save format is changed)
  GAME_VERSION          = '1st Merchants Demo RC1 ' + GAME_REVISION;       //Game version string displayed in menu corner

  FONTS_FOLDER = 'data'+PathDelim+'gfx'+PathDelim+'fonts'+PathDelim;

var
  //These should be TRUE
  MAKE_ANIM_TERRAIN     :boolean=true;  //Should we animate water and swamps
  MAKE_UNIT_SPRITES     :boolean=true;  //Whenever to make Units graphics or not, saves time for GUI debug
  MAKE_HOUSE_SPRITES    :boolean=true;  //Whenever to make Houses graphics or not, saves time for GUI debug
  MAKE_TEAM_COLORS      :boolean=true;  //Whenever to make team colors or not, saves RAM for debug
  DO_UNIT_HUNGER        :boolean=true;  //Wherever units get hungry or not
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
  CHECK_WIN_CONDITIONS  :boolean=true; //Could be disabled in test missions
  CUSTOM_RANDOM         :boolean=true; //Use our custom random number generator or the built in "Random()"
  ENABLE_MP_IN_MENU     :boolean=true; //Keep Multiplayer disabled until it's rigged
  KAM_WATER_DRAW        :boolean=true; //Render underwater sand 
  //Not fully implemented yet
  LOAD_UNIT_RX_FULL     :boolean=false; //Clip UnitsRX to 7885 sprites until we add TPR ballista/catapult support
  FOG_OF_WAR_ENABLE     :boolean=false; //Whenever dynamic fog of war is enabled or not
  AGGRESSIVE_REPLAYS    :boolean=false; //Write a command gic_TempDoNothing every tick in order to find exactly when a replay mismatch occurs

  //These are debug things, should be FALSE
  {User interface options}
  SHOW_DEBUG_CONTROLS   :boolean=false; //Show debug panel / Form1 menu (F11)
  SHOW_CONTROLS_OVERLAY :boolean=false; //Draw colored overlays ontop of controls, usefull for making layout (F6)! always Off here
  SHOW_TEXT_OUTLINES    :boolean=false; //Display text areas outlines
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
  SHOW_NETWORK_DELAY    :boolean=false; //Show the current delay in multiplayer game
  SHOW_ARMYEVALS        :boolean=false; //Show result of enemy armies evaluation
  INI_HITPOINT_RESTORE  :boolean=false; //Use the hitpoint restore rate from the INI file to compare with KaM
  {Gameplay cheats}
  FREE_ROCK_THROWING    :boolean=false; //Throwing a rock from Tower costs nothing. To debug throw algoritm
  REDUCE_SHOOTING_RANGE :boolean=false; //Reduce shooting range for debug
  MULTIPLAYER_CHEATS    :boolean=false; //Multiplayer cheats should be disabled for releases, but are useful for debug
  DEBUG_CHEATS          :boolean=false; //Cheats for debugging (place scout and reveal map) which should be disabled for releases
  {Data output}
  WRITE_DECODED_MISSION :boolean=false; //Save decoded mission as txt file
  WRITE_DELIVERY_LOG    :boolean=false; //Write even more output into log + slows down game noticably
  WRITE_WALKTO_LOG      :boolean=false; //Write even more output into log + slows down game noticably
  WriteResourceInfoToTXT:boolean=false; //Whenever to write txt files with defines data properties on loading
  WriteAllTexturesToBMP :boolean=false; //Whenever to write all generated textures to BMP on loading (extremely time consuming)

  //Statistic
  CtrlPaintCount:word; //How many Controls were painted in last frame

const
  MAX_RES_IN_HOUSE=5;     //Maximum resource items allowed to be in house
  MAX_ORDER=999;          //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc)
  MAX_TEX_RESOLUTION=512; //Maximum texture resolution client can handle (used for packing sprites)
  RX7_SPRITE_COUNT = 27;  //Number of sprites to load for RX7 from the folder \Sprites\

const
  MAX_PLAYERS       = 8;    //Maximum players per map
  SAVEGAME_COUNT    = 10;   //Savegame slots available in game menu
  AUTOSAVE_COUNT    = 3;    //How many autosaves to backup

const //Here we store options that are hidden somewhere in code
  MAX_WARFARE_IN_BARRACKS = 255;          //Maximum number of weapons in the barracks from producers. Not a big problem as they are not from the store.
  MAX_WARFARE_IN_BARRACKS_FROM_STORE = 32;//Maximum number of weapons in the barracks from storehouse. e.g. AI starts with 999 axes, don't spend entire game filling up the barracks.
  GOLD_TO_SCHOOLS_IMPORTANT = true;       //Whenever gold delivery to schools is highly important
  FOOD_TO_INN_IMPORTANT = true;           //Whenever food delivery to inns is highly important
  UNIT_MAX_CONDITION = 45*600;            //*min of life. In KaM it's 45min
  UNIT_MIN_CONDITION = 6*600;             //If unit condition is less it will look for Inn. In KaM it's 6min
  TIME_BETWEEN_MESSAGES = 4*600;          //Time between messages saying house is unoccupied or unit is hungry. In KaM it's 4 minutes
  TIME_ATTACK_WARNINGS = 200;             //Time between audio messages saying you are being attacked
  DISTANCE_FOR_WARNINGS = 30;            //The distance you must be from an event to recieve a warning about it
  TROOPS_FEED_MAX = 0.75;                 //Maximum amount of condition a troop can have to order food (more than this means they won't order food)
  TROOPS_TRAINED_CONDITION = 0.75;        //Condition troops start with when trained
  DEFAULT_HITPOINT_RESTORE = 41;          //1 hitpoint is restored to units every X ticks (using Humbelum's advice)


  //Archer properties
  RANGE_ARBALETMAN_MAX  = 10.99; //KaM: Unit standing 10 tiles from us will be shot, 11 tiles not
  RANGE_BOWMAN_MAX      = 10.99;
  RANGE_SLINGSHOT_MAX   = 10.99;
  RANGE_WATCHTOWER_MAX  = 6.99; //Measured in KaM. Distance from the doorway of tower

  RANGE_ARBALETMAN_MIN  = 4; //KaM: We will shoot a unit standing 4 tiles away, but not one standing 3 tiles away
  RANGE_BOWMAN_MIN      = 4;
  RANGE_SLINGSHOT_MIN      = 4;
  RANGE_WATCHTOWER_MIN  = 0; //In KaM towers have no minimum range, they will shoot any unit less than the range

  LINK_RADIUS = 5; //Radius to search for groups to link to after being trained at the barracks (measured from KaM)

  FIRING_DELAY = 0; //on which frame archer fires his arrow/bolt
  SLINGSHOT_FIRING_DELAY = 12; //on which frame archer fires his arrow/bolt
  AIMING_DELAY_MIN = 4; //minimum time for archer to aim
  AIMING_DELAY_ADD = 8; //random component
  FRIENDLY_FIRE = true; //Whenever archers could kill fellow men with their arrows


type
  TCampaign = (cmp_Nil, cmp_TSK, cmp_TPR, cmp_Custom);

  TPlayerIndex = shortint;
  TPlayerArray = array [0..MAX_PLAYERS-1] of TPlayerIndex; 

const
  PLAYER_NONE = -1; //No player
  PLAYER_ANIMAL = -2; //animals

type
  TRenderMode = (rm2D, rm3D);

{Cursors}
type
  TCursorMode = ( cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Wall, cm_Houses, //Gameplay
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
  CursorOffsetsX:array[1..24] of integer = (0,0,20, 0, 0,-8, 0, 1,1,1,0,-1,-1,-1,0,0,ScrollCursorOffset,0,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0);
  CursorOffsetsY:array[1..24] of integer = (0,9,10,18,20,44, 0,-1,0,1,1, 1, 0,-1,0,0,ScrollCursorOffset,0,ScrollCursorOffset,0,0,ScrollCursorOffset,ScrollCursorOffset,0);

const DirCursorSqrSize  = 33; //Length of square sides
      DirCursorNARadius = 14;  //Radius of centeral part that has no direction

{Controls}
type
  TButtonStyle = (bsMenu, bsGame); //Menu buttons are metal, game buttons are stone
  T3DButtonStateSet = set of (bs_Highlight, bs_Down, bs_Disabled);
  TFlatButtonStateSet = set of (fbs_Highlight, fbs_Selected, fbs_Disabled);

const
  LOCALES_COUNT = 11;
  Locales:array[1..LOCALES_COUNT, 1..3]of shortstring = (
  ('eng', '1252', 'English'),
  ('ger', '1252', 'German'),
  ('pol', '1250', 'Polish'),
  ('svk', '1250', 'Slovak'), //New one
  ('cze', '1250', 'Czech'),  //New one
  ('swe', '1252', 'Swedish'),//New one
  ('fre', '1252', 'French'), //New one
  ('spa', '1252', 'Spanish'),
  ('hun', '1250', 'Hungarian'),
  ('dut', '1252', 'Dutch'),
  ('rus', '1251', 'Russian'));


type TGameResultMsg = ( //Game result
        gr_Win,         //Player has won the game
        gr_Defeat,      //Player was defeated
        gr_Cancel,      //Game was cancelled (unfinished)
        gr_MPCancel,    //Multiplayer game was canceled
        gr_Error,       //Some known error occured
        gr_Disconnect,  //Disconnected from multiplayer game
        gr_Silent,      //Used when loading savegame from running game (show no screens)
        gr_ReplayEnd,   //Replay was cancelled - return to menu without screens
        gr_MapEdEnd);   //Map Editor was closed - return to menu without screens

               
{Palettes}
//There are 9 palette files: Map, Pal0-5, Setup, Setup2, gradient, 2lbm palettes
type
  TKMPal = (
    pal_map,
    pal_0, //pal_1, pal_2, pal_3, pal_4, pal_5, unused since we change brightness with OpenGL overlay
    pal_set,
    pal_set2,
    pal_lin,
    pal2_mapgold,
    pal2_setup);

const
  DEF_PAL = pal_0;            //Default palette to use when generating full-color RGB textures


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
  TKMFont = (fnt_Antiqua,  fnt_Briefing,    fnt_Game,     fnt_Grey,
             fnt_MainB,    fnt_MainMapGold, fnt_Metal,    fnt_Mini,
             fnt_Outline,  fnt_Won);

{Removed fonts that were in KaM:
  Adam (unused)
  Font01 (damaged)
  KMLobby (used for internet lobby in TPR)
  MainA (identical to MainMapGold in all game versions)
  MainA.old (probably never meant to be included in the release anyway)
  Minimum (same as mini but with less characters)
  System (unused)
}

const
   FONT_INTERLINE = 5; //Spacing between lines of text

//Which MapEditor page is being shown. Add more as they are needed.
type TKMMapEdShownPage = (esp_Unknown, esp_Terrain, esp_Buildings, esp_Units);

    TKMissionMode = (mm_Normal, mm_Tactic);

    TAllianceType = (at_Enemy=0, at_Ally=1); //Must match KaM script IDs for now

const
  TKMCursorDirections: array[TKMDirection]of integer = (c_DirN,c_Dir0,c_Dir1,c_Dir2,c_Dir3,c_Dir4,c_Dir5,c_Dir6,c_Dir7);

{Resources}
type
  TResourceType = (
    rt_None,  //Special resource types
    rt_Trunk, rt_Stone, rt_Wood, rt_IronOre, rt_GoldOre,
    rt_Coal, rt_Steel, rt_Gold, rt_Wine, rt_Corn,
    rt_Bread, rt_Flour, rt_Leather, rt_Sausages, rt_Pig,
    rt_Skin, rt_Shield, rt_MetalShield, rt_Armor, rt_MetalArmor,
    rt_Axe, rt_Sword, rt_Pike, rt_Hallebard, rt_Bow,
    rt_Arbalet, rt_Horse, rt_Fish,
    rt_All, rt_Warfare, rt_Food);

const WARE_MIN = rt_Trunk;
      WARE_MAX = rt_Fish;
      WARFARE_MIN = rt_Shield;
      WEAPON_MIN = rt_Shield;
      WEAPON_MAX = rt_Arbalet;
      WARFARE_MAX = rt_Horse;

const //Using shortints instead of bools makes it look much neater in code-view
  CheatStorePattern:array[WARE_MIN..WARE_MAX]of byte = (
  0,0,1,0,0,
  0,1,0,1,0,
  1,0,0,0,1,
  1,0,0,0,1,
  1,1,1,1,1,
  0,0,0);

const {Aligned to right to use them in GUI costs display as well}
  WarfareCosts:array[WEAPON_MIN..WEAPON_MAX,1..2]of TResourceType = (
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
  TUnitType = (ut_None, ut_Any,
    ut_Serf,          ut_Woodcutter,    ut_Miner,         ut_AnimalBreeder,
    ut_Farmer,        ut_Lamberjack,    ut_Baker,         ut_Butcher,
    ut_Fisher,        ut_Worker,        ut_StoneCutter,   ut_Smith,
    ut_Metallurgist,  ut_Recruit,

    ut_Militia,      ut_AxeFighter,   ut_Swordsman,     ut_Bowman,
    ut_Arbaletman,   ut_Pikeman,      ut_Hallebardman,  ut_HorseScout,
    ut_Cavalry,      ut_Barbarian,

    ut_Peasant,      ut_Slingshot,    ut_MetalBarbarian,ut_Horseman,
    //ut_Catapult,   ut_Ballista,

    ut_Wolf,         ut_Fish,         ut_Watersnake,   ut_Seastar,
    ut_Crab,         ut_Waterflower,  ut_Waterleaf,    ut_Duck);

const UNIT_MIN = ut_Serf;
      UNIT_MAX = ut_Duck;
      CITIZEN_MIN = ut_Serf;
      CITIZEN_MAX = ut_Recruit;
      WARRIOR_MIN = ut_Militia;
      WARRIOR_MAX = ut_Horseman;
      WARRIOR_EQUIPABLE_MIN = ut_Militia; //Available from barracks
      WARRIOR_EQUIPABLE_MAX = ut_Barbarian;
      HUMANS_MIN = ut_Serf;
      HUMANS_MAX = ut_Horseman;
      ANIMAL_MIN = ut_Wolf;
      ANIMAL_MAX = ut_Duck;

//Used for AI defence and linking troops
type  TGroupType = (gt_Melee, gt_AntiHorse, gt_Ranged, gt_Mounted);
const KaMGroupType: array[TGroupType] of byte = (0, 1, 2, 3);

type TCheckAxis = (ax_X, ax_Y);

const UnitGroups: array[WARRIOR_MIN..WARRIOR_MAX] of TGroupType = (
    gt_Melee,gt_Melee,gt_Melee, //ut_Militia, ut_AxeFighter, ut_Swordsman
    gt_Ranged,gt_Ranged,        //ut_Bowman, ut_Arbaletman
    gt_AntiHorse,gt_AntiHorse,  //ut_Pikeman, ut_Hallebardman,
    gt_Mounted,gt_Mounted,      //ut_HorseScout, ut_Cavalry,
    gt_Melee,                   //ut_Barbarian
    //TPR Army
    gt_AntiHorse,        //ut_Peasant
    gt_Ranged,           //ut_Slingshot
    gt_Melee,            //ut_MetalBarbarian
    gt_Mounted           //ut_Horseman
    {gt_Ranged,gt_Ranged, //ut_Catapult, ut_Ballista,}
    );

//AI's prefences for training troops
const AITroopTrainOrder: array[TGroupType,1..3] of TUnitType = (
  (ut_Swordsman,    ut_AxeFighter, ut_Militia),
  (ut_Hallebardman, ut_Pikeman,    ut_None),
  (ut_Arbaletman,   ut_Bowman,     ut_None),
  (ut_Cavalry,      ut_HorseScout, ut_None));


const FlagXOffset: array[TGroupType, TKMDirection] of shortint = (
    ( 0, 10, -1,  2,  1, -6,-10,  4, 13),  //gt_Melee
    ( 0,  6,  5,  7, -3,-10, -4, 10,  9),  //gt_AntiHorse
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  //gt_Ranged
    ( 0,  6,  2,  3, -5,-10, -8,  5,  6)); //gt_Mounted

const FlagYOffset: array[TGroupType, TKMDirection] of shortint = (
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  //gt_Melee
    ( 0, 23, 25, 25, 21, 20, 19, 20, 22),  //gt_AntiHorse
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  //gt_Ranged
    ( 0,  4, 16, 16,  4,  5,  2,  3,  4)); //gt_Mounted


type TGoInDirection = (gd_GoOutside=-1, gd_GoInside=1); //Switch to set if unit goes into house or out of it

type //Army_Flag=4962,
  TUnitThought = (th_None, th_Eat, th_Home, th_Build, th_Stone, th_Wood, th_Death, th_Quest);

const //Corresponding indices in units.rx
  ThoughtBounds:array[1..7,1..2] of word = (
  (6250,6257),(6258,6265),(6266,6273),(6274,6281),(6282,6289),(6290,6297),(6298,6305)
  );

type
  TUnitTaskName = ( utn_Unknown, //Uninitialized task to detect bugs
        utn_SelfTrain, utn_Deliver,        utn_BuildRoad,  utn_BuildWine,        utn_BuildField,
        utn_BuildWall, utn_BuildHouseArea, utn_BuildHouse, utn_BuildHouseRepair, utn_GoHome,
        utn_GoEat,     utn_Mining,         utn_Die,        utn_GoOutShowHungry,  utn_AttackHouse,
        utn_ThrowRock);

type
  TUnitActionName = ( uan_Unknown, //Uninitialized action to detect bugs
        uan_Stay, uan_WalkTo, uan_GoInOut, uan_AbandonWalk, uan_Fight, uan_StormAttack);

type
  TUnitActionType = (ua_Walk=120, ua_Work, ua_Spec, ua_Die, ua_Work1,
                     ua_Work2, ua_WorkEnd, ua_Eat, ua_WalkArm, ua_WalkTool,
                     ua_WalkBooty, ua_WalkTool2, ua_WalkBooty2, ua_Unknown);
  TUnitActionTypeSet = set of TUnitActionType;

  //What player has ordered us to do
  TWarriorOrder = (
    wo_None, //No orders
    wo_Walk, //Walk somewhere
    wo_WalkOut, //Walk out of Barracks
    wo_AttackUnit, //Attack someone
    wo_AttackHouse, //Attack house
    wo_Storm //Do Storm attack
  );

  //What we are doing at the moment
  TWarriorState = (
    ws_None, //Warrior is idle
    ws_Walking, //Warrior is in the process of walking by player instruction (could have been ordered to attack too because there is no difference)
    ws_RepositionPause, //Warrior has just finished walking and is pausing breifly before repositioning (i.e. rotating to the final facing direction) Without this pause it looks too quick odd.
    ws_Engage //One or more of our group members are in combat and we are on our way to help them.
  );

const FishCountAct:array[1..5]of TUnitActionType = (ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Work1);

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
  UnitAct:array[TUnitActionType]of string = ('ua_Walk', 'ua_Work', 'ua_Spec', 'ua_Die', 'ua_Work1',
             'ua_Work2', 'ua_WorkEnd', 'ua_Eat', 'ua_WalkArm', 'ua_WalkTool',
             'ua_WalkBooty', 'ua_WalkTool2', 'ua_WalkBooty2', 'ua_Unknown');


type
  TGatheringScript = (
    gs_None,
    gs_WoodCutterCut, gs_WoodCutterPlant,
    gs_FarmerSow, gs_FarmerCorn, gs_FarmerWine,
    gs_FisherCatch,
    gs_StoneCutter,
    gs_CoalMiner, gs_GoldMiner, gs_IronMiner,
    gs_HorseBreeder, gs_SwineBreeder);

{Houses in game}
type
  //I've removed values, enums don't need them by intent
  THouseType = ( ht_None, ht_Any,
    ht_ArmorSmithy,     ht_ArmorWorkshop,   ht_Bakery,        ht_Barracks,      ht_Butchers,
    ht_CoalMine,        ht_Farm,            ht_FisherHut,     ht_GoldMine,      ht_Inn,
    ht_IronMine,        ht_IronSmithy,      ht_Marketplace,   ht_Metallurgists, ht_Mill,
    ht_Quary,           ht_Sawmill,         ht_School,        ht_SiegeWorkshop, ht_Stables,
    ht_Store,           ht_Swine,           ht_Tannery,       ht_TownHall,      ht_WatchTower,
    ht_WeaponSmithy,    ht_WeaponWorkshop,  ht_Wineyard,      ht_Woodcutters    );

  //House has 3 basic states: no owner inside, owner inside, owner working inside
  THouseState = (hst_Empty, hst_Idle, hst_Work);
  //These are house building states
  THouseBuildState = (hbs_Glyph, hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);

  THouseActionType = (
  ha_Work1, ha_Work2, ha_Work3, ha_Work4, ha_Work5, //Start, InProgress, .., .., Finish
  ha_Smoke, ha_FlagShtok, ha_Idle,
  ha_Flag1, ha_Flag2, ha_Flag3,
  ha_Fire1, ha_Fire2, ha_Fire3, ha_Fire4, ha_Fire5, ha_Fire6, ha_Fire7, ha_Fire8);
  THouseActionSet = set of THouseActionType;

const
  HouseAction:array[THouseActionType] of string = (
  'ha_Work1', 'ha_Work2', 'ha_Work3', 'ha_Work4', 'ha_Work5', //Start, InProgress, .., .., Finish
  'ha_Smoke', 'ha_FlagShtok', 'ha_Idle',
  'ha_Flag1', 'ha_Flag2', 'ha_Flag3',
  'ha_Fire1', 'ha_Fire2', 'ha_Fire3', 'ha_Fire4', 'ha_Fire5', 'ha_Fire6', 'ha_Fire7', 'ha_Fire8');


  //Number means ResourceType as it is stored in Barracks, hence it's not rt_Something
  TroopCost:array[ut_Militia..ut_Cavalry,1..4] of TResourceType = (
  (rt_Axe,          rt_None,        rt_None,  rt_None ), //Militia
  (rt_Shield,       rt_Armor,       rt_Axe,   rt_None ), //Axefighter
  (rt_MetalShield,  rt_MetalArmor,  rt_Sword, rt_None ), //Swordfighter
  (rt_Armor,        rt_Bow,         rt_None,  rt_None ), //Bowman
  (rt_MetalArmor,   rt_Arbalet,     rt_None,  rt_None ), //Crossbowman
  (rt_Armor,        rt_Pike,        rt_None,  rt_None ), //Lance Carrier
  (rt_MetalArmor,   rt_Hallebard,   rt_None,  rt_None ), //Pikeman
  (rt_Shield,       rt_Armor,       rt_Axe,   rt_Horse), //Scout
  (rt_MetalShield,  rt_MetalArmor,  rt_Sword, rt_Horse)  //Knight
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


  TKMVertexUsage = (vu_None=0,  //Nobody is on this vertex
                    vu_NWSE,    //Vertex is used NW-SE like this: \
                    vu_NESW);   //Vertex is used NE-SW like this: /

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
const
  UnitStillFrames: array[TKMDirection] of byte = (0,3,2,2,1,6,7,6,6);

  
type
  TSoundFX = (
    sfx_None=0,
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
    sfx_Melee34, //Usage of melee sounds can be found in Docs\Melee sounds in KaM.csv
    sfx_Melee35,
    sfx_Melee36,
    sfx_Melee37,
    sfx_Melee38,
    sfx_Melee39,
    sfx_Melee40,
    sfx_Melee41,
    sfx_Melee42,
    sfx_Melee43,
    sfx_Melee44,
    sfx_Melee45,
    sfx_Melee46,
    sfx_Melee47,
    sfx_Melee48,
    sfx_Melee49,
    sfx_Melee50,
    sfx_Melee51,
    sfx_Melee52,
    sfx_Melee53,
    sfx_Melee54,
    sfx_Melee55,
    sfx_Melee56,
    sfx_Melee57,
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

const MeleeSoundsHit:array[0..14] of TSoundFX = (
    sfx_Melee34,
    sfx_Melee35,
    sfx_Melee36,
    sfx_Melee41,
    sfx_Melee42,
    sfx_Melee44,
    sfx_Melee45,
    sfx_Melee46,
    sfx_Melee47,
    sfx_Melee48,
    sfx_Melee49,
    sfx_Melee50,
    sfx_Melee55,
    sfx_Melee56,
    sfx_Melee57
        );

const MeleeSoundsMiss:array[0..8] of TSoundFX = (
    sfx_Melee37,
    sfx_Melee38,
    sfx_Melee39,
    sfx_Melee40,
    sfx_Melee43,
    sfx_Melee51,
    sfx_Melee52,
    sfx_Melee53,
    sfx_Melee54
        );

const MeleeSoundsHouse:array[0..12] of TSoundFX = (
    sfx_Melee37,
    sfx_Melee38,
    sfx_Melee39,
    sfx_Melee40,
    sfx_Melee41,
    sfx_Melee42,
    sfx_Melee43,
    sfx_Melee47,
    sfx_Melee51,
    sfx_Melee52,
    sfx_Melee53,
    sfx_Melee54,
    sfx_Melee57
        );

const SSoundFX:array[TSoundFX] of string = (
    'sfx_None',
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
type
  TWarriorSpeech = (sp_Select, sp_Eat, sp_RotLeft, sp_RotRight, sp_Split, sp_Join, sp_Halt, sp_Move, sp_Attack,
                  sp_Formation, sp_Death, sp_BattleCry, sp_StormAttack);

  TAttackNotification = (an_Citizens, an_Town, an_Troops);

  TProjectileType = (pt_Arrow, pt_Bolt, pt_SlingRock, pt_TowerRock); {pt_BallistaRock, }

const //Corresponding indices in units.rx //pt_Arrow, pt_Bolt are unused
  ProjectileBounds:array[TProjectileType,1..2] of word = ( (0,0),(0,0),(0,0),(4186,4190) );
  ProjectileLaunchSounds:array[TProjectileType] of TSoundFX = (sfx_BowShoot, sfx_CrossbowShoot, sfx_None, sfx_RockThrow);
  ProjectileHitSounds:   array[TProjectileType] of TSoundFX = (sfx_ArrowHit, sfx_ArrowHit, sfx_ArrowHit, sfx_None);
  ProjectileSpeeds:array[TProjectileType] of single = (0.5, 0.55, 0.5, 0.6);
  ProjectileArcs:array[TProjectileType,1..2] of single = ((1.5, 0.25), (1, 0.2), (1.6, 0.3), (1.25, 0)); //Arc curve and random fraction
  ProjectileJitter:array[TProjectileType] of single = (0.05, 0.04, 0.06, 0.025); //Jitter added according to distance
  ProjectilePredictJitter:array[TProjectileType] of single = (2, 2, 2, 2); //Jitter added according to target's speed (moving target harder to hit)
  ProjectileMissChance:array[TProjectileType] of single = (0.33, 0.33, 0.33, 0.33);

  const STORM_SPEEDUP=1.5;
  
type
  TAIAttackType = (aat_Once,       //Attack will occur once (after the set time has passed and if they have enough troops
                   aat_Repeating); //Attack will happen multiple times, (after delay time) whenever the AI has enough troops

const //KaM uses 0 for repeating attack in TSK (disused and replaced with later by Remake), 1 for once and 2 for repeating in TPR
  RemakeAttackType:array[0..2] of TAIAttackType = (aat_Repeating, aat_Once, aat_Repeating);
  KaMAttackType:array[TAIAttackType] of byte = (1,0);


type //Indexes must match with KaM script values (for now)
  TAIAttackTarget = (att_ClosestUnit=0, //Closest enemy unit (untested as to whether this is relative to army or start position)
                     att_ClosestBuildingFromArmy=1, //Closest building from the group(s) lauching the attack
                     att_ClosestBuildingFromStartPos=2, //Closest building from the AI's start position
                     att_CustomPosition=3); //Custom point defined with CustomPosition

  TAIAttack = record
    AttackType: TAIAttackType; //Once or repeating
    HasOccured: boolean; //Has this attack happened already?
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


//Pixel positions (waypoints) for sliding around other units. Uses a lookup to save on-the-fly calculations.
//Follows a sort of a bell curve (normal distribution) shape for realistic acceleration/deceleration.
//I tweaked it by hand to look similar to KaM.
//1st row for straight, 2nd for diagonal sliding
const
  SlideLookup: array[1..2, 0..Round(CELL_SIZE_PX*1.42)] of byte = ( //1.42 instead of 1.41 because we want to round up just in case (it was causing a crash because Round(40*sqrt(2)) = 57 but Round(40*1.41) = 56)
    (0,0,0,0,0,0,1,1,2,2,3,3,4,5,6,7,7,8,8,9,9,9,9,8,8,7,7,6,5,4,3,3,2,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,6,6,6,5,5,5,4,4,4,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0,0,0,0));


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

var
  //Indexes are the same as above. Contains the highest refresh rate for each resolution. If 0 then not supported.
  SupportedRefreshRates: array[1..RESOLUTION_COUNT] of word;

const
  MAPSIZES_COUNT = 11;
  MapSize: array[1..MAPSIZES_COUNT] of word=( 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192 );


  //Colors available for selection in multiplayer
  MP_COLOR_COUNT = 15;
  MP_TEAM_COLORS: array[1..MP_COLOR_COUNT] of cardinal = (
  $FF0707FF, //Red
  $FF0061FF, //Orange
  $FF07FFFF, //Yellow
  $FF00FF00, //Lime green
  $FF008000, //Dark green
  $FFE3BB5B, //Cyan
  $FFFF0000, //Blue
  $FF800080, //Purple
  $FFFF67FF, //Pink
  $FF000080, //Maroon
  $FF004F7D, //Brown
  $FF00B8A8, //Olive green
  $FFFFFFFF, //White
  $FF808080, //Grey
  $FF000000  //Black
  );

  //Players colors, as they appear in KaM when the color is not specified in the script, copied from pallete values.
  //Using these as the defaults when loading the script means the colors will be the same as KaM when not defined.
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
  DefaultTeamColors:array[0..7]of cardinal = (
  $FF0707FF, //Red
  $FFE3BB5B, //Cyan
  $FF27A700, //Green
  $FFFF67FF, //Magenta
  $FF07FFFF, //Yellow
  $FF577B7B, //Grey
  $FF000000, //Black
  $FF000000  //Black
  );

var
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


implementation


end.
