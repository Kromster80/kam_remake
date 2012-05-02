unit KM_Defaults;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_Points;


//Global const
const
//|===================| <- constant name length
  MAX_MAP_SIZE          = 192;
  CELL_SIZE_PX          = 40;           //Single cell size in pixels (width)
  CELL_HEIGHT_DIV       = 33.333;       //Height divider, controlls terrains pseudo-3d look
  TOOLBAR_WIDTH         = 224;          //Toolbar width in game
  TERRAIN_PACE          = 100;          //Each tile gets updated once per ** ticks (100 by default), Warning, it affects field/tree growth rate
  FOW_PACE              = 10;           //Each tile gets updated once per ** ticks (10 by default)

  FOG_OF_WAR_MIN        = 80;           //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  FOG_OF_WAR_ACT        = 160;          //Until this value FOW is not rendered at all
  FOG_OF_WAR_MAX        = 255;          //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  FOG_OF_WAR_INC        = 128;          //Increment for FOW
  FOG_OF_WAR_DEC        = 12;           //Decrement for FOW

  FPS_LAG               = 16;           //Allowed lag between frames, 1000/FPSLag = max allowed FPS, 1 means unlimited
  FPS_INTERVAL          = 1000;         //Time in ms between FPS measurements, bigger value = more accurate result
  SCROLLSPEED           = 1;            //This is the speed that the viewport will scroll every 100 ms, in cells
  SCROLLFLEX            = 4;            //This is the number of pixels either side of the edge of the screen which will count as scrolling
  MENU_DESIGN_X         = 1024;         //Thats the size menu was designed for. All elements are placed in this size
  MENU_DESIGN_Y         = 768;          //Thats the size menu was designed for. All elements are placed in this size

  MENU_SIZE_MIN_X       = 1024;         //Thats the size menu was designed for. All elements are placed in this size
  MENU_SIZE_MIN_Y       = 576;          //Thats the size menu was designed for. All elements are placed in this size

  GAME_REVISION         = 'r3252';       //Should be updated for every release (each time save format is changed)
  GAME_VERSION          = '4th Multiplayer Demo Release Candidate ' + GAME_REVISION;       //Game version string displayed in menu corner
  NET_PROTOCOL_REVISON  = GAME_REVISION;     //Clients of this version may connect to the dedicated server

  FONTS_FOLDER = 'data'+PathDelim+'gfx'+PathDelim+'fonts'+PathDelim;

  DEL_LOGS_OLDER_THAN   = 14;           //in days

var
  //These should be True
  MAKE_ANIM_TERRAIN     :Boolean = True;  //Should we animate water and swamps
  MAKE_TEAM_COLORS      :Boolean = True;  //Whenever to make team colors or not, saves RAM for debug
  DO_UNIT_HUNGER        :Boolean = True;  //Wherever units get hungry or not
  CHEATS_ENABLED        :Boolean = True;  //Enable cheats in game (add_resource, instant_win, etc)
  FREE_POINTERS         :Boolean = True;  //If True, units/houses will be freed and removed from the list once they are no longer needed
  CAP_MAX_FPS           :Boolean = True;  //Should limit rendering performance to avoid GPU overheating (disable to measure debug performance)
  CRASH_ON_REPLAY       :Boolean = True;  //Crash as soon as replay consistency fails (random numbers mismatch)
  BLOCK_DUPLICATE_APP   :Boolean = True;  //Do not allow to run multiplae games at once (to prevent MP cheating)

  //Implemented
  MOUSEWHEEL_ZOOM_ENABLE:Boolean = True; //Should we allow to zoom in game or not
  DO_UNIT_INTERACTION   :Boolean = True; //Debug for unit interaction
  SMOOTH_SCROLLING      :Boolean = True; //Smooth viewport scrolling
  ENABLE_FIGHTING       :Boolean = True; //Allow fighting
  DO_WEIGHT_ROUTES      :Boolean = True; //Add additional cost to tiles in A* if they are occupied by other units (IsUnit=1)
  CUSTOM_RANDOM         :Boolean = True; //Use our custom random number generator or the built in "Random()"
  KAM_WATER_DRAW        :Boolean = True; //Render underwater sand
  //Not fully implemented yet
  DISPLAY_CHARTS_RESULT :Boolean = True; //Show charts in game resultst screen
  USE_NEW_WALKCONNECT   :Boolean = True;
  FOG_OF_WAR_ENABLE     :Boolean = False; //Whenever dynamic fog of war is enabled or not
  SHOW_DISMISS_BUTTON   :Boolean = False; //The button to order citizens go back to school

  //These are debug things, should be False
  {User interface options}
  SHOW_DEBUG_CONTROLS   :Boolean = False; //Show debug panel / Form1 menu (F11)
  SHOW_CONTROLS_OVERLAY :Boolean = False; //Draw colored overlays ontop of controls, usefull for making layout (F6)! always Off here
  SHOW_TEXT_OUTLINES    :Boolean = False; //Display text areas outlines
  ENABLE_DESIGN_CONTORLS:Boolean = False; //Enable special mode to allow to move/edit controls
  MODE_DESIGN_CONTORLS  :Boolean = False; //Special mode to move/edit controls activated by F7, it must block OnClick events! always Off here
  OVERLAY_RESOLUTIONS   :Boolean = False; //Render constraining frame
  {Gameplay display}
  SKIP_RENDER           :Boolean = False; //Skip all the rendering in favor of faster logic
  SKIP_SOUND            :Boolean = False; //Skip all the sounds in favor of faster logic
  AGGRESSIVE_REPLAYS    :Boolean = False; //Write a command gic_TempDoNothing every tick in order to find exactly when a replay mismatch occurs
  SHOW_TERRAIN_WIRES    :Boolean = False; //Makes terrain height visible
  SHOW_UNIT_ROUTES      :Boolean = False; //Draw unit routes
  SHOW_PROJECTILES      :Boolean = False; //Shows projectiles trajectory
  SHOW_POINTER_DOTS     :Boolean = False; //Show pointer count as small dots below unit/houses
  SHOW_GROUND_LINES     :Boolean = False; //Show a line below all sprites to mark the ground height used in Z-Order
  SHOW_UNIT_MOVEMENT    :Boolean = False; //Draw unit movement overlay (occupied tile), Only if unit interaction enabled
  SHOW_WALK_CONNECT     :Boolean = False; //Show floodfill areas of interconnected areas
  TEST_VIEW_CLIP_INSET  :Boolean = False; //Renders smaller area to see if everything gets clipped well
  SHOW_SPRITES_RECT     :Boolean = False; //Render outline around every sprite
  SHOW_ATTACK_RADIUS    :Boolean = False; //Render towers/archers attack radius
  DISPLAY_SOUNDS        :Boolean = False; //Display sounds on map
  RENDER_3D             :Boolean = False; //Experimental 3D render
  {Stats}
  SHOW_SPRITE_COUNT     :Boolean = False; //display rendered controls/sprites count
  SHOW_POINTER_COUNT    :Boolean = False; //Show debug total count of unit/house pointers being tracked
  SHOW_CMDQUEUE_COUNT   :Boolean = False; //Show how many commands were processed and stored by TGameInputProcess
  SHOW_NETWORK_DELAY    :Boolean = False; //Show the current delay in multiplayer game
  SHOW_ARMYEVALS        :Boolean = False; //Show result of enemy armies evaluation
  INI_HITPOINT_RESTORE  :Boolean = False; //Use the hitpoint restore rate from the INI file to compare with KaM
  SLOW_MAP_SCAN         :Boolean = False; //Scan maps with a pause to emulate uncached file access
  SLOW_SAVE_SCAN        :Boolean = False; //Scan saves with a pause to emulate uncached file access
  {Gameplay cheats}
  UNLOCK_CAMPAIGN_MAPS  :Boolean = False; //Unlock more maps for debug
  FREE_ROCK_THROWING    :Boolean = False; //Throwing a rock from Tower costs nothing. To debug throw algoritm
  REDUCE_SHOOTING_RANGE :Boolean = False; //Reduce shooting range for debug
  MULTIPLAYER_CHEATS    :Boolean = False; //Allow cheats and debug overlays (e.g. CanWalk) in Multiplayer
  DEBUG_CHEATS          :Boolean = False; //Cheats for debug (place scout and reveal map) which can be turned On from menu
  MULTIPLAYER_SPEEDUP   :Boolean = False; //Allow you to use F8 to speed up multiplayer for debugging (only effects local client)
  {Data output}
  WRITE_DECODED_MISSION :Boolean = False; //Save decoded mission as txt file
  WRITE_DELIVERY_LOG    :Boolean = False; //Write even more output into log + slows down game noticably
  WRITE_WALKTO_LOG      :Boolean = False; //Write even more output into log + slows down game noticably
  WRITE_RECONNECT_LOG   :Boolean = True;
  WriteResourceInfoToTXT:Boolean = False; //Whenever to write txt files with defines data properties on loading
  WriteAllTexturesToBMP :Boolean = False; //Whenever to write all generated textures to BMP on loading (extremely time consuming)
  //Statistic
  CtrlPaintCount: Word; //How many Controls were painted in last frame

const
  MAX_RES_IN_HOUSE    = 5;     //Maximum resource items allowed to be in house
  MAX_ORDER           = 999;          //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc)

const
  MAX_PLAYERS       = 8;    //Maximum players per map
  AUTOSAVE_COUNT    = 3;    //How many autosaves to backup

const //Here we store options that are hidden somewhere in code
  GOLD_TO_SCHOOLS_IMPORTANT = True;       //Whenever gold delivery to schools is highly important
  FOOD_TO_INN_IMPORTANT = True;           //Whenever food delivery to inns is highly important
  UNIT_MAX_CONDITION = 45*600;            //*min of life. In KaM it's 45min
  UNIT_MIN_CONDITION = 6*600;             //If unit condition is less it will look for Inn. In KaM it's 6min
  TIME_BETWEEN_MESSAGES = 4*600;          //Time between messages saying house is unoccupied or unit is hungry. In KaM it's 4 minutes
  TIME_ATTACK_WARNINGS = 200;             //Time between audio messages saying you are being attacked
  DISTANCE_FOR_WARNINGS = 30;             //The distance you must be from an event to recieve a warning about it
  TROOPS_FEED_MAX = 0.75;                 //Maximum amount of condition a troop can have to order food (more than this means they won't order food)
  TROOPS_TRAINED_CONDITION = 0.75;        //Condition troops start with when trained
  DEFAULT_HITPOINT_RESTORE = 100;         //1 hitpoint is restored to units every X ticks (using Humbelum's advice)


  //Archer properties
  RANGE_ARBALETMAN_MAX  = 10.99; //KaM: Unit standing 10 tiles from us will be shot, 11 tiles not
  RANGE_BOWMAN_MAX      = 10.99;
  RANGE_SLINGSHOT_MAX   = 10.99;
  RANGE_WATCHTOWER_MAX  = 6.99; //Measured in KaM. Distance from the doorway of tower

  RANGE_ARBALETMAN_MIN  = 4; //KaM: We will shoot a unit standing 4 tiles away, but not one standing 3 tiles away
  RANGE_BOWMAN_MIN      = 4;
  RANGE_SLINGSHOT_MIN   = 4;
  RANGE_WATCHTOWER_MIN  = 0; //In KaM towers have no minimum range, they will shoot any unit less than the range

  LINK_RADIUS = 5; //Radius to search for groups to link to after being trained at the barracks (measured from KaM)

  FIRING_DELAY = 0; //on which frame archer fires his arrow/bolt
  SLINGSHOT_FIRING_DELAY = 12; //on which frame archer fires his arrow/bolt
  AIMING_DELAY_MIN = 8; //minimum time for archer to aim
  AIMING_DELAY_ADD = 8; //random component
  FRIENDLY_FIRE = True; //Whenever archers could kill fellow men with their arrows

  NET_DROP_PLAYER_MIN_WAIT = 30; //Host must wait at least this long before dropping disconnected players
  ANNOUNCE_BUILD_MAP = 30*60*10; //30 minutes
  ANNOUNCE_BATTLE_MAP = 2*60*10; //2 minutes

  CHARTS_SAMPLING_FOR_ECONOMY = 450; //Each 45sec
  CHARTS_SAMPLING_FOR_TACTICS = 50; //Each 5sec, cos average game length is much shorter

type
  TPlayerIndex = shortint;
  TPlayerArray = array [0..MAX_PLAYERS-1] of TPlayerIndex;

const
  PLAYER_NONE = -1; //No player
  PLAYER_ANIMAL = -2; //animals

{Cursors}
type
  TCursorMode = ( cm_None, cm_Erase, cm_Road, cm_Field, cm_Wine, cm_Wall, cm_Houses, //Gameplay
                  cm_Elevate, cm_Equalize, cm_Tiles, cm_Objects, cm_Units); //MapEditor

const
  MAPED_TILES_COLS = 6;
  MAPED_TILES_ROWS = 8;


const
  SETTINGS_FILE = 'KaM_Remake_Settings.ini';

  DirCursorCircleRadius  = 32; //Radius of the direction selector cursor restriction area
  DirCursorNARadius = 15;  //Radius of centeral part that has no direction


type TGameResultMsg = ( //Game result
        gr_Win,         //Player has won the game
        gr_Defeat,      //Player was defeated
        gr_Cancel,      //Game was cancelled (unfinished)
        gr_Error,       //Some known error occured
        gr_Disconnect,  //Disconnected from multiplayer game
        gr_Silent,      //Used when loading savegame from running game (show no screens)
        gr_ReplayEnd,   //Replay was cancelled - return to menu without screens
        gr_MapEdEnd);   //Map Editor was closed - return to menu without screens

               
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


//Which MapEditor page is being shown. Add more as they are needed.
type TKMMapEdShownPage = (esp_Unknown, esp_Terrain, esp_Buildings, esp_Units);

    TKMissionMode = (mm_Normal, mm_Tactic);

    TAllianceType = (at_Enemy=0, at_Ally=1); //Must match KaM script IDs for now

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

const
  WARE_MIN = rt_Trunk;
  WARE_MAX = rt_Fish;
  WARFARE_MIN = rt_Shield;
  WEAPON_MIN = rt_Shield;
  WEAPON_MAX = rt_Arbalet;
  WARFARE_MAX = rt_Horse;

  //Resources colors for Results charts
  //Made by naospor from kamclub.ru
  ResourceColor: array [WARE_MIN..WARE_MAX] of Cardinal = (
    $004080, $BFBFBF, $0080BF, $BF4040, $00FFFF,
    $606060, $BF0000, $00BFFF, $FF40FF, $80FFFF,
    $80BFFF, $FFFFFF, $4040BF, $0000FF, $0040BF,
    $008080, $00BF00, $00FF7F, $FFBF00, $BF0080,
    $FF0040, $00FF40, $FFFF40, $FF0080, $FFFF80,
    $FF00BF, $0080FF, $FFBF00);

const //Using shortints instead of bools makes it look much neater in code-view
  CheatStorePattern: array[WARE_MIN..WARE_MAX]of byte = (
  0,0,1,0,0,
  0,1,0,1,0,
  1,0,0,0,1,
  1,0,0,0,1,
  1,1,1,1,1,
  0,0,0);

const {Aligned to right to use them in GUI costs display as well}
  WarfareCosts: array[WEAPON_MIN..WEAPON_MAX, 1..2]of TResourceType = (
    (rt_None,   rt_Wood), //rt_Shield
    (rt_Coal,  rt_Steel), //rt_MetalShield
    (rt_None,rt_Leather), //rt_Armor
    (rt_Coal,  rt_Steel), //rt_MetalArmor
    (rt_Wood,   rt_Wood), //rt_Axe
    (rt_Coal,  rt_Steel), //rt_Sword
    (rt_Wood,   rt_Wood), //rt_Pike
    (rt_Coal,  rt_Steel), //rt_Hallebard
    (rt_Wood,   rt_Wood), //rt_Bow
    (rt_Coal,  rt_Steel)  //rt_Arbalet
  );

{ Terrain }
type
  TPassability = (CanWalk=1, CanWalkRoad, CanBuild, CanBuildIron, CanBuildGold,
                  CanMakeRoads, CanMakeFields, CanPlantTrees, CanFish, CanCrab,
                  CanWolf, CanElevate, CanWorker, CanFactor);
  TPassabilitySet = set of TPassability;

type
  TWalkConnect = (
    wcWalk, 
    wcRoad,
    wcFish,
    wcWolf,
    wcCrab, //These things are used often but changed rarely
    wcWork  //CanWorker areas
  );

const
  //todo: Replace with GetEnumName
  PassabilityStr: array [TPassability] of string = (
    'CanWalk',      // General passability of tile for any walking units
    'CanWalkRoad',  // Type of passability for Serfs when transporting goods, only roads have it
    'CanBuild',     // Can we build a house on this tile?
    'CanBuildIron', // Special allowance for Iron Mines
    'CanBuildGold', // Special allowance for Gold Mines
    'CanMakeRoads', // Thats less strict than house building, roads Can be placed almost everywhere where units Can walk, except e.g. bridges
    'CanMakeFields',// Thats more strict than roads, cos e.g. on beaches you Can't make fields
    'CanPlantTrees',// If Forester Can plant a tree here, dunno if it's the same as fields
    'CanFish',      // Water tiles where fish Can move around
    'CanCrab',      // Sand tiles where crabs Can move around
    'CanWolf',      // Soil tiles where wolfs Can move around
    'CanElevate',   // Nodes which are forbidden to be elevated by workers (house basements, water, etc..)
    'CanWorker',    // Like CanWalk but allows walking on building sites
    'CanFactor'     // Allows vertex (top left) to be factored as a neighbour in flattening algorithm
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

const
  UnitGroups: array[WARRIOR_MIN..WARRIOR_MAX] of TGroupType = (
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
  AITroopTrainOrder: array[TGroupType,1..3] of TUnitType = (
    (ut_Swordsman,    ut_AxeFighter, ut_Militia),
    (ut_Hallebardman, ut_Pikeman,    ut_None),
    (ut_Arbaletman,   ut_Bowman,     ut_None),
    (ut_Cavalry,      ut_HorseScout, ut_None));

  //Offsets for flags rendering in pixels
  FlagXOffset: array [TGroupType, TKMDirection] of shortint = (
    ( 0, 10, -1,  2,  1, -6,-10,  4, 13),  //gt_Melee
    ( 0,  6,  5,  7, -3,-10, -4, 10,  9),  //gt_AntiHorse
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  //gt_Ranged
    ( 0,  6,  2,  3, -5,-10, -8,  5,  6)); //gt_Mounted

  FlagYOffset: array [TGroupType, TKMDirection] of shortint = (
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  //gt_Melee
    ( 0, 23, 25, 25, 21, 20, 19, 20, 22),  //gt_AntiHorse
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  //gt_Ranged
    ( 0,  4, 16, 16,  4,  5,  2,  3,  4)); //gt_Mounted


type
  TGoInDirection = (gd_GoOutside=-1, gd_GoInside=1); //Switch to set if unit goes into house or out of it

type
  TUnitThought = (th_None, th_Eat, th_Home, th_Build, th_Stone, th_Wood, th_Death, th_Quest);

const //Corresponding indices in units.rx
  ThoughtBounds: array [TUnitThought, 1..2] of Word = (
  (0,0), (6250,6257), (6258,6265), (6266,6273), (6274,6281), (6282,6289), (6290,6297), (6298,6305)
  );

  UNIT_OFF_X = -0.5;
  UNIT_OFF_Y = -0.4;

type
  TUnitTaskName = ( utn_Unknown, //Uninitialized task to detect bugs
        utn_SelfTrain, utn_Deliver,        utn_BuildRoad,  utn_BuildWine,        utn_BuildField,
        utn_BuildWall, utn_BuildHouseArea, utn_BuildHouse, utn_BuildHouseRepair, utn_GoHome,
        utn_GoEat,     utn_Mining,         utn_Die,        utn_GoOutShowHungry,  utn_AttackHouse,
        utn_ThrowRock);

type
  TUnitActionName = (uan_Stay, uan_WalkTo, uan_GoInOut, uan_AbandonWalk, uan_Fight, uan_StormAttack);

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

const
  FishCountAct: array [1..5] of TUnitActionType = (ua_Walk, ua_Work, ua_Spec, ua_Die, ua_Work1);

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
  THouseBuildState = (hbs_NoGlyph, hbs_Wood, hbs_Stone, hbs_Done);

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
  TFieldType = (
    ft_None=0,
    ft_Road,
    ft_Corn,
    ft_InitWine, //Reset rotation and set grapes ground, but without Grapes yet
    ft_Wine,
    ft_Wall
    );

  THouseStage = (
    hsNone,        //Nothing, clear area
    hsFence,       //Wooden fence, partially walkable as workers digg it up
    hsBuilt        //Done
  );

  TTileOverlay = (to_None=0, to_Dig1, to_Dig2, to_Dig3, to_Dig4, to_Road, to_Wall);

  //There are 4 steps in tile blocking scheme:
  // 0. Tile is normally walkable
  // 1. Set the tile as CantBuild
  // 2. Sets the tile as CantWalk to anyone except workers who are performing
  //    the digging task, so they could escape the area.
  //    The Worker will push out any unit on his way.
  //    sidenote: CanElevate is per-vertex property, hence it's not identical to CanWorker
  // 3. Set the tile as fully blocked
  TTileLock = (     // CanBuild CanWalk CanWorker CanElevate House Digged Fenced
        tlNone,     // X        X         X       X          -     -      -
        tlFenced,   // -        X         X       X          X     -      X
        tlDigged,   // -        -         X       X          X     X      X
        tlHouse,    // -        -         -       -          X     X      -
        //Used by workers making roads/fields to prevent you from building over them
        tlRoadWork  // -        X         X       X          -     X      -
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

  //Ages at which trees/fields grow up/change sprite multiplied by TERRAIN_PACE
  TREE_AGE_1 = 25;
  TREE_AGE_2 = 50;
  TREE_AGE_FULL = 80; //Tree is old enough to be chopped

  CORN_AGE_1 = 22;   //Number measured from KaM ~195sec
  CORN_AGE_2 = 43;
  CORN_AGE_FULL = 64; //Corn ready to be cut

  WINE_AGE_1 = 22;   //Number measured from KaM ~195sec
  WINE_AGE_2 = 43;
  WINE_AGE_FULL = 64; //Wine ready to be harvested




//The frame shown when a unit is standing still in ua_Walk. Same for all units!
const
  UnitStillFrames: array [TKMDirection] of byte = (0,3,2,2,1,6,7,6,6);


type
  TProjectileType = (pt_Arrow, pt_Bolt, pt_SlingRock, pt_TowerRock); {pt_BallistaRock, }

const //Corresponding indices in units.rx //pt_Arrow, pt_Bolt are unused
  ProjectileBounds:array[TProjectileType,1..2] of word = ( (0,0),(0,0),(0,0),(4186,4190) );



type
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


//Lowest supported resolution
const
  MIN_RESOLUTION_WIDTH  = 1024;
  MIN_RESOLUTION_HEIGHT = 576;


//Record storing resolution and refresh rate
type
  TScreenRes = record
                 Width, Height, RefRate: SmallInt;
               end;

  TResIndex = record ResID, RefID: Integer; end;


const
  //Colors available for selection in multiplayer
  MP_COLOR_COUNT = 15;
  MP_TEAM_COLORS: array[1..MP_COLOR_COUNT] of cardinal = (
  $FF0707FF, //Red
  $FF0061FF, //Orange
  $FF07FFFF, //Yellow
  $FF00D4A8, //Olive green
  $FF00FF00, //Lime green
  $FF008000, //Dark green
  $FFE3BB5B, //Cyan
  $FFFF0000, //Blue
  $FF800080, //Purple
  $FFFF67FF, //Pink
  $FF000080, //Maroon
  $FF004F7D, //Brown
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
  DefaultTeamColors:array[0..MAX_PLAYERS-1]of cardinal = (
  $FF0707FF, //Red
  $FFE3BB5B, //Cyan
  $FF27A700, //Green
  $FFFF67FF, //Magenta
  $FF07FFFF, //Yellow
  $FF577B7B, //Grey
  $FF000000, //Black
  $FF000000  //Black
  );

  //Intarface Colors used for coloring status messages
  icWhite  = $FFFFFFFF;
  icGreen  = $FF00C000;
  icYellow = $FF07FFFF;
  icOrange = $FF0099FF;
  icRed    = $FF0707FF;


var
  ExeDir:string;

  GameCursor: record
    Float:TKMPointF;    //Precise cursor position in map coords
    Cell:TKMPoint;      //Cursor position cell
    SState:TShiftState; //Thats actually used to see if Left or Right mouse button is pressed
    Mode:TCursorMode;   //Modes used in game (building, unit, road, etc..)
    Tag1:byte;    //Tag to know building type, unit type etc.

    MapEdDir: Byte;
    MapEdShape: (hsCircle, hsSquare);
    MapEdSlope: Byte;
    MapEdSize: Byte;
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
