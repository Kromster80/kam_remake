unit KM_InterfaceDefaults;
{$I KaM_Remake.inc}
interface
uses
   Controls, Classes, Windows, KM_Defaults, KM_Controls;


type
  TKMUserInterface = class
  protected
    fMyControls: TKMMasterControl;

  public
    constructor Create(aScreenX, aScreenY: Word);
    destructor Destroy; override;

    property MyControls: TKMMasterControl read fMyControls;

    procedure KeyDown(Key: Word; Shift: TShiftState); virtual; abstract;
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState); virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure Resize(X,Y: Word); virtual; abstract;
    procedure UpdateState(aTickCount: Cardinal); virtual; abstract;
    procedure Paint; virtual;
  end;


const
  //Toolbar pads
  TB_PAD = 9; //Picked up empirically
  TB_WIDTH = 180;
  PAGE_TITLE_Y = 5; //Page title offset

  //Options sliders
  OPT_SLIDER_MIN = 0;
  OPT_SLIDER_MAX = 20;

  //Shortcuts
  //All shortcuts are in English and are the same for all languages to avoid
  //names collisions and confusion in discussions
  SC_MENU_BUILD = VK_F1;
  SC_MENU_RATIO = VK_F2;
  SC_MENU_STATS = VK_F3;
  SC_MENU_MENU  = VK_F4;

  SC_SELECT_LOW = '0';
  SC_SELECT_HIGH = '9';

  SC_ARMY_HALT = 'H';
  SC_ARMY_SPLIT = 'S';
  SC_ARMY_LINK = 'L';

  SC_DEBUG_REVEALMAP = 'M';
  SC_DEBUG_VICTORY = 'V';
  SC_DEBUG_DEFEAT = 'D';
  SC_DEBUG_ADDSCOUT = 'C'; //Usefull when mouse has no middle-button

  SC_BEACON = 'B';
  SC_PAUSE = 'P';
  SC_SHOW_TEAMS = 'T';

  //
  GUI_HOUSE_COUNT = 27;   //Number of KaM houses to show in GUI
  GUIHouseOrder: array [1..GUI_HOUSE_COUNT] of THouseType = (
    ht_School, ht_Inn, ht_Quary, ht_Woodcutters, ht_Sawmill,
    ht_Farm, ht_Mill, ht_Bakery, ht_Swine, ht_Butchers,
    ht_Wineyard, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_WeaponWorkshop,
    ht_Tannery, ht_ArmorWorkshop, ht_Stables, ht_IronMine, ht_IronSmithy,
    ht_WeaponSmithy, ht_ArmorSmithy, ht_Barracks, ht_Store, ht_WatchTower,
    ht_FisherHut, ht_Marketplace);

  //Template for how resources are shown in Barracks
  BARRACKS_RES_COUNT = 11;
  BarracksResType: array [1..BARRACKS_RES_COUNT] of TResourceType =
    (rt_Shield, rt_MetalShield, rt_Armor, rt_MetalArmor, rt_Axe, rt_Sword,
     rt_Pike, rt_Hallebard, rt_Bow, rt_Arbalet, rt_Horse);

  //Layout of resources in Store
  STORE_RES_COUNT = 28;
  StoreResType: array [1..STORE_RES_COUNT] of TResourceType =
    (rt_Trunk,    rt_Stone,   rt_Wood,        rt_IronOre,   rt_GoldOre,
     rt_Coal,     rt_Steel,   rt_Gold,        rt_Wine,      rt_Corn,
     rt_Bread,    rt_Flour,   rt_Leather,     rt_Sausages,  rt_Pig,
     rt_Skin,     rt_Shield,  rt_MetalShield, rt_Armor,     rt_MetalArmor,
     rt_Axe,      rt_Sword,   rt_Pike,        rt_Hallebard, rt_Bow,
     rt_Arbalet,  rt_Horse,   rt_Fish);

  School_Order: array [0..13] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);

  Barracks_Order: array [0..8] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  //Stats get stacked by UI logic (so that on taller screens they all were
  //in nice pairs, and would stack up only on short screens)
  StatPlan: array [0..12] of record
    HouseType: array [0..3] of THouseType;
    UnitType: array [0..1] of TUnitType;
  end = (
    (HouseType: (ht_Quary, ht_None, ht_None, ht_None); UnitType: (ut_StoneCutter, ut_None)),
    (HouseType: (ht_Woodcutters, ht_None, ht_None, ht_None); UnitType: (ut_Woodcutter, ut_None)),
    (HouseType: (ht_FisherHut, ht_None, ht_None, ht_None); UnitType: (ut_Fisher, ut_None)),
    (HouseType: (ht_Farm, ht_Wineyard, ht_None, ht_None); UnitType: (ut_Farmer, ut_None)),
    (HouseType: (ht_Mill, ht_Bakery, ht_None, ht_None); UnitType: (ut_Baker, ut_None)),
    (HouseType: (ht_Swine, ht_Stables, ht_None, ht_None); UnitType: (ut_AnimalBreeder, ut_None)),
    (HouseType: (ht_Butchers, ht_Tannery, ht_None, ht_None); UnitType: (ut_Butcher, ut_None)),
    (HouseType: (ht_Metallurgists, ht_IronSmithy, ht_None, ht_None); UnitType: (ut_Metallurgist, ut_None)),
    (HouseType: (ht_ArmorSmithy, ht_WeaponSmithy, ht_None, ht_None); UnitType: (ut_Smith, ut_None)),
    (HouseType: (ht_CoalMine, ht_IronMine, ht_GoldMine, ht_None); UnitType: (ut_Miner, ut_None)),
    (HouseType: (ht_Sawmill, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_None); UnitType: (ut_Lamberjack, ut_None)),
    (HouseType: (ht_Barracks, ht_WatchTower, ht_None, ht_None); UnitType: (ut_Recruit, ut_None)),
    (HouseType: (ht_Store, ht_School, ht_Inn, ht_Marketplace); UnitType: (ut_Serf, ut_Worker))
    );

  MapEd_Order: array [0..13] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian,
    ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman);

  MapEd_Icon: array [0..13] of Word = (
    61, 62, 63, 64, 65,
    66, 67, 68, 69, 70,
    79, 80, 81, 82);

  Animal_Order: array [0..7] of TUnitType = (
    ut_Wolf, ut_Fish,        ut_Watersnake, ut_Seastar,
    ut_Crab, ut_Waterflower, ut_Waterleaf,  ut_Duck);

  Animal_Icon: array [0..7] of word = (
    71, 72, 73, 74,
    75, 76, 77, 78);


  //In FPC there are 5 TMouseButtons
  ClickAmount: array [TMouseButton] of Byte = (1, 10, 0 {$IFDEF FPC}, 0, 0{$ENDIF});

  MARKET_RES_HEIGHT = 35;


implementation


{ TKMUserInterface }
constructor TKMUserInterface.Create(aScreenX, aScreenY: Word);
begin
  inherited Create;

  fMyControls := TKMMasterControl.Create;
end;


destructor TKMUserInterface.Destroy;
begin
  fMyControls.Free;
  inherited;
end;


procedure TKMUserInterface.KeyPress(Key: Char);
begin
  fMyControls.KeyPress(Key);
end;


procedure TKMUserInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //It may seem that sometimes Up gets called after mouse moves few more px from last Move event
  fMyControls.MouseMove(X, Y, Shift);
  fMyControls.MouseDown(X, Y, Shift, Button);
end;


procedure TKMUserInterface.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  fMyControls.MouseMove(X, Y, Shift);
end;


procedure TKMUserInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //Sometimes Up gets called after mouse moves few more px from last Move event
  fMyControls.MouseMove(X, Y, Shift);
end;


procedure TKMUserInterface.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer);
begin
  fMyControls.MouseWheel(X, Y, WheelDelta);
end;


procedure TKMUserInterface.Paint;
begin
  fMyControls.Paint;
end;


end.
