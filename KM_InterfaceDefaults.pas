unit KM_InterfaceDefaults;
{$I KaM_Remake.inc}
interface
uses
   KM_Defaults;


const
  GUI_HOUSE_COUNT   = 27;   //Number of KaM houses to show in GUI
  GUIHouseOrder:array[1..GUI_HOUSE_COUNT]of THouseType = (
    ht_School, ht_Inn, ht_Quary, ht_Woodcutters, ht_Sawmill,
    ht_Farm, ht_Mill, ht_Bakery, ht_Swine, ht_Butchers,
    ht_Wineyard, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_WeaponWorkshop,
    ht_Tannery, ht_ArmorWorkshop, ht_Stables, ht_IronMine, ht_IronSmithy,
    ht_WeaponSmithy, ht_ArmorSmithy, ht_Barracks, ht_Store, ht_WatchTower,
    ht_FisherHut, ht_Marketplace);

  BARRACKS_RES_COUNT = 11;
  BarracksResType: array[1..BARRACKS_RES_COUNT] of TResourceType =
    (rt_Shield, rt_MetalShield, rt_Armor, rt_MetalArmor, rt_Axe, rt_Sword,
     rt_Pike, rt_Hallebard, rt_Bow, rt_Arbalet, rt_Horse);

  STORE_RES_COUNT = 28;
  StoreResType: array[1..STORE_RES_COUNT] of TResourceType =
    (rt_Trunk,    rt_Stone,   rt_Wood,        rt_IronOre,   rt_GoldOre,
     rt_Coal,     rt_Steel,   rt_Gold,        rt_Wine,      rt_Corn,
     rt_Bread,    rt_Flour,   rt_Leather,     rt_Sausages,  rt_Pig,
     rt_Skin,     rt_Shield,  rt_MetalShield, rt_Armor,     rt_MetalArmor,
     rt_Axe,      rt_Sword,   rt_Pike,        rt_Hallebard, rt_Bow,
     rt_Arbalet,  rt_Horse,   rt_Fish);

  //Statistics page in game menu
  //0=space, 1=house, 2=unit
  StatCount:array[1..8,1..8]of byte = (
  (1,2,0,1,2,0,1,2),
  (1,1,2,0,1,1,2,0),
  (1,1,2,0,1,1,2,0),
  (1,1,2,0,1,1,2,0),
  (1,1,1,2,0,0,0,0),
  (1,1,1,2,0,0,0,0),
  (1,0,0,0,0,1,1,2),
  (1,1,1,0,0,2,2,0));

  StatHouse:array[1..27] of THouseType = (
  ht_Quary, ht_Woodcutters, ht_FisherHut,
  ht_Farm, ht_Wineyard, ht_Mill, ht_Bakery,
  ht_Swine, ht_Stables, ht_Butchers, ht_Tannery,
  ht_Metallurgists, ht_IronSmithy, ht_ArmorSmithy, ht_WeaponSmithy,
  ht_CoalMine, ht_IronMine, ht_GoldMine,
  ht_Sawmill, ht_WeaponWorkshop, ht_ArmorWorkshop,
  ht_Marketplace, ht_Barracks, ht_WatchTower,
  ht_Store, ht_School, ht_Inn );

  StatUnit:array[1..14] of TUnitType = (
  ut_StoneCutter, ut_Woodcutter, ut_Fisher,
  ut_Farmer, ut_Baker,
  ut_AnimalBreeder, ut_Butcher,
  ut_Metallurgist, ut_Smith,
  ut_Miner,
  ut_Lamberjack,
  ut_Recruit,
  ut_Serf, ut_Worker);

  MapEd_Order:array[0..13] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian,
    ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman);

  MapEd_Icon:array[0..13] of word = (
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    19, 20, 21, 22);

  Animal_Order:array[0..7] of TUnitType = (
    ut_Wolf, ut_Fish,        ut_Watersnake, ut_Seastar,
    ut_Crab, ut_Waterflower, ut_Waterleaf,  ut_Duck);

  Animal_Icon:array[0..7] of word = (
    11, 12, 13, 14,
    15, 16, 17, 18);


implementation


end.
