unit KM_Defaults;
interface

type bmBrushMode = (bm_None,bm_Houses);

  TMouseButton2 = (mb2None, mb2Left, mb2Right);

  TResourceType = (rt_None=0, rt_All=30,
    rt_Trunk     =1  , rt_Stone      =2 , rt_Wood       =3 , rt_IronOre     =4 ,
    rt_GoldOre   =5  , rt_Coal       =6 , rt_Steel      =7 , rt_Gold        =8 ,
    rt_Wine      =9  , rt_Corn       =10, rt_Bread      =11, rt_Flour       =12,
    rt_Leather   =13 , rt_Sousages   =14, rt_Pig        =15, rt_Skin        =16,
    rt_WoodShield=17 , rt_MetalShield=18, rt_Armor      =19, rt_MetalArmor  =20,
    rt_Axe       =21 , rt_Sword      =22, rt_Pike       =23, rt_Hallebard   =24,
    rt_Bow       =25 , rt_Arbalet    =26, rt_Horse      =27, rt_Fish        =28);

  TUnitType = ( ut_None=0,
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

  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;

  TGoInDirection = (gid_In=1, gid_Out=-1);

  THouseType = ( ht_None=0,
    ht_Sawmill=1,        ht_IronSmithy=2, ht_WeaponSmithy=3, ht_CoalMine=4,       ht_IronMine=5,
    ht_GoldMine=6,       ht_FisherHut=7,  ht_Bakery=8,       ht_Farm=9,           ht_Woodcutters=10,
    ht_ArmorSmithy=11,   ht_Store=12,     ht_Stables=13,     ht_School=14,        ht_Quary=15,
    ht_Metallurgists=16, ht_Swine=17,     ht_WatchTower=18,  ht_TownHall=19,      ht_WeaponWorkshop=20,
    ht_ArmorWorkshop=21, ht_Barracks=22,  ht_Mill=23,        ht_SiegeWorkshop=24, ht_Butchers=25,
    ht_Tannery=26,       ht_NA=27,        ht_Inn=28,         ht_Wineyard=29);

  THouseState = ( hst_Plan, hst_Wood, hst_Stone, hst_Empty, hst_Idle, hst_Work );

  THouseActionType = (
  ha_Work1=1, ha_Work2=2, ha_Work3=3, ha_Work4=4, ha_Work5=5, //Start, InProgress, .., .., Finish
  ha_Smoke=6, ha_FlagShtok=7, ha_Idle=8,
  ha_Flag1=9, ha_Flag2=10, ha_Flag3=11,
  ha_Fire1=12, ha_Fire2=13, ha_Fire3=14, ha_Fire4=15, ha_Fire5=16, ha_Fire6=17, ha_Fire7=18, ha_Fire8=19);

  THouseActionSet = set of THouseActionType;

const
  HouseOwnerUnit:array[1..29]of TUnitType = (
    ut_Lamberjack, ut_Metallurgist, ut_Smith, ut_Miner, ut_Miner,
    ut_Miner, ut_Fisher, ut_Baker, ut_Farmer, ut_Woodcutter,
    ut_Smith, ut_None, ut_AnimalBreeder, ut_None, ut_StoneCutter,
    ut_Metallurgist, ut_AnimalBreeder, ut_Recruit, ut_None, ut_Lamberjack,
    ut_Lamberjack, ut_Recruit, ut_Baker, ut_Lamberjack, ut_Butcher,
    ut_Butcher, ut_None, ut_None, ut_Farmer);

  UnitSpeeds:array[1..40]of single =(
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,                //Civilian units
    1,1,1,1,1,1,1,1.5,1.5,1,1,1,1,1.5,0.5,0.5,  //Army units
    1,1,1,1,1,1,1,1,1,1);                       //Animals

  UnitSupportedActions:array[1..14]of TUnitActionTypeSet = (
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkTool2],
    [],
    [],
    [ua_Walk, ua_Work, ua_Die..ua_WalkBooty2],
    [ua_Walk, ua_Die, ua_Eat],
    [ua_Walk, ua_Work, ua_Die, ua_Eat],
    [],
    [],
    [ua_Walk, ua_Work, ua_Die, ua_Eat, ua_Work1, ua_Work2],
    [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkBooty],
    [],
    [],
    []
    );

type
  TGatheringScript = (
    gs_WoodCutterCut=1, gs_WoodCutterPlant=2,
    gs_FarmerSow=3, gs_FarmerCorn=4, gs_FarmerWine=5,
    gs_Fisher=6,
    gs_StoneCutter=7);

const
//Resource, Count, Action1, Action2, Action3, Act2Cycles, HomeIdle()
  UnitMiningPlan:array[1..7,1..7]of byte = (
    (byte(rt_Trunk), 1, byte(ua_WalkBooty), byte(ua_Work) , byte(ua_WalkTool2) ,6, 10), //Chop the tree
    (byte(rt_None) , 0, byte(ua_WalkTool) , byte(ua_Work1), byte(ua_Walk)      ,6, 10), //Plant new tree

    (byte(rt_None) , 0, byte(ua_Walk)     , byte(ua_Work1), byte(ua_Walk)      ,6, 10), //Seed the corn
    (byte(rt_Corn) , 1, byte(ua_WalkTool) , byte(ua_Work) , byte(ua_WalkBooty) ,6, 10), //Gather crops
    (byte(rt_Wine) , 0, byte(ua_WalkTool2), byte(ua_Work2), byte(ua_WalkBooty2),6, 50), //Gather grapes

    (byte(rt_Fish) , 0, byte(ua_Walk)     , byte(ua_Walk) , byte(ua_Walk)      ,6, 10), //Catch fish

    (byte(rt_Stone), 3, byte(ua_Walk)     , byte(ua_Work) , byte(ua_WalkTool)  ,6, 50)  //Cut stone
    );

//Resource1, Count1, Resource2, Count2, Count2, Action1, Action2, Action3, Act2Count
HouseProductionPlan:array[0..3,1..6]of integer = (
(byte(rt_None ), 0, byte(rt_None) ,  0, byte(rt_None) , 0), //Houses with no production
(byte(rt_Corn ), 1, byte(rt_None) ,  0, byte(rt_Flour), 1), //Mill
(byte(rt_Flour), 1, byte(ha_Work2),  8, byte(rt_Bread), 1), //Bakery
(byte(rt_Trunk), 1, byte(ha_Work1),  8, byte(rt_Wood) , 2)  //SawMill
);

HouseProductionPlanID:array[1..29]of byte = (
3,0,0,0,0,0,0,2,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,0,0);

//These are colors of all tiles to use in MiniMap
const TileMMColor:array[1..256]of integer = (
131+149*256+ 25*65536,125+140*256+24*65536,133+150*256+41*65536,138+155*256+20*65536,
109+155*256+213*65536,133+156*256+24*65536,135+159*256+24*65536,214+131*256+47*65536,
117+134*256+ 22*65536,119+136*256+23*65536,132+160*256+215*65536,124+138*256+23*65536,
100+149*256+149*65536,121+130*256+26*65536,121+126*256+35*65536,172+131*256+79*65536,
141+134*256+ 37*65536,146+135*256+37*65536,120+138*256+23*65536,125+142*256+24*65536,
122+112*256+ 60*65536,119+108*256+54*65536,102+154*256+192*65536,102+151*256+150*65536,
133+125*256+ 89*65536,152+139*256+98*65536,151+146*256+33*65536,172+162*256+38*65536,
193+163*256+ 48*65536,208+158*256+56*65536,207+157*256+56*65536,188+159*256+89*65536,
194+167*256+ 97*65536,188+162*256+98*65536,123+118*256+34*65536,110+100*256+41*65536,
107+100*256+ 47*65536,115+105*256+53*65536,117+105*256+54*65536,120+108*256+56*65536,
103+103*256+ 33*65536,103+108*256+34*65536,100+113*256+30*65536,104+118*256+32*65536,
 93+153*256+205*65536,193+192*256+248*65536,177+173*256+227*65536,161+152*256+170*65536,
102+151*256+ 74*65536,161+152*256+169*65536,76+63*256+32*65536,145+137*256+126*65536,
175+169*256+207*65536,114+91*256+41*65536,149+130*256+94*65536,119+110*256+37*65536,
123+130*256+ 28*65536,117+115*256+33*65536,111+103*256+38*65536,124+116*256+38*65536,
152+133*256+ 42*65536,114+99*256+42*65536,108+95*256+41*65536,137+113*256+44*65536,
142+130*256+117*65536,153+142*256+143*65536,137+145*256+39*65536,139+141*256+41*65536,
143+139*256+ 44*65536,146+151*256+55*65536,163+154*256+76*65536,172+157*256+87*65536,
138+148*256+ 39*65536,144+147*256+41*65536,149+147*256+43*65536,157+150*256+45*65536,
160+154*256+ 46*65536,165+157*256+47*65536,172+160*256+49*65536,176+160*256+52*65536,
179+160*256+ 54*65536,184+160*256+57*65536,186+158*256+58*65536,189+157*256+59*65536,
129+138*256+ 39*65536,127+131*256+41*65536,126+125*256+42*65536,122+116*256+46*65536,
118+109*256+ 48*65536,115+105*256+49*65536,128+133*256+40*65536,121+120*256+44*65536,
114+110*256+ 44*65536,149+152*256+43*65536,164+156*256+49*65536,177+161*256+53*65536,
131+127*256+ 43*65536,135+130*256+44*65536,141+133*256+45*65536,185+160*256+87*65536,
189+160*256+ 76*65536,191+158*256+65*65536,177+161*256+83*65536,175+161*256+68*65536,
171+161*256+ 55*65536,96+121*256+87*65536,97+102*256+62*65536,102+95*256+52*65536,
179+158*256+ 94*65536,167+144*256+81*65536,141+117*256+61*65536,137+122*256+64*65536,
158+138*256+ 75*65536,170+150*256+86*65536,103+149*256+96*65536,103+149*256+113*65536,
174+163*256+103*65536,154+159*256+110*65536,129+154*256+124*65536,102+148*256+125*65536,
118+144*256+ 52*65536,108+143*256+65*65536,102+144*256+75*65536,130+138*256+77*65536,
131+145*256+100*65536,126+142*256+89*65536,112+151*256+133*65536,112+151*256+133*65536,
108+106*256+ 53*65536,101+93*256+65*65536,98+90*256+62*65536,95+88*256+61*65536,
129+122*256+ 87*65536,109+110*256+49*65536,100+93*256+65*65536,100+92*256+63*65536,
104+ 96*256+ 68*65536,123+114*256+81*65536,120+121*256+58*65536,128+140*256+37*65536,
124+129*256+ 47*65536,121+123*256+52*65536,107+132*256+104*65536,110+137*256+109*65536,
135+126*256+106*65536,138+128*256+107*65536,138+127*256+100*65536,145+132*256+97*65536,
134+108*256+ 52*65536,131+107*256+57*65536,139+115*256+68*65536,147+122*256+79*65536,
105+ 95*256+ 47*65536,98+88*256+43*65536,88+80*256+42*65536,79+73*256+43*65536,
128+119*256+105*65536,114+108*256+87*65536,117+109*256+86*65536,128+120*256+103*65536,
136+107*256+ 45*65536,134+106*256+45*65536,147+115*256+51*65536,145+115*256+52*65536,
157+122*256+ 54*65536,36+33*256+25*65536,168+159*256+177*65536,117+98*256+43*65536,
132+131*256+ 33*65536,174+144*256+78*65536,159+140*256+42*65536,156+148*256+155*65536,
133+141*256+ 58*65536,175+154*256+103*65536,160+150*256+63*65536,119+109*256+67*65536,
132+130*256+ 86*65536,146+134*256+104*65536,142+133*256+90*65536,127+118*256+91*65536,
135+148*256+ 41*65536,190+164*256+100*65536,171+161*256+46*65536,114+104*256+52*65536,
132+110*256+ 37*65536,141+111*256+50*65536,136+111*256+39*65536,128+100*256+41*65536,
129+140*256+ 25*65536,185+156*256+89*65536,165+153*256+37*65536,109+95*256+39*65536,
102+154*256+137*65536,97+146*256+127*65536,77+129*256+111*65536,113+114*256+67*65536,
 98+159*256+130*65536,131+114*256+67*65536,159+161*256+128*65536,106+108*256+81*65536,
108+167*256+140*65536,106+101*256+85*65536,132+126*256+91*65536,181+181*256+237*65536,
187+186*256+247*65536,191+191*256+252*65536,138+130*256+95*65536,132+118*256+74*65536,
101+153*256+136*65536,103+154*256+137*65536,113+170*256+144*65536,113+170*256+144*65536,
173+168*256+213*65536,176+172*256+221*65536,148+119*256+67*65536,129+112*256+60*65536,
181+215*256+202*65536,206+224*256+213*65536,235+251*256+249*65536,242+252*256+250*65536,
168+162*256+193*65536,168+135*256+76*65536,152+125*256+76*65536,140+125*256+80*65536,
180+206*256+187*65536,216+224*256+210*65536,218+230*256+220*65536,216+233*256+224*65536,
187+203*256+184*65536,219+229*256+219*65536,146+187*256+168*65536,30+28*256+23*65536,
170+207*256+194*65536,168+207*256+192*65536,122+148*256+122*65536,102+155*256+130*65536,
 99+153*256+132*65536,118+146*256+125*65536,128+144*256+99*65536,126+144*256+103*65536,
103+153*256+136*65536,110+148*256+94*65536,142+153*256+57*65536,165+158*256+42*65536,
 99+150*256+130*65536,22+22*256+22*65536,212+164*256+87*65536,129+116*256+84*65536,
118+122*256+ 93*65536,100+130*256+125*65536,114+126*256+101*65536,106+102*256+59*65536,
121+118*256+ 80*65536,107+102*256+61*65536,124+116*256+72*65536,112+115*256+82*65536
);

HouseName:array[1..29]of string = (
'Sawmill','Iron smithy','Weapon smithy','Coal mine','Iron mine',
'Gold mine','Fisher hut','Bakery','Farm','Woodcutter',
'Armor smithy','Store','Stables','School','Quary',
'Metallurgist','Swine','Watch tower','Town hall','Weapon workshop',
'Armor workshop','Barracks','Mill','Siege workshop','Butchers',
'Tannery','N/A','Inn','Wineyard');

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

HouseOutput:array[1..29,1..4] of TResourceType = (
(rt_Wood,       rt_None,       rt_None,       rt_None), //Sawmill        //1
(rt_None,       rt_None,       rt_None,       rt_None), //Iron smithy    //21
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon smithy  //244
(rt_None,       rt_None,       rt_None,       rt_None), //Coal mine      //134
(rt_None,       rt_None,       rt_None,       rt_None), //Iron mine      //61
(rt_None,       rt_None,       rt_None,       rt_None), //Gold mine      //239
(rt_None,       rt_None,       rt_None,       rt_None), //Fisher hut     //81
(rt_Bread,      rt_None,       rt_None,       rt_None), //Bakery         //101
(rt_Corn,       rt_None,       rt_None,       rt_None), //Farm           //124
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Woodcutter     //142
(rt_None,       rt_None,       rt_None,       rt_None), //Armor smithy   //41
(rt_None,       rt_None,       rt_None,       rt_None), //Store          //138
(rt_None,       rt_None,       rt_None,       rt_None), //Stables        //146
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
(rt_None,       rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_None,       rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_None,       rt_None,       rt_None,       rt_None), //Inn            //363
(rt_None,       rt_None,       rt_None,       rt_None)  //Wineyard       //378
);

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
(rt_None,       rt_None,       rt_None,       rt_None), //Stables        //146
(rt_None,       rt_None,       rt_None,       rt_None), //School         //250
(rt_None,       rt_None,       rt_None,       rt_None), //Quarry         //211
(rt_None,       rt_None,       rt_None,       rt_None), //Metallurgist   //235
(rt_None,       rt_None,       rt_None,       rt_None), //Swine          //368
(rt_None,       rt_None,       rt_None,       rt_None), //Watch tower    //255
(rt_None,       rt_None,       rt_None,       rt_None), //Town hall      //1657
(rt_None,       rt_None,       rt_None,       rt_None), //Weapon workshop//273
(rt_None,       rt_None,       rt_None,       rt_None), //Armor workshop //663
(rt_None,       rt_None,       rt_None,       rt_None), //Barracks       //334
(rt_Corn,       rt_None,       rt_None,       rt_None), //Mill           //358
(rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop //1681
(rt_None,       rt_None,       rt_None,       rt_None), //Butcher        //397
(rt_None,       rt_None,       rt_None,       rt_None), //Tannery        //668
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_None,       rt_None,       rt_None,       rt_None), //Inn            //363
(rt_None,       rt_None,       rt_None,       rt_None)  //Wineyard       //378
);

//   1      //Depending on surrounding tiles
//  8*2
//   4
RoadsConnectivity:array [0..15,1..2]of byte = (
(249,0),(249,0),(249,1),(251,3),
(249,0),(249,0),(251,0),(253,0),
(249,1),(251,2),(249,1),(253,3),
(251,1),(253,2),(253,1),(255,0));

TeamColors:array[1..4,1..4]of byte =
((255,80,00,255),(255,192,0,255),(70,160,220,255),(60,40,20,255));

ZoomLevels:array[1..7]of single = (0.25,0.5,0.75,1,1.25,1.5,2);

implementation

end.
