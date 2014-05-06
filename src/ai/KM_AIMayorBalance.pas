unit KM_AIMayorBalance;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, StrUtils,
  KM_Defaults, KM_CommonClasses,
  KM_ResHouses, KM_ResWares;

type
  //MayorBalance is a cluster of functions that choose optimal houses to build

//Calculate various demands and save intermediate numbers in case we need them
//in determing what exactly to build to satisfy demand the best
//Production is how much of this resource gets made each minute
// - we evaluate each links theoretical production of end resource (f.e. 1 Corn = 3/5 Sausages)
// - in chain production the speed is limited by slowest link
//todo: - resource in reserve adds to each production rate a fraction
//Consumption is how much gets consumed
//Balance = Production - Consumption;

  TKMCoreBalance = record
    StoreBalance, SchoolBalance, InnBalance, BarracksBalance: Single;
    Balance: Single; //Resulting balance
  end;
  TKMMaterialsBalance = record
    WoodcutTheory, SawmillTheory: Single;
    StoneProduction, WoodProduction: Single;
    StoneBalance, WoodBalance: Single;
    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceGold = record
    CoalTheory, GoldOreTheory, GoldTheory: Single;
    Production: Single; //How much do we produce
    Consumption: Single; //How much is used
    Reserve: Single; //
    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceFood = record
    Bread: record
      FarmTheory, MillTheory, BakeryTheory: Single;
    end;
    Sausages: record
      FarmTheory, SwineTheory, ButchersTheory: Single;
    end;
    BreadProduction, SausagesProduction, WineProduction, FishProduction: Single;
    Production: Single; //How much food do we produce
    Consumption: Single; //How much food do we use
    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceWarfare = record
    SteelWeapon: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
    end;
    SteelArmor: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
    end;
    WoodWeapon: record
      TrunkTheory, WoodTheory, WorkshopTheory: Single;
    end;
    LeatherArmor: record
      TrunkTheory, WoodTheory: Single; //Shields
      FarmTheory, SwineTheory, TanneryTheory: Single; //Armor
      WorkshopTheory: Single;
    end;
    Horse: record
      FarmTheory, StablesTheory: Single;
    end;

    AxeDemandPercentage, PikeDemandPercentage, BowDemandPercentage: Single;
    ArmorDemandPercentage, ShieldDemandPercentage: Single;

    Warfare: array [WARFARE_MIN..WARFARE_MAX] of record
      Production, Demand, Balance: Single;
    end;

    Balance: Single; //Resulting balance
  end;

type
  TKMayorBalance = class
  private
    fOwner: THandIndex;

    fAdvice: array of THouseType;

    //The following are recalculated before each use, so they don't need saving
    fCore: TKMCoreBalance;
    fMaterials: TKMMaterialsBalance;
    fGold: TKMWareBalanceGold;
    fFood: TKMWareBalanceFood;
    fWarfare: TKMWareBalanceWarfare;

    //For debugging (doesn't need saving)
    fCoreText: UnicodeString;
    fMaterialsText: UnicodeString;
    fGoldText: UnicodeString;
    fFoodText: UnicodeString;
    fWarfareText: UnicodeString;
    fAdviceText: UnicodeString;

    function WeaponUsed(aWare: TWareType): Boolean;

    procedure AppendCore;
    procedure AppendMaterials;
    procedure AppendGold;
    procedure AppendFood;
    procedure AppendWeaponry;

    procedure Append(aHouse: THouseType; aCount: Byte = 1);
    function HouseCount(aHouse: THouseType): Integer;

    procedure DistributeCorn;
    procedure DistributeCoal;
    procedure DistributeSteel;
    procedure DistributeWood;

    procedure UpdateBalanceGold;
    procedure UpdateBalanceCore;
    procedure UpdateBalanceMaterials;
    procedure UpdateBalanceFood;
    procedure UpdateBalanceLeather;
    procedure UpdateBalanceIron;
    procedure UpdateBalanceWarfare;
  public
    ArmyType: TArmyType;
    GoldNeed: Single; //How much gold the town needs per minute (may change over time)
    StoneNeed: Single; //How much building materials do we need for city development
    WoodNeed: Single; //How much building materials do we need for city development
    constructor Create(aPlayer: THandIndex);

    procedure OwnerUpdate(aPlayer: THandIndex);
    procedure Refresh;
    function Peek: THouseType;
    procedure Take;
    procedure Reject;
    procedure SetArmyDemand(ShieldNeed, ArmorNeed, AxeNeed, PikeNeed, BowNeed, HorseNeed: Single);
    function BalanceText: UnicodeString;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_HandsCollection, KM_Hand, KM_Resource;


{ TKMayorBalance }
constructor TKMayorBalance.Create(aPlayer: THandIndex);
begin
  inherited Create;
  fOwner := aPlayer;

  GoldNeed := 1;
  StoneNeed := 10;
  WoodNeed := 3.5;
end;


//How many houses of certain type we have (assume all wip houses will be finished)
function TKMayorBalance.HouseCount(aHouse: THouseType): Integer;
begin
  Result := gHands[fOwner].Stats.GetHouseTotal(aHouse);
end;


function TKMayorBalance.WeaponUsed(aWare: TWareType): Boolean;
begin
  case ArmyType of
    atLeather:      Result := aWare in [wt_Shield, wt_Armor, wt_Axe, wt_Pike, wt_Bow, wt_Horse];
    atIron:         Result := aWare in [wt_MetalShield, wt_MetalArmor, wt_Sword, wt_Hallebard, wt_Arbalet, wt_Horse];
    atLeatherIron:  Result := True;
    else            Result := False;
  end;
end;


procedure TKMayorBalance.Append(aHouse: THouseType; aCount: Byte = 1);
var
  I: Integer;
begin
  //ArmorWorkshop is needed to produce Shields before Tannery is made
  if (aHouse = ht_ArmorWorkshop)
  and not gHands[fOwner].Stats.GetCanBuild(ht_ArmorWorkshop)
  and (gHands[fOwner].Stats.GetHouseTotal(ht_Tannery) = 0) then
    Append(ht_Tannery);

  //If the same house is asked for independently then don't add it again, since f.e. gold and iron
  //might both ask for a coal mine, when only 1 extra is needed.
  for I := 0 to Length(fAdvice) - 1 do
    if fAdvice[I] = aHouse then
    begin
      Dec(aCount);
      if aCount = 0 then Exit;
    end;

  SetLength(fAdvice, Length(fAdvice) + aCount);
  for I := Length(fAdvice) - aCount to Length(fAdvice) - 1 do
    fAdvice[I] := aHouse;
end;


//These houses are core for town development
procedure TKMayorBalance.AppendCore;
begin
  with fCore do
  case PickMin([0, StoreBalance, SchoolBalance, InnBalance, BarracksBalance]) of
    0: ;
    1: Append(ht_Store);
    2: Append(ht_School);
    3: Append(ht_Inn);
    4: Append(ht_Barracks);
  end;
end;


procedure TKMayorBalance.AppendMaterials;
var
  List: array [0..2] of Single;
begin
  List[0] := fMaterials.StoneBalance;
  List[1] := fMaterials.WoodcutTheory - WoodNeed;
  List[2] := fMaterials.SawmillTheory - WoodNeed;

  repeat
    case PickMin([0, List[0], List[1], List[2]]) of
      0:  Break;
      1:  begin
            Append(ht_Quary);
            List[0] := List[0] + ProductionRate[wt_Stone];
          end;
      2:  begin
            Append(ht_Woodcutters);
            List[1] := List[1] + ProductionRate[wt_Trunk] * 2; //Each trunk brings 2 wood
          end;
      3:  begin
            Append(ht_Sawmill);
            List[2] := List[2] + ProductionRate[wt_Wood];
          end;
    end;


    //Do not build extra houses if we are low on building materials
    if (gHands[fOwner].Stats.GetWareBalance(wt_Stone) < 40)
    and (gHands[fOwner].Stats.GetHouseQty(ht_Quary) = 0)
    and (gHands[fOwner].Stats.GetHouseWip(ht_Quary) > 1) then
      Break;

{  if (fPlayers[fOwner].Stats.GetHouseQty(ht_Quary) = 0)
  and (fPlayers[fOwner].Stats.GetWareBalance(wt_Stone) < 40) then
  while not (Result in [ht_None, ht_School, ht_Quary]) do
  begin
    Take;
    Result := Peek;
  end;}
  
  until False;
end;


//Increase Gold production
procedure TKMayorBalance.AppendGold;
begin
  //If all 3 shares 0 we whould pick in that order Gold > Coal > Metallurgists
  with fGold do
  if Balance < 0 then
  case PickMin([GoldOreTheory, CoalTheory, GoldTheory]) of
    0:  Append(ht_GoldMine);
    1:  Append(ht_CoalMine, 2);
    2:  Append(ht_Metallurgists);
  end;
end;


procedure TKMayorBalance.AppendFood;
begin
  //Pick smallest production and increase it
  //If all 3 shares 0 we whould pick Sausages first to ensure Leather supply
  with fFood do
  if Balance < 0 then
  case PickMin([SausagesProduction, BreadProduction, WineProduction]) of
    0:  with Sausages do
        case PickMin([FarmTheory, SwineTheory, ButchersTheory]) of
          0:  Append(ht_Farm, 2);
          1:  Append(ht_Swine);
          2:  Append(ht_Butchers);
        end;
    1:  with Bread do
        case PickMin([FarmTheory, MillTheory, BakeryTheory]) of
          0:  Append(ht_Farm, 2);
          1:  Append(ht_Mill);
          2:  Append(ht_Bakery);
        end;
    2:  Append(ht_Wineyard);
  end;
end;


procedure TKMayorBalance.AppendWeaponry;
const
  MAX_WEAPON_TYPES = Byte(WARFARE_MAX)-Byte(WARFARE_MIN);
var
  I, TmpWare: TWareType;
  Tmp: Single;
  WeaponsCount, J, K: Integer;
  Weapons: array[0..MAX_WEAPON_TYPES-1] of TWareType;
  WeaponSatisfactions: array[0..MAX_WEAPON_TYPES-1] of Single;
begin
  //List all the required weapons
  J := 0;
  for I := WARFARE_MIN to WARFARE_MAX do
    if WeaponUsed(I) and (fWarfare.Warfare[I].Demand > 0)
    and (fWarfare.Warfare[I].Production < fWarfare.Warfare[I].Demand) then
    begin
      Weapons[J] := I;
      //Calculate weapon production satisfaction rate (0..1)
      WeaponSatisfactions[J] := fWarfare.Warfare[I].Production / fWarfare.Warfare[I].Demand;
      Inc(J);
    end;
  WeaponsCount := J;

  //Sort (since there's not many items bubble sort is okay for now)
  for J := WeaponsCount-1 downto 0 do
    for K := WeaponsCount-1 downto 0 do
      if WeaponSatisfactions[J] > WeaponSatisfactions[K] then
      begin
        Tmp := WeaponSatisfactions[J];
        WeaponSatisfactions[J] := WeaponSatisfactions[K];
        WeaponSatisfactions[K] := Tmp;

        TmpWare := Weapons[J];
        Weapons[J] := Weapons[K];
        Weapons[K] := TmpWare;
      end;

  for J := 0 to WeaponsCount-1 do
  case Weapons[J] of
    wt_MetalShield,
    wt_MetalArmor:  with fWarfare.SteelArmor do
                    case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
                      0: Append(ht_CoalMine);
                      1: Append(ht_IronMine);
                      2: Append(ht_IronSmithy);
                      3: Append(ht_ArmorSmithy);
                    end;
    wt_Sword,
    wt_Hallebard,
    wt_Arbalet:     with fWarfare.SteelWeapon do
                    case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
                      0: Append(ht_CoalMine);
                      1: Append(ht_IronMine);
                      2: Append(ht_IronSmithy);
                      3: Append(ht_WeaponSmithy);
                    end;
    wt_Shield:      with fWarfare.LeatherArmor do
                    case PickMin([TrunkTheory, WoodTheory, WorkshopTheory*fWarfare.ShieldDemandPercentage]) of
                      0: Append(ht_Woodcutters);
                      1: Append(ht_Sawmill);
                      2: Append(ht_ArmorWorkshop);
                    end;
    wt_Armor:       with fWarfare.LeatherArmor do
                    case PickMin([FarmTheory, SwineTheory, TanneryTheory, WorkshopTheory*fWarfare.ArmorDemandPercentage]) of
                      0: Append(ht_Farm);
                      1: Append(ht_Swine);
                      2: Append(ht_Tannery);
                      3: Append(ht_ArmorWorkshop);
                    end;
    wt_Axe,
    wt_Pike,
    wt_Bow:         with fWarfare.WoodWeapon do
                    case PickMin([TrunkTheory, WoodTheory, WorkshopTheory]) of
                      0: Append(ht_Woodcutters);
                      1: Append(ht_Sawmill);
                      2: Append(ht_WeaponWorkshop);
                    end;
    wt_Horse:       with fWarfare.Horse do
                    case PickMin([FarmTheory, StablesTheory]) of
                      0: Append(ht_Farm);
                      1: Append(ht_Stables);
                    end;
  end;
end;


procedure TKMayorBalance.OwnerUpdate(aPlayer: THandIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMayorBalance.DistributeCoal;
var
  CoalProductionRate, CoalConsumptionRate: Single;
  GoldPerMin, SteelPerMin, WeaponsPerMin, ArmorPerMin: Single;
  ExtraCoal, DeficitCoal: Single;
begin
  CoalProductionRate := HouseCount(ht_CoalMine) * ProductionRate[wt_Coal];

  with fWarfare do
  begin
    //Theoretical gold production
    GoldPerMin := Min(HouseCount(ht_GoldMine) * ProductionRate[wt_GoldOre] * 2,
                      HouseCount(ht_Metallurgists) * ProductionRate[wt_Gold]);

    //Theoretical steel production
    SteelPerMin := Min(HouseCount(ht_IronMine) * ProductionRate[wt_IronOre],
                       HouseCount(ht_IronSmithy) * ProductionRate[wt_Steel]);

    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(ht_WeaponSmithy) * ProductionRate[wt_Sword],
                         Warfare[wt_Sword].Demand
                         + Warfare[wt_Hallebard].Demand
                         + Warfare[wt_Arbalet].Demand);

    //Theoretical armor production
    ArmorPerMin := Min(HouseCount(ht_ArmorSmithy) * ProductionRate[wt_MetalArmor],
                       Warfare[wt_MetalShield].Demand + Warfare[wt_MetalArmor].Demand);

    //Current coal consumption
    CoalConsumptionRate := GoldPerMin/2 + SteelPerMin + WeaponsPerMin + ArmorPerMin;
  end;

  if CoalProductionRate >= CoalConsumptionRate then
  begin
    //Let every industry think the extra belongs to it
    ExtraCoal := CoalProductionRate - CoalConsumptionRate;
    fGold.CoalTheory := GoldPerMin + ExtraCoal * 2;
    fWarfare.SteelWeapon.CoalTheory := WeaponsPerMin + ExtraCoal / 2; //Takes 2 coal to make each weapon
    fWarfare.SteelArmor.CoalTheory := ArmorPerMin + ExtraCoal / 2; //Takes 2 coal to make each armor
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    DeficitCoal := CoalConsumptionRate - CoalProductionRate;
    fGold.CoalTheory := Max(0, GoldPerMin - DeficitCoal * 2); //Each coal makes 2 gold
    fWarfare.SteelWeapon.CoalTheory := Max(0, WeaponsPerMin - DeficitCoal);
    fWarfare.SteelArmor.CoalTheory := Max(0, ArmorPerMin - DeficitCoal);
  end;
end;


procedure TKMayorBalance.DistributeSteel;
var
  WeaponsPerMin, ArmorPerMin: Single;

  SteelPerMin, SteelConsumptionRate: Single;
  ExtraSteel, DeficitSteel: Single;

  IronPerMin, IronProduction, IronConsumption: Single;
  ExtraIron, RateIron: Single;
begin
  SteelPerMin := HouseCount(ht_IronSmithy) * ProductionRate[wt_Steel];
  IronPerMin := HouseCount(ht_IronMine) * ProductionRate[wt_IronOre];

  with fWarfare do
  begin
    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(ht_WeaponSmithy) * ProductionRate[wt_Sword],
                      Warfare[wt_Sword].Demand
                      + Warfare[wt_Hallebard].Demand
                      + Warfare[wt_Arbalet].Demand);

    //Theoretical armor production
    ArmorPerMin := Min(HouseCount(ht_ArmorSmithy) * ProductionRate[wt_MetalArmor],
                      Warfare[wt_MetalArmor].Demand
                      + Warfare[wt_MetalShield].Demand);
  end;

  //Current steel consumption
  SteelConsumptionRate := WeaponsPerMin + ArmorPerMin;

  if SteelPerMin >= SteelConsumptionRate then
  begin
    //Let every industry think the extra belongs to it
    ExtraSteel := SteelPerMin - SteelConsumptionRate;
    fWarfare.SteelWeapon.SteelTheory := WeaponsPerMin + ExtraSteel;
    fWarfare.SteelArmor.SteelTheory := ArmorPerMin + ExtraSteel;
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    DeficitSteel := SteelConsumptionRate - SteelPerMin;
    fWarfare.SteelWeapon.SteelTheory := Max(0, WeaponsPerMin - DeficitSteel);
    fWarfare.SteelArmor.SteelTheory := Max(0, ArmorPerMin - DeficitSteel);
  end;

  IronProduction := IronPerMin;
  IronConsumption := SteelConsumptionRate; //Any not being used for steel is excess

  if IronProduction >= IronConsumption then
  begin
    //Let every industry think the extra belongs to it
    ExtraIron := IronProduction - IronConsumption;
    fWarfare.SteelWeapon.IronTheory := WeaponsPerMin + ExtraIron;
    fWarfare.SteelArmor.IronTheory := ArmorPerMin + ExtraIron;
  end
  else
  begin
    //Share proportionaly
    RateIron := IronProduction / IronConsumption;
    fWarfare.SteelWeapon.IronTheory := RateIron * IronPerMin;
    fWarfare.SteelArmor.IronTheory := RateIron * IronPerMin;
  end;
end;


procedure TKMayorBalance.DistributeWood;
const
  WEAP_COST = 2;
  WT = 2; //Wood from Trunk ratio
var
  TrunkPerMin, WoodPerMin: Single;
  WeaponsPerMin, ShieldsPerMin: Single;
  WoodConsumption: Single;
  TrunkProduction, TrunkConsumption: Single;
  ExtraWood, DeficitWood: Single;
  ExtraTrunk, DeficitTrunk: Single;
begin
  //Theoretical limit on Wood production
  TrunkPerMin := HouseCount(ht_Woodcutters) * ProductionRate[wt_Trunk];
  WoodPerMin := HouseCount(ht_Sawmill) * ProductionRate[wt_Wood];

  with fWarfare do
  begin
    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(ht_WeaponWorkshop) * ProductionRate[wt_Axe],
                      Warfare[wt_Axe].Demand
                      + Warfare[wt_Pike].Demand
                      + Warfare[wt_Bow].Demand);

    //Min from available production (shields are only part of workshops orders) and demand
    ShieldsPerMin := Min(HouseCount(ht_ArmorWorkshop) * ShieldDemandPercentage * ProductionRate[wt_Shield],
                         Warfare[wt_Shield].Demand);

    //Current wood consumption
    WoodConsumption := WoodNeed + ShieldsPerMin + WeaponsPerMin * WEAP_COST;

    //Wood shares
    if WoodPerMin >= WoodConsumption then
    begin
      //Let every industry think the extra belongs to it
      ExtraWood := WoodPerMin - WoodConsumption;
      WoodWeapon.WoodTheory := WeaponsPerMin + ExtraWood / WEAP_COST;
      LeatherArmor.WoodTheory := ShieldsPerMin + ExtraWood;
    end
    else
    begin
      //Sharing proportionaly doesn't work since closer houses get more.
      //Let every industry think the deficit belongs to it
      if (WoodConsumption > 0) and (WoodPerMin > WoodNeed) then
      begin
        DeficitWood := WoodConsumption - WoodPerMin;
        WoodWeapon.WoodTheory := Max(0, WeaponsPerMin - DeficitWood / WEAP_COST);
        LeatherArmor.WoodTheory := Max(0, ShieldsPerMin - DeficitWood);
      end
      else
      begin
        WoodWeapon.WoodTheory := 0;
        LeatherArmor.WoodTheory := 0;
      end;
    end;

    TrunkProduction := TrunkPerMin;
    TrunkConsumption := WoodPerMin / WT;

    //Trunk shares
    if TrunkProduction >= TrunkConsumption then
    begin
      //Let every industry think the extra belongs to it
      ExtraTrunk := TrunkProduction - TrunkConsumption;
      WoodWeapon.TrunkTheory := WeaponsPerMin + ExtraTrunk * WT / WEAP_COST;
      LeatherArmor.TrunkTheory := ShieldsPerMin + ExtraTrunk * WT;
    end
    else
    begin
      //Sharing proportionaly doesn't work since closer houses get more.
      //Let every industry think the deficit belongs to it
      if TrunkConsumption <> 0 then
      begin
        DeficitTrunk := TrunkConsumption - TrunkProduction;
        WoodWeapon.TrunkTheory := Max(0, WeaponsPerMin - DeficitTrunk * WT / WEAP_COST);
        LeatherArmor.TrunkTheory := Max(0, ShieldsPerMin - DeficitTrunk * WT);
      end
      else
      begin
        WoodWeapon.TrunkTheory := 0;
        LeatherArmor.TrunkTheory := 0;
      end;
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceGold;
begin
  with fGold do
  begin
    //How much gol in theory we could get
    //CoalTheory - coal calculated above
    GoldOreTheory := HouseCount(ht_GoldMine) * ProductionRate[wt_GoldOre] * 2; //*2 since every Ore becomes 2 Gold
    GoldTheory := HouseCount(ht_Metallurgists) * ProductionRate[wt_Gold];

    //Actual production is minimum of the above
    Production := Min(CoalTheory, GoldOreTheory, GoldTheory);

    //How much Gold do we need
    Consumption := GoldNeed + Byte(HouseCount(ht_Barracks) > 0) * gHands[fOwner].AI.Setup.WarriorsPerMinute(ArmyType);

    //How much reserve do we have
    Reserve := gHands[fOwner].Stats.GetWareBalance(wt_Gold) / Consumption;

    Balance := Production - Consumption + Max(Reserve - 30, 0);

    fGoldText := Format('%.2f Gold (%.2f - %.2f + %.2f)', [Balance, Production, Consumption, Reserve]);
  end;
end;


procedure TKMayorBalance.UpdateBalanceLeather;
begin
  with fWarfare do
  begin
    with WoodWeapon do
    begin
      //Trunk
      //Wood
      //All 3 produced at the same speed
      WorkshopTheory := HouseCount(ht_WeaponWorkshop) * ProductionRate[wt_Pike];

      Warfare[wt_Axe].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * AxeDemandPercentage;
      Warfare[wt_Pike].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * PikeDemandPercentage;
      Warfare[wt_Bow].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * BowDemandPercentage;
    end;

    with LeatherArmor do
    begin
      //Trunk
      //Wood
      //FarmTheory calculated above
      SwineTheory := HouseCount(ht_Swine) * ProductionRate[wt_Skin] * 2;
      TanneryTheory := HouseCount(ht_Tannery) * ProductionRate[wt_Leather];
      WorkshopTheory := HouseCount(ht_ArmorWorkshop) * ProductionRate[wt_Armor];

      Warfare[wt_Shield].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory * ShieldDemandPercentage);
      Warfare[wt_Armor].Production := Min(Min(FarmTheory, SwineTheory), Min(TanneryTheory, WorkshopTheory * ArmorDemandPercentage));
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceIron;
begin
  with fWarfare do
  begin
    //Weapon
    //Calculate how much Weapon each link could possibly produce
    with SteelWeapon do
    begin
      //Coal/steel/iron calculated above
      SmithyTheory := HouseCount(ht_WeaponSmithy) * ProductionRate[wt_Hallebard];

      //All 3 weapons are the same for now
      Warfare[wt_Sword].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * AxeDemandPercentage;
      Warfare[wt_Hallebard].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * PikeDemandPercentage;
      Warfare[wt_Arbalet].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * BowDemandPercentage;
    end;

    //Armor
    //Calculate how many Armor each link could possibly produce
    with SteelArmor do
    begin
      //Coal/steel/iron calculated above
      SmithyTheory := HouseCount(ht_ArmorSmithy) * ProductionRate[wt_MetalArmor];

      Warfare[wt_MetalShield].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * ShieldDemandPercentage;
      Warfare[wt_MetalArmor].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * ArmorDemandPercentage;
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceWarfare;
var
  I: TWareType;
  S: UnicodeString;
begin
  UpdateBalanceLeather;
  UpdateBalanceIron;

  with fWarfare do
  begin
    //Horse.FarmTheory calculated above
    Horse.StablesTheory := HouseCount(ht_Stables) * ProductionRate[wt_Horse];
    Warfare[wt_Horse].Production := Min(Horse.FarmTheory, Horse.StablesTheory);

    for I := WARFARE_MIN to WARFARE_MAX do
      Warfare[I].Balance := Warfare[I].Production - Warfare[I].Demand;

    //Set Weaponry balance to the most required warfare kind
    Balance := MaxSingle;
    for I := WARFARE_MIN to WARFARE_MAX do
    if WeaponUsed(I) and (Warfare[I].Balance < Balance) then
      Balance := Warfare[I].Balance;

    S := Format('%.2f Weaponry: |', [Balance]);
    with fWarfare.LeatherArmor do
    begin
      S := S + Format('WoodShields: T%.1f : W%.1f : W%.1f|', [TrunkTheory, WoodTheory, WorkshopTheory*ShieldDemandPercentage]);
      S := S + Format('LeatherArm: F%.1f : S%.1f : T%.1f : W%.1f|', [FarmTheory, SwineTheory, TanneryTheory, WorkshopTheory*ArmorDemandPercentage]);
    end;

    with fWarfare.WoodWeapon do
      S := S + Format('WoodWeap: T%.1f W%.1f W%.1f|', [TrunkTheory, WoodTheory, WorkshopTheory]);

    with fWarfare.SteelArmor do
      S := S + Format('SteelArmor: C%.1f : I%.1f : S%.1f : S%.1f|', [CoalTheory, IronTheory, SteelTheory, SmithyTheory]);

    with fWarfare.SteelWeapon do
      S := S + Format('SteelWeapon: C%.1f : I%.1f : S%.1f : S%.1f|', [CoalTheory, IronTheory, SteelTheory, SmithyTheory]);

    for I := WARFARE_MIN to WARFARE_MAX do
    if WeaponUsed(I) then
      S := S + Format('%s: %.2f - %.2f|', [gResource.Wares[I].Title,
                                                  Warfare[I].Production,
                                                  Warfare[I].Demand]);

    fWarfareText := S;
  end;
end;


procedure TKMayorBalance.UpdateBalanceCore;
var
  P: TKMHand;
begin
  P := gHands[fOwner];

  with fCore do
  begin
    //Balance = Available - Required + Reserve
    StoreBalance    := HouseCount(ht_Store)       - 1; //HouseCount(ht_Any) / 35;
    //1 school can make ~4 units per minute. If we need more warriors than that, build 2
    SchoolBalance   := HouseCount(ht_School)      - 1 - Byte((gHands[fOwner].Stats.GetHouseQty(ht_Barracks) > 0) and (gHands[fOwner].AI.Setup.WarriorsPerMinute(ArmyType) > 4));
    InnBalance      := HouseCount(ht_Inn)         - P.Stats.GetCitizensCount / 80;
    BarracksBalance := HouseCount(ht_Barracks)    - Byte(P.Stats.GetWeaponsProduced > 0);

    Balance := Min([StoreBalance, SchoolBalance, InnBalance, BarracksBalance]);
    fCoreText := Format
      ('%.2f Core: (Store %.2f, School %.2f, Inn %.2f, Barracks %.2f)',
      [Balance, StoreBalance, SchoolBalance, InnBalance, BarracksBalance]);
  end;
end;


procedure TKMayorBalance.UpdateBalanceMaterials;
begin
  with fMaterials do
  begin
    StoneProduction := HouseCount(ht_Quary) * ProductionRate[wt_Stone];

    WoodcutTheory := HouseCount(ht_Woodcutters) * ProductionRate[wt_Trunk] * 2;
    SawmillTheory := HouseCount(ht_Sawmill) * ProductionRate[wt_Wood];
    WoodProduction := Min(WoodcutTheory, SawmillTheory);

    StoneBalance    := StoneProduction - StoneNeed;
    WoodBalance     := WoodProduction - WoodNeed;

    Balance := Min(StoneBalance, WoodBalance);
    fMaterialsText := Format('%.2f Materials: (Stone %.1f-%.1f, Wood (%.1f:%.1f)-%.1f)',
                             [Balance, StoneProduction, StoneNeed, WoodcutTheory, SawmillTheory, WoodNeed]);
  end;
end;


procedure TKMayorBalance.DistributeCorn;
const
  BEAST_COST = 4;
var
  CornProduction, CornConsumption: Single;
  FlourPerMin, PigPerMin, HorsePerMin: Single;
  CornExtra, CornDeficit: Single;
begin
  CornProduction := HouseCount(ht_Farm) * ProductionRate[wt_Corn];

  //With stable production rate we can assume consumption rate that would be required
  FlourPerMin := HouseCount(ht_Mill) * ProductionRate[wt_Flour];
  PigPerMin   := HouseCount(ht_Swine) * ProductionRate[wt_Pig] * BEAST_COST;
  HorsePerMin := HouseCount(ht_Stables) * ProductionRate[wt_Horse] * BEAST_COST;
  CornConsumption := FlourPerMin + PigPerMin + HorsePerMin;

  if CornProduction >= CornConsumption then
  begin
    //Let every industry think the extra belongs to it
    CornExtra := CornProduction - CornConsumption;
    fFood.Bread.FarmTheory := (FlourPerMin + CornExtra) * 2;
    fFood.Sausages.FarmTheory := (PigPerMin + CornExtra) / BEAST_COST * 3;
    fWarfare.LeatherArmor.FarmTheory := (PigPerMin + CornExtra) / BEAST_COST * 2;
    fWarfare.Horse.FarmTheory := (HorsePerMin + CornExtra) / BEAST_COST;
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    CornDeficit := CornConsumption - CornProduction;
    fFood.Bread.FarmTheory := Max(0, (FlourPerMin - CornDeficit) * 2);
    fFood.Sausages.FarmTheory := Max(0, (PigPerMin - CornDeficit) / BEAST_COST * 3);
    fWarfare.LeatherArmor.FarmTheory := Max(0, (PigPerMin - CornDeficit) / BEAST_COST * 2);
    fWarfare.Horse.FarmTheory := Max(0, (HorsePerMin - CornDeficit) / BEAST_COST);
  end;
end;


procedure TKMayorBalance.UpdateBalanceFood;
var
  P: TKMHand;
  UT: TUnitType;
begin
  P := gHands[fOwner];

  with fFood do
  begin
    //Bread
    //Calculate how much bread each link could possibly produce
    //Bread.FarmTheory calculated above
    Bread.MillTheory := HouseCount(ht_Mill) * ProductionRate[wt_Flour] * 2;
    Bread.BakeryTheory := HouseCount(ht_Bakery) * ProductionRate[wt_Bread];
    BreadProduction := Min(Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory);

    //Sausages
    //Calculate how many sausages each link could possibly produce
    //Sausages.FarmTheory calculated above
    Sausages.SwineTheory := HouseCount(ht_Swine) * ProductionRate[wt_Pig] * 3;
    Sausages.ButchersTheory := HouseCount(ht_Butchers) * ProductionRate[wt_Sausages];
    SausagesProduction := Min(Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory);

    //Wine, Fish
    WineProduction := HouseCount(ht_Wineyard) * ProductionRate[wt_Wine];
    FishProduction := HouseCount(ht_FisherHut) * ProductionRate[wt_Fish];

    //Count in "food units per minute"
    Production := BreadProduction * BREAD_RESTORE +
                  SausagesProduction * SAUSAGE_RESTORE +
                  WineProduction *  WINE_RESTORE +
                  FishProduction * FISH_RESTORE;

    Consumption := 0;

    for UT := CITIZEN_MIN to CITIZEN_MAX do
       Consumption := Consumption + P.Stats.GetUnitQty(UT) / 40; //On average unit needs to eat each 40min

    Consumption := Consumption * 1.2; //Otherwise not enough food is made for citizens

    //Warriors eat on average half as much as citizens
    for UT := WARRIOR_MIN to WARRIOR_MAX do
       Consumption := Consumption + P.Stats.GetUnitQty(UT) / 2 / 40; //On average unit needs to eat each 40min

    Balance := Production - Consumption;
    fFoodText := Format('%.2f Food: %.2f - %.2f|', [Balance, Production, Consumption])
               + Format('       Bread: min(F%.2f, M%.2f, B%.2f)|', [Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory])
               + Format('    Sausages: min(F%.2f, S%.2f, B%.2f)|', [Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory])
               + Format('        Wine: W%.2f)|', [WineProduction])
               + Format('        Fish: F%.2f)|', [FishProduction])
               + Format('  Food value: %.2f + %.2f + %.2f + %.2f|', [BreadProduction * BREAD_RESTORE, SausagesProduction * SAUSAGE_RESTORE, WineProduction * WINE_RESTORE, FishProduction * FISH_RESTORE]);
  end;
end;


//Tell Mayor what proportions of army is needed
procedure TKMayorBalance.SetArmyDemand(ShieldNeed, ArmorNeed, AxeNeed, PikeNeed, BowNeed, HorseNeed: Single);
begin
  //Convert army request into how many weapons are needed
  with fWarfare do
  begin
    Warfare[wt_Shield].Demand := ShieldNeed;
    Warfare[wt_MetalShield].Demand := ShieldNeed;
    Warfare[wt_Armor].Demand := ArmorNeed;
    Warfare[wt_MetalArmor].Demand := ArmorNeed;
    Warfare[wt_Axe].Demand := AxeNeed;
    Warfare[wt_Sword].Demand := AxeNeed;
    Warfare[wt_Pike].Demand := PikeNeed;
    Warfare[wt_Hallebard].Demand := PikeNeed;
    Warfare[wt_Bow].Demand := BowNeed;
    Warfare[wt_Arbalet].Demand := BowNeed;
    Warfare[wt_Horse].Demand := HorseNeed;

    ShieldDemandPercentage := ShieldNeed / (ShieldNeed + ArmorNeed);
    ArmorDemandPercentage := ArmorNeed / (ShieldNeed + ArmorNeed);
    AxeDemandPercentage := AxeNeed / (AxeNeed + PikeNeed + BowNeed);
    PikeDemandPercentage := PikeNeed / (AxeNeed + PikeNeed + BowNeed);
    BowDemandPercentage := BowNeed / (AxeNeed + PikeNeed + BowNeed);
  end;
end;


function TKMayorBalance.BalanceText: UnicodeString;
begin
  Result := fCoreText + '|' +
            fMaterialsText + '|' +
            fGoldText + '|' +
            fFoodText + '|' +
            fWarfareText + '|' +
            fAdviceText;
end;


procedure TKMayorBalance.Refresh;
var
  I: Integer;
begin
  SetLength(fAdvice, 0);
  try
    //Refresh balance of each industry
    //Try to express needs in terms of Balance = Production - Demand
    UpdateBalanceCore;
    AppendCore;

    //Don't build anything if we don't have a working School
    if gHands[fOwner].Stats.GetHouseQty(ht_School) = 0 then Exit;

    UpdateBalanceMaterials;
    AppendMaterials;

    //Don't build anything if we are short on materials
    if (fMaterials.StoneProduction < 0.5)
    or (fMaterials.WoodProduction < 0.5) then Exit;

    DistributeCorn;
    DistributeCoal;
    DistributeSteel;
    DistributeWood;

    UpdateBalanceGold;
    AppendGold;

    UpdateBalanceFood;
    AppendFood;

    UpdateBalanceWarfare;
    AppendWeaponry;

  finally
    fAdviceText := 'Advice: ';
    for I := 0 to High(fAdvice) do
      fAdviceText := fAdviceText + gResource.HouseDat[fAdvice[I]].HouseName + IfThen(I < High(fAdvice), ', ', '.');
  end;
end;


//Look at next item in advice queue
function TKMayorBalance.Peek: THouseType;
begin
  //Take element from fAdvice queue
  if Length(fAdvice) > 0 then
    Result := fAdvice[0]
  else
    Result := ht_None;
end;


//Reject the item because it could not be built
procedure TKMayorBalance.Reject;
begin
  Take;

  //Some logic that would not build anything and waste resources on that
  //if we dont have e.g. gold supply
end;


//Advised item was taken and most certainly will be finished
procedure TKMayorBalance.Take;
begin
  if Length(fAdvice) > 1 then
    Move(fAdvice[1], fAdvice[0], Length(fAdvice) - 1);

  //Trim last element
  SetLength(fAdvice, Length(fAdvice) - 1);
end;


procedure TKMayorBalance.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);

  SaveStream.Write(ArmyType, SizeOf(TArmyType));
  SaveStream.Write(GoldNeed);
  SaveStream.Write(StoneNeed);
  SaveStream.Write(WoodNeed);

  //Save because there are demands for weaponry (set by General)
  SaveStream.Write(fWarfare.Warfare, SizeOf(fWarfare.Warfare));

  //These are not saved because they are recalculated before each use
  {LoadStream.Read(fDemandCore, SizeOf(fDemandCore));
  LoadStream.Read(fDemandMaterials, SizeOf(fDemandMaterials));
  LoadStream.Read(fDemandGold, SizeOf(fDemandGold));
  LoadStream.Read(fDemandFood, SizeOf(fDemandFood));
  LoadStream.Read(fDemandWeaponry, SizeOf(fDemandWeaponry));}
end;


procedure TKMayorBalance.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);

  LoadStream.Read(ArmyType, SizeOf(TArmyType));
  LoadStream.Read(GoldNeed);
  LoadStream.Read(StoneNeed);
  LoadStream.Read(WoodNeed);

  //Load demands for weaponry set by General
  LoadStream.Read(fWarfare.Warfare, SizeOf(fWarfare.Warfare));

  //These are not saved because they are recalculated before each use
  {LoadStream.Read(fDemandCore, SizeOf(fDemandCore));
  LoadStream.Read(fDemandMaterials, SizeOf(fDemandMaterials));
  LoadStream.Read(fDemandGold, SizeOf(fDemandGold));
  LoadStream.Read(fDemandFood, SizeOf(fDemandFood));
  LoadStream.Read(fDemandWeaponry, SizeOf(fDemandWeaponry));}
end;


end.
