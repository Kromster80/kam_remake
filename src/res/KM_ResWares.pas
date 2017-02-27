unit KM_ResWares;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils;

  //Collection of types and arrays for Wares usage

type
  TWareType = (
    wt_None,
    wt_Trunk,   wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
    wt_Coal,    wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
    wt_Bread,   wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
    wt_Skin,    wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
    wt_Axe,     wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
    wt_Arbalet, wt_Horse,   wt_Fish,
    wt_All,     wt_Warfare, wt_Food //Special ware types
  );

  TKMWare = class
  private
    fType: TWareType;
    fMarketPrice: Single;
    function GetGUIIcon: Word;
    function GetTextID: Integer;
    function GetTitle: UnicodeString;
    function GetGUIColor: Cardinal;
  public
    constructor Create(aType: TWareType);
    function IsValid: Boolean;
    property GUIColor: Cardinal read GetGUIColor;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: Single read fMarketPrice;
    property Title: UnicodeString read GetTitle;
    property TextID: Integer read GetTextID;
  end;

  TKMResWares = class
  private
    fList: array [TWareType] of TKMWare;
    procedure CalculateCostsTable;
    function GetWare(aIndex: TWareType): TKMWare;
  public
    constructor Create;
    destructor Destroy; override;
    property Wares[aIndex: TWareType]: TKMWare read GetWare; default;
    procedure ExportCostsTable(const aFilename: string);
  end;


const
  WARE_MIN = wt_Trunk;
  WARE_MAX = wt_Fish;
  WARFARE_MIN = wt_Shield;
  WEAPON_MIN = wt_Shield;
  WEAPON_MAX = wt_Arbalet;
  WARFARE_MAX = wt_Horse;

  WARFARE_IRON = [wt_MetalShield, wt_MetalArmor, wt_Sword, wt_Hallebard, wt_Arbalet];

  MARKET_TRADEOFF_FACTOR = 2.2; //X resources buys 1 resource of equal value

  WareTypeToIndex: array [TWareType] of byte = (0, //rt_None
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27,
    0, 0, 0); //rt_All, rt_Warfare, rt_Food

  RES_COUNT = 28;
  WareIndexToType: array [0..RES_COUNT-1] of TWareType = (
    wt_Trunk, wt_Stone, wt_Wood, wt_IronOre, wt_GoldOre,
    wt_Coal, wt_Steel, wt_Gold, wt_Wine, wt_Corn,
    wt_Bread, wt_Flour, wt_Leather, wt_Sausages, wt_Pig,
    wt_Skin, wt_Shield, wt_MetalShield, wt_Armor, wt_MetalArmor,
    wt_Axe, wt_Sword, wt_Pike, wt_Hallebard, wt_Bow,
    wt_Arbalet, wt_Horse, wt_Fish);


  //Aligned to right to use them in GUI costs display as well
  WarfareCosts: array [WEAPON_MIN..WEAPON_MAX, 1..2] of TWareType = (
    (wt_None,   wt_Wood), //rt_Shield
    (wt_Coal,  wt_Steel), //rt_MetalShield
    (wt_None,wt_Leather), //rt_Armor
    (wt_Coal,  wt_Steel), //rt_MetalArmor
    (wt_Wood,   wt_Wood), //rt_Axe
    (wt_Coal,  wt_Steel), //rt_Sword
    (wt_Wood,   wt_Wood), //rt_Pike
    (wt_Coal,  wt_Steel), //rt_Hallebard
    (wt_Wood,   wt_Wood), //rt_Bow
    (wt_Coal,  wt_Steel)  //rt_Arbalet
  );

  //How many of resource gets produced per minute on AVERAGE
  //Measured on a test map RES_COUNT / TIME in minutes
  ProductionRate: array [WARE_MIN..WARE_MAX] of Single = (
     88/120, 414/120, 390/120, 160/120, 160/120,
    155/120, 218/120, 330/120, 120/120, 138/120,
    336/120, 162/120, 324/120, 510/120,  84/180,
     84/180, 180/120, 155/120, 180/120, 155/120,
    200/120, 195/120, 200/120, 195/120, 200/120,
    195/120,  69/120, 122/120);

  //How much time it takes from owner taking a house till stable production
  //1 minute on average for the time it takes to process input into output
  //Some wares need extra time to grow (e.g. Horses) and some
  //depend on environment supply (e.g. Trunks)
  //Trunks 1-15
  //Wine 1-8
  //Corn 1-11
  //Pigs 6
  //Skins 6
  //Horses 6
  ProductionLag: array [WARE_MIN..WARE_MAX] of Byte = (
     6, 1, 1, 1, 1,
     1, 1, 1, 4, 5,
     1, 1, 1, 1, 6,
     6, 1, 1, 1, 1,
     1, 1, 1, 1, 1,
     1, 6, 1);


implementation
uses
  KM_ResTexts;


{ TKMWare }
constructor TKMWare.Create(aType: TWareType);
begin
  inherited Create;

  fType := aType;
end;


function TKMWare.GetGUIColor: Cardinal;
const
  //Resources colors for Results charts
  //Made by naospor from kamclub.ru
  WareColor: array [WARE_MIN..WARE_MAX] of Cardinal = (
    $004080, $BFBFBF, $0080BF, $BF4040, $00FFFF,
    $606060, $BF0000, $00BFFF, $000080, $80FFFF,
    $80BFFF, $FFFFFF, $4040BF, $0000FF, $0040BF,
    $008080, $00BF00, $00FF7F, $FFBF00, $BF0080,
    $FF0040, $00FF40, $FFFF40, $FF0080, $FFFF80,
    $101080, $0080FF, $FFBF00);
begin
  Result := WareColor[fType];
end;


function TKMWare.GetGUIIcon: Word;
begin
  case fType of
    WARE_MIN..WARE_MAX: Result := 351 + WareTypeToIndex[fType];
    wt_All:             Result := 657;
    wt_Warfare:         Result := 658;
    wt_Food:            Result := 659;
  else
    Result := 41; // "Question mark"
  end;
end;


function TKMWare.GetTextID: Integer;
begin
  case fType of
    WARE_MIN..WARE_MAX: Result := TX_RESOURCES_NAMES__27 + WareTypeToIndex[fType];
    wt_All:             Result := TX_RESOURCES_ALL;
    wt_Warfare:         Result := TX_RESOURCES_WARFARE;
    wt_Food:            Result := TX_RESOURCES_FOOD;
  else
    Result := -1;
  end;
end;


function TKMWare.GetTitle: UnicodeString;
begin
  if GetTextID <> -1 then
    Result := gResTexts[GetTextID]
  else
    Result := 'N/A';
end;


function TKMWare.IsValid: Boolean;
begin
  Result := fType in [WARE_MIN..WARE_MAX];
end;


{ TKMResWares }
constructor TKMResWares.Create;
var
  I: TWareType;
begin
  inherited;

  for I := Low(TWareType) to High(TWareType) do
    fList[I] := TKMWare.Create(I);

  // Calcuate the trade costs for marketplace once
  CalculateCostsTable;
end;


destructor TKMResWares.Destroy;
var
  I: TWareType;
begin
  for I := Low(TWareType) to High(TWareType) do
    fList[I].Free;

  inherited;
end;


function TKMResWares.GetWare(aIndex: TWareType): TKMWare;
begin
  Result := fList[aIndex];
end;


// Export costs table for analysis in human-friendly form
procedure TKMResWares.ExportCostsTable(const aFilename: string);
var
  SL: TStringList;
  I: TWareType;
begin
  SL := TStringList.Create;
  try
    for I := WARE_MIN to WARE_MAX do
      SL.Add(fList[I].GetTitle + #9 + #9 + FloatToStr(fList[I].fMarketPrice));

    SL.SaveToFile(aFilename);
  finally
    SL.Free;
  end;
end;


procedure TKMResWares.CalculateCostsTable;
const
  NON_RENEW = 1.25; //Non-renewable resources are more valuable than renewable ones
  TREE_ADDN = 0.15; //Trees require a large area (e.g. compared to corn)
  WINE_ADDN = 0.1; //Wine takes extra wood to build
  ORE_ADDN = 0.2; //You can only build a few iron/gold mines on most maps (compared to coal)
begin
  //Take advantage of the fact that we have both classes in same unit
  //and assign to private field directly
  Wares[wt_Trunk      ].fMarketPrice := (1/ProductionRate[wt_Trunk]) + TREE_ADDN;
  Wares[wt_Stone      ].fMarketPrice := NON_RENEW*(1/ProductionRate[wt_Stone]);
  Wares[wt_Wood       ].fMarketPrice := (1/ProductionRate[wt_Wood]) + (1/2)*Wares[wt_Trunk].MarketPrice;
  Wares[wt_IronOre    ].fMarketPrice := NON_RENEW*(1/ProductionRate[wt_IronOre]) + ORE_ADDN;
  Wares[wt_GoldOre    ].fMarketPrice := NON_RENEW*(1/ProductionRate[wt_GoldOre]) + ORE_ADDN;
  Wares[wt_Coal       ].fMarketPrice := NON_RENEW*(1/ProductionRate[wt_Coal]);
  Wares[wt_Steel      ].fMarketPrice := (1/ProductionRate[wt_Steel]) + Wares[wt_IronOre].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Gold       ].fMarketPrice := (1/ProductionRate[wt_Gold]) + (1/2)*(Wares[wt_GoldOre].MarketPrice + Wares[wt_Coal].MarketPrice);
  Wares[wt_Wine       ].fMarketPrice := (1/ProductionRate[wt_Wine]) + WINE_ADDN;
  Wares[wt_Corn       ].fMarketPrice := (1/ProductionRate[wt_Corn]);
  Wares[wt_Flour      ].fMarketPrice := (1/ProductionRate[wt_Flour]) + Wares[wt_Corn].MarketPrice;
  Wares[wt_Bread      ].fMarketPrice := (1/ProductionRate[wt_Bread]) + (1/2)*Wares[wt_Flour].MarketPrice;
  Wares[wt_Pig        ].fMarketPrice := (1/ProductionRate[wt_Pig]) + (1/2)*4*Wares[wt_Corn].MarketPrice; //1/2 because two products are made simultaneously
  Wares[wt_Skin       ].fMarketPrice := (1/ProductionRate[wt_Skin]) + (1/2)*4*Wares[wt_Corn].MarketPrice; //1/2 because two products are made simultaneously
  Wares[wt_Leather    ].fMarketPrice := (1/ProductionRate[wt_Leather]) + (1/2)*Wares[wt_Skin].MarketPrice;
  Wares[wt_Sausages   ].fMarketPrice := (1/ProductionRate[wt_Sausages]) + (1/3)*Wares[wt_Pig].MarketPrice;
  Wares[wt_Shield     ].fMarketPrice := (1/ProductionRate[wt_Shield]) + Wares[wt_Wood].MarketPrice;
  Wares[wt_MetalShield].fMarketPrice := (1/ProductionRate[wt_MetalShield]) + Wares[wt_Steel].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Armor      ].fMarketPrice := (1/ProductionRate[wt_Armor]) + Wares[wt_Leather].MarketPrice;
  Wares[wt_MetalArmor ].fMarketPrice := (1/ProductionRate[wt_MetalArmor]) + Wares[wt_Steel].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Axe        ].fMarketPrice := (1/ProductionRate[wt_Axe]) + 2*Wares[wt_Wood].MarketPrice;
  Wares[wt_Sword      ].fMarketPrice := (1/ProductionRate[wt_Sword]) + Wares[wt_Steel].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Pike       ].fMarketPrice := (1/ProductionRate[wt_Pike]) + 2*Wares[wt_Wood].MarketPrice;
  Wares[wt_Hallebard  ].fMarketPrice := (1/ProductionRate[wt_Hallebard]) + Wares[wt_Steel].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Bow        ].fMarketPrice := (1/ProductionRate[wt_Bow]) + 2*Wares[wt_Wood].MarketPrice;
  Wares[wt_Arbalet    ].fMarketPrice := (1/ProductionRate[wt_Arbalet]) + Wares[wt_Steel].MarketPrice + Wares[wt_Coal].MarketPrice;
  Wares[wt_Horse      ].fMarketPrice := (1/ProductionRate[wt_Horse]) + 4*Wares[wt_Corn].MarketPrice;
  Wares[wt_Fish       ].fMarketPrice := NON_RENEW*(1/ProductionRate[wt_Fish]);
end;


end.
