unit KM_ResourceWares;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Defaults;


type
  TKMWare = class
  private
    fType: TWareType;
    fMarketPrice: Single;
    function GetGUIIcon: Word;
    function GetTitle: AnsiString;
  public
    constructor Create(aType: TWareType);
    function IsValid: Boolean;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: Single read fMarketPrice;
    property Title: AnsiString read GetTitle;
  end;

  TKMWaresList = class
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


implementation
uses KM_TextLibrary;


{ TKMWaresList }
constructor TKMWaresList.Create;
var I: TWareType;
begin
  inherited;

  for I := Low(TWareType) to High(TWareType) do
    fList[I] := TKMWare.Create(I);

  //Calcuate the trade costs for marketplace once
  CalculateCostsTable;
end;


destructor TKMWaresList.Destroy;
var I: TWareType;
begin
  for I := Low(TWareType) to High(TWareType) do
    fList[I].Free;

  inherited;
end;


//Export costs table for analysis in human-friendly form
procedure TKMWaresList.ExportCostsTable(const aFilename: string);
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


procedure TKMWaresList.CalculateCostsTable;
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


function TKMWaresList.GetWare(aIndex: TWareType): TKMWare;
begin
  Result := fList[aIndex];
end;


{ TKMWare }
constructor TKMWare.Create(aType: TWareType);
begin
  inherited Create;

  fType := aType;
end;


function TKMWare.GetGUIIcon: Word;
begin
  if IsValid then
    Result := 351 + WareTypeToIndex[fType]
  else
    Result := 41; //Show "Question mark"
end;


function TKMWare.GetTitle: AnsiString;
begin
  case fType of
    WARE_MIN..WARE_MAX: Result := fTextLibrary[TX_RESOURCES_NAMES__27 + WareTypeToIndex[fType]];
    wt_All: Result := 'All';
    wt_Warfare: Result := 'Warfare';
    wt_Food: Result := 'Food';
    else                Result := 'N/A';
  end;
end;


function TKMWare.IsValid: Boolean;
begin
  Result := fType in [WARE_MIN..WARE_MAX];
end;


end.
