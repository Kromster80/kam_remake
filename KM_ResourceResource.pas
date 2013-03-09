unit KM_ResourceResource;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Defaults;


type
  TKMResourceDat = class
  private
    fType: TResourceType;
    fMarketPrice: Single;
    function GetGUIIcon: Word;
    function GetTitle: AnsiString;
  public
    constructor Create(aType: TResourceType);
    function IsValid: Boolean;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: Single read fMarketPrice;
    property Title: AnsiString read GetTitle;
  end;

  TKMResourceCollection = class
  private
    fList: array [TResourceType] of TKMResourceDat;
    procedure CalculateCostsTable;
    function GetResource(aIndex: TResourceType): TKMResourceDat;
  public
    constructor Create;
    destructor Destroy; override;
    property Resources[aIndex: TResourceType]: TKMResourceDat read GetResource; default;
    procedure ExportCostsTable(const aFilename: string);
  end;


const
  MARKET_TRADEOFF_FACTOR = 2.2; //X resources buys 1 resource of equal value


  ResourceTypeToIndex: array [TResourceType] of byte = (0, //rt_None
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27,
    0, 0, 0); //rt_All, rt_Warfare, rt_Food

  RES_COUNT = 28;
  ResourceIndexToType: array [0..RES_COUNT-1] of TResourceType = (
    rt_Trunk, rt_Stone, rt_Wood, rt_IronOre, rt_GoldOre,
    rt_Coal, rt_Steel, rt_Gold, rt_Wine, rt_Corn,
    rt_Bread, rt_Flour, rt_Leather, rt_Sausages, rt_Pig,
    rt_Skin, rt_Shield, rt_MetalShield, rt_Armor, rt_MetalArmor,
    rt_Axe, rt_Sword, rt_Pike, rt_Hallebard, rt_Bow,
    rt_Arbalet, rt_Horse, rt_Fish);

implementation
uses KM_TextLibrary;


{ TKMResourceCollection }
constructor TKMResourceCollection.Create;
var I: TResourceType;
begin
  inherited;

  for I := Low(TResourceType) to High(TResourceType) do
    fList[I] := TKMResourceDat.Create(I);

  //Calcuate the trade costs for marketplace once
  CalculateCostsTable;
end;


destructor TKMResourceCollection.Destroy;
var I: TResourceType;
begin
  for I := Low(TResourceType) to High(TResourceType) do
    fList[I].Free;

  inherited;
end;


//Export costs table for analysis in human-friendly form
procedure TKMResourceCollection.ExportCostsTable(const aFilename: string);
var
  SL: TStringList;
  I: TResourceType;
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


procedure TKMResourceCollection.CalculateCostsTable;
const
  NON_RENEW = 1.25; //Non-renewable resources are more valuable than renewable ones
  TREE_ADDN = 0.15; //Trees require a large area (e.g. compared to corn)
  WINE_ADDN = 0.1; //Wine takes extra wood to build
  ORE_ADDN = 0.2; //You can only build a few iron/gold mines on most maps (compared to coal)
begin
  //Take advantage of the fact that we have both classes in same unit
  //and assign to private field directly
  Resources[rt_Trunk      ].fMarketPrice := (1/ProductionRate[rt_Trunk]) + TREE_ADDN;
  Resources[rt_Stone      ].fMarketPrice := NON_RENEW*(1/ProductionRate[rt_Stone]);
  Resources[rt_Wood       ].fMarketPrice := (1/ProductionRate[rt_Wood]) + (1/2)*Resources[rt_Trunk].MarketPrice;
  Resources[rt_IronOre    ].fMarketPrice := NON_RENEW*(1/ProductionRate[rt_IronOre]) + ORE_ADDN;
  Resources[rt_GoldOre    ].fMarketPrice := NON_RENEW*(1/ProductionRate[rt_GoldOre]) + ORE_ADDN;
  Resources[rt_Coal       ].fMarketPrice := NON_RENEW*(1/ProductionRate[rt_Coal]);
  Resources[rt_Steel      ].fMarketPrice := (1/ProductionRate[rt_Steel]) + Resources[rt_IronOre].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Gold       ].fMarketPrice := (1/ProductionRate[rt_Gold]) + (1/2)*(Resources[rt_GoldOre].MarketPrice + Resources[rt_Coal].MarketPrice);
  Resources[rt_Wine       ].fMarketPrice := (1/ProductionRate[rt_Wine]) + WINE_ADDN;
  Resources[rt_Corn       ].fMarketPrice := (1/ProductionRate[rt_Corn]);
  Resources[rt_Flour      ].fMarketPrice := (1/ProductionRate[rt_Flour]) + Resources[rt_Corn].MarketPrice;
  Resources[rt_Bread      ].fMarketPrice := (1/ProductionRate[rt_Bread]) + (1/2)*Resources[rt_Flour].MarketPrice;
  Resources[rt_Pig        ].fMarketPrice := (1/ProductionRate[rt_Pig]) + (1/2)*4*Resources[rt_Corn].MarketPrice; //1/2 because two products are made simultaneously
  Resources[rt_Skin       ].fMarketPrice := (1/ProductionRate[rt_Skin]) + (1/2)*4*Resources[rt_Corn].MarketPrice; //1/2 because two products are made simultaneously
  Resources[rt_Leather    ].fMarketPrice := (1/ProductionRate[rt_Leather]) + (1/2)*Resources[rt_Skin].MarketPrice;
  Resources[rt_Sausages   ].fMarketPrice := (1/ProductionRate[rt_Sausages]) + (1/3)*Resources[rt_Pig].MarketPrice;
  Resources[rt_Shield     ].fMarketPrice := (1/ProductionRate[rt_Shield]) + Resources[rt_Wood].MarketPrice;
  Resources[rt_MetalShield].fMarketPrice := (1/ProductionRate[rt_MetalShield]) + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Armor      ].fMarketPrice := (1/ProductionRate[rt_Armor]) + Resources[rt_Leather].MarketPrice;
  Resources[rt_MetalArmor ].fMarketPrice := (1/ProductionRate[rt_MetalArmor]) + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Axe        ].fMarketPrice := (1/ProductionRate[rt_Axe]) + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Sword      ].fMarketPrice := (1/ProductionRate[rt_Sword]) + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Pike       ].fMarketPrice := (1/ProductionRate[rt_Pike]) + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Hallebard  ].fMarketPrice := (1/ProductionRate[rt_Hallebard]) + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Bow        ].fMarketPrice := (1/ProductionRate[rt_Bow]) + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Arbalet    ].fMarketPrice := (1/ProductionRate[rt_Arbalet]) + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Horse      ].fMarketPrice := (1/ProductionRate[rt_Horse]) + 4*Resources[rt_Corn].MarketPrice;
  Resources[rt_Fish       ].fMarketPrice := NON_RENEW*(1/ProductionRate[rt_Fish]);
end;


function TKMResourceCollection.GetResource(aIndex: TResourceType): TKMResourceDat;
begin
  Result := fList[aIndex];
end;


{ TKMResourceDat }
constructor TKMResourceDat.Create(aType: TResourceType);
begin
  inherited Create;

  fType := aType;
end;


function TKMResourceDat.GetGUIIcon: Word;
begin
  if IsValid then
    Result := 351 + ResourceTypeToIndex[fType]
  else
    Result := 41; //Show "Question mark"
end;


function TKMResourceDat.GetTitle: AnsiString;
begin
  if IsValid then
    Result := fTextLibrary[TX_RESOURCES_NAMES__27 + ResourceTypeToIndex[fType]]
  else
    Result := 'N/A';
end;


function TKMResourceDat.IsValid: Boolean;
begin
  Result := fType in [WARE_MIN..WARE_MAX];
end;


end.
