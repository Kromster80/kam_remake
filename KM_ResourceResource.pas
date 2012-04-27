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
    function GetTitle: string;
  public
    constructor Create(aType: TResourceType);
    function IsValid: Boolean;
    function IsWeapon: Boolean;
    function IsGood: Boolean;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: Single read fMarketPrice;
    property Title: String read GetTitle;
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
  end;


const
  MARKET_TRADEOFF_FACTOR = 2.5; //X resources buys 1 resource of equal value


  ResourceKaMOrder: array [TResourceType] of byte = (0, //rt_None
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27,
    0, 0, 0); //rt_All, rt_Warfare, rt_Food

  RES_COUNT = 28;
  ResourceKaMIndex: array [0..RES_COUNT-1] of TResourceType = (
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
var i:TResourceType;
begin
  inherited;
  for i:=Low(TResourceType) to High(TResourceType) do
    fList[i] := TKMResourceDat.Create(i);
  CalculateCostsTable; //Calcuate the trade costs once
end;


destructor TKMResourceCollection.Destroy;
var i:TResourceType;
begin
  for i:=Low(TResourceType) to High(TResourceType) do
    fList[i].Free;
  inherited;
end;


procedure TKMResourceCollection.CalculateCostsTable;
const
  NonRenewableFactor = 2;
  ProcessingCost = 1;
begin
  //Take advantage of the fact that we have both classes in same unit
  //and assign to private field directly
  Resources[rt_Trunk      ].fMarketPrice := 1;
  Resources[rt_Stone      ].fMarketPrice := (1/3)*NonRenewableFactor;
  Resources[rt_Wood       ].fMarketPrice := (1/2)*(ProcessingCost + Resources[rt_Trunk].MarketPrice);
  Resources[rt_IronOre    ].fMarketPrice := 1*NonRenewableFactor;
  Resources[rt_GoldOre    ].fMarketPrice := 1*NonRenewableFactor;
  Resources[rt_Coal       ].fMarketPrice := 1*NonRenewableFactor;
  Resources[rt_Steel      ].fMarketPrice := ProcessingCost + Resources[rt_IronOre].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Gold       ].fMarketPrice := (1/2)*(ProcessingCost + Resources[rt_GoldOre].MarketPrice + Resources[rt_Coal].MarketPrice);
  Resources[rt_Wine       ].fMarketPrice := 1;
  Resources[rt_Corn       ].fMarketPrice := 1;
  Resources[rt_Flour      ].fMarketPrice := ProcessingCost + Resources[rt_Corn].MarketPrice;
  Resources[rt_Bread      ].fMarketPrice := (1/2)*(ProcessingCost + Resources[rt_Flour].MarketPrice);
  Resources[rt_Pig        ].fMarketPrice := (1/2)*(ProcessingCost + 4*Resources[rt_Corn].MarketPrice); //1/2 because two products are made simultaneously
  Resources[rt_Skin       ].fMarketPrice := (1/2)*(ProcessingCost + 4*Resources[rt_Corn].MarketPrice); //1/2 because two products are made simultaneously
  Resources[rt_Leather    ].fMarketPrice := (1/2)*(ProcessingCost + Resources[rt_Skin].MarketPrice);
  Resources[rt_Sausages   ].fMarketPrice := (1/3)*(ProcessingCost + Resources[rt_Pig].MarketPrice);
  Resources[rt_Shield     ].fMarketPrice := ProcessingCost + Resources[rt_Wood].MarketPrice;
  Resources[rt_MetalShield].fMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Armor      ].fMarketPrice := ProcessingCost + Resources[rt_Leather].MarketPrice;
  Resources[rt_MetalArmor ].fMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Axe        ].fMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Sword      ].fMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Pike       ].fMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Hallebard  ].fMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Bow        ].fMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Arbalet    ].fMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Horse      ].fMarketPrice := ProcessingCost + 4*Resources[rt_Corn].MarketPrice;
  Resources[rt_Fish       ].fMarketPrice := (1/2)*NonRenewableFactor;
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
    Result := 351 + ResourceKaMOrder[fType]
  else
    Result := 41; //Show "Question mark"
end;


function TKMResourceDat.GetTitle: string;
begin
  if IsValid then
    Result := fTextLibrary[TX_RESOURCES_NAMES__27 + ResourceKaMOrder[fType]]
  else
    Result := 'N/A';
end;


function TKMResourceDat.IsValid: boolean;
begin
  Result := fType in [WARE_MIN..WARE_MAX];
end;


function TKMResourceDat.IsWeapon: Boolean;
begin
  Result := fType in [WEAPON_MIN..WEAPON_MAX];
end;


function TKMResourceDat.IsGood: Boolean;
begin
  Result := IsValid and not (fType in [WEAPON_MIN..WEAPON_MAX]);
end;


end.
