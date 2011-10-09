unit KM_ResourceResource;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Defaults;


type
  TKMResourceDat = class
  private
    fType:TResourceType;
    fMarketPrice:single;
    function GetGUIIcon: Word;
    function GetName: String;
  public
    constructor Create(aType:TResourceType);
    function IsValid:boolean;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: single read fMarketPrice;
    property SetMarketPrice: single write fMarketPrice;
    property Name: String read GetName;
  end;

  TKMResourceCollection = class
  private
    fList: array [TResourceType] of TKMResourceDat;
    procedure CalculateCostsTable;
    function GetResource(aIndex: TResourceType): TKMResourceDat;
  public
    constructor Create;
    destructor Destroy; override;
    property Resources[aIndex:TResourceType]:TKMResourceDat read GetResource; default;
  end;


const
  MarketTradeOffFactor = 4; //X resources buys 1 resource of equal value


  ResourceKaMOrder: array[TResourceType] of byte = (0, //rt_None
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
  Resources[rt_Trunk].SetMarketPrice := 1;
  Resources[rt_Stone].SetMarketPrice := (1/3)*NonRenewableFactor;
  Resources[rt_Wood].SetMarketPrice := ProcessingCost + 0.5*Resources[rt_Trunk].MarketPrice;
  Resources[rt_IronOre].SetMarketPrice := 1*NonRenewableFactor;
  Resources[rt_GoldOre].SetMarketPrice := 1*NonRenewableFactor;
  Resources[rt_Coal].SetMarketPrice := 1*NonRenewableFactor;
  Resources[rt_Steel].SetMarketPrice := ProcessingCost + Resources[rt_IronOre].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Gold].SetMarketPrice := 0.5*(ProcessingCost + Resources[rt_GoldOre].MarketPrice + Resources[rt_Coal].MarketPrice);
  Resources[rt_Wine].SetMarketPrice := 1;
  Resources[rt_Corn].SetMarketPrice := 1;
  Resources[rt_Flour].SetMarketPrice := ProcessingCost + Resources[rt_Corn].MarketPrice;
  Resources[rt_Bread].SetMarketPrice := ProcessingCost + 0.5*Resources[rt_Flour].MarketPrice;
  Resources[rt_Pig].SetMarketPrice := 0.5*(ProcessingCost + 5*Resources[rt_Corn].MarketPrice);
  Resources[rt_Skin].SetMarketPrice := 0.5*(ProcessingCost + 5*Resources[rt_Corn].MarketPrice);
  Resources[rt_Leather].SetMarketPrice := ProcessingCost + 0.5*Resources[rt_Skin].MarketPrice;
  Resources[rt_Sausages].SetMarketPrice := (1/3)*(ProcessingCost + Resources[rt_Pig].MarketPrice);
  Resources[rt_Shield].SetMarketPrice := ProcessingCost + Resources[rt_Wood].MarketPrice;
  Resources[rt_MetalShield].SetMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Armor].SetMarketPrice := ProcessingCost + Resources[rt_Leather].MarketPrice;
  Resources[rt_MetalArmor].SetMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Axe].SetMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Sword].SetMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Pike].SetMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Hallebard].SetMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Bow].SetMarketPrice := ProcessingCost + 2*Resources[rt_Wood].MarketPrice;
  Resources[rt_Arbalet].SetMarketPrice := ProcessingCost + Resources[rt_Steel].MarketPrice + Resources[rt_Coal].MarketPrice;
  Resources[rt_Horse].SetMarketPrice := ProcessingCost + 5*Resources[rt_Corn].MarketPrice;
  Resources[rt_Fish].SetMarketPrice := 0.5*NonRenewableFactor;
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


function TKMResourceDat.GetName: String;
begin
  if IsValid then
    Result := fTextLibrary.GetTextString(siResourceNames+ResourceKaMOrder[fType])
  else
    Result := 'N/A';
end;


function TKMResourceDat.IsValid: boolean;
begin
  //@Krom: Crash here when exporting the delivery queues
  Result := fType in [WARE_MIN..WARE_MAX];
end;


end.
