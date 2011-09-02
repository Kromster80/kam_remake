unit KM_ResourceResource;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonTypes, KM_Defaults;


type
  TKMResourceDat = class
  private
    fType:TResourceType;
    function GetGUIIcon: Word;
    function GetMarketPrice: Word;
    function GetName: String;
  public
    constructor Create(aType:TResourceType);
    function IsValid:boolean;
    property GUIIcon: Word read GetGUIIcon;
    property MarketPrice: Word read GetMarketPrice;
    property Name: String read GetName;
  end;

  TKMResourceCollection = class
  private
    fList: array [TResourceType] of TKMResourceDat;
    function GetResource(aIndex: TResourceType): TKMResourceDat;
  public
    constructor Create;
    destructor Destroy; override;
    property Resources[aIndex:TResourceType]:TKMResourceDat read GetResource; default;
  end;


const
  ResourceKaMOrder: array[TResourceType] of byte = (0,
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 0, 0, 0);

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
end;


destructor TKMResourceCollection.Destroy;
var i:TResourceType;
begin
  for i:=Low(TResourceType) to High(TResourceType) do
    fList[i].Free;
  inherited;
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
    Result := 350 + ResourceKaMOrder[fType]
  else
    Result := 0;
end;


function TKMResourceDat.GetMarketPrice: Word;
begin
  Result := 1;
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
  Result := fType in [rt_Trunk..rt_Fish];
end;


end.
