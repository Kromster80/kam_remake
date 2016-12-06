unit KM_HandLocks;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults,
  KM_ResHouses, KM_ResWares;


type
  // Permissions
  TKMHandLocks = class
  private
    fHouseUnlocked: array [THouseType] of Boolean; //If building requirements performed
    fWareUnlockedForTrade: array [WARE_MIN..WARE_MAX] of Boolean;
    procedure UpdateReqDone(aType: THouseType);
    function IsWareUnlockedForTrade(aWareType: TWareType): Boolean;
  public
    HouseBlocked: array [THouseType] of Boolean; //Allowance derived from mission script
    HouseGranted: array [THouseType] of Boolean; //Allowance derived from mission script
    UnitBlocked: array [TUnitType] of Boolean;   //Allowance derived from mission script
    WareTradeState: array [WARE_MIN..WARE_MAX] of TWareTradeState; //Trade state derived from mission script

    constructor Create;

    procedure HouseCreated(aType: THouseType);
    function HouseCanBuild(aType: THouseType): Boolean;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property WareUnlockedForTrade[aWareType: TWareType]: Boolean read IsWareUnlockedForTrade;
  end;


implementation
uses
  KM_Resource;


{ TKMHandLocks }
constructor TKMHandLocks.Create;
var
  W: TWareType;
begin
  inherited;

  for W := WARE_MIN to WARE_MAX do
  begin
    WareTradeState[W] := wts_Default;
    fWareUnlockedForTrade[W] := False;
  end;

  //Release Store at the start of the game by default
  fHouseUnlocked[ht_Store] := True;
end;


function TKMHandLocks.IsWareUnlockedForTrade(aWareType: TWareType): Boolean;
begin
  case WareTradeState[aWareType] of
    wts_Allow:    Result := True;
    wts_Block:    Result := False;
    wts_Default:  Result := fWareUnlockedForTrade[aWareType];
  end;
end;


procedure TKMHandLocks.UpdateReqDone(aType: THouseType);
var
  H: THouseType;
  I: Integer;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    if gRes.HouseDat[H].ReleasedBy = aType then
      fHouseUnlocked[H] := True;

  for I := Low(THouseRes) to High(THouseRes) do
    if gRes.HouseDat[aType].ResOutput[I] in [WARE_MIN..WARE_MAX] then
      fWareUnlockedForTrade[gRes.HouseDat[aType].ResOutput[I]] := True;
end;


// New house, either built by player or created by mission script
procedure TKMHandLocks.HouseCreated(aType: THouseType);
begin
  UpdateReqDone(aType);
end;


// Get effective permission
function TKMHandLocks.HouseCanBuild(aType: THouseType): Boolean;
begin
  Result := (fHouseUnlocked[aType] or HouseGranted[aType]) and not HouseBlocked[aType];
end;


procedure TKMHandLocks.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('HandLocks');
  SaveStream.Write(HouseBlocked, SizeOf(HouseBlocked));
  SaveStream.Write(HouseGranted, SizeOf(HouseGranted));
  SaveStream.Write(UnitBlocked, SizeOf(UnitBlocked));
  SaveStream.Write(WareTradeState, SizeOf(WareTradeState));
  SaveStream.Write(fWareUnlockedForTrade, SizeOf(fWareUnlockedForTrade));
  SaveStream.Write(fHouseUnlocked, SizeOf(fHouseUnlocked));
end;


procedure TKMHandLocks.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('HandLocks');
  LoadStream.Read(HouseBlocked, SizeOf(HouseBlocked));
  LoadStream.Read(HouseGranted, SizeOf(HouseGranted));
  LoadStream.Read(UnitBlocked, SizeOf(UnitBlocked));
  LoadStream.Read(WareTradeState, SizeOf(WareTradeState));
  LoadStream.Read(fWareUnlockedForTrade, SizeOf(fWareUnlockedForTrade));
  LoadStream.Read(fHouseUnlocked, SizeOf(fHouseUnlocked));
end;


end.
