unit KM_HouseMarket;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,
  KM_Houses, KM_ResHouses, KM_ResWares;

type
  //Marketplace
  TKMHouseMarket = class(TKMHouse)
  private
    fResFrom, fResTo: TWareType;
    fMarketResIn: array [WARE_MIN..WARE_MAX] of Word;
    fMarketResOut: array [WARE_MIN..WARE_MAX] of Word;
    fMarketDeliveryCount: array [WARE_MIN..WARE_MAX] of Word;
    fTradeAmount: Word;
    procedure AttemptExchange;
    procedure SetResFrom(aRes: TWareType);
    procedure SetResTo(aRes: TWareType);
  protected
    function GetResOrder(aId: Byte): Integer; override;
    procedure SetResOrder(aId: Byte; aValue: Integer); override;
  public
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;

    procedure DemolishHouse(aFrom: TKMHandIndex; IsSilent: Boolean = False); override;
    property ResFrom: TWareType read fResFrom write SetResFrom;
    property ResTo: TWareType read fResTo write SetResTo;
    function RatioFrom: Byte;
    function RatioTo: Byte;

    function AllowedToTrade(aRes: TWareType): Boolean;
    function TradeInProgress: Boolean;
    function GetResTotal(aWare: TWareType): Word; overload;
    function CheckResIn(aWare: TWareType): Word; override;
    function CheckResOut(aWare: TWareType): Word; override;
    procedure ResAddToIn(aResource: TWareType; aCount: Word=1; aFromScript: Boolean=false); override;
    procedure ResTakeFromOut(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function ResCanAddToIn(aRes: TWareType): Boolean; override;
    function ResOutputAvailable(aRes: TWareType; const aCount: Word): Boolean; override;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_HandLogistics, KM_HandsCollection, KM_RenderPool, KM_Resource, KM_Sound, KM_ResSound, KM_ScriptingEvents, KM_Hand;


{ TKMHouseMarket }
constructor TKMHouseMarket.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
begin
  inherited;

  fResFrom := wt_None;
  fResTo := wt_None;
end;


procedure TKMHouseMarket.DemolishHouse(aFrom: TKMHandIndex; IsSilent: Boolean = False);
var
  R: TWareType;
begin
  //Count resources as lost
  for R := WARE_MIN to WARE_MAX do
    gHands[fOwner].Stats.WareConsumed(R, fMarketResIn[R] + fMarketResOut[R]);

  inherited;
end;


function TKMHouseMarket.GetResTotal(aWare: TWareType): Word;
begin
  Result := fMarketResIn[aWare] + fMarketResOut[aWare];
end;


function TKMHouseMarket.CheckResIn(aWare: TWareType): Word;
begin
  Result := fMarketResIn[aWare];
end;


function TKMHouseMarket.CheckResOut(aWare: TWareType): Word;
begin
  Result := fMarketResOut[aWare];
end;


function TKMHouseMarket.GetResOrder(aID: Byte): Integer;
begin
  Result := fTradeAmount;
end;


function TKMHouseMarket.RatioFrom: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> wt_None) and (fResTo <> wt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := gRes.Wares[fResFrom].MarketPrice;
    CostTo := gRes.Wares[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostTo / Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


function TKMHouseMarket.RatioTo: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> wt_None) and (fResTo <> wt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := gRes.Wares[fResFrom].MarketPrice;
    CostTo := gRes.Wares[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostFrom / Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


procedure TKMHouseMarket.ResAddToIn(aResource: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
var ResRequired: Integer;
begin
  //If user cancelled the exchange (or began new one with different resources already)
  //then incoming resourced should be added to Offer list immediately
  //We don't want Marketplace to act like a Store
  if not aFromScript then
    dec(fMarketDeliveryCount[aResource], aCount); //We must keep track of the number ordered, which is less now because this has arrived
  if (aResource = fResFrom) and TradeInProgress then
  begin
    inc(fMarketResIn[aResource], aCount); //Place the new resource in the IN list
    //As we only order 10 resources at one time, we might need to order another now to fill the gap made by the one delivered
    ResRequired := fTradeAmount*RatioFrom - (fMarketResIn[aResource]+fMarketDeliveryCount[aResource]);
    if ResRequired > 0 then
    begin
      inc(fMarketDeliveryCount[aResource], Min(aCount, ResRequired));
      gHands[fOwner].Deliveries.Queue.AddDemand(Self, nil, fResFrom, Min(aCount, ResRequired), dtOnce, diNorm);
    end;
    AttemptExchange;
  end
  else
  begin
    inc(fMarketResOut[aResource], aCount); //Place the new resource in the OUT list
    gHands[fOwner].Deliveries.Queue.AddOffer(Self, aResource, aCount);
  end;
end;


function TKMHouseMarket.ResCanAddToIn(aRes: TWareType): Boolean;
begin
  Result := (aRes in [WARE_MIN..WARE_MAX]);
end;


function TKMHouseMarket.ResOutputAvailable(aRes: TWareType; const aCount: Word): Boolean;
begin
  Assert(aRes in [WARE_MIN..WARE_MAX]);
  Result := (fMarketResOut[aRes] >= aCount);
end;


procedure TKMHouseMarket.AttemptExchange;
var TradeCount: Word;
begin
  Assert((fResFrom <> wt_None) and (fResTo <> wt_None) and (fResFrom <> fResTo));

  //Script might have blocked these resources from trading, if so reset trade order
  if TradeInProgress
  and (not AllowedToTrade(fResFrom) or not AllowedToTrade(fResTo)) then
  begin
    SetResOrder(0, 0);
    Exit;
  end;

  if TradeInProgress and (fMarketResIn[fResFrom] >= RatioFrom) then
  begin
    //How much can we trade
    TradeCount := Min((fMarketResIn[fResFrom] div RatioFrom), fTradeAmount);

    Dec(fMarketResIn[fResFrom], TradeCount * RatioFrom);
    gHands[fOwner].Stats.WareConsumed(fResFrom, TradeCount * RatioFrom);
    Dec(fTradeAmount, TradeCount);
    Inc(fMarketResOut[fResTo], TradeCount * RatioTo);
    gHands[fOwner].Stats.WareProduced(fResTo, TradeCount * RatioTo);
    gHands[fOwner].Deliveries.Queue.AddOffer(Self, fResTo, TradeCount * RatioTo);

    gScriptEvents.ProcMarketTrade(Self, fResFrom, fResTo);
    gSoundPlayer.Play(sfxn_Trade, fPosition);
  end;
end;


procedure TKMHouseMarket.ResTakeFromOut(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  if aFromScript then
  begin
    aCount := Min(aCount, fMarketResOut[aWare]);
    if aCount > 0 then
    begin
      gHands[fOwner].Stats.WareConsumed(aWare, aCount);
      gHands[fOwner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;
  Assert(aCount <= fMarketResOut[aWare]);

  dec(fMarketResOut[aWare], aCount);
end;


function TKMHouseMarket.AllowedToTrade(aRes: TWareType): Boolean;
begin
  Result := gHands[fOwner].Locks.AllowToTrade[aRes];
end;


procedure TKMHouseMarket.SetResFrom(aRes: TWareType);
begin
  if TradeInProgress or not AllowedToTrade(aRes) then
    Exit;

  fResFrom := aRes;
  if fResTo = fResFrom then
    fResTo := wt_None;
end;


procedure TKMHouseMarket.SetResTo(aRes: TWareType);
begin
  if TradeInProgress or not AllowedToTrade(aRes) then
    Exit;

  fResTo := aRes;
  if fResFrom = fResTo then
    fResFrom := wt_None;
end;


function TKMHouseMarket.TradeInProgress: Boolean;
begin
  Result := fTradeAmount > 0;
end;


//Player has changed the amount of order
procedure TKMHouseMarket.SetResOrder(aId: Byte; aValue: Integer);
const
  //Maximum number of Demands we can place at once (stops the delivery queue from becoming clogged with 1500 items)
  MAX_RES_ORDERED = 10;
var
  ResRequired, OrdersAllowed, OrdersRemoved: Integer;
begin
  if (fResFrom = wt_None) or (fResTo = wt_None) or (fResFrom = fResTo) then Exit;

  fTradeAmount := EnsureRange(aValue, 0, MAX_WARES_ORDER);

  //Try to make an exchange from existing resources
  AttemptExchange;

  //If player cancelled exchange then move all remainders of From resource to Offers list
  if (fTradeAmount = 0) and (fMarketResIn[fResFrom] > 0) then
  begin
    inc(fMarketResOut[fResFrom], fMarketResIn[fResFrom]);
    gHands[fOwner].Deliveries.Queue.AddOffer(Self, fResFrom, fMarketResIn[fResFrom]);
    fMarketResIn[fResFrom] := 0;
  end;

  //@Lewin: If player has cancelled the exchange and then started it again resources will not be
  //removed from offers list and perhaps serf will carry them off the marketplace
  //@Krom: Yes. It would be better if the deliveries were abandoned and the resources were use in
  //the new trade. For example I might be trading stone to bread, then cancel and change from stone to wine.
  //I would expect any stone already at the marketplace to stay since the new trade requires it,
  //it looks bad that serfs remove the stone then take it back. To be converted to todo item.

  //How much do we need to ask to add to delivery system = Needed - (Ordered + Arrived)
  ResRequired := (fTradeAmount * RatioFrom - (fMarketDeliveryCount[fResFrom]+fMarketResIn[fResFrom]));
  OrdersAllowed := MAX_RES_ORDERED - fMarketDeliveryCount[fResFrom];

  Assert(OrdersAllowed >= 0); //We must never have ordered more than we are allowed

  //Order as many as we can within our limit
  if (ResRequired > 0) and (OrdersAllowed > 0) then
  begin
    inc(fMarketDeliveryCount[fResFrom], Min(ResRequired,OrdersAllowed));
    gHands[fOwner].Deliveries.Queue.AddDemand(Self, nil, fResFrom, Min(ResRequired,OrdersAllowed), dtOnce, diNorm)
  end
  else
    //There are too many resources ordered, so remove as many as we can from the delivery list (some will be being performed)
    if (ResRequired < 0) then
    begin
      OrdersRemoved := gHands[fOwner].Deliveries.Queue.TryRemoveDemand(Self, fResFrom, -ResRequired);
      dec(fMarketDeliveryCount[fResFrom], OrdersRemoved);
    end;
end;


constructor TKMHouseMarket.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fTradeAmount);
  LoadStream.Read(fResFrom, SizeOf(fResFrom));
  LoadStream.Read(fResTo, SizeOf(fResTo));
  LoadStream.Read(fMarketResIn, SizeOf(fMarketResIn));
  LoadStream.Read(fMarketResOut, SizeOf(fMarketResOut));
  LoadStream.Read(fMarketDeliveryCount, SizeOf(fMarketDeliveryCount));
end;


procedure TKMHouseMarket.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fTradeAmount);
  SaveStream.Write(fResFrom, SizeOf(fResFrom));
  SaveStream.Write(fResTo, SizeOf(fResTo));
  SaveStream.Write(fMarketResIn, SizeOf(fMarketResIn));
  SaveStream.Write(fMarketResOut, SizeOf(fMarketResOut));
  SaveStream.Write(fMarketDeliveryCount, SizeOf(fMarketDeliveryCount));
end;


//Render special market wares display
procedure TKMHouseMarket.Paint;
var
  R: TWareType;
  MaxCount: Word;
  MaxRes: TWareType;
begin
  inherited;
  if fBuildState < hbs_Done then Exit;

  //Market can display only one ware at a time (lookup ware that has most count)
  MaxCount := 0;
  MaxRes := wt_None;
  for R := WARE_MIN to WARE_MAX do
  if fMarketResIn[R] > MaxCount then
  begin
    MaxCount := fMarketResIn[R];
    MaxRes := R;
  end;

  if MaxCount > 0 then
    //FlagAnimStep is required for horses animation
    fRenderPool.AddHouseMarketSupply(fPosition, MaxRes, MaxCount, FlagAnimStep);
end;


end.
