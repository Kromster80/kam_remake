unit KM_HouseMarket;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math,
   KM_CommonClasses, KM_Defaults, KM_Houses, KM_Points;

type
  //Marketplace
  TKMHouseMarket = class(TKMHouse)
  protected
    fResFrom, fResTo: TResourceType;
    fMarketResIn: array [WARE_MIN..WARE_MAX] of Word;
    fMarketResOut: array [WARE_MIN..WARE_MAX] of Word;
    fMarketDeliveryCount: array [WARE_MIN..WARE_MAX] of Word;
    fTradeAmount: Word;
    procedure AttemptExchange;
    procedure SetResFrom(Value: TResourceType);
    procedure SetResTo(Value: TResourceType);
  public
    constructor Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;

    procedure DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False); override;
    property ResFrom: TResourceType read fResFrom write SetResFrom;
    property ResTo: TResourceType read fResTo write SetResTo;
    function RatioFrom: Byte;
    function RatioTo: Byte;

    function AllowedToTrade(Value: TResourceType): Boolean;
    function TradeInProgress: Boolean;
    function GetResTotal(aResource: TResourceType): Word; overload;
    function CheckResIn(aResource: TResourceType): Word; override;
    function CheckResOrder(aID: Byte): Word; override;
    procedure ResAddToIn(aResource: TResourceType; aCount: Word=1; aFromScript: Boolean=false); override;
    procedure ResEditOrder(aID:byte; aAmount:integer); override;
    procedure ResTakeFromOut(aResource: TResourceType; const aCount: Word=1); override;
    function ResCanAddToIn(aRes: TResourceType): Boolean; override;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_DeliverQueue, KM_PlayersCollection, KM_RenderPool, KM_Resource, KM_ResourceResource, KM_Sound;


{ TKMHouseMarket }
constructor TKMHouseMarket.Create(aID: Cardinal; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TPlayerIndex; aBuildState: THouseBuildState);
begin
  inherited;

  fResFrom := rt_None;
  fResTo := rt_None;
end;


procedure TKMHouseMarket.DemolishHouse(aFrom: TPlayerIndex; IsEditor: Boolean = False);
var
  R: TResourceType;
begin
  for R := WARE_MIN to WARE_MAX do
    fPlayers[fOwner].Stats.GoodConsumed(R, fMarketResIn[R] + fMarketResOut[R]);

  inherited;
end;


function TKMHouseMarket.GetResTotal(aResource:TResourceType):word;
begin
  if aResource in [WARE_MIN..WARE_MAX] then
    Result := fMarketResIn[aResource] + fMarketResOut[aResource]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


function TKMHouseMarket.CheckResIn(aResource:TResourceType):word;
begin
  if aResource in [WARE_MIN..WARE_MAX] then
    Result := fMarketResIn[aResource]
  else
  begin
    Result := 0;
    Assert(False);
  end;
end;


function TKMHouseMarket.CheckResOrder(aID: byte): word;
begin
  Result := fTradeAmount;
end;


function TKMHouseMarket.RatioFrom: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> rt_None) and (fResTo <> rt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := fResource.Resources[fResFrom].MarketPrice;
    CostTo := fResource.Resources[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostTo/Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


function TKMHouseMarket.RatioTo: Byte;
var CostFrom, CostTo: Single;
begin
  if (fResFrom <> rt_None) and (fResTo <> rt_None) then
  begin
    //When trading target ware is priced higher
    CostFrom := fResource.Resources[fResFrom].MarketPrice;
    CostTo := fResource.Resources[fResTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Round(CostFrom/Min(CostFrom, CostTo));
  end else
    Result := 1;
end;


procedure TKMHouseMarket.ResAddToIn(aResource: TResourceType; aCount:word=1; aFromScript:boolean=false);
var ResRequired:integer;
begin
  //If user cancelled the exchange (or began new one with different resources already)
  //then incoming resourced should be added to Offer list immediately
  //We don't want Marketplace to act like a Store
  if not aFromScript then
    dec(fMarketDeliveryCount[aResource], aCount); //We must keep track of the number ordered, which is less now because this has arrived
  if (aResource = fResFrom) and (fTradeAmount > 0) then
  begin
    inc(fMarketResIn[aResource], aCount); //Place the new resource in the IN list
    //As we only order 10 resources at one time, we might need to order another now to fill the gap made by the one delivered
    ResRequired := fTradeAmount*RatioFrom - (fMarketResIn[aResource]+fMarketDeliveryCount[aResource]);
    if ResRequired > 0 then
    begin
      inc(fMarketDeliveryCount[aResource], Min(aCount, ResRequired));
      fPlayers[fOwner].Deliveries.Queue.AddDemand(Self, nil, fResFrom, Min(aCount, ResRequired), dt_Once, di_Norm);
    end;
    AttemptExchange;
  end
  else
  begin
    inc(fMarketResOut[aResource], aCount); //Place the new resource in the OUT list
    fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, aResource, aCount);
  end;
end;


function TKMHouseMarket.ResCanAddToIn(aRes: TResourceType): Boolean;
begin
  Result := (aRes in [WARE_MIN..WARE_MAX]);
end;


procedure TKMHouseMarket.AttemptExchange;
var TradeCount: Word;
begin
  Assert((fResFrom <> rt_None) and (fResTo <> rt_None) and (fResFrom <> fResTo) and
          AllowedToTrade(fResFrom) and AllowedToTrade(fResTo));

  if (fTradeAmount > 0) and (fMarketResIn[fResFrom] >= RatioFrom) then
  begin
    //How much can we trade
    TradeCount := Min((fMarketResIn[fResFrom] div RatioFrom), fTradeAmount);

    dec(fMarketResIn[fResFrom], TradeCount * RatioFrom);
    fPlayers[fOwner].Stats.GoodConsumed(fResFrom, TradeCount * RatioFrom);
    dec(fTradeAmount, TradeCount);
    inc(fMarketResOut[fResTo], TradeCount * RatioTo);
    fPlayers[fOwner].Stats.GoodProduced(fResTo, TradeCount * RatioTo);
    fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, fResTo, TradeCount * RatioTo);

    fSoundLib.Play(sfxn_Trade,GetEntrance);
  end;
end;


procedure TKMHouseMarket.ResTakeFromOut(aResource:TResourceType; const aCount: Word=1);
begin
  Assert(aCount <= fMarketResOut[aResource]);

  dec(fMarketResOut[aResource], aCount);
end;


function TKMHouseMarket.AllowedToTrade(Value: TResourceType):boolean;
begin
  Result := fPlayers[fOwner].Stats.AllowToTrade[Value];
end;


procedure TKMHouseMarket.SetResFrom(Value: TResourceType);
begin
  if not AllowedToTrade(Value) then exit;
  if fTradeAmount > 0 then Exit;
  fResFrom := Value;
  if fResTo = fResFrom then
    fResTo := rt_None;
end;


procedure TKMHouseMarket.SetResTo(Value: TResourceType);
begin
  if not AllowedToTrade(Value) or TradeInProgress then
    Exit;

  fResTo := Value;
  if fResFrom = fResTo then
    fResFrom := rt_None;
end;


function TKMHouseMarket.TradeInProgress: Boolean;
begin
  Result := fTradeAmount > 0;
end;


//Player has changed the amount of order
procedure TKMHouseMarket.ResEditOrder(aID:byte; aAmount:integer);
var ResRequired, OrdersAllowed, OrdersRemoved:integer;
const MAX_RES_ORDERED = 10; //Maximum number of Demands we can place at once (stops the delivery queue from becoming clogged with 1500 items)
begin
  if (fResFrom = rt_None) or (fResTo = rt_None) or (fResFrom = fResTo) then Exit;

  fTradeAmount := EnsureRange(fTradeAmount + aAmount, 0, MAX_ORDER);

  //Try to make an exchange from existing resources
  AttemptExchange;

  //If player cancelled exchange then move all remainders of From resource to Offers list
  if (fTradeAmount = 0) and (fMarketResIn[fResFrom] > 0) then
  begin
    inc(fMarketResOut[fResFrom], fMarketResIn[fResFrom]);
    fPlayers[fOwner].Deliveries.Queue.AddOffer(Self, fResFrom, fMarketResIn[fResFrom]);
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
    fPlayers[fOwner].Deliveries.Queue.AddDemand(Self, nil, fResFrom, Min(ResRequired,OrdersAllowed), dt_Once, di_Norm)
  end
  else
    //There are too many resources ordered, so remove as many as we can from the delivery list (some will be being performed)
    if (ResRequired < 0) then
    begin
      OrdersRemoved := fPlayers[fOwner].Deliveries.Queue.TryRemoveDemand(Self, fResFrom, -ResRequired);
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
  R: TResourceType;
  MaxCount: Word;
  MaxRes: TResourceType;
begin
  inherited;
  if fBuildState < hbs_Done then Exit;

  //Market can display only one ware at a time (lookup ware that has most count)
  MaxCount := 0;
  MaxRes := rt_None;
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
