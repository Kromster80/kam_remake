unit KM_GUIMapEdPlayerBlockTrade;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_Pics, KM_InterfaceGame;

type
  TKMMapEdPlayerBlockTrade = class
  private
    procedure Player_BlockTradeClick(Sender: TObject);
    procedure Player_BlockTradeRefresh;
  protected
    Panel_BlockTrade: TKMPanel;
    Button_BlockTrade: array [1..STORE_RES_COUNT] of TKMButtonFlat;
    Image_BlockTrade: array [1..STORE_RES_COUNT] of TKMImage;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_Hand,
  KM_Resource, KM_RenderUI, KM_ResFonts, KM_ResWares;


{ TKMMapEdPlayerBlockTrade }
constructor TKMMapEdPlayerBlockTrade.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  Panel_BlockTrade := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_BlockTrade, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_BLOCK_TRADE], fnt_Outline, taCenter);
  for I := 1 to STORE_RES_COUNT do
  begin
    Button_BlockTrade[I] := TKMButtonFlat.Create(Panel_BlockTrade, ((I-1) mod 5)*37, 30 + ((I-1) div 5)*37,33,33, 0);
    Button_BlockTrade[I].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_BlockTrade[I].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_BlockTrade[I].OnClick := Player_BlockTradeClick;
    Button_BlockTrade[I].Tag := I;
    Image_BlockTrade[I] := TKMImage.Create(Panel_BlockTrade, ((I-1) mod 5)*37 + 15, 30 + ((I-1) div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockTrade[I].Hitable := False;
    Image_BlockTrade[I].ImageCenter;
  end;
end;


procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeClick(Sender: TObject);
var
  I: Integer;
  R: TWareType;
begin
  I := TKMButtonFlat(Sender).Tag;
  R := StoreResType[I];

  gMySpectator.Hand.Locks.AllowToTrade[R] := not gMySpectator.Hand.Locks.AllowToTrade[R];

  Player_BlockTradeRefresh;
end;


procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeRefresh;
var
  I: Integer;
  R: TWareType;
begin
  for I := 1 to STORE_RES_COUNT do
  begin
    R := StoreResType[I];
    if gMySpectator.Hand.Locks.AllowToTrade[R] then
      Image_BlockTrade[I].TexID := 0
    else
      Image_BlockTrade[I].TexID := 32; //Red cross
  end;
end;


procedure TKMMapEdPlayerBlockTrade.Hide;
begin
  Panel_BlockTrade.Hide;
end;


procedure TKMMapEdPlayerBlockTrade.Show;
begin
  Player_BlockTradeRefresh;
  Panel_BlockTrade.Show;
end;


function TKMMapEdPlayerBlockTrade.Visible: Boolean;
begin
  Result := Panel_BlockTrade.Visible;
end;


end.
