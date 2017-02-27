unit KM_GUIGameRatios;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_Pics,
  KM_InterfaceGame, KM_ResHouses;


type
  TKMRatioTab = (rtSteel, rtCoal, rtWood, rtCorn);

  TKMGUIGameRatios = class
  private
    fActiveTab: TKMRatioTab; //Active resource distribution tab
    fAllowEditing: Boolean;

    procedure RatioTabClick(Sender: TObject);
    procedure RatioTabSet(aTab: TKMRatioTab);
    procedure RatiosChange(Sender: TObject);
  protected
    Panel_Ratios: TKMPanel;
    Button_Ratios: array [TKMRatioTab] of TKMButton;
    Image_RatioHead: TKMImage;
    Label_RatioHead: TKMLabel;
    Image_RatioPic: array [0..3] of TKMImage;
    TrackBar_RatioValue: array [0..3] of TKMTrackBar;
  public
    constructor Create(aParent: TKMPanel; aAllowEditing: Boolean);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_GameInputProcess, KM_RenderUI, KM_HandsCollection, KM_ResTexts, KM_Game, KM_GameApp,
  KM_Resource, KM_ResFonts, KM_ResWares, KM_Hand;


const
  ResRatioType: array [TKMRatioTab] of TWareType = (wt_Steel, wt_Coal, wt_Wood, wt_Corn);
  ResRatioHint: array [TKMRatioTab] of Word = (298, 300, 302, 304);
  ResRatioHouseCount: array [TKMRatioTab] of Byte = (2, 4, 2, 3);
  ResRatioHouse: array [TKMRatioTab, 0..3] of THouseType = (
      (ht_WeaponSmithy,   ht_ArmorSmithy,     ht_None,          ht_None),
      (ht_IronSmithy,     ht_Metallurgists,   ht_WeaponSmithy,  ht_ArmorSmithy),
      (ht_ArmorWorkshop,  ht_WeaponWorkshop,  ht_None,          ht_None),
      (ht_Mill,           ht_Swine,           ht_Stables,       ht_None));


{ TKMGUIGameRatios }
constructor TKMGUIGameRatios.Create(aParent: TKMPanel; aAllowEditing: Boolean);
var
  I: TKMRatioTab;
  K: Integer;
begin
  inherited Create;

  fAllowEditing := aAllowEditing;
  Panel_Ratios:=TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 332);

  for I := Low(TKMRatioTab) to High(TKMRatioTab) do
  begin
    Button_Ratios[I]         := TKMButton.Create(Panel_Ratios, Byte(I) * 40, 20, 32, 32, 0, rxGui, bsGame);
    Button_Ratios[I].TexID   := gRes.Wares[ResRatioType[I]].GUIIcon;
    Button_Ratios[I].Hint    := gRes.Wares[ResRatioType[I]].Title;
    Button_Ratios[I].Tag     := Byte(I);
    Button_Ratios[I].OnClick := RatioTabClick;
  end;

  Image_RatioHead := TKMImage.Create(Panel_Ratios, 4, 76, 32, 32, 327);
  Label_RatioHead := TKMLabel.Create(Panel_Ratios, 36, 72, 148, 30, NO_TEXT, fnt_Outline, taLeft);

  for K := 0 to 3 do
  begin
    Image_RatioPic[K]             := TKMImage.Create(Panel_Ratios, 4, 124 + K * 50, 32, 32, 327);
    TrackBar_RatioValue[K]          := TKMTrackBar.Create(Panel_Ratios, 40, 116 + K * 50, 140, 0, 5);
    TrackBar_RatioValue[K].Font     := fnt_Grey; //fnt_Metal doesn't fit the text
    TrackBar_RatioValue[K].Tag      := K;
    TrackBar_RatioValue[K].OnChange := RatiosChange;
    TrackBar_RatioValue[K].Enabled := fAllowEditing;
  end;
end;


procedure TKMGUIGameRatios.RatioTabClick(Sender: TObject);
begin
  RatioTabSet(TKMRatioTab(TKMButton(Sender).Tag));
end;


//Switch between tabs
procedure TKMGUIGameRatios.RatioTabSet(aTab: TKMRatioTab);
var
  I: Integer;
  HT: THouseType;
begin
  //Hide everything but the tab buttons
  for I := 0 to Panel_Ratios.ChildCount - 1 do
    if not (Panel_Ratios.Childs[I] is TKMButton) then
      Panel_Ratios.Childs[I].Hide;

  fActiveTab := aTab;

  Image_RatioHead.TexID := gRes.Wares[ResRatioType[fActiveTab]].GUIIcon;//Show resource icon
  Label_RatioHead.Caption := gRes.Wares[ResRatioType[fActiveTab]].Title;
  Image_RatioHead.Show;
  Label_RatioHead.Show;

  for I := 0 to ResRatioHouseCount[fActiveTab] - 1 do
  begin
    HT := ResRatioHouse[fActiveTab, I];
    //Do not allow player to see blocked house (never able to build). Though house may be prebuilt and blocked
    if (not gMySpectator.Hand.Locks.HouseBlocked[HT])
    or (gMySpectator.Hand.Stats.GetHouseQty(HT) > 0) then
    begin
      Image_RatioPic[I].TexID := gRes.Houses[HT].GUIIcon;
      TrackBar_RatioValue[I].Caption := gRes.Houses[HT].HouseName;
      TrackBar_RatioValue[I].Position := gMySpectator.Hand.Stats.WareDistribution[ResRatioType[fActiveTab], HT];
      TrackBar_RatioValue[I].Enabled := fAllowEditing;
    end else begin
      Image_RatioPic[I].TexID := 41; //Question mark
      TrackBar_RatioValue[I].Caption := gResTexts[TX_GAMEPLAY_NOT_AVAILABLE];
      TrackBar_RatioValue[I].Position := 0;
      TrackBar_RatioValue[I].Disable;
    end;

    Image_RatioPic[I].Show;
    TrackBar_RatioValue[I].Show;
  end;
end;


procedure TKMGUIGameRatios.RatiosChange(Sender: TObject);
var
  ware: TWareType;
  house: THouseType;
  value: Byte;
begin
  ware := ResRatioType[fActiveTab];
  house := ResRatioHouse[fActiveTab, TKMTrackBar(Sender).Tag];
  value := TKMTrackBar(Sender).Position;
  gGameApp.GameSettings.WareDistribution[ware, house] := value;
  gGame.GameInputProcess.CmdWareDistribution(gic_WareDistributionChange, ware, house, value);
end;


procedure TKMGUIGameRatios.Show;
var
  I: TKMRatioTab;
  K: Integer;
begin
  Panel_Ratios.Show;
  //Select the default tab, which is the first tab with an unblocked house
  //(so f.e. steel isn't the default tab when it's blocked by mission)
  for I := Low(TKMRatioTab) to High(TKMRatioTab) do
    for K := 0 to ResRatioHouseCount[fActiveTab] - 1 do
      //Do not allow player to see blocked house (never able to build). Though house may be prebuilt and blocked
      if (not gMySpectator.Hand.Locks.HouseBlocked[ResRatioHouse[I, K]])
      or (gMySpectator.Hand.Stats.GetHouseQty(ResRatioHouse[I, K]) > 0) then
      begin
        //Select first tab we find with an unblocked house
        RatioTabSet(I);
        Exit;
      end;
  //All houses are blocked, so select the first tab
  RatioTabSet(rtSteel);
end;


procedure TKMGUIGameRatios.Hide;
begin
  Panel_Ratios.Hide;
end;


function TKMGUIGameRatios.Visible: Boolean;
begin
  Result := Panel_Ratios.Visible;
end;


procedure TKMGUIGameRatios.UpdateState;
begin
  RatioTabSet(fActiveTab);
end;


end.
