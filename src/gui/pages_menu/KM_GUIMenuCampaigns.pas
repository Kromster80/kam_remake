unit KM_GUIMenuCampaigns;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Pics,
  KM_Campaigns, KM_InterfaceDefaults;


type
  TKMMenuCampaigns = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    procedure RefreshList;
    procedure ListChange(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_CampSelect: TKMPanel;
    ColumnBox_Camps: TKMColumnBox;
    Image_CampsPreview: TKMImage;
    Memo_CampDesc: TKMMemo;
    Button_Camp_Start, Button_Camp_Back: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    procedure Show;
  end;


implementation
uses
  KM_ResTexts, KM_ResFonts, KM_GameApp, KM_RenderUI;


{ TKMMainMenuInterface }
constructor TKMMenuCampaigns.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  L: TKMLabel;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;

  Panel_CampSelect := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_CampSelect.AnchorsStretch;

    TKMLabel.Create(Panel_CampSelect, 80, 140, 575, 20, gResTexts[TX_MENU_CAMP_HEADER], fnt_Outline, taCenter).AnchorsCenter;
    ColumnBox_Camps := TKMColumnBox.Create(Panel_CampSelect, 80, 170, 575, 360, fnt_Grey, bsMenu);
    ColumnBox_Camps.SetColumns(fnt_Outline, [gResTexts[TX_MENU_CAMPAIGNS_TITLE],
                                             gResTexts[TX_MENU_CAMPAIGNS_MAPS_COUNT],
                                             gResTexts[TX_MENU_CAMPAIGNS_MAPS_UNLOCKED]],
                                             [0, 305, 440]);
    ColumnBox_Camps.AnchorsCenter;
    ColumnBox_Camps.SearchColumn := 0;
    ColumnBox_Camps.OnChange := ListChange;
    ColumnBox_Camps.OnDoubleClick := StartClick;

    TKMBevel.Create(Panel_CampSelect, 669, 170, 275, 208).AnchorsCenter;
    Image_CampsPreview := TKMImage.Create(Panel_CampSelect, 673, 174, 267, 200, 0, rxGuiMain);
    Image_CampsPreview.ImageStretch;
    Image_CampsPreview.AnchorsCenter;

    Memo_CampDesc := TKMMemo.Create(Panel_CampSelect, 669, 390, 275, 140, fnt_Game, bsMenu);
    Memo_CampDesc.AnchorsCenter;
    Memo_CampDesc.AutoWrap := True;
    Memo_CampDesc.ItemHeight := 16;

    L := TKMLabel.Create(Panel_CampSelect, 80, 540, 864, 40, gResTexts[TX_MENU_CAMP_HINT], fnt_Grey, taCenter);
    L.AnchorsCenter;
    L.AutoWrap := True;

    Button_Camp_Start := TKMButton.Create(Panel_CampSelect, 362, 580, 300, 30, gResTexts[TX_MENU_CAMP_START], bsMenu);
    Button_Camp_Start.AnchorsCenter;
    Button_Camp_Start.OnClick := StartClick;

    Button_Camp_Back := TKMButton.Create(Panel_CampSelect, 362, 625, 300, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_Camp_Back.AnchorsCenter;
    Button_Camp_Back.OnClick := BackClick;
end;


procedure TKMMenuCampaigns.RefreshList;
var
  I: Integer;
  Camps: TKMCampaignsCollection;
begin
  Camps := gGameApp.Campaigns;

  Image_CampsPreview.TexID := 0; //Clear preview image
  ColumnBox_Camps.Clear;
  Memo_CampDesc.Clear;
  for I := 0 to Camps.Count - 1 do
  begin
    ColumnBox_Camps.AddItem(MakeListRow(
                        [Camps[I].CampaignTitle, IntToStr(Camps[I].MapCount), IntToStr(Camps[I].UnlockedMap + 1)],
                        [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF], I));
    if Camps[I].CampName = gGameApp.GameSettings.MenuCampaignName then
    begin
      ColumnBox_Camps.ItemIndex := I;
      ListChange(nil);
    end;

  end;

  if ColumnBox_Camps.ItemIndex = -1 then
    Button_Camp_Start.Disable
  else
    Button_Camp_Start.Enable;
end;


procedure TKMMenuCampaigns.ListChange(Sender: TObject);
var
  cmp: TKMCampaignId;
  Camp: TKMCampaign;
begin
  //Key press can cause ItemIndex = -1
  if ColumnBox_Camps.ItemIndex = -1 then
  begin
    Button_Camp_Start.Disable;
    Image_CampsPreview.TexID := 0;
    Memo_CampDesc.Clear;
  end
  else
  begin
    Button_Camp_Start.Enable;
    cmp := gGameApp.Campaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].CampaignId;
    Camp := gGameApp.Campaigns.CampaignById(cmp);

    Image_CampsPreview.RX := Camp.BackGroundPic.RX;
    Image_CampsPreview.TexID := Camp.BackGroundPic.ID;

    Memo_CampDesc.Text := Camp.CampaignDescription;
    gGameApp.GameSettings.MenuCampaignName := Camp.CampName;
  end;
end;


procedure TKMMenuCampaigns.StartClick(Sender: TObject);
var
  cmp: UnicodeString;
begin
  //Get the caption and pass it to Campaign selection menu (it will be casted to TKMCampaignName there)
  //so that we avoid cast/uncast/cast along the event chain
  cmp := gGameApp.Campaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].CampName;
  fOnPageChange(gpCampaign, cmp);
end;


procedure TKMMenuCampaigns.BackClick(Sender: TObject);
begin
  fOnPageChange(gpSingleplayer);
end;


procedure TKMMenuCampaigns.Show;
begin
  RefreshList;

  Panel_CampSelect.Show;
end;


end.
