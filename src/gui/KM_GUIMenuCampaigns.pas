unit KM_GUIMenuCampaigns;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Pics,
  KM_Campaigns, KM_InterfaceDefaults;


type
  TKMMenuCampaigns = class {(TKMGUIPage)}
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
uses KM_ResTexts, KM_ResFonts, KM_GameApp, KM_RenderUI;


{ TKMMainMenuInterface }
constructor TKMMenuCampaigns.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var L: TKMLabel;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_CampSelect := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_CampSelect.AnchorsStretch;

    L := TKMLabel.Create(Panel_CampSelect, 80, 150, 575, 20, gResTexts[TX_MENU_CAMP_HEADER], fnt_Outline, taCenter);
    L.Anchors := [];
    ColumnBox_Camps := TKMColumnBox.Create(Panel_CampSelect, 80, 180, 575, 360, fnt_Grey, bsMenu);
    ColumnBox_Camps.SetColumns(fnt_Outline, [gResTexts[TX_MENU_CAMPAIGNS_TITLE],
                                             gResTexts[TX_MENU_CAMPAIGNS_MAPS_COUNT],
                                             gResTexts[TX_MENU_CAMPAIGNS_MAPS_UNLOCKED]],
                                             [0, 305, 440]);
    ColumnBox_Camps.Anchors := [];
    ColumnBox_Camps.SearchColumn := 0;
    ColumnBox_Camps.OnChange := ListChange;
    ColumnBox_Camps.OnDoubleClick := StartClick;

    with TKMBevel.Create(Panel_CampSelect, 669, 180, 275, 208) do Anchors := [];
    Image_CampsPreview := TKMImage.Create(Panel_CampSelect, 673, 184, 267, 200, 0, rxGuiMain);
    Image_CampsPreview.ImageStretch;
    Image_CampsPreview.Anchors := [];

    Memo_CampDesc := TKMMemo.Create(Panel_CampSelect, 669, 400, 275, 140, fnt_Game, bsMenu);
    Memo_CampDesc.Anchors := [];
    Memo_CampDesc.AutoWrap := True;
    Memo_CampDesc.ItemHeight := 16;

    Button_Camp_Start := TKMButton.Create(Panel_CampSelect, 362, 550, 300, 30, gResTexts[TX_MENU_CAMP_START], bsMenu);
    Button_Camp_Start.Anchors := [];
    Button_Camp_Start.OnClick := StartClick;

    Button_Camp_Back := TKMButton.Create(Panel_CampSelect, 362, 595, 300, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_Camp_Back.Anchors := [];
    Button_Camp_Back.OnClick := BackClick;
end;


procedure TKMMenuCampaigns.RefreshList;
var
  I: Integer;
  Camps: TKMCampaignsCollection;
begin
  Camps := fGameApp.Campaigns;

  Image_CampsPreview.TexID := 0; //Clear preview image
  ColumnBox_Camps.Clear;
  Memo_CampDesc.Clear;
  for I := 0 to Camps.Count - 1 do
  with Camps[I] do
    ColumnBox_Camps.AddItem(MakeListRow(
                        [CampaignTitle, IntToStr(MapCount), IntToStr(UnlockedMap+1)],
                        [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF], I));

  Button_Camp_Start.Disable;
end;


procedure TKMMenuCampaigns.ListChange(Sender: TObject);
var
  cmp: TKMCampaignId;
  Camp: TKMCampaign;
begin
  Button_Camp_Start.Enable;
  cmp := fGameApp.Campaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].CampaignId;
  Camp := fGameApp.Campaigns.CampaignById(cmp);

  Image_CampsPreview.RX := Camp.BackGroundPic.RX;
  Image_CampsPreview.TexID := Camp.BackGroundPic.ID;

  Memo_CampDesc.Text := Camp.CampaignDescription;
end;


procedure TKMMenuCampaigns.StartClick(Sender: TObject);
var
  cmp: UnicodeString;
begin
  //Get the caption and pass it to Campaign selection menu (it will be casted to TKMCampaignName there)
  //so that we avoid cast/uncast/cast along the event chain
  cmp := fGameApp.Campaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].CampName;
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
