unit KM_GUIMenuCampaign;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Pics,
  KM_Campaigns, KM_InterfaceDefaults;


type
  TKMMenuCampaign = class {(TKMGUIPage)}
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fCampaign: TKMCampaign;
    fMapIndex: Byte;

    procedure BackClick(Sender: TObject);

    procedure Campaign_Set(aCampaign: TKMCampaign);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure StartClick(Sender: TObject);
  protected
    Panel_Campaign: TKMPanel;
      Image_CampaignBG: TKMImage;
      Panel_Campaign_Flags: TKMPanel;
        Image_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMImage;
        Image_CampaignSubNode: array[0..MAX_CAMP_NODES - 1] of TKMImage;
      Panel_CampScroll: TKMPanel;
        Image_ScrollTop, Image_Scroll: TKMImage;
        Label_CampaignTitle, Label_CampaignText: TKMLabel;
      Button_CampaignStart, Button_CampaignBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure Resize(X, Y: Word);
    procedure Show(aCampaign: TKMCampaignId);
  end;


implementation
uses KM_GameApp, KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMGUIMainCampaign }
constructor TKMMenuCampaign.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  I: Integer;
begin
  inherited Create;

  fMapIndex := 1;
  fOnPageChange := aOnPageChange;

  Panel_Campaign := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Campaign.AnchorsStretch;
    Image_CampaignBG := TKMImage.Create(Panel_Campaign, 0, 0, aParent.Width, aParent.Height,0,rxGuiMain);
    Image_CampaignBG.ImageStretch;

    Panel_Campaign_Flags:=TKMPanel.Create(Panel_Campaign, 0, 0, aParent.Width, aParent.Height);
    Panel_Campaign_Flags.AnchorsStretch;
    for I := 0 to High(Image_CampaignFlags) do
    begin
      Image_CampaignFlags[I] := TKMImage.Create(Panel_Campaign_Flags, aParent.Width, aParent.Height, 23, 29, 10, rxGuiMain);
      Image_CampaignFlags[I].OnClick := Campaign_SelectMap;
      Image_CampaignFlags[I].Tag := I;
    end;
    for I := 0 to High(Image_CampaignSubNode) do
    begin
      Image_CampaignSubNode[I] := TKMImage.Create(Panel_Campaign_Flags, aParent.Width, aParent.Height, 0, 0, 16, rxGuiMain);
      Image_CampaignSubNode[I].ImageCenter; //Pivot at the center of the dot (width/height = 0)
    end;

  Panel_CampScroll := TKMPanel.Create(Panel_Campaign, 0, 0, 360, 430);
  Panel_CampScroll.Anchors := [akLeft,akBottom];

    Image_Scroll := TKMImage.Create(Panel_CampScroll, 0, 0, 360, 430, 410, rxGui);
    Image_Scroll.ClipToBounds := True;
    Image_Scroll.AnchorsStretch;
    Image_Scroll.ImageAnchors := [akLeft, akRight, akTop];
    Label_CampaignTitle := TKMLabel.Create(Panel_CampScroll, 130, 46, 100, 20, NO_TEXT, fnt_Outline, taCenter);

    Label_CampaignText := TKMLabel.Create(Panel_CampScroll, 20, 70, 325, 310, NO_TEXT, fnt_Antiqua, taLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := TKMButton.Create(Panel_Campaign, aParent.Width-220-20, aParent.Height-50, 220, 30, gResTexts[TX_MENU_START_MISSION], bsMenu);
  Button_CampaignStart.Anchors := [akLeft,akBottom];
  Button_CampaignStart.OnClick := StartClick;

  Button_CampaignBack := TKMButton.Create(Panel_Campaign, 20, aParent.Height-50, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
  Button_CampaignBack.Anchors := [akLeft,akBottom];
  Button_CampaignBack.OnClick := BackClick;
end;


procedure TKMMenuCampaign.Campaign_Set(aCampaign: TKMCampaign);
const MapPic: array [Boolean] of byte = (10, 11);
var I: Integer;
begin
  fCampaign := aCampaign;

  //Choose background
  Image_CampaignBG.RX := fCampaign.BackGroundPic.RX;
  Image_CampaignBG.TexID := fCampaign.BackGroundPic.ID;

  //Setup sites
  for I := 0 to High(Image_CampaignFlags) do
  begin
    Image_CampaignFlags[I].Visible := I < fCampaign.MapCount;
    Image_CampaignFlags[I].TexID   := MapPic[I <= fCampaign.UnlockedMap];
    Image_CampaignFlags[I].HighlightOnMouseOver := I <= fCampaign.UnlockedMap;
  end;

  //Place sites
  for I := 0 to fCampaign.MapCount - 1 do
  begin
    //Pivot flags around Y=bottom X=middle, that's where the flag pole is
    Image_CampaignFlags[I].Left := fCampaign.Maps[I].Flag.X - Round((Image_CampaignFlags[I].Width/2)*(1-Panel_Campaign_Flags.Scale));
    Image_CampaignFlags[I].Top  := fCampaign.Maps[I].Flag.Y - Round(Image_CampaignFlags[I].Height   *(1-Panel_Campaign_Flags.Scale));
  end;

  //Select last map to play by 'clicking' last node
  Campaign_SelectMap(Image_CampaignFlags[fCampaign.UnlockedMap]);

  //When opening campaign screen set the scroll initial position properly
  //Player can move it later (to allow to select previous maps and look at camp map)
  Panel_CampScroll.Left := IfThen(fCampaign.Maps[fMapIndex].TextPos = bcBottomRight, Panel_Campaign.Width - Panel_CampScroll.Width, 0);
end;


procedure TKMMenuCampaign.Campaign_SelectMap(Sender: TObject);
var
  I: Integer;
begin
  if not TKMImage(Sender).HighlightOnMouseOver then exit; //Skip closed maps

  fMapIndex := TKMImage(Sender).Tag;

  //Place highlight
  for I := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[I].Highlight := (fMapIndex = I);

  //Connect by sub-nodes
  for I := 0 to High(Image_CampaignSubNode) do
  begin
    Image_CampaignSubNode[I].Visible := InRange(I, 0, fCampaign.Maps[fMapIndex].NodeCount-1);
    Image_CampaignSubNode[I].Left := fCampaign.Maps[fMapIndex].Nodes[I].X;
    Image_CampaignSubNode[I].Top  := fCampaign.Maps[fMapIndex].Nodes[I].Y;
  end;

  Label_CampaignTitle.Caption := Format(gResTexts[TX_GAME_MISSION], [fMapIndex+1]);
  Label_CampaignText.Caption := fCampaign.MissionBriefing(fMapIndex);

  //Add offset from top and space on bottom to fit buttons
  Panel_CampScroll.Height := Label_CampaignText.Top + Label_CampaignText.TextSize.Y + 70;
  Panel_CampScroll.Top := Panel_Campaign.Height - Panel_CampScroll.Height;

  fGameApp.MusicLib.StopPlayingOtherFile; //Stop playing the previous breifing even if this one doesn't exist
  fGameApp.PauseMusicToPlayFile(fCampaign.BreifingAudioFile(fMapIndex));
end;


procedure TKMMenuCampaign.StartClick(Sender: TObject);
begin
  fGameApp.MusicLib.StopPlayingOtherFile;
  fGameApp.NewCampaignMap(fCampaign, fMapIndex);
end;


procedure TKMMenuCampaign.Resize(X, Y: Word);
var
  I: Integer;
begin
  //Special rules for resizing the campaigns panel
  Panel_Campaign_Flags.Scale := Min(768,Y) / 768;
  Panel_Campaign_Flags.Left := Round(1024*(1-Panel_Campaign_Flags.Scale) / 2);
  Image_CampaignBG.Left := Round(1024*(1-Panel_Campaign_Flags.Scale) / 2);
  Image_CampaignBG.Height := Min(768,Y);
  Image_CampaignBG.Width := Round(1024*Panel_Campaign_Flags.Scale);
  //Special rule to keep campaign flags pivoted at the right place (so the flagpole doesn't move when you resize)
  if fCampaign <> nil then
    for I := 0 to fCampaign.MapCount - 1 do
      with Image_CampaignFlags[I] do
      begin
        //Pivot flags around Y=bottom X=middle, that's where the flag pole is
        Left := fCampaign.Maps[I].Flag.X - Round((Width/2)*(1-Panel_Campaign_Flags.Scale));
        Top  := fCampaign.Maps[I].Flag.Y - Round(Height   *(1-Panel_Campaign_Flags.Scale));
      end;
end;


//Mission description jumps around to allow to pick any of beaten maps
procedure TKMMenuCampaign.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  //If cursor hits the description, toggle it between left/right corner
  if InRange(Y - Panel_CampScroll.AbsTop, 0, Panel_CampScroll.Height)
  and InRange(X - Panel_CampScroll.AbsLeft, 0, Panel_CampScroll.Width) then
    Panel_CampScroll.Left := Panel_Campaign.Width - Panel_CampScroll.Width - Panel_CampScroll.Left;
end;


procedure TKMMenuCampaign.Show(aCampaign: TKMCampaignId);
begin
  Campaign_Set(fGameApp.Campaigns.CampaignById(aCampaign));

  //Refresh;
  Panel_Campaign.Show;
end;


procedure TKMMenuCampaign.BackClick(Sender: TObject);
begin
  fGameApp.MusicLib.StopPlayingOtherFile; //Cancel briefing if it was playing

  fOnPageChange(gpCampSelect);
end;


end.
