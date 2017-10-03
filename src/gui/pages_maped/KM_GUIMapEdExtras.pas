unit KM_GUIMapEdExtras;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, SysUtils,
   KM_Controls, KM_Defaults;

type
  TKMMapEdExtras = class
  private
    fOnChange: TNotifyEvent;
    procedure Extra_Change(Sender: TObject);
    procedure Extra_Close(Sender: TObject);
    procedure Extra_FOWChange(Sender: TObject);
  protected
    Panel_Extra: TKMPanel;
    Image_ExtraClose: TKMImage;
    TrackBar_Passability: TKMTrackBar;
    Label_Passability: TKMLabel;
    Dropbox_PlayerFOW: TKMDropList;
  public
    CheckBox_ShowObjects: TKMCheckBox;
    CheckBox_ShowHouses: TKMCheckBox;
    CheckBox_ShowUnits: TKMCheckBox;
    CheckBox_ShowDeposits: TKMCheckBox;
    CheckBox_ShowTileOwners: TKMCheckBox;
    CheckBox_ShowTilesGrid: TKMCheckBox;
    constructor Create(aParent: TKMPanel; aOnChange: TNotifyEvent);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_Sound, KM_ResSound,
  KM_RenderUI, KM_ResFonts, KM_ResTexts;


{ TKMMapEdExtras }
constructor TKMMapEdExtras.Create(aParent: TKMPanel; aOnChange: TNotifyEvent);
var
  I: Integer;
begin
  inherited Create;

  fOnChange := aOnChange;

  Panel_Extra := TKMPanel.Create(aParent, TOOLBAR_WIDTH+30, aParent.Height - 190, 600, 190);
  Panel_Extra.Anchors := [anLeft, anBottom];
  Panel_Extra.Hide;

  with TKMImage.Create(Panel_Extra, 0, 0, 600, 190, 409) do
  begin
    Anchors := [anLeft, anTop, anBottom];
    ImageAnchors := [anLeft, anRight, anTop];
  end;

  Image_ExtraClose := TKMImage.Create(Panel_Extra, 600 - 76, 24, 32, 32, 52);
  Image_ExtraClose.Anchors := [anTop, anRight];
  Image_ExtraClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
  Image_ExtraClose.OnClick := Extra_Close;
  Image_ExtraClose.HighlightOnMouseOver := True;

  TrackBar_Passability := TKMTrackBar.Create(Panel_Extra, 50, 70, 180, 0, Byte(High(TKMTerrainPassability)));
  TrackBar_Passability.Font := fnt_Antiqua;
  TrackBar_Passability.Caption := gResTexts[TX_MAPED_VIEW_PASSABILITY];
  TrackBar_Passability.Position := 0; //Disabled by default
  TrackBar_Passability.OnChange := Extra_Change;
  Label_Passability := TKMLabel.Create(Panel_Extra, 50, 114, 180, 0, gResTexts[TX_MAPED_PASSABILITY_OFF], fnt_Antiqua, taLeft);

  CheckBox_ShowObjects := TKMCheckBox.Create(Panel_Extra, 250, 70, 180, 20, gResTexts[TX_MAPED_VIEW_OBJECTS], fnt_Antiqua);
  CheckBox_ShowObjects.Checked := True; //Enabled by default
  CheckBox_ShowObjects.OnClick := Extra_Change;
  CheckBox_ShowHouses := TKMCheckBox.Create(Panel_Extra, 250, 90, 180, 20, gResTexts[TX_MAPED_VIEW_HOUSES], fnt_Antiqua);
  CheckBox_ShowHouses.Checked := True; //Enabled by default
  CheckBox_ShowHouses.OnClick := Extra_Change;
  CheckBox_ShowUnits := TKMCheckBox.Create(Panel_Extra, 250, 110, 180, 20, gResTexts[TX_MAPED_VIEW_UNITS], fnt_Antiqua);
  CheckBox_ShowUnits.Checked := True; //Enabled by default
  CheckBox_ShowUnits.OnClick := Extra_Change;
  CheckBox_ShowDeposits := TKMCheckBox.Create(Panel_Extra, 250, 130, 180, 20, gResTexts[TX_MAPED_VIEW_DEPOSISTS], fnt_Antiqua);
  CheckBox_ShowDeposits.Checked := True; //Enabled by default
  CheckBox_ShowDeposits.OnClick := Extra_Change;
  CheckBox_ShowTileOwners := TKMCheckBox.Create(Panel_Extra, 250, 150, 180, 20, 'Show tile owners', fnt_Antiqua); //Todo translate
  CheckBox_ShowTileOwners.Checked := False; //Disabled by default
  CheckBox_ShowTileOwners.OnClick := Extra_Change;

  CheckBox_ShowTilesGrid := TKMCheckBox.Create(Panel_Extra, 50, 150, 180, 20, 'Show tiles grid', fnt_Antiqua); //Todo translate
  CheckBox_ShowTilesGrid.Checked := False; //Disabled by default
  CheckBox_ShowTilesGrid.OnClick := Extra_Change;

  //dropdown list needs to be ontop other buttons created on Panel_Main
  Dropbox_PlayerFOW := TKMDropList.Create(Panel_Extra, 460, 70, 160, 20, fnt_Metal, '', bsGame);

  Dropbox_PlayerFOW.Add('Show all', -1);
  for I := 0 to MAX_HANDS - 1 do
    Dropbox_PlayerFOW.Add(Format(gResTexts[TX_PLAYER_X], [I]), I);

  Dropbox_PlayerFOW.Hint := gResTexts[TX_REPLAY_PLAYER_PERSPECTIVE];
  Dropbox_PlayerFOW.OnChange := Extra_FOWChange;
  //todo: This feature isn't working properly yet so it's hidden. FOW should be set by
  //revealers list and current locations of units/houses (must update when they move)
  Dropbox_PlayerFOW.Hide;
end;


procedure TKMMapEdExtras.Extra_Change(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES := TrackBar_Passability.Position <> 0;
  SHOW_TERRAIN_PASS := TrackBar_Passability.Position;

  if TrackBar_Passability.Position <> 0 then
    Label_Passability.Caption := PassabilityGuiText[TKMTerrainPassability(SHOW_TERRAIN_PASS)]
  else
    Label_Passability.Caption := gResTexts[TX_MAPED_PASSABILITY_OFF];

  fOnChange(Self);
end;


procedure TKMMapEdExtras.Extra_Close(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdExtras.Extra_FOWChange(Sender: TObject);
begin
  gMySpectator.FOWIndex := Dropbox_PlayerFOW.GetTag(Dropbox_PlayerFOW.ItemIndex);
  //fGame.Minimap.Update(False); //Force update right now so FOW doesn't appear to lag
end;


procedure TKMMapEdExtras.Hide;
begin
  gSoundPlayer.Play(sfxn_MPChatClose);
  Panel_Extra.Hide;
end;


procedure TKMMapEdExtras.Show;
begin
  gSoundPlayer.Play(sfxn_MPChatOpen);
  Panel_Extra.Show;
end;


function TKMMapEdExtras.Visible: Boolean;
begin
  Result := Panel_Extra.Visible;
end;


end.
