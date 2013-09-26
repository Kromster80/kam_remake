unit KM_GUIMapEdMarkerReveal;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics, KM_InterfaceDefaults;


type
  TKMMapEdMarkerReveal = class
  private
    fOwner: THandIndex;
    fIndex: Integer;
    fOnDone: TNotifyEvent;
    procedure Marker_Change(Sender: TObject);
  protected
    Panel_MarkerReveal: TKMPanel;
    Label_MarkerType: TKMLabel;
    Image_MarkerPic: TKMImage;
    TrackBar_RevealSize: TKMTrackBar;
    Button_RevealDelete: TKMButton;
    Button_RevealClose: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    property Index: Integer read fIndex;
    property Owner: THandIndex read fOwner;

    procedure Show(aPlayer: THandIndex; aIndex: Integer);
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_CommonClasses, KM_HandsCollection, KM_ResTexts, KM_Game,
  KM_RenderUI, KM_ResFonts;


{ TKMMapEdMarkerReveal }
constructor TKMMapEdMarkerReveal.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_MarkerReveal := TKMPanel.Create(aParent, 0, 50, TB_WIDTH, 400);

  Label_MarkerType := TKMLabel.Create(Panel_MarkerReveal, 0, 10, TB_WIDTH, 0, '', fnt_Outline, taCenter);
  Image_MarkerPic := TKMImage.Create(Panel_MarkerReveal, 0, 10, 32, 32, 338);

  TrackBar_RevealSize := TKMTrackBar.Create(Panel_MarkerReveal, 0, 45, TB_WIDTH, 1, 64);
  TrackBar_RevealSize.Caption := gResTexts[TX_MAPED_FOG_RADIUS];
  TrackBar_RevealSize.OnChange := Marker_Change;

  Button_RevealDelete := TKMButton.Create(Panel_MarkerReveal, 0, 100, 25, 25, 340, rxGui, bsGame);
  Button_RevealDelete.Hint := gResTexts[TX_MAPED_DELETE_REVEALER_HINT];
  Button_RevealDelete.OnClick := Marker_Change;

  Button_RevealClose := TKMButton.Create(Panel_MarkerReveal, TB_WIDTH-100, 100, 100, 25, gResTexts[TX_MAPED_CLOSE], bsGame);
  Button_RevealClose.Hint := gResTexts[TX_MAPED_CLOSE_REVEALER_HINT];
  Button_RevealClose.OnClick := Marker_Change;
end;


procedure TKMMapEdMarkerReveal.Marker_Change(Sender: TObject);
var
  Rev: TKMPointTagList;
begin
  //Shortcut to structure we update
  Rev := fGame.MapEditor.Revealers[fOwner];

  if Sender = TrackBar_RevealSize then
    Rev.Tag[fIndex] := TrackBar_RevealSize.Position;

  if Sender = Button_RevealDelete then
  begin
    Rev.Delete(fIndex);
    Hide;
    fOnDone(Self);
  end;

  if Sender = Button_RevealClose then
  begin
    Hide;
    fOnDone(Self);
  end;
end;


procedure TKMMapEdMarkerReveal.Show(aPlayer: THandIndex; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Image_MarkerPic.FlagColor := gHands[fOwner].FlagColor;

  Label_MarkerType.Caption := gResTexts[TX_MAPED_FOG];
  Image_MarkerPic.TexID := 393;
  TrackBar_RevealSize.Position := fGame.MapEditor.Revealers[fOwner].Tag[fIndex];

  Panel_MarkerReveal.Show;
end;


procedure TKMMapEdMarkerReveal.Hide;
begin
  Panel_MarkerReveal.Hide;
end;


function TKMMapEdMarkerReveal.Visible: Boolean;
begin
  Result := Panel_MarkerReveal.Visible;
end;


end.
