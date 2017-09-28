unit KM_GUIMapEdMessage;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, SysUtils,
   KM_Controls, KM_Defaults;

type
  TKMMapEdMessage = class
  private
    procedure Message_Close(Sender: TObject);
  protected
    Panel_Message: TKMPanel;
    Label_Message: TKMLabel;
    Image_MessageClose: TKMImage;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show; overload;
    procedure Show(const aText: UnicodeString); overload;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_RenderUI, KM_Sound, KM_ResSound, KM_ResFonts;


{ TKMMapEdMessage }
constructor TKMMapEdMessage.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Message := TKMPanel.Create(aParent, TOOLBAR_WIDTH+30, aParent.Height - 190, 600, 190);
  Panel_Message.Anchors := [anLeft, anBottom];
  Panel_Message.Hide;

  with TKMImage.Create(Panel_Message, 0, 0, 600, 190, 409) do
  begin
    Anchors := [anLeft, anTop, anBottom];
    ImageAnchors := [anLeft, anRight, anTop];
  end;

  Image_MessageClose := TKMImage.Create(Panel_Message, 600 - 76, 24, 32, 32, 52);
  Image_MessageClose.Anchors := [anTop, anRight];
  Image_MessageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
  Image_MessageClose.OnClick := Message_Close;
  Image_MessageClose.HighlightOnMouseOver := True;

  Label_Message := TKMLabel.Create(Panel_Message, 40, 60, 7000, 0, '', fnt_Grey, taLeft);
end;


procedure TKMMapEdMessage.Message_Close(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMessage.Hide;
begin
  gSoundPlayer.Play(sfxn_MPChatClose);
  Panel_Message.Hide;
end;


procedure TKMMapEdMessage.Show;
begin
  gSoundPlayer.Play(sfxn_MPChatOpen);
  Panel_Message.Show;
end;


procedure TKMMapEdMessage.Show(const aText: UnicodeString);
begin
  Label_Message.Caption := aText;
  Show;
end;


function TKMMapEdMessage.Visible: Boolean;
begin
  Result := Panel_Message.Visible;
end;


end.
