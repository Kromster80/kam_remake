unit KM_GUIMenuCredits;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF WDC} ShellAPI, Windows, {$ENDIF} //Required for OpenURL in Delphi
  {$IFDEF FPC} LCLIntf, {$ENDIF} //Required for OpenURL in Lazarus
  Forms, Controls, KromUtils,
  KM_Controls, KM_Defaults,
  KM_InterfaceDefaults;


type
  TKMMenuCredits = class {(TKMGUIPage)}
  private
    fOnPageChange: TGUIEventText;

    procedure LinkClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_Credits: TKMPanel;
    Label_Credits_KaM: TKMLabelScroll;
    Label_Credits_Remake: TKMLabelScroll;
    Button_CreditsHomepage: TKMButton;
    Button_CreditsFacebook: TKMButton;
    Button_CreditsBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show;
  end;


implementation
uses KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_ResLocales;


{ TKMGUIMainCredits }
constructor TKMMenuCredits.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
const
  OFFSET = 312;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Credits := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Credits.Stretch;

    TKMLabel.Create(Panel_Credits, aParent.Width div 2 - OFFSET, 70, gResTexts[TX_CREDITS],fnt_Outline,taCenter);
    Label_Credits_Remake := TKMLabelScroll.Create(Panel_Credits, aParent.Width div 2 - OFFSET, 110, 0, aParent.Height - 130,
    gResTexts[TX_CREDITS_PROGRAMMING]+'|Krom|Lewin||'+
    gResTexts[TX_CREDITS_ADDITIONAL_PROGRAMMING]+'|Alex|Danjb||'+
    gResTexts[TX_CREDITS_ADDITIONAL_GRAPHICS]+'|StarGazer|Malin|H.A.H.||'+
    gResTexts[TX_CREDITS_ADDITIONAL_MUSIC]+'|Andre Sklenar - www.juicelab.cz||'+
    gResTexts[TX_CREDITS_ADDITIONAL_SOUNDS]+'|trb1914||'+
    gResTexts[TX_CREDITS_ADDITIONAL_TRANSLATIONS]+'|'+gResLocales.TranslatorCredits+'|'+
    gResTexts[TX_CREDITS_SPECIAL]+'|KaM Community members'
    ,fnt_Grey,taCenter);
    Label_Credits_Remake.Anchors := [akLeft,akTop,akBottom];

    TKMLabel.Create(Panel_Credits, aParent.Width div 2 + OFFSET, 70, gResTexts[TX_CREDITS_ORIGINAL], fnt_Outline, taCenter);
    Label_Credits_KaM := TKMLabelScroll.Create(Panel_Credits, aParent.Width div 2 + OFFSET, 110, 0, aParent.Height - 130, gResTexts[TX_CREDITS_TEXT], fnt_Grey, taCenter);
    Label_Credits_KaM.Anchors := [akLeft,akTop,akBottom];

    Button_CreditsHomepage:=TKMButton.Create(Panel_Credits,400,610,224,30,'[$F8A070]www.kamremake.com[]',bsMenu);
    Button_CreditsHomepage.Anchors := [akLeft,akBottom];
    Button_CreditsHomepage.OnClick := LinkClick;

    Button_CreditsFacebook:=TKMButton.Create(Panel_Credits,400,646,224,30,'[$F8A070]Facebook[]',bsMenu);
    Button_CreditsFacebook.Anchors := [akLeft,akBottom];
    Button_CreditsFacebook.OnClick := LinkClick;

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,400,700,224,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_CreditsBack.Anchors := [akLeft,akBottom];
    Button_CreditsBack.OnClick := BackClick;
end;


procedure TKMMenuCredits.LinkClick(Sender: TObject);

  //This can't be moved to e.g. KM_Utils because the dedicated server needs that, and it must be Linux compatible
  procedure GoToURL(aUrl: string);
  begin
    {$IFDEF WDC}
    ShellExecute(Application.Handle, 'open', PChar(aUrl), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
    {$IFDEF FPC}
    OpenURL(aUrl);
    {$ENDIF}
  end;

begin
  if Sender = Button_CreditsHomepage then GoToURL('http://www.kamremake.com/redirect.php?page=homepage&rev='+GAME_REVISION);
  if Sender = Button_CreditsFacebook then GoToURL('http://www.kamremake.com/redirect.php?page=facebook&rev='+GAME_REVISION);
end;


procedure TKMMenuCredits.Show;
begin
  //Set initial position
  Label_Credits_KaM.SmoothScrollToTop := TimeGet;
  Label_Credits_Remake.SmoothScrollToTop := TimeGet;

  Panel_Credits.Show;
end;


procedure TKMMenuCredits.BackClick(Sender: TObject);
begin
  fOnPageChange(gpMainMenu);
end;


end.
