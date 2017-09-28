unit KM_GUIMenuError;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, SysUtils,
  KM_Controls, KM_InterfaceDefaults;


type
  TKMMenuError = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText;
    procedure BackClick(Sender: TObject);
  protected
    Panel_Error: TKMPanel;
    Label_Error: TKMLabel;
    Button_ErrorBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    procedure Show(const aText: UnicodeString);
  end;


implementation
uses
  KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMGUIMenuError }
constructor TKMMenuError.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;

  Panel_Error := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Error.AnchorsStretch;
    TKMLabel.Create(Panel_Error, aParent.Width div 2, aParent.Height div 2 - 20, gResTexts[TX_MENU_ERROR], fnt_Antiqua, taCenter).AnchorsCenter;
    Label_Error := TKMLabel.Create(Panel_Error, 8, aParent.Height div 2+10, aParent.Width-16, 200, '...', fnt_Grey, taCenter);
    Label_Error.AnchorsCenter;
    Label_Error.AutoWrap := True;
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,630,224,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_ErrorBack.AnchorsCenter;
    Button_ErrorBack.OnClick := BackClick;
end;


procedure TKMMenuError.BackClick(Sender: TObject);
begin
  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuError.Show(const aText: UnicodeString);
begin
  Label_Error.Caption := aText;
  Panel_Error.Show;
end;


end.
