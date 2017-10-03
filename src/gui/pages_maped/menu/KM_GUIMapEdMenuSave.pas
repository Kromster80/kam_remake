unit KM_GUIMapEdMenuSave;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Maps, KM_InterfaceGame, KM_CommonTypes;


type
  TKMMapEdMenuSave = class
  private
    fOnDone: TNotifyEvent;

    procedure Menu_SaveClick(Sender: TObject);
  protected
    Panel_Save: TKMPanel;
      Radio_Save_MapType: TKMRadioGroup;
      Edit_SaveName: TKMEdit;
      Label_SaveExists: TKMLabel;
      CheckBox_SaveExists: TKMCheckBox;
  public
    OnChangeMapType: TBooleanEvent;
    Button_SaveSave: TKMButton;
    Button_SaveCancel: TKMButton;
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent; aTopPanelInset: Integer = 45; aControlsWidth: Integer = TB_WIDTH);

    procedure SetLoadMode(aMultiplayer:boolean);
    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_Game, KM_RenderUI, KM_ResFonts, KM_ResTexts, KM_InterfaceDefaults;


{ TKMMapEdMenuSave }
constructor TKMMapEdMenuSave.Create(aParent: TKMPanel; aOnDone: TNotifyEvent; aTopPanelInset: Integer = 45; aControlsWidth: Integer = TB_WIDTH);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_Save := TKMPanel.Create(aParent, 0, aTopPanelInset, aControlsWidth, 230);
  TKMLabel.Create(Panel_Save,0,0,aControlsWidth,20,gResTexts[TX_MAPED_SAVE_TITLE],fnt_Outline,taLeft);

  TKMBevel.Create(Panel_Save, 0, 25, aControlsWidth, 37);
  Radio_Save_MapType  := TKMRadioGroup.Create(Panel_Save,4,27,aControlsWidth,35,fnt_Grey);
  Radio_Save_MapType.ItemIndex := 0;
  Radio_Save_MapType.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
  Radio_Save_MapType.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
  Radio_Save_MapType.OnChange := Menu_SaveClick;

  Edit_SaveName       := TKMEdit.Create(Panel_Save,0,80,aControlsWidth,20, fnt_Grey);
  Edit_SaveName.MaxLen := MAX_SAVENAME_LENGTH;
  Edit_SaveName.AllowedChars := acFileName;
  Label_SaveExists    := TKMLabel.Create(Panel_Save,0,110,aControlsWidth,0,gResTexts[TX_MAPED_SAVE_EXISTS],fnt_Outline,taCenter);
  CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,130,aControlsWidth,20,gResTexts[TX_MAPED_SAVE_OVERWRITE], fnt_Metal);
  Button_SaveSave     := TKMButton.Create(Panel_Save,0,150,aControlsWidth,30,gResTexts[TX_MAPED_SAVE],bsGame);
  Button_SaveCancel   := TKMButton.Create(Panel_Save,0,190,aControlsWidth,30,gResTexts[TX_MAPED_SAVE_CANCEL],bsGame);
  Edit_SaveName.OnChange      := Menu_SaveClick;
  CheckBox_SaveExists.OnClick := Menu_SaveClick;
  Button_SaveSave.OnClick     := Menu_SaveClick;
  Button_SaveCancel.OnClick   := Menu_SaveClick;
end;


procedure TKMMapEdMenuSave.Menu_SaveClick(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := TKMapsCollection.FullPath(Trim(Edit_SaveName.Text), '.dat', Radio_Save_MapType.ItemIndex = 1);

  if (Sender = Edit_SaveName) or (Sender = Radio_Save_MapType) then
  begin
    CheckBox_SaveExists.Enabled := FileExists(SaveName);
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    Button_SaveSave.Enabled := not CheckBox_SaveExists.Enabled and (Length(Trim(Edit_SaveName.Text)) > 0);
  end;

  if (Sender = Radio_Save_MapType) and Assigned(OnChangeMapType) then
    OnChangeMapType(Radio_Save_MapType.ItemIndex = 1);

  if Sender = CheckBox_SaveExists then
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;

  if Sender = Button_SaveSave then
  begin
    gGame.SaveMapEditor(SaveName);
    gGame.MapEditor.WereSaved := True;

    //Player colors and mapname has changed
    gGame.ActiveInterface.SyncUI(False); //Don't move the viewport

    fOnDone(Self);
  end;

  if Sender = Button_SaveCancel then
    fOnDone(Self);
end;


procedure TKMMapEdMenuSave.Hide;
begin
  Panel_Save.Hide;
end;


procedure TKMMapEdMenuSave.Show;
begin
  Edit_SaveName.Text := gGame.GameName;
  Menu_SaveClick(Edit_SaveName);
  Panel_Save.Show;
end;


procedure TKMMapEdMenuSave.SetLoadMode(aMultiplayer: Boolean);
begin
  if aMultiplayer then
    Radio_Save_MapType.ItemIndex := 1
  else
    Radio_Save_MapType.ItemIndex := 0;
end;


end.
