unit KM_GUIMapEdMenuSave;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Maps;


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
    Button_SaveSave: TKMButton;
    Button_SaveCancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_Game, KM_RenderUI, KM_ResFonts, KM_ResTexts, KM_InterfaceGame;


{ TKMMapEdMenuSave }
constructor TKMMapEdMenuSave.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_Save := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);
  TKMBevel.Create(Panel_Save, 0, 30, TB_WIDTH, 37);
  Radio_Save_MapType  := TKMRadioGroup.Create(Panel_Save,4,32,TB_WIDTH,35,fnt_Grey);
  Radio_Save_MapType.ItemIndex := 0;
  Radio_Save_MapType.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
  Radio_Save_MapType.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
  Radio_Save_MapType.OnChange := Menu_SaveClick;
  TKMLabel.Create(Panel_Save,0,90,TB_WIDTH,20,gResTexts[TX_MAPED_SAVE_TITLE],fnt_Outline,taCenter);
  Edit_SaveName       := TKMEdit.Create(Panel_Save,0,110,TB_WIDTH,20, fnt_Grey);
  Edit_SaveName.AllowedChars := acFileName;
  Label_SaveExists    := TKMLabel.Create(Panel_Save,0,140,TB_WIDTH,0,gResTexts[TX_MAPED_SAVE_EXISTS],fnt_Outline,taCenter);
  CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,160,TB_WIDTH,20,gResTexts[TX_MAPED_SAVE_OVERWRITE], fnt_Metal);
  Button_SaveSave     := TKMButton.Create(Panel_Save,0,180,TB_WIDTH,30,gResTexts[TX_MAPED_SAVE],bsGame);
  Button_SaveCancel   := TKMButton.Create(Panel_Save,0,220,TB_WIDTH,30,gResTexts[TX_MAPED_SAVE_CANCEL],bsGame);
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
    Button_SaveSave.Enabled := not CheckBox_SaveExists.Enabled;
  end;

  if Sender = CheckBox_SaveExists then
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;

  if Sender = Button_SaveSave then
  begin
    gGame.SaveMapEditor(SaveName);

    //Player colors and mapname has changed
    gGame.ActiveInterface.SyncUI;

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
  if gGame.MapEditor.HumanCount > 1 then
    Radio_Save_MapType.ItemIndex := 1
  else
    Radio_Save_MapType.ItemIndex := 0;

  Edit_SaveName.Text := gGame.GameName;
  Menu_SaveClick(Edit_SaveName);
  Panel_Save.Show;
end;


end.
