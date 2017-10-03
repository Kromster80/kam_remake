unit KM_GUIMapEdMenuResize;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils, KM_Controls;


type
  TKMMapEdMenuResize = class
  private
    fOnDone: TNotifyEvent;
    fOnPageChange: TNotifyEvent;
    fIsMultiplayer: Boolean;

    procedure ResizeRefresh(Sender: TObject);
    procedure PanelConfirm_Switch(Sender: TObject);
    procedure Resize_Click(Sender: TObject);
    procedure Menu_Click(Sender: TObject);
  protected
    Panel_Resize: TKMPanel;
      Panel_Resize_Edit: TKMPanel;
        NumEdit_Resize_Left, NumEdit_Resize_Right,
        NumEdit_Resize_Top, NumEdit_Resize_Bottom: TKMNumericEdit;
        Button_Resize: TKMButton;
        Button_Cancel: TKMButton;
        Label_CurrentMapSize, Label_NewMapSize: TKMLabel;
      Panel_Resize_Confirm: TKMPanel;
        Label_Resize_Confirm: TKMLabel;
        Button_Resize_Confirm_Yes, Button_Resize_Confirm_No: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone, aOnPageChange: TNotifyEvent);

    procedure SetLoadMode(aMultiplayer: Boolean);
    function Visible: Boolean;
    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KromUtils, Math, KM_Defaults, KM_GameApp, KM_Game, KM_Terrain, KM_InterfaceDefaults, KM_RenderAux,
  KM_InterfaceGame, KM_ResFonts, KM_RenderUI, KM_Points, KM_Maps, KM_ResTexts;


{ TKMMapEdMenuSave }
constructor TKMMapEdMenuResize.Create(aParent: TKMPanel; aOnDone, aOnPageChange: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;
  fOnPageChange := aOnPageChange;

  Panel_Resize := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);
    Panel_Resize_Edit := TKMPanel.Create(Panel_Resize, 0, 0, Panel_Resize.Width, Panel_Resize.Height);

      TKMLabel.Create(Panel_Resize_Edit, 0, PAGE_TITLE_Y, TB_WIDTH, 30, 'Move borders:', fnt_Outline, taCenter); //Todo translate

      // Use left-top-right-bottom order of creation. Same order will be used for Tab focus change
      NumEdit_Resize_Left   := TKMNumericEdit.Create(Panel_Resize_Edit, 0, 55, -224, 224);
      NumEdit_Resize_Top    := TKMNumericEdit.Create(Panel_Resize_Edit, 50, 25, -224, 224);
      NumEdit_Resize_Right  := TKMNumericEdit.Create(Panel_Resize_Edit, 100, 55, -224, 224);
      NumEdit_Resize_Bottom := TKMNumericEdit.Create(Panel_Resize_Edit, 50, 85, -224, 224);

      NumEdit_Resize_Left.OnChange    := ResizeRefresh;
      NumEdit_Resize_Right.OnChange   := ResizeRefresh;
      NumEdit_Resize_Top.OnChange     := ResizeRefresh;
      NumEdit_Resize_Bottom.OnChange  := ResizeRefresh;

      Label_CurrentMapSize := TKMLabel.Create(Panel_Resize_Edit, 0, 115, TB_WIDTH, 30, '', fnt_Outline, taCenter);
      Label_NewMapSize := TKMLabel.Create(Panel_Resize_Edit, 0, 160, TB_WIDTH, 30, '', fnt_Outline, taCenter);

      Button_Resize := TKMButton.Create(Panel_Resize_Edit, 0, 205, TB_WIDTH, 30, 'Resize and Save', bsGame); //Todo translate
      Button_Resize.Hint := 'Resize map and then save it'; //Todo translate
      Button_Resize.OnClick := PanelConfirm_Switch;
      Button_Resize.Disable;

      Button_Cancel := TKMButton.Create(Panel_Resize_Edit, 0, 250, TB_WIDTH, 30, 'Cancel', bsGame); //Todo translate
      Button_Cancel.OnClick   := Menu_Click;

    Panel_Resize_Confirm := TKMPanel.Create(Panel_Resize, 0, 0, Panel_Resize.Width, Panel_Resize.Height);
      Label_Resize_Confirm := TKMLabel.Create(Panel_Resize_Confirm, 0, 0, TB_WIDTH, 20, '', fnt_Outline, taCenter); //Todo translate
      Label_Resize_Confirm.AutoWrap := True;

      Button_Resize_Confirm_Yes := TKMButton.Create(Panel_Resize_Confirm, 0, Max(120, Label_Resize_Confirm.TextSize.Y + 10), TB_WIDTH, 30, 'Yes', bsGame);
      Button_Resize_Confirm_Yes.Hint := 'Resize map and then save it'; //Todo translate
      Button_Resize_Confirm_No := TKMButton.Create(Panel_Resize_Confirm, 0, Max(160, Label_Resize_Confirm.TextSize.Y + 50), TB_WIDTH, 30, 'No', bsGame);
      Button_Resize_Confirm_No.Hint := 'Go previous menu'; //Todo translate

      Button_Resize_Confirm_Yes.OnClick := Resize_Click;
      Button_Resize_Confirm_No.OnClick := PanelConfirm_Switch;
end;


procedure TKMMapEdMenuResize.Menu_Click(Sender: TObject);
begin
  fOnDone(Self);
  fOnPageChange(Self);
end;


procedure TKMMapEdMenuResize.ResizeRefresh(Sender: TObject);
var
  NewMapX, NewMapY: SmallInt;
begin
  Button_Resize.Enabled := (NumEdit_Resize_Left.Value   <> 0)
                        or (NumEdit_Resize_Right.Value  <> 0)
                        or (NumEdit_Resize_Top.Value    <> 0)
                        or (NumEdit_Resize_Bottom.Value <> 0);

  // Calc num edits range restrictions
  NumEdit_Resize_Left.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapX - NumEdit_Resize_Right.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Left.ValueMin := EnsureRange(-(gTerrain.MapX - MIN_MAP_SIZE + NumEdit_Resize_Right.Value), -MAX_MAP_SIZE, 0);
  NumEdit_Resize_Right.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapX - NumEdit_Resize_Left.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Right.ValueMin := EnsureRange(-(gTerrain.MapX - MIN_MAP_SIZE + NumEdit_Resize_Left.Value), -MAX_MAP_SIZE, 0);

  NumEdit_Resize_Top.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapY - NumEdit_Resize_Bottom.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Top.ValueMin := EnsureRange(-(gTerrain.MapY - MIN_MAP_SIZE + NumEdit_Resize_Bottom.Value), -MAX_MAP_SIZE, 0);
  NumEdit_Resize_Bottom.ValueMax := EnsureRange(MAX_MAP_SIZE - gTerrain.MapY - NumEdit_Resize_Top.Value, 0, MAX_MAP_SIZE);
  NumEdit_Resize_Bottom.ValueMin := EnsureRange(-(gTerrain.MapY - MIN_MAP_SIZE + NumEdit_Resize_Top.Value), -MAX_MAP_SIZE, 0);

  gGame.MapEditor.ResizeMapRect.Left := Abs(Min(0, NumEdit_Resize_Left.Value)) + 1;
  gGame.MapEditor.ResizeMapRect.Top := Abs(Min(0, NumEdit_Resize_Top.Value)) + 1;
  gGame.MapEditor.ResizeMapRect.Right := gTerrain.MapX - Abs(Min(0, NumEdit_Resize_Right.Value)) - 1;
  gGame.MapEditor.ResizeMapRect.Bottom := gTerrain.MapY - Abs(Min(0, NumEdit_Resize_Bottom.Value)) - 1;

  NewMapX := gTerrain.MapX + NumEdit_Resize_Left.Value + NumEdit_Resize_Right.Value;
  NewMapY := gTerrain.MapY + NumEdit_Resize_Top.Value + NumEdit_Resize_Bottom.Value;

  Label_CurrentMapSize.Caption := Format('Current map size:|[ %d : %d ]', [gTerrain.MapX, gTerrain.MapY]);
  Label_NewMapSize.Enabled := Button_Resize.Enabled;
  Label_NewMapSize.Caption := Format('New map size:|[ %d : %d ]',
                                    [EnsureRange(NewMapX, MIN_MAP_SIZE, MAX_MAP_SIZE),
                                     EnsureRange(NewMapY, MIN_MAP_SIZE, MAX_MAP_SIZE)]);
end;


procedure TKMMapEdMenuResize.Resize_Click(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := TKMapsCollection.FullPath(gGame.GameName, '.dat', fIsMultiplayer);
  gGame.SaveMapEditor(SaveName, KMRect(NumEdit_Resize_Left.Value, NumEdit_Resize_Top.Value, NumEdit_Resize_Right.Value, NumEdit_Resize_Bottom.Value));
  FreeThenNil(gGame);
  gGameApp.NewMapEditor(SaveName, 0, 0);
  gGame.MapEditor.TerrainPainter.RebuildMap(KMRect(1,1,gTerrain.MapX, gTerrain.MapY));
end;


procedure TKMMapEdMenuResize.Hide;
begin
  Panel_Resize.Hide;
end;


procedure TKMMapEdMenuResize.Show;
begin
  Panel_Resize_Confirm.Hide;
  Panel_Resize_Edit.Show;
  Panel_Resize.Show;
  ResizeRefresh(nil);
  fOnPageChange(Self);
end;


function TKMMapEdMenuResize.Visible: Boolean;
begin
  Result := Panel_Resize.Visible;
end;


procedure TKMMapEdMenuResize.PanelConfirm_Switch(Sender: TObject);
begin
  if Sender = Button_Resize then
  begin
    Panel_Resize_Edit.Hide;
    Panel_Resize_Confirm.Show;
    if not gGame.MapEditor.IsNewMap or gGame.MapEditor.WereSaved then
    begin
      Label_Resize_Confirm.Caption := 'Are you sure want to resize the map?||It will automatically override current map';
      Button_Resize_Confirm_Yes.Visible := True;
      Button_Resize_Confirm_No.Caption := 'No'; //Todo translate
    end else begin
      Label_Resize_Confirm.Caption := 'Resize is not available||You need to save new map first';
      Button_Resize_Confirm_Yes.Hide;
      Button_Resize_Confirm_No.Caption := gResTexts[TX_MENU_TAB_HINT_GO_BACK]; //Todo translate
    end;
  end
  else
  if Sender = Button_Resize_Confirm_No then
  begin
    Panel_Resize_Confirm.Hide;
    Panel_Resize_Edit.Show;
  end;
end;


procedure TKMMapEdMenuResize.SetLoadMode(aMultiplayer: Boolean);
begin
  fIsMultiplayer := aMultiplayer;
end;


end.
