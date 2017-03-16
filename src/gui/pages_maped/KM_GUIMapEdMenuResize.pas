unit KM_GUIMapEdMenuResize;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils, KM_Controls;


type
  TKMMapEdMenuResize = class
  private
    fOnDone: TNotifyEvent;
    fIsMultiplayer: Boolean;

    procedure ResizeRefresh(Sender: TObject);
    procedure Resize_Click(Sender: TObject);
    procedure Menu_Click(Sender: TObject);
  protected
    Panel_Resize: TKMPanel;
      NumEdit_Resize_Left, NumEdit_Resize_Right,
      NumEdit_Resize_Top, NumEdit_Resize_Bottom: TKMNumericEdit;
      Button_Resize: TKMButton;
      Button_Cancel: TKMButton;
      Label_CurrentMapSize, Label_NewMapSize: TKMLabel;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    procedure SetLoadMode(aMultiplayer: Boolean);
    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KromUtils, Math, KM_Defaults, KM_GameApp, KM_Game, KM_Terrain, KM_InterfaceDefaults,
  KM_InterfaceGame, KM_ResFonts, KM_RenderUI, KM_Points, KM_Maps;


{ TKMMapEdMenuSave }
constructor TKMMapEdMenuResize.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_Resize := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);
    TKMLabel.Create(Panel_Resize, 0, PAGE_TITLE_Y, TB_WIDTH, 30, 'Move borders:', fnt_Outline, taCenter); //Todo translate

    NumEdit_Resize_Left   := TKMNumericEdit.Create(Panel_Resize, 0, 55, -224, 224);
    NumEdit_Resize_Right  := TKMNumericEdit.Create(Panel_Resize, 100, 55, -224, 224);
    NumEdit_Resize_Top    := TKMNumericEdit.Create(Panel_Resize, 50, 25, -224, 224);
    NumEdit_Resize_Bottom := TKMNumericEdit.Create(Panel_Resize, 50, 85, -224, 224);

    NumEdit_Resize_Left.OnChange    := ResizeRefresh;
    NumEdit_Resize_Right.OnChange   := ResizeRefresh;
    NumEdit_Resize_Top.OnChange     := ResizeRefresh;
    NumEdit_Resize_Bottom.OnChange  := ResizeRefresh;

    Label_CurrentMapSize := TKMLabel.Create(Panel_Resize, 0, 115, TB_WIDTH, 30, '', fnt_Outline, taCenter);
    Label_NewMapSize := TKMLabel.Create(Panel_Resize, 0, 155, TB_WIDTH, 30, '', fnt_Outline, taCenter);

    Button_Resize := TKMButton.Create(Panel_Resize, 0, 195, TB_WIDTH, 30, 'Resize', bsGame); //Todo translate
    Button_Resize.OnClick   := Resize_Click;
    Button_Resize.Disable;

    Button_Cancel := TKMButton.Create(Panel_Resize, 0, 250, TB_WIDTH, 30, 'Cancel', bsGame); //Todo translate
    Button_Cancel.OnClick   := Menu_Click;
end;



procedure TKMMapEdMenuResize.Hide;
begin
  Panel_Resize.Hide;
end;


procedure TKMMapEdMenuResize.Menu_Click(Sender: TObject);
begin
  if Sender = Button_Cancel then
    fOnDone(Self);
end;


procedure TKMMapEdMenuResize.ResizeRefresh(Sender: TObject);
var
  NewMapX, NewMapY: SmallInt;
begin
  Button_Resize.Enabled := (NumEdit_Resize_Left.Value <> 0)
                        or (NumEdit_Resize_Right.Value <> 0)
                        or (NumEdit_Resize_Top.Value <> 0)
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

  NewMapX := gTerrain.MapX + NumEdit_Resize_Left.Value + NumEdit_Resize_Right.Value;
  NewMapY := gTerrain.MapY + NumEdit_Resize_Top.Value + NumEdit_Resize_Bottom.Value;

  Label_CurrentMapSize.Caption := Format('Current map size:|[%d:%d]', [gTerrain.MapX, gTerrain.MapY]); //Todo translate
  Label_NewMapSize.Enabled := Button_Resize.Enabled;
  Label_NewMapSize.Caption := Format('New map size:|[%d:%d]', //Todo translate
                                    [EnsureRange(NewMapX, MIN_MAP_SIZE, MAX_MAP_SIZE),
                                     EnsureRange(NewMapY, MIN_MAP_SIZE, MAX_MAP_SIZE)]);
end;


procedure TKMMapEdMenuResize.Resize_Click(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := TKMapsCollection.FullPath(gGame.GameName, '.dat', fIsMultiplayer);
  gGame.SaveMapEditor(SaveName, KMRect(NumEdit_Resize_Left.Value, NumEdit_Resize_Top.Value, NumEdit_Resize_Right.Value, NumEdit_Resize_Bottom.Value));
//  Label_CurrentMapSize.Caption := 'Saved';
  FreeThenNil(gGame);
  gGameApp.NewMapEditor(SaveName, 0, 0);
end;


procedure TKMMapEdMenuResize.Show;
begin
  Panel_Resize.Show;
  ResizeRefresh(nil);
end;


procedure TKMMapEdMenuResize.SetLoadMode(aMultiplayer: Boolean);
begin
  fIsMultiplayer := aMultiplayer;
end;


end.
