unit KM_GUIMapEdMenuQuickPlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, //SysUtils,
  KM_Controls, KM_Defaults, KM_GUIMapEdMenuSave;

type
  TKMMapEdMenuQuickPlay = class
  private
    fMenuSave: TKMMapEdMenuSave;
    fIsMultiplayer: Boolean;
    procedure Cancel_Click(Sender: TObject);
    procedure QuickPlay_Click(Sender: TObject);
    procedure Update_PlayerSelect;
    procedure PlayerSelectFirst;
    procedure UpdatePanel;
    procedure SaveDone(Sender: TObject);
    procedure SaveBtn_EnableStatusChanged(aValue: Boolean);
  protected
    PopUp_QuickPlay: TKMPopUpPanel;
      Panel_Save: TKMPanel;

      DropList_SelectHand: TKMDropList;
      Label_QuickPlay: TKMLabel;
      Button_QuickPlay, Button_Cancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);
    destructor Destroy; override;

    procedure MapTypeChanged(aIsMultiplayer: Boolean);
    procedure SetLoadMode(aMultiplayer: Boolean);
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
  end;


implementation
uses
  SysUtils, KromUtils, KM_GameApp, KM_Game, KM_HandsCollection, KM_Maps, KM_Hand, KM_InterfaceGame, KM_InterfaceGamePlay,
  KM_RenderUI, KM_ResFonts, KM_ResTexts, KM_Resource, Math;


constructor TKMMapEdMenuQuickPlay.Create(aParent: TKMPanel);
const
  ControlsWidth = 220;
var
  Left: Integer;
begin
  inherited Create;

  PopUp_QuickPlay := TKMPopUpPanel.Create(aParent, 240, 420, 'Quick Play'); //Todo translate

  PopUp_QuickPlay.Width := Math.Max(240, gRes.Fonts[PopUp_QuickPlay.Font].GetTextSize(PopUp_QuickPlay.Caption).X + 40);
  Left := (PopUp_QuickPlay.Width - ControlsWidth) div 2;
    TKMLabel.Create(PopUp_QuickPlay, PopUp_QuickPlay.Width div 2, 50, 'Select player:', fnt_Metal, taCenter); //Todo translate

    DropList_SelectHand := TKMDropList.Create(PopUp_QuickPlay, Left, 75, ControlsWidth, 20, fnt_Game, '', bsGame);
    DropList_SelectHand.Hint := 'Select player to start map with'; //Todo translate

    Panel_Save := TKMPanel.Create(PopUp_QuickPlay, Left, 95, ControlsWidth, 230);

    Button_QuickPlay := TKMButton.Create(PopUp_QuickPlay, Left, 310, ControlsWidth, 30, 'Start without save', bsGame); //Todo translate
    Button_QuickPlay.Hint := 'Discard changes and start game for selected player'; //Todo translate
    Button_QuickPlay.OnClick := QuickPlay_Click;

    Button_Cancel := TKMButton.Create(PopUp_QuickPlay, (PopUp_QuickPlay.Width - ControlsWidth) div 2, PopUp_QuickPlay.Height - 40, ControlsWidth, 30, 'Cancel', bsGame); //Todo translate
    Button_Cancel.Hint := 'Cancel'; //Todo translate
    Button_Cancel.OnClick := Cancel_Click;

  fMenuSave := TKMMapEdMenuSave.Create(Panel_Save, SaveDone, 10, 220);
  fMenuSave.OnChangeMapType := MapTypeChanged;

  fMenuSave.Button_SaveCancel.Hide;

  fMenuSave.Button_SaveSave.Top := fMenuSave.Button_SaveSave.Top + 10;
  fMenuSave.Button_SaveSave.Caption := 'Save and start'; //Todo translate
  fMenuSave.Button_SaveSave.Hint := 'Save map and start game for selected player'; //Todo translate
  fMenuSave.Button_SaveSave.OnChangeEnableStatus := SaveBtn_EnableStatusChanged;

end;


destructor TKMMapEdMenuQuickPlay.Destroy;
begin
  FreeAndNil(fMenuSave);
  inherited;
end;


procedure TKMMapEdMenuQuickPlay.QuickPlay_Click(Sender: TObject);
var
  MapName, GameName: String;
  Color: Cardinal;
  HandID: Integer;
  IsMultiplayer: Boolean;
begin
  MapName := TKMapsCollection.FullPath(gGame.GameName, '.dat', fIsMultiplayer);
  GameName := gGame.GameName;
  HandId := DropList_SelectHand.GetSelectedTag;
  Color := gHands[HandId].FlagColor;
  IsMultiplayer := fIsMultiplayer; //Somehow fIsMultiplayer sometimes change its value... have no time to debug it. Just save to local value for now
  FreeThenNil(gGame);
  gGameApp.NewSingleMap(MapName, GameName, HandId, Color);
  gGame.StartedFromMapEditor := True;
  gGame.StartedFromMapEdAsMPMap := IsMultiplayer;
  TKMGamePlayInterface(gGame.ActiveInterface).SetMenuState(gGame.MissionMode = mm_Tactic);
end;


function TKMMapEdMenuQuickPlay.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True; //We want to handle all keys here
  case Key of
    VK_ESCAPE:  if Button_Cancel.IsClickable then
                  Cancel_Click(Button_Cancel);
  end;
end;


procedure TKMMapEdMenuQuickPlay.MapTypeChanged(aIsMultiplayer: Boolean);
begin
  SetLoadMode(aIsMultiplayer);
end;


procedure TKMMapEdMenuQuickPlay.UpdatePanel;
begin
  Update_PlayerSelect;
  if not DropList_SelectHand.List.Selected then
  begin
    if gMySpectator.Hand.HasAssets then
      DropList_SelectHand.SelectByTag(gMySpectator.HandIndex)
    else
      PlayerSelectFirst;
  end;
  Button_QuickPlay.Enabled := (not gGame.MapEditor.IsNewMap or gGame.MapEditor.WereSaved) and DropList_SelectHand.List.Selected;
end;


procedure TKMMapEdMenuQuickPlay.Update_PlayerSelect;
var
  I: Integer;
begin
  DropList_SelectHand.Clear;
  for I := 0 to MAX_HANDS - 1 do
  begin
    if gHands[I].HasAssets then
      DropList_SelectHand.Add(Format(gResTexts[TX_PLAYER_X], [I + 1]), I);
  end;
end;


procedure TKMMapEdMenuQuickPlay.PlayerSelectFirst;
var
  I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
  begin
    if gHands[I].HasAssets then
    begin
      DropList_SelectHand.SelectByTag(I);
      Break;
    end;
  end;
end;


procedure TKMMapEdMenuQuickPlay.SaveBtn_EnableStatusChanged(aValue: Boolean);
begin
  if aValue and not DropList_SelectHand.List.Selected then
    fMenuSave.Button_SaveSave.Disable;
end;


procedure TKMMapEdMenuQuickPlay.SaveDone(Sender: TObject);
begin
  QuickPlay_Click(Sender);
end;


procedure TKMMapEdMenuQuickPlay.SetLoadMode(aMultiplayer: Boolean);
begin
  fIsMultiplayer := aMultiplayer;
  fMenuSave.SetLoadMode(aMultiplayer);
end;


procedure TKMMapEdMenuQuickPlay.Cancel_Click(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMenuQuickPlay.Hide;
begin
  PopUp_QuickPlay.Hide;
end;


procedure TKMMapEdMenuQuickPlay.Show;
begin
  UpdatePanel;
  PopUp_QuickPlay.Show;
  fMenuSave.Show;
end;


function TKMMapEdMenuQuickPlay.Visible: Boolean;
begin
  Result := PopUp_QuickPlay.Visible;
end;


end.
