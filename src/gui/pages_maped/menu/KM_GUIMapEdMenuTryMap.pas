unit KM_GUIMapEdMenuTryMap;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, //SysUtils,
  KM_Controls, KM_Defaults, KM_GUIMapEdMenuSave;

type
  TKMMapEdMenuTryMap = class
  private
    fMenuSave: TKMMapEdMenuSave;
    fIsMultiplayer: Boolean;
    procedure Cancel_Click(Sender: TObject);
    procedure Try_Map_Click(Sender: TObject);
    procedure Update_PlayerSelect;
    procedure PlayerSelectFirst;
    procedure UpdatePanel;
    procedure SaveDone(Sender: TObject);
    procedure SaveBtn_EnableStatusChanged(aValue: Boolean);
  protected
    PopUp_TryMap: TKMPopUpPanel;
      Panel_Save: TKMPanel;

      DropList_SelectHand: TKMDropList;
      Label_TryMap: TKMLabel;
      Button_TryMap, Button_Cancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);
    destructor Destroy; override;

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


constructor TKMMapEdMenuTryMap.Create(aParent: TKMPanel);
const
  ControlsWidth = 220;
var
  Left: Integer;
begin
  inherited Create;

  PopUp_TryMap := TKMPopUpPanel.Create(aParent, 240, 420, 'Try Map'); //Todo translate

  PopUp_TryMap.Width := Math.Max(240, gRes.Fonts[PopUp_TryMap.Font].GetTextSize(PopUp_TryMap.Caption).X + 40);
  Left := (PopUp_TryMap.Width - ControlsWidth) div 2;
    TKMLabel.Create(PopUp_TryMap, PopUp_TryMap.Width div 2, 50, 'Select player:', fnt_Metal, taCenter); //Todo translate

    DropList_SelectHand := TKMDropList.Create(PopUp_TryMap, Left, 75, ControlsWidth, 20, fnt_Game, '', bsGame);
    DropList_SelectHand.Hint := 'Select player to start map with'; //Todo translate

    Panel_Save := TKMPanel.Create(PopUp_TryMap, Left, 95, ControlsWidth, 230);

    Button_TryMap := TKMButton.Create(PopUp_TryMap, Left, 310, ControlsWidth, 30, 'Start without save', bsGame); //Todo translate
    Button_TryMap.Hint := 'Discard changes and start game for selected player'; //Todo translate
    Button_TryMap.OnClick := Try_Map_Click;

    Button_Cancel := TKMButton.Create(PopUp_TryMap, (PopUp_TryMap.Width - ControlsWidth) div 2, PopUp_TryMap.Height - 40, ControlsWidth, 30, 'Cancel', bsGame); //Todo translate
    Button_Cancel.Hint := 'Cancel'; //Todo translate
    Button_Cancel.OnClick := Cancel_Click;

  fMenuSave := TKMMapEdMenuSave.Create(Panel_Save, SaveDone, 10, 220);
  fMenuSave.Button_SaveCancel.Hide;

  fMenuSave.Button_SaveSave.Top := fMenuSave.Button_SaveSave.Top + 10;
  fMenuSave.Button_SaveSave.Caption := 'Save and start'; //Todo translate
  fMenuSave.Button_SaveSave.Hint := 'Save map and start game for selected player'; //Todo translate
  fMenuSave.Button_SaveSave.OnChangeEnableStatus := SaveBtn_EnableStatusChanged;

end;


destructor TKMMapEdMenuTryMap.Destroy;
begin
  FreeAndNil(fMenuSave);
  inherited;
end;


procedure TKMMapEdMenuTryMap.Try_Map_Click(Sender: TObject);
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


function TKMMapEdMenuTryMap.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True; //We want to handle all keys here
  case Key of
    VK_ESCAPE:  if Button_Cancel.IsClickable then
                  Cancel_Click(Button_Cancel);
  end;
end;


procedure TKMMapEdMenuTryMap.UpdatePanel;
begin
  Update_PlayerSelect;
  if not DropList_SelectHand.List.Selected then
  begin
    if gMySpectator.Hand.HasAssets then
      DropList_SelectHand.SelectByTag(gMySpectator.HandIndex)
    else
      PlayerSelectFirst;
  end;
  Button_TryMap.Enabled := (not gGame.MapEditor.IsNewMap or gGame.MapEditor.WereSaved) and DropList_SelectHand.List.Selected;
end;


procedure TKMMapEdMenuTryMap.Update_PlayerSelect;
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


procedure TKMMapEdMenuTryMap.PlayerSelectFirst;
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


procedure TKMMapEdMenuTryMap.SaveBtn_EnableStatusChanged(aValue: Boolean);
begin
  if aValue and not DropList_SelectHand.List.Selected then
    fMenuSave.Button_SaveSave.Disable;
end;


procedure TKMMapEdMenuTryMap.SaveDone(Sender: TObject);
begin
  Try_Map_Click(Sender);
end;


procedure TKMMapEdMenuTryMap.SetLoadMode(aMultiplayer: Boolean);
begin
  fIsMultiplayer := aMultiplayer;
  fMenuSave.SetLoadMode(aMultiplayer);
end;


procedure TKMMapEdMenuTryMap.Cancel_Click(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMenuTryMap.Hide;
begin
  PopUp_TryMap.Hide;
end;


procedure TKMMapEdMenuTryMap.Show;
begin
  UpdatePanel;
  PopUp_TryMap.Show;
  fMenuSave.Show;
end;


function TKMMapEdMenuTryMap.Visible: Boolean;
begin
  Result := PopUp_TryMap.Visible;
end;


end.
