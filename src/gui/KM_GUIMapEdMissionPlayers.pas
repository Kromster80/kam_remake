unit KM_GUIMapEdMissionPlayers;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;

type
  TKMMapEdMissionPlayers = class
  private
    procedure Mission_PlayerTypesChange(Sender: TObject);
    procedure Mission_PlayerTypesUpdate;
    procedure Mission_PlayerIdUpdate;
  protected
    Panel_PlayerTypes: TKMPanel;
    CheckBox_PlayerTypes: array [0..MAX_HANDS-1, 0..2] of TKMCheckBox;
    Label_PlayerId : array [0..MAX_HANDS-1] of TKMLabel;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_Game, KM_RenderUI, KM_ResFonts, KM_InterfaceGame,
  KM_Hand;


{ TKMMapEdMissionPlayers }
constructor TKMMapEdMissionPlayers.Create(aParent: TKMPanel);
var
  I,K: Integer;
begin
  inherited Create;

  Panel_PlayerTypes := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_PlayerTypes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYERS_TYPE], fnt_Outline, taCenter);
  TKMLabel.Create(Panel_PlayerTypes,  4, 30, 20, 20, '#',       fnt_Grey, taLeft);
  TKMLabel.Create(Panel_PlayerTypes, 24, 30, 60, 20, gResTexts[TX_MAPED_PLAYERS_DEFAULT], fnt_Grey, taLeft);
  TKMImage.Create(Panel_PlayerTypes,104, 30, 60, 20, 588, rxGui);
  TKMImage.Create(Panel_PlayerTypes,164, 30, 20, 20,  62, rxGuiMain);
  for I := 0 to MAX_HANDS - 1 do
  begin
    Label_PlayerId[i] := TKMLabel.Create(Panel_PlayerTypes,  4, 50+I*25, 20, 20, IntToStr(I+1), fnt_Outline, taLeft);

    for K := 0 to 2 do
    begin
      CheckBox_PlayerTypes[I,K] := TKMCheckBox.Create(Panel_PlayerTypes, 44+K*60, 48+I*25, 20, 20, '', fnt_Metal);
      CheckBox_PlayerTypes[I,K].Tag       := I;
      CheckBox_PlayerTypes[I,K].OnClick   := Mission_PlayerTypesChange;
    end;
  end;
end;


procedure TKMMapEdMissionPlayers.Mission_PlayerTypesUpdate;
var I: Integer;
begin
  for I := 0 to gHands.Count - 1 do
  begin
    CheckBox_PlayerTypes[I, 0].Enabled := gHands[I].HasAssets;
    CheckBox_PlayerTypes[I, 1].Enabled := gHands[I].HasAssets;
    CheckBox_PlayerTypes[I, 2].Enabled := gHands[I].HasAssets;

    CheckBox_PlayerTypes[I, 0].Checked := gHands[I].HasAssets and (gGame.MapEditor.DefaultHuman = I);
    CheckBox_PlayerTypes[I, 1].Checked := gHands[I].HasAssets and gGame.MapEditor.PlayerHuman[I];
    CheckBox_PlayerTypes[I, 2].Checked := gHands[I].HasAssets and gGame.MapEditor.PlayerAI[I];
  end;
end;


procedure TKMMapEdMissionPlayers.Mission_PlayerTypesChange(Sender: TObject);
var PlayerId: Integer;
begin
  PlayerId := TKMCheckBox(Sender).Tag;

  //There should be exactly one default human player
  if Sender = CheckBox_PlayerTypes[PlayerId, 0] then
    gGame.MapEditor.DefaultHuman := PlayerId;


  if Sender = CheckBox_PlayerTypes[PlayerId, 1] then
  begin
    gGame.MapEditor.PlayerHuman[PlayerId] := CheckBox_PlayerTypes[PlayerId, 1].Checked;
    //User cannot set player type undetermined
    if (not CheckBox_PlayerTypes[PlayerId, 1].Checked)
        and (not CheckBox_PlayerTypes[PlayerId, 2].Checked) then
        gGame.MapEditor.PlayerAI[PlayerId] := true;
  end;

  if Sender = CheckBox_PlayerTypes[PlayerId, 2] then
  begin
    gGame.MapEditor.PlayerAI[PlayerId] := CheckBox_PlayerTypes[PlayerId, 2].Checked;
    //User cannot set player type undetermined
    if (not CheckBox_PlayerTypes[PlayerId, 1].Checked)
        and (not CheckBox_PlayerTypes[PlayerId, 2].Checked) then
        gGame.MapEditor.PlayerHuman[PlayerId] := true;
  end;

  Mission_PlayerTypesUpdate;
end;

procedure TKMMapEdMissionPlayers.Mission_PlayerIdUpdate;
var I : integer;
begin
  for I := 0 to MAX_HANDS - 1 do
    if I < gHands.Count then
      if gHands[I].HasAssets then
        Label_PlayerId[i].FontColor := $FFFFFFFF
      else
        Label_PlayerId[i].FontColor := $FF808080;
end;


procedure TKMMapEdMissionPlayers.Hide;
begin
  Panel_PlayerTypes.Hide;
end;


procedure TKMMapEdMissionPlayers.Show;
begin
  Mission_PlayerTypesUpdate;
  Mission_PlayerIdUpdate;
  Panel_PlayerTypes.Show;
end;


function TKMMapEdMissionPlayers.Visible: Boolean;
begin
  Result := Panel_PlayerTypes.Visible;
end;


end.
