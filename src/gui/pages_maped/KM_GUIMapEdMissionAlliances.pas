unit KM_GUIMapEdMissionAlliances;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Defaults, KM_InterfaceGame;

type
  TKMMapEdMissionAlliances = class
  private
    procedure Mission_AlliancesChange(Sender: TObject);
    procedure Mission_AlliancesUpdate;
  protected
    Panel_Alliances: TKMPanel;
    CheckBox_Alliances: array [0..MAX_HANDS-1, 0..MAX_HANDS-1] of TKMCheckBox;
    CheckBox_AlliancesSym: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand;


{ TKMMapEdMissionAlliances }
constructor TKMMapEdMissionAlliances.Create(aParent: TKMPanel);
var
  I, K: Integer;
begin
  inherited Create;

  Panel_Alliances := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Alliances, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_ALLIANCE], fnt_Outline, taCenter);
  for I := 0 to MAX_HANDS - 1 do
  begin
    TKMLabel.Create(Panel_Alliances, 20 + I * 15, 30, IntToStr(I + 1), fnt_Grey, taCenter);
    TKMLabel.Create(Panel_Alliances, 0, 46 + I * 22, IntToStr(I + 1), fnt_Grey, taLeft);
    for K := 0 to MAX_HANDS - 1 do
    begin
      CheckBox_Alliances[I,K] := TKMCheckBox.Create(Panel_Alliances, 12 + K * 15, 46 + I * 22, 20, 20, '', fnt_Metal);
      CheckBox_Alliances[I,K].Tag       := I * MAX_HANDS + K;
      CheckBox_Alliances[I,K].OnClick   := Mission_AlliancesChange;
    end;
  end;

  //It does not have OnClick event for a reason:
  // - we don't have a rule to make alliances symmetrical yet
  CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 12, 50 + MAX_HANDS * 22, TB_WIDTH, 20, gResTexts[TX_MAPED_ALLIANCE_SYMMETRIC], fnt_Metal);
  CheckBox_AlliancesSym.Checked := True;
  CheckBox_AlliancesSym.Disable;
end;


procedure TKMMapEdMissionAlliances.Mission_AlliancesChange(Sender: TObject);
const
  ALL: array [Boolean] of TAllianceType = (at_Enemy, at_Ally);
var
  I,K: Integer;
begin
  I := TKMCheckBox(Sender).Tag div gHands.Count;
  K := TKMCheckBox(Sender).Tag mod gHands.Count;

  gHands[I].Alliances[K] := ALL[CheckBox_Alliances[I,K].Checked or (I = K)];

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then
  begin
    CheckBox_Alliances[K,I].Checked := CheckBox_Alliances[I,K].Checked;
    gHands[K].Alliances[I] := gHands[I].Alliances[K];
  end;

  Mission_AlliancesUpdate;
end;


procedure TKMMapEdMissionAlliances.Mission_AlliancesUpdate;
var
  I,K: Integer;
begin
  for I := 0 to gHands.Count - 1 do
  for K := 0 to gHands.Count - 1 do
  begin
    CheckBox_Alliances[I,K].Enabled := gHands[I].HasAssets and gHands[K].HasAssets;
    CheckBox_Alliances[I,K].Checked := gHands[I].HasAssets and gHands[K].HasAssets and (gHands[I].Alliances[K] = at_Ally);
  end;
end;


procedure TKMMapEdMissionAlliances.Hide;
begin
  Panel_Alliances.Hide;
end;


procedure TKMMapEdMissionAlliances.Show;
begin
  Mission_AlliancesUpdate;
  Panel_Alliances.Show;
end;


function TKMMapEdMissionAlliances.Visible: Boolean;
begin
  Result := Panel_Alliances.Visible;
end;


end.
