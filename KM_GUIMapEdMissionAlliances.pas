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
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMMapEdMissionAlliances }
constructor TKMMapEdMissionAlliances.Create(aParent: TKMPanel);
var
  I,K: Integer;
begin
  inherited Create;

  Panel_Alliances := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Alliances, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_ALLIANCE], fnt_Outline, taCenter);
  for I := 0 to MAX_HANDS - 1 do
  begin
    TKMLabel.Create(Panel_Alliances,24+I*20+2,30,20,20,IntToStr(I+1),fnt_Outline,taLeft);
    TKMLabel.Create(Panel_Alliances,4,50+I*25,20,20,IntToStr(I+1),fnt_Outline,taLeft);
    for K := 0 to MAX_HANDS - 1 do
    begin
      CheckBox_Alliances[I,K] := TKMCheckBox.Create(Panel_Alliances, 20+K*20, 46+I*25, 20, 20, '', fnt_Metal);
      CheckBox_Alliances[I,K].Tag       := I * MAX_HANDS + K;
      CheckBox_Alliances[I,K].OnClick   := Mission_AlliancesChange;
    end;
  end;

  //It does not have OnClick event for a reason:
  // - we don't have a rule to make alliances symmetrical yet
  CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 0, 50+MAX_HANDS*25, TB_WIDTH, 20, gResTexts[TX_MAPED_ALLIANCE_SYMMETRIC], fnt_Metal);
  CheckBox_AlliancesSym.Checked := true;
  CheckBox_AlliancesSym.Disable;
end;


procedure TKMMapEdMissionAlliances.Mission_AlliancesChange(Sender: TObject);
var I,K: Integer;
begin
  if Sender = nil then
  begin
    for I:=0 to gHands.Count-1 do
    for K:=0 to gHands.Count-1 do
      if (gHands[I]<>nil)and(gHands[K]<>nil) then
        CheckBox_Alliances[I,K].Checked := (gHands.CheckAlliance(gHands[I].HandIndex, gHands[K].HandIndex)=at_Ally)
      else
        CheckBox_Alliances[I,K].Disable; //Player does not exist?
    exit;
  end;

  I := TKMCheckBox(Sender).Tag div gHands.Count;
  K := TKMCheckBox(Sender).Tag mod gHands.Count;
  if CheckBox_Alliances[I,K].Checked then gHands[I].Alliances[K] := at_Ally
                                     else gHands[I].Alliances[K] := at_Enemy;

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then
  begin
    CheckBox_Alliances[K,I].Checked := CheckBox_Alliances[I,K].Checked;
    gHands[K].Alliances[I] := gHands[I].Alliances[K];
  end;
end;


procedure TKMMapEdMissionAlliances.Hide;
begin
  Panel_Alliances.Hide;
end;


procedure TKMMapEdMissionAlliances.Show;
begin
  Mission_AlliancesChange(nil);
  Panel_Alliances.Show;
end;


function TKMMapEdMissionAlliances.Visible: Boolean;
begin
  Result := Panel_Alliances.Visible;
end;


end.
