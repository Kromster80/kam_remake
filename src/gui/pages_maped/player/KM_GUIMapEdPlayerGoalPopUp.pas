unit KM_GUIMapEdPlayerGoalPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes,
  KM_Controls, KM_Defaults, KM_Pics, KM_AIGoals;

type
  TKMMapEdPlayerGoal = class
  private
    fOwner: TKMHandIndex;
    fIndex: Integer;

    procedure Goal_Change(Sender: TObject);
    procedure Goal_Close(Sender: TObject);
    procedure Goal_Refresh(aGoal: TKMGoal);
    function GetVisible: Boolean;
  protected
    Panel_Goal: TKMPanel;
    Image_GoalFlag: TKMImage;
    Radio_GoalType: TKMRadioGroup;
    Radio_GoalCondition: TKMRadioGroup;
    NumEdit_GoalPlayer: TKMNumericEdit;
    Button_GoalOk: TKMButton;
    Button_GoalCancel: TKMButton;
  public
    fOnDone: TNotifyEvent;
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure Show(aPlayer: TKMHandIndex; aIndex: Integer);
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand;


{ TKMGUIMapEdGoal }
constructor TKMMapEdPlayerGoal.Create(aParent: TKMPanel);
const
  SIZE_X = 600;
  SIZE_Y = 300;
var
  Img: TKMImage;
begin
  inherited Create;

  Panel_Goal := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Goal.AnchorsCenter;
  Panel_Goal.Hide;

  TKMBevel.Create(Panel_Goal, -1000,  -1000, 4000, 4000);
  Img := TKMImage.Create(Panel_Goal, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
  Img.ImageStretch;
  TKMBevel.Create(Panel_Goal,   0,  0, SIZE_X, SIZE_Y);
  TKMLabel.Create(Panel_Goal, SIZE_X div 2, 10, gResTexts[TX_MAPED_GOALS_TITLE], fnt_Outline, taCenter);

  Image_GoalFlag := TKMImage.Create(Panel_Goal, 10, 10, 0, 0, 30, rxGuiMain);

  TKMLabel.Create(Panel_Goal, 20, 40, 160, 0, gResTexts[TX_MAPED_GOALS_TYPE], fnt_Metal, taLeft);
  Radio_GoalType := TKMRadioGroup.Create(Panel_Goal, 20, 60, 160, 60, fnt_Metal);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_NONE], False);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_VICTORY]);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_SURVIVE]);
  Radio_GoalType.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 200, 40, 280, 0, gResTexts[TX_MAPED_GOALS_CONDITION], fnt_Metal, taLeft);
  Radio_GoalCondition := TKMRadioGroup.Create(Panel_Goal, 200, 60, 280, 180, fnt_Metal);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_NONE], False);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TUTORIAL], False);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TIME], False);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_BUILDS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TROOPS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_UNKNOWN], False);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_ASSETS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_SERFS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_ECONOMY]);
  Radio_GoalCondition.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 480, 40, gResTexts[TX_MAPED_GOALS_PLAYER], fnt_Metal, taLeft);
  NumEdit_GoalPlayer := TKMNumericEdit.Create(Panel_Goal, 480, 60, 1, MAX_HANDS);
  NumEdit_GoalPlayer.OnChange := Goal_Change;

  Button_GoalOk := TKMButton.Create(Panel_Goal, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
  Button_GoalOk.OnClick := Goal_Close;
  Button_GoalCancel := TKMButton.Create(Panel_Goal, SIZE_X-20-160, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
  Button_GoalCancel.OnClick := Goal_Close;
end;


procedure TKMMapEdPlayerGoal.Goal_Change(Sender: TObject);
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist
  NumEdit_GoalPlayer.Enabled := TGoalCondition(Radio_GoalCondition.ItemIndex) <> gc_Time;
end;


function TKMMapEdPlayerGoal.GetVisible: Boolean;
begin
  Result := Panel_Goal.Visible;
end;


procedure TKMMapEdPlayerGoal.Goal_Close(Sender: TObject);
var
  G: TKMGoal;
begin
  if Sender = Button_GoalOk then
  begin
    //Copy Goal info from controls to Goals
    FillChar(G, SizeOf(G), #0); //Make sure unused fields like Message are zero, not random data
    G.GoalType := TGoalType(Radio_GoalType.ItemIndex);
    G.GoalCondition := TGoalCondition(Radio_GoalCondition.ItemIndex);
    if G.GoalType = glt_Survive then
      G.GoalStatus := gs_True
    else
      G.GoalStatus := gs_False;
    G.HandIndex := NumEdit_GoalPlayer.Value - 1;

    gHands[fOwner].AI.Goals[fIndex] := G;
  end;

  Panel_Goal.Hide;
  fOnDone(Self);
end;


procedure TKMMapEdPlayerGoal.Goal_Refresh(aGoal: TKMGoal);
begin
  Image_GoalFlag.FlagColor := gHands[fOwner].FlagColor;

  Radio_GoalType.ItemIndex := Byte(aGoal.GoalType);
  Radio_GoalCondition.ItemIndex := Byte(aGoal.GoalCondition);
  NumEdit_GoalPlayer.Value := aGoal.HandIndex + 1;

  //Certain values disable certain controls
  Goal_Change(nil);
end;


function TKMMapEdPlayerGoal.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_GoalCancel.IsClickable then
                begin
                  Goal_Close(Button_GoalCancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_GoalOk.IsClickable then
                begin
                  Goal_Close(Button_GoalOk);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdPlayerGoal.Show(aPlayer: TKMHandIndex; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Goal_Refresh(gHands[fOwner].AI.Goals[fIndex]);
  Panel_Goal.Show;
end;


end.
