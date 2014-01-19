unit KM_GUIMapEdGoal;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_Defaults, KM_Pics, KM_AIGoals;

type
  TKMMapEdGoal = class
  private
    fOwner: THandIndex;
    fIndex: Integer;

    procedure Goal_Change(Sender: TObject);
    procedure Goal_Close(Sender: TObject);
    procedure Goal_Refresh(aGoal: TKMGoal);
  protected
    Panel_Goal: TKMPanel;
    Image_GoalFlag: TKMImage;
    Radio_GoalType: TKMRadioGroup;
    Radio_GoalCondition: TKMRadioGroup;
    NumEdit_GoalTime: TKMNumericEdit;
    NumEdit_GoalMessage: TKMNumericEdit;
    NumEdit_GoalPlayer: TKMNumericEdit;
    Button_GoalOk: TKMButton;
    Button_GoalCancel: TKMButton;
  public
    fOnDone: TNotifyEvent;
    constructor Create(aParent: TKMPanel);

    procedure Show(aPlayer: THandIndex; aIndex: Integer);
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMGUIMapEdGoal }
constructor TKMMapEdGoal.Create(aParent: TKMPanel);
const
  SIZE_X = 600;
  SIZE_Y = 300;
var
  Img: TKMImage;
begin
  inherited Create;

  Panel_Goal := TKMPanel.Create(aParent, 362, 250, SIZE_X, SIZE_Y);
  Panel_Goal.Anchors := [];
  Panel_Goal.Hide;

  TKMBevel.Create(Panel_Goal, -1000,  -1000, 4000, 4000);
  Img := TKMImage.Create(Panel_Goal, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
  Img.ImageStretch;
  TKMBevel.Create(Panel_Goal,   0,  0, SIZE_X, SIZE_Y);
  TKMLabel.Create(Panel_Goal, SIZE_X div 2, 10, gResTexts[TX_MAPED_GOALS_TITLE], fnt_Outline, taCenter);

  Image_GoalFlag := TKMImage.Create(Panel_Goal, 10, 10, 0, 0, 30, rxGuiMain);

  TKMLabel.Create(Panel_Goal, 20, 40, 100, 0, gResTexts[TX_MAPED_GOALS_TYPE], fnt_Metal, taLeft);
  Radio_GoalType := TKMRadioGroup.Create(Panel_Goal, 20, 60, 100, 60, fnt_Metal);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_NONE], False);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_VICTORY]);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_SURVIVE]);
  Radio_GoalType.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 140, 40, 180, 0, gResTexts[TX_MAPED_GOALS_CONDITION], fnt_Metal, taLeft);
  Radio_GoalCondition := TKMRadioGroup.Create(Panel_Goal, 140, 60, 180, 180, fnt_Metal);
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

  TKMLabel.Create(Panel_Goal, 330, 40, gResTexts[TX_MAPED_GOALS_PLAYER], fnt_Metal, taLeft);
  NumEdit_GoalPlayer := TKMNumericEdit.Create(Panel_Goal, 330, 60, 1, MAX_HANDS);
  NumEdit_GoalPlayer.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 480, 40, gResTexts[TX_MAPED_GOALS_TIME], fnt_Metal, taLeft);
  NumEdit_GoalTime := TKMNumericEdit.Create(Panel_Goal, 480, 60, 0, 32767);
  NumEdit_GoalTime.OnChange := Goal_Change;
  NumEdit_GoalTime.SharedHint := 'This setting is deprecated, use scripts instead';

  TKMLabel.Create(Panel_Goal, 480, 90, gResTexts[TX_MAPED_GOALS_MESSAGE], fnt_Metal, taLeft);
  NumEdit_GoalMessage := TKMNumericEdit.Create(Panel_Goal, 480, 110, 0, 0);
  NumEdit_GoalMessage.SharedHint := 'This setting is deprecated, use scripts instead';
  NumEdit_GoalMessage.Disable;

  Button_GoalOk := TKMButton.Create(Panel_Goal, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
  Button_GoalOk.OnClick := Goal_Close;
  Button_GoalCancel := TKMButton.Create(Panel_Goal, SIZE_X-20-160, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
  Button_GoalCancel.OnClick := Goal_Close;
end;


procedure TKMMapEdGoal.Goal_Change(Sender: TObject);
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist

  NumEdit_GoalTime.Enabled := TGoalCondition(Radio_GoalCondition.ItemIndex) = gc_Time;
  NumEdit_GoalPlayer.Enabled := TGoalCondition(Radio_GoalCondition.ItemIndex) <> gc_Time;
end;


procedure TKMMapEdGoal.Goal_Close(Sender: TObject);
var
  G: TKMGoal;
begin
  if Sender = Button_GoalOk then
  begin
    //Copy Goal info from controls to Goals
    G.GoalType := TGoalType(Radio_GoalType.ItemIndex);
    G.GoalCondition := TGoalCondition(Radio_GoalCondition.ItemIndex);
    if G.GoalType = glt_Survive then
      G.GoalStatus := gs_True
    else
      G.GoalStatus := gs_False;
    G.GoalTime := NumEdit_GoalTime.Value * 10;
    G.MessageToShow := NumEdit_GoalMessage.Value;
    G.HandIndex := NumEdit_GoalPlayer.Value - 1;

    gHands[fOwner].AI.Goals[fIndex] := G;
  end;

  Panel_Goal.Hide;
  fOnDone(Self);
end;


procedure TKMMapEdGoal.Goal_Refresh(aGoal: TKMGoal);
begin
  Image_GoalFlag.FlagColor := gHands[fOwner].FlagColor;

  Radio_GoalType.ItemIndex := Byte(aGoal.GoalType);
  Radio_GoalCondition.ItemIndex := Byte(aGoal.GoalCondition);
  NumEdit_GoalTime.Value := aGoal.GoalTime div 10;
  NumEdit_GoalMessage.Value := aGoal.MessageToShow;
  NumEdit_GoalPlayer.Value := aGoal.HandIndex + 1;

  //Certain values disable certain controls
  Goal_Change(nil);
end;


procedure TKMMapEdGoal.Show(aPlayer: THandIndex; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Goal_Refresh(gHands[fOwner].AI.Goals[fIndex]);
  Panel_Goal.Show;
end;


end.
