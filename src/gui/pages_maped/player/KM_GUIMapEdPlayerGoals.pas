unit KM_GUIMapEdPlayerGoals;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_Defaults,
   KM_InterfaceGame, KM_AIGoals,
   KM_GUIMapEdPlayerGoalPopUp;


type
  TKMMapEdPlayerGoals = class
  private
    procedure Goals_Add(Sender: TObject);
    procedure Goals_Del(Sender: TObject);
    procedure Goals_Edit(aIndex: Integer);
    procedure Goals_ListClick(Sender: TObject);
    procedure Goals_ListDoubleClick(Sender: TObject);
    procedure Goals_OnDone(Sender: TObject);
    procedure Goals_Refresh;
  protected
    Panel_Goals: TKMPanel;
    ColumnBox_Goals: TKMColumnBox;
    Button_GoalsAdd: TKMButton;
    Button_GoalsDel: TKMButton;
  public
    GoalPopUp: TKMMapEdPlayerGoal;

    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand;


{ TKMMapEdPlayerGoals }
constructor TKMMapEdPlayerGoals.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Goals := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Goals, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_GOALS], fnt_Outline, taCenter);
  ColumnBox_Goals := TKMColumnBox.Create(Panel_Goals, 0, 30, TB_WIDTH, 230, fnt_Game, bsGame);
  ColumnBox_Goals.SetColumns(fnt_Outline,
    [gResTexts[TX_MAPED_GOALS_TYPE],
     gResTexts[TX_MAPED_GOALS_CONDITION],
     gResTexts[TX_MAPED_GOALS_PLAYER]], [0, 25, 155]);
  ColumnBox_Goals.OnClick := Goals_ListClick;
  ColumnBox_Goals.OnDoubleClick := Goals_ListDoubleClick;

  Button_GoalsAdd := TKMButton.Create(Panel_Goals, 0, 270, 25, 25, '+', bsGame);
  Button_GoalsAdd.OnClick := Goals_Add;
  Button_GoalsDel := TKMButton.Create(Panel_Goals, 30, 270, 25, 25, 'X', bsGame);
  Button_GoalsDel.OnClick := Goals_Del;
end;


//Add a dummy goal and let mapmaker edit it
procedure TKMMapEdPlayerGoals.Goals_Add(Sender: TObject);
var
  G: TKMGoal;
begin
  FillChar(G, SizeOf(G), #0);
  G.GoalType := glt_Victory;
  G.GoalCondition := gc_Buildings;
  G.Disabled := False;
  gMySpectator.Hand.AI.Goals.AddGoal(G);

  Goals_Refresh;
  ColumnBox_Goals.ItemIndex := gMySpectator.Hand.AI.Goals.Count - 1;

  //Edit the attack we have just appended
  Goals_Edit(ColumnBox_Goals.ItemIndex);
end;


procedure TKMMapEdPlayerGoals.Goals_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  if InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1) then
    gMySpectator.Hand.AI.Goals.Delete(I);
  Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, gMySpectator.Hand.AI.Goals.Count - 1));

  GoalPopUp.Show(gMySpectator.HandIndex, aIndex);
  GoalPopUp.fOnDone := Goals_OnDone;
end;


procedure TKMMapEdPlayerGoals.Goals_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  Button_GoalsDel.Enabled := InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1);
end;


procedure TKMMapEdPlayerGoals.Goals_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1) then
    Goals_Edit(I);
end;


procedure TKMMapEdPlayerGoals.Goals_OnDone(Sender: TObject);
begin
  Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_Refresh;
const
  Typ: array [TGoalType] of string = ('-', 'V', 'S');
  Cnd: array [TGoalCondition] of Integer = (
    TX_MAPED_GOALS_CONDITION_NONE, TX_MAPED_GOALS_CONDITION_TUTORIAL, TX_MAPED_GOALS_CONDITION_TIME,
    TX_MAPED_GOALS_CONDITION_BUILDS, TX_MAPED_GOALS_CONDITION_TROOPS, TX_MAPED_GOALS_CONDITION_UNKNOWN,
    TX_MAPED_GOALS_CONDITION_ASSETS, TX_MAPED_GOALS_CONDITION_SERFS, TX_MAPED_GOALS_CONDITION_ECONOMY);
var
  I: Integer;
  G: TKMGoal;
begin
  ColumnBox_Goals.Clear;

  for I := 0 to gMySpectator.Hand.AI.Goals.Count - 1 do
  begin
    G := gMySpectator.Hand.AI.Goals[I];
    ColumnBox_Goals.AddItem(MakeListRow([Typ[G.GoalType],
                                    gResTexts[Cnd[G.GoalCondition]],
                                    IntToStr(G.HandIndex + 1)]));
  end;

  Goals_ListClick(nil);
end;


procedure TKMMapEdPlayerGoals.Hide;
begin
  Panel_Goals.Hide;
end;


procedure TKMMapEdPlayerGoals.Show;
begin
  Goals_Refresh;
  Panel_Goals.Show;
end;


function TKMMapEdPlayerGoals.Visible: Boolean;
begin
  Result := Panel_Goals.Visible;
end;


end.
