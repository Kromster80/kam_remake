unit KM_GUIMapEdPlayerGoals;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_Defaults,
   KM_InterfaceDefaults, KM_AIGoals,
   KM_GUIMapEdGoal;


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
    GoalPopUp: TKMMapEdGoal;

    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_PlayersCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts;


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
     gResTexts[TX_MAPED_GOALS_PLAYER],
     gResTexts[TX_MAPED_GOALS_TIME],
     gResTexts[TX_MAPED_GOALS_MESSAGE]], [0, 20, 120, 140, 160]);
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
  gPlayers[MySpectator.PlayerIndex].AI.Goals.AddGoal(G);

  Goals_Refresh;
  ColumnBox_Goals.ItemIndex := gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1;

  //Edit the attack we have just appended
  Goals_Edit(ColumnBox_Goals.ItemIndex);
end;


procedure TKMMapEdPlayerGoals.Goals_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  if InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1) then
    gPlayers[MySpectator.PlayerIndex].AI.Goals.Delete(I);
  Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1));

  GoalPopUp.Show(MySpectator.PlayerIndex, aIndex);
  GoalPopUp.fOnDone := Goals_OnDone;
end;


procedure TKMMapEdPlayerGoals.Goals_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  Button_GoalsDel.Enabled := InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1);
end;


procedure TKMMapEdPlayerGoals.Goals_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1) then
    Goals_Edit(I);
end;


procedure TKMMapEdPlayerGoals.Goals_OnDone(Sender: TObject);
begin
  Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_Refresh;
const
  Typ: array [TGoalType] of string = ('-', 'V', 'S');
  Cnd: array [TGoalCondition] of string = (
    'None', 'BuildTutorial', 'Time', 'Buildings', 'Troops', 'Unknown',
    'MilitaryAssets', 'SerfsAndSchools', 'EconomyBuildings');
var
  I: Integer;
  G: TKMGoal;
begin
  ColumnBox_Goals.Clear;

  for I := 0 to gPlayers[MySpectator.PlayerIndex].AI.Goals.Count - 1 do
  begin
    G := gPlayers[MySpectator.PlayerIndex].AI.Goals[I];
    ColumnBox_Goals.AddItem(MakeListRow([Typ[G.GoalType],
                                    Cnd[G.GoalCondition],
                                    IntToStr(G.PlayerIndex + 1),
                                    IntToStr(G.GoalTime div 10),
                                    IntToStr(G.MessageToShow)]));
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
