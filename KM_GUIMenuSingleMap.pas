unit KM_GUIMenuSingleMap;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, SysUtils,
  KM_Defaults,
  KM_Controls, KM_Maps, KM_Pics, KM_InterfaceDefaults, KM_Minimap;


const
  MAX_UI_GOALS = 10;


type
  TKMGUIMenuSingleMap = class
  private
    fOnPageChange: TGUIEventText;

    fMaps: TKMapsCollection;
    fMinimap: TKMMinimap;

    fLastMapCRC: Cardinal; //CRC of selected map

    fSingleLoc: Integer;
    fSingleColor: Cardinal;

    procedure Create_SingleMap(aParent: TKMPanel);

    procedure ListClear;
    procedure ScanUpdate(Sender: TObject);
    procedure SortUpdate(Sender: TObject);
    procedure ListRefresh(aJumpToSelected:Boolean);
    procedure ListClick(Sender: TObject);
    procedure OptionsChange(Sender: TObject);
    procedure Update;
    procedure StartClick(Sender: TObject);
    procedure ListSort(aColumn: Integer);
    procedure MinimapLocClick(aValue: Integer);

    procedure BackClick(Sender: TObject);
  protected
    Panel_Single:TKMPanel;
      Panel_SingleDesc: TKMPanel;
        Label_SingleTitle: TKMLabel;
        Memo_SingleDesc: TKMMemo;
        MinimapView_Single: TKMMinimapView;
        DropBox_SingleLoc: TKMDropList;
        DropBox_SingleColor: TKMDropColumns;
        Image_SingleAllies: array [0..MAX_PLAYERS-1] of TKMImage;
        Image_SingleEnemies: array [0..MAX_PLAYERS-1] of TKMImage;
        Image_SingleVictGoal: array [0..MAX_UI_GOALS-1] of TKMImage;
        Label_SingleVictGoal: array [0..MAX_UI_GOALS-1] of TKMLabel;
        Image_SingleVictGoalSt: array [0..MAX_UI_GOALS-1] of TKMImage;
        Image_SingleSurvGoal: array [0..MAX_UI_GOALS-1] of TKMImage;
        Label_SingleSurvGoal: array [0..MAX_UI_GOALS-1] of TKMLabel;
        Image_SingleSurvGoalSt: array [0..MAX_UI_GOALS-1] of TKMImage;
      ColumnBox_SingleMaps: TKMColumnBox;
      Button_SingleBack, Button_SingleStart: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show;
    procedure UpdateState(aTickCount: Cardinal);
  end;


implementation
uses KM_TextLibrary, KM_GameApp, KM_Utils, KM_RenderUI;


{ TKMGUIMenuSingleMap }
constructor TKMGUIMenuSingleMap.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  fMaps := TKMapsCollection.Create(False);
  fMinimap := TKMMinimap.Create(True, False, True);

  Create_SingleMap(aParent);
end;


destructor TKMGUIMenuSingleMap.Destroy;
begin
  fMaps.Free;
  fMinimap.Free;

  inherited;
end;


procedure TKMGUIMenuSingleMap.Create_SingleMap(aParent: TKMPanel);
const
  PAD_VERT = 44; //Padding from top/bottom
  PAD_SIDE = 44; //Padding from sides
var
  I: Integer;
  Half: Word; //Half width for panes
  L: TKMLabel;
  B: TKMBevel;
begin
  Panel_Single := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Single.Stretch;

    Half := (aParent.Width - PAD_SIDE) div 2 - PAD_SIDE;

    ColumnBox_SingleMaps := TKMColumnBox.Create(Panel_Single,
                                                  (aParent.Width + PAD_SIDE) div 2,
                                                  PAD_VERT,
                                                  Half,
                                                  aParent.Height - PAD_VERT*2,
                                                  fnt_MainMapGold, bsMenu);
    ColumnBox_SingleMaps.Anchors := [akTop, akBottom];
    ColumnBox_SingleMaps.SetColumns(fnt_Outline, ['', '', fTextLibrary[TX_MENU_MAP_TITLE], fTextLibrary[TX_MENU_MAP_SIZE]], [0, 50, 100, 380]);
    ColumnBox_SingleMaps.Columns[2].Font := fnt_Metal;
    ColumnBox_SingleMaps.Columns[2].HintFont := fnt_Grey;
    ColumnBox_SingleMaps.Columns[1].TextAlign := taCenter;
    ColumnBox_SingleMaps.Columns[3].TextAlign := taCenter;
    ColumnBox_SingleMaps.ItemHeight := 40;
    ColumnBox_SingleMaps.SearchColumn := 2;
    ColumnBox_SingleMaps.ShowLines := True;
    ColumnBox_SingleMaps.Header.Height := 40;
    ColumnBox_SingleMaps.Header.TextAlign := taCenter;
    ColumnBox_SingleMaps.Header.Columns[0].Glyph := MakePic(rxGui, 42);
    ColumnBox_SingleMaps.Header.Columns[1].Glyph := MakePic(rxGui, 31);
    ColumnBox_SingleMaps.OnColumnClick := ListSort;
    ColumnBox_SingleMaps.OnChange := ListClick;
    ColumnBox_SingleMaps.OnDoubleClick := StartClick;

    Panel_SingleDesc := TKMPanel.Create(Panel_Single, PAD_SIDE, PAD_VERT, Half, aParent.Height - PAD_VERT*2);
    Panel_SingleDesc.Anchors := [akTop, akBottom];

      //Description
      Label_SingleTitle := TKMLabel.Create(Panel_SingleDesc, Half div 2, 0, '', fnt_Outline, taCenter);
      Memo_SingleDesc  := TKMMemo.Create(Panel_SingleDesc, 0, 20, Half, 300, fnt_Metal, bsMenu);
      Memo_SingleDesc.Anchors := [akTop, akBottom];
      Memo_SingleDesc.AutoWrap := True;

      //Minimap preview
      B := TKMBevel.Create(Panel_SingleDesc, 0, 330, 195, 195);
      B.Anchors := [akLeft, akBottom];
      MinimapView_Single := TKMMinimapView.Create(Panel_SingleDesc, 2, 332, 191, 191);
      MinimapView_Single.Anchors := [akLeft, akBottom];
      MinimapView_Single.OnLocClick := MinimapLocClick;

      //Setup (loc and flag placed alongside just like in MP lobby)
      //Other setup settings can go below
      L := TKMLabel.Create(Panel_SingleDesc, 200, 330, 150, 20, fTextLibrary[TX_LOBBY_HEADER_STARTLOCATION], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      DropBox_SingleLoc := TKMDropList.Create(Panel_SingleDesc, 200, 350, 150, 20, fnt_Metal, fTextLibrary[TX_MENU_MAP_LOCATION], bsMenu);
      DropBox_SingleLoc.Anchors := [akLeft, akBottom];
      DropBox_SingleLoc.OnChange := OptionsChange;

      L := TKMLabel.Create(Panel_SingleDesc, 360, 330, 80, 20, fTextLibrary[TX_LOBBY_HEADER_FLAGCOLOR], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      DropBox_SingleColor := TKMDropColumns.Create(Panel_SingleDesc, 360, 350, 80, 20, fnt_Grey, '', bsMenu);
      DropBox_SingleColor.Anchors := [akLeft, akBottom];
      DropBox_SingleColor.SetColumns(fnt_Outline, [''], [0]);
      DropBox_SingleColor.List.ShowHeader := False;
      DropBox_SingleColor.FadeImageWhenDisabled := False;
      DropBox_SingleColor.Add(MakeListRow([''], [$FFFFFFFF], [MakePic(rxGuiMain, 31)], 0));
      DropBox_SingleColor.OnChange := OptionsChange;

      //Goals
      B := TKMBevel.Create(Panel_SingleDesc, 0, 530, Half, 30);
      B.Anchors := [akLeft, akBottom];
      L := TKMLabel.Create(Panel_SingleDesc, 4, 538, 190, 30, fTextLibrary[TX_MENU_WIN_CONDITION], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      B := TKMBevel.Create(Panel_SingleDesc, 0, 560, Half, 30);
      B.Anchors := [akLeft, akBottom];
      L := TKMLabel.Create(Panel_SingleDesc, 4, 568, 190, 30, fTextLibrary[TX_MENU_DEFEAT_CONDITION], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      for I := 0 to MAX_UI_GOALS - 1 do
      begin
        Image_SingleVictGoal[I] := TKMImage.Create(Panel_SingleDesc, 200 + I*35, 530, 30, 30, 41);
        Image_SingleVictGoal[I].Anchors := [akLeft, akBottom];
        Image_SingleVictGoal[I].ImageCenter;
        Label_SingleVictGoal[I] := TKMLabel.Create(Panel_SingleDesc, 215 + I*35, 535, '', fnt_Grey, taCenter);
        Label_SingleVictGoal[I].Anchors := [akLeft, akBottom];
        Image_SingleVictGoalSt[I] := TKMImage.Create(Panel_SingleDesc, 217 + I*35, 545, 20, 20, 371, rxGui);
        Image_SingleVictGoalSt[I].Anchors := [akLeft, akBottom];

        Image_SingleSurvGoal[I] := TKMImage.Create(Panel_SingleDesc, 200 + I*35, 560, 30, 30, 41);
        Image_SingleSurvGoal[I].Anchors := [akLeft, akBottom];
        Image_SingleSurvGoal[I].ImageCenter;
        Label_SingleSurvGoal[I] := TKMLabel.Create(Panel_SingleDesc, 215 + I*35, 565, '', fnt_Grey, taCenter);
        Label_SingleSurvGoal[I].Anchors := [akLeft, akBottom];
        Image_SingleSurvGoalSt[I] := TKMImage.Create(Panel_SingleDesc, 218 + I*35, 575, 20, 20, 44, rxGui);
        Image_SingleSurvGoalSt[I].Anchors := [akLeft, akBottom];
      end;

      //Alliances
      B := TKMBevel.Create(Panel_SingleDesc, 0, 590, Half, 20);
      B.Anchors := [akLeft, akBottom];
      L := TKMLabel.Create(Panel_SingleDesc, 4, 594, 190, 20, fTextLibrary[TX_MENU_ALLIES], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      B := TKMBevel.Create(Panel_SingleDesc, 0, 610, Half, 20);
      B.Anchors := [akLeft, akBottom];
      L := TKMLabel.Create(Panel_SingleDesc, 4, 614, 190, 20, fTextLibrary[TX_MENU_ENEMIES], fnt_Metal, taLeft);
      L.Anchors := [akLeft, akBottom];
      for I := 0 to MAX_PLAYERS - 1 do
      begin
        Image_SingleAllies[I] := TKMImage.Create(Panel_SingleDesc, 200 + I*32, 590, 50, 20, 30, rxGuiMain);
        Image_SingleAllies[I].Anchors := [akLeft, akBottom];
        Image_SingleEnemies[I] := TKMImage.Create(Panel_SingleDesc, 200 + I*32, 610, 50, 20, 30, rxGuiMain);
        Image_SingleEnemies[I].Anchors := [akLeft, akBottom];
      end;

    Button_SingleBack := TKMButton.Create(Panel_Single, 45, aParent.Height - PAD_VERT - 30, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_SingleBack.Anchors := [akLeft, akBottom];
    Button_SingleBack.OnClick := BackClick;
    Button_SingleStart := TKMButton.Create(Panel_Single, 270, aParent.Height - PAD_VERT - 30, 220, 30, fTextLibrary[TX_MENU_SINGLE_START_MAP], bsMenu);
    Button_SingleStart.Anchors := [akLeft, akBottom];
    Button_SingleStart.OnClick := StartClick;
end;


procedure TKMGUIMenuSingleMap.ListClear;
begin
  ColumnBox_SingleMaps.Clear;
  ListClick(nil);
  fLastMapCRC := 0;
end;


procedure TKMGUIMenuSingleMap.ScanUpdate(Sender: TObject);
begin
  ListRefresh(False); //Don't jump to selected with each scan update
end;


procedure TKMGUIMenuSingleMap.SortUpdate(Sender: TObject);
begin
  ListRefresh(True); //After sorting jump to the selected item
end;


procedure TKMGUIMenuSingleMap.ListRefresh(aJumpToSelected: Boolean);
var
  I, PrevTop: Integer;
  R: TKMListRow;
begin
  PrevTop := ColumnBox_SingleMaps.TopIndex;
  ColumnBox_SingleMaps.Clear;

  fMaps.Lock;
  try
    for I := 0 to fMaps.Count - 1 do
    begin
      R := MakeListRow(['', IntToStr(fMaps[I].PlayerCount), fMaps[I].FileName, MapSizeText(fMaps[I].MapSizeX, fMaps[I].MapSizeY)]);
      R.Cells[2].Hint := fMaps[I].SmallDesc;
      R.Cells[0].Pic := MakePic(rxGui, 28 + Byte(fMaps[I].MissionMode <> mm_Tactic) * 14);
      ColumnBox_SingleMaps.AddItem(R);

      if (fMaps[I].CRC = fLastMapCRC) then
        ColumnBox_SingleMaps.ItemIndex := I;
    end;
  finally
    fMaps.Unlock;
  end;

  ColumnBox_SingleMaps.TopIndex := PrevTop;
  if aJumpToSelected
  and not InRange(ColumnBox_SingleMaps.ItemIndex - ColumnBox_SingleMaps.TopIndex, 0, ColumnBox_SingleMaps.GetVisibleRows - 1)
  then
    if ColumnBox_SingleMaps.ItemIndex < ColumnBox_SingleMaps.TopIndex + ColumnBox_SingleMaps.GetVisibleRows - 1 then
      ColumnBox_SingleMaps.TopIndex := ColumnBox_SingleMaps.ItemIndex
    else
    if ColumnBox_SingleMaps.ItemIndex > ColumnBox_SingleMaps.TopIndex + ColumnBox_SingleMaps.GetVisibleRows - 1 then
      ColumnBox_SingleMaps.TopIndex := ColumnBox_SingleMaps.ItemIndex - ColumnBox_SingleMaps.GetVisibleRows + 1;
end;


procedure TKMGUIMenuSingleMap.ListClick(Sender: TObject);
var
  MapId: Integer;
  I: Integer;
begin
  fMaps.Lock;
  try
    MapId := ColumnBox_SingleMaps.ItemIndex;

    //User could have clicked on empty space in list and we get -1 or unused MapId
    if not InRange(MapId, 0, fMaps.Count - 1) then
    begin
      fLastMapCRC := 0;
      Label_SingleTitle.Caption   := '';
      Memo_SingleDesc.Text        := '';

      MinimapView_Single.Hide;

      DropBox_SingleLoc.Clear;
      DropBox_SingleColor.Clear;
    end
    else
    begin
      //Prepare extra data we are about to display
      fMaps[MapId].LoadExtra;

      fLastMapCRC := fMaps[MapId].CRC;
      Label_SingleTitle.Caption   := fMaps[MapId].FileName;
      Memo_SingleDesc.Text        := fMaps[MapId].BigDesc;

      MinimapView_Single.Show;

      //Location
      DropBox_SingleLoc.Clear;
      for I := 0 to fMaps[MapId].PlayerCount - 1 do
      if fMaps[MapId].CanBeHuman[I] or ALLOW_TAKE_AI_PLAYERS then
        DropBox_SingleLoc.Add(fMaps[MapId].LocationName(I), I);

      DropBox_SingleLoc.SelectByTag(fMaps[MapId].DefaultHuman);

      //Color
      //Fill in colors for each map individually
      //I plan to skip colors that are similar to those on a map already
      DropBox_SingleColor.Clear;
      for I := Low(MP_TEAM_COLORS) to High(MP_TEAM_COLORS) do
        DropBox_SingleColor.Add(MakeListRow([''], [MP_TEAM_COLORS[I]], [MakePic(rxGuiMain, 30)]));
      DropBox_SingleColor.ItemIndex := 0; //Select default
    end;

    //Block options if there's nothing to choose there
    DropBox_SingleLoc.Enabled := DropBox_SingleLoc.Count > 1;
    MinimapView_Single.ShowLocs := DropBox_SingleLoc.Count > 1;
    DropBox_SingleColor.Enabled := DropBox_SingleColor.Count > 1;

    OptionsChange(nil);
  finally
    fMaps.Unlock;
  end;
end;


procedure TKMGUIMenuSingleMap.OptionsChange(Sender: TObject);
begin
  if DropBox_SingleLoc.ItemIndex <> -1 then
    fSingleLoc := DropBox_SingleLoc.GetSelectedTag
  else
    fSingleLoc := -1;

  if InRange(DropBox_SingleColor.ItemIndex + 1, Low(MP_TEAM_COLORS), High(MP_TEAM_COLORS)) then
    fSingleColor := MP_TEAM_COLORS[DropBox_SingleColor.ItemIndex + 1]
  else
    fSingleColor := MP_TEAM_COLORS[1];

  Update;
end;


procedure TKMGUIMenuSingleMap.Update;
const
  GoalCondPic: array [TGoalCondition] of Word = (
    41, 39, 592, 38, 62, 41, 303, 141, 312);
var
  I,J,K: Integer;
  MapId: Integer;
  M: TKMapInfo;
  G: TKMMapGoalInfo;
begin
  //Clear all so that later we fill only used
  for I := 0 to MAX_UI_GOALS - 1 do
  begin
    Image_SingleVictGoal[I].TexID := 0;
    Label_SingleVictGoal[I].Caption := '';
    Image_SingleVictGoalSt[I].Hide;
    Image_SingleSurvGoal[I].TexID := 0;
    Label_SingleSurvGoal[I].Caption := '';
    Image_SingleSurvGoalSt[I].Hide;
  end;
  for I := 0 to MAX_PLAYERS - 1 do
  begin
    Image_SingleAllies[I].Hide;
    Image_SingleEnemies[I].Hide;
  end;
  Button_SingleStart.Disable;

  if (fSingleLoc <> -1) and (ColumnBox_SingleMaps.ItemIndex <> -1) then
  begin
    MapId := ColumnBox_SingleMaps.ItemIndex;
    M := fMaps[MapId];

    //Refresh minimap with selected location and player color
    fMinimap.LoadFromMission(M.FullPath('.dat'), [TPlayerIndex(fSingleLoc)]);
    fMinimap.PlayerColors[fSingleLoc] := fSingleColor;
    fMinimap.Update(False);
    MinimapView_Single.SetMinimap(fMinimap);

    //Populate goals section
    for I := 0 to Min(MAX_UI_GOALS, M.GoalsVictoryCount[fSingleLoc]) - 1 do
    begin
      G := M.GoalsVictory[fSingleLoc,I];
      Image_SingleVictGoal[I].TexID := GoalCondPic[G.Cond];
      Image_SingleVictGoal[I].FlagColor := fSingleColor;
      Image_SingleVictGoalSt[I].Show;
      Label_SingleVictGoal[I].Caption := IntToStr(G.Play + 1);
    end;
    for I := 0 to Min(MAX_UI_GOALS, M.GoalsSurviveCount[fSingleLoc]) - 1 do
    begin
      G := M.GoalsSurvive[fSingleLoc,I];
      Image_SingleSurvGoal[I].TexID := GoalCondPic[G.Cond];
      Image_SingleSurvGoal[I].FlagColor := fSingleColor;
      Image_SingleSurvGoalSt[I].Show;
      Label_SingleSurvGoal[I].Caption := IntToStr(G.Play + 1);
    end;

    //Populate alliances section
    J := 0; K := 0;
    for I := 0 to M.PlayerCount - 1 do
    if I <> fSingleLoc then
    begin
      case M.Alliances[fSingleLoc, I] of
        at_Enemy: begin
                    Image_SingleEnemies[J].Show;
                    Image_SingleEnemies[J].FlagColor := M.FlagColors[I];
                    Inc(J);
                  end;
        at_Ally:  begin
                    Image_SingleAllies[K].Show;
                    Image_SingleAllies[K].FlagColor := M.FlagColors[I];
                    Inc(K);
                  end;
      end;
    end;

    Button_SingleStart.Enable;
  end;
end;


procedure TKMGUIMenuSingleMap.StartClick(Sender: TObject);
var
  I: Integer;
begin
  //This is also called by double clicking on a list entry
  if not Button_SingleStart.Enabled then
    Exit;

  fMaps.Lock;
  for I := 0 to fMaps.Count - 1 do
    if fLastMapCRC = fMaps[I].CRC then
    begin
      fMaps.Unlock; //We are about to exit so unlock now
      //Scan should be terminated, as it is no longer needed
      fMaps.TerminateScan;

      //Provide mission FileName mask and title here
      fGameApp.NewSingleMap(fMaps[I].FullPath('.dat'), fMaps[I].FileName, fSingleLoc, fSingleColor);
      Exit;
    end;
  Assert(False); //We should NOT reach here, since we checked that the start button was enabled
end;


procedure TKMGUIMenuSingleMap.ListSort(aColumn: Integer);
var
  Method: TMapsSortMethod;
begin
  //Set Descending order by default and invert it if same column selected again
  case aColumn of
    0:  if fMaps.SortMethod = smByModeDesc then
          Method := smByModeAsc
        else
          Method := smByModeDesc;
    1:
        if fMaps.SortMethod = smByPlayersDesc then
          Method := smByPlayersAsc
        else
          Method := smByPlayersDesc;
    2:  if fMaps.SortMethod = smByNameDesc then
          Method := smByNameAsc
        else
          Method := smByNameDesc;
    3:  if fMaps.SortMethod = smBySizeDesc then
          Method := smBySizeAsc
        else
          Method := smBySizeDesc;
    else
        Method := smByNameAsc; //Default
  end;

  //Start sorting and wait for SortComplete event
  fMaps.Sort(Method, SortUpdate);
end;


procedure TKMGUIMenuSingleMap.MinimapLocClick(aValue: Integer);
begin
  fSingleLoc := aValue;

  DropBox_SingleLoc.SelectByTag(fSingleLoc);

  Update;
end;


procedure TKMGUIMenuSingleMap.Show;
begin
  //Stop current now scan so it can't add a map after we clear the list
  fMaps.TerminateScan;

  //Remove any old entries from UI
  ListClear;

  //Initiate refresh and process each new map added
  fMaps.Refresh(ScanUpdate);
  Panel_Single.Show;
end;


procedure TKMGUIMenuSingleMap.BackClick(Sender: TObject);
begin
  //Scan should be terminated, it is no longer needed
  fMaps.TerminateScan;

  fOnPageChange(Self, gpSingleplayer, '');
end;


procedure TKMGUIMenuSingleMap.UpdateState(aTickCount: Cardinal);
begin
  if fMaps <> nil then
    fMaps.UpdateState;
end;


end.
