unit KM_GUIMenuReplays;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math,
  KM_Controls, KM_Saves, KM_InterfaceDefaults, KM_Minimap;


type
  TKMMenuReplays = class
  private
    fOnPageChange: TGUIEventText;

    fSaves: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLastSaveCRC: Cardinal; //CRC of selected save

    procedure Replays_ListClick(Sender: TObject);
    procedure Replay_TypeChange(Sender: TObject);
    procedure Replays_ScanUpdate(Sender: TObject);
    procedure Replays_SortUpdate(Sender: TObject);
    procedure Replays_RefreshList(aJumpToSelected:Boolean);
    procedure Replays_Sort(aIndex: Integer);
    procedure Replays_Play(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      ColumnBox_Replays: TKMColumnBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
      MinimapView_Replay: TKMMinimapView;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show;
    procedure UpdateState;
  end;


implementation
uses KM_ResTexts, KM_GameApp, KM_RenderUI, KM_ResFonts;


{ TKMGUIMenuReplays }
constructor TKMMenuReplays.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  fSaves := TKMSavesCollection.Create;
  fMinimap := TKMMinimap.Create(False, True);

  Panel_Replays := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Replays.AnchorsStretch;

    TKMLabel.Create(Panel_Replays, aParent.Width div 2, 50, gResTexts[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 86, 680, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,94,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    ColumnBox_Replays := TKMColumnBox.Create(Panel_Replays, 62, 150, 680, 485, fnt_Metal, bsMenu);
    ColumnBox_Replays.SetColumns(fnt_Outline, [gResTexts[TX_MENU_LOAD_FILE], gResTexts[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    ColumnBox_Replays.Anchors := [akLeft,akTop,akBottom];
    ColumnBox_Replays.SearchColumn := 0;
    ColumnBox_Replays.OnChange := Replays_ListClick;
    ColumnBox_Replays.OnColumnClick := Replays_Sort;
    ColumnBox_Replays.OnDoubleClick := Replays_Play;

    with TKMBevel.Create(Panel_Replays, 763, 290, 199, 199) do Anchors := [akLeft];
    MinimapView_Replay := TKMMinimapView.Create(Panel_Replays,767,294,191,191);
    MinimapView_Replay.Anchors := [akLeft];

    Button_ReplaysBack := TKMButton.Create(Panel_Replays, 62, 660, 335, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_ReplaysBack.Anchors := [akLeft,akBottom];
    Button_ReplaysBack.OnClick := BackClick;

    Button_ReplaysPlay := TKMButton.Create(Panel_Replays,407, 660, 335, 30, gResTexts[TX_MENU_VIEW_REPLAY], bsMenu);
    Button_ReplaysPlay.Anchors := [akLeft,akBottom];
    Button_ReplaysPlay.OnClick := Replays_Play;
end;


destructor TKMMenuReplays.Destroy;
begin
  fSaves.Free;
  fMinimap.Free;

  inherited;
end;


procedure TKMMenuReplays.Replays_ListClick(Sender: TObject);
var ID: Integer;
begin
  fSaves.Lock;
    ID := ColumnBox_Replays.ItemIndex;

    Button_ReplaysPlay.Enabled := InRange(ID, 0, fSaves.Count-1)
                                  and fSaves[ID].IsValid
                                  and fSaves[ID].IsReplayValid;

    if InRange(ID, 0, fSaves.Count-1) then
      fLastSaveCRC := fSaves[ID].CRC
    else
      fLastSaveCRC := 0;

    MinimapView_Replay.Hide; //Hide by default, then show it if we load the map successfully
    if Button_ReplaysPlay.Enabled and fSaves[ID].LoadMinimap(fMinimap) then
    begin
      MinimapView_Replay.SetMinimap(fMinimap);
      MinimapView_Replay.Show;
    end;
  fSaves.Unlock;
end;


procedure TKMMenuReplays.Replay_TypeChange(Sender: TObject);
begin
  fSaves.TerminateScan;
  fLastSaveCRC := 0;
  ColumnBox_Replays.Clear;
  Replays_ListClick(nil);
  fSaves.Refresh(Replays_ScanUpdate, (Radio_Replays_Type.ItemIndex = 1));
end;


procedure TKMMenuReplays.Replays_ScanUpdate(Sender: TObject);
begin
  Replays_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMenuReplays.Replays_SortUpdate(Sender: TObject);
begin
  Replays_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMenuReplays.Replays_RefreshList(aJumpToSelected: Boolean);
var
  I, PrevTop: Integer;
begin
  PrevTop := ColumnBox_Replays.TopIndex;
  ColumnBox_Replays.Clear;

  fSaves.Lock;
  try
    for I := 0 to fSaves.Count - 1 do
      ColumnBox_Replays.AddItem(MakeListRow(
                           [fSaves[I].FileName, fSaves[I].Info.GetTitleWithTime],
                           [$FFFFFFFF, $FFFFFFFF]));

    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fLastSaveCRC) then
        ColumnBox_Replays.ItemIndex := I;

  finally
    fSaves.Unlock;
  end;

  ColumnBox_Replays.TopIndex := PrevTop;

  if aJumpToSelected and (ColumnBox_Replays.ItemIndex <> -1)
  and not InRange(ColumnBox_Replays.ItemIndex - ColumnBox_Replays.TopIndex, 0, ColumnBox_Replays.GetVisibleRows-1)
  then
    if ColumnBox_Replays.ItemIndex < ColumnBox_Replays.TopIndex then
      ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex
    else
    if ColumnBox_Replays.ItemIndex > ColumnBox_Replays.TopIndex + ColumnBox_Replays.GetVisibleRows - 1 then
      ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex - ColumnBox_Replays.GetVisibleRows + 1;
end;


procedure TKMMenuReplays.Replays_Sort(aIndex: Integer);
begin
  case ColumnBox_Replays.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if ColumnBox_Replays.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Replays_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Replays_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if ColumnBox_Replays.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Replays_SortUpdate)
        else
          fSaves.Sort(smByDescriptionAsc, Replays_SortUpdate);
  end;
end;


procedure TKMMenuReplays.Replays_Play(Sender: TObject);
var
  ID: Integer;
begin
  if not Button_ReplaysPlay.Enabled then exit; //This is also called by double clicking

  ID := ColumnBox_Replays.ItemIndex;
  if not InRange(ID, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGameApp.NewReplay(fSaves[ID].Path + fSaves[ID].FileName + '.bas');
end;


procedure TKMMenuReplays.BackClick(Sender: TObject);
begin
  //Scan should be terminated, it is no longer needed
  fSaves.TerminateScan;

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuReplays.Show;
begin
  //Copy/Pasted from SwitchPage for now (needed that for ResultsMP BackClick)
  //Probably needs some cleanup when we have GUIMenuReplays
  fLastSaveCRC := 0;
  Radio_Replays_Type.ItemIndex := 0; //we always show SP replays on start
  Replay_TypeChange(nil); //Select SP as this will refresh everything
  Replays_Sort(ColumnBox_Replays.SortIndex); //Apply sorting from last time we were on this page
  Panel_Replays.Show;
end;


procedure TKMMenuReplays.UpdateState;
begin
  fSaves.UpdateState;
end;


end.
