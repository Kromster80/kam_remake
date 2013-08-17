unit KM_GUIMenuLoad;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math,
  KM_Controls, KM_Saves, KM_InterfaceDefaults, KM_Minimap;


type
  TKMMenuLoad = class
  private
    fOnPageChange: TGUIEventText;

    fSaves: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLastSaveCRC: Cardinal; //CRC of selected save

    procedure LoadClick(Sender: TObject);
    procedure Load_Delete_Click(Sender: TObject);
    procedure Load_ListClick(Sender: TObject);
    procedure Load_ScanUpdate(Sender: TObject);
    procedure Load_SortUpdate(Sender: TObject);
    procedure Load_RefreshList(aJumpToSelected:Boolean);
    procedure Load_Sort(aIndex: Integer);
    procedure Load_DeleteConfirmation(aVisible:boolean);
    procedure BackClick(Sender: TObject);
  protected
    Panel_Load: TKMPanel;
    ColumnBox_Load: TKMColumnBox;
    Button_Load: TKMButton;
    Button_Delete: TKMButton;
    Label_DeleteConfirm: TKMLabel;
    Button_DeleteYes, Button_DeleteNo: TKMButton;
    Button_LoadBack:TKMButton;
    MinimapView_Load: TKMMinimapView;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show;
    procedure UpdateState;
  end;


implementation
uses KM_ResTexts, KM_GameApp, KM_RenderUI, KM_ResFonts;


{ TKMGUIMenuLoad }
constructor TKMMenuLoad.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  fMinimap := TKMMinimap.Create(True, False, True);
  fSaves := TKMSavesCollection.Create;

  Panel_Load := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Load.Stretch;

    TKMLabel.Create(Panel_Load, aParent.Width div 2, 50, gResTexts[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    ColumnBox_Load := TKMColumnBox.Create(Panel_Load, 62, 86, 700, 485, fnt_Metal, bsMenu);
    ColumnBox_Load.Anchors := [akLeft,akTop,akBottom];
    ColumnBox_Load.SetColumns(fnt_Outline, [gResTexts[TX_MENU_LOAD_FILE], gResTexts[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    ColumnBox_Load.SearchColumn := 0;
    ColumnBox_Load.OnColumnClick := Load_Sort;
    ColumnBox_Load.OnChange := Load_ListClick;
    ColumnBox_Load.OnDoubleClick := LoadClick;

    Button_Load := TKMButton.Create(Panel_Load,337,590,350,30,gResTexts[TX_MENU_LOAD_LOAD], bsMenu);
    Button_Load.Anchors := [akLeft,akBottom];
    Button_Load.OnClick := LoadClick;

    Button_Delete := TKMButton.Create(Panel_Load, 337, 624, 350, 30, gResTexts[TX_MENU_LOAD_DELETE], bsMenu);
    Button_Delete.Anchors := [akLeft,akBottom];
    Button_Delete.OnClick := Load_Delete_Click;

    Label_DeleteConfirm := TKMLabel.Create(Panel_Load, aParent.Width div 2, 634, gResTexts[TX_MENU_LOAD_DELETE_CONFIRM], fnt_Outline, taCenter);
    Label_DeleteConfirm.Anchors := [akLeft,akBottom];
    Button_DeleteYes := TKMButton.Create(Panel_Load, 337, 660, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
    Button_DeleteYes.Anchors := [akLeft,akBottom];
    Button_DeleteYes.OnClick := Load_Delete_Click;
    Button_DeleteNo  := TKMButton.Create(Panel_Load, 517, 660, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
    Button_DeleteNo.Anchors := [akLeft,akBottom];
    Button_DeleteNo.OnClick := Load_Delete_Click;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 700, 350, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_LoadBack.Anchors := [akLeft,akBottom];
    Button_LoadBack.OnClick := BackClick;

    with TKMBevel.Create(Panel_Load, 785, 226, 199, 199) do Anchors := [akLeft];
    MinimapView_Load := TKMMinimapView.Create(Panel_Load,789,230,191,191);
    MinimapView_Load.Anchors := [akLeft];
end;


destructor TKMMenuLoad.Destroy;
begin
  fSaves.Free;
  fMinimap.Free;

  inherited;
end;


procedure TKMMenuLoad.Load_ListClick(Sender: TObject);
begin
  fSaves.Lock;
    //Hide delete confirmation if player has selected a different savegame item
    if Sender = ColumnBox_Load then
      Load_DeleteConfirmation(False);

    Button_Delete.Enabled := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1);
    Button_Load.Enabled := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1)
                           and fSaves[ColumnBox_Load.ItemIndex].IsValid;

    if InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then
      fLastSaveCRC := fSaves[ColumnBox_Load.ItemIndex].CRC
    else
      fLastSaveCRC := 0;

    MinimapView_Load.Hide; //Hide by default, then show it if we load the map successfully
    if Button_Load.Enabled and fSaves[ColumnBox_Load.ItemIndex].LoadMinimap(fMinimap) then
    begin
      MinimapView_Load.SetMinimap(fMinimap);
      MinimapView_Load.Show;
    end;
  fSaves.Unlock;
end;


procedure TKMMenuLoad.LoadClick(Sender: TObject);
begin
  if not Button_Load.Enabled then exit; //This is also called by double clicking
  if not InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGameApp.NewSingleSave(fSaves[ColumnBox_Load.ItemIndex].FileName);
end;


procedure TKMMenuLoad.Load_Delete_Click(Sender: TObject);
var
  PreviouslySelected: Integer;
begin
  if ColumnBox_Load.ItemIndex = -1 then Exit;

  if Sender = Button_Delete then
    Load_DeleteConfirmation(true);

  if (Sender = Button_DeleteYes) or (Sender = Button_DeleteNo) then
    Load_DeleteConfirmation(false); //Hide confirmation anyways

  //Delete the savegame
  if Sender = Button_DeleteYes then
  begin
    PreviouslySelected := ColumnBox_Load.ItemIndex;
    fSaves.DeleteSave(ColumnBox_Load.ItemIndex);
    Load_RefreshList(False);
    if ColumnBox_Load.RowCount > 0 then
      ColumnBox_Load.ItemIndex := EnsureRange(PreviouslySelected, 0, ColumnBox_Load.RowCount - 1)
    else
      ColumnBox_Load.ItemIndex := -1; //there are no saves, nothing to select
    Load_ListClick(ColumnBox_Load);
  end;
end;


procedure TKMMenuLoad.Load_ScanUpdate(Sender: TObject);
begin
  Load_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMenuLoad.Load_SortUpdate(Sender: TObject);
begin
  Load_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMenuLoad.Load_RefreshList(aJumpToSelected:Boolean);
var I, PrevTop: Integer;
begin
  PrevTop := ColumnBox_Load.TopIndex;
  ColumnBox_Load.Clear;

  fSaves.Lock;
  try
    for I := 0 to fSaves.Count - 1 do
      ColumnBox_Load.AddItem(MakeListRow(
                        [fSaves[i].FileName, fSaves[I].Info.GetTitleWithTime],
                        [$FFFFFFFF, $FFFFFFFF]));

    //IDs of saves could changed, so use CRC to check which one was selected
    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fLastSaveCRC) then
        ColumnBox_Load.ItemIndex := I;
  finally
    fSaves.Unlock;
  end;

  ColumnBox_Load.TopIndex := PrevTop;

  if aJumpToSelected and (ColumnBox_Load.ItemIndex <> -1)
  and not InRange(ColumnBox_Load.ItemIndex - ColumnBox_Load.TopIndex, 0, ColumnBox_Load.GetVisibleRows - 1)
  then
  begin
    if ColumnBox_Load.ItemIndex < ColumnBox_Load.TopIndex + ColumnBox_Load.GetVisibleRows - 1 then
      ColumnBox_Load.TopIndex := ColumnBox_Load.ItemIndex
    else
      if ColumnBox_Load.ItemIndex > ColumnBox_Load.TopIndex + ColumnBox_Load.GetVisibleRows - 1 then
        ColumnBox_Load.TopIndex := ColumnBox_Load.ItemIndex - ColumnBox_Load.GetVisibleRows + 1;
  end;

  Load_ListClick(nil);
end;


procedure TKMMenuLoad.Load_Sort(aIndex: Integer);
begin
  case ColumnBox_Load.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Load_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByDescriptionAsc, Load_SortUpdate);
  end;
end;


//Shortcut to choose if DeleteConfirmation should be displayed or hid
procedure TKMMenuLoad.Load_DeleteConfirmation(aVisible:boolean);
begin
  Label_DeleteConfirm.Visible := aVisible;
  Button_DeleteYes.Visible := aVisible;
  Button_DeleteNo.Visible := aVisible;
  Button_Delete.Visible := not aVisible;
end;


procedure TKMMenuLoad.BackClick(Sender: TObject);
begin
  //Scan should be terminated, it is no longer needed
  fSaves.TerminateScan;

  fOnPageChange(gpSingleplayer);
end;


procedure TKMMenuLoad.Show;
begin
  //Stop current scan so it can't add a save after we clear the list
  fSaves.TerminateScan;
  fLastSaveCRC := 0;
  ColumnBox_Load.Clear; //clear the list
  Load_DeleteConfirmation(False);
  Load_ListClick(nil);

  //Initiate refresh and process each new save added
  fSaves.Refresh(Load_ScanUpdate, False);

  //Apply sorting from last time we were on this page
  Load_Sort(ColumnBox_Load.SortIndex);

  Panel_Load.Show;
end;


procedure TKMMenuLoad.UpdateState;
begin
  fSaves.UpdateState;
end;


end.
