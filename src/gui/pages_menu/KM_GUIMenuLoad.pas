unit KM_GUIMenuLoad;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, Math, SysUtils,
  KM_CommonUtils, KM_Controls, KM_Saves, KM_InterfaceDefaults, KM_Minimap, KM_Defaults;


type
  TKMMenuLoad = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText;

    fSaves: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLastSaveFileName: String; //Name of selected save

    function CanLoadSave: Boolean;
    procedure UpdateUI;
    procedure LoadMinimap;
    procedure SetLastSaveFileName(aFileName: String = '');
    procedure LoadClick(Sender: TObject);
    procedure Load_Delete_Click(Sender: TObject);
    procedure Load_ListClick(Sender: TObject);
    procedure Load_ScanUpdate(Sender: TObject);
    procedure Load_ScanComplete(Sender: TObject);
    procedure Load_SortUpdate(Sender: TObject);
    procedure Load_RefreshList(aJumpToSelected:Boolean);
    procedure Load_Sort(aIndex: Integer);
    procedure Load_DeleteConfirmation(aVisible:boolean);
    procedure BackClick(Sender: TObject);
    procedure EscKeyDown(Sender: TObject);
    procedure KeyDown(Key: Word; Shift: TShiftState);
  protected
    Panel_Load: TKMPanel;
    ColumnBox_Load: TKMColumnBox;
    Button_Load: TKMButton;
    Button_Delete: TKMButton;
    Button_LoadBack:TKMButton;
    MinimapView_Load: TKMMinimapView;

    //PopUp Menus
    PopUp_Delete: TKMPopUpMenu;
      Image_Delete: TKMImage;
      Label_DeleteConfirmTitle, Label_DeleteConfirm: TKMLabel;
      Button_DeleteYes, Button_DeleteNo: TKMButton;

  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_GameApp, KM_RenderUI, KM_ResFonts, KM_Pics;


{ TKMGUIMenuLoad }
constructor TKMMenuLoad.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  OnKeyDown := KeyDown;
  OnEscKeyDown := EscKeyDown;

  fMinimap := TKMMinimap.Create(True, True);
  fSaves := TKMSavesCollection.Create;

  Panel_Load := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Load.AnchorsStretch;

    TKMLabel.Create(Panel_Load, aParent.Width div 2, 50, gResTexts[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    ColumnBox_Load := TKMColumnBox.Create(Panel_Load, 22, 86, 770, 485, fnt_Metal, bsMenu);
    ColumnBox_Load.Anchors := [anLeft,anTop,anBottom];
    ColumnBox_Load.SetColumns(fnt_Outline, ['', gResTexts[TX_MENU_LOAD_FILE], gResTexts[TX_MENU_LOAD_DATE], gResTexts[TX_MENU_LOAD_DESCRIPTION]], [0, 22, 250, 430]);
    ColumnBox_Load.SearchColumn := 1;
    ColumnBox_Load.OnColumnClick := Load_Sort;
    ColumnBox_Load.OnChange := Load_ListClick;
    ColumnBox_Load.OnDoubleClick := LoadClick;

    Button_Load := TKMButton.Create(Panel_Load, 337, 590, 350, 30, gResTexts[TX_MENU_LOAD_LOAD], bsMenu);
    Button_Load.Anchors := [anLeft,anBottom];
    Button_Load.OnClick := LoadClick;

    Button_Delete := TKMButton.Create(Panel_Load, 337, 624, 350, 30, gResTexts[TX_MENU_LOAD_DELETE], bsMenu);
    Button_Delete.Anchors := [anLeft,anBottom];
    Button_Delete.OnClick := Load_Delete_Click;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 700, 350, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_LoadBack.Anchors := [anLeft,anBottom];
    Button_LoadBack.OnClick := BackClick;

    with TKMBevel.Create(Panel_Load, 805, 226, 199, 199) do Anchors := [anLeft];
    MinimapView_Load := TKMMinimapView.Create(Panel_Load, 809, 230, 191, 191);
    MinimapView_Load.Anchors := [anLeft];

    //Delete PopUp
    PopUp_Delete := TKMPopUpMenu.Create(Panel_Load, 450);
    PopUp_Delete.Height := 200;
    // Keep the pop-up centered
    PopUp_Delete.AnchorsCenter;
    PopUp_Delete.Left := (Panel_Load.Width div 2) - (PopUp_Delete.Width div 2);
    PopUp_Delete.Top := (Panel_Load.Height div 2) - 90;

      TKMBevel.Create(PopUp_Delete, -1000,  -1000, 4000, 4000);

      Image_Delete := TKMImage.Create(PopUp_Delete, 0, 0, PopUp_Delete.Width, PopUp_Delete.Height, 15, rxGuiMain);
      Image_Delete.ImageStretch;

      Label_DeleteConfirmTitle := TKMLabel.Create(PopUp_Delete, PopUp_Delete.Width div 2, 40, gResTexts[TX_MENU_LOAD_DELETE], fnt_Outline, taCenter);
      Label_DeleteConfirmTitle.Anchors := [anLeft,anBottom];

      Label_DeleteConfirm := TKMLabel.Create(PopUp_Delete, PopUp_Delete.Width div 2, 85, gResTexts[TX_MENU_LOAD_DELETE_CONFIRM], fnt_Metal, taCenter);
      Label_DeleteConfirm.Anchors := [anLeft,anBottom];

      Button_DeleteYes := TKMButton.Create(PopUp_Delete, 20, 155, 195, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
      Button_DeleteYes.Anchors := [anLeft,anBottom];
      Button_DeleteYes.OnClick := Load_Delete_Click;

      Button_DeleteNo  := TKMButton.Create(PopUp_Delete, 235, 155, 195, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
      Button_DeleteNo.Anchors := [anLeft,anBottom];
      Button_DeleteNo.OnClick := Load_Delete_Click;

end;


destructor TKMMenuLoad.Destroy;
begin
  fSaves.Free;
  fMinimap.Free;

  inherited;
end;


function TKMMenuLoad.CanLoadSave: Boolean;
begin
  Result := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1)
             and fSaves[ColumnBox_Load.ItemIndex].IsValid
end;


procedure TKMMenuLoad.UpdateUI;
begin
  Button_Delete.Enabled := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1);
  Button_Load.Enabled := CanLoadSave;
end;


procedure TKMMenuLoad.LoadMinimap;
begin
  MinimapView_Load.Hide; //Hide by default, then show it if we load the map successfully
  if CanLoadSave and fSaves[ColumnBox_Load.ItemIndex].LoadMinimap(fMinimap) then
  begin
    MinimapView_Load.SetMinimap(fMinimap);
    MinimapView_Load.Show;
  end;
end;


procedure TKMMenuLoad.Load_ListClick(Sender: TObject);
begin
  fSaves.Lock;
  try
    //Hide delete confirmation if player has selected a different savegame item
    if Sender = ColumnBox_Load then
      Load_DeleteConfirmation(False);

    UpdateUI;

    if InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then
      SetLastSaveFileName(fSaves[ColumnBox_Load.ItemIndex].FileName)
    else
      SetLastSaveFileName;

    LoadMinimap;
  finally
    fSaves.Unlock;
  end;
end;


procedure TKMMenuLoad.LoadClick(Sender: TObject);
begin
  if not Button_Load.Enabled then exit; //This is also called by double clicking
  if not InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  gGameApp.NewSingleSave(fSaves[ColumnBox_Load.ItemIndex].FileName);
end;


procedure TKMMenuLoad.SetLastSaveFileName(aFileName: String = '');
begin
  fLastSaveFileName := aFileName;
  gGameApp.GameSettings.MenuSPSaveFileName := aFileName;
end;


procedure TKMMenuLoad.Load_Delete_Click(Sender: TObject);
var
  PreviouslySelected, NewSelected: Integer;
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

    if ColumnBox_Load.RowCount > 1 then
    begin
      NewSelected := EnsureRange(PreviouslySelected, 0, ColumnBox_Load.RowCount - 2);
      SetLastSaveFileName(fSaves[NewSelected].FileName);
    end else
      SetLastSaveFileName; //there are no saves, nothing to select

    Load_RefreshList(True);
  end;
end;


procedure TKMMenuLoad.Load_ScanUpdate(Sender: TObject);
begin
  Load_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMenuLoad.Load_ScanComplete(Sender: TObject);
begin
  Load_RefreshList(True); //After scan complete jump to selected item
end;


procedure TKMMenuLoad.Load_SortUpdate(Sender: TObject);
begin
  Load_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMenuLoad.Load_RefreshList(aJumpToSelected:Boolean);
var I, PrevTop: Integer;
    Row: TKMListRow;
begin
  PrevTop := ColumnBox_Load.TopIndex;
  ColumnBox_Load.Clear;

  fSaves.Lock;
  try
    for I := 0 to fSaves.Count - 1 do
    begin
      Row := MakeListRow(['', fSaves[i].FileName, fSaves[i].Info.GetSaveTimestamp, fSaves[I].Info.GetTitleWithTime],
                         [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF]);
      Row.Cells[0].Pic := MakePic(rxGui, 657 + Byte(fSaves[I].Info.MissionMode = mm_Tactic));
      ColumnBox_Load.AddItem(Row);
    end;

    //IDs of saves could changed, so use CRC to check which one was selected
    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].FileName = fLastSaveFileName) then
      begin
        ColumnBox_Load.ItemIndex := I;
        LoadMinimap;
      end;
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

  UpdateUI;
end;


procedure TKMMenuLoad.Load_Sort(aIndex: Integer);
begin
  case ColumnBox_Load.SortIndex of
    0:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByModeDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByModeAsc, Load_SortUpdate);
    //Sorting by filename goes A..Z by default
    1:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Load_SortUpdate);
    //Sorting by description goes Old..New by default
    2:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByDateDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByDateAsc, Load_SortUpdate);
    //Sorting by description goes A..Z by default
    3:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByDescriptionAsc, Load_SortUpdate);
  end;
end;


//Shortcut to choose if DeleteConfirmation should be displayed or hid
procedure TKMMenuLoad.Load_DeleteConfirmation(aVisible: Boolean);
begin
  if aVisible then
  begin
    PopUp_Delete.Show;
    ColumnBox_Load.Focusable := False;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_Load);
  end else begin
    PopUp_Delete.Hide;
    ColumnBox_Load.Focusable := True;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_Load);
  end;
end;


procedure TKMMenuLoad.KeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:  if Button_DeleteYes.IsClickable then
                  Load_Delete_Click(Button_DeleteYes);
  end;
end;


procedure TKMMenuLoad.EscKeyDown(Sender: TObject);
begin
  if Button_DeleteNo.IsClickable then
    Load_Delete_Click(Button_DeleteNo)
  else
    BackClick(nil);
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
  ColumnBox_Load.Clear; //clear the list
  Load_DeleteConfirmation(False);
  UpdateUI;
  fLastSaveFileName := gGameApp.GameSettings.MenuSPSaveFileName;

  //Initiate refresh and process each new save added
  fSaves.Refresh(Load_ScanUpdate, False, Load_ScanComplete);

  //Apply sorting from last time we were on this page
  Load_Sort(ColumnBox_Load.SortIndex);

  Panel_Load.Show;
end;


procedure TKMMenuLoad.UpdateState;
begin
  fSaves.UpdateState;
end;


end.
