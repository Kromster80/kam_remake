unit KM_GUIMenuMapEditor;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Maps, KM_Minimap,
  KM_InterfaceDefaults;


type
  TKMMenuMapEditor = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fMinimap: TKMMinimap;
    fMinimapLastListId: Integer;  // column id, on which last time minimap was loaded. Avoid multiple loads of same minimap, which could happen on every RefreshList
    fScanCompleted: Boolean;      // True, after scan was completed

    fSelectedMapInfo: TKMFileIdentInfo; // Identification info about last selected map

    procedure StartClick(Sender: TObject);
    procedure MapTypeChange(Sender: TObject);
    procedure SizeChangeByRadio(Sender: TObject);
    procedure SizeChangeByEdit(Sender: TObject);
    procedure UpdateRadioMapEdSizes;
    procedure ListUpdate;
    procedure UpdateUI;
    procedure SetSelectedMapInfo(aID: Integer = -1); overload;
    procedure SetSelectedMapInfo(aCRC: Cardinal; const aName: UnicodeString); overload;
    procedure ScanUpdate(Sender: TObject);
    procedure ScanTerminate(Sender: TObject);
    procedure SortUpdate(Sender: TObject);
    procedure RefreshList(aJumpToSelected:Boolean);
    procedure ColumnClick(aValue: Integer);
    function GetMaps: TKMapsCollection;
    procedure LoadMinimap(aID: Integer = -1);
    procedure ReadmeClick(Sender: TObject);
    procedure SelectMap(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure DeleteConfirm(aVisible: Boolean);
    procedure RenameClick(Sender: TObject);
    procedure Edit_Rename_Change(Sender: TObject);
    procedure RenameConfirm(aVisible: Boolean);
    procedure MoveConfirm(aVisible: Boolean);
    procedure MoveEditChange(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure EscKeyDown(Sender: TObject);
    procedure KeyDown(Key: Word; Shift: TShiftState);
  protected
    Panel_MapEd: TKMPanel;
      Panel_MapEdSizeXY: TKMPanel;
      Radio_MapEdSizeX, Radio_MapEdSizeY: TKMRadioGroup;
      Panel_MapEdLoad: TKMPanel;
      ColumnBox_MapEd: TKMColumnBox;
      Radio_MapEd_MapType: TKMRadioGroup;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load: TKMButton;
      NumEdit_MapSizeX: TKMNumericEdit;
      NumEdit_MapSizeY: TKMNumericEdit;

      MinimapView_MapEd: TKMMinimapView;

      Panel_LobbySetupDesc: TKMPanel;
        Label_MapType: TKMLabel;
        Memo_LobbyMapDesc: TKMMemo;
        Button_LobbySetupReadme: TKMButton;

      //PopUp Menus
      PopUp_Delete: TKMPopUpMenu;
        Image_Delete: TKMImage;
        Button_MapDelete, Button_MapDeleteConfirm, Button_MapDeleteCancel: TKMButton;
        Label_MapDeleteConfirmTitle, Label_MapDeleteConfirm: TKMLabel;

      PopUp_Rename: TKMPopUpMenu;
        Image_Rename: TKMImage;
        Label_RenameTitle, Label_RenameName: TKMLabel;
        Edit_Rename: TKMEdit;
        Button_MapRename, Button_MapRenameConfirm, Button_MapRenameCancel: TKMButton;

      PopUp_Move: TKMPopUpMenu;
        Image_Move: TKMImage;
        Button_MapMove, Button_MapMoveConfirm, Button_MapMoveCancel: TKMButton;
        Edit_MapMove: TKMEdit;
        Label_MoveExists: TKMLabel;
        CheckBox_MoveExists: TKMCheckBox;
        Label_MapMoveConfirmTitle, Label_MapMoveName: TKMLabel;

  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;
    procedure Show;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_GameApp, KM_RenderUI, KM_ResFonts, KM_InterfaceMapEditor, KM_Defaults, KM_Pics;

const
  MINIMAP_NOT_LOADED = -100; // smth, but not -1, as -1 is used for ColumnBox.ItemIndex, when no item is selected


const
  MAPSIZES_COUNT = 15;
  MapSize: array [1..MAPSIZES_COUNT] of Word = (32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256);


{ TKMGUIMainMapEditor }
constructor TKMMenuMapEditor.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  I: Integer;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := EscKeyDown;
  OnKeyDown := KeyDown;

  fMaps := TKMapsCollection.Create(mfSP);
  fMapsMP := TKMapsCollection.Create([mfMP, mfDL]);
  fMinimap := TKMMinimap.Create(True, True);

  Panel_MapEd:=TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MapEd.AnchorsStretch;
    Panel_MapEdSizeXY := TKMPanel.Create(Panel_MapEd, 60, 240, 220, 700);
    Panel_MapEdSizeXY.Anchors := [anLeft, anBottom];
      TKMLabel.Create(Panel_MapEdSizeXY, 6, 0, 188, 20, gResTexts[TX_MENU_NEW_MAP_SIZE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdSizeXY, 0, 20, 220, 406);
      TKMLabel.Create(Panel_MapEdSizeXY, 8, 27, 88, 20, gResTexts[TX_MENU_MAP_WIDTH], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_MapEdSizeXY, 118, 27, 88, 20, gResTexts[TX_MENU_MAP_HEIGHT], fnt_Outline, taLeft);

      Radio_MapEdSizeX := TKMRadioGroup.Create(Panel_MapEdSizeXY, 10, 52, 88, 332, fnt_Metal);
      Radio_MapEdSizeY := TKMRadioGroup.Create(Panel_MapEdSizeXY, 120, 52, 88, 332, fnt_Metal);
      for I := 1 to MAPSIZES_COUNT do
      begin
        Radio_MapEdSizeX.Add(IntToStr(MapSize[I]));
        Radio_MapEdSizeY.Add(IntToStr(MapSize[I]));
      end;
      Radio_MapEdSizeX.ItemIndex := 2; //64
      Radio_MapEdSizeY.ItemIndex := 2; //64

      Radio_MapEdSizeX.OnChange := SizeChangeByRadio;
      Radio_MapEdSizeY.OnChange := SizeChangeByRadio;
      NumEdit_MapSizeX := TKMNumericEdit.Create(Panel_MapEdSizeXY, 8, 392, MIN_MAP_SIZE, MAX_MAP_SIZE);
      NumEdit_MapSizeY := TKMNumericEdit.Create(Panel_MapEdSizeXY, 118, 392, MIN_MAP_SIZE, MAX_MAP_SIZE);
      NumEdit_MapSizeX.Anchors := [anLeft, anBottom];
      NumEdit_MapSizeY.Anchors := [anLeft, anBottom];
      NumEdit_MapSizeX.Value := 64;
      NumEdit_MapSizeY.Value := 64;
      NumEdit_MapSizeX.OnChange := SizeChangeByEdit;
      NumEdit_MapSizeY.OnChange := SizeChangeByEdit;

      Button_MapEd_Create := TKMButton.Create(Panel_MapEdSizeXY, 0, 432, 220, 30, gResTexts[TX_MENU_MAP_CREATE_NEW_MAP], bsMenu);
      Button_MapEd_Create.Anchors := [anLeft, anBottom];
      Button_MapEd_Create.OnClick := StartClick;

    Panel_MapEdLoad := TKMPanel.Create(Panel_MapEd, 320, 40, 620, 700);
    Panel_MapEdLoad.Anchors := [anLeft, anTop, anBottom];
      TKMLabel.Create(Panel_MapEdLoad, 6, 0, 288, 20, gResTexts[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdLoad, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEdLoad,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapTypeChange;
      ColumnBox_MapEd := TKMColumnBox.Create(Panel_MapEdLoad, 0, 80, 440, 506, fnt_Metal,  bsMenu);
      ColumnBox_MapEd.Anchors := [anLeft, anTop, anBottom];
      ColumnBox_MapEd.SetColumns(fnt_Outline, ['', gResTexts[TX_MENU_MAP_TITLE], '#', gResTexts[TX_MENU_MAP_SIZE]], [0, 22, 310, 340]);
      ColumnBox_MapEd.SearchColumn := 1;
      ColumnBox_MapEd.OnColumnClick := ColumnClick;
      ColumnBox_MapEd.OnChange := SelectMap;
      ColumnBox_MapEd.OnDoubleClick := StartClick;

      with TKMBevel.Create(Panel_MapEdLoad, 448, 104, 199, 199) do
        Anchors := [anLeft];
      MinimapView_MapEd := TKMMinimapView.Create(Panel_MapEdLoad, 452, 108, 191, 191);
      MinimapView_MapEd.Anchors := [anLeft];

      Panel_LobbySetupDesc := TKMPanel.Create(Panel_MapEdLoad, 448, 104+199+10, 199, 218);
      Panel_LobbySetupDesc.Anchors := [anLeft, anTop, anBottom];
        Label_MapType := TKMLabel.Create(Panel_LobbySetupDesc, 0, 0, '', fnt_Metal, taLeft);
        Memo_LobbyMapDesc := TKMMemo.Create(Panel_LobbySetupDesc, 0, 0, 199, 218, fnt_Game, bsMenu);
        Memo_LobbyMapDesc.Anchors := [anLeft,anTop,anBottom];
        Memo_LobbyMapDesc.AutoWrap := True;
        Memo_LobbyMapDesc.ItemHeight := 16;

        Button_LobbySetupReadme := TKMButton.Create(Panel_LobbySetupDesc, 0, 225, 199, 25, gResTexts[TX_LOBBY_VIEW_README], bsMenu);
        Button_LobbySetupReadme.Anchors := [anLeft,anBottom];
        Button_LobbySetupReadme.OnClick := ReadmeClick;
        Button_LobbySetupReadme.Hide;

      Button_MapEd_Load := TKMButton.Create(Panel_MapEdLoad, 0, 596, 440, 30, gResTexts[TX_MENU_MAP_LOAD_EXISTING], bsMenu);
      Button_MapEd_Load.Anchors := [anLeft, anBottom];
      Button_MapEd_Load.OnClick := StartClick;

      Button_MapMove := TKMButton.Create(Panel_MapEdLoad, 0, 632, 440, 30, gResTexts[TX_MENU_MAP_MOVE_DOWNLOAD], bsMenu);
      Button_MapMove.Anchors := [anLeft, anBottom];
      Button_MapMove.OnClick := MoveClick;
      Button_MapMove.Hide;

      Button_MapRename := TKMButton.Create(Panel_MapEdLoad, 0, 632, 440, 30, 'Rename Map', bsMenu); //Todo translate
      Button_MapRename.Anchors := [anLeft, anBottom];
      Button_MapRename.OnClick := RenameClick;

      Button_MapDelete := TKMButton.Create(Panel_MapEdLoad, 0, 668, 440, 30, gResTexts[TX_MENU_MAP_DELETE], bsMenu);
      Button_MapDelete.Anchors := [anLeft, anBottom];
      Button_MapDelete.OnClick := DeleteClick;

      Button_MapEdBack := TKMButton.Create(Panel_MapEd, 60, 708, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
      Button_MapEdBack.Anchors := [anLeft, anBottom];
      Button_MapEdBack.OnClick := BackClick;

      //Delete PopUp
      PopUp_Delete := TKMPopUpMenu.Create(Panel_MapEd, 450);
      PopUp_Delete.Height := 200;
      // Keep the pop-up centered
      PopUp_Delete.AnchorsCenter;
      PopUp_Delete.Left := (Panel_MapEd.Width div 2) - (PopUp_Delete.Width div 2);
      PopUp_Delete.Top := (Panel_MapEd.Height div 2) - 90;

        TKMBevel.Create(PopUp_Delete, -1000,  -1000, 4000, 4000);

        Image_Delete := TKMImage.Create(PopUp_Delete, 0, 0, PopUp_Delete.Width, PopUp_Delete.Height, 15, rxGuiMain);
        Image_Delete.ImageStretch;

        Label_MapDeleteConfirmTitle := TKMLabel.Create(PopUp_Delete, PopUp_Delete.Width div 2, 40, gResTexts[TX_MENU_MAP_DELETE], fnt_Outline, taCenter);
        Label_MapDeleteConfirmTitle.Anchors := [anLeft, anBottom];

        Label_MapDeleteConfirm := TKMLabel.Create(PopUp_Delete, PopUp_Delete.Width div 2, 85, gResTexts[TX_MENU_MAP_DELETE_CONFIRM], fnt_Metal, taCenter);
        Label_MapDeleteConfirm.Anchors := [anLeft, anBottom];

        Button_MapDeleteConfirm := TKMButton.Create(PopUp_Delete, 20, 155, 195, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
        Button_MapDeleteConfirm.Anchors := [anLeft, anBottom];
        Button_MapDeleteConfirm.OnClick := DeleteClick;

        Button_MapDeleteCancel  := TKMButton.Create(PopUp_Delete, 235, 155, 195, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
        Button_MapDeleteCancel.Anchors := [anLeft, anBottom];
        Button_MapDeleteCancel.OnClick := DeleteClick;

      PopUp_Rename := TKMPopUpMenu.Create(Panel_MapEd, 400);
      PopUp_Rename.Height := 200;
      // Keep the pop-up centered
      PopUp_Rename.AnchorsCenter;
      PopUp_Rename.Left := (Panel_MapEd.Width div 2) - (PopUp_Rename.Width div 2);
      PopUp_Rename.Top := (Panel_MapEd.Height div 2) - 90;

        TKMBevel.Create(PopUp_Rename, -1000,  -1000, 4000, 4000);

        Image_Rename := TKMImage.Create(PopUp_Rename, 0, 0, PopUp_Rename.Width, PopUp_Rename.Height, 15, rxGuiMain);
        Image_Rename.ImageStretch;

        Label_RenameTitle := TKMLabel.Create(PopUp_Rename, 20, 50, 360, 30, 'Rename Map', fnt_Outline, taCenter);
        Label_RenameTitle.Anchors := [anLeft,anBottom];

        Label_RenameName := TKMLabel.Create(PopUp_Rename, 25, 100, 60, 20, gResTexts[TX_MENU_REPLAY_RENAME_NAME], fnt_Metal, taLeft);
        Label_RenameName.Anchors := [anLeft,anBottom];

        Edit_Rename := TKMEdit.Create(PopUp_Rename, 105, 97, 275, 20, fnt_Metal);
        Edit_Rename.Anchors := [anLeft,anBottom];
        Edit_Rename.AllowedChars := acFileName;
        Edit_Rename.OnChange := Edit_Rename_Change;

        Button_MapRenameConfirm := TKMButton.Create(PopUp_Rename, 20, 155, 170, 30, gResTexts[TX_MENU_REPLAY_RENAME_CONFIRM], bsMenu);
        Button_MapRenameConfirm.Anchors := [anLeft,anBottom];
        Button_MapRenameConfirm.OnClick := RenameClick;

        Button_MapRenameCancel := TKMButton.Create(PopUp_Rename, 210, 155, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
        Button_MapRenameCancel.Anchors := [anLeft,anBottom];
        Button_MapRenameCancel.OnClick := RenameClick;

      //Move PopUp
      PopUp_Move := TKMPopUpMenu.Create(Panel_MapEd, 400);
      PopUp_Move.Height := 200;
      // Keep the pop-up centered
      PopUp_Move.AnchorsCenter;
      PopUp_Move.Left := (Panel_MapEd.Width div 2) - (PopUp_Move.Width div 2);
      PopUp_Move.Top := (Panel_MapEd.Height div 2) - 90;

        TKMBevel.Create(PopUp_Move, -1000,  -1000, 4000, 4000);

        Image_Move := TKMImage.Create(PopUp_Move, 0, 0, PopUp_Move.Width, PopUp_Move.Height, 15, rxGuiMain);
        Image_Move.ImageStretch;

        Label_MapMoveConfirmTitle := TKMLabel.Create(PopUp_Move, PopUp_Move.Width div 2, 40, gResTexts[TX_MENU_MAP_MOVE_DOWNLOAD], fnt_Outline, taCenter);
        Label_MapMoveConfirmTitle.Anchors := [anLeft, anBottom];

        Label_MapMoveName := TKMLabel.Create(PopUp_Move, 25, 75, 60, 20, 'Name', fnt_Metal, taLeft); // Todo translate
        Label_MapMoveName.Anchors := [anLeft,anBottom];

        Edit_MapMove := TKMEdit.Create(PopUp_Move, 105, 72, 275, 20, fnt_Grey);
        Edit_MapMove.Anchors := [anLeft, anBottom];
        Edit_MapMove.OnChange := MoveEditChange;

        Label_MoveExists := TKMLabel.Create(PopUp_Move, 25, 100, gResTexts[TX_MAPED_SAVE_EXISTS], fnt_Outline, taLeft);
        Label_MoveExists.Anchors := [anLeft, anBottom];
        Label_MoveExists.Hide;
        CheckBox_MoveExists := TKMCheckBox.Create(PopUp_Move, 25, 125, 300, 20, gResTexts[TX_MAPED_SAVE_OVERWRITE], fnt_Metal);
        CheckBox_MoveExists.Anchors := [anLeft, anBottom];
        CheckBox_MoveExists.OnClick := MoveEditChange;

        Button_MapMoveConfirm := TKMButton.Create(PopUp_Move, 20, 150, 170, 30, gResTexts[TX_MENU_MAP_MOVE_CONFIRM], bsMenu);
        Button_MapMoveConfirm.Anchors := [anLeft, anBottom];
        Button_MapMoveConfirm.OnClick := MoveClick;

        Button_MapMoveCancel  := TKMButton.Create(PopUp_Move, 210, 150, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
        Button_MapMoveCancel.Anchors := [anLeft, anBottom];
        Button_MapMoveCancel.OnClick := MoveClick;

end;


destructor TKMMenuMapEditor.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  fMinimap.Free;

  inherited;
end;


procedure TKMMenuMapEditor.StartClick(Sender: TObject);
var
  MapEdSizeX, MapEdSizeY: Integer;
  ID: Integer;
  Maps: TKMapsCollection;
begin
  //Create new map (NumEdits hold actual dimensions)
  if Sender = Button_MapEd_Create then
  begin
    MapEdSizeX := NumEdit_MapSizeX.Value;
    MapEdSizeY := NumEdit_MapSizeY.Value;
    gGameApp.NewMapEditor('', MapEdSizeX, MapEdSizeY);
  end;

  //This is also called by double clicking on a map in the list
  if ((Sender = Button_MapEd_Load) or (Sender = ColumnBox_MapEd)) and
     Button_MapEd_Load.Enabled and (ColumnBox_MapEd.ItemIndex <> -1) then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    Maps := GetMaps;

    //Terminate all
    fMaps.TerminateScan;
    fMapsMP.TerminateScan;

    Maps.Lock;
    try
      gGameApp.NewMapEditor(Maps[ID].FullPath('.dat'), 0, 0, Maps[ID].CRC);
    finally
      Maps.Unlock;
    end;

    //Keep MP/SP selected in the map editor interface
    //(if mission failed to load we would have fGame = nil)
    if (gGame <> nil) and (gGame.ActiveInterface is TKMapEdInterface) then
      TKMapEdInterface(gGame.ActiveInterface).SetLoadMode(Radio_MapEd_MapType.ItemIndex <> 0);
  end;
end;


procedure TKMMenuMapEditor.UpdateRadioMapEdSizes;
var I: Integer;
begin
  Radio_MapEdSizeX.ItemIndex := -1;
  Radio_MapEdSizeY.ItemIndex := -1;

  for I := 1 to MAPSIZES_COUNT do
  begin
    if NumEdit_MapSizeX.Value = MapSize[I] then
      Radio_MapEdSizeX.ItemIndex := I - 1;
    if NumEdit_MapSizeY.Value = MapSize[I] then
      Radio_MapEdSizeY.ItemIndex := I - 1;
  end;
end;


procedure TKMMenuMapEditor.SizeChangeByEdit(Sender: TObject);
begin
  UpdateRadioMapEdSizes;
  
  gGameApp.GameSettings.MenuMapEdNewMapX := NumEdit_MapSizeX.Value;
  gGameApp.GameSettings.MenuMapEdNewMapY := NumEdit_MapSizeY.Value;
end;


procedure TKMMenuMapEditor.SizeChangeByRadio(Sender: TObject);
begin
  if Radio_MapEdSizeX.ItemIndex <> -1 then
    NumEdit_MapSizeX.Value := MapSize[Radio_MapEdSizeX.ItemIndex + 1];
  if Radio_MapEdSizeY.ItemIndex <> -1 then
    NumEdit_MapSizeY.Value := MapSize[Radio_MapEdSizeY.ItemIndex + 1];
  gGameApp.GameSettings.MenuMapEdNewMapX := NumEdit_MapSizeX.Value;
  gGameApp.GameSettings.MenuMapEdNewMapY := NumEdit_MapSizeY.Value;
end;


procedure TKMMenuMapEditor.MapTypeChange(Sender: TObject);
begin
  gGameApp.GameSettings.MenuMapEdMapType := Radio_MapEd_MapType.ItemIndex;
  ListUpdate;
  UpdateUI;
  DeleteConfirm(False);
  MoveConfirm(False);
end;


procedure TKMMenuMapEditor.UpdateUI;
begin
  Button_MapEd_Load.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);
  Button_MapDelete.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);
  Button_MapMove.Visible := (ColumnBox_MapEd.ItemIndex <> -1) and (GetMaps[ColumnBox_MapEd.ItemIndex].MapFolder = mfDL);
  Button_MapRename.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);
  Button_MapRename.Visible := not Button_MapMove.Visible;

  if (ColumnBox_MapEd.ItemIndex = -1) then
    MinimapView_MapEd.Hide
end;


//Clear the list and initiate refresh
procedure TKMMenuMapEditor.ListUpdate;
begin
  //Terminate all
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ColumnBox_MapEd.Clear;

  //Reset scan variables
  fScanCompleted := False;
  fMinimapLastListId := MINIMAP_NOT_LOADED;

  //If both Maps and MapsMP are scanning at once ListUpdateDone can be called from either one
  //meaning we can access inconsistent and trigger assertion
  case Radio_MapEd_MapType.ItemIndex of
    0:  begin
          fSelectedMapInfo.CRC := gGameApp.GameSettings.MenuMapEdSPMapCRC;
          fMaps.Refresh(ScanUpdate, ScanTerminate);
        end;
    1:  begin
          fSelectedMapInfo.CRC := gGameApp.GameSettings.MenuMapEdMPMapCRC;
          fSelectedMapInfo.Name := gGameApp.GameSettings.MenuMapEdMPMapName;
          fMapsMP.Refresh(ScanUpdate, ScanTerminate);
        end
  end;
end;


procedure TKMMenuMapEditor.ScanUpdate(Sender: TObject);
begin
  if not fScanCompleted then  // Don't refresh list, if scan was completed already
    RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMenuMapEditor.ScanTerminate(Sender: TObject);
begin
  fScanCompleted := True;
  RefreshList(True); //After scan complete jump to selected item
end;


procedure TKMMenuMapEditor.SortUpdate(Sender: TObject);
begin
  RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMenuMapEditor.ReadmeClick(Sender: TObject);
var
  ID: Integer;
  Maps: TKMapsCollection;
begin
  ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
  Maps := GetMaps;
  Maps[ID].ViewReadme;
end;


procedure TKMMenuMapEditor.RefreshList(aJumpToSelected:Boolean);
var
  I, PrevTop: Integer;
  Maps: TKMapsCollection;
  R: TKMListRow;
begin
  PrevTop := ColumnBox_MapEd.TopIndex;
  ColumnBox_MapEd.Clear;

  Maps := GetMaps;

  Maps.Lock;
  try
    for I := 0 to Maps.Count - 1 do
    begin
      R := MakeListRow(['', Maps[I].FileName, IntToStr(Maps[I].LocCount), Maps[I].SizeText],  //Texts
                       [Maps[I].GetLobbyColor, Maps[I].GetLobbyColor, Maps[I].GetLobbyColor, Maps[I].GetLobbyColor], //Colors
                       I);
      R.Cells[0].Pic := MakePic(rxGui, 657 + Byte(Maps[I].MissionMode = mm_Tactic));
      ColumnBox_MapEd.AddItem(R);

      if (Maps[I].CRC = fSelectedMapInfo.CRC)
        and ((Radio_MapEd_MapType.ItemIndex = 0) or (Maps[I].FileName = fSelectedMapInfo.Name)) then  //Check name only for MP maps
      begin
        ColumnBox_MapEd.ItemIndex := I;
        LoadMinimap(I);
      end;
    end;
  finally
    Maps.Unlock;
  end;

  ColumnBox_MapEd.TopIndex := PrevTop;

  if aJumpToSelected and (ColumnBox_MapEd.ItemIndex <> -1)
    and not InRange(ColumnBox_MapEd.ItemIndex - ColumnBox_MapEd.TopIndex, 0, ColumnBox_MapEd.GetVisibleRows-1)
  then
    if ColumnBox_MapEd.ItemIndex < ColumnBox_MapEd.TopIndex then
      ColumnBox_MapEd.TopIndex := ColumnBox_MapEd.ItemIndex
    else
    if ColumnBox_MapEd.ItemIndex > ColumnBox_MapEd.TopIndex + ColumnBox_MapEd.GetVisibleRows - 1 then
      ColumnBox_MapEd.TopIndex := ColumnBox_MapEd.ItemIndex - ColumnBox_MapEd.GetVisibleRows + 1;

  UpdateUI;
end;


procedure TKMMenuMapEditor.ColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
begin
  //Determine Sort method depending on which column user clicked
  with ColumnBox_MapEd do
  case SortIndex of
    0:  if SortDirection = sdDown then
          SM := smByModeDesc
        else
          SM := smByModeAsc;
    1:  if SortDirection = sdDown then
          SM := smByNameDesc
        else
          SM := smByNameAsc;
    2:  if SortDirection = sdDown then
          SM := smByPlayersDesc
        else
          SM := smByPlayersAsc;
    3:  if SortDirection = sdDown then
          SM := smBySizeDesc
        else
          SM := smBySizeAsc;
    else SM := smByNameAsc;
  end;

  //Keep all lists in sync incase user switches between them
  fMaps.Sort(SM, SortUpdate);
  fMapsMP.Sort(SM, SortUpdate);
end;


function TKMMenuMapEditor.GetMaps: TKMapsCollection;
begin
  case Radio_MapEd_MapType.ItemIndex of
    0: Result := fMaps;
    1: Result := fMapsMP;
    else
      raise Exception.Create('Unknown map type ' + IntToStr(Radio_MapEd_MapType.ItemIndex));
  end;
end;


procedure TKMMenuMapEditor.SelectMap(Sender: TObject);
var ID: Integer;
    Maps: TKMapsCollection;
begin
  UpdateUI;
  if ColumnBox_MapEd.ItemIndex <> -1 then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    Maps := GetMaps;

    DeleteConfirm(False);
    MoveConfirm(False);

    Maps.Lock;
    try
      SetSelectedMapInfo(ID);
      LoadMinimap(ID);
    finally
      Maps.Unlock;
    end;
  end else begin
    SetSelectedMapInfo;
    MinimapView_MapEd.Hide;
  end;
end;


procedure TKMMenuMapEditor.KeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:  if Button_MapDeleteConfirm.IsClickable then
                  DeleteClick(Button_MapDeleteConfirm)
                else if Button_MapRenameConfirm.IsClickable then
                  RenameClick(Button_MapRenameConfirm)
                else if Button_MapMoveConfirm.IsClickable then
                  MoveClick(Button_MapMoveConfirm);
  end;
end;


procedure TKMMenuMapEditor.EscKeyDown(Sender: TObject);
begin
  if Button_MapDeleteCancel.IsClickable then
    DeleteClick(Button_MapDeleteCancel)
  else if Button_MapRenameCancel.IsClickable then
    RenameClick(Button_MapRenameCancel)
  else if Button_MapMoveCancel.IsClickable then
    MoveClick(Button_MapMoveCancel)
  else
    BackClick(nil);
end;


procedure TKMMenuMapEditor.BackClick(Sender: TObject);
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuMapEditor.DeleteClick(Sender: TObject);
var
  OldSelection, NewSelection: Integer;
  Maps: TKMapsCollection;
begin
  Maps := GetMaps;

  if ColumnBox_MapEd.ItemIndex = -1 then Exit;

  if Sender = Button_MapDelete then
    DeleteConfirm(True);

  if (Sender = Button_MapDeleteConfirm) or (Sender = Button_MapDeleteCancel) then
    DeleteConfirm(False);

  //Delete selected map
  if Sender = Button_MapDeleteConfirm then
  begin
    OldSelection := ColumnBox_MapEd.ItemIndex;
    Maps.DeleteMap(ColumnBox_MapEd.ItemIndex);
    if ColumnBox_MapEd.RowCount > 1 then
    begin
      NewSelection := EnsureRange(OldSelection, 0, ColumnBox_MapEd.RowCount - 2);
      SetSelectedMapInfo(NewSelection);
    end else
      SetSelectedMapInfo;

    RefreshList(True);
  end;
end;


procedure TKMMenuMapEditor.DeleteConfirm(aVisible: Boolean);
begin
  if aVisible then
  begin
    PopUp_Delete.Show;
    ColumnBox_MapEd.Focusable := False;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd);
  end else begin
    PopUp_Delete.Hide;
    ColumnBox_MapEd.Focusable := True;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd);
  end;
end;


procedure TKMMenuMapEditor.RenameClick(Sender: TObject);
var
  Maps: TKMapsCollection;
begin
  Maps := GetMaps;

  if ColumnBox_MapEd.ItemIndex = -1 then Exit;

  if Sender = Button_MapRename then
    RenameConfirm(True);

  if (Sender = Button_MapRenameConfirm) or (Sender = Button_MapRenameCancel) then
    RenameConfirm(False);

  // Change name of the save
  if Sender = Button_MapRenameConfirm then
  begin
    Edit_Rename.Text := Trim(Edit_Rename.Text);
    Maps.RenameMap(ColumnBox_MapEd.ItemIndex, Edit_Rename.Text);
    SetSelectedMapInfo(fSelectedMapInfo.CRC, Edit_Rename.Text);
    ListUpdate;
  end;
end;


// Check if new name is allowed
procedure TKMMenuMapEditor.Edit_Rename_Change(Sender: TObject);
begin
  Button_MapRenameConfirm.Enabled := (Trim(Edit_Rename.Text) <> '') and not GetMaps.Contains(Trim(Edit_Rename.Text));
end;


procedure TKMMenuMapEditor.RenameConfirm(aVisible: Boolean);
begin
  if aVisible then
  begin
    Edit_Rename.Text := GetMaps[ColumnBox_MapEd.ItemIndex].FileName;
    Button_MapRenameConfirm.Enabled := False;
    PopUp_Rename.Show;
  end else
    PopUp_Rename.Hide;
end;


procedure TKMMenuMapEditor.MoveConfirm(aVisible: Boolean);
begin
  if aVisible then
  begin
    PopUp_Move.Show;
    ColumnBox_MapEd.Focusable := False;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd);
  end else begin
    PopUp_Move.Hide;
    ColumnBox_MapEd.Focusable := True;
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd);
  end;
end;


procedure TKMMenuMapEditor.SetSelectedMapInfo(aID: Integer = -1);
var CRC: Cardinal;
    Name: UnicodeString;
    Maps: TKMapsCollection;
begin
  if (aID <> -1) then
  begin
    Maps := GetMaps;
    CRC := Maps[aID].CRC;
    Name := Maps[aID].FileName;
  end else begin
    CRC := 0;
    Name := '';
  end;
  SetSelectedMapInfo(CRC, Name);
end;


procedure TKMMenuMapEditor.SetSelectedMapInfo(aCRC: Cardinal; const aName: UnicodeString);
begin
  fSelectedMapInfo.CRC := aCRC;
  fSelectedMapInfo.Name := aName;
  case Radio_MapEd_MapType.ItemIndex of
    0:  gGameApp.GameSettings.MenuMapEdSPMapCRC := aCRC; // Set only CRC, because we do not save selected SP map name
    1:  begin
          gGameApp.GameSettings.MenuMapEdMPMapCRC := aCRC;
          gGameApp.GameSettings.MenuMapEdMPMapName := aName;
        end;
  end;
end;


procedure TKMMenuMapEditor.MoveEditChange(Sender: TObject);
var
  SaveName: string;
begin
  // Do not allow empty file name
  if Trim(Edit_MapMove.Text) = '' then
  begin
    CheckBox_MoveExists.Visible := False;
    Label_MoveExists.Visible := False;
    Button_MapMoveConfirm.Enabled := False;
    Exit;
  end;

  SaveName := TKMapsCollection.FullPath(Trim(Edit_MapMove.Text), '.dat', mfMP);

  if (Sender = Edit_MapMove) or (Sender = Button_MapMove) then
  begin
    CheckBox_MoveExists.Visible := FileExists(SaveName);
    Label_MoveExists.Visible := CheckBox_MoveExists.Visible;
    CheckBox_MoveExists.Checked := False;
    Button_MapMoveConfirm.Enabled := not CheckBox_MoveExists.Visible;
  end;

  if Sender = CheckBox_MoveExists then
    Button_MapMoveConfirm.Enabled := CheckBox_MoveExists.Checked;
end;


procedure TKMMenuMapEditor.LoadMinimap(aID: Integer = -1);
var
  Map: TKMapInfo;
  ShowMapTypeLabel: Boolean;
begin
  if aID <> -1 then
  begin
    if fMinimapLastListId = aID then Exit; //Do not reload same minimap

    fMinimapLastListId := aID;
    Map := GetMaps[aID];
    fMinimap.LoadFromMission(Map.FullPath('.dat'), []);
    fMinimap.Update(True);
    MinimapView_MapEd.SetMinimap(fMinimap);
    MinimapView_MapEd.Show;
    Panel_LobbySetupDesc.Show;
    Map.LoadExtra;
    Memo_LobbyMapDesc.Text := Map.BigDesc;
    if Map.HasReadme then
      Button_LobbySetupReadme.Show
    else
      Button_LobbySetupReadme.Hide;

    ShowMapTypeLabel := Map.IsCoop or Map.IsSpecial;
    if ShowMapTypeLabel then
    begin
      if Map.IsCoop then
        Label_MapType.Caption := gResTexts[TX_LOBBY_MAP_COOP]
      else
        Label_MapType.Caption := gResTexts[TX_LOBBY_MAP_SPECIAL];
      Memo_LobbyMapDesc.Top := 20;
      Button_LobbySetupReadme.Top := 245;
      Label_MapType.Show;
    end
    else
    begin
      Memo_LobbyMapDesc.Top := 0;
      Button_LobbySetupReadme.Top := 225;
      Label_MapType.Hide;
    end;
  end else begin
    MinimapView_MapEd.Hide;
    Panel_LobbySetupDesc.Hide;
    Memo_LobbyMapDesc.Clear;
  end;
end;


procedure TKMMenuMapEditor.MoveClick(Sender: TObject);
var
  ID: Integer;
begin
  Assert(Radio_MapEd_MapType.ItemIndex = 1);

  if ColumnBox_MapEd.ItemIndex = -1 then Exit;

  if Sender = Button_MapMove then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    Edit_MapMove.Text := fMapsMP[ID].FileNameWithoutHash;
    MoveConfirm(True);
    MoveEditChange(Button_MapMove);
  end;

  if (Sender = Button_MapMoveConfirm) or (Sender = Button_MapMoveCancel) then
    MoveConfirm(False);

  //Move selected map
  if Sender = Button_MapMoveConfirm then
  begin
    fMapsMP.MoveMap(ColumnBox_MapEd.ItemIndex, Edit_MapMove.Text, mfMP);
    SetSelectedMapInfo(fSelectedMapInfo.CRC, Edit_MapMove.Text); // Update Name of selected item in list
    gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd); // Set focus to the maps list
    ListUpdate;
  end;
end;


procedure TKMMenuMapEditor.Show;
begin
  // we can get access to gGameApp only here, because in Create it could still be nil
  Radio_MapEd_MapType.ItemIndex := gGameApp.GameSettings.MenuMapEdMapType;
  NumEdit_MapSizeX.Value := gGameApp.GameSettings.MenuMapEdNewMapX;
  NumEdit_MapSizeY.Value := gGameApp.GameSettings.MenuMapEdNewMapY;
  UpdateRadioMapEdSizes;

  ListUpdate;
  UpdateUI;

  Panel_MapEd.Show;
  gGameApp.MainMenuInterface.MyControls.UpdateFocus(ColumnBox_MapEd); // Set focus to the maps list
end;


procedure TKMMenuMapEditor.UpdateState;
begin
  fMaps.UpdateState;
  fMapsMP.UpdateState;
end;


end.
