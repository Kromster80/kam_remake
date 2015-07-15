unit KM_GUIMenuMapEditor;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Maps, KM_Minimap,
  KM_InterfaceDefaults;


type
  TKMMenuMapEditor = class {(TKMGUIPage)}
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fMinimap: TKMMinimap;

    fLastMapCRC: Cardinal; //CRC of selected map

    procedure StartClick(Sender: TObject);
    procedure MapTypeChange(Sender: TObject);
    procedure SizeChangeByRadio(Sender: TObject);
    procedure SizeChangeByEdit(Sender: TObject);
    procedure ListUpdate;
    procedure ScanUpdate(Sender: TObject);
    procedure SortUpdate(Sender: TObject);
    procedure RefreshList(aJumpToSelected:Boolean);
    procedure ColumnClick(aValue: Integer);
    procedure SelectMap(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure DeleteConfirm(aVisible: Boolean);
    procedure MoveConfirm(aVisible: Boolean);
    procedure MoveEditChange(Sender: TObject);
    procedure MoveClick(Sender: TObject);
  protected
    Panel_MapEd: TKMPanel;
      Panel_MapEdSizeXY: TKMPanel;
      Radio_MapEdSizeX, Radio_MapEdSizeY: TKMRadioGroup;
      Panel_MapEdLoad: TKMPanel;
      ColumnBox_MapEd: TKMColumnBox;
      Radio_MapEd_MapType: TKMRadioGroup;
      MinimapView_MapEd: TKMMinimapView;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load: TKMButton;
      NumEdit_MapSizeX: TKMNumericEdit;
      NumEdit_MapSizeY: TKMNumericEdit;

      Button_MapDelete, Button_MapDeleteConfirm, Button_MapDeleteCancel: TKMButton;
      Button_MapMove, Button_MapMoveConfirm, Button_MapMoveCancel: TKMButton;
      Label_MapDeleteConfirm, Label_MapMoveConfirm: TKMLabel;
      Edit_MapMove: TKMEdit;
      Label_MoveExists: TKMLabel;
      CheckBox_MoveExists: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;
    procedure Show;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_GameApp, KM_RenderUI, KM_ResFonts, KM_InterfaceMapEditor, KM_Defaults;


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
      NumEdit_MapSizeX := TKMNumericEdit.Create(Panel_MapEdSizeXY, 8, 392, 32, 256);
      NumEdit_MapSizeY := TKMNumericEdit.Create(Panel_MapEdSizeXY, 118, 392, 32, 256);
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
      ColumnBox_MapEd.SetColumns(fnt_Outline, [gResTexts[TX_MENU_MAP_TITLE], '#', gResTexts[TX_MENU_MAP_SIZE]], [0, 310, 340]);
      ColumnBox_MapEd.SearchColumn := 0;
      ColumnBox_MapEd.OnColumnClick := ColumnClick;
      ColumnBox_MapEd.OnChange := SelectMap;
      ColumnBox_MapEd.OnDoubleClick := StartClick;

      with TKMBevel.Create(Panel_MapEdLoad, 448, 264, 199, 199) do
        Anchors := [anLeft];
      MinimapView_MapEd := TKMMinimapView.Create(Panel_MapEdLoad, 452, 268, 191, 191);
      MinimapView_MapEd.Anchors := [anLeft];

      Button_MapEd_Load := TKMButton.Create(Panel_MapEdLoad, 0, 632, 440, 30, gResTexts[TX_MENU_MAP_LOAD_EXISTING], bsMenu);
      Button_MapEd_Load.Anchors := [anLeft, anBottom];
      Button_MapEd_Load.OnClick := StartClick;
      Button_MapDelete := TKMButton.Create(Panel_MapEdLoad, 0, 668, 440, 30, gResTexts[TX_MENU_MAP_DELETE], bsMenu);
      Button_MapDelete.Anchors := [anLeft, anBottom];
      Button_MapDelete.OnClick := DeleteClick;
      Button_MapMove := TKMButton.Create(Panel_MapEdLoad, 0, 596, 440, 30, gResTexts[TX_MENU_MAP_MOVE_DOWNLOAD], bsMenu);
      Button_MapMove.Anchors := [anLeft, anBottom];
      Button_MapMove.OnClick := MoveClick;
      Button_MapMove.Hide;

      //Delete
      Label_MapDeleteConfirm := TKMLabel.Create(Panel_MapEdLoad, 220, 640, gResTexts[TX_MENU_MAP_DELETE_CONFIRM], fnt_Outline, taCenter);
      Label_MapDeleteConfirm.Anchors := [anLeft, anBottom];
      Label_MapDeleteConfirm.Hide;

      Button_MapDeleteConfirm := TKMButton.Create(Panel_MapEdLoad, 0, 668, 206, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
      Button_MapDeleteConfirm.Anchors := [anLeft, anBottom];
      Button_MapDeleteConfirm.OnClick := DeleteClick;
      Button_MapDeleteConfirm.Hide;

      Button_MapDeleteCancel  := TKMButton.Create(Panel_MapEdLoad, 234, 668, 206, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
      Button_MapDeleteCancel.Anchors := [anLeft, anBottom];
      Button_MapDeleteCancel.OnClick := DeleteClick;
      Button_MapDeleteCancel.Hide;

      //Move
      Label_MapMoveConfirm := TKMLabel.Create(Panel_MapEdLoad, 0, 594, gResTexts[TX_MENU_MAP_MOVE_DOWNLOAD], fnt_Outline, taLeft);
      Label_MapMoveConfirm.Anchors := [anLeft, anBottom];
      Label_MapMoveConfirm.Hide;

      Edit_MapMove := TKMEdit.Create(Panel_MapEdLoad, 0, 610, 300, 20, fnt_Grey);
      Edit_MapMove.Anchors := [anLeft, anBottom];
      Edit_MapMove.OnChange := MoveEditChange;
      Edit_MapMove.Hide;

      Label_MoveExists := TKMLabel.Create(Panel_MapEdLoad, 0, 632, gResTexts[TX_MAPED_SAVE_EXISTS], fnt_Outline, taLeft);
      Label_MoveExists.Anchors := [anLeft, anBottom];
      Label_MoveExists.Hide;
      CheckBox_MoveExists := TKMCheckBox.Create(Panel_MapEdLoad, 0, 650, 300, 20, gResTexts[TX_MAPED_SAVE_OVERWRITE], fnt_Metal);
      CheckBox_MoveExists.Anchors := [anLeft, anBottom];
      CheckBox_MoveExists.OnClick := MoveEditChange;
      CheckBox_MoveExists.Hide;

      Button_MapMoveConfirm := TKMButton.Create(Panel_MapEdLoad, 0, 668, 206, 30, gResTexts[TX_MENU_MAP_MOVE_CONFIRM], bsMenu);
      Button_MapMoveConfirm.Anchors := [anLeft, anBottom];
      Button_MapMoveConfirm.OnClick := MoveClick;
      Button_MapMoveConfirm.Hide;

      Button_MapMoveCancel  := TKMButton.Create(Panel_MapEdLoad, 234, 668, 206, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
      Button_MapMoveCancel.Anchors := [anLeft, anBottom];
      Button_MapMoveCancel.OnClick := MoveClick;
      Button_MapMoveCancel.Hide;

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 60, 708, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_MapEdBack.Anchors := [anLeft, anBottom];
    Button_MapEdBack.OnClick := BackClick;
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
    case Radio_MapEd_MapType.ItemIndex of
      0: Maps := fMaps;
      1: Maps := fMapsMP;
      else  begin
              Assert(False);
              Exit;
            end;
    end;

    //Terminate all
    fMaps.TerminateScan;
    fMapsMP.TerminateScan;

    Maps.Lock;
      gGameApp.NewMapEditor(Maps[ID].FullPath('.dat'), 0, 0);
    Maps.Unlock;

    //Keep MP/SP selected in the map editor interface
    //(if mission failed to load we would have fGame = nil)
    if (gGame <> nil) and (gGame.ActiveInterface is TKMapEdInterface) then
      TKMapEdInterface(gGame.ActiveInterface).SetLoadMode(Radio_MapEd_MapType.ItemIndex <> 0);
  end;
end;


procedure TKMMenuMapEditor.SizeChangeByEdit(Sender: TObject);
var
  I: Integer;
begin
  Radio_MapEdSizeX.ItemIndex := -1;
  Radio_MapEdSizeY.ItemIndex := -1;

  for I := 1 to MAPSIZES_COUNT do
  if NumEdit_MapSizeX.Value = MapSize[I] then
    Radio_MapEdSizeX.ItemIndex := I - 1;

  for I := 1 to MAPSIZES_COUNT do
  if NumEdit_MapSizeY.Value = MapSize[I] then
    Radio_MapEdSizeY.ItemIndex := I - 1;
end;


procedure TKMMenuMapEditor.SizeChangeByRadio(Sender: TObject);
begin
  if Radio_MapEdSizeX.ItemIndex <> -1 then
    NumEdit_MapSizeX.Value := MapSize[Radio_MapEdSizeX.ItemIndex + 1];
  if Radio_MapEdSizeY.ItemIndex <> -1 then
    NumEdit_MapSizeY.Value := MapSize[Radio_MapEdSizeY.ItemIndex + 1];
end;


procedure TKMMenuMapEditor.MapTypeChange(Sender: TObject);
begin
  ListUpdate;
  DeleteConfirm(False);
  MoveConfirm(False);
end;


//Clear the list and initiate refresh
procedure TKMMenuMapEditor.ListUpdate;
begin
  //Terminate all
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ColumnBox_MapEd.Clear;
  fLastMapCRC := 0;
  SelectMap(nil);

  //If both Maps and MapsMP are scanning at once ListUpdateDone can be called from either one
  //meaning we can access inconsistent and trigger assertion
  case Radio_MapEd_MapType.ItemIndex of
    0: fMaps.Refresh(ScanUpdate);
    1: fMapsMP.Refresh(ScanUpdate);
    else  begin
            Assert(False);
            Exit;
          end;
  end;
end;


procedure TKMMenuMapEditor.ScanUpdate(Sender: TObject);
begin
  RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMenuMapEditor.SortUpdate(Sender: TObject);
begin
  RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMenuMapEditor.RefreshList(aJumpToSelected:Boolean);
var
  I, PrevTop: Integer;
  Maps: TKMapsCollection;
begin
  PrevTop := ColumnBox_MapEd.TopIndex;
  ColumnBox_MapEd.Clear;

  case Radio_MapEd_MapType.ItemIndex of
    0: Maps := fMaps;
    1: Maps := fMapsMP;
    else  begin
            Assert(False);
            Exit;
          end;
  end;

  Maps.Lock;
  try
    for I := 0 to Maps.Count - 1 do
    begin
      ColumnBox_MapEd.AddItem(MakeListRow( [Maps[I].FileName,
                                           IntToStr(Maps[I].LocCount),
                                           Maps[I].SizeText],
                                           //Colors
                                           [Maps[I].GetLobbyColor,
                                           Maps[I].GetLobbyColor,
                                           Maps[I].GetLobbyColor], I));

      if (Maps[I].CRC = fLastMapCRC) then
        ColumnBox_MapEd.ItemIndex := I;
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
end;


procedure TKMMenuMapEditor.ColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
begin
  //Determine Sort method depending on which column user clicked
  with ColumnBox_MapEd do
  case SortIndex of
    0:  if SortDirection = sdDown then
          SM := smByNameDesc
        else
          SM := smByNameAsc;
    1:  if SortDirection = sdDown then
          SM := smByPlayersDesc
        else
          SM := smByPlayersAsc;
    2:  if SortDirection = sdDown then
          SM := smBySizeDesc
        else
          SM := smBySizeAsc;
    else SM := smByNameAsc;
  end;

  //Keep all lists in sync incase user switches between them
  fMaps.Sort(SM, SortUpdate);
  fMapsMP.Sort(SM, SortUpdate);
end;


procedure TKMMenuMapEditor.SelectMap(Sender: TObject);
var
  ID: Integer;
  Maps: TKMapsCollection;
begin
  Button_MapEd_Load.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);
  Button_MapDelete.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);

  if Button_MapEd_Load.Enabled then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    case Radio_MapEd_MapType.ItemIndex of
      0: Maps := fMaps;
      1: Maps := fMapsMP;
      else
      begin
        Assert(False);
        Exit;
      end;
    end;

    DeleteConfirm(False);
    MoveConfirm(False);

    Maps.Lock;
      fLastMapCRC := Maps[ID].CRC;
      fMinimap.LoadFromMission(Maps[ID].FullPath('.dat'), []);
    Maps.Unlock;

    fMinimap.Update(True);
    MinimapView_MapEd.SetMinimap(fMinimap);
    MinimapView_MapEd.Show;

    Button_MapMove.Visible := Maps[ID].MapFolder = mfDL;
  end
  else
  begin
    MinimapView_MapEd.Hide;
    fLastMapCRC := 0;
  end;
end;


procedure TKMMenuMapEditor.BackClick(Sender: TObject);
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuMapEditor.DeleteConfirm(aVisible: Boolean);
var Maps: TKMapsCollection;
begin
  Label_MapDeleteConfirm.Visible := aVisible;
  Button_MapDeleteConfirm.Visible := aVisible;
  Button_MapDeleteCancel.Visible := aVisible;
  Button_MapDelete.Visible := not aVisible;
  Button_MapEd_Load.Visible := not aVisible;

  case Radio_MapEd_MapType.ItemIndex of
    0: Maps := fMaps;
    1: Maps := fMapsMP;
    else begin
           Assert(False);
           Exit;
         end;
  end;
  Button_MapMove.Visible := not aVisible and (ColumnBox_MapEd.ItemIndex <> -1)
    and (Maps[ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag].MapFolder = mfDL);
end;


procedure TKMMenuMapEditor.MoveConfirm(aVisible: Boolean);
var Maps: TKMapsCollection;
begin
  Label_MapMoveConfirm.Visible := aVisible;
  Button_MapMoveConfirm.Visible := aVisible;
  Button_MapMoveCancel.Visible := aVisible;
  Edit_MapMove.Visible := aVisible;
  Label_MoveExists.Visible := aVisible;
  CheckBox_MoveExists.Visible := aVisible;
  Button_MapDelete.Visible := not aVisible;
  Button_MapEd_Load.Visible := not aVisible;

  case Radio_MapEd_MapType.ItemIndex of
    0: Maps := fMaps;
    1: Maps := fMapsMP;
    else begin
           Assert(False);
           Exit;
         end;
  end;
  Button_MapMove.Visible := not aVisible and (ColumnBox_MapEd.ItemIndex <> -1)
    and (Maps[ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag].MapFolder = mfDL);
end;


procedure TKMMenuMapEditor.DeleteClick(Sender: TObject);
var
  OldSelection, NewSelection: Integer;
  Maps: TKMapsCollection;
begin
  case Radio_MapEd_MapType.ItemIndex of
    0: Maps := fMaps;
    1: Maps := fMapsMP;
    else  begin
            Assert(False);
            Exit;
          end;
  end;

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
    RefreshList(False);
    if ColumnBox_MapEd.RowCount > 0 then
      ColumnBox_MapEd.ItemIndex := EnsureRange(OldSelection, 0, ColumnBox_MapEd.RowCount - 1)
    else
      ColumnBox_MapEd.ItemIndex := -1;
    NewSelection := ColumnBox_MapEd.ItemIndex;
    if NewSelection >= 0 then begin
      fMinimap.LoadFromMission(Maps[NewSelection].FullPath('.dat'), []);
      fMinimap.Update(True);
      MinimapView_MapEd.SetMinimap(fMinimap);
      MinimapView_MapEd.Show;
    end
    else
      MinimapView_MapEd.Hide;
  end;
end;


procedure TKMMenuMapEditor.MoveEditChange(Sender: TObject);
var
  SaveName: string;
begin
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


procedure TKMMenuMapEditor.MoveClick(Sender: TObject);
var
  OldSelection, NewSelection, ID: Integer;
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
    OldSelection := ColumnBox_MapEd.ItemIndex;
    fMapsMP.MoveMap(ColumnBox_MapEd.ItemIndex, Edit_MapMove.Text, mfMP);
    ListUpdate;
    if ColumnBox_MapEd.RowCount > 0 then
      ColumnBox_MapEd.ItemIndex := EnsureRange(OldSelection, 0, ColumnBox_MapEd.RowCount - 1)
    else
      ColumnBox_MapEd.ItemIndex := -1;
    NewSelection := ColumnBox_MapEd.ItemIndex;
    if NewSelection >= 0 then begin
      fMinimap.LoadFromMission(fMapsMP[NewSelection].FullPath('.dat'), []);
      fMinimap.Update(True);
      MinimapView_MapEd.SetMinimap(fMinimap);
      MinimapView_MapEd.Show;
    end
    else
      MinimapView_MapEd.Hide;
  end;
end;


procedure TKMMenuMapEditor.Show;
begin
  ListUpdate;
  Panel_MapEd.Show;
end;


procedure TKMMenuMapEditor.UpdateState;
begin
  fMaps.UpdateState;
  fMapsMP.UpdateState;
end;


end.
