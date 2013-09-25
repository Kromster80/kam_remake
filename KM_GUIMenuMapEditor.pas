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
    procedure ListUpdate;
    procedure ScanUpdate(Sender: TObject);
    procedure SortUpdate(Sender: TObject);
    procedure RefreshList(aJumpToSelected:Boolean);
    procedure ColumnClick(aValue: Integer);
    procedure SelectMap(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_MapEd: TKMPanel;
      Panel_MapEdSizeXY: TKMPanel;
      Radio_MapEdSizeX, Radio_MapEdSizeY: TKMRadioGroup;
      Panel_MapEdLoad: TKMPanel;
      ColumnBox_MapEd: TKMColumnBox;
      Radio_MapEd_MapType: TKMRadioGroup;
      MinimapView_MapEd: TKMMinimapView;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;
    procedure Show;
    procedure UpdateState;
  end;


implementation
uses KM_ResTexts, KM_GameApp, KM_RenderUI, KM_ResFonts;


const
  MAPSIZES_COUNT = 15;
  MapSize: array [1..MAPSIZES_COUNT] of Word = (32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256);


{ TKMGUIMainMapEditor }
constructor TKMMenuMapEditor.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var I: Integer;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);
  fMinimap := TKMMinimap.Create(True, False, True);

  Panel_MapEd:=TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_MapEd.AnchorsStretch;
    Panel_MapEdSizeXY := TKMPanel.Create(Panel_MapEd, 80, 160, 200, 400);
    Panel_MapEdSizeXY.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEdSizeXY, 6, 0, 188, 20, gResTexts[TX_MENU_NEW_MAP_SIZE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdSizeXY, 0, 20, 200, 370);
      TKMLabel.Create(Panel_MapEdSizeXY, 8, 27, 88, 20, gResTexts[TX_MENU_MAP_WIDTH], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_MapEdSizeXY, 108, 27, 88, 20, gResTexts[TX_MENU_MAP_HEIGHT], fnt_Outline, taLeft);

      Radio_MapEdSizeX := TKMRadioGroup.Create(Panel_MapEdSizeXY, 10, 52, 88, 332, fnt_Metal);
      Radio_MapEdSizeY := TKMRadioGroup.Create(Panel_MapEdSizeXY, 110, 52, 88, 332, fnt_Metal);
      for I := 1 to MAPSIZES_COUNT do begin
        Radio_MapEdSizeX.Add(IntToStr(MapSize[I]));
        Radio_MapEdSizeY.Add(IntToStr(MapSize[I]));
      end;
      Radio_MapEdSizeX.ItemIndex := 2; //64
      Radio_MapEdSizeY.ItemIndex := 2; //64

      Button_MapEd_Create := TKMButton.Create(Panel_MapEdSizeXY, 0, 400, 200, 30, gResTexts[TX_MENU_MAP_CREATE_NEW_MAP], bsMenu);
      Button_MapEd_Create.OnClick := StartClick;

    Panel_MapEdLoad := TKMPanel.Create(Panel_MapEd, 300, 160, 620, 500);
    Panel_MapEdLoad.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEdLoad, 6, 0, 288, 20, gResTexts[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdLoad, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEdLoad,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapTypeChange;
      ColumnBox_MapEd := TKMColumnBox.Create(Panel_MapEdLoad, 0, 80, 440, 310, fnt_Metal,  bsMenu);
      ColumnBox_MapEd.SetColumns(fnt_Outline, [gResTexts[TX_MENU_MAP_TITLE], '#', gResTexts[TX_MENU_MAP_SIZE]], [0, 310, 340]);
      ColumnBox_MapEd.SearchColumn := 0;
      ColumnBox_MapEd.OnColumnClick := ColumnClick;
      ColumnBox_MapEd.OnChange := SelectMap;
      ColumnBox_MapEd.OnDoubleClick := StartClick;
      Button_MapEd_Load := TKMButton.Create(Panel_MapEdLoad, 0, 400, 300, 30, gResTexts[TX_MENU_MAP_LOAD_EXISTING], bsMenu);
      Button_MapEd_Load.OnClick := StartClick;
      TKMBevel.Create(Panel_MapEdLoad, 448, 80, 199, 199);
      MinimapView_MapEd := TKMMinimapView.Create(Panel_MapEdLoad, 452, 84, 191, 191);

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 80, 620, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_MapEdBack.Anchors := [akLeft];
    Button_MapEdBack.OnClick := BackClick;
end;


destructor TKMMenuMapEditor.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;

  inherited;
end;


procedure TKMMenuMapEditor.StartClick(Sender: TObject);
var
  MapEdSizeX, MapEdSizeY: Integer;
  ID: Integer;
  Maps: TKMapsCollection;
begin
  if Sender = Button_MapEd_Create then
  begin
    MapEdSizeX := MapSize[Radio_MapEdSizeX.ItemIndex+1];
    MapEdSizeY := MapSize[Radio_MapEdSizeY.ItemIndex+1];
    fGameApp.NewMapEditor('', MapEdSizeX, MapEdSizeY);
  end;

  //This is also called by double clicking on a map in the list
  if ((Sender = Button_MapEd_Load) or (Sender = ColumnBox_MapEd)) and
     Button_MapEd_Load.Enabled and (ColumnBox_MapEd.ItemIndex <> -1) then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then
      Maps := fMaps
    else
      Maps := fMapsMP;

    //Terminate both
    fMaps.TerminateScan;
    fMapsMP.TerminateScan;

    Maps.Lock;
      fGameApp.NewMapEditor(Maps[ID].FullPath('.dat'), 0, 0);
    Maps.Unlock;
  end;
end;


procedure TKMMenuMapEditor.MapTypeChange(Sender: TObject);
begin
  ListUpdate;
end;


//Clear the list and initiate refresh
procedure TKMMenuMapEditor.ListUpdate;
begin
  //Terminate both
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ColumnBox_MapEd.Clear;
  fLastMapCRC := 0;
  SelectMap(nil);

  //If both Maps and MapsMP are scanning at once ListUpdateDone can be called from either one
  //meaning we can access inconsistent and trigger assertion
  if Radio_MapEd_MapType.ItemIndex = 0 then
    fMaps.Refresh(ScanUpdate)
  else
    fMapsMP.Refresh(ScanUpdate);
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

  if Radio_MapEd_MapType.ItemIndex = 0 then
    Maps := fMaps
  else
    Maps := fMapsMP;

  Maps.Lock;
  try
    for I := 0 to Maps.Count - 1 do
    begin
      ColumnBox_MapEd.AddItem(MakeListRow([Maps[I].FileName, IntToStr(Maps[I].LocCount), Maps[I].SizeText], I));

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

  //Keep both lists in sync incase user switches between them
  fMaps.Sort(SM, SortUpdate);
  fMapsMP.Sort(SM, SortUpdate);
end;


procedure TKMMenuMapEditor.SelectMap(Sender: TObject);
var
  ID: Integer;
  Maps: TKMapsCollection;
begin
  Button_MapEd_Load.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);

  if Button_MapEd_Load.Enabled then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then
      Maps := fMaps
    else
      Maps := fMapsMP;

    Maps.Lock;
      fLastMapCRC := Maps[ID].CRC;
      fMinimap.LoadFromMission(Maps[ID].FullPath('.dat'), []);
    Maps.Unlock;

    fMinimap.Update(True);
    MinimapView_MapEd.SetMinimap(fMinimap);
    MinimapView_MapEd.Show;
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
