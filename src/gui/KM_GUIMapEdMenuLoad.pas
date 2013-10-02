unit KM_GUIMapEdMenuLoad;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_Maps;

type
  TKMMapEdMenuLoad = class
  private
    fOnDone: TNotifyEvent;

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;

    procedure Menu_LoadClick(Sender: TObject);
    procedure Menu_LoadChange(Sender: TObject);
    procedure Menu_LoadUpdate;
    procedure Menu_LoadUpdateDone(Sender: TObject);
  protected
    Panel_Load: TKMPanel;
    Radio_Load_MapType: TKMRadioGroup;
    ListBox_Load: TKMListBox;
    Button_LoadLoad: TKMButton;
    Button_LoadCancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
    destructor Destroy; override;

    procedure Show;
    procedure Hide;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_GameApp, KM_RenderUI, KM_ResFonts, KM_InterfaceGame;


{ TKMMapEdMenuLoad }
constructor TKMMapEdMenuLoad.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);

  Panel_Load := TKMPanel.Create(aParent,0,45,TB_WIDTH,400);
  TKMLabel.Create(Panel_Load, 0, PAGE_TITLE_Y, TB_WIDTH, 30, gResTexts[TX_MAPED_LOAD_TITLE], fnt_Outline, taLeft);
  TKMBevel.Create(Panel_Load, 0, 30, TB_WIDTH, 38);
  Radio_Load_MapType := TKMRadioGroup.Create(Panel_Load,0,32,TB_WIDTH,35,fnt_Grey);
  Radio_Load_MapType.ItemIndex := 0;
  Radio_Load_MapType.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
  Radio_Load_MapType.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
  Radio_Load_MapType.OnChange := Menu_LoadChange;
  ListBox_Load := TKMListBox.Create(Panel_Load, 0, 85, TB_WIDTH, 205, fnt_Grey, bsGame);
  ListBox_Load.ItemHeight := 18;
  ListBox_Load.AutoHideScrollBar := True;
  Button_LoadLoad     := TKMButton.Create(Panel_Load,0,300,TB_WIDTH,30,gResTexts[TX_MAPED_LOAD],bsGame);
  Button_LoadCancel   := TKMButton.Create(Panel_Load,0,335,TB_WIDTH,30,gResTexts[TX_MAPED_LOAD_CANCEL],bsGame);
  Button_LoadLoad.OnClick     := Menu_LoadClick;
  Button_LoadCancel.OnClick   := Menu_LoadClick;
end;


destructor TKMMapEdMenuLoad.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;

  inherited;
end;


//Mission loading dialog
procedure TKMMapEdMenuLoad.Menu_LoadClick(Sender: TObject);
var
  MapName: string;
  IsMulti: Boolean;
begin
  if Sender = Button_LoadLoad then
  begin
    if ListBox_Load.ItemIndex = -1 then Exit;

    MapName := ListBox_Load.Item[ListBox_Load.ItemIndex];
    IsMulti := Radio_Load_MapType.ItemIndex = 1;
    fGameApp.NewMapEditor(TKMapsCollection.FullPath(MapName, '.dat', IsMulti), 0, 0);
  end
  else
  if Sender = Button_LoadCancel then
    fOnDone(Self);
end;


procedure TKMMapEdMenuLoad.Menu_LoadChange(Sender: TObject);
begin
  Menu_LoadUpdate;
end;


procedure TKMMapEdMenuLoad.Menu_LoadUpdate;
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ListBox_Load.Clear;
  ListBox_Load.ItemIndex := -1;

  if Radio_Load_MapType.ItemIndex = 0 then
    fMaps.Refresh(Menu_LoadUpdateDone)
  else
    fMapsMP.Refresh(Menu_LoadUpdateDone);
end;


procedure TKMMapEdMenuLoad.Menu_LoadUpdateDone(Sender: TObject);
var
  I: Integer;
  PrevMap: string;
  PrevTop: Integer;
  M: TKMapsCollection;
begin
  if Radio_Load_MapType.ItemIndex = 0 then
    M := fMaps
  else
    M := fMapsMP;

  //Remember previous map
  if ListBox_Load.ItemIndex <> -1 then
    PrevMap := M.Maps[ListBox_Load.ItemIndex].FileName
  else
    PrevMap := '';
  PrevTop := ListBox_Load.TopIndex;

  ListBox_Load.Clear;

  M.Lock;
  try
    for I := 0 to M.Count - 1 do
    begin
      ListBox_Load.Add(M.Maps[I].FileName);
      if M.Maps[I].FileName = PrevMap then
        ListBox_Load.ItemIndex := I;
    end;
  finally
    M.Unlock;
  end;

  ListBox_Load.TopIndex := PrevTop;
end;


procedure TKMMapEdMenuLoad.Hide;
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;
  Panel_Load.Hide;
end;


procedure TKMMapEdMenuLoad.Show;
begin
  if gGame.MapEditor.HumanCount > 1 then
    Radio_Load_MapType.ItemIndex := 1
  else
    Radio_Load_MapType.ItemIndex := 0;

  Menu_LoadUpdate;
  Panel_Load.Show;
end;


procedure TKMMapEdMenuLoad.UpdateState;
begin
  if fMaps <> nil then fMaps.UpdateState;
  if fMapsMP <> nil then fMapsMP.UpdateState;
end;


end.
