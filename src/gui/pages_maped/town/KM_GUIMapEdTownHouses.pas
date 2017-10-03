unit KM_GUIMapEdTownHouses;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, SysUtils,
   KM_Controls, KM_Defaults, KM_InterfaceGame;

type
  TKMMapEdTownHouses = class
  private
    procedure Town_BuildChange(Sender: TObject);
    procedure Town_BuildRefresh;
  protected
    Panel_Build: TKMPanel;
    Button_BuildRoad: TKMButtonFlat;
    Button_BuildField: TKMButtonFlat;
    Button_BuildWine: TKMButtonFlat;
    Button_BuildCancel: TKMButtonFlat;
    Button_Build: array [1..GUI_HOUSE_COUNT] of TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure BuildRoad;
    procedure BuildField;
    procedure BuildWine;
    procedure BuildCancel;

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
    procedure UpdateStateIdle;
  end;


implementation
uses
  KM_ResTexts, KM_GameCursor, KM_Resource, KM_ResHouses, KM_ResFonts, KM_RenderUI,
  KM_Terrain, KM_Points, KM_ResKeys;


{ TKMMapEdTownHouses }
constructor TKMMapEdTownHouses.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  Panel_Build := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);

  TKMLabel.Create(Panel_Build,0,PAGE_TITLE_Y,TB_WIDTH,0,gResTexts[TX_MAPED_ROAD_TITLE],fnt_Outline,taCenter);
  Button_BuildRoad   := TKMButtonFlat.Create(Panel_Build,  0,28,33,33,335);
  Button_BuildField  := TKMButtonFlat.Create(Panel_Build, 37,28,33,33,337);
  Button_BuildWine   := TKMButtonFlat.Create(Panel_Build, 74,28,33,33,336);
  Button_BuildCancel := TKMButtonFlat.Create(Panel_Build,148,28,33,33,340);

  Button_BuildField.CapColor := clMapEdBtnField;
  Button_BuildWine.CapColor := clMapEdBtnWine;

  Button_BuildRoad.OnClick  := Town_BuildChange;
  Button_BuildField.OnClick := Town_BuildChange;
  Button_BuildWine.OnClick  := Town_BuildChange;
  Button_BuildCancel.OnClick:= Town_BuildChange;
  Button_BuildRoad.Hint     := Format(gResTexts[TX_BUILD_ROAD_HINT], [gResKeys.GetKeyNameById(SC_PLAN_ROAD)]);
  Button_BuildField.Hint    := Format(gResTexts[TX_BUILD_FIELD_HINT], [gResKeys.GetKeyNameById(SC_PLAN_FIELD)]);
  Button_BuildWine.Hint     := Format(gResTexts[TX_BUILD_WINE_HINT], [gResKeys.GetKeyNameById(SC_PLAN_WINE)]);
  Button_BuildCancel.Hint   := Format(gResTexts[TX_BUILD_CANCEL_HINT], [gResKeys.GetKeyNameById(SC_ERASE_PLAN)]);

  TKMLabel.Create(Panel_Build,0,65,TB_WIDTH,0,gResTexts[TX_MAPED_HOUSES_TITLE],fnt_Outline,taCenter);
  for I:=1 to GUI_HOUSE_COUNT do
    if GUIHouseOrder[I] <> ht_None then begin
      Button_Build[I] := TKMButtonFlat.Create(Panel_Build, ((I-1) mod 5)*37,83+((I-1) div 5)*37,33,33,gRes.Houses[GUIHouseOrder[I]].GUIIcon);
      Button_Build[I].OnClick := Town_BuildChange;
      Button_Build[I].Hint := gRes.Houses[GUIHouseOrder[I]].HouseName;
    end;
end;


procedure TKMMapEdTownHouses.BuildRoad;
begin
  Button_BuildRoad.Down := True;
  Town_BuildChange(Button_BuildRoad);
end;


procedure TKMMapEdTownHouses.BuildField;
begin
  Button_BuildField.Down := True;
  Town_BuildChange(Button_BuildField);
end;


procedure TKMMapEdTownHouses.BuildWine;
begin
  Button_BuildWine.Down := True;
  Town_BuildChange(Button_BuildWine);
end;


procedure TKMMapEdTownHouses.BuildCancel;
begin
  Button_BuildCancel.Down := True;
  Town_BuildChange(Button_BuildCancel);
end;


procedure TKMMapEdTownHouses.Town_BuildChange(Sender: TObject);
var I: Integer;
begin
  //Reset cursor and see if it needs to be changed
  gGameCursor.Mode := cmNone;

  if Sender = Button_BuildCancel then
    gGameCursor.Mode := cmErase
  else
  if Sender = Button_BuildRoad then
    gGameCursor.Mode := cmRoad
  else
  if Sender = Button_BuildField then
    gGameCursor.Mode := cmField
  else
  if Sender = Button_BuildWine then
    gGameCursor.Mode := cmWine
  else

  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if Sender = Button_Build[I] then
  begin
    gGameCursor.Mode := cmHouses;
    gGameCursor.Tag1 := Byte(GUIHouseOrder[I]);
  end;

  Town_BuildRefresh;
end;


procedure TKMMapEdTownHouses.Town_BuildRefresh;
var
  I: Integer;
begin
  Button_BuildCancel.Down := (gGameCursor.Mode = cmErase);
  Button_BuildRoad.Down   := (gGameCursor.Mode = cmRoad);
  Button_BuildField.Down  := (gGameCursor.Mode = cmField);
  Button_BuildWine.Down   := (gGameCursor.Mode = cmWine);

  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
    Button_Build[I].Down := (gGameCursor.Mode = cmHouses) and (gGameCursor.Tag1 = Byte(GUIHouseOrder[I]));
end;


procedure TKMMapEdTownHouses.Hide;
begin
  Panel_Build.Hide;
end;


procedure TKMMapEdTownHouses.Show;
begin
  Town_BuildRefresh;
  Panel_Build.Show;
end;


function TKMMapEdTownHouses.Visible: Boolean;
begin
  Result := Panel_Build.Visible;
end;


procedure TKMMapEdTownHouses.UpdateState;
begin
  Town_BuildRefresh;
end;


procedure TKMMapEdTownHouses.UpdateStateIdle;
var P: TKMPoint;
begin
  P := gGameCursor.Cell;
  if (gGameCursor.Mode = cmField)
    and gTerrain.TileIsCornField(P) then
  begin
    Button_BuildField.Caption := IntToStr(gTerrain.GetCornStage(P) + 1);
    Button_BuildField.CapOffsetY := -10;
    Button_BuildField.TexOffsetY := 6;
  end else begin
    Button_BuildField.Caption := '';
    Button_BuildField.CapOffsetY := 0;
    Button_BuildField.TexOffsetY := 0;
  end;

  if (gGameCursor.Mode = cmWine)
    and gTerrain.TileIsWineField(P) then
  begin
    Button_BuildWine.Caption := IntToStr(gTerrain.GetWineStage(P) + 1);
    Button_BuildWine.CapOffsetY := -10;
    Button_BuildWine.TexOffsetY := 6;
  end else begin
    Button_BuildWine.Caption := '';
    Button_BuildWine.CapOffsetY := 0;
    Button_BuildWine.TexOffsetY := 0;
  end;
end;


end.
