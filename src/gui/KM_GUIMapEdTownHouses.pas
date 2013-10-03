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

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_ResTexts, KM_GameCursor, KM_Resource, KM_ResHouses, KM_ResFonts, KM_RenderUI;


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
  Button_BuildRoad.OnClick  := Town_BuildChange;
  Button_BuildField.OnClick := Town_BuildChange;
  Button_BuildWine.OnClick  := Town_BuildChange;
  Button_BuildCancel.OnClick:= Town_BuildChange;
  Button_BuildRoad.Hint     := gResTexts[TX_BUILD_ROAD_HINT];
  Button_BuildField.Hint    := gResTexts[TX_BUILD_FIELD_HINT];
  Button_BuildWine.Hint     := gResTexts[TX_BUILD_WINE_HINT];
  Button_BuildCancel.Hint   := gResTexts[TX_BUILD_CANCEL_HINT];

  TKMLabel.Create(Panel_Build,0,65,TB_WIDTH,0,gResTexts[TX_MAPED_HOUSES_TITLE],fnt_Outline,taCenter);
  for I:=1 to GUI_HOUSE_COUNT do
    if GUIHouseOrder[I] <> ht_None then begin
      Button_Build[I] := TKMButtonFlat.Create(Panel_Build, ((I-1) mod 5)*37,83+((I-1) div 5)*37,33,33,gResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
      Button_Build[I].OnClick := Town_BuildChange;
      Button_Build[I].Hint := gResource.HouseDat[GUIHouseOrder[I]].HouseName;
    end;
end;


procedure TKMMapEdTownHouses.Town_BuildChange(Sender: TObject);
var I: Integer;
begin
  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if Sender = Button_BuildCancel then
    GameCursor.Mode := cmErase
  else
  if Sender = Button_BuildRoad then
    GameCursor.Mode := cmRoad
  else
  if Sender = Button_BuildField then
    GameCursor.Mode := cmField
  else
  if Sender = Button_BuildWine then
    GameCursor.Mode := cmWine
  else

  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if Sender = Button_Build[I] then
  begin
    GameCursor.Mode := cmHouses;
    GameCursor.Tag1 := Byte(GUIHouseOrder[I]);
  end;

  Town_BuildRefresh;
end;


procedure TKMMapEdTownHouses.Town_BuildRefresh;
var
  I: Integer;
begin
  Button_BuildCancel.Down := (GameCursor.Mode = cmErase);
  Button_BuildRoad.Down   := (GameCursor.Mode = cmRoad);
  Button_BuildField.Down  := (GameCursor.Mode = cmField);
  Button_BuildWine.Down   := (GameCursor.Mode = cmWine);

  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
    Button_Build[I].Down := (GameCursor.Mode = cmHouses) and (GameCursor.Tag1 = Byte(GUIHouseOrder[I]));
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


end.
