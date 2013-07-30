unit KM_GUIMapEdPlayerBlockHouse;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_Defaults, KM_Pics, KM_InterfaceDefaults;

type
  TKMMapEdPlayerBlockHouse = class
  private
    procedure Player_BlockHouseClick(Sender: TObject);
    procedure Player_BlockHouseRefresh;
  protected
    Panel_BlockHouse: TKMPanel;
    Button_BlockHouse: array [1..GUI_HOUSE_COUNT] of TKMButtonFlat;
    Image_BlockHouse: array [1..GUI_HOUSE_COUNT] of TKMImage;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_PlayersCollection, KM_ResTexts,
  KM_Resource, KM_ResHouses, KM_RenderUI, KM_ResFonts;


{ TKMMapEdPlayerBlockHouse }
constructor TKMMapEdPlayerBlockHouse.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  Panel_BlockHouse := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_BlockHouse, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_BLOCK_HOUSES], fnt_Outline, taCenter);
  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  begin
    Button_BlockHouse[I] := TKMButtonFlat.Create(Panel_BlockHouse, ((I-1) mod 5)*37, 30 + ((I-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
    Button_BlockHouse[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
    Button_BlockHouse[I].OnClick := Player_BlockHouseClick;
    Button_BlockHouse[I].Tag := I;
    Image_BlockHouse[I] := TKMImage.Create(Panel_BlockHouse, ((I-1) mod 5)*37 + 13, 30 + ((I-1) div 5)*37 + 13, 16, 16, 0, rxGuiMain);
    Image_BlockHouse[I].Hitable := False;
    Image_BlockHouse[I].ImageCenter;
  end;
end;


procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseClick(Sender: TObject);
var
  I: Integer;
  H: THouseType;
begin
  I := TKMButtonFlat(Sender).Tag;
  H := GUIHouseOrder[I];

  //Loop through states CanBuild > CantBuild > Released
  if not gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
  begin
    gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := True;
    gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := False;
  end else
  if gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
  begin
    gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := False;
    gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := True;
  end else
  begin
    gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := False;
    gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := False;
  end;

  Player_BlockHouseRefresh;
end;


procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseRefresh;
var
  I: Integer;
  H: THouseType;
begin
  for I := 1 to GUI_HOUSE_COUNT do
  begin
    H := GUIHouseOrder[I];
    if gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
      Image_BlockHouse[I].TexID := 32
    else
    if gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] and not gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 33
    else
    if not gPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] and not gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 0
    else
      Image_BlockHouse[I].TexID := 24; //Some erroneous value
  end;
end;


procedure TKMMapEdPlayerBlockHouse.Hide;
begin
  Panel_BlockHouse.Hide;
end;


procedure TKMMapEdPlayerBlockHouse.Show;
begin
  Player_BlockHouseRefresh;
  Panel_BlockHouse.Show;
end;


function TKMMapEdPlayerBlockHouse.Visible: Boolean;
begin
  Result := Panel_BlockHouse.Visible;
end;


end.
