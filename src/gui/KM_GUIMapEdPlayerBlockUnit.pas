unit KM_GUIMapEdPlayerBlockUnit;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_Pics, KM_InterfaceGame;

type
  TKMMapEdPlayerBlockUnit = class
  private
    procedure Player_BlockUnitClick(Sender: TObject);
    procedure Player_BlockWarriorsClick(Sender: TObject);
    procedure Player_BlockUnitRefresh;
    procedure Player_BlockWarriorsRefresh;
  protected
    Panel_BlockUnit: TKMPanel;
    Button_BlockUnit: array [0..13] of TKMButtonFlat;
    Button_BlockWarriors: array [0..8] of TKMButtonFlat;
    Image_BlockUnit: array [0..13] of TKMImage;
    Image_BlockWarriors: array[0..8] of TKMImage;
  public
    constructor Create(aParent: TKMPanel);
    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_ResUnits, KM_RenderUI, KM_ResFonts, KM_Resource, KM_Defaults, KM_HandStats;


{ TKMMapEdPlayerBlockUnit }
constructor TKMMapEdPlayerBlockUnit.Create(aParent: TKMPanel);
var
  I, K: Integer;
begin
  inherited Create;

  Panel_BlockUnit := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_BlockUnit, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_BLOCK_UNITS], fnt_Outline, taCenter);
  for I := 0 to High(Button_BlockUnit) do
  begin
    Button_BlockUnit[I] := TKMButtonFlat.Create(Panel_BlockUnit, (I mod 5)*37,30+(I div 5)*37,33,33,gResource.UnitDat[School_Order[I]].GUIIcon);
    Button_BlockUnit[I].OnClick := Player_BlockUnitClick;
    Button_BlockUnit[I].Tag := I;
    Button_BlockUnit[I].Hint := gResource.UnitDat[School_Order[I]].GUIName;
    Image_BlockUnit[I] := TKMImage.Create(Panel_BlockUnit, (I mod 5)*37 + 15,30+(I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockUnit[I].Hitable := False;
    Image_BlockUnit[I].ImageCenter;
  end;

  for K := 0 to High(Button_BlockWarriors) do
  begin
    Button_BlockWarriors[K] := TKMButtonFlat.Create(Panel_BlockUnit,(K mod 5)*37,146+(K div 5)*37,33,33, MapEd_Icon[K], rxGui);
    Button_BlockWarriors[K].Hint := gResource.UnitDat[Barracks_Order[K]].GUIName;
    Button_BlockWarriors[K].Tag := K;
    Button_BlockWarriors[K].OnClick := Player_BlockWarriorsClick;
    Image_BlockWarriors[K] := TKMImage.Create(Panel_BlockUnit, (K mod 5)*37 + 15,146+(K div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockWarriors[K].Hitable := False;
    Image_BlockWarriors[K].ImageCenter;
  end;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockUnitClick(Sender: TObject);
var
  I: Integer;
  U: TUnitType;
begin
  I := TKMButtonFlat(Sender).Tag;
  U := School_Order[I];

  gHands[MySpectator.HandIndex].Stats.UnitBlocked[U] := not gHands[MySpectator.HandIndex].Stats.UnitBlocked[U];

  Player_BlockUnitRefresh;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockWarriorsClick(Sender: TObject);
var
  K: Integer;
  W: TUnitType;
begin
  K := TKMButtonFlat(Sender).Tag;
  W := Barracks_Order[K];

  gHands[MySpectator.HandIndex].Stats.UnitBlocked[W] := not gHands[MySpectator.HandIndex].Stats.UnitBlocked[W];

  Player_BlockWarriorsRefresh;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockUnitRefresh;
var
  I: Integer;
  U: TUnitType;
begin
  for I := 0 to 13 do
  begin
    U := School_Order[I];
    if gHands[MySpectator.HandIndex].Stats.UnitBlocked[U] then
      Image_BlockUnit[I].TexID := 32
    else if not gHands[MySpectator.HandIndex].Stats.UnitBlocked[U] then
      Image_BlockUnit[I].TexID := 0
    else
      Image_BlockUnit[I].TexID := 24;
  end;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockWarriorsRefresh;
var
  K: Integer;
  W: TUnitType;
begin
  for K := 0 to 8 do
  begin
    W := Barracks_Order[K];
    if gHands[MySpectator.HandIndex].Stats.UnitBlocked[W] then
      Image_BlockWarriors[K].TexID := 32
    else if not gHands[MySpectator.HandIndex].Stats.UnitBlocked[W] then
      Image_BlockWarriors[K].TexID := 0
    else
      Image_BlockWarriors[K].TexID := 24;
  end;
end;


procedure TKMMapEdPlayerBlockUnit.Show;
begin
  Player_BlockUnitRefresh;
  Player_BlockWarriorsRefresh;
  Panel_BlockUnit.Show;
end;


procedure TKMMapEdPlayerBlockUnit.Hide;
begin
  Panel_BlockUnit.Hide;
end;


function TKMMapEdPlayerBlockUnit.Visible: Boolean;
begin
  Result := Panel_BlockUnit.Visible;
end;

end.
