unit KM_GUIMapEdTownUnits;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
   KM_Controls, KM_Defaults, KM_Pics, KM_Maps, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_Points, KM_InterfaceDefaults, KM_AIAttacks, KM_AIGoals, KM_Terrain;

type
  TKMMapEdTownUnits = class
  private
    procedure Town_UnitChange(Sender: TObject);
    procedure Town_UnitRefresh;
  protected
    Panel_Units: TKMPanel;
    Button_UnitCancel: TKMButtonFlat;
    Button_Citizen: array [0..13] of TKMButtonFlat;
    Button_Warriors: array [0..13] of TKMButtonFlat;
    Button_Animals: array [0..7] of TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateColors(aColor: Cardinal);
  end;


implementation
uses
  KM_CommonClasses, KM_PlayersCollection, KM_ResTexts, KM_Game, KM_Main, KM_GameCursor,
  KM_GameApp, KM_Resource, KM_TerrainDeposits, KM_ResCursors, KM_Utils,
  KM_AIDefensePos, KM_ResHouses, KM_RenderUI, KM_Sound, KM_ResSound,
  KM_ResWares, KM_ResFonts;


{ TKMMapEdTownUnits }
constructor TKMMapEdTownUnits.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  Panel_Units := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);

  for I := 0 to High(Button_Citizen) do
  begin
    Button_Citizen[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,8+(I div 5)*37,33,33,fResource.UnitDat[School_Order[I]].GUIIcon); //List of tiles 5x5
    Button_Citizen[I].Hint := fResource.UnitDat[School_Order[I]].GUIName;
    Button_Citizen[I].Tag := Byte(School_Order[I]); //Returns unit ID
    Button_Citizen[I].OnClick := Town_UnitChange;
  end;
  Button_UnitCancel := TKMButtonFlat.Create(Panel_Units,((High(Button_Citizen)+1) mod 5)*37,8+(length(Button_Citizen) div 5)*37,33,33,340);
  Button_UnitCancel.Hint := gResTexts[TX_BUILD_CANCEL_HINT];
  Button_UnitCancel.Tag := 255; //Erase
  Button_UnitCancel.OnClick := Town_UnitChange;

  for I := 0 to High(Button_Warriors) do
  begin
    Button_Warriors[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,124+(I div 5)*37,33,33, MapEd_Icon[I], rxGui);
    Button_Warriors[I].Hint := fResource.UnitDat[MapEd_Order[I]].GUIName;
    Button_Warriors[I].Tag := Byte(MapEd_Order[I]); //Returns unit ID
    Button_Warriors[I].OnClick := Town_UnitChange;
  end;

  for I := 0 to High(Button_Animals) do
  begin
    Button_Animals[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,240+(I div 5)*37,33,33, Animal_Icon[I], rxGui);
    Button_Animals[I].Hint := fResource.UnitDat[Animal_Order[I]].GUIName;
    Button_Animals[I].Tag := Byte(Animal_Order[I]); //Returns animal ID
    Button_Animals[I].OnClick := Town_UnitChange;
  end;
end;


procedure TKMMapEdTownUnits.Town_UnitChange(Sender: TObject);
begin
  GameCursor.Mode := cmUnits;
  GameCursor.Tag1 := Byte(TKMButtonFlat(Sender).Tag);

  Town_UnitRefresh;
end;


procedure TKMMapEdTownUnits.Town_UnitRefresh;
var
  I: Integer;
  B: TKMButtonFlat;
begin
  for I := 1 to Panel_Units.ChildCount do
  if Panel_Units.Childs[I] is TKMButtonFlat then
  begin
    B := TKMButtonFlat(Panel_Units.Childs[I]);
    B.Down := (GameCursor.Mode = cmUnits) and (GameCursor.Tag1 = B.Tag);
  end;
end;


procedure TKMMapEdTownUnits.Hide;
begin
  Panel_Units.Hide;
end;


procedure TKMMapEdTownUnits.Show;
begin
  Town_UnitRefresh;
  Panel_Units.Show;
end;


function TKMMapEdTownUnits.Visible: Boolean;
begin
  Result := Panel_Units.Visible;
end;


procedure TKMMapEdTownUnits.UpdateColors(aColor: Cardinal);
var
  I: Integer;
begin
  for I := Low(Button_Citizen) to High(Button_Citizen) do
    Button_Citizen[I].FlagColor := gPlayers[MySpectator.PlayerIndex].FlagColor;
  for I := Low(Button_Warriors) to High(Button_Warriors) do
    Button_Warriors[I].FlagColor := gPlayers[MySpectator.PlayerIndex].FlagColor;
end;


end.
