unit KM_GUIGameBuild;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils,
  KM_Controls, KM_Defaults,
  KM_InterfaceGame, KM_ResHouses;


type
  TKMGUIGameBuild = class
  private
    procedure Build_ButtonClick(Sender: TObject);
  protected
    Panel_Build: TKMPanel;
      Label_Build: TKMLabel;
      Image_Build_Selected: TKMImage;
      Image_BuildCost_WoodPic: TKMImage;
      Image_BuildCost_StonePic: TKMImage;
      Label_BuildCost_Wood: TKMLabel;
      Label_BuildCost_Stone: TKMLabel;
      Button_BuildRoad: TKMButtonFlat;
      Button_BuildField: TKMButtonFlat;
      Button_BuildWine: TKMButtonFlat;
      Button_BuildCancel: TKMButtonFlat;
      Button_Build: array [1..GUI_HOUSE_COUNT] of TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);
    procedure PlanRoad;
    procedure PlanField;
    procedure PlanWine;
    procedure ErasePlan;
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_RenderUI, KM_GameCursor, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts,
  KM_Hand, KM_ResKeys, KM_Utils;


{ TKMGUIGameBuild }
constructor TKMGUIGameBuild.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  Panel_Build := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 332);
    Label_Build := TKMLabel.Create(Panel_Build, 0, 10, TB_WIDTH, 0, '', fnt_Outline, taCenter);
    Image_Build_Selected := TKMImage.Create(Panel_Build, 0, 40, 32, 32, 335);
    Image_Build_Selected.ImageCenter;
    Image_BuildCost_WoodPic := TKMImage.Create(Panel_Build, 67, 40, 32, 32, 353);
    Image_BuildCost_WoodPic.ImageCenter;
    Image_BuildCost_StonePic := TKMImage.Create(Panel_Build, 122, 40, 32, 32, 352);
    Image_BuildCost_StonePic.ImageCenter;
    Label_BuildCost_Wood  := TKMLabel.Create(Panel_Build,  97, 50, 20, 20, '', fnt_Outline, taLeft);
    Label_BuildCost_Stone := TKMLabel.Create(Panel_Build, 152, 50, 20, 20, '', fnt_Outline, taLeft);

    Button_BuildRoad    := TKMButtonFlat.Create(Panel_Build,   0, 80, 33, 33, 335);
    Button_BuildField   := TKMButtonFlat.Create(Panel_Build,  37, 80, 33, 33, 337);
    Button_BuildWine    := TKMButtonFlat.Create(Panel_Build,  74, 80, 33, 33, 336);
    Button_BuildCancel  := TKMButtonFlat.Create(Panel_Build, 148, 80, 33, 33, 340);
    Button_BuildRoad.OnClick    := Build_ButtonClick;
    Button_BuildField.OnClick   := Build_ButtonClick;
    Button_BuildWine.OnClick    := Build_ButtonClick;
    Button_BuildCancel.OnClick  := Build_ButtonClick;
    Button_BuildRoad.Hint   := GetHintWHotKey(TX_BUILD_ROAD_HINT, SC_PLAN_ROAD);
    Button_BuildField.Hint  := GetHintWHotKey(TX_BUILD_FIELD_HINT, SC_PLAN_FIELD);
    Button_BuildWine.Hint   := GetHintWHotKey(TX_BUILD_WINE_HINT, SC_PLAN_WINE);
    Button_BuildCancel.Hint := GetHintWHotKey(TX_BUILD_CANCEL_HINT, SC_ERASE_PLAN);

    for I := 1 to GUI_HOUSE_COUNT do
    if GUIHouseOrder[I] <> ht_None then
    begin
      Button_Build[I] := TKMButtonFlat.Create(Panel_Build, ((I-1) mod 5)*37, 120+((I-1) div 5)*37, 33, 33,
                                              gRes.Houses[GUIHouseOrder[I]].GUIIcon);
      Button_Build[I].Tag := Byte(GUIHouseOrder[I]);
      Button_Build[I].OnClick := Build_ButtonClick;
      Button_Build[I].Hint := gRes.Houses[GUIHouseOrder[I]].HouseName;
    end;
end;


procedure TKMGUIGameBuild.PlanRoad;
begin
  Button_BuildRoad.Down := True;
  Build_ButtonClick(Button_BuildRoad);
end;


procedure TKMGUIGameBuild.PlanField;
begin
  Button_BuildField.Down := True;
  Build_ButtonClick(Button_BuildField);
end;


procedure TKMGUIGameBuild.PlanWine;
begin
  Button_BuildWine.Down := True;
  Build_ButtonClick(Button_BuildWine);
end;


procedure TKMGUIGameBuild.ErasePlan;
begin
  Button_BuildCancel.Down := True;
  Build_ButtonClick(Button_BuildCancel);
end;


procedure TKMGUIGameBuild.Build_ButtonClick(Sender: TObject);
  procedure SetCost(aCursor: TKMCursorMode; aTag, aTexId, aWood, aStone: Word; const aCaption: UnicodeString);
  begin
    gGameCursor.Mode := aCursor;
    gGameCursor.Tag1 := aTag;

    Label_BuildCost_Wood.Caption  := IfThen(aWood <> 0, IntToStr(aWood), '-');
    Label_BuildCost_Stone.Caption := IfThen(aStone <> 0, IntToStr(aStone), '-');
    Label_Build.Caption := aCaption;
    Image_Build_Selected.TexID := aTexId;
  end;
var
  I: Integer;
  house: THouseType;
  houseDat: TKMHouseSpec;
begin
  if Sender = nil then
  begin
    gGameCursor.Mode := cmNone;
    Exit;
  end;

  //Release all buttons (houses and fields)
  for I := 0 to Panel_Build.ChildCount - 1 do
    if Panel_Build.Childs[I] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[I]).Down := False;

  //Press the button
  TKMButtonFlat(Sender).Down := True;

  //Reset building mode and see if it needs to be changed
  SetCost(cmNone, 0, 0, 0, 0, '');

  if Button_BuildCancel.Down then
    SetCost(cmErase, 0, 340, 0, 0, gResTexts[TX_BUILD_DEMOLISH])
  else
  if Button_BuildRoad.Down then
    SetCost(cmRoad, 0, 335, 0, 1, gResTexts[TX_BUILD_ROAD])
  else
  if Button_BuildField.Down then
    SetCost(cmField, 0, 337, 0, 0, gResTexts[TX_BUILD_FIELD])
  else
  if Button_BuildWine.Down then
    SetCost(cmWine, 0, 336, 1, 0, gResTexts[TX_BUILD_WINE])
  else
  begin
    house := THouseType(TKMButton(Sender).Tag);
    houseDat := gRes.Houses[house];
    SetCost(cmHouses, Byte(house), houseDat.GUIIcon, houseDat.WoodCost, houseDat.StoneCost, houseDat.HouseName);
  end;
end;


procedure TKMGUIGameBuild.Show;
begin
  Panel_Build.Show;
  UpdateState;
  Build_ButtonClick(Button_BuildRoad);
end;


procedure TKMGUIGameBuild.Hide;
begin
  //Reset cursor
  Build_ButtonClick(nil);
  Panel_Build.Hide;
end;


function TKMGUIGameBuild.Visible: Boolean;
begin
  Result := Panel_Build.Visible;
end;


procedure TKMGUIGameBuild.UpdateState;
var
  I: Integer;
begin
  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if gMySpectator.Hand.Locks.HouseCanBuild(GUIHouseOrder[I]) then
  begin
    Button_Build[I].Enable;
    Button_Build[I].TexID := gRes.Houses[GUIHouseOrder[I]].GUIIcon;
    Button_Build[I].OnClick := Build_ButtonClick;
    Button_Build[I].Hint := gRes.Houses[GUIHouseOrder[I]].HouseName;
  end
  else
  begin
    Button_Build[I].OnClick := nil;
    Button_Build[I].TexID := 41;
    Button_Build[I].Hint := gResTexts[TX_HOUSE_NOT_AVAILABLE]; //Building not available
  end;
end;


end.
