unit KM_GUIMapEdUnit;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics, KM_Units, KM_UnitGroups,
   KM_Points, KM_InterfaceGame;

type
  TKMMapEdUnit = class
  private
    fUnit: TKMUnit;
    fGroup: TKMUnitGroup;

    procedure Unit_ArmyChange1(Sender: TObject);
    procedure Unit_ArmyChangeShift(Sender: TObject; Shift: TShiftState);
    procedure Unit_ArmyChange2(Sender: TObject; Shift: TShiftState);
    procedure Unit_ArmyClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
    procedure UnitConditionsChange(Sender: TObject; Shift: TShiftState);
    procedure UnitConditionsClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);

  protected
    Panel_Unit: TKMPanel;
    Label_UnitName: TKMLabel;
    Label_UnitCondition: TKMLabel;
    Label_UnitDescription: TKMLabel;
    KMConditionBar_Unit: TKMPercentBar;
    Button_ConditionInc, Button_ConditionDefault, Button_ConditionDec: TKMButton;
    Image_UnitPic: TKMImage;

    Panel_Army: TKMPanel;
    Button_Army_RotCW, Button_Army_RotCCW: TKMButton;
    Button_Army_ForUp, Button_Army_ForDown: TKMButton;
    ImageStack_Army: TKMImageStack;
    Label_ArmyCount: TKMLabel;
    Button_ArmyDec, Button_ArmyFood, Button_ArmyInc: TKMButton;
    DropBox_ArmyOrder: TKMDropList;
    Edit_ArmyOrderX: TKMNumericEdit;
    Edit_ArmyOrderY: TKMNumericEdit;
    Edit_ArmyOrderDir: TKMNumericEdit;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show(aUnit: TKMUnit); overload;
    procedure Show(aGroup: TKMUnitGroup); overload;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_HandsCollection, KM_RenderUI, KM_Resource, KM_ResFonts, KM_ResTexts, KM_CommonUtils, KM_ResUnits, KM_Hand, KM_Utils;


{ TKMMapEdUnit }
constructor TKMMapEdUnit.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Unit := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);
  Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,0,'',fnt_Outline,taCenter);
  Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
  Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,0,gResTexts[TX_UNIT_CONDITION],fnt_Grey,taCenter);

  KMConditionBar_Unit     := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
  Button_ConditionDec     := TKMButton.Create(Panel_Unit,65,78,20,20,'-', bsGame);
  Button_ConditionInc     := TKMButton.Create(Panel_Unit,161,78,20,20,'+', bsGame);
  Button_ConditionDefault := TKMButton.Create(Panel_Unit,86,78,74,20,'default', bsGame);

  Button_ConditionDec.OnClickShift := UnitConditionsChange;
  Button_ConditionInc.OnClickShift := UnitConditionsChange;
  Button_ConditionDec.OnClickHold  := UnitConditionsClickHold;
  Button_ConditionInc.OnClickHold  := UnitConditionsClickHold;
  Button_ConditionDefault.OnClickShift  := UnitConditionsChange;

  Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,152,TB_WIDTH,200,'',fnt_Grey,taLeft); //Taken from LIB resource
  Label_UnitDescription.AutoWrap := True;

  Panel_Army := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 400);
  Button_Army_RotCCW  := TKMButton.Create(Panel_Army,       0,  0, 56, 40, 23, rxGui, bsGame);
  Button_Army_RotCW   := TKMButton.Create(Panel_Army,     124,  0, 56, 40, 24, rxGui, bsGame);
  Button_Army_ForUp   := TKMButton.Create(Panel_Army,       0, 46, 56, 40, 33, rxGui, bsGame);
  ImageStack_Army     := TKMImageStack.Create(Panel_Army,  62, 46, 56, 40, 43, 50);
  Label_ArmyCount     := TKMLabel.Create(Panel_Army,       62, 60, 56, 20, '-', fnt_Outline, taCenter);
  Button_Army_ForDown := TKMButton.Create(Panel_Army,     124, 46, 56, 40, 32, rxGui, bsGame);
  Button_Army_RotCW.OnClick   := Unit_ArmyChange1;
  Button_Army_RotCCW.OnClick  := Unit_ArmyChange1;
  Button_Army_ForUp.OnClickShift   := Unit_ArmyChangeShift;
  Button_Army_ForDown.OnClickShift := Unit_ArmyChangeShift;

  Button_ArmyDec  := TKMButton.Create(Panel_Army,  0,92,56,40,'-', bsGame);
  Button_ArmyFood := TKMButton.Create(Panel_Army, 62,92,56,40,29, rxGui, bsGame);
  Button_ArmyInc  := TKMButton.Create(Panel_Army,124,92,56,40,'+', bsGame);
  Button_ArmyDec.OnClickShift := Unit_ArmyChange2;
  Button_ArmyFood.OnClick     := Unit_ArmyChange1;
  Button_ArmyInc.OnClickShift := Unit_ArmyChange2;

  Button_Army_ForUp.OnClickHold   := Unit_ArmyClickHold;
  Button_Army_ForDown.OnClickHold := Unit_ArmyClickHold;
  Button_Army_RotCW.OnClickHold   := Unit_ArmyClickHold;
  Button_Army_RotCCW.OnClickHold  := Unit_ArmyClickHold;
  Button_ArmyDec.OnClickHold      := Unit_ArmyClickHold;
  Button_ArmyInc.OnClickHold      := Unit_ArmyClickHold;

  //Group order
  //todo: Orders should be placed with a cursor (but keep numeric input as well?)
  TKMLabel.Create(Panel_Army, 0, 140, TB_WIDTH, 0, gResTexts[TX_MAPED_GROUP_ORDER], fnt_Outline, taLeft);
  DropBox_ArmyOrder := TKMDropList.Create(Panel_Army, 0, 160, TB_WIDTH, 20, fnt_Metal, '', bsGame);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_NONE]);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_WALK]);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_ATTACK]);
  DropBox_ArmyOrder.OnChange := Unit_ArmyChange1;

  TKMLabel.Create(Panel_Army, 0, 185, 'X:', fnt_Grey, taLeft);
  Edit_ArmyOrderX := TKMNumericEdit.Create(Panel_Army, 20, 185, 0, 255);
  Edit_ArmyOrderX.OnChange := Unit_ArmyChange1;
  TKMLabel.Create(Panel_Army, 0, 205, 'Y:', fnt_Grey, taLeft);
  Edit_ArmyOrderY := TKMNumericEdit.Create(Panel_Army, 20, 205, 0, 255);
  Edit_ArmyOrderY.OnChange := Unit_ArmyChange1;
  TKMLabel.Create(Panel_Army, 110, 185, gResTexts[TX_MAPED_GROUP_ORDER_DIRECTION], fnt_Grey, taLeft);
  Edit_ArmyOrderDir := TKMNumericEdit.Create(Panel_Army, 110, 205, 0, 7);
  Edit_ArmyOrderDir.OnChange := Unit_ArmyChange1;
end;


procedure TKMMapEdUnit.Show(aUnit: TKMUnit);
begin
  fUnit := aUnit;
  fGroup := nil;

  Label_UnitDescription.Show;
  Panel_Unit.Show;
  Button_ConditionInc.Hide;
  Button_ConditionDec.Hide;
  Button_ConditionDefault.Hide;
  Panel_Army.Hide;

  if fUnit = nil then Exit;

  Label_UnitName.Caption := gRes.Units[fUnit.UnitType].GUIName;
  Image_UnitPic.TexID := gRes.Units[fUnit.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := gHands[fUnit.Owner].FlagColor;
  KMConditionBar_Unit.Position := fUnit.Condition / UNIT_MAX_CONDITION;

  Label_UnitDescription.Caption := gRes.Units[fUnit.UnitType].Description;
end;


procedure TKMMapEdUnit.Show(aGroup: TKMUnitGroup);
begin
  fUnit := nil;
  fGroup := aGroup;

  Label_UnitDescription.Hide;
  Panel_Unit.Show;
  Button_ConditionInc.Show;
  Button_ConditionDec.Show;
  Button_ConditionDefault.Show;
  Button_ConditionDefault.Enabled := not fGroup.FlagBearer.StartWDefaultCondition;
  Panel_Army.Show;

  if fGroup = nil then Exit;

  Label_UnitName.Caption := gRes.Units[fGroup.UnitType].GUIName;
  Image_UnitPic.TexID := gRes.Units[fGroup.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := gHands[fGroup.Owner].FlagColor;
  KMConditionBar_Unit.Position := fGroup.Condition / UNIT_MAX_CONDITION;

  //Warrior specific
  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);
  DropBox_ArmyOrder.ItemIndex := Byte(fGroup.MapEdOrder.Order);
  Edit_ArmyOrderX.Value := fGroup.MapEdOrder.Pos.Loc.X;
  Edit_ArmyOrderY.Value := fGroup.MapEdOrder.Pos.Loc.Y;
  Edit_ArmyOrderDir.Value := Max(Byte(fGroup.MapEdOrder.Pos.Dir) - 1, 0);
  Unit_ArmyChange1(nil);
end;


procedure TKMMapEdUnit.UnitConditionsChange(Sender: TObject; Shift: TShiftState);
begin
  if Sender = Button_ConditionDefault then
    fGroup.FlagBearer.StartWDefaultCondition := not fGroup.FlagBearer.StartWDefaultCondition
  else if Sender = Button_ConditionInc then
  begin
    fGroup.Condition := fGroup.Condition + GetMultiplicator(Shift);
    fGroup.FlagBearer.StartWDefaultCondition := False;
    Button_ConditionDefault.Enable;
  end else if Sender = Button_ConditionDec then
  begin
    fGroup.Condition := fGroup.Condition - GetMultiplicator(Shift);
    fGroup.FlagBearer.StartWDefaultCondition := False;
    Button_ConditionDefault.Enable;
  end;

  if fGroup.FlagBearer.StartWDefaultCondition then
  begin
    KMConditionBar_Unit.Position := 0.5;
    fGroup.Condition := UNIT_MAX_CONDITION div 2;
    Button_ConditionDefault.Disable;
  end else
    KMConditionBar_Unit.Position := fGroup.Condition / UNIT_MAX_CONDITION;
end;


procedure TKMMapEdUnit.UnitConditionsClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
begin
  if (Sender = Button_ConditionDec)
    or (Sender = Button_ConditionInc) then
    UnitConditionsChange(Sender, GetShiftState(aButton));
end;


procedure TKMMapEdUnit.Unit_ArmyChange1(Sender: TObject);
begin
  Unit_ArmyChangeShift(Sender, []);
end;


procedure TKMMapEdUnit.Unit_ArmyChangeShift(Sender: TObject; Shift: TShiftState);
begin
  if Sender = Button_Army_ForUp then
    fGroup.UnitsPerRow := max(fGroup.UnitsPerRow - GetMultiplicator(Shift), 1);
  if Sender = Button_Army_ForDown then
    fGroup.UnitsPerRow := fGroup.UnitsPerRow + GetMultiplicator(Shift);

  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);

  if Sender = Button_Army_RotCW  then
    fGroup.Direction := KMNextDirection(fGroup.Direction);
  if Sender = Button_Army_RotCCW then
    fGroup.Direction := KMPrevDirection(fGroup.Direction);
  fGroup.ResetAnimStep;

  //Toggle between full and half condition
  if Sender = Button_ArmyFood then
  begin
    if fGroup.Condition = UNIT_MAX_CONDITION then
      fGroup.Condition := UNIT_MAX_CONDITION div 2
    else
      fGroup.Condition := UNIT_MAX_CONDITION;
    KMConditionBar_Unit.Position := fGroup.Condition / UNIT_MAX_CONDITION;
  end;

  fGroup.MapEdOrder.Order := TKMInitialOrder(DropBox_ArmyOrder.ItemIndex);
  fGroup.MapEdOrder.Pos.Loc.X := Edit_ArmyOrderX.Value;
  fGroup.MapEdOrder.Pos.Loc.Y := Edit_ArmyOrderY.Value;
  fGroup.MapEdOrder.Pos.Dir := TKMDirection(Edit_ArmyOrderDir.Value + 1);

  if DropBox_ArmyOrder.ItemIndex = 0 then
  begin
    Edit_ArmyOrderX.Disable;
    Edit_ArmyOrderY.Disable;
    Edit_ArmyOrderDir.Disable;
  end
  else
    if DropBox_ArmyOrder.ItemIndex = 2 then
    begin
      Edit_ArmyOrderX.Enable;
      Edit_ArmyOrderY.Enable;
      Edit_ArmyOrderDir.Disable; //Attack position doesn't let you set direction
    end
    else
    begin
      Edit_ArmyOrderX.Enable;
      Edit_ArmyOrderY.Enable;
      Edit_ArmyOrderDir.Enable;
    end;
end;


procedure TKMMapEdUnit.Unit_ArmyClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
begin
  if (Sender = Button_ArmyDec)
    or (Sender = Button_ArmyInc) then
    Unit_ArmyChange2(Sender, GetShiftState(AButton));

  if (Sender = Button_Army_ForUp)
    or (Sender = Button_Army_ForDown) then
    Unit_ArmyChangeShift(Sender, GetShiftState(AButton));

  if (Sender = Button_Army_RotCW)
    or (Sender = Button_Army_RotCCW) then
    Unit_ArmyChange1(Sender);
end;


procedure TKMMapEdUnit.Unit_ArmyChange2(Sender: TObject; Shift: TShiftState);
var
  NewCount: Integer;
begin
  if Sender = Button_ArmyDec then //Decrease
    NewCount := fGroup.MapEdCount - GetMultiplicator(Shift)
  else //Increase
    NewCount := fGroup.MapEdCount + GetMultiplicator(Shift);

  fGroup.MapEdCount := EnsureRange(NewCount, 1, 200); //Limit max members
  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);
end;


procedure TKMMapEdUnit.Hide;
begin
  Panel_Unit.Hide;
end;


function TKMMapEdUnit.Visible: Boolean;
begin
  Result := Panel_Unit.Visible;
end;


end.
