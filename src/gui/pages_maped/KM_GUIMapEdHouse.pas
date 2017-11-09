unit KM_GUIMapEdHouse;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics, KM_Houses, KM_InterfaceGame, KM_ResHouses;

type
  TKMMapEdHouse = class
  private
    fHouse: TKMHouse;

    fStorehouseItem: Byte; //Selected ware in storehouse
    fBarracksItem: ShortInt; //Selected ware in barracks, or -1 for recruit

    procedure Create_Store;
    procedure Create_Barracks;
    procedure Create_Woodcutters;

    procedure HouseChange(Sender: TObject; Shift: TShiftState);
    procedure BarracksRefresh;
    procedure WoodcuttersRefresh;
    procedure BarracksSelectWare(Sender: TObject);
    procedure BarracksSetRallyPoint(Sender: TObject);
    procedure WoodcuttersSetRallyPoint(Sender: TObject);
    procedure BarracksChange(Sender: TObject; Shift: TShiftState);
    procedure StoreRefresh;
    procedure StoreSelectWare(Sender: TObject);
    procedure StoreChange(Sender: TObject; Shift: TShiftState);
  protected
    Panel_House: TKMPanel;
      Label_House: TKMLabel;
      Image_House_Logo, Image_House_Worker: TKMImage;
      KMHealthBar_House: TKMPercentBar;
      Button_HouseHealthDec, Button_HouseHealthInc: TKMButton;
      Label_House_Input, Label_House_Output: TKMLabel;
      ResRow_Resource_Input: array [0..3] of TKMWareOrderRow;
      ResRow_Resource_Output: array [0..3] of TKMWareOrderRow;

    Panel_HouseWoodcutters: TKMPanel;
      Button_Woodcutters_CuttingPoint: TKMButtonFlat;

    Panel_HouseStore: TKMPanel;
      Button_Store: array [1..STORE_RES_COUNT] of TKMButtonFlat;
      Label_Store_WareCount: TKMLabel;
      Button_StoreDec100, Button_StoreDec: TKMButton;
      Button_StoreInc100, Button_StoreInc: TKMButton;

    Panel_HouseBarracks: TKMPanel;
      Button_Barracks: array [1..BARRACKS_RES_COUNT] of TKMButtonFlat;
      Button_Barracks_Recruit: TKMButtonFlat;
      Label_Barracks_WareCount: TKMLabel;
      Button_Barracks_RallyPoint: TKMButtonFlat;
      Button_BarracksDec100, Button_BarracksDec: TKMButton;
      Button_BarracksInc100, Button_BarracksInc: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show(aHouse: TKMHouse);
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_Resource, KM_RenderUI, KM_Hand, KM_ResUnits,
  KM_ResWares, KM_HouseBarracks, KM_ResFonts, KM_Utils, KM_GameCursor;


{ TKMMapEdHouse }
constructor TKMMapEdHouse.Create(aParent: TKMPanel);
var
  I: Integer;
begin
  inherited Create;

  fBarracksItem   := 1; //First ware selected by default
  fStorehouseItem := 1; //First ware selected by default

  Panel_House := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);
    //Thats common things
    Label_House := TKMLabel.Create(Panel_House, 0, 14, TB_WIDTH, 0, '', fnt_Outline, taCenter);
    Image_House_Logo := TKMImage.Create(Panel_House, 0, 41, 32, 32, 338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker := TKMImage.Create(Panel_House, 30, 41, 32, 32, 141);
    Image_House_Worker.ImageCenter;
    KMHealthBar_House := TKMPercentBar.Create(Panel_House, 100, 53, 60, 20);
    Button_HouseHealthDec := TKMButton.Create(Panel_House, 80, 53, 20, 20, '-', bsGame);
    Button_HouseHealthInc := TKMButton.Create(Panel_House, 160, 53, 20, 20, '+', bsGame);
    Button_HouseHealthDec.OnClickShift := HouseChange;
    Button_HouseHealthInc.OnClickShift := HouseChange;

    Label_House_Input := TKMLabel.Create(Panel_House, 0, 85, TB_WIDTH, 0, gResTexts[TX_HOUSE_NEEDS], fnt_Grey, taCenter);

    for I := 0 to 3 do
    begin
      ResRow_Resource_Input[I] := TKMWareOrderRow.Create(Panel_House, 0, 105 + I * 25, TB_WIDTH);
      ResRow_Resource_Input[I].RX := rxGui;
      ResRow_Resource_Input[I].OrderAdd.OnClickShift := HouseChange;
      ResRow_Resource_Input[I].OrderRem.OnClickShift := HouseChange;
    end;
    Label_House_Output := TKMLabel.Create(Panel_House, 0, 155, TB_WIDTH, 0, gResTexts[TX_HOUSE_DELIVERS]+':', fnt_Grey, taCenter);
    for I := 0 to 3 do
    begin
      ResRow_Resource_Output[I] := TKMWareOrderRow.Create(Panel_House, 0, 175 + I * 25, TB_WIDTH);
      ResRow_Resource_Output[I].RX := rxGui;
      ResRow_Resource_Output[I].OrderAdd.OnClickShift := HouseChange;
      ResRow_Resource_Output[I].OrderRem.OnClickShift := HouseChange;
    end;

  Create_Store;
  Create_Barracks;
  Create_Woodcutters;
end;


{Store page}
procedure TKMMapEdHouse.Create_Store;
var I: Integer;
begin
  Panel_HouseStore := TKMPanel.Create(Panel_House,0,76,TB_WIDTH,400);
    for I := 1 to STORE_RES_COUNT do
    begin
      Button_Store[I] := TKMButtonFlat.Create(Panel_HouseStore, 2 + ((I-1)mod 5)*36,8+((I-1)div 5)*42,32,36,0);
      Button_Store[I].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
      Button_Store[I].Tag := I;
      Button_Store[I].Hint := gRes.Wares[StoreResType[I]].Title;
      Button_Store[I].OnClick := StoreSelectWare;
    end;

    Button_StoreDec100     := TKMButton.Create(Panel_HouseStore,108,218,20,20,'<', bsGame);
    Button_StoreDec100.Tag := 100;
    Button_StoreDec        := TKMButton.Create(Panel_HouseStore,108,238,20,20,'-', bsGame);
    Button_StoreDec.Tag    := 1;
    Label_Store_WareCount  := TKMLabel.Create (Panel_HouseStore,128,230,40,20,'',fnt_Metal,taCenter);
    Button_StoreInc100     := TKMButton.Create(Panel_HouseStore,168,218,20,20,'>', bsGame);
    Button_StoreInc100.Tag := 100;
    Button_StoreInc        := TKMButton.Create(Panel_HouseStore,168,238,20,20,'+', bsGame);
    Button_StoreInc.Tag    := 1;
    Button_StoreDec100.OnClickShift := StoreChange;
    Button_StoreDec.OnClickShift    := StoreChange;
    Button_StoreInc100.OnClickShift := StoreChange;
    Button_StoreInc.OnClickShift    := StoreChange;
end;


procedure TKMMapEdHouse.Create_Woodcutters;
begin
  Panel_HouseWoodcutters := TKMPanel.Create(Panel_House,0,85,TB_WIDTH,40);
    Button_Woodcutters_CuttingPoint := TKMButtonFlat.Create(Panel_HouseWoodcutters, 0, 0, TB_WIDTH, 22, 0);
    Button_Woodcutters_CuttingPoint.CapOffsetY := -11;
    Button_Woodcutters_CuttingPoint.Caption := 'Cutting point'; //Todo translate
    Button_Woodcutters_CuttingPoint.Hint := 'Set woodcutters cutting point. Alternatively you can set it via Shift + Right mouse button'; //Todo translate
    Button_Woodcutters_CuttingPoint.OnClick := WoodcuttersSetRallyPoint;
end;


{Barracks page}
procedure TKMMapEdHouse.Create_Barracks;
var
  I: Integer;
begin
  Panel_HouseBarracks:=TKMPanel.Create(Panel_House,0,76,TB_WIDTH,400);

    Button_Barracks_RallyPoint := TKMButtonFlat.Create(Panel_HouseBarracks, 0, 8, TB_WIDTH, 22, 0);
    Button_Barracks_RallyPoint.CapOffsetY := -11;
    Button_Barracks_RallyPoint.Caption := 'Rally point'; //Todo translate
    Button_Barracks_RallyPoint.Hint := 'Set barracks rally point. Alternatively you can set it via Shift + Right mouse button'; //Todo translate
    Button_Barracks_RallyPoint.OnClick := BarracksSetRallyPoint;

    for I := 1 to BARRACKS_RES_COUNT do
    begin
      Button_Barracks[I]:=TKMButtonFlat.Create(Panel_HouseBarracks, ((I-1)mod 6)*31,26+8+((I-1)div 6)*42,28,38,0);
      Button_Barracks[I].Tag := I;
      Button_Barracks[I].TexID := gRes.Wares[BarracksResType[I]].GUIIcon;
      Button_Barracks[I].TexOffsetX := 1;
      Button_Barracks[I].TexOffsetY := 1;
      Button_Barracks[I].CapOffsetY := 2;
      Button_Barracks[I].Hint := gRes.Wares[BarracksResType[I]].Title;
      Button_Barracks[I].OnClick := BarracksSelectWare;
    end;
    Button_Barracks_Recruit := TKMButtonFlat.Create(Panel_HouseBarracks, (BARRACKS_RES_COUNT mod 6)*31,26+8+(BARRACKS_RES_COUNT div 6)*42,28,38,0);
    Button_Barracks_Recruit.Tag := -1;
    Button_Barracks_Recruit.TexOffsetX := 1;
    Button_Barracks_Recruit.TexOffsetY := 1;
    Button_Barracks_Recruit.CapOffsetY := 2;
    Button_Barracks_Recruit.TexID := gRes.Units[ut_Recruit].GUIIcon;
    Button_Barracks_Recruit.Hint := gRes.Units[ut_Recruit].GUIName;
    Button_Barracks_Recruit.OnClick := BarracksSelectWare;

    Button_BarracksDec100     := TKMButton.Create(Panel_HouseBarracks,108,218,20,20,'<', bsGame);
    Button_BarracksDec100.Tag := 100;
    Button_BarracksDec        := TKMButton.Create(Panel_HouseBarracks,108,238,20,20,'-', bsGame);
    Button_BarracksDec.Tag    := 1;
    Label_Barracks_WareCount  := TKMLabel.Create (Panel_HouseBarracks,128,230,40,20,'',fnt_Metal,taCenter);
    Button_BarracksInc100     := TKMButton.Create(Panel_HouseBarracks,168,218,20,20,'>', bsGame);
    Button_BarracksInc100.Tag := 100;
    Button_BarracksInc        := TKMButton.Create(Panel_HouseBarracks,168,238,20,20,'+', bsGame);
    Button_BarracksInc.Tag    := 1;
    Button_BarracksDec100.OnClickShift := BarracksChange;
    Button_BarracksDec.OnClickShift    := BarracksChange;
    Button_BarracksInc100.OnClickShift := BarracksChange;
    Button_BarracksInc.OnClickShift    := BarracksChange;
end;


procedure TKMMapEdHouse.Hide;
begin
  Panel_House.Hide;
end;


function TKMMapEdHouse.Visible: Boolean;
begin
  Result := Panel_House.Visible;
end;


procedure TKMMapEdHouse.UpdateState;
begin
  if Visible then
    case fHouse.HouseType of
      ht_Barracks:    Button_Barracks_RallyPoint.Down := (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_RALLY_POINT);
      ht_Woodcutters: Button_Woodcutters_CuttingPoint.Down := (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_CUTTING_POINT);
    end;
end;


procedure TKMMapEdHouse.Show(aHouse: TKMHouse);
var
  HouseDat: TKMHouseSpec;
  I: Integer;
  Res: TWareType;
begin
  fHouse := aHouse;
  if fHouse = nil then Exit;

  HouseDat := gRes.Houses[fHouse.HouseType];

  {Common data}
  Label_House.Caption := HouseDat.HouseName;
  Image_House_Logo.TexID := HouseDat.GUIIcon;
  Image_House_Worker.TexID := gRes.Units[HouseDat.OwnerType].GUIIcon;
  Image_House_Worker.FlagColor := gHands[fHouse.Owner].FlagColor;
  Image_House_Worker.Hint := gRes.Units[HouseDat.OwnerType].GUIName;
  Image_House_Worker.Visible := HouseDat.OwnerType <> ut_None;
  KMHealthBar_House.Caption := IntToStr(Round(fHouse.GetHealth)) + '/' + IntToStr(HouseDat.MaxHealth);
  KMHealthBar_House.Position := fHouse.GetHealth / HouseDat.MaxHealth;

  Label_House_Input.Hide;
  for I := 0 to 3 do
  begin
    Res := HouseDat.ResInput[I+1];
    if gRes.Wares[Res].IsValid then
    begin
      ResRow_Resource_Input[I].TexID := gRes.Wares[Res].GUIIcon;
      ResRow_Resource_Input[I].Caption := gRes.Wares[Res].Title;
      ResRow_Resource_Input[I].Hint := gRes.Wares[Res].Title;
      ResRow_Resource_Input[I].WareCount := fHouse.CheckResIn(Res);
      ResRow_Resource_Input[I].OrderCount := fHouse.CheckResIn(Res);
      ResRow_Resource_Input[I].Show;
      Label_House_Input.Show;
    end
    else
      ResRow_Resource_Input[I].Hide;
  end;

  Label_House_Output.Hide;
  for I := 0 to 3 do
  begin
    Res := HouseDat.ResOutput[I+1];
    if gRes.Wares[Res].IsValid then
    begin
      ResRow_Resource_Output[I].TexID := gRes.Wares[Res].GUIIcon;
      ResRow_Resource_Output[I].Caption := gRes.Wares[Res].Title;
      ResRow_Resource_Output[I].Hint := gRes.Wares[Res].Title;
      ResRow_Resource_Output[I].WareCount := fHouse.CheckResOut(Res);
      ResRow_Resource_Output[I].OrderCount := fHouse.CheckResOut(Res);
      ResRow_Resource_Output[I].Show;
      Label_House_Output.Show;
    end
    else
      ResRow_Resource_Output[I].Hide;
  end;

  case fHouse.HouseType of
    ht_Store:       begin
                      Panel_HouseStore.Show;
                      StoreRefresh;
                      //Reselect the ware so the display is updated
                      StoreSelectWare(Button_Store[fStorehouseItem]);
                    end;
    ht_Barracks:   begin
                      Panel_HouseBarracks.Show;
                      BarracksRefresh;
                      //In the barrack the recruit icon is always enabled
                      Image_House_Worker.Enable;
                      Button_Barracks_Recruit.FlagColor := gHands[fHouse.Owner].FlagColor;
                      //Reselect the ware so the display is updated
                      if fBarracksItem = -1 then
                        BarracksSelectWare(Button_Barracks_Recruit)
                      else
                        BarracksSelectWare(Button_Barracks[fBarracksItem]);
                    end;
    ht_Woodcutters: begin
                      Panel_HouseWoodcutters.Show;
                      WoodcuttersRefresh;
                    end;
    ht_TownHall:;
    else            begin
                      Panel_HouseWoodcutters.Hide;
                      Panel_House.Show;
                    end;
  end;
end;


procedure TKMMapEdHouse.StoreRefresh;
var
  I, Tmp: Integer;
begin
  for I := 1 to STORE_RES_COUNT do
  begin
    Tmp := TKMHouseStore(fHouse).CheckResIn(StoreResType[I]);
    Button_Store[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
end;


procedure TKMMapEdHouse.BarracksRefresh;
var
  I, Tmp: Integer;
begin
  for I := 1 to BARRACKS_RES_COUNT do
  begin
    Tmp := TKMHouseBarracks(fHouse).CheckResIn(BarracksResType[I]);
    Button_Barracks[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
  Tmp := TKMHouseBarracks(fHouse).MapEdRecruitCount;
  Button_Barracks_Recruit.Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  Button_Barracks_RallyPoint.Down := (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_RALLY_POINT);
end;


procedure TKMMapEdHouse.WoodcuttersRefresh;
begin
  Button_Woodcutters_CuttingPoint.Down := (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_CUTTING_POINT);
end;


procedure TKMMapEdHouse.HouseChange(Sender: TObject; Shift: TShiftState);
var
  I: Integer;
  Res: TWareType;
  NewCount: Integer;
  HouseDat: TKMHouseSpec;
begin
  if Sender = Button_HouseHealthDec then fHouse.AddDamage(GetMultiplicator(Shift), nil, True);
  if Sender = Button_HouseHealthInc then fHouse.AddRepair(GetMultiplicator(Shift));

  HouseDat := gRes.Houses[fHouse.HouseType];
  KMHealthBar_House.Caption := IntToStr(Round(fHouse.GetHealth)) + '/' + IntToStr(HouseDat.MaxHealth);
  KMHealthBar_House.Position := fHouse.GetHealth / HouseDat.MaxHealth;

  for I := 0 to 3 do
  begin
    Res := HouseDat.ResInput[I+1];
    if not (Res in [WARE_MIN..WARE_MAX]) then Continue;

    if Sender = ResRow_Resource_Input[I].OrderAdd then
    begin
      NewCount := Math.Min(GetMultiplicator(Shift), MAX_WARES_IN_HOUSE - fHouse.CheckResIn(Res));
      fHouse.ResAddToIn(Res, NewCount);
    end;

    if Sender = ResRow_Resource_Input[I].OrderRem then
    begin
      NewCount := Math.Min(GetMultiplicator(Shift), fHouse.CheckResIn(Res));
      fHouse.ResTakeFromIn(Res, NewCount);
    end;

    ResRow_Resource_Input[I].OrderCount := fHouse.CheckResIn(Res);
    ResRow_Resource_Input[I].WareCount := fHouse.CheckResIn(Res);
  end;

  for I := 0 to 3 do
  begin
    Res := HouseDat.ResOutput[I+1];
    if not (Res in [WARE_MIN..WARE_MAX]) then Continue;

    if Sender = ResRow_Resource_Output[I].OrderAdd then
    begin
      NewCount := Math.Min(GetMultiplicator(Shift), MAX_WARES_IN_HOUSE - fHouse.CheckResOut(Res));
      fHouse.ResAddToOut(Res, NewCount);
    end;

    if Sender = ResRow_Resource_Output[I].OrderRem then
    begin
      NewCount := Math.Min(GetMultiplicator(Shift), fHouse.CheckResOut(Res));
      fHouse.ResTakeFromOut(Res, NewCount);
    end;

    ResRow_Resource_Output[I].OrderCount := fHouse.CheckResOut(Res);
    ResRow_Resource_Output[I].WareCount := fHouse.CheckResOut(Res);
  end;
end;


procedure TKMMapEdHouse.BarracksSetRallyPoint(Sender: TObject);
begin
  Button_Barracks_RallyPoint.Down := not Button_Barracks_RallyPoint.Down;
  if Button_Barracks_RallyPoint.Down then
  begin
    gGameCursor.Mode := cmMarkers;
    gGameCursor.Tag1 := MARKER_RALLY_POINT;
  end else begin
    gGameCursor.Mode := cmNone;
    gGameCursor.Tag1 := 0;
  end;
end;


procedure TKMMapEdHouse.WoodcuttersSetRallyPoint(Sender: TObject);
begin
  Button_Woodcutters_CuttingPoint.Down := not Button_Woodcutters_CuttingPoint.Down;
  if Button_Woodcutters_CuttingPoint.Down then
  begin
    gGameCursor.Mode := cmMarkers;
    gGameCursor.Tag1 := MARKER_CUTTING_POINT;
  end else begin
    gGameCursor.Mode := cmNone;
    gGameCursor.Tag1 := 0;
  end;
end;


procedure TKMMapEdHouse.BarracksSelectWare(Sender: TObject);
var
  I: Integer;
begin
  if not Panel_HouseBarracks.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag

  Button_Barracks_Recruit.Down := False;
  for I := 1 to BARRACKS_RES_COUNT do
    Button_Barracks[I].Down := False;
  TKMButtonFlat(Sender).Down := True;
  fBarracksItem := TKMButtonFlat(Sender).Tag;
  BarracksChange(Sender, []);
end;


procedure TKMMapEdHouse.StoreSelectWare(Sender: TObject);
var
  I: Integer;
begin
  if not Panel_HouseStore.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag

  for I := 1 to Length(Button_Store) do
    Button_Store[I].Down := False;

  TKMButtonFlat(Sender).Down := True;
  fStorehouseItem := TKMButtonFlat(Sender).Tag;
  StoreChange(Sender, []);
end;


procedure TKMMapEdHouse.BarracksChange(Sender: TObject; Shift: TShiftState);
var
  Res: TWareType;
  Barracks: TKMHouseBarracks;
  NewCount: Word;
begin
  Barracks := TKMHouseBarracks(fHouse);
  if fBarracksItem = -1 then
  begin
    //Recruits
    if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then
      Barracks.MapEdRecruitCount := Math.Max(0, Barracks.MapEdRecruitCount - GetMultiplicator(Shift) * TKMButton(Sender).Tag);

    if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then
      Barracks.MapEdRecruitCount := Math.Min(High(Word), Barracks.MapEdRecruitCount + GetMultiplicator(Shift) * TKMButton(Sender).Tag);

    Label_Barracks_WareCount.Caption := IntToStr(Barracks.MapEdRecruitCount);
  end
  else
  begin
    //Wares
    Res := BarracksResType[fBarracksItem];

    if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then
    begin
      NewCount := Math.Min(Barracks.CheckResIn(Res), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
      Barracks.ResTakeFromOut(Res, NewCount);
    end;

    if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then
    begin
      NewCount := Math.Min(High(Word) - Barracks.CheckResIn(Res), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
      Barracks.ResAddToIn(Res, NewCount);
    end;

    Label_Barracks_WareCount.Caption := IntToStr(Barracks.CheckResIn(Res));
  end;
  BarracksRefresh;
end;


procedure TKMMapEdHouse.StoreChange(Sender: TObject; Shift: TShiftState);
var
  Res: TWareType;
  Store: TKMHouseStore;
  NewCount: Word;
begin
  Store := TKMHouseStore(fHouse);
  Res := StoreResType[fStorehouseItem];

  //We need to take no more than it is there, thats part of bugtracking idea
  if (Sender = Button_StoreDec100) or (Sender = Button_StoreDec) then begin
    NewCount := Math.Min(Store.CheckResIn(Res), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
    Store.ResTakeFromOut(Res, NewCount);
  end;

  //We can always add any amount of resource, it will be capped by Store
  if (Sender = Button_StoreInc100) or (Sender = Button_StoreInc) then
    Store.ResAddToIn(Res, GetMultiplicator(Shift) * TKMButton(Sender).Tag);

  Label_Store_WareCount.Caption := inttostr(Store.CheckResIn(Res));
  StoreRefresh;
end;


end.
