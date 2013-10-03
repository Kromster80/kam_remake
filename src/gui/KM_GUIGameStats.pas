unit KM_GUIGameStats;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, StrUtils, SysUtils,
  KM_Controls, KM_Defaults,
  KM_InterfaceGame, KM_ResHouses;


type
  TKMGUIGameStats = class
  protected
    Panel_Stats: TKMPanel;
      Panel_StatBlock: array [0..12] of TKMPanel;
      Stat_HousePic: array [HOUSE_MIN..HOUSE_MAX] of TKMImage;
      Stat_UnitPic: array [CITIZEN_MIN..CITIZEN_MAX] of TKMImage;
      Stat_HouseQty, Stat_HouseWip: array [HOUSE_MIN..HOUSE_MAX] of TKMLabel;
      Stat_UnitQty, Stat_UnitWip: array [CITIZEN_MIN..CITIZEN_MAX] of TKMLabel;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    procedure Resize;
    procedure UpdateState;
    function Visible: Boolean;
  end;


implementation
uses KM_RenderUI, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts;


{ TKMGUIGameStats }
constructor TKMGUIGameStats.Create(aParent: TKMPanel);
const
  House_Width = 30;
  Unit_Width = 26;
var
  I, K: Integer;
  HT: THouseType;
  UT: TUnitType;
  OffX: Integer;
begin
  Panel_Stats := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 332);
  Panel_Stats.Anchors := [akLeft, akTop, akBottom];

  for I := 0 to High(StatPlan) do
  begin
    //Houses block
    Panel_StatBlock[I] := TKMPanel.Create(Panel_Stats, 0, 0, 30, 30);
    with TKMBevel.Create(Panel_StatBlock[I], 0, 0, 30, 30) do AnchorsStretch;

    OffX := 0;
    for K := Low(StatPlan[I].HouseType) to High(StatPlan[I].HouseType) do
    if StatPlan[I].HouseType[K] <> ht_None then
    begin
      HT := StatPlan[I].HouseType[K];
      Stat_HousePic[HT] := TKMImage.Create(Panel_StatBlock[I], OffX, 0, House_Width, 30, 41); //Filled with [?] at start
      Stat_HousePic[HT].Hint := gResource.HouseDat[HT].HouseName;
      Stat_HousePic[HT].ImageCenter;
      Stat_HouseWip[HT] := TKMLabel.Create(Panel_StatBlock[I], OffX + House_Width  ,  0,  '', fnt_Grey, taRight);
      Stat_HouseWip[HT].Hitable := False;
      Stat_HouseQty[HT] := TKMLabel.Create(Panel_StatBlock[I], OffX + House_Width-2, 16, '-', fnt_Grey, taRight);
      Stat_HouseQty[HT].Hitable := False;
      Inc(OffX, House_Width);
    end;

    for K := Low(StatPlan[I].UnitType) to High(StatPlan[I].UnitType) do
    if StatPlan[I].UnitType[K] <> ut_None then
    begin
      UT := StatPlan[I].UnitType[K];
      Stat_UnitPic[UT] := TKMImage.Create(Panel_StatBlock[I], OffX, 0, Unit_Width, 30, gResource.UnitDat[UT].GUIIcon);
      Stat_UnitPic[UT].Hint := gResource.UnitDat[UT].GUIName;
      Stat_UnitPic[UT].ImageCenter;
      Stat_UnitWip[UT] := TKMLabel.Create(Panel_StatBlock[I], OffX + Unit_Width  ,  0,  '', fnt_Grey, taRight);
      Stat_UnitWip[UT].Hitable := False;
      Stat_UnitQty[UT] := TKMLabel.Create(Panel_StatBlock[I], OffX + Unit_Width-2, 16, '-', fnt_Grey, taRight);
      Stat_UnitQty[UT].Hitable := False;
      Inc(OffX, Unit_Width);
    end;
    Panel_StatBlock[I].Width := OffX;
  end;
end;


//Resize stats page in a way to display data in more readable form
//Try to keep items in corresponding pairs and stack them when dont fit otherwise
procedure TKMGUIGameStats.Resize;
const
  PAD_X = 4;
  PAD_Y = 4;
var
  Rows: Integer;
  I, K: Integer;
  OffX, NextWidth: Integer;
  NeedToCompact: Boolean;
begin
  //How many rows could fit
  Rows := Panel_Stats.Height div (Panel_StatBlock[0].Height + PAD_Y);

  //Adjoin rows till they fit
  K := 0;
  OffX := 0;
  for I := 0 to High(StatPlan) do
  begin
    Panel_StatBlock[I].Left := OffX;
    Panel_StatBlock[I].Top := K * (Panel_StatBlock[0].Height + PAD_Y);

    Inc(OffX, PAD_X + Panel_StatBlock[I].Width);

    //Return caret
    if I <> High(StatPlan) then
    begin
      NeedToCompact := (Length(StatPlan) - I) > (Rows - K);
      NextWidth := Panel_StatBlock[I].Width + PAD_X;
      if not NeedToCompact or (OffX + NextWidth > TB_WIDTH) then
      begin
        OffX := 0;
        Inc(K);
      end;
    end;
  end;
end;


procedure TKMGUIGameStats.UpdateState;
var
  HT: THouseType;
  UT: TUnitType;
  Tmp, Tmp2: Integer;
  I,K: Integer;
begin
  //Update display values
  for I := 0 to High(StatPlan) do
  for K := Low(StatPlan[I].HouseType) to High(StatPlan[I].HouseType) do
  if StatPlan[I].HouseType[K] <> ht_None then
  begin
    HT := StatPlan[I].HouseType[K];
    Tmp := gHands[MySpectator.HandIndex].Stats.GetHouseQty(HT);
    Tmp2 := gHands[MySpectator.HandIndex].Stats.GetHouseWip(HT);
    Stat_HouseQty[HT].Caption := IfThen(Tmp  = 0, '-', IntToStr(Tmp));
    Stat_HouseWip[HT].Caption := IfThen(Tmp2 = 0, '', '+' + IntToStr(Tmp2));
    if gHands[MySpectator.HandIndex].Stats.GetCanBuild(HT) or (Tmp > 0) then
    begin
      Stat_HousePic[HT].TexID := gResource.HouseDat[HT].GUIIcon;
      Stat_HousePic[HT].Hint := gResource.HouseDat[HT].HouseName;
    end
    else
    begin
      Stat_HousePic[HT].TexID := 41;
      Stat_HousePic[HT].Hint := gResTexts[TX_HOUSE_NOT_AVAIABLE]; //Building not available
    end;
  end;

  for UT := CITIZEN_MIN to CITIZEN_MAX do
  begin
    Tmp := gHands[MySpectator.HandIndex].Stats.GetUnitQty(UT);
    Tmp2 := 0;//fPlayers[MySpectator.PlayerIndex].Stats.GetUnitWip(UT);
    Stat_UnitQty[UT].Caption := IfThen(Tmp  = 0, '-', IntToStr(Tmp));
    Stat_UnitWip[UT].Caption := IfThen(Tmp2 = 0, '', '+' + IntToStr(Tmp2));
    Stat_UnitPic[UT].Hint := gResource.UnitDat[UT].GUIName;
    Stat_UnitPic[UT].FlagColor := gHands[MySpectator.HandIndex].FlagColor;
  end;
end;


procedure TKMGUIGameStats.Show;
begin
  UpdateState;
  Resize;
  Panel_Stats.Show;
end;


procedure TKMGUIGameStats.Hide;
begin
  Panel_Stats.Hide;
end;


function TKMGUIGameStats.Visible: Boolean;
begin
  Result := Panel_Stats.Visible;
end;


end.
