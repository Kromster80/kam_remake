unit KM_InterfaceGamePlay;
interface
uses SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls, StrUtils, Windows,
  KM_Controls, KM_Houses, KM_Units, KM_Defaults, KM_LoadDAT, KM_CommonTypes;

type TKMGamePlayInterface = class
  protected
    ToolBarX:word;
  protected
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    ShownHint:TObject;
    LastSchoolUnit:integer;  //Last unit that was selected in School, global for all schools player owns
    LastBarracksUnit:integer;//Last unit that was selected in Barracks, global for all barracks player owns
    fMessageList:TKMMessageList;
    AskDemolish:boolean;

    KMPanel_Main:TKMPanel;
      KMImage_Main1,KMImage_Main2,KMImage_Main3,KMImage_Main4:TKMImage; //Toolbar background
      KMMinimap:TKMMinimap;
      KMLabel_Stat,KMLabel_Hint,KMLabel_PointerCount:TKMLabel;
      KMButtonMain:array[1..5]of TKMButton; //4 common buttons + Return
      KMImage_Message:array[1..32]of TKMImage; //Queue of messages covers 32*48=1536px height
      KMImage_Clock:TKMImage; //Clock displayed when game speed is increased
      KMLabel_Clock:TKMLabel;
      KMLabel_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
    KMPanel_Message:TKMPanel;
      KMImage_MessageBG:TKMImage;
    KMPanel_Pause:TKMPanel;
      KMBevel_Pause:TKMBevel;
      KMImage_Pause:TKMImage;
      KMLabel_Pause1:TKMLabel;
      KMLabel_Pause2:TKMLabel;
    KMPanel_Ratios:TKMPanel;
      KMButton_Ratios:array[1..4]of TKMButton;
      KMImage_RatioPic0:TKMImage;
      KMLabel_RatioLab0:TKMLabel;
      KMImage_RatioPic:array[1..4]of TKMImage;
      KMLabel_RatioLab:array[1..4]of TKMLabel;
      KMRatio_RatioRat:array[1..4]of TKMRatioRow;
    KMPanel_Stats:TKMPanel;
      Stat_HousePic,Stat_UnitPic:array[1..32]of TKMImage;
      Stat_HouseQty,Stat_UnitQty:array[1..32]of TKMLabel;

    KMPanel_Build:TKMPanel;
      KMLabel_Build:TKMLabel;
      KMImage_Build_Selected:TKMImage;
      KMImage_BuildCost_WoodPic:TKMImage;
      KMImage_BuildCdost_StonePic:TKMImage;
      KMLabel_BuildCost_Wood:TKMLabel;
      KMLabel_BuildCost_Stone:TKMLabel;
      KMButton_BuildRoad,KMButton_BuildField,KMButton_BuildWine{,KMButton_BuildWall},KMButton_BuildCancel:TKMButtonFlat;
      KMButton_Build:array[1..HOUSE_COUNT]of TKMButtonFlat;

    KMPanel_Menu:TKMPanel;
      KMButton_Menu_Save,KMButton_Menu_Load,KMButton_Menu_Settings,KMButton_Menu_Quit,KMButton_Menu_TrackUp,KMButton_Menu_TrackDown:TKMButton;
      KMLabel_Menu_Music, KMLabel_Menu_Track: TKMLabel;

      KMPanel_Save:TKMPanel;
        KMButton_Save:array[1..SAVEGAME_COUNT]of TKMButton;

      KMPanel_Load:TKMPanel;
        KMButton_Load:array[1..SAVEGAME_COUNT]of TKMButton;

      KMPanel_Settings:TKMPanel;
        KMLabel_Settings_BrightValue:TKMLabel;
        KMButton_Settings_Dark,KMButton_Settings_Light:TKMButton;
        KMCheckBox_Settings_Autosave,KMCheckBox_Settings_FastScroll:TKMCheckBox;
        KMLabel_Settings_MouseSpeed,KMLabel_Settings_SFX,KMLabel_Settings_Music,KMLabel_Settings_Music2:TKMLabel;
        KMRatio_Settings_Mouse,KMRatio_Settings_SFX,KMRatio_Settings_Music:TKMRatioRow;
        KMButton_Settings_Music:TKMButton;

      KMPanel_Quit:TKMPanel;
        KMButton_Quit_Yes,KMButton_Quit_No:TKMButton;

    KMPanel_Unit:TKMPanel;
      KMLabel_UnitName:TKMLabel;
      KMLabel_UnitCondition:TKMLabel;
      KMLabel_UnitTask:TKMLabel;
      KMLabel_UnitAct:TKMLabel;
      KMLabel_UnitDescription:TKMLabel;
      KMConditionBar_Unit:TKMPercentBar;
      KMImage_UnitPic:TKMImage;
      KMButton_Die:TKMButton;
      
    KMPanel_House:TKMPanel;
      KMLabel_House:TKMLabel;
      KMButton_House_Goods,KMButton_House_Repair:TKMButton;
      KMImage_House_Logo,KMImage_House_Worker:TKMImage;
      KMHealthBar_House:TKMPercentBar;
      KMLabel_HouseHealth:TKMLabel;

    KMPanel_House_Common:TKMPanel;
      KMLabel_Common_Demand,KMLabel_Common_Offer,KMLabel_Common_Costs,
      KMLabel_House_UnderConstruction,KMLabel_House_Demolish:TKMLabel;
      KMButton_House_DemolishYes,KMButton_House_DemolishNo:TKMButton;
      KMRow_Common_Resource:array[1..4]of TKMResourceRow; //4 bars is the maximum
      KMRow_Order:array[1..4]of TKMResourceOrderRow; //3 bars is the maximum
      KMRow_Costs:array[1..4]of TKMCostsRow; //3 bars is the maximum
    KMPanel_HouseStore:TKMPanel;
      KMButton_Store:array[1..28]of TKMButtonFlat;
      KMImage_Store_Accept:array[1..28]of TKMImage;
    KMPanel_House_School:TKMPanel;
      KMLabel_School_Res:TKMLabel;
      KMResRow_School_Resource:TKMResourceRow;
      KMButton_School_UnitWIP:TKMButton;
      KMButton_School_UnitWIPBar:TKMPercentBar;
      KMButton_School_UnitPlan:array[1..5]of TKMButtonFlat;
      KMLabel_School_Unit:TKMLabel;
      KMImage_School_Right,KMImage_School_Train,KMImage_School_Left:TKMImage;
      KMButton_School_Right,KMButton_School_Train,KMButton_School_Left:TKMButton;
    KMPanel_HouseBarracks:TKMPanel;
      KMButton_Barracks:array[1..12]of TKMButtonFlat;
      KMLabel_Barracks_Unit:TKMLabel;
      KMImage_Barracks_Right,KMImage_Barracks_Train,KMImage_Barracks_Left:TKMImage;
      KMButton_Barracks_Right,KMButton_Barracks_Train,KMButton_Barracks_Left:TKMButton;
  private
    procedure Create_Message_Page;
    procedure Create_Pause_Page;
    procedure Create_Build_Page;
    procedure Create_Ratios_Page;
    procedure Create_Stats_Page;
    procedure Create_Menu_Page;
    procedure Create_Save_Page;
    procedure Create_Load_Page;
    procedure Create_Settings_Page;
    procedure Create_Quit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_School_Page;
    procedure Create_Barracks_Page;

    procedure SaveGame(Sender: TObject);
    procedure SwitchPage(Sender: TObject);
    procedure SwitchPageRatios(Sender: TObject);
    procedure RatiosChange(Sender: TObject);
    procedure SetHintEvents(AHintEvent:TMouseMoveEvent);
    procedure DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
    procedure Minimap_Update(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Build_Fill(Sender:TObject);
    procedure Store_Fill(Sender:TObject);
    procedure Stats_Fill(Sender:TObject);
    procedure Menu_Fill(Sender:TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create;
    destructor Destroy; override;
    procedure SetScreenSize(X,Y:word);
    procedure ShowHouseInfo(Sender:TKMHouse; aAskDemolish:boolean=false);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure Unit_Die(Sender:TObject);
    procedure House_Demolish(Sender:TObject);
    procedure House_RepairToggle(Sender:TObject);
    procedure House_WareDeliveryToggle(Sender:TObject);
    procedure House_OrderClick(Sender:TObject);
    procedure House_OrderClickRight(Sender:TObject);
    procedure House_BarracksUnitChange(Sender:TObject);
    procedure House_BarracksUnitChangeRight(Sender:TObject);
    procedure House_SchoolUnitChange(Sender:TObject);
    procedure House_SchoolUnitChangeRight(Sender:TObject);
    procedure House_SchoolUnitRemove(Sender:TObject);
    procedure House_StoreAcceptFlag(Sender:TObject);
    procedure Menu_ShowSettings(Sender: TObject);
    procedure Menu_Settings_Change(Sender:TObject);
    procedure Menu_ShowLoad(Sender: TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Menu_NextTrack(Sender:TObject);
    procedure Menu_PreviousTrack(Sender:TObject);
    procedure Build_SelectRoad;
    procedure Build_RightClickCancel;
    procedure IssueMessage(MsgTyp:TKMMessageType; Text:string);
    procedure EnableOrDisableMenuIcons(NewValue:boolean);
    procedure ShowClock(DoShow:boolean);
    procedure ShowPause(DoShow:boolean);
    procedure ShortcutPress(Key:Word; IsDown:boolean=false);
    property GetShownUnit: TKMUnit read ShownUnit;
    procedure ClearShownUnit;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Unit1, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_Terrain, KM_Utils, KM_Viewport, KM_Game, KM_SoundFX;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPageRatios(Sender: TObject);
const ResPic:array[1..4] of TResourceType = (rt_Steel,rt_Coal,rt_Wood,rt_Corn);
      ResLab:array[1..4] of word = (298,300,302,304);
      ResQty:array[1..4] of byte = (2,4,2,3);
      ResHouse:array[1..4,1..4] of THouseType = (
      (ht_WeaponSmithy,ht_ArmorSmithy,ht_None,ht_None),
      (ht_IronSmithy,ht_Metallurgists,ht_WeaponSmithy,ht_ArmorSmithy),
      (ht_ArmorWorkshop,ht_WeaponWorkshop,ht_None,ht_None),
      (ht_Mill,ht_Swine,ht_Stables,ht_None));
var i:integer; ResID:TResourceType; HouseID:THouseType;
begin

  if (MyPlayer=nil)or(MyPlayer.fMissionSettings=nil) then exit; //We need to be able to access these

  if not (Sender is TKMButton) then exit;

  //Hide everything but the tab buttons
  for i:=1 to KMPanel_Ratios.ChildCount do
    if not (KMPanel_Ratios.Childs[i] is TKMButton) then
      KMPanel_Ratios.Childs[i].Hide;

  ResID:=ResPic[TKMButton(Sender).Tag];

  KMImage_RatioPic0.TexID:=350+byte(ResID);
  KMLabel_RatioLab0.Caption:=fTextLibrary.GetTextString(ResLab[TKMButton(Sender).Tag]);
  KMImage_RatioPic0.Show;
  KMLabel_RatioLab0.Show;

  for i:=1 to ResQty[TKMButton(Sender).Tag] do begin
    HouseID:=ResHouse[TKMButton(Sender).Tag,i];
    KMImage_RatioPic[i].TexID:=GUIBuildIcons[byte(HouseID)];
    KMLabel_RatioLab[i].Caption:=fTextLibrary.GetTextString(GUIBuildIcons[byte(HouseID)]-300);
    KMRatio_RatioRat[i].Position:=MyPlayer.fMissionSettings.GetRatio(ResID,HouseID);
    KMImage_RatioPic[i].Show;
    KMLabel_RatioLab[i].Show;
    KMRatio_RatioRat[i].Show;
  end;


end;


procedure TKMGamePlayInterface.RatiosChange(Sender: TObject);
var ResID:TResourceType; HouseID:THouseType;
begin

  if (MyPlayer=nil)or(MyPlayer.fMissionSettings=nil) then exit; //We need to be able to access these
  if not (Sender is TKMRatioRow) then exit;

  ResID:=TResourceType(KMImage_RatioPic0.TexID-350);
  HouseID:=THouseType(KMImage_RatioPic[TKMRatioRow(Sender).Tag].TexID-300);

  MyPlayer.fMissionSettings.SetRatio(ResID,HouseID,TKMRatioRow(Sender).Position);
end;


procedure TKMGamePlayInterface.SaveGame(Sender: TObject);
begin
  if not (Sender is TKMButton) then exit; //Just in case
  fGame.Save(TKMControl(Sender).Tag);
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var i:integer; LastVisiblePage: TKMPanel;

  procedure Flip4MainButtons(ShowEm:boolean);
  var k:integer;
  begin
    for k:=1 to 4 do KMButtonMain[k].Visible:=ShowEm;
    KMButtonMain[5].Visible:=not ShowEm;
    KMLabel_MenuTitle.Visible:=not ShowEm;
  end;

begin

  if (Sender=KMButtonMain[1])or(Sender=KMButtonMain[2])or
     (Sender=KMButtonMain[3])or(Sender=KMButtonMain[4])or
     (Sender=KMButton_Menu_Settings)or(Sender=KMButton_Menu_Quit) then begin
    ShownHouse:=nil;
    ShownUnit:=nil;
    fPlayers.Selected:=nil;
  end;

  //Reset the CursorMode, to cm_None
  Build_ButtonClick(nil);

  //Set LastVisiblePage to which ever page was last visible, out of the ones needed
  if KMPanel_Settings.Visible then LastVisiblePage := KMPanel_Settings else
  if KMPanel_Save.Visible     then LastVisiblePage := KMPanel_Save     else
  if KMPanel_Load.Visible     then LastVisiblePage := KMPanel_Load     else
    LastVisiblePage := nil;

  //If they just closed settings then we should save them (if something has changed)
  if LastVisiblePage = KMPanel_Settings then
    if fGame.fGameSettings.GetNeedsSave then
      fGame.fGameSettings.SaveSettings;

  //First thing - hide all existing pages
    for i:=1 to KMPanel_Main.ChildCount do
      if KMPanel_Main.Childs[i] is TKMPanel then
        KMPanel_Main.Childs[i].Hide;
  //First thing - hide all existing pages
    for i:=1 to KMPanel_House.ChildCount do
      if KMPanel_House.Childs[i] is TKMPanel then
        KMPanel_House.Childs[i].Hide;

  //If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
  Flip4MainButtons(false);
  if Sender=KMButtonMain[1] then begin
    Build_Fill(nil);
    KMPanel_Build.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(166);
    Build_SelectRoad;
  end else

  if Sender=KMButtonMain[2] then begin
    KMPanel_Ratios.Show;
    SwitchPageRatios(KMButton_Ratios[1]); //Open 1st tab
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(167);
  end else

  if Sender=KMButtonMain[3] then begin
    Stats_Fill(nil);
    KMPanel_Stats.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(168);
  end else

  if (Sender=KMButtonMain[4]) or (Sender=KMButton_Quit_No) or
     ((Sender=KMButtonMain[5]) and (LastVisiblePage=KMPanel_Settings)) or
     ((Sender=KMButtonMain[5]) and (LastVisiblePage=KMPanel_Load)) or
     ((Sender=KMButtonMain[5]) and (LastVisiblePage=KMPanel_Save)) then begin
    Menu_Fill(Sender); //Make sure updating happens before it is shown
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(170);
    KMPanel_Menu.Show;
  end else

  if Sender=KMButton_Menu_Save then begin
    KMPanel_Save.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(173);
  end else

  if Sender=KMButton_Menu_Load then begin
    KMPanel_Load.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(172);
  end else

  if Sender=KMButton_Menu_Settings then begin
    KMPanel_Settings.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(179);
  end else

  if Sender=KMButton_Menu_Quit then begin
    KMPanel_Quit.Show;
  end else
    //If Sender is anything else - then show all 4 buttons and hide Return button
    Flip4MainButtons(true);

  //Now process all other kinds of pages
  if Sender=KMPanel_Unit then begin
    TKMPanel(Sender).Show;
  end else

  if Sender=KMPanel_House then begin
    TKMPanel(Sender).Show;
  end;

  if Sender=KMPanel_House_Common then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender=KMPanel_House_School then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender=KMPanel_HouseBarracks then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender=KMPanel_HouseStore then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end;

end;


procedure TKMGamePlayInterface.DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
begin
  ShownHint:=Sender;
  if((ShownHint<>nil) and ((not TKMControl(ShownHint).CursorOver) or (not TKMControl(ShownHint).Visible)) ) then ShownHint:=nil; //only set if cursor is over and control is visible
  if ((ShownHint<>nil) and (TKMControl(ShownHint).Parent <> nil)) then //only set if parent is visible (e.g. panel)
    if (ShownHint<>nil)and(not (ShownHint as TKMControl).Parent.Visible) then ShownHint:=nil;

  KMLabel_Hint.Top:=fRender.GetRenderAreaSize.Y-16;
  //If hint hasn't changed then don't refresh it
  if ((ShownHint<>nil) and (KMLabel_Hint.Caption = TKMControl(Sender).Hint)) then exit;
  if ((ShownHint=nil) and (KMLabel_Hint.Caption = '')) then exit;
  if ShownHint=nil then KMLabel_Hint.Caption:='' else
    KMLabel_Hint.Caption:=(Sender as TKMControl).Hint;
end;


{Update minimap data}
procedure TKMGamePlayInterface.Minimap_Update(Sender: TObject);
begin
  if Sender=nil then begin //UpdateState loop
    KMMinimap.MapSize:=KMPoint(fTerrain.MapX,fTerrain.MapY);
  end else
    if KMMinimap.CenteredAt.X*KMMinimap.CenteredAt.Y <> 0 then //Quick bugfix incase minimap yet not inited it will center vp on 0;0
    fViewport.SetCenter(KMMinimap.CenteredAt.X,KMMinimap.CenteredAt.Y);

  KMMinimap.CenteredAt:=fViewport.GetCenter;
  KMMinimap.ViewArea:=fViewport.GetMinimapClip;
end;


constructor TKMGamePlayInterface.Create();
var i:integer;
begin
Inherited;
fLog.AssertToLog(fViewport<>nil,'fViewport required to be init first');

  MyControls := TKMControlsCollection.Create;

  ShownUnit:=nil;
  ShownHouse:=nil;

  LastSchoolUnit:=1;
  LastBarracksUnit:=1;
  fMessageList:=TKMMessageList.Create;

{Parent Page for whole toolbar in-game}
  KMPanel_Main:=MyControls.AddPanel(nil,0,0,224,768);

    KMImage_Main1:=MyControls.AddImage(KMPanel_Main,0,0,224,200,407);
    KMImage_Main3:=MyControls.AddImage(KMPanel_Main,0,200,224,168,554);
    KMImage_Main4:=MyControls.AddImage(KMPanel_Main,0,368,224,400,404);
                   MyControls.AddImage(KMPanel_Main,0,768,224,400,404);

    KMMinimap:=MyControls.AddMinimap(KMPanel_Main,10,10,176,176);
    KMMinimap.OnChange:=Minimap_Update;

    {Main 4 buttons +return button}
    for i:=0 to 3 do begin
      KMButtonMain[i+1]:=MyControls.AddButton(KMPanel_Main,  8+46*i, 372, 42, 36, 439+i);
      KMButtonMain[i+1].OnClick:=SwitchPage;
      KMButtonMain[i+1].Hint:=fTextLibrary.GetTextString(160+i);
    end;
    KMButtonMain[4].Hint:=fTextLibrary.GetTextString(164); //This is an exception to the rule above
    KMButtonMain[5]:=MyControls.AddButton(KMPanel_Main,  8, 372, 42, 36, 443);
    KMButtonMain[5].OnClick:=SwitchPage;
    KMButtonMain[5].Hint:=fTextLibrary.GetTextString(165);
    KMLabel_MenuTitle:=MyControls.AddLabel(KMPanel_Main,54,372,138,36,'',fnt_Metal,kaLeft);

    KMImage_Clock:=MyControls.AddImage(KMPanel_Main,232,8,67,65,556);
    KMImage_Clock.Hide;
    KMLabel_Clock:=MyControls.AddLabel(KMPanel_Main,265,80,0,0,'mm:ss',fnt_Outline,kaCenter);
    KMLabel_Clock.Hide;

    for i:=low(KMImage_Message) to high(KMImage_Message) do
    begin
      KMImage_Message[i] := MyControls.AddImage(KMPanel_Main,224,fRender.GetRenderAreaSize.Y-i*48,30,48,495);
      KMImage_Message[i].Tag := i;
      KMImage_Message[i].Disable;
      KMImage_Message[i].Hide;
      //KMImage_Message[i].OnClick := DisplayMessage;
    end;

    KMLabel_Stat:=MyControls.AddLabel(KMPanel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    KMLabel_Hint:=MyControls.AddLabel(KMPanel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,'',fnt_Outline,kaLeft);
    KMLabel_PointerCount:=MyControls.AddLabel(KMPanel_Main,224+8,100,0,0,'',fnt_Outline,kaLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Pause_Page();
  Create_Build_Page();
  Create_Ratios_Page();
  Create_Stats_Page();
  Create_Menu_Page();
    Create_Save_Page();
    Create_Load_Page();
    Create_Settings_Page();
    Create_Quit_Page();

  Create_Unit_Page();
  Create_House_Page();
    Create_Store_Page();
    Create_School_Page();
    Create_Barracks_Page();
    //Create_TownHall_Page();

  SetHintEvents(DisplayHint); //Set all OnHint events to be the correct function

  SwitchPage(nil); //Update
end;


destructor TKMGamePlayInterface.Destroy;
begin
  FreeAndNil(fMessageList);
  FreeAndNil(MyControls);
  inherited;
end;


procedure TKMGamePlayInterface.SetScreenSize(X,Y:word);
begin
  KMBevel_Pause.Width:=X+2;
  KMImage_Pause.Left:=X div 2;
  KMLabel_Pause1.Left:=X div 2;
  KMLabel_Pause2.Left:=X div 2;

  KMBevel_Pause.Height:=Y+2;
  KMImage_Pause.Top:=(Y div 2)-40; //This one is wrong, it looses center 
  KMLabel_Pause1.Top:=(Y div 2);
  KMLabel_Pause2.Top:=(Y div 2)+20;

  //todo: Also update Hint position and all messages in queue..
end;


{Pause overlay page}
procedure TKMGamePlayInterface.Create_Pause_Page;
begin
  KMPanel_Pause:=MyControls.AddPanel(KMPanel_Main,0,0,fRender.GetRenderAreaSize.X,fRender.GetRenderAreaSize.Y);
    KMBevel_Pause:=MyControls.AddBevel(KMPanel_Pause,-1,-1,fRender.GetRenderAreaSize.X+2,fRender.GetRenderAreaSize.Y+2);
    KMImage_Pause:=MyControls.AddImage(KMPanel_Pause,(fRender.GetRenderAreaSize.X div 2),(fRender.GetRenderAreaSize.Y div 2)-40,0,0,556);
    KMImage_Pause.PivotX:=pl_Avg;
    KMImage_Pause.PivotY:=pl_Avg;
    KMLabel_Pause1:=MyControls.AddLabel(KMPanel_Pause,(fRender.GetRenderAreaSize.X div 2),(fRender.GetRenderAreaSize.Y div 2),64,16,fTextLibrary.GetTextString(308),fnt_Antiqua,kaCenter);
    KMLabel_Pause2:=MyControls.AddLabel(KMPanel_Pause,(fRender.GetRenderAreaSize.X div 2),(fRender.GetRenderAreaSize.Y div 2)+20,64,16,'Press ''P'' to resume the game',fnt_Grey,kaCenter);
    KMPanel_Pause.Hide
end;


{Message page}
procedure TKMGamePlayInterface.Create_Message_Page;
begin
  KMPanel_Message:=MyControls.AddPanel(KMPanel_Main,TOOLBARWIDTH,fRender.GetRenderAreaSize.Y-190,fRender.GetRenderAreaSize.X-TOOLBARWIDTH,190);
    KMImage_MessageBG:=MyControls.AddImage(KMPanel_Message,0,20,fRender.GetRenderAreaSize.X-TOOLBARWIDTH,170,409);

end;

{Build page}
procedure TKMGamePlayInterface.Create_Build_Page;
var i:integer;
begin
  KMPanel_Build:=MyControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMLabel_Build:=MyControls.AddLabel(KMPanel_Build,100,10,100,30,'',fnt_Outline,kaCenter);
    KMImage_Build_Selected:=MyControls.AddImage(KMPanel_Build,8,40,32,32,335);
    KMImage_BuildCost_WoodPic:=MyControls.AddImage(KMPanel_Build,75,40,32,32,353);
    KMImage_BuildCdost_StonePic:=MyControls.AddImage(KMPanel_Build,130,40,32,32,352);
    KMLabel_BuildCost_Wood:=MyControls.AddLabel(KMPanel_Build,105,50,10,30,'',fnt_Outline,kaLeft);
    KMLabel_BuildCost_Stone:=MyControls.AddLabel(KMPanel_Build,160,50,10,30,'',fnt_Outline,kaLeft);
    KMButton_BuildRoad   := MyControls.AddButtonFlat(KMPanel_Build,  8,80,33,33,335);
    KMButton_BuildField  := MyControls.AddButtonFlat(KMPanel_Build, 45,80,33,33,337);
    KMButton_BuildWine   := MyControls.AddButtonFlat(KMPanel_Build, 82,80,33,33,336);
//    KMButton_BuildWall   := MyControls.AddButtonFlat(KMPanel_Build,119,80,33,33,339);
    KMButton_BuildCancel := MyControls.AddButtonFlat(KMPanel_Build,156,80,33,33,340);
    KMButton_BuildRoad.OnClick:=Build_ButtonClick;
    KMButton_BuildField.OnClick:=Build_ButtonClick;
    KMButton_BuildWine.OnClick:=Build_ButtonClick;
//    KMButton_BuildWall.OnClick:=Build_ButtonClick;
    KMButton_BuildCancel.OnClick:=Build_ButtonClick;
    KMButton_BuildRoad.Hint:=fTextLibrary.GetTextString(213);
    KMButton_BuildField.Hint:=fTextLibrary.GetTextString(215);
    KMButton_BuildWine.Hint:=fTextLibrary.GetTextString(219);
//    KMButton_BuildWall.Hint:='Build a wall';
    KMButton_BuildCancel.Hint:=fTextLibrary.GetTextString(211);

    for i:=1 to HOUSE_COUNT do
      if GUIHouseOrder[i] <> ht_None then begin
        KMButton_Build[i]:=MyControls.AddButtonFlat(KMPanel_Build, 8+((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,
        GUIBuildIcons[byte(GUIHouseOrder[i])]);

        KMButton_Build[i].OnClick:=Build_ButtonClick;
        KMButton_Build[i].Hint:=fTextLibrary.GetTextString(GUIBuildIcons[byte(GUIHouseOrder[i])]-300);
      end;
end;


{Ratios page}
procedure TKMGamePlayInterface.Create_Ratios_Page;
const ResPic:array[1..4] of TResourceType = (rt_Steel,rt_Coal,rt_Wood,rt_Corn);
      ResHint:array[1..4] of word = (297,299,301,303);
var i:integer;
begin
  KMPanel_Ratios:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);

  for i:=1 to 4 do begin
    KMButton_Ratios[i] := MyControls.AddButton(KMPanel_Ratios, 8+(i-1)*40,20,32,32,350+byte(ResPic[i]));
    KMButton_Ratios[i].Hint := fTextLibrary.GetTextString(ResHint[i]);
    KMButton_Ratios[i].Tag := i;
    KMButton_Ratios[i].OnClick := SwitchPageRatios;
  end;

  KMImage_RatioPic0:=MyControls.AddImage(KMPanel_Ratios,12,76,32,32,327);
  KMLabel_RatioLab0:=MyControls.AddLabel(KMPanel_Ratios,44,72,100,30,'<<<LEER>>>',fnt_Outline,kaLeft);

  for i:=1 to 4 do begin
    KMImage_RatioPic[i]:=MyControls.AddImage(KMPanel_Ratios,12,124+(i-1)*50,32,32,327);
    KMLabel_RatioLab[i]:=MyControls.AddLabel(KMPanel_Ratios,50,116+(i-1)*50,100,30,'<<<LEER>>>',fnt_Grey,kaLeft);
    KMRatio_RatioRat[i]:=MyControls.AddRatioRow(KMPanel_Ratios,48,136+(i-1)*50,140,20,0,5);
    KMRatio_RatioRat[i].Tag:=i;
    KMRatio_RatioRat[i].OnChange:=RatiosChange;
    KMRatio_RatioRat[i].Disable;
  end;
end;


{Statistics page}
procedure TKMGamePlayInterface.Create_Stats_Page;
const LineHeight=34; Nil_Width=10; House_Width=30; Unit_Width=26;
var i,k:integer; hc,uc,off:integer;
  LineBase:integer;
begin
  KMPanel_Stats:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);

  hc:=1; uc:=1;
  for i:=1 to 8 do begin
    LineBase := (i-1)*LineHeight;
    case i of //todo: This should be simplified, compacted and automated
    1: begin
          MyControls.AddBevel(KMPanel_Stats,  8,LineBase,56,30);
          MyControls.AddBevel(KMPanel_Stats, 71,LineBase,56,30);
          MyControls.AddBevel(KMPanel_Stats,134,LineBase,56,30);
       end;
    2: begin
          MyControls.AddBevel(KMPanel_Stats,  8,LineBase,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,LineBase,86,30);
       end;
    3: begin
          MyControls.AddBevel(KMPanel_Stats,  8,LineBase,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,LineBase,86,30);
       end;
    4: begin
          MyControls.AddBevel(KMPanel_Stats,  8,LineBase,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,LineBase,86,30);
       end;
    5:    MyControls.AddBevel(KMPanel_Stats,8,LineBase,116,30);
    6:    MyControls.AddBevel(KMPanel_Stats,8,LineBase,146,30);
    7:    MyControls.AddBevel(KMPanel_Stats,8,LineBase,86,30);
    8: begin
          MyControls.AddBevel(KMPanel_Stats,  8,LineBase,120,30);
          MyControls.AddBevel(KMPanel_Stats,138,LineBase,52,30);
       end;
    end;

    off:=8;
    for k:=1 to 8 do
    case StatCount[i,k] of
      0: if i=1 then
           inc(off,Nil_Width-3) //Special fix to fit first row of 3x2 items
         else
           inc(off,Nil_Width);
      1: begin
        Stat_HousePic[hc]:=MyControls.AddImage(KMPanel_Stats,off,LineBase,House_Width,30,41{byte(StatHouse[hc])+300});
        Stat_HouseQty[hc]:=MyControls.AddLabel(KMPanel_Stats,off+House_Width-2,LineBase+16,37,30,'-',fnt_Grey,kaRight);
        Stat_HousePic[hc].Hint:=TypeToString(StatHouse[hc]);
        Stat_HouseQty[hc].Hint:=TypeToString(StatHouse[hc]);
        inc(hc);
        inc(off,House_Width);
         end;
      2: begin
        Stat_UnitPic[uc]:=MyControls.AddImage(KMPanel_Stats,off,LineBase,Unit_Width,30,byte(StatUnit[uc])+140);
        Stat_UnitQty[uc]:=MyControls.AddLabel(KMPanel_Stats,off+Unit_Width-2,LineBase+16,33,30,'-',fnt_Grey,kaRight);
        Stat_UnitPic[uc].Hint:=TypeToString(StatUnit[uc]);
        Stat_UnitQty[uc].Hint:=TypeToString(StatUnit[uc]);
        inc(uc);
        inc(off,Unit_Width);
         end;
    end;
  end;
end;


{Menu page}
procedure TKMGamePlayInterface.Create_Menu_Page;
begin
  KMPanel_Menu:=MyControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMButton_Menu_Save:=MyControls.AddButton(KMPanel_Menu,8,20,180,30,fTextLibrary.GetTextString(175),fnt_Metal);
    KMButton_Menu_Save.OnClick:=Menu_ShowLoad;
    KMButton_Menu_Save.Hint:=fTextLibrary.GetTextString(175);
    KMButton_Menu_Load:=MyControls.AddButton(KMPanel_Menu,8,60,180,30,fTextLibrary.GetTextString(174),fnt_Metal);
    KMButton_Menu_Load.OnClick:=Menu_ShowLoad;
    KMButton_Menu_Load.Hint:=fTextLibrary.GetTextString(174);
    KMButton_Menu_Load.Disable;
    KMButton_Menu_Settings:=MyControls.AddButton(KMPanel_Menu,8,100,180,30,fTextLibrary.GetTextString(179),fnt_Metal);
    KMButton_Menu_Settings.OnClick:=Menu_ShowSettings;
    KMButton_Menu_Settings.Hint:=fTextLibrary.GetTextString(179);
    KMButton_Menu_Quit:=MyControls.AddButton(KMPanel_Menu,8,180,180,30,fTextLibrary.GetTextString(180),fnt_Metal);
    KMButton_Menu_Quit.Hint:=fTextLibrary.GetTextString(180);
    KMButton_Menu_Quit.OnClick:=SwitchPage;
    KMButton_Menu_TrackUp  :=MyControls.AddButton(KMPanel_Menu,158,320,30,30,'>',fnt_Metal);
    KMButton_Menu_TrackDown:=MyControls.AddButton(KMPanel_Menu,  8,320,30,30,'<',fnt_Metal);
    KMButton_Menu_TrackUp.Hint  :=fTextLibrary.GetTextString(209);
    KMButton_Menu_TrackDown.Hint:=fTextLibrary.GetTextString(208);
    KMButton_Menu_TrackUp.OnClick  :=Menu_NextTrack;
    KMButton_Menu_TrackDown.OnClick:=Menu_PreviousTrack;
    KMLabel_Menu_Music:=MyControls.AddLabel(KMPanel_Menu,100,298,100,30,fTextLibrary.GetTextString(207),fnt_Metal,kaCenter);
    KMLabel_Menu_Track:=MyControls.AddLabel(KMPanel_Menu,100,326,100,30,'Spirit',fnt_Grey,kaCenter);
end;


{Save page}
procedure TKMGamePlayInterface.Create_Save_Page;
var i:integer;
begin
  KMPanel_Save:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    for i:=1 to SAVEGAME_COUNT do begin
      KMButton_Save[i]:=MyControls.AddButton(KMPanel_Save,12,10+(i-1)*28,170,24,'Savegame #'+inttostr(i),fnt_Grey);
      KMButton_Save[i].OnClick:=SaveGame;
      KMButton_Save[i].Tag:=i; //Simplify usage
    end;
end;


{Load page}
procedure TKMGamePlayInterface.Create_Load_Page;
var i:integer;
begin
  KMPanel_Load:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    for i:=1 to SAVEGAME_COUNT do begin
      KMButton_Load[i]:=MyControls.AddButton(KMPanel_Load,12,10+(i-1)*28,170,24,'Savegame #'+inttostr(i),fnt_Grey);
      //KMButton_Load[i].OnClick:=LoadGame;
      KMButton_Load[i].Disable;
    end;
end;


{Options page}
procedure TKMGamePlayInterface.Create_Settings_Page;
var i:integer;
begin
  KMPanel_Settings:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    MyControls.AddLabel(KMPanel_Settings,100,10,100,30,fTextLibrary.GetTextString(181),fnt_Metal,kaCenter);
    KMButton_Settings_Dark:=MyControls.AddButton(KMPanel_Settings,8,30,36,24,fTextLibrary.GetTextString(183),fnt_Metal);
    KMButton_Settings_Light:=MyControls.AddButton(KMPanel_Settings,154,30,36,24,fTextLibrary.GetTextString(182),fnt_Metal);
    KMButton_Settings_Dark.Hint:=fTextLibrary.GetTextString(185);
    KMButton_Settings_Light.Hint:=fTextLibrary.GetTextString(184);
    KMLabel_Settings_BrightValue:=MyControls.AddLabel(KMPanel_Settings,100,34,100,30,'',fnt_Grey,kaCenter);
    KMCheckBox_Settings_Autosave:=MyControls.AddCheckBox(KMPanel_Settings,8,70,100,30,fTextLibrary.GetTextString(203),fnt_Metal);
    KMCheckBox_Settings_Autosave.Disable;
    KMCheckBox_Settings_FastScroll:=MyControls.AddCheckBox(KMPanel_Settings,8,95,100,30,fTextLibrary.GetTextString(204),fnt_Metal);
    KMLabel_Settings_MouseSpeed:=MyControls.AddLabel(KMPanel_Settings,24,130,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
    KMLabel_Settings_MouseSpeed.Disable;
    KMRatio_Settings_Mouse:=MyControls.AddRatioRow(KMPanel_Settings,18,150,160,20,fGame.fGameSettings.GetSlidersMin,fGame.fGameSettings.GetSlidersMax);
    KMRatio_Settings_Mouse.Disable;
    KMRatio_Settings_Mouse.Hint:=fTextLibrary.GetTextString(193);
    KMLabel_Settings_SFX:=MyControls.AddLabel(KMPanel_Settings,24,178,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
    KMRatio_Settings_SFX:=MyControls.AddRatioRow(KMPanel_Settings,18,198,160,20,fGame.fGameSettings.GetSlidersMin,fGame.fGameSettings.GetSlidersMax);
    KMRatio_Settings_SFX.Hint:=fTextLibrary.GetTextString(195);
    KMLabel_Settings_Music:=MyControls.AddLabel(KMPanel_Settings,24,226,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
    KMRatio_Settings_Music:=MyControls.AddRatioRow(KMPanel_Settings,18,246,160,20,fGame.fGameSettings.GetSlidersMin,fGame.fGameSettings.GetSlidersMax);
    KMRatio_Settings_Music.Hint:=fTextLibrary.GetTextString(195);
    KMLabel_Settings_Music2:=MyControls.AddLabel(KMPanel_Settings,100,280,100,30,fTextLibrary.GetTextString(197),fnt_Metal,kaCenter);
    KMButton_Settings_Music:=MyControls.AddButton(KMPanel_Settings,8,300,180,30,'',fnt_Metal);
    KMButton_Settings_Music.Hint:=fTextLibrary.GetTextString(198);
    //There are many clickable controls, so let them all be handled in one procedure to save dozens of lines of code
    for i:=1 to KMPanel_Settings.ChildCount do
    begin
      TKMControl(KMPanel_Settings.Childs[i]).OnClick:=Menu_Settings_Change;
      TKMControl(KMPanel_Settings.Childs[i]).OnChange:=Menu_Settings_Change;
    end;
end;


{Quit page}
procedure TKMGamePlayInterface.Create_Quit_Page;
begin
  KMPanel_Quit:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    MyControls.AddLabel(KMPanel_Quit,100,30,100,30,fTextLibrary.GetTextString(176),fnt_Outline,kaCenter);
    KMButton_Quit_Yes:=MyControls.AddButton(KMPanel_Quit,8,100,180,30,fTextLibrary.GetTextString(177),fnt_Metal);
    KMButton_Quit_No:=MyControls.AddButton(KMPanel_Quit,8,140,180,30,fTextLibrary.GetTextString(178),fnt_Metal);
    KMButton_Quit_Yes.Hint:=fTextLibrary.GetTextString(177);
    KMButton_Quit_No.Hint:=fTextLibrary.GetTextString(178);
    KMButton_Quit_Yes.OnClick:=Menu_QuitMission;
    KMButton_Quit_No.OnClick:=SwitchPage;
end;


{Unit page}
procedure TKMGamePlayInterface.Create_Unit_Page;
begin
  KMPanel_Unit:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    KMLabel_UnitName:=MyControls.AddLabel(KMPanel_Unit,100,16,100,30,'',fnt_Outline,kaCenter);
    KMImage_UnitPic:=MyControls.AddImage(KMPanel_Unit,8,38,54,100,521);
    KMLabel_UnitCondition:=MyControls.AddLabel(KMPanel_Unit,120,40,100,30,fTextLibrary.GetTextString(254),fnt_Grey,kaCenter);
    KMConditionBar_Unit:=MyControls.AddPercentBar(KMPanel_Unit,73,55,116,15,80);
    KMLabel_UnitTask:=MyControls.AddLabel(KMPanel_Unit,73,74,130,30,'',fnt_Grey,kaLeft);
    KMLabel_UnitAct:=MyControls.AddLabel(KMPanel_Unit,73,94,130,30,'',fnt_Grey,kaLeft);
    KMLabel_UnitAct.AutoWrap:=true;
    KMButton_Die:=MyControls.AddButton(KMPanel_Unit,73,112,54,20,'Die',fnt_Grey);
    KMButton_Die.OnClick:=Unit_Die;
    KMLabel_UnitDescription:=MyControls.AddLabel(KMPanel_Unit,8,152,236,200,'',fnt_Grey,kaLeft); //Taken from LIB resource
    //Military buttons start at 8.170 and are 52x38/30 (60x46)
end;


{House description page}
procedure TKMGamePlayInterface.Create_House_Page;
var i:integer;
begin
  KMPanel_House:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    //Thats common things
    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    KMLabel_House:=MyControls.AddLabel(KMPanel_House,100,14,100,30,'',fnt_Outline,kaCenter);
    KMButton_House_Goods:=MyControls.AddButton(KMPanel_House,9,42,30,30,37);
    KMButton_House_Goods.OnClick := House_WareDeliveryToggle;
    KMButton_House_Goods.Hint := fTextLibrary.GetTextString(249);
    KMButton_House_Repair:=MyControls.AddButton(KMPanel_House,39,42,30,30,40);
    KMButton_House_Repair.OnClick := House_RepairToggle;
    KMButton_House_Repair.Hint := fTextLibrary.GetTextString(250);
    KMImage_House_Logo:=MyControls.AddImage(KMPanel_House,68,41,32,32,338);
    KMImage_House_Worker:=MyControls.AddImage(KMPanel_House,98,41,32,32,141);
    KMLabel_HouseHealth:=MyControls.AddLabel(KMPanel_House,156,45,30,50,fTextLibrary.GetTextString(228),fnt_Mini,kaCenter,$FFFFFFFF);
    KMHealthBar_House:=MyControls.AddPercentBar(KMPanel_House,129,57,55,15,50,'',fnt_Mini);
    KMLabel_House_UnderConstruction:=MyControls.AddLabel(KMPanel_House,100,170,100,30,fTextLibrary.GetTextString(230),fnt_Grey,kaCenter);

    KMLabel_House_Demolish:=MyControls.AddLabel(KMPanel_House,100,130,100,30,fTextLibrary.GetTextString(232),fnt_Grey,kaCenter);
    KMButton_House_DemolishYes:=MyControls.AddButton(KMPanel_House,8,185,180,30,fTextLibrary.GetTextString(231),fnt_Metal);
    KMButton_House_DemolishNo :=MyControls.AddButton(KMPanel_House,8,220,180,30,fTextLibrary.GetTextString(224),fnt_Metal);
    KMButton_House_DemolishYes.Hint:=fTextLibrary.GetTextString(233);
    KMButton_House_DemolishNo.Hint:= fTextLibrary.GetTextString(224);
    KMButton_House_DemolishYes.OnClick:=House_Demolish;
    KMButton_House_DemolishNo.OnClick:= House_Demolish;

    KMPanel_House_Common:=MyControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_Common_Demand:=MyControls.AddLabel(KMPanel_House_Common,100,2,100,30,fTextLibrary.GetTextString(227),fnt_Grey,kaCenter);
      KMLabel_Common_Offer:=MyControls.AddLabel(KMPanel_House_Common,100,2,100,30,'',fnt_Grey,kaCenter);
      KMLabel_Common_Costs:=MyControls.AddLabel(KMPanel_House_Common,100,2,100,30,fTextLibrary.GetTextString(248),fnt_Grey,kaCenter);
      KMRow_Common_Resource[1] :=MyControls.AddResourceRow(KMPanel_House_Common,  8,22,180,20,rt_Trunk,5);
      KMRow_Common_Resource[2] :=MyControls.AddResourceRow(KMPanel_House_Common,  8,42,180,20,rt_Stone,5);
      KMRow_Common_Resource[3] :=MyControls.AddResourceRow(KMPanel_House_Common,  8,62,180,20,rt_Trunk,5);
      KMRow_Common_Resource[4] :=MyControls.AddResourceRow(KMPanel_House_Common,  8,82,180,20,rt_Stone,5);
      for i:=1 to 4 do begin
        KMRow_Order[i] :=MyControls.AddResourceOrderRow(KMPanel_House_Common,  8,22,180,20,rt_Trunk,5);
        KMRow_Order[i].OrderRem.OnClick:=House_OrderClick;
        KMRow_Order[i].OrderRem.OnRightClick:=House_OrderClickRight;
        KMRow_Order[i].OrderRem.Hint:=fTextLibrary.GetTextString(234);
        KMRow_Order[i].OrderAdd.OnClick:=House_OrderClick;
        KMRow_Order[i].OrderAdd.OnRightClick:=House_OrderClickRight;
        KMRow_Order[i].OrderAdd.Hint:=fTextLibrary.GetTextString(235);
      end;
      KMRow_Costs[1] :=MyControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[2] :=MyControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[3] :=MyControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[4] :=MyControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
end;

{Store page}
procedure TKMGamePlayInterface.Create_Store_Page;
var i:integer;
begin
    KMPanel_HouseStore:=MyControls.AddPanel(KMPanel_House,0,76,200,400);
      for i:=1 to 28 do begin
        KMButton_Store[i]:=MyControls.AddButtonFlat(KMPanel_HouseStore, 8+((i-1)mod 5)*36,19+((i-1)div 5)*42,32,36,350+i);
        KMButton_Store[i].OnClick:=House_StoreAcceptFlag;
        KMButton_Store[i].Tag:=i;
        KMButton_Store[i].Hint:=TypeToString(TResourceType(i));
        KMImage_Store_Accept[i]:=MyControls.AddImage(KMPanel_HouseStore, 8+((i-1)mod 5)*36+9,18+((i-1)div 5)*42-11,32,36,49);
        KMImage_Store_Accept[i].FOnClick:=House_StoreAcceptFlag;
        KMImage_Store_Accept[i].Hint:=TypeToString(TResourceType(i));
      end;
end;


{School page}
procedure TKMGamePlayInterface.Create_School_Page;
var i:integer;
begin
    KMPanel_House_School:=MyControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_School_Res:=MyControls.AddLabel(KMPanel_House_School,100,2,100,30,fTextLibrary.GetTextString(227),fnt_Grey,kaCenter);
      KMResRow_School_Resource :=MyControls.AddResourceRow(KMPanel_House_School,  8,22,180,20,rt_Gold,5);
      KMResRow_School_Resource.Hint :=TypeToString(rt_Gold);
      KMButton_School_UnitWIP :=MyControls.AddButton(KMPanel_House_School,  8,48,32,32,0);
      KMButton_School_UnitWIP.Hint:=fTextLibrary.GetTextString(225);
      KMButton_School_UnitWIPBar:=MyControls.AddPercentBar(KMPanel_House_School,42,54,138,20,0);
      KMButton_School_UnitWIP.OnClick:= House_SchoolUnitRemove;
      for i:=1 to 5 do begin
        KMButton_School_UnitPlan[i]:= MyControls.AddButtonFlat(KMPanel_House_School, 8+(i-1)*36,80,32,32,0);
        KMButton_School_UnitPlan[i].OnClick:= House_SchoolUnitRemove;
      end;
      KMLabel_School_Unit:=MyControls.AddLabel(KMPanel_House_School,100,116,100,30,'',fnt_Outline,kaCenter);
      KMImage_School_Left :=MyControls.AddImage(KMPanel_House_School,  8,136,54,80,521);
      KMImage_School_Left.Enabled := false;
      KMImage_School_Train:=MyControls.AddImage(KMPanel_House_School, 70,136,54,80,522);
      KMImage_School_Right:=MyControls.AddImage(KMPanel_House_School,132,136,54,80,523);
      KMImage_School_Right.Enabled := false;
      KMButton_School_Left :=MyControls.AddButton(KMPanel_House_School,  8,226,54,40,35);
      KMButton_School_Train:=MyControls.AddButton(KMPanel_House_School, 70,226,54,40,42);
      KMButton_School_Right:=MyControls.AddButton(KMPanel_House_School,132,226,54,40,36);
      KMButton_School_Left.OnClick:=House_SchoolUnitChange;
      KMButton_School_Train.OnClick:=House_SchoolUnitChange;
      KMButton_School_Right.OnClick:=House_SchoolUnitChange;
      KMButton_School_Left.OnRightClick:=House_SchoolUnitChangeRight;
      KMButton_School_Right.OnRightClick:=House_SchoolUnitChangeRight;
      KMButton_School_Left.Hint :=fTextLibrary.GetTextString(242);
      KMButton_School_Train.Hint:=fTextLibrary.GetTextString(243);
      KMButton_School_Right.Hint:=fTextLibrary.GetTextString(241);
end;


{Barracks page}
procedure TKMGamePlayInterface.Create_Barracks_Page;
var i:integer;
begin
    KMPanel_HouseBarracks:=MyControls.AddPanel(KMPanel_House,0,76,200,400);
      for i:=1 to 12 do
      begin
        KMButton_Barracks[i]:=MyControls.AddButtonFlat(KMPanel_HouseBarracks, 8+((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,366+i);
        KMButton_Barracks[i].TexOffsetX:=1;
        KMButton_Barracks[i].TexOffsetY:=1;
        KMButton_Barracks[i].CapOffsetY:=2;
        KMButton_Barracks[i].HideHighlight:=true;
        KMButton_Barracks[i].Hint:=TypeToString(TResourceType(16+i));
      end;
      KMButton_Barracks[12].TexID:=154;
      KMButton_Barracks[12].Hint:=TypeToString(ut_Recruit);

      KMLabel_Barracks_Unit:=MyControls.AddLabel(KMPanel_HouseBarracks,100,96,100,30,'',fnt_Outline,kaCenter);

      KMImage_Barracks_Left :=MyControls.AddImage(KMPanel_HouseBarracks,  8,116,54,80,535);
      KMImage_Barracks_Left.Enabled := false;
      KMImage_Barracks_Train:=MyControls.AddImage(KMPanel_HouseBarracks, 70,116,54,80,536);
      KMImage_Barracks_Right:=MyControls.AddImage(KMPanel_HouseBarracks,132,116,54,80,537);
      KMImage_Barracks_Right.Enabled := false;

      KMButton_Barracks_Left :=MyControls.AddButton(KMPanel_HouseBarracks,  8,226,54,40,35);
      KMButton_Barracks_Train:=MyControls.AddButton(KMPanel_HouseBarracks, 70,226,54,40,42);
      KMButton_Barracks_Right:=MyControls.AddButton(KMPanel_HouseBarracks,132,226,54,40,36);
      KMButton_Barracks_Left.OnClick:=House_BarracksUnitChange;
      KMButton_Barracks_Train.OnClick:=House_BarracksUnitChange;
      KMButton_Barracks_Right.OnClick:=House_BarracksUnitChange;
      KMButton_Barracks_Left.OnRightClick:=House_BarracksUnitChangeRight;
      KMButton_Barracks_Right.OnRightClick:=House_BarracksUnitChangeRight;
      KMButton_Barracks_Left.Hint :=fTextLibrary.GetTextString(237);
      KMButton_Barracks_Train.Hint:=fTextLibrary.GetTextString(240);
      KMButton_Barracks_Right.Hint:=fTextLibrary.GetTextString(238);
      KMButton_Barracks_Train.Disable; //Unimplemented yet
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMGamePlayInterface.UpdateState;
begin
  if ShownUnit<>nil then ShowUnitInfo(ShownUnit) else
  if ShownHouse<>nil then ShowHouseInfo(ShownHouse,AskDemolish);

  if ShownHint<>nil then DisplayHint(ShownHint,[],0,0);
  if Mouse.CursorPos.X>ToolBarWidth then DisplayHint(nil,[],0,0); //Don't display hints if not over ToolBar

  Minimap_Update(nil);
  if KMImage_Clock.Visible then begin
    KMImage_Clock.TexID := ((KMImage_Clock.TexID-556)+1)mod 16 +556;
    KMLabel_Clock.Caption := int2time(fGame.GetMissionTime); 
  end;

  KMLabel_PointerCount.Caption := 'Pointers: U,H: '+IntToStr(MyPlayer.GetUnits.GetTotalPointers)+','+IntToStr(MyPlayer.GetHouses.GetTotalPointers);

  if KMPanel_Build.Visible then Build_Fill(nil);
  if KMPanel_Stats.Visible then Stats_Fill(nil);
  if KMPanel_Menu.Visible then Menu_Fill(nil);

  if SHOW_SPRITE_COUNT then
  KMLabel_Stat.Caption:=
        inttostr(fPlayers.GetUnitCount)+' units'+#124+
        inttostr(fRender.Stat_Sprites)+'/'+inttostr(fRender.Stat_Sprites2)+' sprites/rendered'+#124+
        '';
end;


procedure TKMGamePlayInterface.Build_ButtonClick(Sender: TObject);
var i:integer;
begin
  if Sender=nil then begin CursorMode.Mode:=cm_None; exit; end;

  //Release all buttons
  for i:=1 to KMPanel_Build.ChildCount do
    if KMPanel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(KMPanel_Build.Childs[i]).Down:=false;

  //Press the button
  TKMButtonFlat(Sender).Down:=true;

  //Reset cursor and see if it needs to be changed
  CursorMode.Mode:=cm_None;
  CursorMode.Param:=0;
  KMLabel_BuildCost_Wood.Caption:='-';
  KMLabel_BuildCost_Stone.Caption:='-';
  KMLabel_Build.Caption := '';

  
  if KMButton_BuildCancel.Down then begin
    CursorMode.Mode:=cm_Erase;
    KMImage_Build_Selected.TexID := 340;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(210);
  end;
  if KMButton_BuildRoad.Down then begin
    CursorMode.Mode:=cm_Road;
    KMImage_Build_Selected.TexID := 335;
    KMLabel_BuildCost_Stone.Caption:='1';
    KMLabel_Build.Caption := fTextLibrary.GetTextString(212);
  end;
  if KMButton_BuildField.Down then begin
    CursorMode.Mode:=cm_Field;
    KMImage_Build_Selected.TexID := 337;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(214);
  end;
  if KMButton_BuildWine.Down then begin
    CursorMode.Mode:=cm_Wine;
    KMImage_Build_Selected.TexID := 336;
    KMLabel_BuildCost_Wood.Caption:='1';
    KMLabel_Build.Caption := fTextLibrary.GetTextString(218);
  end;
{  if KMButton_BuildWall.Down then begin
    CursorMode.Mode:=cm_Wall;
    KMImage_Build_Selected.TexID := 339;
    KMLabel_BuildCost_Wood.Caption:='1';
    //KMLabel_Build.Caption := fTextLibrary.GetTextString(218);
  end;}

  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if KMButton_Build[i].Down then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Param:=byte(GUIHouseOrder[i]);
     KMImage_Build_Selected.TexID := GUIBuildIcons[byte(GUIHouseOrder[i])]; //Now update the selected icon
     KMLabel_BuildCost_Wood.Caption:=inttostr(HouseDAT[byte(GUIHouseOrder[i])].WoodCost);
     KMLabel_BuildCost_Stone.Caption:=inttostr(HouseDAT[byte(GUIHouseOrder[i])].StoneCost);
     KMLabel_Build.Caption := TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end;
end;


procedure TKMGamePlayInterface.ShowHouseInfo(Sender:TKMHouse; aAskDemolish:boolean=false);
const LineAdv = 25; //Each new Line is placed ## pixels after previous
var i,RowRes,Base,Line:integer;
begin
  ShownUnit:=nil;
  ShownHouse:=Sender;
  AskDemolish:=aAskDemolish;

  if (not Assigned(Sender)) then begin //=nil produces wrong result when there's no object at all
    SwitchPage(nil);
    exit;
  end;

  {Common data}
  KMLabel_House.Caption:=TypeToString(Sender.GetHouseType);
  KMImage_House_Logo.TexID:=300+byte(Sender.GetHouseType);
  KMImage_House_Worker.TexID:=140+HouseDAT[byte(Sender.GetHouseType)].OwnerType+1;
  KMImage_House_Worker.Hint := TypeToString(TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1));
  KMHealthBar_House.Caption:=inttostr(round(Sender.GetHealth))+'/'+inttostr(HouseDAT[byte(Sender.GetHouseType)].MaxHealth);
  KMHealthBar_House.Position:=round( Sender.GetHealth / HouseDAT[byte(Sender.GetHouseType)].MaxHealth * 100 );

  if AskDemolish then
  begin
    for i:=1 to KMPanel_House.ChildCount do
      KMPanel_House.Childs[i].Hide; //hide all
    KMLabel_House_Demolish.Show;
    KMButton_House_DemolishYes.Show;
    KMButton_House_DemolishNo.Show;
    KMLabel_House.Show;
    KMImage_House_Logo.Show;
    KMImage_House_Worker.Show;
    KMImage_House_Worker.Enable;
    KMHealthBar_House.Show;
    KMLabel_HouseHealth.Show;
    SwitchPage(KMPanel_House);
    exit;
  end;

  if not Sender.IsComplete then
  begin
    for i:=1 to KMPanel_House.ChildCount do
      KMPanel_House.Childs[i].Hide; //hide all
    KMLabel_House_UnderConstruction.Show;
    KMLabel_House.Show;
    KMImage_House_Logo.Show;
    KMImage_House_Worker.Show;
    KMImage_House_Worker.Enable;
    KMHealthBar_House.Show;
    KMLabel_HouseHealth.Show;
    SwitchPage(KMPanel_House);
    exit;
  end;


  for i:=1 to KMPanel_House.ChildCount do
    KMPanel_House.Childs[i].Show; //show all
  KMImage_House_Worker.Enabled := Sender.GetHasOwner;
  KMImage_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  KMButton_House_Goods.Enabled := not (HouseInput[byte(Sender.GetHouseType)][1] in [rt_None,rt_All,rt_Warfare]);
  if Sender.BuildingRepair then KMButton_House_Repair.TexID:=39 else KMButton_House_Repair.TexID:=40;
  if Sender.WareDelivery then KMButton_House_Goods.TexID:=37 else KMButton_House_Goods.TexID:=38;
  KMLabel_House_UnderConstruction.Hide;
  KMLabel_House_Demolish.Hide;
  KMButton_House_DemolishYes.Hide;
  KMButton_House_DemolishNo.Hide;
  SwitchPage(KMPanel_House);

  case Sender.GetHouseType of
    ht_Store: begin
          Store_Fill(nil);
          SwitchPage(KMPanel_HouseStore);
        end;

    ht_School: begin
          KMResRow_School_Resource.ResourceCount:=Sender.CheckResIn(rt_Gold);
          House_SchoolUnitChange(nil);
          SwitchPage(KMPanel_House_School);
        end;

    ht_Barracks: begin
          KMImage_House_Worker.Enabled := true; //In the barrack the recruit icon is always enabled
          House_BarracksUnitChange(nil);
          SwitchPage(KMPanel_HouseBarracks);
          end;
    ht_TownHall:;
  else begin

    //First thing - hide everything
    for i:=1 to KMPanel_House_Common.ChildCount do
      KMPanel_House_Common.Childs[i].Hide;

    //Now show only what we need
    RowRes:=1; Line:=0; Base:=KMPanel_House_Common.Top+2;
    //Show Demand
    if HouseInput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
      KMLabel_Common_Demand.Show;
      KMLabel_Common_Demand.Top:=Base+Line*LineAdv+6;
      inc(Line);
      for i:=1 to 4 do if HouseInput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
        KMRow_Common_Resource[RowRes].Resource:=HouseInput[byte(Sender.GetHouseType),i];
        KMRow_Common_Resource[RowRes].Hint:=TypeToString(HouseInput[byte(Sender.GetHouseType),i]);
        KMRow_Common_Resource[RowRes].ResourceCount:=Sender.CheckResIn(HouseInput[byte(Sender.GetHouseType),i]);
        KMRow_Common_Resource[RowRes].Show;
        KMRow_Common_Resource[RowRes].Top:=Base+Line*LineAdv;
        inc(Line);
        inc(RowRes);
      end;
    end;
    //Show Output
    if not HousePlaceOrders[byte(Sender.GetHouseType)] then
    if HouseOutput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
      KMLabel_Common_Offer.Show;
      KMLabel_Common_Offer.Caption:=fTextLibrary.GetTextString(229)+'(x'+inttostr(HouseDAT[byte(Sender.GetHouseType)].ResProductionX)+'):';
      KMLabel_Common_Offer.Top:=Base+Line*LineAdv+6;
      inc(Line);
      for i:=1 to 4 do
      if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
        KMRow_Common_Resource[RowRes].Resource:=HouseOutput[byte(Sender.GetHouseType),i];
        KMRow_Common_Resource[RowRes].ResourceCount:=Sender.CheckResOut(HouseOutput[byte(Sender.GetHouseType),i]);
        KMRow_Common_Resource[RowRes].Show;
        KMRow_Common_Resource[RowRes].Top:=Base+Line*LineAdv;
        KMRow_Common_Resource[RowRes].Hint:=TypeToString(HouseOutput[byte(Sender.GetHouseType),i]);
        inc(Line);
        inc(RowRes);
      end;
    end;
    //Show Orders
    if HousePlaceOrders[byte(Sender.GetHouseType)] then begin
      KMLabel_Common_Offer.Show;
      KMLabel_Common_Offer.Caption:=fTextLibrary.GetTextString(229)+'(x'+inttostr(HouseDAT[byte(Sender.GetHouseType)].ResProductionX)+'):';
      KMLabel_Common_Offer.Top:=Base+Line*LineAdv+6;
      inc(Line);
      for i:=1 to 4 do //Orders
      if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
        KMRow_Order[i].Resource:=HouseOutput[byte(Sender.GetHouseType),i];
        KMRow_Order[i].ResourceCount:=Sender.CheckResOut(HouseOutput[byte(Sender.GetHouseType),i]);
        KMRow_Order[i].OrderCount:=Sender.CheckResOrder(i);
        KMRow_Order[i].Show;
        KMRow_Order[i].OrderAdd.Show;
        KMRow_Order[i].OrderRem.Show;
        KMRow_Order[i].Hint:=TypeToString(HouseOutput[byte(Sender.GetHouseType),i]);
        KMRow_Order[i].Top:=Base+Line*LineAdv;
        inc(Line);
      end;
      KMLabel_Common_Costs.Show;
      KMLabel_Common_Costs.Top:=Base+Line*LineAdv+6;
      inc(Line);
      for i:=1 to 4 do //Costs
      if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
        KMRow_Costs[i].CostID:=byte(HouseOutput[byte(Sender.GetHouseType),i]);
        KMRow_Costs[i].Show;
        KMRow_Costs[i].Top:=Base+Line*LineAdv;
        inc(Line);
      end;

    end;
  SwitchPage(KMPanel_House_Common);
  end;
  end;
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender:TKMUnit);
begin
  ShownUnit:=Sender;
  ShownHouse:=nil;
  if (not Assigned(Sender))or(not Sender.IsVisible)or((Sender<>nil)and(Sender.IsDead)) then begin
    SwitchPage(nil);
    ShownUnit:=nil; //Make sure it doesn't come back again, especially if it's dead!
    exit;
  end;
  SwitchPage(KMPanel_Unit);
  KMLabel_UnitName.Caption:=TypeToString(Sender.GetUnitType);
  KMImage_UnitPic.TexID:=520+byte(Sender.GetUnitType);
  KMConditionBar_Unit.Position:=EnsureRange(round(Sender.GetCondition / UNIT_MAX_CONDITION * 100),-10,110);
  if Sender.GetHome<>nil then
    KMLabel_UnitTask.Caption:='Task: '+Sender.GetUnitTaskText
  else
    KMLabel_UnitTask.Caption:='Task: '+Sender.GetUnitTaskText;
  KMLabel_UnitAct.Caption:='Act: '+Sender.GetUnitActText;
  if Sender is TKMUnitWarrior then
  begin
    //Warrior specific
    KMLabel_UnitDescription.Hide;
  end
  else
  begin
    //Citizen specific
    KMLabel_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender.GetUnitType));
    KMLabel_UnitDescription.Show;
  end;
end;


procedure TKMGamePlayInterface.Unit_Die(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMUnit) then exit;
  TKMUnit(fPlayers.Selected).KillUnit;
end;


procedure TKMGamePlayInterface.House_Demolish(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  if Sender=KMButton_House_DemolishYes then begin
    MyPlayer.RemHouse(TKMHouse(fPlayers.Selected).GetPosition,false);
    ShowHouseInfo(nil, false); //Simpliest way to reset page and ShownHouse
  end else begin
    AskDemolish:=false;
    SwitchPage(KMButtonMain[1]); //Cancel and return to build menu
  end;
end;


procedure TKMGamePlayInterface.House_RepairToggle(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  with TKMHouse(fPlayers.Selected) do begin
    BuildingRepair := not BuildingRepair;
    if BuildingRepair then KMButton_House_Repair.TexID:=39
                      else KMButton_House_Repair.TexID:=40;
    if BuildingRepair then EnableRepair
                      else DisableRepair;
  end;
end;


procedure TKMGamePlayInterface.House_WareDeliveryToggle(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  with TKMHouse(fPlayers.Selected) do begin
    WareDelivery := not WareDelivery;
    if WareDelivery then KMButton_House_Goods.TexID:=37
                    else KMButton_House_Goods.TexID:=38;
    end;
end;


procedure TKMGamePlayInterface.House_OrderClick(Sender:TObject);
var i:integer;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then TKMHouse(fPlayers.Selected).ResRemOrder(i);
    if Sender = KMRow_Order[i].OrderAdd then TKMHouse(fPlayers.Selected).ResAddOrder(i);
  end;
end;


procedure TKMGamePlayInterface.House_OrderClickRight(Sender:TObject);
var i:integer;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then TKMHouse(fPlayers.Selected).ResRemOrder(i,10);
    if Sender = KMRow_Order[i].OrderAdd then TKMHouse(fPlayers.Selected).ResAddOrder(i,10);
  end;
end;


procedure TKMGamePlayInterface.House_BarracksUnitChange(Sender:TObject);
var i, k, Tmp: integer; Barracks:TKMHouseBarracks; CanEquip: boolean;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseBarracks) then exit;

  Barracks:=TKMHouseBarracks(fPlayers.Selected);
  if (Sender=KMButton_Barracks_Left)and(LastBarracksUnit > 1) then dec(LastBarracksUnit);
  if (Sender=KMButton_Barracks_Right)and(LastBarracksUnit < length(Barracks_Order)) then inc(LastBarracksUnit);

  if Sender=KMButton_Barracks_Train then //Equip unit
  begin
    //Barracks.Equip;
  end;

  CanEquip:=true;
  for i:=1 to 12 do begin
    if i in [1..11] then Tmp:=TKMHouseBarracks(fPlayers.Selected).ResourceCount[i]
                    else Tmp:=TKMHouseBarracks(fPlayers.Selected).RecruitsInside;
    if Tmp=0 then KMButton_Barracks[i].Caption:='-'
             else KMButton_Barracks[i].Caption:=inttostr(Tmp);
    //Set highlights
    KMButton_Barracks[i].Down:=false;
    for k:=1 to 4 do
      if i = TroopCost[TUnitType(14+LastBarracksUnit),k] then
      begin
        KMButton_Barracks[i].Down:=true;
        if Tmp=0 then CanEquip := false; //Can't equip if we don't have a required resource
      end;
  end;
  KMButton_Barracks[12].Down:=true; //Recruit is always enabled, all troops require one

  KMButton_Barracks_Train.Enabled := CanEquip and (Barracks.RecruitsInside > 0);
  KMButton_Barracks_Left.Enabled := LastBarracksUnit > 1;
  KMButton_Barracks_Right.Enabled := LastBarracksUnit < length(Barracks_Order);
  KMImage_Barracks_Left.Visible:= KMButton_Barracks_Left.Enabled;
  KMImage_Barracks_Right.Visible:= KMButton_Barracks_Right.Enabled;

  if KMButton_Barracks_Left.Enabled then
    KMImage_Barracks_Left.TexID:=520+byte(Barracks_Order[LastBarracksUnit-1]);

  KMLabel_Barracks_Unit.Caption:=TypeToString(TUnitType(Barracks_Order[LastBarracksUnit]));
  KMImage_Barracks_Train.TexID:=520+byte(Barracks_Order[LastBarracksUnit]);

  if KMButton_Barracks_Right.Enabled then
    KMImage_Barracks_Right.TexID:=520+byte(Barracks_Order[LastBarracksUnit+1]);
end;


procedure TKMGamePlayInterface.House_BarracksUnitChangeRight(Sender:TObject);
begin
  if Sender=KMButton_Barracks_Left  then LastBarracksUnit := 1;
  if Sender=KMButton_Barracks_Right then LastBarracksUnit := Length(Barracks_Order);
  House_BarracksUnitChange(nil);
end;


{Process click on Left-Train-Right buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitChange(Sender:TObject);
var i:byte; School:TKMHouseSchool;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseSchool) then exit;
  School:=TKMHouseSchool(fPlayers.Selected);

  if (Sender=KMButton_School_Left)and(LastSchoolUnit > 1) then dec(LastSchoolUnit);
  if (Sender=KMButton_School_Right)and(LastSchoolUnit < length(School_Order)) then inc(LastSchoolUnit);

  if Sender=KMButton_School_Train then //Add unit to training queue
  begin
    School.AddUnitToQueue(TUnitType(School_Order[LastSchoolUnit]));
  end;

  if School.UnitQueue[1]<>ut_None then
    KMButton_School_UnitWIP.TexID :=140+byte(School.UnitQueue[1])
  else
    KMButton_School_UnitWIP.TexID :=41; //Question mark

  KMButton_School_UnitWIPBar.Position:=School.GetTrainingProgress;

  for i:=1 to 5 do
    if School.UnitQueue[i+1]<>ut_None then
    begin
      KMButton_School_UnitPlan[i].TexID:=140+byte(School.UnitQueue[i+1]);
      KMButton_School_UnitPlan[i].Hint:=TypeToString(School.UnitQueue[i+1]);
    end
    else
    begin
      KMButton_School_UnitPlan[i].TexID:=0;
      KMButton_School_UnitPlan[i].Hint:='';
    end;

  KMButton_School_Train.Enabled := School.UnitQueue[length(School.UnitQueue)]=ut_None;
  KMButton_School_Left.Enabled := LastSchoolUnit > 1;
  KMButton_School_Right.Enabled := LastSchoolUnit < length(School_Order);
  KMImage_School_Left.Visible:= KMButton_School_Left.Enabled;
  KMImage_School_Right.Visible:= KMButton_School_Right.Enabled;

  if KMButton_School_Left.Enabled then
    KMImage_School_Left.TexID:=520+byte(School_Order[LastSchoolUnit-1]);

  KMLabel_School_Unit.Caption:=TypeToString(TUnitType(School_Order[LastSchoolUnit]));
  KMImage_School_Train.TexID:=520+byte(School_Order[LastSchoolUnit]);

  if KMButton_School_Right.Enabled then
    KMImage_School_Right.TexID:=520+byte(School_Order[LastSchoolUnit+1]);
end;


{Process right click on Left-Right buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitChangeRight(Sender:TObject);
begin
  if Sender=KMButton_School_Left then LastSchoolUnit := 1;
  if Sender=KMButton_School_Right then LastSchoolUnit := Length(School_Order);
  House_SchoolUnitChange(nil);
end;


{Process click on Remove-from-queue buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitRemove(Sender:TObject);
var i:integer;
begin
  if Sender = KMButton_School_UnitWIP then
    TKMHouseSchool(fPlayers.Selected).RemUnitFromQueue(1)
  else for i:=1 to 5 do
    if Sender = KMButton_School_UnitPlan[i] then
    begin
      TKMHouseSchool(fPlayers.Selected).RemUnitFromQueue(i+1);
      fSoundLib.Play(sfx_click); //This is done for all buttons now, see fGame.OnMouseDown
      //True, but these are not buttons, they are flat buttons. They still have to make the sound though. To be deleted
    end;
  House_SchoolUnitChange(nil);
end;


{That small red triangle blocking delivery of goods to Storehouse}
{Resource determined by Button.Tag property}
procedure TKMGamePlayInterface.House_StoreAcceptFlag(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;
  TKMHouseStore(fPlayers.Selected).ToggleAcceptFlag((Sender as TKMControl).Tag);
end;


procedure TKMGamePlayInterface.Menu_ShowSettings(Sender: TObject);
begin
  Menu_Settings_Change(nil); //Prepare eveything first
  SwitchPage(Sender); //Only then switch
end;


procedure TKMGamePlayInterface.Menu_Settings_Change(Sender:TObject);
begin
  if Sender = KMButton_Settings_Dark then fGame.fGameSettings.DecBrightness;
  if Sender = KMButton_Settings_Light then fGame.fGameSettings.IncBrightness;
  if Sender = KMCheckBox_Settings_Autosave then fGame.fGameSettings.IsAutosave:=not fGame.fGameSettings.IsAutosave;
  if Sender = KMCheckBox_Settings_FastScroll then fGame.fGameSettings.IsFastScroll:=not fGame.fGameSettings.IsFastScroll;
  if Sender = KMRatio_Settings_Mouse then fGame.fGameSettings.SetMouseSpeed(KMRatio_Settings_Mouse.Position);
  if Sender = KMRatio_Settings_SFX then fGame.fGameSettings.SetSoundFXVolume(KMRatio_Settings_SFX.Position);
  if Sender = KMRatio_Settings_Music then fGame.fGameSettings.SetMusicVolume(KMRatio_Settings_Music.Position);
  if Sender = KMButton_Settings_Music then fGame.fGameSettings.IsMusic:=not fGame.fGameSettings.IsMusic;
  
  KMLabel_Settings_BrightValue.Caption:=fTextLibrary.GetTextString(185 + fGame.fGameSettings.GetBrightness);
  KMCheckBox_Settings_Autosave.Checked:=fGame.fGameSettings.IsAutosave;
  KMCheckBox_Settings_FastScroll.Checked:=fGame.fGameSettings.IsFastScroll;
  KMRatio_Settings_Mouse.Position:=fGame.fGameSettings.GetMouseSpeed;
  KMRatio_Settings_SFX.Position:=fGame.fGameSettings.GetSoundFXVolume;
  KMRatio_Settings_Music.Position:=fGame.fGameSettings.GetMusicVolume;

  if fGame.fGameSettings.IsMusic then
    KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(201)
  else
    KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(199);
end;


{Show list of savegames and act depending on Sender (Save or Load)}
procedure TKMGamePlayInterface.Menu_ShowLoad(Sender: TObject);
//var i:integer;
begin
{for i:=1 to SAVEGAME_COUNT do
  if CheckSaveGameValidity(i) then begin
    KMButton_Save[i].Caption:=Savegame.Title+Savegame.Time;
    KMButton_Load[i].Caption:=Savegame.Title+Savegame.Time;
  end;}
  SwitchPage(Sender);
end;


{Quit the mission and return to main menu}
procedure TKMGamePlayInterface.Menu_QuitMission(Sender:TObject);
var i:integer;
begin
  KMPanel_Main.Hide;
  for i:=1 to KMPanel_Main.ChildCount do
    if KMPanel_Main.Childs[i] is TKMPanel then
      KMPanel_Main.Childs[i].Hide;

  fGame.StopGame(gr_Cancel);
end;


procedure TKMGamePlayInterface.Menu_NextTrack(Sender:TObject); begin fMusicLib.PlayNextTrack; end;
procedure TKMGamePlayInterface.Menu_PreviousTrack(Sender:TObject); begin fMusicLib.PlayPreviousTrack; end;


procedure TKMGamePlayInterface.Build_Fill(Sender:TObject);
var i:integer;
begin
  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if MyPlayer.GetCanBuild(THouseType(byte(GUIHouseOrder[i]))) then begin
    KMButton_Build[i].Enable;
    KMButton_Build[i].TexID:=GUIBuildIcons[byte(GUIHouseOrder[i])];
    KMButton_Build[i].OnClick:=Build_ButtonClick;
    KMButton_Build[i].Hint:=TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end else begin
    KMButton_Build[i].OnClick:=nil;
    KMButton_Build[i].TexID:=41;
    KMButton_Build[i].Hint:=fTextLibrary.GetTextString(251); //Building not available
  end;
end;


{Virtually press BuildRoad button when changing page to BuildingPage or after house plan is placed}
procedure TKMGamePlayInterface.Build_SelectRoad;
begin
  Build_ButtonClick(KMButton_BuildRoad);
end;


procedure TKMGamePlayInterface.Build_RightClickCancel;
begin
  //This function will be called if the user right clicks on the screen. We should close the build menu if it's open.
  if KMPanel_Build.Visible = true then
    SwitchPage(KMButtonMain[5]);
end;


procedure TKMGamePlayInterface.IssueMessage(MsgTyp:TKMMessageType; Text:string);
var i:integer;
begin
  fMessageList.AddEntry(MsgTyp,Text);

  //MassageList is unlimited, while KMImage_Message has fixed depth and samples data from the list on demand
  for i:=low(KMImage_Message) to high(KMImage_Message) do
  begin
    KMImage_Message[i].TexID := fMessageList.GetPicID(i);
    KMImage_Message[i].Enabled := i in [1..fMessageList.Count]; //Disable and hide at once for safety
    KMImage_Message[i].Visible := i in [1..fMessageList.Count];
  end;

  fSoundLib.Play(sfx_MessageNotice); //Play horn sound on new message
end;


procedure TKMGamePlayInterface.Store_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;

  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(fPlayers.Selected).ResourceCount[i];
    if Tmp=0 then KMButton_Store[i].Caption:='-' else
    //if Tmp>999 then KMButton_Store[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  KMButton_Store[i].Caption:=inttostr(Tmp);
    KMImage_Store_Accept[i].Visible := TKMHouseStore(fPlayers.Selected).NotAcceptFlag[i];
  end;
end;


procedure TKMGamePlayInterface.Menu_Fill(Sender:TObject);
begin
  if fGame.fGameSettings.IsMusic then
  begin
    KMLabel_Menu_Track.Caption := fMusicLib.GetTrackTitle;
    KMLabel_Menu_Track.Enabled := true;
    KMButton_Menu_TrackUp.Enabled := true;
    KMButton_Menu_TrackDown.Enabled := true;
  end
  else begin
    KMLabel_Menu_Track.Caption := '-';
    KMLabel_Menu_Track.Enabled := false;
    KMButton_Menu_TrackUp.Enabled := false;
    KMButton_Menu_TrackDown.Enabled := false;
  end;
end;

procedure TKMGamePlayInterface.Stats_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  for i:=low(StatHouse) to high(StatHouse) do
  begin
    Tmp:=MyPlayer.GetHouseQty(StatHouse[i]);
    if Tmp=0 then Stat_HouseQty[i].Caption:='-' else Stat_HouseQty[i].Caption:=inttostr(Tmp);
    if MyPlayer.GetCanBuild(StatHouse[i]) or (Tmp>0) then
    begin
      Stat_HousePic[i].TexID:=byte(StatHouse[i])+300;
      Stat_HousePic[i].Hint:=TypeToString(StatHouse[i]);
      Stat_HouseQty[i].Hint:=TypeToString(StatHouse[i]);
    end
    else
    begin
      Stat_HousePic[i].TexID:=41;
      Stat_HousePic[i].Hint:=fTextLibrary.GetTextString(251); //Building not available
      Stat_HouseQty[i].Hint:=fTextLibrary.GetTextString(251); //Building not available
    end;
  end;
  for i:=low(StatUnit) to high(StatUnit) do
  begin
    Tmp:=MyPlayer.GetUnitQty(StatUnit[i]);
    if Tmp=0 then Stat_UnitQty[i].Caption:='-' else Stat_UnitQty[i].Caption:=inttostr(Tmp);
    Stat_UnitPic[i].Hint:=TypeToString(StatUnit[i]);
    Stat_UnitQty[i].Hint:=TypeToString(StatUnit[i]);
  end;
end;

procedure TKMGamePlayInterface.SetHintEvents(AHintEvent:TMouseMoveEvent);
var
  i: integer;
begin
  //Here we must go through every control and set the hint event to be the parameter
  for i:=0 to MyControls.Count-1 do
    if MyControls.Items[i] <> nil then
      TKMControl(MyControls.Items[i]).OnHint := AHintEvent;
end;

procedure TKMGamePlayInterface.EnableOrDisableMenuIcons(NewValue:boolean);
begin
  KMButtonMain[1].Enabled := NewValue;
  KMButtonMain[2].Enabled := NewValue;
  KMButtonMain[3].Enabled := NewValue;
end;


procedure TKMGamePlayInterface.ShowClock(DoShow:boolean);
begin
  KMImage_Clock.Visible:=DoShow;
  KMLabel_Clock.Visible:=DoShow;
end;


procedure TKMGamePlayInterface.ShowPause(DoShow:boolean);
begin
  KMPanel_Pause.Visible:=DoShow;
end;


procedure TKMGamePlayInterface.ShortcutPress(Key:Word; IsDown:boolean=false);
begin
  //1-4 game menu shortcuts
  if Key in [49..52] then
  begin
    KMButtonMain[Key-48].Down := IsDown;
    if (not IsDown) and (not KMButtonMain[5].Visible) then SwitchPage(KMButtonMain[Key-48]);
  end;
  if Key=VK_ESCAPE then
  begin
    KMButtonMain[5].Down := IsDown;
    if (not IsDown) and (KMButtonMain[5].Visible) then SwitchPage(KMButtonMain[5]);
  end;
end;


procedure TKMGamePlayInterface.ClearShownUnit;
begin
  ShownUnit := nil;
  SwitchPage(nil);
end;


procedure TKMGamePlayInterface.Paint;
begin
  MyControls.Paint;
end;


end.
