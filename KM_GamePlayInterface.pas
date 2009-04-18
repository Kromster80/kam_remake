unit KM_GamePlayInterface;
interface
uses SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls, StrUtils, OpenGL,
  KM_Controls, KM_Houses, KM_Units, KM_Defaults, KM_LoadDAT;

type TKMMainMenuInterface = class
  private
    ScreenX,ScreenY:word;
    OffX,OffY:integer;
    SingleMap_Top:integer; //Top map in list
    SingleMap_Selected:integer; //Selected map
    SingleMapsInfo:TKMMapsInfo;
    SingleMap_Difficulty:byte;
  protected
    KMPanel_Main1:TKMPanel;
      L:array[1..20]of TKMLabel;
    KMPanel_MainMenu:TKMPanel;
      KMPanel_MainButtons:TKMPanel;
      KMImage_MainMenuBG,KMImage_MainMenu1,KMImage_MainMenu3:TKMImage; //Menu background
      KMButton_MainMenuTutor,KMButton_MainMenuTSK,KMButton_MainMenuTPR,
      KMButton_MainMenuSingle,KMButton_MainMenuOptions,KMButton_MainMenuCredit,KMButton_MainMenuQuit:TKMButton;
      KMLabel_Version:TKMLabel;
    KMPanel_Single:TKMPanel;
      KMImage_SingleBG:TKMImage; //Single background
      KMPanel_SingleList,KMPanel_SingleDesc:TKMPanel;
      KMButton_SingleHeadMode,KMButton_SingleHeadTeams,KMButton_SingleHeadTitle,KMButton_SingleHeadSize:TKMButton;
      KMBevel_SingleBG:array[1..MENU_SINGLE_MAPS_COUNT,1..5]of TKMBevel;
      KMButton_SingleMode:array[1..MENU_SINGLE_MAPS_COUNT]of TKMImage;
      KMButton_SinglePlayers,KMButton_SingleSize:array[1..MENU_SINGLE_MAPS_COUNT]of TKMLabel;
      KMLabel_SingleTitle1,KMLabel_SingleTitle2:array[1..MENU_SINGLE_MAPS_COUNT]of TKMLabel;
      KMScrollBar_SingleMaps:TKMScrollBar;
      KMShape_SingleMap:TKMShape;
      KMImage_SingleScroll1:TKMImage;
      KMLabel_SingleTitle,KMLabel_SingleDesc:TKMLabel;
      KMLabel_SingleCondWin,KMLabel_SingleCondDef:TKMLabel;
      KM_Button_SingleDiff:array[1..4]of TKMButtonFlat;
      KMShape_SingleDiff:TKMShape;
      KMLabel_SingleDiff:TKMLabel;
      KMButton_SingleBack,KMButton_SingleStart:TKMButton;
    KMPanel_Options:TKMPanel;
      Image_Opts_BG:TKMImage; //Credits background
      Label_Opts_MouseSpeed,Label_Opts_SFX,Label_Opts_Music:TKMLabel;
      Ratio_Opts_Mouse,Ratio_Opts_SFX,Ratio_Opts_Music:TKMRatioRow;
      Button_Opts_Back:TKMButton;
    KMPanel_Credits:TKMPanel;
      KMImage_CreditsBG:TKMImage; //Credits background
      KMButton_CreditsBack:TKMButton;
    KMPanel_Loading:TKMPanel;
      KMImage_LoadingBG:TKMImage;
      KMLabel_Loading:TKMLabel;
    KMPanel_Results:TKMPanel;
      KMImage_ResultsBG:TKMImage;
      KMButton_ResultsBack:TKMButton;
  private
    procedure Create_MainMenu_Page;
    procedure Create_Single_Page;
    procedure Create_Options_Page;
    procedure Create_Credits_Page;
    procedure Create_Loading_Page;
    procedure Create_Results_Page;
    procedure SwitchMenuPage(Sender: TObject);
    procedure SingleMap_RefreshList();
    procedure SingleMap_ScrollChange(Sender: TObject);
    procedure SingleMap_SelectMap(Sender: TObject);
    procedure SingleMap_SelectDiff(Sender: TObject);
    procedure SingleMap_Start(Sender: TObject);
    procedure Options_Change(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create(X,Y:word);
    destructor Destroy; override;
    procedure SetScreenSize(X,Y:word);
    procedure ShowScreen_Loading();
    procedure ShowScreen_Main();
    procedure ShowScreen_Results();
  public
    procedure Paint;
end;

type TKMGamePlayInterface = class
  protected
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    ShownHint:TObject;
    LastSchoolUnit:integer; //Last unit that was selected in School, global for all schools player owns
    LastBarrackUnit:integer;//Last unit that was selected in Barracks, global for all barracks player owns

    KMPanel_Main:TKMPanel;
      KMImage_Main1,KMImage_Main2,KMImage_Main3,KMImage_Main4:TKMImage; //Toolbar background
      KMMinimap:TKMMinimap;
      Button_UnPause:TKMButton;
      KMLabel_Stat,KMLabel_Hint:TKMLabel;
      KMButtonMain:array[1..5]of TKMButton; //4 common buttons + Return
      KMLabel_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
    KMPanel_Ratios:TKMPanel;
      //
    KMPanel_Stats:TKMPanel;
      Stat_House,Stat_Unit:array[1..32]of TKMButtonFlat;
      Stat_HouseQty,Stat_UnitQty:array[1..32]of TKMLabel;

    KMPanel_Build:TKMPanel;
      KMLabel_Build:TKMLabel;
      KMImage_Build_Selected:TKMImage;
      KMImage_BuildCost_WoodPic:TKMImage;
      KMImage_BuildCdost_StonePic:TKMImage;
      KMLabel_BuildCost_Wood:TKMLabel;
      KMLabel_BuildCost_Stone:TKMLabel;
      KMButton_BuildRoad,KMButton_BuildField,KMButton_BuildWine,KMButton_BuildCancel:TKMButtonFlat;
      KMButton_Build:array[1..HOUSE_COUNT]of TKMButtonFlat;

    KMPanel_Menu:TKMPanel;
      KMButton_Menu_Save,KMButton_Menu_Load,KMButton_Menu_Settings,KMButton_Menu_Quit,KMButton_Menu_Track:TKMButton;
      KMLabel_Menu_Music, KMLabel_Menu_Track: TKMLabel;

      KMPanel_Save:TKMPanel;
        KMButton_Save:array[1..SAVEGAME_COUNT]of TKMButton;

      KMPanel_Load:TKMPanel;
        KMButton_Load:array[1..SAVEGAME_COUNT]of TKMButton;

      KMPanel_Settings:TKMPanel;
        KMLabel_Settings_Brightness,KMLabel_Settings_BrightValue:TKMLabel;
        KMButton_Settings_Dark,KMButton_Settings_Light:TKMButton;
        KMLabel_Settings_Autosave,KMLabel_Settings_FastScroll:TKMLabel;
        KMLabel_Settings_MouseSpeed,KMLabel_Settings_SFX,KMLabel_Settings_Music,KMLabel_Settings_Music2:TKMLabel;
        KMRatio_Settings_Mouse,KMRatio_Settings_SFX,KMRatio_Settings_Music:TKMRatioRow;
        KMButton_Settings_Music:TKMButton;

      KMPanel_Quit:TKMPanel;
        KMLabel_Quit:TKMLabel;
        KMButton_Quit_Yes,KMButton_Quit_No:TKMButton;

    KMPanel_Unit:TKMPanel;
      KMLabel_UnitName:TKMLabel;
      KMLabel_UnitCondition:TKMLabel;
      KMLabel_UnitTask:TKMLabel;
      KMLabel_UnitAct:TKMLabel;
      KMLabel_UnitDescription:TKMLabel;
      KMConditionBar_Unit:TKMPercentBar;
      KMImage_UnitPic:TKMImage;
      
    KMPanel_House:TKMPanel;
      KMLabel_House:TKMLabel;
      KMButton_House_Goods,KMButton_House_Repair:TKMButton;
      KMImage_House_Logo,KMImage_House_Worker:TKMImage;
      KMHealthBar_House:TKMPercentBar;
      KMLabel_HouseHealth:TKMLabel;

    KMPanel_House_Common:TKMPanel;
      KMLabel_Common_Demand,KMLabel_Common_Offer,KMLabel_Common_Costs,KMLabel_House_UnderConstruction:TKMLabel;
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
  private
    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
    procedure Minimap_Update(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Build_Fill(Sender:TObject);
    procedure Store_Fill(Sender:TObject);
    procedure Barracks_Fill(Sender:TObject);
    procedure Stats_Fill(Sender:TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create;
    destructor Destroy; override;
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
    procedure UpdateState;
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
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
    procedure Build_SelectRoad;
    procedure Build_RightClickCancel;
    procedure SetHintEvents(AHintEvent:TMouseMoveEvent);
    procedure EnableOrDisableMenuIcons(NewValue:boolean);
    procedure UnPauseGame(Sender:TObject);
    procedure Paint;
  end;


implementation
uses KM_Unit1, KM_Users, KM_Settings, KM_Render, KM_LoadLib, KM_Terrain, KM_Viewport, KM_Game, KM_LoadSFX;


constructor TKMMainMenuInterface.Create(X,Y:word);
//var i:integer;
begin
inherited Create;

  Assert(fGameSettings<>nil,'fGameSettings should be init before MainMenuInterface');
  Assert(fTextLibrary<>nil,'fTextLibrary should be init before MainMenuInterface');

  {Parent Page for whole toolbar in-game}
  MyControls:=TKMControlsCollection.Create;
  ScreenX:=min(X,1024);
  ScreenY:=min(Y,768);
  OffX := (X-1024) div 2;
  OffY := (Y-768) div 2;
  SingleMap_Top:=1;
  SingleMap_Selected:=1;

  KMPanel_Main1:=MyControls.AddPanel(nil,OffX,OffY,ScreenX,ScreenY);

  Create_MainMenu_Page;
  Create_Single_Page;
  Create_Options_Page;
  Create_Credits_Page;
  Create_Loading_Page;
  Create_Results_Page;

  {for i:=1 to length(FontFiles) do
    L[i]:=MyControls.AddLabel(KMPanel_Main1,550,300+i*20,160,30,TKMFont(i),kaLeft,FontFiles[i]+' This is a test string for KaM Remake');
  //}

  //Show version info on every page
  KMLabel_Version:=MyControls.AddLabel(KMPanel_Main1,5,5,100,30,GAME_VERSION,fnt_Antiqua,kaLeft);

  SwitchMenuPage(nil);
end;


destructor TKMMainMenuInterface.Destroy;
begin
  FreeAndNil(SingleMapsInfo);
  FreeAndNil(MyControls);
  inherited;
end;


procedure TKMMainMenuInterface.SetScreenSize(X,Y:word);
begin
  ScreenX:=X;
  ScreenY:=Y;
end;

procedure TKMMainMenuInterface.ShowScreen_Loading();
begin
  SwitchMenuPage(KMPanel_Loading);
end;

procedure TKMMainMenuInterface.ShowScreen_Main();
begin
  SwitchMenuPage(nil);
end;


procedure TKMMainMenuInterface.ShowScreen_Results();
begin
  SwitchMenuPage(KMPanel_Results);
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  KMPanel_MainMenu:=MyControls.AddPanel(KMPanel_Main1,0,0,1024,768);
    KMImage_MainMenuBG:=MyControls.AddImage(KMPanel_MainMenu,0,0,ScreenX,ScreenY,2,6);
    KMImage_MainMenuBG.StretchImage:=true;
    KMImage_MainMenu1:=MyControls.AddImage(KMPanel_MainMenu,120,100,423,164,4,5);
    KMImage_MainMenu3:=MyControls.AddImage(KMPanel_MainMenu,660,220,round(207*1.25),round(295*1.25),6,6);
    KMImage_MainMenu3.StretchImage:=true;

    KMPanel_MainButtons:=MyControls.AddPanel(KMPanel_MainMenu,160,300,350,350);
      KMButton_MainMenuTutor  :=MyControls.AddButton(KMPanel_MainButtons,0,  0,350,30,fTextLibrary.GetSetupString( 3),fnt_Metal,bsMenu);
      KMButton_MainMenuTSK    :=MyControls.AddButton(KMPanel_MainButtons,0, 40,350,30,fTextLibrary.GetSetupString( 1),fnt_Metal,bsMenu);
      KMButton_MainMenuTPR    :=MyControls.AddButton(KMPanel_MainButtons,0, 80,350,30,fTextLibrary.GetSetupString( 2),fnt_Metal,bsMenu);
      KMButton_MainMenuSingle :=MyControls.AddButton(KMPanel_MainButtons,0,120,350,30,fTextLibrary.GetSetupString( 4),fnt_Metal,bsMenu);
      KMButton_MainMenuOptions:=MyControls.AddButton(KMPanel_MainButtons,0,160,350,30,fTextLibrary.GetSetupString(12),fnt_Metal,bsMenu);
      KMButton_MainMenuCredit :=MyControls.AddButton(KMPanel_MainButtons,0,200,350,30,fTextLibrary.GetSetupString(13),fnt_Metal,bsMenu);
      KMButton_MainMenuQuit   :=MyControls.AddButton(KMPanel_MainButtons,0,280,350,30,fTextLibrary.GetSetupString(14),fnt_Metal,bsMenu);
      KMButton_MainMenuTutor.OnClick :=MainMenu_PlayTutorial;
      KMButton_MainMenuSingle.OnClick:=SwitchMenuPage;
      KMButton_MainMenuOptions.OnClick:=SwitchMenuPage;
      KMButton_MainMenuCredit.OnClick:=SwitchMenuPage;
      KMButton_MainMenuQuit.OnClick  :=Form1.Exit1.OnClick;
      KMButton_MainMenuTSK.Disable;
      KMButton_MainMenuTPR.Disable;
      KMButton_MainMenuCredit.Disable;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  SingleMapsInfo:=TKMMapsInfo.Create;

  KMPanel_Single:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);

    KMImage_SingleBG:=MyControls.AddImage(KMPanel_Single,0,0,ScreenX,ScreenY,2,6);
    KMImage_SingleBG.StretchImage:=true;

    KMPanel_SingleList:=MyControls.AddPanel(KMPanel_Single,45,100,445,600);

      KMButton_SingleHeadMode :=MyControls.AddButton(KMPanel_SingleList,  0,0, 40,40,42,4,bsMenu);
      KMButton_SingleHeadTeams:=MyControls.AddButton(KMPanel_SingleList, 40,0, 40,40,31,4,bsMenu);
      KMButton_SingleHeadTitle:=MyControls.AddButton(KMPanel_SingleList, 80,0,300,40,'Title',fnt_Metal,bsMenu);
      KMButton_SingleHeadSize :=MyControls.AddButton(KMPanel_SingleList,380,0, 40,40,'Size',fnt_Metal,bsMenu);
      MyControls.AddButton(KMPanel_SingleList,420,0, 25,40,'',fnt_Game,bsMenu);
      for i:=1 to MENU_SINGLE_MAPS_COUNT do begin
        KMBevel_SingleBG[i,1]:=MyControls.AddBevel(KMPanel_SingleList,0,  40+(i-1)*40,40,40);
        KMBevel_SingleBG[i,2]:=MyControls.AddBevel(KMPanel_SingleList,40, 40+(i-1)*40,40,40);
        KMBevel_SingleBG[i,3]:=MyControls.AddBevel(KMPanel_SingleList,80, 40+(i-1)*40,300,40);
        KMBevel_SingleBG[i,4]:=MyControls.AddBevel(KMPanel_SingleList,380,40+(i-1)*40,40,40);
        KMBevel_SingleBG[i,3].Tag:=i;
        KMBevel_SingleBG[i,3].OnClick:=SingleMap_SelectMap;

        KMButton_SingleMode[i]   :=MyControls.AddImage(KMPanel_SingleList,  0   ,40+(i-1)*40,40,40,28+random(2)*14);
        KMButton_SinglePlayers[i]:=MyControls.AddLabel(KMPanel_SingleList, 40+20,40+(i-1)*40+14,40,40,'0',fnt_Metal, kaCenter);
        KMLabel_SingleTitle1[i]  :=MyControls.AddLabel(KMPanel_SingleList, 80+6 ,40+5+(i-1)*40,40,40,fTextLibrary.GetSetupString(random(29)),fnt_Metal, kaLeft);
        KMLabel_SingleTitle2[i]  :=MyControls.AddLabel(KMPanel_SingleList, 80+6 ,40+22+(i-1)*40,40,40,fTextLibrary.GetSetupString(random(29)),fnt_Game, kaLeft);
        KMButton_SingleSize[i]   :=MyControls.AddLabel(KMPanel_SingleList,380+20,40+(i-1)*40+14,40,40,'0',fnt_Metal, kaCenter);
      end;

      KMScrollBar_SingleMaps:=MyControls.AddScrollBar(KMPanel_SingleList,420,40,25,MENU_SINGLE_MAPS_COUNT*40,bsMenu);
      KMScrollBar_SingleMaps.OnChange:=SingleMap_ScrollChange;

      KMShape_SingleMap:=MyControls.AddShape(KMPanel_SingleList,0,40,420,40,$FFFFFF00);

    KMPanel_SingleDesc:=MyControls.AddPanel(KMPanel_Single,512+22,100,445,600);

      KMImage_SingleScroll1:=MyControls.AddImage(KMPanel_SingleDesc,0,0,445,200,15,5);
      KMImage_SingleScroll1.StretchImage:=true;
      KMImage_SingleScroll1.Height:=200; //Need to reset it after stretching is enabled, cos it can't stretch down by default

      KMLabel_SingleTitle:=MyControls.AddLabel(KMPanel_SingleDesc,445 div 2,35,420,180,'',fnt_Outline, kaCenter);
      KMLabel_SingleTitle.AutoWrap:=true;

      KMLabel_SingleDesc:=MyControls.AddLabel(KMPanel_SingleDesc,15,60,420,180,'',fnt_Metal, kaLeft);
      KMLabel_SingleDesc.AutoWrap:=true; 

      MyControls.AddBevel(KMPanel_SingleDesc,0,300,445,20);
      KMLabel_SingleCondWin:=MyControls.AddLabel(KMPanel_SingleDesc,8,304,445,20,'Win condition: ',fnt_Metal, kaLeft);
      MyControls.AddBevel(KMPanel_SingleDesc,0,322,445,20);
      KMLabel_SingleCondDef:=MyControls.AddLabel(KMPanel_SingleDesc,8,326,445,20,'Defeat condition: ',fnt_Metal, kaLeft);

      for i:=1 to 4 do begin
        KM_Button_SingleDiff[i]:=MyControls.AddButtonFlat(KMPanel_SingleDesc,0+(i-1)*60,390,60,80,31+i);
        KM_Button_SingleDiff[i].OnClick:=SingleMap_SelectDiff;
        KM_Button_SingleDiff[i].Tag:=i;
      end;
      KMShape_SingleDiff:=MyControls.AddShape(KMPanel_SingleDesc,0,390,60,80,$FFFFFF00);

      MyControls.AddButtonFlat(KMPanel_SingleDesc,240,390,205,80,0);
      MyControls.AddLabel(KMPanel_SingleDesc,340,402,40,40,'Difficulty:',fnt_Metal, kaCenter);
      KMLabel_SingleDiff:=MyControls.AddLabel(KMPanel_SingleDesc,340,422,40,40,'Medium',fnt_Game, kaCenter);

      KMButton_SingleBack:=MyControls.AddButton(KMPanel_SingleDesc,0,560,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
      KMButton_SingleBack.OnClick:=SwitchMenuPage;
      KMButton_SingleStart:=MyControls.AddButton(KMPanel_SingleDesc,225,560,220,30,fTextLibrary.GetSetupString(8),fnt_Metal,bsMenu);
      KMButton_SingleStart.OnClick:=SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Options_Page;
var i:integer;
begin
  KMPanel_Options:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    Image_Opts_BG:=MyControls.AddImage(KMPanel_Options,0,0,ScreenX,ScreenY,2,6);
    Image_Opts_BG.StretchImage:=true;

    Label_Opts_MouseSpeed:=MyControls.AddLabel(KMPanel_Options,124,130,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
    Label_Opts_MouseSpeed.Disable;
    Ratio_Opts_Mouse:=MyControls.AddRatioRow(KMPanel_Options,118,150,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    Ratio_Opts_Mouse.Disable;
    Label_Opts_SFX:=MyControls.AddLabel(KMPanel_Options,124,178,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
    Ratio_Opts_SFX:=MyControls.AddRatioRow(KMPanel_Options,118,198,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    Label_Opts_Music:=MyControls.AddLabel(KMPanel_Options,124,226,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
    Ratio_Opts_Music:=MyControls.AddRatioRow(KMPanel_Options,118,246,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);

    Ratio_Opts_Mouse.Position:=fGameSettings.GetMouseSpeed;
    Ratio_Opts_SFX.Position  :=fGameSettings.GetSoundFXVolume;
    Ratio_Opts_Music.Position:=fGameSettings.GetMusicVolume;

    for i:=1 to KMPanel_Options.ChildCount do
    if TKMControl(KMPanel_Options.Childs[i]) is TKMRatioRow then
    begin
      TKMControl(KMPanel_Options.Childs[i]).OnClick:=Options_Change;
      TKMControl(KMPanel_Options.Childs[i]).OnChange:=Options_Change;
    end;

    Button_Opts_Back:=MyControls.AddButton(KMPanel_Options,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_Opts_Back.OnClick:=SwitchMenuPage;
    //Should contain resolution selector, fullscreen option and other things
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
begin
  KMPanel_Credits:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_CreditsBG:=MyControls.AddImage(KMPanel_Credits,0,0,ScreenX,ScreenY,2,5);
    KMImage_CreditsBG.StretchImage:=true;
    KMButton_CreditsBack:=MyControls.AddButton(KMPanel_Credits,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal);
    KMButton_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  KMPanel_Loading:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_LoadingBG:=MyControls.AddImage(KMPanel_Loading,0,0,ScreenX,ScreenY,2,5);
    KMImage_LoadingBG.StretchImage:=true;
    KMLabel_Loading:=MyControls.AddLabel(KMPanel_Loading,ScreenX div 2,ScreenY div 2,100,30,'Loading... Please wait',fnt_Grey,kaCenter);
end;


procedure TKMMainMenuInterface.Create_Results_Page;
begin
  KMPanel_Results:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_ResultsBG:=MyControls.AddImage(KMPanel_Results,0,0,ScreenX,ScreenY,7,5);
    KMImage_ResultsBG.StretchImage:=true;
    KMButton_ResultsBack:=MyControls.AddButton(KMPanel_Results,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal);
    KMButton_ResultsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var i:integer;
begin
  //First thing - hide all existing pages
  for i:=1 to KMPanel_Main1.ChildCount do
    if KMPanel_Main1.Childs[i] is TKMPanel then
      KMPanel_Main1.Childs[i].Hide;

  {Return to MainMenu if Sender unspecified}
  if Sender=nil then KMPanel_MainMenu.Show;

  {Return to MainMenu}
  if (Sender=KMButton_CreditsBack)or
     (Sender=KMButton_SingleBack)or
     (Sender=Button_Opts_Back)or
     (Sender=KMButton_ResultsBack) then
    KMPanel_MainMenu.Show;
                          
  {Show SingleMap menu}
  if Sender=KMButton_MainMenuSingle then begin
    SingleMap_RefreshList();
    KMPanel_Single.Show;
  end;

  {Show Options menu}
  if Sender=KMButton_MainMenuOptions then begin
    KMPanel_Options.Show;
  end;

  {Show Credits}
  if Sender=KMButton_MainMenuCredit then
    KMPanel_Credits.Show;

  {Show Loading... screen}
  if Sender=KMPanel_Loading then
    KMPanel_Loading.Show;

  {Show Results screen}
  if Sender=KMPanel_Results then //This page can be accessed only by itself
    KMPanel_Results.Show;
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList();
var i,ci:integer;
begin
  SingleMapsInfo.ScanSingleMapsFolder('');

  for i:=1 to MENU_SINGLE_MAPS_COUNT do begin
    ci:=SingleMap_Top-1+i;
    if ci>SingleMapsInfo.GetMapCount then begin
      KMButton_SingleMode[i].TexID:=0;
      KMButton_SinglePlayers[i].Caption:='';
      KMLabel_SingleTitle1[i].Caption:='';
      KMLabel_SingleTitle2[i].Caption:='';
      KMButton_SingleSize[i].Caption:='';
    end else begin
      KMButton_SingleMode[i].TexID:=28+byte(not SingleMapsInfo.IsFight(i))*14;
      KMButton_SinglePlayers[i].Caption:=inttostr(SingleMapsInfo.GetPlayerCount(i));
      KMLabel_SingleTitle1[i].Caption:=SingleMapsInfo.GetTitle(i);
      KMLabel_SingleTitle2[i].Caption:=SingleMapsInfo.GetSmallDesc(i);
      KMButton_SingleSize[i].Caption:=SingleMapsInfo.GetMapSize(i);
    end;
  end;

  KMScrollBar_SingleMaps.MinValue:=1;
  KMScrollBar_SingleMaps.MaxValue:=max(1,SingleMapsInfo.GetMapCount-SingleMapsInfo.GetMapCount);
  KMScrollBar_SingleMaps.Position:=EnsureRange(KMScrollBar_SingleMaps.Position,KMScrollBar_SingleMaps.MinValue,KMScrollBar_SingleMaps.MaxValue);

  SingleMap_SelectMap(KMBevel_SingleBG[1,3]); //Select first map
end;


procedure TKMMainMenuInterface.SingleMap_ScrollChange(Sender: TObject);
begin
  SingleMap_Top:=KMScrollBar_SingleMaps.Position;
  SingleMap_RefreshList();
end;


procedure TKMMainMenuInterface.SingleMap_SelectMap(Sender: TObject);
var i:integer;
begin

  i:=TKMControl(Sender).Tag;

  KMShape_SingleMap.Top:=KMBevel_SingleBG[1,3].Top+KMBevel_SingleBG[i,3].Height*(i-1);

  SingleMap_Selected:=SingleMap_Top+i-1;
  KMLabel_SingleTitle.Caption:=SingleMapsInfo.GetTitle(SingleMap_Selected);
  KMLabel_SingleDesc.Caption:=SingleMapsInfo.GetBigDesc(SingleMap_Selected);

  KMLabel_SingleCondWin.Caption:='Win condition: '+SingleMapsInfo.GetWin(SingleMap_Selected);
  KMLabel_SingleCondDef.Caption:='Defeat condition: '+SingleMapsInfo.GetDefeat(SingleMap_Selected);

  SingleMap_SelectDiff(KM_Button_SingleDiff[2]);
end;


{ Map Difficulty tweak}
//@Lewin: We will stitch it later
procedure TKMMainMenuInterface.SingleMap_SelectDiff(Sender: TObject);
begin
  SingleMap_Difficulty:=TKMControl(Sender).Tag;
  KMShape_SingleDiff.Left:=TKMControl(Sender).Left;
  KMLabel_SingleDiff.Caption:='Difficulty of '+inttostr(TKMControl(Sender).Tag);
  //@Lewin: Dummy, Easy, Medium, Hard
  //@Krom: I dislike the idea that people can set the difficulty. What would it change anyway?
  //       People don't want to have to specify that kind of stuff in the mission file, it would be to complicated.
  //       I think each mission should have an "estimated difficulty" that the creator sets in the TXT file, so that
  //       people have an idea of how hard the mission will be.
  //@Lewin: You right again, but my idea was to allow some kind of cheat, e.g. Easy makes your troops x1.5 stronger
  //       there will be no need for map author to foresee it, he just makes it on Normal.
  //       Anyway thats too far ahead, so we will think about exact plan later on, for now it's just a placeholder
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
var MissionPath:string;
begin
  Assert(Sender=KMButton_SingleStart);
  if not InRange(SingleMap_Selected,1,SingleMapsInfo.GetMapCount) then exit;
  MissionPath:=ExeDir+'Maps\'+SingleMapsInfo.GetFolder(SingleMap_Selected)+'\'+SingleMapsInfo.GetMissionFile(SingleMap_Selected);
  fGame.StartGame(MissionPath); //Provide mission filename here
end;

procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
begin
  if Sender = Ratio_Opts_Mouse then fGameSettings.SetMouseSpeed(Ratio_Opts_Mouse.Position);
  if Sender = Ratio_Opts_SFX   then fGameSettings.SetSoundFXVolume(Ratio_Opts_SFX.Position);
  if Sender = Ratio_Opts_Music then fGameSettings.SetMusicVolume(Ratio_Opts_Music.Position);
end;

procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  Assert(Sender=KMButton_MainMenuTutor);
  fGame.StartGame(ExeDir+'data\mission\dmission0.dat'); //Provide mission filename here
end;


procedure TKMMainMenuInterface.Paint;
begin
  MyControls.Paint;
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var i:integer; LastVisiblePage: TKMPanel;

  procedure Flip4MainButtons(ShowEm:boolean);
  var i:integer;
  begin
    for i:=1 to 4 do KMButtonMain[i].Visible:=ShowEm;
    KMButtonMain[5].Visible:=not ShowEm;
    KMLabel_MenuTitle.Visible:=not ShowEm;
  end;

begin

  if (Sender=KMButtonMain[1])or(Sender=KMButtonMain[2])or
     (Sender=KMButtonMain[3])or(Sender=KMButtonMain[4])or
     (Sender=KMButton_Menu_Settings)or(Sender=KMButton_Menu_Quit) then begin
    ShownHouse:=nil;
    ShownUnit:=nil;
  end;

  //Reset the CursorMode, to cm_None
  Build_ButtonClick(nil);

  //Set LastVisiblePage to which ever page was last visible, out of the ones needed
  if KMPanel_Settings.Visible then LastVisiblePage := KMPanel_Settings else
  if KMPanel_Save.Visible     then LastVisiblePage := KMPanel_Save     else
  if KMPanel_Load.Visible     then LastVisiblePage := KMPanel_Load     else
    LastVisiblePage := nil;

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
    KMPanel_Menu.Show;
    KMLabel_MenuTitle.Caption:=fTextLibrary.GetTextString(170);
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
var RandomNum: string;
begin
  RandomNum := '    '+inttostr(random(9)); //Random numbers here are to show every time hint gets refreshed
  ShownHint:=Sender;
  if((ShownHint<>nil) and ((not TKMControl(ShownHint).CursorOver) or (not TKMControl(ShownHint).Visible)) ) then ShownHint:=nil; //only set if cursor is over and control is visible
  if ((ShownHint<>nil) and (TKMControl(ShownHint).Parent <> nil)) then //only set if parent is visible (e.g. panel)
    if (ShownHint<>nil)and(not (ShownHint as TKMControl).Parent.Visible) then ShownHint:=nil;

  KMLabel_Hint.Top:=fRender.GetRenderAreaSize.Y-16;
  //If hint hasn't changed then don't refresh it
  if ((ShownHint<>nil) and (LeftStr(KMLabel_Hint.Caption,Length(KMLabel_Hint.Caption)-Length(RandomNum)) = TKMControl(Sender).Hint)) then exit;
  if ((ShownHint=nil) and (Length(KMLabel_Hint.Caption) = Length(RandomNum))) then exit;
  if ShownHint=nil then KMLabel_Hint.Caption:=''+RandomNum else
    KMLabel_Hint.Caption:=(Sender as TKMControl).Hint+RandomNum;
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
  KMMinimap.ViewArea:=fViewport.GetClip;
end;


constructor TKMGamePlayInterface.Create();
var i:integer;
begin
inherited;
Assert(fGameSettings<>nil,'fGameSettings required to be init first');
Assert(fViewport<>nil,'fViewport required to be init first');

  MyControls:=TKMControlsCollection.Create;

  ShownUnit:=nil;
  ShownHouse:=nil;

  LastSchoolUnit:=1;
  LastBarrackUnit:=1;
{Parent Page for whole toolbar in-game}
  KMPanel_Main:=MyControls.AddPanel(nil,0,0,224,768);

    KMImage_Main1:=MyControls.AddImage(KMPanel_Main,0,0,224,200,407);
    KMImage_Main3:=MyControls.AddImage(KMPanel_Main,0,200,224,168,554);
    KMImage_Main4:=MyControls.AddImage(KMPanel_Main,0,368,224,400,404);

    KMMinimap:=MyControls.AddMinimap(KMPanel_Main,10,10,176,176);
    KMMinimap.OnChange:=Minimap_Update;

    Button_UnPause:=MyControls.AddButton(KMPanel_Main,  54, 336, 42, 30,'UnPause >>',fnt_Metal);
    Button_UnPause.OnClick:=UnPauseGame;

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

    KMLabel_Stat:=MyControls.AddLabel(KMPanel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    KMLabel_Hint:=MyControls.AddLabel(KMPanel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,'',fnt_Outline,kaLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
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

  SetHintEvents(DisplayHint); //Set all OnHint events to be the correct function

  SwitchPage(nil); //Update
end;


destructor TKMGamePlayInterface.Destroy;
begin
  FreeAndNil(MyControls);
  inherited;
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
    KMButton_BuildCancel := MyControls.AddButtonFlat(KMPanel_Build,156,80,33,33,340);
    KMButton_BuildRoad.OnClick:=Build_ButtonClick;
    KMButton_BuildField.OnClick:=Build_ButtonClick;
    KMButton_BuildWine.OnClick:=Build_ButtonClick;
    KMButton_BuildCancel.OnClick:=Build_ButtonClick;
    KMButton_BuildRoad.Hint:=fTextLibrary.GetTextString(213);
    KMButton_BuildField.Hint:=fTextLibrary.GetTextString(215);
    KMButton_BuildWine.Hint:=fTextLibrary.GetTextString(219);
    KMButton_BuildCancel.Hint:=fTextLibrary.GetTextString(211);

    for i:=1 to HOUSE_COUNT do begin
      KMButton_Build[i]:=MyControls.AddButtonFlat(KMPanel_Build, 8+((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,
      GUIBuildIcons[byte(GUIHouseOrder[i])]);

      KMButton_Build[i].OnClick:=Build_ButtonClick;
      KMButton_Build[i].Hint:=fTextLibrary.GetTextString(GUIBuildIcons[byte(GUIHouseOrder[i])]-300);
    end;
end;


{Ratios page}
procedure TKMGamePlayInterface.Create_Ratios_Page;
begin
  KMPanel_Ratios:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
end;


{Statistics page}
procedure TKMGamePlayInterface.Create_Stats_Page;
var i,k,ci:integer;
begin
  KMPanel_Stats:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
  ci:=0;
  for i:=1 to 11 do for k:=1 to 4 do
  if StatHouseOrder[i,k]<>ht_None then begin
    inc(ci);
    Stat_House[ci]:=MyControls.AddButtonFlat(KMPanel_Stats,8+(k-1)*42,(i-1)*32,40,30,41);
    Stat_House[ci].TexOffsetX:=-4;
    Stat_House[ci].HideHighlight:=true;
    Stat_House[ci].Hint:=TypeToString(StatHouseOrder[i,k]);
    Stat_HouseQty[ci]:=MyControls.AddLabel(KMPanel_Stats,8+37+(k-1)*42,(i-1)*32+18,33,30,'',fnt_Grey,kaRight);
    Stat_HouseQty[ci].Hint:=TypeToString(StatHouseOrder[i,k]);
  end;
  ci:=0;
  for i:=1 to 11 do for k:=1 to 5 do
  if StatUnitOrder[i,k]<>ut_None then begin     
    inc(ci);
    Stat_Unit[ci]:=MyControls.AddButtonFlat(KMPanel_Stats,8+(k-1)*36,(i-1)*32,35,30,byte(StatUnitOrder[i,k])+140);
    Stat_Unit[ci].TexOffsetX:=-4;
    Stat_Unit[ci].HideHighlight:=true;
    Stat_Unit[ci].Hint:=TypeToString(StatUnitOrder[i,k]);
    Stat_UnitQty[ci]:=MyControls.AddLabel(KMPanel_Stats,8+32+(k-1)*36,(i-1)*32+18,33,30,'',fnt_Grey,kaRight);
    Stat_UnitQty[ci].Hint:=TypeToString(StatUnitOrder[i,k]);
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
    KMButton_Menu_Settings:=MyControls.AddButton(KMPanel_Menu,8,100,180,30,fTextLibrary.GetTextString(179),fnt_Metal);
    KMButton_Menu_Settings.OnClick:=Menu_ShowSettings;
    KMButton_Menu_Settings.Hint:=fTextLibrary.GetTextString(179);
    KMButton_Menu_Quit:=MyControls.AddButton(KMPanel_Menu,8,180,180,30,fTextLibrary.GetTextString(180),fnt_Metal);
    KMButton_Menu_Quit.Hint:=fTextLibrary.GetTextString(180);
    KMButton_Menu_Quit.OnClick:=SwitchPage;
    KMButton_Menu_Track:=MyControls.AddButton(KMPanel_Menu,158,320,30,30,'>',fnt_Metal);
    KMButton_Menu_Track.Hint:=fTextLibrary.GetTextString(209);
    //KMButton_Menu_Quit.OnClick:=TrackUp;
    KMLabel_Menu_Music:=MyControls.AddLabel(KMPanel_Menu,100,298,100,30,fTextLibrary.GetTextString(207),fnt_Metal,kaCenter);
    KMLabel_Menu_Track:=MyControls.AddLabel(KMPanel_Menu,100,326,100,30,'Spirit',fnt_Grey,kaCenter);
end;


{Save page}
procedure TKMGamePlayInterface.Create_Save_Page;
var i:integer;
begin
  KMPanel_Save:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    for i:=1 to SAVEGAME_COUNT do begin
      KMButton_Save[i]:=MyControls.AddButton(KMPanel_Save,12,10+(i-1)*26,170,24,'Savegame #'+inttostr(i),fnt_Grey);
      //KMButton_Save[i].OnClick:=SaveGame;
      KMButton_Save[i].Disable;
    end;
end;


{Load page}
procedure TKMGamePlayInterface.Create_Load_Page;
var i:integer;
begin
  KMPanel_Load:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    for i:=1 to SAVEGAME_COUNT do begin
      KMButton_Load[i]:=MyControls.AddButton(KMPanel_Load,12,10+(i-1)*26,170,24,'Savegame #'+inttostr(i),fnt_Grey);
      //KMButton_Load[i].OnClick:=LoadGame;
      KMButton_Load[i].Disable;
    end;
end;


{Options page}
procedure TKMGamePlayInterface.Create_Settings_Page;
var i:integer;
begin
  KMPanel_Settings:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    KMLabel_Settings_Brightness:=MyControls.AddLabel(KMPanel_Settings,100,10,100,30,fTextLibrary.GetTextString(181),fnt_Metal,kaCenter);
    KMButton_Settings_Dark:=MyControls.AddButton(KMPanel_Settings,8,30,36,24,fTextLibrary.GetTextString(183),fnt_Metal);
    KMButton_Settings_Light:=MyControls.AddButton(KMPanel_Settings,154,30,36,24,fTextLibrary.GetTextString(182),fnt_Metal);
    KMButton_Settings_Dark.Hint:=fTextLibrary.GetTextString(185);
    KMButton_Settings_Light.Hint:=fTextLibrary.GetTextString(184);
    KMLabel_Settings_BrightValue:=MyControls.AddLabel(KMPanel_Settings,100,34,100,30,'',fnt_Grey,kaCenter);
    KMLabel_Settings_Autosave:=MyControls.AddLabel(KMPanel_Settings,8,70,100,30,'',fnt_Metal,kaLeft);
    KMLabel_Settings_Autosave.Disable;
    KMLabel_Settings_FastScroll:=MyControls.AddLabel(KMPanel_Settings,8,95,100,30,'',fnt_Metal,kaLeft);
    KMLabel_Settings_MouseSpeed:=MyControls.AddLabel(KMPanel_Settings,24,130,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
    KMLabel_Settings_MouseSpeed.Disable;
    KMRatio_Settings_Mouse:=MyControls.AddRatioRow(KMPanel_Settings,18,150,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    KMRatio_Settings_Mouse.Disable;
    KMRatio_Settings_Mouse.Hint:=fTextLibrary.GetTextString(193);
    KMLabel_Settings_SFX:=MyControls.AddLabel(KMPanel_Settings,24,178,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
    KMRatio_Settings_SFX:=MyControls.AddRatioRow(KMPanel_Settings,18,198,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    KMRatio_Settings_SFX.Hint:=fTextLibrary.GetTextString(195);
    KMLabel_Settings_Music:=MyControls.AddLabel(KMPanel_Settings,24,226,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
    KMRatio_Settings_Music:=MyControls.AddRatioRow(KMPanel_Settings,18,246,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
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
    KMLabel_Quit:=MyControls.AddLabel(KMPanel_Quit,100,30,100,30,fTextLibrary.GetTextString(176),fnt_Outline,kaCenter);
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
    KMLabel_UnitName:=MyControls.AddLabel(KMPanel_Unit,100,30,100,30,'',fnt_Outline,kaCenter);
    KMLabel_UnitCondition:=MyControls.AddLabel(KMPanel_Unit,130,54,100,30,fTextLibrary.GetTextString(254),fnt_Grey,kaCenter);
    KMLabel_UnitTask:=MyControls.AddLabel(KMPanel_Unit,73,89,130,30,'',fnt_Grey,kaLeft);
    KMLabel_UnitAct:=MyControls.AddLabel(KMPanel_Unit,73,109,130,30,'',fnt_Grey,kaLeft);
    KMLabel_UnitAct.AutoWrap:=true;
    KMConditionBar_Unit:=MyControls.AddPercentBar(KMPanel_Unit,73,69,116,15,80);
    KMLabel_UnitDescription:=MyControls.AddLabel(KMPanel_Unit,8,161,236,200,'',fnt_Grey,kaLeft); //Taken from LIB resource
    KMImage_UnitPic:=MyControls.AddImage(KMPanel_Unit,8,52,54,80,521);
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
        KMButton_Barracks[i]:=MyControls.AddButtonFlat(KMPanel_HouseBarracks, 8+((i-1)mod 6)*31,19+((i-1)div 6)*42,28,36,366+i);
        KMButton_Barracks[i].HideHighlight:=true;
        KMButton_Barracks[i].Hint:=TypeToString(TResourceType(16+i));
      end;
      KMButton_Barracks[12].TexID:=154;
      KMButton_Barracks[12].Hint:=TypeToString(ut_Recruit);
      {KMLabel_School_Unit:=MyControls.AddLabel(KMPanel_House_School,100,116,100,30,'',fnt_Outline,kaCenter);
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
      KMButton_School_Right.Hint:=fTextLibrary.GetTextString(241);}
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMGamePlayInterface.UpdateState;
begin
  if ShownUnit<>nil then ShowUnitInfo(ShownUnit) else
  if ShownHouse<>nil then ShowHouseInfo(ShownHouse);

  if ShownHint<>nil then DisplayHint(ShownHint,[],0,0);
  if Mouse.CursorPos.X>ToolBarWidth then DisplayHint(nil,[],0,0); //Don't display hints if not over ToolBar

  Minimap_Update(nil);

  if KMPanel_Build.Visible then Build_Fill(nil);
  if KMPanel_Stats.Visible then Stats_Fill(nil);

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

  for i:=1 to HOUSE_COUNT do
  if KMButton_Build[i].Down then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Param:=byte(GUIHouseOrder[i]);
     KMImage_Build_Selected.TexID := GUIBuildIcons[byte(GUIHouseOrder[i])]; //Now update the selected icon
     KMLabel_BuildCost_Wood.Caption:=inttostr(HouseDAT[byte(GUIHouseOrder[i])].WoodCost);
     KMLabel_BuildCost_Stone.Caption:=inttostr(HouseDAT[byte(GUIHouseOrder[i])].StoneCost);
     KMLabel_Build.Caption := TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end;
end;


procedure TKMGamePlayInterface.ShowHouseInfo(Sender:TKMHouse);
const LineAdv = 25; //Each new Line is placed ## pixels after previous
var i,RowRes,Base,Line:integer;
begin
  ShownUnit:=nil;
  ShownHouse:=Sender;

  if Sender=nil then begin
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
  end else begin
  for i:=1 to KMPanel_House.ChildCount do
    KMPanel_House.Childs[i].Show; //show all
  KMImage_House_Worker.Enabled := Sender.GetHasOwner;
  KMImage_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  if (HouseInput[byte(Sender.GetHouseType)][1] in [rt_None,rt_All,rt_Warfare]) then
    KMButton_House_Goods.Enabled:=false else KMButton_House_Goods.Enable;
  if Sender.BuildingRepair then KMButton_House_Repair.TexID:=39 else KMButton_House_Repair.TexID:=40;
  if Sender.WareDelivery then KMButton_House_Goods.TexID:=37 else KMButton_House_Goods.TexID:=38;
  KMLabel_House_UnderConstruction.Visible := false;
  SwitchPage(KMPanel_House);

  case Sender.GetHouseType of
  ht_Store: begin
        Store_Fill(nil);
        SwitchPage(KMPanel_HouseStore);
      end;

  ht_School: begin
        KMResRow_School_Resource.ResourceCount:=Sender.CheckResIn(rt_Gold);
        House_SchoolUnitChange(nil);
        KMButton_School_UnitWIPBar.Position:=TKMHouseSchool(Sender).GetTrainingProgress;
        SwitchPage(KMPanel_House_School);
      end;

  ht_Barracks: begin
        KMImage_House_Worker.Enabled := true; //In the barrack the recruit icon is always enabled
        Barracks_Fill(nil);
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
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender:TKMUnit);
begin
  ShownUnit:=Sender;
  ShownHouse:=nil;
  if (Sender=nil)or(not Sender.IsVisible)or((Sender<>nil)and(Sender.IsDestroyed)) then begin
    SwitchPage(nil);
    exit;
  end;
  SwitchPage(KMPanel_Unit);
  KMLabel_UnitName.Caption:=TypeToString(Sender.GetUnitType);
  KMImage_UnitPic.TexID:=520+byte(Sender.GetUnitType);
  KMConditionBar_Unit.Position:=EnsureRange(round(Sender.GetCondition / UNIT_MAX_CONDITION * 100),-10,110);
  //@Krom: No string in LIB files availible.
  //If this is perminate (not just debugging) then we will need to add it.
  //Prehaps we should start a list of new texts added which will need translating?
  //@Lewin: This is for debug atm, but I think this could be a nice new feature.
  //Yes, can you make a sort of extension to LoadLIB? > See LoadLib for my comments.
  KMLabel_UnitTask.Caption:='Task: '+Sender.GetUnitTaskText;
  KMLabel_UnitAct.Caption:='Act: '+Sender.GetUnitActText;
  KMLabel_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender.GetUnitType))
end;


procedure TKMGamePlayInterface.House_RepairToggle(Sender:TObject);
begin
  if fPlayers.SelectedHouse = nil then exit;
  with fPlayers.SelectedHouse do begin
    BuildingRepair := not BuildingRepair;
    if BuildingRepair then KMButton_House_Repair.TexID:=39
                      else KMButton_House_Repair.TexID:=40;
    if BuildingRepair then EnableRepair
                      else DisableRepair;
  end;
end;


procedure TKMGamePlayInterface.House_WareDeliveryToggle(Sender:TObject);
begin
  if fPlayers.SelectedHouse = nil then exit;
  with fPlayers.SelectedHouse do begin
    WareDelivery := not WareDelivery;
    if WareDelivery then KMButton_House_Goods.TexID:=37
                    else KMButton_House_Goods.TexID:=38;
    end;
end;


procedure TKMGamePlayInterface.House_OrderClick(Sender:TObject);
var i:integer;
begin
  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then fPlayers.SelectedHouse.ResRemOrder(i);
    if Sender = KMRow_Order[i].OrderAdd then fPlayers.SelectedHouse.ResAddOrder(i);
  end;
end;


procedure TKMGamePlayInterface.House_OrderClickRight(Sender:TObject);
var i:integer;
begin
  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then fPlayers.SelectedHouse.ResRemOrder(i,10);
    if Sender = KMRow_Order[i].OrderAdd then fPlayers.SelectedHouse.ResAddOrder(i,10);
  end;
end;


procedure TKMGamePlayInterface.House_BarracksUnitChange(Sender:TObject);
begin
//
end;


procedure TKMGamePlayInterface.House_BarracksUnitChangeRight(Sender:TObject);
begin
//
end;


{Process click on Left-Train-Right buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitChange(Sender:TObject);
var i:byte; School:TKMHouseSchool;
begin
  School:=TKMHouseSchool(fPlayers.SelectedHouse);

  if (Sender=KMButton_School_Left)and(LastSchoolUnit > 1) then dec(LastSchoolUnit);
  if (Sender=KMButton_School_Right)and(LastSchoolUnit < length(School_Order)) then inc(LastSchoolUnit);

  if Sender=KMButton_School_Train then //Add unit to training queue
  begin
    fSoundLib.Play(sfx_click,KMPoint(0,0),false);
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
    TKMHouseSchool(fPlayers.SelectedHouse).RemUnitFromQueue(1)
  else for i:=1 to 5 do
    if Sender = KMButton_School_UnitPlan[i] then
    begin
      fSoundLib.Play(sfx_click,KMPoint(0,0),false);
      TKMHouseSchool(fPlayers.SelectedHouse).RemUnitFromQueue(i+1);
    end;
  House_SchoolUnitChange(nil);
end;


{That small red triangle blocking delivery of goods to Storehouse}
{Resource determined by Button.Tag property}
procedure TKMGamePlayInterface.House_StoreAcceptFlag(Sender:TObject);
begin
  TKMHouseStore(fPlayers.SelectedHouse).NotAcceptFlag[(Sender as TKMControl).Tag]:=
    not TKMHouseStore(fPlayers.SelectedHouse).NotAcceptFlag[(Sender as TKMControl).Tag];
end;


procedure TKMGamePlayInterface.Menu_ShowSettings(Sender: TObject);
begin
  SwitchPage(Sender);
  Menu_Settings_Change(nil);
end;


procedure TKMGamePlayInterface.Menu_Settings_Change(Sender:TObject);
begin
  if Sender = KMButton_Settings_Dark then fGameSettings.DecBrightness;
  if Sender = KMButton_Settings_Light then fGameSettings.IncBrightness;
  if Sender = KMLabel_Settings_Autosave then fGameSettings.IsAutosave:=not fGameSettings.IsAutosave;
  if Sender = KMLabel_Settings_FastScroll then fGameSettings.IsFastScroll:=not fGameSettings.IsFastScroll;
  if Sender = KMRatio_Settings_Mouse then fGameSettings.SetMouseSpeed(KMRatio_Settings_Mouse.Position);
  if Sender = KMRatio_Settings_SFX then fGameSettings.SetSoundFXVolume(KMRatio_Settings_SFX.Position);
  if Sender = KMRatio_Settings_Music then fGameSettings.SetMusicVolume(KMRatio_Settings_Music.Position);
  if Sender = KMButton_Settings_Music then fGameSettings.IsMusic:=not fGameSettings.IsMusic;
  KMLabel_Settings_BrightValue.Caption:=fTextLibrary.GetTextString(185 + fGameSettings.GetBrightness);
  if fGameSettings.IsAutosave then
  KMLabel_Settings_Autosave.Caption:='X '+fTextLibrary.GetTextString(203)
  else
  KMLabel_Settings_Autosave.Caption:='O '+fTextLibrary.GetTextString(203);
  if fGameSettings.IsFastScroll then
  KMLabel_Settings_FastScroll.Caption:='X '+fTextLibrary.GetTextString(204)
  else
  KMLabel_Settings_FastScroll.Caption:='O '+fTextLibrary.GetTextString(204);
  KMRatio_Settings_Mouse.Position:=fGameSettings.GetMouseSpeed;
  KMRatio_Settings_SFX.Position:=fGameSettings.GetSoundFXVolume;
  KMRatio_Settings_Music.Position:=fGameSettings.GetMusicVolume;
  if fGameSettings.IsMusic then
  KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(201) else KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(199);
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

  fGame.StopGame();
end;


procedure TKMGamePlayInterface.Build_Fill(Sender:TObject);
var i:integer;
begin
  for i:=1 to HOUSE_COUNT do
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


procedure TKMGamePlayInterface.Store_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.SelectedHouse=nil then exit;
  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(fPlayers.SelectedHouse).ResourceCount[i];
    if Tmp=0 then KMButton_Store[i].Caption:='-' else
    //if Tmp>999 then KMButton_Store[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  KMButton_Store[i].Caption:=inttostr(Tmp);
    KMImage_Store_Accept[i].Visible := TKMHouseStore(fPlayers.SelectedHouse).NotAcceptFlag[i];
  end;
end;


procedure TKMGamePlayInterface.Barracks_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  for i:=1 to 11 do begin
    Tmp:=TKMHouseBarracks(fPlayers.SelectedHouse).ResourceCount[i];
    if Tmp=0 then KMButton_Barracks[i].Caption:='-'
             else KMButton_Barracks[i].Caption:=inttostr(Tmp);
  end;
    Tmp:=TKMHouseBarracks(fPlayers.SelectedHouse).RecruitsInside;
    if Tmp=0 then KMButton_Barracks[12].Caption:='-'
             else KMButton_Barracks[12].Caption:=inttostr(Tmp);
end;


procedure TKMGamePlayInterface.Stats_Fill(Sender:TObject);
var i,k,ci,Tmp:integer;
begin
  ci:=0;
  for i:=1 to 11 do for k:=1 to 4 do
  if StatHouseOrder[i,k]<>ht_None then begin
    inc(ci);
    Tmp:=MyPlayer.GetHouseQty(StatHouseOrder[i,k]);
    if Tmp=0 then Stat_HouseQty[ci].Caption:='-' else Stat_HouseQty[ci].Caption:=inttostr(Tmp);
    if MyPlayer.GetCanBuild(StatHouseOrder[i,k]) or (Tmp>0) then
    begin
      Stat_House[ci].TexID:=byte(StatHouseOrder[i,k])+300;
      Stat_House[ci].Hint:=TypeToString(StatHouseOrder[i,k]);
      Stat_HouseQty[ci].Hint:=TypeToString(StatHouseOrder[i,k]);
    end
    else
    begin
      Stat_House[ci].TexID:=41;
      Stat_House[ci].Hint:=fTextLibrary.GetTextString(251); //Building not available
      Stat_HouseQty[ci].Hint:=fTextLibrary.GetTextString(251); //Building not available
    end;
  end;
  ci:=0;
  for i:=1 to 11 do for k:=1 to 5 do
  if StatUnitOrder[i,k]<>ut_None then begin
    inc(ci);
    Tmp:=MyPlayer.GetUnitQty(StatUnitOrder[i,k]);
    if Tmp=0 then Stat_UnitQty[ci].Caption:='-' else Stat_UnitQty[ci].Caption:=inttostr(Tmp);
    Stat_Unit[ci].Hint:=TypeToString(StatUnitOrder[i,k]);
    Stat_UnitQty[ci].Hint:=TypeToString(StatUnitOrder[i,k]);
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


procedure TKMGamePlayInterface.UnPauseGame(Sender:TObject);
begin
  fGame.PauseGame(false);
end;


procedure TKMGamePlayInterface.Paint;
begin
  MyControls.Paint;
end;


end.
