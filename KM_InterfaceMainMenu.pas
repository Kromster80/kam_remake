unit KM_InterfaceMainMenu;
interface
uses SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls, StrUtils, OpenGL,
  KM_Controls, KM_Defaults, KM_LoadDAT, Windows;


type TKMMainMenuInterface = class
  private
    ScreenX,ScreenY:word;
    OffX,OffY:integer;
    SingleMap_Top:integer; //Top map in list
    SingleMap_Selected:integer; //Selected map
    SingleMapsInfo:TKMMapsInfo;
  protected
    KMPanel_Main1:TKMPanel;
      L:array[1..20]of TKMLabel;
    KMPanel_MainMenu:TKMPanel;
      KMPanel_MainButtons:TKMPanel;
      KMImage_MainMenuBG,KMImage_MainMenu1,KMImage_MainMenu3:TKMImage; //Menu background
      KMButton_MainMenuTutor,KMButton_MainMenuTSK,KMButton_MainMenuTPR,
      KMButton_MainMenuSingle,KMButton_MainMenuLoad,KMButton_MainMenuMulti,
      KMButton_MainMenuOptions,KMButton_MainMenuCredit,KMButton_MainMenuQuit:TKMButton;
      KMLabel_Version:TKMLabel;
    KMPanel_Single:TKMPanel;
      KMImage_SingleBG:TKMImage;
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
      KMLabel_SingleCondTyp,KMLabel_SingleCondWin,KMLabel_SingleCondDef:TKMLabel;
      KMLabel_SingleAllies,KMLabel_SingleEnemies:TKMLabel;
      KMButton_SingleBack,KMButton_SingleStart:TKMButton;
    KMPanel_Options:TKMPanel;
      Image_Options_BG:TKMImage;
      Label_Options_MouseSpeed,Label_Options_SFX,Label_Options_Music,Label_Options_MusicOn:TKMLabel;
      Ratio_Options_Mouse,Ratio_Options_SFX,Ratio_Options_Music:TKMRatioRow;
      Button_Options_MusicOn,Button_Options_Back:TKMButton;
      CheckBox_Options_FullScreen:TKMCheckBox;
      KMBevel_Options_Lang_Background:TKMBevel;
      KMLabel_Options_Lang:TKMLabel;
      CheckBox_Options_Lang:array[1..LocalesCount] of TKMCheckBox;
    KMPanel_Credits:TKMPanel;
      KMImage_CreditsBG:TKMImage;
      KMLabel_Credits:TKMLabel;
      KMButton_CreditsBack:TKMButton;
    KMPanel_Loading:TKMPanel;
      KMImage_LoadingBG:TKMImage;
      KMLabel_Loading:TKMLabel;
    KMPanel_Results:TKMPanel;
      KMImage_ResultsBG:TKMImage;
      Label_Results_Result:TKMLabel;
      KMPanel_Stats:TKMPanel;
      Label_Stat:array[1..9]of TKMLabel;
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
    procedure SingleMap_Start(Sender: TObject);
    procedure Options_Change(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create(X,Y:word);
    destructor Destroy; override;
    procedure SetScreenSize(X,Y:word);
    procedure ShowScreen_Loading(Text:string);
    procedure ShowScreen_Main();
    procedure ShowScreen_Options();
    procedure Fill_Results();
    procedure ShowScreen_Results(Msg:gr_Message);
  public
    procedure UpdateState;
    procedure Paint;
end;


implementation
uses KM_Unit1, KM_Settings, KM_Render, KM_LoadLib, KM_Game, KM_LoadSFX, KM_PlayersCollection, KM_CommonTypes;


constructor TKMMainMenuInterface.Create(X,Y:word);
//var i:integer;
begin
inherited Create;

  fLog.AssertToLog(fGameSettings<>nil,'fGameSettings should be init before MainMenuInterface');
  fLog.AssertToLog(fTextLibrary<>nil,'fTextLibrary should be init before MainMenuInterface');

  {Parent Page for whole toolbar in-game}
  MyControls:=TKMControlsCollection.Create;
  ScreenX:=min(X,MENU_DESIGN_X);
  ScreenY:=min(Y,MENU_DESIGN_Y);
  OffX := (X-MENU_DESIGN_X) div 2;
  OffY := (Y-MENU_DESIGN_Y) div 2;
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
    L[i]:=MyControls.AddLabel(KMPanel_Main1,550,280+i*20,160,30,FontFiles[i]+' This is a test string for KaM Remake',TKMFont(i),kaLeft);
  //}

  //Show version info on every page
  KMLabel_Version:=MyControls.AddLabel(KMPanel_Main1,5,5,100,30,GAME_VERSION+' / OpenGL '+fRender.GetRendererVersion,fnt_Antiqua,kaLeft);

  SwitchMenuPage(nil);
  //ShowScreen_Results(); //Put here page you would like to debug
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


procedure TKMMainMenuInterface.ShowScreen_Loading(Text:string);
begin
  KMLabel_Loading.Caption:=Text;
  SwitchMenuPage(KMPanel_Loading);
end;


procedure TKMMainMenuInterface.ShowScreen_Main();
begin
  SwitchMenuPage(nil);
end;


procedure TKMMainMenuInterface.ShowScreen_Options();
begin
  SwitchMenuPage(KMButton_MainMenuOptions);
end;


procedure TKMMainMenuInterface.Fill_Results();
begin
  if (MyPlayer=nil) or (MyPlayer.fMissionSettings=nil) then exit;

  Label_Stat[1].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsLost);
  Label_Stat[2].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsKilled);
  Label_Stat[3].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesLost);
  Label_Stat[4].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesDestroyed);
  Label_Stat[5].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesConstructed);
  Label_Stat[6].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsTrained);
  Label_Stat[7].Caption := inttostr(MyPlayer.fMissionSettings.GetWeaponsProduced);
  Label_Stat[8].Caption := inttostr(MyPlayer.fMissionSettings.GetSoldiersTrained);
  Label_Stat[9].Caption := int2time(fGame.GetMissionTime);
end;

procedure TKMMainMenuInterface.ShowScreen_Results(Msg:gr_Message);
begin
  case Msg of
  gr_Win:    Label_Results_Result.Caption:=fTextLibrary.GetSetupString(111);
  gr_Defeat: Label_Results_Result.Caption:=fTextLibrary.GetSetupString(112);
  gr_Cancel: Label_Results_Result.Caption:='Mission canceled';
  else       Label_Results_Result.Caption:='<<<LEER>>>';
  end;
  SwitchMenuPage(KMPanel_Results);
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  KMPanel_MainMenu:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_MainMenuBG:=MyControls.AddImage(KMPanel_MainMenu,0,0,ScreenX,ScreenY,2,6);
    KMImage_MainMenuBG.StretchImage:=true;
    KMImage_MainMenu1:=MyControls.AddImage(KMPanel_MainMenu,120,100,423,164,4,5);
    KMImage_MainMenu3:=MyControls.AddImage(KMPanel_MainMenu,635,220,round(207*1.3),round(295*1.3),6,6);
    KMImage_MainMenu3.StretchImage:=true;

    KMPanel_MainButtons:=MyControls.AddPanel(KMPanel_MainMenu,155,300,350,400);
      KMButton_MainMenuTutor  :=MyControls.AddButton(KMPanel_MainButtons,0,  0,350,30,fTextLibrary.GetSetupString( 3),fnt_Metal,bsMenu);
      KMButton_MainMenuTSK    :=MyControls.AddButton(KMPanel_MainButtons,0, 40,350,30,fTextLibrary.GetSetupString( 1),fnt_Metal,bsMenu);
      KMButton_MainMenuTPR    :=MyControls.AddButton(KMPanel_MainButtons,0, 80,350,30,fTextLibrary.GetSetupString( 2),fnt_Metal,bsMenu);
      KMButton_MainMenuSingle :=MyControls.AddButton(KMPanel_MainButtons,0,120,350,30,fTextLibrary.GetSetupString( 4),fnt_Metal,bsMenu);
      KMButton_MainMenuLoad   :=MyControls.AddButton(KMPanel_MainButtons,0,160,350,30,fTextLibrary.GetSetupString(10),fnt_Metal,bsMenu);
      KMButton_MainMenuMulti  :=MyControls.AddButton(KMPanel_MainButtons,0,200,350,30,fTextLibrary.GetSetupString(11),fnt_Metal,bsMenu);
      KMButton_MainMenuOptions:=MyControls.AddButton(KMPanel_MainButtons,0,240,350,30,fTextLibrary.GetSetupString(12),fnt_Metal,bsMenu);
      KMButton_MainMenuCredit :=MyControls.AddButton(KMPanel_MainButtons,0,280,350,30,fTextLibrary.GetSetupString(13),fnt_Metal,bsMenu);
      KMButton_MainMenuQuit   :=MyControls.AddButton(KMPanel_MainButtons,0,360,350,30,fTextLibrary.GetSetupString(14),fnt_Metal,bsMenu);
      KMButton_MainMenuTutor.OnClick    :=MainMenu_PlayTutorial;
      KMButton_MainMenuSingle.OnClick   :=SwitchMenuPage;
      KMButton_MainMenuOptions.OnClick  :=SwitchMenuPage;
      KMButton_MainMenuCredit.OnClick   :=SwitchMenuPage;
      KMButton_MainMenuQuit.OnClick     :=Form1.Exit1.OnClick;
      KMButton_MainMenuTSK.Disable;
      KMButton_MainMenuTPR.Disable;
      KMButton_MainMenuLoad.Disable;
      KMButton_MainMenuMulti.Disable;
//      KMButton_MainMenuCredit.Disable;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  SingleMapsInfo:=TKMMapsInfo.Create;

  KMPanel_Single:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);

    KMImage_SingleBG:=MyControls.AddImage(KMPanel_Single,0,0,ScreenX,ScreenY,2,6);
    KMImage_SingleBG.StretchImage:=true;

    KMPanel_SingleList:=MyControls.AddPanel(KMPanel_Single,512+22,84,445,600);

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
        KMLabel_SingleTitle1[i]  :=MyControls.AddLabel(KMPanel_SingleList, 80+6 ,40+5+(i-1)*40,40,40,'<<<LEER>>>',fnt_Metal, kaLeft);
        KMLabel_SingleTitle2[i]  :=MyControls.AddLabel(KMPanel_SingleList, 80+6 ,40+22+(i-1)*40,40,40,'<<<LEER>>>',fnt_Game, kaLeft);
        KMButton_SingleSize[i]   :=MyControls.AddLabel(KMPanel_SingleList,380+20,40+(i-1)*40+14,40,40,'0',fnt_Metal, kaCenter);
      end;

      KMScrollBar_SingleMaps:=MyControls.AddScrollBar(KMPanel_SingleList,420,40,25,MENU_SINGLE_MAPS_COUNT*40,bsMenu);
      KMScrollBar_SingleMaps.OnChange:=SingleMap_ScrollChange;

      KMShape_SingleMap:=MyControls.AddShape(KMPanel_SingleList,0,40,420,40,$FFFFFF00);

    KMPanel_SingleDesc:=MyControls.AddPanel(KMPanel_Single,45,84,445,600);

      MyControls.AddBevel(KMPanel_SingleDesc,0,0,445,220);

      //KMImage_SingleScroll1:=MyControls.AddImage(KMPanel_SingleDesc,0,0,445,220,15,5);
      //KMImage_SingleScroll1.StretchImage:=true;
      //KMImage_SingleScroll1.Height:=220; //Need to reset it after stretching is enabled, cos it can't stretch down by default

      KMLabel_SingleTitle:=MyControls.AddLabel(KMPanel_SingleDesc,445 div 2,35,420,180,'',fnt_Outline, kaCenter);
      KMLabel_SingleTitle.AutoWrap:=true;

      KMLabel_SingleDesc:=MyControls.AddLabel(KMPanel_SingleDesc,15,60,420,160,'',fnt_Metal, kaLeft);
      KMLabel_SingleDesc.AutoWrap:=true;

      MyControls.AddBevel(KMPanel_SingleDesc,125,230,192,192);

      MyControls.AddBevel(KMPanel_SingleDesc,0,428,445,20);
      KMLabel_SingleCondTyp:=MyControls.AddLabel(KMPanel_SingleDesc,8,431,445,20,'Mission type: ',fnt_Metal, kaLeft);
      MyControls.AddBevel(KMPanel_SingleDesc,0,450,445,20);
      KMLabel_SingleCondWin:=MyControls.AddLabel(KMPanel_SingleDesc,8,453,445,20,'Win condition: ',fnt_Metal, kaLeft);
      MyControls.AddBevel(KMPanel_SingleDesc,0,472,445,20);
      KMLabel_SingleCondDef:=MyControls.AddLabel(KMPanel_SingleDesc,8,475,445,20,'Defeat condition: ',fnt_Metal, kaLeft);
      MyControls.AddBevel(KMPanel_SingleDesc,0,494,445,20);
      KMLabel_SingleAllies:=MyControls.AddLabel(KMPanel_SingleDesc,8,497,445,20,'Allies: ',fnt_Metal, kaLeft);
      MyControls.AddBevel(KMPanel_SingleDesc,0,516,445,20);
      KMLabel_SingleEnemies:=MyControls.AddLabel(KMPanel_SingleDesc,8,519,445,20,'Enemies: ',fnt_Metal, kaLeft);

      KMButton_SingleBack:=MyControls.AddButton(KMPanel_SingleDesc,0,570,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
      KMButton_SingleBack.OnClick:=SwitchMenuPage;
      KMButton_SingleStart:=MyControls.AddButton(KMPanel_SingleDesc,225,570,220,30,fTextLibrary.GetSetupString(8),fnt_Metal,bsMenu);
      KMButton_SingleStart.OnClick:=SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Options_Page;
var i:integer;
begin
  KMPanel_Options:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    Image_Options_BG:=MyControls.AddImage(KMPanel_Options,0,0,ScreenX,ScreenY,2,6);
    Image_Options_BG.StretchImage:=true;

    Label_Options_MouseSpeed:=MyControls.AddLabel(KMPanel_Options,124,130,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
    Label_Options_MouseSpeed.Disable;
    Ratio_Options_Mouse:=MyControls.AddRatioRow(KMPanel_Options,118,150,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    Ratio_Options_Mouse.Disable;
    Label_Options_SFX:=MyControls.AddLabel(KMPanel_Options,124,178,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
    Ratio_Options_SFX:=MyControls.AddRatioRow(KMPanel_Options,118,198,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);
    Label_Options_Music:=MyControls.AddLabel(KMPanel_Options,124,226,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
    Ratio_Options_Music:=MyControls.AddRatioRow(KMPanel_Options,118,246,160,20,fGameSettings.GetSlidersMin,fGameSettings.GetSlidersMax);

    Label_Options_MusicOn:=MyControls.AddLabel(KMPanel_Options,200,280,100,30,fTextLibrary.GetTextString(197),fnt_Metal,kaCenter);
    Button_Options_MusicOn:=MyControls.AddButton(KMPanel_Options,118,300,180,30,'',fnt_Metal, bsMenu);
    Button_Options_MusicOn.OnClick:=Options_Change;

    //Button_Options_1024:=MyControls.AddButton(KMPanel_Options,118,300,180,30,'',fnt_Metal, bsMenu);
    //Button_Options_1024.OnClick:=Options_Change;
    //Button_Options_1280:=MyControls.AddButton(KMPanel_Options,118,300,180,30,'',fnt_Metal, bsMenu);
    //Button_Options_1280.OnClick:=Options_Change;

    CheckBox_Options_FullScreen:=MyControls.AddCheckBox(KMPanel_Options,118,340,100,30,'Fullscreen',fnt_Metal);
    CheckBox_Options_FullScreen.OnClick:=Options_Change;

    KMLabel_Options_Lang:=MyControls.AddLabel(KMPanel_Options,178,380,100,30,'Language',fnt_Outline,kaCenter);
    KMBevel_Options_Lang_Background:=MyControls.AddBevel(KMPanel_Options,110,400,136,8+LocalesCount*20);
    for i:=1 to LocalesCount do
    begin
      CheckBox_Options_Lang[i]:=MyControls.AddCheckBox(KMPanel_Options,118,405+(i-1)*20,100,30,Locales[i,2],fnt_Metal);
      CheckBox_Options_Lang[i].OnClick:=Options_Change;
    end;

    Ratio_Options_Mouse.Position:=fGameSettings.GetMouseSpeed;
    Ratio_Options_SFX.Position  :=fGameSettings.GetSoundFXVolume;
    Ratio_Options_Music.Position:=fGameSettings.GetMusicVolume;
    Options_Change(nil);
    if fGameSettings.IsMusic then Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(201)
                             else Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(199);

    for i:=1 to KMPanel_Options.ChildCount do
    if TKMControl(KMPanel_Options.Childs[i]) is TKMRatioRow then
    begin
      TKMControl(KMPanel_Options.Childs[i]).OnClick:=Options_Change;
      TKMControl(KMPanel_Options.Childs[i]).OnChange:=Options_Change;
    end;

    Button_Options_Back:=MyControls.AddButton(KMPanel_Options,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_Options_Back.OnClick:=SwitchMenuPage;
    //Should contain resolution selector, fullscreen option and other things
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
begin
  KMPanel_Credits:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_CreditsBG:=MyControls.AddImage(KMPanel_Credits,0,0,ScreenX,ScreenY,2,6);
    KMImage_CreditsBG.StretchImage:=true;
    KMLabel_Credits:=MyControls.AddLabel(KMPanel_Credits,ScreenX div 2,ScreenY,100,30,'Credits go here'{ fTextLibrary.GetSetupString(300) @Krom: Can't handle labels with strings this long },fnt_Grey,kaCenter);
    KMButton_CreditsBack:=MyControls.AddButton(KMPanel_Credits,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    KMButton_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  KMPanel_Loading:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_LoadingBG:=MyControls.AddImage(KMPanel_Loading,0,0,ScreenX,ScreenY,2,6);
    KMImage_LoadingBG.StretchImage:=true;
    MyControls.AddLabel(KMPanel_Loading,ScreenX div 2,ScreenY div 2,100,30,'Loading... Please wait',fnt_Outline,kaCenter);
    KMLabel_Loading:=MyControls.AddLabel(KMPanel_Loading,ScreenX div 2,ScreenY div 2+20,100,30,'...',fnt_Grey,kaCenter);
end;


procedure TKMMainMenuInterface.Create_Results_Page;
var i:integer; Adv:integer;
begin
  KMPanel_Results:=MyControls.AddPanel(KMPanel_Main1,0,0,ScreenX,ScreenY);
    KMImage_ResultsBG:=MyControls.AddImage(KMPanel_Results,0,0,ScreenX,ScreenY,7,5);
    KMImage_ResultsBG.StretchImage:=true;

    Label_Results_Result:=MyControls.AddLabel(KMPanel_Results,512,200,100,30,'<<<LEER>>>',fnt_Metal,kaCenter);

    KMPanel_Stats:=MyControls.AddPanel(KMPanel_Results,80,240,400,400);
    Adv:=0;
    for i:=1 to 9 do
    begin
      inc(Adv,25);
      if i in [3,6,7,9] then inc(Adv,15);
      MyControls.AddLabel(KMPanel_Stats,0,Adv,100,30,fTextLibrary.GetSetupString(112+i),fnt_Metal,kaLeft);
      Label_Stat[i]:=MyControls.AddLabel(KMPanel_Stats,340,Adv,100,30,'00',fnt_Metal,kaRight);
    end;

    KMButton_ResultsBack:=MyControls.AddButton(KMPanel_Results,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
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
     (Sender=Button_Options_Back)or
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
  if Sender=KMButton_MainMenuCredit then begin
    KMPanel_Credits.Show;
    KMLabel_Credits.Top:=ScreenY;
  end;

  {Show Loading... screen}
  if Sender=KMPanel_Loading then
    KMPanel_Loading.Show;

  {Show Results screen}
  if Sender=KMPanel_Results then //This page can be accessed only by itself
    KMPanel_Results.Show;

  { Save settings when leaving options, if needed }
  if Sender=Button_Options_Back then
    if fGameSettings.GetNeedsSave then
      fGameSettings.SaveSettings;
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

  KMLabel_SingleCondTyp.Caption:='Mission type: '+SingleMapsInfo.GetTyp(SingleMap_Selected);
  KMLabel_SingleCondWin.Caption:='Win condition: '+SingleMapsInfo.GetWin(SingleMap_Selected);
  KMLabel_SingleCondDef.Caption:='Defeat condition: '+SingleMapsInfo.GetDefeat(SingleMap_Selected);
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
var MissionPath:string;
begin
  fLog.AssertToLog(Sender=KMButton_SingleStart,'not KMButton_SingleStart');
  if not InRange(SingleMap_Selected,1,SingleMapsInfo.GetMapCount) then exit;
  MissionPath:=ExeDir+'Maps\'+SingleMapsInfo.GetFolder(SingleMap_Selected)+'\'+SingleMapsInfo.GetMissionFile(SingleMap_Selected);
  fGame.StartGame(MissionPath); //Provide mission filename here
end;

procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
var i:integer;
begin
  if Sender = Ratio_Options_Mouse then fGameSettings.SetMouseSpeed(Ratio_Options_Mouse.Position);
  if Sender = Ratio_Options_SFX   then fGameSettings.SetSoundFXVolume(Ratio_Options_SFX.Position);
  if Sender = Ratio_Options_Music then fGameSettings.SetMusicVolume(Ratio_Options_Music.Position);
  if Sender = Button_Options_MusicOn then fGameSettings.IsMusic := not fGameSettings.IsMusic;

  if fGameSettings.IsMusic then Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(201)
                           else Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(199);

  //@Krom: Yes, I think it should be a proper control in a KaM style. Just text [x] doesn't look great.
  //       Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
  CheckBox_Options_FullScreen.Checked := fGameSettings.IsFullScreen;

  //This one should be called last since it re-inits whole fGame and the rest
  if Sender = CheckBox_Options_FullScreen then begin
    fGame.ToggleFullScreen(not fGameSettings.IsFullScreen,true);
    exit;
  end;

  for i:=1 to LocalesCount do
    if Sender = CheckBox_Options_Lang[i] then begin
      fGameSettings.SetLocale := Locales[i,1];
      fGame.ToggleLocale;
      exit;
    end;

  for i:=1 to LocalesCount do
    CheckBox_Options_Lang[i].Checked := LowerCase(fGameSettings.GetLocale) = LowerCase(Locales[i,1]);

end;

procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  fLog.AssertToLog(Sender=KMButton_MainMenuTutor,'not KMButton_MainMenuTutor');
  fGame.StartGame(ExeDir+'data\mission\mission0.dat'); //Provide mission filename here
end;


{Should update credits page mostly}
procedure TKMMainMenuInterface.UpdateState;
begin
  if KMPanel_Credits.Visible then
    KMLabel_Credits.Top := KMLabel_Credits.Top - 2; //Very slow, needs to be smoothed!

end;


procedure TKMMainMenuInterface.Paint;
begin
  MyControls.Paint;
end;



end.
