unit KM_InterfaceMapEditor;
interface
uses SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls, StrUtils, Windows,
  KM_Controls, KM_Houses, KM_Units, KM_Defaults, KM_LoadDAT;

type TKMMapEditorInterface = class
  protected
    ToolBarX:word;
  protected
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    ShownHint:TObject;

    KMPanel_Main:TKMPanel;
      KMImage_Main1,KMImage_Main2,KMImage_Main3,KMImage_Main4:TKMImage; //Toolbar background
      KMMinimap:TKMMinimap;
      KMLabel_Stat,KMLabel_Hint:TKMLabel;
      KMButtonMain:array[1..5]of TKMButton; //4 common buttons + Return
      KMImage_Message:array[1..256]of TKMImage; //Queue of messages
      KMLabel_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
    KMPanel_Stats:TKMPanel;
      Stat_HousePic,Stat_UnitPic:array[1..32]of TKMImage;
      Stat_HouseQty,Stat_UnitQty:array[1..32]of TKMLabel;

    KMPanel_Build:TKMPanel;
      KMLabel_Build:TKMLabel;
      KMImage_Build_Selected:TKMImage;
      KMButton_BuildRoad,KMButton_BuildField,KMButton_BuildWine{,KMButton_BuildWall},KMButton_BuildCancel:TKMButtonFlat;
      KMButton_Build:array[1..HOUSE_COUNT]of TKMButtonFlat;

    KMPanel_Menu:TKMPanel;
      KMButton_Menu_Save,KMButton_Menu_Load,KMButton_Menu_Settings,KMButton_Menu_Quit:TKMButton;
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
      KMLabel_UnitDescription:TKMLabel;
      KMConditionBar_Unit:TKMPercentBar;
      KMImage_UnitPic:TKMImage;
      KMButton_Die:TKMButton;
      
    KMPanel_House:TKMPanel;
      KMLabel_House:TKMLabel;
      KMImage_House_Logo,KMImage_House_Worker:TKMImage;
      KMHealthBar_House:TKMPercentBar;
      KMLabel_HouseHealth:TKMLabel;

    KMPanel_HouseStore:TKMPanel;
      KMButton_Store:array[1..28]of TKMButtonFlat;
      KMImage_Store_Accept:array[1..28]of TKMImage;
    KMPanel_HouseBarracks:TKMPanel;
      KMButton_Barracks:array[1..12]of TKMButtonFlat;
      KMLabel_Barracks_Unit:TKMLabel;
      KMImage_Barracks_Right,KMImage_Barracks_Train,KMImage_Barracks_Left:TKMImage;
      KMButton_Barracks_Right,KMButton_Barracks_Train,KMButton_Barracks_Left:TKMButton;
  private
    procedure Create_Build_Page;
    procedure Create_Stats_Page;
    procedure Create_Menu_Page;
    procedure Create_Save_Page;
    procedure Create_Load_Page;
    procedure Create_Settings_Page;
    procedure Create_Quit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_Barracks_Page;

    procedure SwitchPage(Sender: TObject);
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
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure Unit_Die(Sender:TObject);
    procedure House_StoreAcceptFlag(Sender:TObject);
    procedure Menu_ShowSettings(Sender: TObject);
    procedure Menu_Settings_Change(Sender:TObject);
    procedure Menu_ShowLoad(Sender: TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Build_SelectRoad;
    procedure Build_RightClickCancel;
    procedure IssueMessage(MsgTyp:TKMMessageType; Text:string);
    procedure ShortcutPress(Key:Word; IsDown:boolean=false);
    property GetShownUnit: TKMUnit read ShownUnit;
    procedure ClearShownUnit;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Unit1, KM_PlayersCollection, KM_Settings, KM_Render, KM_LoadLib, KM_Terrain, KM_Utils, KM_Viewport, KM_Game, KM_SoundFX, KM_CommonTypes;


{Switch between pages}
procedure TKMMapEditorInterface.SwitchPage(Sender: TObject);
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
    if fGameSettings.GetNeedsSave then
      fGameSettings.SaveSettings;

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

  if Sender=KMPanel_HouseBarracks then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender=KMPanel_HouseStore then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end;

end;


procedure TKMMapEditorInterface.DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
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
procedure TKMMapEditorInterface.Minimap_Update(Sender: TObject);
begin
  if Sender=nil then begin //UpdateState loop
    KMMinimap.MapSize:=KMPoint(fTerrain.MapX,fTerrain.MapY);
  end else
    if KMMinimap.CenteredAt.X*KMMinimap.CenteredAt.Y <> 0 then //Quick bugfix incase minimap yet not inited it will center vp on 0;0
    fViewport.SetCenter(KMMinimap.CenteredAt.X,KMMinimap.CenteredAt.Y);

  KMMinimap.CenteredAt:=fViewport.GetCenter;
  KMMinimap.ViewArea:=fViewport.GetMinimapClip;
end;


constructor TKMMapEditorInterface.Create();
var i:integer;
begin
Inherited;
fLog.AssertToLog(fGameSettings<>nil,'fGameSettings required to be init first');
fLog.AssertToLog(fViewport<>nil,'fViewport required to be init first');

  MyControls := TKMControlsCollection.Create;

  ShownUnit:=nil;
  ShownHouse:=nil;




{Parent Page for whole toolbar in-game}
  KMPanel_Main:=MyControls.AddPanel(nil,0,0,224,768);

    KMImage_Main1:=MyControls.AddImage(KMPanel_Main,0,0,224,200,407);
    KMImage_Main3:=MyControls.AddImage(KMPanel_Main,0,200,224,168,554);
    KMImage_Main4:=MyControls.AddImage(KMPanel_Main,0,368,224,400,403);
                   MyControls.AddImage(KMPanel_Main,0,768,224,400,403);

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

    KMLabel_Stat:=MyControls.AddLabel(KMPanel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    KMLabel_Hint:=MyControls.AddLabel(KMPanel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,'',fnt_Outline,kaLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Build_Page();
  Create_Stats_Page();
  Create_Menu_Page();
    Create_Save_Page();
    Create_Load_Page();
    Create_Settings_Page();
    Create_Quit_Page();

  Create_Unit_Page();
  Create_House_Page();
    Create_Store_Page();
    Create_Barracks_Page();
    //Create_TownHall_Page();

  SetHintEvents(DisplayHint); //Set all OnHint events to be the correct function

  SwitchPage(nil); //Update
end;


destructor TKMMapEditorInterface.Destroy;
begin
  FreeAndNil(MyControls);
  inherited;
end;


procedure TKMMapEditorInterface.SetScreenSize(X,Y:word);
begin
  //todo: Update Hint position and etc..
end;


{Build page}
procedure TKMMapEditorInterface.Create_Build_Page;
var i:integer;
begin
  KMPanel_Build:=MyControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMLabel_Build:=MyControls.AddLabel(KMPanel_Build,100,10,100,30,'',fnt_Outline,kaCenter);
    KMImage_Build_Selected:=MyControls.AddImage(KMPanel_Build,8,40,32,32,335);
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
        KMButton_Build[i]:=MyControls.AddButtonFlat(KMPanel_Build, 8+((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,GUIBuildIcons[byte(GUIHouseOrder[i])]);
        KMButton_Build[i].OnClick:=Build_ButtonClick;
        KMButton_Build[i].Hint:=fTextLibrary.GetTextString(GUIBuildIcons[byte(GUIHouseOrder[i])]-300);
      end;
end;


{Statistics page}
procedure TKMMapEditorInterface.Create_Stats_Page;
const IncY=34; Nil_Width=10; House_Width=30; Unit_Width=26;
var i,k:integer; hc,uc,off:integer;
begin
  KMPanel_Stats:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);

  hc:=1; uc:=1;
  for i:=1 to 8 do begin
    off:=8;
    case i of //This should be simplified, compacted and automated
    1: begin
          MyControls.AddBevel(KMPanel_Stats,  8,(i-1)*IncY,56,30);
          MyControls.AddBevel(KMPanel_Stats, 71,(i-1)*IncY,56,30);
          MyControls.AddBevel(KMPanel_Stats,134,(i-1)*IncY,56,30);
       end;
    2: begin
          MyControls.AddBevel(KMPanel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,(i-1)*IncY,86,30);
       end;
    3: begin
          MyControls.AddBevel(KMPanel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,(i-1)*IncY,86,30);
       end;
    4: begin
          MyControls.AddBevel(KMPanel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(KMPanel_Stats,104,(i-1)*IncY,86,30);
       end;
    5:    MyControls.AddBevel(KMPanel_Stats,8,(i-1)*IncY,116,30);
    6:    MyControls.AddBevel(KMPanel_Stats,8,(i-1)*IncY,146,30);
    7:    MyControls.AddBevel(KMPanel_Stats,8,(i-1)*IncY,86,30);
    8: begin
          MyControls.AddBevel(KMPanel_Stats,  8,(i-1)*IncY,120,30);
          MyControls.AddBevel(KMPanel_Stats,138,(i-1)*IncY,52,30);
       end;
    end;

    for k:=1 to 8 do
    if StatCount[i,k]=0 then begin
      if i=1 then
        inc(off,Nil_Width-3)
      else
        inc(off,Nil_Width);
    end else
    if StatCount[i,k]=1 then begin
      Stat_HousePic[hc]:=MyControls.AddImage(KMPanel_Stats,off,(i-1)*IncY,House_Width,30,41{byte(StatHouse[hc])+300});
      Stat_HousePic[hc].Hint:=TypeToString(StatHouse[hc]);
      Stat_HouseQty[hc]:=MyControls.AddLabel(KMPanel_Stats,off+House_Width-2,(i-1)*IncY+16,37,30,'-',fnt_Grey,kaRight);
      Stat_HouseQty[hc].Hint:=TypeToString(StatHouse[hc]);
      inc(hc);
      inc(off,House_Width);
    end else
    if StatCount[i,k]=2 then begin
      Stat_UnitPic[uc]:=MyControls.AddImage(KMPanel_Stats,off,(i-1)*IncY,Unit_Width,30,byte(StatUnit[uc])+140);
      Stat_UnitPic[uc].Hint:=TypeToString(StatUnit[uc]);
      Stat_UnitQty[uc]:=MyControls.AddLabel(KMPanel_Stats,off+Unit_Width-2,(i-1)*IncY+16,33,30,'-',fnt_Grey,kaRight);
      Stat_UnitQty[uc].Hint:=TypeToString(StatUnit[uc]);
      inc(uc);
      inc(off,Unit_Width);
    end;
  end;

end;


{Menu page}
procedure TKMMapEditorInterface.Create_Menu_Page;
begin
  KMPanel_Menu:=MyControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMButton_Menu_Save:=MyControls.AddButton(KMPanel_Menu,8,20,180,30,fTextLibrary.GetTextString(175),fnt_Metal);
    KMButton_Menu_Save.OnClick:=Menu_ShowLoad;
    KMButton_Menu_Save.Hint:=fTextLibrary.GetTextString(175);
    KMButton_Menu_Save.Disable;
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
    KMLabel_Menu_Music:=MyControls.AddLabel(KMPanel_Menu,100,298,100,30,fTextLibrary.GetTextString(207),fnt_Metal,kaCenter);
    KMLabel_Menu_Track:=MyControls.AddLabel(KMPanel_Menu,100,326,100,30,'Spirit',fnt_Grey,kaCenter);
end;


{Save page}
procedure TKMMapEditorInterface.Create_Save_Page;
var i:integer;
begin
  KMPanel_Save:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    for i:=1 to SAVEGAME_COUNT do begin
      KMButton_Save[i]:=MyControls.AddButton(KMPanel_Save,12,10+(i-1)*28,170,24,'Savegame #'+inttostr(i),fnt_Grey);
      //KMButton_Save[i].OnClick:=SaveGame;
      KMButton_Save[i].Disable;
    end;
end;


{Load page}
procedure TKMMapEditorInterface.Create_Load_Page;
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
procedure TKMMapEditorInterface.Create_Settings_Page;
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
procedure TKMMapEditorInterface.Create_Quit_Page;
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
procedure TKMMapEditorInterface.Create_Unit_Page;
begin
  KMPanel_Unit:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    KMLabel_UnitName:=MyControls.AddLabel(KMPanel_Unit,100,16,100,30,'',fnt_Outline,kaCenter);
    KMImage_UnitPic:=MyControls.AddImage(KMPanel_Unit,8,38,54,100,521);
    KMLabel_UnitCondition:=MyControls.AddLabel(KMPanel_Unit,120,40,100,30,fTextLibrary.GetTextString(254),fnt_Grey,kaCenter);
    KMConditionBar_Unit:=MyControls.AddPercentBar(KMPanel_Unit,73,55,116,15,80);
    KMButton_Die:=MyControls.AddButton(KMPanel_Unit,73,112,54,20,'Die',fnt_Grey);
    KMButton_Die.OnClick:=Unit_Die;
    KMLabel_UnitDescription:=MyControls.AddLabel(KMPanel_Unit,8,152,236,200,'',fnt_Grey,kaLeft); //Taken from LIB resource
    //Military buttons start at 8.170 and are 52x38/30 (60x46)
end;


{House description page}
procedure TKMMapEditorInterface.Create_House_Page;
begin
  KMPanel_House:=MyControls.AddPanel(KMPanel_Main,0,412,200,400);
    //Thats common things
    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    KMLabel_House:=MyControls.AddLabel(KMPanel_House,100,14,100,30,'',fnt_Outline,kaCenter);
    KMImage_House_Logo:=MyControls.AddImage(KMPanel_House,68,41,32,32,338);
    KMImage_House_Worker:=MyControls.AddImage(KMPanel_House,98,41,32,32,141);
    KMLabel_HouseHealth:=MyControls.AddLabel(KMPanel_House,156,45,30,50,fTextLibrary.GetTextString(228),fnt_Mini,kaCenter,$FFFFFFFF);
    KMHealthBar_House:=MyControls.AddPercentBar(KMPanel_House,129,57,55,15,50,'',fnt_Mini);
end;

{Store page}
procedure TKMMapEditorInterface.Create_Store_Page;
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


{Barracks page}
procedure TKMMapEditorInterface.Create_Barracks_Page;
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
      KMButton_Barracks_Left.Hint :=fTextLibrary.GetTextString(237);
      KMButton_Barracks_Train.Hint:=fTextLibrary.GetTextString(240);
      KMButton_Barracks_Right.Hint:=fTextLibrary.GetTextString(238);
      KMButton_Barracks_Train.Disable; //Unimplemented yet
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMMapEditorInterface.UpdateState;
var i:integer;
begin
  if ShownUnit<>nil then ShowUnitInfo(ShownUnit) else
  if ShownHouse<>nil then ShowHouseInfo(ShownHouse);

  if ShownHint<>nil then DisplayHint(ShownHint,[],0,0);
  if Mouse.CursorPos.X>ToolBarWidth then DisplayHint(nil,[],0,0); //Don't display hints if not over ToolBar

  Minimap_Update(nil);

  for i:=low(KMImage_Message) to high(KMImage_Message) do
    if Assigned(KMImage_Message[i]) then
    begin
      if KMImage_Message[i].Top < 768-48 then //Add stacking here
        KMImage_Message[i].Top:= KMImage_Message[i].Top + 24;
    end;

  if KMPanel_Build.Visible then Build_Fill(nil);
  if KMPanel_Stats.Visible then Stats_Fill(nil);
  if KMPanel_Menu.Visible then Menu_Fill(nil);
end;


procedure TKMMapEditorInterface.Build_ButtonClick(Sender: TObject);
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
  KMLabel_Build.Caption := '';
  KMImage_Build_Selected.TexID := TKMButtonFlat(Sender).TexID;

  if KMButton_BuildCancel.Down then begin
    CursorMode.Mode:=cm_Erase;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(210);
  end;
  if KMButton_BuildRoad.Down then begin
    CursorMode.Mode:=cm_Road;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(212);
  end;
  if KMButton_BuildField.Down then begin
    CursorMode.Mode:=cm_Field;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(214);
  end;
  if KMButton_BuildWine.Down then begin
    CursorMode.Mode:=cm_Wine;
    KMLabel_Build.Caption := fTextLibrary.GetTextString(218);
  end;
{  if KMButton_BuildWall.Down then begin
    CursorMode.Mode:=cm_Wall;
    KMLabel_BuildCost_Wood.Caption:='1';
    //KMLabel_Build.Caption := fTextLibrary.GetTextString(218);
  end;}

  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if KMButton_Build[i].Down then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Param:=byte(GUIHouseOrder[i]);
     KMLabel_Build.Caption := TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end;
end;


procedure TKMMapEditorInterface.ShowHouseInfo(Sender:TKMHouse);
var i:integer;
begin
  ShownUnit:=nil;
  ShownHouse:=Sender;

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

  for i:=1 to KMPanel_House.ChildCount do
    KMPanel_House.Childs[i].Show; //show all
  KMImage_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  SwitchPage(KMPanel_House);

  case Sender.GetHouseType of
    ht_Store: begin
          Store_Fill(nil);
          SwitchPage(KMPanel_HouseStore);
        end;

    ht_Barracks: begin
          KMImage_House_Worker.Enabled := true; //In the barrack the recruit icon is always enabled
          SwitchPage(KMPanel_HouseBarracks);
          end;
    ht_TownHall:;
  end;
end;


procedure TKMMapEditorInterface.ShowUnitInfo(Sender:TKMUnit);
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


procedure TKMMapEditorInterface.Unit_Die(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  TKMUnit(fPlayers.Selected).KillUnit;
end;


{That small red triangle blocking delivery of goods to Storehouse}
{Resource determined by Button.Tag property}
procedure TKMMapEditorInterface.House_StoreAcceptFlag(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  TKMHouseStore(fPlayers.Selected).ToggleAcceptFlag((Sender as TKMControl).Tag);
end;


procedure TKMMapEditorInterface.Menu_ShowSettings(Sender: TObject);
begin
  Menu_Settings_Change(nil); //Prepare eveything first
  SwitchPage(Sender); //Only then switch
end;


procedure TKMMapEditorInterface.Menu_Settings_Change(Sender:TObject);
begin
  if Sender = KMButton_Settings_Dark then fGameSettings.DecBrightness;
  if Sender = KMButton_Settings_Light then fGameSettings.IncBrightness;
  if Sender = KMCheckBox_Settings_Autosave then fGameSettings.IsAutosave:=not fGameSettings.IsAutosave;
  if Sender = KMCheckBox_Settings_FastScroll then fGameSettings.IsFastScroll:=not fGameSettings.IsFastScroll;
  if Sender = KMRatio_Settings_Mouse then fGameSettings.SetMouseSpeed(KMRatio_Settings_Mouse.Position);
  if Sender = KMRatio_Settings_SFX then fGameSettings.SetSoundFXVolume(KMRatio_Settings_SFX.Position);
  if Sender = KMRatio_Settings_Music then fGameSettings.SetMusicVolume(KMRatio_Settings_Music.Position);
  if Sender = KMButton_Settings_Music then fGameSettings.IsMusic:=not fGameSettings.IsMusic;
  
  KMLabel_Settings_BrightValue.Caption:=fTextLibrary.GetTextString(185 + fGameSettings.GetBrightness);
  KMCheckBox_Settings_Autosave.Checked:=fGameSettings.IsAutosave;
  KMCheckBox_Settings_FastScroll.Checked:=fGameSettings.IsFastScroll;
  KMRatio_Settings_Mouse.Position:=fGameSettings.GetMouseSpeed;
  KMRatio_Settings_SFX.Position:=fGameSettings.GetSoundFXVolume;
  KMRatio_Settings_Music.Position:=fGameSettings.GetMusicVolume;

  if fGameSettings.IsMusic then
    KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(201)
  else
    KMButton_Settings_Music.Caption:=fTextLibrary.GetTextString(199);
end;


{Show list of savegames and act depending on Sender (Save or Load)}
procedure TKMMapEditorInterface.Menu_ShowLoad(Sender: TObject);
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
procedure TKMMapEditorInterface.Menu_QuitMission(Sender:TObject);
var i:integer;
begin
  KMPanel_Main.Hide;
  for i:=1 to KMPanel_Main.ChildCount do
    if KMPanel_Main.Childs[i] is TKMPanel then
      KMPanel_Main.Childs[i].Hide;

  fGame.StopGame(gr_Cancel);
end;


procedure TKMMapEditorInterface.Build_Fill(Sender:TObject);
var i:integer;
begin
  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  begin
    KMButton_Build[i].Enable;
    KMButton_Build[i].TexID:=GUIBuildIcons[byte(GUIHouseOrder[i])];
    KMButton_Build[i].OnClick:=Build_ButtonClick;
    KMButton_Build[i].Hint:=TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end;
end;


{Virtually press BuildRoad button when changing page to BuildingPage or after house plan is placed}
procedure TKMMapEditorInterface.Build_SelectRoad;
begin
  Build_ButtonClick(KMButton_BuildRoad);
end;


procedure TKMMapEditorInterface.Build_RightClickCancel;
begin
  //This function will be called if the user right clicks on the screen. We should close the build menu if it's open.
  if KMPanel_Build.Visible = true then
    SwitchPage(KMButtonMain[5]);
end;


procedure TKMMapEditorInterface.IssueMessage(MsgTyp:TKMMessageType; Text:string);
var i:integer;
begin
  for i:=low(KMImage_Message) to high(KMImage_Message) do
    if not Assigned(KMImage_Message[i]) then
    begin
      KMImage_Message[i]:=MyControls.AddImage(KMPanel_Main,ToolBarWidth,0,30,48,word(MsgTyp));
      break;
    end;
  fSoundLib.Play(sfx_MessageNotice); //Play horn sound on new message
end;


procedure TKMMapEditorInterface.Store_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(fPlayers.Selected).ResourceCount[i];
    if Tmp=0 then KMButton_Store[i].Caption:='-' else
    //if Tmp>999 then KMButton_Store[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  KMButton_Store[i].Caption:=inttostr(Tmp);
    KMImage_Store_Accept[i].Visible := TKMHouseStore(fPlayers.Selected).NotAcceptFlag[i];
  end;
end;


procedure TKMMapEditorInterface.Menu_Fill(Sender:TObject);
begin
  if fGameSettings.IsMusic then
  begin
    KMLabel_Menu_Track.Caption := fMusicLib.GetTrackTitle;
    KMLabel_Menu_Track.Enabled := true;
  end
  else begin
    KMLabel_Menu_Track.Caption := '-';
    KMLabel_Menu_Track.Enabled := false;
  end;
end;

procedure TKMMapEditorInterface.Stats_Fill(Sender:TObject);
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

procedure TKMMapEditorInterface.SetHintEvents(AHintEvent:TMouseMoveEvent);
var
  i: integer;
begin
  //Here we must go through every control and set the hint event to be the parameter
  for i:=0 to MyControls.Count-1 do
    if MyControls.Items[i] <> nil then
      TKMControl(MyControls.Items[i]).OnHint := AHintEvent;
end;


procedure TKMMapEditorInterface.ShortcutPress(Key:Word; IsDown:boolean=false);
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


procedure TKMMapEditorInterface.ClearShownUnit;
begin
  ShownUnit := nil;
  SwitchPage(nil);
end;


procedure TKMMapEditorInterface.Paint;
begin
  MyControls.Paint;
end;


end.
