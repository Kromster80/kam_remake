unit KM_GamePlayInterface;
interface
uses KM_Controls, KM_Houses, KM_Units, KM_Defaults, SysUtils, KromUtils, KromOGLUtils, Math, Classes;

type TKMGamePlayInterface = class
  protected
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    ShownHint:TObject;
    LastSchoolUnit:integer; //Last unit that was selected in School, global for all schools player owns
    LastBarrackUnit:integer;//Last unit that was selected in Barracks, global for all barracks player owns

    KMPanel_Main:TKMPanel;
      KMImage_1,KMImage_2,KMImage_3,KMImage_4:TKMImage; //Toolbar background
      KMMinimap:TKMMinimap;
      KMLabel_Hint:TKMLabel;
      L:array[1..20]of TKMLabel;
      KMButtonRun,KMButtonRun1:TKMButton;                //Start Village functioning
      KMButtonMain:array[1..5]of TKMButton; //4 common buttons + Return
    KMPanel_Ratios:TKMPanel;
      //
    KMPanel_Stats:TKMPanel;
      Stat_House,Stat_Unit:array[1..32]of TKMButtonFlat;
      Stat_HouseQty,Stat_UnitQty:array[1..32]of TKMLabel;
      //
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
      KMButton_Menu_Save,KMButton_Menu_Load,KMButton_Menu_Settings,KMButton_Menu_Exit:TKMButton;

    KMPanel_Settings:TKMPanel;
      KMLabel_Settings_Brightness,KMLabel_Settings_BrightValue:TKMLabel;
      KMButton_Settings_Dark,KMButton_Settings_Light:TKMButton;
      KMLabel_Settings_Autosave,KMLabel_Settings_FastScroll:TKMLabel;
      KMLabel_Settings_MouseSpeed,KMLabel_Settings_SFX,KMLabel_Settings_Music,KMLabel_Settings_Music2:TKMLabel;
      KMRatio_Settings_Mouse,KMRatio_Settings_SFX,KMRatio_Settings_Music:TKMRatioRow;
      KMButton_Settings_Music:TKMButton;

    KMPanel_Unit:TKMPanel;
      KMLabel_UnitName:TKMLabel;
      KMLabel_UnitCondition:TKMLabel;
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
      KMLabel_Common_Demand,KMLabel_Common_Offer,KMLabel_Common_Costs:TKMLabel;
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
    procedure Minimap_Move(Sender: TObject; AShift:TShiftState; X,Y:integer);
    procedure BuildButtonClick(Sender: TObject);
    procedure Build_Fill(Sender:TObject);
    procedure StoreFill(Sender:TObject);
    procedure BarracksFill(Sender:TObject);
    procedure Stats_Fill(Sender:TObject);
  public
    constructor Create;
    procedure Create_Build_Page;
    procedure Create_Ratios_Page;
    procedure Create_Stats_Page;
    procedure Create_Menu_Page;
    procedure Create_Settings_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_School_Page;
    procedure Create_Barracks_Page;
    procedure UpdateState;
    procedure ShowSettings(Sender: TObject);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure House_RepairToggle(Sender:TObject);
    procedure House_WareDeliveryToggle(Sender:TObject);
    procedure House_OrderClick(Sender:TObject);
    procedure House_OrderClickRight(Sender:TObject);
    procedure House_SchoolUnitChange(Sender:TObject);
    procedure House_SchoolUnitChangeRight(Sender:TObject);
    procedure House_SchoolUnitRemove(Sender:TObject);
    procedure House_StoreAcceptFlag(Sender:TObject);
    procedure Settings_Change(Sender:TObject);
    procedure SelectRoad;
  end;

var
  fGamePlayInterface: TKMGamePlayInterface;

implementation
uses KM_Unit1, KM_Users, KM_Settings, KM_Render, KM_LoadLib, KM_Terrain, KM_Viewport;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var i:integer;
  procedure Hide4MainButtons();
  var i:integer;
  begin
    for i:=1 to 4 do
      KMButtonMain[i].Visible:=false;
    KMButtonMain[5].Visible:=true;
  end;  
  procedure Show4MainButtons();
  var i:integer;
  begin
    for i:=1 to 4 do
      KMButtonMain[i].Visible:=true;
    KMButtonMain[5].Visible:=false;
  end;
begin

if (Sender=KMButtonMain[1])or(Sender=KMButtonMain[2])or(Sender=KMButtonMain[3])or(Sender=KMButtonMain[4])or
   (Sender=KMButton_Menu_Settings) then begin
  ShownHouse:=nil;
  ShownUnit:=nil;
end;

//Reset the CursorMode, to cm_None
BuildButtonClick(nil);

//First thing - hide all existing pages
  for i:=1 to KMPanel_Main.ChildCount do
    if KMPanel_Main.Childs[i] is TKMPanel then
      KMPanel_Main.Childs[i].Visible:=false;
//First thing - hide all existing pages
  for i:=1 to KMPanel_House.ChildCount do
    if KMPanel_House.Childs[i] is TKMPanel then
      KMPanel_House.Childs[i].Visible:=false;

//If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
if Sender=KMButtonMain[1] then begin
  Build_Fill(nil);
  KMPanel_Build.Visible:=true;
  Hide4MainButtons;
  SelectRoad;
end else
if Sender=KMButtonMain[2] then begin
  KMPanel_Ratios.Visible:=true;
  Hide4MainButtons;
end else
if Sender=KMButtonMain[3] then begin
  Stats_Fill(nil);
  KMPanel_Stats.Visible:=true;
  Hide4MainButtons;
end else
if Sender=KMButtonMain[4] then begin
  KMPanel_Menu.Visible:=true;
  Hide4MainButtons;
end else  
if Sender=KMButton_Menu_Settings then begin
  KMPanel_Settings.Visible:=true;
  Hide4MainButtons;
end else //If Sender is anything else - then show all 4 buttons and hide Return button
  Show4MainButtons;

//Now process all other kinds of pages
if Sender=KMPanel_Unit then begin
  TKMPanel(Sender).Visible:=true;
end else
if Sender=KMPanel_House then begin
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_House_Common then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_House_School then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_HouseBarracks then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_HouseStore then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;

end;


procedure TKMGamePlayInterface.DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
begin
  ShownHint:=Sender;
  if (ShownHint<>nil)and(not (ShownHint as TKMControl).CursorOver) then ShownHint:=nil;
  if (ShownHint<>nil)and(not (ShownHint as TKMControl).Visible) then ShownHint:=nil;
  //Random numbers here are to show every time hint gets refreshed
  KMLabel_Hint.Top:=fRender.GetRenderAreaSize.Y-16;
  if ShownHint=nil then KMLabel_Hint.Caption:=''+'    '+inttostr(random(10)) else
  KMLabel_Hint.Caption:=(Sender as TKMControl).Hint+'    '+inttostr(random(10));
end;


procedure TKMGamePlayInterface.Minimap_Move(Sender: TObject; AShift:TShiftState; X,Y:integer);
begin
  KMMinimap.MapSize:=KMPoint(fTerrain.MapX,fTerrain.MapY);

  if Sender=nil then begin
    KMMinimap.CenteredAt:=fViewport.GetCenter;
    KMMinimap.ViewArea:=fViewport.GetClip;
  end else begin
    fViewport.SetCenter(KMMinimap.CenteredAt.X,KMMinimap.CenteredAt.Y);

  end;

end;


constructor TKMGamePlayInterface.Create();
var i:integer;
begin
Assert(fGameSettings<>nil,'fGameSettings required to be init first');

  ShownUnit:=nil;
  ShownHouse:=nil;

  LastSchoolUnit:=1;
  LastBarrackUnit:=1;
{Parent Page for whole toolbar in-game}
  KMPanel_Main:=fControls.AddPanel(nil,0,0,224,768);

    KMImage_1:=fControls.AddImage(KMPanel_Main,0,0,224,200,407);
    KMImage_3:=fControls.AddImage(KMPanel_Main,0,200,224,168,554);
    KMImage_4:=fControls.AddImage(KMPanel_Main,0,368,224,400,404);

    KMMinimap:=fControls.AddMinimap(KMPanel_Main,10,10,176,176);
    KMMinimap.OnMouseOver:=Minimap_Move;

    {for i:=1 to length(FontFiles) do begin
      L[i]:=fControls.AddLabel(KMPanel_Main,250,300+i*20,160,30,TKMFont(i),kaLeft,FontFiles[i]+' This is a test string for KaM Remake');
    end;//}

    //This is button to start Village functioning
    KMButtonRun:=fControls.AddButton(KMPanel_Main,20,205,50,30,36);
    KMButtonRun.OnClick:=Form1.Button1Click; //Procedure where stuff is placed on map

    KMButtonRun1:=fControls.AddButton(KMPanel_Main,80,205,50,30,'1',fnt_Outline);
    KMButtonRun1.OnClick:=Form1.Button2Click; //Procedure where stuff is placed on map

    {Main 4 buttons +return button}
    for i:=0 to 3 do begin
      KMButtonMain[i+1]:=fControls.AddButton(KMPanel_Main,  8+46*i, 372, 42, 36, 439+i);
      KMButtonMain[i+1].OnClick:=SwitchPage;
      KMButtonMain[i+1].Hint:=fTextLibrary.GetTextString(160+i);
      KMButtonMain[i+1].OnMouseOver:=DisplayHint;
    end;
    KMButtonMain[5]:=fControls.AddButton(KMPanel_Main,  8, 372, 42, 36, 443);
    KMButtonMain[5].OnClick:=SwitchPage;
    KMButtonMain[5].Hint:=fTextLibrary.GetTextString(165);
    KMButtonMain[5].OnMouseOver:=DisplayHint;

    KMLabel_Hint:=fControls.AddLabel(KMPanel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,fnt_Outline,kaLeft,'Hint');

{I plan to store all possible layouts on different pages which gets displayed one at a time}

Create_Build_Page();
Create_Ratios_Page();
Create_Stats_Page();
Create_Menu_Page();
  Create_Settings_Page();

Create_Unit_Page();
Create_House_Page();
  Create_Store_Page();
  Create_School_Page();
  Create_Barracks_Page();

SwitchPage(nil);
end;

{Build page}
procedure TKMGamePlayInterface.Create_Build_Page;
var i:integer;
begin
  KMPanel_Build:=fControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMLabel_Build:=fControls.AddLabel(KMPanel_Build,100,10,100,30,fnt_Outline,kaCenter,'Items to build');
    KMImage_Build_Selected:=fControls.AddImage(KMPanel_Build,8,40,32,32,335);
    KMImage_BuildCost_WoodPic:=fControls.AddImage(KMPanel_Build,75,40,32,32,353);
    KMImage_BuildCdost_StonePic:=fControls.AddImage(KMPanel_Build,130,40,32,32,352);
    KMLabel_BuildCost_Wood:=fControls.AddLabel(KMPanel_Build,105,50,10,30,fnt_Outline,kaLeft,'0');
    KMLabel_BuildCost_Stone:=fControls.AddLabel(KMPanel_Build,160,50,10,30,fnt_Outline,kaLeft,'0');
    KMButton_BuildRoad   := fControls.AddButtonFlat(KMPanel_Build,  8,80,33,33,335);
    KMButton_BuildField  := fControls.AddButtonFlat(KMPanel_Build, 45,80,33,33,337);
    KMButton_BuildWine   := fControls.AddButtonFlat(KMPanel_Build, 82,80,33,33,336);
    KMButton_BuildCancel := fControls.AddButtonFlat(KMPanel_Build,156,80,33,33,340);
    KMButton_BuildRoad.OnClick:=BuildButtonClick;
    KMButton_BuildField.OnClick:=BuildButtonClick;
    KMButton_BuildWine.OnClick:=BuildButtonClick;
    KMButton_BuildCancel.OnClick:=BuildButtonClick;

    for i:=1 to HOUSE_COUNT do begin
      KMButton_Build[i]:=fControls.AddButtonFlat(KMPanel_Build, 8+((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,GUIBuildIcons[i]);
      KMButton_Build[i].OnClick:=BuildButtonClick;
      KMButton_Build[i].Hint:=fTextLibrary.GetTextString(GUIBuildIcons[i]-300);
      KMButton_Build[i].OnMouseOver:=DisplayHint;
    end;
end;


{Ratios page}
procedure TKMGamePlayInterface.Create_Ratios_Page;
begin
  KMPanel_Ratios:=fControls.AddPanel(KMPanel_Main,0,412,200,400);

end;


{Statistics page}
procedure TKMGamePlayInterface.Create_Stats_Page;
var i,k,ci:integer;
begin
  KMPanel_Stats:=fControls.AddPanel(KMPanel_Main,0,412,200,400);
  ci:=0;
  for i:=1 to 11 do for k:=1 to 3 do
  if StatHouseOrder[i,k]<>ht_None then begin
    inc(ci);
    Stat_House[ci]:=fControls.AddButtonFlat(KMPanel_Stats,8+(k-1)*42,(i-1)*32,40,30,41);
    Stat_House[ci].TexOffsetX:=-4;
    Stat_HouseQty[ci]:=fControls.AddLabel(KMPanel_Stats,8+37+(k-1)*42,(i-1)*32+18,33,30,fnt_Grey,kaRight,'1');
  end;
  for i:=1 to 11 do begin
    Stat_Unit[i]:=fControls.AddButtonFlat(KMPanel_Stats,154,(i-1)*32,35,30,byte(StatUnitOrder[i])+140);
    Stat_Unit[i].TexOffsetX:=-4;
    Stat_UnitQty[i]:=fControls.AddLabel(KMPanel_Stats,154+32,(i-1)*32+18,33,30,fnt_Grey,kaRight,'3');
  end;
end;


{Menu page}
procedure TKMGamePlayInterface.Create_Menu_Page;
begin
  KMPanel_Menu:=fControls.AddPanel(KMPanel_Main,0,412,196,400);
    KMButton_Menu_Save:=fControls.AddButton(KMPanel_Menu,8,20,180,30,fTextLibrary.GetTextString(173),fnt_Metal);
    KMButton_Menu_Load:=fControls.AddButton(KMPanel_Menu,8,60,180,30,fTextLibrary.GetTextString(172),fnt_Metal);
    KMButton_Menu_Settings:=fControls.AddButton(KMPanel_Menu,8,100,180,30,fTextLibrary.GetTextString(170),fnt_Metal);
    KMButton_Menu_Settings.OnClick:=ShowSettings;
    KMButton_Menu_Exit:=fControls.AddButton(KMPanel_Menu,8,180,180,30,fTextLibrary.GetTextString(177),fnt_Metal);
end;


{Options page}
procedure TKMGamePlayInterface.Create_Settings_Page;
var i:integer;
begin
  KMPanel_Settings:=fControls.AddPanel(KMPanel_Main,0,412,200,400);
    KMLabel_Settings_Brightness:=fControls.AddLabel(KMPanel_Settings,100,10,100,30,fnt_Metal,kaCenter,fTextLibrary.GetTextString(181));
    KMButton_Settings_Dark:=fControls.AddButton(KMPanel_Settings,8,30,36,24,'-',fnt_Metal);
    KMButton_Settings_Light:=fControls.AddButton(KMPanel_Settings,154,30,36,24,'+',fnt_Metal);
    KMButton_Settings_Dark.OnMouseOver:=DisplayHint;
    KMButton_Settings_Light.OnMouseOver:=DisplayHint;
    KMButton_Settings_Dark.Hint:=fTextLibrary.GetTextString(185);
    KMButton_Settings_Light.Hint:=fTextLibrary.GetTextString(184);
    KMLabel_Settings_BrightValue:=fControls.AddLabel(KMPanel_Settings,100,34,100,30,fnt_Grey,kaCenter,'very dark');
    KMLabel_Settings_Autosave:=fControls.AddLabel(KMPanel_Settings,8,70,100,30,fnt_Metal,kaLeft,'O Autosave');
    KMLabel_Settings_Autosave.Enabled:=false;
    KMLabel_Settings_FastScroll:=fControls.AddLabel(KMPanel_Settings,8,95,100,30,fnt_Metal,kaLeft,'O Fast scrolling');
    KMLabel_Settings_MouseSpeed:=fControls.AddLabel(KMPanel_Settings,24,130,100,30,fnt_Metal,kaLeft,'Mouse speed');
    KMLabel_Settings_MouseSpeed.Enabled:=false;
    KMRatio_Settings_Mouse:=fControls.AddRatioRow(KMPanel_Settings,18,150,160,20);
    KMRatio_Settings_Mouse.Enabled:=false;
    KMRatio_Settings_Mouse.MaxValue:=fGameSettings.GetSlidersMax;
    KMRatio_Settings_Mouse.MinValue:=fGameSettings.GetSlidersMin;
    KMLabel_Settings_SFX:=fControls.AddLabel(KMPanel_Settings,24,178,100,30,fnt_Metal,kaLeft,'SFX');
    KMLabel_Settings_SFX.Enabled:=false;
    KMRatio_Settings_SFX:=fControls.AddRatioRow(KMPanel_Settings,18,198,160,20);
    KMRatio_Settings_SFX.Enabled:=false;
    KMRatio_Settings_SFX.MaxValue:=fGameSettings.GetSlidersMax;
    KMRatio_Settings_SFX.MinValue:=fGameSettings.GetSlidersMin;
    KMLabel_Settings_Music:=fControls.AddLabel(KMPanel_Settings,24,226,100,30,fnt_Metal,kaLeft,'Music');
    KMLabel_Settings_Music.Enabled:=false;
    KMRatio_Settings_Music:=fControls.AddRatioRow(KMPanel_Settings,18,246,160,20);
    KMRatio_Settings_Music.Enabled:=false;
    KMRatio_Settings_Music.MaxValue:=fGameSettings.GetSlidersMax;
    KMRatio_Settings_Music.MinValue:=fGameSettings.GetSlidersMin;
    KMLabel_Settings_Music2:=fControls.AddLabel(KMPanel_Settings,100,280,100,30,fnt_Metal,kaCenter,'Music:');
    KMButton_Settings_Music:=fControls.AddButton(KMPanel_Settings,8,300,180,30,'On/Off',fnt_Metal);
    //There are many clickable controls, so let them all be handled in one procedure to save dozens of lines of code
    for i:=1 to KMPanel_Settings.ChildCount do
      TKMControl(KMPanel_Settings.Childs[i]).OnClick:=Settings_Change;
end;


{Unit page}
procedure TKMGamePlayInterface.Create_Unit_Page;
begin
  KMPanel_Unit:=fControls.AddPanel(KMPanel_Main,0,412,200,400);
    KMLabel_UnitName:=fControls.AddLabel(KMPanel_Unit,100,30,100,30,fnt_Outline,kaCenter,'Unit name here');
    KMLabel_UnitCondition:=fControls.AddLabel(KMPanel_Unit,130,54,100,30,fnt_Grey,kaCenter,fTextLibrary.GetTextString(254));
    KMConditionBar_Unit:=fControls.AddPercentBar(KMPanel_Unit,73,69,116,15,80);
    KMLabel_UnitDescription:=fControls.AddLabel(KMPanel_Unit,8,161,236,200,fnt_Grey,kaLeft,''); //Taken from LIB resource
    KMImage_UnitPic:=fControls.AddImage(KMPanel_Unit,8,52,54,80,521);
end;


{House description page}
procedure TKMGamePlayInterface.Create_House_Page;
var i:integer;
begin
  KMPanel_House:=fControls.AddPanel(KMPanel_Main,0,412,200,400);
    //Thats common things
    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    KMLabel_House:=fControls.AddLabel(KMPanel_House,100,14,100,30,fnt_Outline,kaCenter,'');
    KMButton_House_Goods:=fControls.AddButton(KMPanel_House,9,42,30,30,37);
    KMButton_House_Goods.OnClick := fGamePlayInterface.House_WareDeliveryToggle;
    KMButton_House_Repair:=fControls.AddButton(KMPanel_House,39,42,30,30,40);
    KMButton_House_Repair.OnClick := fGamePlayInterface.House_RepairToggle;
    KMButton_House_Repair.Enabled:=false;
    KMImage_House_Logo:=fControls.AddImage(KMPanel_House,68,41,32,32,338);
    KMImage_House_Worker:=fControls.AddImage(KMPanel_House,98,41,32,32,141);
    KMLabel_HouseHealth:=fControls.AddLabel(KMPanel_House,156,45,30,50,fnt_Mini,kaCenter,fTextLibrary.GetTextString(228),$FFFFFFFF);
    KMHealthBar_House:=fControls.AddPercentBar(KMPanel_House,129,57,55,15,50,'-?-',fnt_Mini);

    KMPanel_House_Common:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_Common_Demand:=fControls.AddLabel(KMPanel_House_Common,100,2,100,30,fnt_Grey,kaCenter,'Demand');
      KMLabel_Common_Offer:=fControls.AddLabel(KMPanel_House_Common,100,2,100,30,fnt_Grey,kaCenter,'Offer');
      KMLabel_Common_Costs:=fControls.AddLabel(KMPanel_House_Common,100,2,100,30,fnt_Grey,kaCenter,'Cost');
      KMRow_Common_Resource[1] :=fControls.AddResourceRow(KMPanel_House_Common,  8,22,180,20,rt_Trunk,5);
      KMRow_Common_Resource[2] :=fControls.AddResourceRow(KMPanel_House_Common,  8,42,180,20,rt_Stone,5);
      KMRow_Common_Resource[3] :=fControls.AddResourceRow(KMPanel_House_Common,  8,62,180,20,rt_Trunk,5);
      KMRow_Common_Resource[4] :=fControls.AddResourceRow(KMPanel_House_Common,  8,82,180,20,rt_Stone,5);
      for i:=1 to 4 do begin
        KMRow_Order[i] :=fControls.AddResourceOrderRow(KMPanel_House_Common,  8,22,180,20,rt_Trunk,5);
        KMRow_Order[i].OrderRem.OnClick:=House_OrderClick;
        KMRow_Order[i].OrderRem.OnRightClick:=House_OrderClickRight;
        KMRow_Order[i].OrderAdd.OnClick:=House_OrderClick;
        KMRow_Order[i].OrderAdd.OnRightClick:=House_OrderClickRight;
      end;
      KMRow_Costs[1] :=fControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[2] :=fControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[3] :=fControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
      KMRow_Costs[4] :=fControls.AddCostsRow(KMPanel_House_Common,  8,22,180,20, 1);
end;

{Store page}
procedure TKMGamePlayInterface.Create_Store_Page;
var i:integer;
begin
    KMPanel_HouseStore:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      for i:=1 to 28 do begin
        KMButton_Store[i]:=fControls.AddButtonFlat(KMPanel_HouseStore, 8+((i-1)mod 5)*36,19+((i-1)div 5)*42,32,36,350+i);
        KMButton_Store[i].OnClick:=House_StoreAcceptFlag;
        KMButton_Store[i].Tag:=i;
        KMButton_Store[i].HideHighlight:=true;
        KMImage_Store_Accept[i]:=fControls.AddImage(KMPanel_HouseStore, 8+((i-1)mod 5)*36+9,18+((i-1)div 5)*42-11,32,36,49);
        KMImage_Store_Accept[i].FOnClick:=House_StoreAcceptFlag;
      end;
end;


{School page}
procedure TKMGamePlayInterface.Create_School_Page;
var i:integer;
begin
    KMPanel_House_School:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_School_Res:=fControls.AddLabel(KMPanel_House_School,100,2,100,30,fnt_Grey,kaCenter,fTextLibrary.GetTextString(227));
      KMResRow_School_Resource :=fControls.AddResourceRow(KMPanel_House_School,  8,22,180,20,rt_Gold,5);
      KMButton_School_UnitWIP :=fControls.AddButton(KMPanel_House_School,  8,48,32,32,0);
      KMButton_School_UnitWIPBar:=fControls.AddPercentBar(KMPanel_House_School,42,54,138,20,0);
      KMButton_School_UnitWIP.OnClick:= House_SchoolUnitRemove;
      for i:=1 to 5 do begin
        KMButton_School_UnitPlan[i]:= fControls.AddButtonFlat(KMPanel_House_School, 8+(i-1)*36,80,32,32,0);
        KMButton_School_UnitPlan[i].OnClick:= House_SchoolUnitRemove;
      end;
      KMLabel_School_Unit:=fControls.AddLabel(KMPanel_House_School,100,116,100,30,fnt_Outline,kaCenter,'');
      KMImage_School_Left :=fControls.AddImage(KMPanel_House_School,  8,136,54,80,521);
      KMImage_School_Left.Enabled := false;
      KMImage_School_Train:=fControls.AddImage(KMPanel_House_School, 70,136,54,80,522);
      KMImage_School_Right:=fControls.AddImage(KMPanel_House_School,132,136,54,80,523);
      KMImage_School_Right.Enabled := false;
      KMButton_School_Left :=fControls.AddButton(KMPanel_House_School,  8,226,54,40,35);
      KMButton_School_Train:=fControls.AddButton(KMPanel_House_School, 70,226,54,40,42);
      KMButton_School_Right:=fControls.AddButton(KMPanel_House_School,132,226,54,40,36);
      KMButton_School_Left.OnClick:=House_SchoolUnitChange;
      KMButton_School_Train.OnClick:=House_SchoolUnitChange;
      KMButton_School_Right.OnClick:=House_SchoolUnitChange;
      KMButton_School_Left.OnRightClick:=House_SchoolUnitChangeRight;
      KMButton_School_Right.OnRightClick:=House_SchoolUnitChangeRight;
end;


{Barracks page}
procedure TKMGamePlayInterface.Create_Barracks_Page;
var i:integer;
begin
    KMPanel_HouseBarracks:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      for i:=1 to 12 do
        KMButton_Barracks[i]:=fControls.AddButtonFlat(KMPanel_HouseBarracks, 8+((i-1)mod 6)*31,19+((i-1)div 6)*42,28,36,366+i);
      KMButton_Barracks[12].TexID:=154;
end;


{Should update any items changed by game (resoource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMGamePlayInterface.UpdateState;
begin
  if ShownUnit<>nil then ShowUnitInfo(ShownUnit) else
  if ShownHouse<>nil then ShowHouseInfo(ShownHouse);

  if ShownHint<>nil then DisplayHint(ShownHint,[],0,0);

  Minimap_Move(nil,[],0,0);

  if KMPanel_Build.Visible then Build_Fill(nil);
  if KMPanel_Stats.Visible then Stats_Fill(nil);
end;


procedure TKMGamePlayInterface.BuildButtonClick(Sender: TObject);
var i:integer; WasDown:boolean;
begin
  if Sender=nil then begin CursorMode.Mode:=cm_None; exit; end;

  //Memorize if button was already pressed
  WasDown := TKMButtonFlat(Sender).Down = true;

  //Release all buttons
  for i:=1 to KMPanel_Build.ChildCount do
    if KMPanel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(KMPanel_Build.Childs[i]).Down:=false;

  //Press the button if it wasn't
  if not WasDown then TKMButtonFlat(Sender).Down:=true;

  //Reset cursor and see if it needs to be changed
  CursorMode.Mode:=cm_None;
  CursorMode.Param:=0;
  KMLabel_BuildCost_Wood.Caption:='-';
  KMLabel_BuildCost_Stone.Caption:='-';

  if WasDown then exit; //Button was released
  
  if KMButton_BuildCancel.Down then begin
    CursorMode.Mode:=cm_Erase;
    KMImage_Build_Selected.TexID := 340;
  end;
  if KMButton_BuildRoad.Down then begin
    CursorMode.Mode:=cm_Road;
    KMImage_Build_Selected.TexID := 335;
    KMLabel_BuildCost_Stone.Caption:='1';
  end;
  if KMButton_BuildField.Down then begin
    CursorMode.Mode:=cm_Field;
    KMImage_Build_Selected.TexID := 337;
  end;
  if KMButton_BuildWine.Down then begin
    CursorMode.Mode:=cm_Wine;
    KMImage_Build_Selected.TexID := 336;
    KMLabel_BuildCost_Wood.Caption:='1';
  end;

  for i:=1 to HOUSE_COUNT do
  if KMButton_Build[i].Down then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Param:=GUIBuildIcons[i]-300; // -300 Thats a shortcut, I know
     KMImage_Build_Selected.TexID := GUIBuildIcons[i]; //Now update the selected icon
     KMLabel_BuildCost_Wood.Caption:=inttostr(HouseDAT[GUIBuildIcons[i]-300].WoodCost);
     KMLabel_BuildCost_Stone.Caption:=inttostr(HouseDAT[GUIBuildIcons[i]-300].StoneCost);
  end;
end;


procedure TKMGamePlayInterface.ShowSettings(Sender: TObject);
begin
SwitchPage(Sender);
Settings_Change(nil);
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
  KMImage_House_Worker.Enabled := Sender.GetHasOwner;
  KMImage_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  if (HouseInput[byte(Sender.GetHouseType)][1] in [rt_None,rt_All,rt_Warfare]) then
    KMButton_House_Goods.Enabled:=false else KMButton_House_Goods.Enabled:=true;
  if Sender.BuildingRepair then KMButton_House_Repair.TexID:=39 else KMButton_House_Repair.TexID:=40;
  if Sender.WareDelivery then KMButton_House_Goods.TexID:=37 else KMButton_House_Goods.TexID:=38;
  KMHealthBar_House.Caption:=inttostr(round(Sender.GetHealth))+'/'+inttostr(HouseDAT[byte(Sender.GetHouseType)].MaxHealth);
  KMHealthBar_House.Position:=round( Sender.GetHealth / HouseDAT[byte(Sender.GetHouseType)].MaxHealth * 100 );
  SwitchPage(KMPanel_House);

  case Sender.GetHouseType of
  ht_Store: begin
        StoreFill(nil);
        SwitchPage(KMPanel_HouseStore);
      end;

  ht_School: begin
        KMResRow_School_Resource.ResourceCount:=Sender.CheckResIn(rt_Gold);
        House_SchoolUnitChange(nil);
        SwitchPage(KMPanel_House_School);
      end;

  ht_Barracks: begin
        BarracksFill(nil);
        SwitchPage(KMPanel_HouseBarracks);
        end;
  ht_TownHall:;

  else begin

        //First thing - hide everything
        for i:=1 to KMPanel_House_Common.ChildCount do
          KMPanel_House_Common.Childs[i].Visible:=false;

        //Now show only what we need
        RowRes:=1; Line:=0; Base:=KMPanel_House_Common.Top+2;
        //Show Demand
        if HouseInput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
          KMLabel_Common_Demand.Visible:=true;
          KMLabel_Common_Demand.Top:=Base+Line*LineAdv+6;
          inc(Line);
          for i:=1 to 4 do if HouseInput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Common_Resource[RowRes].Resource:=HouseInput[byte(Sender.GetHouseType),i];
            KMRow_Common_Resource[RowRes].ResourceCount:=Sender.CheckResIn(HouseInput[byte(Sender.GetHouseType),i]);
            KMRow_Common_Resource[RowRes].Visible:=true;
            KMRow_Common_Resource[RowRes].Top:=Base+Line*LineAdv;
            inc(Line);
            inc(RowRes);
          end;
        end;
        //Show Output
        if not HousePlaceOrders[byte(Sender.GetHouseType)] then
        if HouseOutput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
          KMLabel_Common_Offer.Visible:=true;
          KMLabel_Common_Offer.Caption:=fTextLibrary.GetTextString(229)+'(x'+inttostr(HouseDAT[byte(Sender.GetHouseType)].ResProductionX)+'):';
          KMLabel_Common_Offer.Top:=Base+Line*LineAdv+6;
          inc(Line);
          for i:=1 to 4 do
          if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Common_Resource[RowRes].Resource:=HouseOutput[byte(Sender.GetHouseType),i];
            KMRow_Common_Resource[RowRes].ResourceCount:=Sender.CheckResOut(HouseOutput[byte(Sender.GetHouseType),i]);
            KMRow_Common_Resource[RowRes].Visible:=true;
            KMRow_Common_Resource[RowRes].Top:=Base+Line*LineAdv;
            inc(Line);
            inc(RowRes);
          end;
        end;
        //Show Orders
        if HousePlaceOrders[byte(Sender.GetHouseType)] then begin
          KMLabel_Common_Offer.Visible:=true;
          KMLabel_Common_Offer.Caption:=fTextLibrary.GetTextString(229)+'(x'+inttostr(HouseDAT[byte(Sender.GetHouseType)].ResProductionX)+'):';
          KMLabel_Common_Offer.Top:=Base+Line*LineAdv+6;
          inc(Line);
          for i:=1 to 4 do //Orders
          if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Order[i].Resource:=HouseOutput[byte(Sender.GetHouseType),i];
            KMRow_Order[i].ResourceCount:=Sender.CheckResOut(HouseOutput[byte(Sender.GetHouseType),i]);
            KMRow_Order[i].OrderCount:=Sender.CheckResOrder(i);
            KMRow_Order[i].Visible:=true;
            KMRow_Order[i].Top:=Base+Line*LineAdv;
            inc(Line);
          end;
          KMLabel_Common_Costs.Visible:=true;
          KMLabel_Common_Costs.Caption:=fTextLibrary.GetTextString(248);
          KMLabel_Common_Costs.Top:=Base+Line*LineAdv+6;
          inc(Line);
          for i:=1 to 4 do //Costs
          if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Costs[i].CostID:=byte(HouseOutput[byte(Sender.GetHouseType),i]);
            KMRow_Costs[i].Visible:=true;
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
  if (Sender=nil)or(not Sender.IsVisible) then begin
    ShownUnit:=nil;
    SwitchPage(nil);
    exit;
  end;
  SwitchPage(KMPanel_Unit);
  KMLabel_UnitName.Caption:=TypeToString(Sender.GetUnitType);
  KMImage_UnitPic.TexID:=520+byte(Sender.GetUnitType);
  KMConditionBar_Unit.Position:=round(Sender.GetCondition / UNIT_MAX_CONDITION * 100);
  KMLabel_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender.GetUnitType))
end;


procedure TKMGamePlayInterface.House_RepairToggle(Sender:TObject);
begin
  if ControlList.SelectedHouse = nil then exit;
  with ControlList.SelectedHouse do begin
    BuildingRepair := not BuildingRepair;
    if BuildingRepair then fGamePlayInterface.KMButton_House_Repair.TexID:=39
                      else fGamePlayInterface.KMButton_House_Repair.TexID:=40;
  end;
end;


procedure TKMGamePlayInterface.House_WareDeliveryToggle(Sender:TObject);
begin
  if ControlList.SelectedHouse = nil then exit;
  with ControlList.SelectedHouse do begin
    WareDelivery := not WareDelivery;
    if WareDelivery then fGamePlayInterface.KMButton_House_Goods.TexID:=37
                    else fGamePlayInterface.KMButton_House_Goods.TexID:=38;
    end;
end;


procedure TKMGamePlayInterface.House_OrderClick(Sender:TObject);
var i:integer;
begin
  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then ControlList.SelectedHouse.RemOrder(i);
    if Sender = KMRow_Order[i].OrderAdd then ControlList.SelectedHouse.AddOrder(i);
  end;
end;


procedure TKMGamePlayInterface.House_OrderClickRight(Sender:TObject);
var i:integer;
begin
  for i:=1 to 4 do begin
    if Sender = KMRow_Order[i].OrderRem then ControlList.SelectedHouse.RemOrder(i,10);
    if Sender = KMRow_Order[i].OrderAdd then ControlList.SelectedHouse.AddOrder(i,10);
  end;
end;


{Process click on Left-Train-Right buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitChange(Sender:TObject);
var i:byte; School:TKMHouseSchool;
begin
  School:=TKMHouseSchool(ControlList.SelectedHouse);

  if (Sender=KMButton_School_Left)and(LastSchoolUnit > 1) then dec(LastSchoolUnit);
  if (Sender=KMButton_School_Right)and(LastSchoolUnit < length(School_Order)) then inc(LastSchoolUnit);

  if Sender=KMButton_School_Train then //Add unit to training queue
    School.AddUnitToQueue(TUnitType(School_Order[LastSchoolUnit]));

  if School.UnitQueue[1]<>ut_None then
    KMButton_School_UnitWIP.TexID :=140+byte(School.UnitQueue[1])
  else
    KMButton_School_UnitWIP.TexID :=41; //Question mark

  KMButton_School_UnitWIPBar.Position:=School.UnitTrainProgress;

  for i:=1 to 5 do
    if School.UnitQueue[i+1]<>ut_None then
      KMButton_School_UnitPlan[i].TexID:=140+byte(School.UnitQueue[i+1])
    else
      KMButton_School_UnitPlan[i].TexID:=0;

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
    TKMHouseSchool(ControlList.SelectedHouse).RemUnitFromQueue(1)
  else for i:=1 to 5 do
    if Sender = KMButton_School_UnitPlan[i] then
      TKMHouseSchool(ControlList.SelectedHouse).RemUnitFromQueue(i+1);
  House_SchoolUnitChange(nil);
end;


{That small red triangle blocking delivery of goods to Storehouse}
{Resource determined by Button.Tag property}
procedure TKMGamePlayInterface.House_StoreAcceptFlag(Sender:TObject);
begin
  TKMHouseStore(ControlList.SelectedHouse).NotAcceptFlag[(Sender as TKMControl).Tag]:=
    not TKMHouseStore(ControlList.SelectedHouse).NotAcceptFlag[(Sender as TKMControl).Tag];
end;


procedure TKMGamePlayInterface.Settings_Change(Sender:TObject);
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
  KMButton_Settings_Music.Caption:='On' else KMButton_Settings_Music.Caption:='Off';
end;


{Virtually press BuildRoad button when changing page to BuildingPage or after house plan is placed}
procedure TKMGamePlayInterface.SelectRoad;
begin
  BuildButtonClick(KMButton_BuildRoad);
end;


procedure TKMGamePlayInterface.Build_Fill(Sender:TObject);
var i:integer;
begin
  for i:=1 to HOUSE_COUNT do
  if fMissionSettings.GetCanBuild(THouseType(GUIBuildIcons[i]-300)) then begin
    KMButton_Build[i].Enabled:=true;
    KMButton_Build[i].TexID:=GUIBuildIcons[i];
  end else begin
    KMButton_Build[i].Enabled:=false;
    KMButton_Build[i].TexID:=41;
  end;
end;


procedure TKMGamePlayInterface.StoreFill(Sender:TObject);
var i,Tmp:integer;
begin
  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(ControlList.SelectedHouse).ResourceCount[i];
    if Tmp=0 then KMButton_Store[i].Caption:='-'
             else KMButton_Store[i].Caption:=inttostr(Tmp);
    KMImage_Store_Accept[i].Visible := TKMHouseStore(ControlList.SelectedHouse).NotAcceptFlag[i];
  end;
end;


procedure TKMGamePlayInterface.BarracksFill(Sender:TObject);
var i,Tmp:integer;
begin
  for i:=1 to 11 do begin
    Tmp:=TKMHouseBarracks(ControlList.SelectedHouse).ResourceCount[i];
    if Tmp=0 then KMButton_Barracks[i].Caption:='-'
             else KMButton_Barracks[i].Caption:=inttostr(Tmp);
  end;
    Tmp:=TKMHouseBarracks(ControlList.SelectedHouse).RecruitsInside;
    if Tmp=0 then KMButton_Barracks[12].Caption:='-'
             else KMButton_Barracks[12].Caption:=inttostr(Tmp);
end;


procedure TKMGamePlayInterface.Stats_Fill(Sender:TObject);
var i,k,ci,Tmp:integer;
begin
  ci:=0;
  for i:=1 to 11 do for k:=1 to 3 do
  if StatHouseOrder[i,k]<>ht_None then begin
    inc(ci);
    Tmp:=fMissionSettings.GetHouseQty(StatHouseOrder[i,k]);
    if Tmp=0 then Stat_HouseQty[ci].Caption:='-' else Stat_HouseQty[ci].Caption:=inttostr(Tmp);
    if fMissionSettings.GetCanBuild(StatHouseOrder[i,k]) or (Tmp>0) then
      Stat_House[ci].TexID:=byte(StatHouseOrder[i,k])+300
    else
      Stat_House[ci].TexID:=41;
  end;
  for i:=1 to 11 do begin
    Tmp:=fMissionSettings.GetUnitQty(StatUnitOrder[i]);
    if Tmp=0 then Stat_UnitQty[i].Caption:='-' else Stat_UnitQty[i].Caption:=inttostr(Tmp);
  end;
end;



end.
