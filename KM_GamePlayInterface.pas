unit KM_GamePlayInterface;
interface
uses KM_Controls, Forms, Graphics, Windows, SysUtils, KromUtils, KromOGLUtils, Math, KM_Houses, KM_Defaults;

type TKMGamePlayInterface = class
  private
    KMPanel_Main:TKMPanel;
    KMPanel_Ratios,KMPanel_Stats,KMPanel_Menu:TKMPanel;
    //KMPanel:array[0..50]of TKMPanel;      //Pages
      KMImage_1,KMImage_2,KMImage_3,KMImage_4:TKMImage; //Toolbar background
    KMButtonMain:array[1..5]of TKMButton; //4 common buttons
    KMButtonRun:TKMButton;                //Start Village functioning
    KMButton:array[1..20]of TKMButton;    //3D stone buttons
    KMLabel:array[1..20]of TKMLabel;      //Texts

    KMPanel_Build:TKMPanel;
      KMLabel_Build:TKMLabel;
      KMImage_Selected:TKMImage;
      KMButton_BuildRoad,KMButton_BuildField,KMButton_BuildWine,KMButton_BuildCancel:TKMButtonFlat;
      KMButton_Build:array[1..27]of TKMButtonFlat;
    KMPanel_Unit:TKMPanel;
      KMLabel_UnitName:TKMLabel;
      KMLabel_UnitCondition:TKMLabel;
      KMLabel_UnitDescription:TKMLabel;
      KMHealthBar_Unit:TKMPercentBar;
      KMImage_UnitScroll:TKMImage;
    KMPanel_House:TKMPanel;
      KMLabel_House:TKMLabel;
      KMButton_House_Goods,KMButton_House_Repair:TKMButton;
      KMImage_House_Logo,KMImage_House_Worker:TKMImage;
      KMHealthBar_House:TKMPercentBar;
      KMLabel_HouseCondition:TKMLabel;
    procedure SwitchPage(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
  public
    constructor Create;
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TUnitType);
    procedure HouseRepairToggle(Sender:TObject);
    procedure WareDeliveryToggle(Sender:TObject);
  end;

var
  fGamePlayInterface: TKMGamePlayInterface;

implementation
uses KM_Unit1, KM_Global_Data;

constructor TKMGamePlayInterface.Create();
var i,k,Page,Button:integer;
begin
{Parent Page for whole toolbar in-game}
  KMPanel_Main:=fControls.AddPanel(nil,0,0,224,1024);

    KMImage_1:=fControls.AddImage(KMPanel_Main,0,0,224,200,407);
    KMImage_2:=fControls.AddImage(KMPanel_Main,0,200,224,60,404);
    KMImage_3:=fControls.AddImage(KMPanel_Main,0,260,224,168,554);
    KMImage_4:=fControls.AddImage(KMPanel_Main,0,428,224,400,404);

    {for i:=1 to length(FontFiles) do begin
      L[i]:=TKMLabel.Create(50,300+i*20,160,30,TKMFont(i),kaLeft,FontFiles[i]+' This is a test string for KaM Remake');
      fControls.Add(L[i]);
    end;}

    KMLabel[6]:=fControls.AddLabel(KMPanel_Main,8,161,236,200,fnt_Grey,kaLeft,'Description of unit');

    //This is button to start Village functioning
    KMButtonRun:=fControls.AddButton(KMPanel_Main,50,205,100,40,36);
    KMButtonRun.OnClick:=Form1.Button1Click; //Procedure where stuff is placed on map

    {Main 4 buttons +return button}
    for i:=0 to 3 do begin
      KMButtonMain[i+1]:=fControls.AddButton(KMPanel_Main,  8+48*i, 428+5, 42, 36, 439+i);
      KMButtonMain[i+1].OnClick:=SwitchPage;
    end;
    KMButtonMain[5]:=fControls.AddButton(KMPanel_Main,  8, 428+5, 42, 36, 443);
    KMButtonMain[5].OnClick:=SwitchPage;
    KMButtonMain[5].Visible:=false;

{I plan to store all possible layouts on different pages which gets displayed one at a time}

{Building page}
  KMPanel_Build:=fControls.AddPanel(KMPanel_Main,0,474,200,400);

  KMLabel_Build:=fControls.AddLabel(KMPanel_Build,100,10,100,30,fnt_Metal,kaCenter,'Items to build');

  KMImage_Selected:=fControls.AddImage(KMPanel_Build,8,40,32,32,335);

  KMButton_BuildRoad   := fControls.AddButtonFlat(KMPanel_Build,  8,80,32,32,335);
  KMButton_BuildField  := fControls.AddButtonFlat(KMPanel_Build, 44,80,32,32,337);
  KMButton_BuildWine   := fControls.AddButtonFlat(KMPanel_Build, 80,80,32,32,336);
  KMButton_BuildCancel := fControls.AddButtonFlat(KMPanel_Build,152,80,32,32,340);
  KMButton_BuildRoad.OnClick:=BuildButtonClick;
  KMButton_BuildField.OnClick:=BuildButtonClick;
  KMButton_BuildWine.OnClick:=BuildButtonClick;
  KMButton_BuildCancel.OnClick:=BuildButtonClick;

  for i:=0 to 4 do for k:=0 to 4 do begin
    Button:=i*5+k+1;
    Assert(Button<=HouseCount,'Number of build buttons exceeded'); //Stick to 20 TSK buttons for now
    KMButton_Build[Button]:=fControls.AddButtonFlat(KMPanel_Build, 8+k*36,120+i*36,32,32,GUIBuildIcons[i*5+k+1]);
    KMButton_Build[Button].OnClick:=BuildButtonClick;
  end;

{Ratios page}
  KMPanel_Ratios:=fControls.AddPanel(KMPanel_Main,0,474,200,400);

{Stats page}
  KMPanel_Stats:=fControls.AddPanel(KMPanel_Main,0,474,200,400);

{Menu page}
  KMPanel_Menu:=fControls.AddPanel(KMPanel_Main,0,474,200,400);

  KMButton[1]:=fControls.AddButton(KMPanel_Menu,10,20,180,30,'Save game',fnt_Metal);
  KMButton[2]:=fControls.AddButton(KMPanel_Menu,10,60,180,30,'Load game',fnt_Metal);
  KMButton[3]:=fControls.AddButton(KMPanel_Menu,10,100,180,30,'Options',fnt_Metal);
  KMButton[4]:=fControls.AddButton(KMPanel_Menu,10,180,180,30,'Exit',fnt_Metal);

{Village Unit description page, no actions}
  KMPanel_Unit:=fControls.AddPanel(KMPanel_Main,0,474,200,400);

  KMLabel_UnitName:=fControls.AddLabel(KMPanel_Unit,100,30,100,30,fnt_Outline,kaCenter,'Unit name here');
  KMLabel_UnitCondition:=fControls.AddLabel(KMPanel_Unit,130,54,100,30,fnt_Grey,kaCenter,'Condition');
  KMHealthBar_Unit:=fControls.AddPercentBar(KMPanel_Unit,73,69,116,15,80);
  KMLabel_UnitDescription:=fControls.AddLabel(KMPanel_Unit,8,161,236,200,fnt_Grey,kaLeft,
  'Description of unit'+eol+'Line2'+eol+'Line3 '); //Should be taken from LIB resource
  KMImage_UnitScroll:=fControls.AddImage(KMPanel_Unit,8,52,54,80,521);

{House description page}
  KMPanel_House:=fControls.AddPanel(KMPanel_Main,0,474,200,400);
  //Thats common things
  //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need

  KMLabel_House:=fControls.AddLabel(KMPanel_House,100,14,100,30,fnt_Outline,kaCenter,'House name here');
  KMButton_House_Goods:=fControls.AddButton(KMPanel_House,9,42,30,30,37);
  KMButton_House_Repair:=fControls.AddButton(KMPanel_House,39,42,30,30,40);
  KMButton_House_Repair.OnClick := fGamePlayInterface.HouseRepairToggle;
  KMButton_House_Goods.OnClick := fGamePlayInterface.WareDeliveryToggle;
  KMImage_House_Logo:=fControls.AddImage(KMPanel_House,68,41,32,32,338);
  KMImage_House_Worker:=fControls.AddImage(KMPanel_House,98,41,32,32,141);
  KMHealthBar_House:=fControls.AddPercentBar(KMPanel_House,129,57,55,15,100,'550/550',fnt_Mini); //Not the correct color. Font color will have to be added to the percentage bar
  KMLabel_HouseCondition:=fControls.AddLabel(KMPanel_House,156,45,30,50,fnt_Mini,kaCenter,'Condition:',$FFFF00FF);

SwitchPage(nil);
end;


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
begin
//Reset the CursorMode, to cm_None
BuildButtonClick(nil);

//First thing - hide all existing pages
  for i:=1 to KMPanel_Main.ChildCount do
    if KMPanel_Main.Childs[i] is TKMPanel then
      KMPanel_Main.Childs[i].Visible:=false;

//If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
if Sender=KMButtonMain[1] then begin
  KMPanel_Build.Visible:=true;
  Hide4MainButtons;
end else
if Sender=KMButtonMain[2] then begin
  KMPanel_Ratios.Visible:=true;
  Hide4MainButtons;
end else
if Sender=KMButtonMain[3] then begin
  KMPanel_Stats.Visible:=true;
  Hide4MainButtons;
end else
if Sender=KMButtonMain[4] then begin
  KMPanel_Menu.Visible:=true;
  Hide4MainButtons;
end else begin //If Sender is anything else - then show all 4 buttons and hide Return button
  for i:=1 to 4 do
    KMButtonMain[i].Visible:=true;
  KMButtonMain[5].Visible:=false;
end;

//Now process all other kinds of pages
if Sender=KMPanel_Unit then begin
  TKMPanel(Sender).Visible:=true;
end else
if Sender=KMPanel_House then begin
  TKMPanel(Sender).Visible:=true;
end;
end;


procedure TKMGamePlayInterface.BuildButtonClick(Sender: TObject);
var i:integer; WasDown:boolean;
begin
  if Sender=nil then begin CursorMode.Mode:=cm_None; exit; end;

  //Memorize if button was already pressed
  WasDown := TKMButtonFlat(Sender).Checked = true;

  //Release all buttons
  for i:=1 to KMPanel_Build.ChildCount do
    if KMPanel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(KMPanel_Build.Childs[i]).Checked:=false;

  //Press the button if it wasn't
  if not WasDown then TKMButtonFlat(Sender).Checked:=true;

  //Reset cursor and see if it needs to be changed
  CursorMode.Mode:=cm_None;
  CursorMode.Param:=0;
  if WasDown then exit; //Button was released
  if KMButton_BuildCancel.Checked then CursorMode.Mode:=cm_Erase;
  if KMButton_BuildRoad.Checked then CursorMode.Mode:=cm_Road;
  if KMButton_BuildField.Checked then CursorMode.Mode:=cm_Field;
  if KMButton_BuildWine.Checked then CursorMode.Mode:=cm_Wine;

  for i:=1 to HouseCount do
  if KMButton_Build[i].Checked then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Param:=GUIBuildIcons[i]-300; // -300 Thats a shortcut, I know
  end;

end;


procedure TKMGamePlayInterface.HouseRepairToggle(Sender:TObject);
begin
  if ControlList.SelectedHouse <> nil then
    with ControlList.SelectedHouse do begin
      if BuildingRepair = true then BuildingRepair := false else BuildingRepair := true;
      //I'd like to have it working this way though
      //BuildingRepair := not BuildingRepair;
      if BuildingRepair then fGamePlayInterface.KMButton_House_Repair.TexID:=39 else fGamePlayInterface.KMButton_House_Repair.TexID:=40;
    end;
end;  

procedure TKMGamePlayInterface.WareDeliveryToggle(Sender:TObject);
begin
  if ControlList.SelectedHouse <> nil then
    with ControlList.SelectedHouse do begin
      if WareDelivery = true then WareDelivery := false else WareDelivery := true;
      if WareDelivery then fGamePlayInterface.KMButton_House_Goods.TexID:=37 else fGamePlayInterface.KMButton_House_Goods.TexID:=38;
    end;
end;

procedure TKMGamePlayInterface.ShowHouseInfo(Sender:TKMHouse);
begin
  Assert(InRange(gp_HouseA+byte(Sender.GetHouseType)-1,gp_HouseA,gp_HouseZ),'THouseType-HousePages is out of range');
  SwitchPage(KMPanel_House);
  KMLabel_House.Caption:=TypeToString(Sender.GetHouseType);
  KMImage_House_Logo.TexID:=300+byte(Sender.GetHouseType);
  KMImage_House_Worker.TexID:=140+integer(HouseOwnerUnit[integer(Sender.GetHouseType)]);
  KMImage_House_Worker.Enabled := Sender.GetHasOwner;
  KMImage_House_Worker.Visible := HouseOwnerUnit[integer(Sender.GetHouseType)] <> ut_None;
  if (HouseInput[integer(Sender.GetHouseType)][1] = rt_None) or (HouseInput[integer(Sender.GetHouseType)][1] = rt_All) then
    KMButton_House_Goods.Enabled:=false else KMButton_House_Goods.Enabled:=true;
  if Sender.BuildingRepair then KMButton_House_Repair.TexID:=39 else KMButton_House_Repair.TexID:=40;
  if Sender.WareDelivery then KMButton_House_Goods.TexID:=37 else KMButton_House_Goods.TexID:=38;

  //process common properties - done :-)
  //if has demand - list it
  //if has product - list it
  //if has order placement - list it
  //if has production costs - list them
  //Process special houses - Store, Barracks, School
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender:TUnitType);
begin
  SwitchPage(KMPanel_Unit);
  KMLabel_UnitName.Caption:=TypeToString(Sender);
  KMImage_UnitScroll.TexID:=520+integer(Sender);
end;

end.
