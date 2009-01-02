unit KM_GamePlayInterface;
interface
uses KM_Controls, Forms, Graphics, Windows, SysUtils, KromUtils, KromOGLUtils, Math, KM_Houses, KM_Defaults, KM_LoadLib;

type TKMGamePlayInterface = class
  private
    LastSchoolUnit:integer; //Last unit that was selected in School, global for all schools player owns
    LastBarrackUnit:integer;//Last unit that was selected in Barracks, global for all barracks player owns
    KMPanel_Main:TKMPanel;
      KMImage_1,KMImage_2,KMImage_3,KMImage_4:TKMImage; //Toolbar background
      KMButtonRun:TKMButton;                //Start Village functioning
      KMButtonMain:array[1..5]of TKMButton; //4 common buttons + Return
    KMPanel_Ratios:TKMPanel;
      //
    KMPanel_Stats:TKMPanel;
      //
    KMPanel_Build:TKMPanel;
      KMLabel_Build:TKMLabel;
      KMImage_Selected:TKMImage;
      KMButton_BuildRoad,KMButton_BuildField,KMButton_BuildWine,KMButton_BuildCancel:TKMButtonFlat;
      KMButton_Build:array[1..27]of TKMButtonFlat;
    KMPanel_Menu:TKMPanel;
      KMButton_Menu_Save,KMButton_Menu_Load,KMButton_Menu_Options,KMButton_Menu_Exit:TKMButton;
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

    KMPanel_House_Common:TKMPanel;
      KMLabel_Common_Demand,KMLabel_Common_Offer:TKMLabel;
      KMRow_Common_Resource:array[1..4]of TKMResourceBar; //4 bars is the maximum
    KMPanel_HouseStore:TKMPanel;
      //
    KMPanel_House_School:TKMPanel;
      KMLabel_School_Res:TKMLabel;
      KMButton_School_Resource:TKMResourceBar;
      KMButton_School_UnitWIP:TKMButton;
      KMButton_School_UnitWIPBar:TKMPercentBar;
      KMButton_School_UnitPlan:array[1..5]of TKMButtonFlat;
      KMLabel_School_Unit:TKMLabel;
      KMImage_School_Right,KMImage_School_Train,KMImage_School_Left:TKMImage;
      KMButton_School_Right,KMButton_School_Train,KMButton_School_Left:TKMButton;
    KMPanel_HouseBarracks:TKMPanel;
      // Much same as School 
    procedure SwitchPage(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
  public
    constructor Create;
    procedure HouseRepairToggle(Sender:TObject);
    procedure WareDeliveryToggle(Sender:TObject);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TUnitType);
    procedure SchoolUnitChange(Sender:TObject);
    procedure SchoolUnitRemove(Sender:TObject);
  end;

var
  fGamePlayInterface: TKMGamePlayInterface;

implementation
uses KM_Unit1, KM_Global_Data;

constructor TKMGamePlayInterface.Create();
var i,k,Button:integer;
begin
  LastSchoolUnit:=1;
  LastBarrackUnit:=1;
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
    KMButton_Menu_Save:=fControls.AddButton(KMPanel_Menu,10,20,180,30,'Save game',fnt_Metal);
    KMButton_Menu_Load:=fControls.AddButton(KMPanel_Menu,10,60,180,30,'Load game',fnt_Metal);
    KMButton_Menu_Options:=fControls.AddButton(KMPanel_Menu,10,100,180,30,'Options',fnt_Metal);
    KMButton_Menu_Exit:=fControls.AddButton(KMPanel_Menu,10,180,180,30,'Exit',fnt_Metal);

{Village Unit description page, no actions}
  KMPanel_Unit:=fControls.AddPanel(KMPanel_Main,0,474,200,400);
    KMLabel_UnitName:=fControls.AddLabel(KMPanel_Unit,100,30,100,30,fnt_Outline,kaCenter,'Unit name here');
    KMLabel_UnitCondition:=fControls.AddLabel(KMPanel_Unit,130,54,100,30,fnt_Grey,kaCenter,'Condition');
    KMHealthBar_Unit:=fControls.AddPercentBar(KMPanel_Unit,73,69,116,15,80);
    KMLabel_UnitDescription:=fControls.AddLabel(KMPanel_Unit,8,161,236,200,fnt_Grey,kaLeft,''); //Taken from LIB resource
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

{Child Pages}
{Common house page} //These Controls show/hide when needed, Height offset is simple +20px for each  
    KMPanel_House_Common:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_Common_Demand:=fControls.AddLabel(KMPanel_House_Common,100,2,100,30,fnt_Metal,kaCenter,'Resources:');
      KMLabel_Common_Offer:=fControls.AddLabel(KMPanel_House_Common,100,2,100,30,fnt_Metal,kaCenter,'Resources:');
      KMRow_Common_Resource[1] :=fControls.AddResourceRow(KMPanel_House_Common,  8,22,180,20,rt_Trunk,5);
      KMRow_Common_Resource[2] :=fControls.AddResourceRow(KMPanel_House_Common,  8,42,180,20,rt_Stone,5);
      KMRow_Common_Resource[3] :=fControls.AddResourceRow(KMPanel_House_Common,  8,62,180,20,rt_Trunk,5);
      KMRow_Common_Resource[4] :=fControls.AddResourceRow(KMPanel_House_Common,  8,82,180,20,rt_Stone,5);
      //KMLabel_Common_Order:=fControls.AddLabel(KMPanel_House_Common,100,122,100,30,fnt_Metal,kaCenter,'Resources:');
      //KMButton_Common_Order[1] :=fControls.AddResourceRow(KMPanel_House_Common,  8,142,180,20,rt_Trunk,5);
      //KMButton_Common_Order[2] :=fControls.AddResourceRow(KMPanel_House_Common,  8,162,180,20,rt_Stone,5);
      //KMButton_Common_Order[3] :=fControls.AddResourceRow(KMPanel_House_Common,  8,182,180,20,rt_Stone,5);
      //KMLabel_Common_Costs:=fControls.AddLabel(KMPanel_House_Common,100,202,100,30,fnt_Metal,kaCenter,'Resources:');
      //These ones can use ResourceRow for now, it can be updated to KaM looks later 
      //KMButton_Common_Costs[1] :=fControls.AddResourceRow(KMPanel_House_Common,  8,142,180,20,rt_Trunk,5);
      //KMButton_Common_Costs[2] :=fControls.AddResourceRow(KMPanel_House_Common,  8,162,180,20,rt_Stone,5);
      //KMButton_Common_Costs[3] :=fControls.AddResourceRow(KMPanel_House_Common,  8,182,180,20,rt_Stone,5);
{School page}
    KMPanel_House_School:=fControls.AddPanel(KMPanel_House,0,76,200,400);
      KMLabel_School_Res:=fControls.AddLabel(KMPanel_House_School,100,2,100,30,fnt_Metal,kaCenter,'Resources:');
      KMButton_School_Resource :=fControls.AddResourceRow(KMPanel_House_School,  8,22,180,20,rt_Gold,5);
      KMButton_School_UnitWIP :=fControls.AddButton(KMPanel_House_School,  8,48,32,32,0);
      KMButton_School_UnitWIPBar:=fControls.AddPercentBar(KMPanel_House_School,42,54,138,20,0);
      KMButton_School_UnitWIP.OnClick:= SchoolUnitRemove;
      for i:=1 to 5 do begin
        KMButton_School_UnitPlan[i]:= fControls.AddButtonFlat(KMPanel_House_School, 8+(i-1)*36,80,32,32,0);
        KMButton_School_UnitPlan[i].OnClick:= SchoolUnitRemove;
      end;
      KMLabel_School_Unit:=fControls.AddLabel(KMPanel_House_School,100,116,100,30,fnt_Outline,kaCenter,'Unit name here');
      KMImage_School_Left :=fControls.AddImage(KMPanel_House_School,  8,136,54,80,521);
      KMImage_School_Train:=fControls.AddImage(KMPanel_House_School, 70,136,54,80,522);
      KMImage_School_Right:=fControls.AddImage(KMPanel_House_School,132,136,54,80,523);
      KMButton_School_Left :=fControls.AddButton(KMPanel_House_School,  8,226,54,40,35);
      KMButton_School_Train:=fControls.AddButton(KMPanel_House_School, 70,226,54,40,42);
      KMButton_School_Right:=fControls.AddButton(KMPanel_House_School,132,226,54,40,36);
      KMButton_School_Left.OnClick:=SchoolUnitChange;
      KMButton_School_Train.OnClick:=SchoolUnitChange;
      KMButton_School_Right.OnClick:=SchoolUnitChange;

{Store page}
    KMPanel_HouseStore:=fControls.AddPanel(KMPanel_House,0,76,200,400);

{Barracks page}
    KMPanel_HouseBarracks:=fControls.AddPanel(KMPanel_House,0,76,200,400);

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
//First thing - hide all existing pages
  for i:=1 to KMPanel_House.ChildCount do
    if KMPanel_House.Childs[i] is TKMPanel then
      KMPanel_House.Childs[i].Visible:=false;

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
if Sender=KMPanel_House_Common then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_House_School then begin
  TKMPanel(Sender).Parent.Visible:=true;
  TKMPanel(Sender).Visible:=true;
end;
if Sender=KMPanel_HouseStore then begin
  TKMPanel(Sender).Parent.Visible:=true;
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
var i,Row,Base,Line:integer;
begin
  Assert(InRange(gp_HouseA+byte(Sender.GetHouseType)-1,gp_HouseA,gp_HouseZ),'THouseType-HousePages is out of range');
  KMLabel_House.Caption:=TypeToString(Sender.GetHouseType);
  KMImage_House_Logo.TexID:=300+byte(Sender.GetHouseType);
  KMImage_House_Worker.TexID:=140+integer(HouseOwnerUnit[integer(Sender.GetHouseType)]);
  KMImage_House_Worker.Enabled := Sender.GetHasOwner;
  KMImage_House_Worker.Visible := HouseOwnerUnit[integer(Sender.GetHouseType)] <> ut_None;
  if (HouseInput[byte(Sender.GetHouseType)][1] = rt_None) or (HouseInput[integer(Sender.GetHouseType)][1] = rt_All) then
    KMButton_House_Goods.Enabled:=false else KMButton_House_Goods.Enabled:=true;
  if Sender.BuildingRepair then KMButton_House_Repair.TexID:=39 else KMButton_House_Repair.TexID:=40;
  if Sender.WareDelivery then KMButton_House_Goods.TexID:=37 else KMButton_House_Goods.TexID:=38;
  SwitchPage(KMPanel_House);

  case Sender.GetHouseType of
  ht_Store: begin
        SwitchPage(KMPanel_HouseStore);
      end;

  {Fill in data for School info}
  ht_School: begin
        KMButton_School_Resource.ResourceCount:=Sender.CheckResIn(rt_Gold);
        SchoolUnitChange(nil);
        SwitchPage(KMPanel_House_School);
      end;

  ht_Barracks:;
  ht_TownHall:;

  else begin

        //First thing - hide everything
        for i:=1 to KMPanel_House_Common.ChildCount do
          KMPanel_House_Common.Childs[i].Visible:=false;

        //Now show only what we need
        Row:=1; Line:=0; Base:=KMPanel_House_Common.Top+2;
        if HouseInput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
          KMLabel_Common_Demand.Visible:=true;
          KMLabel_Common_Demand.Top:=Base+Line*20;
          inc(Line);
          for i:=1 to 4 do if HouseInput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Common_Resource[Row].Resource:=HouseInput[byte(Sender.GetHouseType),i];
            KMRow_Common_Resource[Row].ResourceCount:=Sender.CheckResIn(HouseInput[byte(Sender.GetHouseType),i]);
            KMRow_Common_Resource[Row].Visible:=true;
            KMRow_Common_Resource[Row].Top:=Base+Line*20;
            inc(Line);
            inc(Row);
          end;
        end;
        if HouseOutput[byte(Sender.GetHouseType),1] in [rt_Trunk..rt_Fish] then begin
          KMLabel_Common_Offer.Visible:=true;
          KMLabel_Common_Offer.Top:=Base+Line*20;
          inc(Line);
          for i:=1 to 4 do if HouseOutput[byte(Sender.GetHouseType),i] in [rt_Trunk..rt_Fish] then begin
            KMRow_Common_Resource[Row].Resource:=HouseOutput[byte(Sender.GetHouseType),i];
            KMRow_Common_Resource[Row].ResourceCount:=Sender.CheckResOut(HouseOutput[byte(Sender.GetHouseType),i]);
            KMRow_Common_Resource[Row].Visible:=true;
            KMRow_Common_Resource[Row].Top:=Base+Line*20;
            inc(Line);
            inc(Row);
          end;
        end;
      SwitchPage(KMPanel_House_Common);
      end;
  end;
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
  KMLabel_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender))
end;

{Process click on Left-Train-Right buttons of School}
procedure TKMGamePlayInterface.SchoolUnitChange(Sender:TObject);
var i:integer;
begin

  if (Sender=KMButton_School_Left)and(LastSchoolUnit > 1) then dec(LastSchoolUnit);
  if (Sender=KMButton_School_Right)and(LastSchoolUnit < length(School_Order)) then inc(LastSchoolUnit);

  if Sender=KMButton_School_Train then //Add unit to training queue
    TKMHouseSchool(ControlList.SelectedHouse).AddUnitToQueue(TUnitType(School_Order[byte(LastSchoolUnit)]));

  if TKMHouseSchool(ControlList.SelectedHouse).UnitQueue[1]<>ut_None then
    KMButton_School_UnitWIP.TexID :=140+byte(TKMHouseSchool(ControlList.SelectedHouse).UnitQueue[1])
  else
    KMButton_School_UnitWIP.TexID :=41;

  KMButton_School_UnitWIPBar.Position:=TKMHouseSchool(ControlList.SelectedHouse).UnitTrainProgress;

  for i:=1 to 5 do
    if TKMHouseSchool(ControlList.SelectedHouse).UnitQueue[i+1]<>ut_None then
      KMButton_School_UnitPlan[i].TexID:=140+byte(TKMHouseSchool(ControlList.SelectedHouse).UnitQueue[i+1])
    else
      KMButton_School_UnitPlan[i].TexID:=0;

  KMButton_School_Left.Enabled := LastSchoolUnit > 1;
  KMButton_School_Right.Enabled := LastSchoolUnit < length(School_Order);
  KMImage_School_Left.Visible:= KMButton_School_Left.Enabled;
  KMImage_School_Right.Visible:= KMButton_School_Right.Enabled;

  if KMButton_School_Left.Enabled then
    KMImage_School_Left.TexID:=520+byte(School_Order[byte(LastSchoolUnit)-1]);

  KMLabel_School_Unit.Caption:=TypeToString(TUnitType(School_Order[byte(LastSchoolUnit)]));
  KMImage_School_Train.TexID:=520+byte(School_Order[byte(LastSchoolUnit)]);

  if KMButton_School_Right.Enabled then
    KMImage_School_Right.TexID:=520+byte(School_Order[byte(LastSchoolUnit)+1]);

end;


{Process click on Remove-from-queue buttons of School}
procedure TKMGamePlayInterface.SchoolUnitRemove(Sender:TObject);
var i:integer;
begin
  if Sender = KMButton_School_UnitWIP then
    TKMHouseSchool(ControlList.SelectedHouse).RemUnitFromQueue(1)
  else for i:=1 to 5 do
    if Sender = KMButton_School_UnitPlan[i] then
      TKMHouseSchool(ControlList.SelectedHouse).RemUnitFromQueue(i+1);
  SchoolUnitChange(nil);
end;

end.
