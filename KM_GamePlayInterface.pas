unit KM_GamePlayInterface;
interface
uses KM_Controls, Forms, Graphics, Windows, SysUtils, KromUtils, KromOGLUtils, Math, KM_Houses, KM_Defaults;

type TKMGamePlayInterface = class
  private
    KMPanel:array[0..50]of TKMPanel;      //Pages
    KMButtonMain:array[1..5]of TKMButton; //4 common buttons
    KMButtonRun:TKMButton;                //Start Village functioning
    KMButton:array[1..20]of TKMButton;    //3D stone buttons
    KMButtonFlat:array[1..40]of TKMButtonFlat; //Flat build buttons
    KMLabel:array[1..20]of TKMLabel;      //Texts
    //Or maybe it's better to store everything as Form1 does for its elements
    //Just make a huge list of KMControls in here
    KMLabel_UnitCondition:TKMLabel;
    KMHealthBar_Unit:TKMPercentBar;
    KMImage:array[1..20]of TKMImage;      //Images
    procedure SwitchPage(Sender: TObject);
  public
    constructor Create;
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TUnitType);
  end;

var
  fGamePlayInterface: TKMGamePlayInterface;

implementation
uses KM_Unit1, KM_Global_Data;

constructor TKMGamePlayInterface.Create();
var i,k,Page,Button:integer;
begin
{Parent Page for whole toolbar in-game}
  KMPanel[gp_ToolBar]:=fControls.AddPanel(nil,0,0,224,1024);

    KMImage[1]:=fControls.AddImage(KMPanel[gp_ToolBar],0,0,224,200,407);
    KMImage[2]:=fControls.AddImage(KMPanel[gp_ToolBar],0,200,224,60,404);
    KMImage[3]:=fControls.AddImage(KMPanel[gp_ToolBar],0,260,224,168,554);
    KMImage[4]:=fControls.AddImage(KMPanel[gp_ToolBar],0,428,224,400,404);

    {for i:=1 to length(FontFiles) do begin
      L[i]:=TKMLabel.Create(50,300+i*20,160,30,TKMFont(i),kaLeft,FontFiles[i]+' This is a test string for KaM Remake');
      fControls.Add(L[i]);
    end;}

    //This is button to start Village functioning
    KMButtonRun:=fControls.AddButton(KMPanel[gp_ToolBar],50,205,100,40,36);
    KMButtonRun.OnClick:=Form1.Button1Click; //Procedure where stuff is placed on map

    {Main 4 buttons +return button}
    for i:=0 to 3 do begin
      KMButtonMain[i+1]:=fControls.AddButton(KMPanel[gp_ToolBar],  8+48*i, 428+5, 42, 36, 439+i);
      KMButtonMain[i+1].OnClick:=SwitchPage;
    end;
    KMButtonMain[5]:=fControls.AddButton(KMPanel[gp_ToolBar],  8, 428+5, 42, 36, 443);
    KMButtonMain[5].OnClick:=SwitchPage;
    KMButtonMain[5].Visible:=false;

{I plan to store all possible layouts on different pages which gets displayed one at a time}

{Building page}
  Page:=gp_Build;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

  KMLabel[1]:=fControls.AddLabel(KMPanel[Page],100,10,100,30,fnt_Metal,kaCenter,'Items   to   build');

  KMButtonFlat[gb_Road]   := fControls.AddButtonFlat(KMPanel[Page],  8,80,32,32,335);
  KMButtonFlat[gb_Field]  := fControls.AddButtonFlat(KMPanel[Page], 44,80,32,32,337);
  KMButtonFlat[gb_Wine]   := fControls.AddButtonFlat(KMPanel[Page], 80,80,32,32,336);
  KMButtonFlat[gb_Cancel] := fControls.AddButtonFlat(KMPanel[Page],152,80,32,32,340);

  for i:=0 to 4 do for k:=0 to 4 do begin
    Button:=gb_BuildItemA+i*5+k;
    Assert(Button<=gb_BuildItemZ,'Number of build buttons exceeded'); //Stick to 20 TSK buttons for now
    KMButtonFlat[Button]:=fControls.AddButtonFlat(KMPanel[Page], 8+k*36,120+i*36,32,32,GUIBuildIcons[i*5+k+1]);
  end;

{Ratios page}
  Page:=gp_Ratios;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

{Stats page}
  Page:=gp_Stats;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

{Menu page}
  Page:=gp_Menu;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

  KMButton[1]:=fControls.AddButton(KMPanel[Page],10,20,180,30,'Save game',fnt_Metal);
  KMButton[2]:=fControls.AddButton(KMPanel[Page],10,60,180,30,'Load game',fnt_Metal);
  KMButton[3]:=fControls.AddButton(KMPanel[Page],10,100,180,30,'Options',fnt_Metal);
  KMButton[4]:=fControls.AddButton(KMPanel[Page],10,180,180,30,'Exit',fnt_Metal);

{Unit description page}
  Page:=gp_Unit;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

  KMLabel[gl_UnitName]:=fControls.AddLabel(KMPanel[Page],100,10,100,30,fnt_Metal,kaCenter,'Unit name here');
  KMLabel[gl_UnitCondition]:=fControls.AddLabel(KMPanel[Page],80,40,100,30,fnt_Metal,kaLeft,'Condition');
  KMHealthBar_Unit:=fControls.AddPercentBar(KMPanel[Page],80,60,100,20,80);
  KMLabel_UnitCondition:=fControls.AddLabel(KMPanel[Page],80,80,100,30,fnt_Metal,kaLeft,'Condition');

{House description pages}
{  Page:=gp_Unit;
  KMPanel[Page]:=fControls.AddPanel(KMPanel[gp_ToolBar],0,474,200,400);
  KMPanel[Page].Visible:=false;

  KMLabel[gl_UnitName]:=fControls.AddLabel(KMPanel[Page],100,10,100,30,fnt_Metal,kaCenter,'Gets replaced');  }
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var i:integer;
begin
//First thing - hide all existing pages and then show one we need now
  for i:=1 to length(KMPanel)-1 do
    if Assigned(KMPanel[i]) then
      KMPanel[i].Visible:=false;

//If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
if (Sender=KMButtonMain[1])or(Sender=KMButtonMain[2])or(Sender=KMButtonMain[3])or(Sender=KMButtonMain[4]) then begin
  for i:=1 to 4 do begin
    KMPanel[i].Visible:= Sender=KMButtonMain[i];
    KMButtonMain[i].Visible:=false;
  end;
  KMButtonMain[5].Visible:=true;
//If Sender is anything else - then show all 4 buttons and hide Return button
end else begin
  for i:=1 to 4 do
    KMButtonMain[i].Visible:=true;
  KMButtonMain[5].Visible:=false;
end;
//Now process all other kinds of pages
if Sender=KMPanel[gp_Unit] then begin
  TKMPanel(Sender).Visible:=true;
end;
end;


procedure TKMGamePlayInterface.ShowHouseInfo(Sender:TKMHouse);
begin
  Assert(InRange(gp_HouseA+byte(Sender.GetHouseType)-1,gp_HouseA,gp_HouseZ),'THouseType-HousePages is out of range');
  if not Assigned(KMPanel[gp_HouseA+byte(Sender.GetHouseType)-1]) then
    SwitchPage(KMPanel[1]);
  SwitchPage(KMPanel[gp_HouseA+byte(Sender.GetHouseType)-1]);
  //Here should be variable stuff, like resources count and etc.. for specific house

end;

procedure TKMGamePlayInterface.ShowUnitInfo(Sender:TUnitType);
begin
  SwitchPage(KMPanel[gp_Unit]);
  KMLabel[gl_UnitName].Caption:=TypeToString(Sender);
end;

end.
