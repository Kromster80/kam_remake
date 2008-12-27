unit KM_GamePlayInterface;
interface
uses KM_Defaults, KM_Controls, Forms, Graphics, Windows, SysUtils, KromUtils, KromOGLUtils;

type TKMGamePlayInterface = class
  private
    //Self:TKMGamePlayInterface;
    KMButtonMain:array[1..5]of TKMButton;
    KMButtonRun:TKMButton;
    KMButton:array[1..20]of TKMButton;
    KMButtonFlat:array[1..40]of TKMButtonFlat;
    KMLabel:array[1..20]of TKMLabel;
    KMImage:array[1..20]of TKMImage;
    KMPanel:array[0..20]of TKMPanel;

    procedure SwitchPage(Sender: TObject);
  public
    constructor Create;
    procedure ShowHouseInfo(Sender:THouseType);
  end;

implementation
uses KM_Unit1, KM_Global_Data, KM_Houses;

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
    Assert(Button<=gb_BuildItemZ,'Number of build buttons exceeded');
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
end;

{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var i:integer;
begin
if Sender=KMButtonMain[5] then begin
  for i:=1 to 4 do begin
    KMPanel[i].Visible:=false;
    KMButtonMain[i].Visible:=true;
  end;
  KMButtonMain[5].Visible:=false;
end else begin
  for i:=1 to 4 do begin
    KMPanel[i].Visible:= Sender=KMButtonMain[i];
    KMButtonMain[i].Visible:=false;
  end;
  KMButtonMain[5].Visible:=true;
end;
end;


procedure TKMGamePlayInterface.ShowHouseInfo(Sender:THouseType);
begin
  KMLabel[1].Caption:=TypeToString(Sender);
end;

end.
