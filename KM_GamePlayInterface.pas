unit KM_GamePlayInterface;
interface
uses KM_Defaults, KM_Houses, Forms, Graphics, Windows, SysUtils, KromUtils, KromOGLUtils, KM_Controls, KM_Global_Data;

procedure InitGUIControls();
procedure SwitchPage(SenderNil: TObject; Sender: TObject);
procedure ShowHouseInfo(KMHouse:TKMHouse);

implementation
uses KM_Unit1;

var
  KMButtonMain:array[1..5]of TKMButton;
  KMButtonRun:TKMButton;
  KMButton:array[1..20]of TKMButton;
  KMButtonFlat:array[1..20]of TKMButtonFlat;
  KMLabel:array[1..20]of TKMLabel;
  KMImage:array[1..20]of TKMImage;
  KMPanel:array[1..20]of TKMPanel;

procedure InitGUIControls();
var i,Page,Button:integer;
begin

KMImage[1]:=TKMImage.Create(0,0,224,200,407);
fControls.Add(KMImage[1]);
KMImage[2]:=TKMImage.Create(0,200,224,60,404);
fControls.Add(KMImage[2]);
KMImage[3]:=TKMImage.Create(0,260,224,168,554);
fControls.Add(KMImage[3]);
KMImage[4]:=TKMImage.Create(0,428,224,400,404);
fControls.Add(KMImage[4]);

{for i:=1 to length(FontFiles) do begin
  L[i]:=TKMLabel.Create(50,300+i*20,160,30,TKMFont(i),kaLeft,FontFiles[i]+' This is a test string for KaM Remake');
  fControls.Add(L[i]);
end;}

KMButtonRun:=TKMButton.Create(50,205,100,40,36);
fControls.Add(KMButtonRun);
KMButtonRun.OnClick:=Form1.Button1Click;

{Main 4 buttons +return button}
for i:=0 to 3 do begin
  KMButtonMain[i+1]:=TKMButton.Create(  8+48*i, 428+5, 42, 36, 439+i);
  fControls.Add(KMButtonMain[i+1]); @KMButtonMain[i+1].OnClick:=@SwitchPage;
end;
KMButtonMain[5]:=TKMButton.Create(  8, 428+5, 42, 36, 443);
fControls.Add(KMButtonMain[5]); @KMButtonMain[5].OnClick:=@SwitchPage;
KMButtonMain[5].Visible:=false;

{Building page}
  Page:=byte(gp_Build);
  KMPanel[Page]:=TKMPanel.Create(0,474,224,400);
  fControls.Add(KMPanel[Page]);
  KMPanel[Page].Visible:=false;

  KMLabel[1]:=TKMLabel.Create(100,10,100,30,fnt_Metal,kaCenter,'Items   to   build');
  fControls.Add(KMLabel[1]);
  KMLabel[1].ParentTo(KMPanel[Page]);

  Button:=byte(gb_Road);
  KMButtonFlat[Button]:=TKMButtonFlat.Create(  8,60,32,32,335);
  fControls.Add(KMButtonFlat[Button]);
  KMButtonFlat[Button].ParentTo(KMPanel[Page]);
  Button:=byte(gb_Field);
  KMButtonFlat[Button]:=TKMButtonFlat.Create( 44,60,32,32,337);
  fControls.Add(KMButtonFlat[Button]);
  KMButtonFlat[Button].ParentTo(KMPanel[Page]);
  Button:=byte(gb_Wine);
  KMButtonFlat[Button]:=TKMButtonFlat.Create( 80,60,32,32,336);
  fControls.Add(KMButtonFlat[Button]);
  KMButtonFlat[Button].ParentTo(KMPanel[Page]);
  Button:=byte(gb_Cancel);
  KMButtonFlat[Button]:=TKMButtonFlat.Create(152,60,32,32,340);
  fControls.Add(KMButtonFlat[Button]);
  KMButtonFlat[Button].ParentTo(KMPanel[Page]);

{Ratios page}
  Page:=byte(gp_Ratios);
  KMPanel[Page]:=TKMPanel.Create(0,474,224,400);
  fControls.Add(KMPanel[Page]);
  KMPanel[Page].Visible:=false;

{Stats page}
  Page:=byte(gp_Stats);
  KMPanel[Page]:=TKMPanel.Create(0,474,224,400);
  fControls.Add(KMPanel[Page]);
  KMPanel[Page].Visible:=false;

{Menu page}
  Page:=byte(gp_Menu);
  KMPanel[Page]:=TKMPanel.Create(0,474,224,400);
  fControls.Add(KMPanel[Page]);
  KMPanel[Page].Visible:=false;

  KMButton[1]:=TKMButton.Create(10,100,180,30,'Save game',fnt_Metal);
  fControls.Add(KMButton[1]);
  KMButton[1].ParentTo(KMPanel[Page]);
  KMButton[2]:=TKMButton.Create(10,140,180,30,'Load game',fnt_Metal);
  fControls.Add(KMButton[2]);
  KMButton[2].ParentTo(KMPanel[Page]);
  KMButton[3]:=TKMButton.Create(10,180,180,30,'Options',fnt_Metal);
  fControls.Add(KMButton[3]);
  KMButton[3].ParentTo(KMPanel[Page]);
  KMButton[4]:=TKMButton.Create(10,260,180,30,'Exit',fnt_Metal);
  fControls.Add(KMButton[4]);
  KMButton[4].ParentTo(KMPanel[Page]);
end;

{Switch between pages}
procedure SwitchPage(SenderNil: TObject; Sender: TObject);
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


procedure ShowHouseInfo(KMHouse:TKMHouse);
var i:integer;
begin
  KMLabel[1].Caption:=TypeToString(KMHouse.GetHouseType);
  case KMHouse.GetHouseType of
    ht_Store:
      begin

      

      end;
    ht_Barracks:;
    ht_School:;
  else begin
    Form1.Label3.Caption:=TypeToString(HouseOutput[byte(KMHouse.GetHouseType),1]);
    Form1.Image2.Canvas.FillRect(Form1.Image2.ClientRect);
    for i:=1 to KMHouse.CheckResOut(HouseOutput[byte(KMHouse.GetHouseType),1]) do
      Form1.IL_ResourceIcons.Draw(Form1.Image2.Canvas,4+i*20,4,byte(HouseOutput[byte(KMHouse.GetHouseType),1])-1);
  end;
  end;
end;

end.
 