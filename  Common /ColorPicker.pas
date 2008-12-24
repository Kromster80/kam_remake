unit ColorPicker;

interface

uses
  SysUtils, Classes, Graphics, Forms, Spin, StdCtrls, ExtCtrls, Controls, Math, KromUtils,
  FloatSpinEdit;

type
  TForm_ColorPicker = class(TForm)
    Shape2: TShape;
    HSImage: TImage;
    BriImage: TImage;
    Ticker: TShape;
    Shape1: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button1: TButton;
    SpinR: TFloatSpinEdit;
    SpinG: TFloatSpinEdit;
    SpinB: TFloatSpinEdit;
    SpinH: TFloatSpinEdit;
    SpinS: TFloatSpinEdit;
    SpinBr: TFloatSpinEdit;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure ApplyHue2RGB(InHue:integer; var R,G,B:integer);
    procedure ApplySat2RGB(InSat:integer; var R,G,B:integer);
    procedure ApplyBri2RGB(inR,inG,inB,InBri:integer; var R,G,B:integer);
    procedure DrawHueSatQuad();
    procedure DrawBriRow();
    procedure DisplayResultColor(Sender:string);
    procedure ConvertRGB2HSB(Rin,Gin,Bin:integer; var Hout,Sout,Bout:integer);
    procedure ConvertHSB2RGB(H_in,S_in,B_in:integer; var R,G,B:integer);
    procedure HSImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HSImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HSImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure BriImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BriImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BriImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SpinRGBChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PositionHSBCursors();
    procedure SpinHSBChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure DefineInputColor(R,G,B:byte; Sender:TObject); overload;
procedure DefineInputColor(RGB:TColor; Sender:TObject); overload;
procedure DefineInputColorRGB(R,G,B:byte; Sender:TObject);

var
  Form_ColorPicker: TForm_ColorPicker;
  SenderShape:TShape;
  BitmapHueSat,BitmapBri:Tbitmap;
  SpyMouseH:boolean=false;
  SpyMouseS:boolean=false;
  SpyMouseB:boolean=false;
  Hue,Sat,Bri:integer; //0..359, 0..255, 0..255
  InputR,InputG,InputB:integer;
  RGBRefresh:boolean;
  HSBRefresh:boolean;

implementation


{$R *.dfm}

procedure TForm_ColorPicker.FormShow(Sender: TObject);
begin
BitmapHueSat:=Tbitmap.Create;
BitmapHueSat.PixelFormat:=pf24bit;
BitmapHueSat.Width:=HSImage.Width;;
BitmapHueSat.Height:=HSImage.Height;
BitmapBri:=Tbitmap.Create;
BitmapBri.PixelFormat:=pf24bit;
BitmapBri.Width:=1;
BitmapBri.Height:=BriImage.Height;
end;   

//This is wrap to acquire data in different formats and convert them to internal R,G,B
//Required since we can't make a call to overloaded procedure itself
procedure DefineInputColor(R,G,B:byte; Sender:TObject); overload;
begin
DefineInputColorRGB(R,G,B, Sender);
end;

//This is wrap for TColor
procedure DefineInputColor(RGB:TColor; Sender:TObject); overload;
begin
DefineInputColorRGB(RGB AND $FF, RGB AND $FF00 SHR 8, RGB AND $FF0000 SHR 16, Sender);
end;

procedure DefineInputColorRGB(R,G,B:byte; Sender:TObject);
begin
if Sender<>nil then SenderShape:=(Sender as TShape);
with Form_ColorPicker do begin
  if Sender<>nil then Show;
  InputR:=R; InputG:=G; InputB:=B;//Keep input RGB incase user wants to cancel
  ConvertRGB2HSB(R,G,B,Hue,Sat,Bri);
  PositionHSBCursors();
  DrawHueSatQuad();
  DrawBriRow();
  DisplayResultColor('Both');
end;
end;

procedure TForm_ColorPicker.DrawHueSatQuad();
var P : PByteArray; R,G,B:integer; ii,kk:integer;
begin //Fill area with Hue and Saturation data respecting Brightness
for ii:=0 to 255 do begin
P:=BitmapHueSat.ScanLine[ii];
for kk:=0 to 359 do begin
ApplyHue2RGB(kk, R,G,B);
ApplySat2RGB(ii, R,G,B);
ApplyBri2RGB(R,G,B, Bri, R,G,B);
P[kk*3+0]:=B;
P[kk*3+1]:=G;
P[kk*3+2]:=R;
end;
end;
HSImage.Canvas.Draw(0,0,BitmapHueSat);
end;

procedure TForm_ColorPicker.DrawBriRow();
var R,G,B,Rt,Gt,Bt:integer; i:integer;
begin
Hue:=Shape1.Left-HSImage.Left+Shape1.Width div 2; //restore after cycle
Sat:=Shape1.Top-HSImage.Top+Shape1.Height div 2;
ApplyHue2RGB(Hue, R,G,B);
ApplySat2RGB(Sat, R,G,B);
for i:=0 to 255 do begin
ApplyBri2RGB(R,G,B,i,Rt,Gt,Bt);
BitmapBri.Canvas.Pixels[0,i]:=Rt+Gt*256+Bt*65536;
end;
Bri:=Ticker.Top-BriImage.Top+(Ticker.Height div 2);
BriImage.Canvas.StretchDraw(BriImage.Canvas.ClipRect,BitmapBri);
end;

procedure TForm_ColorPicker.ApplyHue2RGB(InHue:integer; var R,G,B:integer);
var V:single;
begin V:=255/(360 div 6);
EnsureRange(InHue,0,359);
case InHue of
0..59   :begin R:=255;                  G:=round(InHue*v);       B:=0;                   end;
60..119 :begin R:=round((120-InHue)*v); G:=255;                  B:=0;                   end;
120..179:begin R:=0;                    G:=255;                  B:=round((InHue-120)*v);end;
180..239:begin R:=0;                    G:=round((240-InHue)*v); B:=255;                 end;
240..299:begin R:=round((InHue-240)*v); G:=0;                    B:=255;                 end;
300..359:begin R:=255;                  G:=0;                    B:=round((360-InHue)*v);end;
end;
end;

procedure TForm_ColorPicker.ApplySat2RGB(InSat:integer; var R,G,B:integer);
begin
R:=round((R*(255-InSat)+127*(InSat))/255);
G:=round((G*(255-InSat)+127*(InSat))/255);
B:=round((B*(255-InSat)+127*(InSat))/255);
end;

procedure TForm_ColorPicker.ApplyBri2RGB(inR,inG,inB,InBri:integer; var R,G,B:integer);
begin
if InBri<127 then begin
R:=round((inR*InBri+255*(127-InBri))/127);
G:=round((inG*InBri+255*(127-InBri))/127);
B:=round((inB*InBri+255*(127-InBri))/127);
end else
if InBri>127 then begin
R:=round((inR*(255-InBri)+0*(InBri-127))/127);
G:=round((inG*(255-InBri)+0*(InBri-127))/127);
B:=round((inB*(255-InBri)+0*(InBri-127))/127);
end else
end;

procedure TForm_ColorPicker.ConvertRGB2HSB(Rin,Gin,Bin:integer; var Hout,Sout,Bout:integer);
var Rdel,Gdel,Bdel,Vmin,Vmax,Vdel,xp:integer;
begin
Vmin:=min(Rin,Gin,Bin);
Vmax:=max(Rin,Gin,Bin);
Vdel:=Vmax-Vmin;
Bout:=255-round((Vmax+Vmin)/2);
if Vdel=0 then begin Hout:=180; Sout:=255; end else begin//Middle of HSImage
if Bout>=127 then Sout:=255-round(Vdel/(Vmax+Vmin)*255) //including 127
             else Sout:=255-round(Vdel/(511-Vmax-Vmin)*255);

Rdel:=round((Rin-Vmin)*255/Vdel);
Gdel:=round((Gin-Vmin)*255/Vdel);
Bdel:=round((Bin-Vmin)*255/Vdel);
if Rin=Vmax then xp:=round((Gdel-Bdel)/255*60) else
if Gin=Vmax then xp:=round(120-(Rdel-Bdel)/255*60) else
if Bin=Vmax then xp:=round(240-(Gdel-Rdel)/255*60) else xp:=0;
if xp<0 then inc(xp,360);
if xp>360 then dec(xp,360);
Hout:=xp;
end;
end;

procedure TForm_ColorPicker.ConvertHSB2RGB(H_in,S_in,B_in:integer; var R,G,B:integer);
begin
ApplyHue2RGB(H_in, R,G,B);
ApplySat2RGB(S_in, R,G,B);
ApplyBri2RGB(R,G,B,B_in,R,G,B);
end;

procedure TForm_ColorPicker.DisplayResultColor(Sender:string);
var R,G,B,Ht,St,Bt:integer;
begin
RGBRefresh:=true;
HSBRefresh:=true;
if Sender='RGB' then begin
R:=EnsureRange(round(SpinR.Value),0,255);
G:=EnsureRange(round(SpinG.Value),0,255);
B:=EnsureRange(round(SpinB.Value),0,255);
ConvertRGB2HSB(R,G,B,Ht,St,Bt);
SpinH.Value:=Hue;
SpinS.Value:=255-Sat;
SpinBr.Value:=255-Bri;
end;
if Sender='HSB' then begin
ConvertHSB2RGB(Hue,Sat,Bri,R,G,B);
SpinR.Value:=R;
SpinG.Value:=G;
SpinB.Value:=B;
end;
if Sender='Both' then begin
ConvertHSB2RGB(Hue,Sat,Bri,R,G,B);
SpinR.Value:=R;
SpinG.Value:=G;
SpinB.Value:=B;
SpinH.Value:=Hue;
SpinS.Value:=255-Sat;
SpinBr.Value:=255-Bri;
end;
Shape2.Brush.Color:=round(R)+round(G)*256+round(B)*65536;
SenderShape.Brush.Color:=round(R)+round(G)*256+round(B)*65536;
SenderShape.OnDragDrop(nil,nil,0,0);
RGBRefresh:=false;
HSBRefresh:=false;
end;

procedure TForm_ColorPicker.HSImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin
SpyMouseH:=true;
HSImageMouseMove(nil,Shift,X,Y);
end;

procedure TForm_ColorPicker.HSImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
if not SpyMouseH then exit;
Hue:=EnsureRange(X,0,359);
Sat:=EnsureRange(Y,0,255);
Shape1.Left:=HSImage.Left+Hue-Shape1.Width div 2;// - Shape1.Width mod 2;
Shape1.Top:=HSImage.Top+Sat-Shape1.Height div 2;// - Shape1.Width mod 2;
DrawBriRow();
DisplayResultColor('Both');
end;

procedure TForm_ColorPicker.HSImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin
SpyMouseH:=false;
end;

procedure TForm_ColorPicker.BriImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin
SpyMouseB:=true;
BriImageMouseMove(nil,Shift,X,Y);
end;

procedure TForm_ColorPicker.BriImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin
if not SpyMouseB then exit;
Bri:=EnsureRange(Y,0,BriImage.Height-1);
Ticker.Top:=BriImage.Top+Bri-(Ticker.Height div 2);
DisplayResultColor('Both');
end;

procedure TForm_ColorPicker.BriImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin
SpyMouseB:=false;
DrawHueSatQuad();
end;

procedure TForm_ColorPicker.SpinRGBChange(Sender: TObject);
var R,G,B:integer;
begin
if RGBRefresh then exit;
RGBRefresh:=true;
R:=EnsureRange(round(SpinR.Value),0,255);
G:=EnsureRange(round(SpinG.Value),0,255);
B:=EnsureRange(round(SpinB.Value),0,255);
RGBRefresh:=false;
ConvertRGB2HSB(R,G,B,Hue,Sat,Bri);
PositionHSBCursors();
DrawHueSatQuad();
DrawBriRow();
DisplayResultColor('RGB');
end;

procedure TForm_ColorPicker.Button1Click(Sender: TObject);
begin DefineInputColor(InputR,InputG,InputB,nil); end;

procedure TForm_ColorPicker.PositionHSBCursors();
begin
Shape1.Left:=HSImage.Left+Hue-Shape1.Width div 2;// - Shape1.Width mod 2;
Shape1.Top:=HSImage.Top+Sat-Shape1.Height div 2;// - Shape1.Width mod 2;
Ticker.Top:=BriImage.Top+Bri-(Ticker.Height div 2);
end;

procedure TForm_ColorPicker.SpinHSBChange(Sender: TObject);
var R,G,B:integer;
begin
if HSBRefresh then exit;
HSBRefresh:=true;
Hue:=EnsureRange(round(SpinH. Value),0,359);
Sat:=EnsureRange(255-round(SpinS. Value),0,255);
Bri:=EnsureRange(255-round(SpinBr.Value),0,255);
HSBRefresh:=false;
ConvertHSB2RGB(Hue,Sat,Bri,R,G,B);
PositionHSBCursors();
DrawHueSatQuad();
DrawBriRow();
DisplayResultColor('HSB');
end;

procedure TForm_ColorPicker.Button2Click(Sender: TObject);
begin
Form_ColorPicker.Close;
end;

end.
