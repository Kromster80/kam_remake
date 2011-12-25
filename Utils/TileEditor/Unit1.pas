unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Math;

type
  TForm1 = class(TForm)
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RadioGroup1Click(Sender: TObject);
  end;

type TProperty = (
       tpWater=0,
       tpSand,     //Animals, Trees, Fields
       tpSoil,     //Animals, Trees, Fields
       tpWalkable, //Serfs
       tpRoadable  //Workers
       );

  TPropertySet = set of TProperty;

var
  Form1: TForm1;
  Bits:array[0..255]of TShape;
  Tiles:array[0..255]of TPropertySet;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  for i:=0 to 255 do begin
    Bits[i] := TShape.Create(Form1);
    Bits[i].Parent := Form1;
    Bits[i].Left := Image1.Left + (i mod 16) *32;
    Bits[i].Top := Image1.Top + (i div 16) *32;
    Bits[i].Width := 33;
    Bits[i].Height := 33;
    Bits[i].Brush.Style := bsDiagCross;
    Bits[i].Brush.Color := $FF00;
    Bits[i].Pen.Color := $8800;
    Bits[i].Tag := i;
    Bits[i].OnMouseDown := MouseDown;
    Bits[i].OnMouseMove := MouseMove;
    Bits[i].OnMouseUp   := MouseUp;
  end;
end;


procedure TForm1.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then MouseMove(Sender, [ssLeft], X, Y);
end;


procedure TForm1.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i:integer; xx,yy:integer;
begin
  if not (ssLeft in Shift) then exit;

  if Sender is TShape then begin
    xx := (X+TShape(Sender).Left) div 32;
    yy := (Y+TShape(Sender).Top) div 32;
  end else begin
    xx := (X) div 32;
    yy := (Y) div 32;
  end;
  xx := EnsureRange(xx,0,15);
  yy := EnsureRange(yy,0,15);
  i := yy*16 + xx;

  Bits[i].Visible := (Sender is TImage);
end;


procedure TForm1.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //nothing
end;


procedure TForm1.RadioGroup1Click(Sender: TObject);
var i:integer;
begin
  for i:=0 to 255 do
    Bits[i].Visible := TProperty(RadioGroup1.ItemIndex) in Tiles[i];
end;

end.
