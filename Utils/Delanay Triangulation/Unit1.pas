unit Unit1;
interface
uses
  Forms, Classes, Controls, ExtCtrls, Graphics, Delaunay, StdCtrls, SysUtils;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fDelaunay: TDelaunay;
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  fDelaunay := TDelaunay.Create(10, 10, Image1.Width - 20, Image1.Height - 20);

  Image1Click(Self);
end;


procedure TForm1.Image1Click(Sender: TObject);
var I: Integer;
begin
  fDelaunay.Mesh;

  // Clear the form canvas
  Image1.Canvas.Brush.Color := clSilver;
  Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Image1.Height));
  Image1.Canvas.Brush.Color := clTeal;
  Memo1.Clear;

  //Draw the created triangles
  for I := 0 to fDelaunay.PolyCount - 1 do
  begin
    Image1.Canvas.Polygon([Point(Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv0].x), Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv0].y)),
                           Point(Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv1].x), Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv1].y)),
                           Point(Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv2].x), Trunc(fDelaunay.Vertex^[fDelaunay.Triangle^[i].vv2].y))]);
    Memo1.Lines.Append(Format('%d %d %d', [fDelaunay.Triangle^[i].vv0, fDelaunay.Triangle^[i].vv1, fDelaunay.Triangle^[i].vv2]));
  end;

  Image1.Canvas.Brush.Color := clSilver;
  for I := 0 to fDelaunay.VerticeCount - 1 do
    Image1.Canvas.TextOut(Trunc(fDelaunay.Vertex^[I].x), Trunc(fDelaunay.Vertex^[I].y), IntToStr(I));
end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDelaunay.AddPoint(X, Y);
  Image1Click(nil);
end;


end.
