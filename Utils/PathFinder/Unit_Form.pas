unit Unit_Form;
interface
uses
  SysUtils, Classes, Graphics, Types,
  Controls, Forms, ExtCtrls, StdCtrls,
  Math, Unit_Finder;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Finder: TFinder;
    procedure DisplayMap;
  end;

var
  Form1: TForm1;
  LocA, LocB: TPoint;
  R: TPointArray;


implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  LocA := Point(10, 10);
  LocB := Point(50, 50);
  Grid := TGrid.Create;
  Finder := TFinder.Create;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  I, J, K: Integer;
  L, T, W, H: Byte;
begin
  FillChar(Grid.Map[0,0], SizeOf(Grid.Map), #255);

  for I := 0 to 30 do
  begin
    L := Random(MAX_SIZE);
    T := Random(MAX_SIZE);
    W := Random(30) + 3;
    H := Random(20) + 3;
    for J := T to Min(T + H - 1, MAX_SIZE) do
      for K := L to Min(L + W - 1, MAX_SIZE) do
        Grid.Map[J, K] := False;
  end;

  DisplayMap;
end;


procedure TForm1.DisplayMap;
var
  I: Integer;
  K: Integer;
begin
  Image1.Canvas.Brush.Color := clBlack;
  Image1.Canvas.FillRect(Image1.Canvas.ClipRect);

  for I := 0 to MAX_SIZE - 1 do
    for K := 0 to MAX_SIZE - 1 do
      if Grid.Map[I, K] then
        Image1.Canvas.Pixels[K, I] := clGray;

  for I := 0 to High(R) do
    Image1.Canvas.Pixels[R[I].X, R[I].Y] := clCream;


  Image1.Canvas.Pixels[LocA.X, LocA.Y] := clRed;
  Image1.Canvas.Pixels[LocB.X, LocB.Y] := clWhite;
end;


procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocA.X := X;
  LocA.Y := Y;
end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocB.X := X;
  LocB.Y := Y;

  if Grid.IsWalkableAt(LocA.X, LocA.Y) and Grid.IsWalkableAt(LocB.X, LocB.Y) then
    R := Finder.MakeRoute(LocA, LocB);

  DisplayMap;
end;

end.
