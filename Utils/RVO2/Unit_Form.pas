unit Unit_Form;
interface
uses
  Windows,SysUtils, Classes, Graphics, Types, Controls, Forms, ExtCtrls, StdCtrls, Math, MMSystem,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_Terrain,
  RVO2_Simulator;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button3: TButton;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Times: array [0..2] of Integer;
    procedure DisplayMap;
  end;

  TPointArray = array of TPoint;

var
  Form1: TForm1;
  LocA, LocB: TKMPoint;
  Route: TPointArray;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  RandSeed := 4;

  fTerrain := TTerrain.Create;
  fTerrain.MapX := MAX_SIZE;
  fTerrain.MapY := MAX_SIZE;

  Button1Click(Self);

  timeBeginPeriod(1);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  TimeEndPeriod(1);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  I, J, K: Integer;
  L, T, W, H: Byte;
begin
  FillChar(fTerrain.Land[1,1], SizeOf(fTerrain.Land), #0);

  for I := 0 to 100 do
  begin
    L := Random(MAX_SIZE)+1;
    T := Random(MAX_SIZE)+1;
    W := Random(4) + 2;
    H := Random(4) + 2;
    for J := T to Min(T + H - 1, MAX_SIZE) do
    for K := L to Min(L + W - 1, MAX_SIZE) do
      fTerrain.Land[J, K].Passability := [CanWalk];
  end;

  DisplayMap;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  I: Integer;
  T: Single;
begin
  Times[0] := 0;
  Times[1] := 0;
  Times[2] := 0;

  for I := 0 to 99 do
  begin
    LocA.X := Random(MAX_SIZE);
    LocA.Y := Random(MAX_SIZE);

    T := Random; //Prefer shorter paths
    LocB.X := LocA.X + Round((Sqr(T) * Sign(T) * MAX_SIZE / 2));
    T := Random;
    LocB.Y := LocA.Y + Round((Sqr(T) * Sign(T) * MAX_SIZE / 2));

    DisplayMap;
  end;
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  Times[0] := 0;
  Times[1] := 0;
  Times[2] := 0;

  DisplayMap;
end;


procedure TForm1.DisplayMap;
var
  I,K: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.Height := MAX_SIZE;
    bmp.Width := MAX_SIZE;

    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

    //Obstacles
    for I := 1 to MAX_SIZE  do
      for K := 1 to MAX_SIZE do
        if (fTerrain.Land[I, K].Passability = [CanWalk]) then
          bmp.Canvas.Pixels[K-1, I-1] := clGray;

    //Draw route
    bmp.Canvas.Pen.Color := clSilver;
    bmp.Canvas.MoveTo(LocA.X-1, LocA.Y-1);
    for I := 0 to High(Route) do
      bmp.Canvas.LineTo(Route[I].X-1, Route[I].Y-1);

    //Route nodes
    for I := 0 to High(Route) do
      bmp.Canvas.Pixels[Route[I].X-1, Route[I].Y-1] := clYellow;

    //Start/End
    bmp.Canvas.Pixels[LocA.X-1, LocA.Y-1] := clRed;
    bmp.Canvas.Pixels[LocB.X-1, LocB.Y-1] := clLime;

    Image1.Canvas.StretchDraw(Image1.Canvas.ClipRect, bmp);
  finally
    bmp.Free;
  end;

  Image1.Repaint;
end;


procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocA.X := Ceil(X / Image1.Width * MAX_SIZE);
  LocA.Y := Ceil(Y / Image1.Height * MAX_SIZE);

  SetLength(Route, 0);
end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocB.X := Ceil(X / Image1.Width * MAX_SIZE);
  LocB.Y := Ceil(Y / Image1.Height * MAX_SIZE);

  Times[0] := 0;
  Times[1] := 0;
  Times[2] := 0;

  DisplayMap;
end;


end.
