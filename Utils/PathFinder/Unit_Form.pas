unit Unit_Form;
interface
uses
  Windows,SysUtils, Classes, Graphics, Types, Controls, Forms, ExtCtrls, StdCtrls, Math, MMSystem,
  Unit_Finder, KM_PathFinding, KM_Points, KM_CommonClasses;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Finder: TFinder;
    Old: TPathFinding;

    procedure MakeRoutes;
    procedure DisplayMap;
  end;

var
  Form1: TForm1;
  LocA, LocB: TPoint;
  Route: TPointArray;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  RandSeed := 4;

  LocA := Point(10, 10);
  LocB := Point(50, 50);
  Grid := TGrid.Create;
  Finder := TFinder.Create;
  Old := TPathFinding.Create;

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
  FillChar(Grid.Map[0,0], SizeOf(Grid.Map), #255);

  for I := 0 to 40 do
  begin
    L := Random(MAX_SIZE);
    T := Random(MAX_SIZE);
    W := Random(40) + 3;
    H := Random(40) + 3;
    for J := T to Min(T + H - 1, MAX_SIZE-1) do
    for K := L to Min(L + W - 1, MAX_SIZE-1) do
      Grid.Map[J, K] := False;
  end;

  DisplayMap;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 49 do
  begin
    LocA.X := Random(MAX_SIZE);
    LocA.Y := Random(MAX_SIZE);
    LocB.X := Random(MAX_SIZE);
    LocB.Y := Random(MAX_SIZE);

    MakeRoutes;
    DisplayMap;
  end;
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
    for I := 0 to MAX_SIZE - 1 do
      for K := 0 to MAX_SIZE - 1 do
        if Grid.Map[I, K] then
          bmp.Canvas.Pixels[K, I] := clGray;

    //Draw route
    bmp.Canvas.Pen.Color := clSilver;
    bmp.Canvas.MoveTo(LocA.X, LocA.Y);
    for I := 0 to High(Route) do
      bmp.Canvas.LineTo(Route[I].X, Route[I].Y);

    //Route nodes
    for I := 0 to High(Route) do
      bmp.Canvas.Pixels[Route[I].X, Route[I].Y] := clYellow;

    //Start/End
    bmp.Canvas.Pixels[LocA.X, LocA.Y] := clRed;
    bmp.Canvas.Pixels[LocB.X, LocB.Y] := clLime;

    Image1.Canvas.StretchDraw(Image1.Canvas.ClipRect, bmp);
  finally
    bmp.Free;
  end;

  Image1.Repaint;
end;


procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocA.X := X div 2;
  LocA.Y := Y div 2;

  SetLength(Route, 0);
end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LocB.X := X div 2;
  LocB.Y := Y div 2;

  MakeRoutes;
  DisplayMap;
end;


procedure TForm1.MakeRoutes;
var
  N: TKMPointList;
  I: Integer;
  T: Cardinal;
begin
  SetLength(Route, 0);

  if Grid.IsWalkableAt(LocA.X, LocA.Y) and Grid.IsWalkableAt(LocB.X, LocB.Y) then
  begin
    if CheckBox1.Checked then
    begin
      T := timeGetTime;
      Route := Finder.MakeRoute(LocA, LocB);
      Label1.Caption := 'JPS in ' + IntToStr(timeGetTime - T) + 'ms';
    end;

    if CheckBox2.Checked then
    begin
      N := TKMPointList.Create;
      T := timeGetTime;
      //Old.Free;
      //Old := TPathFinding.Create;
      Old.Route_Make(KMPoint(LocA.X, LocA.Y), KMPoint(LocB.X, LocB.Y), [], 0, N);
      Label2.Caption := 'A* in ' + IntToStr(timeGetTime - T) + 'ms';
      SetLength(Route, N.Count);
      for I := 0 to N.Count - 1 do
      begin
        Route[I].X := N[I].X;
        Route[I].Y := N[I].Y;
      end;
      N.Free;
    end;
  end;
end;


end.
