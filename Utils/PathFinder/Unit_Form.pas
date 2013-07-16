unit Unit_Form;
interface
uses
  Windows,SysUtils, Classes, Graphics, Types, Controls, Forms, ExtCtrls, StdCtrls, Math, MMSystem,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_Terrain,
  KM_PathFinding, KM_PathFindingAStarOld, KM_PathFindingAStarNew, KM_PathFindingJPS;

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
    FinderOld: TPathFindingAStarOld;
    FinderNew: TPathFindingAStarNew;
    FinderJPS: TPathFindingJPS;
    procedure MakeRoutes;
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

  LocA := KMPoint(10, 10);
  LocB := KMPoint(50, 50);
  gTerrain := TKMTerrain.Create;
  gTerrain.MapX := MAX_SIZE;
  gTerrain.MapY := MAX_SIZE;

  FinderOld := TPathFindingAStarOld.Create;
  FinderNew := TPathFindingAStarNew.Create;
  FinderJPS := TPathFindingJPS.Create;

  Button1Click(Self);

  timeBeginPeriod(1);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  timeEndPeriod(1);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  I, J, K: Integer;
  L, T, W, H: Word;
begin
  FillChar(gTerrain.Land[1,1], SizeOf(gTerrain.Land), #0);

  for I := 0 to 200 do
  begin
    L := Random(MAX_SIZE)+1;
    T := Random(MAX_SIZE)+1;
    W := Random(40) + 3;
    H := Random(40) + 3;
    for J := T to Min(T + H - 1, MAX_SIZE) do
    for K := L to Min(L + W - 1, MAX_SIZE) do
      gTerrain.Land[J, K].Passability := [CanWalk];
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

  for I := 0 to 299 do
  begin
    LocA.X := Random(MAX_SIZE);
    LocA.Y := Random(MAX_SIZE);

    T := Random; //Prefer shorter paths
    LocB.X := LocA.X + Round((Sqr(T) * Sign(T) * MAX_SIZE / 2));
    T := Random;
    LocB.Y := LocA.Y + Round((Sqr(T) * Sign(T) * MAX_SIZE / 2));

    MakeRoutes;
    //DisplayMap;
  end;
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  Times[0] := 0;
  Times[1] := 0;
  Times[2] := 0;

  MakeRoutes;
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
        if (gTerrain.Land[I, K].Passability = [CanWalk]) then
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

  if gTerrain.CheckPassability(LocA, CanWalk) and gTerrain.CheckPassability(LocB, CanWalk) then
  begin

    if CheckBox1.Checked then
    begin
      N := TKMPointList.Create;
      T := timeGetTime;
      FinderOld.Route_Make(KMPoint(LocA.X, LocA.Y), KMPoint(LocB.X, LocB.Y), [canWalk], 0, nil, N);
      Inc(Times[0], timeGetTime - T);
      CheckBox1.Caption := 'A* old in ' + IntToStr(Times[0]) + 'ms';
      SetLength(Route, N.Count);
      for I := 0 to N.Count - 1 do
      begin
        Route[I].X := N[I].X;
        Route[I].Y := N[I].Y;
      end;
      N.Free;
    end;

    if CheckBox2.Checked then
    begin
      N := TKMPointList.Create;
      T := timeGetTime;
      FinderNew.Route_Make(KMPoint(LocA.X, LocA.Y), KMPoint(LocB.X, LocB.Y), [canWalk], 0, nil, N);
      Inc(Times[1], timeGetTime - T);
      CheckBox2.Caption := 'A* new in ' + IntToStr(Times[1]) + 'ms';
      SetLength(Route, N.Count);
      for I := 0 to N.Count - 1 do
      begin
        Route[I].X := N[I].X;
        Route[I].Y := N[I].Y;
      end;
      N.Free;
    end;

    if CheckBox3.Checked then
    begin
      N := TKMPointList.Create;
      T := timeGetTime;
      FinderJPS.Route_Make(KMPoint(LocA.X, LocA.Y), KMPoint(LocB.X, LocB.Y), [canWalk], 0, nil, N);
      Inc(Times[2], timeGetTime - T);
      CheckBox3.Caption := 'A* JPS in ' + IntToStr(Times[2]) + 'ms';
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
