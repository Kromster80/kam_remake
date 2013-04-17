unit Unit_Form;
interface
uses
  SysUtils, Classes, Graphics, Types, Controls, Forms, ExtCtrls, StdCtrls, MMSystem,
  RVO2_Math, RVO2_Simulator, RVO2_Vector2, RVO2_Interface;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button2: TButton;
    Timer1: TTimer;
    Button1: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure DisplayMap;
  end;

  TPointArray = array of TPoint;

  TRVORoadmapVertex = class
    position: TRVOVector2;
    //neighbors
    //distToGoal
  end;

var
  Form1: TForm1;
  bmp: TBitmap;
  rvo2: TRVO2;
  goals: array of TRVOVector2;
  roadmap: TList;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Height := 101;
  bmp.Width := 101;

  timeBeginPeriod(1);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  TimeEndPeriod(1);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  A: TRVO2Agent;
begin
  bmp.Height := 400;
  bmp.Width := 400;

  rvo2 := TRVO2.Create;

  // Add agents, specifying their start position.
  for I := 0 to 49 do
  begin
    A := TRVO2Agent.Create;
    A.Position := Vector2(Cos(I/50*2*pi)*50+50, Sin(I/50*2*pi)*50+50);
    A.Radius := 2;
    SetLength(A.Route, 1);
    A.Route[0] := Vector2(-Cos(I/50*2*pi)*50+50, -Sin(I/50*2*pi)*50+50);
    rvo2.AddAgent(A);
  end;

  Timer1.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
  A: TRVO2Agent;
begin
  bmp.Height := 400;
  bmp.Width := 400;

  rvo2 := TRVO2.Create;

  // Add agents, specifying their start position.
  for I := 0 to 3 do
  begin
    A := TRVO2Agent.Create;
    A.Position := Vector2((I mod 2) * 100, (I div 2) * 100);
    A.Radius := 2;
    SetLength(A.Route, 1);
    A.Route[0] := Vector2(100 - A.Position.X, 100 - A.Position.Y);
    rvo2.AddAgent(A);
  end;

  rvo2.AddObstacleRect(40, 30, 20, 40);

  Timer1.Enabled := True;
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  A: TRVO2Agent;
begin
  bmp.Height := 400;
  bmp.Width := 400;

  rvo2 := TRVO2.Create();

  // Add agents, specifying their start position.
  for I := 0 to 5 do
  begin
    A := TRVO2Agent.Create;
    A.Position := Vector2(10 + Random*20-5, 10 + Random*10-5);
    A.Radius := 2.5;
    SetLength(A.Route, 3);
    A.Route[0] := Vector2(10, 50);
    A.Route[1] := Vector2(90, 50);
    A.Route[2] := Vector2(90, 90);
    rvo2.AddAgent(A);
  end;

  for I := 0 to 5 do
  begin
    A := TRVO2Agent.Create;
    A.Position := Vector2(90 + Random*20-5, 90 + Random*10-5);
    A.Radius := 2.5;
    SetLength(A.Route, 3);
    A.Route[0] := Vector2(90, 50);
    A.Route[1] := Vector2(10, 50);
    A.Route[2] := Vector2(10, 10);
    rvo2.AddAgent(A);
  end;

  rvo2.AddObstacleRect(20, 0, 60, 42);
  rvo2.AddObstacleRect(20, 58, 60, 42);

  Timer1.Enabled := True;
end;


procedure TForm1.Timer1Click(Sender: TObject);
begin
  rvo2.Step;

  DisplayMap;
end;


procedure TForm1.DisplayMap;
var
  I,K: Integer;
  ObstacleStart: Integer;
  V: TRVOVector2;
  R: Single;
begin
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

  //Obstacle
  bmp.Canvas.Pen.Color := clGray;
  for I := 0 to rvo2.ObstacleCount - 1 do
  begin
    V := Vector2Scale(rvo2.Obstacles[I].Vertices[Length(rvo2.Obstacles[I].Vertices) - 1], 4);
    bmp.Canvas.MoveTo(Round(V.x), Round(V.y));
    for K := 0 to Length(rvo2.Obstacles[I].Vertices) - 1 do
    begin
      V := Vector2Scale(rvo2.Obstacles[I].Vertices[K], 4);
      bmp.Canvas.LineTo(Round(V.x), Round(V.y));
    end;
  end;

  //bmp.Canvas.Brush.Color := clGray;
  //bmp.Canvas.Rectangle(43, 30, 57, 70);

  //Agents
  bmp.Canvas.Pen.Color := clNone;
  bmp.Canvas.Brush.Color := clGreen;
  for I := 0 to rvo2.AgentCount - 1 do
  begin
    V := Vector2Scale(rvo2.Agents[I].Position, 4);
    R := rvo2.Agents[I].Radius * 4;
    bmp.Canvas.Ellipse(Round(V.x-R), Round(V.y-R), Round(V.x+R), Round(V.y+R));
    bmp.Canvas.Pixels[Round(V.x), Round(V.y)] := clYellow;
  end;

  Image1.Canvas.StretchDraw(Image1.Canvas.ClipRect, bmp);
  Image1.Repaint;
end;


end.
