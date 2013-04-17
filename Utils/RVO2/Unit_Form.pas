unit Unit_Form;
interface
uses
  Windows,SysUtils, Classes, Graphics, Types, Controls, Forms, ExtCtrls, StdCtrls, Math, MMSystem,
  RVO2_Math, RVO2_Simulator, RVO2_Vector2;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button2: TButton;
    Button3: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure DisplayMap;
  end;

  TPointArray = array of TPoint;

var
  Form1: TForm1;
  bmp: TBitmap;
  NUM_AGENTS: Byte = 4;
  goals: array of TRVOVector2;


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


procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
  vertices: array of TRVOVector2;
begin
  gSimulator := TRVOSimulator.Create;
  gSimulator.Clear;

  gSimulator.setTimeStep(0.2);
  gSimulator.setAgentDefaults(15, 10, 10, 5, 2, 2, Vector2(0, 0));

  // Add agents, specifying their start position.
  gSimulator.addAgent(Vector2(  0,   0));
  gSimulator.addAgent(Vector2(100,   0));
  gSimulator.addAgent(Vector2(100, 100));
  gSimulator.addAgent(Vector2(  0, 100));

  // Create goals (simulator is unaware of these).
  SetLength(goals, gSimulator.getNumAgents);
  for i := 0 to gSimulator.getNumAgents - 1 do
  begin
    goals[i].X := 100 - gSimulator.getAgentPosition(i).X;
    goals[i].Y := 100 - gSimulator.getAgentPosition(i).Y;
  end;

  // Add (polygonal) obstacle(s), specifying vertices in counterclockwise order.
  SetLength(vertices, 4);
  vertices[0] := Vector2(43, 30);
  vertices[1] := Vector2(57, 30);
  vertices[2] := Vector2(57, 70);
  vertices[3] := Vector2(43, 70);

  //gSimulator.addObstacle(vertices);

  // Process obstacles so that they are accounted for in the simulation.
  gSimulator.processObstacles;

  Timer1.Enabled := True;
end;


procedure TForm1.Button3Click(Sender: TObject);
  procedure setPreferredVelocities;
  var i: Integer;
  begin
    // Set the preferred velocity for each agent.
    for i := 0 to gSimulator.getNumAgents - 1 do
    begin
      if absSq(Vector2Sub(goals[i], gSimulator.getAgentPosition(i))) < Sqr(gSimulator.getAgentRadius(i)) then
        // Agent is within one radius of its goal, set preferred velocity to zero
        gSimulator.setAgentPrefVelocity(i, Vector2(0, 0))
      else
        // Agent is far away from its goal, set preferred velocity as unit vector towards agent's goal.
        gSimulator.setAgentPrefVelocity(i, normalize(Vector2Sub(goals[i], gSimulator.getAgentPosition(i))))
    end;
  end;
begin
  setPreferredVelocities;
  gSimulator.doStep;

  DisplayMap;
end;


procedure TForm1.DisplayMap;
var
  I: Integer;
begin
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

  //Obstacle
  bmp.Canvas.Brush.Color := clGray;
  bmp.Canvas.Rectangle(43, 30, 57, 70);

  //Agents
  for I := 0 to gSimulator.getNumAgents - 1 do
    bmp.Canvas.Pixels[Round(gSimulator.getAgentPosition(I).X), Round(gSimulator.getAgentPosition(I).Y)] := clYellow;

  Image1.Canvas.StretchDraw(Image1.Canvas.ClipRect, bmp);
  Image1.Repaint;
end;


end.
