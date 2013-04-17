unit RVO2_Interface;
interface
uses Classes, SysUtils, RVO2_Math, RVO2_Simulator, RVO2_Vector2;

type
  TRVO2Agent = class
    Position: TRVOVector2;
    Radius: Single;
    RoutePos: Integer;
    Route: array of TRVOVector2;
    function CurrentGoal: TRVOVector2;
    procedure StepNext;
    function NearGoal: Boolean;
  end;

  TRVO2Obstacle = class
    Vertices: array of TRVOVector2;
  end;

  TRVO2 = class
  private
    fAgents: TList;
    fObstacles: TList;
    function GetAgents(I: Integer): TRVO2Agent;
    function GetObstacles(I: Integer): TRVO2Obstacle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAgent(aAgent: TRVO2Agent);
    procedure AddObstacleRect(x,y,w,h: Single);
    function AgentCount: Integer;
    property Agents[I: Integer]: TRVO2Agent read GetAgents;
    function ObstacleCount: Integer;
    property Obstacles[I: Integer]: TRVO2Obstacle read GetObstacles;

    procedure Step;
  end;


implementation


function TRVO2Agent.CurrentGoal: TRVOVector2;
begin
  if RoutePos < Length(Route) then
    Result := Route[RoutePos]
  else
    Result := Route[Length(Route) - 1];
end;


procedure TRVO2Agent.StepNext;
begin
  Inc(RoutePos);
end;


function TRVO2Agent.NearGoal: Boolean;
begin
  Result := absSq(Vector2Sub(CurrentGoal, Position)) < Sqr(Radius * 2);
end;


{ TRVO2 }
constructor TRVO2.Create();
begin
  inherited;

  fAgents := TList.Create;
  fObstacles := TList.Create;

  gSimulator := TRVOSimulator.Create;
  gSimulator.Clear;
  gSimulator.setTimeStep(0.25);
  gSimulator.setAgentDefaults(15, 10, 10, 5, 2.5, 2, Vector2(0,0));
end;


destructor TRVO2.Destroy;
begin
  FreeAndNil(gSimulator);
  fAgents.Free;
  fObstacles.Free;

  inherited;
end;


procedure TRVO2.AddAgent(aAgent: TRVO2Agent);
begin
  gSimulator.setAgentDefaults(15, 10, 10, 5, aAgent.Radius, 2, Vector2(0,0));
  gSimulator.addAgent(aAgent.Position);
  fAgents.Add(aAgent);
end;


procedure TRVO2.AddObstacleRect(x, y, w, h: Single);
var
  O: TRVO2Obstacle;
begin
  O := TRVO2Obstacle.Create;
  SetLength(O.Vertices, 4);

  // Add (polygonal) obstacle(s), specifying vertices in counterclockwise order.
  O.Vertices[0] := Vector2(  x,   y);
  O.Vertices[1] := Vector2(x+w,   y);
  O.Vertices[2] := Vector2(x+w, y+h);
  O.Vertices[3] := Vector2(  x, y+h);

  gSimulator.addObstacle(O.Vertices);

  gSimulator.processObstacles;

  fObstacles.Add(O);
end;


function TRVO2.AgentCount: Integer;
begin
  Result := gSimulator.getNumAgents;
end;


function TRVO2.GetAgents(I: Integer): TRVO2Agent;
begin
  Result := fAgents[I];
end;


function TRVO2.GetObstacles(I: Integer): TRVO2Obstacle;
begin
  Result := fObstacles[I];
end;


function TRVO2.ObstacleCount: Integer;
begin
  Result := fObstacles.Count;
end;


procedure TRVO2.Step;
  procedure setPreferredVelocities;
  var
    i: Integer;
  begin
    // Set the preferred velocity for each agent.
    for i := 0 to gSimulator.getNumAgents - 1 do
    begin
      Agents[I].Position := gSimulator.getAgentPosition(i);

      if Agents[I].NearGoal then
      begin
        // Agent is within one radius of its goal, set preferred velocity to zero
        gSimulator.setAgentPrefVelocity(i, Vector2(0, 0));
        Agents[I].StepNext;
      end
      else
        // Agent is far away from its goal, set preferred velocity as unit vector towards agent's goal.
        gSimulator.setAgentPrefVelocity(i, normalize(Vector2Sub(Agents[i].CurrentGoal, gSimulator.getAgentPosition(i))));

    end;
  end;
begin
  setPreferredVelocities;

  gSimulator.doStep;
end;


end.
