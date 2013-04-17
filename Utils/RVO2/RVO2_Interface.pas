unit RVO2_Interface;
interface
uses Classes, Math, SysUtils, RVO2_Math, RVO2_Simulator, RVO2_Vector2;

type
  TRVORoadmapVertex = class
  	position: TRVOVector2;
	  neighbors: TList;
  	//std::vector<float> distToGoal;
  end;

  TRVO2Agent = class
    MaxSpeed: Single;
    Position: TRVOVector2;
    Radius: Single;
    RoutePos: Integer;
    Route: array of TRVOVector2;
    constructor Create;
    function CurrentGoal: TRVOVector2;
    procedure StepNext;
    function NearGoal: Boolean;
  end;

  TRVO2Obstacle = class
    Vertices: array of TRVOVector2;
  end;

  TRVO2 = class
  private
    //Agents defaults
    fneighborDist: Single;
    fmaxNeighbors: Integer;
    ftimeHorizon: Single;
    ftimeHorizonObst: Single;
    fradius: Single;
    fmaxSpeed: Single;

    fAgents: TList;
    fObstacles: TList;

    fRoadmap: TList;

    function GetAgents(I: Integer): TRVO2Agent;
    function GetObstacles(I: Integer): TRVO2Obstacle;
    function GetRoadmap(I: Integer): TRVORoadmapVertex;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAgent(aAgent: TRVO2Agent);
    procedure AddObstacleRect(x,y,w,h: Single);
    procedure AddRoadVertice(x,y: Single);
    function AgentCount: Integer;
    property Agents[I: Integer]: TRVO2Agent read GetAgents;
    procedure buildRoadmap;
    function ObstacleCount: Integer;
    property Obstacles[I: Integer]: TRVO2Obstacle read GetObstacles;
    property Roadmap[I: Integer]: TRVORoadmapVertex read GetRoadmap;

    procedure Step;
  end;


implementation


constructor TRVO2Agent.Create;
begin
  MaxSpeed := 2;
  Radius := 2.5;
end;


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

  fneighborDist := 15;
  fmaxNeighbors := 10;
  ftimeHorizon := 10;
  ftimeHorizonObst := 5;
  fradius := 2;
  fmaxSpeed := 2;

  fAgents := TList.Create;
  fObstacles := TList.Create;
  fRoadmap := TList.Create;

  gSimulator := TRVOSimulator.Create;
  gSimulator.Clear;
  gSimulator.setTimeStep(0.2);
end;


destructor TRVO2.Destroy;
begin
  FreeAndNil(gSimulator);
  fAgents.Free;
  fObstacles.Free;
  fRoadmap.Free;

  inherited;
end;


procedure TRVO2.AddAgent(aAgent: TRVO2Agent);
begin
  gSimulator.setAgentDefaults(fneighborDist, fmaxNeighbors, ftimeHorizon, ftimeHorizonObst, aAgent.Radius, aAgent.MaxSpeed, Vector2(0,0));
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


procedure TRVO2.AddRoadVertice(x, y: Single);
var
  v: TRVORoadmapVertex;
begin
  v := TRVORoadmapVertex.Create;
  v.position.x := x;
  v.position.y := y;
  fRoadmap.Add(v);
end;


function TRVO2.AgentCount: Integer;
begin
  Result := gSimulator.getNumAgents;
end;


procedure TRVO2.buildRoadmap;
var
  i,j: Integer;
begin
	// Connect the roadmap vertices by edges if mutually visible
	for i := 0 to fRoadmap.Count - 1 do
  begin
		for j := 0 to fRoadmap.Count - 1 do
    begin
			if (gSimulator.queryVisibility(Roadmap[i].position, Roadmap[j].position, gSimulator.getAgentRadius(0))) then
      begin
				roadmap[i].neighbors.Add(Pointer(j));
			end;
		end;

		// Initialize the distance to each of the four goal vertices at infinity
		// (9e9f).
		roadmap[i].distToGoal.resize(4, 9e9f);
	end;

	(*
	 * Compute the distance to each of the four goals (the first four vertices)
	 * for all vertices using Dijkstra's algorithm.
	 *)
	for i := 0 to 3 do
  begin
		std::multimap<float, int> Q;
		std::vector<std::multimap<float, int>::iterator> posInQ(roadmap.size(), Q.end());

		roadmap[i].distToGoal[i] = 0.0f;
		posInQ[i] = Q.insert(std::make_pair(0.0f, i));

		while (!Q.empty()) do
    begin
			const int u = Q.begin()->second;
			Q.erase(Q.begin());
			posInQ[u] = Q.end();

			for (int j = 0; j < static_cast<int>(roadmap[u].neighbors.size()); ++j) do
      begin
				const int v = roadmap[u].neighbors[j];
				const float dist_uv = RVO::abs(roadmap[v].position - roadmap[u].position);

				if (roadmap[v].distToGoal[i] > roadmap[u].distToGoal[i] + dist_uv) then
        begin
					roadmap[v].distToGoal[i] := roadmap[u].distToGoal[i] + dist_uv;

					if (posInQ[v] = Q.end()) then
          begin
						posInQ[v] := Q.insert(std::make_pair(roadmap[v].distToGoal[i], v));
					end
					else
          begin
						Q.erase(posInQ[v]);
						posInQ[v] := Q.insert(std::make_pair(roadmap[v].distToGoal[i], v));
					end;
				end;
			end;
		end;
	end;
end;


function TRVO2.GetAgents(I: Integer): TRVO2Agent;
begin
  Result := fAgents[I];
end;


function TRVO2.GetObstacles(I: Integer): TRVO2Obstacle;
begin
  Result := fObstacles[I];
end;


function TRVO2.GetRoadmap(I: Integer): TRVORoadmapVertex;
begin
  Result := fRoadmap[I];
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

        if InRange(absSq(gSimulator.getAgentVelocity(i)), 0.0001, 0.1) then
          gSimulator.setAgentPrefVelocity(i, Vector2(Random, Random))
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
