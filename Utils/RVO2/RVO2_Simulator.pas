(*
 * RVO Library / RVO2 Library
 *
 * Copyright © 2008-10 University of North Carolina at Chapel Hill. All rights
 * reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for educational, research, and non-profit purposes, without
 * fee, and without a written agreement is hereby granted, provided that the
 * above copyright notice, this paragraph, and the following four paragraphs
 * appear in all copies.
 *
 * Permission to incorporate this software into commercial products may be
 * obtained by contacting the University of North Carolina at Chapel Hill.
 *
 * This software program and documentation are copyrighted by the University of
 * North Carolina at Chapel Hill. The software program and documentation are
 * supplied "as is", without any accompanying services from the University of
 * North Carolina at Chapel Hill or the authors. The University of North
 * Carolina at Chapel Hill and the authors do not warrant that the operation of
 * the program will be uninterrupted or error-free. The end-user understands
 * that the program was developed for research purposes and is advised not to
 * rely exclusively on the program for any reason.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL OR ITS
 * EMPLOYEES OR THE AUTHORS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE
 * UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL OR THE AUTHORS HAVE BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL AND THE AUTHORS SPECIFICALLY
 * DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE AND ANY
 * STATUTORY WARRANTY OF NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON
 * AN "AS IS" BASIS, AND THE UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL AND THE
 * AUTHORS HAVE NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.
 *)
unit RVO2_Simulator;
interface
uses Classes, SysUtils, Math,
  RVO2_Agent, RVO2_KdTree, RVO2_Vector2;

type
  TRVOWorker = class
    _start: Integer;
    _end: Integer;

    constructor Create(astart, aend: Single);
    procedure step;
    procedure update;
  end;

  TRVOSimulator = class
  public
    defaultAgent_: TRVOAgent;
    time_: Single;
    agents_: TList;
    obstacles_: TList;
    kdTree_: TRVOKdTree;
    timeStep_: Single;

    _numWorkers: Integer;
    _workers: array of TRVOWorker;

    procedure Clear;
    function GetNumWorkers: Integer;
    procedure SetNumWorkers(numWorkers: Integer);
    function getGlobalTime: Single;
    function getNumAgents: Integer;
    function getTimeStep: Single;
    procedure setAgentPrefVelocity(i: Integer; velocity: TRVOVector2);
    procedure setTimeStep(timeStep: Single);
    function getAgentPosition(i: Integer): TRVOVector2;
    function getAgentPrefVelocity(i: Integer): TRVOVector2;
    function getAgentVelocity(i: Integer): TRVOVector2;
    function getAgentRadius(i: Integer): Single;
    function getAgentOrcaLines(i: Integer): TList;
    function addAgent(position: TRVOVector2): Integer;
    procedure setAgentDefaults(neighborDist: Single; maxNeighbors: Integer;
      timeHorizon, timeHorizonObst, radius, maxSpeed: Single; velocity: TRVOVector2);
    function doStep: Single;
    function addObstacle(vertices: array of TRVOVector2): Integer;
    procedure processObstacles;
    function queryVisibility(point1, point2: TRVOVector2; radius: Single): Boolean;
    function getNumObstacleVertices: Integer;
    function getObstacleVertex(vertexNo: Integer): TRVOVector2;
    function getNextObstacleVertexNo(vertexNo: Integer): Integer;
    function getPrevObstacleVertexNo(vertexNo: Integer): Integer;
  end;

var
  gSimulator: TRVOSimulator;


implementation
uses RVO2_Obstacle, RVO2_Math;


procedure TRVOSimulator.Clear;
begin
  agents_ := TList.Create;
  obstacles_ := TList.Create;
  time_ := 0;
  defaultAgent_ := nil;
  kdTree_ := TRVOKdTree.Create;
  timeStep_ := 0.1;

  SetNumWorkers(0);
end;


function TRVOSimulator.GetNumWorkers: Integer;
begin
  Result := _numWorkers;
end;

procedure TRVOSimulator.SetNumWorkers(numWorkers: Integer);
begin
  _numWorkers := numWorkers;
  {if (_numWorkers <= 0) then
  begin
      int completionPorts;
      ThreadPool.GetMinThreads(out _numWorkers, out completionPorts);
  end;
  _workers := nil;}
end;


function TRVOSimulator.getGlobalTime: Single;
begin
  Result := Now;
end;

function TRVOSimulator.getNumAgents: Integer;
begin
  Result := agents_.Count;
end;

function TRVOSimulator.getTimeStep: Single;
begin
  Result := timeStep_;
end;


procedure TRVOSimulator.setAgentPrefVelocity(i: Integer; velocity: TRVOVector2);
begin
  TRVOAgent(agents_[i]).prefVelocity_ := velocity;
end;

procedure TRVOSimulator.setTimeStep(timeStep: Single);
begin
  timeStep_ := timeStep;
end;

function TRVOSimulator.getAgentPosition(i: Integer): TRVOVector2;
begin
  Result := TRVOAgent(agents_[i]).position_;
end;

function TRVOSimulator.getAgentPrefVelocity(i: Integer): TRVOVector2;
begin
  Result := TRVOAgent(agents_[i]).prefVelocity_;
end;

function TRVOSimulator.getAgentVelocity(i: Integer): TRVOVector2;
begin
  Result := TRVOAgent(agents_[i]).velocity_;
end;

function TRVOSimulator.getAgentRadius(i: Integer): Single;
begin
  Result := TRVOAgent(agents_[i]).radius_;
end;

function TRVOSimulator.getAgentOrcaLines(i: Integer): TList;
begin
  Result := TRVOAgent(agents_[i]).orcaLines_;
end;

function TRVOSimulator.addAgent(position: TRVOVector2): Integer;
var
  agent: TRVOAgent;
begin
  if (defaultAgent_ = nil) then
  begin
    Result := -1;
    Exit;
  end;

  agent := TRVOAgent.Create;

  agent.position_ := position;
  agent.maxNeighbors_ := defaultAgent_.maxNeighbors_;
  agent.maxSpeed_ := defaultAgent_.maxSpeed_;
  agent.neighborDist_ := defaultAgent_.neighborDist_;
  agent.radius_ := defaultAgent_.radius_;
  agent.timeHorizon_ := defaultAgent_.timeHorizon_;
  agent.timeHorizonObst_ := defaultAgent_.timeHorizonObst_;
  agent.velocity_ := defaultAgent_.velocity_;

  agent.id_ := agents_.Count;

  agents_.Add(agent);

  Result := agents_.Count - 1;
end;

procedure TRVOSimulator.setAgentDefaults(neighborDist: Single; maxNeighbors: Integer;
  timeHorizon, timeHorizonObst, radius, maxSpeed: Single; velocity: TRVOVector2);
begin
  if (defaultAgent_ = nil) then
  begin
    defaultAgent_ := TRVOAgent.Create;
  end;

  defaultAgent_.maxNeighbors_ := maxNeighbors;
  defaultAgent_.maxSpeed_ := maxSpeed;
  defaultAgent_.neighborDist_ := neighborDist;
  defaultAgent_.radius_ := radius;
  defaultAgent_.timeHorizon_ := timeHorizon;
  defaultAgent_.timeHorizonObst_ := timeHorizonObst;
  defaultAgent_.velocity_ := velocity;
end;

{ TRVOWorker }
constructor TRVOWorker.Create(astart, aend: Single);
begin
  _start := Trunc(astart);
  _end := Trunc(aend);
end;

procedure TRVOWorker.step;
var
  I: Integer;
begin
  for I := _start to _end - 1 do
  begin
    TRVOAgent(gSimulator.agents_[i]).computeNeighbors;
    TRVOAgent(gSimulator.agents_[i]).computeNewVelocity;
  end;
end;

procedure TRVOWorker.update;
var
  I: Integer;
begin
  for I := _start to _end - 1 do
  begin
    TRVOAgent(gSimulator.agents_[i]).update;
  end;
end;

function TRVOSimulator.doStep: Single;
var
  I: Integer;
  block: Integer;
begin
  if Length(_workers) = 0 then
  begin
    SetLength(_workers, _numWorkers);
    for block := 0 to Length(_workers) - 1 do
    begin
      _workers[block] := TRVOWorker.Create(block * getNumAgents() / Length(_workers), (block + 1) * getNumAgents() / Length(_workers));
    end;
  end;

  kdTree_.buildAgentTree;

  for block := 0 to Length(_workers) - 1 do
  begin
    _workers[block].step;
  end;

  for block := 0 to Length(_workers) - 1 do
  begin
    _workers[block].update;
  end;

  time_ := time + timeStep_;
  Result := time_;
end;

function TRVOSimulator.addObstacle(vertices: array of TRVOVector2): Integer;
var
  I: Integer;
  obstacleNo: Integer;
  obstacle: TRVOObstacle;
begin
  if Length(vertices) < 2 then
  begin
    Result := -1;
    Exit;
  end;

  obstacleNo := obstacles_.Count;

  for i := 0 to Length(vertices) - 1 do
  begin
    obstacle := TRVOObstacle.Create;
    obstacle.point_ := vertices[i];
    if (i <> 0) then
    begin
      obstacle.prevObstacle := obstacles_[obstacles_.Count - 1];
      obstacle.prevObstacle.nextObstacle := obstacle;
    end;
    if (i = Length(vertices) - 1) then
    begin
      obstacle.nextObstacle := obstacles_[obstacleNo];
      obstacle.nextObstacle.prevObstacle := obstacle;
    end;
    obstacle.unitDir_ := normalize(Vector2Sub(vertices[IfThen(i = Length(vertices) - 1, 0, i + 1)], vertices[i]));

    if (Length(vertices) = 2) then
    begin
      obstacle.isConvex_ := true;
    end
    else
    begin
      obstacle.isConvex_ := (leftOf(vertices[IfThen(i = 0, Length(vertices) - 1, i - 1)], vertices[i], vertices[IfThen(i = Length(vertices) - 1, 0, i + 1)]) >= 0);
    end;

    obstacle.id_ := obstacles_.Count;

    obstacles_.Add(obstacle);
  end;

  Result := obstacleNo;
end;

procedure TRVOSimulator.processObstacles;
begin
  kdTree_.buildObstacleTree;
end;

function TRVOSimulator.queryVisibility(point1, point2: TRVOVector2; radius: Single): Boolean;
begin
  Result := kdTree_.queryVisibility(point1, point2, radius);
end;

function TRVOSimulator.getNumObstacleVertices: Integer;
begin
  Result := obstacles_.Count;
end;

function TRVOSimulator.getObstacleVertex(vertexNo: Integer): TRVOVector2;
begin
  Result := TRVOObstacle(obstacles_[vertexNo]).point_;
end;

function TRVOSimulator.getNextObstacleVertexNo(vertexNo: Integer): Integer;
begin
  Result := TRVOObstacle(obstacles_[vertexNo]).nextObstacle.id_;
end;

function TRVOSimulator.getPrevObstacleVertexNo(vertexNo: Integer): Integer;
begin
  Result := TRVOObstacle(obstacles_[vertexNo]).prevObstacle.id_;
end;

end.
