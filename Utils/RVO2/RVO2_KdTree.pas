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
unit RVO2_KdTree;
interface
uses Classes, Math,
  RVO2_Agent, RVO2_Obstacle, RVO2_Vector2;


type
  TRVOFloatPair = record
    A, B: Single;
  end;

  function FloatPairLess(lhs, rhs: TRVOFloatPair): Boolean;
  function FloatPairLequal(lhs, rhs: TRVOFloatPair): Boolean;
  function FloatPairGreater(lhs, rhs: TRVOFloatPair): Boolean;
  function FloatPairGequal(lhs, rhs: TRVOFloatPair): Boolean;


type
  TRVOAgentTreeNode = class
    begin_: Integer;
    end_: Integer;
    left: Integer;
    maxX: Single;
    maxY: Single;
    minX: Single;
    minY: Single;
    right: Integer;
  end;

  TRVOObstacleTreeNode = class
    left: TRVOObstacleTreeNode;
    obstacle: TRVOObstacle;
    right: TRVOObstacleTreeNode;
  end;


  TRVOKdTree = class
    agents_: array of TRVOAgent;
    agentTree_: array of TRVOAgentTreeNode;
    obstacleTree_: TRVOObstacleTreeNode;
    constructor Create;
    procedure buildAgentTree;
    procedure buildAgentTreeRecursive(abegin, aend, node: Integer);
    procedure buildObstacleTree;
    function buildObstacleTreeRecursive(obstacles: TList): TRVOObstacleTreeNode;
    procedure queryObstacleTreeRecursive(agent: TRVOAgent; rangeSq: Single; node: TRVOObstacleTreeNode);
    procedure computeAgentNeighbors(agent: TRVOAgent; rangeSq: Single);
    procedure computeObstacleNeighbors(agent: TRVOAgent; rangeSq: Single);
    procedure queryAgentTreeRecursive(agent: TRVOAgent; rangeSq: Single; node: Integer);
    function queryVisibility(q1, q2: TRVOVector2; radius: Single): Boolean;
    function queryVisibilityRecursive(q1, q2: TRVOVector2; radius: Single; node: TRVOObstacleTreeNode): Boolean;
  end;


const
  MAX_LEAF_SIZE: Integer = 10;


implementation
uses RVO2_Math, RVO2_Simulator;


function FloatPairLess(lhs, rhs: TRVOFloatPair): Boolean;
begin
  Result := (lhs.a < rhs.a) or not (rhs.a < lhs.a) and (lhs.b < rhs.b);
end;

function FloatPairLequal(lhs, rhs: TRVOFloatPair): Boolean;
begin
  Result := ((lhs.a = rhs.a) and (lhs.b = rhs.b)) or FloatPairLess(lhs, rhs);
end;

function FloatPairGreater(lhs, rhs: TRVOFloatPair): Boolean;
begin
  Result := not FloatPairLequal(lhs, rhs);
end;

function FloatPairGequal(lhs, rhs: TRVOFloatPair): Boolean;
begin
  Result := not FloatPairLess(lhs, rhs);
end;

{ TRVOKdTree }
constructor TRVOKdTree.Create;
begin

end;


procedure TRVOKdTree.buildAgentTree;
var
  I: Integer;
begin
  if (agents_ = nil) or (Length(agents_) <> gSimulator.agents_.Count) then
  begin
    SetLength(agents_, gSimulator.agents_.Count);
    for I := 0 to Length(agents_) - 1 do
    begin
      agents_[i] := gSimulator.agents_[i];
    end;

    SetLength(agentTree_, 2 * Length(agents_));
    for I := 0 to Length(agentTree_) - 1 do
    begin
      agentTree_[i] := TRVOAgentTreeNode.Create;
    end;
  end;

  if (Length(agents_) <> 0) then
  begin
    buildAgentTreeRecursive(0, Length(agents_), 0);
  end;
end;

procedure TRVOKdTree.buildAgentTreeRecursive(abegin, aend, node: Integer);
var
  i: Integer;
  isVertical: Boolean;
  splitValue: Single;
  left, right: Integer;
  tmp: TRVOAgent;
  leftSize: Integer;
begin
  agentTree_[node].begin_ := abegin;
  agentTree_[node].end_ := aend;
  agentTree_[node].maxX := agents_[abegin].position_.x;
  agentTree_[node].maxY := agents_[abegin].position_.y;
  agentTree_[node].minX := agentTree_[node].maxX;
  agentTree_[node].minY := agentTree_[node].maxY;

  for i := abegin + 1 to aend - 1 do
  begin
    agentTree_[node].maxX := Max(agentTree_[node].maxX, agents_[i].position_.x);
    agentTree_[node].minX := Min(agentTree_[node].minX, agents_[i].position_.x);
    agentTree_[node].maxY := Max(agentTree_[node].maxY, agents_[i].position_.y);
    agentTree_[node].minY := Min(agentTree_[node].minY, agents_[i].position_.y);
  end;

  if (aend - abegin > MAX_LEAF_SIZE) then
  begin
    //No leaf node.
    isVertical := (agentTree_[node].maxX - agentTree_[node].minX > agentTree_[node].maxY - agentTree_[node].minY);
    splitValue := IfThen(isVertical, 0.5 * (agentTree_[node].maxX + agentTree_[node].minX), 0.5 * (agentTree_[node].maxY + agentTree_[node].minY));

    left := abegin;
    right := aend;

    while (left < right) do
    begin
        while (left < right) and (IfThen(isVertical, agents_[left].position_.x, agents_[left].position_.y) < splitValue) do
        begin
          Inc(left);
        end;

        while (right > left) and (IfThen(isVertical, agents_[right - 1].position_.x, agents_[right - 1].position_.y) >= splitValue) do
        begin
          Dec(right);
        end;

        if (left < right) then
        begin
            tmp := agents_[left];
            agents_[left] := agents_[right - 1];
            agents_[right - 1] := tmp;
            Inc(left);
            Dec(right);
        end;
    end;

    leftSize := left - abegin;

    if (leftSize = 0) then
    begin
      Inc(leftSize);
      Inc(left);
      Inc(right);
    end;

    agentTree_[node].left := node + 1;
    agentTree_[node].right := node + 1 + (2 * leftSize - 1);

    buildAgentTreeRecursive(abegin, left, agentTree_[node].left);
    buildAgentTreeRecursive(left, aend, agentTree_[node].right);
  end;
end;

//internal void buildObstacleTree()
procedure TRVOKdTree.buildObstacleTree;
var
  i: Integer;
  obstacles: TList;
begin
  obstacleTree_ := TRVOObstacleTreeNode.Create;

  obstacles := TList.Create;
  obstacles.Capacity := gSimulator.obstacles_.Count;

  for i := 0 to gSimulator.obstacles_.Count - 1 do
  begin
    obstacles.Add(gSimulator.obstacles_[i]);
  end;

  obstacleTree_ := buildObstacleTreeRecursive(obstacles);

  obstacles.Free;
end;


function TRVOKdTree.buildObstacleTreeRecursive(obstacles: TList): TRVOObstacleTreeNode;
var
  i,j,n: Integer;
  node: TRVOObstacleTreeNode;
  optimalSplit: Integer;
  minLeft, minRight: Integer;
  leftSize, rightSize: Integer;
  obstacleI1, obstacleI2: TRVOObstacle;
  obstacleJ1, obstacleJ2: TRVOObstacle;
  j1LeftOfI, j2LeftOfI: Single;
  FP1, FP2: TRVOFloatPair;
  leftObstacles, rightObstacles: TList;
  leftCounter, rightCounter: Integer;
  t: Single;
  splitpoint: TRVOVector2;
  newObstacle: TRVOObstacle;
begin
  if (obstacles.Count = 0) then
  begin
    Result := nil;
    Exit;
  end
  else
  begin
    node := TRVOObstacleTreeNode.Create;

    optimalSplit := 0;
    minLeft := obstacles.Count;
    minRight := obstacles.Count;

    for i := 0 to obstacles.Count - 1 do
    begin
      leftSize := 0;
      rightSize := 0;

      obstacleI1 := obstacles[i];
      obstacleI2 := obstacleI1.nextObstacle;

      //Compute optimal split node.
      for j := 0 to obstacles.Count - 1 do
      begin
          if (i = j) then
          begin
              continue;
          end;

          obstacleJ1 := obstacles[j];
          obstacleJ2 := obstacleJ1.nextObstacle;

          j1LeftOfI := leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ1.point_);
          j2LeftOfI := leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ2.point_);

          if (j1LeftOfI >= -RVO_EPSILON) and (j2LeftOfI >= -RVO_EPSILON) then
          begin
              Inc(leftSize);
          end
          else if (j1LeftOfI <= RVO_EPSILON) and (j2LeftOfI <= RVO_EPSILON) then
          begin
              Inc(rightSize);
          end
          else
          begin
              Inc(leftSize);
              Inc(rightSize);
          end;

          FP1.A := Max(leftSize, rightSize);
          FP1.B := Min(leftSize, rightSize);
          FP2.A := Max(minLeft, minRight);
          FP2.B := Min(minLeft, minRight);
          if FloatPairGequal(FP1, FP2) then
          begin
            break;
          end;
      end;

      FP1.A := Max(leftSize, rightSize);
      FP1.B := Min(leftSize, rightSize);
      FP2.A := Max(minLeft, minRight);
      FP2.B := Min(minLeft, minRight);
      if FloatPairLess(FP1, FP2) then
      begin
        minLeft := leftSize;
        minRight := rightSize;
        optimalSplit := i;
      end;
    end;
//Part2
    begin
        //Build split node.
        leftObstacles := TList.Create;
        for n := 0 to minLeft - 1 do leftObstacles.Add(nil);
        rightObstacles := TList.Create;
        for n := 0 to minRight - 1 do rightObstacles.Add(nil);

        leftCounter := 0;
        rightCounter := 0;
        i := optimalSplit;

        obstacleI1 := obstacles[i];
        obstacleI2 := obstacleI1.nextObstacle;

        for j := 0 to obstacles.Count - 1 do
        begin
          if (i = j) then
          begin
              continue;
          end;

          obstacleJ1 := obstacles[j];
          obstacleJ2 := obstacleJ1.nextObstacle;

          j1LeftOfI := leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ1.point_);
          j2LeftOfI := leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ2.point_);

          if (j1LeftOfI >= -RVO_EPSILON) and (j2LeftOfI >= -RVO_EPSILON) then
          begin
            leftObstacles[leftCounter] := obstacles[j];
            Inc(leftCounter);
          end
          else if (j1LeftOfI <= RVO_EPSILON) and (j2LeftOfI <= RVO_EPSILON) then
          begin
            rightObstacles[rightCounter] := obstacles[j];
            Inc(rightCounter);
          end
          else
          begin
            //Split obstacle j.
            t := det(Vector2Sub(obstacleI2.point_, obstacleI1.point_),
                     Vector2Sub(obstacleJ1.point_, obstacleI1.point_))
                 /
                 det(Vector2Sub(obstacleI2.point_, obstacleI1.point_),
                     Vector2Sub(obstacleJ1.point_, obstacleJ2.point_));

            splitpoint := Vector2Add(obstacleJ1.point_, Vector2Scale(Vector2Sub(obstacleJ2.point_, obstacleJ1.point_), t));

            newObstacle := TRVOObstacle.Create;
            newObstacle.point_ := splitpoint;
            newObstacle.prevObstacle := obstacleJ1;
            newObstacle.nextObstacle := obstacleJ2;
            newObstacle.isConvex_ := True;
            newObstacle.unitDir_ := obstacleJ1.unitDir_;

            newObstacle.id_ := gSimulator.obstacles_.Count;

            gSimulator.obstacles_.Add(newObstacle);

            obstacleJ1.nextObstacle := newObstacle;
            obstacleJ2.prevObstacle := newObstacle;

            if (j1LeftOfI > 0) then
            begin
                Inc(leftCounter);
                Inc(rightCounter);

                leftObstacles[leftCounter] := obstacleJ1;
                rightObstacles[rightCounter] := newObstacle;
            end
            else
            begin
                Inc(leftCounter);
                Inc(rightCounter);

                rightObstacles[rightCounter] := obstacleJ1;
                leftObstacles[leftCounter] := newObstacle;
            end;
          end;
        end;

        node.obstacle := obstacleI1;
        node.left := buildObstacleTreeRecursive(leftObstacles);
        node.right := buildObstacleTreeRecursive(rightObstacles);
        Result := node;
    end;
  end;
end;

procedure TRVOKdTree.computeAgentNeighbors(agent: TRVOAgent; rangeSq: Single);
begin
  queryAgentTreeRecursive(agent, rangeSq, 0);
end;

procedure TRVOKdTree.computeObstacleNeighbors(agent: TRVOAgent; rangeSq: Single);
begin
  queryObstacleTreeRecursive(agent, rangeSq, obstacleTree_);
end;


procedure TRVOKdTree.queryAgentTreeRecursive(agent: TRVOAgent; rangeSq: Single; node: Integer);
var
  i: Integer;
  distSqLeft, distSqRight: Single;
begin
  if (agentTree_[node].end_ - agentTree_[node].begin_ <= MAX_LEAF_SIZE) then
  begin
      for i := agentTree_[node].begin_ to agentTree_[node].end_ - 1 do
      begin
          agent.insertAgentNeighbor(agents_[i], rangeSq);
      end;
  end
  else
  begin
      distSqLeft := Sqr(Max(0, agentTree_[agentTree_[node].left].minX - agent.position_.x)
      ) + Sqr(Max(0, agent.position_.x - agentTree_[agentTree_[node].left].maxX)) +
      Sqr(Math.Max(0, agentTree_[agentTree_[node].left].minY - agent.position_.y)) +
      Sqr(Max(0, agent.position_.y - agentTree_[agentTree_[node].left].maxY));

    distSqRight := Sqr(Max(0, agentTree_[agentTree_[node].right].minX - agent.position_.x)
      ) + Sqr(Max(0, agent.position_.x - agentTree_[agentTree_[node].right].maxX)) +
      Sqr(Math.Max(0, agentTree_[agentTree_[node].right].minY - agent.position_.y)) +
      Sqr(Max(0, agent.position_.y - agentTree_[agentTree_[node].right].maxY));

      if (distSqLeft < distSqRight) then
      begin
          if (distSqLeft < rangeSq) then
          begin
              queryAgentTreeRecursive(agent, rangeSq, agentTree_[node].left);

              if (distSqRight < rangeSq) then
              begin
                  queryAgentTreeRecursive(agent, rangeSq, agentTree_[node].right);
              end;
          end;
      end
      else
      begin
          if (distSqRight < rangeSq) then
          begin
              queryAgentTreeRecursive(agent, rangeSq, agentTree_[node].right);

              if (distSqLeft < rangeSq) then
              begin
                  queryAgentTreeRecursive(agent, rangeSq, agentTree_[node].left);
              end;
          end;
      end;
  end;
end;

procedure TRVOKdTree.queryObstacleTreeRecursive(agent: TRVOAgent; rangeSq: Single; node: TRVOObstacleTreeNode);
var
  obstacle1, obstacle2: TRVOObstacle;
  agentLeftOfLine: Single;
  distSqLine: Single;
begin
  if (node = nil) then
  begin
      Exit;
  end
  else
  begin
      obstacle1 := node.obstacle;
      obstacle2 := obstacle1.nextObstacle;

      agentLeftOfLine := leftOf(obstacle1.point_, obstacle2.point_, agent.position_);

      if agentLeftOfLine >= 0 then
        queryObstacleTreeRecursive(agent, rangeSq, node.left)
      else
        queryObstacleTreeRecursive(agent, rangeSq, node.right);

      distSqLine := sqr(agentLeftOfLine) / absSq(Vector2Sub(obstacle2.point_, obstacle1.point_));

      if (distSqLine < rangeSq) then
      begin
          if (agentLeftOfLine < 0) then
          begin
              //Try obstacle at this node only if agent is on right side of
              //obstacle (and can see obstacle).
              agent.insertObstacleNeighbor(node.obstacle, rangeSq);
          end;

          //Try other side of line.
          if agentLeftOfLine >= 0 then
            queryObstacleTreeRecursive(agent, rangeSq, node.right)
          else
            queryObstacleTreeRecursive(agent, rangeSq, node.left);

      end;
  end;
end;

function TRVOKdTree.queryVisibility(q1, q2: TRVOVector2; radius: Single): Boolean;
begin
  Result := queryVisibilityRecursive(q1, q2, radius, obstacleTree_);
end;

function TRVOKdTree.queryVisibilityRecursive(q1, q2: TRVOVector2; radius: Single; node: TRVOObstacleTreeNode): Boolean;
var
  obstacle1, obstacle2: TRVOObstacle;
  q1LeftOfI, q2LeftOfI, invLengthI: Single;
  point1LeftOfQ, point2LeftOfQ, invLengthQ: Single;
begin
  if (node = nil) then
  begin
      Result := True;
  end
  else
  begin
    obstacle1 := node.obstacle;
    obstacle2 := obstacle1.nextObstacle;

    q1LeftOfI := leftOf(obstacle1.point_, obstacle2.point_, q1);
    q2LeftOfI := leftOf(obstacle1.point_, obstacle2.point_, q2);
    invLengthI := 1 / absSq(Vector2Sub(obstacle2.point_, obstacle1.point_));

    if (q1LeftOfI >= 0) and (q2LeftOfI >= 0) then
    begin
        Result := queryVisibilityRecursive(q1, q2, radius, node.left)
                  and (((sqr(q1LeftOfI) * invLengthI >= sqr(radius))
                       and (sqr(q2LeftOfI) * invLengthI >= sqr(radius)))
                       or queryVisibilityRecursive(q1, q2, radius, node.right));
    end
    else if (q1LeftOfI <= 0) and (q2LeftOfI <= 0) then
    begin
        Result := queryVisibilityRecursive(q1, q2, radius, node.right)
        and (((sqr(q1LeftOfI) * invLengthI >= sqr(radius))
        and (sqr(q2LeftOfI) * invLengthI >= sqr(radius)))
        or queryVisibilityRecursive(q1, q2, radius, node.left));
    end
    else if (q1LeftOfI >= 0) and (q2LeftOfI <= 0) then
    begin
        //One can see through obstacle from left to right.
        Result := queryVisibilityRecursive(q1, q2, radius, node.left)
          and queryVisibilityRecursive(q1, q2, radius, node.right);
      end
    else
    begin
        point1LeftOfQ := leftOf(q1, q2, obstacle1.point_);
        point2LeftOfQ := leftOf(q1, q2, obstacle2.point_);
        invLengthQ := 1 / absSq(Vector2Sub(q2, q1));

        Result := ((point1LeftOfQ * point2LeftOfQ >= 0) and (sqr(point1LeftOfQ)
          * invLengthQ > sqr(radius)) and (sqr(point2LeftOfQ) * invLengthQ
          > sqr(radius)) and queryVisibilityRecursive(q1, q2, radius, node.left)
          and queryVisibilityRecursive(q1, q2, radius, node.right));
      end;
  end;
end;

end.
