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
unit RVO2_Agent;
interface
uses Classes,
  RVO2_Vector2, RVO2_Obstacle;

type
  TRVOAgent = class;

  TRVOKeyValueAgent = class
    Key: Single;
    Value: TRVOAgent;
  end;

  TRVOKeyValueObstacle = class
    Key: Single;
    Value: TRVOObstacle;
  end;

  TRVOAgent = class
  public
    agentNeighbors_: TList;
    maxNeighbors_: Integer;
    maxSpeed_: Single;
    neighborDist_: Single;
    newVelocity_: TRVOVector2;
    obstacleNeighbors_: TList;
    orcaLines_: TList;
    position_: TRVOVector2;
    prefVelocity_: TRVOVector2;
    radius_: Single;
    timeHorizon_: Single;
    timeHorizonObst_: Single;
    velocity_: TRVOVector2;
    id_: Integer;
    constructor Create;
    procedure computeNeighbors;
    procedure computeNewVelocity;
    procedure insertAgentNeighbor(agent: TRVOAgent; rangeSq: Single);
    procedure insertObstacleNeighbor(obstacle: TRVOObstacle; rangeSq: Single);
    procedure update;
  end;


implementation
uses RVO2_Line, RVO2_Math, RVO2_Simulator;


{ TRVOAgent }
constructor TRVOAgent.Create;
begin

  agentNeighbors_ := TList.Create;
  obstacleNeighbors_ := TList.Create;
  orcaLines_ := TList.Create;
end;


procedure TRVOAgent.computeNeighbors;
var
  rangeSq: Single;
begin
  obstacleNeighbors_.Clear;
  rangeSq := Sqr(timeHorizonObst_ * maxSpeed_ + radius_);
  gSimulator.kdTree_.computeObstacleNeighbors(Self, rangeSq);

  agentNeighbors_.Clear;
  if (maxNeighbors_ > 0) then
  begin
    rangeSq := Sqr(neighborDist_);
    gSimulator.kdTree_.computeAgentNeighbors(Self, rangeSq);
  end;
end;


// Search for the best new velocity.
procedure TRVOAgent.computeNewVelocity;
var
  i,j: Integer;
  invTimeHorizonObst: Single;
  obstacle1, obstacle2: TRVOObstacle;
  relativePosition1, relativePosition2: TRVOVector2;
  alreadyCovered: Boolean;
  distSq1, distSq2, radiusSq: Single;
  obstacleVector: TRVOVector2;
  s, distSqLine: Single;
  line: TRVOLine;
  leftLegDirection, rightLegDirection: TRVOVector2;
  leg1, leg2: Single;
  leftNeighbor: TRVOObstacle;
  isLeftLegForeign, isRightLegForeign: Boolean;
begin
  orcaLines_.Clear;

  invTimeHorizonObst := 1 / timeHorizonObst_;

  //Create obstacle ORCA lines.
  for i := 0 to obstacleNeighbors_.Count - 1 do
  begin

    obstacle1 := TRVOKeyValueObstacle(obstacleNeighbors_[i]).Value;
    obstacle2 := obstacle1.nextObstacle;

    relativePosition1 := Vector2Sub(obstacle1.point_,  position_);
    relativePosition2 := Vector2Sub(obstacle2.point_, position_);

    //Check if velocity obstacle of obstacle is already taken care of by
    //previously constructed obstacle ORCA lines.
    alreadyCovered := false;

    for j := 0 to orcaLines_.Count - 1 do
    begin
      if (det(Vector2Sub(Vector2Scale(relativePosition1, invTimeHorizonObst), TRVOLine(orcaLines_[j]).point),
        TRVOLine(orcaLines_[j]).direction) - invTimeHorizonObst * radius_ >= -RVO_EPSILON)
      and
         (det(Vector2Sub(Vector2Scale(relativePosition2, invTimeHorizonObst), TRVOLine(orcaLines_[j]).point),
         TRVOLine(orcaLines_[j]).direction) - invTimeHorizonObst * radius_ >= -RVO_EPSILON) then
      begin
        alreadyCovered := true;
        break;
      end;
    end;

    if (alreadyCovered) then
    begin
      Continue;
    end;

    //Not yet covered. Check for collisions.

    distSq1 := absSq(relativePosition1);
    distSq2 := absSq(relativePosition2);

    radiusSq := sqr(radius_);

    obstacleVector := Vector2Sub(obstacle2.point_, obstacle1.point_);
    s := Vector2Mul(Vector2Neg(relativePosition1), obstacleVector) / absSq(obstacleVector);
    distSqLine := absSq(Vector2Sub(Vector2Neg(relativePosition1), Vector2Scale(obstacleVector, s)));

    line := TRVOLine.Create;

    if (s < 0) and (distSq1 <= radiusSq) then
    begin
      //Collision with left vertex. Ignore if non-convex.
      if (obstacle1.isConvex_) then
      begin
        //line.point := new Vector2(0, 0);
        line.direction.X := -relativePosition1.y;
        line.direction.Y := relativePosition1.x;
        orcaLines_.Add(line);
      end;
      continue;
    end
    else if (s > 1) and (distSq2 <= radiusSq) then
    begin
      //Collision with right vertex. Ignore if non-convex
      //or if it will be taken care of by neighoring obstace */
      if (obstacle2.isConvex_) and (det(relativePosition2, obstacle2.unitDir_) >= 0) then
      begin
        //line.point := new Vector2(0, 0);
        line.direction.X := -relativePosition2.y;
        line.direction.Y := relativePosition2.x;
        orcaLines_.Add(line);
      end;
      continue;
    end
    else if (s >= 0) and (s < 1) and (distSqLine <= radiusSq) then
    begin
      //Collision with obstacle segment.
      //line.point = new Vector2(0, 0);
      line.direction := Vector2Neg(obstacle1.unitDir_);
      orcaLines_.Add(line);
      continue;
    end;

    //No collision.
    //Compute legs. When obliquely viewed, both legs can come from a single
    //vertex. Legs extend cut-off line when nonconvex vertex.

    if (s < 0) and (distSqLine <= radiusSq) then
    begin
      //Obstacle viewed obliquely so that left vertex
      //defines velocity obstacle.
      if (not obstacle1.isConvex_) then
      begin
        //Ignore obstacle.
        continue;
      end;

      obstacle2 := obstacle1;

      leg1 := sqrt(distSq1 - radiusSq);
      leftLegDirection.X := (relativePosition1.x * leg1 - relativePosition1.y * radius_) / distSq1;
      leftLegDirection.Y := (relativePosition1.x * radius_ + relativePosition1.y * leg1) / distSq1;
      rightLegDirection.X := (relativePosition1.x * leg1 + relativePosition1.y * radius_) / distSq1;
      rightLegDirection.Y := (-relativePosition1.x * radius_ + relativePosition1.y * leg1) / distSq1;
    end
    else if (s > 1) and (distSqLine <= radiusSq) then
    begin
      //Obstacle viewed obliquely so that
      //right vertex defines velocity obstacle.
      if ( not obstacle2.isConvex_) then
      begin
        //Ignore obstacle.
        continue;
      end;

      obstacle1 := obstacle2;

      leg2 := sqrt(distSq2 - radiusSq);
      leftLegDirection.X := (relativePosition2.x * leg2 - relativePosition2.y * radius_) / distSq2;
      leftLegDirection.Y := (relativePosition2.x * radius_ + relativePosition2.y * leg2) / distSq2;
      rightLegDirection.X := (relativePosition2.x * leg2 + relativePosition2.y * radius_) / distSq2;
      rightLegDirection.Y := (-relativePosition2.x * radius_ + relativePosition2.y * leg2) / distSq2;
    end
    else
    begin
        //Usual situation.
        if (obstacle1.isConvex_) then
        begin
            leg1 := sqrt(distSq1 - radiusSq);
            leftLegDirection.X := (relativePosition1.x * leg1 - relativePosition1.y * radius_) / distSq1;
            leftLegDirection.Y := (relativePosition1.x * radius_ + relativePosition1.y * leg1) / distSq1;
        end
        else
        begin
            //Left vertex non-convex; left leg extends cut-off line.
            leftLegDirection := Vector2Neg(obstacle1.unitDir_);
        end;

        if (obstacle2.isConvex_) then
        begin
            leg2 := sqrt(distSq2 - radiusSq);
            rightLegDirection.X := (relativePosition2.x * leg2 + relativePosition2.y * radius_) / distSq2;
            rightLegDirection.Y := (-relativePosition2.x * radius_ + relativePosition2.y * leg2) / distSq2;
        end
        else
        begin
            //Right vertex non-convex; right leg extends cut-off line.
            rightLegDirection := obstacle1.unitDir_;
        end;
    end;

    (*
     * Legs can never point into neighboring edge when convex vertex,
     * take cutoff-line of neighboring edge instead. If velocity projected on
     * "foreign" leg, no constraint is added.
     *)

    leftNeighbor := obstacle1.prevObstacle;

    isLeftLegForeign := false;
    isRightLegForeign := false;

    if (obstacle1.isConvex_) and (det(leftLegDirection, Vector2Neg(leftNeighbor.unitDir_)) >= 0) then
    begin
        //Left leg points into obstacle. */
        leftLegDirection := Vector2Neg(leftNeighbor.unitDir_);
        isLeftLegForeign := true;
    end;

    if (obstacle2.isConvex_) and (det(rightLegDirection, obstacle2.unitDir_) <= 0) then
    begin
        //Right leg points into obstacle. */
        rightLegDirection := obstacle2.unitDir_;
        isRightLegForeign := true;
    end;

    // Compute cut-off centers. */
    Vector2 leftCutoff = invTimeHorizonObst * (obstacle1.point_ - position_);
    Vector2 rightCutoff = invTimeHorizonObst * (obstacle2.point_ - position_);
    Vector2 cutoffVec = rightCutoff - leftCutoff;

    /* Project current velocity on velocity obstacle. */

    /* Check if current velocity is projected on cutoff circles. */
    float t = (obstacle1 == obstacle2 ? 0.5f : ((velocity_ - leftCutoff) * cutoffVec) / RVOMath.absSq(cutoffVec));
    float tLeft = ((velocity_ - leftCutoff) * leftLegDirection);
    float tRight = ((velocity_ - rightCutoff) * rightLegDirection);

    if ((t < 0.0f && tLeft < 0.0f) || (obstacle1 == obstacle2 && tLeft < 0.0f && tRight < 0.0f))
    begin
        /* Project on left cut-off circle. */
        Vector2 unitW = RVOMath.normalize(velocity_ - leftCutoff);

        line.direction = new Vector2(unitW.y(), -unitW.x());
        line.point = leftCutoff + radius_ * invTimeHorizonObst * unitW;
        orcaLines_.Add(line);
        continue;
    end;
    else if (t > 1.0f && tRight < 0.0f)
    begin
        /* Project on right cut-off circle. */
        Vector2 unitW = RVOMath.normalize(velocity_ - rightCutoff);

        line.direction = new Vector2(unitW.y(), -unitW.x());
        line.point = rightCutoff + radius_ * invTimeHorizonObst * unitW;
        orcaLines_.Add(line);
        continue;
    end;

    /*
     * Project on left leg, right leg, or cut-off line, whichever is closest
     * to velocity.
     */
    float distSqCutoff = ((t < 0.0f || t > 1.0f || obstacle1 == obstacle2) ? float.PositiveInfinity : RVOMath.absSq(velocity_ - (leftCutoff + t * cutoffVec)));
    float distSqLeft = ((tLeft < 0.0f) ? float.PositiveInfinity : RVOMath.absSq(velocity_ - (leftCutoff + tLeft * leftLegDirection)));
    float distSqRight = ((tRight < 0.0f) ? float.PositiveInfinity : RVOMath.absSq(velocity_ - (rightCutoff + tRight * rightLegDirection)));

    if (distSqCutoff <= distSqLeft && distSqCutoff <= distSqRight)
    begin
        /* Project on cut-off line. */
        line.direction = -obstacle1.unitDir_;
        line.point = leftCutoff + radius_ * invTimeHorizonObst * new Vector2(-line.direction.y(), line.direction.x());
        orcaLines_.Add(line);
        continue;
    end;
    else if (distSqLeft <= distSqRight)
    begin
        /* Project on left leg. */
        if (isLeftLegForeign)
        begin
            continue;
        end;

        line.direction = leftLegDirection;
        line.point = leftCutoff + radius_ * invTimeHorizonObst * new Vector2(-line.direction.y(), line.direction.x());
        orcaLines_.Add(line);
        continue;
    end;
    else
    begin
        /* Project on right leg. */
        if (isRightLegForeign)
        begin
            continue;
        end;

        line.direction = -rightLegDirection;
        line.point = rightCutoff + radius_ * invTimeHorizonObst * new Vector2(-line.direction.y(), line.direction.x());
        orcaLines_.Add(line);
        continue;
    end;
  end;

  int numObstLines = orcaLines_.Count;

  float invTimeHorizon = 1.0f / timeHorizon_;

  /* Create agent ORCA lines. */
  for (int i = 0; i < agentNeighbors_.Count; ++i)
  begin
      Agent other = agentNeighbors_[i].Value;

      Vector2 relativePosition = other.position_ - position_;
      Vector2 relativeVelocity = velocity_ - other.velocity_;
      float distSq = RVOMath.absSq(relativePosition);
      float combinedRadius = radius_ + other.radius_;
      float combinedRadiusSq = RVOMath.sqr(combinedRadius);

      Line line;
      Vector2 u;

      if (distSq > combinedRadiusSq)
      begin
          /* No collision. */
          Vector2 w = relativeVelocity - invTimeHorizon * relativePosition;
          /* Vector from cutoff center to relative velocity. */
          float wLengthSq = RVOMath.absSq(w);

          float dotProduct1 = w * relativePosition;

          if (dotProduct1 < 0.0f && RVOMath.sqr(dotProduct1) > combinedRadiusSq * wLengthSq)
          begin
              /* Project on cut-off circle. */
              float wLength = RVOMath.sqrt(wLengthSq);
              Vector2 unitW = w / wLength;

              line.direction = new Vector2(unitW.y(), -unitW.x());
              u = (combinedRadius * invTimeHorizon - wLength) * unitW;
          end;
          else
          begin
              /* Project on legs. */
              float leg = RVOMath.sqrt(distSq - combinedRadiusSq);

              if (RVOMath.det(relativePosition, w) > 0.0f)
              begin
                  /* Project on left leg. */
                  line.direction = new Vector2(relativePosition.x() * leg - relativePosition.y() * combinedRadius, relativePosition.x() * combinedRadius + relativePosition.y() * leg) / distSq;
              end;
              else
              begin
                  /* Project on right leg. */
                  line.direction = -new Vector2(relativePosition.x() * leg + relativePosition.y() * combinedRadius, -relativePosition.x() * combinedRadius + relativePosition.y() * leg) / distSq;
              end;

              float dotProduct2 = relativeVelocity * line.direction;

              u = dotProduct2 * line.direction - relativeVelocity;
          end;
      end;
      else
      begin
          /* Collision. Project on cut-off circle of time timeStep. */
          float invTimeStep = 1.0f / Simulator.Instance.timeStep_;

          /* Vector from cutoff center to relative velocity. */
          Vector2 w = relativeVelocity - invTimeStep * relativePosition;

          float wLength = RVOMath.abs(w);
          Vector2 unitW = w / wLength;

          line.direction = new Vector2(unitW.y(), -unitW.x());
          u = (combinedRadius * invTimeStep - wLength) * unitW;
      end;

      line.point = velocity_ + 0.5f * u;
      orcaLines_.Add(line);
  end;

  int lineFail = linearProgram2(orcaLines_, maxSpeed_, prefVelocity_, false, ref newVelocity_);

  if (lineFail < orcaLines_.Count)
  begin
      linearProgram3(orcaLines_, numObstLines, lineFail, maxSpeed_, ref newVelocity_);
  end;
end;

procedure TRVOAgent.insertAgentNeighbor(agent: TRVOAgent; rangeSq: Single);
begin
  if (Self <> agent) then
  begin
      float distSq := absSq(position_ - agent.position_);

      if (distSq < rangeSq) then
      begin
          if (agentNeighbors_.Count < maxNeighbors_) then
          begin
              agentNeighbors_.Add(new KeyValuePair<float, Agent>(distSq, agent));
          end;
          int i = agentNeighbors_.Count - 1;
          while (i != 0 && distSq < agentNeighbors_[i - 1].Key) do
          begin
              agentNeighbors_[i] = agentNeighbors_[i - 1];
              Dec(i);
          end;
          agentNeighbors_[i] = new KeyValuePair<float, Agent>(distSq, agent);

          if (agentNeighbors_.Count == maxNeighbors_) then
          begin
              rangeSq = agentNeighbors_[agentNeighbors_.Count-1].Key;
          end;
      end;
  end;
end;

procedure TRVOAgent.insertObstacleNeighbor(obstacle: TRVOObstacle; rangeSq: Single);
begin
  Obstacle nextObstacle = obstacle.nextObstacle;

  float distSq = RVOMath.distSqPointLineSegment(obstacle.point_, nextObstacle.point_, position_);

  if (distSq < rangeSq)
  begin
      obstacleNeighbors_.Add(new KeyValuePair<float, Obstacle>(distSq, obstacle));

      int i = obstacleNeighbors_.Count - 1;
      while (i != 0 && distSq < obstacleNeighbors_[i - 1].Key)
      begin
          obstacleNeighbors_[i] = obstacleNeighbors_[i - 1];
          --i;
      end;
      obstacleNeighbors_[i] = new KeyValuePair<float, Obstacle>(distSq, obstacle);
  end;
end;

procedure TRVOAgent.update;
begin
  velocity_ = newVelocity_;
  position_ += velocity_ * Simulator.Instance.timeStep_;
end;

bool linearProgram1(IList<Line> lines, int lineNo, float radius, Vector2 optVelocity, bool directionOpt, ref Vector2 result)
begin
  float dotProduct = lines[lineNo].point * lines[lineNo].direction;
  float discriminant = RVOMath.sqr(dotProduct) + RVOMath.sqr(radius) - RVOMath.absSq(lines[lineNo].point);

  if (discriminant < 0.0f)
  begin
      /* Max speed circle fully invalidates line lineNo. */
      return false;
  end;

  float sqrtDiscriminant = RVOMath.sqrt(discriminant);
  float tLeft = -dotProduct - sqrtDiscriminant;
  float tRight = -dotProduct + sqrtDiscriminant;

  for (int i = 0; i < lineNo; ++i)
  begin
      float denominator = RVOMath.det(lines[lineNo].direction, lines[i].direction);
      float numerator = RVOMath.det(lines[i].direction, lines[lineNo].point - lines[i].point);

      if (RVOMath.fabs(denominator) <= RVOMath.RVO_EPSILON)
      begin
          /* Lines lineNo and i are (almost) parallel. */
          if (numerator < 0.0f)
          begin
              return false;
          end;
          else
          begin
              continue;
          end;
      end;

      float t = numerator / denominator;

      if (denominator >= 0.0f)
      begin
          /* Line i bounds line lineNo on the right. */
          tRight = Math.Min(tRight, t);
      end;
      else
      begin
          /* Line i bounds line lineNo on the left. */
          tLeft = Math.Max(tLeft, t);
      end;

      if (tLeft > tRight)
      begin
          return false;
      end;
  end;

  if (directionOpt)
  begin
      /* Optimize direction. */
      if (optVelocity * lines[lineNo].direction > 0.0f)
      begin
          /* Take right extreme. */
          result = lines[lineNo].point + tRight * lines[lineNo].direction;
      end;
      else
      begin
          /* Take left extreme. */
          result = lines[lineNo].point + tLeft * lines[lineNo].direction;
      end;
  end;
  else
  begin
      /* Optimize closest point. */
      float t = lines[lineNo].direction * (optVelocity - lines[lineNo].point);

      if (t < tLeft)
      begin
          result = lines[lineNo].point + tLeft * lines[lineNo].direction;
      end;
      else if (t > tRight)
      begin
          result = lines[lineNo].point + tRight * lines[lineNo].direction;
      end;
      else
      begin
          result = lines[lineNo].point + t * lines[lineNo].direction;
      end;
  end;

  return true;
end;

int linearProgram2(IList<Line> lines, float radius, Vector2 optVelocity, bool directionOpt, ref Vector2 result)
begin
  if (directionOpt)
  begin
      /*
       * Optimize direction. Note that the optimization velocity is of unit
       * length in this case.
       */
      result = optVelocity * radius;
  end;
  else if (RVOMath.absSq(optVelocity) > RVOMath.sqr(radius))
  begin
      /* Optimize closest point and outside circle. */
      result = RVOMath.normalize(optVelocity) * radius;
  end;
  else
  begin
      /* Optimize closest point and inside circle. */
      result = optVelocity;
  end;

  for (int i = 0; i < lines.Count; ++i)
  begin
      if (RVOMath.det(lines[i].direction, lines[i].point - result) > 0.0f)
      begin
          /* Result does not satisfy constraint i. Compute new optimal result. */
          Vector2 tempResult = result;
          if (!linearProgram1(lines, i, radius, optVelocity, directionOpt, ref result))
          begin
              result = tempResult;
              return i;
          end;
      end;
  end;

  return lines.Count;
end;

void linearProgram3(IList<Line> lines, int numObstLines, int beginLine, float radius, ref Vector2 result)
begin
  float distance = 0.0f;

  for (int i = beginLine; i < lines.Count; ++i)
  begin
      if (RVOMath.det(lines[i].direction, lines[i].point - result) > distance)
      begin
          /* Result does not satisfy constraint of line i. */
          //std::vector<Line> projLines(lines.begin(), lines.begin() + numObstLines);
          IList<Line> projLines = new List<Line>();
          for (int ii = 0; ii < numObstLines; ++ii)
          begin
              projLines.Add(lines[ii]);
          end;

          for (int j = numObstLines; j < i; ++j)
          begin
              Line line;

              float determinant = RVOMath.det(lines[i].direction, lines[j].direction);

              if (RVOMath.fabs(determinant) <= RVOMath.RVO_EPSILON)
              begin
                  /* Line i and line j are parallel. */
                  if (lines[i].direction * lines[j].direction > 0.0f)
                  begin
                      /* Line i and line j point in the same direction. */
                      continue;
                  end;
                  else
                  begin
                      /* Line i and line j point in opposite direction. */
                      line.point = 0.5f * (lines[i].point + lines[j].point);
                  end;
              end;
              else
              begin
                  line.point = lines[i].point + (RVOMath.det(lines[j].direction, lines[i].point - lines[j].point) / determinant) * lines[i].direction;
              end;

              line.direction = RVOMath.normalize(lines[j].direction - lines[i].direction);
              projLines.Add(line);
          end;

          Vector2 tempResult = result;
          if (linearProgram2(projLines, radius, new Vector2(-lines[i].direction.y(), lines[i].direction.x()), true, ref result) < projLines.Count)
          begin
              (* This should in principle not happen.  The result is by definition
               * already in the feasible region of this linear program. If it fails,
               * it is due to small floating point error, and the current result is
               * kept.
               *)
              result = tempResult;
          end;

          distance = det(lines[i].direction, lines[i].point - result);
      end;
  end;
end;

end.
