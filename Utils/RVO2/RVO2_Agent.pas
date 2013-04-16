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
uses Classes, Math,
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
    agentNeighbors_: array of TRVOKeyValueAgent;
    maxNeighbors_: Integer;
    maxSpeed_: Single;
    neighborDist_: Single;
    newVelocity_: TRVOVector2;
    obstacleNeighbors_: array of TRVOKeyValueObstacle;
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
    function linearProgram1(lines: TList; lineNo: Integer; radius: Single; optVelocity: TRVOVector2; directionOpt: Boolean; var result_1: TRVOVector2): Boolean;
    function linearProgram2(lines: TList; radius: Single; optVelocity: TRVOVector2; directionOpt: Boolean; var result_1: TRVOVector2): Integer;
    procedure linearProgram3(lines: TList; numObstLines, beginLine: Integer; radius: Single; var result_1: TRVOVector2);
  end;


implementation
uses RVO2_Line, RVO2_Math, RVO2_Simulator;


{ TRVOAgent }
constructor TRVOAgent.Create;
begin

  orcaLines_ := TList.Create;
end;


procedure TRVOAgent.computeNeighbors;
var
  rangeSq: Single;
begin
  SetLength(obstacleNeighbors_, 0);
  rangeSq := Sqr(timeHorizonObst_ * maxSpeed_ + radius_);
  gSimulator.kdTree_.computeObstacleNeighbors(Self, rangeSq);

  SetLength(agentNeighbors_, 0);
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
  leftCutoff, rightCutoff, cutoffVec: TRVOVector2;
  t, tLeft, tRight: Single;
  unitW: TRVOVector2;
  distSqCutoff, distSqLeft, distSqRight: Single;
  numObstLines: Integer;
  invTimeHorizon: Single;

  other: TRVOAgent;
  relativePosition, relativeVelocity:TRVOVector2;
  distSq, combinedRadius, combinedRadiusSq: Single;
  u,w: TRVOVector2;
  wLengthSq, dotProduct1: Single;
  wLength: Single;
  leg: Single;
  dotProduct2: Single;
  invTimeStep: Single;
  lineFail: Integer;
begin
  orcaLines_.Clear;

  invTimeHorizonObst := 1 / timeHorizonObst_;

  //Create obstacle ORCA lines.
  for i := 0 to Length(obstacleNeighbors_) - 1 do
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
    leftCutoff := Vector2Scale(Vector2Sub(obstacle1.point_, position_), invTimeHorizonObst);
    rightCutoff := Vector2Scale(Vector2Sub(obstacle2.point_, position_), invTimeHorizonObst);
    cutoffVec := Vector2Sub(rightCutoff, leftCutoff);

    // Project current velocity on velocity obstacle.

    // Check if current velocity is projected on cutoff circles.
    t := IfThen(obstacle1 = obstacle2, 0.5, Vector2Mul(Vector2Sub(velocity_, leftCutoff), cutoffVec) / absSq(cutoffVec));
    tLeft := Vector2Mul(Vector2Sub(velocity_, leftCutoff), leftLegDirection);
    tRight := Vector2Mul(Vector2Sub(velocity_, rightCutoff), rightLegDirection);

    if ((t < 0) and (tLeft < 0) or (obstacle1 = obstacle2) and (tLeft < 0) and (tRight < 0)) then
    begin
        // Project on left cut-off circle.
        unitW := normalize(Vector2Sub(velocity_, leftCutoff));

        line.direction.X := unitW.y;
        line.direction.Y := -unitW.x;
        line.point := Vector2Add(leftCutoff, Vector2Scale(unitW, radius_ * invTimeHorizonObst));
        orcaLines_.Add(line);
        continue;
    end
    else if (t > 1) and (tRight < 0) then
    begin
        // Project on right cut-off circle.
        unitW := normalize(Vector2Sub(velocity_, rightCutoff));

        line.direction.X := unitW.y;
        line.direction.Y := -unitW.x;
        line.point := Vector2Add(rightCutoff, Vector2Scale(unitW, radius_ * invTimeHorizonObst));
        orcaLines_.Add(line);
        continue;
    end;

    //Project on left leg, right leg, or cut-off line, whichever is closest
    //to velocity.

    distSqCutoff := IfThen((t < 0) or (t > 1) or (obstacle1 = obstacle2), MaxSingle, absSq(Vector2Sub(velocity_, Vector2Add(leftCutoff, Vector2Scale(t, cutoffVec)))));
    distSqLeft := IfThen(tLeft < 0, MaxSingle, absSq(Vector2Sub(velocity_, Vector2Add(leftCutoff, Vector2Scale(tLeft, leftLegDirection)))));
    distSqRight := IfThen(tRight < 0, MaxSingle, absSq(Vector2Sub(velocity_, Vector2Add(rightCutoff, Vector2Scale(tRight, rightLegDirection)))));

    if (distSqCutoff <= distSqLeft) and (distSqCutoff <= distSqRight) then
    begin
      // Project on cut-off line.
      line.direction := Vector2Neg(obstacle1.unitDir_);
      line.point := Vector2Add(leftCutoff, Vector2Scale(radius_ * invTimeHorizonObst, Vector2(-line.direction.y, line.direction.x)));
      orcaLines_.Add(line);
      continue;
    end
    else if (distSqLeft <= distSqRight) then
    begin
      // Project on left leg.
      if (isLeftLegForeign) then
      begin
        continue;
      end;

      line.direction := leftLegDirection;
      line.point := Vector2Add(leftCutoff, Vector2Scale(radius_ * invTimeHorizonObst, Vector2(-line.direction.y, line.direction.x)));
      orcaLines_.Add(line);
      continue;
    end
    else
    begin
      //Project on right leg.
      if (isRightLegForeign) then
      begin
        continue;
      end;

      line.direction := Vector2Neg(rightLegDirection);
      line.point := Vector2Add(rightCutoff, Vector2Scale(radius_ * invTimeHorizonObst, Vector2(-line.direction.y, line.direction.x)));
      orcaLines_.Add(line);
      continue;
    end;
  end;

  numObstLines := orcaLines_.Count;

  invTimeHorizon := 1 / timeHorizon_;

  // Create agent ORCA lines.
  for i := 0 to Length(agentNeighbors_) - 1 do
  begin
      other := agentNeighbors_[i].Value;

      relativePosition := Vector2Sub(other.position_, position_);
      relativeVelocity := Vector2Sub(velocity_, other.velocity_);
      distSq := absSq(relativePosition);
      combinedRadius := radius_ + other.radius_;
      combinedRadiusSq := sqr(combinedRadius);

      if (distSq > combinedRadiusSq) then
      begin
          // No collision
          w := Vector2Sub(relativeVelocity, Vector2Scale(invTimeHorizon, relativePosition));
          //Vector from cutoff center to relative velocity.
          wLengthSq := absSq(w);

          dotProduct1 := Vector2Mul(w, relativePosition);

          if (dotProduct1 < 0) and (sqr(dotProduct1) > combinedRadiusSq * wLengthSq) then
          begin
              // Project on cut-off circle.
              wLength := sqrt(wLengthSq);
              unitW := Vector2Scale(w, 1 / wLength);

              line.direction := Vector2(unitW.y, -unitW.x);
              u := Vector2Scale(combinedRadius * invTimeHorizon - wLength, unitW);
          end
          else
          begin
              // Project on legs.
              leg := sqrt(distSq - combinedRadiusSq);

              if (det(relativePosition, w) > 0) then
              begin
                  // Project on left leg. */
                  line.direction := Vector2Scale(Vector2(relativePosition.x * leg - relativePosition.y * combinedRadius, relativePosition.x * combinedRadius + relativePosition.y * leg), 1 / distSq);
              end
              else
              begin
                  // Project on right leg. */
                  line.direction := Vector2Scale(Vector2(relativePosition.x * leg + relativePosition.y * combinedRadius, -relativePosition.x * combinedRadius + relativePosition.y * leg), -1 / distSq);
              end;

              dotProduct2 := Vector2Mul(relativeVelocity, line.direction);

              u := Vector2Sub(Vector2Scale(dotProduct2, line.direction), relativeVelocity);
          end;
      end
      else
      begin
          // Collision. Project on cut-off circle of time timeStep. */
          invTimeStep := 1 / gSimulator.timeStep_;

          // Vector from cutoff center to relative velocity. */
          w := Vector2Sub(relativeVelocity, Vector2Scale(invTimeStep, relativePosition));

          wLength := abs(w);
          unitW := Vector2Scale(w, 1 / wLength);

          line.direction := Vector2(unitW.y, -unitW.x);
          u := Vector2Scale(combinedRadius * invTimeStep - wLength, unitW);
      end;

      line.point := Vector2Add(velocity_, Vector2Scale(0.5, u));
      orcaLines_.Add(line);
  end;

  lineFail := linearProgram2(orcaLines_, maxSpeed_, prefVelocity_, false, newVelocity_);

  if (lineFail < orcaLines_.Count) then
  begin
    linearProgram3(orcaLines_, numObstLines, lineFail, maxSpeed_, newVelocity_);
  end;
end;

procedure TRVOAgent.insertAgentNeighbor(agent: TRVOAgent; rangeSq: Single);
var
  distSq: Single;
  i: Integer;
begin
  if (Self <> agent) then
  begin
    distSq := absSq(Vector2Sub(position_, agent.position_));

    if (distSq < rangeSq) then
    begin
      if (Length(agentNeighbors_) < maxNeighbors_) then
      begin
        SetLength(agentNeighbors_, Length(agentNeighbors_) + 1);
        agentNeighbors_[Length(agentNeighbors_)-1].Key := distSq;
        agentNeighbors_[Length(agentNeighbors_)-1].Value := agent;
      end;
      i := Length(agentNeighbors_) - 1;
      while (i <> 0) and (distSq < agentNeighbors_[i - 1].Key) do
      begin
        agentNeighbors_[i] := agentNeighbors_[i - 1];
        Dec(i);
      end;
      agentNeighbors_[i].Key := distSq;
      agentNeighbors_[i].Value := agent;

      if (Length(agentNeighbors_) = maxNeighbors_) then
      begin
        rangeSq := agentNeighbors_[Length(agentNeighbors_)-1].Key;
      end;
    end;
  end;
end;

procedure TRVOAgent.insertObstacleNeighbor(obstacle: TRVOObstacle; rangeSq: Single);
var
  nextObstacle: TRVOObstacle;
  distSq: Single;
  i: Integer;
begin
  nextObstacle := obstacle.nextObstacle;

  distSq := distSqPointLineSegment(obstacle.point_, nextObstacle.point_, position_);

  if (distSq < rangeSq) then
  begin
    SetLength(obstacleNeighbors_, Length(obstacleNeighbors_) + 1);
    obstacleNeighbors_[Length(obstacleNeighbors_) - 1].Key := distSq;
    obstacleNeighbors_[Length(obstacleNeighbors_) - 1].Value := obstacle;

      i := Length(obstacleNeighbors_) - 1;
      while (i <> 0) and (distSq < obstacleNeighbors_[i - 1].Key) do
      begin
          obstacleNeighbors_[i] := obstacleNeighbors_[i - 1];
          Dec(i);
      end;
      obstacleNeighbors_[i].Key := distSq;
      obstacleNeighbors_[i].Value := obstacle;
  end;
end;

procedure TRVOAgent.update;
begin
  velocity_ := newVelocity_;
  position_ := Vector2Add(position_, Vector2Scale(velocity_, gSimulator.timeStep_));
end;

function TRVOAgent.linearProgram1(lines: TList; lineNo: Integer; radius: Single; optVelocity: TRVOVector2; directionOpt: Boolean; var result_1: TRVOVector2): Boolean;
var
  dotProduct, discriminant: Single;
  sqrtDiscriminant, tLeft, tRight: Single;
  i: Integer;
  denominator, numerator: Single;
  t: Single;
begin
  dotProduct := Vector2Mul(TRVOLine(lines[lineNo]).point, TRVOLine(lines[lineNo]).direction);
  discriminant := sqr(dotProduct) + sqr(radius) - absSq(TRVOLine(lines[lineNo]).point);

  if (discriminant < 0) then
  begin
    //Max speed circle fully invalidates line lineNo
    Result := False;
    Exit;
  end;

  sqrtDiscriminant := sqrt(discriminant);
  tLeft := -dotProduct - sqrtDiscriminant;
  tRight := -dotProduct + sqrtDiscriminant;

  for i := 0 to lineNo - 1 do
  begin
    denominator := det(TRVOLine(lines[lineNo]).direction, TRVOLine(lines[i]).direction);
    numerator := det(TRVOLine(lines[i]).direction, Vector2Sub(TRVOLine(lines[lineNo]).point, TRVOLine(lines[i]).point));

    if (System.Abs(denominator) <= RVO_EPSILON) then
    begin
      // Lines lineNo and i are (almost) parallel. */
      if (numerator < 0) then
      begin
        Result := False;
        Exit;
      end
      else
      begin
        continue;
      end;
    end;

    t := numerator / denominator;

    if (denominator >= 0) then
    begin
      // Line i bounds line lineNo on the right. */
      tRight := Min(tRight, t);
    end
    else
    begin
      // Line i bounds line lineNo on the left. */
      tLeft := Max(tLeft, t);
    end;

    if (tLeft > tRight) then
    begin
      Result := False;
      Exit;
    end;
  end;

  if (directionOpt) then
  begin
    // Optimize direction. */
    if Vector2Mul(optVelocity, TRVOLine(lines[lineNo]).direction) > 0 then
    begin
      // Take right extreme. */
      result_1 := Vector2Add(TRVOLine(lines[lineNo]).point, Vector2Scale(tRight, TRVOLine(lines[lineNo]).direction));
    end
    else
    begin
      // Take left extreme. */
      result_1 := Vector2Add(TRVOLine(lines[lineNo]).point, Vector2Scale(tLeft, TRVOLine(lines[lineNo]).direction));
    end;
  end
  else
  begin
    // Optimize closest point. */
    t := Vector2Mul(TRVOLine(lines[lineNo]).direction, Vector2Sub(optVelocity, TRVOLine(lines[lineNo]).point));

    if (t < tLeft) then
    begin
      result_1 := Vector2Add(TRVOLine(lines[lineNo]).point, Vector2Scale(tLeft, TRVOLine(lines[lineNo]).direction));
    end
    else if (t > tRight) then
    begin
      result_1 := Vector2Add(TRVOLine(lines[lineNo]).point, Vector2Scale(tRight, TRVOLine(lines[lineNo]).direction));
    end
    else
    begin
      result_1 := Vector2Add(TRVOLine(lines[lineNo]).point, Vector2Scale(t, TRVOLine(lines[lineNo]).direction));
    end;
  end;

  Result := True;
end;

function TRVOAgent.linearProgram2(lines: TList; radius: Single; optVelocity: TRVOVector2; directionOpt: Boolean; var result_1: TRVOVector2): Integer;
var
  i: Integer;
  tempResult: TRVOVector2;
begin
  if (directionOpt) then
  begin
      //Optimize direction. Note that the optimization velocity is of unit
      //length in this case.
      result_1 := Vector2Scale(optVelocity, radius);
  end
  else if (absSq(optVelocity) > sqr(radius)) then
  begin
      // Optimize closest point and outside circle. */
      result_1 := Vector2Scale(normalize(optVelocity), radius);
  end
  else
  begin
      // Optimize closest point and inside circle. */
      result_1 := optVelocity;
  end;

  for i := 0 to lines.Count - 1 do
  begin
      if (det(TRVOLine(lines[i]).direction, Vector2Sub(TRVOLine(lines[i]).point, result_1)) > 0) then
      begin
          // Result does not satisfy constraint i. Compute new optimal result. */
          tempResult := result_1;
          if ( not linearProgram1(lines, i, radius, optVelocity, directionOpt, result_1)) then
          begin
              result_1 := tempResult;
              Result := i;
              Exit;
          end;
      end;
  end;

  Result := lines.Count;
end;

procedure TRVOAgent.linearProgram3(lines: TList; numObstLines, beginLine: Integer; radius: Single; var result_1: TRVOVector2);
var
  distance: Single;
  i,ii,j: Integer;
  line: TRVOLine;
  determinant: Single;
  projLines: TList;
  tempResult: TRVOVector2;
begin
  distance := 0;

  for i := beginLine to lines.Count - 1 do
  begin
    if (det(TRVOLine(lines[i]).direction, Vector2Sub(TRVOLine(lines[i]).point, result_1)) > distance) then
    begin
      // Result does not satisfy constraint of line i. */
      //std::vector<Line> projLines(lines.begin(), lines.begin() + numObstLines);
      projLines := TList.Create;
      for ii := 0 to numObstLines - 1 do
      begin
        projLines.Add(lines[ii]);
      end;

      for j := numObstLines to i - 1 do
      begin
        determinant := det(TRVOLine(lines[i]).direction, TRVOLine(lines[j]).direction);

        if (System.Abs(determinant) <= RVO_EPSILON) then
        begin
          // Line i and line j are parallel. */
          if Vector2Mul(TRVOLine(lines[i]).direction, TRVOLine(lines[j]).direction) > 0 then
          begin
            // Line i and line j point in the same direction. */
            continue;
          end
          else
          begin
            // Line i and line j point in opposite direction. */
            line.point := Vector2Scale(0.5, Vector2Add(TRVOLine(lines[i]).point, TRVOLine(lines[j]).point));
          end;
        end
        else
        begin
          line.point := Vector2Add(TRVOLine(lines[i]).point,
            Vector2Scale(det(TRVOLine(lines[j]).direction, Vector2Sub(TRVOLine(lines[i]).point, TRVOLine(lines[j]).point)) / determinant,
            TRVOLine(lines[i]).direction));
        end;

        line.direction := normalize(Vector2Sub(TRVOLine(lines[j]).direction, TRVOLine(lines[i]).direction));
        projLines.Add(line);
      end;

      tempResult := result_1;
      if (linearProgram2(projLines, radius, Vector2(-TRVOLine(lines[i]).direction.y, TRVOLine(lines[i]).direction.x), true, result_1) < projLines.Count) then
      begin
        (* This should in principle not happen.  The result is by definition
         * already in the feasible region of this linear program. If it fails,
         * it is due to small floating point error, and the current result is
         * kept.
         *)
        result_1 := tempResult;
      end;

      distance := det(TRVOLine(lines[i]).direction, Vector2Sub(TRVOLine(lines[i]).point, result_1));
    end;
  end;
end;

end.
