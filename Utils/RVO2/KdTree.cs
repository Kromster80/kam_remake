/*
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
 */

using System.Collections.Generic;
using System;

namespace RVO
{
    internal class KdTree
    {
        private struct FloatPair
        {
            internal float _a;
            internal float _b;

            public FloatPair(float a, float b)
            {
                _a = a;
                _b = b;
            }

            public static bool operator <(FloatPair lhs, FloatPair rhs)
            {
                return (lhs._a < rhs._a || !(rhs._a < lhs._a) && lhs._b < rhs._b);
            }
            public static bool operator <=(FloatPair lhs, FloatPair rhs)
            {
                return (lhs._a == rhs._a && lhs._b==rhs._b) || lhs < rhs;
            }
            public static bool operator >(FloatPair lhs, FloatPair rhs)
            {
                return !(lhs<=rhs);
            }
            public static bool operator >=(FloatPair lhs, FloatPair rhs)
            {
                return !(lhs < rhs);
            }
        }

        private const int MAX_LEAF_SIZE = 10;

        private struct AgentTreeNode
        {
            internal int begin;
            internal int end;
            internal int left;
            internal float maxX;
            internal float maxY;
            internal float minX;
            internal float minY;
            internal int right;
        }
        private class ObstacleTreeNode
        {
            internal ObstacleTreeNode left;
            internal Obstacle obstacle;
            internal ObstacleTreeNode right;
        };

        //private IList<Agent> agents_ = new List<Agent>();
        //private IList<AgentTreeNode> agentTree_ = new List<AgentTreeNode>();
        private Agent[] agents_;
        private AgentTreeNode[] agentTree_;

        private ObstacleTreeNode obstacleTree_;

        internal void buildAgentTree()
        {
            if (agents_==null || agents_.Length != Simulator.Instance.agents_.Count)
            {
                agents_ = new Agent[Simulator.Instance.agents_.Count];
                for (int i = 0; i < agents_.Length; ++i)
                {
                    agents_[i] = Simulator.Instance.agents_[i];
                }

                agentTree_ = new AgentTreeNode[2 * agents_.Length];
                for (int i = 0; i < agentTree_.Length; ++i)
                {
                    agentTree_[i] = new AgentTreeNode();
                }
            }

            if (agents_.Length != 0)
            {
                buildAgentTreeRecursive(0, agents_.Length, 0);
            }
        }

        void buildAgentTreeRecursive(int begin, int end, int node)
        {
            agentTree_[node].begin = begin;
            agentTree_[node].end = end;
            agentTree_[node].minX = agentTree_[node].maxX = agents_[begin].position_.x_;
            agentTree_[node].minY = agentTree_[node].maxY = agents_[begin].position_.y_;

            for (int i = begin + 1; i < end; ++i)
            {
                agentTree_[node].maxX = Math.Max(agentTree_[node].maxX, agents_[i].position_.x_);
                agentTree_[node].minX = Math.Min(agentTree_[node].minX, agents_[i].position_.x_);
                agentTree_[node].maxY = Math.Max(agentTree_[node].maxY, agents_[i].position_.y_);
                agentTree_[node].minY = Math.Min(agentTree_[node].minY, agents_[i].position_.y_);
            }

            if (end - begin > MAX_LEAF_SIZE)
            {
                /* No leaf node. */
                bool isVertical = (agentTree_[node].maxX - agentTree_[node].minX > agentTree_[node].maxY - agentTree_[node].minY);
                float splitValue = (isVertical ? 0.5f * (agentTree_[node].maxX + agentTree_[node].minX) : 0.5f * (agentTree_[node].maxY + agentTree_[node].minY));

                int left = begin;
                int right = end;

                while (left < right)
                {
                    while (left < right && (isVertical ? agents_[left].position_.x_ : agents_[left].position_.y_) < splitValue)
                    {
                        ++left;
                    }

                    while (right > left && (isVertical ? agents_[right - 1].position_.x_ : agents_[right - 1].position_.y_) >= splitValue)
                    {
                        --right;
                    }

                    if (left < right)
                    {
                        Agent tmp = agents_[left];
                        agents_[left] = agents_[right - 1];
                        agents_[right - 1] = tmp;
                        ++left;
                        --right;
                    }
                }

                int leftSize = left - begin;

                if (leftSize == 0)
                {
                    ++leftSize;
                    ++left;
                    ++right;
                }

                agentTree_[node].left = node + 1;
                agentTree_[node].right = node + 1 + (2 * leftSize - 1);

                buildAgentTreeRecursive(begin, left, agentTree_[node].left);
                buildAgentTreeRecursive(left, end, agentTree_[node].right);
            }
        }

        internal void buildObstacleTree()
        {
            obstacleTree_ = new ObstacleTreeNode();

            IList<Obstacle> obstacles = new List<Obstacle>(Simulator.Instance.obstacles_.Count);

            for (int i = 0; i < Simulator.Instance.obstacles_.Count; ++i)
            {
                obstacles.Add(Simulator.Instance.obstacles_[i]);
            }

            obstacleTree_ = buildObstacleTreeRecursive(obstacles);
        }


        ObstacleTreeNode buildObstacleTreeRecursive(IList<Obstacle> obstacles)
        {
            if (obstacles.Count == 0)
            {
                return null;
            }
            else
            {
                ObstacleTreeNode node = new ObstacleTreeNode();

                int optimalSplit = 0;
                int minLeft = obstacles.Count;
                int minRight = obstacles.Count;

                for (int i = 0; i < obstacles.Count; ++i)
                {
                    int leftSize = 0;
                    int rightSize = 0;

                    Obstacle obstacleI1 = obstacles[i];
                    Obstacle obstacleI2 = obstacleI1.nextObstacle;

                    /* Compute optimal split node. */
                    for (int j = 0; j < obstacles.Count; ++j)
                    {
                        if (i == j)
                        {
                            continue;
                        }

                        Obstacle obstacleJ1 = obstacles[j];
                        Obstacle obstacleJ2 = obstacleJ1.nextObstacle;

                        float j1LeftOfI = RVOMath.leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ1.point_);
                        float j2LeftOfI = RVOMath.leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ2.point_);

                        if (j1LeftOfI >= -RVOMath.RVO_EPSILON && j2LeftOfI >= -RVOMath.RVO_EPSILON)
                        {
                            ++leftSize;
                        }
                        else if (j1LeftOfI <= RVOMath.RVO_EPSILON && j2LeftOfI <= RVOMath.RVO_EPSILON)
                        {
                            ++rightSize;
                        }
                        else
                        {
                            ++leftSize;
                            ++rightSize;
                        }

                        if (new FloatPair(Math.Max(leftSize, rightSize), Math.Min(leftSize, rightSize)) >= new FloatPair(Math.Max(minLeft, minRight), Math.Min(minLeft, minRight)))
                        {
                            break;
                        }
                    }

                    if (new FloatPair(Math.Max(leftSize, rightSize), Math.Min(leftSize, rightSize)) < new FloatPair(Math.Max(minLeft, minRight), Math.Min(minLeft, minRight)))
                    {
                        minLeft = leftSize;
                        minRight = rightSize;
                        optimalSplit = i;
                    }
                }

                {
                    /* Build split node. */
                    IList<Obstacle> leftObstacles = new List<Obstacle>(minLeft);
                    for (int n = 0; n < minLeft; ++n) leftObstacles.Add(null);
                    IList<Obstacle> rightObstacles = new List<Obstacle>(minRight);
                    for (int n = 0; n < minRight; ++n) rightObstacles.Add(null);

                    int leftCounter = 0;
                    int rightCounter = 0;
                    int i = optimalSplit;

                    Obstacle obstacleI1 = obstacles[i];
                    Obstacle obstacleI2 = obstacleI1.nextObstacle;

                    for (int j = 0; j < obstacles.Count; ++j)
                    {
                        if (i == j)
                        {
                            continue;
                        }

                        Obstacle obstacleJ1 = obstacles[j];
                        Obstacle obstacleJ2 = obstacleJ1.nextObstacle;

                        float j1LeftOfI = RVOMath.leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ1.point_);
                        float j2LeftOfI = RVOMath.leftOf(obstacleI1.point_, obstacleI2.point_, obstacleJ2.point_);

                        if (j1LeftOfI >= -RVOMath.RVO_EPSILON && j2LeftOfI >= -RVOMath.RVO_EPSILON)
                        {
                            leftObstacles[leftCounter++] = obstacles[j];
                        }
                        else if (j1LeftOfI <= RVOMath.RVO_EPSILON && j2LeftOfI <= RVOMath.RVO_EPSILON)
                        {
                            rightObstacles[rightCounter++] = obstacles[j];
                        }
                        else
                        {
                            /* Split obstacle j. */
                            float t = RVOMath.det(obstacleI2.point_ - obstacleI1.point_, obstacleJ1.point_ - obstacleI1.point_) / RVOMath.det(obstacleI2.point_ - obstacleI1.point_, obstacleJ1.point_ - obstacleJ2.point_);

                            Vector2 splitpoint = obstacleJ1.point_ + t * (obstacleJ2.point_ - obstacleJ1.point_);

                            Obstacle newObstacle = new Obstacle();
                            newObstacle.point_ = splitpoint;
                            newObstacle.prevObstacle = obstacleJ1;
                            newObstacle.nextObstacle = obstacleJ2;
                            newObstacle.isConvex_ = true;
                            newObstacle.unitDir_ = obstacleJ1.unitDir_;

                            newObstacle.id_ = Simulator.Instance.obstacles_.Count;

                            Simulator.Instance.obstacles_.Add(newObstacle);

                            obstacleJ1.nextObstacle = newObstacle;
                            obstacleJ2.prevObstacle = newObstacle;

                            if (j1LeftOfI > 0.0f)
                            {
                                leftObstacles[leftCounter++] = obstacleJ1;
                                rightObstacles[rightCounter++] = newObstacle;
                            }
                            else
                            {
                                rightObstacles[rightCounter++] = obstacleJ1;
                                leftObstacles[leftCounter++] = newObstacle;
                            }
                        }
                    }

                    node.obstacle = obstacleI1;
                    node.left = buildObstacleTreeRecursive(leftObstacles);
                    node.right = buildObstacleTreeRecursive(rightObstacles);
                    return node;
                }
            }
        }

        internal void computeAgentNeighbors(Agent agent, ref float rangeSq)
        {
            queryAgentTreeRecursive(agent, ref rangeSq, 0);
        }

        internal void computeObstacleNeighbors(Agent agent, float rangeSq)
        {
            queryObstacleTreeRecursive(agent, rangeSq, obstacleTree_);
        }


        void queryAgentTreeRecursive(Agent agent, ref float rangeSq, int node)
        {
            if (agentTree_[node].end - agentTree_[node].begin <= MAX_LEAF_SIZE)
            {
                for (int i = agentTree_[node].begin; i < agentTree_[node].end; ++i)
                {
                    agent.insertAgentNeighbor(agents_[i], ref rangeSq);
                }
            }
            else
            {
                float distSqLeft = RVOMath.sqr(Math.Max(0.0f, agentTree_[agentTree_[node].left].minX - agent.position_.x_)) + RVOMath.sqr(Math.Max(0.0f, agent.position_.x_ - agentTree_[agentTree_[node].left].maxX)) + RVOMath.sqr(Math.Max(0.0f, agentTree_[agentTree_[node].left].minY - agent.position_.y_)) + RVOMath.sqr(Math.Max(0.0f, agent.position_.y_ - agentTree_[agentTree_[node].left].maxY));

                float distSqRight = RVOMath.sqr(Math.Max(0.0f, agentTree_[agentTree_[node].right].minX - agent.position_.x_)) + RVOMath.sqr(Math.Max(0.0f, agent.position_.x_ - agentTree_[agentTree_[node].right].maxX)) + RVOMath.sqr(Math.Max(0.0f, agentTree_[agentTree_[node].right].minY - agent.position_.y_)) + RVOMath.sqr(Math.Max(0.0f, agent.position_.y_ - agentTree_[agentTree_[node].right].maxY));

                if (distSqLeft < distSqRight)
                {
                    if (distSqLeft < rangeSq)
                    {
                        queryAgentTreeRecursive(agent, ref rangeSq, agentTree_[node].left);

                        if (distSqRight < rangeSq)
                        {
                            queryAgentTreeRecursive(agent, ref rangeSq, agentTree_[node].right);
                        }
                    }
                }
                else
                {
                    if (distSqRight < rangeSq)
                    {
                        queryAgentTreeRecursive(agent, ref rangeSq, agentTree_[node].right);

                        if (distSqLeft < rangeSq)
                        {
                            queryAgentTreeRecursive(agent, ref rangeSq, agentTree_[node].left);
                        }
                    }
                }

            }
        }

        void queryObstacleTreeRecursive(Agent agent, float rangeSq, ObstacleTreeNode node)
        {
            if (node == null)
            {
                return;
            }
            else
            {
                Obstacle obstacle1 = node.obstacle;
                Obstacle obstacle2 = obstacle1.nextObstacle;

                float agentLeftOfLine = RVOMath.leftOf(obstacle1.point_, obstacle2.point_, agent.position_);

                queryObstacleTreeRecursive(agent, rangeSq, (agentLeftOfLine >= 0.0f ? node.left : node.right));

                float distSqLine = RVOMath.sqr(agentLeftOfLine) / RVOMath.absSq(obstacle2.point_ - obstacle1.point_);

                if (distSqLine < rangeSq)
                {
                    if (agentLeftOfLine < 0.0f)
                    {
                        /*
                         * Try obstacle at this node only if agent is on right side of
                         * obstacle (and can see obstacle).
                         */
                        agent.insertObstacleNeighbor(node.obstacle, rangeSq);
                    }

                    /* Try other side of line. */
                    queryObstacleTreeRecursive(agent, rangeSq, (agentLeftOfLine >= 0.0f ? node.right : node.left));

                }
            }
        }

        internal bool queryVisibility(Vector2 q1, Vector2 q2, float radius)
        {
            return queryVisibilityRecursive(q1, q2, radius, obstacleTree_);
        }

        bool queryVisibilityRecursive(Vector2 q1, Vector2 q2, float radius, ObstacleTreeNode node)
        {
            if (node == null)
            {
                return true;
            }
            else
            {
                Obstacle obstacle1 = node.obstacle;
                Obstacle obstacle2 = obstacle1.nextObstacle;

                float q1LeftOfI = RVOMath.leftOf(obstacle1.point_, obstacle2.point_, q1);
                float q2LeftOfI = RVOMath.leftOf(obstacle1.point_, obstacle2.point_, q2);
                float invLengthI = 1.0f / RVOMath.absSq(obstacle2.point_ - obstacle1.point_);

                if (q1LeftOfI >= 0.0f && q2LeftOfI >= 0.0f)
                {
                    return queryVisibilityRecursive(q1, q2, radius, node.left) && ((RVOMath.sqr(q1LeftOfI) * invLengthI >= RVOMath.sqr(radius) && RVOMath.sqr(q2LeftOfI) * invLengthI >= RVOMath.sqr(radius)) || queryVisibilityRecursive(q1, q2, radius, node.right));
                }
                else if (q1LeftOfI <= 0.0f && q2LeftOfI <= 0.0f)
                {
                    return queryVisibilityRecursive(q1, q2, radius, node.right) && ((RVOMath.sqr(q1LeftOfI) * invLengthI >= RVOMath.sqr(radius) && RVOMath.sqr(q2LeftOfI) * invLengthI >= RVOMath.sqr(radius)) || queryVisibilityRecursive(q1, q2, radius, node.left));
                }
                else if (q1LeftOfI >= 0.0f && q2LeftOfI <= 0.0f)
                {
                    /* One can see through obstacle from left to right. */
                    return queryVisibilityRecursive(q1, q2, radius, node.left) && queryVisibilityRecursive(q1, q2, radius, node.right);
                }
                else
                {
                    float point1LeftOfQ = RVOMath.leftOf(q1, q2, obstacle1.point_);
                    float point2LeftOfQ = RVOMath.leftOf(q1, q2, obstacle2.point_);
                    float invLengthQ = 1.0f / RVOMath.absSq(q2 - q1);

                    return (point1LeftOfQ * point2LeftOfQ >= 0.0f && RVOMath.sqr(point1LeftOfQ) * invLengthQ > RVOMath.sqr(radius) && RVOMath.sqr(point2LeftOfQ) * invLengthQ > RVOMath.sqr(radius) && queryVisibilityRecursive(q1, q2, radius, node.left) && queryVisibilityRecursive(q1, q2, radius, node.right));
                }
            }
        }
    }
}