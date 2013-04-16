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
using System.Threading;
using System;

namespace RVO
{
    public class Simulator
    {
        private static Simulator instance_ = new Simulator();

        private Agent defaultAgent_;
        private float time_;
        internal IList<Agent> agents_;
        internal IList<Obstacle> obstacles_;
        internal KdTree kdTree_;
        internal float timeStep_;

        private int _numWorkers;
        private Worker[] _workers;
        private ManualResetEvent[] _doneEvents;

        public static Simulator Instance { get { return instance_; } }
        private Simulator() { Clear(); }

        public void Clear()
        {
            agents_ = new List<Agent>();
            obstacles_ = new List<Obstacle>();
            time_ = 0;
            defaultAgent_ = null;
            kdTree_ = new KdTree();
            timeStep_ = .1f;

            SetNumWorkers(0);
        }

        public int GetNumWorkers()
        {
            return _numWorkers;
        }

        public void SetNumWorkers(int numWorkers)
        {
            _numWorkers = numWorkers;
            if (_numWorkers <= 0)
            {
                int completionPorts;
                ThreadPool.GetMinThreads(out _numWorkers, out completionPorts);
            }
            _workers = null;
        }

        public float getGlobalTime() { return time_; }
        public int getNumAgents() { return agents_.Count; }
        public float getTimeStep() { return timeStep_; }

        public void setAgentPrefVelocity(int i, Vector2 velocity)
        {
            agents_[i].prefVelocity_ = velocity;
        }
        public void setTimeStep(float timeStep)
        {
            timeStep_ = timeStep;
        }
        public Vector2 getAgentPosition(int i)
        {
            return agents_[i].position_;
        }
        public Vector2 getAgentPrefVelocity(int i)
        {
            return agents_[i].prefVelocity_;
        }
        public Vector2 getAgentVelocity(int i)
        {
            return agents_[i].velocity_;
        }
        public float getAgentRadius(int i)
        {
            return agents_[i].radius_;
        }
        public IList<Line> getAgentOrcaLines(int i)
        {
            return agents_[i].orcaLines_;
        }

        public int addAgent(Vector2 position)
        {
            if (defaultAgent_ == null)
            {
                return -1;
            }

            Agent agent = new Agent();

            agent.position_ = position;
            agent.maxNeighbors_ = defaultAgent_.maxNeighbors_;
            agent.maxSpeed_ = defaultAgent_.maxSpeed_;
            agent.neighborDist_ = defaultAgent_.neighborDist_;
            agent.radius_ = defaultAgent_.radius_;
            agent.timeHorizon_ = defaultAgent_.timeHorizon_;
            agent.timeHorizonObst_ = defaultAgent_.timeHorizonObst_;
            agent.velocity_ = defaultAgent_.velocity_;

            agent.id_ = agents_.Count;

            agents_.Add(agent);

            return agents_.Count - 1;

        }

        public void setAgentDefaults(float neighborDist, int maxNeighbors, float timeHorizon, float timeHorizonObst, float radius, float maxSpeed, Vector2 velocity)
        {
            if (defaultAgent_ == null)
            {
                defaultAgent_ = new Agent();
            }

            defaultAgent_.maxNeighbors_ = maxNeighbors;
            defaultAgent_.maxSpeed_ = maxSpeed;
            defaultAgent_.neighborDist_ = neighborDist;
            defaultAgent_.radius_ = radius;
            defaultAgent_.timeHorizon_ = timeHorizon;
            defaultAgent_.timeHorizonObst_ = timeHorizonObst;
            defaultAgent_.velocity_ = velocity;
        }

        private class Worker
        {
            int _start;
            int _end;
            ManualResetEvent _doneEvent;

            internal Worker(int start, int end, ManualResetEvent doneEvent)
            {
                _start = start;
                _end = end;
                _doneEvent = doneEvent;
            }
            internal void step(object o)
            {
                for (int i = _start; i < _end; ++i)
                {
                    Simulator.Instance.agents_[i].computeNeighbors();
                    Simulator.Instance.agents_[i].computeNewVelocity();
                }
                _doneEvent.Set();
            }
            internal void update(object o)
            {
                for (int i = _start; i < _end; ++i)
                {
                    Simulator.Instance.agents_[i].update();
                }
                _doneEvent.Set();
            }
        }

        public float doStep()
        {
            if(_workers == null)
            {
                _workers = new Worker[_numWorkers];
                _doneEvents = new ManualResetEvent[_workers.Length];
                for (int block = 0; block < _workers.Length; ++block)
                {
                    _doneEvents[block] = new ManualResetEvent(false);
                    _workers[block] = new Worker(block * getNumAgents() / _workers.Length, (block + 1) * getNumAgents() / _workers.Length, _doneEvents[block]);
                }
            }

            kdTree_.buildAgentTree();

            for (int block = 0; block < _workers.Length; ++block)
            {
                _doneEvents[block].Reset();
                ThreadPool.QueueUserWorkItem(_workers[block].step);
            }
            WaitHandle.WaitAll(_doneEvents);

            for (int block = 0; block < _workers.Length; ++block)
            {
                _doneEvents[block].Reset();
                ThreadPool.QueueUserWorkItem(_workers[block].update);
            }
            WaitHandle.WaitAll(_doneEvents);

            time_ += timeStep_;
            return time_;
        }

        public int addObstacle(IList<Vector2> vertices)
        {
            if (vertices.Count < 2)
            {
                return -1;
            }

            int obstacleNo = obstacles_.Count;

            for (int i = 0; i < vertices.Count; ++i)
            {
                Obstacle obstacle = new Obstacle();
                obstacle.point_ = vertices[i];
                if (i != 0)
                {
                    obstacle.prevObstacle = obstacles_[obstacles_.Count - 1];
                    obstacle.prevObstacle.nextObstacle = obstacle;
                }
                if (i == vertices.Count - 1)
                {
                    obstacle.nextObstacle = obstacles_[obstacleNo];
                    obstacle.nextObstacle.prevObstacle = obstacle;
                }
                obstacle.unitDir_ = RVOMath.normalize(vertices[(i == vertices.Count - 1 ? 0 : i + 1)] - vertices[i]);

                if (vertices.Count == 2)
                {
                    obstacle.isConvex_ = true;
                }
                else
                {
                    obstacle.isConvex_ = (RVOMath.leftOf(vertices[(i == 0 ? vertices.Count - 1 : i - 1)], vertices[i], vertices[(i == vertices.Count - 1 ? 0 : i + 1)]) >= 0);
                }

                obstacle.id_ = obstacles_.Count;

                obstacles_.Add(obstacle);
            }

            return obstacleNo;
        }

        public void processObstacles()
        {
            kdTree_.buildObstacleTree();
        }

        public bool queryVisibility(Vector2 point1, Vector2 point2, float radius)
        {
            return kdTree_.queryVisibility(point1, point2, radius);
        }

        public int getNumObstacleVertices()
        {
            return obstacles_.Count;
        }

        public Vector2 getObstacleVertex(int vertexNo)
        {
            return obstacles_[vertexNo].point_;
        }

        public int getNextObstacleVertexNo(int vertexNo)
        {
            return obstacles_[vertexNo].nextObstacle.id_;
        }

        public int getPrevObstacleVertexNo(int vertexNo)
        {
            return obstacles_[vertexNo].prevObstacle.id_;
        }
    }
}
