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

using System;

namespace RVO
{
    public struct RVOMath
    {
        public static float absSq(Vector2 v)
        {
            return v * v;
        }
        public static Vector2 normalize(Vector2 v)
        {
            return v / abs(v);
        }

        internal const float RVO_EPSILON = 0.00001f;

        internal static float sqrt(float a)
        {
            return (float)Math.Sqrt(a);
        }
        internal static float fabs(float a)
        {
            return Math.Abs(a);
        }
        internal static float distSqPointLineSegment(Vector2 a, Vector2 b, Vector2 c)
        {
            float r = ((c - a) * (b - a)) / absSq(b - a);

            if (r < 0.0f)
            {
                return absSq(c - a);
            }
            else if (r > 1.0f)
            {
                return absSq(c - b);
            }
            else
            {
                return absSq(c - (a + r * (b - a)));
            }
        }
        internal static float sqr(float p)
        {
            return p * p;
        }
        internal static float det(Vector2 v1, Vector2 v2)
        {
            return v1.x_ * v2.y_ - v1.y_ * v2.x_;
        }
        internal static float abs(Vector2 v)
        {
            return (float)Math.Sqrt(absSq(v));
        }
        internal static float leftOf(Vector2 a, Vector2 b, Vector2 c)
        {
            return det(a - c, b - a);
        }
    }
}