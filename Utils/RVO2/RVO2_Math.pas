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
unit RVO2_Math;
interface
uses RVO2_Vector2;

  function absSq(v: TRVOVector2): Single;
  function normalize(v: TRVOVector2): TRVOVector2;
  function distSqPointLineSegment(a, b, c: TRVOVector2): Single;
  function det(v1, v2: TRVOVector2): Single;
  function abs(v: TRVOVector2): Single;
  function leftOf(a, b, c: TRVOVector2): Single;

  const
    RVO_EPSILON: Single = 0.00001;

implementation


function absSq(v: TRVOVector2): Single;
begin
  Result := Vector2Mul(v, v);
end;

function normalize(v: TRVOVector2): TRVOVector2;
begin
  Result := Vector2Scale(v, 1 / abs(v));
end;

function distSqPointLineSegment(a, b, c: TRVOVector2): Single;
var
  R: Single;
begin
  r := Vector2Mul(Vector2Sub(c, a), Vector2Sub(b, a)) / absSq(Vector2Sub(b, a));

  if (r < 0) then
  begin
    Result := absSq(Vector2Sub(c, a));
  end
  else if (r > 1) then
  begin
    Result := absSq(Vector2Sub(c, b));
  end
  else
  begin
    Result := absSq(Vector2Sub(c, Vector2Add(a, Vector2Scale(Vector2Sub(b, a), r))));
  end;
end;

function det(v1, v2: TRVOVector2): Single;
begin
  Result := v1.x * v2.y - v1.y * v2.x;
end;

function abs(v: TRVOVector2): Single;
begin
  Result := Sqrt(absSq(v));
end;

function leftOf(a, b, c: TRVOVector2): Single;
begin
  Result := det(Vector2Sub(a, c), Vector2Sub(b, a));
end;

end.
