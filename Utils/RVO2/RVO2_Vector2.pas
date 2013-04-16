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
unit RVO2_Vector2;
interface
uses SysUtils;


type
  TRVOVector2 = record
    X,Y: Single;
  end;

  function Vector2(X, Y: Single): TRVOVector2;
  function Vector2ToString(A: TRVOVector2): string;
  function Vector2Add(A,B: TRVOVector2): TRVOVector2;
  function Vector2Sub(A,B: TRVOVector2): TRVOVector2;
  function Vector2Mul(A,B: TRVOVector2): Single;
  function Vector2Scale(A: TRVOVector2; B: Single): TRVOVector2; overload;
  function Vector2Scale(A: Single; B: TRVOVector2): TRVOVector2; overload;
  function Vector2Neg(A: TRVOVector2): TRVOVector2;


implementation


function Vector2(X, Y: Single): TRVOVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector2ToString(A: TRVOVector2): string;
begin
  Result := Format('( %.2f, %.2f)', [A.X, A.Y]);
end;

function Vector2Add(A,B: TRVOVector2): TRVOVector2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function Vector2Sub(A,B: TRVOVector2): TRVOVector2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

//public static float operator *(Vector2 lhs, Vector2 rhs)
function Vector2Mul(A,B: TRVOVector2): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

//public static Vector2 operator *(float k, Vector2 u)
//public static Vector2 operator *(Vector2 u, float k)
//public static Vector2 operator /(Vector2 u, float k)
function Vector2Scale(A: TRVOVector2; B: Single): TRVOVector2;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

function Vector2Scale(A: Single; B: TRVOVector2): TRVOVector2;
begin
  Result.X := B.X * A;
  Result.Y := B.Y * A;
end;

//public static Vector2 operator -(Vector2 v)
function Vector2Neg(A: TRVOVector2): TRVOVector2;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

end.

