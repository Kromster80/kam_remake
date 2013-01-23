unit KM_Hungarian;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_CommonClasses, KM_CommonTypes, KM_Points;


type
  THungarianOptimisation = (
    hu_Overall, //Minimize total cost (opposite of hu_Individual)
    hu_Individual); //20 agents taking 1 cost beats 1 agent taking 10 cost

  TLocation = record
    Row, Col: SmallInt;
  end;

  //Hungarian solver algorithm based on: http://noldorin.com/programming/HungarianAlgorithm.cs
  //Copyright (c) 2010 Alex Regueiro
  //Licensed under MIT license, available at <http://www.opensource.org/licenses/mit-license.php>.
  //A description of the algorithm and steps is here: http://csclab.murraystate.edu/bob.pilgrim/445/munkres.html
  THungarianMask = (hmNone, hmStar, hmPrime);
  THungarianSolver = class
  private
    fWidth, fHeight: Integer;
    fMasks: array of array of THungarianMask;
    fRowsCovered: array of Boolean;
    fColsCovered: array of Boolean;
    fPathStart: TLocation;
    fPath: array of TLocation;
    function FindZero: TLocation;
    function FindStarInColumn(aRow: Integer): Integer;
    function FindStarInRow(aRow: Integer): Integer;
    function FindPrimeInRow(aRow: Integer): Integer;
    procedure ConvertPath(pathLength: Integer);
    procedure ClearPrimes;
    procedure ClearCovers;
    function FindMinimum: Integer;
    function RunStep1: Integer;
    function RunStep2: Integer;
    function RunStep3: Integer;
    function RunStep4: Integer;
  public
    Costs: array of array of Cardinal; //Input: Costs[X,Y] is the cost of agent X taking task Y
    Solution: array of Cardinal;       //Output: Solution[X] is the task agent X should do
    procedure Solve(aOptimisation: THungarianOptimisation); //Do calculation once Costs has been set up
  end;

  function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TKMCardinalArray;
  function MakeLocation(aRow, aCol: SmallInt): TLocation;


implementation


function MakeLocation(aRow, aCol: SmallInt): TLocation;
begin
  Result.Row := aRow;
  Result.Col := aCol;
end;


{ THungarianSolver }
procedure THungarianSolver.Solve(aOptimisation: THungarianOptimisation);
var
  I, J, MinValue, step: Integer;
begin
  fHeight := Length(Costs);
  fWidth := Length(Costs[0]);
  Assert(fWidth >= fHeight, 'Hungarian only works when cols >= rows. Please rotate input matrix');

  if aOptimisation = hu_Individual then
    //Lewin's hack: Square the entire costs matrix to make high costs very bad compared to low costs
    for I := 0 to fHeight-1 do
      for J := 0 to fWidth-1 do
        Costs[I,J] := Sqr(Costs[I,J]);

  //For each row of the matrix, find the smallest element and subtract it from every element in its row
  for I := 0 to fHeight-1 do
  begin
    MinValue := High(Integer);
    for J := 0 to fWidth-1 do
      MinValue := Min(MinValue, Costs[I, J]);
    for J := 0 to fWidth-1 do
      dec(Costs[I, J], MinValue);
  end;

  SetLength(fMasks, fHeight, fWidth);
  SetLength(fRowsCovered, fHeight);
  SetLength(fColsCovered, fWidth);
  ClearCovers;

  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if (Costs[I, J] = 0) and (not fRowsCovered[I]) and (not fColsCovered[J]) then
      begin
        fMasks[I, J] := hmStar;
        fRowsCovered[I] := True;
        fColsCovered[J] := True;
      end;
  ClearCovers;

  SetLength(fPath, fWidth*fHeight);
  fPathStart := MakeLocation(0,0);
  step := 1;
  while step <> -1 do
    case step of
      1: step := RunStep1;
      2: step := RunStep2;
      3: step := RunStep3;
      4: step := RunStep4;
    end;

  //Fill in the Solution
  SetLength(Solution, fHeight);
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if fMasks[I, J] = hmStar then
      begin
        Solution[I] := J;
        Break;
      end;
end;


function THungarianSolver.RunStep1: Integer;
var I,J,colsCoveredCount: Integer;
begin
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if fMasks[I, J] = hmStar then
        fColsCovered[J] := True;

  colsCoveredCount := 0;
  for J := 0 to fWidth-1 do
    if fColsCovered[J] then
      inc(colsCoveredCount);

  if colsCoveredCount = fHeight then
    Result := -1 //Problem solved already
  else
    Result := 2; //Go to step 2
end;


function THungarianSolver.RunStep2: Integer;
var starCol: Integer; loc: TLocation;
begin
  while True do
  begin
    loc := FindZero;
    if loc.Row = -1 then
    begin
      Result := 4;
      Exit;
    end
    else
    begin
      fMasks[loc.Row, loc.Col] := hmPrime;
      starCol := FindStarInRow(loc.Row);
      if starCol <> -1 then
      begin
        fRowsCovered[loc.Row] := True;
        fColsCovered[starCol] := false
      end
      else
      begin
        fPathStart := loc;
        Result := 3;
        Exit;
      end
    end
  end
end;


function THungarianSolver.RunStep3: Integer;
var pathIndex, row, col: Integer;
begin
  pathIndex := 0;
  fPath[0] := fPathStart;
  while True do
  begin
    row := FindStarInColumn(fPath[pathIndex].Col);
    if row = -1 then
      break;
    inc(pathIndex);
    fPath[pathIndex] := MakeLocation(row, fPath[pathIndex - 1].Col);
    col := FindPrimeInRow(fPath[pathIndex].Row);
    inc(pathIndex);
    fPath[pathIndex] := MakeLocation(fPath[pathIndex - 1].Row, col)
  end;
  ConvertPath(pathIndex + 1);
  ClearCovers;
  ClearPrimes;
  Result := 1;
end;


function THungarianSolver.RunStep4: Integer;
var I, J, minValue: Integer;
begin
  minValue := FindMinimum;
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
    begin
      if fRowsCovered[I] then
        inc(Costs[I, J], minValue);
      if not fColsCovered[J] then
        dec(Costs[I, J], minValue);
    end;
  Result := 2;
end;


function THungarianSolver.FindMinimum: Integer;
var I, J: integer;
begin
  Result := High(Integer);
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if (not fRowsCovered[I]) and (not fColsCovered[J]) then
        Result := Math.Min(Result, Costs[I, J])
end;


procedure THungarianSolver.ConvertPath(pathLength: Integer);
var I: Integer;
begin
  for I := 0 to pathLength-1 do
  begin
    if fMasks[fPath[I].Row, fPath[I].Col] = hmStar then
      fMasks[fPath[I].Row, fPath[I].Col] := hmNone;
    if fMasks[fPath[I].Row, fPath[I].Col] = hmPrime then
      fMasks[fPath[I].Row, fPath[I].Col] := hmStar;
  end;
end;


function THungarianSolver.FindZero: TLocation;
var I,J:Integer;
begin
  Result := MakeLocation(-1, -1);
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if (Costs[I, J] = 0) and (not fRowsCovered[I]) and (not fColsCovered[J]) then
      begin
         Result := MakeLocation(I, J);
         Exit;
      end;
end;


function THungarianSolver.FindStarInColumn(aRow: Integer): Integer;
var J:Integer;
begin
  Result := -1;
  for J := 0 to fWidth-1 do
    if fMasks[J, aRow] = hmStar then
    begin
      Result := J;
      Exit;
    end;
end;


function THungarianSolver.FindStarInRow(aRow: Integer): Integer;
var J:Integer;
begin
  Result := -1;
  for J := 0 to fWidth-1 do
    if fMasks[aRow, J] = hmStar then
    begin
      Result := J;
      Exit;
    end;
end;


function THungarianSolver.FindPrimeInRow(aRow: Integer): Integer;
var J:Integer;
begin
  Result := -1;
  for J := 0 to fWidth-1 do
    if fMasks[aRow, J] = hmPrime then
    begin
      Result := J;
      Exit;
    end;
end;


procedure THungarianSolver.ClearCovers;
var I:Integer;
begin
  for I := 0 to fHeight-1 do
    fRowsCovered[I] := False;
  for I := 0 to fWidth-1 do
    fColsCovered[I] := False;
end;


procedure THungarianSolver.ClearPrimes;
var I,J:Integer;
begin
  for I := 0 to fHeight-1 do
    for J := 0 to fWidth-1 do
      if fMasks[I, J] = hmPrime then
        fMasks[I, J] := hmNone;
end;


//Returns the most efficient matching of aAgents to aTasks
//Result contains optimal order
function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TKMCardinalArray;
var
  Solver: THungarianSolver;
  I, J: Integer;
begin
  Solver := THungarianSolver.Create;

  //Input
  SetLength(Solver.Costs, aAgents.Count, aTasks.Count);
  for I := 0 to aAgents.Count - 1 do
    for J := 0 to aTasks.Count - 1 do
      Solver.Costs[I, J] := Round(10 * KMLengthDiag(aAgents[I], aTasks[J]));

  Solver.Solve(aOptimisation);

  //Output
  Assert(SizeOf(Solver.Solution[0]) = SizeOf(Result[0]), 'Hungarian elements mismatch output size');
  SetLength(Result, aAgents.Count);
  Move(Solver.Solution[0], Result[0], aAgents.Count * SizeOf(Result[0]));

  Solver.Free;
end;


end.