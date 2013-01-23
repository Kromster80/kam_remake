unit KM_Hungarian;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KM_CommonClasses, KM_Points;


type
  THungarianOptimisation = (hu_Overall, //Minimize total cost (oposite of hu_IndividualWork)
                            hu_Individual); //20 agents taking 1 cost beats 1 agent taking 10 cost

  TLocation = record Row, Col: SmallInt; end;
  function Location(aRow, aCol: SmallInt): TLocation;


type
  //Hungarian solver algorithm based on: http://noldorin.com/programming/HungarianAlgorithm.cs
  //Copyright (c) 2010 Alex Regueiro
  //Licensed under MIT license, available at <http://www.opensource.org/licenses/mit-license.php>.
  //A description of the algorithm and steps is here: http://csclab.murraystate.edu/bob.pilgrim/445/munkres.html
  THungarianMask = (hmNone, hmStar, hmPrime);
  THungarianSolver = class
  private
    w, h: Integer;
    masks: array of array of THungarianMask;
    rowsCovered: array of Boolean;
    colsCovered: array of Boolean;
    PathStart: TLocation;
    Path: array of TLocation;
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
    Costs: array of array of Integer; //Input: Costs[X,Y] is the cost of agent X taking task Y
    Solution: array of Integer;       //Output: Solution[X] is the task agent X should do
    procedure Solve(aOptimisation: THungarianOptimisation); //Do calculation once Costs has been set up
  end;

  TIntegerArray = array of Integer;

  function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TIntegerArray;


implementation


function Location(aRow, aCol: SmallInt): TLocation;
begin
  Result.Row := aRow;
  Result.Col := aCol;
end;


procedure THungarianSolver.Solve(aOptimisation: THungarianOptimisation);
var
  i, j, MinValue, step: Integer;
begin
  h := Length(costs);
  w := Length(costs[0]);
  Assert(w >= h, 'Hungarian only works when cols >= rows. Please rotate input matrix');

  if aOptimisation = hu_Individual then
    //Lewin's hack: Square the entire costs matrix to make high costs very bad compared to low costs
    for i := 0 to h-1 do
      for j := 0 to w-1 do
        costs[i,j] := Sqr(costs[i,j]);

  //For each row of the matrix, find the smallest element and subtract it from every element in its row
  for i := 0 to h-1 do
  begin
    MinValue := High(Integer);
    for j := 0 to w-1 do
      MinValue := Math.Min(MinValue, costs[i, j]);
    for j := 0 to w-1 do
      dec(costs[i, j], MinValue);
  end;

  SetLength(masks, h, w);
  SetLength(rowsCovered, h);
  SetLength(colsCovered, w);
  ClearCovers;

  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if (costs[i, j] = 0) and (not rowsCovered[i]) and (not colsCovered[j]) then
      begin
        masks[i, j] := hmStar;
        rowsCovered[i] := true;
        colsCovered[j] := true;
      end;
  ClearCovers;

  SetLength(Path, w*h);
  PathStart := Location(0,0);
  step := 1;
  while step <> -1 do
    case step of
      1: step := RunStep1;
      2: step := RunStep2;
      3: step := RunStep3;
      4: step := RunStep4;
    end;

  //Fill in the solution
  SetLength(Solution, h);
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if masks[i, j] = hmStar then
      begin
        Solution[i] := j;
        Break;
      end;
end;


function THungarianSolver.RunStep1: Integer;
var i,j,colsCoveredCount: Integer;
begin
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if masks[i, j] = hmStar then
        colsCovered[j] := true;

  colsCoveredCount := 0;
  for j := 0 to w-1 do
    if colsCovered[j] then
      inc(colsCoveredCount);

  if colsCoveredCount = h then
    Result := -1 //Problem solved already
  else
    Result := 2; //Go to step 2
end;


function THungarianSolver.RunStep2: Integer;
var starCol: Integer; loc: TLocation;
begin
  while true do
  begin
    loc := FindZero;
    if loc.Row = -1 then
    begin
      Result := 4;
      Exit;
    end
    else
    begin
      masks[loc.Row, loc.Col] := hmPrime;
      starCol := FindStarInRow(loc.Row);
      if starCol <> -1 then
      begin
        rowsCovered[loc.Row] := true;
        colsCovered[starCol] := false
      end
      else
      begin
        pathStart := loc;
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
  path[0] := pathStart;
  while true do
  begin
    row := FindStarInColumn(path[pathIndex].Col);
    if row = -1 then
      break;
    inc(pathIndex);
    path[pathIndex] := Location(row, path[pathIndex - 1].Col);
    col := FindPrimeInRow(path[pathIndex].Row);
    inc(pathIndex);
    path[pathIndex] := Location(path[pathIndex - 1].Row, col)
  end;
  ConvertPath(pathIndex + 1);
  ClearCovers;
  ClearPrimes;
  Result := 1;
end;


function THungarianSolver.RunStep4: Integer;
var i, j, minValue: Integer;
begin
  minValue := FindMinimum;
  for i := 0 to h-1 do
    for j := 0 to w-1 do
    begin
      if rowsCovered[i] then
        inc(costs[i, j], minValue);
      if not colsCovered[j] then
        dec(costs[i, j], minValue);
    end;
  Result := 2;
end;


function THungarianSolver.FindMinimum: Integer;
var i, j: integer;
begin
  Result := High(Integer);
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if (not rowsCovered[i]) and (not colsCovered[j]) then
        Result := Math.Min(Result, costs[i, j])
end;


procedure THungarianSolver.ConvertPath(pathLength: Integer);
var i: Integer;
begin
  for i := 0 to pathLength-1 do
  begin
    if masks[path[i].Row, path[i].Col] = hmStar then
      masks[path[i].Row, path[i].Col] := hmNone;
    if masks[path[i].Row, path[i].Col] = hmPrime then
      masks[path[i].Row, path[i].Col] := hmStar;
  end;
end;


function THungarianSolver.FindZero: TLocation;
var i,j:Integer;
begin
  Result := Location(-1, -1);
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if (costs[i, j] = 0) and (not rowsCovered[i]) and (not colsCovered[j]) then
      begin
         Result := Location(i, j);
         Exit;
      end;
end;


function THungarianSolver.FindStarInColumn(aRow: Integer): Integer;
var j:Integer;
begin
  Result := -1;
  for j := 0 to w-1 do
    if masks[j, aRow] = hmStar then
    begin
      Result := j;
      Exit;
    end;
end;


function THungarianSolver.FindStarInRow(aRow: Integer): Integer;
var j:Integer;
begin
  Result := -1;
  for j := 0 to w-1 do
    if masks[aRow, j] = hmStar then
    begin
      Result := j;
      Exit;
    end;
end;


function THungarianSolver.FindPrimeInRow(aRow: Integer): Integer;
var j:Integer;
begin
  Result := -1;
  for j := 0 to w-1 do
    if masks[aRow, j] = hmPrime then
    begin
      Result := j;
      Exit;
    end;
end;


procedure THungarianSolver.ClearCovers;
var i:Integer;
begin
  for i := 0 to h-1 do
    rowsCovered[i] := False;
  for i := 0 to w-1 do
    colsCovered[i] := False;
end;


procedure THungarianSolver.ClearPrimes;
var i,j:Integer;
begin
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if masks[i, j] = hmPrime then
        masks[i, j] := hmNone;
end;


//Returns the most efficient matching of aAgents to aTasks
function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TIntegerArray;
var
  Solver: THungarianSolver;
  I, J: Integer;
begin
  Solver := THungarianSolver.Create;

  //Input
  SetLength(Solver.Costs, aAgents.Count, aTasks.Count);
  for I := 0 to aAgents.Count-1 do
    for J := 0 to aTasks.Count-1 do
      Solver.Costs[I,J] := Round(10*KMLengthDiag(aAgents[I], aTasks[J]));

  Solver.Solve(aOptimisation);

  //Output
  SetLength(Result, aAgents.Count);
  Move(Solver.Solution[0], Result[0], aAgents.Count*SizeOf(Result[0]));

  Solver.Free;
end;

end.