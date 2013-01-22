unit KM_Hungarian;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math;


type
  TLocation = record Row, Col: SmallInt; end;
  function Location(aRow, aCol: SmallInt): TLocation;


type
  //Hungarian solver algorithm based on: http://noldorin.com/programming/HungarianAlgorithm.cs
  //Copyright (c) 2010 Alex Regueiro
  //Licensed under MIT license, available at <http://www.opensource.org/licenses/mit-license.php>.
  THungarianSolver = class
  private
    w, h: Integer;
    masks: array of array of Byte;
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
    Costs: array of array of Integer;
    Solution: array of Integer;
    procedure Solve;
  end;


implementation


function Location(aRow, aCol: SmallInt): TLocation;
begin
  Result.Row := aRow;
  Result.Col := aCol;
end;


procedure THungarianSolver.Solve;
var
  i, j, min, step: Integer;
begin
  h := Length(costs);
  w := Length(costs[0]);

  //For each row of the matrix, find the smallest element and subtract it from every element in its row
  for i := 0 to h-1 do
  begin
    min := High(Integer);
    for j := 0 to w-1 do
      min := Math.Min(min, costs[i, j]);
    for j := 0 to w-1 do
      dec(costs[i, j], min);
  end;

  SetLength(masks, h, w);
  SetLength(rowsCovered, h);
  SetLength(colsCovered, w);
  ClearCovers;

  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if (costs[i, j] = 0) and (not rowsCovered[i]) and (not colsCovered[j]) then
      begin
        masks[i, j] := 1;
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
      if masks[i, j] = 1 then
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
      if masks[i, j] = 1 then
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
      masks[loc.Row, loc.Col] := 2;
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
    if masks[path[i].Row, path[i].Col] = 1 then
      masks[path[i].Row, path[i].Col] := 0;
    if masks[path[i].Row, path[i].Col] = 2 then
      masks[path[i].Row, path[i].Col] := 1;
  end;
end;


function THungarianSolver.FindZero: TLocation;
var i,j:Integer;
begin
  Result := Location(-1, -1);
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      if (costs[i, j] = 0) and (not rowsCovered[i]) and (not colsCovered[j]) then
         Result := Location(i, j);
end;


function THungarianSolver.FindStarInColumn(aRow: Integer): Integer;
var j:Integer;
begin
  Result := -1;
  for j := 0 to w-1 do
    if masks[j, aRow] = 1 then
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
    if masks[aRow, j] = 1 then
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
    if masks[aRow, j] = 2 then
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
      if masks[i, j] = 2 then
        masks[i, j] := 0;
end;

end.