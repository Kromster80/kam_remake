unit KM_Hungarian;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_CommonClasses, KM_CommonTypes, KM_Points;


type
  THungarianOptimisation = (
    hu_Overall, //Minimize total cost (opposite of hu_Individual)
    hu_Individual); //20 agents taking 1 cost beats 1 agent taking 10 cost

  TLocation = record
    Row, Col: SmallInt;
  end;

  TAssignmentSolver = class
  private
    fWidth, fHeight: Integer;
  public
    Costs: array of array of Cardinal; //Input: Costs[X,Y] is the cost of agent X taking task Y
    Solution: array of Cardinal;       //Output: Solution[X] is the task agent X should do
    procedure Solve; virtual; abstract;
  end;

  //Lewin's faster but less optimal algorithm used for large groups
  TSimpleSolver = class(TAssignmentSolver)
  private
    TaskClaimedBy: array of Integer;
    procedure DoSwaps;
  public
    procedure Solve; override; //Do calculation once Costs has been set up
  end;

  //Hungarian solver algorithm based on: http://noldorin.com/programming/HungarianAlgorithm.cs
  //Copyright (c) 2010 Alex Regueiro
  //Licensed under MIT license, available at <http://www.opensource.org/licenses/mit-license.php>.
  //A description of the algorithm and steps is here: http://csclab.murraystate.edu/bob.pilgrim/445/munkres.html
  THungarianMask = (hmNone, hmStar, hmPrime);

  THungarianSolver = class(TAssignmentSolver)
  private
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
    procedure Solve; override; //Do calculation once Costs has been set up
  end;

  function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TKMCardinalArray;


implementation
const
  HUNGARIAN_MAX = 25; //Maximum number of agents hungarian can handle without causing performance issues


{ TSimpleSolver }
//"Lewin's Algorithm": Faster but less optimal algorithm used for large groups
procedure TSimpleSolver.Solve;
var
  I, J: Integer;
  Distance, Best, CostCurrent, CostSwap: Integer;
begin
  fHeight := Length(Costs);
  fWidth := Length(Costs[0]);
  Assert(fWidth >= fHeight, 'Hungarian only works when cols >= rows. Please rotate input matrix');

  SetLength(Solution, fHeight);
  SetLength(TaskClaimedBy, fWidth);
  for J := 0 to fWidth - 1 do
    TaskClaimedBy[J] := -1;

  //STEP 1: Let each agent claim his prefered task
  for I := fHeight - 1 downto 0 do
  begin
    Best := MaxInt;
    //Find the best task for this agent
    for J := fWidth - 1 downto 0 do
    begin
      Distance := Costs[I, J];
      if (TaskClaimedBy[J] = -1) and (Distance < Best) then
      begin
        Solution[I] := J;
        Best := Distance;
      end;
    end;
    TaskClaimedBy[Solution[I]] := I; //Task is claimed, don't let another agent take it
  end;

  //STEP 2: Swap tasks between agents when it's more efficient to do so
  DoSwaps;
  DoSwaps; //The more times we run it, the better the result. Twice gives good results without costing too much performance

  //Now see if we found a better solution than a basic 1 to 1 assignment between tasks and agents
  CostCurrent := 0;
  CostSwap := 0;
  for I := fHeight - 1 downto 0 do
  begin
    CostCurrent := CostCurrent + Costs[I, Solution[I]];
    CostSwap := CostSwap + Costs[I, I];
  end;
  //If the simple 1 to 1 assignment is as good or better, then use it because that makes
  //group rearrangements look much neater in cases where the entire group moves 1 tile
  if CostSwap <= CostCurrent then
    for I := fHeight - 1 downto 0 do
      Solution[I] := I;
end;


//Swap tasks between agents when it is more efficient to do so
procedure TSimpleSolver.DoSwaps;
var I, J, TaskToSwap, Best, CostCurrent, CostSwap: Integer;
begin
  for I := fHeight - 1 downto 0 do
  begin
    Best := MaxInt;
    TaskToSwap := -1;
    //Find the best task for this agent
    for J := fWidth - 1 downto 0 do
      if TaskClaimedBy[J] <> I then //This is not currently our task
      begin
        //Calculate the cost of swapping with this agent
        CostSwap := Costs[I, J] + Costs[TaskClaimedBy[J], Solution[I]];
        if CostSwap > Best then
          Continue;
        //Calculate the cost of not swapping
        CostCurrent := Costs[I, Solution[I]] + Costs[TaskClaimedBy[J], J];
        if CostSwap < CostCurrent then
        begin
          TaskToSwap := J;
          Best := CostSwap;
        end;
      end;
    if TaskToSwap <> -1 then
    begin
      Solution[TaskClaimedBy[TaskToSwap]] := Solution[I]; //They get our task
      TaskClaimedBy[Solution[I]] := TaskClaimedBy[TaskToSwap]; //They now own our task
      Solution[I] := TaskToSwap; //We get their task
      TaskClaimedBy[TaskToSwap] := I; //We now own their task
    end;
  end;
end;


{ THungarianSolver }
procedure THungarianSolver.Solve;
var I, J, MinValue, step: Integer;
begin
  fHeight := Length(Costs);
  fWidth := Length(Costs[0]);
  Assert(fWidth >= fHeight, 'Hungarian only works when cols >= rows. Please rotate input matrix');

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
  fPathStart.Row := 0;
  fPathStart.Col := 0;
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
  for I := fHeight-1 downto 0 do
    for J := fWidth-1 downto 0 do
      if fMasks[I, J] = hmStar then
        fColsCovered[J] := True;

  colsCoveredCount := 0;
  for J := fWidth-1 downto 0 do
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
    fPath[pathIndex].Row := row;
    fPath[pathIndex].Col := fPath[pathIndex - 1].Col;
    col := FindPrimeInRow(fPath[pathIndex].Row);
    inc(pathIndex);
    fPath[pathIndex].Row := fPath[pathIndex - 1].Row;
    fPath[pathIndex].Col := col;
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
  for I := fHeight-1 downto 0 do
    for J := fWidth-1 downto 0 do
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
  for I := fHeight-1 downto 0 do
    for J := fWidth-1 downto 0 do
      if (not fRowsCovered[I]) and (not fColsCovered[J]) then
        if Costs[I, J] < Result then
          Result := Costs[I, J];
end;


procedure THungarianSolver.ConvertPath(pathLength: Integer);
var I: Integer;
begin
  for I := pathLength-1 downto 0 do
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
  Result.Row := -1;
  Result.Col := -1;
  for I := fHeight-1 downto 0 do
    for J := fWidth-1 downto 0 do
      if (Costs[I, J] = 0) and (not fRowsCovered[I]) and (not fColsCovered[J]) then
      begin
        Result.Row := I;
        Result.Col := J;
        Exit;
      end;
end;


function THungarianSolver.FindStarInColumn(aRow: Integer): Integer;
var J:Integer;
begin
  Result := -1;
  for J := fWidth-1 downto 0 do
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
  for J := fWidth-1 downto 0 do
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
  for J := fWidth-1 downto 0 do
    if fMasks[aRow, J] = hmPrime then
    begin
      Result := J;
      Exit;
    end;
end;


procedure THungarianSolver.ClearCovers;
var I:Integer;
begin
  for I := fHeight-1 downto 0 do
    fRowsCovered[I] := False;
  for I := fWidth-1 downto 0 do
    fColsCovered[I] := False;
end;


procedure THungarianSolver.ClearPrimes;
var I,J:Integer;
begin
  for I := fHeight-1 downto 0 do
    for J := fWidth-1 downto 0 do
      if fMasks[I, J] = hmPrime then
        fMasks[I, J] := hmNone;
end;


//Returns the most efficient matching of aAgents to aTasks
//Result contains optimal order
function HungarianMatchPoints(aAgents, aTasks: TKMPointList; aOptimisation: THungarianOptimisation): TKMCardinalArray;
var
  Solver: TAssignmentSolver;
  I, J: Integer;
begin
  //Hungarian algorithm is slow O(n^3) so we should only use it when group sizes are fairly small,
  //otherwise we use a simpler algorithm which produces almost as good results.
  if aAgents.Count <= HUNGARIAN_MAX then
    Solver := THungarianSolver.Create
  else
    Solver := TSimpleSolver.Create;

  //Input
  SetLength(Solver.Costs, aAgents.Count, aTasks.Count);

  if aOptimisation = hu_Individual then
  begin
    for I := aAgents.Count - 1 downto 0 do
      for J := aTasks.Count - 1 downto 0 do
        //Square the entire costs matrix to make high costs very bad compared to low costs
        //@Lewin: For some weird reason in XE2 (didnt tested in L nor 7)
        //Round(Sqr( = sane values 200, 414, etc
        //Sqr(Round( = insane 32243928 kind of numbers
        Solver.Costs[I, J] := Round(Sqr(10 * KMLengthDiag(aAgents[I], aTasks[J])));
  end
  else
    for I := aAgents.Count - 1 downto 0 do
      for J := aTasks.Count - 1 downto 0 do
        Solver.Costs[I, J] := Round(10 * KMLengthDiag(aAgents[I], aTasks[J]));

  Solver.Solve;

  //Output
  SetLength(Result, aAgents.Count);
  Move(Solver.Solution[0], Result[0], aAgents.Count * SizeOf(Cardinal));

  Solver.Free;
end;


end.