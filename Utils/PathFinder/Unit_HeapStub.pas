unit Unit_HeapStub;
interface
uses Types, Math, SysUtils, Unit_Heap;


type
  TComparator = function(A,B: Pointer) : Boolean of object;

  THeap = class
  private
    Count: SmallInt;
    List: array [0..1000] of Pointer;
  public
    Cmp: TComparator;
    function IsEmpty: Boolean;
    procedure Push(aPoint: Pointer);
    function Pop: Pointer;
    procedure UpdateItem(aPoint: Pointer);
  end;


implementation


function THeap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;


procedure THeap.Push(aPoint: Pointer);
begin
  List[Count] := aPoint;
  Inc(Count);
end;


function THeap.Pop: Pointer;
var
  I: Integer;
  Best: Integer;
begin
  //Return smallest f
  //(which in JS was the top item, cos all items were sorted on Update)
  Best := 0;
  for I := 1 to Count - 1 do
  if Cmp(List[I], List[Best]) then
    Best := I;

  Result := List[Best];

  if Best <> Count then
    Move(List[Best + 1], List[Best], (Count - Best) * SizeOf(List[0]));

  Dec(Count);
end;


procedure THeap.UpdateItem(aPoint: Pointer);
begin
  //
end;


end.
