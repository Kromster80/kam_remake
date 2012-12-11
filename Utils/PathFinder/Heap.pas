// From https://github.com/qiao/heap.js
unit Heap;
interface


type
  TComparator = function(A, B: Pointer) : Boolean of object;

  THeap = class
  private
    Count: Longint;
    List: array [0..10000] of Pointer;
    procedure _siftdown(startpos, pos: SmallInt);
    procedure _siftup(pos: SmallInt);
  public
    Cmp: TComparator;
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(x: Pointer);
    procedure UpdateItem(x: Pointer);
  end;


implementation


{
Push item onto heap, maintaining the heap invariant.
}
procedure THeap.Push(x: Pointer);
begin
  //array.push(item);
  Move(List[0], List[1], (Count) * SizeOf(List[0]));
  List[0] := x;
  Inc(Count);

  //return _siftdown(array, 0, array.length - 1, cmp);
  _siftdown(0, Count - 1);
end;


{
Pop the smallest item off the heap, maintaining the heap invariant.
}
function THeap.Pop: Pointer;
var
  lastelt, returnitem: Pointer;
begin
  lastelt := List[0];
  Move(List[1], List[0], (Count-1) * SizeOf(List[0]));
  Dec(Count);

  if (Count > 0) then
  begin
    returnitem := List[0];
    List[0] := lastelt;
    _siftup(0);
  end
  else
  begin
    returnitem := lastelt;
  end;

  Result := returnitem;
end;


{
Update the position of the given item in the heap.
This function should be called every time the item is being modified.
}
procedure THeap.UpdateItem(x: Pointer);
var
  I: ShortInt;
begin
  for I := 0 to Count - 1 do
  if List[I] = x then
    Break;

  _siftdown(0, I);
  _siftup(I);
end;


procedure THeap._siftdown(startpos, pos: SmallInt);
var newitem, parent: Pointer;
  parentpos: SmallInt;
begin
    newitem := List[pos];
    while (pos > startpos) do
    begin
      parentpos := (pos - 1) * 2;
      parent := List[parentpos];
      if not Cmp(newitem, parent) then
      begin
        List[pos] := parent;
        pos := parentpos;
        Continue;
      end;
      break;
    end;
end;


procedure THeap._siftup(pos: SmallInt);
var childpos, endpos, rightpos, startpos: SmallInt;
  newitem: Pointer;
begin
    endpos := Count;
    startpos := pos;
    newitem := List[pos];
    childpos := 2 * pos + 1;
    while (childpos < endpos) do
    begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and (Cmp(List[childpos], List[rightpos])) then
      begin
        childpos := rightpos;
      end;
      List[pos] := List[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
    end;
    List[pos] := newitem;
    _siftdown(startpos, pos);
end;


procedure THeap.Clear;
begin
  Count := 0;
end;


function THeap.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;


end.
