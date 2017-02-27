// From https://github.com/qiao/heap.js
//Ported by Krom for project Castlesand
unit BinaryHeapGen;
interface
uses Generics.Collections;


type
  TComparator<T: class> = function(A, B: T) : Boolean of object;

  TBinaryHeap<T: class> = class
  private
    fCount: Cardinal;
    fItems: array of T;
    procedure _siftdown(startpos, pos: SmallInt);
    procedure _siftup(pos: SmallInt);
  public
    Cmp: TComparator<T>;
    constructor Create(aSize: Cardinal);
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: T;
    procedure Push(x: T);
    procedure UpdateItem(x: T);
  end;


implementation


{ TBinaryHeap }
constructor TBinaryHeap<T>.Create(aSize: Cardinal);
begin
  inherited Create;

  SetLength(fItems, aSize);
end;


procedure TBinaryHeap<T>.Clear;
begin
  fCount := 0;
end;


function TBinaryHeap<T>.IsEmpty: Boolean;
begin
  Result := (fCount = 0);
end;


//Push item onto heap, maintaining the heap invariant.
procedure TBinaryHeap<T>.Push(x: T);
begin
  fItems[fCount] := x;
  Inc(fCount);

  _siftdown(0, fCount - 1);
end;


//Pop the smallest item off the heap, maintaining the heap invariant.
function TBinaryHeap<T>.Pop: T;
var
  lastelt, returnitem: T;
begin
  lastelt := fItems[fCount - 1];
  Dec(fCount);

  if (fCount <> 0) then
  begin
    returnitem := fItems[0];
    fItems[0] := lastelt;
    _siftup(0);
  end
  else
  begin
    returnitem := lastelt;
  end;

  Result := returnitem;
end;


//Update the position of the given item in the heap.
//This function should be called every time the item is being modified.
procedure TBinaryHeap<T>.UpdateItem(x: T);
var
  I: ShortInt;
begin
  for I := 0 to fCount - 1 do
  if fItems[I] = x then
    Break;

  _siftdown(0, I);
  _siftup(I);
end;


procedure TBinaryHeap<T>._siftdown(startpos, pos: SmallInt);
var newitem, parent: T;
  parentpos: SmallInt;
begin
    newitem := fItems[pos];
    while (pos > startpos) do
    begin
      parentpos := (pos - 1) shr 1;
      parent := fItems[parentpos];
      if Cmp(newitem, parent) then
      begin
        fItems[pos] := parent;
        pos := parentpos;
        Continue;
      end;
      Break;
    end;
  fItems[pos] := newitem;
end;


procedure TBinaryHeap<T>._siftup(pos: SmallInt);
var childpos, endpos, rightpos, startpos: SmallInt;
  newitem: T;
begin
    endpos := fCount;
    startpos := pos;
    newitem := fItems[pos];
    childpos := 2 * pos + 1;
    while (childpos < endpos) do
    begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and (not Cmp(fItems[childpos], fItems[rightpos])) then
      begin
        childpos := rightpos;
      end;
      fItems[pos] := fItems[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
    end;
    fItems[pos] := newitem;
    _siftdown(startpos, pos);
end;


end.
