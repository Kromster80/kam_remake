// From https://github.com/qiao/heap.js
//Ported by Krom for project Castlesand
unit BinaryHeap;
interface


type
  TComparator = function(A, B: Pointer): Boolean of object;

  TBinaryHeap = class
  private
    fCount: Word;
    fItems: array of Pointer;
    procedure _siftdown(startpos, pos: Word);
    procedure _siftup(pos: Word);
  public
    Cmp: TComparator;
    constructor Create(aSize: Word);
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(x: Pointer);
    procedure UpdateItem(x: Pointer);
  end;


implementation


{ TBinaryHeap }
constructor TBinaryHeap.Create(aSize: Word);
begin
  inherited Create;

  SetLength(fItems, aSize);
end;


procedure TBinaryHeap.Clear;
begin
  fCount := 0;
end;


function TBinaryHeap.IsEmpty: Boolean;
begin
  Result := (fCount = 0);
end;


//Push item onto heap, maintaining the heap invariant.
procedure TBinaryHeap.Push(x: Pointer);
begin
  fItems[fCount] := x;
  Inc(fCount);

  _siftdown(0, fCount - 1);
end;


//Pop the smallest item off the heap, maintaining the heap invariant.
function TBinaryHeap.Pop: Pointer;
var
  lastelt, returnitem: Pointer;
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
procedure TBinaryHeap.UpdateItem(x: Pointer);
var
  I: Word;
begin
  for I := 0 to fCount - 1 do
  if fItems[I] = x then
    Break;

  _siftdown(0, I);
  _siftup(I);
end;


procedure TBinaryHeap._siftdown(startpos, pos: Word);
var newitem, parent: Pointer;
  parentpos: Word;
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


procedure TBinaryHeap._siftup(pos: Word);
var childpos, endpos, rightpos, startpos: Word;
  newitem: Pointer;
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
