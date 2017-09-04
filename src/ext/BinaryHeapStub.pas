unit BinaryHeapStub;
interface
uses Types, Math, SysUtils;


type
  TComparator = function(A, B: Pointer): Boolean of object;

  TBinaryHeap = class
  private
    fCount: Cardinal;
    fItems: array of Pointer;
  public
    Cmp: TComparator;
    constructor Create(aSize: Cardinal);
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(x: Pointer);
    procedure UpdateItem(x: Pointer);
  end;


implementation


{ TBinaryHeap }
constructor TBinaryHeap.Create(aSize: Cardinal);
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


procedure TBinaryHeap.Push(x: Pointer);
begin
  fItems[fCount] := x;
  Inc(fCount);
end;


function TBinaryHeap.Pop: Pointer;
var
  I: Integer;
  Best: Integer;
begin
  //Return smallest
  Best := 0;
  for I := 1 to fCount - 1 do
  if Cmp(fItems[I], fItems[Best]) then
    Best := I;

  Result := fItems[Best];

  if Best <> fCount then
    Move(fItems[Best + 1], fItems[Best], (fCount - Best) * SizeOf(fItems[0]));

  Dec(fCount);
end;


procedure TBinaryHeap.UpdateItem(x: Pointer);
begin
  //Placeholder for compatibility with true TBinaryHeap
end;


end.
