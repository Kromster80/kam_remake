unit Unit_HeapStub;
interface
uses Types, Math, SysUtils;


type
  TComparator = function(A, B: Pointer): Boolean of object;

  THeap = class
  private
    Count: SmallInt;
    List: array [0..65535] of Pointer;
  public
    Cmp: TComparator;
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(x: Pointer);
    procedure UpdateItem(x: Pointer);
  end;


implementation


procedure THeap.Clear;
begin
  Count := 0;
end;


function THeap.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;


procedure THeap.Push(x: Pointer);
begin
  List[Count] := x;
  Inc(Count);
end;


function THeap.Pop: Pointer;
var
  I: Integer;
  Best: Integer;
begin
  //Return smallest
  Best := 0;
  for I := 1 to Count - 1 do
  if Cmp(List[I], List[Best]) then
    Best := I;

  Result := List[Best];

  if Best <> Count then
    Move(List[Best + 1], List[Best], (Count - Best) * SizeOf(List[0]));

  Dec(Count);
end;


procedure THeap.UpdateItem(x: Pointer);
begin
  //
end;


end.
