unit Unit_Heap;
interface


type
  TComparator = function(A, B: Pointer) : Boolean of object;

  THeap = class
  Private
    Count: Longint;
    List: array [0..1000]of Pointer;
  Public
    Cmp: TComparator;
    procedure Clear;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(x: Pointer);
    procedure UpdateItem(aPoint: Pointer);
  End;


implementation


procedure THeap.Clear;
begin
  Count := 0;
end;


function THeap.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;


// Push element to heap
Procedure THeap.Push(x: Pointer);
Var
  i, pred: Longint;
  tmp: Pointer;
Begin
  // push element to the end of the heap
  Inc(Count);
  List[Count] := x;
  pred := Count;
  i := Count shr 1;
  // up element to the right place
  While not Cmp(List[i], List[pred]) and (i > 0) Do
  Begin
    tmp := List[i];
    List[i] := List[pred];
    List[pred] := tmp;
    pred := i;
    i := i shr 1;
  End;
End;


// Pop element from the heap
function THeap.Pop: Pointer;
Var
  i, pred: Longint;
  tmp: Pointer;
Begin
  Result := List[1];

  // Replace last element to the top
  List[1] := List[Count];
  pred := 1;
  Dec(Count);
  i := 1 shl 1;
  // down element to the right place
  While not Cmp(List[i], List[pred]) or not Cmp(List[i + 1], List[pred]) and (i <= Count) Do
  Begin
    If not Cmp(List[i], List[pred]) Then
    Begin
      tmp := List[i];
      List[i] := List[pred];
      List[pred] := tmp;
    End
    Else
    Begin
      tmp := List[i + 1];
      List[i + 1] := List[pred];
      List[pred] := tmp;
    End;
    i := i shl 1;
  End;
End;


procedure THeap.UpdateItem(aPoint: Pointer);
begin
  //
end;


end.
