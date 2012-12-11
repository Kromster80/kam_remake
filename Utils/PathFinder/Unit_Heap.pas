unit Unit_Heap;
interface

// This heap returns minimum
Type
  TComparator = function(A,B: Pointer) : Boolean of object;

  THeap = class
  Private
    List: array of Pointer;
    HeapSize: Longint;
  Public
    Cmp: TComparator;
    Constructor Init(HeapLength: Longint);
    Destructor Done;
    Procedure Push(x: Pointer);
    Procedure Pop;
    Function GetSize: Longint;
    function IsEmpty: Boolean;
    Function ExtractMin: Pointer;
  End;

implementation
uses Unit_Finder;

// Heap init
Constructor THeap.Init(HeapLength: Longint);
Var
  i: Longint;
Begin
  SetLength(List, HeapLength + 1);
  //For i := 0 To HeapLength + 1 Do
  //  List[i] := MaxLongint;
  HeapSize := 0;
End;

function THeap.IsEmpty: Boolean;
begin
  Result := (HeapSize = 0);
end;

// Heap destruct
Destructor THeap.Done;
Begin
  HeapSize := 0;
  SetLength(List, 0);
End;

// Push element to heap
Procedure THeap.Push(x: Pointer);
Var
  i, pred: Longint;
  tmp: Pointer;
Begin
  // push element to the end of the heap
  Inc(HeapSize);
  List[HeapSize] := x;
  pred := HeapSize;
  i := HeapSize shr 1;
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
Procedure THeap.Pop;
Var
  i, pred: Longint;
  tmp: Pointer;
Begin
  // Replace last element to the top
  List[1] := List[HeapSize];
  pred := 1;
  Dec(HeapSize);
  i := 1 shl 1;
  // down element to the right place
  While not Cmp(List[i], List[pred]) or not Cmp(List[i + 1], List[pred]) and (i <= HeapSize) Do
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

Function THeap.GetSize: Longint;
Begin
  GetSize := HeapSize;
End;

Function THeap.ExtractMin: Pointer;
Begin
  ExtractMin := List[1];
End;

end.
