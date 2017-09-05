{$IFDEF CPUX64}
function InterlockedIncrement(var Addend: LongInt): LongInt;
asm
       MOV   EAX,1
  LOCK XADD  [RCX].Integer,EAX
       INC   EAX
end;

{$ELSE}

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
asm
       MOV   ECX,EAX
       MOV   EAX,[ECX]
  LOCK XCHG [ECX],EDX
end;

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
asm
       JMP InterlockedExchange
end;

function InterlockedExchangeAdd(var Addend: Integer; Increment: Integer): Integer;
asm
       MOV   ECX,EAX
       MOV   EAX,EDX
  LOCK XADD  [ECX],EAX
end;

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
       MOV   ECX,EAX
       MOV   EAX,EDX
  LOCK XADD  [ECX],EAX
       ADD   EAX,EDX
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
       MOV   EDX,1
       JMP   InterlockedAdd
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
       MOV   EDX,-1
       JMP   InterlockedAdd
end;

{$ENDIF CPUX86}



