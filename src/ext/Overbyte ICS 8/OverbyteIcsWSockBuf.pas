{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TIcsBufferHandler is a class which encapsulate a data fifo to
              be used for TWSocket.
              Fifo is implemented using two doubly linked list of buffers.
              One linked list contain the buffers filled with data while the 
              other contain the buffers already emptyed. When a buffer is 
              emptyed it is placed in the free list. When a buffer is needed
              to hold more data, it is taken from the free list, if any, or
              a new one is created.
Creation:     June 11, 2006 (Built from basic version created in april 1996)
Version:      8.00 (Initial version was 6.01 to match TWSocket version)
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Mar 06, 1998  V2.00 Added a property and a parameter for the create method
              to select the buffer size. Using a 0 value will make the object
              use the default 1514 bytes (the largest size for an ethernet
              packet).
Jul 08, 1998  V2.01 Adadpted for Delphi 4
Jan 19, 2003  V5.00 First pre-release for ICS-SSL. New major version number.
              Skipped version numbers to mach wsocket.pas major version number.
Apr 17, 2004  V6.00 New major release started. Move all platform and runtime
              dependencies to separate units. New base component for handling
              component with window handle.
Jan 26, 2004  V5.01 Use ICSDEFS.INC and reordered uses clause for FPC
              compatibility.
May 23, 2005  V6.00b Added BufUsed property.
Mar 25, 2006  V6.00c Fixed TBuffer.Write to correctly use the offset. Thanks
              to Frans van Daalen <ics@hedaal.nl> for providing a test case.
June 11, 2006 V6.01 New version with TIcsBufferHandler. Take all of the
              buffer handling out of TWSocket.
Apr 15, 2011  V6.02 Arno prepared for 64-bit.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWSockBuf;
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Contnrs{$ELSE}Contnrs{$ENDIF},
  OverbyteIcsTypes;

const
  WSockBufVersion    = 800;
  CopyRight : String = ' TWSockBuf (c) 1996-2012 Francois Piette V8.00 ';

type
{$IFDEF CLR}
  TWSocketData = TBytes;
{$ELSE}
  TWSocketData = type Pointer;
{$ENDIF}

  TIcsBuffer = class(TObject)
  protected
    Buf      : TWSocketData;
    FBufSize : Integer;
    WrCount  : Integer;
    RdCount  : Integer;
    FNext    : TIcsBuffer;
    FPrev    : TIcsBuffer;
    function    GetBufUsed : Integer;
    procedure   SetBufSize(newSize : Integer);
  public
    constructor Create(nSize : Integer); virtual;
    destructor  Destroy; override;
    function    Write(const Data : TWSocketData; Len : Integer) : Integer; overload;
    function    Write(const Data : TWSocketData; Offset : Integer; Len : Integer) : Integer; overload;
    function    Read(out Data : TWSocketData; Len : Integer) : Integer;
    function    Peek(var Len : Integer) : TWSocketData; overload;
    function    Peek(out Data : TWSocketData) : Integer; overload;
    function    Remove(Len : Integer) : Integer;
    property    BufSize : Integer read FBufSize write SetBufSize;
    property    BufUsed : Integer read GetBufUsed;
  end;

  TIcsBufferLinkedList = class(TObject)
  protected
    Head : TIcsBuffer;
    Tail : TIcsBuffer;
  public
    destructor Destroy; override;
    procedure AddToListHead(Buf : TIcsBuffer);
    function  RemoveFromListHead: TIcsBuffer;
    procedure AddToListTail(Buf: TIcsBuffer);
  end;

{$IFDEF CLR}
 [DesignTimeVisibleAttribute(TRUE)]
  TIcsBufferHandler = class(System.ComponentModel.Component)
{$ELSE}
type
  TIcsBufferHandler = class(TComponent)
{$ENDIF}
  protected
    FInUseList : TIcsBufferLinkedList;
    FFreeList  : TIcsBufferLinkedList;
    FBufSize   : Integer;
    FCritSect  : TCriticalSection;
    function    GetBuffer: TIcsBuffer;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure   UnLock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure   Write(const Data : TWSocketData; Len : Integer); overload;
    procedure   Write(const Data : TWSocketData; Offset : Integer; Len : Integer); overload;
    function    Peek(var Len : Integer) : TWSocketData; overload;
    function    Peek(out Data : TWSocketData) : Integer; overload;
    procedure   Remove(Len : Integer);
    procedure   DeleteAllData;
    function    IsEmpty : Boolean;
  published
    property BufSize : Integer read  FBufSize write FBufSize;
  end;
  
function IncPtr(P : Pointer; N : Integer = 1): Pointer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
 
implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IncPtr(P : Pointer; N : Integer = 1): Pointer;
begin
    Result := PAnsiChar(P) + N;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBuffer.Create(nSize : Integer);
begin
    // OutputDebugString('TIcsBuffer.Create');
    inherited Create;
    WrCount  := 0;
    RdCount  := 0;
    SetBufSize(nSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBuffer.Destroy;
begin
    // OutputDebugString('TIcsBuffer.Destroy');
{$IFNDEF CLR}
    if Assigned(Buf) then
        FreeMem(Buf, FBufSize);
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBuffer.SetBufSize(newSize : Integer);
{$IFNDEF CLR}
var
    newBuf : TWSocketData;
{$ENDIF}
begin
    if newSize <= 0 then
        newSize := 1460;

    if newSize = FBufSize then
        Exit;

{$IFDEF CLR}
    FBufSize := newSize;
    SetLength(Buf, FBufSize);
{$ELSE}
    if WrCount = RdCount then begin
        // Buffer is empty
        if Assigned(Buf) then
            FreeMem(Buf, FBufSize);
        FBufSize := newSize;
        if FBufSize > 0 then
            GetMem(Buf, FBufSize)
        else
            Buf := nil;
    end
    else begin
        // Buffer contains data
        if newSize > 0 then begin
            GetMem(newBuf, newSize);
            Move(Buf^, newBuf^, WrCount);
        end
        else
            newBuf := nil;
        if Assigned(Buf) then
            FreeMem(Buf, FBufSize);
        FBufSize := newSize;
        Buf      := newBuf;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.Write(const Data : TWSocketData; Len : Integer) : Integer;
begin
    Result := Write(Data, 0, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.Write(const Data: TWSocketData; Offset, Len: Integer): Integer;
var
    Remaining : Integer;
    Copied    : Integer;
{$IFDEF CLR}
    I         : Integer;
{$ENDIF}
begin
    Remaining := FBufSize - WrCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
{$IFDEF CLR}
        for I := 0 to Copied - 1 do
            Buf[WrCount + I] := Data[Offset + I];
{$ELSE}
        Move(IncPtr(Data, Offset)^, IncPtr(Buf, WrCount)^, Copied);
{$ENDIF}
        WrCount := WrCount + Copied;
        Result  := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.Read(out Data : TWSocketData; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
{$IFDEF CLR}
    I         : Integer;
{$ENDIF}
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
{$IFDEF CLR}
        for I := 0 to Copied - 1 do
            Data[I] := Buf[RdCount + I];
{$ELSE}
        Move(IncPtr(Buf, RdCount)^, Data^, Copied);
{$ENDIF}
        RdCount := RdCount + Copied;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.Peek(var Len : Integer) : TWSocketData;
var
    Remaining : Integer;
{$IFDEF CLR}
    I         : Integer;
{$ENDIF}
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        Len    := 0;
        Result := nil;
    end
    else begin
        Len    := Remaining;
{$IFDEF CLR}
        SetLength(Result, Len);
        for I := 0 to Len - 1 do
            Result[I] := Buf[I];
{$ELSE}
        Result := IncPtr(Buf, RdCount);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF CLR}
function TIcsBuffer.Peek(out Data: TWSocketData): Integer;
var
    Remaining : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        Result := 0;
    end
    else begin
        Data := IncPtr(Buf, RdCount);
        Result := Remaining;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function TIcsBuffer.Peek(out Data: TWSocketData): Integer;
var
    Remaining : Integer;
    I         : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        SetLength(Data, 0);
        Result := 0;
    end
    else begin
        // With .NET, we must copy the data
        SetLength(Data, Remaining);
        for I := 0 to Remaining - 1 do
            Data[I] := Buf[RdCount + I];
        Result := Remaining;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.Remove(Len : Integer) : Integer;
var
    Remaining : Integer;
    Removed   : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len < Remaining then
            Removed := Len
        else
            Removed := Remaining;
        RdCount := RdCount + Removed;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Removed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffer.GetBufUsed: Integer;
begin
    Result := WrCount - RdCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsBufferHandler }
constructor TIcsBufferHandler.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FInUseList := TIcsBufferLinkedList.Create;
    FFreeList  := TIcsBufferLinkedList.Create;
    FBufSize   := 1460;
    FCritSect  := TCriticalSection.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBufferHandler.Destroy;
begin
    FreeAndNil(FInUseList);
    FreeAndNil(FFreeList);
    FCritSect.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferLinkedList.AddToListHead(Buf : TIcsBuffer);
begin
    if not Assigned(Head) then begin
        Head      := Buf;
        Tail      := Buf;
        Buf.FNext := nil;
        Buf.FPrev := nil;
    end
    else begin
        Head.FPrev := Buf;
        Buf.FPrev  := nil;
        Buf.FNext  := Head;
        Head       := Buf; 
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferLinkedList.AddToListTail(Buf : TIcsBuffer);
begin
    if not Assigned(Tail) then begin
        Tail      := Buf; 
        Head      := Buf;
        Buf.FPrev := nil;
        Buf.FNext := nil;
    end
    else begin
        Tail.FNext := Buf;
        Buf.FPrev  := Tail;
        Buf.FNext  := nil;
        Tail       := Buf;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBufferLinkedList.Destroy;
var
    Buf1, Buf2 : TIcsBuffer;
begin
    Buf1 := Head;
    while Assigned(Buf1) do begin
        Buf2 := Buf1.FNext;
        Buf1.Free;
        Buf1 := Buf2;
    end;
    Head := nil;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferLinkedList.RemoveFromListHead: TIcsBuffer;
begin
    Result := Head;
    if Assigned(Result) then begin
        Head := Result.FNext;
        if Assigned(Head) then
            Head.FPrev := nil
        else
            Tail := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferHandler.GetBuffer: TIcsBuffer;
begin
    if Assigned(FFreeList.Head) then
        Result := FFreeList.RemoveFromListHead   // Reuse any free buffer
    else
        Result := TIcsBuffer.Create(FBufSize);
    FInUseList.AddToListTail(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.DeleteAllData;
var
    Buf : TIcsBuffer;
begin
    Buf := FInUseList.RemoveFromListHead;
    while Assigned(Buf) do begin
        Buf.RdCount := 0;                     // Clear data
        Buf.WrCount := 0;
        FFreeList.AddToListHead(Buf);         // Put in free list
        Buf := FInUseList.RemoveFromListHead;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferHandler.IsEmpty: Boolean;
begin
    Result := (FInUseList.Head = nil) or 
              (FInUseList.Head.GetBufUsed = 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.Lock;
begin
    FCritSect.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.UnLock;
begin
    FCritSect.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferHandler.Peek(var Len: Integer): TWSocketData;
begin
    if not Assigned(FInUseList.Head) then begin
        Len    := 0;
        Result := nil;
    end
    else
        Result := FInUseList.Head.Peek(Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferHandler.Peek(out Data: TWSocketData): Integer;
begin
    if not Assigned(FInUseList.Head) then
        Result := 0
    else
        Result := FInUseList.Head.Peek(Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.Remove(Len: Integer);
var
    Buf : TIcsBuffer;
begin
    if Len < 0 then
        raise Exception.Create('TIcsBufferHandler.Remove: Invalid Len ' + IntToStr(Len));
    if not Assigned(FInUseList.Head) then
        raise Exception.Create('TIcsBufferHandler.Remove: nothing to remove');
       
    Buf := FInUseList.Head;
    Buf.Remove(Len);
    if Buf.GetBufUsed = 0 then begin
        Buf := FInUseList.RemoveFromListHead;
        FFreeList.AddToListHead(Buf);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.Write(
    const Data : TWSocketData;
    Len        : Integer);
begin
    Write(Data, 0, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferHandler.Write(
    const Data : TWSocketData;
    Offset     : Integer;
    Len        : Integer);
var
    oBuffer  : TIcsBuffer;
    iOffset  : Integer;
    bMore    : Boolean;
    cWritten : Integer;
begin
    if not Assigned(FInUseList.Tail) then
        oBuffer := GetBuffer
    else
        oBuffer := FInUseList.Tail;
    iOffset := 0;
    bMore   := TRUE;
    while bMore do begin
        cWritten := oBuffer.Write(Data, iOffset, Len);
        if cWritten >= Len then
            bMore := FALSE
        else begin
            Len  := Len - cWritten;
            Inc(iOffset, cWritten);
            if Len < 0 then
                bMore := FALSE
            else begin
                oBuffer := GetBuffer;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

