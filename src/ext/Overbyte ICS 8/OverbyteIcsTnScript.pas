{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TTnScript component add scripting capabilities to TTnEmulVT
Creation:     February 24th, 1998
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org See website for details.
Legal issues: Copyright (C) 1998-2010 by François PIETTE
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

Quick Reference:

TTnScript is a descendent from TTnEmulVT. It does exactly what TTnEmulVT does
(Ansi terminal emulation) and add some scripting capabilities.
TTnScript follows the received data and search in the data stream for
given strings. When found, an event handler is called.

Strings to search are specified by calling AddEvent. Each string is identified
by an ID (an integer number) which must be unique.

You can remove a string using RemoveEvent, passing the ID you gave when
inserting the string in the list. You can remove all the strings with
RemoveAllEvents.

Each string to search for is associated with another string which will be sent
by the component when the search string is found. This can be used for example
when you search for a login prompt ('login') to send the username when this
prompt is found. Same for password.

Each string to search for is also associated with an event handler which will
be triggered when the string is found, right after having sent the string to
send. This specific event can be used to customize what has to be done when
the string is found (for example update the user interface or query the user
for some value to send).

Finally, each string to search is associated with a set of flags which tells
the component some special actions such as ignoring character case when
comparing text, or make the string persistant (normaly when a string has been
found, it is removed from the list).

Strings are searched in the order they are added to the list. So it can be
very different if you add 'login' and 'password' to search for than if you
add 'login' only and then when 'login' is found, add 'password'.

To scan the data stream, the component use a circular buffer whose dimension
is 80 characters by default. You can change that by assigning InputBufferSize.
The buffer size should be at least twice the size of the longest string to
search. If you use an oversized buffer, you have a performance penalty because
the buffer is searched as each data packet comes into the stream.

An automatic login procedure could looks like this:
    TnScript1.AddEvent(1, 'login',    'root' + #13#10, [efIgnoreCase], nil);
    TnScript1.AddEvent(2, 'password', 'boss' + #13#10, [efIgnoreCase], nil);
    TnScript1.Connect;

The nil argument could be replaced by a procedure (event handler) to make some
computing when the string to search for is found. Here is an example:

    TnScript1.AddEvent(2, 'prompt', '', [efIgnoreCase], PromptEvent);

procedure TForm1.PromptEvent(Sender : TObject; ID : Integer);
begin
    .... Your code goes here. You can do everithing ....
    Label1.Caption := 'Logged !';
    TnScript1.SendStr('ls -l' + #13 + #10);    Like sending some data
end;

Updates:
Mar 14, 1999  V1.01 efIgnoreCase was ignored !
May 13, 1999  V1.02 Made all methods virtual.
Aug 20, 1999  V1.03 Added compile time options. Revised for BCB4.
Jul 26, 2004  V1.04 Fixed FindEventString that was not checking last character
              of event string.
Jul 30, 2005  V1.05 Fixed two more bugs related to the previous one:
              - the last character of a packet was not checked
              - only one event per packet was checked
              See "{FP 30/07/2004" for changes
Mar 26, 2006  V6.00 New version 6 started
Feb 01, 2010  V6.01 Fixed Ansi/Unicode issue in ProcessInputData.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTnScript;

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
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

{.DEFINE DUMP}

uses
{$IFDEF USEWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsTnEmulVT;

const
    TnScriptVersion    = 8.00;
    CopyRight : String = ' TTnScript (c) 1998-2012 F. Piette V8.00 ';

type
    TnScriptException = class(Exception);
    TEventHandler = procedure (Sender : TObject; ID : Integer) of object;
    TEventFlag    = (efIgnoreCase,         { Ignore case in comparaisons   }
                     efPersistent);        {Do not delete event when found }
    TEventFlags   = set of TEventFlag;
    TDisplayEvent = procedure (Sender : TObject; Msg : String) of object;
    TStringMatch  = procedure (Sender : TObject; ID : Integer) of object;

    TEventDescriptor = record
        ID      : Integer;
        Search  : String;
        ToSend  : String;
        Flags   : TEventFlags;
        Handler : TEventHandler;
    end;
    PEventDescriptor = ^TEventDescriptor;

    TTnScript = class(TTnEmulVT)
    protected
        FEventList        : TList;
        FInputBuffer      : PChar;
        FInputBufferSize  : Integer;
        FInputBufferCount : Integer;
        FInputBufferStart : Integer;
        FOnDisplay        : TDisplayEvent;
        FOnStringMatch    : TStringMatch;
        function  SearchEvent(ID : Integer) : Integer; virtual;
        procedure TriggerDataAvailable(Buffer : Pointer; Len: Integer); override;
        function  FindEventString(S : String; Flags : TEventFlags) : Integer; virtual;
        procedure ScanEvents; virtual;
        procedure ProcessInputData(Buffer: PAnsiChar; Len: Integer); virtual;
        procedure TriggerDisplay(Msg : String); virtual;
        procedure TriggerStringMatch(ID : Integer); virtual;
        procedure NextOne(var N : Integer); virtual;
        procedure SetInputBufferSize(newSize : Integer); virtual;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure AddEvent(ID      : Integer;
                           Search  : String;
                           ToSend  : String;
                           Flags   : TEventFlags;
                           Handler : TEventHandler); virtual;
        procedure RemoveEvent(ID : Integer); virtual;
        procedure RemoveAllEvents; virtual;
    published
        property InputBufferSize : Integer         read  FInputBufferSize
                                                   write SetInputBufferSize;
        property OnDisplay : TDisplayEvent         read  FOnDisplay
                                                   write FOnDisplay;
        property OnStringMatch : TStringMatch      read  FOnStringMatch
                                                   write FOnStringMatch;

    end;

implementation

{$IFDEF DUMP}
const
    CtrlCode : array [0..31] of String = ('NUL', 'SOH', 'STX', 'ETX',
                                          'EOT', 'ENQ', 'ACK', 'BEL',
                                          'BS',  'HT',  'LF',  'VT',
                                          'FF',  'CR',  'SO',  'SI',
                                          'DLE', 'DC1', 'DC2', 'DC3',
                                          'DC4', 'NAK', 'SYN', 'ETB',
                                          'CAN', 'EM',  'SUB', 'ESC',
                                          'FS',  'GS',  'RS',  'US');
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnScript.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FEventList := TList.Create;
    SetInputBufferSize(80);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTnScript.Destroy;
begin
    if Assigned(FEventList) then begin
        FEventList.Free;
        FEventList := nil;
    end;
    if FInputBuffer <> nil then begin
        FreeMem(FInputBuffer, FInputBufferSize);
        FInputBuffer := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Set the input buffer size. This will clear any data already in the buffer }
procedure TTnScript.SetInputBufferSize(newSize : Integer);
begin
    { Round the size to the nearest upper 16 bytes limit }
    newSize := ((newSize shr 4) + 1) shl 4;

    { If no change, do nothing }
    if FInputBufferSize = newSize then
        Exit;

    { If buffer already allocated, free it }
    if FInputBuffer <> nil then begin
        FreeMem(FInputBuffer, FInputBufferSize);
        FInputBuffer := nil;
    end;

    { Allocate a new buffer of the given size }
    FInputBufferSize := newSize;
    GetMem(FInputBuffer, FInputBufferSize);

    { Clear the markers }
    FInputBufferStart := 0;
    FInputBufferCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.TriggerStringMatch(ID : Integer);
begin
    if Assigned(FOnStringMatch) then
        FOnStringMatch(Self, ID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnScript.SearchEvent(ID : Integer) : Integer;
begin
    if Assigned(FEventList) then begin
        for Result := 0 to FEventList.Count - 1 do begin
            if PEventDescriptor(FEventList.Items[Result])^.ID = ID then
                Exit;
        end;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Add an event (a string to search for) to the list                         }
procedure TTnScript.AddEvent(
    ID      : Integer;
    Search  : String;
    ToSend  : String;
    Flags   : TEventFlags;
    Handler : TEventHandler);
var
    NewEvent : PEventDescriptor;
begin
    if not Assigned(FEventList) then
        raise TnScriptException.Create('AddEvent: No Event List');

    if SearchEvent(ID) <> -1 then
        raise TnScriptException.Create('AddEvent: ID ' + IntToStr(ID) +
                                       ' already exist');
    if Length(Search) <= 0 then
        raise TnScriptException.Create('AddEvent: String to search empty');

    New(NewEvent);
    FEventList.Add(NewEvent);
    NewEvent^.ID      := ID;
    NewEvent^.ToSend  := ToSend;
    NewEvent^.Flags   := Flags;
    NewEvent^.Handler := Handler;
    if efIgnoreCase in Flags then
        NewEvent^.Search  := UpperCase(Search)
    else
        NewEvent^.Search  := Search;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Remove an event from the list, given his ID                               }
procedure TTnScript.RemoveEvent(ID : Integer);
var
    Item   : Integer;
    PEvent : PEventDescriptor;
begin
    if not Assigned(FEventList) then
        raise TnScriptException.Create('AddEvent: No Event List');

    Item := SearchEvent(ID);
    if Item < 0 then
        raise TnScriptException.Create('RemoveEvent: ID ' + IntToStr(ID) +
                                       ' does''nt exist');
    PEvent := FEventList.Items[Item];

    { Replace the ID to check later that we do not reuse the freed event }
    PEvent^.ID := -1;

    { Free the memory and remove the pointer from list }
    Dispose(PEvent);
    FEventList.Delete(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.RemoveAllEvents;
var
    PEvent : PEventDescriptor;
begin
    if not Assigned(FEventList) then
        raise TnScriptException.Create('AddEvent: No Event List');

    while FEventList.Count > 0 do begin
        PEvent := FEventList.Items[0];
        PEvent^.ID := -1;
        Dispose(PEvent);
        FEventList.Delete(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DUMP}
procedure WriteCh(Ch : Char);
begin
    if ord(Ch) < 32 then
        write('<', CtrlCode[Ord(Ch)], '>')
    else
        write(Ch);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WriteBuf(Buffer : PChar; Len : Integer);
var
    I : Integer;
begin
    for I := 0 to Len - 1 do
        WriteCh(Buffer[I]);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Advance char index in the circular buffer                                 }
procedure TTnScript.NextOne(var N : Integer);
begin
    Inc(N);
    if N >= FInputBufferSize then
        N := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Search for a string in the circular buffer.                               }
{ Returns the number of chars between the buffer start and the end of the   }
{ event found, or -1 if not found.                                          }
function TTnScript.FindEventString(S : String; Flags : TEventFlags) : Integer;
var
    N, M, I, J, K : Integer;
    Ch            : Char;
begin
    Result := -1;
    I      := FInputBufferStart;
    N      := 0;
    while N < FInputBufferCount do begin
        if efIgnoreCase in Flags then
            Ch := UpperCase(FInputBuffer[I])[1]
        else
            Ch := FInputBuffer[I];

        if Ch = S[1] then begin
            { Same first letter, check up to end of S }
            J := I;
            K := 2;
            M := N;
            while TRUE do begin
                if K > Length(S) then begin {FP 30/07/2004 moved here }
                    { Found ! }             {FP 30/07/2004            }
                    Result := M + 1;        {FP 30/07/2004            }
                    Exit;                   {FP 30/07/2004            }
                end;                        {FP 30/07/2004            }

                NextOne(J);

                Inc(M);
                if M >= FInputBufferCount then
                    break;

                if efIgnoreCase in Flags then
                    Ch := UpperCase(FInputBuffer[J])[1]
                else
                    Ch := FInputBuffer[J];
                if Ch <> S[K] then
                    break;     { Compare failed }
                Inc(K);
            end;
        end;

        NextOne(I);
        Inc(N);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_FindEventString}
function TTnScript.FindEventString(S : String; Flags : TEventFlags) : Integer;
var
    N, M, I, J, K : Integer;
    Ch            : Char;
begin
    Result := -1;
    I      := FInputBufferStart;
    N      := 0;
    while N < FInputBufferCount do begin
        if efIgnoreCase in Flags then
            Ch := UpperCase(FInputBuffer[I])[1]
        else
            Ch := FInputBuffer[I];

        if Ch = S[1] then begin
            { Same first letter, check up to end of S }
            J := I;
            K := 2;
            M := N;
            while TRUE do begin
                NextOne(J);

                Inc(M);
                if M >= FInputBufferCount then
                    break;

                if K > Length(S) then begin
                    { Found ! }
                    Result := M + 1;
                    Exit;
                end;
                if efIgnoreCase in Flags then
                    Ch := UpperCase(FInputBuffer[J])[1]
                else
                    Ch := FInputBuffer[J];
                if Ch <> S[K] then
                    break;     { Compare failed }
                Inc(K);
            end;
        end;

        NextOne(I);
        Inc(N);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.ScanEvents;
var
    Item    : Integer;
    PEvent  : PEventDescriptor;
    I       : Integer;
{$IFDEF DUMP}
    J       : Integer;
{$ENDIF}
    ID      : Integer;
    Handler : TEventHandler;
begin
{$IFDEF DUMP}
    Write('ScanEvents Start=', FInputBufferStart,
                    ' Count=', FInputBufferCount,
                     ' ''');
    I := FInputBufferStart;
    for J := 1 to FInputBufferCount do begin
        WriteCh(FInputBuffer[I]);
        NextOne(I);
    end;
    WriteLn('''');
{$ENDIF}

    Item := 0;                             {FP 30/07/2004 }
    while Item < FEventList.Count do begin {FP 30/07/2004 }
        PEvent := PEventDescriptor(FEventList.Items[Item]);
{$IFDEF DUMP}
WriteLn('Searhing ''', PEvent^.Search, '''');
{$ENDIF}
        I := FindEventString(PEvent^.Search, PEvent^.Flags);
        if I <> -1 then begin
{$IFDEF DUMP}
            WriteLn('Found event ''', PEvent^.Search, '''');
{$ENDIF}
            TriggerDisplay('Event ''' + PEvent^.Search + '''');
            { Here we delete the character up to the string we just found }
            { This means that we could not see other strings defined in   }
            { events after the one we just found.                         }
            FInputBufferCount := FInputBufferCount - I;
            FInputBufferStart := FInputBufferStart + I;
            if FInputBufferStart >= FInputBufferSize then
                FInputBufferStart := FInputBufferStart - FInputBufferSize;
            ID      := PEvent^.ID;
            Handler := PEvent^.Handler;
            if Length(PEvent^.ToSend) > 0 then
                SendStr(PEvent^.ToSend);
            { Call the global event handler OnStringMatch }
            TriggerStringMatch(ID);
            { Call the specific event handler }
            if Assigned(Handler) then
                Handler(Self, ID);
            { It's possible that the event has been removed !  }
            { Make sure it is always there before using it     }
            try
                if PEvent^.ID = ID then begin
                    if not (efPersistent in PEvent^.FLags) then begin
                        RemoveEvent(ID);
                        Dec(Item);  {FP 30/07/2004 }
                    end;
                end;
            except
                { Ignore any exception }
            end;
{FP  30/07/2004           Exit;                                       }
{ Exit here would suppress search for more events in the same packet. }
{ They would be searched only in when the next packet comes in,       }
{ if it ever comes in...                                              }
        end;
        Inc(Item);   {FP 30/07/2004                                   }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.ProcessInputData(Buffer: PAnsiChar; Len: Integer);
const
    Recurse : Integer = 0;
var
    I, J : Integer;
begin
    if not Assigned(FInputBuffer) then
        Exit;

    Inc(Recurse); { For debugging purpose }

    if Len > (FInputBufferSize div 2) then begin
        { Input buffer too small, process recursively two halfs }
        ProcessInputData(Buffer, Len div 2);
        ProcessInputData(Buffer + (Len div 2), Len - (Len div 2));
        Dec(Recurse);
        Exit;
    end;

{$IFDEF DUMP}
    WriteLn;
    Write({Calls, ' ',} Recurse, ' ', FInputBufferStart, ' ',
          FInputBufferCount, ') Len=', Len, ' Buffer=''');
    WriteBuf(Buffer, Len);
    WriteLn('''');
{$ENDIF}

    { Where is the end of the circular buffer, that's the question ! }
    I := FInputBufferStart + FInputBufferCount;
    if I >= FInputBufferSize then
         I := I - FInputBufferSize;

    { Add data to the end of the circular buffer, overwriting any previously }
    { stored data (remember, we don't ever receive more than 1/2 buffer size }
    J := 0;
    while J < Len do begin
        FInputBuffer[I] := Char(Buffer[J]);
        Inc(J);
        NextOne(I);
        if FInputBufferCount = FInputBufferSize then
            NextOne(FInputBufferStart)
        else
            Inc(FInputBufferCount);
    end;
    { Scan for events }
    ScanEvents;

    Dec(Recurse); { For debugging purpose }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnScript.TriggerDataAvailable(Buffer : Pointer; Len: Integer);
begin
    if FEventList.Count > 0 then
        ProcessInputData(PAnsiChar(Buffer), Len);

    inherited TriggerDataAvailable(Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

