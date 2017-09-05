{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNCNX.PAS
Object:       Delphi component which implement the TCP/IP telnet protocol
              including some options negociations.
              RFC854, RFC885, RFC779, RFC1091
Author:       François PIETTE
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Creation:     April, 1996
Version:      8.01
Support:      Use the mailing list twsocket@elists.org See website for details.
Legal issues: Copyright (C) 1996-2010 by François PIETTE
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
Jul 22, 1997 Adapted to Delphi 3
Sep 05, 1997 Added version information, removed old code, added OnTermType
             Renamed some indentifier to be more standard.
Sep 24, 1997 V2.03 Added procedures to negociate options
May 12, 1998 V2.04 Changed NegociateOption to properly handle unwanted
             option as Jan Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 10, 1998 V2.05 Cleared strSubOption after NegociateSubOption as Jan
             Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 15, 1999 V2.06 Moved Notification procedure to public section for
             BCB4 compatibility
Aug 20, 1999 V2.07 Added compile time options. Revised for BCB4.
Jun 18, 2001 V2.08 Use AllocateHWnd and DeallocateHWnd from wsocket.
Oct 23, 2002 V2.09 Changed Buffer arg in OnDataAvailable to Pointer instead
             of PChar to avoid Delphi 7 messing everything with AnsiChar.
May 31, 2004 V2.10 Used ICSDEFS.INC, removed unused units.
Jan 13, 2005 V2.11 Replaced symbol "Debug" by "DEBUG_OUTPUT"
Mar 11, 2006 V2.12 Arno Garrels made it NOFORMS compatible
Mar 26, 2006 V6.00 New version 6 started from V5
Aug 15, 2008 V7.00 Delphi 2009 (Unicode) support. The communication is not
             unicode, but the component support unicode strings.
Jul 17, 2011 V7.01 Arno fixed some bugs with non-Windows-1252 code pages.
Jul 18, 2011 V7.02 Arno reverted breaking changes from V7.01.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 11, 2013  V8.01 Angus added SocketFamily, LocalAddr and LocalAddr6 for IPv6

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTnCnx;

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
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$IFDEF USEWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsTypes,
    OverbyteIcsUtils,
    OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWinsock;

const
  TnCnxVersion       = 801;
  CopyRight : String = ' TTnCnx (c) 1996-2013 F. Piette V8.01 ';

  { Telnet command characters                                            }
  TNCH_EOR        = #239;     { $EF End Of Record (preceded by IAC)       }
  TNCH_SE         = #240;     { $F0 End of subnegociation parameters      }
  TNCH_NOP        = #241;     { $F1 No operation                          }
  TNCH_DATA_MARK  = #242;     { $F2 Data stream portion of a Synch        }
  TNCH_BREAK      = #243;     { $F3 NVT charcater break                   }
  TNCH_IP         = #244;     { $F4 Interrupt process                     }
  TNCH_AO         = #245;     { $F5 Abort output                          }
  TNCH_AYT        = #246;     { $F6 Are you there                         }
  TNCH_EC         = #247;     { $F7 Erase character                       }
  TNCH_EL         = #248;     { $F8 Erase line                            }
  TNCH_GA         = #249;     { $F9 Go ahead                              }
  TNCH_SB         = #250;     { $FA Subnegociation                        }
  TNCH_WILL       = #251;     { $FB Will                                  }
  TNCH_WONT       = #252;     { $FC Wont                                  }
  TNCH_DO         = #253;     { $FD Do                                    }
  TNCH_DONT       = #254;     { $FE Dont                                  }
  TNCH_IAC        = #255;     { $FF IAC                                   }

  { Telnet options                                                        }
  TN_TRANSMIT_BINARY      = #0;   { $00 }
  TN_ECHO                 = #1;   { $01 }
  TN_RECONNECTION         = #2;   { $02 }
  TN_SUPPRESS_GA          = #3;   { $03 }
  TN_MSG_SZ_NEGOC         = #4;   { $04 }
  TN_STATUS               = #5;   { $05 }
  TN_TIMING_MARK          = #6;   { $06 }
  TN_NOPTIONS             = #6;   { $06 }
  TN_DET                  = #20;  { $14 }
  TN_SEND_LOC             = #23;  { $17 }
  TN_TERMTYPE             = #24;  { $18 }
  TN_EOR                  = #25;  { $19 }
  TN_NAWS                 = #31;  { $1F }
  TN_TERMSPEED            = #32;  { $20 }
  TN_TFC                  = #33;  { $21 }
  TN_XDISPLOC             = #35;  { $23 }
  TN_EXOPL                = #255; { $FF }

  TN_TTYPE_SEND           = #1;
  TN_TTYPE_IS             = #0;

type
  TTnCnx = class;

  TTnSessionConnected = procedure (Sender: TTnCnx; Error : word) of object;
  TTnSessionClosed    = procedure (Sender: TTnCnx; Error : word) of object;
  TTnDataAvailable    = procedure (Sender: TTnCnx; Buffer : Pointer; Len : Integer) of object;
  TTnDisplay          = procedure (Sender: TTnCnx; Str : String) of object;

  TTnCnx= class(TIcsWndControl)
  private
    FPort               : String;
    FHost               : String;
    FLocation           : AnsiString;
    FTermType           : AnsiString;
    RemoteBinMode       : Boolean;
    LocalBinMode        : Boolean;
    FLocalEcho          : Boolean;
    Spga                : Boolean;
    FTType              : Boolean;
    FBuffer             : array [0..2048] of AnsiChar;
    FBufferCnt          : Integer;
    FSocketFamily       : TSocketFamily;  { V8.01 }
    FLocalAddr          : String;         { V8.01 }
    FLocalAddr6         : String;         { V8.01 }
    FOnSessionConnected : TTnSessionConnected;
    FOnSessionClosed    : TTnSessionClosed;
    FOnDataAvailable    : TTnDataAvailable;
    FOnDisplay          : TTnDisplay;
    FOnEOR              : TNotifyEvent;
    FOnSendLoc          : TNotifyEvent;
    FOnTermType         : TNotifyEvent;
    FOnLocalEcho        : TNotifyEvent;
    function GetLocation: String;
    procedure SetLocation(const Value: String);
    function GetTermType: String;
    procedure SetTermType(const Value: String);
    procedure SocketSessionConnected(Sender: TObject; Error : word);
    procedure SocketSessionClosed(Sender: TObject; Error : word);
    procedure SocketDataAvailable(Sender: TObject; Error : word);
    procedure Display(Str : String);
    procedure AddChar(Ch : AnsiChar);
    procedure ReceiveChar(Ch : AnsiChar);
    procedure Answer(chAns : AnsiChar; chOption : AnsiChar);
    procedure NegociateSubOption(const strSubOption : RawByteString);
    procedure NegociateOption(chAction : AnsiChar; chOption : AnsiChar);
    procedure FlushBuffer;
    function  GetState : TSocketState;
  public
    Socket      : TWSocket;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Send(Data : PChar; Len : Integer) : integer;
    function    SendStr(Data : String) : integer;
    procedure   Connect;
    function    IsConnected : Boolean;
    procedure   WillOption(chOption : AnsiChar);
    procedure   WontOption(chOption : AnsiChar);
    procedure   DontOption(chOption : AnsiChar);
    procedure   DoOption(chOption : AnsiChar);
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   Close;
    procedure   Pause;
    procedure   Resume;
    property    State : TSocketState                  read  GetState;
  published
    property Port : String                            read  FPort
                                                      write FPort;
    property Host : String                            read  FHost
                                                      write FHost;
    property Location : String                        read  GetLocation
                                                      write SetLocation;
    property TermType : String                        read  GetTermType
                                                      write SetTermType;
    property LocalEcho : Boolean                      read  FLocalEcho
                                                      write FLocalEcho;
    property SocketFamily  : TSocketFamily            read  FSocketFamily
                                                      write FSocketFamily;    { V8.01 }
    property LocalAddr  : String                      read  FLocalAddr
                                                      write FLocalAddr;       { V8.01 }
    property LocalAddr6 : String                      read  FLocalAddr6
                                                      write FLocalAddr6;      { V8.01 }
    property OnSessionConnected : TTnSessionConnected read  FOnSessionConnected
                                                      write FOnSessionConnected;
    property OnSessionClosed :    TTnSessionClosed    read  FOnSessionClosed
                                                      write FOnSessionClosed;
    property OnDataAvailable :    TTnDataAvailable    read  FOnDataAvailable
                                                      write FOnDataAvailable;
    property OnDisplay :          TTnDisplay          read  FOnDisplay
                                                      write FOnDisplay;
    property OnEndOfRecord :      TNotifyEvent        read  FOnEOR
                                                      write FOnEOR;
    property OnSendLoc :          TNotifyEvent        read  FOnSendLoc
                                                      write FOnSendLoc;
    property OnTermType :         TNotifyEvent        read  FOnTermType
                                                      write FOnTermType;
    property OnLocalEcho :        TNotifyEvent        read  FOnLocalEcho
                                                      write FOnLocalEcho;
  end;

implementation

{-$DEFINE DEBUG_OUTPUT}    { Add or remove minus sign before dollar sign to }
                           { generate code for debug message output         }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugString(Msg : String);
const
    Cnt : Integer = 0;
{$IFDEF DEBUG_OUTPUT}
var
    Buf : String[20];
{$ENDIF}
begin
{$IFDEF DEBUG_OUTPUT}
    Cnt := Cnt + 1;
    Buf := IntToHex(Cnt, 4) + ' ' + #0;
    WinProcs.OutputDebugString(@Buf[1]);

{$IFNDEF MSWINDOWS}
    if Length(Msg) < High(Msg) then
        Msg[Length(Msg) + 1] := #0;
{$ENDIF}

    WinProcs.OutputDebugString(@Msg[1]);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnCnx.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    FLocation                 := 'TNCNX';
    FTermType                 := 'VT100';
    FPort                     := '23';
    FSocketFamily             := DefaultSocketFamily;   { V8.01 }
    Socket                    := TWSocket.Create(Self);
    Socket.OnSessionConnected := SocketSessionConnected;
    Socket.OnDataAvailable    := SocketDataAvailable;
    Socket.OnSessionClosed    := SocketSessionClosed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTnCnx.Destroy;
begin
    if Assigned(Socket) then begin
        Socket.Free;
        Socket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if (AComponent = Socket) and (Operation = opRemove) then
        Socket := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Pause;
begin
    if not Assigned(Socket) then
        Exit;
    Socket.Pause;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Resume;
begin
    if not Assigned(Socket) then
        Exit;
    Socket.Resume;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Connect;
begin
    if not Assigned(Socket) then
        Exit;

    if Socket.State <> wsClosed then
        Socket.Close;

    Socket.Proto := 'tcp';
    Socket.Port  := FPort;
    Socket.Addr  := FHost;
    Socket.LocalAddr := FLocalAddr;       { V8.01 }
    Socket.LocalAddr6 := FLocalAddr6;     { V8.01 }
    Socket.SocketFamily := FSocketFamily; { V8.01 }
    Socket.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.IsConnected : Boolean;
begin
    Result := Socket.State = wsConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Close;
begin
    if Assigned(Socket) then
        Socket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Display(Str : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.GetLocation: String;
begin
    Result := UsAsciiToUnicode(FLocation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.GetState : TSocketState;
begin
    if Assigned(Socket) then
        Result := Socket.State
    else
        Result := wsInvalidState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.GetTermType: String;
begin
    Result := UsAsciiToUnicode(FTermType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionConnected(Sender: TObject; Error : word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionClosed(Sender: TObject; Error : word);
begin
    if Socket.State <> wsClosed then
        Socket.Close;
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketDataAvailable(Sender: TObject; Error : word);
var
    Len, I : Integer;
    Buffer : array [1..2048] of AnsiChar;
    Socket : TWSocket;
begin
    Socket := Sender as TWSocket;
    Len := Socket.Receive(@Buffer[1], High(Buffer));
    if Len = 0 then begin
        { Remote has closed }
        Display(#13 + #10 + '**** Remote has closed ****' + #13 + #10);
    end
    else if Len < 0 then begin
        { An error has occured }
        if Socket.LastError <> WSAEWOULDBLOCK then
            Display(#13 + #10 + '**** ERROR: ' + IntToStr(Socket.LastError) +
                    ' ****' + #13 + #10);
    end
    else begin
        for I := 1 to Len do
            ReceiveChar(Buffer[I]);
        FlushBuffer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TTnCnx.Send(
    Data : PChar;               // This will send Ansi!
    Len  : Integer) : integer;

{$IFDEF COMPILER12_UP}
var
    I, L : Integer;
    SBuf : array[0..1460 - 1] of AnsiChar;
{$ENDIF}

begin
    if Assigned(Socket) then

{$IFDEF COMPILER12_UP}
    begin
        L := Len;
        Result := 0;
        while L > SizeOf(SBuf) do begin
            for I := 0 to SizeOf(SBuf) - 1 do begin
                SBuf[I] := AnsiChar(Data[I]);
                Inc(Result);
            end;
            Socket.PutDataInSendBuffer(@SBuf, SizeOf(SBuf));
            Dec(L, SizeOf(SBuf));
        end;
        if L > 0 then begin
            for I := 0 to L - 1 do begin
                SBuf[I] := AnsiChar(Data[I]);
                Inc(Result);
            end;
            Socket.PutDataInSendBuffer(@SBuf, L);
        end;
        if Result > 0 then
            Socket.Send(nil, 0);
    end
{$ELSE}
        Result := Socket.Send(Data, Len)
{$ENDIF}
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.SendStr(Data : String) : integer;
begin
    Result := Send(PChar(Data), Length(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SetLocation(const Value: String);
begin
    FLocation := UnicodeToUsAscii(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SetTermType(const Value: String);
begin
    FTermType := UnicodeToUsAscii(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Answer(chAns : AnsiChar; chOption : AnsiChar);
var
    Buf   : array[0..2] of AnsiChar;
begin
{    DebugString('Answer ' + IntToHex(ord(chAns), 2) + ' ' + IntToHex(ord(ChOption), 2) + #13 + #10); }
    Buf[0] := TNCH_IAC;
    Buf[1] := chAns;
    Buf[2] := chOption;
    Socket.Send(@Buf, SizeOf(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WillOption(chOption : AnsiChar);
begin
    Answer(TNCH_WILL, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WontOption(chOption : AnsiChar);
begin
    Answer(TNCH_WONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DontOption(chOption : AnsiChar);
begin
    Answer(TNCH_DONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DoOption(chOption : AnsiChar);
begin
    Answer(TNCH_DO, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateSubOption(const strSubOption : RawByteString);
var
    Buf : RawByteString;
begin
{    DebugString('SubNegociation ' +
                IntToHex(ord(strSubOption[1]), 2) + ' ' +
                IntToHex(ord(strSubOption[2]), 2) + #13 + #10); }

    case strSubOption[1] of
    TN_TERMTYPE:
        begin
            if strSubOption[2] = TN_TTYPE_SEND then begin
{                DebugString('Send TermType' + #13 + #10); }
                if Assigned(FOnTermType) then
                    FOnTermType(Self);
                Buf := AnsiChar(TNCH_IAC) + AnsiChar(TNCH_SB) +
                       AnsiChar(TN_TERMTYPE) + AnsiChar(TN_TTYPE_IS) +
                       FTermType + AnsiChar(TNCH_IAC) + AnsiChar(TNCH_SE);
                Socket.Send(@Buf[1], Length(Buf));
            end;
        end;
    else
{        DebugString('Unknown suboption' + #13 + #10); }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateOption(chAction : AnsiChar; chOption : AnsiChar);
var
    Buf : RawByteString;
begin
{    DebugString('Negociation ' + IntToHex(ord(chAction), 2) + ' ' +
                                 IntToHex(ord(ChOption), 2) + #13 + #10); }

    case chOption of
    TN_TRANSMIT_BINARY:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                RemoteBinMode := TRUE;
                LocalBinMode  := TRUE;
            end
            else if chAction = TNCH_WONT then begin
                if RemoteBinMode then begin
                    RemoteBinMode := FALSE;
                    LocalBinMode  := FALSE;
                end;
            end;
        end;
    TN_ECHO:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                FLocalEcho := FALSE;
            end
            else if chAction = TNCH_WONT then begin
                FLocalEcho := TRUE;
            end;
            if Assigned(FOnLocalEcho) then
                FOnLocalEcho(self);
        end;
    TN_SUPPRESS_GA:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                spga := TRUE;
            end;
        end;
    TN_TERMTYPE:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                FTType := TRUE;
            end;
        end;
    TN_SEND_LOC:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                if Assigned(FOnSendLoc) then
                    FOnSendLoc(Self);
                Buf := AnsiChar(TNCH_IAC) + AnsiChar(TNCH_SB) +
                       AnsiChar(TN_SEND_LOC) + FLocation +
                       AnsiChar(TNCH_IAC) + AnsiChar(TNCH_SE);
                {Socket.}Send(@Buf[1], Length(Buf));
            end;
        end;
    TN_EOR:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                FTType := TRUE;
            end;
        end;
    else
{        Answer(TNCH_WONT, chOption); }
        { Jan Tomasek <xtomasej@feld.cvut.cz> }
        if chAction = TNCH_WILL then
            Answer(TNCH_DONT, chOption)
        else
            Answer(TNCH_WONT, chOption);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.FlushBuffer;
var
    Buffer : PAnsiChar;
    Count  : Integer;
begin
    try
        if FBufferCnt > 0 then begin
            if Assigned(FOnDataAvailable) then begin
                { We need to make a copy for the data because we can reenter   }
                { during the event processing                                  }
                Count := FBufferCnt;             { How much we received        }
                try
                    GetMem(Buffer, Count + 1);       { Alloc memory for the copy   }
                except
                    Buffer := nil;
                end;
                if Buffer <> nil then begin
                    try
                        Move(FBuffer, Buffer^, Count);   { Actual copy             }
                        Buffer[Count] := #0;             { Add a nul byte          }
                        FBufferCnt := 0;                 { Reset receivecounter    }
                        FOnDataAvailable(Self, Buffer, Count); { Call event handler  }
                    finally
                        FreeMem(Buffer, Count + 1);      { Release the buffer      }
                    end;
                end;
            end
            else begin
                FBufferCnt := 0
            end;
        end;
    except
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.AddChar(Ch : AnsiChar);
begin
    FBuffer[FBufferCnt] := Ch;
    Inc(FBufferCnt);
    if FBufferCnt >= SizeOf(FBuffer) then
        FlushBuffer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.ReceiveChar(Ch : AnsiChar);
const
    bIAC         : Boolean  = FALSE;
    chVerb       : AnsiChar = #0;
    strSubOption : RawByteString = '';
    bSubNegoc    : Boolean  = FALSE;
begin
    if chVerb <> #0 then begin
        NegociateOption(chVerb, Ch);
        chVerb       := #0;
        strSubOption := '';
        Exit;
    end;

    if bSubNegoc then begin
        if Ch = TNCH_SE then begin
            bSubNegoc    := FALSE;
            NegociateSubOption(strSubOption);
            strSubOption := '';
        end
        else
            strSubOption := strSubOption + AnsiChar(Ch);
        Exit;
    end;

    if bIAC then begin
        case Ch of
        TNCH_IAC: begin
                      AddChar(Ch);
                      bIAC := FALSE;
                  end;
        TNCH_DO, TNCH_WILL, TNCH_DONT, TNCH_WONT:
                  begin
                      bIAC   := FALSE;
                      chVerb := Ch;
                  end;
        TNCH_EOR:
            begin
                DebugString('TNCH_EOR' + #13 + #10);
                bIAC   := FALSE;
                if Assigned(FOnEOR) then
                    FOnEOR(Self);
            end;
        TNCH_SB:
            begin
{               DebugString('Subnegociation' + #13 + #10); }
                bSubNegoc := TRUE;
                bIAC      := FALSE;
            end;
        else
            DebugString('Unknown ' + IntToHex(ord(Ch), 2) + ' ''' + Char(Ch) + '''' + #13 + #10);
            bIAC := FALSE;
        end;

        Exit;
    end;

    case Ch of
    TNCH_EL:
        begin
            DebugString('TNCH_EL' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_EC:
        begin
            DebugString('TNCH_EC' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_AYT:
        begin
            DebugString('TNCH_AYT' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_IP:
        begin
            DebugString('TNCH_IP' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_AO:
        begin
            DebugString('TNCH_AO' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_IAC:
        begin
            bIAC := TRUE
        end;
    else
        AddChar(Ch);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

