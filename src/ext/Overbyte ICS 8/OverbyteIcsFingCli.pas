{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFingerCli is a FINGER protocol client using TWSocket
              Conform to RFC-1288 (supercede RFCs 1196, 1194 and 742)
Creation:     December 18, 1997
Version:      8.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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
Aug 20, 1999 V1.01 Added compile time options. Revised for BCB4.
Aug 18, 2001 V1.02 Angus Robertson <angus@magsys.co.uk> removed
             @domain from the request.
May 31, 2004 V1.03 Used ICSDEFS.INC, removed unused units
Mar 26, 2006 V6.00 Started new version 6
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsFingCli;
{$ENDIF}

interface

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

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket
{$ELSE}
    OverbyteIcsWSocket
{$ENDIF}
     ;

const
    FingCliVersion            = 800;
    CopyRight    : String     = ' FingCli (c) 1997-2012 F. Piette V8.00 ';

type
    TFingerCli = class(TComponent)
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   StartQuery;
        function    Receive(Buffer : Pointer; Len : Integer) : Integer;
        procedure   Abort;
    protected
        FWSocket            : TWSocket;
        FSocketFamily       : TSocketFamily;
        FQuery              : String;
        FQueryDoneFlag      : Boolean;
        FOnSessionConnected : TSessionConnected;
        FOnDataAvailable    : TDataAvailable;
        FOnQueryDone        : TSessionClosed;
        procedure WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WSocketSessionClosed(Sender: TObject; Error: Word);
        procedure TriggerQueryDone(Error: Word);
    published
        property SocketFamily : TSocketFamily           read  FSocketFamily
                                                        write FSocketFamily;
        property Query : String                         read  FQuery
                                                        write FQuery;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                        write FOnDataAvailable;
        property OnQueryDone : TSessionClosed           read  FOnQueryDone
                                                        write FOnQueryDone;
    end;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFingerCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWSocket                    := TWSocket.Create(Self);
    FSocketFamily               := FWSocket.SocketFamily;
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionClosed    := WSocketSessionClosed;
    FWSocket.OnDnsLookupDone    := WSocketDnsLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFingerCli.Destroy;
begin
    if Assigned(FWSocket) then
        FWSocket.Destroy;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.StartQuery;
var
    I    : Integer;
    Host : String;
begin
    I := Pos('@', FQuery);
    if I <= 0 then
         raise Exception.CreateFmt('TFingerCli, Invalid Query: %s', [FQuery]);
    Host := Copy(FQuery, I + 1, Length(FQuery));
    if Length(Host) <= 0 then
         raise Exception.CreateFmt('TFingerCli, Invalid Host in query: %s', [FQuery]);
    FQueryDoneFlag := FALSE;
    FWSocket.SocketFamily := FSocketFamily;
    FWSocket.Addr := Host;
    FWSocket.DnsLookup(Host);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.Abort;
begin
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFingerCli.Receive(Buffer : Pointer; Len : Integer) : Integer;
begin
    Result := FWSocket.Receive(Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        TriggerQueryDone(Error)
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := 'finger';
        FWSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketSessionConnected(Sender: TObject; Error: Word);
var
	I: integer ;
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);

    if Error <> 0 then begin
        TriggerQueryDone(Error);
        FWSocket.Close
    end
    else
    begin
        I := Pos('@', FQuery);     { angus } 
        FWSocket.SendStr(copy (FQuery, 1, pred (I)) + #13 + #10);
	end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketDataAvailable(Sender: TObject; Error: Word);
begin
    if Assigned(FOnDataAvailable) then
        FOnDataAvailable(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.TriggerQueryDone(Error: Word);
begin
    if (FQueryDoneFlag = FALSE) and Assigned(FOnQueryDone) then
        FOnQueryDone(Self, Error);
    FQueryDoneFlag := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    TriggerQueryDone(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

