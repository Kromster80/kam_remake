{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Headers for iconv library (LGPL) 
Creation:     Apr 25, 2010
Version:      8.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.

History:
May 07 2010  1.01 Made it compatible with C++Builder, removed declaration of
             size_t to OverbyteIcsTypes.pas
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIconv;
{$I Include\OverbyteIcsDefs.inc}

interface
{$IFDEF MSWINDOWS}
uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsTypes;

const
    {$EXTERNALSYM E2BIG}
    E2BIG           =  7;
    {$EXTERNALSYM EINVAL}
    EINVAL          = 22;
    {$EXTERNALSYM EILSEQ}
    EILSEQ          = 42;

type
    iconv_t   = Pointer;
    Piconv_t  = ^iconv_t;

    function Errno: Longint; overload;
    procedure Errno(Err: Longint); overload;

    function iconv(cd: iconv_t; InBuf: PPAnsiChar; InBytesLeft: Psize_t;
      OutBuf: PPAnsiChar; OutBytesLeft: Psize_t): size_t;
    function iconv_open(ToCode: PAnsiChar; FromCode: PAnsiChar): iconv_t;
    function iconv_close(cd: iconv_t): Integer;
    function Load_Iconv: Boolean;
{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
const
    libiconv        = 'iconv.dll';
    libmsvcrt       = 'msvcrt.dll';

type
    TIconv = function(cd: iconv_t; InBuf: PPAnsiChar; InBytesLeft: Psize_t;
       OutBuf: PPAnsiChar; OutBytesLeft: Psize_t): size_t; cdecl;
    TIconvOpen = function(ToCode: PAnsiChar; FromCode: PAnsiChar): iconv_t; cdecl;
    TIconvClose = function(cd: iconv_t): Integer; cdecl;
    TGetErrnoLocation = function: PLongint; cdecl;
var
    hIconvLib       : HMODULE = 0;
    fptrIconv       : TIconv = nil;
    fptrIconvOpen   : TIconvOpen = nil;
    fptrIconvClose  : TIconvClose = nil;
    hMsvcrt         : HMODULE = 0;
    fptrGetErrnoLocation : TGetErrnoLocation = nil;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetErrnoLocation: PLongint;
begin
    if not Assigned(fptrGetErrnoLocation) then
    begin
        hMsvcrt := LoadLibrary(libmsvcrt);
        if hMsvcrt = 0 then
            RaiseLastOSError;
        fptrGetErrnoLocation := GetProcAddress(hMsvcrt, '_errno');
        if not Assigned(fptrGetErrnoLocation) then
             RaiseLastOSError;
    end;
    Result := fptrGetErrnoLocation;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Errno: Longint;
begin
    Result := GetErrnoLocation^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Errno(Err: Longint);
begin
    GetErrnoLocation^ := Err;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function iconv(cd: iconv_t; InBuf: PPAnsiChar; InBytesLeft: Psize_t;
  OutBuf: PPAnsiChar; OutBytesLeft: Psize_t): size_t;
begin
    Result := fptrIconv(cd, InBuf, InBytesLeft, OutBuf, OutBytesLeft);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function iconv_open(ToCode: PAnsiChar; FromCode: PAnsiChar): iconv_t;
begin
    Result := fptrIconvOpen(ToCode, FromCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function iconv_close(cd: iconv_t): Integer;
begin
    Result := fptrIconvClose(cd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Load_Iconv: Boolean;
begin
    if Assigned(fptrIconv) then
        Result := TRUE
    else begin
        Result := FALSE;
        hIconvLib := LoadLibrary(libiconv);
        if hIconvLib = 0 then
            hIconvLib := LoadLibrary('lib' + libiconv);
        if hIconvLib <> 0 then
        begin
            fptrIconv      := GetProcAddress(hIconvLib, 'libiconv');
            fptrIconvOpen  := GetProcAddress(hIconvLib, 'libiconv_open');
            fptrIconvClose := GetProcAddress(hIconvLib, 'libiconv_close');
            Result := Assigned(fptrIconv) and Assigned(fptrIconvOpen) and
                      Assigned(fptrIconvClose);
            if not Result then
                fptrIconv  := nil;
        end;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
