{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 01, 2004
Version:      8.00
Description:  This is an implementation of the NTLM authentification
              messages used within HTTP protocol (client side).
              NTLM protocol documentation can be found at:
              http://davenport.sourceforge.net/ntlm.html
Credit:       This code is based on a work by Diego Ariel Degese
              <crapher@utenet.com.ar>. The code was not working at all but
              helped me a lot starting with NTLM.
              Csonka Tibor <bee@rawbite.ro> worked a lot on my original code,
              fixing it and making it work properly.
EMail:        francois.piette@overbyte.be     http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2011 by François PIETTE
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
Mar 26, 2006 V6.00 New version 6 started
Apr 25, 2008 V6.01 A. Garrels - Fixed function Unicode. NtlmGetMessage3() got a
                   new parameter ACodepage : LongWord that defaults to the
                   currently active codepage. Some changes to prepare code for
                   Unicode.
Dec 13, 2010 V6.02 A. Garrels - Fixed wrong offset of target domain in
                   NtlmGetMessage1(). Added procedure NtlmParseUserCode.
Feb 18, 2011 V6.03 procedure NtlmParseUserCode takes an NTLM version argument.
Feb 26, 2011 V6.04 Function NtlmGetMessage2 returned garbage WideStrings with
                   ANSI compilers, those strings were not used in ICS so far.
                   Changed string-types of TNTLM_Msg2_Info in 2009+ from
                   WideString to UnicodeString with small chance that breaks
                   some user code.
Jul 22, 2011 V6.05 Arno - OEM NTLM changes.
Feb 17, 2012 V6.06 Arno added NTLMv2 and NTLMv2 session security (basics).
Feb 29, 2012 V6.07 Arno - Use IcsRandomInt, .Net code removed.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


HowTo NTLMv2:
-------------
There's a new argument LmCompatLevel that has to be passed to NtlmGetMessage1/3.
Components using NTLM got a new property LmCompatLevel: LongWord that defaults to
zero.

This value can be read from the registry:
HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\LSA\LMCompatibilityLevel

And also be changed with gpedit.msc:
Computer Configuration/ Windows Settings/
Security Settings/Local Policies/Security Options/LAN Manager authentication level

LAN Manager authentication level ( by Microsoft, Server 2008 R2 )

This security setting determines which challenge/response authentication protocol
is used for network logons. This choice affects the level of authentication
protocol used by clients, the level of session security negotiated, and the level
of authentication accepted by servers as follows:

Level 0
Send LM & NTLM responses: Clients use LM and NTLM authentication and never use
NTLMv2 session security; domain controllers accept LM, NTLM, and NTLMv2
authentication.

Level 1
Send LM & NTLM - use NTLMv2 session security if negotiated: Clients use LM and
NTLM authentication and use NTLMv2 session security if the server supports it;
domain controllers accept LM, NTLM, and NTLMv2 authentication.

Level 2
Send NTLM response only: Clients use NTLM authentication only and use NTLMv2
session security if the server supports it; domain controllers accept LM, NTLM,
and NTLMv2 authentication.

Level 3
Send NTLMv2 response only: Clients use NTLMv2 authentication only and use NTLMv2
session security if the server supports it; domain controllers accept LM, NTLM,
and NTLMv2 authentication.

Level4
Send NTLMv2 response only\refuse LM: Clients use NTLMv2 authentication only and
use NTLMv2 session security if the server supports it; domain controllers refuse
LM (accept only NTLM and NTLMv2 authentication).

Level 5
Send NTLMv2 response only\refuse LM & NTLM: Clients use NTLMv2 authentication
only and use NTLMv2 session security if the server supports it; domain
controllers refuse LM and NTLM (accept only NTLMv2 authentication).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsNtlmMsgs;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$R-}
{$Q-}
{$IFDEF COMPILER12_UP}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{#$DEFINE SUPPORT_WIN95}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Ics.Posix.Wintypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsDES, OverbyteIcsMD4, OverbyteIcsMD5,
    OverbyteIcsUtils,
    OverbyteIcsTypes,
    OverbyteIcsMimeUtils;

const
    IcsNtlmMsgsVersion     = 800;
    CopyRight : String     = ' IcsNtlmMsgs (c) 2004-2012 F. Piette V8.00 ';

const
    Flags_Negotiate_Unicode               = $00000001;
    Flags_Negotiate_OEM                   = $00000002;
    Flags_Request_Target                  = $00000004;
    Flags_Negotiate_8                     = $00000008;  // r10 (1 bit): This bit is unused and MUST be zero
    Flags_Negotiate_Sign                  = $00000010;
    Flags_Negotiate_Seal                  = $00000020;
    Flags_Negotiate_Datagram_Style        = $00000040;
    Flags_Negotiate_LAN_Manager_Key       = $00000080;
    Flags_Negotiate_100                   = $00000100;  //r9 (1 bit): This bit is unused and MUST be zero.
    Flags_Negotiate_NTLM                  = $00000200;
    Flags_Negotiate_400                   = $00000400;  // unknown
    Flags_Negotiate_800                   = $00000800;  // unknown
    Flags_Negotiate_Domain_Supplied       = $00001000;
    Flags_Negotiate_Workstation_Supplied  = $00002000;
    Flags_Negotiate_Local_Call            = $00004000;
    Flags_Negotiate_Allways_Sign          = $00008000;
    Flags_Target_Type_Domain              = $00010000;
    Flags_Target_Type_Server              = $00020000;
    Flags_Target_Type_Share               = $00040000;
    Flags_Negotiate_NTLM2_Key             = $00080000; //NTLMSSP_NEGOTIATE_EXTENDED_SESSIONSECURITY
    Flags_Request_Init_Response           = $00100000;
    Flags_Request_Accept_Response         = $00200000;
    Flags_Request_Non_NT_Session_Key      = $00400000;
    Flags_Negotiate_Target_Info           = $00800000;
    Flags_Negotiate_1000000               = $01000000;  // unknown
    Flags_Negotiate_Version               = $02000000;
    Flags_Negotiate_4000000               = $04000000;  // unknown
    Flags_Negotiate_8000000               = $08000000;  // unknown
    Flags_Negotiate_10000000              = $10000000;  // unknown
    Flags_Negotiate_128_Bit_Encryption    = $20000000;
    Flags_Negotiate_Key_Exchange          = $40000000;
    Flags_Negotiate_56_Bit_Encryption     = $80000000;

    // target information block types
    TIB_Type_Server                       = 1;
    TIB_Type_Domain                       = 2;
    TIB_Type_DNS_Full                     = 3;
    TIB_Type_DNS_Domain                   = 4;
    TIB_Type_ask_microsoft_or_god         = 5;

type
    TNTLM_OSVersion = array[0..7] of Byte;
    // security buffer
    TNTLM_SecBuff   = packed record
        Length        : Word;
        Space         : Word;
        Offset        : Cardinal;
    end;

    // first message
    TNTLM_Message1  = packed record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        Flags         : Cardinal;
        Domain        : TNTLM_SecBuff;
        Host          : TNTLM_SecBuff;
        //OSVer         : TNTLM_OSVersion;
    end;

    // second message
    TNTLM_Message2  = packed record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        TargetName    : TNTLM_SecBuff;
        Flags         : Cardinal;
        Challenge     : TArrayOf8Bytes;
        Context       : TArrayOf8Bytes;   // reserved, not used
        TargetInfo    : TNTLM_SecBuff;
    end;

{$IFNDEF COMPILER12_UP}
    UnicodeString = WideString;
{$ENDIF}

    // interesting information from message 2
    TNTLM_Msg2_Info = record                      { V6.13 }
        SrvRespOk       : Boolean;                // server response was ok ?
        Unicode         : Boolean;
        Target          : UnicodeString;
        Domain          : UnicodeString;
        Server          : UnicodeString;
        Challenge       : TArrayOf8Bytes;
        Flags           : Cardinal;
        TargetInfoBlock : TBytes;
    end;

    // third message
    TNTLM_Message3  = packed record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        LM            : TNTLM_SecBuff;
        NTLM          : TNTLM_SecBuff;
        Domain        : TNTLM_SecBuff;
        User          : TNTLM_SecBuff;
        Host          : TNTLM_SecBuff;
        SessionKey    : TNTLM_SecBuff;
        Flags         : Cardinal;
        //OSVer         : TNTLM_OSVersion;
    end;

function NtlmGetMessage1(const AHost, ADomain: String; { V6.13 }
    ALmCompatLevel: Integer = 0): String;
function NtlmGetMessage2(const AServerReply: String): TNTLM_Msg2_Info;
function NtlmGetMessage3(const ADomain, AHost, AUser, APassword: String;
    { Param ASrcCodePage is only used with  ANSI compilers }
    AChallenge: TArrayOf8Bytes; ASrcCodePage: LongWord = CP_ACP;
    UnicodeFlag: Boolean = True): String; deprecated
  {$IFDEF COMPILER12_UP}
    'In order to support NTLMv2 call the overloaded version and also pass a LmCompatLevel to NtlmGetMessage1()'
  {$ENDIF}; overload; { V6.13 }
function NtlmGetMessage3(const ADomain, AHost, AUser, APassword: String; { V6.13 }
    const Msg2Info: TNTLM_Msg2_Info; ASrcCodePage: LongWord;
    ALmCompatLevel: Integer): String; overload;

procedure NtlmParseUserCode(const AUserCode : String; out Domain : String;
    out UserName : String; const NtlmV2: Boolean = FALSE);


implementation

{$IFDEF POSIX}
uses
  System.DateUtils;
{$ENDIF}

(*
var
  GlobalOSVer : TNTLM_OSVersion = ($05,$01,$28,$0a,$00,$00,$00,$00);

structure of "0x0501280a0000000f":
0x05	(major version 5)
0x01	(minor version 1; Windows XP)
0x280a	(build number 2600 in hexadecimal little-endian)
0x00000000	(unknown/reserved)
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure NtlmParseUserCode(                                        { V6.02 }
    const AUserCode : String;
    out Domain      : String;
    out UserName    : String;
    const NtlmV2    : Boolean = FALSE);
var
    I : Integer;
begin
  { Name Variations
    DOMAIN\user, domain.com\user, user@DOMAIN user@domain.com
    http://davenport.sourceforge.net/ntlm.html#nameVariations }

    I := Pos('\', AUserCode);
    if I > 0 then begin
        Domain   := Copy(AUserCode, 1, I - 1);
        UserName := Copy(AUserCode, I + 1, MaxInt);
    end
    else begin
        {if NtlmV2 then  V6.13
        begin
            I := Pos('@', AUserCode);
            if I > 0 then begin
                Domain   := Copy(AUserCode, I + 1, MaxInt);
                UserName := Copy(AUserCode, 1, I - 1);
            end
            else begin
                Domain   := '';
                UserName := AUserCode;
            end;
        end
        else begin }
            Domain   := '';
            UserName := AUserCode;
       // end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetCurrentTimeStamp: Int64; { V6.13 }
{$IFDEF POSIX}
var
    Bias: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetSystemTimeAsFileTime(TFileTime(Result));
{$ENDIF}
{$IFDEF POSIX}
    Bias := IcsGetLocalTimeZoneBias * SecsPerMin;
    Result := ((DateTimeToUnix(NOW) + Bias) + 11644473600) * 10000000;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ RC4 currently unused } { V6.13 }
{$IFDEF NEVER}
type
  TRC4Context = record
    D: array[Byte] of Byte;
    I, J: Byte;
  end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RC4Init(var RC4: TRC4Context; const Key: PByte; const KeyLen: Integer);
var
  R, S, T, K: Byte;
  U: Integer;
begin
  RC4.I := 0;
  RC4.J := 0;
  for S := 0 to SizeOf(Byte) do
    RC4.D[S] := S;
  R := 0;
  U := 0;
  for S := 0 to SizeOf(Byte) do
  begin
    if U < KeyLen then
      K := PByteArray(Key)^[U]
    else
      K := 0;
    Inc(U);
    if U >= KeyLen then U := 0;

    Inc(R, RC4.D[S] + K);
    T := RC4.D[S];
    RC4.D[S] := RC4.D[R];
    RC4.D[R] := T;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RC4Encode(var RC4: TRC4Context; const Source; var Dest; Count: Integer); overload;
var
  S: Integer;
  T: Byte;
begin
  for S := 0 to Count -1 do
  begin
    Inc(RC4.I);
    T := RC4.D[RC4.I];
    Inc(RC4.J, T);
    RC4.D[RC4.I] := RC4.D[RC4.J];
    RC4.D[RC4.J] := T;
    Inc(T, RC4.D[RC4.I]);
    TByteArray(Dest)[S] := TByteArray(Source)[S] xor RC4.D[T];
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RC4Encode(var RC4: TRC4Context; const AStr: RawByteString): RawByteString; overload;
var
  Len: Integer;
begin
  Len := Length(AStr);
  SetLength(Result, Len);
  RC4Encode(RC4, Pointer(AStr)^, Pointer(Result)^, Len);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
{$IFDEF SUPPORT_WIN95}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a text to a unicode text stored, this works reliable only with    }
{ 7 bit ASCII characters and mostly with windows codepage 1252 as well.     }
function UnicodeBuggy(const AData: AnsiString): AnsiString;
var
    I, J : Integer;
begin
    SetLength(Result, Length(AData) * 2);
    J := 1;
    for I := 1 to Length(AData) do begin
        Result[J] := AData[I];
        Inc(J);
        Result[J] := #0;
        Inc(J);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a text to an unicode text stored.  A.Garrels                      }
{ Min.: Windows 95, Windows NT 4.0, Windows CE 4.0. Internet Explorer 5.5.  }
function INetMultiByteToUnicode(const Src: AnsiString; ACodePage: LongWord) : AnsiString;
var
    hLib   : THandle;
    Mode   : Cardinal;
    SrcLen : Integer;
    DstLen : Integer;
    f_ConvertINetMultiByteToUnicode : function(var Mode           : Cardinal;
                                               dwSrcEncoding      : Cardinal; // Codepage
                                               lpSrcStr           : PAnsiChar;
                                               var MultiCharCount : Integer;
                                               lpDstStr           : PWideChar;
                                               var WideCharCount  : Integer
                                               ): HRESULT; stdcall;
begin
    Result := '';
    SrcLen := Length(Src);
    Mode   := 0;
    DstLen := SrcLen * (SizeOf(WideChar) + 1);  // allocate space enough
    while DstLen mod SizeOf(WideChar) > 0 do
        Inc(DstLen);
    SetLength(Result, DstLen);
    DstLen := DstLen div SizeOf(WideChar);

    hLib := LoadLibraryA('mlang.dll');
    if hlib = 0 then
        Result := UnicodeBuggy(Src)
    else
    try
        @f_ConvertINetMultiByteToUnicode := GetProcAddress(hLib,
                                              'ConvertINetMultiByteToUnicode');
        if @f_ConvertINetMultiByteToUnicode <> nil then begin
            if ACodePage = CP_ACP then // CP_ACP doesn't work here !
                ACodePage := GetACP;
            if f_ConvertINetMultiByteToUnicode(Mode, ACodePage,
                                               Pointer(Src), SrcLen,
                                               Pointer(Result), DstLen) = S_OK then
                SetLength(Result, DstLen * SizeOf(WideChar))
            else
                Result := ''; //finally call our buggy version
        end
        else
            Result := UnicodeBuggy(Src);
    finally
        if hLib > 0 then
            FreeLibrary(hLib);
    end;
end;
{$ENDIF} // SUPPORT_WIN95

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a text to an unicode text stored.  A.Garrels                      }
{ http://msdn2.microsoft.com/en-us/library/ms776413(VS.85).aspx             }
{ "Windows 95/98/Me: A version of MultiByteToWideChar is included in these  }
{ operating systems, but a more extensive version of the function is        }
{ supported by the Microsoft Layer for Unicode."                            }
function ToUnicode(const AData: AnsiString; ACodePage: LongWord): AnsiString;
var
    Len : Integer;
begin
    Len := Length(AData);
    if Len = 0 then begin
        Result := '';
        Exit;
    end;
{$IFDEF SUPPORT_WIN95}
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
        Result := INetMultiByteToUnicode(AData, ACodePage);
        Exit;
    end;
{$ELSE}
    Len := MultiByteToWideChar(ACodePage, 0, Pointer(AData),
                               Len, nil, 0);
    if Len > 0 then begin
        SetLength(Result, Len * SizeOf(WideChar));
        MultiByteToWideChar(ACodePage, 0, Pointer(AData), Length(AData),
                            Pointer(Result), Len);
    end
    else
        Result := '';
{$ENDIF}
end;
{$ENDIF} //NDEF COMPILER12_UP

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DesEcbEncrypt(
    const AKey  : AnsiString;                // Must be exactly 8 characters
    const AData : TArrayOf8Bytes): AnsiString;
var
    i, j, t, bit : Integer;
    XKey         : TArrayOf8Bytes;
begin
    XKey[0] := Byte(AKey[1]);
    XKey[1] := ((Byte(AKey[1]) shl 7) and $FF) or (Byte(AKey[2]) shr 1);
    XKey[2] := ((Byte(AKey[2]) shl 6) and $FF) or (Byte(AKey[3]) shr 2);
    XKey[3] := ((Byte(AKey[3]) shl 5) and $FF) or (Byte(AKey[4]) shr 3);
    XKey[4] := ((Byte(AKey[4]) shl 4) and $FF) or (Byte(AKey[5]) shr 4);
    XKey[5] := ((Byte(AKey[5]) shl 3) and $FF) or (Byte(AKey[6]) shr 5);
    XKey[6] := ((Byte(AKey[6]) shl 2) and $FF) or (Byte(AKey[7]) shr 6);
    XKey[7] := ((Byte(AKey[7]) shl 1) and $FF);

    for i := 0 to 7 do begin
        for j := 1 to 7 do begin
            bit := 0;
            t   := XKey[i] shl j;
            bit := (t xor bit) and 1;
        end;
        XKey[i] := Byte((XKey[i] and $FE) or bit);
    end;
    SetLength(Result, 8);
    DES(AData, PArrayOf8Bytes(@Result[1])^, XKey, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetLMHash(
    const APassword : AnsiString;
    const ANonce    : TArrayOf8Bytes): AnsiString;
const
    Magic: TArrayOf8Bytes = ($4B, $47, $53, $21, $40, $23, $24, $25);
var
    I        : Integer;
    Pass     : AnsiString;
    PassHash : AnsiString;
begin
    Pass := Copy(UpperCase(APassword), 1, 14);
    for I := Length(APassword) to 14 do
        Pass := Pass + #0;

    PassHash := DesEcbEncrypt(Copy(Pass, 1, 7), Magic) +
                DesEcbEncrypt(Copy(Pass, 8, 7), Magic) +
                #0#0#0#0#0;

    Result := DesEcbEncrypt(Copy(PassHash,  1, 7), ANonce) +
              DesEcbEncrypt(Copy(PassHash,  8, 7), ANonce) +
              DesEcbEncrypt(Copy(PassHash, 15, 7), ANonce);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetNTHash(
    const UPassword : AnsiString;
    const ANonce    : TArrayOf8Bytes): AnsiString;
var
    PassHash: AnsiString;
begin
    PassHash := MD4String(UPassword) + #0#0#0#0#0;
    Result   := DesEcbEncrypt(Copy(PassHash,  1, 7), ANonce) +
                DesEcbEncrypt(Copy(PassHash,  8, 7), ANonce) +
                DesEcbEncrypt(Copy(PassHash, 15, 7), ANonce);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage1(const AHost, ADomain: String;
  ALmCompatLevel: Integer = 0): String; { V6.13 }
var
    Msg         : TNTLM_Message1;
    Host        : AnsiString;  // Ansi even if unicode is supported by the client
    Domain      : AnsiString;  // Ansi even if unicode is supported by the client
    MessageAux  : AnsiString;
begin
    Host   := UpperCase(AHost);
    Domain := UpperCase(ADomain);

    FillChar(Msg, SizeOf(Msg), #0);
    // signature
    Move(PAnsiChar('NTLMSSP' + #0)^, Msg.Protocol, 8);

    // message type (negotiate)
    Msg.MsgType := 1;

    // prepare flags
    Msg.Flags := Flags_Negotiate_Unicode or
                 Flags_Negotiate_OEM or
                 Flags_Request_Target or
                 Flags_Negotiate_NTLM or
                 Flags_Negotiate_Allways_Sign{ or
                 Flags_Negotiate_Version};

    if ALmCompatLevel = 1 then
        Msg.Flags := Msg.Flags or Flags_Negotiate_NTLM2_Key; { V6.13 }

    // host and/or domain supplied ?
    // host
    if Length(Host) > 0 then
         Msg.Flags := Msg.Flags or Flags_Negotiate_Workstation_Supplied;

    // domain
    if Length(Domain) > 0 then
         Msg.Flags := Msg.Flags or Flags_Negotiate_Domain_Supplied;

    // host
    Msg.Host.Length := Length(Host);
    Msg.Host.Space  := Msg.Host.Length;
    Msg.Host.Offset := SizeOf(Msg); //$20 { V6.13 }

    // domain
    Msg.Domain.Length := Length(Domain);
    Msg.Domain.Space  := Msg.Domain.Length;
    Msg.Domain.Offset := Msg.Host.Offset + Msg.Host.Length;  { V6.13 }

    //Msg.OSVer := GlobalOSVer;

    SetLength(MessageAux, SizeOf(Msg));
    Move(Msg, MessageAux[1], SizeOf(Msg));
    MessageAux := MessageAux + Host + Domain;
    Result := Base64Encode(MessageAux);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage2(const AServerReply: String): TNTLM_Msg2_Info;
var
    Msg         : TNTLM_Message2;
    MsgInfo     : TNTLM_Msg2_Info;
    InfoType    : Word;
    InfoLength  : Word;
    InfoStr     : UnicodeString;
    I           : Integer;
    NTLMReply   : AnsiString;
    ReplyLen    : Integer;
    BufA        : AnsiString;
begin
    if Length(AServerReply) > 0 then begin
        // we have a response
        NTLMReply  := Base64Decode(AServerReply);
        ReplyLen := Length(NTLMReply);
        if ReplyLen < SizeOf(Msg) then
        begin
            Result.SrvRespOk := FALSE;
            Exit;
        end;
        MsgInfo.SrvRespOk := TRUE;
        Move(NTLMReply[1], Msg, SizeOf(Msg));
        MsgInfo.Flags     := Msg.Flags;    { V6.13 }
        // extract target
        MsgInfo.Unicode := (Msg.Flags and Flags_Negotiate_Unicode) <> 0;
        {MsgInfo.Target := Copy(NTLMReply, Msg.TargetName.Offset + 1,
                               Msg.TargetName.Length);} // Works in D2009+ only
        if (MsgInfo.Unicode and
            (Msg.TargetName.Length mod SizeOf(WideChar) <> 0)) or
           (Msg.TargetName.Length >= ReplyLen) then
        begin
            Result.SrvRespOk := FALSE;
            Exit;
        end;

        if MsgInfo.Unicode then begin
            SetLength(MsgInfo.Target, Msg.TargetName.Length div SizeOf(WideChar));
            Move(NTLMReply[Msg.TargetName.Offset + 1], Pointer(MsgInfo.Target)^,
                Msg.TargetName.Length);
        end
        else begin
            SetLength(BufA, Msg.TargetName.Length);
            Move(NTLMReply[Msg.TargetName.Offset + 1], Pointer(BufA)^,
                Msg.TargetName.Length);
            MsgInfo.Target := UnicodeString(BufA);
        end;
        // extract challenge
        Move(Msg.Challenge, MsgInfo.Challenge, SizeOf(Msg.Challenge));
        // let's extract the other information
        I := Msg.TargetInfo.Offset + 1;

        // TargetInfoBlock required for NTLMv2 response { V6.13 }
        if (Msg.TargetInfo.Space > 0) and
            (Integer(Msg.TargetInfo.Offset + Msg.TargetInfo.Space) <= ReplyLen) then begin
            SetLength(MsgInfo.TargetInfoBlock, Msg.TargetInfo.Space);
            Move(NTLMReply[I], MsgInfo.TargetInfoBlock[0], Msg.TargetInfo.Space);
        end
        else
            MsgInfo.TargetInfoBlock := nil;

        // loop through target information blocks
        while I < ReplyLen do begin
            // extract type
            Move(NTLMReply[I], InfoType, SizeOf(InfoType));
            I := I + SizeOf(InfoType);
            if I >= ReplyLen then begin
                MsgInfo.SrvRespOk := FALSE;
                Break;
            end;
            // extract length
            Move(NTLMReply[I], InfoLength, SizeOf(InfoLength));
            I := I + SizeOf(InfoLength);
            // terminator block ?
            if (I + InfoLength >= ReplyLen) or
               ((InfoType = 0) and (InfoLength = 0)) then
                break
            else begin
                // extract information
                //InfoStr := Copy(NTLMReply, I, InfoLength); // Works in D2009+ only!

                if MsgInfo.Unicode and
                   (InfoLength mod SizeOf(WideChar) <> 0) then begin
                    MsgInfo.SrvRespOk := FALSE;
                    Break; // Invalid Unicode
                end;
                if MsgInfo.Unicode then begin
                    SetLength(InfoStr, InfoLength div SizeOf(WideChar));
                    Move(NTLMReply[I], Pointer(InfoStr)^, InfoLength);
                    if InfoType = TIB_Type_Server then
                        MsgInfo.Server := InfoStr
                    else if InfoType = TIB_Type_Domain then
                        MsgInfo.Domain := InfoStr;
                end
                else begin
                    SetLength(BufA, InfoLength);
                    Move(NTLMReply[I], Pointer(BufA)^, InfoLength);
                    if InfoType = TIB_Type_Server then
                        MsgInfo.Server := UnicodeString(BufA)
                    else if InfoType = TIB_Type_Domain then
                        MsgInfo.Domain := UnicodeString(BufA);
                end;
                { ToDo: There may be multiple blocks/domains, we only store }
                { the last one here.                                        }
                // jump to next block
                I := I + InfoLength;
            end;
        end;
    end
    else begin
        // no response from server
        MsgInfo.SrvRespOk := FALSE;
    end;
    Result := MsgInfo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
function AnsiToOemStr(const S: AnsiString): AnsiString;
begin
    Result := S;
    if (S <> '') then
        CharToOemA(PAnsiChar(S), PAnsiChar(Result));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage3(const ADomain, AHost, AUser,
  APassword: String; AChallenge: TArrayOf8Bytes; ASrcCodePage: LongWord = CP_ACP;
  UnicodeFlag: Boolean = True): String;
var
    Msg        : TNTLM_Message3;
    MessageAux : AnsiString;
    LM_Resp    : String[30];
    NT_Resp    : String[30];
    UDomain    : AnsiString;
    UHost      : AnsiString;
    UUser      : AnsiString;
    UPassword  : AnsiString;
begin
{$IFNDEF COMPILER12_UP}
   // string is AnsiString
   if UnicodeFlag then begin
        UDomain    := ToUnicode(ADomain, ASrcCodePage);
        UHost      := ToUnicode(AHost, ASrcCodePage);
        UUser      := ToUnicode(AUser, ASrcCodePage);
        UPassword  := ToUnicode(APassword, ASrcCodePage);
    end
    else begin
        UDomain    := ADomain; // Assume ASCII
        UHost      := AHost;   // Assume ASCII
        UUser      := AnsiToOemStr(AUser);
        UPassword  := AnsiToOemStr(APassword);
    end;
{$ELSE}
    // string is UnicodeString
    if UnicodeFlag then begin
        if Length(ADomain) > 0 then begin
            SetLength(UDomain, Length(ADomain) * SizeOf(Char));
            Move(Pointer(ADomain)^, UDomain[1], Length(UDomain));
        end;
        //UDomain    := ADomain;
        if Length(AHost) > 0 then begin
            SetLength(UHost, Length(AHost) * SizeOf(Char));
            Move(Pointer(AHost)^, UHost[1], Length(UHost));
        end;
        //UHost      := AHost;
        if Length(AUser) > 0 then begin
            SetLength(UUser, Length(AUser) * SizeOf(Char));
            Move(Pointer(AUser)^, UUser[1], Length(UUser));
        end;
        //UUser      := AUser;
        if Length(APassword) > 0 then begin
            SetLength(UPassword, Length(APassword) * SizeOf(Char));
            Move(Pointer(APassword)^, UPassword[1], Length(UPassword));
        end;
    end
    else begin
        { OEM string }
        UDomain    := AnsiString(ADomain);// Assume ASCII
        UHost      := AnsiString(AHost);  // Assume ASCII
        UUser      := UnicodeToAnsi(AUser, CP_OEMCP);
        UPassword  := UnicodeToAnsi(APassword, CP_OEMCP);
    end;
{$ENDIF}
    FillChar(Msg, SizeOf(Msg), #0);
    Move(PAnsiChar('NTLMSSP' + #0)^, Msg.Protocol, 8);
    Msg.MsgType := 3;

    // prepare domain
    Msg.Domain.Length := Length(UDomain);
    Msg.Domain.Space  := Msg.Domain.Length;
    Msg.Domain.Offset := $40;

    // prepare user
    Msg.User.Length   := Length(UUser);
    Msg.User.Space    := Msg.User.Length;
    Msg.User.Offset   := Msg.Domain.Offset + Msg.Domain.Length;

    // preapre host
    Msg.Host.Length   := Length(UHost);
    Msg.Host.Space    := Msg.Host.Length;
    Msg.Host.Offset   := Msg.User.Offset + Msg.User.Length;

    // prepare LM and NTLM responses
    Msg.LM.Length     := $18;
    Msg.LM.Space      := Msg.LM.Length;
    Msg.LM.Offset     := Msg.Host.Offset + Msg.Host.Length;

    Msg.NTLM.Length   := $18;
    Msg.NTLM.Space    := Msg.NTLM.Length;  { V6.13 }
    Msg.NTLM.Offset   := Msg.LM.Offset + Msg.LM.Length;

    // no session key
    Msg.SessionKey.Length := 0;
    Msg.SessionKey.Space  := 0;
    Msg.SessionKey.Offset := 0;

    // prepare flags
    if UnicodeFlag then
        Msg.Flags := Flags_Negotiate_Unicode
    else
        Msg.Flags := Flags_Negotiate_OEM;

    Msg.Flags := Msg.Flags or
                 Flags_Request_Target or
                 Flags_Negotiate_NTLM or
                 Flags_Negotiate_Allways_Sign {or
                 Flags_Negotiate_NTLM2_Key};

    LM_Resp := NtlmGetLMHash(APassword, AChallenge);
    NT_Resp := NtlmGetNTHash(UPassword, AChallenge);

    SetLength(MessageAux, SizeOf(Msg));
    Move(Msg, MessageAux[1], SizeOf(Msg));

    MessageAux := MessageAux + UDomain + UUser + UHost + LM_Resp + NT_Resp;

    Result := Base64Encode(MessageAux);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage3(const ADomain, AHost, AUser, APassword: String; { V6.13 }
  const Msg2Info : TNTLM_Msg2_Info;
  ASrcCodePage   : LongWord;
  ALmCompatLevel : Integer): String;
var
    Msg        : TNTLM_Message3;
    MessageAux : AnsiString;
    LM_Resp    : AnsiString;//String[30];
    NT_Resp    : AnsiString;//String[30];
    UDomain    : AnsiString;
    UHost      : AnsiString;
    UUser      : AnsiString;
    UPassword  : AnsiString;
    UUserUpper : AnsiString;
{$IFDEF COMPILER12_UP}
    Temp       : UnicodeString;
{$ENDIF}
    Buf        : AnsiString;
    Digest     : TMD5Digest;
    UnicodeFlag: Boolean;

    procedure GetNtlm2SessionResponses(
        out LmResp, NtlmResp: AnsiString; NtlmOnly: Boolean);
    var
        Nonce: array[0..3] of Word;
        NtlmPwdHash   : AnsiString;
        Ntlm2SesHash  : AnsiString;
        MD5Digest     : TMD5Digest;
        MD5Context    : TMD5Context;
    begin
        Nonce[0] := IcsRandomInt(MaxWord);
        Nonce[1] := IcsRandomInt(MaxWord);
        Nonce[2] := IcsRandomInt(MaxWord);
        Nonce[3] := IcsRandomInt(MaxWord);

        { Ntlm Response }
        SetLength(Buf, 16);
        Move(Msg2Info.Challenge, Pointer(Buf)^, 8);
        Move(Nonce, Buf[9], 8);

        MD5DigestInit(MD5Digest);
        MD5Init(MD5Context);
        MD5UpdateBuffer(MD5Context, Pointer(Buf), Length(Buf));
        MD5Final(MD5Digest, MD5Context);
        SetLength(Ntlm2SesHash, 8);
        Move(MD5Digest, Pointer(Ntlm2SesHash)^, 8);

        NtlmPwdHash := MD4String(UPassword);
        SetLength(Buf, 21);
        FillChar(Pointer(Buf)^, 21, #0);
        Move(Pointer(NtlmPwdHash)^, Pointer(Buf)^, 16);
        NtlmResp :=
            DesEcbEncrypt(Copy(Buf,  1, 7), PArrayOf8Bytes(Ntlm2SesHash)^) +
            DesEcbEncrypt(Copy(Buf,  8, 7), PArrayOf8Bytes(Ntlm2SesHash)^) +
            DesEcbEncrypt(Copy(Buf, 15, 7), PArrayOf8Bytes(Ntlm2SesHash)^);
        if NtlmOnly then
            LmResp := NtlmResp
        else begin
            { LM Response }
            SetLength(LmResp, 24);
            FillChar(Pointer(LmResp)^, 24, #0);
            Move(Nonce, Pointer(LmResp)^, 8);
        end;
    end;

    function GetNtlmV2Hash(
      const AUpperUserName  : AnsiString;
      const AAuthTarget     : AnsiString): AnsiString;
    var
      NtlmPwdHash: AnsiString;
    begin
      NtlmPwdHash := MD4String(UPassword);
      Buf := AUpperUserName + AAuthTarget;
      HMAC_MD5(Pointer(Buf)^, Length(Buf), Pointer(NtlmPwdHash)^, Length(NtlmPwdHash), Digest);
      SetLength(Result, SizeOf(Digest));
      Move(Digest, Pointer(Result)^, SizeOf(Digest));
    end;

    procedure GetNtlmV2Responses(
        out LmResp, NtlmResp: AnsiString);
    type
        TBlob = packed record
          RespType    : Byte;
          RespTypeHi  : Byte;
          Reserved1   : Word;
          Reserved2   : LongWord;
          TimeStamp   : Int64;
          ClientNonce : array[0..3] of Word;
          Reserved3   : LongWord;
        end;
    var
        Buf, NtlmV2Hash : AnsiString;
        PBuf: PAnsiChar;
        Blob: TBlob;
    begin
        { NTLMv2 Response }
        NtlmV2Hash := GetNtlmV2Hash(UUserUpper, UDomain);

        Blob.RespType       := $01;
        Blob.RespTypeHi     := $01;
        Blob.Reserved1      := $0000;
        Blob.Reserved2      := $00000000;
        Blob.TimeStamp      := NtlmGetCurrentTimeStamp;
        Blob.ClientNonce[0] := IcsRandomInt(MaxWord);
        Blob.ClientNonce[1] := IcsRandomInt(MaxWord);
        Blob.ClientNonce[2] := IcsRandomInt(MaxWord);
        Blob.ClientNonce[3] := IcsRandomInt(MaxWord);
        Blob.Reserved3      := $00000000;

        SetLength(Buf, SizeOf(Blob) + Length(Msg2Info.TargetInfoBlock) + 4);
        FillChar(Pointer(Buf)^, Length(Buf), #0);
        PBuf := PAnsiChar(Buf);
        Move(Blob, PBuf^, SizeOf(Blob));
        Inc(PBuf, SizeOf(Blob));
        Move(Msg2Info.TargetInfoBlock[0], PBuf^, Length(Msg2Info.TargetInfoBlock));
        {/blob}

        SetLength(NtlmResp, SizeOf(Msg2Info.Challenge));
        Move(Msg2Info.Challenge, Pointer(NtlmResp)^, SizeOf(Msg2Info.Challenge));
        NtlmResp := NtlmResp + Buf;
        HMAC_MD5(Pointer(NtlmResp)^, Length(NtlmResp), Pointer(NtlmV2Hash)^,
                 Length(NtlmV2Hash), Digest);
        SetLength(NtlmResp, SizeOf(Digest));
        Move(Digest, Pointer(NtlmResp)^, SizeOf(Digest));
        NtlmResp := NtlmResp + Buf;

        { LMv2 Response }
        SetLength(LmResp, 16);
        Move(Msg2Info.Challenge, Pointer(LmResp)^, SizeOf(Msg2Info.Challenge));
        Move(Blob.ClientNonce, LmResp[9], 8);
        HMAC_MD5(Pointer(LmResp)^, 16, Pointer(NtlmV2Hash)^, Length(NtlmV2Hash),
                 Digest);
        SetLength(LmResp, 24);
        Move(Digest, Pointer(LmResp)^, SizeOf(Digest));
        Move(Blob.ClientNonce, LmResp[17], 8);
    end;

begin
    UnicodeFlag := (Msg2Info.Flags and Flags_Negotiate_Unicode) <> 0;
{$IFNDEF COMPILER12_UP}
   // string is AnsiString
    if UnicodeFlag then begin
        UDomain    := ToUnicode(ADomain, ASrcCodePage);
        UHost      := ToUnicode(AHost, ASrcCodePage);
        UUser      := ToUnicode(AUser, ASrcCodePage);
        UPassword  := ToUnicode(APassword, ASrcCodePage);
    end
    else begin
        UDomain    := ADomain; // Assume ASCII
        UHost      := AHost;   // Assume ASCII
        UUser      := AnsiToOemStr(AUser);
        UPassword  := AnsiToOemStr(APassword);
    end;
    UUserUpper := ToUnicode(AnsiUpperCase(AUser), ASrcCodePage);
{$ELSE}
    // string is UnicodeString
    if UnicodeFlag then begin
        if Length(ADomain) > 0 then begin
            SetLength(UDomain, Length(ADomain) * SizeOf(Char));
            Move(Pointer(ADomain)^, UDomain[1], Length(UDomain));
        end;
        //UDomain    := ADomain;
        if Length(AHost) > 0 then begin
            SetLength(UHost, Length(AHost) * SizeOf(Char));
            Move(Pointer(AHost)^, UHost[1], Length(UHost));
        end;
        //UHost      := AHost;
        if Length(AUser) > 0 then begin
            SetLength(UUser, Length(AUser) * SizeOf(Char));
            Move(Pointer(AUser)^, UUser[1], Length(UUser));
        end;
        //UUser      := AUser;
        if Length(APassword) > 0 then begin
            SetLength(UPassword, Length(APassword) * SizeOf(Char));
            Move(Pointer(APassword)^, UPassword[1], Length(UPassword));
        end;
    end
    else begin
        { OEM string }
        UDomain    := AnsiString(ADomain);// Assume ASCII
        UHost      := AnsiString(AHost);  // Assume ASCII
        UUser      := UnicodeToAnsi(AUser, CP_OEMCP);
        UPassword  := UnicodeToAnsi(APassword, CP_OEMCP);
    end;
    Temp := AnsiUpperCase(AUser);
    SetLength(UUserUpper, Length(Temp) * SizeOf(Char));
    Move(Pointer(Temp)^, UUserUpper[1], Length(UUserUpper));
{$ENDIF}
    FillChar(Msg, SizeOf(Msg), #0);
    Move(PAnsiChar('NTLMSSP' + #0)^, Msg.Protocol, 8);
    Msg.MsgType := 3;

    // prepare domain
    Msg.Domain.Length := Length(UDomain);
    Msg.Domain.Space  := Msg.Domain.Length;
    Msg.Domain.Offset := SizeOf(TNTLM_Message3);

    // prepare user
    Msg.User.Length   := Length(UUser);
    Msg.User.Space    := Msg.User.Length;
    Msg.User.Offset   := Msg.Domain.Offset + Msg.Domain.Length;

    // preapre host
    Msg.Host.Length   := Length(UHost);
    Msg.Host.Space    := Msg.Host.Length;
    Msg.Host.Offset   := Msg.User.Offset + Msg.User.Length;

    //Msg.OSVer         := GlobalOSVer;

    // prepare flags
    if UnicodeFlag then
        Msg.Flags := Flags_Negotiate_Unicode
    else
        Msg.Flags := Flags_Negotiate_OEM;
    Msg.Flags := Msg.Flags or
                 Flags_Request_Target or
                 Flags_Negotiate_NTLM or
                 Flags_Negotiate_Allways_Sign{ or
                 Flags_Negotiate_Version};

    if (ALmCompatLevel = 0) then // backward compatible  NTLMv1
    begin
        LM_Resp := NtlmGetLMHash(APassword, Msg2Info.Challenge); // Convert PWD to Ansi/OEM
        NT_Resp := NtlmGetNTHash(UPassword, Msg2Info.Challenge);
    end
    else if (ALmCompatLevel in [1..2]) then
    begin
        if (Msg2Info.Flags and Flags_Negotiate_NTLM2_Key <> 0) then
        begin
            Msg.Flags := Msg.Flags or Flags_Negotiate_NTLM2_Key;
            GetNtlm2SessionResponses(LM_Resp, NT_Resp, False);
        end
        else begin
            NT_Resp := NtlmGetNTHash(UPassword, Msg2Info.Challenge);
            if (ALmCompatLevel = 2) then
                LM_Resp := NT_Resp
            else
                LM_Resp := NtlmGetLMHash(APassword, Msg2Info.Challenge);
        end;
    end
    else begin // Level > 2 NTLMv2 responses
        GetNtlmV2Responses(LM_Resp, NT_Resp);
    end;

    // LM and NTLM security buffers
    Msg.LM.Length     := Length(LM_Resp);
    Msg.LM.Space      := Msg.LM.Length;
    Msg.LM.Offset     := Msg.Host.Offset + Msg.Host.Length;

    Msg.NTLM.Length   := Length(NT_Resp);
    Msg.NTLM.Space    := Msg.NTLM.Length;
    Msg.NTLM.Offset   := Msg.LM.Offset + Msg.LM.Length;

    // no session key
    Msg.SessionKey.Length := 0;
    Msg.SessionKey.Space  := 0;
    Msg.SessionKey.Offset := Msg.NTLM.Offset + Msg.NTLM.Length;

    SetLength(MessageAux, SizeOf(Msg));
    Move(Msg, Pointer(MessageAux)^, SizeOf(Msg));

    { Concatenate final message }
    MessageAux := MessageAux + UDomain + UUser + UHost + LM_Resp + NT_Resp;

    Result := Base64Encode(MessageAux);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
