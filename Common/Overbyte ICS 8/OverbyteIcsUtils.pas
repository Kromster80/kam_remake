{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  A place for common utilities.
Creation:     Apr 25, 2008
Version:      8.07
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2013 by François PIETTE
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
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
Apr 25, 2008 V1.00 AGarrels added first functions UnicodeToAscii, UnicodeToAnsi,
             AnsiToUnicode and IsUsAscii .
May 01, 2008 V1.01 AGarrels added StreamWriteString.
May 02, 2008 V1.02 AGarrels a few optimizations and a bugfix in StreamWriteString.
May 11, 2008 V1.03 USchuster added atoi implementations (moved from several units)
May 15, 2008 V1.04 AGarrels fix in IcsAppendStr made StreamWriteString a function.
May 19, 2008 V1.05 AGarrels added BOM-support to StreamWriteString plus two
             overloads. Made UnicodeString a type alias of WideString in compiler
             versions < COMPILER12 in order to enable use of some conversion
             routines for older compilers as well.
May 19, 2008 V1.06 Don't check actual string codepage but assume UTF-16 Le
             in function StreamWriteString() (temp fix).
Jul 14, 2008 V1.07 atoi improved, should be around 3 times faster.
Jul 17, 2008 V1.08 Added OverbyteIcsTypes to the uses clause and removed
             SysUtils, removed some defines for unsupported old compilers.
             StreamWriteString should work with WideStrings as well with old
             compilers.
Jul 20, 2008 V1.09 Added Utf-8 string functions.
Jul 29, 2008 V1.10 Added parameter "SetCodePage" to UnicodeToAnsi(), defaults
             to "False". Utf-8 functions adjusted accordingly. Does effect
             compiler post RDS2007 only.
Jun 05, 2008 Utf-8 functions modified to take and return AnsiString rather than
             UTF8String.
Aug 11, 2008 CheckUnicodeToAnsi() added. Changed the DefaultFailChar to "?".
Aug 23, 2008 Utf-8 functions modified RawByteString rather than AnsiString.
Aug 27, 2008 Arno Garrels added WideString functions and other stuff.
Sep 11, 2008 Angus added more widestring functions
             No range checking so they all work (IcsFileGetAttrW in particular)
Sep 20, 2008 V1.16 Angus still adding WideString functions
Sep 21, 2008 V1.17 Link RtlCompareUnicodeString() dynamically at run-time
Sep 27, 2008 V1.18 Arno fixed a bug in StringToUtf8.
Sep 28, 2008 V1.19 A. Garrels Moved IsDigit, IsXDigit, XDigit, htoi2 and htoin
             from OverbyteIcsUrl and added overloads. Fixed a bug in
             ConvertCodepage().
Oct 03, 2008 V1.20 A. Garrels moved some double helper functions to this unit.
             Added symbol USE_INLINE that enables inlining.
Oct 23, 2008 V7.21 A. Garrels added IcsStrNextChar, IcsStrPrevChar and
             IcsStrCharLength, see description below. Useful when converting
             a ANSI character stream with known code page to Unicode in
             chunks. Added a PAnsiChar overload to function AnsiToUnicode.
Nov 13, 2008 v7.22 Arno added CharsetDetect, IsUtf8Valid use CharsetDetect.
Dec 05, 2008 v7.23 Arno added function IcsCalcTickDiff.
Apr 18, 2009 V7.24 Arno added a PWideChar overload to UnicodeToAnsi().
May 02, 2009 V7.25 Arno added IcsNextCharIndex().
May 03, 2009 V7.26 Arno added IsUtf8TrailByte and IsLeadChar.
May 14, 2009 V7.27 Arno changed IcsNextCharIndex() to avoid a compiler
             warning in C++ Builder (assertion moved one line up).
             Removed uneccessary overload directives from IcsCharNextUtf8
             and IcsCharPrevUtf8.
May 17, 2009 V7.28 Arno prefixed argument names of various UTF-8 overloads
             by "Utf8" so that C++Builder user know that UTF-8 encoded
             AnsiStrings are expected.
June 4, 2009 V7.29 Angus added IcsExtractLastDir
Jun 22, 2009 V7.30 Angus avoid D2009 error with IcsExtractLastDir
Sep 24, 2009 V7.31 Arno added TIcsIntegerList and IcsBufferToHex.
             Small fix in ConvertCodepage(). Added check for nil in
             IcsCharNextUtf8(). Added global consts CP_UTF16, CP_UTF16Be,
             CP_UTF32 and CP_UTF32Be. New functions IcsBufferToUnicode,
             IcsGetWideCharCount and IcsGetWideChars see comments in interface
             section. Added fast functions to swap byte order: IcsSwap16,
             IcsSwap16Buf, IcsSwap32, IcsSwap32Buf and IcsSwap64Buf.
Dec 15, 2009 V7.32 Arno added typedef PInt64 for CB 2006 and CB2007.
Mar 06, 2010 V7.33 Arno fixed IcsGetWideCharCount, MultiByteToWideChar() does
             not support flag "MB_ERR_INVALID_CHARS" with all code pages.
             Fixed some ugly bugs in UTF-8 helper functions too. Added
             IsUtf8LeadByte() and IcsUtf8Size().
Apr 26, 2010 V7.34 Arno removed some Windows dependencies. Charset conversion
             functions optionally may use GNU iconv library (LGPL) by explicitly
             defining conditional "USE_ICONV".
May 07, 2010 V7.35 Arno added IcsIsSBCSCodepage.
Aug 21, 2010 V7.36 Arno fixed a bug in the UTF-8 constructor of TIcsFileStreamW.
Sep 05, 2010 V7.37 Arno added procedure IcsNameThreadForDebugging
Apr 15, 2011 V7.38 Arno prepared for 64-bit.
May 06, 2011 V7.39 Arno moved TThreadID to OverbyteIcsTypes.
Jun 08, 2011 v7.40 Arno added x64 assembler routines, untested so far.
Jun 14, 2011 v7.41 aguser added Unicode Normalization as IcsNormalizeString()
             see http://www.unicode.org/reports/tr15/tr15-33.html.
Aug 14, 2011 v7.42 Arno fixed IcsSwap64 BASM 32-bit (not yet used in ICS)
Feb 08, 2012 v7.43 Arno - The IcsFileCreateW and IcsFileOpenW functions return a
             THandle in XE2+ now. Same as SysUtils.FileCreate and SysUtils.FileOpen
             in XE2+.
Feb 29, 2012 V7.44 Arno added IcsRandomInt() and IcsCryptGenRandom(), see
             comments at IcsRandomInt's implementation.
Apr 27, 2012 V7.45 Arno added IcsFileUtcModified().
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 06, 2012 v8.01 Arno simplified TIcsIntegerList.IndexOf().
Nov 10, 2012 v8.02 Bugfix IcsCompareTextA IcsCompareStrA
Apr 25, 2013 V8.03 Arno minor XE4 changes. Added IcsStrLen(), IcsStrPas()
                  IcsStrCopy().
Mai 03, 2013 V8.04 Compile some overloaded versions of new functions from V8.03
             in Delphi 2009+ only.
Jul 06, 2013 V8.05 FPiette fixed confitional compilation for IcsStrPCopy so
             that it compiles with Delphi7.
Jul 06, 2013 V8.06 Arno reverted the conditional define from previous fix and
             fixed IcsStrPCopy instead.
Jul 13, 2013 V8.07 Arno added an overloaded version of IcsGetBufferCodepage that
             returns BOM's size.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsUtils;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$R-}           { no range checking, otherwise DWORD=Integer fails with some Windows APIs }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF CPUX64}
  {.$DEFINE PUREPASCAL}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    OverbyteIcsWinnls,
  {$IFDEF USE_ICONV}
    OverbyteIcsIconv,
  {$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
    Posix.SysTypes, Posix.Iconv, Posix.Errno,
    Posix.Unistd, Posix.Stdio, Posix.SysStatvfs,
    Posix.PThread, Posix.Time,
    Ics.Posix.WinTypes,
  {$IFDEF MACOS}
    Macapi.CoreFoundation,
    MacApi.CoreServices,
  {$ENDIF}
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.RtlConsts{$ELSE}RtlConsts{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
{$IFDEF COMPILER16_UP}
    System.SyncObjs,
{$ENDIF}
{$IFDEF COMPILER18_UP}
    {$IFDEF RTL_NAMESPACES}System.AnsiStrings{$ELSE}AnsiStrings{$ENDIF},
{$ENDIF}
    OverbyteIcsMD5, OverbyteIcsTypes; // for TBytes and TThreadID

type
{$IFNDEF COMPILER12_UP}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef __int64* PInt64;' *)
  (*$HPPEMIT '}' *)
{$ENDIF}

{$IFNDEF COMPILER15_UP}
    PLongBool     =  ^LongBool;
{$ENDIF}
    TIcsDbcsLeadBytes = TSysCharset;
    
const
    { From Win 7 GetCPInfoEx() DBCS lead bytes }
    ICS_LEAD_BYTES_932   : TIcsDbcsLeadBytes = [#$81..#$9F, #$E0..#$FC];              // (ANSI/OEM - Japanese Shift-JIS) DBCS Lead Bytes: 81..9F E0..FC
    ICS_LEAD_BYTES_936_949_950 : TIcsDbcsLeadBytes = [#$81..#$FE];                    // (ANSI/OEM - Simplified Chinese GBK) DBCS Lead Bytes: 81..FE
    //ICS_LEAD_BYTES_949   = LEAD_BYTES_936;                                          // (ANSI/OEM - Korean) DBCS Lead Bytes: 81..FE
    //ICS_LEAD_BYTES_950   = LEAD_BYTES_936;                                          // (ANSI/OEM - Traditional Chinese Big5) DBCS Lead Bytes: 81..FE
    ICS_LEAD_BYTES_1361  : TIcsDbcsLeadBytes = [#$84..#$D3, #$D8..#$DE, #$E0..#$F9];  // (Korean - Johab) DBCS Lead Bytes: 84..D3 D8..DE E0..F9
    ICS_LEAD_BYTES_10001 : TIcsDbcsLeadBytes = [#$81..#$9F, #$E0..#$FC];              // (MAC - Japanese) DBCS Lead Bytes: 81..9F E0..FC
    ICS_LEAD_BYTES_10002 : TIcsDbcsLeadBytes = [#$81..#$FC];                          // (MAC - Traditional Chinese Big5) DBCS Lead Bytes: 81..FC
    ICS_LEAD_BYTES_10003 : TIcsDbcsLeadBytes = [#$A1..#$AC, #$B0..#$C8, #$CA..#$FD];  // (MAC - Korean) DBCS Lead Bytes: A1..AC B0..C8 CA..FD
    ICS_LEAD_BYTES_10008 : TIcsDbcsLeadBytes = [#$A1..#$A9, #$B0..#$F7];              // (MAC - Simplified Chinese GB 2312) DBCS Lead Bytes: A1..A9 B0..F7
    ICS_LEAD_BYTES_20000 : TIcsDbcsLeadBytes = [#$A1..#$FE];                          // (CNS - Taiwan) DBCS Lead Bytes: A1..FE
    ICS_LEAD_BYTES_20001 : TIcsDbcsLeadBytes = [#$81..#$84, #$91..#$D8, #$DF..#$FC];  // (TCA - Taiwan) DBCS Lead Bytes: 81..84 91..D8 DF..FC
    ICS_LEAD_BYTES_20002 : TIcsDbcsLeadBytes = [#$81..#$AF, #$DD..#$FE];              // (Eten - Taiwan) DBCS Lead Bytes: 81..AF DD..FE
    ICS_LEAD_BYTES_20003 : TIcsDbcsLeadBytes = [#$81..#$84, #$87..#$87, #$89..#$E8, #$F9..#$FB]; // (IBM5550 - Taiwan) DBCS Lead Bytes: 81..84 87..87 89..E8 F9..FB
    ICS_LEAD_BYTES_20004 : TIcsDbcsLeadBytes = [#$A1..#$FE];                          // (TeleText - Taiwan) DBCS Lead Bytes: A1..FE
    ICS_LEAD_BYTES_20005 : TIcsDbcsLeadBytes = [#$8D..#$F5, #$F9..#$FC];              // (Wang - Taiwan) DBCS Lead Bytes: 8D..F5 F9..FC
    ICS_LEAD_BYTES_20261 : TIcsDbcsLeadBytes = [#$C1..#$CF];                          // (T.61) DBCS Lead Bytes: C1..CF
    ICS_LEAD_BYTES_20932 : TIcsDbcsLeadBytes = [#$8E..#$8E, #$A1..#$FE];              // (JIS X 0208-1990 & 0212-1990) DBCS Lead Bytes: 8E..8E A1..FE
    ICS_LEAD_BYTES_20936 : TIcsDbcsLeadBytes = [#$A1..#$A9, #$B0..#$F7];              // (Simplified Chinese GB2312) DBCS Lead Bytes: A1..A9 B0..F7
    ICS_LEAD_BYTES_51949 : TIcsDbcsLeadBytes = [#$A1..#$AC, #$B0..#$C8, #$CA..#$FD];  // (EUC-Korean) DBCS Lead Bytes: A1..AC B0..C8 CA..FD

{$IFDEF MSWINDOWS}
  {$IFNDEF COMPILER12_UP}
    {$EXTERNALSYM MB_ERR_INVALID_CHARS}
    MB_ERR_INVALID_CHARS            = $00000008;  // Missing in Windows.pas
    {$IFDEF COMPILER11_UP} {$EXTERNALSYM WC_ERR_INVALID_CHARS} {$ENDIF}
    WC_ERR_INVALID_CHARS            = $80;        // Missing in Windows.pas
  {$ENDIF}
{$ENDIF}
    { Unicode code page ID }
    CP_UTF16      = 1200;
    CP_UTF16Be    = 1201;
    CP_UTF32      = 12000;
    CP_UTF32Be    = 12001;

type
    EIcsStringConvertError = class(Exception);
    TCharsetDetectResult = (cdrAscii, cdrUtf8, cdrUnknown);

    TIcsNormForm = (
      icsNormalizationOther,
      icsNormalizationC,
      icsNormalizationD,
      icsNormalizationKC = 5,
      icsNormalizationKD);

{$IFDEF COMPILER12_UP}
    TIcsSearchRecW = {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.TSearchRec;
{$ELSE}
    TIcsSearchRecW = record
        Time        : Integer;
        Size        : Integer;
        Attr        : Integer;
        Name        : UnicodeString;
        ExcludeAttr : Integer;
        FindHandle  : THandle;
        FindData    : TWin32FindDataW;
    end;
{$ENDIF}

{$IFDEF MSWINDOWS}
    TUnicode_String = record
        Length        : Word;
        MaximumLength : Word;
        Buffer        : PWideChar;
    end;
    PUnicode_String = ^TUnicode_String;

    TRtlCompareUnicodeString = function(String1, String2: PUnicode_String; CaseInSensitive: Boolean): LongInt; stdcall;
{$ENDIF}

{$IFNDEF COMPILER12_UP}
    TIcsFileStreamW = class(THandleStream)
{$ELSE}
    TIcsFileStreamW = class(TFileStream)
{$ENDIF}
    private
        FFileName: UnicodeString;
    public
        constructor Create(const FileName: UnicodeString; Mode: Word); overload;
        constructor Create(const FileName: UnicodeString; Mode: Word; Rights: Cardinal); overload;
        constructor Create(const Utf8FileName: UTF8String; Mode: Word); overload;
        constructor Create(const Utf8FileName: UTF8String; Mode: Word; Rights: Cardinal); overload;
        destructor  Destroy; override;
        property    FileName: UnicodeString read FFileName;
    end;
{$IFDEF USE_ICONV}
const
    ICONV_UNICODE     = 'UTF-16LE';
    function IcsIconvNameFromCodePage(CodePage: LongWord): AnsiString;
{$ENDIF}
    function  IcsIsValidAnsiCodePage(const CP: LongWord): Boolean;
    procedure IcsCharLowerA(var ACh: AnsiChar); {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsGetCurrentThreadID: TThreadID;
    function  IcsGetFreeDiskSpace(const APath: String): Int64;
    function  IcsGetLocalTimeZoneBias: LongInt;
    function  IcsDateTimeToUTC (dtDT: TDateTime): TDateTime;
    function  IcsUTCToDateTime (dtDT: TDateTime): TDateTime;
    function  IcsGetTickCount: LongWord;
    function  IcsWcToMb(CodePage: LongWord; Flags: Cardinal;
                        WStr: PWideChar; WStrLen: Integer; MbStr: PAnsiChar;
                        MbStrLen: Integer; DefaultChar: PAnsiChar;
                        UsedDefaultChar: PLongBool): Integer;
    function  IcsMbToWc(CodePage: LongWord; Flags: Cardinal;
                        MbStr: PAnsiChar; MbStrLen: Integer; WStr: PWideChar;
                        WStrLen: Integer): Integer;
    function  IcsGetDefaultWindowsUnicodeChar(CodePage: LongWord): WideChar; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsGetDefaultWindowsAnsiChar(CodePage: LongWord): AnsiChar; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure IcsGetAcp(var CodePage: LongWord);
    function  IcsIsDBCSCodePage(CodePage: LongWord): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsIsDBCSLeadByte(Ch: AnsiChar; CodePage: LongWord): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsIsMBCSCodePage(CodePage: LongWord): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsIsSBCSCodePage(CodePage: LongWord): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsGetLeadBytes(CodePage: LongWord): TIcsDbcsLeadBytes; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  UnicodeToUsAscii(const Str: UnicodeString; FailCh: AnsiChar): AnsiString; overload;
    function  UnicodeToUsAscii(const Str: UnicodeString): AnsiString; overload;
    function  UsAsciiToUnicode(const Str: RawByteString; FailCh: AnsiChar): UnicodeString; overload;
    function  UsAsciiToUnicode(const Str: RawByteString): UnicodeString; overload;
    function  UnicodeToAnsi(const Str: PWideChar; ACodePage: LongWord; SetCodePage: Boolean = False): RawByteString; overload;
    function  UnicodeToAnsi(const Str: UnicodeString; ACodePage: LongWord; SetCodePage: Boolean = False): RawByteString; overload;
    function  UnicodeToAnsi(const Str: UnicodeString): RawByteString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  AnsiToUnicode(const Str: PAnsiChar; ACodePage: LongWord): UnicodeString; overload;
    function  AnsiToUnicode(const Str: RawByteString; ACodePage: LongWord): UnicodeString; overload;
    function  AnsiToUnicode(const Str: RawByteString): UnicodeString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    { Returns a UnicodeString and the number of not translated bytes at the end of the source buffer }
    { BufferCodePage includes Ansi as well as Unicode code page IDs }
    function  IcsBufferToUnicode(const Buffer; BufferSize: Integer; BufferCodePage: LongWord; out FailedByteCount: Integer): UnicodeString; overload;
    { Returns a UnicodeString and optionally raises an exception if there are any number of not translated bytes at the end of the source buffer }
    { BufferCodePage includes Ansi as well as Unicode code page IDs }
    function  IcsBufferToUnicode(const Buffer; BufferSize: Integer; BufferCodePage: LongWord; RaiseFailedBytes: Boolean = FALSE): UnicodeString; overload;
    { Returns the number of WideChars, and the number of not translated bytes at the end of the source buffer }
    { BufferCodePage includes Ansi as well as Unicode code page IDs }
    function  IcsGetWideCharCount(const Buffer; BufferSize: Integer; BufferCodePage: LongWord; out InvalidEndByteCount: Integer): Integer;
    { Returns a Unicode string, ByteCount and CharCount must match, no length checks are done }
    { BufferCodePage includes Ansi as well as Unicode code page IDs }
    function  IcsGetWideChars(const Buffer; BufferSize: Integer; BufferCodePage: LongWord; Chars: PWideChar; CharCount: Integer): Integer;
    function  StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer; ACodePage: LongWord; WriteBOM: Boolean): Integer; overload;
    function  StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer; ACodePage: LongWord): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  StreamWriteString(AStream: TStream; const Str: UnicodeString; ACodePage: LongWord; WriteBOM: Boolean): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  StreamWriteString(AStream: TStream; const Str: UnicodeString; ACodePage: LongWord): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  StreamWriteString(AStream: TStream; const Str: UnicodeString): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsUsAscii(const Str: RawByteString): Boolean; overload;
    function  IsUsAscii(const Str: UnicodeString): Boolean; overload;
    procedure IcsAppendStr(var Dest: RawByteString; const Src: RawByteString);
    function  atoi(const Str: RawByteString): Integer; overload;
    function  atoi(const Str: UnicodeString): Integer; overload;
{$IFDEF STREAM64}
    function  atoi64(const Str: RawByteString): Int64; overload;
    function  atoi64(const Str: UnicodeString): Int64; overload;
{$ENDIF}
    function  IcsCalcTickDiff(const StartTick, EndTick: LongWord): LongWord; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  StringToUtf8(const Str: UnicodeString): RawByteString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  StringToUtf8(const Str: RawByteString; ACodePage: LongWord = CP_ACP): RawByteString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  Utf8ToStringW(const Str: RawByteString): UnicodeString; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  Utf8ToStringA(const Str: RawByteString; ACodePage: LongWord = CP_ACP): AnsiString; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  CheckUnicodeToAnsi(const Str: UnicodeString; ACodePage: LongWord = CP_ACP): Boolean;
    { This is a weak check, it does not detect whether it's a valid UTF-8 byte }  
    function  IsUtf8TrailByte(const B: Byte): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IsUtf8LeadByte(const B: Byte): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsUtf8Size(const LeadByte: Byte): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$IFNDEF COMPILER12_UP}
    function  IsLeadChar(Ch: WideChar): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
    function  IsUtf8Valid(const Str: RawByteString): Boolean; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IsUtf8Valid(const Buf: Pointer; Len: Integer): Boolean; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  CharsetDetect(const Buf: Pointer; Len: Integer): TCharsetDetectResult; overload;
    function  CharsetDetect(const Str: RawByteString): TCharsetDetectResult; overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsCharNextUtf8(const Str: PAnsiChar): PAnsiChar; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  IcsCharPrevUtf8(const Start, Current: PAnsiChar): PAnsiChar; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  ConvertCodepage(const Str: RawByteString; SrcCodePage: LongWord; DstCodePage: LongWord = CP_ACP): RawByteString;
    function  htoin(Value : PWideChar; Len : Integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  htoin(Value : PAnsiChar; Len : Integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  htoi2(value : PWideChar): Integer; overload;
    function  htoi2(value : PAnsiChar): Integer; overload;
    function  IcsBufferToHex(const Buf; Size: Integer): String; overload;
    function  IcsBufferToHex(const Buf; Size: Integer; Separator: Char): String; overload;
    function  IsXDigit(Ch : WideChar): Boolean; overload;
    function  IsXDigit(Ch : AnsiChar): Boolean; overload;
    function  XDigit(Ch : WideChar): Integer; overload;
    function  XDigit(Ch : AnsiChar): Integer; overload;
    function  IsCharInSysCharSet(Ch : WideChar; const ASet : TSysCharSet) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsCharInSysCharSet(Ch : AnsiChar; const ASet : TSysCharSet) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsDigit(Ch : WideChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsDigit(Ch : AnsiChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsSpace(Ch : WideChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsSpace(Ch : AnsiChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsCRLF(Ch : WideChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsCRLF(Ch : AnsiChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsSpaceOrCRLF(Ch : WideChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IsSpaceOrCRLF(Ch : AnsiChar) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  XDigit2(S : PChar) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function  stpblk(PValue : PWideChar) : PWideChar; overload;
    function  stpblk(PValue : PAnsiChar) : PAnsiChar; overload;
    { Retrieves the pointer to the next character in a string. This function }
    { can handle strings consisting of either single- or multi-byte          }
    { characters. including UTF-8. The return value is a pointer to the next }
    { character in the string, or to the terminating null character if at    }
    { the end of the string.                                                 }
    function  IcsStrNextChar(const Str: PAnsiChar; ACodePage: LongWord = CP_ACP): PAnsiChar;
    { Retrieves the pointer to the preceding character in a string. This     }
    { function can handle strings consisting of either single- or multi-byte }
    { characters including UTF-8. The return value is a pointer to the       }
    { preceding character in the string, or to the first character in the    }
    { string if the Current parameter equals the Start parameter.            }
    function  IcsStrPrevChar(const Start, Current: PAnsiChar; ACodePage: LongWord = CP_ACP): PAnsiChar;
    function  IcsStrCharLength(const Str: PAnsiChar; ACodePage: LongWord = CP_ACP): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IcsNextCharIndex(const S: RawByteString; Index: Integer; ACodePage: LongWord = CP_ACP): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function  IcsGetBomBytes(ACodePage: LongWord): TBytes;
    function  IcsGetBufferCodepage(Buf: PAnsiChar; ByteCount: Integer): LongWord; overload;
    function  IcsGetBufferCodepage(Buf: PAnsiChar; ByteCount: Integer; out BOMSize: Integer): LongWord; overload;  { V8.07 }
    function  IcsSwap16(Value: Word): Word;
    procedure IcsSwap16Buf(Src, Dst: PWord; WordCount: Integer);
    function  IcsSwap32(Value: LongWord): LongWord;
    procedure IcsSwap32Buf(Src, Dst: PLongWord; LongWordCount: Integer);
    function  IcsSwap64(Value: Int64): Int64;
    procedure IcsSwap64Buf(Src, Dst: PInt64; QuadWordCount: Integer);
    procedure IcsNameThreadForDebugging(AThreadName: AnsiString; AThreadID: TThreadID = TThreadID(-1));
    function  IcsNormalizeString(const S: UnicodeString; NormForm: TIcsNormForm): UnicodeString;
    function IcsCryptGenRandom(var Buf; BufSize: Integer): Boolean;
    function IcsRandomInt(const ARange: Integer): Integer;
    function IcsFileUtcModified(const FileName: String) : TDateTime;
    function IcsInterlockedCompareExchange(var Destination: Pointer;
        Exchange: Pointer; Comperand: Pointer): Pointer;
{ Wide library }
    function IcsFileCreateW(const FileName: UnicodeString): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; overload;
    function IcsFileCreateW(const Utf8FileName: UTF8String): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFileCreateW(const FileName: UnicodeString; Rights: LongWord): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; overload;
    function IcsFileCreateW(const Utf8FileName: UTF8String; Rights: LongWord): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFileOpenW(const FileName: UnicodeString; Mode: LongWord): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; overload;
    function IcsFileOpenW(const Utf8FileName: UTF8String; Mode: LongWord): {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF}; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsStrScanW(const Str: PWideChar; Ch: WideChar): PWideChar;
    function IcsExtractFilePathW(const FileName: UnicodeString): UnicodeString;
    function IcsExtractFileDirW(const FileName: UnicodeString): UnicodeString;
    function IcsExtractFileDriveW(const FileName: UnicodeString): UnicodeString;
    function IcsExtractFileNameW(const FileName: UnicodeString): UnicodeString;
    function IcsExtractFileExtW(const FileName: UnicodeString): UnicodeString;
    function IcsExpandFileNameW(const FileName: UnicodeString): UnicodeString;
    function IcsExtractNameOnlyW(FileName: UnicodeString): UnicodeString; // angus
    function IcsChangeFileExtW(const FileName, Extension: UnicodeString): UnicodeString;  // angus
    function IcsStrAllocW(Len: Cardinal): PWideChar;
    function IcsStrLenW(Str: PWideChar): Cardinal;
    function IcsAnsiCompareFileNameW(const S1, S2: UnicodeString): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsAnsiCompareFileNameW(const Utf8S1, Utf8S2: UTF8String): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsDirExistsW(const FileName: PWideChar): Boolean; overload;
    function IcsDirExistsW(const FileName: UnicodeString): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsDirExistsW(const Utf8FileName: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFindFirstW(const Path: UnicodeString; Attr: Integer; var F: TIcsSearchRecW): Integer; overload;
    function IcsFindFirstW(const Utf8Path: UTF8String; Attr: Integer; var F: TIcsSearchRecW): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    procedure IcsFindCloseW(var F: TIcsSearchRecW);
    function IcsFindNextW(var F: TIcsSearchRecW): Integer;
    function IcsIncludeTrailingPathDelimiterW(const S: UnicodeString): UnicodeString;
    function IcsExcludeTrailingPathDelimiterW(const S: UnicodeString): UnicodeString;
    function IcsExtractLastDir (const Path: RawByteString): RawByteString ; overload;   // angus
    function IcsExtractLastDir (const Path: UnicodeString): UnicodeString ; overload;   // angus
{$IFDEF MSWINDOWS}
    function IcsFileGetAttrW(const FileName: UnicodeString): Integer; overload;
    function IcsFileGetAttrW(const Utf8FileName: UTF8String): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFileSetAttrW(const FileName: UnicodeString; Attr: Integer): Integer; overload;
    function IcsFileSetAttrW(const Utf8FileName: UTF8String; Attr: Integer): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
{$ENDIF}
    function IcsDeleteFileW(const FileName: UnicodeString): Boolean; overload;
    function IcsDeleteFileW(const Utf8FileName: UTF8String): Boolean; overload;
    function IcsRenameFileW(const OldName, NewName: UnicodeString): Boolean; overload;
    function IcsRenameFileW(const Utf8OldName, Utf8NewName: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsForceDirectoriesW(Dir: UnicodeString): Boolean; overload;
    function IcsForceDirectoriesW(Utf8Dir: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsCreateDirW(const Dir: UnicodeString): Boolean; overload;
    function IcsCreateDirW(const Utf8Dir: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsRemoveDirW(const Dir: UnicodeString): Boolean; overload;
    function IcsRemoveDirW(const Utf8Dir: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFileAgeW(const FileName: UnicodeString): Integer; overload;
    function IcsFileAgeW(const Utf8FileName: UTF8String): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsFileExistsW(const FileName: UnicodeString): Boolean; overload;
    function IcsFileExistsW(const Utf8FileName: UTF8String): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
    function IcsAnsiLowerCaseW(const S: UnicodeString): UnicodeString;     // angus
    function IcsAnsiUpperCaseW(const S: UnicodeString): UnicodeString;     // angus
    function IcsMakeWord(L, H: Byte): Word; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsMakeLong(L, H: Word): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsHiWord(LW: LongWord): Word; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsHiByte(W: Word): Byte; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsLoByte(W: Word): Byte; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsLoWord(LW: LongWord): Word; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure IcsCheckOSError(ALastError: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}

{ Moved from OverbyteIcsLibrary.pas prefix "_" replaced by "Ics" }
    function IcsIntToStrA(N : Integer): AnsiString;
    function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString;
    function IcsTrim(const Str : AnsiString) : AnsiString; {$IFDEF COMPILER12_UP} overload;
    function IcsTrim(const Str : UnicodeString) : UnicodeString; overload;
                    {$ENDIF}
    function IcsLowerCase(const S: AnsiString): AnsiString; {$IFDEF COMPILER12_UP} overload;
    function IcsLowerCase(const S: UnicodeString): UnicodeString; overload;
                    {$ENDIF}
    function IcsUpperCase(const S: AnsiString): AnsiString; {$IFDEF COMPILER12_UP} overload;
    function IcsUpperCase(const S: UnicodeString): UnicodeString; overload;
                    {$ENDIF}
    function IcsUpperCaseA(const S: AnsiString): AnsiString;
    function IcsLowerCaseA(const S: AnsiString): AnsiString;
    function IcsCompareTextA(const S1, S2: AnsiString): Integer;
    function IcsTrimA(const Str: AnsiString): AnsiString;
    function IcsSameTextA(const S1, S2: AnsiString): Boolean;
    function IcsCompareStr(const S1, S2: AnsiString): Integer; {$IFDEF COMPILER12_UP} overload;
    function IcsCompareStr(const S1, S2: UnicodeString): Integer; overload;
                    {$ENDIF}
    function IcsCompareText(const S1, S2: AnsiString): Integer;{$IFDEF COMPILER12_UP} overload;
    function IcsCompareText(const S1, S2: UnicodeString): Integer; overload;
                    {$ENDIF}
    function IcsStrLen(const Str: PAnsiChar): Cardinal;
  {$IFDEF COMPILER12_UP} overload;
    function IcsStrLen(const Str: PWideChar): Cardinal; overload;
  {$ENDIF}
    function IcsStrPas(const Str: PAnsiChar): AnsiString;
  {$IFDEF COMPILER12_UP} overload;
    function IcsStrPas(const Str: PWideChar): string; overload;
  {$ENDIF}
    function IcsStrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
  {$IFDEF COMPILER12_UP} overload;
    function IcsStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; overload;
  {$ENDIF}
    function IcsStrPCopy(Dest: PChar; const Source: string): PChar;
  {$IFDEF COMPILER12_UP} overload;
    function IcsStrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; overload;
  {$ENDIF}
    function IcsStrPLCopy(Dest: PChar; const Source: String; MaxLen: Cardinal): PChar; 
  {$IFDEF COMPILER12_UP} overload;
    function IcsStrPLCopy(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar; overload;
  {$ENDIF}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ end Moved from OverbyteIcsLibrary.pas }

{$IFDEF MSWINDOWS}
    // NT4 and better
    function IcsStrCompOrdinalW(Str1: PWideChar; Str1Length: Integer; Str2: PWideChar; Str2Length: Integer; IgnoreCase: Boolean): Integer;
    function  RtlCompareUnicodeString(String1 : PUNICODE_STRING;
        String2 : PUNICODE_STRING; CaseInsensitive : BOOLEAN): LongInt; stdcall;
  {$IF CompilerVersion < 21}
    function IsDebuggerPresent: BOOL; stdcall;
    {$EXTERNALSYM IsDebuggerPresent}
 {$IFEND}
{$ENDIF}

type
    TIcsIntegerList = class(TObject)
    private
        FList     : TList;
        function  GetCount: Integer;
        function  GetFirst: Integer;
        function  GetLast: Integer;
        function  GetItem(Index: Integer): Integer;
        procedure SetItem(Index: Integer; const Value: Integer);
    public
        constructor Create; virtual;
        destructor  Destroy; override;
        function    IndexOf(Item: Integer): Integer;
        function    Add(Item: Integer): Integer; virtual;
        procedure   Assign(Source: TIcsIntegerList); virtual;
        procedure   Clear; virtual;
        procedure   Delete(Index: Integer); virtual;
        property    Count: Integer read GetCount;
        property    First: Integer read GetFirst;
        property    Last : Integer read GetLast;
        property    Items[Index: Integer] : Integer read  GetItem
                                                    write SetItem; default;
    end;

    TIcsCriticalSection = class
    protected
        FSection: {$IFDEF MSWINDOWS} TRTLCriticalSection; {$ELSE} pthread_mutex_t; {$ENDIF}
    public
        constructor Create;
        destructor Destroy; override;
        procedure Enter; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure Leave; {$IFDEF USE_INLINE} inline; {$ENDIF}
        function TryEnter: Boolean;
    end;

implementation

const
    DefaultFailChar : AnsiChar  = '?';
    MAX_UTF8_SIZE       = 4;

    IcsPathDelimW       : WideChar  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
    IcsPathSepW         : WideChar  = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF}
    IcsPathDriveDelimW  : PWideChar = {$IFDEF MSWINDOWS} '\:';{$ELSE} '/'; {$ENDIF}
    IcsPathDelimA       : AnsiChar  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
{$IFDEF MSWINDOWS}
    IcsDriveDelimW      : WideChar  =  ':';
{$ENDIF}

{$IFDEF MSWINDOWS}
var
    hNtDll : THandle = 0;
    _RtlCompareUnicodeString : Pointer = nil;
  {$IF CompilerVersion < 21}
    function IsDebuggerPresent; external kernel32 name 'IsDebuggerPresent';
  {$IFEND}
{$ENDIF}

{$IFDEF USE_ICONV}
type
    TCpAlias = record
        C : LongWord;
        A : AnsiString;
    end;

const
    { Sorted by CP-ID for binary search, probably some mappings are incorrect }
    IconvCodepageMapping : array [0..44] of TCpAlias = (

    (C : 1200;        A : 'UTF-16LE'),
    (C : 1201;        A : 'UTF-16BE'),

    (C : 10000;       A : 'MAC'),         { MAC Roman; Western European (Mac) }

    (C : 10004;       A : 'MACARABIC'),   { Arabic (Mac) }
    (C : 10005;       A : 'MACHEBREW'),   { Hebrew (Mac) }
    (C : 10006;       A : 'MACGREEK'),    { Greek (Mac) }
    (C : 10007;       A : 'MACCYRILLIC'), { Cyrillic (Mac) }
    (C : 10010;       A : 'MACROMANIA'),  { Romanian (Mac) }
    (C : 10017;       A : 'MACUKRAINE'),  { Ukrainian (Mac) }
    (C : 10021;       A : 'MACTHAI'),     { Thai (Mac) }
    (C : 10029;       A : 'MACCENTRALEUROPE'), { MAC Latin 2; Central European (Mac) }
    (C : 10079;       A : 'MACICELAND'),  { Icelandic (Mac) }
    (C : 10081;       A : 'MACTURKISH'),  { Turkish (Mac) }
    (C : 10082;       A : 'MACCROATIAN'), { Croatian (Mac) }

    (C : 12000;       A : 'UTF-32LE'),
    (C : 12001;       A : 'UTF-32BE'),

    (C : 20127;       A : 'US-ASCII'),
    (C : 20866;       A : 'KOI8-R'),   { Russian (KOI8-R); Cyrillic (KOI8-R) }

    (C : 20932;       A : 'EUC-JP'),   { Japanese (JIS 0208-1990 and 0121-1990) }

    (C : 21866;       A : 'KOI8-U'),   { Ukrainian (KOI8-U); Cyrillic (KOI8-U) }

    (C : 28591;       A : 'iso-8859-1'), { ISO 8859-1 Latin 1; Western European (ISO) }
    (C : 28592;       A : 'iso-8859-2'), { ISO 8859-2 Central European; Central European (ISO) }
    (C : 28593;       A : 'iso-8859-3'), { ISO 8859-3 Latin 3 }
    (C : 28594;       A : 'iso-8859-4'), { ISO 8859-4 Baltic }
    (C : 28595;       A : 'iso-8859-5'), { ISO 8859-5 Cyrillic }
    (C : 28596;       A : 'iso-8859-6'), { ISO 8859-6 Arabic }
    (C : 28597;       A : 'iso-8859-7'), { ISO 8859-7 Greek }
    (C : 28598;       A : 'iso-8859-8'), { ISO 8859-8 Hebrew; Hebrew (ISO-Visual) }
    (C : 28599;       A : 'iso-8859-9'), { ISO 8859-9 Turkish }
    (C : 28603;       A : 'iso-8859-13'), { ISO 8859-13 Estonian }
    (C : 28605;       A : 'iso-8859-15'), { ISO 8859-15 Latin 9 }
    (C : 38598;       A : 'iso-8859-8-i'), { ISO 8859-8 Hebrew; Hebrew (ISO-Logical) }

    (C : 50220;       A : 'iso-2022-jp'), { ? ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS) }
    (C : 50221;       A : 'iso-2022-jp'), { ? ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana) }
    (C : 50222;       A : 'iso-2022-jp'), { ? ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI) }

    (C : 50225;       A : 'iso-2022-kr'), { ISO 2022 Korean }
    (C : 50227;       A : 'iso-2022-cn'), { ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022) }
    (C : 50229;       A : 'ISO-2022-CN-EXT'), { ? ISO 2022 Traditional Chinese }

    (C : 51932;       A : 'euc-jp'), { EUC Japanese }
    (C : 51936;       A : 'EUC-CN'), { EUC Simplified Chinese; Chinese Simplified (EUC) }
    (C : 51949;       A : 'euc-kr'), { EUC Korean }

    (C : 52936;       A : 'hz-gb-2312'), { HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ) }
    (C : 54936;       A : 'GB18030'), { Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030) }

    (C : 65000;       A : 'UTF-7'),
    (C : 65001;       A : 'UTF-8')
    );


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIconvNameFromCodePage(CodePage: LongWord): AnsiString;
var
    L, H, I: Integer;
begin
    if CodePage = CP_ACP then
        IcsGetAcp(CodePage);
    { Quick pre-check }
    if not ((CodePage >= 1250) and (CodePage <= 1258)) then
    begin
        { Binary search ? }
        L := 0;
        H := High(IconvCodepageMapping);
        while L <= H do
        begin
            I := (L + H) shr 1;
            if IconvCodepageMapping[I].C < CodePage then
                L := I + 1
            else begin
                H := I - 1;
                if IconvCodepageMapping[I].C = CodePage then
                begin
                    Result := IconvCodepageMapping[I].A;
                    Exit;
                end;
            end;
        end;
    end;
    Str(CodePage, Result);
    Result := 'CP' + Result;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetDefaultWindowsUnicodeChar(CodePage: LongWord): WideChar;
begin
    case CodePage of
        932,   // (ANSI/OEM - Japanese Shift-JIS) DBCS Lead Bytes: 81..9F E0..FC UnicodeDefaultChar: 0x30FB
        50220..50222, 51932, { Actually the same as for 932 with both MultiByteToWideChar and MLang.dll.}
        10001, // (MAC - Japanese) DBCS Lead Bytes: 81..9F E0..FC UnicodeDefaultChar: 0x30FB
        20932: // (JIS X 0208-1990 & 0212-1990) DBCS Lead Bytes: 8E..8E A1..FE UnicodeDefaultChar: 0x30FB
            Result := #$30FB;
    else
        if {$IFDEF MSWINDOWS} (Win32MajorVersion >= 6) and {$ENDIF}
           ((CodePage = 65000) or (CodePage = 65001)) then
            Result := #$FFFD
        else
            Result := #$003F;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetDefaultWindowsAnsiChar(CodePage: LongWord): AnsiChar;
begin
    case CodePage of
        37, 500, 875, 1026, 1140, 1141, 1142, 1143, 1144, 1145, 1147, 1149,
        20273, 20277, 20278, 20280, 20284, 20285, 20290, 20297, 20420, 20423,
        20424, 20833, 20838, 20871, 20880, 20905, 20924, 21025, 21027
          : Result := #$6F;
    else
        Result := #$3F;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsValidAnsiCodePage(const CP: LongWord): Boolean;
{$IFDEF MSWINDOWS}
begin
    Result := IsValidCodePage(CP);
end;
{$ENDIF}
{$IFDEF POSIX}
var
    Ctx: iconv_t;
begin
    Result := (CP <> 1200) and (CP <> 1201) and (CP <> 12000) and (CP <> 12001);
    if Result then
    begin
        Ctx := iconv_open(PAnsiChar(IcsIconvNameFromCodePage(CP)), ICONV_UNICODE);
        if Ctx = iconv_t(-1) then
            Result := False
        else begin
            iconv_close(Ctx);
            Result := True;
        end;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsCharLowerA(var ACh: AnsiChar);
begin
    if ACh in [#$41..#$5A] then
        ACh := AnsiChar(Ord(ACh) + 32);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  IcsGetCurrentThreadID: TThreadID;
begin
  {$IFDEF MSWINDOWS}
    Result := {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetCurrentThreadID;
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Posix.PThread.GetCurrentThreadID;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetTickCount: LongWord;
{$IFDEF MSWINDOWS}
begin
    Result := {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetTickCount;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF LINUX}
var
    t: tms;
begin
    Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;
{$ENDIF}
{$IFDEF MACOS}
begin
    Result := AbsoluteToNanoseconds(UpTime) div 1000000;
end;
{$ENDIF MACOS}
{$ENDIF POSIX}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetFreeDiskSpace(const APath: String): Int64;
{$IFDEF MSWINDOWS}
var
    TotalSpace, FreeSpace : Int64;
begin
    if GetDiskFreeSpaceEx (PChar(APath), FreeSpace, TotalSpace, nil) then
        Result := FreeSpace
    else
        Result := -1;
{$ENDIF}
{$IFDEF POSIX}
var
    FN  : RawByteString; // Path or file name
    Buf : _statvfs;
begin
    FN := UnicodeToAnsi(APath, CP_UTF8);
    if statvfs(PAnsiChar(FN), Buf) = 0 then
        Result := Int64(Buf.f_bfree) * Buf.f_frsize
    else
        Result := -1;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetLocalTimeZoneBias: LongInt;
{$IFDEF MSWINDOWS}
var
    tzInfo : TTimeZoneInformation;
begin
    case GetTimeZoneInformation(tzInfo) of
        TIME_ZONE_ID_STANDARD: Result := tzInfo.Bias + tzInfo.StandardBias;
        TIME_ZONE_ID_DAYLIGHT: Result := tzInfo.Bias + tzInfo.DaylightBias;
        TIME_ZONE_ID_UNKNOWN : Result := tzInfo.Bias;
    else
        Result := 0; // Error
    end;
end;
{$ENDIF}
{$IFDEF MACOS}
var
    LTZ: CFTimeZoneRef;
    LNow: CFAbsoluteTime;
    LSecFromUTC: CFTimeInterval;
    LSecInt: Integer;
    // DLSOffs: CFTimeInterval;
begin
    LTZ := CFTimeZoneCopyDefault;
    try
        LNow := CFAbsoluteTimeGetCurrent;
        LSecFromUTC := CFTimeZoneGetSecondsFromGMT(LTZ, LNow); // Includes DaylightSavingTime for me
        {if CFTimeZoneIsDaylightSavingTime(LTZ, LNow) then
        begin
            DLSOffs := CFTimeZoneGetDaylightSavingTimeOffset(LTZ, LNow);
        end;}
        LSecInt := Trunc(LSecFromUTC);
        if LSecInt <> 0 then
            Result := -(LSecInt div 60) // Minutes bias as windows, works for me, ToBeChecked
        else
            Result := 0;
    finally
        CFRelease(LTZ);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert local date/time to UTC/GMT }
function IcsDateTimeToUTC (dtDT: TDateTime): TDateTime;
begin
    Result := dtDT + IcsGetLocalTimeZoneBias / (60.0 * 24.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert UTC/GMT to local date/time }
function IcsUTCToDateTime (dtDT: TDateTime): TDateTime;
begin
    Result := dtDT - IcsGetLocalTimeZoneBias / (60.0 * 24.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
{$IFNDEF USE_ICONV}
function  IcsWcToMb(CodePage: LongWord; Flags: Cardinal; WStr: PWideChar;
  WStrLen: Integer; MbStr: PAnsiChar; MbStrLen: Integer; DefaultChar: PAnsiChar;
  UsedDefaultChar: PLongBool): Integer;
begin
    Result := WideCharToMultibyte(CodePage, Flags, WStr, WStrLen, MbStr,
                                  MbStrLen, DefaultChar, PBool(UsedDefaultChar));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  IcsMbToWc(CodePage: LongWord; Flags: Cardinal; MbStr: PAnsiChar;
  MbStrLen: Integer; WStr: PWideChar; WStrLen: Integer): Integer;
begin
    Result := MultiByteToWideChar(CodePage, Flags, MbStr, MbStrLen, WStr, WStrLen);
end;
{$ENDIF}
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_ICONV}
function StrByteCountUtf16(Utf16Str: PWord): Integer;
var
    BeginP : Pointer;
begin
    if Utf16Str <> nil then
    begin
        BeginP := Utf16Str;
        while Utf16Str^ <> $0000 do
            Inc(Utf16Str);
        Result := INT_PTR(Utf16Str) - INT_PTR(BeginP);
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsWcToMb(CodePage: LongWord; Flags: Cardinal; WStr: PWideChar;
  WStrLen: Integer; MbStr: PAnsiChar; MbStrLen: Integer; DefaultChar: PAnsiChar;
  UsedDefaultChar: PLongBool): Integer;
var
    CntOnly : Boolean;
    SBuf: array [0..255] of Byte;
    DstBytesLeft, SrcBytesLeft, CntSize : Integer;
    SrcPtr, DstPtr : Pointer;
    Ctx: iconv_t;
begin
    Result := 0;
{$IFDEF MSWINDOWS}
    if not Load_Iconv then
    begin
        SetLastError(ERROR_INVALID_PARAMETER);
        Exit;
    end;
{$ENDIF}
    Ctx := iconv_open(PAnsiChar(IcsIconvNameFromCodePage(CodePage)), ICONV_UNICODE);
    if Ctx = iconv_t(-1) then
    begin
        SetLastError(ERROR_INVALID_PARAMETER);
        Exit;
    end;
    try
        SrcPtr := WStr;
        SrcBytesLeft := WStrLen * SizeOf(WideChar);
        if SrcBytesLeft < 0 then
            SrcBytesLeft := StrByteCountUtf16(PWord(SrcPtr));
        CntSize := 0;
        if MbStrLen <= 0 then
        begin
            CntOnly := TRUE;
            DstBytesLeft := SizeOf(SBuf);
            DstPtr := @SBuf;
        end
        else begin
            CntOnly := FALSE;
            DstBytesLeft := MbStrLen;
            DstPtr  := MbStr;
        end;
        if UsedDefaultChar <> nil then
            UsedDefaultChar^ := FALSE;
        SetLastError(0);
        while True do
        begin
            Result := iconv(Ctx, @SrcPtr, @SrcBytesLeft, @DstPtr, @DstBytesLeft);
            if Result <> -1 then
            begin
                { Write the shift sequence and reset shift state }
                if (SrcPtr <> nil) {and ((Csc.CharSetType = ictMbcs) or (Csc.CodePage = CP_UTF7))} then
                    SrcPtr := nil
                else
                    Break;
            end
            else begin
                case Errno of
                    E2BIG: { There is not sufficient room at *outbuf }
                        if CntOnly then
                        begin
                            { Save stack buffer size and rewind }
                            Inc(CntSize, SizeOf(SBuf));
                            DstPtr := @SBuf;
                            DstBytesLeft := SizeOf(SBuf);
                        end
                        else begin
                            SetLastError(ERROR_INSUFFICIENT_BUFFER);
                            DstBytesLeft := MbStrLen;
                            Break;
                        end;
                    EILSEQ: { An invalid byte sequence has been encountered in the input }
                        begin
                            if (Flags and WC_ERR_INVALID_CHARS) = WC_ERR_INVALID_CHARS then
                            begin
                                Result := 0;
                                SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                                Exit;
                            end;
                            Inc(INT_PTR(SrcPtr), SizeOf(WideChar));
                            Dec(SrcBytesLeft, SizeOf(WideChar));
                            if not CntOnly then
                            begin
                                if DefaultChar = nil then
                                    PAnsiChar(DstPtr)^ := IcsGetDefaultWindowsAnsiChar(CodePage)
                                else
                                    PAnsiChar(DstPtr)^ :=  DefaultChar^;
                            end;
                            if UsedDefaultChar <> nil then
                                UsedDefaultChar^ := TRUE;
                            Inc(INT_PTR(DstPtr), 1);
                            Dec(DstBytesLeft, 1);
                        end;
                    EINVAL : { An incomplete sequence has been encountered in the input }
                        begin
                            { Write the shift sequence and reset shift state }
                            if (SrcPtr <> nil) {and
                              ((Csc.CharSetType = ictMbcs) or (Csc.CodePage = CP_UTF7))} then
                                SrcPtr := nil
                            else begin
                                if (Flags and WC_ERR_INVALID_CHARS) = WC_ERR_INVALID_CHARS then
                                begin
                                    Result := 0;
                                    SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                                    Exit;
                                end
                                else
                                    Break;
                            end;
                        end;
                else
                    Result := 0;
                    SetLastError(ERROR_INVALID_PARAMETER);
                    Exit;
                end;
            end;
        end;
        if CntOnly then
            Result := CntSize + SizeOf(SBuf) - DstBytesLeft
        else
            Result := MbStrLen - DstBytesLeft;

    finally
        iconv_close(Ctx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMbToWc(CodePage: LongWord; Flags: Cardinal; MbStr: PAnsiChar;
  MbStrLen: Integer; WStr: PWideChar; WStrLen: Integer): Integer;
var
    CntOnly : Boolean;
    SBuf: array [0..255] of Byte;
    DstBytesLeft, SrcBytesLeft, CntSize: Integer;
    SrcPtr, DstPtr : Pointer;
    Ctx: iconv_t;
    //LastBad: Pointer;
begin
    Result := 0;
{$IFDEF MSWINDOWS}
    if not Load_Iconv then
    begin
        SetLastError(ERROR_INVALID_PARAMETER);
        Exit;
    end;
{$ENDIF}
    Ctx := iconv_open(ICONV_UNICODE, PAnsiChar(IcsIconvNameFromCodePage(CodePage)));
    if Ctx = iconv_t(-1) then
    begin
        SetLastError(ERROR_INVALID_PARAMETER);
        Exit;
    end;
    try
        //LastBad := nil;
        SrcPtr := MbStr;
        SrcBytesLeft := MbStrLen;
        if SrcBytesLeft < 0 then
            SrcBytesLeft := IcsStrLen(MbStr) + 1;
        CntSize := 0;
        if WStrLen <= 0 then
        begin
            CntOnly := TRUE;
            DstBytesLeft := SizeOf(SBuf);
            DstPtr := @SBuf;
        end
        else begin
            CntOnly := FALSE;
            DstBytesLeft := WStrLen * SizeOf(WideChar);
            DstPtr := WStr;
        end;
        SetLastError(0);
        while True do
        begin
            Result := iconv(Ctx, @SrcPtr, @SrcBytesLeft, @DstPtr, @DstBytesLeft);
            if Result <> -1 then
                Break
            else begin
                case Errno of
                    E2BIG: { There is not sufficient room at *outbuf }
                        if CntOnly then
                        begin
                            { Save stack buffer size and rewind }
                            Inc(CntSize, SizeOf(SBuf));
                            DstPtr := @SBuf;
                            DstBytesLeft := SizeOf(SBuf);
                        end
                        else begin
                            Result := 0;
                            SetLastError(ERROR_INSUFFICIENT_BUFFER);
                            Break;
                        end;
                    EILSEQ: { An invalid byte sequence has been encountered in the input }
                        if Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS then
                        begin
                            Result := 0;
                            SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                            Exit;
                        end
                        else begin
                            if {(LastBad <> SrcPtr) and} (CodePage <> CP_UTF7) then
                            begin
                                PWideChar(DstPtr)^ := IcsGetDefaultWindowsUnicodeChar(CodePage);
                                Inc(INT_PTR(DstPtr), SizeOf(WideChar));
                                Dec(DstBytesLeft, SizeOf(WideChar));
                            end;
                            if IcsIsDBCSLeadByte(PAnsiChar(SrcPtr)^, CodePage) then
                            begin
                                Inc(INT_PTR(SrcPtr), 2);
                                Dec(SrcBytesLeft, 2);
                            end
                            else begin
                                Inc(INT_PTR(SrcPtr), 1);
                                Dec(SrcBytesLeft, 1);
                            end;
                            //LastBad := SrcPtr;
                        end;
                    EINVAL : { An incomplete multibyte sequence has been encountered in the input }
                        if Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS then
                        begin
                            Result := 0;
                            SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                            Exit;
                        end
                        else
                            Break;
                else
                    Result := 0;
                    SetLastError(ERROR_INVALID_PARAMETER);
                    Exit;
                end;
            end;
        end;
        if CntOnly then
            Result := (CntSize + SizeOf(SBuf) - DstBytesLeft) div SizeOf(WideChar)
        else
            Result := WStrLen - (DstBytesLeft div SizeOf(WideChar));
    finally
        iconv_close(Ctx);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
var
    DefaultAnsiCodePage : LongWord = 0;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsGetAcp(var CodePage: LongWord);
begin
{$IFNDEF COMPILER12_UP}
    if DefaultAnsiCodePage = 0 then
        DefaultAnsiCodePage := Windows.GetACP;
    CodePage := DefaultAnsiCodePage;
{$ELSE}
    CodePage := System.DefaultSystemCodePage;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsDBCSCodePage(CodePage: LongWord): Boolean;
begin
    { From Win 7 }
    case CodePage of
        932,   // (ANSI/OEM - Japanese Shift-JIS) DBCS Lead Bytes: 81..9F E0..FC UnicodeDefaultChar:30FB
        936,   // (ANSI/OEM - Simplified Chinese GBK) DBCS Lead Bytes: 81..FE
        949,   // (ANSI/OEM - Korean) DBCS Lead Bytes: 81..FE
        950,   // (ANSI/OEM - Traditional Chinese Big5) DBCS Lead Bytes: 81..FE
        1361,  // (Korean - Johab) DBCS Lead Bytes: 84..D3 D8..DE E0..F9
        {
        10001, // (MAC - Japanese) DBCS Lead Bytes: 81..9F E0..FC UnicodeDefaultChar:30FB
        10002, // (MAC - Traditional Chinese Big5) DBCS Lead Bytes: 81..FC
        10003, // (MAC - Korean) DBCS Lead Bytes: A1..AC B0..C8 CA..FD
        }
        10001..10003,
        10008, // (MAC - Simplified Chinese GB 2312) DBCS Lead Bytes: A1..A9 B0..F7
        {
        20000, // (CNS - Taiwan) DBCS Lead Bytes: A1..FE
        20001, // (TCA - Taiwan) DBCS Lead Bytes: 81..84 91..D8 DF..FC
        20002, // (Eten - Taiwan) DBCS Lead Bytes: 81..AF DD..FE
        20003, // (IBM5550 - Taiwan) DBCS Lead Bytes: 81..84 87..87 89..E8 F9..FB
        20004, // (TeleText - Taiwan) DBCS Lead Bytes: A1..FE
        20005, // (Wang - Taiwan) DBCS Lead Bytes: 8D..F5 F9..FC
        }
        20000..20005,
        20261, // (T.61) DBCS Lead Bytes: C1..CF
        20932, // (JIS X 0208-1990 & 0212-1990) DBCS Lead Bytes: 8E..8E A1..FE UnicodeDefaultChar: 30FB
        20936, // (Simplified Chinese GB2312) DBCS Lead Bytes: A1..A9 B0..F7
        51949: // (EUC-Korean) DBCS Lead Bytes: A1..AC B0..C8 CA..FD
            Result := TRUE;
    else
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsDBCSLeadByte(Ch: AnsiChar; CodePage: LongWord): Boolean;
begin
    case CodePage of
        932   : Result := Ch in ICS_LEAD_BYTES_932;
        936,
        949,
        950   : Result := Ch in ICS_LEAD_BYTES_936_949_950;
        1361  : Result := Ch in ICS_LEAD_BYTES_1361;
        10001 : Result := Ch in ICS_LEAD_BYTES_10001;
        10002 : Result := Ch in ICS_LEAD_BYTES_10002;
        10003 : Result := Ch in ICS_LEAD_BYTES_10003;
        10008 : Result := Ch in ICS_LEAD_BYTES_10008;
        20000 : Result := Ch in ICS_LEAD_BYTES_20000;
        20001 : Result := Ch in ICS_LEAD_BYTES_20001;
        20002 : Result := Ch in ICS_LEAD_BYTES_20002;
        20003 : Result := Ch in ICS_LEAD_BYTES_20003;
        20004 : Result := Ch in ICS_LEAD_BYTES_20004;
        20005 : Result := Ch in ICS_LEAD_BYTES_20005;
        20261 : Result := Ch in ICS_LEAD_BYTES_20261;
        20932 : Result := Ch in ICS_LEAD_BYTES_20932;
        20936 : Result := Ch in ICS_LEAD_BYTES_20936;
        51949 : Result := Ch in ICS_LEAD_BYTES_51949;
    else
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetLeadBytes(CodePage: LongWord): TIcsDbcsLeadBytes;
begin
    case CodePage of
        932   : Result := ICS_LEAD_BYTES_932;
        936,
        949,
        950   : Result := ICS_LEAD_BYTES_936_949_950;
        1361  : Result := ICS_LEAD_BYTES_1361;
        10001 : Result := ICS_LEAD_BYTES_10001;
        10002 : Result := ICS_LEAD_BYTES_10002;
        10003 : Result := ICS_LEAD_BYTES_10003;
        10008 : Result := ICS_LEAD_BYTES_10008;
        20000 : Result := ICS_LEAD_BYTES_20000;
        20001 : Result := ICS_LEAD_BYTES_20001;
        20002 : Result := ICS_LEAD_BYTES_20002;
        20003 : Result := ICS_LEAD_BYTES_20003;
        20004 : Result := ICS_LEAD_BYTES_20004;
        20005 : Result := ICS_LEAD_BYTES_20005;
        20261 : Result := ICS_LEAD_BYTES_20261;
        20932 : Result := ICS_LEAD_BYTES_20932;
        20936 : Result := ICS_LEAD_BYTES_20936;
        51949 : Result := ICS_LEAD_BYTES_51949;
    else
        Result := [];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsMBCSCodePage(CodePage: LongWord): Boolean;
begin
    { Reminder: MBCS do not support MBTOWC flag "MB_ERR_INVALID_CHARS" }
    case CodePage of
        {
        50220, // (ISO-2022 Japanese with no halfwidth Katakana) MBCS Max Size: 5
        50221, // (ISO-2022 Japanese with halfwidth Katakana) MBCS Max Size: 5
        50222, // (ISO-2022 Japanese JIS X 0201-1989) MBCS Max Size: 5
        }
        50220..50222,  // 7-Bit
        50225, // (ISO-2022 Korean) MBCS Max Size: 5 7-Bit
        50227, // (ISO-2022 Simplified Chinese) MBCS Max Size: 5 7-Bit
        50229, // (ISO-2022 Traditional Chinese) MBCS Max Size: 5 7-Bit

        51932, // (euc-jp  EUC Japanese MBCS Max Size: 3 // ** MLang.Dll only **  8-Bit
        52936, // (HZ-GB2312 Simplified Chinese) MBCS Max Size: 5  7-Bit
        54936, // (GB18030 Simplified Chinese) MBCS Max Size: 4   8-Bit


        //65000  // (UTF-7) MBCS Max Size: 5 UnicodeDefaultChar: FFFD, 003F XP 7-Bit
        //65001  // (UTF-8) MBCS Max Size: 4 UnicodeDefaultChar: FFFD, 003F XP 8-Bit

        {
        57002, // (ISCII - Devanagari) MBCS Max Size: 4
        57003, // (ISCII - Bengali) MBCS Max Size: 4
        57004, // (ISCII - Tamil) MBCS Max Size: 4
        57005, // (ISCII - Telugu) MBCS Max Size: 4
        57006, // (ISCII - Assamesisch) MBCS Max Size: 4
        57007, // (ISCII - Oriya) MBCS Max Size: 4
        57008, // (ISCII - Kannada) MBCS Max Size: 4
        57009, // (ISCII - Malayalam) MBCS Max Size: 4
        57010, // (ISCII - Gujarati) MBCS Max Size: 4
        57011  // (ISCII - Punjabi (Gurmukhi)) MBCS Max Size: 4
        }
        57002..57011 : Result := TRUE;   // 8-Bit
    else
        Result := FALSE;
    end;
end;


function IcsIsSBCSCodePage(CodePage: LongWord): Boolean;
begin
    case CodePage of
        {
        1250  (ANSI - Mitteleuropa)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1251  (ANSI - Kyrillisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1252  (ANSI - Lateinisch I)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1253  (ANSI - Griechisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1254  (ANSI - Türkisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1255  (ANSI - Hebräisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1256  (ANSI - Arabisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1257  (ANSI - Baltisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        1258  (ANSI/OEM - Vietnam)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        }
        1250..1258,
        20127, // (US-ASCII)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        21866, // (Ukrainisch - KOI8-U)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        {
        28591 (ISO 8859-1 Lateinisch I)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28592 (ISO 8859-2 Mitteleuropa)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28593 (ISO 8859-3 Lateinisch 3)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28594 (ISO 8859-4 Baltisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28595 (ISO 8859-5 Kyrillisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28596 (ISO 8859-6 Arabisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28597 (ISO 8859-7 Griechisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28598 (ISO 8859-8 Hebräisch: Visuelle Sortierung)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        28599 (ISO 8859-9 Lateinisch 5)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        }
        28591..28599,
        28605, // (ISO 8859-15 Lateinisch 9)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        38598, // (ISO 8859-8 Hebräisch: Logische Sortierung)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        20866, // (Russisch - KOI8)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F

        37,   // (IBM EBCDIC - USA/Kanada)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        437,  // (OEM - Vereinigte Staaten)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        500,  // (IBM EBCDIC - International)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        708,  // (Arabisch - ASMO)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        720,  // (Arabisch- Transparent ASMO)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        737,  // (OEM - Griechisch 437G)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        775,  // (OEM - Baltisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        850,  // (OEM - Multilingual Lateinisch I)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        852,  // (OEM - Lateinisch II)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        855,  // (OEM - Kyrillisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        857,  // (OEM - Türkisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        858,  // (OEM - Multilingual Lateinisch I + Euro)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        {
        860   (OEM - Portugisisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        861   (OEM - Isländisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        862   (OEM - Hebräisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        863   (OEM - Französch (Kanada))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        864   (OEM - Arabisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        865   (OEM - Nordisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        866   (OEM - Russisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        }
        860..866,
        869,  // (OEM - Modernes Griechisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        //874,  // (ANSI/OEM - Thai)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        874..875,  // (IBM EBCDIC - Modernes Griechisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1026, // (IBM EBCDIC - Türkisch (Lateinisch-5))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        {
        1140  (IBM EBCDIC - USA/Kanada (37 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1141  (IBM EBCDIC - Deutschland (20273 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1142  (IBM EBCDIC - Dänemark/Norwegen (20277 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1143  (IBM EBCDIC - Finnland/Schweden (20278 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1144  (IBM EBCDIC - Italien (20280 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1145  (IBM EBCDIC - Lateinamerika/Spanien (20284 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        }
        1140..1145,
        1147, // (IBM EBCDIC - Frankreich (20297 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        1149, // (IBM EBCDIC - Isländisch (20871 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F


        10000, // (MAC - Roman)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        {
        10004 (MAC - Arabisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10005 (MAC - Hebräisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10006 (MAC - Griechisch I)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10007 (MAC - Kyrillisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        }
        10004..10007,
        10010, // (MAC - Rumänisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10017, // (MAC - Ukrainisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10021, // (MAC - Thai)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10029, // (MAC - Lateinisch II)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10079, // (MAC - Isländisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10081, // (MAC - Türkisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        10082, // (MAC - Kroatisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        {
        20105 (IA5 IRV Internationales Alphabet Nr. 5)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        20106 (IA5 Deutsch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        20107 (IA5 Swedisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        20108 (IA5 Norwegisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        }
        20105..20108,

        20269, // (ISO 6937 Akzent ohne Zwischenraum)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:003F
        20273, // (IBM EBCDIC - Deutschland)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        {
        20277 (IBM EBCDIC - Dänemark/Norwegen)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20278 (IBM EBCDIC - Finnland/Schweden)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        }
        20277..20278,
        20280, // (IBM EBCDIC - Italien)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        {
        20284, // (IBM EBCDIC - Lateinamerika/Spanien)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20285, // (IBM EBCDIC - Großbritannien)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        }
        20284..20285,
        20290, // (IBM EBCDIC - Japanisch (erweitertes Katakana))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20297, // (IBM EBCDIC - Frankreich)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20420, // (IBM EBCDIC - Arabisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        {
        20423, // (IBM EBCDIC - Griechisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20424, // (IBM EBCDIC - Hebräisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        }
        20423..20424,
        20833, // (IBM EBCDIC - erweitertes Koreanisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20838, // (IBM EBCDIC - Thai)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20871, // (IBM EBCDIC - Isländisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20880, // (IBM EBCDIC - Kyrillisch (Russisch))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20905, // (IBM EBCDIC - Türkisch)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        20924, // (IBM EBCDIC - Lateinisch-1/Offenes System (1047 + Euro))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        21025, // (IBM EBCDIC - Kyrillisch (Serbisch, Bulgarisch))	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        21027 // (Ext Alpha Kleinbuchstaben)	SBCS Size: 1	UnicodeDefaultChar: 003F	DefaultChar:006F
        : Result := TRUE;
    else
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUsAscii(const Str: RawByteString): Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(Str) do
        if Byte(Str[I]) > 127 then begin
            Result := FALSE;
            Exit;
        end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUsAscii(const Str: UnicodeString): Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(Str) do
        if Ord(Str[I]) > 127 then begin
            Result := FALSE;
            Exit;
        end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Assumes parameter Str does not contain any 8Bit characters otherwise they   }
{ are replaced by FailCh. When we use plain ASCII payload this could be the   }
{ fastes cast. Sometimes we handle 7 bit strings only.                        }
function UnicodeToUsAscii(const Str: UnicodeString; FailCh: AnsiChar): AnsiString;
var
    I   : Integer;
    Len : Integer;
begin
    Len := Length(Str);
    SetLength(Result, Len);
    for I := 1 to Len do begin
        if Ord(Str[I]) > 127 then
            Result[I] := FailCh
        else
            Result[I] := AnsiChar(Str[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnicodeToUsAscii(const Str: UnicodeString): AnsiString;
begin
    Result := UnicodeToUsAscii(Str, DefaultFailChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts an UnicodeString to an AnsiString.                                 }
function UnicodeToAnsi(const Str: UnicodeString; ACodePage: LongWord; SetCodePage: Boolean = False): RawByteString;
var
    Len, Len2 : Integer;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := IcsWcToMb(ACodePage, 0, Pointer(Str), Len, nil, 0, nil, nil);
        SetLength(Result, Len);
        if Len > 0 then begin
            Len2 := IcsWcToMb(ACodePage, 0, Pointer(Str), Length(Str),
                                Pointer(Result), Len, nil, nil);
            if Len2 <> Len then // May happen, very rarely
                SetLength(Result, Len2);                    
        {$IFDEF COMPILER12_UP}
            if SetCodePage and (ACodePage <> CP_ACP) then
                PWord(INT_PTR(Result) - 12)^ := ACodePage;
        {$ENDIF}
        end;
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts an UnicodeString to an AnsiString using current code page.         }
function UnicodeToAnsi(const Str: UnicodeString): RawByteString;
begin
    Result := UnicodeToAnsi(Str, CP_ACP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: PAnsiChar; ACodePage: LongWord): UnicodeString;
var
    Len, Len2 : Integer;
begin
    if (Str <> nil) then begin
        Len := IcsMbToWc(ACodePage, 0, Str, -1, nil, 0);
        if Len > 1 then begin // counts the null-terminator
            SetLength(Result, Len - 1);
            Len2 := IcsMbToWc(ACodePage, 0, Str, -1,
                                Pointer(Result), Len);
            if Len2 <> Len then  // May happen, very rarely
            begin
                if Len2 > 0 then
                    SetLength(Result, Len2 - 1)
                else
                    Result := '';
            end;
        end
        else
            Result := '';
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnicodeToAnsi(const Str: PWideChar; ACodePage: LongWord;
  SetCodePage: Boolean = False): RawByteString;
var
    Len, Len2 : Integer;
begin
    if (Str <> nil) then begin
        Len := IcsWcToMb(ACodePage, 0, Str, -1, nil, 0, nil, nil);
        if Len > 1 then begin // counts the null-terminator
            SetLength(Result, Len - 1);
            Len2 := IcsWcToMb(ACodePage, 0, Str, -1,
                                Pointer(Result), Len,
                                nil, nil);
            if Len2 <> Len then // May happen, very rarely
            begin
                if Len2 > 0 then
                    SetLength(Result, Len2 - 1)
                else
                    Result := '';
            end;
        {$IFDEF COMPILER12_UP}
            if SetCodePage and (ACodePage <> CP_ACP) then
                PWord(INT_PTR(Result) - 12)^ := ACodePage;
        {$ENDIF}
        end
        else
            Result := '';
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: RawByteString; ACodePage: LongWord): UnicodeString;
var
    Len, Len2 : Integer;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := IcsMbToWc(ACodePage, 0, Pointer(Str),
                                   Len, nil, 0);
        SetLength(Result, Len);
        if Len > 0 then
        begin
            Len2 := IcsMbToWc(ACodePage, 0, Pointer(Str), Length(Str),
                                Pointer(Result), Len);
            if Len2 <> Len then // May happen, very rarely
                SetLength(Result, Len2);
        end;
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: RawByteString): UnicodeString;
begin
    Result := AnsiToUnicode(Str, CP_ACP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UsAsciiToUnicode(const Str: RawByteString; FailCh: AnsiChar): UnicodeString;
var
    I  : Integer;
    P  : PSmallInt;
begin
    SetLength(Result, Length(Str));
    P := Pointer(Result);
    for I := 1 to Length(Str) do begin
        if Byte(Str[I]) > 127 then
            P^ := Byte(FailCh)
        else
            P^ := Byte(Str[I]);
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UsAsciiToUnicode(const Str: RawByteString): UnicodeString;
begin
    Result := UsAsciiToUnicode(Str, DefaultFailChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSwap16(Value: Word): Word;
{$IFDEF PUREPASCAL}
begin
    Result := (Value shr 8) or (Value shl 8);
{$ELSE}
asm
{$IFDEF CPUX64}
    MOV   AX, CX
{$ENDIF}
    XCHG  AL, AH
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsSwap16Buf(Src, Dst: PWord; WordCount: Integer);
{$IFDEF PUREPASCAL}
var
    I : Integer;
begin
    for I := 1 to WordCount do
    begin
        Dst^ := (Src^ shr 8) or (Src^ shl 8);
        Inc(Src);
        Inc(Dst);
    end;
{$ELSE}
asm
{$IFDEF CPUX64}
{ Src in RCX
  Dst in RDX
  WordCount in R8D }

       SUB    RCX, RDX
       SUB    R8D, 4
       JS     @@2
@@1:
       MOV    EAX, [RCX + RDX]
       MOV    R9D, [RCX + RDX + 4]
       BSWAP  EAX
       BSWAP  R9D
       MOV    WORD PTR [RDX + 2], AX
       MOV    WORD PTR [RDX + 6], R9W
       SHR    EAX, 16
       SHR    R9D, 16
       MOV    WORD PTR [RDX], AX
       MOV    WORD PTR [RDX + 4], R9W
       ADD    RDX, 8
       SUB    R8D, 4
       JNS    @@1
@@2:
       ADD    R8D, 2
       JS     @@3
       MOV    EAX, [RCX + RDX]
       BSWAP  EAX
       MOV    WORD PTR [RDX + 2], AX
       SHR    EAX, 16
       MOV    WORD PTR [EDX], AX
       ADD    RDX, 4
       SUB    R8D, 2
@@3:
       INC    R8D
       JNZ    @@Exit
       MOV    RAX, [RCX + RDX]
       XCHG   AL, AH
       MOV    WORD PTR [RDX], AX
@@Exit:

{$ELSE}
{ Thanks to Jens Dierks for this code }
{ Src in EAX
  Dst in EDX
  WordCount in ECX }

       PUSH   ESI
       PUSH   EBX
       SUB    EAX,EDX
       SUB    ECX,4
       JS     @@2
@@1:
       MOV    EBX,[EAX + EDX]
       MOV    ESI,[EAX + EDX + 4]
       BSWAP  EBX
       BSWAP  ESI
       MOV    [EDX + 2],BX
       MOV    [EDX + 6],SI
       SHR    EBX, 16
       SHR    ESI, 16
       MOV    [EDX],BX
       MOV    [EDX + 4],SI
       ADD    EDX, 8
       SUB    ECX, 4
       JNS    @@1
@@2:
       ADD    ECX, 2
       JS     @@3
       MOV    EBX,[EAX + EDX]
       BSWAP  EBX
       MOV    [EDX + 2],BX
       SHR    EBX, 16
       MOV    [EDX],BX
       ADD    EDX, 4
       SUB    ECX, 2
@@3:
       INC    ECX
       JNZ    @@4
       MOV    BX,[EAX + EDX]
       XCHG   BL,BH
       MOV    [EDX],BX
@@4:
       POP    EBX
       POP    ESI
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSwap32(Value: LongWord): LongWord;
{$IFDEF PUREPASCAL}
begin
    Result := Word(((Value shr 16) shr 8) or ((Value shr 16) shl 8)) or
              Word((Word(Value) shr 8) or (Word(Value) shl 8)) shl 16;
{$ELSE}
asm
{$IFDEF CPUX64}
    MOV    EAX, ECX
{$ENDIF}
    BSWAP  EAX
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsSwap32Buf(Src, Dst: PLongWord; LongWordCount: Integer);
{$IFDEF PUREPASCAL}
var
    I : Integer;
begin
    for I := 1 to LongWordCount do
    begin
        Dst^ := Word(((Src^ shr 16) shr 8) or ((Src^ shr 16) shl 8)) or
                Word((Word(Src^) shr 8) or (Word(Src^) shl 8)) shl 16;
        Inc(Src);
        Inc(Dst);
    end;
{$ELSE}
asm
{$IFDEF CPUX64}
{ Src in RCX
  Dst in RDX
  LongWordCount in R8D }

       SUB    RCX, RDX
       SUB    R8D, 2
       JS     @@2
@@1:
       MOV    EAX, [RCX + RDX]
       MOV    R9D, [RCX + RDX + 4]
       BSWAP  EAX
       BSWAP  R9D
       MOV    DWORD PTR [RDX], EAX
       MOV    DWORD PTR [RDX + 4], R9D
       ADD    RDX, 8
       SUB    R8D, 2
       JNS    @@1
@@2:
       INC    R8D
       JS     @Exit
       MOV    EAX, [RCX + RDX]
       BSWAP  EAX
       MOV    DWORD PTR [RDX], EAX
@Exit:

{$ELSE}
{ Src in EAX
  Dst in EDX
  LongWordCount in ECX }

       PUSH   ESI
       PUSH   EBX
       SUB    EAX, EDX
       SUB    ECX, 2
       JS     @@2
@@1:
       MOV    EBX,[EAX + EDX]
       MOV    ESI,[EAX + EDX + 4]
       BSWAP  EBX
       BSWAP  ESI
       MOV    [EDX], EBX
       MOV    [EDX + 4], ESI
       ADD    EDX, 8
       SUB    ECX, 2
       JNS    @@1
@@2:
       INC    ECX
       JS     @Exit
       MOV    EBX,[EAX + EDX]
       BSWAP  EBX
       MOV    [EDX], EBX
@Exit:
       POP    EBX
       POP    ESI
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSwap64(Value: Int64): Int64;
{$IFDEF PUREPASCAL}
var
    H, L: LongWord;
begin
    H := LongWord(Value shr 32);
    L := LongWord(Value);
    H := Word(((H shr 16) shr 8) or ((H shr 16) shl 8)) or
         Word((Word(H) shr 8) or (Word(H) shl 8)) shl 16;
    L := Word(((L shr 16) shr 8) or ((L shr 16) shl 8)) or
         Word((Word(L) shr 8) or (Word(L) shl 8)) shl 16;
    Result := Int64(H) or Int64(L) shl 32;
{$ELSE}
asm
{$IFDEF CPUX64}
    MOV    RAX, RCX
    BSWAP  RAX
{$ELSE}
    MOV   EDX,  [EBP + $08]
    MOV   EAX,  [EBP + $0C]
    BSWAP EAX
    BSWAP EDX
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsSwap64Buf(Src, Dst: PInt64; QuadWordCount: Integer);
{$IFDEF PUREPASCAL}
var
    H, L: LongWord;
    I : Integer;
begin
    for I := 1 to QuadWordCount do
    begin
        H := LongWord(Src^ shr 32);
        L := LongWord(Src^);
        H := Word(((H shr 16) shr 8) or ((H shr 16) shl 8)) or
             Word((Word(H) shr 8) or (Word(H) shl 8)) shl 16;
        L := Word(((L shr 16) shr 8) or ((L shr 16) shl 8)) or
             Word((Word(L) shr 8) or (Word(L) shl 8)) shl 16;
        Dst^ := Int64(H) or Int64(L) shl 32;
        Inc(Src);
        Inc(Dst);
    end;
{$ELSE}
asm
{$IFDEF CPUX64}
{ Src in RCX
  Dst in RDX
  QuadWordCount in R8D }

       SUB    RCX, RDX
       SUB    R8D, 2
       JS     @@2
@@1:
       MOV    RAX, [RCX + RDX]
       MOV    R9,  [RCX + RDX + 8]
       BSWAP  RAX
       BSWAP  R9
       MOV    [RDX], RAX
       MOV    [RDX + 8], R9
       ADD    RDX, 16
       SUB    R8D, 2
       JNS    @@1
@@2:
       INC    R8D
       JS     @Exit
       MOV    RAX, [RCX + RDX]
       BSWAP  RAX
       MOV    [RDX], RAX
@Exit:

{$ELSE}
{ Src in EAX
  Dst in EDX
  QuadWordCount in ECX }

       PUSH   ESI
       PUSH   EBX
       SUB    EAX, EDX
       DEC    ECX
       JS     @Exit
@@1:
       MOV    EBX,[EAX + EDX]
       MOV    ESI,[EAX + EDX + 4]
       BSWAP  EBX
       BSWAP  ESI
       MOV    [EDX], ESI
       MOV    [EDX + 4], EBX
       ADD    EDX, 8
       DEC    ECX
       JNS    @@1
@Exit:
       POP    EBX
       POP    ESI
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Result is the number of translated WideChars, InvalidEndByteCount returns   }
{ the number of untranslated bytes at the end of the source buffer only       }
{ (if any). If there were invalid byte sequences somewhere else they may be   }
{ translated/counted or not depending on the OS version and code page.        }
{ BufferCodePage may include CP_UTF16, CP_UTF16Be, CP_UTF32 and CP_UTF32Be    }
function IcsGetWideCharCount(const Buffer; BufferSize: Integer;
  BufferCodePage: LongWord; out InvalidEndByteCount: Integer): Integer;
    
    function GetMbcsInvalidEndBytes(const EndBuf: PAnsiChar): Integer;
    var
        P : PAnsiChar;
        Utf8Size : Integer;
    begin
        { If last byte equals NULL this function always returns "0"           }
        if INT_PTR(@Buffer) < INT_PTR(EndBuf) then
        begin
            { Try to get a pointer to the last lead byte, see comment in      }
            { IcsStrPrevChar()                                                }
            P := IcsStrPrevChar(@Buffer, EndBuf, BufferCodePage);
            Result := INT_PTR(EndBuf) - INT_PTR(P);
            if (Result > 0) and (BufferCodePage = CP_UTF8) then
            begin
                Utf8Size := IcsUtf8Size(Byte(P^));
                if (Utf8Size > 0) and (Utf8Size < Result) then
                begin { Looks like we got a complete and a trunkated sequence }
                    if (Utf8Size = 1) { should always translate } or
                       (IcsMbToWc(BufferCodePage, MB_ERR_INVALID_CHARS,
                                            P, Utf8Size, nil, 0) > 0) then
                    begin
                        Inc(P, Utf8Size);
                        Dec(Result, Utf8Size);
                    end;
                end;
            end;
            if (Result > 0) and
               (IcsMbToWc(BufferCodePage, MB_ERR_INVALID_CHARS,
                           P, Result, nil, 0) > 0) then
                Result := 0;
        end
        else
            Result := 0;
    end;

var
    I     : Integer;
    Bytes : PByte;
    LastErr : LongWord;
begin
    Bytes := @Buffer;
    case BufferCodePage of
        CP_UTF16,
        CP_UTF16Be  :
            begin
                Result := BufferSize div SizeOf(WideChar);
                InvalidEndByteCount := BufferSize mod SizeOf(WideChar);
            end;
        CP_UTF32    :
            begin
                Result := BufferSize div SizeOf(UCS4Char);
                InvalidEndByteCount := BufferSize mod SizeOf(UCS4Char);
                for I := 1 to Result do
                begin
                    if PLongWord(Bytes)^ > $10000 then
                        Inc(Result); // Surrogate pair
                    Inc(Bytes, SizeOf(UCS4Char));
                end;
            end;
        CP_UTF32Be  :
            begin
                Result := BufferSize div SizeOf(UCS4Char);
                InvalidEndByteCount := BufferSize mod SizeOf(UCS4Char);
                for I := 1 to Result do
                begin
                    if IcsSwap32(PLongWord(Bytes)^) > $10000 then
                        Inc(Result); // Surrogate pair
                    Inc(Bytes, SizeOf(UCS4Char));
                end;
            end;
        else
            InvalidEndByteCount := 0;
            Result := IcsMbToWc(BufferCodePage, MB_ERR_INVALID_CHARS,
                                          PAnsiChar(Bytes), BufferSize, nil, 0);
            { Not every code page supports flag MB_ERR_INVALID_CHARS.         }
            { Depends on the Windows version as well, see SDK-docs.           }
            { However mbtowc's doc is not correct regarding older Windows.    }
            { Some tests with UTF-8 showed that in W2K SP4 and XP SP3 mbtowc  }
            { happily takes this flag and seems to skip invalid source bytes  }
            { silently if they are NOT at the end of the source buffer. If    }
            { they are at the end mbtowc fails as documented. Other MBCS seem }
            { to work as documented (tested 932 only). Windows Vista seems to }
            { work as documented too.                                         }
            if Result = 0 then
            begin
                LastErr := GetLastError;
                if LastErr = ERROR_INVALID_FLAGS then
                    { Try again with flags "0", nothing else can be done      }
                    Result := IcsMbToWc(BufferCodePage, 0,
                                        PAnsiChar(Bytes), BufferSize, nil, 0)
                else if LastErr = ERROR_NO_UNICODE_TRANSLATION then
                begin
                    { There's some invalid bytes but we don't know where in  }
                    { the source buffer. Try to get the number of            }
                    { untranslated bytes at the end of the source buffer     }
                    {(if any). It won't work with all code pages correctly.  }
                    { According to Mrs. Kaplan, code pages 932, 936, 949,    }
                    { 950, and 1361 are supported. UTF-8 support is an ICS   }
                    { home-grown routine.                                    }
                    InvalidEndByteCount := GetMbcsInvalidEndBytes(
                                              PAnsiChar(Bytes) + BufferSize);
                    { Then call mbtowc with a shorter source buffer and flag }
                    { "0".                                                   }
                    Result := IcsMbToWc(BufferCodePage, 0,
                        PAnsiChar(Bytes), BufferSize - InvalidEndByteCount,
                        nil, 0);
                end;
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ BufferCodePage may include CP_UTF16, CP_UTF16Be, CP_UTF32 and CP_UTF32Be }
function IcsGetWideChars(const Buffer; BufferSize: Integer;
   BufferCodePage: LongWord; Chars: PWideChar; CharCount: Integer): Integer;
var
    PUCS4 : PUCS4Char;
    I     : Integer;
    
    procedure UCS4ToU16;
    begin
        I := 0;
        while I < CharCount do begin
            if PUCS4^ > $10000 then
            begin
                { Encode Surrogate pair }
                Inc(I);
                Chars^ := WideChar((((PUCS4^ - $00010000) shr 10) and
                                   $000003FF) or $D800);
                Inc(I);
                Inc(Chars);
                Chars^ := WideChar(((PUCS4^ - $00010000) and $000003FF) or
                                   $DC00);
            end
            else begin
                Inc(I);
                Chars^ := WideChar(PUCS4^);
            end;
            Inc(PUCS4);
            Inc(Chars);
        end;
    end;

begin
    case BufferCodePage of
        CP_UTF16    :
            begin
                Move(Buffer, Chars^, BufferSize);
                Result := CharCount;
            end;
        CP_UTF16Be  :
            begin
                IcsSwap16Buf(@Buffer, Pointer(Chars), CharCount);
                Result := CharCount;
            end;
        CP_UTF32    :
            begin
                PUCS4 := @Buffer;
                UCS4ToU16;
                Result := CharCount;
            end;
        CP_UTF32Be  :
            begin
                IcsSwap32Buf(@Buffer, @Buffer, BufferSize div SizeOf(UCS4Char));
                PUCS4 := @Buffer;
                UCS4ToU16;
                Result := CharCount;
            end;

        else
           Result := IcsMbToWc(BufferCodePage, 0, @Buffer,
                                         BufferSize, Chars, CharCount);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ BufferCodePage may include CP_UTF16, CP_UTF16Be, CP_UTF32 and CP_UTF32Be    }
function IcsBufferToUnicode(const Buffer; BufferSize: Integer;
  BufferCodePage: LongWord; out FailedByteCount: Integer): UnicodeString;
var
    WCharCnt: Integer;
begin
    FailedByteCount := 0;
    if (@Buffer = nil) or (BufferSize <= 0) then
        Result := ''
    else begin
        WCharCnt := IcsGetWideCharCount(Buffer, BufferSize, BufferCodePage,
                                        FailedByteCount);
        SetLength(Result, WCharCnt);
        if WCharCnt > 0 then
            IcsGetWideChars(Buffer, BufferSize - FailedByteCount,
                            BufferCodePage, PWideChar(Result), WCharCnt);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ BufferCodePage may include CP_UTF16, CP_UTF16Be, CP_UTF32 and CP_UTF32Be }
function IcsBufferToUnicode(const Buffer; BufferSize: Integer;
  BufferCodePage: LongWord; RaiseFailedBytes: Boolean = FALSE): UnicodeString;
var
    FailedBytes : Integer;
begin
    FailedBytes := 0;
    Result := IcsBufferToUnicode(Buffer, BufferSize, BufferCodePage, FailedBytes);
    if RaiseFailedBytes and (FailedBytes > 0) then
        raise EIcsStringConvertError.CreateFmt(
                        'Invalid bytes in source buffer. %d bytes untranslated',
                                                 [FailedBytes]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsAppendStr(var Dest: RawByteString; const Src: RawByteString);
begin
{$IFDEF COMPILER12_UP}
    SetLength(Dest, Length(Dest) + Length(Src));
    Move(Pointer(Src)^, Dest[Length(Dest) - Length(Src) + 1], Length(Src));
{$ELSE}
    Dest := Dest + Src;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetBomBytes(ACodePage: LongWord): TBytes;
begin
    case ACodePage of
        CP_UTF16 :
        begin
            SetLength(Result, 2);
            Result[0] := $FF;
            Result[1] := $FE;
        end;
        CP_UTF16Be :
        begin
            SetLength(Result, 2);
            Result[0] := $FE;
            Result[1] := $FF;
        end;
        CP_UTF8    :
        begin
            SetLength(Result, 3);
            Result[0] := $EF;
            Result[1] := $BB;
            Result[2] := $BF;
        end;
        CP_UTF32   :
        begin
            SetLength(Result, 4);
            Result[0] := $FF;
            Result[1] := $FE;
            Result[2] := $00;
            Result[3] := $00;
        end;
        CP_UTF32Be :
        begin
            SetLength(Result, 4);
            Result[0] := $00;
            Result[1] := $00;
            Result[2] := $FE;
            Result[3] := $FF;
        end;
        else
            SetLength(Result, 0);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  IcsGetBufferCodepage(Buf: PAnsiChar; ByteCount: Integer): LongWord;  { V8.07 }
var
  LBOMSize: Integer;
begin
  Result := IcsGetBufferCodepage(Buf, ByteCount, LBOMSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetBufferCodepage(Buf: PAnsiChar; ByteCount: Integer;              { V8.07 }
  out BOMSize: Integer): LongWord;
begin
    Result := CP_ACP;
    BOMSize := 0;
    if (Buf = nil) then
        Exit;
    if (ByteCount > 3) and (Buf[0] = #$FF) and (Buf[1] = #$FE) and
       (Buf[2] = #0) and (Buf[3] = #0) then begin
        Result := CP_UTF32;
        BOMSize := 4;
    end
    else if (ByteCount > 3) and (Buf[0] = #0) and (Buf[1] = #0) and
            (Buf[2] = #$FE) and (Buf[3] = #$FF) then begin
        Result := CP_UTF32Be;
        BOMSize := 4;
    end
    else if (ByteCount > 2) and (Buf[0] = #$EF) and (Buf[1] = #$BB) and
            (Buf[2] = #$BF)  then begin
        Result := CP_UTF8;
        BOMSize := 3;
    end
    else if (ByteCount > 1) and (Buf[0] = #$FF) and (Buf[1] = #$FE) then begin
        Result := CP_UTF16;
        BOMSize := 2;
    end
    else if (ByteCount > 1) and (Buf[0] = #$FE) and (Buf[1] = #$FF) then begin
        Result := CP_UTF16Be;
        BOMSize := 2;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Assumes that the string is a Windows, UTF-16 little endian wide string      }
function StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer;
  ACodePage: LongWord; WriteBOM: Boolean): Integer;
var
    SBuf  : array [0..2047] of Byte;
    Len   : Integer;
    HBuf  : PAnsiChar;
    Bom   : TBytes;
    CurCP : Word;
    Swap  : Boolean;
    Dump  : Boolean;
begin
    Result := 0;
    if (Str = nil) or (cLen <= 0) then
        Exit;
    CurCP := CP_UTF16; //PWord(Integer(Str) - 12)^;
    case ACodePage of
        CP_UTF16  :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 2);
                    BOM[0] := $FF;
                    BOM[1] := $FE;
                end;
                Swap := CurCP = CP_UTF16Be;
                Dump := (CurCP = ACodePage) or Swap;
            end;
        CP_UTF16Be :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 2);
                    BOM[0] := $FE;
                    BOM[1] := $FF;
                end;
                Swap := CurCP = CP_UTF16;
                Dump := (CurCP = ACodePage) or Swap;
            end;
        CP_UTF8 :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 3);
                    BOM[0] := $EF;
                    BOM[1] := $BB;
                    BOM[2] := $BF;
                end;
                Dump := FALSE;
                Swap := FALSE;
            end;
        else
            SetLength(BOM, 0);
            Dump := FALSE;
            Swap := FALSE;
    end; // case

    if Dump and not Swap then
    begin // No conversion needed
        if Bom <> nil then
            AStream.Write(Bom[0], Length(Bom));
        Result := AStream.Write(Pointer(Str)^, cLen * 2); //Use const char length 
    end
    else begin
        if Dump and Swap then
        begin // We need to swap bytes and write them to the stream
            if Bom <> nil then
                AStream.Write(Bom[0], Length(Bom));
            IcsSwap16Buf(Pointer(Str), Pointer(Str), cLen);
            Result := Result + AStream.Write(Str^, cLen * 2);
        end
        else begin // Charset conversion
            Len := IcsWcToMb(ACodePage, 0, Pointer(Str), cLen,
                                       nil, 0, nil, nil);
            if Len <= SizeOf(SBuf) then begin
                Len := IcsWcToMb(ACodePage, 0, Pointer(Str), cLen,
                                           @SBuf, Len, nil, nil);
                if (Len > 0) then begin
                    if Bom <> nil then
                        AStream.Write(Bom[0], Length(Bom));
                    Result := AStream.Write(SBuf[0], Len);
                end;
            end
            else begin
                GetMem(HBuf, Len);
                try
                    Len := IcsWcToMb(ACodePage, 0, Pointer(Str), cLen,
                                               HBuf, Len, nil, nil);
                    if (Len > 0) then begin
                        if Bom <> nil then
                            AStream.Write(Bom[0], Length(Bom));
                        Result := AStream.Write(HBuf^, Len);
                    end;
                finally
                    FreeMem(HBuf);
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer;
  ACodePage: LongWord): Integer;
begin
    Result := StreamWriteString(AStream, Str, cLen, ACodePage, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString;
 ACodePage: LongWord; WriteBOM: Boolean): Integer;
begin
    Result := StreamWriteString(AStream, Pointer(Str), Length(Str),
                                ACodePage, WriteBom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString): Integer;
begin
    Result:= StreamWriteString(AStream, Pointer(Str), Length(Str),
                               CP_ACP, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString;
  ACodePage: LongWord): Integer;
begin
    Result:= StreamWriteString(AStream, Pointer(Str), Length(Str),
                               ACodePage, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi(const Str: AnsiString): Integer;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi(const Str : RawByteString): Integer;
var
    P : PAnsiChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$20 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Byte(P^) - Byte('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi(const Str: UnicodeString): Integer;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi(const Str : UnicodeString): Integer;
var
    P : PWideChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$0020 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Ord(P^) - Ord('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF STREAM64}
(*
function atoi64(const Str: AnsiString): Int64;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi64(const Str : RawByteString): Int64;
var
    P : PAnsiChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$20 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Byte(P^) - Byte('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi64(const Str: UnicodeString): Int64;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi64(const Str : UnicodeString): Int64;
var
    P : PWideChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$0020 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Ord(P^) - Ord('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCalcTickDiff(const StartTick, EndTick : LongWord): LongWord;
begin
    if EndTick >= StartTick then
        Result := EndTick - StartTick
    else
        Result := High(LongWord) - StartTick + EndTick;
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function StringToUtf8(const Str: UnicodeString): RawByteString;
begin
    Result := UnicodeToAnsi(Str, CP_UTF8, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ConvertCodepage(const Str: RawByteString; SrcCodePage: LongWord;
  DstCodePage: LongWord = CP_ACP): RawByteString;
var
    SBuf : array[0..2047] of WideChar;
    P    : PWideChar;
    sLen : Integer;
    dLen : Integer;
    FreeFlag : Boolean;
begin
    sLen := Length(Str);
    if (sLen = 0) or (SrcCodePage = DstCodePage) then
    begin
        Result := Str;
        Exit;
    end;
    dLen := IcsMbToWc(SrcCodePage, 0, Pointer(Str), sLen, nil, 0);
    if dLen = 0 then
    begin
        Result := '';
        Exit;
    end;
    if dLen > Length(SBuf) then
    begin
        GetMem(P, dLen * 2);
        FreeFlag := TRUE;
    end
    else begin
        FreeFlag := FALSE;
        P := SBuf;
    end;
    dLen := IcsMbToWc(SrcCodePage, 0, Pointer(Str), sLen, P, dLen);
    if dLen > 0 then
    begin
        sLen := IcsWcToMb(DstCodePage, 0, P, dLen, nil, 0, nil, nil);
        SetLength(Result, sLen);
        if sLen > 0 then
        begin
            dLen := IcsWcToMb(DstCodePage, 0, P, dLen, Pointer(Result), sLen, nil, nil);
            if dLen <> sLen then
                SetLength(Result, dLen);
        {$IFDEF COMPILER12_UP}
            if DstCodePage <> CP_ACP then
                PWord(INT_PTR(Result) - 12)^ := DstCodePage;
        {$ENDIF}
        end;
    end
    else
        Result := '';
    if FreeFlag then FreeMem(P);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToUtf8(const Str: RawByteString; ACodePage: LongWord = CP_ACP): RawByteString;
begin
    Result := ConvertCodepage(Str, ACodePage, CP_UTF8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Utf8ToStringW(const Str: RawByteString): UnicodeString;
begin
    Result := AnsiToUnicode(Str, CP_UTF8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Utf8ToStringA(const Str: RawByteString; ACodePage: LongWord = CP_ACP): AnsiString;
begin
    Result := ConvertCodepage(Str, CP_UTF8, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUnicodeToAnsi(const Str: UnicodeString; ACodePage: LongWord = CP_ACP): Boolean;
var
    Len : Integer;
    B   : LongBool;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := IcsWcToMb(ACodePage, 0, Pointer(Str), Len, nil, 0, nil, @B);
        { MS-docs: For the CP_UTF7 and CP_UTF8 settings for CodePage, parameter }
        { lpUsedDefaultChar must be set to NULL. Otherwise, the function fails  }
        { with ERROR_INVALID_PARAMETER.                                         }
        if (Len = 0) and (GetLastError = ERROR_INVALID_PARAMETER) then
            Result := IcsWcToMb(ACodePage, 0, Pointer(Str),
                                          Len, nil, 0, nil, nil) > 0
        else
            Result := (not B) and (Len > 0);
    end
    else
        Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns size of a UTF-8 byte sequence calculated from the UTF-8 lead byte   }
{ Returns "0" if LeadByte is not valid UTF-8 lead byte.                       }
function IcsUtf8Size(const LeadByte: Byte): Integer;
begin
    case LeadByte of
        $00..$7F : Result := 1;
        $C2..$DF : Result := 2;
        $E0..$EF : Result := 3;
        $F0..$F4 : Result := 4;
    else
        Result := 0; // Invalid lead byte
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUtf8LeadByte(const B: Byte): Boolean;
begin
    Result := (B < $80) or (B in [$C2..$F4]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUtf8TrailByte(const B: Byte): Boolean;
begin
    Result := B in [$80..$BF];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
function IsLeadChar(Ch: WideChar): Boolean;
begin
    Result := (Ch >= #$D800) and (Ch <= #$DFFF);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CharsetDetect(const Buf: Pointer; Len: Integer): TCharsetDetectResult;
var
    PEndBuf   : PByte;
    PBuf      : PByte;
    Byte2Mask : Byte;
    Ch        : Byte;
    Trailing  : Integer; // trailing (continuation) bytes to follow
begin
    PBuf        := Buf;
    PEndBuf     := Pointer(INT_PTR(Buf) + Len);
    Byte2Mask   := $00;
    Trailing    := 0;
    Result      := cdrAscii;
    while (PBuf <> PEndBuf) do
    begin
        Ch := PBuf^;
        Inc(PBuf);
        if Trailing <> 0 then
        begin
            if Ch and $C0 = $80 then // Does trailing byte follow UTF-8 format?
            begin
                if (Byte2Mask <> 0) then // Need to check 2nd byte for proper range?
                    if Ch and Byte2Mask <> 0 then // Are appropriate bits set?
                        Byte2Mask := 0
                    else begin
                        Result := cdrUnknown;
                        Exit;
                    end;
                Dec(Trailing);
                Result := cdrUtf8;
            end
            else begin
                Result := cdrUnknown;
                Exit;
            end;
        end
        else begin
            if Ch and $80 = 0 then
                Continue                      // valid 1 byte UTF-8
            else if Ch and $E0 = $C0 then     // valid 2 byte UTF-8
            begin
                if Ch and $1E <> 0 then       // Is UTF-8 byte in proper range?
                    Trailing := 1
                else begin
                    Result := cdrUnknown;
                    Exit;
                end;
            end
            else if Ch and $F0 = $E0 then     // valid 3 byte UTF-8
            begin
                if Ch and $0F = 0 then        // Is UTF-8 byte in proper range?
                    Byte2Mask := $20;         // If not set mask to check next byte
                Trailing := 2;
            end
            else if Ch and $F8 = $F0 then     // valid 4 byte UTF-8
            begin
                if Ch and $07 = 0 then        // Is UTF-8 byte in proper range?
                    Byte2Mask := $30;         // If not set mask to check next byte
                Trailing := 3;
            end
          { 4 byte is the maximum today, see ISO 10646, so let's break here }
          { else if Ch and $FC = $F8 then     // valid 5 byte UTF-8
            begin
                if Ch and $03 = 0 then        // Is UTF-8 byte in  proper range?
                    Byte2Mask := $38;         // If not set mask to check next byte
                Trailing := 4;
            end
            else if Ch and $FE = $FC then     // valid 6 byte UTF-8
            begin
                if ch and $01 = 0 then        // Is UTF-8 byte in proper range?
                    Byte2Mask := $3C;         // If not set mask to check next byte
                Trailing := 5;
            end}
            else begin
                Result := cdrUnknown;
                Exit;
            end;
        end;
    end;// while

    case Result of
      cdrUtf8, cdrAscii : if Trailing <> 0 then Result := cdrUnknown;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CharsetDetect(const Str: RawByteString): TCharsetDetectResult;
begin
    Result := CharsetDetect(Pointer(Str), Length(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUtf8Valid(const Str: RawByteString): Boolean;
begin
    Result := CharSetDetect(Pointer(Str), Length(Str)) <> cdrUnknown;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUtf8Valid(const Buf: Pointer; Len: Integer): Boolean;
begin
    Result := CharSetDetect(Buf, Len) <> cdrUnknown;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The return value is a pointer to the preceding character in the string,   }
{ or to the first character in the string if the Current parameter equals   }
{ the Start parameter.                                                      }
function IcsCharPrevUtf8(const Start, Current: PAnsiChar): PAnsiChar;
var
    Cnt : Integer;
begin
    Cnt := 0;
    Result := Current;
    while (Result > Start) and (Cnt < MAX_UTF8_SIZE) do
    begin
        Dec(Result);
        if IsUtf8LeadByte(Byte(Result^)) then
            Break;
        Inc(Cnt);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCharNextUtf8(const Str: PAnsiChar): PAnsiChar;
var
    Cnt : Integer;
begin
    Result := Str;
    if (Result = nil) or (Result^ = #0) then
        Exit;
    for Cnt := 1 to MAX_UTF8_SIZE do
    begin
        Inc(Result);
        if (Result^ = #0) or IsUtf8LeadByte(Byte(Result^)) then
            Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This doesn't work with stateful charsets like true MBCS. Such as          }
{ iso-2022-xy or UTF-7                                                      }
function IcsStrNextChar(const Str: PAnsiChar;
  ACodePage: LongWord = CP_ACP): PAnsiChar;
begin
    if ACodePage = CP_ACP then
        IcsGetAcp(ACodePage);
    if ACodePage = CP_UTF8 then
        Result := IcsCharNextUtf8(Str)
    else
       (*
        Result := CharNextExA(Word(ACodePage), Str, 0);
        { From Mitch Kaplan's blog                                        }
        { http://blogs.msdn.com/michkap/archive/2007/04/19/2190207.aspx): }
        { Neither CharNextExA nor CharPrevExA are broken in any version   }
        { of Windows, but neither one was designed with UTF-8 in mind.    }
        {...                                                              }
        { It is completely dependent on the behavior of IsDBCSLeadByteEx, }
        { which is an NLS function that is (for obvious reasons) only     }
        { dealing with East Asian, DBCS code page.                        }

        { Comment: Poor design isn't it? IsDBCSLeadByteEx validates lead  }
        { byte values only in code pages 932, 936, 949, 950, and 1361.    }
       *)
        if (Str <> nil) and (Str^ <> #0) then
        begin
            if IcsIsDBCSLeadByte(Str^, ACodePage) and
              (PAnsiChar(Str + 2)^ <> #0) then
                Result := Str + 2
            else
                Result := Str + 1;
        end
        else
            Result := Str;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrPrevChar(const Start, Current: PAnsiChar;
  ACodePage: LongWord = CP_ACP): PAnsiChar;
begin
    if ACodePage = CP_ACP then
        IcsGetAcp(ACodePage);
    if ACodePage = CP_UTF8 then
        Result := IcsCharPrevUtf8(Start, Current)
    else begin
        Result := Current;
        if Result - 1 >= Start then
        begin
            Dec(Result);
            if (Result - 1 >= Start) and
               IcsIsDBCSLeadByte(PAnsiChar(Result - 1)^, ACodePage) then
                Dec(Result);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrCharLength(const Str: PAnsiChar;
  ACodePage: LongWord = CP_ACP): Integer;
begin
    Result := INT_PTR(IcsStrNextChar(Str, ACodePage)) - INT_PTR(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsNextCharIndex(const S: RawByteString; Index: Integer;
  ACodePage: LongWord = CP_ACP): Integer;
begin
    Assert((Index > 0) and (Index <= Length(S)));
    Result := Index + 1;
    if (ACodePage = CP_ACP) and not (S[Index] in LeadBytes) then
        Exit;
    Result := Index + IcsStrCharLength(PAnsiChar(S) + Index - 1, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function XDigit(Ch : WideChar) : Integer;
begin
    case Ch of
        '0'..'9' : Result := Ord(Ch) - Ord('0');
    else
        Result := (Ord(Ch) and 15) + 9;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function XDigit(Ch : AnsiChar) : Integer;
begin
    case Ch of
        '0'..'9' : Result := Ord(Ch) - Ord('0');
    else
        Result := (Ord(Ch) and 15) + 9;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function XDigit2(S : PChar) : Integer;
begin
    Result := 16 * XDigit(S[0]) + XDigit(S[1]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsXDigit(Ch : WideChar) : Boolean;
begin
    Result := ((Ch >= '0') and (Ch <= '9')) or
              ((Ch >= 'a') and (Ch <= 'f')) or
              ((Ch >= 'A') and (Ch <= 'F'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsXDigit(Ch : AnsiChar) : Boolean;
begin
    Result := ((Ch >= '0') and (Ch <= '9')) or
              ((Ch >= 'a') and (Ch <= 'f')) or
              ((Ch >= 'A') and (Ch <= 'F'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoin(Value : PWideChar; Len : Integer) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 0;
    while (I < Len) and (Value[I] = ' ') do
        I := I + 1;
    while (I < len) and (IsXDigit(Value[I])) do begin
        Result := Result * 16 + XDigit(Value[I]);
        I := I + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoin(Value : PAnsiChar; Len : Integer) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 0;
    while (I < Len) and (Value[I] = ' ') do
        I := I + 1;
    while (I < len) and (IsXDigit(Value[I])) do begin
        Result := Result * 16 + XDigit(Value[I]);
        I := I + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoi2(Value : PWideChar) : Integer;
begin
    Result := htoin(Value, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoi2(Value : PAnsiChar) : Integer;
begin
    Result := htoin(Value, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    HexTable : array[0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

function IcsBufferToHex(const Buf; Size: Integer; Separator: Char): String;
const
    Fact = 3;
var
    I : Integer;
    P : PChar;
    B : PAnsiChar;
begin
    if Size <= 0 then
        Result := ''
    else begin
        SetLength(Result, (Fact * Size) - 1);
        P := PChar(Result);
        B := @Buf;
        for I := 0 to Size -1 do begin
            P[I * Fact]     := HexTable[(Ord(B[I]) shr 4) and 15];
            P[I * Fact + 1] := HexTable[Ord(B[I]) and 15];
            if (I < Size -1) then
                P[I * Fact + 2] := Separator;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsBufferToHex(const Buf; Size: Integer): String;
const
    Fact = 2;
var
    I : Integer;
    P : PChar;
    B : PAnsiChar;
begin
    if Size <= 0 then
        Result := ''
    else begin
        SetLength(Result, (Fact * Size));
        P := PChar(Result);
        B := @Buf;
        for I := 0 to Size -1 do begin
            P[I * Fact]     := HexTable[(Ord(B[I]) shr 4) and 15];
            P[I * Fact + 1] := HexTable[Ord(B[I]) and 15];
        end;
    end;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCharInSysCharSet(Ch : WideChar; const ASet : TSysCharSet) : Boolean;
begin
    Result := (Ord(Ch) < 256) and (AnsiChar(Ch) in ASet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCharInSysCharSet(Ch : AnsiChar; const ASet : TSysCharSet) : Boolean;
begin
    Result := Ch in ASet;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : WideChar) : Boolean;
begin
    Result := (Ch >= '0') and (Ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : AnsiChar) : Boolean;
begin
    Result := Ch in ['0'..'9'];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpace(Ch : WideChar) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpace(Ch : AnsiChar) : Boolean;
begin
    Result := Ch in [' ', #9];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCRLF(Ch : WideChar) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCRLF(Ch : AnsiChar) : Boolean;
begin
    Result := Ch in [#10, #13];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpaceOrCRLF(Ch : WideChar) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9) or (Ch = #10) or (Ch = #13);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpaceOrCRLF(Ch : AnsiChar) : Boolean;
begin
    Result := Ch in [' ', #9, #10, #13];

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PWideChar) : PWideChar;
begin
    Result := PValue;
    while IsSpaceOrCRLF(Result^) do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PAnsiChar) : PAnsiChar;
begin
    Result := PValue;
    while IsSpaceOrCRLF(Result^) do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsNameThreadForDebugging(AThreadName: AnsiString; AThreadID: TThreadID);
{$IFNDEF COMPILER14_UP}
type
    TThreadNameInfo = record
        FType: LongWord;     // must be 0x1000
        FName: PAnsiChar;    // pointer to name (in user address space)
        FThreadID: LongWord; // thread ID (-1 indicates caller thread)
        FFlags: LongWord;    // reserved for future use, must be zero
    end;
var
    ThreadNameInfo: TThreadNameInfo;
begin
    if IsDebuggerPresent then
    begin
        ThreadNameInfo.FType := $1000;
        ThreadNameInfo.FName := PAnsiChar(AThreadName);
        ThreadNameInfo.FThreadID := AThreadID;
        ThreadNameInfo.FFlags := 0;
        try
            RaiseException($406D1388, 0,
                  SizeOf(ThreadNameInfo) div SizeOf(LongWord), @ThreadNameInfo);
        except
        end;
    end;
{$ELSE}
begin
    TThread.NameThreadForDebugging(AThreadName, AThreadID);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsNormalizeString(const S: UnicodeString;
  NormForm: TIcsNormForm): UnicodeString;
{$IFDEF MSWINDOWS}
var
    Cnt   : Integer;
    Flags : DWORD;
begin
    Result := '';
    if S = '' then
        Exit;
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 6) then
    begin
        { Available since D7 in Windows.pas }
        case NormForm of
            icsNormalizationD  : Flags := MAP_COMPOSITE;
            icsNormalizationC  : Flags := MAP_PRECOMPOSED;
            icsNormalizationKD : Flags := MAP_FOLDCZONE;
            icsNormalizationKC : Flags := MAP_FOLDCZONE or MAP_COMPOSITE;
            else
              Result := S;
              Exit;
        end;
        Cnt := FoldStringW(Flags, PWideChar(S), Length(S), nil, 0);
        if Cnt > 0 then
        begin
            SetLength(Result, Cnt);
            Cnt := FoldStringW(Flags, PWideChar(S), Length(S),
                               PWideChar(Result), Cnt);
            SetLength(Result, Cnt);
        end;
    end
    else begin
        { Vista+, not yet available in Windows.pas }
        if IsNormalizedString(TNormForm(NormForm), PWideChar(S), Length(S)) then
            Result := S
        else begin
            Cnt := NormalizeString(TNormForm(NormForm), PWideChar(S),
                                   Length(S), nil, 0);
            if Cnt > 0 then
            begin
                SetLength(Result, Cnt);
                Cnt := NormalizeString(TNormForm(NormForm), PWideChar(S),
                                       Length(S), PWideChar(Result), Cnt);
                SetLength(Result, Cnt);
            end;
        end;
    end;
{$ELSE MSWINDOWS}

{$IFDEF MACOS}
    function CFStringToStr(StringRef: CFStringRef): string;
    var
        Range: CFRange;
    begin
        if StringRef = nil then Exit('');
        Range := CFRangeMake(0, CFStringGetLength(StringRef));
        if Range.Length > 0 then
        begin
            SetLength(Result, Range.Length);
            CFStringGetCharacters(StringRef, Range, @Result[1]);
        end
        else
            Result := EmptyStr;
    end;

var
    MutableStringRef        : CFMutableStringRef;
    kCFStringNormalization  : Integer;
begin
    Result := '';
    if S = '' then
        Exit;
    case NormForm of
        icsNormalizationD  :
            kCFStringNormalization := kCFStringNormalizationFormD;
        icsNormalizationC  :
            kCFStringNormalization := kCFStringNormalizationFormC;
        icsNormalizationKD :
            kCFStringNormalization := kCFStringNormalizationFormKD;
        icsNormalizationKC :
            kCFStringNormalization := kCFStringNormalizationFormKC;
      else
          Result := S;
          Exit;
    end;
    MutableStringRef := CFStringCreateMutable(kCFAllocatorDefault, 0);
    try
        CFStringAppendCharacters(MutableStringRef, PWideChar(S), Length(S));
        CFStringNormalize(MutableStringRef, kCFStringNormalization);
        Result := CFStringToStr(CFStringRef(MutableStringRef));
    finally
        CFRelease(MutableStringRef);
    end;
{$ELSE MACOS}
    raise Exception.Create('Not implemented');
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCryptGenRandom(var Buf; BufSize: Integer): Boolean;
{$IFDEF MSWINDOWS}
var
    F_Acquire  : function (var hProv: THandle; pszContainer: PWideChar;
        pszProvider: PWideChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
    F_Gen      : function (hProv: THandle; dwLen: DWORD; pbBuffer: PByte): BOOL; stdcall;
    F_Release  : function (hProv: THandle; dwFlags: ULONG_PTR): BOOL; stdcall;

    hLib       : HMODULE;
    hCryptProv : THandle;
begin
    Result := False;
    hLib := LoadLibrary(advapi32);
    if hLib <> 0 then
    begin
        @F_Acquire := GetProcAddress(hLib, 'CryptAcquireContextW');
        @F_Gen     := GetProcAddress(hLib, 'CryptGenRandom');
        @F_Release := GetProcAddress(hLib, 'CryptReleaseContext');
        if (@F_Acquire <> nil) and (@F_Gen <> nil) and (@F_Release <> nil) then
        begin
            // PROV_RSA_FULL = 1; CRYPT_VERIFYCONTEXT  = DWORD($F0000000);
            if F_Acquire(hCryptProv, nil, nil, 1, DWORD($F0000000)) then
            begin
                Result := F_Gen(hCryptProv, BufSize, @Buf);
                F_Release(hCryptProv, 0);
            end;
        end;
        FreeLibrary(hLib);
    end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
begin
    Result := False; // ToDo
end;
{$ENDIF MACOS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RandSeed32: LongWord;
const
    DEFAULT_SEED32 = 2463534242;
var
    I   : Integer;
    Buf : Int64;
    Md  : TMD5Digest;
    Ctx : TMD5Context;
begin
    MD5DigestInit(Md);
    MD5Init(Ctx);

{$IFDEF MSWINDOWS}
    if QueryPerformanceCounter(Buf) then
        MD5Update(Ctx, Buf, SizeOf(Buf))
    else begin
        Buf := GetTickCount;
        MD5Update(Ctx, Buf, SizeOf(Buf));
    end;
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
    Buf := AbsoluteToNanoseconds(UpTime);
    MD5Update(Ctx, Buf, SizeOf(Buf));
{$ENDIF MACOS}
    { Add eight additional cryptographically random bytes }
    if IcsCryptGenRandom(Buf, SizeOf(Buf)) then // So far Win only
        MD5Update(Ctx, Buf, SizeOf(Buf));
    MD5Final(Md, Ctx);
    for I := Low(Md) to High(Md) - SizeOf(LongWord) do
    begin
        Result := PLongWord(@Md[I])^;
        if Result <> 0 then
            Exit;
    end;
    Result := DEFAULT_SEED32;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    GSeed32 : LongWord = 0;

{ This makes us independent from System.Random() and we do no longer screw  }
{ up RTL's global var RandSeed by various calls to Randomize.               }
{ The PRNG below is a XorShift RNG by George Marsaglia.                     }
{ It uses one of his favorite choices, [a, b, c] = [13, 17, 5], and will    }
{ pass almost all tests of randomness, except the binary rank test in       }
{ Diehard. It is much better than System.Random() however as thread-        }
{ unsafe as System.Random() is. See also PrngTst.dpr in MiscDemos.    AG    }
function IcsRandomInt(const ARange: Integer): Integer;
var
    x : LongWord;
begin
    if GSeed32 = 0 then
        x := RandSeed32  // MUST be <> 0
    else
        x := GSeed32;
    x := x xor (x shl 13);
    x := x xor (x shr 17);
    x := x xor (x shl 5);
    GSeed32 := x;
    Result := (UInt64(LongWord(ARange)) * UInt64(x)) shr 32;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileUtcModified(const FileName : String) : TDateTime;
var
    SearchRec : TSearchRec;
    Status    : Integer;
{$IFDEF POSIX}
    LUT       : tm;
{$ENDIF}
{$IFDEF MSWINDOWS}
    LInt64    : Int64;
const
    FileTimeBase = -109205.0;   // days between years 1601 and 1900
    FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nsec per Day
{$ENDIF}
begin
    Status := FindFirst(FileName, faAnyFile, SearchRec);
    try
        if Status <> 0 then
            Result := 0
        else begin
          {$IFDEF MSWINDOWS}
            Move(SearchRec.FindData.ftLastWriteTime, LInt64, SizeOf(LInt64));
            Result := (LInt64 / FileTimeStep) + FileTimeBase;
          {$ENDIF}
          {$IFDEF POSIX}
            gmtime_r(SearchRec.Time, LUT);
            Result := EncodeDate(LUT.tm_year + 1900, LUT.tm_mon + 1, LUT.tm_mday) +
                      EncodeTime(LUT.tm_hour, LUT.tm_min, LUT.tm_sec, 0);
          {$ENDIF}
        end;
    finally
        FindClose(SearchRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsInterlockedCompareExchange(
    var Destination : Pointer;
    Exchange        : Pointer;
    Comperand       : Pointer): Pointer;
begin
{$IFDEF COMPILER12_UP}
  {$IFDEF COMPILER16_UP}
    Result := TInterlocked.CompareExchange(Destination, Exchange, Comperand);
  {$ELSE}
    Result := InterlockedCompareExchangePointer(Destination, Exchange, Comperand);
  {$ENDIF}
{$ELSE}
  {$IFDEF COMPILER10_UP} // Possibly even COMPILER9_UP - Delphi 2005?
    Result := Pointer(InterlockedCompareExchange(Integer(Destination),
                                        Integer(Exchange), Integer(Comperand)));
  {$ELSE} { Delphi 7 }
    Result := InterlockedCompareExchange(Destination, Exchange, Comperand);
  {$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsFindCloseW(var F: TIcsSearchRecW);
begin
{$IFDEF COMPILER12_UP}
    {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FindClose(F);
{$ELSE}
    if F.FindHandle <> INVALID_HANDLE_VALUE then
    begin
        Windows.FindClose(F.FindHandle);
        F.FindHandle := INVALID_HANDLE_VALUE;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
function IcsFindMatchingFileW(var F: TIcsSearchRecW): Integer;
var
    LocalFileTime : TFileTime;
begin
    with F do
    begin
        while FindData.dwFileAttributes and ExcludeAttr <> 0 do
            if not FindNextFileW(FindHandle, FindData) then
        begin
            Result := GetLastError;
            Exit;
        end;
        FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
        FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
        Size := FindData.nFileSizeLow;
        Attr := FindData.dwFileAttributes;
        Name := FindData.cFileName;
    end;
    Result := 0;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFindNextW(var F: TIcsSearchRecW): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FindNext(F);
{$ELSE}
    if FindNextFileW(F.FindHandle, F.FindData) then
        Result := IcsFindMatchingFileW(F)
    else
        Result := GetLastError;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFindFirstW(const Path: UnicodeString; Attr: Integer;
  var  F: TIcsSearchRecW): Integer;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FindFirst(Path, Attr, F);
{$ELSE}
const
    faSpecial = faHidden or faSysFile or faDirectory;
begin
    F.ExcludeAttr := not Attr and faSpecial;
    F.FindHandle := FindFirstFileW(PWideChar(Path), F.FindData);
    if F.FindHandle <> INVALID_HANDLE_VALUE then
    begin
        Result := IcsFindMatchingFileW(F);
        if Result <> 0 then IcsFindCloseW(F);
    end else
        Result := GetLastError;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFindFirstW(const Utf8Path: UTF8String; Attr: Integer;
  var  F: TIcsSearchRecW): Integer;
begin
    Result := IcsFindFirstW(AnsiToUnicode(Utf8Path, CP_UTF8), Attr, F);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCreateDirW(const Dir: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.CreateDir(Dir);
{$ELSE}
    Result := CreateDirectoryW(PWideChar(Dir), nil);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCreateDirW(const Utf8Dir: UTF8String): Boolean; overload;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.CreateDir(Utf8Dir);
{$ELSE}
    Result := CreateDirectoryW(PWideChar(AnsiToUnicode(Utf8Dir, CP_UTF8)), nil);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsForceDirectoriesW(Dir: UnicodeString): Boolean;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ForceDirectories(Dir);
{$ELSE}
var
    E: EInOutError;
begin
    Result := True;
    if Length(Dir) = 0 then
    begin
        E := EInOutError.CreateRes(@SCannotCreateDir);
        E.ErrorCode := 3;
        raise E;
    end;
    Dir := IcsExcludeTrailingPathDelimiterW(Dir);
    if (Length(Dir) < 3) or IcsDirExistsW(Dir)
        or (IcsExtractFilePathW(Dir) = Dir) then Exit;
    Result := IcsForceDirectoriesW(IcsExtractFilePathW(Dir)) and IcsCreateDirW(Dir);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsForceDirectoriesW(Utf8Dir: UTF8String): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ForceDirectories(Utf8Dir);
{$ELSE}
    Result := IcsForceDirectoriesW(AnsiToUnicode(Utf8Dir, CP_UTF8));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDirExistsW(const FileName: PWideChar): Boolean;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DirectoryExists(UnicodeString(FileName));
{$ELSE}
var
    Res : DWord;
begin
    Res := GetFileAttributesW(FileName);
    Result := (Res <> INVALID_HANDLE_VALUE) and
              ((Res and FILE_ATTRIBUTE_DIRECTORY) <> 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDirExistsW(const FileName: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DirectoryExists(FileName);
{$ELSE}
    Result := IcsDirExistsW(PWideChar(FileName));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDirExistsW(const Utf8FileName: UTF8String): Boolean; overload;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DirectoryExists(Utf8FileName);
{$ELSE}
    Result := IcsDirExistsW(PWideChar(AnsiToUnicode(Utf8FileName, CP_UTF8)));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function RtlCompareUnicodeString(String1, String2: PUnicode_String;
  CaseInSensitive: Boolean): LongInt; stdcall;
begin
    { Supported OS: NT4 and better! }
    if _RtlCompareUnicodeString = nil then
    begin
        if hNtDll = 0 then
        begin
            hNtDll := GetModuleHandle('ntdll.dll');
            if hNtDll = 0 then
                RaiseLastOsError;
        end;
        _RtlCompareUnicodeString := GetProcAddress(hNtDll, 'RtlCompareUnicodeString');
        if _RtlCompareUnicodeString = nil then
            RaiseLastOsError;
    end;
    Result := TRtlCompareUnicodeString(_RtlCompareUnicodeString)(
                                        String1, String2, CaseInsensitive);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Borrowed from Jordan Russell }
function IcsStrCompOrdinalW(Str1: PWideChar; Str1Length: Integer;
  Str2: PWideChar; Str2Length: Integer; IgnoreCase: Boolean): Integer;
var
    S1, S2: TUnicode_String;
    Len: Integer;
begin
    S1.Buffer := Str1;
    S2.Buffer := Str2;
    while True do
    begin
        if Str1Length <= Str2Length then
            Len := Str1Length
        else
            Len := Str2Length;
        if Len <= 0 then
            Break;
        // Can only process 32K characters at a time
        if Len > $7FF0 then
            Len := $7FF0;

        S1.Length        := Len * 2;   // Length is in bytes
        S1.MaximumLength := S1.Length;
        S2.Length        := S1.Length;
        S2.MaximumLength := S1.Length;
        Result := RtlCompareUnicodeString(@S1, @S2, IgnoreCase);
        if Result <> 0 then
            Exit;

        Dec(Str1Length, Len);
        Dec(Str2Length, Len);
        Inc(S1.Buffer, Len);
        Inc(S2.Buffer, Len);
    end;
    Result := Str1Length - Str2Length;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsAnsiCompareFileNameW(const S1, S2: UnicodeString): Integer;
begin
{$IFDEF MSWINDOWS}
    Result := IcsStrCompOrdinalW(PWideChar(S1), Length(S1), PWideChar(S2),
                             Length(S2), True);
{$ELSE}
    Result := SysUtils.AnsiCompareFileName(S1, S2);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsAnsiCompareFileNameW(const Utf8S1, Utf8S2: UTF8String): Integer;
begin
    Result := IcsAnsiCompareFileNameW(AnsiToUnicode(Utf8S1, CP_UTF8),
                                      AnsiToUnicode(Utf8S2, CP_UTF8));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrAllocW(Len: Cardinal): PWideChar;
begin
    Len := (Len * 2) + 4;
    GetMem(Result, Len);
    FillChar(Result^, Len, #0);
    Cardinal(Pointer(Result)^) := Len;
    Inc(Result, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrScanW(const Str: PWideChar; Ch: WideChar): PWideChar;
begin
    Result := Str;
    while Result^ <> Ch do
    begin
        if Result^ = #0 then
        begin
            Result := nil;
            Exit;
        end;
        Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsDelimiterW(const Delimiters: PWideChar;
  S : UnicodeString; Index: Integer): Boolean;
begin
    Result := False;
    if (Index <= 0) or (Index > Length(S)) then
        Exit;
    Result := IcsStrScanW(Delimiters, S[Index]) <> nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLastDelimiterW(const Delimiters: PWideChar;
  S: UnicodeString): Integer;
begin
    Result := Length(S);
    while Result >= 0 do
    begin
        if (S[Result] <> #0) and (IcsStrScanW(Delimiters, S[Result]) <> nil) then
            Exit;
        Dec(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMakeLong(L, H: Word): Longint;
begin
    Result := L or H shl 16;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMakeWord(L, H: Byte): Word;
begin
    Result := L or H shl 8;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsHiWord(LW: LongWord): Word;
begin
    Result := LW shr 16;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLoWord(LW: LongWord): Word;
begin
    Result := Word(LW);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsHiByte(W: Word): Byte;
begin
    Result := W shr 8;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLoByte(W: Word): Byte;
begin
    Result := Byte(W);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsCheckOSError(ALastError: Integer);
var
    Error: EOSError;
begin
    if ALastError <> 0 then begin
        Error := EOSError.CreateResFmt(@SOSError, [ALastError,
                                       SysErrorMessage(ALastError)]);
        Error.ErrorCode := ALastError;
        raise Error;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Needs optimization!      }
{ It's a bit slower that the RTL routine.        }
{ We should realy use a FastCode function here.  }
function IntToStrA(N : Integer) : AnsiString;
var
    I : Integer;
    Buf : array [0..11] of AnsiChar;
    Sign : Boolean;
begin
    if N >= 0 then
        Sign := FALSE
    else begin
        Sign := TRUE;
        if N = Low(Integer) then
        begin
            Result := '-2147483648';
            Exit;
        end
        else
            N := Abs(N);
    end;
    I := Length(Buf);
    repeat
        Dec(I);
        Buf[I] := AnsiChar(N mod 10 + $30);
        N := N div 10;
    until N = 0;
    if Sign then begin
        Dec(I);
        Buf[I] := '-';
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIntToStrA(N : Integer) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToStrA(N);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToStr(N);
{$ELSE}
    Result := IntToStrA(N);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IntToHexA(N : Integer; Digits: Byte) : AnsiString;
var
    Buf : array [0..7] of Byte;
    V : Cardinal;
    I : Integer;
begin
    V := Cardinal(N);
    I := Length(Buf);
    if Digits > I then Digits := I;
    repeat
        Dec(I);
        Buf[I] := V mod 16;
        if Buf[I] < 10 then
            Inc(Buf[I], $30)
        else
            Inc(Buf[I], $37);
        V := V div 16;
    until V = 0;
    while Digits > Length(Buf) - I do begin
       Dec(I);
       Buf[I] := $30;
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToHexA(N, Digits);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToHex(N, Digits);
{$ELSE}
    Result := IntToHexA(N, Digits);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Feel free to optimize!                }
{ It's a bit faster than the RTL routine.                     }
function IcsTrimA(const Str: AnsiString): AnsiString;
var
    I, L : Integer;
begin
    L := Length(Str);
    I := 1;
    while (I <= L) and (Str[I] <= ' ') do
        Inc(I);
    if I > L then
        Result := ''
    else begin
        while Str[L] <= ' ' do
            Dec(L);
        SetLength(Result, L - I + 1);
        Move(Str[I], Pointer(Result)^, L - I + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsTrim(const Str: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsTrimA(Str);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.Trim(Str);
{$ELSE}
    Result := IcsTrimA(Str);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsTrim(const Str : UnicodeString) : UnicodeString;
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.Trim(Str);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsLowerCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I  : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['A'..'Z'] then Inc(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLowerCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsLowerCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.LowerCase(S);
{$ELSE}
    Result := IcsLowerCaseA(S);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsLowerCase(const S: UnicodeString): UnicodeString;
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.LowerCase(S);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsUpperCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['a'..'z'] then Dec(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsUpperCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsUpperCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.UpperCase(S);
{$ELSE}
    Result := IcsUpperCaseA(S);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsUpperCase(const S: UnicodeString): UnicodeString;
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.UpperCase(S);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSameTextA(const S1, S2: AnsiString): Boolean;
begin
    Result := (IcsCompareTextA(S1, S2) = 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsCompareTextA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen : Integer;
    Ch1, Ch2 : AnsiChar;
    P1, P2 : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 0 to MinLen -1 do
    begin
        Ch1 := P1[I];
        Ch2 := P2[I];
        if (Ch1 <> Ch2) then
        begin
            { Strange, but this is how the original works, }
            { for instance, "a" is smaller than "[" .      }
            if (Ch1 > Ch2) then
            begin
                if Ch1 in ['a'..'z'] then
                    Dec(Byte(Ch1), 32);
            end
            else begin
                if Ch2 in ['a'..'z'] then
                    Dec(Byte(Ch2), 32);
            end;
        end;
        if (Ch1 <> Ch2) then
        begin
            Result := Byte(Ch1) - Byte(Ch2);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCompareText(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareTextA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareText(S1, S2);
{$ELSE}
    Result := IcsCompareTextA(S1, S2);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsCompareText(const S1, S2: UnicodeString): Integer;
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.CompareText(S1, S2);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCompareStrA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen    : Integer;
    P1, P2    : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 0 to MinLen -1 do
    begin
        if (P1[I] <> P2[I]) then
        begin
            Result := Ord(P1[I]) - Ord(P2[I]);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCompareStr(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareStrA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareStr(S1, S2);
{$ELSE}
    Result := IcsCompareStrA(S1, S2);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsCompareStr(const S1, S2: UnicodeString): Integer;
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.CompareStr(S1, S2);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrLen(const Str: PAnsiChar): Cardinal;
begin
{$IFDEF COMPILER18_UP}
  Result := System.AnsiStrings.StrLen(Str);
{$ELSE}
  Result := StrLen(Str);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsStrLen(const Str: PWideChar): Cardinal;
begin
  Result := StrLen(Str);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrPas(const Str: PAnsiChar): AnsiString;
begin
  Result := Str;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsStrPas(const Str: PWideChar): string;
begin
  Result := Str;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
{$IFDEF COMPILER18_UP}
  Result := System.AnsiStrings.StrCopy(Dest, Source);
{$ELSE}
  Result := StrCopy(Dest, Source);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  Result := StrCopy(Dest, Source);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrPCopy(Dest: PChar; const Source: string): PChar;
begin
  Result := StrLCopy(Dest, PChar(Source), Length(Source));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsStrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;
begin
{$IFDEF COMPILER18_UP}
  Result := System.AnsiStrings.StrLCopy(Dest, PAnsiChar(Source), Length(Source));
{$ELSE}
  Result := StrLCopy(Dest, PAnsiChar(Source), Length(Source));
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrPLCopy(Dest: PChar; const Source: String; MaxLen: Cardinal): PChar;
begin
  Result := StrPLCopy(Dest, PChar(Source), MaxLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsStrPLCopy(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar;
begin
{$IFDEF COMPILER18_UP}
  Result := System.AnsiStrings.StrLCopy(Dest, PAnsiChar(Source), MaxLen);
{$ELSE}
  Result := StrLCopy(Dest, PAnsiChar(Source), MaxLen);
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractFilePathW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExtractFilePath(FileName);
{$ELSE}
var
    I: Integer;
begin
    I := IcsLastDelimiterW(IcsPathDriveDelimW, FileName);
    Result := Copy(FileName, 1, I);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractFileDirW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExtractFileDir(FileName);
{$ELSE}
var
    I: Integer;
begin
    I := IcsLastDelimiterW(IcsPathDriveDelimW, Filename);
    if (I > 1) and (FileName[I] = IcsPathDelimW) and
    (not IcsIsDelimiterW(IcsPathDriveDelimW, FileName, I - 1)) then
      Dec(I);
    Result :=Copy(FileName, 1, I);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractFileDriveW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExtractFileDrive(FileName);
{$ELSE}
var
    I, J: Integer;
    Len : Integer;
begin
    Len := Length(FileName);
    if (Len >= 2) and (FileName[2] = DriveDelim) then
        Result := Copy(FileName, 1, 2)
    else if (Len >= 2) and (FileName[1] = PathDelim) and
            (FileName[2] = PathDelim) then
    begin
        J := 0;
        I := 3;
        while (I < Len) and (J < 2) do
        begin
            if FileName[I] = PathDelim then
                Inc(J);
            if J < 2 then
                Inc(I);
        end;
        if FileName[I] = PathDelim then
            Dec(I);
        Result := Copy(FileName, 1, I);
    end
    else
        Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractFileNameW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExtractFileName(FileName);
{$ELSE}
var
    I: Integer;
begin
    I := IcsLastDelimiterW(IcsPathDriveDelimW, FileName);
    Result := Copy(FileName, I + 1, MaxInt);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractFileExtW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExtractFileExt(FileName);
{$ELSE}
const
    Delim : PWideChar = '.\:';
var
    I: Integer;
begin
    I := IcsLastDelimiterW(Delim, FileName);
    if (I > 0) and (FileName[I] = '.') then
        Result := Copy(FileName, I, MaxInt)
    else
        Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractNameOnlyW(FileName: UnicodeString): UnicodeString; // angus
var
    I: Integer;

    function IsPathSep (Ch: WideChar): Boolean;
    begin
        Result := (Ch = IcsPathDelimW)
          {$IFDEF MSWINDOWS} or (Ch = IcsDriveDelimW) {$ENDIF} or (Ch = '.');
    end;

begin
    FileName := IcsExtractFileNameW (FileName);  // remove path
    I := Length(FileName);
    while (I > 0) and not (IsPathSep (FileName[I])) do Dec(I);  // find .
    if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
    Result := Copy(FileName, 1, I - 1) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsChangeFileExtW(const FileName, Extension: UnicodeString): UnicodeString;  // angus
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ChangeFileExt(FileName, Extension);
{$ELSE}
const
    Delim : PWideChar = '.\:';
var
    I: Integer;
begin
    I := IcsLastDelimiterW(Delim, Filename);
    if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
    Result := Copy(FileName, 1, I - 1) + Extension;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsStrLenW(Str: PWideChar): Cardinal;
var
    BeginP : Pointer;
begin
    Result := 0;
    if Str <> nil then
    begin
        BeginP := Str;
        while Str^ <> #0 do
            Inc(Str);
        Result := (INT_PTR(Str) - INT_PTR(BeginP)) div 2;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExpandFileNameW(const FileName: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExpandFileName(FileName);
{$ELSE}
var
    Name: PWideChar;
    Buf: array[0..MAX_PATH - 1] of WideChar;
begin
    if GetFullPathNameW(PWideChar(FileName), Length(Buf), @Buf[0], Name) > 0 then
    begin
        SetLength(Result, IcsStrLenW(Buf));
        Move(Buf, Result[1], IcsStrLenW(Buf) * 2);
    end
    else
        Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIncludeTrailingPathDelimiterW(const S : UnicodeString): UnicodeString;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.IncludeTrailingPathDelimiter(S);
{$ELSE}
    if (Length(S) > 0) and (S[Length(S)] <> IcsPathDelimW) then
        Result := S + IcsPathDelimW
    else
        Result := S;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExcludeTrailingPathDelimiterW(const S : UnicodeString): UnicodeString;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ExcludeTrailingPathDelimiter(S);
{$ELSE}
    Result := S;
    if (Length(S) > 0) and (S[Length(S)] = IcsPathDelimW) then
        SetLength(Result, Length(Result) -1);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDeleteFileW(const FileName: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DeleteFile(FileName);
{$ELSE}
    Result := Windows.DeleteFileW(PWideChar(FileName));
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractLastDir (const Path: RawByteString): RawByteString ;    // angus
var
    I, Len: integer;
begin
    Len := Length (Path);
    if Path [Len] = IcsPathDelimA then Dec (Len) ;
    for I := Len downto 1 do begin
        if Path [I] = IcsPathDelimA then begin
            Result := Copy (Path, I + 1, Len - I);
            exit;
        end;
    end;
    Result := Copy (Path, 1, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractLastDir (const Path: UnicodeString): UnicodeString ;     // angus
var
    I, Len: integer;
begin
    Len := Length (Path);
    if Path [Len] = IcsPathDelimW then Dec (Len) ;
    for I := Len downto 1 do begin
        if Path [I] = IcsPathDelimW then begin
            Result := Copy (Path, I + 1, Len - I);
            exit;
        end;
    end;
    Result := Copy (Path, 1, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDeleteFileW(const Utf8FileName: UTF8String): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DeleteFile(Utf8FileName);
{$ELSE}
    Result := Windows.DeleteFileW(PWideChar(AnsiToUnicode(Utf8FileName, CP_UTF8)));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function IcsFileGetAttrW(const FileName: UnicodeString): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileGetAttr(FileName);
{$ELSE}
    Result := GetFileAttributesW(PWideChar(FileName));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileGetAttrW(const Utf8FileName: UTF8String): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileGetAttr(Utf8FileName);
{$ELSE}
    Result := GetFileAttributesW(PWideChar(AnsiToUnicode(Utf8FileName, CP_UTF8)));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileSetAttrW(const FileName: UnicodeString; Attr: Integer): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileSetAttr(FileName, Attr);
{$ELSE}
    Result := 0;
    if not SetFileAttributesW(PWideChar(FileName), Attr) then
        Result := GetLastError;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileSetAttrW(const Utf8FileName: UTF8String; Attr: Integer): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileSetAttr(Utf8FileName, Attr);
{$ELSE}
    Result := 0;
    if not SetFileAttributesW(PWideChar(AnsiToUnicode(Utf8FileName, CP_UTF8)), Attr) then
        Result := GetLastError;
{$ENDIF}
end;
{$ENDIF MSWINDOWS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileCreateW(const FileName: UnicodeString):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileCreate(FileName);
{$ELSE}
    Result := Integer(CreateFileW(PWideChar(FileName),
                                  GENERIC_READ or GENERIC_WRITE,
                                  0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileCreateW(const Utf8FileName: UTF8String):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileCreate(Utf8FileName);
{$ELSE}
    Result := IcsFileCreateW(AnsiToUnicode(Utf8FileName, CP_UTF8));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileCreateW(const FileName: UnicodeString; Rights: LongWord):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileCreate(FileName, Rights);
{$ELSE}
    Result := IcsFileCreateW(FileName);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileCreateW(const Utf8FileName: UTF8String; Rights: LongWord):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileCreate(Utf8FileName, Rights);
{$ELSE}
    Result := IcsFileCreateW(AnsiToUnicode(Utf8FileName, CP_UTF8));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileOpenW(const FileName: UnicodeString; Mode: LongWord):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileOpen(FileName, Mode);
{$ELSE}
const
    AccessMode: array[0..2] of LongWord = (
                                            GENERIC_READ,
                                            GENERIC_WRITE,
                                            GENERIC_READ or GENERIC_WRITE);
    ShareMode: array[0..4] of LongWord = (
                                            0,
                                            0,
                                            FILE_SHARE_READ,
                                            FILE_SHARE_WRITE,
                                            FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
    Result := -1;
    if ((Mode and 3) <= fmOpenReadWrite) and
       ((Mode and $F0) <= fmShareDenyNone) then
    Result := Integer(CreateFileW(PWideChar(FileName),
                      AccessMode[Mode and 3],
                      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL, 0));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function IcsFileOpenW(const Utf8FileName: UTF8String; Mode: LongWord):
  {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileOpen(Utf8FileName, Mode);
{$ELSE}
    Result := IcsFileOpenW(AnsiToUnicode(Utf8FileName, CP_UTF8), Mode);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRemoveDirW(const Dir: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.RemoveDir(Dir);
{$ELSE}
    Result := RemoveDirectoryW(PWideChar(Dir));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRemoveDirW(const Utf8Dir: UTF8String): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.RemoveDir(Utf8Dir);
{$ELSE}
    Result := RemoveDirectoryW(PWideChar(AnsiToUnicode(Utf8Dir, CP_UTF8)));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRenameFileW(const OldName, NewName: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.RenameFile(OldName, NewName);
{$ELSE}
    Result := MoveFileW(PWideChar(OldName), PWideChar(NewName));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRenameFileW(const Utf8OldName, Utf8NewName: UTF8String): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.RenameFile(Utf8OldName, Utf8NewName);
{$ELSE}
    Result := MoveFileW(PWideChar(AnsiToUnicode(Utf8OldName, CP_UTF8)),
                        PWideChar(AnsiToUnicode(Utf8NewName, CP_UTF8)));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileAgeW(const FileName: UnicodeString): Integer;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileAge(FileName);
{$ELSE}
var
    Handle        : THandle;
    FindData      : TWin32FindDataW;
    LocalFileTime : TFileTime;
begin
    Handle := FindFirstFileW(PWideChar(FileName), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
        Windows.FindClose(Handle);
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        begin
            FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
            if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
                LongRec(Result).Lo) then Exit;
        end;
    end;
    Result := -1;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileAgeW(const Utf8FileName: UTF8String): Integer;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileAge(Utf8FileName);
{$ELSE}
    Result := IcsFileAgeW(AnsiToUnicode(Utf8FileName, CP_UTF8));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileExistsW(const FileName: UnicodeString): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileExists(FileName);
{$ELSE}
    Result := IcsFileAgeW(FileName) <> -1;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFileExistsW(const Utf8FileName: UTF8String): Boolean;
begin
{$IFDEF COMPILER12_UP}
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileExists(Utf8FileName);
{$ELSE}
    Result := IcsFileAgeW(Utf8FileName) <> -1;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Note: despite the name, this is a full Unicode function changing non-ANSI characters }
function IcsAnsiLowerCaseW(const S: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.AnsiLowerCase(S);
{$ELSE}
var
    Len: Integer;
begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Note: despite the name, this is a full Unicode function changing non-ANSI characters }
function IcsAnsiUpperCaseW(const S: UnicodeString): UnicodeString;
{$IFDEF COMPILER12_UP}
begin
    Result := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.AnsiUpperCase(S);
{$ELSE}
var
    Len: Integer;
begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsFileStreamW }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsFileStreamW.Create(const FileName: UnicodeString; Mode: Word);
begin
{$IFDEF COMPILER12_UP}
    inherited Create(FileName, Mode);
    FFileName := FileName;
{$ELSE}
    Create(Filename, Mode, 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsFileStreamW.Create(const FileName: UnicodeString; Mode: Word;
  Rights: Cardinal);
begin
{$IFDEF COMPILER12_UP}
    inherited Create(FileName, Mode, Rights);
    FFileName := FileName;
{$ELSE}
    if Mode = fmCreate then
    begin
        inherited Create(IcsFileCreateW(FileName));
        if Cardinal(FHandle) = INVALID_HANDLE_VALUE then
        {$IFDEF COMPILER12_UP}
            raise Exception.CreateResFmt(@SFCreateErrorEx,
                                [ExpandFileName(FileName),
                                SysErrorMessage(GetLastError)]);
        {$ELSE}
            raise Exception.CreateResFmt(@SFCreateErrorEx,
                                [IcsExpandFileNameW(FileName),
                                SysErrorMessage(GetLastError)]);
        {$ENDIF}

    end
    else begin
        inherited Create(IcsFileOpenW(FileName, Mode));
        if Cardinal(FHandle) = INVALID_HANDLE_VALUE then
        {$IFDEF COMPILER12_UP}
            raise Exception.CreateResFmt(@SFCreateErrorEx,
                                [ExpandFileName(FileName),
                                SysErrorMessage(GetLastError)]);
        {$ELSE}
            raise Exception.CreateResFmt(@SFCreateErrorEx,
                                [IcsExpandFileNameW(FileName),
                                SysErrorMessage(GetLastError)]);
        {$ENDIF}
    end;
    FFileName := FileName;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsFileStreamW.Create(const Utf8FileName: UTF8String;
  Mode: Word);
begin
{$IFDEF COMPILER12_UP}
    FFileName := Utf8FileName;
    inherited Create(FFileName, Mode);
{$ELSE}
    Create(AnsiToUnicode(Utf8FileName, CP_UTF8), Mode, 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsFileStreamW.Create(const Utf8FileName: UTF8String; Mode: Word;
  Rights: Cardinal);
begin
{$IFDEF COMPILER12_UP}
    FFileName := Utf8FileName;
    inherited Create(FFileName, Mode, Rights);
{$ELSE}
    Create(AnsiToUnicode(Utf8FileName, CP_UTF8), Mode, Rights);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsFileStreamW.Destroy;
begin
{$IFNDEF COMPILER12_UP}
    if Integer(FHandle) >= 0 then
        FileClose(FHandle);
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsIntegerList }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.Add(Item: Integer): Integer;
begin
    Result := FList.Add(Pointer(Item));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsIntegerList.Clear;
begin
    FList.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsIntegerList.Create;
begin
    FList := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsIntegerList.Delete(Index: Integer);
begin
    FList.Delete(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsIntegerList.Destroy;
begin
    FreeAndNil(FList);
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.GetCount: Integer;
begin
    Result := FList.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.GetFirst: Integer;
begin
    Result := Integer(FList.First);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.GetLast: Integer;
begin
    Result := Integer(FList.Last);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.GetItem(Index: Integer): Integer;
begin
    Result := Integer(FList[Index]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsIntegerList.SetItem(Index: Integer; const Value: Integer);
begin
    FList[Index] := Pointer(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsIntegerList.IndexOf(Item: Integer): Integer;
begin
    Result := FList.IndexOf(Pointer(Item));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsIntegerList.Assign(Source: TIcsIntegerList);
var
    I: Integer;
begin
    Clear;
    if Assigned(Source) then
        for I := 0 to Source.Count -1 do
            Add(Source[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsCriticalSection.Create;
{$IFDEF POSIX}
var
    LAttr: pthread_mutexattr_t;
{$ENDIF}
begin
    inherited;
  {$IFDEF MSWINDOWS}
    InitializeCriticalSection(FSection);
  {$ENDIF}
  {$IFDEF POSIX}
    IcsCheckOSError(pthread_mutexattr_init(LAttr));
    IcsCheckOSError(pthread_mutexattr_settype(LAttr, PTHREAD_MUTEX_RECURSIVE));
    IcsCheckOSError(pthread_mutex_init(FSection, LAttr));
    pthread_mutexattr_destroy(LAttr);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsCriticalSection.Destroy;
begin
  {$IFDEF MSWINDOWS}
    DeleteCriticalSection(FSection);
  {$ENDIF}
  {$IFDEF POSIX}
    pthread_mutex_destroy(FSection);
  {$ENDIF}
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCriticalSection.Enter;
begin
  {$IFDEF MSWINDOWS}
    EnterCriticalSection(FSection);
  {$ENDIF}
  {$IFDEF POSIX}
    IcsCheckOSError(pthread_mutex_lock(FSection));
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCriticalSection.Leave;
begin
  {$IFDEF MSWINDOWS}
    LeaveCriticalSection(FSection);
  {$ENDIF}
  {$IFDEF POSIX}
    IcsCheckOSError(pthread_mutex_unlock(FSection));
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCriticalSection.TryEnter: Boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := TryEnterCriticalSection(FSection);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := pthread_mutex_trylock(FSection) = 0;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
