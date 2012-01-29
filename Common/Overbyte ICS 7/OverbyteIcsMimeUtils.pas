{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       Mime support routines (RFC2045).
Creation:     May 03, 2003  (Extracted from SmtpProt unit)
Version:      7.22
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2010 by François PIETTE
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

History:
May 03, 2003  V1.00 Initial release
Jun 19, 2003  V1.01 Fixed SplitQuotedPrintableString. Thanks to Arno Garrels
                    <arno.garrels@gmx.de>
Jan 12, 2004  V1.02 Marc HUBAUT <mhu@wanadoo.fr> fixed DoFileEncBase64 in case
                    of file size is a multple of 3.
May 31, 2004  V1.03 Used ICSDEFS.INC, added const with version and copyright
May 28, 2005  V1.04 Piotr Hellrayzer Dalek <enigmatical@interia.pl>
              added a fast quoted-printable encoder. Arno Garrels
              <arno.garrels@gmx.de> added some routines and fixed a bug in
              func. SplitQuotedPrintableString.
Jan 28, 2006  V1.05 Gerhard Rattinger fixed TSysCharSet for Delphi 3
Mar 26, 2006  V6.00 New version 6.00 started
Dec 14, 2006  V6.01 Updated Base64Decode to ignore CR and LF
Jul 28, 2007  V6.02 Updated for DotNET
Aug 29, 2007  V6.03 A. Garrels added functions DoFileEncQuotedPrintable and
                    DoTextFileReadNoEncoding.
Mar 10, 2008  V6.04 Francois Piette made some changes to prepare code
                    for Unicode.
                    Call StrPas with appropriate typecast
Apr 20, 2008        A. Garrels fixed a bug in StrEncodeQPEx that may caused an
                    infinite loop when DoFold was FALSE.
Apr 21, 2008        A. Garrels, overload Base64Encode and Base64Decode added.
                    Also improved performance of these functions by
                    pre-allocating the result buffer, String + String should
                    realy be avoided.
Apr 25, 2008  V6.05 A. Garrels added more overloads to fight the string-hell <g>.
May 01, 2008  V6.06 A. Garrels - Function names adjusted according to changes in
                    OverbyteIcsLibrary.pas and use of OverbyteIcsUtils.pas.
Jul 20, 2008  V6.07 A. Garrels Changed IcsWrapText according to SysUtils.WrapText,
                    added parameter CodePage to StrEncodeQP, added an overloaded
                    version of func. NeedsEncoding. 
Jul 24, 2008 V6.08  A. Garrels - NeedsEncoding() returned "False" for character
                    #127.
Aug 03, 2008 v6.09  A. Garrels changed some string types again.
Aug 11, 2008 V6.10  A. Garrels - Base64Encode() changed.
Oct 03, 2008 V6.11  A. Garrels moved IsCharInSysCharSet to OverbyteIcsUtils.pas,
                    simplified Set-stuff for.NET.
Oct 11, 2008 V7.12  A. Garrels added an AnsiString overload to FoldString().
                    Bumped version number to v7.
Oct 12, 2008 V7.13  Angus added HdrEncodeInLineEx which encodes UTF-16 to raw header
                    HdrEncodeInLine now RawByteString so D2009 does not mess with it
Jan 03, 2009 V7.14  A. Garrels added a PAnsiChar overload to Base64Encode().
Jan 20, 2009 V7.15  A. Garrels added function CalcBase64AttachmentGrow and
                    fixed return of var More in DoFileEncBase64() which could
                    lead to an additional blank line in MIME part.
May 02, 2009 V7.16  A. Garrels fixed a bug in IcsWrapTextEx that could break
                    surrogate-pairs in the Unicode overload and multbyte characters
                    in the ANSI overloaded version. The latter takes a CodePage
                    argument now and RawByteString instead of AnsiString.
May 02, 2009 V7.16a A. Garrels - Avoid unnecessary calls to IcsStrCharLength()
                    in IcsWrapTextEx() ANSI overload.
May 10, 2009 V7.17  A. Garrels - Some AnsiString types changed to RawByteString.
                    Made some changes to work around an issue in 2009 where global
                    var Syslocale.FarEast is always True.
May 30, 2009 V7.18  A. Garrels fixed a bug in IcsWrapTextEx that could truncate
                    a line.
Jul 31, 2009 V7.19  A. Garrels enlarged base64 encoded data chunks. Especially
                    SSL benefits from this change, less CPU use and 6 times
                    faster sends of base64 encoded file attachments.
Mar 19, 2010 V7.20  A. Garrels added the same fix of V7.18 to the Unicode version
                    of IcsWrapTextEx. Fixed a bug with MBCS and Base64 encoding.
Apr 26, 2010 V7.21  Arno added EncodeMbcsInline() that handles UTF-7 and some
                    other stateful and MBCS. Fixed a bug with folding and MIME
                    inline encoding.
Jun 10, 2010 V7.22  A. Buzanakov fixed DecodeQuotedPrintable return value.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMimeUtils;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$DEFINE USE_BUFFERED_STREAM}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

{$R-}

uses
{$IFDEF CLR}
    System.Text,
    System.IO,
{$ENDIF}
{$IFDEF USE_BUFFERED_STREAM}
    OverbyteIcsStreams,
{$ENDIF}
    SysUtils, // For the LeadChar and Exception
    Classes,
    Math,
    OverbyteIcsUtils,
    OverbyteIcsLibrary,
    OverbyteIcsCsc,
    OverbyteIcsCharsetUtils;

const
    TMimeUtilsVersion = 721;
    CopyRight : String = ' MimeUtils (c) 2003-2010 F. Piette V7.21 ';

    SmtpDefaultLineLength = 76; // without CRLF
    SMTP_SND_BUF_SIZE     = 2048;

    { Explicit type cast to Ansi works in .NET as well }
    SpecialsRFC822 : TSysCharSet = [AnsiChar('('), AnsiChar(')'), AnsiChar('<'),
                AnsiChar('>'), AnsiChar('@'), AnsiChar(','), AnsiChar(';'),
                AnsiChar(':'), AnsiChar('\'), AnsiChar('"'), AnsiChar('['),
                AnsiChar(']'), AnsiChar('.')];
    CrLfSet : TSysCharSet = [AnsiChar(#13), AnsiChar(#10)];
    QuotedCharSet : TSysCharSet = [AnsiChar('?'), AnsiChar('='), AnsiChar(' '),
                                   AnsiChar('_')];
    BreakCharsSet : TSysCharSet = [AnsiChar(#9), AnsiChar(#32), AnsiChar(';'),
                                   AnsiChar(','), AnsiChar('>'), AnsiChar(']')];

    HexTable : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9',
                                        'A','B','C','D','E','F');      {HLX}
    HexTableA : array[0..15] of AnsiChar = ('0','1','2','3','4','5','6','7','8','9',
                                        'A','B','C','D','E','F');      {HLX}

{ Functions to encode/decode string as a "quoted-printable" string RFC2045}
function  EncodeQuotedPrintable(const S: RawByteString) : String; overload;
{$IFDEF COMPILER12_UP}
function  EncodeQuotedPrintable(const S: UnicodeString; ACodePage: LongWord) : UnicodeString;  overload;
function  EncodeQuotedPrintable(const S: UnicodeString) : UnicodeString; overload;
{$ENDIF}
function  DecodeQuotedPrintable(const S: RawByteString) : RawByteString; overload;
{$IFDEF COMPILER12_UP}
function  DecodeQuotedPrintable(const S: UnicodeString; ACodePage: LongWord) : UnicodeString; overload;
function  DecodeQuotedPrintable(const S: UnicodeString) : UnicodeString; overload;
{$ENDIF}
function  SplitQuotedPrintableString(const S : String) : String;
{ Find a Content-Type from a file name                                   }
function  FilenameToContentType(FileName : String) : String;

{ Base 64 encoding }
function  Base64Encode(const Input : AnsiString) : AnsiString; overload;
{$IFNDEF CLR}
function  Base64Encode(const Input : PAnsiChar; Len : Integer) : AnsiString; overload;
{$ENDIF}
{$IFDEF COMPILER12_UP}
function  Base64Encode(const Input : UnicodeString; ACodePage: LongWord) : UnicodeString; overload;
function  Base64Encode(const Input : UnicodeString) : UnicodeString; overload;
{$ENDIF}
{$IFDEF CLR}
function  Base64Encode(Input : StringBuilder) : StringBuilder; overload;
{$ENDIF}

function  Base64Decode(const Input : AnsiString) : AnsiString; overload;
{$IFDEF COMPILER12_UP}
function  Base64Decode(const Input : UnicodeString; ACodePage: LongWord) : UnicodeString; overload;
function  Base64Decode(const Input : UnicodeString) : UnicodeString; overload;
{$ENDIF}
{$IFDEF CLR}
function  Base64Decode(Input : StringBuilder) : StringBuilder; overload;
{$ENDIF}

function  InitFileEncBase64(const FileName : String;
                            ShareMode      : Word) : TStream;
function  DoFileEncBase64(var Stream     : TStream;
                          var More       : Boolean) : AnsiString;
function  DoFileEncQuotedPrintable(var Stream     : TStream;                {AG}
                          var More       : Boolean) : AnsiString;
function  DoTextFileReadNoEncoding(var Stream     : TStream;                {AG}
                          var More       : Boolean) : AnsiString;
function  DoFileLoadNoEncoding(var Stream     : TStream;               {Bjørnar}
                          var More       : Boolean) : String;
{ Similar to Base64Encode, returns just a coded line                      }
function  Base64EncodeEx(const Input    : RawByteString;
                         MaxCol         : Integer;
                         var cPos       : Integer;
                         CodePage       : LongWord = CP_ACP;
                         IsMultiByteCP  : Boolean = FALSE) : RawByteString; overload;
{$IFDEF COMPILER12_UP}
function  Base64EncodeEx(const Input : UnicodeString;
                         MaxCol      : Integer;
                         var cPos    : Integer) : UnicodeString; overload;
function  Base64EncodeEx(const Input : UnicodeString;
                         MaxCol      : Integer;
                         var cPos    : Integer;
                         ACodePage   : LongWord) : UnicodeString; overload;
{$ENDIF}
procedure EndFileEncBase64(var Stream : TStream);
{ Dot at start of line escaping for SMTP and NNTP (double the dot)        }
procedure DotEscape(var S : String; OnlyAfterCrLf : Boolean = False); {AG 11/04/07}
{ Similar to IcsWrapText, returns just a single line                      } {AG}
function IcsWrapTextEx(const Line : RawByteString;
                       const BreakStr : RawByteString;
                       const BreakingChars: TSysCharSet;
                       MaxCol         : Integer;
                       QuoteChars     : TSysCharSet;
                       var cPos       : Integer;
                       ForceBreak     : Boolean = False;
                       ACodePage      : LongWord = CP_ACP;
                       IsMultiByteCP  : Boolean  = FALSE): RawByteString; {$IFDEF COMPILER12_UP} overload;

function IcsWrapTextEx(const Line : String;
                       const BreakStr : String;
                       const BreakingChars: TSysCharSet;
                       MaxCol       : Integer;
                       QuoteChars   : TSysCharSet;
                       var cPos     : Integer;
                       ForceBreak: Boolean = False): String; overload;
{$ENDIF}
{ Unfolds folded headers                                                  } {AG}
function UnFoldHdrLine(const S : String): String;
{Helper function                                                          }
function NeedsEncoding(const S : AnsiString) : Boolean; {$IFDEF COMPILER12_UP} overload;           {AG}
function NeedsEncoding(const S : UnicodeString) : Boolean; overload;        {AG}
{$ENDIF}
{$IFDEF WIN32}
function NeedsEncodingPChar(S : PChar) : Boolean;                           {FP}
{$ENDIF}
{ MIME In-Line-Encoding plus Folding, see comments in function source     } {AG}
function HdrEncodeInLine(const Input   : RawByteString;         { V7.13 was Ansi }
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : AnsiChar;    { Either 'Q' or 'B' }
                         const CharSet : AnsiString;  { e.g. 'iso-8859-1' existing Input charset }
                         MaxCol        : Integer;
                         DoFold        : Boolean;
                         CodePage      : LongWord = CP_ACP;
                         IsMultiByteCP : Boolean = FALSE): RawByteString; {$IFDEF COMPILER12_UP} overload;

function HdrEncodeInLine(const Input   : UnicodeString;
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : WideChar;    { Either 'Q' or 'B'        }
                         const CharSet : UnicodeString;  { e.g. 'iso-8859-1'     }
                         MaxCol        : Integer;
                         DoFold        : Boolean;
                         Codepage       : LongWord = CP_ACP;
                         IsMultiByteCP  : Boolean = FALSE): UnicodeString; overload;
{$ENDIF}
function HdrEncodeInLineEx(const Input : UnicodeString;    { V7.13 }
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : WideChar;    { Either 'Q' or 'B'        }
                         CodePage      : LongWord;    { Input will be encoded into this CharSet }
                         MaxCol        : Integer;
                         DoFold        : Boolean;
                         IsMultiByteCP : Boolean = FALSE): RawByteString;
{ Alternate of functions:                                                      }
{ EncodeQuotedPrintable + SplitQuotedPrintableString + DotEscape               }
function StrEncodeQP(const Input   : RawByteString;                    {HLX, AG}
                     MaxCol        : Integer;
                     Specials      : TSysCharSet;
                     CodePage      : LongWord = CP_ACP;
                     IsMultibyteCP : Boolean = FALSE): String; overload;
{$IFDEF COMPILER12_UP}
function StrEncodeQP(const Input   : UnicodeString;                    {HLX, AG}
                     MaxCol        : Integer;
                     Specials      : TSysCharSet;
                     ACodePage     : LongWord;
                     IsMultibyteCP : Boolean = FALSE): UnicodeString; overload;
{$ENDIF}
{ Similar to StrEncodeQP, returns just a single line                      } {AG}
function StrEncodeQPEx(const Buf     : RawByteString;
                       MaxCol        : Integer;
                       Specials      : TSysCharSet;
                       ShortSpace    : Boolean; {'_' e.g. for in-line}
                       var cPos      : Integer;
                       DoFold        : Boolean;
                       CodePage      : LongWord = CP_ACP;
                       IsMultibyteCP : Boolean = FALSE) : RawByteString; overload;
{$IFDEF COMPILER12_UP}
function StrEncodeQPEx(const Buf   : UnicodeString;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean; {'_' e.g. for in-line}
                       var cPos    : Integer;
                       DoFold      : Boolean) : UnicodeString; overload;


procedure FoldHdrLine(HdrLines      : TStrings;                             {AG}
                      const HdrLine : String); overload;
{$ENDIF}
procedure FoldHdrLine(HdrLines      : TStrings;                             {AG}
                      const HdrLine : RawByteString;
                      ACodePage     : LongWord = CP_ACP;
                      IsMultiByteCP : Boolean = FALSE); overload;

function FoldString(const Input   : RawByteString;                          {AG}
                    BreakCharsSet : TSysCharSet;
                    MaxCol        : Integer;
                    ACodePage     : LongWord = CP_ACP;
                    IsMultiByteCP : Boolean = False): RawByteString; overload;
{$IFDEF COMPILER12_UP}
function FoldString(const Input   : UnicodeString;
                    BreakCharsSet : TSysCharSet;
                    MaxCol        : Integer): UnicodeString; overload;
{$ENDIF}

{ Calculates Base64 grow including CRLFs }
function CalcBase64AttachmentGrow(FileSize: Int64): Int64;
function EncodeMbcsInline(CodePage        : LongWord;
                          const Charset   : String;
                          EncType         : Char;
                          Body            : PWideChar;
                          Len             : Integer;
                          DoFold          : Boolean;
                          MaxLen          : Integer): AnsiString; overload;
function EncodeMbcsInline(CodePage        : LongWord;
                          const Charset   : String;
                          EncType         : Char;
                          Body            : PAnsiChar;
                          Len             : Integer;
                          DoFold          : Boolean;
                          MaxLen          : Integer): AnsiString; overload;

implementation

const
{$IFDEF DELPHI1}
{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
  LeadBytes: set of Char = [];
{$ENDIF}
  CP_ACP = 0; // Windows.pas
  FILE_BASE64_LINE_LENGTH = SmtpDefaultLineLength; // (76) Without CRLF
  FILE_BASE64_BUF_SIZE    = (SMTP_SND_BUF_SIZE -1) div (FILE_BASE64_LINE_LENGTH + 2)
                             * (FILE_BASE64_LINE_LENGTH + 2);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ See also SplitQuotedPrintableString !                                     }
function EncodeQuotedPrintable(const S: RawByteString) : String;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and
              (S[I] <> '=') and
              (S[I] >= ' ') and
              (Ord(S[I]) <= 126) do
            Inc(I);
        if I > Length(S) then begin
            if J = 1 then
{$IFDEF COMPILER12_UP}
                Result := UsAsciiToUnicode(S)     { Optimisation }
            else
                Result := Result + UsAsciiToUnicode(Copy(S, J, I - J));
{$ELSE}
                Result := S     { Optimisation }
            else
                Result := Result + Copy(S, J, I - J);
{$ENDIF}
            Exit;
        end;
{$IFDEF COMPILER12_UP}
        Result := Result + UsAsciiToUnicode(Copy(S, J, I - J)) + '=' +
                  _UpperCase(_IntToHex(Ord(S[I]), 2));
{$ELSE}
        Result := Result + Copy(S, J, I - J) + '=' +
                  _UpperCase(_IntToHex(Ord(S[I]), 2));
{$ENDIF}
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function EncodeQuotedPrintable(const S: UnicodeString; ACodePage: LongWord) : UnicodeString;
begin
    Result := EncodeQuotedPrintable(UnicodeToAnsi(S, ACodePage));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeQuotedPrintable(const S: UnicodeString) : UnicodeString;
begin
    Result := EncodeQuotedPrintable(S, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A line ending with an equal sign is continued on the next line. This is   }
{ what RFC2045 refers as a "soft line break".                               }
{ This routine doesn't take care of the equal sign at the end of string.    }
{ It is simply ignored. The caller must check that condition and merge      }
{ successives lines. But the routine handle embedded soft line break.       }
function DecodeQuotedPrintable(const S: RawByteString) : RawByteString;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and (S[I] <> '=') do
            Inc(I);
        Result := Result + Copy(S, J, I - J);
        if I >= Length(S) then
            break;
        if S[I + 1] = #13 then  { Could also check for #10 }
            { Soft line break, nothing to do except continuing }
        else
            Result := Result + AnsiChar(_StrToInt('$' + Copy(S, I + 1, 2)));
        Inc(I, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
{ Input must not be converted since it is plain US-ASCII, assumes one input }
{ char can safely be casted to one byte.                                    }
function DecodeQuotedPrintable(const S: UnicodeString; ACodePage: LongWord) : UnicodeString;
var
    I, J : Integer;
    Buf : AnsiString;
begin
    Buf    := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and (S[I] <> '=') do
            Inc(I);
        Buf := Buf + UnicodeToUsAscii(Copy(S, J, I - J));
        if I >= Length(S) then
            break;
        if S[I + 1] = #13 then  { Could also check for #10 }
            { Soft line break, nothing to do except continuing }
        else
            Buf := Buf + AnsiChar(_StrToInt('$' + Copy(S, I + 1, 2)));
        Inc(I, 3);
    end;
    Result := AnsiToUnicode(Buf, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DecodeQuotedPrintable(const S: UnicodeString) : UnicodeString;
begin
    Result := DecodeQuotedPrintable(S, CP_ACP);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SplitQuotedPrintableString(const S : String) : String;
var
    I, J : Integer;
begin
    if Length(S) <= SmtpDefaultLineLength then begin
        { No need to split }
        Result := S;
        Exit;
    end;
    Result := '';
    J      := 1;
    I      := SmtpDefaultLineLength;
    while TRUE do begin
        if S[I - 1] = '=' then
            Dec(I)
        else if S[I - 2] = '=' then
            Dec(I, 2);
        Result := Result + Copy(S, J, I - J) + '=' + #13#10;
        J      := I;
        Inc(I, 75);
        if I > Length(S) then begin
            Result := Result + Copy(S, J, I - J);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DotEscape(var S : String; OnlyAfterCrLf : Boolean = False);  {AG 11/04/07}
var
    I : Integer;
begin
    if S = '' then
        Exit;
    if (S[1] = '.') and not OnlyAfterCrLf then begin  {AG 11/04/07}
        Insert('.', S, 1);
        I := 3;
    end
    else
        I := 1;
    while I <= (Length(S) - 2) do begin  // {AG 10/29/07}
        if (S[I] = #13) and (S[I + 1] = #10) and (S[I + 2] = '.') then begin
            Insert('.', S, I + 2);
            Inc(I, 4);
            continue;
        end;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FilenameToContentType(FileName : String) : String;
var
    Ext : String;
begin
    { We probably should the registry to find MIME type for known file types }
    Ext := _LowerCase(_ExtractFileExt(FileName));
    if Length(Ext) > 1 then
        Ext := Copy(Ext, 2, Length(Ext));
    if (Ext = 'htm') or (Ext = 'html') then
        Result := 'text/html'
    else if Ext = 'gif' then
        Result := 'image/gif'
    else if Ext = 'bmp' then
        Result := 'image/bmp'
    else if (Ext = 'jpg') or (Ext = 'jpeg') then
        Result := 'image/jpeg'
    else if Ext = 'txt' then
        Result := 'text/plain'
    else
        Result := 'application/octet-stream';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  InitFileEncBase64(
    const FileName : String;
    ShareMode      : Word) : TStream;
begin
{$IFNDEF USE_BUFFERED_STREAM}
    Result := TFileStream.Create(FileName, fmOpenRead or ShareMode);
{$ELSE}
    Result := TBufferedFileStream.Create(FileName, fmOpenRead or ShareMode, 4096);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    Base64Out: array [0..64] of Char = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
    );
    Base64OutA: array [0..64] of AnsiChar = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
    );
    Base64In: array[0..127] of Byte = (
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
         56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
          0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
         13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
        255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
         33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
         46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
    );

{Bjørnar}
function DoFileLoadNoEncoding(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    //MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MULTIPLIER            = 4;
    MAX_READ              = SmtpDefaultLineLength * MULTIPLIER;
var
    DataOut      : array [0..MAX_READ]  of Byte;
    ByteCount    : Integer;
    //I          : Integer;
    //Lines      : Integer;
begin

    ByteCount := Stream.Read(DataOut, MAX_READ);
    //Lines := ByteCount div MAX_READ;
    //Insert(#09, Result, 1);
    SetLength(Result, ByteCount);// + (Lines * 2));
    Move(DataOut[0], Result[1], Length(Result));
    { Splitting lines
    I := SmtpDefaultLineLength + 1;
    while I < Lines do begin;
        Insert(#13#10, Result, I);
        Inc(I, SmtpDefaultLineLength + 2);
        Inc(Lines);
    end;}
    More := (ByteCount = MAX_READ);
end;
{Bjørnar}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is a slow function, it realy should be used with TBufferedFileStream }
{ Assumes a plain ASCII text file.                                          }
function DoTextFileReadNoEncoding(                                       {AG}
    var Stream : TStream;
    var More   : Boolean) : AnsiString;
const
    LINE_LENGTH  = 1022;
var
    Cnt  : Integer;
    I    : Integer;
    Buf  : AnsiChar;
begin
    I   := 0;
    Cnt := 1;
    SetLength(Result, LINE_LENGTH);
    while (I < LINE_LENGTH + 2) and (Cnt = 1) do begin
        Cnt := Stream.Read(Buf, 1);
        if (Cnt = 1) then begin
            if not (Buf in CrLfSet) then begin
            //if not IsCharInSysCharSet(Buf, CrLfSet) then begin
                if I >= LINE_LENGTH then begin
                    Stream.Seek(-1, sofromCurrent);
                    Break;
                end
                else
                Result[I + 1] := Buf
            end
            else begin
                if (Buf = #13) then
                  Continue
                else
                  Break;
            end;
            Inc(I);
        end
    end;
    if I <> Length(Result) then
        SetLength(Result, I);
    More := Stream.Position <> Stream.Size;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is a slow function, it realy should be used with TBufferedFileStream }
function DoFileEncQuotedPrintable(                                       {AG}
    var Stream : TStream;
    var More   : Boolean) : AnsiString;
const
    LINE_LENGTH = 73; // + 2 = 75 + trailing '=' = 76
var
    Cnt  : Integer;
    I    : Integer;
    Buf  : AnsiChar;
begin
    I   := 0;
    Cnt := 1;
    SetLength(Result, LINE_LENGTH + 3);
    while (I < LINE_LENGTH) and (Cnt = 1) do begin
        Cnt := Stream.Read(Buf, 1);
        if (Cnt = 1) then begin
            if (Ord(Buf) > 126)  or
               (Ord(Buf) < 32)   or
               (Buf in ['=', '.']) then begin
               //IsCharInSysCharSet(Buf, ['=', '.']) then begin
                Inc(I);
                Result[I] := '=';
                Inc(I);
                Result[I] := HexTableA[(Ord(Buf) shr 4) and 15];
                Inc(I);
                Result[I] := HexTableA[Ord(Buf) and 15];
            end
            else begin
                Inc(I);
                Result[I] := Buf; 
            end;
        end;
    end;
    if I > 0 then begin
        Inc(I);
        Result[I] := '=';
    end;
    if I <> Length(Result) then
        Setlength(Result, I);
    More := Cnt <> 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    HLX_MULTIPLIER        = 3;  { for three lines at once }
    //MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MAX_LENGTH_MULTIPLIED = (SmtpDefaultLineLength + 2) * HLX_MULTIPLIER;
    MAX_READ              = ((SmtpDefaultLineLength * 3)div 4) * HLX_MULTIPLIER;
    MAX_READ_MOD          = (SmtpDefaultLineLength * 3) div 4;
var
    Count, Place : Integer;
    DataIn       : array [0..MAX_READ]  of Byte;
    DataOut      : array [0..MAX_LENGTH_MULTIPLIED + 8] of Byte;
    ByteCount    : Integer;
    I            : Integer;
{ HLX: The following code is rewritten, so it loads data in MAX_READ chunks and
  encodes all loaded data. The trick relies on the fact that TriggerGetData's
  MsgLine buffer can hold up to 1024 chars. We'll encode 3 lines at once,
  add CRLF's, and return all three as one: component will see it as one,
  server will still see it as three.
  I've noticed a strange behavior: having HLX_MULTIPLIER > 3, data aren't
  sent completely, although it shouldn't occur
  (see: TCustomSmtpClient.DataNext) }
begin
    Count     := 0;
    Place     := 0;
    ByteCount := Stream.Read(DataIn, MAX_READ);
    while Place < ByteCount do begin
        DataOut[Count] := (DataIn[Place] and $FC) shr 2;
        Inc(Count);
        DataOut[Count] := (DataIn[Place] and $03) shl 4;
        Inc(Place);
        if Place < ByteCount then begin
            DataOut[Count] := DataOut[Count] + (DataIn[Place] and $F0) shr 4;
            Inc(Count);
            DataOut[Count] := (DataIn[Place] and $0F) shl 2;
            Inc(Place);
            if Place < ByteCount then begin
                DataOut[Count] := DataOut[Count] + (DataIn[Place] and $C0) shr 6;
                Inc(Count);
                DataOut[Count] := (DataIn[Place] and $3F);
                Inc(Place);
                Inc(Count);
            end
            else begin
                Inc(Count);
                DataOut[Count] := $40;
                Inc(Count);
            end;
        end
        else begin
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
        end;
    end;
    { Moved out of the main loop, so it has the chance to work in the }
    { processor's L1 Cache                                            }
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
        DataOut[I] := Byte(Base64Out[DataOut[I]]);
    Move(DataOut[0], Result[1], Count);
    { Splitting lines }
    I := MAX_LENGTH + 1;
    while I < Count do begin;
        Insert(#13#10, Result, I);
        Inc(I, MAX_LENGTH + 2);
        Inc(Count);
    end;
    More := (ByteCount = MAX_READ);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : AnsiString;
var
    Count     : Integer;
    DataIn    : array [0..2]  of Byte;
    DataOut   : array [0..FILE_BASE64_BUF_SIZE] of Byte;
    ByteCount : Integer;
    LineLength : Integer;
    I         : Integer;
{$IFDEF CLR}
    SB        : StringBuilder;
{$ENDIF}
begin
    Count      := 0;
    LineLength := 0;
    while TRUE do begin
        ByteCount := Stream.Read(DataIn, 3);
        if ByteCount = 0 then                            {<=MHU}
           Break;                                        {<=MHU}
        DataOut[Count]     := (DataIn[0] and $FC) shr 2;
        DataOut[Count + 1] := (DataIn[0] and $03) shl 4;
        if ByteCount > 1 then begin
            DataOut[Count + 1] := DataOut[Count + 1] +
                                  (DataIn[1] and $F0) shr 4;
            DataOut[Count + 2] := (DataIn[1] and $0F) shl 2;
            if ByteCount > 2 then begin
                DataOut[Count + 2] := DataOut[Count + 2] +
                                      (DataIn[2] and $C0) shr 6;
                DataOut[Count + 3] := (DataIn[2] and $3F);
            end
            else begin
                DataOut[Count + 3] := $40;
            end;
        end
        else begin
            DataOut[Count + 2] := $40;
            DataOut[Count + 3] := $40;
        end;

        for I := 0 to 3 do
            DataOut[Count + I] := Byte(Base64Out[DataOut[Count + I]]);

        Count := Count + 4;

        if (Count + 6 > FILE_BASE64_BUF_SIZE) or (ByteCount < 3) then
            break;
        // Do not append CRLF to the last chunk, SmtpCli will do
        Inc(LineLength, 4);
        if (LineLength >= FILE_BASE64_LINE_LENGTH) then
        begin
            DataOut[Count]     := $0D;
            DataOut[Count + 1] := $0A;
            Inc(Count, 2);
            LineLength := 0;
        end;
    end;

    DataOut[Count] := $0;

    { Next line commented since it led to an additional blank line in      }
    { the MIME part. Instead use Stream.Size, see below.                   }

    //More           := (ByteCount = 3);

{$IFDEF USE_BUFFERED_STREAM}
    { If ShareMode does not allow shared writes we may use the file size   }
    if TBufferedFileStream(Stream).Mode and $F0 <= fmShareDenyWrite then
        More := Stream.Position < TBufferedFileStream(Stream).FastSize
    else
        { This is slow! But does anybody really allow shared writes?       }
        More := Stream.Position < Stream.Size;
{$ELSE}
    { Slow, TBufferedFileStream should be used anyway, it's much faster.   }
    More := Stream.Position < Stream.Size;
{$ENDIF}

{$IFDEF CLR}
    SB := StringBuilder.Create(Count);
    for I := 0 to Count - 1 do
        SB[I] := Char(DataOut[I]);
    Result := SB.ToString;
{$ELSE}
    Result := _StrPas(PAnsiChar(@DataOut[0]));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EndFileEncBase64(var Stream : TStream);
begin
    if Assigned(Stream) then begin
        Stream.Free;
        Stream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF CLR}
function Base64Encode(const Input : PAnsiChar; Len: Integer) : AnsiString;
var
    Count : Integer;
    I     : Integer;
begin
    Count := 0;
    I := Len;
    while (I mod 3) > 0 do
        Inc(I);
    I := (I div 3) * 4;
    SetLength(Result, I);
    I := 0;
    while Count < Len do begin
        Inc(I);
        Result[I] := Base64OutA[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) < Len then begin
            Inc(I);
            Result[I] := Base64OutA[((Byte(Input[Count]) and $03) shl 4) +
                                    ((Byte(Input[Count + 1]) and $F0) shr 4)];
            if (Count + 2) < Len then begin
                Inc(I);
                Result[I] := Base64OutA[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                       ((Byte(Input[Count + 2]) and $C0) shr 6)];
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[Count + 2]) and $3F)];
            end
            else begin
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[Count + 1]) and $0F) shl 2];
                Inc(I);
                Result[I] := '=';
            end
        end
        else begin
            Inc(I);
            Result[I] := Base64OutA[(Byte(Input[Count]) and $03) shl 4];
            Inc(I);
            Result[I] := '=';
            Inc(I);
            Result[I] := '=';
        end;
        Inc(Count, 3);
    end;
    //SetLength(Result, I);
end;
{$ENDIF}


function Base64Encode(const Input : AnsiString) : AnsiString;
{$IFNDEF CLR}
begin
    Result := Base64Encode(PAnsiChar(Input), Length(Input));
end;
{$ELSE}
var
    Count : Integer;
    Len   : Integer;
    I     : Integer;
begin
    Count  := 1;
    I      := 0;
    Len    := Length(Input);
    SetLength(Result, ((Len + 2) div 3) * 4);
    while Count <= Len do begin
        Inc(I);
        Result[I] := Base64OutA[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Inc(I);
            Result[I] := Base64OutA[((Byte(Input[Count]) and $03) shl 4) +
                                    ((Byte(Input[Count + 1]) and $F0) shr 4)];
            if (Count + 2) <= Len then begin
                Inc(I);
                Result[I] := Base64OutA[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                       ((Byte(Input[Count + 2]) and $C0) shr 6)];
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[Count + 2]) and $3F)];
            end
            else begin
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[Count + 1]) and $0F) shl 2];
                Inc(I);
                Result[I] := '=';
            end
        end
        else begin
            Inc(I);
            Result[I] := Base64OutA[(Byte(Input[Count]) and $03) shl 4];
            Inc(I);
            Result[I] := '=';
            Inc(I);
            Result[I] := '=';
        end;
        Inc(Count, 3);
    end;
    SetLength(Result, I);
end;
{$ENDIF}

{$IFDEF COMPILER12_UP}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts the UnicodeString to AnsiString using the code page specified,   }
{ converts the Base64 AnsiString result to Unicode using default code page. }
function Base64Encode(const Input : UnicodeString; ACodePage: LongWord) : UnicodeString;
begin
    Result := String(Base64Encode(UnicodeToAnsi(Input, ACodePage)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts the UnicodeString to AnsiString using the default code page,     }
{ converts the Base64 AnsiString result to Unicode using default code page. }
function Base64Encode(const Input : UnicodeString) : UnicodeString;
begin
    Result := String(Base64Encode(AnsiString(Input)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}

{$IFDEF CLR}
function Base64Encode(Input : StringBuilder) : StringBuilder;
var
    Count : Integer;
    Len   : Integer;
begin
    Result := StringBuilder.Create;
    Count  := 1;
    Len    := Input.Length;
    while Count <= Len do begin
        Result.Append(Base64Out[(Byte(Input[Count]) and $FC) shr 2]);
        if (Count + 1) <= Len then begin
            Result.Append(Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                    ((Byte(Input[Count + 1]) and $F0) shr 4)]);
            if (Count + 2) <= Len then begin
                Result.Append(Base64Out[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                        ((Byte(Input[Count + 2]) and $C0) shr 6)]);
                Result.Append(Base64Out[(Byte(Input[Count + 2]) and $3F)]);
            end
            else begin
                Result.Append(Base64Out[(Byte(Input[Count + 1]) and $0F) shl 2]);
                Result.Append('=');
            end
        end
        else begin
            Result.Append(Base64Out[(Byte(Input[Count]) and $03) shl 4]);
            Result.Append('==');
        end;
        Count := Count + 3;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(const Input : AnsiString) : AnsiString;
var
    Count   : Integer;
    Len     : Integer;
    I       : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Count := 1;
    Len   := Length(Input);
    I     := 0;
    SetLength(Result, Len + 2);
    while Count <= Len do begin
        if Byte(Input[Count]) in [13, 10] then
            Inc(Count)
        else begin
            DataIn0 := Base64In[Byte(Input[Count])];
            DataIn1 := Base64In[Byte(Input[Count+1])];
            DataIn2 := Base64In[Byte(Input[Count+2])];
            DataIn3 := Base64In[Byte(Input[Count+3])];
            Inc(I);
            Result[I] := AnsiChar(((DataIn0 and $3F) shl 2) +
                                  ((DataIn1 and $30) shr 4));
            if DataIn2 <> $40 then begin
                Inc(I);
                Result[I] := AnsiChar(((DataIn1 and $0F) shl 4) +
                                      ((DataIn2 and $3C) shr 2));
                if DataIn3 <> $40 then begin
                    Inc(I);
                    Result[I] :=  AnsiChar(((DataIn2 and $03) shl 6) +
                                           (DataIn3 and $3F));
                end;
            end;
            Count := Count + 4;
        end;
    end;
    SetLength(Result, I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Input must not be converted since it is plain US-ASCII, assumes one input }
{ char can safely be casted to one byte.                                    }
{$IFDEF COMPILER12_UP}
function Base64Decode(const Input : UnicodeString; ACodePage: LongWord) : UnicodeString;
var
    Count   : Integer;
    Len     : Integer;
    I       : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
    Buf     : AnsiString;
begin
    Count := 1;
    Len   := Length(Input);
    I     := 0;
    SetLength(Buf, Len + 2);
    while Count <= Len do begin
        if Ord(Input[Count]) in [13, 10] then
            Inc(Count)
        else begin
            DataIn0 := Base64In[Byte(Input[Count])];
            DataIn1 := Base64In[Byte(Input[Count+1])];
            DataIn2 := Base64In[Byte(Input[Count+2])];
            DataIn3 := Base64In[Byte(Input[Count+3])];
            Inc(I);
            Buf[I] := AnsiChar(((DataIn0 and $3F) shl 2) +
                               ((DataIn1 and $30) shr 4));
            if DataIn2 <> $40 then begin
                Inc(I);
                Buf[I] := AnsiChar(((DataIn1 and $0F) shl 4) +
                                   ((DataIn2 and $3C) shr 2));
                if DataIn3 <> $40 then begin
                    Inc(I);
                    Buf[I] :=  AnsiChar(((DataIn2 and $03) shl 6) +
                                        (DataIn3 and $3F));
                end;
            end;
            Count := Count + 4;
        end;
    end;
    SetLength(Buf, I);
    Result := AnsiToUnicode(Buf, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(const Input : UnicodeString) : UnicodeString;
begin
    Result := Base64Decode(Input, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function Base64Decode(Input : StringBuilder) : StringBuilder;
var
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Result := StringBuilder.Create;
    Count  := 1;
    Len    := Input.Length;
    while Count <= Len do begin
        DataIn0 := Base64In[Byte(Input[Count])];
        DataIn1 := Base64In[Byte(Input[Count+1])];
        DataIn2 := Base64In[Byte(Input[Count+2])];
        DataIn3 := Base64In[Byte(Input[Count+3])];

        Result.Append(Char(((DataIn0 and $3F) shl 2) +
                           ((DataIn1 and $30) shr 4)));
        if DataIn2 <> $40 then begin
            Result.Append(Char(((DataIn1 and $0F) shl 4) +
                               ((DataIn2 and $3C) shr 2)));
            if DataIn3 <> $40 then
                Result.Append(Char(((DataIn2 and $03) shl 6) +
                                   (DataIn3 and $3F)));
        end;
        Count := Count + 4;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Works with DBCS and UTF-8 only                                            }
function B64SafeMaxIndex(CP: LongWord; const S: RawbyteString;    {AG}
  Index: Integer; MaxBytes: Integer): Integer;
var
    P    : PAnsiChar;
    L    : Integer;
    ChL  : Integer;
    TL   : Integer;
    B64L : Integer;
begin
    L := Length(S);
    if (Index > 0) and (Index <= L) then
    begin
        P := PAnsiChar(S) + (Index - 1);
        TL  := 0;
        ChL := 0;
        while TRUE do
        begin
            Inc(P, ChL);
            ChL := IcsStrCharLength(P, CP);
            if (ChL <= 0) then
                Break;
            Inc(TL, ChL);
            B64L := TL;
            while B64L mod 3 > 0 do
                Inc(B64L);
            B64L := (B64L div 3) * 4;
            if B64L > MaxBytes then
            begin
                Dec(TL, ChL);
                Break;
            end
            else if B64L = MaxBytes then
                Break;
        end;
        Result := Index + TL;
    end
    else
        Result := Index;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to Base64Encode, returns just a coded line                        }
function Base64EncodeEx(
    const Input   : RawByteString;
    MaxCol        : Integer;
    var cPos      : Integer;
    CodePage      : LongWord = CP_ACP;
    IsMultiByteCP : Boolean = FALSE) : RawByteString;
var
    Len : Integer;
    I   : Integer;
    NextCharIdx : Integer;
begin
    Len := Length(Input);

    if IsMultibyteCP then
    begin
        NextCharIdx := B64SafeMaxIndex(CodePage, Input, cPos, MaxCol);
        if NextCharIdx > cPos then
        begin
            Result := Base64Encode(PAnsiChar(Input) + (cPos - 1),
                                   NextCharIdx - cPos);
            cPos := NextCharIdx;
        end;
        Exit;
    end;

    I := 0;
    SetLength(Result, MaxCol + 3);
    while (cPos <= Len) do begin
        Inc(I);

        if (I > MaxCol) then begin
            Dec(I);
            Break;
        end;

        Result[I] := Base64OutA[(Byte(Input[cPos]) and $FC) shr 2];
        if (cPos + 1) <= Len  then begin
            Inc(I);
            Result[I] := Base64OutA[((Byte(Input[cPos]) and $03) shl 4) +
                                   ((Byte(Input[cPos + 1]) and $F0) shr 4)];
            if (cPos + 2) <= Len then begin
                Inc(I);
                Result[I] := Base64OutA[((Byte(Input[cPos + 1]) and $0F) shl 2) +
                                       ((Byte(Input[cPos + 2]) and $C0) shr 6)];
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[cPos + 2]) and $3F)];
            end
            else begin
                Inc(I);
                Result[I] := Base64OutA[(Byte(Input[cPos + 1]) and $0F) shl 2];
                Inc(I);
                Result[I] := '=';
            end
        end
        else begin
            Inc(I);
            Result[I] := Base64OutA[(Byte(Input[cPos]) and $03) shl 4];
            Inc(I);
            Result[I] := '=';
            Inc(I);
            Result[I] := '=';
        end;
        Inc(cPos, 3);
    end;
    SetLength(Result, I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
{ Similar to Base64Encode, returns just an encoded line                     }
function Base64EncodeEx(const Input : UnicodeString;
                        MaxCol      : Integer;
                        var cPos    : Integer;
                        ACodePage   : LongWord) : UnicodeString;
begin
    Result := Base64EncodeEx(UnicodeToAnsi(Input, ACodePage), MaxCol, cPos);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64EncodeEx(const Input : UnicodeString;
                        MaxCol      : Integer;
                        var cPos    : Integer) : UnicodeString;
begin
    Result := Base64EncodeEx(Input, MaxCol, cPos, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Any BreakStr not in a quoted string is truncated and the function returns!   }
{ i.e. when BreakStr #13#10#9 is found cPos returned is pos of char #9 + 1.    }
{ Breaking chars appear at the end of a line. ForceBreak works outside quoted  }
{ strings only and forces a break at MaxCol if no breaking char has been found.}
function IcsWrapTextEx(
    const Line           : RawByteString;
    const BreakStr       : RawByteString;
    const BreakingChars  : TSysCharSet;
    MaxCol               : Integer;
    QuoteChars           : TSysCharSet;
    var cPos             : Integer;
    ForceBreak           : Boolean = FALSE;
    ACodePage            : LongWord = CP_ACP;
    IsMultiByteCP        : Boolean = False): RawByteString;
var
    Col                : Integer;
    LinePos, LineLen   : Integer;
    BreakLen, BreakPos : Integer;
    QuoteChar, CurChar : AnsiChar;
    ExistingBreak      : Boolean;
    L                  : Integer;
begin
    Col           := 1;
    LinePos       := cPos;
    BreakPos      := 0;
    QuoteChar     := #0;
    ExistingBreak := False;
    LineLen       := Length(Line);
    BreakLen      := Length(BreakStr);
    Result        := '';

    while cPos <= LineLen do begin
        { Ensure MBCS (including UTF-8) are not wrapped in the middle of a }
        { code point. Doesn't work with UTF-7.                             }
        if IsMultiByteCP and (ACodePage <> CP_UTF7) then
        begin
            L := IcsStrCharLength(PAnsiChar(@Line[cPos]), ACodePage) - 1;
            if L > 0 then begin
                Inc(cPos, L);
                Inc(Col, L);
            end;
        end;
        CurChar := Line[cPos];
        //else begin
            if CurChar = BreakStr[1] then begin
                if QuoteChar = #0 then begin
                    ExistingBreak := _StrLComp(PAnsiChar(BreakStr),
                                              PAnsiChar(@Line[cPos]),
                                              BreakLen) = 0;
                    if ExistingBreak then begin
                        Inc(cPos, BreakLen - 1);
                        BreakPos := cPos;
                    end;
                end
            end
            else if IsCharInSysCharSet(CurChar,
                                       BreakingChars) then begin
                if QuoteChar = #0 then
                    BreakPos := cPos;
            end
            else if IsCharInSysCharSet(CurChar, QuoteChars) then begin
                if CurChar = QuoteChar then begin
                    QuoteChar := #0;
                    if ForceBreak and (Col >= MaxCol) and (BreakPos = 0) then
                        BreakPos := cPos;
                end
                else if QuoteChar = #0 then begin
                    QuoteChar := CurChar;
                    if ForceBreak and (cPos > LinePos) then
                        BreakPos := cPos -1; // Break before the QuoteChar
                end;
            end
            else if ForceBreak and (QuoteChar = #0) and (Col >= MaxCol) and
                    (BreakPos = 0) then begin
                BreakPos := cPos;
            end;
        //end;
        Inc(cPos);
        Inc(Col);

        if (not IsCharInSysCharSet(QuoteChar, QuoteChars)) and
           (ExistingBreak or
           ((Col > MaxCol) and (BreakPos >= LinePos))) then begin
            if ExistingBreak then
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1 - BreakLen)
            else
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1);
            if (not IsCharInSysCharSet(CurChar, QuoteChars)) or
               (ExistingBreak) then begin
                if (cPos <= LineLen) and (BreakPos + 1 = cPos) then begin
                    if _StrLComp(PChar(@Line[cPos]), #13#10, 2) = 0 then begin
                        if not ExistingBreak then begin
                            { Break due to one of the breaking chars found and CRLF follows }
                            Inc(cPos, 2);
                            Exit;
                        end;
                        { Empty line follows }
                        BreakPos := cPos - 1;
                    end;
                end;
            end;
            Inc(BreakPos);
            cPos := BreakPos;
            Exit;
        end;
    end;
    Result := Copy(Line, LinePos, MaxInt);
    cPos := MaxInt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function IcsWrapTextEx(
    const Line           : String;
    const BreakStr       : String;
    const BreakingChars  : TSysCharSet;
    MaxCol               : Integer;
    QuoteChars           : TSysCharSet;
    var cPos             : Integer;
    ForceBreak           : Boolean): String;
var
    Col                : Integer;
    LinePos, LineLen   : Integer;
    BreakLen, BreakPos : Integer;
    QuoteChar, CurChar : Char;
    ExistingBreak      : Boolean;
    L                  : Integer;
begin
    Col           := 1;
    LinePos       := cPos;
    BreakPos      := 0;
    QuoteChar     := #0;
    ExistingBreak := False;
    LineLen       := Length(Line);
    BreakLen      := Length(BreakStr);
    Result        := '';
    while cPos <= LineLen do begin
        CurChar := Line[cPos];
        { Ensure surrogate-pairs are not wrapped }
        if IsLeadChar(CurChar) then begin // Check for surrogate-pairs
        //if IsCharInSysCharSet(CurChar, LeadBytes) then begin
            L := CharLength(Line, cPos) div SizeOf(Char) -1;
            Inc(cPos, L);
            Inc(Col, L);
            CurChar := Line[cPos];
        end;
        //else begin
            if CurChar = BreakStr[1] then begin
                if QuoteChar = #0 then begin
                    ExistingBreak := _StrLComp(PChar(BreakStr),
                                              PChar(@Line[cPos]),
                                              BreakLen) = 0;
                    if ExistingBreak then begin
                        Inc(cPos, BreakLen - 1);
                        BreakPos := cPos;
                    end;
                end
            end
            else if IsCharInSysCharSet(CurChar,
                                       BreakingChars) then begin
                if QuoteChar = #0 then
                    BreakPos := cPos;
            end
            else if IsCharInSysCharSet(CurChar, QuoteChars) then begin
                if CurChar = QuoteChar then begin
                    QuoteChar := #0;
                    if ForceBreak and (Col >= MaxCol) and (BreakPos = 0) then
                        BreakPos := cPos;
                end
                else if QuoteChar = #0 then begin
                    QuoteChar := CurChar;
                    if ForceBreak and (cPos > LinePos) then
                        BreakPos := cPos -1; // Break before the QuoteChar
                end;
            end
            else if ForceBreak and (QuoteChar = #0) and (Col >= MaxCol) and
                    (BreakPos = 0) then begin
                BreakPos := cPos;
            end;
        //end;
        Inc(cPos);
        Inc(Col);

        if (not IsCharInSysCharSet(QuoteChar, QuoteChars)) and
           (ExistingBreak or
           ((Col > MaxCol) and (BreakPos >= LinePos))) then begin
            if ExistingBreak then
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1 - BreakLen)
            else
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1);
            if (not IsCharInSysCharSet(CurChar, QuoteChars)) or
               ExistingBreak then begin
                if (cPos <= LineLen) and (BreakPos + 1 = cPos) then begin
                    if _StrLComp(PChar(@Line[cPos]), #13#10, 2) = 0 then begin
                        if not ExistingBreak then begin
                            { Break due to one of the breaking chars found and CRLF follows }
                            Inc(cPos, 2);
                            Exit;
                        end;
                        { Empty line follows }
                        BreakPos := cPos - 1;
                    end;
                end;
            end;
            Inc(BreakPos);
            cPos := BreakPos;
            Exit;
        end;
    end;
    Result := Copy(Line, LinePos, MaxInt);
    cPos := MaxInt;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Unfold header lines                                                       } {AG}
{ RFC822 says "Unfolding is accomplished by regarding CRLF immediately      }
{ followed by a LWSP-char as equivalent to the LWSP-char."                  }
function UnFoldHdrLine(const S : String): String;
var
    I, J : Integer;
begin
    SetLength(Result, Length(S));
    J := 1;
    I := 1;
    while I <= Length(S) do begin
        if S[I] = #13 then begin
            if (I + 2 <= Length(S)) and
               (S[I + 1] = #10)     and
               (Ord(S[I + 2]) in [9, 32]) then begin
                Result[J] := #32;
                Inc(J);
                Inc(I, 2);
            end;
        end
        else begin
            Result[J] := S[I];
            Inc(J);
        end;
        Inc(I);
    end;
    SetLength(Result, J - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NeedsEncoding(const S : AnsiString) : Boolean;                  { AG }
var
    P : PAnsiChar;
    I : Integer;
begin
    P := Pointer(S);
    for I := 0 to Length(S) -1 do begin
        if (P[I] in [#0..#8, #11, #12, #14..#31]) or
           (P[I] > #126) then begin
            Result := True;
            Exit;
        end;
    end;
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function NeedsEncoding(const S : UnicodeString) : Boolean;               { AG }
var
    P : PWideChar;
    I : Integer;
begin
    P := Pointer(S);
    for I := 0 to Length(S) -1 do begin
        if (Word(P[I]) in [0..8, 11, 12, 14..31]) or
           (P[I] > #126) then begin
            Result := True;
            Exit;
        end;
    end;
    Result := False;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function NeedsEncodingPChar(S : PChar) : Boolean;
begin
    while S^ <> #0 do begin
        //if IsCharInSysCharSet(S^, [#0..#8, #11, #12, #14..#31, #127..#255]) then begin
        if (Ord(S^) in [0..8, 11, 12, 14..31]) or
           (Ord(S^) > 126) then begin
            Result := True;
            Exit;
        end;
        Inc(S);
    end;
    Result := False;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HdrEncodeInLine(const Input    : RawByteString;     { V7.13 was Ansi }
                         Specials       : TSysCharSet;
                         EncType        : AnsiChar;    { Either 'Q' or 'B' }
                         const CharSet  : AnsiString;  { e.g. 'iso-8859-1' input is already in this }
                         MaxCol         : Integer;
                         DoFold         : Boolean;
                         CodePage       : LongWord = CP_ACP;
                         IsMultiByteCP  : Boolean = FALSE): RawByteString;
const
    Suffix    = '?=';
    SuffixLen = 2;
    LineBreak = #13#10#09;
    Pad       = '=';
var
    Len,
    lPos,
    LenRes      : Integer;
    Prefix,
    Res         : AnsiString;
    NextCharIdx : Integer;
    OldCharIdx  : Integer;
begin
    Result := '';
    if DoFold and (MaxCol < 25) then
        MaxCol := 25;

    if Length(CharSet) < 2 then // MIME charset strings of length 2 exist.
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid CharSet: ' +
                                '' + Charset + '');
    if not (Byte(EncType) in [Ord('Q'), Ord('B')]) then
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid EncType: ' +
                                '' + EncType + '');
    Res    := '';
    Prefix := '=?' + _LowerCase(CharSet) + '?' + EncType + '?';
    Len    := Length(Input);
    lPos   := 1;

    if EncType = 'Q' then begin
        if lPos <= Len then
        begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + QuotedCharSet,
                                  True,
                                  lPos,
                                  DoFold,
                                  CodePage,
                                  IsMultiByteCP);
            if Length(Res) = 0 then
                Exit;
            if Res[Length(Res)] = '=' then
                SetLength(Res, Length(Res) - 1);
            Result := Prefix + Res + Suffix;
        end;
        while lPos <= Length(Input) do begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + QuotedCharSet,
                                  True,
                                  lPos,
                                  DoFold,
                                  CodePage,
                                  IsMultiByteCP);
            if (Length(Res) > 0) then begin
                if Res[Length(Res)] = '=' then
                    SetLength(Res, Length(Res) - 1);
                Result := Result + LineBreak {#13#10#09} + Prefix + Res  + Suffix;
            end;
        end;
    end
    else begin
        { Base64 }
        { taken from function B64Encode and modified slightly     }

        if not DoFold then
            MaxCol := 1022;

        { Make MaxCol 2 bytes smaller. Just to be sure to not get }
        { wrapped later on with default line length.              }
        Dec(MaxCol, 2);

        LenRes := Length(Prefix) + SuffixLen;

        if IsMultibyteCP then
        begin
            NextCharIdx := lPos;
            OldCharIdx  := lPos;
            MaxCol := MaxCol - LenRes; // sub decoration-length
            while NextCharIdx <= Len do
            begin
                NextCharIdx := B64SafeMaxIndex(CodePage, Input,
                                                         OldCharIdx, MaxCol);
                if NextCharIdx > OldCharIdx then
                begin
                    Res := Res + Prefix +
                           Base64Encode(PAnsiChar(Input) + (OldCharIdx - 1),
                                        NextCharIdx - OldCharIdx) + Suffix;
                    if NextCharIdx <= Len then
                        Res := Res + LineBreak;
                    OldCharIdx := NextCharIdx;
                end
                else
                    Break;
            end;
            Result := Res;
            Exit;
        end;

        Res := Res + Prefix;

        while lPos <= Len do begin
            if (LenRes + 4 > MaxCol) then begin
                Res := Res + Suffix + LineBreak {#13#10#09} + Prefix;
                LenRes := Length(Prefix) + SuffixLen;
            end;
            Res := Res + Base64OutA[(Byte(Input[lPos]) and $FC) shr 2];
            if (lPos + 1) <= Len  then begin
                Res := Res + Base64OutA[((Byte(Input[lPos]) and $03) shl 4) +
                                       ((Byte(Input[lPos + 1]) and $F0) shr 4)];
                if (lPos + 2) <= Len then begin
                    Res := Res + Base64OutA[((Byte(Input[lPos + 1]) and $0F) shl 2) +
                                           ((Byte(Input[lPos + 2]) and $C0) shr 6)];
                    Res := Res + Base64OutA[(Byte(Input[lPos + 2]) and $3F)];
                end
                else begin
                    Res := Res + Base64OutA[(Byte(Input[lPos + 1]) and $0F) shl 2];
                    Res := Res + Pad;
                end
            end
            else begin
                 Res := Res + Base64OutA[(Byte(Input[lPos]) and $03) shl 4];
                 Res := Res + Pad + Pad;
            end;
            Inc(LenRes, 4);
            Inc(lPos, 3);
        end;
        Result := Res + Suffix;
    end;
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function HdrEncodeInLine(const Input    : UnicodeString;
                         Specials       : TSysCharSet;
                         EncType        : WideChar;       { Either 'Q' or 'B' }
                         const CharSet  : UnicodeString;  { e.g. 'iso-8859-1' }
                         MaxCol         : Integer;
                         DoFold         : Boolean;
                         Codepage       : LongWord = CP_ACP;
                         IsMultiByteCP  : Boolean = FALSE): UnicodeString;
begin
    Result := HdrEncodeInLine(AnsiString(Input), Specials, AnsiChar(EncType),
                              AnsiString(CharSet), MaxCol, DoFold, Codepage,
                              IsMultiByteCP);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V7.13 Angus enoded UTF-16 into desired characters set, then enode into MIME Header }
function HdrEncodeInLineEx(const Input : UnicodeString;
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : WideChar;    { Either 'Q' or 'B'        }
                         CodePage      : LongWord;    { Input will be encoded into this CharSet }
                         MaxCol        : Integer;
                         DoFold        : Boolean;
                         IsMultiByteCP : Boolean = FALSE): RawByteString;
var
    CharSet: AnsiString;
begin
    CharSet := AnsiString(CodePageToMimeCharsetString(CodePage));
    Result := HdrEncodeInLine(UnicodeToAnsi(Input,CodePage), Specials,
                              AnsiChar(EncType), CharSet, MaxCol, DoFold,
                              CodePage, IsMultiByteCP);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Piotr Hellrayzer Dalek <enigmatical@interia.pl>, AG                                                                    }
{ Use it to code message text that includes extended ASCII chars, passing    }
{ empty Specials '[]' will work mostly.                                      }
{ Param MaxCol should be set to 1 below max. line length                     }
function StrEncodeQP(
    const Input   : RawByteString;
    MaxCol        : Integer;
    Specials      : TSysCharSet;
    CodePage      : LongWord = CP_ACP;
    IsMultiByteCP : Boolean = FALSE) : String;
var
    cPos, rPos, lPos, ResLen, NextCharIdx : Integer;

    procedure InsertSoftBreak;
    begin
        { Need to grow? Should never happen }
        if cPos + 3 > ResLen then begin
            Inc(ResLen, MaxCol);
            SetLength(Result, ResLen);
        end;
        Result[cPos] := '=';
        Inc(cPos);
        Result[cPos] := #13;
        Inc(cPos);
        Result[cPos] := #10;
        Inc(cPos);
        lPos := 1;
    end;
    
begin;
    if MaxCol < 16 then MaxCol := 16;
    { Allocate max. possible result string        }
    { Max. possible result length w/o soft breaks }
    ResLen :=  Length(Input) * 3;
    { Add length max. possible breaks             }
    while ResLen mod MaxCol <> 0 do
        Inc(ResLen);
    Inc(ResLen, (ResLen div MaxCol) * 3);

    SetLength(Result, ResLen);

    cPos   := 1;
    lPos   := 1;
    NextCharIdx := 1;
    for rPos := 1 to Length(Input) do begin

        if IsMultiByteCP and (rPos = NextCharIdx) then begin
            NextCharIdx := IcsNextCharIndex(Input, rPos, CodePage);
            if (NextCharIdx > rPos + 1) and
               ((NextCharIdx - rPos) * 3 + lPos > MaxCol) then
                InsertSoftBreak;
        end;

        if (Ord(Input[rPos]) > 126)  or
           (Ord(Input[rPos]) < 32)   or
           (Input[rPos]      = '=')  or
           (Input[rPos] in Specials) then begin            
            Result[cPos] := '=';
            Inc(cPos);
            Result[cPos] := HexTable[(Ord(Input[rPos]) shr 4) and 15];
            Inc(cPos);
            Result[cPos] := HexTable[Ord(Input[rPos]) and 15];
            Inc(cPos);
            Inc(lPos, 3);
            if lPos >= MaxCol then
                InsertSoftBreak;
        end
        else begin
            Result[cPos] := Char(Input[rPos]); // No problem here since plain US-ASCII
            Inc(cPos);
            Inc(lPos);
            if lPos >= MaxCol then
                InsertSoftBreak;
        end;
    end;
    Setlength(Result, cPos - 1);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function StrEncodeQP(const Input    : UnicodeString;
                     MaxCol         : Integer;
                     Specials       : TSysCharSet;
                     ACodePage      : LongWord;
                     IsMultiByteCP  : Boolean = FALSE) : UnicodeString;
begin;
    Result := StrEncodeQP(UnicodeToAnsi(Input, ACodePage), MaxCol, Specials,
                          ACodePage, IsMultiByteCP);
end;
{$ENDIF}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to StrEncodeQP, returns just a coded line                          }
function StrEncodeQPEx(
    const Buf     : RawByteString;
    MaxCol        : Integer;
    Specials      : TSysCharSet;
    ShortSpace    : Boolean;
    var cPos      : Integer;
    DoFold        : Boolean;
    CodePage      : LongWord = CP_ACP;
    IsMultibyteCP : Boolean = FALSE) : RawByteString;
var
    lPosRes : Integer;
    NextCharIdx : Integer;
begin
    lPosRes := 1;
    NextCharIdx := cPos;
    if not DoFold then
        MaxCol := Length(Buf) * 3;                                     { AG }
    if MaxCol < 16 then MaxCol := 16;                                  { AG }
    SetLength(Result, MaxCol);
    while cPos <= Length(Buf) do begin

        if IsMultibyteCP and (cPos = NextCharIdx) then
        begin
            NextCharIdx := IcsNextCharIndex(Buf, cPos, CodePage);
            if (NextCharIdx > cPos + 1) and
               ((NextCharIdx - cPos) * 3 + lPosRes >= MaxCol - 2) then
            begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Break;
            end;
        end;

        if (Ord(Buf[cPos]) > 126)  or
           (Ord(Buf[cPos]) < 32)   or
           (Buf[cPos] in Specials) or
           (Buf[cPos] = '=') then begin
            if (Buf[cPos] = ' ') and ShortSpace then begin
                Result[lPosRes] := '_';
                Inc(lPosRes);
                Inc(cPos);
            end
            else if (lPosRes < MaxCol - 2) then begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Result[lPosRes] := HexTableA[(Ord(Buf[cPos]) shr 4) and 15];
                Inc(lPosRes);
                Result[lPosRes] := HexTableA[Ord(Buf[cPos]) and 15];
                Inc(lPosRes);
                Inc(cPos);
            end
            else begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Break;
            end;
        end
        else
            if lPosRes < MaxCol then begin
                Result[lPosRes] := Buf[cPos];
                Inc(lPosRes);
                Inc(cPos);
            end
            else begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Break;
            end;
    end;
    SetLength(Result, lPosRes - 1);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function StrEncodeQPEx(const Buf   : UnicodeString;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean;
                       var cPos    : Integer;
                       DoFold      : Boolean) : UnicodeString;
begin
    Result := StrEncodeQPEx(UnicodeToAnsi(Buf), MaxCol, Specials,
                            ShortSpace, cPos, DoFold);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ RFC822 - 3.1.1.  LONG HEADER FIELDS                                       }
{ This is just my (AG) interpretation of folding long header lines.         }
{ Probably further BreakCharsSet are possible here.                          }
{ However before you modify this procedure you should refer to RFC822.       }
{ Also note that header lines may be encoded 'in-line' as described in       }
{ RFC2047. The passed HdrLine String *MUST not include CRLF except they      }
{ are followed by one of the space chars, means that a already folded        }
{ line should work. If a string doesn't include one of the BreakChars        }
{ it won't fold to the next line!                                            }
procedure FoldHdrLine(
    HdrLines      : TStrings;
    const HdrLine : String);
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(HdrLine) then
        HdrLines.Add(_Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                          BreakCharsSet, SmtpDefaultLineLength, [], rPos)));
    while rPos <= Length(HdrLine) do
        HdrLines.Add(#09 + _Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                                BreakCharsSet, SmtpDefaultLineLength, [], rPos)))
end;
{$ENDIF}


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FoldHdrLine(
    HdrLines      : TStrings;
    const HdrLine : RawByteString;
    ACodePage     : LongWord = CP_ACP;
    IsMultiByteCP : Boolean = FALSE);
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(HdrLine) then
        HdrLines.Add(_Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                    BreakCharsSet, SmtpDefaultLineLength, [], rPos,
                    FALSE, ACodePage, IsMultiByteCP)));
    while rPos <= Length(HdrLine) do
        HdrLines.Add(#09 + _Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                    BreakCharsSet, SmtpDefaultLineLength, [], rPos,
                    FALSE, ACodePage, IsMultiByteCP)));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} {AG}
function FoldString(const Input : RawByteString;
    BreakCharsSet : TSysCharSet;
    MaxCol        : Integer;
    ACodePage     : LongWord = CP_ACP;
    IsMultiByteCP : Boolean = FALSE): RawByteString;
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(Input) then
        Result := _Trim(IcsWrapTextEx(Input, AnsiString(#13#10#09),
                       BreakCharsSet, MaxCol, [], rPos,
                       False, ACodePage, IsMultiByteCP));
    while rPos <= Length(Input) do
        Result := Result + AnsiString(#13#10#09) +
                  _Trim(IcsWrapTextEx(Input, AnsiString(#13#10#09),
                                      BreakCharsSet, MaxCol, [], rPos,
                                      False, ACodePage, IsMultiByteCP));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function FoldString(const Input   : UnicodeString;
                    BreakCharsSet : TSysCharSet;
                    MaxCol        : Integer): UnicodeString;
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(Input) then
        Result := _Trim(IcsWrapTextEx(Input, #13#10#09,
                       BreakCharsSet, MaxCol, [], rPos));
    while rPos <= Length(Input) do
        Result := Result + #13#10#09 + _Trim(IcsWrapTextEx(Input, #13#10#09,
                                                          BreakCharsSet,
                                                          MaxCol,
                                                          [], rPos))
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CalcBase64AttachmentGrow(FileSize: Int64): Int64;
var
    LineCount : Int64;
begin
    if FileSize > 0 then
    begin
        while FileSize mod 3 > 0 do
            Inc(FileSize);
        Result := ((FileSize div 3) * 4);
        { Add line break byte count. }
        if Result mod FILE_BASE64_LINE_LENGTH  > 0 then
            LineCount := (Result div FILE_BASE64_LINE_LENGTH) + 1
        else
            LineCount := Result div FILE_BASE64_LINE_LENGTH;
        Inc(Result, LineCount * 2);
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeMbcsInline(
  CodePage        : LongWord;
  const Charset   : String;
  EncType         : Char;
  Body            : PWideChar;
  Len             : Integer;
  DoFold          : Boolean;
  MaxLen          : Integer): AnsiString;
var
    Suffix  : AnsiString;
    Prefix  : AnsiString;
    TempStr : AnsiString;
    DecoLen, CurMaxLen, BodyLen, QPGrow, LineCnt : Integer;
    P       : PWideChar;
    QPChars : TSysCharSet;
    Csc     : TIcsCsc;
    DoQP    : Boolean;

    function GetQPGrow(Str: PAnsiChar; Len: Integer; var NeedsQP: Boolean): Integer;
    var
        C: AnsiChar;
        I : Integer;
    begin
        NeedsQP:= FALSE;
        Result := 0;
        for I := 1 to Len do begin
            C := Str^;
            if (C in QPChars) then
            begin
                if C <> #$20 then
                    Inc(Result, 2);
                NeedsQP := TRUE;
            end;
            Inc(Str);
        end;
    end;

    function EncQP(const S: RawByteString) : AnsiString;
    const
        HexTable : array[0..15] of AnsiChar =
          ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
    var
        SLen, I : Integer;
        SrcPtr, DstPtr : PAnsiChar;
        C : AnsiChar;
    begin
        SLen := Length(S);
        SrcPtr := PAnsiChar(S);
        SetLength(Result, SLen + QPGrow);
        DstPtr := PAnsiChar(Result);
        for I := 1 to SLen do begin
            C := SrcPtr^;
            if C in QPChars then begin
                if C = #$20 then
                    DstPtr^ := '_'
                else begin
                    DstPtr^ := '=';
                    Inc(DstPtr);
                    DstPtr^ := HexTable[(Ord(C) shr 4) and 15];
                    Inc(DstPtr);
                    DstPtr^ := HexTable[Ord(C) and 15];
                end;    
            end
            else
                DstPtr^ := C;

            Inc(SrcPtr);
            Inc(DstPtr);
        end;
    end;

    function ConvertClosestTempStr(var FromValue: Integer;
        const AMax: Integer): Integer;
    var
        L, H, I, CurLen, LQPGrow: Integer;
        LTmp : AnsiString;
        LNeedsQP : Boolean;
    begin
        { Trial and error, encode and see if result-length matches }
        if FromValue <= AMax then
        begin
            LQPGrow := 0;
            LNeedsQP:= FALSE;
            {CurLen := IcsWcToMb(CodePage, 0, P, FromValue, nil, 0, nil, nil);
            SetLength(LTmp, CurLen);
            CurLen := IcsWcToMb(CodePage, 0, P, FromValue, Pointer(LTmp),
                                CurLen, nil, nil);}

            CurLen := Csc.FromWc(0, P, FromValue * SizeOf(WideChar), nil, 0);
            SetLength(LTmp, CurLen);
            CurLen := Csc.FromWc(0, P, FromValue * SizeOf(WideChar), Pointer(LTmp), CurLen);

            SetLength(LTmp, CurLen);
            if (CurLen > 0) and (CurLen <= AMax) then
            begin
                if EncType = 'Q' then begin
                    LQPGrow := GetQPGrow(PAnsiChar(LTmp), CurLen, LNeedsQP);
                    Inc(CurLen, LQPGrow);
                end
                else if (EncType = 'B') then begin
                    { Calculate Base64 length }
                    while CurLen mod 3 > 0 do
                        Inc(CurLen);
                    CurLen := (CurLen div 3) * 4;
                end;
            end;
            if CurLen <= AMax then begin
                Result := CurLen;
                QPGrow := LQPGrow;
                TempStr := LTmp;
                DoQP := LNeedsQP;
                Exit;
            end;
        end;

        { The encoded string was too long. Use some kind of       }
        { "binary-closest-match-search" to avoid too many trials. }

        L := 1;
        H := Min(FromValue, AMax);
        FromValue  := H;
        Result     := 0;
        LQPGrow    := 0;
        LNeedsQP   := FALSE;

        while L <= H do begin
            I := (L + H) shr 1;
            {CurLen := IcsWcToMb(CodePage, 0, P, I, nil, 0, nil, nil);
            SetLength(LTmp, CurLen);
            CurLen := IcsWcToMb(CodePage, 0, P, I, Pointer(LTmp),
                                CurLen, nil, nil);}
            CurLen := Csc.FromWc(0, P, I * SizeOf(WideChar), nil, 0);
            SetLength(LTmp, CurLen);
            CurLen := Csc.FromWc(0, P, I * SizeOf(WideChar), Pointer(LTmp), CurLen);

            SetLength(LTmp, CurLen);
            if CurLen > 0 then begin
                if EncType = 'Q' then begin
                    LQPGrow := GetQPGrow(PAnsiChar(LTmp), CurLen, LNeedsQP);
                    Inc(CurLen, LQPGrow);
                end
                else if (EncType = 'B') then begin
                    { Calculate Base64 length }
                    while CurLen mod 3 > 0 do
                        Inc(CurLen);
                    CurLen := (CurLen div 3) * 4;
                end;
            end;
            if CurLen <= AMax then begin
                FromValue  := I;
                Result     := CurLen;
                QPGrow     := LQPGrow;
                DoQP       := LNeedsQP;
                TempStr    := LTmp;
                if (CurLen = AMax) {or (CurLen + 1 = AMax) or
                   (CurLen + 2 = AMax)} then
                    Break;
                L := I + 1;
            end
            else
                H := I - 1;
        end;
    end;

begin
    Suffix  := '?=';
    Prefix  := '=?' + _LowerCase(AnsiString(CharSet)) + '?' + AnsiChar(EncType) + '?';
    DecoLen := Length(Prefix) + Length(Suffix);
    BodyLen := Len;
    P       := Body;
    LineCnt := 1;
    if not DoFold then
        MaxLen := 1022;
    Dec(MaxLen, 4);
    QPChars := SpecialsRFC822 + QuotedCharSet + [#0..#31, #127..#255];
    CurMaxLen := MaxLen - DecoLen;
    Csc := TIcsCsc.Create(CodePage);
    try
        while BodyLen > 0 do
        begin
            DoQP := FALSE;
            if ConvertClosestTempStr(BodyLen, CurMaxLen) = 0 then
                Exit;
            if (EncType = 'Q') and DoQP then
            begin
                TempStr := EncQP(TempStr);
                QPGrow := 0;
            end
            else if (EncType = 'B') then
                TempStr := Base64Encode(TempStr);

            if LineCnt = 1 then
                Result := Prefix + TempStr + Suffix
            else
                Result := Result + #13#10#9 + Prefix + TempStr + Suffix;
            Inc(LineCnt);
            Inc(P, BodyLen);
            Dec(Len, BodyLen);
            BodyLen := Len;
        end;
    finally
        Csc.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeMbcsInline(
  CodePage        : LongWord;
  const Charset   : String;
  EncType         : Char;
  Body            : PAnsiChar;
  Len             : Integer;
  DoFold          : Boolean;
  MaxLen          : Integer): AnsiString;
var
    Pu  : PWideChar;
    Res : Integer;
begin
    Res := IcsMbToWc(IcsSystemCodePage, 0, Body, Len, nil, 0);
    GetMem(Pu, Res * SizeOf(WideChar));
    try
        Res := IcsMbToWc(IcsSystemCodePage, 0, Body, Len, Pu, Res);
        if Res > 0 then
            Result := EncodeMbcsInline(CodePage, Charset, EncType, Pu, Res,
                                       DoFold, MaxLen)
        else
            Result := '';
    finally
        FreeMem(Pu);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
