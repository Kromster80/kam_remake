{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  TIcsCsc (Csc => Charset Context) provides string conversion and
              improved (Ansi) multi-byte character set routines.
Creation:     Apr 25, 2010
Version:      8.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2010 by Arno Garrels, contributed to ICS

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
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsCsc;

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
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

{ Iconv lib is loaded dynamically at run-time in Windows  }
{.$DEFINE USE_ICONV}

{ MLang.DLL is loaded dynamically at run-time, used only  }
{ in Windows with true MBCS and if iconv is not available }
{$DEFINE USE_MLANG}

{$IFNDEF MSWINDOWS}
  {$IFDEF USE_MLANG}
    {$UNDEF USE_MLANG}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF USE_ICONV}
      OverbyteIcsIconv,
  {$ENDIF}
  {$IFDEF USE_MLANG}
      OverbyteIcsMLang,
  {$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
    Posix.Base, Posix.SysTypes, Posix.Errno, Posix.Iconv,
    Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Math{$ELSE}Math{$ENDIF},
    OverbyteIcsTypes,
    OverbyteIcsUtils;

const
    ICS_ERR_EINVAL    =  -1;  // An incomplete byte sequence has been encountered in the input.
    ICS_ERR_E2BIG     =  -2;  // There is not sufficient room at *outbuf.
    ICS_ERR_EILSEQ    =  -10; // An invalid byte sequence has been encountered in the input.
    // ICS_ERR_EILSEQ: Actually value of ( ICS_ERR_EILSEQ minus invalid sequence size )

{$IFDEF USE_ICONV}
    {$IFNDEF MSWINDOWS}
        Load_Iconv    = TRUE; // It's statically loaded
    {$ENDIF}
type
    TIcsIconvCompatFlags = set of (icfUseDefaultUnicodeChar,
                                   icfBreakOnInvalidSequence);
{$ENDIF}

type
    TIcsCharsetType =(ictSbcs, ictDbcs, ictMbcs, ictMbcsUnicode, ictUnicode);

    TIcsNextCodePointFlags = set of (ncfSkipEILSEQ, ncfSkipEINVAL);

    TIcsCsc = class; //Forward

    TIcsCpSizeFunc  = function(Csc: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
    TIcsConvertFunc = function(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer; InSize: Integer;
                               OutBuf: Pointer; OutSize: Integer): Integer;
    TIcsCsc = class(TObject)
    private
        FCodePage           : LongWord;
        FCharSetType        : TIcsCharsetType;
        FDefaultUnicodeChar : WideChar;
        FDefaultAnsiChar    : AnsiChar;
        FMinCpSize          : Integer;           // Minimum codepoint size
        FLeadBytes          : TIcsDbcsLeadBytes; // DBCS lead bytes
        FToWcFunc           : TIcsConvertFunc;   // Convert to UTF-16 function pointer
        FFromWcFunc         : TIcsConvertFunc;   // Convert from UTF-16 function pointer
        FCpSizeFunc         : TIcsCpSizeFunc;    // Codepoint size function pointer
    {$IFDEF USE_ICONV}
        FIconvCompatFlags   : TIcsIconvCompatFlags;
    {$ENDIF}
    protected
        FToWcShiftState     : LongWord;        // Used with stateful charsets
        FFromWcShiftState   : LongWord;        // Used with stateful charsets
    {$IFDEF USE_ICONV}
        FFromWcIconvCtx     : iconv_t;         // Iconv conversion descriptor/handle
        FToWcIconvCtx       : iconv_t;         // Iconv conversion descriptor/handle
    {$ENDIF}
        procedure SetCodePage(const Value: LongWord); virtual;
        procedure Init; virtual;
    public
        constructor Create(CodePage: LongWord); virtual;
        destructor Destroy; override;
        function  GetBomBytes: TBytes;
        function  GetBufferEncoding(const Buf: Pointer; BufSize: Integer; Detect: Boolean): Integer;

        procedure ClearToWcShiftState;   // Reset ShiftState
        procedure ClearFromWcShiftState; // Reset ShiftState
        procedure ClearAllShiftStates;

        function GetNextCodePointSize(Buf: Pointer; BufSize: Integer; Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer;
        function GetNextCodePoint(Buf: Pointer; BufSize: Integer; Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Pointer;

        function FromWc(Flags: LongWord; InBuf: Pointer; InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer; virtual;
        function ToWc(Flags: LongWord; InBuf: Pointer; InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer; virtual;

        property CharSetType: TIcsCharsetType read FCharSetType;
        property LeadBytes: TIcsDbcsLeadBytes read FLeadBytes;
        property CodePage: LongWord read FCodePage write SetCodePage;
        property DefaultUnicodeChar: WideChar read FDefaultUnicodeChar;
        property DefaultAnsiChar: AnsiChar read FDefaultAnsiChar;
        property MinCpSize: Integer read FMinCpSize;
    end;

    TIcsCscStr = class(TIcsCsc)
    public
        function GetNextCodePointIndex(const S: RawByteString; Index: Integer; Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer; overload;
        function GetNextCodePointIndex(const S: UnicodeString; Index: Integer; Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer; overload;
    end;

    function IcsCscGetWideCharCount(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer; out BytesLeft: Integer): Integer;
    function IcsCscGetWideChars(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer; Chars: PWideChar; WCharCount: Integer): Integer;
    function IcsCscBufferToUnicodeString(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer; out BytesLeft: Integer): UnicodeString;
    function IcsCscToWcString(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer): UnicodeString;

implementation

resourcestring
    sInvalidEncoding = 'Codepage "%d" not supported.';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetDBCSLeadBytes(CodePage: LongWord): TIcsDbcsLeadBytes; {$IFDEF USE_INLINE} inline; {$ENDIF}
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
function StrByteCountUcs4(Ucs4Str: PLongWord): Integer;
var
    BeginP : Pointer;
begin
    if Ucs4Str <> nil then
    begin
        BeginP := Ucs4Str;
        while Ucs4Str^ <> $00000000 do
            Inc(Ucs4Str);
        Result := INT_PTR(Ucs4Str) - INT_PTR(BeginP);
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckDbcCpSize(Csc: TIcsCsc; Buf: Pointer;
  BufSize: Integer): Integer;
begin
    if PAnsiChar(Buf)^ in Csc.Leadbytes then
    begin
        if BufSize < 2 then
            Result := ICS_ERR_EINVAL
        else begin
            //if Csc.ToWc(MB_ERR_INVALID_CHARS, Buf, 2, nil, 0) > 0 then
                Result := 2
            {else
                Result := ICS_ERR_EILSEQ - 2;}
        end;
    end
    else
        Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUtf8CpSize(Csc: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
begin
    Result := IcsUtf8Size(PByte(Buf)^);
    if Result > BufSize then
        Result := ICS_ERR_EINVAL
    else if Result = 0 then
        Result := ICS_ERR_EILSEQ -1
    else if (Result > 1) and (CharSetDetect(Buf, Result) = cdrUnknown) then
        Result := ICS_ERR_EILSEQ - 1 {Result};
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    //Utf7_Set_D : TSysCharset = [#39..#41, #44..#58, #63, #65..#90, #97..#122];
    //Utf7_Set_O : TSysCharset = [#33..#38, #42, #59..#62, #64, #91, #93..#96, #123..#125];
    Utf7_Set_O_LAZY : TSysCharset = [#09, #10, #13, #32..#42, #44..#91, #93..#125];
    Utf7_Set_B : TSysCharset = ['0'..'9', '+', '/', 'A'..'Z', 'a'..'z'];
    Utf7_Set_C : TSysCharset = ['0'..'9', '+', '/', '-', 'A'..'Z', 'a'..'z'];


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUtf7CpSize(Csc: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
var
    B : PAnsiChar;
    I : Integer;
begin
    B := Buf;
    Result := ICS_ERR_EINVAL;
    I := 0;
    if Csc.FToWcShiftState = 0 then
    begin
        if B[0] in Utf7_Set_O_LAZY then
            Result := 1
        else begin
            if B[0] = '+' then
            begin
                Csc.FToWcShiftState := 1;
                Inc(I);
                if (I < BufSize) and (not (B[I] in Utf7_Set_C)) then
                begin
                    Result := ICS_ERR_EILSEQ - 2;
                    Exit;
                end;
                Csc.FToWcShiftState := 2;
                while I < BufSize do
                begin
                    if not (B[I] in Utf7_Set_B) then
                    begin
                        Csc.FToWcShiftState := 0;
                        if not (B[I] in Utf7_Set_O_LAZY) then
                            Result := ICS_ERR_EILSEQ - (I + 1)
                        else        
                            Result := I + 1;
                        Exit;
                    end;
                    Inc(I);
                end;
            end
            else
                Result := ICS_ERR_EILSEQ -1;
        end;
    end
    else begin
        if (Csc.FToWcShiftState = 1) then
        begin
            if (not (B[0] in Utf7_Set_C)) then
            begin
                Result := ICS_ERR_EILSEQ -1;
                Exit;
            end
            else
                Csc.FToWcShiftState := 2;
        end;
        while I < BufSize do
        begin
            if not (B[I] in Utf7_Set_B) then
            begin
                Csc.FToWcShiftState := 0;
                if not (B[I] in Utf7_Set_O_LAZY) then
                    Result := ICS_ERR_EILSEQ - (I + 1)
                else        
                    Result := I + 1;
                Exit;
            end;
            Inc(I);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckSbcCpSize(Dummy: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
begin
    Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckGB18030CpSize(Dummy: TIcsCsc; ABuf: Pointer; Bufsize: Integer): Integer;
var
    Buf: PAnsiChar;
begin
    //(CP = 54936) Windows XP and later
    Buf := ABuf;
    Result := ICS_ERR_EINVAL;
    if Buf[0] <= #$7F then
        Result := 1
    else if Buf[0] in [#$81..#$FE] then // Either 2 or 4 bytes
    begin
        if BufSize >= 2 then
        begin
            if Buf[1] in [#$40..#$FE] then
                Result := 2
            else if Buf[1] in [#$30..#$39] then
            begin
                if BufSize >= 4 then
                begin
                    if (Buf[2] in [#$81..#$FE]) and (Buf[4] in [#$30..#$39]) then
                        Result := 4
                    else
                        Result := ICS_ERR_EILSEQ - 1;
                end;
                // else ICS_ERR_EINVAL
            end
            else
                Result := ICS_ERR_EILSEQ - 1;
        end;
    end
    else
        Result := ICS_ERR_EILSEQ - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckEucJpCpSize(Dummy: TIcsCsc; ABuf: Pointer; BufSize: Integer): Integer;
var
    Buf : PAnsiChar;
begin
    //51932, // (euc-jp  EUC Japanese MBCS Max Size: 3 mlang.dll only
    Buf := ABuf;
    Result := ICS_ERR_EINVAL;
    if Buf[0] < #$80 then //* ASCII */
        Result := 1
    else if (Buf[0] = #$8E) then //* JIS X 0201 */
    begin
        if (BufSize < 2) then
            Exit; //ICS_ERR_EINVAL
        if Buf[1] in [#$A1..#$DF] then
            Result := 2
        else
            Result := ICS_ERR_EILSEQ - 1;
    end
    else if Buf[0] = #$8F then //* JIS X 0212 */
    begin
        if (BufSize < 3) then
            Exit; //ICS_ERR_EINVAL
        if (Buf[1] in [#$A1..#$FE]) and (Buf[2] in [#$A1..#$FE]) then
            Result := 3
        else
            Result := ICS_ERR_EILSEQ - 1;
    end
    else //* JIS X 0208 */
    begin
        if (BufSize < 2) then
            Exit; //ICS_ERR_EINVAL
        if (Buf[0] in [#$A1..#$FE]) and (Buf[1] in [#$A1..#$FE]) then
            Result := 2
        else
            Result := ICS_ERR_EILSEQ - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUtf32CpSize(Csc: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
var
    Ch : LongWord;
begin
    if BufSize < 4 then
        Result := ICS_ERR_EINVAL
    else begin
        if Csc.CodePage = CP_UTF32Be then
            Ch := IcsSwap32(PLongWord(Buf)^)
        else
            Ch := PLongWord(Buf)^;

        if (($D800 <= Ch) and (Ch <= $DFFF)) or (Ch > $10FFFF) then
            Result := ICS_ERR_EILSEQ - 4
        else
            Result := 4;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUtf16CpSize(Csc: TIcsCsc; Buf: Pointer; BufSize: Integer): Integer;
var
    Ch : Word;
begin
    if BufSize < 2 then
        Result := ICS_ERR_EINVAL
    else begin
        if Csc.CodePage = CP_UTF16Be then
            Ch := IcsSwap16(PWord(Buf)^)
        else
            Ch := PWord(Buf)^;
        if (Ch >= $D800) and (Ch <= $DFFF) then
        begin
            if (Ch >= $DC00) then
                Result := ICS_ERR_EILSEQ - 2
            else if BufSize < 4 then
                Result := ICS_ERR_EINVAL
            else begin
                if Csc.CodePage = CP_UTF16Be then
                    Ch := IcsSwap16(PWord(PAnsiChar(Buf) + 2)^)
                else
                    Ch := PWord(PAnsiChar(Buf) + 2)^;
                if not ((Ch >= $DC00) and (Ch <= $DFFF)) then
                    Result := ICS_ERR_EILSEQ - 4
                else
                    Result := 4;
            end;
        end
        else
            Result := 2;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckMbcCpSize(Csc: TIcsCsc;
  Buf: Pointer; BufSize: Integer): Integer;
var
    Cnt : Integer;
    Tmp : array [0..3] of WideChar;
begin
    Cnt := 0;
    while TRUE do
    begin
        Inc(Cnt);
        if Cnt > BufSize then
        begin
            Result := ICS_ERR_EINVAL;
            Exit;
        end;
        Result := Csc.FToWcFunc(Csc, 0, Buf, Cnt, @Tmp, SizeOf(Tmp));
        if Result > 0 then
        begin
            { According to MS the default Unicode char is inserted if the }
            { source is not the default char and translation failed.      }
            if Tmp[0] = Csc.DefaultUnicodeChar then
            begin
                // Windows.OutputDebugStringW(PWideChar(IntToHex(Ord(tmp[0]), 4)));
                if ((Cnt = 1) and (Ord(PAnsiChar(Buf)^) <> Ord(Tmp[0]))) then
                    Result := ICS_ERR_EILSEQ - 1
                else if ((Cnt > 1) and (Tmp[0] = #$30FB)) and
                    (not ((PAnsiChar(Buf)[Cnt - 2] = #$81) and
                          (PAnsiChar(Buf)[Cnt - 1] = #$45))) then
                        Result := ICS_ERR_EILSEQ - Cnt
                else
                    Result := Cnt;
            end
            else
                Result := Cnt;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_ICONV}
function CheckMbcCpSizeIconv(Csc: TIcsCsc;
  InBuf: Pointer; InSize: Integer): Integer;
var
    InCnt, OutCnt: Integer;
    Cnt : Integer;
    Tmp : array [0..5] of WideChar;
    SrcPtr, DstPtr : Pointer;
begin
    if InSize < 0 then
        InSize := StrLen(PAnsiChar(InBuf)) + 1;

    DstPtr := @Tmp;
    Cnt := 0;
    while TRUE do
    begin
        Inc(Cnt);
        if Cnt > InSize then
        begin
            Result := ICS_ERR_EINVAL;
            Exit;
        end;
        InCnt  := Cnt;
        OutCnt := SizeOf(Tmp);
        SrcPtr := InBuf;
        Result := iconv(Csc.FToWcIconvCtx, @SrcPtr, @InCnt, @DstPtr, @OutCnt);
        if Result = -1 then
        begin            
            case Errno of
                E2BIG: { There is not sufficient room at *outbuf }
                    begin
                        Result := ICS_ERR_EINVAL;
                        Exit;
                    end;
                EILSEQ: { An invalid byte sequence has been encountered in the input }
                    begin
                        Result := ICS_ERR_EILSEQ - Cnt;
                        Exit;
                    end;
                EINVAL:
                    begin
                        //Result := ICS_ERR_EINVAL;
                    end;
            end;
        end
        else
            if OutCnt < SizeOf(Tmp) then
            begin
                Result := Cnt - InCnt;
                Exit;
            end;
            if (Cnt = Insize) and (InCnt = 0) then // shift sequence at the end of buffer
            begin
                Result := Cnt;
                Exit;
            end;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
{$IFDEF USE_MLANG}
function CheckMbcCpSizeMLang(Csc: TIcsCsc;
  InBuf: Pointer; InSize: Integer): Integer;
var
    InCnt, OutCnt: Integer;
    Cnt : Integer;
    Tmp : array [0..5] of WideChar;
begin
    if InSize < 0 then
        InSize := StrLen(PAnsiChar(InBuf)) + 1;

    Cnt := 0;
    while TRUE do
    begin
        Inc(Cnt);
        if Cnt > InSize then
        begin
            Result := ICS_ERR_EINVAL;
            Exit;
        end;
        InCnt  := Cnt;
        OutCnt := SizeOf(Tmp) * SizeOf(WideChar);
        case ConvertINetMultibyteToUnicode(Csc.FToWcShiftState, Csc.CodePage,
                                          InBuf, InCnt,
                                          @Tmp, OutCnt) of
        S_OK    :
            begin
                if OutCnt > 0 then
                begin
                    if Tmp[0] = Csc.DefaultUnicodeChar then
                    begin
                        // Windows.OutputDebugStringW(PWideChar(IntToHex(Ord(tmp[0]), 4)));
                        if ((Cnt = 1) and (Ord(PAnsiChar(InBuf)^) <> Ord(Tmp[0]))) then
                            Result := ICS_ERR_EILSEQ - 1
                        else if ((Cnt > 1) and (Tmp[0] = #$30FB)) and
                            (not ((PAnsiChar(InBuf)[Cnt - 2] = #$81) and
                                  (PAnsiChar(InBuf)[Cnt - 1] = #$45))) then
                            Result := ICS_ERR_EILSEQ - InCnt
                        else
                            Result := InCnt;
                    end
                    else
                        Result := InCnt;
                    Exit;
                end
                else if (InCnt = InSize) then // shift sequence at the end of buffer
                begin
                    Result := InCnt;
                    Exit;
                end;
             end;
        {S_FALSE E_FAIL}
        else
            Result := ICS_ERR_EINVAL;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MLangMbToWc(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    InCnt, OutCnt: Integer;
begin
    if OutSize <= 0 then
    begin // Count only
        OutCnt  := 0;
        OutBuf  := nil;
    end
    else
        OutCnt := OutSize div SizeOf(WideChar);
    if InSize < 0 then
        InSize := StrLen(PAnsiChar(InBuf)) + 1;

    InCnt := InSize;
    case ConvertINetMultibyteToUnicode(Csc.FToWcShiftState, Csc.CodePage,
                                       InBuf, InCnt,
                                       OutBuf, OutCnt) of
        S_OK    :
            begin
                Result := OutCnt * SizeOf(WideChar);
                if (Result > 0) and (InSize <> InCnt) then
                begin
                    if Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS then
                    begin
                        { Actually it's impossible to detect invalid sequences }
                        { since default unicode chars are inserted.            }
                        Result := 0;
                        SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                    end
                    else
                        SetLastError(0);
                end
                else
                    SetLastError(0);
            end;
        {S_FALSE E_FAIL}
        else
            Result := 0;
            SetLastError(ERROR_INVALID_PARAMETER);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MLangWcToMb(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    InCnt, OutCnt: Integer;
begin
    { The shift state doesn't seem to be used from wc to mb }
    if OutSize <= 0 then
    begin // Count only
        OutSize := 0;
        OutBuf := nil;
    end;
    if InSize < 0 then
        InSize := StrByteCountUtf16(PWord(InBuf)); { Counts the NULL-terminator }
    InSize := InSize div SizeOf(WideChar);
    InCnt := InSize;
    OutCnt:= OutSize;
    case ConvertINetUnicodeToMultiByte(Csc.FFromWcShiftState, Csc.CodePage,
                                       InBuf, InCnt,
                                       OutBuf, OutCnt) of
        S_OK  :
            begin
                Result := OutCnt;
                if (Result > 0) and (InSize <> InCnt) then
                begin
                    if Flags and WC_ERR_INVALID_CHARS = WC_ERR_INVALID_CHARS then
                    begin
                        Result := 0;
                        SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                    end
                    else
                        SetLastError(0);
                end
                else
                    SetLastError(0);
            end;
        else
            Result := 0;
            SetLastError(ERROR_INVALID_PARAMETER);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinMbToWc(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
begin
    Result := MultiByteToWideChar(Csc.CodePage, Flags, InBuf, InSize,
                       OutBuf, OutSize div SizeOf(WideChar)) * SizeOf(WideChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinWcToMb(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
begin
    Result := WideCharToMultiByte(Csc.CodePage, Flags,
                                  InBuf, InSize div SizeOf(WideChar),
                                  OutBuf, OutSize, nil, nil);
end;
{$ENDIF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_ICONV}
function IconvMbToWc(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    CntOnly : Boolean;
    SBuf: array [0..255] of Byte;
    DstBytesLeft, SrcBytesLeft, CntSize: Integer;
    SrcPtr, DstPtr : Pointer;    
begin
    SrcPtr := InBuf;
    SrcBytesLeft := InSize;
    if SrcBytesLeft < 0 then
        SrcBytesLeft := StrLen(PAnsiChar(SrcPtr)) + 1;
    CntSize := 0;
    if OutSize <= 0 then
    begin
        CntOnly := TRUE;
        DstBytesLeft := SizeOf(SBuf);
        DstPtr := @SBuf;
    end
    else begin
        CntOnly := FALSE;
        DstBytesLeft := OutSize;
        DstPtr := OutBuf;
    end;
    SetLastError(0);
    while True do
    begin
        Result := iconv(Csc.FToWcIconvCtx, @SrcPtr, @SrcBytesLeft, @DstPtr, @DstBytesLeft);
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
                        // Return a length of 0
                        SetLastError(ERROR_INSUFFICIENT_BUFFER);
                        DstBytesLeft := OutSize;
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
                        if icfBreakOnInvalidSequence in Csc.FIconvCompatFlags then
                            Break;
                        if icfUseDefaultUnicodeChar in Csc.FIconvCompatFlags then
                        begin
                            PWideChar(DstPtr)^ := Csc.DefaultUnicodeChar;
                            Inc(INT_PTR(DstPtr), SizeOf(WideChar));
                            Dec(DstBytesLeft, SizeOf(WideChar));
                        end;
                        Result := Csc.GetNextCodePointSize(SrcPtr, SrcBytesLeft);
                        if Result < 1 then
                            Result := 1;
                        Inc(INT_PTR(SrcPtr), Result);
                        Dec(SrcBytesLeft, Result);
                    end;
                EINVAL :  { An incomplete multibyte sequence has been encountered in the input }
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
        Result := CntSize + SizeOf(SBuf) - DstBytesLeft
    else
        Result := OutSize - DstBytesLeft;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IconvWcToMb(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    CntOnly : Boolean;
    SBuf: array [0..255] of Byte;
    DstBytesLeft, SrcBytesLeft, CntSize : Integer;
    SrcPtr, DstPtr : Pointer;
begin
    SrcPtr := InBuf;
    SrcBytesLeft := InSize;
    if SrcBytesLeft < 0 then
        SrcBytesLeft := StrByteCountUtf16(PWord(SrcPtr));
    CntSize := 0;
    if OutSize <= 0 then
    begin
        CntOnly := TRUE;
        DstBytesLeft := SizeOf(SBuf);
        DstPtr := @SBuf;
    end
    else begin
        CntOnly := FALSE;
        DstBytesLeft := OutSize;
        DstPtr  := OutBuf;
    end;
    SetLastError(0);
    while True do
    begin
        Result := iconv(Csc.FFromWcIconvCtx, @SrcPtr, @SrcBytesLeft, @DstPtr, @DstBytesLeft);
        if Result <> -1 then
        begin
            { Write the shift sequence and reset shift state }
            if (SrcPtr <> nil) and ((Csc.CharSetType = ictMbcs) or (Csc.CodePage = CP_UTF7)) then
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
                        DstBytesLeft := OutSize;
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
                            PAnsiChar(DstPtr)^ := Csc.DefaultAnsiChar;
                        Inc(INT_PTR(DstPtr), 1);
                        Dec(DstBytesLeft, 1);
                    end;
                EINVAL : { An incomplete sequence has been encountered in the input }
                    begin
                        { Write the shift sequence and reset shift state }
                        if (SrcPtr <> nil) and
                           ((Csc.CharSetType = ictMbcs) or (Csc.CodePage = CP_UTF7)) then
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
                    end
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
        Result := OutSize - DstBytesLeft;
end;
 {$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnicodeToUnicode(Csc: TIcsCsc;
  var InBuf  : Pointer; var InBytesLeft: Integer;
  var OutBuf : Pointer; var OutBytesCnt: Integer): Integer;
var
    Ch1, Ch2 : Word;
    I : Integer;
    CountOnly : Boolean;
    LoopCount : Integer;
    LOutCount : Integer;
    LInCount  : Integer;
begin
    if InBytesLeft < 0 then
        InBytesLeft := StrByteCountUtf16(InBuf); {Counts the NULL-terminator}
    if InBytesLeft = 0 then
    begin
        OutBytesCnt := 0;
        Result := 0;
        Exit;
    end;
    Result      := ICS_ERR_EINVAL;
    CountOnly   := (OutBytesCnt <= 0) or (OutBuf = nil);
    LOutCount   := 0;
    LInCount    := 0;
    LoopCount   := InBytesLeft div SizeOf(WideChar);
    I := 1;
    while I <= LoopCount do
    begin
        if (not CountOnly) and (LOutCount + 2 > OutBytesCnt) then
        begin
            Result := ICS_ERR_E2BIG;
            Break;
        end;

        if Csc.CodePage = CP_UTF16Be then
            Ch1 := IcsSwap16(PWord(InBuf)^)
        else
            Ch1 := PWord(InBuf)^;

        if (Ch1 >= $D800) and (Ch1 <= $DFFF) then
        begin
            if (Ch1 >= $DC00) then
            begin
                Result := ICS_ERR_EILSEQ - 2;
                Break;
            end
            else if (Ch1 <= $DBFF) then
            begin
                Inc(I);
                if I > LoopCount then
                    Break;
                if (not CountOnly) and (LOutCount + 4 > OutBytesCnt) then
                begin
                    Result := ICS_ERR_E2BIG;
                    Break;
                end;
                Inc(PWord(InBuf));
                if Csc.CodePage = CP_UTF16Be then
                    Ch2 := IcsSwap16(PWord(InBuf)^)
                else
                    Ch2 := PWord(InBuf)^;
                if not ((Ch2 >= $DC00) and (Ch2 <= $DFFF)) then
                begin
                    Result := ICS_ERR_EILSEQ - 4;
                    Dec(PWord(InBuf));
                    Break;
                end;
                if not CountOnly then
                begin
                    PWord(OutBuf)^ := Ch1;
                    Inc(PWord(OutBuf));
                    PWord(OutBuf)^ := Ch2;
                    Inc(PWord(OutBuf));
                end;
                Inc(LOutCount, 4);
                Inc(LInCount,  4);
            end;
        end
        else begin
            if not CountOnly then
            begin
                PWord(OutBuf)^ := Ch1;
                Inc(PWord(OutBuf));
            end;
            Inc(LOutCount, 2);
            Inc(LInCount,  2);
        end;
        Inc(I);
        Inc(PWord(InBuf));
    end;
    OutBytesCnt := LOutCount;
    if LInCount = InBytesLeft then
        Result := 0;
    InBytesLeft := InBytesLeft - LInCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WcToWc(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    Res, LInBytesLeft, LOutCnt: Integer;
begin
    if InSize < 0 then
        InSize := StrByteCountUtf16(PWORD(InBuf)); { Counts the NULL-terminator }
    LInBytesLeft  := InSize;
    LOutCnt := OutSize;
    Result  := 0;
    while TRUE do
    begin
        Res := UnicodeToUnicode(Csc, InBuf, LInBytesLeft, OutBuf, LOutCnt);
        if Res <= ICS_ERR_EILSEQ then
        begin
            if (Flags and WC_ERR_INVALID_CHARS) = WC_ERR_INVALID_CHARS then
            begin
                Result := 0;
                SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                Exit;
            end;
            Inc(Result, LOutCnt + 2);
            if OutSize > 0 then
            begin
                if Csc.CodePage = CP_UTF16Be then
                    PWord(OutBuf)^ := IcsSwap16(Word(Csc.DefaultUnicodeChar))
                else
                    PWord(OutBuf)^ := Word(Csc.DefaultUnicodeChar);
                Inc(PWord(OutBuf));
                LOutCnt := OutSize - Result;
            end
            else
                LOutCnt := 0;
            Inc(PWord(InBuf));
            Dec(LInBytesLeft, 2);
        end
        else begin
            if Res < 0 then
            begin
                Result := 0;
                if Res = ICS_ERR_EINVAL then
                    SetLastError(ERROR_NO_UNICODE_TRANSLATION)
                else
                    SetLastError(ERROR_INSUFFICIENT_BUFFER);
            end
            else begin
                Inc(Result, LOutCnt);
                SetLastError(0);
            end;
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ucs4ToUnicode(Swapped: Boolean;
  var InBuf: Pointer; var InBytesLeft: Integer;
  var OutBuf: Pointer; var OutByteCount: Integer): Integer;
var
    Ch : LongWord;
    I : Integer;
    CountOnly  : Boolean;
    LOutCnt : Integer;
begin
    if InBytesLeft < 0 then
        InBytesLeft := StrByteCountUcs4(InBuf); {includes the NULL-terminator}
    if InBytesLeft = 0 then
    begin
        OutByteCount := 0;
        Result := 0;
        Exit;
    end;
    Result := ICS_ERR_EINVAL;
    CountOnly := (OutByteCount <= 0) or (OutBuf = nil);
    LOutCnt := 0;
    for I := 1 to (InBytesLeft div 4) do
    begin
        if Swapped then
            Ch := IcsSwap32(PLongWord(InBuf)^)
        else
            Ch := PLongWord(InBuf)^;
        if ((Ch >= $D800) and (Ch <= $DFFF)) or (Ch > $10FFFF) then
        begin
            Result := ICS_ERR_EILSEQ - 4;
            Break;
        end
        else begin

            if Ch > $10000 then
            begin
                { Encode surrogate pair }
                if not CountOnly then
                begin
                    PWord(OutBuf)^ := Word((((Ch - $00010000) shr 10) and
                                              $000003FF) or $D800);
                    Inc(PWord(OutBuf));
                    PWord(OutBuf)^ := Word(((Ch - $00010000) and $000003FF) or
                                               $DC00);
                    Inc(PWord(OutBuf));
                end;
                Inc(LOutCnt, 4);
            end
            else begin // BMP
                if not CountOnly then
                begin
                    PWord(OutBuf)^ := Word(Ch);
                    Inc(PWord(OutBuf));
                end;
                Inc(LOutCnt, 2);
            end;

            if (not CountOnly) and (LOutCnt > OutByteCount) then
            begin
                OutByteCount := LOutCnt;
                Result := ICS_ERR_E2BIG;
                Exit;
            end
            else
                Dec(InBytesLeft, 4);
        end;
        Inc(PLongWord(InBuf));
    end;
    OutByteCount := LOutCnt;
    if InBytesLeft = 0 then
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ucs4ToWc(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    Res, LInBytesLeft, LOutCnt: Integer;
begin
    if InSize < 0 then
        InSize := StrByteCountUcs4(PLongWord(InBuf)); { Counts the NULL-terminator }
    LInBytesLeft  := InSize;
    LOutCnt       := OutSize;
    Result        := 0;
    while TRUE do
    begin
        Res := Ucs4ToUnicode(Csc.CodePage = CP_UTF32BE, InBuf, LInBytesLeft,
                             OutBuf, LOutCnt);
        if Res <= ICS_ERR_EILSEQ then
        begin
            if (Flags and MB_ERR_INVALID_CHARS) = MB_ERR_INVALID_CHARS then
            begin
                Result := 0;
                SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                Exit;
            end;
            Inc(Result, LOutCnt + 2);
            if OutSize > 0 then
            begin
                PWord(OutBuf)^ := Word(Csc.DefaultUnicodeChar);
                Inc(PWord(OutBuf));
                LOutCnt := OutSize - (Result * SizeOf(WideChar));
            end
            else
                LOutCnt := 0;
            Inc(PLongWord(InBuf));
            Dec(LInBytesLeft, 4);
        end
        else begin
            if Res < 0 then
            begin
                Result := 0;
                if Res = ICS_ERR_EINVAL then
                    SetLastError(ERROR_NO_UNICODE_TRANSLATION)
                else
                    SetLastError(ERROR_INSUFFICIENT_BUFFER);
            end
            else begin
                Inc(Result, LOutCnt);
                SetLastError(0);
            end;
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnicodeToUcs4(Swapped: Boolean;
  var InBuf  : Pointer; var InBytesLeft: Integer;
  var OutBuf : Pointer; var OutBytesCnt: Integer): Integer;
var
    Ch1, Ch2 : Word;
    I : Integer;
    CountOnly : Boolean;
    LoopCount : Integer;
    LOutCount : Integer;
    LInCount  : Integer;
begin
    if InBytesLeft < 0 then
        InBytesLeft := StrByteCountUtf16(InBuf); { Counts the NULL-terminator }
    if InBytesLeft = 0 then
    begin
        OutBytesCnt := 0;
        Result := 0;
        Exit;
    end;
    Result      := ICS_ERR_EINVAL;
    CountOnly   := (OutBytesCnt <= 0) or (OutBuf = nil);
    LOutCount   := 0;
    LInCount    := 0;
    LoopCount   := InBytesLeft div SizeOf(WideChar);
    I := 1;
    while I <= LoopCount do
    begin
        if (not CountOnly) and (LOutCount + 4 > OutBytesCnt) then
        begin
            Result := ICS_ERR_E2BIG;
            Break;
        end;

        Ch1 := PWord(InBuf)^;

        if (Ch1 >= $D800) and (Ch1 <= $DFFF) then
        begin
            if (Ch1 >= $DC00) then
            begin
                Result := ICS_ERR_EILSEQ -2;
                Break;
            end
            else if (Ch1 <= $DBFF) then
            begin
                Inc(I);
                if I > LoopCount then
                    Break;
                if (not CountOnly) and (LOutCount + 4 > OutBytesCnt) then
                begin
                    Result := ICS_ERR_E2BIG;
                    Break;
                end;
                Inc(PWord(InBuf));

                Ch2 := PWord(InBuf)^;

                if not ((Ch2 >= $DC00) and (Ch2 <= $DFFF)) then
                begin
                    Result := ICS_ERR_EILSEQ - 4;
                    Dec(PWord(InBuf));
                    Break;
                end;
                if not CountOnly then
                begin
                    PLongWord(OutBuf)^ := ((Ch1 and $3FF) shl 10) +
                                          (Ch2 and $3FF) + $10000;
                    if Swapped then
                       PLongWord(OutBuf)^ := IcsSwap32(PLongWord(OutBuf)^);
                    Inc(PLongWord(OutBuf));
                end;
                Inc(LInCount, 4);
            end;
        end
        else begin
            if not CountOnly then
            begin
                PLongWord(OutBuf)^ := Ch1;
                if Swapped then
                    PLongWord(OutBuf)^ := IcsSwap32(PLongWord(OutBuf)^);
                Inc(PLongWord(OutBuf));
            end;
            Inc(LInCount, 2);
        end;
        Inc(I);
        Inc(PWord(InBuf));
        Inc(LOutCount, 4);
    end;
    OutBytesCnt := LOutCount;
    if LInCount = InBytesLeft then
        Result := 0;
    InBytesLeft := InBytesLeft - LInCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WcToUcs4(Csc: TIcsCsc; Flags: LongWord; InBuf: Pointer;
  InSize: Integer; OutBuf: Pointer; OutSize: Integer): Integer;
var
    Res, LInBytesLeft, LOutCnt: Integer;
begin
    if InSize < 0 then
        InSize := StrByteCountUcs4(PLongWord(InBuf)); { Counts the NULL-terminator }
    LInBytesLeft  := InSize;
    LOutCnt := OutSize;
    Result  := 0;
    while TRUE do
    begin
        Res := UnicodeToUcs4(Csc.CodePage = CP_UTF32Be, InBuf, LInBytesLeft,
                             OutBuf, LOutCnt);
        if Res <= ICS_ERR_EILSEQ then
        begin
            if (Flags and WC_ERR_INVALID_CHARS) = WC_ERR_INVALID_CHARS then
            begin
                Result := 0;
                SetLastError(ERROR_NO_UNICODE_TRANSLATION);
                Exit;
            end;
            Inc(Result, LOutCnt + 4);
            if OutSize > 0 then
            begin
                PLongWord(OutBuf)^ := Word(Csc.DefaultUnicodeChar);
                if Csc.CodePage = CP_UTF32Be then
                    PLongWord(OutBuf)^ := IcsSwap32(PLongWord(OutBuf)^);
                Inc(PLongWord(OutBuf));
                LOutCnt := OutSize - (Result * SizeOf(LongWord));
            end
            else
                LOutCnt := 0;
            Inc(PWord(InBuf));
            Dec(LInBytesLeft, 2);
        end
        else begin
            if Res < 0 then
            begin
                Result := 0;
                SetLastError(ERROR_INSUFFICIENT_BUFFER);
            end
            else begin
                Inc(Result, LOutCnt);
                SetLastError(0);
            end;
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.GetBufferEncoding(const Buf: Pointer; BufSize: Integer;
  Detect: Boolean): Integer;

    function HasBOM(BOM: TBytes): Boolean;
    var
        I : Integer;
    begin
        Result := True;
        if BufSize >= Length(BOM) then
        begin
            for I := 0 to Length(BOM) -1 do
                if PByte(PAnsiChar(Buf) + I)^ <> BOM[I] then
            begin
                Result := False;
                Break;
            end;
        end
        else
            Result := False;
    end;

var
    BOM: TBytes;
begin
    Result := 0;
    if Detect then begin
        // Detect and set code page
        if HasBOM(IcsGetBomBytes(CP_UTF32)) then
              SetCodePage(CP_UTF32)
        else if HasBOM(IcsGetBomBytes(CP_UTF32Be)) then
              SetCodePage(CP_UTF32Be)
        else if HasBOM(IcsGetBomBytes(CP_UTF16)) then
              SetCodePage(CP_UTF16)
        else if HasBOM(IcsGetBomBytes(CP_UTF16Be)) then
              SetCodePage(CP_UTF16Be)
        else if HasBOM(IcsGetBomBytes(CP_UTF8)) then
              SetCodePage(CP_UTF8)
        else
            SetCodePage(CP_ACP);

        Result := Length(GetBomBytes);
    end
    else begin
        BOM := GetBomBytes;
        if HasBOM(BOM) then
            Result := Length(BOM);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCscGetWideCharCount(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer;
  out BytesLeft: Integer): Integer;
var
    TranslatableBytes : Integer;
    CP : PByte;
    LSize : Integer;
begin
    CP := Buf;
    TranslatableBytes := 0;
    LSize := BufSize;
    BytesLeft := 0;
    while LSize > 0 do
    begin
        Dec(LSize, BytesLeft);
        BytesLeft := Csc.GetNextCodePointSize(CP, LSize);        
        if BytesLeft > 0 then
            Inc(TranslatableBytes, BytesLeft)
        else
            Break;
        Inc(CP, BytesLeft);
    end;
    BytesLeft := BufSize - TranslatableBytes;
    Result := Csc.ToWc(0, @Buf, TranslatableBytes,
                       nil, 0) div SizeOf(WideChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCscGetWideChars(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer; Chars: PWideChar;
  WCharCount: Integer): Integer;
begin
    Result := Csc.ToWc(0, Buf, BufSize, Chars,
                       WCharCount * SizeOf(WideChar)) div SizeOf(WideChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCscBufferToUnicodeString(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer;
  out BytesLeft: Integer): UnicodeString;
var
    WCharCnt : Integer;
begin
    BytesLeft := 0;
    if (Buf = nil) or (BufSize <= 0) then
        Result := ''
    else begin
        WCharCnt := IcsCscGetWideCharCount(Csc, Buf, BufSize, BytesLeft);
        SetLength(Result, WCharCnt);
        if WCharCnt > 0 then
        begin
            WCharCnt := IcsCscGetWideChars(Csc, Buf, BufSize - BytesLeft,
                                        Pointer(Result), WCharCnt);
            SetLength(Result, WCharCnt);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsCscToWcString(Csc: TIcsCsc; const Buf: Pointer; BufSize: Integer): UnicodeString;
var
    Len: Integer;
begin
    Len := Csc.ToWc(0, Buf, BufSize, nil, 0);
    SetLength(Result, Len div 2);
    Len := Csc.ToWc(0, Buf, BufSize, Pointer(Result), Len);
    SetLength(Result, Len div 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsCsc }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsCsc.Create(CodePage: LongWord);
begin
    inherited Create;
    SetCodePage(CodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsCsc.Destroy;
begin
{$IFDEF USE_ICONV}
    if FFromWcIconvCtx <> nil then
        iconv_close(FFromWcIconvCtx);
    if FToWcIconvCtx <> nil then
        iconv_close(FToWcIconvCtx);
 {$ENDIF}
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCsc.SetCodePage(const Value: LongWord);
begin
    if (Value = CP_ACP) then
        IcsGetAcp(FCodePage)
    else
        FCodePage := Value;
    Init;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCsc.Init;
{$IFDEF MSWINDOWS}
var
    Info: TCpInfo;
{$ENDIF}

{$IFDEF USE_ICONV}
    function LInitIconv: Boolean;
    var
        CpName : AnsiString;
    begin
        Result := Assigned(FToWcIconvCtx) and Assigned(FFromWcIconvCtx);
        if Result then
            Exit;
        if Load_Iconv then
        begin
            CpName := IcsIconvNameFromCodePage(FCodePage);
            FToWcIconvCtx := iconv_open(ICONV_UNICODE, PAnsiChar(CpName));
            if FToWcIconvCtx = iconv_t(-1) then
            begin
                FToWcIconvCtx := nil;
                Result := FALSE;
                Exit;
            end;
            FFromWcIconvCtx := iconv_open(PAnsiChar(CpName), ICONV_UNICODE);
            if FFromWcIconvCtx = iconv_t(-1) then
            begin
                iconv_close(FToWcIconvCtx);
                FToWcIconvCtx := nil;
                FFromWcIconvCtx := nil;
                Result := FALSE;
                Exit;
            end;
            Result := TRUE;
        end;
    end;
{$ENDIF}

begin

{$IFDEF USE_ICONV}
    if FFromWcIconvCtx <> nil then
    begin
        iconv_close(FFromWcIconvCtx);
        FFromWcIconvCtx := nil;
    end;
    if FToWcIconvCtx <> nil then
    begin
        iconv_close(FToWcIconvCtx);
        FToWcIconvCtx := nil;
    end;
{$ENDIF}

    FToWcShiftState     := 0;
    FFromWcShiftState   := 0;
    FToWcFunc           := nil;
    FFromWcFunc         := nil;
    FCpSizeFunc         := nil;

    FLeadBytes          := GetDbcsLeadBytes(FCodePage);
    FDefaultUnicodeChar := IcsGetDefaultWindowsUnicodeChar(FCodePage);
    FDefaultAnsiChar    := IcsGetDefaultWindowsAnsiChar(FCodePage);

    if (FCodePage = CP_UTF16) or (FCodePage = CP_UTF16Be) then
    begin
        FCpSizeFunc := CheckUtf16CpSize;
        FToWcFunc   := WcToWc;
        FFromWcFunc := WcToWc;
        FMinCpSize  := 2;
        FCharsetType := ictUnicode;
    end
    else

    if (FCodePage = CP_UTF32) or (FCodePage = CP_UTF32Be) then
    begin
        FCpSizeFunc := CheckUtf32CpSize;
        FToWcFunc   := Ucs4ToWC;
        FFromWcFunc := WcToUcs4;
        FMinCpSize  := 4;
        FCharsetType := ictUnicode;
    end
    else

    if IcsIsSBCSCodePage(FCodepage) then
    begin
    {$IFDEF USE_ICONV}
        if LInitIconv then
        begin
            FToWcFunc   := IconvMbToWc;
            FFromWcFunc := IconvWcToMb;
        end
        else
    {$ENDIF}
    {$IFDEF MSWINDOWS}
        if IsValidCodePage(FCodePage) and
            GetCpInfo(FCodePage, Info) then
        begin
            FToWcFunc   := WinMbToWc;
            FFromWcFunc := WinWcToMb;
        end
        else
    {$ENDIF}
            raise EIcsStringConvertError.CreateFmt(sInvalidEncoding, [FCodePage]);

        FCpSizeFunc := CheckSbcCpSize;
        FCharsetType := ictSBCS;
        FMinCpSize  := 1;
    end
    else

    if FLeadBytes <> [] then
    begin
    {$IFDEF USE_ICONV}
        if LInitIconv then
        begin
            FToWcFunc   := IconvMbToWc;
            FFromWcFunc := IconvWcToMb;
        end
        else
    {$ENDIF}
    {$IFDEF MSWINDOWS}
        if IsValidCodePage(FCodePage) and
            GetCpInfo(FCodePage, Info) then
        begin
            FToWcFunc   := WinMbToWc;
            FFromWcFunc := WinWcToMb;
        end
        else
    {$ENDIF}
            raise EIcsStringConvertError.CreateFmt(sInvalidEncoding, [FCodePage]);

        FCharsetType := ictDBCS;
        FCpSizeFunc  := CheckDbcCpSize;
        FMinCpSize   := 1;
    end
    else

    if FCodePage = CP_UTF8 then
    begin
    {$IFDEF USE_ICONV}
        if LInitIconv then
        begin
            FToWcFunc   := IconvMbToWc;
            FFromWcFunc := IconvWcToMb;
        end
        else
    {$ENDIF}
    {$IFDEF MSWINDOWS}
        if IsValidCodePage(FCodePage) and
            GetCpInfo(FCodePage, Info) then
        begin
            FToWcFunc   := WinMbToWc;
            FFromWcFunc := WinWcToMb;
        end
        else
    {$ENDIF}
            raise EIcsStringConvertError.CreateFmt(sInvalidEncoding, [FCodePage]);

        FCpSizeFunc := CheckUtf8CpSize;
        FCharsetType := ictMbcsUnicode;
        FMinCpSize  := 1;
    end
    else

    if CodePage = CP_UTF7 then
    begin
    {$IFDEF USE_ICONV}
        if LInitIconv then
        begin
            FToWcFunc   := IconvMbToWc;
            FFromWcFunc := IconvWcToMb;
        end
        else
    {$ENDIF}
    {$IFDEF MSWINDOWS}
        if IsValidCodePage(FCodePage) and
            GetCpInfo(FCodePage, Info) then
        begin
            FToWcFunc   := WinMbToWc;
            FFromWcFunc := WinWcToMb;
        end
        else
    {$ENDIF}
            raise EIcsStringConvertError.CreateFmt(sInvalidEncoding, [FCodePage]);

        FCpSizeFunc := CheckUtf7CpSize;
        FCharsetType := ictMbcsUnicode;
        FMinCpSize  := 1;
    end
    else begin

        FCharsetType := ictMBCS;
        FMinCpSize  := 1;

    {$IFDEF USE_ICONV}
        if LInitIconv then
        begin
            FToWcFunc   := IconvMbToWc;
            FFromWcFunc := IconvWcToMb;
            FCpSizeFunc := CheckMbcCpSizeIconv;
        end
        else
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      {$IFDEF USE_MLANG}
        if Load_MLang and
          (IsConvertINetStringAvailable(FCodePage, CP_UTF16) = S_OK) and
          (IsConvertINetStringAvailable(CP_UTF16, FCodePage) = S_OK) then
        begin
            FToWcFunc   := MLangMbToWc;
            FFromWcFunc := MLangWcToMb;
            FCpSizeFunc := CheckMbcCpSizeMLang;
        end
        else
      {$ENDIF}
        if IsValidCodePage(FCodePage) and
            GetCpInfo(FCodePage, Info) then
        begin
            FToWcFunc   := WinMbToWc;
            FFromWcFunc := WinWcToMb;
            FCpSizeFunc := CheckMbcCpSize;
        end
        else
    {$ENDIF}
            raise EIcsStringConvertError.CreateFmt(sInvalidEncoding, [FCodePage]);

        { GB18030 }
        if FCodePage = 54936 then
            FCpSizeFunc := CheckGB18030CpSize
        { euc-jp  EUC Japanese MBCS Max Size: 3 | MLang.Dll and iconv }
        else if FCodePage = 51932 then
            FCpSizeFunc := CheckEucJpCpSize;
    end;

{$IFDEF USE_ICONV}
    // An attempt to mimic changes Win-XP => Vista/Win7 in handling invalid source bytes
    FIconvCompatFlags := [];
  {$IFDEF MSWINDOWS}
    if Win32MajorVersion >= 6 then
    begin
  {$ENDIF}
        if FCharsetType = ictMBCS then
            Include(FIconvCompatFlags, icfBreakOnInvalidSequence)
        else
        case FCodePage of
            CP_UTF7 : {No icfUseDefaultUnicodeChar};
        else
            Include(FIconvCompatFlags, icfUseDefaultUnicodeChar);
        end;
  {$IFDEF MSWINDOWS}
    end
    else begin
        case FCodePage of
            CP_UTF7..CP_UTF8 : {No icfUseDefaultUnicodeChar};
        else
            Include(FIconvCompatFlags, icfUseDefaultUnicodeChar);
        end;
    end;
  {$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCsc.ClearToWcShiftState;
{$IFDEF USE_ICONV}
var
    LZero: size_t;
    LNil : Pointer;
{$ENDIF}
begin
    FToWcShiftState := 0;
{$IFDEF USE_ICONV}
    LNil := nil;
    if FToWcIconvCtx <> nil then
        iconv(FToWcIconvCtx, @LNil, @LZero, @LNil, @LZero);
 {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCsc.ClearFromWcShiftState;
{$IFDEF USE_ICONV}
var
    LZero: size_t;
    LNil : Pointer;
 {$ENDIF}
begin
    FFromWcShiftState := 0;
{$IFDEF USE_ICONV}
    LNil := nil;
    if FFromWcIconvCtx <> nil then
        iconv(FFromWcIconvCtx, @LNil, @LZero, @LNil, @LZero);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCsc.ClearAllShiftStates;
begin
    ClearToWcShiftState;
    ClearFromWcShiftState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.FromWc(Flags: LongWord; InBuf: Pointer; InSize: Integer;
  OutBuf: Pointer; OutSize: Integer): Integer;
begin
    Result := FFromWcFunc(Self, Flags, InBuf, InSize, OutBuf, OutSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.ToWc(Flags: LongWord; InBuf: Pointer; InSize: Integer;
  OutBuf: Pointer; OutSize: Integer): Integer;
begin
    Result := FToWcFunc(Self, Flags, InBuf, InSize, OutBuf, OutSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.GetBomBytes: TBytes;
begin
    Result := IcsGetBomBytes(FCodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.GetNextCodePoint(Buf: Pointer; BufSize: Integer;
  Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Pointer;
var
    SeqSize : Integer;
begin
    Result := Buf;
    SeqSize := GetNextCodePointSize(Buf, BufSize, Flags);
    if SeqSize > 0 then
        Inc(PByte(Result), SeqSize)    
    else if (SeqSize = ICS_ERR_EINVAL) and (ncfSkipEINVAL in Flags) then
        Inc(PByte(Result), BufSize);
    {else
        ICS_ERR_EINVAL }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCsc.GetNextCodePointSize(Buf: Pointer;
  BufSize: Integer; Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer;
begin
    if (Buf = nil) or (BufSize < 1) then
        Result := 0
    else begin
        Result := FCpSizeFunc(Self, Buf, BufSize);
        if (Result <= ICS_ERR_EILSEQ) and (ncfSkipEILSEQ in Flags) then
        begin
            if Result < ICS_ERR_EILSEQ then
                Result := -(Result - ICS_ERR_EILSEQ)
            else
                Result := MinCpSize;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsCscStr }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCscStr.GetNextCodePointIndex(const S: RawByteString; Index: Integer;
  Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer;
var
    Len : Integer;
    SeqSize : Integer;
begin
    Len := Length(S);
    Assert((Index > 0) and (Index <= Len));
    Result := Index;
    SeqSize := GetNextCodePointSize(PAnsiChar(S) + Index - 1, Len - Index + 1,
                                    Flags);
    if SeqSize > 0 then
        Inc(Result, SeqSize)
    else if (SeqSize = ICS_ERR_EINVAL) and (ncfSkipEINVAL in Flags) then
        Inc(Result, Len - Index + 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCscStr.GetNextCodePointIndex(const S: UnicodeString; Index: Integer;
  Flags: TIcsNextCodePointFlags = [ncfSkipEILSEQ]): Integer;
var
    Len : Integer;
    SeqSize : Integer;
begin
    Len := Length(S);
    Assert((Index > 0) and (Index <= Len));
    Result := Index;
    SeqSize := GetNextCodePointSize(PWideChar(S) + Index - 1,
                                    (Len - Index + 1) * SizeOf(WideChar),
                                    Flags);
    if SeqSize > 0 then
        Inc(Result, SeqSize div SizeOf(WideChar))    
    else if (SeqSize = ICS_ERR_EINVAL) and (ncfSkipEINVAL in Flags) then
        Inc(Result, (Len - Index + 1));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
