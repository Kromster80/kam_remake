{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       Mime support routines (RFC2045).
Creation:     May 03, 2003  (Extracted from SmtpProt unit)
Version:      1.06
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by François PIETTE
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
Dec 14, 2006  V1.06 Updated Base64Decode to ignore CR and LF


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I ICSDEFS.INC}
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

unit MimeUtil;

interface

{$R-}

uses
    SysUtils, Classes;

{$IFNDEF DELPHI4_UP}
type
    TSysCharSet = set of Char;
{$ENDIF}

const
    TMimeUtilsVersion = 106;
    CopyRight : String = ' MimeUtils (c) 1997-2007 F. Piette V1.06 ';

    SpecialsRFC822 : TSysCharSet = ['(', ')', '<', '>', '@', ',', ';', ':',
                                    '\', '"', '[', ']', '.'];

    HexTable : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9',
                                       'A','B','C','D','E','F');      {HLX}


{ Functions to encode/decode string as a "quoted-printable" string RFC2045}
function  EncodeQuotedPrintable(const S: String) : String;
function  DecodeQuotedPrintable(const S: String) : String;
function  SplitQuotedPrintableString(const S : String) : String;
{ Find a Content-Type from a file name                                   }
function  FilenameToContentType(FileName : String) : String;
{ Base 64 encoding }
function  Base64Encode(Input : String) : String;
{ Similar to Base64Encode, returns just a coded line                      }
function Base64EncodeEx(Input    : String;
                        MaxCol   : Integer;
                        var cPos : Integer) : String;
function  Base64Decode(Input : String) : String;
function  InitFileEncBase64(const FileName : String;
                            ShareMode      : Word) : TStream;
function  DoFileEncBase64(var Stream     : TStream;
                          var More       : Boolean) : String;
procedure EndFileEncBase64(var Stream : TStream);
{ Dot at start of line escaping for SMTP and NNTP (double the dot)        }
procedure DotEscape(var S : String);
{ Text wrap and folding                                                   } {AG}
{function  IcsWrapText(const Line,
                      BreakStr   : String;
                      BreakChars : TSysCharSet;
                      MaxCol     : Integer;
                      QuoteChars : TSysCharSet): String;}
{ Similar to IcsWrapText, returns just a single line                      } {AG}
function IcsWrapTextEx(const Line, BreakStr : String;
                       BreakChars   : TSysCharSet;
                       MaxCol       : Integer;
                       QuoteChars   : TSysCharSet;
                       var cPos     : Integer): String;
{ Unfolds folded headers                                                  } {AG}
function UnFoldHdrLine(const S : String): String;
{Helper function                                                          }
function NeedsEncoding(const S : String) : Boolean;                         {AG}
function NeedsEncodingPChar(S : PChar) : Boolean;                           {FP}
{ MIME In-Line-Encoding plus Folding, see comments in function source     } {AG}
function HdrEncodeInLine(const Input   : String;
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : Char;        { Either 'Q' or 'B'        }
                         const CharSet : String;      { e.g. 'iso-8859-1'        }
                         MaxCol        : Integer;
                         DoFold         : Boolean): String;
{ Alternate to functions
{ EncodeQuotedPrintable + SplitQuotedPrintableString + DotEscape          }
function StrEncodeQP(const Input : String;                                  {HLX, AG}
                     MaxCol      : Integer; 
                     Specials    : TSysCharSet): String;
{ Similar to StrEncodeQP, returns just a single line                      } {AG}
function StrEncodeQPEx(const Buf   : String;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean; {'_' e.g. for in-line}
                       var cPos    : Integer;
                       DoFold      : Boolean) : String;

procedure FoldHdrLine(HdrLines      : TStrings;                             {AG}
                      const HdrLine : String);

function FoldString(const Input : String;                                {AG}
                    BreakChars  : TSysCharSet;
                    MaxCol      : Integer): String;

implementation

{$IFDEF DELPHI1}
{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
const
  LeadBytes: set of Char = [];
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI1}
{ Delphi 1 miss the SetLength procedure. So we rewrite it. }
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimRight(Str : String) : String;
var
    I : Integer;
begin
    I := Length(Str);
    while (I > 0) and (Str[I] in [' ', #9]) do
        I := I - 1;
    Result := Copy(Str, 1, I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    I : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        I := 1;
        while (I <= Length(Str)) and (Str[I] = ' ') do
            I := I + 1;
        Result := Copy(Str, I, Length(Str) - I + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ See also SplitQuotedPrintableString !                                     }
function EncodeQuotedPrintable(const S: String) : String;
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
                Result := S     { Optimisation }
            else
                Result := Result + Copy(S, J, I - J);
            Exit;
        end;
        Result := Result + Copy(S, J, I - J) + '=' +
                  UpperCase(IntToHex(Ord(S[I]), 2));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A line ending with an equal sign is continued on the next line. This is   }
{ what RFC2045 refers as a "soft line break".                               }
{ This routine doesn't take care of the equal sign at the end of string.    }
{ It is simply ignored. The caller must check that condition and merge      }
{ successives lines. But the routine handle embedded soft line break.       }
function DecodeQuotedPrintable(const S: String) : String;
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
            Result := Result + Char(StrToInt('$' + Copy(S, I + 1, 2)));
        Inc(I, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SplitQuotedPrintableString(const S : String) : String;
var
    I, J : Integer;
begin
    if Length(S) <= 76 then begin
        { No need to split }
        Result := S;
        Exit;
    end;
    Result := '';
    J      := 1;
    I      := 76;
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
procedure DotEscape(var S : String);
var
    I : Integer;
begin
    if S = '' then
        Exit;
    if S[1] = '.' then begin
        Insert('.', S, 1);
        I := 3;
    end
    else
        I := 1;
    while I < (Length(S) - 2) do begin
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
    Ext := LowerCase(ExtractFileExt(FileName));
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
    Result := TFileStream.Create(FileName, fmOpenRead or ShareMode);
    {Result := TBufferedFileStream.Create(FileName, fmOpenRead or ShareMode, 4096);}
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    HLX_MULTIPLIER        = 3;  { for three lines at once }
    MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MAX_LENGTH_MULTIPLIED = (MAX_LENGTH + 2) * HLX_MULTIPLIER;
    MAX_READ              = ((MAX_LENGTH * 3)div 4) * HLX_MULTIPLIER;
    MAX_READ_MOD          = (MAX_LENGTH * 3) div 4;
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
    var More       : Boolean) : String;
const
    MAX_LENGTH = 72;
var
    Count     : Integer;
    DataIn    : array [0..2]  of Byte;
    DataOut   : array [0..MAX_LENGTH + 8] of Byte;
    ByteCount : Integer;
    I         : Integer;
begin
    Count     := 0;
    ByteCount := 0;
    while Count < MAX_LENGTH do begin
        ByteCount          := Stream.Read(DataIn, 3);
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
        if (Count > MAX_LENGTH) or (ByteCount < 3) then
            break;
    end;

    DataOut[Count] := $0;
    Result         := StrPas(PAnsiChar(@DataOut[0]));
    More           := (ByteCount = 3);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EndFileEncBase64(var Stream : TStream);
begin
    if Assigned(Stream) then begin
        Stream.Destroy;
        Stream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Encode(Input : String) : String;
var
    Final : String;
    Count : Integer;
    Len   : Integer;
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                       ((Byte(Input[Count+1]) and $F0) shr 4)];
            if (Count+2) <= Len then begin
                Final := Final + Base64Out[((Byte(Input[Count+1]) and $0F) shl 2) +
                                           ((Byte(Input[Count+2]) and $C0) shr 6)];
                Final := Final + Base64Out[(Byte(Input[Count+2]) and $3F)];
            end
            else begin
                Final := Final + Base64Out[(Byte(Input[Count+1]) and $0F) shl 2];
                Final := Final + '=';
            end
        end
        else begin
            Final := Final + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Final := Final + '==';
        end;
        Count := Count + 3;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to Base64Encode, returns just a coded line                        }
function Base64EncodeEx(Input    : String;
                        MaxCol   : Integer;
                        var cPos : Integer) : String;
var
    Len : Integer;
begin
    Len := Length(Input);
    while cPos <= Len do begin
        if Length(Result) >= MaxCol then
            Exit;
        Result := Result + Base64Out[(Byte(Input[cPos]) and $FC) shr 2];
        if (cPos + 1) <= Len  then begin
            Result := Result + Base64Out[((Byte(Input[cPos]) and $03) shl 4) +
                                   ((Byte(Input[cPos + 1]) and $F0) shr 4)];
            if (cPos + 2) <= Len then begin
                Result := Result + Base64Out[((Byte(Input[cPos + 1]) and $0F) shl 2) +
                                       ((Byte(Input[cPos + 2]) and $C0) shr 6)];
                Result := Result + Base64Out[(Byte(Input[cPos + 2]) and $3F)];
            end
            else begin
                Result := Result + Base64Out[(Byte(Input[cPos + 1]) and $0F) shl 2];
                Result := Result + '=';
            end
        end
        else begin
             Result := Result + Base64Out[(Byte(Input[cPos]) and $03) shl 4];
             Result := Result + '==';
        end;
        Inc(cPos, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(Input : String) : String;
var
    Final   : String;
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        if Input[Count] in [#13, #10] then
            Inc(Count)
        else begin
            DataIn0 := Base64In[Byte(Input[Count])];
            DataIn1 := Base64In[Byte(Input[Count+1])];
            DataIn2 := Base64In[Byte(Input[Count+2])];
            DataIn3 := Base64In[Byte(Input[Count+3])];

            Final := Final + Char(((DataIn0 and $3F) shl 2) +
                                  ((DataIn1 and $30) shr 4));
            if DataIn2 <> $40 then begin
                Final := Final + Char(((DataIn1 and $0F) shl 4) +
                                      ((DataIn2 and $3C) shr 2));
                if DataIn3 <> $40 then
                    Final := Final + Char(((DataIn2 and $03) shl 6) +
                                          (DataIn3 and $3F));
            end;
            Count := Count + 4;
        end;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function takes the QuoteChars as a parameter and it returns just a   }
{ line.                                                                     }
function IcsWrapTextEx(const Line, BreakStr : String;
                       BreakChars   : TSysCharSet;
                       MaxCol       : Integer;
                       QuoteChars   : TSysCharSet;
                       var cPos     : Integer): String;
var
    Col                : Integer;
    LinePos, LineLen   : Integer;
    BreakLen, BreakPos : Integer;
    QuoteChar, CurChar : Char;
    ExistingBreak      : Boolean;
begin
    Col := 1;
    LinePos := cPos;
    BreakPos := 0;
    QuoteChar := ' ';
    ExistingBreak := False;
    LineLen := Length(Line);
    BreakLen := Length(BreakStr);
    Result := '';
    while cPos <= LineLen do begin
        CurChar := Line[cPos];
        if CurChar in LeadBytes then begin
            Inc(cPos);
            Inc(Col);
        end
        else if CurChar = BreakStr[1] then begin
            if QuoteChar = ' ' then begin
                ExistingBreak := CompareText(BreakStr, Copy(Line, cPos, BreakLen)) = 0;
                if ExistingBreak then begin
                    Inc(cPos, BreakLen-1);
                    BreakPos := cPos;
                end;
            end
        end
        else if CurChar in BreakChars then begin
            if QuoteChar = ' ' then
                BreakPos := cPos
        end
        else if CurChar in QuoteChars then
            if CurChar = QuoteChar then
                QuoteChar := ' '
            else if QuoteChar = ' ' then
                QuoteChar := CurChar;
        Inc(cPos);
        Inc(Col);
        if not (QuoteChar in QuoteChars) and
               (ExistingBreak or ((Col > MaxCol) and (BreakPos > LinePos))) then begin
            { Col := cPos - BreakPos; }
            Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
            if not (CurChar in QuoteChars) then
                while (cPos <= LineLen) and (Line[cPos] in BreakChars + [#13, #10]) do
                    Inc(cPos);
                if ExistingBreak then
                    Result := Copy(Result, 1, Length(Result) - BreakLen);
            Inc(BreakPos);
            cPos := BreakPos;
            Exit;
        end;
    end;
    Result := Result + Copy(Line, LinePos, MaxInt);
    cPos   := MaxInt;
end;


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
               (S[I + 2] in [#09, #32]) then begin
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} {AG}
function NeedsEncoding(const S : String) : Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(S) do
        if S[I] in [#0..#8, #11, #12, #14..#31, #127..#255] then begin
            Result := True;
            Exit;
        end;
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NeedsEncodingPChar(S : PChar) : Boolean;
begin
    while S^ <> #0 do begin
        if S^ in [#0..#8, #11, #12, #14..#31, #127..#255] then begin
            Result := True;
            Exit;
        end;
        Inc(S);
    end;
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HdrEncodeInLine(const Input    : String;
                         Specials       : TSysCharSet;
                         EncType        : Char;        { Either 'Q' or 'B' }
                         const CharSet  : String;      { e.g. 'iso-8859-1' }
                         MaxCol         : Integer;
                         DoFold         : Boolean): String;
const
    Suffix = '?=';
var
    Len,
    lPos,
    LenRes      : Integer;
    Prefix,
    Res         : String;
begin
    Result := '';
    if DoFold and (MaxCol < 25) then
        MaxCol := 25;

    if Length(CharSet) < 4 then
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid CharSet: ' +
                                '' + Charset + '');
    if not (EncType in ['Q', 'B']) then
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid EncType: ' +
                                '' + EncType + '');
    Res    := '';
    Prefix := '=?' + LowerCase(CharSet) + '?' + EncType + '?';
    Len    := Length(Input);
    lPos   := 1;

    if EncType = 'Q' then begin
        if lPos <= Len then
        begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + ['?', '=',  ' ', '_'],
                                  True,
                                  lPos,
                                  DoFold);
            if Length(Res) = 0 then
                Exit;
            if Res[Length(Res)] = '=' then
                SetLength(Res, Length(Res) - 1);
            Result := Prefix + Res  + Suffix;
        end;
        while lPos <= Length(Input) do begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + ['?', '=',  ' ', '_'],
                                  True,
                                  lPos,
                                  DoFold);
            if Length(Res) > 0 then begin
                if Res[Length(Res)] = '=' then
                    SetLength(Res, Length(Res) - 1);
                Result := Result + #13#10#09 + Prefix + Res  + Suffix;
            end;
        end;
    end
    else begin
        { Base64 }
        { taken from function B64Encode and modified slightly }
        if not DoFold then
            MaxCol := MaxInt;

        Res    := Res + Prefix;
        LenRes := Length(Prefix) + 2;

        while lPos <= Len do begin
            if (LenRes + 4 > MaxCol) then begin
                Res := Res + Suffix + #13#10#09 + Prefix;
                LenRes := Length(Prefix) + 2;
            end;
            Res := Res + Base64Out[(Byte(Input[lPos]) and $FC) shr 2];
            if (lPos + 1) <= Len  then begin
                Res := Res + Base64Out[((Byte(Input[lPos]) and $03) shl 4) +
                                       ((Byte(Input[lPos + 1]) and $F0) shr 4)];
                if (lPos + 2) <= Len then begin
                    Res := Res + Base64Out[((Byte(Input[lPos + 1]) and $0F) shl 2) +
                                           ((Byte(Input[lPos + 2]) and $C0) shr 6)];
                    Res := Res + Base64Out[(Byte(Input[lPos + 2]) and $3F)];
                end
                else begin
                    Res := Res + Base64Out[(Byte(Input[lPos + 1]) and $0F) shl 2];
                    Res := Res + '=';
                end
            end
            else begin
                 Res := Res + Base64Out[(Byte(Input[lPos]) and $03) shl 4];
                 Res := Res + '==';
            end;
            Inc(LenRes, 4);
            Inc(lPos, 3);
        end;
        Result := Res + Suffix;
    end;
end;




{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Piotr Hellrayzer Dalek <enigmatical@interia.pl>, AG                                                                    }
{ Use it to code message text that includes extended ASCII chars, passing    }
{ empty Specials '[]' will work mostly.                                      }
{ Param MaxCol should be set to 1 below max. line length                     }

function StrEncodeQP(const Input : String;
                     MaxCol      : Integer;
                     Specials    : TSysCharSet) : String;
var
    cPos, rPos, lPos :Integer;
begin;
    SetLength(Result, Length(Input) * 2);
    cPos   := 1;
    lPos   := 1;
    for rPos := 1 to Length(Input) do begin
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
            if lPos >= MaxCol then begin
                Result[cPos] := '=';
                Inc(cPos);
                Result[cPos] := #13;
                Inc(cPos);
                Result[cPos] := #10;
                Inc(cPos);
                lPos := 1;
            end;
        end
        else begin
            Result[cPos] := Input[rPos];
            Inc(cPos);
            Inc(lPos);
            if lPos >= MaxCol then begin
                Result[cPos] := '=';
                Inc(cPos);
                Result[cPos] := #13;
                Inc(cPos);
                Result[cPos] := #10;
                Inc(cPos);
                lPos := 1;
            end;
        end;
        { Grow }
        if cPos > Length(Result) - 3 then
            SetLength(Result, Length(Result) + MaxCol);
    end;
    Setlength(Result, cPos - 1);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to StrEncodeQP, returns just a coded line                          }
function StrEncodeQPEx(const Buf   : String;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean;
                       var cPos    : Integer;
                       DoFold      : Boolean) : String;
var
    lPosRes : Integer;
begin
    lPosRes := 1;
    if not DoFold then
        MaxCol := Length(Buf);
    SetLength(Result, MaxCol);
    while cPos <= Length(Buf) do begin
        if (Ord(Buf[cPos]) > 126)  or
           (Ord(Buf[cPos]) < 32)   or
           (Buf[cPos] in Specials) or
           (Buf[cPos] = '=') then begin
            if (Buf[cPos] = ' ') and ShortSpace then begin
                Result[lPosRes] := '_';
                Inc(lPosRes);
                Inc(cPos);
            end
            else
            if lPosRes < MaxCol - 2 then begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Result[lPosRes] := HexTable[(Ord(Buf[cPos]) shr 4) and 15];
                Inc(lPosRes);
                Result[lPosRes] := HexTable[Ord(Buf[cPos]) and 15];
                Inc(lPosRes);
                Inc(cPos);
            end
            else begin
                     Result[lPosRes] := '=';
                     Inc(lPosRes);
                     Break;
            end;
        end else
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
{ RFC822 - 3.1.1.  LONG HEADER FIELDS                                       }
{ This is just my (AG) interpretation of folding long header lines.         }
{ Probably further BreakChars are possible here. However before you modify  }
{ this procedure you should refer to RFC822. Also note that header lines may}
{ be encoded 'in-line' as described in RFC2047. The passed HdrLine String   }
{ *MUST not include CRLF except they are followed by one of the space chars,}
{ means that a already folded line should work. If a string doesn't include }
{ one of the BreakChars it won't fold to the next line!                     }

procedure FoldHdrLine(HdrLines      : TStrings;
                      const HdrLine : String);
const
    BreakChars = [#09, #32, ';', ',', '>', ']'];
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(HdrLine) then
        HdrLines.Add(Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                          BreakChars, 76, [], rPos)));
    while rPos <= Length(HdrLine) do
        HdrLines.Add(#09 + Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                                BreakChars, 76, [], rPos)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} {AG}
function FoldString(const Input : String;
                    BreakChars  : TSysCharSet;
                    MaxCol      : Integer): String;
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(Input) then
        Result := Trim(IcsWrapTextEx(Input, #13#10#09,
                       BreakChars, MaxCol, [], rPos));
    while rPos <= Length(Input) do
        Result := Result + #13#10#09 + Trim(IcsWrapTextEx(Input, #13#10#09,
                                                          BreakChars, MaxCol,
                                                          [], rPos))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
