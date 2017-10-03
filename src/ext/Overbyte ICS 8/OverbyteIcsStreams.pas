{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Oct 25, 2005
Description:  Fast streams for ICS tested on D5 and D7.
Version:      8.01
Legal issues: Copyright (C) 2005-2016 by Arno Garrels, Berlin, Germany,
              contact: <arno.garrels@gmx.de>

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

History:
Jan 05, 2006 V1.01 F. Piette added missing resourcestring for Delphi 6
Mar 26, 2006 V6.00 F. Piette started new version 6
Jun 26, 2006 V6.01 A. Garrels fixed corrupted data when Mode contained
             fmOpenWrite. Even in case of fmOpenWrite is set the class needs
             to be able to read from file to keep it's buffer in sync.
Aug 28, 2006 V6.02 Tobias Giesen <tobias_subscriber@tgtools.com> fixed a
             bug in SetSize.
Aug 31, 2006 V6.03 A. Garrels added TMultipartFileReader, a read only file
             stream capable to merge a custom header as well as a footer
             with the file. For instance usefull as a HTTP-POST-stream.             
Aug 31, 2006 V6.04 Do not call SetSize in destroy by A. Garrels.
Jun 01, 2007 V6.05 A. Garrels added TTextStream. A very fast stream wrapper,
             optimized to read and write lines. Methods ReadLn as well
             as WriteLn benefit from internal buffering as long as the same
             method is called more than once in sequence, intended as a
             replacement of ReadLn(TextFile) and WriteLn(TextFile).
Jan 22, 2008 V6.06 Angus allow for read file shrinking with fmShareDenyNone
Mar 24, 2008 V6.07 Francois Piette made some changes to prepare code
                   for Unicode:
                   TTextStream use AnsiString.
Apr 15, 2008 V6.08 A. Garrels, in FBuf of TBufferedFileStream changed to
                   PAnsiChar
Aug 27, 2008 V6.09 Arno added a WideString overload to TBufferedFileStream
Jan 20, 2009 V6.10 Arno added property Mode to TBufferedFileStream.
Apr 17, 2009 V6.11 Arno fixed a bug in TBufferedFileStream that could corrupt
             data on writes after seeks. Size and Position now behave exactly
             as TFileStream, this is a fix as well a *Breaking Change*.
             Fixed some 'false' ERangeErrors when range checking was turned on.
             New class TIcsBufferedStream is compatible with TBufferedFileStream,
             it writes a little bit slower than TBufferedFileStream, however
             contains cleaner code, is hopefully less error prone and can also be
             used as a stream-wrapper in order to buffer any TStream descendant.
             New classes TIcsStreamReader and TIcsStreamWriter derived from
             TIcsBufferedStream ease reading and writing text from/to streams,
             both are still experimental.
             Removed plenty of old conditional code.
May 03, 2009 V6.12 Arno fixed forced line breaks in TIcsStreamReader.
             On forced line breaks a multi-byte character code point including
             UTF-8 will be preserved. Added method ReadToEnd.
May 07, 2009 V6.13 TIcsStreamWriter did not convert from ANSI to UTF-7.
             Note that on forced line breaks (MaxLineLength) TIcsStreamReader
             may still break UTF-7 code points, however UTF-7 should normally be
             used only in the context of 7 bit transports, such as e-mail where
             line length restrictions exist, so setting property MaxLineLength
             to >= 1024 should work around this issue.
Dec 05, 2009 V6.14 Use IcsSwap16Buf() and global code page ID constants.
Oct 20, 2010 V6.15 Fixed a bug in TIcsStreamReader.InternalReadLn.
Oct 20, 2010 V6.16 Fixed a bug in TIcsStreamReader.ReadLn.
Feb 08, 2012 V6.17 Fixed a 64-bit bug and a memory leak in TBufferedFileStream.
                   TIcsBufferedStream, TIcsStreamReader and TIcsStreamWriter
                   removed some default parameter values from constructors,
                   verify that existing code calls the right overload.
Mar 31, 2012 V6.18 Fixed TMultiPartFileReader.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 23, 2016 V8.01 Angus renamed TBufferedFileStream to TIcsBufferedFileStream
                   to avoid conflicts with other libraries

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsStreams;

interface
{$Q-}           { Disable overflow checking           }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$I Include\OverbyteIcsDefs.inc}
{$ObjExportAll On}

{ Comment next line in order to replace TIcsBufferedFileStream by TIcsBufferedStream }
{$Define USE_OLD_BUFFERED_FILESTREAM}

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.UniStd,
    Ics.Posix.WinTypes,
  {$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.RTLConsts{$ELSE}RTLConsts{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Math{$ELSE}Math{$ENDIF},
    OverbyteIcsTypes, // for TBytes
    OverbyteIcsUtils;

resourcestring
    SFCreateErrorEx = 'Cannot create file "%s". %s';
    SFOpenErrorEx   = 'Cannot open file "%s". %s';

const
    DEFAULT_BUFSIZE = 4096;
    MIN_BUFSIZE     = 128;    { V6.11 }
    MAX_BUFSIZE     = 1024 * 64;

type
    BigInt = Int64;
    EBufferedStreamError = class(Exception);

    TIcsBufferedStream = class; // forward
{$IFNDEF USE_OLD_BUFFERED_FILESTREAM}
    TIcsBufferedFileStream = TIcsBufferedStream;
{$ELSE}
    TIcsBufferedFileStream = class(TStream)
    private
        FHandle     : {$IFDEF COMPILER16_UP} THandle {$ELSE} Integer {$ENDIF};
        FFileSize   : BigInt;
        FFileOffset : BigInt;
        FBuf        : TBytes;  { V6.11 }
        FBufSize    : Longint;
        FBufCount   : Longint;
        FBufPos     : Longint;
        FDirty      : Boolean;
        FMode       : Word;
    protected
        procedure   SetSize(NewSize: Longint); override;
        procedure   SetSize(const NewSize: Int64); override;
        function    GetFileSize: BigInt;
        procedure   Init(BufSize: Longint);
        procedure   ReadFromFile;
        procedure   WriteToFile;
    public
        constructor Create(const FileName: String; Mode: Word; BufferSize: Longint); overload;
        constructor Create(const FileName: WideString; Mode: Word; BufferSize: Longint); overload;
        constructor Create(const FileName: String; Mode: Word; Rights: Cardinal; BufferSize: Longint); overload;
        constructor Create(const FileName: WideString; Mode: Word; Rights: Cardinal; BufferSize: Longint); overload;

        destructor  Destroy; override;

        procedure   Flush;
        function    Read(var Buffer; Count: Longint): Longint; override;

        function    Seek(Offset: Longint; Origin: Word): Longint; override;
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function    Write(const Buffer; Count: Longint): Longint; override;
        property    FastSize : BigInt read FFileSize;
        property    Mode : Word read FMode;
    end;
{$ENDIF USE_OLD_BUFFERED_FILESTREAM}

    EMultiPartFileReaderException = class(Exception);

    { Read only file stream capable to merge a custom header as well as a footer }
    { with the file. For instance usefull as a HTTP-POST-stream.                 }
    TMultiPartFileReader = class(TFileStream)
    private
        FHeader     : RawByteString;
        FFooter     : RawByteString;
        FFooterLen  : Integer;
        FHeaderLen  : Integer;
        FCurrentPos : BigInt;
        FFileSize   : BigInt;
        FTotSize    : BigInt;
        function GetFooter: String;
        function GetHeader: String;
    protected
        function    GetSize: Int64; override;
    public
     {$IFDEF UNICODE}
        constructor Create(const FileName: String; const Header, Footer: RawByteString) overload;
     {$ENDIF}
        constructor Create(const FileName: String; const Header, Footer: String
          {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF}); overload;
        constructor Create(const FileName: String; Mode: Word; const Header, Footer: String
          {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF}); overload;
        constructor Create(const FileName: String; Mode: Word; Rights: Cardinal; const Header, Footer: String
          {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF}); overload;
        procedure   SetSize(const NewSize: Int64); override;
        function    Seek(Offset: Longint; Origin: Word): Longint; override;
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function    Read(var Buffer; Count: Longint): Longint; override;
        function    Write(const Buffer; Count: Longint): Longint; override;
        property    Header : String read GetHeader;
        property    Footer : String read GetFooter;
    end;

    TTextStreamMode = (tsmReadLn, tsmWriteLn, tsmRead, tsmWrite);
    TTextStream = class(TStream)
    private
        FStream : TStream;
        FBuf    : array of AnsiChar;
        TF      : TextFile;
        FMode   : TTextStreamMode;
        procedure SetRealPos;
        procedure SetMode(NewMode: TTextStreamMode);
    protected
        procedure SetSize(const NewSize: Int64); override;
    public
        constructor Create(AStream : TStream; BufferSize : Integer = 256 ; Style: TTextLineBreakStyle = tlbsCRLF);
        destructor Destroy; override;
        procedure Flush;
        function  ReadLn: Boolean; overload;
        function  ReadLn(var S: AnsiString): Boolean; overload;
        function  ReadLn(var WS: WideString): Boolean; overload;
        procedure WriteLn(const S: AnsiString); overload;
        procedure WriteLn(const WS: WideString); overload;
        function  Read(var Buffer; Count: Longint): Longint; override;
        function  Write(const Buffer; Count: Longint): Longint; override;
        function  Seek(Offset: Longint; Origin: Word): Longint; override;
        function  Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    end;

    { .todo a base stream wrapper class TIcsStreamWrapper to derive }
    { TIcsBufferedStream from.                                      }

    TIcsBufferedStream = class(TStream)
    private
        FStream           : TStream;
        FOwnsStream       : Boolean;
        FBuffer           : TBytes;
        FBufferSize       : Integer;
        FBufferedDataSize : Integer; // Number of bytes currently buffered
        FDirtyCount       : Integer; // Range of dirty bytes in buffer counted from buffer index zero
        FStreamBufPos     : Int64;   // First byte of the buffer in stream
        FPosition         : Int64;   // Helper var, partly calculated 
        FFastSize         : Int64;   // Size of FStream at the time IsReadOnly is set to TRUE. See property IsReadOnly below
        FIsReadOnly       : Boolean; // See property IsReadOnly below
    protected
        procedure   SetIsReadOnly(const Value: Boolean);
        procedure   SetSize(NewSize: Integer); override;
        procedure   SetSize(const NewSize: Int64); override;
        function    InternalGetSize: Int64; {$IFDEF USE_INLINE} inline;{$ENDIF}
        function    GetSize: Int64; override;
        procedure   Init; virtual;
        function    FillBuffer: Boolean; {$IFDEF USE_INLINE} inline;{$ENDIF}
    public
        constructor Create; overload; // Dummy, don't call!
        constructor Create(Stream     : TStream;
                           BufferSize : Integer;
                           OwnsStream : Boolean = FALSE); overload; virtual;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           Rights         : Cardinal;
                           BufferSize     : Integer); overload; virtual;
        constructor Create(const FileName : WideString;
                           Mode           : Word;
                           Rights         : Cardinal;
                           BufferSize     : Integer); overload; virtual;
        constructor Create(const FileName: String; Mode: Word; BufferSize: Integer); overload; virtual;
        constructor Create(const FileName: WideString; Mode: Word; BufferSize: Integer); overload; virtual;
        destructor  Destroy; override;

        procedure   Flush; {$IFDEF USE_INLINE} inline;{$ENDIF}
        function    Read(var Buffer; Count: Integer): Integer; override;

        function    Seek(Offset: Integer; Origin: Word): Integer; override;
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function    Write(const Buffer; Count: Integer): Integer; override;
        { Set IsReadOnly if you are sure you will never write to the stream  }
        { and nobody else will do, this speeds up getter Size and in turn    }
        { Seeks as well. IsReadOnly is set to TRUE if a constructor with     }
        { filename is called with a read only mode and a share lock.         }
        property    IsReadOnly: Boolean read FIsReadOnly write SetIsReadOnly;
        property    FastSize: Int64 read GetSize; // For compatibility with old TIcsBufferedFileStream; 
    end;

    EStreamReaderError = class(Exception);
    TIcsLineBreakStyle = (ilbsCRLF, ilbsLF, ilbsCR);
    TIcsStreamReader = class(TIcsBufferedStream)
    private
        FReadBuffer     : TBytes;
        FReadBufSize    : Integer;
        FCodePage       : LongWord;
        FLeadBytes      : set of AnsiChar;
        FDetectBOM      : Boolean;
        { Max. number of chars (elements) forcing a new line even though     }
        { none of the break chars was found. Note that this value is not     }
        { accurate since multi-byte code points including UTF-8 will be      }
        { preserved. Default = 2048                                          }
        FMaxLineLength  : Integer;
        function    InternalReadLn: Boolean;
        function    InternalReadLnWLe: Boolean;
        function    InternalReadLnWBe: Boolean;
        procedure   EnsureReadBufferW(Size: Integer; var P: PWideChar);
                         {$IFDEF USE_INLINE} inline;{$ENDIF}
        procedure   EnsureReadBufferA(Size: Integer; var P: PAnsiChar);
                         {$IFDEF USE_INLINE} inline;{$ENDIF}
        procedure   SetMaxLineLength(const Value: Integer);
        procedure   SetCodePage(const Value : LongWord);
    protected
        function    GetCodePageFromBOM: LongWord; virtual;
        procedure   Init; override;
    public
        constructor Create(Stream     : TStream;
                           BufferSize : Integer = DEFAULT_BUFSIZE;
                           OwnsStream : Boolean = FALSE); override;
        constructor Create(Stream     : TStream;
                           DetectBOM  : Boolean = FALSE;
                           CodePage   : LongWord = CP_ACP;
                           OwnsStream : Boolean = FALSE;
                           BufferSize : Integer = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); override;
        constructor Create(const FileName : String;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : WideString;
                           Mode           : Word;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); override;
        constructor Create(const FileName : WideString;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;

        function    DetectLineBreakStyle: TIcsLineBreakStyle;
        function    ReadLine(var S: RawByteString): Boolean; overload; virtual;
        function    ReadLine(var S: UnicodeString): Boolean; overload; virtual;
        procedure   ReadToEnd(var S: RawByteString); overload; virtual;
        procedure   ReadToEnd(var S: UnicodeString); overload; virtual;

        property    CurrentCodePage: LongWord read FCodePage write SetCodePage;
        property    MaxLineLength: Integer read FMaxLineLength write SetMaxLineLength;
    end;

    TIcsStreamWriter = class(TIcsBufferedStream)
    private
        FWriteBuffer    : TBytes;  // For charset conversion
        FWriteBufSize   : Integer;
        FReadBuffer     : TBytes;  // For charset conversion
        FReadBufSize    : Integer;
        FLineBreakStyle : TIcsLineBreakStyle;
        FCodePage       : LongWord;
        FLineBreak      : AnsiString;
        procedure   SetLineBreakStyle(Value: TIcsLineBreakStyle);
        procedure   EnsureWriteBuffer(Size: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   EnsureReadBuffer(Size: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}
    protected
        function    GetBomFromCodePage(ACodePage: LongWord) : TBytes; virtual;
        function    GetCodePageFromBOM: LongWord; virtual;
        procedure   Init; override;
    public
        constructor Create(Stream     : TStream;
                           BufferSize : Integer = DEFAULT_BUFSIZE;
                           OwnsStream : Boolean = FALSE); override;
        constructor Create(Stream     : TStream;
                           Append     : Boolean = TRUE;
                           DetectBOM  : Boolean = FALSE;
                           CodePage   : LongWord = CP_ACP;
                           OwnsStream : Boolean = FALSE;
                           BufferSize : Longint = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : String;
                           Mode           : Word;
                           Rights         : Cardinal;
                           BufferSize     : Integer); override;
        constructor Create(const FileName : String;
                           Append         : Boolean = TRUE;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;

        constructor Create(const FileName : WideString;
                           Mode           : Word;
                           Rights         : Cardinal;
                           BufferSize     : Integer); override;
        constructor Create(const FileName : WideString;
                           Append         : Boolean = TRUE;
                           DetectBOM      : Boolean = TRUE;
                           CodePage       : LongWord = CP_ACP;
                           BufferSize     : Integer = DEFAULT_BUFSIZE); overload;

        function    DetectLineBreakStyle: TIcsLineBreakStyle;
        procedure   Write(const S     : UnicodeString); reintroduce; overload; virtual;
        procedure   Write(const S     : RawByteString;
                          SrcCodePage : LongWord = CP_ACP); reintroduce; overload; virtual;
        {
        procedure   Write(Value: Boolean); reintroduce; overload;
        procedure   Write(Value: WideChar); reintroduce; overload;
        procedure   Write(Value: AnsiChar); reintroduce; overload;
        ..
        }
        procedure   WriteLine(const S     : UnicodeString); overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   WriteLine(const S     : RawByteString;
                              SrcCodePage : LongWord = CP_ACP); overload; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   WriteBOM;
        property    CurrentCodePage : LongWord read FCodePage write FCodePage;
        property    LineBreakStyle  : TIcsLineBreakStyle read  FLineBreakStyle
                                                         write SetLineBreakStyle;
    end;

    TIcsBinaryReader = class
    private
        FStream     : TStream;
        FOwnsStream : Boolean;
    protected
        function GetStream: TStream; virtual;
    public
        constructor Create(Stream: TStream); overload;
        constructor Create(Stream: TStream; AOwnsStream: Boolean = False); overload;
        constructor Create(const Filename: string); overload;
        constructor Create; overload;
        destructor Destroy; override;
        procedure Close; virtual;
        function Read(const Buffer: TBytes; Index, Count: Integer): Integer; overload; virtual;
        function ReadBoolean: Boolean; virtual;
        function ReadByte: Byte; virtual;
        function ReadBytes(Count: Integer): TBytes; virtual;
        function ReadDouble: Double; virtual;
        function ReadShortInt: ShortInt; virtual;
        function ReadInteger: Integer; virtual;
        function ReadLongWord: Longword; virtual;
        function ReadInt64: Int64; virtual;
        function ReadSingle: Single; virtual;
        function ReadAnsiString: AnsiString; virtual;
        function ReadUTF8String: UTF8String; virtual;
        function ReadUnicodeString: UnicodeString; virtual;
        function ReadWord: Word; virtual;
        procedure SetStream(Value: TStream; AOwnsStream: Boolean); virtual;
        property Stream: TStream read GetStream;
    end;

    TIcsBinaryWriter = class
    private
        FStream     : TStream;
        FOwnsStream : Boolean;
    protected
        function GetStream: TStream; virtual;
        constructor Create; overload;
    public
        constructor Create(Stream: TStream); overload;
        constructor Create(Stream: TStream; AOwnsStream: Boolean); overload;
        constructor Create(const Filename: string; Append: Boolean = False); overload;
        destructor Destroy; override;
        procedure Close; virtual;
        procedure Write(Value: Byte); overload; virtual;
        procedure Write(Value: Boolean); overload; virtual;
        procedure Write(const Value: TBytes); overload; virtual;
        procedure Write(Value: Double); overload; virtual;
        procedure Write(Value: Integer); overload; virtual;
        procedure Write(Value: ShortInt); overload; virtual;
        procedure Write(Value: Word); overload; virtual;
        procedure Write(Value: LongWord); overload; virtual;
        procedure Write(Value: Int64); overload; virtual;
        procedure Write(Value: Single); overload; virtual;
        procedure Write(const Value: RawByteString); overload; virtual;
        procedure Write(const Value: UnicodeString); overload; virtual;
        procedure Write(const Buffer: TBytes; Index, Count: Integer); overload; virtual;
        procedure SetStream(Value: TStream; AOwnsStream: Boolean); virtual;
        property Stream: TStream read GetStream;
    end;

implementation

{$IFOPT R+}                                                        { V6.11 }
  {$DEFINE SETRANGECHECKSBACK} { We'll turn off range checking temporarily }
{$ENDIF}

type
    TDummyByteArray = array [0..0] of Byte; { Casts require range checks OFF }{ V6.11 }

const
  {$IFDEF COMPILER16_UP}
    ICS_INVALID_FILE_HANDLE = INVALID_HANDLE_VALUE;
  {$ELSE}
    ICS_INVALID_FILE_HANDLE = -1;
  {$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CheckAddFileModeReadWrite(var AMode: Word);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
var
    LSMode, LOMode: Word;
begin
    LOMode := AMode and 3;
    if LOMode = fmOpenWrite then begin
        LSMode := AMode and $F0;
        AMode := fmOpenReadWrite or LSMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsFileModeReadOnly(AMode: Word): Boolean;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
var
    LOMode: Word;
    LSMode: Word;
begin
    LOMode := AMode and 3;
    LSMode := AMode and $F0;
    Result := (AMode and fmCreate <> fmCreate) and (LOMode = fmOpenRead) and
              ((LSMode = fmShareDenyWrite) or (LSMode = fmShareExclusive));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_OLD_BUFFERED_FILESTREAM}
procedure TIcsBufferedFileStream.Init(BufSize: Longint);
begin
    FBufSize := BufSize;
    if FBufSize < MIN_BUFSIZE then
        FBufsize := MIN_BUFSIZE
    else
    if FBufSize > MAX_BUFSIZE then
        FBufSize := MAX_BUFSIZE
    else
        FBufSize := (BufSize div MIN_BUFSIZE) * MIN_BUFSIZE; { V6.11 }
    SetLength(FBuf, FBufSize);
    FFileSize   := GetFileSize;
    FBufCount   := 0;
    FFileOffset := 0;
    FBufPos     := 0;
    FDirty      := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedFileStream.Create(const FileName: String; Mode: Word;
    BufferSize: Longint);
begin
{$IFDEF MSWINDOWS}
    Create(Filename, Mode, 0, BufferSize);
{$ENDIF}
{$IFDEF POSIX}
    Create(Filename, Mode, FileAccessRights, BufferSize);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedFileStream.Create(const FileName : String; Mode: Word;
    Rights: Cardinal; BufferSize: Longint);
{$IFDEF COMPILER16_UP}
var
    LMode: Word;
{$ENDIF}
begin
    inherited Create;
    FHandle := ICS_INVALID_FILE_HANDLE;
    FBuf    := nil;
    //FmWriteFlag := FALSE;  { V1.04 }
    if Mode and fmCreate = fmCreate then begin
      {$IFDEF COMPILER16_UP}
        LMode := Mode and $FF;
        if LMode = $FF then
          LMode := fmShareExclusive;
      {$ENDIF}
        FHandle := FileCreate(FileName,
                            {$IFDEF COMPILER16_UP}
                              LMode,
                            {$ENDIF}  
                              Rights);
        if FHandle = ICS_INVALID_FILE_HANDLE then
            raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
                                             [ExpandFileName(FileName),
                                             SysErrorMessage(GetLastError)]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        CheckAddFileModeReadWrite(Mode);
        FHandle := FileOpen(FileName, Mode);
        if FHandle = ICS_INVALID_FILE_HANDLE then
            raise EFOpenError.CreateResFmt(@SFOpenErrorEx,
                                           [ExpandFileName(FileName),
                                           SysErrorMessage(GetLastError)]);
    end;
    FMode := Mode;
    Init(BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedFileStream.Create(const FileName: WideString; Mode: Word;
    BufferSize: Longint);
begin
{$IFDEF MSWINDOWS}
    Create(Filename, Mode, 0, BufferSize);
{$ENDIF}
{$IFDEF POSIX}
    Create(Filename, Mode, FileAccessRights, BufferSize);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedFileStream.Create(const FileName : WideString; Mode: Word;
    Rights: Cardinal; BufferSize: Longint);
begin
    inherited Create;
    FHandle := ICS_INVALID_FILE_HANDLE;
    FBuf    := nil;
    if Mode and fmCreate = fmCreate then begin
        FHandle := IcsFileCreateW(FileName, Rights);
        if FHandle = ICS_INVALID_FILE_HANDLE then
            raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
                                             [ExpandFileName(FileName),
                                             SysErrorMessage(GetLastError)]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        CheckAddFileModeReadWrite(Mode);
        FHandle := IcsFileOpenW(FileName, Mode);
        if FHandle = ICS_INVALID_FILE_HANDLE then
            raise EFOpenError.CreateResFmt(@SFOpenErrorEx,
                                           [ExpandFileName(FileName),
                                           SysErrorMessage(GetLastError)]);
    end;
    FMode := Mode;
    Init(BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBufferedFileStream.Destroy;
begin
    if FHandle <> ICS_INVALID_FILE_HANDLE then begin
        if FDirty then
            WriteToFile;
        FileClose(FHandle);
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedFileStream.GetFileSize: BigInt;
var
    OldPos : BigInt;
begin
    OldPos := FileSeek(FHandle, Int64(0), sofromCurrent);
    Result := FileSeek(FHandle, Int64(0), sofromEnd);
    FileSeek(FHandle, OldPos, sofromBeginning);
    if Result < 0 then
        raise EBufferedStreamError.Create('Cannot determine correct file size');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedFileStream.ReadFromFile;
begin
    if FileSeek(FHandle, FFileOffset, sofromBeginning) <> FFileOffset then
        raise EBufferedStreamError.Create('Seek before read from file failed');
    FBufCount := FileRead(FHandle, FBuf[0], FBufSize);
    if FBufCount = -1 then
        raise EBufferedStreamError.Create(SysErrorMessage(GetLastError));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedFileStream.WriteToFile;
begin
    if FileSeek(FHandle, FFileOffset, soFromBeginning) <> FFileOffset then
        raise EBufferedStreamError.Create('Seek before write to file failed');
    if FileWrite(FHandle, FBuf[0], FBufCount) <> FBufCount then
        raise EBufferedStreamError.Create('Could not write to file');
{$IFDEF DEBUG}
    //FillChar(FBuf[0], FBufCount, #0);
{$ENDIF}
    FDirty := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedFileStream.Flush;
begin
    if FDirty and (FHandle <> ICS_INVALID_FILE_HANDLE) and (FBuf <> nil) then
        WriteToFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$R-} { V6.11 }
function TIcsBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
    Remaining   : Longint;
    Copied      : Longint;
    DestPos     : Longint;
begin
    Result := 0;
    if FHandle = ICS_INVALID_FILE_HANDLE then Exit;
    Remaining := Min(Count, FFileSize - (FFileOffset + FBufPos));
    Result := Remaining;
    if (Remaining > 0) then begin
        if (FBufCount = 0) then
            ReadFromFile;
        Copied := Min(Remaining, FBufCount - FBufPos);
        Move(FBuf[FBufPos], TDummyByteArray(Buffer)[0], Copied); { V6.11 }
        Inc(FBufPos, Copied);
        Dec(Remaining, Copied);
        DestPos := 0;
        while Remaining > 0 do begin
            if FDirty then
                WriteToFile;
            FBufPos := 0;
            Inc(FFileOffset, FBufSize);
            ReadFromFile;
            Inc(DestPos, Copied);
            Copied := Min(Remaining, FBufCount - FBufPos);
            if Copied <= 0 then break;  { V6.06 angus, nothing more to read, break loop }
            Move(FBuf[FBufPos], TDummyByteArray(Buffer)[DestPos], Copied); { V6.11 }
            Inc(FBufPos, Copied);
            Dec(Remaining, Copied);
        end;
    end;
end;
{$IFDEF SETRANGECHECKSBACK}  { V6.11 }
  {$R+}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$R-}  { V6.11 }
function TIcsBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
    Remaining : Longint;
    Copied    : Longint;
    DestPos   : Longint;
begin
    Result := 0;
    if FHandle = ICS_INVALID_FILE_HANDLE then Exit;
    Remaining := Count;
    Result := Remaining;
    if (Remaining > 0) then begin
        if (FBufCount = 0) and ((FFileOffset {+ FBufPos} ) <= FFileSize) then { V6.11 }
            ReadFromFile;
        Copied := Min(Remaining, FBufSize - FBufPos);
        Move(TDummyByteArray(Buffer)[0], FBuf[FBufPos], Copied); { V6.11 }
        FDirty := True;
        Inc(FBufPos, Copied);
        if (FBufCount < FBufPos) then begin
            FBufCount := FBufPos;
            FFileSize := FFileOffset + FBufPos;
        end;
        Dec(Remaining, Copied);
        DestPos := 0;
        while Remaining > 0 do begin
            WriteToFile;
            FBufPos := 0;
            Inc(FFileOffset, FBufSize);
            if (FFileOffset < FFileSize) then
                ReadFromFile
            else
                FBufCount := 0;
            Inc(DestPos, Copied);
            Copied := Min(Remaining, FBufSize - FBufPos);
            if Copied <= 0 then break;  { V6.06 angus, nothing more to read, break loop }
            Move(TDummyByteArray(Buffer)[DestPos], FBuf[0], Copied); { V6.11 }
            FDirty := True;
            Inc(FBufPos, Copied);
            if (FBufCount < FBufPos) then begin
                FBufCount := FBufPos;
                FFileSize := FFileOffset + FBufPos;
            end;
            Dec(Remaining, Copied);
        end;
    end;
end;
{$IFDEF SETRANGECHECKSBACK} { V6.11 }
    {$R+}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
    NewPos        : BigInt;
    NewFileOffset : BigInt;
begin
    Result := 0;
    if FHandle = ICS_INVALID_FILE_HANDLE then Exit;

    {if (Offset = 0) and (Origin = soCurrent) then begin   V6.11
        Result := FFileOffset + FBufPos;
        Exit;
    end;}
    
    case Origin of
        soBeginning : NewPos := Offset;
        soCurrent   : NewPos := (FFileOffset + FBufPos) + Offset;
        soEnd       : NewPos := FFileSize + Offset;
    else
        NewPos := -1; { V6.11 }
    end;

    if (NewPos < 0) then begin
        //NewPos := 0; { V6.11 }
        Result := -1;  { V6.11 }
        Exit;
    end;    

    {else if (NewPos > FFileSize) then  // FileSeek now called in SetSize() V6.11
        FFileSize := FileSeek(FHandle, NewPos - FFileSize, soFromEnd);}

    NewFileOffset := (NewPos div FBufSize) * FBufSize;

    if (NewFileOffset <> FFileOffset) then begin
        if FDirty then
            WriteToFile;
        FFileOffset := NewFileOffset;
        FBufCount := 0;
    end;
    FBufPos := NewPos - FFileOffset;
    Result  := NewPos;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedFileStream.SetSize(NewSize: Integer);
begin
    SetSize(Int64(NewSize));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedFileStream.SetSize(const NewSize: Int64);
var
    NSize : Int64;                                          { V6.11 }
begin
    if FHandle = ICS_INVALID_FILE_HANDLE then Exit;
    Seek(NewSize, {sofromBeginning} soBeginning); {TG 08/28/2006} { V1.03 }
    // if NewSize < FFileSize then                            { V6.11 }
    NSize := FileSeek(FHandle, NewSize, soFromBeginning);   { V6.11 }
{$IFDEF MSWINDOWS}                                        { V6.11 }
    if not SetEndOfFile(FHandle) then
{$ELSE}
    if ftruncate(FHandle, Position) = -1 then
{$ENDIF}
        raise EBufferedStreamError.Create(sStreamSetSize);
    if NSize >= 0 then                                      { V6.11 }
        FFileSize := NSize;                                 { V6.11 }
end;

{$ENDIF USE_OLD_BUFFERED_FILESTREAM}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TMultiPartFileReader }
{$IFDEF UNICODE}
constructor TMultiPartFileReader.Create(const FileName: String;
  const Header, Footer: RawByteString);
begin
    inherited Create(FileName, fmOpenRead or fmShareDenyNone);
    FHeader     := Header;
    FFooter     := Footer;
    FFooterLen  := Length(FFooter);
    FHeaderLen  := Length(FHeader);
    FCurrentPos := 0;
    FFileSize   := inherited Seek(0, soEnd);
    FTotSize    := FHeaderLen + FFileSize + FFooterLen;
    inherited Seek(0, soBeginning);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMultiPartFileReader.Create(const FileName: String;
  const Header, Footer: String
  {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF});
begin
    Create(Filename, fmOpenRead or fmShareDenyWrite, Header, Footer
    {$IFDEF UNICODE}, AStringCodePage {$ENDIF});
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMultiPartFileReader.Create(const FileName: String; Mode: Word;
    const Header, Footer: String
    {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF});
begin
  {$IFDEF POSIX}
    Create(Filename, Mode, FileAccessRights, Header, Footer, AStringCodePage);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Create(Filename, Mode, 0, Header, Footer
      {$IFDEF UNICODE}, AStringCodePage {$ENDIF});
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMultiPartFileReader.Create(const FileName: String; Mode: Word;
    Rights: Cardinal; const Header, Footer: String
    {$IFDEF UNICODE}; AStringCodePage: LongWord = CP_ACP {$ENDIF});
begin
    if (Mode and fmOpenWrite <> 0) or
       (Mode and fmOpenReadWrite <> 0)  then
        raise EMultiPartFileReaderException.Create('Invalid open mode');
    inherited Create(FileName, Mode, Rights);
  {$IFDEF UNICODE}
    FHeader     := UnicodeToAnsi(Header, AStringCodePage, True);
    FFooter     := UnicodeToAnsi(Footer, AStringCodePage, True);
  {$ELSE}
    FHeader     := Header;
    FFooter     := Footer;
  {$ENDIF}
    FFooterLen  := Length(FFooter);
    FHeaderLen  := Length(FHeader);
    FCurrentPos := 0;
    FFileSize   := inherited Seek(0, soEnd);
    FTotSize    := FHeaderLen + FFileSize + FFooterLen;
    inherited Seek(0, soBeginning);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.GetFooter: String;
begin
  {$IFDEF UNICODE}
    Result := UnicodeString(FFooter);
  {$ELSE}
    Result := FFooter;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.GetHeader: String;
begin
  {$IFDEF UNICODE}
    Result := UnicodeString(FHeader);
  {$ELSE}
    Result := FHeader;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.GetSize: Int64;
begin
    Result := FTotSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultiPartFileReader.SetSize(const NewSize: Int64);
begin
    raise EMultiPartFileReaderException.Create('Class is read only');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Read(var Buffer; Count: Integer): Longint;
var
    ToRead    : Integer;
    FooterPos : Integer;
    POutBuf   : PAnsiChar;
begin
    Result := 0;
    if (Count <= 0) or (FCurrentPos >= FTotSize) then
        Exit;
    POutBuf := @Buffer;
    if FCurrentPos < FHeaderLen then begin
        ToRead := FHeaderLen - FCurrentPos;
        if Count < ToRead then
            ToRead := Count;
        Move(FHeader[FCurrentPos + 1], POutBuf[0], ToRead);
        Inc(Result, ToRead);
        Inc(FCurrentPos, ToRead);
        Dec(Count, ToRead);
    end;
    if (Count > 0) and (FCurrentPos < FHeaderLen + FFileSize) then begin
        ToRead := inherited Read(POutBuf[Result], Count);
        Inc(Result, ToRead);
        Inc(FCurrentPos, ToRead);
        Dec(Count, ToRead);
    end;
    if (Count > 0) and
       (FCurrentPos >= FHeaderLen + FFileSize) and
       (FCurrentPos < FTotSize) then begin
        FooterPos := FCurrentPos - (FHeaderLen + FFileSize);
        ToRead := FTotSize - FCurrentPos;
        if ToRead > Count then
            ToRead := Count;
        Move(FFooter[FooterPos + 1], POutBuf[Result], ToRead);
        Inc(Result, ToRead);
        Inc(FCurrentPos, ToRead);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Write(const Buffer; Count: Integer): Longint;
begin
    Result := 0;  // Read only!
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Seek(Offset: Integer; Origin: Word): Longint;
begin
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
    NewPos   : Int64;
begin
    case Origin of
        soBeginning : if Offset < 0 then
                          NewPos := 0
                      else
                          NewPos := Offset;
        soCurrent   : NewPos := FCurrentPos + Offset;
        soEnd       : NewPos := (FFileSize + FFooterLen + FHeaderLen) + Offset;
        else
            raise EMultiPartFileReaderException.Create('Invalid seek origin');
    end;
    if NewPos < 0 then
        NewPos := 0;
    if NewPos <> FCurrentPos then begin
        if NewPos > FHeaderLen then begin
            if NewPos <= (FHeaderLen + FFileSize) then
                inherited Seek(NewPos - FHeaderLen , soBeginning)
            else
                if NewPos >= (FHeaderLen + FFileSize + FFooterLen) then begin
                    inherited Seek(NewPos - FHeaderLen - FFooterLen, soBeginning);
                    NewPos := FHeaderLen + FFileSize + FFooterLen;
                end
                else
                    inherited Seek(NewPos - FHeaderLen, soBeginning);
        end
        else
            inherited Seek(0, soBeginning);
    end;
    FCurrentPos := NewPos;
    Result      := NewPos;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TTextStream }

type
    { 32 bytes max }
    TTextStreamUserData = record
        Stream    : TStream;
        StreamPos : BigInt;
    end;
    PTextStreamUserData = ^TTextStreamUserData;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamDummy(var TR: TTextRec ): Integer;
begin
    Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetTextStream(var TR: TTextRec): TStream;
begin
    Result := PTextStreamUserData(@TR.Userdata)^.Stream;
    if Result = nil then
        raise Exception.Create('Stream not assigned');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamIn(var TR: TTextRec): Integer;
begin
    Result := 0;
    TR.BufEnd := GetTextStream(TR).Read(TR.BufPtr^, TR.BufSize);
    Inc(PTextStreamUserData(@TR.Userdata)^.StreamPos, TR.BufEnd);
    TR.BufPos := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamOut(var TR: TTextRec): Integer;
var
    Len : Cardinal;
begin
    Result := 0;
    if TR.BufPos > 0 then
    begin
        Len := GetTextStream(TR).Write(TR.BufPtr^, TR.BufPos);
        if Len <> TR.BufPos then
            Result := GetLastError;
        Inc(PTextStreamUserData(@TR.Userdata)^.StreamPos, Len);
        TR.BufPos := TR.BufPos - Len;
        if Result <> 0 then
            raise Exception.Create(SWriteError + ' ' + SysErrorMessage(Result));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamFlushOut(var TR: TTextRec): Integer;
begin
    Result := TextStreamOut(TR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTextStream.Create(
    AStream    : TStream;
    BufferSize : Integer;
    Style      : TTextLineBreakStyle);
var
    UData : TTextStreamUserData;
begin
    inherited Create;
    FStream := AStream;
    if not Assigned(FStream) then
        raise Exception.Create('Stream not assigned');
    // FStream.Position  := 0;

    FillChar(TTextRec(TF), Sizeof(TFileRec), #0);
    with TTextRec(TF) do
    begin
        { Init TextRec and put it into input mode }
        Mode      := fmInput;
        InOutFunc := @TextStreamIn;
        FlushFunc := @TextStreamDummy;
        //BufPos    := 0;
        //BufEnd    := 0;
        Flags     := (Flags and not tfCRLF) or (tfCRLF * Byte(Style));
        OpenFunc  := @TextStreamDummy;
        CloseFunc := @TextStreamDummy;
        //Name[0]  := #0;

        {Set and allocate buffer }
        BufSize := BufferSize;
        if BufSize < SizeOf(Buffer) then
            BufSize := SizeOf(Buffer)
        else if BufSize mod SizeOf(Buffer) <> 0 then
            BufSize := ((BufSize div SizeOf(Buffer)) * SizeOf(Buffer));
        if BufSize > SizeOf(Buffer) then
        begin
            SetLength(FBuf, BufSize);
            BufPtr := PAnsiChar(FBuf);
        end
        else begin
            BufSize := SizeOf(Buffer);
            BufPtr  := @Buffer;
        end;

        { Userdata }
        UData.Stream    := FStream;
        UData.StreamPos := 0;
        Move(UData, Userdata, Sizeof(UData));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTextStream.Destroy;
begin
    try
        System.CloseFile(TF);
    finally
        SetLength(FBuf, 0);
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetRealPos;
begin
    PTextStreamUserData(@TTextRec(TF).UserData)^.StreamPos :=
    FStream.Seek(- BigInt(TTextRec(TF).BufSize - TTextRec(TF).BufPos), soCurrent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetMode(NewMode: TTextStreamMode);
begin
    if (FMode = NewMode) then
        Exit;
    case NewMode of
        tsmReadLn :
            with TTextRec(TF) do
            begin
                if FMode = tsmWriteLn then
                    System.Flush(TF);
                Mode      := fmInput;
                InOutFunc := @TextStreamIn;
                FlushFunc := @TextStreamDummy;
                BufPos    := 0;
                BufEnd    := 0;
            end;
        tsmWriteLn :
            with TTextRec(TF) do
            begin
                if FMode = tsmReadLn then
                    SetRealPos;
                Mode      := fmOutput;
                InOutFunc := @TextStreamOut;
                FlushFunc := @TextStreamFlushOut;
                BufPos    := 0;
                BufEnd    := 0;
            end;
        tsmWrite, tsmRead :
            if FMode = tsmReadLn then
                SetRealPos
            else if FMode = tsmWriteLn then
                System.Flush(TF);
    end;
    FMode := NewMode
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.ReadLn : Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.ReadLn(var S: AnsiString): Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF, S);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.ReadLn(var WS: WideString): Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF, WS);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.WriteLn(const S: AnsiString);
begin
    SetMode(tsmWriteLn);
    System.Writeln(TF, S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.WriteLn(const WS: WideString);
begin
    SetMode(tsmWriteLn);
    System.Writeln(TF, WS);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Read(var Buffer; Count: Integer): Longint;
begin
    SetMode(tsmRead);
    Result := FStream.Read(Buffer, Count)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Write(const Buffer; Count: Integer): Longint;
begin
    SetMode(tsmWrite);
    Result := FStream.Write(Buffer, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    SetMode(tsmRead);
    Result := FStream.Seek(Offset, Origin);
    PTextStreamUserData(@TTextRec(TF).UserData)^.StreamPos := Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetSize(const NewSize: Int64);
begin
    FStream.Size := NewSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.Flush;
begin
    if FMode = tsmWriteLn then
        System.Flush(TF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsBufferedStream }
{$IFDEF MSWINDOWS}
var
  FileAccessRights : Cardinal = 0;
{$ENDIF}

constructor TIcsBufferedStream.Create(Stream: TStream; BufferSize: Longint;
  OwnsStream: Boolean = FALSE);
begin
    inherited Create;
    Assert(Stream <> nil, 'Stream must be assigned');
    FStream := Stream;
    FOwnsStream := OwnsStream;
    FBufferSize := BufferSize;
    Init;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedStream.Create;
begin
    Create(nil, 0); // dummy!
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedStream.Create(const FileName: String; Mode: Word;
  BufferSize: Integer);
begin
    Create(FileName, Mode, FileAccessRights, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedStream.Create(const FileName: WideString; Mode: Word;
  BufferSize: Integer);
begin
    Create(FileName, Mode, FileAccessRights, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedStream.Create(const FileName: String; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
    inherited Create;
    { Even in mode fmOpenWrite we need to read from file as well }
    CheckAddFileModeReadWrite(Mode);
    FStream := TFileStream.Create(FileName, Mode, Rights);
    FBufferSize := BufferSize;
    FOwnsStream := True;
    IsReadOnly := IsFileModeReadOnly(Mode);
    Init;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBufferedStream.Create(const FileName: WideString; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
    inherited Create;
    { Even in mode fmOpenWrite we need to read from file as well }
    CheckAddFileModeReadWrite(Mode);
    FStream := TIcsFileStreamW.Create(FileName, Mode, Rights);
    FBufferSize := BufferSize;
    FOwnsStream := True;
    IsReadOnly := IsFileModeReadOnly(Mode);
    Init;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBufferedStream.Destroy;
begin
    Flush;
    if FOwnsStream then
        FreeAndNil(FStream);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedStream.Flush;
begin
    if FDirtyCount > 0 then begin
        FStream.Position := FStreamBufPos;
        FStream.WriteBuffer(FBuffer[0], FDirtyCount);
        FDirtyCount := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedStream.Init;
begin
    if FBufferSize < MIN_BUFSIZE then
        FBufferSize := MIN_BUFSIZE
    else if FBufferSize > MAX_BUFSIZE then
        FBufferSize := MAX_BUFSIZE
    else
        FBufferSize := (FBufferSize div MIN_BUFSIZE) * MIN_BUFSIZE;
    SetLength(FBuffer, FBufferSize);
    FPosition := FStream.Position;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.Read(var Buffer; Count: Integer): Longint;
var
    BufPos   : Integer;
    SrcIndex : Integer;
    Remaining: Integer;
begin
    Result := Count;
    while Count > 0 do begin
        if not ((FStreamBufPos <= FPosition) and
                (FPosition < (FStreamBufPos + FBufferedDataSize))) then
            if not FillBuffer then
                Break;
        { Read from buffer }
        SrcIndex := Result - Count;
        Remaining := Count;
        BufPos := FPosition - FStreamBufPos;
        if Remaining > FBufferedDataSize - BufPos then
            Remaining := FBufferedDataSize - BufPos;
      {$R-}
        Move(FBuffer[BufPos], TDummyByteArray(Buffer)[SrcIndex], Remaining);
    {$IFDEF SETRANGECHECKSBACK}
      {$R+}
    {$ENDIF}
        Inc(FPosition, Remaining);
        Dec(Count, Remaining);
    end;
    Result := Result - Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.FillBuffer: Boolean;
begin
    Flush;
    FStream.Position  := FPosition;
    FStreamBufPos     := FPosition;
    FBufferedDataSize := FStream.Read(FBuffer[0], FBufferSize);
    Result            := FBufferedDataSize > 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
    NewPos: Int64;
begin
    case Origin of
        soBeginning : NewPos := Offset;
        soCurrent   : NewPos := FPosition + Offset;
        soEnd       :
            begin
                NewPos := InternalGetSize + Offset;
                if (FDirtyCount > 0) and
                   (NewPos < FStreamBufPos + FDirtyCount) then
                begin
                    Flush;
                    NewPos := FStream.Size + Offset;
                end;
            end;
        else
            NewPos := -1;
    end;
    if NewPos < 0 then
        NewPos := -1
    else
        FPosition := NewPos;
    Result := NewPos;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedStream.SetIsReadOnly(const Value: Boolean);
begin
    FIsReadOnly := Value;
    if FIsReadOnly then
        FFastSize := FStream.Size
    else
        FFastSize := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.InternalGetSize: Int64;
begin
    if IsReadOnly then
        Result := FFastSize
    else
        Result := FStream.Size;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.GetSize: Int64;
begin
    { Gets the calculated size in order to not trigger Flush in method  }
    { Seek which was wasted time. Do not call inherited.                }
    Result := InternalGetSize;
    if Result < FStreamBufPos + FDirtyCount then
        Result := FStreamBufPos + FDirtyCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedStream.SetSize(const NewSize: Int64);
begin
    FStream.Size := NewSize;
    FPosition := FStream.Position;
    if NewSize < (FStreamBufPos + FDirtyCount) then begin
        FDirtyCount := NewSize - FStreamBufPos;
        if FDirtyCount < 0 then
            FDirtyCount := 0;
    end;
    if NewSize < (FStreamBufPos + FBufferedDataSize) then begin
        FBufferedDataSize := NewSize - FStreamBufPos;
        if FBufferedDataSize < 0 then
            FBufferedDataSize := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBufferedStream.SetSize(NewSize: Integer);
begin
    SetSize(Int64(NewSize));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBufferedStream.Write(const Buffer; Count: Integer): Longint;
var
    DestPos   : Integer;
    SrcIndex  : Integer;
    Remaining : Integer;
begin
    Result := Count;
    while Count > 0 do begin
        if not ((FStreamBufPos <= FPosition) and
                (FPosition < (FStreamBufPos + FBufferedDataSize))) then
            if not ((FStreamBufPos <= FPosition) and
                    (FPosition < (FStreamBufPos + FBufferSize))) then
                FillBuffer;
        { Write to buffer }
        SrcIndex := Result - Count;
        Remaining := Count;
        DestPos := FPosition - FStreamBufPos;
        if Remaining > FBufferSize - DestPos then
            Remaining := FBufferSize - DestPos;
        if FBufferedDataSize < DestPos + Remaining then
            FBufferedDataSize := DestPos + Remaining;
      {$R-}
        Move(TDummyByteArray(Buffer)[SrcIndex], FBuffer[DestPos], Remaining);
    {$IFDEF SETRANGECHECKSBACK}
      {$R+}
    {$ENDIF}
        FDirtyCount := DestPos + Remaining;
        Inc(FPosition, Remaining);
        Dec(Count, Remaining);
    end;
    Result := Result - Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsStreamReader }

const
    DefaultBufferSize = 1024 * SizeOf(Char);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(Stream: TStream;
  BufferSize: Integer = DEFAULT_BUFSIZE; OwnsStream: Boolean = FALSE);
begin
    Create(Stream, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(Stream: TStream; DetectBOM: Boolean = FALSE;
  CodePage: LongWord = CP_ACP; OwnsStream: Boolean = FALSE;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FDetectBOM := DetectBOM;
    FCodePage  := CodePage;
    inherited Create(Stream, BufferSize, OwnsStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(const FileName: String; Mode: Word;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    Create(FileName, FALSE, CP_ACP, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(const FileName: String;
  DetectBOM: Boolean = TRUE; CodePage: LongWord = CP_ACP;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FDetectBOM := DetectBOM;
    FCodePage  := CodePage;
    inherited Create(FileName, fmOpenRead or fmShareDenyWrite, FileAccessRights,
                     BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(const FileName: WideString; Mode: Word;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    Create(FileName, FALSE, CP_ACP, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamReader.Create(const FileName: WideString;
  DetectBOM: Boolean = TRUE; CodePage: LongWord = CP_ACP;
  BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FDetectBOM := DetectBOM;
    FCodePage  := CodePage;
    inherited Create(FileName, fmOpenRead or fmShareDenyWrite, FileAccessRights,
                     BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.Init;
begin
    inherited;
    FReadBufSize   := DefaultBufferSize;
    SetMaxLineLength(2048);
    SetLength(FReadBuffer, FReadBufSize + SizeOf(Char));
    if FDetectBom then
        SetCodePage(GetCodePageFromBOM)
    else
        SetCodePage(FCodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.SetMaxLineLength(const Value: Integer);
begin
    if Value < 1 then
        FMaxLineLength := 1
    else
        FMaxLineLength := Value
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.SetCodePage(const Value: LongWord);
{$IFDEF MSWINDOWS}
var
    CPInfo : TCPInfo;
    I      : Integer;
    J      : Byte;
{$ENDIF}
begin
    case Value of
        CP_ACP      :
            begin
                FLeadBytes := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.Leadbytes;
                FCodePage  := Value;
            end;
        CP_UTF8,
        CP_UTF16,
        CP_UTF16Be  :
            begin
                FLeadBytes := [];
                FCodePage  := Value;
            end;
        else
          {$IFDEF MSWINDOWS}
            if GetCPInfo(Value, CPInfo) then begin
                FCodePage := Value;

                if CPInfo.MaxCharSize > 1 then begin
                    I := 0;
                    while (I < MAX_LEADBYTES) and
                          ((CPInfo.LeadByte[I] or CPInfo.LeadByte[I + 1]) <> 0) do begin
                        for J := CPInfo.LeadByte[I] to CPInfo.LeadByte[I + 1] do
                            Include(FLeadBytes, AnsiChar(J));
                        Inc(I, 2);
                    end;
                end
                else
                    FLeadBytes := [];
            end
            else
                raise EStreamReaderError.Create(SysErrorMessage(GetLastError));
         {$ENDIF}
         {$IFDEF POSIX}
            FCodePage  := Value;
            FLeadBytes := IcsGetLeadBytes(FCodePage);
         {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.GetCodePageFromBOM: LongWord;
var
    OldPos : Int64;
    A : array [0..2] of Byte;
    BomLen : Integer;
begin
    FillChar(A, 3, #0);
    OldPos := Position;
    Seek(0, soBeginning);
    Read(A, 3);
    if (A[0] = $FF) and (A[1] = $FE) then begin
        Result := CP_UTF16;
        BomLen := 2;
    end
    else if (A[0] = $FE) and (A[1] = $FF) then begin
        Result := CP_UTF16Be;
        BomLen := 2;
    end
    else if (A[0] = $EF) and (A[1] = $BB) and (A[2] = $BF) then begin
        Result := CP_UTF8;
        BomLen := 3;
    end
    else begin
        Result := CP_ACP;
        BomLen := 0;
    end;
    if OldPos > BomLen then
        Position := OldPos
    else
        Position := BomLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.DetectLineBreakStyle: TIcsLineBreakStyle;
var
    OldPos    : Int64;
    ChA       : AnsiChar;
    ChW       : WideChar;
    CodePage  : LongWord;
begin
    Result := ilbsCRLF;
    OldPos := Position;
    CodePage := GetCodePageFromBOM;
    try
    case CodePage of
        CP_UTF16, CP_UTF16Be :
        begin
            Seek(2, soBeginning);
            while Read(ChW, SizeOf(ChW)) = SizeOf(ChW) do
            begin
                if CodePage = CP_UTF16Be then
                    ChW := WideChar((Word(ChW) shl 8) or (Word(ChW) shr 8));
                case ChW of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
        end;
        else // case
            if CodePage = CP_UTF8 then
                Seek(3, soBeginning);

            while Read(ChA, SizeOf(ChA)) = SizeOf(ChA) do
            begin
                case ChA of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
    end;
    finally
        Position := OldPos;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.InternalReadLn: Boolean;
var
    Ch       : AnsiChar;
    Idx      : Integer;
    P        : PAnsiChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PAnsiChar(@FReadBuffer[0]);
    while Read(Ch, SizeOf(AnsiChar)) = SizeOf(AnsiChar) do begin
        Inc(Idx);
        if (Idx >= FMaxLineLength) then begin
            if ((FCodePage <> CP_UTF8) and (not (Ch in FLeadBytes))) or
               ((FCodePage = CP_UTF8) and (not IsUtf8TrailByte(Byte(Ch)))) then
            begin
                Seek(-1, soCurrent);
                Result := TRUE;
                Exit;
            end;
        end;
        EnsureReadBufferA(Idx + 1, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-1, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end
            else
                if Flag then begin
                    Seek(-1, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.InternalReadLnWLe: Boolean;
var
    Ch       : WideChar;
    Idx      : Integer;
    P        : PWideChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PWideChar(@FReadBuffer[0]);
    while Read(Ch, SizeOf(WideChar)) = SizeOf(WideChar) do
    begin
        Inc(Idx);
        if (Idx >= FMaxLineLength) and (not IsLeadChar(Ch)) then begin
            Seek(-2, soCurrent);
            Result := TRUE;
            Exit;
        end;
        EnsureReadBufferW((Idx + 1) * 2, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-2, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end
            else
                if Flag then begin
                    Seek(-2, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.InternalReadLnWBe: Boolean;
var
    Ch       : WideChar;
    Wrd      : Word;
    Idx      : Integer;
    P        : PWideChar;
    Flag     : Boolean;
begin
    Flag := FALSE;
    Idx := -1;
    P := PWideChar(@FReadBuffer[0]);
    while Read(Wrd, SizeOf(Word)) = SizeOf(Word) do begin
        Inc(Idx);
        Ch := WideChar((Wrd shr 8) or (Wrd shl 8));
        if (Idx >= FMaxLineLength) and (not IsLeadChar(Ch)) then begin
            Seek(-2, soCurrent);
            Result := TRUE;
            Exit;
        end;
        EnsureReadBufferW((Idx + 1) * 2, P);
        case Ch of
            #10 :
                begin
                    P[Idx] := #0;
                    Result := TRUE;
                    Exit;
                end;
            #13 :
                begin
                    if Flag then begin
                        Seek(-2, soCurrent);
                        Result := TRUE;
                        Exit;
                    end;
                    P[Idx] := #0;
                    Flag := TRUE;
                end
            else
                if Flag then begin
                    Seek(-2, soCurrent);
                    Result := TRUE;
                    Exit;
                end;
                P[Idx] := Ch;
        end;
    end;
    if Idx >= 0 then begin
        P[Idx + 1] := #0;
        Result := TRUE;
    end
    else begin
        P[0]  := #0;
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.ReadLine(var S: UnicodeString): Boolean;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                Result := InternalReadLnWLe;
                S := PWideChar(@FReadBuffer[0]);
            end;
        CP_UTF16Be :
            begin
                Result := InternalReadLnWBe;
                S := PWideChar(@FReadBuffer[0]);
            end;
        else
            Result := InternalReadLn;
            S := AnsiToUnicode(PAnsiChar(@FReadBuffer[0]), FCodePage);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamReader.ReadLine(var S: RawByteString): Boolean;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                Result := InternalReadLnWLe;
                S := UnicodeToAnsi(PWideChar(@FReadBuffer[0]), CP_ACP, TRUE);
            end;
        CP_UTF16Be :
            begin
                Result := InternalReadLnWBe;
                S := UnicodeToAnsi(PWideChar(@FReadBuffer[0]), CP_ACP, TRUE);
            end;
        else
            Result := InternalReadLn;
            S := RawByteString(PAnsiChar(@FReadBuffer[0]));
        {$IFDEF COMPILER12_UP}
            if (S <> '') and (FCodePage <> CP_ACP) then
                PWord(INT_PTR(S) - 12)^ := FCodePage;
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.ReadToEnd(var S: RawByteString);
var
    Buf : TBytes;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                SetLength(Buf, (Size - Position) + 2);
                Read(Buf[0], Length(Buf) - 2);
                Buf[Length(Buf) - 1] := 0;
                Buf[Length(Buf) - 2] := 0;
                S := UnicodeToAnsi(PWideChar(@Buf[0]), CP_ACP, TRUE);
            end;
        CP_UTF16Be :
            begin
                SetLength(Buf, (Size - Position) + 2);
                Read(Buf[0], Length(Buf) - 2);
                Buf[Length(Buf) - 1] := 0;
                Buf[Length(Buf) - 2] := 0;
                IcsSwap16Buf(@Buf[0], @Buf[0], (Length(Buf) - 2) div 2);
                S := UnicodeToAnsi(PWideChar(@Buf[0]), CP_ACP, TRUE);
            end;
        else
            SetLength(S, Size - Position);
            Read(PAnsiChar(S)^, Length(S));
        {$IFDEF COMPILER12_UP}
            if (S <> '') and (FCodePage <> CP_ACP) then
                PWord(INT_PTR(S) - 12)^ := FCodePage;
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.ReadToEnd(var S: UnicodeString);
var
    Buf : TBytes;
begin
    case FCodePage of
        CP_UTF16 :
            begin
                SetLength(S, (Size - Position) div 2);
                Read(PWideChar(S)^, Length(S) * 2);
            end;
        CP_UTF16Be :
            begin
                SetLength(S, (Size - Position) div 2);
                Read(PWideChar(S)^, Length(S) * 2);
                IcsSwap16Buf(Pointer(S), Pointer(S), Length(S));
            end;
        else
            SetLength(Buf, (Size - Position) + 1);
            Read(Buf[0], Length(Buf) - 1);
            Buf[Length(Buf) - 1] := 0;
            S := AnsiToUnicode(PAnsiChar(@Buf[0]), FCodePage);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.EnsureReadBufferW(Size: Integer; var P: PWideChar);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DefaultBufferSize);
        SetLength(FReadBuffer, FReadBufSize);
        P := @FReadBuffer[0];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamReader.EnsureReadBufferA(Size: Integer; var P: PAnsiChar);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DefaultBufferSize);
        SetLength(FReadBuffer, FReadBufSize);
        P := @FReadBuffer[0];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsStreamWriter }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(Stream: TStream;
  BufferSize: Integer = DEFAULT_BUFSIZE; OwnsStream: Boolean = FALSE);
begin
    Create(Stream, TRUE, FALSE, CP_ACP, FALSE, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(Stream: TStream; Append: Boolean = TRUE;
  DetectBOM: Boolean = FALSE; CodePage: LongWord = CP_ACP;
  OwnsStream: Boolean = FALSE; BufferSize: Integer = DEFAULT_BUFSIZE);
begin
    FCodePage  := CodePage;
    inherited Create(Stream, BufferSize, OwnsStream);
    if DetectBom then
        FCodePage := GetCodePageFromBOM;
    if Append then Seek(0, soEnd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(const FileName: String; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
    FCodePage := CP_ACP;
    inherited Create(FileName, Mode, Rights, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(const FileName: String;
  Append: Boolean = TRUE; DetectBOM: Boolean = TRUE;
  CodePage: LongWord = CP_ACP; BufferSize: Integer = DEFAULT_BUFSIZE);
var
    Mode : Word;
begin
    FCodePage := CodePage;
    if Append and FileExists(FileName) then begin
        Mode := fmOpenReadWrite or fmShareDenyWrite;
        inherited Create(FileName, Mode, FileAccessRights, BufferSize);
        if DetectBom then
            FCodePage := GetCodePageFromBOM;
        Seek(0, soEnd);
    end
    else begin
        Mode := fmCreate;
        inherited Create(FileName, Mode, FileAccessRights, BufferSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(const FileName: WideString; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
    FCodePage := CP_ACP;
    inherited Create(FileName, Mode, Rights, BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsStreamWriter.Create(const FileName: WideString;
  Append: Boolean = TRUE; DetectBOM: Boolean = TRUE;
  CodePage: LongWord = CP_ACP; BufferSize: Integer = DEFAULT_BUFSIZE);
var
    Mode : Word;
begin
    FCodePage := CodePage;
    if Append and FileExists(FileName) then begin
        Mode := fmOpenReadWrite or fmShareDenyWrite;
        inherited Create(FileName, Mode, FileAccessRights, BufferSize);
        if DetectBom then
            FCodePage := GetCodePageFromBOM;
        Seek(0, soEnd);
    end
    else begin
        Mode := fmCreate;
        inherited Create(FileName, Mode, FileAccessRights, BufferSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.Init;
begin
    inherited;
    FReadBufSize := DefaultBufferSize;
    SetLength(FReadBuffer, FReadBufSize);
    FWriteBufSize := DefaultBufferSize;
    SetLength(FWriteBuffer, FWriteBufSize);
    LineBreakStyle := ilbsCRLF;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamWriter.GetCodePageFromBOM: LongWord;
var
    OldPos: Int64;
    A : array [0..2] of Byte;
    BomLen : Integer;
begin
    FillChar(a, 3, #0);
    OldPos := Position;
    Seek(0, sofromBeginning);
    Read(A, 3);
    if (A[0] = $FF) and (A[1] = $FE) then begin
        Result := CP_UTF16;
        BomLen := 2;
    end
    else if (A[0] = $FE) and (A[1] = $FF) then begin
        Result := CP_UTF16Be;
        BomLen := 2;
    end
    else if (A[0] = $EF) and (A[1] = $BB) and (A[2] = $BF) then begin
        Result := CP_UTF8;
        BomLen := 3;
    end
    else begin
        Result := CP_ACP;
        BomLen := 0;
    end;
    if OldPos > BomLen then
        Position := OldPos
    else
        Position := BomLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamWriter.GetBomFromCodePage(ACodePage: LongWord) : TBytes;
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
        else
            SetLength(Result, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsStreamWriter.DetectLineBreakStyle: TIcsLineBreakStyle;
var
    OldPos    : Int64;
    ChA       : AnsiChar;
    ChW       : WideChar;
    CodePage  : LongWord;
begin
    Result := ilbsCRLF;
    OldPos := Position;
    CodePage := GetCodePageFromBOM;
    try
    case CodePage of
        CP_UTF16, CP_UTF16Be :
        begin
            Seek(2, soBeginning);
            while Read(ChW, SizeOf(ChW)) = SizeOf(ChW) do
            begin
                if CodePage = CP_UTF16Be then
                    ChW := WideChar((Word(ChW) shl 8) or (Word(ChW) shr 8));
                case ChW of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
        end;
        else // case
            if CodePage = CP_UTF8 then
                Seek(3, soBeginning);

            while Read(ChA, SizeOf(ChA)) = SizeOf(ChA) do
            begin
                case ChA of
                    #10 :
                        begin
                            if Result = ilbsCRLF then begin
                                Result := ilbsLF;
                                Exit;
                            end
                            else if Result = ilbsCR then begin
                                Result := ilbsCRLF;
                                Exit;
                            end;
                        end;
                    #13 :
                        begin
                            Result := ilbsCR;
                        end;
                  else
                      if Result = ilbsCR then
                          Exit;
                      Result := ilbsCRLF;
                end;
            end;
    end;
    finally
        Position := OldPos;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
procedure TXStreamWriter.Write(Value: Boolean);
begin
    Write(BoolToStr(Value, True));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TXStreamWriter.Write(Value: WideChar);
begin
    Write(UnicodeString(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TXStreamWriter.Write(Value: AnsiChar);
begin
    Write(RawByteString(Value));
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.WriteBOM;
var
    Bom : TBytes;
begin
    Bom := GetBomFromCodePage(FCodePage);
    if Length(Bom) > 0 then begin
        Seek(0, soBeginning);
        Write(Bom[0], Length(Bom));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.EnsureWriteBuffer(Size: Integer);
begin
    if Size > FWriteBufSize then begin
        while Size > FWriteBufSize do
            Inc(FWriteBufSize, DefaultBufferSize);
        SetLength(FWriteBuffer, FWriteBufSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.EnsureReadBuffer(Size: Integer);
begin
    if Size > FReadBufSize then begin
        while Size > FReadBufSize do
            Inc(FReadBufSize, DefaultBufferSize);
        SetLength(FReadBuffer, FReadBufSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.Write(const S: UnicodeString);
var
    Len   : Integer;
    SLen  : Integer;
begin
    SLen := Length(S);
    if SLen = 0 then Exit;
    case FCodePage of
        CP_UTF16   :
            begin
                WriteBuffer(Pointer(S)^, SLen * 2);
            end;
        CP_UTF16Be :
            begin
                EnsureWriteBuffer((SLen + 1) * 2);
                Move(Pointer(S)^, FWriteBuffer[0], SLen * 2);
                PWideChar(FWriteBuffer)[SLen] := #0;
                IcsSwap16Buf(@FWriteBuffer[0], @FWriteBuffer[0], SLen);
                WriteBuffer(FWriteBuffer[0], SLen * 2);
            end;
        else
            Len := IcsWcToMb{WideCharToMultiByte}(FCodePage, 0, Pointer(S), SLen, nil, 0,
                                       nil, nil);
            EnsureWriteBuffer(Len);
            Len := IcsWcToMb{WideCharToMultiByte}(FCodePage, 0, Pointer(S), SLen,
                                       @FWriteBuffer[0], Len, nil, nil);
            WriteBuffer(FWriteBuffer[0], Len);
    end; //case
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.Write(const S: RawByteString;
  SrcCodePage: LongWord = CP_ACP);
var
    Len   : Integer;
    Len1  : Integer;
    SLen  : Integer;
begin
    SLen := Length(S);
    if SLen = 0 then Exit;
    case FCodePage of
        CP_UTF8,
        CP_UTF7     :
            begin
                if SrcCodePage <> FCodePage then
                begin
                    Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S),
                                               SLen, nil, 0);
                    EnsureReadBuffer(Len);
                    Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S), SLen,
                                               @FReadBuffer[0], Len);

                    Len1 := IcsWcToMb{WideCharToMultibyte}(FCodePage, 0, @FReadBuffer[0],
                                                Len, nil, 0, nil, nil);
                    EnsureWriteBuffer(Len1);
                    Len1 := IcsWcToMb{WideCharToMultibyte}(FCodePage, 0, @FReadBuffer[0],
                                                Len, @FWriteBuffer[0], Len1,
                                                nil, nil);
                    WriteBuffer(FWriteBuffer[0], Len1);
                end
                else
                    WriteBuffer(Pointer(S)^, SLen);
            end;
        CP_UTF16   :
            begin
                Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S), SLen,
                                           nil, 0);
                EnsureWriteBuffer(Len * 2);
                Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S), SLen,
                                           @FWriteBuffer[0], Len);
                WriteBuffer(FWriteBuffer[0], Len * 2);
            end;
        CP_UTF16Be :
            begin
                Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S), SLen,
                                           nil, 0);
                EnsureWriteBuffer((Len + 1) * 2);
                Len := IcsMbToWc{MultibyteToWideChar}(SrcCodePage, 0, Pointer(S), SLen,
                                           @FWriteBuffer[0], Len);
                PWideChar(FWriteBuffer)[Len] := #0;
                IcsSwap16Buf(@FWriteBuffer[0], @FWriteBuffer[0], Len);
                WriteBuffer(FWriteBuffer[0], Len * 2);
            end;
        else
            WriteBuffer(Pointer(S)^, SLen);
    end; //case
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.WriteLine(const S: RawByteString;
  SrcCodePage: LongWord = CP_ACP);
begin
    Write(S, SrcCodePage);
    Write(FLineBreak, SrcCodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.WriteLine(const S: UnicodeString);
begin
    Write(S);
    Write(UnicodeString(FLineBreak));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsStreamWriter.SetLineBreakStyle(Value: TIcsLineBreakStyle);
begin
    FLineBreakStyle := Value;
    case FLineBreakStyle of
        ilbsCRLF : FLineBreak := #13#10;
        ilbsLF   : FLineBreak := #10;
    else
        FLineBreak := #13;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsBinaryReader }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryReader.Close;
begin
    if FOwnsStream then
        FreeAndNil(FStream)
    else
        FStream := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryReader.Create;
begin
    Create(nil, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryReader.Create(Stream: TStream);
begin
    Create(Stream, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryReader.Create(Stream: TStream; AOwnsStream: Boolean);
begin
    inherited Create;
    FStream := Stream;
    FOwnsStream := AOwnsStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryReader.Create(const Filename: string);
begin
    Create(TIcsBufferedStream.Create(FileName, fmOpenRead or fmShareDenyWrite,
                                     FileAccessRights, DEFAULT_BUFSIZE),
           True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBinaryReader.Destroy;
begin
    Close;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.GetStream: TStream;
begin
    Result := FStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryReader.SetStream(Value: TStream; AOwnsStream: Boolean);
begin
    Close;
    FStream := Value;
    FOwnsStream := AOwnsStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.Read(const Buffer: TBytes; Index,
  Count: Integer): Integer;
begin
    Assert((Index >= 0) and (Count >= 0) and (Length(Buffer) - Index < Count));
    Result := FStream.Read(Buffer[Index], Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadDouble: Double;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadInt64: Int64;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadInteger: Integer;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadLongWord: Longword;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadShortInt: ShortInt;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadSingle: Single;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadUTF8String: UTF8String;
var
    Len: Integer;
begin
    FStream.ReadBuffer(Len, SizeOf(Integer));
    SetLength(Result, Len);
    FStream.ReadBuffer(Pointer(Result)^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadAnsiString: AnsiString;
var
    Len: Integer;
begin
    FStream.ReadBuffer(Len, SizeOf(Integer));
    SetLength(Result, Len);
    FStream.ReadBuffer(Pointer(Result)^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadUnicodeString: UnicodeString;
var
    Len: Integer;
begin
    FStream.ReadBuffer(Len, SizeOf(Integer));
    SetLength(Result, Len div 2);
    FStream.ReadBuffer(Pointer(Result)^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadWord: Word;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadBoolean: Boolean;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadByte: Byte;
begin
    FStream.ReadBuffer(Result, SizeOf(Result));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryReader.ReadBytes(Count: Integer): TBytes;
begin
    Assert(Count >= 0);
    SetLength(Result, Count);
    if Count > 0 then
        FStream.ReadBuffer(Result[0], Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsBinaryWriter }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Close;
begin
    if FOwnsStream then
        FreeAndNil(FStream)
    else
        FStream := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryWriter.Create(Stream: TStream);
begin
    Create(Stream, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryWriter.Create(Stream: TStream; AOwnsStream: Boolean);
begin
    inherited Create;
    FStream := Stream;
    FOwnsStream := AOwnsStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryWriter.Create(const Filename: string; Append: Boolean);
begin
    if (not FileExists(Filename)) or (not Append) then
        FStream := TIcsBufferedStream.Create(Filename, fmCreate,
                                             DEFAULT_BUFSIZE, FileAccessRights)
    else begin
        FStream := TIcsBufferedStream.Create(Filename, fmOpenWrite,
                                             DEFAULT_BUFSIZE, FileAccessRights);
        FStream.Seek(0, soEnd);
    end;
    Create(FStream, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBinaryWriter.Create;
begin
    Create(nil, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBinaryWriter.Destroy;
begin
    Close;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBinaryWriter.GetStream: TStream;
begin
    Result := FStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.SetStream(Value: TStream; AOwnsStream: Boolean);
begin
    Close;
    FStream := Value;
    FOwnsStream := AOwnsStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Boolean);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Byte);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Int64);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: LongWord);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Single);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(const Buffer: TBytes; Index, Count: Integer);
begin
    Assert((Index >= 0) and (Count >= 0) and (Length(Buffer) - Index < Count));
    FStream.WriteBuffer(Buffer[Index], Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(const Value: UnicodeString);
var
    Len: Integer;
begin
    Len := Length(Value) * 2;
    FStream.Write(Len, SizeOf(Len));
    FStream.WriteBuffer(Pointer(Value)^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(const Value: RawByteString);
var
    Len: Integer;
begin
    Len := Length(Value);
    FStream.Write(Len, SizeOf(Len));
    FStream.WriteBuffer(Pointer(Value)^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Double);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(const Value: TBytes);
begin
    FStream.WriteBuffer(Value[0], Length(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Integer);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: Word);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBinaryWriter.Write(Value: ShortInt);
begin
    FStream.Write(Value, SizeOf(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
