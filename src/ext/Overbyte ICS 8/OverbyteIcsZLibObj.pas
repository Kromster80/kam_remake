{
  file   : IcsZLibObj.pas
  date   : 6 Dec 2005
  ICS version: Angus Robertson

  Subject
  -------
  A Borland Delphi unit to interface zlib.dll functions

  Acknowledgements
  ----------------
  Thanks to Jean-loup Gailly and Mark Adler for zLib library
         and Gilles Vollant for zlibwapi.dll

  zLib library version 1.2.5
  Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler
  Informations at http://www.zlib.net (or http://www.zlib.org)


  Adaptation
  ----------
  Xavier Le Bris
  xavier.lebris@free.fr   (english or french)

  ICS Updates
  -----------

  27 Nov 2005 by Angus Robertson, Magenta Systems
  delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  Added an alterative Zlib implementation using the ZLIB OBJ files linked into
  the program to avoid needing an external DLL, in IcsZLibObj
  Renamed the units for use with ICS from http://www.overbyte.be
  This OBJ files are from zlibpas by Gabriel Corneanu (gabrielcorneanu(AT)yahoo.com)

  02 May 2008 by A.Garrels <arno.garrels@gmx.de>
              Prepared code for Unicode, changed most types from String
              and PChar to AnsiString and PAnsiChar.

  Aug 05, 2008 F. Piette added some casts for unicode support

  Sep 10, 2010 Angus and Arno updated ZLIB to 1.2.5, subdirectory now lowercase

  Apr 15, 2011 Arno prepared for 64-bit.
  Aug 13, 2011 Arno fixed record allignment.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the author be held liable for any damages arising from the use of this software.

  This code is free under the same terms and conditions as 'zlib.'

}

unit OverbyteIcsZLibObj;

interface

{$IFDEF MSWINDOWS}

{$ALIGN ON}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\OverbyteIcsZlib.inc}
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

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF};

{xlb constants and variables}
const
   Z_DLL_NOT_FOUND               = -97;
   Z_UNKNOWN_COMPRESSION_VERSION = -98;
   Z_CHECK_PROBLEM               = -99;

var
   zlibDllLoaded     : boolean;
   zlibProblemAlert  : boolean;
   zlibProblemString : AnsiString;
   zlibRaiseError    : boolean;
   ZLibWinapi        : boolean;

{zLib constants}
const
   ZLIB_VERSION    = '1.2.5';

   { Allowed flush values; see deflate() below for details }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
   Z_BLOCK         = 5;

   Z_OK            = 0;
   Z_STREAM_END    = 1;
   Z_NEED_DICT     = 2;
   Z_ERRNO         = -1;
   Z_STREAM_ERROR  = -2;
   Z_DATA_ERROR    = -3;
   Z_MEM_ERROR     = -4;
   Z_BUF_ERROR     = -5;
   Z_VERSION_ERROR = -6;

   { compression levels }
   Z_NO_COMPRESSION         = 0;
   Z_BEST_SPEED             = 1;
   Z_BEST_COMPRESSION       = 9;
   Z_DEFAULT_COMPRESSION    = -1;

   { compression strategy; see deflateInit2() below for details }
   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_RLE                 = 3;
   Z_DEFAULT_STRATEGY    = 0;
   Z_MAX_STRATEGY        = 3;

   { Possible values of the data_type field }
   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;

   { The deflate compression method (the only one supported in this version) }
   Z_DEFLATED = 8;

   Z_NULL  = nil;  { for initializing zalloc, zfree, opaque }

{other constants}
   Z_BUFSIZE = 16384;

   MAX_WBITS     = 15; { 32K LZ77 window }
   MAX_MEM_LEVEL = 9;
   DEF_MEM_LEVEL = 8;  { if MAX_MEM_LEVEL > 8 }

{zLib types}
type
   tZLibCompressionStrategy = (ctStandard, ctFiltered, ctHuffmanOnly, ctRle);
   tZLibFlag = (zfuInt, zfuLong, zfvoidpf, zfz_off_t, zfdebug, zfasm, zfwinapi, zfbuildfixed, zfdynamic_crc_table, zfno_gzcompress, zfno_gzip, zfpkzip_bug, zffastest);

   alloc_func = function(opaque: Pointer; items, size: Integer): Pointer; cdecl;
   free_func  = procedure(opaque, address: Pointer); cdecl;

   in_func    = function(opaque: Pointer; var buf: PByte): Integer; cdecl;
   out_func   = function(opaque: Pointer; buf: PByte; size: Integer): Integer; cdecl;

   internal_state  = record end;
   pinternal_state = ^internal_state;

   z_streamp = ^z_stream;
   z_stream = record
      next_in     : pointer;           // next input byte
      avail_in    : longint;           // number of bytes available at next_in
      total_in    : longint;           // total nb of input bytes read so far

      next_out    : pointer;           // next output byte should be put there
      avail_out   : longint;           // remaining free space at next_out
      total_out   : longint;           // total nb of bytes output so far

      msg         : PAnsiChar;         // last error message, NULL if no error
      state       : pinternal_state;   // not visible by applications

      zalloc      : alloc_func;        // used to allocate the internal state
      zfree       : free_func;         // used to free the internal state
      AppData     : pointer;           // private data object passed to zalloc and zfree

      data_type   : longint;           // best guess about the data type: ascii or binary
      adler       : longword;          // adler32 value of the uncompressed data
      reserved    : longint;           // reserved for future use
   end;

   pZStreamRec = ^tZStreamRec;
   tZStreamRec = z_stream;

   gz_stream = record
      stream      : z_stream;
      z_err       : longint;           // error code for last stream operation
      z_eof       : longint;           // set if end of input file
      gzfile      : pointer;           // .gz file
      inbuf       : pointer;           // input buffer
      outbuf      : pointer;           // output buffer
      crc         : longword;          // crc32 of uncompressed data
      msg         : PAnsiChar;         // error message
      path        : PAnsiChar;         // path name for debugging only
      transparent : longint;           // 1 if input file is not a .gz file
      mode        : AnsiChar;              // 'w' or 'r'
      start       : longint;           // start of compressed data in file (header skipped)
      into        : longint;           // bytes into deflate or inflate
      outof       : longint;           // bytes out of deflate or inflate
      back        : longint;           // one character push-back
      last        : longint;           // true if push-back is last character
   end;

   pGzStreamRec = ^tGzStreamRec;
   tGzStreamRec = gz_stream;
   tGzFile      = pGzStreamRec;

(*
  gzip header information passed to and from zlib routines.  See RFC 1952
  for more details on the meanings of these fields.
*)
  gz_headerp = ^gz_header;
  gz_header = record
    text       : integer;   //* true if compressed data believed to be text */
    time       : Cardinal;  //* modification time */
    xflags     : integer;   //* extra flags (not used when writing a gzip file) */
    os         : integer;   //* operating system */
    extra      : PByte;     //* pointer to extra field or Z_NULL if none */
    extra_len  : Cardinal;  //* extra field length (valid if extra != Z_NULL) */
    extra_max  : Cardinal;  //* space at extra (only when reading header) */
    name       : PAnsiChar;     //* pointer to zero-terminated file name or Z_NULL */
    name_max   : Cardinal;  //* space at name (only when reading header) */
    comment    : PAnsiChar;     //* pointer to zero-terminated comment or Z_NULL */
    comm_max   : Cardinal;  //* space at comment (only when reading header) */
    hcrc       : integer;   //* true if there was or will be a header crc */
    done       : integer;   //* true when done reading gzip header (not used when writing a gzip file) */
  end;

  TZStreamType = (
    zsZLib,  //standard zlib stream
    zsGZip,  //gzip stream
    zsRaw);  //raw stream (without any header)


{zLib functions}
function zlibVersionDll                : AnsiString;
function zlibCompileFlags              : longword;

(* basic functions *)
function zlibVersion: PAnsiChar;
function deflateInit(var strm: z_stream; level: Integer): Integer;
function deflate(var strm: z_stream; flush: Integer): Integer;
function deflateEnd(var strm: z_stream): Integer;
function inflateInit(var strm: z_stream): Integer;
function inflate(var strm: z_stream; flush: Integer): Integer;
function inflateEnd(var strm: z_stream): Integer;

(* advanced functions *)
function deflateInit2(var strm: z_stream; level, method, windowBits,
                      memLevel, strategy: Integer): Integer;
function deflateSetDictionary(var strm: z_stream; const dictionary: PAnsiChar;
                              dictLength: Integer): Integer;
function deflateCopy(var dest, source: z_stream): Integer;
function deflateReset(var strm: z_stream): Integer;
function deflateParams(var strm: z_stream; level, strategy: Integer): Integer;
function deflateBound(var strm: z_stream; sourceLen: LongInt): LongInt;
function deflatePrime(var strm: z_stream; bits, value: Integer): Integer;
function inflateInit2(var strm: z_stream; windowBits: Integer): Integer;
function inflateSetDictionary(var strm: z_stream; const dictionary: PAnsiChar;
                              dictLength: Integer): Integer;
function inflateSync(var strm: z_stream): Integer;
function inflateCopy(var dest, source: z_stream): Integer;
function inflateReset(var strm: z_stream): Integer;
function inflateBackInit(var strm: z_stream;
                         windowBits: Integer; window: PAnsiChar): Integer;
function inflateBack(var strm: z_stream; in_fn: in_func; in_desc: Pointer;
                     out_fn: out_func; out_desc: Pointer): Integer;
function inflateBackEnd(var strm: z_stream): Integer;

function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;
function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;

(* utility functions *)
function compress(dest: PAnsiChar; var destLen: LongInt;
                  const source: PAnsiChar; sourceLen: LongInt): Integer;
function compress2(dest: PAnsiChar; var destLen: LongInt;
                  const source: PAnsiChar; sourceLen: LongInt;
                  level: Integer): Integer;
function compressBound(sourceLen: LongInt): LongInt;
function uncompress(dest: PAnsiChar; var destLen: LongInt;
                    const source: PAnsiChar; sourceLen: LongInt): Integer;

(* checksum functions *)
function adler32(adler: LongInt; const buf: PAnsiChar; len: Integer): LongInt;
function crc32(crc: LongInt; const buf: PAnsiChar; len: Integer): LongInt;

(* various hacks, don't look :) *)
function deflateInit_(var strm: z_stream; level: Integer;
                      const version: PAnsiChar; stream_size: Integer): Integer;
function inflateInit_(var strm: z_stream; const version: PAnsiChar;
                      stream_size: Integer): Integer;
function deflateInit2_(var strm: z_stream;
                       level, method, windowBits, memLevel, strategy: Integer;
                       const version: PAnsiChar; stream_size: Integer): Integer;
function inflateInit2_(var strm: z_stream; windowBits: Integer;
                       const version: PAnsiChar; stream_size: Integer): Integer;
function inflateBackInit_(var strm: z_stream;
                          windowBits: Integer; window: PAnsiChar;
                          const version: PAnsiChar; stream_size: Integer): Integer;

function deflateSetHeader(var strm: z_stream; var head: gz_header): integer;
function inflateGetHeader(var strm: z_stream; var head: gz_header): integer;

function  zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
procedure zlibFreeMem(AppData, Block: Pointer); cdecl;


{added functions}
function  ZLibCheck                    (Code : longint) : longint;
procedure ZLibError;
function  ZLibFlagsString              (ZLibFlag : tZLibFlag) : AnsiString;
procedure ZLibSetDeflateStateItem      (strm : TZStreamRec; Index : integer; Value : integer);
function  ZLibGetDeflateStateItem      (strm : TZStreamRec; Index : integer) : integer;

var
    Z_DS_MaxChainLen : integer;
    Z_DS_LazyMatch   : integer;
    Z_DS_GoodMatch   : integer;
    Z_DS_NiceMatch   : integer;

{==============================================================================}

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

uses {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF};

const
   {return code messages}

   ZLibErrMsg  : array[-6..2] of PAnsiChar = (
     'incompatible version', // Z_VERSION_ERROR  (-6)
     'buffer error',         // Z_BUF_ERROR      (-5)
     'insufficient memory',  // Z_MEM_ERROR      (-4)
     'data error',           // Z_DATA_ERROR     (-3)
     'stream error',         // Z_STREAM_ERROR   (-2)
     'file error',           // Z_ERRNO          (-1)
     '',                     // Z_OK             (0)
     'stream end',           // Z_STREAM_END     (1)
     'need dictionary'       // Z_NEED_DICT      (2)
   );

   SZLibInvalid = 'Invalid ZStream operation!';

type
   EZLibCheckError = class(Exception);

// currently not importing gzio, minigzip, gzclose, gzread, gzwrite
{$IFDEF WIN64}
    {$L zobj125/win64/adler32.obj}
    {$L zobj125/win64/compress.obj}
    {$L zobj125/win64/crc32.obj}
    {$L zobj125/win64/deflate.obj}
    { L zobj125/win64/gzclose.obj}
    { L zobj125/win64/gzread.obj}
    { L zobj125/win64/gzio.obj}
    { L zobj125/win64/gzwrite.obj}
    {$L zobj125/win64/infback.obj}
    {$L zobj125/win64/inffast.obj}
    {$L zobj125/win64/inflate.obj}
    {$L zobj125/win64/inftrees.obj}
    { L zobj125/win64/minigzip.obj}
    {$L zobj125/win64/trees.obj}
    {$L zobj125/win64/uncompr.obj}
    {$L zobj125/win64/zutil.obj}
{$ENDIF WIN64}
{$IFDEF WIN32}
    {$L zobj125/adler32.obj}
    {$L zobj125/compress.obj}
    {$L zobj125/crc32.obj}
    {$L zobj125/deflate.obj}
    { L zobj125/gzclose.obj}
    { L zobj125/gzread.obj}
    { L zobj125/gzio.obj}
    { L zobj125/gzwrite.obj}
    {$L zobj125/infback.obj}
    {$L zobj125/inffast.obj}
    {$L zobj125/inflate.obj}
    {$L zobj125/inftrees.obj}
    { L zobj125/minigzip.obj}
    {$L zobj125/trees.obj}
    {$L zobj125/uncompr.obj}
    {$L zobj125/zutil.obj}
{$ENDIF WIN32}

function adler32; external;
function compress; external;
function compress2; external;
function compressBound; external;
function crc32; external;
function deflate; external;
function deflateBound; external;
function deflateCopy; external;
function deflateEnd; external;
function deflateInit_; external;
function deflateInit2_; external;
function deflateParams; external;
function deflatePrime; external;
function deflateReset; external;
function deflateSetDictionary; external;
function inflate; external;
function inflateBack; external;
function inflateBackEnd; external;
function inflateBackInit_; external;
function inflateCopy; external;
function inflateEnd; external;
function inflateInit_; external;
function inflateInit2_; external;
function inflateReset; external;
function inflateSetDictionary; external;
function inflateSync; external;
function uncompress; external;
function zlibCompileFlags; external;
function zlibVersion; external;
function deflateSetHeader; external;
function inflateGetHeader; external;
{
Note: use of these gzio functions needs more C runtime declares here
function gzOpen; external;
function gzSetParams; external;
function gzRead; external;
function gzWrite; external;
function gzClose; external;  }

{==============================================================================}

function ZLibCheck(Code : longint) : longint;
begin
     Result := Code;
     if (Code < 0) and (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          if (Code < Z_VERSION_ERROR) then
          begin
               case Code of
                  Z_DLL_NOT_FOUND               : zlibProblemString := 'Dll not found';
                  Z_UNKNOWN_COMPRESSION_VERSION : zlibProblemString := 'Unknown compression stream version';
                  Z_CHECK_PROBLEM               : zlibProblemString := 'Check problem';
                                           else   zlibProblemString := 'Error n°' + AnsiString(IntToStr(-Code));
               end;
          end else
               zlibProblemString := ZLibErrMsg[Code];

          if zlibRaiseError then
              raise EZLibCheckError.Create(String(zlibProblemString));
     end;
end;
{==============================================================================}

procedure ZLibError;
begin
     if (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          zlibProblemString := SZLibInvalid;
          if zlibRaiseError then raise EZLibCheckError.Create(SZLibInvalid);
     end;
end;
{==============================================================================}

function zlibVersionDll : AnsiString;
begin
    Result := zlibVersion ;
end;

{==============================================================================}

function ZLibFlagsString(ZLibFlag : tZLibFlag) : AnsiString;
var  Flags : longword;

     function FlagSize(L : longword) : AnsiString;
     var  N : longword;
     begin
          N := (Flags shr L) and $0003;
          case N of
             0 : Result := '16';            {uInt}
             1 : Result := '32';            {uLong}
             2 : Result := '64';            {voidpf}
             3 : Result := '0';             {z_off_t}
          end;
     end;

     function FlagBit(L : longword) : boolean;
     begin
          Result := (((Flags shr L) and $0001) = 1);
     end;
begin
     Result := '';
     Flags := zlibCompileFlags;
     case  ZLibFlag of
        zfuInt               : Result := 'uInt : ' + FlagSize(0);
        zfuLong              : Result := 'uLong : ' + FlagSize(2);
        zfvoidpf             : Result := 'voidpf : ' + FlagSize(4);
        zfz_off_t            : Result := 'z_off_t : ' + FlagSize(6);
        zfdebug              : if FlagBit(8)  then Result := 'debug';
        zfasm                : if FlagBit(9)  then Result := 'asm' else Result := 'noasm';
        zfwinapi             : if FlagBit(10) then Result := 'stdcall' else Result := 'cdecl';
        zfbuildfixed         : if FlagBit(12) then Result := 'buildfixed';
        zfdynamic_crc_table  : if FlagBit(13) then Result := 'dynamic_crc_table';
        zfno_gzcompress      : if FlagBit(16) then Result := 'no_gzcompress';
        zfno_gzip            : if FlagBit(17) then Result := 'no_gzip';
        zfpkzip_bug          : if FlagBit(20) then Result := 'pkzip_bug';
        zffastest            : if FlagBit(21) then Result := 'fastest';
     end;
end;
{==============================================================================}
{ for XLB purposes only : access to internal zlib variables : only for versions 1.1.4 and 1.2.x; must be updated in future versions }

const
     Z_DS_MaxItemsMax = 34;
var
     Z_DS_MaxItems    : integer;

type pDeflateState = ^tDeflateState;
     tDeflateState = array[0..Z_DS_MaxItemsMax] of longint;
{——————————————————————————————————————————————————————————————————————————————}
(*   dans match.s
     WSize 36 WMask 44 Window 48 Prev 56 MatchLen 88 PrevMatch 92 StrStart 100 MatchStart 104 Lookahead 108 PrevLen 112
     MaxChainLen 116 GoodMatch 132 NiceMatch 136 *)

procedure ZLibDeflateStateInit;
begin
     Z_DS_MaxItems    := 0;
     Z_DS_MaxChainLen := 0;
     Z_DS_LazyMatch   := 0;
     Z_DS_GoodMatch   := 0;
     Z_DS_NiceMatch   := 0;
     Z_DS_MaxItems    := Z_DS_MaxItemsMax;  {34}
     Z_DS_MaxChainLen := 29; {116 / 4}
     Z_DS_LazyMatch   := 30;
     Z_DS_GoodMatch   := 33; {132 / 4}
     Z_DS_NiceMatch   := 34; {136 / 4}
end;
{——————————————————————————————————————————————————————————————————————————————}

procedure ZLibSetDeflateStateItem(strm : TZStreamRec; Index : integer; Value : integer);
var  PtrDS : pDeflateState;
begin
     if (Z_DS_MaxItems > 0) then
     begin
          PtrDS := pDeflateState(strm.state);
          if (PtrDS <> nil) and (Index in [0..Z_DS_MaxItems]) then PtrDS^[Index] := Value;
     end;
end;
{——————————————————————————————————————————————————————————————————————————————}

function ZLibGetDeflateStateItem(strm : TZStreamRec; Index : integer) : integer;
var  PtrDS : pDeflateState;
begin
     Result := 0;
     if (Z_DS_MaxItems > 0) then
     begin
          PtrDS := pDeflateState(strm.state);
          if (PtrDS <> nil) and (Index in [0..Z_DS_MaxItems]) then Result := PtrDS^[Index];
     end;
end;

function deflateInit(var strm: z_stream; level: Integer): Integer;
begin
//  if not Assigned(strm.zalloc) then strm.zalloc := zlibAllocMem;
//  if not Assigned(strm.zfree)  then strm.zfree  := zlibFreeMem;
  Result := deflateInit_(strm, level, ZLIB_VERSION, sizeof(z_stream));
end;

function deflateInit2(var strm: z_stream; level, method, windowBits, memLevel,
                      strategy: Integer): Integer;
begin
//  if not Assigned(strm.zalloc) then strm.zalloc := zlibAllocMem;
//  if not Assigned(strm.zfree)  then strm.zfree  := zlibFreeMem;
  Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy,
                          ZLIB_VERSION, sizeof(z_stream));
end;

const
  WBits : array[TZStreamType] of integer = (MAX_WBITS, MAX_WBITS + 16, -MAX_WBITS);

function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;
begin
  Result := deflateInit2(strm, level, Z_DEFLATED, WBits[streamtype],
    MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY);
end;

function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;
begin
  Result := inflateInit2(strm, WBits[streamtype]);
end;

function inflateInit(var strm: z_stream): Integer;
begin
//  if not Assigned(strm.zalloc) then strm.zalloc := zlibAllocMem;
//  if not Assigned(strm.zfree)  then strm.zfree  := zlibFreeMem;
  Result := inflateInit_(strm, ZLIB_VERSION, sizeof(z_stream));
end;

function inflateInit2(var strm: z_stream; windowBits: Integer): Integer;
begin
//  if not Assigned(strm.zalloc) then strm.zalloc := zlibAllocMem;
//  if not Assigned(strm.zfree)  then strm.zfree  := zlibFreeMem;
  Result := inflateInit2_(strm, windowBits, ZLIB_VERSION, sizeof(z_stream));
end;

function inflateBackInit(var strm: z_stream;
                         windowBits: Integer; window: PAnsiChar): Integer;
begin
  Result := inflateBackInit_(strm, windowBits, window,
                             ZLIB_VERSION, sizeof(z_stream));
end;

{$IFDEF WIN64}
function malloc(Size: Integer): Pointer;
begin
  GetMem(Result, Size);
end;

procedure free(Block: Pointer);
begin
  FreeMem(Block);
end;

procedure memset(P: Pointer; B: Byte; count: Integer);
begin
  FillChar(P^, count, B);
end;

procedure memcpy(dest, source: Pointer; count: Integer);
begin
  Move(source^, dest^, count);
end;
{$ENDIF WIN64}

{$IFDEF WIN32}
function _malloc(Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _free(Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;
{$ENDIF WIN32}

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Items*Size);
//  Result := AllocMem(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

{==============================================================================}
{ EZLibCheckError }

initialization
// If the compiler doesn't find the obj files
// add YourIcsDir\delphi\vc32\zobj123 to the library path
   zlibProblemAlert  := false;
   zlibProblemString := '';
   zlibRaiseError    := true;
   ZLibWinapi        := false;
   ZLibDeflateStateInit;
   zlibDllLoaded     := true ;  // really OBJ files linked here

finalization
{==============================================================================}
{$ENDIF MSWINDOWS}
end.
