{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Creation:     15 December 2005
Version:      8.00
Description:  High level functions for ZLIB compression and decompression
Credit:       Based on work by Gabriel Corneanu <gabrielcorneanu(AT)yahoo.com>
              Derived from original sources by Bob Dellaca and Cosmin Truta.
              ZLIB is Copyright (C) 1995-2005 Jean-loup Gailly and Mark Adler
EMail:        francois.piette@overbyte.be      http://www.overbyte.be
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

History:
Mar 26, 2006 V6.00 F. Piette started new version 6
9 Dec 2007   V6.01 Angus added missing compression levels
                  added ZlibCompressStreamEx overload with numeric level for ease of use
                    and callback functions
                  added ZlibDecompressStreamEx with callback functions
                  added ZlibErrMess to report zlib error messages as literals
                  added TZlibProgress callback
May 02, 2008 V6.02 A.Garrels prepared code for Unicode, type-changes from String
                   and PChar to AnsiString and PAnsiChar.
Aug 05, 2008 V6.03 F. Piette reverted ZlibErrMess to from AnsiString to String.
Sep 10, 2010 V7.00 Angus and Arno updated ZLIB to 1.2.5, subdirectory now lowercase
Apr 15, 2011 V7.01 Arno prepared for 64-bit.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


pending: compress callback not correct total count


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsZlibHigh;

interface

{$R-}
{$Q-}
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsZlib.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF USE_ZLIB_OBJ}
    OverbyteIcsZLibObj;             {interface to access ZLIB C OBJ files}
{$ELSE}
    OverbyteIcsZLibDll;             {interface to access zLib1.dll} { AG V6.02 }
{$ENDIF}


const
  WindowSize = 1 shl MAX_WBITS;

type
  TZlibProg = procedure(Sender: TObject; Count: Int64; var Cancel: Boolean);  { V6.01 }

type
  PZBack = ^TZBack;
  TZBack = record
    InStream  : TStream;
    OutStream : TStream;
    InMem     : PAnsiChar; //direct memory access
    InMemSize : integer;
    ReadBuf   : array[word] of AnsiChar;
    Window    : array[0..WindowSize] of AnsiChar;
    MainObj   : TObject; // Angus
    ProgressCallback : TZlibProg; { V6.01 }
    Count     : Int64;  { V6.01 }
  end;

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

  TCompressionLevel = (clNone, clFastest, clDefault, clMax,
                       clLev0, clLev1, clLev2, clLev3, clLev4,     { V6.01 }
                       clLev5, clLev6, clLev7, clLev8, clLev9);

const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION,
     Z_NO_COMPRESSION, Z_BEST_SPEED, 2, 3, 4, 5, 6, 7, 8, Z_BEST_COMPRESSION) ;

function ZlibGetDllLoaded: boolean ;
function ZlibGetVersionDll: AnsiString ;
function ZlibCCheck(code: Integer): Integer;
function ZlibDCheck(code: Integer): Integer;
procedure ZlibDecompressStream(InStream, OutStream: TStream);
procedure ZlibDecompressStreamEx(InStream, OutStream: TStream; Sender: TObject;
                                                       ProgCallback: TZlibProg); { V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream;
        NumLevel: Integer; StreamType : TZStreamType; UseDirectOut: boolean;
                              Sender: TObject; ProgCallback: TZlibProg); overload;  { V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream; Level:
     TCompressionLevel; StreamType : TZStreamType; UseDirectOut: boolean); overload;
function ZlibCheckInitInflateStream (var strm: TZStreamRec;
                                    gzheader: gz_headerp): TZStreamType;
function Strm_in_func(BackObj: PZBack; var buf: PByte): Integer; cdecl;
function Strm_out_func(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
function DMAOfStream(AStream: TStream; out Available: integer): Pointer;
function ZlibErrMess(code: Integer): String;                        { V6.01 }


implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibGetDllLoaded: boolean ;
begin
    result := ZlibDllLoaded ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibGetVersionDll: AnsiString ;
begin
    result := zlibVersionDll ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibErrMess(code: Integer): String;              { V6.01 }
begin
    case code of
        Z_OK            : Result := 'No error';
        Z_STREAM_END    : Result := 'Stream end';
        Z_NEED_DICT     : Result := 'Need dictionary';
        Z_ERRNO         : Result := 'Errno';
        Z_STREAM_ERROR  : Result := 'Stream error';
        Z_DATA_ERROR    : Result := 'Data error';
        Z_MEM_ERROR     : Result := 'Memory error';
        Z_BUF_ERROR     : Result := 'Buffer error';
        Z_VERSION_ERROR : Result := 'Version error';
    else
        Result := 'Unknown error ' + IntToStr(code);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibCCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create(ZlibErrMess(code));  //!! angus added code
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibDCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create(ZlibErrMess(code));  //!! angus added code
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DMAOfStream(AStream: TStream; out Available: integer): Pointer;
begin
  if AStream.inheritsFrom(TCustomMemoryStream) then
    Result := TCustomMemoryStream(AStream).Memory
  else if AStream.inheritsFrom(TStringStream) then
    Result := Pointer(TStringStream(AStream).DataString)
  else
    Result := nil;
  if Result <> nil then
  begin
    //what if integer overflow?
    Available := AStream.Size - AStream.Position;
    Inc(PAnsiChar(Result), AStream.Position);
  end
  else Available := 0;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CanResizeDMAStream(AStream: TStream): boolean;
begin
  Result := AStream.inheritsFrom(TMemoryStream) or
            AStream.inheritsFrom(TStringStream);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//tries to get the stream info
//strm.next_in and available_in needs enough data!
//strm should not contain an initialized inflate

function ZlibCheckInitInflateStream (var strm: TZStreamRec;
                                    gzheader: gz_headerp): TZStreamType;
var
  InitBuf: PAnsiChar;
  InitIn : integer;

  function TryStreamType(AStreamType: TZStreamType): boolean;
  begin
    ZlibDCheck(inflateInitEx(strm, AStreamType));

    if (AStreamType = zsGZip) and (gzheader <> nil) then
                  ZlibDCheck(inflateGetHeader(strm, gzheader^));

    Result := inflate(strm, Z_BLOCK) = Z_OK;
    ZlibDCheck(inflateEnd(strm));

    if Result then exit;
    //rollback
    strm.next_in  := InitBuf;
    strm.avail_in := InitIn;
  end;

begin
  if strm.next_out = nil then
    //needed for reading, but not used
    strm.next_out := strm.next_in;

  try
    InitBuf := strm.next_in;
    InitIn  := strm.avail_in;
    for Result := zsZLib to zsGZip do
      if TryStreamType(Result) then exit;
    Result := zsRaw;
  finally

  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Strm_in_func(BackObj: PZBack; var buf: PByte): Integer; cdecl;
var
  S : TStream;
  Cancel: boolean;
begin
  S := BackObj.InStream; //help optimizations
  if BackObj.InMem <> nil then
  begin
    //direct memory access if available!
    buf := Pointer(BackObj.InMem);
    //what if integer overflow?
    Result := S.Size - S.Position;
    S.Seek(Result, soFromCurrent);
  end
  else
  begin
    buf    := @BackObj.ReadBuf;
    Result := S.Read(buf^, SizeOf(BackObj.ReadBuf));
  end;
  if Assigned(BackObj.ProgressCallback) then begin   { V6.01 tell user }
    inc (BackObj.Count, Result);    { V6.01 keep track of data read }
    Cancel := false;
    BackObj.ProgressCallback(BackObj.MainObj, BackObj.Count, Cancel);
    if Cancel then Result := 0;  { pretend end of stream }
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Strm_out_func(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
var
  Cancel: boolean;
begin
  Result := BackObj.OutStream.Write(buf^, size) - size;
  if Assigned(BackObj.ProgressCallback) then begin   { V6.01 tell user }
    inc (BackObj.Count, size);    { V6.01 keep track of data read }
    Cancel := false;
    BackObj.ProgressCallback(BackObj.MainObj, BackObj.Count, Cancel);
    if Cancel then Result := -1; { pretend write failed }
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  V6.01 }
procedure ZlibDecompressStreamEx(InStream, OutStream: TStream; Sender: TObject;
                                                       ProgCallback: TZlibProg);
var
  strm   : z_stream;
  BackObj: PZBack;
begin
  FillChar(strm, sizeof(strm), 0);
  GetMem(BackObj, SizeOf(BackObj^));
  try
    //direct memory access if possible!
    BackObj.InMem := DMAOfStream(InStream, BackObj.InMemSize);

    BackObj.InStream  := InStream;
    BackObj.OutStream := OutStream;
    BackObj.MainObj := Sender;               { V6.01 stuff for callback }
    BackObj.ProgressCallback := ProgCallback;
    BackObj.Count := 0;

    //use our own function for reading
    strm.avail_in := Strm_in_func(BackObj, PByte(strm.next_in));
    strm.next_out := @BackObj.Window;
    strm.avail_out := 0;

    ZlibCheckInitInflateStream(strm, nil);

    strm.next_out := nil;
    strm.avail_out := 0;
    ZlibDCheck(inflateBackInit(strm, MAX_WBITS, BackObj.Window));
    try
      ZlibDCheck(inflateBack(strm, @Strm_in_func, BackObj, @Strm_out_func, BackObj));
      //seek back when unused data
      InStream.Seek(-strm.avail_in, soFromCurrent);
      //now trailer can be checked
    finally
      ZlibDCheck(inflateBackEnd(strm));
    end;
  finally
    FreeMem(BackObj);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ZlibDecompressStream(InStream, OutStream: TStream);
begin
    ZlibDecompressStreamEx(InStream, OutStream, Nil, Nil);
end;
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
  TMemStreamHack = class(TMemoryStream);

function ExpandStream(AStream: TStream; const ACapacity : Int64): boolean;
begin
  Result := true;
  AStream.Size := ACapacity;
  if AStream.InheritsFrom(TMemoryStream) then
    AStream.Size := TMemStreamHack(AStream).Capacity;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream;
            NumLevel: Integer; StreamType : TZStreamType; UseDirectOut: boolean;
                                            Sender: TObject; ProgCallback: TZlibProg);
const
  //64 KB buffer
  BufSize = 65536;
var
  strm   : z_stream;
  InBuf, OutBuf : PAnsiChar;
  UseInBuf, UseOutBuf : boolean;
  LastOutCount : integer;
  Finished : boolean;
  Cancel: boolean;
  Totcount: int64;

  procedure WriteOut;
  begin
    if UseOutBuf then
    begin
      if LastOutCount > 0 then OutStream.Write(OutBuf^, LastOutCount - strm.avail_out);
      strm.avail_out := BufSize;
      strm.next_out  := OutBuf;
    end
    else
    begin
      if (strm.avail_out = 0) then ExpandStream(OutStream, OutStream.Size + BufSize);
      OutStream.Seek(LastOutCount - strm.avail_out, soFromCurrent);
      strm.next_out  := DMAOfStream(OutStream, strm.avail_out);
      //because we can't really know how much resize is increasing!
    end;
    LastOutCount := strm.avail_out;
  end;

begin
  FillChar(strm, sizeof(strm), 0);

  InBuf          := nil;
  OutBuf         := nil;
  LastOutCount   := 0;
  Totcount       := 0;

  strm.next_in   := DMAOfStream(InStream, strm.avail_in);
  UseInBuf := strm.next_in = nil;

  if UseInBuf then
    GetMem(InBuf, BufSize);

  UseOutBuf := not (UseDirectOut and CanResizeDMAStream(OutStream));
  if UseOutBuf then GetMem(OutBuf, BufSize);

  ZlibCCheck(deflateInitEx(strm, NumLevel, StreamType));          { V6.01 }
  try
    repeat
      if strm.avail_in = 0 then
      begin
        if UseInBuf then
        begin
          strm.avail_in := InStream.Read(InBuf^, BufSize);
          strm.next_in  := InBuf;
        end;
        if strm.avail_in = 0 then break;
      end;
      if strm.avail_out = 0 then WriteOut;

      ZlibCCheck(deflate(strm, Z_NO_FLUSH));
      if Assigned(ProgCallback) then begin   { V6.01 tell user }
      {  inc (Totcount, strm.avail_in);    { V6.01 keep track of data read }
        Cancel := false;
        ProgCallback(Sender, TotCount, Cancel);
        if Cancel then break;
      end;
    until false;

    repeat
      if strm.avail_out = 0 then WriteOut;
      Finished := ZlibCCheck(deflate(strm, Z_FINISH)) = Z_STREAM_END;
      WriteOut;
    until Finished;

    if not UseOutBuf then
    begin
      //truncate when using direct output
      OutStream.Size := OutStream.Position;
    end;

    //adjust position of the input stream
    if UseInBuf then
      //seek back when unused data
      InStream.Seek(-strm.avail_in, soFromCurrent)
    else
      //simple seek
      InStream.Seek(strm.total_in, soFromCurrent);

    ZlibCCheck(deflateEnd(strm));
  finally
    if InBuf <> nil then FreeMem(InBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream; Level:
     TCompressionLevel; StreamType : TZStreamType; UseDirectOut: boolean);
begin
    ZlibCompressStreamEx(InStream, OutStream, Levels[Level], StreamType,
                                                     UseDirectOut, Nil, Nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
