unit OverbyteIcsHttpCCodZLib;
{
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 8, 2012  V8.01 - Angus, announce deflate is supported as well as gzip

}

interface

{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsZlib.inc}

uses
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsHttpContCod,
    OverbyteIcsZlibHigh,
{$IFDEF USE_ZLIB_OBJ}
    OverbyteIcsZLibObj;          {interface to access ZLIB C OBJ files}
{$ELSE}
    OverbyteIcsZLibDll;          {interface to access zLib1.dll}
{$ENDIF}

type
  THttpCCodzlib = class(THttpContentCoding)
  private
    FStream: TMemoryStream;
  protected
    class function GetActive: Boolean; override;
    class function GetCoding: String; override;
  public
    constructor Create(WriteBufferProc: TWriteBufferProcedure); override;
    destructor Destroy; override;

    procedure Complete; override;
    procedure WriteBuffer(Buffer: Pointer; Count: Integer); override;
  end;

implementation

{ THttpCCodzlib }

constructor THttpCCodzlib.Create(WriteBufferProc: TWriteBufferProcedure);
begin
    inherited Create(WriteBufferProc);
    FStream := TMemoryStream.Create;
end;

destructor THttpCCodzlib.Destroy;
begin
    FStream.Free;
    inherited Destroy;
end;

class function THttpCCodzlib.GetActive: Boolean;
begin
    Result := ZlibGetDllLoaded and (Pos('1.2', String(ZlibGetVersionDll)) = 1);
end;

class function THttpCCodzlib.GetCoding: String;
begin
    Result := 'gzip, deflate';   // V8.01
end;

function Strm_Write(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
begin
    THttpCCodzlib (BackObj.MainObj).OutputWriteBuffer(buf, size) ;
    Result := 0 ;  // assume we wrote all the data OK
end;

{ write the content stream a block at a time, it handles gzip, zlib or raw streams }

procedure THttpCCodzlib.Complete;
var
    strm: z_stream;
    BackObj: PZBack;
begin
    FStream.Position := 0;
    FillChar (strm, sizeof(strm), 0);
    GetMem (BackObj, SizeOf(BackObj^));
    try
        BackObj.InMem := FStream.Memory;  //direct memory access
        BackObj.InStream  := FStream;
        BackObj.OutStream := Nil ;        // not used
        BackObj.MainObj := Self;
        BackObj.ProgressCallback :=  nil;
    //use our own function for reading
        strm.avail_in := Strm_in_func (BackObj, PByte(strm.next_in));
        strm.next_out := @BackObj.Window;  // buffer
        strm.avail_out := 0;
        ZlibCheckInitInflateStream (strm, nil);  // returns stream type which we ignore
        strm.next_out := nil;
        strm.avail_out := 0;
        ZlibDCheck (inflateBackInit (strm, MAX_WBITS, BackObj.Window));
        try
            ZlibDCheck (inflateBack (strm, @Strm_in_func, BackObj,
                                                 @Strm_Write, BackObj));
          //seek back when unused data
            FStream.Seek (-strm.avail_in, soFromCurrent);
          //now trailer can be checked
         finally
            ZlibDCheck (inflateBackEnd (strm));
         end;
    finally
        FreeMem (BackObj);
    end;
end ;

procedure THttpCCodzlib.WriteBuffer(Buffer: Pointer; Count: Integer);
begin
  FStream.WriteBuffer(Buffer^, Count);
end;

initialization
  THttpContCodHandler.RegisterContentCoding(1, THttpCCodzlib);

finalization
  THttpContCodHandler.UnregisterAuthenticateClass(THttpCCodzlib);

end.
