//----------------------------------------------------------------------------
//
// Author      : Jan Horn
// Email       : jhorn@global.co.za
// Website     : http://www.sulaco.co.za
//               http://home.global.co.za/~jhorn
// Version     : 1.0
// Date        : 28 April 2002
//
// Description : A unit that used with OpenGL projects to load compressed
//               and uncompressed 24 and 32bit TGA files from the disk.
// Usage       : LoadTexture(FileName, TextureName);
//
//----------------------------------------------------------------------------
unit KM_TGATexture;
{$I ..\KaM_Remake.inc}
interface
uses
  Forms,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  SysUtils, Classes, dglOpenGL
  {$IFDEF WDC}, ZLib {$ENDIF}
  {$IFDEF FPC}, zstream {$ENDIF}
  ;

{$IFDEF UNIX}
const
  opengl32 = 'libGL.so';
  glu32 = 'libGLU.so';
{$ENDIF}


function GenerateTextureCommon: GLuint;
function LoadTextureTGA(FileName: string; var Texture: GLuint): Boolean;


implementation


function GenerateTextureCommon: GLuint;
var Texture: GLuint;
begin
  Result := 0;
  if not Assigned(glGenTextures) then Exit;

  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);

  {Enable color blending into texture}
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  //GL_MODULATE is our choice
  //GL_REPLACE is also available since version 1.1
  //can't use GL_REPLACE cos it disallows blending of texture with custom color (e.g. trees in FOW)

  {Keep original KaM grainy look}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

  {Clamping UVs solves edge artifacts}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  Result := Texture;
end;


{------------------------------------------------------------------}
{  Create the Texture                                              }
{------------------------------------------------------------------}
function CreateTexture(Width, Height, Format: Word; pData: Pointer): Integer;
begin
  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps

  if Result <> 0 then
    if Format = GL_RGBA then
      gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
    else
      gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
  //  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL
end;


procedure FlipImageVertical(W,H,BPP: Word; Image: Pointer);
var ii,kk:cardinal;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
begin
  for kk:=0 to (H div 2)-1 do
  for ii:=0 to W*BPP-1 do
  begin
    Front := Pointer(cardinal(Image)+kk*W*BPP+ii);
    Back := Pointer(cardinal(Image)+(H-kk-1)*W*BPP+ii);
    Temp := Front^;
    Front^ := Back^;
    Back^ := Temp;
  end;
end;

{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}
function LoadTextureTGA(FileName: string; var Texture: GLuint): Boolean;
var
  TGAHeader : packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : Array[0..4] of Byte;
    OrigX  : Array [0..1] of Byte;
    OrigY  : Array [0..1] of Byte;
    Width  : Array [0..1] of Byte;
    Height : Array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;
  TGAFile   : File;
  bytesRead : Integer;
  ZLibCompressed:boolean;
  Image     : Pointer;
  Width, Height : Integer;
  ColorDepth    : Integer;
  ImageSize     : Integer;
  i : cardinal;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  Errs:string;

  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF WDC}
  DecompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  DecompressionStream: TDecompressionStream;
  ii: Integer;
  Buf: array[0..1023]of Byte;
  {$ENDIF}
begin
  Result := false;

  {$IFDEF WDC} OutputStream := nil; {$ENDIF} //This makes compiler happy

  if FileExists(FileName) then begin
    AssignFile(TGAFile, FileName);
    FileMode := 0;
    Reset(TGAFile,1);
    FileMode := 2; //Open ReadOnly

    // Read in the bitmap file header
    BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader), bytesRead);

    if SizeOf(TGAHeader) <> bytesRead then begin
      CloseFile(TGAFile);
      Errs := 'Couldn''t read file header "'+ FileName +'".';
      MessageBox(HWND(nil),PChar(Errs), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

  end else begin
    Errs := 'File not found  - ' + FileName;
    Application.MessageBox(PChar(Errs), PChar('TGA Texture'), MB_OK);
    Exit;
  end;

  //TGA is compressed by ZLibEx, thats only KaM Remake custom option
  ZLibCompressed := TGAHeader.FileType=120;

  //Uncompress TGA header
  if ZLibCompressed then
  begin
    CloseFile(TGAFile);
    InputStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    OutputStream := TMemoryStream.Create;
    {$IFDEF WDC}
    DecompressionStream := TZDecompressionStream.Create(InputStream);
    OutputStream.CopyFrom(DecompressionStream, 0);
    {$ENDIF}
    {$IFDEF FPC}
    DecompressionStream := TDecompressionStream.Create(InputStream);
    repeat
      ii := DecompressionStream.Read(Buf, SizeOf(Buf));
      if ii <> 0 then OutputStream.Write(Buf, ii);
    until ii <= 0;
    {$ENDIF}
    InputStream.Free;
    DecompressionStream.Free;
    OutputStream.Position:=0;
    OutputStream.ReadBuffer(TGAHeader, SizeOf(TGAHeader));
  end;

  // Only support 24, 32 bit uncompressed images
  if (TGAHeader.ImageType <> 2) then    { TGA_RGB }
  begin
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ FileName +'". Only 24 and 32bit TGA supported.';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  // Don't support colormapped files
  if TGAHeader.ColorMapType <> 0 then
  begin
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ FileName +'". Colormapped TGA files not supported.';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  // Get the width, height, and color depth
  Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
  Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
  ColorDepth := TGAHeader.BPP;
  ImageSize  := Width*Height*(ColorDepth div 8);

  if ColorDepth < 24 then
  begin
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ FileName +'". Only 24 and 32 bit TGA files supported.';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  if ZLibCompressed then
  begin
    GetMem(Image, ImageSize);
    bytesRead := OutputStream.Read(Image^, ImageSize);
    OutputStream.Free;
  end
  else
  begin
    GetMem(Image, ImageSize);
    BlockRead(TGAFile, Image^, ImageSize, bytesRead);
    CloseFile(TGAFile);
  end;

  if bytesRead <> ImageSize then
  begin
    Result := False;
    Errs := 'Couldn''t read file "'+ FileName +'".';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  FlipImageVertical(Width,Height,(ColorDepth div 8),Image);

  // TGAs are stored BGR and not RGB, so swap the R and B bytes.
  // 32 bit TGA files have alpha channel and gets loaded differently
  if TGAHeader.BPP = 24 then
  begin
    for i:=0 to Width*Height-1 do
    begin
      Front := Pointer(cardinal(Image) + i*3);
      Back := Pointer(cardinal(Image) + i*3 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture := CreateTexture(Width, Height, GL_RGB, Image);
  end
  else
  begin
    for i:=0 to Width*Height-1 do
    begin
      Front := Pointer(cardinal(Image) + i*4);
      Back := Pointer(cardinal(Image) + i*4 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture := CreateTexture(Width, Height, GL_RGBA, Image);
  end;

  Result := true;
  FreeMem(Image);
end;


end.

