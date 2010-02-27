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
// Usage       : LoadTexture(Filename, TextureName);
//
//----------------------------------------------------------------------------
unit KM_TGATexture;
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
interface
uses
  Forms, Windows,
  {$IFDEF VER140} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  SysUtils, Classes, dglOpenGL
  {$IFDEF VER140}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, paszlib {$ENDIF}
  ;

function LoadTexture(Filename: String; var Texture : GLuint; NewVersionCheckFlip:byte): Boolean;
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
function GenerateTextureCommon():GLuint;

implementation

{$IFDEF VER140}
function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
{$ENDIF}
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;


function GenerateTextureCommon():GLuint;
var Texture : GLuint;
begin
  glGenTextures(1, Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);

  {Enable color blending into texture}
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  //GL_MODULATE is our choice
  //GL_REPLACE is also available since version 1.1, maybe it can fix that flaw of houses blending into black..
  //can't use REPLACE cos it disallows blending of texture with custom color (e.g. trees in FOW)

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
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
begin
  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps

  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
//  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL
end;


procedure FlipImageVertical(W,H,bpp:integer; Image:pointer);
var ii,kk:integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
begin
for kk:=0 to (H div 2)-1 do
for ii:=0 to W*bpp-1 do begin
          Front := Pointer(integer(Image)+kk*W*bpp+ii);
          Back := Pointer(integer(Image)+(H-kk-1)*W*bpp+ii);
          Temp := Front^;
          Front^ := Back^;
          Back^ := Temp;
          end;

end;

{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}
function LoadTexture(Filename: String; var Texture : GLuint; NewVersionCheckFlip:byte): Boolean;
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
  I : Integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  Errs:string;
var
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF VER140} DeCompressionStream: TZDecompressionStream; {$ENDIF}
  DestSize:cardinal;
begin
  result :=FALSE;
  if FileExists(Filename) then begin
    AssignFile(TGAFile, Filename);
    FileMode:=0; Reset(TGAFile,1); FileMode:=2; //Open ReadOnly

    // Read in the bitmap file header
    BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader), bytesRead);

    if SizeOf(TGAHeader) <> bytesRead then begin
      Result := False;      
      CloseFile(TGAFile);
      Errs := 'Couldn''t read file header "'+ Filename +'".';
      MessageBox(HWND(nil),PChar(Errs), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    result :=TRUE;
  end else
  begin
    Errs := 'File not found  - ' + Filename;
    Application.MessageBox(PChar(Errs), PChar('TGA Texture'), MB_OK);
    Exit;
  end;

  if Result <> TRUE then exit;

  OutputStream := nil;
  ZLibCompressed := TGAHeader.FileType=120;

  //TGA is compressed by ZLibEx, thats only KaM Remake custom option
  if ZLibCompressed{TGAHeader.FileType=120} then
  begin
  {$IFDEF VER140}
    CloseFile(TGAFile);
    InputStream := TFileStream.Create(FileName, fmOpenRead);
    OutputStream := TMemoryStream.Create;
    DecompressionStream := TZDecompressionStream.Create(InputStream);
    OutputStream.CopyFrom(DecompressionStream, 0);
    InputStream.Free;
    DeCompressionStream.Free;
    OutputStream.Position:=0;
    OutputStream.ReadBuffer(TGAHeader, SizeOf(TGAHeader));
  {$ENDIF}
  {$IFDEF FPC}
    CloseFile(TGAFile);
    InputStream := TFileStream.Create(FileName, fmOpenRead);

    OutputStream := TMemoryStream.Create;
    uncompress(@OutputStream, DestSize, @InputStream, InputStream.Size);
    InputStream.Free;
    OutputStream.Position:=0;
    OutputStream.ReadBuffer(TGAHeader, SizeOf(TGAHeader));
  {$ENDIF}
  end;

  // Only support 24, 32 bit uncompressed images
  if (TGAHeader.ImageType <> 2) then    { TGA_RGB }
  begin
    Result := False;
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ Filename +'". Only 24 and 32bit TGA supported.';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  // Don't support colormapped files
  if TGAHeader.ColorMapType <> 0 then
  begin
    Result := False;
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ Filename +'". Colormapped TGA files not supported.';
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
    Result := False;
    CloseFile(TGAFile);
    Errs := 'Couldn''t load "'+ Filename +'". Only 24 and 32 bit TGA files supported.';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  GetMem(Image, ImageSize);

  if ZLibCompressed then
  begin
    bytesRead := OutputStream.Read(Image^, ImageSize);
    OutputStream.Free;
  end
  else
  begin
    BlockRead(TGAFile, image^, ImageSize, bytesRead);
    CloseFile(TGAFile);
  end;

  if bytesRead <> ImageSize then
  begin
    Result := False;
    Errs := 'Couldn''t read file "'+ Filename +'".';
    Application.MessageBox(PChar(Errs), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  FlipImageVertical(Width,Height,(ColorDepth div 8),Image);

  // TGAs are stored BGR and not RGB, so swap the R and B bytes.
  // 32 bit TGA files have alpha channel and gets loaded differently
  if TGAHeader.BPP = 24 then
  begin
    for I :=0 to Width * Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*3);
      Back := Pointer(Integer(Image) + I*3 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture :=CreateTexture(Width, Height, GL_RGB, Image);
  end
  else
  begin
    for I :=0 to Width * Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*4);
      Back := Pointer(Integer(Image) + I*4 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture :=CreateTexture(Width, Height, GL_RGBA, Image);
  end;

  Result :=TRUE;
  FreeMem(Image);

end;

end.

