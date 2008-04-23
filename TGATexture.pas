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
unit TGATexture;

interface

uses
  Windows, OpenGL, SysUtils;

function LoadTexture(Filename: String; var Texture: GLuint): Boolean;
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;

implementation     

function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;


{------------------------------------------------------------------}
{  Create the Texture                                              }
{------------------------------------------------------------------}
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
var
  Texture : GLuint;
begin
  glGenTextures(1, Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
//  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture does NOT blend with object background}

  { Select a filtering type. BiLinear filtering produces very good results with little performance impact
    GL_NEAREST               - Basic texture (grainy looking texture)
    GL_LINEAR                - BiLinear filtering
    GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
    GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
  }

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
//  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL

  result :=Texture;
end;


// Copy a pixel from source to dest and Swap the RGB color values
procedure CopySwapPixel(const Source, Destination : Pointer);
asm
  push ebx
  mov bl,[eax+0]
  mov bh,[eax+1]
  mov [edx+2],bl
  mov [edx+1],bh
  mov bl,[eax+2]
  mov bh,[eax+3]
  mov [edx+0],bl
  mov [edx+3],bh
  pop ebx
end;


{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}
function LoadTexture(Filename: String; var Texture : GLuint): Boolean;
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
  image     : Pointer;    {or PRGBTRIPLE}
  CompImage : Pointer;
  Width, Height : Integer;
  ColorDepth    : Integer;
  ImageSize     : Integer;
  BufferIndex : Integer;
  currentByte : Integer;
  CurrentPixel : Integer;
  I : Integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
begin
  result :=FALSE;
  if FileExists(Filename) then begin
    AssignFile(TGAFile, Filename);
    FileMode:=0; Reset(TGAFile,1); FileMode:=2; //Open ReadOnly

    // Read in the bitmap file header
    BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader));
    result :=TRUE;
  end else
  begin
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('TGA Texture'), MB_OK);
    Exit;
  end;

  if Result = TRUE then
  begin
//    Result :=FALSE;

    // Only support 24, 32 bit images
    if (TGAHeader.ImageType <> 2) AND    { TGA_RGB }
       (TGAHeader.ImageType <> 10) then  { Compressed RGB }
    begin
      Result := False;
      CloseFile(TGAFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32bit TGA supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    // Don't support colormapped files
    if TGAHeader.ColorMapType <> 0 then
    begin
      Result := False;
      CloseFile(TGAFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
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
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    GetMem(Image, ImageSize);

    if TGAHeader.ImageType = 2 then   // Standard 24, 32 bit TGA file
    begin
      BlockRead(TGAFile, image^, ImageSize, bytesRead);
      CloseFile(TGAFile);
      if bytesRead <> ImageSize then
      begin
        Result := False;
        CloseFile(TGAFile);
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
        Exit;
      end;

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
    end;

    // Compressed 24, 32 bit TGA files
    if TGAHeader.ImageType = 10 then
    begin
      ColorDepth :=ColorDepth DIV 8;
      CurrentByte :=0;
      CurrentPixel :=0;
      BufferIndex :=0;

      GetMem(CompImage, FileSize(TGAFile)-sizeOf(TGAHeader));
      BlockRead(TGAFile, CompImage^, FileSize(TGAFile)-sizeOf(TGAHeader), BytesRead);   // load compressed data into memory
      CloseFile(TGAFile);
      if bytesRead <> FileSize(TGAFile)-sizeOf(TGAHeader) then
      begin
        Result := False;
        CloseFile(TGAFile);
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
        Exit;
      end;

      // Extract pixel information from compressed data
      repeat
        Front := Pointer(Integer(CompImage) + BufferIndex);
        Inc(BufferIndex);
        if Front^ < 128 then
        begin
          For I := 0 to Front^ do
          begin
            CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex+I*ColorDepth), Pointer(Integer(image)+CurrentByte));
            CurrentByte := CurrentByte + ColorDepth;
            inc(CurrentPixel);
          end;
          BufferIndex :=BufferIndex + (Front^+1)*ColorDepth
        end
        else
        begin
          For I := 0 to Front^ -128 do
          begin
            CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex), Pointer(Integer(image)+CurrentByte));
            CurrentByte := CurrentByte + ColorDepth;
            inc(CurrentPixel);
          end;
          BufferIndex :=BufferIndex + ColorDepth
        end;
      until CurrentPixel >= Width*Height;

      if ColorDepth = 3 then
        Texture :=CreateTexture(Width, Height, GL_RGB, Image)
      else
        Texture :=CreateTexture(Width, Height, GL_RGBA, Image);
    FreeMem(CompImage);
    end;

    Result :=TRUE;
    FreeMem(Image);
  end;
end;


end.
