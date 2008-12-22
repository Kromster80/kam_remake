unit PTXTexture;
interface
uses Windows, OpenGL, SysUtils, dglOpenGL, JPEG, Graphics;

function LoadTexturePTX(Filename:string; var Texture: GLuint): Boolean;
function LoadTextureJPG(Filename:string; var Texture: GLuint): Boolean;
function LoadTextureJPGa(Filename:string; var Texture: GLuint): Boolean;
function CreateTexture(Width, Height, Compression, BitsPerPixel, MipMaps : Word; pData :array of Pointer) : Integer;

implementation

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;


function CreateTexture(Width, Height, Compression, BitsPerPixel, MipMaps : Word; pData :array of Pointer) : Integer;
var
  Texture1:GLuint;
  BlockSize,i:integer;
  h,w,mip:integer;
  MipSize:integer;
begin
  glGenTextures(1, Texture1);
  glBindTexture(GL_TEXTURE_2D, Texture1);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  mip:=0; h:=Height; w:=Width;  //Computing number of mipmaps for image until size of 4
  while((W>4)and(H>4)) do begin //smaller sizes can't be handled well in GLSL
  W:=W div 2;                   //and causes 0x00 alpha channel for 24bit images
  H:=H div 2;                   //instead of 0xff.
  inc(mip);                     //P.S. 4x4 is a smallest block size allowed in DXT
  end;
  if Mip>MipMaps then Mip:=MipMaps; //if declared number is smaller - use it

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL,Mip-1);

  if Compression=1 then begin
    case BitsPerPixel of
      16: BlockSize:=8;
      24: BlockSize:=8;
      32: BlockSize:=16;
      else BlockSize:=0;
    end;

    for i:=0 to MipMaps-1 do begin
      MipSize:=(Width*Height div 16)*BlockSize;
      if (BitsPerPixel=16)or(BitsPerPixel=24) then
        glCompressedTexImage2DARB(GL_TEXTURE_2D,i,GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,Width,Height,0,MipSize,pData[i]) else
      if BitsPerPixel=32 then
        glCompressedTexImage2DARB(GL_TEXTURE_2D,i,GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,Width,Height,0,MipSize,pData[i]);
      Width:=Width div 2;
      Height:=Height div 2;
    end;

  end else
  if Compression=0 then begin
    if BitsPerPixel=24 then
      gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, Width, Height, GL_BGRA, GL_UNSIGNED_BYTE, pData[0])
//      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Width, Height, 0, GL_BGR, GL_UNSIGNED_BYTE, pData[0])
      else
    if BitsPerPixel=32 then
      gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_BGRA, GL_UNSIGNED_BYTE, pData[0]);
    end;

  result :=Texture1;
end;


function LoadTexturePTX(Filename:string; var Texture : GLuint): Boolean;
var
  PTXFile       :File;
  PTXHead       :record// Header of PTX
    Compression :Byte;
    BitPerPixel :Byte;
    Clear1      :Byte;
    Clear2      :Byte;
    Width       :integer;
    Height      :integer;
    MipMaps     :byte;
    R,G,B       :byte;
  end;
  CurLev        :integer; //counter
  NumRead       :Integer;
  MipHead:array[1..16]of record
  Size,SYNSize  :integer;
  end;
  MipLevel:array[1..32] of pointer;
  SYNData:array of byte;
  i,k,ci,CurChr,addv,x:integer;
  flag:array[1..8]of byte;
  Dist,Leng:integer;
  Dat,Dat2:^byte;
begin
  result :=FALSE;

  if FileExists(Filename) then begin
    AssignFile(PTXFile, Filename);
    FileMode:=0; Reset(PTXFile,1); FileMode:=2; //Open ReadOnly
    BlockRead(PTXFile,PTXHead,16); //Read head
    result :=TRUE;
  end else

  begin
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('PTX Texture'), MB_OK);
    Exit;
  end;

  if Result = TRUE then
  begin

    // Only support 24, 32 bit images
    if (PTXHead.BitPerPixel<>16)and(PTXHead.BitPerPixel<>24)and(PTXHead.BitPerPixel<>32) then
    begin
      Result:=False;
      CloseFile(PTXFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 16, 24 and 32bit PTX supported.'), PChar('PTX File Error'), MB_OK);
      Exit;
    end;

    for CurLev:=1 to PTXHead.MipMaps do begin
      BlockRead(PTXFile,MipHead[CurLev],8);//

      if MipHead[CurLev].SYNSize>0 then begin //decompress
        setlength(SYNData,MipHead[CurLev].SYNSize+100);
        GetMem(MipLevel[CurLev], MipHead[CurLev].Size+100);
        blockread(PTXFile,SYNData[1],MipHead[CurLev].SYNSize); //read all needed data

        ci:=1; CurChr:=1; //currentbyte
        addv:=0;
        repeat
          x:=ord(SYNData[ci]); inc(ci);
          if x>=128 then begin x:=x-128; flag[8]:=1; end else flag[8]:=2;  //2 means 0
          if x>=64 then begin x:=x-64; flag[7]:=1; end else flag[7]:=2;    //1-Take that
          if x>=32 then begin x:=x-32; flag[6]:=1; end else flag[6]:=2;    //2-Take from behind
          if x>=16 then begin x:=x-16; flag[5]:=1; end else flag[5]:=2;
          if x>=8 then begin x:=x-8; flag[4]:=1; end else flag[4]:=2;
          if x>=4 then begin x:=x-4; flag[3]:=1; end else flag[3]:=2;
          if x>=2 then begin x:=x-2; flag[2]:=1; end else flag[2]:=2;
          if x>=1 then begin {x:=x-1;} flag[1]:=1; end else flag[1]:=2;

          for i:=1 to 8 do if flag[i]=1 then begin
            Dat:=pointer(integer(MipLevel[CurLev])-1+CurChr);
            Dat^:=SYNData[ci];
            inc(CurChr);
            inc(ci);
          end else begin

            Dist:=(SYNData[ci])+((SYNData[ci+1]) AND $F0)*16; //1byte + 4bits from 2nd byte  Length is only last 4bits+3
            Leng:=((SYNData[ci+1]) AND $0F) + 3;

            if CurChr>(18+addv+4096) then inc(addv,4096);

            for k:=1 to Leng do
              if Dist>=(CurChr-addv) then
                if (18+k+Dist+addv-4096)<=0 then begin
                  Dat:=pointer(integer(MipLevel[CurLev])-1+CurChr+k-1);
                  Dat^:=byte(32);
                end else begin
                  Dat:=pointer(integer(MipLevel[CurLev])-1+CurChr+k-1);
                  Dat2:=pointer(integer(MipLevel[CurLev])-1+18+k+Dist+addv-4096);
                  Dat^:=Dat2^;
                end
              else
                if (18+k+Dist+addv)>(CurChr+k-1) then begin//if overlap forward
                  Dat:=pointer(integer(MipLevel[CurLev])-1+CurChr+k-1);
                  Dat2:=pointer(integer(MipLevel[CurLev])-1+18+k+Dist+addv-4096);
                  Dat^:=Dat2^;
                end else begin
                  Dat:=pointer(integer(MipLevel[CurLev])-1+CurChr+k-1);
                  Dat2:=pointer(integer(MipLevel[CurLev])-1+18+k+Dist+addv);
                  Dat^:=Dat2^;
                end;
                  //Synetic used ring buffer filled with #32 after all :-)
            inc(CurChr,Leng);
            inc(ci,2);
          end;
        until(ci>MipHead[CurLev].SYNSize);
      end else begin

        GetMem(MipLevel[CurLev], MipHead[CurLev].Size);
        BlockRead(PTXFile, MipLevel[CurLev]^, MipHead[CurLev].Size, NumRead);

        if NumRead <> MipHead[CurLev].Size then begin
        Result := False;
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'". Unexpected end of file.'), PChar('PTX File Error'), MB_OK);
        Exit;
        end;

      end;

    end;

    CloseFile(PTXFile);
    Texture:=CreateTexture(     PTXHead.Width,
                                PTXHead.Height,
                                PTXHead.Compression,
                                PTXHead.BitPerPixel,
                                PTXHead.MipMaps,
                                MipLevel);

    for CurLev:=1 to PTXHead.MipMaps do FreeMem(MipLevel[CurLev]);
    Result :=TRUE;
  end;
end;


function LoadTextureJPG(Filename:string; var Texture : GLuint): Boolean;
var
  JPGi:TJPEGImage;
  BMPi:TBitmap;
  MipData:array of cardinal;
  pData:array[0..1]of pointer;
  i,k:integer;
  Line : ^cardinal;
begin
  result :=FALSE;
  JPGi:=TJPEGImage.Create;

  if not FileExists(Filename) then begin
  //MessageBox(0, PChar('File not found  - ' + Filename), PChar('JPG Texture'), MB_OK);

  setlength(MipData, 32*16); //size in bytes
  for i:=0 to length(MipData)-1 do
    MipData[i]:=random($FFFFFF);
  pData[0]:=@MipData[0];
  Texture:=CreateTexture(32, 16, 0, 24, 1, pData);

  exit;
  end;

  JPGi.LoadFromFile(Filename);

  BMPi:=TBitmap.Create;
  BMPi.pixelformat:=pf32bit;
  BMPi.Width:=JPGi.Width;
  BMPi.Height:=JPGi.Height;
  BMPi.canvas.draw(0,0,JPGi); //Copy JPEG to Bitmap

  setlength(MipData, BMPi.Width*BMPi.Height); //size in bytes

  for i:=0 to BMPi.Height-1 do begin
    Line:=BMPi.scanline[i];
    for k:=0 to BMPi.Width-1 do begin
      MipData[i*BMPi.Width+k]:=Line^;
      inc(Line);
    end;
  end;

  pData[0]:=@MipData[0];
  Texture:=CreateTexture(BMPi.Width, BMPi.Height, 0, 24, 1, pData);

  setlength(MipData,0);
  Result :=TRUE;
end;

function LoadTextureJPGa(Filename:string; var Texture : GLuint): Boolean;
var
  JPGi:TJPEGImage;
  BMPi:TBitmap;
  MipData:array of cardinal;
  pData:array[0..1]of pointer;
  i,k:integer;
  Line : ^cardinal;
begin
  result :=FALSE;
  JPGi:=TJPEGImage.Create;

  if not FileExists(Filename) then begin
  MessageBox(0, PChar('File not found  - ' + Filename), PChar('JPG Texture'), MB_OK);
  exit; end;

  JPGi.LoadFromFile(Filename);

  BMPi:=TBitmap.Create;
  BMPi.pixelformat:=pf32bit;
  BMPi.Width:=JPGi.Width;
  BMPi.Height:=JPGi.Height;
  BMPi.canvas.draw(0,0,JPGi); //Copy JPEG to Bitmap

  setlength(MipData, BMPi.Width*BMPi.Height); //size in bytes

  for i:=0 to BMPi.Height-1 do begin
    Line:=BMPi.scanline[i];
    for k:=0 to BMPi.Width-1 do begin
      MipData[i*BMPi.Width+k]:=Line^;
      inc(Line);
    end;
  end;

  for i:=0 to BMPi.Width*BMPi.Height-1 do MipData[i]:=(MipData[i] shl 8) or $00FFFFFF;

  pData[0]:=@MipData[0];
  Texture:=CreateTexture(BMPi.Width, BMPi.Height, 0, 32, 1, pData);

  setlength(MipData,0);
  Result :=TRUE;
end;


end.
