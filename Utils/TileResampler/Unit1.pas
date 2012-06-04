unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Classes, SysUtils, Dialogs, KromUtils, PNGImage
  {$IFDEF MSWindows}, Windows {$ENDIF}
  {$IFDEF Unix}, LCLType {$ENDIF}
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC} ,LResources, zstream, fileutil {$ENDIF}
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var f,f2:file;
    h:array[1..18]of char;
    b:array[1..160]of char;
    i,k:integer;
begin
  assignfile(f,'Tiles.tga'); reset (f,1);
  assignfile(f2,'Tiles1.tga'); rewrite (f2,1);
  blockread(f,h,18);
  h[13]:=#0; h[15]:=#0; //512x512
  blockwrite(f2,h,18);
  for i:=1 to 640 do
  for k:=1 to 16 do
  begin
    blockread(f,b,160);
    if not ((i-1) mod 40 in [4,9,14,19,24,29,34,39]) then
    begin
      blockwrite(f2,b[0*4+1],4*4);
      blockwrite(f2,b[5*4+1],3*4);
      blockwrite(f2,b[9*4+1],4*4);
      blockwrite(f2,b[14*4+1],3*4);
      blockwrite(f2,b[18*4+1],4*4);
      blockwrite(f2,b[23*4+1],3*4);
      blockwrite(f2,b[27*4+1],4*4);
      blockwrite(f2,b[32*4+1],3*4);
      blockwrite(f2,b[36*4+1],4*4);
    end;
  end;
  closefile(f);
  closefile(f2);
end;


procedure TForm1.Button2Click(Sender: TObject);
var f1,f2,f3,f4,f5,f6:file;
    h:array[1..18]of char;
    e:array[1..4]of char;
    a,b,c:array[1..4]of char;
    i,k:integer;
begin
  assignfile(f1,'11.tga'); reset (f1,1);
  assignfile(f2,'22.tga'); reset (f2,1);
  assignfile(f3,'33.tga'); reset (f3,1);
  assignfile(f4,'out1.tga'); rewrite (f4,1);
  assignfile(f5,'out2.tga'); rewrite (f5,1);
  assignfile(f6,'out3.tga'); rewrite (f6,1);
  blockread(f1,h,18);
  blockread(f2,h,18);
  blockread(f3,h,18);
  blockwrite(f4,h,18);
  blockwrite(f5,h,18);
  blockwrite(f6,h,18);
  FillChar(e,4,#0);
  for i:=1 to 640 do for k:=1 to 640 do begin
    blockread(f1,a,4);
    blockread(f2,b,4);
    blockread(f3,c,4);
    if (a[1]=b[1])and(b[1]=c[1])
    and(a[2]=b[2])and(b[2]=c[2])
    and(a[3]=b[3])and(b[3]=c[3])
    then begin
      blockwrite(f4,e,4);
      blockwrite(f5,e,4);
      blockwrite(f6,e,4);
    end else begin
      blockwrite(f4,a,4);
      blockwrite(f5,b,4);
      blockwrite(f6,c,4);
    end;
  end;
  closefile(f1);
  closefile(f2);
  closefile(f3);
  closefile(f4);
  closefile(f5);
  closefile(f6);
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  InputStream, OutputStream: TFileStream;
  {$IFDEF WDC}
  CompressionStream: TZCompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  CompressionStream: TCompressionStream;
  {$ENDIF}
  c:char;
begin
  if not RunOpenDialog(OpenDialog1, '', '', 'Any file|*.*') then exit;
  InputStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
  InputStream.Position := 0;
  InputStream.Read(c, 1);

  if c = #120 then begin
    Application.MessageBox('File is already a zLib archive', 'Error', MB_OK);
    InputStream.Free;
    exit;
  end;
  InputStream.Position := 0; //Reset just in case

  OutputStream := TFileStream.Create(OpenDialog1.FileName+'.tmp', fmCreate);
  {$IFDEF WDC}
  CompressionStream := TZCompressionStream.Create(OutputStream, zcMax);
  {$ENDIF}
  {$IFDEF FPC}
  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  {$ENDIF}
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
  CopyFile(PChar(OpenDialog1.FileName+'.tmp'),PChar(OpenDialog1.FileName), false); //Overwrite
  DeleteFile(PChar(OpenDialog1.FileName+'.tmp'));
end;


procedure TForm1.Button4Click(Sender: TObject);
var
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF WDC}
  DeCompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  DeCompressionStream: TDecompressionStream;
  i: Integer;
  Buf: array[0..1023]of Byte;
  {$ENDIF}
  c:char;
begin
  if not RunOpenDialog(OpenDialog1, '', '', 'Any file|*.*') then exit;
  InputStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
  InputStream.Position := 0;
  InputStream.Read(c, 1);

  if c <> #120 then begin
    Application.MessageBox('File is not a zLib archive', 'Error', MB_OK);
    InputStream.Free;
    exit;
  end;
  InputStream.Position := 0; //Reset just in case

  OutputStream := TMemoryStream.Create;
  {$IFDEF WDC}
  DecompressionStream := TZDecompressionStream.Create(InputStream);
  OutputStream.CopyFrom(DecompressionStream, 0);
  {$ENDIF}
  {$IFDEF FPC}
  DecompressionStream := TDecompressionStream.Create(InputStream);
  repeat
  i:=DecompressionStream.Read(Buf, SizeOf(Buf));
  if i <> 0 then OutputStream.Write(Buf, i);
  until i <= 0;
  {$ENDIF}
  InputStream.Free; //Free the Stream before we write to same filename
  OutputStream.SaveToFile(OpenDialog1.Filename);
  DecompressionStream.Free;
  OutputStream.Free;
end;


procedure TForm1.Button5Click(Sender: TObject);
var
  L, ii,kk,h,j,pX: Integer;
  c:array of byte;
  SizeX,SizeY, ID: Integer;
  f: file;
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF WDC}
  DecompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  DecompressionStream: TDecompressionStream;
  I: Integer;
  Buf: array[0..1023]of Byte;
  {$ENDIF}
  PNG: TPNGObject;
  Blank: Boolean;
  Filename: string;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));

  if not OpenDialog1.Execute then Exit;

  CreateDir('Split');
  for L := 0 to OpenDialog1.Files.Count - 1 do
  begin
    Filename := ExtractFileName(OpenDialog1.Files[L]);
    AssignFile(f, Filename);
    FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

    SetLength(c,18+1);
    blockread(f,c[1],18); //SizeOf(TGAHeader)
    SizeX := c[13]+c[14]*256;
    SizeY := c[15]+c[16]*256;

    if c[1]=120 then
    begin
      closefile(f);
      InputStream := TFileStream.Create(OpenDialog1.Files[L], fmOpenRead or fmShareDenyNone);
      OutputStream := TMemoryStream.Create;
      {$IFDEF WDC}
       DecompressionStream := TZDecompressionStream.Create(InputStream);
       OutputStream.CopyFrom(DecompressionStream, 0);
      {$ENDIF}
      {$IFDEF FPC}
       DecompressionStream := TDecompressionStream.Create(InputStream);
       repeat
         i:=DecompressionStream.Read(Buf, SizeOf(Buf));
         if i <> 0 then OutputStream.Write(Buf, i);
       until i <= 0;
      {$ENDIF}
      OutputStream.Position := 0;
      OutputStream.ReadBuffer(c[1], 18); //SizeOf(TGAHeader)
      SizeX := c[13]+c[14]*256;
      SizeY := c[15]+c[16]*256;
      SetLength(c,SizeX*SizeY*4+1);
      OutputStream.ReadBuffer(c[1], SizeX*SizeY*4);
      InputStream.Free;
      OutputStream.Free;
      DecompressionStream.Free;
    end
    else
    begin
      SetLength(c,SizeX*SizeY*4+1);
      blockread(f,c[1],SizeX*SizeY*4);
      closefile(f);
    end;

    PNG := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, 32, 32);

    for ii := 0 to 15 do
    for kk := 0 to 15 do
    begin

      Blank := True;
      for j := 0 to 31 do
      for h := 0 to 31 do
      begin
        //TGA comes flipped upside down
        pX := (((SizeX - 1) - (ii * 32 + j)) * SizeX + kk * 32 + h) * 4;
        PNG.Pixels[h,j] := c[pX+3] + c[pX+2] shl 8 + c[pX+1] shl 16;
        PNG.AlphaScanline[j]^[h] := c[pX+4];
        Blank := Blank and (PCardinal(@c[pX+1])^ = 0);
      end;

      ID := ii * 16 + kk + 1;
      if Copy(Filename, 0, 5) = 'Water' then
        Inc(ID, 300 * StrToInt(Filename[6]));
      if Copy(Filename, 0, 5) = 'Falls' then
        Inc(ID, 300 * (8 + StrToInt(Filename[6])));
      if Copy(Filename, 0, 5) = 'Swamp' then
        Inc(ID, 300 * (8 + 5 + StrToInt(Filename[6])));

      if not Blank then
        PNG.SaveToFile('Split\7_' + Int2Fix(ID, 4) + '.png');
    end;

    PNG.Free;
  end;

end;


{$IFDEF FPC}
initialization
  {$i Unit1.lrs}
{$ENDIF}

end.
