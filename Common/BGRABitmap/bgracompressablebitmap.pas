unit bgracompressablebitmap;

{$mode objfpc}{$H+}

interface

{ This unit contains the TBGRACompressableBitmap class, which
  can be used to temporarily compress bitmaps in memory.
  To use it, create an instance with the bitmap you want
  to compress. You can then free the original bitmap because
  TBGRACompressableBitmap contains all information necessary
  to build it again. To construct again your bitmap, call
  the GetBitmap function.

  When you have your bitmap in TBGRACompressableBitmap,
  you can call Compress function as many times as necessary
  until all data is compressed. It does only a part of the
  work at each call, so you can put it in a loop or in
  a timer. When it's done, Compress returns false to
  notify that it did nothing, which means you can
  stop calling Compress.

  In this implementation, the memory usage grows during
  the compression process and is lower only after it is
  finished. So it is recommended to compress one bitmap
  at a time. }

uses
  Classes, SysUtils, BGRABitmap;

type

   { TBGRACompressableBitmap }

   TBGRACompressableBitmap = class
   private
     FWidth,FHeight: integer;
     FCaption: String;
     FBounds: TRect;
     FCompressedDataArray: array of TMemoryStream;
     FUncompressedData: TMemoryStream;
     FCompressionProgress: Int64;
     procedure Decompress;
     procedure FreeData;
   public
     constructor Create;
     constructor Create(Source: TBGRABitmap);
     function GetBitmap: TBGRABitmap;
     
     //call Compress as many times as necessary
     //when it returns false, it means that
     //the image compression is finished
     function Compress: boolean; 
     
     function UsedMemory: Int64;
     procedure Assign(Source: TBGRABitmap);
     destructor Destroy; override;
     property Width : Integer read FWidth;
     property Height: Integer read FHeight;
     property Caption : string read FCaption;
   end;

implementation

uses zstream, BGRABitmapTypes;

// size of each chunk treated by Compress function
const maxPartSize = 524288;

{ TBGRACompressedBitmap }

constructor TBGRACompressableBitmap.Create;
begin
  FUncompressedData := nil;
  FCompressedDataArray := nil;
  FWidth := 0;
  FHeight := 0;
  FCaption := '';
  FCompressionProgress := 0;
end;

constructor TBGRACompressableBitmap.Create(Source: TBGRABitmap);
begin
  FUncompressedData := nil;
  FCompressedDataArray := nil;
  FWidth := 0;
  FHeight := 0;
  FCaption := '';
  FCompressionProgress := 0;
  Assign(Source);
end;

{ Constructs the bitmap again, decompressing if necessary.
  After this, the image is not compressed anymore so the
  memoy usage grows again and the access becomes fast
  because there is no need to decompress anymore. }
function TBGRACompressableBitmap.GetBitmap: TBGRABitmap;
var UsedPart: TBGRABitmap;
    UsedNbPixels: Integer;
begin
  Decompress;
  if FUncompressedData = nil then
  begin
    result := nil;
    exit;
  end;
  result := TBGRABitmap.Create(FWidth,FHeight);
  result.Caption := FCaption;
  FUncompressedData.Position := 0;
  if (FBounds.Left <> 0) or (FBounds.Top <> 0)
    or (FBounds.Right <> FWidth) or (FBounds.Bottom <> FHeight) then
  begin
    UsedNbPixels := (FBounds.Right-FBounds.Left)*(FBounds.Bottom-FBounds.Top);
    if UsedNbPixels > 0 then
    begin
      UsedPart := TBGRABitmap.Create(FBounds.Right-FBounds.Left,FBounds.Bottom-FBounds.Top);
      FUncompressedData.Read(UsedPart.Data^,UsedPart.NbPixels*Sizeof(TBGRAPixel));
      result.PutImage(FBounds.Left,FBounds.Top,UsedPart,dmSet);
      UsedPart.Free;
    end;
  end else
    FUncompressedData.Read(result.Data^,result.NbPixels*Sizeof(TBGRAPixel));
end;

{ Returns the total memory used by this object for storing bitmap data }
function TBGRACompressableBitmap.UsedMemory: Int64;
var i: integer;
begin
  result := 0;
  for i := 0 to high(FCompressedDataArray) do
    result += FCompressedDataArray[i].Size;
  if FUncompressedData <> nil then result += FUncompressedData.Size;
end;

{ Do one compress step or return false }
function TBGRACompressableBitmap.Compress: boolean;
var comp: Tcompressionstream;
    partSize: integer;
begin
  if FCompressedDataArray = nil then FCompressionProgress := 0;
  if (FUncompressedData = nil) or (FUncompressedData.Size = 0) then
  begin
    result := false;
    exit;
  end;
  if FCompressionProgress < FUncompressedData.Size then
  begin
    setlength(FCompressedDataArray, length(FCompressedDataArray)+1);
    FCompressedDataArray[high(FCompressedDataArray)] := TMemoryStream.Create;
    FUncompressedData.Position := FCompressionProgress;
    if FUncompressedData.Size - FCompressionProgress > maxPartSize then
      partSize := maxPartSize else
        partSize := integer(FUncompressedData.Size - FCompressionProgress);

    //use fast compression to avoid slowing down the application
    comp := Tcompressionstream.Create(clfastest,FCompressedDataArray[high(FCompressedDataArray)]);
    comp.write(partSize,sizeof(partSize));
    comp.CopyFrom(FUncompressedData,partSize);
    comp.Free;
    inc(FCompressionProgress, partSize);
  end;
  if FCompressionProgress >= FUncompressedData.Size then
    FreeAndNil(FUncompressedData);
  result := true;
end;

procedure TBGRACompressableBitmap.Decompress;
var decomp: Tdecompressionstream;
    i: integer;
    partSize: integer;
begin
  if (FUncompressedData <> nil) or (FCompressedDataArray = nil) then exit;
  FUncompressedData := TMemoryStream.Create;
  for i := 0 to high(FCompressedDataArray) do
  begin
    FCompressedDataArray[i].Position := 0;
    decomp := Tdecompressionstream.Create(FCompressedDataArray[i]);
    {$hints off}
    decomp.read(partSize,sizeof(partSize));
    {$hints on}
    FUncompressedData.CopyFrom(decomp,partSize);
    decomp.Free;
    FreeAndNil(FCompressedDataArray[i]);
  end;
  FCompressedDataArray := nil;
end;

{ Free all data }
procedure TBGRACompressableBitmap.FreeData;
var i: integer;
begin
  if FCompressedDataArray <> nil then
  begin
    for i := 0 to high(FCompressedDataArray) do
      FCompressedDataArray[I].Free;
    FCompressedDataArray := nil;
  end;
  if FUncompressedData <> nil then FreeAndNil(FUncompressedData);
end;

{ Copy a bitmap into this object. As it is copied, you need not
  keep a copy of the source }
procedure TBGRACompressableBitmap.Assign(Source: TBGRABitmap);
var
  UsedPart: TBGRABitmap;
  NbUsedPixels: integer;
begin
  FreeData;
  if Source = nil then
  begin
    FWidth := 0;
    FHeight := 0;
    FCaption := '';
    exit;
  end;
  FWidth := Source.Width;
  FHeight := Source.Height;
  FCaption := Source.Caption;
  FBounds := Source.GetImageBounds;
  NbUsedPixels := (FBounds.Right-FBounds.Left)*(FBounds.Bottom-FBounds.Top);
  FUncompressedData := TMemoryStream.Create;
  if NbUsedPixels = 0 then exit;

  if (FBounds.Left <> 0) or (FBounds.Top <> 0)
    or (FBounds.Right <> Source.Width) or (FBounds.Bottom <> Source.Height) then
  begin
    UsedPart := Source.GetPart(FBounds) as TBGRABitmap;
    FUncompressedData.Write(UsedPart.Data^,NbUsedPixels*Sizeof(TBGRAPixel));
    UsedPart.Free;
  end else
    FUncompressedData.Write(Source.Data^,Source.NbPixels*Sizeof(TBGRAPixel));
end;

destructor TBGRACompressableBitmap.Destroy;
begin
  FreeData;
  inherited Destroy;
end;

end.

