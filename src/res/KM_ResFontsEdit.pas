unit KM_ResFontsEdit;
{$I KaM_Remake.inc}
interface
uses
  Windows,
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  Classes, Graphics, Math, SysUtils, Types,
  KM_CommonTypes, KM_ResFonts
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};


type
  TWideCharArray = array of WideChar;
  //Child class that has the advanced editing methods
  TKMFontDataEdit = class(TKMFontData)
  private
    fTexPadding: Byte;
    function GetTexData(aIndex: Integer): TKMCardinalArray;
  public
    procedure CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; aAntialias: Boolean; const aChars: TWideCharArray);
    procedure CollateFonts(aFonts: array of TKMFontDataEdit);
    procedure ExportGridPng(const aFilename: string; aPadding: TRect);
    procedure ImportGridPng(const aFilename: string);
    procedure ImportPng(const aFilename: string; aIndex: Integer);
    function MaxLetterHeight: Byte;
    function MaxLetterWidth: Byte;
    procedure SaveToFont(const aFilename: string);
    procedure SaveToFontX(const aFilename: string);

    property TexData[aIndex: Integer]: TKMCardinalArray read GetTexData;
    property TexPadding: Byte read fTexPadding write fTexPadding;
    property TexSizeX: Word read fTexSizeX write fTexSizeX;
    property TexSizeY: Word read fTexSizeY write fTexSizeY;
    property IsUnicode: Boolean read fIsUnicode;
    property Codepage: Word read fCodepage;

    //Same as in TKMFontData, but writeable
    property CharSpacing: SmallInt read fCharSpacing write fCharSpacing;
    property LineSpacing: Byte read fLineSpacing write fLineSpacing;
    property BaseHeight: SmallInt read fBaseHeight write fBaseHeight;
    property WordSpacing: SmallInt read fWordSpacing write fWordSpacing;
    property Unknown: SmallInt read fUnknown write fUnknown;
  end;


implementation
uses KM_PNG;

const
  BG_COLOR = $00FFFFFF; //Transparent


{ TKMFontDataEdit }
procedure TKMFontDataEdit.CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; aAntialias: Boolean; const aChars: TWideCharArray);
const
  FONT_INTERLINE = 5; //Spacing between lines of text
var
  bmp:  TBitmap;
  I, J, K, pX, pY: Integer;
  chWidth: Byte;
  chRect: TRect;
  scLine: Cardinal;
  txtHeight: Integer;
  ch: UnicodeString;
begin
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.Width := 64;
    bmp.Height := 64;
    bmp.Canvas.Font.Color := $00FF00; //Only green channel to avoid ClearType antialiasing
    bmp.Canvas.Font.Size := aFontSize;
    bmp.Canvas.Font.Name := aFontName;
    bmp.Canvas.Font.Style := aFontStyle;
    if aAntialias then
      bmp.Canvas.Font.Quality := fqClearType
    else
      bmp.Canvas.Font.Quality := fqNonAntialiased;

    //Common font props
    fBaseHeight := aFontSize;
    fWordSpacing := 4;
    fCharSpacing := 0;
    fLineSpacing := FONT_INTERLINE;

    txtHeight := bmp.Canvas.TextHeight('"_pI|,');

    //Characters we gonna use
    FillChar(Used, SizeOf(Used), #0);
    for I := Low(aChars) to High(aChars) do
      Used[Ord(aChars[I])] := 1;

    //Obtain each characters dimensions (KaM char heights are per-char, so we do the same)
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
    begin
      ch := WideChar(I); //Lazarus needs extra verbose types
      Letters[I].Width := bmp.Canvas.TextWidth(UTF8Encode(ch));
      Letters[I].Height := txtHeight;
    end;

    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

    fAtlasCount := 1;
    pX := fTexPadding;
    pY := fTexPadding;
    SetLength(fAtlases, 0);
    SetLength(fAtlases, fAtlasCount);
    SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
    begin
      chWidth := Letters[I].Width;

      if chWidth = 0 then Continue;

      if pX + chWidth + fTexPadding * 2 >= fTexSizeX then
      begin
        pX := fTexPadding;
        Inc(pY, txtHeight + fTexPadding * 2);
        if pY + txtHeight + fTexPadding * 2 > fTexSizeY then
        begin
          //Append new atlas
          Assert((AtlasCount < 255), 'We hit a limit of 256 atlases');
          Inc(fAtlasCount);
          pX := fTexPadding;
          pY := fTexPadding;
          SetLength(fAtlases, fAtlasCount);
          SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
        end;
      end;

      Letters[I].AtlasId := AtlasCount - 1;
      Letters[I].u1 := (pX) / fTexSizeX;
      Letters[I].v1 := (pY) / fTexSizeY;
      Letters[I].u2 := (pX + chWidth) / fTexSizeX;
      Letters[I].v2 := (pY + txtHeight) / fTexSizeY;

      chRect.Left := 0;
      chRect.Top := 0;
      chRect.Right := chWidth;
      chRect.Bottom := txtHeight;

      //Lazarus needs extra verbose types
      ch := WideChar(I);

      //Draw single letter and copy it to atlas
      bmp.Canvas.TextRect(chRect, 0, 0, UTF8Encode(ch));

      //Only Green will be used to generate the texture to avoid rimming
      for J := 0 to txtHeight - 1 do
      begin
        scLine := Cardinal(bmp.scanline[J]);
        for K := 0 to chWidth - 1 do
          fAtlases[fAtlasCount - 1].TexData[((pY + J) * fTexSizeX + pX + K)] := Cardinal(PByte(scLine + K * 4+1)^ shl 24) or $FFFFFF;
      end;

      Inc(pX, chWidth + fTexPadding * 2);
    end;
  finally
    bmp.Free;
  end;
end;


//Create font by collating several different codepages
procedure TKMFontDataEdit.CollateFonts(aFonts: array of TKMFontDataEdit);
  function AnsiCharToWideChar(ac: AnsiChar; CodePage: Word): WideChar;
  begin
    if MultiByteToWideChar(CodePage, 0, @ac, 1, @Result, 1) <> 1 then
      RaiseLastOSError;
  end;
const
  INS = 0;
var
  I, K, L, M, pX, pY: Integer;
  chWidth, chHeight, lineHeight: Byte;
  srcX, srcY: Word;
  dstPixel, srcPixel: Cardinal;
  uniChar: Char;
  uniCode: Word;
begin
  //Common font properties are presumably the same for all codepages
  fBaseHeight := aFonts[0].BaseHeight;
  fWordSpacing := aFonts[0].WordSpacing;
  fCharSpacing := aFonts[0].CharSpacing;
  fLineSpacing := aFonts[0].LineSpacing;

  //Atlas line height
  lineHeight := 0;
  for K := Low(aFonts) to High(aFonts) do
    lineHeight := Math.max(lineHeight, aFonts[K].MaxLetterHeight);

  //Texture data
  fAtlasCount := 1;
  pX := fTexPadding;
  pY := fTexPadding;
  SetLength(fAtlases, 0);
  SetLength(fAtlases, fAtlasCount);
  SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
  for K := Low(aFonts) to High(aFonts) do
    for I := 0 to aFonts[K].CharCount do
    begin
      if aFonts[K].Used[I] = 0 then Continue;

      if aFonts[K].IsUnicode then
        uniChar := WideChar(I)
      else
        uniChar := AnsiCharToWideChar(AnsiChar(I), aFonts[K].Codepage);

      uniCode := Ord(uniChar);

      //We already have that letter
      if Used[uniCode] <> 0 then Continue;

      chWidth := aFonts[K].Letters[I].Width;
      chHeight := aFonts[K].Letters[I].Height;

      if chWidth = 0 then Continue;

      if pX + chWidth + fTexPadding*2 >= fTexSizeX then
      begin
        pX := fTexPadding;
        Inc(pY, lineHeight + fTexPadding*2);
        if pY + lineHeight + fTexPadding*2 >= fTexSizeY then
        begin
          //Append new atlas
          Assert((AtlasCount < 255), 'We hit a limit of 256 atlases');
          Inc(fAtlasCount);
          pX := fTexPadding;
          pY := fTexPadding;
          SetLength(fAtlases, fAtlasCount);
          SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
        end;
      end;

      //Copy the character over
      for M := 0 to chHeight - 1 do
      for L := 0 to chWidth - 1 do
      begin
        srcX := Round(aFonts[K].Letters[I].u1 * aFonts[K].fTexSizeX);
        srcY := Round(aFonts[K].Letters[I].v1 * aFonts[K].fTexSizeY);
        srcPixel := (srcY + M) * aFonts[K].fTexSizeX + srcX + L;
        dstPixel := (pY + M) * fTexSizeX + pX + L;
        fAtlases[fAtlasCount - 1].TexData[dstPixel] := aFonts[K].TexData[aFonts[K].Letters[I].AtlasId][srcPixel];
      end;

      Used[uniCode] := 1;
      Letters[uniCode].Width := chWidth;
      Letters[uniCode].Height := chHeight;
      Letters[uniCode].YOffset := aFonts[K].Letters[I].YOffset;
      Letters[uniCode].AtlasId := fAtlasCount - 1;
      Letters[uniCode].u1 := (pX + INS) / fTexSizeX;
      Letters[uniCode].v1 := (pY + INS) / fTexSizeY;
      Letters[uniCode].u2 := (pX + chWidth - INS) / fTexSizeX;
      Letters[uniCode].v2 := (pY + chHeight - INS) / fTexSizeY;

      Inc(pX, chWidth + fTexPadding*2);
    end;
end;


procedure TKMFontDataEdit.ExportGridPng(const aFilename: string; aPadding: TRect);
var
  cellX, cellY, pngWidth, pngHeight: Word;
  I, M, L: Integer;
  chWidth, chHeight: Byte;
  srcX, srcY: Word;
  dstPixel, srcPixel: Cardinal;
  data: TKMCardinalArray;
begin
  cellX := MaxLetterWidth + aPadding.Left + aPadding.Right;
  cellY := MaxLetterHeight + aPadding.Top + aPadding.Bottom;
  pngWidth := 256 * cellX;
  pngHeight := 256 * cellY;

  SetLength(data, pngWidth * pngHeight);

  //fill everything so we later cut the letters from it
  for I := 0 to pngWidth * pngHeight - 1 do
    data[I] := BG_COLOR;

  //Draw letters
  for I := 0 to fCharCount - 1 do
  if Used[I] <> 0 then
  begin
    chWidth := Letters[I].Width;
    chHeight := Letters[I].Height;

    //Cut the window for the character
    for M := 0 to chHeight + aPadding.Top + aPadding.Bottom - 1 do
    for L := 0 to chWidth + aPadding.Left + aPadding.Right - 1 do
    begin
      dstPixel := ((I div 256) * cellY + M) * pngWidth + (I mod 256) * cellX + L;
      data[dstPixel] := $00000000;
    end;

    //Copy the character over
    for M := 0 to chHeight - 1 do
    for L := 0 to chWidth - 1 do
    begin
      srcX := Round(Letters[I].u1 * fTexSizeX);
      srcY := Round(Letters[I].v1 * fTexSizeY);
      srcPixel := (srcY + M) * fTexSizeX + srcX + L;
      dstPixel := ((I div 256) * cellY + M + aPadding.Top) * pngWidth + (I mod 256) * cellX + L + aPadding.Left;
      data[dstPixel] := fAtlases[Letters[I].AtlasId].TexData[srcPixel];
    end;
  end;

  SaveToPng(pngWidth, pngHeight, data, aFilename);
end;


function TKMFontDataEdit.GetTexData(aIndex: Integer): TKMCardinalArray;
begin
  Result := fAtlases[aIndex].TexData;
end;


procedure TKMFontDataEdit.ImportGridPng(const aFilename: string);
var
  cellX, cellY, pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
  I, K, M, L: Integer;
  chMaxX, chMaxY: ShortInt;
  cX, cY, dstX, dstY: Word;
  letter: Word;
  lineHeight: Byte;
  dstPixel, srcPixel: Cardinal;
begin
  LoadFromPng(aFilename, pngWidth, pngHeight, pngData);

  //Guess cell size
  cellX := pngWidth div 256;
  cellY := pngHeight div 256;

  Assert((pngWidth > 0) and (pngHeight > 0), 'Imported image should have a size');
  Assert((pngWidth mod 256 = 0) and (pngHeight mod 256 = 0), 'Imported image dimensions should be multiple of 256');

  //Scan all letter-boxes
  for I := 0 to 255 do
  for K := 0 to 255 do
  begin
    chMaxX := -1;
    chMaxY := -1;

    //Scan all pixels of a single letter to determine its dimensions
    for L := 0 to cellY - 1 do
    for M := 0 to cellX - 1 do
    begin
      //Pixel coords
      cX := K * cellX + M;
      cY := I * cellY + L;

      //Ignore fully transparent areas
      if pngData[cY * pngWidth + cX] and $FF000000 > $00 then
      begin
        chMaxX := Math.Max(chMaxX, M);
        chMaxY := Math.Max(chMaxY, L);
      end;
    end;

    letter := I * 256 + K;

    Used[letter] := Byte((chMaxX > 0) and (chMaxY > 0));
    Letters[letter].Width := chMaxX + 1;
    Letters[letter].Height := chMaxY + 1;
  end;

  fAtlasCount := 1;
  dstX := fTexPadding;
  dstY := fTexPadding;
  SetLength(fAtlases, 0);
  SetLength(fAtlases, fAtlasCount);
  SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
  lineHeight := MaxLetterHeight;

  //Pack found letters into atlas
  //Hope that TexSize did not changed
  for I := 0 to fCharCount - 1 do
  if Used[I] <> 0 then
  begin
    if dstX + Letters[I].Width + fTexPadding * 2 >= fTexSizeX then
    begin
      dstX := fTexPadding;
      Inc(dstY, lineHeight + fTexPadding * 2);
      if dstY + lineHeight + fTexPadding * 2 >= fTexSizeY then
      begin
        //Append new atlas
        Assert((AtlasCount < 255), 'We hit a limit of 256 atlases');
        Inc(fAtlasCount);
        dstX := fTexPadding;
        dstY := fTexPadding;
        SetLength(fAtlases, fAtlasCount);
        SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);
      end;
    end;

    //Copy the character over
    for M := 0 to Letters[I].Height - 1 do
    for L := 0 to Letters[I].Width - 1 do
    begin
      srcPixel := (I div 256 * CellY + M) * pngWidth + I mod 256 * CellX + L;
      dstPixel := (dstY + M) * fTexSizeX + dstX + L;
      fAtlases[fAtlasCount - 1].TexData[dstPixel] := pngData[srcPixel];
    end;

    Letters[I].AtlasId := fAtlasCount - 1;
    Letters[I].u1 := (dstX) / fTexSizeX;
    Letters[I].v1 := (dstY) / fTexSizeY;
    Letters[I].u2 := (dstX + Letters[I].Width) / fTexSizeX;
    Letters[I].v2 := (dstY + Letters[I].Height) / fTexSizeY;

    Inc(dstX, Letters[I].Width + fTexPadding * 2);
  end;
end;


procedure TKMFontDataEdit.ImportPng(const aFilename: string; aIndex: Integer);
var
  I, K: Word;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  LoadFromPng(aFilename, pngWidth, pngHeight, pngData);

  Assert((pngWidth = fTexSizeX) and (pngHeight = fTexSizeY), 'Size must match because of letters data');

  for I := 0 to fTexSizeY - 1 do
  for K := 0 to fTexSizeX - 1 do
    (PCardinal(Cardinal(@fAtlases[aIndex].TexData[0]) + (I * fTexSizeX + K) * 4))^ := pngData[I * fTexSizeX + K];
end;


//Maximum letter height, used for atlas generation and export
function TKMFontDataEdit.MaxLetterHeight: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to fCharCount - 1 do
  if Used[I] <> 0 then
    Result := Math.max(Result, Letters[I].Height);
end;


function TKMFontDataEdit.MaxLetterWidth: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to fCharCount - 1 do
  if Used[I] <> 0 then
    Result := Math.max(Result, Letters[I].Width);
end;


procedure TKMFontDataEdit.SaveToFont(const aFilename: string);
const
  Zero: Word = 0;
var
  I: Byte;
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.Write(fBaseHeight, 2);
    S.Write(fWordSpacing, 2);
    S.Write(fCharSpacing, 2);
    S.Write(fUnknown, 2); //Unknown field

    S.Write(Used[0], 256);

    //Write font data
    for I := 0 to 255 do
    if Used[I] <> 0 then
    begin
      S.Write(Letters[I].Width, 2);
      S.Write(Letters[I].Height, 2);
      S.Write(Letters[I].AtlasId, 2); //was Unknown field
      S.Write(Zero, 2); //Unknown field
      S.Write(Letters[I].YOffset, 2);
      S.Write(Zero, 2); //Unknown field

      S.Write(rawData[I,0], Letters[I].Width * Letters[I].Height);
    end;

    S.SaveToFile(aFilename);
  finally
    S.Free;
  end;
end;


//Save font in extended format (with unicode and 32bit support)
procedure TKMFontDataEdit.SaveToFontX(const aFilename: string);
const
  FNTX_HEAD: AnsiString = 'FNTX';
var
  I: Integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  InputStream := TMemoryStream.Create;
  try
    //Header
    InputStream.Write(FNTX_HEAD[1], 4);

    //Base font properties
    InputStream.Write(fBaseHeight, 2);
    InputStream.Write(fWordSpacing, 2);
    InputStream.Write(fCharSpacing, 2);
    InputStream.Write(fLineSpacing, 1);

    //Letters data
    InputStream.Write(Used[0], Length(Used) * SizeOf(Used[0]));
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
      InputStream.Write(Letters[I], SizeOf(TKMLetter));

    //Texture data
    InputStream.Write(fAtlasCount, 1);
    InputStream.Write(fTexSizeX, 2);
    InputStream.Write(fTexSizeY, 2);
    for I := 0 to fAtlasCount - 1 do
      InputStream.Write(fAtlases[I].TexData[0], fTexSizeX * fTexSizeY * 4);

    OutputStream := TFileStream.Create(aFileName, fmCreate);
    CompressionStream := TCompressionStream.Create(clMax, OutputStream);
    try
      InputStream.Position := 0;
      CompressionStream.CopyFrom(InputStream, InputStream.Size);
    finally
      CompressionStream.Free;
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;


end.
