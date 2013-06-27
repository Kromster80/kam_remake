unit KM_ResourceFontsEdit;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, Math, SysUtils, Types, PngImage,
  KM_ResourceFonts;


type
  //Child class that has the advanced editing methods
  TKMFontDataEdit = class(TKMFontData)
  private
    fTexPadding: Byte;
  public
    procedure CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; const aChars: array of Char);
    procedure CollateFont(aFonts: array of TKMFontDataEdit; aCodepages: array of Word);
    procedure ImportPng(const aPath: string);
    procedure SaveToFontX(const aFilename: string);

    property TexPadding: Byte read fTexPadding write fTexPadding;
    property TexSizeX: Word read fTexSizeX write fTexSizeX;
    property TexSizeY: Word read fTexSizeY write fTexSizeY;
  end;


implementation


{ TKMFontDataEdit }
procedure TKMFontDataEdit.CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; const aChars: array of Char);
const
  INS = 0;
  FONT_INTERLINE = 5; //Spacing between lines of text
var
  bitmap: TBitmap;
  I, K, pX, pY: Integer;
  chWidth: Byte;
  chRect: TRect;
  byteArray: PByteArray;
  txtHeight: Integer;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.PixelFormat := pf32bit;
    bitmap.Width := fTexSizeX;
    bitmap.Height := fTexSizeY;
    bitmap.Canvas.Font.Color := clWhite;
    bitmap.Canvas.Font.Size := aFontSize;
    bitmap.Canvas.Font.Name := aFontName;
    bitmap.Canvas.Font.Style := aFontStyle;

    //Common font props
    fBaseHeight := aFontSize;
    fWordSpacing := 4;
    fCharSpacing := 0;
    fLineSpacing := FONT_INTERLINE;

    txtHeight := bitmap.Canvas.TextHeight('"_pI|,');

    //Characters we gonna use
    FillChar(Used, SizeOf(Used), #0);
    for I := Low(aChars) to High(aChars) do
      Used[Word(aChars[I])] := 1;

    //Obtain each characters dimensions (KaM char heights are per-char, so we do the same)
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
    begin
      Letters[I].Width := bitmap.Canvas.TextWidth(Char(I));
      Letters[I].Height := txtHeight;
    end;

    bitmap.Canvas.Brush.Style := bsSolid;
    bitmap.Canvas.Brush.Color := clBlack;
    bitmap.Canvas.FillRect(Rect(0, 0, fTexSizeX, fTexSizeY));

    pX := fTexPadding;
    pY := fTexPadding;
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
    begin
      chWidth := Letters[I].Width;

      if chWidth = 0 then Continue;

      if pX + chWidth + fTexPadding >= fTexSizeX then
      begin
        pX := fTexPadding;
        Inc(pY, txtHeight + fTexPadding);
        if pY + txtHeight + fTexPadding > fTexSizeY then
          Break;
      end;

      Letters[I].u1 := (pX + INS) / fTexSizeX;
      Letters[I].v1 := (pY + INS) / fTexSizeY;
      Letters[I].u2 := (pX + chWidth - INS) / fTexSizeX;
      Letters[I].v2 := (pY + txtHeight - INS) / fTexSizeY;

      chRect.Left := pX;
      chRect.Top := pY;
      chRect.Right := pX + chWidth;
      chRect.Bottom := pY + txtHeight;
      bitmap.Canvas.TextRect(chRect, pX, pY, Char(I));

      Inc(pX, chWidth + fTexPadding);
    end;

    SetLength(fTexData, fTexSizeX * fTexSizeY);
    for I := 0 to bitmap.Height - 1 do
    begin
      //Only Alpha will be used to generate the texture
      byteArray := bitmap.ScanLine[I];
      for K := 0 to bitmap.Width - 1 do
        fTexData[(I * bitmap.Width + K)] := byteArray[K * 4 + 1] shl 24 or $FFFFFF;
    end;
  finally
    bitmap.Free;
  end;
end;


//Create font by collating several different codepages
procedure TKMFontDataEdit.CollateFont(aFonts: array of TKMFontDataEdit; aCodepages: array of Word);
const
  INS = 0;
var
  I, K, L, M, pX, pY: Integer;
  chWidth, chHeight, MaxHeight: Byte;
  srcX, srcY: Word;
  dstPixel, srcPixel: Cardinal;
  anChar: AnsiString;
  uniChar: Char;
  uniCode: Word;
  Tmp: RawByteString;
begin
  //Common font props
  fBaseHeight := aFonts[0].BaseHeight;
  fWordSpacing := aFonts[0].WordSpacing;
  fCharSpacing := aFonts[0].CharSpacing;
  fLineSpacing := aFonts[0].LineSpacing;

  //Atlas line height
  MaxHeight := 0;
  for I := 0 to 255 do
  if aFonts[0].Used[I] <> 0 then
    MaxHeight := Math.max(MaxHeight, aFonts[0].Letters[I].Height);

  //Texture data
  SetLength(fTexData, fTexSizeX * fTexSizeY);

  pX := fTexPadding;
  pY := fTexPadding;
  for K := Low(aFonts) to High(aFonts) do
    for I := 0 to 255 do
    begin
      if aFonts[K].Used[I] = 0 then Continue;

      anChar := AnsiChar(I);
      Tmp := anChar;
      SetCodePage(Tmp, aCodepages[K], False);
      uniChar := UnicodeString(Tmp)[1];
      uniCode := Word(uniChar);

      //We already have that letter
      if Used[uniCode] <> 0 then Continue;

      chWidth := aFonts[K].Letters[I].Width;
      chHeight := aFonts[K].Letters[I].Height;

      if chWidth = 0 then Continue;

      if pX + chWidth + fTexPadding >= fTexSizeX then
      begin
        pX := fTexPadding;
        Inc(pY, MaxHeight + fTexPadding);
        if pY + MaxHeight + fTexPadding >= fTexSizeY then
          Exit;
      end;

      //Copy the character over
      for M := 0 to chHeight - 1 do
      for L := 0 to chWidth - 1 do
      begin
        srcX := Round(aFonts[K].Letters[I].u1 * aFonts[K].fTexSizeX);
        srcY := Round(aFonts[K].Letters[I].v1 * aFonts[K].fTexSizeY);
        srcPixel := (srcY + M) * aFonts[K].fTexSizeX + srcX + L;
        dstPixel := (pY + M) * fTexSizeX + pX + L;
        fTexData[dstPixel] := aFonts[K].fTexData[srcPixel];
      end;

      Used[uniCode] := 1;
      Letters[uniCode].Width := chWidth;
      Letters[uniCode].Height := chHeight;
      Letters[uniCode].YOffset := aFonts[K].Letters[I].YOffset;
      Letters[uniCode].u1 := (pX + INS) / fTexSizeX;
      Letters[uniCode].v1 := (pY + INS) / fTexSizeY;
      Letters[uniCode].u2 := (pX + chWidth - INS) / fTexSizeX;
      Letters[uniCode].v2 := (pY + chHeight - INS) / fTexSizeY;

      Inc(pX, chWidth + fTexPadding);
    end;
end;


procedure TKMFontDataEdit.ImportPng(const aPath: string);
begin

end;


//Save font in extended format (with unicode and 32bit support)
procedure TKMFontDataEdit.SaveToFontX(const aFilename: string);
const
  FNTX_HEAD: AnsiString = 'FNTX';
var
  S: TMemoryStream;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    //Header
    S.Write(FNTX_HEAD[1], 4);

    //Base font properties
    S.Write(fBaseHeight, 2);
    S.Write(fWordSpacing, 2);
    S.Write(fCharSpacing, 2);
    S.Write(fLineSpacing, 1);

    //Letters data
    S.Write(Used[0], Length(Used) * SizeOf(Used[0]));
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
      S.Write(Letters[I], SizeOf(TKMLetter));

    //Texture data
    S.Write(fTexSizeX, 2);
    S.Write(fTexSizeY, 2);
    S.Write(fTexData[0], fTexSizeX * fTexSizeY * 4);

    S.SaveToFile(aFilename);
  finally
    S.Free;
  end;
end;


end.
