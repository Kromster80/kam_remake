unit KM_ResourceFonts;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, Math, SysUtils, Types, PngImage,
  KM_Defaults, KM_Points, KM_Render, KM_ResourcePalettes;


type
  TKMFont = (fnt_Antiqua, fnt_Briefing, fnt_Game, fnt_Grey, fnt_MainB, fnt_MainMapGold,
    fnt_Metal, fnt_Mini, fnt_Outline, fnt_Won, fntx_ArialUni);
  {
  Removed fonts that were in KaM:
  Adam (unused)
  Font01 (damaged)
  KMLobby (used for internet lobby in TPR)
  MainA (identical to MainMapGold in all game versions)
  MainA.old (probably never meant to be included in the release anyway)
  Minimum (same as mini but with less characters)
  System (unused)
  }

  TKMFontInfo = record
      FontFile: string;
      Pal: TKMPal; //Palette fnt needs
      Ext: Boolean;
      TexMode: TTexFormat; //Format font texture needs to be in
    end;

  TKMLetter = packed record
      Width, Height, YOffset: Word;
      u1,v1,u2,v2: Single; //Location within texture atlas
    end;

  TKMFontData = class
  private
    fTexID: Cardinal;
    fTexData: array of Cardinal;
    fTexSizeX, fTexSizeY: Word;
    fTexPadding: Byte;
    fBaseHeight, fWordSpacing, fCharSpacing: SmallInt; //BaseCharHeight?, Unknown, CharSpacingX, LineOffset?
    Pal: array [0..High(Word)] of Byte;
    fLineSpacing: Byte; //Not in KaM files, we use custom value that fits well
  public
    Letters: array [0..High(Word)] of TKMLetter;

    procedure CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; const aChars: array of Char);
    procedure CollateFont(aFonts: array of TKMFontData; aCodepages: array of Word);
    procedure LoadFont(const aFileName: string; aPal: TKMPalData);
    procedure LoadFontX(const aFileName: string);
    procedure GenerateTexture(aRender: TRender; aTexMode: TTexFormat);
    procedure Compact;
    procedure ExportBimap(aBitmap: TBitmap; aOnlyAlpha: Boolean); overload;
    procedure ExportBimap(const aPath: string; aOnlyAlpha: Boolean); overload;
    procedure ExportPng(const aPath: string);
    procedure ImportPng(const aPath: string);
    procedure SaveToFontX(const aFilename: string);

    property TexPadding: Byte read fTexPadding write fTexPadding;
    property TexSizeX: Word read fTexSizeX write fTexSizeX;
    property TexSizeY: Word read fTexSizeY write fTexSizeY;

    property CharSpacing: SmallInt read fCharSpacing;
    property LineSpacing: Byte read fLineSpacing;
    property BaseHeight: SmallInt read fBaseHeight;
    property WordSpacing: SmallInt read fWordSpacing;
    property TexID: Cardinal read fTexID;
  end;


  //Collection of fonts
  TKMResourceFont = class
  private
    fRender: TRender;
    fFontData: array [TKMFont] of TKMFontData;
    function GetFontData(aIndex: TKMFont): TKMFontData;
  public
    constructor Create(aRender: TRender);
    destructor Destroy; override;

    property FontData[aIndex: TKMFont]: TKMFontData read GetFontData;

    function WordWrap(aText: AnsiString; aFont: TKMFont; aMaxPxWidth: Integer; aForced: Boolean; aIndentAfterNL: Boolean): AnsiString;
    function CharsThatFit(const aText: AnsiString; aFont: TKMFont; aMaxPxWidth: integer): integer;
    function GetTextSize(const aText: AnsiString; Fnt: TKMFont): TKMPoint;

    procedure LoadFonts(aCodePage: AnsiString);
    procedure ExportFonts(aCodePage: AnsiString);
  end;


const
  FontInfo: array [TKMFont] of TKMFontInfo = (
    (FontFile: 'antiqua';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'briefing';    Pal: pal_map;       TexMode: tf_RGB5A1),
    (FontFile: 'game';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'grey';        Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'mainb';       Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'mainmapgold'; Pal: pal2_mapgold;  TexMode: tf_RGB5A1),
    (FontFile: 'metal';       Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'mini';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'outline';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'won';         Pal: pal_set2;      TexMode: tf_RGB5A1),
    (FontFile: 'arialuni';    Pal: pal_0;         Ext: True; TexMode: tf_Alpha8)
  );


implementation
uses KM_Resource;


const
  FONT_INTERLINE = 5; //Spacing between lines of text


{ TKMFontData }
procedure TKMFontData.CreateFont(aFontName: string; aFontSize: Byte; aFontStyle: TFontStyles; const aChars: array of Char);
const
  INS = 0;
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
    FillChar(Pal, SizeOf(Pal), #0);
    for I := Low(aChars) to High(aChars) do
      Pal[Word(aChars[I])] := 1;

    //Obtain each characters dimensions (KaM char heights are per-char, so we do the same)
    for I := 0 to High(Word) do
    if Pal[I] <> 0 then
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
    if Pal[I] <> 0 then
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
procedure TKMFontData.CollateFont(aFonts: array of TKMFontData; aCodepages: array of Word);
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
  fBaseHeight := aFonts[0].fBaseHeight;
  fWordSpacing := aFonts[0].fWordSpacing;
  fCharSpacing := aFonts[0].fCharSpacing;
  fLineSpacing := aFonts[0].fLineSpacing;

  MaxHeight := 0;
  for I := 0 to 255 do
  if aFonts[0].Pal[I] <> 0 then
    MaxHeight := Math.max(MaxHeight, aFonts[0].Letters[I].Height);

  SetLength(fTexData, fTexSizeX * fTexSizeY);

  pX := fTexPadding;
  pY := fTexPadding;
  for K := Low(aFonts) to High(aFonts) do
    for I := 0 to 255 do
    begin
      if aFonts[K].Pal[I] = 0 then Continue;

      anChar := AnsiChar(I);
      Tmp := anChar;
      SetCodePage(Tmp, aCodepages[K], False);
      uniChar := UnicodeString(Tmp)[1];
      uniCode := Word(uniChar);

      //We already have that letter
      if Letters[uniCode].Width <> 0 then Continue;

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
        dstPixel := (pY + fTexPadding + M) * fTexSizeX + pX + fTexPadding + L;
        fTexData[dstPixel] := aFonts[K].fTexData[srcPixel];
      end;

      Letters[uniCode].Width := chWidth;
      Letters[uniCode].Height := chHeight;
      Letters[uniCode].u1 := (pX + INS) / fTexSizeX;
      Letters[uniCode].v1 := (pY + INS) / fTexSizeY;
      Letters[uniCode].u2 := (pX + chWidth - INS) / fTexSizeX;
      Letters[uniCode].v2 := (pY + MaxHeight - INS) / fTexSizeY;

      Inc(pX, chWidth + fTexPadding);
    end;
end;


procedure TKMFontData.LoadFont(const aFileName: string; aPal: TKMPalData);
const TEX_SIZE = 256;
var
  S: TMemoryStream;
  I, K: Integer;
  pX, pY: Integer;
  MaxHeight: Integer;
  AdvX, AdvY: Integer;
  rawData: array [0..255] of array of Byte;
begin
  MaxHeight := 0;
  if not FileExists(aFileName) then
    Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(aFileName);

  S.Read(fBaseHeight, 2);
  S.Read(fWordSpacing, 2);
  S.Read(fCharSpacing, 2);
  S.Seek(2, soFromCurrent); //Skip Unknown field
  S.Read(Pal[0], 256);

  //Read font data
  for I := 0 to 255 do
  if Pal[I] <> 0 then
  begin
    S.Read(Letters[I].Width, 2);
    S.Read(Letters[I].Height, 2);
    S.Seek(2, soFromCurrent); //Skip Unknown field
    S.Seek(2, soFromCurrent); //Skip Unknown field
    S.Read(Letters[I].YOffset, 2);
    S.Seek(2, soFromCurrent); //Skip Unknown field

    MaxHeight := Math.max(MaxHeight, Letters[I].Height);

    if Letters[I].Width * Letters[I].Height = 0 then
      Assert('Font data Width * Height = 0'); //Font01.fnt seems to be damaged..

    SetLength(rawData[I], Letters[I].Width*Letters[I].Height);
    S.Read(rawData[I,0], Letters[I].Width*Letters[I].Height);
  end;
  S.Free;

  fLineSpacing := FONT_INTERLINE;

  //Compile texture
  AdvX := 0;
  AdvY := 0;
  fTexSizeX := TEX_SIZE;
  fTexSizeY := TEX_SIZE;
  SetLength(fTexData, fTexSizeX * fTexSizeY);

  for I := 0 to 255 do
  if Pal[I] <> 0 then
  begin
    //Switch to new line
    if AdvX+Letters[I].Width+2 > fTexSizeX then
    begin
      AdvX := 0;
      Inc(AdvY, MaxHeight);
    end;

    //Fill in colors
    for pY := 0 to Letters[I].Height - 1 do
    for pX := 0 to Letters[I].Width - 1 do
      fTexData[(AdvY+pY)*fTexSizeX+AdvX+1+pX] := aPal.Color32(rawData[I, pY*Letters[I].Width+pX]);

    Letters[I].u1 := (AdvX + 1) / fTexSizeX;
    Letters[I].v1 := AdvY / fTexSizeY;
    Letters[I].u2 := (AdvX + 1 + Letters[I].Width) / fTexSizeX;
    Letters[I].v2 := (AdvY + Letters[I].Height) / fTexSizeY;

    Inc(AdvX, 1 + Letters[I].Width + 1);
  end;
end;


procedure TKMFontData.LoadFontX(const aFileName: string);
var
  S: TMemoryStream;
  Head: AnsiString;
  I: Integer;
begin
  if not FileExists(aFileName) then Exit;

  S := TMemoryStream.Create;
  try
    S.LoadFromFile(aFileName);

    SetLength(Head, 4);
    S.Read(Head[1], 4);

    Assert(Head = 'FNTX');

    S.Read(fBaseHeight, 1);
    S.Read(fWordSpacing, 1);
    S.Read(fCharSpacing, 1);
    S.Read(fLineSpacing, 1);

    S.Read(Pal[0], Length(Pal) * SizeOf(Pal[0]));
    for I := 0 to High(Word) do
    if Pal[I] <> 0 then
      S.Read(Letters[I], SizeOf(TKMLetter));

    S.Read(fTexSizeX, 2);
    S.Read(fTexSizeY, 2);
    SetLength(fTexData, fTexSizeX * fTexSizeY);
    S.Read(fTexData[0], fTexSizeX * fTexSizeY * 4);
  finally
    S.Free;
  end;
end;


//After font has been loaded and texture generated we can flush temp data
procedure TKMFontData.Compact;
begin
  //Discard texture data to save mem
  SetLength(fTexData, 0);
  fTexSizeX := 0;
  fTexSizeY := 0;
end;


//Generate color texture from prepared data
procedure TKMFontData.GenerateTexture(aRender: TRender; aTexMode: TTexFormat);
begin
  if Length(fTexData) = 0 then Exit;

  fTexID := aRender.GenTexture(fTexSizeX, fTexSizeY, @fTexData[0], aTexMode);
end;


//Export texture data into bitmap
procedure TKMFontData.ExportBimap(aBitmap: TBitmap; aOnlyAlpha: Boolean);
var
  I, K: Integer;
begin
  Assert(Length(fTexData) > 0, 'There is no font data in memory');

  aBitmap.PixelFormat := pf32bit;
  aBitmap.Width  := fTexSizeX;
  aBitmap.Height := fTexSizeY;

  if aOnlyAlpha then
    for I := 0 to fTexSizeY - 1 do
    for K := 0 to fTexSizeX - 1 do
      aBitmap.Canvas.Pixels[K, I] := (fTexData[(I * fTexSizeX + K)] shr 24) * 65793
  else
    for I := 0 to fTexSizeY - 1 do
    for K := 0 to fTexSizeX - 1 do
      aBitmap.Canvas.Pixels[K,I]:= fTexData[I * fTexSizeX + K] and $FFFFFF;
end;


//Export texture data into a bitmap file
procedure TKMFontData.ExportBimap(const aPath: string; aOnlyAlpha: Boolean);
var
  exportBmp: TBitmap;
begin
  Assert(Length(fTexData) > 0, 'There is no font data in memory');

  exportBmp := TBitMap.Create;
  try
    ExportBimap(exportBmp, aOnlyAlpha);

    ForceDirectories(ExtractFilePath(aPath));
    exportBmp.SaveToFile(aPath);
  finally
    exportBmp.Free;
  end;
end;


procedure TKMFontData.ExportPng(const aPath: string);
var
  png: TPngObject;
  I, K: Integer;
  T: Cardinal;
begin
  Assert(Length(fTexData) > 0, 'There is no font data in memory');

  Png := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, fTexSizeX, fTexSizeY);
  try
    for I := 0 to fTexSizeY - 1 do
    for K := 0 to fTexSizeX - 1 do
    begin
      T := (PCardinal(Cardinal(@fTexData[0]) + (I * fTexSizeX + K) * 4))^;
      Png.Canvas.Pixels[K, I] := T and $FFFFFF; //Ignore alpha
      Png.AlphaScanline[I]^[K] := T shr 24; //Alpha
    end;

    Png.SaveToFile(aPath);
  finally
    Png.Free;
  end;
end;


procedure TKMFontData.ImportPng(const aPath: string);
begin

end;


//Save font in extended format (with unicode and 32bit support)
procedure TKMFontData.SaveToFontX(const aFilename: string);
const
  Head: AnsiString = 'FNTX';
var
  S: TMemoryStream;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    //Header
    S.Write(Head[1], 4);

    //Base font properties
    S.Write(fBaseHeight, 1);
    S.Write(fWordSpacing, 1);
    S.Write(fCharSpacing, 1);
    S.Write(fLineSpacing, 1);

    //Letters data
    S.Write(Pal[0], Length(Pal) * SizeOf(Pal[0]));
    for I := 0 to High(Word) do
    if Pal[I] <> 0 then
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


{ TResourceFont }
constructor TKMResourceFont.Create(aRender: TRender);
var F: TKMFont;
begin
  inherited Create;
  fRender := aRender;

  for F := Low(TKMFont) to High(TKMFont) do
    fFontData[F] := TKMFontData.Create;
end;


destructor TKMResourceFont.Destroy;
var F: TKMFont;
begin
  for F := Low(TKMFont) to High(TKMFont) do
    fFontData[F].Free;

  inherited;
end;


function TKMResourceFont.GetFontData(aIndex: TKMFont): TKMFontData;
begin
  Result := fFontData[aIndex];
end;


procedure TKMResourceFont.LoadFonts(aCodePage: AnsiString);
var
  F: TKMFont;
  FntPath: string;
begin
  for F := Low(TKMFont) to High(TKMFont) do
  begin
    if not FontInfo[F].Ext then
    begin
      FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.' + aCodePage + '.fnt';
      if not FileExists(FntPath) then
        FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fnt';

      fFontData[F].LoadFont(FntPath, fResource.Palettes[FontInfo[F].Pal]);
    end
    else
      fFontData[F].LoadFontX(ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fntx');

    fFontData[F].GenerateTexture(fRender, FontInfo[F].TexMode);
    fFontData[F].Compact;
  end;
end;


procedure TKMResourceFont.ExportFonts(aCodePage: AnsiString);
var
  F: TKMFont;
  FntFront: string;
begin
  //We need to reload fonts to regenerate TexData
  for F := Low(TKMFont) to High(TKMFont) do
  begin
    FntFront := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile;

    if FileExists(FntFront + '.' + aCodePage + '.fnt') then
      fFontData[F].LoadFont(FntFront + '.' + aCodePage + '.fnt', fResource.Palettes[FontInfo[F].Pal])
    else
      fFontData[F].LoadFont(FntFront + '.fnt', fResource.Palettes[FontInfo[F].Pal]);

    fFontData[F].ExportBimap(ExeDir + 'Export' + PathDelim + 'Fonts' + PathDelim + FontInfo[F].FontFile + '.bmp', False);
    fFontData[F].Compact;
  end;
end;


function TKMResourceFont.WordWrap(aText: AnsiString; aFont: TKMFont; aMaxPxWidth: Integer; aForced: Boolean; aIndentAfterNL: Boolean): AnsiString;
var
  I, CharSpacing, AdvX, PrevX, LastSpace, TmpColor: Integer;
const
  INDENT = '   ';
begin
  Assert(aMaxPxWidth > 0);

  AdvX := 0;
  PrevX := 0;
  LastSpace := -1;
  CharSpacing := fFontData[aFont].CharSpacing; //Spacing between letters, this varies between fonts

  I:=1;
  while I <= length(aText) do
  begin
    //Ignore color markups [$FFFFFF][]
    if (aText[I]='[') and (I+1 <= Length(aText)) and (aText[I+1]=']') then
      inc(I) //Skip past this markup
    else
      if (aText[I]='[') and (I+8 <= Length(aText))
      and (aText[I+1] = '$') and (aText[I+8]=']')
      and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
        inc(I,8) //Skip past this markup
      else
        if aText[I]=#32 then inc(AdvX, fFontData[aFont].WordSpacing)
                        else inc(AdvX, fFontData[aFont].Letters[byte(aText[I])].Width + CharSpacing);

    if (aText[I]=#32) or (aText[I]=#124) then
    begin
      LastSpace := I;
      PrevX := AdvX;
    end;

    //This algorithm is not perfect, somehow line width is not within SizeX, but very rare
    if ((AdvX > aMaxPxWidth)and(LastSpace<>-1))or(aText[I]=#124) then
    begin
      if (aText[I] <> #124) and aIndentAfterNL then
      begin
        Insert(INDENT, aText, LastSpace+1);
        Inc(I, Length(INDENT));
        Inc(AdvX, Length(INDENT)*fFontData[aFont].WordSpacing);
      end;
      aText[LastSpace] := #124; //Replace last whitespace with EOL
      dec(AdvX, PrevX); //Subtract width since replaced whitespace
      LastSpace := -1;
    end;
    //Force an EOL part way through a word
    if aForced and (AdvX > aMaxPxWidth) and (LastSpace = -1) then
    begin
      Insert(#124,aText,I); //Insert an EOL before this character
      AdvX := 0;
      LastSpace := -1;
      if aIndentAfterNL then
      begin
        Insert(INDENT, aText, I+1);
        Inc(I, Length(INDENT));
        Inc(AdvX, Length(INDENT)*fFontData[aFont].WordSpacing);
      end;
    end;
    inc(I);
  end;
  Result := aText;
end;


function TKMResourceFont.CharsThatFit(const aText: AnsiString; aFont: TKMFont; aMaxPxWidth:integer):integer;
var I, CharSpacing, AdvX: Integer;
begin
  AdvX := 0;
  Result := Length(aText);
  CharSpacing := fFontData[aFont].CharSpacing; //Spacing between letters, this varies between fonts

  for I := 1 to length(aText) do
  begin
    if aText[I] = #32 then Inc(AdvX, fFontData[aFont].WordSpacing)
                      else Inc(AdvX, fFontData[aFont].Letters[byte(aText[I])].Width + CharSpacing);

    if (AdvX > aMaxPxWidth) then
    begin
      Result := I - 1; //Previous character fits, this one does not
      Exit;
    end;
  end;
end;


function TKMResourceFont.GetTextSize(const aText: AnsiString; Fnt: TKMFont): TKMPoint;
var
  I: Integer;
  CharSpacing, LineCount, TmpColor: Integer;
  LineWidth: array of Integer; // Some fonts may have negative CharSpacing
begin
  Result.X := 0;
  Result.Y := 0;

  if aText = '' then Exit;

  LineCount := 1;
  for I := 1 to Length(aText) do
    if aText[I] = #124 then Inc(LineCount);

  SetLength(LineWidth, LineCount+2); //1..n+1 (for last line)

  LineCount := 1;
  CharSpacing := fFontData[Fnt].CharSpacing; //Spacing between letters varies between fonts
  I:=1;
  while I <= length(aText) do
  begin
    //Ignore color markups [$FFFFFF][]
    if (aText[I]='[') and (I+1 <= Length(aText)) and (aText[I+1]=']') then
      inc(I) //Skip past this markup
    else
      if (aText[I]='[') and (I+8 <= Length(aText))
      and (aText[I+1] = '$') and (aText[I+8]=']')
      and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
        inc(I,8) //Skip past this markup
      else
        if aText[I] <> #124 then
          if aText[I] = #32 then
            Inc(LineWidth[LineCount], fFontData[Fnt].WordSpacing)
          else
            Inc(LineWidth[LineCount], fFontData[Fnt].Letters[byte(aText[I])].Width + CharSpacing);

    if (aText[I] = #124) or (I = Length(aText)) then
    begin // If EOL or aText end
      LineWidth[LineCount] := Math.max(0, LineWidth[LineCount] - CharSpacing);
      // Remove last interletter space and negate double EOLs
      Inc(LineCount);
    end;
    Inc(I);
  end;

  Dec(LineCount);
  Result.Y := (fFontData[Fnt].BaseHeight + fFontData[Fnt].LineSpacing) * LineCount;
  for I := 1 to LineCount do
    Result.X := Math.max(Result.X, LineWidth[I]);
end;


end.
