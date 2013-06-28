unit KM_ResourceFonts;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, Math, SysUtils, Types, PngImage,
  KM_Defaults, KM_Points, KM_Render, KM_ResourcePalettes;


type
  TKMFont = (fnt_Antiqua, fnt_Game, fnt_Grey,
    fnt_Metal, fnt_Mini, fnt_Outline, fntx_ArialUni);
  {
  Removed fonts that were in KaM:
  Adam (unused)
  Briefing (same typeface as Antiqua, just darker)
  Font01 (damaged)
  KMLobby (used for internet lobby in TPR)
  MainA (identical to MainMapGold in all game versions)
  MainA.old (probably never meant to be included in the release anyway)
  MainB (identical to Game)
  MainMapGold (same typeface as Metal, just with a goldish tint)
  Minimum (same as mini but with less characters)
  System (unused)
  Won (same typeface as Metal, just with a blueish tint)
  }

  TKMFontInfo = record
      FontFile: string;
      Pal: TKMPal; //Palette fnt needs
      TexMode: TTexFormat; //Format font texture needs to be in
    end;

  TKMLetter = packed record
      Width, Height, YOffset: Word;
      Unknown1, Unknown2, Unknown3: Word;
      u1,v1,u2,v2: Single; //Location within texture atlas
    end;

  TKMFontData = class
  protected
    fTexID: Cardinal;
    fTexData: array of Cardinal;
    fTexSizeX, fTexSizeY: Word;
    fBaseHeight, fWordSpacing, fCharSpacing, fUnknown: SmallInt;
    fLineSpacing: Byte; //Not in KaM files, we use custom value that fits well
    Used: array [0..High(Word)] of Byte;
    rawData: array [0..255] of array of Byte; //Raw data for ANSI fonts
  public
    Letters: array [0..High(Word)] of TKMLetter;

    procedure LoadFont(const aFileName: string; aPal: TKMPalData);
    procedure LoadFontX(const aFileName: string);
    procedure GenerateTexture(aRender: TRender; aTexMode: TTexFormat);
    procedure Compact;
    procedure ExportBimap(aBitmap: TBitmap; aOnlyAlpha: Boolean); overload;
    procedure ExportBimap(const aPath: string; aOnlyAlpha: Boolean); overload;
    procedure ExportPng(const aPath: string);

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
    function GetTextSize(const aText: string; Fnt: TKMFont): TKMPoint;

    procedure LoadFonts(aCodePage: Word);
    procedure ExportFonts(aCodePage: Word);
  end;


const
  FontInfo: array [TKMFont] of TKMFontInfo = (
    (FontFile: 'antiqua';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'game';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'grey';        Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'metal';       Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'mini';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'outline';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'arialuni';    Pal: pal_0;         TexMode: tf_Alpha8)
  );


implementation
uses KM_Resource;


{ TKMFontData }
procedure TKMFontData.LoadFont(const aFileName: string; aPal: TKMPalData);
const
  TEX_SIZE = 256; //Static texture size, all KaM fonts fit within 256^2 space
  FONT_INTERLINE = 5; //Spacing between lines of text
  PAD = 1;
var
  S: TMemoryStream;
  I, M, L: Integer;
  MaxHeight: Integer;
  pX, pY: Integer;
begin
  MaxHeight := 0;
  if not FileExists(aFileName) then
    Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(aFileName);

  S.Read(fBaseHeight, 2);
  S.Read(fWordSpacing, 2);
  S.Read(fCharSpacing, 2);
  S.Read(fUnknown, 2); //Unknown field
  fLineSpacing := FONT_INTERLINE;

  S.Read(Used[0], 256);

  //Read font data
  for I := 0 to 255 do
  if Used[I] <> 0 then
  begin
    S.Read(Letters[I].Width, 2);
    S.Read(Letters[I].Height, 2);
    S.Read(Letters[I].Unknown1, 2); //Unknown field
    S.Read(Letters[I].Unknown2, 2); //Unknown field
    S.Read(Letters[I].YOffset, 2);
    S.Read(Letters[I].Unknown3, 2); //Unknown field

    MaxHeight := Math.max(MaxHeight, Letters[I].Height);

    if Letters[I].Width * Letters[I].Height = 0 then
      Assert('Font data Width * Height = 0'); //Font01.fnt seems to be damaged..

    SetLength(rawData[I], Letters[I].Width*Letters[I].Height);
    S.Read(rawData[I,0], Letters[I].Width*Letters[I].Height);
  end;
  S.Free;

  //Compile texture
  pX := PAD;
  pY := PAD;
  fTexSizeX := TEX_SIZE;
  fTexSizeY := TEX_SIZE;
  SetLength(fTexData, fTexSizeX * fTexSizeY);

  for I := 0 to 255 do
  if Used[I] <> 0 then
  begin
    //Switch to new line
    if pX + Letters[I].Width + PAD > fTexSizeX then
    begin
      pX := PAD;
      Inc(pY, MaxHeight + PAD);
    end;

    //Fill in colors
    for L := 0 to Letters[I].Height - 1 do
    for M := 0 to Letters[I].Width - 1 do
      fTexData[(pY + L) * fTexSizeX + pX + M] :=
        aPal.Color32(rawData[I, L * Letters[I].Width + M]);

    Letters[I].u1 := pX / fTexSizeX;
    Letters[I].v1 := pY / fTexSizeY;
    Letters[I].u2 := (pX + Letters[I].Width) / fTexSizeX;
    Letters[I].v2 := (pY + Letters[I].Height) / fTexSizeY;

    Inc(pX, Letters[I].Width + PAD);
  end;
end;


procedure TKMFontData.LoadFontX(const aFileName: string);
const
  FNTX_HEAD: AnsiString = 'FNTX';
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

    Assert(Head = FNTX_HEAD);

    S.Read(fBaseHeight, 2);
    S.Read(fWordSpacing, 2);
    S.Read(fCharSpacing, 2);
    S.Read(fLineSpacing, 1);

    S.Read(Used[0], Length(Used) * SizeOf(Used[0]));
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
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

  aBitmap.Canvas.Brush.Style := bsClear;
  aBitmap.Canvas.Pen.Color := clAqua;
  for I := 0 to High(Word) do
  if Used[I] <> 0 then
  begin
      aBitmap.Canvas.Rectangle(Round(Letters[I].u1 * fTexSizeX),
        Round(Letters[I].v1 * fTexSizeY), Round(Letters[I].u2 * fTexSizeX),
        Round(Letters[I].v2 * fTexSizeY));
  end;
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


procedure TKMResourceFont.LoadFonts(aCodePage: Word);
var
  F: TKMFont;
  FntPath: string;
begin
  for F := Low(TKMFont) to High(TKMFont) do
  begin
    {$IFDEF UNICODE}
      FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fntx';
      fFontData[F].LoadFontX(FntPath);
    {$ENDIF}

    {$IFNDEF UNICODE}
      FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.' + aCodePage + '.fnt';
      if not FileExists(FntPath) then
        FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fnt';

      fFontData[F].LoadFont(FntPath, fResource.Palettes[FontInfo[F].Pal]);
    {$ENDIF}

    fFontData[F].GenerateTexture(fRender, FontInfo[F].TexMode);
    fFontData[F].Compact;
  end;
end;


procedure TKMResourceFont.ExportFonts(aCodePage: Word);
var
  F: TKMFont;
  FntFront: string;
begin
  //We need to reload fonts to regenerate TexData
  for F := Low(TKMFont) to High(TKMFont) do
  begin
    FntFront := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile;

    if FileExists(FntFront + '.' + IntToStr(aCodePage) + '.fnt') then
      fFontData[F].LoadFont(FntFront + '.' + IntToStr(aCodePage) + '.fnt', fResource.Palettes[FontInfo[F].Pal])
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


function TKMResourceFont.GetTextSize(const aText: string; Fnt: TKMFont): TKMPoint;
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
  while I <= Length(aText) do
  begin
    //Ignore color markups [$FFFFFF][]
    if (aText[I]='[') and (I+1 <= Length(aText)) and (aText[I+1]=']') then
      Inc(I) //Skip past this markup
    else
      if (aText[I]='[') and (I+8 <= Length(aText))
      and (aText[I+1] = '$') and (aText[I+8]=']')
      and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
        Inc(I,8) //Skip past this markup
      else
        if aText[I] <> #124 then
          if aText[I] = #32 then
            Inc(LineWidth[LineCount], fFontData[Fnt].WordSpacing)
          else
            Inc(LineWidth[LineCount], fFontData[Fnt].Letters[Ord(aText[I])].Width + CharSpacing);

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
