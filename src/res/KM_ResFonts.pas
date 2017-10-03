unit KM_ResFonts;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, Math, StrUtils, SysUtils, KromUtils, KM_PNG,
  KM_CommonTypes, KM_Defaults, KM_Points, KM_Render, KM_ResPalettes
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};


const
  TAB_WIDTH = 30;

type
  TKMFont = (fnt_Antiqua, fnt_Game, fnt_Grey,
    fnt_Metal, fnt_Mini, fnt_Outline, fnt_Arial);

  TKMFontLoadLevel = (fll_Full, fll_Minimal);
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
      Width, Height: Word;
      YOffset: SmallInt;
      AtlasId: Word; //Was Unknown field, we use it for multi-atlas fonts to mark the letters location
      u1,v1,u2,v2: Single; //Location within texture atlas
    end;

  TKMFontData = class
  private
    function GetTexID(aIndex: Integer): Cardinal;
  protected
    fTexSizeX, fTexSizeY: Word; //All atlases have same dimensions
    //Character atlases
    fAtlasCount: Byte;
    fAtlases: array of record
      TexID: Cardinal;
      TexData: TKMCardinalArray;
    end;
    fCharCount: Word;
    fBaseHeight, fWordSpacing, fCharSpacing, fUnknown: SmallInt;
    fLineSpacing: Byte; //Not in KaM files, we use custom value that fits well
    fCodepage: Word;
    fIsUnicode: Boolean;
    rawData: array [0..High(Word)] of array of Byte; //Raw data for ANSI fonts
  public
    Used: array [0..High(Word)] of Byte;
    Letters: array [0..High(Word)] of TKMLetter;

    procedure LoadFont(const aFileName: string; aPalette: TKMPaletteInfo);
    procedure LoadFontX(const aFileName: string; aLoadLevel: TKMFontLoadLevel = fll_Full);
    procedure GenerateTextures(aTexMode: TTexFormat);
    procedure Compact;
    procedure ExportAtlasBmp(aBitmap: TBitmap; aIndex: Integer; aShowCells: Boolean); overload;
    procedure ExportAtlasBmp(const aPath: string; aIndex: Integer); overload;
    procedure ExportAtlasPng(const aFilename: string; aIndex: Integer);

    function GetLetter(aChar: WideChar): TKMLetter;
    property AtlasCount: Byte read fAtlasCount;
    property TexID[aIndex: Integer]: Cardinal read GetTexID;

    property CharCount: Word read fCharCount;
    property CharSpacing: SmallInt read fCharSpacing;
    property LineSpacing: Byte read fLineSpacing;
    property BaseHeight: SmallInt read fBaseHeight;
    property WordSpacing: SmallInt read fWordSpacing;

    function GetCharWidth(aChar: WideChar): Integer;
    function WordWrap(aText: UnicodeString; aMaxPxWidth: Integer; aForced: Boolean; aIndentAfterNL: Boolean; aTabWidth: Integer = TAB_WIDTH): UnicodeString;
    function CharsThatFit(const aText: UnicodeString; aMaxPxWidth: Integer; aRound: Boolean = False; aTabWidth: Integer = TAB_WIDTH): Integer;
    function GetTextSize(const aText: UnicodeString; aCountMarkup: Boolean = False; aTabWidth: Integer = TAB_WIDTH): TKMPoint;
    function GetMaxPrintWidthOfStrings(aStrings: array of string): Integer;
  end;


  //Collection of fonts
  TKMResFonts = class
  private
    fLoadLevel: TKMFontLoadLevel;
    fFontData: array [TKMFont] of TKMFontData;
    function GetFontData(aIndex: TKMFont): TKMFontData;
  public
    constructor Create;
    destructor Destroy; override;

    property FontData[aIndex: TKMFont]: TKMFontData read GetFontData; default;
    property LoadLevel: TKMFontLoadLevel read fLoadLevel;
    class function GuessPalette(const aFileName: string): TKMPal;

    procedure LoadFonts(aLoadLevel: TKMFontLoadLevel = fll_Full);
    procedure ExportFonts;
  end;


const
  PLACEHOLDER_CHAR = 0; //Box, used for characters missing from font

  FontInfo: array [TKMFont] of TKMFontInfo = (
    (FontFile: 'antiqua';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'game';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'grey';        Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'metal';       Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'mini';        Pal: pal_bw;        TexMode: tf_Alpha8),
    (FontFile: 'outline';     Pal: pal_0;         TexMode: tf_RGB5A1),
    (FontFile: 'arial';       Pal: pal_0;         TexMode: tf_RGBA8)
  );


implementation
uses
  KM_CommonUtils, KM_Log;


var
  LOG_EXTRA_FONTS: Boolean = False;


{ TKMFontData }
procedure TKMFontData.LoadFont(const aFileName: string; aPalette: TKMPaletteInfo);
const
  TEX_SIZE = 256; //Static texture size, all KaM fonts fit within 256^2 space
  FONT_INTERLINE = 5; //Spacing between lines of text
  PAD = 1;
var
  S: TMemoryStream;
  fileName: string;
  I, K, M, L: Integer;
  MaxHeight: Integer;
  pX, pY: Integer;
begin
  MaxHeight := 0;
  if not FileExists(aFileName) then
    Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(aFileName);

  //Fnt allows to store 256 or 65000 characters, but there's no flag inside, we can test only filesize
  fCharCount := IfThen(S.Size <= 65000, 256, 65000);

  //Try to get the codepage
  fileName := ExtractFileName(aFileName);
  I := Pos('.', fileName);
  K := PosEx('.', fileName, I+1);

  fCodepage := StrToIntDef(Copy(fileName, I+1, K-I-1), 0);
  fIsUnicode := S.Size > 65000;

  S.Read(fBaseHeight, 2);
  S.Read(fWordSpacing, 2);
  S.Read(fCharSpacing, 2);
  S.Read(fUnknown, 2); //Unknown field
  fLineSpacing := FONT_INTERLINE;

  S.Read(Used[0], fCharCount);

  //Read font data
  for I := 0 to fCharCount - 1 do
  if Used[I] <> 0 then
  begin
    S.Read(Letters[I].Width, 2);
    S.Read(Letters[I].Height, 2);
    S.Read(Letters[I].AtlasId, 2); //was Unknown field
    S.Seek(2, soFromCurrent); //Unknown field
    S.Read(Letters[I].YOffset, 2);
    S.Seek(2, soFromCurrent); //Unknown field

    MaxHeight := Math.max(MaxHeight, Letters[I].Height);

    if Letters[I].Width * Letters[I].Height = 0 then
      raise Exception.Create('Font data Width * Height = 0'); //Font01.fnt seems to be damaged..

    SetLength(rawData[I], Letters[I].Width*Letters[I].Height);
    S.Read(rawData[I,0], Letters[I].Width*Letters[I].Height);
  end;
  S.Free;

  //Compile texture
  pX := PAD;
  pY := PAD;
  fTexSizeX := TEX_SIZE * (1 + Byte(fIsUnicode) * 3); //256 / 1024
  fTexSizeY := TEX_SIZE * (1 + Byte(fIsUnicode) * 1); //256 / 512
  fAtlasCount := 1;
  SetLength(fAtlases, 0);
  SetLength(fAtlases, fAtlasCount);
  SetLength(fAtlases[fAtlasCount - 1].TexData, fTexSizeX * fTexSizeY);

  for I := 0 to fCharCount - 1 do
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
      fAtlases[fAtlasCount - 1].TexData[(pY + L) * fTexSizeX + pX + M] :=
        aPalette.Color32(rawData[I, L * Letters[I].Width + M]);

    Letters[I].u1 := pX / fTexSizeX;
    Letters[I].v1 := pY / fTexSizeY;
    Letters[I].u2 := (pX + Letters[I].Width) / fTexSizeX;
    Letters[I].v2 := (pY + Letters[I].Height) / fTexSizeY;

    Inc(pX, Letters[I].Width + PAD);
  end;
end;


procedure TKMFontData.LoadFontX(const aFileName: string; aLoadLevel: TKMFontLoadLevel = fll_Full);
const
  FNTX_HEAD: AnsiString = 'FNTX';
var
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
  Head: AnsiString;
  I: Integer;
begin
  if not FileExists(aFileName) then Exit;

  InputStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);
  try
    SetLength(Head, 4);
    DecompressionStream.Read(Head[1], 4);

    Assert(Head = FNTX_HEAD);

    fCodepage := 0;
    fIsUnicode := True;
    fCharCount := 65535;

    DecompressionStream.Read(fBaseHeight, 2);
    DecompressionStream.Read(fWordSpacing, 2);
    DecompressionStream.Read(fCharSpacing, 2);
    DecompressionStream.Read(fLineSpacing, 1);

    DecompressionStream.Read(Used[0], Length(Used) * SizeOf(Used[0]));
    for I := 0 to High(Word) do
    if Used[I] <> 0 then
      DecompressionStream.Read(Letters[I], SizeOf(TKMLetter));

    DecompressionStream.Read(fAtlasCount, 1);
    DecompressionStream.Read(fTexSizeX, 2);
    DecompressionStream.Read(fTexSizeY, 2);

    if aLoadLevel = fll_Minimal then
    begin
      fAtlasCount := 1; //Only load the first atlas
      for I := 0 to High(Word) do
        Used[I] := Byte(Letters[I].AtlasId = 0); //Only allow letters on first atlas
    end;

    SetLength(fAtlases, fAtlasCount);
    for I := 0 to fAtlasCount - 1 do
    begin
      SetLength(fAtlases[I].TexData, fTexSizeX * fTexSizeY);
      DecompressionStream.Read(fAtlases[I].TexData[0], fTexSizeX * fTexSizeY * 4);
    end;
  finally
    DecompressionStream.Free;
    InputStream.Free;
  end;
end;


//After font has been loaded and texture generated we can flush temp data
procedure TKMFontData.Compact;
var
  I: Integer;
begin
  //Discard texture data to save mem
  for I := 0 to fAtlasCount - 1 do
    SetLength(fAtlases[I].TexData, 0);

  fTexSizeX := 0;
  fTexSizeY := 0;
end;


//Generate color texture from prepared data
procedure TKMFontData.GenerateTextures(aTexMode: TTexFormat);
var
  I: Integer;
  TextureRAM: Cardinal;
begin
  TextureRAM := 0;
  for I := 0 to fAtlasCount - 1 do
    if fAtlases[I].TexID = 0 then //Don't load atlases twice if switching from minimal to full
      if Length(fAtlases[I].TexData) <> 0 then
      begin
        fAtlases[I].TexID := TRender.GenTexture(fTexSizeX, fTexSizeY, @fAtlases[I].TexData[0], aTexMode);
        Inc(TextureRAM, fTexSizeX * fTexSizeY * TexFormatSize[aTexMode]);
      end
      else
        fAtlases[I].TexID := 0;

  if LOG_EXTRA_FONTS then
    gLog.AddNoTime( 'Font RAM usage: '+IntToStr(TextureRAM));
end;


function TKMFontData.GetLetter(aChar: WideChar): TKMLetter;
begin
  if Used[Ord(aChar)] <> 0 then
    Result := Letters[Ord(aChar)]
  else
    Result := Letters[PLACEHOLDER_CHAR];
end;


function TKMFontData.GetTexID(aIndex: Integer): Cardinal;
begin
  Result := fAtlases[aIndex].TexID;
end;


//Export texture atlas into bitmap (just for looks)
procedure TKMFontData.ExportAtlasBmp(aBitmap: TBitmap; aIndex: Integer; aShowCells: Boolean);
const
  BG: Integer = $AF6B6B;
var
  I, K: Integer;
  scLine: Cardinal;
  TD: TKMCardinalArray;
  C: Integer;
  A: Byte;
begin
  Assert(Length(fAtlases[aIndex].TexData) > 0, 'There is no font data in memory');

  aBitmap.PixelFormat := pf32bit;
  aBitmap.Width  := fTexSizeX;
  aBitmap.Height := fTexSizeY;

  {$IFDEF WDC}
  //todo: Add Lazarus analog
  TD := fAtlases[aIndex].TexData;
  for I := 0 to fTexSizeY - 1 do
  begin
    scLine := Cardinal(aBitmap.ScanLine[I]);
    for K := 0 to fTexSizeX - 1 do
    begin
      C := TD[I * fTexSizeX + K] and $FFFFFF;
      A := 255 - (TD[I * fTexSizeX + K] shr 24) and $FF;
      //C + (D - C) * A
      PCardinal(scLine + K * 4)^ := ((C and $FF) + ((BG and $FF - C and $FF) * A) div 255) shl 16 +
                                    ((C shr 8 and $FF) + ((BG shr 8 and $FF - C shr 8 and $FF) * A) div 255) shl 8 +
                                    ((C shr 16 and $FF) + ((BG shr 16 and $FF - C shr 16 and $FF) * A) div 255);
    end;
  end;
  {$ENDIF}

  if aShowCells then
  begin
    aBitmap.Canvas.Brush.Style := bsClear;
    aBitmap.Canvas.Pen.Color := clAqua;
    for I := 0 to High(Word) do
    if (Used[I] <> 0) and (Letters[I].AtlasId = aIndex) then
    begin
      //Draw cell outside letter area
      aBitmap.Canvas.Rectangle(Round(Letters[I].u1 * fTexSizeX)-1,
                               Round(Letters[I].v1 * fTexSizeY)-1,
                               Round(Letters[I].u2 * fTexSizeX)+1,
                               Round(Letters[I].v2 * fTexSizeY)+1);
    end;
  end;
end;


//Export texture atlas into a bitmap file (just for looks)
procedure TKMFontData.ExportAtlasBmp(const aPath: string; aIndex: Integer);
var
  exportBmp: TBitmap;
begin
  Assert(Length(fAtlases[aIndex].TexData) > 0, 'There is no font data in memory');

  exportBmp := TBitMap.Create;
  try
    ExportAtlasBmp(exportBmp, aIndex, False);

    ForceDirectories(ExtractFilePath(aPath));
    exportBmp.SaveToFile(aPath);
  finally
    exportBmp.Free;
  end;
end;


procedure TKMFontData.ExportAtlasPng(const aFilename: string; aIndex: Integer);
var
  I, K: Integer;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  Assert(Length(fAtlases[aIndex].TexData) > 0, 'There is no font data in memory');

  pngWidth := fTexSizeX;
  pngHeight := fTexSizeY;
  SetLength(pngData, pngWidth * pngHeight);

  for I := 0 to fTexSizeY - 1 do
  for K := 0 to fTexSizeX - 1 do
    pngData[I * fTexSizeX + K] := (PCardinal(Cardinal(@fAtlases[aIndex].TexData[0]) + (I * fTexSizeX + K) * 4))^;

  SaveToPng(pngWidth,pngHeight, pngData, aFilename);
end;


{ TResourceFont }
constructor TKMResFonts.Create;
var
  F: TKMFont;
begin
  inherited;

  for F := Low(TKMFont) to High(TKMFont) do
    fFontData[F] := TKMFontData.Create;
end;


destructor TKMResFonts.Destroy;
var
  F: TKMFont;
begin
  for F := Low(TKMFont) to High(TKMFont) do
    fFontData[F].Free;

  inherited;
end;


function TKMResFonts.GetFontData(aIndex: TKMFont): TKMFontData;
begin
  Result := fFontData[aIndex];
end;


class function TKMResFonts.GuessPalette(const aFileName: string): TKMPal;
var
  fileName: string;
  filePart: string;
  I: Integer;
  K: TKMFontInfo;
begin
  Result := pal_map;

  fileName := ExtractFileName(aFileName);
  I := Pos('.', fileName);
  filePart := Copy(fileName, 1, I-1);

  for K in FontInfo do
  if K.FontFile = filePart then
    Result := K.Pal;
end;


procedure TKMResFonts.LoadFonts(aLoadLevel: TKMFontLoadLevel = fll_Full);
var
  F: TKMFont;
  FntPath: string;
  StartTime, TotalTime: Cardinal;
begin
  fLoadLevel := aLoadLevel;
  StartTime := TimeGet;

  for F := Low(TKMFont) to High(TKMFont) do
  begin
    FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fntx';
    fFontData[F].LoadFontX(FntPath, aLoadLevel);
    fFontData[F].GenerateTextures(FontInfo[F].TexMode);
    fFontData[F].Compact;
  end;

  TotalTime := GetTimeSince(StartTime);
  gLog.AddTime('Font load took ' + IntToStr(TotalTime) + 'ms');
end;


procedure TKMResFonts.ExportFonts;
var
  F: TKMFont;
  FntPath: string;
  I: Integer;
begin
  //We need to reload fonts to regenerate TexData
  for F := Low(TKMFont) to High(TKMFont) do
  begin
    FntPath := ExeDir + FONTS_FOLDER + FontInfo[F].FontFile + '.fntx';
    fFontData[F].LoadFontX(FntPath);
    for I := 0 to fFontData[F].AtlasCount - 1 do
      fFontData[F].ExportAtlasBmp(ExeDir + 'Export' + PathDelim + 'Fonts' + PathDelim + FontInfo[F].FontFile + IntToStr(I) + '.bmp', I);
    fFontData[F].Compact;
  end;
end;


function TKMFontData.GetCharWidth(aChar: WideChar): Integer;
begin
  if AnsiChar(aChar) in [#124, #9] then
    Result := 0
  else if aChar = #32 then
    Result := WordSpacing
  else
    Result := GetLetter(aChar).Width + CharSpacing;
end;


function TKMFontData.WordWrap(aText: UnicodeString; aMaxPxWidth: Integer; aForced: Boolean; aIndentAfterNL: Boolean; aTabWidth: Integer = TAB_WIDTH): UnicodeString;
const
  INDENT = '   ';
var
  I: Integer;
  LastWrappable: Integer;
  LastWrappableIsSpace: Boolean;
  dx, PrevX: Integer;
  TmpColor: Integer;
begin
  Assert(aMaxPxWidth > 0);

  dx := 0;
  PrevX := 0;
  LastWrappable := -1;
  LastWrappableIsSpace := False;

  I := 1;
  while I <= Length(aText) do
  begin
    //Chinese/Japanese characters (not punctuation) can always be wrapped before
    //Check this before we update dx since we are allowing wrapping before this char
    if ((Ord(aText[I]) >= 19968) and (Ord(aText[I]) <= 40870))
      or ((Ord(aText[I]) >= $3040) and (Ord(aText[I]) <= $30ff)) then
    begin
      LastWrappable := I;
      PrevX := dx; //dx does not include this char yet, since we are wrapping before it
      LastWrappableIsSpace := False;
    end;

    //Ignore color markups [$FFFFFF][]
    if (aText[I] = '[') and (I+1 <= Length(aText)) and (aText[I+1] = ']') then
      Inc(I) //Skip past this markup
    else
      if (aText[I] = '[') and (I+8 <= Length(aText))
        and (aText[I+1] = '$') and (aText[I+8] = ']')
        and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
        Inc(I,8) //Skip past this markup
      else if (aText[I] = #9) then
        dx := (Floor(dx / aTabWidth) + 1) * aTabWidth
      else
        Inc(dx, GetCharWidth(aText[I]));

    if AnsiChar(aText[I]) in [#9,#32,#124] then
    begin
      LastWrappable := I;
      PrevX := dx;
      LastWrappableIsSpace := True;
    end;

    //This algorithm is not perfect, somehow line width is not within SizeX, but very rare
    if ((dx > aMaxPxWidth) and (LastWrappable <> -1)) or (aText[I] = #124) then
    begin
      if (aText[I] <> #124) and aIndentAfterNL then
      begin
        Insert(INDENT, aText, LastWrappable);
        Inc(I, Length(INDENT));
        Inc(dx, Length(INDENT) * WordSpacing);
      end;
      if LastWrappableIsSpace then
        aText[LastWrappable] := #124 //Replace last whitespace with EOL
      else
        Insert(#124, aText, LastWrappable+1); //Insert EOL after last wrappable char
      Dec(dx, PrevX); //Subtract width since replaced whitespace
      LastWrappable := -1;
    end;
    //Force an EOL part way through a word
    if aForced and (dx > aMaxPxWidth) and (LastWrappable = -1) then
    begin
      Insert(#124, aText, I); //Insert an EOL before this character
      dx := 0;
      LastWrappable := -1;
      if aIndentAfterNL then
      begin
        Insert(INDENT, aText, I+1);
        Inc(I, Length(INDENT));
        Inc(dx, Length(INDENT) * WordSpacing);
      end;
    end;
    Inc(I);
  end;
  Result := aText;
end;


function TKMFontData.CharsThatFit(const aText: UnicodeString; aMaxPxWidth: Integer; aRound: Boolean = False; aTabWidth: Integer = TAB_WIDTH): Integer;
var
  I, dx, PrevX, LastCharW: Integer;
begin
  dx := 0;
  Result := Length(aText);

  for I := 1 to Length(aText) do
  begin
    LastCharW := GetCharWidth(aText[I]);
    PrevX := dx;
    if aText[I] = #9 then
      dx := (Floor(dx / aTabWidth) + 1) * aTabWidth
    else
      Inc(dx, LastCharW);

    if (dx > aMaxPxWidth) then
    begin
      // If we want to get approximate result, then check if total width is closer to prev width or to current
      if aRound and (dx - aMaxPxWidth < aMaxPxWidth - PrevX) then
        Result := I
      else
        Result := I - 1; //Previous character fits, this one does not
      Exit;
    end;
  end;
end;


function TKMFontData.GetTextSize(const aText: UnicodeString; aCountMarkup: Boolean = False; aTabWidth: Integer = TAB_WIDTH): TKMPoint;
var
  I: Integer;
  LineCount, LineWidthInc, TmpColor: Integer;
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
  I := 1;
  while I <= Length(aText) do
  begin
    LineWidthInc := 0;
    if aCountMarkup then
    begin
      //Count all characters including markup
      if aText[I] = #9 then // Tab char
        LineWidthInc := (Floor(LineWidth[LineCount] / aTabWidth) + 1) * aTabWidth - LineWidth[LineCount]
      else
        LineWidthInc := GetCharWidth(aText[I]);
      Inc(LineWidth[LineCount], LineWidthInc);
    end else
      //Ignore color markups [$FFFFFF][]
      if (aText[I]='[') and (I+1 <= Length(aText)) and (aText[I+1]=']') then
        Inc(I) //Skip past this markup
      else
        if (aText[I]='[') and (I+8 <= Length(aText))
          and (aText[I+1] = '$') and (aText[I+8]=']')
          and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
          Inc(I,8) //Skip past this markup
        else begin
          //Not markup so count width normally
          if aText[I] = #9 then // Tab char
            LineWidthInc := (Floor(LineWidth[LineCount] / aTabWidth) + 1) * aTabWidth - LineWidth[LineCount]
          else
            LineWidthInc := GetCharWidth(aText[I]);
          Inc(LineWidth[LineCount], LineWidthInc);
        end;

    if (aText[I] = #124) or (I = Length(aText)) then
    begin // If EOL or aText end
      if aText[I] <> #9 then       // for Tab reduce line width for CharSpacing and also for TAB 'jump'
        LineWidthInc := 0;
      LineWidth[LineCount] := Math.Max(0, LineWidth[LineCount] - CharSpacing - LineWidthInc);
      // Remove last interletter space and negate double EOLs
      Inc(LineCount);
    end;
    Inc(I);
  end;

  Dec(LineCount);
  Result.Y := (BaseHeight + LineSpacing) * LineCount;
  for I := 1 to LineCount do
    Result.X := Math.Max(Result.X, LineWidth[I]);
end;


// Return maximum of the width of specified strings when printed on screen with specified font.
function TKMFontData.GetMaxPrintWidthOfStrings(aStrings: array of string): Integer;
var I, Width: Integer;
begin
  Result := 0;
  for I := Low(aStrings) to High(aStrings) do
  begin
    Width := GetTextSize(aStrings[I]).X;
    if (Width > Result) then
      Result := Width;
  end;
end;


end.
