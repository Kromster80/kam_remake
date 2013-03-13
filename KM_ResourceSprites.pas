unit KM_ResourceSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Pics, KM_Render, KM_TextLibrary
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF}
  {$IFDEF FPC}, BGRABitmap, BGRABitmapTypes {$ENDIF};


const
  //Colors to paint beneath player color areas (flags)
  //The blacker/whighter - the more contrast player color will be
  FLAG_COLOR_DARK = $FF101010;   //Dark-grey (Black)
  FLAG_COLOR_LITE = $FFFFFFFF;   //White

type
  TRXUsage = (ruMenu, ruGame); //Where sprites are used

  TRXInfo = record
    FileName: string; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    LoadingTextID: Word;
  end;


var
  RXInfo: array [TRXType] of TRXInfo = (
    (FileName: 'Trees';      TeamColors: False; Usage: ruGame; LoadingTextID: TX_MENU_LOADING_TREES;),
    (FileName: 'Houses';     TeamColors: True;  Usage: ruGame; LoadingTextID: TX_MENU_LOADING_HOUSES;),
    (FileName: 'Units';      TeamColors: True;  Usage: ruGame; LoadingTextID: TX_MENU_LOADING_UNITS;),
    (FileName: 'GUI';        TeamColors: True;  Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'GUIMain';    TeamColors: False; Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'GUIMainH';   TeamColors: False; Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'Tileset';    TeamColors: False; Usage: ruMenu; LoadingTextID: TX_MENU_LOADING_TILESET;));

type
  TRXData = record
    Count: Integer;
    Flag: array of Byte; //Sprite is valid
    Size: array of record X,Y: Word; end;
    Pivot: array of record x,y: Integer; end;
    Data: array of array of Byte;
    RGBA: array of array of Cardinal; //Expanded image
    Mask: array of array of Byte; //Mask for team colors
    HasMask: array of Boolean; //Flag if Mask for team colors is used
  end;
  PRXData = ^TRXData;

  //Base class for Sprite loading
  TKMSpritePack = class
  private
    fPad: Byte; //Force padding between sprites to avoid neighbour edge visibility
    procedure MakeGFX_BinPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal);
    procedure MakeGFX_StripPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal);
    procedure SaveTextureToPNG(aWidth, aHeight: Word; aFilename: string; const Data: TKMCardinalArray);
  protected
    fRT: TRXType;
    fRXData: TRXData;
    procedure Allocate(aCount: Integer); virtual; //Allocate space for data that is being loaded
  public
    constructor Create(aRT: TRXType);

    procedure AddImage(aFolder, aFilename: string; aIndex: Integer);
    property RXData: TRXData read fRXData;
    property Padding: Byte read fPad write fPad;

    procedure LoadFromRXXFile(const aFileName: string; aStartingIndex: Integer = 1);
    procedure OverloadFromFolder(const aFolder: string);
    procedure MakeGFX(aAlphaShadows: Boolean; aStartingIndex: Integer = 1);

    function GetSpriteColors(aCount: Byte): TRGBArray;

    procedure ExportToPNG(const aFolder: string);

    procedure ClearTemp; virtual;//Release non-required data
  end;

  //Overrides for:
  //GUI: Cursors

  TKMSprites = class
  private
    fAlphaShadows: Boolean; //Remember which state we loaded
    fSprites: array[TRXType] of TKMSpritePack;
    fStepProgress: TEvent;
    fStepCaption: TStringEvent;

    function GetRXFileName(aRX: TRXType): string;
    function GetSprites(aRT: TRXType): TKMSpritePack;
  public
    constructor Create(aStepProgress: TEvent; aStepCaption: TStringEvent);
    destructor Destroy; override;

    procedure LoadMenuResources;
    procedure LoadGameResources(aAlphaShadows: Boolean);
    procedure ClearTemp;

    property Sprites[aRT: TRXType]: TKMSpritePack read GetSprites; default;

    //Used externally to access raw RGBA data (e.g. by ExportAnim)
    procedure LoadSprites(aRT: TRXType; aAlphaShadows: Boolean);
    procedure ExportToPNG(aRT: TRXType);

    property AlphaShadows: Boolean read fAlphaShadows;
    property FileName[aRX: TRXType]: string read GetRXFileName;
  end;


  TKMTexCoords = record
                  ID: Cardinal;
                  u1,v1,u2,v2: Single; //Top-Left, Bottom-Right uv coords
                end;

var
  GFXData: array [TRXType] of array of record
    Tex, Alt: TKMTexCoords; //AltID used for team colors and house building steps
    PxWidth, PxHeight: Word;
  end;


implementation
uses KromUtils, KM_Log, Types, StrUtils, KM_BinPacking, KM_Utils;


{ TKMSpritePack }
constructor TKMSpritePack.Create(aRT: TRXType);
begin
  inherited Create;

  fRT := aRT;

  if fRT = rxTiles then
    fPad := 1;
end;


procedure TKMSpritePack.Allocate(aCount: Integer);
begin
  fRXData.Count := aCount;

  aCount := fRXData.Count + 1;
  SetLength(GFXData[fRT],     aCount);
  SetLength(fRXData.Flag,     aCount);
  SetLength(fRXData.Size,     aCount);
  SetLength(fRXData.Pivot,    aCount);
  SetLength(fRXData.RGBA,     aCount);
  SetLength(fRXData.Mask,     aCount);
  SetLength(fRXData.HasMask,  aCount);
end;


//Release RAM that is no longer needed
procedure TKMSpritePack.ClearTemp;
var I: Integer;
begin
  for I := 1 to fRXData.Count do
  begin
    SetLength(fRXData.RGBA[I], 0);
    SetLength(fRXData.Mask[I], 0);
  end;
end;


//Add PNG images to spritepack if user has any addons in Sprites folder
procedure TKMSpritePack.AddImage(aFolder, aFilename: string; aIndex: Integer);
var
  I,K:integer;
  Tr, Tg, Tb, T: Byte;
  Thue, Tsat, Tbri: Single;
  ft: TextFile;
  {$IFDEF WDC}
  p: Cardinal;
  po: TPNGObject;
  {$ENDIF}
  {$IFDEF FPC}
  po: TBGRABitmap;
  {$ENDIF}
  FileNameA: string;
  Transparent: Byte;
begin
  Assert(SameText(ExtractFileExt(aFilename), '.png'));

  if aIndex >= fRXData.Count then
    Allocate(aIndex);

  {$IFDEF WDC}
  po := TPNGObject.Create;
  po.LoadFromFile(aFolder + aFilename);
  {$ENDIF}
  {$IFDEF FPC}
  po := TBGRABitmap.Create(aFolder + aFilename);
  {$ENDIF}
  Assert((po.Width <= 1024) and (po.Height <= 1024), 'Image size should be less than 1024x1024 pixels');

  try
    fRXData.Flag[aIndex] := Byte(po.Width * po.Height <> 0); //Mark as used (required for saving RXX)
    fRXData.Size[aIndex].X := po.Width;
    fRXData.Size[aIndex].Y := po.Height;

    SetLength(fRXData.RGBA[aIndex], po.Width * po.Height);
    SetLength(fRXData.Mask[aIndex], po.Width * po.Height); //Should allocate space for it's always comes along

    {$IFDEF WDC}
    //There are ways to process PNG transparency
    case po.TransparencyMode of
      ptmNone:
        for K:=0 to po.Height-1 do for I:=0 to po.Width-1 do
          fRXData.RGBA[aIndex, K*po.Width+I] := cardinal(po.Pixels[I,K]) or $FF000000;
      ptmBit:
        begin
          Transparent := 0;
          if TChunktRNS(po.Chunks.ItemFromClass(TChunktRNS)).DataSize > 0 then
            Transparent := TChunktRNS(po.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[0]; //We don't handle multi-transparent palettes yet
          for K:=0 to po.Height-1 do for I:=0 to po.Width-1 do
            if PByteArray(po.Scanline[K])^[I] = Transparent then
              fRXData.RGBA[aIndex, K*po.Width+I] := cardinal(po.Pixels[I,K]) and $FFFFFF //avoid black edging
            else
              fRXData.RGBA[aIndex, K*po.Width+I] := cardinal(po.Pixels[I,K]) or $FF000000;
        end;
      ptmPartial:
        for K:=0 to po.Height-1 do for I:=0 to po.Width-1 do
        begin
          p := po.AlphaScanline[K]^[I];
          fRXData.RGBA[aIndex, K*po.Width+I] := cardinal(po.Pixels[I,K]) or (p shl 24);
        end;
      else
        Assert(false, 'Unknown PNG transparency mode')
    end;
    {$ENDIF}
    {$IFDEF FPC}
    for K:=0 to po.Height-1 do for I:=0 to po.Width-1 do
      fRXData.RGBA[aIndex, K*po.Width+I] := cardinal(po.GetPixel(I,K).red) or (cardinal(po.GetPixel(I,K).green) shl 8) or
                                           (cardinal(po.GetPixel(I,K).blue) shl 16) or (cardinal(po.GetPixel(I,K).alpha) shl 24);
    {$ENDIF}
  finally
    po.Free;
  end;

  FileNameA := StringReplace(aFilename, '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]);
  fRXData.HasMask[aIndex] := FileExists(aFolder + FileNameA);

  //Load and process the mask if it exists
  if fRXData.HasMask[aIndex] then
  begin
    //! PNG masks are designed for the artist, they take standard KaM reds so it's easier
    {$IFDEF WDC}
    po := TPNGObject.Create;
    po.LoadFromFile(aFolder + FileNameA);
    {$ENDIF}
    {$IFDEF FPC}
    po := TBGRABitmap.Create(aFolder + FileNameA);
    {$ENDIF}

    if (fRXData.Size[aIndex].X = po.Width)
    and (fRXData.Size[aIndex].Y = po.Height) then
    begin
      //We don't handle transparency in Masks
      {$IFDEF WDC}
      for K := 0 to po.Height - 1 do
      for I := 0 to po.Width - 1 do
      if Cardinal(po.Pixels[I,K] and $FFFFFF) <> 0 then
      begin
        Tr := fRXData.RGBA[aIndex, K*po.Width+I] and $FF;
        Tg := fRXData.RGBA[aIndex, K*po.Width+I] shr 8 and $FF;
        Tb := fRXData.RGBA[aIndex, K*po.Width+I] shr 16 and $FF;

        //Determine color brightness
        ConvertRGB2HSB(Tr, Tg, Tb, Thue, Tsat, Tbri);

        //Make background RGBA black or white for more saturated colors
        if Tbri < 0.5 then
          fRXData.RGBA[aIndex, K*po.Width+I] := FLAG_COLOR_DARK
        else
          fRXData.RGBA[aIndex, K*po.Width+I] := FLAG_COLOR_LITE;

        //Map brightness from 0..1 to 0..255..0
        T := Trunc((0.5 - Abs(Tbri - 0.5)) * 510);
        fRXData.Mask[aIndex, K*po.Width+I] := T;
      end;
      {$ENDIF}
      {$IFDEF FPC}
      for K := 0 to po.Height - 1 do
      for I := 0 to po.Width - 1 do
      if Cardinal(po.GetPixel(I,K).red) <> 0 then
      begin
        Tr := fRXData.RGBA[aIndex, K*po.Width+I] and $FF;
        Tg := fRXData.RGBA[aIndex, K*po.Width+I] shr 8 and $FF;
        Tb := fRXData.RGBA[aIndex, K*po.Width+I] shr 16 and $FF;

        //Determine color brightness
        ConvertRGB2HSB(Tr, Tg, Tb, Thue, Tsat, Tbri);

        //Make background RGBA black or white for more saturated colors
        if Tbri < 0.5 then
          fRXData.RGBA[aIndex, K*po.Width+I] := FLAG_COLOR_DARK
        else
          fRXData.RGBA[aIndex, K*po.Width+I] := FLAG_COLOR_LITE;

        //Map brightness from 0..1 to 0..255..0
        T := Trunc((0.5 - Abs(Tbri - 0.5)) * 510);
        fRXData.Mask[aIndex, K*po.Width+I] := T;
      end;
      {$ENDIF}
    end;
    po.Free;
  end;

  //Read pivot info
  if FileExists(aFolder + Copy(aFilename, 1, 6) + '.txt') then
  begin
    AssignFile(ft, aFolder + Copy(aFilename, 1, 6) + '.txt');
    Reset(ft);
    ReadLn(ft, fRXData.Pivot[aIndex].X);
    ReadLn(ft, fRXData.Pivot[aIndex].Y);
    CloseFile(ft);
  end;
end;


procedure TKMSpritePack.LoadFromRXXFile(const aFileName: string; aStartingIndex: Integer = 1);
var
  I: Integer;
  RXXCount: Integer;
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
  if SKIP_RENDER then Exit;
  if not FileExists(aFileName) then Exit;

  InputStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);

  try
    DecompressionStream.Read(RXXCount, 4);
    if fLog <> nil then
      fLog.AddTime(RXInfo[fRT].FileName + ' -', RXXCount);

    if RXXCount = 0 then
      Exit;

    Allocate(aStartingIndex + RXXCount);

    DecompressionStream.Read(fRXData.Flag[aStartingIndex], RXXCount);

    for I := aStartingIndex to aStartingIndex + RXXCount do
      if fRXData.Flag[I] = 1 then
      begin
        DecompressionStream.Read(fRXData.Size[I].X, 4);
        DecompressionStream.Read(fRXData.Pivot[I].X, 8);
        //Data part of each sprite is 32BPP RGBA in Remake RXX files
        SetLength(fRXData.RGBA[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        SetLength(fRXData.Mask[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.RGBA[I, 0], 4 * fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.HasMask[I], 1);
        if fRXData.HasMask[I] then
          DecompressionStream.Read(fRXData.Mask[I, 0], fRXData.Size[I].X * fRXData.Size[I].Y);
      end;
  finally
    DecompressionStream.Free;
    InputStream.Free;
  end;
end;


//Parse all valid files in Sprites folder and load them additionaly to or replacing original sprites
procedure TKMSpritePack.OverloadFromFolder(const aFolder: string);
  procedure ProcessFolder(const aProcFolder: string);
  var
    FileList: TStringList;
    SearchRec: TSearchRec;
    I, ID: Integer;
  begin
    if not DirectoryExists(aFolder) then Exit;
    FileList := TStringList.Create;
    try
      //PNGs
      if FindFirst(aProcFolder + IntToStr(Byte(fRT) + 1) + '_????.png', faAnyFile - faDirectory, SearchRec) = 0 then
      repeat
        FileList.Add(SearchRec.Name);
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);

      //PNG may be accompanied by some more files
      //#_####.png - Base texture
      //#_####a.png - Flag color mask
      //#_####.txt - Pivot info (optional)
      for I := 0 to FileList.Count - 1 do
        if TryStrToInt(Copy(FileList.Strings[I], 3, 4), ID) then
          AddImage(aProcFolder, FileList.Strings[I], ID);

      //Delete following sprites
      if FindFirst(aProcFolder + IntToStr(Byte(fRT) + 1) + '_????', faAnyFile - faDirectory, SearchRec) = 0 then
      repeat
        if TryStrToInt(Copy(SearchRec.Name, 3, 4), ID) then
          fRXData.Flag[ID] := 0;
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
    finally
      FileList.Free;
    end;
  end;
begin
  if SKIP_RENDER then Exit;

  ProcessFolder(aFolder);
  ProcessFolder(aFolder + IntToStr(Byte(fRT) + 1) + '\');
end;


//Export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes
procedure TKMSpritePack.ExportToPNG(const aFolder: string);
var
  {$IFDEF WDC} Png: TPNGObject; {$ENDIF}
  {$IFDEF FPC} po: TBGRABitmap; {$ENDIF}
  ID, I, K: Integer;
  M: Byte;
  SizeX, SizeY: Integer;
begin
  ForceDirectories(aFolder);

  {$IFDEF WDC} Png := TPNGObject.CreateBlank(COLor_RGBALPHA, 8, 0, 0); {$ENDIF}
  {$IFDEF FPC} po := TBGRABitmap.Create(0, 0, BGRABlack); {$ENDIF}

  for ID := 1 to fRXData.Count do
  if fRXData.Flag[ID] = 1 then
  begin
    SizeX := fRXData.Size[ID].X;
    SizeY := fRXData.Size[ID].Y;
    {$IFDEF WDC} Png.Resize(SizeX, SizeY); {$ENDIF}
    {$IFDEF FPC} po.SetSize(SizeX, SizeY); {$ENDIF}

    //Export RGB values
    for I := 0 to SizeY - 1 do
    for K := 0 to SizeX - 1 do
    begin
      {$IFDEF WDC}
      if fRXData.HasMask[ID] and (fRXData.Mask[ID, I*SizeX + K] > 0) then
      begin
        M := fRXData.Mask[ID, I*SizeX + K];
        if fRXData.RGBA[ID, I*SizeX + K] = FLAG_COLOR_DARK then
          Png.Pixels[K,I] := M
        else
          Png.Pixels[K,I] := $FF + (255 - M) * $010100;
      end
      else
        Png.Pixels[K,I] := fRXData.RGBA[ID, I*SizeX + K] and $FFFFFF;

      Png.AlphaScanline[I]^[K] := fRXData.RGBA[ID, I*SizeX + K] shr 24;
      {$ENDIF}
      {$IFDEF FPC}
      //I can't figure out how to get transparency to save in PNGs, so for now everything is opaque
      if fRXData.HasMask[ID] and (fRXData.Mask[ID, I*SizeX + K] > 0) then
      begin
        M := fRXData.Mask[ID, I*SizeX + K];
        if fRXData.RGBA[ID, I*SizeX + K] = FLAG_COLOR_DARK then
          po.CanvasBGRA.Pixels[K,I] := M
        else
          po.CanvasBGRA.Pixels[K,I] := $FF + (255 - M) * $010100;
      end
      else
        po.CanvasBGRA.Pixels[K,I] := fRXData.RGBA[ID, I*SizeX + K] and $FFFFFF;
      {$ENDIF}
    end;

    //Mark pivot location with a dot
    {K := SizeX + fRXData.Pivot[ID].x;
    I := SizeY + fRXData.Pivot[ID].y;
    if InRange(I, 0, SizeY-1) and InRange(K, 0, SizeX-1) then
      Png.Pixels[K,I] := $FF00FF;//}

    {$IFDEF WDC} Png.SaveToFile(aFolder + Format('%d_%.4d.png', [Byte(fRT)+1, ID])); {$ENDIF}
    {$IFDEF FPC} po.SaveToFile(aFolder + Format('%d_%.4d.png', [Byte(fRT)+1, ID])); {$ENDIF}

    //Export Mask
    if fRXData.HasMask[ID] then
    begin
      for I := 0 to SizeY - 1 do
      for K := 0 to SizeX - 1 do
      begin
        {$IFDEF WDC}
        Png.Pixels[K,I] := Byte(fRXData.Mask[ID, I*SizeX + K] > 0) * $FFFFFF;
        Png.AlphaScanline[I]^[K] := $FF; //Always there
        {$ENDIF}
        {$IFDEF FPC}
        po.CanvasBGRA.Pixels[K,I] := (Byte(fRXData.Mask[ID, I*SizeX + K] > 0) * $FFFFFF) or $FF000000;
        {$ENDIF}
      end;

      {$IFDEF WDC} Png.SaveToFile(aFolder + Format('%d_%.4da.png', [Byte(fRT)+1, ID])); {$ENDIF}
      {$IFDEF FPC} po.SaveToFile(aFolder + Format('%d_%.4da.png', [Byte(fRT)+1, ID])); {$ENDIF}
    end;
  end;

  {$IFDEF WDC} Png.Free; {$ENDIF}
  {$IFDEF FPC} po.Free; {$ENDIF}
end;


function TKMSpritePack.GetSpriteColors(aCount: Byte): TRGBArray;
var
  I, L, M: Integer;
  PixelCount: Word;
  R,G,B: Integer;
begin
  SetLength(Result, Min(fRXData.Count, aCount));

  for I := 1 to Min(fRXData.Count, aCount) do
  begin
    R := 0;
    G := 0;
    B := 0;
    for L := 0 to fRXData.Size[I].Y - 1 do
    for M := 0 to fRXData.Size[I].X - 1 do
    begin
      Inc(R, fRXData.RGBA[I, L * fRXData.Size[I].X + M] and $FF);
      Inc(G, fRXData.RGBA[I, L * fRXData.Size[I].X + M] shr 8 and $FF);
      Inc(B, fRXData.RGBA[I, L * fRXData.Size[I].X + M] shr 16 and $FF);
    end;
    PixelCount := fRXData.Size[I].X * fRXData.Size[I].Y;
    Result[I-1].R := Round(R / PixelCount);
    Result[I-1].G := Round(G / PixelCount);
    Result[I-1].B := Round(B / PixelCount);
  end;
end;


//Take RX data and make nice atlas texture out of it
//Atlases should be POT to improve performance and avoid driver bugs
//In result we have GFXData structure filled
procedure TKMSpritePack.MakeGFX(aAlphaShadows: Boolean; aStartingIndex: Integer = 1);
var
  TexType: TTexFormat;
  BaseRAM, IdealRAM, ColorRAM, TexCount: Cardinal;
  I: Integer;
begin
  if SKIP_RENDER then Exit;
  if fRXData.Count = 0 then Exit;

  if aAlphaShadows and (fRT in [rxTrees,rxHouses,rxUnits,rxGui]) then
    TexType := tf_RGBA8
  else
    TexType := tf_RGB5A1;

  //Second algorithm is kept until we implement better one
  if USE_BIN_PACKING then
    MakeGFX_BinPacking(TexType, aStartingIndex, BaseRAM, ColorRAM, TexCount)
  else
    MakeGFX_StripPacking(TexType, aStartingIndex, BaseRAM, ColorRAM, TexCount);

  IdealRAM := 0;
  for I := aStartingIndex to fRXData.Count - 1 do
  if fRXData.Flag[I] <> 0 then
    Inc(IdealRAM, fRXData.Size[I].X * fRXData.Size[I].Y * TexFormatSize[TexType]);

  fLog.AddTime(IntToStr(TexCount) + ' Textures created');
  fLog.AddNoTime(Format('%d/%d', [BaseRAM div 1024, IdealRAM div 1024]) +
                ' Kbytes allocated/ideal for ' + RXInfo[fRT].FileName + ' GFX when using Packing');
  fLog.AddNoTime(IntToStr(ColorRAM div 1024) + ' KBytes for team colors');
end;


//This algorithm tries to stack sprites into strips of nearest POT length
procedure TKMSpritePack.MakeGFX_StripPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal);
const
  MAX_TEX_RESOLUTION  = 512; //Maximum texture resolution client can handle (used for packing sprites)
var
  ci, J, I, K, LeftIndex, RightIndex, SpanCount: Integer;
  WidthPOT, HeightPOT: Word;
  TD: array of Cardinal;
  TA: array of Cardinal;
  HasMsk: Boolean;
begin
  LeftIndex := aStartingIndex - 1;
  TexCount := 0;
  repeat
    inc(LeftIndex);

    WidthPOT  := fRXData.Size[LeftIndex].X;
    HeightPOT := MakePOT(fRXData.Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<fRXData.Count)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(fRXData.Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(fRXData.Size[LeftIndex+SpanCount].Y))and(WidthPOT+fRXData.Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+fRXData.Size[LeftIndex+SpanCount].X <= MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,fRXData.Size[LeftIndex+SpanCount].X);
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    SetLength(TD,WidthPOT*HeightPOT+1);
    SetLength(TA,WidthPOT*HeightPOT+1);

    for i := 0 to HeightPOT - 1 do
    begin
      ci := 0;
      for j := LeftIndex to RightIndex do
        for k := 0 to fRXData.Size[j].X - 1 do
        begin
          if i < fRXData.Size[j].Y then
          begin
            //CopyMemory(TD[(i-1)*WidthPOT+ci-1], fRXData.RGBA[j,(i-1)*fRXData.Size[j].X+k-1], )
            TD[i*WidthPOT+ci] := fRXData.RGBA[j,i*fRXData.Size[j].X+k];
            TA[i*WidthPOT+ci] := (fRXData.Mask[j,i*fRXData.Size[j].X+k] SHL 24) or $FFFFFF;
          end;
          inc(ci);
        end;
    end;

    HasMsk:=false;
    for j:=LeftIndex to RightIndex do
      HasMsk := HasMsk or fRXData.HasMask[j];

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXInfo[fRT].TeamColors and (not ((fRT=rxGui)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[fRT,LeftIndex].Tex.ID := TRender.GenTexture(WidthPOT,HeightPOT,@TD[0],aTexType);
      //TeamColors are done through alternative plain colored texture
      if HasMsk then
      begin
        GFXData[fRT,LeftIndex].Alt.ID := TRender.GenTexture(WidthPOT,HeightPOT,@TA[0],tf_Alpha8);
        Inc(ColorRAM, WidthPOT * HeightPOT * TexFormatSize[aTexType]); //GL_ALPHA4
      end;
    end
    else
      GFXData[fRT, LeftIndex].Tex.ID := TRender.GenTexture(WidthPOT, HeightPOT, @TD[0], aTexType);

    Inc(BaseRAM, WidthPOT * HeightPOT * TexFormatSize[aTexType]);

    SaveTextureToPNG(WidthPOT, HeightPOT, RXInfo[fRT].FileName + '_Strip' + IntToStr(LeftIndex) + '-' + IntToStr(RightIndex), @TD[0]);
    if HasMsk then
      SaveTextureToPNG(WidthPOT, HeightPOT, RXInfo[fRT].FileName + '_StripMask' + IntToStr(LeftIndex) + '-' + IntToStr(RightIndex), @TA[0]);

    SetLength(TD, 0);
    SetLength(TA, 0);

    K := 0;
    for J := LeftIndex to RightIndex do
    begin
      GFXData[fRT, J].Tex.ID := GFXData[fRT, LeftIndex].Tex.ID;
      GFXData[fRT, J].Alt.ID := GFXData[fRT, LeftIndex].Alt.ID;
      GFXData[fRT, J].Tex.u1 := K / WidthPOT;
      GFXData[fRT, J].Tex.v1 := 0;
      Inc(K, fRXData.Size[J].X);
      GFXData[fRT, J].Tex.u2 := K / WidthPOT;
      GFXData[fRT, J].Tex.v2 := fRXData.Size[J].Y / HeightPOT;
      GFXData[fRT, J].Alt.u1 := GFXData[fRT, J].Tex.u1;
      GFXData[fRT, J].Alt.v1 := GFXData[fRT, J].Tex.v1;
      GFXData[fRT, J].Alt.u2 := GFXData[fRT, J].Tex.u2;
      GFXData[fRT, J].Alt.v2 := GFXData[fRT, J].Tex.v2;
      GFXData[fRT, J].PxWidth := fRXData.Size[J].X;
      GFXData[fRT, J].PxHeight := fRXData.Size[J].Y;
    end;

    Inc(LeftIndex, SpanCount - 1);
    Inc(TexCount);

  until (LeftIndex >= fRXData.Count);
end;


//This algorithm is planned to take advantage of more efficient 2D bin packing
procedure TKMSpritePack.MakeGFX_BinPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal);
type
  TSpriteAtlas = (saBase, saMask);
  procedure PrepareAtlases(SpriteInfo: TBinArray; aMode: TSpriteAtlas; aTexType: TTexFormat);
  const ExportName: array [TSpriteAtlas] of string = ('Base', 'Mask');
  var
    I, K, L, M: Integer;
    CT, CL, Pixel: Cardinal;
    Tx: Cardinal;
    ID: Word;
    TxCoords: TKMTexCoords;
    TD: array of Cardinal;
  begin
    //Prepare atlases
    for I := 0 to High(SpriteInfo) do
    begin
      Assert(MakePOT(SpriteInfo[I].Width) = SpriteInfo[I].Width);
      Assert(MakePOT(SpriteInfo[I].Height) = SpriteInfo[I].Height);
      SetLength(TD, 0);
      SetLength(TD, SpriteInfo[I].Width * SpriteInfo[I].Height);

      //Copy sprite to Atlas
      for K := 0 to High(SpriteInfo[I].Sprites) do
      begin
        ID := SpriteInfo[I].Sprites[K].SpriteID;
        for L := 0 to fRXData.Size[ID].Y - 1 do
        for M := 0 to fRXData.Size[ID].X - 1 do
        begin
          CT := SpriteInfo[I].Sprites[K].PosY;
          CL := SpriteInfo[I].Sprites[K].PosX;
          Pixel := (CT + L) * SpriteInfo[I].Width + CL + M;
          if aMode = saBase then
            TD[Pixel] := fRXData.RGBA[ID, L * fRXData.Size[ID].X + M]
          else
            TD[Pixel] := $FFFFFF or (fRXData.Mask[ID, L * fRXData.Size[ID].X + M] shl 24);

          //Fill padding with edge pixels
          if fPad > 0 then
          begin
            if (M = 0) then
            begin
              TD[Pixel - 1] := TD[Pixel];
              if (L = 0) then
                TD[Pixel - SpriteInfo[I].Width - 1] := TD[Pixel]
              else
              if (L = fRXData.Size[ID].Y - 1) then
                TD[Pixel + SpriteInfo[I].Width - 1] := TD[Pixel];
            end;

            if (M = fRXData.Size[ID].X - 1) then
            begin
              TD[Pixel + 1] := TD[Pixel];
              if (L = 0) then
                TD[Pixel - SpriteInfo[I].Width + 1] := TD[Pixel]
              else
              if (L = fRXData.Size[ID].Y - 1) then
                TD[Pixel + SpriteInfo[I].Width + 1] := TD[Pixel];
            end;

            if (L = 0) then                       TD[Pixel - SpriteInfo[I].Width] := TD[Pixel];
            if (L = fRXData.Size[ID].Y - 1) then  TD[Pixel + SpriteInfo[I].Width] := TD[Pixel];
          end;

          //Sprite outline
          if OUTLINE_ALL_SPRITES and (
            (L = 0) or (M = 0)
            or (L = fRXData.Size[ID].Y - 1)
            or (M = fRXData.Size[ID].X - 1)) then
            TD[Pixel] := $FF0000FF;
        end;
      end;

      //Generate texture once
      Tx := TRender.GenTexture(SpriteInfo[I].Width, SpriteInfo[I].Height, @TD[0], aTexType);

      //Now that we know texture IDs we can fill GFXData structure
      for K := 0 to High(SpriteInfo[I].Sprites) do
      begin
        ID := SpriteInfo[I].Sprites[K].SpriteID;

        TxCoords.ID := Tx;
        TxCoords.u1 := SpriteInfo[I].Sprites[K].PosX / SpriteInfo[I].Width;
        TxCoords.v1 := SpriteInfo[I].Sprites[K].PosY / SpriteInfo[I].Height;
        TxCoords.u2 := (SpriteInfo[I].Sprites[K].PosX + fRXData.Size[ID].X) / SpriteInfo[I].Width;
        TxCoords.v2 := (SpriteInfo[I].Sprites[K].PosY + fRXData.Size[ID].Y) / SpriteInfo[I].Height;

        if aMode = saBase then
        begin
          GFXData[fRT, ID].Tex := TxCoords;
          GFXData[fRT, ID].PxWidth := fRXData.Size[ID].X;
          GFXData[fRT, ID].PxHeight := fRXData.Size[ID].Y;
        end
        else
          GFXData[fRT, ID].Alt := TxCoords;
      end;

      if aMode = saBase then
        Inc(BaseRAM, SpriteInfo[I].Width * SpriteInfo[I].Height * TexFormatSize[aTexType])
      else
        Inc(ColorRAM, SpriteInfo[I].Width * SpriteInfo[I].Height * TexFormatSize[aTexType]); //GL_ALPHA4

      Inc(TexCount);

      SaveTextureToPNG(SpriteInfo[I].Width, SpriteInfo[I].Height, RXInfo[fRT].FileName + '_' + ExportName[aMode] + IntToStr(aStartingIndex+I), @TD[0]);
    end;
  end;
const
  AtlasSize = 512;
var
  I, K: Integer;
  SpriteSizes: TIndexSizeArray;
  SpriteInfo: TBinArray;
begin
  //Prepare base atlases
  SetLength(SpriteSizes, fRXData.Count - aStartingIndex);
  K := 0;
  for I := aStartingIndex to fRXData.Count - 1 do
  if (fRXData.Size[I].X * fRXData.Size[I].Y <> 0) then
  begin
    SpriteSizes[K].ID := I;
    SpriteSizes[K].X := fRXData.Size[I].X;
    SpriteSizes[K].Y := fRXData.Size[I].Y;
    Inc(K);
  end;
  SetLength(SpriteSizes, K);

  SetLength(SpriteInfo, 0);
  BinPack(SpriteSizes, AtlasSize, fPad, SpriteInfo);
  PrepareAtlases(SpriteInfo, saBase, aTexType);

  //Prepare masking atlases
  SetLength(SpriteSizes, fRXData.Count - aStartingIndex);
  K := 0;
  for I := aStartingIndex to fRXData.Count - 1 do
  if (fRXData.Size[I].X * fRXData.Size[I].Y <> 0) and fRXData.HasMask[I] then
  begin
    SpriteSizes[K].ID := I;
    SpriteSizes[K].X := fRXData.Size[I].X;
    SpriteSizes[K].Y := fRXData.Size[I].Y;
    Inc(K);
  end;
  SetLength(SpriteSizes, K);

  SetLength(SpriteInfo, 0);
  BinPack(SpriteSizes, AtlasSize, fPad, SpriteInfo);
  PrepareAtlases(SpriteInfo, saMask, tf_Alpha8);
end;


procedure TKMSpritePack.SaveTextureToPNG(aWidth, aHeight: Word; aFilename: string; const Data: TKMCardinalArray);
var
  I, K: Word;
  T: Cardinal;
  {$IFDEF WDC} Png: TPNGObject; {$ENDIF}
  {$IFDEF FPC} po: TBGRABitmap; {$ENDIF}
  Folder: string;
begin
  if EXPORT_SPRITE_ATLASES then
  begin
    Folder := ExeDir + 'Export\GenTextures\';
    ForceDirectories(Folder);

    {$IFDEF WDC}
    Png := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, aWidth, aHeight);
    try
      for I := 0 to aHeight - 1 do
      for K := 0 to aWidth - 1 do
      begin
        T := (PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^;
        Png.Canvas.Pixels[K, I] := T and $FFFFFF; //Ignore alpha
        Png.AlphaScanline[I]^[K] := T shr 24; //Alpha
      end;

      Png.SaveToFile(Folder + aFilename + '.png');
    finally
      Png.Free;
    end;
    {$ENDIF}

    {$IFDEF FPC}
    po := TBGRABitmap.Create(aWidth, aHeight, BGRABlack);
    try
      for I := 0 to aHeight - 1 do
      for K := 0 to aWidth - 1 do
      begin
        T := (PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^;
        //I can't figure out how to get transparency to save in PNGs, so for now everything is opaque
        po.CanvasBGRA.Pixels[K,I] := T and $FFFFFF;
        po.AlphaPixel(K,I,255);
      end;

      po.SaveToFile(Folder + aFilename + '.png');
    finally
      po.Free;
    end;
    {$ENDIF}
  end;
end;


{ TKMSprites }
constructor TKMSprites.Create(aStepProgress: TEvent; aStepCaption: TStringEvent);
var
  RT: TRXType;
begin
  inherited Create;

  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT] := TKMSpritePack.Create(RT);

  fStepProgress := aStepProgress;
  fStepCaption := aStepCaption;
end;


destructor TKMSprites.Destroy;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].Free;

  inherited;
end;


//Clear unused RAM
procedure TKMSprites.ClearTemp;
var RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].ClearTemp;
end;


function TKMSprites.GetRXFileName(aRX: TRXType): string;
begin
  Result := RXInfo[aRX].FileName;
end;


function TKMSprites.GetSprites(aRT: TRXType): TKMSpritePack;
begin
  Result := fSprites[aRT];
end;


procedure TKMSprites.LoadMenuResources;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    if RXInfo[RT].Usage = ruMenu then
    begin
      fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');
      LoadSprites(RT, RT = rxGUI); //Only GUI needs alpha shadows
      fSprites[RT].MakeGFX(RT = rxGUI);
      fStepProgress;
    end;
end;


procedure TKMSprites.LoadGameResources(aAlphaShadows: Boolean);
var RT: TRXType;
begin
  //Remember which version we load, so if it changes inbetween games we reload it
  fAlphaShadows := aAlphaShadows;

  for RT := Low(TRXType) to High(TRXType) do
  if RXInfo[RT].Usage = ruGame then
  begin
    fStepCaption(fTextLibrary[RXInfo[RT].LoadingTextID]);
    fLog.AddTime('Reading ' + RXInfo[RT].FileName + '.rx');
    LoadSprites(RT, fAlphaShadows);
    fSprites[RT].MakeGFX(fAlphaShadows);
  end;
end;


//Try to load RXX first, then RX, then use Folder
procedure TKMSprites.LoadSprites(aRT: TRXType; aAlphaShadows: Boolean);
begin
  if aAlphaShadows and FileExists(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '_a.rxx') then
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '_a.rxx')
  else
  if FileExists(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '.rxx') then
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '.rxx')
  else
    Exit;

  fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
end;


procedure TKMSprites.ExportToPNG(aRT: TRXType);
begin
  LoadSprites(aRT, False);
  fSprites[aRT].ExportToPNG(ExeDir + 'Export\' + RXInfo[aRT].FileName + '.rx\');
  ClearTemp;
end;


end.
