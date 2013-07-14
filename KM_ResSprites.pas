unit KM_ResSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Pics, KM_PNG, KM_Render, KM_ResTexts
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};


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

    procedure ExportAll(const aFolder: string);
    procedure ExportImage(const aFile: string; aIndex: Integer);
    procedure ExportMask(const aFile: string; aIndex: Integer);

    procedure ClearTemp; virtual;//Release non-required data
  end;

  //Overrides for:
  //GUI: Cursors

  TKMSprites = class
  private
    fAlphaShadows: Boolean; //Remember which state we loaded
    fSprites: array[TRXType] of TKMSpritePack;
    fStepProgress: TEvent;
    fStepCaption: TUnicodeStringEvent;

    function GetRXFileName(aRX: TRXType): string;
    function GetSprites(aRT: TRXType): TKMSpritePack;
  public
    constructor Create(aStepProgress: TEvent; aStepCaption: TUnicodeStringEvent);
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
uses KromUtils, KM_Log, KM_BinPacking, KM_Utils;


var
  LOG_EXTRA_GFX: Boolean = False;


{ TKMSpritePack }
constructor TKMSpritePack.Create(aRT: TRXType);
begin
  inherited Create;

  fRT := aRT;

  //Terrain tiles need padding to avoid edge bleeding
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
type TMaskType = (mtNone, mtPlain, mtSmart);
var
  I,K:integer;
  Tr, Tg, Tb, T: Byte;
  Thue, Tsat, Tbri: Single;
  ft: TextFile;
  MaskFile: array [TMaskType] of string;
  MaskTyp: TMaskType;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  Assert(SameText(ExtractFileExt(aFilename), '.png'));

  if aIndex >= fRXData.Count then
    Allocate(aIndex);

  LoadFromPng(aFolder + aFilename, pngWidth, pngHeight, pngData);
  Assert((pngWidth <= 1024) and (pngHeight <= 1024), 'Image size should be less than 1024x1024 pixels');

  fRXData.Flag[aIndex] := Byte(pngWidth * pngHeight <> 0); //Mark as used (required for saving RXX)
  fRXData.Size[aIndex].X := pngWidth;
  fRXData.Size[aIndex].Y := pngHeight;

  SetLength(fRXData.RGBA[aIndex], pngWidth * pngHeight);
  SetLength(fRXData.Mask[aIndex], pngWidth * pngHeight); //Should allocate space for it's always comes along

  for K := 0 to pngHeight - 1 do
  for I := 0 to pngWidth - 1 do
    fRXData.RGBA[aIndex, K * pngWidth + I] := pngData[K * pngWidth + I];

  MaskFile[mtPlain] := aFolder + StringReplace(aFilename, '.png', 'm.png', [rfReplaceAll, rfIgnoreCase]);
  MaskFile[mtSmart] := aFolder + StringReplace(aFilename, '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]);

  //Determine mask processing mode
  if FileExists(MaskFile[mtPlain]) then
    MaskTyp := mtPlain
  else
  if FileExists(MaskFile[mtSmart]) then
    MaskTyp := mtSmart
  else
    MaskTyp := mtNone;

  fRXData.HasMask[aIndex] := MaskTyp in [mtPlain, mtSmart];

  //Load and process the mask if it exists
  if fRXData.HasMask[aIndex] then
  begin
    //Plain masks are used 'as is'
    //Smart masks are designed for the artist, they convert color brightness into a mask

    LoadFromPng(MaskFile[MaskTyp], pngWidth, pngHeight, pngData);

    if (fRXData.Size[aIndex].X = pngWidth)
    and (fRXData.Size[aIndex].Y = pngHeight) then
    begin
      //We don't handle transparency in Masks
      for K := 0 to pngHeight - 1 do
      for I := 0 to pngWidth - 1 do
      case MaskTyp of
        mtPlain:  begin
                    //For now process just red (assume pic is greyscale)
                    fRXData.Mask[aIndex, K*pngWidth+I] := pngData[K*pngWidth+I] and $FF;
                  end;
        mtSmart:  begin
                    if Cardinal(pngData[K*pngWidth+I] and $FFFFFF) <> 0 then
                    begin
                      Tr := fRXData.RGBA[aIndex, K*pngWidth+I] and $FF;
                      Tg := fRXData.RGBA[aIndex, K*pngWidth+I] shr 8 and $FF;
                      Tb := fRXData.RGBA[aIndex, K*pngWidth+I] shr 16 and $FF;

                      //Determine color brightness
                      ConvertRGB2HSB(Tr, Tg, Tb, Thue, Tsat, Tbri);

                      //Make background RGBA black or white for more saturated colors
                      if Tbri < 0.5 then
                        fRXData.RGBA[aIndex, K*pngWidth+I] := FLAG_COLOR_DARK
                      else
                        fRXData.RGBA[aIndex, K*pngWidth+I] := FLAG_COLOR_LITE;

                      //Map brightness from 0..1 to 0..255..0
                      T := Trunc((0.5 - Abs(Tbri - 0.5)) * 510);
                      fRXData.Mask[aIndex, K*pngWidth+I] := T;
                    end
                    else
                      fRXData.Mask[aIndex, K*pngWidth+I] := 0;
                 end;
        end;
    end;
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
    if gLog <> nil then
      gLog.AddTime(RXInfo[fRT].FileName + ' -', RXXCount);

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
  ProcessFolder(aFolder + IntToStr(Byte(fRT) + 1) + PathDelim);
end;


//Export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes
procedure TKMSpritePack.ExportAll(const aFolder: string);
var
  I: Integer;
  SL: TStringList;
begin
  ForceDirectories(aFolder);

  SL := TStringList.Create;

  for I := 1 to fRXData.Count do
  if fRXData.Flag[I] = 1 then
  begin
    ExportImage(aFolder + Format('%d_%.4d.png', [Byte(fRT)+1, I]), I);

    if fRXData.HasMask[I] then
      ExportMask(aFolder + Format('%d_%.4da.png', [Byte(fRT)+1, I]), I);

    //Export pivot
    SL.Clear;
    SL.Append(IntToStr(fRXData.Pivot[I].x));
    SL.Append(IntToStr(fRXData.Pivot[I].y));
    SL.SaveToFile(aFolder + Format('%d_%.4d.txt', [Byte(fRT)+1, I]));
  end;

  SL.Free;
end;


procedure TKMSpritePack.ExportImage(const aFile: string; aIndex: Integer);
var
  I, K: Integer;
  M: Byte;
  TreatMask: Boolean;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  pngWidth := fRXData.Size[aIndex].X;
  pngHeight := fRXData.Size[aIndex].Y;

  SetLength(pngData, pngWidth * pngHeight);

  //Export RGB values
  for I := 0 to pngHeight - 1 do
  for K := 0 to pngWidth - 1 do
  begin
    TreatMask := fRXData.HasMask[aIndex] and (fRXData.Mask[aIndex, I*pngWidth + K] > 0);
    if (fRT = rxHouses) and ((aIndex < 680) or (aIndex = 1657) or (aIndex = 1659) or (aIndex = 1681) or (aIndex = 1683)) then
      TreatMask := False;

    if TreatMask then
    begin
      M := fRXData.Mask[aIndex, I*pngWidth + K];

      //Replace background with corresponding brightness of Red
      if fRXData.RGBA[aIndex, I*pngWidth + K] = FLAG_COLOR_DARK then
        //Brightness < 0.5, mix with black
        pngData[I*pngWidth + K] := M
      else
        //Brightness > 0.5, mix with white
        pngData[I*pngWidth + K] := $FF + (255 - M) * $010100;
    end
    else
      pngData[I*pngWidth + K] := fRXData.RGBA[aIndex, I*pngWidth + K] and $FFFFFF;

    pngData[I*pngWidth + K] := pngData[I*pngWidth + K] or (fRXData.RGBA[aIndex, I*pngWidth + K] and $FF000000);
  end;

  //Mark pivot location with a dot
  K := pngWidth + fRXData.Pivot[aIndex].x;
  I := pngHeight + fRXData.Pivot[aIndex].y;
  if InRange(I, 0, pngHeight-1) and InRange(K, 0, pngWidth-1) then
    pngData[I*pngWidth + K] := $FF00FF;//}

  SaveToPng(pngWidth, pngHeight, pngData, aFile);
end;


procedure TKMSpritePack.ExportMask(const aFile: string; aIndex: Integer);
var
  I, K: Integer;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  pngWidth := fRXData.Size[aIndex].X;
  pngHeight := fRXData.Size[aIndex].Y;

  SetLength(pngData, pngWidth * pngHeight);

  //Export Mask
  if fRXData.HasMask[aIndex] then
  begin
    for I := 0 to pngHeight - 1 do
    for K := 0 to pngWidth - 1 do
      pngData[I * pngWidth + K] := (Byte(fRXData.Mask[aIndex, I * pngWidth + K] > 0) * $FFFFFF) or $FF000000;

    SaveToPng(pngWidth, pngHeight, pngData, aFile);
  end;
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

  MakeGFX_BinPacking(TexType, aStartingIndex, BaseRAM, ColorRAM, TexCount);

  if LOG_EXTRA_GFX then
  begin
    IdealRAM := 0;
    for I := aStartingIndex to fRXData.Count - 1 do
    if fRXData.Flag[I] <> 0 then
      Inc(IdealRAM, fRXData.Size[I].X * fRXData.Size[I].Y * TexFormatSize[TexType]);

    gLog.AddTime(IntToStr(TexCount) + ' Textures created');
    gLog.AddNoTime(Format('%d/%d', [BaseRAM div 1024, IdealRAM div 1024]) +
                  ' Kbytes allocated/ideal for ' + RXInfo[fRT].FileName + ' GFX when using Packing');
    gLog.AddNoTime(IntToStr(ColorRAM div 1024) + ' KBytes for team colors');
  end;
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
  Folder: string;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  if not EXPORT_SPRITE_ATLASES then Exit;

  Folder := ExeDir + 'Export'+PathDelim+'GenTextures'+PathDelim;
  ForceDirectories(Folder);

  pngWidth := aWidth;
  pngHeight := aHeight;
  SetLength(pngData, pngWidth * pngHeight);

  for I := 0 to aHeight - 1 do
  for K := 0 to aWidth - 1 do
    pngData[I * aWidth + K] := (PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^;

  SaveToPng(pngWidth, pngHeight, pngData, Folder + aFilename + '.png');
end;


{ TKMSprites }
constructor TKMSprites.Create(aStepProgress: TEvent; aStepCaption: TUnicodeStringEvent);
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
var
  RT: TRXType;
begin
  //Remember which version we load, so if it changes inbetween games we reload it
  fAlphaShadows := aAlphaShadows;

  for RT := Low(TRXType) to High(TRXType) do
  if RXInfo[RT].Usage = ruGame then
  begin
    fStepCaption(fTextMain[RXInfo[RT].LoadingTextID]);
    gLog.AddTime('Reading ' + RXInfo[RT].FileName + '.rx');
    LoadSprites(RT, fAlphaShadows);
    fSprites[RT].MakeGFX(fAlphaShadows);
  end;
end;


//Try to load RXX first, then RX, then use Folder
procedure TKMSprites.LoadSprites(aRT: TRXType; aAlphaShadows: Boolean);
begin
  if aAlphaShadows and FileExists(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '_a.rxx') then
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '_a.rxx')
  else
  if FileExists(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '.rxx') then
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '.rxx')
  else
    Exit;

  fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites' + PathDelim);
end;


procedure TKMSprites.ExportToPNG(aRT: TRXType);
begin
  LoadSprites(aRT, False);
  fSprites[aRT].ExportAll(ExeDir + 'Export' + PathDelim + RXInfo[aRT].FileName + '.rx' + PathDelim);
  ClearTemp;
end;


end.
